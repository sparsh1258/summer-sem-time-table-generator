module View

open Feliz
open Fable.Core
open Fable.Core.JsInterop
open Domain
open State

[<Import("downloadTimetablePdf", "./pdf-export.js")>]
let private downloadTimetablePdf (elementId: string, filename: string) : JS.Promise<unit> = jsNative

// ---------------------------------------------------------------------------
// Small helpers
// ---------------------------------------------------------------------------

let private courseColorIndex (model: Model) (code: string) =
    model.SelectedCourses
    |> List.tryFindIndex ((=) code)
    |> Option.defaultValue 0

/// Standard component colors, identical across every subject:
/// Lecture = blue, Tutorial = green, Practical = purple.
let private typeClass (t: ClassType) =
    match t with
    | Lecture -> "type-l"
    | Tutorial -> "type-t"
    | Practical -> "type-p"

let private typeBadge (t: ClassType) =
    Html.span [
        prop.className (sprintf "type-badge %s" (typeClass t))
        prop.title t.Label
        prop.text t.Short
    ]

let private formatDuration (minutes: int) =
    if minutes <= 0 then "0m"
    elif minutes < 60 then sprintf "%dm" minutes
    elif minutes % 60 = 0 then sprintf "%dh" (minutes / 60)
    else sprintf "%dh %02dm" (minutes / 60) (minutes % 60)

let private fmtClock (m: int) = sprintf "%02d:%02d" (m / 60) (m % 60)

let private parseClock (v: string) =
    match v.Split(':') with
    | [| h; m |] ->
        match System.Int32.TryParse h, System.Int32.TryParse m with
        | (true, hh), (true, mm) when hh >= 0 && hh < 24 && mm >= 0 && mm < 60 -> Some(hh * 60 + mm)
        | _ -> None
    | _ -> None

/// While a block is being dragged, blocks must not swallow pointer events or
/// the drop would never reach the cell underneath them.
let private setGridDragging (on: bool) =
    let grid = Browser.Dom.document.querySelector ".tt-grid"

    if not (isNull grid) then
        let el = grid :?> Browser.Types.HTMLElement
        if on then el.classList.add "dragging" else el.classList.remove "dragging"

/// Touch devices don't fire HTML5 drag events, so the editor implements its
/// own pointer-based drag for them: document-level listeners follow the
/// finger, highlight the slot underneath, and drop on release.
let private startTouchDrag
    (move: (int * Day * int) -> unit)
    (index: int)
    (source: Browser.Types.HTMLElement)
    (startEv: Browser.Types.PointerEvent)
    =
    startEv.preventDefault ()
    source.classList.add "touch-dragging"
    setGridDragging true

    let doc = Browser.Dom.document
    let mutable hover: Browser.Types.Element option = None

    let clearHover () =
        hover |> Option.iter (fun c -> c.classList.remove "drag-over")
        hover <- None

    let cellAt (x: float) (y: float) =
        match doc.elementFromPoint (x, y) with
        | null -> None
        | el ->
            let cell: Browser.Types.Element = el?closest ("[data-slot]")
            if isNull (box cell) then None else Some cell

    let onMove (e: Browser.Types.Event) =
        let pe = e :?> Browser.Types.PointerEvent
        clearHover ()

        match cellAt pe.clientX pe.clientY with
        | Some cell ->
            cell.classList.add "drag-over"
            hover <- Some cell
        | None -> ()

    let mutable cleanup = fun () -> ()

    let onUp (e: Browser.Types.Event) =
        let pe = e :?> Browser.Types.PointerEvent
        let target = cellAt pe.clientX pe.clientY
        cleanup ()

        match target with
        | Some cell ->
            let dayAttr = cell.getAttribute "data-day"
            let slotAttr = cell.getAttribute "data-slot"

            if not (isNull dayAttr) && not (isNull slotAttr) then
                match Day.TryParse dayAttr, System.Int32.TryParse slotAttr with
                | Some d, (true, slot) -> move (index, d, slot)
                | _ -> ()
        | None -> ()

    let onCancel (_: Browser.Types.Event) = cleanup ()

    cleanup <-
        fun () ->
            doc.removeEventListener ("pointermove", onMove)
            doc.removeEventListener ("pointerup", onUp)
            doc.removeEventListener ("pointercancel", onCancel)
            source.classList.remove "touch-dragging"
            setGridDragging false
            clearHover ()

    doc.addEventListener ("pointermove", onMove)
    doc.addEventListener ("pointerup", onUp)
    doc.addEventListener ("pointercancel", onCancel)

// ---------------------------------------------------------------------------
// Header
// ---------------------------------------------------------------------------

let private header (model: Model) (dispatch: Msg -> unit) =
    Html.header [
        prop.className "app-header"
        prop.children [
            Html.div [
                prop.className "header-text"
                prop.children [
                    Html.h1 [ prop.text "Time Table Generator" ]
                    Html.p [
                        prop.className "subtitle"
                        prop.text (
                            match model.Page with
                            | Planner ->
                                sprintf
                                    "%s · pick 1–3 subjects and get every conflict-free week, ranked by fewest idle gaps."
                                    Data.semester
                            | Admin -> "Admin · manage the embedded dataset and see how the app is being used."
                        )
                    ]
                ]
            ]
            Html.div [
                prop.className "header-actions"
                prop.children [
                    Html.span [ prop.className "header-badge"; prop.text Data.semester ]
                    Html.button [
                        prop.className "btn ghost nav-btn"
                        prop.onClick (fun _ ->
                            dispatch (NavigateTo(if model.Page = Admin then Planner else Admin)))
                        prop.text (
                            match model.Page with
                            | Planner -> "⚙ Admin"
                            | Admin -> "← Back to planner"
                        )
                    ]
                ]
            ]
        ]
    ]

// ---------------------------------------------------------------------------
// Alert center
// ---------------------------------------------------------------------------

let private alertCenter (model: Model) (dispatch: Msg -> unit) =
    if model.Alerts.IsEmpty then
        Html.none
    else
        Html.div [
            prop.className "alert-center"
            prop.children [
                for alert in model.Alerts ->
                    let levelClass =
                        match alert.Level with
                        | Info -> "info"
                        | Success -> "success"
                        | Warning -> "warning"
                        | Danger -> "danger"

                    Html.div [
                        prop.key alert.Id
                        prop.className (sprintf "alert %s" levelClass)
                        prop.children [
                            Html.span [ prop.className "alert-text"; prop.text alert.Message ]
                            Html.button [
                                prop.className "alert-x"
                                prop.title "Dismiss"
                                prop.onClick (fun _ -> dispatch (DismissAlert alert.Id))
                                prop.text "×"
                            ]
                        ]
                    ]
            ]
        ]

// ---------------------------------------------------------------------------
// Subject tag input
// ---------------------------------------------------------------------------

let private courseCard (model: Model) (dispatch: Msg -> unit) =
    Html.section [
        prop.className "card"
        prop.children [
            Html.h2 [ prop.text "1 · Pick your subjects" ]
            Html.div [
                prop.className "tag-row"
                prop.children [
                    for code in model.SelectedCourses ->
                        Html.span [
                            prop.key code
                            prop.className (sprintf "tag color-%d" (courseColorIndex model code))
                            prop.children [
                                Html.span [ prop.text code ]
                                Html.button [
                                    prop.className "tag-x"
                                    prop.title (sprintf "Remove %s" code)
                                    prop.onClick (fun _ -> dispatch (RemoveCourse code))
                                    prop.text "×"
                                ]
                            ]
                        ]
                ]
            ]
            Html.div [
                prop.className "course-input-row"
                prop.children [
                    Html.input [
                        prop.className "course-input"
                        prop.placeholder (
                            if model.SelectedCourses.Length >= State.maxCourses then
                                "Maximum 3 subjects selected"
                            else
                                "Type a course code, e.g. UEC301"
                        )
                        prop.value model.CourseInput
                        prop.disabled (model.SelectedCourses.Length >= State.maxCourses)
                        prop.custom ("list", "course-codes")
                        prop.autoFocus true
                        prop.custom ("spellCheck", false)
                        prop.onChange (fun (text: string) -> dispatch (CourseInputChanged text))
                        prop.onKeyDown (fun e -> if e.key = "Enter" then dispatch AddCourse)
                    ]
                    Html.button [
                        prop.className "btn primary"
                        prop.disabled (
                            model.CourseInput.Trim() = ""
                            || model.SelectedCourses.Length >= State.maxCourses
                        )
                        prop.onClick (fun _ -> dispatch AddCourse)
                        prop.text "Add"
                    ]
                ]
            ]
            Html.datalist [
                prop.id "course-codes"
                prop.children [
                    for code in Data.courseCodes ->
                        Html.option [ prop.key code; prop.value code ]
                ]
            ]
            Html.p [
                prop.className "hint"
                prop.text (
                    sprintf
                        "%d of %d subjects selected · cross-listed codes (e.g. UTA015) are accepted under either name."
                        model.SelectedCourses.Length
                        State.maxCourses
                )
            ]
        ]
    ]

// ---------------------------------------------------------------------------
// Dataset card
// ---------------------------------------------------------------------------

let private datasetCard (model: Model) =
    let stat (value: string) (label: string) =
        Html.div [
            prop.className "stat"
            prop.children [
                Html.span [ prop.className "stat-value"; prop.text value ]
                Html.span [ prop.className "stat-label"; prop.text label ]
            ]
        ]

    Html.section [
        prop.className "card"
        prop.children [
            Html.h2 [ prop.text "Built-in master sheet" ]
            Html.div [
                prop.className "stat-row"
                prop.children [
                    stat (string Data.courseCodes.Length) "courses"
                    stat (string model.Entries.Length) "weekly sessions"
                    stat (string Data.roomCount) "rooms & labs"
                ]
            ]
            Html.p [
                prop.className "hint"
                prop.text (
                    if Data.isCustom then
                        sprintf "Using the admin-uploaded “%s” dataset stored in this browser." Data.semester
                    else
                        sprintf
                            "The official %s master timetable is embedded in this app — no upload needed."
                            Data.semester
                )
            ]
        ]
    ]

// ---------------------------------------------------------------------------
// Schedule picker tabs + metrics + legends
// ---------------------------------------------------------------------------

let private scheduleTabs (model: Model) (dispatch: Msg -> unit) =
    Html.div [
        prop.className "schedule-tabs"
        prop.children [
            for i, s in model.Schedules |> List.truncate 10 |> List.indexed do
                yield Html.button [
                    prop.key i
                    prop.className (
                        if i = model.SelectedScheduleIndex then "schedule-tab active"
                        else "schedule-tab"
                    )
                    prop.onClick (fun _ -> dispatch (SelectSchedule i))
                    prop.children [
                        Html.span [ prop.className "tab-title"; prop.text (sprintf "Option %d" (i + 1)) ]
                        Html.span [
                            prop.className "tab-meta"
                            prop.text (sprintf "%s gaps" (formatDuration s.GapMinutes))
                        ]
                    ]
                ]

            if model.Schedules.Length > 10 then
                yield Html.span [
                    prop.className "tab-more"
                    prop.text (sprintf "+%d more not shown" (model.Schedules.Length - 10))
                ]
        ]
    ]

let private metricsBar (model: Model) (dispatch: Msg -> unit) (schedule: Schedule) =
    Html.div [
        prop.className "metrics"
        prop.children [
            Html.span [
                prop.className "metric"
                prop.text (sprintf "🕳 Idle gaps: %s" (formatDuration schedule.GapMinutes))
            ]
            Html.span [
                prop.className "metric"
                prop.text (
                    if schedule.LateMinutes > 0 then
                        sprintf "🌙 After 17:00: %s" (formatDuration schedule.LateMinutes)
                    else
                        "🌙 No evening classes"
                )
            ]
            Html.span [
                prop.className "metric"
                prop.text (sprintf "📚 %d sessions / week" schedule.Sessions.Length)
            ]
            Html.button [
                prop.className "btn ghost print-btn"
                prop.onClick (fun _ -> dispatch EnterEditMode)
                prop.text "✏ Edit timetable"
            ]
            Html.button [
                prop.className "btn ghost"
                prop.onClick (fun _ ->
                    let safe = (model.SelectedCourses |> String.concat "-").Replace("/", "_")

                    downloadTimetablePdf ("tt-capture", sprintf "timetable-%s.pdf" safe)
                    |> ignore)
                prop.text "⬇ Download PDF"
            ]
            Html.button [
                prop.className "btn ghost"
                prop.onClick (fun _ -> Browser.Dom.window?print ())
                prop.text "🖨 Print"
            ]
        ]
    ]

/// Standard Lecture/Tutorial/Practical color key — the colors live only on
/// the L/T/P badge, so the legend shows the badges themselves.
let private typeLegend (withConflict: bool) =
    let chip (t: ClassType) =
        Html.span [
            prop.key t.Short
            prop.className "legend-chip"
            prop.children [ typeBadge t; Html.span [ prop.text t.Label ] ]
        ]

    Html.div [
        prop.className "legend type-legend"
        prop.children [
            yield chip Lecture
            yield chip Tutorial
            yield chip Practical
            if withConflict then
                yield Html.span [
                    prop.key "clash"
                    prop.className "legend-chip"
                    prop.children [
                        Html.span [ prop.className "chip-swatch conflict-chip" ]
                        Html.span [ prop.text "Clash" ]
                    ]
                ]
        ]
    ]

/// Which group was chosen for each component of each selected course.
let private choicesLegend (model: Model) (choices: GroupOption list) =
    Html.div [
        prop.className "legend"
        prop.children [
            for index, code in List.indexed model.SelectedCourses ->
                let courseChoices =
                    choices
                    |> List.filter (fun c -> c.CourseCode = code)
                    |> List.sortBy (fun c -> c.Type.Order)

                let groups =
                    courseChoices
                    |> List.map (fun c -> sprintf "%s: %s" c.Type.Short c.GroupName)
                    |> String.concat " · "

                Html.div [
                    prop.key code
                    prop.className "legend-item"
                    prop.children [
                        Html.div [
                            prop.className "legend-text"
                            prop.children [
                                Html.span [
                                    prop.className (sprintf "legend-code code-color-%d" index)
                                    prop.text code
                                ]
                                Html.span [ prop.className "legend-groups"; prop.text groups ]
                            ]
                        ]
                    ]
                ]
        ]
    ]

// ---------------------------------------------------------------------------
// Weekly calendar grid
// ---------------------------------------------------------------------------

/// One block placed on the grid. Lanes let overlapping sessions sit side by
/// side inside the same day column. DragIndex points into the draft entry
/// list when the grid is editable.
type private GridBlock =
    { Entry: ClassEntry
      Lane: int
      LaneCount: int
      ConflictsWith: ClassEntry list
      DragIndex: int option
      /// Index of the subject in the selection — colors only its code text.
      CourseColor: int }

/// Renders the weekly grid. When `onMove` is given the grid is editable:
/// blocks can be dragged and dropped onto any day/hour cell (snapped to
/// 10-minute steps), reporting (entryIndex, day, newStartMinute).
let private renderGrid (onMove: ((int * Day * int) -> unit) option) (blocks: GridBlock list) =
    if blocks.IsEmpty then
        Html.none
    else
        let sessions = blocks |> List.map (fun b -> b.Entry)

        // Weekend columns appear only when something is scheduled on them.
        let days =
            Day.All
            |> List.filter (fun d -> d.Index < 5 || sessions |> List.exists (fun s -> s.Day = d))

        // One row band per official 50-minute slot, at 10-minute resolution.
        let rowsPerSlot = Domain.slotMinutes / 10
        let totalRows = Domain.slotCount * rowsPerSlot

        let rowOf minutes =
            let r = (minutes - Domain.dayStartMin) / 10 + 2
            max 2 (min (totalRows + 2) r)

        let colOf (d: Day) = (days |> List.findIndex ((=) d)) + 2

        Html.div [
            prop.className (if onMove.IsSome then "tt-grid editable" else "tt-grid")
            prop.style [
                style.custom ("gridTemplateColumns", sprintf "84px repeat(%d, minmax(110px, 1fr))" days.Length)
                style.custom ("gridTemplateRows", sprintf "36px repeat(%d, 9px)" totalRows)
            ]
            prop.children [
                // Day headers
                for d in days do
                    yield Html.div [
                        prop.key (sprintf "head-%s" d.Label)
                        prop.className "tt-day-header"
                        prop.style [
                            style.custom ("gridColumn", string (colOf d))
                            style.custom ("gridRow", "1")
                        ]
                        prop.text d.Short
                    ]

                // Slot labels (08:00–08:50, 08:50–09:40, …) + background cells
                for k in 0 .. Domain.slotCount - 1 do
                    let slotStart = Domain.dayStartMin + k * Domain.slotMinutes
                    let row = rowOf slotStart
                    let rowEnd = rowOf (slotStart + Domain.slotMinutes)

                    yield Html.div [
                        prop.key (sprintf "slot-%d" k)
                        prop.className "tt-hour"
                        prop.style [
                            style.custom ("gridColumn", "1")
                            style.custom ("gridRow", sprintf "%d / %d" row rowEnd)
                        ]
                        prop.text (
                            sprintf "%s–%s" (fmtClock slotStart) (fmtClock (slotStart + Domain.slotMinutes))
                        )
                    ]

                    for d in days do
                        yield Html.div [
                            yield prop.key (sprintf "cell-%s-%d" d.Label k)
                            yield prop.className (if k % 2 = 0 then "tt-cell even" else "tt-cell")
                            yield prop.style [
                                style.custom ("gridColumn", string (colOf d))
                                style.custom ("gridRow", sprintf "%d / %d" row rowEnd)
                            ]
                            match onMove with
                            | Some move ->
                                yield prop.custom ("data-day", d.Label)
                                yield prop.custom ("data-slot", string slotStart)
                                yield prop.onDragOver (fun e ->
                                    e.preventDefault ()
                                    e.dataTransfer.dropEffect <- "move")
                                yield prop.onDragEnter (fun e ->
                                    (e.currentTarget :?> Browser.Types.HTMLElement).classList.add "drag-over")
                                yield prop.onDragLeave (fun e ->
                                    (e.currentTarget :?> Browser.Types.HTMLElement).classList.remove "drag-over")
                                yield prop.onDrop (fun e ->
                                    e.preventDefault ()
                                    let el = e.currentTarget :?> Browser.Types.HTMLElement
                                    el.classList.remove "drag-over"
                                    setGridDragging false

                                    match System.Int32.TryParse(e.dataTransfer.getData "text/plain") with
                                    | true, index ->
                                        // The cell IS the slot — drop starts there.
                                        move (index, d, slotStart)
                                    | _ -> ())
                            | None -> ()
                        ]

                // Class blocks
                for b in blocks do
                    let s = b.Entry
                    let isConflict = not b.ConflictsWith.IsEmpty

                    let vsText =
                        b.ConflictsWith
                        |> List.map (fun o -> sprintf "%s %s %s" o.CourseCode o.Type.Short o.Group)
                        |> List.distinct
                        |> String.concat ", "

                    yield Html.div [
                        yield prop.key (
                            sprintf "%s-%s-%s-%s-%d"
                                s.CourseCode s.Group s.Type.Short s.Day.Label s.Interval.StartMin
                        )
                        yield prop.className (
                            sprintf "class-block %s%s" (typeClass s.Type) (if isConflict then " conflict" else "")
                        )
                        yield prop.style [
                            yield style.custom ("gridColumn", string (colOf s.Day))
                            yield style.custom (
                                "gridRow",
                                sprintf "%d / %d" (rowOf s.Interval.StartMin) (rowOf s.Interval.EndMin)
                            )
                            if b.LaneCount > 1 then
                                let width = 100.0 / float b.LaneCount
                                yield style.custom ("width", sprintf "calc(%.4f%% - 2px)" width)
                                yield style.custom ("marginLeft", sprintf "%.4f%%" (width * float b.Lane))
                        ]
                        yield prop.title (
                            sprintf "%s %s (%s) · %s %s · %s%s%s"
                                s.CourseCode s.Type.Label s.Group s.Day.Label s.Interval.Format s.Room
                                (if isConflict then sprintf "\nCLASHES WITH: %s" vsText else "")
                                (if onMove.IsSome then "\nDrag to move this class." else "")
                        )
                        match onMove, b.DragIndex with
                        | Some move, Some index ->
                            yield prop.draggable true
                            yield prop.onDragStart (fun e ->
                                e.dataTransfer.setData ("text/plain", string index) |> ignore
                                e.dataTransfer.effectAllowed <- "move"
                                // Changing the source block's styles synchronously
                                // inside dragstart cancels the drag in Chrome —
                                // defer the pointer-events/opacity switch a tick.
                                Browser.Dom.window.setTimeout ((fun () -> setGridDragging true), 0)
                                |> ignore)
                            yield prop.onDragEnd (fun _ -> setGridDragging false)
                            // Touch/pen drags use the custom pointer-based path;
                            // mouse keeps native HTML5 drag and drop.
                            yield prop.onPointerDown (fun e ->
                                if e.pointerType <> "mouse" then
                                    startTouchDrag
                                        move
                                        index
                                        (e.currentTarget :?> Browser.Types.HTMLElement)
                                        e)
                        | _ -> ()
                        yield prop.children [
                            yield Html.div [
                                prop.className "block-head"
                                prop.children [
                                    typeBadge s.Type
                                    Html.span [
                                        prop.className (sprintf "block-code code-color-%d" b.CourseColor)
                                        prop.text s.CourseCode
                                    ]
                                ]
                            ]
                            yield Html.span [
                                prop.className "block-meta"
                                prop.text (sprintf "%s · %s" s.Group s.Room)
                            ]
                            yield Html.span [ prop.className "block-time"; prop.text s.Interval.Format ]
                            if isConflict then
                                yield Html.span [
                                    prop.className "block-conflict"
                                    prop.text (sprintf "⚠ vs %s" vsText)
                                ]
                        ]
                    ]
            ]
        ]

/// Greedy interval lane assignment: each session gets the first lane that is
/// already free at its start time. Returns (tag, session, lane, lanesInDay).
let private assignLanes (daySessions: (int option * ClassEntry) list) =
    let sorted =
        daySessions |> List.sortBy (fun (_, s) -> s.Interval.StartMin, s.Interval.EndMin)

    let laneEnds = ResizeArray<int>()

    let pick (s: ClassEntry) =
        let mutable lane = -1
        let mutable i = 0

        while lane < 0 && i < laneEnds.Count do
            if laneEnds.[i] <= s.Interval.StartMin then lane <- i
            i <- i + 1

        if lane < 0 then
            laneEnds.Add s.Interval.EndMin
            laneEnds.Count - 1
        else
            laneEnds.[lane] <- s.Interval.EndMin
            lane

    let assigned = sorted |> List.map (fun (tag, s) -> tag, s, pick s)
    let laneCount = max 1 laneEnds.Count
    assigned |> List.map (fun (tag, s, lane) -> tag, s, lane, laneCount)

/// All overlapping pairs among a set of sessions.
let private clashPairsOf (sessions: ClassEntry list) =
    let arr = List.toArray sessions

    [ for i in 0 .. arr.Length - 1 do
        for j in i + 1 .. arr.Length - 1 do
            if sessionsConflict arr.[i] arr.[j] then
                yield arr.[i], arr.[j] ]

let private conflictsFrom (pairs: (ClassEntry * ClassEntry) list) (s: ClassEntry) =
    pairs
    |> List.choose (fun (a, b) ->
        if a = s then Some b
        elif b = s then Some a
        else None)

let private scheduleGrid (model: Model) (schedule: Schedule) =
    schedule.Sessions
    |> List.map (fun s ->
        { Entry = s
          Lane = 0
          LaneCount = 1
          ConflictsWith = []
          DragIndex = None
          CourseColor = courseColorIndex model s.CourseCode })
    |> renderGrid None

let private clashGrid (model: Model) (option: ClashSchedule) =
    option.Sessions
    |> List.map (fun s -> None, s)
    |> List.groupBy (fun (_, s) -> s.Day)
    |> List.collect (fun (_, daySessions) -> assignLanes daySessions)
    |> List.map (fun (_, s, lane, laneCount) ->
        { Entry = s
          Lane = lane
          LaneCount = laneCount
          ConflictsWith = conflictsFrom option.ClashPairs s
          DragIndex = None
          CourseColor = courseColorIndex model s.CourseCode })
    |> renderGrid None

let private clashTabs (model: Model) (dispatch: Msg -> unit) =
    Html.div [
        prop.className "schedule-tabs"
        prop.children [
            for i, c in List.indexed model.ClashOptions ->
                let n = c.ClashPairs.Length

                Html.button [
                    prop.key i
                    prop.className (
                        if i = model.SelectedClashIndex then "schedule-tab clash active"
                        else "schedule-tab clash"
                    )
                    prop.onClick (fun _ -> dispatch (SelectClashOption i))
                    prop.children [
                        Html.span [ prop.className "tab-title"; prop.text (sprintf "Option %d" (i + 1)) ]
                        Html.span [
                            prop.className "tab-meta"
                            prop.text (sprintf "%d clash%s" n (if n = 1 then "" else "es"))
                        ]
                    ]
                ]
        ]
    ]

// ---------------------------------------------------------------------------
// Edit mode: drag classes freely on a draft, then finalise
// ---------------------------------------------------------------------------

let private editorRow (dispatch: Msg -> unit) (index: int) (entry: ClassEntry) =
    Html.div [
        prop.key index
        prop.className "editor-row"
        prop.children [
            Html.span [
                prop.className (sprintf "editor-type %s" (typeClass entry.Type))
                prop.title entry.Type.Label
                prop.text entry.Type.Short
            ]
            Html.span [ prop.className "editor-code"; prop.text entry.CourseCode ]
            Html.span [ prop.className "editor-group"; prop.text entry.Group ]
            Html.select [
                prop.className "editor-select"
                prop.value entry.Day.Label
                prop.onChange (fun (v: string) ->
                    match Day.TryParse v with
                    | Some d -> dispatch (DraftUpdate(index, { entry with Day = d }))
                    | None -> ())
                prop.children [
                    for d in Day.All ->
                        Html.option [ prop.key d.Label; prop.value d.Label; prop.text d.Short ]
                ]
            ]
            // Start times are restricted to the sheet's official 50-minute
            // slots; the end follows from the class's fixed duration.
            Html.select [
                prop.className "editor-select"
                prop.value (fmtClock entry.Interval.StartMin)
                prop.onChange (fun (v: string) ->
                    match parseClock v with
                    | Some m ->
                        dispatch (
                            DraftUpdate(
                                index,
                                { entry with
                                    Interval = { StartMin = m; EndMin = m + entry.Interval.DurationMin } }
                            )
                        )
                    | None -> ())
                prop.children [
                    for slotStart in Domain.slotStarts do
                        if slotStart + entry.Interval.DurationMin <= Domain.dayEndMin then
                            Html.option [
                                prop.key slotStart
                                prop.value (fmtClock slotStart)
                                prop.text (fmtClock slotStart)
                            ]
                ]
            ]
            Html.span [
                prop.className "editor-dash"
                prop.text (sprintf "→ %s" (fmtClock entry.Interval.EndMin))
            ]
            Html.input [
                prop.className "editor-room"
                prop.value entry.Room
                prop.title "Room"
                prop.onChange (fun (v: string) -> dispatch (DraftUpdate(index, { entry with Room = v })))
            ]
        ]
    ]

let private editCanvas (model: Model) (dispatch: Msg -> unit) =
    // The sessions being arranged: the draft entries belonging to the groups
    // of the schedule the user was viewing, keyed by group name so they stay
    // attached to their blocks as they move.
    let displayed =
        model.DraftEntries
        |> List.indexed
        |> List.filter (fun (_, e) ->
            model.EditChoices
            |> List.exists (fun o ->
                o.CourseCode = e.CourseCode && o.Type = e.Type && o.GroupName = e.Group))

    let sessions = displayed |> List.map snd
    let pairs = clashPairsOf sessions

    let blocks =
        displayed
        |> List.map (fun (i, s) -> Some i, s)
        |> List.groupBy (fun (_, s) -> s.Day)
        |> List.collect (fun (_, daySessions) -> assignLanes daySessions)
        |> List.map (fun (tag, s, lane, laneCount) ->
            { Entry = s
              Lane = lane
              LaneCount = laneCount
              ConflictsWith = conflictsFrom pairs s
              DragIndex = tag
              CourseColor = courseColorIndex model s.CourseCode })

    Html.div [
        prop.className "edit-canvas"
        prop.children [
            yield Html.div [
                prop.className "edit-bar"
                prop.children [
                    Html.button [
                        prop.className "btn success"
                        prop.onClick (fun _ -> dispatch FinaliseEdit)
                        prop.text "✓ Finalise"
                    ]
                    Html.button [
                        prop.className "btn"
                        prop.onClick (fun _ -> dispatch CancelEdit)
                        prop.text "✕ Cancel"
                    ]
                    Html.button [
                        prop.className "btn ghost"
                        prop.onClick (fun _ -> dispatch ResetDraft)
                        prop.text "↺ Reset draft"
                    ]
                    Html.button [
                        prop.className "btn ghost"
                        prop.onClick (fun _ -> dispatch ToggleListEditor)
                        prop.text (if model.ShowListEditor then "Hide list editor" else "≡ List editor")
                    ]
                    Html.span [
                        prop.className (if pairs.IsEmpty then "edit-status ok" else "edit-status clash")
                        prop.text (
                            if pairs.IsEmpty then
                                "✓ No clashes in draft"
                            else
                                sprintf "⚠ %d clash%s in draft" pairs.Length (if pairs.Length = 1 then "" else "es")
                        )
                    ]
                ]
            ]
            yield Html.p [
                prop.className "drag-hint"
                prop.text (
                    "Drag any class to another slot (on phones: press a class and slide your finger) — "
                    + "drops snap to the official 50-minute periods from the master sheet, and red blocks "
                    + "mark overlaps. Nothing is applied until you press Finalise."
                )
            ]
            yield typeLegend true
            yield renderGrid (Some(fun (i, d, m) -> dispatch (DraftMove(i, d, m)))) blocks

            if model.ShowListEditor then
                let rows =
                    displayed
                    |> List.sortBy (fun (_, e) -> e.CourseCode, e.Type.Order, e.Day.Index, e.Interval.StartMin)

                yield Html.div [
                    prop.className "edit-list"
                    prop.children [ for index, entry in rows -> editorRow dispatch index entry ]
                ]
        ]
    ]

// ---------------------------------------------------------------------------
// Results panel
// ---------------------------------------------------------------------------

let private resultsSection (model: Model) (dispatch: Msg -> unit) =
    Html.section [
        prop.className (if model.EditMode then "card results editing" else "card results")
        prop.children [
            yield Html.div [
                prop.className "results-head"
                prop.children [
                    Html.h2 [
                        prop.text (if model.EditMode then "2 · Editing your week" else "2 · Your week")
                    ]
                    if not model.EditMode && not model.SelectedCourses.IsEmpty then
                        Html.button [
                            prop.className "btn ghost"
                            prop.onClick (fun _ -> dispatch EnterEditMode)
                            prop.text "✏ Edit timetable"
                        ]
                ]
            ]

            if model.EditMode then
                yield editCanvas model dispatch
            elif not model.Diagnosis.IsEmpty then
                yield Html.div [
                    prop.className "diagnosis"
                    prop.children [
                        yield Html.h3 [ prop.text "Why no clash-free schedule is possible" ]
                        for i, line in List.indexed model.Diagnosis do
                            yield Html.p [ prop.key i; prop.className "diagnosis-line"; prop.text line ]
                    ]
                ]

                if not model.ClashOptions.IsEmpty then
                    yield Html.h3 [
                        prop.className "conflict-title"
                        prop.text "Closest timetables — fewest clashes first"
                    ]
                    yield Html.p [
                        prop.className "hint conflict-hint"
                        prop.text (
                            "Each option picks exactly one group per component, just like a real schedule, "
                            + "and keeps the clashes to a minimum. Red blocks show the collisions side by side "
                            + "and name what they collide with."
                        )
                    ]
                    yield clashTabs model dispatch

                    match model.ClashOptions |> List.tryItem model.SelectedClashIndex with
                    | Some option ->
                        yield Html.div [
                            prop.className "edit-bar"
                            prop.children [
                                Html.button [
                                    prop.className "btn ghost"
                                    prop.onClick (fun _ -> dispatch EnterEditMode)
                                    prop.text "✏ Edit timetable to fix these clashes"
                                ]
                            ]
                        ]
                        yield typeLegend true
                        yield choicesLegend model option.Choices
                        yield clashGrid model option
                    | None -> ()
            elif model.Schedules.IsEmpty then
                yield Html.div [
                    prop.className "empty-state"
                    prop.children [
                        Html.span [ prop.className "empty-icon"; prop.text "🗓" ]
                        Html.p [ prop.text "Add a subject on the left to see your ranked, conflict-free weeks here." ]
                        Html.p [
                            prop.className "empty-sub"
                            prop.text "Schedules are ranked by fewest idle gaps; evening classes count double."
                        ]
                    ]
                ]
            else
                match model.Schedules |> List.tryItem model.SelectedScheduleIndex with
                | Some schedule ->
                    yield scheduleTabs model dispatch
                    yield metricsBar model dispatch schedule
                    yield Html.div [
                        prop.id "tt-capture"
                        prop.className "capture-area"
                        prop.children [
                            Html.div [
                                prop.className "capture-title"
                                prop.text (
                                    sprintf "%s · %s"
                                        (String.concat " + " model.SelectedCourses)
                                        Data.semester
                                )
                            ]
                            typeLegend false
                            choicesLegend model schedule.Choices
                            scheduleGrid model schedule
                        ]
                    ]
                | None -> yield Html.none
        ]
    ]

// ---------------------------------------------------------------------------
// Admin page
// ---------------------------------------------------------------------------

let private adminDatasetCard (model: Model) (dispatch: Msg -> unit) =
    Html.section [
        prop.className "card"
        prop.children [
            Html.h2 [ prop.text "Dataset" ]
            Html.div [
                prop.className "info-rows"
                prop.children [
                    Html.div [
                        prop.className "info-row"
                        prop.children [
                            Html.span [ prop.text "Active dataset" ]
                            Html.span [
                                prop.className (
                                    if Data.isCustom then "badge-pill custom" else "badge-pill official"
                                )
                                prop.text (if Data.isCustom then "Admin upload" else "Official embedded")
                            ]
                        ]
                    ]
                    Html.div [
                        prop.className "info-row"
                        prop.children [
                            Html.span [ prop.text "Semester" ]
                            Html.span [ prop.className "info-value"; prop.text Data.semester ]
                        ]
                    ]
                    Html.div [
                        prop.className "info-row"
                        prop.children [
                            Html.span [ prop.text "Courses · sessions · rooms" ]
                            Html.span [
                                prop.className "info-value"
                                prop.text (
                                    sprintf "%d · %d · %d"
                                        Data.courseCodes.Length
                                        model.Entries.Length
                                        Data.roomCount
                                )
                            ]
                        ]
                    ]
                ]
            ]
            Html.h3 [ prop.className "admin-sub"; prop.text "Replace with a new master workbook" ]
            Html.input [
                prop.className "file-input"
                prop.type' "file"
                prop.custom ("accept", ".xlsx,.xls")
                prop.disabled model.ImportBusy
                prop.onChange (fun (file: Browser.Types.File) -> dispatch (ImportFile file))
            ]
            if model.ImportBusy then
                Html.p [ prop.className "hint"; prop.text "Parsing workbook…" ]
            Html.div [
                prop.className "editor-actions"
                prop.children [
                    Html.button [
                        prop.className "btn ghost"
                        prop.disabled (not Data.isCustom)
                        prop.onClick (fun _ -> dispatch RestoreDefaultDataset)
                        prop.text "↺ Restore official dataset"
                    ]
                ]
            ]
            Html.p [
                prop.className "hint"
                prop.text (
                    "The workbook must follow the master grid layout (one block per room, days as rows, "
                    + "50-minute slots as columns, codes suffixed L/T/P). The import is parsed entirely in "
                    + "the browser and stored locally — to publish it for everyone, run "
                    + "`npm run extract` and redeploy."
                )
            ]
        ]
    ]

let private adminStatsCard (model: Model) =
    let stat (value: string) (label: string) =
        Html.div [
            prop.className "stat"
            prop.children [
                Html.span [ prop.className "stat-value"; prop.text value ]
                Html.span [ prop.className "stat-label"; prop.text label ]
            ]
        ]

    let top = Stats.topCourses 8

    Html.section [
        prop.className "card"
        prop.children [
            Html.h2 [ prop.text "Usage statistics" ]
            Html.div [
                prop.className "stat-row"
                prop.children [
                    stat (string (Stats.visits ())) "visits"
                    stat (string (Stats.schedulesGenerated ())) "searches solved"
                    stat (string (Stats.deadlocksHit ())) "deadlocks hit"
                ]
            ]
            Html.h3 [ prop.className "admin-sub"; prop.text "Most searched subjects" ]
            if top.IsEmpty then
                Html.p [ prop.className "hint"; prop.text "No subject searches recorded yet." ]
            else
                Html.table [
                    prop.className "stats-table"
                    prop.children [
                        Html.thead [
                            Html.tr [
                                Html.th [ prop.text "#" ]
                                Html.th [ prop.text "Course code" ]
                                Html.th [ prop.text "Times added" ]
                            ]
                        ]
                        Html.tbody [
                            for i, (code, count) in List.indexed top ->
                                Html.tr [
                                    prop.key code
                                    prop.children [
                                        Html.td [ prop.text (string (i + 1)) ]
                                        Html.td [ prop.className "stats-code"; prop.text code ]
                                        Html.td [ prop.text (string count) ]
                                    ]
                                ]
                        ]
                    ]
                ]
            Html.p [
                prop.className "hint"
                prop.text (
                    "This app is fully client-side, so the counters cover this browser only. "
                    + "For real multi-user numbers, put the deployed site behind an analytics service."
                )
            ]
        ]
    ]

let private adminLockCard (model: Model) (dispatch: Msg -> unit) =
    Html.main [
        prop.className "admin-lock-wrap"
        prop.children [
            Html.section [
                prop.className "card admin-lock"
                prop.children [
                    Html.h2 [ prop.text "🔒 Admin access" ]
                    Html.p [
                        prop.className "hint"
                        prop.text "Enter the admin password to manage the dataset and view usage statistics."
                    ]
                    Html.div [
                        prop.className "course-input-row"
                        prop.children [
                            Html.input [
                                prop.className "course-input admin-pass"
                                prop.type' "password"
                                prop.placeholder "Password"
                                prop.value model.AdminPasswordInput
                                prop.autoFocus true
                                prop.onChange (fun (v: string) -> dispatch (AdminPasswordChanged v))
                                prop.onKeyDown (fun e -> if e.key = "Enter" then dispatch SubmitAdminPassword)
                            ]
                            Html.button [
                                prop.className "btn primary"
                                prop.disabled (model.AdminPasswordInput = "")
                                prop.onClick (fun _ -> dispatch SubmitAdminPassword)
                                prop.text "Unlock"
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let private adminPage (model: Model) (dispatch: Msg -> unit) =
    if not model.AdminUnlocked then
        adminLockCard model dispatch
    else
        Html.main [
            prop.className "admin-layout"
            prop.children [
                adminDatasetCard model dispatch
                adminStatsCard model
            ]
        ]

// ---------------------------------------------------------------------------
// Root view
// ---------------------------------------------------------------------------

let view (model: Model) (dispatch: Msg -> unit) =
    Html.div [
        prop.className "app"
        prop.children [
            alertCenter model dispatch
            header model dispatch
            (match model.Page with
             | Admin -> adminPage model dispatch
             | Planner ->
                 Html.main [
                     prop.className "layout"
                     prop.children [
                         Html.div [
                             prop.className "sidebar"
                             prop.children [
                                 courseCard model dispatch
                                 datasetCard model
                             ]
                         ]
                         Html.div [
                             prop.className "main-col"
                             prop.children [ resultsSection model dispatch ]
                         ]
                     ]
                 ])
            Html.footer [
                prop.className "app-footer"
                prop.text (
                    sprintf "%s master timetable · runs entirely in your browser." Data.semester
                )
            ]
        ]
    ]
