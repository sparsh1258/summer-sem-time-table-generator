module State

open Elmish
open Browser
open Fable.Core
open Domain

[<Import("sha256Hex", "./auth.js")>]
let private sha256Hex (text: string) : JS.Promise<string> = jsNative

/// SHA-256 of the admin password — the plaintext never appears in the code.
[<Literal>]
let private adminPasswordHash =
    "fd2548756075ad4f8dfeb742120396e5798b78fec73ee41a670ed7c8e6cb2b83"

// ---------------------------------------------------------------------------
// Model
// ---------------------------------------------------------------------------

let maxCourses = 3

type AlertLevel =
    | Info
    | Success
    | Warning
    | Danger

type Alert =
    { Id: int
      Level: AlertLevel
      Message: string }

type Page =
    | Planner
    | Admin

type Model =
    { Entries: ClassEntry list
      CourseInput: string
      SelectedCourses: string list
      Schedules: Schedule list
      SelectedScheduleIndex: int
      ClashOptions: ClashSchedule list
      SelectedClashIndex: int
      Diagnosis: string list
      /// Draft editing: while true, the user rearranges DraftEntries freely
      /// on the grid; nothing touches Entries until they press Finalise.
      EditMode: bool
      DraftEntries: ClassEntry list
      /// The group choices of the schedule being edited — identifies which
      /// sessions the edit grid shows, surviving moves (keyed by group name).
      EditChoices: GroupOption list
      ShowListEditor: bool
      Page: Page
      ImportBusy: bool
      AdminUnlocked: bool
      AdminPasswordInput: string
      Alerts: Alert list
      NextAlertId: int }

type Msg =
    | CourseInputChanged of string
    | AddCourse
    | RemoveCourse of string
    | GenerateSchedules
    | SelectSchedule of int
    | SelectClashOption of int
    | DismissAlert of int
    // Draft editing
    | EnterEditMode
    | CancelEdit
    | FinaliseEdit
    /// Drag-and-drop inside the editor: entry index in DraftEntries, new day,
    /// new start minute (duration preserved).
    | DraftMove of int * Day * int
    /// List editor inside the editor: replaces the entry at the index.
    | DraftUpdate of int * ClassEntry
    | ResetDraft
    | ToggleListEditor
    // Admin
    | NavigateTo of Page
    | AdminPasswordChanged of string
    | SubmitAdminPassword
    | AdminUnlockResult of bool
    | ImportFile of Browser.Types.File
    | ImportSucceeded of Data.ImportResult
    | ImportFailed of exn
    | RestoreDefaultDataset

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Appends an alert, keeping only the five most recent so the stack never
/// grows unbounded.
let private pushAlert (level: AlertLevel) (message: string) (model: Model) =
    let alerts =
        model.Alerts @ [ { Id = model.NextAlertId; Level = level; Message = message } ]

    let trimmed =
        if alerts.Length > 5 then List.skip (alerts.Length - 5) alerts else alerts

    { model with
        Alerts = trimmed
        NextAlertId = model.NextAlertId + 1 }

/// Re-runs the search over the current entries and selection. When nothing
/// clash-free exists, also computes the closest (fewest-clash) timetables.
/// `verbose` controls alerts and stat counters.
let private regenerate (verbose: bool) (model: Model) : Model =
    if model.SelectedCourses.IsEmpty then
        { model with
            Schedules = []
            ClashOptions = []
            Diagnosis = []
            SelectedScheduleIndex = 0
            SelectedClashIndex = 0 }
    else
        let slots = buildSlots model.Entries model.SelectedCourses
        let ranked = findValidSchedules slots |> rankSchedules

        if ranked.IsEmpty then
            let m =
                { model with
                    Schedules = []
                    ClashOptions = findLeastClashSchedules slots 8
                    Diagnosis = diagnoseDeadlock slots
                    SelectedScheduleIndex = 0
                    SelectedClashIndex = 0 }

            if verbose then
                Stats.recordDeadlock ()

                m
                |> pushAlert
                    Danger
                    "No clash-free combination exists — showing the closest timetables with the fewest clashes instead."
            else
                m
        else
            let m =
                { model with
                    Schedules = ranked
                    ClashOptions = []
                    Diagnosis = []
                    SelectedScheduleIndex = 0
                    SelectedClashIndex = 0 }

            if verbose then
                Stats.recordSchedules ()

                m
                |> pushAlert
                    Success
                    (sprintf "Found %d conflict-free schedule(s) — best one shown first." ranked.Length)
            else
                m

[<Literal>]
let private importNoticeKey = "tt-import-notice"

/// Alerts auto-dismiss after 2 seconds.
let private dismissLater (id: int) : Cmd<Msg> =
    [ fun dispatch ->
        window.setTimeout ((fun () -> dispatch (DismissAlert id)), 2000)
        |> ignore ]

let init () : Model * Cmd<Msg> =
    let model =
        { Entries = Data.entries
          CourseInput = ""
          SelectedCourses = []
          Schedules = []
          SelectedScheduleIndex = 0
          ClashOptions = []
          SelectedClashIndex = 0
          Diagnosis = []
          EditMode = false
          DraftEntries = []
          EditChoices = []
          ShowListEditor = false
          Page = (if window.location.hash = "#admin" then Admin else Planner)
          ImportBusy = false
          AdminUnlocked = (window.sessionStorage.getItem "tt-admin-ok" = "1")
          AdminPasswordInput = ""
          Alerts = []
          NextAlertId = 0 }

    // Surface the outcome of an import that triggered the reload we just did.
    let model =
        match window.localStorage.getItem importNoticeKey with
        | null -> model
        | notice ->
            window.localStorage.removeItem importNoticeKey
            model |> pushAlert Success notice

    model, Cmd.batch [ for id in 0 .. model.NextAlertId - 1 -> dismissLater id ]

// ---------------------------------------------------------------------------
// Update
// ---------------------------------------------------------------------------

let private updateCore (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | CourseInputChanged text -> { model with CourseInput = text }, Cmd.none

    | AddCourse ->
        if model.CourseInput.Trim() = "" then
            model, Cmd.none
        elif model.SelectedCourses.Length >= maxCourses then
            model
            |> pushAlert Warning (sprintf "You can select at most %d subjects. Remove one first." maxCourses),
            Cmd.none
        else
            match Data.resolveCode model.CourseInput with
            | Error message -> model |> pushAlert Danger message, Cmd.none
            | Ok code when model.SelectedCourses |> List.contains code ->
                { model with CourseInput = "" }
                |> pushAlert Info (sprintf "%s is already selected." code),
                Cmd.none
            | Ok code ->
                Stats.recordCourse code

                { model with
                    SelectedCourses = model.SelectedCourses @ [ code ]
                    CourseInput = ""
                    EditMode = false }
                |> regenerate true,
                Cmd.none

    | RemoveCourse code ->
        { model with
            SelectedCourses = model.SelectedCourses |> List.filter ((<>) code)
            EditMode = false }
        |> regenerate true,
        Cmd.none

    | GenerateSchedules -> regenerate true model, Cmd.none

    | SelectSchedule index ->
        if index >= 0 && index < model.Schedules.Length then
            { model with SelectedScheduleIndex = index }, Cmd.none
        else
            model, Cmd.none

    | SelectClashOption index ->
        if index >= 0 && index < model.ClashOptions.Length then
            { model with SelectedClashIndex = index }, Cmd.none
        else
            model, Cmd.none

    | DismissAlert id ->
        { model with Alerts = model.Alerts |> List.filter (fun a -> a.Id <> id) }, Cmd.none

    // -- Draft editing --------------------------------------------------

    | EnterEditMode ->
        let choices =
            if not model.Schedules.IsEmpty then
                model.Schedules
                |> List.tryItem model.SelectedScheduleIndex
                |> Option.map (fun s -> s.Choices)
                |> Option.defaultValue []
            elif not model.ClashOptions.IsEmpty then
                model.ClashOptions
                |> List.tryItem model.SelectedClashIndex
                |> Option.map (fun c -> c.Choices)
                |> Option.defaultValue []
            else
                []

        if choices.IsEmpty then
            model |> pushAlert Warning "Add at least one subject before editing the timetable.", Cmd.none
        else
            { model with
                EditMode = true
                DraftEntries = model.Entries
                EditChoices = choices },
            Cmd.none

    | CancelEdit ->
        { model with EditMode = false }
        |> pushAlert Info "Edit cancelled — nothing was changed.",
        Cmd.none

    | FinaliseEdit ->
        let committed =
            { model with
                Entries = model.DraftEntries
                EditMode = false }
            |> regenerate true

        // Keep showing the exact arrangement the user just edited, instead of
        // jumping to whatever the solver ranks first.
        let key (opts: GroupOption list) =
            opts
            |> List.map (fun o -> o.CourseCode, o.Type.Order, o.GroupName)
            |> List.sort

        let editedKey = key model.EditChoices

        let idx =
            committed.Schedules |> List.tryFindIndex (fun s -> key s.Choices = editedKey)

        { committed with SelectedScheduleIndex = defaultArg idx 0 }, Cmd.none

    | DraftMove(index, day, startMin) ->
        match model.DraftEntries |> List.tryItem index with
        | None -> model, Cmd.none
        | Some entry ->
            let duration = entry.Interval.DurationMin
            // Only the official 50-minute slots of the master sheet are valid
            // drop positions — snap to the nearest one the class fits in.
            let start = snapToSlot duration startMin

            let moved =
                { entry with
                    Day = day
                    Interval = { StartMin = start; EndMin = start + duration } }

            { model with
                DraftEntries = model.DraftEntries |> List.mapi (fun i e -> if i = index then moved else e) },
            Cmd.none

    | DraftUpdate(index, entry) ->
        if entry.Interval.EndMin <= entry.Interval.StartMin then
            model |> pushAlert Warning "End time must be after the start time.", Cmd.none
        else
            { model with
                DraftEntries = model.DraftEntries |> List.mapi (fun i e -> if i = index then entry else e) },
            Cmd.none

    | ResetDraft ->
        { model with DraftEntries = Data.entries }
        |> pushAlert Info "Draft reset to the official sheet — press Finalise to apply.",
        Cmd.none

    | ToggleListEditor -> { model with ShowListEditor = not model.ShowListEditor }, Cmd.none

    // -- Admin ----------------------------------------------------------

    | NavigateTo page ->
        window.location.hash <-
            match page with
            | Admin -> "#admin"
            | Planner -> ""

        { model with Page = page }, Cmd.none

    | AdminPasswordChanged value -> { model with AdminPasswordInput = value }, Cmd.none

    | SubmitAdminPassword ->
        if model.AdminPasswordInput = "" then
            model, Cmd.none
        else
            model,
            Cmd.OfPromise.perform
                sha256Hex
                model.AdminPasswordInput
                (fun hash -> AdminUnlockResult(hash = adminPasswordHash))

    | AdminUnlockResult ok ->
        if ok then
            window.sessionStorage.setItem ("tt-admin-ok", "1")

            { model with
                AdminUnlocked = true
                AdminPasswordInput = "" }
            |> pushAlert Success "Admin unlocked for this browser session.",
            Cmd.none
        else
            { model with AdminPasswordInput = "" }
            |> pushAlert Danger "Wrong password.",
            Cmd.none

    | ImportFile file ->
        { model with ImportBusy = true },
        Cmd.OfPromise.either Data.importTimetableFile file ImportSucceeded ImportFailed

    | ImportSucceeded result ->
        Data.saveCustom result.semester result.entries

        let warningNote =
            if result.warnings.Length > 0 then
                sprintf " (%d cells could not be parsed)" result.warnings.Length
            else
                ""

        window.localStorage.setItem (
            importNoticeKey,
            sprintf "Imported %d sessions as “%s”%s." result.entries.Length result.semester warningNote
        )

        window.location.reload ()
        model, Cmd.none

    | ImportFailed e ->
        { model with ImportBusy = false }
        |> pushAlert Danger (sprintf "Import failed: %s" e.Message),
        Cmd.none

    | RestoreDefaultDataset ->
        Data.clearCustom ()
        window.location.reload ()
        model, Cmd.none

/// Wraps the core update so every alert pushed during this update gets an
/// auto-dismiss timer, without each call site having to schedule one.
let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    let updated, cmd = updateCore msg model

    let timers =
        [ for id in model.NextAlertId .. updated.NextAlertId - 1 -> dismissLater id ]

    updated, Cmd.batch (cmd :: timers)
