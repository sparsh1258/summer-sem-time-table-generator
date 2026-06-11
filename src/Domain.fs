module Domain

open System

// ---------------------------------------------------------------------------
// Core calendar primitives
// ---------------------------------------------------------------------------

type Day =
    | Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday

    member this.Index =
        match this with
        | Monday -> 0
        | Tuesday -> 1
        | Wednesday -> 2
        | Thursday -> 3
        | Friday -> 4
        | Saturday -> 5
        | Sunday -> 6

    member this.Label =
        match this with
        | Monday -> "Monday"
        | Tuesday -> "Tuesday"
        | Wednesday -> "Wednesday"
        | Thursday -> "Thursday"
        | Friday -> "Friday"
        | Saturday -> "Saturday"
        | Sunday -> "Sunday"

    member this.Short = this.Label.Substring(0, 3)

    static member All =
        [ Monday; Tuesday; Wednesday; Thursday; Friday; Saturday; Sunday ]

    static member TryParse(raw: string) =
        match raw.Trim().ToLowerInvariant() with
        | "monday" | "mon" | "m" -> Some Monday
        | "tuesday" | "tue" | "tues" -> Some Tuesday
        | "wednesday" | "wed" | "w" -> Some Wednesday
        | "thursday" | "thu" | "thur" | "thurs" -> Some Thursday
        | "friday" | "fri" | "f" -> Some Friday
        | "saturday" | "sat" -> Some Saturday
        | "sunday" | "sun" -> Some Sunday
        | _ -> None

/// A half-open interval measured in minutes since midnight: [StartMin, EndMin)
type TimeInterval =
    { StartMin: int
      EndMin: int }

    member this.Overlaps(other: TimeInterval) =
        this.StartMin < other.EndMin && other.StartMin < this.EndMin

    member this.DurationMin = this.EndMin - this.StartMin

    member this.Format =
        let fmt m = sprintf "%02d:%02d" (m / 60) (m % 60)
        sprintf "%s–%s" (fmt this.StartMin) (fmt this.EndMin)

    /// Parses strings like "09:00-10:00", "9:00 - 10:00", "14:00–16:00"
    static member TryParse(raw: string) =
        let cleaned =
            raw.Trim().Replace("–", "-").Replace("—", "-").Replace(" ", "")

        let parsePart (p: string) =
            match p.Split(':') with
            | [| h; m |] ->
                match Int32.TryParse h, Int32.TryParse m with
                | (true, hh), (true, mm) when hh >= 0 && hh < 24 && mm >= 0 && mm < 60 ->
                    Some(hh * 60 + mm)
                | _ -> None
            | [| h |] ->
                match Int32.TryParse h with
                | true, hh when hh >= 0 && hh < 24 -> Some(hh * 60)
                | _ -> None
            | _ -> None

        match cleaned.Split('-') with
        | [| a; b |] ->
            match parsePart a, parsePart b with
            | Some s, Some e when e > s -> Some { StartMin = s; EndMin = e }
            | _ -> None
        | _ -> None

type ClassType =
    | Lecture
    | Tutorial
    | Practical

    member this.Label =
        match this with
        | Lecture -> "Lecture"
        | Tutorial -> "Tutorial"
        | Practical -> "Practical"

    member this.Short =
        match this with
        | Lecture -> "L"
        | Tutorial -> "T"
        | Practical -> "P"

    member this.Order =
        match this with
        | Lecture -> 0
        | Tutorial -> 1
        | Practical -> 2

    static member TryParse(raw: string) =
        match raw.Trim().ToLowerInvariant() with
        | "l" | "lec" | "lecture" -> Some Lecture
        | "t" | "tut" | "tutorial" -> Some Tutorial
        | "p" | "pr" | "prac" | "practical" | "lab" -> Some Practical
        | _ -> None

// ---------------------------------------------------------------------------
// Official slot grid: fourteen 50-minute slots from 08:00 to 19:40, exactly
// as laid out in the master workbook. All manual edits snap to these.
// ---------------------------------------------------------------------------

let dayStartMin = 8 * 60
let slotMinutes = 50
let slotCount = 14
let dayEndMin = dayStartMin + slotMinutes * slotCount // 19:40

let slotStarts =
    [ for i in 0 .. slotCount - 1 -> dayStartMin + i * slotMinutes ]

/// Snaps an arbitrary start minute to the nearest official slot start such
/// that a class of the given duration still fits inside the slot grid.
let snapToSlot (duration: int) (rawStart: int) =
    let span = max 1 ((duration + slotMinutes - 1) / slotMinutes)
    let maxIndex = max 0 (slotCount - span)
    let idx = (rawStart - dayStartMin + slotMinutes / 2) / slotMinutes
    let idx = max 0 (min maxIndex idx)
    dayStartMin + idx * slotMinutes

/// One row of the master sheet: a single weekly meeting of one group.
type ClassEntry =
    { CourseCode: string
      CourseTitle: string
      Group: string
      Type: ClassType
      Day: Day
      Interval: TimeInterval
      Room: string }

// ---------------------------------------------------------------------------
// Scheduling model
// ---------------------------------------------------------------------------

/// One selectable group (e.g. "Tutorial group T2 of UCS520") together with
/// every weekly session that choosing it commits the student to.
type GroupOption =
    { CourseCode: string
      CourseTitle: string
      Type: ClassType
      GroupName: string
      Sessions: ClassEntry list }

/// One decision the solver must make: pick exactly one option for this
/// (course, component-type) pair.
type CourseSlot =
    { CourseCode: string
      Type: ClassType
      Options: GroupOption list }

/// A fully resolved, conflict-free weekly schedule with ranking metrics.
type Schedule =
    { Choices: GroupOption list
      Sessions: ClassEntry list
      GapMinutes: int
      LateMinutes: int
      Score: int }

// ---------------------------------------------------------------------------
// Conflict predicates
// ---------------------------------------------------------------------------

let sessionsConflict (a: ClassEntry) (b: ClassEntry) =
    a.Day = b.Day && a.Interval.Overlaps b.Interval

let private optionConflictsWithSessions (chosen: ClassEntry list) (opt: GroupOption) =
    opt.Sessions
    |> List.exists (fun s -> chosen |> List.exists (sessionsConflict s))

let optionsConflict (a: GroupOption) (b: GroupOption) =
    a.Sessions
    |> List.exists (fun sa -> b.Sessions |> List.exists (sessionsConflict sa))

let rec private anyPairConflicts (sessions: ClassEntry list) =
    match sessions with
    | [] | [ _ ] -> false
    | x :: rest -> (rest |> List.exists (sessionsConflict x)) || anyPairConflicts rest

// ---------------------------------------------------------------------------
// Slot construction
// ---------------------------------------------------------------------------

/// For each selected course, derive one CourseSlot per component type that
/// actually exists in the sheet (a course without practicals simply has no
/// Practical slot), each listing its distinct groups.
let buildSlots (entries: ClassEntry list) (courses: string list) : CourseSlot list =
    [ for code in courses do
        let courseEntries = entries |> List.filter (fun e -> e.CourseCode = code)

        let types =
            courseEntries
            |> List.map (fun e -> e.Type)
            |> List.distinct
            |> List.sortBy (fun t -> t.Order)

        for t in types do
            let options =
                courseEntries
                |> List.filter (fun e -> e.Type = t)
                |> List.groupBy (fun e -> e.Group)
                |> List.map (fun (groupName, sessions) ->
                    { CourseCode = code
                      CourseTitle = sessions.Head.CourseTitle
                      Type = t
                      GroupName = groupName
                      Sessions = sessions })
                |> List.sortBy (fun o -> o.GroupName)

            { CourseCode = code; Type = t; Options = options } ]

// ---------------------------------------------------------------------------
// Backtracking search
// ---------------------------------------------------------------------------

/// Hard cap on enumerated results so a pathological sheet can never freeze
/// the browser tab. 500 valid schedules is far more than anyone will browse.
let private maxResults = 500

/// Depth-first backtracking over the cartesian product of slot options,
/// pruning any branch as soon as a chosen group clashes with the partial
/// selection. Slots with the fewest options are decided first (fail-fast).
let findValidSchedules (slots: CourseSlot list) : GroupOption list list =
    let ordered = slots |> List.sortBy (fun s -> List.length s.Options)

    let rec go remaining (chosenOpts: GroupOption list) (chosenSessions: ClassEntry list) =
        seq {
            match remaining with
            | [] -> yield List.rev chosenOpts
            | slot :: rest ->
                for opt in slot.Options do
                    if not (optionConflictsWithSessions chosenSessions opt) then
                        yield! go rest (opt :: chosenOpts) (opt.Sessions @ chosenSessions)
        }

    go ordered [] [] |> Seq.truncate maxResults |> Seq.toList

// ---------------------------------------------------------------------------
// Heuristic ranking
// ---------------------------------------------------------------------------

let private eveningStart = 17 * 60

/// Total idle minutes spent on campus between consecutive classes, per day.
let totalGapMinutes (sessions: ClassEntry list) =
    sessions
    |> List.groupBy (fun s -> s.Day)
    |> List.sumBy (fun (_, daySessions) ->
        daySessions
        |> List.sortBy (fun s -> s.Interval.StartMin)
        |> List.pairwise
        |> List.sumBy (fun (a, b) -> max 0 (b.Interval.StartMin - a.Interval.EndMin)))

/// Minutes of class time falling after 17:00.
let totalLateMinutes (sessions: ClassEntry list) =
    sessions
    |> List.sumBy (fun s -> max 0 (s.Interval.EndMin - max s.Interval.StartMin eveningStart))

/// Lower score = better schedule. Gaps hurt; evening minutes hurt double.
let rankSchedules (combos: GroupOption list list) : Schedule list =
    combos
    |> List.map (fun choices ->
        let sessions = choices |> List.collect (fun c -> c.Sessions)
        let gaps = totalGapMinutes sessions
        let late = totalLateMinutes sessions

        { Choices = choices
          Sessions = sessions
          GapMinutes = gaps
          LateMinutes = late
          Score = gaps + 2 * late })
    |> List.sortBy (fun s -> s.Score, s.Sessions.Length)

// ---------------------------------------------------------------------------
// Least-clash fallback search
// ---------------------------------------------------------------------------

/// A complete one-group-per-component selection that could not avoid clashes.
/// Produced only when the backtracking search proves a clash-free week
/// impossible — the "least bad" timetables, ranked by fewest clashing pairs.
type ClashSchedule =
    { Choices: GroupOption list
      Sessions: ClassEntry list
      ClashPairs: (ClassEntry * ClassEntry) list
      GapMinutes: int }

/// Beam search over the option product minimising the number of clashing
/// session pairs. Options that produce the exact same set of clashes are
/// collapsed so each result shows a genuinely different conflict pattern.
let findLeastClashSchedules (slots: CourseSlot list) (maxOptions: int) : ClashSchedule list =
    let ordered = slots |> List.sortBy (fun s -> List.length s.Options)
    let beamWidth = 300

    let step states (slot: CourseSlot) =
        states
        |> List.collect (fun (choices, sessions: ClassEntry list, clashes) ->
            slot.Options
            |> List.map (fun opt ->
                let added =
                    opt.Sessions
                    |> List.sumBy (fun s -> sessions |> List.filter (sessionsConflict s) |> List.length)

                opt :: choices, opt.Sessions @ sessions, clashes + added))
        |> List.sortBy (fun (_, _, c) -> c)
        |> List.truncate beamWidth

    ordered
    |> List.fold step [ ([], [], 0) ]
    |> List.map (fun (choices, sessions, _) ->
        let arr = List.toArray sessions

        let pairs =
            [ for i in 0 .. arr.Length - 1 do
                for j in i + 1 .. arr.Length - 1 do
                    if sessionsConflict arr.[i] arr.[j] then
                        yield arr.[i], arr.[j] ]

        { Choices = List.rev choices
          Sessions = sessions
          ClashPairs = pairs
          GapMinutes = totalGapMinutes sessions })
    |> List.sortBy (fun c -> c.ClashPairs.Length, c.GapMinutes)
    |> List.distinctBy (fun c ->
        c.ClashPairs
        |> List.map (fun (a, b) ->
            sprintf
                "%s %s %s %d / %s %s %s %d"
                a.CourseCode a.Group a.Day.Label a.Interval.StartMin
                b.CourseCode b.Group b.Day.Label b.Interval.StartMin)
        |> List.sort
        |> String.concat ";")
    |> List.truncate maxOptions

// ---------------------------------------------------------------------------
// Deadlock diagnosis
// ---------------------------------------------------------------------------

/// Called only when the search returned zero schedules. Produces a
/// human-readable breakdown of which component pairs make a solution
/// impossible (or nearly impossible).
let diagnoseDeadlock (slots: CourseSlot list) : string list =
    let arr = List.toArray slots
    let slotName (s: CourseSlot) = sprintf "%s %s" s.CourseCode s.Type.Label

    let selfIssues =
        [ for s in arr do
            for opt in s.Options do
                if anyPairConflicts opt.Sessions then
                    yield
                        sprintf
                            "Data issue: group %s of %s has sessions that overlap each other in the sheet itself."
                            opt.GroupName
                            (slotName s) ]

    let pairIssues =
        [ for i in 0 .. arr.Length - 1 do
            for j in i + 1 .. arr.Length - 1 do
                let a, b = arr.[i], arr.[j]
                let total = a.Options.Length * b.Options.Length

                let conflicting =
                    a.Options
                    |> List.sumBy (fun oa -> b.Options |> List.filter (optionsConflict oa) |> List.length)

                if total > 0 && conflicting = total then
                    yield
                        sprintf
                            "Hard deadlock: every %s group clashes with every %s group — these two components can never coexist."
                            (slotName a)
                            (slotName b)
                elif total > 0 && float conflicting / float total >= 0.5 then
                    yield
                        sprintf
                            "Bottleneck: %d of %d pairings between %s and %s clash, leaving very little room for the third subject."
                            conflicting
                            total
                            (slotName a)
                            (slotName b) ]

    let messages = selfIssues @ pairIssues

    if messages.IsEmpty then
        [ "No single pair of components is fully incompatible — the clash only appears when three or more groups are combined. Try removing one subject at a time to isolate the chain." ]
    else
        messages
