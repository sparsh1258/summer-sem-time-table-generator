module Data

open Fable.Core
open Fable.Core.JsInterop
open Browser
open Domain

// ---------------------------------------------------------------------------
// Embedded master timetable + admin-uploaded override
// ---------------------------------------------------------------------------
// src/timetable.json is generated from the university's master sheet by
// tools/extract-timetable.mjs. An admin can replace it at runtime by
// importing a workbook on the admin page; the parsed dataset is persisted in
// localStorage and wins over the embedded sheet on the next load.

type RawEntry =
    { code: string
      ``type``: string
      group: string
      day: string
      start: int
      ``end``: int
      room: string }

type RawData =
    { semester: string
      entries: RawEntry[] }

/// Shape returned by excel-import.js for an uploaded workbook.
type ImportResult =
    { semester: string
      entries: RawEntry[]
      warnings: string[] }

[<Import("importTimetableFile", "./excel-import.js")>]
let importTimetableFile (file: Browser.Types.File) : JS.Promise<ImportResult> = jsNative

[<ImportDefault("./timetable.json")>]
let private defaultRaw: RawData = jsNative

[<Literal>]
let private customKey = "tt-custom-dataset"

let private customRaw: RawData option =
    match window.localStorage.getItem customKey with
    | null -> None
    | s ->
        try
            let parsed: RawData = unbox (JS.JSON.parse s)

            if isNull (box parsed.entries) || parsed.entries.Length = 0 then
                None
            else
                Some parsed
        with _ ->
            None

/// True when an admin upload has replaced the embedded sheet in this browser.
let isCustom = customRaw.IsSome

let private raw = defaultArg customRaw defaultRaw

let semester = raw.semester

/// Persists an imported dataset; it takes effect on the next page load.
let saveCustom (sem: string) (rawEntries: RawEntry[]) =
    let data: RawData = { semester = sem; entries = rawEntries }
    window.localStorage.setItem (customKey, JS.JSON.stringify data)

let clearCustom () = window.localStorage.removeItem customKey

let entries: ClassEntry list =
    [ for e in raw.entries do
        match Day.TryParse e.day, ClassType.TryParse e.``type`` with
        | Some day, Some classType ->
            { CourseCode = e.code
              CourseTitle = ""
              Group = e.group
              Type = classType
              Day = day
              Interval = { StartMin = e.start; EndMin = e.``end`` }
              Room = e.room }
        | _ -> () ]

/// Canonical course codes, sorted. Cross-listed courses keep their combined
/// form, e.g. "UES101/UTA015".
let courseCodes =
    entries |> List.map (fun e -> e.CourseCode) |> List.distinct |> List.sort

let roomCount =
    entries |> List.map (fun e -> e.Room) |> List.distinct |> List.length

/// Resolves what the user typed to a canonical course code. Either half of a
/// cross-listed code is accepted: "UTA015" resolves to "UES101/UTA015".
/// An exact match always wins over a partial one.
let resolveCode (input: string) : Result<string, string> =
    let needle = input.Trim().ToUpperInvariant()

    if needle = "" then
        Error "Type a course code first."
    elif courseCodes |> List.contains needle then
        Ok needle
    else
        match courseCodes |> List.filter (fun c -> c.Split('/') |> Array.contains needle) with
        | [ single ] -> Ok single
        | [] ->
            Error(
                sprintf
                    "Course code %s was not found in the %s timetable — either the code is wrong, or the subject is self-study (no scheduled classes)."
                    needle
                    semester
            )
        | several ->
            Error(
                sprintf
                    "%s is cross-listed in several courses — pick one of: %s."
                    needle
                    (String.concat ", " several)
            )
