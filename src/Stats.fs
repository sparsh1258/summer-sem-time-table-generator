module Stats

// Lightweight usage counters for the admin page. This is a fully client-side
// app with no backend, so everything is persisted in this browser's
// localStorage — visits and searches from other devices are not visible.

open Fable.Core
open Browser

let private intOf (key: string) =
    match window.localStorage.getItem key with
    | null -> 0
    | s ->
        match System.Int32.TryParse s with
        | true, n -> n
        | _ -> 0

let private bump (key: string) =
    window.localStorage.setItem (key, string (intOf key + 1))

let visits () = intOf "tt-stat-visits"
let schedulesGenerated () = intOf "tt-stat-schedules"
let deadlocksHit () = intOf "tt-stat-deadlocks"

let recordVisit () = bump "tt-stat-visits"
let recordSchedules () = bump "tt-stat-schedules"
let recordDeadlock () = bump "tt-stat-deadlocks"

[<Literal>]
let private countsKey = "tt-stat-course-counts"

/// How often each course code has been added, stored as a JSON array of
/// [code, count] pairs (tuples are plain arrays in Fable's JS output).
let courseCounts () : (string * int) list =
    match window.localStorage.getItem countsKey with
    | null -> []
    | s ->
        try
            unbox<(string * int)[]> (JS.JSON.parse s) |> Array.toList
        with _ ->
            []

let recordCourse (code: string) =
    let current = courseCounts ()

    let updated =
        if current |> List.exists (fun (k, _) -> k = code) then
            current |> List.map (fun (k, v) -> if k = code then k, v + 1 else k, v)
        else
            (code, 1) :: current

    window.localStorage.setItem (countsKey, JS.JSON.stringify (List.toArray updated))

let topCourses (n: int) =
    courseCounts () |> List.sortByDescending snd |> List.truncate n
