# Time Table Generator & Conflict Resolver

A 100% client-side SPA built with **F# / Fable 5 / Elmish / Feliz**. The
official **Summer Semester 2026** master timetable is embedded in the app —
just type 1–3 subject codes and get every conflict-free weekly schedule,
ranked by fewest idle gaps and fewest late-evening classes.

Cross-listed courses are accepted under either code (e.g. `UTA015` resolves
to `UES101/UTA015`).

## Prerequisites

- [.NET SDK 9.0+](https://dotnet.microsoft.com/download) (`winget install Microsoft.DotNet.SDK.9`)
- Node.js 18+

## Run

```sh
dotnet tool restore   # installs the fable CLI from .config/dotnet-tools.json
npm install
npm start             # fable watch + vite dev server
```

Then open the URL Vite prints (default http://localhost:5173).

## Build for production

```sh
npm run build         # output in dist/ — deploy to any static host
```

## Admin page

Open `/#admin` (or the **⚙ Admin** button in the header) to:

- **Replace the dataset** by uploading a master workbook (.xlsx) — parsed
  entirely in the browser and persisted in `localStorage`, so it only affects
  that browser. To publish a new sheet for everyone, use `npm run extract`
  and redeploy.
- **See usage stats** — visits, searches solved, deadlocks hit, and the most
  searched subjects. The app has no backend, so counters are per-browser.

## Updating the timetable data

`src/timetable.json` is generated from the university's grid-style master
workbook (one block per room/lab, days as rows, fourteen 50-minute slots from
08:00 to 19:40 as columns, course codes suffixed L/T/P, group qualifiers in
the rows beneath each day). When a new sheet is published:

```sh
npm run extract -- "path/to/TIME TABLE.xlsx"
```

The extractor merges consecutive slots of the same class into one interval,
expands shorthand cross-listings (`UMA004/23L` → `UMA004/UMA023 L`), and
prints any cells it could not parse.

## Project layout

| File                          | Responsibility |
|-------------------------------|----------------|
| `src/Domain.fs`               | Pure domain types, backtracking combination search, gap/evening ranking, deadlock diagnosis |
| `src/Data.fs`                 | Loads the embedded `timetable.json`, resolves typed codes (incl. cross-listings) |
| `src/State.fs`                | Elmish model/update — subject tags, validation, alert center, schedule generation |
| `src/View.fs`                 | Feliz UI — tag input with suggestions, alerts, ranked schedule tabs, CSS-grid weekly calendar, print view |
| `src/App.fs`                  | Entry point (`React.useElmish` + `ReactDOM.createRoot`) |
| `src/timetable.json`          | Generated dataset — do not edit by hand |
| `tools/extract-timetable.mjs` | Converts the master workbook into `src/timetable.json` |
