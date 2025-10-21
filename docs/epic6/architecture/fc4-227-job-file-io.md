# FC4-227 — Harden Job File I/O

## Working-Storage Additions
- Introduced an error context string and flag to track `JOBS-FILE` failures (`src/InCollege.cob:207`).

## Load Path Guards
- `INIT-LOAD-JOBS` now sets the error flag before opening the file, handles missing optional files, and reports any non-`00` open/close/read status (`src/InCollege.cob:2294`).
- Read failures short-circuit the loop, surface an error message via `REPORT-JOBS-FILE-ERROR`, and stop further processing to keep in-memory state consistent (`src/InCollege.cob:2304`).

## Save Path Guards
- `SAVE-JOBS` validates the status after every `OPEN`, `WRITE`, and `CLOSE`, emitting targeted diagnostics that include the two-character status code when something goes wrong (`src/InCollege.cob:2367`).
- `EXIT PERFORM` prevents additional writes once a failure occurs, while the subsequent close still runs so resources are tidied up (`src/InCollege.cob:2393`).

## Shared Error Reporter
- `REPORT-JOBS-FILE-ERROR` assembles a single-line message: `"Error: <context> (status XX)."`, reusing the existing screen/file logging routine so testers see the code immediately (`src/InCollege.cob:2412`).

## Validation
- Recompiled with `cobc -x -o ./incollege src/InCollege.cob`.
- Ran the default scripted flow via `./incollege` to confirm normal posting still succeeds and no spurious errors are emitted.
