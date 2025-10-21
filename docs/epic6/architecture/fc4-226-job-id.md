# FC4-226 — Job Posting ID Field

## Schema Updates
- `WS-JOB-ENTRY` now includes a numeric `WS-JOB-ID` alongside the existing poster/title/etc. fields (`src/InCollege.cob:185`).
- `data/jobs.txt` persists IDs as the first pipe-delimited token, e.g. `1|testuser|Software Engineer|...`.
- New working storage helpers (`WS-NEW-JOB-ID`, `WS-JOB-ID-TEXT`, `WS-JOB-ID-DISPLAY`) format IDs for saves and user-facing messages.

## Runtime Behaviour
- `POST-NEW-JOB` seeds `WS-NEW-JOB-ID` from the incremented job count and writes it into the in-memory table before persisting (`src/InCollege.cob:2260`).
- `PARSE-JOB-REC` backfills IDs when loading existing records and auto-generates placeholders for legacy rows without an ID (`src/InCollege.cob:2305`).
- `SAVE-JOBS` emits the ID as the first column for each record (`src/InCollege.cob:2340`), keeping the file schema consistent across sessions.

## UX Impact
- Job confirmation now reads `Job posted successfully! (ID: <n>)`, giving the poster an immediate reference (`src/InCollege.cob:2277`).

## Validation
- Rebuilt the program with `cobc -x -o ./incollege src/InCollege.cob`.
- Exercised the default `io/InCollege-Input.txt` flow via `./incollege`, confirming the new confirmation message and updated `data/jobs.txt` output.
