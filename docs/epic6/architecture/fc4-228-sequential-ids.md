# FC4-228 — Sequential Job IDs

## State Tracking
- Added `WS-JOBS-HIGHEST-ID` to working storage so the runtime always knows the largest ID observed (`src/InCollege.cob:187`).
- `INIT-LOAD-JOBS` resets both the job count and the highest ID before reading from disk, ensuring reloads start from a clean baseline (`src/InCollege.cob:2294`).

## Load Behaviour
- `PARSE-JOB-REC` now:
  - Parses the ID column when present, updates `WS-JOBS-HIGHEST-ID` to the greater value, and stores the numeric ID alongside the other fields.
  - Generates a replacement ID (`highest + 1`) for legacy rows that lacked the field and immediately bumps the highest tracker (`src/InCollege.cob:2331`).

## Save/Post Behaviour
- Each new job increments `WS-JOBS-HIGHEST-ID` before assignment, guaranteeing the posted record receives the next sequential number even if earlier IDs were sparse (`src/InCollege.cob:2264`).
- `SAVE-JOBS` continues to emit the ID as the first token, so the sequential numbering persists across restarts (`src/InCollege.cob:2367`).

## Validation
- Recompiled with `cobc -x -o ./incollege src/InCollege.cob`.
- Ran the default scripted flow via `./incollege`; existing rows reloaded with their stored IDs and the new posting materialised with the next sequential value (`data/jobs.txt:1`).
