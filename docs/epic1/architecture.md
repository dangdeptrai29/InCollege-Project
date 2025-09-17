# Architecture – Single Program, Modular Sections

This milestone uses a single COBOL program organized into Sections and Paragraphs so multiple team members can work independently. Design follows the project breakdown: file‑driven input, mirrored output, account persistence, login, post‑login menus, and password validation.

Sections and key paragraphs
- INITIALIZATION-SECTION
  - `INIT-FILES`: Open `io/InCollege-Input.txt` and `io/InCollege-Output.txt`; open/create `data/users.txt`; call `INIT-LOAD-ACCOUNTS`.
  - `INIT-LOAD-ACCOUNTS`: Load existing accounts from `data/users.txt` into an in‑memory table; set `USER-COUNT`. If missing, create the file (empty) or use a fallback example.
- MENU-SECTION
  - `RUN-APP`: Show base menu (Login, Create Account, Exit) using `DISPLAY-AND-LOG`; route by choice; loop until Exit/EOF.
- CREATE-ACCOUNT-SECTION
  - `CREATE-ACCOUNT`: Enforce 5‑account limit; prompt username (unique); prompt password; call `VALIDATE-PASSWORD`; on success call `APPEND-ACCOUNT`, update in‑memory table and `USER-COUNT`, and print success message.
  - `VALIDATE-PASSWORD`: Enforce 8–12 chars, at least one uppercase, one digit, and one special `!@#$%^&*?-_+`; print reason(s) and re‑prompt on failure.
  - `APPEND-ACCOUNT`: Append `username|password` to `data/users.txt` immediately.
- LOGIN-SECTION
  - `LOGIN`: Prompt for username and password via input file; call `CHECK-CREDENTIALS`; on success print success + `Welcome, [Username]!` and `PERFORM POST-LOGIN-MENU`; on failure print error and re‑prompt (unlimited retries).
- VALIDATION-SECTION
  - `CHECK-CREDENTIALS`: Exact match against the in‑memory users table.
- POST-LOGIN-SECTION
  - `POST-LOGIN-MENU`: Show options — Search for a job, Find someone you know, Learn a new skill. First two print “under construction”. Learn routes to `LEARN-SKILL`.
  - `LEARN-SKILL`: List 5 skills plus “Go Back”. Selecting a skill prints “This skill is under construction.” and redisplays the list. “Go Back” returns to `POST-LOGIN-MENU`.
- PARSING-SECTION
  - `PARSE-USER-REC`: Split lines formatted `username|password` into fields; trim spaces.
- IO-SECTION
  - `DISPLAY-AND-LOG`: The only output path. Writes the exact same text to console and `io/InCollege-Output.txt`.
  - `READ-NEXT-INPUT-LINE`: Read the next line from `io/InCollege-Input.txt` in sequence. Convenience wrappers: `READ-CHOICE`, `READ-USERNAME`, `READ-PASSWORD` call this.

Data layout and limits
- `MAX-USERS` = 5; `USER-COUNT` tracks current accounts.
- Users table with entries: `USERNAME` and `PASSWORD` (plain strings for now).
- Persistence file: `data/users.txt`, format `username|password` one per line.

Conventions and rules
- All messages must go through `DISPLAY-AND-LOG`; no direct DISPLAY elsewhere.
- Output must match sample transcripts exactly (text, punctuation, order).
- Input is fully file‑driven from `io/InCollege-Input.txt` to enable deterministic tests.

User flows (aligned with spec)
- Create Account: check limit → username (unique) → password (validate) → persist and confirm → back to main menu.
- Login: prompt username/password → success → welcome → `POST-LOGIN-MENU`; failure → retry.
- Post‑Login: three options; Learn shows 5 skills + Go Back; each skill prints “under construction”.

Deliverables mapping
- Program: `InCollege.cob` (single program with sections above).
- Input/Output: `io/InCollege-Input.txt`, `io/InCollege-Output.txt`.
- Persistence: `data/users.txt`.
- Samples/Tests: sample output transcript and test input files per scenarios.

Rationale
- File‑driven IO enables repeatable tests and scripted scenarios.
- Centralized output guarantees console/file parity.
- Clear Sections/Paragraphs map directly to tickets and ownership.

Future considerations
- Split into subprograms (`CALL`) in later weeks if scope grows.
