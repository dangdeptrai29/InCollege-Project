# InCollege Project – Week 3 Deliverable

## Description
This repository contains the Week 3 deliverable for the InCollege Project.  
The objective of this milestone is to implement the **core authentication system** and simulate the initial user navigation menu in COBOL.  

The program provides the following functionality:
- **User Registration**: New users can create an account (up to 5 accounts).  
  - Passwords must be 8–12 characters long and include at least one capital letter, one digit, and one special character.  
  - Accounts are persisted in a file and automatically reloaded when the program restarts.  
  - On the 6th account attempt, the system displays:  
    `"All permitted accounts have been created, please come back later."`  

- **User Login**: Returning users can log in using existing credentials.  
  - Successful login → `"You have successfully logged in."`  
  - Failed login → `"Incorrect username/password, please try again."` (unlimited attempts allowed).  

- **Post-Login Navigation**:  
  - Options presented: **Search for a job**, **Find someone you know**, **Learn a new skill**.  
  - Job Search & Find Someone → `"Under construction"` message.  
  - Learn a new skill → Presents 5 skills to choose from (all return `"Under construction"`).  
  - Includes option to return to main menu.  

- **Input/Output Handling**:  
  - All input (username, password, menu selections) is read from an input file.  
  - All output is displayed on the screen **and** written to an output file for verification.  
  - Output file must exactly match console output.  

This milestone also includes tester responsibilities:
- Development of comprehensive test input files (positive, negative, edge cases).  
- Verification that outputs in console and output file match exactly.  
- Logging bugs in Jira when discrepancies are found.  


## Instruction
To run Cobol, first install a COBOL compiler. Common choice: GnuCOBOL (open-cobol), available via package managers.
- Linux/macOS: `brew install gnu-cobol` or `apt-get install open-cobol`
- Windows: GnuCOBOL binaries exist, or run inside WSL

Verify installation:
- `cobc -v`

## Docs
- Project Breakdown (source spec): `docs/project-breakdown.md`
- Architecture (sections/paragraphs layout): `docs/epic1/architecture.md`
- Login – Part 1 details: `docs/epic1/login-part-1.md`

Architecture is the source of truth for structure and ownership in Epic 1. See Jira mapping for tickets linked to each architecture section: `docs/jira/epic1.md`.

## Testing (High-Level)
This project is file‑driven per spec: all input comes from `io/InCollege_Input.txt` and every printed line is mirrored to `io/InCollege_Output.txt`. 

Quickly create inputs:
- Success input: `bash scripts/make_login_inputs.sh success testuser 'Password1!'`
- 4 failures then success: `bash scripts/make_login_inputs.sh failure-4-then-success testuser 'Password1!'`

Manual run (success):
1) Build: `cobc -x src/InCollege.cob -o incollege`
2) `printf '1\n%s\n%s\n' 'testuser' 'Password1!' > io/InCollege_Input.txt`
3) `: > io/InCollege_Output.txt && ./incollege`
4) Expect: menu, `Enter Your Choice: 1`, prompts, success, and `Welcome, testuser`

More detailed flows: `docs/epic1/login-part-1.md`

Notes
- Multiple attempts in one run: add more username/password pairs (two lines per attempt) to `io/InCollege_Input.txt`. The program tries each pair until a success or end of file.
- Output overwrite vs append: The program currently overwrites `io/InCollege_Output.txt` each run for deterministic testing. If you prefer appending runs with a separator, say the word and we can switch to `OPEN EXTEND` and add a run header printed to both console and file.

Troubleshooting
- Seeing “Incorrect username/password…” when you expect success? Verify:
  - `io/InCollege_Input.txt` contains exactly the username on one line and the password on the next.
  - `data/users.txt` still has the matching `username|password` (it should not contain any commands or unrelated text).
- Warning “implicit CLOSE of USERS-FILE”: This used to happen when the user file reached EOF and we conditionally skipped a CLOSE. The program now tracks the open state and closes the file explicitly; the warning should no longer appear.

File Roles
- `data/users.txt`: canonical, writable user store (git‑ignored). Format: `username|password`. Created/updated by the app when accounts are added.
- `data/users.examples.txt`: example seed checked into git. Used as a fallback if `users.txt` is missing or empty.
- `io/InCollege_Input.txt`: test input for login attempts. Each attempt is two lines: first the username, second the password.
- `io/InCollege_Output.txt`: program output mirror. Overwritten each run for clean comparisons.

Users file: example vs. real
- Prefer `data/users.txt` for real runs; it is ignored by git to avoid leaking local credentials.
- If `data/users.txt` is missing or empty, the program falls back to `data/users.examples.txt` for convenience (contains `testuser|Password1!`).
- To start with the example as your real data, copy it once: `cp data/users.examples.txt data/users.txt`.

zsh Tips
- zsh expands `!` even inside double quotes. Use one of:
  - Single quotes: `printf '%s\n%s\n' 'testuser' 'Password1!' > io/InCollege_Input.txt`
  - Escape the exclamation: `Password1\!`
  - Or temporarily disable history expansion: `setopt NO_BANG_HIST` (re-enable with `setopt BANG_HIST`).
