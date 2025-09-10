# Create Account – Spec, Flows, and Tests

This document describes the Create Account flow for Epic 1, including prompts, messages, password policy, and file‑driven test inputs.

Core rules
- Stores up to 5 users in memory; attempts beyond 5 print: `All permitted accounts have been created, please come back later.`
- Username must be unique; duplicates are rejected with: `That username already exists, please try a different one.`
- Password policy (validated on input):
  - Length: 8–12 characters
  - At least one uppercase letter
  - At least one digit
  - At least one special character from `!@#$%^&*?-_+`
- On success: `Account created successfully.` and the user is persisted to `data/users.txt` (format: `username|password`). The current run also updates in‑memory state immediately.

Prompts
- `Please enter your username:`
- `Please enter your password:`

Failure messages (specific)
- `Password must be 8 to 12 characters.`
- `Password must contain at least one capital letter.`
- `Password must contain at least one digit.`
- `Password must contain at least one special character: !@#$%^&*?-_+`

Tester Workflow (Quickstart)

- Build once: `cobc -x src/InCollege.cob -o incollege`
- Prepare `data/users.txt` per scenario (empty, seeded, or 5 users for capacity).
- Generate `io/InCollege-Input.txt` with a helper script (or write it manually).
 - Clear output: `: > io/InCollege-Output.txt`
- Run: `./incollege`
- Verify: console output matches `io/InCollege-Output.txt`; check `data/users.txt` after create flows.

Notes
- The first line in `io/InCollege-Input.txt` is the menu choice (1 = Login, 2 = Create Account).
- To test “Create then Login”, run two executions back-to-back (Create run, then Login run). The current program performs one action per run by design for Epic 1’s file‑driven tests.
- zsh tip: if your password contains `!`, use single quotes or escape it (`Password1\!`).

How To Test (file‑driven)

Prereqs
- Build: `cobc -x src/InCollege.cob -o incollege`
- Clean output before runs: `: > io/InCollege-Output.txt`
- Users file location: `data/users.txt` (authoritative). Each test below sets it explicitly to control state.

1) Create — Success
- Prepare users file (one seed user):
  - `printf 'testuser|Password1!\n' > data/users.txt`
- Generate input: `bash scripts/make_create_inputs.sh success newuser 'Password1!'`
- Run: `: > io/InCollege-Output.txt && ./incollege`
 - Expect in `io/InCollege-Output.txt`:
  - `Enter Your Choice: 2`
  - `Please enter your username:`
  - `Please enter your password:`
  - `Account created successfully.`
- Verify persistence: `tail -n 1 data/users.txt` shows `newuser|Password1!`
- Verify login (next run):
  - `bash scripts/make_login_inputs.sh success newuser 'Password1!'`
  - `: > io/InCollege-Output.txt && ./incollege`
  - Expect: `You have successfully logged in.` and `Welcome, newuser`

2) Create — Duplicate username then success
- Users file contains `testuser` and `newuser` from previous test.
- Input: `bash scripts/make_create_inputs.sh duplicate-then-success testuser another 'Password1!'`
  - Here, `testuser` triggers the duplicate path; `another` is the unique username for the second prompt.
- Run: `: > io/InCollege-Output.txt && ./incollege`
- Expect (in order):
  - Duplicate message: `That username already exists, please try a different one.`
  - Then `Account created successfully.`
- Verify persistence: `grep '^another|' data/users.txt`

3) Create — Invalid passwords then success
- Input: `bash scripts/make_create_inputs.sh invalid-passwords-then-success userx`
- Run: `: > io/InCollege-Output.txt && ./incollege`
- Expect specific messages after each bad password until the valid one:
  - `Password must be 8 to 12 characters.`
  - `Password must contain at least one capital letter.`
  - `Password must contain at least one digit.`
  - `Password must contain at least one special character: !@#$%^&*?-_+`
- Final line for the flow: `Account created successfully.`
- Verify persistence: `grep '^userx|ValidPass1!' data/users.txt`

4) Create — Capacity reached (5 users)
- Prepare exactly 5 users: `printf 'u1|Password1!\nu2|Password1!\nu3|Password1!\nu4|Password1!\nu5|Password1!\n' > data/users.txt`
- Input: `bash scripts/make_create_inputs.sh capacity extra 'Password1!'`
- Run: `: > io/InCollege-Output.txt && ./incollege`
- Expect: `All permitted accounts have been created, please come back later.`
- Verify no change: `wc -l data/users.txt` still `5`

File‑driven examples

Success
```
2
newuser
Password1!
```
Expected (excerpt):
```
Welcome to InCollege!
1. Login
2. Create Account
Enter Your Choice:
Enter Your Choice: 2
Please enter your username:
Please enter your password:
Account created successfully.
```

Duplicate username then unique
```
2
testuser
newuser
Password1!
```
Excerpt:
```
Please enter your username:
That username already exists, please try a different one.
Please enter your username:
Please enter your password:
Account created successfully.
```

Password re‑prompting
```
2
userx
short
NoDigit!
noupper1!
NoSpecial1
ValidPass1!
```
Excerpt:
```
Please enter your password:
Password must be 8 to 12 characters.
Please enter your password:
Password must contain at least one digit.
Please enter your password:
Password must contain at least one capital letter.
Please enter your password:
Password must contain at least one special character: !@#$%^&*?-_+
Please enter your password:
Account created successfully.
```

Capacity reached (after 5 users exist)
```
2
anyuser
AnyPass1!
```
Excerpt:
```
All permitted accounts have been created, please come back later.
```

Notes
- The program updates the in‑memory user table immediately after creating an account. In the current single‑action menu, testing “create then login” in the same run isn’t navigated via menu; verify by logging in on a subsequent run. The persisted file (`data/users.txt`) is authoritative across runs.
