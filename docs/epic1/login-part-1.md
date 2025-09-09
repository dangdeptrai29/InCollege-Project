# Login – Part 1 (File‑Driven Test Flow)

This document describes the required file‑driven login flows and how to reproduce them using a single input file per run. All inputs come from `io/InCollege_Input.txt`. Every line the program prints is also written identically to `io/InCollege_Output.txt`.

Setup
- Build: `cobc -x src/InCollege.cob -o incollege`
- Users file:
  - Preferred: `data/users.txt` (git‑ignored) stores real users in `username|password` format.
  - Fallback: if `data/users.txt` is missing or empty, the program uses `data/users.examples.txt` (contains `testuser|Password1!`).
  - To seed a real file from the example: `cp data/users.examples.txt data/users.txt`.
- Clean output file before each run: `: > io/InCollege_Output.txt`

Login Success
Flow A — Successful Login
- Quick script to create the input file:
```
bash scripts/make_login_inputs.sh success testuser 'Password1!'
```
- Or write the input file manually (`io/InCollege_Input.txt`):
```
1
testuser
Password1!
```
- Run:
```
: > io/InCollege_Output.txt && ./incollege
```
- Expected output (console and file, identical):
```
Welcome to InCollege!
1. Login
2. Create Account
Enter Your Choice:
Enter Your Choice: 1
Please enter your username:
Please enter your password:
You have successfully logged in.
Welcome, testuser
```

Login Failure
Flow B — 4 Failed Attempts then Success (single run)
- Quick script to create the input file:
```
bash scripts/make_login_inputs.sh failure-4-then-success testuser 'Password1!'
```
- Or write the input file manually (`io/InCollege_Input.txt`):
```
1
wronguser1
badpass1
wronguser2
badpass2
wronguser3
badpass3
wronguser4
badpass4
testuser
Password1!
```
- Run:
```
: > io/InCollege_Output.txt && ./incollege
```
- Expected output (console and file, identical):
```
Welcome to InCollege!
1. Login
2. Create Account
Enter Your Choice:
Enter Your Choice: 1
Please enter your username:
Please enter your password:
Incorrect username/password, please try again.
Please enter your username:
Please enter your password:
Incorrect username/password, please try again.
Please enter your username:
Please enter your password:
Incorrect username/password, please try again.
Please enter your username:
Please enter your password:
Incorrect username/password, please try again.
Please enter your username:
Please enter your password:
You have successfully logged in.
Welcome, testuser
```

Acceptance Criteria — Login Failure
- After each invalid username/password pair, prints: `Incorrect username/password, please try again.`
- Prompts again for username and password; retries are unlimited.
- Does not exit to the main menu or terminate until success or EOF.
- Output is mirrored exactly to `io/InCollege_Output.txt` and matches console output.

Notes
- The menu is always printed first. The program prints `Enter Your Choice:` (prompt), then echoes the chosen value (e.g., `Enter Your Choice: 1`) after reading it from the input file.
- Retries are unlimited; the input file controls how many attempts occur before success.
- Output is trimmed (no trailing spaces) and mirrored exactly to `io/InCollege_Output.txt`.
- To create different scenarios, change the input file accordingly and rerun `./incollege`.
