# Epic #1 – InCollege: Log In, Part 1

## Objective
Lay the foundation for the InCollege application by implementing the **core authentication system**.  
This includes:
- User registration and login functionality
- File-driven input and output (screen + identical log file)
- Initial post-login navigation menus (Job Search, Find Someone, Learn a New Skill)

---

## Focus Areas

### 1. Modular Design
Break COBOL code into logical, reusable sections:
- **INIT-LOAD-ACCOUNTS** → load persisted users from file
- **CREATE-ACCOUNT** → new user flow, password validation, limit check, persist to file
- **LOGIN** → read username/password, validate, loop until success
- **CHECK-CREDENTIALS** → compare against persisted accounts
- **POST-LOGIN-MENU** → main menu after login
- **LEARN-SKILL** → skill menu with 5 items + Go Back
- **DISPLAY-AND-LOG** → ensure output is mirrored to screen and file
- **READ-NEXT-INPUT-LINE / WRITE-OUTPUT-LINE** → file I/O helpers
- **VALIDATE-PASSWORD** → enforce password rules

---

### 2. Input File Handling
- All user input comes from `io/InCollege-Input.txt`
- Input lines are consumed sequentially (username, password, menu selections)
- Supports scripting different test scenarios by rewriting the input file

---

### 3. Output File Handling
- All messages printed to console must also be written identically to `io/InCollege-Output.txt`
- **One helper only** (`DISPLAY-AND-LOG`) — no raw `DISPLAY` allowed
- Output must match the sample transcripts exactly (including punctuation and order)

---

### 4. Data Structures
- Store up to **5 user accounts** in memory
- Each record has:
  - Username
  - Password (plain string for now; hashing optional later)
- Track `USER-COUNT` to enforce the account limit

---

### 5. Persistence File I/O
- User accounts persisted in `data/users.txt`
- Format: `username|password`
- **INIT-LOAD-ACCOUNTS**: load existing users into memory on startup
- **APPEND-ACCOUNT**: write new accounts to `users.txt` immediately
- On startup, if file missing, program creates or falls back to an example file

---

### 6. Input Validation (for Create Account)
- Password requirements:
  - Length 8–12 characters
  - At least one uppercase letter
  - At least one digit
  - At least one special character (`!@#$%^&*?-_+`)
- Invalid inputs return error messages and re-prompt
- Usernames must be unique — reject duplicates

---

## Account Creation Flow
1. User selects **Create Account** from main menu
2. If `USER-COUNT >= 5`:
   - Print: `"All permitted accounts have been created, please come back later"`
   - Return to main menu
3. Prompt for username
   - If already exists, print error and re-prompt
4. Prompt for password
   - Run `VALIDATE-PASSWORD`
   - If invalid, print reason and re-prompt
5. On success:
   - Persist to `users.txt`
   - Increment `USER-COUNT`
   - Print `"Account created successfully."`
   - Return to main menu

---

## Login Flow
1. User selects **Login**
2. Prompt: `"Please enter your username:"`
3. Prompt: `"Please enter your password:"`
4. Compare with accounts in memory
   - **Success**: `"You have successfully logged in."` + `"Welcome, [Username]!"` → go to post-login menu
   - **Failure**: `"Incorrect username/password, please try again"` → re-prompt (unlimited retries)

---

## Post-Login Navigation
Once logged in, show the menu:
Search for a job

- **Search for a job** → `"Job search/internship is under construction."`
- **Find someone you know** → `"Find someone you know is under construction."`
- **Learn a new skill**:
  - Show 5 skills + `"Go Back"`
  - Selecting a skill → `"This skill is under construction."` → redisplay list
  - `"Go Back"` returns to post-login menu

---

## Deliverables
- **Roles.txt** – list team members and weekly roles
- **InCollege.cob** – working COBOL program
- **InCollege-Input.txt** – sample run inputs
- **InCollege-Test.txt** – test inputs (positive, negative, edge cases)
- **Sample Output File** – expected transcript
- **InCollege-Output.txt** – actual program output

---

## Testing Responsibilities
- **Positive cases**: valid login, valid account creation
- **Negative cases**: invalid password formats, duplicate usernames, bad login
- **Edge cases**: password exactly 8 or 12 characters, creation of 5th vs. 6th account
- **Output verification**: compare console vs. output file, must be identical
- **Bug reporting**: all discrepancies logged in Jira with repro steps
