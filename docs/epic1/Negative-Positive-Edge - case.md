## Test Plan: InCollege End-to-End Scenarios (Positive, Negative, Edge)

### Objective
Validate core functionality across account creation, login, and post-login navigation using a mix of positive, negative, and edge cases. This plan pairs each input with its scenario and expected output for fast verification.

### How to Run
- Set SELECT INPUT-FILE in `incollege.cob` to `InCollege-Test.txt`.
- Delete any existing `data/users.txt` to ensure a clean state.
- Compile and run the program.
- Review `io/InCollege-Output.txt` and compare against the expected outcomes below.

---

### Test Matrix (Overview)
| # | Area         | Scenario                   | Input (condensed)        | Expected                      |
|---|--------------|----------------------------|--------------------------|-------------------------------|
| 1 | Registration | Too short password         | `2, newuser, short`      | Reject: password requirements |
| 2 | Registration | Missing special char       | `2, newuser, NoSpecial1` | Reject: password requirements |
| 3 | Registration | Min length accepted (8)    | `2, edge1, Minimum1!`    | Account created               |
| 4 | Registration | Max length accepted (12)   | `2, edge2, Maximum123!!` | Account created               |
| 5 | Registration | Valid account (fill slots) | `2, user3, Perfect9@`    | Account created               |
| 6 | Registration | Valid account (fill slots) | `2, user4, Another2#`    | Account created               |
| 7 | Registration | Fill last slot (5th)       | `2, user5, LastOne3$`    | Account created (5 total)     |
| 8 | Registration | Exceed account limit (6th) | `2, user6, ShouldFail4%` | Reject: limit reached         |
| 9 | Login        | Incorrect credentials      | `1, wronguser, wrongpass`| Reject: incorrect creds       |
| 10| Login        | Successful login           | `1, user3, Perfect9@`    | Logged in + welcome + menu    |
| 11| Post-login   | Navigate options 1-4       | `1,2,3,4`                | Under construction + skills menu |

---

### Detailed Test Cases

#### 1–2. Invalid Password Registration
- **Purpose**: Enforce password rules.
- **Inputs**:
  - Too short: `2, newuser, short`
  - Missing special character: `2, newuser, NoSpecial1`
- **Expected**: `Password does not meet requirements. Please try again.`

#### 3. Minimum Password Length Accepted (8 chars)
- **Input**:
  - Action: `2` (Create New Account)
  - Username: `edge1`
  - Password: `Minimum1!`
- **Expected**:
  - `Account created successfully!`

#### 4. Maximum Password Length Accepted (12 chars)
- **Input**:
  - Action: `2`
  - Username: `edge2`
  - Password: `Maximum123!!`
- **Expected**:
  - `Account created successfully!`

#### 5–6. Standard Valid Accounts
- **Inputs**:
  - `2, user3, Perfect9@`
  - `2, user4, Another2#`
- **Expected**: Both accounts created successfully.

#### 7. Fill the Last Account Slot (5 total)
- **Note**: Assumes `WS-MAX-ACCOUNTS = 5`.
- **Input**:
  - Action: `2`
  - Username: `user5`
  - Password: `LastOne3$`
- **Expected**:
  - `Account created successfully!`
  - Total accounts = 5.

#### 8. Exceed Account Limit (Attempt 6th)
- **Input**:
  - Action: `2`
  - Username: `user6`
  - Password: `ShouldFail4%`
- **Expected**:
  - `All permitted accounts have been created, please come back later.`

#### 9. Failed Login Attempt
- **Input**: `1, wronguser, wrongpass`
- **Expected**: `Incorrect username/password, please try again.`

#### 10. Successful Login after Failed Login
- **Input**: `user3, Perfect9@`
- **Expected**:
  - `You have successfully logged in.`
  - `Welcome, user3!`
  - Post-login menu is displayed.

#### 11. Post-Login Navigation
- **Input sequence** (from post-login menu): `1, 2, 3, 4`
- **Expected**:
  - After `1`: `Job search/internship is under construction.`
  - After `2`: `Find someone you know is under construction.`
  - After `3`: `Learn a New Skill` menu is displayed.
  - After `4`: `This skill is under construction.`

---

### Notes
- Keep the input order exactly as listed to properly exercise limit checks and subsequent login tests.
- The login in Test 10 intentionally uses `user3` created in Test 5 for continuity and consistency.

