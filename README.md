# InCollege Project – Week 5 Deliverable

## Description

This repository contains the **Week 5 deliverable** for the InCollege Project.  
The objective of this milestone is to complete the connection system by enabling users to **accept or reject pending connection requests** and **view their established network**.  
All inputs are read from file, all outputs are displayed on screen and duplicated into a file for validation.

---

## Key Features

### ✅ Manage Pending Connection Requests
- View all pending connection requests (from Week 4).  
- Accept or reject each request individually.
- Accepting a request:
  - Creates a permanent connection between the two users.
  - Removes that request from the pending list.
- Rejecting a request:
  - Simply removes it from the pending list.
- The user receives confirmation after each action.

### ✅ Display Established Network
- A new menu option `5. View My Network` shows all users currently connected to the logged-in user.
- Each connection displays at least the connected user's full name and may include their University and Major.

### ✅ File I/O Requirements
- **Input**: All inputs are read from `io/InCollege-Input.txt`.
- **Output**: All screen output is mirrored to `io/InCollege-Output.txt`.
- This ensures reproducible, automated testing.

---

## Tester Responsibilities

Testers are responsible for verifying every new feature end-to-end:

1. **Develop Test Cases**:
   - Accept single connection.
   - Reject single connection.
   - Mixed accept/reject scenario.
   - Network display after accepted requests.
   - Edge cases (no pending requests, multiple requests).

2. **Run Automated Tests**:
   Use the helper script to compile, seed test data, and run each case.

3. **Verify Outputs**:
   Compare program console vs. output files — they must be identical.

---

## Installation & Usage

### Prerequisites
Install GnuCOBOL on Linux/macOS/WSL:
```bash
sudo apt-get install open-cobol
# or
brew install gnu-cobol
```

Check version:
```bash
cobc -v
```

---

### Compile Program
```bash
cobc -x -free -o InCollegeTest src/InCollege.cob
```

---

### Run Program
```bash
./InCollegeTest
```

---

## Automated Testing (Recommended)

### Run All Epic 5 Test Cases

To automatically compile, seed data, and run every Epic 5 test case:

```bash
scripts/run_epic5_generate_outputs.sh
```

This will:
- Reset persistent data (`data/users.txt`, `data/requests.txt`, `data/connections.txt`).
- Automatically execute every file in `tests/epic5_inputs/`.
- Save corresponding results in `tests/epic5_outputs/`.

Each run prints confirmation lines like:
```
🟩 Generated: tests/epic5_outputs/TC01_accept_single.out.txt
```

---

### Manual Testing

To test individual cases:
```bash
cp tests/epic5_inputs/TC01_accept_single.txt io/InCollege-Input.txt
:> io/InCollege-Output.txt
./InCollegeTest
diff -u tests/epic5_expected/TC01_accept_single.out.txt io/InCollege-Output.txt
```

If there’s no difference, the test passes silently.
If there’s a mismatch, `diff` will print the differing lines.

---

## Sample Input/Output

### Sample Input (`InCollege-Input.txt`)
```
1
TestUser
Password123!
4
5
```

### Sample Output (`InCollege-Output.txt`)
```
Welcome to InCollege!
1. Log In
2. Create New Account
Enter your choice:
Please enter your username:
Please enter your password:
You have successfully logged in.
Welcome, TestUser!
1. View My Profile
2. Search for User
3. Learn a New Skill
4. View My Pending Connection Requests
5. View My Network
Enter your choice:
--- Pending Connection Requests ---
Request from: OtherUser
1. Accept
2. Reject
Enter your choice for OtherUser:
Connection request from OtherUser accepted!
-----------------------------------
1. View My Profile
2. Search for User
3. Learn a New Skill
4. View My Pending Connection Requests
5. View My Network
Enter your choice:
--- Your Network ---
Connected with: OtherUser (University: Another U, Major: Marketing)
Connected with: FriendB (University: Big State, Major: Engineering)
--------------------
1. View My Profile
2. Search for User
3. Learn a New Skill
4. View My Pending Connection Requests
5. View My Network
Enter your choice:
--- END_OF_PROGRAM_EXECUTION ---
```

---

## Repository Structure

```
src/                     → COBOL source files
data/                    → User, profile, request, connection data
io/                      → Active input/output files for program runs
tests/epic5_inputs/      → Input test cases for Week 5
tests/epic5_outputs/     → Generated outputs
scripts/                 → Automation scripts (including run_epic5_generate_outputs.sh)
submission/epic5/        → Zipped deliverables for final submission
```

---

## Deliverables Summary

| # | Deliverable | Description |
|---|--------------|-------------|
| 1 | `Roles.txt` | Roles of all team members |
| 2 | `InCollege.cob` | Working COBOL source file |
| 3 | `InCollege-Input.txt` | Sample input |
| 4 | `InCollege-Output.txt` | Sample output |
| 5 | `Epic5-Storyx-Test-Input.zip` | Compressed test input set |
| 6 | `Epic5-Storyx-Test-Output.zip` | Actual generated test outputs |
| 7 | `Jira.jpg` | Screenshot of updated Jira board |
| 8 | `GitHub.jpg` | Screenshot of GitHub commit history |

---

## Submission Instructions

1. Ensure all tests pass:
   ```bash
   scripts/run_epic5_generate_outputs.sh
   ```
2. Zip deliverables:
   ```bash
   zip -r submission/epic5/Epic5-Storyx-Test-Input.zip tests/epic5_inputs
   zip -r submission/epic5/Epic5-Storyx-Test-Output.zip tests/epic5_outputs
   ```
3. Commit & push your final work:
   ```bash
   git add .
   git commit -m "Finalize Week 5 deliverable: Connection acceptance/rejection & network display"
   git push origin epic5/test
   ```
4. Upload your zipped files and screenshots to Canvas.

---

## Documentation

- **Architecture Notes:** `docs/epic5/architecture.md`
- **Test Cases:** `docs/epic5/test_cases.md`
- **Automation Script:** `scripts/run_epic5_generate_outputs.sh`
- **Data Samples:** `data/users.txt`, `data/profiles.txt`, `data/requests.txt`, `data/connections.txt`
