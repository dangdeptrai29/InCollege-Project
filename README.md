## InCollege Project – Week 10 (Final) Deliverable

### Description

This repository contains the Week 10 deliverable for the InCollege Project. The objective of this milestone is system enhancements & bug fixing. The application has been tested for bugs, and quality-of-life enhancements have been implemented to ensure stability and user-friendliness.

This version represents the culmination of Weeks 1 through 9. To ensure no previous functionality has been broken by recent changes, we utilize a full Regression Testing Suite and complement this with Exploratory Testing

### Key Features

The InCollege application now supports the full suite of user stories implemeted throughout the semester:

- **User Management**
  - Registration: Create unique accounts with password validation (uppercase, digit, special character)
  - Authentication: Secure login with credential verification

- **Profile System**
  - Create/Edit: Users can add details including University, Major, About Me, and lists for Experience and Education
  - View Profile: Display profile details for self and searched users

- **Job System**
  - Post Jobs: Users can post job opportunities (persisted to file)
  - Browse & Apply: View available jobs and apply for them
  - Application History: View a summary of jobs the user has applied to

- **Message System**
  - Send Message: Send private messages to connected users
  - View Inbox: View received messages, sorted chronologically by timestamp

- **Quality-of-life Enhancements (Week 10)**
  - Universal Navigation: Added "Go Back" options to sub-menus
  - Data Persistence: All Data (Users, Profiles, Connections, Jobs, Applications, Messages) is saved to text files
  - Input/Output: All user interactions are read from `io/InCollege-Input.txt` and mirrored to `io/InCollege-Output.txt`

### Installation & Usage

#### Prerequisites

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

#### Compile Program

```bash
cobc -x -free -o InCollegeTest src/InCollege.cob
```

#### Run Program

```bash
./InCollegeTest
```

### Automated Testing (Recommended)

#### Run All Epic 10 Test Cases

To verify the stability of the application, we utilize robust scripts for both formal regression and unstructured exploratory testing.

1. Regression Testing

This script (`run_epic10_tests.sh`) runs formally predefined test cases from Epics 1-9 to confirm existing functionality remains intact after bug fixes and enhancements.

- To run:

```bash
scripts/run_epic10_tests.sh
```

- Purpose: Confirms that bug fixes haven't introduced regressions.
- Process: Automatically handles data setup, execution, comparison against expected outputs, and generates a Pass/Fail summary.

2. Exploratory Testing

This script (`run_all.sh`) is designed to run unstructured and edge case test scenarios (TC01 through TC15) used in exploratory testing to discover unexpected behaviors.

- To run:

```bash
tests/epic10/exploratory/run_all.sh
```

- Purpose: Executes non-formal test cases to cover edge cases and unexpected behaviors.
- Process: 
  - Wipes and resets the data/ and io/ directories for each test run.
  - Loads setup files from tests/epic10/exploratory/setups/$tc/ if present.
  - Runs the InCollege executable.
  - Saves the resulting InCollege-Output.txt to tests/epic10/exploratory/outputs/ for manual review.

### Sample Input/Output

#### Sample Input (`io/InCollege-Input.txt`)

```text
1
AdminUser
AdminPass1!
7
2
1
1
0
3
4
8
2
3
9
```

#### Sample Output (`io/InCollege-Output.txt`)


### Repository Structure

- `src/` → `InCollege.cob` (Main source code)
- `data/` → `users.txt`, `profiles.txt`, `connections.txt`, `jobs.txt`, `applications.txt`, `messages.txt`, `requests.txt`, `search_history.txt`
- `io/` → Active I/O files (`InCollege-Input.txt`, `InCollege-Output.txt`)
- `scripts/run_epic10_tests.sh` → Main regression testing script.
- `tests/epic10/exploratory/run_all.sh` → Script for running exploratory test cases.
- `tests/epic10/regression/` → Inputs and expected outputs for formal regression.
- `tests/epic10/exploratory/` → Inputs, setups, and generated outputs for exploratory tests.
- `submission/week10/` → Deliverables.

### Deliverables Summary

| # | Deliverable                    | Description                                       |
|---|--------------------------------|---------------------------------------------------|
| 1 | `Roles.txt`                    | Roles of all team members                         |
| 2 | `InCollege.cob`                | Working COBOL program        |
| 3 | `InCollege-Input.txt`          | Sample input demonstrating all features        |
| 4 | `InCollege-Output.txt`         | Sample output        |
| 5 | `Epic10-Tests.zip`  | Screenshot of the Jira board                       |
| 6 | `Jira.jpg` | Actual generated test outputs                     |
| 7 | `Burndown.jpg`                      | Sprint Burndown charts        |
| 8 | `GitHub.jpg`                    | Screenshot of GitHub commit history               |