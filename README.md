## InCollege Project – Week 8 Deliverable

### Description

This repository contains the Week 8 deliverable for the InCollege Project. The objective of this milestone is to implement the Basic Messaging System (Part 1), enabling users to send private messages to other users they are already connected with.

All inputs are read from file, and all outputs are displayed on screen and duplicated into an output file for verification and automated testing.

### Key Features

- **Messaging System – Part 1 (Send Message)**
  - A new Messages option is added to the main post-login menu.
  - Upon selecting Messages, users will see:
    1. Send a New Message
    2. View My Messages (under construction for this week)

- **Sending a Message**
  - The user selects Send a New Message and enters the recipient’s username.
  - The program verifies:
    - The recipient exists and is already connected to the sender.
    - If not connected or not found, an appropriate error is displayed.
  - After validation, the user is prompted to enter message content (free-form text).
  - Once entered:
    - The message is persistently saved in a data file.
    - Confirmation is displayed on screen and recorded in the output file.

- **Message Persistence**
  - Each message record includes:
    - Sender’s Username
    - Recipient’s Username
    - Message Content
    - (Optional) Timestamp of when the message was sent
  - Messages remain available even after the program exits (for Week 9 retrieval).

- **File I/O Requirements**
  - Input: All inputs are read from `io/InCollege-Input.txt`.
  - Output: All screen output is mirrored to `io/InCollege-Output.txt`.
  - Ensures reproducible and verifiable test runs.

### Tester Responsibilities

Testers are responsible for verifying all new message-sending functionality end-to-end.

- **Develop Test Cases**
  -  Positive cases (sending messages to valid connections)
  -  Negative cases:
    - Sending to non-existent users
    - Sending to users not in network
    - Sending to pending connections
    - Sending overly long messages (edge case)
  - Persistence test: Verify that sent messages are saved correctly.

- **Run Automated Tests**
  - Use the provided script to compile, seed data, and run each case.

- **Verify Outputs**
  - Compare the console and output files — they must be identical.

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

#### Run All Epic 8 Test Cases

To automatically compile, seed data, and run every Epic 8 test case:

```bash
scripts/run_epic8_generate_outputs.sh
```

This will:

- Reset persistent data (`data/users.txt`, `data/connections.txt`, `data/messages.txt`)
- Automatically execute every file in `tests/epic8/inputs/`
- Save outputs to `tests/epic8/outputs/`

Each successful test shows:

```text
🟩 Generated: tests/epic8_outputs/TC01_send_valid_connection.out.txt
```

### Sample Input/Output

#### Sample Input (`io/InCollege-Input.txt`)

```text
1
SendingUser
Password123!
6
1
ConnectedUser
Hello there! How are you?
3
```

#### Sample Output (`io/InCollege-Output.txt`)


### Repository Structure

- `src/` → COBOL source files
- `data/` → Users, connections, and messages data
- `io/` → Active input/output files for program runs
- `tests/epic8/inputs/` → Input test cases for Week 8
- `tests/epic8/outputs/` → Generated outputs
- `scripts/`
- `submission/epic8/` → Zipped deliverables for submission

### Deliverables Summary

| # | Deliverable                    | Description                                       |
|---|--------------------------------|---------------------------------------------------|
| 1 | `Roles.txt`                    | Roles of all team members                         |
| 2 | `InCollege.cob`                | Working COBOL program with Messaging menu         |
| 3 | `InCollege-Input.txt`          | Sample input demonstrating message sending        |
| 4 | `InCollege-Output.txt`         | Sample output of message sent successfully        |
| 5 | `Epic8-Storyx-Test-Input.zip`  | Compressed test input files                       |
| 6 | `Epic8-Storyx-Test-Output.zip` | Actual generated test outputs                     |
| 7 | `Jira.jpg`                      | Screenshot of updated Jira board (Epic #8)        |
| 8 | `GitHub.jpg`                    | Screenshot of GitHub commit history               |