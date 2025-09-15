# InCollege Project – Week 2 Deliverable

## Description

This repository contains the Week 2 deliverable for the InCollege Project.  
The objective of this milestone is to implement the **personal profiles management** functionality in COBOL. This is a crucial step for users to personalize their presence and eventually connect with others effectively

The program provides the following functionality:

- **Menu Navigation**: After logging in, users have option to create/view/edit their personal profile.

- **Profile Creation**: Users can create a personal profile with basic fields: First Name, Last Name, Major, University, Graduation Year. The profile also includes optional sections: About Me, Experience, Education.

- **Profile Viewing**: Users can view their complete profile information in a formatted manner.

- **Profile Editing**: Users can edit any field in their profile. The program validates inputs (e.g., email format, graduation year range).

- **Data Persistence**: Profiles are saved to `data/profiles.txt` in a structured format. On startup, existing profiles are loaded into memory. User's profile must be linked to their account created in Week 1 (`data/users.txt`).

This milestone also includes tester responsibilities:

- Development of comprehensive test input/output files (positive, negative, edge cases). All cases are created/stored in `tests/` directory with pattern `Input_<id>.txt` and `Output_<id>.txt`.
- Verification that outputs in console and output file match exactly.
- Logging bugs in Jira when discrepancies are found.

## Installation

1. To run Cobol, first install a COBOL compiler. Common choice: GnuCOBOL (open-cobol), available via package managers.

- Linux/macOS: `brew install gnu-cobol` or `apt-get install open-cobol`
- Windows: GnuCOBOL binaries exist, or run inside WSL

2. Verify installation:

- `cobc -v`

3. Compile the program:

- `cobc -x src/InCollege.cob -o incollege`

4. Run the program:

- Command: `./incollege`

There are 2 modes of operation:

- **Interactive**: The program reads directly from keyboard and writes to console.
- **File-driven** (per spec): All input comes from `io/InCollege-Input.txt` and every printed line is mirrored to `io/InCollege-Output.txt`. This mode enables deterministic testing.

## Docs

- Project Breakdown (source spec): `docs/epic2/project-breakdown.md`
- Architecture (sections/paragraphs layout): `docs/epic2/architecture.md`
