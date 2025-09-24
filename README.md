# InCollege Project – Week 3 Deliverable

## Description

This repository contains the Week 3 deliverable for the InCollege Project. The objective of this milestone is to implement profile viewing and basic user search functionality in COBOL. This is a critical step towards fostering connections within the InCollege community and enabling users to effectively view their complete profile information.

### Key Features

The program provides the following functionality:

- **Enhanced Profile Viewing**: Users can now reliably view their complete profile information including all fields from Week 2 (First Name, Last Name, University/College, Major, Graduation Year, About Me, Experience entries, and Education entries) in a clearly formatted, readable console display.

- **Basic User Search**: The "Find someone you know" option is now fully functional, allowing users to search for other registered InCollege users by their exact full name (e.g., "John Doe").

- **Search Results Display**: When a match is found, the system displays the complete profile of the found user. When no match is found, the system informs the user appropriately.

- **Data Persistence**: Profile data continues to be stored in `data/profiles.txt` and linked to user accounts in `data/users.txt` from Week 1. Search functionality reads through existing user data efficiently.

- **File I/O Operations**: All user input (menu selections, search queries) is read from `io/InCollege-Input.txt` and all program output is both displayed on screen and written to `io/InCollege-Output.txt` for testing consistency.

### Tester Responsibilities

This milestone also includes comprehensive tester responsibilities:

- Development of extensive test input/output files covering positive cases (successful profile viewing, successful user searches), negative cases (searching for non-existent users), and edge cases (users with long names, partial name searches). All test cases are stored in `tests/` directory with pattern `Input_<id>.txt` and `Output_<id>.txt` for cases 10-19.

- Verification that console output and output file content match exactly for all profile viewing and search scenarios.

## Installation

To run COBOL, first install a COBOL compiler. Common choice: GnuCOBOL (open-cobol), available via package managers.

### Prerequisites

- **Linux/macOS**: `brew install gnu-cobol` or `apt-get install open-cobol`
- **Windows**: GnuCOBOL binaries exist, or run inside WSL

### Verify Installation

```bash
cobc -v
```

### Compile the Program

```bash
cobc -x src/InCollege.cob -o incollege
```

### Run the Program

```bash
./incollege
```

### Important: Input File Configuration

- **Note**: The program reads ALL input from `io/InCollege-Input.txt`. What you see when running `./incollege` depends entirely on what's in this file.

**For the sample demo flow** (profile viewing + search):
```bash
# Set up sample input
echo -e "1\nTestUser\nPassword1!\n2\n4\nAnother Student\n4\nNonExistentUser\n6" > io/InCollege-Input.txt
./incollege
```

**For specific test cases**:
```bash
# Run test case 10 (example)
cp tests/Input_10.txt io/InCollege-Input.txt
./incollege

# Run test case 12 (example)  
cp tests/Input_12.txt io/InCollege-Input.txt
./incollege
```

### Modes of Operation

There are 2 modes of operation:

1. **Interactive**: The program reads directly from keyboard and writes to console.
2. **File-driven (per spec)**: All input comes from `io/InCollege-Input.txt` and every printed line is mirrored to `io/InCollege-Output.txt`. This mode enables deterministic testing for profile viewing and search functionality.

## New Features in Week 3

### Enhanced Profile Viewing

- Complete profile display including all optional fields (About Me, Experience, Education)
- Formatted console output for improved readability
- Reliable retrieval of all profile information stored in Week 2

### Basic User Search Functionality

- Exact name matching search capability
- Full profile display for found users
- Appropriate messaging when no user matches the search query
- Integration with existing menu navigation system

## Sample Usage

After logging in, users will see an updated menu with these options:

1. Create/Edit My Profile
2. View My Profile (enhanced functionality)
3. Search for a job
4. Find someone you know (new functionality)
5. Learn a New Skill

When selecting "View My Profile", users see a formatted display of their complete profile. When selecting "Find someone you know", users can enter a full name to search for other users in the system.

## Testing

### Epic 3 Test Suite
The project includes comprehensive test coverage with 10 test cases (10-19):

- **Tests 10-12**: Positive cases (exact matches, profile viewing)
- **Tests 13-15**: Negative cases (non-existent users, partial names, case sensitivity)  
- **Tests 16-19**: Edge cases (long names, punctuation, duplicates, blank input)

### Running Tests
```bash
# Compile program
cobc -x src/InCollege.cob -o incollege

# Run individual test (example: test 10)
cp tests/Input_10.txt io/InCollege-Input.txt
./incollege < io/InCollege-Input.txt > io/result.txt 2>&1
diff tests/Output_10.txt io/result.txt

## Documentation

- **Architecture (implementation details)**: `docs/epic3/architecture.md`
- **Test Cases (comprehensive test suite)**: `docs/epic3/test_cases.md`
