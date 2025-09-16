# Test Plan: InCollege Profile Management (Epic #2)

## Objective
Validate the end-to-end functionality of user profile management as defined in Epic #2. This includes testing profile creation, viewing, editing, input validation, and data persistence through a sequence of positive, negative, and edge case scenarios.

## How to Run
- Set SELECT INPUT-FILE in your COBOL program to point to the master input file for this test plan.
- Compile and run the program.
- Review `io/InCollege-Output.txt` and compare it against the detailed expected outcomes below.

## Test Matrix (Overview)

| # | Type | Description | Input File(s) |
|---------|------|-------------|---------------|
| 10 | Positive | Verifies a user can create a complete profile with all required and multiple optional fields. | input_10.txt |
| 11 | Positive | Confirms that profile data is saved and persists across separate application runs. | input_10.txt, input_11.txt |
| 12 | Positive | Ensures a user can successfully edit an existing profile and that the changes are saved correctly. | input_12.txt |
| 13 | Negative | Tests that the program rejects a blank entry for a required field (e.g., Last Name) and re-prompts for input. | input_13.txt |
| 14 | Negative | Checks that the program validates the Graduation Year, rejecting non-numeric and out-of-range values. | input_14.txt |
| 15 | Negative | Validates that the post-login menu gracefully handles invalid user input (e.g., non-existent options). | input_15.txt |
| 16 | Edge | Tests creating a minimal profile with only required fields, omitting all optional sections. | input_16.txt |
| 17 | Edge | Verifies the program correctly handles the maximum boundary of three entries for Experience and Education. | input_17.txt |
| 18 | Edge | Confirms the program flow when a user immediately skips adding optional Experience and Education entries. | input_18.txt |
| 19 | Edge | Checks that an Experience entry can be saved with its optional "Description" sub-field left blank. | input_19.txt |

## Detailed Test Cases

### 10: Complete Profile Creation (Positive)

**Purpose:** Verifies a user can create a complete profile with all required and multiple optional fields.

**Input File:**  input_10.txt

**Input:** The user successfully fills all required fields (First Name, Last Name, Major, University, Graduation Year) and adds an "About Me" section, multiple Experience entries, and multiple Education entries.

**Expected:** The profile saves successfully, and all entered information is displayed in a well-formatted manner.

### 11: Data Persistence (Positive)

**Purpose:** Confirms that profile data is saved and persists across separate application runs.

**Input Files:**  input_10.txt, input_11.txt

**Note:** This test requires two separate program runs.

**Input:**
- **Run 1:** Create and save a profile, then terminate the program.
- **Run 2:** Relaunch the program, log in with the same credentials, and select '2' to view the profile.

**Expected:** The profile data from Run 1 is loaded and displayed accurately in Run 2.

### 12: Profile Editing (Positive)

**Purpose:** Ensures a user can successfully edit an existing profile and that the changes are saved correctly.

**Input File:** input_12.txt

**Input:** The user logs in, views their existing profile, then edits various fields including required and optional sections.

**Expected:** The profile updates are saved successfully, and the changes are reflected when viewing the profile again.

### 13: Blank Required Field (Negative)

**Purpose:** Tests that the program rejects a blank entry for a required field (e.g., Last Name) and re-prompts for input.

**Input File:** input_13.txt

**Input:** After logging in and selecting '1', the user provides a blank input for "Last Name" before providing a valid one.

**Expected:** The program displays an error like "This field is required." and re-prompts for the Last Name.

### 14: Invalid Graduation Year (Negative)

**Purpose:** Checks that the program validates the Graduation Year, rejecting non-numeric and not 4 digits values.

**Input File:** input_14.txt

**Input:** During a profile edit, the user enters non-numeric text (ABCD), then an out-of-range year (180), before entering a valid year (2025).

**Expected:** The program displays an "Invalid year" error after the first two attempts and only proceeds after the valid year is entered.

### 15: Invalid Menu Navigation (Negative)

**Purpose:** Validates that the post-login menu gracefully handles invalid user input (e.g., non-existent options).

**Input File:** input_15.txt

**Input:** After logging in, the user enters an invalid choice ('5'), then another invalid choice ('abc'), before selecting a valid option.

**Expected:** The program displays an "Invalid option" message after each invalid attempt and re-displays the menu without crashing.

### 16: Minimal Profile Creation (Edge)

**Purpose:** Tests creating a minimal profile with only required fields, omitting all optional sections.

**Input File:** input_16.txt

**Input:** The user creates a profile, keeping only the required fields but providing blank input for "About Me" and using "DONE" to skip Experience and Education.

**Expected:** The profile saves correctly. The view displays only the required information (Name, University, etc.) without any optional sections.

### 17: Maximum Entries Boundary (Edge)

**Purpose:** Verifies the program correctly handles the maximum boundary of three entries for Experience and Education.

**Input File:** input_17.txt

**Input:** The user edits their profile to add exactly three Experience and three Education entries.

**Expected:** The program accepts all three entries for each section without error and does not prompt for a fourth. The subsequent view displays all entries correctly.

### 18: Skip Optional Entries (Edge)

**Purpose:** Confirms the program flow when a user immediately skips adding optional Experience and Education entries.

**Input File:** input_18.txt

**Input:** The user creates a profile and immediately skips adding Experience and Education entries by entering "DONE" when prompted.

**Expected:** The program handles the skip gracefully and saves the profile with only the required fields and any completed optional fields.

### 19: Blank Optional Description (Edge)

**Purpose:** Checks that an Experience entry can be saved with its optional "Description" sub-field left blank.

**Input File:** input_19.txt

**Input:** The user adds an Experience entry but leaves the optional "Description" field blank while filling in other required fields.

**Expected:** The Experience entry saves successfully without requiring the Description field, and displays correctly in the profile view.