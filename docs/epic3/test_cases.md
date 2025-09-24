# Epic 3 Search Test Suite (Cases 10–19)

## Overview
These automated cases exercise the Week 3 requirement for the "Find someone you know" feature. Each test is a pair of scripted files under `tests/` (`Input_<id>.txt`, `Output_<id>.txt`) that drives the COBOL console application through login and the basic user search flow. The suite spans positive, negative, and edge scenarios so that we can validate result accuracy and integration once engineering lands the search implementation.

## Scenario Summary
| ID | Type | Purpose | Input Path | Expected Output Path | Expected Behaviour |
|----|------|---------|------------|----------------------|--------------------|
| 10 | Positive | Exact-match lookup for a fully populated profile | `tests/Input_10.txt` | `tests/Output_10.txt` | Displays Alex Johnson's complete profile with experience and education entries. |
| 11 | Positive | Exact-match lookup for minimal optional data | `tests/Input_11.txt` | `tests/Output_11.txt` | Renders Mina Kim's profile showing `Experience: None` and `Education: None`. |
| 12 | Positive | View-own-profile followed by searching a multi-entry profile | `tests/Input_12.txt` | `tests/Output_12.txt` | Shows the logged-in user's profile, returns to menu, then prints Jordan Blake's profile with multiple entries. |
| 13 | Negative | Search for a non-existent user | `tests/Input_13.txt` | `tests/Output_13.txt` | System reports "No one by that name could be found." |
| 14 | Negative | Partial-name query | `tests/Input_14.txt` | `tests/Output_14.txt` | Treated as no match; no profile displayed. |
| 15 | Negative | Case-variation query | `tests/Input_15.txt` | `tests/Output_15.txt` | Lowercase input does not match mixed-case record; returns not found. |
| 16 | Edge | Maximum-length full name | `tests/Input_16.txt` | `tests/Output_16.txt` | Presents the long name profile without truncation. |
| 17 | Edge | Name containing punctuation | `tests/Input_17.txt` | `tests/Output_17.txt` | Matches Jean-Luc O'Connor and prints profile with punctuation intact. |
| 18 | Edge | Duplicate full names | `tests/Input_18.txt` | `tests/Output_18.txt` | Returns the first stored Jamie Lee profile consistently. |
| 19 | Edge | Blank/whitespace search input | `tests/Input_19.txt` | `tests/Output_19.txt` | Handles blank input gracefully by reporting no match and returning to menu. |

## Data & Environment Assumptions
- Test inputs assume the dataset includes the named accounts and profiles (Alex Johnson, Mina Kim, Jordan Blake, etc.). If they are absent, expected outputs will differ; this should be logged as a defect or fixture gap.
- Search is expected to perform a trimmed, case-sensitive exact match on the concatenated first and last name. Deviations will be captured during execution as either defects or requirement clarifications.
- When duplicate names exist, the system should return the first persisted record; any alternate behaviour requires documentation.

## Execution Commands

### Run Individual Tests
````bash
# Test 10 - Exact-match lookup for fully populated profile
cp tests/Input_10.txt io/InCollege-Input.txt && ./incollege < io/InCollege-Input.txt > io/result.txt 2>&1 && diff tests/Output_10.txt io/result.txt

# Test 11 - Exact-match lookup for minimal optional data  
cp tests/Input_11.txt io/InCollege-Input.txt && ./incollege < io/InCollege-Input.txt > io/result.txt 2>&1 && diff tests/Output_11.txt io/result.txt

# Test 12 - View-own-profile followed by searching multi-entry profile
cp tests/Input_12.txt io/InCollege-Input.txt && ./incollege < io/InCollege-Input.txt > io/result.txt 2>&1 && diff tests/Output_12.txt io/result.txt

# Test 13 - Search for non-existent user
cp tests/Input_13.txt io/InCollege-Input.txt && ./incollege < io/InCollege-Input.txt > io/result.txt 2>&1 && diff tests/Output_13.txt io/result.txt

# Test 14 - Partial-name query
cp tests/Input_14.txt io/InCollege-Input.txt && ./incollege < io/InCollege-Input.txt > io/result.txt 2>&1 && diff tests/Output_14.txt io/result.txt

# Test 15 - Case-variation query
cp tests/Input_15.txt io/InCollege-Input.txt && ./incollege < io/InCollege-Input.txt > io/result.txt 2>&1 && diff tests/Output_15.txt io/result.txt

# Test 16 - Maximum-length full name
cp tests/Input_16.txt io/InCollege-Input.txt && ./incollege < io/InCollege-Input.txt > io/result.txt 2>&1 && diff tests/Output_16.txt io/result.txt

# Test 17 - Name containing punctuation
cp tests/Input_17.txt io/InCollege-Input.txt && ./incollege < io/InCollege-Input.txt > io/result.txt 2>&1 && diff tests/Output_17.txt io/result.txt

# Test 18 - Duplicate full names
cp tests/Input_18.txt io/InCollege-Input.txt && ./incollege < io/InCollege-Input.txt > io/result.txt 2>&1 && diff tests/Output_18.txt io/result.txt

# Test 19 - Blank/whitespace search input
cp tests/Input_19.txt io/InCollege-Input.txt && ./incollege < io/InCollege-Input.txt > io/result.txt 2>&1 && diff tests/Output_19.txt io/result.txt
````

### Quick Test Validation
````bash
# Quick pass/fail check for all tests
for id in {10..19}; do
  cp tests/Input_${id}.txt io/InCollege-Input.txt
  ./incollege < io/InCollege-Input.txt > io/result.txt 2>&1
  if diff -q tests/Output_${id}.txt io/result.txt > /dev/null; then
    echo "Test $id: ✅ PASSED"
  else
    echo "Test $id: ❌ FAILED"
  fi
done
````

## Execution Status (Step 4)
Search implementation is now available. After updating profile display formatting and list serialization, we re-ran Inputs_10-19 using `./incollege` with seeded `data/profiles.txt`; all scenarios now match the Epic 3 specification.

### Result Summary
| ID | Result | Notes |
|----|--------|-------|
| 10 | PASS | Exact-match search displays full profile with Name/Graduation Year and multi-line sections. |
| 11 | PASS | Minimal profile shows `About Me:` blank, with Experience/Education reported as `None`. |
| 12 | PASS | View + search flow renders multi-entry experience and education blocks with indentation. |
| 13 | PASS | Non-existent user handled correctly with not-found message. |
| 14 | PASS | Partial-name query returns not-found as expected. |
| 15 | PASS | Case-sensitive lookup confirmed; lowercase query fails cleanly. |
| 16 | PASS | Long-name profile formatted correctly with graduation year and wrapped sections. |
| 17 | PASS | Hyphen/apostrophe name renders correctly with full details. |
| 18 | PASS | Duplicate-name scenario returns profile with correct formatting. |
| 19 | PASS | Blank input treated as no-match and menu resumes. |