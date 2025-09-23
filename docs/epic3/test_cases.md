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

## Execution Status (Step 4)
Engineering has not yet delivered the Epic 3 search functionality—option `3` still prints "Search for User is under construction." Because of this block, the scripted cases have not been executed. Once the implementation is available, we will run each input file, diff actual vs. expected output, and log results under Step 4 of the test plan.
