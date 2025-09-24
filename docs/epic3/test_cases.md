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

## Execution Command
````bash
for id in 10 11 12 13 14 15 16 17 18 19; do
  cp tests/Input_${id}.txt io/InCollege-Input.txt
  rm -f io/InCollege-Output.txt
  ./incollege > /tmp/case_${id}.log
  diff -u tests/Output_${id}.txt io/InCollege-Output.txt || true
  cat /tmp/case_${id}.log
done
````

## Execution Status (Step 4)
Search implementation is now available. We executed Inputs_10-19 using `./incollege` with seeded `data/profiles.txt`. Positive cases (10, 11, 12, 16, 17, 18) failed because the program emits separate First/Last name lines, omits Graduation Year, and collapses experience/education into serialized strings—diverging from the Epic 3 formatting requirements. Negative cases (13, 14, 15, 19) passed. Defect tickets need to be filed for the formatting regressions.

### Result Summary
| ID | Result | Notes |
|----|--------|-------|
| 10 | FAIL | Program splits first/last name, omits graduation year, and prints serialized experience/education (expected single `Name:` line with multi-line sections). |
| 11 | FAIL | Same formatting defect as case 10; also leaves Experience/Education blank instead of `None`. |
| 12 | FAIL | View-profile loses indentation; search portion repeats name/grad/serialization issues. |
| 13 | PASS | Non-existent user yields correct not-found message and menu loop. |
| 14 | PASS | Partial-name query treated as no match; behavior matches spec. |
| 15 | PASS | Case-sensitive search confirmed; lowercase query returns not-found as expected. |
| 16 | FAIL | Long-name profile still shows split names, missing grad year, serialized sections. |
| 17 | FAIL | Hyphen/apostrophe profile has same formatting defects (split names, serialized details). |
| 18 | FAIL | Duplicate-name scenario returns profile but with formatting defects (split names, no grad year). |
| 19 | PASS | Blank/whitespace input handled as not-found; output matches expectation. |

### Case 10 – Output Comparison
**Actual (`io/InCollege-Output.txt` after running `Input_10`):**
````text
Welcome to InCollege!
1. Log In
2. Create New Account
Enter your choice:
Please enter your username:
Please enter your password:
You have successfully logged in.
Welcome, testuser!
1. Create/Edit My Profile
2. View My Profile
3. Search for a job
4. Find someone you know
5. Learn a New Skill
Enter your choice:
Enter the full name of the person you are looking for:
--- Found User Profile ---
First Name: Alex
Last Name:  Johnson
University: State University
Major:      Computer Science
About:      Passionate about software development.
Experience: Intern~Tech Solutions~Summer 2024~Assisted with front-end development.
Education:  Bachelor's~State University~2021-2025
1. Create/Edit My Profile
2. View My Profile
3. Search for a job
4. Find someone you know
5. Learn a New Skill
Enter your choice:
````

**Expected (`tests/Output_10.txt`):**
````text
Welcome to InCollege!
1. Log In
2. Create New Account
Enter your choice:
Please enter your username:
Please enter your password:
You have successfully logged in.
Welcome, testuser!
1. Create/Edit My Profile
2. View My Profile
3. Search for a job
4. Find someone you know
5. Learn a New Skill
Enter your choice:
Enter the full name of the person you are looking for:
--- Found User Profile ---
Name: Alex Johnson
University: State University
Major: Computer Science
Graduation Year: 2025
About Me: Passionate about software development.
Experience:
  Title: Intern
  Company: Tech Solutions
  Dates: Summer 2024
  Description: Assisted with front-end development.
Education:
  Degree: Bachelor's
  University: State University
  Years: 2021-2025
-------------------------
1. Create/Edit My Profile
2. View My Profile
3. Search for a job
4. Find someone you know
5. Learn a New Skill
Enter your choice:
````

### Case 11 – Output Comparison
**Actual (`io/InCollege-Output.txt` after running `Input_11`):**
````text
Welcome to InCollege!
1. Log In
2. Create New Account
Enter your choice:
Please enter your username:
Please enter your password:
You have successfully logged in.
Welcome, testuser!
1. Create/Edit My Profile
2. View My Profile
3. Search for a job
4. Find someone you know
5. Learn a New Skill
Enter your choice:
Enter the full name of the person you are looking for:
--- Found User Profile ---
First Name: Mina
Last Name:  Kim
University: Coastal University
Major:      Marketing
About:
Experience:
Education:
1. Create/Edit My Profile
2. View My Profile
3. Search for a job
4. Find someone you know
5. Learn a New Skill
Enter your choice:
````

**Expected (`tests/Output_11.txt`):**
````text
Welcome to InCollege!
1. Log In
2. Create New Account
Enter your choice:
Please enter your username:
Please enter your password:
You have successfully logged in.
Welcome, testuser!
1. Create/Edit My Profile
2. View My Profile
3. Search for a job
4. Find someone you know
5. Learn a New Skill
Enter your choice:
Enter the full name of the person you are looking for:
--- Found User Profile ---
Name: Mina Kim
University: Coastal University
Major: Marketing
Graduation Year: 2024
About Me: 
Experience: None
Education: None
-------------------------
1. Create/Edit My Profile
2. View My Profile
3. Search for a job
4. Find someone you know
5. Learn a New Skill
Enter your choice:
````

### Case 12 – Output Comparison
**Actual (`io/InCollege-Output.txt` after running `Input_12`):**
````text
Welcome to InCollege!
1. Log In
2. Create New Account
Enter your choice:
Please enter your username:
Please enter your password:
You have successfully logged in.
Welcome, testuser!
1. Create/Edit My Profile
2. View My Profile
3. Search for a job
4. Find someone you know
5. Learn a New Skill
Enter your choice:
--- Your Profile ---
Name: Test User
University: State University
Major: Computer Science
Graduation Year: 2025
About Me: Passionate about software development.
Experience:
Title: Intern
Company: Tech Solutions
Dates: Summer 2024
Description: Assisted with front-end development.
Education:
Degree: Bachelor's
University: State University
Years: 2021-2025
--------------------
1. Create/Edit My Profile
2. View My Profile
3. Search for a job
4. Find someone you know
5. Learn a New Skill
Enter your choice:
Enter the full name of the person you are looking for:
--- Found User Profile ---
First Name: Jordan
Last Name:  Blake
University: Mountain Tech University
Major:      Information Systems
About:      Building future-ready systems.
Experience: Systems Analyst~Horizon Corp~2022-2024~Led cross-team integration projects.
Education:  Intern~InnovateX~Summer 2021~Built automation scripts for QA.
1. Create/Edit My Profile
2. View My Profile
3. Search for a job
4. Find someone you know
5. Learn a New Skill
Enter your choice:
````

**Expected (`tests/Output_12.txt`):**
````text
Welcome to InCollege!
1. Log In
2. Create New Account
Enter your choice:
Please enter your username:
Please enter your password:
You have successfully logged in.
Welcome, testuser!
1. Create/Edit My Profile
2. View My Profile
3. Search for a job
4. Find someone you know
5. Learn a New Skill
Enter your choice:
--- Your Profile ---
Name: Test User
University: State University
Major: Computer Science
Graduation Year: 2025
About Me: Passionate about software development.
Experience:
  Title: Intern
  Company: Tech Solutions
  Dates: Summer 2024
  Description: Assisted with front-end development.
Education:
  Degree: Bachelor's
  University: State University
  Years: 2021-2025
--------------------
1. Create/Edit My Profile
2. View My Profile
3. Search for a job
4. Find someone you know
5. Learn a New Skill
Enter your choice:
Enter the full name of the person you are looking for:
--- Found User Profile ---
Name: Jordan Blake
University: Mountain Tech University
Major: Information Systems
Graduation Year: 2025
About Me: Building future-ready systems.
Experience:
  Title: Systems Analyst
  Company: Horizon Corp
  Dates: 2022-2024
  Description: Led cross-team integration projects.
  Title: Intern
  Company: InnovateX
  Dates: Summer 2021
  Description: Built automation scripts for QA.
Education:
  Degree: Master's
  University: Mountain Tech University
  Years: 2023-2025
  Degree: Bachelor's
  University: Lakeside College
  Years: 2019-2023
-------------------------
1. Create/Edit My Profile
2. View My Profile
3. Search for a job
4. Find someone you know
5. Learn a New Skill
Enter your choice:
````

### Case 16 – Output Comparison
**Actual (`io/InCollege-Output.txt` after running `Input_16`):**
````text
Welcome to InCollege!
1. Log In
2. Create New Account
Enter your choice:
Please enter your username:
Please enter your password:
You have successfully logged in.
Welcome, testuser!
1. Create/Edit My Profile
2. View My Profile
3. Search for a job
4. Find someone you know
5. Learn a New Skill
Enter your choice:
Enter the full name of the person you are looking for:
--- Found User Profile ---
First Name: Maximilianus
Last Name:  Theophilus Bartholomew IV
University: Royal Academy of Technology
Major:      Quantum Computing
About:      Dedicated to pushing computational limits.
Experience: Research Fellow~Royal Lab~2028-2030~Developed quantum algorithms for navigation.
Education:  PhD Candidate~Royal Academy of Technology~2026-2030
1. Create/Edit My Profile
2. View My Profile
3. Search for a job
4. Find someone you know
5. Learn a New Skill
Enter your choice:
````

**Expected (`tests/Output_16.txt`):**
````text
Welcome to InCollege!
1. Log In
2. Create New Account
Enter your choice:
Please enter your username:
Please enter your password:
You have successfully logged in.
Welcome, testuser!
1. Create/Edit My Profile
2. View My Profile
3. Search for a job
4. Find someone you know
5. Learn a New Skill
Enter your choice:
Enter the full name of the person you are looking for:
--- Found User Profile ---
Name: Maximilianus Theophilus Bartholomew IV
University: Royal Academy of Technology
Major: Quantum Computing
Graduation Year: 2030
About Me: Dedicated to pushing computational limits.
Experience:
  Title: Research Fellow
  Company: Royal Lab
  Dates: 2028-2030
  Description: Developed quantum algorithms for navigation.
Education:
  Degree: PhD Candidate
  University: Royal Academy of Technology
  Years: 2026-2030
-------------------------
1. Create/Edit My Profile
2. View My Profile
3. Search for a job
4. Find someone you know
5. Learn a New Skill
Enter your choice:
````

### Case 17 – Output Comparison
**Actual (`io/InCollege-Output.txt` after running `Input_17`):**
````text
Welcome to InCollege!
1. Log In
2. Create New Account
Enter your choice:
Please enter your username:
Please enter your password:
You have successfully logged in.
Welcome, testuser!
1. Create/Edit My Profile
2. View My Profile
3. Search for a job
4. Find someone you know
5. Learn a New Skill
Enter your choice:
Enter the full name of the person you are looking for:
--- Found User Profile ---
First Name: Jean-Luc
Last Name:  O'Connor
University: International Space Institute
Major:      Aerospace Engineering
About:      Testing prototype starships.
Experience: Flight Engineer~Orbital Dynamics~2024-2025~Monitored mission telemetry.
Education:  Master's~International Space Institute~2024-2026
1. Create/Edit My Profile
2. View My Profile
3. Search for a job
4. Find someone you know
5. Learn a New Skill
Enter your choice:
````

**Expected (`tests/Output_17.txt`):**
````text
Welcome to InCollege!
1. Log In
2. Create New Account
Enter your choice:
Please enter your username:
Please enter your password:
You have successfully logged in.
Welcome, testuser!
1. Create/Edit My Profile
2. View My Profile
3. Search for a job
4. Find someone you know
5. Learn a New Skill
Enter your choice:
Enter the full name of the person you are looking for:
--- Found User Profile ---
Name: Jean-Luc O'Connor
University: International Space Institute
Major: Aerospace Engineering
Graduation Year: 2026
About Me: Testing prototype starships.
Experience:
  Title: Flight Engineer
  Company: Orbital Dynamics
  Dates: 2024-2025
  Description: Monitored mission telemetry.
Education:
  Degree: Master's
  University: International Space Institute
  Years: 2024-2026
-------------------------
1. Create/Edit My Profile
2. View My Profile
3. Search for a job
4. Find someone you know
5. Learn a New Skill
Enter your choice:
````

### Case 18 – Output Comparison
**Actual (`io/InCollege-Output.txt` after running `Input_18`):**
````text
Welcome to InCollege!
1. Log In
2. Create New Account
Enter your choice:
Please enter your username:
Please enter your password:
You have successfully logged in.
Welcome, testuser!
1. Create/Edit My Profile
2. View My Profile
3. Search for a job
4. Find someone you know
5. Learn a New Skill
Enter your choice:
Enter the full name of the person you are looking for:
--- Found User Profile ---
First Name: Jamie
Last Name:  Lee
University: Metro University
Major:      Data Analytics
About:      Loves turning data into stories.
Experience: Data Analyst~Insight Corp~2023-2024~Built executive dashboards.
Education:  Bachelor's~Metro University~2021-2025
1. Create/Edit My Profile
2. View My Profile
3. Search for a job
4. Find someone you know
5. Learn a New Skill
Enter your choice:
````

**Expected (`tests/Output_18.txt`):**
````text
Welcome to InCollege!
1. Log In
2. Create New Account
Enter your choice:
Please enter your username:
Please enter your password:
You have successfully logged in.
Welcome, testuser!
1. Create/Edit My Profile
2. View My Profile
3. Search for a job
4. Find someone you know
5. Learn a New Skill
Enter your choice:
Enter the full name of the person you are looking for:
--- Found User Profile ---
Name: Jamie Lee
University: Metro University
Major: Data Analytics
Graduation Year: 2025
About Me: Loves turning data into stories.
Experience:
  Title: Data Analyst
  Company: Insight Corp
  Dates: 2023-2024
  Description: Built executive dashboards.
Education:
  Degree: Bachelor's
  University: Metro University
  Years: 2021-2025
-------------------------
1. Create/Edit My Profile
2. View My Profile
3. Search for a job
4. Find someone you know
5. Learn a New Skill
Enter your choice:
````
