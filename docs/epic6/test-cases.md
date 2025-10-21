# InCollege Epic #6: Job Posting - Test Cases

## Prerequisites
- **Account**: `testuser` / `Password123!`
- **Environment**: Clean database, input files in `/tests/`

## Test Summary
**Total**: 10 cases |
---

## Test Cases

### JP-TC-11: Multiple Job Posting with Salary Variations
**File**: `Input_11.txt`  | **Type**: Functional

**Objective**: Verify multiple job postings with different salary configurations.

**Data**:
- Job 1: Senior Developer, Innovatech, New York, NY, $150,000/year
- Job 2: Product Manager, Innovatech, Remote, NONE

**Steps**: Login → Navigate to job menu → Post both jobs → Verify saved

**Expected**: Both jobs successfully posted and stored.

---

### JP-TC-12: Job Menu Navigation and Browse Jobs
**File**: `Input_12.txt`  | **Type**: Navigation

**Objective**: Test menu navigation and browse jobs feature.

**Steps**: Login → Navigate to job menu → Select "Browse Jobs" (2) → Verify message → Return to main menu (3)

**Expected**: Menu navigation works, browse jobs shows under-construction message.

---

### JP-TC-13: Required Field Validation (Description)
**File**: `Input_13.txt`| **Type**: Validation

**Objective**: Test validation when description field is missing.

**Data**: Title: "Technical Writer", Description: *(blank)*, Employer: "Manuals & More", Location: "Chicago, IL", Salary: "$60,000/year"

**Steps**: Login → Navigate to job menu → Enter job with blank description → Verify prompt → Enter valid description → Complete posting

**Expected**: System validates required fields and allows posting after correction.

---

### JP-TC-14: Oversized Input Handling
**File**: `Input_14.txt` | **Type**: Stress Test

**Objective**: Verify system stability with extremely long input.

**Data**: Title: 200+ character string, Other fields: Normal values

**Steps**: Login → Navigate to job menu → Enter extremely long title → Complete other fields → Submit

**Expected**: System handles long input without crashing and posts job successfully.

---

### JP-TC-15: Invalid Menu Input Handling
**File**: `Input_15.txt` | **Type**: Error Handling

**Objective**: Test response to invalid menu selections and non-numeric input.

**Steps**: Login → Navigate to job menu → Enter invalid option (5) → Enter non-numeric input ("abc") → Verify errors → Return to main menu (3)

**Expected**: System displays appropriate error messages and recovers gracefully.

---

### JP-TC-16: Unauthorized Access Prevention
**File**: `Input_16.txt` | **Type**: Security

**Objective**: Ensure job posting is inaccessible to non-authenticated users.

**Steps**: Attempt to access job posting without login → Verify access denied → Verify error handling

**Expected**: System prevents unauthorized access and prompts for authentication.

---

### JP-TC-17: Special Characters and Edge Cases
**File**: `Input_17.txt` | **Type**: Data Handling

**Objective**: Test handling of special characters and minimal field values.

**Data**:
- Job 1: "R&D Engineer #5", "Work on "next-gen" projects!", AT&T, St. Louis, MO
- Job 2: Single character fields (A, B, C, D, E)

**Steps**: Login → Navigate to job menu → Post both jobs → Verify data integrity

**Expected**: Special characters preserved and single-character inputs accepted.

---

### JP-TC-18: Sequential Job Posting
**File**: `Input_18.txt` | **Type**: Functional

**Objective**: Verify multiple jobs in single session without data loss.

**Data**:
- Job 1: "First Sequential Job", MultiPost Inc., City A, $1000
- Job 2: "Second Sequential Job", MultiPost Inc., City B, $2000

**Steps**: Login → Navigate to job menu → Post first job → Post second job immediately → Verify both saved

**Expected**: Both jobs successfully posted and stored in same session.

---

### JP-TC-19: Missing Required Field (Employer) with Recovery
**File**: `Input_19.txt` | **Type**: Error Recovery

**Objective**: Test error handling when employer field is missing.

**Data**: Title: "I/O Test Analyst", Description: "Tests input/output consistency.", Employer: *(blank)*, Location: "Testville", Salary: "$55,000/year"

**Steps**: Login → Navigate to job menu → Enter job with missing employer → Verify prompt → Enter valid employer → Complete posting

**Expected**: System handles missing field gracefully and allows posting after correction.

---

### JP-TC-20: Mixed Navigation with Error Recovery
**File**: `Input_20.txt` | **Type**: Navigation & Error

**Objective**: Test system behavior with mixed valid and invalid navigation choices.

**Steps**: Login → Navigate to main menu → Select invalid option (9) → Navigate to job menu (1) → Select invalid option (5) → Select valid option (1) → Post job: "Final Job Post" at "End"

**Expected**: System handles invalid choices gracefully and allows successful job posting.

---


## Notes
- Verify results against corresponding output files
- Clean system state required before execution
- Tests cover functional, security, validation, error handling, and data integrity scenarios