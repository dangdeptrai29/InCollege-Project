## Detailed Test Case Specifications

### Test Case 0: Create New Profile (Positive - Sub-task 2)
**Purpose**: Verify profile creation and immediate persistence
**Setup**: Clean data files, existing user account
**Input Sequence**: 
```
1                    # Log In
testuser            # Username  
Password1!          # Password
1                   # Create/Edit My Profile
John                # First Name
Doe                 # Last Name  
University of Test  # University
Computer Science    # Major
2025               # Graduation Year
Test bio           # About Me
DONE               # Experience
DONE               # Education
```
**Expected**: Profile saved to data/profiles.txt, success message displayed
**Validation**: File contains: `testuser|John|Doe|University of Test|Computer Science|2025|Test bio||`

### Test Case 1: Edit Existing Profile (Positive - Sub-task 2)  
**Purpose**: Verify profile editing and change persistence
**Setup**: Existing profile from Test Case 0
**Input Sequence**:
```
1                    # Log In
testuser            # Username
Password1!          # Password  
1                   # Create/Edit My Profile
Jane                # Updated First Name
Smith               # Updated Last Name
Stanford University # Updated University
Engineering         # Updated Major
2024               # Updated Year
Updated bio        # Updated About Me
DONE               # Experience
DONE               # Education
```
**Expected**: Profile updated in data/profiles.txt, changes persisted
**Validation**: File contains updated data, old data replaced

### Test Case 2: View Profile After Creation (Positive - Sub-task 2)
**Purpose**: Verify profile viewing displays persisted data correctly  
**Setup**: Existing profile from Test Case 1
**Input Sequence**:
```
1          # Log In
testuser   # Username
Password1! # Password
2          # View My Profile
```
**Expected**: Displays all saved profile information accurately
**Validation**: Output shows current persisted profile data

### Test Case 3: Invalid Graduation Year (Negative - Sub-task 2)
**Purpose**: Verify validation prevents invalid data persistence
**Setup**: Clean profile, existing user
**Input Sequence**:
```
1                    # Log In  
testuser            # Username
Password1!          # Password
1                   # Create/Edit My Profile
Bob                 # First Name
Johnson             # Last Name
MIT                 # University
Computer Science    # Major
1800               # Invalid Year (too old)
Bio text           # About Me
DONE               # Experience  
DONE               # Education
```
**Expected**: Error message, profile not saved
**Validation**: data/profiles.txt unchanged, error displayed

### Test Case 4: Empty Required Fields (Negative - Sub-task 2)
**Purpose**: Verify required field validation prevents invalid persistence
**Setup**: Clean profile, existing user
**Input Sequence**:
```
1          # Log In
testuser   # Username  
Password1! # Password
1          # Create/Edit My Profile
           # Empty First Name
Alice      # Last Name
Harvard    # University
           # Empty Major
2023       # Year
Bio        # About Me
DONE       # Experience
DONE       # Education
```
**Expected**: Error messages, profile not saved
**Validation**: Required field validation enforced

### Test Case 5: Profile Persistence Across Restart (Edge - Sub-task 1)
**Purpose**: **CRITICAL** - Verify profiles persist after application restart
**Setup**: Create profile, then simulate restart
**Input Sequence** (Two-phase test):
**Phase 1 - Create Profile:**
```
1                   # Log In
testuser           # Username
Password1!         # Password  
1                  # Create/Edit My Profile
Alice              # First Name
Brown              # Last Name
Yale               # University
Physics            # Major
2023              # Year
Persistent bio    # About Me
DONE              # Experience
DONE              # Education
```
**Phase 2 - After Restart:**
```
1          # Log In
testuser   # Username
Password1! # Password
2          # View My Profile
```
**Expected**: Profile data intact after application restart
**Validation**: Phase 2 displays same data from Phase 1 - proves persistence

### Test Case 6: Multiple Users with Profiles (Complex - Sub-task 3)
**Purpose**: Verify user profile isolation and multi-user persistence
**Setup**: Multiple user accounts in data/users.txt
**Input Sequence**: 
```
2             # Create New Account
userA         # Username
PassA123!     # Password
1             # Log In  
userA         # Username
PassA123!     # Password
1             # Create/Edit My Profile
First         # First Name
UserA         # Last Name
University A  # University
Major A       # Major
2020         # Year
Bio A        # About Me
DONE         # Experience
DONE         # Education
2             # Create New Account
userB         # Username
PassB123!     # Password
1             # Log In
userB         # Username  
PassB123!     # Password
1             # Create/Edit My Profile
Second        # First Name
UserB         # Last Name
University B  # University
Major B       # Major
2021         # Year
Bio B        # About Me
DONE         # Experience
DONE         # Education
```
**Expected**: Each user sees only their own profile, data isolated
**Validation**: data/profiles.txt contains both profiles separately

### Test Case 7: Profile Update Tracking (Complex - Sub-task 2)
**Purpose**: Verify multiple profile updates are properly tracked
**Setup**: Existing user profile
**Input Sequence**:
```
1                    # Log In
testuser            # Username
Password1!          # Password
1                   # Create/Edit My Profile (First Update)
Updated1            # First Name
Last1               # Last Name  
University1         # University
Major1              # Major
2026               # Year
Bio1               # About Me
DONE               # Experience
DONE               # Education
1                   # Create/Edit My Profile (Second Update)
Final               # First Name
Name                # Last Name
Final University    # University
Final Major         # Major
2027               # Year
Final bio          # About Me
DONE               # Experience
DONE               # Education
2                   # View My Profile
```
**Expected**: Final update persisted, previous data replaced
**Validation**: Only latest profile data displayed and stored

### Test Case 8: Profile View When None Exists (Edge - Sub-task 3)
**Purpose**: Verify graceful handling when no profile exists
**Setup**: Clean profile data, new user account
**Input Sequence**:
```
2          # Create New Account
noprofile  # Username
NoProfile1! # Password
1          # Log In
noprofile  # Username  
NoProfile1! # Password
2          # View My Profile
```
**Expected**: Appropriate message indicating no profile exists
**Validation**: No crash, graceful message displayed

### Test Case 9: Graduation Year Boundary Values (Edge - Sub-task 3)
**Purpose**: Verify boundary value handling for graduation years
**Setup**: Clean profile, existing user
**Input Sequence**:
```
1                    # Log In
testuser            # Username
Password1!          # Password
1                   # Create/Edit My Profile
Edge                # First Name
Case                # Last Name
Boundary University # University
Computer Science    # Major
1900               # Valid boundary (minimum)
Bio                # About Me
DONE               # Experience
DONE               # Education
1                   # Create/Edit My Profile (Test maximum)
Edge                # First Name
Case                # Last Name
Boundary University # University
Computer Science    # Major  
2100               # Valid boundary (maximum)
Bio                # About Me
DONE               # Experience
DONE               # Education
1                   # Create/Edit My Profile (Test invalid)
Edge                # First Name
Case                # Last Name
Boundary University # University
Computer Science    # Major
1899               # Invalid (below minimum)
Bio                # About Me
DONE               # Experience
DONE               # Education
```
**Expected**: 1900 and 2100 accepted, 1899 rejected with error
**Validation**: Proper boundary validation enforced

## Success Criteria for Epic 2 Profile Persistence Testing

### ✅ Requirements Compliance
1. **10 test cases created** (Input_0.txt through Input_9.txt + corresponding Output files)
2. **Positive, negative, and edge cases covered** (comprehensive scenario coverage)
3. **All files saved in tests/ folder** (correct location and naming)
4. **Test ID range [0, 9]** (complementing colleague's tests 10-19)

### ✅ Sub-task Validation  
1. **Profile saving, loading, linking across restarts** - Test Cases 0, 1, 2, 5
2. **Profile changes during create/edit/view operations** - Test Cases 1, 2, 3, 7
3. **Multi-user profiles and complex scenarios** - Test Cases 6, 7, 8, 9

### ✅ Technical Verification
1. **Data persistence to data/profiles.txt** - All test cases validate file writes
2. **Cross-session data integrity** - Test Case 5 specifically validates this  
3. **User profile isolation** - Test Case 6 validates multi-user scenarios
4. **Input validation** - Test Cases 3, 4, 9 validate error handling
5. **Boundary conditions** - Test Cases 8, 9 validate edge cases

## Next Steps
1. Implement test cases in sequence (Tasks 1-5)
2. Execute each test case and verify against actual program behavior  
3. Update Output files based on actual program responses
4. Validate data persistence requirements are met
5. Document final results and create execution guide

---
*Epic 2 Profile Persistence Testing Plan - Complementing I/O Testing (10-19)*

## Test Case Specifications

### Test Case 0: Create New Profile (Positive)
**Purpose**: Verify successful profile creation and persistence
**Input**: Login → Create Profile → Valid data → Save
**Expected**: Profile saved to data/profiles.txt with correct format

### Test Case 1: Edit Existing Profile (Positive)  
**Purpose**: Verify profile editing functionality
**Input**: Login → Edit Profile → Modify fields → Save
**Expected**: Updated data persisted to file

### Test Case 2: View Profile After Creation (Positive)
**Purpose**: Verify profile viewing displays saved data
**Input**: Login → View Profile
**Expected**: Displays all saved profile information

### Test Case 3: Invalid Graduation Year (Negative)
**Purpose**: Verify validation for graduation year
**Input**: Login → Create Profile → Invalid year (e.g., 1800, 2200)
**Expected**: Error message, profile not saved

### Test Case 4: Empty Required Fields (Negative)
**Purpose**: Verify required field validation
**Input**: Login → Create Profile → Leave required fields empty
**Expected**: Error messages, profile not saved

### Test Case 5: Profile Persistence Across Restart (Edge)
**Purpose**: Verify profiles persist after application restart
**Input**: Create profile → Exit app → Restart → View profile
**Expected**: Profile data intact after restart

### Test Case 6: Multiple Users with Profiles (Complex)
**Purpose**: Verify multiple user profile management
**Input**: Create multiple user accounts → Create profiles for each
**Expected**: Each user sees only their own profile

### Test Case 7: Profile Update Tracking (Complex)
**Purpose**: Verify profile updates are properly tracked
**Input**: Create profile → Update multiple times → View
**Expected**: Latest updates are saved and displayed

### Test Case 8: Profile View When None Exists (Edge)
**Purpose**: Verify behavior when no profile exists
**Input**: Login → View Profile (no profile created)
**Expected**: Appropriate message indicating no profile exists

### Test Case 9: Graduation Year Boundary Values (Edge)
**Purpose**: Verify boundary value handling
**Input**: Test with years 1900, 2100, 1899, 2101
**Expected**: Valid years accepted, invalid rejected

## Implementation Approach
1. Each test case will be a pair of files: `Input_X.txt` and `Output_X.txt`
2. Input files will contain the sequence of user inputs
3. Output files will contain the expected program output
4. Test cases will cover positive, negative, and edge scenarios
5. Focus on data persistence validation across application restarts

## Success Criteria
- All 10 test case pairs created
- All test cases validate data persistence requirements
- Coverage of positive, negative, and edge cases
- Verification of profile saving, loading, and linking functionality
- Documentation of complex multi-user scenarios