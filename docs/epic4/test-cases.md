# Epic 4 Test Cases: Connection Requests

## Overview
This document describes the comprehensive test cases for Epic 4's connection request functionality. The test cases cover both positive and negative scenarios for sending connection requests and viewing pending requests.

## Test Case Structure

### Input Files
- Located in `tests/epic4_inputs/`
- Format: Sequential user inputs (menu choices, usernames, passwords, search terms)
- Each test case has a corresponding `TC##_<description>.txt` file

### Expected Output Files
- Located in `tests/epic4_expected/`
- Format: Complete program output including prompts, user messages, and data display
- Each test case has a corresponding `TC##_<description>.out.txt` file

## Test Cases

### TC01: View Pending Requests - No Pending Requests
**User:** alice (no pending requests)  
**Scenario:** User logs in and views their pending connection requests when they have none.

**Input:**
```
1          # Log In
alice      # Username
alice123   # Password
4          # View My Pending Connection Requests
```

**Expected Behavior:**
- Successful login
- Display main menu with new option 4
- Show "You have no pending connection requests at this time."

### TC02: Send Connection Request - Success
**User:** alice  
**Scenario:** User successfully sends a connection request to another user.

**Input:**
```
1          # Log In
alice      # Username
alice123   # Password
2          # Search for User
Bob Tran   # Search term
1          # Send Connection Request
4          # View My Pending Connection Requests
```

**Expected Behavior:**
- Successful login and user search
- Display user profile (Bob Tran)
- Show connection request menu
- Accept request and display "Connection request sent to Bob Tran."
- No pending requests shown (alice sent request, didn't receive any)

### TC03: Send Connection Request - Already Connected (Negative)
**User:** alice  
**Scenario:** User attempts to send request to someone they're already connected with.

**Input:**
```
1          # Log In
alice      # Username
alice123   # Password
2          # Search for User
Bob Tran   # Search term
1          # Send Connection Request
```

**Expected Behavior:**
- Successful login and user search
- Display user profile
- Show error: "You are already connected with this user."

### TC04: Send Connection Request - Reverse Pending (Negative)
**User:** alice  
**Scenario:** User attempts to send request to someone who already sent them a request.

**Input:** Similar to TC03 but with different user relationship

**Expected Behavior:**
- Show error: "This user has already sent you a connection request."

### TC05: View Pending Requests - Multiple Requests
**User:** alice  
**Scenario:** User views multiple pending connection requests.

**Input:**
```
1          # Log In
alice      # Username
alice123   # Password
4          # View My Pending Connection Requests
```

**Expected Behavior:**
- Display list of users who sent pending requests to alice

### TC06: Sample Week 4 Output
**User:** TestUser  
**Scenario:** Complete flow showing login, viewing no pending requests, searching for user, and sending request.

**Input:** Complex multi-step scenario

**Expected Behavior:**
- Complete program flow as shown in sample output
- Demonstrates integration with existing functionality

### TC07: View Pending Requests - No Pending (Different User)
**User:** testuser  
**Scenario:** Different user with no pending requests.

**Input:**
```
1          # Log In
testuser   # Username
Password1! # Password
4          # View My Pending Connection Requests
```

**Expected Behavior:**
- "You have no pending connection requests at this time."

### TC08: Send Connection Request - New User (Positive)
**User:** testuser  
**Scenario:** User sends successful request to new connection.

**Input:** Similar to TC02 but with different users

**Expected Behavior:**
- Successful request sending
- Confirmation message displayed

### TC09: Send Connection Request - Already Sent (Negative)
**User:** testuser  
**Scenario:** User tries to send request to someone they've already sent a request to.

**Input:**
```
1          # Log In
testuser   # Username
Password1! # Password
2          # Search for User
Mina Kim   # Search term
1          # Send Connection Request
```

**Expected Behavior:**
- Error: "You have already sent a pending connection request to this user."

### TC10: Send Connection Request - Non-existent User (Negative)
**User:** testuser  
**Scenario:** User searches for a user that doesn't exist.

**Input:**
```
1          # Log In
testuser   # Username
Password1! # Password
2          # Search for User
NonExistent User  # Search term
```

**Expected Behavior:**
- "No one by that name could be found."

### TC11: View Pending Requests - Two Requests
**User:** mkim  
**Scenario:** User views exactly two pending connection requests.

**Input:**
```
1          # Log In
mkim       # Username
Marketing1! # Password
4          # View My Pending Connection Requests
```

**Expected Behavior:**
- Display pending requests from:
  - ajohnson
  - testuser

### TC12: Send Connection Request - User Already Sent Request (Negative)
**User:** mkim  
**Scenario:** User tries to send request to someone who already sent them a request.

**Input:** mkim searches for Test1 User and tries to send request

**Expected Behavior:**
- Error: "This user has already sent you a connection request."

### TC13: Send Connection Request - Already Accepted (Negative)
**User:** mkim  
**Scenario:** User tries to send request to someone they already accepted a connection with.

**Input:** mkim searches for Alex Johnson

**Expected Behavior:**
- Error: "You are already connected with this user."

### TC14: Send Connection Request - Sent and Accepted (Negative)
**User:** ajohnson  
**Scenario:** User tries to send request to someone they already sent a request to and got accepted.

**Input:** ajohnson searches for Mina Kim

**Expected Behavior:**
- Error: "You are already connected with this user."

### TC15: Send Multiple Connection Requests (Positive)
**User:** ajohnson  
**Scenario:** User sends two consecutive connection requests to different new users.

**Input:** ajohnson sends requests to two different users in sequence

**Expected Behavior:**
- Both requests sent successfully
- Confirmation messages for each

### TC16: Complex Flow - View and Send
**User:** TestUser  
**Scenario:** User views requests (none), then sends request to user who already received a request.

**Input:** Complex flow combining view and send operations

**Expected Behavior:**
- Complete flow with appropriate messages and state changes

## Test Case Categories

### Positive Test Cases
- **TC02**: Successful connection request sending
- **TC08**: Send request to new user
- **TC15**: Multiple consecutive requests

### Negative Test Cases
- **TC03**: Already connected users
- **TC04**: Reverse pending request scenario
- **TC09**: Duplicate request sending
- **TC10**: Non-existent user search
- **TC12**: User already sent request
- **TC13**: Already accepted connection
- **TC14**: Sent and accepted connection

### View Pending Requests Cases
- **TC01**: No pending requests
- **TC05**: Multiple pending requests
- **TC07**: Different user, no requests
- **TC11**: Exactly two pending requests

### Integration Cases
- **TC06**: Complete program flow
- **TC16**: Complex user interaction flow

## Test Execution

### Prerequisites
- Valid user accounts exist in `data/users.txt`
- User profiles exist in `data/profiles.txt`
- Initial connection data in `data/connections.txt`
- Input files placed in `io/InCollege-Input.txt`
- Output verification against expected files

### Running Tests
1. Copy test input file to `io/InCollege-Input.txt`
2. Run the program
3. Compare `io/InCollege-Output.txt` with expected output file
4. Verify exact match (including formatting and line breaks)

### Test Data Setup
The test cases assume the following initial data:
- Users: alice, testuser, mkim, ajohnson, etc.
- Profiles: Complete profile data for searchable users
- Connections: Pre-existing connection relationships for testing various states

## Coverage Analysis

### Functional Coverage
- ✅ Send connection request (positive cases)
- ✅ View pending requests (empty and populated)
- ✅ Error handling for invalid requests
- ✅ Integration with user search functionality
- ✅ Menu navigation and option selection

### Edge Cases Covered
- ✅ No pending requests scenario
- ✅ Multiple pending requests
- ✅ All connection validation rules
- ✅ Non-existent user handling
- ✅ Self-request prevention (implied through search)

### Data Validation
- ✅ File I/O operations
- ✅ In-memory data consistency
- ✅ Persistent storage updates
- ✅ Output formatting requirements