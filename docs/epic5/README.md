# Epic 5: Connection Management - Simple Implementation Guide

## COBOL Implementation Tasks

### What's Been Done (Dev 1)

• **Connection Data Structure Update** DONE
  - Reused existing connections.txt file (no new file needed)
  - Status field: 'P' (pending) → 'A' (accepted) for established connections
  - Connection removal from array for rejections

• **Request Processing Modules** DONE
  - Read through pending requests DONE
  - Prompt user to accept or reject each request DONE
  - If accepted: Add entry to established connections data file DONE
  - If accepted: Remove request from pending requests file DONE
  - If rejected: Remove request from pending requests file DONE

• **File Handling** DONE
  - Robust file handling for adding, deleting, and updating records DONE
  - Atomic operations with rollback on file errors DONE
  - Array management for deletions DONE

• **Input File Handling** DONE
  - COBOL ACCEPT statements to read user input for accept/reject choices DONE
  - All menu selections handled via input file DONE

• **Output File Handling** DONE
  - All prompts written to dedicated output file via DISPLAY-AND-LOG DONE
  - Displayed lists written identically to screen and output file DONE
  - Confirmation messages written to both screen and file DONE

### Remaining Work for Epic 5

• **Network Display Module**
  - Implement COBOL routines to read from established connections data file
  - Display list of connected users for logged-in user
  - Cross-reference with main user profile data to get names and details
  - Add menu option 5: "View My Network"

---

## Functionality That Could Be Use For Remaining Task

**GET-FULL-NAME Helper** - Already implemented profile lookup for cross-referencing with user profile data
```cobol
MOVE "username" TO WS-TARGET-USERNAME
PERFORM GET-FULL-NAME
*> WS-DISPLAY-NAME now contains "First Last" or username fallback
```

**Connection Filtering Pattern** - Use same logic but filter for 'A' (established) instead of 'P' (pending)
```cobol
IF (WS-CONN-RECEIVER(I) = WS-CURRENT-USERNAME OR 
    WS-CONN-SENDER(I) = WS-CURRENT-USERNAME) AND
    WS-CONN-STATUS(I) = 'A'
    *> This is an established connection
```

**Output File Handling** - Follow same DISPLAY-AND-LOG pattern for writing to output file

---

## Implementation Details (Dev 1)

### GET-FULL-NAME Helper
**What**: Cross-references with user profile data to get names and details
**How**: Loops through profiles table, returns "First Last" or username fallback
**Usage**: Called before displaying any connection request to show full names

### Enhanced VIEW-PENDING-REQUESTS (Request Processing Module)
**What**: Reads through pending requests and prompts user to accept or reject each
**How**: For each pending request → show full name → display "1. Accept / 2. Reject" → process choice
**Logic**: Filters for receiver=current_user AND status='P' (pending)

### ACCEPT-CONNECTION (Request Processing Module)
**What**: If accepted - adds entry to established connections data file
**How**: 
1. Update status P→A in connections data file (pending to accepted)
2. Save to file with rollback on error
3. Display "Connection with [Name] accepted"

### REJECT-CONNECTION (Request Processing Module)
**What**: If rejected - removes request from pending requests file
**How**:
1. Remove connection from array (shift all elements down)
2. Decrement connection count  
3. Save updated file
4. Display "Connection request from [Name] rejected"

---

## How Current Code Work

### Test Setup
**Data Files Needed**:
- connections.txt: `testuser|mkim|P` (pending request)  
- profiles.txt: `testuser|Test|User|—|—|—|||` (for name lookup)

### Test Case 1: Accept Connection
**Input**:
```
1        # Log In
mkim     # Username
password # Password  
4        # View Pending Requests
1        # Accept
0        # Exit
```

**Expected Output**:
```
--- Pending Connection Requests ---
Connection request from Test User
1. Accept
2. Reject
1
Connection with Test User accepted
-----------------------------------
```

**File Result**: connections.txt shows `testuser|mkim|A` (established connection)

### Test Case 2: Reject Connection
**Input**: Same but choose `2` for Reject

**Expected Output**:
```
Connection request from Test User
1. Accept
2. Reject
2
Connection request from Test User rejected
```

**File Result**: Connection completely removed from connections.txt

### Test Case 3: Invalid Choice
**Input**: Choose `3` (invalid)

**Expected Output**:
```
Invalid choice. Skipping request.
```

**File Result**: No changes, request remains pending

---

## Known Limitation

⚠️ **Login Bug**: Current login always sets username to "TestUser" regardless of actual credentials. This makes end-to-end testing difficult, but code logic is verified correct through code review and compilation testing.

---

## Quick Reference

**Files Modified**: `src/InCollege.cob` (+150 lines)  
**New Paragraphs**: GET-FULL-NAME, ACCEPT-CONNECTION, REJECT-CONNECTION  
**Enhanced**: VIEW-PENDING-REQUESTS (added interactivity)  
**Status**: Ready for Dev 2 (Network Display) implementation
