# Epic 4 Architecture: Connection Requests

## Overview
Epic 4 implements the connection request functionality for the InCollege platform, allowing users to send and manage connection requests to build their professional networks. This feature builds upon the user search functionality from Epic 3 and adds persistent storage for connection relationships.

## Key Features Implemented

### 1. Connection Request Sending
- **Integration Point**: After successfully searching for and viewing another user's profile (from Epic 3), users are presented with a connection request option
- **Validation Logic**:
  - Cannot send request to self
  - Cannot send request if already connected (accepted status)
  - Cannot send request if already sent a pending request to this user
  - Cannot send request if this user has already sent a pending request to you
- **User Feedback**: Personalized confirmation message showing the recipient's name

### 2. Pending Requests Viewing
- **New Menu Option**: Added "4. View My Pending Connection Requests" to the main post-login menu
- **Display Logic**: Shows all pending connection requests received by the current user
- **Empty State**: Displays appropriate message when no pending requests exist

### 3. Data Persistence
- **Storage File**: `data/connections.txt`
- **Record Format**: `sender_username|receiver_username|status`
- **Status Values**:
  - `P`: Pending connection request
  - `A`: Accepted connection
- **File Operations**: Load connections at application startup, save new connections immediately

## Technical Implementation

### Data Structures

#### File Section Additions
```cobol
FD  CONNECTIONS-FILE.
01  CONNECTION-REC                PIC X(258).

FD  REQUEST-FILE.  *> Alternative implementation (not used in main flow)
01  REQUEST-REC                   PIC X(256).
```

#### Working Storage Additions
```cobol
*> Connection management variables
01  WS-CONNECTIONS-MAX            PIC 9(4) VALUE 500.
01  WS-CONNECTIONS-COUNT          PIC 9(4) VALUE 0.
01  WS-CONNECTIONS-TABLE.
    05  WS-CONNECTION OCCURS 0 TO 500 TIMES
            DEPENDING ON WS-CONNECTIONS-COUNT
            INDEXED BY CONN-IDX.
        10  WS-CONN-SENDER        PIC X(128).
        10  WS-CONN-RECEIVER      PIC X(128).
        10  WS-CONN-STATUS        PIC X.  *> 'P'ending or 'A'ccepted

*> Connection status flags with condition names
01  WS-CONNECTION-STATUS-FLAG     PIC X(2) VALUE SPACES.
    88 CONN-OK                            VALUE "OK".
    88 CONN-ALREADY-ACCEPTED              VALUE "AC".
    88 CONN-PENDING-BY-ME                 VALUE "P1".
    88 CONN-PENDING-BY-THEM               VALUE "P2".
```

### Core Procedures

#### Connection Request Flow
1. **PROMPT-FOR-CONNECTION**: Displays menu after profile viewing
2. **PROCESS-CONNECTION-REQUEST**: Validates and processes the request
3. **CHECK-CONNECTION-STATUS**: Validates relationship between users
4. **ADD-NEW-CONNECTION**: Creates new pending connection record
5. **SAVE-CONNECTIONS**: Persists connections to file

#### Viewing Pending Requests
- **VIEW-PENDING-REQUESTS**: Main entry point from menu option 4
- Iterates through connections table to find pending requests addressed to current user
- Displays sender usernames or "No pending requests" message

### File I/O Operations

#### Initialization
- **INIT-LOAD-CONNECTIONS**: Loads existing connections from `data/connections.txt` into memory table at application startup
- **PARSE-CONNECTION-REC**: Parses pipe-delimited records into table structure

#### Persistence
- **SAVE-CONNECTIONS**: Writes entire connections table to file after adding new connections
- Uses sequential file organization for simple append operations

### Menu Integration

#### Main Menu Enhancement
Added option 4 to the logged-in user menu:
```
1. View My Profile
2. Search for User
3. Learn a New Skill
4. View My Pending Connection Requests
```

#### Profile Viewing Enhancement
After displaying a found user's profile (from Epic 3 search), the system now:
1. Checks if the found user is not the current user
2. Prompts for connection request option
3. Processes the request if selected

### Error Handling & Validation

#### Request Validation
- **Already Connected**: Checks for existing accepted connections in both directions
- **Duplicate Requests**: Prevents sending requests to users who already have pending requests from/to the sender
- **Self-Requests**: Prevents users from sending requests to themselves

#### File I/O Error Handling
- Checks file status after open operations
- Graceful handling of missing or corrupted connection files

## Data Flow

### Connection Request Process
1. User searches for another user (Epic 3 functionality)
2. System displays profile and prompts for connection request
3. User selects option to send request
4. System validates request constraints
5. If valid: creates new connection record with status 'P'
6. Saves updated connections to file
7. Displays confirmation message

### View Pending Requests Process
1. User selects menu option 4
2. System scans connections table for records where:
   - Current user is the receiver
   - Status is 'P' (pending)
3. Displays list of sender usernames
4. Shows "no pending requests" if none found

## Integration Points

### With Epic 3 (User Search)
- Connection request option appears after successful profile viewing
- Uses existing profile display and search result variables
- Maintains existing user experience flow

### With Epic 1 & 2 (Login & Profiles)
- Uses existing user authentication and profile data
- Integrates with logged-in user session management
- Maintains consistent menu structure and I/O handling

## Testing Considerations

### Test Cases Covered
- Viewing pending requests when none exist
- Successfully sending new connection requests
- Attempting invalid requests (already connected, duplicate requests, non-existent users)
- Multiple pending requests display
- Consecutive request sending

### I/O Requirements Maintained
- All input read from predefined input files
- All output displayed to screen AND written to output files
- Identical output preservation for verification

## Future Extensibility

### Connection Acceptance
The data structure supports connection acceptance (status 'A'), though the UI for accepting requests is not implemented in this epic.

### Connection Management
The connections table structure allows for future features like:
- Viewing accepted connections
- Withdrawing sent requests
- Connection recommendations

## File Structure Changes

### New Files
- `data/connections.txt`: Persistent storage for connection relationships

### Modified Files
- `src/InCollege.cob`: Added connection management logic, new menu option, file I/O operations

### Unchanged Files
- All existing data files and I/O test files remain compatible
- Epic 1-3 functionality preserved without modification