# Epic 3 Architecture – Enhanced Profile & Search Features

## Overview
Epic 3 builds upon Epic 1 (authentication) and Epic 2 (profile management) by adding comprehensive profile viewing and user search capabilities. The implementation maintains the single-program, modular COBOL architecture while extending functionality for the "Find someone you know" feature.

## Key Components Added

### Enhanced Profile Viewing
- **Complete Profile Display**: Shows all profile fields including optional sections (About Me, Experience, Education)
- **Formatted Output**: Properly formatted console display with consistent spacing and line separators
- **Data Integration**: Retrieves and displays all profile information stored in Epic 2

### User Search Functionality  
- **Exact Name Matching**: Case-sensitive, trimmed exact match on concatenated first and last name
- **Profile Display**: Shows complete profile of found users with same formatting as personal profile view
- **Error Handling**: Appropriate messaging for users not found, blank searches, and partial name queries
- **Menu Integration**: Seamlessly integrated with existing post-login menu navigation

## Data Structure Extensions

### Profile Data Format
Profiles stored in `data/profiles.txt` with pipe-delimited format:
```
username|first|last|university|major|year|about|experience|education
```

**Experience/Education Serialization:**
- Multiple entries separated by `^` (caret)
- Fields within entries separated by `~` (tilde)
- Format: `title~company~dates~description` (experience)
- Format: `degree~university~years` (education)

### Search Implementation
- **WS-SEARCH-RESULT-IDX**: Index tracking for search results
- **Case-Sensitive Matching**: Preserves original name capitalization
- **Trimmed Input**: Handles whitespace in search queries
- **Duplicate Name Handling**: Returns first matching profile found

## Menu Flow Extensions

### Updated Post-Login Menu
```
1. Create/Edit My Profile
2. View My Profile          (Enhanced in Epic 3)
3. Search for a job
4. Find someone you know    (New in Epic 3)  
5. Learn a New Skill
```

### Search Flow
1. User selects option 4 "Find someone you know"
2. System prompts: "Enter the full name of the person you are looking for:"
3. User enters full name (e.g., "Alex Johnson")
4. System searches profiles by exact name match
5. If found: Display complete profile with formatting
6. If not found: Display "No one by that name could be found."
7. Return to post-login menu

## Output Formatting Standards

### Profile Display Format
```
--- Your Profile --- / --- Found User Profile ---
Name: [First Last]
University: [University Name]
Major: [Major]
Graduation Year: [YYYY]
About Me: [About text or blank]
Experience:
    Title: [Job Title]
    Company: [Company]
    Dates: [Date Range]
    Description: [Description]
Education:
    Degree: [Degree]
    University: [University]
    Years: [Year Range]
--------------------    (20 dashes)
```

### Line Separators
- **MSG-LINE**: 20 characters `--------------------` (profile sections)
- **MSG-LINE-LONG**: 25 characters `-------------------------` (search results)

## Testing Coverage

### Test Scenarios (Cases 10-19)
- **Positive Cases**: Exact matches for full/minimal profiles, profile viewing flow
- **Negative Cases**: Non-existent users, partial names, case variations  
- **Edge Cases**: Long names, punctuation, duplicate names, blank input

### Validation Requirements
- All test cases must pass with exact output matching
- Console output must match expected output files precisely
- Search functionality must handle all edge cases gracefully
- Profile display must show all stored data correctly

## Integration Points

### File Dependencies
- `data/users.txt`: User authentication (Epic 1)
- `data/profiles.txt`: Profile data (Epic 2, Epic 3)
- `io/InCollege-Input.txt`: Test input sequences
- `io/InCollege_SampleOutput.txt`: Expected program output

### Module Dependencies  
- **Epic 1**: Authentication and menu foundation
- **Epic 2**: Profile creation and storage infrastructure
- **Epic 3**: Search and enhanced viewing capabilities

## Implementation Status
- **Complete**: All Epic 3 functionality implemented and tested  
- **Profile Viewing**: Enhanced display with all fields  
- **User Search**: Exact name matching with proper error handling  
- **Test Coverage**: All 10 test cases (0-19) passing  
