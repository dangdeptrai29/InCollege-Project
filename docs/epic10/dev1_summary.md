# Epic 10 — Dev 1 Comprehensive Summary

> Scope (Dev 1): Logic, input handling, persistence  
> Partners: Dev 2 (display/QOL) + QA testers

---

## 1. Revisit Existing Functionalities (Part 1)

### 1.1 Login & Account Creation
- **Password Validation**: Enforces 8–12 characters with ≥1 uppercase, ≥1 digit, and ≥1 special from `!@#$%^&*?-_+`. Invalid passwords show precise reasons before re-prompting.
- **Login Loop**: Resets state after each failed attempt, echoes menu choice (e.g., `Enter Your Choice: 1`), and supports unlimited retries until success or EOF.
- **Account Limit**: Uses `WS-ACCOUNT-LIMIT` (set to 5) so total accounts (loaded + new) never exceed the spec’s cap. Attempts beyond that print “All permitted accounts…”.
- **Back Navigation**: Users can enter `0` or `BACK` at login and account creation prompts to return gracefully with a “Returning to the previous menu…” message.
- **Persistence**: `data/users.txt` is still authoritative; the login test suite temporarily trims the file to keep within the 5-account limit, then restores it afterwards.

**Dev 2 Tips**  
Mirror the echoed choice lines and new instructions in display expectations. If they format menu copy (spacing/caps), distribute updates to the QA team to keep diffs consistent.

**QA Scenarios**  
Run `scripts/test_login.sh` (through a CR-free copy) for success-first and four-failures-then-success. Manually try `BACK` during username/password entry to ensure clean exits.

### 1.2 Profile Creation / Editing
- **Input Reset**: All profile buffers (name, about, experience arrays) reset before prompting, preventing stale data when editing in a single session.
- **Experience/Education Serialization**: Fixed duplication bug so entries are stored with single `~`/`^` separators and load correctly when viewing profiles.
- **About Me / BACK Support**: The About prompt now respects EOF and BACK, showing instructions and returning to the menu without corrupting state.
- **Validation Loop**: Required fields re-prompt until non-blank, matching spec. Year validation remains 1900–2100 (four-digit).

**Dev 2 Tips**  
Prompts now include the BACK instruction; adjust formatting to keep spacing/indentation consistent. For display refactors, rely on docstrings around `CREATE-OR-EDIT-PROFILE`.

**QA Scenarios**  
Create/edit a profile with and without experiences/education. Test BACK during each prompt (especially inside the experience loop) and confirm `data/profiles.txt` updates correctly.

### 1.3 Search Logic
- **Normalization**: Names are trimmed and internal whitespace collapsed so “Alex   Johnson” matches “Alex Johnson” without requiring users to type exact spacing.
- **Blank Input Handling**: Empty/whitespace-only searches trigger a dedicated message (“Search request skipped: name required…”) instead of a misleading not-found.
- **Logging**: Every search attempt is logged to `data/search_history.txt` in `username|query|result|timestamp` format. Results include `FOUND`, `NOT_FOUND`, `BLANK_INPUT`, or `BACK`.
- **Back Navigation**: Search prompts also accept 0/BACK to exit early and still log the attempt as `BACK`.

**Dev 2 Tips**  
Integrate the new blank-input message into display templates. If they provide a search history viewer later, the logging format is ready.

**QA Scenarios**  
Perform positive and negative searches; inspect `data/search_history.txt` to confirm entries. Delete `data/search_history.txt` before a run to ensure the program recreates it during init.

### 1.4 Menu Flow & Persistence
- **Menu Echoes**: Main menu, logged-in menu, skills, jobs, requests, and messages all echo `Enter Your Choice: X` once per selection using a shared helper. This keeps console and output file identical.
- **State Reset**: Submenu choice variables reset before loops to avoid skipping menus when reopened in the same session.
- **Search History File**: `ENSURE-SEARCH-HISTORY-FILE` runs during initialization to create the log file if missing; logging warns only once if a write fails.

**Dev 2 Tips**  
Output formatting is now deterministic; they should ensure display documents/tests include the echoed lines. For UI polish, they can rely on the docstrings around each menu.

**QA Scenarios**  
Traverse each menu (skills/jobs/messages/requests) to confirm the echo appears once and BACK returns to the previous menu without lingering state.

---

## 2. Quality-of-Life Improvements (Part 2)

### 2.1 Validation Messaging
- Added a global instruction (“Enter 0 or BACK at any prompt to return to the menu”) shown before login, account creation, profile editing, and search.
- When the user opts to leave mid-flow, the app prints “Returning to the previous menu…” so testers know the branch was intentional.
- Blank search input now produces a distinct warning message; no hidden failure states remain.

### 2.2 Flow Control & Edge Cases
- **BACK Support** expanded to every profile prompt, including multi-step loops (experience/education). The app saves partial data only if the user completes the flow.
- Search history records `BACK` attempts to keep diagnostics complete.
- All menu loops share the same `ECHO-CHOICE-VALUE` helper, reducing duplication and ensuring logs/test fixtures match expected output.

### 2.3 Persistence Safeguards
- `ENSURE-SEARCH-HISTORY-FILE` ensures the log file exists even if deleted. Logging now handles missing files gracefully with a single warning and no crash.
- Search logging uses `CURRENT-DATE`, capturing up to minutes (YYYYMMDDHHmm). Testers can rely on stable format for diffing.

---

## 3. Refactoring & Cleanup (Part 3)

### 3.1 Documentation & Structure
- Added descriptive `*>` docstrings before major paragraphs (LOGIN, CREATE-ACCOUNT, USER-SEARCH-MENU, ADD-EXPERIENCE, ADD-EDUCATION, NORMALIZE-NAME, LOG-SEARCH-ATTEMPT, ENSURE-SEARCH-HISTORY-FILE). These explain responsibilities so Dev 2/QA and future engineers can zero in on behavior.
- Introduced `WS-CHOICE-BUFFER` plus shared helpers (`ECHO-CHOICE-VALUE`, `CHECK-BACK-FROM-LINE`, `HANDLE-BACK-EXIT`) to eliminate repeated STRING blocks and reduce mistakes when expanding menus.

### 3.2 File I/O Improvements
- Consolidated search history creation into startup instead of ad-hoc per write, reducing error handling clutter inside `LOG-SEARCH-ATTEMPT`.
- Continue to treat `data/users.txt`, `data/profiles.txt`, and other files as authoritative, but now loggers warn if they cannot write (instead of failing silently).

### 3.3 Remaining Technical Debt
- Automated coverage is still limited to the login script; the rest of the flows rely on manual testing. Future work could wire up the Epic 3 search tests or build a harness for profile creation.
- `scripts/test_login.sh` has CRLF line endings; we used a temporary CR-stripped copy to run it. Ideally the repository version should be normalized to avoid extra steps.
- Display formatting is still Dev 2’s domain; they need to ensure the new instructions and echoed lines match the spec’s capitalization/spaces to avoid diff churn.

---

## 4. Suggested Test/Usage Matrix

| Test | Steps | Expected Outcome | Notes |
|------|-------|------------------|-------|
| **Login Suite** | (1) Strip CRs from `scripts/test_login.sh`, (2) Temporarily set `data/users.txt` to `testuser|Password1!`, (3) Run suite, (4) Restore users file. | Both success-first and 4-failures-then-success pass; console matches `io/InCollege-Output.txt`. | Verify echoed “Enter Your Choice: X” lines appear once per menu. |
| **Profile Flow** | Login → Create/Edit profile → Enter data; attempt BACK mid-flow to exit gracefully. | Profile persists to `data/profiles.txt`; BACK cancels cleanly with “Returning…” message. | Test adding experiences/education up to limit; verify serialization uses expected `~`/`^` format. |
| **Search** | Run exact-match, partial, blank, and BACK queries; view results. | Exact matches show profile; partial mismatch prints not-found; blank prints dedicated warning; BACK returns to menu. | Inspect `data/search_history.txt` to confirm logged entries (result field shows FOUND/NOT_FOUND/BLANK_INPUT/BACK). |
| **Menu Traversal** | Login, then walk through skills, jobs, messages, requests menus. | Each menu prints options once per loop; echoed `Enter Your Choice: X` lines show the user’s input; BACK exits to prior menu. | For jobs/messages, ensure choice buffer resets so re-entry works even after selecting BACK earlier. |
| **Persistence Recovery** | Delete `data/search_history.txt`, restart program, run a search. | File is recreated during initialization; new log entries appear without errors. | Optionally delete other data files and confirm existing init routines load fallback examples if available. |

---

## 5. Coordination & Next Steps

- **For Dev 2 (Display/QOL)**  
  - Mirror the new BACK instructions and echoed choice formatting in UI text and test fixtures.  
  - If they adjust copy (e.g., lowercase vs uppercase prompts), notify QA so console/file diffs stay aligned.  
  - Consider highlighting the search logging data in a future UI module.

- **For QA/Testers**  
  - Extend automated coverage beyond login by scripting profile/search flows.  
  - When running the login suite, remember the temporary `data/users.txt` adjustment to stay within the 5-account limit.  
  - Capture screenshots/logs of the new BACK behavior to document the improved UX.

- **Future Enhancements**  
  - Normalize all shell scripts to LF endings to avoid cross-platform friction.  
  - Expand logging to other flows (e.g., profile edits) if audit trails become necessary.  
  - Consider adding a CLI flag or environment variable to enable/disable login menu echoing for specific tests.
