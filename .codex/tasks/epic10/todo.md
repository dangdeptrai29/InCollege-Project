# Epic 10 — Dev 1 Todo

Plan owner: Developer 1 (logic, input handling, persistence)

## Part 1 — Revisit Existing Functionalities
- [ ] Rework registration/login validation (password rules, account limit, login loop)
  - [x] Align password checks/messages with spec (length, uppercase, digit, allowed specials)
  - [x] Reset login attempt state after each failed attempt to keep loop reliable
  - [x] Enforce 5-account cap via shared constant/load logic
  - Tests: `tests/test_registration.py`, `tests/test_login.py`, plus manual multi-user login attempt
- [ ] Fix profile create/edit logic (required fields, saving/loading optional data)
  - [x] Reset profile input buffers/experience data before prompting to avoid stale values
  - [x] Fix experience/education serialization so persisted data matches entered fields
  - [x] Guard optional sections (About Me) for EOF/blank handling
  - Tests: `tests/test_profiles.py`, manual profile creation/edit walkthrough
- [ ] Correct search logic (exact match, record retrieval, profile lookup)
  - [x] Normalize names (trim + collapse extra spaces) before comparing stored profiles
  - [x] Capture each search attempt (username/query/result) to a history file for debugging
  - Tests: `tests/test_search.py`, manual run through search menu using sample profiles
- [ ] Verify persistence files (accounts, profiles, search history) save/load consistently
  - [x] Persist search history entries via new `data/search_history.txt`
  - Tests: Run full program, inspect `data/` files, rerun and ensure state remains
- [ ] Stabilize prompt flow/state transitions for all menus
  - [x] Reset submenu choices (skills/jobs/messages) before loops so they can be revisited
  - Tests: Manual traversal through every menu branch, confirm expected prompts appear
- [ ] Log deeper logic bugs that surface during fixes in Jira queue
  - Tests: Review logs/observations and ensure each identified issue is captured

## Part 2 — Quality-of-Life Improvements
- [ ] Improve validation messaging clarity based on logic outcomes
  - [x] Added explicit “Enter 0/BACK” instruction on login/create/profile/search prompts plus “Returning…” confirmation
  - [x] Provided dedicated message when search input is blank (vs generic not-found)
  - Tests: Trigger each validation path and confirm updated messages in output logs
- [ ] Add flow control options (returns/back navigation, no dead ends)
  - [x] Allow BACK/0 escape at login, account creation, profile fields, and search prompts
  - [x] Reset submenu choices and echo entered values so scripts/tests can follow the flow
  - Tests: Manual navigation through each submenu to ensure proper escape paths
- [ ] Simplify logic paths for login/profile/search modules
  - [x] Added shared helper to detect BACK input and centralized “returning” handling
  - Tests: Run affected unit tests and perform code walkthrough to confirm reduction
- [ ] Handle edge cases (empty profiles, missing files, invalid inputs)
  - [x] Treat blank search queries as a special case instead of matching against empty strings
  - Tests: Start app with missing data files, supply invalid inputs, ensure graceful handling
- [ ] Resolve QOL issues tied to persistence (atomic writes, data integrity)
  - [x] Auto-create `data/search_history.txt` when logging searches and include username/query/result/timestamp
  - Tests: Force program interruption mid-save and verify recovery strategy works

## Part 3 — Refactoring & Cleanup
- [ ] Refactor core logic modules (login, profile, search) for readability
  - [x] Added descriptive `*>` docstrings before LOGIN/CREATE-ACCOUNT/search/experience routines
  - [x] Consolidated “Enter Your Choice” echo into `ECHO-CHOICE-VALUE` helper to reduce duplication
  - Tests: Rerun all module-specific tests; confirm no behavior regression
- [ ] Remove duplicated conditionals and improve variable naming
  - [x] Introduced shared BACK-detection helper and centralized choice echo buffer
  - Tests: Static analysis/lint run plus targeted unit tests
- [ ] Clean up file I/O routines for accounts/profiles/search data
  - [x] Added `ENSURE-SEARCH-HISTORY-FILE` to create logging file during init + simplified logger
  - Tests: Integration test that creates users, profiles, searches, restarts app, and verifies data
- [ ] Document new structure and logic decisions as docstrings/comments
  - [x] Annotated major paragraphs with purpose comments (login, search, experience, education, logging)
  - Tests: Code review check ensuring coverage of updated functions

## Review & Documentation Checklist
- [ ] Capture summary of completed work and remaining risks
- [ ] Note technical debt/follow-up Jira tickets needed
- [ ] Record dependency impacts or required coordination with Dev 2
