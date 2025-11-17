# Epic 10 — Dev 1 Summary

> Scope: logic, input handling, persistence (Developer 1)  
> Companion roles: Developer 2 (display/QOL) and QA testers

---

## Part 1 – Revisit Existing Functionalities

| Area | What changed | How Dev 2/QA can leverage it |
|------|--------------|------------------------------|
| **Login** | Password validator enforces 8‑12 chars w/ uppercase+digit+special; login loop resets state; 5-account cap enforced on load/create; menu echoes choices. | Dev 2 can align display text with the echoed “Enter Your Choice: X”; QA can rerun `scripts/test_login.sh` (CR‑stripped) to verify success-first + failure-four-then-success flows. |
| **Profiles** | Input buffers reset per session; experience/education serialize correctly; About Me prompt handles EOF/back; “BACK/0” instruction shown. | Dev 2 can polish prompts knowing logic already handles BACK/0; QA can run manual profile creation/edit scenarios to ensure persistence. |
| **Search** | Names normalized (trim/collapse spaces); blank input warns user; search logs (user/query/result/timestamp) in `data/search_history.txt`. | Dev 2 can format the new blank-input message; QA can inspect `data/search_history.txt` after manual searches to confirm logging. |
| **Persistence & Menus** | Search history file auto-created; submenu choices reset; request/jobs/messages menus echo selections to match console/file spec. | Dev 2 should keep display spacing consistent; QA can traverse every menu to confirm “Enter Your Choice: X” lines appear once per input. |

---

## Part 2 – Quality-of-Life Improvements

| Area | What changed | Tips for Dev 2 / Testers |
|------|--------------|-------------------------|
| **Validation messaging** | Added explicit “Enter 0 or BACK…” instructions plus “Returning…” confirmation when user escapes a flow; blank search message clarifies requirement. | Dev 2 can style the new instructions and ensure spacing matches; QA should test BACK at login/create/profile/search to ensure graceful exits. |
| **Flow control** | BACK/0 accepted at login, account creation, profile prompts, and search; menus echo selections via shared helper; search blank input handled gracefully. | QA can attempt BACK at every prompt (including experience/education loops) to confirm no dead ends; Dev 2 can highlight the new instructions in UI copy. |
| **Edge handling & persistence** | Blank search queries flagged instead of doing false matches; search log file created automatically; safe logging if file missing. | QA can delete `data/search_history.txt` and rerun the app to confirm it recreates the log; testers can run multiple searches and confirm log entries are appended. |

---

## Part 3 – Refactoring & Cleanup

| Area | What changed | Dev 2 / QA notes |
|------|--------------|-----------------|
| **Login / Menu logic** | Added docstrings before major paragraphs; centralised choice echoing and BACK detection; reduced duplicate STRING blocks. | Dev 2 can follow the docstrings to align display updates; QA can rely on consistent menu output for snapshot tests. |
| **Profile/Search routines** | Annotated ADD-EXPERIENCE/EDUCATION and search paragraphs; normalized helper names; simplified search logging. | Dev 2 can reuse docstrings for documentation; QA can inspect `data/search_history.txt` knowing the logger runs during init and each search. |
| **File I/O** | `ENSURE-SEARCH-HISTORY-FILE` added so the history file exists before logging; logger now reuses helper and warns once if writes fail. | Testers can delete the file to confirm it reappears on startup; Dev 2 doesn’t need to handle missing file errors in display code. |

---

## Suggested Tests

1. **Automated Login Suite** – Run `scripts/test_login.sh` through a CR-normalized copy. Requires `data/users.txt` temporarily reduced to `testuser|Password1!` to honor the 5-account limit, then restored.
2. **Manual Profile Flow** – Create/edit a profile using BACK at various prompts, confirm persistence in `data/profiles.txt`, and ensure experience/education loops exit cleanly.
3. **Search Logging** – Run multiple searches (valid, invalid, blank, BACK) and inspect `data/search_history.txt` for accurate entries (`username|query|result|timestamp`).
4. **Menu Navigation** – Traverse Skills, Jobs, Requests, and Messages menus to confirm each echo (“Enter Your Choice: X”) appears once and BACK exits return to the previous menu.

---

## Coordination / Follow-ups

- **Dev 2**: Adjust display formatting so the new instructions/echo lines match UI typography; ensure their test fixtures include the echoed choice lines.
- **QA**: Expand beyond the login suite to include profile/search/logging scenarios; consider adding automated diffs for the new BACK instructions once Dev 2 finalizes UI wording.
