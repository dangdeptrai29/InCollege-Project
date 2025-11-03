# Epic 8 – Tester 2 Results (Task 10)

| Test ID | Scenario | Status | Notes |
| --- | --- | --- | --- |
| TC01_invalid_recipient_missing_user | Recipient username not found in users table | ✅ PASS | `User not found in your network.` shown; no entry written to `data/messages.txt`. |
| TC02_invalid_recipient_not_connected | Recipient exists but lacks accepted connection | ✅ PASS | Same error emitted; log mirrors console exactly. |
| TC03_invalid_recipient_pending | Connection status pending (`P`) | ✅ PASS | Pending treated as not connected; messages file unchanged. |
| TC04_empty_message | Blank message body after valid connection | ✅ PASS | Rejects with `Message cannot be empty.` and returns to menu. |
| TC05_message_too_long | 201-character payload | ✅ PASS | Length guard triggers `Message exceeds 200 characters.`; no persistence. |
| TC06_message_exactly_200 | Boundary success at 200 characters | ✅ PASS | Success banner displayed; `messages.txt` captured 200-char record. |
| TC07_message_with_special_chars | Valid message using punctuation symbols | ✅ PASS | Message delivered and persisted with full symbol set intact. |

All console transcripts (`tests/epic8/outputs/*_console.txt`) match their corresponding log files and expected baselines.
