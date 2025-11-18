# Epic 9 Architecture: View Messages

## Overview
Epic 9 completes the basic messaging system by implementing the "View My Messages" feature. This allows logged-in users to retrieve and display all messages sent to them by their connections, showing both the sender and the message content.

---

## Implementation Status

### ✅ Phase 1: View Messages (COMPLETE)
**Completion Date**: Week 9

## How to Use

- Log in to the InCollege application.
- Navigate to the **Messages** menu (option 6).
- Select **View My Messages** (option 2).
- The system will display all messages addressed to your username with the sender and message content.
- If no messages are found, a clear notification will inform you ("You have no messages at this time.").


## Implementation Details

- Messages are stored and loaded from the `messages.txt` file.
- The system filters messages by the logged-in username.
- Displays messages in chronological order (your team’s choice for oldest or newest first).
- Outputs are printed to the console and simultaneously saved to an output log for audit/testing purposes.
- Each message shows:
  - Sender's username
  - Message content
- Messages are separated by a clear delimiter (`---`).

---

## Example Output

--- Your Messages ---
From: alice
Message: Hi Brandon! This is Alice.
---
From: alice
Message: Hi Brandon! This is Alice 2.
---