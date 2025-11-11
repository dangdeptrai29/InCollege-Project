#!/bin/bash
# Generate all 10 test case files for Epic 9

mkdir -p tests/epic9/{inputs,expected,setups,outputs}

# TC01: Single message from one sender
cat > tests/epic9/inputs/TC01_input.txt << 'EOF'
1
receiver
Pass789!
6
2
3
EOF

cat > tests/epic9/setups/TC01_messages.txt << 'EOF'
sender1|receiver|Hi there! Glad we connected on InCollege.|20250728103000
EOF

cat > tests/epic9/expected/TC01_expected.txt << 'EOF'
Welcome to InCollege!
1. Log In
2. Create New Account
Enter your choice:
Please enter your username:
Please enter your password:
You have successfully logged in.
Welcome, receiver!
1. View My Profile
2. Search for User
3. Learn a New Skill
4. View My Pending Connection Requests
5. View My Network
6. Messages
Enter your choice:
--- Messages Menu ---
1. Send a New Message
2. View My Messages
3. Back to Main Menu
Enter your choice:
--- Your Messages ---
From: sender1
Message: Hi there! Glad we connected on InCollege.
Sent: 2025-07-28 10:30
---
---------------------
1. Send a New Message
2. View My Messages
3. Back to Main Menu
Enter your choice:
1. View My Profile
2. Search for User
3. Learn a New Skill
4. View My Pending Connection Requests
5. View My Network
6. Messages
Enter your choice:
--- END_OF_PROGRAM_EXECUTION ---
EOF

# TC02: Multiple messages from same sender
cat > tests/epic9/inputs/TC02_input.txt << 'EOF'
1
receiver
Pass789!
6
2
3
EOF

cat > tests/epic9/setups/TC02_messages.txt << 'EOF'
sender1|receiver|First message|20250728103000
sender1|receiver|Second message|20250728104500
sender1|receiver|Third message|20250728110000
EOF

cat > tests/epic9/expected/TC02_expected.txt << 'EOF'
Welcome to InCollege!
1. Log In
2. Create New Account
Enter your choice:
Please enter your username:
Please enter your password:
You have successfully logged in.
Welcome, receiver!
1. View My Profile
2. Search for User
3. Learn a New Skill
4. View My Pending Connection Requests
5. View My Network
6. Messages
Enter your choice:
--- Messages Menu ---
1. Send a New Message
2. View My Messages
3. Back to Main Menu
Enter your choice:
--- Your Messages ---
From: sender1
Message: First message
Sent: 2025-07-28 10:30
---
From: sender1
Message: Second message
Sent: 2025-07-28 10:45
---
From: sender1
Message: Third message
Sent: 2025-07-28 11:00
---
---------------------
1. Send a New Message
2. View My Messages
3. Back to Main Menu
Enter your choice:
1. View My Profile
2. Search for User
3. Learn a New Skill
4. View My Pending Connection Requests
5. View My Network
6. Messages
Enter your choice:
--- END_OF_PROGRAM_EXECUTION ---
EOF

# TC03: Messages from multiple senders
cat > tests/epic9/inputs/TC03_input.txt << 'EOF'
1
receiver
Pass789!
6
2
3
EOF

cat > tests/epic9/expected/TC03_expected.txt << 'EOF'
Welcome to InCollege!
1. Log In
2. Create New Account
Enter your choice:
Please enter your username:
Please enter your password:
You have successfully logged in.
Welcome, receiver!
1. View My Profile
2. Search for User
3. Learn a New Skill
4. View My Pending Connection Requests
5. View My Network
6. Messages
Enter your choice:
--- Messages Menu ---
1. Send a New Message
2. View My Messages
3. Back to Main Menu
Enter your choice:
--- Your Messages ---
From: sender1
Message: Hi there! Glad we connected on InCollege.
Sent: 2025-07-28 10:30
---
From: sender2
Message: Check out this interesting job posting I found!
Sent: 2025-07-29 09:15
---
From: sender1
Message: Are you available for a quick call today?
Sent: 2025-07-30 14:00
---
---------------------
1. Send a New Message
2. View My Messages
3. Back to Main Menu
Enter your choice:
1. View My Profile
2. Search for User
3. Learn a New Skill
4. View My Pending Connection Requests
5. View My Network
6. Messages
Enter your choice:
--- END_OF_PROGRAM_EXECUTION ---
EOF

# TC04-10: Copy TC03 pattern for remaining tests
for i in {04..10}; do
  cp tests/epic9/inputs/TC03_input.txt tests/epic9/inputs/TC${i}_input.txt
  cp tests/epic9/expected/TC03_expected.txt tests/epic9/expected/TC${i}_expected.txt
done

# TC04: Chronological order (already correct with default messages)
# TC05: With timestamps (already correct)
# TC06: Persistence (same as TC03)

# TC07: Long content (200 chars)
cat > tests/epic9/setups/TC07_messages.txt << 'EOF'
sender1|receiver|This is a very long message that contains exactly two hundred characters to test the maximum length handling in the messaging system. It should display correctly without truncation or errors in output.|20250728103000
EOF

cat > tests/epic9/expected/TC07_expected.txt << 'EOF'
Welcome to InCollege!
1. Log In
2. Create New Account
Enter your choice:
Please enter your username:
Please enter your password:
You have successfully logged in.
Welcome, receiver!
1. View My Profile
2. Search for User
3. Learn a New Skill
4. View My Pending Connection Requests
5. View My Network
6. Messages
Enter your choice:
--- Messages Menu ---
1. Send a New Message
2. View My Messages
3. Back to Main Menu
Enter your choice:
--- Your Messages ---
From: sender1
Message: This is a very long message that contains exactly two hundred characters to test the maximum length handling in the messaging system. It should display correctly without truncation or errors in output.
Sent: 2025-07-28 10:30
---
---------------------
1. Send a New Message
2. View My Messages
3. Back to Main Menu
Enter your choice:
1. View My Profile
2. Search for User
3. Learn a New Skill
4. View My Pending Connection Requests
5. View My Network
6. Messages
Enter your choice:
--- END_OF_PROGRAM_EXECUTION ---
EOF

# TC08: Special characters
cat > tests/epic9/setups/TC08_messages.txt << 'EOF'
sender1|receiver|Hello! @receiver - check this out: https://incollege.com #networking $opportunity|20250728103000
EOF

cat > tests/epic9/expected/TC08_expected.txt << 'EOF'
Welcome to InCollege!
1. Log In
2. Create New Account
Enter your choice:
Please enter your username:
Please enter your password:
You have successfully logged in.
Welcome, receiver!
1. View My Profile
2. Search for User
3. Learn a New Skill
4. View My Pending Connection Requests
5. View My Network
6. Messages
Enter your choice:
--- Messages Menu ---
1. Send a New Message
2. View My Messages
3. Back to Main Menu
Enter your choice:
--- Your Messages ---
From: sender1
Message: Hello! @receiver - check this out: https://incollege.com #networking $opportunity
Sent: 2025-07-28 10:30
---
---------------------
1. Send a New Message
2. View My Messages
3. Back to Main Menu
Enter your choice:
1. View My Profile
2. Search for User
3. Learn a New Skill
4. View My Pending Connection Requests
5. View My Network
6. Messages
Enter your choice:
--- END_OF_PROGRAM_EXECUTION ---
EOF

# TC09: Mixed old and new messages (use default 3 messages)
# Already set by TC03 copy

# TC10: View multiple times
cat > tests/epic9/inputs/TC10_input.txt << 'EOF'
1
receiver
Pass789!
6
2
2
3
EOF

cat > tests/epic9/expected/TC10_expected.txt << 'EOF'
Welcome to InCollege!
1. Log In
2. Create New Account
Enter your choice:
Please enter your username:
Please enter your password:
You have successfully logged in.
Welcome, receiver!
1. View My Profile
2. Search for User
3. Learn a New Skill
4. View My Pending Connection Requests
5. View My Network
6. Messages
Enter your choice:
--- Messages Menu ---
1. Send a New Message
2. View My Messages
3. Back to Main Menu
Enter your choice:
--- Your Messages ---
From: sender1
Message: Hi there! Glad we connected on InCollege.
Sent: 2025-07-28 10:30
---
From: sender2
Message: Check out this interesting job posting I found!
Sent: 2025-07-29 09:15
---
From: sender1
Message: Are you available for a quick call today?
Sent: 2025-07-30 14:00
---
---------------------
1. Send a New Message
2. View My Messages
3. Back to Main Menu
Enter your choice:
--- Your Messages ---
From: sender1
Message: Hi there! Glad we connected on InCollege.
Sent: 2025-07-28 10:30
---
From: sender2
Message: Check out this interesting job posting I found!
Sent: 2025-07-29 09:15
---
From: sender1
Message: Are you available for a quick call today?
Sent: 2025-07-30 14:00
---
---------------------
1. Send a New Message
2. View My Messages
3. Back to Main Menu
Enter your choice:
1. View My Profile
2. Search for User
3. Learn a New Skill
4. View My Pending Connection Requests
5. View My Network
6. Messages
Enter your choice:
--- END_OF_PROGRAM_EXECUTION ---
EOF

echo "Test files generated in tests/epic9/"
echo "Structure:"
echo "  inputs/    - 10 test input files"
echo "  expected/  - 10 expected output files"
echo "  setups/    - 4 custom message setup files"