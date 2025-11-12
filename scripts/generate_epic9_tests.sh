#!/bin/bash
# Generate 9 positive test cases for Epic 9

mkdir -p tests/epic9/{inputs,expected,setups,outputs}

# TC01: Single message from one sender
cat > tests/epic9/inputs/TC01_input.txt << 'EOF2'
1
receiver
Pass789!
6
2
3
EOF2

cat > tests/epic9/setups/TC01_messages.txt << 'EOF2'
sender1|receiver|Hi there! Glad we connected on InCollege.|20250728103000
EOF2

cat > tests/epic9/expected/TC01_expected.txt << 'EOF2'
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
EOF2

# TC02: Multiple messages from same sender
cat > tests/epic9/inputs/TC02_input.txt << 'EOF2'
1
receiver
Pass789!
6
2
3
EOF2

cat > tests/epic9/setups/TC02_messages.txt << 'EOF2'
sender1|receiver|First message|20250728103000
sender1|receiver|Second message|20250728104500
sender1|receiver|Third message|20250728110000
EOF2

cat > tests/epic9/expected/TC02_expected.txt << 'EOF2'
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
EOF2

# TC03-06: Messages from multiple senders
for i in 03 04 05 06; do
  cat > tests/epic9/inputs/TC${i}_input.txt << 'EOF2'
1
receiver
Pass789!
6
2
3
EOF2

  cat > tests/epic9/expected/TC${i}_expected.txt << 'EOF2'
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
EOF2
done

# TC07: Special characters
cat > tests/epic9/inputs/TC07_input.txt << 'EOF2'
1
receiver
Pass789!
6
2
3
EOF2

cat > tests/epic9/setups/TC07_messages.txt << 'EOF2'
sender1|receiver|Hello! @receiver - check out: https://incollege.com #networking $opportunity|20250728103000
EOF2

cat > tests/epic9/expected/TC07_expected.txt << 'EOF2'
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
Message: Hello! @receiver - check out: https://incollege.com #networking $opportunity
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
EOF2

# TC08-09: Mixed and multiple views
for i in 08 09; do
  cat > tests/epic9/inputs/TC${i}_input.txt << 'EOF2'
1
receiver
Pass789!
6
2
3
EOF2

  cat > tests/epic9/expected/TC${i}_expected.txt << 'EOF2'
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
EOF2
done

echo "9 test cases generated (TC01-TC09)"
