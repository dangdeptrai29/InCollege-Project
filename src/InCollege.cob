       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INCOLLEGE.
       AUTHOR. InCollege Team.
       *> Epic 1 – Task 1: Notify user on unsuccessful login
       *> Follow PDF requirements: read input from file, display output to
       *> screen and also write identical output to an output file.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "io/InCollege_Input.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-IN-STATUS.
           SELECT OUTPUT-FILE ASSIGN TO "io/InCollege_Output.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT USERS-FILE ASSIGN TO "data/users.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-USR-STATUS.
           SELECT USERS-EXAMPLE-FILE ASSIGN TO "data/users.examples.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-UEX-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-REC                  PIC X(256).

       FD  OUTPUT-FILE.
       01  OUTPUT-REC                 PIC X(256).

       FD  USERS-FILE.
       01  USER-REC                   PIC X(256).

      FD  USERS-EXAMPLE-FILE.
      01  USER-REC-EX                PIC X(256).

       WORKING-STORAGE SECTION.
       *> File status codes
       01  WS-IN-STATUS               PIC XX VALUE "00".
       01  WS-OUT-STATUS              PIC XX VALUE "00".
       01  WS-USR-STATUS              PIC XX VALUE "00".
       01  WS-UEX-STATUS              PIC XX VALUE "00".

       *> End-of-file flags with condition names
       01  WS-EOF-IN                  PIC X  VALUE 'N'.
           88  EOF-IN                         VALUE 'Y'.
           88  NOT-EOF-IN                     VALUE 'N'.
       01  WS-EOF-USR                 PIC X  VALUE 'N'.
           88  EOF-USR                        VALUE 'Y'.
           88  NOT-EOF-USR                    VALUE 'N'.

       *> Credentials for the current attempt
       01  WS-USERNAME                PIC X(128) VALUE SPACES.
       01  WS-PASSWORD                PIC X(128) VALUE SPACES.
       01  WS-CHOICE                  PIC X(8)   VALUE SPACES.

       *> Message buffer and constants
       01  WS-MSG                     PIC X(256) VALUE SPACES.
       01  MSG-SUCCESS                PIC X(64)  VALUE "You have successfully logged in.".
       01  MSG-FAILURE                PIC X(64)  VALUE "Incorrect username/password, please try again.".
       01  MSG-WELCOME                PIC X(64)  VALUE "Welcome to InCollege!".
       01  MSG-LOGIN                  PIC X(32)  VALUE "1. Login".
       01  MSG-CREATE                 PIC X(32)  VALUE "2. Create Account".
       01  MSG-ENTER-CHOICE           PIC X(19)  VALUE "Enter Your Choice: ".
       01  MSG-WELCOME-PFX            PIC X(9)   VALUE "Welcome, ".
       01  MSG-ENTER-USER             PIC X(64)  VALUE "Please enter your username:".
       01  MSG-ENTER-PASS             PIC X(64)  VALUE "Please enter your password:".
       01  MSG-UNDER-CONST            PIC X(32)  VALUE "Under construction".
       01  MSG-INVALID-CHOICE         PIC X(32)  VALUE "Invalid option".

       *> In-memory users table loaded at init to avoid re-scanning file
       01  WS-MAX-USERS               PIC 9(4) VALUE 200.
       01  WS-USERS-COUNT             PIC 9(4) VALUE 0.
       01  WS-USERS-TABLE.
           05  WS-USER OCCURS 0 TO 200 TIMES
                   DEPENDING ON WS-USERS-COUNT
                   INDEXED BY USR-IDX.
               10  WS-TBL-USERNAME    PIC X(128).
               10  WS-TBL-PASSWORD    PIC X(128).

       01  WS-I                       PIC 9(4) VALUE 0.

       *> Scratch area for parsing user file records
       01  WS-USER-FILE-USERNAME      PIC X(128) VALUE SPACES.
       01  WS-USER-FILE-PASSWORD      PIC X(128) VALUE SPACES.

       *> Match flag with condition names
       01  WS-MATCH-FOUND             PIC X VALUE 'N'.
           88  MATCH-FOUND                    VALUE 'Y'.
           88  MATCH-NOT-FOUND                VALUE 'N'.

       *> Variables to hold input while creating new account
       01  WS-NEW-USERNAME            PIC X(128) VALUE SPACES.
       01  WS-NEW-PASSWORD            PIC X(128) VALUE SPACES.

       *> Vars for validating password:
       01  WS-PASSWORD-INVALID        PIC X VALUE 'N'.
           88 PASS-VALID              VALUE 'N'.
           88 PASS-INVALID            VALUE 'Y'.

       01  WS-PASSWORD-ERROR          PIC X(128) VALUE SPACES.
       01  WS-PASS-LEN                PIC 9(4) VALUE 0.
       01  WS-UPPER-COUNT             PIC 9(4) VALUE 0.
       01  WS-DIGIT-COUNT             PIC 9(4) VALUE 0.
       01  WS-SPECIAL-COUNT           PIC 9(4) VALUE 0.
       01  WS-SPECIAL-CHARS           PIC X(20) VALUE "!@#$%^&*-_+".

              *> Message for account creation 
       01 MSG-ACCOUNT-LIMIT           PIC X(64) VALUE "All permitted accounts have been created.".
       01 MSG-USERNAME-EXISTS         PIC X(64) VALUE "Username already exists. Please try a different one.".
       01 MSG-ENTER-NEW-USER          PIC X(64) VALUE "Enter new username:".
       01 MSG-ENTER-NEW-PASS          PIC X(64) VALUE "Enter new password:".
       01 MSG-ACCOUNT-SUCCESS         PIC X(64) VALUE "Account created successfully.".

       01  WS-LOGGED-CHOICE           PIC X(8) VALUE SPACES.
       01  WS-SKILL-CHOICE            PIC X(8) VALUE SPACES.

       01  MSG-MENU-JOB               PIC X(32) VALUE "Search for a new job".
       01  MSG-MENU-FIND              PIC X(32) VALUE "Find someone you know".
       01  MSG-MENU-SKILL             PIC X(32) VALUE "Learn a new skill".
       01  MSG-ENTER-CHOICE2          PIC X(19) VALUE "Enter your choice: ".

       01  MSG-SKILL1                 PIC X(32) VALUE "Skill 1".
       01  MSG-SKILL2                 PIC X(32) VALUE "Skill 2".
       01  MSG-SKILL3                 PIC X(32) VALUE "Skill 3".
       01  MSG-SKILL4                 PIC X(32) VALUE "Skill 4".
       01  MSG-SKILL5                 PIC X(32) VALUE "Skill 5".
       01  MSG-SKILL6                 PIC X(32) VALUE "Go back".
       01  MSG-ENTER-SKILL            PIC X(19) VALUE "Enter your choice: ".
       01  MSG-SKILL-UNDER            PIC X(64) VALUE "This skill is under construction.".


       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM INIT-FILES
           PERFORM RUN-APP
           PERFORM CLOSE-FILES
           GOBACK.

       INITIALIZATION-SECTION.
       INIT-FILES.
           *> File-driven only: open input and output files; create/overwrite output.
           OPEN INPUT  INPUT-FILE
                OUTPUT OUTPUT-FILE.

           *> Load users from file into memory (optional if file missing)
           PERFORM INIT-LOAD-ACCOUNTS
           EXIT.

       CLOSE-FILES.
           CLOSE INPUT-FILE OUTPUT-FILE
           EXIT.

       MENU-SECTION.
       RUN-APP.
           *> Show main menu and route based on choice
           MOVE MSG-WELCOME      TO WS-MSG PERFORM DISPLAY-AND-LOG
           MOVE MSG-LOGIN        TO WS-MSG PERFORM DISPLAY-AND-LOG
           MOVE MSG-CREATE       TO WS-MSG PERFORM DISPLAY-AND-LOG
           *> Always show the prompt before reading (file-driven simulation)
           MOVE MSG-ENTER-CHOICE TO WS-MSG PERFORM DISPLAY-AND-LOG
           PERFORM READ-CHOICE
           IF EOF-IN
              EXIT PARAGRAPH
           END-IF
           *> Echo the choice value for verification
           MOVE SPACES TO WS-MSG
           STRING MSG-ENTER-CHOICE DELIMITED BY SIZE
                  WS-CHOICE        DELIMITED BY SIZE
                  INTO WS-MSG
           END-STRING
           PERFORM DISPLAY-AND-LOG
           EVALUATE WS-CHOICE
             WHEN '1'
               PERFORM LOGIN
             WHEN '2'
               PERFORM CREATE-ACCOUNT
             WHEN OTHER
               MOVE MSG-INVALID-CHOICE TO WS-MSG PERFORM DISPLAY-AND-LOG
           END-EVALUATE
           EXIT.

       LOGIN-SECTION.
       LOGIN.
           *> Loop until correct credentials or EOF
           PERFORM UNTIL MATCH-FOUND OR EOF-IN
              MOVE MSG-ENTER-USER TO WS-MSG PERFORM DISPLAY-AND-LOG
              PERFORM READ-USERNAME
              IF EOF-IN
                 EXIT PERFORM
              END-IF
              MOVE MSG-ENTER-PASS TO WS-MSG PERFORM DISPLAY-AND-LOG
              PERFORM READ-PASSWORD
              IF EOF-IN
                 MOVE MSG-FAILURE TO WS-MSG
                 PERFORM DISPLAY-AND-LOG
                 EXIT PERFORM
              END-IF

              PERFORM CHECK-CREDENTIALS

              IF MATCH-FOUND
                 MOVE MSG-SUCCESS TO WS-MSG
                 PERFORM DISPLAY-AND-LOG
                 *> Print personalized welcome
                 MOVE SPACES TO WS-MSG
                 STRING MSG-WELCOME-PFX DELIMITED BY SIZE
                        WS-USERNAME     DELIMITED BY SIZE
                        INTO WS-MSG
                 END-STRING
                 PERFORM DISPLAY-AND-LOG

                 PERFORM LOGGED-IN-MENU

                 EXIT PERFORM
              ELSE
                 MOVE MSG-FAILURE TO WS-MSG
                 PERFORM DISPLAY-AND-LOG
                 *> Continue asking for credentials
              END-IF
           END-PERFORM
           EXIT.

       READ-USERNAME.
           *> Read one line for username; trim whitespace
           MOVE SPACES TO WS-USERNAME
           READ INPUT-FILE
               AT END SET EOF-IN       TO TRUE
               NOT AT END
                   MOVE FUNCTION TRIM(INPUT-REC) TO WS-USERNAME
           END-READ
           EXIT.

       READ-PASSWORD.
           *> Read one line for password; trim whitespace
           MOVE SPACES TO WS-PASSWORD
           READ INPUT-FILE
               AT END SET EOF-IN       TO TRUE
               NOT AT END
                   MOVE FUNCTION TRIM(INPUT-REC) TO WS-PASSWORD
           END-READ
           EXIT.

       READ-CHOICE.
           MOVE SPACES TO WS-CHOICE
           READ INPUT-FILE
               AT END SET EOF-IN TO TRUE
               NOT AT END
                   MOVE FUNCTION TRIM(INPUT-REC) TO WS-CHOICE
           END-READ
           EXIT.


       CREATE-ACCOUNT.
           *>Check max account
           IF WS-USERS-COUNT >= 5
               MOVE MSG-ACCOUNT-LIMIT TO WS-MSG PERFORM DISPLAY-AND-LOG
               EXIT PARAGRAPH
           END-IF
           
           *> Prompt for new username
           PERFORM UNTIL WS-NEW-USERNAME NOT = SPACES AND MATCH-NOT-FOUND
               MOVE MSG-ENTER-NEW-USER TO WS-MSG PERFORM DISPLAY-AND-LOG
               PERFORM READ-NEW-USERNAME

               *> CHECK UNIQUENESS
               SET MATCH-NOT-FOUND TO TRUE
               PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-USERS-COUNT OR MATCH-FOUND
                   IF WS-NEW-USERNAME = WS-TBL-USERNAME(WS-I)
                       SET MATCH-FOUND TO TRUE
                   END-IF
               END-PERFORM
               IF MATCH-FOUND
                   MOVE MSG-USERNAME-EXISTS TO WS-MSG PERFORM DISPLAY-AND-LOG
                   MOVE SPACES TO WS-NEW-USERNAME
               END-IF
           END-PERFORM

           *> PROMPT FOR NEW PASSWORD WITH VALIDATION
           PERFORM UNTIL PASS-VALID
               MOVE MSG-ENTER-NEW-PASS TO WS-MSG PERFORM DISPLAY-AND-LOG

               PERFORM READ-NEW-PASSWORD
               PERFORM VALIDATE-PASSWORD

               IF PASS-INVALID
                   MOVE WS-PASSWORD-ERROR TO WS-MSG PERFORM DISPLAY-AND-LOG
               END-IF
           END-PERFORM
           
           *> SAVE NEW ACCOUNT
           ADD 1 TO WS-USERS-COUNT
           MOVE WS-NEW-USERNAME TO WS-TBL-USERNAME(WS-USERS-COUNT)
           MOVE WS-NEW-PASSWORD TO WS-TBL-PASSWORD(WS-USERS-COUNT)

           *> UPDATE USERS.TXT
           OPEN EXTEND USERS-FILE
           STRING WS-NEW-USERNAME DELIMITED BY SIZE 
                   "|" DELIMITED BY SIZE
                   WS-NEW-PASSWORD DELIMITED BY SIZE INTO USER-REC
           END-STRING
           WRITE USER-REC
           CLOSE USERS-FILE

           MOVE MSG-ACCOUNT-SUCCESS TO WS-MSG PERFORM DISPLAY-AND-LOG
           EXIT.

       READ-NEW-USERNAME.
           MOVE SPACES TO WS-NEW-USERNAME
           READ INPUT-FILE
               AT END SET EOF-IN TO TRUE
               NOT AT END
                   MOVE FUNCTION TRIM(INPUT-REC) TO WS-NEW-USERNAME
           END-READ
           EXIT.

       READ-NEW-PASSWORD.
           MOVE SPACES TO WS-NEW-PASSWORD
           READ INPUT-FILE
               AT END SET EOF-IN TO TRUE
               NOT AT END
                   MOVE FUNCTION TRIM(INPUT-REC) TO WS-NEW-PASSWORD
           END-READ
           EXIT.

       VALIDATE-PASSWORD.
           SET PASS-VALID TO TRUE
           MOVE SPACES TO WS-PASSWORD-ERROR

           *> Check length
           MOVE FUNCTION LENGTH(WS-NEW-PASSWORD) TO WS-PASS-LEN
           IF WS-PASS-LEN < 8 OR WS-PASS-LEN > 12
               SET PASS-INVALID TO TRUE
               MOVE "Password must be 8–12 characters." TO WS-PASSWORD-ERROR
           END-IF

           *> Count uppercase letters
           INSPECT WS-NEW-PASSWORD TALLYING WS-UPPER-COUNT FOR ALL 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
           IF WS-UPPER-COUNT = 0
               SET PASS-INVALID TO TRUE
               MOVE "Password must contain at least one uppercase letter." TO WS-PASSWORD-ERROR
           END-IF

           *> Count digits
           INSPECT WS-NEW-PASSWORD TALLYING WS-DIGIT-COUNT FOR ALL '0123456789'
           IF WS-DIGIT-COUNT = 0
               SET PASS-INVALID TO TRUE
               MOVE "Password must contain at least one digit." TO WS-PASSWORD-ERROR
           END-IF

           *> Count special characters
           INSPECT WS-NEW-PASSWORD TALLYING WS-SPECIAL-COUNT FOR ALL WS-SPECIAL-CHARS
           IF WS-SPECIAL-COUNT = 0
               SET PASS-INVALID TO TRUE
               MOVE "Password must contain at least one special character (!@#$%^&*?-_+)." TO WS-PASSWORD-ERROR
           END-IF
           EXIT.

       LOGGED-IN-SECTION.
       LOGGED-IN-MENU.
           PERFORM FOREVER
               MOVE MSG-MENU-FIND TO WS-MSG PERFORM DISPLAY-AND-LOG
               MOVE MSG-MENU-JOB TO WS-MSG PERFORM DISPLAY-AND-LOG
               MOVE MSG-MENU-SKILL TO WS-MSG PERFORM DISPLAY-AND-LOG
               MOVE MSG-ENTER-CHOICE2 TO WS-MSG PERFORM DISPLAY-AND-LOG

               PERFORM READ-LOGGED-CHOICE

               EVALUATE WS-LOGGED-CHOICE
                   WHEN '1'
                       MOVE SPACES TO WS-MSG
                       STRING "Find someone you know" DELIMITED BY SIZE
                           " is under construction" DELIMITED BY SIZE
                           INTO WS-MSG
                       END-STRING
                       PERFORM DISPLAY-AND-LOG
                   WHEN '2'
                       MOVE SPACES TO WS-MSG
                       STRING "Search for a new job" DELIMITED BY SIZE
                           " is under construction" DELIMITED BY SIZE
                           INTO WS-MSG
                       END-STRING
                       PERFORM DISPLAY-AND-LOG

                   WHEN '3'
                       PERFORM SKILL-MENU

                   WHEN OTHER
                       MOVE MSG-INVALID-CHOICE TO WS-MSG PERFORM DISPLAY-AND-LOG
               END-EVALUATE
           END-PERFORM
           EXIT.
       READ-LOGGED-CHOICE.
           MOVE SPACES TO WS-LOGGED-CHOICE
           READ INPUT-FILE
               AT END SET EOF-IN TO TRUE
               NOT AT END
                   MOVE FUNCTION TRIM(INPUT-REC) TO WS-LOGGED-CHOICE
           END-READ
           EXIT.

       SKILL-MENU.
           PERFORM UNTIL WS-SKILL-CHOICE = '6'
               MOVE MSG-SKILL1 TO WS-MSG PERFORM DISPLAY-AND-LOG
               MOVE MSG-SKILL2 TO WS-MSG PERFORM DISPLAY-AND-LOG
               MOVE MSG-SKILL3 TO WS-MSG PERFORM DISPLAY-AND-LOG
               MOVE MSG-SKILL4 TO WS-MSG PERFORM DISPLAY-AND-LOG
               MOVE MSG-SKILL5 TO WS-MSG PERFORM DISPLAY-AND-LOG
               MOVE MSG-ENTER-SKILL TO WS-MSG PERFORM DISPLAY-AND-LOG

               PERFORM READ-SKILL-CHOICE

               EVALUATE WS-SKILL-CHOICE
                   WHEN '1' THRU '5'
                       MOVE MSG-SKILL-UNDER TO WS-MSG PERFORM DISPLAY-AND-LOG
                   WHEN '6'
                       PERFORM LOGGED-IN-MENU
                   WHEN OTHER
                       MOVE MSG-INVALID-CHOICE TO WS-MSG PERFORM DISPLAY-AND-LOG
               END-EVALUATE
           END-PERFORM
           EXIT.
       READ-SKILL-CHOICE.
           MOVE SPACES TO WS-SKILL-CHOICE
           READ INPUT-FILE
               AT END SET EOF-IN TO TRUE
               NOT AT END
                   MOVE FUNCTION TRIM(INPUT-REC) TO WS-SKILL-CHOICE
           END-READ
           EXIT.
          
       VALIDATION-SECTION.
       CHECK-CREDENTIALS.
           *> Scan in-memory users table for an exact, case-sensitive match
           SET MATCH-NOT-FOUND TO TRUE
           IF WS-USERS-COUNT = 0
              *> No users loaded; cannot match
              EXIT PARAGRAPH
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-USERS-COUNT OR MATCH-FOUND
              IF WS-USERNAME = WS-TBL-USERNAME(WS-I)
                 AND WS-PASSWORD = WS-TBL-PASSWORD(WS-I)
                 SET MATCH-FOUND TO TRUE
              END-IF
           END-PERFORM
           EXIT.

       PARSING-SECTION.
       PARSE-USER-REC.
           *> Split a line of the form username|password and trim parts
           MOVE SPACES TO WS-USER-FILE-USERNAME WS-USER-FILE-PASSWORD
           UNSTRING USER-REC
               DELIMITED BY '|'
               INTO WS-USER-FILE-USERNAME
                    WS-USER-FILE-PASSWORD
           END-UNSTRING
           MOVE FUNCTION TRIM(WS-USER-FILE-USERNAME) TO WS-USER-FILE-USERNAME
           MOVE FUNCTION TRIM(WS-USER-FILE-PASSWORD) TO WS-USER-FILE-PASSWORD
           EXIT.

       INIT-LOAD-ACCOUNTS.
           *> Prefer real users.txt; fall back to users.examples.txt for testing
           OPEN INPUT USERS-FILE
           IF WS-USR-STATUS = "00"
              PERFORM LOAD-ACCOUNTS-FROM-USERS
              CLOSE USERS-FILE
              EXIT PARAGRAPH
           END-IF

           OPEN INPUT USERS-EXAMPLE-FILE
           IF WS-UEX-STATUS = "00"
              PERFORM LOAD-ACCOUNTS-FROM-EXAMPLE
              CLOSE USERS-EXAMPLE-FILE
           END-IF
           EXIT.

       LOAD-ACCOUNTS-FROM-USERS.
           SET NOT-EOF-USR TO TRUE
           PERFORM UNTIL EOF-USR
              READ USERS-FILE
                 AT END SET EOF-USR TO TRUE
                 NOT AT END
                    MOVE USER-REC TO USER-REC  *> no-op; keep symmetry
                    PERFORM PARSE-USER-REC
                    IF WS-USER-FILE-USERNAME NOT = SPACES AND WS-USER-FILE-PASSWORD NOT = SPACES
                       IF WS-USERS-COUNT < WS-MAX-USERS
                          ADD 1 TO WS-USERS-COUNT
                          MOVE WS-USER-FILE-USERNAME TO WS-TBL-USERNAME(WS-USERS-COUNT)
                          MOVE WS-USER-FILE-PASSWORD TO WS-TBL-PASSWORD(WS-USERS-COUNT)
                       END-IF
                    END-IF
              END-READ
           END-PERFORM
           EXIT.

       LOAD-ACCOUNTS-FROM-EXAMPLE.
           SET NOT-EOF-USR TO TRUE
           PERFORM UNTIL EOF-USR
              READ USERS-EXAMPLE-FILE
                 AT END SET EOF-USR TO TRUE
                 NOT AT END
                    MOVE USER-REC-EX TO USER-REC
                    PERFORM PARSE-USER-REC
                    IF WS-USER-FILE-USERNAME NOT = SPACES AND WS-USER-FILE-PASSWORD NOT = SPACES
                       IF WS-USERS-COUNT < WS-MAX-USERS
                          ADD 1 TO WS-USERS-COUNT
                          MOVE WS-USER-FILE-USERNAME TO WS-TBL-USERNAME(WS-USERS-COUNT)
                          MOVE WS-USER-FILE-PASSWORD TO WS-TBL-PASSWORD(WS-USERS-COUNT)
                       END-IF
                    END-IF
              END-READ
           END-PERFORM
           EXIT.

       IO-SECTION.
       DISPLAY-AND-LOG.
           *> Write message to output file and display it
           MOVE SPACES TO OUTPUT-REC
           MOVE FUNCTION TRIM(WS-MSG) TO OUTPUT-REC
           WRITE OUTPUT-REC
           DISPLAY FUNCTION TRIM(WS-MSG)
           EXIT.
