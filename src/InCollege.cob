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
           SELECT INPUT-FILE ASSIGN TO "io/InCollege-Input.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-IN-STATUS.
           SELECT OUTPUT-FILE ASSIGN TO "io/InCollege-Output.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT USERS-FILE ASSIGN TO "data/users.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-USR-STATUS.
           SELECT USERS-EXAMPLE-FILE ASSIGN TO "data/users.examples.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-UEX-STATUS.
           SELECT PROFILES-FILE ASSIGN TO "data/profiles.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-PROF-STATUS.
      *> New file for connections
           SELECT CONNECTIONS-FILE ASSIGN TO "data/connections.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CONN-FILE-STATUS.



       DATA DIVISION.
       FILE SECTION.
        FD  INPUT-FILE.
        01  INPUT-REC                     PIC X(256).

        FD  OUTPUT-FILE.
        01  OUTPUT-REC                    PIC X(256).

        FD  USERS-FILE.
        01  USER-REC                      PIC X(256).

        FD  USERS-EXAMPLE-FILE.
        01  USER-REC-EX                   PIC X(256).

        FD  PROFILES-FILE.
        01  PROFILE-REC                   PIC X(2048).

      *> New FD for connections file
        FD  CONNECTIONS-FILE.
        01  CONNECTION-REC                PIC X(258).


       WORKING-STORAGE SECTION.
       *> File status codes
       01  WS-IN-STATUS                  PIC XX VALUE "00".
       01  WS-OUT-STATUS                 PIC XX VALUE "00".
       01  WS-USR-STATUS                 PIC XX VALUE "00".
       01  WS-UEX-STATUS                 PIC XX VALUE "00".
       01  WS-PROF-STATUS                PIC XX VALUE "00".
       01  WS-CONN-FILE-STATUS           PIC XX VALUE "00".
       01  WS-J-DISP                     PIC 9.




       *> End-of-file flags with condition names
       01  WS-EOF-IN                     PIC X  VALUE 'N'.
           88  EOF-IN                            VALUE 'Y'.
           88  NOT-EOF-IN                        VALUE 'N'.
       01  WS-EOF-USR                    PIC X  VALUE 'N'.
           88  EOF-USR                           VALUE 'Y'.
           88  NOT-EOF-USR                       VALUE 'N'.
       01  WS-EOF-PROF                   PIC X  VALUE 'N'.
           88  EOF-PROF                          VALUE 'Y'.
           88  NOT-EOF-PROF                      VALUE 'N'.
       01  WS-EOF-CONN                   PIC X  VALUE 'N'.
           88  EOF-CONN                          VALUE 'Y'.
           88  NOT-EOF-CONN                      VALUE 'N'.

       *> Generic Input buffer
       01 WS-LINE                      PIC X(256) VALUE SPACES.

       *> Credentials for the current attempt
       01  WS-USERNAME                   PIC X(128) VALUE SPACES.
       01  WS-PASSWORD                   PIC X(128) VALUE SPACES.
       01  WS-CHOICE                     PIC X(8)   VALUE SPACES.
       01  WS-CURRENT-USERNAME           PIC X(128) VALUE SPACES.


       *> Message buffer and constants
       01  WS-MSG                        PIC X(256) VALUE SPACES.
       01  MSG-SUCCESS                   PIC X(64)  VALUE "You have successfully logged in.".
       01  MSG-FAILURE                   PIC X(64)  VALUE "Incorrect username/password, please try again.".
       01  MSG-WELCOME                   PIC X(64)  VALUE "Welcome to InCollege!".
       01  MSG-LOGIN                     PIC X(32)  VALUE "1. Log In".
       01  MSG-CREATE                    PIC X(32)  VALUE "2. Create New Account".
       01  MSG-ENTER-CHOICE              PIC X(20)  VALUE "Enter your choice: ".
       01  MSG-WELCOME-PFX               PIC X(9)   VALUE "Welcome, ".
       01  MSG-ENTER-USER                PIC X(64)  VALUE "Please enter your username:".
       01  MSG-ENTER-PASS                PIC X(64)  VALUE "Please enter your password:".
       01  MSG-UNDER-CONST               PIC X(32)  VALUE "Under construction".
       01  MSG-INVALID-CHOICE            PIC X(32)  VALUE "Invalid option".

       *> In-memory users table loaded at init to avoid re-scanning file
       01  WS-MAX-USERS                  PIC 9(4) VALUE 200.
       01  WS-USERS-COUNT                PIC 9(4) VALUE 0.
       01  WS-USERS-TABLE.
           05  WS-USER OCCURS 0 TO 200 TIMES
                   DEPENDING ON WS-USERS-COUNT
                   INDEXED BY USR-IDX.
               10  WS-TBL-USERNAME       PIC X(128).
               10  WS-TBL-PASSWORD       PIC X(128).

         *> --- existing users table ends here ---

         *> --- profiles table --

       01  WS-PROFILES-MAX               PIC 9(4) VALUE 200.
       01  WS-PROFILES-COUNT             PIC 9(4) VALUE 0.
       01  WS-PROFILES-TABLE.
           05  WS-PROFILE OCCURS 0 TO 200 TIMES
                   DEPENDING ON WS-PROFILES-COUNT
                   INDEXED BY PROF-IDX.
               10  WS-PROF-USERNAME      PIC X(128).
               10  WS-PROF-FIRST         PIC X(64).
               10  WS-PROF-LAST          PIC X(64).
               10  WS-PROF-UNIV          PIC X(128).
               10  WS-PROF-MAJOR         PIC X(128).
               10  WS-PROF-GYEAR         PIC X(4).      *> YYYY, may be SPACES

               10  WS-PROF-ABOUT         PIC X(200).

               10  WS-PROF-EXPERIENCES   PIC X(512).  *> Serialized string for experiences
               10  WS-PROF-EDUCATIONS    PIC X(512). *> Serialized string for education

      *> --- Connections table ---
       01  WS-CONNECTIONS-MAX            PIC 9(4) VALUE 500.
       01  WS-CONNECTIONS-COUNT          PIC 9(4) VALUE 0.
       01  WS-CONNECTIONS-TABLE.
           05  WS-CONNECTION OCCURS 0 TO 500 TIMES
                   DEPENDING ON WS-CONNECTIONS-COUNT
                   INDEXED BY CONN-IDX.
               10  WS-CONN-SENDER        PIC X(128).
               10  WS-CONN-RECEIVER      PIC X(128).
               10  WS-CONN-STATUS        PIC X. *> 'P'ending, 'A'ccepted


       01  WS-I                          PIC 9(4) VALUE 0.
       01  WS-SEARCH-RESULT-IDX          PIC 9(4) VALUE 0.

       *> Scratch area for parsing user file records
       01  WS-USER-FILE-USERNAME         PIC X(128) VALUE SPACES.
       01  WS-USER-FILE-PASSWORD         PIC X(128) VALUE SPACES.

       *> Match flag with condition names
       01  WS-MATCH-FOUND                PIC X VALUE 'N'.
           88  MATCH-FOUND                     VALUE 'Y'.
           88  MATCH-NOT-FOUND                 VALUE 'N'.

       *> Variables to hold input while creating new account
       01  WS-NEW-USERNAME               PIC X(128) VALUE SPACES.
       01  WS-NEW-PASSWORD               PIC X(128) VALUE SPACES.

       *> Vars for validating password:
       01  WS-PASSWORD-INVALID           PIC X VALUE 'N'.
           88 PASS-VALID                       VALUE 'N'.
           88 PASS-INVALID                     VALUE 'Y'.

       01  WS-PASSWORD-ERROR             PIC X(128) VALUE SPACES.
       01  WS-PASS-LEN                   PIC 9(4) VALUE 0.
       01  WS-UPPER-COUNT                PIC 9(4) VALUE 0.
       01  WS-DIGIT-COUNT                PIC 9(4) VALUE 0.
       01  WS-SPECIAL-COUNT              PIC 9(4) VALUE 0.
       01  WS-SPECIAL-CHARS              PIC X(20) VALUE "!@#$%^&*-_+".
       01  WS-CHAR                       PIC X      VALUE SPACE.
       01  WS-TMP-COUNT                  PIC 9(4)   VALUE 0.


       01  WS-PROF-USER                  PIC X(128) VALUE SPACES.
       01  WS-PROF-FIRST-IN              PIC X(64)  VALUE SPACES.
       01  WS-PROF-LAST-IN               PIC X(64)  VALUE SPACES.
       01  WS-PROF-UNIV-IN               PIC X(128) VALUE SPACES.
       01  WS-PROF-MAJOR-IN              PIC X(128) VALUE SPACES.
       01  WS-PROF-GYEAR-IN              PIC X(4)   VALUE SPACES.
       01  WS-PROF-ABOUT-IN              PIC X(200) VALUE SPACES.

       01  WS-GYEAR-NUM                  PIC 9(4)   VALUE 0.
       01  WS-YEAR-INVALID               PIC X      VALUE 'N'.
           88  YEAR-VALID                        VALUE 'N'.
           88  YEAR-INVALID                      VALUE 'Y'.

       01  WS-PROFILE-FOUND              PIC X      VALUE 'N'.
           88  PROFILE-FOUND                     VALUE 'Y'.
           88  PROFILE-NOT-FOUND                 VALUE 'N'.

       01  WS-PROFILE-IDX                PIC 9(4)   VALUE 0.
       01  WS-J                          PIC 9(4)   VALUE 0.

       *> temp holders for (de)serializing lists
       01  WS-EXPS-STR                   PIC X(512) VALUE SPACES.
       01  WS-EDUS-STR                   PIC X(512) VALUE SPACES.
       01  WS-ENTRY                      PIC X(256) VALUE SPACES.
       01  WS-T1                         PIC X(128) VALUE SPACES.
       01  WS-T2                         PIC X(128) VALUE SPACES.
       01  WS-T3                         PIC X(128) VALUE SPACES.
       01  WS-T4                         PIC X(128) VALUE SPACES.
       01  WS-REST                       PIC X(1024) VALUE SPACES.
       01  WS-REST-LEN                   PIC 9(4) VALUE 0.
       01  WS-LAST-PIPE                  PIC 9(4) VALUE 0.


             *> Message for account creation
       01 MSG-ACCOUNT-LIMIT             PIC X(80) VALUE "All permitted accounts have been created, please come back later.".
       01 MSG-USERNAME-EXISTS           PIC X(64) VALUE "Username already exists. Please try a different one.".
       01 MSG-ENTER-NEW-USER            PIC X(64) VALUE "Please enter your username:".
       01 MSG-ENTER-NEW-PASS            PIC X(64) VALUE "Please enter your password:".
       01 MSG-ACCOUNT-SUCCESS           PIC X(64) VALUE "Account created successfully.".

       01  WS-LOGGED-CHOICE              PIC X(8) VALUE SPACES.
       01  WS-SKILL-CHOICE               PIC X(8) VALUE SPACES.

      *> MODIFIED: New menu messages to match sample output
       01 MSG-MENU-VIEW-PROFILE         PIC X(32) VALUE "1. View My Profile".
       01 MSG-MENU-SEARCH-USER-NEW      PIC X(32) VALUE "2. Search for User".
       01 MSG-MENU-LEARN-SKILL          PIC X(32) VALUE "3. Learn a New Skill".
       01 MSG-MENU-VIEW-PENDING         PIC X(48) VALUE "4. View My Pending Connection Requests".
       01  MSG-ENTER-CHOICE2             PIC X(20) VALUE "Enter your choice: ".


       01  MSG-SKILL1                    PIC X(32) VALUE "Skill 1".
       01  MSG-SKILL2                    PIC X(32) VALUE "Skill 2".
       01  MSG-SKILL3                    PIC X(32) VALUE "Skill 3".
       01  MSG-SKILL4                    PIC X(32) VALUE "Skill 4".
       01  MSG-SKILL5                    PIC X(32) VALUE "Skill 5".
       01  MSG-SKILL6                    PIC X(32) VALUE "Go Back".
       01  MSG-ENTER-SKILL               PIC X(19) VALUE "Enter your choice: ".
       01  MSG-SKILL-UNDER               PIC X(64) VALUE "This skill is under construction.".

       01  MSG-EDIT-HEADER               PIC X(32) VALUE "--- Create/Edit Profile ---".
       01  MSG-VIEW-HEADER               PIC X(32) VALUE "--- Your Profile ---".
       01  MSG-LINE                      PIC X(20) VALUE "--------------------".
       01  MSG-LINE-LONG                 PIC X(25) VALUE "-------------------------".
       01  MSG-END-OF-PROGRAM            PIC X(32) VALUE "--- END_OF_PROGRAM_EXECUTION ---".

       01  MSG-ENTER-FIRST               PIC X(32) VALUE "Enter First Name:".
       01  MSG-ENTER-LAST                PIC X(32) VALUE "Enter Last Name:".
       01  MSG-ENTER-UNIV                PIC X(48) VALUE "Enter University/College Attended:".
       01  MSG-ENTER-MAJOR               PIC X(32) VALUE "Enter Major:".
       01  MSG-ENTER-GYEAR2              PIC X(32) VALUE "Enter Graduation Year (YYYY):".

       01  MSG-REQUIRED                  PIC X(64) VALUE "This field is required. Please try again.".
       01  MSG-YEAR-INVALID              PIC X(80) VALUE "Graduation year must be 1900-2100 and 4 digits.".
       01  MSG-PROFILE-SAVED-OK          PIC X(64) VALUE "Profile saved successfully!".
       01  MSG-PROFILE-NOT-FOUND         PIC X(64) VALUE "No profile found. Please create your profile first.".


       *> Message for ABOUT ME
       01  MSG-ABOUT-ME                  PIC X(80) VALUE "Enter About Me (optional, max 200 chars, enter blank line to skip):".
       01  WS-ABOUT-ME                   PIC X(200).
       *> Experiences
       01  MSG-ADD-EXP                   PIC X(90) VALUE "Add Experiences (optional, max 3 entries. Enter 'DONE' to finish):".
       01  WS-EXP-CHOICE                 PIC X(20).
       01  WS-EXPERIENCE.
           05 WS-EXP-COUNT               PIC 9.
           05 WS-EXP-ENTRY OCCURS 3 TIMES.
               10 WS-EXP-TITLE           PIC X(50).
               10 WS-EXP-COMPANY         PIC X(50).
               10 WS-EXP-DATES           PIC X(50).
               10 WS-EXP-DESC            PIC X(100).
       01  WS-TITLE-INPUT                PIC X(50).
       01  WS-COMPANY-INPUT              PIC X(50).
       01  WS-DATES-INPUT                PIC X(50).
       01  WS-DESC-INPUT                 PIC X(100).

       *> Education
       01  MSG-ADD-EDUCATION             PIC X(90) VALUE "Add Education (optional, max 3 entries. Enter 'DONE' to finish):".
       01  WS-EDU-CHOICE                 PIC X(20).
       01  WS-EDUCATION.
           05 WS-EDU-COUNT               PIC 9.
           05 WS-EDU-ENTRY OCCURS 3 TIMES.
               10 WS-EDU-DEGREE          PIC X(50).
               10 WS-EDU-SCHOOL          PIC X(50).
               10 WS-EDU-YEARS           PIC X(20).
       01  WS-DEGREE-INPUT               PIC X(50).
       01  WS-SCHOOL-INPUT               PIC X(50).
       01  WS-YEARS-INPUT                PIC X(20).

       *> Search user
       01  MSG-ENTER-USER-SEARCH         PIC X(64) VALUE "Enter the full name of the person you are looking for:".
       01  MSG-USER-NOT-FOUND            PIC X(64) VALUE "No one by that name could be found.".
       01  MSG-USER-PROFILE-HEADER       PIC X(32) VALUE "--- Found User Profile ---".
       01  WS-SEARCH-FULLNAME            PIC X(128) VALUE SPACES.
       01  WS-SEARCH-FOUND               PIC X VALUE 'N'.
           88  SEARCH-FOUND                      VALUE 'Y'.
           88  SEARCH-NOT-FOUND                  VALUE 'N'.

       *> Connection Request variables
       01  WS-CONN-CHOICE                PIC X(8)   VALUE SPACES.
       01  WS-FOUND-USER-USERNAME        PIC X(128) VALUE SPACES.
       01  WS-CONNECTION-STATUS-FLAG     PIC X(2)   VALUE SPACES.
           88 CONN-OK                            VALUE "OK".
           88 CONN-ALREADY-ACCEPTED              VALUE "AC".
           88 CONN-PENDING-BY-ME                 VALUE "P1".
           88 CONN-PENDING-BY-THEM               VALUE "P2".

       01  MSG-SEND-REQUEST              PIC X(32)  VALUE "1. Send Connection Request".
       01  MSG-BACK-TO-MENU              PIC X(32)  VALUE "2. Back to Main Menu".
       01  MSG-ALREADY-CONNECTED         PIC X(64)  VALUE "You are already connected with this user.".
       01  MSG-PENDING-REQUEST-EXISTS    PIC X(80)  VALUE "You have already sent a pending connection request to this user.".
       01  MSG-THEY-SENT-REQUEST         PIC X(80)  VALUE "This user has already sent you a connection request.".

      *> New messages for pending requests view
       01 MSG-PENDING-HEADER            PIC X(64) VALUE "--- Pending Connection Requests ---".
       01 MSG-NO-PENDING-REQUESTS       PIC X(64) VALUE "You have no pending connection requests at this time.".
       01 MSG-PENDING-LINE              PIC X(35) VALUE "-----------------------------------".


       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM INIT-FILES
           PERFORM RUN-APP
           MOVE MSG-END-OF-PROGRAM TO WS-MSG PERFORM DISPLAY-AND-LOG
           PERFORM CLOSE-FILES
           GOBACK.

       INITIALIZATION-SECTION.
       INIT-FILES.
           *> File-driven only: open input and output files; create/overwrite output.
           OPEN INPUT  INPUT-FILE
                OUTPUT OUTPUT-FILE.

           *> Load users from file into memory (optional if file missing)
           PERFORM INIT-LOAD-ACCOUNTS
           PERFORM INIT-LOAD-PROFILES
      *> New: Load connections
           PERFORM INIT-LOAD-CONNECTIONS

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

           PERFORM READ-NEXT-LINE
           MOVE WS-LINE TO WS-CHOICE
           IF EOF-IN
              EXIT PARAGRAPH
           END-IF

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
             PERFORM READ-NEXT-LINE
             MOVE WS-LINE TO WS-USERNAME
             IF EOF-IN
                EXIT PERFORM
             END-IF
             MOVE MSG-ENTER-PASS TO WS-MSG PERFORM DISPLAY-AND-LOG

             PERFORM READ-NEXT-LINE
             MOVE WS-LINE TO WS-PASSWORD

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
                       FUNCTION TRIM(WS-USERNAME) DELIMITED BY SIZE
                       "!"                      DELIMITED BY SIZE
                       INTO WS-MSG
                END-STRING
                PERFORM DISPLAY-AND-LOG
                MOVE FUNCTION TRIM(WS-USERNAME) TO WS-CURRENT-USERNAME
                PERFORM LOGGED-IN-MENU
                EXIT PERFORM
             ELSE
                MOVE MSG-FAILURE TO WS-MSG
                PERFORM DISPLAY-AND-LOG
                *> Continue asking for credentials
             END-IF
           END-PERFORM
           EXIT.

       CREATE-ACCOUNT.
           *>Check max account
           IF WS-USERS-COUNT >= 5
               MOVE MSG-ACCOUNT-LIMIT TO WS-MSG PERFORM DISPLAY-AND-LOG
               EXIT PARAGRAPH
           END-IF

           *> Prompt for new username
           PERFORM UNTIL WS-NEW-USERNAME NOT = SPACES AND MATCH-NOT-FOUND OR EOF-IN
               MOVE MSG-ENTER-NEW-USER TO WS-MSG PERFORM DISPLAY-AND-LOG

               PERFORM READ-NEXT-LINE
               MOVE WS-LINE TO WS-NEW-USERNAME

               IF EOF-IN
                   EXIT PARAGRAPH
               END-IF

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
           SET PASS-INVALID TO TRUE
           MOVE SPACES TO WS-NEW-PASSWORD
           PERFORM UNTIL PASS-VALID OR EOF-IN
               MOVE MSG-ENTER-NEW-PASS TO WS-MSG PERFORM DISPLAY-AND-LOG

               PERFORM READ-NEXT-LINE
               MOVE WS-LINE TO WS-NEW-PASSWORD

               IF EOF-IN
                   EXIT PARAGRAPH
               END-IF

               PERFORM VALIDATE-PASSWORD

               IF PASS-INVALID
                   MOVE WS-PASSWORD-ERROR TO WS-MSG PERFORM DISPLAY-AND-LOG
               END-IF
           END-PERFORM

           IF EOF-IN
               EXIT PARAGRAPH
           END-IF

           *> SAVE NEW ACCOUNT - only if we have a valid password
           IF WS-NEW-PASSWORD = SPACES
               EXIT PARAGRAPH
           END-IF

           ADD 1 TO WS-USERS-COUNT
           MOVE WS-NEW-USERNAME TO WS-TBL-USERNAME(WS-USERS-COUNT)
           MOVE WS-NEW-PASSWORD TO WS-TBL-PASSWORD(WS-USERS-COUNT)

           *> UPDATE USERS.TXT
           OPEN EXTEND USERS-FILE
           MOVE SPACES TO USER-REC
           STRING FUNCTION TRIM(WS-NEW-USERNAME) DELIMITED BY SIZE
                  "|" DELIMITED BY SIZE
                  FUNCTION TRIM(WS-NEW-PASSWORD) DELIMITED BY SIZE
                  INTO USER-REC
           END-STRING
           WRITE USER-REC
           CLOSE USERS-FILE

           MOVE MSG-ACCOUNT-SUCCESS TO WS-MSG PERFORM DISPLAY-AND-LOG
           EXIT.

       VALIDATE-PASSWORD.
           SET PASS-VALID TO TRUE
           MOVE SPACES TO WS-PASSWORD-ERROR
           MOVE 0 TO WS-UPPER-COUNT WS-DIGIT-COUNT WS-SPECIAL-COUNT

           *> Check length
           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-NEW-PASSWORD)) TO WS-PASS-LEN
           IF WS-PASS-LEN < 8 OR WS-PASS-LEN > 12
               SET PASS-INVALID TO TRUE
               MOVE "Password must be 8-12 characters." TO WS-PASSWORD-ERROR
           END-IF

           *> Character category checks (scan once)
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > FUNCTION LENGTH(FUNCTION TRIM(WS-NEW-PASSWORD))
               MOVE WS-NEW-PASSWORD(WS-I:1) TO WS-CHAR
               IF WS-CHAR >= 'A' AND WS-CHAR <= 'Z'
                   ADD 1 TO WS-UPPER-COUNT
               END-IF
               IF WS-CHAR >= '0' AND WS-CHAR <= '9'
                   ADD 1 TO WS-DIGIT-COUNT
               END-IF
               MOVE 0 TO WS-TMP-COUNT
               INSPECT WS-SPECIAL-CHARS TALLYING WS-TMP-COUNT FOR ALL WS-CHAR
               IF WS-TMP-COUNT > 0
                   ADD 1 TO WS-SPECIAL-COUNT
               END-IF
           END-PERFORM

           IF WS-UPPER-COUNT = 0
               SET PASS-INVALID TO TRUE
               MOVE "Password must contain at least one uppercase letter." TO WS-PASSWORD-ERROR
           END-IF
           IF WS-DIGIT-COUNT = 0
               SET PASS-INVALID TO TRUE
               MOVE "Password must contain at least one digit." TO WS-PASSWORD-ERROR
           END-IF
           IF WS-SPECIAL-COUNT = 0
               SET PASS-INVALID TO TRUE
               MOVE "Password must contain at least one special character (!@#$%^&*?-_+)." TO WS-PASSWORD-ERROR
           END-IF
           EXIT.

       LOGGED-IN-SECTION.
       LOGGED-IN-MENU.
           PERFORM UNTIL EOF-IN
      *> MODIFIED: Display the new menu from the sample output
               MOVE MSG-MENU-VIEW-PROFILE  TO WS-MSG PERFORM DISPLAY-AND-LOG
               MOVE MSG-MENU-SEARCH-USER-NEW TO WS-MSG PERFORM DISPLAY-AND-LOG
               MOVE MSG-MENU-LEARN-SKILL   TO WS-MSG PERFORM DISPLAY-AND-LOG
               MOVE MSG-MENU-VIEW-PENDING  TO WS-MSG PERFORM DISPLAY-AND-LOG
               MOVE MSG-ENTER-CHOICE2      TO WS-MSG PERFORM DISPLAY-AND-LOG

               PERFORM READ-NEXT-LINE
               MOVE WS-LINE TO WS-LOGGED-CHOICE

               IF EOF-IN
                   EXIT PERFORM
               END-IF

      *> MODIFIED: Evaluate choices based on the new menu
               EVALUATE WS-LOGGED-CHOICE
                   WHEN '1'
                       PERFORM VIEW-MY-PROFILE
                   WHEN '2'
                       PERFORM USER-SEARCH-MENU
                   WHEN '3'
                       PERFORM SKILL-MENU
                   WHEN '4'
                       PERFORM VIEW-PENDING-REQUESTS
                   WHEN OTHER
                       MOVE MSG-INVALID-CHOICE TO WS-MSG PERFORM DISPLAY-AND-LOG
               END-EVALUATE
           END-PERFORM
           EXIT.

       SKILL-MENU.
           PERFORM UNTIL WS-SKILL-CHOICE = '6' OR EOF-IN
               MOVE "Learn a new skill:" TO WS-MSG PERFORM DISPLAY-AND-LOG
               MOVE MSG-SKILL1 TO WS-MSG PERFORM DISPLAY-AND-LOG
               MOVE MSG-SKILL2 TO WS-MSG PERFORM DISPLAY-AND-LOG
               MOVE MSG-SKILL3 TO WS-MSG PERFORM DISPLAY-AND-LOG
               MOVE MSG-SKILL4 TO WS-MSG PERFORM DISPLAY-AND-LOG
               MOVE MSG-SKILL5 TO WS-MSG PERFORM DISPLAY-AND-LOG
               MOVE MSG-SKILL6 TO WS-MSG PERFORM DISPLAY-AND-LOG
               MOVE MSG-ENTER-SKILL TO WS-MSG PERFORM DISPLAY-AND-LOG

               PERFORM READ-NEXT-LINE
               MOVE WS-LINE TO WS-SKILL-CHOICE

               IF EOF-IN
                   EXIT PERFORM
               END-IF

               EVALUATE WS-SKILL-CHOICE
                   WHEN '1' THRU '5'
                       MOVE MSG-SKILL-UNDER TO WS-MSG PERFORM DISPLAY-AND-LOG
                   WHEN '6'
                       EXIT PERFORM
                   WHEN OTHER
                       MOVE MSG-INVALID-CHOICE TO WS-MSG PERFORM DISPLAY-AND-LOG
               END-EVALUATE
           END-PERFORM
           EXIT.

       USER-SEARCH-MENU.
           MOVE MSG-ENTER-USER-SEARCH TO WS-MSG
           PERFORM DISPLAY-AND-LOG

           PERFORM READ-NEXT-LINE
           MOVE WS-LINE TO WS-SEARCH-FULLNAME

           IF EOF-IN
               EXIT PARAGRAPH
           END-IF

           PERFORM FIND-USER-BY-NAME
           IF SEARCH-FOUND
               PERFORM DISPLAY-FOUND-USER
           ELSE
               PERFORM DISPLAY-NO-MATCH-MSG
           END-IF
           EXIT.
       FIND-USER-BY-NAME.
           MOVE 0 TO WS-SEARCH-RESULT-IDX
           SET SEARCH-NOT-FOUND TO TRUE
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-PROFILES-COUNT OR SEARCH-FOUND
               MOVE SPACES TO WS-T1
               STRING FUNCTION TRIM(WS-PROF-FIRST(WS-I)) DELIMITED BY SIZE
                      " " DELIMITED BY SIZE
                      FUNCTION TRIM(WS-PROF-LAST(WS-I)) DELIMITED BY SIZE
                      INTO WS-T1
               END-STRING
               IF WS-T1 = FUNCTION TRIM(WS-SEARCH-FULLNAME)
                   SET SEARCH-FOUND TO TRUE
                   MOVE WS-I TO WS-SEARCH-RESULT-IDX
               END-IF
           END-PERFORM
           EXIT.

       DISPLAY-FOUND-USER.
           IF WS-SEARCH-RESULT-IDX = 0
               EXIT PARAGRAPH
           END-IF
           MOVE WS-SEARCH-RESULT-IDX TO WS-I
           PERFORM DISPLAY-PROFILE-BY-ID

      *> After displaying profile, ask to connect if not self.
           MOVE WS-PROF-USERNAME(WS-SEARCH-RESULT-IDX) TO WS-FOUND-USER-USERNAME
           IF WS-FOUND-USER-USERNAME NOT = WS-CURRENT-USERNAME AND NOT EOF-IN
               PERFORM PROMPT-FOR-CONNECTION
           END-IF
           EXIT.

       DISPLAY-PROFILE-BY-ID.
           IF WS-I < 1 OR WS-I > WS-PROFILES-COUNT
               MOVE "Invalid profile ID." TO WS-MSG PERFORM DISPLAY-AND-LOG
               EXIT PARAGRAPH
           END-IF

           *> Load profile data into shared buffers for consistent formatting
           MOVE FUNCTION TRIM(WS-PROF-FIRST(WS-I))       TO WS-PROF-FIRST-IN
           MOVE FUNCTION TRIM(WS-PROF-LAST(WS-I))        TO WS-PROF-LAST-IN
           MOVE FUNCTION TRIM(WS-PROF-UNIV(WS-I))        TO WS-PROF-UNIV-IN
           MOVE FUNCTION TRIM(WS-PROF-MAJOR(WS-I))       TO WS-PROF-MAJOR-IN
           MOVE FUNCTION TRIM(WS-PROF-GYEAR(WS-I))       TO WS-PROF-GYEAR-IN
           MOVE FUNCTION TRIM(WS-PROF-ABOUT(WS-I))       TO WS-PROF-ABOUT-IN
           MOVE FUNCTION TRIM(WS-PROF-EXPERIENCES(WS-I)) TO WS-EXPS-STR
           MOVE FUNCTION TRIM(WS-PROF-EDUCATIONS(WS-I))  TO WS-EDUS-STR

           MOVE MSG-USER-PROFILE-HEADER TO WS-MSG PERFORM DISPLAY-AND-LOG

           MOVE SPACES TO WS-MSG
           STRING "Name: " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-PROF-FIRST-IN) DELIMITED BY SIZE
                  " " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-PROF-LAST-IN) DELIMITED BY SIZE
                  INTO WS-MSG
           END-STRING
           PERFORM DISPLAY-AND-LOG

           MOVE SPACES TO WS-MSG
           STRING "University: " FUNCTION TRIM(WS-PROF-UNIV-IN) DELIMITED BY SIZE INTO WS-MSG END-STRING
           PERFORM DISPLAY-AND-LOG

           MOVE SPACES TO WS-MSG
           STRING "Major: " FUNCTION TRIM(WS-PROF-MAJOR-IN) DELIMITED BY SIZE INTO WS-MSG END-STRING
           PERFORM DISPLAY-AND-LOG

           MOVE SPACES TO WS-MSG
           STRING "Graduation Year: " FUNCTION TRIM(WS-PROF-GYEAR-IN) DELIMITED BY SIZE INTO WS-MSG END-STRING
           PERFORM DISPLAY-AND-LOG

           MOVE SPACES TO WS-MSG
           STRING "About Me: " FUNCTION TRIM(WS-PROF-ABOUT-IN) DELIMITED BY SIZE INTO WS-MSG END-STRING
           PERFORM DISPLAY-AND-LOG

           PERFORM DISPLAY-EXPERIENCES

           PERFORM DISPLAY-EDUCATION

           MOVE MSG-LINE-LONG TO WS-MSG PERFORM DISPLAY-AND-LOG
           EXIT PARAGRAPH.

       DISPLAY-NO-MATCH-MSG.
           MOVE MSG-USER-NOT-FOUND TO WS-MSG
           PERFORM DISPLAY-AND-LOG
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
           *> Prefer real users.txt; if missing OR empty, fall back to users.examples.txt
           OPEN INPUT USERS-FILE
           IF WS-USR-STATUS = "00"
              PERFORM LOAD-ACCOUNTS-FROM-USERS
              CLOSE USERS-FILE
           END-IF

           *> If nothing loaded, try examples
           IF WS-USERS-COUNT = 0
             OPEN INPUT USERS-EXAMPLE-FILE
             IF WS-UEX-STATUS = "00"
                PERFORM LOAD-ACCOUNTS-FROM-EXAMPLE
                CLOSE USERS-EXAMPLE-FILE
             END-IF
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

        PROFILE-IO-SECTION.
        INIT-LOAD-PROFILES.
            OPEN INPUT PROFILES-FILE
            IF WS-PROF-STATUS = "00"
              SET NOT-EOF-PROF TO TRUE
              PERFORM UNTIL EOF-PROF
                  READ PROFILES-FILE
                      AT END SET EOF-PROF TO TRUE
                      NOT AT END PERFORM PARSE-PROFILE-REC
                  END-READ
              END-PERFORM
              CLOSE PROFILES-FILE
            END-IF
            EXIT.

       PARSE-PROFILE-REC.
           *> Format: username|first|last|univ|major|gyear|about|experiences|educations
           MOVE 1 TO WS-J
           UNSTRING PROFILE-REC DELIMITED BY '|'
               INTO WS-PROF-USER
                    WS-PROF-FIRST-IN
                    WS-PROF-LAST-IN
                    WS-PROF-UNIV-IN
                    WS-PROF-MAJOR-IN
                    WS-PROF-GYEAR-IN
                    WS-PROF-ABOUT-IN
               WITH POINTER WS-J
           END-UNSTRING

           MOVE FUNCTION TRIM(PROFILE-REC(WS-J:)) TO WS-REST
           MOVE SPACES TO WS-EXPS-STR WS-EDUS-STR

           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-REST)) TO WS-REST-LEN
           IF WS-REST-LEN > 0
             MOVE 0 TO WS-LAST-PIPE
             PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-REST-LEN
                 IF WS-REST(WS-I:1) = "|"
                    MOVE WS-I TO WS-LAST-PIPE
                 END-IF
             END-PERFORM
             IF WS-LAST-PIPE = 0
                MOVE FUNCTION TRIM(WS-REST) TO WS-EXPS-STR
             ELSE
                IF WS-LAST-PIPE > 1
                   MOVE FUNCTION TRIM(WS-REST(1:WS-LAST-PIPE - 1)) TO WS-EXPS-STR
                END-IF
                MOVE FUNCTION TRIM(WS-REST(WS-LAST-PIPE + 1:)) TO WS-EDUS-STR
             END-IF
           END-IF

           IF WS-PROF-USER = SPACES
              EXIT PARAGRAPH
           END-IF

           IF WS-PROFILES-COUNT < WS-PROFILES-MAX
             ADD 1 TO WS-PROFILES-COUNT
             MOVE FUNCTION TRIM(WS-PROF-USER)       TO WS-PROF-USERNAME(WS-PROFILES-COUNT)
             MOVE FUNCTION TRIM(WS-PROF-FIRST-IN)   TO WS-PROF-FIRST(WS-PROFILES-COUNT)
             MOVE FUNCTION TRIM(WS-PROF-LAST-IN)    TO WS-PROF-LAST(WS-PROFILES-COUNT)
             MOVE FUNCTION TRIM(WS-PROF-UNIV-IN)    TO WS-PROF-UNIV(WS-PROFILES-COUNT)
             MOVE FUNCTION TRIM(WS-PROF-MAJOR-IN)   TO WS-PROF-MAJOR(WS-PROFILES-COUNT)
             MOVE FUNCTION TRIM(WS-PROF-GYEAR-IN)   TO WS-PROF-GYEAR(WS-PROFILES-COUNT)
             MOVE FUNCTION TRIM(WS-PROF-ABOUT-IN)   TO WS-PROF-ABOUT(WS-PROFILES-COUNT)
             MOVE FUNCTION TRIM(WS-EXPS-STR)        TO WS-PROF-EXPERIENCES(WS-PROFILES-COUNT)
             MOVE FUNCTION TRIM(WS-EDUS-STR)        TO WS-PROF-EDUCATIONS(WS-PROFILES-COUNT)
           END-IF
           EXIT.


                 SAVE-PROFILES.
                     OPEN OUTPUT PROFILES-FILE
                     PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-PROFILES-COUNT
                         MOVE SPACES TO PROFILE-REC
                         STRING FUNCTION TRIM(WS-PROF-USERNAME(WS-I)) DELIMITED BY SIZE
        "|" DELIMITED BY SIZE
        FUNCTION TRIM(WS-PROF-FIRST(WS-I))     DELIMITED BY SIZE
        "|" DELIMITED BY SIZE
        FUNCTION TRIM(WS-PROF-LAST(WS-I))      DELIMITED BY SIZE
        "|" DELIMITED BY SIZE
        FUNCTION TRIM(WS-PROF-UNIV(WS-I))      DELIMITED BY SIZE
        "|" DELIMITED BY SIZE
        FUNCTION TRIM(WS-PROF-MAJOR(WS-I))     DELIMITED BY SIZE
        "|" DELIMITED BY SIZE
        FUNCTION TRIM(WS-PROF-GYEAR(WS-I))     DELIMITED BY SIZE
        "|" DELIMITED BY SIZE
        FUNCTION TRIM(WS-PROF-ABOUT(WS-I))     DELIMITED BY SIZE
        "|" DELIMITED BY SIZE
        FUNCTION TRIM(WS-PROF-EXPERIENCES(WS-I))     DELIMITED BY SIZE
        "|" DELIMITED BY SIZE
        FUNCTION TRIM(WS-PROF-EDUCATIONS(WS-I))      DELIMITED BY SIZE
        INTO PROFILE-REC
            END-STRING

                      WRITE PROFILE-REC
            END-PERFORM
            CLOSE PROFILES-FILE
            EXIT.


        FIND-PROFILE-BY-USERNAME.
            SET PROFILE-NOT-FOUND TO TRUE
            MOVE 0 TO WS-PROFILE-IDX
            IF WS-PROFILES-COUNT = 0
              EXIT PARAGRAPH
            END-IF
            PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-PROFILES-COUNT OR PROFILE-FOUND
              IF FUNCTION TRIM(WS-CURRENT-USERNAME) = FUNCTION TRIM(WS-PROF-USERNAME(WS-I))
                 SET PROFILE-FOUND TO TRUE
                 MOVE WS-I TO WS-PROFILE-IDX
              END-IF
            END-PERFORM
            EXIT.

       VALIDATE-GRAD-YEAR.
           MOVE FUNCTION TRIM(WS-PROF-GYEAR-IN) TO WS-PROF-GYEAR-IN
           SET YEAR-VALID TO TRUE
           IF FUNCTION LENGTH(WS-PROF-GYEAR-IN) NOT = 4
             SET YEAR-INVALID TO TRUE
             EXIT PARAGRAPH
           END-IF
           SET YEAR-VALID TO TRUE
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4 OR YEAR-INVALID
             MOVE WS-PROF-GYEAR-IN(WS-I:1) TO WS-CHAR
             IF WS-CHAR < '0' OR WS-CHAR > '9'
                SET YEAR-INVALID TO TRUE
             END-IF
           END-PERFORM
           IF YEAR-INVALID
              EXIT PARAGRAPH
           END-IF
           MOVE WS-PROF-GYEAR-IN TO WS-GYEAR-NUM
           IF WS-GYEAR-NUM < 1900 OR WS-GYEAR-NUM > 2100
             SET YEAR-INVALID TO TRUE
           END-IF
           EXIT.

      *> ===============================================================
      *> CONNECTION HANDLING SECTION
      *> ===============================================================
       CONNECTION-HANDLING-SECTION.
       PROMPT-FOR-CONNECTION.
           MOVE MSG-SEND-REQUEST TO WS-MSG PERFORM DISPLAY-AND-LOG
      *> MODIFIED: Use new message for clarity
           MOVE MSG-BACK-TO-MENU TO WS-MSG PERFORM DISPLAY-AND-LOG
           MOVE MSG-ENTER-CHOICE TO WS-MSG PERFORM DISPLAY-AND-LOG

           PERFORM READ-NEXT-LINE
           MOVE WS-LINE TO WS-CONN-CHOICE

           IF EOF-IN EXIT PARAGRAPH END-IF

           EVALUATE WS-CONN-CHOICE
               WHEN '1'
                   PERFORM PROCESS-CONNECTION-REQUEST
               WHEN '2'
                   CONTINUE
               WHEN OTHER
                   MOVE MSG-INVALID-CHOICE TO WS-MSG PERFORM DISPLAY-AND-LOG
           END-EVALUATE
           EXIT.

       PROCESS-CONNECTION-REQUEST.
           PERFORM CHECK-CONNECTION-STATUS
           EVALUATE TRUE
               WHEN CONN-ALREADY-ACCEPTED
                   MOVE MSG-ALREADY-CONNECTED TO WS-MSG
                   PERFORM DISPLAY-AND-LOG
               WHEN CONN-PENDING-BY-ME
                   MOVE MSG-PENDING-REQUEST-EXISTS TO WS-MSG
                   PERFORM DISPLAY-AND-LOG
               WHEN CONN-PENDING-BY-THEM
                   MOVE MSG-THEY-SENT-REQUEST TO WS-MSG
                   PERFORM DISPLAY-AND-LOG
               WHEN CONN-OK
                   PERFORM ADD-NEW-CONNECTION
                   PERFORM SAVE-CONNECTIONS
      *> MODIFIED: Create personalized confirmation message
                   MOVE WS-SEARCH-RESULT-IDX to WS-I
                   STRING "Connection request sent to " DELIMITED BY SIZE
                          FUNCTION TRIM(WS-PROF-FIRST(WS-I)) DELIMITED BY SIZE
                          " " DELIMITED BY SIZE
                          FUNCTION TRIM(WS-PROF-LAST(WS-I)) DELIMITED BY SIZE
                          "." DELIMITED BY SIZE
                          INTO WS-MSG
                   END-STRING
                   PERFORM DISPLAY-AND-LOG
           END-EVALUATE
           EXIT.

       CHECK-CONNECTION-STATUS.
      *> Checks relationship between WS-CURRENT-USERNAME and WS-FOUND-USER-USERNAME
           SET CONN-OK TO TRUE
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-CONNECTIONS-COUNT
               *> Check for ME -> THEM
               IF WS-CONN-SENDER(WS-I)   = WS-CURRENT-USERNAME AND
                  WS-CONN-RECEIVER(WS-I) = WS-FOUND-USER-USERNAME
                   IF WS-CONN-STATUS(WS-I) = 'A'
                       SET CONN-ALREADY-ACCEPTED TO TRUE
                   ELSE
                       SET CONN-PENDING-BY-ME TO TRUE
                   END-IF
                   EXIT PERFORM
               END-IF
               *> Check for THEM -> ME
               IF WS-CONN-SENDER(WS-I)   = WS-FOUND-USER-USERNAME AND
                  WS-CONN-RECEIVER(WS-I) = WS-CURRENT-USERNAME
                   IF WS-CONN-STATUS(WS-I) = 'A'
                       SET CONN-ALREADY-ACCEPTED TO TRUE
                   ELSE
                       SET CONN-PENDING-BY-THEM TO TRUE
                   END-IF
                   EXIT PERFORM
               END-IF
           END-PERFORM
           EXIT.

       ADD-NEW-CONNECTION.
           ADD 1 TO WS-CONNECTIONS-COUNT
           MOVE WS-CURRENT-USERNAME   TO WS-CONN-SENDER(WS-CONNECTIONS-COUNT)
           MOVE WS-FOUND-USER-USERNAME TO WS-CONN-RECEIVER(WS-CONNECTIONS-COUNT)
           MOVE 'P'                   TO WS-CONN-STATUS(WS-CONNECTIONS-COUNT)
           EXIT.

      *> NEW PARAGRAPH: To handle menu option 4
       VIEW-PENDING-REQUESTS.
           MOVE MSG-PENDING-HEADER TO WS-MSG PERFORM DISPLAY-AND-LOG
           MOVE 0 TO WS-TMP-COUNT

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-CONNECTIONS-COUNT
               IF WS-CONN-RECEIVER(WS-I) = WS-CURRENT-USERNAME AND
                  WS-CONN-STATUS(WS-I) = 'P'
                   MOVE WS-CONN-SENDER(WS-I) TO WS-MSG
                   PERFORM DISPLAY-AND-LOG
                   ADD 1 TO WS-TMP-COUNT
               END-IF
           END-PERFORM

           IF WS-TMP-COUNT = 0
               MOVE MSG-NO-PENDING-REQUESTS TO WS-MSG
               PERFORM DISPLAY-AND-LOG
           END-IF

           MOVE MSG-PENDING-LINE TO WS-MSG PERFORM DISPLAY-AND-LOG
           EXIT.

       INIT-LOAD-CONNECTIONS.
           OPEN INPUT CONNECTIONS-FILE
           IF WS-CONN-FILE-STATUS = "00"
               SET NOT-EOF-CONN TO TRUE
               PERFORM UNTIL EOF-CONN
                   READ CONNECTIONS-FILE
                       AT END SET EOF-CONN TO TRUE
                       NOT AT END PERFORM PARSE-CONNECTION-REC
                   END-READ
               END-PERFORM
               CLOSE CONNECTIONS-FILE
           END-IF
           EXIT.

       PARSE-CONNECTION-REC.
           INITIALIZE WS-T1 WS-T2 WS-T3
           UNSTRING CONNECTION-REC DELIMITED BY '|'
               INTO WS-T1 WS-T2 WS-T3
           END-UNSTRING

           IF WS-T1 NOT = SPACES AND WS-CONNECTIONS-COUNT < WS-CONNECTIONS-MAX
               ADD 1 TO WS-CONNECTIONS-COUNT
               MOVE FUNCTION TRIM(WS-T1) TO WS-CONN-SENDER(WS-CONNECTIONS-COUNT)
               MOVE FUNCTION TRIM(WS-T2) TO WS-CONN-RECEIVER(WS-CONNECTIONS-COUNT)
               MOVE FUNCTION TRIM(WS-T3) TO WS-CONN-STATUS(WS-CONNECTIONS-COUNT)
           END-IF
           EXIT.

       SAVE-CONNECTIONS.
           OPEN OUTPUT CONNECTIONS-FILE
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-CONNECTIONS-COUNT
               MOVE SPACES TO CONNECTION-REC
               STRING
                   FUNCTION TRIM(WS-CONN-SENDER(WS-I))   DELIMITED BY SIZE
                   "|" DELIMITED BY SIZE
                   FUNCTION TRIM(WS-CONN-RECEIVER(WS-I)) DELIMITED BY SIZE
                   "|" DELIMITED BY SIZE
                   FUNCTION TRIM(WS-CONN-STATUS(WS-I))   DELIMITED BY SIZE
                   INTO CONNECTION-REC
               END-STRING
               WRITE CONNECTION-REC
           END-PERFORM
           CLOSE CONNECTIONS-FILE
           EXIT.

       SERIALIZATION-SECTION.
       SERIALIZE-EXPERIENCE.
       *> Convert to WS-EXPERIENCE table into a single string for saving.
       *> Format: Title~Company~Dates~Desc|Title~Company~Dates~Desc
           INITIALIZE WS-EXPS-STR
           MOVE 1 to WS-J
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-EXP-COUNT
               IF WS-I > 1
                   STRING "^" DELIMITED BY SIZE
                       INTO WS-EXPS-STR
                       WITH POINTER WS-J
                   END-STRING
               END-IF

               STRING FUNCTION TRIM(WS-EXP-TITLE(WS-I)) DELIMITED BY SIZE
                      "~" DELIMITED BY SIZE
                      FUNCTION TRIM(WS-EXP-COMPANY(WS-I)) DELIMITED BY SIZE
                      "~" DELIMITED BY SIZE
                      FUNCTION TRIM(WS-EXP-DATES(WS-I)) DELIMITED BY SIZE
                      "~" DELIMITED BY SIZE
                      FUNCTION TRIM(WS-EXP-DESC(WS-I)) DELIMITED BY SIZE
                      INTO WS-EXPS-STR
                      WITH POINTER WS-J
                END-STRING
           END-PERFORM
           EXIT.

       SERIALIZE-EDUCATION.
       *> Converts the WS-EDUCATION table into a single string for saving.
       *> Format: Degree~School~Years|Degree~School~Years
           INITIALIZE WS-EDUS-STR.
           MOVE 1 to WS-J
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-EDU-COUNT
               IF WS-I > 1
                   STRING "^" DELIMITED BY SIZE
                       INTO WS-EDUS-STR
                       WITH POINTER WS-J
                   END-STRING
               END-IF
               STRING FUNCTION TRIM(WS-EDU-DEGREE(WS-I)) DELIMITED BY SIZE
                      "~" DELIMITED BY SIZE
                      FUNCTION TRIM(WS-EDU-SCHOOL(WS-I)) DELIMITED BY SIZE
                      "~" DELIMITED BY SIZE
                      FUNCTION TRIM(WS-EDU-YEARS(WS-I))  DELIMITED BY SIZE
                      INTO WS-EDUS-STR
                      WITH POINTER WS-J
               END-STRING
           END-PERFORM.
           EXIT.

       DISPLAY-EXPERIENCES.
           IF WS-EXPS-STR = SPACES
               MOVE SPACES TO WS-MSG
               STRING "Experience: None" DELIMITED BY SIZE INTO WS-MSG
               END-STRING
               PERFORM DISPLAY-AND-LOG
               EXIT PARAGRAPH
           END-IF

           MOVE SPACES TO WS-MSG
           STRING "Experience:" DELIMITED BY SIZE INTO WS-MSG
           END-STRING
           PERFORM DISPLAY-AND-LOG

           MOVE 1 TO WS-J.
           PERFORM UNTIL WS-J > FUNCTION LENGTH(FUNCTION TRIM(WS-EXPS-STR))
               INITIALIZE WS-ENTRY
               UNSTRING WS-EXPS-STR DELIMITED BY "^"
                   INTO WS-ENTRY
                   WITH POINTER WS-J
               END-UNSTRING

               INITIALIZE WS-T1 WS-T2 WS-T3 WS-T4
               UNSTRING WS-ENTRY DELIMITED BY "~"
                   INTO WS-T1 WS-T2 WS-T3 WS-T4
               END-UNSTRING

               MOVE SPACES TO WS-MSG
               STRING "    Title: " FUNCTION TRIM(WS-T1) INTO WS-MSG
               PERFORM DISPLAY-AND-LOG

               MOVE SPACES TO WS-MSG
               STRING "    Company: " FUNCTION TRIM(WS-T2) INTO WS-MSG
               PERFORM DISPLAY-AND-LOG

               MOVE SPACES TO WS-MSG
               STRING "    Dates: " FUNCTION TRIM(WS-T3) INTO WS-MSG
               PERFORM DISPLAY-AND-LOG

               MOVE SPACES TO WS-MSG
               STRING "    Description: " FUNCTION TRIM(WS-T4) INTO WS-MSG
               PERFORM DISPLAY-AND-LOG
           END-PERFORM.
           EXIT.

       DISPLAY-EDUCATION.
           IF WS-EDUS-STR = SPACES
               MOVE SPACES TO WS-MSG
               STRING "Education: None" DELIMITED BY SIZE INTO WS-MSG
               END-STRING
               PERFORM DISPLAY-AND-LOG
               EXIT PARAGRAPH
           END-IF

           MOVE SPACES TO WS-MSG
           STRING "Education:" DELIMITED BY SIZE INTO WS-MSG
           END-STRING
           PERFORM DISPLAY-AND-LOG

           MOVE 1 TO WS-J.
           PERFORM UNTIL WS-J > FUNCTION LENGTH(FUNCTION TRIM(WS-EDUS-STR))
               INITIALIZE WS-ENTRY
               UNSTRING WS-EDUS-STR DELIMITED BY "^"
                   INTO WS-ENTRY
                   WITH POINTER WS-J
               END-UNSTRING

               INITIALIZE WS-T1 WS-T2 WS-T3
               UNSTRING WS-ENTRY DELIMITED BY "~"
                   INTO WS-T1 WS-T2 WS-T3
               END-UNSTRING

               MOVE SPACES TO WS-MSG
               STRING "    Degree: " FUNCTION TRIM(WS-T1) INTO WS-MSG
               PERFORM DISPLAY-AND-LOG

               MOVE SPACES TO WS-MSG
               STRING "    University: " FUNCTION TRIM(WS-T2) INTO WS-MSG
               PERFORM DISPLAY-AND-LOG

               MOVE SPACES TO WS-MSG
               STRING "    Years: " FUNCTION TRIM(WS-T3) INTO WS-MSG
               PERFORM DISPLAY-AND-LOG
           END-PERFORM.
           EXIT.

       DESERIALIZE-EXPERIENCE.
       *> Converts the saved string back into the WS-EXPERIENCE table.
           MOVE 0 TO WS-EXP-COUNT.
           MOVE WS-PROF-EXPERIENCES(WS-PROFILE-IDX) TO WS-EXPS-STR.
           IF WS-EXPS-STR = SPACES
               EXIT PARAGRAPH
           END-IF.

           MOVE 1 TO WS-J.
           PERFORM UNTIL WS-J > FUNCTION LENGTH(FUNCTION TRIM(WS-EXPS-STR))
               ADD 1 TO WS-EXP-COUNT
               INITIALIZE WS-ENTRY
               UNSTRING WS-EXPS-STR DELIMITED BY "^"
                   INTO WS-ENTRY
                   WITH POINTER WS-J
               END-UNSTRING

               INITIALIZE WS-T1 WS-T2 WS-T3 WS-T4
               UNSTRING WS-ENTRY DELIMITED BY "~"
                   INTO WS-T1 WS-T2 WS-T3 WS-T4
               END-UNSTRING

               MOVE WS-T1 TO WS-EXP-TITLE(WS-EXP-COUNT)
               MOVE WS-T2 TO WS-EXP-COMPANY(WS-EXP-COUNT)
               MOVE WS-T3 TO WS-EXP-DATES(WS-EXP-COUNT)
               MOVE WS-T4 TO WS-EXP-DESC(WS-EXP-COUNT)
           END-PERFORM.
           EXIT.

       DESERIALIZE-EDUCATION.
        *> Converts the saved string back into the WS-EDUCATION table.
           MOVE 0 TO WS-EDU-COUNT.
           MOVE WS-PROF-EDUCATIONS(WS-PROFILE-IDX) TO WS-EDUS-STR.
           IF WS-EDUS-STR = SPACES
               EXIT PARAGRAPH
           END-IF.

           MOVE 1 TO WS-J.
           PERFORM UNTIL WS-J > FUNCTION LENGTH(FUNCTION TRIM(WS-EDUS-STR))
               ADD 1 TO WS-EDU-COUNT
               INITIALIZE WS-ENTRY
               UNSTRING WS-EDUS-STR DELIMITED BY "^"
                   INTO WS-ENTRY
                   WITH POINTER WS-J
               END-UNSTRING

               INITIALIZE WS-T1 WS-T2 WS-T3
               UNSTRING WS-ENTRY DELIMITED BY "~"
                   INTO WS-T1 WS-T2 WS-T3
               END-UNSTRING

               MOVE WS-T1 TO WS-EDU-DEGREE(WS-EDU-COUNT)
               MOVE WS-T2 TO WS-EDU-SCHOOL(WS-EDU-COUNT)
               MOVE WS-T3 TO WS-EDU-YEARS(WS-EDU-COUNT)
           END-PERFORM.
           EXIT.

       PROFILE-SECTION.
           CREATE-OR-EDIT-PROFILE.
           IF FUNCTION TRIM(WS-CURRENT-USERNAME) = SPACES
             MOVE "Internal error: no logged-in user." TO WS-MSG PERFORM DISPLAY-AND-LOG
             EXIT PARAGRAPH
           END-IF

           MOVE MSG-EDIT-HEADER TO WS-MSG PERFORM DISPLAY-AND-LOG

           PERFORM UNTIL FUNCTION TRIM(WS-PROF-FIRST-IN) NOT = SPACES
               MOVE MSG-ENTER-FIRST TO WS-MSG PERFORM DISPLAY-AND-LOG

               PERFORM READ-NEXT-LINE
               MOVE WS-LINE TO WS-PROF-FIRST-IN

               IF EOF-IN EXIT PARAGRAPH END-IF
               IF FUNCTION TRIM(WS-PROF-FIRST-IN) = SPACES
                 MOVE MSG-REQUIRED TO WS-MSG PERFORM DISPLAY-AND-LOG
               END-IF
           END-PERFORM

           PERFORM UNTIL FUNCTION TRIM(WS-PROF-LAST-IN) NOT = SPACES
               MOVE MSG-ENTER-LAST TO WS-MSG PERFORM DISPLAY-AND-LOG

               PERFORM READ-NEXT-LINE
               MOVE WS-LINE TO WS-PROF-LAST-IN

               IF EOF-IN EXIT PARAGRAPH END-IF
               IF FUNCTION TRIM(WS-PROF-LAST-IN) = SPACES
                 MOVE MSG-REQUIRED TO WS-MSG PERFORM DISPLAY-AND-LOG
               END-IF
           END-PERFORM

           PERFORM UNTIL FUNCTION TRIM(WS-PROF-UNIV-IN) NOT = SPACES
               MOVE MSG-ENTER-UNIV TO WS-MSG PERFORM DISPLAY-AND-LOG

               PERFORM READ-NEXT-LINE
               MOVE WS-LINE TO WS-PROF-UNIV-IN

               IF EOF-IN EXIT PARAGRAPH END-IF
               IF FUNCTION TRIM(WS-PROF-UNIV-IN) = SPACES
                 MOVE MSG-REQUIRED TO WS-MSG PERFORM DISPLAY-AND-LOG
               END-IF
           END-PERFORM

           PERFORM UNTIL FUNCTION TRIM(WS-PROF-MAJOR-IN) NOT = SPACES
               MOVE MSG-ENTER-MAJOR TO WS-MSG PERFORM DISPLAY-AND-LOG

               PERFORM READ-NEXT-LINE
               MOVE WS-LINE TO WS-PROF-MAJOR-IN

               IF EOF-IN EXIT PARAGRAPH END-IF
               IF FUNCTION TRIM(WS-PROF-MAJOR-IN) = SPACES
                 MOVE MSG-REQUIRED TO WS-MSG PERFORM DISPLAY-AND-LOG
               END-IF
           END-PERFORM

           *> GRAD YEAR (required, 1900–2100, 4 digits)
           SET YEAR-INVALID TO TRUE       *> start invalid so we enter the loop
           PERFORM UNTIL YEAR-VALID OR EOF-IN
               MOVE MSG-ENTER-GYEAR2 TO WS-MSG PERFORM DISPLAY-AND-LOG

               PERFORM READ-NEXT-LINE
               MOVE WS-LINE TO WS-PROF-GYEAR-IN

               IF EOF-IN EXIT PARAGRAPH END-IF
               PERFORM VALIDATE-GRAD-YEAR
               IF YEAR-INVALID
                   MOVE MSG-YEAR-INVALID TO WS-MSG PERFORM DISPLAY-AND-LOG
               END-IF
           END-PERFORM

           *> ABOUT ME
           MOVE MSG-ABOUT-ME TO WS-MSG PERFORM DISPLAY-AND-LOG

           *> Read profile about me (multi-line until a blank line)
           PERFORM READ-NEXT-LINE
           MOVE WS-LINE TO WS-PROF-ABOUT-IN

           PERFORM ADD-EXPERIENCE
           PERFORM ADD-EDUCATION

           PERFORM SERIALIZE-EXPERIENCE
           PERFORM SERIALIZE-EDUCATION

           PERFORM FIND-PROFILE-BY-USERNAME
           IF PROFILE-FOUND
             MOVE FUNCTION TRIM(WS-PROF-FIRST-IN) TO WS-PROF-FIRST(WS-PROFILE-IDX)
             MOVE FUNCTION TRIM(WS-PROF-LAST-IN)  TO WS-PROF-LAST(WS-PROFILE-IDX)
             MOVE FUNCTION TRIM(WS-PROF-UNIV-IN)  TO WS-PROF-UNIV(WS-PROFILE-IDX)
             MOVE FUNCTION TRIM(WS-PROF-MAJOR-IN) TO WS-PROF-MAJOR(WS-PROFILE-IDX)
             MOVE FUNCTION TRIM(WS-PROF-GYEAR-IN) TO WS-PROF-GYEAR(WS-PROFILE-IDX)
             MOVE FUNCTION TRIM(WS-PROF-ABOUT-IN) TO WS-PROF-ABOUT(WS-PROFILE-IDX)
             MOVE FUNCTION TRIM(WS-EXPS-STR)      TO WS-PROF-EXPERIENCES(WS-PROFILE-IDX)
             MOVE FUNCTION TRIM(WS-EDUS-STR)      TO WS-PROF-EDUCATIONS(WS-PROFILE-IDX)

           ELSE
             ADD 1 TO WS-PROFILES-COUNT
             MOVE WS-PROFILES-COUNT TO WS-PROFILE-IDX
             MOVE FUNCTION TRIM(WS-CURRENT-USERNAME) TO WS-PROF-USERNAME(WS-PROFILE-IDX)
             MOVE FUNCTION TRIM(WS-PROF-FIRST-IN)    TO WS-PROF-FIRST(WS-PROFILE-IDX)
             MOVE FUNCTION TRIM(WS-PROF-LAST-IN)     TO WS-PROF-LAST(WS-PROFILE-IDX)
             MOVE FUNCTION TRIM(WS-PROF-UNIV-IN)     TO WS-PROF-UNIV(WS-PROFILE-IDX)
             MOVE FUNCTION TRIM(WS-PROF-MAJOR-IN)    TO WS-PROF-MAJOR(WS-PROFILE-IDX)
             MOVE FUNCTION TRIM(WS-PROF-GYEAR-IN)    TO WS-PROF-GYEAR(WS-PROFILE-IDX)
             MOVE FUNCTION TRIM(WS-PROF-ABOUT-IN) TO WS-PROF-ABOUT(WS-PROFILE-IDX)
             MOVE FUNCTION TRIM(WS-EXPS-STR)      TO WS-PROF-EXPERIENCES(WS-PROFILE-IDX)
             MOVE FUNCTION TRIM(WS-EDUS-STR)      TO WS-PROF-EDUCATIONS(WS-PROFILE-IDX)
           END-IF

           PERFORM SAVE-PROFILES
           MOVE MSG-PROFILE-SAVED-OK TO WS-MSG PERFORM DISPLAY-AND-LOG
           EXIT.

       ADD-EXPERIENCE.
           *> RESET COUNT TO 0
           MOVE 0 TO WS-EXP-COUNT
           MOVE SPACES TO WS-EXP-CHOICE

           PERFORM UNTIL WS-EXP-COUNT >= 3 OR WS-EXP-CHOICE = "DONE" OR EOF-IN
               MOVE MSG-ADD-EXP TO WS-MSG PERFORM DISPLAY-AND-LOG

                PERFORM READ-NEXT-LINE
                MOVE WS-LINE TO WS-EXP-CHOICE

               IF EOF-IN
                   EXIT PERFORM
               END-IF

               IF WS-EXP-CHOICE = "DONE"
                   EXIT PERFORM
               ELSE
                   ADD 1 TO WS-EXP-COUNT

                   *>TITLE
                   MOVE SPACES TO WS-MSG
                   STRING "Experience #" WS-EXP-COUNT " - Title: " DELIMITED BY SIZE
                       INTO WS-MSG
                   END-STRING
                   PERFORM DISPLAY-AND-LOG

                   PERFORM READ-NEXT-LINE
                   MOVE WS-LINE TO WS-TITLE-INPUT

                   IF EOF-IN
                       EXIT PERFORM
                   END-IF

                   MOVE WS-TITLE-INPUT TO WS-EXP-TITLE(WS-EXP-COUNT)

                   *>COMPANY/ORG
                   MOVE SPACES TO WS-MSG
                   STRING "Experience #" WS-EXP-COUNT " - Company/Organization: " DELIMITED BY SIZE
                       INTO WS-MSG
                   END-STRING
                   PERFORM DISPLAY-AND-LOG

                   PERFORM READ-NEXT-LINE
                   MOVE WS-LINE TO WS-COMPANY-INPUT

                   IF EOF-IN
                       EXIT PERFORM
                   END-IF

                   MOVE WS-COMPANY-INPUT TO WS-EXP-COMPANY(WS-EXP-COUNT)

                   *>DATE
                   MOVE SPACES TO WS-MSG
                   STRING "Experience #" WS-EXP-COUNT " - Dates (e.g., Summer 2024): " DELIMITED BY SIZE
                       INTO WS-MSG
                   END-STRING
                   PERFORM DISPLAY-AND-LOG

                   PERFORM READ-NEXT-LINE
                   MOVE WS-LINE TO WS-DATES-INPUT

                   IF EOF-IN
                       EXIT PERFORM
                   END-IF

                   MOVE WS-DATES-INPUT TO WS-EXP-DATES(WS-EXP-COUNT)

                   *>DESCRIPTION
                   MOVE SPACES TO WS-MSG
                   STRING "Experience #" WS-EXP-COUNT " - Description (max 100 chars, blank to skip): " DELIMITED BY SIZE
                       INTO WS-MSG
                   END-STRING
                   PERFORM DISPLAY-AND-LOG

                   PERFORM READ-NEXT-LINE
                   MOVE WS-LINE TO WS-DESC-INPUT

                   IF EOF-IN
                       EXIT PERFORM
                   END-IF

                   IF WS-DESC-INPUT NOT = SPACES
                       MOVE WS-DESC-INPUT TO WS-EXP-DESC(WS-EXP-COUNT)
                   END-IF
               END-IF
           END-PERFORM
           EXIT.

       ADD-EDUCATION.
           MOVE 0 TO WS-EDU-COUNT
           MOVE SPACES TO WS-EDU-CHOICE

           PERFORM UNTIL WS-EDU-COUNT >= 3 OR WS-EDU-CHOICE = "DONE" OR EOF-IN
               MOVE MSG-ADD-EDUCATION TO WS-MSG PERFORM DISPLAY-AND-LOG

               PERFORM READ-NEXT-LINE
               MOVE WS-LINE TO WS-EDU-CHOICE

               IF EOF-IN
                   EXIT PERFORM
               END-IF

               IF WS-EDU-CHOICE = "DONE"
                   EXIT PERFORM
               ELSE
                   ADD 1 TO WS-EDU-COUNT

                   *>Degree
                   MOVE SPACES TO WS-MSG
                   STRING "Education #" WS-EDU-COUNT " - Degree: " DELIMITED BY SIZE
                       INTO WS-MSG
                   END-STRING
                   PERFORM DISPLAY-AND-LOG

                   PERFORM READ-NEXT-LINE
                   MOVE WS-LINE TO WS-DEGREE-INPUT

                   IF EOF-IN
                       EXIT PERFORM
                   END-IF

                   MOVE WS-DEGREE-INPUT TO WS-EDU-DEGREE(WS-EDU-COUNT)

                   *>Uni/College
                   MOVE SPACES TO WS-MSG
                   STRING "Education #" WS-EDU-COUNT " - University/College: " DELIMITED BY SIZE
                       INTO WS-MSG
                   END-STRING
                   PERFORM DISPLAY-AND-LOG

                   PERFORM READ-NEXT-LINE
                   MOVE WS-LINE TO WS-SCHOOL-INPUT

                   IF EOF-IN
                       EXIT PERFORM
                   END-IF

                   MOVE WS-SCHOOL-INPUT TO WS-EDU-SCHOOL(WS-EDU-COUNT)

                   *>Years attended
                   MOVE SPACES TO WS-MSG
                   STRING "Education #" WS-EDU-COUNT " - Years Attended (e.g., 2023-2025): " DELIMITED BY SIZE
                       INTO WS-MSG
                   END-STRING
                   PERFORM DISPLAY-AND-LOG

                   PERFORM READ-NEXT-LINE
                   MOVE WS-LINE TO WS-YEARS-INPUT

                   IF EOF-IN
                       EXIT PERFORM
                   END-IF

                   MOVE WS-YEARS-INPUT TO WS-EDU-YEARS(WS-EDU-COUNT)

               END-IF
           END-PERFORM
           EXIT.

       VIEW-MY-PROFILE.
           PERFORM FIND-PROFILE-BY-USERNAME
           IF PROFILE-FOUND
             *> load stored profile into display/input buffers for viewing
             MOVE FUNCTION TRIM(WS-PROF-FIRST(WS-PROFILE-IDX))   TO WS-PROF-FIRST-IN
             MOVE FUNCTION TRIM(WS-PROF-LAST(WS-PROFILE-IDX))    TO WS-PROF-LAST-IN
             MOVE FUNCTION TRIM(WS-PROF-UNIV(WS-PROFILE-IDX))    TO WS-PROF-UNIV-IN
             MOVE FUNCTION TRIM(WS-PROF-MAJOR(WS-PROFILE-IDX))   TO WS-PROF-MAJOR-IN
             MOVE FUNCTION TRIM(WS-PROF-GYEAR(WS-PROFILE-IDX))   TO WS-PROF-GYEAR-IN
             MOVE FUNCTION TRIM(WS-PROF-ABOUT(WS-PROFILE-IDX))   TO WS-PROF-ABOUT-IN
             MOVE FUNCTION TRIM(WS-PROF-EXPERIENCES(WS-PROFILE-IDX)) TO WS-EXPS-STR
             MOVE FUNCTION TRIM(WS-PROF-EDUCATIONS(WS-PROFILE-IDX))  TO WS-EDUS-STR
           ELSE
               MOVE MSG-PROFILE-NOT-FOUND TO WS-MSG PERFORM DISPLAY-AND-LOG
               EXIT PARAGRAPH
           END-IF

           MOVE MSG-VIEW-HEADER TO WS-MSG PERFORM DISPLAY-AND-LOG

           MOVE SPACES TO WS-MSG
           STRING "Name: " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-PROF-FIRST-IN) DELIMITED BY SIZE
                  " " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-PROF-LAST-IN) DELIMITED BY SIZE
                  INTO WS-MSG
           END-STRING
           PERFORM DISPLAY-AND-LOG

           MOVE SPACES TO WS-MSG
           STRING "University: " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-PROF-UNIV-IN) DELIMITED BY SIZE
                  INTO WS-MSG
           END-STRING
           PERFORM DISPLAY-AND-LOG

           MOVE SPACES TO WS-MSG
           STRING "Major: " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-PROF-MAJOR-IN) DELIMITED BY SIZE
                  INTO WS-MSG
           END-STRING
           PERFORM DISPLAY-AND-LOG

           MOVE SPaces TO WS-MSG
           STRING "Graduation Year: " DELIMITED BY SIZE
                  FUNCTION TRIM(WS-PROF-GYEAR-IN) DELIMITED BY SIZE
                  INTO WS-MSG
           END-STRING
           PERFORM DISPLAY-AND-LOG

           IF FUNCTION TRIM(WS-PROF-ABOUT-IN) NOT = SPACES
               MOVE SPACES TO WS-MSG
               STRING "About Me: " DELIMITED BY SIZE
                      FUNCTION TRIM(WS-PROF-ABOUT-IN) DELIMITED BY SIZE
                      INTO WS-MSG
               END-STRING
               PERFORM DISPLAY-AND-LOG
           END-IF

           PERFORM DISPLAY-EXPERIENCES

           PERFORM DISPLAY-EDUCATION

           MOVE MSG-LINE TO WS-MSG PERFORM DISPLAY-AND-LOG
           EXIT.

       HELPER-SECTION.
       DISPLAY-AND-LOG.
           *> Write message to output file and display it (preserve indentation)
           MOVE SPACES TO OUTPUT-REC
           MOVE FUNCTION TRIM(WS-MSG TRAILING) TO OUTPUT-REC
           WRITE OUTPUT-REC
           DISPLAY FUNCTION TRIM(WS-MSG TRAILING)
           EXIT.

       READ-NEXT-LINE.
           *> Reusable read line, save input to WS-LINE (username, password, choice, etc) then trim whitespace
           *> Usage: PERFORM READ-NEXT-LINE then MOVE WS-LINE TO <var> (assign the output back to the caller's variable)
           MOVE SPACES TO WS-LINE
           READ INPUT-FILE
               AT END SET EOF-IN TO TRUE
               NOT AT END
                   MOVE FUNCTION TRIM(INPUT-REC) TO WS-LINE
           END-READ
           EXIT.
