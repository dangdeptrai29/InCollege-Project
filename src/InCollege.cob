        >>SOURCE FORMAT FREE
        >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INCOLLEGE.
       AUTHOR. Wisconsin Team.

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
           SELECT REQUEST-FILE ASSIGN TO "data/requests.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-REQ-STATUS.
           *> New file for jobs
           SELECT JOBS-FILE ASSIGN TO "data/jobs.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-JOBS-FILE-STATUS.
           *> New file for job applications
           SELECT APPLICATIONS-FILE ASSIGN TO "data/applications.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-APP-STATUS.
           *> New file for messages
           SELECT MESSAGES-FILE ASSIGN TO "data/messages.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-MSG-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-REC                     PIC X(256).
       01  INPUT-REC                     PIC X(256).

       FD  OUTPUT-FILE.
       01  OUTPUT-REC                    PIC X(256).
       01  OUTPUT-REC                    PIC X(256).

       FD  USERS-FILE.
       01  USER-REC                      PIC X(256).
       01  USER-REC                      PIC X(256).

       FD  USERS-EXAMPLE-FILE.
       01  USER-REC-EX                   PIC X(256).
       01  USER-REC-EX                   PIC X(256).

       FD  PROFILES-FILE.
       01  PROFILE-REC                   PIC X(2048).
       01  PROFILE-REC                   PIC X(2048).

       FD  REQUEST-FILE.
       01  REQUEST-REC                   PIC X(256).
       01  REQUEST-REC                   PIC X(256).

       *> New FD for connections file
       FD  CONNECTIONS-FILE.
       01  CONNECTION-REC                PIC X(258).
       01  CONNECTION-REC                PIC X(258).

       *> New FD for jobs file
       FD  JOBS-FILE.
       01  JOB-REC                       PIC X(1024).
       01  JOB-REC                       PIC X(1024).

       FD  APPLICATIONS-FILE.
       01  APPLICATION-REC               PIC X(256).
       01  APPLICATION-REC               PIC X(256).

       FD  MESSAGES-FILE.
       01  MESSAGE-REC                   PIC X(512).
       01  MESSAGE-REC                   PIC X(512).

       WORKING-STORAGE SECTION.
       *> File status codes
       01  WS-IN-STATUS                  PIC XX VALUE "00".
       01  WS-OUT-STATUS                 PIC XX VALUE "00".
       01  WS-USR-STATUS                 PIC XX VALUE "00".
       01  WS-UEX-STATUS                 PIC XX VALUE "00".
       01  WS-PROF-STATUS                PIC XX VALUE "00".
       01  WS-CONN-FILE-STATUS           PIC XX VALUE "00".
       01  WS-JOBS-FILE-STATUS           PIC XX VALUE "00".
       01  WS-J-DISP                     PIC 9.
       01  WS-APP-STATUS                 PIC XX VALUE "00".
       01  WS-APPL-STATUS                PIC XX VALUE "00".
       01  WS-MSG-FILE-STATUS            PIC XX VALUE "00".
       01  WS-IN-STATUS                  PIC XX VALUE "00".
       01  WS-OUT-STATUS                 PIC XX VALUE "00".
       01  WS-USR-STATUS                 PIC XX VALUE "00".
       01  WS-UEX-STATUS                 PIC XX VALUE "00".
       01  WS-PROF-STATUS                PIC XX VALUE "00".
       01  WS-CONN-FILE-STATUS           PIC XX VALUE "00".
       01  WS-JOBS-FILE-STATUS           PIC XX VALUE "00".
       01  WS-J-DISP                     PIC 9.
       01  WS-APP-STATUS                 PIC XX VALUE "00".
       01  WS-APPL-STATUS                PIC XX VALUE "00".
       01  WS-MSG-FILE-STATUS            PIC XX VALUE "00".

       *> End-of-file flags with condition names
       01  WS-EOF-IN                     PIC X VALUE 'N'.
           88  EOF-IN                        VALUE 'Y'.
           88  NOT-EOF-IN                    VALUE 'N'.
       01  WS-EOF-USR                    PIC X VALUE 'N'.
           88  EOF-USR                       VALUE 'Y'.
           88  NOT-EOF-USR                   VALUE 'N'.
       01  WS-EOF-PROF                   PIC X VALUE 'N'.
           88  EOF-PROF                      VALUE 'Y'.
           88  NOT-EOF-PROF                  VALUE 'N'.
       01  WS-EOF-CONN                   PIC X VALUE 'N'.
           88  EOF-CONN                      VALUE 'Y'.
           88  NOT-EOF-CONN                  VALUE 'N'.
       01  WS-EOF-JOBS                   PIC X VALUE 'N'.
           88  EOF-JOBS                      VALUE 'Y'.
           88  NOT-EOF-JOBS                  VALUE 'N'.
       01  WS-EOF-APPS                   PIC X VALUE 'N'.
           88  EOF-APPS                      VALUE 'Y'.
           88  NOT-EOF-APPS                  VALUE 'N'.
       01  WS-EOF-MSG                    PIC X VALUE 'N'.
           88  EOF-MSG                       VALUE 'Y'.
           88  NOT-EOF-MSG                   VALUE 'N'.
       01  WS-EOF-IN                     PIC X VALUE 'N'.
           88  EOF-IN                        VALUE 'Y'.
           88  NOT-EOF-IN                    VALUE 'N'.
       01  WS-EOF-USR                    PIC X VALUE 'N'.
           88  EOF-USR                       VALUE 'Y'.
           88  NOT-EOF-USR                   VALUE 'N'.
       01  WS-EOF-PROF                   PIC X VALUE 'N'.
           88  EOF-PROF                      VALUE 'Y'.
           88  NOT-EOF-PROF                  VALUE 'N'.
       01  WS-EOF-CONN                   PIC X VALUE 'N'.
           88  EOF-CONN                      VALUE 'Y'.
           88  NOT-EOF-CONN                  VALUE 'N'.
       01  WS-EOF-JOBS                   PIC X VALUE 'N'.
           88  EOF-JOBS                      VALUE 'Y'.
           88  NOT-EOF-JOBS                  VALUE 'N'.
       01  WS-EOF-APPS                   PIC X VALUE 'N'.
           88  EOF-APPS                      VALUE 'Y'.
           88  NOT-EOF-APPS                  VALUE 'N'.
       01  WS-EOF-MSG                    PIC X VALUE 'N'.
           88  EOF-MSG                       VALUE 'Y'.
           88  NOT-EOF-MSG                   VALUE 'N'.

       *> Generic Input buffer
       01  WS-LINE                       PIC X(256) VALUE SPACES.
       01  WS-LINE                       PIC X(256) VALUE SPACES.

       *> Credentials for the current attempt
       01  WS-USERNAME                   PIC X(128) VALUE SPACES.
       01  WS-PASSWORD                   PIC X(128) VALUE SPACES.
       01  WS-CHOICE                     PIC X(16)  VALUE SPACES.
       01  WS-CURRENT-USERNAME           PIC X(128) VALUE SPACES.
       01  WS-USERNAME                   PIC X(128) VALUE SPACES.
       01  WS-PASSWORD                   PIC X(128) VALUE SPACES.
       01  WS-CHOICE                     PIC X(16)  VALUE SPACES.
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
       01  MSG-INVALID-CHOICE            PIC X(32)  VALUE "Invalid option".
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
       01  MSG-INVALID-CHOICE            PIC X(32)  VALUE "Invalid option".

       *> In-memory users table
       01  WS-MAX-USERS                  PIC 9(4) VALUE 200.
       01  WS-ACCOUNT-LIMIT              PIC 9(4) VALUE 5.
       01  WS-USERS-COUNT                PIC 9(4) VALUE 0.
       01  WS-MAX-USERS                  PIC 9(4) VALUE 200.
       01  WS-USERS-COUNT                PIC 9(4) VALUE 0.
       01  WS-USERS-TABLE.
           05  WS-USER OCCURS 0 TO 200 TIMES
                   DEPENDING ON WS-USERS-COUNT
                   INDEXED BY USR-IDX.
               10  WS-TBL-USERNAME       PIC X(128).
               10  WS-TBL-PASSWORD       PIC X(128).
               10  WS-TBL-USERNAME       PIC X(128).
               10  WS-TBL-PASSWORD       PIC X(128).

       *> Profiles table
       01  WS-PROFILES-MAX               PIC 9(4) VALUE 200.
       01  WS-PROFILES-COUNT             PIC 9(4) VALUE 0.
       01  WS-PROFILES-MAX               PIC 9(4) VALUE 200.
       01  WS-PROFILES-COUNT             PIC 9(4) VALUE 0.
       01  WS-PROFILES-TABLE.
           05  WS-PROFILE OCCURS 0 TO 200 TIMES
                   DEPENDING ON WS-PROFILES-COUNT
                   INDEXED BY PROF-INDEX.
               10  WS-PROF-USERNAME      PIC X(128).
               10  WS-PROF-FIRST         PIC X(64).
               10  WS-PROF-LAST          PIC X(64).
               10  WS-PROF-UNIV          PIC X(128).
               10  WS-PROF-MAJOR         PIC X(128).
               10  WS-PROF-GYEAR         PIC X(4).
               10  WS-PROF-ABOUT         PIC X(200).
               10  WS-PROF-EXPERIENCES   PIC X(512).
               10  WS-PROF-EDUCATIONS    PIC X(512).
               10  WS-PROF-USERNAME      PIC X(128).
               10  WS-PROF-FIRST         PIC X(64).
               10  WS-PROF-LAST          PIC X(64).
               10  WS-PROF-UNIV          PIC X(128).
               10  WS-PROF-MAJOR         PIC X(128).
               10  WS-PROF-GYEAR         PIC X(4).
               10  WS-PROF-ABOUT         PIC X(200).
               10  WS-PROF-EXPERIENCES   PIC X(512).
               10  WS-PROF-EDUCATIONS    PIC X(512).

       *> Connections table
       01  WS-CONNECTIONS-MAX            PIC 9(4) VALUE 500.
       01  WS-CONNECTIONS-COUNT          PIC 9(4) VALUE 0.
       01  WS-CONNECTIONS-MAX            PIC 9(4) VALUE 500.
       01  WS-CONNECTIONS-COUNT          PIC 9(4) VALUE 0.
       01  WS-CONNECTIONS-TABLE.
           05  WS-CONNECTION OCCURS 0 TO 500 TIMES
                   DEPENDING ON WS-CONNECTIONS-COUNT
                   INDEXED BY CONN-IDX.
               10  WS-CONN-SENDER        PIC X(128).
               10  WS-CONN-RECEIVER      PIC X(128).
               10  WS-CONN-STATUS        PIC X. *> 'P' or 'A'
               10  WS-CONN-SENDER        PIC X(128).
               10  WS-CONN-RECEIVER      PIC X(128).
               10  WS-CONN-STATUS        PIC X. *> 'P' or 'A'

       *> Job postings table
       01  WS-JOBS-MAX                   PIC 9(4) VALUE 200.
       01  WS-JOBS-COUNT                 PIC 9(4) VALUE 0.
       01  WS-JOBS-HIGHEST-ID            PIC 9(6) VALUE 0.
       01  WS-JOBS-MAX                   PIC 9(4) VALUE 200.
       01  WS-JOBS-COUNT                 PIC 9(4) VALUE 0.
       01  WS-JOBS-HIGHEST-ID            PIC 9(6) VALUE 0.
       01  WS-JOBS-TABLE.
           05  WS-JOB-ENTRY OCCURS 0 TO 200 TIMES
                   DEPENDING ON WS-JOBS-COUNT
                   INDEXED BY JOB-IDX.
               10  WS-JOB-ID             PIC 9(6).
               10  WS-JOB-POSTER-USER    PIC X(128).
               10  WS-JOB-TITLE          PIC X(128).
               10  WS-JOB-DESC           PIC X(256).
               10  WS-JOB-EMPLOYER       PIC X(128).
               10  WS-JOB-LOCATION       PIC X(128).
               10  WS-JOB-SALARY         PIC X(128).
               10  WS-JOB-ID             PIC 9(6).
               10  WS-JOB-POSTER-USER    PIC X(128).
               10  WS-JOB-TITLE          PIC X(128).
               10  WS-JOB-DESC           PIC X(256).
               10  WS-JOB-EMPLOYER       PIC X(128).
               10  WS-JOB-LOCATION       PIC X(128).
               10  WS-JOB-SALARY         PIC X(128).

       *> Applications table (job-id | username)
       01  WS-APPLICATIONS-MAX           PIC 9(4) VALUE 500.
       01  WS-APPLICATIONS-COUNT         PIC 9(4) VALUE 0.
       01  WS-APPLICATIONS-MAX           PIC 9(4) VALUE 500.
       01  WS-APPLICATIONS-COUNT         PIC 9(4) VALUE 0.
       01  WS-APPLICATIONS-TABLE.
           05  WS-APPLICATION OCCURS 0 TO 500 TIMES
                   DEPENDING ON WS-APPLICATIONS-COUNT
                   INDEXED BY APP-IDX.
               10  WS-APP-JOB-ID         PIC 9(6).
               10  WS-APP-USER           PIC X(128).
               10  WS-APP-JOB-ID         PIC 9(6).
               10  WS-APP-USER           PIC X(128).

       *> Messages table (sender | receiver | content)
       01  WS-MESSAGES-MAX               PIC 9(4) VALUE 500.
       01  WS-MESSAGES-COUNT             PIC 9(4) VALUE 0.
       01  WS-MESSAGES-MAX               PIC 9(4) VALUE 500.
       01  WS-MESSAGES-COUNT             PIC 9(4) VALUE 0.
       01  WS-MESSAGES-TABLE.
           05  WS-MESSAGE-ENTRY OCCURS 0 TO 500 TIMES
                   DEPENDING ON WS-MESSAGES-COUNT
                   INDEXED BY MSG-IDX.
               10  WS-MSG-SENDER-ENTRY   PIC X(128).
               10  WS-MSG-RECEIVER-ENTRY PIC X(128).
               10  WS-MSG-CONTENT-ENTRY  PIC X(200).
               10  WS-MSG-TIMESTAMP-ENTRY PIC X(20).
               10  WS-MSG-SENDER-ENTRY   PIC X(128).
               10  WS-MSG-RECEIVER-ENTRY PIC X(128).
               10  WS-MSG-CONTENT-ENTRY  PIC X(200).
               10  WS-MSG-TIMESTAMP-ENTRY PIC X(20).

       *> Variables for handling job input
       01  WS-NEW-JOB-ID                 PIC 9(6).
       01  WS-NEW-JOB-TITLE              PIC X(128).
       01  WS-NEW-JOB-DESC               PIC X(256).
       01  WS-NEW-JOB-EMPLOYER           PIC X(128).
       01  WS-NEW-JOB-LOCATION           PIC X(128).
       01  WS-NEW-JOB-SALARY             PIC X(128).
       01  WS-NEW-JOB-ID                 PIC 9(6).
       01  WS-NEW-JOB-TITLE              PIC X(128).
       01  WS-NEW-JOB-DESC               PIC X(256).
       01  WS-NEW-JOB-EMPLOYER           PIC X(128).
       01  WS-NEW-JOB-LOCATION           PIC X(128).
       01  WS-NEW-JOB-SALARY             PIC X(128).

       77  WS-JOB-ID-TEXT                PIC X(12).
       77  WS-JOB-DELIM-COUNT            PIC 9(02).
       77  WS-JOB-ID-DISPLAY             PIC Z(5)9.
       01  WS-JOBS-ERR-CONTEXT           PIC X(64).
       01  WS-JOBS-ERROR-FLAG            PIC X VALUE 'N'.
           88  JOBS-IO-OK                    VALUE 'N'.
           88  JOBS-IO-ERROR                 VALUE 'Y'.
       77  WS-JOB-ID-TEXT                PIC X(12).
       77  WS-JOB-DELIM-COUNT            PIC 9(02).
       77  WS-JOB-ID-DISPLAY             PIC Z(5)9.
       01  WS-JOBS-ERR-CONTEXT           PIC X(64).
       01  WS-JOBS-ERROR-FLAG            PIC X VALUE 'N'.
           88  JOBS-IO-OK                    VALUE 'N'.
           88  JOBS-IO-ERROR                 VALUE 'Y'.

       *> Connection requests variables
       01  WS-REQ-STATUS                 PIC XX VALUE "00".
       77  APP-ID-TEXT                   PIC X(12).
       77  SAVE-JOBS-COUNT               PIC 9(4) VALUE 0.
       77  SAVE-APPS-COUNT               PIC 9(4) VALUE 0.
       01  WS-REQ-STATUS                 PIC XX VALUE "00".
       77  APP-ID-TEXT                   PIC X(12).
       77  SAVE-JOBS-COUNT               PIC 9(4) VALUE 0.
       77  SAVE-APPS-COUNT               PIC 9(4) VALUE 0.

       01  WS-EOF-REQ                    PIC X VALUE 'N'.
           88  EOF-REQ                       VALUE 'Y'.
           88  NOT-EOF-REQ                   VALUE 'N'.
       01  WS-EOF-REQ                    PIC X VALUE 'N'.
           88  EOF-REQ                       VALUE 'Y'.
           88  NOT-EOF-REQ                   VALUE 'N'.

       *> Simple request variables
       01  WS-REQ-SENDER                 PIC X(128) VALUE SPACES.
       01  WS-REQ-RECEIVER               PIC X(128) VALUE SPACES.
       01  WS-REQ-STATUS-VALUE           PIC X(10)  VALUE SPACES.

       01  WS-I                          PIC 9(4) VALUE 0.
       01  WS-J                          PIC 9(4) VALUE 0.
       01  WS-SEARCH-RESULT-IDX          PIC 9(4) VALUE 0.

       *> Scratch area for parsing user file records
       01  WS-USER-FILE-USERNAME         PIC X(128) VALUE SPACES.
       01  WS-USER-FILE-PASSWORD         PIC X(128) VALUE SPACES.
       01  WS-USER-FILE-USERNAME         PIC X(128) VALUE SPACES.
       01  WS-USER-FILE-PASSWORD         PIC X(128) VALUE SPACES.

       *> Match flag with condition names
       01  WS-MATCH-FOUND                PIC X VALUE 'N'.
           88  MATCH-FOUND                   VALUE 'Y'.
           88  MATCH-NOT-FOUND               VALUE 'N'.
       01  WS-MATCH-FOUND                PIC X VALUE 'N'.
           88  MATCH-FOUND                   VALUE 'Y'.
           88  MATCH-NOT-FOUND               VALUE 'N'.

       *> Variables to hold input while creating new account
       01  WS-NEW-USERNAME               PIC X(128) VALUE SPACES.
       01  WS-NEW-PASSWORD               PIC X(128) VALUE SPACES.
       01  WS-NEW-USERNAME               PIC X(128) VALUE SPACES.
       01  WS-NEW-PASSWORD               PIC X(128) VALUE SPACES.

       *> Vars for validating password
       01  WS-PASSWORD-INVALID           PIC X VALUE 'N'.
           88  PASS-VALID                    VALUE 'N'.
           88  PASS-INVALID                  VALUE 'Y'.
       01  WS-PASSWORD-ERROR             PIC X(128) VALUE SPACES.
       01  WS-PASS-LEN                   PIC 9(4) VALUE 0.
       01  WS-UPPER-COUNT                PIC 9(4) VALUE 0.
       01  WS-DIGIT-COUNT                PIC 9(4) VALUE 0.
       01  WS-SPECIAL-COUNT              PIC 9(4) VALUE 0.
       01  WS-SPECIAL-CHARS              PIC X(20) VALUE "!@#$%^&*?-_+".
       01  WS-CHAR                       PIC X      VALUE SPACE.
       01  WS-TMP-COUNT                  PIC 9(4)   VALUE 0.
       01  WS-PASSWORD-INVALID           PIC X VALUE 'N'.
           88  PASS-VALID                    VALUE 'N'.
           88  PASS-INVALID                  VALUE 'Y'.
       01  WS-PASSWORD-ERROR             PIC X(128) VALUE SPACES.
       01  WS-PASS-LEN                   PIC 9(4) VALUE 0.
       01  WS-UPPER-COUNT                PIC 9(4) VALUE 0.
       01  WS-DIGIT-COUNT                PIC 9(4) VALUE 0.
       01  WS-SPECIAL-COUNT              PIC 9(4) VALUE 0.
       01  WS-SPECIAL-CHARS              PIC X(20) VALUE "!@#$%^&*-_+".
       01  WS-CHAR                       PIC X      VALUE SPACE.
       01  WS-TMP-COUNT                  PIC 9(4)   VALUE 0.

       *> Profile I/O buffers
       01  WS-PROF-USER                  PIC X(128) VALUE SPACES.
       01  WS-PROF-FIRST-IN              PIC X(64)  VALUE SPACES.
       01  WS-PROF-LAST-IN               PIC X(64)  VALUE SPACES.
       01  WS-PROF-UNIV-IN               PIC X(128) VALUE SPACES.
       01  WS-PROF-MAJOR-IN              PIC X(128) VALUE SPACES.
       01  WS-PROF-GYEAR-IN              PIC X(4)   VALUE SPACES.
       01  WS-PROF-ABOUT-IN              PIC X(200) VALUE SPACES.
       01  WS-PROF-USER                  PIC X(128) VALUE SPACES.
       01  WS-PROF-FIRST-IN              PIC X(64)  VALUE SPACES.
       01  WS-PROF-LAST-IN               PIC X(64)  VALUE SPACES.
       01  WS-PROF-UNIV-IN               PIC X(128) VALUE SPACES.
       01  WS-PROF-MAJOR-IN              PIC X(128) VALUE SPACES.
       01  WS-PROF-GYEAR-IN              PIC X(4)   VALUE SPACES.
       01  WS-PROF-ABOUT-IN              PIC X(200) VALUE SPACES.

       01  WS-GYEAR-NUM                  PIC 9(4)   VALUE 0.
       01  WS-YEAR-INVALID               PIC X      VALUE 'N'.
           88  YEAR-VALID                    VALUE 'N'.
           88  YEAR-INVALID                  VALUE 'Y'.
       01  WS-GYEAR-NUM                  PIC 9(4)   VALUE 0.
       01  WS-YEAR-INVALID               PIC X      VALUE 'N'.
           88  YEAR-VALID                    VALUE 'N'.
           88  YEAR-INVALID                  VALUE 'Y'.

       01  WS-PROFILE-FOUND              PIC X      VALUE 'N'.
           88  PROFILE-FOUND                 VALUE 'Y'.
           88  PROFILE-NOT-FOUND             VALUE 'N'.
       01  WS-PROFILE-FOUND              PIC X      VALUE 'N'.
           88  PROFILE-FOUND                 VALUE 'Y'.
           88  PROFILE-NOT-FOUND             VALUE 'N'.

       01  WS-PROFILE-IDX                PIC 9(4)   VALUE 0.
       01  WS-PROFILE-IDX                PIC 9(4)   VALUE 0.

       *> Epic 5
       01  WS-DISPLAY-NAME               PIC X(256) VALUE SPACES.
       01  WS-TARGET-USERNAME            PIC X(128) VALUE SPACES.
       01  WS-DISPLAY-NAME               PIC X(256) VALUE SPACES.
       01  WS-TARGET-USERNAME            PIC X(128) VALUE SPACES.

       *> temp holders for (de)serializing lists
       01  WS-EXPS-STR                   PIC X(512)  VALUE SPACES.
       01  WS-EDUS-STR                   PIC X(512)  VALUE SPACES.
       01  WS-ENTRY                      PIC X(256)  VALUE SPACES.
       01  WS-T1                         PIC X(128)  VALUE SPACES.
       01  WS-T2                         PIC X(128)  VALUE SPACES.
       01  WS-T3                         PIC X(128)  VALUE SPACES.
       01  WS-T4                         PIC X(128)  VALUE SPACES.
       01  WS-REST                       PIC X(1024) VALUE SPACES.
       01  WS-REST-LEN                   PIC 9(4)    VALUE 0.
       01  WS-LAST-PIPE                  PIC 9(4)    VALUE 0.
       01  WS-EXPS-STR                   PIC X(512)  VALUE SPACES.
       01  WS-EDUS-STR                   PIC X(512)  VALUE SPACES.
       01  WS-ENTRY                      PIC X(256)  VALUE SPACES.
       01  WS-T1                         PIC X(128)  VALUE SPACES.
       01  WS-T2                         PIC X(128)  VALUE SPACES.
       01  WS-T3                         PIC X(128)  VALUE SPACES.
       01  WS-T4                         PIC X(128)  VALUE SPACES.
       01  WS-REST                       PIC X(1024) VALUE SPACES.
       01  WS-REST-LEN                   PIC 9(4)    VALUE 0.
       01  WS-LAST-PIPE                  PIC 9(4)    VALUE 0.

       *> Account creation messages
       01  MSG-ACCOUNT-LIMIT             PIC X(80) VALUE
       01  MSG-ACCOUNT-LIMIT             PIC X(80) VALUE
           "All permitted accounts have been created, please come back later.".
       01  MSG-USERNAME-EXISTS           PIC X(64) VALUE
       01  MSG-USERNAME-EXISTS           PIC X(64) VALUE
           "Username already exists. Please try a different one.".
       01  MSG-ENTER-NEW-USER            PIC X(64) VALUE
       01  MSG-ENTER-NEW-USER            PIC X(64) VALUE
           "Please enter your username:".
       01  MSG-ENTER-NEW-PASS            PIC X(64) VALUE
       01  MSG-ENTER-NEW-PASS            PIC X(64) VALUE
           "Please enter your password:".
       01  MSG-ACCOUNT-SUCCESS           PIC X(64) VALUE
       01  MSG-ACCOUNT-SUCCESS           PIC X(64) VALUE
           "Account created successfully.".

       *> Logged-in choices
       01  WS-LOGGED-CHOICE              PIC X(8) VALUE SPACES.
       01  WS-SKILL-CHOICE               PIC X(8) VALUE SPACES.
       01  WS-LOGGED-CHOICE              PIC X(8) VALUE SPACES.
       01  WS-SKILL-CHOICE               PIC X(8) VALUE SPACES.

       *> Jobs sub-menu
       01  WS-JOB-CHOICE                 PIC X(8) VALUE SPACES.
       01  WS-JOB-CHOICE                 PIC X(8) VALUE SPACES.

       *> Main menu messages
       01  MSG-MENU-VIEW-PROFILE         PIC X(32) VALUE "1. View My Profile".
       01  MSG-MENU-JOBS                 PIC X(32) VALUE "Search for a job".
       01  MSG-MENU-SEARCH-USER          PIC X(32) VALUE "2. Search for User".
       01  MSG-MENU-LEARN-SKILL          PIC X(32) VALUE "3. Learn a New Skill".
       01  MSG-MENU-VIEW-PENDING         PIC X(48) VALUE
       01  MSG-MENU-VIEW-PROFILE         PIC X(32) VALUE "1. View My Profile".
       01  MSG-MENU-JOBS                 PIC X(32) VALUE "Search for a job".
       01  MSG-MENU-SEARCH-USER          PIC X(32) VALUE "2. Search for User".
       01  MSG-MENU-LEARN-SKILL          PIC X(32) VALUE "3. Learn a New Skill".
       01  MSG-MENU-VIEW-PENDING         PIC X(48) VALUE
           "4. View My Pending Connection Requests".
       01  MSG-MENU-VIEW-NETWORK         PIC X(32) VALUE "5. View My Network".
       01  MSG-MENU-MESSAGE              PIC X(32) VALUE "6. Messages".
       01  MSG-MENU-VIEW-NETWORK         PIC X(32) VALUE "5. View My Network".
       01  MSG-MENU-MESSAGE              PIC X(32) VALUE "6. Messages".

       *> Skills
       01  MSG-SKILL1                    PIC X(32) VALUE "Skill 1".
       01  MSG-SKILL2                    PIC X(32) VALUE "Skill 2".
       01  MSG-SKILL3                    PIC X(32) VALUE "Skill 3".
       01  MSG-SKILL4                    PIC X(32) VALUE "Skill 4".
       01  MSG-SKILL5                    PIC X(32) VALUE "Skill 5".
       01  MSG-SKILL6                    PIC X(32) VALUE "Go Back".
       01  MSG-SKILL-UNDER               PIC X(64) VALUE
       01  MSG-SKILL1                    PIC X(32) VALUE "Skill 1".
       01  MSG-SKILL2                    PIC X(32) VALUE "Skill 2".
       01  MSG-SKILL3                    PIC X(32) VALUE "Skill 3".
       01  MSG-SKILL4                    PIC X(32) VALUE "Skill 4".
       01  MSG-SKILL5                    PIC X(32) VALUE "Skill 5".
       01  MSG-SKILL6                    PIC X(32) VALUE "Go Back".
       01  MSG-SKILL-UNDER               PIC X(64) VALUE
           "This skill is under construction.".

       *> Profile messages
       01  MSG-EDIT-HEADER               PIC X(32) VALUE "--- Create/Edit Profile ---".
       01  MSG-VIEW-HEADER               PIC X(32) VALUE "--- Your Profile ---".
       01  MSG-LINE                      PIC X(20) VALUE "--------------------".
       01  MSG-LINE-LONG                 PIC X(25) VALUE "-------------------------".
       01  MSG-END-OF-PROGRAM            PIC X(32) VALUE
       01  MSG-EDIT-HEADER               PIC X(32) VALUE "--- Create/Edit Profile ---".
       01  MSG-VIEW-HEADER               PIC X(32) VALUE "--- Your Profile ---".
       01  MSG-LINE                      PIC X(20) VALUE "--------------------".
       01  MSG-LINE-LONG                 PIC X(25) VALUE "-------------------------".
       01  MSG-END-OF-PROGRAM            PIC X(32) VALUE
           "--- END_OF_PROGRAM_EXECUTION ---".
       01  MSG-ENTER-FIRST               PIC X(32) VALUE "Enter First Name:".
       01  MSG-ENTER-LAST                PIC X(32) VALUE "Enter Last Name:".
       01  MSG-ENTER-UNIV                PIC X(48)
       01  MSG-ENTER-FIRST               PIC X(32) VALUE "Enter First Name:".
       01  MSG-ENTER-LAST                PIC X(32) VALUE "Enter Last Name:".
       01  MSG-ENTER-UNIV                PIC X(48)
           VALUE "Enter University/College Attended:".
       01  MSG-ENTER-MAJOR               PIC X(32) VALUE "Enter Major:".
       01  MSG-ENTER-GYEAR2              PIC X(32)
       01  MSG-ENTER-MAJOR               PIC X(32) VALUE "Enter Major:".
       01  MSG-ENTER-GYEAR2              PIC X(32)
           VALUE "Enter Graduation Year (YYYY):".
       01  MSG-REQUIRED                  PIC X(64)
       01  MSG-REQUIRED                  PIC X(64)
           VALUE "This field is required. Please try again.".
       01  MSG-YEAR-INVALID              PIC X(80)
       01  MSG-YEAR-INVALID              PIC X(80)
           VALUE "Graduation year must be 1900-2100 and 4 digits.".
       01  MSG-PROFILE-SAVED-OK          PIC X(64) VALUE "Profile saved successfully!".
       01  MSG-PROFILE-NOT-FOUND         PIC X(64)
       01  MSG-PROFILE-SAVED-OK          PIC X(64) VALUE "Profile saved successfully!".
       01  MSG-PROFILE-NOT-FOUND         PIC X(64)
           VALUE "No profile found. Please create your profile first.".

       *> ABOUT / Experience / Education
       01  MSG-ABOUT-ME                  PIC X(80)
       01  MSG-ABOUT-ME                  PIC X(80)
           VALUE "Enter About Me (optional, max 200 chars, enter blank line to skip):".
       01  WS-ABOUT-ME                   PIC X(200).
       01  MSG-ADD-EXP                   PIC X(90)
       01  WS-ABOUT-ME                   PIC X(200).
       01  MSG-ADD-EXP                   PIC X(90)
           VALUE "Add Experiences (optional, max 3 entries. Enter 'DONE' to finish):".
       01  WS-EXP-CHOICE                 PIC X(20).
       01  WS-EXP-CHOICE                 PIC X(20).
       01  WS-EXPERIENCE.
           05  WS-EXP-COUNT              PIC 9.
           05  WS-EXP-COUNT              PIC 9.
           05  WS-EXP-ENTRY OCCURS 3 TIMES.
               10  WS-EXP-TITLE          PIC X(50).
               10  WS-EXP-COMPANY        PIC X(50).
               10  WS-EXP-DATES          PIC X(50).
               10  WS-EXP-DESC           PIC X(100).
       01  WS-TITLE-INPUT                PIC X(50).
       01  WS-COMPANY-INPUT              PIC X(50).
       01  WS-DATES-INPUT                PIC X(50).
       01  WS-DESC-INPUT                 PIC X(100).
               10  WS-EXP-TITLE          PIC X(50).
               10  WS-EXP-COMPANY        PIC X(50).
               10  WS-EXP-DATES          PIC X(50).
               10  WS-EXP-DESC           PIC X(100).
       01  WS-TITLE-INPUT                PIC X(50).
       01  WS-COMPANY-INPUT              PIC X(50).
       01  WS-DATES-INPUT                PIC X(50).
       01  WS-DESC-INPUT                 PIC X(100).

       01  MSG-ADD-EDUCATION             PIC X(90)
       01  MSG-ADD-EDUCATION             PIC X(90)
           VALUE "Add Education (optional, max 3 entries. Enter 'DONE' to finish):".
       01  WS-EDU-CHOICE                 PIC X(20).
       01  WS-EDU-CHOICE                 PIC X(20).
       01  WS-EDUCATION.
           05  WS-EDU-COUNT              PIC 9.
           05  WS-EDU-COUNT              PIC 9.
           05  WS-EDU-ENTRY OCCURS 3 TIMES.
               10  WS-EDU-DEGREE         PIC X(50).
               10  WS-EDU-SCHOOL         PIC X(50).
               10  WS-EDU-YEARS          PIC X(20).
       01  WS-DEGREE-INPUT               PIC X(50).
       01  WS-SCHOOL-INPUT               PIC X(50).
       01  WS-YEARS-INPUT                PIC X(20).
               10  WS-EDU-DEGREE         PIC X(50).
               10  WS-EDU-SCHOOL         PIC X(50).
               10  WS-EDU-YEARS          PIC X(20).
       01  WS-DEGREE-INPUT               PIC X(50).
       01  WS-SCHOOL-INPUT               PIC X(50).
       01  WS-YEARS-INPUT                PIC X(20).

       *> Search user
       01  MSG-ENTER-USER-SEARCH         PIC X(64)
       01  MSG-ENTER-USER-SEARCH         PIC X(64)
           VALUE "Enter the full name of the person you are looking for:".
       01  MSG-USER-NOT-FOUND            PIC X(64)
       01  MSG-USER-NOT-FOUND            PIC X(64)
           VALUE "No one by that name could be found.".
       01  MSG-USER-PROFILE-HEADER       PIC X(32)
       01  MSG-USER-PROFILE-HEADER       PIC X(32)
           VALUE "--- Found User Profile ---".
       01  WS-SEARCH-FULLNAME            PIC X(128) VALUE SPACES.
       01  WS-SEARCH-FOUND               PIC X VALUE 'N'.
           88  SEARCH-FOUND                  VALUE 'Y'.
           88  SEARCH-NOT-FOUND              VALUE 'N'.
       01  WS-SEARCH-FULLNAME            PIC X(128) VALUE SPACES.
       01  WS-SEARCH-FOUND               PIC X VALUE 'N'.
           88  SEARCH-FOUND                  VALUE 'Y'.
           88  SEARCH-NOT-FOUND              VALUE 'N'.

       *> Connection request messages/vars
       01  WS-CONN-CHOICE                PIC X(8)   VALUE SPACES.
       01  WS-FOUND-USER-USERNAME        PIC X(128) VALUE SPACES.
       01  WS-CONNECTION-STATUS-FLAG     PIC X(2)   VALUE SPACES.
           88  CONN-OK                       VALUE "OK".
           88  CONN-ALREADY-ACCEPTED         VALUE "AC".
           88  CONN-PENDING-BY-ME            VALUE "P1".
           88  CONN-PENDING-BY-THEM          VALUE "P2".
       01  MSG-SEND-REQUEST              PIC X(32)  VALUE "1. Send Connection Request".
       01  MSG-BACK-TO-MENU              PIC X(32)  VALUE "2. Back to Main Menu".
       01  MSG-ALREADY-CONNECTED         PIC X(64)  VALUE
       01  WS-CONN-CHOICE                PIC X(8)   VALUE SPACES.
       01  WS-FOUND-USER-USERNAME        PIC X(128) VALUE SPACES.
       01  WS-CONNECTION-STATUS-FLAG     PIC X(2)   VALUE SPACES.
           88  CONN-OK                       VALUE "OK".
           88  CONN-ALREADY-ACCEPTED         VALUE "AC".
           88  CONN-PENDING-BY-ME            VALUE "P1".
           88  CONN-PENDING-BY-THEM          VALUE "P2".
       01  MSG-SEND-REQUEST              PIC X(32)  VALUE "1. Send Connection Request".
       01  MSG-BACK-TO-MENU              PIC X(32)  VALUE "2. Back to Main Menu".
       01  MSG-ALREADY-CONNECTED         PIC X(64)  VALUE
           "You are already connected with this user.".
       01  MSG-PENDING-REQUEST-EXISTS    PIC X(80)  VALUE
       01  MSG-PENDING-REQUEST-EXISTS    PIC X(80)  VALUE
           "You have already sent a pending connection request to this user.".
       01  MSG-THEY-SENT-REQUEST         PIC X(80)  VALUE
       01  MSG-THEY-SENT-REQUEST         PIC X(80)  VALUE
           "This user has already sent you a connection request.".

       *> Pending requests view
       01  MSG-PENDING-HEADER            PIC X(64)
       01  MSG-PENDING-HEADER            PIC X(64)
           VALUE "--- Pending Connection Requests ---".
       01  MSG-NO-PENDING-REQUESTS       PIC X(64)
       01  MSG-NO-PENDING-REQUESTS       PIC X(64)
           VALUE "You have no pending connection requests at this time.".
       01  MSG-PENDING-LINE              PIC X(35)
       01  MSG-PENDING-LINE              PIC X(35)
           VALUE "-----------------------------------".
       01  MSG-ACCEPT-OPTION             PIC X(16) VALUE "1. Accept".
       01  MSG-REJECT-OPTION             PIC X(16) VALUE "2. Reject".
       01  MSG-INVALID-CHOICE-SKIP       PIC X(48)
       01  MSG-ACCEPT-OPTION             PIC X(16) VALUE "1. Accept".
       01  MSG-REJECT-OPTION             PIC X(16) VALUE "2. Reject".
       01  MSG-INVALID-CHOICE-SKIP       PIC X(48)
           VALUE "Invalid choice. Skipping request.".

       *> Network view
       01  MSG-NETWORK-HEADER            PIC X(32) VALUE "--- Your Network ---".
       01  MSG-NO-CONNECTIONS            PIC X(64)
       01  MSG-NETWORK-HEADER            PIC X(32) VALUE "--- Your Network ---".
       01  MSG-NO-CONNECTIONS            PIC X(64)
           VALUE "You have no connections in your network yet.".

       *> Request menu remnants
       01  MSG-REQUEST-MENU-1            PIC X(32) VALUE "1. Send Connection Request".
       01  MSG-REQUEST-MENU-2            PIC X(32) VALUE "2. Back to Main Menu".
       01  MSG-REQUEST-SENT              PIC X(64) VALUE "Connection request sent to".
       01  WS-REQUEST-CHOICE             PIC X(8)  VALUE SPACES.
       01  MSG-REQUEST-MENU-1            PIC X(32) VALUE "1. Send Connection Request".
       01  MSG-REQUEST-MENU-2            PIC X(32) VALUE "2. Back to Main Menu".
       01  MSG-REQUEST-SENT              PIC X(64) VALUE "Connection request sent to".
       01  WS-REQUEST-CHOICE             PIC X(8)  VALUE SPACES.

       *> EPIC 6: Jobs / Internships
       01  MSG-JOBS-HEADER               PIC X(40)
       01  MSG-JOBS-HEADER               PIC X(40)
           VALUE "--- Job Search/Internship Menu ---".
       01  MSG-JOBS-POST                 PIC X(32) VALUE "Post a Job/Internship".
       01  MSG-JOBS-BROWSE               PIC X(32) VALUE "Browse Jobs/Internships".
       01  MSG-JOBS-VIEW-APPS            PIC X(32) VALUE "View My Applications".
       01  MSG-JOBS-BACK                 PIC X(32) VALUE "Back to Main Menu".
       01  MSG-JOBS-POST                 PIC X(32) VALUE "Post a Job/Internship".
       01  MSG-JOBS-BROWSE               PIC X(32) VALUE "Browse Jobs/Internships".
       01  MSG-JOBS-VIEW-APPS            PIC X(32) VALUE "View My Applications".
       01  MSG-JOBS-BACK                 PIC X(32) VALUE "Back to Main Menu".

       01  MSG-POST-JOB-HEADER           PIC X(40) VALUE "--- Post a New Job/Internship ---".
       01  MSG-POST-JOB-TITLE            PIC X(32) VALUE "Enter Job Title:".
       01  MSG-POST-JOB-DESC             PIC X(40) VALUE "Enter Description (max 200 chars):".
       01  MSG-POST-JOB-EMPLOYER         PIC X(32) VALUE "Enter Employer Name:".
       01  MSG-POST-JOB-LOCATION         PIC X(32) VALUE "Enter Location:".
       01  MSG-POST-JOB-SALARY           PIC X(48)
       01  MSG-POST-JOB-HEADER           PIC X(40) VALUE "--- Post a New Job/Internship ---".
       01  MSG-POST-JOB-TITLE            PIC X(32) VALUE "Enter Job Title:".
       01  MSG-POST-JOB-DESC             PIC X(40) VALUE "Enter Description (max 200 chars):".
       01  MSG-POST-JOB-EMPLOYER         PIC X(32) VALUE "Enter Employer Name:".
       01  MSG-POST-JOB-LOCATION         PIC X(32) VALUE "Enter Location:".
       01  MSG-POST-JOB-SALARY           PIC X(48)
           VALUE "Enter Salary (optional, enter 'NONE' to skip):".
       01  MSG-POST-SUCCESS              PIC X(32) VALUE "Job posted successfully!".
       01  MSG-SEPARATOR-LINE            PIC X(40) VALUE "----------------------------------".
       01  MSG-POST-SUCCESS              PIC X(32) VALUE "Job posted successfully!".
       01  MSG-SEPARATOR-LINE            PIC X(40) VALUE "----------------------------------".

       *> Browse/details
       01  MSG-JOBS-LIST-HEADER          PIC X(40) VALUE "--- Available Jobs Listings ---".
       01  MSG-NO-JOBS                   PIC X(40) VALUE "No jobs/internships available.".
       01  MSG-ENTER-JOB                 PIC X(80) VALUE "Enter job number to view details, or 0 to go back:".
       01  MSG-INVALID-JOB               PIC X(32) VALUE "Invalid job selection.".
       01  MSG-JOB-DETAILS-HEADER        PIC X(24) VALUE "--- Job Details ---".
       01  MSG-JOB-DETAILS-DIVIDER       PIC X(40) VALUE "-------------------".
       01  MSG-APPLY-OPT                 PIC X(24) VALUE "Apply for this Job".
       01  MSG-BACK-OPT                  PIC X(24) VALUE "Back to Job List".
       01  MSG-APPLY-SUCCESS             PIC X(64) VALUE "Your application for ".
       01  MSG-APPLY-DUPLICATE           PIC X(64) VALUE "You have already applied for this job.".
       01  MSG-JOBS-LIST-HEADER          PIC X(40) VALUE "--- Available Jobs Listings ---".
       01  MSG-NO-JOBS                   PIC X(40) VALUE "No jobs/internships available.".
       01  MSG-ENTER-JOB                 PIC X(80) VALUE "Enter job number to view details, or 0 to go back:".
       01  MSG-INVALID-JOB               PIC X(32) VALUE "Invalid job selection.".
       01  MSG-JOB-DETAILS-HEADER        PIC X(24) VALUE "--- Job Details ---".
       01  MSG-JOB-DETAILS-DIVIDER       PIC X(40) VALUE "-------------------".
       01  MSG-APPLY-OPT                 PIC X(24) VALUE "Apply for this Job".
       01  MSG-BACK-OPT                  PIC X(24) VALUE "Back to Job List".
       01  MSG-APPLY-SUCCESS             PIC X(64) VALUE "Your application for ".
       01  MSG-APPLY-DUPLICATE           PIC X(64) VALUE "You have already applied for this job.".

       *> EPIC 7: View Applications messages
       01  MSG-APPS-HEADER               PIC X(32) VALUE "--- Your Job Applications ---".
       01  MSG-APPS-USER-SUMMARY         PIC X(32) VALUE "Application Summary for ".
       01  MSG-APPS-SEP-TOP              PIC X(32) VALUE "------------------------------".
       01  MSG-APPS-SEP-ITEM             PIC X(16) VALUE "---".
       01  MSG-APPS-SEP-FOOTER           PIC X(32) VALUE "------------------------------".
       01  MSG-APPS-TOTAL                PIC X(20) VALUE "Total Applications: ".
       01  MSG-NO-APPS-FOUND             PIC X(40) VALUE "You have not applied to any jobs yet.".
       01  MSG-APPS-HEADER               PIC X(32) VALUE "--- Your Job Applications ---".
       01  MSG-APPS-USER-SUMMARY         PIC X(32) VALUE "Application Summary for ".
       01  MSG-APPS-SEP-TOP              PIC X(32) VALUE "------------------------------".
       01  MSG-APPS-SEP-ITEM             PIC X(16) VALUE "---".
       01  MSG-APPS-SEP-FOOTER           PIC X(32) VALUE "------------------------------".
       01  MSG-APPS-TOTAL                PIC X(20) VALUE "Total Applications: ".
       01  MSG-NO-APPS-FOUND             PIC X(40) VALUE "You have not applied to any jobs yet.".

       01  WS-BROWSE-CHOICE              PIC X(8)  VALUE SPACES.
       77  WS-SEL-NUM                    PIC 9(6)  VALUE 0.
       77  WS-IDX-DISPLAY                PIC Z(3)9 VALUE ZERO.
       77  WS-SALARY-TRIM                PIC X(128) VALUE SPACES.
       01  WS-BROWSE-CHOICE              PIC X(8)  VALUE SPACES.
       77  WS-SEL-NUM                    PIC 9(6)  VALUE 0.
       77  WS-IDX-DISPLAY                PIC Z(3)9 VALUE ZERO.
       77  WS-SALARY-TRIM                PIC X(128) VALUE SPACES.

       *> Test mode flag
       01  WS-TEST-MODE                  PIC X VALUE 'N'.
           88  TEST-MODE-ON                  VALUE 'Y'.
           88  TEST-MODE-OFF                 VALUE 'N'.

       01  WS-TEST-MODE                  PIC X VALUE 'N'.
           88  TEST-MODE-ON                  VALUE 'Y'.
           88  TEST-MODE-OFF                 VALUE 'N'.

       *> EPIC 8: Send/Receive Messages
       01  MSG-MESSAGES-HEADER           PIC X(21) VALUE "--- Messages Menu ---".
       01  MSG-MESSAGES-FOOTER           PIC X(32) VALUE "---------------------".
       01  MSG-MESSAGES-SEND             PIC X(22) VALUE "1. Send a New Message".
       01  MSG-MESSAGES-VIEW             PIC X(21) VALUE "2. View My Messages".
       01  MSG-MESSAGES-BACK             PIC X(22) VALUE "3. Back to Main Menu".

       01  MSG-ENTER-RECEIVER            PIC X(64) VALUE "Enter recipient's username (must be a connection):".
       01  MSG-ENTER-CONTENT             PIC X(64) VALUE "Enter your message (max 200 chars):".
       01  MSG-SEND-SUCCESS-1            PIC X(16) VALUE "Message sent to ".
       01  MSG-SEND-SUCCESS-2            PIC X(16) VALUE " successfully!".
       01  MSG-MESSAGES-HEADER           PIC X(21) VALUE "--- Messages Menu ---".
       01  MSG-MESSAGES-FOOTER           PIC X(32) VALUE "---------------------".
       01  MSG-MESSAGES-SEND             PIC X(22) VALUE "1. Send a New Message".
       01  MSG-MESSAGES-VIEW             PIC X(21) VALUE "2. View My Messages".
       01  MSG-MESSAGES-BACK             PIC X(22) VALUE "3. Back to Main Menu".

       01  MSG-ENTER-RECEIVER            PIC X(64) VALUE "Enter recipient's username (must be a connection):".
       01  MSG-ENTER-CONTENT             PIC X(64) VALUE "Enter your message (max 200 chars):".
       01  MSG-SEND-SUCCESS-1            PIC X(16) VALUE "Message sent to ".
       01  MSG-SEND-SUCCESS-2            PIC X(16) VALUE " successfully!".

       01  MSG-NOT-CONNECTED             PIC X(32) VALUE "User not found in your network.".
       01  MSG-VIEW-CONSTRUCTION         PIC X(100) VALUE "View My Messages is under construction.".

       01  WS-MESSAGE-CHOICE             PIC X(8) VALUE SPACES.
       77  WS-RECEIVER                   PIC X(128) VALUE SPACES.
       77  WS-CONTENT                    PIC X(256) VALUE SPACES.
       77  WS-CONTENT-LENGTH             PIC 9(4)   VALUE 0.

       *> EPIC 9: View Messages
       *> Added for Week 9
       01  MSG-MESSAGES-VIEW-HEADER      PIC X(22) VALUE "--- Your Messages ---".
       01  MSG-BLANK-LINE                PIC X(1)  VALUE SPACES.
       01  MSG-NO-MESSAGES               PIC X(40) VALUE "You have no messages at this time.".
       01  MSG-VIEW-FROM                 PIC X(8)  VALUE "From: ".
       01  MSG-VIEW-CONTENT              PIC X(10) VALUE "Message: ".

       01  WS-MESSAGES-FOUND-FLAG        PIC X     VALUE 'N'.
           88  MESSAGES-FOUND                    VALUE 'Y'.
           88  MESSAGES-NOT-FOUND                VALUE 'N'.

        *> Format for timestamp into YYYY-MM-DD HH:MM
       77 WS-FORMATTED-TS           PIC X(20) VALUE SPACES.
       77  WS-TS-YEAR                PIC X(4)  VALUE SPACES.
       77  WS-TS-MONTH               PIC X(2)  VALUE SPACES.
       77  WS-TS-DAY                 PIC X(2)  VALUE SPACES.
       77  WS-TS-HOUR                PIC X(2)  VALUE SPACES.
       77  WS-TS-MINUTE              PIC X(2)  VALUE SPACES.
       01  MSG-NOT-CONNECTED             PIC X(32) VALUE "User not found in your network.".
       01  MSG-VIEW-CONSTRUCTION         PIC X(100) VALUE "View My Messages is under construction.".

       01  WS-MESSAGE-CHOICE             PIC X(8) VALUE SPACES.
       77  WS-RECEIVER                   PIC X(128) VALUE SPACES.
       77  WS-CONTENT                    PIC X(256) VALUE SPACES.
       77  WS-CONTENT-LENGTH             PIC 9(4)   VALUE 0.

       *> EPIC 9: View Messages
       *> Added for Week 9
       01  MSG-MESSAGES-VIEW-HEADER      PIC X(22) VALUE "--- Your Messages ---".
       01  MSG-BLANK-LINE                PIC X(1)  VALUE SPACES.
       01  MSG-NO-MESSAGES               PIC X(40) VALUE "You have no messages at this time.".
       01  MSG-VIEW-FROM                 PIC X(8)  VALUE "From: ".
       01  MSG-VIEW-CONTENT              PIC X(10) VALUE "Message: ".

       01  WS-MESSAGES-FOUND-FLAG        PIC X     VALUE 'N'.
           88  MESSAGES-FOUND                    VALUE 'Y'.
           88  MESSAGES-NOT-FOUND                VALUE 'N'.

        *> Format for timestamp into YYYY-MM-DD HH:MM
       77 WS-FORMATTED-TS           PIC X(20) VALUE SPACES.
       77  WS-TS-YEAR                PIC X(4)  VALUE SPACES.
       77  WS-TS-MONTH               PIC X(2)  VALUE SPACES.
       77  WS-TS-DAY                 PIC X(2)  VALUE SPACES.
       77  WS-TS-HOUR                PIC X(2)  VALUE SPACES.
       77  WS-TS-MINUTE              PIC X(2)  VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM INIT-FILES
           PERFORM RUN-APP
           MOVE MSG-END-OF-PROGRAM TO WS-MSG
           PERFORM DISPLAY-AND-LOG
           PERFORM CLOSE-FILES
           GOBACK.

       INITIALIZATION-SECTION.
       INIT-FILES.
           *> File-driven only: open input and output files; create/overwrite output.
           OPEN INPUT  INPUT-FILE
                OUTPUT OUTPUT-FILE
           .

           *> Load users from file into memory (optional if file missing)
           PERFORM INIT-LOAD-ACCOUNTS
           PERFORM INIT-LOAD-PROFILES
           *> New: Load connections
           PERFORM INIT-LOAD-CONNECTIONS
           *> Epic 6: Load job data
           PERFORM INIT-LOAD-JOBS
           *> Epic 7: Load applications
           PERFORM INIT-LOAD-APPLICATIONS
           *> Epic 8: Load messages
           PERFORM INIT-LOAD-MESSAGES

           EXIT.


       CLOSE-FILES.
           CLOSE INPUT-FILE OUTPUT-FILE
           EXIT.

       MENU-SECTION.
       RUN-APP.
           MOVE MSG-WELCOME       TO WS-MSG PERFORM DISPLAY-AND-LOG
           MOVE MSG-LOGIN         TO WS-MSG PERFORM DISPLAY-AND-LOG
           MOVE MSG-CREATE        TO WS-MSG PERFORM DISPLAY-AND-LOG
           MOVE MSG-ENTER-CHOICE  TO WS-MSG PERFORM DISPLAY-AND-LOG

           PERFORM READ-NEXT-LINE
           MOVE WS-LINE TO WS-CHOICE
           IF EOF-IN
              EXIT PARAGRAPH
           END-IF

           EVALUATE WS-CHOICE
              WHEN '1'
              WHEN '1'
               PERFORM LOGIN
              WHEN '2'
              WHEN '2'
               PERFORM CREATE-ACCOUNT
              WHEN 'TEST-JOBS'
              WHEN 'TEST-JOBS'
               PERFORM UNIT-TESTS-JOBS
              WHEN OTHER
              WHEN OTHER
               MOVE MSG-INVALID-CHOICE TO WS-MSG PERFORM DISPLAY-AND-LOG
           END-EVALUATE
           EXIT.

       LOGIN-SECTION.
       LOGIN.
           PERFORM RESET-LOGIN-STATE
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
                MOVE SPACES TO WS-MSG
                STRING
                   MSG-WELCOME-PFX         DELIMITED BY SIZE
                   FUNCTION TRIM(WS-USERNAME)  DELIMITED BY SIZE
                   "!"                       DELIMITED BY SIZE
                   INTO WS-MSG
                   MSG-WELCOME-PFX         DELIMITED BY SIZE
                   FUNCTION TRIM(WS-USERNAME)  DELIMITED BY SIZE
                   "!"                       DELIMITED BY SIZE
                   INTO WS-MSG
                END-STRING
                PERFORM DISPLAY-AND-LOG
                MOVE FUNCTION TRIM(WS-USERNAME) TO WS-CURRENT-USERNAME
                PERFORM LOGGED-IN-MENU
                EXIT PERFORM
             ELSE
                MOVE MSG-FAILURE TO WS-MSG
                PERFORM DISPLAY-AND-LOG
                PERFORM RESET-LOGIN-STATE
             END-IF
           END-PERFORM
           EXIT.

       RESET-LOGIN-STATE.
           SET MATCH-NOT-FOUND TO TRUE
           MOVE SPACES TO WS-USERNAME WS-PASSWORD
           EXIT.

       CREATE-ACCOUNT.
           IF WS-USERS-COUNT >= WS-ACCOUNT-LIMIT
               MOVE MSG-ACCOUNT-LIMIT TO WS-MSG PERFORM DISPLAY-AND-LOG
               EXIT PARAGRAPH
           END-IF

           *> Username prompt (with uniqueness)
           PERFORM UNTIL (WS-NEW-USERNAME NOT = SPACES AND MATCH-NOT-FOUND) OR EOF-IN
               MOVE MSG-ENTER-NEW-USER TO WS-MSG PERFORM DISPLAY-AND-LOG
               PERFORM READ-NEXT-LINE
               MOVE WS-LINE TO WS-NEW-USERNAME
               IF EOF-IN
                   EXIT PARAGRAPH
               END-IF

               SET MATCH-NOT-FOUND TO TRUE
               PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-USERS-COUNT OR MATCH-FOUND
                   IF WS-NEW-USERNAME = WS-TBL-USERNAME(WS-I)
                       SET MATCH-FOUND TO TRUE
                   END-IF
               END-PERFORM

               IF MATCH-FOUND
                   MOVE MSG-USERNAME-EXISTS TO WS-MSG PERFORM DISPLAY-AND-LOG
                   MOVE SPACES TO WS-NEW-USERNAME
               END-IF
           END-PERFORM

           *> Password prompt + validation
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

           IF WS-NEW-PASSWORD = SPACES
               EXIT PARAGRAPH
           END-IF

           ADD 1 TO WS-USERS-COUNT
           MOVE WS-NEW-USERNAME TO WS-TBL-USERNAME(WS-USERS-COUNT)
           MOVE WS-NEW-PASSWORD TO WS-TBL-PASSWORD(WS-USERS-COUNT)

           OPEN EXTEND USERS-FILE
           MOVE SPACES TO USER-REC
           STRING
               FUNCTION TRIM(WS-NEW-USERNAME) DELIMITED BY SIZE
               "|"                            DELIMITED BY SIZE
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

           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-NEW-PASSWORD)) TO WS-PASS-LEN
           IF WS-PASS-LEN < 8 OR WS-PASS-LEN > 12
               SET PASS-INVALID TO TRUE
               MOVE "Password must be 8 to 12 characters."
                   TO WS-PASSWORD-ERROR
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-UPPER-COUNT WS-DIGIT-COUNT WS-SPECIAL-COUNT
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > FUNCTION LENGTH(FUNCTION TRIM(WS-NEW-PASSWORD))
               MOVE WS-NEW-PASSWORD(WS-I:1) TO WS-CHAR
               IF WS-CHAR >= 'A' AND WS-CHAR = 'Z'
                   ADD 1 TO WS-UPPER-COUNT
               END-IF
               IF WS-CHAR = '0' AND WS-CHAR <= '9'
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
               MOVE "Password must contain at least one capital letter."
                   TO WS-PASSWORD-ERROR
               EXIT PARAGRAPH
           END-IF

           IF WS-DIGIT-COUNT = 0
               SET PASS-INVALID TO TRUE
               MOVE "Password must contain at least one digit."
                   TO WS-PASSWORD-ERROR
               EXIT PARAGRAPH
           END-IF

           IF WS-SPECIAL-COUNT = 0
               SET PASS-INVALID TO TRUE
               MOVE "Password must contain at least one special character: !@#$%^&*?-_+"
                   TO WS-PASSWORD-ERROR
               EXIT PARAGRAPH
           END-IF

           EXIT.

       LOGGED-IN-SECTION.
       LOGGED-IN-MENU.
           PERFORM UNTIL EOF-IN

               MOVE MSG-MENU-VIEW-PROFILE TO WS-MSG PERFORM DISPLAY-AND-LOG
       *>        MOVE MSG-MENU-JOBS         TO WS-MSG PERFORM DISPLAY-AND-LOG
       *>        MOVE MSG-MENU-JOBS         TO WS-MSG PERFORM DISPLAY-AND-LOG
               MOVE MSG-MENU-SEARCH-USER  TO WS-MSG PERFORM DISPLAY-AND-LOG
               MOVE MSG-MENU-LEARN-SKILL  TO WS-MSG PERFORM DISPLAY-AND-LOG
               MOVE MSG-MENU-VIEW-PENDING TO WS-MSG PERFORM DISPLAY-AND-LOG
               MOVE MSG-MENU-VIEW-NETWORK TO WS-MSG PERFORM DISPLAY-AND-LOG
               MOVE MSG-MENU-MESSAGE      TO WS-MSG PERFORM DISPLAY-AND-LOG
               MOVE MSG-ENTER-CHOICE      TO WS-MSG PERFORM DISPLAY-AND-LOG
               MOVE MSG-MENU-MESSAGE      TO WS-MSG PERFORM DISPLAY-AND-LOG
               MOVE MSG-ENTER-CHOICE      TO WS-MSG PERFORM DISPLAY-AND-LOG

               PERFORM READ-NEXT-LINE
               MOVE WS-LINE TO WS-LOGGED-CHOICE
               IF EOF-IN
                   EXIT PERFORM
               END-IF

               EVALUATE WS-LOGGED-CHOICE
       *>            WHEN '1'  PERFORM JOBS-MENU
       *>            WHEN '1'  PERFORM JOBS-MENU
                   WHEN '1'  PERFORM VIEW-MY-PROFILE
                   WHEN '2'  PERFORM USER-SEARCH-MENU
                   WHEN '3'  PERFORM SKILL-MENU
                   WHEN '4'  PERFORM VIEW-PENDING-REQUESTS
                   WHEN '5'  PERFORM VIEW-MY-NETWORK
                   WHEN '6'  PERFORM MESSAGE-MENU
                   WHEN OTHER
                       MOVE MSG-INVALID-CHOICE TO WS-MSG PERFORM DISPLAY-AND-LOG
               END-EVALUATE
           END-PERFORM
           EXIT.

       SKILL-MENU.
           PERFORM UNTIL WS-SKILL-CHOICE = '6' OR EOF-IN
               MOVE MSG-MENU-LEARN-SKILL TO WS-MSG PERFORM DISPLAY-AND-LOG
               MOVE MSG-SKILL1 TO WS-MSG PERFORM DISPLAY-AND-LOG
               MOVE MSG-SKILL2 TO WS-MSG PERFORM DISPLAY-AND-LOG
               MOVE MSG-SKILL3 TO WS-MSG PERFORM DISPLAY-AND-LOG
               MOVE MSG-SKILL4 TO WS-MSG PERFORM DISPLAY-AND-LOG
               MOVE MSG-SKILL5 TO WS-MSG PERFORM DISPLAY-AND-LOG
               MOVE MSG-SKILL6 TO WS-MSG PERFORM DISPLAY-AND-LOG
               MOVE MSG-ENTER-CHOICE TO WS-MSG PERFORM DISPLAY-AND-LOG

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
           MOVE MSG-ENTER-USER-SEARCH TO WS-MSG PERFORM DISPLAY-AND-LOG
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
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-PROFILES-COUNT OR SEARCH-FOUND
               MOVE SPACES TO WS-T1
               STRING
                   FUNCTION TRIM(WS-PROF-FIRST(WS-I)) DELIMITED BY SIZE
                   " "                                DELIMITED BY SIZE
                   " "                                DELIMITED BY SIZE
                   FUNCTION TRIM(WS-PROF-LAST(WS-I))  DELIMITED BY SIZE
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

           *> Prompt to connect (not self)
           MOVE WS-PROF-USERNAME(WS-SEARCH-RESULT-IDX)
               TO WS-FOUND-USER-USERNAME
           IF WS-FOUND-USER-USERNAME NOT = WS-CURRENT-USERNAME
              AND NOT EOF-IN
               PERFORM PROMPT-FOR-CONNECTION
           END-IF
           EXIT.

       DISPLAY-PROFILE-BY-ID.
           IF WS-I < 1 OR WS-I > WS-PROFILES-COUNT
               MOVE "Invalid profile ID." TO WS-MSG PERFORM DISPLAY-AND-LOG
               EXIT PARAGRAPH
           END-IF

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
           STRING "Name: "                         DELIMITED BY SIZE
                  FUNCTION TRIM(WS-PROF-FIRST-IN)  DELIMITED BY SIZE
                  " "                              DELIMITED BY SIZE
                  FUNCTION TRIM(WS-PROF-LAST-IN)   DELIMITED BY SIZE
           STRING "Name: "                         DELIMITED BY SIZE
                  FUNCTION TRIM(WS-PROF-FIRST-IN)  DELIMITED BY SIZE
                  " "                              DELIMITED BY SIZE
                  FUNCTION TRIM(WS-PROF-LAST-IN)   DELIMITED BY SIZE
                  INTO WS-MSG
           END-STRING
           PERFORM DISPLAY-AND-LOG

           MOVE SPACES TO WS-MSG
           STRING "University: " FUNCTION TRIM(WS-PROF-UNIV-IN) DELIMITED BY SIZE
                  INTO WS-MSG
           END-STRING
           PERFORM DISPLAY-AND-LOG

           MOVE SPACES TO WS-MSG
           STRING "Major: " FUNCTION TRIM(WS-PROF-MAJOR-IN) DELIMITED BY SIZE
                  INTO WS-MSG
           END-STRING
           PERFORM DISPLAY-AND-LOG

           MOVE SPACES TO WS-MSG
           STRING "Graduation Year: " FUNCTION TRIM(WS-PROF-GYEAR-IN) DELIMITED BY SIZE
                  INTO WS-MSG
           END-STRING
           PERFORM DISPLAY-AND-LOG

           MOVE SPACES TO WS-MSG
           STRING "About Me: " FUNCTION TRIM(WS-PROF-ABOUT-IN) DELIMITED BY SIZE
                  INTO WS-MSG
           END-STRING
           PERFORM DISPLAY-AND-LOG

           PERFORM DISPLAY-EXPERIENCES
           PERFORM DISPLAY-EDUCATION

           MOVE MSG-LINE-LONG TO WS-MSG PERFORM DISPLAY-AND-LOG
           EXIT PARAGRAPH.

       DISPLAY-NO-MATCH-MSG.
           MOVE MSG-USER-NOT-FOUND TO WS-MSG PERFORM DISPLAY-AND-LOG
           EXIT.

       VALIDATION-SECTION.
       CHECK-CREDENTIALS.
           SET MATCH-NOT-FOUND TO TRUE
           IF WS-USERS-COUNT = 0
              EXIT PARAGRAPH
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-USERS-COUNT OR MATCH-FOUND
             IF WS-USERNAME = WS-TBL-USERNAME(WS-I)
                AND WS-PASSWORD = WS-TBL-PASSWORD(WS-I)
                SET MATCH-FOUND TO TRUE
             END-IF
           END-PERFORM
           EXIT.

       PARSING-SECTION.
       PARSE-USER-REC.
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
           OPEN INPUT USERS-FILE
           IF WS-USR-STATUS = "00"
             PERFORM LOAD-ACCOUNTS-FROM-USERS
             CLOSE USERS-FILE
           END-IF
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
                   PERFORM PARSE-USER-REC
                   IF WS-USER-FILE-USERNAME NOT = SPACES
                      AND WS-USER-FILE-PASSWORD NOT = SPACES
                      IF WS-USERS-COUNT < WS-ACCOUNT-LIMIT
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
                   IF WS-USER-FILE-USERNAME NOT = SPACES
                      AND WS-USER-FILE-PASSWORD NOT = SPACES
                      IF WS-USERS-COUNT < WS-ACCOUNT-LIMIT
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
               STRING
                   FUNCTION TRIM(WS-PROF-USERNAME(WS-I))    DELIMITED BY SIZE
                   "|"                                    DELIMITED BY SIZE
                   FUNCTION TRIM(WS-PROF-FIRST(WS-I))     DELIMITED BY SIZE
                   "|"                                    DELIMITED BY SIZE
                   FUNCTION TRIM(WS-PROF-LAST(WS-I))      DELIMITED BY SIZE
                   "|"                                    DELIMITED BY SIZE
                   FUNCTION TRIM(WS-PROF-UNIV(WS-I))      DELIMITED BY SIZE
                   "|"                                    DELIMITED BY SIZE
                   FUNCTION TRIM(WS-PROF-MAJOR(WS-I))     DELIMITED BY SIZE
                   "|"                                    DELIMITED BY SIZE
                   FUNCTION TRIM(WS-PROF-GYEAR(WS-I))     DELIMITED BY SIZE
                   "|"                                    DELIMITED BY SIZE
                   FUNCTION TRIM(WS-PROF-ABOUT(WS-I))     DELIMITED BY SIZE
                   "|"                                    DELIMITED BY SIZE
                   FUNCTION TRIM(WS-PROF-USERNAME(WS-I))    DELIMITED BY SIZE
                   "|"                                    DELIMITED BY SIZE
                   FUNCTION TRIM(WS-PROF-FIRST(WS-I))     DELIMITED BY SIZE
                   "|"                                    DELIMITED BY SIZE
                   FUNCTION TRIM(WS-PROF-LAST(WS-I))      DELIMITED BY SIZE
                   "|"                                    DELIMITED BY SIZE
                   FUNCTION TRIM(WS-PROF-UNIV(WS-I))      DELIMITED BY SIZE
                   "|"                                    DELIMITED BY SIZE
                   FUNCTION TRIM(WS-PROF-MAJOR(WS-I))     DELIMITED BY SIZE
                   "|"                                    DELIMITED BY SIZE
                   FUNCTION TRIM(WS-PROF-GYEAR(WS-I))     DELIMITED BY SIZE
                   "|"                                    DELIMITED BY SIZE
                   FUNCTION TRIM(WS-PROF-ABOUT(WS-I))     DELIMITED BY SIZE
                   "|"                                    DELIMITED BY SIZE
                   FUNCTION TRIM(WS-PROF-EXPERIENCES(WS-I)) DELIMITED BY SIZE
                   "|"                                    DELIMITED BY SIZE
                   "|"                                    DELIMITED BY SIZE
                   FUNCTION TRIM(WS-PROF-EDUCATIONS(WS-I)) DELIMITED BY SIZE
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
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-PROFILES-COUNT OR PROFILE-FOUND
              IF FUNCTION TRIM(WS-CURRENT-USERNAME)
                   = FUNCTION TRIM(WS-PROF-USERNAME(WS-I))
                   = FUNCTION TRIM(WS-PROF-USERNAME(WS-I))
                 SET PROFILE-FOUND TO TRUE
                 MOVE WS-I TO WS-PROFILE-IDX
              END-IF
           END-PERFORM
           EXIT.

       VALIDATE-GRAD-YEAR.
           MOVE FUNCTION TRIM(WS-PROF-GYEAR-IN) TO WS-PROF-GYEAR-IN
           SET YEAR-VALID TO TRUE
           IF FUNCTION LENGTH(FUNCTION TRIM(WS-PROF-GYEAR-IN)) NOT = 4
              SET YEAR-INVALID TO TRUE
              EXIT PARAGRAPH
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
              SET YEAR-INVALID TO TRUE
           END-IF
           EXIT.

       *> ===============================================================
       *> CONNECTION HANDLING SECTION
       *> ===============================================================
       CONNECTION-HANDLING-SECTION.
       PROMPT-FOR-CONNECTION.
           MOVE MSG-SEND-REQUEST TO WS-MSG PERFORM DISPLAY-AND-LOG
           MOVE MSG-BACK-TO-MENU TO WS-MSG PERFORM DISPLAY-AND-LOG
           MOVE MSG-ENTER-CHOICE TO WS-MSG PERFORM DISPLAY-AND-LOG

           PERFORM READ-NEXT-LINE
           MOVE WS-LINE TO WS-CONN-CHOICE
           IF EOF-IN
               EXIT PARAGRAPH
           END-IF

           EVALUATE WS-CONN-CHOICE
               WHEN '1'  PERFORM PROCESS-CONNECTION-REQUEST
               WHEN '2'  CONTINUE
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
                   MOVE WS-SEARCH-RESULT-IDX TO WS-I
                   MOVE SPACES TO WS-MSG
                   STRING
                       "Connection request sent to "     DELIMITED BY SIZE
                       "Connection request sent to "     DELIMITED BY SIZE
                       FUNCTION TRIM(WS-PROF-FIRST(WS-I))  DELIMITED BY SIZE
                       " "                               DELIMITED BY SIZE
                       " "                               DELIMITED BY SIZE
                       FUNCTION TRIM(WS-PROF-LAST(WS-I))   DELIMITED BY SIZE
                       "."                               DELIMITED BY SIZE
                       "."                               DELIMITED BY SIZE
                       INTO WS-MSG
                   END-STRING
                   PERFORM DISPLAY-AND-LOG
           END-EVALUATE
           EXIT.

       CHECK-CONNECTION-STATUS.
           SET CONN-OK TO TRUE
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-CONNECTIONS-COUNT
               IF WS-CONN-SENDER(WS-I)   = WS-CURRENT-USERNAME AND
                  WS-CONN-RECEIVER(WS-I) = WS-FOUND-USER-USERNAME
                   IF WS-CONN-STATUS(WS-I) = 'A'
                       SET CONN-ALREADY-ACCEPTED TO TRUE
                   ELSE
                       SET CONN-PENDING-BY-ME TO TRUE
                   END-IF
                   EXIT PERFORM
               END-IF
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
           MOVE WS-CURRENT-USERNAME    TO WS-CONN-SENDER(WS-CONNECTIONS-COUNT)
           MOVE WS-FOUND-USER-USERNAME TO WS-CONN-RECEIVER(WS-CONNECTIONS-COUNT)
           MOVE 'P'                    TO WS-CONN-STATUS(WS-CONNECTIONS-COUNT)
           EXIT.

       *> View and act on pending requests
       VIEW-PENDING-REQUESTS.
           MOVE MSG-PENDING-HEADER TO WS-MSG PERFORM DISPLAY-AND-LOG
           MOVE 0 TO WS-TMP-COUNT

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-CONNECTIONS-COUNT
               IF WS-CONN-RECEIVER(WS-I) = WS-CURRENT-USERNAME AND
                  WS-CONN-STATUS(WS-I) = 'P'
                   ADD 1 TO WS-TMP-COUNT
                   MOVE WS-CONN-SENDER(WS-I) TO WS-TARGET-USERNAME
                   PERFORM GET-FULL-NAME

                   MOVE SPACES TO WS-MSG
                   STRING
                       "Connection request from "     DELIMITED BY SIZE
                       FUNCTION TRIM(WS-DISPLAY-NAME) DELIMITED BY SIZE
                       INTO WS-MSG
                   END-STRING
                   PERFORM DISPLAY-AND-LOG

                   MOVE MSG-ACCEPT-OPTION TO WS-MSG PERFORM DISPLAY-AND-LOG
                   MOVE MSG-REJECT-OPTION TO WS-MSG PERFORM DISPLAY-AND-LOG

                   PERFORM READ-NEXT-LINE

                   IF WS-LINE = "1"
                       PERFORM ACCEPT-CONNECTION
                   ELSE
                       IF WS-LINE = "2"
                           PERFORM REJECT-CONNECTION
                       ELSE
                           MOVE MSG-INVALID-CHOICE-SKIP TO WS-MSG
                           PERFORM DISPLAY-AND-LOG
                       END-IF
                   END-IF
               END-IF
           END-PERFORM

           IF WS-TMP-COUNT = 0
               MOVE MSG-NO-PENDING-REQUESTS TO WS-MSG
               PERFORM DISPLAY-AND-LOG
           END-IF

           MOVE MSG-PENDING-LINE TO WS-MSG PERFORM DISPLAY-AND-LOG
           EXIT.

       VIEW-MY-NETWORK.
           MOVE MSG-NETWORK-HEADER TO WS-MSG PERFORM DISPLAY-AND-LOG
           MOVE 0 TO WS-TMP-COUNT

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-CONNECTIONS-COUNT
               IF WS-CONN-STATUS(WS-I) = 'A'
                   INITIALIZE WS-TARGET-USERNAME
                   IF WS-CONN-SENDER(WS-I) = WS-CURRENT-USERNAME
                       MOVE WS-CONN-RECEIVER(WS-I) TO WS-TARGET-USERNAME
                   ELSE
                       IF WS-CONN-RECEIVER(WS-I) = WS-CURRENT-USERNAME
                           MOVE WS-CONN-SENDER(WS-I) TO WS-TARGET-USERNAME
                       END-IF
                   END-IF

                   IF WS-TARGET-USERNAME NOT = SPACES
                       ADD 1 TO WS-TMP-COUNT
                       SET PROFILE-NOT-FOUND TO TRUE

                       PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > WS-PROFILES-COUNT
                           IF WS-PROF-USERNAME(WS-J) = WS-TARGET-USERNAME
                               SET PROFILE-FOUND TO TRUE
                               MOVE SPACES TO WS-MSG
                               STRING
                                   "Connected with: "                DELIMITED BY SIZE
                                   FUNCTION TRIM(WS-PROF-FIRST(WS-J))  DELIMITED BY SIZE
                                   " "                               DELIMITED BY SIZE
                                   FUNCTION TRIM(WS-PROF-LAST(WS-J))   DELIMITED BY SIZE
                                   " (University: "                  DELIMITED BY SIZE
                                   FUNCTION TRIM(WS-PROF-UNIV(WS-J))   DELIMITED BY SIZE
                                   ", Major: "                       DELIMITED BY SIZE
                                   FUNCTION TRIM(WS-PROF-MAJOR(WS-J))  DELIMITED BY SIZE
                                   ")"                               DELIMITED BY SIZE
                                   "Connected with: "                DELIMITED BY SIZE
                                   FUNCTION TRIM(WS-PROF-FIRST(WS-J))  DELIMITED BY SIZE
                                   " "                               DELIMITED BY SIZE
                                   FUNCTION TRIM(WS-PROF-LAST(WS-J))   DELIMITED BY SIZE
                                   " (University: "                  DELIMITED BY SIZE
                                   FUNCTION TRIM(WS-PROF-UNIV(WS-J))   DELIMITED BY SIZE
                                   ", Major: "                       DELIMITED BY SIZE
                                   FUNCTION TRIM(WS-PROF-MAJOR(WS-J))  DELIMITED BY SIZE
                                   ")"                               DELIMITED BY SIZE
                                   INTO WS-MSG
                               END-STRING
                               PERFORM DISPLAY-AND-LOG
                               EXIT PERFORM
                           END-IF
                       END-PERFORM

                       IF PROFILE-NOT-FOUND
                           MOVE SPACES TO WS-MSG
                           STRING
                               "Connected with: " FUNCTION TRIM(WS-TARGET-USERNAME)
                               " (Profile not found)"
                               INTO WS-MSG
                           END-STRING
                           PERFORM DISPLAY-AND-LOG
                       END-IF
                   END-IF
               END-IF
           END-PERFORM

           IF WS-TMP-COUNT = 0
               MOVE MSG-NO-CONNECTIONS TO WS-MSG PERFORM DISPLAY-AND-LOG
           ELSE
               MOVE MSG-LINE TO WS-MSG PERFORM DISPLAY-AND-LOG
           END-IF
           EXIT.

       GET-FULL-NAME.
           SET PROFILE-NOT-FOUND TO TRUE
           INITIALIZE WS-DISPLAY-NAME
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > WS-PROFILES-COUNT
               IF WS-PROF-USERNAME(WS-J) = WS-TARGET-USERNAME
                   SET PROFILE-FOUND TO TRUE
                   STRING
                       FUNCTION TRIM(WS-PROF-FIRST(WS-J)) DELIMITED BY SIZE
                       " "                                DELIMITED BY SIZE
                       FUNCTION TRIM(WS-PROF-LAST(WS-J))  DELIMITED BY SIZE
                       INTO WS-DISPLAY-NAME
                   END-STRING
                   EXIT PERFORM
               END-IF
           END-PERFORM
           IF PROFILE-NOT-FOUND
               MOVE WS-TARGET-USERNAME TO WS-DISPLAY-NAME
           END-IF
           EXIT.

       ACCEPT-CONNECTION.
           IF WS-CONN-STATUS(WS-I) NOT = 'P'
               MOVE "Error: This request has already been processed." TO WS-MSG
               PERFORM DISPLAY-AND-LOG
               EXIT PARAGRAPH
           END-IF
           IF WS-CONN-RECEIVER(WS-I) NOT = WS-CURRENT-USERNAME
               MOVE "Error: You cannot accept this request." TO WS-MSG
               PERFORM DISPLAY-AND-LOG
               EXIT PARAGRAPH
           END-IF
           MOVE 'A' TO WS-CONN-STATUS(WS-I)
           PERFORM SAVE-CONNECTIONS
           IF WS-CONN-FILE-STATUS NOT = "00"
               MOVE 'P' TO WS-CONN-STATUS(WS-I)
               MOVE "Error: Could not save connection. Please try again." TO WS-MSG
               PERFORM DISPLAY-AND-LOG
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO WS-MSG
           STRING
               "Connection accepted with " DELIMITED BY SIZE
               FUNCTION TRIM(WS-DISPLAY-NAME) DELIMITED BY SIZE
               INTO WS-MSG
           END-STRING
           PERFORM DISPLAY-AND-LOG
           EXIT.

       REJECT-CONNECTION.
           IF WS-CONN-STATUS(WS-I) NOT = 'P'
               MOVE "Error: This request has already been processed." TO WS-MSG
               PERFORM DISPLAY-AND-LOG
               EXIT PARAGRAPH
           END-IF
           IF WS-CONN-RECEIVER(WS-I) NOT = WS-CURRENT-USERNAME
               MOVE "Error: You cannot reject this request." TO WS-MSG
               PERFORM DISPLAY-AND-LOG
               EXIT PARAGRAPH
           END-IF

           PERFORM VARYING WS-J FROM WS-I BY 1 UNTIL WS-J >= WS-CONNECTIONS-COUNT
               MOVE WS-CONN-SENDER  (WS-J + 1) TO WS-CONN-SENDER  (WS-J)
               MOVE WS-CONN-RECEIVER(WS-J + 1) TO WS-CONN-RECEIVER(WS-J)
               MOVE WS-CONN-STATUS  (WS-J + 1) TO WS-CONN-STATUS  (WS-J)
           END-PERFORM

           SUBTRACT 1 FROM WS-CONNECTIONS-COUNT
           PERFORM SAVE-CONNECTIONS
           IF WS-CONN-FILE-STATUS NOT = "00"
               MOVE "Error: Could not save changes. Please restart program." TO WS-MSG
               PERFORM DISPLAY-AND-LOG
               EXIT PARAGRAPH
           END-IF

           MOVE SPACES TO WS-MSG
           STRING
               "Connection request from "     DELIMITED BY SIZE
               "Connection request from "     DELIMITED BY SIZE
               FUNCTION TRIM(WS-DISPLAY-NAME) DELIMITED BY SIZE
               " rejected"                    DELIMITED BY SIZE
               " rejected"                    DELIMITED BY SIZE
               INTO WS-MSG
           END-STRING
           PERFORM DISPLAY-AND-LOG
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
                   "|"                                   DELIMITED BY SIZE
                   FUNCTION TRIM(WS-CONN-RECEIVER(WS-I)) DELIMITED BY SIZE
                   "|"                                   DELIMITED BY SIZE
                   FUNCTION TRIM(WS-CONN-STATUS(WS-I))   DELIMITED BY SIZE
                   INTO CONNECTION-REC
               END-STRING
               WRITE CONNECTION-REC
           END-PERFORM
           CLOSE CONNECTIONS-FILE
           EXIT.

       APPLICATIONS-IO-SECTION.
       INIT-LOAD-APPLICATIONS.
           MOVE 0 TO WS-APPLICATIONS-COUNT
           OPEN INPUT APPLICATIONS-FILE
           EVALUATE TRUE
               WHEN WS-APP-STATUS = "00"
                   SET NOT-EOF-APPS TO TRUE
                   PERFORM UNTIL EOF-APPS
                       READ APPLICATIONS-FILE
                           AT END
                               SET EOF-APPS TO TRUE
                           NOT AT END
                               PERFORM PARSE-APPLICATION-REC
                       END-READ
                   END-PERFORM
                   CLOSE APPLICATIONS-FILE
               WHEN WS-APP-STATUS = "05" OR WS-APP-STATUS = "35"
                   CONTINUE  *> missing is OK
               WHEN OTHER
                   MOVE SPACES TO WS-MSG
                   STRING
                       "Error opening applications file (status " DELIMITED BY SIZE
                       WS-APP-STATUS                           DELIMITED BY SIZE
                       ")."                                     DELIMITED BY SIZE
                       WS-APP-STATUS                           DELIMITED BY SIZE
                       ")."                                     DELIMITED BY SIZE
                       INTO WS-MSG
                   END-STRING
                   PERFORM DISPLAY-AND-LOG
           END-EVALUATE
           EXIT.



       PARSE-APPLICATION-REC.
           *> Format: jobId|username
           MOVE SPACES TO APP-ID-TEXT
           IF WS-APPLICATIONS-COUNT < WS-APPLICATIONS-MAX
               ADD 1 TO WS-APPLICATIONS-COUNT
               UNSTRING APPLICATION-REC DELIMITED BY '|'
                   INTO APP-ID-TEXT
                        WS-APP-USER(WS-APPLICATIONS-COUNT)
               END-UNSTRING
               MOVE FUNCTION NUMVAL(FUNCTION TRIM(APP-ID-TEXT))
                    TO WS-APP-JOB-ID(WS-APPLICATIONS-COUNT)
           END-IF
           EXIT.

       SAVE-APPLICATION-REC.
           OPEN EXTEND APPLICATIONS-FILE
           IF WS-APP-STATUS = "00"
               MOVE SPACES TO APPLICATION-REC
               MOVE WS-JOB-ID(WS-I) TO WS-JOB-ID-DISPLAY
               MOVE SPACES           TO WS-JOB-ID-TEXT
               MOVE SPACES           TO WS-JOB-ID-TEXT
               MOVE WS-JOB-ID-DISPLAY TO WS-JOB-ID-TEXT
               STRING
                   FUNCTION TRIM(WS-JOB-ID-TEXT)       DELIMITED BY SIZE
                   "|"                                 DELIMITED BY SIZE
                   FUNCTION TRIM(WS-CURRENT-USERNAME)  DELIMITED BY SIZE
                   FUNCTION TRIM(WS-JOB-ID-TEXT)       DELIMITED BY SIZE
                   "|"                                 DELIMITED BY SIZE
                   FUNCTION TRIM(WS-CURRENT-USERNAME)  DELIMITED BY SIZE
                   INTO APPLICATION-REC
               END-STRING
               WRITE APPLICATION-REC
               CLOSE APPLICATIONS-FILE
           ELSE
               MOVE SPACES TO WS-MSG
               STRING
                   "Error: cannot open applications file (status "
                   WS-APP-STATUS ")."
                   INTO WS-MSG
               END-STRING
               PERFORM DISPLAY-AND-LOG
           END-IF
           EXIT.

       CHECK-ALREADY-APPLIED.
           SET MATCH-NOT-FOUND TO TRUE
           PERFORM VARYING WS-J FROM 1 BY 1
                   UNTIL WS-J > WS-APPLICATIONS-COUNT OR MATCH-FOUND
               IF WS-APP-JOB-ID(WS-J) = WS-JOB-ID(WS-I)
                  AND FUNCTION TRIM(WS-APP-USER(WS-J))
                      = FUNCTION TRIM(WS-CURRENT-USERNAME)
                   SET MATCH-FOUND TO TRUE
               END-IF
           END-PERFORM
           EXIT.

       SERIALIZATION-SECTION.
       SERIALIZE-EXPERIENCE.
           INITIALIZE WS-EXPS-STR
           MOVE 1 TO WS-J
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-EXP-COUNT
               IF WS-I > 1
                   STRING "^" INTO WS-EXPS-STR WITH POINTER WS-J
                   END-STRING
               END-IF
               STRING
                   FUNCTION TRIM(WS-EXP-TITLE(WS-I))    DELIMITED BY SIZE
                   "~"                                  DELIMITED BY SIZE
                   FUNCTION TRIM(WS-EXP-COMPANY(WS-I))  DELIMITED BY SIZE
                   "~"                                  DELIMITED BY SIZE
                   FUNCTION TRIM(WS-EXP-DATES(WS-I))    DELIMITED BY SIZE
                   "~"                                  DELIMITED BY SIZE
                   FUNCTION TRIM(WS-EXP-DESC(WS-I))     DELIMITED BY SIZE
                   FUNCTION TRIM(WS-EXP-TITLE(WS-I))    DELIMITED BY SIZE
                   "~"                                  DELIMITED BY SIZE
                   FUNCTION TRIM(WS-EXP-COMPANY(WS-I))  DELIMITED BY SIZE
                   "~"                                  DELIMITED BY SIZE
                   FUNCTION TRIM(WS-EXP-DATES(WS-I))    DELIMITED BY SIZE
                   "~"                                  DELIMITED BY SIZE
                   FUNCTION TRIM(WS-EXP-DESC(WS-I))     DELIMITED BY SIZE
                   INTO WS-EXPS-STR
                   WITH POINTER WS-J
               END-STRING
           END-PERFORM
           EXIT.

       SERIALIZE-EDUCATION.
           INITIALIZE WS-EDUS-STR
           MOVE 1 TO WS-J
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-EDU-COUNT
               IF WS-I > 1
                   STRING "^" INTO WS-EDUS-STR WITH POINTER WS-J
                   END-STRING
               END-IF
               STRING
                   FUNCTION TRIM(WS-EDU-DEGREE(WS-I))   DELIMITED BY SIZE
                   "~"                                  DELIMITED BY SIZE
                   FUNCTION TRIM(WS-EDU-SCHOOL(WS-I))   DELIMITED BY SIZE
                   "~"                                  DELIMITED BY SIZE
                   FUNCTION TRIM(WS-EDU-YEARS(WS-I))    DELIMITED BY SIZE
                   FUNCTION TRIM(WS-EDU-DEGREE(WS-I))   DELIMITED BY SIZE
                   "~"                                  DELIMITED BY SIZE
                   FUNCTION TRIM(WS-EDU-SCHOOL(WS-I))   DELIMITED BY SIZE
                   "~"                                  DELIMITED BY SIZE
                   FUNCTION TRIM(WS-EDU-YEARS(WS-I))    DELIMITED BY SIZE
                   INTO WS-EDUS-STR
                   WITH POINTER WS-J
               END-STRING
           END-PERFORM
           EXIT.

       DISPLAY-EXPERIENCES.
           IF WS-EXPS-STR = SPACES
               MOVE SPACES TO WS-MSG
               STRING "Experience: None" INTO WS-MSG
               END-STRING
               PERFORM DISPLAY-AND-LOG
               EXIT PARAGRAPH
           END-IF

           MOVE SPACES TO WS-MSG
           STRING "Experience:" INTO WS-MSG END-STRING
           PERFORM DISPLAY-AND-LOG

           MOVE 1 TO WS-J
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
               STRING "   Title: " FUNCTION TRIM(WS-T1) INTO WS-MSG END-STRING
               PERFORM DISPLAY-AND-LOG

               MOVE SPACES TO WS-MSG
               STRING "   Company: " FUNCTION TRIM(WS-T2) INTO WS-MSG END-STRING
               PERFORM DISPLAY-AND-LOG

               MOVE SPACES TO WS-MSG
               STRING "   Dates: " FUNCTION TRIM(WS-T3) INTO WS-MSG END-STRING
               PERFORM DISPLAY-AND-LOG

               MOVE SPACES TO WS-MSG
               STRING "   Description: " FUNCTION TRIM(WS-T4) INTO WS-MSG END-STRING
               PERFORM DISPLAY-AND-LOG
           END-PERFORM
           EXIT.

       DISPLAY-EDUCATION.
           IF WS-EDUS-STR = SPACES
               MOVE SPACES TO WS-MSG
               STRING "Education: None" INTO WS-MSG END-STRING
               PERFORM DISPLAY-AND-LOG
               EXIT PARAGRAPH
           END-IF

           MOVE SPACES TO WS-MSG
           STRING "Education:" INTO WS-MSG END-STRING
           PERFORM DISPLAY-AND-LOG

           MOVE 1 TO WS-J
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
               STRING "   Degree: " FUNCTION TRIM(WS-T1) INTO WS-MSG END-STRING
               PERFORM DISPLAY-AND-LOG

               MOVE SPACES TO WS-MSG
               STRING "   University: " FUNCTION TRIM(WS-T2) INTO WS-MSG END-STRING
               PERFORM DISPLAY-AND-LOG

               MOVE SPACES TO WS-MSG
               STRING "   Years: " FUNCTION TRIM(WS-T3) INTO WS-MSG END-STRING
               PERFORM DISPLAY-AND-LOG
           END-PERFORM
           EXIT.

       DESERIALIZE-EXPERIENCE.
           MOVE 0 TO WS-EXP-COUNT
           MOVE WS-PROF-EXPERIENCES(WS-PROFILE-IDX) TO WS-EXPS-STR
           IF WS-EXPS-STR = SPACES
               EXIT PARAGRAPH
           END-IF
           MOVE 1 TO WS-J
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
           END-PERFORM
           EXIT.

       DESERIALIZE-EDUCATION.
           MOVE 0 TO WS-EDU-COUNT
           MOVE WS-PROF-EDUCATIONS(WS-PROFILE-IDX) TO WS-EDUS-STR
           IF WS-EDUS-STR = SPACES
               EXIT PARAGRAPH
           END-IF
           MOVE 1 TO WS-J
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
           END-PERFORM
           EXIT.

       PROFILE-SECTION.
       CREATE-OR-EDIT-PROFILE.
           IF FUNCTION TRIM(WS-CURRENT-USERNAME) = SPACES
             MOVE "Internal error: no logged-in user." TO WS-MSG
             PERFORM DISPLAY-AND-LOG
             EXIT PARAGRAPH
           END-IF

           MOVE MSG-EDIT-HEADER TO WS-MSG PERFORM DISPLAY-AND-LOG

           PERFORM UNTIL FUNCTION TRIM(WS-PROF-FIRST-IN) NOT = SPACES
               MOVE MSG-ENTER-FIRST TO WS-MSG PERFORM DISPLAY-AND-LOG
               PERFORM READ-NEXT-LINE
               MOVE WS-LINE TO WS-PROF-FIRST-IN
               IF EOF-IN
                   EXIT PARAGRAPH
               END-IF
               IF FUNCTION TRIM(WS-PROF-FIRST-IN) = SPACES
                   MOVE MSG-REQUIRED TO WS-MSG PERFORM DISPLAY-AND-LOG
               END-IF
           END-PERFORM

           PERFORM UNTIL FUNCTION TRIM(WS-PROF-LAST-IN) NOT = SPACES
               MOVE MSG-ENTER-LAST TO WS-MSG PERFORM DISPLAY-AND-LOG
               PERFORM READ-NEXT-LINE
               MOVE WS-LINE TO WS-PROF-LAST-IN
               IF EOF-IN
                   EXIT PARAGRAPH
               END-IF
               IF FUNCTION TRIM(WS-PROF-LAST-IN) = SPACES
                   MOVE MSG-REQUIRED TO WS-MSG PERFORM DISPLAY-AND-LOG
               END-IF
           END-PERFORM

           PERFORM UNTIL FUNCTION TRIM(WS-PROF-UNIV-IN) NOT = SPACES
               MOVE MSG-ENTER-UNIV TO WS-MSG PERFORM DISPLAY-AND-LOG
               PERFORM READ-NEXT-LINE
               MOVE WS-LINE TO WS-PROF-UNIV-IN
               IF EOF-IN
                   EXIT PARAGRAPH
               END-IF
               IF FUNCTION TRIM(WS-PROF-UNIV-IN) = SPACES
                   MOVE MSG-REQUIRED TO WS-MSG PERFORM DISPLAY-AND-LOG
               END-IF
           END-PERFORM

           PERFORM UNTIL FUNCTION TRIM(WS-PROF-MAJOR-IN) NOT = SPACES
               MOVE MSG-ENTER-MAJOR TO WS-MSG PERFORM DISPLAY-AND-LOG
               PERFORM READ-NEXT-LINE
               MOVE WS-LINE TO WS-PROF-MAJOR-IN
               IF EOF-IN
                   EXIT PARAGRAPH
               END-IF
               IF FUNCTION TRIM(WS-PROF-MAJOR-IN) = SPACES
                   MOVE MSG-REQUIRED TO WS-MSG PERFORM DISPLAY-AND-LOG
               END-IF
           END-PERFORM

           SET YEAR-INVALID TO TRUE
           PERFORM UNTIL YEAR-VALID OR EOF-IN
               MOVE MSG-ENTER-GYEAR2 TO WS-MSG PERFORM DISPLAY-AND-LOG
               PERFORM READ-NEXT-LINE
               MOVE WS-LINE TO WS-PROF-GYEAR-IN
               IF EOF-IN
                   EXIT PARAGRAPH
               END-IF
               PERFORM VALIDATE-GRAD-YEAR
               IF YEAR-INVALID
                   MOVE MSG-YEAR-INVALID TO WS-MSG PERFORM DISPLAY-AND-LOG
               END-IF
           END-PERFORM

           MOVE MSG-ABOUT-ME TO WS-MSG PERFORM DISPLAY-AND-LOG
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
             MOVE FUNCTION TRIM(WS-PROF-ABOUT-IN)    TO WS-PROF-ABOUT(WS-PROFILE-IDX)
             MOVE FUNCTION TRIM(WS-EXPS-STR)         TO WS-PROF-EXPERIENCES(WS-PROFILE-IDX)
             MOVE FUNCTION TRIM(WS-EDUS-STR)         TO WS-PROF-EDUCATIONS(WS-PROFILE-IDX)
           END-IF

           PERFORM SAVE-PROFILES
           MOVE MSG-PROFILE-SAVED-OK TO WS-MSG PERFORM DISPLAY-AND-LOG
           EXIT.

       ADD-EXPERIENCE.
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

                   MOVE SPACES TO WS-MSG
                   STRING "Experience #" WS-EXP-COUNT " - Title: " INTO WS-MSG
                   END-STRING
                   PERFORM DISPLAY-AND-LOG
                   PERFORM READ-NEXT-LINE
                   MOVE WS-LINE TO WS-TITLE-INPUT
                   IF EOF-IN
                       EXIT PERFORM
                   END-IF
                   MOVE WS-TITLE-INPUT TO WS-EXP-TITLE(WS-EXP-COUNT)

                   MOVE SPACES TO WS-MSG
                   STRING "Experience #" WS-EXP-COUNT " - Company/Organization: "
                          INTO WS-MSG
                   END-STRING
                   PERFORM DISPLAY-AND-LOG
                   PERFORM READ-NEXT-LINE
                   MOVE WS-LINE TO WS-COMPANY-INPUT
                   IF EOF-IN
                       EXIT PERFORM
                   END-IF
                   MOVE WS-COMPANY-INPUT TO WS-EXP-COMPANY(WS-EXP-COUNT)

                   MOVE SPACES TO WS-MSG
                   STRING "Experience #" WS-EXP-COUNT " - Dates (e.g., Summer 2024): "
                          INTO WS-MSG
                   END-STRING
                   PERFORM DISPLAY-AND-LOG
                   PERFORM READ-NEXT-LINE
                   MOVE WS-LINE TO WS-DATES-INPUT
                   IF EOF-IN
                       EXIT PERFORM
                   END-IF
                   MOVE WS-DATES-INPUT TO WS-EXP-DATES(WS-EXP-COUNT)

                   MOVE SPACES TO WS-MSG
                   STRING "Experience #" WS-EXP-COUNT
                          " - Description (max 100 chars, blank to skip): "
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

                   MOVE SPACES TO WS-MSG
                   STRING "Education #" WS-EDU-COUNT " - Degree: " INTO WS-MSG
                   END-STRING
                   PERFORM DISPLAY-AND-LOG
                   PERFORM READ-NEXT-LINE
                   MOVE WS-LINE TO WS-DEGREE-INPUT
                   IF EOF-IN
                       EXIT PERFORM
                   END-IF
                   MOVE WS-DEGREE-INPUT TO WS-EDU-DEGREE(WS-EDU-COUNT)

                   MOVE SPACES TO WS-MSG
                   STRING "Education #" WS-EDU-COUNT " - University/College: "
                          INTO WS-MSG
                   END-STRING
                   PERFORM DISPLAY-AND-LOG
                   PERFORM READ-NEXT-LINE
                   MOVE WS-LINE TO WS-SCHOOL-INPUT
                   IF EOF-IN
                       EXIT PERFORM
                   END-IF
                   MOVE WS-SCHOOL-INPUT TO WS-EDU-SCHOOL(WS-EDU-COUNT)

                   MOVE SPACES TO WS-MSG
                   STRING "Education #" WS-EDU-COUNT " - Years Attended (e.g., 2023-2025): "
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
             MOVE FUNCTION TRIM(WS-PROF-FIRST(WS-PROFILE-IDX))    TO WS-PROF-FIRST-IN
             MOVE FUNCTION TRIM(WS-PROF-LAST(WS-PROFILE-IDX))     TO WS-PROF-LAST-IN
             MOVE FUNCTION TRIM(WS-PROF-UNIV(WS-PROFILE-IDX))     TO WS-PROF-UNIV-IN
             MOVE FUNCTION TRIM(WS-PROF-MAJOR(WS-PROFILE-IDX))    TO WS-PROF-MAJOR-IN
             MOVE FUNCTION TRIM(WS-PROF-GYEAR(WS-PROFILE-IDX))    TO WS-PROF-GYEAR-IN
             MOVE FUNCTION TRIM(WS-PROF-ABOUT(WS-PROFILE-IDX))    TO WS-PROF-ABOUT-IN
             MOVE FUNCTION TRIM(WS-PROF-EXPERIENCES(WS-PROFILE-IDX)) TO WS-EXPS-STR
             MOVE FUNCTION TRIM(WS-PROF-EDUCATIONS(WS-PROFILE-IDX))  TO WS-EDUS-STR
           ELSE
               MOVE MSG-PROFILE-NOT-FOUND TO WS-MSG PERFORM DISPLAY-AND-LOG
               EXIT PARAGRAPH
           END-IF

           MOVE MSG-VIEW-HEADER TO WS-MSG PERFORM DISPLAY-AND-LOG

           MOVE SPACES TO WS-MSG
           STRING "Name: "                         DELIMITED BY SIZE
                  FUNCTION TRIM(WS-PROF-FIRST-IN)  DELIMITED BY SIZE
                  " "                              DELIMITED BY SIZE
                  FUNCTION TRIM(WS-PROF-LAST-IN)   DELIMITED BY SIZE
                  INTO WS-MSG
           END-STRING
           PERFORM DISPLAY-AND-LOG

           MOVE SPACES TO WS-MSG
           STRING "University: " FUNCTION TRIM(WS-PROF-UNIV-IN) INTO WS-MSG
           END-STRING
           PERFORM DISPLAY-AND-LOG

           MOVE SPACES TO WS-MSG
           STRING "Major: " FUNCTION TRIM(WS-PROF-MAJOR-IN) INTO WS-MSG
           END-STRING
           PERFORM DISPLAY-AND-LOG

           MOVE SPACES TO WS-MSG
           STRING "Graduation Year: " FUNCTION TRIM(WS-PROF-GYEAR-IN) INTO WS-MSG
           END-STRING
           PERFORM DISPLAY-AND-LOG

           IF FUNCTION TRIM(WS-PROF-ABOUT-IN) NOT = SPACES
               MOVE SPACES TO WS-MSG
               STRING "About Me: " FUNCTION TRIM(WS-PROF-ABOUT-IN) INTO WS-MSG
               END-STRING
               PERFORM DISPLAY-AND-LOG
           END-IF

           PERFORM DISPLAY-EXPERIENCES
           PERFORM DISPLAY-EDUCATION
           MOVE MSG-LINE TO WS-MSG PERFORM DISPLAY-AND-LOG
           EXIT.

       REQUESTS-SECTION.
       VIEW-PENDING-REQUESTS-FILE.
           MOVE MSG-PENDING-HEADER TO WS-MSG PERFORM DISPLAY-AND-LOG
           OPEN INPUT REQUEST-FILE
           IF WS-REQ-STATUS = "00"
              SET NOT-EOF-REQ TO TRUE
              MOVE 0 TO WS-I
              PERFORM UNTIL EOF-REQ
                 READ REQUEST-FILE
                   AT END SET EOF-REQ TO TRUE
                   NOT AT END PERFORM CHECK-PENDING-REQUEST
                   AT END SET EOF-REQ TO TRUE
                   NOT AT END PERFORM CHECK-PENDING-REQUEST
                 END-READ
              END-PERFORM
              CLOSE REQUEST-FILE
              IF WS-I = 0
                 MOVE MSG-NO-PENDING-REQUESTS TO WS-MSG PERFORM DISPLAY-AND-LOG
              END-IF
           ELSE
              MOVE MSG-NO-PENDING-REQUESTS TO WS-MSG PERFORM DISPLAY-AND-LOG
           END-IF
           MOVE "-----------------------------------" TO WS-MSG PERFORM DISPLAY-AND-LOG
           EXIT.

       CHECK-PENDING-REQUEST.
           MOVE SPACES TO WS-REQ-SENDER WS-REQ-RECEIVER WS-REQ-STATUS-VALUE
           UNSTRING REQUEST-REC DELIMITED BY '|'
               INTO WS-REQ-SENDER
                    WS-REQ-RECEIVER
                    WS-REQ-STATUS-VALUE
           END-UNSTRING
           IF FUNCTION TRIM(WS-REQ-RECEIVER) = FUNCTION TRIM(WS-CURRENT-USERNAME)
              AND FUNCTION TRIM(WS-REQ-STATUS-VALUE) = "PENDING"
              ADD 1 TO WS-I
              PERFORM FIND-SENDER-NAME
              MOVE SPACES TO WS-MSG
              STRING
                 "Connection request from " DELIMITED BY SIZE
                 FUNCTION TRIM(WS-T1)       DELIMITED BY SIZE
                 "."                        DELIMITED BY SIZE
                 "."                        DELIMITED BY SIZE
                 INTO WS-MSG
              END-STRING
              PERFORM DISPLAY-AND-LOG
           END-IF
           EXIT.

       FIND-SENDER-NAME.
           MOVE SPACES TO WS-T1
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > WS-PROFILES-COUNT
               IF FUNCTION TRIM(WS-PROF-USERNAME(WS-J)) =
                  FUNCTION TRIM(WS-REQ-SENDER)
                   STRING
                       FUNCTION TRIM(WS-PROF-FIRST(WS-J)) DELIMITED BY SIZE
                       " "                                DELIMITED BY SIZE
                       FUNCTION TRIM(WS-PROF-LAST(WS-J))  DELIMITED BY SIZE
                       INTO WS-T1
                   END-STRING
                   EXIT PERFORM
               END-IF
           END-PERFORM
           IF WS-T1 = SPACES
               MOVE FUNCTION TRIM(WS-REQ-SENDER) TO WS-T1
           END-IF
           EXIT.

       REQUEST-MENU.
           MOVE MSG-REQUEST-MENU-1 TO WS-MSG PERFORM DISPLAY-AND-LOG
           MOVE MSG-REQUEST-MENU-2 TO WS-MSG PERFORM DISPLAY-AND-LOG
           MOVE MSG-ENTER-CHOICE   TO WS-MSG PERFORM DISPLAY-AND-LOG
           PERFORM READ-NEXT-LINE
           MOVE WS-LINE TO WS-REQUEST-CHOICE
           IF EOF-IN
               EXIT PARAGRAPH
           END-IF
           EVALUATE WS-REQUEST-CHOICE
               WHEN '1'
                   *> Placeholder for future SEND-REQUEST
                   EXIT PARAGRAPH
               WHEN '2'
                   EXIT PARAGRAPH
               WHEN OTHER
                   MOVE MSG-INVALID-CHOICE TO WS-MSG PERFORM DISPLAY-AND-LOG
           END-EVALUATE
           EXIT.

       SAVE-REQUEST.
           MOVE WS-PROF-USERNAME(WS-SEARCH-RESULT-IDX) TO WS-REQ-RECEIVER
           MOVE WS-CURRENT-USERNAME                      TO WS-REQ-SENDER
           MOVE "PENDING"                                TO WS-REQ-STATUS-VALUE
           MOVE WS-CURRENT-USERNAME                      TO WS-REQ-SENDER
           MOVE "PENDING"                                TO WS-REQ-STATUS-VALUE

           OPEN EXTEND REQUEST-FILE
           IF WS-REQ-STATUS = "00"
               MOVE SPACES TO REQUEST-REC
               STRING
                   FUNCTION TRIM(WS-REQ-SENDER)   DELIMITED BY SIZE
                   "|"                            DELIMITED BY SIZE
                   "|"                            DELIMITED BY SIZE
                   FUNCTION TRIM(WS-REQ-RECEIVER) DELIMITED BY SIZE
                   "|"                            DELIMITED BY SIZE
                   "|"                            DELIMITED BY SIZE
                   FUNCTION TRIM(WS-REQ-STATUS-VALUE) DELIMITED BY SIZE
                   INTO REQUEST-REC
               END-STRING
               WRITE REQUEST-REC
               CLOSE REQUEST-FILE
           ELSE
               IF WS-REQ-STATUS NOT = "05"
                   CLOSE REQUEST-FILE
               END-IF
               MOVE "Error: Unable to save connection request." TO WS-MSG
               PERFORM DISPLAY-AND-LOG
           END-IF
           EXIT.

       JOBS-SECTION.
       JOBS-MENU.
           PERFORM UNTIL WS-JOB-CHOICE = '4' OR EOF-IN
               MOVE MSG-JOBS-HEADER   TO WS-MSG PERFORM DISPLAY-AND-LOG
               *>MOVE MSG-JOBS-POST     TO WS-MSG PERFORM DISPLAY-AND-LOG
               *>MOVE MSG-JOBS-BROWSE   TO WS-MSG PERFORM DISPLAY-AND-LOG
               *>MOVE MSG-JOBS-VIEW-APPS TO WS-MSG PERFORM DISPLAY-AND-LOG
               *>MOVE MSG-JOBS-BACK     TO WS-MSG PERFORM DISPLAY-AND-LOG
               *>MOVE MSG-ENTER-CHOICE  TO WS-MSG PERFORM DISPLAY-AND-LOG

               MOVE SPACES TO WS-MSG
               STRING "   " FUNCTION TRIM(MSG-JOBS-POST) INTO WS-MSG END-STRING
               STRING "   " FUNCTION TRIM(MSG-JOBS-POST) INTO WS-MSG END-STRING
               PERFORM DISPLAY-AND-LOG

               MOVE SPACES TO WS-MSG
               STRING "   " FUNCTION TRIM(MSG-JOBS-BROWSE) INTO WS-MSG END-STRING
               STRING "   " FUNCTION TRIM(MSG-JOBS-BROWSE) INTO WS-MSG END-STRING
               PERFORM DISPLAY-AND-LOG

               MOVE SPACES TO WS-MSG
               STRING "   " FUNCTION TRIM(MSG-JOBS-VIEW-APPS) INTO WS-MSG END-STRING
               STRING "   " FUNCTION TRIM(MSG-JOBS-VIEW-APPS) INTO WS-MSG END-STRING
               PERFORM DISPLAY-AND-LOG

               MOVE SPACES TO WS-MSG
               STRING "   " FUNCTION TRIM(MSG-JOBS-BACK) INTO WS-MSG END-STRING
               STRING "   " FUNCTION TRIM(MSG-JOBS-BACK) INTO WS-MSG END-STRING
               PERFORM DISPLAY-AND-LOG

               MOVE MSG-ENTER-CHOICE  TO WS-MSG PERFORM DISPLAY-AND-LOG

               PERFORM READ-NEXT-LINE
               MOVE WS-LINE TO WS-JOB-CHOICE
               IF EOF-IN
                   EXIT PERFORM
               END-IF

               EVALUATE WS-JOB-CHOICE
                   WHEN '1'  PERFORM POST-NEW-JOB
                   WHEN '2'  PERFORM BROWSE-JOBS
                   WHEN '3'  PERFORM VIEW-MY-APPLICATIONS
                   WHEN '4'  EXIT PERFORM
                   WHEN OTHER
                       MOVE MSG-INVALID-CHOICE TO WS-MSG
                       PERFORM DISPLAY-AND-LOG
               END-EVALUATE
           END-PERFORM
           MOVE SPACES TO WS-JOB-CHOICE
           EXIT.

       *> ===============================================================
       *> JOBS BROWSE / DETAILS / APPLY
       *> ===============================================================
       BROWSE-JOBS.
           IF WS-JOBS-COUNT = 0
               MOVE MSG-NO-JOBS        TO WS-MSG PERFORM DISPLAY-AND-LOG
               MOVE MSG-SEPARATOR-LINE TO WS-MSG PERFORM DISPLAY-AND-LOG
               EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL EOF-IN
               PERFORM DISPLAY-JOB-LIST

               MOVE "-----------------------------" TO WS-MSG PERFORM DISPLAY-AND-LOG
               MOVE MSG-ENTER-JOB TO WS-MSG PERFORM DISPLAY-AND-LOG
               PERFORM READ-NEXT-LINE
               IF EOF-IN
                   EXIT PERFORM
               END-IF

               MOVE FUNCTION NUMVAL(WS-LINE) TO WS-SEL-NUM
               IF WS-SEL-NUM = 0
                   EXIT PERFORM
               ELSE
                   IF WS-SEL-NUM < 1 OR WS-SEL-NUM > WS-JOBS-COUNT
                       MOVE MSG-INVALID-JOB TO WS-MSG PERFORM DISPLAY-AND-LOG
                   ELSE
                       MOVE WS-SEL-NUM TO WS-I
                       PERFORM DISPLAY-JOB-DETAILS
                   END-IF
               END-IF
           END-PERFORM

           MOVE MSG-SEPARATOR-LINE TO WS-MSG PERFORM DISPLAY-AND-LOG
           EXIT.

       DISPLAY-JOB-DETAILS.
           MOVE MSG-JOB-DETAILS-HEADER TO WS-MSG PERFORM DISPLAY-AND-LOG

           MOVE SPACES TO WS-MSG
           STRING "Title: " FUNCTION TRIM(WS-JOB-TITLE(WS-I)) INTO WS-MSG END-STRING
           PERFORM DISPLAY-AND-LOG

           MOVE SPACES TO WS-MSG
           STRING "Employer: " FUNCTION TRIM(WS-JOB-EMPLOYER(WS-I)) INTO WS-MSG END-STRING
           PERFORM DISPLAY-AND-LOG

           MOVE SPACES TO WS-MSG
           STRING "Location: " FUNCTION TRIM(WS-JOB-LOCATION(WS-I)) INTO WS-MSG END-STRING
           PERFORM DISPLAY-AND-LOG

           MOVE SPACES TO WS-MSG
           STRING "Description: " FUNCTION TRIM(WS-JOB-DESC(WS-I)) INTO WS-MSG END-STRING
           PERFORM DISPLAY-AND-LOG

           MOVE FUNCTION TRIM(WS-JOB-SALARY(WS-I)) TO WS-SALARY-TRIM
           IF WS-SALARY-TRIM NOT = SPACES AND WS-SALARY-TRIM NOT = "NONE"
               MOVE SPACES TO WS-MSG
               STRING "Salary: " FUNCTION TRIM(WS-SALARY-TRIM) INTO WS-MSG END-STRING
               PERFORM DISPLAY-AND-LOG
           END-IF

           IF TEST-MODE-ON
               MOVE MSG-SEPARATOR-LINE TO WS-MSG PERFORM DISPLAY-AND-LOG
               EXIT PARAGRAPH
           END-IF

           MOVE SPACES TO WS-MSG
           MOVE MSG-JOB-DETAILS-DIVIDER TO WS-MSG PERFORM DISPLAY-AND-LOG

           *>MOVE MSG-APPLY-OPT    TO WS-MSG PERFORM DISPLAY-AND-LOG
           *>MOVE MSG-BACK-OPT     TO WS-MSG PERFORM DISPLAY-AND-LOG

           MOVE SPACES TO WS-MSG
               STRING "   " FUNCTION TRIM(MSG-APPLY-OPT) INTO WS-MSG END-STRING
               STRING "   " FUNCTION TRIM(MSG-APPLY-OPT) INTO WS-MSG END-STRING
               PERFORM DISPLAY-AND-LOG
           MOVE SPACES TO WS-MSG
               STRING "   " FUNCTION TRIM(MSG-BACK-OPT) INTO WS-MSG END-STRING
               STRING "   " FUNCTION TRIM(MSG-BACK-OPT) INTO WS-MSG END-STRING
               PERFORM DISPLAY-AND-LOG

           MOVE MSG-ENTER-CHOICE TO WS-MSG PERFORM DISPLAY-AND-LOG

           PERFORM READ-NEXT-LINE
           IF EOF-IN
               EXIT PARAGRAPH
           END-IF

           EVALUATE WS-LINE
               WHEN "1"  PERFORM APPLY-FOR-JOB
               WHEN "2"  CONTINUE
               WHEN OTHER
                   MOVE MSG-INVALID-CHOICE TO WS-MSG PERFORM DISPLAY-AND-LOG
           END-EVALUATE

           *>MOVE MSG-SEPARATOR-LINE TO WS-MSG PERFORM DISPLAY-AND-LOG
           EXIT.

       APPLY-FOR-JOB.
           PERFORM CHECK-ALREADY-APPLIED
           IF MATCH-FOUND
               MOVE MSG-APPLY-DUPLICATE TO WS-MSG PERFORM DISPLAY-AND-LOG
               EXIT PARAGRAPH
           END-IF

           IF WS-APPLICATIONS-COUNT >= WS-APPLICATIONS-MAX
               MOVE "Error: applications storage full." TO WS-MSG
               PERFORM DISPLAY-AND-LOG
               EXIT PARAGRAPH
           END-IF

           ADD 1 TO WS-APPLICATIONS-COUNT
           MOVE WS-JOB-ID(WS-I)        TO WS-APP-JOB-ID(WS-APPLICATIONS-COUNT)
           MOVE WS-CURRENT-USERNAME    TO WS-APP-USER(WS-APPLICATIONS-COUNT)
           MOVE WS-JOB-ID(WS-I)        TO WS-APP-JOB-ID(WS-APPLICATIONS-COUNT)
           MOVE WS-CURRENT-USERNAME    TO WS-APP-USER(WS-APPLICATIONS-COUNT)

           IF TEST-MODE-OFF
               PERFORM SAVE-APPLICATION-REC
           END-IF

           MOVE SPACES TO WS-MSG
           STRING MSG-APPLY-SUCCESS            DELIMITED BY ' '
                  " "                          DELIMITED BY SIZE
           STRING MSG-APPLY-SUCCESS            DELIMITED BY ' '
                  " "                          DELIMITED BY SIZE
                  FUNCTION TRIM(WS-JOB-TITLE(WS-I))
                  " at "                       DELIMITED BY SIZE
                  " at "                       DELIMITED BY SIZE
                  FUNCTION TRIM(WS-JOB-EMPLOYER(WS-I))
                  " has been submitted."        DELIMITED BY SIZE
                  " has been submitted."        DELIMITED BY SIZE
                  INTO WS-MSG
           END-STRING
           PERFORM DISPLAY-AND-LOG

           EXIT.

       POST-NEW-JOB.
           MOVE MSG-POST-JOB-HEADER TO WS-MSG PERFORM DISPLAY-AND-LOG

           INITIALIZE WS-NEW-JOB-ID
                      WS-NEW-JOB-TITLE WS-NEW-JOB-DESC
                      WS-NEW-JOB-EMPLOYER WS-NEW-JOB-LOCATION
                      WS-NEW-JOB-SALARY

           PERFORM UNTIL FUNCTION TRIM(WS-NEW-JOB-TITLE) NOT = SPACES
               MOVE MSG-POST-JOB-TITLE TO WS-MSG PERFORM DISPLAY-AND-LOG
               PERFORM READ-NEXT-LINE
               MOVE WS-LINE TO WS-NEW-JOB-TITLE
               IF EOF-IN EXIT PARAGRAPH END-IF
               IF FUNCTION TRIM(WS-NEW-JOB-TITLE) = SPACES
                   MOVE MSG-REQUIRED TO WS-MSG PERFORM DISPLAY-AND-LOG
               END-IF
           END-PERFORM

           PERFORM UNTIL FUNCTION TRIM(WS-NEW-JOB-DESC) NOT = SPACES
               MOVE MSG-POST-JOB-DESC TO WS-MSG PERFORM DISPLAY-AND-LOG
               PERFORM READ-NEXT-LINE
               MOVE WS-LINE TO WS-NEW-JOB-DESC
               IF EOF-IN EXIT PARAGRAPH END-IF
               IF FUNCTION TRIM(WS-NEW-JOB-DESC) = SPACES
                   MOVE MSG-REQUIRED TO WS-MSG PERFORM DISPLAY-AND-LOG
               END-IF
           END-PERFORM

           PERFORM UNTIL FUNCTION TRIM(WS-NEW-JOB-EMPLOYER) NOT = SPACES
               MOVE MSG-POST-JOB-EMPLOYER TO WS-MSG PERFORM DISPLAY-AND-LOG
               PERFORM READ-NEXT-LINE
               MOVE WS-LINE TO WS-NEW-JOB-EMPLOYER
               IF EOF-IN EXIT PARAGRAPH END-IF
               IF FUNCTION TRIM(WS-NEW-JOB-EMPLOYER) = SPACES
                   MOVE MSG-REQUIRED TO WS-MSG PERFORM DISPLAY-AND-LOG
               END-IF
           END-PERFORM

           PERFORM UNTIL FUNCTION TRIM(WS-NEW-JOB-LOCATION) NOT = SPACES
               MOVE MSG-POST-JOB-LOCATION TO WS-MSG PERFORM DISPLAY-AND-LOG
               PERFORM READ-NEXT-LINE
               MOVE WS-LINE TO WS-NEW-JOB-LOCATION
               IF EOF-IN EXIT PARAGRAPH END-IF
               IF FUNCTION TRIM(WS-NEW-JOB-LOCATION) = SPACES
                   MOVE MSG-REQUIRED TO WS-MSG PERFORM DISPLAY-AND-LOG
               END-IF
           END-PERFORM

           PERFORM UNTIL FUNCTION TRIM(WS-NEW-JOB-SALARY) NOT = SPACES
               MOVE MSG-POST-JOB-SALARY TO WS-MSG PERFORM DISPLAY-AND-LOG
               PERFORM READ-NEXT-LINE
               MOVE WS-LINE TO WS-NEW-JOB-SALARY
               IF EOF-IN EXIT PARAGRAPH END-IF
               IF FUNCTION TRIM(WS-NEW-JOB-SALARY) = SPACES
                   MOVE "Enter 'NONE' to skip this field."
                       TO WS-MSG PERFORM DISPLAY-AND-LOG
               END-IF
           END-PERFORM

           ADD 1 TO WS-JOBS-COUNT
           ADD 1 TO WS-JOBS-HIGHEST-ID
           MOVE WS-JOBS-HIGHEST-ID  TO WS-NEW-JOB-ID
           MOVE WS-JOBS-HIGHEST-ID  TO WS-NEW-JOB-ID
           MOVE WS-NEW-JOB-ID       TO WS-JOB-ID(WS-JOBS-COUNT)
           MOVE WS-CURRENT-USERNAME TO WS-JOB-POSTER-USER(WS-JOBS-COUNT)
           MOVE WS-NEW-JOB-TITLE    TO WS-JOB-TITLE(WS-JOBS-COUNT)
           MOVE WS-NEW-JOB-DESC     TO WS-JOB-DESC(WS-JOBS-COUNT)
           MOVE WS-NEW-JOB-EMPLOYER TO WS-JOB-EMPLOYER(WS-JOBS-COUNT)
           MOVE WS-NEW-JOB-LOCATION TO WS-JOB-LOCATION(WS-JOBS-COUNT)
           MOVE WS-NEW-JOB-SALARY   TO WS-JOB-SALARY(WS-JOBS-COUNT)

           PERFORM SAVE-JOBS

           MOVE WS-NEW-JOB-ID TO WS-JOB-ID-DISPLAY
           MOVE SPACES TO WS-JOB-ID-TEXT
           MOVE WS-JOB-ID-DISPLAY TO WS-JOB-ID-TEXT
           MOVE SPACES TO WS-MSG
           STRING
               FUNCTION TRIM(MSG-POST-SUCCESS) DELIMITED BY SIZE
               " (ID: "                       DELIMITED BY SIZE
               FUNCTION TRIM(WS-JOB-ID-TEXT)   DELIMITED BY SIZE
               ")"                             DELIMITED BY SIZE
               " (ID: "                       DELIMITED BY SIZE
               FUNCTION TRIM(WS-JOB-ID-TEXT)   DELIMITED BY SIZE
               ")"                             DELIMITED BY SIZE
               INTO WS-MSG
           END-STRING
           PERFORM DISPLAY-AND-LOG
           MOVE MSG-SEPARATOR-LINE TO WS-MSG PERFORM DISPLAY-AND-LOG
           EXIT.

       JOBS-IO-SECTION.
       INIT-LOAD-JOBS.
           SET JOBS-IO-OK TO TRUE
           MOVE 0 TO WS-JOBS-COUNT
           MOVE 0 TO WS-JOBS-HIGHEST-ID
           OPEN INPUT JOBS-FILE
           EVALUATE WS-JOBS-FILE-STATUS
               WHEN "00"
                   SET NOT-EOF-JOBS TO TRUE
                   PERFORM UNTIL EOF-JOBS
                       READ JOBS-FILE
                           AT END SET EOF-JOBS TO TRUE
                           NOT AT END PERFORM PARSE-JOB-REC
                       END-READ
                       IF WS-JOBS-FILE-STATUS NOT = "00"
                          AND WS-JOBS-FILE-STATUS NOT = "10"
                           SET JOBS-IO-ERROR TO TRUE
                           MOVE "reading jobs file" TO WS-JOBS-ERR-CONTEXT
                           PERFORM REPORT-JOBS-FILE-ERROR
                           SET EOF-JOBS TO TRUE
                       END-IF
                   END-PERFORM
                   CLOSE JOBS-FILE
                   IF WS-JOBS-FILE-STATUS NOT = "00"
                       SET JOBS-IO-ERROR TO TRUE
                       MOVE "closing jobs file after load" TO WS-JOBS-ERR-CONTEXT
                       PERFORM REPORT-JOBS-FILE-ERROR
                   END-IF
               WHEN "05"  CONTINUE
               WHEN "35"  CONTINUE
               WHEN OTHER
                   SET JOBS-IO-ERROR TO TRUE
                   MOVE "opening jobs file for load" TO WS-JOBS-ERR-CONTEXT
                   PERFORM REPORT-JOBS-FILE-ERROR
           END-EVALUATE
           EXIT.

       PARSE-JOB-REC.
           *> Format: id|poster|title|desc|employer|location|salary
           IF WS-JOBS-COUNT < WS-JOBS-MAX
               ADD 1 TO WS-JOBS-COUNT
               MOVE 0 TO WS-JOB-DELIM-COUNT
               INSPECT JOB-REC TALLYING WS-JOB-DELIM-COUNT FOR ALL "|"
               MOVE SPACES TO WS-JOB-ID-TEXT
               IF WS-JOB-DELIM-COUNT >= 6
                   UNSTRING JOB-REC DELIMITED BY '|'
                       INTO WS-JOB-ID-TEXT
                            WS-JOB-POSTER-USER(WS-JOBS-COUNT)
                            WS-JOB-TITLE(WS-JOBS-COUNT)
                            WS-JOB-DESC(WS-JOBS-COUNT)
                            WS-JOB-EMPLOYER(WS-JOBS-COUNT)
                            WS-JOB-LOCATION(WS-JOBS-COUNT)
                            WS-JOB-SALARY(WS-JOBS-COUNT)
                   END-UNSTRING
                   IF FUNCTION TRIM(WS-JOB-ID-TEXT) = SPACES
                       ADD 1 TO WS-JOBS-HIGHEST-ID
                       MOVE WS-JOBS-HIGHEST-ID TO WS-JOB-ID(WS-JOBS-COUNT)
                   ELSE
                       MOVE FUNCTION NUMVAL(WS-JOB-ID-TEXT)
                            TO WS-JOB-ID(WS-JOBS-COUNT)
                            TO WS-JOB-ID(WS-JOBS-COUNT)
                       IF WS-JOB-ID(WS-JOBS-COUNT) > WS-JOBS-HIGHEST-ID
                           MOVE WS-JOB-ID(WS-JOBS-COUNT) TO WS-JOBS-HIGHEST-ID
                       END-IF
                   END-IF
               ELSE
                   UNSTRING JOB-REC DELIMITED BY '|'
                       INTO WS-JOB-POSTER-USER(WS-JOBS-COUNT)
                            WS-JOB-TITLE(WS-JOBS-COUNT)
                            WS-JOB-DESC(WS-JOBS-COUNT)
                            WS-JOB-EMPLOYER(WS-JOBS-COUNT)
                            WS-JOB-LOCATION(WS-JOBS-COUNT)
                            WS-JOB-SALARY(WS-JOBS-COUNT)
                   END-UNSTRING
                   ADD 1 TO WS-JOBS-HIGHEST-ID
                   MOVE WS-JOBS-HIGHEST-ID TO WS-JOB-ID(WS-JOBS-COUNT)
               END-IF
           END-IF
           EXIT.

       SAVE-JOBS.
           SET JOBS-IO-OK TO TRUE
           OPEN OUTPUT JOBS-FILE
           IF WS-JOBS-FILE-STATUS = "00"
               PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-JOBS-COUNT
                   MOVE SPACES TO JOB-REC
                   MOVE WS-JOB-ID(WS-I) TO WS-JOB-ID-DISPLAY
                   MOVE SPACES           TO WS-JOB-ID-TEXT
                   MOVE SPACES           TO WS-JOB-ID-TEXT
                   MOVE WS-JOB-ID-DISPLAY TO WS-JOB-ID-TEXT
                   STRING
                       FUNCTION TRIM(WS-JOB-ID-TEXT)           DELIMITED BY SIZE
                       FUNCTION TRIM(WS-JOB-ID-TEXT)           DELIMITED BY SIZE
                       "|"                                     DELIMITED BY SIZE
                       FUNCTION TRIM(WS-JOB-POSTER-USER(WS-I)) DELIMITED BY SIZE
                       "|"                                     DELIMITED BY SIZE
                       FUNCTION TRIM(WS-JOB-TITLE(WS-I))       DELIMITED BY SIZE
                       "|"                                     DELIMITED BY SIZE
                       FUNCTION TRIM(WS-JOB-DESC(WS-I))        DELIMITED BY SIZE
                       "|"                                     DELIMITED BY SIZE
                       FUNCTION TRIM(WS-JOB-EMPLOYER(WS-I))    DELIMITED BY SIZE
                       "|"                                     DELIMITED BY SIZE
                       FUNCTION TRIM(WS-JOB-LOCATION(WS-I))    DELIMITED BY SIZE
                       "|"                                     DELIMITED BY SIZE
                       FUNCTION TRIM(WS-JOB-SALARY(WS-I))      DELIMITED BY SIZE
                       INTO JOB-REC
                   END-STRING
                   WRITE JOB-REC
                   IF WS-JOBS-FILE-STATUS NOT = "00"
                       SET JOBS-IO-ERROR TO TRUE
                       MOVE "writing jobs file" TO WS-JOBS-ERR-CONTEXT
                       PERFORM REPORT-JOBS-FILE-ERROR
                       EXIT PERFORM
                   END-IF
               END-PERFORM
               CLOSE JOBS-FILE
               IF WS-JOBS-FILE-STATUS NOT = "00"
                   SET JOBS-IO-ERROR TO TRUE
                   MOVE "closing jobs file after save" TO WS-JOBS-ERR-CONTEXT
                   PERFORM REPORT-JOBS-FILE-ERROR
               END-IF
           ELSE
               SET JOBS-IO-ERROR TO TRUE
               MOVE "opening jobs file for save" TO WS-JOBS-ERR-CONTEXT
               PERFORM REPORT-JOBS-FILE-ERROR
           END-IF
           EXIT.

       REPORT-JOBS-FILE-ERROR.
           MOVE SPACES TO WS-MSG
           STRING
               "Error: "                          DELIMITED BY SIZE
               FUNCTION TRIM(WS-JOBS-ERR-CONTEXT) DELIMITED BY SIZE
               " (status "                        DELIMITED BY SIZE
               WS-JOBS-FILE-STATUS                DELIMITED BY SIZE
               " (status "                        DELIMITED BY SIZE
               WS-JOBS-FILE-STATUS                DELIMITED BY SIZE
               ")."                                DELIMITED BY SIZE
               INTO WS-MSG
           END-STRING
           PERFORM DISPLAY-AND-LOG
           EXIT.

       DISPLAY-JOB-LIST.
           MOVE MSG-JOBS-LIST-HEADER TO WS-MSG PERFORM DISPLAY-AND-LOG
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-JOBS-COUNT
               MOVE WS-I TO WS-IDX-DISPLAY
               MOVE SPACES TO WS-MSG
               STRING
                   "   "                                  DELIMITED BY SIZE
                   FUNCTION TRIM(WS-JOB-TITLE(WS-I))      DELIMITED BY SIZE
                   " at "                                 DELIMITED BY SIZE
                   FUNCTION TRIM(WS-JOB-EMPLOYER(WS-I))   DELIMITED BY SIZE
                   "   "                                  DELIMITED BY SIZE
                   FUNCTION TRIM(WS-JOB-TITLE(WS-I))      DELIMITED BY SIZE
                   " at "                                 DELIMITED BY SIZE
                   FUNCTION TRIM(WS-JOB-EMPLOYER(WS-I))   DELIMITED BY SIZE
                   " ("                                  DELIMITED BY SIZE
                   FUNCTION TRIM(WS-JOB-LOCATION(WS-I))   DELIMITED BY SIZE
                   ")"                                    DELIMITED BY SIZE
                   FUNCTION TRIM(WS-JOB-LOCATION(WS-I))   DELIMITED BY SIZE
                   ")"                                    DELIMITED BY SIZE
                   INTO WS-MSG
               END-STRING
               PERFORM DISPLAY-AND-LOG
           END-PERFORM
           EXIT.

       VIEW-MY-APPLICATIONS.
           MOVE MSG-APPS-HEADER TO WS-MSG PERFORM DISPLAY-AND-LOG
           MOVE SPACES TO WS-MSG
           STRING MSG-APPS-USER-SUMMARY        DELIMITED BY ' '
                  " "                          DELIMITED BY SIZE
           STRING MSG-APPS-USER-SUMMARY        DELIMITED BY ' '
                  " "                          DELIMITED BY SIZE
                  FUNCTION TRIM(WS-CURRENT-USERNAME) DELIMITED BY SIZE
                  INTO WS-MSG
           END-STRING
           PERFORM DISPLAY-AND-LOG

           MOVE MSG-APPS-SEP-TOP TO WS-MSG PERFORM DISPLAY-AND-LOG

           MOVE 0 TO WS-TMP-COUNT

           *> Loop through all applications
           PERFORM VARYING APP-IDX FROM WS-APPLICATIONS-COUNT BY -1 UNTIL APP-IDX < 1
               *> Check if the application belongs to the current user
               IF FUNCTION TRIM(WS-APP-USER(APP-IDX)) = FUNCTION TRIM(WS-CURRENT-USERNAME)
                   *> Found an application. Now find the job details.
                   SET MATCH-NOT-FOUND TO TRUE
                   PERFORM VARYING JOB-IDX FROM 1 BY 1
                       UNTIL JOB-IDX > WS-JOBS-COUNT OR MATCH-FOUND
                       IF WS-JOB-ID(JOB-IDX) = WS-APP-JOB-ID(APP-IDX)
                           SET MATCH-FOUND TO TRUE

                           *> Display separator if this is not the first job found
                           IF WS-TMP-COUNT > 0
                               MOVE MSG-APPS-SEP-ITEM TO WS-MSG
                               PERFORM DISPLAY-AND-LOG
                           END-IF
                           ADD 1 TO WS-TMP-COUNT

                           *> Display Job Details
                           MOVE SPACES TO WS-MSG
                           STRING "Job Title: " FUNCTION TRIM(WS-JOB-TITLE(JOB-IDX))
                                  INTO WS-MSG END-STRING
                           PERFORM DISPLAY-AND-LOG

                           MOVE SPACES TO WS-MSG
                           STRING "Employer: " FUNCTION TRIM(WS-JOB-EMPLOYER(JOB-IDX))
                                  INTO WS-MSG END-STRING
                           PERFORM DISPLAY-AND-LOG

                           MOVE SPACES TO WS-MSG
                           STRING "Location: " FUNCTION TRIM(WS-JOB-LOCATION(JOB-IDX))
                                  INTO WS-MSG END-STRING
                           PERFORM DISPLAY-AND-LOG
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM

           MOVE MSG-APPS-SEP-FOOTER TO WS-MSG PERFORM DISPLAY-AND-LOG

           *> Display total count and final separator
           IF WS-TMP-COUNT = 0
               MOVE MSG-NO-APPS-FOUND TO WS-MSG PERFORM DISPLAY-AND-LOG
           ELSE
               MOVE SPACES TO WS-MSG
               MOVE WS-TMP-COUNT TO WS-IDX-DISPLAY
               STRING MSG-APPS-TOTAL FUNCTION TRIM(WS-IDX-DISPLAY) INTO WS-MSG END-STRING
               PERFORM DISPLAY-AND-LOG
           END-IF

           MOVE MSG-APPS-SEP-FOOTER TO WS-MSG PERFORM DISPLAY-AND-LOG
           EXIT.

       *> ===============================================================
       *> UNIT TESTS: enter "TEST-JOBS" at main menu
       *> ===============================================================
       UNIT-TESTS-JOBS.
           MOVE "=== RUN UNIT TESTS: JOBS ===" TO WS-MSG PERFORM DISPLAY-AND-LOG
           SET TEST-MODE-ON TO TRUE

           MOVE WS-JOBS-COUNT TO SAVE-JOBS-COUNT
           MOVE 0 TO WS-JOBS-COUNT
           PERFORM BROWSE-JOBS
           MOVE "TEST 1 (empty browse): PASS" TO WS-MSG PERFORM DISPLAY-AND-LOG

           IF SAVE-JOBS-COUNT = 0
               ADD 1 TO WS-JOBS-COUNT
               MOVE 1           TO WS-JOB-ID(WS-JOBS-COUNT)
               MOVE 1           TO WS-JOB-ID(WS-JOBS-COUNT)
               MOVE "Test Title" TO WS-JOB-TITLE(WS-JOBS-COUNT)
               MOVE "Test Desc"  TO WS-JOB-DESC(WS-JOBS-COUNT)
               MOVE "TestCo"    TO WS-JOB-EMPLOYER(WS-JOBS-COUNT)
               MOVE "TestCo"    TO WS-JOB-EMPLOYER(WS-JOBS-COUNT)
               MOVE "Tampa, FL"  TO WS-JOB-LOCATION(WS-JOBS-COUNT)
               MOVE "NONE"      TO WS-JOB-SALARY(WS-JOBS-COUNT)
               MOVE "NONE"      TO WS-JOB-SALARY(WS-JOBS-COUNT)
           ELSE
               MOVE SAVE-JOBS-COUNT TO WS-JOBS-COUNT
           END-IF
           MOVE 1 TO WS-I
           PERFORM DISPLAY-JOB-DETAILS
           MOVE "TEST 2 (details view): PASS" TO WS-MSG PERFORM DISPLAY-AND-LOG

           MOVE WS-APPLICATIONS-COUNT TO SAVE-APPS-COUNT
           PERFORM APPLY-FOR-JOB
           IF WS-APPLICATIONS-COUNT = SAVE-APPS-COUNT + 1
               MOVE "TEST 3a (first apply): PASS" TO WS-MSG PERFORM DISPLAY-AND-LOG
           ELSE
               MOVE "TEST 3a (first apply): FAIL" TO WS-MSG PERFORM DISPLAY-AND-LOG
           END-IF

           PERFORM APPLY-FOR-JOB
           IF WS-APPLICATIONS-COUNT = SAVE-APPS-COUNT + 1
               MOVE "TEST 3b (duplicate apply blocked): PASS" TO WS-MSG PERFORM DISPLAY-AND-LOG
           ELSE
               MOVE "TEST 3b (duplicate apply blocked): FAIL" TO WS-MSG PERFORM DISPLAY-AND-LOG
           END-IF

           SET TEST-MODE-OFF TO TRUE
           MOVE "=== UNIT TESTS DONE ===" TO WS-MSG PERFORM DISPLAY-AND-LOG
           EXIT.


       MESSAGES-SECTION.
       MESSAGE-MENU.
           MOVE MSG-MESSAGES-HEADER TO WS-MSG
           MOVE MSG-MESSAGES-HEADER TO WS-MSG
           PERFORM DISPLAY-AND-LOG
           PERFORM UNTIL WS-MESSAGE-CHOICE = '3' OR EOF-IN
               MOVE MSG-MESSAGES-SEND TO WS-MSG
               MOVE MSG-MESSAGES-SEND TO WS-MSG
               PERFORM DISPLAY-AND-LOG
               MOVE MSG-MESSAGES-VIEW TO WS-MSG
               MOVE MSG-MESSAGES-VIEW TO WS-MSG
               PERFORM DISPLAY-AND-LOG
               MOVE MSG-MESSAGES-BACK TO WS-MSG
               MOVE MSG-MESSAGES-BACK TO WS-MSG
               PERFORM DISPLAY-AND-LOG

               MOVE MSG-ENTER-CHOICE TO WS-MSG

               MOVE MSG-ENTER-CHOICE TO WS-MSG
               PERFORM DISPLAY-AND-LOG
               PERFORM READ-NEXT-LINE
               MOVE WS-LINE TO WS-MESSAGE-CHOICE
               IF EOF-IN
                   EXIT PERFORM
               END-IF


               EVALUATE WS-MESSAGE-CHOICE
                   WHEN '1'
                       PERFORM SEND-MESSAGE
                   WHEN '2'
                       PERFORM VIEW-MESSAGES
                   WHEN '3'
                       EXIT PERFORM
                   WHEN OTHER
                       MOVE MSG-INVALID-CHOICE TO WS-MSG
                       MOVE MSG-INVALID-CHOICE TO WS-MSG
                       PERFORM DISPLAY-AND-LOG
               END-EVALUATE
           END-PERFORM
           MOVE SPACES TO WS-MESSAGE-CHOICE
           EXIT.


       SEND-MESSAGE.
           MOVE MSG-ENTER-RECEIVER TO WS-MSG
           PERFORM DISPLAY-AND-LOG

           PERFORM READ-NEXT-LINE
           MOVE WS-LINE TO WS-RECEIVER
           IF EOF-IN
               EXIT PARAGRAPH
           END-IF

           MOVE MSG-ENTER-CONTENT TO WS-MSG
           PERFORM DISPLAY-AND-LOG

           PERFORM READ-NEXT-LINE
           MOVE WS-LINE TO WS-CONTENT
           IF EOF-IN
               EXIT PARAGRAPH
           END-IF

           *> Validate receiver exists and is a connection
           PERFORM VALIDATE-RECEIVER

           IF MATCH-NOT-FOUND
               EXIT PARAGRAPH
           END-IF

           *> Validate message content (empty check)
           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-CONTENT))
               TO WS-CONTENT-LENGTH

           IF WS-CONTENT-LENGTH = 0
               MOVE "Message cannot be empty. Please try again." TO WS-MSG
               PERFORM DISPLAY-AND-LOG
               EXIT PARAGRAPH
           END-IF

           *> Validate message length (200 char max)
           IF WS-CONTENT-LENGTH > 200
               MOVE "Message exceeds 200 characters. Please try again." TO WS-MSG
               PERFORM DISPLAY-AND-LOG
               EXIT PARAGRAPH
           END-IF

           *> Save message to table
           IF WS-MESSAGES-COUNT < WS-MESSAGES-MAX
               ADD 1 TO WS-MESSAGES-COUNT
               MOVE WS-CURRENT-USERNAME TO WS-MSG-SENDER-ENTRY(WS-MESSAGES-COUNT)
               MOVE FUNCTION TRIM(WS-RECEIVER)
                   TO WS-MSG-RECEIVER-ENTRY(WS-MESSAGES-COUNT)
               MOVE FUNCTION TRIM(WS-CONTENT)
                   TO WS-MSG-CONTENT-ENTRY(WS-MESSAGES-COUNT)
           END-IF

           *> Get current timestamp using built-in function CURRENT-DATE
           *> This return a string in the format YYYYMMDDHHMMSSmmmmmm
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-T4  *> YYYYMMDD
           MOVE FUNCTION CURRENT-DATE(9:6) TO WS-T4(9:6)  *> HHMMSS
           MOVE WS-T4 TO WS-MSG-TIMESTAMP-ENTRY(WS-MESSAGES-COUNT)

           *> Get current timestamp using built-in function CURRENT-DATE
           *> This return a string in the format YYYYMMDDHHMMSSmmmmmm
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-T4  *> YYYYMMDD
           MOVE FUNCTION CURRENT-DATE(9:6) TO WS-T4(9:6)  *> HHMMSS
           MOVE WS-T4 TO WS-MSG-TIMESTAMP-ENTRY(WS-MESSAGES-COUNT)

           *> Save to file
           PERFORM SAVE-MESSAGES

           *> Display success message
           MOVE SPACES TO WS-MSG
           STRING
               MSG-SEND-SUCCESS-1         DELIMITED BY SIZE
               MSG-SEND-SUCCESS-1         DELIMITED BY SIZE
               FUNCTION TRIM(WS-RECEIVER)   DELIMITED BY SIZE
               MSG-SEND-SUCCESS-2         DELIMITED BY SIZE
               MSG-SEND-SUCCESS-2         DELIMITED BY SIZE
               INTO WS-MSG
           END-STRING
           PERFORM DISPLAY-AND-LOG

           MOVE MSG-MESSAGES-FOOTER TO WS-MSG
           PERFORM DISPLAY-AND-LOG
           EXIT.

       VALIDATE-RECEIVER.
           SET MATCH-NOT-FOUND TO TRUE

           *> Step 1: Check if recipient exists in users table
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-USERS-COUNT OR MATCH-FOUND
               IF FUNCTION TRIM(WS-TBL-USERNAME(WS-I)) =
               IF FUNCTION TRIM(WS-TBL-USERNAME(WS-I)) =
                  FUNCTION TRIM(WS-RECEIVER)
                   SET MATCH-FOUND TO TRUE
               END-IF
           END-PERFORM

           IF MATCH-NOT-FOUND
               MOVE MSG-NOT-CONNECTED TO WS-MSG
               PERFORM DISPLAY-AND-LOG
               EXIT PARAGRAPH
           END-IF

           *> Step 2: Check if they are connected (status = 'A')
           SET MATCH-NOT-FOUND TO TRUE
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-CONNECTIONS-COUNT OR MATCH-FOUND
               IF WS-CONN-STATUS(WS-I) = 'A'
                   IF (WS-CONN-SENDER(WS-I) = WS-CURRENT-USERNAME AND
                       WS-CONN-RECEIVER(WS-I) = WS-RECEIVER)
                   OR (WS-CONN-SENDER(WS-I) = WS-RECEIVER AND
                       WS-CONN-RECEIVER(WS-I) = WS-CURRENT-USERNAME)
                       SET MATCH-FOUND TO TRUE
                   END-IF
               END-IF
           END-PERFORM

           IF MATCH-NOT-FOUND
               MOVE MSG-NOT-CONNECTED TO WS-MSG
               PERFORM DISPLAY-AND-LOG
           END-IF

           EXIT.
    
       *> Sort messages chronologically (oldest to newest)
       *> Uses bubble sort algorithm on timestamp field
       *> Only sorts messages for current user to maintain efficiency
       SORT-MESSAGES-BY-TIMESTAMP.
           MOVE 0 TO WS-J
           PERFORM VARYING WS-I FROM 1 BY 1 
               UNTIL WS-I >= WS-MESSAGES-COUNT
               PERFORM VARYING WS-J FROM 1 BY 1 
                   UNTIL WS-J > (WS-MESSAGES-COUNT - WS-I)
                   
                   *> Compare timestamps of adjacent messages
                   IF WS-MSG-TIMESTAMP-ENTRY(WS-J) > 
                      WS-MSG-TIMESTAMP-ENTRY(WS-J + 1)
                       *> Swap all fields
                       MOVE WS-MSG-SENDER-ENTRY(WS-J) TO WS-T1
                       MOVE WS-MSG-SENDER-ENTRY(WS-J + 1) 
                           TO WS-MSG-SENDER-ENTRY(WS-J)
                       MOVE WS-T1 TO WS-MSG-SENDER-ENTRY(WS-J + 1)
                       
                       MOVE WS-MSG-RECEIVER-ENTRY(WS-J) TO WS-T2
                       MOVE WS-MSG-RECEIVER-ENTRY(WS-J + 1) 
                           TO WS-MSG-RECEIVER-ENTRY(WS-J)
                       MOVE WS-T2 TO WS-MSG-RECEIVER-ENTRY(WS-J + 1)
                       
                       MOVE WS-MSG-CONTENT-ENTRY(WS-J) TO WS-T3
                       MOVE WS-MSG-CONTENT-ENTRY(WS-J + 1) 
                           TO WS-MSG-CONTENT-ENTRY(WS-J)
                       MOVE WS-T3 TO WS-MSG-CONTENT-ENTRY(WS-J + 1)
                       
                       MOVE WS-MSG-TIMESTAMP-ENTRY(WS-J) TO WS-T4
                       MOVE WS-MSG-TIMESTAMP-ENTRY(WS-J + 1) 
                           TO WS-MSG-TIMESTAMP-ENTRY(WS-J)
                       MOVE WS-T4 TO WS-MSG-TIMESTAMP-ENTRY(WS-J + 1)
                   END-IF
               END-PERFORM
           END-PERFORM
           EXIT.
    
       *> Sort messages chronologically (oldest to newest)
       *> Uses bubble sort algorithm on timestamp field
       *> Only sorts messages for current user to maintain efficiency
       SORT-MESSAGES-BY-TIMESTAMP.
           MOVE 0 TO WS-J
           PERFORM VARYING WS-I FROM 1 BY 1 
               UNTIL WS-I >= WS-MESSAGES-COUNT
               PERFORM VARYING WS-J FROM 1 BY 1 
                   UNTIL WS-J > (WS-MESSAGES-COUNT - WS-I)
                   
                   *> Compare timestamps of adjacent messages
                   IF WS-MSG-TIMESTAMP-ENTRY(WS-J) > 
                      WS-MSG-TIMESTAMP-ENTRY(WS-J + 1)
                       *> Swap all fields
                       MOVE WS-MSG-SENDER-ENTRY(WS-J) TO WS-T1
                       MOVE WS-MSG-SENDER-ENTRY(WS-J + 1) 
                           TO WS-MSG-SENDER-ENTRY(WS-J)
                       MOVE WS-T1 TO WS-MSG-SENDER-ENTRY(WS-J + 1)
                       
                       MOVE WS-MSG-RECEIVER-ENTRY(WS-J) TO WS-T2
                       MOVE WS-MSG-RECEIVER-ENTRY(WS-J + 1) 
                           TO WS-MSG-RECEIVER-ENTRY(WS-J)
                       MOVE WS-T2 TO WS-MSG-RECEIVER-ENTRY(WS-J + 1)
                       
                       MOVE WS-MSG-CONTENT-ENTRY(WS-J) TO WS-T3
                       MOVE WS-MSG-CONTENT-ENTRY(WS-J + 1) 
                           TO WS-MSG-CONTENT-ENTRY(WS-J)
                       MOVE WS-T3 TO WS-MSG-CONTENT-ENTRY(WS-J + 1)
                       
                       MOVE WS-MSG-TIMESTAMP-ENTRY(WS-J) TO WS-T4
                       MOVE WS-MSG-TIMESTAMP-ENTRY(WS-J + 1) 
                           TO WS-MSG-TIMESTAMP-ENTRY(WS-J)
                       MOVE WS-T4 TO WS-MSG-TIMESTAMP-ENTRY(WS-J + 1)
                   END-IF
               END-PERFORM
           END-PERFORM
           EXIT.
    
       VIEW-MESSAGES.
      *> IMPLEMENTED FOR EPIC 9
      *> Purpose: Displays all messages received by the currently logged-in user
      *> Uses a two-pass approach: first counts messages, then displays them
      *> This allows early exit if no messages are found
        *> Display header
        MOVE "--- Your Messages ---" TO WS-MSG
        PERFORM DISPLAY-AND-LOG

        *> Pass 1: Count messages for current user
        *> Iterate through all messages to determine if user has any messages
        MOVE 0 TO WS-TMP-COUNT
        PERFORM VARYING WS-I FROM 1 BY 1
            UNTIL WS-I > WS-MESSAGES-COUNT
            IF FUNCTION TRIM(WS-MSG-RECEIVER-ENTRY(WS-I)) =
            FUNCTION TRIM(WS-CURRENT-USERNAME)
                ADD 1 TO WS-TMP-COUNT
            END-IF
        END-PERFORM

        *> Early exit if no messages found
        *> Display "no messages" message and footer, then return to menu
        IF WS-TMP-COUNT = 0
            MOVE "You have no messages at this time." TO WS-MSG
            PERFORM DISPLAY-AND-LOG
            MOVE MSG-MESSAGES-FOOTER TO WS-MSG
            PERFORM DISPLAY-AND-LOG
            EXIT PARAGRAPH
        END-IF

        PERFORM SORT-MESSAGES-BY-TIMESTAMP

        *> Pass 2: Display all messages for the current user
        *> Loop through messages again, displaying only those for current user
        PERFORM VARYING WS-I FROM 1 BY 1
            UNTIL WS-I > WS-MESSAGES-COUNT
            IF FUNCTION TRIM(WS-MSG-RECEIVER-ENTRY(WS-I)) =
            FUNCTION TRIM(WS-CURRENT-USERNAME)
                *> Display sender information
                MOVE SPACES TO WS-MSG
                STRING "From: " DELIMITED BY SIZE
                    FUNCTION TRIM(WS-MSG-SENDER-ENTRY(WS-I))
                    DELIMITED BY SIZE
                    INTO WS-MSG
                END-STRING
                PERFORM DISPLAY-AND-LOG

                *> Display message content
                MOVE SPACES TO WS-MSG
                STRING "Message: " DELIMITED BY SIZE
                    FUNCTION TRIM(WS-MSG-CONTENT-ENTRY(WS-I))
                    DELIMITED BY SIZE
                    INTO WS-MSG
                END-STRING
                PERFORM DISPLAY-AND-LOG

                *> Display timestamp
                PERFORM FORMAT-TIMESTAMP
                MOVE SPACES TO WS-MSG
                STRING "Sent: " DELIMITED BY SIZE
                   FUNCTION TRIM(WS-FORMATTED-TS)
                   INTO WS-MSG
                END-STRING
                PERFORM DISPLAY-AND-LOG

                *> Display separator between messages for visual clarity
                MOVE "---" TO WS-MSG
                PERFORM DISPLAY-AND-LOG
            END-IF
        END-PERFORM

        *> Display footer separator before returning to menu
        MOVE MSG-MESSAGES-FOOTER TO WS-MSG
        PERFORM DISPLAY-AND-LOG
        EXIT.
      *> IMPLEMENTED FOR EPIC 9
      *> Purpose: Displays all messages received by the currently logged-in user
      *> Uses a two-pass approach: first counts messages, then displays them
      *> This allows early exit if no messages are found
        *> Display header
        MOVE "--- Your Messages ---" TO WS-MSG
        PERFORM DISPLAY-AND-LOG

        *> Pass 1: Count messages for current user
        *> Iterate through all messages to determine if user has any messages
        MOVE 0 TO WS-TMP-COUNT
        PERFORM VARYING WS-I FROM 1 BY 1
            UNTIL WS-I > WS-MESSAGES-COUNT
            IF FUNCTION TRIM(WS-MSG-RECEIVER-ENTRY(WS-I)) =
            FUNCTION TRIM(WS-CURRENT-USERNAME)
                ADD 1 TO WS-TMP-COUNT
            END-IF
        END-PERFORM

        *> Early exit if no messages found
        *> Display "no messages" message and footer, then return to menu
        IF WS-TMP-COUNT = 0
            MOVE "You have no messages at this time." TO WS-MSG
            PERFORM DISPLAY-AND-LOG
            MOVE MSG-MESSAGES-FOOTER TO WS-MSG
            PERFORM DISPLAY-AND-LOG
            EXIT PARAGRAPH
        END-IF

        PERFORM SORT-MESSAGES-BY-TIMESTAMP

        *> Pass 2: Display all messages for the current user
        *> Loop through messages again, displaying only those for current user
        PERFORM VARYING WS-I FROM 1 BY 1
            UNTIL WS-I > WS-MESSAGES-COUNT
            IF FUNCTION TRIM(WS-MSG-RECEIVER-ENTRY(WS-I)) =
            FUNCTION TRIM(WS-CURRENT-USERNAME)
                *> Display sender information
                MOVE SPACES TO WS-MSG
                STRING "From: " DELIMITED BY SIZE
                    FUNCTION TRIM(WS-MSG-SENDER-ENTRY(WS-I))
                    DELIMITED BY SIZE
                    INTO WS-MSG
                END-STRING
                PERFORM DISPLAY-AND-LOG

                *> Display message content
                MOVE SPACES TO WS-MSG
                STRING "Message: " DELIMITED BY SIZE
                    FUNCTION TRIM(WS-MSG-CONTENT-ENTRY(WS-I))
                    DELIMITED BY SIZE
                    INTO WS-MSG
                END-STRING
                PERFORM DISPLAY-AND-LOG

                *> Display timestamp
                PERFORM FORMAT-TIMESTAMP
                MOVE SPACES TO WS-MSG
                STRING "Sent: " DELIMITED BY SIZE
                   FUNCTION TRIM(WS-FORMATTED-TS)
                   INTO WS-MSG
                END-STRING
                PERFORM DISPLAY-AND-LOG

                *> Display separator between messages for visual clarity
                MOVE "---" TO WS-MSG
                PERFORM DISPLAY-AND-LOG
            END-IF
        END-PERFORM

        *> Display footer separator before returning to menu
        MOVE MSG-MESSAGES-FOOTER TO WS-MSG
        PERFORM DISPLAY-AND-LOG
        EXIT.

       SAVE-MESSAGES.
      *> IMPLEMENTED FOR EPIC 8
      *> Purpose: Persists all messages from memory to messages.txt file
      *> Format: sender|receiver|content (pipe-delimited, one per line)
        OPEN OUTPUT MESSAGES-FILE
        PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-MESSAGES-COUNT
            MOVE SPACES TO MESSAGE-REC
            STRING
                FUNCTION TRIM(WS-MSG-SENDER-ENTRY(WS-I))   DELIMITED BY SIZE
                "|"                                         DELIMITED BY SIZE
                FUNCTION TRIM(WS-MSG-RECEIVER-ENTRY(WS-I)) DELIMITED BY SIZE
                "|"                                         DELIMITED BY SIZE
                FUNCTION TRIM(WS-MSG-CONTENT-ENTRY(WS-I))  DELIMITED BY SIZE
                "|"                                         DELIMITED BY SIZE
                FUNCTION TRIM(WS-MSG-TIMESTAMP-ENTRY(WS-I)) DELIMITED BY SIZE
                INTO MESSAGE-REC
            END-STRING
            WRITE MESSAGE-REC
        END-PERFORM
        CLOSE MESSAGES-FILE
        EXIT.
      *> IMPLEMENTED FOR EPIC 8
      *> Purpose: Persists all messages from memory to messages.txt file
      *> Format: sender|receiver|content (pipe-delimited, one per line)
        OPEN OUTPUT MESSAGES-FILE
        PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-MESSAGES-COUNT
            MOVE SPACES TO MESSAGE-REC
            STRING
                FUNCTION TRIM(WS-MSG-SENDER-ENTRY(WS-I))   DELIMITED BY SIZE
                "|"                                         DELIMITED BY SIZE
                FUNCTION TRIM(WS-MSG-RECEIVER-ENTRY(WS-I)) DELIMITED BY SIZE
                "|"                                         DELIMITED BY SIZE
                FUNCTION TRIM(WS-MSG-CONTENT-ENTRY(WS-I))  DELIMITED BY SIZE
                "|"                                         DELIMITED BY SIZE
                FUNCTION TRIM(WS-MSG-TIMESTAMP-ENTRY(WS-I)) DELIMITED BY SIZE
                INTO MESSAGE-REC
            END-STRING
            WRITE MESSAGE-REC
        END-PERFORM
        CLOSE MESSAGES-FILE
        EXIT.



       INIT-LOAD-MESSAGES.
           MOVE 0 TO WS-MESSAGES-COUNT
           OPEN INPUT MESSAGES-FILE
           IF WS-MSG-FILE-STATUS = "00"
               SET NOT-EOF-MSG TO TRUE
               PERFORM UNTIL EOF-MSG
                   READ MESSAGES-FILE
                       AT END SET EOF-MSG TO TRUE
                       NOT AT END PERFORM PARSE-MESSAGE-REC
                   END-READ
               END-PERFORM
               CLOSE MESSAGES-FILE
           END-IF
           EXIT.

       PARSE-MESSAGE-REC.
           INITIALIZE WS-T1 WS-T2 WS-T3 WS-T4
           *> Format: sender|receiver|content|timestamp
           INITIALIZE WS-T1 WS-T2 WS-T3 WS-T4
           *> Format: sender|receiver|content|timestamp
           UNSTRING MESSAGE-REC DELIMITED BY '|'
               INTO WS-T1 WS-T2 WS-T3 WS-T4
               INTO WS-T1 WS-T2 WS-T3 WS-T4
           END-UNSTRING
           IF WS-T1 NOT = SPACES AND WS-MESSAGES-COUNT < WS-MESSAGES-MAX
               ADD 1 TO WS-MESSAGES-COUNT
               MOVE FUNCTION TRIM(WS-T1) TO WS-MSG-SENDER-ENTRY(WS-MESSAGES-COUNT)
               MOVE FUNCTION TRIM(WS-T2) TO WS-MSG-RECEIVER-ENTRY(WS-MESSAGES-COUNT)
               MOVE FUNCTION TRIM(WS-T3) TO WS-MSG-CONTENT-ENTRY(WS-MESSAGES-COUNT)
               MOVE FUNCTION TRIM(WS-T4) TO WS-MSG-TIMESTAMP-ENTRY(WS-MESSAGES-COUNT)
               MOVE FUNCTION TRIM(WS-T4) TO WS-MSG-TIMESTAMP-ENTRY(WS-MESSAGES-COUNT)
           END-IF
           EXIT.

       FORMAT-TIMESTAMP.
           *> Input: WS-MSG-TIMESTAMP-ENTRY(WS-I) = YYYYMMDDHHmmSS
           *> Output: WS-FORMATTED-TS = YYYY-MM-DD HH:MM
           
           MOVE SPACES TO WS-FORMATTED-TS
           
           IF WS-MSG-TIMESTAMP-ENTRY(WS-I) = SPACES OR
              WS-MSG-TIMESTAMP-ENTRY(WS-I) = LOW-VALUES
               MOVE "N/A" TO WS-FORMATTED-TS
               EXIT PARAGRAPH
           END-IF
           
           *> Extract components from YYYYMMDDHHmmSS (14 chars)
           MOVE WS-MSG-TIMESTAMP-ENTRY(WS-I)(1:4)  TO WS-TS-YEAR
           MOVE WS-MSG-TIMESTAMP-ENTRY(WS-I)(5:2)  TO WS-TS-MONTH
           MOVE WS-MSG-TIMESTAMP-ENTRY(WS-I)(7:2)  TO WS-TS-DAY
           MOVE WS-MSG-TIMESTAMP-ENTRY(WS-I)(9:2)  TO WS-TS-HOUR
           MOVE WS-MSG-TIMESTAMP-ENTRY(WS-I)(11:2) TO WS-TS-MINUTE
           
           *> Build formatted string: YYYY-MM-DD HH:MM
           STRING
               WS-TS-YEAR      DELIMITED BY SIZE
               "-"             DELIMITED BY SIZE
               WS-TS-MONTH     DELIMITED BY SIZE
               "-"             DELIMITED BY SIZE
               WS-TS-DAY       DELIMITED BY SIZE
               " "             DELIMITED BY SIZE
               WS-TS-HOUR      DELIMITED BY SIZE
               ":"             DELIMITED BY SIZE
               WS-TS-MINUTE    DELIMITED BY SIZE
               INTO WS-FORMATTED-TS
           END-STRING

           EXIT.
       FORMAT-TIMESTAMP.
           *> Input: WS-MSG-TIMESTAMP-ENTRY(WS-I) = YYYYMMDDHHmmSS
           *> Output: WS-FORMATTED-TS = YYYY-MM-DD HH:MM
           
           MOVE SPACES TO WS-FORMATTED-TS
           
           IF WS-MSG-TIMESTAMP-ENTRY(WS-I) = SPACES OR
              WS-MSG-TIMESTAMP-ENTRY(WS-I) = LOW-VALUES
               MOVE "N/A" TO WS-FORMATTED-TS
               EXIT PARAGRAPH
           END-IF
           
           *> Extract components from YYYYMMDDHHmmSS (14 chars)
           MOVE WS-MSG-TIMESTAMP-ENTRY(WS-I)(1:4)  TO WS-TS-YEAR
           MOVE WS-MSG-TIMESTAMP-ENTRY(WS-I)(5:2)  TO WS-TS-MONTH
           MOVE WS-MSG-TIMESTAMP-ENTRY(WS-I)(7:2)  TO WS-TS-DAY
           MOVE WS-MSG-TIMESTAMP-ENTRY(WS-I)(9:2)  TO WS-TS-HOUR
           MOVE WS-MSG-TIMESTAMP-ENTRY(WS-I)(11:2) TO WS-TS-MINUTE
           
           *> Build formatted string: YYYY-MM-DD HH:MM
           STRING
               WS-TS-YEAR      DELIMITED BY SIZE
               "-"             DELIMITED BY SIZE
               WS-TS-MONTH     DELIMITED BY SIZE
               "-"             DELIMITED BY SIZE
               WS-TS-DAY       DELIMITED BY SIZE
               " "             DELIMITED BY SIZE
               WS-TS-HOUR      DELIMITED BY SIZE
               ":"             DELIMITED BY SIZE
               WS-TS-MINUTE    DELIMITED BY SIZE
               INTO WS-FORMATTED-TS
           END-STRING

           EXIT.

       HELPER-SECTION.
       DISPLAY-AND-LOG.
           MOVE SPACES TO OUTPUT-REC
           MOVE FUNCTION TRIM(WS-MSG TRAILING) TO OUTPUT-REC
           WRITE OUTPUT-REC
           DISPLAY FUNCTION TRIM(WS-MSG TRAILING)
           EXIT.

       READ-NEXT-LINE.
           MOVE SPACES TO WS-LINE
           READ INPUT-FILE
               AT END SET EOF-IN TO TRUE
               NOT AT END
                   MOVE FUNCTION TRIM(INPUT-REC) TO WS-LINE
           END-READ
           EXIT.
