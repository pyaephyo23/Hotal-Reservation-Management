        IDENTIFICATION DIVISION.
       PROGRAM-ID. viewGuests.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GUEST-FILE ASSIGN TO '../DATA/GUESTS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS GUEST-ID
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  GUEST-FILE.
       COPY "./CopyBooks/GUESTS.cpy".

       WORKING-STORAGE SECTION.
       01  WS-EOF                  PIC X VALUE 'N'.
       01  WS-GUEST-COUNTER        PIC 999 VALUE 0.
       01  MENU-CHOICE             PIC 99.
       01  WS-FILE-STATUS          PIC 99.
       01  WS-SEARCH-NAME          PIC X(20).
       01  WS-FOUND                PIC X VALUE 'N'.

       *> Color codes for display
       01 RED-COLOR          PIC X(8) VALUE X"1B5B33316D".
       01 GREEN-COLOR        PIC X(8) VALUE X"1B5B33326D".
       01 RESET-COLOR        PIC X(4) VALUE X"1B5B306D".

       01  WS-HEADER-1.
           05 FILLER               PIC X(8) VALUE 'GUEST ID'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(20) VALUE 'GUEST NAME'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(3) VALUE 'AGE'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(18) VALUE 'NRC'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(6) VALUE 'GENDER'.

       01  WS-HEADER-2.
           05 FILLER               PIC X(8) VALUE '--------'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER             PIC X(20) VALUE '--------------------'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(3) VALUE '---'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(18) VALUE '--------------'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(6) VALUE '------'.

       01  WS-DETAIL-LINE.
           05 WS-DL-GUEST-ID       PIC 9(5).
           05 FILLER               PIC X(3) VALUE SPACES.
           05 WS-DL-GUEST-NAME     PIC X(20).
           05 FILLER               PIC X(2) VALUE SPACES.
           05 WS-DL-GUEST-AGE      PIC Z9.
           05 FILLER               PIC X(2) VALUE SPACES.
           05 WS-DL-GUEST-NRC      PIC X(18).
           05 FILLER               PIC X(2) VALUE SPACES.
           05 WS-DL-GUEST-GENDER   PIC X(1).

       LINKAGE SECTION.
       01 LINK PIC 9.

       PROCEDURE DIVISION USING LINK.

       MAIN-LOOP.
           PERFORM UNTIL MENU-CHOICE = 9
           DISPLAY
           "**************************************************"
           DISPLAY "View Hotel Guests"
           DISPLAY "1. View All Guests"
           DISPLAY "2. Search Guest by Name"
           DISPLAY "3. View Guests by Age Range"
           DISPLAY "4. View Guests by Gender"
           DISPLAY "9. Go Back To Main Menu"
           DISPLAY
           "**************************************************"
           ACCEPT MENU-CHOICE
           EVALUATE MENU-CHOICE
               WHEN 1 PERFORM ALL-GUESTS-DSP
               WHEN 2 PERFORM SEARCH-BY-NAME
               WHEN 3 PERFORM SEARCH-BY-AGE
               WHEN 4 PERFORM SEARCH-BY-GENDER
               WHEN 9 GOBACK
               WHEN OTHER
                   DISPLAY RED-COLOR "Invalid selection." RESET-COLOR
           END-EVALUATE
           END-PERFORM.
           GOBACK.

       ALL-GUESTS-DSP.
           MOVE 0 TO WS-GUEST-COUNTER
           MOVE 'N' TO WS-EOF
           OPEN INPUT GUEST-FILE
           IF WS-FILE-STATUS NOT = '00' AND WS-FILE-STATUS NOT = '97'
               DISPLAY RED-COLOR "Error opening guest file: "
               WS-FILE-STATUS RESET-COLOR
               MOVE 'Y' TO WS-EOF
           ELSE
               PERFORM DISPLAY-HEADERS
               PERFORM READ-AND-DISPLAY-ALL UNTIL WS-EOF = 'Y'
               PERFORM DISPLAY-SUMMARY
           END-IF
           CLOSE GUEST-FILE.

       SEARCH-BY-NAME.
           DISPLAY "Enter Guest Name to search: "
           ACCEPT WS-SEARCH-NAME
           MOVE 0 TO WS-GUEST-COUNTER
           MOVE 'N' TO WS-EOF
           MOVE 'N' TO WS-FOUND
           OPEN INPUT GUEST-FILE
           IF WS-FILE-STATUS NOT = '00' AND WS-FILE-STATUS NOT = '97'
               DISPLAY "Error opening GUEST file: " WS-FILE-STATUS
               MOVE 'Y' TO WS-EOF
           ELSE
               PERFORM DISPLAY-HEADERS
               PERFORM READ-AND-SEARCH-NAME UNTIL WS-EOF = 'Y'
               IF WS-FOUND = 'N'
                   DISPLAY RED-COLOR "No guests found with name: "
                   WS-SEARCH-NAME RESET-COLOR
               ELSE
                   PERFORM DISPLAY-SUMMARY
               END-IF
           END-IF
           CLOSE GUEST-FILE.

       SEARCH-BY-AGE.
           DISPLAY "View guests by age range:"
           DISPLAY "1. Children (0-12 years)"
           DISPLAY "2. Teenagers (13-19 years)"
           DISPLAY "3. Adults (20-59 years)"
           DISPLAY "4. Seniors (60+ years)"
           DISPLAY "Enter choice: "
           ACCEPT MENU-CHOICE

           *> Validate input choice using EVALUATE
           EVALUATE MENU-CHOICE
               WHEN 1
               WHEN 2
               WHEN 3
               WHEN 4
                   PERFORM PROCESS-AGE-SEARCH
               WHEN OTHER
                 DISPLAY RED-COLOR "Invalid choice."
                   RESET-COLOR
                   PERFORM SEARCH-BY-AGE
           END-EVALUATE.

       PROCESS-AGE-SEARCH.
           MOVE 0 TO WS-GUEST-COUNTER
           MOVE 'N' TO WS-EOF
           MOVE 'N' TO WS-FOUND
           OPEN INPUT GUEST-FILE
           IF WS-FILE-STATUS NOT = '00' AND WS-FILE-STATUS NOT = '97'
               DISPLAY "Error opening GUEST file: " WS-FILE-STATUS
               MOVE 'Y' TO WS-EOF
           ELSE
               PERFORM DISPLAY-HEADERS
               PERFORM READ-AND-SEARCH-AGE UNTIL WS-EOF = 'Y'
               IF WS-FOUND = 'N'
                   DISPLAY "No guests found in selected age range."
               ELSE
                   PERFORM DISPLAY-SUMMARY
               END-IF
           END-IF
           CLOSE GUEST-FILE.

       SEARCH-BY-GENDER.
           DISPLAY "Enter Gender (M/F): "
           ACCEPT WS-SEARCH-NAME
           MOVE 0 TO WS-GUEST-COUNTER
           MOVE 'N' TO WS-EOF
           MOVE 'N' TO WS-FOUND
           OPEN INPUT GUEST-FILE
           IF WS-FILE-STATUS NOT = '00' AND WS-FILE-STATUS NOT = '97'
               DISPLAY "Error opening GUEST file: " WS-FILE-STATUS
               MOVE 'Y' TO WS-EOF
           ELSE
               PERFORM DISPLAY-HEADERS
               PERFORM READ-AND-SEARCH-GENDER UNTIL WS-EOF = 'Y'
               IF WS-FOUND = 'N'
                  DISPLAY "No guests found with gender: " WS-SEARCH-NAME
               ELSE
                   PERFORM DISPLAY-SUMMARY
               END-IF
           END-IF
           CLOSE GUEST-FILE.

       DISPLAY-HEADERS.
           DISPLAY WS-HEADER-1
           DISPLAY WS-HEADER-2.

       READ-AND-DISPLAY-ALL.
           READ GUEST-FILE NEXT
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   PERFORM DISPLAY-GUEST-RECORD
                   ADD 1 TO WS-GUEST-COUNTER
           END-READ.

       READ-AND-SEARCH-NAME.
           READ GUEST-FILE NEXT
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   IF GUEST-NAME = WS-SEARCH-NAME
                       PERFORM DISPLAY-GUEST-RECORD
                       ADD 1 TO WS-GUEST-COUNTER
                       MOVE 'Y' TO WS-FOUND
                   END-IF
           END-READ.

       READ-AND-SEARCH-AGE.
           READ GUEST-FILE NEXT
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   EVALUATE MENU-CHOICE
                       WHEN 1
                           IF GUEST-AGE >= 0 AND GUEST-AGE <= 12
                               PERFORM DISPLAY-GUEST-RECORD
                               ADD 1 TO WS-GUEST-COUNTER
                               MOVE 'Y' TO WS-FOUND
                           END-IF
                       WHEN 2
                           IF GUEST-AGE >= 13 AND GUEST-AGE <= 19
                               PERFORM DISPLAY-GUEST-RECORD
                               ADD 1 TO WS-GUEST-COUNTER
                               MOVE 'Y' TO WS-FOUND
                           END-IF
                       WHEN 3
                           IF GUEST-AGE >= 20 AND GUEST-AGE <= 59
                               PERFORM DISPLAY-GUEST-RECORD
                               ADD 1 TO WS-GUEST-COUNTER
                               MOVE 'Y' TO WS-FOUND
                           END-IF
                       WHEN 4
                           IF GUEST-AGE >= 60
                               PERFORM DISPLAY-GUEST-RECORD
                               ADD 1 TO WS-GUEST-COUNTER
                               MOVE 'Y' TO WS-FOUND
                           END-IF
                   END-EVALUATE
           END-READ.

       READ-AND-SEARCH-GENDER.
           READ GUEST-FILE NEXT
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   IF GUEST-GENDER = WS-SEARCH-NAME(1:1)
                       PERFORM DISPLAY-GUEST-RECORD
                       ADD 1 TO WS-GUEST-COUNTER
                       MOVE 'Y' TO WS-FOUND
                   END-IF
           END-READ.

       DISPLAY-GUEST-RECORD.
           MOVE GUEST-ID TO WS-DL-GUEST-ID
           MOVE GUEST-NAME TO WS-DL-GUEST-NAME
           MOVE GUEST-AGE TO WS-DL-GUEST-AGE
           MOVE GUEST-NRC TO WS-DL-GUEST-NRC
           MOVE GUEST-GENDER TO WS-DL-GUEST-GENDER
           DISPLAY WS-DL-GUEST-ID WITH NO ADVANCING
           DISPLAY '       ' WITH NO ADVANCING
           DISPLAY WS-DL-GUEST-NAME(1:20) WITH NO ADVANCING
           DISPLAY '   ' WITH NO ADVANCING
           DISPLAY WS-DL-GUEST-AGE WITH NO ADVANCING
           DISPLAY '   ' WITH NO ADVANCING
           DISPLAY WS-DL-GUEST-NRC WITH NO ADVANCING
           DISPLAY '   ' WITH NO ADVANCING
           DISPLAY WS-DL-GUEST-GENDER.

       DISPLAY-SUMMARY.
           DISPLAY SPACES
           DISPLAY GREEN-COLOR 'Total Guests: '
           WS-GUEST-COUNTER RESET-COLOR.

       END PROGRAM viewGuests.
