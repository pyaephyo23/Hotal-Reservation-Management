       IDENTIFICATION DIVISION.
       PROGRAM-ID. viewBookings.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BOOKING-FILE ASSIGN TO '../DATA/BOOKINGS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS BOOKING-ID
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  BOOKING-FILE.
       COPY "./CopyBooks/BOOKINGS.cpy".

       WORKING-STORAGE SECTION.
       01  WS-EOF                  PIC X VALUE 'N'.
       01  WS-BOOKING-COUNTER      PIC 999 VALUE 0.
       01  WS-BOOKING-COUNT-DISPLAY PIC ZZZ.
       01  MENU-CHOICE             PIC 9.
       01  WS-FILE-STATUS          PIC 99.

       *> Color codes for display - ANSI escape sequences
       01 RED-COLOR          PIC X(8) VALUE X"1B5B33316D".
       01 GREEN-COLOR        PIC X(8) VALUE X"1B5B33326D".
       01 RESET-COLOR        PIC X(4) VALUE X"1B5B306D".
       01 BLUE-COLOR         PIC X(8) VALUE X"1B5B33346D".
       01 YELLOW-COLOR       PIC X(8) VALUE X"1B5B33336D".
       01 CYAN-COLOR         PIC X(8) VALUE X"1B5B33366D".

       *> Screen formatting
       01 CLEAR-SCREEN       PIC X(4) VALUE X"1B5B324A".
       01 WS-DUMMY-INPUT     PIC X.

       01  WS-HEADER-1.
           05 FILLER               PIC X(7) VALUE 'BOOKING'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(4) VALUE 'ROOM'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(15) VALUE 'CUSTOMER NAME'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(12) VALUE 'PHONE NUMBER'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(10) VALUE 'CHECK-IN'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(10) VALUE 'STATUS'.

       01  WS-HEADER-2.
           05 FILLER               PIC X(7) VALUE '-------'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(4) VALUE '----'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(15) VALUE '---------------'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(12) VALUE '------------'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(10) VALUE '----------'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(10) VALUE '----------'.

       01  WS-DETAIL-LINE.
           05 WS-DL-BOOKING-ID     PIC Z(5)9.
           05 FILLER               PIC X(4) VALUE SPACES.
           05 WS-DL-ROOM-ID        PIC X(5).
           05 FILLER               PIC X(2) VALUE SPACES.
           05 WS-DL-CUSTOMER-NAME  PIC X(15).
           05 FILLER               PIC X(3) VALUE SPACES.
           05 WS-DL-CUSTOMER-PHONE PIC X(12).
           05 FILLER               PIC X(3) VALUE SPACES.
           05 WS-DL-CHECKIN        PIC X(10).
           05 FILLER               PIC X(3) VALUE SPACES.
           05 WS-DL-STATUS         PIC X(10).

       LINKAGE SECTION.
       01 LINK PIC 9.

       PROCEDURE DIVISION USING LINK.

       MAIN-LOOP.
           PERFORM UNTIL MENU-CHOICE = 9
           DISPLAY CLEAR-SCREEN
           DISPLAY BLUE-COLOR
           DISPLAY "==================================================="
           "============================"
           DISPLAY "                           VIEW HOTEL BOOKINGS SYS"
           "TEM                           "
           DISPLAY "==================================================="
           "============================"
           RESET-COLOR
           DISPLAY "                                                   "
           DISPLAY "                        1. View All Bookings      "
           "                        "
           DISPLAY "                        2. View Active Bookings   "
           "                        "
           DISPLAY "                        3. View Cancelled Bookings"
           "                        "
           DISPLAY "                        4. View Completed Bookings"
           "                        "
           DISPLAY "                                                   "
           DISPLAY "==================================================="
           "============================"
           DISPLAY "                        9. Go Back to Main Menu    "
           "                     "
           DISPLAY "==================================================="
           "============================"
           ACCEPT MENU-CHOICE
           EVALUATE MENU-CHOICE
               WHEN 1 PERFORM ALL-BOOKINGS-DSP
               WHEN 2 PERFORM ACTIVE-BOOKINGS-DSP
               WHEN 3 PERFORM CANCELLED-BOOKINGS-DSP
               WHEN 4 PERFORM COMPLETED-BOOKINGS-DSP
               WHEN 9 GOBACK
               WHEN OTHER
                   DISPLAY " "
                   DISPLAY RED-COLOR "*** ERROR: Invalid selection. P"
                   "lease choose 1-4 or 9. ***" RESET-COLOR
                   DISPLAY " "
                   DISPLAY "Press ENTER to continue..."
                   ACCEPT WS-DUMMY-INPUT
           END-EVALUATE
           END-PERFORM.
           GOBACK.

       ALL-BOOKINGS-DSP.
           MOVE 0 TO WS-BOOKING-COUNTER
           MOVE 'N' TO WS-EOF
           DISPLAY CLEAR-SCREEN
           DISPLAY CYAN-COLOR
           DISPLAY "==============================================="
           "================================"
           DISPLAY "                           ALL BOOKINGS REPOR"
           "T                               "
           DISPLAY "==============================================="
           "================================"
           RESET-COLOR
           DISPLAY " "

           OPEN INPUT BOOKING-FILE
           IF WS-FILE-STATUS NOT = '00' AND WS-FILE-STATUS NOT = '97'
               DISPLAY RED-COLOR "Error opening BOOKING file: "
               WS-FILE-STATUS RESET-COLOR
               DISPLAY " "
               DISPLAY "Press ENTER to continue..."
               ACCEPT WS-DUMMY-INPUT
               MOVE 'Y' TO WS-EOF
           ELSE
               PERFORM DISPLAY-HEADERS
               PERFORM READ-AND-DISPLAY-ALL UNTIL WS-EOF = 'Y'
               PERFORM DISPLAY-SUMMARY
           END-IF
           CLOSE BOOKING-FILE.

       ACTIVE-BOOKINGS-DSP.
           MOVE 0 TO WS-BOOKING-COUNTER
           MOVE 'N' TO WS-EOF
           DISPLAY CLEAR-SCREEN
           DISPLAY CYAN-COLOR
           DISPLAY "==============================================="
           "================================"
           DISPLAY "                         ACTIVE BOOKINGS REPO"
           "RT                             "
           DISPLAY "==============================================="
           "================================"
           RESET-COLOR
           DISPLAY " "

           OPEN INPUT BOOKING-FILE
           IF WS-FILE-STATUS NOT = '00' AND WS-FILE-STATUS NOT = '97'
               DISPLAY RED-COLOR "Error opening BOOKING file: "
               WS-FILE-STATUS RESET-COLOR
               DISPLAY " "
               DISPLAY "Press ENTER to continue..."
               ACCEPT WS-DUMMY-INPUT
               MOVE 'Y' TO WS-EOF
           ELSE
               PERFORM DISPLAY-HEADERS
               PERFORM READ-AND-DISPLAY-ACTIVE UNTIL WS-EOF = 'Y'
               PERFORM DISPLAY-SUMMARY
           END-IF
           CLOSE BOOKING-FILE.

       CANCELLED-BOOKINGS-DSP.
           MOVE 0 TO WS-BOOKING-COUNTER
           MOVE 'N' TO WS-EOF
           DISPLAY CLEAR-SCREEN
           DISPLAY CYAN-COLOR
           DISPLAY "==============================================="
           "================================"
           DISPLAY "                       CANCELLED BOOKINGS REP"
           "ORT                           "
           DISPLAY "==============================================="
           "================================"
           RESET-COLOR
           DISPLAY " "

           OPEN INPUT BOOKING-FILE
           IF WS-FILE-STATUS NOT = '00' AND WS-FILE-STATUS NOT = '97'
               DISPLAY RED-COLOR "Error opening BOOKING file: "
               WS-FILE-STATUS RESET-COLOR
               DISPLAY " "
               DISPLAY "Press ENTER to continue..."
               ACCEPT WS-DUMMY-INPUT
               MOVE 'Y' TO WS-EOF
           ELSE
               PERFORM DISPLAY-HEADERS
               PERFORM READ-AND-DISPLAY-CANCELLED UNTIL WS-EOF = 'Y'
               PERFORM DISPLAY-SUMMARY
           END-IF
           CLOSE BOOKING-FILE.

       COMPLETED-BOOKINGS-DSP.
           MOVE 0 TO WS-BOOKING-COUNTER
           MOVE 'N' TO WS-EOF
           DISPLAY CLEAR-SCREEN
           DISPLAY CYAN-COLOR
           DISPLAY "==============================================="
           "================================"
           DISPLAY "                       COMPLETED BOOKINGS REP"
           "ORT                           "
           DISPLAY "==============================================="
           "================================"
           RESET-COLOR
           DISPLAY " "

           OPEN INPUT BOOKING-FILE
           IF WS-FILE-STATUS NOT = '00' AND WS-FILE-STATUS NOT = '97'
               DISPLAY RED-COLOR "Error opening BOOKING file: "
               WS-FILE-STATUS RESET-COLOR
               DISPLAY " "
               DISPLAY "Press ENTER to continue..."
               ACCEPT WS-DUMMY-INPUT
               MOVE 'Y' TO WS-EOF
           ELSE
               PERFORM DISPLAY-HEADERS
               PERFORM READ-AND-DISPLAY-COMPLETED UNTIL WS-EOF = 'Y'
               PERFORM DISPLAY-SUMMARY
           END-IF
           CLOSE BOOKING-FILE.

       DISPLAY-HEADERS.
           DISPLAY YELLOW-COLOR
           DISPLAY WS-HEADER-1
           DISPLAY WS-HEADER-2
           RESET-COLOR.

       READ-AND-DISPLAY-ALL.
           READ BOOKING-FILE
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   PERFORM DISPLAY-BOOKING-RECORD
                   ADD 1 TO WS-BOOKING-COUNTER
           END-READ.

       READ-AND-DISPLAY-ACTIVE.
           READ BOOKING-FILE
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   IF BOOKING-STATUS = 'Active'
                       PERFORM DISPLAY-BOOKING-RECORD
                       ADD 1 TO WS-BOOKING-COUNTER
                   END-IF
           END-READ.

       READ-AND-DISPLAY-CANCELLED.
           READ BOOKING-FILE
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   IF BOOKING-STATUS = 'Cancelled'
                       PERFORM DISPLAY-BOOKING-RECORD
                       ADD 1 TO WS-BOOKING-COUNTER
                   END-IF
           END-READ.

       READ-AND-DISPLAY-COMPLETED.
           READ BOOKING-FILE
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   IF BOOKING-STATUS = 'Completed'
                       PERFORM DISPLAY-BOOKING-RECORD
                       ADD 1 TO WS-BOOKING-COUNTER
                   END-IF
           END-READ.

       DISPLAY-BOOKING-RECORD.
           MOVE BOOKING-ID TO WS-DL-BOOKING-ID
           MOVE ROOM-ID-BK TO WS-DL-ROOM-ID
           MOVE CUSTOMER-NAME-BK TO WS-DL-CUSTOMER-NAME
           MOVE CUSTOMER-PH-BK TO WS-DL-CUSTOMER-PHONE
           *> Format check-in date as YYYY/MM/DD
           STRING CHECKIN-DATE(1:4) "/" CHECKIN-DATE(5:2) "/"
                  CHECKIN-DATE(7:2) INTO WS-DL-CHECKIN
           MOVE BOOKING-STATUS TO WS-DL-STATUS
           DISPLAY WS-DETAIL-LINE.

       DISPLAY-SUMMARY.
           MOVE WS-BOOKING-COUNTER TO WS-BOOKING-COUNT-DISPLAY
           DISPLAY " "
           DISPLAY "==============================================="
           "================================"
           DISPLAY GREEN-COLOR "Total Bookings Found: "
           WS-BOOKING-COUNT-DISPLAY
           RESET-COLOR
           DISPLAY "==============================================="
           "================================"
           DISPLAY " "
           DISPLAY "Press ENTER to continue..."
           ACCEPT WS-DUMMY-INPUT.

       END PROGRAM viewBookings.
