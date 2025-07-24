       IDENTIFICATION DIVISION.
       PROGRAM-ID. viewCheckInOut.


       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CHECKINOUT-FILE ASSIGN TO '../DATA/CHECKINOUT.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CHECKIN-ID.

       DATA DIVISION.
       FILE SECTION.
       FD  CHECKINOUT-FILE.
       COPY "./CopyBooks/CHECKINOUT.cpy".

       WORKING-STORAGE SECTION.
       *> Menu control variables
       01 WS-CHOICE                   PIC 9.
       01 WS-EXIT-FLAG                PIC X VALUE 'N'.
       01 WS-FOUND                    PIC X VALUE 'N'.
       01 WS-EOF                      PIC X VALUE 'N'.

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

       *> Search criteria
       01 WS-SEARCH-CHECKIN-ID        PIC 9(5).
       01 WS-SEARCH-ROOM-ID           PIC X(5).

       *> Display formatting
       01 WS-RECORD-COUNT             PIC 9(3) VALUE 0.
       01 WS-DISPLAY-COUNT            PIC Z99.

       *> Date/time formatting
       01 WS-TEMP-DATE.
           05 WS-YEAR                 PIC X(4).
           05 WS-MONTH                PIC X(2).
           05 WS-DAY                  PIC X(2).

       01 WS-TEMP-TIME.
           05 WS-HOUR                 PIC X(2).
           05 WS-MINUTE               PIC X(2).
           05 WS-SECOND               PIC X(2).

       01 WS-FORMATTED-DATE           PIC X(10).
       01 WS-FORMATTED-TIME           PIC X(8).

       *> Working storage for formatted display fields
       01 WS-FORMATTED-CHECKIN-ID    PIC X(6).
       01 WS-FORMATTED-ROOM-ID       PIC X(6).
       01 WS-FORMATTED-BOOKING-ID    PIC X(7).
       01 WS-FORMATTED-STATUS        PIC X(11).

       LINKAGE SECTION.
       01 LINK                        PIC 9.

       PROCEDURE DIVISION USING LINK.
       MAIN-PROCEDURE.
           PERFORM UNTIL WS-EXIT-FLAG = 'Y'
               DISPLAY CLEAR-SCREEN
               DISPLAY BLUE-COLOR
               DISPLAY "==============================================="
               "===================================="
               DISPLAY "                       CHECK-IN/OUT RECORD VI"
               "EWER                          "
               DISPLAY "==============================================="
               "===================================="
               RESET-COLOR
               DISPLAY "                                               "
               DISPLAY "                  1. View All Check-in/out Rec"
               "ords                          "
               DISPLAY "                  2. View Active Check-ins (No"
               "t checked out)                "
               DISPLAY "                  3. View Completed Check-ins "
               "(Checked out)                 "
               DISPLAY "                  4. Search by Check-in ID    "
               "                              "
               DISPLAY "                  5. Search by Room Number    "
               "                              "
               DISPLAY "                  9. Return to Main Menu      "
               "                              "
               DISPLAY "                                               "
               DISPLAY "==============================================="
               "===================================="
               ACCEPT WS-CHOICE

               EVALUATE WS-CHOICE
                   WHEN 1
                       PERFORM VIEW-ALL-RECORDS
                   WHEN 2
                       PERFORM VIEW-ACTIVE-CHECKINS
                   WHEN 3
                       PERFORM VIEW-COMPLETED-CHECKINS
                   WHEN 4
                       PERFORM SEARCH-BY-CHECKIN-ID
                   WHEN 5
                       PERFORM SEARCH-BY-ROOM-ID
                   WHEN 9
                       MOVE 'Y' TO WS-EXIT-FLAG
                   WHEN OTHER
                       DISPLAY " "
                       DISPLAY RED-COLOR "*** ERROR: Invalid selection."
                       "P"
                       "lease choose 1-5 or 9. ***" RESET-COLOR
                       DISPLAY " "
                       DISPLAY "Press ENTER to continue..."
                       ACCEPT WS-DUMMY-INPUT
               END-EVALUATE
           END-PERFORM

           GOBACK.

       *> View all check-in/out records
       VIEW-ALL-RECORDS.
           DISPLAY CLEAR-SCREEN
           DISPLAY CYAN-COLOR
           DISPLAY "==============================================="
           "===================================="
           DISPLAY "                     ALL CHECK-IN/OUT RECORDS "
           "                               "
           DISPLAY "==============================================="
           "===================================="
           RESET-COLOR
           DISPLAY " "
           PERFORM DISPLAY-HEADER
           MOVE 0 TO WS-RECORD-COUNT
           MOVE 'N' TO WS-EOF

           OPEN INPUT CHECKINOUT-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ CHECKINOUT-FILE NEXT
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       PERFORM DISPLAY-RECORD
                       ADD 1 TO WS-RECORD-COUNT
               END-READ
           END-PERFORM
           CLOSE CHECKINOUT-FILE

           PERFORM DISPLAY-SUMMARY.

       *> View only active check-ins (not checked out yet)
       VIEW-ACTIVE-CHECKINS.
           DISPLAY CLEAR-SCREEN
           DISPLAY CYAN-COLOR
           DISPLAY "==============================================="
           "===================================="
           DISPLAY "                   ACTIVE CHECK-INS (NOT CHECK"
           "ED OUT)                       "
           DISPLAY "==============================================="
           "===================================="
           RESET-COLOR
           DISPLAY " "
           PERFORM DISPLAY-HEADER
           MOVE 0 TO WS-RECORD-COUNT
           MOVE 'N' TO WS-EOF

           OPEN INPUT CHECKINOUT-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ CHECKINOUT-FILE NEXT
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF CHECKOUT-FLAG = 'N'
                           PERFORM DISPLAY-RECORD
                           ADD 1 TO WS-RECORD-COUNT
                       END-IF
               END-READ
           END-PERFORM
           CLOSE CHECKINOUT-FILE

           PERFORM DISPLAY-SUMMARY.

       *> View only completed check-ins (already checked out)
       VIEW-COMPLETED-CHECKINS.
           DISPLAY CLEAR-SCREEN
           DISPLAY CYAN-COLOR
           DISPLAY "==============================================="
           "===================================="
           DISPLAY "                 COMPLETED CHECK-INS (CHECKED "
           "OUT)                         "
           DISPLAY "==============================================="
           "===================================="
           RESET-COLOR
           DISPLAY " "
           PERFORM DISPLAY-HEADER
           MOVE 0 TO WS-RECORD-COUNT
           MOVE 'N' TO WS-EOF

           OPEN INPUT CHECKINOUT-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ CHECKINOUT-FILE NEXT
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF CHECKOUT-FLAG = 'Y'
                           PERFORM DISPLAY-RECORD
                           ADD 1 TO WS-RECORD-COUNT
                       END-IF
               END-READ
           END-PERFORM
           CLOSE CHECKINOUT-FILE

           PERFORM DISPLAY-SUMMARY.

       *> Search for a specific check-in by ID
       SEARCH-BY-CHECKIN-ID.
           DISPLAY CLEAR-SCREEN
           DISPLAY YELLOW-COLOR
           DISPLAY "==============================================="
           "===================================="
           DISPLAY "                       SEARCH BY CHECK-IN ID  "
           "                               "
           DISPLAY "==============================================="
           "===================================="
           RESET-COLOR
           DISPLAY " "
           DISPLAY "Enter Check-in ID: "
           ACCEPT WS-SEARCH-CHECKIN-ID

           DISPLAY " "
           DISPLAY CYAN-COLOR "CHECK-IN DETAILS (ID: "
                   WS-SEARCH-CHECKIN-ID ")" RESET-COLOR
           PERFORM DISPLAY-HEADER

           MOVE 'N' TO WS-FOUND
           OPEN INPUT CHECKINOUT-FILE
           MOVE WS-SEARCH-CHECKIN-ID TO CHECKIN-ID
           READ CHECKINOUT-FILE KEY IS CHECKIN-ID
               INVALID KEY
                   DISPLAY " "
                   DISPLAY RED-COLOR "No check-in record found with ID "
                           WS-SEARCH-CHECKIN-ID RESET-COLOR
                   DISPLAY " "
               NOT INVALID KEY
                   PERFORM DISPLAY-RECORD
                   MOVE 'Y' TO WS-FOUND
           END-READ
           CLOSE CHECKINOUT-FILE

           IF WS-FOUND = 'N'
               DISPLAY RED-COLOR "Check-in record not found."
               RESET-COLOR
           END-IF
           DISPLAY "==============================================="
           "===================================="
           DISPLAY " "
           DISPLAY "Press ENTER to continue..."
           ACCEPT WS-DUMMY-INPUT.

       *> Search for check-ins by room ID
       SEARCH-BY-ROOM-ID.
           DISPLAY CLEAR-SCREEN
           DISPLAY YELLOW-COLOR
           DISPLAY "==============================================="
           "===================================="
           DISPLAY "                       SEARCH BY ROOM NUMBER  "
           "                               "
           DISPLAY "==============================================="
           "===================================="
           RESET-COLOR
           DISPLAY " "
           DISPLAY "Enter Room ID: "
           ACCEPT WS-SEARCH-ROOM-ID

           DISPLAY " "
           DISPLAY CYAN-COLOR "CHECK-INS FOR ROOM " WS-SEARCH-ROOM-ID
           RESET-COLOR
           PERFORM DISPLAY-HEADER
           MOVE 0 TO WS-RECORD-COUNT
           MOVE 'N' TO WS-EOF

           OPEN INPUT CHECKINOUT-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ CHECKINOUT-FILE NEXT
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF ROOM-ID-IO = WS-SEARCH-ROOM-ID
                           PERFORM DISPLAY-RECORD
                           ADD 1 TO WS-RECORD-COUNT
                       END-IF
               END-READ
           END-PERFORM
           CLOSE CHECKINOUT-FILE

           PERFORM DISPLAY-SUMMARY.

       *> Display table header
       DISPLAY-HEADER.
           DISPLAY YELLOW-COLOR
           DISPLAY "==============================================="
           "===================================="
           DISPLAY "CHK-ID | ROOM   | BKG-ID  | CHECKIN DT          |"
           " CHECKOUT DT         | STATUS"
           DISPLAY "==============================================="
           "===================================="
           RESET-COLOR.

       *> Display a single check-in/out record
       DISPLAY-RECORD.
           *> Format check-in date and time
           MOVE ACTUAL-CHECKIN-DATE TO WS-TEMP-DATE
           STRING WS-TEMP-DATE(1:4) "/" WS-TEMP-DATE(5:2) "/"
                  WS-TEMP-DATE(7:2) INTO WS-FORMATTED-DATE

           MOVE ACTUAL-CHECKIN-TIME TO WS-TEMP-TIME
           STRING WS-TEMP-TIME(1:2) ":" WS-TEMP-TIME(3:2) ":"
                  WS-TEMP-TIME(5:2) INTO WS-FORMATTED-TIME

           *> Format fields with proper spacing
           MOVE CHECKIN-ID TO WS-FORMATTED-CHECKIN-ID
           MOVE ROOM-ID-IO TO WS-FORMATTED-ROOM-ID
           MOVE BOOKING-ID-IO TO WS-FORMATTED-BOOKING-ID

           *> Display record with check-in date/time
           DISPLAY WS-FORMATTED-CHECKIN-ID " | "
                   WS-FORMATTED-ROOM-ID " | "
                   WS-FORMATTED-BOOKING-ID " | "
                   WS-FORMATTED-DATE " " WS-FORMATTED-TIME " | "
                   WITH NO ADVANCING


           *> Display checkout date/time or dash if not checked out
           IF CHECKOUT-FLAG = 'Y' AND CHECKOUT-DATE > 0
               MOVE CHECKOUT-DATE TO WS-TEMP-DATE
               MOVE CHECKOUT-TIME TO WS-TEMP-TIME
               STRING WS-TEMP-DATE(1:4) "/" WS-TEMP-DATE(5:2) "/"
                      WS-TEMP-DATE(7:2) INTO WS-FORMATTED-DATE
               STRING WS-TEMP-TIME(1:2) ":" WS-TEMP-TIME(3:2) ":"
                      WS-TEMP-TIME(5:2) INTO WS-FORMATTED-TIME
               DISPLAY WS-FORMATTED-DATE " " WS-FORMATTED-TIME " | "
               WITH NO ADVANCING
           ELSE
               DISPLAY "         -          | " WITH NO ADVANCING
           END-IF

           *> Display status
           IF CHECKOUT-FLAG = 'Y'
               DISPLAY "CHECKED OUT"
           ELSE
               DISPLAY "ACTIVE     "
           END-IF.

       *> Display summary information
       DISPLAY-SUMMARY.
           MOVE WS-RECORD-COUNT TO WS-DISPLAY-COUNT
           DISPLAY "==============================================="
           "===================================="
           DISPLAY GREEN-COLOR "Total records found: " WS-DISPLAY-COUNT
           RESET-COLOR
           DISPLAY "==============================================="
           "===================================="
           DISPLAY " "
           DISPLAY "Press ENTER to continue..."
           ACCEPT WS-DUMMY-INPUT.

       END PROGRAM viewCheckInOut.
