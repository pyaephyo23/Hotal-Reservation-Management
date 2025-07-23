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
               DISPLAY " "
               DISPLAY "=============================================="
               DISPLAY "         CHECK-IN/OUT RECORD VIEWER          "
               DISPLAY "=============================================="
               DISPLAY "1. View All Check-in/out Records"
               DISPLAY "2. View Active Check-ins (Not checked out)"
               DISPLAY "3. View Completed Check-ins (Checked out)"
               DISPLAY "4. Search by Check-in ID"
               DISPLAY "5. Search by Room Number"
               DISPLAY "9. Return to Main Menu"
               DISPLAY "=============================================="
               DISPLAY "Enter choice: " WITH NO ADVANCING
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
                       DISPLAY "Invalid choice. Please try again."
               END-EVALUATE
           END-PERFORM

           GOBACK.

       *> View all check-in/out records
       VIEW-ALL-RECORDS.
           DISPLAY " "
           DISPLAY
           "============== ALL CHECK-IN/OUT RECORDS ================"
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
           DISPLAY " "
           DISPLAY
           "=========== ACTIVE CHECK-INS (NOT CHECKED OUT) ============"
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
           DISPLAY " "
           DISPLAY
           "========= COMPLETED CHECK-INS (CHECKED OUT) ============"
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
           DISPLAY " "
           DISPLAY "Enter Check-in ID: " WITH NO ADVANCING
           ACCEPT WS-SEARCH-CHECKIN-ID

           DISPLAY " "
           DISPLAY "======= CHECK-IN DETAILS (ID: "
                   WS-SEARCH-CHECKIN-ID ") ======="
           PERFORM DISPLAY-HEADER

           MOVE 'N' TO WS-FOUND
           OPEN INPUT CHECKINOUT-FILE
           MOVE WS-SEARCH-CHECKIN-ID TO CHECKIN-ID
           READ CHECKINOUT-FILE KEY IS CHECKIN-ID
               INVALID KEY
                   DISPLAY "No check-in record found with ID "
                           WS-SEARCH-CHECKIN-ID
               NOT INVALID KEY
                   PERFORM DISPLAY-RECORD
                   MOVE 'Y' TO WS-FOUND
           END-READ
           CLOSE CHECKINOUT-FILE

           IF WS-FOUND = 'N'
               DISPLAY "Check-in record not found."
           END-IF
           DISPLAY
           "========================================================".

       *> Search for check-ins by room ID
       SEARCH-BY-ROOM-ID.
           DISPLAY " "
           DISPLAY "Enter Room ID: " WITH NO ADVANCING
           ACCEPT WS-SEARCH-ROOM-ID

           DISPLAY " "
           DISPLAY "==== CHECK-INS FOR ROOM " WS-SEARCH-ROOM-ID " ===="
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
           DISPLAY
       "---------------------------------------------------------------"
           DISPLAY
        "CHK-ID | ROOM  | BKG-ID | CHECKIN DT"
        "          | CHECKOUT DT         | STATUS"
           DISPLAY
       "--------------------------------------------------------------".

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
           DISPLAY
        "--------------------------------------------------------------"
           DISPLAY
           "Total records found: " WS-DISPLAY-COUNT
           DISPLAY
       "==============================================================".

       END PROGRAM viewCheckInOut.
