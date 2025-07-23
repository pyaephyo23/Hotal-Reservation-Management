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
       01  MENU-CHOICE             PIC 9.
       01  WS-FILE-STATUS          PIC 99.

       *> Color codes for display
       01 RED-COLOR          PIC X(8) VALUE X"1B5B33316D".
       01 GREEN-COLOR        PIC X(8) VALUE X"1B5B33326D".
       01 RESET-COLOR        PIC X(4) VALUE X"1B5B306D".

       01  WS-HEADER-1.
           05 FILLER               PIC X(7) VALUE 'BOOKING'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(4) VALUE 'ROOM'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(11) VALUE 'CUSTOMER ID'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(10) VALUE 'CHECK-IN'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(10) VALUE 'CHECK-OUT'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(8) VALUE 'IN-FLAG'.
           05 FILLER               PIC X(2) VALUE SPACES.
           05 FILLER               PIC X(9) VALUE 'OUT-FLAG'.
           05 FILLER               PIC X(2) VALUE SPACES.
           05 FILLER               PIC X(10) VALUE 'STATUS'.

       01  WS-HEADER-2.
           05 FILLER               PIC X(7) VALUE '-------'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(5) VALUE '-----'.
           05 FILLER               PIC X(2) VALUE SPACES.
           05 FILLER               PIC X(11) VALUE '-----------'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(10) VALUE '----------'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(10) VALUE '----------'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(8) VALUE '--------'.
           05 FILLER               PIC X(2) VALUE SPACES.
           05 FILLER               PIC X(9) VALUE '---------'.
           05 FILLER               PIC X(2) VALUE SPACES.
           05 FILLER               PIC X(10) VALUE '----------'.

       01  WS-DETAIL-LINE.
           05 WS-DL-BOOKING-ID     PIC Z(5)9.
           05 FILLER               PIC X(4) VALUE SPACES.
           05 WS-DL-ROOM-ID        PIC X(5).
           05 FILLER               PIC X(3) VALUE SPACES.
           05 WS-DL-CUSTOMER-ID    PIC Z(5)9.
           05 FILLER               PIC X(7) VALUE SPACES.
           05 WS-DL-CHECKIN        PIC X(10).
           05 FILLER               PIC X(3) VALUE SPACES.
           05 WS-DL-CHECKOUT       PIC X(10).
           05 FILLER               PIC X(3) VALUE SPACES.
           05 WS-DL-CHECKIN-FLAG   PIC X(1).
           05 FILLER               PIC X(9) VALUE SPACES.
           05 WS-DL-CHECKOUT-FLAG  PIC X(1).
           05 FILLER               PIC X(10) VALUE SPACES.
           05 WS-DL-STATUS         PIC X(10).

       LINKAGE SECTION.
       01 LINK PIC 9.

       PROCEDURE DIVISION USING LINK.

       MAIN-LOOP.
           PERFORM UNTIL MENU-CHOICE = 9
           DISPLAY
           "**************************************************"
           DISPLAY "View Hotel Bookings"
           DISPLAY "1. View All Bookings"
           DISPLAY "2. View All Active Bookings"
           DISPLAY "3. View All Cancelled Bookings"
           DISPLAY "4. View All Completed Bookings"
           DISPLAY "9. Go Back"
           DISPLAY
           "**************************************************"
           ACCEPT MENU-CHOICE
           EVALUATE MENU-CHOICE
               WHEN 1 PERFORM ALL-BOOKINGS-DSP
               WHEN 2 PERFORM ACTIVE-BOOKINGS-DSP
               WHEN 3 PERFORM CANCELLED-BOOKINGS-DSP
               WHEN 4 PERFORM COMPLETED-BOOKINGS-DSP
               WHEN 9 GOBACK
               WHEN OTHER
                   DISPLAY RED-COLOR "Invalid selection." RESET-COLOR
           END-EVALUATE
           END-PERFORM.
           GOBACK.

       ALL-BOOKINGS-DSP.
           MOVE 0 TO WS-BOOKING-COUNTER
           MOVE 'N' TO WS-EOF
           OPEN INPUT BOOKING-FILE
           IF WS-FILE-STATUS NOT = '00' AND WS-FILE-STATUS NOT = '97'
               DISPLAY "Error opening BOOKING file: " WS-FILE-STATUS
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
           OPEN INPUT BOOKING-FILE
           IF WS-FILE-STATUS NOT = '00' AND WS-FILE-STATUS NOT = '97'
               DISPLAY "Error opening BOOKING file: " WS-FILE-STATUS
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
           OPEN INPUT BOOKING-FILE
           IF WS-FILE-STATUS NOT = '00' AND WS-FILE-STATUS NOT = '97'
               DISPLAY "Error opening BOOKING file: " WS-FILE-STATUS
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
           OPEN INPUT BOOKING-FILE
           IF WS-FILE-STATUS NOT = '00' AND WS-FILE-STATUS NOT = '97'
               DISPLAY "Error opening BOOKING file: " WS-FILE-STATUS
               MOVE 'Y' TO WS-EOF
           ELSE
               PERFORM DISPLAY-HEADERS
               PERFORM READ-AND-DISPLAY-COMPLETED UNTIL WS-EOF = 'Y'
               PERFORM DISPLAY-SUMMARY
           END-IF
           CLOSE BOOKING-FILE.

       DISPLAY-HEADERS.
           DISPLAY WS-HEADER-1
           DISPLAY WS-HEADER-2.

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
           MOVE CUSTOMER-ID-BK TO WS-DL-CUSTOMER-ID
           *> Format check-in date as YYYY/MM/DD
           STRING CHECKIN-DATE(1:4) "/" CHECKIN-DATE(5:2) "/"
                  CHECKIN-DATE(7:2) INTO WS-DL-CHECKIN
           *> Format check-out date as YYYY/MM/DD
           STRING CHECKOUT-DATE(1:4) "/" CHECKOUT-DATE(5:2) "/"
                  CHECKOUT-DATE(7:2) INTO WS-DL-CHECKOUT
           MOVE CHEKIN-FLAG TO WS-DL-CHECKIN-FLAG
           MOVE CHECKOUT-FLAG TO WS-DL-CHECKOUT-FLAG
           MOVE BOOKING-STATUS TO WS-DL-STATUS
           DISPLAY WS-DETAIL-LINE.

       DISPLAY-SUMMARY.
           DISPLAY SPACES
           DISPLAY 'Total Bookings: ' WS-BOOKING-COUNTER.

       END PROGRAM viewBookings.
