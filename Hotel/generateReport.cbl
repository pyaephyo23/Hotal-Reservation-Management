       IDENTIFICATION DIVISION.
       PROGRAM-ID. generateReport.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BOOKING-FILE ASSIGN TO '../DATA/BOOKINGS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS BOOKING-ID
               FILE STATUS IS WS-BOOKING-FILE-STATUS.

           SELECT INVOICES-FILE ASSIGN TO '../DATA/INVOICES.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS INVOICE-ID
               FILE STATUS IS WS-INVOICE-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  BOOKING-FILE.
       COPY "./CopyBooks/BOOKINGS.cpy".

       FD  INVOICES-FILE.
       COPY "./CopyBooks/INVOICES.cpy".

       WORKING-STORAGE SECTION.
       01  WS-BOOKING-FILE-STATUS  PIC 99.
       01  WS-INVOICE-FILE-STATUS  PIC 99.
       01  WS-EOF                  PIC X VALUE 'N'.
       01  MENU-CHOICE             PIC 9.
       01  WS-CONTINUE             PIC X VALUE 'Y'.

       01  WS-CURRENT-DATE.
           05 WS-CURRENT-YEAR      PIC 9(4).
           05 WS-CURRENT-MONTH     PIC 99.
           05 WS-CURRENT-DAY       PIC 99.

       01  WS-TARGET-DATE.
           05 WS-TARGET-YEAR       PIC 9(4).
           05 WS-TARGET-MONTH      PIC 99.
           05 WS-TARGET-DAY        PIC 99.

       01  WS-YESTERDAY-DATE.
           05 WS-YESTERDAY-YEAR    PIC 9(4).
           05 WS-YESTERDAY-MONTH   PIC 99.
           05 WS-YESTERDAY-DAY     PIC 99.

       01  WS-THIS-MONTH-DATE.
           05 WS-THIS-MONTH-YEAR   PIC 9(4).
           05 WS-THIS-MONTH-MONTH  PIC 99.
           05 WS-THIS-MONTH-DAY    PIC 99.

       01  WS-CHECKIN-DATE-NUM     PIC 9(8).
       01  WS-CHECKOUT-DATE-NUM    PIC 9(8).
       01  WS-TARGET-DATE-NUM      PIC 9(8).
       01  WS-YESTERDAY-DATE-NUM   PIC 9(8).
       01  WS-THIS-MONTH-START-NUM PIC 9(8).
       01  WS-THIS-MONTH-END-NUM   PIC 9(8).

       01  WS-DAILY-REVENUE        PIC 9(9)V99 VALUE 0.
       01  WS-MONTHLY-REVENUE      PIC 9(9)V99 VALUE 0.
       01  WS-DAILY-ROOMS-OCCUPIED PIC 9(3) VALUE 0.
       01  WS-MONTHLY-ROOMS-OCCUPIED PIC 9(3) VALUE 0.
       01  WS-TOTAL-CHARGE-DEC     PIC 9(9)V99.

       01  WS-DISPLAY-REVENUE      PIC Z,ZZZ,ZZ9.99.
       01  WS-DISPLAY-ROOMS        PIC ZZ9.
       01  WS-TARGET-BOOKING-ID    PIC 9(5).
       01  WS-INVOICE-FOUND        PIC X VALUE 'N'.
       01  WS-INVOICE-EOF          PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM UNTIL WS-CONTINUE = 'N'
               PERFORM SHOW-MENU
               ACCEPT MENU-CHOICE

               EVALUATE MENU-CHOICE
                   WHEN 1 PERFORM DAILY-REPORT
                   WHEN 2 PERFORM MONTHLY-REPORT
                   WHEN 9 GOBACK
                  WHEN OTHER DISPLAY "Invalid choice. Please try again."
               END-EVALUATE

               IF WS-CONTINUE = 'Y'
                   DISPLAY " "
                   DISPLAY "Press Enter to continue..."
                   ACCEPT WS-CONTINUE
               END-IF
           END-PERFORM.

           STOP RUN.

       SHOW-MENU.
           DISPLAY " "
           DISPLAY "======================================="
           DISPLAY "       REVENUE REPORT GENERATOR"
           DISPLAY "======================================="
           DISPLAY "1. Daily Report (Yesterday)"
           DISPLAY "2. Monthly Report (This Month)"
           DISPLAY "9. Go Back"
           DISPLAY "======================================="
           WITH NO ADVANCING.

       DAILY-REPORT.
           PERFORM GET-CURRENT-DATE
           PERFORM CALCULATE-YESTERDAY
           PERFORM RESET-COUNTERS
           PERFORM PROCESS-DAILY-BOOKINGS
           PERFORM DISPLAY-DAILY-REPORT.

       MONTHLY-REPORT.
           PERFORM GET-CURRENT-DATE
           PERFORM CALCULATE-THIS-MONTH
           PERFORM RESET-COUNTERS
           PERFORM PROCESS-MONTHLY-BOOKINGS
           PERFORM DISPLAY-MONTHLY-REPORT.

       GET-CURRENT-DATE.
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD.

       CALCULATE-YESTERDAY.
           MOVE WS-CURRENT-YEAR TO WS-YESTERDAY-YEAR
           MOVE WS-CURRENT-MONTH TO WS-YESTERDAY-MONTH
           MOVE WS-CURRENT-DAY TO WS-YESTERDAY-DAY

           SUBTRACT 1 FROM WS-YESTERDAY-DAY

           IF WS-YESTERDAY-DAY = 0
               SUBTRACT 1 FROM WS-YESTERDAY-MONTH
               IF WS-YESTERDAY-MONTH = 0
                   MOVE 12 TO WS-YESTERDAY-MONTH
                   SUBTRACT 1 FROM WS-YESTERDAY-YEAR
               END-IF
               EVALUATE WS-YESTERDAY-MONTH
                   WHEN 1 MOVE 31 TO WS-YESTERDAY-DAY
                   WHEN 2 MOVE 28 TO WS-YESTERDAY-DAY
                   WHEN 3 MOVE 31 TO WS-YESTERDAY-DAY
                   WHEN 4 MOVE 30 TO WS-YESTERDAY-DAY
                   WHEN 5 MOVE 31 TO WS-YESTERDAY-DAY
                   WHEN 6 MOVE 30 TO WS-YESTERDAY-DAY
                   WHEN 7 MOVE 31 TO WS-YESTERDAY-DAY
                   WHEN 8 MOVE 31 TO WS-YESTERDAY-DAY
                   WHEN 9 MOVE 30 TO WS-YESTERDAY-DAY
                   WHEN 10 MOVE 31 TO WS-YESTERDAY-DAY
                   WHEN 11 MOVE 30 TO WS-YESTERDAY-DAY
                   WHEN 12 MOVE 31 TO WS-YESTERDAY-DAY
               END-EVALUATE
           END-IF

           COMPUTE WS-YESTERDAY-DATE-NUM =
               WS-YESTERDAY-YEAR * 10000 +
               WS-YESTERDAY-MONTH * 100 +
               WS-YESTERDAY-DAY.

       CALCULATE-THIS-MONTH.
           MOVE WS-CURRENT-YEAR TO WS-THIS-MONTH-YEAR
           MOVE WS-CURRENT-MONTH TO WS-THIS-MONTH-MONTH
           MOVE 1 TO WS-THIS-MONTH-DAY

           COMPUTE WS-THIS-MONTH-START-NUM =
               WS-THIS-MONTH-YEAR * 10000 +
               WS-THIS-MONTH-MONTH * 100 +
               WS-THIS-MONTH-DAY

           EVALUATE WS-THIS-MONTH-MONTH
               WHEN 1 MOVE 31 TO WS-THIS-MONTH-DAY
               WHEN 2 MOVE 28 TO WS-THIS-MONTH-DAY
               WHEN 3 MOVE 31 TO WS-THIS-MONTH-DAY
               WHEN 4 MOVE 30 TO WS-THIS-MONTH-DAY
               WHEN 5 MOVE 31 TO WS-THIS-MONTH-DAY
               WHEN 6 MOVE 30 TO WS-THIS-MONTH-DAY
               WHEN 7 MOVE 31 TO WS-THIS-MONTH-DAY
               WHEN 8 MOVE 31 TO WS-THIS-MONTH-DAY
               WHEN 9 MOVE 30 TO WS-THIS-MONTH-DAY
               WHEN 10 MOVE 31 TO WS-THIS-MONTH-DAY
               WHEN 11 MOVE 30 TO WS-THIS-MONTH-DAY
               WHEN 12 MOVE 31 TO WS-THIS-MONTH-DAY
           END-EVALUATE

           COMPUTE WS-THIS-MONTH-END-NUM =
               WS-THIS-MONTH-YEAR * 10000 +
               WS-THIS-MONTH-MONTH * 100 +
               WS-THIS-MONTH-DAY.

       RESET-COUNTERS.
           MOVE 0 TO WS-DAILY-REVENUE
           MOVE 0 TO WS-MONTHLY-REVENUE
           MOVE 0 TO WS-DAILY-ROOMS-OCCUPIED
           MOVE 0 TO WS-MONTHLY-ROOMS-OCCUPIED.

       PROCESS-DAILY-BOOKINGS.
           OPEN INPUT BOOKING-FILE
           IF WS-BOOKING-FILE-STATUS NOT = 00
               DISPLAY "Error opening BOOKING file: "
                       WS-BOOKING-FILE-STATUS
               STOP RUN
           END-IF

           OPEN INPUT INVOICES-FILE
           IF WS-INVOICE-FILE-STATUS NOT = 00
               DISPLAY "Error opening INVOICES file: "
                       WS-INVOICE-FILE-STATUS
               STOP RUN
           END-IF

           MOVE 'N' TO WS-EOF

           PERFORM UNTIL WS-EOF = 'Y'
               READ BOOKING-FILE NEXT RECORD
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   PERFORM CHECK-DAILY-BOOKING
               END-READ
           END-PERFORM

           CLOSE BOOKING-FILE
           CLOSE INVOICES-FILE.

       CHECK-DAILY-BOOKING.
           *> Only process completed bookings
           IF BOOKING-STATUS = "Completed"
               MOVE FUNCTION NUMVAL(CHECKIN-DATE) TO WS-CHECKIN-DATE-NUM
               MOVE FUNCTION NUMVAL(CHECKOUT-DATE)
               TO WS-CHECKOUT-DATE-NUM

               IF WS-CHECKIN-DATE-NUM <= WS-YESTERDAY-DATE-NUM AND
                  WS-CHECKOUT-DATE-NUM >= WS-YESTERDAY-DATE-NUM
                   PERFORM GET-INVOICE-REVENUE
                   ADD 1 TO WS-DAILY-ROOMS-OCCUPIED
               END-IF
           END-IF.

       GET-INVOICE-REVENUE.
           *> Find corresponding invoice for this booking
           MOVE BOOKING-ID TO WS-TARGET-BOOKING-ID
           *> Save current position in invoices file
           PERFORM FIND-INVOICE-FOR-BOOKING
           IF WS-INVOICE-FOUND = 'Y'
               *> Convert total charge to decimal format
               COMPUTE WS-TOTAL-CHARGE-DEC = TOTAL-CHARGE / 100
               ADD WS-TOTAL-CHARGE-DEC TO WS-DAILY-REVENUE
           END-IF.

       PROCESS-MONTHLY-BOOKINGS.
           OPEN INPUT BOOKING-FILE
           IF WS-BOOKING-FILE-STATUS NOT = 00
               DISPLAY "Error opening BOOKING file: "
                       WS-BOOKING-FILE-STATUS
               STOP RUN
           END-IF

           OPEN INPUT INVOICES-FILE
           IF WS-INVOICE-FILE-STATUS NOT = 00
               DISPLAY "Error opening INVOICES file: "
                       WS-INVOICE-FILE-STATUS
               STOP RUN
           END-IF

           MOVE 'N' TO WS-EOF

           PERFORM UNTIL WS-EOF = 'Y'
               READ BOOKING-FILE NEXT RECORD
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   PERFORM CHECK-MONTHLY-BOOKING
               END-READ
           END-PERFORM

           CLOSE BOOKING-FILE
           CLOSE INVOICES-FILE.

       CHECK-MONTHLY-BOOKING.
           *> Only process completed bookings
           IF BOOKING-STATUS = "Completed"
               MOVE FUNCTION NUMVAL(CHECKIN-DATE) TO WS-CHECKIN-DATE-NUM
               MOVE FUNCTION NUMVAL(CHECKOUT-DATE)
               TO WS-CHECKOUT-DATE-NUM

               IF ((WS-CHECKIN-DATE-NUM >= WS-THIS-MONTH-START-NUM AND
                   WS-CHECKIN-DATE-NUM <= WS-THIS-MONTH-END-NUM) OR
                  (WS-CHECKOUT-DATE-NUM >= WS-THIS-MONTH-START-NUM AND
                   WS-CHECKOUT-DATE-NUM <= WS-THIS-MONTH-END-NUM) OR
                  (WS-CHECKIN-DATE-NUM <= WS-THIS-MONTH-START-NUM AND
                   WS-CHECKOUT-DATE-NUM >= WS-THIS-MONTH-END-NUM))
                   PERFORM GET-MONTHLY-INVOICE-REVENUE
                   ADD 1 TO WS-MONTHLY-ROOMS-OCCUPIED
               END-IF
           END-IF.

       GET-MONTHLY-INVOICE-REVENUE.
           *> Find corresponding invoice for this booking
           MOVE BOOKING-ID TO WS-TARGET-BOOKING-ID
           PERFORM FIND-INVOICE-FOR-BOOKING
           IF WS-INVOICE-FOUND = 'Y'
               *> Convert total charge to decimal format
               COMPUTE WS-TOTAL-CHARGE-DEC = TOTAL-CHARGE / 100
               ADD WS-TOTAL-CHARGE-DEC TO WS-MONTHLY-REVENUE
           END-IF.

       DISPLAY-DAILY-REPORT.
           MOVE WS-DAILY-REVENUE TO WS-DISPLAY-REVENUE
           MOVE WS-DAILY-ROOMS-OCCUPIED TO WS-DISPLAY-ROOMS

           DISPLAY " "
           DISPLAY "======================================="
           DISPLAY "         DAILY REVENUE REPORT"
           DISPLAY "======================================="
           DISPLAY "Report Date: " WS-YESTERDAY-MONTH "/"
                   WS-YESTERDAY-DAY "/" WS-YESTERDAY-YEAR
           DISPLAY "Total Revenue: $" WS-DISPLAY-REVENUE
           DISPLAY "Rooms Occupied: " WS-DISPLAY-ROOMS
           DISPLAY "======================================="
           DISPLAY " ".

       DISPLAY-MONTHLY-REPORT.
           MOVE WS-MONTHLY-REVENUE TO WS-DISPLAY-REVENUE
           MOVE WS-MONTHLY-ROOMS-OCCUPIED TO WS-DISPLAY-ROOMS

           DISPLAY " "
           DISPLAY "======================================="
           DISPLAY "        MONTHLY REVENUE REPORT"
           DISPLAY "======================================="
           DISPLAY "Report Month: " WS-THIS-MONTH-MONTH "/"
                   WS-THIS-MONTH-YEAR
           DISPLAY "Total Revenue: $" WS-DISPLAY-REVENUE
           DISPLAY "Total Bookings: " WS-DISPLAY-ROOMS
           DISPLAY "======================================="
           DISPLAY " ".

       FIND-INVOICE-FOR-BOOKING.
           MOVE 'N' TO WS-INVOICE-FOUND
           MOVE 'N' TO WS-INVOICE-EOF

           *> Close and reopen invoices file for fresh search
           CLOSE INVOICES-FILE
           OPEN INPUT INVOICES-FILE

           IF WS-INVOICE-FILE-STATUS = 00
               PERFORM UNTIL WS-INVOICE-EOF = 'Y'
               OR WS-INVOICE-FOUND = 'Y'

                   READ INVOICES-FILE NEXT RECORD
                   AT END
                       MOVE 'Y' TO WS-INVOICE-EOF
                   NOT AT END
                       IF BOOKING-ID-IV = WS-TARGET-BOOKING-ID
                           MOVE 'Y' TO WS-INVOICE-FOUND
                       END-IF
                   END-READ
               END-PERFORM
           END-IF.

       END PROGRAM generateReport.
