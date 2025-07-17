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

           SELECT ROOM-FILE ASSIGN TO '../DATA/ROOMS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ROOM-ID
               FILE STATUS IS WS-ROOM-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  BOOKING-FILE.
       COPY "./CopyBooks/BOOKINGS.cpy".

       FD  ROOM-FILE.
       COPY "./CopyBooks/ROOMS.cpy".

       WORKING-STORAGE SECTION.
       01  WS-BOOKING-FILE-STATUS  PIC 99.
       01  WS-ROOM-FILE-STATUS     PIC 99.
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
       01  WS-ROOM-NIGHTS          PIC 9(3) VALUE 0.
       01  WS-PRICE-PER-NIGHT-DEC  PIC 9(9)V99.

       01  WS-TEMP-PRICE           PIC 9(9).
       01  WS-TEMP-DAYS            PIC 9(3).
       01  WS-TEMP-REVENUE         PIC 9(9)V99.

       01  WS-DISPLAY-REVENUE      PIC Z,ZZZ,ZZ9.99.
       01  WS-DISPLAY-ROOMS        PIC ZZ9.
       01  WS-DISPLAY-NIGHTS       PIC ZZ9.

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
           MOVE 0 TO WS-MONTHLY-ROOMS-OCCUPIED
           MOVE 0 TO WS-ROOM-NIGHTS.

       PROCESS-DAILY-BOOKINGS.
           OPEN INPUT BOOKING-FILE
           IF WS-BOOKING-FILE-STATUS NOT = 00
               DISPLAY "Error opening BOOKING file: "
                       WS-BOOKING-FILE-STATUS
               STOP RUN
           END-IF

           OPEN INPUT ROOM-FILE
           IF WS-ROOM-FILE-STATUS NOT = 00
               DISPLAY "Error opening ROOM file: " WS-ROOM-FILE-STATUS
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
           CLOSE ROOM-FILE.

       CHECK-DAILY-BOOKING.
           *> Only process completed bookings
           IF BOOKING-STATUS = "Completed"
               MOVE FUNCTION NUMVAL(CHECKIN-DATE) TO WS-CHECKIN-DATE-NUM
             MOVE FUNCTION NUMVAL(CHECKOUT-DATE) TO WS-CHECKOUT-DATE-NUM

               IF WS-CHECKIN-DATE-NUM <= WS-YESTERDAY-DATE-NUM AND
                  WS-CHECKOUT-DATE-NUM >= WS-YESTERDAY-DATE-NUM
                   PERFORM CALCULATE-DAILY-REVENUE
                   ADD 1 TO WS-DAILY-ROOMS-OCCUPIED
               END-IF
           END-IF.

       CALCULATE-DAILY-REVENUE.
           MOVE ROOM-ID-BK TO ROOM-ID
           READ ROOM-FILE
           IF WS-ROOM-FILE-STATUS = 00
               MOVE PRICE-PER-NIGHT TO WS-TEMP-PRICE
               COMPUTE WS-PRICE-PER-NIGHT-DEC = WS-TEMP-PRICE / 100
               ADD WS-PRICE-PER-NIGHT-DEC TO WS-DAILY-REVENUE
           END-IF.

       PROCESS-MONTHLY-BOOKINGS.
           OPEN INPUT BOOKING-FILE
           IF WS-BOOKING-FILE-STATUS NOT = 00
               DISPLAY "Error opening BOOKING file: "
                       WS-BOOKING-FILE-STATUS
               STOP RUN
           END-IF

           OPEN INPUT ROOM-FILE
           IF WS-ROOM-FILE-STATUS NOT = 00
               DISPLAY "Error opening ROOM file: " WS-ROOM-FILE-STATUS
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
           CLOSE ROOM-FILE.

       CHECK-MONTHLY-BOOKING.
           *> Only process completed bookings
           IF BOOKING-STATUS = "Completed"
               MOVE FUNCTION NUMVAL(CHECKIN-DATE) TO WS-CHECKIN-DATE-NUM
            MOVE FUNCTION NUMVAL(CHECKOUT-DATE) TO WS-CHECKOUT-DATE-NUM

               IF ((WS-CHECKIN-DATE-NUM >= WS-THIS-MONTH-START-NUM AND
                   WS-CHECKIN-DATE-NUM <= WS-THIS-MONTH-END-NUM) OR
                  (WS-CHECKOUT-DATE-NUM >= WS-THIS-MONTH-START-NUM AND
                   WS-CHECKOUT-DATE-NUM <= WS-THIS-MONTH-END-NUM) OR
                  (WS-CHECKIN-DATE-NUM <= WS-THIS-MONTH-START-NUM AND
                   WS-CHECKOUT-DATE-NUM >= WS-THIS-MONTH-END-NUM))
                   PERFORM CALCULATE-MONTHLY-REVENUE
                   ADD 1 TO WS-MONTHLY-ROOMS-OCCUPIED
               END-IF
           END-IF.

       CALCULATE-MONTHLY-REVENUE.
           MOVE ROOM-ID-BK TO ROOM-ID
           READ ROOM-FILE
           IF WS-ROOM-FILE-STATUS = 00
               MOVE PRICE-PER-NIGHT TO WS-TEMP-PRICE
               COMPUTE WS-PRICE-PER-NIGHT-DEC = WS-TEMP-PRICE / 100

               PERFORM CALCULATE-OVERLAP-DAYS
               COMPUTE WS-TEMP-REVENUE =
                   WS-PRICE-PER-NIGHT-DEC * WS-TEMP-DAYS
               ADD WS-TEMP-REVENUE TO WS-MONTHLY-REVENUE
           END-IF.

       CALCULATE-OVERLAP-DAYS.
           MOVE WS-CHECKIN-DATE-NUM TO WS-TARGET-DATE-NUM
           IF WS-TARGET-DATE-NUM < WS-THIS-MONTH-START-NUM
               MOVE WS-THIS-MONTH-START-NUM TO WS-TARGET-DATE-NUM
           END-IF

           MOVE WS-CHECKOUT-DATE-NUM TO WS-TEMP-DAYS
           IF WS-TEMP-DAYS > WS-THIS-MONTH-END-NUM
               MOVE WS-THIS-MONTH-END-NUM TO WS-TEMP-DAYS
           END-IF

           COMPUTE WS-TEMP-DAYS = WS-TEMP-DAYS - WS-TARGET-DATE-NUM + 1

           IF WS-TEMP-DAYS < 1
               MOVE 1 TO WS-TEMP-DAYS
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

       END PROGRAM generateReport.
