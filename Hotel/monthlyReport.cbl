
       IDENTIFICATION DIVISION.
       PROGRAM-ID. monthlyReport.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BOOKING-FILE ASSIGN TO '../DATA/BOOKINGS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS BOOKING-ID.

           SELECT ROOMS-FILE ASSIGN TO '../DATA/ROOMS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ROOM-ID.

           SELECT INVOICES-FILE ASSIGN TO '../DATA/INVOICES.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS INVOICE-ID.

           SELECT CHECKINOUT-FILE ASSIGN TO '../DATA/CHECKINOUT.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CHECKIN-ID.

       DATA DIVISION.
       FILE SECTION.
       FD  BOOKING-FILE.
       COPY "./CopyBooks/BOOKINGS.cpy".

       FD  ROOMS-FILE.
       COPY "./CopyBooks/ROOMS.cpy".

       FD  INVOICES-FILE.
       COPY "./CopyBooks/INVOICES.cpy".

       FD  CHECKINOUT-FILE.
       COPY "./CopyBooks/CHECKINOUT.cpy".

       WORKING-STORAGE SECTION.
       01  WS-BOOKING-FILE-STATUS  PIC 99.
       01  WS-ROOMS-FILE-STATUS    PIC 99.
       01  WS-INVOICE-FILE-STATUS  PIC 99.
       01  WS-CHECKINOUT-FILE-STATUS PIC 99.
       01  WS-EOF                  PIC X VALUE 'N'.

       01  WS-REPORT-DATE.
           05 WS-REPORT-YEAR       PIC 9(4).
           05 WS-REPORT-MONTH      PIC 99.
           05 WS-REPORT-DAY        PIC 99.

       01  WS-REPORT-DATE-NUM      PIC 9(8).
       01  WS-CHECKIN-DATE-NUM     PIC 9(8).
       01  WS-CHECKOUT-DATE-NUM    PIC 9(8).
       01  WS-BOOKING-MONTH        PIC 99.
       01  WS-BOOKING-YEAR         PIC 9(4).

       *> Counters
       01  WS-CHECKINS-MONTH       PIC 9(5) VALUE 0.
       01  WS-CHECKOUTS-MONTH      PIC 9(5) VALUE 0.
       01  WS-OCCUPIED-ROOMS       PIC 9(3) VALUE 0.
       01  WS-TOTAL-ROOMS          PIC 9(3) VALUE 0.
       01  WS-MONTHLY-REVENUE      PIC 9(9)V99 VALUE 0.

       *> Booking counters
       01  WS-TOTAL-BOOKINGS       PIC 9(5) VALUE 0.
       01  WS-CANCELLED-BOOKINGS   PIC 9(5) VALUE 0.
       01  WS-COMPLETED-BOOKINGS   PIC 9(5) VALUE 0.
       01  WS-ACTIVE-BOOKINGS      PIC 9(5) VALUE 0.

       *> Calculations
       01  WS-OCCUPANCY-RATE       PIC 9(3)V99.
       01  WS-OCCUPANCY-PERCENT    PIC 999V99.
       01  WS-CANCELLATION-RATE    PIC 9(3)V99.

       *> Display fields
       01  WS-DISPLAY-CHECKINS     PIC Z(4)9.
       01  WS-DISPLAY-CHECKOUTS    PIC Z(4)9.
       01  WS-DISPLAY-OCCUPIED     PIC ZZ9.
       01  WS-DISPLAY-TOTAL        PIC ZZ9.
       01  WS-DISPLAY-OCCUPANCY    PIC ZZ9.99.
       01  WS-DISPLAY-REVENUE      PIC Z(9).
       01  WS-DISPLAY-TOTAL-BOOK   PIC Z(4)9.
       01  WS-DISPLAY-CANCELLED    PIC Z(4)9.
       01  WS-DISPLAY-COMPLETED    PIC Z(4)9.
       01  WS-DISPLAY-ACTIVE       PIC Z(4)9.
       01  WS-DISPLAY-CANCEL-RATE  PIC ZZ9.99.

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

       *> Temporary fields
       01  WS-TOTAL-CHARGE-DEC     PIC 9(9)V99.

       LINKAGE SECTION.
       01 LINK PIC 9.

       PROCEDURE DIVISION USING LINK.
       MAIN-PROCEDURE.
           PERFORM GET-REPORT-DATE
           PERFORM COUNT-CHECKINOUT-FROM-CHINOUT
           PERFORM COUNT-MONTHLY-BOOKINGS
           PERFORM CALCULATE-OCCUPANCY
           PERFORM CALCULATE-MONTHLY-REVENUE
           PERFORM DISPLAY-MONTHLY-SUMMARY-REPORT
           GOBACK.

       GET-REPORT-DATE.
           ACCEPT WS-REPORT-DATE FROM DATE YYYYMMDD
           COMPUTE WS-REPORT-DATE-NUM =
               WS-REPORT-YEAR * 10000 +
               WS-REPORT-MONTH * 100 +
               WS-REPORT-DAY.

       COUNT-CHECKINOUT-FROM-CHINOUT.
           OPEN INPUT CHECKINOUT-FILE
           IF WS-CHECKINOUT-FILE-STATUS NOT = 00
               DISPLAY "Error opening CHECKINOUT file: "
                       WS-CHECKINOUT-FILE-STATUS
               GOBACK
           END-IF

           MOVE 'N' TO WS-EOF
           MOVE 0 TO WS-CHECKINS-MONTH
           MOVE 0 TO WS-CHECKOUTS-MONTH

           PERFORM UNTIL WS-EOF = 'Y'
               READ CHECKINOUT-FILE NEXT RECORD
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   PERFORM CHECK-MONTHLY-CHECKINOUT-DATES
               END-READ
           END-PERFORM

           CLOSE CHECKINOUT-FILE.

       COUNT-MONTHLY-BOOKINGS.
           OPEN INPUT BOOKING-FILE
           IF WS-BOOKING-FILE-STATUS NOT = 00
               DISPLAY "Error opening BOOKING file: "
                       WS-BOOKING-FILE-STATUS
               GOBACK
           END-IF

           MOVE 'N' TO WS-EOF
           MOVE 0 TO WS-TOTAL-BOOKINGS
           MOVE 0 TO WS-CANCELLED-BOOKINGS
           MOVE 0 TO WS-COMPLETED-BOOKINGS
           MOVE 0 TO WS-ACTIVE-BOOKINGS

           PERFORM UNTIL WS-EOF = 'Y'
               READ BOOKING-FILE NEXT RECORD
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   PERFORM CHECK-BOOKING-MONTH
               END-READ
           END-PERFORM

           CLOSE BOOKING-FILE

           *> Calculate cancellation rate
           IF WS-TOTAL-BOOKINGS > 0
               COMPUTE WS-CANCELLATION-RATE =
                   (WS-CANCELLED-BOOKINGS / WS-TOTAL-BOOKINGS) * 100
           ELSE
               MOVE 0 TO WS-CANCELLATION-RATE
           END-IF.

       CHECK-BOOKING-MONTH.
           *> Convert CREATED-AT date (YYYYMMDD) to numeric for comparison
           MOVE FUNCTION NUMVAL(CREATED-AT) TO WS-CHECKIN-DATE-NUM

           *> Extract year and month from booking creation date
           DIVIDE WS-CHECKIN-DATE-NUM BY 10000 GIVING WS-BOOKING-YEAR
           COMPUTE WS-BOOKING-MONTH =
               (WS-CHECKIN-DATE-NUM - (WS-BOOKING-YEAR * 10000)) / 100

           *> Count bookings created in this month
           IF WS-BOOKING-YEAR = WS-REPORT-YEAR AND
              WS-BOOKING-MONTH = WS-REPORT-MONTH
               ADD 1 TO WS-TOTAL-BOOKINGS

               *> Count by status
               EVALUATE FUNCTION TRIM(BOOKING-STATUS)
                   WHEN 'Cancelled'
                       ADD 1 TO WS-CANCELLED-BOOKINGS
                   WHEN 'Completed'
                       ADD 1 TO WS-COMPLETED-BOOKINGS
                   WHEN 'Active'
                       ADD 1 TO WS-ACTIVE-BOOKINGS
               END-EVALUATE
           END-IF.

       CHECK-MONTHLY-CHECKINOUT-DATES.
           *> Convert dates to numeric for comparison
           MOVE FUNCTION NUMVAL(ACTUAL-CHECKIN-DATE)
           TO WS-CHECKIN-DATE-NUM

           *> Extract year and month from check-in date
           DIVIDE WS-CHECKIN-DATE-NUM BY 10000 GIVING WS-BOOKING-YEAR
           COMPUTE WS-BOOKING-MONTH =
               (WS-CHECKIN-DATE-NUM - (WS-BOOKING-YEAR * 10000)) / 100

           *> Count check-ins in this month
           IF WS-BOOKING-YEAR = WS-REPORT-YEAR AND
              WS-BOOKING-MONTH = WS-REPORT-MONTH
               ADD 1 TO WS-CHECKINS-MONTH
           END-IF

           *> Count check-outs in this month if checkout date exists
           IF CHECKOUT-DATE NOT = SPACES AND
              CHECKOUT-DATE NOT = "00000000"
               MOVE FUNCTION NUMVAL(CHECKOUT-DATE)
               TO WS-CHECKOUT-DATE-NUM

               *> Extract year and month from check-out date
               DIVIDE WS-CHECKOUT-DATE-NUM BY 10000
               GIVING WS-BOOKING-YEAR
               COMPUTE WS-BOOKING-MONTH =
                   (WS-CHECKOUT-DATE-NUM -
                   (WS-BOOKING-YEAR * 10000)) / 100

               IF WS-BOOKING-YEAR = WS-REPORT-YEAR AND
                  WS-BOOKING-MONTH = WS-REPORT-MONTH
                   ADD 1 TO WS-CHECKOUTS-MONTH
               END-IF
           END-IF.

       CALCULATE-OCCUPANCY.
           OPEN INPUT ROOMS-FILE
           IF WS-ROOMS-FILE-STATUS NOT = 00
               DISPLAY "Error opening ROOMS file: "
                       WS-ROOMS-FILE-STATUS
               GOBACK
           END-IF

           MOVE 'N' TO WS-EOF
           MOVE 0 TO WS-OCCUPIED-ROOMS
           MOVE 0 TO WS-TOTAL-ROOMS

           PERFORM UNTIL WS-EOF = 'Y'
               READ ROOMS-FILE NEXT RECORD
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   ADD 1 TO WS-TOTAL-ROOMS
                   IF R-STATUS = "Occupied" OR R-STATUS = "Booked"
                       ADD 1 TO WS-OCCUPIED-ROOMS
                   END-IF
               END-READ
           END-PERFORM

           CLOSE ROOMS-FILE

           *> Calculate occupancy rate percentage
           IF WS-TOTAL-ROOMS > 0
               COMPUTE WS-OCCUPANCY-RATE =
                   (WS-OCCUPIED-ROOMS / WS-TOTAL-ROOMS) * 100
           ELSE
               MOVE 0 TO WS-OCCUPANCY-RATE
           END-IF.

       CALCULATE-MONTHLY-REVENUE.
           OPEN INPUT INVOICES-FILE
           IF WS-INVOICE-FILE-STATUS NOT = 00
               DISPLAY "Error opening INVOICES file"
               GOBACK
           END-IF

           MOVE 'N' TO WS-EOF
           MOVE 0 TO WS-MONTHLY-REVENUE

           PERFORM UNTIL WS-EOF = 'Y'
               READ INVOICES-FILE NEXT RECORD
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   PERFORM CHECK-INVOICE-MONTH
               END-READ
           END-PERFORM

           CLOSE INVOICES-FILE.

       CHECK-INVOICE-MONTH.
           *> Extract year and month from CREATED-AT-IV (YYYYMMDD format)
           DIVIDE CREATED-AT-IV BY 10000 GIVING WS-BOOKING-YEAR
           COMPUTE WS-BOOKING-MONTH =
               (CREATED-AT-IV - (WS-BOOKING-YEAR * 10000)) / 100

           *> Include invoice total if it was created in the report month
           IF WS-BOOKING-YEAR = WS-REPORT-YEAR AND
              WS-BOOKING-MONTH = WS-REPORT-MONTH
               ADD TOTAL-CHARGE TO WS-MONTHLY-REVENUE
           END-IF.

       DISPLAY-MONTHLY-SUMMARY-REPORT.
           MOVE WS-CHECKINS-MONTH TO WS-DISPLAY-CHECKINS
           MOVE WS-CHECKOUTS-MONTH TO WS-DISPLAY-CHECKOUTS
           MOVE WS-OCCUPIED-ROOMS TO WS-DISPLAY-OCCUPIED
           MOVE WS-TOTAL-ROOMS TO WS-DISPLAY-TOTAL
           MOVE WS-OCCUPANCY-RATE TO WS-DISPLAY-OCCUPANCY
           MOVE WS-MONTHLY-REVENUE TO WS-DISPLAY-REVENUE
           MOVE WS-TOTAL-BOOKINGS TO WS-DISPLAY-TOTAL-BOOK
           MOVE WS-CANCELLED-BOOKINGS TO WS-DISPLAY-CANCELLED
           MOVE WS-COMPLETED-BOOKINGS TO WS-DISPLAY-COMPLETED
           MOVE WS-ACTIVE-BOOKINGS TO WS-DISPLAY-ACTIVE
           MOVE WS-CANCELLATION-RATE TO WS-DISPLAY-CANCEL-RATE

           DISPLAY CLEAR-SCREEN
           DISPLAY BLUE-COLOR
           DISPLAY "==================================================="
           "============================"
           DISPLAY "                        MONTHLY SUMMARY REPORT SYS"
           "TEM                        "
           DISPLAY "==================================================="
           "============================"
           RESET-COLOR
           DISPLAY "                                                   "
           DISPLAY "                     Report Month: " WS-REPORT-MONTH "/" WS-REPORT-YEAR
           DISPLAY "                                                   "
           DISPLAY CYAN-COLOR "                     BOOKING STATISTICS:"
           RESET-COLOR
           DISPLAY "                       Total Bookings       : "
           FUNCTION TRIM(WS-DISPLAY-TOTAL-BOOK)
           DISPLAY "                       Completed Bookings   : "
           FUNCTION TRIM(WS-DISPLAY-COMPLETED)
           DISPLAY "                       Active Bookings      : "
           FUNCTION TRIM(WS-DISPLAY-ACTIVE)
           DISPLAY "                       Cancelled Bookings   : "
           FUNCTION TRIM(WS-DISPLAY-CANCELLED)
           DISPLAY "                       Cancellation Rate    : "
           FUNCTION TRIM(WS-DISPLAY-CANCEL-RATE) "%"
           DISPLAY "                                                   "
           DISPLAY CYAN-COLOR "                     CHECK-IN/CHECK-OUT "
           "ACTIVITY:" RESET-COLOR
           DISPLAY "                       Check-ins This Month : "
           FUNCTION TRIM(WS-DISPLAY-CHECKINS)
           DISPLAY "                       Check-outs This Month: "
           FUNCTION TRIM(WS-DISPLAY-CHECKOUTS)
           DISPLAY "                                                   "
           DISPLAY CYAN-COLOR "                     REVENUE:"
           RESET-COLOR
           DISPLAY "                       Monthly Revenue      : $"
           FUNCTION TRIM(WS-DISPLAY-REVENUE)
           DISPLAY "                                                   "
           DISPLAY "==================================================="
           "============================"
           DISPLAY "                                                   "
           DISPLAY "Press ENTER to continue...    "

           ACCEPT WS-DUMMY-INPUT.

       END PROGRAM monthlyReport.
