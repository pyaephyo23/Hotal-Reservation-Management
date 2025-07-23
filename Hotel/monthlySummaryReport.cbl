******************************************************************
      * Author: [Pyae Phyo kyaw]
      * Date: 2025-07-18
      * Purpose: Monthly Summary Report - Check-ins, Check-outs,
      *          Occupancy Rate, and Revenue
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. monthlySummaryReport.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BOOKING-FILE ASSIGN TO './DATA/BOOKINGS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS BOOKING-ID.

           SELECT ROOMS-FILE ASSIGN TO './DATA/ROOMS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ROOM-ID.

           SELECT INVOICES-FILE ASSIGN TO './DATA/INVOICES.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS INVOICE-ID.

           SELECT CHECKINOUT-FILE ASSIGN TO './DATA/CHECKINOUT.DAT'
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

       01  WS-BOOKING-MONTH        PIC 99.
       01  WS-BOOKING-YEAR         PIC 9(4).

       *> Counters
       01  WS-CHECKINS-MONTH       PIC 9(5) VALUE 0.
       01  WS-CHECKOUTS-MONTH      PIC 9(5) VALUE 0.
       01  WS-OCCUPIED-ROOMS       PIC 9(3) VALUE 0.
       01  WS-TOTAL-ROOMS          PIC 9(3) VALUE 0.
       01  WS-MONTHLY-REVENUE      PIC 9(9)V99 VALUE 0.

       *> Calculations
       01  WS-OCCUPANCY-RATE       PIC 9(3)V99.

       *> Display fields
       01  WS-DISPLAY-CHECKINS     PIC Z(4)9.
       01  WS-DISPLAY-CHECKOUTS    PIC Z(4)9.
       01  WS-DISPLAY-OCCUPIED     PIC ZZ9.
       01  WS-DISPLAY-TOTAL        PIC ZZ9.
       01  WS-DISPLAY-OCCUPANCY    PIC ZZ9.99.
       01  WS-DISPLAY-REVENUE      PIC Z(8)9.99.

       LINKAGE SECTION.
       01 LINK PIC 9.

       PROCEDURE DIVISION USING LINK.
       MAIN-PROCEDURE.
           PERFORM GET-REPORT-DATE
           PERFORM COUNT-CHECKIN-OUT
           PERFORM CALCULATE-OCCUPANCY
           PERFORM CALCULATE-MONTHLY-REVENUE
           PERFORM DISPLAY-MONTHLY-SUMMARY-REPORT
           GOBACK.

       GET-REPORT-DATE.
           ACCEPT WS-REPORT-DATE FROM DATE YYYYMMDD.

       COUNT-CHECKIN-OUT.
           *> Count check-ins and check-outs from CHECKINOUT file
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

       CHECK-MONTHLY-CHECKINOUT-DATES.
           *> Extract year and month from check-in date
           DIVIDE ACTUAL-CHECKIN-DATE BY 10000 GIVING WS-BOOKING-YEAR
           COMPUTE WS-BOOKING-MONTH =
               (ACTUAL-CHECKIN-DATE - (WS-BOOKING-YEAR * 10000)) / 100

           *> Count check-ins in this month
           IF WS-BOOKING-YEAR = WS-REPORT-YEAR AND
              WS-BOOKING-MONTH = WS-REPORT-MONTH
               ADD 1 TO WS-CHECKINS-MONTH
           END-IF

           *> Count check-outs in this month (only if check-out occurred)
           IF CHECKOUT-FLAG = 'Y'
               DIVIDE CHECKOUT-DATE BY 10000 GIVING WS-BOOKING-YEAR
               COMPUTE WS-BOOKING-MONTH =
                   (CHECKOUT-DATE - (WS-BOOKING-YEAR * 10000)) / 100

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

           DISPLAY " "
           DISPLAY "=========================================="
           DISPLAY "        MONTHLY SUMMARY REPORT"
           DISPLAY "=========================================="
           DISPLAY "Report Month: " WS-REPORT-MONTH "/" WS-REPORT-YEAR
           DISPLAY " "
           DISPLAY "CHECK-IN/CHECK-OUT ACTIVITY:"
           DISPLAY "  Check-ins This Month : "
           FUNCTION TRIM(WS-DISPLAY-CHECKINS)
           DISPLAY "  Check-outs This Month: "
           FUNCTION TRIM(WS-DISPLAY-CHECKOUTS)
           DISPLAY " "
           DISPLAY "ROOM OCCUPANCY:"
           DISPLAY "  Occupied Rooms       : "
           FUNCTION TRIM(WS-DISPLAY-OCCUPIED)
           DISPLAY "  Total Rooms          : "
           FUNCTION TRIM(WS-DISPLAY-TOTAL)
           DISPLAY "  Occupancy Rate       : "
           FUNCTION TRIM(WS-DISPLAY-OCCUPANCY) "%"
           DISPLAY " "
           DISPLAY "REVENUE:"
           DISPLAY "  Monthly Revenue      : $"
           FUNCTION TRIM(WS-DISPLAY-REVENUE)
           DISPLAY "=========================================="
           DISPLAY " ".

       END PROGRAM monthlySummaryReport.
