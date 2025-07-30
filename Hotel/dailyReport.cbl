
       IDENTIFICATION DIVISION.
       PROGRAM-ID. dailyReport.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
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
       FD  ROOMS-FILE.
       COPY "./CopyBooks/ROOMS.cpy".

       FD  INVOICES-FILE.
       COPY "./CopyBooks/INVOICES.cpy".

       FD  CHECKINOUT-FILE.
       COPY "./CopyBooks/CHECKINOUT.cpy".

       WORKING-STORAGE SECTION.
       01  WS-ROOMS-FILE-STATUS    PIC 99.
       01  WS-INVOICE-FILE-STATUS  PIC 99.
       01  WS-CHECKINOUT-FILE-STATUS PIC 99.
       01  WS-EOF                  PIC X VALUE 'N'.

       01  WS-REPORT-DATE.
           05 WS-REPORT-YEAR       PIC 9(4).
           05 WS-REPORT-MONTH      PIC 9(2).
           05 WS-REPORT-DAY        PIC 9(2).
       01  WS-CHECKIN-DATE         PIC 9(8).
       01  WS-CHECKOUT-DATE        PIC 9(8).

       *> Counters
       01  WS-CHECKINS-TODAY       PIC 9(3) VALUE 0.
       01  WS-CHECKOUTS-TODAY      PIC 9(3) VALUE 0.
       01  WS-OCCUPIED-ROOMS       PIC 9(3) VALUE 0.
       01  WS-TOTAL-ROOMS          PIC 9(3) VALUE 0.
       01  WS-DAILY-REVENUE        PIC 9(9) VALUE 0.

       *> Calculations
       01  WS-OCCUPANCY-RATE       PIC 9(3)V99.
       01  WS-OCCUPANCY-PERCENT    PIC 999V99.

       *> Display fields
       01  WS-DISPLAY-CHECKINS     PIC ZZ9.
       01  WS-DISPLAY-CHECKOUTS    PIC ZZ9.
       01  WS-DISPLAY-OCCUPIED     PIC ZZ9.
       01  WS-DISPLAY-TOTAL        PIC ZZ9.
       01  WS-DISPLAY-OCCUPANCY    PIC ZZ9.99.
       01  WS-DISPLAY-REVENUE      PIC ZZZ,ZZZ,ZZ9.

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
           PERFORM GET-REPORT-DATE
           PERFORM COUNT-CHECKINS-CHECKOUTS
           PERFORM CALCULATE-OCCUPANCY
           PERFORM CALCULATE-DAILY-REVENUE
           PERFORM DISPLAY-SUMMARY-REPORT
           GOBACK.

       GET-REPORT-DATE.
           ACCEPT WS-REPORT-DATE FROM DATE YYYYMMDD
           DISPLAY "Report date set to: " WS-REPORT-DATE.

       COUNT-CHECKINS-CHECKOUTS.
           OPEN INPUT CHECKINOUT-FILE
           IF WS-CHECKINOUT-FILE-STATUS NOT = 00
               DISPLAY "Error opening CHECKINOUT file: "
                       WS-CHECKINOUT-FILE-STATUS
               GOBACK
           END-IF

           MOVE 'N' TO WS-EOF
           MOVE 0 TO WS-CHECKINS-TODAY
           MOVE 0 TO WS-CHECKOUTS-TODAY

           PERFORM UNTIL WS-EOF = 'Y'
               READ CHECKINOUT-FILE NEXT RECORD
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   PERFORM CHECK-CHECKINOUT-DATES
               END-READ
           END-PERFORM

           CLOSE CHECKINOUT-FILE.

       CHECK-CHECKINOUT-DATES.
           *> Convert dates to numeric for comparison
           MOVE ACTUAL-CHECKIN-DATE TO WS-CHECKIN-DATE
           MOVE CHECKOUT-DATE TO WS-CHECKOUT-DATE

           *> Count check-ins today
           IF WS-CHECKIN-DATE = WS-REPORT-DATE
               ADD 1 TO WS-CHECKINS-TODAY
           END-IF

           *> Count check-outs today
           IF WS-CHECKOUT-DATE = WS-REPORT-DATE AND
              CHECKOUT-FLAG = 'Y'
               ADD 1 TO WS-CHECKOUTS-TODAY
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
                   IF R-STATUS = "Occupied"
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

       CALCULATE-DAILY-REVENUE.
           DISPLAY "Starting daily revenue calculation..."
           DISPLAY "Initial revenue: " WS-DAILY-REVENUE

           OPEN INPUT INVOICES-FILE
           IF WS-INVOICE-FILE-STATUS NOT = 00
               DISPLAY "Error opening INVOICES file for revenue"
               GOBACK
           END-IF

           MOVE 'N' TO WS-EOF
           
        *> Read all invoice records sequentially
                   PERFORM UNTIL WS-EOF = 'Y'
                       READ INVOICES-FILE NEXT RECORD
                       AT END
                           MOVE 'Y' TO WS-EOF
                       NOT AT END
                           PERFORM CHECK-DAILY-INVOICE-REVENUE
                       END-READ
                   END-PERFORM         *> Then read subsequent invoice records
           PERFORM UNTIL WS-EOF = 'Y'
               READ INVOICES-FILE NEXT RECORD
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   PERFORM CHECK-DAILY-INVOICE-REVENUE
               END-READ
           END-PERFORM

           CLOSE INVOICES-FILE

           DISPLAY "Final daily revenue: " WS-DAILY-REVENUE.

       CHECK-DAILY-INVOICE-REVENUE.
           DISPLAY "Processing Invoice ID: " INVOICE-ID
           DISPLAY "Invoice creation date: " CREATED-AT-IV
           DISPLAY "Report date: " WS-REPORT-DATE

           *> Include revenue for invoices created on report date
           IF CREATED-AT-IV = WS-REPORT-DATE
               DISPLAY "Invoice matches report date - adding to revenue"
               ADD TOTAL-CHARGE TO WS-DAILY-REVENUE
               DISPLAY "Invoice amount: " TOTAL-CHARGE
               DISPLAY "Running total: " WS-DAILY-REVENUE
           ELSE
               DISPLAY "Invoice date does not match report date"
           END-IF.

       DISPLAY-SUMMARY-REPORT.
           MOVE WS-CHECKINS-TODAY TO WS-DISPLAY-CHECKINS
           MOVE WS-CHECKOUTS-TODAY TO WS-DISPLAY-CHECKOUTS
           MOVE WS-OCCUPIED-ROOMS TO WS-DISPLAY-OCCUPIED
           MOVE WS-TOTAL-ROOMS TO WS-DISPLAY-TOTAL
           MOVE WS-OCCUPANCY-RATE TO WS-DISPLAY-OCCUPANCY
           MOVE WS-DAILY-REVENUE TO WS-DISPLAY-REVENUE

           DISPLAY CLEAR-SCREEN
           DISPLAY BLUE-COLOR
           DISPLAY "==================================================="
           "============================"
           DISPLAY "                         DAILY SUMMARY REPORT SYST"
           "EM                         "
           DISPLAY "==================================================="
           "============================"
           RESET-COLOR
           DISPLAY "                                                   "
           DISPLAY "                     Report Date: " WS-REPORT-YEAR
           "/"
                   WS-REPORT-MONTH "/" WS-REPORT-DAY
           DISPLAY "                                                   "
           DISPLAY CYAN-COLOR "                     CHECK-IN/CHECK-OUT "
           "ACTIVITY:" RESET-COLOR
           DISPLAY "                       Check-ins Today      : "
           FUNCTION TRIM(WS-DISPLAY-CHECKINS)
           DISPLAY "                       Check-outs Today     : "
           FUNCTION TRIM(WS-DISPLAY-CHECKOUTS)
           DISPLAY "                                                   "
           DISPLAY CYAN-COLOR "                     ROOM OCCUPANCY:"
           RESET-COLOR
           DISPLAY "                       Occupied Rooms       : "
           FUNCTION TRIM(WS-DISPLAY-OCCUPIED)
           DISPLAY "                       Total Rooms          : "
           FUNCTION TRIM(WS-DISPLAY-TOTAL)
           DISPLAY "                       Occupancy Rate       : "
           FUNCTION TRIM(WS-DISPLAY-OCCUPANCY) "%"
           DISPLAY "                                                   "
           DISPLAY CYAN-COLOR "                     REVENUE:"
           RESET-COLOR
           DISPLAY "                       Daily Revenue        : $"
           FUNCTION TRIM(WS-DISPLAY-REVENUE)
           DISPLAY "                                                   "
           DISPLAY "==================================================="
           "============================"
           DISPLAY "                                                   "
           DISPLAY "Press ENTER to continue...    "

           ACCEPT WS-DUMMY-INPUT.

       END PROGRAM dailyReport.
