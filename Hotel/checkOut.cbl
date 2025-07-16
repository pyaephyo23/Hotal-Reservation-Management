        IDENTIFICATION DIVISION.
        PROGRAM-ID. checkOut.
        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT ROOMS-FILE ASSIGN TO '../DATA/ROOMS.DAT'
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS ROOM-ID
                  FILE STATUS IS WS-ROOM-FILE-STATUS.
           SELECT BOOKING-FILE ASSIGN TO '../DATA/BOOKINGS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS BOOKING-ID
               ALTERNATE RECORD KEY IS CHECKIN-DATE WITH DUPLICATES
               ALTERNATE RECORD KEY IS CHECKOUT-DATE WITH DUPLICATES
               FILE STATUS IS WS-BOOKING-FILE-STATUS.
           SELECT CUSTOMER-FILE ASSIGN TO '../DATA/CUSTOMERS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUSTOMER-ID
               FILE STATUS IS WS-CUSTOMER-FILE-STATUS.
        DATA DIVISION.
        FILE SECTION.
        FD  ROOMS-FILE.
        COPY "./CopyBooks/ROOMS.cpy".

        FD  BOOKING-FILE.
        COPY "./CopyBooks/BOOKINGS.cpy".
        FD  CUSTOMER-FILE.
        COPY "./CopyBooks/CUSTOMERS.cpy".

        WORKING-STORAGE SECTION.
           01 WS-BOOKING-ID           PIC 9(5).
           01 WS-ROOM-FILE-STATUS     PIC 99.
           01 WS-BOOKING-FILE-STATUS  PIC 99.
           01 WS-CUSTOMER-FILE-STATUS PIC 99.
           01 WS-FOUND-FLAG           PIC X VALUE 'N'.
           01 WS-INVOICE-ID           PIC Z(5).
           01 WS-CURRENT-DATE         PIC X(8).
           01 WS-NIGHTS               PIC 9(3).
           01 WS-SUBTOTAL             PIC 9(7)V99.
           01 WS-TAX                  PIC 9(7)V99.
           01 WS-TOTAL                PIC 9(7)V99.
           01 WS-SERVICE-CHARGES      PIC 9(7)V99.
           01 WS-TAX-RATE             PIC V99 VALUE 0.15.
           01 WS-DAYS-DIFF            PIC 9(8).
           01 WS-CHECKIN-YEAR         PIC 9(4).
           01 WS-CHECKIN-MONTH        PIC 9(2).
           01 WS-CHECKIN-DAY          PIC 9(2).
           01 WS-CHECKOUT-YEAR        PIC 9(4).
           01 WS-CHECKOUT-MONTH       PIC 9(2).
           01 WS-CHECKOUT-DAY         PIC 9(2).
           01 WS-PRICE-DISPLAY        PIC $$,$$$,$$9.99.
           01 WS-INVOICE-COUNTER      PIC 9(5) VALUE 1.
           01 WS-CURRENT-DATE-NUM     PIC 9(8).
           01 WS-ORIGINAL-CHECKOUT    PIC 9(8).
           01 WS-EARLY-CHECKOUT-FLAG  PIC X VALUE 'N'.
           01 WS-IS-LEAP-YEAR         PIC X VALUE 'N'.
           01 WS-MAX-DAYS             PIC 9(2).
           01 WS-TEMP-DAYS            PIC 9(3).
           01 WS-YEAR-COUNTER         PIC 9(4).
           01 WS-MONTH-COUNTER        PIC 9(2).
           01 WS-DATE-YEAR            PIC 9(4).
           01 WS-DATE-MONTH           PIC 9(2).
           01 WS-DATE-DAY             PIC 9(2).

           01 WS-INVOICE-HEADER.
               05 FILLER PIC X(40) VALUE '============================'.
               05 FILLER PIC X(40) VALUE '      INVOICE               '.
               05 FILLER PIC X(40) VALUE '============================'.


       LINKAGE SECTION.
          01 LINK PIC 9.
        PROCEDURE DIVISION USING LINK.

           MAIN-PROCESS.
           DISPLAY "Hotel Check-Out System"
           DISPLAY "===================="
           DISPLAY "Enter Booking ID For Check-Out: "
           ACCEPT WS-BOOKING-ID
           DISPLAY "Enter Additional Servies Charges:"
           ACCEPT WS-SERVICE-CHARGES

           PERFORM OPEN-FILES
           PERFORM SEARCH-BOOKING
           IF WS-FOUND-FLAG = 'Y'
               PERFORM CHECK-AND-UPDATE-CHECKOUT-DATE
               PERFORM GET-CUSTOMER-INFO
               PERFORM GET-ROOM-INFO
               PERFORM CALCULATE-CHARGES
               PERFORM GENERATE-INVOICE
               PERFORM UPDATE-ROOM-STATUS
               PERFORM UPDATE-BOOKING-STATUS
               DISPLAY "Check-out completed successfully!"
               DISPLAY "Room " ROOM-ID " is now available."
           ELSE
               DISPLAY "Booking not found or already checked out."
           END-IF

           PERFORM CLOSE-FILES
           GOBACK.

        OPEN-FILES.
           OPEN I-O ROOMS-FILE BOOKING-FILE
           OPEN INPUT CUSTOMER-FILE
           IF WS-ROOM-FILE-STATUS NOT = 00 OR
              WS-BOOKING-FILE-STATUS NOT = 00 OR
              WS-CUSTOMER-FILE-STATUS NOT = 00
               DISPLAY "Error opening files"
               STOP RUN
           END-IF.

        SEARCH-BOOKING.
           MOVE WS-BOOKING-ID TO BOOKING-ID
           READ BOOKING-FILE
            INVALID KEY
               MOVE 'N' TO WS-FOUND-FLAG
            NOT INVALID KEY
               IF BOOKING-STATUS = 'Active'
                   MOVE 'Y' TO WS-FOUND-FLAG
               ELSE
                   MOVE 'N' TO WS-FOUND-FLAG
                   DISPLAY "Booking is not active or already completed."
               END-IF
           END-READ.

        GET-CUSTOMER-INFO.
           DISPLAY "Fetching customer information..."
           display "Customer ID: " CUSTOMER-ID-BK
           MOVE CUSTOMER-ID-BK TO CUSTOMER-ID
           READ CUSTOMER-FILE
               INVALID KEY
                   DISPLAY "Customer information not found"
           END-READ.

        GET-ROOM-INFO.
           MOVE ROOM-ID-BK TO ROOM-ID
           READ ROOMS-FILE
               INVALID KEY
                   DISPLAY "Room information not found"
           END-READ.

        CALCULATE-CHARGES.
           *> Extract date components for proper calculation
           MOVE CHECKIN-DATE(1:4) TO WS-CHECKIN-YEAR
           MOVE CHECKIN-DATE(5:2) TO WS-CHECKIN-MONTH
           MOVE CHECKIN-DATE(7:2) TO WS-CHECKIN-DAY
           
           MOVE CHECKOUT-DATE(1:4) TO WS-CHECKOUT-YEAR
           MOVE CHECKOUT-DATE(5:2) TO WS-CHECKOUT-MONTH
           MOVE CHECKOUT-DATE(7:2) TO WS-CHECKOUT-DAY
           
           *> Enhanced date difference calculation
           MOVE ZERO TO WS-DAYS-DIFF
           
           *> If same year and month, simple day difference
           IF WS-CHECKIN-YEAR = WS-CHECKOUT-YEAR AND
              WS-CHECKIN-MONTH = WS-CHECKOUT-MONTH
               COMPUTE WS-DAYS-DIFF = 
                   WS-CHECKOUT-DAY - WS-CHECKIN-DAY
           ELSE
               *> Calculate across months/years
               PERFORM CALC-COMPLEX-DATE-DIFF
           END-IF
           
           *> Ensure result is reasonable (fallback protection)
           IF WS-DAYS-DIFF < 0 OR WS-DAYS-DIFF > 400
               DISPLAY "Warning: Date calculation may be inaccurate"
               *> Use simplified calculation as fallback
               COMPUTE WS-DAYS-DIFF = 
                   (WS-CHECKOUT-YEAR - WS-CHECKIN-YEAR) * 365
                   + (WS-CHECKOUT-MONTH - WS-CHECKIN-MONTH) * 30
                   + (WS-CHECKOUT-DAY - WS-CHECKIN-DAY)
           END-IF

           *> Ensure minimum of 1 night for billing
           IF WS-DAYS-DIFF > 0
               MOVE WS-DAYS-DIFF TO WS-NIGHTS
           ELSE
               MOVE 1 TO WS-NIGHTS
           END-IF

           COMPUTE WS-SUBTOTAL = PRICE-PER-NIGHT * WS-NIGHTS
           COMPUTE WS-TAX = (WS-SUBTOTAL + WS-SERVICE-CHARGES) * 
                             WS-TAX-RATE
           COMPUTE WS-TOTAL = WS-SUBTOTAL + WS-SERVICE-CHARGES + WS-TAX.

        GENERATE-INVOICE.
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           MOVE WS-CURRENT-DATE TO WS-CURRENT-DATE
           MOVE WS-INVOICE-COUNTER TO WS-INVOICE-ID

           DISPLAY "========================================"
           DISPLAY "              INVOICE                  "
           DISPLAY "========================================"
           DISPLAY "Invoice ID    : " WS-INVOICE-ID
           DISPLAY "Booking ID    : " BOOKING-ID
           DISPLAY "Invoice Date  : " WS-CURRENT-DATE
           DISPLAY " "
           DISPLAY "Customer Details:"
           DISPLAY "Name          : " CUSTOMER-NAME
           DISPLAY "Phone         : " CUSTOMER-PHONE
           DISPLAY "Email         : " CUSTOMER-EMAIL
           DISPLAY "Address       : " NRC-NUMBER
           DISPLAY " "
           DISPLAY "Booking Details:"
           DISPLAY "Room ID       : " ROOM-ID
           DISPLAY "Room Type     : " ROOM-TYPE
           DISPLAY "Check-in      : " CHECKIN-DATE
           DISPLAY "Check-out     : " CHECKOUT-DATE
           IF WS-EARLY-CHECKOUT-FLAG = 'Y'
               DISPLAY "Original Checkout: " WS-ORIGINAL-CHECKOUT
               DISPLAY "** EARLY CHECKOUT **"
           END-IF
           DISPLAY "Nights        : " WS-NIGHTS
           DISPLAY " "
           DISPLAY "Charges:"
           MOVE PRICE-PER-NIGHT TO WS-PRICE-DISPLAY
           DISPLAY "Rate/Night    : $" WS-PRICE-DISPLAY
           MOVE WS-SUBTOTAL TO WS-PRICE-DISPLAY
           DISPLAY "Subtotal      : $" WS-PRICE-DISPLAY
           MOVE WS-SERVICE-CHARGES TO WS-PRICE-DISPLAY
           DISPLAY "Service Charges: $" WS-PRICE-DISPLAY
           MOVE WS-TAX TO WS-PRICE-DISPLAY
           DISPLAY "Tax (15%)     : $" WS-PRICE-DISPLAY
           DISPLAY "========================================"
           MOVE WS-TOTAL TO WS-PRICE-DISPLAY
           DISPLAY "Total Amount  : $" WS-PRICE-DISPLAY
           DISPLAY "========================================"
           DISPLAY "Status        : GENERATED"
           DISPLAY " ".

        UPDATE-ROOM-STATUS.
           *> Initialize ACTIVE-BOOKING-COUNT if it contains non-numeric data
           IF ACTIVE-BOOKING-COUNT NOT NUMERIC
               MOVE ZERO TO ACTIVE-BOOKING-COUNT
           END-IF

           *> Update room status to Available and decrement active booking count
           MOVE 'Available' TO R-STATUS

           *> Subtract 1 from active booking count (ensure it doesn't go below 0)
           IF ACTIVE-BOOKING-COUNT > 0
               SUBTRACT 1 FROM ACTIVE-BOOKING-COUNT
           ELSE
               MOVE 0 TO ACTIVE-BOOKING-COUNT
           END-IF

           REWRITE ROOMS-RECORD
               INVALID KEY
                   DISPLAY "Error updating room status"
           END-REWRITE.

        UPDATE-BOOKING-STATUS.
           MOVE 'Completed' TO BOOKING-STATUS
           MOVE 'Y' TO CHECKOUT-FLAG
           REWRITE BOOKING-RECORD
               INVALID KEY
                   DISPLAY "Error updating booking status"
           END-REWRITE.

        CHECK-AND-UPDATE-CHECKOUT-DATE.
           *> Get current date
           ACCEPT WS-CURRENT-DATE-NUM FROM DATE YYYYMMDD
           MOVE CHECKOUT-DATE TO WS-ORIGINAL-CHECKOUT
           
           *> Check if current date is earlier than scheduled checkout
           IF WS-CURRENT-DATE-NUM < CHECKOUT-DATE
               DISPLAY "Early checkout detected."
               DISPLAY "Original checkout date: " CHECKOUT-DATE
               DISPLAY "Actual checkout date: " WS-CURRENT-DATE-NUM
               MOVE WS-CURRENT-DATE-NUM TO CHECKOUT-DATE
               MOVE 'Y' TO WS-EARLY-CHECKOUT-FLAG
               
               *> Update the booking record with new checkout date
               REWRITE BOOKING-RECORD
                   INVALID KEY
                       DISPLAY "Error updating checkout date"
                   NOT INVALID KEY
                       DISPLAY "Checkout date updated successfully."
               END-REWRITE
           ELSE
               MOVE 'N' TO WS-EARLY-CHECKOUT-FLAG
               DISPLAY "Checkout on scheduled date: " CHECKOUT-DATE
           END-IF.

        CLOSE-FILES.
           CLOSE ROOMS-FILE BOOKING-FILE CUSTOMER-FILE.

        CALC-COMPLEX-DATE-DIFF.
           *> Step 1: Add remaining days in check-in month
           MOVE WS-CHECKIN-YEAR TO WS-DATE-YEAR
           MOVE WS-CHECKIN-MONTH TO WS-DATE-MONTH
           PERFORM CHECK-LEAP-YEAR
           PERFORM VALIDATE-DAYS-IN-MONTH
           COMPUTE WS-TEMP-DAYS = WS-MAX-DAYS - WS-CHECKIN-DAY
           ADD WS-TEMP-DAYS TO WS-DAYS-DIFF
           
           *> Step 2: Add full months between check-in and check-out
           MOVE WS-CHECKIN-YEAR TO WS-YEAR-COUNTER
           MOVE WS-CHECKIN-MONTH TO WS-MONTH-COUNTER
           
           PERFORM UNTIL (WS-YEAR-COUNTER = WS-CHECKOUT-YEAR AND 
                         WS-MONTH-COUNTER = WS-CHECKOUT-MONTH)
               *> Move to next month
               ADD 1 TO WS-MONTH-COUNTER
               IF WS-MONTH-COUNTER > 12
                   MOVE 1 TO WS-MONTH-COUNTER
                   ADD 1 TO WS-YEAR-COUNTER
               END-IF
               
               *> Don't add days for the checkout month
               IF NOT (WS-YEAR-COUNTER = WS-CHECKOUT-YEAR AND 
                      WS-MONTH-COUNTER = WS-CHECKOUT-MONTH)
                   *> Get days in this month
                   MOVE WS-YEAR-COUNTER TO WS-DATE-YEAR
                   MOVE WS-MONTH-COUNTER TO WS-DATE-MONTH
                   PERFORM CHECK-LEAP-YEAR
                   PERFORM VALIDATE-DAYS-IN-MONTH
                   ADD WS-MAX-DAYS TO WS-DAYS-DIFF
               END-IF
           END-PERFORM
           
           *> Step 3: Add days in checkout month
           ADD WS-CHECKOUT-DAY TO WS-DAYS-DIFF.

        CHECK-LEAP-YEAR.
           MOVE 'N' TO WS-IS-LEAP-YEAR
           
           *> Leap year logic:
           *> - Divisible by 4 AND
           *> - If divisible by 100, must also be divisible by 400
           IF FUNCTION MOD(WS-DATE-YEAR, 4) = 0
               IF FUNCTION MOD(WS-DATE-YEAR, 100) = 0
                   IF FUNCTION MOD(WS-DATE-YEAR, 400) = 0
                       MOVE 'Y' TO WS-IS-LEAP-YEAR
                   END-IF
               ELSE
                   MOVE 'Y' TO WS-IS-LEAP-YEAR
               END-IF
           END-IF.

        VALIDATE-DAYS-IN-MONTH.
           *> Set maximum days for each month
           EVALUATE WS-DATE-MONTH
               WHEN 1  *> January
               WHEN 3  *> March
               WHEN 5  *> May
               WHEN 7  *> July
               WHEN 8  *> August
               WHEN 10 *> October
               WHEN 12 *> December
                   MOVE 31 TO WS-MAX-DAYS
               WHEN 4  *> April
               WHEN 6  *> June
               WHEN 9  *> September
               WHEN 11 *> November
                   MOVE 30 TO WS-MAX-DAYS
               WHEN 2  *> February
                   IF WS-IS-LEAP-YEAR = 'Y'
                       MOVE 29 TO WS-MAX-DAYS
                   ELSE
                       MOVE 28 TO WS-MAX-DAYS
                   END-IF
           END-EVALUATE.

        END PROGRAM checkOut.
