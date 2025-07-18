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
               FILE STATUS IS WS-BOOKING-FILE-STATUS.
           SELECT CUSTOMER-FILE ASSIGN TO '../DATA/CUSTOMERS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUSTOMER-ID
               FILE STATUS IS WS-CUSTOMER-FILE-STATUS.
           SELECT INVOICES-FILE ASSIGN TO '../DATA/INVOICES.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS INVOICE-ID
               FILE STATUS IS WS-INVOICE-FILE-STATUS.
        DATA DIVISION.
        FILE SECTION.
        FD  ROOMS-FILE.
        COPY "./CopyBooks/ROOMS.cpy".

        FD  BOOKING-FILE.
        COPY "./CopyBooks/BOOKINGS.cpy".
        FD  CUSTOMER-FILE.
        COPY "./CopyBooks/CUSTOMERS.cpy".
        FD  INVOICES-FILE.
        COPY "./CopyBooks/INVOICES.cpy".

        WORKING-STORAGE SECTION.
           01 WS-BOOKING-ID           PIC 9(5).
           01 WS-ROOM-FILE-STATUS     PIC 99.
           01 WS-BOOKING-FILE-STATUS  PIC 99.
           01 WS-CUSTOMER-FILE-STATUS PIC 99.
           01 WS-INVOICE-FILE-STATUS  PIC 99.
           01 WS-FOUND-FLAG           PIC X VALUE 'N'.
           01 WS-INVOICE-ID           PIC Z(5).
           01 WS-NIGHTS               PIC 9(3).
           01 WS-SUBTOTAL             PIC 9(9).
           01 WS-TAX                  PIC 9(9).
           01 WS-TOTAL                PIC 9(9).
           01 WS-SERVICE-CHARGES      PIC 9(9).
           01 WS-TAX-RATE             PIC V99 VALUE 0.15.
           01 WS-DAYS-DIFF            PIC 9(8).
           01 WS-CHECKIN-YEAR         PIC 9(4).
           01 WS-CHECKIN-MONTH        PIC 9(2).
           01 WS-CHECKIN-DAY          PIC 9(2).
           01 WS-CHECKOUT-YEAR        PIC 9(4).
           01 WS-CHECKOUT-MONTH       PIC 9(2).
           01 WS-CHECKOUT-DAY         PIC 9(2).
           01 WS-PRICE-DISPLAY        PIC $$,$$$,$$9.
           01 WS-INVOICE-COUNTER      PIC 9(5).
           01 WS-CURRENT-DATE         PIC 9(8).
           01 WS-ORIGINAL-CHECKOUT    PIC 9(8).
           01 WS-EARLY-CHECKOUT-FLAG  PIC X VALUE 'N'.
           01 WS-EOF-INVOICE         PIC X.

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
           PERFORM GET-NEXT-INVOICE-ID
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
           OPEN I-O INVOICES-FILE
           IF WS-ROOM-FILE-STATUS NOT = 00 OR
              WS-BOOKING-FILE-STATUS NOT = 00 OR
              WS-CUSTOMER-FILE-STATUS NOT = 00 OR
              WS-INVOICE-FILE-STATUS NOT = 00
               DISPLAY "Error opening files"
               STOP RUN
           END-IF.

       GET-NEXT-INVOICE-ID.
           OPEN INPUT INVOICES-FILE
           MOVE 0 TO WS-INVOICE-COUNTER
           MOVE 'N' TO WS-EOF-INVOICE

           PERFORM UNTIL WS-EOF-INVOICE = 'Y'
               READ INVOICES-FILE NEXT
                   AT END
                       MOVE 'Y' TO WS-EOF-INVOICE
                   NOT AT END
                       IF INVOICE-ID > WS-INVOICE-COUNTER
                           MOVE INVOICE-ID TO WS-INVOICE-COUNTER
                       END-IF
               END-READ
           END-PERFORM
           CLOSE INVOICES-FILE
           ADD 1 TO WS-INVOICE-COUNTER.

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
           COMPUTE WS-DAYS-DIFF =
           FUNCTION INTEGER-OF-DATE(CHECKOUT-DATE) -
           FUNCTION INTEGER-OF-DATE(CHECKIN-DATE)

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
           OPEN INPUT INVOICES-FILE
           MOVE 0 TO WS-INVOICE-COUNTER
           MOVE 'N' TO WS-EOF-INVOICE

           PERFORM UNTIL WS-EOF-INVOICE = 'Y'
               READ INVOICES-FILE NEXT
                   AT END
                       MOVE 'Y' TO WS-EOF-INVOICE
                   NOT AT END
                       IF INVOICE-ID > WS-INVOICE-COUNTER
                           MOVE INVOICE-ID TO WS-INVOICE-COUNTER
                       END-IF
               END-READ
           END-PERFORM
           CLOSE INVOICES-FILE
           ADD 1 TO WS-INVOICE-COUNTER

           *> Prepare invoice record for writing (only required fields)
           MOVE WS-INVOICE-COUNTER TO INVOICE-ID
           MOVE BOOKING-ID         TO BOOKING-ID-IV
           MOVE WS-SUBTOTAL        TO ROOM-CHARGE
           MOVE WS-SERVICE-CHARGES TO SERVICE-CHARGE
           MOVE 15                 TO TAX-RATE
           MOVE WS-TOTAL           TO TOTAL-CHARGE
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           MOVE   WS-CURRENT-DATE TO CREATED-AT-IV

           OPEN I-O INVOICES-FILE
           WRITE INVOICE-RECORD
               INVALID KEY
                   DISPLAY "Error writing invoice to file"
               NOT INVALID KEY
                   DISPLAY "Invoice record written to file"
           END-WRITE
           DISPLAY "INVOICES-FILE write successful"
           CLOSE INVOICES-FILE

           DISPLAY "========================================"
           DISPLAY "              INVOICE                  "
           DISPLAY "========================================"
           DISPLAY "Invoice ID    : " INVOICE-ID
           DISPLAY "Booking ID    : " BOOKING-ID
           DISPLAY "Invoice Date  : " CREATED-AT-IV
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
           DISPLAY "Rate/Night     :" FUNCTION TRIM(WS-PRICE-DISPLAY)
           MOVE WS-SUBTOTAL TO WS-PRICE-DISPLAY
           DISPLAY "Subtotal       :" FUNCTION TRIM(WS-PRICE-DISPLAY)
           MOVE WS-SERVICE-CHARGES TO WS-PRICE-DISPLAY
           DISPLAY "Service Charges:" FUNCTION TRIM(WS-PRICE-DISPLAY)
           MOVE WS-TAX TO WS-PRICE-DISPLAY
           DISPLAY "Tax (15%)      :" FUNCTION TRIM(WS-PRICE-DISPLAY)
           DISPLAY "========================================"
           MOVE WS-TOTAL TO WS-PRICE-DISPLAY
           DISPLAY "Total Amount   :" FUNCTION TRIM(WS-PRICE-DISPLAY)
           DISPLAY "========================================"
           DISPLAY "Status         : GENERATED"
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
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           MOVE CHECKOUT-DATE TO WS-ORIGINAL-CHECKOUT

           *> Check if current date is earlier than scheduled checkout
           IF FUNCTION INTEGER-OF-DATE(WS-CURRENT-DATE) <
               FUNCTION INTEGER-OF-DATE(CHECKOUT-DATE)
               DISPLAY "Early checkout detected."
               DISPLAY "Original checkout date: " CHECKOUT-DATE
               DISPLAY "Actual checkout date: " WS-CURRENT-DATE
               MOVE WS-CURRENT-DATE TO CHECKOUT-DATE
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
           CLOSE ROOMS-FILE BOOKING-FILE CUSTOMER-FILE INVOICES-FILE.
           END PROGRAM checkOut.
