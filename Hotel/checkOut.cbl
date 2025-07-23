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
           01 WS-CHOICE               PIC 9.
           01 WS-BOOKING-ID           PIC 9(5).
           01 WS-ROOM-ID-INPUT        PIC X(5).
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

           *> Color codes for display
           01 RED-COLOR          PIC X(8) VALUE X"1B5B33316D".
           01 GREEN-COLOR        PIC X(8) VALUE X"1B5B33326D".
           01 RESET-COLOR        PIC X(4) VALUE X"1B5B306D".


       LINKAGE SECTION.
          01 LINK PIC 9.
        PROCEDURE DIVISION USING LINK.

           MAIN-PROCESS.
           DISPLAY "**************************************************"
           DISPLAY "Guest Check-Out Service"
           DISPLAY "1. Check-Out by Booking ID"
           DISPLAY "2. Check-Out by Room ID"
           DISPLAY "9. Return to Main Menu"
           DISPLAY "**************************************************"
           ACCEPT WS-CHOICE
           EVALUATE WS-CHOICE
               WHEN 1
                   PERFORM CHECKOUT-PROCESS
               WHEN 2
                   PERFORM CHECKOUT-BY-ROOM-ID
               WHEN 9
                   GOBACK
               WHEN OTHER
                 DISPLAY RED-COLOR "Invalid selection." RESET-COLOR
                   GO TO MAIN-PROCESS
           END-EVALUATE
           GO TO MAIN-PROCESS.

       CHECKOUT-PROCESS.
           DISPLAY "Please enter your Booking ID: "
           ACCEPT WS-BOOKING-ID

           PERFORM OPEN-FILES
           PERFORM SEARCH-BOOKING
           IF WS-FOUND-FLAG = 'Y'
               DISPLAY GREEN-COLOR
                "Booking found! Processing your checkout..." RESET-COLOR
               DISPLAY "Additional service charges (if any): "
               ACCEPT WS-SERVICE-CHARGES

               PERFORM GET-NEXT-INVOICE-ID
               PERFORM CHECK-AND-UPDATE-CHECKOUT-DATE
               PERFORM GET-CUSTOMER-INFO
               PERFORM GET-ROOM-INFO
               PERFORM CALCULATE-CHARGES
               PERFORM GENERATE-INVOICE
               PERFORM UPDATE-ROOM-STATUS
               PERFORM UPDATE-BOOKING-STATUS
               DISPLAY GREEN-COLOR "Check-out completed successfully!"
               RESET-COLOR
               DISPLAY GREEN-COLOR "Room " ROOM-ID
               " is now available for new guests." RESET-COLOR
               DISPLAY " "
           ELSE
             DISPLAY RED-COLOR
             "Error: Booking ID not found or not eligible for checkout."
             RESET-COLOR
             DISPLAY " "
           END-IF

           PERFORM CLOSE-FILES.

       CHECKOUT-BY-ROOM-ID.
           DISPLAY "Please enter Room ID: "
           ACCEPT WS-ROOM-ID-INPUT
           DISPLAY " "
           PERFORM OPEN-FILES
           PERFORM FIND-BOOKING-BY-ROOM-ID
           IF WS-FOUND-FLAG = 'Y'
               DISPLAY GREEN-COLOR
                "Processing your checkout..."
                RESET-COLOR
                DISPLAY " "
               DISPLAY "Additional service charges (if any): "
               ACCEPT WS-SERVICE-CHARGES

               PERFORM GET-NEXT-INVOICE-ID
               PERFORM CHECK-AND-UPDATE-CHECKOUT-DATE
               PERFORM GET-CUSTOMER-INFO
               PERFORM GET-ROOM-INFO
               PERFORM CALCULATE-CHARGES
               PERFORM GENERATE-INVOICE
               PERFORM UPDATE-ROOM-STATUS
               PERFORM UPDATE-BOOKING-STATUS
               DISPLAY GREEN-COLOR "Check-out completed successfully!"
               RESET-COLOR
             DISPLAY GREEN-COLOR
             "Room " ROOM-ID " is now available for new guests."
              RESET-COLOR
           ELSE
             DISPLAY RED-COLOR
             "Error: No guest currently checked into this room."
              RESET-COLOR
           END-IF

           PERFORM CLOSE-FILES.

       FIND-BOOKING-BY-ROOM-ID.
           MOVE 'N' TO WS-FOUND-FLAG
           MOVE 'N' TO WS-EOF-INVOICE

           OPEN INPUT BOOKING-FILE
           PERFORM UNTIL WS-EOF-INVOICE = 'Y'
               READ BOOKING-FILE NEXT
                   AT END
                       MOVE 'Y' TO WS-EOF-INVOICE
                   NOT AT END
                       IF ROOM-ID-BK = WS-ROOM-ID-INPUT
                          AND BOOKING-STATUS = 'Active'
                          AND CHEKIN-FLAG = 'Y'
                          AND CHECKOUT-FLAG = 'N'
                           MOVE 'Y' TO WS-FOUND-FLAG
                           MOVE BOOKING-ID TO WS-BOOKING-ID
                           MOVE 'Y' TO WS-EOF-INVOICE
                           DISPLAY GREEN-COLOR "Guest found in Room "
                                   WS-ROOM-ID-INPUT RESET-COLOR
                           DISPLAY "Booking Reference: " BOOKING-ID
                       END-IF
               END-READ
           END-PERFORM
           CLOSE BOOKING-FILE

           IF WS-FOUND-FLAG = 'Y'
               *> Now re-read the specific booking record with the key
               MOVE WS-BOOKING-ID TO BOOKING-ID
               OPEN INPUT BOOKING-FILE
               READ BOOKING-FILE
                   INVALID KEY
                       MOVE 'N' TO WS-FOUND-FLAG
                       DISPLAY RED-COLOR "Error reading booking record."
                        RESET-COLOR
               END-READ
               CLOSE BOOKING-FILE
           END-IF.

        OPEN-FILES.
           OPEN I-O ROOMS-FILE BOOKING-FILE
           OPEN INPUT CUSTOMER-FILE
           OPEN I-O INVOICES-FILE
           IF WS-ROOM-FILE-STATUS NOT = 00 OR
              WS-BOOKING-FILE-STATUS NOT = 00 OR
              WS-CUSTOMER-FILE-STATUS NOT = 00 OR
              WS-INVOICE-FILE-STATUS NOT = 00
               DISPLAY RED-COLOR "Error opening files" RESET-COLOR
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
               END-IF
           END-READ.

        GET-CUSTOMER-INFO.
           DISPLAY "Retrieving guest information..."
           DISPLAY "Guest ID: " CUSTOMER-ID-BK
           MOVE CUSTOMER-ID-BK TO CUSTOMER-ID
           READ CUSTOMER-FILE
               INVALID KEY
               DISPLAY RED-COLOR
                "Warning: Guest information not found in records"
                 RESET-COLOR
           END-READ.

        GET-ROOM-INFO.
           MOVE ROOM-ID-BK TO ROOM-ID
           READ ROOMS-FILE
               INVALID KEY
                   DISPLAY RED-COLOR "Room information not found"
                   RESET-COLOR
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
           ADD WS-SERVICE-CHARGES TO WS-SUBTOTAL
           COMPUTE WS-TAX = WS-SUBTOTAL * WS-TAX-RATE
           COMPUTE WS-TOTAL = WS-SUBTOTAL + WS-TAX.

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
                   DISPLAY RED-COLOR "Error writing invoice to file"
                    RESET-COLOR
               NOT INVALID KEY
                   DISPLAY GREEN-COLOR
                    "Invoice record created successfully" RESET-COLOR
           END-WRITE
           DISPLAY GREEN-COLOR "Invoice generation completed"
           RESET-COLOR
           CLOSE INVOICES-FILE

           DISPLAY "=================================================="
           DISPLAY "              INVOICE                  "
           DISPLAY "=================================================="
           DISPLAY "Invoice ID    : " INVOICE-ID
           DISPLAY "Booking ID    : " BOOKING-ID
           DISPLAY "Invoice Date  : " CREATED-AT-IV(1:4) "/"
                   CREATED-AT-IV(5:2) "/" CREATED-AT-IV(7:2)
           DISPLAY " "
           DISPLAY "Customer Details:"
           DISPLAY "Name          : " CUSTOMER-NAME
           DISPLAY "Phone         : " CUSTOMER-PHONE
           DISPLAY " "
           DISPLAY "Booking Details:"
           DISPLAY "Room ID       : " ROOM-ID
           DISPLAY "Room Type     : " ROOM-TYPE
           DISPLAY "Check-in      : " CHECKIN-DATE(1:4) "/"
                   CHECKIN-DATE(5:2) "/" CHECKIN-DATE(7:2)
           DISPLAY "Check-out     : " CHECKOUT-DATE(1:4) "/"
                   CHECKOUT-DATE(5:2) "/" CHECKOUT-DATE(7:2)
           IF WS-EARLY-CHECKOUT-FLAG = 'Y'
               DISPLAY "Original Checkout: " WS-ORIGINAL-CHECKOUT(1:4)
               "/"
                       WS-ORIGINAL-CHECKOUT(5:2) "/"
                       WS-ORIGINAL-CHECKOUT(7:2)
               DISPLAY "** EARLY CHECKOUT **"
           END-IF
           DISPLAY "Nights        : " WS-NIGHTS
           DISPLAY " "
           DISPLAY "Charges:"
           MOVE PRICE-PER-NIGHT TO WS-PRICE-DISPLAY
           DISPLAY "Rate/Night        :" FUNCTION TRIM(WS-PRICE-DISPLAY)
           COMPUTE WS-SUBTOTAL = (PRICE-PER-NIGHT * WS-NIGHTS)
           MOVE WS-SUBTOTAL TO WS-PRICE-DISPLAY
           DISPLAY "Room Charges      :" FUNCTION TRIM(WS-PRICE-DISPLAY)
           MOVE WS-SERVICE-CHARGES TO WS-PRICE-DISPLAY
          DISPLAY "Additional Services:" FUNCTION TRIM(WS-PRICE-DISPLAY)
           COMPUTE WS-SUBTOTAL = (PRICE-PER-NIGHT * WS-NIGHTS) +
                                  WS-SERVICE-CHARGES
           MOVE WS-SUBTOTAL TO WS-PRICE-DISPLAY
           DISPLAY "Subtotal          :" FUNCTION TRIM(WS-PRICE-DISPLAY)
           MOVE WS-TAX TO WS-PRICE-DISPLAY
           DISPLAY "Tax (15%)         :" FUNCTION TRIM(WS-PRICE-DISPLAY)
           DISPLAY "=================================================="
           MOVE WS-TOTAL TO WS-PRICE-DISPLAY
           DISPLAY "Total Amount      :" FUNCTION TRIM(WS-PRICE-DISPLAY)
           DISPLAY "=================================================="
           DISPLAY "Status            : COMPLETED"
           DISPLAY " "
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
                   DISPLAY RED-COLOR "Error updating room status"
                    RESET-COLOR
           END-REWRITE.

        UPDATE-BOOKING-STATUS.
           *> Read the booking record with the key first
           MOVE WS-BOOKING-ID TO BOOKING-ID
           OPEN I-O BOOKING-FILE
           READ BOOKING-FILE
               INVALID KEY
                   DISPLAY RED-COLOR "Error reading booking for update"
                   RESET-COLOR
               NOT INVALID KEY
                   *> Update the fields
                   MOVE 'Completed' TO BOOKING-STATUS
                   MOVE 'Y' TO CHECKOUT-FLAG
                   *> Write back the updated record
                   REWRITE BOOKING-RECORD
                       INVALID KEY
                       DISPLAY RED-COLOR "Error updating booking status"
                           RESET-COLOR
                       NOT INVALID KEY
               DISPLAY GREEN-COLOR "Booking status updated to Completed"
                           RESET-COLOR
                   END-REWRITE
           END-READ
           CLOSE BOOKING-FILE.

        CHECK-AND-UPDATE-CHECKOUT-DATE.
           *> Get current date
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           MOVE CHECKOUT-DATE TO WS-ORIGINAL-CHECKOUT

           *> Check if current date is earlier than scheduled checkout
           IF FUNCTION INTEGER-OF-DATE(WS-CURRENT-DATE) <
               FUNCTION INTEGER-OF-DATE(CHECKOUT-DATE)
               DISPLAY " "
               DISPLAY "Early checkout processed."
               DISPLAY "Original checkout date: " CHECKOUT-DATE(1:4) "/"
                       CHECKOUT-DATE(5:2) "/" CHECKOUT-DATE(7:2)
               DISPLAY "Actual checkout date: " WS-CURRENT-DATE(1:4) "/"
                       WS-CURRENT-DATE(5:2) "/" WS-CURRENT-DATE(7:2)
               DISPLAY " "
               MOVE WS-CURRENT-DATE TO CHECKOUT-DATE
               MOVE 'Y' TO WS-EARLY-CHECKOUT-FLAG

               *> Update the booking record with new checkout date
               REWRITE BOOKING-RECORD
                   INVALID KEY
                       DISPLAY RED-COLOR "Error updating checkout date"
                        RESET-COLOR
                   NOT INVALID KEY
                       DISPLAY GREEN-COLOR
                        "Guest checkout completed successfully."
                        DISPLAY " "
                        RESET-COLOR
               END-REWRITE
           ELSE
               MOVE 'N' TO WS-EARLY-CHECKOUT-FLAG
               DISPLAY "Checkout on scheduled date: "
               CHECKOUT-DATE(1:4) "/"
                       CHECKOUT-DATE(5:2) "/" CHECKOUT-DATE(7:2)
           END-IF.

        CLOSE-FILES.
           CLOSE ROOMS-FILE BOOKING-FILE CUSTOMER-FILE INVOICES-FILE.
           END PROGRAM checkOut.
