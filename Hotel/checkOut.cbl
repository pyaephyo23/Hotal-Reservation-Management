       IDENTIFICATION DIVISION.
       PROGRAM-ID. checkOut.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CHECKINOUT-FILE ASSIGN TO './DATA/CHECKINOUT.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CHECKIN-ID.
           SELECT ROOMS-FILE ASSIGN TO './DATA/ROOMS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ROOM-ID.
           SELECT BOOKING-FILE ASSIGN TO './DATA/BOOKINGS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS BOOKING-ID.
           SELECT CUSTOMER-FILE ASSIGN TO './DATA/CUSTOMERS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUSTOMER-ID.
           SELECT STAYLOG-FILE ASSIGN TO './DATA/STAYLOG.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS STAYLOG-ID.
           SELECT INVOICES-FILE ASSIGN TO './DATA/INVOICES.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS INVOICE-ID.

       DATA DIVISION.
       FILE SECTION.
       FD  CHECKINOUT-FILE.
       COPY "./CopyBooks/CHECKINOUT.cpy".

       FD  ROOMS-FILE.
       COPY "./CopyBooks/ROOMS.cpy".

       FD  BOOKING-FILE.
       COPY "./CopyBooks/BOOKINGS.cpy".

       FD  CUSTOMER-FILE.
       COPY "./CopyBooks/CUSTOMERS.cpy".

       FD  STAYLOG-FILE.
       COPY "./CopyBooks/STAYLOG.cpy".

       FD  INVOICES-FILE.
       COPY "./CopyBooks/INVOICES.cpy".

       WORKING-STORAGE SECTION.
       *> Menu control variables
       01 WS-CHOICE                   PIC 9.
       01 WS-EXIT-FLAG                PIC X VALUE 'N'.
       01 WS-FOUND                    PIC X VALUE 'N'.
       01 WS-EOF                      PIC X VALUE 'N'.
       01 WS-CONFIRMATION             PIC X.

       *> Search criteria
       01 WS-SEARCH-CHECKIN-ID        PIC 9(5).
       01 WS-SEARCH-ROOM-ID           PIC X(5).
       01 WS-SEARCH-CUSTOMER-NAME     PIC X(20).

       *> Date and time variables
       01 WS-CURRENT-DATE             PIC 9(8).
       01 WS-CURRENT-TIME             PIC 9(6).
       01 WS-CHECKOUT-DATE            PIC 9(8).
       01 WS-CHECKOUT-TIME            PIC 9(6).

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

       *> Customer and room information
       01 WS-CUSTOMER-NAME            PIC X(20).
       01 WS-CUSTOMER-PHONE           PIC X(15).
       01 WS-ROOM-TYPE                PIC X(10).
       01 WS-ROOM-PRICE               PIC 9(9).

       *> Billing calculations
       01 WS-STAY-DAYS                PIC 9(3).
       01 WS-ROOM-CHARGES             PIC 9(9).
       01 WS-SERVICE-CHARGES          PIC 9(9).
       01 WS-TAX-AMOUNT               PIC 9(9).
       01 WS-TOTAL-AMOUNT             PIC 9(9).
       01 WS-TAX-RATE                 PIC V99 VALUE 0.15.

       *> Display formatting
       01 WS-FORMATTED-PRICE          PIC $(9).
       01 WS-FORMATTED-TOTAL          PIC $(9).

       *> Invoice generation
       01 WS-NEXT-INVOICE-ID          PIC 9(5) VALUE 0.

       LINKAGE SECTION.
       01 LINK                        PIC 9.

       PROCEDURE DIVISION USING LINK.
       MAIN-PROCEDURE.
           PERFORM UNTIL WS-EXIT-FLAG = 'Y'
               DISPLAY " "
               DISPLAY "=============================================="
               DISPLAY "            GUEST CHECK-OUT SYSTEM           "
               DISPLAY "=============================================="
               DISPLAY "1. Check-out"
               DISPLAY "9. Return to Main Menu"
               DISPLAY "=============================================="
               DISPLAY "Enter choice: "
               ACCEPT WS-CHOICE

               EVALUATE WS-CHOICE
                   WHEN 1
                       PERFORM CHECKOUT
                   WHEN 9
                       MOVE 'Y' TO WS-EXIT-FLAG
                   WHEN OTHER
                       DISPLAY "Invalid choice. Please try again."
               END-EVALUATE
           END-PERFORM

           GOBACK.

       *> Check-out guest with Check-in ID and Room Number verification
       CHECKOUT.
           DISPLAY " "
           DISPLAY "========== CHECK-OUT =========="
           DISPLAY "Enter Check-in ID: "
           ACCEPT WS-SEARCH-CHECKIN-ID

           DISPLAY "Enter Room Number: "
           ACCEPT WS-SEARCH-ROOM-ID

           MOVE 'N' TO WS-FOUND
           OPEN I-O CHECKINOUT-FILE
           MOVE WS-SEARCH-CHECKIN-ID TO CHECKIN-ID
           READ CHECKINOUT-FILE KEY IS CHECKIN-ID
               INVALID KEY
                   DISPLAY "No check-in record found with ID "
                           WS-SEARCH-CHECKIN-ID
               NOT INVALID KEY
                   *> Verify room number matches
                   IF ROOM-ID-IO NOT = WS-SEARCH-ROOM-ID
                       DISPLAY "Error: Room number does not match."
                       DISPLAY "Check-in ID " WS-SEARCH-CHECKIN-ID
                               " is for room " ROOM-ID-IO
                               ", not " WS-SEARCH-ROOM-ID
                   ELSE
                       IF CHECKOUT-FLAG = 'Y'
                           DISPLAY
                           "Checked out already completed."
                       ELSE
                           MOVE 'Y' TO WS-FOUND
                           PERFORM PROCESS-CHECKOUT
                       END-IF
                   END-IF
           END-READ
           CLOSE CHECKINOUT-FILE

           IF WS-FOUND = 'N'
               DISPLAY "Check-out not completed."
           END-IF.

       *> Process the actual checkout
       PROCESS-CHECKOUT.
           *> Display check-in details
           PERFORM DISPLAY-CHECKIN-DETAILS

           *> Get confirmation
           DISPLAY " "
           DISPLAY "Confirm check-out? (Y/N): "
           ACCEPT WS-CONFIRMATION

           IF WS-CONFIRMATION = 'Y' OR WS-CONFIRMATION = 'y'
               *> Get current date and time
               PERFORM GET-CURRENT-DATETIME

               *> Get additional information
               PERFORM GET-GUEST-INFORMATION

               *> Calculate billing
               PERFORM CALCULATE-BILLING

               *> Generate invoice
               PERFORM GENERATE-INVOICE

               *> Update check-out record
               PERFORM UPDATE-CHECKOUT-RECORD

               *> Update room status
               PERFORM UPDATE-ROOM-STATUS

               DISPLAY " "
               DISPLAY "✓ Check-out completed successfully!"
               DISPLAY "✓ Room " ROOM-ID-IO " is now Available."
           ELSE
               DISPLAY "Check-out cancelled."
           END-IF.

       *> Display check-in details
       DISPLAY-CHECKIN-DETAILS.
           *> Format check-in date and time
           MOVE ACTUAL-CHECKIN-DATE TO WS-TEMP-DATE
           STRING WS-TEMP-DATE(1:4) "-" WS-TEMP-DATE(5:2) "-"
                  WS-TEMP-DATE(7:2) INTO WS-FORMATTED-DATE

           MOVE ACTUAL-CHECKIN-TIME TO WS-TEMP-TIME
           STRING WS-TEMP-TIME(1:2) ":" WS-TEMP-TIME(3:2) ":"
                  WS-TEMP-TIME(5:2) INTO WS-FORMATTED-TIME

           DISPLAY " "
           DISPLAY "========== CHECK-IN DETAILS =========="
           DISPLAY "Check-in ID: " CHECKIN-ID
           DISPLAY "Room Number: " ROOM-ID-IO
           DISPLAY "Booking ID:  " BOOKING-ID-IO
           DISPLAY "Check-in:    " WS-FORMATTED-DATE " "
           WS-FORMATTED-TIME
           DISPLAY "======================================".

       *> Get current date and time
       GET-CURRENT-DATETIME.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE
           MOVE FUNCTION CURRENT-DATE(9:6) TO WS-CURRENT-TIME

           MOVE WS-CURRENT-DATE TO WS-CHECKOUT-DATE
           MOVE WS-CURRENT-TIME TO WS-CHECKOUT-TIME.

       *> Get guest and room information
       GET-GUEST-INFORMATION.
           *> Get customer info from staylog
           OPEN INPUT STAYLOG-FILE
           MOVE 'N' TO WS-EOF
           MOVE 'N' TO WS-FOUND

           PERFORM UNTIL WS-EOF = 'Y' OR WS-FOUND = 'Y'
               READ STAYLOG-FILE NEXT
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF CHECKIN-ID-SL = CHECKIN-ID
                           MOVE CUSTOMER-ID-SL TO CUSTOMER-ID
                           OPEN INPUT CUSTOMER-FILE
                           READ CUSTOMER-FILE KEY IS CUSTOMER-ID
                           INVALID KEY
                              MOVE "Unknown" TO WS-CUSTOMER-NAME
                              MOVE "Unknown" TO WS-CUSTOMER-PHONE
                           NOT INVALID KEY
                              MOVE CUSTOMER-NAME TO WS-CUSTOMER-NAME
                               MOVE CUSTOMER-PHONE TO WS-CUSTOMER-PHONE
                           END-READ
                           CLOSE CUSTOMER-FILE
                           MOVE 'Y' TO WS-FOUND
                       END-IF
               END-READ
           END-PERFORM
           CLOSE STAYLOG-FILE

           *> Get room info
           OPEN INPUT ROOMS-FILE
           MOVE ROOM-ID-IO TO ROOM-ID
           READ ROOMS-FILE KEY IS ROOM-ID
               INVALID KEY
                   MOVE "Unknown" TO WS-ROOM-TYPE
                   MOVE 0 TO WS-ROOM-PRICE
               NOT INVALID KEY
                   MOVE ROOM-TYPE TO WS-ROOM-TYPE
                   MOVE PRICE-PER-NIGHT TO WS-ROOM-PRICE
           END-READ
           CLOSE ROOMS-FILE.

       *> Calculate billing amounts
       CALCULATE-BILLING.
           *> Calculate stay duration
           IF WS-CHECKOUT-DATE = ACTUAL-CHECKIN-DATE
               *> Same-day checkout, charge for 1 day
               MOVE 1 TO WS-STAY-DAYS
           ELSE
               COMPUTE WS-STAY-DAYS =
               FUNCTION INTEGER-OF-DATE(WS-CHECKOUT-DATE) -
               FUNCTION INTEGER-OF-DATE(ACTUAL-CHECKIN-DATE)
           END-IF

           *> Calculate room charges
           COMPUTE WS-ROOM-CHARGES = WS-ROOM-PRICE * WS-STAY-DAYS

           *> Get service charges
           DISPLAY " "
           DISPLAY "Enter additional service charges (0 if none): "
           ACCEPT WS-SERVICE-CHARGES

           *> Calculate tax
           COMPUTE WS-TAX-AMOUNT =
               (WS-ROOM-CHARGES + WS-SERVICE-CHARGES) * WS-TAX-RATE

           *> Calculate total
           COMPUTE WS-TOTAL-AMOUNT =
               WS-ROOM-CHARGES + WS-SERVICE-CHARGES + WS-TAX-AMOUNT.

       *> Generate and display invoice
       GENERATE-INVOICE.
           *> Generate next invoice ID
           PERFORM GENERATE-NEXT-INVOICE-ID

           *> Create invoice record
           OPEN I-O INVOICES-FILE
           MOVE WS-NEXT-INVOICE-ID TO INVOICE-ID
           MOVE BOOKING-ID-IO TO BOOKING-ID-IV
           MOVE WS-ROOM-CHARGES TO ROOM-CHARGE
           MOVE WS-SERVICE-CHARGES TO SERVICE-CHARGE
           MOVE 15 TO TAX-RATE
           MOVE WS-TOTAL-AMOUNT TO TOTAL-CHARGE
           MOVE WS-CURRENT-DATE TO CREATED-AT-IV

           WRITE INVOICE-RECORD
               INVALID KEY
                   DISPLAY "Error: Unable to create invoice record."
               NOT INVALID KEY
                   DISPLAY "✓ Invoice created successfully!"
           END-WRITE
           CLOSE INVOICES-FILE

           *> Display invoice
           PERFORM DISPLAY-INVOICE.

       *> Display formatted invoice
       DISPLAY-INVOICE.
           *> Format checkout date and time
           MOVE WS-CHECKOUT-DATE TO WS-TEMP-DATE
           STRING WS-TEMP-DATE(1:4) "-" WS-TEMP-DATE(5:2) "-"
                  WS-TEMP-DATE(7:2) INTO WS-FORMATTED-DATE

           MOVE WS-CHECKOUT-TIME TO WS-TEMP-TIME
           STRING WS-TEMP-TIME(1:2) ":" WS-TEMP-TIME(3:2) ":"
                  WS-TEMP-TIME(5:2) INTO WS-FORMATTED-TIME

           DISPLAY " "
           DISPLAY "================================================"
           DISPLAY "                   INVOICE                      "
           DISPLAY "================================================"
           DISPLAY "Invoice ID:     " INVOICE-ID
           DISPLAY "Check-out Date: " WS-FORMATTED-DATE
           DISPLAY "Check-out Time: " WS-FORMATTED-TIME
           DISPLAY " "
           DISPLAY "Guest Information:"
           DISPLAY "  Name:         " WS-CUSTOMER-NAME
           DISPLAY "  Phone:        " WS-CUSTOMER-PHONE
           DISPLAY " "
           DISPLAY "Stay Information:"
           DISPLAY "  Room:         " ROOM-ID-IO " (" WS-ROOM-TYPE ")"
           DISPLAY "  Check-in ID:  " CHECKIN-ID
           DISPLAY "  Days Stayed:  " WS-STAY-DAYS
           DISPLAY " "
           DISPLAY "Charges:"
           MOVE WS-ROOM-PRICE TO WS-FORMATTED-PRICE
           DISPLAY "  Room Rate:    " WS-FORMATTED-PRICE " per night"
           MOVE WS-ROOM-CHARGES TO WS-FORMATTED-TOTAL
           DISPLAY "  Room Charges: " WS-FORMATTED-TOTAL
           MOVE WS-SERVICE-CHARGES TO WS-FORMATTED-TOTAL
           DISPLAY "  Service Fees: " WS-FORMATTED-TOTAL
           MOVE WS-TAX-AMOUNT TO WS-FORMATTED-TOTAL
           DISPLAY "  Tax (15%):    " WS-FORMATTED-TOTAL
           DISPLAY "================================================"
           MOVE WS-TOTAL-AMOUNT TO WS-FORMATTED-TOTAL
           DISPLAY "  TOTAL AMOUNT: " WS-FORMATTED-TOTAL
           DISPLAY "================================================".

       *> Update checkout record
       UPDATE-CHECKOUT-RECORD.
           MOVE 'Y' TO CHECKOUT-FLAG
           MOVE WS-CHECKOUT-DATE TO CHECKOUT-DATE
           MOVE WS-CHECKOUT-TIME TO CHECKOUT-TIME

           REWRITE CHECKINOUT-RECORD
               INVALID KEY
                   DISPLAY "Error: Could not update check-out record."
               NOT INVALID KEY
                   DISPLAY "✓ Check-out record updated successfully!"
           END-REWRITE.

       *> Update room status to Available
       UPDATE-ROOM-STATUS.
           OPEN I-O ROOMS-FILE
           MOVE ROOM-ID-IO TO ROOM-ID
           READ ROOMS-FILE KEY IS ROOM-ID
               INVALID KEY
                   DISPLAY "Warning: Could not find room " ROOM-ID-IO
               NOT INVALID KEY
                   MOVE 'Available' TO R-STATUS
                   REWRITE ROOMS-RECORD
                       INVALID KEY
                           DISPLAY
                           "Error: Unable to update room status."
                       NOT INVALID KEY
                           DISPLAY
                           "✓ Room status updated to Available."
                   END-REWRITE
           END-READ
           CLOSE ROOMS-FILE.

       *> Generate next invoice ID
       GENERATE-NEXT-INVOICE-ID.
           MOVE 'N' TO WS-EOF
           MOVE 0 TO WS-NEXT-INVOICE-ID

           OPEN INPUT INVOICES-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ INVOICES-FILE NEXT
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF INVOICE-ID > WS-NEXT-INVOICE-ID
                           MOVE INVOICE-ID TO WS-NEXT-INVOICE-ID
                       END-IF
               END-READ
           END-PERFORM
           CLOSE INVOICES-FILE

           ADD 1 TO WS-NEXT-INVOICE-ID.

       END PROGRAM checkOut.
