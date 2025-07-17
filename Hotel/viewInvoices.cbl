       IDENTIFICATION DIVISION.
       PROGRAM-ID. viewInvoices.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INVOICE-FILE ASSIGN TO '../DATA/INVOICES.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS INVOICE-ID
               FILE STATUS IS WS-INVOICE-FILE-STATUS.
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

       DATA DIVISION.
       FILE SECTION.
       FD  INVOICE-FILE.
       COPY "./CopyBooks/INVOICES.cpy".

       FD  BOOKING-FILE.
       COPY "./CopyBooks/BOOKINGS.cpy".

       FD  CUSTOMER-FILE.
       COPY "./CopyBooks/CUSTOMERS.cpy".

       WORKING-STORAGE SECTION.
       01  WS-EOF                     PIC X VALUE 'N'.
       01  WS-INVOICE-COUNTER         PIC 999 VALUE 0.
       01  WS-TOTAL-AMOUNT            PIC 9(9) VALUE 0.
       01  MENU-CHOICE                PIC 9.
       01  WS-INVOICE-FILE-STATUS     PIC 99.
       01  WS-BOOKING-FILE-STATUS     PIC 99.
       01  WS-CUSTOMER-FILE-STATUS    PIC 99.
       01  WS-SEARCH-INVOICE          PIC 9(5).
       01  WS-SEARCH-BOOKING          PIC 9(5).
       01  WS-PRICE-DISPLAY           PIC $$,$$$,$$9.99.

       01  WS-HEADER-1.
           05 FILLER               PIC X(7) VALUE 'INVOICE'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(7) VALUE 'BOOKING'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(11) VALUE 'ROOM CHARGE'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(7) VALUE 'SERVICE'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(3) VALUE 'TAX'.
           05 FILLER               PIC X(5) VALUE SPACES.
           05 FILLER               PIC X(12) VALUE 'TOTAL CHARGE'.

       01  WS-HEADER-2.
           05 FILLER               PIC X(7) VALUE '-------'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(7) VALUE '-------'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(11) VALUE '-----------'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(7) VALUE '-------'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(3) VALUE '---'.
           05 FILLER               PIC X(5) VALUE SPACES.
           05 FILLER               PIC X(12) VALUE '------------'.

       01  WS-DETAIL-LINE.
           05 WS-DL-INVOICE-ID     PIC Z(5)9.
           05 FILLER               PIC X(4) VALUE SPACES.
           05 WS-DL-BOOKING-ID     PIC Z(5)9.
           05 FILLER               PIC X(4) VALUE SPACES.
           05 WS-DL-ROOM-CHARGE    PIC $(9).
           05 FILLER               PIC X(2) VALUE SPACES.
           05 WS-DL-SERVICE        PIC $(9).
           05 FILLER               PIC X(5) VALUE SPACES.
           05 WS-DL-TAX-RATE       PIC 9(2).
           05 FILLER               PIC X(3) VALUE SPACES.
           05 WS-DL-TOTAL          PIC $(9).

       01  WS-CUSTOMER-INFO-LINE.
           05 FILLER               PIC X(15) VALUE 'Customer: '.
           05 WS-CUSTOMER-NAME-DISP PIC X(20).
           05 FILLER               PIC X(10) VALUE ' | Phone: '.
           05 WS-CUSTOMER-PHONE-DISP PIC X(15).

       LINKAGE SECTION.
       01 LINK PIC 9.

       PROCEDURE DIVISION USING LINK.

       MAIN-LOOP.
           PERFORM UNTIL MENU-CHOICE = 9
           DISPLAY " "
           DISPLAY
           "***********************************************************"
           DISPLAY "                View Hotel Invoices"
           DISPLAY
           "***********************************************************"
           DISPLAY "1. View All Invoices"
           DISPLAY "2. Search Invoice by Invoice ID"
           DISPLAY "3. Search Invoices by Booking ID"
           DISPLAY "4. View Invoice Summary Report"
           DISPLAY "9. Go Back"
           DISPLAY
           "***********************************************************"
           ACCEPT MENU-CHOICE
           EVALUATE MENU-CHOICE
               WHEN 1 PERFORM ALL-INVOICES-DSP
               WHEN 2 PERFORM SEARCH-BY-INVOICE-ID
               WHEN 3 PERFORM SEARCH-BY-BOOKING-ID
               WHEN 4 PERFORM INVOICE-SUMMARY-REPORT
               WHEN 9 GOBACK
               WHEN OTHER DISPLAY "Invalid choice! Please try again."
           END-EVALUATE
           END-PERFORM.
           GOBACK.

       ALL-INVOICES-DSP.
           MOVE 0 TO WS-INVOICE-COUNTER
           MOVE 0 TO WS-TOTAL-AMOUNT
           MOVE 'N' TO WS-EOF
           PERFORM OPEN-FILES
           IF WS-INVOICE-FILE-STATUS = '00'
               OR WS-INVOICE-FILE-STATUS = '97'
               DISPLAY " "
               DISPLAY "ALL INVOICES"
               DISPLAY "============"
               PERFORM DISPLAY-HEADERS
               PERFORM READ-AND-DISPLAY-ALL UNTIL WS-EOF = 'Y'
               PERFORM DISPLAY-SUMMARY
           ELSE
               DISPLAY
               "Error opening invoice file or no invoices found."
           END-IF
           PERFORM CLOSE-FILES.

       SEARCH-BY-INVOICE-ID.
           DISPLAY " "
           DISPLAY "SEARCH BY INVOICE ID"
           DISPLAY "===================="
           DISPLAY "Enter Invoice ID: "
           ACCEPT WS-SEARCH-INVOICE

           MOVE 0 TO WS-INVOICE-COUNTER
           MOVE 'N' TO WS-EOF
           PERFORM OPEN-FILES
           IF WS-INVOICE-FILE-STATUS = '00'
               OR WS-INVOICE-FILE-STATUS = '97'
               MOVE WS-SEARCH-INVOICE TO INVOICE-ID
               READ INVOICE-FILE KEY IS INVOICE-ID
                   INVALID KEY
                       DISPLAY "Invoice ID "
                       WS-SEARCH-INVOICE " not found."
                   NOT INVALID KEY
                       PERFORM DISPLAY-HEADERS
                       PERFORM DISPLAY-DETAILED-INVOICE
                       ADD 1 TO WS-INVOICE-COUNTER
               END-READ
           ELSE
               DISPLAY "Error opening invoice file."
           END-IF
           PERFORM CLOSE-FILES.

       SEARCH-BY-BOOKING-ID.
           DISPLAY " "
           DISPLAY "SEARCH BY BOOKING ID"
           DISPLAY "===================="
           DISPLAY "Enter Booking ID: "
           ACCEPT WS-SEARCH-BOOKING

           MOVE 0 TO WS-INVOICE-COUNTER
           MOVE 'N' TO WS-EOF
           PERFORM OPEN-FILES
           IF WS-INVOICE-FILE-STATUS = '00'
               OR WS-INVOICE-FILE-STATUS = '97'
               DISPLAY " "
               DISPLAY "INVOICES FOR BOOKING ID: " WS-SEARCH-BOOKING
               DISPLAY "========================================"
               PERFORM DISPLAY-HEADERS
               START INVOICE-FILE KEY IS GREATER THAN
               OR EQUAL TO INVOICE-ID
               READ INVOICE-FILE NEXT RECORD
                   AT END MOVE 'Y' TO WS-EOF
               END-READ

               PERFORM UNTIL WS-EOF = 'Y'
                   IF BOOKING-ID-IV = WS-SEARCH-BOOKING
                       PERFORM DISPLAY-DETAILED-INVOICE
                       ADD 1 TO WS-INVOICE-COUNTER
                   END-IF
                   READ INVOICE-FILE NEXT RECORD
                       AT END MOVE 'Y' TO WS-EOF
                   END-READ
               END-PERFORM

               IF WS-INVOICE-COUNTER = 0
                   DISPLAY "No invoices found for Booking ID "
                   WS-SEARCH-BOOKING
               ELSE
                   DISPLAY " "
                   DISPLAY "Total invoices found: " WS-INVOICE-COUNTER
               END-IF
           ELSE
               DISPLAY "Error opening invoice file."
           END-IF
           PERFORM CLOSE-FILES.

       INVOICE-SUMMARY-REPORT.
           MOVE 0 TO WS-INVOICE-COUNTER
           MOVE 0 TO WS-TOTAL-AMOUNT
           MOVE 'N' TO WS-EOF
           PERFORM OPEN-FILES
           IF WS-INVOICE-FILE-STATUS = '00'
               OR WS-INVOICE-FILE-STATUS = '97'
               DISPLAY " "
               DISPLAY "========================================"
               DISPLAY "        INVOICE SUMMARY REPORT"
               DISPLAY "========================================"

               START INVOICE-FILE KEY IS GREATER THAN
               OR EQUAL TO INVOICE-ID
               READ INVOICE-FILE NEXT RECORD
                   AT END MOVE 'Y' TO WS-EOF
               END-READ

               PERFORM UNTIL WS-EOF = 'Y'
                   ADD 1 TO WS-INVOICE-COUNTER
                   ADD TOTAL-CHARGE TO WS-TOTAL-AMOUNT
                   READ INVOICE-FILE NEXT RECORD
                       AT END MOVE 'Y' TO WS-EOF
                   END-READ
               END-PERFORM

               DISPLAY "Total Invoices Generated: " WS-INVOICE-COUNTER
               MOVE WS-TOTAL-AMOUNT TO WS-PRICE-DISPLAY
               DISPLAY "Total Revenue: " WS-PRICE-DISPLAY

               IF WS-INVOICE-COUNTER > 0
                   COMPUTE WS-PRICE-DISPLAY = WS-TOTAL-AMOUNT /
                   WS-INVOICE-COUNTER
                   DISPLAY "Average Invoice Amount: " WS-PRICE-DISPLAY
               END-IF
               DISPLAY "========================================"
           ELSE
               DISPLAY
               "Error opening invoice file or no invoices found."
           END-IF
           PERFORM CLOSE-FILES.

       OPEN-FILES.
           OPEN INPUT INVOICE-FILE BOOKING-FILE CUSTOMER-FILE.

       CLOSE-FILES.
           CLOSE INVOICE-FILE BOOKING-FILE CUSTOMER-FILE.

       DISPLAY-HEADERS.
           DISPLAY WS-HEADER-1
           DISPLAY WS-HEADER-2.

       READ-AND-DISPLAY-ALL.
           *> Always start from the lowest possible key to avoid duplicates or missing records
           MOVE 0 TO INVOICE-ID
           START INVOICE-FILE KEY IS NOT LESS THAN INVOICE-ID
               INVALID KEY MOVE 'Y' TO WS-EOF
           END-START
           IF WS-EOF NOT = 'Y'
               READ INVOICE-FILE NEXT RECORD
                   AT END MOVE 'Y' TO WS-EOF
               END-READ
           END-IF

           PERFORM UNTIL WS-EOF = 'Y'
               PERFORM DISPLAY-INVOICE-RECORD
               ADD 1 TO WS-INVOICE-COUNTER
               ADD TOTAL-CHARGE TO WS-TOTAL-AMOUNT
               READ INVOICE-FILE NEXT RECORD
                   AT END MOVE 'Y' TO WS-EOF
               END-READ
           END-PERFORM.

       DISPLAY-INVOICE-RECORD.
           MOVE INVOICE-ID TO WS-DL-INVOICE-ID
           MOVE BOOKING-ID-IV TO WS-DL-BOOKING-ID
           MOVE ROOM-CHARGE TO WS-DL-ROOM-CHARGE
           MOVE SERVICE-CHARGE TO WS-DL-SERVICE
           IF TAX-RATE NUMERIC
               MOVE TAX-RATE TO WS-DL-TAX-RATE
           ELSE
               MOVE 15 TO WS-DL-TAX-RATE
           END-IF
           MOVE TOTAL-CHARGE TO WS-DL-TOTAL
           DISPLAY WS-DETAIL-LINE.

       DISPLAY-DETAILED-INVOICE.
           PERFORM DISPLAY-INVOICE-RECORD
           PERFORM GET-BOOKING-DETAILS
           PERFORM GET-CUSTOMER-DETAILS
           DISPLAY WS-CUSTOMER-INFO-LINE
           DISPLAY " ".

       GET-BOOKING-DETAILS.
           MOVE BOOKING-ID-IV TO BOOKING-ID
           READ BOOKING-FILE KEY IS BOOKING-ID
               INVALID KEY
                   DISPLAY "  Warning: Booking details not found"
           END-READ.

       GET-CUSTOMER-DETAILS.
           MOVE CUSTOMER-ID-BK TO CUSTOMER-ID
           READ CUSTOMER-FILE KEY IS CUSTOMER-ID
               INVALID KEY
                   MOVE "N/A" TO WS-CUSTOMER-NAME-DISP
                   MOVE "N/A" TO WS-CUSTOMER-PHONE-DISP
               NOT INVALID KEY
                   MOVE CUSTOMER-NAME(1:20) TO WS-CUSTOMER-NAME-DISP
                   MOVE CUSTOMER-PHONE TO WS-CUSTOMER-PHONE-DISP
           END-READ.

       DISPLAY-SUMMARY.
           DISPLAY " "
           IF WS-INVOICE-COUNTER = 0
               DISPLAY "No invoices found."
           ELSE
               DISPLAY "Total Invoices: " WS-INVOICE-COUNTER
               MOVE WS-TOTAL-AMOUNT TO WS-PRICE-DISPLAY
               DISPLAY "Total Amount: " WS-PRICE-DISPLAY
           END-IF
           DISPLAY " ".

       END PROGRAM viewInvoices.
