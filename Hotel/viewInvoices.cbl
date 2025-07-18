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

       DATA DIVISION.
       FILE SECTION.
       FD  INVOICE-FILE.
       COPY "./CopyBooks/INVOICES.cpy".

       WORKING-STORAGE SECTION.
       01  WS-EOF                     PIC X VALUE 'N'.
       01  WS-INVOICE-COUNTER         PIC 999 VALUE 0.
       01  WS-TOTAL-AMOUNT            PIC 9(9) VALUE 0.
       01  MENU-CHOICE                PIC 9.
       01  WS-INVOICE-FILE-STATUS     PIC 99.
       01  WS-SEARCH-INVOICE          PIC 9(5).
       01  WS-SEARCH-BOOKING          PIC 9(5).
       01  WS-PRICE-DISPLAY           PIC $$,$$$,$$9.
       01  WS-FORMATTED-DATE          PIC X(10).

       01  WS-HEADER-1.
           05 FILLER               PIC X(7) VALUE 'INVOICE'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(7) VALUE 'BOOKING'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(10) VALUE 'CREATED '.
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
           05 FILLER               PIC X(10) VALUE '----------'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(11) VALUE '-----------'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(7) VALUE '-------'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(4) VALUE '----'.
           05 FILLER               PIC X(4) VALUE SPACES.
           05 FILLER               PIC X(12) VALUE '------------'.

       01  WS-DETAIL-LINE.
           05 WS-DL-INVOICE-ID     PIC Z(5)9.
           05 FILLER               PIC X(4) VALUE SPACES.
           05 WS-DL-BOOKING-ID     PIC Z(5)9.
           05 FILLER               PIC X(4) VALUE SPACES.
           05 WS-DL-CREATED-AT     PIC X(10).
           05 FILLER               PIC X(3) VALUE SPACES.
           05 WS-DL-ROOM-CHARGE    PIC $(9).
           05 FILLER               PIC X(2) VALUE SPACES.
           05 WS-DL-SERVICE        PIC $(9).
           05 FILLER               PIC X(5) VALUE SPACES.
           05 WS-DL-TAX-RATE       PIC 9(2).
           05 FILLER               PIC X VALUE '%'.
           05 FILLER               PIC X(1) VALUE SPACES.
           05 WS-DL-TOTAL          PIC $(9).

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
           DISPLAY "3. Search Invoice by Booking ID"
           DISPLAY "9. Go Back"
           DISPLAY
           "***********************************************************"
           ACCEPT MENU-CHOICE
           EVALUATE MENU-CHOICE
               WHEN 1 PERFORM ALL-INVOICES-DSP
               WHEN 2 PERFORM SEARCH-BY-INVOICE-ID
               WHEN 3 PERFORM SEARCH-BY-BOOKING-ID
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
                       PERFORM DISPLAY-INVOICE-RECORD
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
               DISPLAY "INVOICES FOR BOOKING ID: "
               WS-SEARCH-BOOKING
               DISPLAY "========================="
               PERFORM DISPLAY-HEADERS
              MOVE 0 TO INVOICE-ID
              *>START INVOICE-FILE KEY IS NOT LESS THAN INVOICE-ID
              *>    INVALID KEY MOVE 'Y' TO WS-EOF
             *> END-START
              IF WS-EOF NOT = 'Y'
                  READ INVOICE-FILE NEXT RECORD
                   AT END MOVE 'Y' TO WS-EOF
               END-READ

               PERFORM UNTIL WS-EOF = 'Y'
                   IF BOOKING-ID-IV = WS-SEARCH-BOOKING
                       PERFORM DISPLAY-INVOICE-RECORD
                       ADD 1 TO WS-INVOICE-COUNTER
                   END-IF
                   READ INVOICE-FILE NEXT RECORD
                       AT END MOVE 'Y' TO WS-EOF
                   END-READ
               END-PERFORM

               IF WS-INVOICE-COUNTER = 0
                   DISPLAY "No record found for Booking ID "
                   WS-SEARCH-BOOKING
               END-IF
           ELSE
               DISPLAY "Error opening invoice file."
           END-IF
           PERFORM CLOSE-FILES.

       OPEN-FILES.
           OPEN INPUT INVOICE-FILE.

       CLOSE-FILES.
           CLOSE INVOICE-FILE.

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
           PERFORM FORMAT-DATE
           MOVE WS-FORMATTED-DATE TO WS-DL-CREATED-AT
           MOVE ROOM-CHARGE TO WS-DL-ROOM-CHARGE
           MOVE SERVICE-CHARGE TO WS-DL-SERVICE
           IF TAX-RATE NUMERIC
               MOVE TAX-RATE TO WS-DL-TAX-RATE
           ELSE
               MOVE 15 TO WS-DL-TAX-RATE
           END-IF
           MOVE TOTAL-CHARGE TO WS-DL-TOTAL
           DISPLAY WS-DETAIL-LINE.

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

       FORMAT-DATE.
           *> Convert YYYYMMDD to YYYY/MM/DD format
           MOVE CREATED-AT-IV(1:4) TO WS-FORMATTED-DATE(1:4)
           MOVE "/" TO WS-FORMATTED-DATE(5:1)
           MOVE CREATED-AT-IV(5:2) TO WS-FORMATTED-DATE(6:2)
           MOVE "/" TO WS-FORMATTED-DATE(8:1)
           MOVE CREATED-AT-IV(7:2) TO WS-FORMATTED-DATE(9:2).

       END PROGRAM viewInvoices.
