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
       01  WS-INVOICE-COUNT-DISPLAY   PIC ZZZ.
       01  WS-TOTAL-AMOUNT            PIC 9(9) VALUE 0.
       01  MENU-CHOICE                PIC 9.
       01  WS-INVOICE-FILE-STATUS     PIC 99.
       01  WS-SEARCH-INVOICE          PIC 9(5).
       01  WS-SEARCH-CHECKIN          PIC 9(5).
       01  WS-PRICE-DISPLAY           PIC $$,$$$,$$9.
       01  WS-FORMATTED-DATE          PIC X(10).

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

       01  WS-HEADER-1.
           05 FILLER               PIC X(7) VALUE 'INVOICE'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(7) VALUE 'CHECKIN'.
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
           05 WS-DL-CHECKIN-ID     PIC Z(5)9.
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
           DISPLAY CLEAR-SCREEN
           DISPLAY BLUE-COLOR
           DISPLAY "==================================================="
           "============================"
           DISPLAY "                         VIEW HOTEL INVOICES SYSTE"
           "M                           "
           DISPLAY "==================================================="
           "============================"
           RESET-COLOR
           DISPLAY "                                                   "
           DISPLAY "                        1. View All Invoices      "
           "                        "
           DISPLAY "                        2. Search Invoice by Invoi"
           "ce ID                   "
           DISPLAY "                        3. Search Invoice by Check"
           "in ID                   "
           DISPLAY "                                                   "
           DISPLAY "==================================================="
           "============================"
           DISPLAY "                        9. Go Back to Main Menu    "
           "                     "
           DISPLAY "==================================================="
           "============================"
           ACCEPT MENU-CHOICE
           EVALUATE MENU-CHOICE
               WHEN 1 PERFORM ALL-INVOICES-DSP
               WHEN 2 PERFORM SEARCH-BY-INVOICE-ID
               WHEN 3 PERFORM SEARCH-BY-CHECKIN-ID
               WHEN 9 GOBACK
               WHEN OTHER
                   DISPLAY " "
                   DISPLAY RED-COLOR "*** ERROR: Invalid selection. P"
                   "lease choose 1-3 or 9. ***" RESET-COLOR
                   DISPLAY " "
                   DISPLAY "Press ENTER to continue..."
                   ACCEPT WS-DUMMY-INPUT
           END-EVALUATE
           END-PERFORM.
           GOBACK.

       ALL-INVOICES-DSP.
           MOVE 0 TO WS-INVOICE-COUNTER
           MOVE 0 TO WS-TOTAL-AMOUNT
           MOVE 'N' TO WS-EOF
           DISPLAY CLEAR-SCREEN
           DISPLAY CYAN-COLOR
           DISPLAY "==============================================="
           "================================"
           DISPLAY "                         ALL INVOICES REPORT  "
           "                               "
           DISPLAY "==============================================="
           "================================"
           RESET-COLOR
           DISPLAY " "

           PERFORM OPEN-FILES
           IF WS-INVOICE-FILE-STATUS = '00'
               OR WS-INVOICE-FILE-STATUS = '97'
               PERFORM DISPLAY-HEADERS
               PERFORM READ-AND-DISPLAY-ALL UNTIL WS-EOF = 'Y'
               PERFORM DISPLAY-SUMMARY
           ELSE
               DISPLAY RED-COLOR
               "Error opening invoice file or no invoices found."
               RESET-COLOR
               DISPLAY " "
               DISPLAY "Press ENTER to continue..."
               ACCEPT WS-DUMMY-INPUT
           END-IF
           PERFORM CLOSE-FILES.

       SEARCH-BY-INVOICE-ID.
           DISPLAY CLEAR-SCREEN
           DISPLAY YELLOW-COLOR
           DISPLAY "==============================================="
           "================================"
           DISPLAY "                       SEARCH BY INVOICE ID   "
           "                               "
           DISPLAY "==============================================="
           "================================"
           RESET-COLOR
           DISPLAY " "
           DISPLAY "Enter Invoice ID: "
           ACCEPT WS-SEARCH-INVOICE

           MOVE 0 TO WS-INVOICE-COUNTER
           MOVE 'N' TO WS-EOF
           DISPLAY " "
           PERFORM OPEN-FILES
           IF WS-INVOICE-FILE-STATUS = '00'
               OR WS-INVOICE-FILE-STATUS = '97'
               MOVE WS-SEARCH-INVOICE TO INVOICE-ID
               READ INVOICE-FILE KEY IS INVOICE-ID
                   INVALID KEY
           DISPLAY "==============================================="
           "================================"
                       DISPLAY RED-COLOR "Invoice ID "
                       WS-SEARCH-INVOICE " not found." RESET-COLOR
           DISPLAY "==============================================="
           "================================"
                       DISPLAY " "
                       DISPLAY "Press ENTER to continue..."
                       ACCEPT WS-DUMMY-INPUT
                   NOT INVALID KEY
                       PERFORM DISPLAY-HEADERS
                       PERFORM DISPLAY-INVOICE-RECORD
                       ADD 1 TO WS-INVOICE-COUNTER
                       PERFORM DISPLAY-SUMMARY-NO-AMOUNT
               END-READ
           ELSE
               DISPLAY RED-COLOR "Error opening invoice file."
               RESET-COLOR
               DISPLAY " "
               DISPLAY "Press ENTER to continue..."
               ACCEPT WS-DUMMY-INPUT
           END-IF
           PERFORM CLOSE-FILES.

       SEARCH-BY-CHECKIN-ID.
           DISPLAY CLEAR-SCREEN
           DISPLAY YELLOW-COLOR
           DISPLAY "==============================================="
           "================================"
           DISPLAY "                       SEARCH BY CHECKIN ID   "
           "                               "
           DISPLAY "==============================================="
           "================================"
           RESET-COLOR
           DISPLAY " "
           DISPLAY "Enter Checkin ID: "
           ACCEPT WS-SEARCH-CHECKIN

           MOVE 0 TO WS-INVOICE-COUNTER
           MOVE 'N' TO WS-EOF
           DISPLAY " "
           PERFORM OPEN-FILES
           IF WS-INVOICE-FILE-STATUS = '00'
               OR WS-INVOICE-FILE-STATUS = '97'
               DISPLAY CYAN-COLOR "INVOICES FOR CHECKIN ID: "
               WS-SEARCH-CHECKIN RESET-COLOR
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
                   IF CHECKIN-ID-IV = WS-SEARCH-CHECKIN
                       PERFORM DISPLAY-INVOICE-RECORD
                       ADD 1 TO WS-INVOICE-COUNTER
                   END-IF
                   READ INVOICE-FILE NEXT RECORD
                       AT END MOVE 'Y' TO WS-EOF
                   END-READ
               END-PERFORM

               IF WS-INVOICE-COUNTER = 0
                   DISPLAY " "
                   DISPLAY RED-COLOR "No record found for Checkin ID "
                   WS-SEARCH-CHECKIN RESET-COLOR
                   DISPLAY " "
                   DISPLAY "Press ENTER to continue..."
                   ACCEPT WS-DUMMY-INPUT
               ELSE
                   PERFORM DISPLAY-SUMMARY-NO-AMOUNT
               END-IF
           ELSE
               DISPLAY RED-COLOR "Error opening invoice file."
               RESET-COLOR
               DISPLAY " "
               DISPLAY "Press ENTER to continue..."
               ACCEPT WS-DUMMY-INPUT
           END-IF
           PERFORM CLOSE-FILES.

       OPEN-FILES.
           OPEN INPUT INVOICE-FILE.

       CLOSE-FILES.
           CLOSE INVOICE-FILE.

       DISPLAY-HEADERS.
           DISPLAY YELLOW-COLOR
           DISPLAY WS-HEADER-1
           DISPLAY WS-HEADER-2
           RESET-COLOR.

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
           MOVE CHECKIN-ID-IV TO WS-DL-CHECKIN-ID
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
           DISPLAY "==============================================="
           "================================"
           IF WS-INVOICE-COUNTER = 0
               DISPLAY RED-COLOR "No invoices found." RESET-COLOR
           ELSE
               MOVE WS-INVOICE-COUNTER TO WS-INVOICE-COUNT-DISPLAY
               DISPLAY GREEN-COLOR "Total Invoices Found: "
               WS-INVOICE-COUNT-DISPLAY RESET-COLOR
               MOVE WS-TOTAL-AMOUNT TO WS-PRICE-DISPLAY
               DISPLAY GREEN-COLOR "Total Amount: " WS-PRICE-DISPLAY
               RESET-COLOR
           END-IF
           DISPLAY "==============================================="
           "================================"
           DISPLAY " "
           DISPLAY "Press ENTER to continue..."
           ACCEPT WS-DUMMY-INPUT.

       DISPLAY-SUMMARY-NO-AMOUNT.
           DISPLAY " "
           DISPLAY "==============================================="
           "================================"
           IF WS-INVOICE-COUNTER = 0
               DISPLAY RED-COLOR "No invoices found." RESET-COLOR
           ELSE
               MOVE WS-INVOICE-COUNTER TO WS-INVOICE-COUNT-DISPLAY
               DISPLAY GREEN-COLOR "Total Invoices Found: "
               WS-INVOICE-COUNT-DISPLAY RESET-COLOR
           END-IF
           DISPLAY "==============================================="
           "================================"
           DISPLAY " "
           DISPLAY "Press ENTER to continue..."
           ACCEPT WS-DUMMY-INPUT.

       FORMAT-DATE.
           *> Convert YYYYMMDD to YYYY/MM/DD format
           MOVE CREATED-AT-IV(1:4) TO WS-FORMATTED-DATE(1:4)
           MOVE "/" TO WS-FORMATTED-DATE(5:1)
           MOVE CREATED-AT-IV(5:2) TO WS-FORMATTED-DATE(6:2)
           MOVE "/" TO WS-FORMATTED-DATE(8:1)
           MOVE CREATED-AT-IV(7:2) TO WS-FORMATTED-DATE(9:2).

       END PROGRAM viewInvoices.
