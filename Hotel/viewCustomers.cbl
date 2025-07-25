       IDENTIFICATION DIVISION.
       PROGRAM-ID. viewCustomers.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO '../DATA/CUSTOMERS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUSTOMER-ID
               ALTERNATE RECORD KEY IS CUSTOMER-NAME WITH DUPLICATES
               FILE STATUS IS WS-FILE-STATUS.
           SELECT BOOKING-FILE ASSIGN TO '../DATA/BOOKINGS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS BOOKING-ID
               FILE STATUS IS WS-BOOKING-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE.
       COPY "./CopyBooks/CUSTOMERS.cpy".

       FD  BOOKING-FILE.
       COPY "./CopyBooks/BOOKINGS.cpy".

       WORKING-STORAGE SECTION.
       01  WS-EOF                  PIC X VALUE 'N'.
       01  WS-BOOKING-EOF          PIC X VALUE 'N'.
       01  WS-CUSTOMER-COUNTER     PIC 999 VALUE 0.
       01  WS-CUSTOMER-COUNT-DISPLAY PIC ZZZ.
       01  MENU-CHOICE             PIC 9.
       01  WS-FILE-STATUS          PIC 99.
       01  WS-BOOKING-FILE-STATUS  PIC 99.
       01  WS-SEARCH-NAME          PIC X(20).
       01  WS-SEARCH-NAME-UPPER    PIC X(20).
       01  WS-CUSTOMER-NAME-UPPER  PIC X(20).
       01  WS-BOOKING-COUNT        PIC 999 VALUE 0.

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
           05 FILLER               PIC X(11) VALUE 'CUSTOMER ID'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(4) VALUE 'NAME'.
           05 FILLER               PIC X(19) VALUE SPACES.
           05 FILLER               PIC X(5) VALUE 'PHONE'.
           05 FILLER               PIC X(14) VALUE SPACES.
           05 FILLER               PIC X(3) VALUE 'AGE'.
           05 FILLER               PIC X(5) VALUE SPACES.
           05 FILLER               PIC X(6) VALUE 'GENDER'.
           05 FILLER               PIC X(5) VALUE SPACES.
           05 FILLER               PIC X(10) VALUE 'NRC NUMBER'.

       01  WS-HEADER-2.
           05 FILLER               PIC X(11) VALUE '-----------'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(20)
           VALUE '--------------------'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(15) VALUE '---------------'.
           05 FILLER               PIC X(4) VALUE SPACES.
           05 FILLER               PIC X(3) VALUE '---'.
           05 FILLER               PIC X(5) VALUE SPACES.
           05 FILLER               PIC X(6) VALUE '------'.
           05 FILLER               PIC X(5) VALUE SPACES.
           05 FILLER               PIC X(25)
           VALUE '-------------------------'.

       01  WS-DETAIL-LINE.
           05 WS-DL-CUSTOMER-ID    PIC Z(5)9.
           05 FILLER               PIC X(8) VALUE SPACES.
           05 WS-DL-NAME           PIC X(20).
           05 FILLER               PIC X(3) VALUE SPACES.
           05 WS-DL-PHONE          PIC X(15).
           05 FILLER               PIC X(4) VALUE SPACES.
           05 WS-DL-AGE            PIC ZZ9.
           05 FILLER               PIC X(8) VALUE SPACES.
           05 WS-DL-GENDER         PIC X(1).
           05 FILLER               PIC X(7) VALUE SPACES.
           05 WS-DL-NRC-NUMBER     PIC X(25).

       LINKAGE SECTION.
       01 LINK PIC 9.

       PROCEDURE DIVISION USING LINK.

       MAIN-LOOP.
           PERFORM UNTIL MENU-CHOICE = 9
           DISPLAY CLEAR-SCREEN
           DISPLAY BLUE-COLOR
           DISPLAY "==================================================="
           "============================"
           DISPLAY "                         VIEW HOTEL CUSTOMERS SYST"
           "EM                           "
           DISPLAY "==================================================="
           "============================"
           RESET-COLOR
           DISPLAY "                                                   "
           DISPLAY "                        1. View All Customers     "
           "                        "
           DISPLAY "                        2. Search Customer By ID  "
           "                        "
           DISPLAY "                        3. Search Customer By Name"
           "                        "
           DISPLAY "                                                   "
           DISPLAY "==================================================="
           "============================"
           DISPLAY "                        9. Go Back to Main Menu    "
           "                     "
           DISPLAY "==================================================="
           "============================"
           ACCEPT MENU-CHOICE
           EVALUATE MENU-CHOICE
               WHEN 1 PERFORM ALL-CUSTOMERS-DSP
               WHEN 2 PERFORM SEARCH-BY-ID
               WHEN 3 PERFORM SEARCH-BY-NAME
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

       ALL-CUSTOMERS-DSP.
           MOVE 0 TO WS-CUSTOMER-COUNTER
           MOVE 'N' TO WS-EOF
           DISPLAY CLEAR-SCREEN
           DISPLAY CYAN-COLOR
           DISPLAY "==============================================="
           "================================"
           "====================="
           DISPLAY "                         ALL CUSTOMERS REPORT "
           "                               "
           DISPLAY "==============================================="
           "================================"
           "====================="
           RESET-COLOR
           DISPLAY " "

           PERFORM OPEN-FILES
           IF WS-EOF = 'N'
               PERFORM DISPLAY-HEADERS
               PERFORM READ-AND-DISPLAY-ALL UNTIL WS-EOF = 'Y'
               PERFORM DISPLAY-SUMMARY
           END-IF
           PERFORM CLOSE-FILES.

       SEARCH-BY-ID.
           DISPLAY CLEAR-SCREEN
           DISPLAY YELLOW-COLOR
           DISPLAY "==============================================="
           "================================"
           "====================="
           DISPLAY "                       SEARCH CUSTOMER BY ID  "
           "                               "
           DISPLAY "==============================================="
           "================================"
           "====================="
           RESET-COLOR
           DISPLAY " "
           DISPLAY "Enter Customer ID to search: "
           ACCEPT CUSTOMER-ID
           MOVE 'N' TO WS-EOF
           DISPLAY " "
           PERFORM OPEN-FILES
           IF WS-EOF = 'N'
               PERFORM DISPLAY-HEADERS
               READ CUSTOMER-FILE KEY IS CUSTOMER-ID
                   INVALID KEY
                       DISPLAY " "
                       DISPLAY RED-COLOR "Customer ID " CUSTOMER-ID
                       " not found." RESET-COLOR
                       DISPLAY " "
                   NOT INVALID KEY
                       PERFORM DISPLAY-CUSTOMER-RECORD
                       ADD 1 TO WS-CUSTOMER-COUNTER
               END-READ
               PERFORM DISPLAY-SUMMARY
           END-IF
           PERFORM CLOSE-FILES.

       SEARCH-BY-NAME.
           DISPLAY CLEAR-SCREEN
           DISPLAY YELLOW-COLOR
           DISPLAY "==============================================="
           "================================"
           "====================="
           DISPLAY "                     SEARCH CUSTOMER BY NAME  "
           "                               "
           DISPLAY "==============================================="
           "================================"
           "====================="
           RESET-COLOR
           DISPLAY " "
           DISPLAY "Enter Customer Name to search: "
           ACCEPT WS-SEARCH-NAME
           MOVE FUNCTION UPPER-CASE(WS-SEARCH-NAME)
           TO WS-SEARCH-NAME-UPPER
           MOVE 'N' TO WS-EOF
           MOVE 0 TO WS-CUSTOMER-COUNTER
           DISPLAY " "
           PERFORM OPEN-FILES
           IF WS-EOF = 'N'
               PERFORM DISPLAY-HEADERS
               PERFORM UNTIL WS-EOF = 'Y'
                   READ CUSTOMER-FILE NEXT RECORD
                       AT END
                           MOVE 'Y' TO WS-EOF
                       NOT AT END
                           MOVE FUNCTION UPPER-CASE(CUSTOMER-NAME)
                                TO WS-CUSTOMER-NAME-UPPER
                           IF WS-SEARCH-NAME-UPPER(1:10) =
                              WS-CUSTOMER-NAME-UPPER(1:10)
                               PERFORM DISPLAY-CUSTOMER-RECORD
                               ADD 1 TO WS-CUSTOMER-COUNTER
                           END-IF
                   END-READ
               END-PERFORM
               PERFORM DISPLAY-SUMMARY
           END-IF
           PERFORM CLOSE-FILES.

       DISPLAY-HEADERS.
           DISPLAY YELLOW-COLOR
           DISPLAY WS-HEADER-1
           DISPLAY WS-HEADER-2
           RESET-COLOR.

       READ-AND-DISPLAY-ALL.
           READ CUSTOMER-FILE NEXT RECORD
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   PERFORM DISPLAY-CUSTOMER-RECORD
                   ADD 1 TO WS-CUSTOMER-COUNTER
           END-READ.

       DISPLAY-CUSTOMER-RECORD.
           MOVE CUSTOMER-ID TO WS-DL-CUSTOMER-ID
           MOVE CUSTOMER-NAME TO WS-DL-NAME
           MOVE CUSTOMER-PHONE TO WS-DL-PHONE
           MOVE CUSTOMER-AGE TO WS-DL-AGE
           MOVE CUSTOMER-GENDER TO WS-DL-GENDER
           MOVE NRC-NUMBER TO WS-DL-NRC-NUMBER
           DISPLAY WS-DETAIL-LINE.

       DISPLAY-SUMMARY.
           DISPLAY " "
           DISPLAY "==============================================="
           "================================"
           "====================="
           IF WS-CUSTOMER-COUNTER = 0
               DISPLAY RED-COLOR "No customers found." RESET-COLOR
           ELSE
               MOVE WS-CUSTOMER-COUNTER TO WS-CUSTOMER-COUNT-DISPLAY
               DISPLAY GREEN-COLOR "Total Customers Found: "
               WS-CUSTOMER-COUNT-DISPLAY RESET-COLOR
           END-IF
           DISPLAY "==============================================="
           "================================"
           "====================="
           DISPLAY " "
           DISPLAY "Press ENTER to continue..."
           ACCEPT WS-DUMMY-INPUT.

       OPEN-FILES.
           OPEN INPUT CUSTOMER-FILE BOOKING-FILE
           IF WS-FILE-STATUS NOT = '00' AND WS-FILE-STATUS NOT = '97'
               DISPLAY " "
               DISPLAY RED-COLOR "Error opening CUSTOMER file: "
               WS-FILE-STATUS RESET-COLOR
               IF WS-FILE-STATUS = '35'
                   DISPLAY RED-COLOR
                   "Customer file doesn't exist or no records found."
                   RESET-COLOR
               END-IF
               DISPLAY " "
               DISPLAY "Press ENTER to continue..."
               ACCEPT WS-DUMMY-INPUT
               MOVE 'Y' TO WS-EOF
           ELSE
               IF WS-BOOKING-FILE-STATUS NOT = '00' AND
                  WS-BOOKING-FILE-STATUS NOT = '97'
                   DISPLAY YELLOW-COLOR
                   "Warning: Cannot access booking data" RESET-COLOR
               END-IF
           END-IF.

       CLOSE-FILES.
           CLOSE CUSTOMER-FILE BOOKING-FILE.

       END PROGRAM viewCustomers.
