       IDENTIFICATION DIVISION.
       PROGRAM-ID. viewCustomers.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO '../DATA/CUSTOMER.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUSTOMER-ID
               ALTERNATE RECORD KEY IS CUSTOMER-NAME WITH DUPLICATES
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE.
       01  CUSTOMER-RECORD.
           05 CUSTOMER-ID     PIC 9(5).
           05 CUSTOMER-NAME   PIC X(30).
           05 CUSTOMER-PHONE  PIC X(15).
           05 CUSTOMER-EMAIL  PIC X(30).
           05 CUSTOMER-ADDR   PIC X(50).

       WORKING-STORAGE SECTION.
       01  WS-EOF                  PIC X VALUE 'N'.
       01  WS-CUSTOMER-COUNTER     PIC 999 VALUE 0.
       01  MENU-CHOICE             PIC 9.
       01  WS-FILE-STATUS          PIC 99.

       01  WS-HEADER-1.
           05 FILLER               PIC X(11) VALUE 'CUSTOMER ID'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(4) VALUE 'NAME'.
           05 FILLER               PIC X(29) VALUE SPACES.
           05 FILLER               PIC X(5) VALUE 'PHONE'.
           05 FILLER               PIC X(14) VALUE SPACES.
           05 FILLER               PIC X(5) VALUE 'EMAIL'.
           05 FILLER               PIC X(29) VALUE SPACES.
           05 FILLER               PIC X(7) VALUE 'ADDRESS'.

       01  WS-HEADER-2.
           05 FILLER               PIC X(10) VALUE '----------'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(30)
           VALUE '------------------------------'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(15) VALUE '---------------'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(30)
           VALUE '------------------------------'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(50)
           VALUE '--------------------------------------------------'.

       01  WS-DETAIL-LINE.
           05 WS-DL-CUSTOMER-ID    PIC Z(4)9.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 WS-DL-NAME           PIC X(30).
           05 FILLER               PIC X(3) VALUE SPACES.
           05 WS-DL-PHONE          PIC X(15).
           05 FILLER               PIC X(3) VALUE SPACES.
           05 WS-DL-EMAIL          PIC X(30).
           05 FILLER               PIC X(3) VALUE SPACES.
           05 WS-DL-ADDRESS        PIC X(50).

       LINKAGE SECTION.
       01 LINK PIC 9.

       PROCEDURE DIVISION USING LINK.

       MAIN-LOOP.
           PERFORM UNTIL MENU-CHOICE = 9
           DISPLAY
           "***********************************************************"
           DISPLAY "View Hotel Customers"
           DISPLAY "1. View All Customers"
           DISPLAY "2. Search Customer By ID"
           DISPLAY "3. Search Customer By Name"
           DISPLAY "9. Go Back"
           DISPLAY
           "***********************************************************"
           ACCEPT MENU-CHOICE
           EVALUATE MENU-CHOICE
               WHEN 1 PERFORM ALL-CUSTOMERS-DSP
               WHEN 2 PERFORM SEARCH-BY-ID
               WHEN 3 PERFORM SEARCH-BY-NAME
               WHEN 9 GOBACK
               WHEN OTHER DISPLAY "Invalid choice"
           END-EVALUATE
           END-PERFORM.
           GOBACK.

       ALL-CUSTOMERS-DSP.
           MOVE 0 TO WS-CUSTOMER-COUNTER
           MOVE 'N' TO WS-EOF
           OPEN INPUT CUSTOMER-FILE
           IF WS-FILE-STATUS NOT = '00' AND WS-FILE-STATUS NOT = '97'
               DISPLAY "Error opening CUSTOMER file: " WS-FILE-STATUS
               IF WS-FILE-STATUS = '35'
                   DISPLAY
                   "Customer file doesn't exist or no records found."
               END-IF
               MOVE 'Y' TO WS-EOF
           ELSE
               PERFORM DISPLAY-HEADERS
               PERFORM READ-AND-DISPLAY-ALL UNTIL WS-EOF = 'Y'
               PERFORM DISPLAY-SUMMARY
           END-IF
           CLOSE CUSTOMER-FILE.

       SEARCH-BY-ID.
           DISPLAY "Enter Customer ID to search: "
           ACCEPT CUSTOMER-ID
           MOVE 'N' TO WS-EOF
           OPEN INPUT CUSTOMER-FILE
           IF WS-FILE-STATUS NOT = '00' AND WS-FILE-STATUS NOT = '97'
               DISPLAY "Error opening CUSTOMER file: " WS-FILE-STATUS
               IF WS-FILE-STATUS = '35'
                   DISPLAY
                   "Customer file doesn't exist or no records found."
               END-IF
               MOVE 'Y' TO WS-EOF
           ELSE
               PERFORM DISPLAY-HEADERS
               READ CUSTOMER-FILE KEY IS CUSTOMER-ID
                   INVALID KEY
                       DISPLAY "Customer ID " CUSTOMER-ID " not found."
                   NOT INVALID KEY
                       PERFORM DISPLAY-CUSTOMER-RECORD
                       ADD 1 TO WS-CUSTOMER-COUNTER
               END-READ
               PERFORM DISPLAY-SUMMARY
           END-IF
           CLOSE CUSTOMER-FILE.

       SEARCH-BY-NAME.
           DISPLAY "Enter Customer Name to search: "
           ACCEPT CUSTOMER-NAME
           MOVE 'N' TO WS-EOF
           MOVE 0 TO WS-CUSTOMER-COUNTER
           OPEN INPUT CUSTOMER-FILE
           IF WS-FILE-STATUS NOT = '00' AND WS-FILE-STATUS NOT = '97'
               DISPLAY "Error opening CUSTOMER file: " WS-FILE-STATUS
               IF WS-FILE-STATUS = '35'
                   DISPLAY
                   "Customer file doesn't exist or no records found."
               END-IF
               MOVE 'Y' TO WS-EOF
           ELSE
               PERFORM DISPLAY-HEADERS
               START CUSTOMER-FILE KEY = CUSTOMER-NAME
                   INVALID KEY
                       DISPLAY
                       "No customers found with name " CUSTOMER-NAME
                       MOVE 'Y' TO WS-EOF
               END-START
               PERFORM UNTIL WS-EOF = 'Y'
                   READ CUSTOMER-FILE NEXT RECORD
                       AT END
                           MOVE 'Y' TO WS-EOF
                       NOT AT END
                           IF CUSTOMER-NAME(1:10) = CUSTOMER-NAME(1:10)
                               PERFORM DISPLAY-CUSTOMER-RECORD
                               ADD 1 TO WS-CUSTOMER-COUNTER
                           ELSE
                               MOVE 'Y' TO WS-EOF
                           END-IF
                   END-READ
               END-PERFORM
               PERFORM DISPLAY-SUMMARY
           END-IF
           CLOSE CUSTOMER-FILE.

       DISPLAY-HEADERS.
           DISPLAY WS-HEADER-1
           DISPLAY WS-HEADER-2.

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
           MOVE CUSTOMER-EMAIL TO WS-DL-EMAIL
           MOVE CUSTOMER-ADDR TO WS-DL-ADDRESS
           DISPLAY WS-DETAIL-LINE.

       DISPLAY-SUMMARY.
           DISPLAY SPACES
           IF WS-CUSTOMER-COUNTER = 0
               DISPLAY "No customers found."
           ELSE
               DISPLAY "Total Customers: " WS-CUSTOMER-COUNTER
           END-IF.

       END PROGRAM viewCustomers.
