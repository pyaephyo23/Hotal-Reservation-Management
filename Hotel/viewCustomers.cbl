       IDENTIFICATION DIVISION.
       PROGRAM-ID. viewCustomers.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO './DATA/CUSTOMERS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUSTOMER-ID
               ALTERNATE RECORD KEY IS CUSTOMER-NAME WITH DUPLICATES
               FILE STATUS IS WS-FILE-STATUS.
           SELECT BOOKING-FILE ASSIGN TO './DATA/BOOKINGS.DAT'
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
       01  MENU-CHOICE             PIC 9.
       01  WS-FILE-STATUS          PIC 99.
       01  WS-BOOKING-FILE-STATUS  PIC 99.
       01  WS-SEARCH-NAME          PIC X(20).
       01  WS-SEARCH-NAME-UPPER    PIC X(20).
       01  WS-CUSTOMER-NAME-UPPER  PIC X(20).
       01  WS-SEARCH-PHONE         PIC X(15).
       01  WS-BOOKING-COUNT        PIC 999 VALUE 0.
       01  WS-UPDATE-CHOICE        PIC X.
       01  WS-FOUND-CUSTOMER       PIC X VALUE 'N'.
       01  WS-NEW-NAME             PIC X(20).
       01  WS-NEW-PHONE            PIC X(15).
       01  WS-NEW-AGE              PIC 99.
       01  WS-NEW-GENDER           PIC X.
       01  WS-NEW-NRC              PIC X(25).

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

       PROCEDURE DIVISION.

       MAIN-LOOP.
           PERFORM UNTIL MENU-CHOICE = 9
           DISPLAY
           "***********************************************************"
           DISPLAY "View Hotel Customers"
           DISPLAY "1. View All Customers"
           DISPLAY "2. Search Customer By Phone"
           DISPLAY "3. Search Customer By Name"
           DISPLAY "9. Go Back"
           DISPLAY
           "***********************************************************"
           ACCEPT MENU-CHOICE
           EVALUATE MENU-CHOICE
               WHEN 1 PERFORM ALL-CUSTOMERS-DSP
               WHEN 2 PERFORM SEARCH-BY-PHONE
               WHEN 3 PERFORM SEARCH-BY-NAME
               WHEN 9 GOBACK
               WHEN OTHER DISPLAY "Invalid choice"
           END-EVALUATE
           END-PERFORM.
           GOBACK.

       ALL-CUSTOMERS-DSP.
           MOVE 0 TO WS-CUSTOMER-COUNTER
           MOVE 'N' TO WS-EOF
           PERFORM OPEN-FILES
           IF WS-EOF = 'N'
               PERFORM DISPLAY-HEADERS
               PERFORM READ-AND-DISPLAY-ALL UNTIL WS-EOF = 'Y'
               PERFORM DISPLAY-SUMMARY
           END-IF
           PERFORM CLOSE-FILES.

       SEARCH-BY-PHONE.
           DISPLAY "Enter Customer Phone Number to search: "
           ACCEPT WS-SEARCH-PHONE
           MOVE 'N' TO WS-EOF
           MOVE 'N' TO WS-FOUND-CUSTOMER
           MOVE 0 TO WS-CUSTOMER-COUNTER
           PERFORM OPEN-FILES
           IF WS-EOF = 'N'
               PERFORM DISPLAY-HEADERS
               PERFORM UNTIL WS-EOF = 'Y'
                   READ CUSTOMER-FILE NEXT RECORD
                       AT END
                           MOVE 'Y' TO WS-EOF
                       NOT AT END
                           IF CUSTOMER-PHONE = WS-SEARCH-PHONE
                               PERFORM DISPLAY-CUSTOMER-RECORD
                               ADD 1 TO WS-CUSTOMER-COUNTER
                               MOVE 'Y' TO WS-FOUND-CUSTOMER
                               MOVE 'Y' TO WS-EOF
                           END-IF
                   END-READ
               END-PERFORM
               PERFORM DISPLAY-SUMMARY
               IF WS-FOUND-CUSTOMER = 'Y'
                   PERFORM ASK-UPDATE-CUSTOMER
               END-IF
           END-IF
           PERFORM CLOSE-FILES.

       SEARCH-BY-NAME.
           DISPLAY "Enter Customer Name to search: "
           ACCEPT WS-SEARCH-NAME
           MOVE FUNCTION UPPER-CASE(WS-SEARCH-NAME)
           TO WS-SEARCH-NAME-UPPER
           MOVE 'N' TO WS-EOF
           MOVE 'N' TO WS-FOUND-CUSTOMER
           MOVE 0 TO WS-CUSTOMER-COUNTER
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
                               MOVE 'Y' TO WS-FOUND-CUSTOMER
                           END-IF
                   END-READ
               END-PERFORM
               PERFORM DISPLAY-SUMMARY
               IF WS-FOUND-CUSTOMER = 'Y'
                   PERFORM ASK-UPDATE-CUSTOMER
               END-IF
           END-IF
           PERFORM CLOSE-FILES.

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
           MOVE CUSTOMER-AGE TO WS-DL-AGE
           MOVE CUSTOMER-GENDER TO WS-DL-GENDER
           MOVE NRC-NUMBER TO WS-DL-NRC-NUMBER
           DISPLAY WS-DETAIL-LINE.

       DISPLAY-SUMMARY.
           DISPLAY SPACES
           IF WS-CUSTOMER-COUNTER = 0
               DISPLAY "No customers found."
           ELSE
               DISPLAY "Total Customers: " WS-CUSTOMER-COUNTER
           END-IF.

       OPEN-FILES.
           OPEN INPUT CUSTOMER-FILE BOOKING-FILE
           IF WS-FILE-STATUS NOT = '00' AND WS-FILE-STATUS NOT = '97'
               DISPLAY "Error opening CUSTOMER file: " WS-FILE-STATUS
               IF WS-FILE-STATUS = '35'
                   DISPLAY
                   "Customer file doesn't exist or no records found."
               END-IF
               MOVE 'Y' TO WS-EOF
           ELSE
               IF WS-BOOKING-FILE-STATUS NOT = '00' AND
                  WS-BOOKING-FILE-STATUS NOT = '97'
                   DISPLAY "Warning: Cannot access booking data"
               END-IF
           END-IF.

       CLOSE-FILES.
           CLOSE CUSTOMER-FILE BOOKING-FILE.

       ASK-UPDATE-CUSTOMER.
           DISPLAY " "
           DISPLAY 
           "Do you want to update this customer's information? (Y/N): "
           ACCEPT WS-UPDATE-CHOICE
           IF WS-UPDATE-CHOICE = 'Y' OR WS-UPDATE-CHOICE = 'y'
               PERFORM UPDATE-CUSTOMER-INFO
           END-IF.

       UPDATE-CUSTOMER-INFO.
           DISPLAY " "
           DISPLAY "========== UPDATE CUSTOMER INFORMATION =========="
           DISPLAY "Current Customer ID: " CUSTOMER-ID
           DISPLAY "Current Name: " CUSTOMER-NAME
           DISPLAY "Current Phone: " CUSTOMER-PHONE
           DISPLAY "Current Age: " CUSTOMER-AGE
           DISPLAY "Current Gender: " CUSTOMER-GENDER
           DISPLAY "Current NRC: " NRC-NUMBER
           DISPLAY " "
           
           DISPLAY 
           "Enter new information (press ENTER to keep current):"
           
           DISPLAY "New Name [" FUNCTION TRIM(CUSTOMER-NAME) "]: "
           ACCEPT WS-NEW-NAME
           IF WS-NEW-NAME NOT = SPACES
               MOVE WS-NEW-NAME TO CUSTOMER-NAME
           END-IF
           
           DISPLAY "New Phone [" FUNCTION TRIM(CUSTOMER-PHONE) "]: "
           ACCEPT WS-NEW-PHONE
           IF WS-NEW-PHONE NOT = SPACES
               MOVE WS-NEW-PHONE TO CUSTOMER-PHONE
           END-IF
           
           DISPLAY "New Age [" CUSTOMER-AGE "]: "
           ACCEPT WS-NEW-AGE
           IF WS-NEW-AGE NOT = 0
               MOVE WS-NEW-AGE TO CUSTOMER-AGE
           END-IF
           
           DISPLAY "New Gender [" CUSTOMER-GENDER "]: "
           ACCEPT WS-NEW-GENDER
           IF WS-NEW-GENDER NOT = SPACES
               MOVE WS-NEW-GENDER TO CUSTOMER-GENDER
           END-IF
           
           DISPLAY "New NRC Number [" FUNCTION TRIM(NRC-NUMBER) "]: "
           ACCEPT WS-NEW-NRC
           IF WS-NEW-NRC NOT = SPACES
               MOVE WS-NEW-NRC TO NRC-NUMBER
           END-IF
           
           PERFORM SAVE-CUSTOMER-CHANGES.

       SAVE-CUSTOMER-CHANGES.
           CLOSE CUSTOMER-FILE
           OPEN I-O CUSTOMER-FILE
           IF WS-FILE-STATUS = '00'
               REWRITE CUSTOMER-RECORD
               IF WS-FILE-STATUS = '00'
                   DISPLAY " "
                   DISPLAY "Customer information updated successfully!"
                   DISPLAY "Updated Information:"
                   PERFORM DISPLAY-CUSTOMER-RECORD
               ELSE
                   DISPLAY "Error updating customer: " WS-FILE-STATUS
               END-IF
           ELSE
               DISPLAY "Error opening file for update: " WS-FILE-STATUS
           END-IF
           CLOSE CUSTOMER-FILE.

       END PROGRAM viewCustomers.
