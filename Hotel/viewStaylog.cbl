       IDENTIFICATION DIVISION.
       PROGRAM-ID. viewStaylog.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STAYLOG-FILE ASSIGN TO '../DATA/STAYLOG.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS STAYLOG-ID.
           SELECT CUSTOMER-FILE ASSIGN TO '../DATA/CUSTOMERS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUSTOMER-ID.
           SELECT CHECKINOUT-FILE ASSIGN TO '../DATA/CHECKINOUT.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CHECKIN-ID.

       DATA DIVISION.
       FILE SECTION.
       FD  STAYLOG-FILE.
       COPY "./CopyBooks/STAYLOG.cpy".

       FD  CUSTOMER-FILE.
       COPY "./CopyBooks/CUSTOMERS.cpy".

       FD  CHECKINOUT-FILE.
       COPY "./CopyBooks/CHECKINOUT.cpy".

       WORKING-STORAGE SECTION.
       01  WS-EOF                  PIC X VALUE 'N'.
       01  WS-STAYLOG-COUNTER      PIC 999 VALUE 0.
       01  MENU-CHOICE             PIC 9.
       01  WS-FILE-STATUS          PIC 99.
       01  WS-SEARCH-CUSTOMER-ID   PIC 9(5).
       01  WS-SEARCH-CHECKIN-ID    PIC 9(5).
       01  WS-SEARCH-ROOM-ID       PIC X(5).
       01  WS-CUSTOMER-NAME-TEMP   PIC X(20).
       01  WS-CHECKIN-DATE-TEMP    PIC 9(8).
       01  WS-CHECKOUT-FLAG-TEMP   PIC X.
       01  WS-CHECKOUT-DATE-TEMP   PIC 9(8).
       01  WS-STAY-STATUS-TEMP     PIC X(10).

       01  WS-HEADER-1.
           05 FILLER               PIC X(8) VALUE 'STAYLOG'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(7) VALUE 'CUST-ID'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(13) VALUE 'CUSTOMER NAME'.
           05 FILLER               PIC X(8) VALUE SPACES.
           05 FILLER               PIC X(7) VALUE 'CK-ID'.
           05 FILLER               PIC X(2) VALUE SPACES.
           05 FILLER               PIC X(5) VALUE 'BK-ID'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(4) VALUE 'ROOM'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(12) VALUE 'CHECKIN DATE'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(6) VALUE 'STATUS'.

       01  WS-HEADER-2.
           05 FILLER               PIC X(7) VALUE '-------'.
           05 FILLER               PIC X(4) VALUE SPACES.
           05 FILLER               PIC X(7) VALUE '-------'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(15) VALUE '---------------'.
           05 FILLER               PIC X(6) VALUE SPACES.
           05 FILLER               PIC X(5) VALUE '-----'.
           05 FILLER               PIC X(4) VALUE SPACES.
           05 FILLER               PIC X(5) VALUE '-----'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(5) VALUE '-----'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(11) VALUE '-----------'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(6) VALUE '------'.

       01  WS-DETAIL-LINE.
           05 WS-DL-STAYLOG-ID     PIC Z(5)9.
           05 FILLER               PIC X(5) VALUE SPACES.
           05 WS-DL-CUSTOMER-ID    PIC Z(5)9.
           05 FILLER               PIC X(4) VALUE SPACES.
           05 WS-DL-CUSTOMER-NAME  PIC X(20).
           05 WS-DL-CHECKIN-ID     PIC Z(5)9.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 WS-DL-BOOKING-ID     PIC Z(5)9.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 WS-DL-ROOM-ID        PIC X(5).
           05 FILLER               PIC X(3) VALUE SPACES.
           05 WS-DL-CHECKIN-DATE   PIC X(11).
           05 FILLER               PIC X(3) VALUE SPACES.
           05 WS-DL-STATUS         PIC X(6).

       LINKAGE SECTION.
       01 LINK PIC 9.

       PROCEDURE DIVISION USING LINK.

       MAIN-LOOP.
           PERFORM UNTIL MENU-CHOICE = 9
           DISPLAY
           "***********************************************************"
           DISPLAY "View Stay Log"
           DISPLAY "1. View All Stay Logs"
           DISPLAY "2. Search by Customer ID"
           DISPLAY "3. Search by Check-in ID"
           DISPLAY "4. Search by Room ID"
           DISPLAY "5. View Active Stays"
           DISPLAY "6. View Completed Stays"
           DISPLAY "9. Go Back"
           DISPLAY
           "***********************************************************"
           ACCEPT MENU-CHOICE
           EVALUATE MENU-CHOICE
               WHEN 1 PERFORM ALL-STAYLOGS-DSP
               WHEN 2 PERFORM SEARCH-BY-CUSTOMER-ID
               WHEN 3 PERFORM SEARCH-BY-CHECKIN-ID
               WHEN 4 PERFORM SEARCH-BY-ROOM-ID
               WHEN 5 PERFORM ACTIVE-STAYS-DSP
               WHEN 6 PERFORM COMPLETED-STAYS-DSP
               WHEN 9 GOBACK
               WHEN OTHER DISPLAY "Invalid choice"
           END-EVALUATE
           END-PERFORM.
           GOBACK.

       ALL-STAYLOGS-DSP.
           MOVE 0 TO WS-STAYLOG-COUNTER
           MOVE 'N' TO WS-EOF
           PERFORM OPEN-FILES
           IF WS-EOF = 'N'
               PERFORM DISPLAY-HEADERS
               PERFORM READ-AND-DISPLAY-ALL UNTIL WS-EOF = 'Y'
               PERFORM DISPLAY-SUMMARY
           END-IF
           PERFORM CLOSE-FILES.

       SEARCH-BY-CUSTOMER-ID.
           DISPLAY "Enter Customer ID to search: "
           ACCEPT WS-SEARCH-CUSTOMER-ID
           MOVE 0 TO WS-STAYLOG-COUNTER
           MOVE 'N' TO WS-EOF
           PERFORM OPEN-FILES
           IF WS-EOF = 'N'
               PERFORM DISPLAY-HEADERS
               PERFORM READ-AND-DISPLAY-BY-CUSTOMER UNTIL WS-EOF = 'Y'
               PERFORM DISPLAY-SUMMARY
           END-IF
           PERFORM CLOSE-FILES.

       SEARCH-BY-CHECKIN-ID.
           DISPLAY "Enter Check-in ID to search: "
           ACCEPT WS-SEARCH-CHECKIN-ID
           MOVE 0 TO WS-STAYLOG-COUNTER
           MOVE 'N' TO WS-EOF
           PERFORM OPEN-FILES
           IF WS-EOF = 'N'
               PERFORM DISPLAY-HEADERS
               PERFORM READ-AND-DISPLAY-BY-CHECKIN UNTIL WS-EOF = 'Y'
               PERFORM DISPLAY-SUMMARY
           END-IF
           PERFORM CLOSE-FILES.

       SEARCH-BY-ROOM-ID.
           DISPLAY "Enter Room ID to search: "
           ACCEPT WS-SEARCH-ROOM-ID
           MOVE 0 TO WS-STAYLOG-COUNTER
           MOVE 'N' TO WS-EOF
           PERFORM OPEN-FILES
           IF WS-EOF = 'N'
               PERFORM DISPLAY-HEADERS
               PERFORM READ-AND-DISPLAY-BY-ROOM UNTIL WS-EOF = 'Y'
               PERFORM DISPLAY-SUMMARY
           END-IF
           PERFORM CLOSE-FILES.

       ACTIVE-STAYS-DSP.
           MOVE 0 TO WS-STAYLOG-COUNTER
           MOVE 'N' TO WS-EOF
           PERFORM OPEN-FILES
           IF WS-EOF = 'N'
               PERFORM DISPLAY-HEADERS
               PERFORM READ-AND-DISPLAY-ACTIVE UNTIL WS-EOF = 'Y'
               PERFORM DISPLAY-SUMMARY
           END-IF
           PERFORM CLOSE-FILES.

       COMPLETED-STAYS-DSP.
           MOVE 0 TO WS-STAYLOG-COUNTER
           MOVE 'N' TO WS-EOF
           PERFORM OPEN-FILES
           IF WS-EOF = 'N'
               PERFORM DISPLAY-HEADERS
               PERFORM READ-AND-DISPLAY-COMPLETED UNTIL WS-EOF = 'Y'
               PERFORM DISPLAY-SUMMARY
           END-IF
           PERFORM CLOSE-FILES.

       DISPLAY-HEADERS.
           DISPLAY WS-HEADER-1
           DISPLAY WS-HEADER-2.

       READ-AND-DISPLAY-ALL.
           READ STAYLOG-FILE NEXT RECORD
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   PERFORM DISPLAY-STAYLOG-RECORD
                   ADD 1 TO WS-STAYLOG-COUNTER
           END-READ.

       READ-AND-DISPLAY-BY-CUSTOMER.
           READ STAYLOG-FILE NEXT RECORD
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   IF CUSTOMER-ID-SL = WS-SEARCH-CUSTOMER-ID
                       PERFORM DISPLAY-STAYLOG-RECORD
                       ADD 1 TO WS-STAYLOG-COUNTER
                   END-IF
           END-READ.

       READ-AND-DISPLAY-BY-CHECKIN.
           READ STAYLOG-FILE NEXT RECORD
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   IF CHECKIN-ID-SL = WS-SEARCH-CHECKIN-ID
                       PERFORM DISPLAY-STAYLOG-RECORD
                       ADD 1 TO WS-STAYLOG-COUNTER
                   END-IF
           END-READ.

       READ-AND-DISPLAY-BY-ROOM.
           READ STAYLOG-FILE NEXT RECORD
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   IF FUNCTION TRIM(ROOM-ID-SL) =
                      FUNCTION TRIM(WS-SEARCH-ROOM-ID)
                       PERFORM DISPLAY-STAYLOG-RECORD
                       ADD 1 TO WS-STAYLOG-COUNTER
                   END-IF
           END-READ.

       READ-AND-DISPLAY-ACTIVE.
           READ STAYLOG-FILE NEXT RECORD
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   *> Get checkout info to determine status
                   PERFORM GET-CHECKIN-INFO
                   IF FUNCTION TRIM(WS-STAY-STATUS-TEMP) = 'Active'
                       PERFORM DISPLAY-STAYLOG-RECORD
                       ADD 1 TO WS-STAYLOG-COUNTER
                   END-IF
           END-READ.

       READ-AND-DISPLAY-COMPLETED.
           READ STAYLOG-FILE NEXT RECORD
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   *> Get checkout info to determine status
                   PERFORM GET-CHECKIN-INFO
                   IF FUNCTION TRIM(WS-STAY-STATUS-TEMP) = 'Completed'
                       PERFORM DISPLAY-STAYLOG-RECORD
                       ADD 1 TO WS-STAYLOG-COUNTER
                   END-IF
           END-READ.

       DISPLAY-STAYLOG-RECORD.
           *> Get customer name
           PERFORM GET-CUSTOMER-NAME
           *> Get check-in date and determine status
           PERFORM GET-CHECKIN-INFO

           MOVE STAYLOG-ID TO WS-DL-STAYLOG-ID
           MOVE CUSTOMER-ID-SL TO WS-DL-CUSTOMER-ID
           MOVE WS-CUSTOMER-NAME-TEMP TO WS-DL-CUSTOMER-NAME
           MOVE CHECKIN-ID-SL TO WS-DL-CHECKIN-ID
           MOVE BOOKING-ID-SL TO WS-DL-BOOKING-ID
           MOVE ROOM-ID-SL TO WS-DL-ROOM-ID

           *> Format check-in date for display
           IF WS-CHECKIN-DATE-TEMP > 0
               STRING WS-CHECKIN-DATE-TEMP(1:4) DELIMITED BY SIZE
                      "/" DELIMITED BY SIZE
                      WS-CHECKIN-DATE-TEMP(5:2) DELIMITED BY SIZE
                      "/" DELIMITED BY SIZE
                      WS-CHECKIN-DATE-TEMP(7:2) DELIMITED BY SIZE
                      INTO WS-DL-CHECKIN-DATE
           ELSE
               MOVE "N/A" TO WS-DL-CHECKIN-DATE
           END-IF

           MOVE WS-STAY-STATUS-TEMP TO WS-DL-STATUS
           DISPLAY WS-DETAIL-LINE.

       GET-CUSTOMER-NAME.
           MOVE SPACES TO WS-CUSTOMER-NAME-TEMP
           OPEN INPUT CUSTOMER-FILE
           MOVE CUSTOMER-ID-SL TO CUSTOMER-ID
           READ CUSTOMER-FILE KEY IS CUSTOMER-ID
               INVALID KEY
                   MOVE "Unknown" TO WS-CUSTOMER-NAME-TEMP
               NOT INVALID KEY
                   MOVE CUSTOMER-NAME TO WS-CUSTOMER-NAME-TEMP
           END-READ
           CLOSE CUSTOMER-FILE.

       GET-CHECKIN-INFO.
           MOVE 0 TO WS-CHECKIN-DATE-TEMP
           MOVE 0 TO WS-CHECKOUT-DATE-TEMP
           MOVE 'N' TO WS-CHECKOUT-FLAG-TEMP
           MOVE 'Unknown' TO WS-STAY-STATUS-TEMP

           OPEN INPUT CHECKINOUT-FILE
           MOVE CHECKIN-ID-SL TO CHECKIN-ID
           READ CHECKINOUT-FILE KEY IS CHECKIN-ID
               INVALID KEY
                   MOVE 0 TO WS-CHECKIN-DATE-TEMP
                   MOVE 'Unknown' TO WS-STAY-STATUS-TEMP
               NOT INVALID KEY
                   MOVE ACTUAL-CHECKIN-DATE TO WS-CHECKIN-DATE-TEMP
                   MOVE CHECKOUT-FLAG TO WS-CHECKOUT-FLAG-TEMP
                   MOVE CHECKOUT-DATE TO WS-CHECKOUT-DATE-TEMP

                   *> Determine status based on checkout information
                   IF WS-CHECKOUT-FLAG-TEMP = 'Y'
                      OR WS-CHECKOUT-DATE-TEMP > 0
                       MOVE 'Completed' TO WS-STAY-STATUS-TEMP
                   ELSE
                       MOVE 'Active' TO WS-STAY-STATUS-TEMP
                   END-IF
           END-READ
           CLOSE CHECKINOUT-FILE.

       DISPLAY-SUMMARY.
           DISPLAY SPACES
           IF WS-STAYLOG-COUNTER = 0
               DISPLAY "No stay log records found."
           ELSE
               DISPLAY "Total Stay Log Records: " WS-STAYLOG-COUNTER
           END-IF.

       OPEN-FILES.
           OPEN INPUT STAYLOG-FILE
           IF WS-FILE-STATUS NOT = '00' AND WS-FILE-STATUS NOT = '97'
               DISPLAY "Error opening STAYLOG file: " WS-FILE-STATUS
               IF WS-FILE-STATUS = '35'
                   DISPLAY
                   "Stay log file doesn't exist or no records found."
               END-IF
               MOVE 'Y' TO WS-EOF
           END-IF.

       CLOSE-FILES.
           CLOSE STAYLOG-FILE.

       END PROGRAM viewStaylog.
