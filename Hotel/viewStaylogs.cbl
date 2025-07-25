       IDENTIFICATION DIVISION.
       PROGRAM-ID. viewStaylogs.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STAYLOG-FILE ASSIGN TO '../DATA/STAYLOG.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS STAYLOG-ID
               FILE STATUS IS WS-STAYLOG-FILE-STATUS.
           SELECT CUSTOMER-FILE ASSIGN TO '../DATA/CUSTOMERS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUSTOMER-ID
               FILE STATUS IS WS-CUSTOMER-FILE-STATUS.
           SELECT CHECKINOUT-FILE ASSIGN TO '../DATA/CHECKINOUT.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CHECKIN-ID
               FILE STATUS IS WS-CHECKIN-FILE-STATUS.
           SELECT ROOMS-FILE ASSIGN TO '../DATA/ROOMS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ROOM-ID
               FILE STATUS IS WS-ROOM-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  STAYLOG-FILE.
       COPY "./CopyBooks/STAYLOG.cpy".

       FD  CUSTOMER-FILE.
       COPY "./CopyBooks/CUSTOMERS.cpy".

       FD  CHECKINOUT-FILE.
       COPY "./CopyBooks/CHECKINOUT.cpy".

       FD  ROOMS-FILE.
       COPY "./CopyBooks/ROOMS.cpy".

       WORKING-STORAGE SECTION.
       01  WS-EOF                      PIC X VALUE 'N'.
       01  WS-STAYLOG-COUNTER          PIC 999 VALUE 0.
       01  WS-STAYLOG-COUNT-DISPLAY    PIC ZZZ.
       01  MENU-CHOICE                 PIC 9.
       01  WS-STAYLOG-FILE-STATUS      PIC 99.
       01  WS-CUSTOMER-FILE-STATUS     PIC 99.
       01  WS-CHECKIN-FILE-STATUS      PIC 99.
       01  WS-ROOM-FILE-STATUS         PIC 99.
       01  WS-SEARCH-CUSTOMER-ID       PIC 9(5).
       01  WS-SEARCH-ROOM-ID           PIC X(5).
       01  WS-SEARCH-CHECKIN-ID        PIC 9(5).
       01  WS-FOUND                    PIC X VALUE 'N'.

       *> Working variables for display
       01  WS-CUSTOMER-NAME-TEMP       PIC X(20).
       01  WS-ROOM-TYPE-TEMP           PIC X(10).
       01  WS-CHECKOUT-STATUS          PIC X(15).
       01  WS-CHECKIN-DATE-TEMP        PIC 9(8).
       01  WS-CHECKIN-TIME-TEMP        PIC 9(6).
       01  WS-CHECKOUT-DATE-TEMP       PIC 9(8).
       01  WS-CHECKOUT-TIME-TEMP       PIC 9(6).

       *> Date/time formatting
       01 WS-TEMP-DATE.
           05 WS-YEAR                  PIC X(4).
           05 WS-MONTH                 PIC X(2).
           05 WS-DAY                   PIC X(2).

       01 WS-TEMP-TIME.
           05 WS-HOUR                  PIC X(2).
           05 WS-MINUTE                PIC X(2).
           05 WS-SECOND                PIC X(2).

       01 WS-FORMATTED-DATE            PIC X(10).
       01 WS-FORMATTED-TIME            PIC X(8).
       01 WS-FORMATTED-CHECKOUT-DATE   PIC X(10).
       01 WS-FORMATTED-CHECKOUT-TIME   PIC X(8).

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
           05 FILLER               PIC X(8) VALUE 'STAY ID'.
           05 FILLER               PIC X(2) VALUE SPACES.
           05 FILLER               PIC X(10) VALUE 'CHECKIN ID'.
           05 FILLER               PIC X(2) VALUE SPACES.
           05 FILLER               PIC X(11) VALUE 'CUSTOMER ID'.
           05 FILLER               PIC X(2) VALUE SPACES.
           05 FILLER               PIC X(20) VALUE 'CUSTOMER NAME'.
           05 FILLER               PIC X(2) VALUE SPACES.
           05 FILLER               PIC X(7) VALUE 'ROOM ID'.
           05 FILLER               PIC X(2) VALUE SPACES.
           05 FILLER               PIC X(9) VALUE 'ROOM TYPE'.
           05 FILLER               PIC X(2) VALUE SPACES.
           05 FILLER               PIC X(10) VALUE 'CHECK-IN'.
           05 FILLER               PIC X(2) VALUE SPACES.
           05 FILLER               PIC X(11) VALUE 'CHECK-OUT'.

       01  WS-HEADER-2.
           05 FILLER               PIC X(8) VALUE '--------'.
           05 FILLER               PIC X(2) VALUE SPACES.
           05 FILLER               PIC X(10) VALUE '----------'.
           05 FILLER               PIC X(2) VALUE SPACES.
           05 FILLER               PIC X(11) VALUE '-----------'.
           05 FILLER               PIC X(2) VALUE SPACES.
           05 FILLER             PIC X(20) VALUE '--------------------'.
           05 FILLER               PIC X(2) VALUE SPACES.
           05 FILLER               PIC X(7) VALUE '-------'.
           05 FILLER               PIC X(2) VALUE SPACES.
           05 FILLER               PIC X(9) VALUE '---------'.
           05 FILLER               PIC X(2) VALUE SPACES.
           05 FILLER               PIC X(10) VALUE '----------'.
           05 FILLER               PIC X(2) VALUE SPACES.
           05 FILLER               PIC X(11) VALUE '-----------'.

       01  WS-DETAIL-LINE.
           05 WS-DL-STAYLOG-ID     PIC Z(5)9.
           05 FILLER               PIC X(4) VALUE SPACES.
           05 WS-DL-CHECKIN-ID     PIC Z(5)9.
           05 FILLER               PIC X(6) VALUE SPACES.
           05 WS-DL-CUSTOMER-ID    PIC Z(5)9.
           05 FILLER               PIC X(7) VALUE SPACES.
           05 WS-DL-CUSTOMER-NAME  PIC X(20).
           05 FILLER               PIC X(2) VALUE SPACES.
           05 WS-DL-ROOM-ID        PIC X(7).
           05 FILLER               PIC X(2) VALUE SPACES.
           05 WS-DL-ROOM-TYPE      PIC X(9).
           05 FILLER               PIC X(2) VALUE SPACES.
           05 WS-DL-CHECKIN        PIC X(10).
           05 FILLER               PIC X(2) VALUE SPACES.
           05 WS-DL-CHECKOUT       PIC X(11).

       LINKAGE SECTION.
       01 LINK PIC 9.

       PROCEDURE DIVISION USING LINK.

       MAIN-LOOP.
           PERFORM UNTIL MENU-CHOICE = 9
           DISPLAY CLEAR-SCREEN
           DISPLAY BLUE-COLOR
           DISPLAY "==================================================="
           "================================"
           DISPLAY "                              STAY LOGS VIEW SYSTE"
           "M                              "
           DISPLAY "==================================================="
           "================================"
           RESET-COLOR
           DISPLAY "                                                   "
           DISPLAY "                         1. View All Stay Logs    "
           "                         "
           DISPLAY "                         2. Search by Customer ID "
           "                         "
           DISPLAY "                         3. Search by Room ID     "
           "                         "
           DISPLAY "                         4. Search by Check-in ID "
           "                         "
           DISPLAY "                                                   "
           DISPLAY "==================================================="
           "================================"
           DISPLAY "                         9. Go Back to Main Menu   "
           "                     "
           DISPLAY "==================================================="
           "================================"
           DISPLAY " "
           ACCEPT MENU-CHOICE

           EVALUATE MENU-CHOICE
               WHEN 1
                   PERFORM VIEW-ALL-STAYLOGS
               WHEN 2
                   PERFORM SEARCH-BY-CUSTOMER-ID
               WHEN 3
                   PERFORM SEARCH-BY-ROOM-ID
               WHEN 4
                   PERFORM SEARCH-BY-CHECKIN-ID
               WHEN 9
                   CONTINUE
               WHEN OTHER
                   DISPLAY " "
                   DISPLAY RED-COLOR "*** ERROR: Invalid selection."
                   " Please choose 1-4 or 9. ***" RESET-COLOR
                   DISPLAY " "
                   DISPLAY "Press ENTER to continue..."
                   ACCEPT WS-DUMMY-INPUT
           END-EVALUATE
           END-PERFORM

           GOBACK.

       VIEW-ALL-STAYLOGS.
           DISPLAY CLEAR-SCREEN
           DISPLAY GREEN-COLOR
           DISPLAY "=============================================="
           "========================================="
           DISPLAY "                              ALL STAY LOGS "
           "                                    "
           DISPLAY "=================================================="
           "====================================="
           RESET-COLOR
           DISPLAY " "

           OPEN INPUT STAYLOG-FILE
           OPEN INPUT CUSTOMER-FILE
           OPEN INPUT CHECKINOUT-FILE
           OPEN INPUT ROOMS-FILE

           IF WS-STAYLOG-FILE-STATUS NOT = 00
               DISPLAY RED-COLOR "*** ERROR: Cannot open STAYLOG f"
               "ile. Status: " WS-STAYLOG-FILE-STATUS " ***"
               RESET-COLOR
               DISPLAY " "
               DISPLAY "Press ENTER to continue..."
               ACCEPT WS-DUMMY-INPUT
               GO TO CLOSE-ALL-FILES
           END-IF

           DISPLAY WS-HEADER-1
           DISPLAY WS-HEADER-2
           DISPLAY " "

           MOVE 'N' TO WS-EOF
           MOVE 0 TO WS-STAYLOG-COUNTER

           PERFORM UNTIL WS-EOF = 'Y'
               READ STAYLOG-FILE NEXT
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       PERFORM DISPLAY-STAYLOG-DETAIL
                       ADD 1 TO WS-STAYLOG-COUNTER
               END-READ
           END-PERFORM

           MOVE WS-STAYLOG-COUNTER TO WS-STAYLOG-COUNT-DISPLAY
           DISPLAY " "
           DISPLAY "=================================================="
           "====================================="
           DISPLAY GREEN-COLOR "Total Stay Logs: " 
           WS-STAYLOG-COUNT-DISPLAY
           RESET-COLOR
           DISPLAY "=================================================="
           "====================================="

           PERFORM CLOSE-ALL-FILES

           DISPLAY " "
           DISPLAY "Press ENTER to continue..."
           ACCEPT WS-DUMMY-INPUT.

       SEARCH-BY-CUSTOMER-ID.
           DISPLAY CLEAR-SCREEN
           DISPLAY CYAN-COLOR
           DISPLAY "=================================================="
           "====================================="
           DISPLAY "                          SEARCH BY CUSTOMER"
           " ID                                 "
           DISPLAY "=================================================="
           "====================================="
           RESET-COLOR
           DISPLAY " "
           DISPLAY "Enter Customer ID: "
           ACCEPT WS-SEARCH-CUSTOMER-ID

           OPEN INPUT STAYLOG-FILE
           OPEN INPUT CUSTOMER-FILE
           OPEN INPUT CHECKINOUT-FILE
           OPEN INPUT ROOMS-FILE

           DISPLAY " "
           DISPLAY WS-HEADER-1
           DISPLAY WS-HEADER-2
           DISPLAY " "

           MOVE 'N' TO WS-EOF
           MOVE 'N' TO WS-FOUND
           MOVE 0 TO WS-STAYLOG-COUNTER

           PERFORM UNTIL WS-EOF = 'Y'
               READ STAYLOG-FILE NEXT
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF CUSTOMER-ID-SL = WS-SEARCH-CUSTOMER-ID
                           PERFORM DISPLAY-STAYLOG-DETAIL
                           ADD 1 TO WS-STAYLOG-COUNTER
                           MOVE 'Y' TO WS-FOUND
                       END-IF
               END-READ
           END-PERFORM

           IF WS-FOUND = 'N'
               DISPLAY " "
           DISPLAY "=================================================="
           "====================================="
               DISPLAY YELLOW-COLOR "No stay logs found for Customer"
               " ID: " WS-SEARCH-CUSTOMER-ID RESET-COLOR
           DISPLAY "=================================================="
           "====================================="
           ELSE
               DISPLAY " "
           DISPLAY "=================================================="
           "====================================="
               MOVE WS-STAYLOG-COUNTER TO WS-STAYLOG-COUNT-DISPLAY
               DISPLAY GREEN-COLOR "Found " 
               WS-STAYLOG-COUNT-DISPLAY
               " stay log(s) for Customer ID: "
               WS-SEARCH-CUSTOMER-ID RESET-COLOR
           DISPLAY "=================================================="
           "====================================="
           END-IF

           PERFORM CLOSE-ALL-FILES

           DISPLAY " "
           DISPLAY "Press ENTER to continue..."
           ACCEPT WS-DUMMY-INPUT.

       SEARCH-BY-ROOM-ID.
           DISPLAY CLEAR-SCREEN
           DISPLAY CYAN-COLOR
           DISPLAY "=================================================="
           "====================================="
           DISPLAY "                            SEARCH BY ROOM I"
           "D                                   "
           DISPLAY "=================================================="
           "====================================="
           RESET-COLOR
           DISPLAY " "
           DISPLAY "Enter Room ID: "
           ACCEPT WS-SEARCH-ROOM-ID

           OPEN INPUT STAYLOG-FILE
           OPEN INPUT CUSTOMER-FILE
           OPEN INPUT CHECKINOUT-FILE
           OPEN INPUT ROOMS-FILE

           DISPLAY " "
           DISPLAY WS-HEADER-1
           DISPLAY WS-HEADER-2
           DISPLAY " "

           MOVE 'N' TO WS-EOF
           MOVE 'N' TO WS-FOUND
           MOVE 0 TO WS-STAYLOG-COUNTER

           PERFORM UNTIL WS-EOF = 'Y'
               READ STAYLOG-FILE NEXT
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF ROOM-ID-SL = WS-SEARCH-ROOM-ID
                           PERFORM DISPLAY-STAYLOG-DETAIL
                           ADD 1 TO WS-STAYLOG-COUNTER
                           MOVE 'Y' TO WS-FOUND
                       END-IF
               END-READ
           END-PERFORM

           IF WS-FOUND = 'N'
               DISPLAY " "
           DISPLAY "=================================================="
           "====================================="
               DISPLAY YELLOW-COLOR "No stay logs found for Room ID:"
               " " WS-SEARCH-ROOM-ID RESET-COLOR
           DISPLAY "=================================================="
           "====================================="
           ELSE
               DISPLAY " "
           DISPLAY "=================================================="
           "====================================="
               MOVE WS-STAYLOG-COUNTER TO WS-STAYLOG-COUNT-DISPLAY
               DISPLAY GREEN-COLOR "Found " 
               WS-STAYLOG-COUNT-DISPLAY
               " stay log(s) for Room ID: "
               WS-SEARCH-ROOM-ID RESET-COLOR
           DISPLAY "=================================================="
           "====================================="
           END-IF

           PERFORM CLOSE-ALL-FILES

           DISPLAY " "
           DISPLAY "Press ENTER to continue..."
           ACCEPT WS-DUMMY-INPUT.

       SEARCH-BY-CHECKIN-ID.
           DISPLAY CLEAR-SCREEN
           DISPLAY CYAN-COLOR
           DISPLAY "=================================================="
           "================================================"
           DISPLAY "                          SEARCH BY CHECK-IN"
           " ID                                 "
           DISPLAY "=================================================="
           "================================================"
           RESET-COLOR
           DISPLAY " "
           DISPLAY "Enter Check-in ID: "
           ACCEPT WS-SEARCH-CHECKIN-ID

           OPEN INPUT STAYLOG-FILE
           OPEN INPUT CUSTOMER-FILE
           OPEN INPUT CHECKINOUT-FILE
           OPEN INPUT ROOMS-FILE

           DISPLAY " "
           DISPLAY WS-HEADER-1
           DISPLAY WS-HEADER-2
           DISPLAY " "

           MOVE 'N' TO WS-EOF
           MOVE 'N' TO WS-FOUND
           MOVE 0 TO WS-STAYLOG-COUNTER

           PERFORM UNTIL WS-EOF = 'Y'
               READ STAYLOG-FILE NEXT
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF CHECKIN-ID-SL = WS-SEARCH-CHECKIN-ID
                           PERFORM DISPLAY-STAYLOG-DETAIL
                           ADD 1 TO WS-STAYLOG-COUNTER
                           MOVE 'Y' TO WS-FOUND
                       END-IF
               END-READ
           END-PERFORM

           IF WS-FOUND = 'N'
               DISPLAY " "
           DISPLAY "=================================================="
           "================================================"
               DISPLAY YELLOW-COLOR "No stay logs found for Check-in"
               " ID: " WS-SEARCH-CHECKIN-ID RESET-COLOR
           DISPLAY "=================================================="
           "================================================"
           ELSE
               DISPLAY " "
           DISPLAY "=================================================="
           "================================================"
               MOVE WS-STAYLOG-COUNTER TO WS-STAYLOG-COUNT-DISPLAY
               DISPLAY GREEN-COLOR "Found " 
               WS-STAYLOG-COUNT-DISPLAY
               " stay log(s) for Check-in ID: "
               WS-SEARCH-CHECKIN-ID RESET-COLOR
           DISPLAY "=================================================="
           "=======================================4========="
           END-IF

           PERFORM CLOSE-ALL-FILES

           DISPLAY " "
           DISPLAY "Press ENTER to continue..."
           ACCEPT WS-DUMMY-INPUT.

       DISPLAY-STAYLOG-DETAIL.
           *> Get customer name
           MOVE CUSTOMER-ID-SL TO CUSTOMER-ID
           READ CUSTOMER-FILE KEY IS CUSTOMER-ID
               INVALID KEY
                   MOVE "Unknown" TO WS-CUSTOMER-NAME-TEMP
               NOT INVALID KEY
                   MOVE CUSTOMER-NAME TO WS-CUSTOMER-NAME-TEMP
           END-READ

           *> Get room type
           MOVE ROOM-ID-SL TO ROOM-ID
           READ ROOMS-FILE KEY IS ROOM-ID
               INVALID KEY
                   MOVE "Unknown" TO WS-ROOM-TYPE-TEMP
               NOT INVALID KEY
                   MOVE ROOM-TYPE TO WS-ROOM-TYPE-TEMP
           END-READ

           *> Get check-in/check-out information
           MOVE CHECKIN-ID-SL TO CHECKIN-ID
           READ CHECKINOUT-FILE KEY IS CHECKIN-ID
               INVALID KEY
                   MOVE "N/A" TO WS-FORMATTED-DATE
                   MOVE "N/A" TO WS-FORMATTED-CHECKOUT-DATE
               NOT INVALID KEY
                   *> Format check-in date
                   MOVE ACTUAL-CHECKIN-DATE TO WS-TEMP-DATE
                   STRING WS-TEMP-DATE(7:2) "/"
                          WS-TEMP-DATE(5:2) "/"
                          WS-TEMP-DATE(1:4)
                          INTO WS-FORMATTED-DATE

                   *> Format checkout status
                   IF CHECKOUT-FLAG = 'Y'
                       MOVE CHECKOUT-DATE TO WS-TEMP-DATE
                       STRING WS-TEMP-DATE(7:2) "/"
                              WS-TEMP-DATE(5:2) "/"
                              WS-TEMP-DATE(1:4)
                              INTO WS-FORMATTED-CHECKOUT-DATE
                   ELSE
                       MOVE "Active" TO WS-FORMATTED-CHECKOUT-DATE
                   END-IF
           END-READ

           *> Build detail line
           MOVE STAYLOG-ID TO WS-DL-STAYLOG-ID
           MOVE CHECKIN-ID-SL TO WS-DL-CHECKIN-ID
           MOVE CUSTOMER-ID-SL TO WS-DL-CUSTOMER-ID
           MOVE WS-CUSTOMER-NAME-TEMP(1:20) TO WS-DL-CUSTOMER-NAME
           MOVE ROOM-ID-SL TO WS-DL-ROOM-ID
           MOVE WS-ROOM-TYPE-TEMP(1:9) TO WS-DL-ROOM-TYPE
           MOVE WS-FORMATTED-DATE TO WS-DL-CHECKIN
           MOVE WS-FORMATTED-CHECKOUT-DATE TO WS-DL-CHECKOUT

           DISPLAY WS-DETAIL-LINE.

       CLOSE-ALL-FILES.
           CLOSE STAYLOG-FILE
           CLOSE CUSTOMER-FILE
           CLOSE CHECKINOUT-FILE
           CLOSE ROOMS-FILE.

       END PROGRAM viewStaylogs.
