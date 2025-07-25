       IDENTIFICATION DIVISION.
       PROGRAM-ID. viewRooms.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ROOMS-FILE ASSIGN TO '../DATA/ROOMS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ROOM-ID.

       DATA DIVISION.
       FILE SECTION.
       FD  ROOMS-FILE.
       COPY "./CopyBooks/ROOMS.cpy".

       WORKING-STORAGE SECTION.
       01  WS-EOF                  PIC X VALUE 'N'.
       01  WS-ROOM-COUNTER         PIC 999 VALUE 0.
       01  WS-ROOM-COUNT-DISPLAY   PIC ZZZ.
       01  MENU-CHOICE             PIC 9.

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
           05 FILLER               PIC X(8) VALUE SPACES.
           05 FILLER               PIC X(6) VALUE 'ROOM  '.
           05 FILLER               PIC X(8) VALUE SPACES.
           05 FILLER               PIC X(6) VALUE 'TYPE  '.
           05 FILLER               PIC X(8) VALUE SPACES.
           05 FILLER               PIC X(8) VALUE 'PRICE   '.
           05 FILLER               PIC X(8) VALUE SPACES.
           05 FILLER               PIC X(8) VALUE 'STATUS  '.
           05 FILLER               PIC X(11) VALUE SPACES.
       01  WS-HEADER-2.
           05 FILLER               PIC X(8) VALUE SPACES.
           05 FILLER               PIC X(6) VALUE '------'.
           05 FILLER               PIC X(8) VALUE SPACES.
           05 FILLER               PIC X(10) VALUE '----------'.
           05 FILLER               PIC X(4) VALUE SPACES.
           05 FILLER               PIC X(10) VALUE '----------'.
           05 FILLER               PIC X(4) VALUE SPACES.
           05 FILLER               PIC X(10) VALUE '----------'.
           05 FILLER               PIC X(11) VALUE SPACES.
       01  WS-DETAIL-LINE.
           05 FILLER               PIC X(8) VALUE SPACES.
           05 WS-DL-ROOM-ID        PIC X(6).
           05 FILLER               PIC X(8) VALUE SPACES.
           05 WS-DL-ROOM-TYPE      PIC X(10).
           05 FILLER               PIC X(4) VALUE SPACES.
           05 WS-DL-PRICE          PIC $(9).
           05 FILLER               PIC X(5) VALUE SPACES.
           05 WS-DL-STATUS         PIC X(10).
           05 FILLER               PIC X(11) VALUE SPACES.
       LINKAGE SECTION.
       01 LINK PIC 9.
       PROCEDURE DIVISION USING LINK.
       MAIN-LOOP.
           PERFORM UNTIL MENU-CHOICE = 9
           DISPLAY CLEAR-SCREEN
           DISPLAY BLUE-COLOR
           DISPLAY "==================================================="
           "============================"
           DISPLAY "                         VIEW HOTEL ROOMS SYSTEM  "
           "                           "
           DISPLAY "==================================================="
           "============================"
           RESET-COLOR
           DISPLAY "                                                   "
           DISPLAY "                        1. View All Rooms         "
           "                        "
           DISPLAY "                        2. View Single Rooms      "
           "                        "
           DISPLAY "                        3. View Double Rooms      "
           "                        "
           DISPLAY "                        4. View Delux Rooms       "
           "                        "
           DISPLAY "                                                   "
           DISPLAY "==================================================="
           "============================"
           DISPLAY "                        9. Go Back to Main Menu   "
           "                     "
           DISPLAY "==================================================="
           "============================"
           ACCEPT MENU-CHOICE
           EVALUATE MENU-CHOICE
               WHEN 1 PERFORM All-ROOMS-DSP
               WHEN 2 PERFORM SINGLE-ROOMS-DSP
               WHEN 3 PERFORM DOUBLE-ROOMS-DSP
               WHEN 4 PERFORM DELUX-ROOMS-DSP
               WHEN 9 GOBACK
               WHEN OTHER
                   DISPLAY " "
                   DISPLAY RED-COLOR "*** ERROR: Invalid selection. P"
                   "lease choose 1-4 or 9. ***" RESET-COLOR
                   DISPLAY " "
                   DISPLAY "Press ENTER to continue..."
                   ACCEPT WS-DUMMY-INPUT
           END-EVALUATE
           END-PERFORM.
           GOBACK.

       All-ROOMS-DSP.
           MOVE 0 TO WS-ROOM-COUNTER
           MOVE 'N' TO WS-EOF
           DISPLAY CLEAR-SCREEN
           DISPLAY CYAN-COLOR
           DISPLAY "==============================================="
           "================================"
           DISPLAY "                           ALL ROOMS REPORT   "
           "                               "
           DISPLAY "==============================================="
           "================================"
           RESET-COLOR
           DISPLAY " "

           OPEN INPUT ROOMS-FILE
           PERFORM DISPLAY-HEADERS
           PERFORM READ-AND-DISPLAY-ALL UNTIL WS-EOF = 'Y'
           PERFORM DISPLAY-SUMMARY
           CLOSE ROOMS-FILE.

       SINGLE-ROOMS-DSP.
           MOVE 0 TO WS-ROOM-COUNTER
           MOVE 'N' TO WS-EOF
           DISPLAY CLEAR-SCREEN
           DISPLAY CYAN-COLOR
           DISPLAY "==============================================="
           "================================"
           DISPLAY "                         SINGLE ROOMS REPORT "
           "                               "
           DISPLAY "==============================================="
           "================================"
           RESET-COLOR
           DISPLAY " "

           OPEN INPUT ROOMS-FILE
           PERFORM DISPLAY-HEADERS
           PERFORM READ-AND-DISPLAY-SINGLE UNTIL WS-EOF = 'Y'
           PERFORM DISPLAY-SUMMARY
           CLOSE ROOMS-FILE.

       DOUBLE-ROOMS-DSP.
           MOVE 0 TO WS-ROOM-COUNTER
           MOVE 'N' TO WS-EOF
           DISPLAY CLEAR-SCREEN
           DISPLAY CYAN-COLOR
           DISPLAY "==============================================="
           "================================"
           DISPLAY "                         DOUBLE ROOMS REPORT "
           "                               "
           DISPLAY "==============================================="
           "================================"
           RESET-COLOR
           DISPLAY " "

           OPEN INPUT ROOMS-FILE
           PERFORM DISPLAY-HEADERS
           PERFORM READ-AND-DISPLAY-DOUBLE UNTIL WS-EOF = 'Y'
           PERFORM DISPLAY-SUMMARY
           CLOSE ROOMS-FILE.

       DELUX-ROOMS-DSP.
           MOVE 0 TO WS-ROOM-COUNTER
           MOVE 'N' TO WS-EOF
           DISPLAY CLEAR-SCREEN
           DISPLAY CYAN-COLOR
           DISPLAY "==============================================="
           "================================"
           DISPLAY "                          DELUX ROOMS REPORT "
           "                               "
           DISPLAY "==============================================="
           "================================"
           RESET-COLOR
           DISPLAY " "

           OPEN INPUT ROOMS-FILE
           PERFORM DISPLAY-HEADERS
           PERFORM READ-AND-DISPLAY-DELUX UNTIL WS-EOF = 'Y'
           PERFORM DISPLAY-SUMMARY
           CLOSE ROOMS-FILE.

       DISPLAY-HEADERS.
           DISPLAY YELLOW-COLOR
           DISPLAY WS-HEADER-1
           DISPLAY WS-HEADER-2
           RESET-COLOR.

       READ-AND-DISPLAY-ALL.
           READ ROOMS-FILE
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   PERFORM DISPLAY-ROOM-RECORD
                   ADD 1 TO WS-ROOM-COUNTER
           END-READ.

       READ-AND-DISPLAY-SINGLE.
           READ ROOMS-FILE
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   IF ROOM-TYPE = 'Single'
                       PERFORM DISPLAY-ROOM-RECORD
                       ADD 1 TO WS-ROOM-COUNTER
                   END-IF
           END-READ.

       READ-AND-DISPLAY-DOUBLE.
           READ ROOMS-FILE
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   IF ROOM-TYPE = 'Double'
                       PERFORM DISPLAY-ROOM-RECORD
                       ADD 1 TO WS-ROOM-COUNTER
                   END-IF
           END-READ.

       READ-AND-DISPLAY-DELUX.
           READ ROOMS-FILE
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   IF ROOM-TYPE = 'Delux'
                       PERFORM DISPLAY-ROOM-RECORD
                       ADD 1 TO WS-ROOM-COUNTER
                   END-IF
           END-READ.

       DISPLAY-ROOM-RECORD.
           MOVE ROOM-ID TO WS-DL-ROOM-ID
           MOVE ROOM-TYPE TO WS-DL-ROOM-TYPE
           MOVE PRICE-PER-NIGHT TO WS-DL-PRICE
           MOVE R-STATUS TO WS-DL-STATUS
           DISPLAY WS-DETAIL-LINE.

       DISPLAY-SUMMARY.
           MOVE WS-ROOM-COUNTER TO WS-ROOM-COUNT-DISPLAY
           DISPLAY " "
           DISPLAY "==============================================="
           "================================"
           DISPLAY GREEN-COLOR "Total Rooms Found: " 
           WS-ROOM-COUNT-DISPLAY RESET-COLOR
           DISPLAY "==============================================="
           "================================"
           DISPLAY " "
           DISPLAY "Press ENTER to continue..."
           ACCEPT WS-DUMMY-INPUT.
       END PROGRAM viewRooms.
