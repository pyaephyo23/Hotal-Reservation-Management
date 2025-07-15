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
       01  MENU-CHOICE             PIC 9.
       01  WS-HEADER-1.
           05 FILLER               PIC X(5) VALUE 'ROOM'.
           05 FILLER               PIC X(5) VALUE SPACES.
           05 FILLER               PIC X(4) VALUE 'TYPE'.
           05 FILLER               PIC X(9) VALUE SPACES.
           05 FILLER               PIC X(5) VALUE 'PRICE'.
           05 FILLER               PIC X(8) VALUE SPACES.
           05 FILLER               PIC X(6) VALUE 'STATUS'.
       01  WS-HEADER-2.
           05 FILLER               PIC X(5) VALUE '-----'.
           05 FILLER               PIC X(5) VALUE SPACES.
           05 FILLER               PIC X(10) VALUE '----------'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(10) VALUE '----------'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(10) VALUE '----------'.
       01  WS-DETAIL-LINE.
           05 WS-DL-ROOM-ID        PIC X(5).
           05 FILLER               PIC X(5) VALUE SPACES.
           05 WS-DL-ROOM-TYPE      PIC X(10).
           05 FILLER               PIC X(3) VALUE SPACES.
           05 WS-DL-PRICE          PIC Z,ZZZ,ZZ9.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 WS-DL-STATUS         PIC X(10).
       LINKAGE SECTION.
       01 LINK PIC 9.
       PROCEDURE DIVISION USING LINK.
       MAIN-LOOP.
           PERFORM UNTIL MENU-CHOICE = 9
           DISPLAY
           "***********************************************************"
           DISPLAY "View Hotel Rooms"
           DISPLAY "1. View All Rooms"
           DISPLAY "2. View All Available Rooms"
           DISPLAY "3. View All Booked Rooms"
           DISPLAY "4. View All Occupied Rooms."
           DISPLAY "9. Go Back."
           DISPLAY
           "***********************************************************"
           ACCEPT MENU-CHOICE
           EVALUATE MENU-CHOICE
               WHEN 1 PERFORM All-ROOMS-DSP
               WHEN 2 PERFORM AVAILABLE-ROOMS-DSP
               WHEN 3 PERFORM BOOKED-ROOMS-DSP
               WHEN 4 PERFORM OCCUPIED-ROOMS-DSP
               WHEN 9 GOBACK
               WHEN OTHER DISPLAY "Invalid choice"
           END-EVALUATE
           END-PERFORM.
           GOBACK.

       All-ROOMS-DSP.
           MOVE 0 TO WS-ROOM-COUNTER
           MOVE 'N' TO WS-EOF
           OPEN INPUT ROOMS-FILE
           PERFORM DISPLAY-HEADERS
           PERFORM READ-AND-DISPLAY-ALL UNTIL WS-EOF = 'Y'
           PERFORM DISPLAY-SUMMARY
           CLOSE ROOMS-FILE.

       AVAILABLE-ROOMS-DSP.
           MOVE 0 TO WS-ROOM-COUNTER
           MOVE 'N' TO WS-EOF
           OPEN INPUT ROOMS-FILE
           PERFORM DISPLAY-HEADERS
           PERFORM READ-AND-DISPLAY-AVAILABLE UNTIL WS-EOF = 'Y'
           PERFORM DISPLAY-SUMMARY
           CLOSE ROOMS-FILE.

       BOOKED-ROOMS-DSP.
           MOVE 0 TO WS-ROOM-COUNTER
           MOVE 'N' TO WS-EOF
           OPEN INPUT ROOMS-FILE
           PERFORM DISPLAY-HEADERS
           PERFORM READ-AND-DISPLAY-BOOKED UNTIL WS-EOF = 'Y'
           PERFORM DISPLAY-SUMMARY
           CLOSE ROOMS-FILE.

       OCCUPIED-ROOMS-DSP.
           MOVE 0 TO WS-ROOM-COUNTER
           MOVE 'N' TO WS-EOF
           OPEN INPUT ROOMS-FILE
           PERFORM DISPLAY-HEADERS
           PERFORM READ-AND-DISPLAY-OCCUPIED UNTIL WS-EOF = 'Y'
           PERFORM DISPLAY-SUMMARY
           CLOSE ROOMS-FILE.

       DISPLAY-HEADERS.
           DISPLAY WS-HEADER-1
           DISPLAY WS-HEADER-2.

       READ-AND-DISPLAY-ALL.
           READ ROOMS-FILE
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   PERFORM DISPLAY-ROOM-RECORD
                   ADD 1 TO WS-ROOM-COUNTER
           END-READ.

       READ-AND-DISPLAY-AVAILABLE.
           READ ROOMS-FILE
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   IF R-STATUS = 'Available'
                       PERFORM DISPLAY-ROOM-RECORD
                       ADD 1 TO WS-ROOM-COUNTER
                   END-IF
           END-READ.

       READ-AND-DISPLAY-BOOKED.
           READ ROOMS-FILE
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   IF R-STATUS = 'Booked'
                       PERFORM DISPLAY-ROOM-RECORD
                       ADD 1 TO WS-ROOM-COUNTER
                   END-IF
           END-READ.

       READ-AND-DISPLAY-OCCUPIED.
           READ ROOMS-FILE
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   IF R-STATUS = 'Occupied'
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
           DISPLAY SPACES
           DISPLAY 'Total Rooms: ' WS-ROOM-COUNTER.
       END PROGRAM viewRooms.
