       IDENTIFICATION DIVISION.
       PROGRAM-ID. viewRooms.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ROOMS-FILE ASSIGN TO '../DATA/ROOMS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ROOM-ID.
           SELECT BOOKINGS-FILE ASSIGN TO '../DATA/BOOKINGS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS BOOKING-ID.

       DATA DIVISION.
       FILE SECTION.
       FD  ROOMS-FILE.
       COPY "./CopyBooks/ROOMS.cpy".
       FD  BOOKINGS-FILE.
       COPY "./CopyBooks/BOOKINGS.cpy".

       WORKING-STORAGE SECTION.
       01  WS-EOF                  PIC X VALUE 'N'.
       01  WS-BOOKING-EOF          PIC X VALUE 'N'.
       01  WS-ROOM-COUNTER         PIC 999 VALUE 0.
       01  WS-ACTIVE-BOOKINGS      PIC 999 VALUE 0.
       01  MENU-CHOICE             PIC 9.

       *> Color codes for display
       01 RED-COLOR          PIC X(8) VALUE X"1B5B33316D".
       01 GREEN-COLOR        PIC X(8) VALUE X"1B5B33326D".
       01 RESET-COLOR        PIC X(4) VALUE X"1B5B306D".

       01  WS-HEADER-1.
           05 FILLER               PIC X(5) VALUE 'ROOM'.
           05 FILLER               PIC X(5) VALUE SPACES.
           05 FILLER               PIC X(4) VALUE 'TYPE'.
           05 FILLER               PIC X(9) VALUE SPACES.
           05 FILLER               PIC X(5) VALUE 'PRICE'.
           05 FILLER               PIC X(8) VALUE SPACES.
           05 FILLER               PIC X(6) VALUE 'STATUS'.
           05 FILLER               PIC X(5) VALUE SPACES.
           05 FILLER               PIC X(15) VALUE 'ACTIVE BOOKINGS'.
       01  WS-HEADER-2.
           05 FILLER               PIC X(5) VALUE '-----'.
           05 FILLER               PIC X(5) VALUE SPACES.
           05 FILLER               PIC X(10) VALUE '----------'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(10) VALUE '----------'.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 FILLER               PIC X(10) VALUE '----------'.
           05 FILLER               PIC X(5) VALUE SPACES.
           05 FILLER               PIC X(15) VALUE '---------------'.
       01  WS-DETAIL-LINE.
           05 WS-DL-ROOM-ID        PIC X(5).
           05 FILLER               PIC X(5) VALUE SPACES.
           05 WS-DL-ROOM-TYPE      PIC X(10).
           05 FILLER               PIC X(3) VALUE SPACES.
           05 WS-DL-PRICE          PIC Z,ZZZ,ZZ9.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 WS-DL-STATUS         PIC X(10).
           05 FILLER               PIC X(5) VALUE SPACES.
           05 WS-DL-ACTIVE-COUNT   PIC ZZ9.
       LINKAGE SECTION.
       01 LINK PIC 9.
       PROCEDURE DIVISION USING LINK.
       MAIN-LOOP.
           PERFORM UNTIL MENU-CHOICE = 9
           DISPLAY
           "**************************************************"
           DISPLAY "View Hotel Rooms"
           DISPLAY "1. View All Rooms"
           DISPLAY "2. View Single Rooms"
           DISPLAY "3. View Double Rooms"
           DISPLAY "4. View Delux Rooms"
           DISPLAY "9. Go Back."
           DISPLAY
           "**************************************************"
           ACCEPT MENU-CHOICE
           EVALUATE MENU-CHOICE
               WHEN 1 PERFORM All-ROOMS-DSP
               WHEN 2 PERFORM SINGLE-ROOMS-DSP
               WHEN 3 PERFORM DOUBLE-ROOMS-DSP
               WHEN 4 PERFORM DELUX-ROOMS-DSP
               WHEN 9 GOBACK
               WHEN OTHER
                   DISPLAY RED-COLOR "Invalid selection." RESET-COLOR
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

       SINGLE-ROOMS-DSP.
           MOVE 0 TO WS-ROOM-COUNTER
           MOVE 'N' TO WS-EOF
           OPEN INPUT ROOMS-FILE
           PERFORM DISPLAY-HEADERS
           PERFORM READ-AND-DISPLAY-SINGLE UNTIL WS-EOF = 'Y'
           PERFORM DISPLAY-SUMMARY
           CLOSE ROOMS-FILE.

       DOUBLE-ROOMS-DSP.
           MOVE 0 TO WS-ROOM-COUNTER
           MOVE 'N' TO WS-EOF
           OPEN INPUT ROOMS-FILE
           PERFORM DISPLAY-HEADERS
           PERFORM READ-AND-DISPLAY-DOUBLE UNTIL WS-EOF = 'Y'
           PERFORM DISPLAY-SUMMARY
           CLOSE ROOMS-FILE.

       DELUX-ROOMS-DSP.
           MOVE 0 TO WS-ROOM-COUNTER
           MOVE 'N' TO WS-EOF
           OPEN INPUT ROOMS-FILE
           PERFORM DISPLAY-HEADERS
           PERFORM READ-AND-DISPLAY-DELUX UNTIL WS-EOF = 'Y'
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
                   PERFORM COUNT-ACTIVE-BOOKINGS
                   PERFORM DISPLAY-ROOM-RECORD
                   ADD 1 TO WS-ROOM-COUNTER
           END-READ.

       READ-AND-DISPLAY-SINGLE.
           READ ROOMS-FILE
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   IF ROOM-TYPE = 'Single'
                       PERFORM COUNT-ACTIVE-BOOKINGS
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
                       PERFORM COUNT-ACTIVE-BOOKINGS
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
                       PERFORM COUNT-ACTIVE-BOOKINGS
                       PERFORM DISPLAY-ROOM-RECORD
                       ADD 1 TO WS-ROOM-COUNTER
                   END-IF
           END-READ.

       DISPLAY-ROOM-RECORD.
           MOVE ROOM-ID TO WS-DL-ROOM-ID
           MOVE ROOM-TYPE TO WS-DL-ROOM-TYPE
           MOVE PRICE-PER-NIGHT TO WS-DL-PRICE
           MOVE R-STATUS TO WS-DL-STATUS
           MOVE WS-ACTIVE-BOOKINGS TO WS-DL-ACTIVE-COUNT
           DISPLAY WS-DETAIL-LINE.

       COUNT-ACTIVE-BOOKINGS.
           MOVE 0 TO WS-ACTIVE-BOOKINGS
           MOVE 'N' TO WS-BOOKING-EOF
           OPEN INPUT BOOKINGS-FILE
           PERFORM COUNT-BOOKINGS-LOOP UNTIL WS-BOOKING-EOF = 'Y'
           CLOSE BOOKINGS-FILE.

       COUNT-BOOKINGS-LOOP.
           READ BOOKINGS-FILE
               AT END
                   MOVE 'Y' TO WS-BOOKING-EOF
               NOT AT END
                   IF ROOM-ID-BK = ROOM-ID AND
                      (BOOKING-STATUS = 'Active' OR
                       BOOKING-STATUS = 'Confirmed')
                       ADD 1 TO WS-ACTIVE-BOOKINGS
                   END-IF
           END-READ.

       DISPLAY-SUMMARY.
           DISPLAY SPACES
           DISPLAY 'Total Rooms: ' WS-ROOM-COUNTER.
       END PROGRAM viewRooms.
