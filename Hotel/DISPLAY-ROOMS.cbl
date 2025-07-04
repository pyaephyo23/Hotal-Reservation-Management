       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISPLAYROOMS.

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
       01  ROOM-RECORD.
           05  ROOM-ID             PIC X(5).
           05  ROOM-TYPE           PIC X(10).
           05  PRICE-PER-NIGHT     PIC 9(9).
           05  R-STATUS            PIC X(10).

       WORKING-STORAGE SECTION.
       01  WS-EOF                  PIC X VALUE 'N'.
       01  WS-ROOM-COUNTER         PIC 999 VALUE 0.
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

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM OPEN-FILES
           PERFORM DISPLAY-HEADERS
           PERFORM READ-AND-DISPLAY-RECORDS UNTIL WS-EOF = 'Y'
           PERFORM DISPLAY-SUMMARY
           PERFORM CLOSE-FILES
           STOP RUN.

       OPEN-FILES.
           OPEN INPUT ROOMS-FILE.

       DISPLAY-HEADERS.
           DISPLAY WS-HEADER-1
           DISPLAY WS-HEADER-2.

       READ-AND-DISPLAY-RECORDS.
           READ ROOMS-FILE
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   PERFORM DISPLAY-ROOM-RECORD
                   ADD 1 TO WS-ROOM-COUNTER
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

       CLOSE-FILES.
           CLOSE ROOMS-FILE.
