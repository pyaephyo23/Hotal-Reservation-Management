       IDENTIFICATION DIVISION.
       PROGRAM-ID. INITROOMS.

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
       01  WS-EOF              PIC X VALUE 'N'.
       01  WS-INDEX            PIC 99.
       01  WS-ROOM-NUM         PIC 99.
       01  WS-TEMP-NUM         PIC 99.
       01  WS-ROOM-ID-NUM      PIC 9(3).

       PROCEDURE DIVISION.
           OPEN OUTPUT ROOMS-FILE

           *> Add 20 Single rooms (R001-R020)
           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 20
               MOVE SPACES TO ROOM-ID
               IF WS-INDEX < 10
                   STRING 'R00'
                         WS-INDEX
                         DELIMITED BY SIZE
                         INTO ROOM-ID
               ELSE
                   STRING 'R0'
                         WS-INDEX
                         DELIMITED BY SIZE
                         INTO ROOM-ID
               END-IF
               MOVE 'Single' TO ROOM-TYPE
               MOVE 50000 TO PRICE-PER-NIGHT
               MOVE 'Available' TO R-STATUS
               WRITE ROOM-RECORD
           END-PERFORM

           *> Add 10 Double rooms (R021-R030)
           PERFORM VARYING WS-INDEX FROM 21 BY 1 UNTIL WS-INDEX > 30
               MOVE SPACES TO ROOM-ID
               STRING 'R0'
                     WS-INDEX
                     DELIMITED BY SIZE
                     INTO ROOM-ID
               MOVE 'Double' TO ROOM-TYPE
               MOVE 80000 TO PRICE-PER-NIGHT
               MOVE 'Available' TO R-STATUS
               WRITE ROOM-RECORD
           END-PERFORM

           *> Add 5 Deluxe rooms (R031-R035)
           PERFORM VARYING WS-INDEX FROM 31 BY 1 UNTIL WS-INDEX > 35
               MOVE SPACES TO ROOM-ID
               STRING 'R0'
                     WS-INDEX
                     DELIMITED BY SIZE
                     INTO ROOM-ID
               MOVE 'Deluxe' TO ROOM-TYPE
               MOVE 120000 TO PRICE-PER-NIGHT
               MOVE 'Available' TO R-STATUS
               WRITE ROOM-RECORD
           END-PERFORM

           CLOSE ROOMS-FILE
           STOP RUN.

           END PROGRAM INITROOMS.
