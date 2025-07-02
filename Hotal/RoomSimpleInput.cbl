      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ROOM-FILE ASSIGN TO '../ROOMS.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.
      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
       FD ROOM-FILE.
       01 ROOM-RECORD.
           05 ROOM-NO        PIC 9(3).
           05 ROOM-TYPE      PIC X(10).
           05 ROOM-STATUS    PIC X(10).
           05 ROOM-RATE      PIC 9(5)V99.
      *-----------------------
       WORKING-STORAGE SECTION.
      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
           OPEN OUTPUT ROOM-FILE.

           MOVE 101 TO ROOM-NO
           MOVE "SINGLE" TO ROOM-TYPE
           MOVE "AVAILABLE" TO ROOM-STATUS
           MOVE 50.00 TO ROOM-RATE
           WRITE ROOM-RECORD

           MOVE 102 TO ROOM-NO
           MOVE "DOUBLE" TO ROOM-TYPE
           MOVE "BOOKED" TO ROOM-STATUS
           MOVE 70.00 TO ROOM-RATE
           WRITE ROOM-RECORD

           MOVE 103 TO ROOM-NO
           MOVE "DELUXE" TO ROOM-TYPE
           MOVE "AVAILABLE" TO ROOM-STATUS
           MOVE 100.00 TO ROOM-RATE
           WRITE ROOM-RECORD

           MOVE 104 TO ROOM-NO
           MOVE "SINGLE" TO ROOM-TYPE
           MOVE "BOOKED" TO ROOM-STATUS
           MOVE 50.00 TO ROOM-RATE
           WRITE ROOM-RECORD

           CLOSE ROOM-FILE.
           DISPLAY "ROOMS.DAT initialized with sample data."
      **
            STOP RUN.
      ** add other procedures here
       END PROGRAM YOUR-PROGRAM-NAME.
