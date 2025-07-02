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
           SELECT CUST-FILE ASSIGN TO '../CUSTOMERS.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.
      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
       FD CUST-FILE.
       01 CUST-RECORD.
           05 CUST-ID        PIC 9(5).
           05 CUST-NAME      PIC X(30).
           05 ROOM-NO-BOOKED PIC 9(3).
           05 CHECKIN-DATE   PIC X(10).
           05 CHECKOUT-DATE  PIC X(10).
           05 TOTAL-AMOUNT   PIC 9(5)V99.
      *-----------------------
       WORKING-STORAGE SECTION.
      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
       OPEN OUTPUT CUST-FILE.

           MOVE 10001 TO CUST-ID
           MOVE "Alice Smith" TO CUST-NAME
           MOVE 102 TO ROOM-NO-BOOKED
           MOVE "01-07-2025" TO CHECKIN-DATE
           MOVE "03-07-2025" TO CHECKOUT-DATE
           MOVE 168.00 TO TOTAL-AMOUNT
           WRITE CUST-RECORD

           MOVE 10002 TO CUST-ID
           MOVE "Bob Johnson" TO CUST-NAME
           MOVE 104 TO ROOM-NO-BOOKED
           MOVE "02-07-2025" TO CHECKIN-DATE
           MOVE "04-07-2025" TO CHECKOUT-DATE
           MOVE 120.00 TO TOTAL-AMOUNT
           WRITE CUST-RECORD

           CLOSE CUST-FILE.
           DISPLAY "CUSTOMERS.DAT initialized with sample data.".
            STOP RUN.
      ** add other procedures here
       END PROGRAM YOUR-PROGRAM-NAME.
