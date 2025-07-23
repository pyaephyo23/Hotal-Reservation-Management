       IDENTIFICATION DIVISION.
       PROGRAM-ID. initFiles.
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
               FILE STATUS IS WS-FILE-STATUS.
           SELECT INVOICE-FILE ASSIGN TO './DATA/INVOICES.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS INVOICE-ID
               FILE STATUS IS WS-FILE-STATUS.
           SELECT CHECKINOUT-FILE ASSIGN TO './DATA/CHECKINOUT.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CHECKIN-ID
               FILE STATUS IS WS-FILE-STATUS.
           SELECT STAYLOG-FILE ASSIGN TO './DATA/STAYLOG.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS STAYLOG-ID
               FILE STATUS IS WS-FILE-STATUS.
       DATA DIVISION.

       FILE SECTION.
       FD  CUSTOMER-FILE.
       COPY "./CopyBooks/CUSTOMERS.cpy".

       FD  BOOKING-FILE.
       COPY "./CopyBooks/BOOKINGS.cpy".

       FD  INVOICE-FILE.
       COPY "./CopyBooks/INVOICES.cpy".

       FD  CHECKINOUT-FILE.
       COPY "./CopyBooks/CHECKINOUT.cpy".

       FD  STAYLOG-FILE.
       COPY "./CopyBooks/STAYLOG.cpy".

       WORKING-STORAGE SECTION.
       01 WS-DUMMY-VALUE     PIC X VALUE ' '.
       01  WS-FILE-STATUS    PIC 99.
           88 FILE-OK        VALUE 00.
           88 FILE-ERROR     VALUE 01 THRU 99.
       PROCEDURE DIVISION.
       BEGIN.
           OPEN OUTPUT CUSTOMER-FILE
           IF NOT FILE-OK
               DISPLAY "Error opening CUSTOMER-FILE: " WS-FILE-STATUS
               STOP RUN
           END-IF

           OPEN OUTPUT BOOKING-FILE
           IF NOT FILE-OK
               DISPLAY "Error opening BOOKING-FILE: " WS-FILE-STATUS
               CLOSE CUSTOMER-FILE
               STOP RUN
           END-IF

           OPEN OUTPUT INVOICE-FILE
           IF NOT FILE-OK
               DISPLAY "Error opening INVOICE-FILE: " WS-FILE-STATUS
               CLOSE CUSTOMER-FILE
               CLOSE BOOKING-FILE
               STOP RUN
           END-IF

           OPEN OUTPUT CHECKINOUT-FILE
           IF NOT FILE-OK
               DISPLAY "Error opening CHECKINOUT-FILE: " WS-FILE-STATUS
               CLOSE CUSTOMER-FILE
               CLOSE BOOKING-FILE
               CLOSE INVOICE-FILE
               STOP RUN
           END-IF

           OPEN OUTPUT STAYLOG-FILE
           IF NOT FILE-OK
               DISPLAY "Error opening STAYLOG-FILE: " WS-FILE-STATUS
               CLOSE CUSTOMER-FILE
               CLOSE BOOKING-FILE
               CLOSE INVOICE-FILE
               CLOSE CHECKINOUT-FILE
               STOP RUN
           END-IF

           *> Optionally write a dummy record, or just close to create empty files
           CLOSE CUSTOMER-FILE
           CLOSE BOOKING-FILE
           CLOSE INVOICE-FILE
           CLOSE CHECKINOUT-FILE
           CLOSE STAYLOG-FILE

           DISPLAY
           "Files initialized: CUSTOMERS,"
           " BOOKINGS, INVOICES, CHECKINOUT, STAYLOG"
           STOP RUN.
       END PROGRAM initFiles.
