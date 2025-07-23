       IDENTIFICATION DIVISION.
       PROGRAM-ID. initFiles.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO '../DATA/CUSTOMERS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUSTOMER-ID
               ALTERNATE RECORD KEY IS CUSTOMER-NAME WITH DUPLICATES.
           SELECT BOOKING-FILE ASSIGN TO '../DATA/BOOKINGS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS BOOKING-ID.
           SELECT INVOICE-FILE ASSIGN TO '../DATA/INVOICES.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS INVOICE-ID.
           SELECT GUEST-FILE ASSIGN TO '../DATA/GUESTS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS GUEST-ID.
       DATA DIVISION.

       FILE SECTION.
       FD  CUSTOMER-FILE.
       COPY "./CopyBooks/CUSTOMERS.cpy".

       FD  BOOKING-FILE.
       COPY "./CopyBooks/BOOKINGS.cpy".

       FD  INVOICE-FILE.
       COPY "./CopyBooks/INVOICES.cpy".

       FD  GUEST-FILE.
       COPY "./CopyBooks/GUESTS.cpy".

       WORKING-STORAGE SECTION.
       01 WS-DUMMY-VALUE     PIC X VALUE ' '.
       01  WS-FILE-STATUS    PIC 99.
       PROCEDURE DIVISION.
       BEGIN.
           OPEN OUTPUT CUSTOMER-FILE
           OPEN OUTPUT BOOKING-FILE
           OPEN OUTPUT INVOICE-FILE
           OPEN OUTPUT GUEST-FILE

           *> Optionally write a dummy record, or just close to create empty files
           CLOSE CUSTOMER-FILE
           CLOSE BOOKING-FILE
           CLOSE INVOICE-FILE
           CLOSE GUEST-FILE

           DISPLAY
           "Files initialized (CUSTOMERS, BOOKINGS, INVOICES, GUESTS)."
           STOP RUN.
       END PROGRAM initFiles.
