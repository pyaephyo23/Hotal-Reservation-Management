       IDENTIFICATION DIVISION.
       PROGRAM-ID. INITFILES.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO '../DATA/CUSTOMER.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUSTOMER-ID
               ALTERNATE RECORD KEY IS CUSTOMER-NAME WITH DUPLICATES.
           SELECT BOOKING-FILE ASSIGN TO '../DATA/BOOKING.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS BOOKING-ID.
       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE.
       01  CUSTOMER-RECORD.
           05 CUSTOMER-ID     PIC 9(5).
           05 CUSTOMER-NAME   PIC X(30).
           05 CUSTOMER-PHONE  PIC X(15).
           05 CUSTOMER-EMAIL  PIC X(30).
           05 CUSTOMER-ADDR   PIC X(50).
       FD  BOOKING-FILE.
       01  BOOKING-RECORD.
           05 BOOKING-ID      PIC 9(5).
           05 ROOM-ID-BK      PIC X(5).
           05 CUSTOMER-ID-BK  PIC 9(6).
           05 CHECKIN-DATE    PIC X(8).
           05 CHECKOUT-DATE   PIC X(8).
           05 BOOKING-STATUS  PIC X(10).
       WORKING-STORAGE SECTION.
       01 WS-DUMMY-VALUE     PIC X VALUE ' '.
       01  WS-FILE-STATUS    PIC 99.
       PROCEDURE DIVISION.
       BEGIN.
           OPEN OUTPUT CUSTOMER-FILE
           OPEN OUTPUT BOOKING-FILE

           *> Optionally write a dummy record, or just close to create empty files
           CLOSE CUSTOMER-FILE
           CLOSE BOOKING-FILE

           DISPLAY "Files initialized."
           STOP RUN.
