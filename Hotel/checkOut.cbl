        IDENTIFICATION DIVISION.
        PROGRAM-ID. checkOut.
        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT BOOKING-FILE ASSIGN TO '../DATA/BOOKING.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS BOOKING-ID.
        DATA DIVISION.
        FILE SECTION.
        FD  BOOKING-FILE.
           01  BOOKING-RECORD.
               05  BOOKING-ID       PIC 9(5).
               05  CHECKIN-DATE     PIC 9(8).
               05  CHECKOUT-DATE    PIC 9(8).
               05  ROOM-ID          PIC 9(5).
               05  CUST-ID         PIC 9(5).
        WORKING-STORAGE SECTION.
           01 WS-BOOKING-ID PIC 9(5).
        PROCEDURE DIVISION.
           OPEN INPUT BOOKING-FILE.
           READ BOOKING-FILE INTO BOOKING-RECORD
               AT END
                   DISPLAY "No booking found."
               NOT AT END
                   DISPLAY "Booking found: " BOOKING-ID
           END-READ.
           CLOSE BOOKING-FILE.
        END PROGRAM checkOut.
