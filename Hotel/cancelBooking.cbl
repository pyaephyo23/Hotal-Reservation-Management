      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. cancelBooking.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ROOMS-FILE ASSIGN TO '../DATA/ROOMS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ROOM-ID.
           SELECT BOOKING-FILE ASSIGN TO '../DATA/BOOKINGS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS BOOKING-ID
               ALTERNATE RECORD KEY IS CHECKIN-DATE WITH DUPLICATES
               ALTERNATE RECORD KEY IS CHECKOUT-DATE WITH DUPLICATES.
       DATA DIVISION.
       FILE SECTION.
       FD  ROOMS-FILE.
       COPY "./CopyBooks/ROOMS.cpy".

       FD  BOOKING-FILE.
       COPY "./CopyBooks/BOOKINGS.cpy".

       WORKING-STORAGE SECTION.
       01 WS-CHOICE          PIC 9.
       01 WS-BOOKING-ID      PIC 9(5).
       01 WS-ROOM-ID         PIC X(5).
       01 WS-FOUND           PIC X VALUE 'N'.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "***************************************************"
           DISPLAY "1. Cancel Booking"
           DISPLAY "9. Exit"
           DISPLAY "***************************************************"
           ACCEPT WS-CHOICE
           EVALUATE WS-CHOICE
               WHEN 1
                   DISPLAY "Enter Booking ID to Cancel"
                   ACCEPT WS-BOOKING-ID
                   PERFORM CANCEL-BOOKING-PROCESS
                   GO TO MAIN-PROCEDURE
               WHEN 9
                   STOP RUN
               WHEN OTHER
                   DISPLAY "Invalid option. Try again."
                   GO TO MAIN-PROCEDURE
           END-EVALUATE.
       CANCEL-BOOKING-PROCESS.
           MOVE 'N' TO WS-FOUND
           OPEN I-O BOOKING-FILE
          MOVE WS-BOOKING-ID to NRC
           READ BOOKING-FILE KEY IS BOOKING-ID
               INVALID KEY
                   DISPLAY "Invalid Booking ID."
                   CLOSE BOOKING-FILE
                   EXIT PARAGRAPH
               NOT INVALID KEY
                   IF BOOKING-STATUS = 'Active'
                       MOVE "Cancelled" TO BOOKING-STATUS
                       MOVE ROOM-ID-BK TO WS-ROOM-ID
                       REWRITE BOOKING-RECORD
                           INVALID KEY
                     DISPLAY "Error: Unable to rewrite booking record."
                           NOT INVALID KEY
                    DISPLAY "Booking ID " WS-BOOKING-ID " is Cancelled."
                       END-REWRITE
                       MOVE 'Y' TO WS-FOUND
                   ELSE
                       DISPLAY "Booking is not Active."
                       MOVE 'Y' TO WS-FOUND
                   END-IF
           END-READ
           CLOSE BOOKING-FILE
           IF WS-FOUND = 'Y'
               PERFORM CANCEL-ROOM-PROCESS
           END-IF.

       CANCEL-ROOM-PROCESS.
           OPEN I-O ROOMS-FILE

           MOVE WS-ROOM-ID to ROOM-ID
           READ ROOMS-FILE KEY IS ROOM-ID
               INVALID KEY
                   DISPLAY "Associated Room not found."
               NOT INVALID KEY
                   IF R-STATUS = 'Booked'
                       MOVE "Available" TO R-STATUS
                       REWRITE ROOMS-RECORD
                           INVALID KEY
                         DISPLAY "Error: Unable to rewrite room record."
                           NOT INVALID KEY
                      DISPLAY "Room ID " WS-ROOM-ID " is now Available."
                       END-REWRITE
                   ELSE
                  DISPLAY "Room " WS-ROOM-ID " is not currently Booked."
                   END-IF
           END-READ
           CLOSE ROOMS-FILE.
       END PROGRAM cancelBooking.
