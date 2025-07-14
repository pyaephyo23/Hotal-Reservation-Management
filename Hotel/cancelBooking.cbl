      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CANCEL-ROOMS.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ROOMS-FILE ASSIGN TO '../DATA/ROOMS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ROOM-ID.
           SELECT BOOKING-FILE ASSIGN TO '../DATA/BOOKING.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS BOOKING-ID.
       DATA DIVISION.
       FILE SECTION.
       FD  ROOMS-FILE.
       01  ROOMS-RECORD.
           05  ROOM-ID             PIC X(5).
           05  ROOM-TYPE           PIC X(10).
           05  PRICE-PER-NIGHT     PIC 9(9).
           05  R-STATUS            PIC X(10).
       FD  BOOKING-FILE.
       01  BOOKING-RECORD.
           05 BOOKING-ID      PIC 9(5).
           05 ROOM-ID-BK      PIC X(5).
           05 CUSTOMER-ID-BK  PIC 9(6).
           05 CHECKIN-DATE    PIC X(8).
           05 CHECKOUT-DATE   PIC X(8).
           05 BOOKING-STATUS  PIC X(10).
       WORKING-STORAGE SECTION.
       01 WS-CHOICE          PIC 9.
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
                   DISPLAY "Enter Room ID to Cancel"
                   ACCEPT WS-ROOM-ID
                   PERFORM CANCEL-ROOM-PROCESS
                   PERFORM CHANGE-BOOKING-STATUS
                   GO TO MAIN-PROCEDURE
               WHEN 9
                   STOP RUN
               WHEN OTHER
                   DISPLAY "Invalid option. Try again."
                   GO TO MAIN-PROCEDURE
           END-EVALUATE.
           STOP RUN.
           CANCEL-ROOM-PROCESS.
           OPEN I-O ROOMS-FILE
               MOVE 'N' TO WS-FOUND
              PERFORM UNTIL WS-FOUND = 'Y'
               READ ROOMS-FILE NEXT
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       IF ROOM-ID = WS-ROOM-ID
                           IF R-STATUS = 'Booked'
                             MOVE "Available" TO R-STATUS
                             REWRITE ROOMS-RECORD INVALID KEY
                             DISPLAY "Error: Unable to rewrite record."
                             END-REWRITE
                             DISPLAY "Room ID " WS-ROOM-ID
                             " is Cancelled"
                             DISPLAY "Room ID " WS-ROOM-ID
                             " is now "R-STATUS
                             MOVE 'Y' TO WS-FOUND
                           ELSE
                            DISPLAY "Room " WS-ROOM-ID " is Not Booked"
                            MOVE 'Y' TO WS-FOUND
                           END-IF
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE ROOMS-FILE.
           IF WS-FOUND NOT = 'Y'
               DISPLAY "Invalid Room ID"
           END-IF.
           CHANGE-BOOKING-STATUS.
               MOVE 'N' TO WS-FOUND
               OPEN I-O BOOKING-FILE
               PERFORM UNTIL WS-FOUND = 'Y'
               READ BOOKING-FILE NEXT
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       IF WS-ROOM-ID = ROOM-ID-BK
                           IF BOOKING-STATUS = 'Active'
                             MOVE "Cancelled" TO BOOKING-STATUS
                             REWRITE BOOKING-RECORD
                              INVALID KEY
                              DISPLAY "Error: Unable to rewrite record."
                             END-REWRITE
                             DISPLAY "Booking ID "
                             BOOKING-ID " is "BOOKING-STATUS
                             MOVE 'Y' TO WS-FOUND
                           END-IF
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE BOOKING-FILE.
       END PROGRAM CANCEL-ROOMS.
