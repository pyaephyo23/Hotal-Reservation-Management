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
               RECORD KEY IS BOOKING-ID.
       DATA DIVISION.
       FILE SECTION.
       FD  ROOMS-FILE.
       COPY "../CopyBooks/ROOMS.cpy".

       FD  BOOKING-FILE.
       COPY "../CopyBooks/BOOKINGS.cpy".

       WORKING-STORAGE SECTION.
       01 WS-CHOICE          PIC 9.
       01 WS-BOOKING-ID      PIC 9(5).
       01 WS-ROOM-ID         PIC X(5).
       01 WS-FOUND           PIC X VALUE 'N'.
       01 WS-CURRENT-DATE    PIC X(8).
       01 WS-CANCELLED-COUNT PIC 999 VALUE 0.
       01 WS-EOF             PIC X VALUE 'N'.



       LINKAGE SECTION.
       01 LINK PIC 9.
       PROCEDURE DIVISION USING LINK.
       MAIN-PROCEDURE.
           DISPLAY "***************************************************"
           DISPLAY "1. Cancel Booking By ID"
           DISPLAY "2. Cancel All Expired Bookings (Past Check-in Date)"
           DISPLAY "9. Go Back"
           DISPLAY "***************************************************"
           ACCEPT WS-CHOICE
           EVALUATE WS-CHOICE
               WHEN 1
                   DISPLAY "Enter Booking ID to Cancel"
                   ACCEPT WS-BOOKING-ID
                   PERFORM CANCEL-BOOKING-PROCESS
                   GO TO MAIN-PROCEDURE
               WHEN 2
                   PERFORM CANCEL-EXPIRED-BOOKINGS
                   GO TO MAIN-PROCEDURE
               WHEN 9
                   GOBACK
               WHEN OTHER
                   DISPLAY "Invalid option. Try again."
                   GO TO MAIN-PROCEDURE
           END-EVALUATE.
       CANCEL-BOOKING-PROCESS.
           MOVE 'N' TO WS-FOUND
           OPEN I-O BOOKING-FILE
           MOVE WS-BOOKING-ID TO BOOKING-ID
           READ BOOKING-FILE KEY IS BOOKING-ID
               INVALID KEY
                   DISPLAY "Invalid Booking ID."
                   CLOSE BOOKING-FILE
                   EXIT PARAGRAPH
               NOT INVALID KEY
                   IF BOOKING-STATUS = 'Active' AND CHEKIN-FLAG ='N'
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
                   *> Initialize ACTIVE-BOOKING-COUNT if it contains non-numeric data
                   IF ACTIVE-BOOKING-COUNT NOT NUMERIC
                       MOVE ZERO TO ACTIVE-BOOKING-COUNT
                   END-IF

                   *> Decrease active booking count (with bounds checking)
                   IF ACTIVE-BOOKING-COUNT > 0
                       SUBTRACT 1 FROM ACTIVE-BOOKING-COUNT
                   END-IF

                   *> Update room status based on remaining active bookings
                   IF ACTIVE-BOOKING-COUNT = 0
                       MOVE "Available" TO R-STATUS
                   END-IF

                   REWRITE ROOMS-RECORD
                       INVALID KEY
                         DISPLAY "Error: Unable to rewrite room record."
                       NOT INVALID KEY
                           DISPLAY "Room ID " WS-ROOM-ID
                                   " updated. Active bookings: "
                                   ACTIVE-BOOKING-COUNT
                   END-REWRITE
           END-READ
           CLOSE ROOMS-FILE.

       CANCEL-EXPIRED-BOOKINGS.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE
           MOVE 0 TO WS-CANCELLED-COUNT
           MOVE 'N' TO WS-EOF

           DISPLAY "Checking for expired bookings..."
           DISPLAY "Current date: " WS-CURRENT-DATE(1:4) "/"
                   WS-CURRENT-DATE(5:2) "/" WS-CURRENT-DATE(7:2)
           DISPLAY " "

           OPEN I-O BOOKING-FILE

           *> Start reading from the beginning of the file
           START BOOKING-FILE KEY IS GREATER THAN OR EQUAL TO BOOKING-ID
           READ BOOKING-FILE NEXT RECORD
               AT END MOVE 'Y' TO WS-EOF
           END-READ

           PERFORM UNTIL WS-EOF = 'Y'
               *> Check if booking is active, not checked in, and past check-in date
             IF BOOKING-STATUS = 'Active'AND CHEKIN-FLAG = 'N'
             AND CHECKIN-DATE < WS-CURRENT-DATE

                   DISPLAY "Cancelling expired booking:"
                   DISPLAY "  Booking ID: " BOOKING-ID
                   DISPLAY "  Room: " ROOM-ID-BK
                   DISPLAY "  Check-in Date: " CHECKIN-DATE(1:4) "/"
                           CHECKIN-DATE(5:2) "/" CHECKIN-DATE(7:2)

                   *> Cancel the booking
                   MOVE "Cancelled" TO BOOKING-STATUS
                   MOVE ROOM-ID-BK TO WS-ROOM-ID
                   REWRITE BOOKING-RECORD
                       INVALID KEY
                           DISPLAY "  Error: Unable to cancel booking "
                           BOOKING-ID
                       NOT INVALID KEY
                           ADD 1 TO WS-CANCELLED-COUNT
                           DISPLAY "  Successfully cancelled booking "
                           BOOKING-ID
                           *> Update the associated room
                           PERFORM UPDATE-ROOM-FOR-CANCELLATION
                   END-REWRITE
               END-IF

               READ BOOKING-FILE NEXT RECORD
                   AT END MOVE 'Y' TO WS-EOF
               END-READ
           END-PERFORM

           CLOSE BOOKING-FILE

           DISPLAY " "
           IF WS-CANCELLED-COUNT = 0
               DISPLAY "No expired bookings found to cancel."
           ELSE
               DISPLAY "Expired bookings cancellation completed."
               DISPLAY "Total bookings cancelled: " WS-CANCELLED-COUNT
           END-IF
           DISPLAY " ".

       UPDATE-ROOM-FOR-CANCELLATION.
           OPEN I-O ROOMS-FILE

           MOVE WS-ROOM-ID TO ROOM-ID
           READ ROOMS-FILE KEY IS ROOM-ID
               INVALID KEY
                   DISPLAY "  Warning: Associated room "
                   WS-ROOM-ID " not found."
               NOT INVALID KEY
                   *> Initialize ACTIVE-BOOKING-COUNT if it contains non-numeric data
                   IF ACTIVE-BOOKING-COUNT NOT NUMERIC
                       MOVE ZERO TO ACTIVE-BOOKING-COUNT
                   END-IF

                   *> Decrease active booking count (with bounds checking)
                   IF ACTIVE-BOOKING-COUNT > 0
                       SUBTRACT 1 FROM ACTIVE-BOOKING-COUNT
                   END-IF

                   *> Update room status based on remaining active bookings
                   IF ACTIVE-BOOKING-COUNT = 0
                       MOVE "Available" TO R-STATUS
                   END-IF

                   REWRITE ROOMS-RECORD
                       INVALID KEY
                           DISPLAY "  Error: Unable to update room "
                           WS-ROOM-ID
                       NOT INVALID KEY
                           DISPLAY "  Updated room " WS-ROOM-ID
                                   " - Active bookings: "
                                   ACTIVE-BOOKING-COUNT
                   END-REWRITE
           END-READ

           CLOSE ROOMS-FILE.

       END PROGRAM cancelBooking.
