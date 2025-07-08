       IDENTIFICATION DIVISION.
       PROGRAM-ID. bookRoom.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ROOMS-FILE ASSIGN TO '../DATA/ROOMS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ROOM-ID.
           SELECT CUSTOMER-FILE ASSIGN TO '../DATA/CUSTOMER.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUSTOMER-ID.
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

       FD  CUSTOMER-FILE.
       01  CUSTOMER-RECORD.
           05 CUSTOMER-ID     PIC 9(6).
           05 CUSTOMER-NAME   PIC X(30).
           05 CUSTOMER-PHONE  PIC X(15).
           05 CUSTOMER-EMAIL  PIC X(30).
           05 CUSTOMER-ADDR   PIC X(50).

       FD  BOOKING-FILE.
       01  BOOKING-RECORD.
           05 BOOKING-ID      PIC 9(6).
           05 ROOM-ID-BK      PIC X(5).
           05 CUSTOMER-ID-BK  PIC 9(6).
           05 CHECKIN-DATE    PIC X(8).
           05 CHECKOUT-DATE   PIC X(8).
           05 BOOKING-STATUS  PIC X(10).

       WORKING-STORAGE SECTION.
       01 WS-ROOM-ID         PIC X(5).
       01 WS-FOUND           PIC X VALUE 'N'.
       01 WS-CUSTOMER-ID     PIC 9(6).
       01 WS-CUSTOMER-NAME   PIC X(30).
       01 WS-CUSTOMER-PHONE  PIC X(15).
       01 WS-CUSTOMER-EMAIL  PIC X(30).
       01 WS-CUSTOMER-ADDR   PIC X(50).
       01 WS-BOOKING-ID      PIC 9(6).
       01 WS-CHECKIN-DATE    PIC 9(8).
       01 WS-CHECKOUT-DATE   PIC 9(8).
       01 WS-CHOICE          PIC 9.
       01 WS-VALID-FLAG      PIC X VALUE 'Y'.
       01 WS-TEMP-CHAR       PIC X.
       01 WS-TEMP-INDEX      PIC 9(4).

       PROCEDURE DIVISION.

       MAIN-PAGE.
           DISPLAY "***************************************************"
           DISPLAY "1. Book Room"
           DISPLAY "9. Exit"
           DISPLAY "***************************************************"
           ACCEPT WS-CHOICE
           EVALUATE WS-CHOICE
               WHEN 1
                   PERFORM BOOK-ROOM-PROCESS
                   GO TO MAIN-PAGE
               WHEN 9
                   STOP RUN
               WHEN OTHER
                   DISPLAY "Invalid option. Try again."
                   GO TO MAIN-PAGE
           END-EVALUATE.

       BOOK-ROOM-PROCESS.
           MOVE 'N' TO WS-FOUND
           PERFORM VALIDATE-ROOM-ID

           OPEN I-O ROOMS-FILE
           PERFORM UNTIL WS-FOUND = 'Y'
               READ ROOMS-FILE NEXT
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       IF ROOM-ID = WS-ROOM-ID
                           IF R-STATUS = 'Available'
                            DISPLAY "Room " WS-ROOM-ID " is AVAILABLE."
                               MOVE 'Y' TO WS-FOUND
                               PERFORM BOOK-ROOM
                           ELSE
                        DISPLAY "Room " WS-ROOM-ID " is NOT AVAILABLE."
                               PERFORM BOOK-ROOM-RETRY
                           END-IF
                       END-IF
               END-READ
           END-PERFORM
           CLOSE ROOMS-FILE

           IF WS-FOUND NOT = 'Y'
               DISPLAY "Room ID " WS-ROOM-ID " not found."
               PERFORM BOOK-ROOM-RETRY
           END-IF.

       BOOK-ROOM-RETRY.
           DISPLAY "***************************************************"
           DISPLAY "1. Book Again"
           DISPLAY "9. Exit"
           DISPLAY "***************************************************"
           ACCEPT WS-CHOICE
           EVALUATE WS-CHOICE
               WHEN 1
                   PERFORM BOOK-ROOM-PROCESS
               WHEN 9
                   STOP RUN
               WHEN OTHER
                   DISPLAY "Invalid option. Try again."
                   PERFORM BOOK-ROOM-RETRY
           END-EVALUATE.

       BOOK-ROOM.
           PERFORM VALIDATE-CUSTOMER-ID
           PERFORM VALIDATE-CUSTOMER-NAME
           PERFORM VALIDATE-CUSTOMER-PHONE
           PERFORM VALIDATE-CUSTOMER-EMAIL
           PERFORM VALIDATE-CUSTOMER-ADDR

           OPEN I-O CUSTOMER-FILE
           MOVE WS-CUSTOMER-ID TO CUSTOMER-ID
           MOVE WS-CUSTOMER-NAME TO CUSTOMER-NAME
           MOVE WS-CUSTOMER-PHONE TO CUSTOMER-PHONE
           MOVE WS-CUSTOMER-EMAIL TO CUSTOMER-EMAIL
           MOVE WS-CUSTOMER-ADDR TO CUSTOMER-ADDR
           WRITE CUSTOMER-RECORD
           CLOSE CUSTOMER-FILE

           PERFORM VALIDATE-BOOKING-ID

           OPEN I-O BOOKING-FILE
           MOVE WS-BOOKING-ID TO BOOKING-ID
           MOVE WS-ROOM-ID TO ROOM-ID-BK
           MOVE WS-CUSTOMER-ID TO CUSTOMER-ID-BK
           MOVE '00000000' TO CHECKIN-DATE
           MOVE '00000000' TO CHECKOUT-DATE
           MOVE 'Active' TO BOOKING-STATUS
           WRITE BOOKING-RECORD
           CLOSE BOOKING-FILE

           *> Update room status to 'Booked'
           MOVE WS-ROOM-ID TO ROOM-ID
           READ ROOMS-FILE KEY IS ROOM-ID
               INVALID KEY
                   DISPLAY "Error: Unable to update room status."
               NOT INVALID KEY
                   MOVE 'Booked' TO R-STATUS
                   REWRITE ROOMS-RECORD
           END-READ
           CLOSE ROOMS-FILE

           DISPLAY "Booking completed for Room " WS-ROOM-ID
           DISPLAY "Customer Name: " WS-CUSTOMER-NAME
           PERFORM BOOK-ROOM-RETRY
           STOP RUN.

      *-------------------------
      * VALIDATION ROUTINES
      *-------------------------

       VALIDATE-ROOM-ID.
           DISPLAY "Enter Room ID to check availability: "
           ACCEPT WS-ROOM-ID
           IF FUNCTION TRIM(WS-ROOM-ID) = SPACE
               DISPLAY "Room ID cannot be empty."
               GO TO VALIDATE-ROOM-ID
           END-IF.


       VALIDATE-CUSTOMER-ID.
           DISPLAY "Enter Customer ID: "
           ACCEPT WS-CUSTOMER-ID
           IF FUNCTION TRIM(WS-CUSTOMER-ID) = SPACE
               DISPLAY "Customer ID cannot be empty."
               GO TO VALIDATE-CUSTOMER-ID
           END-IF.

       VALIDATE-CUSTOMER-NAME.
           DISPLAY "Enter Customer Name: "
           ACCEPT WS-CUSTOMER-NAME
           IF FUNCTION TRIM(WS-CUSTOMER-NAME) = SPACE
               DISPLAY "Name cannot be empty."
               GO TO VALIDATE-CUSTOMER-NAME
           END-IF.

       VALIDATE-CUSTOMER-PHONE.
           DISPLAY "Enter Customer Phone: "
           ACCEPT WS-CUSTOMER-PHONE
           IF FUNCTION TRIM(WS-CUSTOMER-PHONE) = SPACE
               DISPLAY "Phone number cannot be empty."
               GO TO VALIDATE-CUSTOMER-PHONE
           END-IF.


       VALIDATE-CUSTOMER-EMAIL.
           DISPLAY "Enter Customer Email: "
           ACCEPT WS-CUSTOMER-EMAIL
           IF FUNCTION TRIM(WS-CUSTOMER-EMAIL) = SPACE
               DISPLAY "Email  cannot be empty."
               GO TO VALIDATE-CUSTOMER-PHONE
           END-IF.

       VALIDATE-CUSTOMER-ADDR.
           DISPLAY "Enter Customer Address: "
           ACCEPT WS-CUSTOMER-ADDR
           IF FUNCTION TRIM(WS-CUSTOMER-ADDR) = SPACE
               DISPLAY "Address cannot be empty."
               GO TO VALIDATE-CUSTOMER-ADDR
           END-IF.

       VALIDATE-BOOKING-ID.
           DISPLAY "Enter Booking ID: "
           ACCEPT WS-BOOKING-ID
           IF FUNCTION TRIM(WS-BOOKING-ID) = SPACE
               DISPLAY "Booking ID cannot be empty."
               GO TO VALIDATE-BOOKING-ID
           END-IF.

       END PROGRAM bookRoom.
