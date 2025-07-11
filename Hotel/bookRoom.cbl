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
       FD ROOMS-FILE.
       01 ROOMS-RECORD.
           05 ROOM-ID             PIC X(5).
           05 ROOM-TYPE           PIC X(10).
           05 PRICE-PER-NIGHT     PIC 9(9).
           05 R-STATUS            PIC X(10).

       FD CUSTOMER-FILE.
       01 CUSTOMER-RECORD.
           05 CUSTOMER-ID     PIC 9(5).
           05 CUSTOMER-NAME   PIC X(30).
           05 CUSTOMER-PHONE  PIC X(15).
           05 CUSTOMER-EMAIL  PIC X(30).
           05 CUSTOMER-ADDR   PIC X(50).

       FD BOOKING-FILE.
       01 BOOKING-RECORD.
           05 BOOKING-ID      PIC 9(5).
           05 ROOM-ID-BK      PIC X(5).
           05 CUSTOMER-ID-BK  PIC 9(6).
           05 CHECKIN-DATE    PIC X(8).
           05 CHECKOUT-DATE   PIC X(8).
           05 BOOKING-STATUS  PIC X(10).

       WORKING-STORAGE SECTION.
       01 WS-ROOM-ID         PIC X(5).
       01 WS-FOUND           PIC X VALUE 'N'.
       01 WS-CUSTOMER-ID     PIC 9(5) VALUE ZEROS.
       01 WS-CUSTOMER-NAME   PIC X(30).
       01 WS-CUSTOMER-PHONE  PIC X(15).
       01 WS-CUSTOMER-EMAIL  PIC X(30).
       01 WS-CUSTOMER-ADDR   PIC X(50).
       01 WS-BOOKING-ID      PIC 9(5) VALUE ZEROS.
       01 WS-CHECKIN-DATE    PIC X(8) VALUE "00000000".
       01 WS-CHECKOUT-DATE   PIC X(8) VALUE "00000000".
       01 WS-CHOICE          PIC 9.
       01 WS-VALID-FLAG      PIC X VALUE 'Y'.
       01 WS-TEMP-CHAR       PIC X.
       01 WS-TEMP-INDEX      PIC 9(4).
       01 WS-ID-FOUND        PIC X VALUE 'N'.
       01 WS-EXIST-CHOICE    PIC X.

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
                               DISPLAY "Room Details:"
                               DISPLAY "ID:       " ROOM-ID
                               DISPLAY "Type:     " ROOM-TYPE
                               DISPLAY "Price:    " PRICE-PER-NIGHT
                               DISPLAY "Status:   " R-STATUS
                               DISPLAY "Room is AVAILABLE."
                               MOVE 'Y' TO WS-FOUND
                               PERFORM BOOK-ROOM
                               CLOSE ROOMS-FILE
                           ELSE
                         DISPLAY "Room " WS-ROOM-ID " is NOT AVAILABLE."
                               CLOSE ROOMS-FILE
                               PERFORM BOOK-ROOM-RETRY
                           END-IF
                       END-IF
               END-READ
           END-PERFORM

           IF WS-FOUND NOT = 'Y'
               CLOSE ROOMS-FILE
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
           PERFORM VALIDATE-CUSTOMER-NAME

           *> Check if customer exists by name
           OPEN INPUT CUSTOMER-FILE
           MOVE 'N' TO WS-ID-FOUND
           PERFORM UNTIL WS-ID-FOUND = 'Y'
               READ CUSTOMER-FILE NEXT
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       IF CUSTOMER-NAME = WS-CUSTOMER-NAME
                       DISPLAY "Customer exists with following details:"
                           DISPLAY "ID: " CUSTOMER-ID
                           DISPLAY "Phone: " CUSTOMER-PHONE
                           DISPLAY "Email: " CUSTOMER-EMAIL
                           DISPLAY "Address: " CUSTOMER-ADDR
                           DISPLAY "Use this customer? (Y/N): "
                           ACCEPT WS-EXIST-CHOICE
                       IF WS-EXIST-CHOICE = 'Y' OR WS-EXIST-CHOICE = 'y'
                              MOVE CUSTOMER-ID TO WS-CUSTOMER-ID
                               MOVE 'Y' TO WS-ID-FOUND
                           ELSE
             DISPLAY "Do you want to enter a different name? (Y/N): "
                             ACCEPT WS-EXIST-CHOICE
                    IF WS-EXIST-CHOICE = 'Y' OR WS-EXIST-CHOICE = 'y'
                        CLOSE CUSTOMER-FILE
                                 GO TO BOOK-ROOM
                             ELSE
                                 DISPLAY "Booking cancelled."
                                 CLOSE CUSTOMER-FILE
                                 CLOSE ROOMS-FILE
                                 PERFORM BOOK-ROOM-RETRY
                             END-IF
                       END-IF
               END-READ
           END-PERFORM
           CLOSE CUSTOMER-FILE

           IF WS-ID-FOUND NOT = 'Y'
               *> Create new customer
               PERFORM VALIDATE-CUSTOMER-PHONE
               PERFORM VALIDATE-CUSTOMER-EMAIL
               PERFORM VALIDATE-CUSTOMER-ADDR

               OPEN INPUT CUSTOMER-FILE
               MOVE 0 TO WS-CUSTOMER-ID
               PERFORM UNTIL WS-ID-FOUND = 'Y'
                   READ CUSTOMER-FILE NEXT
                       AT END
                           MOVE 'Y' TO WS-ID-FOUND
                       NOT AT END
                           IF CUSTOMER-ID > WS-CUSTOMER-ID
                               MOVE CUSTOMER-ID TO WS-CUSTOMER-ID
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE CUSTOMER-FILE
               ADD 1 TO WS-CUSTOMER-ID
               MOVE WS-CUSTOMER-ID TO CUSTOMER-ID
               MOVE WS-CUSTOMER-NAME TO CUSTOMER-NAME
               MOVE WS-CUSTOMER-PHONE TO CUSTOMER-PHONE
               MOVE WS-CUSTOMER-EMAIL TO CUSTOMER-EMAIL
               MOVE WS-CUSTOMER-ADDR TO CUSTOMER-ADDR

               OPEN I-O CUSTOMER-FILE
               WRITE CUSTOMER-RECORD
               CLOSE CUSTOMER-FILE
           END-IF

           *> Generate booking ID
           OPEN I-O BOOKING-FILE
           MOVE 0 TO WS-BOOKING-ID
           MOVE 'Y' TO WS-ID-FOUND
           PERFORM UNTIL WS-ID-FOUND = 'N'
               READ BOOKING-FILE NEXT
                   AT END
                       MOVE 'N' TO WS-ID-FOUND
                   NOT AT END
                       IF BOOKING-ID > WS-BOOKING-ID
                           MOVE BOOKING-ID TO WS-BOOKING-ID
                       END-IF
               END-READ
           END-PERFORM
           ADD 1 TO WS-BOOKING-ID
           MOVE WS-BOOKING-ID TO BOOKING-ID
           MOVE WS-ROOM-ID TO ROOM-ID-BK
           MOVE WS-CUSTOMER-ID TO CUSTOMER-ID-BK
           MOVE WS-CHECKIN-DATE TO CHECKIN-DATE
           MOVE WS-CHECKOUT-DATE TO CHECKOUT-DATE
           MOVE 'Active' TO BOOKING-STATUS
           WRITE BOOKING-RECORD
           CLOSE BOOKING-FILE

           *> Update room status
           MOVE WS-ROOM-ID TO ROOM-ID
           READ ROOMS-FILE KEY IS ROOM-ID
               INVALID KEY
                   DISPLAY "Error updating room status."
               NOT INVALID KEY
                   MOVE 'Booked' TO R-STATUS
                   REWRITE ROOMS-RECORD
           END-READ
           CLOSE ROOMS-FILE

           DISPLAY "========== Booking Completed =========="
           DISPLAY "Booking ID: " WS-BOOKING-ID
           DISPLAY "Room ID:    " WS-ROOM-ID
           DISPLAY "Customer ID:" WS-CUSTOMER-ID
           DISPLAY "Customer Name: " WS-CUSTOMER-NAME
           DISPLAY "========================================"

           PERFORM BOOK-ROOM-RETRY
           STOP RUN.

       VALIDATE-ROOM-ID.
           DISPLAY "Enter Room ID: "
           ACCEPT WS-ROOM-ID
           IF WS-ROOM-ID = SPACES
               DISPLAY "Room ID cannot be empty."
               GO TO VALIDATE-ROOM-ID
           END-IF.

       VALIDATE-CUSTOMER-NAME.
           DISPLAY "Enter Customer Name: "
           ACCEPT WS-CUSTOMER-NAME
           IF WS-CUSTOMER-NAME = SPACES
               DISPLAY "Customer Name cannot be empty."
               GO TO VALIDATE-CUSTOMER-NAME
           END-IF.

       VALIDATE-CUSTOMER-PHONE.
           DISPLAY "Enter Customer Phone: "
           ACCEPT WS-CUSTOMER-PHONE
           IF WS-CUSTOMER-PHONE = SPACES
               DISPLAY "Customer Phone cannot be empty."
               GO TO VALIDATE-CUSTOMER-PHONE
           END-IF.

       VALIDATE-CUSTOMER-EMAIL.
           DISPLAY "Enter Customer Email: "
           ACCEPT WS-CUSTOMER-EMAIL
           IF WS-CUSTOMER-EMAIL = SPACES
               DISPLAY "Customer Email cannot be empty."
               GO TO VALIDATE-CUSTOMER-EMAIL
           END-IF.

       VALIDATE-CUSTOMER-ADDR.
           DISPLAY "Enter Customer Address: "
           ACCEPT WS-CUSTOMER-ADDR
           IF WS-CUSTOMER-ADDR = SPACES
               DISPLAY "Customer Address cannot be empty."
               GO TO VALIDATE-CUSTOMER-ADDR
           END-IF.

       END PROGRAM bookRoom.
