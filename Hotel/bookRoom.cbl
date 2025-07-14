       IDENTIFICATION DIVISION.
       PROGRAM-ID. bookRoom.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ROOMS-FILE ASSIGN TO '../DATA/ROOMS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ROOM-ID
               FILE STATUS IS WS-FILE-STATUS.
           SELECT CUSTOMER-FILE ASSIGN TO '../DATA/CUSTOMER.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUSTOMER-ID
               ALTERNATE RECORD KEY IS CUSTOMER-NAME WITH DUPLICATES
               FILE STATUS IS WS-FILE-STATUS.
           SELECT BOOKING-FILE ASSIGN TO '../DATA/BOOKING.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS BOOKING-ID
               FILE STATUS IS WS-FILE-STATUS.

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
           05 CUSTOMER-ID     PIC 9(5).
           05 CUSTOMER-NAME   PIC X(30).
           05 CUSTOMER-PHONE  PIC X(15).
           05 CUSTOMER-EMAIL  PIC X(30).
           05 CUSTOMER-ADDR   PIC X(50).

       FD  BOOKING-FILE.
       01  BOOKING-RECORD.
           05 BOOKING-ID      PIC 9(5).
           05 ROOM-ID-BK      PIC X(5).
           05 CUSTOMER-ID-BK  PIC 9(5).
           05 CHECKIN-DATE    PIC X(8).
           05 CHECKOUT-DATE   PIC X(8).
           05 BOOKING-STATUS  PIC X(10).

       WORKING-STORAGE SECTION.
       01 WS-ROOM-ID         PIC X(5).
       01 WS-FOUND           PIC X VALUE 'N'.
       01 WS-CUSTOMER-ID     PIC 9(5).
       01 WS-CUSTOMER-NAME   PIC X(30).
       01 WS-CUSTOMER-PHONE  PIC X(15).
       01 WS-CUSTOMER-EMAIL  PIC X(30).
       01 WS-CUSTOMER-ADDR   PIC X(50).
       01 WS-BOOKING-ID      PIC 9(5).
       01 WS-CHOICE          PIC 9.
       01 WS-VALID-FLAG      PIC X VALUE 'Y'.
       01 WS-TEMP-CHAR       PIC X.
       01 WS-TEMP-INDEX      PIC 9(4).

       *> File status
       01 WS-FILE-STATUS     PIC 99.

       *> Auto-increment counters
       01 WS-NEXT-CUSTOMER-ID PIC 9(5).
       01 WS-NEXT-BOOKING-ID  PIC 9(5).
       01 WS-EOF-FLAG         PIC X VALUE 'N'.
       01 WS-EXISTING-CUSTOMER-FLAG PIC X VALUE 'N'.
       *> Simple date fields
       01 WS-CHECKIN-DATE     PIC 9(8) VALUE ZEROS.
       01 WS-CHECKOUT-DATE    PIC 9(8) VALUE ZEROS.

       *> No current date needed

       PROCEDURE DIVISION.

       MAIN-PAGE.
           DISPLAY "***************************************************"
           DISPLAY "          HOTEL ROOM BOOKING SYSTEM"
           DISPLAY "***************************************************"
           DISPLAY "1. Book Room"
           DISPLAY "9. Exit"
           DISPLAY "***************************************************"
           DISPLAY "Enter your choice: " WITH NO ADVANCING
           ACCEPT WS-CHOICE
           EVALUATE WS-CHOICE
               WHEN 1
                   PERFORM BOOK-ROOM-PROCESS
                   GO TO MAIN-PAGE
               WHEN 9
                   GOBACK
               WHEN OTHER
                   DISPLAY "Invalid option. Please try again."
                   GO TO MAIN-PAGE
           END-EVALUATE.

       BOOK-ROOM-PROCESS.
           MOVE 'N' TO WS-FOUND
           PERFORM VALIDATE-ROOM-ID

           OPEN INPUT ROOMS-FILE
           MOVE WS-ROOM-ID TO ROOM-ID
           READ ROOMS-FILE KEY IS ROOM-ID
               INVALID KEY
                   DISPLAY "Room ID " WS-ROOM-ID " not found."
                   CLOSE ROOMS-FILE
                   MOVE 'N' TO WS-FOUND
               NOT INVALID KEY
                   IF R-STATUS = 'Available'
                       DISPLAY
                  "***************************************************"
                       DISPLAY "ROOM DETAILS"
                       DISPLAY
                  "***************************************************"
                       DISPLAY "Room ID: " ROOM-ID
                       DISPLAY "Room Type: " ROOM-TYPE
                       DISPLAY "Price per night: " PRICE-PER-NIGHT
                       DISPLAY "Status: " R-STATUS
                       DISPLAY
                  "***************************************************"
                       MOVE 'Y' TO WS-FOUND
                       CLOSE ROOMS-FILE
                       PERFORM BOOK-ROOM
                   ELSE
                       DISPLAY "Room " WS-ROOM-ID " is NOT AVAILABLE."
                       DISPLAY "Current Status: " R-STATUS
                       CLOSE ROOMS-FILE
                       MOVE 'N' TO WS-FOUND
                   END-IF
           END-READ

           IF WS-FOUND = 'N'
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
                   CONTINUE
               WHEN 9
                   GOBACK
               WHEN OTHER
                   DISPLAY "Invalid option. Try again."
                   GO TO BOOK-ROOM-RETRY
           END-EVALUATE.

       BOOK-ROOM.
           *> Check if customer exists by name first
           PERFORM CHECK-EXISTING-CUSTOMER

           *> If we found an existing customer, use that customer's info
           IF WS-EXISTING-CUSTOMER-FLAG = 'Y'
               DISPLAY "Using existing customer record"
           ELSE
               *> Get additional customer info to create new record
               PERFORM VALIDATE-CUSTOMER-NAME
               PERFORM VALIDATE-CUSTOMER-PHONE
               PERFORM VALIDATE-CUSTOMER-EMAIL
               PERFORM VALIDATE-CUSTOMER-ADDR

               *> Create new customer file if it doesn't exist
               IF WS-FILE-STATUS = 35
                   OPEN OUTPUT CUSTOMER-FILE
                   CLOSE CUSTOMER-FILE
                   OPEN I-O CUSTOMER-FILE
               ELSE
                   OPEN I-O CUSTOMER-FILE
               END-IF

               *> Create new customer record
               MOVE WS-CUSTOMER-ID TO CUSTOMER-ID
               MOVE WS-CUSTOMER-NAME TO CUSTOMER-NAME
               MOVE WS-CUSTOMER-PHONE TO CUSTOMER-PHONE
               MOVE WS-CUSTOMER-EMAIL TO CUSTOMER-EMAIL
               MOVE WS-CUSTOMER-ADDR TO CUSTOMER-ADDR

               WRITE CUSTOMER-RECORD
               END-WRITE
               CLOSE CUSTOMER-FILE
               DISPLAY
               "New customer record created with ID: " WS-CUSTOMER-ID
           END-IF

           PERFORM VALIDATE-BOOKING-ID

           *> Check if file exists, if not create it
           IF WS-FILE-STATUS = '35'
               OPEN OUTPUT BOOKING-FILE
           ELSE
               OPEN I-O BOOKING-FILE
           END-IF

           *> Create the booking record
           MOVE WS-BOOKING-ID TO BOOKING-ID
           MOVE WS-ROOM-ID TO ROOM-ID-BK
           MOVE WS-CUSTOMER-ID TO CUSTOMER-ID-BK
           MOVE ZEROS TO CHECKIN-DATE
           MOVE ZEROS TO CHECKOUT-DATE
           MOVE 'Active' TO BOOKING-STATUS

           WRITE BOOKING-RECORD
           END-WRITE
           CLOSE BOOKING-FILE

           *> Update room status to 'Booked'
           OPEN I-O ROOMS-FILE
           MOVE WS-ROOM-ID TO ROOM-ID
           READ ROOMS-FILE KEY IS ROOM-ID
               INVALID KEY
                   DISPLAY "Error: Unable to update room status."
               NOT INVALID KEY
                   MOVE 'Booked' TO R-STATUS
                   REWRITE ROOMS-RECORD
                       INVALID KEY
                          DISPLAY "Error: Could not update room status."
               END-REWRITE
           END-READ
           CLOSE ROOMS-FILE

           DISPLAY
           "***************************************************"
           DISPLAY "BOOKING CONFIRMATION"
           DISPLAY
           "***************************************************"
           DISPLAY "Booking ID: " WS-BOOKING-ID
           DISPLAY "Room ID: " WS-ROOM-ID
           DISPLAY "Customer ID: " WS-CUSTOMER-ID
           DISPLAY "Customer Name: " WS-CUSTOMER-NAME
           DISPLAY "Status: Active"
           DISPLAY
           "***************************************************".

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
           *> Generate auto-increment customer ID
           MOVE 'N' TO WS-EOF-FLAG
           MOVE 0 TO WS-NEXT-CUSTOMER-ID

           *> Find highest existing customer ID
           OPEN INPUT CUSTOMER-FILE

           *> Check if file opened successfully (35 = file not found)
           IF WS-FILE-STATUS = 35
               *> File doesn't exist - use default starting ID
               MOVE 10001 TO WS-CUSTOMER-ID
               MOVE 'Y' TO WS-EOF-FLAG
           ELSE
               PERFORM UNTIL WS-EOF-FLAG = 'Y'
                  READ CUSTOMER-FILE NEXT RECORD
                      AT END
                         MOVE 'Y' TO WS-EOF-FLAG
                      NOT AT END
                         IF CUSTOMER-ID >= WS-NEXT-CUSTOMER-ID
                            MOVE CUSTOMER-ID TO WS-NEXT-CUSTOMER-ID
                         END-IF
                   END-READ
               END-PERFORM
               CLOSE CUSTOMER-FILE

               *> Set next ID (highest + 1)
               IF WS-NEXT-CUSTOMER-ID = 0
                   MOVE 10001 TO WS-CUSTOMER-ID
               ELSE
                   ADD 1 TO WS-NEXT-CUSTOMER-ID
                   MOVE WS-NEXT-CUSTOMER-ID TO WS-CUSTOMER-ID
               END-IF
           END-IF

           DISPLAY "Auto-generated Customer ID: " WS-CUSTOMER-ID.

       CHECK-EXISTING-CUSTOMER.
           DISPLAY "Enter Customer Name: "
           ACCEPT WS-CUSTOMER-NAME

           *> Check if name is empty
           IF FUNCTION TRIM(WS-CUSTOMER-NAME) = SPACE
               DISPLAY "Name cannot be empty."
               GO TO CHECK-EXISTING-CUSTOMER
           END-IF

           *> First check if customer file exists or has any records
           MOVE 'N' TO WS-EXISTING-CUSTOMER-FLAG
           MOVE 'N' TO WS-EOF-FLAG

           *> Try to open the file
           OPEN INPUT CUSTOMER-FILE

           *> Check if file opened successfully (35 = file not found)
           IF WS-FILE-STATUS = 35
               *> File doesn't exist - create a new customer record
               MOVE 'Y' TO WS-EOF-FLAG
               DISPLAY
               "Customer file doesn't exist. Creating new customer."
               PERFORM VALIDATE-CUSTOMER-ID
           ELSE
               *> File exists, search for customer by name
               PERFORM UNTIL WS-EOF-FLAG = 'Y'
                   READ CUSTOMER-FILE NEXT RECORD
                       AT END
                           MOVE 'Y' TO WS-EOF-FLAG
                       NOT AT END
                           IF CUSTOMER-NAME = WS-CUSTOMER-NAME
                               MOVE 'Y' TO WS-EXISTING-CUSTOMER-FLAG
                               MOVE CUSTOMER-ID TO WS-CUSTOMER-ID
                               MOVE CUSTOMER-PHONE TO WS-CUSTOMER-PHONE
                               MOVE CUSTOMER-EMAIL TO WS-CUSTOMER-EMAIL
                               MOVE CUSTOMER-ADDR TO WS-CUSTOMER-ADDR
                               DISPLAY "Found existing customer ID: "
                                   WS-CUSTOMER-ID
                               MOVE 'Y' TO WS-EOF-FLAG
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE CUSTOMER-FILE
           END-IF.

       VALIDATE-CUSTOMER-NAME.
           *> We already have the customer name from CHECK-EXISTING-CUSTOMER
           IF FUNCTION TRIM(WS-CUSTOMER-NAME) = SPACE
               DISPLAY "Name cannot be empty."
               DISPLAY "Enter Customer Name again: "
               ACCEPT WS-CUSTOMER-NAME
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
               DISPLAY "Email cannot be empty."
               GO TO VALIDATE-CUSTOMER-EMAIL
           END-IF.

       VALIDATE-CUSTOMER-ADDR.
           DISPLAY "Enter Customer Address: "
           ACCEPT WS-CUSTOMER-ADDR
           IF FUNCTION TRIM(WS-CUSTOMER-ADDR) = SPACE
               DISPLAY "Address cannot be empty."
               GO TO VALIDATE-CUSTOMER-ADDR
           END-IF.

       VALIDATE-BOOKING-ID.
           *> Generate auto-increment booking ID
           MOVE 'N' TO WS-EOF-FLAG
           MOVE 0 TO WS-NEXT-BOOKING-ID

           *> Find highest existing booking ID
           OPEN INPUT BOOKING-FILE

           *> Check if file opened successfully (35 = file not found)
           IF WS-FILE-STATUS = '35'
               *> File doesn't exist - use default starting ID
               DISPLAY "Booking file doesn't exist. Using default ID."
               MOVE 10001 TO WS-BOOKING-ID
               MOVE 'Y' TO WS-EOF-FLAG
           ELSE
               PERFORM UNTIL WS-EOF-FLAG = 'Y'
                   READ BOOKING-FILE NEXT RECORD
                       AT END
                           MOVE 'Y' TO WS-EOF-FLAG
                       NOT AT END
                           IF BOOKING-ID >= WS-NEXT-BOOKING-ID
                               MOVE BOOKING-ID TO WS-NEXT-BOOKING-ID
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE BOOKING-FILE

               *> Set next ID (highest + 1)
               IF WS-NEXT-BOOKING-ID = 0
                   MOVE 10001 TO WS-BOOKING-ID
               ELSE
                   ADD 1 TO WS-NEXT-BOOKING-ID
                   MOVE WS-NEXT-BOOKING-ID TO WS-BOOKING-ID
               END-IF
           END-IF

           DISPLAY "Auto-generated Booking ID: " WS-BOOKING-ID.

       END PROGRAM bookRoom.
