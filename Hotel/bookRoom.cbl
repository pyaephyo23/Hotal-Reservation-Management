       IDENTIFICATION DIVISION.
       PROGRAM-ID. bookRoom.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ROOMS-FILE ASSIGN TO '../DATA/ROOMS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ROOM-ID.
           SELECT CUSTOMER-FILE ASSIGN TO '../DATA/CUSTOMERS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUSTOMER-ID
               ALTERNATE RECORD KEY IS CUSTOMER-NAME WITH DUPLICATES.
           SELECT BOOKING-FILE ASSIGN TO '../DATA/BOOKINGS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS BOOKING-ID.
       DATA DIVISION.
       FILE SECTION.
       FD ROOMS-FILE.
       COPY "./CopyBooks/ROOMS.cpy".

       FD CUSTOMER-FILE.
       COPY "./CopyBooks/CUSTOMERS.cpy".

       FD  BOOKING-FILE.
       COPY "./CopyBooks/BOOKINGS.cpy".

       WORKING-STORAGE SECTION.
       01 WS-ROOM-ID         PIC X(5).
       01 WS-FOUND           PIC X VALUE 'N'.
       01 WS-CUSTOMER-ID     PIC 9(5) VALUE ZEROS.
       01 WS-CUSTOMER-NAME   PIC X(30).
       01 WS-CUSTOMER-PHONE  PIC X(15).
       01 WS-CUSTOMER-EMAIL  PIC X(30).
       01 WS-CUSTOMER-ADDR   PIC X(50).
       01 WS-BOOKING-ID      PIC 9(5) VALUE ZEROS.
       01 WS-CHOICE          PIC 9.
       01 WS-VALID-FLAG      PIC X VALUE 'Y'.
       01 WS-TEMP-CHAR       PIC X.
       01 WS-TEMP-INDEX      PIC 9(4).
       01 WS-ROOM-TYPE       PIC X(10).
       01 WS-CONFLICT-FOUND  PIC X VALUE 'N'.
       01 WS-EOF             PIC X VALUE 'N'.
       01 WS-AVAILABLE-COUNT PIC 9(2) VALUE ZEROS.
       01 WS-ROOM-CHOICE     PIC 9(2).
       01 WS-ROOM-ENTRY OCCURS 20 TIMES.
           05 WS-AVAILABLE-ROOM-ID   PIC X(5).
           05 WS-AVAILABLE-ROOM-PRICE PIC 9(9).
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

       *> Current date for validation
       01 WS-CURRENT-DATE-DATA.
           05 WS-CURRENT-DATE.
               10 WS-CURRENT-YEAR     PIC 9(4).
               10 WS-CURRENT-MONTH    PIC 9(2).
               10 WS-CURRENT-DAY      PIC 9(2).
           05 WS-CURRENT-TIME.
               10 WS-CURRENT-HOURS    PIC 9(2).
               10 WS-CURRENT-MINUTES  PIC 9(2).
               10 WS-CURRENT-SECONDS  PIC 9(2).
       01 WS-CURRENT-DATE-NUM     PIC 9(8).
       01 WS-CREATED-AT-TIMESTAMP PIC X(14).

       *> No current date needed
       01 WS-ID-FOUND        PIC X VALUE 'N'.
       01 WS-EXIST-CHOICE    PIC X.
       01 WS-DATE-TO-CHECK   PIC 9(8).
       LINKAGE SECTION.
       01 LINK PIC 9.

       PROCEDURE DIVISION USING LINK.

       MAIN-PAGE.
           DISPLAY "***************************************************"
           DISPLAY "1. Book Room"
           DISPLAY "9. Go back to Main Menu"
           DISPLAY "***************************************************"
           ACCEPT WS-CHOICE
           EVALUATE WS-CHOICE
               WHEN 1
                   PERFORM BOOK-ROOM-PROCESS
                   GO TO MAIN-PAGE
               WHEN 9
                   GOBACK
               WHEN OTHER
                   DISPLAY "Invalid option. Try again."
                   GO TO MAIN-PAGE
           END-EVALUATE.

       BOOK-ROOM-PROCESS.
           *> Step 1: Get booking dates
           PERFORM VALIDATE-CHECKIN-DATE
           PERFORM VALIDATE-CHECKOUT-DATE

           *> Step 2: Get room type preference
           PERFORM VALIDATE-ROOM-TYPE

           *> Step 3: Check for available rooms of that type
           PERFORM CHECK-ROOM-AVAILABILITY

           IF WS-FOUND = 'Y'
               *> Step 4: Get customer information
               PERFORM VALIDATE-CUSTOMER-NAME
               PERFORM HANDLE-CUSTOMER-RECORD

               *> Step 5: Create booking
               PERFORM CREATE-BOOKING

               DISPLAY "========== Booking Completed =========="
               DISPLAY "Booking ID: " WS-BOOKING-ID
               DISPLAY "Room ID:    " WS-ROOM-ID
               DISPLAY "Room Type:  " WS-ROOM-TYPE
               DISPLAY "Customer ID:" WS-CUSTOMER-ID
               DISPLAY "Customer Name: " WS-CUSTOMER-NAME
               DISPLAY "Check-in Date: " WS-CHECKIN-DATE
               DISPLAY "Check-out Date: " WS-CHECKOUT-DATE
               DISPLAY "Created At: " WS-CREATED-AT-TIMESTAMP
               DISPLAY "========================================"
           ELSE
               DISPLAY "No available rooms of type " WS-ROOM-TYPE
                       " for the requested dates."
           END-IF

           PERFORM BOOK-ROOM-RETRY.

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

       VALIDATE-ROOM-TYPE.
           DISPLAY "Select Room Type:"
           DISPLAY "1. Single"
           DISPLAY "2. Double"
           DISPLAY "3. Deluxe"
           DISPLAY "Enter choice (1-3): "
           ACCEPT WS-CHOICE

           EVALUATE WS-CHOICE
               WHEN 1
                   MOVE 'Single' TO WS-ROOM-TYPE
               WHEN 2
                   MOVE 'Double' TO WS-ROOM-TYPE
               WHEN 3
                   MOVE 'Deluxe' TO WS-ROOM-TYPE
               WHEN OTHER
                   DISPLAY "Invalid choice. Please try again."
                   GO TO VALIDATE-ROOM-TYPE
           END-EVALUATE

           DISPLAY "Selected room type: " WS-ROOM-TYPE.

       CHECK-ROOM-AVAILABILITY.
           MOVE 'N' TO WS-FOUND
           MOVE ZEROS TO WS-AVAILABLE-COUNT

           DISPLAY "Checking availability for "
                   FUNCTION TRIM(WS-ROOM-TYPE)
                   " rooms from " WS-CHECKIN-DATE
                   " to " WS-CHECKOUT-DATE "..."

           *> Open rooms file to get all rooms of the requested type
           OPEN INPUT ROOMS-FILE
           MOVE 'N' TO WS-EOF

           PERFORM UNTIL WS-EOF = 'Y'
               READ ROOMS-FILE NEXT
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF ROOM-TYPE = WS-ROOM-TYPE
                           *> Initialize ACTIVE-BOOKING-COUNT if it contains non-numeric data
                           IF ACTIVE-BOOKING-COUNT NOT NUMERIC
                               MOVE ZERO TO ACTIVE-BOOKING-COUNT
                           END-IF
                           
                           *> Only check conflicts if room has active bookings
                           IF ACTIVE-BOOKING-COUNT = 0
                               *> No active bookings, room is available
                               ADD 1 TO WS-AVAILABLE-COUNT
                               MOVE ROOM-ID TO
                                WS-AVAILABLE-ROOM-ID(WS-AVAILABLE-COUNT)
                               MOVE PRICE-PER-NIGHT TO
                             WS-AVAILABLE-ROOM-PRICE(WS-AVAILABLE-COUNT)
                           ELSE
                               *> Check if this room is available during the dates
                               PERFORM CHECK-ROOM-CONFLICTS
                               IF WS-CONFLICT-FOUND = 'N'
                                   ADD 1 TO WS-AVAILABLE-COUNT
                                   MOVE ROOM-ID TO
                                WS-AVAILABLE-ROOM-ID(WS-AVAILABLE-COUNT)
                                   MOVE PRICE-PER-NIGHT TO
                             WS-AVAILABLE-ROOM-PRICE(WS-AVAILABLE-COUNT)
                               END-IF
                           END-IF
                       END-IF
               END-READ
           END-PERFORM
           DISPLAY WS-AVAILABLE-COUNT
           CLOSE ROOMS-FILE

           *> Display available rooms and let user choose
           IF WS-AVAILABLE-COUNT > 0
               PERFORM DISPLAY-AVAILABLE-ROOMS
               PERFORM SELECT-ROOM-FROM-LIST
           ELSE
               MOVE 'N' TO WS-FOUND
           END-IF.

       DISPLAY-AVAILABLE-ROOMS.
           DISPLAY "Available " WS-ROOM-TYPE " rooms:"
           DISPLAY "============================================"
           PERFORM VARYING WS-TEMP-INDEX FROM 1 BY 1
                   UNTIL WS-TEMP-INDEX > WS-AVAILABLE-COUNT
               DISPLAY WS-TEMP-INDEX ". Room "
                       WS-AVAILABLE-ROOM-ID(WS-TEMP-INDEX)
                       " - Price: "
                       WS-AVAILABLE-ROOM-PRICE(WS-TEMP-INDEX)
           END-PERFORM
           DISPLAY "============================================".

       SELECT-ROOM-FROM-LIST.
           DISPLAY "Select a room (1-" WS-AVAILABLE-COUNT "): "
           ACCEPT WS-ROOM-CHOICE

           IF WS-ROOM-CHOICE >= 1
               AND WS-ROOM-CHOICE <= WS-AVAILABLE-COUNT
               MOVE WS-AVAILABLE-ROOM-ID(WS-ROOM-CHOICE) TO WS-ROOM-ID
               MOVE 'Y' TO WS-FOUND
               DISPLAY "Selected room: " WS-ROOM-ID
           ELSE
               DISPLAY "Invalid choice. Please try again."
               GO TO SELECT-ROOM-FROM-LIST
           END-IF.

       CHECK-ROOM-CONFLICTS.
           MOVE 'N' TO WS-CONFLICT-FOUND

           *> Open booking file to check for conflicts
           OPEN INPUT BOOKING-FILE
           MOVE 'N' TO WS-EOF

           PERFORM UNTIL WS-EOF = 'Y'
               READ BOOKING-FILE NEXT
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       *> Check if this booking conflicts with our dates
                  IF ROOM-ID-BK = ROOM-ID AND BOOKING-STATUS = 'Active'
                           *> Check for date overlap
                           IF (WS-CHECKIN-DATE <= CHECKOUT-DATE) AND
                              (WS-CHECKOUT-DATE >= CHECKIN-DATE)
                               MOVE 'Y' TO WS-CONFLICT-FOUND
                           END-IF
                       END-IF
               END-READ
           END-PERFORM

           CLOSE BOOKING-FILE.

       HANDLE-CUSTOMER-RECORD.
           *> Check if customer exists by name
           OPEN INPUT CUSTOMER-FILE
           MOVE 'N' TO WS-ID-FOUND
           MOVE 'N' TO WS-EOF

           PERFORM UNTIL WS-EOF = 'Y'
               READ CUSTOMER-FILE NEXT
                   AT END
                       MOVE 'Y' TO WS-EOF
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
                               MOVE 'Y' TO WS-EOF
                           END-IF
                       END-IF
               END-READ
           END-PERFORM
           CLOSE CUSTOMER-FILE

           IF WS-ID-FOUND NOT = 'Y'
               *> Create new customer
               PERFORM CREATE-NEW-CUSTOMER
           END-IF.

       CREATE-NEW-CUSTOMER.
           DISPLAY "Creating new customer record..."
           PERFORM VALIDATE-CUSTOMER-PHONE
           PERFORM VALIDATE-CUSTOMER-EMAIL
           PERFORM VALIDATE-CUSTOMER-ADDR

           *> Find next customer ID
           OPEN INPUT CUSTOMER-FILE
           MOVE 0 TO WS-CUSTOMER-ID
           MOVE 'N' TO WS-EOF

           PERFORM UNTIL WS-EOF = 'Y'
               READ CUSTOMER-FILE NEXT
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF CUSTOMER-ID > WS-CUSTOMER-ID
                           MOVE CUSTOMER-ID TO WS-CUSTOMER-ID
                       END-IF
               END-READ
           END-PERFORM
           CLOSE CUSTOMER-FILE

           ADD 1 TO WS-CUSTOMER-ID

           *> Create customer record
           MOVE WS-CUSTOMER-ID TO CUSTOMER-ID
           MOVE WS-CUSTOMER-NAME TO CUSTOMER-NAME
           MOVE WS-CUSTOMER-PHONE TO CUSTOMER-PHONE
           MOVE WS-CUSTOMER-EMAIL TO CUSTOMER-EMAIL
           MOVE WS-CUSTOMER-ADDR TO CUSTOMER-ADDR

           OPEN I-O CUSTOMER-FILE
           WRITE CUSTOMER-RECORD
           CLOSE CUSTOMER-FILE

           DISPLAY "New customer created with ID: " WS-CUSTOMER-ID.

       CREATE-BOOKING.
           *> Generate booking ID
           OPEN INPUT BOOKING-FILE
           MOVE 0 TO WS-BOOKING-ID
           MOVE 'N' TO WS-EOF

           PERFORM UNTIL WS-EOF = 'Y'
               READ BOOKING-FILE NEXT
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF BOOKING-ID > WS-BOOKING-ID
                           MOVE BOOKING-ID TO WS-BOOKING-ID
                       END-IF
               END-READ
           END-PERFORM
           CLOSE BOOKING-FILE
           ADD 1 TO WS-BOOKING-ID

           *> Get current date and time for CREATED-AT
           ACCEPT WS-CURRENT-DATE-DATA FROM DATE YYYYMMDD
           STRING WS-CURRENT-YEAR
                  WS-CURRENT-MONTH
                  WS-CURRENT-DAY
                  WS-CURRENT-HOURS
                  WS-CURRENT-MINUTES
                  WS-CURRENT-SECONDS
                  DELIMITED BY SIZE
                  INTO WS-CREATED-AT-TIMESTAMP

           *> Create booking record
           OPEN I-O BOOKING-FILE
           MOVE WS-BOOKING-ID TO BOOKING-ID
           MOVE WS-ROOM-ID TO ROOM-ID-BK
           MOVE WS-CUSTOMER-ID TO CUSTOMER-ID-BK
           MOVE WS-CHECKIN-DATE TO CHECKIN-DATE
           MOVE WS-CHECKOUT-DATE TO CHECKOUT-DATE
           MOVE 'Active' TO BOOKING-STATUS
           MOVE 'N' TO CHEKIN-FLAG
           MOVE 'N' TO CHECKOUT-FLAG
           MOVE WS-CREATED-AT-TIMESTAMP TO CREATED-AT
           WRITE BOOKING-RECORD
           CLOSE BOOKING-FILE

           *> Update room status to Booked and increment active booking count
           OPEN I-O ROOMS-FILE
           MOVE WS-ROOM-ID TO ROOM-ID
           READ ROOMS-FILE KEY IS ROOM-ID
               INVALID KEY
                   DISPLAY "Error updating room status."
               NOT INVALID KEY
                   *> Initialize ACTIVE-BOOKING-COUNT if it contains non-numeric data
                   IF ACTIVE-BOOKING-COUNT NOT NUMERIC
                       MOVE ZERO TO ACTIVE-BOOKING-COUNT
                   END-IF
                   MOVE 'Booked' TO R-STATUS
                   ADD 1 TO ACTIVE-BOOKING-COUNT
                   REWRITE ROOMS-RECORD
           END-READ
           CLOSE ROOMS-FILE.

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

       VALIDATE-CHECKIN-DATE.
           *> Get current date first
           ACCEPT WS-CURRENT-DATE-DATA FROM DATE YYYYMMDD
           MOVE WS-CURRENT-DATE TO WS-CURRENT-DATE-NUM

           DISPLAY "Enter Check-in Date (YYYYMMDD): "
           ACCEPT WS-CHECKIN-DATE
           IF WS-CHECKIN-DATE = ZEROS OR WS-CHECKIN-DATE = SPACES
               DISPLAY "Check-in date cannot be empty."
               GO TO VALIDATE-CHECKIN-DATE
           END-IF
           MOVE WS-CHECKIN-DATE TO WS-DATE-TO-CHECK
           PERFORM VALIDATE-DATE-FORMAT
           IF WS-VALID-FLAG = 'N'
               DISPLAY "Invalid date format. Please use YYYYMMDD."
               GO TO VALIDATE-CHECKIN-DATE
           END-IF

           *> Check if check-in date is not earlier than current date
           IF WS-CHECKIN-DATE < WS-CURRENT-DATE-NUM
               DISPLAY "Check-in date cannot be earlier than today ("
                       WS-CURRENT-DATE-NUM ")."
               GO TO VALIDATE-CHECKIN-DATE
           END-IF.

       VALIDATE-CHECKOUT-DATE.
           DISPLAY "Enter Check-out Date (YYYYMMDD): "
           ACCEPT WS-CHECKOUT-DATE
           IF WS-CHECKOUT-DATE = ZEROS OR WS-CHECKOUT-DATE = SPACES
               DISPLAY "Check-out date cannot be empty."
               GO TO VALIDATE-CHECKOUT-DATE
           END-IF
           MOVE WS-CHECKOUT-DATE TO WS-DATE-TO-CHECK
           PERFORM VALIDATE-DATE-FORMAT
           IF WS-VALID-FLAG = 'N'
               DISPLAY "Invalid date format. Please use YYYYMMDD."
               GO TO VALIDATE-CHECKOUT-DATE
           END-IF
           IF WS-CHECKOUT-DATE <= WS-CHECKIN-DATE
               DISPLAY "Check-out date must be after check-in date."
               GO TO VALIDATE-CHECKOUT-DATE
           END-IF.

       VALIDATE-DATE-FORMAT.
           MOVE 'Y' TO WS-VALID-FLAG

           *> Check if all characters are numeric
           PERFORM VARYING WS-TEMP-INDEX FROM 1 BY 1
           UNTIL WS-TEMP-INDEX > 8
               MOVE WS-DATE-TO-CHECK(WS-TEMP-INDEX:1) TO WS-TEMP-CHAR
               IF WS-TEMP-CHAR NOT NUMERIC
                   MOVE 'N' TO WS-VALID-FLAG
                   EXIT PERFORM
               END-IF
           END-PERFORM

           *> Basic range validation (not comprehensive)
           IF WS-VALID-FLAG = 'Y'
            EVALUATE WS-DATE-TO-CHECK(5:2)
             WHEN '01' WHEN '02' WHEN '03' WHEN '04' WHEN '05' WHEN '06'
             WHEN '07' WHEN '08' WHEN '09' WHEN '10' WHEN '11' WHEN '12'
                       CONTINUE
                   WHEN OTHER
                       MOVE 'N' TO WS-VALID-FLAG
               END-EVALUATE

               EVALUATE WS-DATE-TO-CHECK(7:2)
                   WHEN '01' THRU '31'
                       CONTINUE
                   WHEN OTHER
                       MOVE 'N' TO WS-VALID-FLAG
               END-EVALUATE
           END-IF.

       END PROGRAM bookRoom.
