       IDENTIFICATION DIVISION.
       PROGRAM-ID. checkIn.
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
           SELECT GUEST-FILE ASSIGN TO '../DATA/GUESTS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS GUEST-ID.
       DATA DIVISION.

       FILE SECTION.
       FD  ROOMS-FILE.
       COPY "./CopyBooks/ROOMS.cpy".

       FD CUSTOMER-FILE.
       COPY "./CopyBooks/CUSTOMERS.cpy".

       FD  BOOKING-FILE.
       COPY "./CopyBooks/BOOKINGS.cpy".

       FD  GUEST-FILE.
       COPY "./CopyBooks/GUESTS.cpy".

       WORKING-STORAGE SECTION.
       01 WS-CHOICE            PIC 9.
       01 WS-BOOKING-ID        PIC 9(5).
       01 WS-CUSTOMER-ID       PIC 9(5).
       01 WS-CUSTOMER-PHONE    PIC X(15).
       01 WS-CUSTOMER-NAME     PIC X(30).
       01 WS-ROOM-ID           PIC X(5).
       01 WS-ROOM-TYPE         PIC X(10).
       01 WS-CHECKOUT-DATE     PIC 9(8).
       01 WS-FOUND             PIC X VALUE 'N'.
       01 WS-EOF               PIC X VALUE 'N'.
       01 WS-CHECKIN-DATE      PIC X(8).
       01 WS-CURRENT-DATE      PIC 9(8).
       01 WS-TIME-FORMATTED    PIC X(6).
       01 WS-CURRENT-TIME-FIELDS.
           05 WS-CURRENT-HOUR    PIC 9(2).
           05 WS-CURRENT-MINUTE  PIC 9(2).
           05 WS-CURRENT-SECOND  PIC 9(2).
       01 WS-BOOKING-COUNT     PIC 9(2) VALUE 0.
       01 WS-BOOKING-CHOICE    PIC 9(2).
       01 WS-ID-FOUND          PIC X VALUE 'N'.
       01 WS-EXIST-CHOICE      PIC X.
       01 WS-BOOKING-ENTRY OCCURS 20 TIMES.
           05 WS-FOUND-BOOKING-ID   PIC 9(5).
           05 WS-FOUND-ROOM-ID      PIC X(5).
           05 WS-FOUND-CHECKIN-DATE PIC 9(8).
           05 WS-FOUND-CHECKOUT-DATE PIC 9(8).
       01 WS-AVAILABLE-COUNT   PIC 9(2) VALUE ZEROS.
       01 WS-ROOM-CHOICE       PIC 9(2).
       01 WS-ROOM-ENTRY OCCURS 20 TIMES.
           05 WS-AVAILABLE-ROOM-ID   PIC X(5).
           05 WS-AVAILABLE-ROOM-TYPE PIC X(10).
           05 WS-AVAILABLE-ROOM-PRICE PIC 9(9).
       01 WS-CREATED-AT-TIMESTAMP PIC X(14).
       01 WS-GUEST-ID          PIC 9(5).
       01 WS-GUEST-NAME        PIC X(20).
       01 WS-GUEST-AGE         PIC 9(3).
       01 WS-GUEST-NRC         PIC X(18).
       01 WS-GUEST-GENDER      PIC X(1).
       01 WS-GUEST-COUNT       PIC 9(2) VALUE ZEROS.
       01 WS-ADD-GUESTS        PIC X VALUE 'N'.
       01 WS-FORMATTED-PRICE   PIC ZZZZZZZZ9.
       01 WS-CURRENT-DATE-DATA.
           05 WS-CURRENT-DATE-PART.
               10 WS-CURRENT-YEAR     PIC 9(4).
               10 WS-CURRENT-MONTH    PIC 9(2).
               10 WS-CURRENT-DAY      PIC 9(2).
           05 WS-CURRENT-TIME-PART.
               10 WS-CURRENT-HOURS    PIC 9(2).
               10 WS-CURRENT-MINUTES  PIC 9(2).
               10 WS-CURRENT-SECONDS  PIC 9(2).

       *> Color codes for display
       01 RED-COLOR          PIC X(8) VALUE X"1B5B33316D".
       01 GREEN-COLOR        PIC X(8) VALUE X"1B5B33326D".
       01 RESET-COLOR        PIC X(4) VALUE X"1B5B306D".
       LINKAGE SECTION.
       01 LINK PIC 9.

       PROCEDURE DIVISION USING LINK.
       MAIN-PROCEDURE.
           DISPLAY "***************************************************"
           DISPLAY "1. Check In by Booking ID"
           DISPLAY "2. Check In by Phone Number"
           DISPLAY "3. Walk-in Check In"
           DISPLAY "9. Return to Main Menu"
           DISPLAY "***************************************************"
           ACCEPT WS-CHOICE
           EVALUATE WS-CHOICE
               WHEN 1
                   DISPLAY "Please enter your Booking ID:"
                   ACCEPT WS-BOOKING-ID
                   PERFORM CHECK-BOOKING-ID-AND-CHECK-IN
                   PERFORM CHANGE-ROOM-STATUS
                   GO TO MAIN-PROCEDURE
               WHEN 2
                   DISPLAY "Please enter your phone number:"
                   ACCEPT WS-CUSTOMER-PHONE
                   DISPLAY " "
                   PERFORM CHECK-IN-BY-PHONE
                   GO TO MAIN-PROCEDURE
               WHEN 3
                   PERFORM WALK-IN-CHECK-IN
                   GO TO MAIN-PROCEDURE
               WHEN 9
                   GOBACK
               WHEN OTHER
                   DISPLAY RED-COLOR "Invalid selection. " RESET-COLOR
                   GO TO MAIN-PROCEDURE
           END-EVALUATE.
           STOP RUN.

       CHECK-BOOKING-ID-AND-CHECK-IN.
           MOVE 'N' TO WS-FOUND.
           OPEN I-O BOOKING-FILE.

           *> Get the current system date
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD.

            *> Get the current system time
           ACCEPT WS-CURRENT-TIME-FIELDS FROM TIME.
           STRING WS-CURRENT-HOUR DELIMITED BY SIZE
                  WS-CURRENT-MINUTE DELIMITED BY SIZE
                  WS-CURRENT-SECOND DELIMITED BY SIZE
                  INTO WS-TIME-FORMATTED.

           MOVE WS-BOOKING-ID TO BOOKING-ID OF BOOKING-RECORD.
           READ BOOKING-FILE
               INVALID KEY
                   DISPLAY RED-COLOR "Booking ID " WS-BOOKING-ID
                   " not found." RESET-COLOR
                   CLOSE BOOKING-FILE
                   GO TO MAIN-PROCEDURE
               NOT INVALID KEY
                   IF BOOKING-STATUS OF BOOKING-RECORD = 'Active'
                       IF CHEKIN-FLAG OF BOOKING-RECORD = 'N'
                           *> Allow early check-in or normal check-in
                           PERFORM CHECK-EARLY-CHECKIN-ALLOWED
                           IF WS-FOUND = 'Y'
                               MOVE 'Y' TO CHEKIN-FLAG OF BOOKING-RECORD
                               REWRITE BOOKING-RECORD
                                   INVALID KEY
                               DISPLAY RED-COLOR
                               "Error processing check-in." RESET-COLOR
                                   NOT INVALID KEY
                                       DISPLAY GREEN-COLOR
                                      "Check-In Completed Successfully!"
                                      RESET-COLOR
                                      DISPLAY " "
                                       DISPLAY "Booking ID: " BOOKING-ID
                                      DISPLAY "Room Number: " ROOM-ID-BK
                                       DISPLAY "Check-in Date: "
                                           CHECKIN-DATE(1:4) "/"
                                           CHECKIN-DATE(5:2) "/"
                                           CHECKIN-DATE(7:2)
                                       DISPLAY "Check-in Time: "
                                           WS-TIME-FORMATTED
                                           DISPLAY " "
                                       *> Add guest information
                                       PERFORM ADD-GUEST-INFORMATION
                               END-REWRITE
                           END-IF
                       ELSE
                           DISPLAY RED-COLOR "Booking ID "
                    WS-BOOKING-ID " is already checked in." RESET-COLOR
                           DISPLAY "Status: Already Checked In"
                           CLOSE BOOKING-FILE
                           GO TO MAIN-PROCEDURE
                       END-IF
                   ELSE
                       DISPLAY RED-COLOR "Booking ID " WS-BOOKING-ID
                       " is not active." RESET-COLOR
                       DISPLAY "Current status: "
                       BOOKING-STATUS OF BOOKING-RECORD
                       CLOSE BOOKING-FILE
                       GO TO MAIN-PROCEDURE
                   END-IF
           END-READ.

           CLOSE BOOKING-FILE.

       CHECK-EARLY-CHECKIN-ALLOWED.
           MOVE 'Y' TO WS-FOUND
           *> Check if current date is before scheduled check-in date
           IF FUNCTION INTEGER-OF-DATE(WS-CURRENT-DATE) <
              FUNCTION INTEGER-OF-DATE(CHECKIN-DATE)
               *> Early check-in requested
               DISPLAY "Early check-in requested."
               DISPLAY "Scheduled check-in: " CHECKIN-DATE(1:4) "/"
                       CHECKIN-DATE(5:2) "/" CHECKIN-DATE(7:2)
               DISPLAY "Current date: " WS-CURRENT-DATE(1:4) "/"
                       WS-CURRENT-DATE(5:2) "/" WS-CURRENT-DATE(7:2)
               *> Check if room is available for early check-in
               PERFORM CHECK-EARLY-CHECKIN
               IF WS-FOUND = 'N'
                   DISPLAY RED-COLOR
                 "Room is not available for early check-in." RESET-COLOR
              DISPLAY "Please check in on or after your scheduled date."
               ELSE
                  DISPLAY GREEN-COLOR
                   "Early check-in approved - room is available."
                   RESET-COLOR
               END-IF
           ELSE
               DISPLAY "Normal check-in on scheduled date."
           END-IF.

       CHECK-EARLY-CHECKIN.
           MOVE 'Y' TO WS-FOUND
           *> Check if room is currently occupied or has conflicting bookings

           MOVE 'N' TO WS-EOF

           PERFORM UNTIL WS-EOF = 'Y'
               READ BOOKING-FILE NEXT
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       *> Check for other active bookings for same room
                       IF ROOM-ID-BK = ROOM-ID-BK OF BOOKING-RECORD
                      AND BOOKING-ID NOT = BOOKING-ID OF BOOKING-RECORD
                          AND BOOKING-STATUS = 'Active'
                          AND CHEKIN-FLAG = 'Y'
                          AND CHECKOUT-FLAG = 'N'
                           *> Room is currently occupied
                           MOVE 'N' TO WS-FOUND
                           MOVE 'Y' TO WS-EOF
                       END-IF
               END-READ
           END-PERFORM.

       CHECK-IN-BY-PHONE.
           *> Find customer by phone number
           PERFORM FIND-CUSTOMER-BY-PHONE
           IF WS-FOUND = 'N'
               DISPLAY RED-COLOR "No guest found with phone number: "
                       WS-CUSTOMER-PHONE RESET-COLOR
               EXIT PARAGRAPH
           END-IF

           *> Find active bookings for this customer
           PERFORM FIND-ACTIVE-CHECKIN
           IF WS-BOOKING-COUNT = 0
              DISPLAY RED-COLOR "No active bookings found for check-in."
               RESET-COLOR
               EXIT PARAGRAPH
           END-IF

           *> Display bookings and let user choose
           PERFORM DISPLAY-CHECKIN-BOOKINGS
           PERFORM SELECT-BOOKING-FOR-CHECKIN.

       FIND-CUSTOMER-BY-PHONE.
           MOVE 'N' TO WS-ID-FOUND
           MOVE 'N' TO WS-EOF

           OPEN INPUT CUSTOMER-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ CUSTOMER-FILE NEXT
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF CUSTOMER-PHONE = WS-CUSTOMER-PHONE
                      DISPLAY "We found your guest profile:"
                           DISPLAY "Guest ID: " CUSTOMER-ID
                           DISPLAY "Name: " CUSTOMER-NAME
                           DISPLAY "Phone: " CUSTOMER-PHONE
                           DISPLAY "Use this profile? (Y/N): "
                           ACCEPT WS-EXIST-CHOICE
                           DISPLAY " "

                       IF WS-EXIST-CHOICE = 'Y' OR WS-EXIST-CHOICE = 'y'
                               MOVE CUSTOMER-ID TO WS-CUSTOMER-ID
                               MOVE CUSTOMER-NAME TO WS-CUSTOMER-NAME
                               MOVE 'Y' TO WS-ID-FOUND
                               MOVE 'Y' TO WS-EOF
                           END-IF
                       END-IF
               END-READ
           END-PERFORM
           CLOSE CUSTOMER-FILE

           IF WS-ID-FOUND = 'Y'
               MOVE 'Y' TO WS-FOUND
           ELSE
               MOVE 'N' TO WS-FOUND
           END-IF.

       FIND-ACTIVE-CHECKIN.
           MOVE 0 TO WS-BOOKING-COUNT
           MOVE 'N' TO WS-EOF

           OPEN INPUT BOOKING-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ BOOKING-FILE NEXT
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF CUSTOMER-ID-BK = WS-CUSTOMER-ID
                          AND BOOKING-STATUS = 'Active'
                          AND CHEKIN-FLAG = 'N'
                           ADD 1 TO WS-BOOKING-COUNT
                           IF WS-BOOKING-COUNT <= 20
                               MOVE BOOKING-ID TO
                                   WS-FOUND-BOOKING-ID(WS-BOOKING-COUNT)
                               MOVE ROOM-ID-BK TO
                                   WS-FOUND-ROOM-ID(WS-BOOKING-COUNT)
                               MOVE CHECKIN-DATE TO
                                 WS-FOUND-CHECKIN-DATE(WS-BOOKING-COUNT)
                               MOVE CHECKOUT-DATE TO
                                WS-FOUND-CHECKOUT-DATE(WS-BOOKING-COUNT)
                           END-IF
                       END-IF
               END-READ
           END-PERFORM
           CLOSE BOOKING-FILE.

       DISPLAY-CHECKIN-BOOKINGS.
           DISPLAY "Active bookings for check-in:"
           DISPLAY "=================================================="
           PERFORM VARYING WS-BOOKING-CHOICE FROM 1 BY 1
                   UNTIL WS-BOOKING-CHOICE > WS-BOOKING-COUNT
               DISPLAY WS-BOOKING-CHOICE ". Booking ID: "
                       WS-FOUND-BOOKING-ID(WS-BOOKING-CHOICE)
                       " | Room: "
                       WS-FOUND-ROOM-ID(WS-BOOKING-CHOICE)
               DISPLAY "   Check-in: "
                   WS-FOUND-CHECKIN-DATE(WS-BOOKING-CHOICE)(1:4) "/"
                   WS-FOUND-CHECKIN-DATE(WS-BOOKING-CHOICE)(5:2) "/"
                   WS-FOUND-CHECKIN-DATE(WS-BOOKING-CHOICE)(7:2)
                       " | Check-out: "
                   WS-FOUND-CHECKOUT-DATE(WS-BOOKING-CHOICE)(1:4) "/"
                   WS-FOUND-CHECKOUT-DATE(WS-BOOKING-CHOICE)(5:2) "/"
                   WS-FOUND-CHECKOUT-DATE(WS-BOOKING-CHOICE)(7:2)
           END-PERFORM
           DISPLAY "==================================================".


       SELECT-BOOKING-FOR-CHECKIN.
           DISPLAY "Please select a booking to check in (1-"
            WS-BOOKING-COUNT
                   ") or 0 to cancel: "
           ACCEPT WS-BOOKING-CHOICE
           DISPLAY " "

           IF WS-BOOKING-CHOICE = 0
               DISPLAY RED-COLOR "Check-in cancelled." RESET-COLOR
           ELSE IF WS-BOOKING-CHOICE >= 1
               AND WS-BOOKING-CHOICE <= WS-BOOKING-COUNT
               MOVE WS-FOUND-BOOKING-ID(WS-BOOKING-CHOICE)
                   TO WS-BOOKING-ID
               PERFORM CHECK-BOOKING-ID-AND-CHECK-IN
               PERFORM CHANGE-ROOM-STATUS
           ELSE
               DISPLAY RED-COLOR "Invalid selection. Please try again."
                RESET-COLOR
               GO TO SELECT-BOOKING-FOR-CHECKIN
           END-IF.

       WALK-IN-CHECK-IN.
           DISPLAY "===== Walk-in Guest Check-In ====="

           *> Get customer information
           PERFORM GET-WALKIN-CUSTOMER-INFO

           *> Show available rooms
           PERFORM SHOW-AVAILABLE-ROOMS-FOR-WALKIN
           IF WS-AVAILABLE-COUNT = 0
               DISPLAY RED-COLOR
           "Sorry, no rooms are currently available for walk-in guests."
            RESET-COLOR
               EXIT PARAGRAPH
           END-IF

           *> Let customer select room
           PERFORM SELECT-ROOM-FOR-WALKIN
           IF WS-FOUND = 'N'
               EXIT PARAGRAPH
           END-IF

           *> Get checkout date
           PERFORM GET-WALKIN-CHECKOUT-DATE

           *> Create booking and check in immediately
           PERFORM CREATE-WALKIN-BOOKING
           PERFORM CHANGE-ROOM-STATUS
           *> Add guest information for walk-in
           PERFORM ADD-GUEST-INFORMATION.

       GET-WALKIN-CUSTOMER-INFO.
           DISPLAY "Please enter your phone number: "
           ACCEPT WS-CUSTOMER-PHONE
           DISPLAY " "
           *> Check if customer exists by phone number
           PERFORM FIND-CUSTOMER-BY-PHONE
           IF WS-FOUND = 'N'
               *> Get customer name for new customer
               DISPLAY "Please enter your full name: "
               ACCEPT WS-CUSTOMER-NAME
               DISPLAY " "
               *> Create new customer
               PERFORM CREATE-WALKIN-CUSTOMER
           END-IF.

       CREATE-WALKIN-CUSTOMER.
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

           OPEN I-O CUSTOMER-FILE
           WRITE CUSTOMER-RECORD
           CLOSE CUSTOMER-FILE

           DISPLAY GREEN-COLOR "New guest profile created with ID: "
           WS-CUSTOMER-ID RESET-COLOR.

       SHOW-AVAILABLE-ROOMS-FOR-WALKIN.
           MOVE ZEROS TO WS-AVAILABLE-COUNT
           MOVE 'N' TO WS-EOF

           OPEN INPUT ROOMS-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ ROOMS-FILE NEXT
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF R-STATUS = 'Available'
                           ADD 1 TO WS-AVAILABLE-COUNT
                           IF WS-AVAILABLE-COUNT <= 20
                               MOVE ROOM-ID TO
                                WS-AVAILABLE-ROOM-ID(WS-AVAILABLE-COUNT)
                               MOVE ROOM-TYPE TO
                              WS-AVAILABLE-ROOM-TYPE(WS-AVAILABLE-COUNT)
                               MOVE PRICE-PER-NIGHT TO
                             WS-AVAILABLE-ROOM-PRICE(WS-AVAILABLE-COUNT)
                           END-IF
                       END-IF
               END-READ
           END-PERFORM
           CLOSE ROOMS-FILE

           IF WS-AVAILABLE-COUNT > 0
               DISPLAY "Available rooms for tonight:"
            DISPLAY "=================================================="
               *> Display up to 20 rooms maximum
               IF WS-AVAILABLE-COUNT > 20
                   MOVE 20 TO WS-AVAILABLE-COUNT
                   DISPLAY "Note: Showing first 20 available rooms."
               END-IF
               PERFORM VARYING WS-ROOM-CHOICE FROM 1 BY 1
                       UNTIL WS-ROOM-CHOICE > WS-AVAILABLE-COUNT
                   *> Format price to remove leading zeros
                   MOVE WS-AVAILABLE-ROOM-PRICE(WS-ROOM-CHOICE)
                        TO WS-FORMATTED-PRICE
                   DISPLAY WS-ROOM-CHOICE ". Room "
                           WS-AVAILABLE-ROOM-ID(WS-ROOM-CHOICE)
                           " - " WS-AVAILABLE-ROOM-TYPE(WS-ROOM-CHOICE)
                           " - Rate: $" WS-FORMATTED-PRICE " per night"
               END-PERFORM
            DISPLAY "=================================================="
               DISPLAY "Enter 0 to return to main menu"
           END-IF.

       SELECT-ROOM-FOR-WALKIN.
           DISPLAY "Please select a room (1-" WS-AVAILABLE-COUNT
                   ") or 0 to cancel: "
           ACCEPT WS-ROOM-CHOICE

           IF WS-ROOM-CHOICE = 0
             DISPLAY RED-COLOR "Walk-in check-in cancelled." RESET-COLOR
               MOVE 'N' TO WS-FOUND
           ELSE IF WS-ROOM-CHOICE >= 1
               AND WS-ROOM-CHOICE <= WS-AVAILABLE-COUNT
               MOVE WS-AVAILABLE-ROOM-ID(WS-ROOM-CHOICE) TO WS-ROOM-ID
             MOVE WS-AVAILABLE-ROOM-TYPE(WS-ROOM-CHOICE) TO WS-ROOM-TYPE
               MOVE 'Y' TO WS-FOUND
               DISPLAY GREEN-COLOR "Room " WS-ROOM-ID
               " selected for your stay." RESET-COLOR
           ELSE
               DISPLAY RED-COLOR "Invalid selection. Please try again."
               RESET-COLOR
               GO TO SELECT-ROOM-FOR-WALKIN
           END-IF.

       GET-WALKIN-CHECKOUT-DATE.
           DISPLAY "Please enter your checkout date (YYYYMMDD): "
           ACCEPT WS-CHECKOUT-DATE
           DISPLAY " "
           *> Get current date for validation
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD

           *> Validate checkout date format and value
           IF WS-CHECKOUT-DATE NOT NUMERIC
               DISPLAY RED-COLOR
               "Invalid date format. Please use YYYYMMDD." RESET-COLOR
               GO TO GET-WALKIN-CHECKOUT-DATE
           END-IF

           *> Check if checkout date is after check-in date (today)
           IF FUNCTION INTEGER-OF-DATE(WS-CHECKOUT-DATE) <=
              FUNCTION INTEGER-OF-DATE(WS-CURRENT-DATE)
               DISPLAY RED-COLOR "Check-out date must be after today ("
                       WS-CURRENT-DATE(1:4) "/"
                       WS-CURRENT-DATE(5:2) "/"
                       WS-CURRENT-DATE(7:2) ")." RESET-COLOR
               DISPLAY RED-COLOR "Please enter a valid checkout date."
                RESET-COLOR
               GO TO GET-WALKIN-CHECKOUT-DATE
           END-IF

           *> Additional validation for reasonable date range (max 30 days)
           IF FUNCTION INTEGER-OF-DATE(WS-CHECKOUT-DATE) -
              FUNCTION INTEGER-OF-DATE(WS-CURRENT-DATE) > 30
               DISPLAY RED-COLOR
               "Checkout date cannot be more than 30 days from today."
               RESET-COLOR
               DISPLAY RED-COLOR
                "Please enter a reasonable checkout date." RESET-COLOR
               GO TO GET-WALKIN-CHECKOUT-DATE
           END-IF

           DISPLAY GREEN-COLOR "Checkout date accepted: "
            WS-CHECKOUT-DATE(1:4) "/"
                   WS-CHECKOUT-DATE(5:2) "/" WS-CHECKOUT-DATE(7:2)
                   RESET-COLOR
                       DISPLAY " ".

       CREATE-WALKIN-BOOKING.
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

           *> Get current date for check-in and timestamp
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           ACCEPT WS-CURRENT-DATE-DATA FROM DATE YYYYMMDD
           STRING WS-CURRENT-YEAR
                  WS-CURRENT-MONTH
                  WS-CURRENT-DAY
                  WS-CURRENT-HOURS
                  WS-CURRENT-MINUTES
                  WS-CURRENT-SECONDS
                  DELIMITED BY SIZE
                  INTO WS-CREATED-AT-TIMESTAMP

           *> Create booking record (walk-in books and checks in same day)
           OPEN I-O BOOKING-FILE
           MOVE WS-BOOKING-ID TO BOOKING-ID
           MOVE WS-ROOM-ID TO ROOM-ID-BK
           MOVE WS-CUSTOMER-ID TO CUSTOMER-ID-BK
           MOVE WS-CURRENT-DATE TO CHECKIN-DATE
           MOVE WS-CHECKOUT-DATE TO CHECKOUT-DATE
           MOVE 'Active' TO BOOKING-STATUS
           MOVE 'Y' TO CHEKIN-FLAG        *> Already checked in
           MOVE 'N' TO CHECKOUT-FLAG
           MOVE WS-CREATED-AT-TIMESTAMP TO CREATED-AT
           WRITE BOOKING-RECORD
           CLOSE BOOKING-FILE

           DISPLAY "====== Walk-in Check In Complete ======"
           DISPLAY "Booking ID: " WS-BOOKING-ID
           DISPLAY "Room ID: " WS-ROOM-ID
           DISPLAY "Room Type: " WS-ROOM-TYPE
           DISPLAY "Customer ID: " WS-CUSTOMER-ID
           DISPLAY "Customer Name: " WS-CUSTOMER-NAME
           DISPLAY "Customer Phone: " WS-CUSTOMER-PHONE
           DISPLAY "Check-in Date: " WS-CURRENT-DATE(1:4) "/"
                   WS-CURRENT-DATE(5:2) "/" WS-CURRENT-DATE(7:2)
           DISPLAY "Check-out Date: " WS-CHECKOUT-DATE(1:4) "/"
                   WS-CHECKOUT-DATE(5:2) "/" WS-CHECKOUT-DATE(7:2)
           DISPLAY "==================================================".



           CHANGE-ROOM-STATUS.
           OPEN I-O ROOMS-FILE.
           MOVE 'N' TO WS-FOUND.
           PERFORM UNTIL WS-FOUND = 'Y'
               READ ROOMS-FILE NEXT
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       IF ROOM-ID = ROOM-ID-BK
                           MOVE 'Y' TO WS-FOUND
                               MOVE 'Occupied' TO R-STATUS
                               REWRITE ROOMS-RECORD
                                   INVALID KEY
           DISPLAY RED-COLOR
           "Error: Unable to update room status for "
            ROOM-ID RESET-COLOR
                                   NOT INVALID KEY
           DISPLAY GREEN-COLOR "Room " ROOM-ID
           " status updated to Occupied." RESET-COLOR
                               END-REWRITE
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE ROOMS-FILE.

       ADD-GUEST-INFORMATION.
           DISPLAY "=== Guest Information ==="
           DISPLAY "Do you want to add guest details? (Y/N): "
           ACCEPT WS-ADD-GUESTS

           IF WS-ADD-GUESTS = 'Y' OR WS-ADD-GUESTS = 'y'
               MOVE ZEROS TO WS-GUEST-COUNT
               PERFORM ADD-GUEST-DETAILS
           ELSE
               DISPLAY "Guest details skipped."
           END-IF.

       ADD-GUEST-DETAILS.
           DISPLAY "Number of guests staying (maximum 9): "
           ACCEPT WS-GUEST-COUNT
           DISPLAY " "
           IF WS-GUEST-COUNT > 0 AND WS-GUEST-COUNT <= 9
               PERFORM VARYING WS-ROOM-CHOICE FROM 1 BY 1
                       UNTIL WS-ROOM-CHOICE > WS-GUEST-COUNT
                   DISPLAY "============= Guest "
                   WS-ROOM-CHOICE " Details ============="
                   PERFORM COLLECT-GUEST-DATA
                   PERFORM SAVE-GUEST-RECORD
               END-PERFORM
               DISPLAY GREEN-COLOR
               "Guest information saved successfully." RESET-COLOR
           ELSE
             DISPLAY RED-COLOR
             "Invalid number of guests. Skipping guest details."
              RESET-COLOR
           END-IF.

       COLLECT-GUEST-DATA.
           DISPLAY " "
           DISPLAY "============= Guest Information============="
           DISPLAY "Guest name: "
           ACCEPT WS-GUEST-NAME

           DISPLAY "Guest age: "
           ACCEPT WS-GUEST-AGE

           DISPLAY "National Registration Card (NRC): "
           ACCEPT WS-GUEST-NRC

           DISPLAY "Gender (M/F): "
           ACCEPT WS-GUEST-GENDER

           DISPLAY " ".
           *> Use customer phone as guest phone for simplicity
           *> In a more complex system, each guest might have their own phone.

       SAVE-GUEST-RECORD.
           *> Generate next guest ID
           OPEN INPUT GUEST-FILE
           MOVE 0 TO WS-GUEST-ID
           MOVE 'N' TO WS-EOF

           PERFORM UNTIL WS-EOF = 'Y'
               READ GUEST-FILE NEXT
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF GUEST-ID > WS-GUEST-ID
                           MOVE GUEST-ID TO WS-GUEST-ID
                       END-IF
               END-READ
           END-PERFORM
           CLOSE GUEST-FILE
           ADD 1 TO WS-GUEST-ID

           *> Create guest record
           OPEN I-O GUEST-FILE
           MOVE WS-GUEST-ID TO GUEST-ID
           MOVE WS-GUEST-NAME TO GUEST-NAME
           MOVE WS-GUEST-AGE TO GUEST-AGE
           MOVE WS-GUEST-NRC TO GUEST-NRC
           MOVE WS-GUEST-GENDER TO GUEST-GENDER
           WRITE GUEST-RECORD
           CLOSE GUEST-FILE

           DISPLAY "Guest " FUNCTION TRIM(WS-GUEST-NAME)
                   " (ID: " WS-GUEST-ID
                   ", Age: " WS-GUEST-AGE
                   ", NRC: " FUNCTION TRIM(WS-GUEST-NRC)
                   ", Gender: " WS-GUEST-GENDER
                   ") added successfully."
           DISPLAY " ".

       END PROGRAM checkIn.
