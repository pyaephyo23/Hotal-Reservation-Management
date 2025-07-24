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
       01 WS-NRC-NUMBER      PIC X(40).
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
       01 WS-FORMATTED-PRICE PIC ZZZZZZZZ9.

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
       01 WS-MAX-CHECKOUT-DATE PIC 9(8).
       01 WS-DAYS-DIFFERENCE PIC 9(3).
       01 WS-DATE-TO-CHECK   PIC 9(8).
       01 WS-DATE-YEAR       PIC 9(4).
       01 WS-DATE-MONTH      PIC 9(2).
       01 WS-DATE-DAY        PIC 9(2).
       01 WS-IS-LEAP-YEAR    PIC X VALUE 'N'.
       01 WS-MAX-DAYS        PIC 9(2).
       01 WS-CHECKIN-YEAR    PIC 9(4).
       01 WS-CHECKIN-MONTH   PIC 9(2).
       01 WS-CHECKIN-DAY     PIC 9(2).
       01 WS-CHECKOUT-YEAR   PIC 9(4).
       01 WS-CHECKOUT-MONTH  PIC 9(2).
       01 WS-CHECKOUT-DAY    PIC 9(2).
       01 WS-TEMP-DAYS       PIC 9(3).
       01 WS-YEAR-COUNTER    PIC 9(4).
       01 WS-MONTH-COUNTER   PIC 9(2).
       01 WS-CURRENT-ROOM-ID PIC X(5).
       01 WS-BOOKING-EOF     PIC X VALUE 'N'.
       01 WS-USER-CANCELLED  PIC X VALUE 'N'.
       01 WS-DUMMY-INPUT     PIC X.

       *> Color codes for display - ANSI escape sequences
       01 RED-COLOR          PIC X(8) VALUE X"1B5B33316D".
       01 GREEN-COLOR        PIC X(8) VALUE X"1B5B33326D".
       01 RESET-COLOR        PIC X(4) VALUE X"1B5B306D".
       01 BLUE-COLOR         PIC X(8) VALUE X"1B5B33346D".
       01 YELLOW-COLOR       PIC X(8) VALUE X"1B5B33336D".
       01 CYAN-COLOR         PIC X(8) VALUE X"1B5B33366D".

       *> Screen formatting
       01 CLEAR-SCREEN       PIC X(4) VALUE X"1B5B324A".
       01 WS-SCREEN-WIDTH    PIC 99 VALUE 78.
       01 WS-HEADER-LINE     PIC X(78) VALUE ALL "=".
       01 WS-BLANK-LINE      PIC X(78) VALUE ALL " ".

       *> Error display flags
       01 WS-ERROR-FLAG      PIC X VALUE 'N'.
       01 WS-ERROR-MESSAGE   PIC X(80) VALUE SPACES.

       LINKAGE SECTION.
       01 LINK PIC 9.

       PROCEDURE DIVISION USING LINK.

       MAIN-PAGE.
           DISPLAY CLEAR-SCREEN
           DISPLAY BLUE-COLOR
           DISPLAY "==================================================="
           "============================"
           DISPLAY "                           HOTEL ROOM BOOKING SYSTE"
           "M                          "
           DISPLAY "==================================================="
           "============================"
            RESET-COLOR
           DISPLAY "                                                   "

           DISPLAY "                              1. Book Room        "

           DISPLAY "                              9. Return to Main Me"
           "nu                          "
           DISPLAY "                                                   "

           DISPLAY "==================================================="
           "============================"


           ACCEPT WS-CHOICE
           EVALUATE WS-CHOICE
               WHEN 1
                   PERFORM BOOK-ROOM-PROCESS
                   GO TO MAIN-PAGE
               WHEN 9
                   GOBACK
               WHEN OTHER
                   DISPLAY " "
                   DISPLAY RED-COLOR "*** ERROR: Invalid selection. Ple"
                   "ase choose 1 or 9. ***" RESET-COLOR
                   DISPLAY " "
                   DISPLAY "Press ENTER to continue..."
                   ACCEPT WS-TEMP-CHAR
                   GO TO MAIN-PAGE
           END-EVALUATE.

       BOOK-ROOM-PROCESS.
           *> Initialize cancellation flag
           MOVE 'N' TO WS-USER-CANCELLED

           *> Step 1: Get booking dates
           PERFORM VALIDATE-CHECKIN-DATE
           IF WS-USER-CANCELLED = 'Y'
               PERFORM BOOK-ROOM-RETRY
               EXIT PARAGRAPH
           END-IF

           PERFORM VALIDATE-CHECKOUT-DATE
           IF WS-USER-CANCELLED = 'Y'
               PERFORM BOOK-ROOM-RETRY
               EXIT PARAGRAPH
           END-IF

           *> Step 2: Get room type preference
           PERFORM VALIDATE-ROOM-TYPE
           IF WS-USER-CANCELLED = 'Y'
               PERFORM BOOK-ROOM-RETRY
               EXIT PARAGRAPH
           END-IF

           *> Step 3: Check for available rooms of that type
           PERFORM CHECK-ROOM-AVAILABILITY

           IF WS-FOUND = 'Y'
               *> Step 4: Get customer information
               PERFORM HANDLE-CUSTOMER-RECORD
               IF WS-USER-CANCELLED = 'Y'
                   PERFORM BOOK-ROOM-RETRY
                   EXIT PARAGRAPH
               END-IF

               *> Step 5: Create booking
               PERFORM CREATE-BOOKING

               *> Display success message after booking is created
               DISPLAY CLEAR-SCREEN
               DISPLAY GREEN-COLOR
               DISPLAY "==============================================="
               "================================"
               DISPLAY "                        BOOKING COMPLETED SUCCE"
               "SSFULLY                        "
               DISPLAY "==============================================="
               "================================"

               DISPLAY "  Booking ID:       " WS-BOOKING-ID
               DISPLAY "  Room ID:          " WS-ROOM-ID
               DISPLAY "  Room Type:        " WS-ROOM-TYPE
               DISPLAY "  Customer ID:      " WS-CUSTOMER-ID
               DISPLAY "  Customer Name:    " WS-CUSTOMER-NAME
               DISPLAY "  Customer Phone:   " WS-CUSTOMER-PHONE
               DISPLAY "  Check-in Date:    " WS-CHECKIN-DATE(1:4) "/"
                       WS-CHECKIN-DATE(5:2) "/" WS-CHECKIN-DATE(7:2)
               DISPLAY "  Check-out Date:   " WS-CHECKOUT-DATE(1:4) "/"
                       WS-CHECKOUT-DATE(5:2) "/" WS-CHECKOUT-DATE(7:2)
               DISPLAY "  Created At:       "
               WS-CREATED-AT-TIMESTAMP(1:4) "/"
                       WS-CREATED-AT-TIMESTAMP(5:2) "/"
                       WS-CREATED-AT-TIMESTAMP(7:2)
               DISPLAY "=============================================="
               "================================="
               RESET-COLOR
               DISPLAY " "
               DISPLAY "Press ENTER to continue..."
               ACCEPT WS-TEMP-CHAR
           ELSE
               *> Only show "no rooms available" if user didn't cancel
               IF WS-USER-CANCELLED = 'N'
                   DISPLAY " "
                   DISPLAY RED-COLOR "Sorry, no " WS-ROOM-TYPE
                         " rooms are available for your selected dates."
                         RESET-COLOR
                   DISPLAY " "
                   DISPLAY "Press ENTER to continue..."
                   ACCEPT WS-TEMP-CHAR
               END-IF
           END-IF

           PERFORM BOOK-ROOM-RETRY.

       BOOK-ROOM-RETRY.
           DISPLAY CLEAR-SCREEN
           DISPLAY BLUE-COLOR
           DISPLAY "==================================================="
           "==========================="
           DISPLAY "                              BOOKING OPTIONS      "

           DISPLAY "==================================================="
           "==========================="
           RESET-COLOR
           DISPLAY "                                                   "

           DISPLAY "                              1. Book Again        "

          DISPLAY "                              9. Return to Main Menu"

           DISPLAY "                                                   "

           DISPLAY "==================================================="
           "==========================="


           ACCEPT WS-CHOICE
           EVALUATE WS-CHOICE
               WHEN 1
                   PERFORM BOOK-ROOM-PROCESS
               WHEN 9
                    GOBACK
               WHEN OTHER
                   DISPLAY " "
                   DISPLAY RED-COLOR "*** ERROR: Invalid selection. Ple"
                   "ase choose 1 or 9. ***" RESET-COLOR
                   DISPLAY " "
                   DISPLAY "Press ENTER to continue..."
                   ACCEPT WS-TEMP-CHAR
                   PERFORM BOOK-ROOM-RETRY
           END-EVALUATE.

       VALIDATE-ROOM-TYPE.
           DISPLAY CLEAR-SCREEN
           DISPLAY CYAN-COLOR
           DISPLAY "==================================================="
           "============================"
           DISPLAY "                             ROOM TYPE SELECTION   "

           DISPLAY "==================================================="
           "============================"
           RESET-COLOR

           DISPLAY "                                                   "

           DISPLAY "                      Please select your preferred "
           "room type:                 "
           DISPLAY "                                                   "

           DISPLAY "                              1. Single Room       "

           DISPLAY "                              2. Double Room       "

           DISPLAY "                              3. Deluxe Room       "

           DISPLAY "  "
           DISPLAY "==================================================="
           "============================"
           DISPLAY "                            0. Cancel reservation  "
           DISPLAY "==================================================="
           "============================"

           DISPLAY "Enter your choice (1-3) or 0 to cancel: "
           WITH NO ADVANCING
           ACCEPT WS-CHOICE
           DISPLAY " "
           EVALUATE WS-CHOICE
               WHEN 0
                   DISPLAY " "
                   DISPLAY RED-COLOR "*** Booking cancelled by user. **"
                   "*" RESET-COLOR
                   DISPLAY " "
                   MOVE 'Y' TO WS-USER-CANCELLED
               WHEN 1
                   MOVE 'Single' TO WS-ROOM-TYPE
               WHEN 2
                   MOVE 'Double' TO WS-ROOM-TYPE
               WHEN 3
                   MOVE 'Delux' TO WS-ROOM-TYPE
               WHEN OTHER
                   DISPLAY " "
                   DISPLAY RED-COLOR "*** ERROR: Invalid selection. Ple"
                   "ase choose 1, 2, 3, or 0. ***" RESET-COLOR
                   DISPLAY " "
                   DISPLAY "Press ENTER to try again..."
                   ACCEPT WS-TEMP-CHAR
                   GO TO VALIDATE-ROOM-TYPE
           END-EVALUATE

           IF WS-USER-CANCELLED = 'N'
               DISPLAY " "
               DISPLAY GREEN-COLOR "Room type selected: " WS-ROOM-TYPE
               RESET-COLOR
               DISPLAY " "
           END-IF.

       CHECK-ROOM-AVAILABILITY.
           MOVE 'N' TO WS-FOUND
           MOVE ZEROS TO WS-AVAILABLE-COUNT

           DISPLAY CLEAR-SCREEN
           DISPLAY CYAN-COLOR
           DISPLAY "==================================================="
           "==========================="
           DISPLAY "                           ROOM AVAILABILITY SEARCH"

           DISPLAY "==================================================="
           "==========================="
           RESET-COLOR
           DISPLAY "                                                   "

           DISPLAY "  Searching for available "
           FUNCTION TRIM(WS-ROOM-TYPE)
                   " rooms"
           DISPLAY "  From: " WS-CHECKIN-DATE(1:4) "/"
                   WS-CHECKIN-DATE(5:2) "/" WS-CHECKIN-DATE(7:2)
                   "  To: " WS-CHECKOUT-DATE(1:4) "/"
                   WS-CHECKOUT-DATE(5:2) "/" WS-CHECKOUT-DATE(7:2)
           DISPLAY "                                                   "

           DISPLAY "                            Please wait...         "

           DISPLAY "==================================================="
           "==========================="


           *> Open rooms file to get all rooms of the requested type
           OPEN INPUT ROOMS-FILE
           MOVE 'N' TO WS-EOF

           PERFORM UNTIL WS-EOF = 'Y'
               READ ROOMS-FILE NEXT
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF ROOM-TYPE = WS-ROOM-TYPE
                           IF ACTIVE-BOOKING-COUNT NOT NUMERIC
                               MOVE ZERO TO ACTIVE-BOOKING-COUNT
                           END-IF
                           PERFORM CHECK-ROOM-CONFLICTS
                           IF WS-CONFLICT-FOUND = 'N'
                               *> No conflicts found, room is available
                               ADD 1 TO WS-AVAILABLE-COUNT
                               MOVE ROOM-ID TO
                                WS-AVAILABLE-ROOM-ID(WS-AVAILABLE-COUNT)
                               MOVE PRICE-PER-NIGHT TO
                             WS-AVAILABLE-ROOM-PRICE(WS-AVAILABLE-COUNT)
                           END-IF
                       END-IF
               END-READ
           END-PERFORM

           DISPLAY " "
           DISPLAY GREEN-COLOR
           "Search completed! Available rooms found: "
                   WS-AVAILABLE-COUNT RESET-COLOR
           DISPLAY " "
           CLOSE ROOMS-FILE

           *> Display available rooms and let user choose
           IF WS-AVAILABLE-COUNT > 0
               PERFORM DISPLAY-AVAILABLE-ROOMS
               PERFORM SELECT-ROOM-FROM-LIST
           ELSE
               DISPLAY RED-COLOR
               "No " FUNCTION TRIM(WS-ROOM-TYPE) " rooms available "
               "for the selected dates!"
               RESET-COLOR
               DISPLAY "Press Enter to continue..."
               ACCEPT WS-DUMMY-INPUT
               MOVE 'N' TO WS-FOUND
           END-IF.

       DISPLAY-AVAILABLE-ROOMS.
           DISPLAY CLEAR-SCREEN
           DISPLAY YELLOW-COLOR
           DISPLAY "==================================================="
           "==========================="
           DISPLAY "                    AVAILABLE "
           FUNCTION TRIM(WS-ROOM-TYPE) " ROOMS"
           DISPLAY "==================================================="
           "==========================="
           RESET-COLOR

           DISPLAY "  Room No.    Room ID         Rate per Night      "

           DISPLAY "==================================================="
           "==========================="
           PERFORM VARYING WS-TEMP-INDEX FROM 1 BY 1
                   UNTIL WS-TEMP-INDEX > WS-AVAILABLE-COUNT
               MOVE WS-AVAILABLE-ROOM-PRICE(WS-TEMP-INDEX)
                    TO WS-FORMATTED-PRICE
               DISPLAY "     " WS-TEMP-INDEX "         "
                       WS-AVAILABLE-ROOM-ID(WS-TEMP-INDEX)
                       "           $" WS-FORMATTED-PRICE " per night"
           END-PERFORM
           DISPLAY "==================================================="
           "==========================="
           DISPLAY "                           0. Cancel reservation   "

           DISPLAY "==================================================="
           "===========================".


       SELECT-ROOM-FROM-LIST.
           DISPLAY "Please select a room (1-" WS-AVAILABLE-COUNT
           ") or 0 to cancel: " WITH NO ADVANCING
           ACCEPT WS-ROOM-CHOICE

           IF WS-ROOM-CHOICE = 0
              DISPLAY RED-COLOR "Booking cancelled by user." RESET-COLOR
               MOVE 'N' TO WS-FOUND
               MOVE 'Y' TO WS-USER-CANCELLED
           ELSE IF WS-ROOM-CHOICE >= 1
               AND WS-ROOM-CHOICE <= WS-AVAILABLE-COUNT
               MOVE WS-AVAILABLE-ROOM-ID(WS-ROOM-CHOICE) TO WS-ROOM-ID
               MOVE 'Y' TO WS-FOUND
               DISPLAY GREEN-COLOR "Room " WS-ROOM-ID
               " is selected " RESET-COLOR
           ELSE
               DISPLAY RED-COLOR
               "Invalid selection. Please choose a valid room number."
               RESET-COLOR
               DISPLAY "Press Enter to continue..."
               ACCEPT WS-DUMMY-INPUT
               GO TO SELECT-ROOM-FROM-LIST
           END-IF.

       CHECK-ROOM-CONFLICTS.
           MOVE 'N' TO WS-CONFLICT-FOUND
           *> Save current room ID before opening booking file
           MOVE ROOM-ID TO WS-CURRENT-ROOM-ID

           *> Open booking file to check for conflicts
           OPEN INPUT BOOKING-FILE
           MOVE 'N' TO WS-BOOKING-EOF

           PERFORM UNTIL WS-BOOKING-EOF = 'Y'
               READ BOOKING-FILE NEXT
                   AT END
                       MOVE 'Y' TO WS-BOOKING-EOF
                   NOT AT END
                       *> Check if this booking is for the current room and is active
                       IF ROOM-ID-BK = WS-CURRENT-ROOM-ID
                          AND BOOKING-STATUS = 'Active'
                           *> Check for date overlap
                           IF (WS-CHECKIN-DATE <= CHECKOUT-DATE) AND
                              (WS-CHECKOUT-DATE >= CHECKIN-DATE)
                               MOVE 'Y' TO WS-CONFLICT-FOUND
               *> Exit the booking loop early since we found a conflict
                               MOVE 'Y' TO WS-BOOKING-EOF
                           END-IF
                       END-IF
               END-READ
           END-PERFORM

           CLOSE BOOKING-FILE.

       CHECK-FOR-CANCELLATION.
           *> Check if user entered 0 to cancel
           IF WS-CHECKIN-DATE = 0 OR WS-CHECKOUT-DATE = 0
           OR WS-CHOICE = 0
              DISPLAY RED-COLOR "Booking cancelled by user." RESET-COLOR
               MOVE 'Y' TO WS-USER-CANCELLED
           END-IF.

       HANDLE-CUSTOMER-RECORD.
           *> First get customer phone number
           PERFORM VALIDATE-CUSTOMER-PHONE
           IF WS-USER-CANCELLED = 'Y'
               EXIT PARAGRAPH
           END-IF

           *> Check if customer exists by phone number
           OPEN INPUT CUSTOMER-FILE
           MOVE 'N' TO WS-ID-FOUND
           MOVE 'N' TO WS-EOF

           PERFORM UNTIL WS-EOF = 'Y'
               READ CUSTOMER-FILE NEXT
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                      IF CUSTOMER-PHONE = WS-CUSTOMER-PHONE
                       DISPLAY "We found an existing guest record:"
                       DISPLAY "Guest ID: " CUSTOMER-ID
                       DISPLAY "Name: " CUSTOMER-NAME
                       DISPLAY "Phone: " CUSTOMER-PHONE
                    DISPLAY "Use this guest record? (Y/N/C to cancel): "
                           ACCEPT WS-EXIST-CHOICE

                      *> Check for cancellation
                      IF WS-EXIST-CHOICE = 'C' OR WS-EXIST-CHOICE = 'c'
                          DISPLAY " "
                          DISPLAY RED-COLOR "*** Booking cancelled by u"
                          "ser. ***" RESET-COLOR
                          DISPLAY " "
                          MOVE 'Y' TO WS-USER-CANCELLED
                          CLOSE CUSTOMER-FILE
                          EXIT PARAGRAPH
                      END-IF

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
           IF WS-ID-FOUND NOT = 'Y' AND WS-USER-CANCELLED = 'N'
               *> Get customer name for new customer
               PERFORM VALIDATE-CUSTOMER-NAME
               IF WS-USER-CANCELLED = 'Y'
                   EXIT PARAGRAPH
               END-IF
               *> Create new customer
               PERFORM CREATE-NEW-CUSTOMER
           END-IF.

       CREATE-NEW-CUSTOMER.
           DISPLAY "Creating new guest profile..."

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
                   DISPLAY RED-COLOR
                   "Error: Unable to update room status." RESET-COLOR
               NOT INVALID KEY
                   *> Initialize ACTIVE-BOOKING-COUNT if it contains non-numeric data
                   IF ACTIVE-BOOKING-COUNT NOT NUMERIC
                       MOVE ZERO TO ACTIVE-BOOKING-COUNT
                   END-IF
                   ADD 1 TO ACTIVE-BOOKING-COUNT
                   REWRITE ROOMS-RECORD
           END-READ
           CLOSE ROOMS-FILE.

       VALIDATE-CUSTOMER-NAME.
           DISPLAY CLEAR-SCREEN
           DISPLAY BLUE-COLOR
           DISPLAY "==================================================="
           "==========================="
           DISPLAY "                           CUSTOMER INFORMATION    "

           DISPLAY "==================================================="
           "==========================="
           RESET-COLOR
           DISPLAY "                                                   "

           DISPLAY "                      Please enter your full name  "

           DISPLAY "  "
           DISPLAY "==================================================="
           "==========================="
           DISPLAY "                        Type 'CANCEL' to cancel    "
           DISPLAY "==================================================="
           "==========================="

           DISPLAY "Full Name: " WITH NO ADVANCING
           ACCEPT WS-CUSTOMER-NAME
           DISPLAY " "
           *> Check for cancellation
           IF WS-CUSTOMER-NAME = 'CANCEL' OR WS-CUSTOMER-NAME = 'cancel'
               DISPLAY " "
               DISPLAY RED-COLOR "*** Booking cancelled by user. ***"
               RESET-COLOR
               DISPLAY " "
               MOVE 'Y' TO WS-USER-CANCELLED
               EXIT PARAGRAPH
           END-IF

           IF WS-CUSTOMER-NAME = SPACES
               DISPLAY " "
               DISPLAY RED-COLOR "*** ERROR: Guest name is required. Pl"
               "ease enter your full name. ***" RESET-COLOR
               DISPLAY " "
               DISPLAY "Press ENTER to try again..."
               ACCEPT WS-TEMP-CHAR
               GO TO VALIDATE-CUSTOMER-NAME
           END-IF.

       VALIDATE-CUSTOMER-PHONE.
           DISPLAY CLEAR-SCREEN
           DISPLAY BLUE-COLOR
           DISPLAY "==================================================="
           "==========================="
           DISPLAY "                             CONTACT INFORMATION   "

           DISPLAY "==================================================="
           "==========================="
            RESET-COLOR
           DISPLAY "                                                   "

           DISPLAY "                   Please enter your contact phone "
           "number                   "

           DISPLAY "  "
           DISPLAY "==================================================="
           "==========================="
           DISPLAY "                        Type 'CANCEL' to cancel    "
           DISPLAY "==================================================="
           "==========================="

           DISPLAY "Phone Number: " WITH NO ADVANCING
           ACCEPT WS-CUSTOMER-PHONE
           DISPLAY " "
           *> Check for cancellation
           IF WS-CUSTOMER-PHONE = 'CANCEL'
               OR WS-CUSTOMER-PHONE = 'cancel'
               DISPLAY " "
               DISPLAY RED-COLOR "*** Booking cancelled by user. ***"
               RESET-COLOR
               DISPLAY " "
               MOVE 'Y' TO WS-USER-CANCELLED
               EXIT PARAGRAPH
           END-IF

           IF WS-CUSTOMER-PHONE = SPACES
               DISPLAY " "
               DISPLAY RED-COLOR "*** ERROR: Customer Phone cannot be e"
               "mpty. ***" RESET-COLOR
               DISPLAY " "
               DISPLAY "Press ENTER to try again..."
               ACCEPT WS-TEMP-CHAR
               GO TO VALIDATE-CUSTOMER-PHONE
           END-IF.

       VALIDATE-CUSTOMER-EMAIL.
           DISPLAY " "
           DISPLAY "Enter Customer Email or type 'CANCEL' to cancel: "
           ACCEPT WS-CUSTOMER-EMAIL

           *> Check for cancellation
           IF WS-CUSTOMER-EMAIL = 'CANCEL'
               OR WS-CUSTOMER-EMAIL = 'cancel'
               DISPLAY " "
               DISPLAY RED-COLOR "*** Booking cancelled by user. ***"
               RESET-COLOR
               DISPLAY " "
               MOVE 'Y' TO WS-USER-CANCELLED
               EXIT PARAGRAPH
           END-IF

           IF WS-CUSTOMER-EMAIL = SPACES
               DISPLAY " "
               DISPLAY RED-COLOR "*** ERROR: Customer Email cannot be e"
               "mpty. ***" RESET-COLOR
               DISPLAY " "
               DISPLAY "Press ENTER to try again..."
               ACCEPT WS-TEMP-CHAR
               GO TO VALIDATE-CUSTOMER-EMAIL
           END-IF.

       VALIDATE-NRC-NUMBER.
           DISPLAY " "
           DISPLAY "Enter Customer NRC Number or type 'CANCEL' to canc"
           "el: "
           ACCEPT WS-NRC-NUMBER

           *> Check for cancellation
           IF WS-NRC-NUMBER = 'CANCEL' OR WS-NRC-NUMBER = 'cancel'
               DISPLAY " "
               DISPLAY RED-COLOR "*** Booking cancelled by user. ***"
               RESET-COLOR
               DISPLAY " "
               MOVE 'Y' TO WS-USER-CANCELLED
               EXIT PARAGRAPH
           END-IF

           IF WS-NRC-NUMBER = SPACES
               DISPLAY " "
               DISPLAY RED-COLOR "*** ERROR: Customer NRC Number cannot"
               "be empty. ***" RESET-COLOR
               DISPLAY " "
               DISPLAY "Press ENTER to try again..."
               ACCEPT WS-TEMP-CHAR
               GO TO VALIDATE-NRC-NUMBER
           END-IF.

       VALIDATE-CHECKIN-DATE.
           *> Get current date first
           ACCEPT WS-CURRENT-DATE-DATA FROM DATE YYYYMMDD
           MOVE WS-CURRENT-DATE TO WS-CURRENT-DATE-NUM

           DISPLAY CLEAR-SCREEN
           DISPLAY GREEN-COLOR
           DISPLAY "==================================================="
           "============================"
           DISPLAY "                              CHECK-IN DATE        "

           DISPLAY "==================================================="
           "============================"
            RESET-COLOR

           DISPLAY "                                                   "

           DISPLAY "                  Please enter your check-in date ("
           "YYYYMMDD)                 "

           DISPLAY "  "
           DISPLAY "==================================================="
           "============================"
           DISPLAY "                            0. Cancel reservation  "
           DISPLAY "==================================================="
           "============================"

           DISPLAY "Check-in date: " WITH NO ADVANCING
           ACCEPT WS-CHECKIN-DATE
           DISPLAY " "
           *> Check for cancellation
           IF WS-CHECKIN-DATE = 0
               DISPLAY " "
               DISPLAY RED-COLOR "*** Booking cancelled by user. ***"
               RESET-COLOR
               DISPLAY " "
               MOVE 'Y' TO WS-USER-CANCELLED
               EXIT PARAGRAPH
           END-IF

           IF WS-CHECKIN-DATE = ZEROS OR WS-CHECKIN-DATE = SPACES
               DISPLAY " "
               DISPLAY RED-COLOR "*** ERROR: Check-in date is required."
               "Please enter a valid date. ***" RESET-COLOR
               DISPLAY " "
               DISPLAY "Press ENTER to try again..."
               ACCEPT WS-TEMP-CHAR
               GO TO VALIDATE-CHECKIN-DATE
           END-IF
           MOVE WS-CHECKIN-DATE TO WS-DATE-TO-CHECK
           PERFORM VALIDATE-DATE-FORMAT
           IF WS-VALID-FLAG = 'N'
               DISPLAY " "
               DISPLAY RED-COLOR "*** ERROR: Invalid date format. Pleas"
               "e use YYYYMMDD. ***" RESET-COLOR
               DISPLAY " "
               DISPLAY "Press ENTER to try again..."
               ACCEPT WS-TEMP-CHAR
               GO TO VALIDATE-CHECKIN-DATE
           END-IF

           *> Check if check-in date is not earlier than current date
           IF WS-CHECKIN-DATE < WS-CURRENT-DATE-NUM
               DISPLAY " "
               DISPLAY RED-COLOR "*** ERROR: Check-in date must be toda"
               "y or a future date ("
                       WS-CURRENT-DATE-NUM(1:4) "/"
                       WS-CURRENT-DATE-NUM(5:2) "/"
                       WS-CURRENT-DATE-NUM(7:2) " or later). ***"
                       RESET-COLOR
               DISPLAY " "
               DISPLAY "Press ENTER to try again..."
               ACCEPT WS-TEMP-CHAR
               GO TO VALIDATE-CHECKIN-DATE
           END-IF.

       VALIDATE-CHECKOUT-DATE.
           DISPLAY CLEAR-SCREEN
           DISPLAY GREEN-COLOR
           DISPLAY "==================================================="
           "============================"
           DISPLAY "                             CHECK-OUT DATE        "

           DISPLAY "==================================================="
           "============================" RESET-COLOR

           DISPLAY "                                                   "

           DISPLAY "                 Please enter your check-out date ("
           "YYYYMMDD)                 "

           DISPLAY "  "
           DISPLAY "==================================================="
           "============================"
           DISPLAY "                            0. Cancel reservation  "
           DISPLAY "==================================================="
           "============================"

           DISPLAY "Enter check-out date: " WITH NO ADVANCING
           ACCEPT WS-CHECKOUT-DATE

           DISPLAY " "
           *> Check for cancellation
           IF WS-CHECKOUT-DATE = 0
               DISPLAY " "
               DISPLAY RED-COLOR "*** Booking cancelled by user. ***"
               RESET-COLOR
               DISPLAY " "
               MOVE 'Y' TO WS-USER-CANCELLED
               EXIT PARAGRAPH
           END-IF

           IF WS-CHECKOUT-DATE = ZEROS OR WS-CHECKOUT-DATE = SPACES
               DISPLAY " "
               DISPLAY RED-COLOR "*** ERROR: Check-out date is required"
               ". Please enter a valid date. ***" RESET-COLOR
               DISPLAY " "
               DISPLAY "Press ENTER to try again..."
               ACCEPT WS-TEMP-CHAR
               GO TO VALIDATE-CHECKOUT-DATE
           END-IF
           MOVE WS-CHECKOUT-DATE TO WS-DATE-TO-CHECK
           PERFORM VALIDATE-DATE-FORMAT
           IF WS-VALID-FLAG = 'N'
               DISPLAY " "
               DISPLAY RED-COLOR "*** ERROR: Invalid date format. Pleas"
               "e use YYYYMMDD. ***" RESET-COLOR
               DISPLAY " "
               DISPLAY "Press ENTER to try again..."
               ACCEPT WS-TEMP-CHAR
               GO TO VALIDATE-CHECKOUT-DATE
           END-IF
           IF WS-CHECKOUT-DATE <= WS-CHECKIN-DATE
               DISPLAY " "
               DISPLAY RED-COLOR "*** ERROR: Check-out date must be aft"
               "er your check-in date. ***" RESET-COLOR
               DISPLAY " "
               DISPLAY "Press ENTER to try again..."
               ACCEPT WS-TEMP-CHAR
               GO TO VALIDATE-CHECKOUT-DATE
           END-IF

           *> Calculate days difference between check-in and check-out
           PERFORM CALCULATE-DAYS-DIFFERENCE
           IF WS-DAYS-DIFFERENCE > 28
               DISPLAY " "
               DISPLAY RED-COLOR "*** ERROR: Maximum stay duration is 2"
               "8 days. "
                   "Your selected stay is " WS-DAYS-DIFFERENCE " days. "
                   "***" RESET-COLOR
               DISPLAY " "
               DISPLAY "Press ENTER to try again..."
               ACCEPT WS-TEMP-CHAR
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

           *> If basic format is valid, perform comprehensive validation
           IF WS-VALID-FLAG = 'Y'
               *> Extract date components
               MOVE WS-DATE-TO-CHECK(1:4) TO WS-DATE-YEAR
               MOVE WS-DATE-TO-CHECK(5:2) TO WS-DATE-MONTH
               MOVE WS-DATE-TO-CHECK(7:2) TO WS-DATE-DAY

               *> Validate year (reasonable range: 2020-2050)
               IF WS-DATE-YEAR < 2020 OR WS-DATE-YEAR > 2050
                   MOVE 'N' TO WS-VALID-FLAG
               END-IF

               *> Validate month (1-12)
               IF WS-DATE-MONTH < 1 OR WS-DATE-MONTH > 12
                   MOVE 'N' TO WS-VALID-FLAG
               END-IF

               *> Validate day (1-31, depending on month and leap year)
               IF WS-DATE-DAY < 1 OR WS-DATE-DAY > 31
                   MOVE 'N' TO WS-VALID-FLAG
               END-IF

               *> If basic ranges are valid, check month-specific day limits
               IF WS-VALID-FLAG = 'Y'
                   PERFORM CHECK-LEAP-YEAR
                   PERFORM VALIDATE-DAYS-IN-MONTH
               END-IF
           END-IF.

       CHECK-LEAP-YEAR.
           MOVE 'N' TO WS-IS-LEAP-YEAR

           *> Leap year logic:
           *> - Divisible by 4 AND
           *> - If divisible by 100, must also be divisible by 400
           IF FUNCTION MOD(WS-DATE-YEAR, 4) = 0
               IF FUNCTION MOD(WS-DATE-YEAR, 100) = 0
                   IF FUNCTION MOD(WS-DATE-YEAR, 400) = 0
                       MOVE 'Y' TO WS-IS-LEAP-YEAR
                   END-IF
               ELSE
                   MOVE 'Y' TO WS-IS-LEAP-YEAR
               END-IF
           END-IF.

       VALIDATE-DAYS-IN-MONTH.
           *> Set maximum days for each month
           EVALUATE WS-DATE-MONTH
               WHEN 1  *> January
               WHEN 3  *> March
               WHEN 5  *> May
               WHEN 7  *> July
               WHEN 8  *> August
               WHEN 10 *> October
               WHEN 12 *> December
                   MOVE 31 TO WS-MAX-DAYS
               WHEN 4  *> April
               WHEN 6  *> June
               WHEN 9  *> September
               WHEN 11 *> November
                   MOVE 30 TO WS-MAX-DAYS
               WHEN 2  *> February
                   IF WS-IS-LEAP-YEAR = 'Y'
                       MOVE 29 TO WS-MAX-DAYS
                   ELSE
                       MOVE 28 TO WS-MAX-DAYS
                   END-IF
           END-EVALUATE

           *> Check if day is valid for the month
           IF WS-DATE-DAY > WS-MAX-DAYS
               MOVE 'N' TO WS-VALID-FLAG
               DISPLAY " "
               DISPLAY RED-COLOR "*** ERROR: Invalid day for month "
               WS-DATE-MONTH
                       ". Maximum days: " WS-MAX-DAYS " ***" RESET-COLOR
               IF WS-DATE-MONTH = 2 AND WS-IS-LEAP-YEAR = 'Y'
                   DISPLAY "(" WS-DATE-YEAR " is a leap year)"
               END-IF
               DISPLAY " "
           END-IF.

       CALCULATE-DAYS-DIFFERENCE.
           *> Enhanced date difference calculation with leap year consideration
           *> Extract date components from both dates
           MOVE WS-CHECKIN-DATE(1:4) TO WS-CHECKIN-YEAR
           MOVE WS-CHECKIN-DATE(5:2) TO WS-CHECKIN-MONTH
           MOVE WS-CHECKIN-DATE(7:2) TO WS-CHECKIN-DAY

           MOVE WS-CHECKOUT-DATE(1:4) TO WS-CHECKOUT-YEAR
           MOVE WS-CHECKOUT-DATE(5:2) TO WS-CHECKOUT-MONTH
           MOVE WS-CHECKOUT-DATE(7:2) TO WS-CHECKOUT-DAY

           MOVE ZERO TO WS-DAYS-DIFFERENCE

           *> If same year and month, simple day difference
           IF WS-CHECKIN-YEAR = WS-CHECKOUT-YEAR AND
              WS-CHECKIN-MONTH = WS-CHECKOUT-MONTH
               COMPUTE WS-DAYS-DIFFERENCE =
                   WS-CHECKOUT-DAY - WS-CHECKIN-DAY
           ELSE
               *> Calculate across months/years
               PERFORM CALC-COMPLEX-DATE-DIFF
           END-IF

           *> Ensure result is reasonable (fallback protection)
           IF WS-DAYS-DIFFERENCE < 0 OR WS-DAYS-DIFFERENCE > 400
               DISPLAY " "
               DISPLAY YELLOW-COLOR "*** WARNING: Date calculation may "
               "be inaccurate ***" RESET-COLOR
               DISPLAY " "
               *> Use simplified calculation as fallback
               COMPUTE WS-DAYS-DIFFERENCE =
                   (WS-CHECKOUT-YEAR - WS-CHECKIN-YEAR) * 365
                   + (WS-CHECKOUT-MONTH - WS-CHECKIN-MONTH) * 30
                   + (WS-CHECKOUT-DAY - WS-CHECKIN-DAY)

               *> Ensure minimum reasonable result
               IF WS-DAYS-DIFFERENCE < 1
                   MOVE 1 TO WS-DAYS-DIFFERENCE
               END-IF
               IF WS-DAYS-DIFFERENCE > 365
                   MOVE 365 TO WS-DAYS-DIFFERENCE
               END-IF
           END-IF.

       CALC-COMPLEX-DATE-DIFF.
           *> Step 1: Add remaining days in check-in month
           MOVE WS-CHECKIN-YEAR TO WS-DATE-YEAR
           MOVE WS-CHECKIN-MONTH TO WS-DATE-MONTH
           PERFORM CHECK-LEAP-YEAR
           PERFORM VALIDATE-DAYS-IN-MONTH
           COMPUTE WS-TEMP-DAYS = WS-MAX-DAYS - WS-CHECKIN-DAY
           ADD WS-TEMP-DAYS TO WS-DAYS-DIFFERENCE

           *> Step 2: Add full months between check-in and check-out
           MOVE WS-CHECKIN-YEAR TO WS-YEAR-COUNTER
           MOVE WS-CHECKIN-MONTH TO WS-MONTH-COUNTER

           PERFORM UNTIL (WS-YEAR-COUNTER = WS-CHECKOUT-YEAR AND
                         WS-MONTH-COUNTER = WS-CHECKOUT-MONTH)
               *> Move to next month
               ADD 1 TO WS-MONTH-COUNTER
               IF WS-MONTH-COUNTER > 12
                   MOVE 1 TO WS-MONTH-COUNTER
                   ADD 1 TO WS-YEAR-COUNTER
               END-IF

               *> Don't add days for the checkout month
               IF NOT (WS-YEAR-COUNTER = WS-CHECKOUT-YEAR AND
                      WS-MONTH-COUNTER = WS-CHECKOUT-MONTH)
                   *> Get days in this month
                   MOVE WS-YEAR-COUNTER TO WS-DATE-YEAR
                   MOVE WS-MONTH-COUNTER TO WS-DATE-MONTH
                   PERFORM CHECK-LEAP-YEAR
                   PERFORM VALIDATE-DAYS-IN-MONTH
                   ADD WS-MAX-DAYS TO WS-DAYS-DIFFERENCE
               END-IF
           END-PERFORM

           *> Step 3: Add days in checkout month
           ADD WS-CHECKOUT-DAY TO WS-DAYS-DIFFERENCE.

       END PROGRAM bookRoom.
