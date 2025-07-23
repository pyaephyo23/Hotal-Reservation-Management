       IDENTIFICATION DIVISION.
       PROGRAM-ID. bookRoom.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ROOMS-FILE ASSIGN TO './DATA/ROOMS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ROOM-ID.
           SELECT BOOKING-FILE ASSIGN TO './DATA/BOOKINGS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS BOOKING-ID.

       DATA DIVISION.
       FILE SECTION.
       FD ROOMS-FILE.
       COPY "./CopyBooks/ROOMS.cpy".

       FD BOOKING-FILE.
       COPY "./CopyBooks/BOOKINGS.cpy".

       WORKING-STORAGE SECTION.
       01 WS-CHOICE          PIC 9.
       01 WS-CHECKIN-DATE    PIC 9(8).
       01 WS-BOOKER-NAME     PIC X(30).
       01 WS-BOOKER-PHONE    PIC X(15).
       01 WS-SELECTED-ROOM   PIC X(5).
       01 WS-BOOKING-ID      PIC 9(5).
       01 WS-ROOM-TYPE-CHOICE PIC X(10).

       *> Date validation
       01 WS-CURRENT-DATE.
           05 WS-CURRENT-YEAR  PIC 9(4).
           05 WS-CURRENT-MONTH PIC 9(2).
           05 WS-CURRENT-DAY   PIC 9(2).
       01 WS-CURRENT-DATE-NUM  PIC 9(8).
       01 WS-DATE-TO-CHECK     PIC 9(8).
       01 WS-VALID-FLAG        PIC X VALUE 'Y'.
       01 WS-TEMP-CHAR         PIC X.
       01 WS-TEMP-INDEX        PIC 9(2).

       *> Enhanced date validation variables
       01 WS-YEAR              PIC 9(4).
       01 WS-MONTH             PIC 9(2).
       01 WS-DAY               PIC 9(2).
       01 WS-IS-LEAP-YEAR      PIC X VALUE 'N'.
       01 WS-MAX-DAYS          PIC 9(2).
       01 WS-DAYS-IN-MONTH.
           05 WS-MONTH-DAYS OCCURS 12 TIMES PIC 9(2).

       *> Available rooms
       01 WS-AVAILABLE-ROOMS.
           05 WS-ROOM-COUNT    PIC 9(2) VALUE 0.
           05 WS-ROOM-LIST OCCURS 20 TIMES.
               10 WS-ROOM-ID   PIC X(5).
               10 WS-ROOM-TYPE PIC X(10).
               10 WS-ROOM-PRICE PIC $(6).

       *> File status and flags
       01 WS-EOF               PIC X VALUE 'N'.
       01 WS-FOUND             PIC X VALUE 'N'.
       01 WS-ROOM-CHOICE       PIC 9(2).

       *> Created timestamp
       01 WS-CURRENT-TIMESTAMP.
           05 WS-TS-DATE.
               10 WS-TS-YEAR   PIC 9(4).
               10 WS-TS-MONTH  PIC 9(2).
               10 WS-TS-DAY    PIC 9(2).
           05 WS-TS-TIME.
               10 WS-TS-HOUR   PIC 9(2).
               10 WS-TS-MIN    PIC 9(2).
               10 WS-TS-SEC    PIC 9(2).
       01 WS-CREATED-AT        PIC X(14).

       *> Color codes for display - ANSI escape sequences
       01 RED-COLOR          PIC X(8) VALUE X"1B5B33316D".
       01 GREEN-COLOR        PIC X(8) VALUE X"1B5B33326D".
       01 RESET-COLOR        PIC X(4) VALUE X"1B5B306D".
       01 BLUE-COLOR         PIC X(8) VALUE X"1B5B33346D".
       01 YELLOW-COLOR       PIC X(8) VALUE X"1B5B33336D".
       01 CYAN-COLOR         PIC X(8) VALUE X"1B5B33366D".

       *> Screen formatting
       01 CLEAR-SCREEN       PIC X(4) VALUE X"1B5B324A".
       01 WS-DUMMY-INPUT     PIC X.

       LINKAGE SECTION.
       01 LINK PIC 9.

       PROCEDURE DIVISION USING LINK.
           PERFORM UNTIL WS-CHOICE = 9
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
           DISPLAY "                              1. Book a Room       "
           DISPLAY "                                                   "
           DISPLAY "==================================================="
           "============================"
           DISPLAY "                              9. Go Back to Main Me"
           "nu                          "
           DISPLAY "==================================================="
           "============================"
           ACCEPT WS-CHOICE

               EVALUATE WS-CHOICE
                   WHEN 1 PERFORM BOOK-ROOM-PROCESS
                   WHEN 9 GOBACK
                   WHEN OTHER
                       DISPLAY " "
                       DISPLAY RED-COLOR "*** ERROR: Invalid selection."
                       "Please choose 1 or 9. ***" RESET-COLOR
                       DISPLAY " "
                       DISPLAY "Press ENTER to continue..."
                       ACCEPT WS-DUMMY-INPUT
               END-EVALUATE
           END-PERFORM.

       BOOK-ROOM-PROCESS.
           DISPLAY CLEAR-SCREEN
           DISPLAY CYAN-COLOR
           DISPLAY "==================================================="
           "============================"
           DISPLAY "                              NEW ROOM BOOKING     "
           DISPLAY "==================================================="
           "============================"
           RESET-COLOR
           DISPLAY "                                                   "
           DISPLAY "                      Note: You can cancel at any s"
           "tep                       "
           DISPLAY "                                                   "

           *> Step 1: Accept and validate check-in date
           PERFORM ACCEPT-CHECKIN-DATE
           IF WS-VALID-FLAG = 'N'
               EXIT PARAGRAPH
           END-IF

           *> Step 2: Ask for room type preference
           PERFORM ASK-ROOM-TYPE
           IF WS-ROOM-TYPE-CHOICE = SPACES
               EXIT PARAGRAPH
           END-IF

           *> Step 3: Search for available rooms
           PERFORM SEARCH-AVAILABLE-ROOMS
           IF WS-ROOM-COUNT = 0
               DISPLAY " "
               DISPLAY RED-COLOR "No rooms available for the selected d"
               "ate." RESET-COLOR
               DISPLAY " "
               DISPLAY "Press ENTER to continue..."
               ACCEPT WS-DUMMY-INPUT
               EXIT PARAGRAPH
           END-IF

           *> Step 4: Display available rooms and let user select
           PERFORM DISPLAY-AVAILABLE-ROOMS
           PERFORM SELECT-ROOM
           IF WS-SELECTED-ROOM = SPACES
               EXIT PARAGRAPH
           END-IF

           *> Step 5: Record booker information
           PERFORM RECORD-BOOKER-INFO
           *> Check if user cancelled during booker info entry
           IF WS-BOOKER-NAME = SPACES OR WS-BOOKER-PHONE = SPACES
               EXIT PARAGRAPH
           END-IF

           *> Step 6: Final confirmation
           PERFORM CONFIRM-BOOKING
           IF WS-VALID-FLAG = 'N'
               DISPLAY " "
               DISPLAY RED-COLOR "Booking cancelled by user."
               RESET-COLOR
               DISPLAY " "
               DISPLAY "Press ENTER to continue..."
               ACCEPT WS-DUMMY-INPUT
               EXIT PARAGRAPH
           END-IF

           *> Step 7: Create booking and update room status
           PERFORM CREATE-BOOKING-RECORD
           PERFORM UPDATE-ROOM-STATUS

           DISPLAY CLEAR-SCREEN
           DISPLAY GREEN-COLOR
           DISPLAY "==============================================="
           "================================"
           DISPLAY "                        BOOKING COMPLETED SUCCE"
           "SSFULLY                        "
           DISPLAY "==============================================="
           "================================"
           DISPLAY "  Booking ID:       " WS-BOOKING-ID
           DISPLAY "  Room:             " WS-SELECTED-ROOM
           DISPLAY "  Customer:         " WS-BOOKER-NAME
           DISPLAY "  Phone:            " WS-BOOKER-PHONE
           DISPLAY "  Check-in:         " WS-CHECKIN-DATE(1:4) "/"
                   WS-CHECKIN-DATE(5:2) "/" WS-CHECKIN-DATE(7:2)
           DISPLAY "=============================================="
           "================================="
           RESET-COLOR
           DISPLAY " "
           DISPLAY "Press ENTER to continue..."
           ACCEPT WS-DUMMY-INPUT.

       ACCEPT-CHECKIN-DATE.
           *> Get current date for validation
           ACCEPT WS-CURRENT-TIMESTAMP FROM DATE YYYYMMDD
           MOVE WS-TS-DATE TO WS-CURRENT-DATE-NUM

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
           DISPLAY "Check-in date: "
           ACCEPT WS-CHECKIN-DATE

           *> Check for cancellation
           IF WS-CHECKIN-DATE = 0 OR WS-CHECKIN-DATE = SPACES
               DISPLAY " "
               DISPLAY RED-COLOR "*** Booking cancelled by user. ***"
               RESET-COLOR
               DISPLAY " "
               DISPLAY "Press ENTER to continue..."
               ACCEPT WS-DUMMY-INPUT
               MOVE 'N' TO WS-VALID-FLAG
               EXIT PARAGRAPH
           END-IF

           *> Validate date format
           MOVE WS-CHECKIN-DATE TO WS-DATE-TO-CHECK
           PERFORM VALIDATE-DATE-FORMAT
           IF WS-VALID-FLAG = 'N'
               DISPLAY " "
               DISPLAY RED-COLOR "*** ERROR: Invalid date format. Pleas"
               "e use YYYYMMDD. ***" RESET-COLOR
               DISPLAY " "
               DISPLAY "Press ENTER to try again..."
               ACCEPT WS-DUMMY-INPUT
               GO TO ACCEPT-CHECKIN-DATE
           END-IF

           *> Check if date is not in the past
           IF WS-CHECKIN-DATE < WS-CURRENT-DATE-NUM
               DISPLAY " "
               DISPLAY RED-COLOR "*** ERROR: Check-in date cannot be in"
               " the past. Today is: " WS-CURRENT-DATE-NUM(1:4) "/"
                       WS-CURRENT-DATE-NUM(5:2) "/"
                       WS-CURRENT-DATE-NUM(7:2) " ***" RESET-COLOR
               DISPLAY " "
               DISPLAY "Press ENTER to try again..."
               ACCEPT WS-DUMMY-INPUT
               GO TO ACCEPT-CHECKIN-DATE
           END-IF

           DISPLAY " "
           DISPLAY GREEN-COLOR "Check-in date accepted: "
                   WS-CHECKIN-DATE(1:4) "/"
                   WS-CHECKIN-DATE(5:2) "/" WS-CHECKIN-DATE(7:2)
                   RESET-COLOR
           DISPLAY " ".

       ASK-ROOM-TYPE.
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

           ACCEPT WS-TEMP-CHAR

           EVALUATE WS-TEMP-CHAR
               WHEN '1'
                   MOVE "Single" TO WS-ROOM-TYPE-CHOICE
                   DISPLAY " "
                   DISPLAY GREEN-COLOR "Selected: Single rooms"
                   RESET-COLOR
               WHEN '2'
                   MOVE "Double" TO WS-ROOM-TYPE-CHOICE
                   DISPLAY " "
                   DISPLAY GREEN-COLOR "Selected: Double rooms"
                   RESET-COLOR
               WHEN '3'
                   MOVE "Delux" TO WS-ROOM-TYPE-CHOICE
                   DISPLAY " "
                   DISPLAY GREEN-COLOR "Selected: Delux rooms"
                   RESET-COLOR
               WHEN '0'
                   DISPLAY " "
                   DISPLAY RED-COLOR "*** Booking cancelled by user."
                   "***"
                   RESET-COLOR
                   DISPLAY " "
                   DISPLAY "Press ENTER to continue..."
                   ACCEPT WS-DUMMY-INPUT
                   MOVE SPACES TO WS-ROOM-TYPE-CHOICE
               WHEN OTHER
                   DISPLAY " "
                   DISPLAY RED-COLOR "*** ERROR: Invalid selection."
                   "Pleas"
                   "e choose 1, 2, 3, or 0. ***" RESET-COLOR
                   DISPLAY " "
                   DISPLAY "Press ENTER to try again..."
                   ACCEPT WS-DUMMY-INPUT
                   GO TO ASK-ROOM-TYPE
           END-EVALUATE.

       VALIDATE-DATE-FORMAT.
           MOVE 'Y' TO WS-VALID-FLAG

           *> Check if all 8 characters are numeric
           PERFORM VARYING WS-TEMP-INDEX FROM 1 BY 1
                   UNTIL WS-TEMP-INDEX > 8
               MOVE WS-DATE-TO-CHECK(WS-TEMP-INDEX:1) TO WS-TEMP-CHAR
               IF WS-TEMP-CHAR NOT NUMERIC
                   MOVE 'N' TO WS-VALID-FLAG
                   EXIT PERFORM
               END-IF
           END-PERFORM

           *> If numeric format is valid, perform enhanced date validation
           IF WS-VALID-FLAG = 'Y'
               MOVE WS-DATE-TO-CHECK(1:4) TO WS-YEAR
               MOVE WS-DATE-TO-CHECK(5:2) TO WS-MONTH
               MOVE WS-DATE-TO-CHECK(7:2) TO WS-DAY
               PERFORM ENHANCED-DATE-VALIDATION
           END-IF.

       ENHANCED-DATE-VALIDATION.
           *> Initialize days in month array
           PERFORM INITIALIZE-MONTH-DAYS

           *> Validate year (reasonable range)
           IF WS-YEAR < 2020 OR WS-YEAR > 2099
               MOVE 'N' TO WS-VALID-FLAG
               DISPLAY " "
               DISPLAY RED-COLOR "*** ERROR: Year must be between 2020"
               " and 2099. ***" RESET-COLOR
               DISPLAY " "
               EXIT PARAGRAPH
           END-IF

           *> Validate month
           IF WS-MONTH < 01 OR WS-MONTH > 12
               MOVE 'N' TO WS-VALID-FLAG
               DISPLAY " "
               DISPLAY RED-COLOR "*** ERROR: Month must be between 01 a"
               "nd 12. ***" RESET-COLOR
               DISPLAY " "
               EXIT PARAGRAPH
           END-IF

           *> Check for leap year if February
           IF WS-MONTH = 02
               PERFORM CHECK-LEAP-YEAR
               IF WS-IS-LEAP-YEAR = 'Y'
                   MOVE 29 TO WS-MONTH-DAYS(2)
               END-IF
           END-IF

           *> Get maximum days for the month
           MOVE WS-MONTH-DAYS(WS-MONTH) TO WS-MAX-DAYS

           *> Validate day
           IF WS-DAY < 01 OR WS-DAY > WS-MAX-DAYS
               MOVE 'N' TO WS-VALID-FLAG
               DISPLAY " "
               DISPLAY RED-COLOR "*** ERROR: Invalid day " WS-DAY
               " for month " WS-MONTH ". Maximum days: " WS-MAX-DAYS
               " ***" RESET-COLOR
               DISPLAY " "
           END-IF.

       INITIALIZE-MONTH-DAYS.
           MOVE 31 TO WS-MONTH-DAYS(1)   *> January
           MOVE 28 TO WS-MONTH-DAYS(2)   *> February (adjusted for leap year)
           MOVE 31 TO WS-MONTH-DAYS(3)   *> March
           MOVE 30 TO WS-MONTH-DAYS(4)   *> April
           MOVE 31 TO WS-MONTH-DAYS(5)   *> May
           MOVE 30 TO WS-MONTH-DAYS(6)   *> June
           MOVE 31 TO WS-MONTH-DAYS(7)   *> July
           MOVE 31 TO WS-MONTH-DAYS(8)   *> August
           MOVE 30 TO WS-MONTH-DAYS(9)   *> September
           MOVE 31 TO WS-MONTH-DAYS(10)  *> October
           MOVE 30 TO WS-MONTH-DAYS(11)  *> November
           MOVE 31 TO WS-MONTH-DAYS(12). *> December

       CHECK-LEAP-YEAR.
           MOVE 'N' TO WS-IS-LEAP-YEAR

           *> A year is a leap year if:
           *> 1. It is divisible by 4 AND
           *> 2. If it's divisible by 100, it must also be divisible by 400

           *> Check if divisible by 4
           IF FUNCTION MOD(WS-YEAR, 4) = 0
               *> If divisible by 100, check if also divisible by 400
               IF FUNCTION MOD(WS-YEAR, 100) = 0
                   IF FUNCTION MOD(WS-YEAR, 400) = 0
                       MOVE 'Y' TO WS-IS-LEAP-YEAR
                   END-IF
               ELSE
                   *> Divisible by 4 but not by 100
                   MOVE 'Y' TO WS-IS-LEAP-YEAR
               END-IF
           END-IF.

       SEARCH-AVAILABLE-ROOMS.
           MOVE 0 TO WS-ROOM-COUNT
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
           FUNCTION TRIM(WS-ROOM-TYPE-CHOICE)
                   " rooms"
           DISPLAY "  For date: " WS-CHECKIN-DATE(1:4) "/"
                   WS-CHECKIN-DATE(5:2) "/" WS-CHECKIN-DATE(7:2)
           DISPLAY "                                                   "
           DISPLAY "                            Please wait...         "
           DISPLAY "==================================================="
           "==========================="

           OPEN INPUT ROOMS-FILE
           MOVE 'N' TO WS-EOF

           PERFORM UNTIL WS-EOF = 'Y'
               READ ROOMS-FILE NEXT RECORD
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   *> Check if room is available and matches type preference
                   IF R-STATUS = "Available" AND
                      ROOM-TYPE = WS-ROOM-TYPE-CHOICE
                       *> Check if we haven't exceeded the array limit
                       IF WS-ROOM-COUNT < 20
                           *> Room is available, add to list
                           ADD 1 TO WS-ROOM-COUNT
                           MOVE ROOM-ID TO WS-ROOM-ID(WS-ROOM-COUNT)
                           MOVE ROOM-TYPE TO
                                WS-ROOM-TYPE(WS-ROOM-COUNT)
                           MOVE PRICE-PER-NIGHT TO
                                WS-ROOM-PRICE(WS-ROOM-COUNT)
                       END-IF
                   END-IF
               END-READ
           END-PERFORM

           CLOSE ROOMS-FILE
           DISPLAY " "
           DISPLAY GREEN-COLOR
           "Search completed! Available rooms found: "
                   WS-ROOM-COUNT RESET-COLOR
           DISPLAY " "
           IF WS-ROOM-COUNT = 20
               DISPLAY YELLOW-COLOR "Note: Maximum of 20 rooms displaye"
               "d. "
                       "There may be more available." RESET-COLOR
           END-IF.

       DISPLAY-AVAILABLE-ROOMS.
           DISPLAY CLEAR-SCREEN
           DISPLAY YELLOW-COLOR
           DISPLAY "==================================================="
           "==========================="
           DISPLAY "                    AVAILABLE "
           FUNCTION TRIM(WS-ROOM-TYPE-CHOICE) " ROOMS"
           DISPLAY "==================================================="
           "==========================="
           RESET-COLOR
           DISPLAY "  Room No.    Room ID         Rate per Night      "
           DISPLAY "==================================================="
           "==========================="
           PERFORM VARYING WS-TEMP-INDEX FROM 1 BY 1
                   UNTIL WS-TEMP-INDEX > WS-ROOM-COUNT
               DISPLAY "     " WS-TEMP-INDEX "         "
                       WS-ROOM-ID(WS-TEMP-INDEX) "           "
                       WS-ROOM-PRICE(WS-TEMP-INDEX) " per night"
           END-PERFORM
           DISPLAY "==================================================="
           "==========================="
           DISPLAY "                           0. Cancel reservation   "
           DISPLAY "==================================================="
           "===========================".

       SELECT-ROOM.
            ACCEPT WS-ROOM-CHOICE

           IF WS-ROOM-CHOICE = 0
               DISPLAY " "
               DISPLAY RED-COLOR "Booking cancelled by user."
               RESET-COLOR
               DISPLAY " "
               DISPLAY "Press ENTER to continue..."
               ACCEPT WS-DUMMY-INPUT
               MOVE SPACES TO WS-SELECTED-ROOM
           ELSE IF WS-ROOM-CHOICE >= 1 AND
                   WS-ROOM-CHOICE <= WS-ROOM-COUNT
               MOVE WS-ROOM-ID(WS-ROOM-CHOICE) TO WS-SELECTED-ROOM
               DISPLAY " "
               DISPLAY GREEN-COLOR "Selected room: " WS-SELECTED-ROOM
               RESET-COLOR
               DISPLAY " "
           ELSE
               DISPLAY " "
               DISPLAY RED-COLOR "*** ERROR: Invalid selection. Please"
               " choose a valid room number. ***" RESET-COLOR
               DISPLAY " "
               DISPLAY "Press ENTER to try again..."
               ACCEPT WS-DUMMY-INPUT
               GO TO SELECT-ROOM
           END-IF.

       RECORD-BOOKER-INFO.
           DISPLAY CLEAR-SCREEN
           DISPLAY BLUE-COLOR
           DISPLAY "==================================================="
           "==========================="
           DISPLAY "                           CUSTOMER INFORMATION    "
           DISPLAY "==================================================="
           "==========================="
           RESET-COLOR
           DISPLAY "                                                   "
           DISPLAY "                      Please enter your informatio"
           "n                      "
           DISPLAY "  "
           DISPLAY "==================================================="
           "==========================="
           DISPLAY "                        Type 'CANCEL' to cancel    "
           DISPLAY "==================================================="
           "==========================="
           DISPLAY "Customer Name: "
           ACCEPT WS-BOOKER-NAME

           *> Check for cancellation (empty input)
           IF WS-BOOKER-NAME = SPACES OR WS-BOOKER-NAME = 'CANCEL'
               DISPLAY " "
               DISPLAY RED-COLOR "*** Booking cancelled by user. ***"
               RESET-COLOR
               DISPLAY " "
               DISPLAY "Press ENTER to continue..."
               ACCEPT WS-DUMMY-INPUT
               EXIT PARAGRAPH
           END-IF

           DISPLAY " "
           DISPLAY "Phone Number: "
           ACCEPT WS-BOOKER-PHONE

           *> Check for cancellation (empty input)
           IF WS-BOOKER-PHONE = SPACES OR WS-BOOKER-PHONE = 'CANCEL'
               DISPLAY " "
               DISPLAY RED-COLOR "*** Booking cancelled by user. ***"
               RESET-COLOR
               DISPLAY " "
               DISPLAY "Press ENTER to continue..."
               ACCEPT WS-DUMMY-INPUT
               MOVE SPACES TO WS-BOOKER-NAME
               EXIT PARAGRAPH
           END-IF.

       CREATE-BOOKING-RECORD.
           *> Find next booking ID
           OPEN INPUT BOOKING-FILE
           MOVE 0 TO WS-BOOKING-ID
           MOVE 'N' TO WS-EOF

           PERFORM UNTIL WS-EOF = 'Y'
               READ BOOKING-FILE NEXT RECORD
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

           *> Get current timestamp
           ACCEPT WS-CURRENT-TIMESTAMP FROM DATE YYYYMMDD
           STRING WS-TS-YEAR WS-TS-MONTH WS-TS-DAY
                  WS-TS-HOUR WS-TS-MIN WS-TS-SEC
                  DELIMITED BY SIZE INTO WS-CREATED-AT

           *> Write booking record
           OPEN I-O BOOKING-FILE
           MOVE WS-BOOKING-ID TO BOOKING-ID
           MOVE WS-SELECTED-ROOM TO ROOM-ID-BK
           MOVE WS-BOOKER-NAME TO CUSTOMER-NAME-BK
           MOVE WS-BOOKER-PHONE TO CUSTOMER-PH-BK
           MOVE WS-CHECKIN-DATE TO CHECKIN-DATE
           MOVE "Active" TO BOOKING-STATUS
           MOVE WS-CREATED-AT TO CREATED-AT
           WRITE BOOKING-RECORD
           CLOSE BOOKING-FILE.

       UPDATE-ROOM-STATUS.
           OPEN I-O ROOMS-FILE
           MOVE WS-SELECTED-ROOM TO ROOM-ID
           READ ROOMS-FILE KEY IS ROOM-ID
           INVALID KEY
               DISPLAY "Error: Could not find room to update."
           NOT INVALID KEY
               MOVE "Booked" TO R-STATUS
               REWRITE ROOMS-RECORD
           END-READ
           CLOSE ROOMS-FILE.

       CONFIRM-BOOKING.
           DISPLAY CLEAR-SCREEN
           DISPLAY YELLOW-COLOR
           DISPLAY "==================================================="
           "============================"
           DISPLAY "                         BOOKING CONFIRMATION      "
           DISPLAY "==================================================="
           "============================"
           RESET-COLOR
           DISPLAY "                                                   "
           DISPLAY "  Room:             " WS-SELECTED-ROOM
           DISPLAY "  Customer:           " WS-BOOKER-NAME
           DISPLAY "  Phone:            " WS-BOOKER-PHONE
           DISPLAY "  Check-in:         " WS-CHECKIN-DATE(1:4) "/"
                   WS-CHECKIN-DATE(5:2) "/" WS-CHECKIN-DATE(7:2)
           DISPLAY "                                                   "
           DISPLAY "==================================================="
           "============================"
           DISPLAY "Confirm booking? (Y/N): "
           ACCEPT WS-TEMP-CHAR

           IF WS-TEMP-CHAR = 'Y' OR WS-TEMP-CHAR = 'y'
               MOVE 'Y' TO WS-VALID-FLAG
               DISPLAY " "
               DISPLAY GREEN-COLOR "Booking confirmed." RESET-COLOR
           ELSE
               MOVE 'N' TO WS-VALID-FLAG
           END-IF.

       END PROGRAM bookRoom.
