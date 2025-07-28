        IDENTIFICATION DIVISION.
       PROGRAM-ID. checkIn.
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
           SELECT CUSTOMER-FILE ASSIGN TO '../DATA/CUSTOMERS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUSTOMER-ID.
           SELECT STAYLOG-FILE ASSIGN TO '../DATA/STAYLOG.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS STAYLOG-ID.
           SELECT CHECKINOUT-FILE ASSIGN TO '../DATA/CHECKINOUT.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CHECKIN-ID.

       DATA DIVISION.
       FILE SECTION.
       FD  ROOMS-FILE.
       COPY "./CopyBooks/ROOMS.cpy".

       FD  BOOKING-FILE.
       COPY "./CopyBooks/BOOKINGS.cpy".

       FD  CUSTOMER-FILE.
       COPY "./CopyBooks/CUSTOMERS.cpy".

       FD  STAYLOG-FILE.
       COPY "./CopyBooks/STAYLOG.cpy".

       FD  CHECKINOUT-FILE.
       COPY "./CopyBooks/CHECKINOUT.cpy".

       WORKING-STORAGE SECTION.
       *> Navigation and control variables
       01 WS-CHOICE                  PIC 9.
       01 WS-FOUND                   PIC X VALUE 'N'.
       01 WS-CONFIRMATION            PIC X.
       01 WS-CONFIRM-INFO            PIC X.
       01 WS-EOF                     PIC X VALUE 'N'.
       01 WS-UPDATE-CHOICE           PIC X.

       *> Booking information
       01 WS-CUSTOMER-PHONE          PIC X(15).
       01 WS-ROOM-NUMBER             PIC X(5).
       01 WS-ROOM-TYPE               PIC X(10).
       01 WS-DATE-DISPLAY            PIC X(8).

       *> Customer count information
       01 WS-CUSTOMER-COUNT          PIC 9.
       01 WS-MAX-CUSTOMERS           PIC 9.
       01 WS-CURRENT-CUSTOMER        PIC 9.

       *> Time and date handling
       01 WS-CURRENT-DATE            PIC 9(8).
       01 WS-CURRENT-TIMESTAMP       PIC 9(14).
       01 WS-TIME-FORMATTED          PIC X(6).
       01 WS-CURRENT-TIME-FIELDS.
           05 WS-CURRENT-HOUR        PIC 9(2).
           05 WS-CURRENT-MINUTE      PIC 9(2).
           05 WS-CURRENT-SECOND      PIC 9(2).

       *> ID tracking variables
       01 WS-NEXT-CHECKIN-ID         PIC 9(5) VALUE 0.
       01 WS-NEXT-CUSTOMER-ID        PIC 9(5) VALUE 0.
       01 WS-NEXT-STAYLOG-ID         PIC 9(5) VALUE 0.
       01 WS-EXISTING-CUSTOMER       PIC X VALUE 'N'.
       01 WS-EXISTING-CUSTOMER-ID    PIC 9(5) VALUE 0.

       *> Guest information storage for all customers
       01 WS-GUEST-ARRAY.
           05 WS-GUEST-INFO OCCURS 9 TIMES INDEXED BY WS-GUEST-IDX.
               10 WS-GUEST-NAME-T    PIC X(20).
               10 WS-GUEST-PHONE-T   PIC X(15).
               10 WS-GUEST-AGE-T     PIC 9(3).
               10 WS-GUEST-GENDER-T  PIC X(1).
               10 WS-GUEST-NRC-T     PIC X(25).
               10 WS-GUEST-CHOICE    PIC X.
               10 WS-GUEST-CUST-ID   PIC 9(5).

       *> Working variables for current guest
       01 WS-CURRENT-GUEST.
           05 WS-GUEST-NAME          PIC X(20).
           05 WS-GUEST-PHONE         PIC X(15).
           05 WS-GUEST-AGE           PIC 9(3).
           05 WS-GUEST-GENDER        PIC X(1).
           05 WS-GUEST-NRC           PIC X(25).

       *> General error handling
       01 WS-ERROR-MESSAGE           PIC X(80).

       *> Validation variables
       01 WS-VALIDATION-PASSED       PIC X VALUE 'N'.
       01 WS-INPUT-VALID             PIC X VALUE 'N'.
       01 WS-TEMP-INPUT              PIC X(25).
       01 WS-CHAR-CHECK              PIC X.
       01 WS-CHAR-COUNT              PIC 9(3).
       01 WS-LOOP-COUNTER            PIC 9(3).
       01 WS-ROOM-COUNT-DSP              PIC ZZZ.

       *> Display formatting variables
       01 WS-GUEST-NUMBER            PIC 9.

       *> Walk-in check-in variables
       01 WS-SELECTED-ROOM-TYPE      PIC X(10).
       01 WS-AVAILABLE-ROOMS         PIC 9(3) VALUE 0.
       01 WS-ROOM-CHOICE             PIC X(5).
       01 WS-ROOM-DISPLAY-COUNT      PIC 9(3) VALUE 0.
       01 WS-ROOM-SELECTION          PIC 9(3).
       01 WS-ROOM-COUNTER            PIC 9(3) VALUE 0.

       *> Room array for selection
       01 WS-ROOM-ARRAY.
           05 WS-ROOM-LIST OCCURS 50 TIMES.
               10 WS-ROOM-ID-ARRAY   PIC X(5).
               10 WS-ROOM-TYPE-ARRAY PIC X(10).
               10 WS-ROOM-PRICE-ARRAY PIC $(9).

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
       MAIN-PROCEDURE.
           DISPLAY CLEAR-SCREEN
           DISPLAY BLUE-COLOR
           DISPLAY "==================================================="
           "============================"
           DISPLAY "                           GUEST CHECK-IN SYSTEM   "
           DISPLAY "==================================================="
           "============================"
           RESET-COLOR
           DISPLAY "                                                   "
           DISPLAY "                              1. Check In          "
           DISPLAY "                              2. Walk-in Check In  "
           DISPLAY "                                                   "
           DISPLAY "==================================================="
           "============================"
           DISPLAY "                              9. Go Back to Main Me"
           "nu            "
           DISPLAY "==================================================="
           "============================"
           ACCEPT WS-CHOICE
           EVALUATE WS-CHOICE
               WHEN 1
                   PERFORM CHECK-IN-PROCESS
                   GO TO MAIN-PROCEDURE
               WHEN 2
                   PERFORM WALKIN-CHECK-IN-PROCESS
                   GO TO MAIN-PROCEDURE
               WHEN 9
                   GOBACK
               WHEN OTHER
                   DISPLAY " "
                   DISPLAY RED-COLOR "*** ERROR: Invalid selection. Ple"
                   "ase choose 1, 2, or 9. ***" RESET-COLOR
                   DISPLAY " "
                   DISPLAY "Press ENTER to continue..."
                   ACCEPT WS-DUMMY-INPUT
                   GO TO MAIN-PROCEDURE
           END-EVALUATE.

      *****************************************************************
      * CHECK-IN MAIN PROCESS
      *****************************************************************
       CHECK-IN-PROCESS.
           PERFORM GET-BOOKING-DETAILS
           IF WS-FOUND = 'Y'
               PERFORM CONFIRM-CHECK-IN
               IF WS-FOUND = 'Y'
                   PERFORM EXECUTE-CHECK-IN
               END-IF
           END-IF.

      *****************************************************************
      * STEP 1: GET BOOKING DETAILS
      *****************************************************************
       GET-BOOKING-DETAILS.
           DISPLAY CLEAR-SCREEN
           DISPLAY CYAN-COLOR
           DISPLAY "==================================================="
           "============================"
           DISPLAY "                        BOOKING VERIFICATION       "
           DISPLAY "==================================================="
           "============================"
           RESET-COLOR
           DISPLAY "                                                   "
           DISPLAY "           Please provide booking details for verif"
           "ication                    "
           DISPLAY "                                                   "

           *> Accept booker phone number
           DISPLAY "Enter Customer Phone Number: "
           ACCEPT WS-CUSTOMER-PHONE
           DISPLAY ' '
           *> Accept room number
           DISPLAY "Enter Room Number: "
           ACCEPT WS-ROOM-NUMBER

           *> Search for active booking using phone and room
           PERFORM FIND-ACTIVE-BOOKING
           IF WS-FOUND = 'N'
               DISPLAY " "
               DISPLAY RED-COLOR "No active booking found for phone: "
                       FUNCTION TRIM(WS-CUSTOMER-PHONE)
                       " and room: " WS-ROOM-NUMBER RESET-COLOR
               DISPLAY " "
               DISPLAY "Press ENTER to continue..."
               ACCEPT WS-DUMMY-INPUT
           END-IF.

       FIND-ACTIVE-BOOKING.
           MOVE 'N' TO WS-FOUND
           MOVE 'N' TO WS-EOF

           OPEN I-O BOOKING-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ BOOKING-FILE NEXT
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF FUNCTION TRIM(CUSTOMER-PH-BK) =
                          FUNCTION TRIM(WS-CUSTOMER-PHONE)
                         AND FUNCTION TRIM(ROOM-ID-BK) =
                             FUNCTION TRIM(WS-ROOM-NUMBER)
                         AND FUNCTION TRIM(BOOKING-STATUS) = 'Active'
                           MOVE 'Y' TO WS-FOUND
                           *> Get room type from rooms file
                           PERFORM GET-ROOM-TYPE
                           PERFORM DISPLAY-BOOKING-INFO
                           MOVE 'Y' TO WS-EOF
                       END-IF
               END-READ
           END-PERFORM

           IF WS-FOUND = 'N'
               CLOSE BOOKING-FILE
           END-IF.

       GET-ROOM-TYPE.
           OPEN INPUT ROOMS-FILE
           MOVE ROOM-ID-BK TO ROOM-ID
           READ ROOMS-FILE KEY IS ROOM-ID
               INVALID KEY
                   MOVE "Unknown" TO WS-ROOM-TYPE
               NOT INVALID KEY
                   MOVE ROOM-TYPE TO WS-ROOM-TYPE
           END-READ
           CLOSE ROOMS-FILE.

       DISPLAY-BOOKING-INFO.
           DISPLAY CLEAR-SCREEN
           DISPLAY GREEN-COLOR
           DISPLAY "==================================================="
           "============================"
           DISPLAY "                        BOOKING FOUND              "
           DISPLAY "==================================================="
           "============================"
           RESET-COLOR
           DISPLAY "                                                   "
           DISPLAY "  Booking ID:       " BOOKING-ID
           DISPLAY "  Customer:         " CUSTOMER-NAME-BK
           DISPLAY "  Phone:            " CUSTOMER-PH-BK
           DISPLAY "  Room:             " ROOM-ID-BK " (" WS-ROOM-TYPE
           ")"
           MOVE CHECKIN-DATE TO WS-DATE-DISPLAY
           DISPLAY "  Check-in Date:    "
           WS-DATE-DISPLAY(1:4) "/"
           WS-DATE-DISPLAY(5:2) "/" WS-DATE-DISPLAY(7:2)
           DISPLAY "                                                   "
           DISPLAY "==================================================="
           "============================".

      *****************************************************************
      * STEP 2: CONFIRM CHECK-IN AND COLLECT GUEST INFORMATION
      *****************************************************************
       CONFIRM-CHECK-IN.
           DISPLAY CLEAR-SCREEN
           DISPLAY YELLOW-COLOR
           DISPLAY "==================================================="
           "============================"
           DISPLAY "                         CHECK-IN CONFIRMATION     "
           DISPLAY "==================================================="
           "============================"
           RESET-COLOR
           DISPLAY "                                                   "
           DISPLAY "Proceed with check-in? (Y/N): "
           ACCEPT WS-CONFIRMATION

           IF WS-CONFIRMATION = 'Y' OR WS-CONFIRMATION = 'y'
               MOVE 'Y' TO WS-FOUND
               DISPLAY " "
               DISPLAY GREEN-COLOR "Check-in confirmed." RESET-COLOR
               DISPLAY " "
               *> Determine number of customers based on room type
               PERFORM DETERMINE-CUSTOMER-COUNT
               *> Collect guest information for all customers
               PERFORM COLLECT-ALL-GUEST-INFO
           ELSE
               MOVE 'N' TO WS-FOUND
               DISPLAY " "
               DISPLAY RED-COLOR "Check-in cancelled." RESET-COLOR
               DISPLAY " "
               DISPLAY "Press ENTER to continue..."
               ACCEPT WS-DUMMY-INPUT
               CLOSE BOOKING-FILE
           END-IF.

       DETERMINE-CUSTOMER-COUNT.
           DISPLAY CLEAR-SCREEN
           DISPLAY CYAN-COLOR
           DISPLAY "==================================================="
           "============================"
           DISPLAY "                         GUEST COUNT SETUP         "
           DISPLAY "==================================================="
           "============================"
           RESET-COLOR
           DISPLAY "                                                   "

           EVALUATE TRUE
               WHEN WS-ROOM-TYPE = 'Single'
                   MOVE 1 TO WS-MAX-CUSTOMERS
                   DISPLAY "                   Single room - 1 guest "
                   "required.                   "

               WHEN WS-ROOM-TYPE = 'Double'
                   DISPLAY "           Double room - How many guests (1"
                   "or 2)?:                    "
                   ACCEPT WS-CUSTOMER-COUNT
                   IF WS-CUSTOMER-COUNT = 1 OR WS-CUSTOMER-COUNT = 2
                       MOVE WS-CUSTOMER-COUNT TO WS-MAX-CUSTOMERS
                   ELSE
                       DISPLAY " "
                      DISPLAY YELLOW-COLOR "Invalid input. Defaulting t"
                       "o 1 guest." RESET-COLOR
                       MOVE 1 TO WS-MAX-CUSTOMERS
                   END-IF

               WHEN WS-ROOM-TYPE = 'Delux'
                   DISPLAY "           Delux room - How many guests (1-"
                   "9)?:"
                   ACCEPT WS-CUSTOMER-COUNT
                   IF WS-CUSTOMER-COUNT >= 1 AND WS-CUSTOMER-COUNT <= 9
                       MOVE WS-CUSTOMER-COUNT TO WS-MAX-CUSTOMERS
                   ELSE
                       DISPLAY " "
                       DISPLAY YELLOW-COLOR "Invalid input. Defaulting "
                       "to 1 guest." RESET-COLOR
                       MOVE 1 TO WS-MAX-CUSTOMERS
                   END-IF

               WHEN OTHER
                   DISPLAY " "
                   DISPLAY YELLOW-COLOR "Unknown room type. Defaultin"
                   "g to 1 guest." RESET-COLOR
                   MOVE 1 TO WS-MAX-CUSTOMERS
           END-EVALUATE

           DISPLAY "                                                   "
           DISPLAY GREEN-COLOR "Collecting information for "
           WS-MAX-CUSTOMERS " guest(s)." RESET-COLOR
           DISPLAY " "
           DISPLAY "Press ENTER to continue..."
           ACCEPT WS-DUMMY-INPUT.

       COLLECT-ALL-GUEST-INFO.
           MOVE 1 TO WS-CURRENT-CUSTOMER
           PERFORM VARYING WS-GUEST-IDX FROM 1 BY 1
                   UNTIL WS-GUEST-IDX > WS-MAX-CUSTOMERS
               MOVE WS-GUEST-IDX TO WS-GUEST-NUMBER
               DISPLAY CLEAR-SCREEN
               DISPLAY BLUE-COLOR
               DISPLAY "==============================================="
               "===="
               "============================"
               DISPLAY "                         GUEST " WS-GUEST-NUMBER
               " INFORMATION                      "
               DISPLAY "==============================================="
               "===="
               "============================"
               RESET-COLOR
               DISPLAY "                                               "

               DISPLAY "              Please provide guest "
               WS-GUEST-NUMBER
               " details:                       "
               DISPLAY "                                               "


               *> Get basic guest information
               PERFORM GET-GUEST-BASIC-INFO

               *> Check for existing customer
               PERFORM CHECK-FOR-EXISTING-CUSTOMER
           END-PERFORM.

       GET-GUEST-BASIC-INFO.
           *> Get and validate guest name
           PERFORM GET-VALID-NAME

           *> Get and validate guest phone
           PERFORM GET-VALID-PHONE

           *> Move values for database lookup
           MOVE WS-GUEST-NAME-T(WS-GUEST-IDX) TO WS-GUEST-NAME
           MOVE WS-GUEST-PHONE-T(WS-GUEST-IDX) TO WS-GUEST-PHONE.

       CHECK-FOR-EXISTING-CUSTOMER.
           *> Search for existing customer
           PERFORM SEARCH-EXISTING-CUSTOMER

           IF WS-EXISTING-CUSTOMER = 'Y'
               *> Found existing customer - offer options
               PERFORM HANDLE-EXISTING-CUSTOMER
               *> Store the choice for this customer
               MOVE WS-UPDATE-CHOICE TO WS-GUEST-CHOICE(WS-GUEST-IDX)
               *> Store the existing customer ID
               MOVE WS-EXISTING-CUSTOMER-ID
               TO WS-GUEST-CUST-ID(WS-GUEST-IDX)

               EVALUATE WS-UPDATE-CHOICE
                   WHEN '1'
                       *> Skip data entry, use existing customer info
                       DISPLAY " "
                       DISPLAY GREEN-COLOR "Using existing customer inf"
                       "ormation." RESET-COLOR
                       DISPLAY GREEN-COLOR "Skipping remaining data ent"
                       "ry for this guest." RESET-COLOR
                       DISPLAY GREEN-COLOR "Guest " WS-GUEST-NUMBER
                       " information confirmed." RESET-COLOR
                       DISPLAY " "
                       DISPLAY "Press ENTER to continue..."
                       ACCEPT WS-DUMMY-INPUT

                   WHEN '2'
                       *> Continue with data entry for update
                       PERFORM GET-GUEST-DETAILED-INFO

                   WHEN '3'
                       *> Continue with data entry for new record
                       PERFORM GET-GUEST-DETAILED-INFO

                   WHEN OTHER
                       *> Invalid choice, treat as new customer
                       DISPLAY " "
                       DISPLAY YELLOW-COLOR "Invalid choice. Creating n"
                       "ew record." RESET-COLOR
                       DISPLAY " "
                       MOVE '3' TO WS-GUEST-CHOICE(WS-GUEST-IDX)
                       PERFORM GET-GUEST-DETAILED-INFO
               END-EVALUATE
           ELSE
               *> New customer, set choice to 3 (create new)
               MOVE '3' TO WS-GUEST-CHOICE(WS-GUEST-IDX)
               *> New customer, continue with normal data entry
               PERFORM GET-GUEST-DETAILED-INFO
           END-IF.

       SEARCH-EXISTING-CUSTOMER.
           *> Reset existing customer flag and ID
           MOVE 'N' TO WS-EXISTING-CUSTOMER
           MOVE 0 TO WS-EXISTING-CUSTOMER-ID
           MOVE 'N' TO WS-EOF

           OPEN INPUT CUSTOMER-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ CUSTOMER-FILE NEXT
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF FUNCTION TRIM(CUSTOMER-NAME) =
                          FUNCTION TRIM(WS-GUEST-NAME)
                         AND FUNCTION TRIM(CUSTOMER-PHONE) =
                             FUNCTION TRIM(WS-GUEST-PHONE)
                           MOVE 'Y' TO WS-EXISTING-CUSTOMER
                           MOVE CUSTOMER-ID TO WS-EXISTING-CUSTOMER-ID
                           MOVE 'Y' TO WS-EOF
                       END-IF
               END-READ
           END-PERFORM
           CLOSE CUSTOMER-FILE.

       HANDLE-EXISTING-CUSTOMER.
           DISPLAY " "
           DISPLAY GREEN-COLOR "✓ Found existing customer record!"
           RESET-COLOR
           DISPLAY "  Customer ID: " WS-EXISTING-CUSTOMER-ID
           DISPLAY "  Name: " WS-GUEST-NAME
           DISPLAY "  Phone: " WS-GUEST-PHONE
           DISPLAY " "
           DISPLAY YELLOW-COLOR "Customer information options:"
           RESET-COLOR
           DISPLAY "1. Use existing information (skip data entry)"
           DISPLAY "2. Update with new information (continue entry)"
           DISPLAY "3. Create new customer record (continue entry)"
           DISPLAY "Enter choice (1, 2, or 3): "
           ACCEPT WS-UPDATE-CHOICE.

       GET-GUEST-DETAILED-INFO.
           *> Get age with validation
           PERFORM GET-VALID-AGE

           *> Get and validate gender
           PERFORM GET-VALID-GENDER

           *> Get NRC with age validation
           PERFORM GET-VALID-NRC

           *> Show entered info and confirm
           PERFORM CONFIRM-GUEST-INFO.

       GET-VALID-AGE.
           MOVE 'N' TO WS-VALIDATION-PASSED
           PERFORM UNTIL WS-VALIDATION-PASSED = 'Y'
               DISPLAY "Guest Age (1-***): "
               ACCEPT WS-GUEST-AGE
               DISPLAY " "
               IF WS-GUEST-AGE IS NUMERIC
                   IF WS-GUEST-AGE >= 1 AND WS-GUEST-AGE <= 120
                       MOVE WS-GUEST-AGE TO WS-GUEST-AGE-T(WS-GUEST-IDX)
                       MOVE 'Y' TO WS-VALIDATION-PASSED
                   ELSE
                       DISPLAY " "
                       DISPLAY RED-COLOR
                      "Invalid age. Please enter age between 1 and 120."
                       RESET-COLOR
                       DISPLAY " "
                   END-IF
               ELSE
                   DISPLAY " "
                   DISPLAY RED-COLOR
                   "Invalid input. Please enter a numeric age."
                   RESET-COLOR
                   DISPLAY " "
               END-IF
           END-PERFORM.

       CONFIRM-GUEST-INFO.
           DISPLAY " "
           DISPLAY CYAN-COLOR "--- Entered Information ---" RESET-COLOR
           DISPLAY "Name: " WS-GUEST-NAME-T(WS-GUEST-IDX)
           DISPLAY "Phone: " WS-GUEST-PHONE-T(WS-GUEST-IDX)
           DISPLAY "Age: " WS-GUEST-AGE-T(WS-GUEST-IDX)
           DISPLAY "Gender: " WS-GUEST-GENDER-T(WS-GUEST-IDX)
           DISPLAY "NRC: " WS-GUEST-NRC-T(WS-GUEST-IDX)
           DISPLAY "Is this information correct? (Y/N): "
           DISPLAY " "
           ACCEPT WS-CONFIRM-INFO

           IF WS-CONFIRM-INFO = 'Y' OR WS-CONFIRM-INFO = 'y'
               DISPLAY " "
               DISPLAY GREEN-COLOR "Guest " WS-GUEST-NUMBER
               " information confirmed." RESET-COLOR
               DISPLAY GREEN-COLOR "==================================="
               "===="
               RESET-COLOR
               DISPLAY " "
               DISPLAY "Press ENTER to continue..."
               ACCEPT WS-DUMMY-INPUT
           ELSE
               *> If not confirmed, re-enter information for this guest
               DISPLAY " "
               DISPLAY YELLOW-COLOR "Please re-enter information for gu"
               "est"
               " " WS-GUEST-NUMBER RESET-COLOR
               DISPLAY YELLOW-COLOR "=================================="
               "====="
               RESET-COLOR
               DISPLAY " "
               PERFORM GET-GUEST-BASIC-INFO
               PERFORM CHECK-FOR-EXISTING-CUSTOMER
           END-IF.

      *****************************************************************
      * STEP 3: EXECUTE CHECK-IN PROCESS
      *****************************************************************
       EXECUTE-CHECK-IN.
           *> Check for duplicate check-in first
           PERFORM CHECK-DUPLICATE-CHECKIN
           IF WS-FOUND = 'N'
               EXIT PARAGRAPH
           END-IF

           *> Generate next check-in ID
           PERFORM GENERATE-NEXT-CHECKIN-ID

           *> Get current date and time
           PERFORM GET-CURRENT-DATETIME

           *> Create check-in record
           PERFORM CREATE-CHECKIN-RECORD

           *> Process all customer records
           PERFORM PROCESS-ALL-CUSTOMER-RECORDS

           *> Create staylog records for all guests
           PERFORM CREATE-ALL-STAYLOG-RECORDS

           *> Update booking and room status
           PERFORM UPDATE-BOOKING-STATUS
           PERFORM UPDATE-ROOM-TO-OCCUPIED

           *> Close booking file
           CLOSE BOOKING-FILE

           DISPLAY CLEAR-SCREEN
           DISPLAY GREEN-COLOR
           DISPLAY "==============================================="
           "================================"
           DISPLAY "                        CHECK-IN COMPLETED SUCCE"
           "SSFULLY                        "
           DISPLAY "==============================================="
           "================================"
           RESET-COLOR
           DISPLAY "                                                   "
           DISPLAY "          Guest has been successfully checked in!"
           DISPLAY "                                                   "
           DISPLAY "==============================================="
           "================================"
           DISPLAY " "
           DISPLAY "Press ENTER to continue..."
           ACCEPT WS-DUMMY-INPUT.

       GET-CURRENT-DATETIME.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-TIMESTAMP
           ACCEPT WS-CURRENT-TIME-FIELDS FROM TIME
           STRING WS-CURRENT-HOUR DELIMITED BY SIZE
                  WS-CURRENT-MINUTE DELIMITED BY SIZE
                  WS-CURRENT-SECOND DELIMITED BY SIZE
                  INTO WS-TIME-FORMATTED.

       CREATE-CHECKIN-RECORD.
           OPEN I-O CHECKINOUT-FILE
           MOVE WS-NEXT-CHECKIN-ID TO CHECKIN-ID
           MOVE BOOKING-ID TO BOOKING-ID-IO
           MOVE ROOM-ID-BK TO ROOM-ID-IO
           MOVE WS-CURRENT-DATE TO ACTUAL-CHECKIN-DATE
           MOVE WS-TIME-FORMATTED TO ACTUAL-CHECKIN-TIME
           MOVE 'N' TO CHECKOUT-FLAG
           MOVE 0 TO CHECKOUT-DATE

           WRITE CHECKINOUT-RECORD
               INVALID KEY
                   DISPLAY " "
                   DISPLAY RED-COLOR "*** ERROR: Unable to create ch"
                   "eck-in record. ***" RESET-COLOR
                   DISPLAY " "
               NOT INVALID KEY
                   DISPLAY " "
                   DISPLAY GREEN-COLOR "✓ Check-in record created suc"
                   "cessfully!" RESET-COLOR
                   DISPLAY "  Check-in ID: " CHECKIN-ID
                   DISPLAY "  Check-in Date: "
                   ACTUAL-CHECKIN-DATE(1:4) "/"
                           ACTUAL-CHECKIN-DATE(5:2) "/"
                           ACTUAL-CHECKIN-DATE(7:2)
                   DISPLAY "  Check-in Time: "
                   ACTUAL-CHECKIN-TIME(1:2) ":"
                           ACTUAL-CHECKIN-TIME(3:2) ":"
                           ACTUAL-CHECKIN-TIME(5:2)
                   DISPLAY " "
           END-WRITE
           CLOSE CHECKINOUT-FILE.

       PROCESS-ALL-CUSTOMER-RECORDS.
           PERFORM VARYING WS-GUEST-IDX FROM 1 BY 1
                   UNTIL WS-GUEST-IDX > WS-MAX-CUSTOMERS
               DISPLAY " "
               DISPLAY "Processing customer record "
                       FUNCTION TRIM(WS-GUEST-IDX)
                       " of " FUNCTION TRIM(WS-MAX-CUSTOMERS) "..."

               *> Load guest info to working area
               PERFORM LOAD-GUEST-INFO

               *> Process based on the choice
               EVALUATE WS-GUEST-CHOICE(WS-GUEST-IDX)
                   WHEN '1'
                       *> Option 1: Use existing customer - don't need to do anything
                       DISPLAY "✓ Using existing customer record!"
                       DISPLAY "  Customer ID: "
                              WS-GUEST-CUST-ID(WS-GUEST-IDX)

                   WHEN '2'
                       *> Option 2: Update existing customer
                       MOVE WS-GUEST-CUST-ID(WS-GUEST-IDX)
                          TO WS-EXISTING-CUSTOMER-ID
                       PERFORM UPDATE-EXISTING-CUSTOMER

                   WHEN '3'
                       *> Option 3: Create new customer record
                       PERFORM CREATE-CUSTOMER-RECORD
                       MOVE WS-NEXT-CUSTOMER-ID
                          TO WS-GUEST-CUST-ID(WS-GUEST-IDX)

                   WHEN OTHER
                       *> Should not happen - default to creating a new record
                       DISPLAY
                       "Warning: Invalid choice - creating new record"
                       PERFORM CREATE-CUSTOMER-RECORD
                       MOVE WS-NEXT-CUSTOMER-ID
                          TO WS-GUEST-CUST-ID(WS-GUEST-IDX)
               END-EVALUATE
           END-PERFORM.

       LOAD-GUEST-INFO.
           *> Move guest data to working variables
           MOVE WS-GUEST-NAME-T(WS-GUEST-IDX) TO WS-GUEST-NAME
           MOVE WS-GUEST-PHONE-T(WS-GUEST-IDX) TO WS-GUEST-PHONE
           MOVE WS-GUEST-AGE-T(WS-GUEST-IDX) TO WS-GUEST-AGE
           MOVE WS-GUEST-GENDER-T(WS-GUEST-IDX) TO WS-GUEST-GENDER
           MOVE WS-GUEST-NRC-T(WS-GUEST-IDX) TO WS-GUEST-NRC.

       UPDATE-EXISTING-CUSTOMER.
           OPEN I-O CUSTOMER-FILE
           MOVE WS-EXISTING-CUSTOMER-ID TO CUSTOMER-ID
           READ CUSTOMER-FILE KEY IS CUSTOMER-ID
               INVALID KEY
                   DISPLAY
                   "Error: Could not find customer record to update."
               NOT INVALID KEY
                   DISPLAY "Updating customer information..."
                   MOVE WS-GUEST-NAME TO CUSTOMER-NAME
                   MOVE WS-GUEST-PHONE TO CUSTOMER-PHONE
                   MOVE WS-GUEST-AGE TO CUSTOMER-AGE
                   MOVE WS-GUEST-GENDER TO CUSTOMER-GENDER
                   MOVE WS-GUEST-NRC TO NRC-NUMBER

                   REWRITE CUSTOMER-RECORD
                       INVALID KEY
                           DISPLAY
                           "Error: Unable to update customer record."
                       NOT INVALID KEY
                           DISPLAY
                           "✓ Customer record updated successfully!"
                           DISPLAY "  Customer ID: " CUSTOMER-ID
                           DISPLAY "  Updated Name: " CUSTOMER-NAME
                           DISPLAY "  Updated Phone: " CUSTOMER-PHONE
                   END-REWRITE
           END-READ
           CLOSE CUSTOMER-FILE.

       CREATE-CUSTOMER-RECORD.
           *> Generate next customer ID
           PERFORM GENERATE-NEXT-CUSTOMER-ID

           OPEN I-O CUSTOMER-FILE
           MOVE WS-NEXT-CUSTOMER-ID TO CUSTOMER-ID
           MOVE WS-GUEST-NAME TO CUSTOMER-NAME
           MOVE WS-GUEST-PHONE TO CUSTOMER-PHONE
           MOVE WS-GUEST-AGE TO CUSTOMER-AGE
           MOVE WS-GUEST-GENDER TO CUSTOMER-GENDER
           MOVE WS-GUEST-NRC TO NRC-NUMBER

           WRITE CUSTOMER-RECORD
               INVALID KEY
                   DISPLAY "Error: Unable to create customer record."
               NOT INVALID KEY
                   DISPLAY "✓ Customer record created successfully!"
                   DISPLAY "  Customer ID: " CUSTOMER-ID
                   DISPLAY "  Name: " CUSTOMER-NAME
                   DISPLAY "  Phone: " CUSTOMER-PHONE
           END-WRITE
           CLOSE CUSTOMER-FILE.

       CREATE-ALL-STAYLOG-RECORDS.
           PERFORM VARYING WS-GUEST-IDX FROM 1 BY 1
                   UNTIL WS-GUEST-IDX > WS-MAX-CUSTOMERS
               DISPLAY " "
               DISPLAY "Creating staylog record "
                       FUNCTION TRIM(WS-GUEST-IDX)
                       " of " FUNCTION TRIM(WS-MAX-CUSTOMERS) "..."

               *> Create the staylog record
               PERFORM CREATE-STAYLOG-RECORD
           END-PERFORM.

       CREATE-STAYLOG-RECORD.
           *> Generate next staylog ID
           PERFORM GENERATE-NEXT-STAYLOG-ID

           OPEN I-O STAYLOG-FILE
           MOVE WS-NEXT-STAYLOG-ID TO STAYLOG-ID
           *> Use the stored customer ID for this guest
           MOVE WS-GUEST-CUST-ID(WS-GUEST-IDX) TO CUSTOMER-ID-SL
           MOVE WS-NEXT-CHECKIN-ID TO CHECKIN-ID-SL
           MOVE BOOKING-ID OF BOOKING-RECORD TO BOOKING-ID-SL
           MOVE ROOM-ID-BK TO ROOM-ID-SL

           WRITE STAYLOG-RECORD
               INVALID KEY
                   DISPLAY "Error: Unable to create staylog record."
               NOT INVALID KEY
                   DISPLAY "✓ Staylog record created successfully!"
                   DISPLAY "  Staylog ID: " STAYLOG-ID
                   DISPLAY "  Customer ID: " CUSTOMER-ID-SL
                   DISPLAY "  Check-in ID: " CHECKIN-ID-SL
           END-WRITE
           CLOSE STAYLOG-FILE.

       UPDATE-BOOKING-STATUS.
           *> Update booking status to completed
           MOVE 'Completed' TO BOOKING-STATUS
           REWRITE BOOKING-RECORD
               INVALID KEY
                   DISPLAY "Error: Unable to update booking record."
               NOT INVALID KEY
                   DISPLAY "✓ Booking status updated to Completed"
                   DISPLAY "  Booking ID: " BOOKING-ID
                   DISPLAY "  Room: " ROOM-ID-BK
                   DISPLAY "  Check-in Time: "
                   WS-TIME-FORMATTED(1:2) ":"
                   WS-TIME-FORMATTED(3:2) ":" WS-TIME-FORMATTED(5:2)
           END-REWRITE.

       UPDATE-ROOM-TO-OCCUPIED.
           OPEN I-O ROOMS-FILE
           MOVE ROOM-ID-BK TO ROOM-ID
           READ ROOMS-FILE KEY IS ROOM-ID
               INVALID KEY
                   DISPLAY "Warning: Could not find room " ROOM-ID-BK
               NOT INVALID KEY
                   MOVE 'Occupied' TO R-STATUS
                   REWRITE ROOMS-RECORD
                       INVALID KEY
                          DISPLAY "Error: Unable to update room status."
                       NOT INVALID KEY
                           DISPLAY "✓ Room " ROOM-ID-BK
                                   " status updated to Occupied."
                   END-REWRITE
           END-READ
           CLOSE ROOMS-FILE.

      *****************************************************************
      * ID GENERATION PROCEDURES
      *****************************************************************
       GENERATE-NEXT-CHECKIN-ID.
           MOVE 'N' TO WS-EOF
           MOVE 0 TO WS-NEXT-CHECKIN-ID

           OPEN INPUT CHECKINOUT-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ CHECKINOUT-FILE NEXT
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF CHECKIN-ID > WS-NEXT-CHECKIN-ID
                           MOVE CHECKIN-ID TO WS-NEXT-CHECKIN-ID
                       END-IF
               END-READ
           END-PERFORM
           CLOSE CHECKINOUT-FILE

           ADD 1 TO WS-NEXT-CHECKIN-ID.

       GENERATE-NEXT-CUSTOMER-ID.
           MOVE 'N' TO WS-EOF
           MOVE 0 TO WS-NEXT-CUSTOMER-ID

           OPEN INPUT CUSTOMER-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ CUSTOMER-FILE NEXT
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF CUSTOMER-ID > WS-NEXT-CUSTOMER-ID
                           MOVE CUSTOMER-ID TO WS-NEXT-CUSTOMER-ID
                       END-IF
               END-READ
           END-PERFORM
           CLOSE CUSTOMER-FILE

           ADD 1 TO WS-NEXT-CUSTOMER-ID.

       GENERATE-NEXT-STAYLOG-ID.
           MOVE 'N' TO WS-EOF
           MOVE 0 TO WS-NEXT-STAYLOG-ID

           OPEN INPUT STAYLOG-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ STAYLOG-FILE NEXT
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF STAYLOG-ID > WS-NEXT-STAYLOG-ID
                           MOVE STAYLOG-ID TO WS-NEXT-STAYLOG-ID
                       END-IF
               END-READ
           END-PERFORM
           CLOSE STAYLOG-FILE

           ADD 1 TO WS-NEXT-STAYLOG-ID.

       CHECK-DUPLICATE-CHECKIN.
           MOVE 'Y' TO WS-FOUND
           MOVE 'N' TO WS-EOF

           DISPLAY " "
           DISPLAY "Checking for existing check-in records..."

           OPEN INPUT CHECKINOUT-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ CHECKINOUT-FILE NEXT
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                   *> Check if this booking ID already has a check-in
                   IF BOOKING-ID-IO = BOOKING-ID
                      DISPLAY CLEAR-SCREEN
                      DISPLAY RED-COLOR
                      DISPLAY "========================================"
                      "======="
                      "================================"
                      DISPLAY "                        DUPLICATE CHECK-"
                      "IN DE"
                      "TECTED!                        "
                      DISPLAY "========================================"
                      "======="
                      "================================"
                      RESET-COLOR
                      DISPLAY "                                        "

                      DISPLAY "        This booking has already been ch"
                      "ecked "
                      "in:                       "
                      DISPLAY "          Booking ID: " BOOKING-ID-IO
                      DISPLAY "          Check-in ID: " CHECKIN-ID
                      DISPLAY "          Room: " ROOM-ID-IO

                      *> Check if guest is currently checked out
                      IF CHECKOUT-FLAG = 'Y'
                          DISPLAY "          Status: Previously checked"
                          "out"
                          DISPLAY " "
                          DISPLAY "        This booking was already use"
                          "d for "
                          "a completed stay.             "
                      ELSE
                          DISPLAY "          Status: Currently active ("
                          "not c"
                          "hecked out)                  "
                          DISPLAY " "
                          DISPLAY "        Guest is currently checked "
                          "in!"
                      END-IF

                      DISPLAY " "
                      DISPLAY "        Cannot proceed with duplicate ch"
                      "eck-i"
                      "n.                        "
                      DISPLAY "======================================="
                      "========"
                      "================================"
                      DISPLAY " "
                      DISPLAY "Press ENTER to continue..."
                      ACCEPT WS-DUMMY-INPUT
                      MOVE 'N' TO WS-FOUND
                      MOVE 'Y' TO WS-EOF
                       END-IF
               END-READ
           END-PERFORM
           CLOSE CHECKINOUT-FILE

           IF WS-FOUND = 'Y'
               DISPLAY " "
               DISPLAY GREEN-COLOR "✓ No duplicate check-in found. Pr"
               "ocee"
               "ding..." RESET-COLOR
               DISPLAY " "
           END-IF.

      *****************************************************************
      * WALK-IN CHECK-IN MAIN PROCESS
      *****************************************************************
       WALKIN-CHECK-IN-PROCESS.
           PERFORM SELECT-ROOM-TYPE
           IF WS-FOUND = 'Y'
               PERFORM LIST-AVAILABLE-ROOMS
               IF WS-FOUND = 'Y'
                   PERFORM SELECT-ROOM
                   IF WS-FOUND = 'Y'
                       PERFORM WALKIN-COLLECT-GUEST-INFO
                       IF WS-FOUND = 'Y'

                           PERFORM EXECUTE-WALKIN-CHECK-IN
                       END-IF
                   END-IF
               END-IF
           END-IF.

       SELECT-ROOM-TYPE.
           DISPLAY CLEAR-SCREEN
           DISPLAY CYAN-COLOR
           DISPLAY "==================================================="
           "============================"
           DISPLAY "                             WALK-IN CHECK-IN      "

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
               WHEN 1
                   MOVE 'Single' TO WS-SELECTED-ROOM-TYPE
                   MOVE 'Y' TO WS-FOUND
               WHEN 2
                   MOVE 'Double' TO WS-SELECTED-ROOM-TYPE
                   MOVE 'Y' TO WS-FOUND
               WHEN 3
                   MOVE 'Delux' TO WS-SELECTED-ROOM-TYPE
                   MOVE 'Y' TO WS-FOUND
               WHEN 0
                   MOVE 'N' TO WS-FOUND
                   DISPLAY " "
                   DISPLAY RED-COLOR "*** Walk-in check-in cancelled by"
                   "user. ***"
                   RESET-COLOR
                   DISPLAY " "
               WHEN OTHER
                   DISPLAY " "
                   DISPLAY RED-COLOR "*** ERROR: Invalid selection. Ple"
                   "ase choose 1, 2, 3, or 0. ***" RESET-COLOR
                   DISPLAY " "
                   DISPLAY "Press ENTER to try again..."
                   ACCEPT WS-DUMMY-INPUT
                   MOVE 'N' TO WS-FOUND
           END-EVALUATE

           IF WS-FOUND = 'Y'
               DISPLAY " "
               DISPLAY GREEN-COLOR "Room type selected: "
               WS-SELECTED-ROOM-TYPE RESET-COLOR
               DISPLAY " "
           END-IF.

       LIST-AVAILABLE-ROOMS.
           DISPLAY CLEAR-SCREEN
           DISPLAY GREEN-COLOR
           DISPLAY "==================================================="
           "============================"
           DISPLAY
           "                      AVAILABLE " WS-SELECTED-ROOM-TYPE
           " ROOMS                            "
           DISPLAY "==================================================="
           "============================"
           RESET-COLOR
           DISPLAY "                                                   "

           MOVE 0 TO WS-AVAILABLE-ROOMS
           MOVE 0 TO WS-ROOM-DISPLAY-COUNT
           MOVE 0 TO WS-ROOM-COUNTER
           MOVE 'N' TO WS-EOF

           *> Initialize room array
           PERFORM VARYING WS-ROOM-COUNTER FROM 1 BY 1
           UNTIL WS-ROOM-COUNTER > 50
               MOVE SPACES TO WS-ROOM-ID-ARRAY(WS-ROOM-COUNTER)
               MOVE SPACES TO WS-ROOM-TYPE-ARRAY(WS-ROOM-COUNTER)
               MOVE 0 TO WS-ROOM-PRICE-ARRAY(WS-ROOM-COUNTER)
           END-PERFORM

           MOVE 0 TO WS-ROOM-COUNTER

           OPEN INPUT ROOMS-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ ROOMS-FILE NEXT
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF ROOM-TYPE = WS-SELECTED-ROOM-TYPE
                         AND R-STATUS = 'Available'
                           ADD 1 TO WS-AVAILABLE-ROOMS
                           ADD 1 TO WS-ROOM-COUNTER

                           *> Store room information in array
                           MOVE ROOM-ID
                           TO WS-ROOM-ID-ARRAY(WS-ROOM-COUNTER)
                           MOVE ROOM-TYPE
                           TO WS-ROOM-TYPE-ARRAY(WS-ROOM-COUNTER)
                           MOVE PRICE-PER-NIGHT
                           TO WS-ROOM-PRICE-ARRAY(WS-ROOM-COUNTER)

                           *> Display room
                           DISPLAY "                " WS-ROOM-COUNTER
                           ". Room " ROOM-ID " - " ROOM-TYPE
                           " ("
                  FUNCTION TRIM(WS-ROOM-PRICE-ARRAY(WS-ROOM-COUNTER))
                           "/night)                       "
                       END-IF
               END-READ
           END-PERFORM
           CLOSE ROOMS-FILE

           IF WS-AVAILABLE-ROOMS = 0
               DISPLAY " "
               DISPLAY RED-COLOR "Sorry, no available "
               FUNCTION TRIM(WS-SELECTED-ROOM-TYPE)
               " rooms found." RESET-COLOR
               DISPLAY " "
               DISPLAY "Press ENTER to continue..."
               ACCEPT WS-DUMMY-INPUT
               MOVE 'N' TO WS-FOUND
           ELSE
               DISPLAY " "
               MOVE WS-AVAILABLE-ROOMS TO WS-ROOM-COUNT-DSP
               DISPLAY GREEN-COLOR "Found " WS-ROOM-COUNT-DSP
               " available room(s)." RESET-COLOR
               DISPLAY " "
           DISPLAY "==================================================="
           "============================"
           DISPLAY "                            0. Cancel reservation  "
           DISPLAY "==================================================="
           "============================"
               MOVE 'Y' TO WS-FOUND
           END-IF.

       SELECT-ROOM.
           DISPLAY " "
           DISPLAY "Enter your choice (1-" WS-ROOM-COUNT-DSP
           " or 0 to go back): "
           ACCEPT WS-ROOM-SELECTION
           DISPLAY " "

           *> Validate selection
           IF WS-ROOM-SELECTION = 0
               *> Go back to room type selection
               MOVE 'N' TO WS-FOUND
           ELSE
               IF WS-ROOM-SELECTION >= 1 AND
                  WS-ROOM-SELECTION <= WS-AVAILABLE-ROOMS
                   *> Valid selection - get room info from array
                   MOVE WS-ROOM-ID-ARRAY(WS-ROOM-SELECTION)
                        TO WS-ROOM-NUMBER
                   MOVE WS-ROOM-TYPE-ARRAY(WS-ROOM-SELECTION)
                        TO WS-ROOM-TYPE

                   *> Verify room is still available
                   PERFORM VERIFY-ROOM-AVAILABILITY

                   IF WS-FOUND = 'Y'
                       DISPLAY " "
                       DISPLAY GREEN-COLOR "Room " WS-ROOM-NUMBER
                               " selected successfully!" RESET-COLOR
                       DISPLAY "Room Type: " WS-ROOM-TYPE
                       DISPLAY "Price: "
            FUNCTION TRIM(WS-ROOM-PRICE-ARRAY(WS-ROOM-SELECTION))
            "/night"
                       DISPLAY " "
                       DISPLAY "Press ENTER to continue..."
                       ACCEPT WS-DUMMY-INPUT
                   END-IF
               ELSE
                   *> Invalid selection
                   DISPLAY " "
                   MOVE WS-AVAILABLE-ROOMS TO WS-ROOM-COUNT-DSP
                   DISPLAY RED-COLOR "Invalid selection. Please choose "
                           "a number between 1 and " WS-ROOM-COUNT-DSP
                           " or 0 to go back." RESET-COLOR
                   DISPLAY " "
                   DISPLAY "Press ENTER to continue..."
                   ACCEPT WS-DUMMY-INPUT
                   MOVE 'N' TO WS-FOUND
               END-IF
           END-IF.

       VERIFY-ROOM-AVAILABILITY.
           *> Double-check that the selected room is still available
           OPEN INPUT ROOMS-FILE
           MOVE WS-ROOM-NUMBER TO ROOM-ID
           READ ROOMS-FILE KEY IS ROOM-ID
               INVALID KEY
                   DISPLAY " "
                   DISPLAY RED-COLOR "Error: Room " WS-ROOM-NUMBER
                           " not found." RESET-COLOR
                   DISPLAY " "
                   DISPLAY "Press ENTER to continue..."
                   ACCEPT WS-DUMMY-INPUT
                   MOVE 'N' TO WS-FOUND
               NOT INVALID KEY
                   IF R-STATUS = 'Available'
                       MOVE 'Y' TO WS-FOUND
                   ELSE
                       DISPLAY " "
                       DISPLAY RED-COLOR "Sorry, room " WS-ROOM-NUMBER
                               " is no longer available." RESET-COLOR
                       DISPLAY " "
                       DISPLAY "Press ENTER to continue..."
                       ACCEPT WS-DUMMY-INPUT
                       MOVE 'N' TO WS-FOUND
                   END-IF
           END-READ
           CLOSE ROOMS-FILE.

       WALKIN-COLLECT-GUEST-INFO.
           DISPLAY " "
           DISPLAY "Proceed with guest information collection? (Y/N): "

           ACCEPT WS-CONFIRMATION
           DISPLAY " "

           IF WS-CONFIRMATION = 'Y' OR WS-CONFIRMATION = 'y'
               MOVE 'Y' TO WS-FOUND
               *> Determine number of customers based on room type
               PERFORM WALKIN-DETERMINE-CUSTOMER-COUNT
               *> Collect guest information for all customers
               PERFORM COLLECT-ALL-GUEST-INFO
           ELSE
               MOVE 'N' TO WS-FOUND
               DISPLAY " "
               DISPLAY RED-COLOR "Walk-in check-in cancelled."
               RESET-COLOR
               DISPLAY " "
               DISPLAY "Press ENTER to continue..."
               ACCEPT WS-DUMMY-INPUT
           END-IF.

       WALKIN-DETERMINE-CUSTOMER-COUNT.
           DISPLAY CLEAR-SCREEN
           DISPLAY CYAN-COLOR
           DISPLAY "==================================================="
           "============================"
           DISPLAY "                             GUEST COUNT SETUP     "

           DISPLAY "==================================================="
           "============================"
           RESET-COLOR
           DISPLAY "                                                   "

           EVALUATE TRUE
               WHEN WS-ROOM-TYPE = 'Single'
                   MOVE 1 TO WS-MAX-CUSTOMERS
                   DISPLAY "                   Single room - 1 guest "
                   "required.                   "

               WHEN WS-ROOM-TYPE = 'Double'
                   DISPLAY "           Double room - How many guests (1"
                   " or 2)?:                    "
                   ACCEPT WS-CUSTOMER-COUNT
                   IF WS-CUSTOMER-COUNT = 1 OR WS-CUSTOMER-COUNT = 2
                       MOVE WS-CUSTOMER-COUNT TO WS-MAX-CUSTOMERS
                   ELSE
                       DISPLAY " "
                       DISPLAY YELLOW-COLOR "Invalid input. Defaulting "
                       "to 1 guest." RESET-COLOR
                       MOVE 1 TO WS-MAX-CUSTOMERS
                   END-IF

               WHEN WS-ROOM-TYPE = 'Delux'
                   DISPLAY "              Delux room - How many guests"
                   " (1-9)?:                           "
                   ACCEPT WS-CUSTOMER-COUNT
                   IF WS-CUSTOMER-COUNT >= 1 AND WS-CUSTOMER-COUNT <= 9
                       MOVE WS-CUSTOMER-COUNT TO WS-MAX-CUSTOMERS
                   ELSE
                       DISPLAY " "
                       DISPLAY YELLOW-COLOR "Invalid input. Defaulting "
                       "to 1 guest." RESET-COLOR
                       MOVE 1 TO WS-MAX-CUSTOMERS
                   END-IF

               WHEN OTHER
                   DISPLAY " "
                   DISPLAY YELLOW-COLOR "Unknown room type. Defaulting"
                   " to 1 guest." RESET-COLOR
                   MOVE 1 TO WS-MAX-CUSTOMERS
           END-EVALUATE

           DISPLAY " "
           DISPLAY GREEN-COLOR "Collecting information for "
           WS-MAX-CUSTOMERS " guest(s)." RESET-COLOR
           DISPLAY " "
           DISPLAY "Press ENTER to continue..."
           ACCEPT WS-DUMMY-INPUT.

       EXECUTE-WALKIN-CHECK-IN.
           *> Generate next check-in ID
           PERFORM GENERATE-NEXT-CHECKIN-ID

           *> Get current date and time
           PERFORM GET-CURRENT-DATETIME

           *> Create check-in record for walk-in (no booking ID)
           PERFORM CREATE-WALKIN-CHECKIN-RECORD

           *> Process all customer records
           PERFORM PROCESS-ALL-CUSTOMER-RECORDS

           *> Create staylog records for all guests
           PERFORM CREATE-ALL-WALKIN-SLREC

           *> Update room status to occupied
           PERFORM UPDATE-WALKIN-ROOM-TO-OCCUPIED

           DISPLAY CLEAR-SCREEN
           DISPLAY GREEN-COLOR
           DISPLAY "==============================================="
           "================================"
           DISPLAY "                      WALK-IN CHECK-IN COMPLETED "
           "SUCCESSFULLY                   "
           DISPLAY "==============================================="
           "================================"
           RESET-COLOR
           DISPLAY " "
           DISPLAY "            Walk-in guest has been successfully che"
           "cked in!"
           DISPLAY " "
           DISPLAY "==============================================="
           "================================"
           DISPLAY " "
           DISPLAY "Press ENTER to continue..."
           ACCEPT WS-DUMMY-INPUT.

       CREATE-WALKIN-CHECKIN-RECORD.
           OPEN I-O CHECKINOUT-FILE
           MOVE WS-NEXT-CHECKIN-ID TO CHECKIN-ID
           MOVE 0 TO BOOKING-ID-IO  *> Zero for walk-in
           MOVE WS-ROOM-NUMBER TO ROOM-ID-IO
           MOVE WS-CURRENT-DATE TO ACTUAL-CHECKIN-DATE
           MOVE WS-TIME-FORMATTED TO ACTUAL-CHECKIN-TIME
           MOVE 'N' TO CHECKOUT-FLAG
           MOVE 0 TO CHECKOUT-DATE

           WRITE CHECKINOUT-RECORD
               INVALID KEY
                   DISPLAY " "
                   DISPLAY RED-COLOR
                   "*** ERROR: Unable to create walk-in"
                   " check-in record. ***" RESET-COLOR
                   DISPLAY " "
               NOT INVALID KEY
                   DISPLAY " "
                   DISPLAY GREEN-COLOR
                   "✓ Walk-in check-in record created"
                   " successfully!" RESET-COLOR
                   DISPLAY "  Check-in ID: " CHECKIN-ID
                   DISPLAY "  Room: " ROOM-ID-IO
                   DISPLAY "  Check-in Date: "
                   ACTUAL-CHECKIN-DATE(1:4) "/"
                           ACTUAL-CHECKIN-DATE(5:2) "/"
                           ACTUAL-CHECKIN-DATE(7:2)
                   DISPLAY "  Check-in Time: "
                   ACTUAL-CHECKIN-TIME(1:2) ":"
                           ACTUAL-CHECKIN-TIME(3:2) ":"
                           ACTUAL-CHECKIN-TIME(5:2)
                   DISPLAY " "
           END-WRITE
           CLOSE CHECKINOUT-FILE.

       CREATE-ALL-WALKIN-SLREC.
           PERFORM VARYING WS-GUEST-IDX FROM 1 BY 1
                   UNTIL WS-GUEST-IDX > WS-MAX-CUSTOMERS
               DISPLAY " "
               DISPLAY "Creating walk-in staylog record "
                       FUNCTION TRIM(WS-GUEST-IDX)
                       " of " FUNCTION TRIM(WS-MAX-CUSTOMERS) "..."

               *> Create the staylog record
               PERFORM CREATE-WALKIN-STAYLOG-RECORD
           END-PERFORM.

       CREATE-WALKIN-STAYLOG-RECORD.
           *> Generate next staylog ID
           PERFORM GENERATE-NEXT-STAYLOG-ID

           OPEN I-O STAYLOG-FILE
           MOVE WS-NEXT-STAYLOG-ID TO STAYLOG-ID
           *> Use the stored customer ID for this guest
           MOVE WS-GUEST-CUST-ID(WS-GUEST-IDX) TO CUSTOMER-ID-SL
           MOVE WS-NEXT-CHECKIN-ID TO CHECKIN-ID-SL
           MOVE 0 TO BOOKING-ID-SL  *> Zero for walk-in
           MOVE WS-ROOM-NUMBER TO ROOM-ID-SL

           WRITE STAYLOG-RECORD
               INVALID KEY
                   DISPLAY
                   "Error: Unable to create walk-in staylog record."
               NOT INVALID KEY
                   DISPLAY
                   "✓ Walk-in staylog record created successfully!"
                   DISPLAY "  Staylog ID: " STAYLOG-ID
                   DISPLAY "  Customer ID: " CUSTOMER-ID-SL
                   DISPLAY "  Check-in ID: " CHECKIN-ID-SL
           END-WRITE
           CLOSE STAYLOG-FILE.

       UPDATE-WALKIN-ROOM-TO-OCCUPIED.
           OPEN I-O ROOMS-FILE
           MOVE WS-ROOM-NUMBER TO ROOM-ID
           READ ROOMS-FILE KEY IS ROOM-ID
               INVALID KEY
                   DISPLAY
                   "Warning: Could not find room " WS-ROOM-NUMBER
               NOT INVALID KEY
                   MOVE 'Occupied' TO R-STATUS
                   REWRITE ROOMS-RECORD
                       INVALID KEY
                          DISPLAY "Error: Unable to update room status."
                       NOT INVALID KEY
                           DISPLAY "✓ Room " WS-ROOM-NUMBER
                                   " status updated to Occupied."
                   END-REWRITE
           END-READ
           CLOSE ROOMS-FILE.

       GET-VALID-NAME.
           MOVE 'N' TO WS-VALIDATION-PASSED
           PERFORM UNTIL WS-VALIDATION-PASSED = 'Y'
               DISPLAY "Guest Name (2-20 characters, letters only): "
               ACCEPT WS-TEMP-INPUT
               DISPLAY " "

               *> Check if input is not empty and within length limits
               IF FUNCTION LENGTH(FUNCTION TRIM(WS-TEMP-INPUT)) >= 2 AND
                  FUNCTION LENGTH(FUNCTION TRIM(WS-TEMP-INPUT)) <= 20
                   *> Validate that name contains only letters and spaces
                   PERFORM VALIDATE-NAME-CHARACTERS
                   IF WS-INPUT-VALID = 'Y'
                       MOVE FUNCTION TRIM(WS-TEMP-INPUT)
                            TO WS-GUEST-NAME-T(WS-GUEST-IDX)
                       MOVE 'Y' TO WS-VALIDATION-PASSED
                   ELSE
                       DISPLAY " "
                       DISPLAY RED-COLOR
                       "Invalid name. Use only letters and spaces."
                       RESET-COLOR
                       DISPLAY " "
                   END-IF
               ELSE
                   DISPLAY " "
                   DISPLAY RED-COLOR
                   "Invalid length. Name must be 2-20 characters."
                   RESET-COLOR
                   DISPLAY " "
               END-IF
           END-PERFORM.

       VALIDATE-NAME-CHARACTERS.
           MOVE 'Y' TO WS-INPUT-VALID
           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-TEMP-INPUT))
                TO WS-CHAR-COUNT

           PERFORM VARYING WS-LOOP-COUNTER FROM 1 BY 1
                   UNTIL WS-LOOP-COUNTER > WS-CHAR-COUNT
               MOVE WS-TEMP-INPUT(WS-LOOP-COUNTER:1) TO WS-CHAR-CHECK
               IF WS-CHAR-CHECK NOT ALPHABETIC AND
                  WS-CHAR-CHECK NOT = SPACE
                   MOVE 'N' TO WS-INPUT-VALID
                   MOVE WS-CHAR-COUNT TO WS-LOOP-COUNTER
               END-IF
           END-PERFORM.

       GET-VALID-PHONE.
           MOVE 'N' TO WS-VALIDATION-PASSED
           PERFORM UNTIL WS-VALIDATION-PASSED = 'Y'
               DISPLAY "Guest Phone Number (6-11 digits): "
               ACCEPT WS-TEMP-INPUT
               DISPLAY " "
               *> Check if input length is valid
               IF FUNCTION LENGTH(FUNCTION TRIM(WS-TEMP-INPUT)) >= 6 AND
                  FUNCTION LENGTH(FUNCTION TRIM(WS-TEMP-INPUT)) <= 11
                   *> Validate that phone contains only digits
                   PERFORM VALIDATE-PHONE-CHARACTERS
                   IF WS-INPUT-VALID = 'Y'
                       MOVE FUNCTION TRIM(WS-TEMP-INPUT)
                            TO WS-GUEST-PHONE-T(WS-GUEST-IDX)
                       MOVE 'Y' TO WS-VALIDATION-PASSED
                   ELSE
                       DISPLAY " "
                       DISPLAY RED-COLOR
                       "Invalid phone. Use only digits (0-9)."
                       RESET-COLOR
                       DISPLAY " "
                   END-IF
               ELSE
                   DISPLAY " "
                   DISPLAY RED-COLOR
                   "Invalid length. Phone must be 6-11 digits."
                   RESET-COLOR
                   DISPLAY " "
               END-IF
           END-PERFORM.

       VALIDATE-PHONE-CHARACTERS.
           MOVE 'Y' TO WS-INPUT-VALID
           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-TEMP-INPUT))
                TO WS-CHAR-COUNT

           PERFORM VARYING WS-LOOP-COUNTER FROM 1 BY 1
                   UNTIL WS-LOOP-COUNTER > WS-CHAR-COUNT
               MOVE WS-TEMP-INPUT(WS-LOOP-COUNTER:1) TO WS-CHAR-CHECK
               IF WS-CHAR-CHECK NOT NUMERIC
                   MOVE 'N' TO WS-INPUT-VALID
                   MOVE WS-CHAR-COUNT TO WS-LOOP-COUNTER
               END-IF
           END-PERFORM.

       GET-VALID-GENDER.
           MOVE 'N' TO WS-VALIDATION-PASSED
           PERFORM UNTIL WS-VALIDATION-PASSED = 'Y'
               DISPLAY "Guest Gender (M/F): "
               ACCEPT WS-CHAR-CHECK

               EVALUATE FUNCTION UPPER-CASE(WS-CHAR-CHECK)
                   WHEN 'M'
                       MOVE 'M' TO WS-GUEST-GENDER-T(WS-GUEST-IDX)
                       MOVE 'Y' TO WS-VALIDATION-PASSED
                   WHEN 'F'
                       MOVE 'F' TO WS-GUEST-GENDER-T(WS-GUEST-IDX)
                       MOVE 'Y' TO WS-VALIDATION-PASSED
                   WHEN OTHER
                       DISPLAY " "
                       DISPLAY RED-COLOR
                       "Invalid gender. Please enter M or F."
                       RESET-COLOR
                       DISPLAY " "
               END-EVALUATE
           END-PERFORM.

       GET-VALID-NRC.
           *> Check if guest is 18 or older
           IF WS-GUEST-AGE-T(WS-GUEST-IDX) >= 18
               DISPLAY " "
               DISPLAY YELLOW-COLOR
               "Guest is 18 or older - NRC is required."
               RESET-COLOR
               DISPLAY " "

               MOVE 'N' TO WS-VALIDATION-PASSED
               PERFORM UNTIL WS-VALIDATION-PASSED = 'Y'
                   DISPLAY "NRC Number (format: 12/LLLLLL(L)123456): "
                   ACCEPT WS-TEMP-INPUT

                   *> Check if NRC is provided and has minimum length
             IF FUNCTION LENGTH(FUNCTION TRIM(WS-TEMP-INPUT)) >= 10 AND
                     FUNCTION LENGTH(FUNCTION TRIM(WS-TEMP-INPUT)) <= 25
                       *> Basic format validation
                       PERFORM VALIDATE-NRC-FORMAT
                       IF WS-INPUT-VALID = 'Y'
                           MOVE FUNCTION TRIM(WS-TEMP-INPUT)
                                TO WS-GUEST-NRC-T(WS-GUEST-IDX)
                           MOVE 'Y' TO WS-VALIDATION-PASSED
                       ELSE
                           DISPLAY " "
                           DISPLAY RED-COLOR
                        "Invalid NRC format. Please check and re-enter."
                           RESET-COLOR
                           DISPLAY " "
                       END-IF
                   ELSE
                       DISPLAY " "
                       DISPLAY RED-COLOR
                    "NRC too short or too long. Please enter valid NRC."
                       RESET-COLOR
                       DISPLAY " "
                   END-IF
               END-PERFORM
           ELSE
               *> Guest is under 18, NRC is optional
               DISPLAY " "
               DISPLAY CYAN-COLOR
               "Guest is under 18 - NRC is optional."
               RESET-COLOR
               DISPLAY "NRC Number (optional, press ENTER to skip): "
               ACCEPT WS-TEMP-INPUT

               IF FUNCTION TRIM(WS-TEMP-INPUT) = SPACES
                   MOVE "N/A" TO WS-GUEST-NRC-T(WS-GUEST-IDX)
               ELSE
                   *> If provided, validate it
                  IF FUNCTION LENGTH(FUNCTION TRIM(WS-TEMP-INPUT)) >= 10
                       PERFORM VALIDATE-NRC-FORMAT
                       IF WS-INPUT-VALID = 'Y'
                           MOVE FUNCTION TRIM(WS-TEMP-INPUT)
                                TO WS-GUEST-NRC-T(WS-GUEST-IDX)
                       ELSE
                           DISPLAY " "
                           DISPLAY YELLOW-COLOR
                           "Invalid NRC format. Setting to N/A."
                           RESET-COLOR
                           MOVE "N/A" TO WS-GUEST-NRC-T(WS-GUEST-IDX)
                       END-IF
                   ELSE
                       DISPLAY " "
                       DISPLAY YELLOW-COLOR
                       "NRC too short. Setting to N/A."
                       RESET-COLOR
                       MOVE "N/A" TO WS-GUEST-NRC-T(WS-GUEST-IDX)
                   END-IF
               END-IF
           END-IF.

       VALIDATE-NRC-FORMAT.
           *> Basic NRC format validation
           *> This is a simplified validation - you can enhance as needed
           MOVE 'Y' TO WS-INPUT-VALID

           *> Check if it contains some digits and some letters
           MOVE 0 TO WS-CHAR-COUNT
           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-TEMP-INPUT))
                TO WS-LOOP-COUNTER

           *> Simple check: NRC should contain both numbers and letters
           PERFORM VARYING WS-CHAR-COUNT FROM 1 BY 1
                   UNTIL WS-CHAR-COUNT > WS-LOOP-COUNTER
               MOVE WS-TEMP-INPUT(WS-CHAR-COUNT:1) TO WS-CHAR-CHECK
               *> Allow digits, letters, parentheses, and forward slash
               IF WS-CHAR-CHECK NOT NUMERIC AND
                  WS-CHAR-CHECK NOT ALPHABETIC AND
                  WS-CHAR-CHECK NOT = '/' AND
                  WS-CHAR-CHECK NOT = '(' AND
                  WS-CHAR-CHECK NOT = ')'
                   MOVE 'N' TO WS-INPUT-VALID
                   MOVE WS-LOOP-COUNTER TO WS-CHAR-COUNT
               END-IF
           END-PERFORM.
       END PROGRAM checkIn.
