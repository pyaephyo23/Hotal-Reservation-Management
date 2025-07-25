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
       COPY "./CopyBooks/ROOMS.cpy".

       FD  BOOKING-FILE.
       COPY "./CopyBooks/BOOKINGS.cpy".

       WORKING-STORAGE SECTION.
       01 WS-CHOICE          PIC 9.
       01 WS-BOOKING-ID      PIC 9(5).
       01 WS-CUSTOMER-PHONE  PIC X(15).
       01 WS-ROOM-ID         PIC X(5).
       01 WS-FOUND           PIC X VALUE 'N'.
       01 WS-CURRENT-DATE    PIC X(8).
       01 WS-CANCELLED-COUNT PIC 999 VALUE 0.
       01 WS-EOF             PIC X VALUE 'N'.
       01 WS-BOOKING-COUNT   PIC 9(2) VALUE 0.
       01 WS-BOOKING-CHOICE  PIC 9(2).
       01 WS-BOOKING-ENTRY OCCURS 20 TIMES.
           05 WS-FOUND-BOOKING-ID   PIC 9(5).
           05 WS-FOUND-ROOM-ID      PIC X(5).
           05 WS-FOUND-CHECKIN-DATE PIC 9(8).
           05 WS-FOUND-CUSTOMER-NAME PIC X(30).

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
           DISPLAY "                           BOOKING CANCELLATION SY"
           "STEM                       "
           DISPLAY "==================================================="
           "============================"
           RESET-COLOR
           DISPLAY "                                                   "
           DISPLAY "                    1. Cancel Booking By Phone Num"
           "ber                    "
           DISPLAY "                    2. Cancel All Expired Bookings"
           " (Past Check-in Date)      "
           DISPLAY "                                                   "
           DISPLAY "==================================================="
           "============================"
           DISPLAY "                    9. Go Back to Main Menu        "

           DISPLAY "==================================================="
           "============================"
           ACCEPT WS-CHOICE
           EVALUATE WS-CHOICE
               WHEN 1
                   DISPLAY CLEAR-SCREEN
                   DISPLAY CYAN-COLOR
                   DISPLAY "==========================================="
                   "========"
                   "============================"
                   DISPLAY "                      CANCEL BOOKING BY PHO"
                   "NE     "
                   DISPLAY "==========================================="
                   "========"
                   "============================"
                   RESET-COLOR
                   DISPLAY "                            "

                   DISPLAY "Enter Customer Phone Number: "
                   ACCEPT WS-CUSTOMER-PHONE
                   PERFORM CANCEL-BOOKING-BY-PHONE
                   GO TO MAIN-PROCEDURE
               WHEN 2
                   PERFORM CANCEL-EXPIRED-BOOKINGS
                   GO TO MAIN-PROCEDURE
               WHEN 9
                   GOBACK
               WHEN OTHER
                   DISPLAY " "
                   DISPLAY RED-COLOR "*** ERROR: Invalid selection. Ple"
                   "as"
                   "e choose 1, 2, or 9. ***" RESET-COLOR
                   DISPLAY " "
                   DISPLAY "Press ENTER to continue..."
                   ACCEPT WS-DUMMY-INPUT
                   GO TO MAIN-PROCEDURE
           END-EVALUATE.
       CANCEL-BOOKING-BY-PHONE.
           *> Find active bookings for this phone number
           PERFORM FIND-ACTIVE-BOOKINGS-BY-PHONE
           IF WS-BOOKING-COUNT = 0
               DISPLAY " "
               DISPLAY RED-COLOR "No active bookings found for phone nu"
               "mber: " WS-CUSTOMER-PHONE RESET-COLOR
               DISPLAY " "
               DISPLAY "Press ENTER to continue..."
               ACCEPT WS-DUMMY-INPUT
               EXIT PARAGRAPH
           END-IF

           *> Display bookings and let user choose
           PERFORM DISPLAY-CUSTOMER-BOOKINGS
           PERFORM SELECT-BOOKING-TO-CANCEL.

       FIND-ACTIVE-BOOKINGS-BY-PHONE.
           MOVE 0 TO WS-BOOKING-COUNT
           MOVE 'N' TO WS-EOF

           OPEN INPUT BOOKING-FILE
           PERFORM UNTIL WS-EOF = 'Y'
           READ BOOKING-FILE NEXT
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   IF CUSTOMER-PH-BK = WS-CUSTOMER-PHONE
                     AND BOOKING-STATUS = 'Active'
                       ADD 1 TO WS-BOOKING-COUNT
                       IF WS-BOOKING-COUNT <= 20
                           MOVE BOOKING-ID TO
                               WS-FOUND-BOOKING-ID(WS-BOOKING-COUNT)
                           MOVE ROOM-ID-BK TO
                               WS-FOUND-ROOM-ID(WS-BOOKING-COUNT)
                           MOVE CHECKIN-DATE TO
                             WS-FOUND-CHECKIN-DATE(WS-BOOKING-COUNT)
                           MOVE CUSTOMER-NAME-BK TO
                             WS-FOUND-CUSTOMER-NAME(WS-BOOKING-COUNT)
                       END-IF
                   END-IF
           END-READ
           END-PERFORM
           CLOSE BOOKING-FILE.

       DISPLAY-CUSTOMER-BOOKINGS.
           DISPLAY CLEAR-SCREEN
           DISPLAY YELLOW-COLOR
           DISPLAY "==================================================="
           "==========================="
           DISPLAY "                    ACTIVE BOOKINGS FOR "
           WS-CUSTOMER-PHONE
           DISPLAY "==================================================="
           "==========================="
           RESET-COLOR
           PERFORM VARYING WS-BOOKING-CHOICE FROM 1 BY 1
                   UNTIL WS-BOOKING-CHOICE > WS-BOOKING-COUNT
               DISPLAY "  " WS-BOOKING-CHOICE ". Booking ID: "
                       WS-FOUND-BOOKING-ID(WS-BOOKING-CHOICE)
                       " | Room: "
                       WS-FOUND-ROOM-ID(WS-BOOKING-CHOICE)
               DISPLAY "     Customer: "
                       WS-FOUND-CUSTOMER-NAME(WS-BOOKING-CHOICE)
               DISPLAY "     Check-in: "
                   WS-FOUND-CHECKIN-DATE(WS-BOOKING-CHOICE)(1:4) "/"
                   WS-FOUND-CHECKIN-DATE(WS-BOOKING-CHOICE)(5:2) "/"
                   WS-FOUND-CHECKIN-DATE(WS-BOOKING-CHOICE)(7:2)
               DISPLAY " "
           END-PERFORM
           DISPLAY "==================================================="
           "==========================="
           DISPLAY "                           0. Cancel operation     "
           DISPLAY "==================================================="
           "===========================".

       SELECT-BOOKING-TO-CANCEL.
           DISPLAY "Select booking to cancel (1-" WS-BOOKING-COUNT
                   ") or 0 to cancel: "
           ACCEPT WS-BOOKING-CHOICE

           IF WS-BOOKING-CHOICE = 0
               DISPLAY " "
               DISPLAY RED-COLOR "Cancellation operation cancelled by u"
               "ser." RESET-COLOR
               DISPLAY " "
               DISPLAY "Press ENTER to continue..."
               ACCEPT WS-DUMMY-INPUT
           ELSE IF WS-BOOKING-CHOICE >= 1
               AND WS-BOOKING-CHOICE <= WS-BOOKING-COUNT
               MOVE WS-FOUND-BOOKING-ID(WS-BOOKING-CHOICE)
                   TO WS-BOOKING-ID
               PERFORM CANCEL-BOOKING-PROCESS
           ELSE
               DISPLAY " "
               DISPLAY RED-COLOR "*** ERROR: Invalid choice. Please tr"
               "y again. ***" RESET-COLOR
               DISPLAY " "
               DISPLAY "Press ENTER to continue..."
               ACCEPT WS-DUMMY-INPUT
               GO TO SELECT-BOOKING-TO-CANCEL
           END-IF.

       CANCEL-BOOKING-PROCESS.
           MOVE 'N' TO WS-FOUND
           OPEN I-O BOOKING-FILE
           MOVE WS-BOOKING-ID TO BOOKING-ID
           READ BOOKING-FILE KEY IS BOOKING-ID
               INVALID KEY
                   DISPLAY " "
                   DISPLAY RED-COLOR "*** ERROR: Invalid Booking ID. *"
                   "**"
                   RESET-COLOR
                   DISPLAY " "
                   DISPLAY "Press ENTER to continue..."
                   ACCEPT WS-DUMMY-INPUT
                   CLOSE BOOKING-FILE
                   EXIT PARAGRAPH
               NOT INVALID KEY
                   IF BOOKING-STATUS = 'Active'
                       MOVE "Cancelled" TO BOOKING-STATUS
                       MOVE ROOM-ID-BK TO WS-ROOM-ID
                       REWRITE BOOKING-RECORD
                           INVALID KEY
                               DISPLAY " "
                               DISPLAY RED-COLOR "*** ERROR: Unable to "
                               "rewrite booking record. ***" RESET-COLOR
                               DISPLAY " "
                               DISPLAY "Press ENTER to continue..."
                               ACCEPT WS-DUMMY-INPUT
                           NOT INVALID KEY
                               DISPLAY " "
                               DISPLAY GREEN-COLOR "Booking ID "
                               WS-BOOKING-ID " successfully cancelled."
                               RESET-COLOR
                               DISPLAY " "
                               DISPLAY "Press ENTER to continue..."
                               ACCEPT WS-DUMMY-INPUT
                       END-REWRITE
                       MOVE 'Y' TO WS-FOUND
                   ELSE
                       DISPLAY " "
                       DISPLAY YELLOW-COLOR "Booking is not Active."
                       RESET-COLOR
                       DISPLAY " "
                       DISPLAY "Press ENTER to continue..."
                       ACCEPT WS-DUMMY-INPUT
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
                   DISPLAY " "
                   DISPLAY YELLOW-COLOR "Associated Room not found."
                   RESET-COLOR
                   DISPLAY " "
               NOT INVALID KEY
                   *> Set room status to Available when booking is cancelled
                   MOVE "Available" TO R-STATUS

                   REWRITE ROOMS-RECORD
                       INVALID KEY
                           DISPLAY " "
                           DISPLAY RED-COLOR "*** ERROR: Unable to re"
                           "write room record. ***" RESET-COLOR
                           DISPLAY " "
                       NOT INVALID KEY
                           DISPLAY " "
                           DISPLAY GREEN-COLOR "Room ID " WS-ROOM-ID
                                   " status updated to Available."
                                   RESET-COLOR
                           DISPLAY " "
                   END-REWRITE
           END-READ
           CLOSE ROOMS-FILE.

       CANCEL-EXPIRED-BOOKINGS.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE
           MOVE 0 TO WS-CANCELLED-COUNT
           MOVE 'N' TO WS-EOF

           DISPLAY CLEAR-SCREEN
           DISPLAY CYAN-COLOR
           DISPLAY "==================================================="
           "============================"
           DISPLAY "                      CANCEL EXPIRED BOOKINGS     "
           DISPLAY "==================================================="
           "============================"
           RESET-COLOR
           DISPLAY "                                                   "
           DISPLAY "  Checking for expired bookings..."
           DISPLAY "  Current date: " WS-CURRENT-DATE(1:4) "/"
                   WS-CURRENT-DATE(5:2) "/" WS-CURRENT-DATE(7:2)
           DISPLAY " "

           OPEN I-O BOOKING-FILE

           *> Start reading from the beginning of the file
           START BOOKING-FILE KEY IS GREATER THAN OR EQUAL TO BOOKING-ID
           READ BOOKING-FILE NEXT RECORD
               AT END MOVE 'Y' TO WS-EOF
           END-READ

           PERFORM UNTIL WS-EOF = 'Y'
               *> Check if booking is active and past check-in date
               IF BOOKING-STATUS = 'Active'
                  AND CHECKIN-DATE < WS-CURRENT-DATE

                   DISPLAY YELLOW-COLOR "Cancelling expired booking:"
                   RESET-COLOR
                   DISPLAY "    Booking ID: " BOOKING-ID
                   DISPLAY "    Room: " ROOM-ID-BK
                   DISPLAY "    Customer: " CUSTOMER-NAME-BK
                   DISPLAY "    Check-in Date: " CHECKIN-DATE(1:4) "/"
                           CHECKIN-DATE(5:2) "/" CHECKIN-DATE(7:2)

                   *> Cancel the booking
                   MOVE "Cancelled" TO BOOKING-STATUS
                   MOVE ROOM-ID-BK TO WS-ROOM-ID
                   REWRITE BOOKING-RECORD
                       INVALID KEY
                           DISPLAY "    " RED-COLOR "Error: Unable to "
                           "cancel booking " BOOKING-ID RESET-COLOR
                       NOT INVALID KEY
                           ADD 1 TO WS-CANCELLED-COUNT
                           DISPLAY "    " GREEN-COLOR "Successfully "
                           "cancelled booking " BOOKING-ID RESET-COLOR
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
               DISPLAY YELLOW-COLOR "No expired bookings found to cance"
               "l."
               RESET-COLOR
           ELSE
               DISPLAY GREEN-COLOR "Expired bookings cancellation compl"
               "e"
               "ted." RESET-COLOR
               DISPLAY GREEN-COLOR "Total bookings cancelled: "
               WS-CANCELLED-COUNT RESET-COLOR
           END-IF
           DISPLAY " "
           DISPLAY "Press ENTER to continue..."
           ACCEPT WS-DUMMY-INPUT.

       UPDATE-ROOM-FOR-CANCELLATION.
           OPEN I-O ROOMS-FILE

           MOVE WS-ROOM-ID TO ROOM-ID
           READ ROOMS-FILE KEY IS ROOM-ID
               INVALID KEY
                   DISPLAY "    " YELLOW-COLOR "Warning: Associated "
                   "room " WS-ROOM-ID " not found." RESET-COLOR
               NOT INVALID KEY
                   *> Set room status to Available when booking is cancelled
                   MOVE "Available" TO R-STATUS

                   REWRITE ROOMS-RECORD
                       INVALID KEY
                           DISPLAY "    " RED-COLOR "Error: Unable to "
                           "update room " WS-ROOM-ID RESET-COLOR
                       NOT INVALID KEY
                           DISPLAY "    " GREEN-COLOR "Updated room "
                           WS-ROOM-ID " status to Available."
                           RESET-COLOR
                   END-REWRITE
           END-READ

           CLOSE ROOMS-FILE.

       END PROGRAM cancelBooking.
