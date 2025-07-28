
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Main.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 MENU-CHOICE PIC 99.

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

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM UNTIL MENU-CHOICE = 99
           DISPLAY CLEAR-SCREEN
           DISPLAY BLUE-COLOR
           DISPLAY "==================================================="
           "============================"
           DISPLAY "                    HOTEL RESERVATION MANAGEMENT S"
           "YSTEM                     "
           DISPLAY "==================================================="
           "============================"
           RESET-COLOR
           DISPLAY "                                                   "
           DISPLAY "                              1. Book a Room       "
           DISPLAY "                              2. Cancel Booking    "
           DISPLAY "                              3. Check-In          "
           DISPLAY "                              4. Check-Out         "
           DISPLAY "                              5. View Bookings     "
           DISPLAY "                              6. View Hotel Rooms  "
           DISPLAY "                              7. View Customers    "
           DISPLAY "                              8. View Check-In/Out "
           DISPLAY "                              9. View Stay Logs    "
          DISPLAY "                              10. Generate Daily Rep"
           "ort                          "
           DISPLAY "                              11. Generate Monthly "
           "Report                        "
           DISPLAY " "
           DISPLAY "==================================================="
           "============================"

           DISPLAY "                              99. Exit             "

           DISPLAY "==================================================="
           "============================"
           ACCEPT MENU-CHOICE

           EVALUATE MENU-CHOICE
               WHEN 1 PERFORM BOOK-ROOM
               WHEN 2 PERFORM CANCEL-BOOKING
               WHEN 3 PERFORM CHECK-IN
               WHEN 4 PERFORM CHECK-OUT
               WHEN 5 PERFORM VIEW-BOOKINGS
               WHEN 6 PERFORM VIEW-ROOMS
               WHEN 7 PERFORM VIEW-CUSTOMERS
               WHEN 8 PERFORM VIEW-CHECKINOUT
               WHEN 9 PERFORM VIEW-STAYLOGS
               WHEN 10 PERFORM DAILY-REPORT
               WHEN 11 PERFORM MONTHLY-REPORT
               WHEN 99
                   DISPLAY CLEAR-SCREEN
                   DISPLAY GREEN-COLOR
                   DISPLAY "==========================================="
                   "========"
                   "============================"
                   DISPLAY "                      THANK YOU FOR USING O"
                   "UR SYST"
                   "EM                        "
                   DISPLAY "==========================================="
                   "========"
                   "============================"
                   RESET-COLOR

                   STOP RUN
               WHEN OTHER
                   DISPLAY " "
                   DISPLAY RED-COLOR "*** ERROR: Invalid selection. Ple"
                   "ase choose a valid option (1-11, 99). ***"
                   RESET-COLOR
                   DISPLAY " "
                   DISPLAY "Press ENTER to continue..."
                   ACCEPT WS-DUMMY-INPUT
           END-EVALUATE
           END-PERFORM.
           STOP RUN.

       BOOK-ROOM.
           CALL 'bookRoom'.
           cancel 'bookRoom'.

       CANCEL-BOOKING.
           CALL 'cancelBooking'.
              cancel 'cancelBooking'.
       VIEW-BOOKINGS.
           CALL 'viewBookings'.
              cancel 'viewBookings'.
       CHECK-IN.
           CALL 'checkIn'.
                cancel 'checkIn'.
       CHECK-OUT.
           CALL 'checkOut'.
                cancel 'checkOut'.
       VIEW-ROOMS.
           CALL 'viewRooms'.
              cancel 'viewRooms'.
       VIEW-CUSTOMERS.
           CALL 'viewCustomers'.
                cancel 'viewCustomers'.
       VIEW-CHECKINOUT.
           CALL 'viewCheckInOut'.
                cancel 'viewCheckInOut'.
       VIEW-STAYLOGS.
           CALL 'viewStaylogs'.
                cancel 'viewStaylogs'.
       DAILY-REPORT.
           CALL 'dailyReport'.
                cancel 'dailyReport'.
       MONTHLY-REPORT.
           CALL 'monthlyReport'.
                cancel 'monthlyReport'.
       END PROGRAM Main.
