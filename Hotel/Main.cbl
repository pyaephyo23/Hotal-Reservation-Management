      ******************************************************************
      * Author: Kaung Myat Htun
      * Date:
      * Purpose: The Main program of Hotel Reservation Management System
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Main.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 MENU-CHOICE PIC 9.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM UNTIL MENU-CHOICE = 9
           DISPLAY
           "***********************************************************"
           DISPLAY "Hotel Reservation Management System"
           DISPLAY "1. Book a Room"
           DISPLAY "2. Cancel Booking"
           DISPLAY "3. Browse Bookings."
           DISPLAY "4. Check-In"
           DISPLAY "5. Check-Out"
           DISPLAY "6. View Hotel Rooms"
           DISPLAY "7. Generate Revenue Report"
           DISPLAY "8. View Customers"
           DISPLAY "9. Exit"
           DISPLAY
           "***********************************************************"
           ACCEPT MENU-CHOICE

           EVALUATE MENU-CHOICE
               WHEN 1 PERFORM BOOK-ROOM
               WHEN 2 PERFORM CANCEL-BOOKING
               WHEN 3 PERFORM VIEW-BOOKINGS
               WHEN 4 PERFORM CHECK-IN
               WHEN 5 PERFORM CHECK-OUT
               WHEN 6 PERFORM VIEW-ROOMS
               WHEN 7 PERFORM GENERATE-REPORT
               WHEN 8 PERFORM VIEW-CUSTOMERS
               WHEN 9 STOP RUN
               WHEN OTHER DISPLAY "Invalid choice"
           END-EVALUATE
           END-PERFORM.
           STOP RUN.

       BOOK-ROOM.
           CALL 'bookRoom'.
       CANCEL-BOOKING.
           CALL 'cancelBooking'.
       VIEW-BOOKINGS.
           CALL 'viewBookings'.
       CHECK-IN.
           CALL 'checkIn'.
       CHECK-OUT.
           CALL 'checkOut'.
       VIEW-ROOMS.
           CALL 'viewRooms'.
       VIEW-CUSTOMERS.
           CALL 'viewCustomers'.
       GENERATE-REPORT.
           CALL 'generateReport'.
       END PROGRAM Main.
