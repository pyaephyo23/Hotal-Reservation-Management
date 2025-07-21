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
           DISPLAY "3. Check-In"
           DISPLAY "4. Check-Out"
           DISPLAY "5. View Rooms"
           DISPLAY "6. View Bookings"
           DISPLAY "7. View Customers"
           DISPLAY "8. View Invoices"
           DISPLAY "9. Generate Daily Summary Report"
           DISPLAY "10. Generate Monthly Summary Report"
           DISPLAY "99. Exit"
           DISPLAY
           "***********************************************************"
           ACCEPT MENU-CHOICE

           EVALUATE MENU-CHOICE
               WHEN 1 PERFORM BOOK-ROOM
               WHEN 2 PERFORM CANCEL-BOOKING
               WHEN 3 PERFORM CHECK-IN
               WHEN 4 PERFORM CHECK-OUT
               WHEN 5 PERFORM VIEW-ROOMS
               WHEN 6 PERFORM VIEW-BOOKINGS
               WHEN 7 PERFORM VIEW-CUSTOMERS
               WHEN 8 PERFORM VIEW-INVOICES
               WHEN 9 PERFORM GENERATE-DAILY-REPORT
               WHEN 10 PERFORM GENERATE-MONTHLY-REPORT
               WHEN OTHER DISPLAY "Invalid choice"
           END-EVALUATE
           END-PERFORM.
           STOP RUN.

       BOOK-ROOM.
           CALL 'bookRoom'
           CANCEL 'bookRoom'.
       CANCEL-BOOKING.
           CALL 'cancelBooking'
           CANCEL 'cancelBooking'.
       CHECK-IN.
           CALL 'checkIn'
           CANCEL 'checkIn'.
       CHECK-OUT.
           CALL 'checkOut'
           CANCEL 'checkOut'.
       VIEW-ROOMS.
           CALL 'viewRooms'
           CANCEL 'viewRooms'.
       VIEW-BOOKINGS.
           CALL 'viewBookings'
           CANCEL 'viewBookings'.
       VIEW-CUSTOMERS.
           CALL 'viewCustomers'
           CANCEL 'viewCustomers'.
       VIEW-INVOICES.
           CALL 'viewInvoices'
           CANCEL 'viewInvoices'.
       GENERATE-DAILY-REPORT.
           CALL 'dailySummaryReport'
           CANCEL 'dailySummaryReport'.
       GENERATE-MONTHLY-REPORT.
           CALL 'monthlySummaryReport'
           CANCEL 'monthlySummaryReport'.
       END PROGRAM Main.
