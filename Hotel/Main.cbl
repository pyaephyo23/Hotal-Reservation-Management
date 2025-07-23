
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Main.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 MENU-CHOICE PIC 99.
       01 RED-COLOR PIC X(10) VALUE X"1B5B33316D".
       01 BLUE-COLOR PIC X(10) VALUE X"1B5B33346D".
       01 GREEN-COLOR PIC X(10) VALUE X"1B5B33326D".
       01 RESET-COLOR PIC X(4) VALUE X"1B5B306D".
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM UNTIL MENU-CHOICE = 99
           DISPLAY
           "**************************************************"
           DISPLAY BLUE-COLOR WITH NO ADVANCING
           DISPLAY "Hotel Reservation Management System"
           WITH NO ADVANCING
           DISPLAY RESET-COLOR

           DISPLAY
           "**************************************************"
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
           "**************************************************"
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
               WHEN 99
                   DISPLAY GREEN-COLOR WITH NO ADVANCING
                   DISPLAY "Thank you for using Our System!"
                           WITH NO ADVANCING
                   DISPLAY RESET-COLOR
                   DISPLAY GREEN-COLOR WITH NO ADVANCING
                   DISPLAY "Goodbye!" WITH NO ADVANCING
                   DISPLAY RESET-COLOR
               WHEN OTHER
                   DISPLAY RED-COLOR WITH NO ADVANCING
                   DISPLAY "Invalid selection." WITH NO ADVANCING
                   DISPLAY RESET-COLOR
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
           CALL 'viewGuests'
           CANCEL 'viewGuests'.
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
