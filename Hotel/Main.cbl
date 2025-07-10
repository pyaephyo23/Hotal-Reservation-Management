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
           DISPLAY "5. View Available Rooms"
           DISPLAY "6. Generate Revenue Report"
           DISPLAY "9. Exit"
           DISPLAY
           "***********************************************************"
           ACCEPT MENU-CHOICE

           EVALUATE MENU-CHOICE
               WHEN 1 PERFORM BOOK-ROOM
               WHEN 2 PERFORM CANCEL-BOOKING
               WHEN 3 PERFORM CHECK-IN
               WHEN 4 PERFORM CHECK-OUT
               WHEN 5 PERFORM VIEW-ROOMS
               WHEN 6 PERFORM GENERATE-REPORT
               WHEN 9 STOP RUN
               WHEN OTHER DISPLAY "Invalid choice"
           END-EVALUATE
           END-PERFORM.
           STOP RUN.

       BOOK-ROOM.
       CANCEL-BOOKING.
       CHECK-IN.
       CHECK-OUT.
       VIEW-ROOMS.
           CALL 'viewRooms'.
       GENERATE-REPORT.
       END PROGRAM Main.
