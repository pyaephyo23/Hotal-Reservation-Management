      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ROOM-FILE ASSIGN TO '../ROOMS.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CUST-FILE ASSIGN TO '../CUSTOMERS.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

      *-----------------------

      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-
       FILE SECTION.
       FD ROOM-FILE.
       01 ROOM-RECORD.
           05 ROOM-NO        PIC 9(3).
           05 ROOM-TYPE      PIC X(10).
           05 ROOM-STATUS    PIC X(10).
           05 ROOM-RATE      PIC 9(5)V99.

       FD CUST-FILE.
       01 CUST-RECORD.
           05 CUST-ID        PIC 9(5).
           05 CUST-NAME      PIC X(30).
           05 ROOM-NO-BOOKED PIC 9(3).
           05 CHECKIN-DATE   PIC X(10).
           05 CHECKOUT-DATE  PIC X(10).
           05 TOTAL-AMOUNT   PIC 9(5)V99.
      *-----------------------
       WORKING-STORAGE SECTION.
       01 MENU-CHOICE       PIC 9.
       01 SEARCH-NO         PIC 9(3).
       01 FOUND             PIC X VALUE 'N'.
       01 TAX-RATE          PIC 9(2)V99 VALUE 0.05.
       01 SERVICE-CHARGE    PIC 9(2)V99 VALUE 0.10.
       01 BASE-AMOUNT       PIC 9(5)V99.
       01 FINAL-AMOUNT      PIC 9(5)V99.
       01 WS-LINE           PIC X(80).
       01 INPUT-ROOM-TYPE      PIC X(10).
       01 WS-EOF            PIC X VALUE 'N'.
      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-LOOP.
           DISPLAY "Hotel Reservation Management System".
           DISPLAY "1. Book a Room".
           DISPLAY "2. Cancel Booking".
           DISPLAY "3. Check-In".
           DISPLAY "4. Check-Out".
           DISPLAY "5. View Available Rooms".
           DISPLAY "6. Generate Report".
           DISPLAY "9. Exit".
           ACCEPT MENU-CHOICE.

           EVALUATE MENU-CHOICE
               WHEN 1 PERFORM BOOK-ROOM
               WHEN 2 PERFORM CANCEL-ROOM
               WHEN 3 PERFORM CHECK-IN
               WHEN 4 PERFORM CHECK-OUT
               WHEN 5 PERFORM VIEW-ROOMS
               WHEN 6 PERFORM GENERATE-REPORT
               WHEN 9 STOP RUN
               WHEN OTHER DISPLAY "Invalid choice"
           END-EVALUATE.

           GO TO MAIN-LOOP.

           BOOK-ROOM.
           DISPLAY "Enter room type (SINGLE/DOUBLE/DELUXE): ".
           ACCEPT INPUT-ROOM-TYPE.
           OPEN INPUT ROOM-FILE.
           PERFORM UNTIL WS-EOF = 'Y'
               READ ROOM-FILE
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF ROOM-TYPE = INPUT-ROOM-TYPE AND
                           ROOM-STATUS = "AVAILABLE"
                           MOVE "BOOKED" TO ROOM-STATUS
                           REWRITE ROOM-RECORD
                           DISPLAY "Room " ROOM-NO " booked."
                           PERFORM ADD-CUSTOMER
                           EXIT PERFORM
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE ROOM-FILE.

           ADD-CUSTOMER.
           OPEN EXTEND CUST-FILE.
           DISPLAY "Enter Customer ID: " ACCEPT CUST-ID.
           DISPLAY "Enter Customer Name: " ACCEPT CUST-NAME.
           DISPLAY "Enter Check-in Date (DD-MM-YYYY): "
           ACCEPT CHECKIN-DATE.
           DISPLAY "Enter Check-out Date (DD-MM-YYYY): "
           ACCEPT CHECKOUT-DATE.
           COMPUTE BASE-AMOUNT = ROOM-RATE * 1
           COMPUTE FINAL-AMOUNT = BASE-AMOUNT + (BASE-AMOUNT * TAX-RATE)
                                   + (BASE-AMOUNT * SERVICE-CHARGE).
           MOVE FINAL-AMOUNT TO TOTAL-AMOUNT.
           MOVE ROOM-NO TO ROOM-NO-BOOKED.
           WRITE CUST-RECORD.
           CLOSE CUST-FILE.

           CANCEL-ROOM.
           DISPLAY "Enter Room No to Cancel: ".
           ACCEPT SEARCH-NO.
           OPEN I-O ROOM-FILE.
           PERFORM UNTIL WS-EOF = 'Y'
               READ ROOM-FILE
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF ROOM-NO = SEARCH-NO AND ROOM-STATUS = "BOOKED"
                           MOVE "AVAILABLE" TO ROOM-STATUS
                           REWRITE ROOM-RECORD
                           DISPLAY "Booking cancelled."
                           EXIT PERFORM
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE ROOM-FILE.

           CHECK-IN.
           DISPLAY "Enter Room No for Check-in: ".
           ACCEPT SEARCH-NO.
           DISPLAY "Check-in Successful.".

           CHECK-OUT.
           DISPLAY "Enter Room No for Check-out: ".
           ACCEPT SEARCH-NO.
           DISPLAY "Check-out Successful.".

           VIEW-ROOMS.
           OPEN INPUT ROOM-FILE.
           PERFORM UNTIL WS-EOF = 'Y'
               READ ROOM-FILE
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF ROOM-STATUS = "AVAILABLE"
                           DISPLAY "Room " ROOM-NO ", " ROOM-TYPE
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE ROOM-FILE.

           GENERATE-REPORT.
           OPEN INPUT CUST-FILE.
           MOVE 0 TO FINAL-AMOUNT.
           PERFORM UNTIL WS-EOF = 'Y'
               READ CUST-FILE
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                       ADD TOTAL-AMOUNT TO FINAL-AMOUNT
               END-READ
           END-PERFORM.
           DISPLAY "Total Revenue Today: " FINAL-AMOUNT.
           CLOSE CUST-FILE.
      ** add other procedures here
       END PROGRAM YOUR-PROGRAM-NAME.
