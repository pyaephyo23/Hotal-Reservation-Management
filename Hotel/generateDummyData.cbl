******************************************************************
      * Author: System
      * Date: 2025-07-18
      * Purpose: Generate dummy expired bookings for testing
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. generateDummyData.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BOOKING-FILE ASSIGN TO '../DATA/BOOKINGS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS BOOKING-ID.

           SELECT ROOMS-FILE ASSIGN TO '../DATA/ROOMS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ROOM-ID.

       DATA DIVISION.
       FILE SECTION.
       FD  BOOKING-FILE.
       COPY "./CopyBooks/BOOKINGS.cpy".

       FD  ROOMS-FILE.
       COPY "./CopyBooks/ROOMS.cpy".

       WORKING-STORAGE SECTION.
       01  WS-BOOKING-FILE-STATUS  PIC 99.
       01  WS-ROOMS-FILE-STATUS    PIC 99.
       01  WS-COUNTER              PIC 9(3) VALUE 1.
       01  WS-DUMMY-BOOKINGS       PIC 9(2) VALUE 5.
       01  WS-CURRENT-DATE         PIC 9(8).
       01  WS-EXPIRED-DATE         PIC 9(8).
       01  WS-CHECKOUT-DATE        PIC 9(8).
       01  WS-NEXT-BOOKING-ID      PIC 9(5) VALUE 90000.
       01  WS-ROOM-COUNTER         PIC 9(2) VALUE 1.
       01  WS-CUSTOMER-COUNTER     PIC 9(5) VALUE 10001.
       01  WS-STARTING-BOOKING-ID  PIC 9(5).

       01  WS-EXPIRED-DATES.
           05 FILLER PIC 9(8) VALUE 20250701.  
           05 FILLER PIC 9(8) VALUE 20250702.  
           05 FILLER PIC 9(8) VALUE 20250703.  
           05 FILLER PIC 9(8) VALUE 20250704.  
           05 FILLER PIC 9(8) VALUE 20250705.

       01  WS-EXPIRED-DATES-ARRAY REDEFINES WS-EXPIRED-DATES.
           05 WS-EXPIRED-DATE-ITEM PIC 9(8) OCCURS 5 TIMES.

       01  WS-ROOM-IDS.
           05 FILLER PIC X(5) VALUE "R0001".
           05 FILLER PIC X(5) VALUE "R0002".
           05 FILLER PIC X(5) VALUE "R0003".
           05 FILLER PIC X(5) VALUE "R0004".
           05 FILLER PIC X(5) VALUE "R0005".

       01  WS-ROOM-IDS-ARRAY REDEFINES WS-ROOM-IDS.
           05 WS-ROOM-ID-ITEM PIC X(5) OCCURS 5 TIMES.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "=========================================="
           DISPLAY "    DUMMY EXPIRED BOOKINGS GENERATOR"
           DISPLAY "=========================================="
           DISPLAY "This will create 5 expired bookings for testing."
           DISPLAY " "

           PERFORM FIND-NEXT-BOOKING-ID
           PERFORM CREATE-DUMMY-BOOKINGS
           PERFORM UPDATE-ROOM-STATUSES
           DISPLAY " "
           DISPLAY "Dummy data generation completed!"
           DISPLAY "Created " WS-DUMMY-BOOKINGS " expired bookings"
           COMPUTE WS-STARTING-BOOKING-ID = WS-NEXT-BOOKING-ID -
           WS-DUMMY-BOOKINGS
           DISPLAY "Starting from Booking ID: " WS-STARTING-BOOKING-ID
           DISPLAY " "
           DISPLAY "You can now test the expired booking cancellation."
           DISPLAY "=========================================="

           STOP RUN.

       FIND-NEXT-BOOKING-ID.
           OPEN INPUT BOOKING-FILE
           IF WS-BOOKING-FILE-STATUS NOT = 00
               DISPLAY "Note: BOOKINGS.DAT not found, using default ID"
               MOVE 90001 TO WS-NEXT-BOOKING-ID
               CLOSE BOOKING-FILE
               EXIT PARAGRAPH
           END-IF

           MOVE ZEROS TO BOOKING-ID
           START BOOKING-FILE KEY IS GREATER THAN OR EQUAL TO BOOKING-ID
           IF WS-BOOKING-FILE-STATUS = 00
               MOVE 0 TO WS-NEXT-BOOKING-ID
               PERFORM UNTIL 1 = 2
                   READ BOOKING-FILE NEXT
                   IF WS-BOOKING-FILE-STATUS = 00
                       IF BOOKING-ID > WS-NEXT-BOOKING-ID
                           MOVE BOOKING-ID TO WS-NEXT-BOOKING-ID
                       END-IF
                   ELSE
                       EXIT PERFORM
                   END-IF
               END-PERFORM
               IF WS-NEXT-BOOKING-ID > 0
                   ADD 1 TO WS-NEXT-BOOKING-ID
               ELSE
                   MOVE 90001 TO WS-NEXT-BOOKING-ID
               END-IF
           ELSE
               MOVE 90001 TO WS-NEXT-BOOKING-ID
           END-IF

           CLOSE BOOKING-FILE.

       CREATE-DUMMY-BOOKINGS.
           OPEN I-O BOOKING-FILE
           IF WS-BOOKING-FILE-STATUS NOT = 00
               DISPLAY "Error opening BOOKING file: "
                       WS-BOOKING-FILE-STATUS
               STOP RUN
           END-IF

           PERFORM VARYING WS-COUNTER FROM 1 BY 1
                   UNTIL WS-COUNTER > WS-DUMMY-BOOKINGS

               *> Set booking details
               MOVE WS-NEXT-BOOKING-ID TO BOOKING-ID
               MOVE WS-ROOM-ID-ITEM(WS-COUNTER) TO ROOM-ID-BK
               MOVE WS-CUSTOMER-COUNTER TO CUSTOMER-ID-BK
               MOVE WS-EXPIRED-DATE-ITEM(WS-COUNTER) TO CHECKIN-DATE

               *> Set checkout date (same pattern as checkin for simplicity)
               MOVE WS-EXPIRED-DATE-ITEM(WS-COUNTER) TO WS-CHECKOUT-DATE
               ADD 2 TO WS-CHECKOUT-DATE
               MOVE WS-CHECKOUT-DATE TO CHECKOUT-DATE

               MOVE "Active" TO BOOKING-STATUS
               MOVE 'N' TO CHEKIN-FLAG
               MOVE 'N' TO CHECKOUT-FLAG
               MOVE "20250718120000" TO CREATED-AT

               WRITE BOOKING-RECORD
               INVALID KEY
                   DISPLAY "Error writing booking " BOOKING-ID
               NOT INVALID KEY
                   DISPLAY "Created expired booking:"
                   DISPLAY "  ID: " BOOKING-ID
                   DISPLAY "  Room: " ROOM-ID-BK
                   DISPLAY "  Check-in: " CHECKIN-DATE
                   DISPLAY "  Check-out: " CHECKOUT-DATE
                   DISPLAY "  Status: " BOOKING-STATUS
                   DISPLAY " "
               END-WRITE

               ADD 1 TO WS-NEXT-BOOKING-ID
               ADD 1 TO WS-CUSTOMER-COUNTER
           END-PERFORM

           CLOSE BOOKING-FILE.

       UPDATE-ROOM-STATUSES.
           OPEN I-O ROOMS-FILE
           IF WS-ROOMS-FILE-STATUS NOT = 00
               DISPLAY
               "Warning: Could not open ROOMS file to update statuses"
               EXIT PARAGRAPH
           END-IF

           PERFORM VARYING WS-ROOM-COUNTER FROM 1 BY 1
                   UNTIL WS-ROOM-COUNTER > WS-DUMMY-BOOKINGS

               MOVE WS-ROOM-ID-ITEM(WS-ROOM-COUNTER) TO ROOM-ID
               READ ROOMS-FILE KEY IS ROOM-ID
               INVALID KEY
                   DISPLAY "Warning: Room " ROOM-ID " not found"
               NOT INVALID KEY
                   *> Increment active booking count for this room
                   IF ACTIVE-BOOKING-COUNT NOT NUMERIC
                       MOVE 0 TO ACTIVE-BOOKING-COUNT
                   END-IF
                   ADD 1 TO ACTIVE-BOOKING-COUNT
                   MOVE "Booked" TO R-STATUS

                   REWRITE ROOMS-RECORD
                   INVALID KEY
                       DISPLAY "Error updating room " ROOM-ID
                   NOT INVALID KEY
                       DISPLAY "Updated room " ROOM-ID
                               " - Active bookings: "
                               ACTIVE-BOOKING-COUNT
                   END-REWRITE
               END-READ
           END-PERFORM

           CLOSE ROOMS-FILE.

       END PROGRAM generateDummyData.
