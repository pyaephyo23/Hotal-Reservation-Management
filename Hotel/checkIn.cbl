
      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. checkIn.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ROOMS-FILE ASSIGN TO '../DATA/ROOMS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ROOM-ID.
           SELECT BOOKING-FILE ASSIGN TO '../DATA/BOOKING.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS BOOKING-ID
               ALTERNATE RECORD KEY IS CHECKIN-DATE WITH DUPLICATES
               ALTERNATE RECORD KEY IS CHECKOUT-DATE WITH DUPLICATES.
       DATA DIVISION.
       FILE SECTION.
       FD  ROOMS-FILE.
       01  ROOMS-RECORD.
           05  ROOM-ID             PIC X(5).
           05  ROOM-TYPE           PIC X(10).
           05  PRICE-PER-NIGHT     PIC 9(9).
           05  R-STATUS            PIC X(10).
       FD  BOOKING-FILE.
       01  BOOKING-RECORD.
           05 BOOKING-ID      PIC 9(5).
           05 ROOM-ID-BK      PIC X(5).
           05 CUSTOMER-ID-BK  PIC 9(6).
           05 CHECKIN-DATE    PIC X(8).
           05 CHECKOUT-DATE   PIC X(8).
           05 BOOKING-STATUS  PIC X(10).
       WORKING-STORAGE SECTION.
       01 WS-CHOICE          PIC 9.
       01 WS-ROOM-ID         PIC X(5).
       01 WS-FOUND           PIC X VALUE 'N'.
       01 WS-CHECKIN-DATE    PIC X(8).
              01 WS-CURRENT-DATE-FIELDS.
           05 WS-CURRENT-YEAR    PIC 9(4).
           05 WS-CURRENT-MONTH   PIC 9(2).
           05 WS-CURRENT-DAY     PIC 9(2).
       LINKAGE SECTION.
       01 LINK PIC 9.
       PROCEDURE DIVISION USING LINK.
       MAIN-PROCEDURE.
           DISPLAY "***************************************************"
           DISPLAY "1. Check In"
           DISPLAY "9. Go Back"
           DISPLAY "***************************************************"
           ACCEPT WS-CHOICE
           EVALUATE WS-CHOICE
               WHEN 1
                   DISPLAY "Enter Room ID to Check In"
                   ACCEPT WS-ROOM-ID
                   PERFORM CHECK-ROOM-STATUS
                   PERFORM PERFORM-CHECKIN
                   GO TO MAIN-PROCEDURE
               WHEN 9
                   GOBACK
               WHEN OTHER
                   DISPLAY "Invalid option. Try again."
                   GO TO MAIN-PROCEDURE
           END-EVALUATE.
           STOP RUN.

       CHECK-ROOM-STATUS.
           OPEN I-O ROOMS-FILE.
           MOVE 'N' TO WS-FOUND.
           PERFORM UNTIL WS-FOUND = 'Y'
               READ ROOMS-FILE NEXT
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       IF ROOM-ID = WS-ROOM-ID
                           MOVE 'Y' TO WS-FOUND
                           IF R-STATUS = 'Booked'
                               MOVE 'Occupied' TO R-STATUS
                               REWRITE ROOMS-RECORD
                                   INVALID KEY
           DISPLAY "Error: Unable to rewrite record for " ROOM-ID
                                   NOT INVALID KEY
           DISPLAY "ROOM-ID " ROOM-ID " (Status Changed to Occupied)."
                               END-REWRITE
                           ELSE IF R-STATUS = 'Available'
           DISPLAY "ROOM-ID " ROOM-ID " is not booked."
           DISPLAY "Current Status is " R-STATUS
                               CLOSE ROOMS-FILE
                               GO TO MAIN-PROCEDURE
                           ELSE IF R-STATUS = 'Occupied'
           DISPLAY "ROOM-ID " ROOM-ID " is already checked in."
           DISPLAY "Current Status is " R-STATUS
                               CLOSE ROOMS-FILE
                               GO TO MAIN-PROCEDURE
                           END-IF
                       END-IF
               END-READ
           END-PERFORM.

           IF WS-FOUND = 'N'
               DISPLAY "Invalid Room ID"
           END-IF.
           CLOSE ROOMS-FILE.

       PERFORM-CHECKIN.
           MOVE 'N' TO WS-FOUND.
           OPEN I-O BOOKING-FILE.

           *> Get the current system date
           ACCEPT WS-CURRENT-DATE-FIELDS FROM DATE YYYYMMDD.
           STRING WS-CURRENT-YEAR DELIMITED BY SIZE
                  WS-CURRENT-MONTH DELIMITED BY SIZE
                  WS-CURRENT-DAY DELIMITED BY SIZE
                  INTO WS-CHECKIN-DATE.

           PERFORM UNTIL WS-FOUND = 'Y'
               READ BOOKING-FILE NEXT
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       IF WS-ROOM-ID = ROOM-ID-BK
                           IF BOOKING-STATUS = 'Active'
                               *> Use the automatically obtained date
                               MOVE WS-CHECKIN-DATE TO CHECKIN-DATE
                               REWRITE BOOKING-RECORD
                                   INVALID KEY
                                       DISPLAY "Error"
                               END-REWRITE
                               DISPLAY "ROOM-ID "
                               ROOM-ID-BK " is Now Check In."
                               DISPLAY "CHECKIN-DATE : " CHECKIN-DATE
                               MOVE 'Y' TO WS-FOUND
                           END-IF
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE BOOKING-FILE.
       END PROGRAM checkIn.
