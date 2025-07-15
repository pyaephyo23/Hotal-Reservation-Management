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
       DATA DIVISION.
 
       FILE SECTION.
       FD  ROOMS-FILE.
       COPY "./CopyBooks/ROOMS.cpy".
 
       FD  BOOKING-FILE.
       01  BOOKING-RECORD.
           05 BOOKING-ID      PIC 9(5).
           05 ROOM-ID-BK      PIC X(5).
           05 CUSTOMER-ID-BK  PIC 9(5).
           05 CHECKIN-DATE    PIC X(8).
           05 CHECKOUT-DATE   PIC X(8).
           05 BOOKING-STATUS  PIC X(10).
           05 CHEKIN-FLAG     PIC X VALUE 'N'.
           05 CHECKOUT-FLAG   PIC X VALUE 'N'.
           05 CREATED-AT      PIC X(14).
 
       WORKING-STORAGE SECTION.
       01 WS-CHOICE            PIC 9.
       01 WS-BOOKING-ID        PIC 9(5).
       01 WS-FOUND             PIC X VALUE 'N'.
       01 WS-CHECKIN-DATE      PIC X(8).
       01 WS-CURRENT-DATE-FIELDS.
           05 WS-CURRENT-YEAR    PIC 9(4).
           05 WS-CURRENT-MONTH   PIC 9(2).
           05 WS-CURRENT-DAY     PIC 9(2).
       01 WS-DATE-FORMATTED    PIC X(8).
       01 WS-TIME-FORMATTED    PIC X(6).
       01 WS-CURRENT-TIME-FIELDS.
           05 WS-CURRENT-HOUR    PIC 9(2).
           05 WS-CURRENT-MINUTE  PIC 9(2).
           05 WS-CURRENT-SECOND  PIC 9(2).
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
                   DISPLAY "Enter Booking ID to Check In"
                   ACCEPT WS-BOOKING-ID
                   PERFORM CHECK-BOOKING-ID-AND-CHECK-IN
                   PERFORM CHANGE-ROOM-STATUS
                   GO TO MAIN-PROCEDURE
               WHEN 9
                   GOBACK
               WHEN OTHER
                   DISPLAY "Invalid option. Try again."
                   GO TO MAIN-PROCEDURE
           END-EVALUATE.
           STOP RUN.
 
       CHECK-BOOKING-ID-AND-CHECK-IN.
           MOVE 'N' TO WS-FOUND.
           OPEN I-O BOOKING-FILE.
 
           *> Get the current system date
           ACCEPT WS-CURRENT-DATE-FIELDS FROM DATE YYYYMMDD.
           STRING WS-CURRENT-YEAR DELIMITED BY SIZE
                  WS-CURRENT-MONTH DELIMITED BY SIZE
                  WS-CURRENT-DAY DELIMITED BY SIZE
                  INTO WS-DATE-FORMATTED.
           
            *> Get the current system time
           ACCEPT WS-CURRENT-TIME-FIELDS FROM TIME.
           STRING WS-CURRENT-HOUR DELIMITED BY SIZE
                  WS-CURRENT-MINUTE DELIMITED BY SIZE
                  WS-CURRENT-SECOND DELIMITED BY SIZE
                  INTO WS-TIME-FORMATTED.
 
           MOVE WS-BOOKING-ID TO BOOKING-ID OF BOOKING-RECORD.
           READ BOOKING-FILE
               INVALID KEY
                   DISPLAY "Booking ID " WS-BOOKING-ID " not found."
                   CLOSE BOOKING-FILE
                   GO TO MAIN-PROCEDURE
               NOT INVALID KEY
                   IF BOOKING-STATUS OF BOOKING-RECORD = 'Active'
                       IF CHEKIN-FLAG OF BOOKING-RECORD = 'N'
                MOVE WS-DATE-FORMATTED TO CHECKIN-DATE OF BOOKING-RECORD
                MOVE 'Y' TO CHEKIN-FLAG OF BOOKING-RECORD
                           REWRITE BOOKING-RECORD
                               INVALID KEY
                           DISPLAY "Error rewriting booking record."
                               NOT INVALID KEY
               DISPLAY "Check In Complete."
               DISPLAY "BOOKING-ID: " BOOKING-ID
               DISPLAY "ROOM-ID-BK: " ROOM-ID-BK                
               DISPLAY "CHECKIN-DATE: " CHECKIN-DATE OF BOOKING-RECORD
               DISPLAY "CHECKIN-TIME: " WS-TIME-FORMATTED
                           END-REWRITE
                       ELSE
                           DISPLAY "Booking ID "
                           WS-BOOKING-ID " is already checked in."
                           DISPLAY "CHEKIN-FLAG : "
                           CHEKIN-FLAG OF BOOKING-RECORD
                           CLOSE BOOKING-FILE
                           GO TO MAIN-PROCEDURE
                       END-IF
                   ELSE
                       DISPLAY "Booking ID " WS-BOOKING-ID
                       " is not 'Active'."
                       DISPLAY "Current status: "
                       BOOKING-STATUS OF BOOKING-RECORD
                       CLOSE BOOKING-FILE
                       GO TO MAIN-PROCEDURE
                   END-IF
           END-READ.
 
           CLOSE BOOKING-FILE.
           
           CHANGE-ROOM-STATUS.
           OPEN I-O ROOMS-FILE.
           MOVE 'N' TO WS-FOUND.
           PERFORM UNTIL WS-FOUND = 'Y'
               READ ROOMS-FILE NEXT
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       IF ROOM-ID = ROOM-ID-BK
                           MOVE 'Y' TO WS-FOUND
                               MOVE 'Occupied' TO R-STATUS
                               REWRITE ROOMS-RECORD
                                   INVALID KEY
           DISPLAY "Error: Unable to rewrite record for " ROOM-ID
                                   NOT INVALID KEY
           DISPLAY "ROOM-ID " ROOM-ID " (Status Changed to Occupied)."
                               END-REWRITE
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE ROOMS-FILE.
       END PROGRAM checkIn.
 