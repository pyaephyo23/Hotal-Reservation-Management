        IDENTIFICATION DIVISION.
        PROGRAM-ID. checkOut.
        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT ROOMS-FILE ASSIGN TO '../DATA/ROOMS.DAT'
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS ROOM-ID
                  FILE STATUS IS WS-ROOM-FILE-STATUS.
           SELECT BOOKING-FILE ASSIGN TO '../DATA/BOOKING.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS BOOKING-ID
               ALTERNATE RECORD KEY IS CHECKIN-DATE WITH DUPLICATES
               ALTERNATE RECORD KEY IS CHECKOUT-DATE WITH DUPLICATES
               FILE STATUS IS WS-BOOKING-FILE-STATUS.
           SELECT CUSTOMER-FILE ASSIGN TO '../DATA/CUSTOMER.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUSTOMER-ID
               FILE STATUS IS WS-CUSTOMER-FILE-STATUS.
        DATA DIVISION.
        FILE SECTION.
        FD  ROOMS-FILE.COPY "./CopyBooks/ROOMS.cpy".
        FD  BOOKING-FILE.
        COPY "./CopyBooks/BOOKINGS.cpy".
        FD  CUSTOMER-FILE.
        COPY "./CopyBooks/CUSTOMERS.cpy".

        WORKING-STORAGE SECTION.
           01 WS-BOOKING-ID           PIC 9(5).
           01 WS-ROOM-FILE-STATUS     PIC 99.
           01 WS-BOOKING-FILE-STATUS  PIC 99.
           01 WS-CUSTOMER-FILE-STATUS PIC 99.
           01 WS-FOUND-FLAG           PIC X VALUE 'N'.
           01 WS-INVOICE-ID           PIC Z(5).
           01 WS-CURRENT-DATE         PIC X(8).
           01 WS-NIGHTS               PIC 9(3).
           01 WS-SUBTOTAL             PIC 9(7)V99.
           01 WS-TAX                  PIC 9(7)V99.
           01 WS-TOTAL                PIC 9(7)V99.
           01 WS-TAX-RATE             PIC V99 VALUE 0.15.
           01 WS-CHECKIN-NUM          PIC 9(8).
           01 WS-CHECKOUT-NUM         PIC 9(8).
           01 WS-DAYS-DIFF            PIC 9(8).
           01 WS-PRICE-DISPLAY        PIC $$,$$$,$$9.99.
           01 WS-INVOICE-COUNTER      PIC 9(5) VALUE 1.

           01 WS-INVOICE-HEADER.
               05 FILLER PIC X(40) VALUE '============================'.
               05 FILLER PIC X(40) VALUE '      INVOICE               '.
               05 FILLER PIC X(40) VALUE '============================'.
        01 LINK PIC 9.

        PROCEDURE DIVISION USING LINK.

           MAIN-PROCESS.
           DISPLAY "Hotel Check-Out System"
           DISPLAY "===================="
           DISPLAY "Enter Booking ID: "
           ACCEPT WS-BOOKING-ID

           PERFORM OPEN-FILES
           PERFORM SEARCH-BOOKING
           IF WS-FOUND-FLAG = 'Y'
               PERFORM GET-CUSTOMER-INFO
               PERFORM GET-ROOM-INFO
               PERFORM CALCULATE-CHARGES
               PERFORM GENERATE-INVOICE
               PERFORM UPDATE-ROOM-STATUS
               PERFORM UPDATE-BOOKING-STATUS
               DISPLAY "Check-out completed successfully!"
               DISPLAY "Room " ROOM-ID " is now available."
           ELSE
               DISPLAY "Booking not found or already checked out."
           END-IF

           PERFORM CLOSE-FILES
           GOBACK.

        OPEN-FILES.
           OPEN I-O ROOMS-FILE BOOKING-FILE
           OPEN INPUT CUSTOMER-FILE
           IF WS-ROOM-FILE-STATUS NOT = 00 OR
              WS-BOOKING-FILE-STATUS NOT = 00 OR
              WS-CUSTOMER-FILE-STATUS NOT = 00
               DISPLAY "Error opening files"
               STOP RUN
           END-IF.

        SEARCH-BOOKING.
           MOVE WS-BOOKING-ID TO BOOKING-ID
           READ BOOKING-FILE
            INVALID KEY
               MOVE 'N' TO WS-FOUND-FLAG
            NOT INVALID KEY
                MOVE 'Y' TO WS-FOUND-FLAG
           END-READ.

        GET-CUSTOMER-INFO.
           DISPLAY "Fetching customer information..."
           display "Customer ID: " CUSTOMER-ID-BK
           MOVE CUSTOMER-ID-BK TO CUSTOMER-ID
           READ CUSTOMER-FILE
               INVALID KEY
                   DISPLAY "Customer information not found"
           END-READ.

        GET-ROOM-INFO.
           MOVE ROOM-ID-BK TO ROOM-ID
           READ ROOMS-FILE
               INVALID KEY
                   DISPLAY "Room information not found"
           END-READ.

        CALCULATE-CHARGES.
           MOVE FUNCTION NUMVAL(CHECKIN-DATE) TO WS-CHECKIN-NUM
           MOVE FUNCTION NUMVAL(CHECKOUT-DATE) TO WS-CHECKOUT-NUM
           COMPUTE WS-DAYS-DIFF = WS-CHECKOUT-NUM - WS-CHECKIN-NUM

           IF WS-DAYS-DIFF > 0
               MOVE WS-DAYS-DIFF TO WS-NIGHTS
           ELSE
               MOVE 1 TO WS-NIGHTS
           END-IF

           COMPUTE WS-SUBTOTAL = PRICE-PER-NIGHT * WS-NIGHTS
           COMPUTE WS-TAX = WS-SUBTOTAL * WS-TAX-RATE
           COMPUTE WS-TOTAL = WS-SUBTOTAL + WS-TAX.

        GENERATE-INVOICE.
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           MOVE WS-CURRENT-DATE TO WS-CURRENT-DATE
           MOVE WS-INVOICE-COUNTER TO WS-INVOICE-ID

           DISPLAY "========================================"
           DISPLAY "              INVOICE                  "
           DISPLAY "========================================"
           DISPLAY "Invoice ID    : " WS-INVOICE-ID
           DISPLAY "Booking ID    : " BOOKING-ID
           DISPLAY "Invoice Date  : " WS-CURRENT-DATE
           DISPLAY " "
           DISPLAY "Customer Details:"
           DISPLAY "Name          : " CUSTOMER-NAME
           DISPLAY "Phone         : " CUSTOMER-PHONE
           DISPLAY "Email         : " CUSTOMER-EMAIL
           DISPLAY "Address       : " CUSTOMER-ADDR
           DISPLAY " "
           DISPLAY "Booking Details:"
           DISPLAY "Room ID       : " ROOM-ID
           DISPLAY "Room Type     : " ROOM-TYPE
           DISPLAY "Check-in      : " CHECKIN-DATE
           DISPLAY "Check-out     : " CHECKOUT-DATE
           DISPLAY "Nights        : " WS-NIGHTS
           DISPLAY " "
           DISPLAY "Charges:"
           MOVE PRICE-PER-NIGHT TO WS-PRICE-DISPLAY
           DISPLAY "Rate/Night    : $" WS-PRICE-DISPLAY
           MOVE WS-SUBTOTAL TO WS-PRICE-DISPLAY
           DISPLAY "Subtotal      : $" WS-PRICE-DISPLAY
           MOVE WS-TAX TO WS-PRICE-DISPLAY
           DISPLAY "Tax (15%)     : $" WS-PRICE-DISPLAY
           DISPLAY "========================================"
           MOVE WS-TOTAL TO WS-PRICE-DISPLAY
           DISPLAY "Total Amount  : $" WS-PRICE-DISPLAY
           DISPLAY "========================================"
           DISPLAY "Status        : GENERATED"
           DISPLAY " ".

        UPDATE-ROOM-STATUS.
           *> Initialize ACTIVE-BOOKING-COUNT if it contains non-numeric data
           IF ACTIVE-BOOKING-COUNT NOT NUMERIC
               MOVE ZERO TO ACTIVE-BOOKING-COUNT
           END-IF
           
           *> Update room status to Available and decrement active booking count
           MOVE 'Available' TO R-STATUS
           
           *> Subtract 1 from active booking count (ensure it doesn't go below 0)
           IF ACTIVE-BOOKING-COUNT > 0
               SUBTRACT 1 FROM ACTIVE-BOOKING-COUNT
           ELSE
               MOVE 0 TO ACTIVE-BOOKING-COUNT
           END-IF
           
           REWRITE ROOMS-RECORD
               INVALID KEY
                   DISPLAY "Error updating room status"
           END-REWRITE.

        UPDATE-BOOKING-STATUS.
           MOVE 'Completed' TO BOOKING-STATUS
           REWRITE BOOKING-RECORD
               INVALID KEY
                   DISPLAY "Error updating booking status"
           END-REWRITE.

        CLOSE-FILES.
           CLOSE ROOMS-FILE BOOKING-FILE CUSTOMER-FILE.

        END PROGRAM checkOut.
