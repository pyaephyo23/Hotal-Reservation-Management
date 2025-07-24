       01 CHECKINOUT-RECORD.
           05 CHECKIN-ID            PIC 9(5).
           05 BOOKING-ID-IO         PIC 9(5).
           05 ROOM-ID-IO            PIC X(5).
           05 ACTUAL-CHECKIN-DATE   PIC 9(8).
           05 ACTUAL-CHECKIN-TIME   PIC 9(6).
           05 CHECKOUT-FLAG         PIC X VALUE 'N'.
           05 CHECKOUT-DATE         PIC 9(8) VALUE ZEROES.
           05 CHECKOUT-TIME         PIC 9(6) VALUE ZEROES.
