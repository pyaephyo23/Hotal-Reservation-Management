       IDENTIFICATION DIVISION.
       PROGRAM-ID. randomGenerator.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CURRENT-TIME.
           05  WS-HOURS            PIC 9(2).
           05  WS-MINUTES          PIC 9(2).
           05  WS-SECONDS          PIC 9(2).
           05  WS-MILLISECONDS     PIC 9(2).

       01  WS-SEED                 PIC 9(8).
       01  WS-RANDOM-NUMBER        PIC 9(8).
       01  WS-SCALED-RANDOM        PIC 9(5).
       01  WS-CHOICE               PIC 9.
       01  WS-MIN-RANGE            PIC 9(5) VALUE 1.
       01  WS-MAX-RANGE            PIC 9(5) VALUE 100.
       01  WS-RANGE                PIC 9(5).
       01  WS-FLOAT-RANDOM         PIC 9V9999.

       PROCEDURE DIVISION.
       MAIN-MENU.
           DISPLAY "========================================="
           DISPLAY "       Random Number Generator"
           DISPLAY "========================================="
           DISPLAY "1. Generate Random Number (1-100)"
           DISPLAY "2. Generate Random Number (Custom Range)"
           DISPLAY "3. Generate Multiple Random Numbers"
           DISPLAY "4. Generate Random Booking ID"
           DISPLAY "9. Exit"
           DISPLAY "========================================="
           DISPLAY "Enter your choice: "
           ACCEPT WS-CHOICE

           EVALUATE WS-CHOICE
               WHEN 1
                   PERFORM GENERATE-DEFAULT-RANDOM
               WHEN 2
                   PERFORM GENERATE-CUSTOM-RANDOM
               WHEN 3
                   PERFORM GENERATE-MULTIPLE-RANDOM
               WHEN 4
                   PERFORM GENERATE-BOOKING-ID
               WHEN 9
                   DISPLAY "Goodbye!"
                   STOP RUN
               WHEN OTHER
                   DISPLAY "Invalid choice. Please try again."
           END-EVALUATE

           DISPLAY ""
           DISPLAY "Press Enter to continue..."
           ACCEPT WS-CHOICE
           GO TO MAIN-MENU.

       GENERATE-DEFAULT-RANDOM.
           PERFORM INITIALIZE-RANDOM-SEED
           PERFORM GENERATE-RANDOM-NUMBER

           *> Scale to 1-100 range
           COMPUTE WS-SCALED-RANDOM =
               (WS-RANDOM-NUMBER / 99999999 * 100) + 1

           DISPLAY "Random number (1-100): " WS-SCALED-RANDOM.

       GENERATE-CUSTOM-RANDOM.
           DISPLAY "Enter minimum value: "
           ACCEPT WS-MIN-RANGE
           DISPLAY "Enter maximum value: "
           ACCEPT WS-MAX-RANGE

           IF WS-MIN-RANGE >= WS-MAX-RANGE
               DISPLAY "Error: Minimum must be less than maximum."
               GO TO GENERATE-CUSTOM-RANDOM
           END-IF

           PERFORM INITIALIZE-RANDOM-SEED
           PERFORM GENERATE-RANDOM-NUMBER

           *> Calculate range
           COMPUTE WS-RANGE = WS-MAX-RANGE - WS-MIN-RANGE + 1

           *> Scale to custom range
           COMPUTE WS-SCALED-RANDOM =
               (WS-RANDOM-NUMBER / 99999999 * WS-RANGE) + WS-MIN-RANGE

           DISPLAY "Random number (" WS-MIN-RANGE "-" WS-MAX-RANGE "): "
                   WS-SCALED-RANDOM.

       GENERATE-MULTIPLE-RANDOM.
           DISPLAY "How many random numbers to generate? "
           ACCEPT WS-CHOICE

           IF WS-CHOICE < 1 OR WS-CHOICE > 9
               DISPLAY "Please enter a number between 1 and 9."
               GO TO GENERATE-MULTIPLE-RANDOM
           END-IF

           PERFORM INITIALIZE-RANDOM-SEED

           DISPLAY "Generating " WS-CHOICE " random numbers (1-1000):"
           PERFORM VARYING WS-RANGE FROM 1 BY 1
           UNTIL WS-RANGE > WS-CHOICE
               PERFORM GENERATE-RANDOM-NUMBER
               COMPUTE WS-SCALED-RANDOM =
                   (WS-RANDOM-NUMBER / 99999999 * 1000) + 1
               DISPLAY "Number " WS-RANGE ": " WS-SCALED-RANDOM
               *> Add small delay to change seed slightly
               PERFORM SMALL-DELAY
           END-PERFORM.

       GENERATE-BOOKING-ID.
           DISPLAY "Generating random 5-digit Booking ID..."
           PERFORM INITIALIZE-RANDOM-SEED
           PERFORM GENERATE-RANDOM-NUMBER

           *> Scale to 5-digit number (10000-99999)
           COMPUTE WS-SCALED-RANDOM =
               (WS-RANDOM-NUMBER / 99999999 * 89999) + 10000

           DISPLAY "Random Booking ID: " WS-SCALED-RANDOM
           DISPLAY "This could be used as a unique booking reference.".

       INITIALIZE-RANDOM-SEED.
           *> Get current time
           ACCEPT WS-CURRENT-TIME FROM TIME

           *> Create seed from time components
           COMPUTE WS-SEED =
               (WS-HOURS * 1000000) +
               (WS-MINUTES * 10000) +
               (WS-SECONDS * 100) +
               WS-MILLISECONDS

           DISPLAY "Time-based seed: " WS-SEED

           *> Initialize random number generator with seed
           MOVE WS-SEED TO WS-RANDOM-NUMBER.

       GENERATE-RANDOM-NUMBER.
           *> Simple Linear Congruential Generator (LCG)
           *> Formula: next = (a * seed + c) mod m
           *> Using common LCG parameters
           COMPUTE WS-RANDOM-NUMBER =
         FUNCTION MOD((WS-RANDOM-NUMBER * 1103515245 + 12345), 99999999)

           *> Update seed for next generation
           MOVE WS-RANDOM-NUMBER TO WS-SEED.

       SMALL-DELAY.
           *> Simple delay loop to change timing slightly
           PERFORM VARYING WS-RANGE FROM 1 BY 1 UNTIL WS-RANGE > 1000
               CONTINUE
           END-PERFORM.

       END PROGRAM randomGenerator.
