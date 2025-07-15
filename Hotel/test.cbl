       IDENTIFICATION DIVISION.
       PROGRAM-ID. test.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CURRENT-DATE-DATA.
           05  WS-CURRENT-DATE.
               10  WS-CURRENT-YEAR         PIC 9(4).
               10  WS-CURRENT-MONTH        PIC 9(2).
               10  WS-CURRENT-DAY          PIC 9(2).
           05  WS-CURRENT-TIME.
               10  WS-CURRENT-HOURS        PIC 9(2).
               10  WS-CURRENT-MINUTES      PIC 9(2).
               10  WS-CURRENT-SECONDS      PIC 9(2).
               10  WS-CURRENT-MILLISECONDS PIC 9(2).
           05  WS-GMT-DIFFERENCE           PIC S9(4).

       01  WS-DATE-FORMATTED               PIC X(10).
       01  WS-TIME-FORMATTED               PIC X(8).
       01  WS-DATETIME-FORMATTED           PIC X(19).

       *> Fields for date manipulation
       01  WS-FUTURE-DATE.
           05  WS-FUTURE-YEAR              PIC 9(4).
           05  WS-FUTURE-MONTH             PIC 9(2).
           05  WS-FUTURE-DAY               PIC 9(2).
       01  WS-FUTURE-DATE-FORMATTED        PIC X(10).
       01  WS-DAYS-TO-ADD                  PIC 9(3) VALUE 7.
       01  WS-MONTH-DAYS                   PIC 9(2).

       PROCEDURE DIVISION.
           *> Get current date and time from system
           ACCEPT WS-CURRENT-DATE-DATA FROM DATE YYYYMMDD.

           *> Display raw date components
           DISPLAY "Current Date Components:".
           DISPLAY "  Year: " WS-CURRENT-YEAR.
           DISPLAY "  Month: " WS-CURRENT-MONTH.
           DISPLAY "  Day: " WS-CURRENT-DAY.

        *> Get current time from system
             ACCEPT WS-CURRENT-TIME FROM TIME.

             *> In GnuCOBOL, we can use CALL to run a system command to get timezone
             *> This method uses a temporary file to capture the output
             CALL "SYSTEM" USING
           "powershell -Command \"[System.TimeZoneInfo]::Local.BaseUtcOffset.TotalMinutes >"
           " timezone_temp.txt\""
             END-CALL.

             *> Open the file that contains the timezone offset
             DECLARE TEMP-FILE USAGE IS POINTER.
             CALL "fopen" USING
                 BY REFERENCE "timezone_temp.txt"
                 BY REFERENCE "r"
                 RETURNING TEMP-FILE.

             *> Read the timezone offset value
             IF TEMP-FILE NOT EQUAL NULL
                 CALL "fscanf" USING
                     BY VALUE TEMP-FILE
                     BY REFERENCE "%d"
                     BY REFERENCE WS-GMT-DIFFERENCE
                 END-CALL

                 *> Close the file
                 CALL "fclose" USING
                     BY VALUE TEMP-FILE
                 END-CALL
             ELSE
                 *> Default if file operation fails
                 MOVE 0 TO WS-GMT-DIFFERENCE
             END-IF

             *> Clean up temporary file
             CALL "SYSTEM" USING "del timezone_temp.txt" END-CALL

           DISPLAY "Timezone offset (minutes from UTC): "
           WS-GMT-DIFFERENCE.

           DISPLAY "Current Time Components:".
           DISPLAY "  Hours: " WS-CURRENT-HOURS.
           DISPLAY "  Minutes: " WS-CURRENT-MINUTES.
           DISPLAY "  Seconds: " WS-CURRENT-SECONDS. *> Format date as YYYY-MM-DD
           STRING WS-CURRENT-YEAR DELIMITED BY SIZE
                  "-" DELIMITED BY SIZE
                  WS-CURRENT-MONTH DELIMITED BY SIZE
                  "-" DELIMITED BY SIZE
                  WS-CURRENT-DAY DELIMITED BY SIZE
                  INTO WS-DATE-FORMATTED.

           *> Format time as HH:MM:SS
           STRING WS-CURRENT-HOURS DELIMITED BY SIZE
                  ":" DELIMITED BY SIZE
                  WS-CURRENT-MINUTES DELIMITED BY SIZE
                  ":" DELIMITED BY SIZE
                  WS-CURRENT-SECONDS DELIMITED BY SIZE
                  INTO WS-TIME-FORMATTED.

           *> Combine date and time
           STRING WS-DATE-FORMATTED DELIMITED BY SIZE
                  " " DELIMITED BY SIZE
                  WS-TIME-FORMATTED DELIMITED BY SIZE
                  INTO WS-DATETIME-FORMATTED.

           *> Display formatted date and time
           DISPLAY "Formatted Date: " WS-DATE-FORMATTED.
           DISPLAY "Formatted Time: " WS-TIME-FORMATTED.
           DISPLAY "Formatted DateTime: " WS-DATETIME-FORMATTED.

           *> Calculate a future date (example: add days for checkout date)
           DISPLAY "".
           DISPLAY "Calculating future date (for checkout):"
           DISPLAY "Current date: " WS-DATE-FORMATTED.
           DISPLAY "Days to add: " WS-DAYS-TO-ADD.

           *> Copy current date to future date
           MOVE WS-CURRENT-YEAR TO WS-FUTURE-YEAR.
           MOVE WS-CURRENT-MONTH TO WS-FUTURE-MONTH.
           MOVE WS-CURRENT-DAY TO WS-FUTURE-DAY.

           *> Add days (simple version - doesn't handle month/year boundaries perfectly)
           ADD WS-DAYS-TO-ADD TO WS-FUTURE-DAY.

           *> Basic month adjustment (not handling leap years)
           EVALUATE WS-FUTURE-MONTH
               WHEN 1 WHEN 3 WHEN 5 WHEN 7 WHEN 8 WHEN 10 WHEN 12
                   MOVE 31 TO WS-MONTH-DAYS
               WHEN 4 WHEN 6 WHEN 9 WHEN 11
                   MOVE 30 TO WS-MONTH-DAYS
               WHEN 2
                   MOVE 28 TO WS-MONTH-DAYS
           END-EVALUATE.

           *> Adjust if days exceed month length
           IF WS-FUTURE-DAY > WS-MONTH-DAYS
               SUBTRACT WS-MONTH-DAYS FROM WS-FUTURE-DAY
               ADD 1 TO WS-FUTURE-MONTH
               *> Handle year rollover
               IF WS-FUTURE-MONTH > 12
                   MOVE 1 TO WS-FUTURE-MONTH
                   ADD 1 TO WS-FUTURE-YEAR
               END-IF
           END-IF.

           *> Format future date
           STRING WS-FUTURE-YEAR DELIMITED BY SIZE
                  "-" DELIMITED BY SIZE
                  WS-FUTURE-MONTH DELIMITED BY SIZE
                  "-" DELIMITED BY SIZE
                  WS-FUTURE-DAY DELIMITED BY SIZE
                  INTO WS-FUTURE-DATE-FORMATTED.

           DISPLAY "Checkout date: " WS-FUTURE-DATE-FORMATTED.

           *> Show how to use these dates in a hotel booking context
           DISPLAY "".
           DISPLAY "Example Hotel Booking Record:".
           DISPLAY "  Room: R001".
           DISPLAY "  Customer: CUST12345".
           DISPLAY "  Check-in: " WS-DATE-FORMATTED.
           DISPLAY "  Check-out: " WS-FUTURE-DATE-FORMATTED.
           DISPLAY "  Status: Active".

           STOP RUN.

       END PROGRAM test.
