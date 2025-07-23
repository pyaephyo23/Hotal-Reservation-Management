
       IDENTIFICATION DIVISION.
       PROGRAM-ID. summaryReport.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 MENU-CHOICE PIC 9.

       *> Color codes for display
       01 RED-COLOR          PIC X(8) VALUE X"1B5B33316D".
       01 GREEN-COLOR        PIC X(8) VALUE X"1B5B33326D".
       01 RESET-COLOR        PIC X(4) VALUE X"1B5B306D".

       LINKAGE SECTION.
       01 LINK PIC 9.
       PROCEDURE DIVISION USING LINK.
       MAIN-PROCEDURE.
           PERFORM UNTIL MENU-CHOICE = 9
           DISPLAY
           "**************************************************"
           DISPLAY "Generate Summary Report"
           DISPLAY "1. Daily Summary Report"
           DISPLAY "2. Monthly Summary Report"
           DISPLAY "9. Goback"
           DISPLAY
           "**************************************************"
           ACCEPT MENU-CHOICE

           EVALUATE MENU-CHOICE
               WHEN 1 PERFORM DAILY-REPORT
               WHEN 2 PERFORM MONTHLY-REPORT

               WHEN 9 GOBACK
               WHEN OTHER
                   DISPLAY RED-COLOR "Invalid selection." RESET-COLOR
           END-EVALUATE
           END-PERFORM.
           GOBACK.

       DAILY-REPORT.
           CALL 'dailySummaryReport'.
       MONTHLY-REPORT.
           CALL 'monthlySummaryReport'.

       END PROGRAM summaryReport.
