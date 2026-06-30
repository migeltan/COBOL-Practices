       IDENTIFICATION DIVISION.
       PROGRAM-ID. SWT2.
      *AUTHOR. MIGEL TAN.
      *INSTALLATION. VALENZUELA CITY.
      *DATE-WRITTEN. JANUARY 9, 2025.
      *DATE-COMPILED. JANUARY 9, 2025.
      *SECURITY. BSIT 2-4 AND DR. FABREGAS.
      *REMARKS. DISP AND ACC.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  SNO PIC X(5) VALUE SPACES.
       01  SNA PIC X(25) VALUE SPACES.
       01  CC PIC 9 VALUE ZERO.
       01  YRSEC PIC X(5) VALUE SPACES.
       01  STYPE PIC 9 VALUE ZERO.
       01  MG PIC 9V99 VALUE ZERO.
       01  FG PIC 9V99 VALUE ZERO.
       01  AVE PIC 9V99 VALUE ZERO.
       01  REM-I PIC X(6) VALUE SPACES.
       01  DEC PIC X VALUE SPACES.
       01  INDICATOR PIC 9 VALUE ZERO.
       01  AVE-R PIC 9.999 VALUE ZERO.

       SCREEN SECTION.
       01  SCRE.
           02 BLANK SCREEN.

       PROCEDURE DIVISION.
       MAIN-RTN.
           PERFORM INPUT-RTN UNTIL INDICATOR = 1.
           STOP RUN.

       INPUT-RTN.
           DISPLAY SCRE.
           DISPLAY 'PUP' LINE 1 COLUMN 40.
           DISPLAY 'CCIS' LINE 2 COLUMN 40.

           DISPLAY 'Student Number: ' LINE 5 COLUMN 10.
           ACCEPT SNO LINE 5 COLUMN 50.

           DISPLAY 'Student Name: ' LINE 6 COLUMN 10.
           ACCEPT SNA LINE 6 COLUMN 50.

           DISPLAY 'Course: ' LINE 7 COLUMN 10.
           ACCEPT CC LINE 7 COLUMN 50.

           DISPLAY 'Year & Section: ' LINE 8 COLUMN 10.
           ACCEPT YRSEC LINE 8 COLUMN 50.

           DISPLAY 'Student Type: ' LINE 9 COLUMN 10.
           ACCEPT STYPE LINE 9 COLUMN 50.

           DISPLAY 'Midterm Grade: ' LINE 10 COLUMN 10.
           ACCEPT MG LINE 10 COLUMN 50.

           DISPLAY 'Final Grade: ' LINE 11 COLUMN 10.
           ACCEPT FG LINE 11 COLUMN 50.

           COMPUTE AVE = (MG + FG) / 2.
           MOVE AVE TO AVE-R.

           DISPLAY 'Average: ' LINE 12 COLUMN 10.
           DISPLAY AVE-R LINE 12 COLUMN 50.

           IF AVE > 3.00
               MOVE "FAILED" TO REM-I
           ELSE
               MOVE "PASSED" TO REM-I
           END-IF.

           DISPLAY 'Remarks: ' LINE 13 COLUMN 10.
           DISPLAY REM-I LINE 13 COLUMN 50.

           DISPLAY "Enter Another Record (Y/N): " LINE 15 COLUMN 10.
           ACCEPT DEC LINE 15 COLUMN 50.

           IF DEC IS = 'Y'
               MOVE 0 TO AVE
               MOVE SPACES TO REM-I
           ELSE IF DEC IS = 'N'
               MOVE 1 TO INDICATOR
           END-IF.
