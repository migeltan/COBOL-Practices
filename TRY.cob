       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRY.
      *AUTHOR. MIGEL H. TAN.
      *DATE-WRITTEN. MARCH 4, 2026.
      *DATE-COMPILED. MARCH 4, 2026.
      *INSTALLATION. PUP, STA. MESA.
      *SECURITY. BSIT 2-4 AND DR. ALETA FABREGAS.
      *REMARKS. NAMISS KO MAG-COBOL.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTFILE ASSIGN TO "TEST1.TXT".

       DATA DIVISION.
       FILE SECTION.
       FD OUTFILE.
       01 PRINT-REC.
           05 P-SNA PIC X(35).
           05 P-SNO PIC 9(5).
           05 P-CNA PIC X(4).
           05 P-UNA PIC X(10). 
      *UNINAME
           05 P-AVE PIC 9(2)V99.
           05 P-REM PIC X(6).

       01 DATABASE.
           05 FILLER PIC X(80).

       WORKING-STORAGE SECTION.
       01 SNA PIC X(35) VALUE SPACES.
       01 SNO PIC 9(5) VALUE ZERO.
       01 CC PIC 9 VALUE ZERO.
      *COURSE CODE
       01 CNA PIC X(4) VALUE SPACES.
       01 UC PIC 9 VALUE ZERO.
      *UNIVERSITY CODE
       01 UNA PIC X(10) VALUE SPACES.  
       01 MID PIC 9(2)V99 VALUE ZERO.
       01 FIN PIC 9(2)V99 VALUE ZERO.
       01 AVE PIC 9(2)V99 VALUE ZERO.
       01 REM PIC X(6) VALUE ZERO.
       01 EOFSW PIC 9 VALUE ZERO.
       01 ANOTHER PIC X VALUE SPACES.

      *TOTALS
       01 TNP PIC 9(3) VALUE ZERO.
       01 TNF PIC 9(3) VALUE ZERO.
       01 TOPUNI PIC X(10) VALUE SPACES.

       SCREEN SECTION.
       01 SCRE.
           02 BLANK SCREEN.

       PROCEDURE DIVISION.
       MAIN-RTN.
           OPEN EXTEND OUTFILE.
           PERFORM PROCESS-RTN UNTIL EOFSW = 1.
           CLOSE OUTFILE.
           STOP RUN.

       PROCESS-RTN.
           PERFORM HEADING-RTN.
           PERFORM NAME-NUMBER-RTN.
           PERFORM COURSE-UNI-RTN THRU COURSE-UNI-RTN-END.
           PERFORM GRADE-RTN THRU GRADE-RTN-END.
           PERFORM CONTINUE-RTN.
           PERFORM PRINT-RTN.

       HEADING-RTN.
           DISPLAY SCRE.
           DISPLAY " " LINE 1 COLUMN 1.
           DISPLAY "Cisco Certified" LINE 2 COLUMN 21.
           DISPLAY " Network Administration" LINE 2 COLUMN 36.
           DISPLAY " " LINE 3 COLUMN 1.
           DISPLAY "Course Results" LINE 4 COLUMN 34.
           DISPLAY "2026" LINE 5 COLUMN 38.

       NAME-NUMBER-RTN.
           DISPLAY "Enter your name: " LINE 7 COLUMN 5.
           ACCEPT SNA LINE 7 COLUMN 60.

           DISPLAY "Enter your student number: " LINE 8 COLUMN 5.
           ACCEPT SNO LINE 8 COLUMN 60.

       COURSE-UNI-RTN.

       UNI-RTN.
           DISPLAY "Enter your university code: " LINE 9 COLUMN 5.
           ACCEPT UC LINE 9 COLUMN 60.
           EVALUATE UC
            WHEN 1
            MOVE "UP" TO UNA
            WHEN 2
            MOVE "ADMU" TO UNA
            WHEN 3
            MOVE "DLSU" TO UNA
            WHEN 4
            MOVE "UST" TO UNA
            WHEN 5
            MOVE "PUP" TO UNA
            WHEN 6
            MOVE "MAPUA" TO UNA
            WHEN 7
            MOVE "FEU" TO UNA
            WHEN OTHER
            DISPLAY "1-7 Only!" LINE 9 COLUMN 35
            GO TO UNI-RTN
           END-EVALUATE.

           DISPLAY "University: " LINE 10 COLUMN 5.
           DISPLAY UNA LINE 10 COLUMN 60.

       COURSE-RTN.
           DISPLAY "Enter your program code: " LINE 11 COLUMN 5.
           ACCEPT CC LINE 11 COLUMN 60.
           EVALUATE CC
            WHEN 1
            MOVE "BSCS" TO CNA
            WHEN 2
            MOVE "BSIT" TO CNA
            WHEN 3
            MOVE "BSIS" TO CNA
            WHEN 4
            MOVE "BSCpE" TO CNA
            WHEN OTHER
            DISPLAY "1-4 Only!" LINE 11 COLUMN 35
            GO TO COURSE-RTN
           END-EVALUATE.

           DISPLAY "Program: " LINE 12 COLUMN 5.
           DISPLAY CNA LINE 12 COLUMN 60.

       COURSE-UNI-RTN-END.

       GRADE-RTN.

       MID-RTN.
           DISPLAY "Enter your midterm grade: " LINE 13 COLUMN 5.
           ACCEPT MID LINE 13 COLUMN 60.
           IF MID < 1.00 OR MID > 5.00
               DISPLAY "1.00 - 5.00 only!" LINE 13 COLUMN 35
               GO TO MID-RTN
               END-IF.

       FIN-RTN.
           DISPLAY "Enter your final grade: " LINE 14 COLUMN 5.
           ACCEPT FIN LINE 14 COLUMN 60.
           IF FIN < 1.00 OR FIN > 5.00
               DISPLAY "1.00 - 5.00 only!" LINE 14 COLUMN 35
               GO TO FIN-RTN
               END-IF.
       
       REM-RTN.
           COMPUTE AVE = (MID + FIN) / 2
           DISPLAY "Your average is: " LINE 15 COLUMN 5.
           DISPLAY AVE LINE 15 COLUMN 60.

           DISPLAY "Remarks: " LINE 16 COLUMN 5.
           IF AVE > 1.00 AND AVE < 1.15
               DISPLAY "Summa Cum Laude!" LINE 16 COLUMN 60
               MOVE "Passed" TO REM
           ELSE IF AVE > 1.15 AND AVE < 1.50
               DISPLAY "Magna Cum Laude!" LINE 16 COLUMN 60
               MOVE "Passed" TO REM
           ELSE IF AVE > 1.50 AND AVE < 1.75
               DISPLAY "Cum Laude!" LINE 16 COLUMN 60
               MOVE "Passed" TO REM
           ELSE IF AVE > 1.75 AND AVE < 3.00 
               DISPLAY "Passed!" LINE 16 COLUMN 60
               MOVE "Passed" TO REM
           ELSE 
               DISPLAY "Failed." LINE 16 COLUMN 60
               MOVE "Failed" TO REM
           END-IF
           END-IF
           END-IF
           END-IF.
       GRADE-RTN-END.
           
       CONTINUE-RTN.
           DISPLAY 'Input Another Record? (Y/N): ' LINE 19 COLUMN 26.
           ACCEPT ANOTHER LINE 19 COLUMN 55.
           IF ANOTHER = 'Y' OR ANOTHER = 'y'
           MOVE 0 TO EOFSW
           MOVE SPACES TO REM
               ELSE
               IF ANOTHER = 'N' OR ANOTHER = 'n'
               MOVE 1 TO EOFSW
                 ELSE
                 DISPLAY 'Y or N only!' LINE 18 COLUMN 32
                 GO TO CONTINUE-RTN
              END-IF
           END-IF.
       
       PRINT-RTN.
           MOVE SNA TO P-SNA.
           MOVE SNO TO P-SNO.
           MOVE CNA TO P-CNA.
           MOVE UNA TO P-UNA.
           MOVE AVE TO P-AVE.
           MOVE REM TO P-REM.