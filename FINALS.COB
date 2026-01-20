       IDENTIFICATION DIVISION.
       PROGRAM-ID. FINALS.
      *AUTHOR. MIGEL H. TAN.
      *DATE-WRITTEN. JANUARY 20, 2026.
      *DATE-COMPILED. JANUARY 20, 2026.
      *INSTALLATION. PUP SOUTH WING.
      *SECURITY. BSIT 2-4 AND DR. ALETA.
      *REMARKS. FINALS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTFILE ASSIGN TO "TAN.TXT".

       DATA DIVISION.
       FILE SECTION.
       FD OUTFILE.
       01 PRINT-REC.
           05 S-ENO PIC 9(10).
           05 S-ENA PIC X(25).
           05 S-DOB PIC X(20).
           05 S-UNA PIC X(5).
           05 S-CNA PIC X(5).
           05 S-REM PIC X(6).
       01 DATABASE.
           02 FILLER PIC X(80).

       WORKING-STORAGE SECTION.
       01 ENO PIC 9(10) VALUE ZERO.
       01 ENA PIC X(25) VALUE SPACES.
       01 DOB PIC X(20) VALUE SPACES.
       01 UNC PIC 9 VALUE ZERO.
       01 UNA PIC X(5) VALUE SPACES.
       01 CC PIC 9 VALUE ZERO.
       01 CNA PIC X(4) VALUE SPACES.
       01 TOTITEMS PIC 9(3) VALUE ZERO.
       01 TESTR PIC 99 VALUE ZERO.
       01 REM PIC X(6).
       01 RAVERAGE PIC 9(2)V99 VALUE ZERO.
       01 EOFSW PIC 9 VALUE ZERO.
       01 ANOTHER PIC X VALUE SPACES.

      *TOTAL
       01 TOTPASSED PIC 99 VALUE ZERO.
       01 TOTFAILED PIC 99 VALUE ZERO.
       01 LARPAS PIC X(5) VALUE SPACES.

      *UPASS TRACK
       01 UP-PASSED PIC 99 VALUE ZERO.
       01 PUP-PASSED PIC 99 VALUE ZERO.
       01 DLSU-PASSED PIC 99 VALUE ZERO.
       01 ADMU-PASSED PIC 99 VALUE ZERO.
       01 MAPUA-PASSED PIC 99 VALUE ZERO.
       01 MAX-PASSED PIC 99 VALUE ZERO.

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
           PERFORM ACCEPT-RTN.
           PERFORM UNI-RTN.
           PERFORM CC-RTN.
           PERFORM TOTAL-RTN.
           PERFORM TEST-RTN.
           PERFORM REM-RTN.
           PERFORM CON-RTN.
           PERFORM SUMMARY-RTN.
           PERFORM WRITE-RTN.

       HEADING-RTN.
           DISPLAY SCRE.
           DISPLAY ' ' LINE 1.
           DISPLAY 'Professional Regulation' LINE 2 COLUMN 23
           DISPLAY 'Commission' LINE 2 COLUMN 47.
           DISPLAY ' ' LINE 3 COLUMN 40.
           DISPLAY ' ' LINE 4 COLUMN 40.
           DISPLAY 'IT Professional Board ' LINE 5 COLUMN 23.
           DISPLAY 'Exam Results' LINE 5 COLUMN 45.
           DISPLAY '2013' LINE 6 COLUMN 38.

       ACCEPT-RTN.
           DISPLAY 'Examinee Number' LINE 8 COLUMN 10.
           ACCEPT ENO LINE 8 COLUMN 50.

           DISPLAY 'Examinee Name: ' LINE 9 COLUMN 10.
           ACCEPT ENA LINE 9 COLUMN 50.

           DISPLAY 'Date of Birth: ' LINE 10 COLUMN 10.
           ACCEPT DOB LINE 10 COLUMN 50.

       UNI-RTN.
           DISPLAY 'University Code: ' LINE 11 COLUMN 10.
           ACCEPT UNC LINE 11 COLUMN 50.
           EVALUATE UNC
            WHEN 1
            MOVE 'UP' TO UNA
            WHEN 2
            MOVE 'PUP' TO UNA
            WHEN 3
            MOVE 'DLSU' TO UNA
            WHEN 4
            MOVE 'ADMU' TO UNA
            WHEN 5
            MOVE 'MAPUA' TO UNA
            WHEN OTHER
            DISPLAY '1-5 Only!' LINE 11 COLUMN 40
            GO TO UNI-RTN
           END-EVALUATE.

           DISPLAY 'University Name: ' LINE 12 COLUMN 10.
           DISPLAY UNA LINE 12 COLUMN 50.

       CC-RTN.
           DISPLAY 'Course Code: ' LINE 13 COLUMN 10.
           ACCEPT CC LINE 13 COLUMN 50.
           EVALUATE CC
            WHEN 1
            MOVE 'BSIT' TO CNA
            WHEN 2
            MOVE 'BSCS' TO CNA
            WHEN 3
            MOVE 'BSIS' TO CNA
            WHEN OTHER
            DISPLAY '1-3 Only!' LINE 13 COLUMN 40
            GO TO CC-RTN
           END-EVALUATE.

           DISPLAY 'Course Name: ' LINE 14 COLUMN 10.
           DISPLAY CNA LINE 14 COLUMN 50.

       TOTAL-RTN.
           DISPLAY 'Total No. of Items: ' LINE 15 COLUMN 10.
           ACCEPT TOTITEMS LINE 15 COLUMN 50.
           IF TOTITEMS < 1 OR TOTITEMS > 100
           DISPLAY '1-100 Only!' LINE 15 COLUMN 35
           GO TO TOTAL-RTN
           END-IF.

       TEST-RTN.
           DISPLAY 'Test Result (Score): ' LINE 16 COLUMN 10.
           ACCEPT TESTR LINE 16 COLUMN 50.
           IF TESTR IS LESS THAN 1 OR TESTR GREATER THAN TOTITEMS
               DISPLAY '1-100 Only!' LINE 16 COLUMN 35
               GO TO TEST-RTN
               END-IF.

       REM-RTN.
           DISPLAY 'Remarks: ' LINE 17 COLUMN 10.
           COMPUTE RAVERAGE = (TESTR / TOTITEMS) * 100

           IF CC = 1 AND RAVERAGE IS GREATER THAN OR EQUAL 60
           MOVE 'PASSED' TO REM
           ADD 1 TO TOTPASSED
           EVALUATE UNC
            WHEN 1 ADD 1 TO UP-PASSED
            WHEN 2 ADD 1 TO PUP-PASSED
            WHEN 3 ADD 1 TO DLSU-PASSED
            WHEN 4 ADD 1 TO ADMU-PASSED
            WHEN 5 ADD 1 TO MAPUA-PASSED
           END-EVALUATE
            ELSE IF CC = 1 AND RAVERAGE IS LESS THAN 60
            MOVE 'FAILED' TO REM
            ADD 1 TO TOTFAILED
           END-IF.

            IF CC = 2 AND RAVERAGE IS GREATER THAN OR EQUAL 70
           MOVE 'PASSED' TO REM
           ADD 1 TO TOTPASSED
           EVALUATE UNC
            WHEN 1 ADD 1 TO UP-PASSED
            WHEN 2 ADD 1 TO PUP-PASSED
            WHEN 3 ADD 1 TO DLSU-PASSED
            WHEN 4 ADD 1 TO ADMU-PASSED
            WHEN 5 ADD 1 TO MAPUA-PASSED
           END-EVALUATE
           ELSE IF CC = 2 AND RAVERAGE IS LESS THAN 70
           MOVE 'FAILED' TO REM
           ADD 1 TO TOTFAILED
            END-IF.

           IF CC = 3 AND RAVERAGE IS GREATER THAN OR EQUAL 50
           MOVE 'PASSED' TO REM
           ADD 1 TO TOTPASSED
            EVALUATE UNC
            WHEN 1 ADD 1 TO UP-PASSED
            WHEN 2 ADD 1 TO PUP-PASSED
            WHEN 3 ADD 1 TO DLSU-PASSED
            WHEN 4 ADD 1 TO ADMU-PASSED
            WHEN 5 ADD 1 TO MAPUA-PASSED
           END-EVALUATE
           ELSE IF CC = 3 AND RAVERAGE IS LESS THAN 50
           MOVE 'FAILED' TO REM
            ADD 1 TO TOTFAILED
           END-IF.

            DISPLAY REM LINE 17 COLUMN 50.

           EVALUATE UNC
           WHEN 1 ADD 1 TO UP-PASSED
           WHEN 2 ADD 1 TO PUP-PASSED
           WHEN 3 ADD 1 TO DLSU-PASSED
           WHEN 4 ADD 1 TO ADMU-PASSED
           WHEN 5 ADD 1 TO MAPUA-PASSED
           END-EVALUATE

           DISPLAY REM LINE 17 COLUMN 50.

       CON-RTN.
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
                 GO TO CON-RTN
              END-IF
           END-IF.

       SUMMARY-RTN.
           DISPLAY 'Total No. of Passed: ' LINE 21 COLUMN 10.
           DISPLAY TOTPASSED LINE 21 COLUMN 37.

           DISPLAY 'Total No. of Failed: ' LINE 22 COLUMN 10.
           DISPLAY TOTFAILED LINE 22 COLUMN 37.
           DISPLAY ' ' LINE 23 COLUMN 37.


           DISPLAY 'School with Largest' LINE 24 COLUMN 10
           DISPLAY 'no. of Passed: ' LINE 24 COLUMN 30.
           MOVE UP-PASSED TO MAX-PASSED
           MOVE 'UP' TO LARPAS
           IF PUP-PASSED > MAX-PASSED
           MOVE PUP-PASSED TO MAX-PASSED
           MOVE 'PUP' TO LARPAS
           END-IF

           IF DLSU-PASSED > MAX-PASSED
           MOVE DLSU-PASSED TO MAX-PASSED
           MOVE 'DLSU' TO LARPAS
           END-IF

           IF ADMU-PASSED > MAX-PASSED
           MOVE ADMU-PASSED TO MAX-PASSED
           MOVE 'ADMU' TO LARPAS
           END-IF

           IF MAPUA-PASSED > MAX-PASSED
           MOVE MAPUA-PASSED TO MAX-PASSED
           MOVE 'MAPUA' TO LARPAS
           END-IF

           DISPLAY LARPAS LINE 26 COLUMN 56.

       WRITE-RTN.
           MOVE ENO TO S-ENO.
           MOVE ENA TO S-ENA.
           MOVE DOB TO S-DOB.
           MOVE UNA TO S-UNA.
           MOVE CNA TO S-CNA.
           MOVE REM TO S-REM.
           WRITE PRINT-REC.
