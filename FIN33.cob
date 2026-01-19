       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIN3.
      *AUTHOR MIGEL H. TAN
      *DATE-WRITTEN. JANUARY 20, 2026.
      *DATE-COMPILED. JANUARY 20, 2026.
      *INSTALLATION. PUP SOUTH WING.
      *SECURITY. BSIT 2-4 AND DR. FABREGAS.
      *REMARKS. FINAL PRACTICE 3.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTFILE ASSIGN TO 'FIN33.TXT'.

       DATA DIVISION.
       FILE SECTION.
       FD OUTFILE.
       01 PRINT-REC.
           05 S-SNA PIC X(20).
           05 S-MNA PIC X(12).
           05 S-AIRTIME PIC X(15).
           05 S-TVIEWERS PIC 9(6).

       01 DATABASE.
           05 FILLER PIC X(80).

       WORKING-STORAGE SECTION.
       01 SCO PIC 9 VALUE ZERO.
       01 SNA PIC X(20) VALUE SPACES.
       01 MCO PIC 99 VALUE ZERO.
       01 MNA PIC X(12) VALUE SPACES.
       01 TCO PIC 9 VALUE ZERO.
       01 AIRTIME PIC X(15) VALUE SPACES.
       01 TVIEWERS PIC 9(6) VALUE ZERO.
       01 EOFSW PIC 9 VALUE ZERO.
       01 CONT PIC X VALUE SPACES.

      *SHOW TABLE - TABLE HANDLING
       01 SHOW-TABLE.
           05 SHOW-ENTRY OCCURS 4 TIMES.
               10 ST-NAME PIC X(20).
               10 ST-TOTAL-VIEWERS PIC 9(6) VALUE ZERO.

      *TRACKING VARIABLES
       01 MWTV PIC X(25) VALUE SPACES.
       01 MAX-VIEWERS PIC 9(6) VALUE ZERO.
       01 MOST-WATCHED-AIRTIME PIC X(15) VALUE SPACES.
       01 I PIC 9 VALUE ZERO.

       SCREEN SECTION.
       01 SCRE.
           05 BLANK SCREEN.

       PROCEDURE DIVISION.
       MAIN-RTN.
      *INITIALIZE SHOW TABLE
           MOVE 'Eat Bulaga' TO ST-NAME(1).
           MOVE "It's Showtime" TO ST-NAME(2).
           MOVE 'PBB' TO ST-NAME(3).
           MOVE 'Probinsiano' TO ST-NAME(4).

           OPEN EXTEND OUTFILE.
           PERFORM PROCESS-RTN UNTIL EOFSW = 1.
           CLOSE OUTFILE.
           PERFORM SUMMARY-RTN.
           STOP RUN.

       PROCESS-RTN.
           PERFORM HEADING-RTN.
           PERFORM ACCEPT-RTN.
           PERFORM NAME-RTN.
           PERFORM MONTH-RTN THRU MONTH-RTN-END.
           PERFORM TIME-RTN THRU TIME-RTN-END.
           PERFORM VIEWERS-RTN.
           PERFORM CONTINUE-RTN THRU CONTINUE-RTN-END.
           PERFORM WRITE-RTN.

       HEADING-RTN.
           DISPLAY SCRE.
           DISPLAY "Television's Leading" LINE 2 COLUMN 23.
           DISPLAY "Primetime Shows" LINE 2 COLUMN 44.
           DISPLAY 'ABS-CBN and GMA Network' LINE 3 COLUMN 30.
           DISPLAY ' ' LINE 4.
           DISPLAY ' ' LINE 5.
           DISPLAY 'January-December, 2025 Survey' LINE 6 COLUMN 26.
           DISPLAY ' ' LINE 7.

       ACCEPT-RTN.
           DISPLAY 'Show Code: ' LINE 8 COLUMN 10.
           ACCEPT SCO LINE 8 COLUMN 50.
           IF SCO < 1 OR SCO > 4
              DISPLAY 'Invalid!' LINE 8 COLUMN 30
              GO TO ACCEPT-RTN
           END-IF.

       NAME-RTN.
           DISPLAY 'Name of the Show: ' LINE 9 COLUMN 10.
      *GET NAME FROM TABLE
           MOVE ST-NAME(SCO) TO SNA.
           DISPLAY SNA LINE 9 COLUMN 50.

       MONTH-RTN.
           DISPLAY 'Month Code: ' LINE 10 COLUMN 10.
           ACCEPT MCO LINE 10 COLUMN 50.
           IF MCO < 1 OR MCO > 12
               DISPLAY 'Invalid!' LINE 10 COLUMN 30
               GO TO MONTH-RTN
           END-IF.
           
           EVALUATE MCO
            WHEN 1
            MOVE 'January' TO MNA
            WHEN 2
            MOVE 'February' TO MNA
            WHEN 3
            MOVE 'March' TO MNA
            WHEN 4
            MOVE 'April' TO MNA
            WHEN 5
            MOVE 'May' TO MNA
            WHEN 6
            MOVE 'June' TO MNA
            WHEN 7
            MOVE 'July' TO MNA
            WHEN 8
            MOVE 'August' TO MNA
            WHEN 9
            MOVE 'September' TO MNA
            WHEN 10
            MOVE 'October' TO MNA
            WHEN 11
            MOVE 'November' TO MNA
            WHEN 12
            MOVE 'December' TO MNA
           END-EVALUATE.

           DISPLAY 'Month Name: ' LINE 11 COLUMN 10.
           DISPLAY MNA LINE 11 COLUMN 50.
       MONTH-RTN-END.

       TIME-RTN.
           DISPLAY 'Time Code: ' LINE 12 COLUMN 10.
           ACCEPT TCO LINE 12 COLUMN 50.

           EVALUATE TCO
            WHEN 1
            MOVE '1200-2:30 pm' TO AIRTIME
            WHEN 2
            MOVE '8:00-10:00pm' TO AIRTIME
            WHEN OTHER
            DISPLAY 'Invalid!' LINE 12 COLUMN 30
            GO TO TIME-RTN
           END-EVALUATE.

           DISPLAY 'Air Time: ' LINE 13 COLUMN 10.
           DISPLAY AIRTIME LINE 13 COLUMN 50.
       TIME-RTN-END.

       VIEWERS-RTN.
           DISPLAY 'Number of Televiewers: ' LINE 14 COLUMN 10.
           ACCEPT TVIEWERS LINE 14 COLUMN 50.
           
           IF TVIEWERS < 1
               DISPLAY 'Invalid!' LINE 14 COLUMN 30
               GO TO VIEWERS-RTN
           END-IF.

       CONTINUE-RTN.
           DISPLAY 'Input Another Record? (Y/N)? ' LINE 17 COLUMN 25.
           ACCEPT CONT LINE 17 COLUMN 55.
           IF CONT = 'Y' OR CONT = 'y'
               MOVE 0 TO EOFSW
           ELSE
               IF CONT = 'N' OR CONT = 'n'
                   MOVE 1 TO EOFSW
               ELSE
                   DISPLAY 'Invalid!' LINE 17 COLUMN 30
                   GO TO CONTINUE-RTN
               END-IF
           END-IF.
       CONTINUE-RTN-END.

       WRITE-RTN.
      *ACCUMULATE VIEWERS FOR THIS SHOW (TABLE HANDLING)
           ADD TVIEWERS TO ST-TOTAL-VIEWERS(SCO).

      *WRITE TO FILE
           MOVE SNA TO S-SNA.
           MOVE MNA TO S-MNA.
           MOVE AIRTIME TO S-AIRTIME.
           MOVE TVIEWERS TO S-TVIEWERS.
           WRITE PRINT-REC.

       SUMMARY-RTN.
      *FIND MOST WATCHED SHOW (LOOP THROUGH TABLE)
           MOVE 0 TO MAX-VIEWERS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 4
               IF ST-TOTAL-VIEWERS(I) > MAX-VIEWERS
                   MOVE ST-TOTAL-VIEWERS(I) TO MAX-VIEWERS
                   MOVE ST-NAME(I) TO MWTV
               END-IF
           END-PERFORM.

      *DISPLAY RESULTS
           DISPLAY SCRE.
           DISPLAY 'Most Watched Television Show: ' LINE 19 COLUMN 10.
           DISPLAY MWTV LINE 19 COLUMN 50.
           
           DISPLAY 'Total no. of Televiewers: ' LINE 21 COLUMN 10.
           DISPLAY MAX-VIEWERS LINE 21 COLUMN 50.
           
           DISPLAY 'Air Time: ' LINE 23 COLUMN 10.
           DISPLAY AIRTIME LINE 23 COLUMN 50.