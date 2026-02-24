       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIN3.
      *AUTHOR MIGEL H. TAN
      *DATE-WRITTEN. JANUARY 20, 2026.
      *DATE-COMPILED. JANUARY 20, 2026.
      *INSTALLATION. PUP SOUTH WING.
      *SECURITY. BSIT 2-4 AND DR. FABREGAS.
      *REMARKS. FINAL 3.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTFILE ASSIGN TO 'TAN3.TXT'.

       DATA DIVISION.
       FILE SECTION.
       FD OUTFILE.
       01 PRINT-REC.
           05 S-SNA PIC X(20).
           05 S-MNA PIC X(12).
           05 S-AIRTIME PIC X(10).
           05 S-TVIEWERS PIC 9(6).

       01 DATABASE.
           05 FILLER PIC X(80).

       WORKING-STORAGE SECTION.
       01 SCO PIC 9 VALUE ZERO.
       01 SNA PIC X(20) VALUE SPACES.
       01 MCO PIC 99 VALUE ZERO.
       01 MNA PIC X(12) VALUE SPACES.
       01 TCO PIC 9(10) VALUE ZERO.
       01 AIRTIME PIC X(20) VALUE SPACES.
       01 TVIEWERS PIC 9(6) VALUE ZERO.
       01 EOFSW PIC 9 VALUE ZERO.
       01 CONT PIC X VALUE SPACES.

      *track count
       01 EBC PIC 99 VALUE ZERO.
       01 ISC PIC 99 VALUE ZERO.
       01 PBBC PIC 99 VALUE ZERO.
       01 PBC PIC 99 VALUE ZERO.
       01 MWTV PIC X(25) VALUE SPACES.
       01 TOT-TVIEWERS PIC 9(6) VALUE ZERO.
       01 TOT-AIRTIME PIC 9(10) VALUE ZERO.
       
       01 MAXV PIC X(25) VALUE SPACES.
       01 MOWA PIC X(15) VALUE SPACES.
      *4 LANG IARRAY
       01 I PIC 9 VALUE ZERO.

      *TABLE-HANDLING ARRAY DAHIL 4 NA BESES ANG SHOWS
       01 SHOW-TB.
           05 SHOW-ENT OCCURS 4  TIMES.
            10 SH-NAME PIC X(20) VALUE SPACES.
            10 SH-TOTV PIC 9(6) VALUE ZERO.

       SCREEN SECTION.
       01 SCRE.
           05 BLANK SCREEN.

       PROCEDURE DIVISION.
       MAIN-RTN.
           OPEN EXTEND OUTFILE.
           PERFORM PROCESS-RTN UNTIL EOFSW = 1.
           CLOSE OUTFILE.
           STOP RUN.

       PROCESS-RTN.
           PERFORM HEADING-RTN.
           PERFORM ACCEPT-RTN.
           PERFORM NAME-RTN.
           PERFORM MONTH-RTN THRU MONTH-RTN-END.
           PERFORM TIME-RTN THRU TIME-RTN-END.
           PERFORM VIEWERS-RTN.
           PERFORM CONTINUE-RTN THRU CONTINUE-RTN-END.
           PERFORM TRACK-RTN.
           PERFORM SUMMARY-RTN.
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
           EVALUATE SCO
            WHEN 1
            MOVE 'Eat Bulaga' TO SNA
            ADD 1 TO EBC
            WHEN 2
            MOVE "It's Showtime" TO SNA
            ADD 1 TO ISC
            WHEN 3
            MOVE 'PBB' TO SNA
            ADD 1 TO PBBC
            WHEN 4
            MOVE 'Probinsiano' TO SNA
            ADD 1 TO PBC
            WHEN OTHER
            DISPLAY 'Invalid!' LINE 9 COLUMN 30
           GO TO ACCEPT-RTN
           END-EVALUATE.
           DISPLAY SNA LINE 9 COLUMN 50.


       MONTH-RTN.
           DISPLAY 'Month Code: ' LINE 10 COLUMN 10.
           ACCEPT MCO LINE 10 COLUMN 50.
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
            WHEN OTHER
            DISPLAY 'Invalid!' LINE 10 COLUMN 30
            GO TO MONTH-RTN
           END-EVALUATE.

           DISPLAY 'Month Name: ' LINE 11 COLUMN 10.
           DISPLAY MNA LINE 11 COLUMN 50.
       MONTH-RTN-END.

       TIME-RTN.
           DISPLAY 'Time Code: ' LINE 12 COLUMN 10.
           ACCEPT TCO LINE 12 COLUMN 50.

           EVALUATE TCO
      *2:30, 90 MINUTES
            WHEN 1
            MOVE '12:00-2:30 PM' TO AIRTIME
            ADD 90 TO TOT-AIRTIME
            WHEN 2
            MOVE '8:00-10:00 PM' TO AIRTIME
            ADD 60 TO TOT-AIRTIME
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
           ADD TVIEWERS TO TOT-TVIEWERS.

       CONTINUE-RTN.
           DISPLAY 'Input Another Record? (Y/N)? ' LINE 17 COLUMN 25.
           ACCEPT CONT LINE 17 COLUMN 55.
           IF CONT = 'Y' OR CONT = 'y'
               MOVE 0 TO EOFSW
               ELSE
                 IF CONT = 'N' OR CONT = 'n'
                 MOVE 1 TO EOFSW
                 ELSE
                 DISPLAY 'Invalid!' LINE 16 COLUMN 36
                 GO TO CONTINUE-RTN
                 END-IF
           END-IF.
       CONTINUE-RTN-END.

       TRACK-RTN.
      *NAME RTN TRACK
           IF EBC > ISC AND EBC > PBBC AND EBC > PBC
           MOVE 'Eat Bulaga' TO MWTV
           ELSE
           IF ISC > EBC AND ISC > PBBC AND ISC > PBC
           MOVE "It's Showtime" TO MWTV
           ELSE
           IF PBBC > EBC AND PBBC > ISC AND PBBC > PBC
           MOVE 'PBB' TO MWTV
           ELSE
           IF PBC > EBC AND PBC > ISC AND PBC > PBBC
           MOVE 'Probinsiano' TO MWTV
           END-IF
           END-IF
           END-IF
           END-IF.

       SUMMARY-RTN.
           DISPLAY 'Most Watched Television Show: ' LINE 19 COLUMN 10.
           DISPLAY MWTV LINE 19 COLUMN 50.
           DISPLAY 'Total no. of Televiewers: ' LINE 21 COLUMN 10.
           DISPLAY TOT-TVIEWERS LINE 21 COLUMN 50.
           DISPLAY 'Air Time: ' LINE 23 COLUMN 10.
           DISPLAY TOT-AIRTIME LINE 23 COLUMN 50.

       WRITE-RTN.
           MOVE SNA TO S-SNA.
           MOVE MNA TO S-MNA.
           MOVE AIRTIME TO S-AIRTIME
           MOVE TVIEWERS TO S-TVIEWERS
           WRITE PRINT-REC.
