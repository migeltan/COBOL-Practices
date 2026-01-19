       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIN2.
      *AUTHOR. MIGEL H. TAN.
      *INSTALLATION. PUP STA. MESA.
      *DATE-WRITTEN. JAUARY 20. 2026.
      *DATE-COMPILED. JANUARY 20, 2026.
      *SECURITY. BSIT 2-4 AND DR. FABREGAS.
      *REMARKS. FINAL 2.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT OUTFILE ASSIGN TO "TAN1.TXT".

       DATA DIVISION.
       FILE SECTION.

       FD OUTFILE.
       01 PRINT-REC.
           05 P-NOS PIC X(20).
      *    05 P-WS PIC 9(3).
           05 P-TOS PIC X(20).
           05 P-CRF PIC X(6).
           05 P-AFA PIC X(10).
           05 P-MON PIC X(12).

       01 DATABASE.
           05 FILLER PIC X(80).

       WORKING-STORAGE SECTION.
      *COMMON fields
       01 NOS PIC X(20) VALUE SPACES.
       01 WS PIC 9(3) VALUE ZERO.
       01 TOS PIC X(20) VALUE SPACES.
       01 CRF PIC X(6) VALUE SPACES.
       01 ACT PIC X(20) VALUE SPACES.
       01 AFA PIC X(10) VALUE SPACES.
       01 MON PIC 9(2) VALUE ZERO.
       01 NMON PIC X(12) VALUE SPACES.
       01 CON PIC X VALUE SPACES.

      *tracking highest ws, sname, hp
       01 H-WS PIC 9(3) VALUE ZERO.
       01 S-STN PIC X(20) VALUE SPACES.
       01 H-SP PIC 9(3) VALUE ZERO.

       01 EOFSW PIC 9 VALUE ZERO.

      *counts LVM, and most flooded
       01 LZC PIC 99 VALUE ZERO.
       01 VSC PIC 99 VALUE ZERO.
       01 MDC PIC 99 VALUE 0.
       01 MFA PIC X(10) VALUE SPACES.

      *months, INDEXING, tsaka moncount, monw most, mon w name
       01 MONC.
           05 M-C OCCURS 12 TIMES PIC 99 VALUE ZERO.
       01 MAXMONC PIC 99 VALUE ZERO.
       01 MONWMOST PIC 9(2) VALUE ZERO.
       01 MONNAME PIC X(12) VALUE SPACES.
       01 I PIC 99 VALUE ZERO.

       SCREEN SECTION.
       01 SCRE.
           02 BLANK SCREEN.

       PROCEDURE DIVISION.
       MAIN-RTN.
           OPEN EXTEND OUTFILE.
           PERFORM PROCESS-RECORDS UNTIL EOFSW = 1.
           CLOSE OUTFILE.
           STOP RUN.

       PROCESS-RECORDS.
           PERFORM HEADING-RTN.
           PERFORM ACCEPT-RTN.
           PERFORM WS-RTN.
           PERFORM TYPE-STORM-RTN THRU TYPE-STORM-RTN-END.
           PERFORM COLOR-RTN THRU COLOR-RTN-END.
           PERFORM AFFECTED-RTN THRU AFFECTED-RTN-END.
           PERFORM MONTH-RTN THRU MONTH-RTN-END.
           PERFORM CONTINUE-RTN THRU CONTINUE-RTN-END.
           PERFORM WRITE-RTN THRU WRITE-RTN-END.
           PERFORM TRACK-RTN.
           PERFORM SUMMARY-RTN THRU SUMMARY-RTN-END.

       HEADING-RTN.
           DISPLAY SCRE.
           DISPLAY 'PAG-ASA' LINE 1 COLUMN 37.
           DISPLAY ' ' LINE 2.
           DISPLAY ' ' LINE 3.
           DISPLAY 'Weather Situation in ' LINE 4 COLUMN 22.
           DISPLAY 'the Philippines' LINE 4 COLUMN 43.
           DISPLAY 'Year 2025' LINE 5 COLUMN 35.
           DISPLAY ' ' LINE 6.

       ACCEPT-RTN.
           DISPLAY 'Name of Storm: ' LINE 7 COLUMN 10.
           ACCEPT NOS LINE 7 COLUMN 55.
       WS-RTN.
           DISPLAY 'Wind Speed (in km): ' LINE 8 COLUMN 10.
           ACCEPT WS LINE 8 COLUMN 55.

       TYPE-STORM-RTN.
           IF WS < 1
           DISPLAY 'Invalid!' LINE 8 COLUMN 30
           GO TO WS-RTN
           ELSE
           IF WS < 63
            MOVE 'LOW PRESSURE AREA' TO TOS
           ELSE
            IF WS >= 63 AND WS <= 118
                MOVE 'TROPICAL DEPRESSION' TO TOS
            ELSE
                IF WS >= 118 AND <= 184
                    MOVE 'TYPHOON SIGNAL NO. 4' TO TOS
               ELSE 
                   IF WS > 184
                    MOVE 'TYPHOON SIGNAL NO. 5' TO TOS
               END-IF
               END-IF
            END-IF
           END-IF
           DISPLAY TOS LINE 9 COLUMN 55
           END-IF.
                      DISPLAY 'Type of Storm: ' LINE 9 COLUMN 10.
       TYPE-STORM-RTN-END.

       COLOR-RTN.
           DISPLAY 'Color Coded Rainfall ' LINE 10 COLUMN 10.
           DISPLAY 'Warning System:' LINE 10 COLUMN 31.
           ACCEPT CRF LINE 10 COLUMN 55.

           DISPLAY 'Action/Response: ' LINE 11 COLUMN 10.

           EVALUATE CRF
            WHEN 'Yellow'
            WHEN 'YELLOW'
            WHEN 'yellow'
            DISPLAY 'Response Monitor' LINE 11 COLUMN 55

            WHEN 'Orange'
            WHEN 'ORANGE'
            WHEN 'orange'
            DISPLAY 'Response Alert' LINE 11 COLUMN 55

            WHEN 'Red'
            WHEN 'RED'
            WHEN 'red'
            DISPLAY 'Response Evacuation' LINE 11 COLUMN 55

            WHEN OTHER
            DISPLAY 'Invalid!' LINE 11 COLUMN 30
            GO TO COLOR-RTN
           END-EVALUATE.
       COLOR-RTN-END.

       AFFECTED-RTN.
           DISPLAY 'Most Affected or Target Area: ' LINE 12 COLUMN 10.
           ACCEPT AFA LINE 12 COLUMN 55.

           IF AFA NOT = 'LUZON' AND AFA NOT = 'VISAYAS'
              AND AFA NOT = 'MINDANAO'
              AND AFA NOT = 'Luzon' AND AFA NOT = 'Visayas'
              AND AFA NOT = 'Mindanao'
              DISPLAY 'Invalid!' LINE 12 COLUMN 40
              GO TO AFFECTED-RTN.
       AFFECTED-RTN-END.

       MONTH-RTN.
           DISPLAY 'Month Occured: ' LINE 13 COLUMN 10.
           ACCEPT MON LINE 13 COLUMN 55.
           IF MON < 1 OR MON > 12
               DISPLAY 'Invalid!' LINE 13 COLUMN 30
           GO TO MONTH-RTN
           ELSE
           EVALUATE MON
            WHEN 1
                MOVE 'January' TO NMON
            WHEN 2
                MOVE 'February' TO NMON
            WHEN 3
                MOVE 'March' TO NMON
            WHEN 4
                MOVE 'April' TO NMON
            WHEN 5
                MOVE 'May' TO NMON
            WHEN 6
                MOVE 'June' TO NMON
            WHEN 7
                MOVE 'July' TO NMON
            WHEN 8
                MOVE 'August' TO NMON
            WHEN 9
                MOVE 'September' TO NMON
            WHEN 10
                MOVE 'October' TO NMON
            WHEN 11
                MOVE 'November' TO NMON
            WHEN 12
                MOVE 'December' TO NMON
           END-EVALUATE
           END-IF.
       MONTH-RTN-END.

       CONTINUE-RTN.
           DISPLAY 'Input Another Record? (Y/N): ' LINE 16 COLUMN 25.
           ACCEPT CON LINE 16 COLUMN 55.
           IF CON = 'Y' OR CON = 'y'
               MOVE 0 TO EOFSW
               ELSE
                   IF CON = 'N' OR CON = 'n'
                   MOVE 1 TO EOFSW
                   ELSE
                   DISPLAY 'Invalid!' LINE 15 COLUMN 30
                   GO TO CONTINUE-RTN
                   END-IF
           END-IF.
       CONTINUE-RTN-END.

       TRACK-RTN.
      *track high ws, strongest name
           IF WS > H-WS
           MOVE WS TO H-WS
           MOVE NOS TO S-STN
           END-IF.

      * track lvm
           EVALUATE AFA
            WHEN 'LUZON'
            WHEN 'luzon'
            WHEN 'Luzon'
            ADD 1 TO LZC

            WHEN 'VISAYAS'
            WHEN 'visayas'
            WHEN 'Visayas'
            ADD 1 TO VSC

            WHEN 'MINDANAO'
            WHEN 'mindanao'
            WHEN 'Mindanao'
            ADD 1 TO MDC
           END-EVALUATE.
      *TABLE
           ADD 1 TO M-C(MON).

       WRITE-RTN.
           MOVE NOS TO P-NOS.
           MOVE TOS TO P-TOS.
           MOVE AFA TO P-AFA
           MOVE NMON TO P-MON
           WRITE PRINT-REC.
       WRITE-RTN-END.

       SUMMARY-RTN.
      *COND TO CHECK MOST FLOODED
           IF LZC > VSC AND LZC > MDC
           MOVE 'LUZON' TO MFA
           ELSE
           IF VSC > LZC AND VSC > MDC
           MOVE 'VISAYAS' TO MFA
           ELSE
           IF MDC > LZC AND MDC > VSC
              MOVE 'MINDANAO' TO MFA
           END-IF
           END-IF.

      *mOST STORMS, IF MC INDEX
           MOVE 0 TO MAXMONC
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 12
                IF M-C(I) > MAXMONC
                    MOVE M-C(I) TO MAXMONC
                    MOVE I TO MONWMOST
                END-IF
              END-PERFORM.

      *NUMBER TO NAME MONTH
              EVALUATE MONWMOST
                WHEN 1
                 MOVE 'January' TO MONNAME
                WHEN 2
                 MOVE 'February' TO MONNAME
                WHEN 3
                 MOVE 'March' TO MONNAME
                WHEN 4
                 MOVE 'April' TO MONNAME
                WHEN 5
                 MOVE 'May' TO MONNAME
                WHEN 6
                 MOVE 'June' TO MONNAME
                WHEN 7
                 MOVE 'July' TO MONNAME
                WHEN 8
                 MOVE 'August' TO MONNAME
                WHEN 9
                 MOVE 'September' TO MONNAME
                WHEN 10
                 MOVE 'October' TO MONNAME
                WHEN 11
                 MOVE 'November' TO MONNAME
                WHEN 12
                 MOVE 'December' TO MONNAME
              END-EVALUATE.

      *RESULT
           DISPLAY 'The Strongest Storm ' LINE 19 COLUMN 10
           DISPLAY 'in a Year is: ' LINE 19 COLUMN 30.
           DISPLAY S-STN LINE 19 COLUMN 55.

           DISPLAY 'The Most Flooded Area: ' LINE 20 COLUMN 10.
           DISPLAY MFA LINE 20 COLUMN 55.

           DISPLAY 'The Month with the ' LINE 21 COLUMN 10
           DISPLAY 'Most Numbered of Storms: ' LINE 21 COLUMN 29.
           DISPLAY MONNAME LINE 21 COLUMN 55.
           DISPLAY ' ' LINE 22.
       SUMMARY-RTN-END.
