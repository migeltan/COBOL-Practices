       IDENTIFICATION DIVISION.
       PROGRAM-ID. SW2.
      *AUTHOR. GRP12.
      *INSTALLATION. PUP MANILA.
      *DATE-WRITTEN. NOVEMBER 21, 2025.
      *DATE-COMPILED. NOVEMBER 21, 2025.
      *SECURITY. EXVLUSIVE FOR ALL OF US.
      *REMARKS. SEATWORK NO.2.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE ASSIGN TO 'TRANS.txt'
               FILE STATUS WS-FS-IN.
           SELECT OUTFILE ASSIGN TO 'REPORT'
               FILE STATUS WS-FS-OUT.

       DATA DIVISION.
       FILE SECTION.

       FD  INFILE
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 45 CHARACTERS
           DATA RECORD IS INREC.

       01  INREC.
           02 ANO PIC X(10).
           02 ANA PIC X(25).
           02 TC PIC X(1).
           02 TAMT PIC 9(7)V99.

       FD  OUTFILE
           LABEL RECORD IS OMITTED
           DATA RECORD IS OUTREC.

       01 OUTREC.
           02 FILLER PIC X(80).

       WORKING-STORAGE SECTION.

       01  WS-FS-IN PIC XX VALUE SPACES.
       01  WS-FS-OUT PIC XX VALUE SPACES.
       01  EOF-FLAG PIC X VALUE 'N'.
       01  HOLD-ACC-NO  PIC X(10) VALUE SPACES.
       01  HOLD-ACC-NAME PIC X(25) VALUE SPACES.
       01  WS-DEPOSIT PIC 9(7)V99 VALUE 0.
       01  WS-WITHDRAW PIC 9(7)V99 VALUE 0.
       01  WS-BALANCE PIC 9(7)V99 VALUE 0.
       01  GRAND-BALANCE PIC 9(10)V99 VALUE 0.
       01  REC-COUNT PIC 9(6) VALUE 0.

       01  HEAD-1.
           02 FILLER PIC X(30) VALUE SPACES.
           02 FILLER PIC X(20) VALUE 'China Trust Bank'.
           02 FILLER PIC X(30) VALUE SPACES.

       01  HEAD-2.
           02 FILLER PIC X(31) VALUE SPACES.
           02 FILLER PIC X(14) VALUE 'Makati Avenue'.
           02 FILLER PIC X(35) VALUE SPACES.

       01  HEAD-3.
           02 FILLER PIC X(32) VALUE SPACES.
           02 FILLER PIC X(12) VALUE 'Makati City'.
           02 FILLER PIC X(36) VALUE SPACES.

       01  SUBHEAD-1.
           02 FILLER PIC X(30) VALUE SPACES.
           02 FILLER PIC X(20) VALUE 'Account''s Report'.
           02 FILLER PIC X(30) VALUE SPACES.

       01  SUBHEAD-2.
           02 FILLER PIC X(6) VALUE SPACES.
           02 FILLER PIC X(7) VALUE 'Account'.
           02 FILLER PIC X(6) VALUE SPACES.
           02 FILLER PIC X(7) VALUE 'Account'.
           02 FILLER PIC X(22) VALUE SPACES.
           02 FILLER PIC X(7) VALUE 'Balance'.

       01  SUBHEAD-3.
           02 FILLER PIC X(6) VALUE SPACES.
           02 FILLER PIC X(6) VALUE 'Number'.
           02 FILLER PIC X(8) VALUE SPACES.
           02 FILLER PIC X(4) VALUE 'Name'.
           02 FILLER PIC X(56) VALUE SPACES.

       01  DETAIL-LINE.
           02 FILLER PIC X(2)  VALUE SPACES.
           02 P-ANO PIC X(10).
           02 FILLER PIC X(5)  VALUE SPACES.
           02 P-ANA PIC X(25).
           02 FILLER PIC X(5)  VALUE SPACES.
           02 P-BAL PIC Z,ZZZ,ZZZ.99.

       01  TOTAL-REC.
           02 FILLER PIC X(2) VALUE SPACES.
           02 FILLER PIC X(22) VALUE 'Total No. of Records:'.
           02 P-REC PIC Z,ZZZ,ZZ9.
           02 FILLER PIC X(50) VALUE SPACES.

       01  TOTAL-BAL.
           02 FILLER PIC X(2) VALUE SPACES.
           02 FILLER PIC X(26) VALUE 'Total Accumulated Balance:'.
           02 FILLER PIC X(2) VALUE 'P '.
           02 P-GRAND PIC Z,ZZZ,ZZZ,ZZ9.99.
           02 FILLER PIC X(30) VALUE SPACES.

       01  BLANK-LINE.
           02 FILLER PIC X(80) VALUE SPACES.

       SCREEN SECTION.
       01 SCRE.
           02 BLANK SCREEN.

       PROCEDURE DIVISION.
       MAIN-RTN.
           PERFORM INIT-RTN.
           IF WS-FS-IN NOT = '00'
               DISPLAY 'ERROR OPENING INFILE. FILE STATUS: ' WS-FS-IN
               GO TO END-PROG
           END-IF
           IF WS-FS-OUT NOT = '00'
               DISPLAY 'ERROR OPENING OUTFILE. FILE STATUS: ' WS-FS-OUT
               GO TO END-PROG
           END-IF

           PERFORM PROCESS-LOOP UNTIL EOF-FLAG = 'Y'.
           IF REC-COUNT > 0
               PERFORM ACCOUNT-BREAK
           END-IF

           PERFORM FINAL-RTN.

       END-PROG.
           STOP RUN.

       INIT-RTN.
           OPEN INPUT INFILE OUTPUT OUTFILE.
           READ INFILE
               AT END
                   MOVE 'Y' TO EOF-FLAG
               NOT AT END
                   MOVE ANO TO HOLD-ACC-NO
                   MOVE ANA TO HOLD-ACC-NAME
           END-READ.

           PERFORM PRINT-HEADERS.
       INIT-RTN-END.

       PRINT-HEADERS.
           WRITE OUTREC FROM HEAD-1 AFTER PAGE.
           WRITE OUTREC FROM HEAD-2 AFTER 1.
           WRITE OUTREC FROM HEAD-3 AFTER 1.
           WRITE OUTREC FROM SUBHEAD-1 AFTER ADVANCING 3 LINES.
           WRITE OUTREC FROM SUBHEAD-2 AFTER ADVANCING 2 LINES.
           WRITE OUTREC FROM SUBHEAD-3 AFTER 1.
       PRINT-HEADERS-END.

       PROCESS-LOOP.
           PERFORM UNTIL EOF-FLAG = 'Y'
               IF TC = 'D'
                   ADD TAMT TO WS-DEPOSIT
               END-IF
               IF TC = 'W'
                   ADD TAMT TO WS-WITHDRAW
               END-IF

               ADD 1 TO REC-COUNT

               READ INFILE
                   AT END
                       MOVE 'Y' TO EOF-FLAG
                   NOT AT END
                       IF ANO NOT = HOLD-ACC-NO
                           PERFORM ACCOUNT-BREAK
                           MOVE ANO TO HOLD-ACC-NO
                           MOVE ANA TO HOLD-ACC-NAME
                       END-IF
               END-READ
           END-PERFORM.
       PROCESS-LOOP-END.

       ACCOUNT-BREAK.
           COMPUTE WS-BALANCE = WS-DEPOSIT - WS-WITHDRAW.
           ADD WS-BALANCE TO GRAND-BALANCE.

           MOVE HOLD-ACC-NO TO P-ANO.
           MOVE HOLD-ACC-NAME TO P-ANA.
           MOVE WS-BALANCE TO P-BAL.

           WRITE OUTREC FROM DETAIL-LINE AFTER 1.
           MOVE 0 TO WS-DEPOSIT WS-WITHDRAW WS-BALANCE.
       ACCOUNT-BREAK-END.

       FINAL-RTN.
           MOVE REC-COUNT TO P-REC
           MOVE GRAND-BALANCE TO P-GRAND

           WRITE OUTREC FROM BLANK-LINE AFTER 2
           WRITE OUTREC FROM TOTAL-REC AFTER 1
           WRITE OUTREC FROM TOTAL-BAL AFTER 1

           CLOSE INFILE OUTFILE
           DISPLAY 'REPORT GENERATED (REPORT).'.
       FINAL-RTN-END.
