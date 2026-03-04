       IDENTIFICATION DIVISION.
       PROGRAM-ID. HW1.
      *AUTHOR. MIGEL H. TAN.
      *INSTALLATION. VALENZUELA CITY.
      *DATE-WRITTEN. NOVEMBER 21, 2025.
      *DATE-COMPILED. NOVEMBER 21, 2025.
      *REMARKS. HOMEWORK/SEATWORK PAGE 93 ENHANCED.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE  ASSIGN TO "TRANSV.txt".
           SELECT OUTFILE ASSIGN TO "REPORT2".

       DATA DIVISION.
       FILE SECTION.

       FD  INFILE
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 45 CHARACTERS
           DATA RECORD IS INREC.

       01  INREC.
           02 ANO  PIC X(10).
           02 ANA  PIC X(25).
           02 TC   PIC X.
           02 TAMT PIC 9(7)V99.

       FD  OUTFILE
           LABEL RECORD IS OMITTED
           DATA RECORD IS OUTREC.

       01  OUTREC.
           02 FILLER PIC X(80).

       WORKING-STORAGE SECTION.

       01 EOFSW          PIC 9 VALUE 0.
       01 HOLD-ACC-NO    PIC X(10) VALUE SPACES.
       01 HOLD-ACC-NAME  PIC X(25) VALUE SPACES.
       01 WS-BALANCE     PIC S9(9)V99 VALUE 0.
       01 GRAND-BALANCE  PIC S9(10)V99 VALUE 0.
       01 REC-COUNT      PIC 9(3) VALUE 0.
       01 PAGE-COUNT     PIC 9(2) VALUE 0.
       01 INVALID-FLAG   PIC 9 VALUE 0.
       01 WS-FORMATTED PIC ZZ,ZZZ,ZZZ,ZZ9.99.

       01 HEAD-1.
           02 FILLER PIC X(30) VALUE SPACES.
           02 FILLER PIC X(20) VALUE "China Trust Bank".
           02 FILLER PIC X(30) VALUE SPACES.

       01 HEAD-2.
           02 FILLER PIC X(32) VALUE SPACES.
           02 FILLER PIC X(14) VALUE "Makati Avenue".
           02 FILLER PIC X(34) VALUE SPACES.

       01 HEAD-3.
           02 FILLER PIC X(33) VALUE SPACES.
           02 FILLER PIC X(12) VALUE "Makati City".
           02 FILLER PIC X(35) VALUE SPACES.

       01 SUBHEAD-1.
           02 FILLER PIC X(30) VALUE SPACES.
           02 FILLER PIC X(20) VALUE "Account's Report".
           02 FILLER PIC X(30) VALUE SPACES.

       01 SUBHEAD-2.
           02 FILLER PIC X(6) VALUE SPACES.
           02 FILLER PIC X(7) VALUE "Account".
           02 FILLER PIC X(6) VALUE SPACES.
           02 FILLER PIC X(7) VALUE "Account".
           02 FILLER PIC X(22) VALUE SPACES.
           02 FILLER PIC X(7) VALUE "Balance".

       01 SUBHEAD-3.
           02 FILLER PIC X(6) VALUE SPACES.
           02 FILLER PIC X(6) VALUE "Number".
           02 FILLER PIC X(8) VALUE SPACES.
           02 FILLER PIC X(4) VALUE "Name".
           02 FILLER PIC X(56) VALUE SPACES.

       01 DETALYE.
           02 FILLER PIC X(2) VALUE SPACES.
           02 P-ANO PIC X(10).
           02 FILLER PIC X(5) VALUE SPACES.
           02 P-ANA PIC X(25).
           02 FILLER PIC X(5) VALUE SPACES.
           02 P-BAL PIC X(18).

       01 TOTAL-REC.
           02 FILLER PIC X(2) VALUE SPACES.
           02 FILLER PIC X(22) VALUE "Total No. of Records:".
           02 P-REC PIC X(3).
           02 FILLER PIC X(58) VALUE SPACES.

       01 TOTAL-BAL.
           02 FILLER PIC X(2) VALUE SPACES.
           02 FILLER PIC X(26) VALUE "Total Accumulated Balance:".
           02 FILLER PIC X(3) VALUE " P ".
           02 P-GRAND PIC X(18).
           02 FILLER PIC X(29) VALUE SPACES.

       01 BLANK-LINE.
           02 FILLER PIC X(80) VALUE SPACES.

       SCREEN SECTION.
       01 SCRE.
           02 BLANK SCREEN.

       PROCEDURE DIVISION.

       MAIN-RTN.
           PERFORM INIT-RTN THRU INIT-RTN-END.
           PERFORM PROCESS-RTN UNTIL EOFSW = 1.
           PERFORM FINISH-RTN.
           STOP RUN.

       INIT-RTN.
           OPEN INPUT INFILE, OUTPUT OUTFILE.

           READ INFILE
               AT END MOVE 1 TO EOFSW
           END-READ.

           IF EOFSW = 1
               DISPLAY "EMPTY FILE"
               GO TO INIT-RTN-END
           END-IF

           MOVE ANO TO HOLD-ACC-NO.
           MOVE ANA TO HOLD-ACC-NAME.

           PERFORM HEADING-RTN.
       INIT-RTN-END.

       HEADING-RTN.
           WRITE OUTREC FROM HEAD-1 AFTER PAGE.
           WRITE OUTREC FROM HEAD-2 AFTER 1.
           WRITE OUTREC FROM HEAD-3 AFTER 1.
           WRITE OUTREC FROM SUBHEAD-1 AFTER 3.
           WRITE OUTREC FROM SUBHEAD-2 AFTER 2.
           WRITE OUTREC FROM SUBHEAD-3 AFTER 1.

           MOVE 0 TO PAGE-COUNT.

       PROCESS-RTN.
           DISPLAY SCRE.

           IF ANO NOT = HOLD-ACC-NO
               PERFORM BREAK-RTN
           END-IF

           IF TC = "W"
               PERFORM WITHDRAW-RTN
           END-IF

           IF TC = "D" AND INVALID-FLAG = 0
               COMPUTE WS-BALANCE = WS-BALANCE + TAMT
               ADD 1 TO PAGE-COUNT
               IF PAGE-COUNT > 10
                   PERFORM HEADING-RTN
               END-IF

               MOVE HOLD-ACC-NO   TO P-ANO
               MOVE HOLD-ACC-NAME TO P-ANA
               MOVE WS-BALANCE    TO WS-FORMATTED
               MOVE WS-FORMATTED  TO P-BAL

               WRITE OUTREC FROM DETALYE AFTER 1
           END-IF

           ADD 1 TO REC-COUNT

           READ INFILE
               AT END
                   MOVE 1 TO EOFSW
                   PERFORM BREAK-RTN
           END-READ.

       WITHDRAW-RTN.
           IF TAMT > WS-BALANCE
               MOVE HOLD-ACC-NO   TO P-ANO
               MOVE HOLD-ACC-NAME TO P-ANA
               MOVE "INVALID WITHDRAWAL" TO P-BAL
               WRITE OUTREC FROM DETALYE AFTER 1
               MOVE 1 TO INVALID-FLAG
           ELSE
               COMPUTE WS-BALANCE = WS-BALANCE - TAMT
           END-IF.

       BREAK-RTN.
           IF INVALID-FLAG = 1
               MOVE 0 TO WS-BALANCE
               MOVE 0 TO INVALID-FLAG
               MOVE ANO TO HOLD-ACC-NO
               MOVE ANA TO HOLD-ACC-NAME

           END-IF

           IF PAGE-COUNT = 10
               PERFORM HEADING-RTN
           END-IF

           MOVE HOLD-ACC-NO   TO P-ANO
           MOVE HOLD-ACC-NAME TO P-ANA
           MOVE WS-BALANCE    TO WS-FORMATTED
           MOVE WS-FORMATTED  TO P-BAL

           WRITE OUTREC FROM DETALYE AFTER 1

           ADD WS-BALANCE TO GRAND-BALANCE
           MOVE 0 TO WS-BALANCE
           ADD 1 TO PAGE-COUNT

           MOVE ANO TO HOLD-ACC-NO
           MOVE ANA TO HOLD-ACC-NAME.

       FINISH-RTN.
           MOVE REC-COUNT     TO P-REC.
           MOVE GRAND-BALANCE TO WS-FORMATTED.
           MOVE WS-FORMATTED TO P-GRAND.

           WRITE OUTREC FROM BLANK-LINE AFTER 2.
           WRITE OUTREC FROM TOTAL-REC AFTER 1.
           WRITE OUTREC FROM TOTAL-BAL AFTER 1.

           CLOSE INFILE, OUTFILE.

           DISPLAY "FINISH NA MI".
