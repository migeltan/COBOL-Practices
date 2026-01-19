       IDENTIFICATION DIVISION.
       PROGRAM-ID. STUDREP.

      *AUTHOR. MIGEL H. TAN.
      *REMARKS. STUDENT QUIZ REPORT WITH TABLE HANDLING.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT ASSIGN TO "STUDQ.TXT".
           SELECT OUTFILE ASSIGN TO "REPQ".

       DATA DIVISION.
       FILE SECTION.

       FD STUDENT
           LABEL RECORD IS STANDARD
           DATA RECORD IS INREC.

       01 INREC.
           02 SNO PIC 9(5).
           02 SNA PIC X(25).
           02 QUIZ OCCURS 5 TIMES PIC 99.

       FD OUTFILE
           LABEL RECORD IS OMITTED
           DATA RECORD IS OUTREC.

       01 OUTREC.
           02 FILLER PIC X(80).

       WORKING-STORAGE SECTION.

       01 EOFSW       PIC 9 VALUE 0.
       01 PAGE-COUNT  PIC 9(2) VALUE 0.
       01 REC-COUNT   PIC 9(3) VALUE 0.

       01 WS-TOTAL    PIC 9(3) VALUE 0.
       01 WS-AVE      PIC 99V99.
       01 I           PIC 9 VALUE 1.

       01 PRT-TABLE.
           02 PRT-SNO PIC 9(5).
           02 PRT-SNA PIC X(25).
           02 PRT-Q OCCURS 5 TIMES.
              03 PRT-QUIZ PIC ZZ.
           02 PRT-AVE PIC 99.99.

       01 HEAD-1.
           02 FILLER PIC X(36) VALUE SPACES.
           02 FILLER PIC X(10) VALUE "puP".
           02 FILLER PIC X(35) VALUE SPACES.

       01 HEAD-2.
           02 FILLER PIC X(35) VALUE SPACES.
           02 FILLER PIC X(12) VALUE "ccis".
           02 FILLER PIC X(30) VALUE SPACES.

       01 SUBHEAD-1.
           02 FILLER PIC X(2) VALUE SPACES.
           02 FILLER PIC X(7) VALUE "Student".
           02 FILLER PIC X(4) VALUE SPACES.
           02 FILLER PIC X(7) VALUE "Student".
           02 FILLER PIC X(17) VALUE SPACES.
           02 FILLER PIC X(4) VALUE "Quiz".
           02 FILLER PIC X(3) VALUE SPACES.
           02 FILLER PIC X(4) VALUE "Quiz".
           02 FILLER PIC X(3) VALUE SPACES.
           02 FILLER PIC X(4) VALUE "Quiz".
           02 FILLER PIC X(3) VALUE SPACES.
           02 FILLER PIC X(4) VALUE "Quiz".
           02 FILLER PIC X(3) VALUE SPACES.
           02 FILLER PIC X(4) VALUE "Quiz".
           02 FILLER PIC X(3) VALUE SPACES.
           02 FILLER PIC X(8) VALUE "Average".
           02 FILLER PIC X(17) VALUE SPACES.

       01 SUBHEAD-2.
           02 FILLER PIC X(2) VALUE SPACES.
           02 FILLER PIC X(6) VALUE "Number".
           02 FILLER PIC X(5) VALUE SPACES.
           02 FILLER PIC X(4) VALUE "Name".
           02 FILLER PIC X(21) VALUE SPACES.
           02 FILLER PIC X(2) VALUE "1".
           02 FILLER PIC X(5) VALUE SPACES.
           02 FILLER PIC X(2) VALUE "2".
           02 FILLER PIC X(5) VALUE SPACES.
           02 FILLER PIC X(2) VALUE "3".
           02 FILLER PIC X(5) VALUE SPACES.
           02 FILLER PIC X(2) VALUE "4".
           02 FILLER PIC X(6) VALUE SPACES.
           02 FILLER PIC X(2) VALUE "5".
           02 FILLER PIC X(25) VALUE SPACES.

       01 DETALYE.
           02 FILLER PIC X(2) VALUE SPACES.
           02 P-SNO PIC 9(5).
           02 FILLER PIC X(6) VALUE SPACES.
           02 P-SNA PIC X(25).
           02 PRT-QX OCCURS 5 TIMES.
              03 PRT-QV PIC ZZ.
              03 FILLER PIC X(5).
           02 P-AVE PIC 99.99.
           02 FILLER PIC X(15) VALUE SPACES.

      *01 TOTAL-LINE.
      *    02 FILLER PIC X(2) VALUE SPACES.
      *    02 FILLER PIC X(22) VALUE "Total Students Read :".
      *    02 P-TOT PIC X(3).
      *    02 FILLER PIC X(54) VALUE SPACES.

       01 BLANK-LINE.
           02 FILLER PIC X(80) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-RTN.
           PERFORM INIT-RTN.
           PERFORM PROCESS-RTN UNTIL EOFSW = 1.
           PERFORM FINISH-RTN.
           STOP RUN.

       INIT-RTN.
           OPEN INPUT STUDENT OUTPUT OUTFILE.

           READ STUDENT
               AT END MOVE 1 TO EOFSW
           END-READ.

           IF EOFSW = 1
               DISPLAY "EMPTY FILE"
               GO TO INIT-DONE
           END-IF

           PERFORM HEADING-RTN.

       INIT-DONE.
           EXIT.

       HEADING-RTN.
           WRITE OUTREC FROM HEAD-1 AFTER PAGE.
           WRITE OUTREC FROM HEAD-2 AFTER 1.
           WRITE OUTREC FROM SUBHEAD-1 AFTER 2.
           WRITE OUTREC FROM SUBHEAD-2 AFTER 1.
           MOVE 0 TO PAGE-COUNT.
           EXIT.

       PROCESS-RTN.
           MOVE SNO TO P-SNO.
           MOVE SNA TO P-SNA.

           MOVE 0 TO WS-TOTAL.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
           MOVE QUIZ(I) TO PRT-QV(I)
           ADD QUIZ(I) TO WS-TOTAL
           END-PERFORM


           COMPUTE WS-AVE = WS-TOTAL / 5.
           MOVE WS-AVE TO P-AVE.

           WRITE OUTREC FROM DETALYE AFTER 1.

           ADD 1 TO REC-COUNT.
           ADD 1 TO PAGE-COUNT.

           READ STUDENT
               AT END MOVE 1 TO EOFSW
           END-READ.

           IF PAGE-COUNT = 10
               PERFORM HEADING-RTN
           END-IF.
           EXIT.

       FINISH-RTN.
      *    MOVE REC-COUNT TO P-TOT.

      *    WRITE OUTREC FROM BLANK-LINE AFTER 2.
      *    WRITE OUTREC FROM TOTAL-LINE AFTER 1.

           CLOSE STUDENT OUTFILE.
           DISPLAY "REPORT COMPLETE.".
           EXIT.
