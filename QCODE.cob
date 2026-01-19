       IDENTIFICATION DIVISION.
       PROGRAM-ID. QCODE.
      *AUTHOR. MIGEL H. TAN.
      *INSTALLATION. VALENZUELA CITY
      *DATE-WRITTEN. DECEMBER 5, 2025.
      *DATE-COMPILED. DECEMBER 5, 2025.
      *SECURITY. FOR BSIT 2-4.
      *REMARKS. STUDENT QUIZ REPORT- multiple records per student.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT ASSIGN TO "QCIN.TXT".
           SELECT OUTFILE ASSIGN TO "QCOUT".

       DATA DIVISION.
       FILE SECTION.

       FD STUDENT
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 34 CHARACTERS
           DATA RECORD IS INREC.

       01 INREC.
           02 SNO-IN PIC 9(5).
           02 SNA-IN PIC X(25).
           02 SCORECODE-IN PIC 9(4).

       FD OUTFILE
           LABEL RECORD IS OMITTED
           DATA RECORD IS OUTREC.

       01 OUTREC.
           02 OUT-LINE PIC X(80).

       WORKING-STORAGE SECTION.

       01 EOFSW PIC 9 VALUE 0.
       01 PAGE-COUNT PIC 9(2) VALUE 0.
       01 REC-COUNT PIC 9(3) VALUE 0.
       01 WS-TOTAL PIC 9(4) VALUE 0.
       01 WS-AVE PIC 9(3)V99.
       01 QUIZ-COUNT PIC 9(1) VALUE 0.
       01 J PIC 9 VALUE 1.

       01 SCORE-CODE.
           02 SC-SCORE PIC 9(3).
           02 SC-CODE PIC 9.

       01 WS-TEMP PIC 9(4).

       01 QUIZ-TBL.
           02 QUIZ-ELEM OCCURS 5 TIMES PIC 9(3) VALUE 0.

       01 PRT-TABLE.
           02 P-SNO PIC 9(5).
           02 P-SNA PIC X(25).
           02 PRT-Q OCCURS 5 TIMES.
              03 PRT-QUIZ PIC 9(3).
           02 P-AVE PIC ZZ9.99.

       01 HEAD-1.
           02 FILLER PIC X(39) VALUE SPACES.
           02 H1 PIC X(3) VALUE "PUP".
           02 FILLER PIC X(26) VALUE SPACES.

       01 HEAD-2.
           02 FILLER PIC X(19)  VALUE SPACES.
           02 H2 PIC X(23) VALUE "College of Computer and".
           02 H22 PIC X(21) VALUE " Information Sciences".
           02 FILLER PIC X(7)  VALUE SPACES.

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
           02 FILLER PIC X(4) VALUE SPACES.
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
           02 FILLER PIC X(5) VALUE SPACES.
           02 FILLER PIC X(2) VALUE "5".
           02 FILLER PIC X(25) VALUE SPACES.

       01 DETALYE.
           02 FILLER PIC X(2) VALUE SPACES.
           02 P-SNO-OUT PIC 9(5).
           02 FILLER PIC X(6) VALUE SPACES.
           02 P-SNA-OUT PIC X(24).
           02 PRT-QX OCCURS 5 TIMES.
              03 PRT-QV PIC 9(3).
              03 FILLER  PIC X(4) VALUE SPACES.
      *    02 FILLER PIC X(4) VALUE SPACES.
           02 P-AVE-OUT PIC ZZ9.99.
           02 FILLER PIC X(15) VALUE SPACES.

       01 BLANK-LINE.
           02 FILLER PIC X(80) VALUE SPACES.

       01 TOTAL-LINE.
           02 FILL1 PIC X(2) VALUE SPACES.
           02 FILL2 PIC X(22) VALUE "Total no. of Records. ".
           02 P-TOT PIC 9(3).
           02 FILL3 PIC X(53) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-RTN.
           PERFORM INIT-RTN.
           IF EOFSW = 0
              PERFORM PROCESS-FIRST-RECORD
              PERFORM UNTIL EOFSW = 1
                 PERFORM PROCESS-LOOP
              END-PERFORM
              PERFORM PRINT-CURRENT-STUDENT
           END-IF
           PERFORM FINISH-RTN.
           STOP RUN.

       INIT-RTN.
           OPEN INPUT STUDENT OUTPUT OUTFILE.
           MOVE 0 TO EOFSW.
           MOVE 0 TO REC-COUNT.
           MOVE 0 TO PAGE-COUNT.

           READ STUDENT
               AT END MOVE 1 TO EOFSW
           END-READ.

           IF EOFSW = 1
               DISPLAY "WALA FILE DETECTED."
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

       PROCESS-FIRST-RECORD.
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 5
               MOVE 0 TO QUIZ-ELEM (J)
           END-PERFORM
           MOVE 0 TO WS-TOTAL
           MOVE 0 TO QUIZ-COUNT
           MOVE SNO-IN TO P-SNO-OUT
           MOVE SNA-IN TO P-SNA-OUT

           MOVE SCORECODE-IN TO WS-TEMP
           COMPUTE SC-SCORE = WS-TEMP / 10
           DIVIDE SC-SCORE BY 1 GIVING SC-SCORE
           COMPUTE SC-CODE = WS-TEMP - (SC-SCORE * 10)

           PERFORM STORE-QUIZ-FROM-CURRENT-REC

           READ STUDENT
               AT END MOVE 1 TO EOFSW
           END-READ.
           EXIT.

       PROCESS-LOOP.
           IF EOFSW = 1
               EXIT
           END-IF

           IF SNO-IN = P-SNO-OUT
               MOVE SCORECODE-IN TO WS-TEMP
               COMPUTE SC-SCORE = WS-TEMP / 10
               DIVIDE SC-SCORE BY 1 GIVING SC-SCORE
               COMPUTE SC-CODE = WS-TEMP - (SC-SCORE * 10)

               PERFORM STORE-QUIZ-FROM-CURRENT-REC
           ELSE
               PERFORM PRINT-CURRENT-STUDENT
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 5
                   MOVE 0 TO QUIZ-ELEM (J)
               END-PERFORM
               MOVE 0 TO WS-TOTAL
               MOVE 0 TO QUIZ-COUNT
               MOVE SNO-IN TO P-SNO-OUT
               MOVE SNA-IN TO P-SNA-OUT

               MOVE SCORECODE-IN TO WS-TEMP
               COMPUTE SC-SCORE = WS-TEMP / 10
               DIVIDE SC-SCORE BY 1 GIVING SC-SCORE
               COMPUTE SC-CODE = WS-TEMP - (SC-SCORE * 10)

               PERFORM STORE-QUIZ-FROM-CURRENT-REC
           END-IF

           READ STUDENT
               AT END MOVE 1 TO EOFSW
           END-READ.
           IF PAGE-COUNT = 10
               PERFORM HEADING-RTN
           END-IF.
           EXIT.

       STORE-QUIZ-FROM-CURRENT-REC.
           IF SC-CODE >= 1 AND SC-CODE <= 5
               IF QUIZ-ELEM (SC-CODE) = 0
                   ADD 1 TO QUIZ-COUNT
                   ADD SC-SCORE TO WS-TOTAL
                   MOVE SC-SCORE TO QUIZ-ELEM (SC-CODE)
               ELSE
                   SUBTRACT QUIZ-ELEM (SC-CODE) FROM WS-TOTAL
                   ADD SC-SCORE TO WS-TOTAL
                   MOVE SC-SCORE TO QUIZ-ELEM (SC-CODE)
               END-IF
           END-IF.
           EXIT.

       PRINT-CURRENT-STUDENT.
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 5
               MOVE QUIZ-ELEM (J) TO PRT-QV (J)
           END-PERFORM
           IF QUIZ-COUNT > 0
               COMPUTE WS-AVE = WS-TOTAL / 5
           ELSE
               MOVE 0 TO WS-AVE
           END-IF
           MOVE WS-AVE TO P-AVE-OUT
           MOVE P-SNO-OUT TO P-SNO
           MOVE P-SNA-OUT TO P-SNA

           WRITE OUTREC FROM DETALYE AFTER 1.

           ADD 1 TO REC-COUNT.
           ADD 1 TO PAGE-COUNT.

           EXIT.

       FINISH-RTN.
           WRITE OUTREC FROM BLANK-LINE AFTER 2.
           MOVE REC-COUNT TO P-TOT.
           WRITE OUTREC FROM TOTAL-LINE AFTER 1.

           CLOSE STUDENT OUTFILE.
           DISPLAY "FINISH NA MI.".
           EXIT.
