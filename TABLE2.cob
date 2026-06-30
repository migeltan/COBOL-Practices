       IDENTIFICATION DIVISION.
       PROGRAM-ID. TABLE2.
      *AUTHOR. MIGEL H. TAN.
      *INSTALLATION. VALENZUELA CITY.
      *DATE-WRITTEN. DECEMBER 7, 2025.
      *DATE-COMPILED. DECEMBER 8, 2025.
      *SECURITY. BSIT 2-4.
      *REMARKS. 2-DIMENSIONAL TABLE HANDLING PAGE 83.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT ASSIGN TO "TABIN.TXT"
               ORGANIZATION IS SEQUENTIAL.
           SELECT OUTFILE ASSIGN TO "TABOUT"
               ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD STUDENT
           LABEL RECORD IS STANDARD
           DATA RECORD IS INREC.
       01 INREC.
           05 F-COURSE PIC X(4).
           05 F-YEAR   PIC 9.
           05 F-COUNT  PIC 9(3).

       FD OUTFILE
           LABEL RECORD IS OMITTED
           DATA RECORD IS OUTREC.
       01 OUTREC PIC X(80).

       WORKING-STORAGE SECTION.

       01 EOFSW PIC 9 VALUE 0.
       01 COURSE-TABLE.
           05 COURSE OCCURS 2 TIMES.
               10 YR OCCURS 4 TIMES PIC 9(4).

       01 I PIC 9.
       01 J PIC 9.
       01 TEMP PIC 9(5).

       01 OUT-WS.
           05 OUT-YEAR  PIC X(10).
           05 OUT-BSIT  PIC X(5).
           05 OUT-BSCS  PIC X(5).
           05 OUT-TOTAL PIC X(5).

       01 WS-NUM PIC 9(5).
       01 WS-STR PIC X(5).

       PROCEDURE DIVISION.

       MAIN-RTN.
           OPEN INPUT STUDENT
                OUTPUT OUTFILE

           PERFORM READ-RTN

           PERFORM UNTIL EOFSW = 1
               PERFORM STORE-RTN
               PERFORM READ-RTN
           END-PERFORM

           PERFORM PRINT-RTN

           CLOSE STUDENT OUTFILE
           STOP RUN.

       READ-RTN.
           READ STUDENT
               AT END MOVE 1 TO EOFSW
           END-READ.

       STORE-RTN.
           IF F-COURSE = "BSIT"
               MOVE 1 TO I
           ELSE
               MOVE 2 TO I
           END-IF

           MOVE F-COUNT TO YR(I, F-YEAR).

       PRINT-RTN.
           MOVE "YEAR        BSIT       BSCS       TOTAL" TO OUTREC
           WRITE OUTREC

           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 4
               MOVE SPACES TO OUTREC

               EVALUATE J
                   WHEN 1 MOVE "FRESHMAN " TO OUT-YEAR
                   WHEN 2 MOVE "SOPHOMORE" TO OUT-YEAR
                   WHEN 3 MOVE "JUNIOR   " TO OUT-YEAR
                   WHEN 4 MOVE "SENIOR   " TO OUT-YEAR
               END-EVALUATE

               MOVE YR(1, J) TO WS-NUM
               MOVE WS-NUM TO WS-STR
               MOVE WS-STR TO OUT-BSIT

               MOVE YR(2, J) TO WS-NUM
               MOVE WS-NUM TO WS-STR
               MOVE WS-STR TO OUT-BSCS

               COMPUTE TEMP = YR(1, J) + YR(2, J)
               MOVE TEMP TO WS-NUM
               MOVE WS-NUM TO WS-STR
               MOVE WS-STR TO OUT-TOTAL

               STRING OUT-YEAR DELIMITED BY SIZE
                      "  " DELIMITED BY SIZE
                      OUT-BSIT DELIMITED BY SIZE
                      "      " DELIMITED BY SIZE
                      OUT-BSCS DELIMITED BY SIZE
                      "      " DELIMITED BY SIZE
                      OUT-TOTAL DELIMITED BY SIZE
                      INTO OUTREC
               END-STRING

               WRITE OUTREC
           END-PERFORM.
