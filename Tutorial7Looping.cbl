
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Tutorial7Looping.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       *>Creating an INDEX
       01 Ind PIC 9(1) VALUE 0.

       PROCEDURE DIVISION.
       *>Executes until condition is met.
       *>WhileLoop
       PERFORM OutputData WITH TEST AFTER UNTIL Ind > 5
           GO TO ForLoop.

        *>Displays current value, then increments
       OutputData.
           DISPLAY Ind.
           ADD 1 TO Ind.

       *> Paragraph that is forloop, performs to another parag
       ForLoop.
           PERFORM OutputData2 VARYING Ind FROM 1 BY 1 UNTIL Ind=5

           STOP RUN.

       OutputData2.
           DISPLAY Ind.

       END PROGRAM Tutorial7Looping.
