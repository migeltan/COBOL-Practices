      ******************************************************************
      * Author: Migel Tan
      * Date: August 27, 2025
      * Purpose: A tutorial for SubRoutines/Linkages
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Tutorial6SubRoutines.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           01 Num1 PIC 9 VALUE 5.
           01 Num2 PIC 9 VALUE 4.
           01 Sum1 PIC 99.

       PROCEDURE DIVISION.
       CALL 'Linkage' USING Num1, Num2, Sum1.
       DISPLAY Num1 " + " Num2 " = " Sum1.

            STOP RUN.
       END PROGRAM Tutorial6SubRoutines.
