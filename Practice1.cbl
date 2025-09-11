      ******************************************************************
      * Author: Migel H. Tan
      * Date: 9/10/2025
      * Purpose: Practice program.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Practice1.

       DATA DIVISION.

       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 A PIC 9(9).
       01 B PIC 9(9).
       01 C PIC 9(9).

       PROCEDURE DIVISION.
       DISPLAY "Enter two numbers: "
       DISPLAY "First Number: "
       ACCEPT A.
       DISPLAY "Second Number: "
       ACCEPT B.

       COMPUTE C = A + B.
       DISPLAY "The sum of " A " + " B " is " C.

            STOP RUN.
