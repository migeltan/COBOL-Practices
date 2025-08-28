      ******************************************************************
      * Author: Migel Tan
      * Date: 8/28/25
      * Purpose: Fixed point decimal arithmetic, rounding values
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Tutorial9.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 Price PIC 9(4)V99.
       01 TaxRate PIC V999 VALUE .075.
       01 FullPrice PIC 9(4)V99.


       PROCEDURE DIVISION.
       DISPLAY "Enter a price: "
       ACCEPT Price

       COMPUTE FullPrice ROUNDED = Price + (Price * TaxRate)
       DISPLAY "Price + Tax: " FullPrice.
            STOP RUN.
