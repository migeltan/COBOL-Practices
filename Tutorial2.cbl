       IDENTIFICATION DIVISION.
       PROGRAM-ID. Tutorial2.
       *> Author: Migel Tan
       *> Date-Written: August 26, 2025

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 UserName PIC X(30).
       01 Num1 PIC 9 VALUE ZEROS.
       01 Num2 PIC 9 VALUE ZEROS.
       01 Total PIC 99 VALUE 0.
       01 SSNum. *>SS is subsection
           02 SSArea PIC 999.
           02 SSGroup PIC 99.
           02 SSSerial PIC 9999.

       01 PIValue CONSTANT AS 3.14.

       PROCEDURE DIVISION.
       *>Its like printf and scanf
           DISPLAY "What is your name? "
           ACCEPT UserName.
           DISPLAY "Hello, " UserName.
           DISPLAY "PI Value is: " PIValue.

           MOVE ZERO TO UserName
           DISPLAY UserName
           DISPLAY "Enter 2 values to sum: "
           ACCEPT Num1
           ACCEPT Num2

           COMPUTE Total = Num1 + Num2
           DISPLAY Num1 " + " Num2 " = " Total

           DISPLAY "Enter your social security number: "
           ACCEPT SSNum
           DISPLAY "Area " SSArea


           STOP RUN.
