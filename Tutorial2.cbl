       IDENTIFICATION DIVISION.
       PROGRAM-ID. Tutorial2.
       *> Author: Migel Tan
       *> Date-Written: August 26, 2025

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 UserName PIC X(30).
       *>PIC X(SOME NUMBER) means alphanumeric data which holds digits, letters, symbol
       *>PIC X() is just char UserName[30]; in this case.

       01 Num1 PIC 9 VALUE ZEROS.
       *>Means single digits. 9 will always be the limit, but if it's 99 it will be tenth digit
       *>And so on, but since it has VALUE ZEROS, it will have 0 as default rather than none.
       *>Instead of 123, it will be: 000000123

       01 Num2 PIC 9 VALUE ZEROS.
       01 Total PIC 99 VALUE 0.
       01 SSNum. *>SS is subsection
           02 SSArea PIC 999.
           02 SSGroup PIC 99.
           02 SSSerial PIC 9999.

       01 PIValue CONSTANT AS 3.14.
       *>You can also do CONSTANT values.

       PROCEDURE DIVISION.
       *>Its like printf and scanf, DISPLAY being the print, and ACCEPT being scanf
           DISPLAY "What is your name? "
           ACCEPT UserName.
           DISPLAY "Hello, " UserName.
           DISPLAY "PI Value is: " PIValue.

           MOVE ZERO TO UserName
           DISPLAY UserName
           DISPLAY "Enter 2 values to sum: "
           ACCEPT Num1
           ACCEPT Num2

              *>COMPUTE would be allowing a variable in storing a formula for something.
              *>In this case, the sum of Num1 and Num2 will be stored in Total.
           COMPUTE Total = Num1 + Num2
           DISPLAY Num1 " + " Num2 " = " Total

           DISPLAY "Enter your social security number: "
           ACCEPT SSNum
           DISPLAY "Area " SSArea


           STOP RUN.

