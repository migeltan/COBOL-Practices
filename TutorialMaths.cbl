       IDENTIFICATION DIVISION.
       PROGRAM-ID. Tutorial3.
       *>AUTHOR Migel Tan.
       *>DATE-WRITTEN. August 26 2025

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 SampleData PIC X(10) VALUE "Stuff".
       01 JustLetters PIC AAA VALUE "ABC".
       01 JustNums PIC 9(4) VALUE 1234.
       01 SignedInt PIC S9(4) VALUE -1234.

       *>DECIMALS
       01 PayCheck PIC 9(4)V99 VALUE ZEROS.

       *>GROUP ITEMS
       01 Customer.
           02 Ident PIC 9(3).
           02 CUstName PIC X(20).
           02 DateofBirth.
               03 MOB PIC 99.
               03 DOB PIC 99.
               03 YOB PIC 9(4).
       01 Num1 PIC 9 VALUE 5.
       01 Num2 PIC 9 VALUE 4.
       01 Num3 PIC 9 VALUE 3.
       01 Ans PIC S99V99 VALUE 0.
       01 Rem PIC SV99.

       PROCEDURE DIVISION.
       *>MATH IN COBOL based on Tutorial3
       *>Remember, COBOL is close to human language.
       *>ADD - TO
       *>SUBTRACT - FROM
       *>MULTIPLY - BY
       *>DIVISION - INTO, and REMAINDER

       ADD Num1, Num2 TO Num3 GIVING Ans
       ADD Num1, Num2, Num3 GIVING Ans
       DISPLAY Ans

       *>You could also do: COMPUTE
       COMPUTE Ans = Num1 + Num2
       COMPUTE Ans = Num1 - Num2
       COMPUTE Ans = Num1 * Num2
       COMPUTE Ans = Num1 / Num2
       DISPLAY Ans

       COMPUTE Ans = Num1 ** 2
       DISPLAY Ans
       COMPUTE Ans = (3 + 5) * 5
       DISPLAY Ans
       COMPUTE Ans= 3 + 5 * 5
       DISPLAY Ans
       COMPUTE Ans ROUNDED = 3.0 + 2.005

       STOP RUN.
