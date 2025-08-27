
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Tutorial4Cond.
       *>AUTHOR. Migel Tan
       *>DATE-WRITTEN. August 27, 2025

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CLASS PassingScore IS "A" THRU "C", "D".

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 Age PIC 99 VALUE 0.
       01 Grade PIC 99 VALUE 0.
       01 Score PIC X(1) VALUE "B".
       01 CanVoteFlag PIC 9 VALUE 0.
           88 CanVote VALUE 1.
           88 CantVote VALUE 0.

       01 TestNumber PIC X.
           88 IsPrime VALUE "1", "3", "5", "7".
           88 IsOdd VALUE "1", "3", "5", "7".
           88 IsEven VALUE "2", "4", "6", "8".
           88 Lessthan5 VALUE "1" THRU "4".
           88 ANumber VALUE "0" THRU "9".

       PROCEDURE DIVISION.

       *> ELSE-IF STATEMENTS:
       DISPLAY "Enter Age: "
       ACCEPT Age
       IF Age > 18 THEN
           DISPLAY "You can vote."
       ELSE
           DISPLAY "You can't vote."
       END-IF

       *> > GREATER THAN, < LESS THAN, EQUAL TO, ETC.
       *> Remember, symbols can be written as is.
       *> Below is an if-else condition where we validate if the age
       *> is legal for voting, as well as seeing what grade they should be.

       IF Age LESS THAN 6 THEN
           DISPLAY "Stay home kid!."
       END-IF

       IF AGE = 6 THEN
           DISPLAY "Go to Kindergarten."
       END-IF

       IF Age > 6 AND Age < 18 THEN
           COMPUTE Grade = Age - 6
           DISPLAY "Go to Grade " Grade
       END-IF

       IF Age GREATER THAN OR EQUAL TO 18
           DISPLAY "Go to college!"
       END-IF

       *> Score (WSS) and Class (SN), here is the function
       IF Score IS PassingScore THEN
           DISPLAY "You Passed!"
       ELSE
           DISPLAY "You Failed!"
       END-IF

       *> NUMERIC ALPHABETIC, ALPHABETIC-UPPER
       IF Score IS NOT NUMERIC THEN
           DISPLAY "Not a Number!"
       END-IF

       *>Toggle Value
       IF Age GREATER THAN 18 THEN
           SET CanVote TO TRUE
       ELSE
           SET CantVote TO TRUE
       END-IF
       DISPLAY "Vote " CanVoteFlag

       *>Evaluating a value, test number in wss
       DISPLAY "Enter a Single Numer or X to Exit: "
       ACCEPT TestNumber

       *>Loop example: Accepts a number until a non-integer.
       PERFORM UNTIL NOT ANumber
           EVALUATE TRUE
               WHEN IsPrime DISPLAY "Prime"
               WHEN IsOdd DISPLAY "Odd"
               WHEN IsEven DISPLAY "Even"
               WHEN LessThan5 DISPLAY "Less than 5"
               WHEN OTHER DISPLAY "Default Action"
           END-EVALUATE
           ACCEPT TestNumber
       END-PERFORM

            STOP RUN.

       END PROGRAM Tutorial4Cond.
