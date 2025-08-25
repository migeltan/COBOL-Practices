
       IDENTIFICATION DIVISION.
       *> Program metadata, like name, author, etc.
       PROGRAM-ID. YOUR-PROGRAM-NAME.

       ENVIRONMENT DIVISION.
       *>Similar to #include or file handling setup
       *> Often skipped in simple COBOL programs

       DATA DIVISION.
       *> Where you declare variables and constants
       *> Kinda like declaring int, char[], or struct
       *> Parang struct talaga besh

       *>FILE SECTION.
       *>WORKING-STORAGE SECTION.

       PROCEDURE DIVISION.
       *> Main function of the program, equivalent to int main ()

       MAIN-PROCEDURE.
           *> Simple hello world.
            DISPLAY "Hello world"
            STOP RUN.
