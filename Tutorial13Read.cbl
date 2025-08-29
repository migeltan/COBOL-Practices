      ******************************************************************
      * Author: Migel Tan
      * Date: 8/29/25
      * Purpose: Reading to File
      * Tutorial for File handling
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Tutorial11FileHandling.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CustomerFile ASSIGN TO "Customer.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CustomerFile.
       01 CustomerData.
           02 IDNum PIC 9(5).
           02 CustName.
               03 FirstName PIC X(15).
               03 LastName PIC X(15).
       WORKING-STORAGE SECTION.
       *>Object
       01 WSCustomer.
           *>Record- collection of field for an object.
           02 WSIDNum PIC 9(5).
           02 WSCustName.
               *>Field- indiv information
               03 WSFirstName PIC X(15).
               03 WSLastName PIC X(15).
       *>For the end of file
       01 WSEOF PIC A(1).

       PROCEDURE DIVISION.
       *>READ is INPUT
       OPEN INPUT CustomerFile.
           PERFORM UNTIL WSEOF = 'Y'
               READ CustomerFile INTO WSCustomer
                   AT END MOVE 'Y' TO WSEOF
                   NOT AT END DISPLAY WSCustomer
               END-READ
           END-PERFORM
       CLOSE CustomerFile.

           STOP RUN.
