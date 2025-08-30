      ******************************************************************
      * Author: Migel Tan
      * Date: 8/29/25
      * Purpose: Appending to File
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
       01 WSCustomerData.
           *>Record- collection of field for an object.
           02 WSIDNum PIC 9(5).
           02 WSCustName.
               *>Field- indiv information
               03 WSFirstName PIC X(15).
               03 WSLastName PIC X(15).


       PROCEDURE DIVISION.
       *>Append is EXTEND
       OPEN EXTEND CustomerFile.
           DISPLAY "Customer ID: "
           ACCEPT IDNum
           DISPLAY "Customer First Name: "
           ACCEPT FirstName
           DISPLAY "Customer Last Name: "
           ACCEPT LastName

           WRITE CustomerData
           END-WRITE.
       CLOSE CustomerFile.

           STOP RUN.
