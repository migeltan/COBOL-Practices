       IDENTIFICATION DIVISION.
       PROGRAM-ID. FINALS-PRACTICE.
      *AUTHOR. MIGEL H. TAN.
      *INSTALLATION. VALENZUELA CITY.
      *DATE-WRITTEN. JANUARY 17, 2026.
      *DATE-COMPILED. JANUARY 17, 2026.
      *SECURITY. FOR BSIT 2-4 AND DR. FABREGAS.
      *REMARKS. FINALS PRACTICE FOR COBOL.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTFILE ASSIGN TO "TAN.TXT".
       
       DATA DIVISION.
       FILE SECTION.
       FD OUTFILE.

       01 PRINT-REC.
           05 SENUM PIC 9(10).
           05 SENAM PIC X(25).
           05 SDOB PIC X(20).
      *    05 SUNICODE PIC 9.
           05 SUNINAME PIC X(5).
      *    05 SCC PIC 9.
           05 SCNAME PIC X(4).
      *    05 STOTITEMS PIC 9(3).
      *    05 STESTR PIC 99.
           05 SREMARKS PIC X(6).
      *    05 STOTPASSED PIC 99.
      *    05 STOTFAILED PIC 99.

       01 DATABASE.
           02 FILLER PIC X(80).
       
       WORKING-STORAGE SECTION.
       01 ENO PIC 9(10) VALUE ZERO.
       01 ENAM PIC X(25) VALUE SPACES.
       01 DOB PIC X(20) VALUE SPACES.
       01 UNC PIC 9 VALUE ZERO.
       01 UNINAME PIC X(5) VALUE SPACES.
       01 CC PIC 9 VALUE ZERO.
       01 CNAME PIC X(4) VALUE SPACES.
       01 TOTITEMS PIC 9(3) VALUE ZERO.
       01 TESTR PIC 99 VALUE ZERO.
       01 REMARKSS PIC X(6).
       01 EOFSW PIC 9 VALUE ZERO.
       01 ANOTHER PIC X VALUE SPACES.
       01 TOTPASSED PIC 99 VALUE ZERO.
       01 TOTFAILED PIC 99 VALUE ZERO.

       SCREEN SECTION.
       01 SCRE.
           02 BLANK SCREEN.

       PROCEDURE DIVISION.
       MAIN-RTN.
           OPEN EXTEND OUTFILE.
           PERFORM PROCESS-RECORDS UNTIL EOFSW = 1.
           CLOSE OUTFILE.
           STOP RUN.
       
       PROCESS-RECORDS.
           PERFORM DISPLAY-HEADER.
           PERFORM ACCEPT-RTN THRU ACCEPT-RTN-END.
           PERFORM UNI-RTN THRU UNI-RTN-END.
           PERFORM CC-RTN THRU CC-RTN-END.
           PERFORM TOTAL-RTN THRU TOTAL-RTN-END.
           PERFORM REMARKS-RTN THRU REMARKS-RTN-END.
           PERFORM WRITE-FILE.
           PERFORM CONTINUE-RTN.

       DISPLAY-HEADER.
           DISPLAY SCRE.
           DISPLAY ' ' LINE 1 COLUMN 40.
           DISPLAY 'Professional Regulation ' LINE 2 COLUMN 23. 
           DISPLAY 'Commission' LINE 2 COLUMN 47.
           DISPLAY ' ' LINE 3 COLUMN 40.
           DISPLAY ' ' LINE 4 COLUMN 40.
           DISPLAY 'IT Professional Board ' LINE 5 COLUMN 23.
           DISPLAY 'Exam Results' LINE 5 COLUMN 45.
           DISPLAY ' ' LINE 6 COLUMN 40.

       ACCEPT-RTN.
           DISPLAY 'Examinee Number: ' LINE 7 COLUMN 10.
           ACCEPT ENO LINE 7 COLUMN 55.

           DISPLAY 'Examinee Name: ' LINE 8 COLUMN 10.
           ACCEPT ENAM LINE 8 COLUMN 55.

           DISPLAY 'Date of Birth (M/D/Y): ' LINE 9 COLUMN 10.
           ACCEPT DOB LINE 9 COLUMN 55.
       
       UNI-RTN.
           DISPLAY 'University Code (1-5): ' LINE 10 COLUMN 10.
           ACCEPT UNC LINE 10 COLUMN 55.
           IF UNC = 1
           MOVE 1 TO UNC
           MOVE 'UP' TO UNINAME
       
           ELSE IF UNC = 2
           MOVE 2 TO UNC 
           MOVE 'PUP' TO UNINAME
           
           ELSE IF UNC = 3
           MOVE 3 TO UNC
           MOVE 'DLSU' TO UNINAME

           ELSE IF UNC = 4
           MOVE 4 TO UNC
           MOVE 'ADMU' TO UNINAME

           ELSE IF UNC = 5
           MOVE 5 TO UNC
           MOVE 'MAPUA' TO UNINAME

           ELSE IF UNC IS GREATER THAN 5 OR IF UNC IS LESS THAN 1 
           DISPLAY '1-5 only!' LINE 10 COLUMN 40
           GO TO UNI-RTN.
           
           DISPLAY 'University Name: ' LINE 11 COLUMN 10.
           DISPLAY UNINAME LINE 11 COLUMN 55.
          
       UNI-RTN-END.

       CC-RTN.
           DISPLAY 'Course Code (1-3): ' LINE 12 COLUMN 10.
           ACCEPT CC LINE 12 COLUMN 55.
           IF CC = 1
           MOVE 1 TO CC
           MOVE 'BSIT' TO CNAME

           ELSE IF CC = 2
           MOVE 2 TO CC
           MOVE 'BSCS' TO CNAME

           ELSE IF CC = 3
           MOVE 3 TO CC
           MOVE 'BSIS' TO CNAME

           ELSE IF CC IS GREATER THAN 3 OR IF CC IS LESS THAN 1
           DISPLAY '1-3 only!' LINE 12 COLUMN 40
           GO TO CC-RTN.
           
           DISPLAY 'Course Name: ' LINE 13 COLUMN 10.
           DISPLAY CNAME LINE 13 COLUMN 55.
       CC-RTN-END.

       TOTAL-RTN.
           DISPLAY 'Total No. of Items: ' LINE 14 COLUMN 10.
           ACCEPT TOTITEMS LINE 14 COLUMN 55.
           IF TOTITEMS IS GREATER THAN 100 OR
           IF TOTITEMS IS LESS THAN 0
           DISPLAY 'Must be 0-100!' LINE 14 COLUMN 40
           GO TO TOTAL-RTN.
       
       TEST-RTN.
           DISPLAY 'Test Result (Score): ' LINE 15 COLUMN 10.
           ACCEPT TESTR LINE 15 COLUMN 55.
           IF TESTR IS GREATER THAN TOTITEMS OR 
           IF TESTR IS LESS THAN ZERO
           DISPLAY 'Must be 0-100!' LINE 15 COLUMN 40
           GO TO TEST-RTN THRU TEST-RTN-END.
       TEST-RTN-END.
       
       REMARKS-RTN.
           IF CC = 1 AND TESTR IS GREATER THAN OR EQUAL 60
           MOVE 'PASSED' TO REMARKSS
           ADD 1 TO TOTPASSED
           DISPLAY 'Remarks: ' LINE 16 COLUMN 10.
           DISPLAY REMARKSS LINE 16 COLUMN 55.
           ELSE IF TESTR IS LESS THAN OR EQUAL TO 59
           MOVE 'FAILED' TO REMARKSS
           ADD 1 TO TOTFAILED
           DISPLAY 'Remarks: ' LINE 16 COLUMN 10.

           ELSE IF CC=2 AND TESTR IS GREATER THAN OR EQUAL 70
           MOVE 'PASSED' TO REMARKSS
           ADD 1 TO TOTPASSED
           DISPLAY 'Remarks: ' LINE 16 COLUMN 10.
           DISPLAY REMARKSS LINE 16 COLUMN 55.
           ELSE IF TESTR IS LESS THAN OR EQUAL TO 69
           MOVE 'FAILED' TO REMARKSS
           ADD 1 TO TOTFAILED
           DISPLAY 'Remarks: ' LINE 16 COLUMN 10.

           ELSE IF CC=3 AND TESTR IS GREATER THAN OR EQUAL 50
           MOVE 'PASSED' TO REMARKSS
           ADD 1 TO TOTPASSED
           DISPLAY 'Remarks: ' LINE 16 COLUMN 10.
           DISPLAY REMARKSS LINE 16 COLUMN 55.
           ELSE IF TESTR IS LESS THAN OR EQUAL TO 49
           MOVE 'FAILED' TO REMARKSS
           ADD 1 TO TOTFAILED
           DISPLAY 'Remarks: ' LINE 16 COLUMN 10.
       REMARKS-RTN-END.    
       TOTAL-RTN-END.
       ACCEPT-RTN-END.

       WRITE-FILE.
           MOVE ENO TO SENUM.
           MOVE ENAM TO SENAM.
           MOVE DOB TO SDOB.
           MOVE UNINAME TO SUNINAME.
           MOVE CNAME TO SCNAME.
           MOVE REMARKSS TO SREMARKS.
           WRITE PRINT-REC.

       CONTINUE-RTN.
           DISPLAY 'Input Another Record? (Y/N): ' LINE 18 COLUMN 26.
           ACCEPT ANOTHER LINE 18 COLUMN 55.
           IF ANOTHER IS = 'Y'
           MOVE 0 TO EOFSW
           ELSE IF ANOTHER IS = 'N'
           MOVE 1 TO EOFSW
           ELSE IF ANOTHER IS NOT = 'Y' OR 'N'
           DISPLAY 'Y or N only!' LINE 19 COLUMN 30.
           GO TO CONTINUE-RTN THRU CONTINUE-RTN-END.

       CONTINUE-RTN-END.

           DISPLAY 'Total No. of Passed: ' LINE 21 COLUMN 10.
           DISPLAY TOTPASSED LINE 21 COLUMN 37.

           DISPLAY 'Total No. of Failed: ' LINE 22 COLUMN 10.
           DISPLAY TOTFAILED LINE 22 COLUMN 37.
           DISPLAY ' ' LINE 23 COLUMN 37.
