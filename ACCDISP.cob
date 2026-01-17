       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCDISP.
      *AUTHOR. MIGEL H. TAN.
      *INSTALLATION. VALENZUELA CITY.
      *DATE-WRITTEN. JANUARY 10, 2026.
      *DATE-COMPILED. JANUARY 10, 2026.
      *SECURITY. FOR BSIT 2-4 AND DR. FABREGAS.
      *REMARKS. ACCEPT DISPLAY NO OUTPUT AND INPUT FILE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                                           
           SELECT OUTFILE ASSIGN TO "OUTAD.TXT".

       DATA DIVISION.
       FILE SECTION.
       FD OUTFILE.

       01 PRINT-REC.
           05 SSNO PIC 9(5).
           05 SNA PIC X(25).
           05 SCOURSE PIC 9.
           05 SYS PIC X(5).
           05 SSTYPE PIC 9.
           05 SMIDTERM PIC 9V99.
           05 SFINALS PIC 9V99.
           05 SAVERAGE PIC 9V99.
           05 SREMARKSS PIC X(6).

       01 DATABASE.
           02 FILLER PIC X(80).

       WORKING-STORAGE SECTION.

       01 SNO PIC 9(5) VALUE ZERO.
       01 SNAME PIC X(25) VALUE SPACES.
       01 COURSE PIC 9 VALUE ZERO.
       01 YEAR-SECTION PIC X(5) VALUE SPACES.
       01 STYPE PIC 9 VALUE ZERO.
       01 MIDTERM PIC 9V99 VALUE ZERO.
       01 FINALS PIC 9V99 VALUE ZERO.
       01 AVERAGE PIC 9V99 VALUE ZERO.
       01 AVERAGEFIN PIC 9.999 VALUE ZERO.
       01 REMARKSS PIC X(6) VALUE SPACES.
       01 ANOTHER PIC X VALUE SPACES.
       01 EOFSW PIC 9 VALUE 9.

       SCREEN SECTION.
       01 CLRSCR.
           02 BLANK SCREEN.

       PROCEDURE DIVISION.

      *eofsw
       MAIN-RT.
           OPEN EXTEND OUTFILE.
           PERFORM PROCESS-RECORDS UNTIL EOFSW = 1.
           CLOSE OUTFILE.
           STOP RUN.

      *functions na gagamitin
       PROCESS-RECORDS.

           PERFORM DISPLAY-HEADER.
           PERFORM ACCEPT-STUD.
           PERFORM ACCEPT-COURSE THRU ACCEPT-COURSE-END.
           PERFORM ACCEPT-STYPE THRU ACCEPT-STYPE-END.
           PERFORM ACCEPT-GRADES THRU ACCEPT-GRADES-END.
           PERFORM COMPUTE-AVERAGE.
           PERFORM STUD-REMARKS.
           PERFORM WRITE-FILE.
           PERFORM ASK-CONTINUE THRU ASK-CONTINUE-END.

      *Terminal
       DISPLAY-HEADER.
           DISPLAY CLRSCR.
           DISPLAY ' ' LINE 1 COLUMN 40.
           DISPLAY 'PUP' LINE 2 COLUMN 39.
           DISPLAY 'CCIS' LINE 3 COLUMN 39.
           DISPLAY ' ' LINE 4 COLUMN 40.
           DISPLAY ' ' LINE 5 COLUMN 40.

      *display of user input
       ACCEPT-STUD.
           DISPLAY 'Student Number: ' LINE 6 COLUMN 10.
           ACCEPT SNO LINE 6 COLUMN 60.
           DISPLAY 'Student Name: ' LINE 7 COLUMN 10.
           ACCEPT SNAME LINE 7 COLUMN 60.

      *course validation
       ACCEPT-COURSE.
           DISPLAY 'Course: ' LINE 8 COLUMN 10.
           ACCEPT COURSE LINE 8 COLUMN 60.
           IF COURSE IS GREATER THAN 2 OR COURSE IS LESS THAN 1
               DISPLAY '1 or 2 only.' LINE 8 COLUMN 30
               GO TO ACCEPT-COURSE
           ELSE
               GO TO ACCEPT-COURSE-END
           END-IF.
       ACCEPT-COURSE-END.

           DISPLAY 'Year & Section: ' LINE 9 COLUMN 10.
           ACCEPT YEAR-SECTION LINE 9 COLUMN 60.

       ACCEPT-STYPE.
           DISPLAY 'Student Type: ' LINE 10 COLUMN 10.
           ACCEPT STYPE LINE 10 COLUMN 60.
           IF STYPE IS GREATER THAN 2 OR LESS THAN 1
               DISPLAY '1 or 2 only.' LINE 10 COLUMN 30
               GO TO ACCEPT-STYPE
           ELSE
               GO TO ACCEPT-STYPE-END.
       ACCEPT-STYPE-END.

       ACCEPT-GRADES.
       ACCEPT-GRADES-MT.
           DISPLAY 'Midterm Grade: ' LINE 11 COLUMN 10.
           ACCEPT MIDTERM LINE 11 COLUMN 60.
           IF MIDTERM IS GREATER THAN 5.00
           DISPLAY 'Must be >5 or <0.' LINE 11 COLUMN 30
           GO TO ACCEPT-GRADES-MT
           ELSE
               GO TO ACCEPT-GRADES-MT-END.
       ACCEPT-GRADES-MT-END.

       ACCEPT-GRADES-FINALS.
           DISPLAY 'Final Grade: ' LINE 12 COLUMN 10.
           ACCEPT FINALS LINE 12 COLUMN 60.
           IF FINALS IS GREATER THAN 5.00
           DISPLAY 'Must be >5 or <0.' LINE 12 COLUMN 30
           GO TO ACCEPT-GRADES-FINALS
           ELSE
               GO TO ACCEPT-GRADES-FINALS-END.
       ACCEPT-GRADES-FINALS-END.
       ACCEPT-GRADES-END.

      *computing average functions
       COMPUTE-AVERAGE.
           COMPUTE AVERAGE = (MIDTERM + FINALS) / 2.
           MOVE AVERAGE TO AVERAGEFIN.
           DISPLAY 'Average: ' LINE 13 COLUMN 10.
           DISPLAY AVERAGEFIN LINE 13 COLUMN 60.

      *validation
       STUD-REMARKS.
           IF AVERAGE IS GREATER THAN 3.00
               MOVE 'Failed' TO REMARKSS
           ELSE
               MOVE 'Passed' TO REMARKSS.

           DISPLAY 'Remarks: ' LINE 14 COLUMN 10.
           DISPLAY REMARKSS LINE 14 COLUMN 60.

      *write to file
       WRITE-FILE.
           MOVE SNO TO SSNO.
           MOVE SNAME TO SNA.
           MOVE COURSE TO SCOURSE.
           MOVE YEAR-SECTION TO SYS.
           MOVE STYPE TO SSTYPE.
           MOVE MIDTERM TO SMIDTERM.
           MOVE FINALS TO SFINALS.
           MOVE AVERAGE TO SAVERAGE.
           MOVE REMARKSS TO SREMARKSS.
           WRITE PRINT-REC.

      *SAME SYNTAX SA DO WHILE.
       ASK-CONTINUE.
           DISPLAY ' ' LINE 15 COLUMN 50.
           DISPLAY 'Enter Another Record? (Y/N): ' LINE 16 COLUMN 3.
           ACCEPT ANOTHER LINE 16 COLUMN 60.
           IF ANOTHER IS = 'Y'
               MOVE 0 TO AVERAGE
               MOVE SPACES TO REMARKSS
           ELSE
               IF ANOTHER IS = 'N'
                   MOVE 1 TO EOFSW
               ELSE
                   DISPLAY 'Invalid!' LINE 16 COLUMN 33
                   GO TO ASK-CONTINUE
               END-IF
           END-IF.
       ASK-CONTINUE-END.

           DISPLAY 'MADE BY: Migel H. Tan' LINE 18 COLUMN 29.
           DISPLAY ' ' LINE 19 COLUMN 60.
           DISPLAY ' ' LINE 20 COLUMN 60.
