       *>Gravity-driven programming - execution will fall THROUGH
       *> unless redirected
       *> This includes paragraphs.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. Tutorial5.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       PROCEDURE DIVISION.
       *>Open Paragraphs: SubOne.
       SubOne.
           DISPLAY "In Paragraph 1"
           PERFORM SubTwo
           DISPLAY "Returned to Paragraph 1"
           PERFORM Subfour 2 TIMES.
           STOP RUN.

       SubThree.
           DISPLAY "In Paragraph 3".
       SubTwo.
           DISPLAY "In Paragraph 2"
           PERFORM SubThree
           DISPLAY "Returned to Paragraph 2".

       SubFour.
           DISPLAY "Repeat"

            STOP RUN.
       END PROGRAM Tutorial5.
