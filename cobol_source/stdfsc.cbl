       IDENTIFICATION DIVISION.
       PROGRAM-ID.      STDFSC.
       AUTHOR.              JN.
      **
      **  Sidst ændret af JN : Tir 17 Apr 2007 14:06:03 CEST
      **  Company............: WeDo Computer Systems ApS
      **  Product............: Standard-Program
      **  Release............: 7.2
      **  Computer...........: Any UNIX-box
      **  Program............: Returnerer File Status Codes
      **
      **  Programmør.........: John Niclasen
      **
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY     "sestdf06.cpy".
       DATA DIVISION.
       FILE SECTION.
           COPY     "fdstdf06.cpy".
       WORKING-STORAGE SECTION.
           COPY     "exstd.cpy".
           COPY     "wsstdf06.cpy".
           COPY     "lsstdlic.cpy".
       77  W-POP-ERR           PIC X(10).
       77  US-STAT             PIC XX.
           88 BUSSY                    VALUE "99".
       LINKAGE SECTION.
       77  LS-STAT             PIC XX.
       77  LS-TXT              PIC X(25).
       PROCEDURE DIVISION USING LS-STAT LS-TXT.
       DECLARATIVES.
           COPY     "usstdf06.cpy".
       END DECLARATIVES.
      ********************************
       MAIN-LOGIC SECTION.
      ********************************
       ML-CONTROL.
           PERFORM  R-OPEN.
      
           MOVE     LS-STAT TO ST06-STAT.
           PERFORM  READ-ST06-NL.
      
           IF       VAL-ST06
                    MOVE     ST06-TXT TO LS-TXT
           ELSE
                    MOVE     SPACE TO LS-TXT.
      
           PERFORM  R-CLOSE.
      
       ML-EXIT.
           EXIT     PROGRAM.
      
      ***********************
       R-FILE SECTION.
      ***********************
           COPY     "pdstdf06.cpy".
       R-OPEN.
           PERFORM  OPEN-I-ST06.
      
       R-CLOSE.
           PERFORM  CLOSE-ST06.
      
       R-EXIT.
           EXIT.
