       IDENTIFICATION DIVISION.
       PROGRAM-ID.      STDLOG.
       AUTHOR.              JN.
      **
      **  Sidst ændret af JN : Tor 14 Dec 2006 11:58:14 CET
      **  Company............: WeDo Computer Systems ApS
      **  Product............: Standard-Program
      **  Release............: 7.2
      **  Computer...........: Any UNIX-box
      **  Program............: Data- og funktions-logning
      **
      **  Programmør.........: John Niclasen
      **
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           COPY     "csstd.cpy".
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY     "selogf02.cpy".
           COPY     "selogf03.cpy".
           COPY     "selogf04.cpy".
       DATA DIVISION.
       FILE SECTION.
           COPY     "fdlogf02.cpy".
           COPY     "fdlogf03.cpy".
           COPY     "fdlogf04.cpy".
       WORKING-STORAGE SECTION.
      *    COPY     "wsstd.cpy".
           COPY     "wsdato.cpy".
           COPY     "wslogf02.cpy".
           COPY     "wslogf03.cpy".
           COPY     "wslogf04.cpy".
       77  CRT-STATUS          PIC 9(3).
      * Til brug for F2-programmer:
           88 KEY-ENTER                VALUE 13.
       01  EVENT-STATUS.
           03 EVENT-TYPE       PIC X(4) COMP-X.
           03 EVENT-WINDOW-HANDLE      USAGE HANDLE OF WINDOW.
           03 EVENT-CONTROL-HANDLE     USAGE HANDLE.
           03 EVENT-CONTROL-ID PIC XX  COMP-X.
           03 EVENT-DATA-1             USAGE SIGNED-SHORT.
           03 EVENT-DATA-2             USAGE SIGNED-LONG.
           03 EVENT-ACTION     PIC X   COMP-X.
       01  SCREEN-CONTROL.
           03 ACCEPT-CONTROL   PIC 9.
              88 GOTO-FIELD            VALUE 1.
              88 TERMINATE-NORMAL      VALUE 2.
              88 TERMINATE-EXCEPTION   VALUE 3.
              88 GOTO-ID               VALUE 4.
           03 CONTROL-VALUE    PIC 999.
           03 FIELD-NUMBER REDEFINES CONTROL-VALUE     PIC 999.
           03 CONTROL-HANDLE           USAGE HANDLE.
           03 CONTROL-ID       PIC XX  COMP-X.
       77  WS-DEFAULT-FONT             USAGE HANDLE EXTERNAL.
       77  WS-ERROR-WINDOW             USAGE HANDLE.
      *
      * Fælles variable ved fil-behandling.
      *
       77  US-FILNAVN          PIC X(20).
       77  US-STAT             PIC XX.
           88 FILE-OK                  VALUE "00".
           88 DUPLICATE-OK             VALUE "02".
           88 DUPLICATE-KEY            VALUE "22".
           88 FILE-NOT-FOUND           VALUE "35".
           88 FILE-CONFLICT            VALUE "39".
           88 SEQ-FILE-NOT-FOUND       VALUE "47".
           88 FILE-LOCKED              VALUE "93".
           88 BUSSY                    VALUE "99".
       77  US-TXT              PIC X(25).
      *
      * External data.
      *
           COPY     "exstd.cpy".
       LINKAGE SECTION.
           COPY     "lsstdlog.cpy".
      *SCREEN SECTION.
      *    COPY     "scstdlog.cpy".
       PROCEDURE DIVISION USING LS-STDLOG.
       DECLARATIVES.
           COPY     "uslogf02.cpy".
           COPY     "uslogf03.cpy".
           COPY     "uslogf04.cpy".
       END DECLARATIVES.
      ********************************
       MAIN-LOGIC SECTION.
      ********************************
       ML-CONTROL.
           IF       LS-STDLOG-FUNK-LOG
                    PERFORM  OPEN-LF03
                    MOVE     LS-STDLOG-TEGNTXT  TO LF03-ST01-TEGNTXT
                    MOVE     LS-STDLOG-ADGANGSNIVEAU
                          TO LF03-ST01-ADGANGSNIVEAU
                    PERFORM  WRITE-LF03
                    PERFORM  CLOSE-LF03
           ELSE IF  LS-STDLOG-DATA-LOG
                    PERFORM  OPEN-LF02
                    MOVE     LS-STDLOG-FIL      TO LF02-FIL
                    MOVE     LS-STDLOG-FIL-KEY  TO LF02-FIL-KEY
                    MOVE     LS-STDLOG-EVENT    TO LF02-EVENT
                    PERFORM  WRITE-LF02
                    PERFORM  CLOSE-LF02
           ELSE IF  LS-STDLOG-CMD-LOG
                    PERFORM  OPEN-LF04
                    MOVE     LS-STDLOG-KOMMANDO TO LF04-KOMMANDO
                    PERFORM  WRITE-LF04
                    PERFORM  CLOSE-LF04.
      
       ML-EXIT.
           EXIT     PROGRAM.
      
      ***********************
       R-FILE SECTION.
      ***********************
           COPY     "pdlogf02.cpy".
           COPY     "pdlogf03.cpy".
           COPY     "pdlogf04.cpy".
       R-EXIT.
           EXIT.
      
      ***********************
       STD SECTION.
      ***********************
           COPY     "userror.cpy".
           COPY     "pddato.cpy".
       STD-EXIT.
           EXIT.
