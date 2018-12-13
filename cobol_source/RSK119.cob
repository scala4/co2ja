       IDENTIFICATION DIVISION.
       PROGRAM-ID. RSK119R.
      **********************************************  Z-WIN-RPG2      *
      *  PROGRAM.......: RSK119                                      *
      *  PROGRAMMET DANNER RELATERINGS-RECORDS TIL OPPSLAGSFILE      *
      *  FOR TEST PÅ KUNDE-RELNUMMER-BRUTTOSUM I PROGRAMMET BOKF     *
      *  SAMT FJERNER RELATERINGS-POSTER ELDRE ENN 3 MÅNEDER.        *
      *E 12.01.00 LISTER TRANSER SOM FJERNES NÅR U1 ER PÅ.           *
      *           SKRIVER SANERINGSKRITERIER.                        *
      ****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RSK119.rpg
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
               UPSI-0
                    ON STATUS IS U-1-ON
                   OFF STATUS IS U-1-OFF
               UPSI-1
                    ON STATUS IS U-2-ON
                   OFF STATUS IS U-2-OFF
               UPSI-2
                    ON STATUS IS U-3-ON
                   OFF STATUS IS U-3-OFF
               UPSI-3
                    ON STATUS IS U-4-ON
                   OFF STATUS IS U-4-OFF
               UPSI-4
                    ON STATUS IS U-5-ON
                   OFF STATUS IS U-5-OFF
               UPSI-5
                    ON STATUS IS U-6-ON
                   OFF STATUS IS U-6-OFF
               UPSI-6
                    ON STATUS IS U-7-ON
                   OFF STATUS IS U-7-OFF
               UPSI-7
                    ON STATUS IS U-8-ON
                   OFF STATUS IS U-8-OFF
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RESPAR
               ASSIGN TO UT-S-RESPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RESPAR-STATUS.
           SELECT GMLREL
               ASSIGN TO UT-S-GMLREL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS GMLREL-STATUS.
           SELECT SUMFILE
               ASSIGN TO UT-S-SUMFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS SUMFILE-STATUS.
           SELECT RELFILE
               ASSIGN TO UT-S-RELFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RELFILE-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD RESPAR
               BLOCK CONTAINS 300
               RECORD CONTAINS 300.
       01  RESPAR-IO-AREA.
           05  RESPAR-IO-AREA-X            PICTURE X(300).
       FD GMLREL
               BLOCK CONTAINS 2640
               RECORD CONTAINS 60.
       01  GMLREL-IO-AREA.
           05  GMLREL-IO-AREA-X            PICTURE X(60).
       FD SUMFILE
               BLOCK CONTAINS 4080
               RECORD CONTAINS 30.
       01  SUMFILE-IO-AREA.
           05  SUMFILE-IO-AREA-X           PICTURE X(30).
       FD RELFILE
               BLOCK CONTAINS 2640
               RECORD CONTAINS 60.
       01  RELFILE-IO-AREA.
           05  RELFILE-IO-AREA-X           PICTURE X(60).
      *BUGFILO O   F  80  80            PRINTERSYSLST
      *
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  RESPAR-STATUS               PICTURE 99 VALUE 0.
           10  GMLREL-STATUS               PICTURE 99 VALUE 0.
           10  SUMFILE-STATUS              PICTURE 99 VALUE 0.
           10  RELFILE-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  RESPAR-EOF-OFF          VALUE '0'.
               88  RESPAR-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESPAR-READ-OFF         VALUE '0'.
               88  RESPAR-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESPAR-PROCESS-OFF      VALUE '0'.
               88  RESPAR-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLREL-EOF-OFF          VALUE '0'.
               88  GMLREL-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLREL-READ-OFF         VALUE '0'.
               88  GMLREL-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLREL-PROCESS-OFF      VALUE '0'.
               88  GMLREL-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  GMLREL-LEVEL-INIT-OFF   VALUE '0'.
               88  GMLREL-LEVEL-INIT       VALUE '1'.
           05  LISTE-DATA-FIELDS.
               10  LISTE-AFTER-SPACE       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE-AFTER-SKIP        PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE-BEFORE-SPACE      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE-BEFORE-SKIP       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE-MAX-LINES         PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE-LINE-COUNT        PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE-CLR-IO            PICTURE X VALUE 'Y'.
           05  RESPAR-DATA-FIELDS.
               10  INKNR-IO.
                   15  INKNR               PICTURE S9(2).
               10  INKA-ELGR-IO.
                   15  INKA-ELGR           PICTURE S9(1).
               10  INKPER-IO.
                   15  INKPER              PICTURE S9(4).
           05  GMLREL-LEVEL-02.
               10  GMLREL-02-L1.
                   15  GMLREL-02-L1-FIRMA  PICTURE X(3).
                   15  GMLREL-02-L1-KUNDE  PICTURE X(6).
                   15  GMLREL-02-L1-RELNR  PICTURE X(6).
           05  GMLREL-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  KUNDE                   PICTURE X(6).
               10  RELNR                   PICTURE X(6).
               10  BRUTTO-IO.
                   15  BRUTTO              PICTURE S9(8)V9(2).
               10  REC                     PICTURE X(60).
               10  PERNR-IO.
                   15  PERNR               PICTURE S9(2).
               10  PERA-ELGR-IO.
                   15  PERA-ELGR           PICTURE S9(1).
      *
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(15).
           05  TEMPORARY-FIELDS.
               10  Y-IO.
                   15  Y                   PICTURE S9(1).
               10  INK14-IO.
                   15  INK14               PICTURE S9(2).
               10  TOT-IO.
                   15  TOT                 PICTURE S9(9).
               10  NYUT-IO.
                   15  NYUT                PICTURE S9(9).
               10  GML-IO.
                   15  GML                 PICTURE S9(9).
               10  ANT-IO.
                   15  ANT                 PICTURE S9(9).
           05  EDITTING-FIELDS.
               10  XO-82P-EF.
                 15  XO-82P                PICTURE S9(8)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-10YY9                PICTURE 9.
               10  XO-20YY9                PICTURE Z9.
               10  XO-90YY9                PICTURE ZZZ.ZZZ.ZZ9.
       01  WORK-AREA.
           05  INDICATOR-TABLE.
               COPY TCRPGIN.
           05  SYSTEM-DATE                 PICTURE 9(6).
           05  SYSTEM-DATE-ALPHA           REDEFINES SYSTEM-DATE.
               10  SYSTEM-YEAR             PICTURE 99.
               10  SYSTEM-MONTH            PICTURE 99.
               10  SYSTEM-DAY              PICTURE 99.
           05  SYSTEM-TIME-X.
               10  SYSTEM-TIME             PICTURE 9(6).
               10  FILLER                  PICTURE 99.
           05  LR-CHECK                    PICTURE 9(4) BINARY.
           05  UDATE                       PICTURE 9(6).
           05  UDATE-DDMMYY.
               10  UDAY                    PICTURE 99.
               10  UMONTH                  PICTURE 99.
               10  UYEAR                   PICTURE 99.
           05  EDIT-DATE                   PICTURE 99.99.99.99.99.
           05  TID                         PICTURE X(8).
           05  FILLER                      PICTURE X.
               88  NOT-IN-DETAIL-OUTPUT    VALUE '0'.
               88  IN-DETAIL-OUTPUT        VALUE '1'.
           05  FILLER                      PICTURE X.
               88  RECORD-SELECTED-OFF     VALUE '0'.
               88  RECORD-SELECTED         VALUE '1'.
           05  E-R-R-O-R                   PICTURE X(12).
           05  BW-A                        PICTURE 9(4) USAGE BINARY.
           05  FILLER REDEFINES BW-A.
               10  BW-A-1                  PICTURE X.
               10  BW-A-2                  PICTURE X.
           05  BW-B                        PICTURE 9(4) USAGE BINARY.
           05  FILLER REDEFINES BW-B.
               10  BW-B-1                  PICTURE X.
               10  BW-B-2                  PICTURE X.
       PROCEDURE DIVISION.
 
       MAIN-LINE.
           PERFORM INITIALIZATION
 
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  RESPAR-PROCESS
               SET RESPAR-PROCESS-OFF      TO TRUE
               SET RESPAR-READ             TO TRUE
           END-IF
 
           IF  RESPAR-READ
           AND RECORD-SELECTED-OFF
               PERFORM RESPAR-GET
               SET RESPAR-READ-OFF         TO TRUE
               IF  NOT RESPAR-EOF
                   SET RESPAR-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  GMLREL-PROCESS
               SET GMLREL-PROCESS-OFF      TO TRUE
               SET GMLREL-READ             TO TRUE
           END-IF
 
           IF  GMLREL-READ
           AND RECORD-SELECTED-OFF
               PERFORM GMLREL-GET
               SET GMLREL-READ-OFF         TO TRUE
               IF  NOT GMLREL-EOF
                   SET GMLREL-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  RESPAR-PROCESS
               PERFORM RESPAR-IDSET
           END-IF
 
           IF  GMLREL-PROCESS
               PERFORM GMLREL-IDSET
           END-IF
 
           IF  GMLREL-PROCESS
               PERFORM GMLREL-CHK-LEVEL
           END-IF
 
           IF I-1ST
               GO TO LR-INDICATOR-TEST
           END-IF.
 
       MAINLINE-TOTAL-CALCS.
           PERFORM TOTAL-OUTPUT.
 
       LR-INDICATOR-TEST.
           IF  I-LR
               GO TO MAINLINE-TERMINATION
           END-IF
 
           IF  RESPAR-PROCESS
               PERFORM RESPAR-FLDSET
           END-IF
 
           IF  GMLREL-PROCESS
               PERFORM GMLREL-FLDOFF
               PERFORM GMLREL-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  GMLREL-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-01)
               SUBTRACT 10                 FROM INKNR
               SET NOT-I-15                TO TRUE
               IF  INKNR NOT > 0
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-15)
               ADD 48                      TO INKNR
      *
      *  AVIK P.G.A 24 INK.PERIODER 1984.
           END-IF
           IF  (I-01)
               SET NOT-I-14                TO TRUE
               IF  UYEAR = 85
                   SET I-14                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-14 AND I-15)
               DIVIDE INKNR BY 2       GIVING INKNR
           END-IF
           IF  (I-01)
               MOVE UYEAR (2:1)            TO Y-IO
      *
           END-IF
           IF  (I-01 AND I-15)
               SUBTRACT 1                  FROM INKA-ELGR
               SET NOT-I-16                TO TRUE
               IF  INKA-ELGR < 0
                   SET I-16                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-16)
               SET NOT-I-16                TO TRUE
               IF  INKA-ELGR > Y
                   SET I-16                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-16)
               MOVE 9                      TO INKA-ELGR
           END-IF
           IF  (I-01)
               ADD 50 TO INKNR         GIVING INK14
      *  01                MOVE "INKÅR   "BUGFL1  8        LEDETXT DEBUG
      *  01      BUGFL1    DEBUGBUGFILO   INKÅR            VIS FELT/IND
      *  01                MOVE "INK14   "BUGFL1  8        LEDETXT DEBUG
      *  01      BUGFL1    DEBUGBUGFILO   INK14            VIS FELT/IND
      *
      *  POSTER ELDRE ENN INKASSO-PERIODE - 2,5 MÅNEDER FJERNES.
      *
           END-IF
           IF  (I-L1)
               SET NOT-I-20                TO TRUE
               IF  PERNR > 48
                   SET I-20                TO TRUE
               END-IF
      *
           END-IF
           IF  (I-L1)
               SET NOT-I-25                TO TRUE
               SET NOT-I-26                TO TRUE
               SET NOT-I-27                TO TRUE
           END-IF
           IF  (I-L1 AND NOT-I-20)
               SET NOT-I-26                TO TRUE
               IF  PERNR NOT < INKNR
                   SET I-26                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-20)
               SET NOT-I-26                TO TRUE
               IF  PERNR NOT < INK14
                   SET I-26                TO TRUE
               END-IF
           END-IF
           IF  (I-L1)
               SET NOT-I-25                TO TRUE
               SET NOT-I-27                TO TRUE
               IF  PERA-ELGR > INKA-ELGR
                   SET I-25                TO TRUE
               END-IF
               IF  PERA-ELGR = INKA-ELGR
                   SET I-27                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-16 AND I-17)
               SET I-25                    TO TRUE
           END-IF
           IF  (I-L1 AND I-26 AND I-27)
               SET I-25                    TO TRUE
      *
           END-IF
           IF  (I-02)
               ADD 1                       TO TOT
           END-IF
           IF  (I-02 AND I-25)
               ADD 1                       TO NYUT
           END-IF
           IF  (I-02 AND NOT-I-25)
               ADD 1                       TO GML
           END-IF
           IF  (I-L1 AND I-25)
               ADD 1                       TO ANT
      *
           END-IF
           .
 
       RESPAR-GET SECTION.
       RESPAR-GET-P.
           IF  RESPAR-EOF-OFF
               READ RESPAR
               AT END
                   SET RESPAR-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RESPAR-FLDSET SECTION.
       RESPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RESPAR-IO-AREA (3:2)   TO INKNR-IO
               INSPECT INKNR-IO REPLACING ALL ' ' BY '0'
               MOVE RESPAR-IO-AREA (6:1)   TO INKA-ELGR-IO
               INSPECT INKA-ELGR-IO REPLACING ALL ' ' BY '0'
               MOVE RESPAR-IO-AREA (3:4)   TO INKPER-IO
               INSPECT INKPER-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       RESPAR-IDSET SECTION.
       RESPAR-IDSET-P.
           SET I-01                        TO TRUE.
 
       GMLREL-GET SECTION.
       GMLREL-GET-P.
           IF  GMLREL-EOF-OFF
               READ GMLREL
               AT END
                   SET GMLREL-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       GMLREL-FLDOFF SECTION.
       GMLREL-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-17                TO TRUE
           END-EVALUATE.
 
       GMLREL-FLDSET SECTION.
       GMLREL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE GMLREL-IO-AREA (3:3)   TO FIRMA (1:3)
               MOVE GMLREL-IO-AREA (6:6)   TO KUNDE (1:6)
               MOVE GMLREL-IO-AREA (12:6)  TO RELNR (1:6)
               MOVE GMLREL-IO-AREA (39:10) TO BRUTTO-IO
               INSPECT BRUTTO-IO REPLACING ALL ' ' BY '0'
               MOVE GMLREL-IO-AREA (1:60)  TO REC (1:60)
               MOVE GMLREL-IO-AREA (15:2)  TO PERNR-IO
               INSPECT PERNR-IO REPLACING ALL ' ' BY '0'
               MOVE GMLREL-IO-AREA (17:1)  TO PERA-ELGR-IO
               INSPECT PERA-ELGR-IO REPLACING ALL ' ' BY '0'
               IF  PERA-ELGR = ZERO
                   SET I-17                TO TRUE
               END-IF
           END-EVALUATE.
 
       GMLREL-IDSET SECTION.
       GMLREL-IDSET-P.
           SET I-02                        TO TRUE.
 
       GMLREL-CHK-LEVEL SECTION.
       GMLREL-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO GMLREL-LEVEL-02
               MOVE GMLREL-IO-AREA (3:3)   TO GMLREL-02-L1-FIRMA
               MOVE GMLREL-IO-AREA (6:6)   TO GMLREL-02-L1-KUNDE
               MOVE GMLREL-IO-AREA (12:6)  TO GMLREL-02-L1-RELNR
               IF  GMLREL-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  GMLREL-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  GMLREL-02-L1          TO THE-PRIOR-L1
               SET GMLREL-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       LISTE-PRINT-LINE SECTION.
       LISTE-PRINT-LINE-P.
           IF  LISTE-BEFORE-SKIP > 0
               PERFORM LISTE-SKIP-BEFORE
           END-IF
           IF  LISTE-BEFORE-SPACE > 0
               PERFORM LISTE-SPACE-BEFORE
               IF  LISTE-AFTER-SKIP > 0
                   PERFORM LISTE-SKIP-AFTER
               END-IF
               IF  LISTE-AFTER-SPACE > 0
                   PERFORM LISTE-SPACE-AFTER
               END-IF
           ELSE
               IF  LISTE-AFTER-SKIP > 0
                   PERFORM LISTE-SKIP-AFTER
               END-IF
               PERFORM LISTE-SPACE-AFTER
           END-IF
           IF  LISTE-LINE-COUNT NOT < LISTE-MAX-LINES
               MOVE 7                      TO LISTE-AFTER-SKIP
           END-IF.
 
       LISTE-SKIP-BEFORE SECTION.
       LISTE-SKIP-BEFORE-P.
           WRITE LISTE-IO-PRINT         AFTER ADVANCING PAGE
           MOVE 1                          TO LISTE-LINE-COUNT
           MOVE 0                          TO LISTE-BEFORE-SKIP
           INITIALIZE LISTE-IO-AREA.
 
       LISTE-SPACE-BEFORE SECTION.
       LISTE-SPACE-BEFORE-P.
           WRITE LISTE-IO-PRINT         AFTER LISTE-BEFORE-SPACE LINES
           ADD LISTE-BEFORE-SPACE          TO LISTE-LINE-COUNT
           MOVE SPACES TO LISTE-IO-AREA
           INITIALIZE LISTE-IO-AREA
           MOVE 0                          TO LISTE-BEFORE-SPACE.
 
       LISTE-SKIP-AFTER SECTION.
       LISTE-SKIP-AFTER-P.
           WRITE LISTE-IO-PRINT        BEFORE ADVANCING PAGE
           MOVE 1                          TO LISTE-LINE-COUNT
           MOVE 0                          TO LISTE-AFTER-SKIP
           INITIALIZE LISTE-IO-AREA.
 
       LISTE-SPACE-AFTER SECTION.
       LISTE-SPACE-AFTER-P.
           WRITE LISTE-IO-PRINT        BEFORE LISTE-AFTER-SPACE LINES
           ADD LISTE-AFTER-SPACE           TO LISTE-LINE-COUNT
           INITIALIZE LISTE-IO-AREA
           MOVE 0                          TO LISTE-AFTER-SPACE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND I-25)
               MOVE SPACES TO RELFILE-IO-AREA
               INITIALIZE RELFILE-IO-AREA
               MOVE REC                    TO RELFILE-IO-AREA (1:60)
               WRITE RELFILE-IO-AREA
           END-IF
           IF  (I-L1 AND I-25)
               MOVE SPACES TO SUMFILE-IO-AREA
               INITIALIZE SUMFILE-IO-AREA
               MOVE 'R'                    TO SUMFILE-IO-AREA (1:1)
               MOVE FIRMA                  TO SUMFILE-IO-AREA (2:3)
               MOVE 'REL*'                 TO SUMFILE-IO-AREA (5:4)
               MOVE KUNDE                  TO SUMFILE-IO-AREA (9:6)
               MOVE RELNR                  TO SUMFILE-IO-AREA (15:6)
               MOVE '  '                   TO SUMFILE-IO-AREA (21:2)
               MOVE BRUTTO                 TO XO-82P
               MOVE XO-82P-EF              TO SUMFILE-IO-AREA (23:6)
               WRITE SUMFILE-IO-AREA
           END-IF
           IF  (I-02 AND NOT-I-25 AND I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE REC                    TO LISTE-IO-AREA (1:60)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (13:8)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE '************************' TO LISTE-IO-AREA (49:24)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '****  FJERNING FRA RELFI' TO LISTE-IO-AREA (25:24)
               MOVE 'LE   ** RSK119 **   ****' TO LISTE-IO-AREA (49:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '****  SYSTEMÅR         =' TO LISTE-IO-AREA (25:24)
               MOVE Y                      TO XO-10YY9
               MOVE XO-10YY9               TO LISTE-IO-AREA (59:1)
               MOVE '****'                 TO LISTE-IO-AREA (69:4)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '****  INKPER           =' TO LISTE-IO-AREA (25:24)
               MOVE INKPER-IO              TO LISTE-IO-AREA (56:4)
               MOVE '****'                 TO LISTE-IO-AREA (69:4)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '****                    ' TO LISTE-IO-AREA (25:24)
               MOVE '****'                 TO LISTE-IO-AREA (69:4)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '****  SANERINGSGRENSE:  ' TO LISTE-IO-AREA (25:24)
               MOVE '****'                 TO LISTE-IO-AREA (69:4)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '****  - NÅR INKÅR      =' TO LISTE-IO-AREA (25:24)
               MOVE INKA-ELGR              TO XO-10YY9
               MOVE XO-10YY9               TO LISTE-IO-AREA (59:1)
               MOVE '****'                 TO LISTE-IO-AREA (69:4)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '****  - OG INKNR       <' TO LISTE-IO-AREA (25:24)
               MOVE INKNR                  TO XO-20YY9
               MOVE XO-20YY9               TO LISTE-IO-AREA (58:2)
               MOVE '****'                 TO LISTE-IO-AREA (69:4)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '****  - ELLER INK14    <' TO LISTE-IO-AREA (25:24)
               MOVE INK14                  TO XO-20YY9
               MOVE XO-20YY9               TO LISTE-IO-AREA (58:2)
               MOVE '****'                 TO LISTE-IO-AREA (69:4)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '****                    ' TO LISTE-IO-AREA (25:24)
               MOVE '****'                 TO LISTE-IO-AREA (69:4)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '****       ANTALL LEST =' TO LISTE-IO-AREA (25:24)
               MOVE TOT                    TO XO-90YY9
               MOVE XO-90YY9               TO LISTE-IO-AREA (49:11)
               MOVE '****'                 TO LISTE-IO-AREA (69:4)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '****    ANTALL FJERNET =' TO LISTE-IO-AREA (25:24)
               MOVE GML                    TO XO-90YY9
               MOVE XO-90YY9               TO LISTE-IO-AREA (49:11)
               MOVE '****'                 TO LISTE-IO-AREA (69:4)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '****  ANTALL NY MASTER =' TO LISTE-IO-AREA (25:24)
               MOVE NYUT                   TO XO-90YY9
               MOVE XO-90YY9               TO LISTE-IO-AREA (49:11)
               MOVE '****'                 TO LISTE-IO-AREA (69:4)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '****                    ' TO LISTE-IO-AREA (25:24)
               MOVE '****'                 TO LISTE-IO-AREA (69:4)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '****  ANTALL - OPPSMAS =' TO LISTE-IO-AREA (25:24)
               MOVE ANT                    TO XO-90YY9
               MOVE XO-90YY9               TO LISTE-IO-AREA (49:11)
               MOVE '****'                 TO LISTE-IO-AREA (69:4)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '****                    ' TO LISTE-IO-AREA (25:24)
               MOVE '****'                 TO LISTE-IO-AREA (69:4)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE '************************' TO LISTE-IO-AREA (49:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HALT-INDICATOR-CHECK SECTION.
       HALT-INDICATOR-CHECK-P.
           IF (I-H0 OR I-H1 OR I-H2 OR I-H3 OR I-H4
           OR  I-H5 OR I-H6 OR I-H7 OR I-H8 OR I-H9)
               DISPLAY 'USER SET HALT INDICATORS ARE: '
               F-H0 ',' F-H1 ',' F-H2 ',' F-H3 ',' F-H4 ','
               F-H5 ',' F-H6 ',' F-H7 ',' F-H8 ',' F-H9 UPON CONSOLE
               GO TO MAINLINE-TERMINATION
           END-IF.
 
       INITIALIZATION SECTION.
       INITIALIZATION-P.
           MOVE ZERO                       TO RETURN-CODE
           MOVE ZEROS                      TO INDICATOR-TABLE
           SET I-1ST                       TO TRUE
           SET I-L0                        TO TRUE
           SET I-1P                        TO TRUE
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
           IF  U-1-ON
               SET I-U1                    TO TRUE
           END-IF
           IF  U-2-ON
               SET I-U2                    TO TRUE
           END-IF
           IF  U-3-ON
               SET I-U3                    TO TRUE
           END-IF
           IF  U-4-ON
               SET I-U4                    TO TRUE
           END-IF
           IF  U-5-ON
               SET I-U5                    TO TRUE
           END-IF
           IF  U-6-ON
               SET I-U6                    TO TRUE
           END-IF
           IF  U-7-ON
               SET I-U7                    TO TRUE
           END-IF
           IF  U-8-ON
               SET I-U8                    TO TRUE
           END-IF
           ACCEPT SYSTEM-DATE            FROM DATE
           ACCEPT SYSTEM-TIME-X          FROM TIME
           MOVE SYSTEM-YEAR                TO UYEAR
           MOVE SYSTEM-MONTH               TO UMONTH
           MOVE SYSTEM-DAY                 TO UDAY
           MOVE UDATE-DDMMYY               TO UDATE
           MOVE 2                          TO LR-CHECK
           INITIALIZE RESPAR-DATA-FIELDS
           SET RESPAR-EOF-OFF              TO TRUE
           SET RESPAR-PROCESS              TO TRUE
           OPEN INPUT RESPAR
           SET GMLREL-LEVEL-INIT           TO TRUE
           INITIALIZE GMLREL-DATA-FIELDS
           SET GMLREL-EOF-OFF              TO TRUE
           SET GMLREL-PROCESS              TO TRUE
           OPEN INPUT GMLREL
           OPEN OUTPUT SUMFILE
           OPEN OUTPUT RELFILE
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE RESPAR
           CLOSE GMLREL
           CLOSE SUMFILE
           CLOSE RELFILE
           IF LISTE-IO-AREA NOT = SPACES
             WRITE LISTE-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE-IO-AREA
           END-IF
           CLOSE LISTE.
 
       SETOFF-I-L SECTION.
           SET NOT-I-L1                    TO TRUE.
           SET NOT-I-L2                    TO TRUE.
           SET NOT-I-L3                    TO TRUE.
           SET NOT-I-L4                    TO TRUE.
           SET NOT-I-L5                    TO TRUE.
           SET NOT-I-L6                    TO TRUE.
           SET NOT-I-L7                    TO TRUE.
           SET NOT-I-L8                    TO TRUE.
           SET NOT-I-L9                    TO TRUE.
 
       SETON-I-L9 SECTION.
           SET I-L9                        TO TRUE.
           PERFORM SETON-I-L8.
 
       SETON-I-L8 SECTION.
           SET I-L8                        TO TRUE.
           PERFORM SETON-I-L7.
 
       SETON-I-L7 SECTION.
           SET I-L7                        TO TRUE.
           PERFORM SETON-I-L6.
 
       SETON-I-L6 SECTION.
           SET I-L6                        TO TRUE.
           PERFORM SETON-I-L5.
 
       SETON-I-L5 SECTION.
           SET I-L5                        TO TRUE.
           PERFORM SETON-I-L4.
 
       SETON-I-L4 SECTION.
           SET I-L4                        TO TRUE.
           PERFORM SETON-I-L3.
 
       SETON-I-L3 SECTION.
           SET I-L3                        TO TRUE.
           PERFORM SETON-I-L2.
 
       SETON-I-L2 SECTION.
           SET I-L2                        TO TRUE.
           PERFORM SETON-I-L1.
 
       SETON-I-L1 SECTION.
           SET I-L1                        TO TRUE.
 
       SETOFF-I-H SECTION.
           SET NOT-I-H1                    TO TRUE.
           SET NOT-I-H2                    TO TRUE.
           SET NOT-I-H3                    TO TRUE.
           SET NOT-I-H4                    TO TRUE.
           SET NOT-I-H5                    TO TRUE.
           SET NOT-I-H6                    TO TRUE.
           SET NOT-I-H7                    TO TRUE.
           SET NOT-I-H8                    TO TRUE.
           SET NOT-I-H9                    TO TRUE.
           SET NOT-I-H0                    TO TRUE.
