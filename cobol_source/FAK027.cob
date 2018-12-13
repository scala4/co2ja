       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK027R.
      ******************************************* :   Z-WIN-RPG2     **
      * PROGRAM FAK027      ESPEN LARSEN    16. MAI. 2003             *
      * PROGRAMMET HENTER VAREOPPLYSNINGER FRA FILEN VARETEK, OG      *
      * DANNER EN TEKSTLINJE MED DISSE OPPLYSNINGER.                  *
      * 16.05.03 HENTER NOBB-NR FOR ENKELTE KUNDER AV FIRMA 913(FOMA) *
      * 14.03.13 HENTER for enkelt kunde hos 527                      *
      *          LEGGER UT 20 I ARTIKKEL OG "   " I ALFA              *
      * 26.03.14 KKAT 120 SKAL PRINT NOBB-NR FOR 913                  *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK027.rpg
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
           SELECT INNF
               ASSIGN TO UT-S-INNF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INNF-STATUS.
           SELECT KUNDEMA
               ASSIGN TO KUNDEMA
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMA-STATUS
               RECORD KEY IS KUNDEMA-KEY1.
           SELECT VARETEK
               ASSIGN TO VARETEK
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VARETEK-STATUS
               RECORD KEY IS VARETEK-KEY1.
           SELECT OUTF
               ASSIGN TO UT-S-OUTF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTF-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD INNF
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  INNF-IO-AREA.
           05  INNF-IO-AREA-X              PICTURE X(200).
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1            PICTURE X(9).
               10  FILLER                  PICTURE X(190).
       FD VARETEK
               RECORD CONTAINS 80.
       01  VARETEK-IO-AREA.
           05  VARETEK-IO-AREA-X.
               10  VARETEK-KEY1            PICTURE X(12).
               10  FILLER                  PICTURE X(68).
       FD OUTF
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  OUTF-IO-AREA.
           05  OUTF-IO-AREA-X              PICTURE X(200).
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
           10  INNF-STATUS                 PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  VARETEK-STATUS              PICTURE 99 VALUE 0.
           10  OUTF-STATUS                 PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  INNF-EOF-OFF            VALUE '0'.
               88  INNF-EOF                VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNF-READ-OFF           VALUE '0'.
               88  INNF-READ               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNF-PROCESS-OFF        VALUE '0'.
               88  INNF-PROCESS            VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  INNF-LEVEL-INIT-OFF     VALUE '0'.
               88  INNF-LEVEL-INIT         VALUE '1'.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  VARETEK-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  INNF-LEVEL-01.
               10  INNF-01-L2.
                   15  INNF-01-L2-FIRMNR   PICTURE X(3).
               10  INNF-01-L1.
                   15  INNF-01-L1-ORDENR   PICTURE X(6).
                   15  INNF-01-L1-KUNDNR   PICTURE X(6).
           05  INNF-DATA-FIELDS.
               10  REC1                    PICTURE X(200).
               10  FIRMNR                  PICTURE X(3).
               10  ORDENR                  PICTURE X(6).
               10  RECART                  PICTURE X(1).
               10  LAGER                   PICTURE X(2).
               10  EDBNR                   PICTURE X(7).
               10  RECTYP                  PICTURE X(1).
               10  KUNDNR                  PICTURE X(6).
           05  KUNDEMA-DATA-FIELDS.
               10  KKAT-IO.
                   15  KKAT                PICTURE S9(3) USAGE
                                                       PACKED-DECIMAL.
           05  VARETEK-DATA-FIELDS.
               10  NOBBNR                  PICTURE X(8).
               10  IND2                    PICTURE X(2).
               10  INDNR                   PICTURE X(18).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(12).
           05  TEMPORARY-FIELDS.
               10  ANT-IO.
                   15  ANT                 PICTURE S9(7).
               10  KNRKEY                  PICTURE X(9).
               10  F5                      PICTURE X(5).
               10  VATKEY                  PICTURE X(12).
               10  NULANT-IO.
                   15  NULANT              PICTURE S9(5)V9(2).
               10  NULL-X-IO.
                   15  NULL-X              PICTURE S9(7)V9(2).
               10  ANTNYE-IO.
                   15  ANTNYE              PICTURE S9(7).
           05  EDITTING-FIELDS.
               10  XO-52P-EF.
                 15  XO-52P                PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-72P-EF.
                 15  XO-72P                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-70YY9                PICTURE Z.ZZZ.ZZ9.
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
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  INNF-PROCESS
               SET INNF-PROCESS-OFF        TO TRUE
               SET INNF-READ               TO TRUE
           END-IF
 
           IF  INNF-READ
           AND RECORD-SELECTED-OFF
               PERFORM INNF-GET
               SET INNF-READ-OFF           TO TRUE
               IF  NOT INNF-EOF
                   SET INNF-PROCESS        TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  INNF-PROCESS
               PERFORM INNF-IDSET
           END-IF
 
           IF  INNF-PROCESS
               PERFORM INNF-CHK-LEVEL
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
 
           IF  INNF-PROCESS
               PERFORM INNF-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  INNF-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-50                    TO TRUE
           ADD 1                           TO ANT
      *****************************************************************
      * RUTINE FOR Å HENTE NOBB-NR FRA VARTEK FOR FIRMA 913 FOMA.     *
      *****************************************************************
           IF  (I-L2)
               SET NOT-I-11                TO TRUE
               IF  FIRMNR = '913'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-11)
               GO TO STANOB-T
      *  L2      FIRMNR    COMP "527"                    31 INDUSTRIVA
      *  L1 31             SETOF
      *  L1 31   KUNDNR    COMP "120274"                 13 KUNDE
      *  31 13             GOTO STAIND                      IKKE FOMA
           END-IF
           GO TO ENDNOB-T
      * SJEKK OM DET ER ØNSKET KUNDEKAT.
           .
 
       STANOB-T.
           IF  (I-L1)
               MOVE FIRMNR                 TO KNRKEY (1:3)
               MOVE KUNDNR                 TO KNRKEY (4:6)
               MOVE KNRKEY                 TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-12                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-12            TO TRUE
                   PERFORM KUNDEMA-FLDSET
                   PERFORM KUNDEMA-IDSET
               END-READ
           END-IF
           IF  (I-12)
               GO TO ENDNOB-T
           END-IF
           IF  (I-L1)
               SET NOT-I-13                TO TRUE
               IF  KKAT = 105
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KKAT = 107
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KKAT = 109
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KKAT = 116
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KKAT = 120
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               GO TO ENDNOB-T
           END-IF.
 
       STAIND-T.
      * SJEKK RECORDTYPER.
           SET NOT-I-14                    TO TRUE
           IF  RECART = 'L'
               SET I-14                    TO TRUE
           END-IF
           IF  (NOT-I-14)
               GO TO ENDNOB-T
           END-IF
           SET NOT-I-14                    TO TRUE
           IF  EDBNR > '0000000'
               SET I-14                    TO TRUE
           END-IF
           IF  (NOT-I-14)
               GO TO ENDNOB-T
           END-IF
           SET NOT-I-14                    TO TRUE
           IF  RECTYP = 'R'
               SET I-14                    TO TRUE
           END-IF
           IF  (NOT-I-14)
               SET NOT-I-14                TO TRUE
               IF  RECTYP = 'F'
                   SET I-14                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-14)
               SET NOT-I-14                TO TRUE
               IF  RECTYP = 'B'
                   SET I-14                TO TRUE
               END-IF
           END-IF
           IF  (I-14)
               GO TO ENDNOB-T
           END-IF
           SET NOT-I-14                    TO TRUE
           IF  LAGER = 'PT'
               SET I-14                    TO TRUE
           END-IF
           IF  (I-14)
               GO TO ENDNOB-T
      * SJEKK OM NOBB-NR ER INNMELDT PÅ DENNE VARE.
           END-IF
           IF  (I-01)
               MOVE '10'                   TO F5 (1:2)
           END-IF
           IF  (I-01 AND I-31)
               MOVE '01'                   TO F5 (1:2)
           END-IF
           IF  (I-01)
               MOVE FIRMNR                 TO F5 (3:3)
               MOVE F5                     TO VATKEY (1:5)
               MOVE EDBNR                  TO VATKEY (6:7)
               MOVE VATKEY                 TO VARETEK-KEY1
               READ VARETEK RECORD KEY IS VARETEK-KEY1
               INVALID KEY
                   SET I-12                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-12            TO TRUE
                   PERFORM VARETEK-FLDSET
                   PERFORM VARETEK-IDSET
               END-READ
           END-IF
           IF  (I-01 AND I-12)
               GO TO ENDNOB-T
           END-IF
           IF  (I-01 AND I-31)
               SET NOT-I-14                TO TRUE
               IF  IND2 = '**'
                   SET I-14                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-31 AND NOT-I-14)
               GO TO ENDNOB-T
           END-IF
           IF  (I-01)
               SET NOT-I-14                TO TRUE
               IF  NOBBNR > '        '
                   SET I-14                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-14)
               GO TO ENDNOB-T
           END-IF
           IF  (I-01)
               SET I-50                    TO TRUE
               MOVE 0,00                   TO NULANT
               MOVE 0,00                   TO NULL-X
           END-IF
           IF  (I-50)
               ADD 1                       TO ANTNYE
           END-IF.
 
       ENDNOB-T.
      *****************************************************************
      *          SLUTT     TAG
           CONTINUE.
 
       INNF-GET SECTION.
       INNF-GET-P.
           IF  INNF-EOF-OFF
               READ INNF
               AT END
                   SET INNF-EOF            TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INNF-FLDSET SECTION.
       INNF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INNF-IO-AREA (1:200)   TO REC1 (1:200)
               MOVE INNF-IO-AREA (1:3)     TO FIRMNR (1:3)
               MOVE INNF-IO-AREA (19:6)    TO ORDENR (1:6)
               MOVE INNF-IO-AREA (25:1)    TO RECART (1:1)
               MOVE INNF-IO-AREA (62:2)    TO LAGER (1:2)
               MOVE INNF-IO-AREA (141:7)   TO EDBNR (1:7)
               MOVE INNF-IO-AREA (177:1)   TO RECTYP (1:1)
               MOVE INNF-IO-AREA (184:6)   TO KUNDNR (1:6)
           END-EVALUATE.
 
       INNF-IDSET SECTION.
       INNF-IDSET-P.
           SET I-01                        TO TRUE.
 
       INNF-CHK-LEVEL SECTION.
       INNF-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO INNF-LEVEL-01
               MOVE INNF-IO-AREA (1:3)     TO INNF-01-L2-FIRMNR
               MOVE INNF-IO-AREA (19:6)    TO INNF-01-L1-ORDENR
               MOVE INNF-IO-AREA (184:6)   TO INNF-01-L1-KUNDNR
               IF  INNF-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INNF-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INNF-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INNF-01-L2            TO THE-PRIOR-L2
               MOVE  INNF-01-L1            TO THE-PRIOR-L1
               SET INNF-LEVEL-INIT         TO TRUE
           END-EVALUATE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (162:2) TO KKAT-IO
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-02                        TO TRUE.
 
       VARETEK-FLDSET SECTION.
       VARETEK-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VARETEK-IO-AREA (13:8) TO NOBBNR (1:8)
               MOVE VARETEK-IO-AREA (13:2) TO IND2 (1:2)
               MOVE VARETEK-IO-AREA (15:18) TO INDNR (1:18)
           END-EVALUATE.
 
       VARETEK-IDSET SECTION.
       VARETEK-IDSET-P.
           SET I-03                        TO TRUE.
 
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
           IF  (I-01)
               MOVE SPACES TO OUTF-IO-AREA
               INITIALIZE OUTF-IO-AREA
               MOVE REC1                   TO OUTF-IO-AREA (1:200)
               WRITE OUTF-IO-AREA
           END-IF
           IF  (I-01 AND I-50 AND I-31)
               MOVE SPACES TO OUTF-IO-AREA
               INITIALIZE OUTF-IO-AREA
               MOVE REC1                   TO OUTF-IO-AREA (1:200)
               MOVE ' '                    TO OUTF-IO-AREA (37:1)
               MOVE '     '                TO OUTF-IO-AREA (77:5)
               MOVE '             '        TO OUTF-IO-AREA (82:13)
               MOVE '    '                 TO OUTF-IO-AREA (95:4)
               MOVE '   '                  TO OUTF-IO-AREA (99:3)
               MOVE '                    ' TO OUTF-IO-AREA (102:20)
               MOVE 'EMG NORAUTRON: '      TO OUTF-IO-AREA (87:15)
               MOVE INDNR                  TO OUTF-IO-AREA (102:18)
               MOVE '          '           TO OUTF-IO-AREA (122:10)
               MOVE NULANT                 TO XO-52P
               MOVE XO-52P-EF              TO OUTF-IO-AREA (133:4)
               MOVE NULANT                 TO XO-52P
               MOVE XO-52P-EF              TO OUTF-IO-AREA (137:4)
               MOVE '                    ' TO OUTF-IO-AREA (141:20)
               MOVE '     '                TO OUTF-IO-AREA (161:5)
               MOVE NULL-X                 TO XO-72P
               MOVE XO-72P-EF              TO OUTF-IO-AREA (171:5)
               MOVE ' '                    TO OUTF-IO-AREA (177:1)
               MOVE '  '                   TO OUTF-IO-AREA (179:2)
               MOVE 'EMG'                  TO OUTF-IO-AREA (190:3)
               WRITE OUTF-IO-AREA
           END-IF
           IF  (I-01 AND I-50 AND I-11)
               MOVE SPACES TO OUTF-IO-AREA
               INITIALIZE OUTF-IO-AREA
               MOVE REC1                   TO OUTF-IO-AREA (1:200)
               MOVE ' '                    TO OUTF-IO-AREA (37:1)
               MOVE '     '                TO OUTF-IO-AREA (77:5)
               MOVE 'NOBB NR.'             TO OUTF-IO-AREA (82:8)
               MOVE NOBBNR                 TO OUTF-IO-AREA (91:8)
               MOVE '   '                  TO OUTF-IO-AREA (99:3)
               IF  (I-31)
                   MOVE INDNR              TO OUTF-IO-AREA (84:18)
               END-IF
               MOVE '                    ' TO OUTF-IO-AREA (102:20)
               MOVE '          '           TO OUTF-IO-AREA (122:10)
               MOVE NULANT                 TO XO-52P
               MOVE XO-52P-EF              TO OUTF-IO-AREA (133:4)
               MOVE NULANT                 TO XO-52P
               MOVE XO-52P-EF              TO OUTF-IO-AREA (137:4)
               MOVE '                    ' TO OUTF-IO-AREA (141:20)
               MOVE '     '                TO OUTF-IO-AREA (161:5)
               MOVE NULL-X                 TO XO-72P
               MOVE XO-72P-EF              TO OUTF-IO-AREA (171:5)
               MOVE ' '                    TO OUTF-IO-AREA (177:1)
               MOVE '  '                   TO OUTF-IO-AREA (179:2)
               MOVE 'NOB'                  TO OUTF-IO-AREA (190:3)
               IF  (I-31)
                   MOVE '   '              TO OUTF-IO-AREA (190:3)
               END-IF
               WRITE OUTF-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '***   AVSTEMNINGSTOTALER' TO LISTE-IO-AREA (1:24)
               MOVE '   --- FAK027 ---    ***' TO LISTE-IO-AREA (25:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANT                    TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (2:9)
               MOVE 'RECORDS LEST           ' TO LISTE-IO-AREA (12:23)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '**             NYE TEKST' TO LISTE-IO-AREA (1:24)
               MOVE ' REC.DANNET NOBB-NR  ***' TO LISTE-IO-AREA (25:24)
               MOVE ANTNYE                 TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (6:9)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
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
           MOVE 1                          TO LR-CHECK
           SET INNF-LEVEL-INIT             TO TRUE
           INITIALIZE INNF-DATA-FIELDS
           SET INNF-EOF-OFF                TO TRUE
           SET INNF-PROCESS                TO TRUE
           OPEN INPUT INNF
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           INITIALIZE VARETEK-DATA-FIELDS
           OPEN INPUT VARETEK
           OPEN OUTPUT OUTF
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INNF
           CLOSE KUNDEMA
           CLOSE VARETEK
           CLOSE OUTF
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
