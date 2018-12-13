       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORD775R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAM ORD775,  PROGRAMMERER: RUNE ERSVIK                    *
      * DANNER FIL MED LAYOUT SOM FAKTURA.SALGDATA MED RESTORDRE OG   *
      * FORHÅNDSORDRE                                                 *
      *                    , SAMT BEREGNER SALGSUM PR. VARELINJE.     *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ORD775.rpg
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
           SELECT RESTMAS
               ASSIGN TO RESTMAS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS RESTMAS-STATUS
               RECORD KEY IS RESTMAS-KEY1.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT VAREMAS
               ASSIGN TO VAREMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAREMAS-STATUS
               RECORD KEY IS VAREMAS-KEY1.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
           SELECT UTFIL
               ASSIGN TO UT-S-UTFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS UTFIL-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD RESTMAS
               RECORD CONTAINS 160.
       01  RESTMAS-IO-AREA.
           05  RESTMAS-IO-AREA-X.
               10  RESTMAS-KEY1.
                   15  RESTMAS-KEY1N       PICTURE S9(17).
               10  FILLER                  PICTURE X(143).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD VAREMAS
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X.
               10  VAREMAS-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       FD UTFIL
               BLOCK CONTAINS 340
               RECORD CONTAINS 170.
       01  UTFIL-IO-AREA.
           05  UTFIL-IO-AREA-X             PICTURE X(170).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  RESTMAS-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  UTFIL-STATUS                PICTURE 99 VALUE 0.
           10  VLFELT-XX-STATUS            PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  RESTMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  RESTMAS-EOF-OFF         VALUE '0'.
               88  RESTMAS-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESTMAS-READ-OFF        VALUE '0'.
               88  RESTMAS-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESTMAS-PROCESS-OFF     VALUE '0'.
               88  RESTMAS-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  RESTMAS-LEVEL-INIT-OFF  VALUE '0'.
               88  RESTMAS-LEVEL-INIT      VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  VAREMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
      *PSDS: DATA STRUCTURE FIELDS
           05  PSDS-DATA-FIELDS.
               10  FILLER                  PICTURE X(28).
               10  R                       PICTURE X(8).
               10  FILLER                  PICTURE X(44).
           05  FILLER REDEFINES PSDS-DATA-FIELDS.
               10  FILLER                  PICTURE X(36).
               10  P-IO.
                   15  P                   PICTURE S9(3).
               10  FILLER                  PICTURE X(41).
           05  FILLER REDEFINES PSDS-DATA-FIELDS.
               10  FILLER                  PICTURE X(10).
               10  S-IO.
                   15  S                   PICTURE S9(5).
               10  FILLER                  PICTURE X(65).
      *DSDS: DATA STRUCTURE FIELDS
           05  VLFELT-XX-DATA-FIELDS.
               10  VLANT-IO.
                   15  VLANT               PICTURE S9(7)V9(2).
               10  FILLER                  PICTURE X(38).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(9).
               10  VLBEL-IO.
                   15  VLBEL               PICTURE S9(9)V9(2).
               10  FILLER                  PICTURE X(27).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(20).
               10  VLPTIL-IO.
                   15  VLPTIL              PICTURE S9(9)V9(2).
               10  FILLER                  PICTURE X(16).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(31).
               10  VLRAB1-IO.
                   15  VLRAB1              PICTURE S9(2)V9(1).
               10  FILLER                  PICTURE X(13).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(34).
               10  VLRAB2-IO.
                   15  VLRAB2              PICTURE S9(2)V9(1).
               10  FILLER                  PICTURE X(10).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(37).
               10  VLRAB3-IO.
                   15  VLRAB3              PICTURE S9(2)V9(1).
               10  FILLER                  PICTURE X(7).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(40).
               10  VLEDBN-IO.
                   15  VLEDBN              PICTURE S9(7).
           05  RESTMAS-LEVEL-01.
               10  RESTMAS-01-L2.
                   15  RESTMAS-01-L2-FIRMA PICTURE X(3).
               10  RESTMAS-01-L1.
                   15  RESTMAS-01-L1-ORDNR PICTURE X(6).
           05  RESTMAS-DATA-FIELDS.
      *                                       1  17 MSTKEY
               10  FIRMA                   PICTURE X(3).
               10  OTYPE                   PICTURE X(1).
               10  KUNDNR                  PICTURE X(6).
               10  BETM                    PICTURE X(2).
               10  LAGERK                  PICTURE X(2).
               10  EDBNR                   PICTURE X(7).
               10  EDBNRN-IO.
                   15  EDBNRN              PICTURE S9(7).
               10  EDB2F                   PICTURE X(2).
               10  EDB3F                   PICTURE X(3).
               10  ORDNR                   PICTURE X(6).
               10  ORDAAR-IO.
                   15  ORDAAR              PICTURE S9(2).
               10  ORDMND-IO.
                   15  ORDMND              PICTURE S9(2).
               10  ORDDAG-IO.
                   15  ORDDAG              PICTURE S9(2).
      *                                      73  78 MODATO
               10  ANTLEV-IO.
                   15  ANTLEV              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  GALFA                   PICTURE X(3).
               10  GARTNR                  PICTURE X(20).
      *                                     106 135 GVAREB
               10  BEL-IO.
                   15  BEL                 PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  RAB1-IO.
                   15  RAB1                PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  RAB2-IO.
                   15  RAB2                PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  RAB3-IO.
                   15  RAB3                PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  AVD                     PICTURE X(1).
               10  ORDMOT                  PICTURE X(2).
               10  HND                     PICTURE X(3).
      *                                     156 156 STATUS
           05  FIRMAF-DATA-FIELDS.
               10  KONFNR                  PICTURE X(3).
               10  FSTAT                   PICTURE X(1).
           05  VAREMAS-DATA-FIELDS.
               10  SKPRIS-IO.
                   15  SKPRIS              PICTURE S9(7)V9(2).
               10  VGR                     PICTURE X(5).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  ANTORD-IO.
                   15  ANTORD              PICTURE S9(7).
               10  ANTOLR-IO.
                   15  ANTOLR              PICTURE S9(7).
               10  ANTRL2-IO.
                   15  ANTRL2              PICTURE S9(8).
               10  ANTRLR-IO.
                   15  ANTRLR              PICTURE S9(8).
               10  NTOSUM-IO.
                   15  NTOSUM              PICTURE S9(7)V9(2).
               10  KEY10                   PICTURE X(10).
               10  SVS-IO.
                   15  SVS                 PICTURE S9(7)V9(2).
           05  EDITTING-FIELDS.
               10  XO-52D                  PICTURE S9(5)V9(2).
               10  XO-52U                  PICTURE 9(5)V9(2).
               10  XO-72D                  PICTURE S9(7)V9(2).
               10  XO-72U                  PICTURE 9(7)V9(2).
               10  XO-21D                  PICTURE S9(2)V9(1).
               10  XO-21U                  PICTURE 9(2)V9(1).
               10  XO-40YNZ                PICTURE ZZZZ.
               10  XO-80YY9                PICTURE ZZ.ZZZ.ZZ9.
               10  XO-70YY9                PICTURE Z.ZZZ.ZZ9.
           05  PREDEFINED-FIELDS.
               10  PAGE0                   PICTURE S9(4) USAGE BINARY.
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
               88  NOT-SET-I-OF            VALUE '0'.
               88  SET-I-OF                VALUE '1'.
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
 
           PERFORM HEADING-OUTPUT
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
           PERFORM HEADING-OUTPUT
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
           IF  NOT-SET-I-OF
               SET NOT-I-OF                TO TRUE
           END-IF
           SET NOT-SET-I-OF                TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-02                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  RESTMAS-PROCESS
               SET RESTMAS-PROCESS-OFF     TO TRUE
               SET RESTMAS-READ            TO TRUE
           END-IF
 
           IF  RESTMAS-READ
           AND RECORD-SELECTED-OFF
               PERFORM RESTMAS-GET
               SET RESTMAS-READ-OFF        TO TRUE
               IF  NOT RESTMAS-EOF
                   SET RESTMAS-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  RESTMAS-PROCESS
               PERFORM RESTMAS-IDSET
           END-IF
 
           IF  RESTMAS-PROCESS
               PERFORM RESTMAS-CHK-LEVEL
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
           PERFORM HEADING-OVERFLOW
 
           IF  RESTMAS-PROCESS
               PERFORM RESTMAS-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  RESTMAS-PROCESS
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
               SET NOT-I-50                TO TRUE
           END-IF
           IF  (I-01 AND NOT-I-U2)
               SET NOT-I-15                TO TRUE
               IF  FIRMA = '913'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-U2)
               SET NOT-I-15                TO TRUE
               IF  FIRMA = '828'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-15)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-13                    TO TRUE
           IF  KUNDNR = '115967'
               SET I-13                    TO TRUE
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '115968'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '119193'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '119194'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '119195'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '119197'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '119274'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '119276'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '119278'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '119582'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '119674'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '123739'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '131898'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '132510'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '136034'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '500555'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '500560'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '500563'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '500570'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '500580'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '500900'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '500950'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '500951'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '500952'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '500953'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '500954'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '500955'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '500959'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '500961'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '500962'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '500970'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '500956'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '500551'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '500963'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '123992'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '123994'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '123996'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '123993'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '123989'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '123985'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '137575'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '123991'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '128657'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '123986'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '136700'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '128658'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '139458'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '123997'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '123998'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '123990'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '127623'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '119584'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '119673'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '109793'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '109795'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '127622'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '109794'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '123740'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KUNDNR = '500960'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (I-13)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               SET NOT-I-16                TO TRUE
               IF  OTYPE = 'R'
                   SET I-16                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-16)
               SET NOT-I-16                TO TRUE
               IF  OTYPE = 'F'
                   SET I-16                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-16)
               GO TO SLUTT-T
      *****************************************************************
      * OPPSLAG MOT FIRMAFILE HENT KONSERNFIRMANR.                    *
      *****************************************************************
           END-IF
           IF  (I-L2)
               MOVE FIRMA                  TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-11                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-11            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
           END-IF
           IF  (I-L2 AND NOT-I-11)
               SET NOT-I-11                TO TRUE
               IF  FSTAT = 'S'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-11)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-31                    TO TRUE
           IF  FIRMA = '694'
               SET I-31                    TO TRUE
           END-IF
           IF  (NOT-I-31)
               SET NOT-I-31                TO TRUE
               IF  FIRMA = '695'
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-31)
               SET NOT-I-31                TO TRUE
               IF  FIRMA = '696'
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-31)
               SET NOT-I-31                TO TRUE
               IF  FIRMA = '697'
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-31)
               SET NOT-I-31                TO TRUE
               IF  FIRMA = '698'
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-31)
               SET NOT-I-31                TO TRUE
               IF  FIRMA = '699'
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (I-31)
               MOVE '693'                  TO KONFNR
           END-IF
           SET I-50                        TO TRUE
           IF  (I-L1)
               ADD 1                       TO ANTORD
               ADD 1                       TO ANTOLR
           END-IF
           IF  (I-01)
               ADD 1                       TO ANTRL2
               ADD 1                       TO ANTRLR
      *****************************************************************
      *  DIVERSE INDIKATOR SETTING.                                   *
      *****************************************************************
           END-IF
           SET NOT-I-25                    TO TRUE
           IF  LAGERK = 'PT'
               SET I-25                    TO TRUE
           END-IF
           SET NOT-I-27                    TO TRUE
           IF  OTYPE = 'F'
               SET I-27                    TO TRUE
           END-IF
      *****************************************************************
      *    RUTINE FOR BEREGNING AV NETTO VARELINJE BELØP              *
      *    OG NETTOSUM PÅ ALLE LEVELS.                                *
      *    Subrutinen snur ntosum om det er edb-nr. 995xxxx eller     *
      *        edb-nr. 94xxxxx. Dette er allerede gjort i FAK075      *
      *****************************************************************
           SET NOT-I-33                    TO TRUE
           IF  EDB3F = '995'
               SET I-33                    TO TRUE
           END-IF
           IF  (NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  EDB2F = '94'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           ADD ANTLEV TO ZERO          GIVING VLANT
           ADD BEL TO ZERO             GIVING VLBEL
           MOVE 0                          TO VLPTIL
           ADD RAB1 TO ZERO            GIVING VLRAB1
           ADD RAB2 TO ZERO            GIVING VLRAB2
           ADD RAB3 TO ZERO            GIVING VLRAB3
           ADD EDBNRN TO ZERO          GIVING VLEDBN
           IF  (I-33)
               MOVE 0000000                TO VLEDBN
           END-IF
           CALL 'NETTOSUM' USING VLFELT-XX-DATA-FIELDS
           ADD VLBEL TO ZERO           GIVING NTOSUM ROUNDED
      *****************************************************************
      * FAKTURA/KREDITNOTA RUTINE. SNU BELØP TIL MINUS.               *
      *****************************************************************
      *          FAKT      COMP "1"                      21 FAKTURA
      *          FAKT      COMP "2"                      22 KREDIT-NOTA
      *  22      NTOSUM    DIV  -1        NTOSUM  92       NEGATIVT.
      *****************************************************************
      * BEREGNING AV SOLGTE VARERS SELVKOST (SVS).                    *
      *****************************************************************
      *****************************************************************
      * HENTE DATA FRA VAREMASTER.                                   *
      *****************************************************************
           MOVE FIRMA                      TO KEY10 (1:3)
           MOVE EDBNR                      TO KEY10 (4:7)
           MOVE KEY10                      TO VAREMAS-KEY1
           READ VAREMAS RECORD KEY IS VAREMAS-KEY1
           INVALID KEY
               SET I-53                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-53                TO TRUE
               PERFORM VAREMAS-FLDSET
               PERFORM VAREMAS-IDSET
           END-READ
           IF  (NOT-I-53)
               MULTIPLY ANTLEV BY SKPRIS GIVING SVS ROUNDED
           END-IF
           IF  (I-53)
               MOVE 0,00                   TO SVS
      *  22      SVS       MULT -1        SVS              KR.NOTA
           END-IF
           IF  (I-25)
               ADD NTOSUM TO ZERO      GIVING SVS
      *  22N23             Z-ADDNTOSUM    SVS              KR.NOTA F.PRIS/RAB.
           END-IF
           .
 
       SLUTT-T.
      *****************************************************************
      * FAKTURA.SALGDATA FIL MED RESTORDRE OG FORHÅNDSORDRE           *
      *****************************************************************
           CONTINUE.
 
       RESTMAS-GET SECTION.
       RESTMAS-GET-P.
           IF  RESTMAS-EOF-OFF
               READ RESTMAS
               AT END
                   SET RESTMAS-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RESTMAS-FLDSET SECTION.
       RESTMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RESTMAS-IO-AREA (1:3)  TO FIRMA (1:3)
               MOVE RESTMAS-IO-AREA (5:1)  TO OTYPE (1:1)
               MOVE RESTMAS-IO-AREA (6:6)  TO KUNDNR (1:6)
               MOVE RESTMAS-IO-AREA (30:2) TO BETM (1:2)
               MOVE RESTMAS-IO-AREA (33:2) TO LAGERK (1:2)
               MOVE RESTMAS-IO-AREA (41:7) TO EDBNR (1:7)
               MOVE RESTMAS-IO-AREA (41:7) TO EDBNRN-IO
               INSPECT EDBNRN-IO REPLACING ALL ' ' BY '0'
               MOVE RESTMAS-IO-AREA (41:2) TO EDB2F (1:2)
               MOVE RESTMAS-IO-AREA (41:3) TO EDB3F (1:3)
               MOVE RESTMAS-IO-AREA (64:6) TO ORDNR (1:6)
               MOVE RESTMAS-IO-AREA (73:2) TO ORDAAR-IO
               INSPECT ORDAAR-IO REPLACING ALL ' ' BY '0'
               MOVE RESTMAS-IO-AREA (75:2) TO ORDMND-IO
               INSPECT ORDMND-IO REPLACING ALL ' ' BY '0'
               MOVE RESTMAS-IO-AREA (77:2) TO ORDDAG-IO
               INSPECT ORDDAG-IO REPLACING ALL ' ' BY '0'
               MOVE RESTMAS-IO-AREA (79:4) TO ANTLEV-IO
               MOVE RESTMAS-IO-AREA (83:3) TO GALFA (1:3)
               MOVE RESTMAS-IO-AREA (86:20) TO GARTNR (1:20)
               MOVE RESTMAS-IO-AREA (137:5) TO BEL-IO
               MOVE RESTMAS-IO-AREA (142:2) TO RAB1-IO
               MOVE RESTMAS-IO-AREA (144:2) TO RAB2-IO
               MOVE RESTMAS-IO-AREA (146:2) TO RAB3-IO
               MOVE RESTMAS-IO-AREA (148:1) TO AVD (1:1)
               MOVE RESTMAS-IO-AREA (149:2) TO ORDMOT (1:2)
               MOVE RESTMAS-IO-AREA (151:3) TO HND (1:3)
           END-EVALUATE.
 
       RESTMAS-IDSET SECTION.
       RESTMAS-IDSET-P.
           SET I-01                        TO TRUE.
 
       RESTMAS-CHK-LEVEL SECTION.
       RESTMAS-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO RESTMAS-LEVEL-01
               MOVE RESTMAS-IO-AREA (1:3)  TO RESTMAS-01-L2-FIRMA
               MOVE RESTMAS-IO-AREA (64:6) TO RESTMAS-01-L1-ORDNR
               IF  RESTMAS-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  RESTMAS-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  RESTMAS-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  RESTMAS-01-L2         TO THE-PRIOR-L2
               MOVE  RESTMAS-01-L1         TO THE-PRIOR-L1
               SET RESTMAS-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (1:3)   TO KONFNR (1:3)
               MOVE FIRMAF-IO-AREA (123:1) TO FSTAT (1:1)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-03                        TO TRUE.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (66:9) TO SKPRIS-IO
               INSPECT SKPRIS-IO REPLACING ALL ' ' BY '0'
               MOVE VAREMAS-IO-AREA (118:5) TO VGR (1:5)
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-02                        TO TRUE.
 
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
               SET I-OF                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OF            TO TRUE
               END-IF
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
           IF  (I-01 AND I-50)
               MOVE SPACES TO UTFIL-IO-AREA
               INITIALIZE UTFIL-IO-AREA
               MOVE KONFNR                 TO UTFIL-IO-AREA (1:3)
      *                        FAKÅM      9
               MOVE '20'                   TO UTFIL-IO-AREA (4:2)
               MOVE ORDAAR-IO              TO UTFIL-IO-AREA (6:2)
               MOVE ORDMND-IO              TO UTFIL-IO-AREA (8:2)
               MOVE KUNDNR                 TO UTFIL-IO-AREA (10:6)
               MOVE '00000'                TO UTFIL-IO-AREA (16:5)
      *                        ODATO8    28
               MOVE '20'                   TO UTFIL-IO-AREA (21:2)
               MOVE ORDAAR-IO              TO UTFIL-IO-AREA (23:2)
               MOVE ORDMND-IO              TO UTFIL-IO-AREA (25:2)
               MOVE ORDDAG-IO              TO UTFIL-IO-AREA (27:2)
      *                        REGKL     32
               MOVE '    '                 TO UTFIL-IO-AREA (29:4)
               MOVE ORDNR                  TO UTFIL-IO-AREA (33:6)
               MOVE FIRMA                  TO UTFIL-IO-AREA (39:3)
               MOVE BETM                   TO UTFIL-IO-AREA (42:2)
               MOVE LAGERK                 TO UTFIL-IO-AREA (44:2)
               MOVE AVD                    TO UTFIL-IO-AREA (46:1)
      *                      21          47 "F"
      *                      22          47 "K"
               MOVE OTYPE                  TO UTFIL-IO-AREA (47:1)
               IF  (I-27)
                   MOVE 'H'                TO UTFIL-IO-AREA (47:1)
      *                        KRTYPE    48
               END-IF
               MOVE ' '                    TO UTFIL-IO-AREA (48:1)
      *                        MERKN     49
               MOVE ' '                    TO UTFIL-IO-AREA (49:1)
      *                        OKODE     50
               MOVE ' '                    TO UTFIL-IO-AREA (50:1)
               IF  (NOT-I-53)
                   MOVE VGR                TO UTFIL-IO-AREA (51:5)
               END-IF
               IF  (I-53)
                   MOVE '00000'            TO UTFIL-IO-AREA (51:5)
               END-IF
               MOVE HND                    TO UTFIL-IO-AREA (56:3)
      *                        BK        59
               MOVE ' '                    TO UTFIL-IO-AREA (59:1)
               MOVE EDBNRN-IO              TO UTFIL-IO-AREA (60:7)
               MOVE '   '                  TO UTFIL-IO-AREA (67:3)
               MOVE GALFA                  TO UTFIL-IO-AREA (67:3)
               MOVE '                    ' TO UTFIL-IO-AREA (70:20)
               MOVE GARTNR                 TO UTFIL-IO-AREA (70:20)
      *                        ANTBST    96
               MOVE ANTLEV                 TO XO-52U
               MOVE XO-52U (1:7)           TO UTFIL-IO-AREA (90:7)
               IF ANTLEV < 0
                 MOVE ANTLEV               TO XO-52D
                 MOVE XO-52D (1:7)         TO UTFIL-IO-AREA (97:7)
               ELSE
                 MOVE ANTLEV               TO XO-52U
                 MOVE XO-52U (1:7)         TO UTFIL-IO-AREA (97:7)
               END-IF
               MOVE BEL                    TO XO-72U
               MOVE XO-72U (1:9)           TO UTFIL-IO-AREA (104:9)
               MOVE RAB1                   TO XO-21U
               MOVE XO-21U (1:3)           TO UTFIL-IO-AREA (113:3)
               IF RAB2 < 0
                 MOVE RAB2                 TO XO-21D
                 MOVE XO-21D (1:3)         TO UTFIL-IO-AREA (116:3)
               ELSE
                 MOVE RAB2                 TO XO-21U
                 MOVE XO-21U (1:3)         TO UTFIL-IO-AREA (116:3)
               END-IF
               IF RAB3 < 0
                 MOVE RAB3                 TO XO-21D
                 MOVE XO-21D (1:3)         TO UTFIL-IO-AREA (119:3)
               ELSE
                 MOVE RAB3                 TO XO-21U
                 MOVE XO-21U (1:3)         TO UTFIL-IO-AREA (119:3)
               END-IF
               MOVE NTOSUM-IO              TO UTFIL-IO-AREA (122:9)
               MOVE SVS-IO                 TO UTFIL-IO-AREA (131:9)
      *                     N12FAKTNR   145
               MOVE '      '               TO UTFIL-IO-AREA (140:6)
      *                     N12ORDTYP   146
               MOVE ' '                    TO UTFIL-IO-AREA (146:1)
      *                        FNRMND   147
               MOVE ' '                    TO UTFIL-IO-AREA (147:1)
               MOVE ORDMOT                 TO UTFIL-IO-AREA (148:2)
      *                  N12N72PLUKAV   151
               MOVE '  '                   TO UTFIL-IO-AREA (150:2)
               MOVE 'R'                    TO UTFIL-IO-AREA (152:1)
      *                      12         160 "O"
      *****************************************************************
      * KONTROLLISTE.                                                 *
      *****************************************************************
               WRITE UTFIL-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P AND I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'PROG.'                TO LISTE-IO-AREA (2:5)
               MOVE 'ORD775 '              TO LISTE-IO-AREA (7:7)
               MOVE 'DANNE FAKTURA.SALGDATA' TO LISTE-IO-AREA (17:22)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (95:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (106:4)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (110:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-1P AND I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'PROG.'                TO LISTE-IO-AREA (2:5)
               MOVE 'ORD775 '              TO LISTE-IO-AREA (7:7)
               MOVE 'DANNE FAKTURA.SALGDATA' TO LISTE-IO-AREA (17:22)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (95:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (106:4)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (110:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-OF AND NOT-I-L2)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L2 AND I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FIRMA                  TO LISTE-IO-AREA (1:3)
               MOVE FSTAT                  TO LISTE-IO-AREA (5:1)
               MOVE 'ANT.REC'              TO LISTE-IO-AREA (8:7)
               MOVE ANTRL2                 TO XO-80YY9
               MOVE XO-80YY9               TO LISTE-IO-AREA (16:10)
               INITIALIZE ANTRL2
               MOVE 'ANT.ORDRE'            TO LISTE-IO-AREA (31:9)
               MOVE ANTORD                 TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (41:9)
               INITIALIZE ANTORD
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR AND I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '***'                  TO LISTE-IO-AREA (1:3)
               MOVE 'ANT.REC'              TO LISTE-IO-AREA (8:7)
               MOVE ANTRLR                 TO XO-80YY9
               MOVE XO-80YY9               TO LISTE-IO-AREA (16:10)
               MOVE 'ANT.ORDRE'            TO LISTE-IO-AREA (31:9)
               MOVE ANTOLR                 TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (41:9)
               MOVE 1                      TO LISTE-BEFORE-SPACE
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
           SET RESTMAS-LEVEL-INIT          TO TRUE
           INITIALIZE RESTMAS-DATA-FIELDS
           SET RESTMAS-EOF-OFF             TO TRUE
           SET RESTMAS-PROCESS             TO TRUE
           OPEN INPUT RESTMAS
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           INITIALIZE VAREMAS-DATA-FIELDS
           OPEN INPUT VAREMAS
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES
           OPEN OUTPUT UTFIL.
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE RESTMAS
           CLOSE FIRMAF
           CLOSE VAREMAS
           IF LISTE-IO-AREA NOT = SPACES
             WRITE LISTE-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE-IO-AREA
           END-IF
           CLOSE LISTE
           CLOSE UTFIL.
 
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
