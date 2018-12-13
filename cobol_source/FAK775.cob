       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK775R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAM FAK775,  PROGRAMMERER: ESPEN LARSEN                   *
      * DANNER FAKTURA.SALGDATA, OG HENTER DATA FRA FIRMAFILE OG      *
      *        ORDRENR.FILE, SAMT BEREGNER SALGSUM PR. VARELINJE.     *
      * 080217 TOM  FJERNET Å I VAR                                   *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK775.rpg
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
           SELECT FAKVARE
               ASSIGN TO UT-S-FAKVARE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKVARE-STATUS.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT ORDNRM
               ASSIGN TO ORDNRM
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS ORDNRM-STATUS
               RECORD KEY IS ORDNRM-KEY1.
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
       FD FAKVARE
               BLOCK CONTAINS 164
               RECORD CONTAINS 82.
       01  FAKVARE-IO-AREA.
           05  FAKVARE-IO-AREA-X           PICTURE X(82).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD ORDNRM
               RECORD CONTAINS 100.
       01  ORDNRM-IO-AREA.
           05  ORDNRM-IO-AREA-X.
               10  ORDNRM-KEY1             PICTURE X(9).
               10  FILLER                  PICTURE X(91).
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       FD UTFIL
               BLOCK CONTAINS 320
               RECORD CONTAINS 160.
       01  UTFIL-IO-AREA.
           05  UTFIL-IO-AREA-X             PICTURE X(160).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  FAKVARE-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  ORDNRM-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  UTFIL-STATUS                PICTURE 99 VALUE 0.
           10  VLFELT-XX-STATUS            PICTURE 99 VALUE 0.
           10  DATOER-XX-STATUS            PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKVARE-EOF-OFF         VALUE '0'.
               88  FAKVARE-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKVARE-READ-OFF        VALUE '0'.
               88  FAKVARE-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKVARE-PROCESS-OFF     VALUE '0'.
               88  FAKVARE-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  FAKVARE-LEVEL-INIT-OFF  VALUE '0'.
               88  FAKVARE-LEVEL-INIT      VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  ORDNRM-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
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
               10  FILLER                  PICTURE X(71).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(9).
               10  VLBEL-IO.
                   15  VLBEL               PICTURE S9(9)V9(2).
               10  FILLER                  PICTURE X(60).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(20).
               10  VLPTIL-IO.
                   15  VLPTIL              PICTURE S9(9)V9(2).
               10  FILLER                  PICTURE X(49).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(31).
               10  VLRAB1-IO.
                   15  VLRAB1              PICTURE S9(2)V9(1).
               10  FILLER                  PICTURE X(46).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(34).
               10  VLRAB2-IO.
                   15  VLRAB2              PICTURE S9(2)V9(1).
               10  FILLER                  PICTURE X(43).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(37).
               10  VLRAB3-IO.
                   15  VLRAB3              PICTURE S9(2)V9(1).
               10  FILLER                  PICTURE X(40).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(40).
               10  VLEDBN-IO.
                   15  VLEDBN              PICTURE S9(7).
               10  FILLER                  PICTURE X(33).
           05  DATOER-XX REDEFINES VLFELT-XX-DATA-FIELDS.
               10  DATOK                   PICTURE X(1).
               10  FILLER                  PICTURE X(79).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(1).
               10  DATO6                   PICTURE X(6).
               10  FILLER                  PICTURE X(73).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(7).
               10  DMA8                    PICTURE X(8).
               10  FILLER                  PICTURE X(65).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(15).
               10  AMD8                    PICTURE X(8).
               10  FILLER                  PICTURE X(57).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(23).
               10  DATOM                   PICTURE X(57).
      *DSDS: DATA STRUCTURE FIELDS
           05  DATOER-XX-DATA-FIELDS.
               10  DATOK                   PICTURE X(1).
               10  FILLER                  PICTURE X(79).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(1).
               10  DATO6                   PICTURE X(6).
               10  FILLER                  PICTURE X(73).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(7).
               10  DMA8                    PICTURE X(8).
               10  FILLER                  PICTURE X(65).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(15).
               10  AMD8                    PICTURE X(8).
               10  FILLER                  PICTURE X(57).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(23).
               10  DATOM                   PICTURE X(57).
           05  FAKVARE-LEVEL-01.
               10  FAKVARE-01-L2.
                   15  FAKVARE-01-L2-FIRMA PICTURE X(3).
               10  FAKVARE-01-L1.
                   15  FAKVARE-01-L1-ORDNRP PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
           05  FAKVARE-DATA-FIELDS.
               10  AVD                     PICTURE X(1).
               10  SKPRIS-IO.
                   15  SKPRIS              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ANTBST-IO.
                   15  ANTBST              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ANTLEV-IO.
                   15  ANTLEV              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  EDBNR-IO.
                   15  EDBNR               PICTURE S9(7).
               10  EDB2F                   PICTURE X(2).
               10  EDB3F                   PICTURE X(3).
               10  RAB1-IO.
                   15  RAB1                PICTURE S9(2)V9(1).
               10  RAB2-IO.
                   15  RAB2                PICTURE S9(2)V9(1).
               10  RAB3-IO.
                   15  RAB3                PICTURE S9(2)V9(1).
               10  BEL-IO.
                   15  BEL                 PICTURE S9(7)V9(2).
               10  FAKT                    PICTURE X(1).
               10  FAKMND                  PICTURE X(2).
               10  BK                      PICTURE X(1).
               10  KUNDNR                  PICTURE X(6).
               10  FIRMA                   PICTURE X(3).
               10  DISTNR                  PICTURE X(3).
               10  OKODE                   PICTURE X(1).
               10  FAKAAR                  PICTURE X(2).
               10  VGR                     PICTURE X(5).
               10  MERKN                   PICTURE X(1).
               10  KRTYPE                  PICTURE X(1).
               10  FNRMND                  PICTURE X(1).
               10  LAGERK                  PICTURE X(2).
               10  ORDNRP-IO.
                   15  ORDNRP              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  ODATO-IO.
                   15  ODATO               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  BETM                    PICTURE X(2).
           05  FIRMAF-DATA-FIELDS.
               10  KONFNR                  PICTURE X(3).
               10  FSTAT                   PICTURE X(1).
           05  ORDNRM-DATA-FIELDS.
               10  FAKTNR                  PICTURE X(6).
               10  ORTIDP-IO.
                   15  ORTIDP              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  ORDMOT                  PICTURE X(2).
               10  PLUKAV                  PICTURE X(2).
               10  ORDTYP                  PICTURE X(1).
               10  PAKKAV                  PICTURE X(2).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(4).
           05  TEMPORARY-FIELDS.
               10  ANTORD-IO.
                   15  ANTORD              PICTURE S9(7).
               10  ANTOLR-IO.
                   15  ANTOLR              PICTURE S9(7).
               10  ANTRL2-IO.
                   15  ANTRL2              PICTURE S9(8).
               10  ANTRLR-IO.
                   15  ANTRLR              PICTURE S9(8).
               10  ORDNRN-IO.
                   15  ORDNRN              PICTURE S9(6).
               10  ORDNRP-N-IO.
                   15  ORDNRP-N            PICTURE S9(7).
               10  ORDNR                   PICTURE X(6).
               10  FNRONR                  PICTURE X(9).
               10  ONRIF-IO.
                   15  ONRIF               PICTURE S9(7).
               10  ONRIFL-IO.
                   15  ONRIFL              PICTURE S9(7).
               10  MA-ELG                  PICTURE X(4).
               10  DMA-ELG                 PICTURE X(6).
               10  FAKA-ELGM               PICTURE X(6).
               10  ODATON-IO.
                   15  ODATON              PICTURE S9(6).
               10  ODATO-N-IO.
                   15  ODATO-N             PICTURE S9(7).
               10  ODATO8                  PICTURE X(8).
               10  ODATO6                  PICTURE X(6).
               10  REGKLN-IO.
                   15  REGKLN              PICTURE S9(6).
               10  ORTIDP-N-IO.
                   15  ORTIDP-N            PICTURE S9(7).
               10  REGKL                   PICTURE X(4).
               10  NTOSUM-IO.
                   15  NTOSUM              PICTURE S9(7)V9(2).
               10  SVS-IO.
                   15  SVS                 PICTURE S9(7)V9(2).
               10  ORDAT2                  PICTURE X(8).
           05  EDITTING-FIELDS.
               10  XO-52D                  PICTURE S9(5)V9(2).
               10  XO-52U                  PICTURE 9(5)V9(2).
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
           SET NOT-I-04                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  FAKVARE-PROCESS
               SET FAKVARE-PROCESS-OFF     TO TRUE
               SET FAKVARE-READ            TO TRUE
           END-IF
 
           IF  FAKVARE-READ
           AND RECORD-SELECTED-OFF
               PERFORM FAKVARE-GET
               SET FAKVARE-READ-OFF        TO TRUE
               IF  NOT FAKVARE-EOF
                   SET FAKVARE-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  FAKVARE-PROCESS
               PERFORM FAKVARE-IDSET
           END-IF
 
           IF  FAKVARE-PROCESS
               PERFORM FAKVARE-CHK-LEVEL
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
 
           IF  FAKVARE-PROCESS
               PERFORM FAKVARE-FLDOFF
               PERFORM FAKVARE-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  FAKVARE-PROCESS
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
      * OPPSLAG MOT ORDRENUMMER MASTER.                               *
      *****************************************************************
           END-IF
           IF  (I-L1)
               MOVE ORDNRP                 TO ORDNRP-N
               MOVE ORDNRP-N-IO (2:6)      TO ORDNRN-IO
               MOVE ORDNRN                 TO ORDNR
      ** MLLzo
               MOVE '1'                    TO BW-A-2
               MULTIPLY 16                 BY BW-A
               MOVE ORDNR (6:1)            TO BW-B-2
               MULTIPLY 16                 BY BW-B
               MOVE BW-A-1                 TO BW-B-1
               DIVIDE 16                   INTO BW-B
               MOVE BW-B-2                 TO ORDNR (6:1)
               SET NOT-I-20                TO TRUE
               IF  ORDNR = '000000'
                   SET I-20                TO TRUE
               END-IF
               MOVE FIRMA                  TO FNRONR (1:3)
               MOVE ORDNR                  TO FNRONR (4:6)
           END-IF
           IF  (I-L1 AND NOT-I-20)
               MOVE FNRONR                 TO ORDNRM-KEY1
               READ ORDNRM RECORD KEY IS ORDNRM-KEY1
               INVALID KEY
                   SET I-12                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-12            TO TRUE
                   PERFORM ORDNRM-FLDSET
                   PERFORM ORDNRM-IDSET
               END-READ
           END-IF
           IF  (I-L1 AND I-12 AND NOT-I-20)
               ADD 1                       TO ONRIF
               ADD 1                       TO ONRIFL
           END-IF
           IF  (I-L1 AND I-20)
               SET I-12                    TO TRUE
           END-IF
           IF  (I-L1 AND NOT-I-12)
               SET NOT-I-71                TO TRUE
               IF  ORDMOT < '  '
                   SET I-71                TO TRUE
               END-IF
               SET NOT-I-72                TO TRUE
               IF  PLUKAV < '  '
                   SET I-72                TO TRUE
               END-IF
      *****************************************************************
      *  DIVERSE INDIKATOR SETTING.                                   *
      *****************************************************************
           END-IF
           SET NOT-I-23                    TO TRUE
           IF  KRTYPE = '2'
               SET I-23                    TO TRUE
           END-IF
           IF  (NOT-I-23)
               SET NOT-I-23                TO TRUE
               IF  KRTYPE = '5'
                   SET I-23                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-23)
               SET NOT-I-23                TO TRUE
               IF  KRTYPE = '6'
                   SET I-23                TO TRUE
               END-IF
           END-IF
           SET NOT-I-25                    TO TRUE
           IF  LAGERK = 'PT'
               SET I-25                    TO TRUE
           END-IF
      *****************************************************************
      * RUTINE FOR Å EDITERE ØVRIGE FELT.                             *
      *****************************************************************
           IF  (I-L1)
               MOVE FAKMND                 TO MA-ELG (1:2)
               MOVE FAKAAR                 TO MA-ELG (3:2)
               MOVE MA-ELG                 TO DMA-ELG (3:4)
               MOVE '15'                   TO DMA-ELG (1:2)
               PERFORM DTOSNU-S
           END-IF
           IF  (I-L1)
               MOVE AMD8 (1:6)             TO FAKA-ELGM
      *****************************************************************
           END-IF
           IF  (I-L1)
               MOVE ODATO                  TO ODATO-N
               MOVE ODATO-N-IO (2:6)       TO ODATON-IO
               MOVE ODATON                 TO DMA-ELG
      ** MLLzo
               MOVE '1'                    TO BW-A-2
               MULTIPLY 16                 BY BW-A
               MOVE DMA-ELG (6:1)          TO BW-B-2
               MULTIPLY 16                 BY BW-B
               MOVE BW-A-1                 TO BW-B-1
               DIVIDE 16                   INTO BW-B
               MOVE BW-B-2                 TO DMA-ELG (6:1)
               PERFORM DTOSNU-S
           END-IF
           IF  (I-L1)
               MOVE AMD8                   TO ODATO8
               MOVE AMD8 (3:6)             TO ODATO6
      *****************************************************************
           END-IF
           IF  (I-L1 AND NOT-I-12)
               MOVE ORTIDP                 TO ORTIDP-N
               MOVE ORTIDP-N-IO (2:6)      TO REGKLN-IO
               MOVE REGKLN (1:4)           TO REGKL
      ** MLLzo
               MOVE '1'                    TO BW-A-2
               MULTIPLY 16                 BY BW-A
               MOVE REGKL (4:1)            TO BW-B-2
               MULTIPLY 16                 BY BW-B
               MOVE BW-A-1                 TO BW-B-1
               DIVIDE 16                   INTO BW-B
               MOVE BW-B-2                 TO REGKL (4:1)
           END-IF
           IF  (I-L1 AND I-12)
               MOVE '0000'                 TO REGKL
      *****************************************************************
      * RETTE ANTALL BEST. OG LEVERT PÅ RENTENOTA TIL 1.              *
      * KONTANTSALG KAN OGSÅ HA 0 I ANTALL. DETTE SKAL BEHANDLES SOM  *
      * NULL. DA DET ER SLIK DET BEHANDLES I KONTANTFAKTURARUTINEN.   *
      * RENTENOTA BLIR SATT TIL 1 I ANTALL I FAK075 (14.01.2004)      *
      *****************************************************************
           END-IF
           SET NOT-I-27                    TO TRUE
           IF  ODATO6 = ORDNR
               SET I-27                    TO TRUE
           END-IF
           IF  (I-27 AND I-07 AND I-08)
               MOVE 1,00                   TO ANTBST-IO (2:6)
               MOVE 1,00                   TO ANTLEV-IO (2:6)
      *****************************************************************
      *    RUTINE FOR BEREGNING AV NETTO VARELINJE BELØP              *
      *    OG NETTOSUM PÅ ALLE LEVELS.                                *
      *    Subrutinen snur ntosum om det er edb-nr. 995xxxx eller     *
      *        edb-nr. 94xxxxx. Dette er allerede gjort i FAK075      *
      *****************************************************************
           END-IF
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
           ADD EDBNR TO ZERO           GIVING VLEDBN
           IF  (I-33)
               MOVE 0000000                TO VLEDBN
           END-IF
           CALL 'NETTOSUM' USING VLFELT-XX-DATA-FIELDS
           ADD VLBEL TO ZERO           GIVING NTOSUM ROUNDED
      *****************************************************************
      * FAKTURA/KREDITNOTA RUTINE. SNU BELØP TIL MINUS.               *
      *****************************************************************
           SET NOT-I-21                    TO TRUE
           IF  FAKT = '1'
               SET I-21                    TO TRUE
           END-IF
           SET NOT-I-22                    TO TRUE
           IF  FAKT = '2'
               SET I-22                    TO TRUE
           END-IF
           IF  (I-22)
               DIVIDE NTOSUM BY -1     GIVING NTOSUM
      *****************************************************************
      * BEREGNING AV SOLGTE VARERS SELVKOST (SVS).                    *
      *****************************************************************
           END-IF
           MULTIPLY ANTLEV BY SKPRIS   GIVING SVS ROUNDED
           IF  (I-22)
               MULTIPLY -1 BY SVS      GIVING SVS
           END-IF
           IF  (I-25)
               ADD NTOSUM TO ZERO      GIVING SVS
           END-IF
           IF  (I-22 AND NOT-I-23)
               ADD NTOSUM TO ZERO      GIVING SVS
           END-IF.
 
       SLUTT-T.
      *****************************************************************
      *                                                               *
      * SUBRUTINE FOR Å SNU ORDREDATO TIL ÅR MND DAG OG 4 SIFFERET ÅR *
      *****************************************************************
           CONTINUE.
 
       DTOSNU-S SECTION.
       DTOSNU-S-P.
      *
           MOVE 'A'                        TO DATOK
           MOVE DMA-ELG                    TO DATO6
           CALL 'DATO8SIF' USING DATOER-XX-DATA-FIELDS
           MOVE AMD8                       TO ORDAT2.
      *****************************************************************
      * FAKTURA.SALGDATA                                              *
      *****************************************************************
 
       FAKVARE-GET SECTION.
       FAKVARE-GET-P.
           IF  FAKVARE-EOF-OFF
               READ FAKVARE
               AT END
                   SET FAKVARE-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FAKVARE-FLDOFF SECTION.
       FAKVARE-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-07                TO TRUE
               SET NOT-I-08                TO TRUE
           END-EVALUATE.
 
       FAKVARE-FLDSET SECTION.
       FAKVARE-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKVARE-IO-AREA (2:1)  TO AVD (1:1)
               MOVE FAKVARE-IO-AREA (3:5)  TO SKPRIS-IO
               MOVE FAKVARE-IO-AREA (8:4)  TO ANTBST-IO
               IF  ANTBST = ZERO
                   SET I-07                TO TRUE
               END-IF
               MOVE FAKVARE-IO-AREA (12:4) TO ANTLEV-IO
               IF  ANTLEV = ZERO
                   SET I-08                TO TRUE
               END-IF
               MOVE FAKVARE-IO-AREA (16:7) TO EDBNR-IO
               INSPECT EDBNR-IO REPLACING ALL ' ' BY '0'
               MOVE FAKVARE-IO-AREA (16:2) TO EDB2F (1:2)
               MOVE FAKVARE-IO-AREA (16:3) TO EDB3F (1:3)
               MOVE FAKVARE-IO-AREA (23:3) TO RAB1-IO
               INSPECT RAB1-IO REPLACING ALL ' ' BY '0'
               MOVE FAKVARE-IO-AREA (26:3) TO RAB2-IO
               INSPECT RAB2-IO REPLACING ALL ' ' BY '0'
               MOVE FAKVARE-IO-AREA (29:3) TO RAB3-IO
               INSPECT RAB3-IO REPLACING ALL ' ' BY '0'
               MOVE FAKVARE-IO-AREA (32:9) TO BEL-IO
               INSPECT BEL-IO REPLACING ALL ' ' BY '0'
               MOVE FAKVARE-IO-AREA (41:1) TO FAKT (1:1)
               MOVE FAKVARE-IO-AREA (42:2) TO FAKMND (1:2)
               MOVE FAKVARE-IO-AREA (44:1) TO BK (1:1)
               MOVE FAKVARE-IO-AREA (45:6) TO KUNDNR (1:6)
               MOVE FAKVARE-IO-AREA (51:3) TO FIRMA (1:3)
               MOVE FAKVARE-IO-AREA (54:3) TO DISTNR (1:3)
               MOVE FAKVARE-IO-AREA (57:1) TO OKODE (1:1)
               MOVE FAKVARE-IO-AREA (58:2) TO FAKAAR (1:2)
               MOVE FAKVARE-IO-AREA (60:5) TO VGR (1:5)
               MOVE FAKVARE-IO-AREA (65:1) TO MERKN (1:1)
               MOVE FAKVARE-IO-AREA (66:1) TO KRTYPE (1:1)
               MOVE FAKVARE-IO-AREA (68:1) TO FNRMND (1:1)
               MOVE FAKVARE-IO-AREA (69:2) TO LAGERK (1:2)
               MOVE FAKVARE-IO-AREA (71:4) TO ORDNRP-IO
               MOVE FAKVARE-IO-AREA (77:4) TO ODATO-IO
               MOVE FAKVARE-IO-AREA (81:2) TO BETM (1:2)
           END-EVALUATE.
 
       FAKVARE-IDSET SECTION.
       FAKVARE-IDSET-P.
           SET I-01                        TO TRUE.
 
       FAKVARE-CHK-LEVEL SECTION.
       FAKVARE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO FAKVARE-LEVEL-01
               MOVE FAKVARE-IO-AREA (51:3) TO FAKVARE-01-L2-FIRMA
               MOVE FAKVARE-IO-AREA (71:4) TO FAKVARE-01-L1-ORDNRP
               IF  FAKVARE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  FAKVARE-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  FAKVARE-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  FAKVARE-01-L2         TO THE-PRIOR-L2
               MOVE  FAKVARE-01-L1         TO THE-PRIOR-L1
               SET FAKVARE-LEVEL-INIT      TO TRUE
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
 
       ORDNRM-FLDSET SECTION.
       ORDNRM-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE ORDNRM-IO-AREA (18:6)  TO FAKTNR (1:6)
               MOVE ORDNRM-IO-AREA (35:4)  TO ORTIDP-IO
               MOVE ORDNRM-IO-AREA (66:2)  TO ORDMOT (1:2)
               MOVE ORDNRM-IO-AREA (68:2)  TO PLUKAV (1:2)
               MOVE ORDNRM-IO-AREA (76:1)  TO ORDTYP (1:1)
               MOVE ORDNRM-IO-AREA (90:2)  TO PAKKAV (1:2)
           END-EVALUATE.
 
       ORDNRM-IDSET SECTION.
       ORDNRM-IDSET-P.
           SET I-04                        TO TRUE.
 
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
               MOVE FAKA-ELGM              TO UTFIL-IO-AREA (4:6)
               MOVE KUNDNR                 TO UTFIL-IO-AREA (10:6)
               MOVE '00000'                TO UTFIL-IO-AREA (16:5)
               MOVE ODATO8                 TO UTFIL-IO-AREA (21:8)
               MOVE REGKL                  TO UTFIL-IO-AREA (29:4)
               MOVE ORDNR                  TO UTFIL-IO-AREA (33:6)
               MOVE FIRMA                  TO UTFIL-IO-AREA (39:3)
               MOVE BETM                   TO UTFIL-IO-AREA (42:2)
               MOVE LAGERK                 TO UTFIL-IO-AREA (44:2)
               MOVE AVD                    TO UTFIL-IO-AREA (46:1)
               IF  (I-21)
                   MOVE 'F'                TO UTFIL-IO-AREA (47:1)
               END-IF
               IF  (I-22)
                   MOVE 'K'                TO UTFIL-IO-AREA (47:1)
               END-IF
               MOVE KRTYPE                 TO UTFIL-IO-AREA (48:1)
               MOVE MERKN                  TO UTFIL-IO-AREA (49:1)
               MOVE OKODE                  TO UTFIL-IO-AREA (50:1)
               MOVE VGR                    TO UTFIL-IO-AREA (51:5)
               MOVE DISTNR                 TO UTFIL-IO-AREA (56:3)
               MOVE BK                     TO UTFIL-IO-AREA (59:1)
               MOVE EDBNR-IO               TO UTFIL-IO-AREA (60:7)
               MOVE '   '                  TO UTFIL-IO-AREA (67:3)
               MOVE '                    ' TO UTFIL-IO-AREA (70:20)
               MOVE ANTBST                 TO XO-52U
               MOVE XO-52U (1:7)           TO UTFIL-IO-AREA (90:7)
               IF ANTLEV < 0
                 MOVE ANTLEV               TO XO-52D
                 MOVE XO-52D (1:7)         TO UTFIL-IO-AREA (97:7)
               ELSE
                 MOVE ANTLEV               TO XO-52U
                 MOVE XO-52U (1:7)         TO UTFIL-IO-AREA (97:7)
               END-IF
               MOVE BEL-IO                 TO UTFIL-IO-AREA (104:9)
               MOVE RAB1-IO                TO UTFIL-IO-AREA (113:3)
               MOVE RAB2-IO                TO UTFIL-IO-AREA (116:3)
               MOVE RAB3-IO                TO UTFIL-IO-AREA (119:3)
               MOVE NTOSUM-IO              TO UTFIL-IO-AREA (122:9)
               MOVE SVS-IO                 TO UTFIL-IO-AREA (131:9)
               IF  (NOT-I-12)
                   MOVE FAKTNR             TO UTFIL-IO-AREA (140:6)
               END-IF
               IF  (NOT-I-12)
                   MOVE ORDTYP             TO UTFIL-IO-AREA (146:1)
               END-IF
               MOVE FNRMND                 TO UTFIL-IO-AREA (147:1)
               IF  (NOT-I-12 AND NOT-I-71)
                   MOVE ORDMOT             TO UTFIL-IO-AREA (148:2)
               END-IF
               IF  (NOT-I-12 AND NOT-I-72)
                   MOVE PLUKAV             TO UTFIL-IO-AREA (150:2)
               END-IF
               MOVE 'F'                    TO UTFIL-IO-AREA (152:1)
               IF  (I-12)
                   MOVE 'O'                TO UTFIL-IO-AREA (160:1)
      *****************************************************************
      * KONTROLLISTE.                                                 *
      *****************************************************************
               END-IF
               WRITE UTFIL-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P AND I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'PROG.'                TO LISTE-IO-AREA (2:5)
               MOVE 'FAK775 '              TO LISTE-IO-AREA (7:7)
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
               MOVE 'FAK775 '              TO LISTE-IO-AREA (7:7)
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
               MOVE 'ANT. IKKE I ORDNRM'   TO LISTE-IO-AREA (53:18)
               MOVE ONRIF                  TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (71:9)
               INITIALIZE ONRIF
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
               MOVE 'ANT. IKKE I ORDNRM'   TO LISTE-IO-AREA (53:18)
               MOVE ONRIFL                 TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (71:9)
               INITIALIZE ONRIFL
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
           SET FAKVARE-LEVEL-INIT          TO TRUE
           INITIALIZE FAKVARE-DATA-FIELDS
           SET FAKVARE-EOF-OFF             TO TRUE
           SET FAKVARE-PROCESS             TO TRUE
           OPEN INPUT FAKVARE
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           INITIALIZE ORDNRM-DATA-FIELDS
           OPEN INPUT ORDNRM
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES
           OPEN OUTPUT UTFIL.
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FAKVARE
           CLOSE FIRMAF
           CLOSE ORDNRM
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
