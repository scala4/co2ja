       IDENTIFICATION DIVISION.
       PROGRAM-ID. RKO003R.
      * NY VERSJON AV RSK.RSK003                 ***TXT***OK MT
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM.......: RKO003 - FØR SEPT.05-RSK003                  *
      *  BRUKES I DOP.XDOP12UD                                        *
      *E 18.01.00 SUMMERER BELØP PÅ NY KONTOKURANT                    *
      *E 15.04.11 TATT MED UTVIDET BELØPSFELT                         *
      *E 03.10.12 SKRIVER AVSTEMMINGSFIL
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RKO003.rpg
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
           SELECT NYKTO
               ASSIGN TO UT-S-NYKTO
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS NYKTO-STATUS.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT GMLKTO
               ASSIGN TO UT-S-GMLKTO
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS GMLKTO-STATUS.
           SELECT AVSTEMM
               ASSIGN TO UT-S-AVSTEMM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS AVSTEMM-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD NYKTO
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  NYKTO-IO-AREA.
           05  NYKTO-IO-AREA-X             PICTURE X(200).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD GMLKTO
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  GMLKTO-IO-AREA.
           05  GMLKTO-IO-AREA-X            PICTURE X(200).
       FD AVSTEMM
               BLOCK CONTAINS 120
               RECORD CONTAINS 120.
       01  AVSTEMM-IO-AREA.
           05  AVSTEMM-IO-AREA-X           PICTURE X(120).
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
           10  NYKTO-STATUS                PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  GMLKTO-STATUS               PICTURE 99 VALUE 0.
           10  AVSTEMM-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  VALPAR-XX-STATUS            PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  NYKTO-EOF-OFF           VALUE '0'.
               88  NYKTO-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  NYKTO-READ-OFF          VALUE '0'.
               88  NYKTO-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  NYKTO-PROCESS-OFF       VALUE '0'.
               88  NYKTO-PROCESS           VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  NYKTO-LEVEL-INIT-OFF    VALUE '0'.
               88  NYKTO-LEVEL-INIT        VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
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
      * * START - DATASTRUKTUR FOR SUB-PROGRAM ADVALUTA ********
      *DSDS: DATA STRUCTURE FIELDS
           05  VALPAR-XX-DATA-FIELDS.
               10  AKSEKD                  PICTURE X(1).
               10  FILLER                  PICTURE X(79).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(1).
               10  ADVK                    PICTURE X(1).
               10  FILLER                  PICTURE X(78).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(2).
               10  STDVK                   PICTURE X(3).
               10  FILLER                  PICTURE X(75).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(5).
               10  NORVN                   PICTURE X(20).
               10  FILLER                  PICTURE X(55).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(25).
               10  STDVN                   PICTURE X(20).
               10  FILLER                  PICTURE X(35).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(45).
               10  LANDKD                  PICTURE X(2).
               10  FILLER                  PICTURE X(33).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(47).
               10  LAND                    PICTURE X(20).
               10  FILLER                  PICTURE X(13).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(67).
               10  VALIX-IO.
                   15  VALIX               PICTURE S9(3).
               10  FILLER                  PICTURE X(10).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(70).
               10  DIV                     PICTURE X(10).
      * * END - DATASTRUKTUR FOR SUB-PROGRAM ADVALUTA ********
           05  NYKTO-LEVEL-01.
               10  NYKTO-01-L1.
                   15  NYKTO-01-L1-FIRMA   PICTURE X(3).
           05  NYKTO-LEVEL-02.
               10  NYKTO-02-L1.
                   15  NYKTO-02-L1-FIRMA   PICTURE X(3).
           05  NYKTO-DATA-FIELDS.
               10  SREC                    PICTURE X(200).
               10  SBEL-IO.
                   15  SBEL                PICTURE S9(8)V9(2).
               10  SVBEL-IO.
                   15  SVBEL               PICTURE S9(9)V9(2).
               10  VT                      PICTURE X(1).
               10  SBELU-IO.
                   15  SBELU               PICTURE S9(11)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  SVBELU-IO.
                   15  SVBELU              PICTURE S9(11)V9(4) USAGE
                                                       PACKED-DECIMAL.
               10  FIRMA                   PICTURE X(3).
               10  KREC                    PICTURE X(200).
               10  KBEL-IO.
                   15  KBEL                PICTURE S9(7)V9(2).
               10  KVBEL-IO.
                   15  KVBEL               PICTURE S9(8)V9(2).
               10  KBELU-IO.
                   15  KBELU               PICTURE S9(11)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  KVBELU-IO.
                   15  KVBELU              PICTURE S9(11)V9(4) USAGE
                                                       PACKED-DECIMAL.
           05  FIRMAF-DATA-FIELDS.
               10  FIRMSL                  PICTURE X(1).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  DATO-IO.
                   15  DATO                PICTURE S9(6).
               10  PRDDTO-IO.
                   15  PRDDTO              PICTURE S9(8).
               10  PRDKLK-IO.
                   15  PRDKLK              PICTURE S9(6).
               10  TIDSP-IO.
                   15  TIDSP               PICTURE S9(6).
               10  TOTREC-IO.
                   15  TOTREC              PICTURE S9(6).
               10  ANT-IO.
                   15  ANT                 PICTURE S9(6).
               10  BEL132-IO.
                   15  BEL132              PICTURE S9(11)V9(2).
               10  BEL154-IO.
                   15  BEL154              PICTURE S9(11)V9(4).
               10  SLEBEL-IO.
                   15  SLEBEL              PICTURE S9(9)V9(2).
               10  TOTBEL-IO.
                   15  TOTBEL              PICTURE S9(9)V9(2).
               10  SLEBEV-IO.
                   15  SLEBEV              PICTURE S9(11)V9(2).
               10  TOTBEV-IO.
                   15  TOTBEV              PICTURE S9(11)V9(2).
               10  SLEBEU-IO.
                   15  SLEBEU              PICTURE S9(11)V9(2).
               10  TOTBEU-IO.
                   15  TOTBEU              PICTURE S9(11)V9(2).
               10  SLEBVU-IO.
                   15  SLEBVU              PICTURE S9(11)V9(4).
               10  TOTBVU-IO.
                   15  TOTBVU              PICTURE S9(11)V9(4).
               10  VALTYP                  PICTURE X(3).
               10  LRRSAN-IO.
                   15  LRRSAN              PICTURE S9(13).
               10  LRRSBE-IO.
                   15  LRRSBE              PICTURE S9(13)V9(2).
               10  LERSBU-IO.
                   15  LERSBU              PICTURE S9(13)V9(2).
           05  EDITTING-FIELDS.
               10  XO-112P-EF.
                 15  XO-112P               PICTURE S9(11)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-114P-EF.
                 15  XO-114P               PICTURE S9(11)V9(4) USAGE
                                                       PACKED-DECIMAL.
               10  XO-80P-EF.
                 15  XO-80P                PICTURE S9(8) USAGE
                                                       PACKED-DECIMAL.
               10  XO-60P-EF.
                 15  XO-60P                PICTURE S9(6) USAGE
                                                       PACKED-DECIMAL.
               10  XO-60YNZ                PICTURE ZZZZZZ.
               10  XO-92YY9R               PICTURE ZZZ.ZZZ.ZZZ,99-.
               10  XO-112YY9R              PICTURE ZZ.ZZZ.ZZZ.ZZZ,99-.
               10  XO-114YY9R              PICTURE
                                                 ZZ.ZZZ.ZZZ.ZZZ,9999-.
               10  XO-60YY9                PICTURE ZZZ.ZZ9.
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
           SET NOT-I-08                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  NYKTO-PROCESS
               SET NYKTO-PROCESS-OFF       TO TRUE
               SET NYKTO-READ              TO TRUE
           END-IF
 
           IF  NYKTO-READ
           AND RECORD-SELECTED-OFF
               PERFORM NYKTO-GET
               SET NYKTO-READ-OFF          TO TRUE
               IF  NOT NYKTO-EOF
                   PERFORM NYKTO-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET NYKTO-PROCESS       TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  NYKTO-PROCESS
               PERFORM NYKTO-IDSET
           END-IF
 
           IF  NYKTO-PROCESS
               PERFORM NYKTO-CHK-LEVEL
           END-IF
 
           IF I-1ST
               GO TO LR-INDICATOR-TEST
           END-IF.
 
       MAINLINE-TOTAL-CALCS.
           IF  I-LR
               PERFORM LR-CALCS
           END-IF
           PERFORM TOTAL-OUTPUT.
 
       LR-INDICATOR-TEST.
           IF  I-LR
               GO TO MAINLINE-TERMINATION
           END-IF
 
           IF  NYKTO-PROCESS
               PERFORM NYKTO-FLDOFF
               PERFORM NYKTO-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  NYKTO-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (NOT-I-99)
               MOVE UDATE                  TO DATO-IO
               MOVE UYEAR                  TO DATO (1:2)
               MOVE UDAY                   TO DATO-IO (5:2)
               SET NOT-I-31                TO TRUE
               IF  UYEAR > 80
                   SET I-31                TO TRUE
               END-IF
               MOVE DATO                   TO PRDDTO-IO (3:6)
           END-IF
           IF  (NOT-I-99 AND I-31)
               MOVE '19'                   TO PRDDTO (1:2)
           END-IF
           IF  (NOT-I-99 AND NOT-I-31)
               MOVE '20'                   TO PRDDTO (1:2)
           END-IF
           IF  (NOT-I-99)
               ACCEPT SYSTEM-TIME-X      FROM TIME
               MOVE SYSTEM-TIME            TO PRDKLK (1:6)
               SET I-99                    TO TRUE
           END-IF
           IF  (I-L1)
               PERFORM FISLET-S
           END-IF
           IF  (I-L1)
               ACCEPT SYSTEM-TIME-X      FROM TIME
               MOVE SYSTEM-TIME            TO TIDSP (1:6)
      * * *   S A L D O R E C O R D   * * *
           END-IF
           IF  (I-01 AND NOT-I-98 AND NOT-I-05)
               ADD 1                       TO TOTREC
           END-IF
           IF  (I-01 AND I-05)
               OR  (I-01 AND I-98)
               ADD 1                       TO ANT
           END-IF
           IF  (I-01 AND NOT-I-98 AND I-U1)
               ADD SBEL TO ZERO        GIVING BEL132
               ADD SVBEL TO ZERO       GIVING BEL154
           END-IF
           IF  (I-02 AND NOT-I-98 AND I-U1)
               ADD KBEL TO ZERO        GIVING BEL132
               ADD KVBEL TO ZERO       GIVING BEL154
           END-IF
           IF  (I-01 AND I-05)
               OR  (I-01 AND I-98)
               ADD SBEL                    TO SLEBEL
           END-IF
           IF  (I-01 AND NOT-I-98)
               ADD SBEL                    TO TOTBEL
           END-IF
           IF  (I-01 AND I-05)
               OR  (I-01 AND I-98)
               ADD SVBEL                   TO SLEBEV
           END-IF
           IF  (I-01 AND NOT-I-98)
               ADD SVBEL                   TO TOTBEV
           END-IF
           IF  (I-01 AND I-05)
               OR  (I-01 AND I-98 AND NOT-I-U1)
               ADD SBELU                   TO SLEBEU
           END-IF
           IF  (I-01 AND NOT-I-98 AND NOT-I-U1)
               ADD SBELU                   TO TOTBEU
           END-IF
           IF  (I-01 AND I-05)
               OR  (I-01 AND I-98 AND NOT-I-U1)
               ADD SVBELU                  TO SLEBVU
           END-IF
           IF  (I-01 AND NOT-I-98 AND NOT-I-U1)
               ADD SVBELU                  TO TOTBVU
      * * *   T R A N S R E C O R D   * * *
           END-IF
           IF  (I-02 AND NOT-I-98)
               ADD 1                       TO TOTREC
           END-IF
           IF  (I-02 AND I-98)
               ADD 1                       TO ANT
           END-IF
           IF  (I-02 AND NOT-I-98)
               ADD KBEL                    TO TOTBEL
           END-IF
           IF  (I-02 AND I-98)
               ADD KBEL                    TO SLEBEL
           END-IF
           IF  (I-02 AND NOT-I-98)
               ADD KVBEL                   TO TOTBEV
           END-IF
           IF  (I-02 AND I-98)
               ADD KVBEL                   TO SLEBEV
           END-IF
           IF  (I-02 AND NOT-I-98 AND NOT-I-U1)
               ADD KBELU                   TO TOTBEU
           END-IF
           IF  (I-02 AND I-98 AND NOT-I-U1)
               ADD KBELU                   TO SLEBEU
           END-IF
           IF  (I-02 AND NOT-I-98 AND NOT-I-U1)
               ADD KVBELU                  TO TOTBVU
           END-IF
           IF  (I-02 AND I-98 AND NOT-I-U1)
               ADD KVBELU                  TO SLEBVU
           END-IF
           MOVE '   '                      TO VALTYP
           IF  (NOT-I-98)
               PERFORM VALRUT-S
           END-IF.
 
       FISLET-S SECTION.
       FISLET-S-P.
           SET NOT-I-98                    TO TRUE
           SET NOT-I-98                    TO TRUE
           IF  FIRMA = '   '
               SET I-98                    TO TRUE
           END-IF
           IF  (I-98)
               GO TO FIEND-T
           END-IF
           MOVE FIRMA                      TO FIRMAF-KEY1
           READ FIRMAF RECORD KEY IS FIRMAF-KEY1
           INVALID KEY
               SET I-96                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-96                TO TRUE
               PERFORM FIRMAF-FLDSET
               PERFORM FIRMAF-IDSET
           END-READ
           IF  (NOT-I-96)
               SET NOT-I-98                TO TRUE
               IF  FIRMSL = 'S'
                   SET I-98                TO TRUE
               END-IF
           END-IF.
 
       FIEND-T.
           CONTINUE.
      ******************************************************
      *****************************************************************
      *  RUTINE FOR Å HENTE VALUTAKODER.                              *
      *****************************************************************
 
       VALRUT-S SECTION.
       VALRUT-S-P.
           SET NOT-I-97                    TO TRUE
           MOVE 'A'                        TO AKSEKD
           MOVE VT                         TO ADVK
           CALL 'ADVALUTA' USING VALPAR-XX-DATA-FIELDS
           MOVE STDVK                      TO VALTYP
      *RN97                MOVE VT        VALTYP            UKJENT
           .
 
       LR-CALCS SECTION.
       LR-CALCS-P.
           ADD ANT TO ZERO             GIVING LRRSAN
           ADD SLEBEL TO ZERO          GIVING LRRSBE
           ADD SLEBEU TO ZERO          GIVING LERSBU
      ******************************************************
      *    SUBRUTINE FOR SLETTING AV HELE FIRMA            *
      ******************************************************
           .
 
       NYKTO-GET SECTION.
       NYKTO-GET-P.
           IF  NYKTO-EOF-OFF
               READ NYKTO
               AT END
                   SET NYKTO-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       NYKTO-FLDOFF SECTION.
       NYKTO-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( NYKTO-IO-AREA (1:1) = '3'
            AND   NYKTO-IO-AREA (2:1) = '0' )
               SET NOT-I-05                TO TRUE
           END-EVALUATE.
 
       NYKTO-FLDSET SECTION.
       NYKTO-FLDSET-P.
           EVALUATE TRUE
           WHEN ( NYKTO-IO-AREA (1:1) = '3'
            AND   NYKTO-IO-AREA (2:1) = '0' )
               MOVE NYKTO-IO-AREA (1:200)  TO SREC (1:200)
               MOVE NYKTO-IO-AREA (38:10)  TO SBEL-IO
               INSPECT SBEL-IO REPLACING ALL ' ' BY '0'
               IF  SBEL = ZERO
                   SET I-05                TO TRUE
               END-IF
               MOVE NYKTO-IO-AREA (50:11)  TO SVBEL-IO
               INSPECT SVBEL-IO REPLACING ALL ' ' BY '0'
               MOVE NYKTO-IO-AREA (70:1)   TO VT (1:1)
               MOVE NYKTO-IO-AREA (114:7)  TO SBELU-IO
               MOVE NYKTO-IO-AREA (121:8)  TO SVBELU-IO
               MOVE NYKTO-IO-AREA (3:3)    TO FIRMA (1:3)
           WHEN ( NYKTO-IO-AREA (1:1) = '3'
            AND   NYKTO-IO-AREA (2:1) NOT = '0' )
               MOVE NYKTO-IO-AREA (1:200)  TO KREC (1:200)
               MOVE NYKTO-IO-AREA (39:9)   TO KBEL-IO
               INSPECT KBEL-IO REPLACING ALL ' ' BY '0'
               MOVE NYKTO-IO-AREA (51:10)  TO KVBEL-IO
               INSPECT KVBEL-IO REPLACING ALL ' ' BY '0'
               MOVE NYKTO-IO-AREA (70:1)   TO VT (1:1)
               MOVE NYKTO-IO-AREA (114:7)  TO KBELU-IO
               MOVE NYKTO-IO-AREA (121:8)  TO KVBELU-IO
               MOVE NYKTO-IO-AREA (3:3)    TO FIRMA (1:3)
           END-EVALUATE.
 
       NYKTO-IDCHK SECTION.
       NYKTO-IDCHK-P.
           EVALUATE TRUE
           WHEN ( NYKTO-IO-AREA (1:1) = '3'
            AND   NYKTO-IO-AREA (2:1) = '0' )
             OR ( NYKTO-IO-AREA (1:1) = '3'
            AND   NYKTO-IO-AREA (2:1) NOT = '0' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       NYKTO-IDSET SECTION.
       NYKTO-IDSET-P.
           EVALUATE TRUE
           WHEN ( NYKTO-IO-AREA (1:1) = '3'
            AND   NYKTO-IO-AREA (2:1) = '0' )
               SET I-01                    TO TRUE
           WHEN ( NYKTO-IO-AREA (1:1) = '3'
            AND   NYKTO-IO-AREA (2:1) NOT = '0' )
               SET I-02                    TO TRUE
           END-EVALUATE.
 
       NYKTO-CHK-LEVEL SECTION.
       NYKTO-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( NYKTO-IO-AREA (1:1) = '3'
            AND   NYKTO-IO-AREA (2:1) = '0' )
               MOVE LOW-VALUES             TO NYKTO-LEVEL-01
               MOVE NYKTO-IO-AREA (3:3)    TO NYKTO-01-L1-FIRMA
               IF  NYKTO-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  NYKTO-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  NYKTO-01-L1           TO THE-PRIOR-L1
               SET NYKTO-LEVEL-INIT        TO TRUE
           WHEN ( NYKTO-IO-AREA (1:1) = '3'
            AND   NYKTO-IO-AREA (2:1) NOT = '0' )
               MOVE LOW-VALUES             TO NYKTO-LEVEL-02
               MOVE NYKTO-IO-AREA (3:3)    TO NYKTO-02-L1-FIRMA
               IF  NYKTO-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  NYKTO-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  NYKTO-02-L1           TO THE-PRIOR-L1
               SET NYKTO-LEVEL-INIT        TO TRUE
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (123:1) TO FIRMSL (1:1)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-08                        TO TRUE.
 
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
           IF  (I-01 AND NOT-I-98 AND NOT-I-05)
               MOVE SPACES TO GMLKTO-IO-AREA
               INITIALIZE GMLKTO-IO-AREA
               MOVE SREC                   TO GMLKTO-IO-AREA (1:200)
               IF  (I-U1)
                   MOVE BEL132             TO XO-112P
                   MOVE XO-112P-EF         TO GMLKTO-IO-AREA (114:7)
               END-IF
               IF  (I-U1)
                   MOVE BEL154             TO XO-114P
                   MOVE XO-114P-EF         TO GMLKTO-IO-AREA (121:8)
               END-IF
               IF  (I-U1)
                   MOVE VALTYP             TO GMLKTO-IO-AREA (129:3)
               END-IF
               IF  (I-U1)
                   MOVE 'INIT'             TO GMLKTO-IO-AREA (188:4)
               END-IF
               IF  (I-U1)
                   MOVE PRDDTO             TO XO-80P
                   MOVE XO-80P-EF          TO GMLKTO-IO-AREA (192:5)
               END-IF
               IF  (I-U1)
                   MOVE PRDKLK             TO XO-60P
                   MOVE XO-60P-EF          TO GMLKTO-IO-AREA (197:4)
               END-IF
               WRITE GMLKTO-IO-AREA
           END-IF
           IF  (I-02 AND NOT-I-98)
               MOVE SPACES TO GMLKTO-IO-AREA
               INITIALIZE GMLKTO-IO-AREA
               MOVE KREC                   TO GMLKTO-IO-AREA (1:200)
               IF  (I-U1)
                   MOVE BEL132             TO XO-112P
                   MOVE XO-112P-EF         TO GMLKTO-IO-AREA (114:7)
               END-IF
               IF  (I-U1)
                   MOVE BEL154             TO XO-114P
                   MOVE XO-114P-EF         TO GMLKTO-IO-AREA (121:8)
               END-IF
               IF  (I-U1)
                   MOVE VALTYP             TO GMLKTO-IO-AREA (129:3)
               END-IF
               IF  (I-U1)
                   MOVE 'INIT'             TO GMLKTO-IO-AREA (188:4)
               END-IF
               IF  (I-U1)
                   MOVE PRDDTO             TO XO-80P
                   MOVE XO-80P-EF          TO GMLKTO-IO-AREA (192:5)
               END-IF
               IF  (I-U1)
                   MOVE PRDKLK             TO XO-60P
                   MOVE XO-60P-EF          TO GMLKTO-IO-AREA (197:4)
               END-IF
               WRITE GMLKTO-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO AVSTEMM-IO-AREA
               INITIALIZE AVSTEMM-IO-AREA
               MOVE PRDDTO-IO              TO AVSTEMM-IO-AREA (1:8)
               MOVE '035'                  TO AVSTEMM-IO-AREA (9:3)
               MOVE 'RES'                  TO AVSTEMM-IO-AREA (12:3)
               MOVE 'RKO003'               TO AVSTEMM-IO-AREA (15:6)
               MOVE '*RKO003*'             TO AVSTEMM-IO-AREA (21:8)
               MOVE TIDSP-IO               TO AVSTEMM-IO-AREA (30:6)
               MOVE LRRSAN-IO              TO AVSTEMM-IO-AREA (36:13)
               MOVE LRRSBE-IO              TO AVSTEMM-IO-AREA (49:15)
               MOVE LERSBU-IO              TO AVSTEMM-IO-AREA (64:15)
               MOVE PRDDTO-IO              TO AVSTEMM-IO-AREA (79:8)
               WRITE AVSTEMM-IO-AREA
               MOVE SPACES TO AVSTEMM-IO-AREA
               INITIALIZE AVSTEMM-IO-AREA
               MOVE '********'             TO AVSTEMM-IO-AREA (1:8)
               MOVE '035'                  TO AVSTEMM-IO-AREA (9:3)
               MOVE 'RES'                  TO AVSTEMM-IO-AREA (12:3)
               MOVE '******'               TO AVSTEMM-IO-AREA (15:6)
               MOVE '*RKO003*'             TO AVSTEMM-IO-AREA (21:8)
               MOVE TIDSP-IO               TO AVSTEMM-IO-AREA (30:6)
               MOVE LRRSAN-IO              TO AVSTEMM-IO-AREA (36:13)
               MOVE LRRSBE-IO              TO AVSTEMM-IO-AREA (49:15)
               MOVE LERSBU-IO              TO AVSTEMM-IO-AREA (64:15)
               MOVE PRDDTO-IO              TO AVSTEMM-IO-AREA (79:8)
               WRITE AVSTEMM-IO-AREA
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'AVSTEMMING PROG. RKO003 ' TO LISTE-IO-AREA (1:24)
               MOVE 'DATO  :'              TO LISTE-IO-AREA (65:7)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (73:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KLOKKE:'              TO LISTE-IO-AREA (65:7)
               MOVE TIDSP                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (73:8)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT REC KOPIERT'      TO LISTE-IO-AREA (6:15)
               MOVE 'FRA NY TIL GML KTOKUR' TO LISTE-IO-AREA (22:21)
               MOVE TOTREC                 TO XO-60YNZ
               MOVE XO-60YNZ               TO LISTE-IO-AREA (54:6)
               INITIALIZE TOTREC
               MOVE 'SELVKOST/VALUTA'      TO LISTE-IO-AREA (66:15)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'BELØP   KOPIERT'      TO LISTE-IO-AREA (6:15)
               MOVE 'FRA NY TIL GML KTOKUR' TO LISTE-IO-AREA (22:21)
               MOVE TOTBEL                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (46:15)
               MOVE TOTBEV                 TO XO-112YY9R
               MOVE XO-112YY9R             TO LISTE-IO-AREA (61:18)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '--""""""""""""""""""""""""-(TIE    T IT-OAE 61)
               MOVE 'BELØPSFELT)          ' TO LISTE-IO-AREA (22:21)
               MOVE TOTBEU                 TO XO-112YY9R
               MOVE XO-112YY9R             TO LISTE-IO-AREA (43:18)
               MOVE TOTBVU                 TO XO-114YY9R
               MOVE XO-114YY9R             TO LISTE-IO-AREA (61:20)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT REC SLETTET'      TO LISTE-IO-AREA (6:15)
               MOVE 'FRA NY TIL GML KTOKUR' TO LISTE-IO-AREA (22:21)
               MOVE ANT                    TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (53:7)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'BELØP   SLETTET'      TO LISTE-IO-AREA (6:15)
               MOVE 'FRA NY TIL GML KTOKUR' TO LISTE-IO-AREA (22:21)
               MOVE SLEBEL                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (46:15)
               MOVE SLEBEV                 TO XO-112YY9R
               MOVE XO-112YY9R             TO LISTE-IO-AREA (61:18)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '--""""""""""""""""""""""""-(TIE    T IT-OAE 61)
               MOVE 'BELØPSFELT)          ' TO LISTE-IO-AREA (22:21)
               MOVE SLEBEU                 TO XO-112YY9R
               MOVE XO-112YY9R             TO LISTE-IO-AREA (43:18)
               MOVE SLEBVU                 TO XO-114YY9R
               MOVE XO-114YY9R             TO LISTE-IO-AREA (61:20)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE '************************' TO LISTE-IO-AREA (49:24)
               MOVE 2                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '***** OBS! SANERING MÅ H' TO LISTE-IO-AREA (1:24)
               MOVE 'ENSYNTAS I DAGLIG AVSTEM' TO LISTE-IO-AREA (25:24)
               MOVE 'MING (JFR RSK030)  *****' TO LISTE-IO-AREA (49:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE '************************' TO LISTE-IO-AREA (49:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR AND I-08 AND I-U8)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE PSDS                   TO LISTE-IO-AREA (53:80)
               MOVE R                      TO LISTE-IO-AREA (125:8)
               MOVE P-IO                   TO LISTE-IO-AREA (130:3)
               MOVE S-IO                   TO LISTE-IO-AREA (128:5)
               MOVE AKSEKD                 TO LISTE-IO-AREA (132:1)
               MOVE ADVK                   TO LISTE-IO-AREA (132:1)
               MOVE NORVN                  TO LISTE-IO-AREA (113:20)
               MOVE STDVN                  TO LISTE-IO-AREA (113:20)
               MOVE LANDKD                 TO LISTE-IO-AREA (131:2)
               MOVE LAND                   TO LISTE-IO-AREA (113:20)
               MOVE VALIX-IO               TO LISTE-IO-AREA (130:3)
               MOVE DIV                    TO LISTE-IO-AREA (123:10)
               MOVE AKSEKD                 TO LISTE-IO-AREA (132:1)
               MOVE ADVK                   TO LISTE-IO-AREA (132:1)
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
           SET NYKTO-LEVEL-INIT            TO TRUE
           INITIALIZE NYKTO-DATA-FIELDS
           SET NYKTO-EOF-OFF               TO TRUE
           SET NYKTO-PROCESS               TO TRUE
           OPEN INPUT NYKTO
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           OPEN OUTPUT GMLKTO
           OPEN OUTPUT AVSTEMM
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE NYKTO
           CLOSE FIRMAF
           CLOSE GMLKTO
           CLOSE AVSTEMM
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
