       IDENTIFICATION DIVISION.
       PROGRAM-ID. VRG825R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAMMET OVERFØRER ALLE VARERECORDS SOM IKKE FINNES I     *
      * DET ENE VARE.MASTER.                                        *
      * SAMTIDIG OPPDATERER DEN GAMLE RECORDEN.                     *
      * INNLEGGING PÅ VAREMASTER KAN IKKE UTFØRES NÅR CICS ER OPPE. *
      ***************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VRG825.rpg
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
           SELECT VAREBUP
               ASSIGN TO UT-S-VAREBUP
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VAREBUP-STATUS.
           SELECT VAREMAS
               ASSIGN TO VAREMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAREMAS-STATUS
               RECORD KEY IS VAREMAS-KEY1.
           SELECT VARETIL
               ASSIGN TO VARETIL
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VARETIL-STATUS
               RECORD KEY IS VARETIL-KEY1.
           SELECT VAREREC
               ASSIGN TO UT-S-VAREREC
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VAREREC-STATUS.
           SELECT NYEVART
               ASSIGN TO UT-S-NYEVART
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS NYEVART-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
           SELECT OUTPUN
               ASSIGN TO UT-S-OUTPUN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTPUN-STATUS.
           SELECT OUTHOST
               ASSIGN TO OUTHOST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTHOST-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD VAREBUP
               BLOCK CONTAINS 8000
               RECORD CONTAINS 200.
       01  VAREBUP-IO-AREA.
           05  VAREBUP-IO-AREA-X           PICTURE X(200).
       FD VAREMAS
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X.
               10  VAREMAS-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       FD VARETIL
               RECORD CONTAINS 200.
       01  VARETIL-IO-AREA.
           05  VARETIL-IO-AREA-X.
               10  VARETIL-KEY1            PICTURE X(12).
               10  FILLER                  PICTURE X(188).
       FD VAREREC
               BLOCK CONTAINS 9400
               RECORD CONTAINS 200.
       01  VAREREC-IO-AREA.
           05  VAREREC-IO-AREA-X           PICTURE X(200).
       FD NYEVART
               BLOCK CONTAINS 9400
               RECORD CONTAINS 200.
       01  NYEVART-IO-AREA.
           05  NYEVART-IO-AREA-X           PICTURE X(200).
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       FD OUTPUN
               BLOCK CONTAINS 128
               RECORD CONTAINS 128.
       01  OUTPUN-IO-AREA.
           05  OUTPUN-IO-AREA-X            PICTURE X(128).
       FD OUTHOST
               RECORD CONTAINS 600.
       01  OUTHOST-IO-AREA.
           05  OUTHOST-IO-AREA-X           PICTURE X(600).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  VAREBUP-STATUS              PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  VARETIL-STATUS              PICTURE 99 VALUE 0.
           10  VAREREC-STATUS              PICTURE 99 VALUE 0.
           10  NYEVART-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  OUTPUN-STATUS               PICTURE 99 VALUE 0.
           10  OUTHOST-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREBUP-EOF-OFF         VALUE '0'.
               88  VAREBUP-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREBUP-READ-OFF        VALUE '0'.
               88  VAREBUP-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREBUP-PROCESS-OFF     VALUE '0'.
               88  VAREBUP-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  VAREBUP-LEVEL-INIT-OFF  VALUE '0'.
               88  VAREBUP-LEVEL-INIT      VALUE '1'.
           05  VAREMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  VARETIL-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  VAREBUP-LEVEL-01.
               10  VAREBUP-01-L1.
                   15  VAREBUP-01-L1-FIRMA PICTURE X(3).
           05  VAREBUP-DATA-FIELDS.
               10  EDBKEY                  PICTURE X(10).
               10  FIRMA                   PICTURE X(3).
               10  EDBBUP                  PICTURE X(7).
               10  EDBNUM-IO.
                   15  EDBNUM              PICTURE S9(7).
               10  BUPREC                  PICTURE X(200).
               10  BUPRE                   PICTURE X(188).
               10  KEYA                    PICTURE X(10).
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  NAVN                    PICTURE X(30).
               10  PRIST-IO.
                   15  PRIST               PICTURE S9(7)V9(2).
               10  PRISV-IO.
                   15  PRISV               PICTURE S9(7)V9(2).
               10  PTYPE                   PICTURE X(1).
               10  PRITYP                  PICTURE X(1).
               10  KINNH-IO.
                   15  KINNH               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  NYVEKT-IO.
                   15  NYVEKT              PICTURE S9(3)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  VAREGR                  PICTURE X(5).
               10  MERKN                   PICTURE X(1).
               10  NYRAB-IO.
                   15  NYRAB               PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  PRISPT-IO.
                   15  PRISPT              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  PRISM-IO.
                   15  PRISM               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  VALUTA                  PICTURE X(1).
           05  VAREMAS-DATA-FIELDS.
      *                                       3  12 EDBKEY
               10  KEYB                    PICTURE X(10).
               10  FNR                     PICTURE X(3).
               10  EDBNR                   PICTURE X(7).
               10  VALF                    PICTURE X(3).
               10  VARTNR                  PICTURE X(20).
               10  VNAVN                   PICTURE X(30).
               10  GMLS-IO.
                   15  GMLS                PICTURE S9(7)V9(2).
               10  GMLU-IO.
                   15  GMLU                PICTURE S9(7)V9(2).
               10  GPTYPE                  PICTURE X(1).
               10  GMLPT                   PICTURE X(1).
               10  GMKINH-IO.
                   15  GMKINH              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  GMVEKT-IO.
                   15  GMVEKT              PICTURE S9(3)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  VVGR                    PICTURE X(5).
               10  GMERKN                  PICTURE X(1).
      *                                     140 145 GMLLOC
               10  GRAB-IO.
                   15  GRAB                PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  PTILL-IO.
                   15  PTILL               PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  GMLL-IO.
                   15  GMLL                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  GMLVAL                  PICTURE X(1).
           05  VARETIL-DATA-FIELDS.
               10  GMLLN1-IO.
                   15  GMLLN1              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  GMLEAN                  PICTURE X(13).
               10  GMLV-IO.
                   15  GMLV                PICTURE S9(7)V9(2).
      * CHAIN MOT VAREMAS
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  KEY12                   PICTURE X(12).
               10  DAT1                    PICTURE X(4).
               10  DAT2                    PICTURE X(6).
               10  DATO-IO.
                   15  DATO                PICTURE S9(6).
               10  EDATO-IO.
                   15  EDATO               PICTURE S9(7).
               10  MND                     PICTURE X(2).
               10  AAR                     PICTURE X(2).
               10  ANTMR-IO.
                   15  ANTMR               PICTURE S9(6).
               10  ANTNMR-IO.
                   15  ANTNMR              PICTURE S9(6).
               10  NYUTS-IO.
                   15  NYUTS               PICTURE S9(7)V9(2).
               10  NYVEIL-IO.
                   15  NYVEIL              PICTURE S9(7)V9(2).
               10  LEVNR1-IO.
                   15  LEVNR1              PICTURE S9(7).
               10  NULL2-IO.
                   15  NULL2               PICTURE S9(2).
               10  NULL3-IO.
                   15  NULL3               PICTURE S9(3).
               10  NULL4-IO.
                   15  NULL4               PICTURE S9(4).
               10  NULL5-IO.
                   15  NULL5               PICTURE S9(5).
               10  NULL6-IO.
                   15  NULL6               PICTURE S9(6).
               10  NULL7-IO.
                   15  NULL7               PICTURE S9(7).
               10  NULL8-IO.
                   15  NULL8               PICTURE S9(8).
               10  NULL9-IO.
                   15  NULL9               PICTURE S9(9).
               10  TOTNMR-IO.
                   15  TOTNMR              PICTURE S9(6).
           05  EDITTING-FIELDS.
               10  XO-70P-EF.
                 15  XO-70P                PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  XO-21P-EF.
                 15  XO-21P                PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  XO-72P-EF.
                 15  XO-72P                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-50P-EF.
                 15  XO-50P                PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  XO-90P-EF.
                 15  XO-90P                PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
               10  XO-60P-EF.
                 15  XO-60P                PICTURE S9(6) USAGE
                                                       PACKED-DECIMAL.
               10  XO-60YY9                PICTURE ZZZ.ZZ9.
               10  XO-72D                  PICTURE S9(7)V9(2).
               10  XO-72U                  PICTURE 9(7)V9(2).
               10  XO-21D                  PICTURE S9(2)V9(1).
               10  XO-21U                  PICTURE 9(2)V9(1).
               10  XO-70D                  PICTURE S9(7).
               10  XO-70U                  PICTURE 9(7).
               10  XO-72YY9                PICTURE Z.ZZZ.ZZZ,99.
               10  XO-21YY9                PICTURE ZZ,9.
               10  XO-52YY9                PICTURE ZZ.ZZZ,99.
               10  XO-50YY9                PICTURE ZZ.ZZ9.
               10  XO-32YY9                PICTURE ZZZ,99.
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
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  VAREBUP-PROCESS
               SET VAREBUP-PROCESS-OFF     TO TRUE
               SET VAREBUP-READ            TO TRUE
           END-IF
 
           IF  VAREBUP-READ
           AND RECORD-SELECTED-OFF
               PERFORM VAREBUP-GET
               SET VAREBUP-READ-OFF        TO TRUE
               IF  NOT VAREBUP-EOF
                   SET VAREBUP-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  VAREBUP-PROCESS
               PERFORM VAREBUP-IDSET
           END-IF
 
           IF  VAREBUP-PROCESS
               PERFORM VAREBUP-CHK-LEVEL
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
           PERFORM DETAIL-OVERFLOW
 
           IF  VAREBUP-PROCESS
               PERFORM VAREBUP-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  VAREBUP-PROCESS
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
               MOVE KEYA                   TO VAREMAS-KEY1
               READ VAREMAS RECORD KEY IS VAREMAS-KEY1
               INVALID KEY
                   SET I-10                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-10            TO TRUE
                   PERFORM VAREMAS-FLDSET
                   PERFORM VAREMAS-IDSET
               END-READ
      * CHAIN MOT VARETIL
           END-IF
           MOVE '80'                       TO KEY12 (1:2)
           MOVE KEYA                       TO KEY12 (3:10)
           MOVE KEY12                      TO VARETIL-KEY1
           READ VARETIL RECORD KEY IS VARETIL-KEY1
           INVALID KEY
               SET I-11                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-11                TO TRUE
               PERFORM VARETIL-FLDSET
               PERFORM VARETIL-IDSET
           END-READ
      * FORMATER DATO
           MOVE UDAY                       TO DAT1 (1:2)
           MOVE UMONTH                     TO DAT1 (3:2)
           MOVE DAT1                       TO DAT2 (1:4)
           MOVE UYEAR                      TO DAT2 (5:2)
           MOVE DAT2                       TO DATO-IO
           ADD DATO TO ZERO            GIVING EDATO
           MOVE UMONTH                     TO MND
           MOVE UYEAR                      TO AAR
      * NULLSTILLING
           IF  (I-L1)
               MOVE 0                      TO ANTMR
               MOVE 0                      TO ANTNMR
      * FLYTTER PRISER INN I NYE FELT
           END-IF
           ADD PRISM TO ZERO           GIVING NYUTS
           ADD PRISV TO ZERO           GIVING NYVEIL
           MOVE 900131                     TO LEVNR1
           MOVE 0                          TO NULL2
           MOVE 0                          TO NULL3
           MOVE 0                          TO NULL4
           MOVE 0                          TO NULL5
           MOVE 0                          TO NULL6
           MOVE 0                          TO NULL7
           MOVE 0                          TO NULL8
           MOVE 0                          TO NULL9
      * TELLERE
           IF  (I-01 AND NOT-I-10)
               ADD 1                       TO ANTMR
           END-IF
           IF  (I-01 AND I-10)
               ADD 1                       TO ANTNMR
               ADD 1                       TO TOTNMR
      *
           END-IF
           .
 
       VAREBUP-GET SECTION.
       VAREBUP-GET-P.
           IF  VAREBUP-EOF-OFF
               READ VAREBUP
               AT END
                   SET VAREBUP-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VAREBUP-FLDSET SECTION.
       VAREBUP-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREBUP-IO-AREA (3:10) TO EDBKEY (1:10)
               MOVE VAREBUP-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE VAREBUP-IO-AREA (6:7)  TO EDBBUP (1:7)
               MOVE VAREBUP-IO-AREA (6:7)  TO EDBNUM-IO
               INSPECT EDBNUM-IO REPLACING ALL ' ' BY '0'
               MOVE VAREBUP-IO-AREA (1:200) TO BUPREC (1:200)
               MOVE VAREBUP-IO-AREA (13:188) TO BUPRE (1:188)
               MOVE VAREBUP-IO-AREA (3:10) TO KEYA (1:10)
               MOVE VAREBUP-IO-AREA (13:3) TO ALFA (1:3)
               MOVE VAREBUP-IO-AREA (16:20) TO ARTNR (1:20)
               MOVE VAREBUP-IO-AREA (36:30) TO NAVN (1:30)
               MOVE VAREBUP-IO-AREA (66:9) TO PRIST-IO
               INSPECT PRIST-IO REPLACING ALL ' ' BY '0'
               MOVE VAREBUP-IO-AREA (75:9) TO PRISV-IO
               INSPECT PRISV-IO REPLACING ALL ' ' BY '0'
               MOVE VAREBUP-IO-AREA (95:1) TO PTYPE (1:1)
               MOVE VAREBUP-IO-AREA (107:1) TO PRITYP (1:1)
               MOVE VAREBUP-IO-AREA (108:3) TO KINNH-IO
               MOVE VAREBUP-IO-AREA (111:3) TO NYVEKT-IO
               MOVE VAREBUP-IO-AREA (118:5) TO VAREGR (1:5)
               MOVE VAREBUP-IO-AREA (127:1) TO MERKN (1:1)
               MOVE VAREBUP-IO-AREA (153:2) TO NYRAB-IO
               MOVE VAREBUP-IO-AREA (161:4) TO PRISPT-IO
               MOVE VAREBUP-IO-AREA (165:5) TO PRISM-IO
               MOVE VAREBUP-IO-AREA (170:1) TO VALUTA (1:1)
           END-EVALUATE.
 
       VAREBUP-IDSET SECTION.
       VAREBUP-IDSET-P.
           SET I-01                        TO TRUE.
 
       VAREBUP-CHK-LEVEL SECTION.
       VAREBUP-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO VAREBUP-LEVEL-01
               MOVE VAREBUP-IO-AREA (3:3)  TO VAREBUP-01-L1-FIRMA
               IF  VAREBUP-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VAREBUP-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  VAREBUP-01-L1         TO THE-PRIOR-L1
               SET VAREBUP-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (3:10) TO KEYB (1:10)
               MOVE VAREMAS-IO-AREA (3:3)  TO FNR (1:3)
               MOVE VAREMAS-IO-AREA (6:7)  TO EDBNR (1:7)
               MOVE VAREMAS-IO-AREA (13:3) TO VALF (1:3)
               MOVE VAREMAS-IO-AREA (16:20) TO VARTNR (1:20)
               MOVE VAREMAS-IO-AREA (36:30) TO VNAVN (1:30)
               MOVE VAREMAS-IO-AREA (66:9) TO GMLS-IO
               INSPECT GMLS-IO REPLACING ALL ' ' BY '0'
               MOVE VAREMAS-IO-AREA (75:9) TO GMLU-IO
               INSPECT GMLU-IO REPLACING ALL ' ' BY '0'
               MOVE VAREMAS-IO-AREA (95:1) TO GPTYPE (1:1)
               MOVE VAREMAS-IO-AREA (107:1) TO GMLPT (1:1)
               MOVE VAREMAS-IO-AREA (108:3) TO GMKINH-IO
               MOVE VAREMAS-IO-AREA (111:3) TO GMVEKT-IO
               MOVE VAREMAS-IO-AREA (118:5) TO VVGR (1:5)
               MOVE VAREMAS-IO-AREA (127:1) TO GMERKN (1:1)
               MOVE VAREMAS-IO-AREA (153:2) TO GRAB-IO
               MOVE VAREMAS-IO-AREA (161:4) TO PTILL-IO
               MOVE VAREMAS-IO-AREA (165:5) TO GMLL-IO
               MOVE VAREMAS-IO-AREA (170:1) TO GMLVAL (1:1)
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-02                        TO TRUE.
 
       VARETIL-FLDSET SECTION.
       VARETIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VARETIL-IO-AREA (68:4) TO GMLLN1-IO
               MOVE VARETIL-IO-AREA (148:13) TO GMLEAN (1:13)
               MOVE VARETIL-IO-AREA (180:9) TO GMLV-IO
               INSPECT GMLV-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       VARETIL-IDSET SECTION.
       VARETIL-IDSET-P.
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
           IF  (I-01 AND NOT-I-10)
      *                        BUPRE    200
               MOVE NAVN                   TO VAREMAS-IO-AREA (36:30)
      *                        PRISV X   83
               MOVE NYUTS-IO               TO VAREMAS-IO-AREA (75:9)
               MOVE EDATO                  TO XO-70P
               MOVE XO-70P-EF              TO VAREMAS-IO-AREA (84:4)
               MOVE ' '                    TO VAREMAS-IO-AREA (127:1)
               MOVE VAREGR                 TO VAREMAS-IO-AREA (118:5)
               MOVE NYRAB                  TO XO-21P
               MOVE XO-21P-EF              TO VAREMAS-IO-AREA (153:2)
               MOVE PRISM                  TO XO-72P
               MOVE XO-72P-EF              TO VAREMAS-IO-AREA (165:5)
               REWRITE VAREMAS-IO-AREA
                   INVALID KEY
                       DISPLAY 'Bad REWRITE - file = VAREMAS'
               END-REWRITE
           END-IF
           IF  (I-01 AND NOT-I-11)
               MOVE NYVEIL-IO              TO VARETIL-IO-AREA (180:9)
               REWRITE VARETIL-IO-AREA
                   INVALID KEY
                       DISPLAY 'Bad REWRITE - file = VARETIL'
               END-REWRITE
           END-IF
           IF  (I-01 AND I-10)
               MOVE SPACES TO VAREREC-IO-AREA
               INITIALIZE VAREREC-IO-AREA
               MOVE BUPREC                 TO VAREREC-IO-AREA (1:200)
               MOVE NYUTS-IO               TO VAREREC-IO-AREA (75:9)
               WRITE VAREREC-IO-AREA
           END-IF
           IF  (I-01 AND I-10 AND I-11)
               MOVE SPACES TO NYEVART-IO-AREA
               INITIALIZE NYEVART-IO-AREA
               MOVE '80'                   TO NYEVART-IO-AREA (1:2)
               MOVE FIRMA                  TO NYEVART-IO-AREA (3:3)
               MOVE EDBNUM-IO              TO NYEVART-IO-AREA (6:7)
               MOVE '                    ' TO NYEVART-IO-AREA (13:20)
      *                        LEVART    32
               MOVE '                    ' TO NYEVART-IO-AREA (33:20)
               MOVE '          '           TO NYEVART-IO-AREA (53:10)
      *                        LEVBET    62
               MOVE '   '                  TO NYEVART-IO-AREA (63:3)
      *                        ENHET     65
               MOVE NULL2-IO               TO NYEVART-IO-AREA (66:2)
               MOVE NULL7                  TO XO-70P
               MOVE XO-70P-EF              TO NYEVART-IO-AREA (68:4)
               MOVE LEVNR1                 TO XO-70P
               MOVE XO-70P-EF              TO NYEVART-IO-AREA (68:4)
               MOVE NULL7                  TO XO-70P
               MOVE XO-70P-EF              TO NYEVART-IO-AREA (72:4)
      *                        LEVNR2    75P
               MOVE NULL7                  TO XO-70P
               MOVE XO-70P-EF              TO NYEVART-IO-AREA (76:4)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO NYEVART-IO-AREA (80:3)
      *                        MINFAK    82P
               MOVE NULL9                  TO XO-90P
               MOVE XO-90P-EF              TO NYEVART-IO-AREA (83:5)
               MOVE NULL9                  TO XO-90P
               MOVE XO-90P-EF              TO NYEVART-IO-AREA (88:5)
               MOVE NULL9                  TO XO-90P
               MOVE XO-90P-EF              TO NYEVART-IO-AREA (93:5)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO NYEVART-IO-AREA (98:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO NYEVART-IO-AREA (101:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO NYEVART-IO-AREA (104:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO NYEVART-IO-AREA (107:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO NYEVART-IO-AREA (110:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO NYEVART-IO-AREA (113:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO NYEVART-IO-AREA (116:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO NYEVART-IO-AREA (119:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO NYEVART-IO-AREA (122:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO NYEVART-IO-AREA (125:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO NYEVART-IO-AREA (128:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO NYEVART-IO-AREA (131:3)
               MOVE NULL9                  TO XO-90P
               MOVE XO-90P-EF              TO NYEVART-IO-AREA (134:5)
               MOVE NULL9                  TO XO-90P
               MOVE XO-90P-EF              TO NYEVART-IO-AREA (139:5)
               MOVE NULL6                  TO XO-60P
               MOVE XO-60P-EF              TO NYEVART-IO-AREA (144:4)
               MOVE '             '        TO NYEVART-IO-AREA (148:13)
      *                     N32EAN      160
               MOVE '       '              TO NYEVART-IO-AREA (161:7)
               MOVE '            '         TO NYEVART-IO-AREA (168:12)
               MOVE NULL9-IO               TO NYEVART-IO-AREA (180:9)
               MOVE NYVEIL-IO              TO NYEVART-IO-AREA (180:9)
               MOVE '             '        TO NYEVART-IO-AREA (188:13)
               WRITE NYEVART-IO-AREA
           END-IF
           IF  (I-1P)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'MERGE VAREMASTER    ' TO LISTE-IO-AREA (1:20)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (23:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMANR.'             TO LISTE-IO-AREA (3:8)
               MOVE 'ANT. MR.'             TO LISTE-IO-AREA (13:8)
               MOVE 'ANT.NMR.'             TO LISTE-IO-AREA (23:8)
      *       D  1     02 MR
      *                        KEYA      10
      *                        KEYB      30
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-01 AND NOT-I-10)
               MOVE SPACES TO OUTPUN-IO-AREA
               INITIALIZE OUTPUN-IO-AREA
               MOVE FNR                    TO OUTPUN-IO-AREA (1:3)
               MOVE EDBNR                  TO OUTPUN-IO-AREA (4:7)
               MOVE GMLL                   TO XO-72U
               MOVE XO-72U (1:9)           TO OUTPUN-IO-AREA (11:9)
               MOVE GMLU-IO                TO OUTPUN-IO-AREA (20:9)
      *                        GMLS  X   37
               IF  (NOT-I-11)
                   MOVE GMLV-IO            TO OUTPUN-IO-AREA (38:9)
               END-IF
               MOVE VVGR                   TO OUTPUN-IO-AREA (47:5)
               MOVE VNAVN                  TO OUTPUN-IO-AREA (52:30)
      *                        GMERKN    82
      *                        PTILL X   89
      *                        GMLPT     90
      *                        GMKINHX   95
      *                        GMVEKTX  100
               MOVE GRAB                   TO XO-21U
               MOVE XO-21U (1:3)           TO OUTPUN-IO-AREA (101:3)
               IF  (NOT-I-11)
                   MOVE GMLLN1             TO XO-70U
                   MOVE XO-70U (1:7)       TO OUTPUN-IO-AREA (104:7)
               END-IF
               IF  (NOT-I-11)
                   MOVE GMLEAN             TO OUTPUN-IO-AREA (112:13)
               END-IF
               WRITE OUTPUN-IO-AREA
           END-IF
           IF  (I-01 AND NOT-I-10)
               MOVE SPACES TO OUTHOST-IO-AREA
               INITIALIZE OUTHOST-IO-AREA
               MOVE FNR                    TO OUTHOST-IO-AREA (1:3)
               MOVE ';'                    TO OUTHOST-IO-AREA (4:1)
               MOVE FIRMA                  TO OUTHOST-IO-AREA (5:3)
               MOVE ';'                    TO OUTHOST-IO-AREA (8:1)
               MOVE VVGR                   TO OUTHOST-IO-AREA (9:5)
               MOVE ';'                    TO OUTHOST-IO-AREA (14:1)
               MOVE VAREGR                 TO OUTHOST-IO-AREA (15:5)
               MOVE ';'                    TO OUTHOST-IO-AREA (20:1)
               MOVE VALF                   TO OUTHOST-IO-AREA (21:3)
               MOVE ';'                    TO OUTHOST-IO-AREA (24:1)
               MOVE ALFA                   TO OUTHOST-IO-AREA (25:3)
               MOVE ';'                    TO OUTHOST-IO-AREA (28:1)
               MOVE VARTNR                 TO OUTHOST-IO-AREA (29:20)
               MOVE ';'                    TO OUTHOST-IO-AREA (49:1)
               MOVE ARTNR                  TO OUTHOST-IO-AREA (50:20)
               MOVE ';'                    TO OUTHOST-IO-AREA (70:1)
               MOVE VNAVN                  TO OUTHOST-IO-AREA (71:30)
               MOVE ';'                    TO OUTHOST-IO-AREA (101:1)
               MOVE NAVN                   TO OUTHOST-IO-AREA (102:30)
               MOVE ';'                    TO OUTHOST-IO-AREA (132:1)
               MOVE GMLL                   TO XO-72YY9
               MOVE XO-72YY9               TO OUTHOST-IO-AREA (136:12)
               MOVE ';'                    TO OUTHOST-IO-AREA (148:1)
               MOVE PRISM                  TO XO-72YY9
               MOVE XO-72YY9               TO OUTHOST-IO-AREA (152:12)
               MOVE ';'                    TO OUTHOST-IO-AREA (164:1)
               MOVE GMLU                   TO XO-72YY9
               MOVE XO-72YY9               TO OUTHOST-IO-AREA (168:12)
               MOVE ';'                    TO OUTHOST-IO-AREA (180:1)
      *                        PRISV 1  195
               MOVE NYUTS                  TO XO-72YY9
               MOVE XO-72YY9               TO OUTHOST-IO-AREA (184:12)
               MOVE ';'                    TO OUTHOST-IO-AREA (196:1)
               MOVE GMLS                   TO XO-72YY9
               MOVE XO-72YY9               TO OUTHOST-IO-AREA (200:12)
               MOVE ';'                    TO OUTHOST-IO-AREA (212:1)
               MOVE PRIST                  TO XO-72YY9
               MOVE XO-72YY9               TO OUTHOST-IO-AREA (216:12)
               MOVE ';'                    TO OUTHOST-IO-AREA (228:1)
               MOVE GMLV                   TO XO-72YY9
               MOVE XO-72YY9               TO OUTHOST-IO-AREA (232:12)
               MOVE ';'                    TO OUTHOST-IO-AREA (244:1)
      *                        PRISVE1  259
               MOVE NYVEIL                 TO XO-72YY9
               MOVE XO-72YY9               TO OUTHOST-IO-AREA (248:12)
               MOVE ';'                    TO OUTHOST-IO-AREA (260:1)
               MOVE GRAB                   TO XO-21YY9
               MOVE XO-21YY9               TO OUTHOST-IO-AREA (265:4)
               MOVE ';'                    TO OUTHOST-IO-AREA (269:1)
               MOVE NYRAB                  TO XO-21YY9
               MOVE XO-21YY9               TO OUTHOST-IO-AREA (274:4)
               MOVE ';'                    TO OUTHOST-IO-AREA (278:1)
               IF GMLLN1 < 0
                 MOVE GMLLN1               TO XO-70D
                 MOVE XO-70D (1:7)         TO OUTHOST-IO-AREA (280:7)
               ELSE
                 MOVE GMLLN1               TO XO-70U
                 MOVE XO-70U (1:7)         TO OUTHOST-IO-AREA (280:7)
               END-IF
               MOVE ';'                    TO OUTHOST-IO-AREA (287:1)
               MOVE LEVNR1-IO              TO OUTHOST-IO-AREA (289:7)
               MOVE ';'                    TO OUTHOST-IO-AREA (296:1)
               MOVE GMLEAN                 TO OUTHOST-IO-AREA (298:13)
               MOVE ';'                    TO OUTHOST-IO-AREA (311:1)
      *                        EAN      325
               MOVE ';'                    TO OUTHOST-IO-AREA (326:1)
               MOVE PTILL                  TO XO-52YY9
               MOVE XO-52YY9               TO OUTHOST-IO-AREA (331:9)
               MOVE ';'                    TO OUTHOST-IO-AREA (340:1)
               MOVE PRISPT                 TO XO-52YY9
               MOVE XO-52YY9               TO OUTHOST-IO-AREA (345:9)
               MOVE ';'                    TO OUTHOST-IO-AREA (354:1)
               MOVE GMLPT                  TO OUTHOST-IO-AREA (356:1)
               MOVE ';'                    TO OUTHOST-IO-AREA (357:1)
               MOVE PRITYP                 TO OUTHOST-IO-AREA (359:1)
               MOVE ';'                    TO OUTHOST-IO-AREA (360:1)
               MOVE GMERKN                 TO OUTHOST-IO-AREA (362:1)
               MOVE ';'                    TO OUTHOST-IO-AREA (363:1)
      *                        MERKN    365
               MOVE ';'                    TO OUTHOST-IO-AREA (366:1)
               MOVE GMLVAL                 TO OUTHOST-IO-AREA (368:1)
               MOVE ';'                    TO OUTHOST-IO-AREA (369:1)
               MOVE VALUTA                 TO OUTHOST-IO-AREA (371:1)
               MOVE ';'                    TO OUTHOST-IO-AREA (372:1)
      *                        GMLLEV   392
               MOVE ';'                    TO OUTHOST-IO-AREA (393:1)
      *                        LEVART   413
               MOVE ';'                    TO OUTHOST-IO-AREA (414:1)
      *                        LBETEG   444
               MOVE ';'                    TO OUTHOST-IO-AREA (445:1)
      *                        LEVBET   475
               MOVE ';'                    TO OUTHOST-IO-AREA (476:1)
               MOVE GMKINH                 TO XO-50YY9
               MOVE XO-50YY9               TO OUTHOST-IO-AREA (479:6)
               MOVE ';'                    TO OUTHOST-IO-AREA (485:1)
               MOVE KINNH                  TO XO-50YY9
               MOVE XO-50YY9               TO OUTHOST-IO-AREA (488:6)
               MOVE ';'                    TO OUTHOST-IO-AREA (494:1)
               MOVE GMVEKT                 TO XO-32YY9
               MOVE XO-32YY9               TO OUTHOST-IO-AREA (497:6)
               MOVE ';'                    TO OUTHOST-IO-AREA (503:1)
               MOVE NYVEKT                 TO XO-32YY9
               MOVE XO-32YY9               TO OUTHOST-IO-AREA (506:6)
               MOVE ';'                    TO OUTHOST-IO-AREA (512:1)
      *                        GMLENH   515
               MOVE ';'                    TO OUTHOST-IO-AREA (516:1)
      *                        ENHET    519
               MOVE ';'                    TO OUTHOST-IO-AREA (520:1)
               MOVE GPTYPE                 TO OUTHOST-IO-AREA (522:1)
               MOVE ';'                    TO OUTHOST-IO-AREA (523:1)
               MOVE PTYPE                  TO OUTHOST-IO-AREA (525:1)
               MOVE ';'                    TO OUTHOST-IO-AREA (526:1)
               MOVE EDBNR                  TO OUTHOST-IO-AREA (528:7)
               MOVE ';'                    TO OUTHOST-IO-AREA (535:1)
               MOVE 'OPPDATERT'            TO OUTHOST-IO-AREA (536:9)
               MOVE ';'                    TO OUTHOST-IO-AREA (545:1)
               WRITE OUTHOST-IO-AREA
           END-IF
           IF  (I-01 AND I-10)
               MOVE SPACES TO OUTHOST-IO-AREA
               INITIALIZE OUTHOST-IO-AREA
               MOVE ';'                    TO OUTHOST-IO-AREA (4:1)
               MOVE FIRMA                  TO OUTHOST-IO-AREA (5:3)
               MOVE ';'                    TO OUTHOST-IO-AREA (8:1)
               MOVE ';'                    TO OUTHOST-IO-AREA (14:1)
               MOVE VAREGR                 TO OUTHOST-IO-AREA (15:5)
               MOVE ';'                    TO OUTHOST-IO-AREA (20:1)
               MOVE ';'                    TO OUTHOST-IO-AREA (24:1)
               MOVE ALFA                   TO OUTHOST-IO-AREA (25:3)
               MOVE ';'                    TO OUTHOST-IO-AREA (28:1)
               MOVE ';'                    TO OUTHOST-IO-AREA (49:1)
               MOVE ARTNR                  TO OUTHOST-IO-AREA (50:20)
               MOVE ';'                    TO OUTHOST-IO-AREA (70:1)
               MOVE ';'                    TO OUTHOST-IO-AREA (101:1)
               MOVE NAVN                   TO OUTHOST-IO-AREA (102:30)
               MOVE ';'                    TO OUTHOST-IO-AREA (132:1)
               MOVE ';'                    TO OUTHOST-IO-AREA (148:1)
               MOVE PRISM                  TO XO-72YY9
               MOVE XO-72YY9               TO OUTHOST-IO-AREA (152:12)
               MOVE ';'                    TO OUTHOST-IO-AREA (164:1)
               MOVE ';'                    TO OUTHOST-IO-AREA (180:1)
      *                        PRISV 1  195
               MOVE NYUTS                  TO XO-72YY9
               MOVE XO-72YY9               TO OUTHOST-IO-AREA (184:12)
               MOVE ';'                    TO OUTHOST-IO-AREA (196:1)
               MOVE ';'                    TO OUTHOST-IO-AREA (212:1)
               MOVE PRIST                  TO XO-72YY9
               MOVE XO-72YY9               TO OUTHOST-IO-AREA (216:12)
               MOVE ';'                    TO OUTHOST-IO-AREA (228:1)
               MOVE ';'                    TO OUTHOST-IO-AREA (244:1)
      *                        PRISVE1  259
               MOVE NYVEIL                 TO XO-72YY9
               MOVE XO-72YY9               TO OUTHOST-IO-AREA (248:12)
               MOVE ';'                    TO OUTHOST-IO-AREA (260:1)
               MOVE ';'                    TO OUTHOST-IO-AREA (269:1)
               MOVE NYRAB                  TO XO-21YY9
               MOVE XO-21YY9               TO OUTHOST-IO-AREA (274:4)
               MOVE ';'                    TO OUTHOST-IO-AREA (278:1)
      *                        GMLLN1X  286
               MOVE ';'                    TO OUTHOST-IO-AREA (287:1)
               MOVE LEVNR1-IO              TO OUTHOST-IO-AREA (289:7)
               MOVE ';'                    TO OUTHOST-IO-AREA (296:1)
      *                        GMLEAN   310
               MOVE ';'                    TO OUTHOST-IO-AREA (311:1)
      *                        EAN      325
               MOVE ';'                    TO OUTHOST-IO-AREA (326:1)
               MOVE ';'                    TO OUTHOST-IO-AREA (340:1)
               MOVE PRISPT                 TO XO-52YY9
               MOVE XO-52YY9               TO OUTHOST-IO-AREA (345:9)
               MOVE ';'                    TO OUTHOST-IO-AREA (354:1)
               MOVE ';'                    TO OUTHOST-IO-AREA (357:1)
               MOVE PRITYP                 TO OUTHOST-IO-AREA (359:1)
               MOVE ';'                    TO OUTHOST-IO-AREA (360:1)
               MOVE ';'                    TO OUTHOST-IO-AREA (363:1)
      *                        MERKN    365
               MOVE ';'                    TO OUTHOST-IO-AREA (366:1)
               MOVE ';'                    TO OUTHOST-IO-AREA (369:1)
               MOVE VALUTA                 TO OUTHOST-IO-AREA (371:1)
               MOVE ';'                    TO OUTHOST-IO-AREA (372:1)
      *                        GMLLEV   392
               MOVE ';'                    TO OUTHOST-IO-AREA (393:1)
      *                        LEVART   413
               MOVE ';'                    TO OUTHOST-IO-AREA (414:1)
      *                        LBETEG   444
               MOVE ';'                    TO OUTHOST-IO-AREA (445:1)
      *                        LEVBET   475
               MOVE ';'                    TO OUTHOST-IO-AREA (476:1)
               MOVE ';'                    TO OUTHOST-IO-AREA (485:1)
               MOVE KINNH                  TO XO-50YY9
               MOVE XO-50YY9               TO OUTHOST-IO-AREA (488:6)
               MOVE ';'                    TO OUTHOST-IO-AREA (494:1)
               MOVE ';'                    TO OUTHOST-IO-AREA (503:1)
               MOVE NYVEKT                 TO XO-32YY9
               MOVE XO-32YY9               TO OUTHOST-IO-AREA (506:6)
               MOVE ';'                    TO OUTHOST-IO-AREA (512:1)
      *                        GMLENH   515
               MOVE ';'                    TO OUTHOST-IO-AREA (516:1)
      *                        ENHET    519
               MOVE ';'                    TO OUTHOST-IO-AREA (520:1)
               MOVE ';'                    TO OUTHOST-IO-AREA (523:1)
               MOVE PTYPE                  TO OUTHOST-IO-AREA (525:1)
               MOVE ';'                    TO OUTHOST-IO-AREA (526:1)
               MOVE EDBBUP                 TO OUTHOST-IO-AREA (528:7)
               MOVE ';'                    TO OUTHOST-IO-AREA (535:1)
               MOVE 'NY       '            TO OUTHOST-IO-AREA (536:9)
               MOVE ';'                    TO OUTHOST-IO-AREA (545:1)
               WRITE OUTHOST-IO-AREA
           END-IF.
 
       DETAIL-OVERFLOW SECTION.
       DETAIL-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'MERGE VAREMASTER    ' TO LISTE-IO-AREA (1:20)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (23:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMANR.'             TO LISTE-IO-AREA (3:8)
               MOVE 'ANT. MR.'             TO LISTE-IO-AREA (13:8)
               MOVE 'ANT.NMR.'             TO LISTE-IO-AREA (23:8)
      *       D  1     02 MR
      *                        KEYA      10
      *                        KEYB      30
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P)
               MOVE SPACES TO OUTHOST-IO-AREA
               INITIALIZE OUTHOST-IO-AREA
               MOVE 'GML'                  TO OUTHOST-IO-AREA (1:3)
               MOVE ';'                    TO OUTHOST-IO-AREA (4:1)
               MOVE 'NY '                  TO OUTHOST-IO-AREA (5:3)
               MOVE ';'                    TO OUTHOST-IO-AREA (8:1)
               MOVE 'GML  '                TO OUTHOST-IO-AREA (9:5)
               MOVE ';'                    TO OUTHOST-IO-AREA (14:1)
               MOVE 'NY   '                TO OUTHOST-IO-AREA (15:5)
               MOVE ';'                    TO OUTHOST-IO-AREA (20:1)
               MOVE 'GML'                  TO OUTHOST-IO-AREA (21:3)
               MOVE ';'                    TO OUTHOST-IO-AREA (24:1)
               MOVE 'NY '                  TO OUTHOST-IO-AREA (25:3)
               MOVE ';'                    TO OUTHOST-IO-AREA (28:1)
               MOVE 'GML      '            TO OUTHOST-IO-AREA (29:9)
               MOVE ';'                    TO OUTHOST-IO-AREA (49:1)
               MOVE 'NY       '            TO OUTHOST-IO-AREA (50:9)
               MOVE ';'                    TO OUTHOST-IO-AREA (70:1)
               MOVE 'GML           '       TO OUTHOST-IO-AREA (72:14)
               MOVE ';'                    TO OUTHOST-IO-AREA (101:1)
               MOVE 'NY            '       TO OUTHOST-IO-AREA (103:14)
               MOVE ';'                    TO OUTHOST-IO-AREA (132:1)
               MOVE 'GML    '              TO OUTHOST-IO-AREA (133:7)
               MOVE ';'                    TO OUTHOST-IO-AREA (148:1)
               MOVE 'NY     '              TO OUTHOST-IO-AREA (149:7)
               MOVE ';'                    TO OUTHOST-IO-AREA (164:1)
               MOVE 'GML        '          TO OUTHOST-IO-AREA (165:11)
               MOVE ';'                    TO OUTHOST-IO-AREA (180:1)
               MOVE 'NY         '          TO OUTHOST-IO-AREA (181:11)
               MOVE ';'                    TO OUTHOST-IO-AREA (196:1)
               MOVE 'GML     '             TO OUTHOST-IO-AREA (197:8)
               MOVE ';'                    TO OUTHOST-IO-AREA (212:1)
               MOVE 'NY      '             TO OUTHOST-IO-AREA (213:8)
               MOVE ';'                    TO OUTHOST-IO-AREA (228:1)
               MOVE 'GML      '            TO OUTHOST-IO-AREA (229:9)
               MOVE ';'                    TO OUTHOST-IO-AREA (244:1)
               MOVE 'NY       '            TO OUTHOST-IO-AREA (245:9)
               MOVE ';'                    TO OUTHOST-IO-AREA (260:1)
               MOVE 'GML     '             TO OUTHOST-IO-AREA (261:8)
               MOVE ';'                    TO OUTHOST-IO-AREA (269:1)
               MOVE 'NY      '             TO OUTHOST-IO-AREA (270:8)
               MOVE ';'                    TO OUTHOST-IO-AREA (278:1)
               MOVE 'GML     '             TO OUTHOST-IO-AREA (279:8)
               MOVE ';'                    TO OUTHOST-IO-AREA (287:1)
               MOVE 'NY      '             TO OUTHOST-IO-AREA (288:8)
               MOVE ';'                    TO OUTHOST-IO-AREA (296:1)
               MOVE 'GML   '               TO OUTHOST-IO-AREA (297:6)
               MOVE ';'                    TO OUTHOST-IO-AREA (311:1)
               MOVE 'NY    '               TO OUTHOST-IO-AREA (312:6)
               MOVE ';'                    TO OUTHOST-IO-AREA (326:1)
               MOVE 'GML        '          TO OUTHOST-IO-AREA (327:11)
               MOVE ';'                    TO OUTHOST-IO-AREA (340:1)
               MOVE 'NY         '          TO OUTHOST-IO-AREA (341:11)
               MOVE ';'                    TO OUTHOST-IO-AREA (354:1)
               MOVE 'GM'                   TO OUTHOST-IO-AREA (355:2)
               MOVE ';'                    TO OUTHOST-IO-AREA (357:1)
               MOVE 'NY'                   TO OUTHOST-IO-AREA (358:2)
               MOVE ';'                    TO OUTHOST-IO-AREA (360:1)
               MOVE 'GM'                   TO OUTHOST-IO-AREA (361:2)
               MOVE ';'                    TO OUTHOST-IO-AREA (363:1)
               MOVE 'NY'                   TO OUTHOST-IO-AREA (364:2)
               MOVE ';'                    TO OUTHOST-IO-AREA (366:1)
               MOVE 'GM'                   TO OUTHOST-IO-AREA (367:2)
               MOVE ';'                    TO OUTHOST-IO-AREA (369:1)
               MOVE 'NY'                   TO OUTHOST-IO-AREA (370:2)
               MOVE ';'                    TO OUTHOST-IO-AREA (372:1)
               MOVE 'GML      '            TO OUTHOST-IO-AREA (373:9)
               MOVE ';'                    TO OUTHOST-IO-AREA (393:1)
               MOVE 'NY       '            TO OUTHOST-IO-AREA (394:9)
               MOVE ';'                    TO OUTHOST-IO-AREA (414:1)
               MOVE 'GML               '   TO OUTHOST-IO-AREA (415:18)
               MOVE ';'                    TO OUTHOST-IO-AREA (445:1)
               MOVE 'NY                '   TO OUTHOST-IO-AREA (446:18)
               MOVE ';'                    TO OUTHOST-IO-AREA (476:1)
               MOVE 'GML     '             TO OUTHOST-IO-AREA (477:8)
               MOVE ';'                    TO OUTHOST-IO-AREA (485:1)
               MOVE 'NY      '             TO OUTHOST-IO-AREA (486:8)
               MOVE ';'                    TO OUTHOST-IO-AREA (494:1)
               MOVE 'GML '                 TO OUTHOST-IO-AREA (495:4)
               MOVE ';'                    TO OUTHOST-IO-AREA (503:1)
               MOVE 'NY  '                 TO OUTHOST-IO-AREA (504:4)
               MOVE ';'                    TO OUTHOST-IO-AREA (512:1)
               MOVE 'GML'                  TO OUTHOST-IO-AREA (513:3)
               MOVE ';'                    TO OUTHOST-IO-AREA (516:1)
               MOVE 'NY '                  TO OUTHOST-IO-AREA (517:3)
               MOVE ';'                    TO OUTHOST-IO-AREA (520:1)
               MOVE 'GM'                   TO OUTHOST-IO-AREA (521:2)
               MOVE ';'                    TO OUTHOST-IO-AREA (523:1)
               MOVE 'NY'                   TO OUTHOST-IO-AREA (524:2)
               MOVE ';'                    TO OUTHOST-IO-AREA (526:1)
               MOVE '        '             TO OUTHOST-IO-AREA (527:8)
               MOVE ';'                    TO OUTHOST-IO-AREA (535:1)
               MOVE '         '            TO OUTHOST-IO-AREA (536:9)
               MOVE ';'                    TO OUTHOST-IO-AREA (545:1)
               WRITE OUTHOST-IO-AREA
               MOVE SPACES TO OUTHOST-IO-AREA
               INITIALIZE OUTHOST-IO-AREA
               MOVE 'FNR'                  TO OUTHOST-IO-AREA (1:3)
               MOVE ';'                    TO OUTHOST-IO-AREA (4:1)
               MOVE 'FNR'                  TO OUTHOST-IO-AREA (5:3)
               MOVE ';'                    TO OUTHOST-IO-AREA (8:1)
               MOVE 'VGR  '                TO OUTHOST-IO-AREA (9:5)
               MOVE ';'                    TO OUTHOST-IO-AREA (14:1)
               MOVE 'VGR  '                TO OUTHOST-IO-AREA (15:5)
               MOVE ';'                    TO OUTHOST-IO-AREA (20:1)
               MOVE 'ALF'                  TO OUTHOST-IO-AREA (21:3)
               MOVE ';'                    TO OUTHOST-IO-AREA (24:1)
               MOVE 'ALF'                  TO OUTHOST-IO-AREA (25:3)
               MOVE ';'                    TO OUTHOST-IO-AREA (28:1)
               MOVE 'ARTIKELNR'            TO OUTHOST-IO-AREA (29:9)
               MOVE ';'                    TO OUTHOST-IO-AREA (49:1)
               MOVE 'ARTIKELNR'            TO OUTHOST-IO-AREA (50:9)
               MOVE ';'                    TO OUTHOST-IO-AREA (70:1)
               MOVE 'VAREBETEGNELSE'       TO OUTHOST-IO-AREA (72:14)
               MOVE ';'                    TO OUTHOST-IO-AREA (101:1)
               MOVE 'VAREBETEGNELSE'       TO OUTHOST-IO-AREA (103:14)
               MOVE ';'                    TO OUTHOST-IO-AREA (132:1)
               MOVE 'LEVPRIS'              TO OUTHOST-IO-AREA (133:7)
               MOVE ';'                    TO OUTHOST-IO-AREA (148:1)
               MOVE 'LEVPRIS'              TO OUTHOST-IO-AREA (149:7)
               MOVE ';'                    TO OUTHOST-IO-AREA (164:1)
               MOVE 'UTSALGSPRIS'          TO OUTHOST-IO-AREA (165:11)
               MOVE ';'                    TO OUTHOST-IO-AREA (180:1)
               MOVE 'UTSALGSPRIS'          TO OUTHOST-IO-AREA (181:11)
               MOVE ';'                    TO OUTHOST-IO-AREA (196:1)
               MOVE 'SELVKOST'             TO OUTHOST-IO-AREA (197:8)
               MOVE ';'                    TO OUTHOST-IO-AREA (212:1)
               MOVE 'SELVKOST'             TO OUTHOST-IO-AREA (213:8)
               MOVE ';'                    TO OUTHOST-IO-AREA (228:1)
               MOVE 'VEIL.PRIS'            TO OUTHOST-IO-AREA (229:9)
               MOVE ';'                    TO OUTHOST-IO-AREA (244:1)
               MOVE 'VEIL.PRIS'            TO OUTHOST-IO-AREA (245:9)
               MOVE ';'                    TO OUTHOST-IO-AREA (260:1)
               MOVE 'LEVRABB '             TO OUTHOST-IO-AREA (261:8)
               MOVE ';'                    TO OUTHOST-IO-AREA (269:1)
               MOVE 'LEVRABB '             TO OUTHOST-IO-AREA (270:8)
               MOVE ';'                    TO OUTHOST-IO-AREA (278:1)
               MOVE 'LEVNR 1 '             TO OUTHOST-IO-AREA (279:8)
               MOVE ';'                    TO OUTHOST-IO-AREA (287:1)
               MOVE 'LEVNR 1 '             TO OUTHOST-IO-AREA (288:8)
               MOVE ';'                    TO OUTHOST-IO-AREA (296:1)
               MOVE 'EAN NR'               TO OUTHOST-IO-AREA (397:6)
               MOVE ';'                    TO OUTHOST-IO-AREA (311:1)
               MOVE 'EAN NR'               TO OUTHOST-IO-AREA (312:6)
               MOVE ';'                    TO OUTHOST-IO-AREA (326:1)
               MOVE 'PRISTILLEGG'          TO OUTHOST-IO-AREA (327:11)
               MOVE ';'                    TO OUTHOST-IO-AREA (340:1)
               MOVE 'PRISTILLEGG'          TO OUTHOST-IO-AREA (341:11)
               MOVE ';'                    TO OUTHOST-IO-AREA (354:1)
               MOVE 'PT'                   TO OUTHOST-IO-AREA (355:2)
               MOVE ';'                    TO OUTHOST-IO-AREA (357:1)
               MOVE 'PT'                   TO OUTHOST-IO-AREA (358:2)
               MOVE ';'                    TO OUTHOST-IO-AREA (360:1)
               MOVE 'MR'                   TO OUTHOST-IO-AREA (361:2)
               MOVE ';'                    TO OUTHOST-IO-AREA (363:1)
               MOVE 'MR'                   TO OUTHOST-IO-AREA (364:2)
               MOVE ';'                    TO OUTHOST-IO-AREA (366:1)
               MOVE 'VL'                   TO OUTHOST-IO-AREA (367:2)
               MOVE ';'                    TO OUTHOST-IO-AREA (369:1)
               MOVE 'VL'                   TO OUTHOST-IO-AREA (370:2)
               MOVE ';'                    TO OUTHOST-IO-AREA (372:1)
               MOVE 'LEV ARTNR'            TO OUTHOST-IO-AREA (373:9)
               MOVE ';'                    TO OUTHOST-IO-AREA (393:1)
               MOVE 'LEV ARTNR'            TO OUTHOST-IO-AREA (394:9)
               MOVE ';'                    TO OUTHOST-IO-AREA (414:1)
               MOVE 'LEV VAREBETEGNELSE'   TO OUTHOST-IO-AREA (415:18)
               MOVE ';'                    TO OUTHOST-IO-AREA (445:1)
               MOVE 'LEV VAREBETEGNELSE'   TO OUTHOST-IO-AREA (446:18)
               MOVE ';'                    TO OUTHOST-IO-AREA (476:1)
               MOVE 'KARTONG '             TO OUTHOST-IO-AREA (456:8)
               MOVE ';'                    TO OUTHOST-IO-AREA (485:1)
               MOVE 'KARTONG '             TO OUTHOST-IO-AREA (486:8)
               MOVE ';'                    TO OUTHOST-IO-AREA (494:1)
               MOVE 'VEKT'                 TO OUTHOST-IO-AREA (495:4)
               MOVE ';'                    TO OUTHOST-IO-AREA (503:1)
               MOVE 'VEKT'                 TO OUTHOST-IO-AREA (504:4)
               MOVE ';'                    TO OUTHOST-IO-AREA (512:1)
               MOVE 'ENH'                  TO OUTHOST-IO-AREA (513:3)
               MOVE ';'                    TO OUTHOST-IO-AREA (516:1)
               MOVE 'ENH'                  TO OUTHOST-IO-AREA (517:3)
               MOVE ';'                    TO OUTHOST-IO-AREA (520:1)
               MOVE 'PT'                   TO OUTHOST-IO-AREA (521:2)
               MOVE ';'                    TO OUTHOST-IO-AREA (523:1)
               MOVE 'PT'                   TO OUTHOST-IO-AREA (524:2)
               MOVE ';'                    TO OUTHOST-IO-AREA (526:1)
               MOVE 'EDBNR   '             TO OUTHOST-IO-AREA (527:8)
               MOVE ';'                    TO OUTHOST-IO-AREA (535:1)
               MOVE 'STATUS   '            TO OUTHOST-IO-AREA (536:9)
               MOVE ';'                    TO OUTHOST-IO-AREA (545:1)
               WRITE OUTHOST-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FIRMA                  TO LISTE-IO-AREA (8:3)
               MOVE ANTMR                  TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (14:7)
               MOVE ANTNMR                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (24:7)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE TOTNMR                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (24:7)
               MOVE 'RECORDS ER KOPIERT.     ' TO LISTE-IO-AREA (32:24)
               MOVE 2                      TO LISTE-BEFORE-SPACE
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
           SET VAREBUP-LEVEL-INIT          TO TRUE
           INITIALIZE VAREBUP-DATA-FIELDS
           SET VAREBUP-EOF-OFF             TO TRUE
           SET VAREBUP-PROCESS             TO TRUE
           OPEN INPUT VAREBUP
           INITIALIZE VAREMAS-DATA-FIELDS
           OPEN I-O VAREMAS
           INITIALIZE VARETIL-DATA-FIELDS
           OPEN I-O VARETIL
           OPEN OUTPUT VAREREC
           OPEN OUTPUT NYEVART
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES
           OPEN OUTPUT OUTPUN
           OPEN OUTPUT OUTHOST.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE VAREBUP
           CLOSE VAREMAS
           CLOSE VARETIL
           CLOSE VAREREC
           CLOSE NYEVART
           IF LISTE-IO-AREA NOT = SPACES
             WRITE LISTE-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE-IO-AREA
           END-IF
           CLOSE LISTE
           CLOSE OUTPUN
           CLOSE OUTHOST.
 
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
