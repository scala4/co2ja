       IDENTIFICATION DIVISION.
       PROGRAM-ID. REG810R.
      **********************************************  Z-WIN-RPG2   ****
      * NY VERSJON AV REG.REG410                 ***TXT***OK
      *  PROGRAM....: REG810 - FØR SEPT.05-REG410                     *
      *   PROGRAM  REG810    A U T O - P O S T E R I N G.                      *
      *   LESER DAGENS RESKONTRO OG REGNSKAPS-RECORDS OG SELEKTERER
      *   RECORDS SOM ER GRUNNLAG FOR AUTO-POSTERING I FØLGE SELEKT-TABELL.    *
      *   DET BLIR OGSÅ DANNET I BILAGSNR.RECORD PR. BILAGSNR. FOR SENERE      *
      *   GRUNNLAG FOR NYTT BILAGSNR. TILDELING PÅ NYTT FIRMA.                 *
      * 16/4-93  BET.MÅTE 07 OG 14 SKAL IKKE DANNE AUTO-POSTERINGER.
      * 16/4-93  LEGGER INN TIL FIRMANR. SLIK ATT UTSKRIFTEN KOMMER I
      *          REKKEFØLGE PR. TIL FIRMA.
      * 09.08.95 FEILRETTING: EKSKLUDERER REGNSKAPSTRANSER FRA KONTANT-
      *          FAKTURERINGEN.
      * 09.04.96 UTVIDET TABELL TIL 450.
      * 01.08.96 UTVIDET TABELL TIL 500.
      * 15.01.97 UTVIDET TABELL TIL 550.
      * 08.04.97 UTVIDET TABELL TIL 600.
      * 04.08.97 UTVIDET TABELL TIL 650.
      * 22.09.98 UTVIDET TABELL TIL 700.
      * 10.03.00 UTVIDET TABELL TIL 750.
      * 18.08.00 LAGT OM FRA SYSIPT TABELL TIL VSAM-FIL.
      *          LEGGER HELE TABELLEN I BILAGSFILEN.
      * 21.02.07 UTVIDET AUTOPOT TABELL
      * 09.06.10 UTVIDET TABELL MED FILEN AUTOPOX
      * 15.04.11 UTVIDET REGNSKAPSFIL TIL 240 OG LAGT INN PROD.TIDSPKT
      **************************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: REG810.rpg
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
           SELECT DAGRESK
               ASSIGN TO UT-S-DAGRESK
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS DAGRESK-STATUS.
           SELECT DAGREGN
               ASSIGN TO UT-S-DAGREGN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS DAGREGN-STATUS.
           SELECT AUTOPOT
               ASSIGN TO AUTOPOT
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS AUTOPOT-STATUS
               RECORD KEY IS AUTOPOT-KEY1.
           SELECT AUTOPOX
               ASSIGN TO AUTOPOX
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS AUTOPOX-STATUS
               RECORD KEY IS AUTOPOX-KEY1.
           SELECT BILAGFI
               ASSIGN TO UT-S-BILAGFI
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS BILAGFI-STATUS.
           SELECT SELRESK
               ASSIGN TO UT-S-SELRESK
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS SELRESK-STATUS.
           SELECT SELREGN
               ASSIGN TO UT-S-SELREGN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS SELREGN-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD DAGRESK
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  DAGRESK-IO-AREA.
           05  DAGRESK-IO-AREA-X           PICTURE X(200).
       FD DAGREGN
               BLOCK CONTAINS 240
               RECORD CONTAINS 240.
       01  DAGREGN-IO-AREA.
           05  DAGREGN-IO-AREA-X           PICTURE X(240).
       FD AUTOPOT
               RECORD CONTAINS 200.
       01  AUTOPOT-IO-AREA.
           05  AUTOPOT-IO-AREA-X.
               10  AUTOPOT-KEY1            PICTURE X(9).
               10  FILLER                  PICTURE X(191).
       FD AUTOPOX
               RECORD CONTAINS 204.
       01  AUTOPOX-IO-AREA.
           05  AUTOPOX-IO-AREA-X.
               10  AUTOPOX-KEY1            PICTURE X(11).
               10  FILLER                  PICTURE X(193).
       FD BILAGFI
               BLOCK CONTAINS 396
               RECORD CONTAINS 396.
       01  BILAGFI-IO-AREA.
           05  BILAGFI-IO-AREA-X           PICTURE X(396).
       FD SELRESK
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  SELRESK-IO-AREA.
           05  SELRESK-IO-AREA-X           PICTURE X(200).
       FD SELREGN
               BLOCK CONTAINS 240
               RECORD CONTAINS 240.
       01  SELREGN-IO-AREA.
           05  SELREGN-IO-AREA-X           PICTURE X(240).
      *BUGFILO O   F  80  80            PRINTERSYSLST
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
           10  DAGRESK-STATUS              PICTURE 99 VALUE 0.
           10  DAGREGN-STATUS              PICTURE 99 VALUE 0.
           10  AUTOPOT-STATUS              PICTURE 99 VALUE 0.
           10  AUTOPOX-STATUS              PICTURE 99 VALUE 0.
           10  BILAGFI-STATUS              PICTURE 99 VALUE 0.
           10  SELRESK-STATUS              PICTURE 99 VALUE 0.
           10  SELREGN-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  DAGRESK-EOF-OFF         VALUE '0'.
               88  DAGRESK-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  DAGRESK-READ-OFF        VALUE '0'.
               88  DAGRESK-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  DAGRESK-PROCESS-OFF     VALUE '0'.
               88  DAGRESK-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  DAGRESK-LEVEL-INIT-OFF  VALUE '0'.
               88  DAGRESK-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  DAGREGN-EOF-OFF         VALUE '0'.
               88  DAGREGN-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  DAGREGN-READ-OFF        VALUE '0'.
               88  DAGREGN-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  DAGREGN-PROCESS-OFF     VALUE '0'.
               88  DAGREGN-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  DAGREGN-LEVEL-INIT-OFF  VALUE '0'.
               88  DAGREGN-LEVEL-INIT      VALUE '1'.
           05  AUTOPOT-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  AUTOPOX-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  DAGRESK-LEVEL-01.
               10  DAGRESK-01-L3.
                   15  DAGRESK-01-L3-FINR  PICTURE X(3).
               10  DAGRESK-01-L2.
                   15  DAGRESK-01-L2-BIART PICTURE X(1).
               10  DAGRESK-01-L1.
                   15  DAGRESK-01-L1-BINR  PICTURE X(6).
           05  DAGRESK-DATA-FIELDS.
               10  RESREC                  PICTURE X(200).
               10  FINR                    PICTURE X(3).
               10  RESKNR                  PICTURE X(6).
               10  BINR                    PICTURE X(6).
               10  BIART                   PICTURE X(1).
               10  BETBET                  PICTURE X(2).
           05  DAGRESK-MP                  PICTURE X(10).
           05  DAGRESK-MC                  PICTURE X(10).
           05  DAGRESK-M-01            REDEFINES DAGRESK-MC.
               10  DAGRESK-M-01-M3.
                   15  DAGRESK-M-01-M3-FINR-G.
                       20  DAGRESK-M-01-M3-FINR PICTURE X(3).
               10  DAGRESK-M-01-M2.
                   15  DAGRESK-M-01-M2-BIART-G.
                       20  DAGRESK-M-01-M2-BIART PICTURE X(1).
               10  DAGRESK-M-01-M1.
                   15  DAGRESK-M-01-M1-BINR-G.
                       20  DAGRESK-M-01-M1-BINR PICTURE X(6).
           05  DAGREGN-LEVEL-02.
               10  DAGREGN-02-L3.
                   15  DAGREGN-02-L3-FINR  PICTURE X(3).
               10  DAGREGN-02-L2.
                   15  DAGREGN-02-L2-BIART PICTURE X(1).
               10  DAGREGN-02-L1.
                   15  DAGREGN-02-L1-BINR  PICTURE X(6).
           05  DAGREGN-DATA-FIELDS.
               10  REGREC                  PICTURE X(240).
               10  FRAFAK                  PICTURE X(1).
           05  DAGREGN-MP                  PICTURE X(10).
           05  DAGREGN-MC                  PICTURE X(10).
           05  DAGREGN-M-02            REDEFINES DAGREGN-MC.
               10  DAGREGN-M-02-M3.
                   15  DAGREGN-M-02-M3-FINR-G.
                       20  DAGREGN-M-02-M3-FINR PICTURE X(3).
               10  DAGREGN-M-02-M2.
                   15  DAGREGN-M-02-M2-BIART-G.
                       20  DAGREGN-M-02-M2-BIART PICTURE X(1).
               10  DAGREGN-M-02-M1.
                   15  DAGREGN-M-02-M1-BINR-G.
                       20  DAGREGN-M-02-M1-BINR PICTURE X(6).
           05  AUTOPOT-DATA-FIELDS.
               10  TFINR                   PICTURE X(3).
               10  TRSKNR                  PICTURE X(6).
               10  TAVD                    PICTURE X(4).
               10  VGR28                   PICTURE X(35).
               10  VGR914                  PICTURE X(30).
               10  KTO18                   PICTURE X(40).
               10  KTO914                  PICTURE X(30).
               10  VG1518                  PICTURE X(20).
               10  KT1518                  PICTURE X(20).
               10  SLETT                   PICTURE X(1).
           05  AUTOPOX-DATA-FIELDS.
               10  XVGRP                   PICTURE X(90).
               10  XKTO                    PICTURE X(90).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(1).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  APKEY                   PICTURE X(9).
               10  AXKEY1                  PICTURE X(9).
               10  AXKEY                   PICTURE X(11).
               10  ANTBIL-IO.
                   15  ANTBIL              PICTURE S9(5).
               10  ANTRES-IO.
                   15  ANTRES              PICTURE S9(5).
               10  ANTREG-IO.
                   15  ANTREG              PICTURE S9(5).
           05  EDITTING-FIELDS.
               10  XO-50YY9                PICTURE ZZ.ZZ9.
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
               88  NOT-CALL-MATCH-RECS     VALUE '0'.
               88  CALL-MATCH-RECS         VALUE '1'.
           05  FILLER                      PICTURE X.
               88  NOT-SET-I-MR            VALUE '0'.
               88  SET-I-MR                VALUE '1'.
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
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  DAGRESK-PROCESS
               SET DAGRESK-PROCESS-OFF     TO TRUE
               SET DAGRESK-READ            TO TRUE
           END-IF
 
           IF  DAGRESK-READ
               PERFORM DAGRESK-GET
               SET DAGRESK-READ-OFF        TO TRUE
               IF  NOT DAGRESK-EOF
                   PERFORM DAGRESK-MATCH-SET
               END-IF
           END-IF
 
           IF  DAGREGN-PROCESS
               SET DAGREGN-PROCESS-OFF     TO TRUE
               SET DAGREGN-READ            TO TRUE
           END-IF
 
           IF  DAGREGN-READ
               PERFORM DAGREGN-GET
               SET DAGREGN-READ-OFF        TO TRUE
               IF  NOT DAGREGN-EOF
                   PERFORM DAGREGN-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  DAGRESK-PROCESS
               PERFORM DAGRESK-IDSET
           END-IF
 
           IF  DAGREGN-PROCESS
               PERFORM DAGREGN-IDSET
           END-IF
 
           IF  DAGRESK-PROCESS
               PERFORM DAGRESK-CHK-LEVEL
           END-IF
 
           IF  DAGREGN-PROCESS
               PERFORM DAGREGN-CHK-LEVEL
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
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  DAGRESK-PROCESS
               PERFORM DAGRESK-FLDSET
           END-IF
 
           IF  DAGREGN-PROCESS
               PERFORM DAGREGN-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  DAGRESK-PROCESS
           OR  DAGREGN-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-L1)
               SET NOT-I-50                TO TRUE
      ****************************************************************
      *  SJEKK OM KONTANT ELLER OPPKRAV- SALG.                       *
      *  - SKAL IKKE VÆRE MED                                        *
      ****************************************************************
           END-IF
           IF  (I-L1 AND I-01)
               SET NOT-I-14                TO TRUE
               IF  BETBET = '07'
                   SET I-14                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-01 AND NOT-I-14)
               SET NOT-I-14                TO TRUE
               IF  BETBET = '14'
                   SET I-14                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-02 AND NOT-I-14)
               SET NOT-I-14                TO TRUE
               IF  FRAFAK = 'K'
                   SET I-14                TO TRUE
               END-IF
           END-IF
           IF  (I-14)
               GO TO SLUTT-T
      ****************************************************************
      *  SJEKK OM BILAGSART 2 ELLER 3 (UTGÅENDE FAKTURAER OG KR.NOTA)*
      *  - BARE DE SKAL VÆRE MED                                     *
      ****************************************************************
           END-IF
           IF  (I-L1)
               SET NOT-I-50                TO TRUE
               IF  BIART = '2'
                   SET I-50                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-50)
               SET NOT-I-50                TO TRUE
               IF  BIART = '3'
                   SET I-50                TO TRUE
               END-IF
      ****************************************************************
      *  LESER AUTOPOT, TABELL MED AVTALER OM AUTOPOSTERING, HVIS    *
      *  MATCH/FINNES SKAL DEN VÆRE MED.                             *
      ****************************************************************
           END-IF
           IF  (I-L1 AND I-50)
               MOVE FINR                   TO APKEY (1:3)
               MOVE RESKNR                 TO APKEY (4:6)
               MOVE APKEY                  TO AUTOPOT-KEY1
               READ AUTOPOT RECORD KEY IS AUTOPOT-KEY1
               INVALID KEY
                   SET I-40                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-40            TO TRUE
                   PERFORM AUTOPOT-FLDSET
                   PERFORM AUTOPOT-IDSET
               END-READ
           END-IF
           IF  (I-L1 AND I-50 AND NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  SLETT = 'S'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-40 AND I-50)
               SET NOT-I-50                TO TRUE
           END-IF
           IF  (I-L1 AND I-50)
               MOVE FINR                   TO AXKEY1 (1:3)
               MOVE RESKNR                 TO AXKEY1 (4:6)
               MOVE AXKEY1                 TO AXKEY (1:9)
               MOVE '01'                   TO AXKEY (10:2)
               MOVE AXKEY                  TO AUTOPOX-KEY1
               READ AUTOPOX RECORD KEY IS AUTOPOX-KEY1
               INVALID KEY
                   SET I-41                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-41            TO TRUE
                   PERFORM AUTOPOX-FLDSET
                   PERFORM AUTOPOX-IDSET
               END-READ
               ADD 1                       TO ANTBIL
      ******************************************************
           END-IF
           IF  (I-01 AND I-50)
               ADD 1                       TO ANTRES
           END-IF
           IF  (I-02 AND I-50)
               ADD 1                       TO ANTREG
      *  L1 50             MOVE "VGR28   "BUGFL1  8        DISPLAY FIELD
      *  L1 50   BUGFL1    DEBUGBUGFILO   VGR28            VIS INDIKATOR
           END-IF
           .
 
       SLUTT-T.
           CONTINUE.
 
       DAGRESK-GET SECTION.
       DAGRESK-GET-P.
           IF  DAGRESK-EOF-OFF
               READ DAGRESK
               AT END
                   SET DAGRESK-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       DAGRESK-FLDSET SECTION.
       DAGRESK-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE DAGRESK-IO-AREA (1:200) TO RESREC (1:200)
               MOVE DAGRESK-IO-AREA (3:3)  TO FINR (1:3)
               MOVE DAGRESK-IO-AREA (6:6)  TO RESKNR (1:6)
               MOVE DAGRESK-IO-AREA (26:6) TO BINR (1:6)
               MOVE DAGRESK-IO-AREA (32:1) TO BIART (1:1)
               MOVE DAGRESK-IO-AREA (49:2) TO BETBET (1:2)
           END-EVALUATE.
 
       DAGRESK-IDSET SECTION.
       DAGRESK-IDSET-P.
           SET I-01                        TO TRUE.
 
       DAGRESK-CHK-LEVEL SECTION.
       DAGRESK-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO DAGRESK-LEVEL-01
               MOVE DAGRESK-IO-AREA (3:3)  TO DAGRESK-01-L3-FINR
               MOVE DAGRESK-IO-AREA (32:1) TO DAGRESK-01-L2-BIART
               MOVE DAGRESK-IO-AREA (26:6) TO DAGRESK-01-L1-BINR
               IF  DAGRESK-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  DAGRESK-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  DAGRESK-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  DAGRESK-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  DAGRESK-01-L3         TO THE-PRIOR-L3
               MOVE  DAGRESK-01-L2         TO THE-PRIOR-L2
               MOVE  DAGRESK-01-L1         TO THE-PRIOR-L1
               SET DAGRESK-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       DAGRESK-MATCH-SET SECTION.
       DAGRESK-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE DAGRESK-IO-AREA (3:3)  TO DAGRESK-M-01-M3-FINR
               MOVE DAGRESK-IO-AREA (32:1) TO DAGRESK-M-01-M2-BIART
               MOVE DAGRESK-IO-AREA (26:6) TO DAGRESK-M-01-M1-BINR
           END-EVALUATE.
 
       DAGREGN-GET SECTION.
       DAGREGN-GET-P.
           IF  DAGREGN-EOF-OFF
               READ DAGREGN
               AT END
                   SET DAGREGN-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       DAGREGN-FLDSET SECTION.
       DAGREGN-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE DAGREGN-IO-AREA (1:240) TO REGREC (1:240)
               MOVE DAGREGN-IO-AREA (3:3)  TO FINR (1:3)
               MOVE DAGREGN-IO-AREA (19:6) TO RESKNR (1:6)
               MOVE DAGREGN-IO-AREA (7:6)  TO BINR (1:6)
               MOVE DAGREGN-IO-AREA (6:1)  TO BIART (1:1)
               MOVE DAGREGN-IO-AREA (39:1) TO FRAFAK (1:1)
           END-EVALUATE.
 
       DAGREGN-IDSET SECTION.
       DAGREGN-IDSET-P.
           SET I-02                        TO TRUE.
 
       DAGREGN-CHK-LEVEL SECTION.
       DAGREGN-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO DAGREGN-LEVEL-02
               MOVE DAGREGN-IO-AREA (3:3)  TO DAGREGN-02-L3-FINR
               MOVE DAGREGN-IO-AREA (6:1)  TO DAGREGN-02-L2-BIART
               MOVE DAGREGN-IO-AREA (7:6)  TO DAGREGN-02-L1-BINR
               IF  DAGREGN-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  DAGREGN-02-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  DAGREGN-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  DAGREGN-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  DAGREGN-02-L3         TO THE-PRIOR-L3
               MOVE  DAGREGN-02-L2         TO THE-PRIOR-L2
               MOVE  DAGREGN-02-L1         TO THE-PRIOR-L1
               SET DAGREGN-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       DAGREGN-MATCH-SET SECTION.
       DAGREGN-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE DAGREGN-IO-AREA (3:3)  TO DAGREGN-M-02-M3-FINR
               MOVE DAGREGN-IO-AREA (6:1)  TO DAGREGN-M-02-M2-BIART
               MOVE DAGREGN-IO-AREA (7:6)  TO DAGREGN-M-02-M1-BINR
           END-EVALUATE.
 
       AUTOPOT-FLDSET SECTION.
       AUTOPOT-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE AUTOPOT-IO-AREA (10:3) TO TFINR (1:3)
               MOVE AUTOPOT-IO-AREA (13:6) TO TRSKNR (1:6)
               MOVE AUTOPOT-IO-AREA (19:4) TO TAVD (1:4)
               MOVE AUTOPOT-IO-AREA (23:35) TO VGR28 (1:35)
               MOVE AUTOPOT-IO-AREA (58:30) TO VGR914 (1:30)
               MOVE AUTOPOT-IO-AREA (88:40) TO KTO18 (1:40)
               MOVE AUTOPOT-IO-AREA (128:30) TO KTO914 (1:30)
               MOVE AUTOPOT-IO-AREA (159:20) TO VG1518 (1:20)
               MOVE AUTOPOT-IO-AREA (179:20) TO KT1518 (1:20)
               MOVE AUTOPOT-IO-AREA (158:1) TO SLETT (1:1)
           END-EVALUATE.
 
       AUTOPOT-IDSET SECTION.
       AUTOPOT-IDSET-P.
           SET I-03                        TO TRUE.
 
       AUTOPOX-FLDSET SECTION.
       AUTOPOX-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE AUTOPOX-IO-AREA (25:90) TO XVGRP (1:90)
               MOVE AUTOPOX-IO-AREA (115:90) TO XKTO (1:90)
           END-EVALUATE.
 
       AUTOPOX-IDSET SECTION.
       AUTOPOX-IDSET-P.
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
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  DAGRESK-EOF
               MOVE HIGH-VALUES            TO DAGRESK-MC
                                              DAGRESK-MP
           END-IF
           IF  DAGREGN-EOF
               MOVE HIGH-VALUES            TO DAGREGN-MC
                                              DAGREGN-MP
           END-IF
           IF  DAGRESK-MC < DAGRESK-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  DAGREGN-MC < DAGREGN-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  DAGRESK-MC < DAGREGN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET DAGRESK-PROCESS     TO TRUE
                   MOVE DAGRESK-MC         TO DAGRESK-MP
                   IF  DAGRESK-MC = DAGREGN-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  DAGREGN-MC < DAGRESK-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET DAGREGN-PROCESS     TO TRUE
                   MOVE DAGREGN-MC         TO DAGREGN-MP
                   IF  DAGREGN-MC = DAGRESK-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  DAGRESK-MC = DAGREGN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET DAGRESK-PROCESS     TO TRUE
                   MOVE DAGRESK-MC         TO DAGRESK-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-50)
               MOVE SPACES TO SELRESK-IO-AREA
               INITIALIZE SELRESK-IO-AREA
               MOVE RESREC                 TO SELRESK-IO-AREA (1:200)
               MOVE TFINR                  TO SELRESK-IO-AREA (86:3)
               WRITE SELRESK-IO-AREA
           END-IF
           IF  (I-02 AND I-50)
               MOVE SPACES TO SELREGN-IO-AREA
               INITIALIZE SELREGN-IO-AREA
               MOVE REGREC                 TO SELREGN-IO-AREA (1:240)
               MOVE TFINR                  TO SELREGN-IO-AREA (118:3)
               WRITE SELREGN-IO-AREA
           END-IF
           IF  (I-L1 AND I-50)
               MOVE SPACES TO BILAGFI-IO-AREA
               INITIALIZE BILAGFI-IO-AREA
               MOVE FINR                   TO BILAGFI-IO-AREA (1:3)
               MOVE BIART                  TO BILAGFI-IO-AREA (4:1)
               MOVE BINR                   TO BILAGFI-IO-AREA (5:6)
               MOVE TFINR                  TO BILAGFI-IO-AREA (11:3)
               MOVE '0'                    TO BILAGFI-IO-AREA (14:1)
               MOVE '      '               TO BILAGFI-IO-AREA (15:6)
               MOVE TRSKNR                 TO BILAGFI-IO-AREA (21:6)
               MOVE RESKNR                 TO BILAGFI-IO-AREA (27:6)
               MOVE TAVD                   TO BILAGFI-IO-AREA (33:4)
               MOVE 'XXXXX'                TO BILAGFI-IO-AREA (37:5)
               MOVE VGR28                  TO BILAGFI-IO-AREA (42:35)
               MOVE VGR914                 TO BILAGFI-IO-AREA (77:30)
               MOVE VG1518                 TO BILAGFI-IO-AREA (107:20)
               IF  (NOT-I-41)
                   MOVE XVGRP              TO BILAGFI-IO-AREA (127:90)
               END-IF
               MOVE KTO18                  TO BILAGFI-IO-AREA (217:40)
               MOVE KTO914                 TO BILAGFI-IO-AREA (257:30)
               MOVE KT1518                 TO BILAGFI-IO-AREA (287:20)
               IF  (NOT-I-41)
                   MOVE XKTO               TO BILAGFI-IO-AREA (307:90)
               END-IF
               WRITE BILAGFI-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'AVSTEMMING PROG. REG810 ' TO LISTE-IO-AREA (1:24)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-BEFORE-SPACE
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTRES                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (4:6)
               MOVE 'RESKONTRORECORD KOPIERT.' TO LISTE-IO-AREA (11:24)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTREG                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (4:6)
               MOVE 'REGNSKAPSRECORD KOPIERT.' TO LISTE-IO-AREA (11:24)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTBIL                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (4:6)
               MOVE 'BILAGSNR. SELEKTERT.    ' TO LISTE-IO-AREA (11:24)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR AND I-U1 AND I-03)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ' '                    TO LISTE-IO-AREA (34:1)
               MOVE 2                      TO LISTE-AFTER-SPACE
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
           SET DAGRESK-LEVEL-INIT          TO TRUE
           INITIALIZE DAGRESK-DATA-FIELDS
           SET DAGRESK-EOF-OFF             TO TRUE
           SET DAGRESK-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO DAGRESK-MC
                                              DAGRESK-MP
           OPEN INPUT DAGRESK
           SET DAGREGN-LEVEL-INIT          TO TRUE
           INITIALIZE DAGREGN-DATA-FIELDS
           SET DAGREGN-EOF-OFF             TO TRUE
           SET DAGREGN-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO DAGREGN-MC
                                              DAGREGN-MP
           OPEN INPUT DAGREGN
           INITIALIZE AUTOPOT-DATA-FIELDS
           OPEN INPUT AUTOPOT
           INITIALIZE AUTOPOX-DATA-FIELDS
           OPEN INPUT AUTOPOX
           OPEN OUTPUT BILAGFI
           OPEN OUTPUT SELRESK
           OPEN OUTPUT SELREGN
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE DAGRESK
           CLOSE DAGREGN
           CLOSE AUTOPOT
           CLOSE AUTOPOX
           CLOSE BILAGFI
           CLOSE SELRESK
           CLOSE SELREGN
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
 
       SETOFF-I-M SECTION.
           SET NOT-I-M1                    TO TRUE.
           SET NOT-I-M2                    TO TRUE.
           SET NOT-I-M3                    TO TRUE.
           SET NOT-I-M4                    TO TRUE.
           SET NOT-I-M5                    TO TRUE.
           SET NOT-I-M6                    TO TRUE.
           SET NOT-I-M7                    TO TRUE.
           SET NOT-I-M8                    TO TRUE.
           SET NOT-I-M9                    TO TRUE.
 
       SETON-I-M9 SECTION.
           SET I-M9                        TO TRUE.
           PERFORM SETON-I-M8.
 
       SETON-I-M8 SECTION.
           SET I-M8                        TO TRUE.
           PERFORM SETON-I-M7.
 
       SETON-I-M7 SECTION.
           SET I-M7                        TO TRUE.
           PERFORM SETON-I-M6.
 
       SETON-I-M6 SECTION.
           SET I-M6                        TO TRUE.
           PERFORM SETON-I-M5.
 
       SETON-I-M5 SECTION.
           SET I-M5                        TO TRUE.
           PERFORM SETON-I-M4.
 
       SETON-I-M4 SECTION.
           SET I-M4                        TO TRUE.
           PERFORM SETON-I-M3.
 
       SETON-I-M3 SECTION.
           SET I-M3                        TO TRUE.
           PERFORM SETON-I-M2.
 
       SETON-I-M2 SECTION.
           SET I-M2                        TO TRUE.
           PERFORM SETON-I-M1.
 
       SETON-I-M1 SECTION.
           SET I-M1                        TO TRUE.
 
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
