       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK800R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM....: FAK800                                          *
      *  PROGRAMERER: ESPEN LARSEN                                    *
      *  PROGRAMMERT: 11.09.2003                                      *
      *  RETTET.....: 12.09.2003                                      *
      *                                                               *
      *  SALGSOPPGAVE PR. ORDREMOTTAGER. SUM ANT.ORDRE/KR.ORDRE OG   *
      *  TOTAL SALGSBELØP OG BRUTTOFJ.BELØP. DENNE MND + HITIL I ÅR. *
      *  UPSI 1 LISTER UT ALLE ORDRE.                                *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK800.rpg
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
           SELECT PARAM
               ASSIGN TO UT-S-PARAM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PARAM-STATUS.
           SELECT FAKPAR
               ASSIGN TO UT-S-FAKPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKPAR-STATUS.
           SELECT FAKSALG
               ASSIGN TO UT-S-FAKSALG
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKSALG-STATUS.
           SELECT STATTAB
               ASSIGN TO STATTAB
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS STATTAB-STATUS
               RECORD KEY IS STATTAB-KEY1.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD PARAM
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  PARAM-IO-AREA.
           05  PARAM-IO-AREA-X             PICTURE X(80).
       FD FAKPAR
               BLOCK CONTAINS 200
               RECORD CONTAINS 200.
       01  FAKPAR-IO-AREA.
           05  FAKPAR-IO-AREA-X            PICTURE X(200).
       FD FAKSALG
               BLOCK CONTAINS 320
               RECORD CONTAINS 160.
       01  FAKSALG-IO-AREA.
           05  FAKSALG-IO-AREA-X           PICTURE X(160).
       FD STATTAB
               RECORD CONTAINS 40.
       01  STATTAB-IO-AREA.
           05  STATTAB-IO-AREA-X.
               10  STATTAB-KEY1            PICTURE X(8).
               10  FILLER                  PICTURE X(32).
      * **START RBS - DATASTRUKTUR FOR SUB-PROGRAM RBS000 ********
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
           10  PARAM-STATUS                PICTURE 99 VALUE 0.
           10  FAKPAR-STATUS               PICTURE 99 VALUE 0.
           10  FAKSALG-STATUS              PICTURE 99 VALUE 0.
           10  STATTAB-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-EOF-OFF           VALUE '0'.
               88  PARAM-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-READ-OFF          VALUE '0'.
               88  PARAM-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-PROCESS-OFF       VALUE '0'.
               88  PARAM-PROCESS           VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKSALG-EOF-OFF         VALUE '0'.
               88  FAKSALG-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKSALG-READ-OFF        VALUE '0'.
               88  FAKSALG-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKSALG-PROCESS-OFF     VALUE '0'.
               88  FAKSALG-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  FAKSALG-LEVEL-INIT-OFF  VALUE '0'.
               88  FAKSALG-LEVEL-INIT      VALUE '1'.
           05  STATTAB-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  LDATA-XX-DATA-FIELDS.
               10  LONR                    PICTURE X(5).
               10  FILLER                  PICTURE X(252).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(5).
               10  LFIRMA                  PICTURE X(3).
               10  FILLER                  PICTURE X(249).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(8).
               10  LUNDGR                  PICTURE X(3).
               10  FILLER                  PICTURE X(246).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(11).
               10  LPROG                   PICTURE X(8).
               10  FILLER                  PICTURE X(238).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(19).
               10  LANTX-IO.
                   15  LANTX               PICTURE S9(3).
               10  FILLER                  PICTURE X(235).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(22).
               10  FINAVN                  PICTURE X(30).
               10  FILLER                  PICTURE X(205).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(52).
               10  LOPNVN                  PICTURE X(35).
               10  FILLER                  PICTURE X(170).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(87).
               10  LPRIID                  PICTURE X(4).
               10  FILLER                  PICTURE X(166).
      *     *  BESTILLINGSOPPGAVER (OVERSTYRING AV RBS-FILE) *    *
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(91).
               10  BJOBN                   PICTURE X(8).
               10  FILLER                  PICTURE X(158).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(99).
               10  BBEST                   PICTURE X(1).
               10  FILLER                  PICTURE X(157).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(100).
               10  BPERS                   PICTURE X(30).
               10  FILLER                  PICTURE X(127).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(130).
               10  BETTB                   PICTURE X(40).
               10  FILLER                  PICTURE X(87).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(170).
               10  BFORS                   PICTURE X(40).
               10  FILLER                  PICTURE X(47).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(210).
               10  BMEMO                   PICTURE X(40).
               10  FILLER                  PICTURE X(7).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(250).
               10  BANTX-IO.
                   15  BANTX               PICTURE S9(3).
               10  FILLER                  PICTURE X(4).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(253).
               10  BPCLAS                  PICTURE X(1).
               10  FILLER                  PICTURE X(3).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(254).
               10  BPRJE                   PICTURE X(3).
      * * END - RBS - DATASTRUKTUR FOR SUB-PROGRAM RBSH01 ********
      * * START RBS - PARAMETERKORT OVERSTYRING AV RBS-FILE  * *
           05  PARAM-DATA-FIELDS.
               10  PJOBN                   PICTURE X(8).
               10  PRKODE                  PICTURE X(1).
               10  PPERS                   PICTURE X(30).
               10  PANTX-IO.
                   15  PANTX               PICTURE S9(3).
               10  PETTB                   PICTURE X(40).
               10  PFORS                   PICTURE X(40).
               10  PMEMO                   PICTURE X(40).
               10  PMND                    PICTURE X(2).
               10  PAAR                    PICTURE X(2).
      * * END   RBS - PARAMETERKORT OVERSTYRING AV RBS-FILE  * *
           05  FAKPAR-DATA-FIELDS.
               10  PARAAR                  PICTURE X(4).
               10  PARMND                  PICTURE X(2).
               10  PARMAR                  PICTURE X(6).
               10  PARMNA                  PICTURE X(9).
           05  FAKSALG-LEVEL-01.
               10  FAKSALG-01-L3.
                   15  FAKSALG-01-L3-FIRMA PICTURE X(3).
               10  FAKSALG-01-L2.
                   15  FAKSALG-01-L2-ORDMOT PICTURE X(2).
               10  FAKSALG-01-L1.
                   15  FAKSALG-01-L1-ORDNR PICTURE X(6).
           05  FAKSALG-DATA-FIELDS.
               10  FAKAAR                  PICTURE X(4).
               10  FAKMND                  PICTURE X(2).
               10  KUNDNR                  PICTURE X(6).
               10  SEQNR                   PICTURE X(5).
               10  ORDDTO                  PICTURE X(8).
               10  ORDTID                  PICTURE X(4).
               10  ORDNR                   PICTURE X(6).
               10  FIRMA                   PICTURE X(3).
               10  BM                      PICTURE X(2).
               10  LK                      PICTURE X(2).
               10  AVD                     PICTURE X(1).
               10  FK                      PICTURE X(1).
               10  KRTYPE                  PICTURE X(1).
               10  KOSTN                   PICTURE X(1).
               10  SERVO                   PICTURE X(1).
               10  VGR                     PICTURE X(5).
               10  HND                     PICTURE X(3).
               10  BK                      PICTURE X(1).
               10  EDBNR                   PICTURE X(7).
               10  ALFAK                   PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  ART8F                   PICTURE X(8).
               10  ANTB-IO.
                   15  ANTB                PICTURE S9(5)V9(2).
               10  ANTL-IO.
                   15  ANTL                PICTURE S9(5)V9(2).
               10  ORDPRI-IO.
                   15  ORDPRI              PICTURE S9(7)V9(2).
               10  RAB1-IO.
                   15  RAB1                PICTURE S9(2)V9(1).
               10  RAB2-IO.
                   15  RAB2                PICTURE S9(2)V9(1).
               10  RAB3-IO.
                   15  RAB3                PICTURE S9(2)V9(1).
               10  NTOSUM-IO.
                   15  NTOSUM              PICTURE S9(7)V9(2).
               10  SVSSUM-IO.
                   15  SVSSUM              PICTURE S9(7)V9(2).
               10  FAKTNR                  PICTURE X(6).
               10  ORDTYP                  PICTURE X(1).
               10  FAKOMG                  PICTURE X(1).
               10  ORDMOT                  PICTURE X(2).
               10  PLUKKA                  PICTURE X(2).
           05  STATTAB-DATA-FIELDS.
               10  OMNAVN                  PICTURE X(30).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(2).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  F5                      PICTURE X(5).
               10  F3                      PICTURE X(3).
               10  TABKEY                  PICTURE X(8).
               10  ORDSA2-IO.
                   15  ORDSA2              PICTURE S9(8)V9(2).
               10  BFJSA2-IO.
                   15  BFJSA2              PICTURE S9(8)V9(2).
               10  ANTOA2-IO.
                   15  ANTOA2              PICTURE S9(5).
               10  ANTKA2-IO.
                   15  ANTKA2              PICTURE S9(5).
               10  ORDSM2-IO.
                   15  ORDSM2              PICTURE S9(8)V9(2).
               10  BFJSM2-IO.
                   15  BFJSM2              PICTURE S9(8)V9(2).
               10  ANTOM2-IO.
                   15  ANTOM2              PICTURE S9(5).
               10  ANTKM2-IO.
                   15  ANTKM2              PICTURE S9(5).
               10  ORDSA1-IO.
                   15  ORDSA1              PICTURE S9(8)V9(2).
               10  BFJSA1-IO.
                   15  BFJSA1              PICTURE S9(8)V9(2).
               10  ANTOA1-IO.
                   15  ANTOA1              PICTURE S9(5).
               10  ANTKA1-IO.
                   15  ANTKA1              PICTURE S9(5).
               10  ORDSM1-IO.
                   15  ORDSM1              PICTURE S9(8)V9(2).
               10  BFJSM1-IO.
                   15  BFJSM1              PICTURE S9(8)V9(2).
               10  ANTOM1-IO.
                   15  ANTOM1              PICTURE S9(5).
               10  ANTKM1-IO.
                   15  ANTKM1              PICTURE S9(5).
               10  ORDSMU-IO.
                   15  ORDSMU              PICTURE S9(8)V9(2).
               10  BFJSMU-IO.
                   15  BFJSMU              PICTURE S9(8)V9(2).
               10  BRFJ-IO.
                   15  BRFJ                PICTURE S9(8)V9(2).
               10  F12-IO.
                   15  F12                 PICTURE S9(10)V9(2).
               10  BFPSMU-IO.
                   15  BFPSMU              PICTURE S9(3)V9(1).
               10  BFPSM1-IO.
                   15  BFPSM1              PICTURE S9(3)V9(1).
               10  BFPSA1-IO.
                   15  BFPSA1              PICTURE S9(3)V9(1).
               10  L2NETM-IO.
                   15  L2NETM              PICTURE S9(5).
               10  L2NETA-IO.
                   15  L2NETA              PICTURE S9(5).
               10  L2GJTM-IO.
                   15  L2GJTM              PICTURE S9(8)V9(2).
               10  L2GJTA-IO.
                   15  L2GJTA              PICTURE S9(8)V9(2).
               10  BFPSM2-IO.
                   15  BFPSM2              PICTURE S9(3)V9(1).
               10  BFPSA2-IO.
                   15  BFPSA2              PICTURE S9(3)V9(1).
               10  L3NETM-IO.
                   15  L3NETM              PICTURE S9(5).
               10  L3NETA-IO.
                   15  L3NETA              PICTURE S9(5).
               10  L3GJTM-IO.
                   15  L3GJTM              PICTURE S9(8)V9(2).
               10  L3GJTA-IO.
                   15  L3GJTA              PICTURE S9(8)V9(2).
           05  EDITTING-FIELDS.
               10  XO-40YNZ                PICTURE ZZZZ.
               10  XO-82YY9R               PICTURE ZZ.ZZZ.ZZZ,99-.
               10  XO-31YY9R               PICTURE ZZZ,9-.
               10  XO-50YY9                PICTURE ZZ.ZZ9.
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
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
           PERFORM HEADING-OUTPUT
           IF  NOT-SET-I-OF
               SET NOT-I-OF                TO TRUE
           END-IF
           SET NOT-SET-I-OF                TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-81                    TO TRUE
           SET NOT-I-82                    TO TRUE
           SET NOT-I-83                    TO TRUE
           SET NOT-I-84                    TO TRUE
           SET NOT-I-85                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-01                    TO TRUE
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  PARAM-PROCESS
               SET PARAM-PROCESS-OFF       TO TRUE
               SET PARAM-READ              TO TRUE
           END-IF
 
           IF  PARAM-READ
           AND RECORD-SELECTED-OFF
               PERFORM PARAM-GET
               SET PARAM-READ-OFF          TO TRUE
               IF  NOT PARAM-EOF
                   PERFORM PARAM-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET PARAM-PROCESS       TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  FAKSALG-PROCESS
               SET FAKSALG-PROCESS-OFF     TO TRUE
               SET FAKSALG-READ            TO TRUE
           END-IF
 
           IF  FAKSALG-READ
           AND RECORD-SELECTED-OFF
               PERFORM FAKSALG-GET
               SET FAKSALG-READ-OFF        TO TRUE
               IF  NOT FAKSALG-EOF
                   SET FAKSALG-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  PARAM-PROCESS
               PERFORM PARAM-IDSET
           END-IF
 
           IF  FAKSALG-PROCESS
               PERFORM FAKSALG-IDSET
           END-IF
 
           IF  FAKSALG-PROCESS
               PERFORM FAKSALG-CHK-LEVEL
           END-IF
 
           IF I-1ST
               GO TO LR-INDICATOR-TEST
           END-IF.
 
       MAINLINE-TOTAL-CALCS.
           PERFORM TOTAL-CALCS
           PERFORM TOTAL-OUTPUT.
 
       LR-INDICATOR-TEST.
           IF  I-LR
               GO TO MAINLINE-TERMINATION
           END-IF
           PERFORM HEADING-OVERFLOW
 
           IF  PARAM-PROCESS
               PERFORM PARAM-FLDSET
           END-IF
 
           IF  FAKSALG-PROCESS
               PERFORM FAKSALG-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  FAKSALG-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-L2)
               SET NOT-I-50                TO TRUE
               SET NOT-I-61                TO TRUE
               SET NOT-I-62                TO TRUE
      **************************************************************
      *    RUTINE FOR OVERSTYRING AV RBS-FILE VED BESTILLINGSJOB"S *
      **************************************************************
           END-IF
           IF  (I-81)
               MOVE PJOBN                  TO BJOBN
               SET NOT-I-89                TO TRUE
               IF  PRKODE = 'B'
                   SET I-89                TO TRUE
               END-IF
               MOVE PRKODE                 TO BBEST
           END-IF
           IF  (I-81 AND I-89)
               MOVE PPERS                  TO BPERS
               MOVE PANTX                  TO BANTX-IO
           END-IF
           IF  (I-82 AND I-89)
               MOVE PETTB                  TO BETTB
           END-IF
           IF  (I-83 AND I-89)
               MOVE PFORS                  TO BFORS
           END-IF
           IF  (I-84 AND I-89)
               MOVE PMEMO                  TO BMEMO
           END-IF
           IF  (I-85)
               SET NOT-I-80                TO TRUE
               IF  PMND > '00'
                   SET I-80                TO TRUE
               END-IF
      **************************************************************
           END-IF
           IF  (I-01 AND NOT-I-30)
               READ FAKPAR
               AT END
                   SET I-H0                TO TRUE
                   MOVE 'M'                TO E-R-R-O-R
               NOT AT END
                   PERFORM FAKPAR-IDCHK
                   PERFORM FAKPAR-FLDSET
                   PERFORM FAKPAR-IDSET
               END-READ
           END-IF
           IF  (I-01 AND NOT-I-30 AND I-80)
               PERFORM MNDRUT-S
           END-IF
           IF  (I-01 AND NOT-I-30 AND I-80)
               MOVE PMND                   TO PARMND
               MOVE PAAR                   TO PARAAR (3:2)
           END-IF
           IF  (I-01 AND NOT-I-30)
               SET I-30                    TO TRUE
           END-IF
           IF  (I-L3)
               PERFORM RBSRUT-S
           END-IF
           IF  (I-86)
               GO TO SLUTT-T
      *****************************************************************
      * HENTE DATA FRA STAT.TAB.FILE                                  *
      *****************************************************************
           END-IF
           IF  (I-L2)
               MOVE FIRMA                  TO F5 (1:3)
               MOVE '07'                   TO F5 (4:2)
               MOVE '   '                  TO F3
               MOVE ORDMOT                 TO F3 (1:2)
               MOVE F5                     TO TABKEY (1:5)
               MOVE F3                     TO TABKEY (6:3)
               MOVE TABKEY                 TO STATTAB-KEY1
               READ STATTAB RECORD KEY IS STATTAB-KEY1
               INVALID KEY
                   SET I-51                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-51            TO TRUE
                   PERFORM STATTAB-FLDSET
                   PERFORM STATTAB-IDSET
               END-READ
      *****************************************************************
      * NULLSTILLING PR. FIRMA. AKK. I ÅR OG DENNE MND.               *
      *****************************************************************
           END-IF
           IF  (I-L3)
               MOVE 0,00                   TO ORDSA2
               MOVE 0,00                   TO BFJSA2
               MOVE 0                      TO ANTOA2
               MOVE 0                      TO ANTKA2
      *****************************************************************
           END-IF
           IF  (I-L3)
               MOVE 0,00                   TO ORDSM2
               MOVE 0,00                   TO BFJSM2
               MOVE 0                      TO ANTOM2
               MOVE 0                      TO ANTKM2
      *****************************************************************
      * NULLSTILLING PR. ORDREMOTTAGER. AKK. I ÅR OG DENNE MND.       *
      *****************************************************************
           END-IF
           IF  (I-L2)
               MOVE 0,00                   TO ORDSA1
               MOVE 0,00                   TO BFJSA1
               MOVE 0                      TO ANTOA1
               MOVE 0                      TO ANTKA1
      *****************************************************************
           END-IF
           IF  (I-L2)
               MOVE 0,00                   TO ORDSM1
               MOVE 0,00                   TO BFJSM1
               MOVE 0                      TO ANTOM1
               MOVE 0                      TO ANTKM1
      *****************************************************************
           END-IF
           IF  (I-L1)
               MOVE 0,00                   TO ORDSMU
               MOVE 0,00                   TO BFJSMU
      *****************************************************************
      * SELEKSJONSRUTINE.                                             *
      *****************************************************************
           END-IF
           SET NOT-I-11                    TO TRUE
           IF  FAKAAR = PARAAR
               SET I-11                    TO TRUE
           END-IF
           IF  (NOT-I-11)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-12                    TO TRUE
           IF  FAKMND = PARMND
               SET I-12                    TO TRUE
           END-IF
      *****************************************************************
      * SUMMERINGSRUTINE.                                             *
      *    KREDITNOTA HAR NEGATIVT BELØP, MEN IKKE ANTALL.            *
      *****************************************************************
           SET NOT-I-21                    TO TRUE
           SET NOT-I-19                    TO TRUE
           IF  ORDNR > '899999'
               SET I-19                    TO TRUE
           END-IF
           SET NOT-I-20                    TO TRUE
           IF  KRTYPE = '2'
               SET I-20                    TO TRUE
           END-IF
           IF  (I-19 AND I-20)
               SET I-21                    TO TRUE
           END-IF
           SUBTRACT SVSSUM FROM NTOSUM GIVING BRFJ
           ADD NTOSUM                      TO ORDSA2
           SET NOT-I-72                    TO TRUE
           IF  ORDSA2 = 0
               SET I-72                    TO TRUE
           END-IF
           ADD NTOSUM                      TO ORDSA1
           SET NOT-I-71                    TO TRUE
           IF  ORDSA1 = 0
               SET I-71                    TO TRUE
           END-IF
           IF  (I-12)
               ADD NTOSUM                  TO ORDSM2
               ADD NTOSUM                  TO ORDSM1
               ADD NTOSUM                  TO ORDSMU
           END-IF
           ADD BRFJ                        TO BFJSA2
           ADD BRFJ                        TO BFJSA1
           IF  (I-12)
               ADD BRFJ                    TO BFJSM2
               ADD BRFJ                    TO BFJSM1
               ADD BRFJ                    TO BFJSMU
           END-IF
           IF  (I-L1 AND NOT-I-19)
               ADD 1                       TO ANTOA2
               ADD 1                       TO ANTOA1
           END-IF
           IF  (I-L1 AND NOT-I-19 AND I-12)
               ADD 1                       TO ANTOM2
               ADD 1                       TO ANTOM1
           END-IF
           IF  (I-L1 AND I-19)
               ADD 1                       TO ANTKA2
               ADD 1                       TO ANTKA1
           END-IF
           IF  (I-L1 AND I-19 AND I-12)
               ADD 1                       TO ANTKM2
               ADD 1                       TO ANTKM1
      *****************************************************************
      * SETT PÅ INDIKATORER.                                          *
           END-IF
           SET I-50                        TO TRUE
      *****************************************************************
           .
 
       SLUTT-T.
      *****************************************************************
      * BEREGNING AV BRTF.PROSENT PR. ORDRE. (KUN VED UPSI 1)         *
      *****************************************************************
           CONTINUE.
 
       MNDRUT-S SECTION.
       MNDRUT-S-P.
           MOVE '        '                 TO PARMNA (1:8)
           MOVE ' '                        TO PARMNA (9:1)
           SET NOT-I-24                    TO TRUE
           IF  PMND = '01'
               SET I-24                    TO TRUE
           END-IF
           IF  (I-24)
               MOVE 'JANUAR  '             TO PARMNA (1:8)
           END-IF
           SET NOT-I-24                    TO TRUE
           IF  PMND = '02'
               SET I-24                    TO TRUE
           END-IF
           IF  (I-24)
               MOVE 'FEBRUAR '             TO PARMNA (1:8)
           END-IF
           SET NOT-I-24                    TO TRUE
           IF  PMND = '03'
               SET I-24                    TO TRUE
           END-IF
           IF  (I-24)
               MOVE 'MARS    '             TO PARMNA (1:8)
           END-IF
           SET NOT-I-24                    TO TRUE
           IF  PMND = '04'
               SET I-24                    TO TRUE
           END-IF
           IF  (I-24)
               MOVE 'APRIL   '             TO PARMNA (1:8)
           END-IF
           SET NOT-I-24                    TO TRUE
           IF  PMND = '05'
               SET I-24                    TO TRUE
           END-IF
           IF  (I-24)
               MOVE 'MAI     '             TO PARMNA (1:8)
           END-IF
           SET NOT-I-24                    TO TRUE
           IF  PMND = '06'
               SET I-24                    TO TRUE
           END-IF
           IF  (I-24)
               MOVE 'JUNI    '             TO PARMNA (1:8)
           END-IF
           SET NOT-I-24                    TO TRUE
           IF  PMND = '07'
               SET I-24                    TO TRUE
           END-IF
           IF  (I-24)
               MOVE 'JULI    '             TO PARMNA (1:8)
           END-IF
           SET NOT-I-24                    TO TRUE
           IF  PMND = '08'
               SET I-24                    TO TRUE
           END-IF
           IF  (I-24)
               MOVE 'AUGUST  '             TO PARMNA (1:8)
           END-IF
           SET NOT-I-24                    TO TRUE
           IF  PMND = '09'
               SET I-24                    TO TRUE
           END-IF
           IF  (I-24)
               MOVE 'SEPTEMBE'             TO PARMNA (1:8)
               MOVE 'R'                    TO PARMNA (9:1)
           END-IF
           SET NOT-I-24                    TO TRUE
           IF  PMND = '10'
               SET I-24                    TO TRUE
           END-IF
           IF  (I-24)
               MOVE 'OKTOBER '             TO PARMNA (1:8)
           END-IF
           SET NOT-I-24                    TO TRUE
           IF  PMND = '11'
               SET I-24                    TO TRUE
           END-IF
           IF  (I-24)
               MOVE 'NOVEMBER'             TO PARMNA (1:8)
           END-IF
           SET NOT-I-24                    TO TRUE
           IF  PMND = '12'
               SET I-24                    TO TRUE
           END-IF
           IF  (I-24)
               MOVE 'DESEMBER'             TO PARMNA (1:8)
           END-IF.
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           IF  (NOT-I-89)
               MOVE ' '                    TO BBEST
           END-IF
           MOVE 'FAK80'                    TO LONR
           MOVE FIRMA                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'FAK800  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF.
      ******************************************************
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1 AND NOT-I-U1)
               GO TO ENDU1-T
           END-IF
           IF  (I-L1)
               SET NOT-I-60                TO TRUE
               IF  ORDSMU = 0,00
                   SET I-60                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-50)
               MULTIPLY 100 BY BFJSMU  GIVING F12
           END-IF
           IF  (I-L1 AND I-50 AND NOT-I-60)
               DIVIDE F12 BY ORDSMU    GIVING BFPSMU ROUNDED
           END-IF.
 
       ENDU1-T.
      *****************************************************************
      * BEREGNING AV BRTF.PROSENT PR. ORDREMOTAGER.                   *
      *****************************************************************
           IF  (I-L2)
               SET NOT-I-61                TO TRUE
               IF  ORDSM1 = 0,00
                   SET I-61                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-50)
               MULTIPLY 100 BY BFJSM1  GIVING F12
           END-IF
           IF  (I-L2 AND I-50 AND NOT-I-61)
               DIVIDE F12 BY ORDSM1    GIVING BFPSM1 ROUNDED
           END-IF
           IF  (I-L2 AND I-50)
               MULTIPLY 100 BY BFJSA1  GIVING F12
           END-IF
           IF  (I-L2 AND I-50 AND NOT-I-71)
               DIVIDE F12 BY ORDSA1    GIVING BFPSA1 ROUNDED
           END-IF
           IF  (I-L2 AND I-50)
               SUBTRACT ANTKM1 FROM ANTOM1 GIVING L2NETM
               SUBTRACT ANTKA1 FROM ANTOA1 GIVING L2NETA
           END-IF
           IF  (I-L2)
               SET NOT-I-63                TO TRUE
               IF  L2NETM = 0
                   SET I-63                TO TRUE
               END-IF
               SET NOT-I-64                TO TRUE
               IF  L2NETA = 0
                   SET I-64                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-50 AND NOT-I-63)
               DIVIDE ORDSM1 BY L2NETM GIVING L2GJTM
           END-IF
           IF  (I-L2 AND I-50 AND NOT-I-64)
               DIVIDE ORDSA1 BY L2NETA GIVING L2GJTA
           END-IF
           IF  (I-L2 AND I-50 AND I-63)
               ADD ORDSM1 TO ZERO      GIVING L2GJTM
           END-IF
           IF  (I-L2 AND I-50 AND I-64)
               ADD ORDSA1 TO ZERO      GIVING L2GJTA
      *****************************************************************
      * BEREGNING AV BRTF.PROSENT PR. FIRMA.                          *
      *****************************************************************
           END-IF
           IF  (I-L3)
               SET NOT-I-62                TO TRUE
               IF  ORDSM2 = 0,00
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND I-50)
               MULTIPLY 100 BY BFJSM2  GIVING F12
           END-IF
           IF  (I-L3 AND I-50 AND NOT-I-62)
               DIVIDE F12 BY ORDSM2    GIVING BFPSM2 ROUNDED
           END-IF
           IF  (I-L3 AND I-50)
               MULTIPLY 100 BY BFJSA2  GIVING F12
           END-IF
           IF  (I-L3 AND I-50 AND NOT-I-72)
               DIVIDE F12 BY ORDSA2    GIVING BFPSA2 ROUNDED
           END-IF
           IF  (I-L3 AND I-50)
               SUBTRACT ANTKM2 FROM ANTOM2 GIVING L3NETM
               SUBTRACT ANTKA2 FROM ANTOA2 GIVING L3NETA
           END-IF
           IF  (I-L2)
               SET NOT-I-65                TO TRUE
               IF  L3NETM = 0
                   SET I-65                TO TRUE
               END-IF
               SET NOT-I-66                TO TRUE
               IF  L3NETA = 0
                   SET I-66                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND I-50 AND NOT-I-65)
               DIVIDE ORDSM2 BY L3NETM GIVING L3GJTM
           END-IF
           IF  (I-L3 AND I-50 AND NOT-I-66)
               DIVIDE ORDSA2 BY L3NETA GIVING L3GJTA
           END-IF
           IF  (I-L2 AND I-50 AND I-65)
               ADD ORDSM2 TO ZERO      GIVING L3GJTM
           END-IF
           IF  (I-L2 AND I-50 AND I-66)
               ADD ORDSA2 TO ZERO      GIVING L3GJTA
      *****************************************************************
      * SUBRUTINE FOR Å LAGE AVIKENDE MÅNEDSNAVN.                     *
      *****************************************************************
           END-IF
           .
 
       PARAM-GET SECTION.
       PARAM-GET-P.
           IF  PARAM-EOF-OFF
               READ PARAM
               AT END
                   SET PARAM-EOF           TO TRUE
               END-READ
           END-IF.
 
       PARAM-FLDSET SECTION.
       PARAM-FLDSET-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '1' )
               MOVE PARAM-IO-AREA (8:8)    TO PJOBN (1:8)
               MOVE PARAM-IO-AREA (19:1)   TO PRKODE (1:1)
               MOVE PARAM-IO-AREA (32:30)  TO PPERS (1:30)
               MOVE PARAM-IO-AREA (69:3)   TO PANTX-IO
               INSPECT PANTX-IO REPLACING ALL ' ' BY '0'
           WHEN ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '2' )
               MOVE PARAM-IO-AREA (21:40)  TO PETTB (1:40)
           WHEN ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '3' )
               MOVE PARAM-IO-AREA (21:40)  TO PFORS (1:40)
           WHEN ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '4' )
               MOVE PARAM-IO-AREA (21:40)  TO PMEMO (1:40)
           WHEN ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '5' )
               MOVE PARAM-IO-AREA (21:2)   TO PMND (1:2)
               MOVE PARAM-IO-AREA (23:2)   TO PAAR (1:2)
           END-EVALUATE.
 
       PARAM-IDCHK SECTION.
       PARAM-IDCHK-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '1' )
             OR ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '2' )
             OR ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '3' )
             OR ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '4' )
             OR ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '5' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       PARAM-IDSET SECTION.
       PARAM-IDSET-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '1' )
               SET I-81                    TO TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '2' )
               SET I-82                    TO TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '3' )
               SET I-83                    TO TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '4' )
               SET I-84                    TO TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '5' )
               SET I-85                    TO TRUE
           END-EVALUATE.
 
       FAKPAR-FLDSET SECTION.
       FAKPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ( FAKPAR-IO-AREA (1:1) = '9' )
               MOVE FAKPAR-IO-AREA (95:4)  TO PARAAR (1:4)
               MOVE FAKPAR-IO-AREA (20:2)  TO PARMND (1:2)
               MOVE FAKPAR-IO-AREA (95:6)  TO PARMAR (1:6)
               MOVE FAKPAR-IO-AREA (74:9)  TO PARMNA (1:9)
           END-EVALUATE.
 
       FAKPAR-IDCHK SECTION.
       FAKPAR-IDCHK-P.
           EVALUATE TRUE
           WHEN ( FAKPAR-IO-AREA (1:1) = '9' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       FAKPAR-IDSET SECTION.
       FAKPAR-IDSET-P.
           EVALUATE TRUE
           WHEN ( FAKPAR-IO-AREA (1:1) = '9' )
               SET I-02                    TO TRUE
           END-EVALUATE.
 
       FAKSALG-GET SECTION.
       FAKSALG-GET-P.
           IF  FAKSALG-EOF-OFF
               READ FAKSALG
               AT END
                   SET FAKSALG-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FAKSALG-FLDSET SECTION.
       FAKSALG-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKSALG-IO-AREA (4:4)  TO FAKAAR (1:4)
               MOVE FAKSALG-IO-AREA (8:2)  TO FAKMND (1:2)
               MOVE FAKSALG-IO-AREA (10:6) TO KUNDNR (1:6)
               MOVE FAKSALG-IO-AREA (16:5) TO SEQNR (1:5)
               MOVE FAKSALG-IO-AREA (21:8) TO ORDDTO (1:8)
               MOVE FAKSALG-IO-AREA (29:4) TO ORDTID (1:4)
               MOVE FAKSALG-IO-AREA (33:6) TO ORDNR (1:6)
               MOVE FAKSALG-IO-AREA (39:3) TO FIRMA (1:3)
               MOVE FAKSALG-IO-AREA (42:2) TO BM (1:2)
               MOVE FAKSALG-IO-AREA (44:2) TO LK (1:2)
               MOVE FAKSALG-IO-AREA (46:1) TO AVD (1:1)
               MOVE FAKSALG-IO-AREA (47:1) TO FK (1:1)
               MOVE FAKSALG-IO-AREA (48:1) TO KRTYPE (1:1)
               MOVE FAKSALG-IO-AREA (49:1) TO KOSTN (1:1)
               MOVE FAKSALG-IO-AREA (50:1) TO SERVO (1:1)
               MOVE FAKSALG-IO-AREA (51:5) TO VGR (1:5)
               MOVE FAKSALG-IO-AREA (56:3) TO HND (1:3)
               MOVE FAKSALG-IO-AREA (59:1) TO BK (1:1)
               MOVE FAKSALG-IO-AREA (60:7) TO EDBNR (1:7)
               MOVE FAKSALG-IO-AREA (67:3) TO ALFAK (1:3)
               MOVE FAKSALG-IO-AREA (70:20) TO ARTNR (1:20)
               MOVE FAKSALG-IO-AREA (70:8) TO ART8F (1:8)
               MOVE FAKSALG-IO-AREA (90:7) TO ANTB-IO
               INSPECT ANTB-IO REPLACING ALL ' ' BY '0'
               MOVE FAKSALG-IO-AREA (97:7) TO ANTL-IO
               INSPECT ANTL-IO REPLACING ALL ' ' BY '0'
               MOVE FAKSALG-IO-AREA (104:9) TO ORDPRI-IO
               INSPECT ORDPRI-IO REPLACING ALL ' ' BY '0'
               MOVE FAKSALG-IO-AREA (113:3) TO RAB1-IO
               INSPECT RAB1-IO REPLACING ALL ' ' BY '0'
               MOVE FAKSALG-IO-AREA (116:3) TO RAB2-IO
               INSPECT RAB2-IO REPLACING ALL ' ' BY '0'
               MOVE FAKSALG-IO-AREA (119:3) TO RAB3-IO
               INSPECT RAB3-IO REPLACING ALL ' ' BY '0'
               MOVE FAKSALG-IO-AREA (122:9) TO NTOSUM-IO
               INSPECT NTOSUM-IO REPLACING ALL ' ' BY '0'
               MOVE FAKSALG-IO-AREA (131:9) TO SVSSUM-IO
               INSPECT SVSSUM-IO REPLACING ALL ' ' BY '0'
               MOVE FAKSALG-IO-AREA (140:6) TO FAKTNR (1:6)
               MOVE FAKSALG-IO-AREA (146:1) TO ORDTYP (1:1)
               MOVE FAKSALG-IO-AREA (147:1) TO FAKOMG (1:1)
               MOVE FAKSALG-IO-AREA (148:2) TO ORDMOT (1:2)
               MOVE FAKSALG-IO-AREA (150:2) TO PLUKKA (1:2)
           END-EVALUATE.
 
       FAKSALG-IDSET SECTION.
       FAKSALG-IDSET-P.
           SET I-01                        TO TRUE.
 
       FAKSALG-CHK-LEVEL SECTION.
       FAKSALG-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO FAKSALG-LEVEL-01
               MOVE FAKSALG-IO-AREA (39:3) TO FAKSALG-01-L3-FIRMA
               MOVE FAKSALG-IO-AREA (148:2) TO FAKSALG-01-L2-ORDMOT
               MOVE FAKSALG-IO-AREA (33:6) TO FAKSALG-01-L1-ORDNR
               IF  FAKSALG-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  FAKSALG-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  FAKSALG-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  FAKSALG-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  FAKSALG-01-L3         TO THE-PRIOR-L3
               MOVE  FAKSALG-01-L2         TO THE-PRIOR-L2
               MOVE  FAKSALG-01-L1         TO THE-PRIOR-L1
               SET FAKSALG-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       STATTAB-FLDSET SECTION.
       STATTAB-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE STATTAB-IO-AREA (11:30) TO OMNAVN (1:30)
           END-EVALUATE.
 
       STATTAB-IDSET SECTION.
       STATTAB-IDSET-P.
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
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L3 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE LOPNVN                 TO LISTE-IO-AREA (32:35)
               MOVE PARMNA                 TO LISTE-IO-AREA (68:9)
               MOVE PARAAR                 TO LISTE-IO-AREA (78:4)
               MOVE 'FREMSTILLT'           TO LISTE-IO-AREA (84:10)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (95:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (106:4)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (110:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '--------'             TO LISTE-IO-AREA (121:8)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'OM'                   TO LISTE-IO-AREA (2:2)
               MOVE 'PERSON NAVN '         TO LISTE-IO-AREA (5:12)
               MOVE 'PERIODE'              TO LISTE-IO-AREA (36:7)
               MOVE 'SUM SALG'             TO LISTE-IO-AREA (56:8)
               MOVE 'SUM BRT.FJ.'          TO LISTE-IO-AREA (68:11)
               MOVE 'FJ.%'                 TO LISTE-IO-AREA (81:4)
               MOVE 'ANT.ORDRE'            TO LISTE-IO-AREA (87:9)
               MOVE 'ANT.KRNOTA'           TO LISTE-IO-AREA (97:10)
               MOVE ' NETTO'               TO LISTE-IO-AREA (109:6)
               MOVE 'GJSN TRANS'           TO LISTE-IO-AREA (119:10)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '--------'             TO LISTE-IO-AREA (121:8)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF AND NOT-I-L3)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE LOPNVN                 TO LISTE-IO-AREA (32:35)
               MOVE PARMNA                 TO LISTE-IO-AREA (68:9)
               MOVE PARAAR                 TO LISTE-IO-AREA (78:4)
               MOVE 'FREMSTILLT'           TO LISTE-IO-AREA (84:10)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (95:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (106:4)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (110:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '--------'             TO LISTE-IO-AREA (121:8)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'OM'                   TO LISTE-IO-AREA (2:2)
               MOVE 'PERSON NAVN '         TO LISTE-IO-AREA (5:12)
               MOVE 'PERIODE'              TO LISTE-IO-AREA (36:7)
               MOVE 'SUM SALG'             TO LISTE-IO-AREA (56:8)
               MOVE 'SUM BRT.FJ.'          TO LISTE-IO-AREA (68:11)
               MOVE 'FJ.%'                 TO LISTE-IO-AREA (81:4)
               MOVE 'ANT.ORDRE'            TO LISTE-IO-AREA (87:9)
               MOVE 'ANT.KRNOTA'           TO LISTE-IO-AREA (97:10)
               MOVE ' NETTO'               TO LISTE-IO-AREA (109:6)
               MOVE 'GJSN TRANS'           TO LISTE-IO-AREA (119:10)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '--------'             TO LISTE-IO-AREA (121:8)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND NOT-I-86 AND I-50)
           AND (I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ORDMOT                 TO LISTE-IO-AREA (2:2)
               MOVE 'ORDENR.'              TO LISTE-IO-AREA (5:7)
               MOVE ORDNR                  TO LISTE-IO-AREA (13:6)
               MOVE PARMNA                 TO LISTE-IO-AREA (39:9)
               MOVE ORDSMU                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (50:14)
               MOVE BFJSMU                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (65:14)
               MOVE BFPSMU                 TO XO-31YY9R
               MOVE XO-31YY9R              TO LISTE-IO-AREA (80:6)
               INITIALIZE BFPSMU
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L2 AND NOT-I-86 AND I-50)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ORDMOT                 TO LISTE-IO-AREA (2:2)
               IF  (NOT-I-51)
                   MOVE OMNAVN             TO LISTE-IO-AREA (5:30)
               END-IF
               IF  (I-51)
                   MOVE 'UKJENT SIGNATUR'  TO LISTE-IO-AREA (5:15)
               END-IF
               MOVE '* '                   TO LISTE-IO-AREA (36:2)
               MOVE PARMNA                 TO LISTE-IO-AREA (39:9)
               MOVE ORDSM1                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (50:14)
               MOVE BFJSM1                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (65:14)
               MOVE BFPSM1                 TO XO-31YY9R
               MOVE XO-31YY9R              TO LISTE-IO-AREA (80:6)
               INITIALIZE BFPSM1
               MOVE ANTOM1                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (90:6)
               MOVE ANTKM1                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (101:6)
               MOVE L2NETM                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (108:6)
               MOVE L2GJTM                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (115:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '** AKK. I ÅR.'        TO LISTE-IO-AREA (36:13)
               MOVE ORDSA1                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (50:14)
               MOVE BFJSA1                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (65:14)
               MOVE BFPSA1                 TO XO-31YY9R
               MOVE XO-31YY9R              TO LISTE-IO-AREA (80:6)
               INITIALIZE BFPSA1
               MOVE ANTOA1                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (90:6)
               MOVE ANTKA1                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (101:6)
               MOVE L2NETA                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (108:6)
               MOVE L2GJTA                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (115:14)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L3 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*** FIRMATOTALER ***' TO LISTE-IO-AREA (5:20)
               MOVE '* '                   TO LISTE-IO-AREA (36:2)
               MOVE PARMNA                 TO LISTE-IO-AREA (39:9)
               MOVE ORDSM2                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (50:14)
               MOVE BFJSM2                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (65:14)
               MOVE BFPSM2                 TO XO-31YY9R
               MOVE XO-31YY9R              TO LISTE-IO-AREA (80:6)
               INITIALIZE BFPSM2
               MOVE ANTOM2                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (90:6)
               MOVE ANTKM2                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (101:6)
               MOVE L3NETM                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (108:6)
               MOVE L3GJTM                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (115:14)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '** AKK. I ÅR.'        TO LISTE-IO-AREA (36:13)
               MOVE ORDSA2                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (50:14)
               MOVE BFJSA2                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (65:14)
               MOVE BFPSA2                 TO XO-31YY9R
               MOVE XO-31YY9R              TO LISTE-IO-AREA (80:6)
               INITIALIZE BFPSA2
               MOVE ANTOA2                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (90:6)
               MOVE ANTKA2                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (101:6)
               MOVE L3NETA                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (108:6)
               MOVE L3GJTA                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (115:14)
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
           MOVE 1                          TO LR-CHECK
           INITIALIZE PARAM-DATA-FIELDS
           SET PARAM-EOF-OFF               TO TRUE
           SET PARAM-PROCESS               TO TRUE
           OPEN INPUT PARAM
           INITIALIZE FAKPAR-DATA-FIELDS
           OPEN INPUT FAKPAR
           SET FAKSALG-LEVEL-INIT          TO TRUE
           INITIALIZE FAKSALG-DATA-FIELDS
           SET FAKSALG-EOF-OFF             TO TRUE
           SET FAKSALG-PROCESS             TO TRUE
           OPEN INPUT FAKSALG
           INITIALIZE STATTAB-DATA-FIELDS
           OPEN INPUT STATTAB
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PARAM
           CLOSE FAKPAR
           CLOSE FAKSALG
           CLOSE STATTAB
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
