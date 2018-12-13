       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK205R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAM FAK205
      *        PROGRAMERT AV: ESPEN LARSEN. 16/12-2003                  *
      * UTREGNING AV BONUSBERETTIGET KJØP OG BONUS PR. KUNDE.           *
      * KUNDER UTEN KJØP SKAL OGSÅ FREMKOMME.                           *
      * SELEKSJON AV DATA ER FORETATT I SORTERINGSPROGRAM.              *
      * INPUT ER OPPSAMLEDE FAKTURA BONUS-DATA OG BONUSTABELL.          *
      * OUTPUT ER BONUSOPPGAVE TIL B.O.STEEN
      *******************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK205.rpg
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
           SELECT BBONTAB
               ASSIGN TO UT-S-BBONTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS BBONTAB-STATUS.
           SELECT FAKPAR
               ASSIGN TO UT-S-FAKPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKPAR-STATUS.
           SELECT FAKBON
               ASSIGN TO UT-S-FAKBON
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKBON-STATUS.
           SELECT KUNDEMA
               ASSIGN TO KUNDEMA
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMA-STATUS
               RECORD KEY IS KUNDEMA-KEY1.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD BBONTAB
               BLOCK CONTAINS 160
               RECORD CONTAINS 40.
       01  BBONTAB-IO-AREA.
           05  BBONTAB-IO-AREA-X           PICTURE X(40).
       FD FAKPAR
               BLOCK CONTAINS 200
               RECORD CONTAINS 200.
       01  FAKPAR-IO-AREA.
           05  FAKPAR-IO-AREA-X            PICTURE X(200).
       FD FAKBON
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  FAKBON-IO-AREA.
           05  FAKBON-IO-AREA-X            PICTURE X(200).
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1            PICTURE X(9).
               10  FILLER                  PICTURE X(190).
       FD LISTE
               BLOCK CONTAINS 121
               RECORD CONTAINS 121.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(120).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  BBONTAB-STATUS              PICTURE 99 VALUE 0.
           10  FAKPAR-STATUS               PICTURE 99 VALUE 0.
           10  FAKBON-STATUS               PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  VLFELT-XX-STATUS            PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  BBONTAB-EOF-OFF         VALUE '0'.
               88  BBONTAB-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BBONTAB-READ-OFF        VALUE '0'.
               88  BBONTAB-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BBONTAB-PROCESS-OFF     VALUE '0'.
               88  BBONTAB-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  BBONTAB-LEVEL-INIT-OFF  VALUE '0'.
               88  BBONTAB-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKBON-EOF-OFF          VALUE '0'.
               88  FAKBON-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKBON-READ-OFF         VALUE '0'.
               88  FAKBON-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKBON-PROCESS-OFF      VALUE '0'.
               88  FAKBON-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  FAKBON-LEVEL-INIT-OFF   VALUE '0'.
               88  FAKBON-LEVEL-INIT       VALUE '1'.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
               10  FILLER                  PICTURE X(248).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(9).
               10  VLBEL-IO.
                   15  VLBEL               PICTURE S9(9)V9(2).
               10  FILLER                  PICTURE X(237).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(20).
               10  VLPTIL-IO.
                   15  VLPTIL              PICTURE S9(9)V9(2).
               10  FILLER                  PICTURE X(226).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(31).
               10  VLRAB1-IO.
                   15  VLRAB1              PICTURE S9(2)V9(1).
               10  FILLER                  PICTURE X(223).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(34).
               10  VLRAB2-IO.
                   15  VLRAB2              PICTURE S9(2)V9(1).
               10  FILLER                  PICTURE X(220).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(37).
               10  VLRAB3-IO.
                   15  VLRAB3              PICTURE S9(2)V9(1).
               10  FILLER                  PICTURE X(217).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(40).
               10  VLEDBN-IO.
                   15  VLEDBN              PICTURE S9(7).
               10  FILLER                  PICTURE X(210).
           05  LDATA-XX REDEFINES VLFELT-XX-DATA-FIELDS.
               10  LONR                    PICTURE X(5).
               10  FILLER                  PICTURE X(252).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(5).
               10  LFIRMA                  PICTURE X(3).
               10  FILLER                  PICTURE X(249).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(8).
               10  LUNDGR                  PICTURE X(3).
               10  FILLER                  PICTURE X(246).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(11).
               10  LPROG                   PICTURE X(8).
               10  FILLER                  PICTURE X(238).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(19).
               10  LANTX-IO.
                   15  LANTX               PICTURE S9(3).
               10  FILLER                  PICTURE X(235).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(22).
               10  FINAVN                  PICTURE X(30).
               10  FILLER                  PICTURE X(205).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(52).
               10  LOPNVN                  PICTURE X(35).
               10  FILLER                  PICTURE X(170).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(87).
               10  LPRIID                  PICTURE X(4).
               10  FILLER                  PICTURE X(166).
      *     *  BESTILLINGSOPPGAVER (OVERSTYRING AV RBS-FILE) *    *
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(91).
               10  BJOBN                   PICTURE X(8).
               10  FILLER                  PICTURE X(158).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(99).
               10  BBEST                   PICTURE X(1).
               10  FILLER                  PICTURE X(157).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(100).
               10  BPERS                   PICTURE X(30).
               10  FILLER                  PICTURE X(127).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(130).
               10  BETTB                   PICTURE X(40).
               10  FILLER                  PICTURE X(87).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(170).
               10  BFORS                   PICTURE X(40).
               10  FILLER                  PICTURE X(47).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(210).
               10  BMEMO                   PICTURE X(40).
               10  FILLER                  PICTURE X(7).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(250).
               10  BANTX-IO.
                   15  BANTX               PICTURE S9(3).
               10  FILLER                  PICTURE X(4).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(253).
               10  BPCLAS                  PICTURE X(1).
               10  FILLER                  PICTURE X(3).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(254).
               10  BPRJE                   PICTURE X(3).
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
           05  BBONTAB-LEVEL-03.
               10  BBONTAB-03-L2.
                   15  BBONTAB-03-L2-FIRMNR PICTURE X(3).
               10  BBONTAB-03-L1.
                   15  BBONTAB-03-L1-RESKNR PICTURE X(6).
           05  BBONTAB-DATA-FIELDS.
               10  FIRMNR                  PICTURE X(3).
               10  RESKNR                  PICTURE X(6).
               10  FNRKNR                  PICTURE X(9).
           05  BBONTAB-MP                  PICTURE X(9).
           05  BBONTAB-MC                  PICTURE X(9).
           05  BBONTAB-M-03            REDEFINES BBONTAB-MC.
               10  BBONTAB-M-03-M2.
                   15  BBONTAB-M-03-M2-FIRMNR-G.
                       20  BBONTAB-M-03-M2-FIRMNR PICTURE X(3).
               10  BBONTAB-M-03-M1.
                   15  BBONTAB-M-03-M1-RESKNR-G.
                       20  BBONTAB-M-03-M1-RESKNR PICTURE X(6).
           05  FAKPAR-DATA-FIELDS.
               10  FAKDM                   PICTURE X(4).
               10  FAKDMA                  PICTURE X(6).
               10  FAKM                    PICTURE X(2).
               10  FAKA                    PICTURE X(2).
               10  FAKPER-IO.
                   15  FAKPER              PICTURE S9(6).
           05  FAKBON-LEVEL-02.
               10  FAKBON-02-L2.
                   15  FAKBON-02-L2-FIRMNR PICTURE X(3).
               10  FAKBON-02-L1.
                   15  FAKBON-02-L1-RESKNR PICTURE X(6).
           05  FAKBON-DATA-FIELDS.
               10  FAKTYP                  PICTURE X(1).
               10  ORDRDD                  PICTURE X(2).
               10  ORDRMM                  PICTURE X(2).
               10  ORDRAA                  PICTURE X(2).
               10  VGR                     PICTURE X(5).
               10  VGR1                    PICTURE X(1).
               10  ANT-IO.
                   15  ANT                 PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  EDBNR-IO.
                   15  EDBNR               PICTURE S9(7).
               10  RAB1-IO.
                   15  RAB1                PICTURE S9(2)V9(1).
               10  RAB2-IO.
                   15  RAB2                PICTURE S9(2)V9(1).
               10  RAB3-IO.
                   15  RAB3                PICTURE S9(2)V9(1).
               10  BEL-IO.
                   15  BEL                 PICTURE S9(7)V9(2).
               10  ALFAK                   PICTURE X(3).
               10  BONGR                   PICTURE X(1).
               10  BONPRO-IO.
                   15  BONPRO              PICTURE S9(2)V9(2).
           05  FAKBON-MP                   PICTURE X(9).
           05  FAKBON-MC                   PICTURE X(9).
           05  FAKBON-M-02             REDEFINES FAKBON-MC.
               10  FAKBON-M-02-M2.
                   15  FAKBON-M-02-M2-FIRMNR-G.
                       20  FAKBON-M-02-M2-FIRMNR PICTURE X(3).
               10  FAKBON-M-02-M1.
                   15  FAKBON-M-02-M1-RESKNR-G.
                       20  FAKBON-M-02-M1-RESKNR PICTURE X(6).
           05  KUNDEMA-DATA-FIELDS.
               10  KNAVN1                  PICTURE X(30).
               10  KNAVN2                  PICTURE X(30).
               10  KADDR                   PICTURE X(30).
               10  KPSTED                  PICTURE X(15).
               10  KPNR                    PICTURE X(4).
      *****************************************************************
      * NULLSTILLING OG SETOF AV INDIKATORER.                         *
      *****************************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  SUML2-IO.
                   15  SUML2               PICTURE S9(7)V9(2).
               10  BONBL2-IO.
                   15  BONBL2              PICTURE S9(7)V9(2).
               10  SUML1-IO.
                   15  SUML1               PICTURE S9(7)V9(2).
               10  BONBL1-IO.
                   15  BONBL1              PICTURE S9(7)V9(2).
               10  EDBNRB                  PICTURE X(7).
               10  ANTKUN-IO.
                   15  ANTKUN              PICTURE S9(5).
               10  ANTUB-IO.
                   15  ANTUB               PICTURE S9(5).
               10  F11-IO.
                   15  F11                 PICTURE S9(9)V9(2).
               10  BONBEL-IO.
                   15  BONBEL              PICTURE S9(7)V9(2).
           05  EDITTING-FIELDS.
               10  XO-72YY9R               PICTURE Z.ZZZ.ZZZ,99-.
               10  XO-22YY9                PICTURE ZZ,99.
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
               88  NOT-SET-I-OF            VALUE '0'.
               88  SET-I-OF                VALUE '1'.
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
 
           PERFORM HEADING-OUTPUT
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
           PERFORM HEADING-OUTPUT
           IF  NOT-SET-I-OF
               SET NOT-I-OF                TO TRUE
           END-IF
           SET NOT-SET-I-OF                TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-03                    TO TRUE
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-04                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  BBONTAB-PROCESS
               SET BBONTAB-PROCESS-OFF     TO TRUE
               SET BBONTAB-READ            TO TRUE
           END-IF
 
           IF  BBONTAB-READ
               PERFORM BBONTAB-GET
               SET BBONTAB-READ-OFF        TO TRUE
               IF  NOT BBONTAB-EOF
                   PERFORM BBONTAB-MATCH-SET
               END-IF
           END-IF
 
           IF  FAKBON-PROCESS
               SET FAKBON-PROCESS-OFF      TO TRUE
               SET FAKBON-READ             TO TRUE
           END-IF
 
           IF  FAKBON-READ
               PERFORM FAKBON-GET
               SET FAKBON-READ-OFF         TO TRUE
               IF  NOT FAKBON-EOF
                   PERFORM FAKBON-MATCH-SET
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
 
           IF  BBONTAB-PROCESS
               PERFORM BBONTAB-IDSET
           END-IF
 
           IF  FAKBON-PROCESS
               PERFORM FAKBON-IDSET
           END-IF
 
           IF  BBONTAB-PROCESS
               PERFORM BBONTAB-CHK-LEVEL
           END-IF
 
           IF  FAKBON-PROCESS
               PERFORM FAKBON-CHK-LEVEL
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
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  BBONTAB-PROCESS
               PERFORM BBONTAB-FLDSET
           END-IF
 
           IF  FAKBON-PROCESS
               PERFORM FAKBON-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  BBONTAB-PROCESS
           OR  FAKBON-PROCESS
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
               SUBTRACT SUML2              FROM SUML2
               SUBTRACT BONBL2             FROM BONBL2
           END-IF
           IF  (I-L1)
               SET NOT-I-60                TO TRUE
               SUBTRACT SUML1              FROM SUML1
               SUBTRACT BONBL1             FROM BONBL1
      *****************************************************************
      * FAKTURAPARAMETER RUTINE.                                      *
      *****************************************************************
           END-IF
           IF  (NOT-I-16)
               READ FAKPAR
               AT END
                   SET I-17                TO TRUE
               NOT AT END
                   SET NOT-I-17            TO TRUE
                   PERFORM FAKPAR-FLDSET
                   PERFORM FAKPAR-IDSET
               END-READ
           END-IF
           SET I-16                        TO TRUE
      *****************************************************************
      * HOVEDRUTINE PR. FIRMA.                                        *
      *****************************************************************
           IF  (I-L2)
               PERFORM RBSRUT-S
           END-IF
           IF  (I-L2)
               SET NOT-I-18                TO TRUE
               IF  FIRMNR = '918'
                   SET I-18                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-18)
               MOVE '9002553'              TO EDBNRB
      *****************************************************************
      *  RUTINE FOR Å HENTE KUNDEOPPLYSNINGER.                        *
      *****************************************************************
           END-IF
           IF  (I-03)
               ADD 1                       TO ANTKUN
               MOVE FNRKNR                 TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-90                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-90            TO TRUE
                   PERFORM KUNDEMA-FLDSET
                   PERFORM KUNDEMA-IDSET
               END-READ
               GO TO SLUTT-T
      *****************************************************************
      *****************************************************************
      *    RUTINE FOR BEREGNING AV NETTO VARELINJE BELØP              *
      *    OG NETTOSUM PR.VGR(SUML1) PR.KUNDE(SUML2) PR.FIRMA(SUML3)  *
      *****************************************************************
           END-IF
           SET NOT-I-89                    TO TRUE
           IF  FAKTYP = 'K'
               SET I-89                    TO TRUE
           END-IF
           ADD ANT TO ZERO             GIVING VLANT
           ADD BEL TO ZERO             GIVING VLBEL
           MOVE 0                          TO VLPTIL
           ADD RAB1 TO ZERO            GIVING VLRAB1
           ADD RAB2 TO ZERO            GIVING VLRAB2
           ADD RAB3 TO ZERO            GIVING VLRAB3
           ADD EDBNR TO ZERO           GIVING VLEDBN
           CALL 'NETTOSUM' USING VLFELT-XX-DATA-FIELDS
           ADD VLBEL                       TO SUML1
           IF  (I-89)
               SUBTRACT VLBEL              FROM SUML1
           END-IF
           IF  (NOT-I-89)
               ADD VLBEL                   TO SUML2
           END-IF
           IF  (I-89)
               SUBTRACT VLBEL              FROM SUML2
      *****************************************************************
           END-IF
           .
 
       SLUTT-T.
      *****************************************************************
      * SKAL BONUSRECORD LEGGES UT TIL FAKTURERING.                   *
      *****************************************************************
           CONTINUE.
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE ' '                        TO BBEST
           MOVE 'FAK26'                    TO LONR
           MOVE FIRMNR                     TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'FAK205  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF.
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1)
               SET NOT-I-60                TO TRUE
               IF  SUML1 NOT = 0
                   SET I-60                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-60)
               ADD 1                       TO ANTUB
           END-IF
           IF  (I-L1 AND I-60)
               MULTIPLY BONPRO BY SUML1 GIVING F11 ROUNDED
               DIVIDE F11 BY 100       GIVING BONBEL ROUNDED
               ADD BONBEL                  TO BONBL1
               ADD BONBEL                  TO BONBL2
      *****************************************************************
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           END-IF
           .
 
       BBONTAB-GET SECTION.
       BBONTAB-GET-P.
           IF  BBONTAB-EOF-OFF
               READ BBONTAB
               AT END
                   SET BBONTAB-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       BBONTAB-FLDSET SECTION.
       BBONTAB-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE BBONTAB-IO-AREA (1:3)  TO FIRMNR (1:3)
               MOVE BBONTAB-IO-AREA (4:6)  TO RESKNR (1:6)
               MOVE BBONTAB-IO-AREA (1:9)  TO FNRKNR (1:9)
           END-EVALUATE.
 
       BBONTAB-IDSET SECTION.
       BBONTAB-IDSET-P.
           SET I-03                        TO TRUE.
 
       BBONTAB-CHK-LEVEL SECTION.
       BBONTAB-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO BBONTAB-LEVEL-03
               MOVE BBONTAB-IO-AREA (1:3)  TO BBONTAB-03-L2-FIRMNR
               MOVE BBONTAB-IO-AREA (4:6)  TO BBONTAB-03-L1-RESKNR
               IF  BBONTAB-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  BBONTAB-03-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  BBONTAB-03-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  BBONTAB-03-L2         TO THE-PRIOR-L2
               MOVE  BBONTAB-03-L1         TO THE-PRIOR-L1
               SET BBONTAB-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       BBONTAB-MATCH-SET SECTION.
       BBONTAB-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE BBONTAB-IO-AREA (1:3)  TO BBONTAB-M-03-M2-FIRMNR
               MOVE BBONTAB-IO-AREA (4:6)  TO BBONTAB-M-03-M1-RESKNR
           END-EVALUATE.
 
       FAKPAR-FLDSET SECTION.
       FAKPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKPAR-IO-AREA (12:4)  TO FAKDM (1:4)
               MOVE FAKPAR-IO-AREA (12:6)  TO FAKDMA (1:6)
               MOVE FAKPAR-IO-AREA (14:2)  TO FAKM (1:2)
               MOVE FAKPAR-IO-AREA (16:2)  TO FAKA (1:2)
               MOVE FAKPAR-IO-AREA (12:6)  TO FAKPER-IO
               INSPECT FAKPER-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       FAKPAR-IDSET SECTION.
       FAKPAR-IDSET-P.
           SET I-01                        TO TRUE.
 
       FAKBON-GET SECTION.
       FAKBON-GET-P.
           IF  FAKBON-EOF-OFF
               READ FAKBON
               AT END
                   SET FAKBON-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FAKBON-FLDSET SECTION.
       FAKBON-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKBON-IO-AREA (1:3)   TO FIRMNR (1:3)
               MOVE FAKBON-IO-AREA (184:6) TO RESKNR (1:6)
               MOVE FAKBON-IO-AREA (10:1)  TO FAKTYP (1:1)
               MOVE FAKBON-IO-AREA (71:2)  TO ORDRDD (1:2)
               MOVE FAKBON-IO-AREA (73:2)  TO ORDRMM (1:2)
               MOVE FAKBON-IO-AREA (75:2)  TO ORDRAA (1:2)
               MOVE FAKBON-IO-AREA (77:5)  TO VGR (1:5)
               MOVE FAKBON-IO-AREA (77:1)  TO VGR1 (1:1)
               MOVE FAKBON-IO-AREA (137:4) TO ANT-IO
               MOVE FAKBON-IO-AREA (141:7) TO EDBNR-IO
               INSPECT EDBNR-IO REPLACING ALL ' ' BY '0'
               MOVE FAKBON-IO-AREA (148:3) TO RAB1-IO
               INSPECT RAB1-IO REPLACING ALL ' ' BY '0'
               MOVE FAKBON-IO-AREA (151:3) TO RAB2-IO
               INSPECT RAB2-IO REPLACING ALL ' ' BY '0'
               MOVE FAKBON-IO-AREA (154:3) TO RAB3-IO
               INSPECT RAB3-IO REPLACING ALL ' ' BY '0'
               MOVE FAKBON-IO-AREA (157:9) TO BEL-IO
               INSPECT BEL-IO REPLACING ALL ' ' BY '0'
               MOVE FAKBON-IO-AREA (190:3) TO ALFAK (1:3)
               MOVE FAKBON-IO-AREA (193:1) TO BONGR (1:1)
               MOVE FAKBON-IO-AREA (194:4) TO BONPRO-IO
               INSPECT BONPRO-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       FAKBON-IDSET SECTION.
       FAKBON-IDSET-P.
           SET I-02                        TO TRUE.
 
       FAKBON-CHK-LEVEL SECTION.
       FAKBON-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO FAKBON-LEVEL-02
               MOVE FAKBON-IO-AREA (1:3)   TO FAKBON-02-L2-FIRMNR
               MOVE FAKBON-IO-AREA (184:6) TO FAKBON-02-L1-RESKNR
               IF  FAKBON-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  FAKBON-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  FAKBON-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  FAKBON-02-L2          TO THE-PRIOR-L2
               MOVE  FAKBON-02-L1          TO THE-PRIOR-L1
               SET FAKBON-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       FAKBON-MATCH-SET SECTION.
       FAKBON-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKBON-IO-AREA (1:3)   TO FAKBON-M-02-M2-FIRMNR
               MOVE FAKBON-IO-AREA (184:6) TO FAKBON-M-02-M1-RESKNR
           END-EVALUATE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (16:30) TO KNAVN1 (1:30)
               MOVE KUNDEMA-IO-AREA (46:30) TO KNAVN2 (1:30)
               MOVE KUNDEMA-IO-AREA (76:30) TO KADDR (1:30)
               MOVE KUNDEMA-IO-AREA (106:15) TO KPSTED (1:15)
               MOVE KUNDEMA-IO-AREA (121:4) TO KPNR (1:4)
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
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
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  BBONTAB-EOF
               MOVE HIGH-VALUES            TO BBONTAB-MC
                                              BBONTAB-MP
           END-IF
           IF  FAKBON-EOF
               MOVE HIGH-VALUES            TO FAKBON-MC
                                              FAKBON-MP
           END-IF
           IF  BBONTAB-MC < BBONTAB-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  FAKBON-MC < FAKBON-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  BBONTAB-MC < FAKBON-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET BBONTAB-PROCESS     TO TRUE
                   MOVE BBONTAB-MC         TO BBONTAB-MP
                   IF  BBONTAB-MC = FAKBON-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  FAKBON-MC < BBONTAB-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FAKBON-PROCESS      TO TRUE
                   MOVE FAKBON-MC          TO FAKBON-MP
                   IF  FAKBON-MC = BBONTAB-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  BBONTAB-MC = FAKBON-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET BBONTAB-PROCESS     TO TRUE
                   MOVE BBONTAB-MC         TO BBONTAB-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L2)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE LOPNVN                 TO LISTE-IO-AREA (32:35)
               MOVE 'PR'                   TO LISTE-IO-AREA (75:2)
               MOVE FAKPER                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (78:8)
               MOVE 'FREMSTILT'            TO LISTE-IO-AREA (91:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (101:8)
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
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KUNDE'                TO LISTE-IO-AREA (2:5)
               MOVE 'KUNDENAVN OG ADRESSE' TO LISTE-IO-AREA (8:20)
               MOVE 'BONUS GRUNNLAG'       TO LISTE-IO-AREA (70:14)
               MOVE 'BONUS UTFAKT.'        TO LISTE-IO-AREA (86:13)
               MOVE 'PROS.'                TO LISTE-IO-AREA (106:5)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF AND NOT-I-L2)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE LOPNVN                 TO LISTE-IO-AREA (32:35)
               MOVE 'PR'                   TO LISTE-IO-AREA (75:2)
               MOVE FAKPER                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (78:8)
               MOVE 'FREMSTILT'            TO LISTE-IO-AREA (91:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (101:8)
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
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KUNDE'                TO LISTE-IO-AREA (2:5)
               MOVE 'KUNDENAVN OG ADRESSE' TO LISTE-IO-AREA (8:20)
               MOVE 'BONUS GRUNNLAG'       TO LISTE-IO-AREA (70:14)
               MOVE 'BONUS UTFAKT.'        TO LISTE-IO-AREA (86:13)
               MOVE 'PROS.'                TO LISTE-IO-AREA (106:5)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE RESKNR                 TO LISTE-IO-AREA (1:6)
               IF  (NOT-I-90)
                   MOVE KNAVN1             TO LISTE-IO-AREA (8:30)
               END-IF
               IF  (I-90)
                   MOVE 'UKJENT KUNDE *'   TO LISTE-IO-AREA (8:14)
               END-IF
               IF  (NOT-I-90)
                   MOVE KNAVN2             TO LISTE-IO-AREA (39:30)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               IF  (NOT-I-90)
                   MOVE KADDR              TO LISTE-IO-AREA (8:30)
               END-IF
               IF  (I-90)
                   MOVE 'UKJENT KUNDE *'   TO LISTE-IO-AREA (8:14)
               END-IF
               IF  (NOT-I-90)
                   MOVE KPNR               TO LISTE-IO-AREA (39:4)
               END-IF
               IF  (NOT-I-90)
                   MOVE KPSTED             TO LISTE-IO-AREA (44:15)
               END-IF
               MOVE SUML1                  TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (71:13)
               MOVE BONBL1                 TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (86:13)
               MOVE BONPRO                 TO XO-22YY9
               MOVE XO-22YY9               TO LISTE-IO-AREA (106:5)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L2)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '***'                  TO LISTE-IO-AREA (4:3)
               MOVE 'TOTALT'               TO LISTE-IO-AREA (8:6)
               MOVE SUML2                  TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (71:13)
               MOVE BONBL2                 TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (86:13)
               INITIALIZE BONBL2
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '***'                  TO LISTE-IO-AREA (4:3)
               MOVE 'ANTALL KUNDER TOTAL'  TO LISTE-IO-AREA (8:19)
               MOVE ANTKUN                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (28:6)
               INITIALIZE ANTKUN
               MOVE 'HERAV UTEN KJØP'      TO LISTE-IO-AREA (35:15)
               MOVE ANTUB                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (51:6)
               INITIALIZE ANTUB
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
           SET BBONTAB-LEVEL-INIT          TO TRUE
           INITIALIZE BBONTAB-DATA-FIELDS
           SET BBONTAB-EOF-OFF             TO TRUE
           SET BBONTAB-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO BBONTAB-MC
                                              BBONTAB-MP
           OPEN INPUT BBONTAB
           INITIALIZE FAKPAR-DATA-FIELDS
           OPEN INPUT FAKPAR
           SET FAKBON-LEVEL-INIT           TO TRUE
           INITIALIZE FAKBON-DATA-FIELDS
           SET FAKBON-EOF-OFF              TO TRUE
           SET FAKBON-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO FAKBON-MC
                                              FAKBON-MP
           OPEN INPUT FAKBON
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE BBONTAB
           CLOSE FAKPAR
           CLOSE FAKBON
           CLOSE KUNDEMA
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
