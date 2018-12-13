       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK040R.
      ******************************************* :   Z-WIN-RPG2     **
      * PROGRAM FAK040
      *        PROGRAMERT AV: ESPEN LARSEN.  4/8-95                     *
      * DANNER BONUSRECORD FRA OPPSAMLEDE BONUSBERETTIGEDE FAKT.DATA    *
      * DISSE DATA SUMMERES OG DANNES NY FAKTURARECORD PR. VAREGRUPPE   *
      * PR. KUNDEGRUPPE (BILIMPORTØR)                                   *
      * BONUS KR.NOTA DANNES VED SISTE FAKTURERING PR. KVARTAL.
      * DETTE PROGRAM ER IKKE TILPASSET KONSERNMODELL, DA DET KUN ER
      *  FIRMA 918 SOM BENYTTER DETTE PROGRAM.
      * 05.09.00  LAGT INN AUTOINDUSTRI (SUBARU OG ROVER)
      * 04.10.00 FORD FJERNET SOM BONUSKUNDE FIRMA 918 I PROG FAK035  *
      * 11.10.00 NY RUTINE FOR BACKBONUS. BRUK BONUSTABELL FAK.FAKBONUS
      *            BRUK BONUSTABELL FAK.FAKBONUS I PROG FAK035
      *  6.01.06 MÅNEDLIG ELLER KVARTALSVIS DANNING AV KREDITNOTA.
      *******************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK040.rpg
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
           SELECT FAKBIN
               ASSIGN TO UT-S-FAKBIN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKBIN-STATUS.
           SELECT FAKPAR
               ASSIGN TO UT-S-FAKPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKPAR-STATUS.
           SELECT KUNDEMA
               ASSIGN TO KUNDEMA
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMA-STATUS
               RECORD KEY IS KUNDEMA-KEY1.
           SELECT FAKBUT
               ASSIGN TO UT-S-FAKBUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKBUT-STATUS.
           SELECT TILFAK
               ASSIGN TO UT-S-TILFAK
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TILFAK-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD FAKBIN
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  FAKBIN-IO-AREA.
           05  FAKBIN-IO-AREA-X            PICTURE X(200).
       FD FAKPAR
               BLOCK CONTAINS 200
               RECORD CONTAINS 200.
       01  FAKPAR-IO-AREA.
           05  FAKPAR-IO-AREA-X            PICTURE X(200).
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1            PICTURE X(9).
               10  FILLER                  PICTURE X(190).
       FD FAKBUT
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  FAKBUT-IO-AREA.
           05  FAKBUT-IO-AREA-X            PICTURE X(200).
       FD TILFAK
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  TILFAK-IO-AREA.
           05  TILFAK-IO-AREA-X            PICTURE X(200).
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
           10  FAKBIN-STATUS               PICTURE 99 VALUE 0.
           10  FAKPAR-STATUS               PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  FAKBUT-STATUS               PICTURE 99 VALUE 0.
           10  TILFAK-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  VLFELT-XX-STATUS            PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKBIN-EOF-OFF          VALUE '0'.
               88  FAKBIN-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKBIN-READ-OFF         VALUE '0'.
               88  FAKBIN-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKBIN-PROCESS-OFF      VALUE '0'.
               88  FAKBIN-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  FAKBIN-LEVEL-INIT-OFF   VALUE '0'.
               88  FAKBIN-LEVEL-INIT       VALUE '1'.
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
           05  FAKBIN-LEVEL-02.
               10  FAKBIN-02-L3.
                   15  FAKBIN-02-L3-FIRMNR PICTURE X(3).
               10  FAKBIN-02-L2.
                   15  FAKBIN-02-L2-RESKNR PICTURE X(6).
               10  FAKBIN-02-L1.
                   15  FAKBIN-02-L1-VGR    PICTURE X(5).
           05  FAKBIN-DATA-FIELDS.
               10  FIRMNR                  PICTURE X(3).
               10  RESKNR                  PICTURE X(6).
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
               10  FAKREC                  PICTURE X(200).
           05  FAKPAR-DATA-FIELDS.
               10  FAKDM                   PICTURE X(4).
               10  FAKDMA                  PICTURE X(6).
               10  FAKM                    PICTURE X(2).
               10  FAKA                    PICTURE X(2).
               10  FAKPER-IO.
                   15  FAKPER              PICTURE S9(6).
           05  KUNDEMA-DATA-FIELDS.
               10  KNAVN1                  PICTURE X(30).
               10  BETBET                  PICTURE X(2).
      *                                     170 170 KOD3X
               10  CFAKTM                  PICTURE X(1).
               10  CHND                    PICTURE X(3).
      *****************************************************************
      * NULLSTILLING OG SETOF AV INDIKATORER.                         *
      *****************************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(6).
               10  THE-PRIOR-L1            PICTURE X(5).
           05  TEMPORARY-FIELDS.
               10  SUML3-IO.
                   15  SUML3               PICTURE S9(7)V9(2).
               10  SEQNR-IO.
                   15  SEQNR               PICTURE S9(4).
               10  SUML2-IO.
                   15  SUML2               PICTURE S9(7)V9(2).
               10  SUML2B-IO.
                   15  SUML2B              PICTURE S9(7)V9(2).
               10  SUML2C-IO.
                   15  SUML2C              PICTURE S9(7)V9(2).
               10  SUML1-IO.
                   15  SUML1               PICTURE S9(7)V9(2).
               10  SUML1B-IO.
                   15  SUML1B              PICTURE S9(7)V9(2).
               10  SUML1C-IO.
                   15  SUML1C              PICTURE S9(7)V9(2).
               10  KVART                   PICTURE X(1).
               10  MNDNAV                  PICTURE X(8).
               10  EDBNRB                  PICTURE X(7).
               10  ANTINN-IO.
                   15  ANTINN              PICTURE S9(5).
               10  ANTKOP-IO.
                   15  ANTKOP              PICTURE S9(5).
               10  ANTFJ-IO.
                   15  ANTFJ               PICTURE S9(5).
               10  RESKEY                  PICTURE X(9).
               10  ANTSUM-IO.
                   15  ANTSUM              PICTURE S9(5).
               10  F11-IO.
                   15  F11                 PICTURE S9(9)V9(2).
               10  BONBEL-IO.
                   15  BONBEL              PICTURE S9(7)V9(2).
               10  BONBL2-IO.
                   15  BONBL2              PICTURE S9(7)V9(2).
               10  BONBL3-IO.
                   15  BONBL3              PICTURE S9(7)V9(2).
               10  ANTLEV-IO.
                   15  ANTLEV              PICTURE S9(5)V9(2).
               10  RAB0-IO.
                   15  RAB0                PICTURE S9(2)V9(1).
               10  SKOST-IO.
                   15  SKOST               PICTURE S9(7)V9(2).
               10  ANTFR-IO.
                   15  ANTFR               PICTURE S9(5).
           05  EDITTING-FIELDS.
               10  XO-22YY9                PICTURE ZZ,99.
               10  XO-72YY9R               PICTURE Z.ZZZ.ZZZ,99-.
               10  XO-52P-EF.
                 15  XO-52P                PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-72P-EF.
                 15  XO-72P                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
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
           SET NOT-I-02                    TO TRUE
           SET NOT-I-01                    TO TRUE
           SET NOT-I-04                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  FAKBIN-PROCESS
               SET FAKBIN-PROCESS-OFF      TO TRUE
               SET FAKBIN-READ             TO TRUE
           END-IF
 
           IF  FAKBIN-READ
           AND RECORD-SELECTED-OFF
               PERFORM FAKBIN-GET
               SET FAKBIN-READ-OFF         TO TRUE
               IF  NOT FAKBIN-EOF
                   SET FAKBIN-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  FAKBIN-PROCESS
               PERFORM FAKBIN-IDSET
           END-IF
 
           IF  FAKBIN-PROCESS
               PERFORM FAKBIN-CHK-LEVEL
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
 
           IF  FAKBIN-PROCESS
               PERFORM FAKBIN-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  FAKBIN-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-L3)
               SET NOT-I-80                TO TRUE
               SUBTRACT SUML3              FROM SUML3
           END-IF
           IF  (I-L2)
               SUBTRACT SEQNR              FROM SEQNR
               SET NOT-I-62                TO TRUE
               SUBTRACT SUML2              FROM SUML2
               SUBTRACT SUML2B             FROM SUML2B
               SUBTRACT SUML2C             FROM SUML2C
           END-IF
           IF  (I-L1)
               SET NOT-I-60                TO TRUE
               SUBTRACT SUML1              FROM SUML1
               SUBTRACT SUML1B             FROM SUML1B
               SUBTRACT SUML1C             FROM SUML1C
      *****************************************************************
      * FAKTURAPARAMETER RUTINE.                                      *
      *****************************************************************
           END-IF
           IF  (I-16)
               GO TO ENDPAR-T
           END-IF
           READ FAKPAR
           AT END
               SET I-17                    TO TRUE
           NOT AT END
               SET NOT-I-17                TO TRUE
               PERFORM FAKPAR-FLDSET
               PERFORM FAKPAR-IDSET
           END-READ
           SET NOT-I-40                    TO TRUE
           IF  FAKDM = '3103'
               SET I-40                    TO TRUE
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  FAKDM = '3006'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  FAKDM = '3009'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  FAKDM = '3112'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           SET NOT-I-41                    TO TRUE
           IF  FAKDM = '3103'
               SET I-41                    TO TRUE
           END-IF
           SET NOT-I-42                    TO TRUE
           IF  FAKDM = '3006'
               SET I-42                    TO TRUE
           END-IF
           SET NOT-I-43                    TO TRUE
           IF  FAKDM = '3009'
               SET I-43                    TO TRUE
           END-IF
           SET NOT-I-44                    TO TRUE
           IF  FAKDM = '3112'
               SET I-44                    TO TRUE
           END-IF
           IF  (I-41)
               MOVE '1'                    TO KVART
           END-IF
           IF  (I-42)
               MOVE '2'                    TO KVART
           END-IF
           IF  (I-43)
               MOVE '3'                    TO KVART
           END-IF
           IF  (I-44)
               MOVE '4'                    TO KVART
      *****************************************************************
           END-IF
           SET NOT-I-70                    TO TRUE
           IF  FAKDM = '3101'
               SET I-70                    TO TRUE
           END-IF
           IF  (NOT-I-70)
               SET NOT-I-70                TO TRUE
               IF  FAKDM = '2802'
                   SET I-70                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-70)
               SET NOT-I-70                TO TRUE
               IF  FAKDM = '2902'
                   SET I-70                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-70)
               SET NOT-I-70                TO TRUE
               IF  FAKDM = '3103'
                   SET I-70                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-70)
               SET NOT-I-70                TO TRUE
               IF  FAKDM = '3004'
                   SET I-70                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-70)
               SET NOT-I-70                TO TRUE
               IF  FAKDM = '3105'
                   SET I-70                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-70)
               SET NOT-I-70                TO TRUE
               IF  FAKDM = '3006'
                   SET I-70                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-70)
               SET NOT-I-70                TO TRUE
               IF  FAKDM = '3107'
                   SET I-70                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-70)
               SET NOT-I-70                TO TRUE
               IF  FAKDM = '3108'
                   SET I-70                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-70)
               SET NOT-I-70                TO TRUE
               IF  FAKDM = '3009'
                   SET I-70                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-70)
               SET NOT-I-70                TO TRUE
               IF  FAKDM = '3110'
                   SET I-70                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-70)
               SET NOT-I-70                TO TRUE
               IF  FAKDM = '3011'
                   SET I-70                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-70)
               SET NOT-I-70                TO TRUE
               IF  FAKDM = '3112'
                   SET I-70                TO TRUE
               END-IF
      *****************************************************************
           END-IF
           SET NOT-I-71                    TO TRUE
           IF  FAKDM = '3101'
               SET I-71                    TO TRUE
           END-IF
           IF  (I-71)
               MOVE 'JANUAR  '             TO MNDNAV
           END-IF
           SET NOT-I-71                    TO TRUE
           IF  FAKDM = '2802'
               SET I-71                    TO TRUE
           END-IF
           IF  (NOT-I-71)
               SET NOT-I-71                TO TRUE
               IF  FAKDM = '2902'
                   SET I-71                TO TRUE
               END-IF
           END-IF
           IF  (I-71)
               MOVE 'FEBRUAR '             TO MNDNAV
           END-IF
           SET NOT-I-71                    TO TRUE
           IF  FAKDM = '3103'
               SET I-71                    TO TRUE
           END-IF
           IF  (I-71)
               MOVE 'MARS    '             TO MNDNAV
           END-IF
           SET NOT-I-71                    TO TRUE
           IF  FAKDM = '3004'
               SET I-71                    TO TRUE
           END-IF
           IF  (I-71)
               MOVE 'APRIL   '             TO MNDNAV
           END-IF
           SET NOT-I-71                    TO TRUE
           IF  FAKDM = '3105'
               SET I-71                    TO TRUE
           END-IF
           IF  (I-71)
               MOVE 'MAI     '             TO MNDNAV
           END-IF
           SET NOT-I-71                    TO TRUE
           IF  FAKDM = '3006'
               SET I-71                    TO TRUE
           END-IF
           IF  (I-71)
               MOVE 'JUNI    '             TO MNDNAV
           END-IF
           SET NOT-I-71                    TO TRUE
           IF  FAKDM = '3107'
               SET I-71                    TO TRUE
           END-IF
           IF  (I-71)
               MOVE 'JULI    '             TO MNDNAV
           END-IF
           SET NOT-I-71                    TO TRUE
           IF  FAKDM = '3108'
               SET I-71                    TO TRUE
           END-IF
           IF  (I-71)
               MOVE 'AUGUST  '             TO MNDNAV
           END-IF
           SET NOT-I-71                    TO TRUE
           IF  FAKDM = '3009'
               SET I-71                    TO TRUE
           END-IF
           IF  (I-71)
               MOVE 'SEPT.   '             TO MNDNAV
           END-IF
           SET NOT-I-71                    TO TRUE
           IF  FAKDM = '3110'
               SET I-71                    TO TRUE
           END-IF
           IF  (I-71)
               MOVE 'OKTOBER '             TO MNDNAV
           END-IF
           SET NOT-I-71                    TO TRUE
           IF  FAKDM = '3011'
               SET I-71                    TO TRUE
           END-IF
           IF  (I-71)
               MOVE 'NOVEMBER'             TO MNDNAV
           END-IF
           SET NOT-I-71                    TO TRUE
           IF  FAKDM = '3112'
               SET I-71                    TO TRUE
           END-IF
           IF  (I-71)
               MOVE 'DESEMBER'             TO MNDNAV
      *****************************************************************
           END-IF
           SET I-16                        TO TRUE.
 
       ENDPAR-T.
      *****************************************************************
      * HOVEDRUTINE PR. FIRMA.                                        *
      *   ER DET KVARTALSVIS ELLER MÅNEDLIG                           *
      *****************************************************************
           IF  (I-L3)
               SET NOT-I-18                TO TRUE
               IF  FIRMNR = '918'
                   SET I-18                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND I-18)
               MOVE '9002553'              TO EDBNRB
           END-IF
           IF  (I-L3 AND I-18 AND I-40)
               SET I-80                    TO TRUE
           END-IF
           IF  (I-L3)
               SET NOT-I-19                TO TRUE
               IF  FIRMNR = '950'
                   SET I-19                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND I-19 AND I-70)
               MOVE '9001204'              TO EDBNRB
               SET I-80                    TO TRUE
           END-IF
           IF  (I-L3 AND I-80)
               PERFORM RBSRUT-S
      *****************************************************************
      * SKAL BONUS KR.NOTA DANNES ?                                   *
      *      DISSE DANNES KUN VED SISTE FAKTURERING -                 *
      *      PR. KVARTAL FOR FIRMA 918.                               *
      *      PR. MÅNED   FOR FIRMA 950.                               *
      *****************************************************************
           END-IF
           ADD 1                           TO ANTINN
           IF  (NOT-I-80)
               ADD 1                       TO ANTKOP
           END-IF
           IF  (I-80)
               ADD 1                       TO ANTFJ
           END-IF
           IF  (NOT-I-80)
               GO TO SLUTT-T
      *****************************************************************
      *  RUTINE FOR Å HENTE KUNDEOPPLYSNINGER.                        *
      *****************************************************************
           END-IF
           IF  (I-L2)
               MOVE RESKNR                 TO RESKEY (4:6)
               MOVE FIRMNR                 TO RESKEY (1:3)
               MOVE RESKEY                 TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-90                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-90            TO TRUE
                   PERFORM KUNDEMA-FLDSET
                   PERFORM KUNDEMA-IDSET
               END-READ
           END-IF
           IF  (I-90)
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
           END-IF
           IF  (NOT-I-89)
               ADD VLBEL                   TO SUML3
           END-IF
           IF  (I-89)
               SUBTRACT VLBEL              FROM SUML3
           END-IF
           ADD 1                           TO ANTSUM
      *****************************************************************
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
           MOVE 'FAK05'                    TO LONR
           MOVE FIRMNR                     TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'FAK040  '                 TO LPROG
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
           IF  (I-L1 AND I-18 AND NOT-I-80)
               SET NOT-I-60                TO TRUE
           END-IF
           IF  (I-L1 AND I-60)
               ADD 1                       TO SEQNR
               MULTIPLY BONPRO BY SUML1 GIVING F11 ROUNDED
               DIVIDE F11 BY 100       GIVING BONBEL ROUNDED
               ADD BONBEL                  TO BONBL2
               ADD BONBEL                  TO BONBL3
               MOVE 1                      TO ANTLEV
               MOVE 0                      TO RAB0
               MOVE 0                      TO SKOST
               ADD 1                       TO ANTFR
               SET I-62                    TO TRUE
      *****************************************************************
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           END-IF
           .
 
       FAKBIN-GET SECTION.
       FAKBIN-GET-P.
           IF  FAKBIN-EOF-OFF
               READ FAKBIN
               AT END
                   SET FAKBIN-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FAKBIN-FLDSET SECTION.
       FAKBIN-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKBIN-IO-AREA (1:3)   TO FIRMNR (1:3)
               MOVE FAKBIN-IO-AREA (4:6)   TO RESKNR (1:6)
               MOVE FAKBIN-IO-AREA (10:1)  TO FAKTYP (1:1)
               MOVE FAKBIN-IO-AREA (71:2)  TO ORDRDD (1:2)
               MOVE FAKBIN-IO-AREA (73:2)  TO ORDRMM (1:2)
               MOVE FAKBIN-IO-AREA (75:2)  TO ORDRAA (1:2)
               MOVE FAKBIN-IO-AREA (77:5)  TO VGR (1:5)
               MOVE FAKBIN-IO-AREA (77:1)  TO VGR1 (1:1)
               MOVE FAKBIN-IO-AREA (137:4) TO ANT-IO
               MOVE FAKBIN-IO-AREA (141:7) TO EDBNR-IO
               INSPECT EDBNR-IO REPLACING ALL ' ' BY '0'
               MOVE FAKBIN-IO-AREA (148:3) TO RAB1-IO
               INSPECT RAB1-IO REPLACING ALL ' ' BY '0'
               MOVE FAKBIN-IO-AREA (151:3) TO RAB2-IO
               INSPECT RAB2-IO REPLACING ALL ' ' BY '0'
               MOVE FAKBIN-IO-AREA (154:3) TO RAB3-IO
               INSPECT RAB3-IO REPLACING ALL ' ' BY '0'
               MOVE FAKBIN-IO-AREA (157:9) TO BEL-IO
               INSPECT BEL-IO REPLACING ALL ' ' BY '0'
               MOVE FAKBIN-IO-AREA (190:3) TO ALFAK (1:3)
               MOVE FAKBIN-IO-AREA (193:1) TO BONGR (1:1)
               MOVE FAKBIN-IO-AREA (194:4) TO BONPRO-IO
               INSPECT BONPRO-IO REPLACING ALL ' ' BY '0'
               MOVE FAKBIN-IO-AREA (1:200) TO FAKREC (1:200)
           END-EVALUATE.
 
       FAKBIN-IDSET SECTION.
       FAKBIN-IDSET-P.
           SET I-02                        TO TRUE.
 
       FAKBIN-CHK-LEVEL SECTION.
       FAKBIN-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO FAKBIN-LEVEL-02
               MOVE FAKBIN-IO-AREA (1:3)   TO FAKBIN-02-L3-FIRMNR
               MOVE FAKBIN-IO-AREA (4:6)   TO FAKBIN-02-L2-RESKNR
               MOVE FAKBIN-IO-AREA (77:5)  TO FAKBIN-02-L1-VGR
               IF  FAKBIN-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  FAKBIN-02-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  FAKBIN-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  FAKBIN-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  FAKBIN-02-L3          TO THE-PRIOR-L3
               MOVE  FAKBIN-02-L2          TO THE-PRIOR-L2
               MOVE  FAKBIN-02-L1          TO THE-PRIOR-L1
               SET FAKBIN-LEVEL-INIT       TO TRUE
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
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (16:30) TO KNAVN1 (1:30)
               MOVE KUNDEMA-IO-AREA (127:2) TO BETBET (1:2)
               MOVE KUNDEMA-IO-AREA (171:1) TO CFAKTM (1:1)
               MOVE KUNDEMA-IO-AREA (185:3) TO CHND (1:3)
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
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND NOT-I-80)
               MOVE SPACES TO FAKBUT-IO-AREA
               INITIALIZE FAKBUT-IO-AREA
               MOVE FAKREC                 TO FAKBUT-IO-AREA (1:200)
               WRITE FAKBUT-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L3 AND I-80)
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
               MOVE 'FIRMA'                TO LISTE-IO-AREA (2:5)
               MOVE 'BONUS TIL'            TO LISTE-IO-AREA (8:9)
               MOVE 'BONUS GRUNNLAG'       TO LISTE-IO-AREA (60:14)
               MOVE 'BONUS UTFAKT.'        TO LISTE-IO-AREA (76:13)
               MOVE 'VGR. '                TO LISTE-IO-AREA (90:5)
               MOVE 'PROS.'                TO LISTE-IO-AREA (96:5)
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
           IF  (I-OF AND NOT-I-L3)
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
               MOVE 'FIRMA'                TO LISTE-IO-AREA (2:5)
               MOVE 'BONUS TIL'            TO LISTE-IO-AREA (8:9)
               MOVE 'BONUS GRUNNLAG'       TO LISTE-IO-AREA (60:14)
               MOVE 'BONUS UTFAKT.'        TO LISTE-IO-AREA (76:13)
               MOVE 'VGR. '                TO LISTE-IO-AREA (90:5)
               MOVE 'PROS.'                TO LISTE-IO-AREA (96:5)
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
           IF  (I-L1 AND I-60)
               MOVE SPACES TO TILFAK-IO-AREA
               INITIALIZE TILFAK-IO-AREA
               MOVE FIRMNR                 TO TILFAK-IO-AREA (1:3)
               MOVE RESKNR                 TO TILFAK-IO-AREA (4:6)
               MOVE 'K'                    TO TILFAK-IO-AREA (10:1)
               MOVE 'F'                    TO TILFAK-IO-AREA (11:1)
               MOVE CFAKTM                 TO TILFAK-IO-AREA (12:1)
               MOVE 'P'                    TO TILFAK-IO-AREA (13:1)
               MOVE BETBET                 TO TILFAK-IO-AREA (14:2)
               MOVE '   '                  TO TILFAK-IO-AREA (16:3)
               MOVE '999999'               TO TILFAK-IO-AREA (19:6)
               MOVE 'L'                    TO TILFAK-IO-AREA (25:1)
               MOVE SEQNR-IO               TO TILFAK-IO-AREA (26:4)
               MOVE FAKDMA                 TO TILFAK-IO-AREA (30:6)
               IF  (I-18)
                   MOVE 'BONUS   KVARTAL'  TO TILFAK-IO-AREA (41:15)
               END-IF
               IF  (I-18)
                   MOVE KVART              TO TILFAK-IO-AREA (47:1)
               END-IF
               IF  (I-19)
                   MOVE 'BONUS          '  TO TILFAK-IO-AREA (41:15)
               END-IF
               IF  (I-19)
                   MOVE MNDNAV             TO TILFAK-IO-AREA (48:8)
               END-IF
               MOVE '      '               TO TILFAK-IO-AREA (56:6)
               MOVE ' '                    TO TILFAK-IO-AREA (66:1)
               MOVE '6'                    TO TILFAK-IO-AREA (67:1)
               MOVE CHND                   TO TILFAK-IO-AREA (68:3)
               MOVE FAKDMA                 TO TILFAK-IO-AREA (71:6)
               MOVE VGR                    TO TILFAK-IO-AREA (77:5)
               MOVE BONPRO                 TO XO-22YY9
               MOVE XO-22YY9               TO TILFAK-IO-AREA (82:5)
               MOVE 'PROS BONUS AV '       TO TILFAK-IO-AREA (88:14)
               MOVE SUML1                  TO XO-72YY9R
               MOVE XO-72YY9R              TO TILFAK-IO-AREA (102:13)
               MOVE ' VAREGR.         '    TO TILFAK-IO-AREA (115:17)
               MOVE VGR                    TO TILFAK-IO-AREA (124:5)
               MOVE 'B'                    TO TILFAK-IO-AREA (132:1)
               MOVE ANTLEV                 TO XO-52P
               MOVE XO-52P-EF              TO TILFAK-IO-AREA (133:4)
               MOVE ANTLEV                 TO XO-52P
               MOVE XO-52P-EF              TO TILFAK-IO-AREA (137:4)
               MOVE EDBNRB                 TO TILFAK-IO-AREA (141:7)
               MOVE RAB0-IO                TO TILFAK-IO-AREA (148:3)
               MOVE RAB0-IO                TO TILFAK-IO-AREA (151:3)
               MOVE RAB0-IO                TO TILFAK-IO-AREA (154:3)
               MOVE BONBEL-IO              TO TILFAK-IO-AREA (157:9)
               MOVE VGR1                   TO TILFAK-IO-AREA (166:1)
               MOVE FAKA                   TO TILFAK-IO-AREA (167:2)
               MOVE FAKM                   TO TILFAK-IO-AREA (169:2)
               MOVE SKOST                  TO XO-72P
               MOVE XO-72P-EF              TO TILFAK-IO-AREA (171:5)
               MOVE 'B'                    TO TILFAK-IO-AREA (177:1)
               MOVE '4'                    TO TILFAK-IO-AREA (179:1)
               MOVE '2'                    TO TILFAK-IO-AREA (181:1)
               MOVE RESKNR                 TO TILFAK-IO-AREA (184:6)
               MOVE ALFAK                  TO TILFAK-IO-AREA (190:3)
               WRITE TILFAK-IO-AREA
           END-IF
           IF  (I-L1 AND I-80 AND I-60)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FIRMNR                 TO LISTE-IO-AREA (2:3)
               MOVE RESKNR                 TO LISTE-IO-AREA (10:6)
               MOVE KNAVN1                 TO LISTE-IO-AREA (26:30)
               MOVE SUML1                  TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (61:13)
               MOVE BONBEL                 TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (76:13)
               MOVE VGR                    TO LISTE-IO-AREA (90:5)
               MOVE BONPRO                 TO XO-22YY9
               MOVE XO-22YY9               TO LISTE-IO-AREA (96:5)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L2 AND I-80 AND I-62)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FIRMNR                 TO LISTE-IO-AREA (2:3)
               MOVE RESKNR                 TO LISTE-IO-AREA (10:6)
               MOVE KNAVN1                 TO LISTE-IO-AREA (26:30)
               MOVE SUML2                  TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (61:13)
               MOVE BONBL2                 TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (76:13)
               INITIALIZE BONBL2
               MOVE 'KUNDE TOTALT'         TO LISTE-IO-AREA (90:12)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L3 AND I-80)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FIRMNR                 TO LISTE-IO-AREA (2:3)
               MOVE '***'                  TO LISTE-IO-AREA (8:3)
               MOVE 'TOTALT'               TO LISTE-IO-AREA (18:6)
               MOVE SUML3                  TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (61:13)
               MOVE BONBL3                 TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (76:13)
               INITIALIZE BONBL3
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTINN                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (5:6)
               INITIALIZE ANTINN
               MOVE 'BONUSGRUNNLAGSREC. LEST' TO LISTE-IO-AREA (12:23)
               MOVE 2                      TO LISTE-BEFORE-SPACE
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTFJ                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (5:6)
               INITIALIZE ANTFJ
               MOVE 'BONUSGRUNNLAGSREC. SLET' TO LISTE-IO-AREA (12:23)
               MOVE 'TET NÅ.'              TO LISTE-IO-AREA (35:7)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTKOP                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (5:6)
               INITIALIZE ANTKOP
               MOVE 'BONUSGRUNNLAGSREC. KOPI' TO LISTE-IO-AREA (12:23)
               MOVE 'ERT TIL NEST FAKT.'   TO LISTE-IO-AREA (35:18)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTSUM                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (5:6)
               INITIALIZE ANTSUM
               MOVE 'BONUSGRUNNLAGSREC. SUMM' TO LISTE-IO-AREA (12:23)
               MOVE 'ERT TIL BONUSFAKT.'   TO LISTE-IO-AREA (35:18)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTFR                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (5:6)
               INITIALIZE ANTFR
               MOVE 'NYE BONUS FAKTURA REC. ' TO LISTE-IO-AREA (12:23)
               MOVE 'DANNET TIL FAKT. NÅ'  TO LISTE-IO-AREA (35:19)
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
           SET FAKBIN-LEVEL-INIT           TO TRUE
           INITIALIZE FAKBIN-DATA-FIELDS
           SET FAKBIN-EOF-OFF              TO TRUE
           SET FAKBIN-PROCESS              TO TRUE
           OPEN INPUT FAKBIN
           INITIALIZE FAKPAR-DATA-FIELDS
           OPEN INPUT FAKPAR
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           OPEN OUTPUT FAKBUT
           OPEN OUTPUT TILFAK
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FAKBIN
           CLOSE FAKPAR
           CLOSE KUNDEMA
           CLOSE FAKBUT
           CLOSE TILFAK
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
