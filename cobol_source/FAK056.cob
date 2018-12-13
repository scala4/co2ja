       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK056R.
      ******************************************* :   Z-WIN-RPG2     **
      * PROGRAM: FAK056                                               *
      * PROGRAMMET DANNER:                                            *
      * FAKTURA GEBYR RECORDS FOR HAFNOR/HYDROTEXACO,                 *
      * FAKTURATOTAL 0.100 - 1.999 GIR 200,- I GEBYR.                 *
      * FAKTURATOTAL 2.000 - 3.999 GIR 100,- I GEBYR.                 *
      * KUNDER UTEN BET.MÅTE AUTOGIRO   45,- I GEBYR.                 *
      * UPSI 1 KUN KONTROLL LISTE.                                    *
      * 14.08.06 PROGRAMMET PROGRAMMERT AV ESPEN LARSEN               *
      * 07.09.06 ANBREKK AV HEL KARTONG 10,- I GEBYR                  *
      * 08.09.06 NYE ØVRE GRENSE FOR FAKTURAGEBYR ER 2.999            *
      * 15.09.06 LAGT INN BEREGNING AV GEBYR PÅ MANUELLE ORDRE.       *
      * 18.09.06 KUNDER I KUNDETAB. SOM KUN HAR KJØPT DEKK SKAL IKKE  *
      *          GEBYR PÅ MANUELLE ORDRE.                             *
      *  3.10.06 KUNDER I KUNDETAB-2 SKAL IKKE HA BEST.GEBYR.         *
      *  4.10.06 AUTOGIROGEBYR SKAL IKKE LENGER BEREGNES.             *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK056.rpg
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
           SELECT KNRTAB
               ASSIGN TO UT-S-KNRTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KNRTAB-STATUS.
           SELECT KNTAB2
               ASSIGN TO UT-S-KNTAB2
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KNTAB2-STATUS.
           SELECT FAKPAR
               ASSIGN TO UT-S-FAKPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKPAR-STATUS.
           SELECT FAKTIN
               ASSIGN TO UT-S-FAKTIN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKTIN-STATUS.
           SELECT KUNDEMA
               ASSIGN TO KUNDEMA
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMA-STATUS
               RECORD KEY IS KUNDEMA-KEY1.
           SELECT VAREMAS
               ASSIGN TO VAREMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAREMAS-STATUS
               RECORD KEY IS VAREMAS-KEY1.
           SELECT FAKTUT
               ASSIGN TO UT-S-FAKTUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKTUT-STATUS.
           SELECT PRF
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PRF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD KNRTAB
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  KNRTAB-IO-AREA.
           05  KNRTAB-IO-AREA-X            PICTURE X(80).
       FD KNTAB2
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  KNTAB2-IO-AREA.
           05  KNTAB2-IO-AREA-X            PICTURE X(80).
       FD FAKPAR
               BLOCK CONTAINS 200
               RECORD CONTAINS 200.
       01  FAKPAR-IO-AREA.
           05  FAKPAR-IO-AREA-X            PICTURE X(200).
       FD FAKTIN
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  FAKTIN-IO-AREA.
           05  FAKTIN-IO-AREA-X            PICTURE X(200).
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1            PICTURE X(9).
               10  FILLER                  PICTURE X(190).
       FD VAREMAS
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X.
               10  VAREMAS-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       FD FAKTUT
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  FAKTUT-IO-AREA.
           05  FAKTUT-IO-AREA-X            PICTURE X(200).
       FD PRF
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  PRF-IO-PRINT.
           05  PRF-IO-AREA-CONTROL         PICTURE X VALUE ' '.
        02 PRF-IO-AREA.
           05  PRF-IO-AREA-X               PICTURE X(132).
       WORKING-STORAGE SECTION.
       77  TABKNR-MAX   VALUE 200          PICTURE 9(4) USAGE BINARY.
       77  TAB2KN-MAX   VALUE 200          PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  TABKNR-TABLE.
               10  TABKNR-ENTRY
                                           OCCURS 200 TIMES
                                           INDEXED BY TABKNR-I
                                                      TABKNR-S.
                   15  TABKNR              PICTURE X(6).
      * **START RBS - DATASTRUKTUR FOR SUB-PROGRAM RBS000 ********              & K
           05  TAB2KN-TABLE.
               10  TAB2KN-ENTRY
                                           OCCURS 200 TIMES
                                           INDEXED BY TAB2KN-I
                                                      TAB2KN-S.
                   15  TAB2KN              PICTURE X(6).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  KNRTAB-STATUS               PICTURE 99 VALUE 0.
           10  KNTAB2-STATUS               PICTURE 99 VALUE 0.
           10  FAKPAR-STATUS               PICTURE 99 VALUE 0.
           10  FAKTIN-STATUS               PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  FAKTUT-STATUS               PICTURE 99 VALUE 0.
           10  PRF-STATUS                  PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  KNRTAB-EOF-OFF          VALUE '0'.
               88  KNRTAB-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KNTAB2-EOF-OFF          VALUE '0'.
               88  KNTAB2-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKTIN-EOF-OFF          VALUE '0'.
               88  FAKTIN-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKTIN-READ-OFF         VALUE '0'.
               88  FAKTIN-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKTIN-PROCESS-OFF      VALUE '0'.
               88  FAKTIN-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  FAKTIN-LEVEL-INIT-OFF   VALUE '0'.
               88  FAKTIN-LEVEL-INIT       VALUE '1'.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  VAREMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  PRF-DATA-FIELDS.
               10  PRF-AFTER-SPACE         PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRF-AFTER-SKIP          PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRF-BEFORE-SPACE        PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRF-BEFORE-SKIP         PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRF-MAX-LINES           PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRF-LINE-COUNT          PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRF-CLR-IO              PICTURE X VALUE 'Y'.
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
           05  FAKPAR-DATA-FIELDS.
               10  SFMND                   PICTURE X(1).
           05  FAKTIN-LEVEL-03.
               10  FAKTIN-03-L4.
                   15  FAKTIN-03-L4-FIRMNR PICTURE X(3).
               10  FAKTIN-03-L3.
                   15  FAKTIN-03-L3-RESKNR PICTURE X(6).
               10  FAKTIN-03-L2.
                   15  FAKTIN-03-L2-FAKTYP PICTURE X(1).
               10  FAKTIN-03-L1.
                   15  FAKTIN-03-L1-ORDNR  PICTURE X(6).
           05  FAKTIN-DATA-FIELDS.
               10  FIRMNR                  PICTURE X(3).
               10  FAKTYP                  PICTURE X(1).
      *                                   P  16  180BRKOD2
               10  FAKREC                  PICTURE X(200).
               10  FAKART                  PICTURE X(1).
      *                                      14  15 BETB
               10  ORDNR                   PICTURE X(6).
               10  RECART                  PICTURE X(1).
               10  SEQNR-IO.
                   15  SEQNR               PICTURE S9(4).
               10  BESGEB                  PICTURE X(1).
               10  LAGERK                  PICTURE X(2).
      *                                      66  66 BK
      *                                      68  70 HDIST
      *                                      71  72 ODAG
      *                                      73  74 OMND
      *                                      75  76 OAAR
      *                                      77  81 VGR
               10  VGR1F                   PICTURE X(1).
               10  ARTNR                   PICTURE X(20).
      *                                     132 132 OKODE
               10  BESENH-IO.
                   15  BESENH              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  LEVENH-IO.
                   15  LEVENH              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  EDBNR                   PICTURE X(7).
               10  EDB2F                   PICTURE X(2).
               10  EDB3F                   PICTURE X(3).
               10  RAB1-IO.
                   15  RAB1                PICTURE S9(2)V9(1).
               10  RAB2-IO.
                   15  RAB2                PICTURE S9(2)V9(1).
               10  RAB3-IO.
                   15  RAB3                PICTURE S9(2)V9(1).
               10  ENHPR-IO.
                   15  ENHPR               PICTURE S9(7)V9(2).
      *                                     166 166 REGAVD
      *                                     167 168 KPAR
      *                                     169 170 KPMND
      *                                   P 171 1752KOSTPR
      *                                     176 176 BONUSK
               10  RECTYP                  PICTURE X(1).
      *                                     178 178 OGEBYR
      *                                     179 179 KRETYP
               10  RESKNR                  PICTURE X(6).
               10  ALFAK                   PICTURE X(3).
           05  KUNDEMA-DATA-FIELDS.
               10  KNAVN                   PICTURE X(30).
               10  BETM                    PICTURE X(2).
               10  KKAT-IO.
                   15  KKAT                PICTURE S9(3) USAGE
                                                       PACKED-DECIMAL.
           05  VAREMAS-DATA-FIELDS.
               10  MERKN                   PICTURE X(1).
               10  EDBNR7                  PICTURE X(7).
               10  AKART-IO.
                   15  AKART               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  SKART                   PICTURE X(1).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L4            PICTURE X(3).
               10  THE-PRIOR-L3            PICTURE X(6).
               10  THE-PRIOR-L2            PICTURE X(1).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  ANTINN-IO.
                   15  ANTINN              PICTURE S9(7).
               10  FNRKNR                  PICTURE X(9).
               10  AGEBYR-IO.
                   15  AGEBYR              PICTURE S9(7)V9(2).
               10  ANTLA0-IO.
                   15  ANTLA0              PICTURE S9(5).
               10  NYPRIS-IO.
                   15  NYPRIS              PICTURE S9(7)V9(2).
               10  FNRENR                  PICTURE X(10).
               10  ANTHK-IO.
                   15  ANTHK               PICTURE S9(5).
               10  ANTSTK-IO.
                   15  ANTSTK              PICTURE S9(5).
               10  REST-IO.
                   15  REST                PICTURE S9(5)V9(2).
               10  ANTABF-IO.
                   15  ANTABF              PICTURE S9(5)V9(2).
               10  ANTABX-IO.
                   15  ANTABX              PICTURE S9(5).
               10  ANTABT-IO.
                   15  ANTABT              PICTURE S9(5).
               10  BGEBYR-IO.
                   15  BGEBYR              PICTURE S9(7)V9(2).
               10  BELABK-IO.
                   15  BELABK              PICTURE S9(7)V9(2).
               10  BELABT-IO.
                   15  BELABT              PICTURE S9(7)V9(2).
               10  ANTGEB-IO.
                   15  ANTGEB              PICTURE S9(5).
               10  GEBANT-IO.
                   15  GEBANT              PICTURE S9(5)V9(2).
               10  NULL-X-IO.
                   15  NULL-X              PICTURE S9(7)V9(2).
               10  MGEBYR-IO.
                   15  MGEBYR              PICTURE S9(7)V9(2).
               10  SEQNR2-IO.
                   15  SEQNR2              PICTURE S9(4).
               10  ANTF-IO.
                   15  ANTF                PICTURE S9(7).
               10  TOTGEB-IO.
                   15  TOTGEB              PICTURE S9(7)V9(2).
               10  ANTBGT-IO.
                   15  ANTBGT              PICTURE S9(5).
               10  BELBGT-IO.
                   15  BELBGT              PICTURE S9(7)V9(2).
               10  ANTBON-IO.
                   15  ANTBON              PICTURE S9(5).
               10  BONANT-IO.
                   15  BONANT              PICTURE S9(5)V9(2).
               10  SEQNR3-IO.
                   15  SEQNR3              PICTURE S9(4).
               10  SEQNR4-IO.
                   15  SEQNR4              PICTURE S9(4).
               10  TOTBON-IO.
                   15  TOTBON              PICTURE S9(7)V9(2).
               10  LINSUM-IO.
                   15  LINSUM              PICTURE S9(7)V9(2).
               10  SUM-X-IO.
                   15  SUM-X               PICTURE S9(10)V9(2).
               10  BONGRL-IO.
                   15  BONGRL              PICTURE S9(7)V9(2).
               10  FAKTOT-IO.
                   15  FAKTOT              PICTURE S9(7)V9(2).
               10  GEBGRL-IO.
                   15  GEBGRL              PICTURE S9(7)V9(2).
               10  FGEBYR-IO.
                   15  FGEBYR              PICTURE S9(7)V9(2).
               10  BEL94-IO.
                   15  BEL94               PICTURE S9(5)V9(4).
               10  FBONUS-IO.
                   15  FBONUS              PICTURE S9(7)V9(2).
           05  EDITTING-FIELDS.
               10  XO-52P-EF.
                 15  XO-52P                PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-72P-EF.
                 15  XO-72P                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-50YN9                PICTURE ZZZZ9.
               10  XO-52YN9                PICTURE ZZZZZ,99.
               10  XO-30D                  PICTURE S9(3).
               10  XO-30U                  PICTURE 9(3).
               10  XO-72YY9R               PICTURE Z.ZZZ.ZZZ,99-.
               10  XO-50YY9                PICTURE ZZ.ZZ9.
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
           SET NOT-I-06                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-04                    TO TRUE
           SET NOT-I-05                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  FAKTIN-PROCESS
               SET FAKTIN-PROCESS-OFF      TO TRUE
               SET FAKTIN-READ             TO TRUE
           END-IF
 
           IF  FAKTIN-READ
           AND RECORD-SELECTED-OFF
               PERFORM FAKTIN-GET
               SET FAKTIN-READ-OFF         TO TRUE
               IF  NOT FAKTIN-EOF
                   SET FAKTIN-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  FAKTIN-PROCESS
               PERFORM FAKTIN-IDSET
           END-IF
 
           IF  FAKTIN-PROCESS
               PERFORM FAKTIN-CHK-LEVEL
           END-IF
 
           IF I-1ST
               GO TO LR-INDICATOR-TEST
           END-IF.
 
       MAINLINE-TOTAL-CALCS.
           PERFORM TOTAL-CALCS
           IF  I-LR
               PERFORM LR-CALCS
           END-IF
           PERFORM TOTAL-OUTPUT.
 
       LR-INDICATOR-TEST.
           IF  I-LR
               GO TO MAINLINE-TERMINATION
           END-IF
           PERFORM HEADING-OVERFLOW
 
           IF  FAKTIN-PROCESS
               PERFORM FAKTIN-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  FAKTIN-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-L4 AND NOT-I-99)
               READ FAKPAR
               AT END
                   SET I-H0                TO TRUE
                   MOVE 'M'                TO E-R-R-O-R
               NOT AT END
                   PERFORM FAKPAR-FLDSET
                   PERFORM FAKPAR-IDSET
               END-READ
               SET NOT-I-87                TO TRUE
               IF  SFMND = 'J'
                   SET I-87                TO TRUE
               END-IF
           END-IF
           IF  (I-L4)
               SET I-99                    TO TRUE
      *****************************************************************
      *  HOVEDRUTINE.                                                 *
      *****************************************************************
           END-IF
           IF  (I-03)
               ADD 1                       TO ANTINN
           END-IF
           IF  (I-L2)
               SET NOT-I-22                TO TRUE
               SET NOT-I-25                TO TRUE
               SET NOT-I-65                TO TRUE
               SET NOT-I-81                TO TRUE
               SET NOT-I-82                TO TRUE
               SET NOT-I-83                TO TRUE
               SET NOT-I-84                TO TRUE
               SET NOT-I-31                TO TRUE
               SET NOT-I-32                TO TRUE
               SET NOT-I-39                TO TRUE
           END-IF
           IF  (I-L1)
               SET NOT-I-35                TO TRUE
               SET NOT-I-36                TO TRUE
               SET NOT-I-37                TO TRUE
           END-IF
           IF  (I-03)
               SET NOT-I-30                TO TRUE
               SET NOT-I-63                TO TRUE
               SET NOT-I-38                TO TRUE
           END-IF
           IF  (I-L2)
               SUBTRACT BONGRL             FROM BONGRL
               SUBTRACT GEBGRL             FROM GEBGRL
               SUBTRACT FAKTOT             FROM FAKTOT
               SUBTRACT ANTABF             FROM ANTABF
               SUBTRACT ANTABX             FROM ANTABX
               SUBTRACT BELABK             FROM BELABK
           END-IF
           IF  (I-03)
               SET NOT-I-10                TO TRUE
      *****************************************************************
      * TEST PÅ FIRMA.                                                *
      * utgår i fra fakt 8.7.07 iflg Tommy Land.                      *
      *****************************************************************
           END-IF
           IF  (I-L4)
               SET NOT-I-23                TO TRUE
               IF  FIRMNR = '999'
                   SET I-23                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-23)
               GO TO SLUTT-T
           END-IF
           IF  (I-L4)
               PERFORM RBSRUT-S
      *****************************************************************
      * TEST PÅ KUNDEKATEGORI. KUN HYDROTEXACO. KUNDEKAT. X51         *
      *****************************************************************
           END-IF
           IF  (I-L3)
               MOVE FIRMNR                 TO FNRKNR (1:3)
               MOVE RESKNR                 TO FNRKNR (4:6)
               MOVE FNRKNR                 TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-24                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-24            TO TRUE
                   PERFORM KUNDEMA-FLDSET
                   PERFORM KUNDEMA-IDSET
               END-READ
           END-IF
           IF  (I-L3 AND NOT-I-24)
               SET NOT-I-25                TO TRUE
               IF  KKAT = 051
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-24 AND NOT-I-25)
               SET NOT-I-25                TO TRUE
               IF  KKAT = 151
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-24 AND NOT-I-25)
               SET NOT-I-25                TO TRUE
               IF  KKAT = 251
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-24 AND NOT-I-25)
               SET NOT-I-25                TO TRUE
               IF  KKAT = 351
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-24 AND NOT-I-25)
               SET NOT-I-25                TO TRUE
               IF  KKAT = 451
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-24 AND NOT-I-25)
               SET NOT-I-25                TO TRUE
               IF  KKAT = 551
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-24 AND NOT-I-25)
               SET NOT-I-25                TO TRUE
               IF  KKAT = 651
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-24 AND NOT-I-25)
               SET NOT-I-25                TO TRUE
               IF  KKAT = 751
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-24 AND NOT-I-25)
               SET NOT-I-25                TO TRUE
               IF  KKAT = 851
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-24 AND NOT-I-25)
               SET NOT-I-25                TO TRUE
               IF  KKAT = 951
                   SET I-25                TO TRUE
               END-IF
      *****************************************************************
      * TEST PÅ BETALINGSKODE. OM DET IKKE ER AUTOGIRO SKAL DETTE GI  *
      *      GEBYR. KUN VED SISTE FAKT. I MND.                        *
      *      AUTOGIROGEBYR SKAL IKKE LENGERE BENYTTES.                *
      *****************************************************************
           END-IF
           IF  (I-L2)
               SET NOT-I-26                TO TRUE
               IF  FAKTYP = 'F'
                   SET I-26                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-25)
               SET NOT-I-21                TO TRUE
               IF  BETM = '11'
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-25 AND NOT-I-21)
               SET NOT-I-21                TO TRUE
               IF  BETM = '12'
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-25 AND NOT-I-21)
               SET NOT-I-21                TO TRUE
               IF  BETM = '07'
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-25 AND NOT-I-21)
               SET NOT-I-21                TO TRUE
               IF  BETM = '47'
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-25 AND NOT-I-21)
               SET NOT-I-21                TO TRUE
               IF  BETM = '14'
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-25 AND NOT-I-21)
               AND (I-87 AND I-26)
               SET I-22                    TO TRUE
           END-IF
           IF  (I-L2 AND I-25 AND I-22)
               MOVE 45,00                  TO AGEBYR
           END-IF
           IF  (I-L2)
               SET NOT-I-22                TO TRUE
      *****************************************************************
      * TEST PÅ OM DET ER KREDITNOTA, VARELINJE OG TEKSTLINJE.        *
      *      ELLER OM DET ER RENTE/GEBYR/BONUS LINJE.                 *
      *****************************************************************
           END-IF
           IF  (I-03)
               SET NOT-I-28                TO TRUE
               IF  RECTYP = 'R'
                   SET I-28                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-28)
               SET NOT-I-28                TO TRUE
               IF  RECTYP = 'F'
                   SET I-28                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-28)
               SET NOT-I-28                TO TRUE
               IF  RECTYP = 'B'
                   SET I-28                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-28)
               GO TO SLUTT-T
           END-IF
           IF  (I-03)
               SET NOT-I-27                TO TRUE
               IF  RECART = 'L'
                   SET I-27                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-27)
               GO TO SLUTT-T
           END-IF
           IF  (I-03)
               SET NOT-I-28                TO TRUE
               IF  EDBNR > '0000000'
                   SET I-28                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-28)
               GO TO SLUTT-T
           END-IF
           IF  (I-03)
               SET NOT-I-10                TO TRUE
               IF  LEVENH = 0,00
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-10)
               ADD 1                       TO ANTLA0
           END-IF
           IF  (I-03 AND I-24)
               GO TO SLUTT-T
           END-IF
           IF  (I-03 AND NOT-I-25)
               GO TO SLUTT-T
           END-IF
           IF  (I-03)
               SET NOT-I-26                TO TRUE
               IF  FAKTYP = 'F'
                   SET I-26                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-26)
               GO TO SLUTT-T
      *****************************************************************
      * ORDRE MED 1 GEBYR SKAL IKKE VÆRE MED I BEREGNINGSGRUNNLAGET.  *
      *       DETTE ER FJERNET FOR HAFNOR.                            *
      *****************************************************************
      *  03      OGEBYR    COMP "1"                      30 IKKE GEBYR
      *  03 30             SETON                     31     IKKE GEBYR
      *  03 30             GOTO SLUTT
      *****************************************************************
      * ORDRE MED KODE B I BESGEB ER MANUELLE ORDRE SOM SKAL TILDELES *
      *       BESTILLINGSGEBYR PÅ KR. 50,-  (KODEN TILDELES I FAK030) *
      *****************************************************************
           END-IF
           IF  (I-03 AND NOT-I-35)
               SET NOT-I-35                TO TRUE
               IF  BESGEB = 'B'
                   SET I-35                TO TRUE
               END-IF
      *****************************************************************
      * SJEKK OM DET KUN ER KJØPT DEKK.                               *
      *****************************************************************
           END-IF
           IF  (I-03 AND NOT-I-36)
               SET NOT-I-36                TO TRUE
               IF  VGR1F NOT = '2'
                   SET I-36                TO TRUE
               END-IF
      *****************************************************************
      *  HVIS EDBNR STARTER MED "94" ELLER "995" SNUS BELØPET.        *
      *****************************************************************
           END-IF
           IF  (I-03)
               MOVE ENHPR                  TO NYPRIS-IO
               SET NOT-I-33                TO TRUE
               IF  EDB3F = '995'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  EDB2F = '94'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-33)
               MULTIPLY -1 BY ENHPR    GIVING NYPRIS
           END-IF
           IF  (I-03)
               PERFORM FAKSUM-S
      *****************************************************************
      * OPPSLAG PÅ VAREMASTER.                                        *
      *****************************************************************
           END-IF
           IF  (I-03)
               MOVE FIRMNR                 TO FNRENR (1:3)
               MOVE EDBNR                  TO FNRENR (4:7)
               MOVE FNRENR                 TO VAREMAS-KEY1
               READ VAREMAS RECORD KEY IS VAREMAS-KEY1
               INVALID KEY
                   SET I-29                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-29            TO TRUE
                   PERFORM VAREMAS-FLDSET
                   PERFORM VAREMAS-IDSET
               END-READ
           END-IF
           IF  (I-03 AND NOT-I-29)
               SET NOT-I-40                TO TRUE
               IF  LAGERK = 'PT'
                   SET I-40                TO TRUE
               END-IF
      *****************************************************************
      * RUTINE FOR Å SJEKKE OM DET ER ANBREKK.                        *
      * VARER SOM IKKE SELGES I HELE KARTONGER SKAL HA GEBYR PÅ       *
      *   KR. 10,- PR. ANBREKK.                                       *
      *   ER ANTALL BEST. = 0 BLIR DET INGEN GEBYR.                   *
      *   ER ANTALL LEV.  = 0 BLIR DET INGEN GEBYR.                   *
      *   ER ANTALL LEV. ULIK ANTALL BEST BLIR DET INGEN GEBYR.       *
      *   ANBREKK UTGÅR IFLG TOMMY LAND 17.01.07
      *****************************************************************
           END-IF
           IF  (I-03)
               GO TO ENDKAR-T
           END-IF
           IF  (I-03 AND I-29)
               GO TO ENDKAR-T
           END-IF
           IF  (I-03 AND I-40)
               GO TO ENDKAR-T
           END-IF
           IF  (I-03)
               SET NOT-I-41                TO TRUE
               IF  SKART = 'K'
                   SET I-41                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-41)
               GO TO ENDKAR-T
           END-IF
           IF  (I-03)
               SET NOT-I-42                TO TRUE
               IF  AKART > 1
                   SET I-42                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-42)
               GO TO ENDKAR-T
           END-IF
           IF  (I-03)
               SET NOT-I-42                TO TRUE
               IF  BESENH > 0,00
                   SET I-42                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-42)
               GO TO ENDKAR-T
           END-IF
           IF  (I-03)
               SET NOT-I-42                TO TRUE
               IF  LEVENH > 0,00
                   SET I-42                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-42)
               GO TO ENDKAR-T
           END-IF
           IF  (I-03)
               SET NOT-I-42                TO TRUE
               IF  LEVENH = BESENH
                   SET I-42                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-42)
               GO TO ENDKAR-T
      *
           END-IF
           IF  (I-03)
               SET NOT-I-43                TO TRUE
               IF  BESENH < AKART
                   SET I-43                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-43)
               GO TO ANBGEB-T
      *
           END-IF
           IF  (I-03)
               DIVIDE BESENH BY AKART  GIVING ANTHK
               MULTIPLY AKART BY ANTHK GIVING ANTSTK
               SUBTRACT ANTSTK FROM BESENH GIVING REST
               SET NOT-I-44                TO TRUE
               IF  REST = 0
                   SET I-44                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-44)
               GO TO ENDKAR-T
           END-IF.
 
       ANBGEB-T.
           IF  (I-03)
               SET I-38                    TO TRUE
               SET I-39                    TO TRUE
               ADD 1,00                    TO ANTABF
               ADD 1,00                    TO ANTABX
               ADD 1                       TO ANTABT
               MOVE 10,00                  TO BGEBYR
               ADD BGEBYR                  TO BELABK
               ADD BGEBYR                  TO BELABT
           END-IF.
 
       ENDKAR-T.
      *****************************************************************
      * SJEKK OM VARELINJEN SKAL VÆRE MED I BEREGNINGSGRUNNLAGET.     *
      *****************************************************************
           IF  (I-03 AND NOT-I-29)
               SET NOT-I-30                TO TRUE
               IF  MERKN = 'F'
                   SET I-30                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-29 AND I-30)
               SET I-32                    TO TRUE
           END-IF
           IF  (I-03)
               PERFORM GEBSUM-S
           END-IF
           IF  (I-03 AND NOT-I-29 AND I-30)
               GO TO SLUTT-T
           END-IF
           IF  (I-03)
               PERFORM GRLSUM-S
           END-IF.
 
       SLUTT-T.
      *****************************************************************
      *  TOTALRUTINE FOR BESTILLINGSGEBYR MANUELLE ORDRE.             *
      *  KUNDER I TABELL-2 SKAL ALDRI HA BEST.GEBYR.                  *
      *  KUNDER I TABELL SOM KUN HAR KJØPT GUMMI SKAL IKKE HA GEBYR.  *
      *****************************************************************
           CONTINUE.
 
       GRLSUM-S SECTION.
       GRLSUM-S-P.
           MULTIPLY BESENH BY NYPRIS   GIVING LINSUM
           MULTIPLY RAB1 BY LINSUM     GIVING SUM-X
           DIVIDE SUM-X BY 100         GIVING SUM-X
           SUBTRACT SUM-X                  FROM LINSUM
           MULTIPLY RAB2 BY LINSUM     GIVING SUM-X
           DIVIDE SUM-X BY 100         GIVING SUM-X
           SUBTRACT SUM-X                  FROM LINSUM
           MULTIPLY RAB3 BY LINSUM     GIVING SUM-X
           DIVIDE SUM-X BY 100         GIVING SUM-X
           SUBTRACT SUM-X                  FROM LINSUM
           ADD LINSUM                      TO BONGRL
           SET I-65                        TO TRUE.
      ******************************************************
      * SUBRUTINE FOR UTREGNING AV FAKTURATOTAL FOR        *
      * KONTROLLISTE                                       *
      * REGNES UTIFRA LEV.ANTALL FOR HAFNOR.               *
      ******************************************************
 
       FAKSUM-S SECTION.
       FAKSUM-S-P.
           MULTIPLY LEVENH BY NYPRIS   GIVING LINSUM
           MULTIPLY RAB1 BY LINSUM     GIVING SUM-X
           DIVIDE SUM-X BY 100         GIVING SUM-X
           SUBTRACT SUM-X                  FROM LINSUM
           MULTIPLY RAB2 BY LINSUM     GIVING SUM-X
           DIVIDE SUM-X BY 100         GIVING SUM-X
           SUBTRACT SUM-X                  FROM LINSUM
           MULTIPLY RAB3 BY LINSUM     GIVING SUM-X
           DIVIDE SUM-X BY 100         GIVING SUM-X
           SUBTRACT SUM-X                  FROM LINSUM
           ADD LINSUM                      TO FAKTOT.
      ******************************************************
      * SUBRUTINE FOR UTREGNING AV GEBYR-GRUNNLAG          *
      * REGNES UTIFRA BST.ANTALL.                          *
      *  VARER MERKET MED "F" I MERKNAD SKAL IIKE VÆRE MED *
      *  MED MINDRE DET ER OLJEPRODUKTER (VGR. 60001-60030)*
      ******************************************************
 
       GEBSUM-S SECTION.
       GEBSUM-S-P.
      *          VGR       COMP "60001"              61
      *          VGR       COMP "60030"                6262
      *  61 62             SETON                     63
      *  30N63             GOTO GEBSUX
           IF  (I-30)
               GO TO GEBSUX-T
           END-IF
           MULTIPLY BESENH BY NYPRIS   GIVING LINSUM
           MULTIPLY RAB1 BY LINSUM     GIVING SUM-X
           DIVIDE SUM-X BY 100         GIVING SUM-X
           SUBTRACT SUM-X                  FROM LINSUM
           MULTIPLY RAB2 BY LINSUM     GIVING SUM-X
           DIVIDE SUM-X BY 100         GIVING SUM-X
           SUBTRACT SUM-X                  FROM LINSUM
           MULTIPLY RAB3 BY LINSUM     GIVING SUM-X
           DIVIDE SUM-X BY 100         GIVING SUM-X
           SUBTRACT SUM-X                  FROM LINSUM
           ADD LINSUM                      TO GEBGRL.
 
       GEBSUX-T.
           CONTINUE.
      *****************************************************************
      * SUBRUTINE FOR TILDELING AV FAKTURAGEBYR/FAKTURABONUS.         *
      *****************************************************************
 
       TILRUT-S SECTION.
       TILRUT-S-P.
           SET NOT-I-71                    TO TRUE
           IF  BONGRL < 1,00
               SET I-71                    TO TRUE
           END-IF
           IF  (NOT-I-71)
               SET NOT-I-71                TO TRUE
               IF  FAKTOT < 1,00
                   SET I-71                TO TRUE
               END-IF
           END-IF
           IF  (I-71)
               MOVE 0,00                   TO FGEBYR
               GO TO ENDTIL-T
      *
           END-IF
           SET NOT-I-71                    TO TRUE
           IF  GEBGRL < 2000,00
               SET I-71                    TO TRUE
           END-IF
           IF  (I-71)
               MOVE 200,00                 TO FGEBYR
               SET I-81                    TO TRUE
               SET I-83                    TO TRUE
               GO TO ENDTIL-T
      *
           END-IF
           SET NOT-I-71                    TO TRUE
           IF  GEBGRL < 3000,00
               SET I-71                    TO TRUE
           END-IF
           IF  (I-71)
               MOVE 100,00                 TO FGEBYR
               SET I-81                    TO TRUE
               SET I-84                    TO TRUE
               GO TO ENDTIL-T
           END-IF
           GO TO ENDTIL-T
      *****************************************************************
      * BONUSGRUNNLAG BENYTTES IKKE. KUN GEBYRGRUNNLAG.               *
      *****************************************************************
      *
           SET NOT-I-71                    TO TRUE
           IF  BONGRL < 6001,00
               SET I-71                    TO TRUE
           END-IF
           IF  (I-71)
               MOVE 0,00                   TO FGEBYR
               GO TO ENDTIL-T
      *
           END-IF
           SET NOT-I-71                    TO TRUE
           IF  BONGRL < 8001,00
               SET I-71                    TO TRUE
           END-IF
           IF  (I-71)
               DIVIDE BONGRL BY 100    GIVING BEL94
               MULTIPLY 0,5 BY BEL94   GIVING FBONUS ROUNDED
               SET I-82                    TO TRUE
               GO TO ENDTIL-T
      *
           END-IF
           SET NOT-I-71                    TO TRUE
           IF  BONGRL < 12001,00
               SET I-71                    TO TRUE
           END-IF
           IF  (I-71)
               DIVIDE BONGRL BY 100    GIVING BEL94
               MULTIPLY 1,0 BY BEL94   GIVING FBONUS ROUNDED
               SET I-82                    TO TRUE
               GO TO ENDTIL-T
      *
           END-IF
           SET NOT-I-71                    TO TRUE
           IF  BONGRL < 20001,00
               SET I-71                    TO TRUE
           END-IF
           IF  (I-71)
               DIVIDE BONGRL BY 100    GIVING BEL94
               MULTIPLY 2,5 BY BEL94   GIVING FBONUS ROUNDED
               SET I-82                    TO TRUE
               GO TO ENDTIL-T
      *
           END-IF
           SET NOT-I-71                    TO TRUE
           IF  BONGRL < 30001,00
               SET I-71                    TO TRUE
           END-IF
           IF  (I-71)
               DIVIDE BONGRL BY 100    GIVING BEL94
               MULTIPLY 3,5 BY BEL94   GIVING FBONUS ROUNDED
               SET I-82                    TO TRUE
               GO TO ENDTIL-T
      *
           END-IF
           SET NOT-I-71                    TO TRUE
           IF  BONGRL > 30000,99
               SET I-71                    TO TRUE
           END-IF
           IF  (I-71)
               DIVIDE BONGRL BY 100    GIVING BEL94
               MULTIPLY 4,5 BY BEL94   GIVING FBONUS ROUNDED
               SET I-82                    TO TRUE
               GO TO ENDTIL-T
      *
           END-IF
           .
 
       ENDTIL-T.
           CONTINUE.
      *******************************************************
      *****************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE ' '                        TO BBEST
           MOVE 'FAK56'                    TO LONR
           MOVE FIRMNR                     TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'FAK056  '                 TO LPROG
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
           IF  (I-L1 AND I-35)
               SET NOT-I-37                TO TRUE
               SET TAB2KN-S                TO TAB2KN-I
               PERFORM WITH TEST AFTER
                       VARYING TAB2KN-I FROM 1 BY 1
                         UNTIL TAB2KN-I >= TAB2KN-MAX
                            OR I-37
                   IF  RESKNR = TAB2KN (TAB2KN-I)
                       SET I-37            TO TRUE
                       SET TAB2KN-S        TO TAB2KN-I
                   END-IF
               END-PERFORM
           END-IF
           IF  (I-L1 AND I-35 AND I-37)
               SET NOT-I-35                TO TRUE
           END-IF
           IF  (I-L1 AND I-35 AND NOT-I-36)
               SET NOT-I-37                TO TRUE
               SET TABKNR-S                TO TABKNR-I
               PERFORM WITH TEST AFTER
                       VARYING TABKNR-I FROM 1 BY 1
                         UNTIL TABKNR-I >= TABKNR-MAX
                            OR I-37
                   IF  RESKNR = TABKNR (TABKNR-I)
                       SET I-37            TO TRUE
                       SET TABKNR-S        TO TABKNR-I
                   END-IF
               END-PERFORM
           END-IF
           IF  (I-L1 AND I-35 AND NOT-I-36 AND I-37)
               SET NOT-I-35                TO TRUE
           END-IF
           IF  (I-L1 AND I-35)
               ADD 1                       TO ANTGEB
               MOVE 1,00                   TO GEBANT
               MOVE 0                      TO NULL-X
               MOVE 50,00                  TO MGEBYR
               ADD 1 TO SEQNR          GIVING SEQNR2
               ADD 1                       TO ANTF
               ADD MGEBYR                  TO TOTGEB
               ADD 1                       TO ANTBGT
               ADD MGEBYR                  TO BELBGT
      *****************************************************************
      *  TOTALRUTINE FOR FAKTURAGEBYR/FAKTURABONUS.                   *
      *****************************************************************
           END-IF
           IF  (I-L2 AND I-65)
               PERFORM TILRUT-S
           END-IF
           IF  (I-L2 AND I-81)
               ADD 1                       TO ANTGEB
           END-IF
           IF  (I-L2 AND I-22)
               ADD 1                       TO ANTGEB
           END-IF
           IF  (I-L2 AND I-39)
               ADD 1                       TO ANTGEB
           END-IF
           IF  (I-L2 AND I-82)
               ADD 1                       TO ANTBON
           END-IF
           IF  (I-L2 AND I-81)
               MOVE 1,00                   TO GEBANT
           END-IF
           IF  (I-L2 AND I-82)
               MOVE 1,00                   TO BONANT
           END-IF
           IF  (I-L2 AND I-81)
               MOVE 0                      TO NULL-X
           END-IF
           IF  (I-L2 AND I-82)
               MOVE 0                      TO NULL-X
           END-IF
           IF  (I-L2 AND I-81)
               ADD 1 TO SEQNR          GIVING SEQNR2
           END-IF
           IF  (I-L2 AND I-82)
               ADD 1 TO SEQNR          GIVING SEQNR3
           END-IF
           IF  (I-L2 AND I-22)
               ADD 1 TO SEQNR          GIVING SEQNR4
           END-IF
           IF  (I-L2 AND I-81)
               ADD 1                       TO ANTF
           END-IF
           IF  (I-L2 AND I-22)
               ADD 1                       TO ANTF
           END-IF
           IF  (I-L2 AND I-39)
               ADD 1                       TO ANTF
           END-IF
           IF  (I-L2 AND I-82)
               ADD 1                       TO ANTF
           END-IF
           IF  (I-L2 AND I-81)
               ADD FGEBYR                  TO TOTGEB
           END-IF
           IF  (I-L2 AND I-22)
               ADD AGEBYR                  TO TOTGEB
           END-IF
           IF  (I-L2 AND I-39)
               ADD BELABK                  TO TOTGEB
           END-IF
           IF  (I-L2 AND I-82)
               ADD FBONUS                  TO TOTBON
      *****************************************************************
      * GRANTOTAL RUTINE.                                             *
      *****************************************************************
           END-IF
           .
 
       LR-CALCS SECTION.
       LR-CALCS-P.
           MULTIPLY -1 BY TOTBON       GIVING TOTBON
      ******************************************************
      * SUBRUTINE FOR UTREGNING AV BONUS GRUNNLAG FOR      *
      * BEREGNING AV FAKTURAGEBYR/FAKTURABONUS.     .      *
      * REGNES UTIFRA BEST.ANTALL FOR HAFNOR.              *
      ******************************************************
           .
 
       FAKPAR-FLDSET SECTION.
       FAKPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKPAR-IO-AREA (68:1)  TO SFMND (1:1)
           END-EVALUATE.
 
       FAKPAR-IDSET SECTION.
       FAKPAR-IDSET-P.
           SET I-06                        TO TRUE.
 
       FAKTIN-GET SECTION.
       FAKTIN-GET-P.
           IF  FAKTIN-EOF-OFF
               READ FAKTIN
               AT END
                   SET FAKTIN-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FAKTIN-FLDSET SECTION.
       FAKTIN-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKTIN-IO-AREA (1:3)   TO FIRMNR (1:3)
               MOVE FAKTIN-IO-AREA (10:1)  TO FAKTYP (1:1)
               MOVE FAKTIN-IO-AREA (1:200) TO FAKREC (1:200)
               MOVE FAKTIN-IO-AREA (11:1)  TO FAKART (1:1)
               MOVE FAKTIN-IO-AREA (19:6)  TO ORDNR (1:6)
               MOVE FAKTIN-IO-AREA (25:1)  TO RECART (1:1)
               MOVE FAKTIN-IO-AREA (26:4)  TO SEQNR-IO
               INSPECT SEQNR-IO REPLACING ALL ' ' BY '0'
               MOVE FAKTIN-IO-AREA (36:1)  TO BESGEB (1:1)
               MOVE FAKTIN-IO-AREA (62:2)  TO LAGERK (1:2)
               MOVE FAKTIN-IO-AREA (77:1)  TO VGR1F (1:1)
               MOVE FAKTIN-IO-AREA (82:20) TO ARTNR (1:20)
               MOVE FAKTIN-IO-AREA (133:4) TO BESENH-IO
               MOVE FAKTIN-IO-AREA (137:4) TO LEVENH-IO
               MOVE FAKTIN-IO-AREA (141:7) TO EDBNR (1:7)
               MOVE FAKTIN-IO-AREA (141:2) TO EDB2F (1:2)
               MOVE FAKTIN-IO-AREA (141:3) TO EDB3F (1:3)
               MOVE FAKTIN-IO-AREA (148:3) TO RAB1-IO
               INSPECT RAB1-IO REPLACING ALL ' ' BY '0'
               MOVE FAKTIN-IO-AREA (151:3) TO RAB2-IO
               INSPECT RAB2-IO REPLACING ALL ' ' BY '0'
               MOVE FAKTIN-IO-AREA (154:3) TO RAB3-IO
               INSPECT RAB3-IO REPLACING ALL ' ' BY '0'
               MOVE FAKTIN-IO-AREA (157:9) TO ENHPR-IO
               INSPECT ENHPR-IO REPLACING ALL ' ' BY '0'
               MOVE FAKTIN-IO-AREA (177:1) TO RECTYP (1:1)
               MOVE FAKTIN-IO-AREA (184:6) TO RESKNR (1:6)
               MOVE FAKTIN-IO-AREA (190:3) TO ALFAK (1:3)
           END-EVALUATE.
 
       FAKTIN-IDSET SECTION.
       FAKTIN-IDSET-P.
           SET I-03                        TO TRUE.
 
       FAKTIN-CHK-LEVEL SECTION.
       FAKTIN-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO FAKTIN-LEVEL-03
               MOVE FAKTIN-IO-AREA (1:3)   TO FAKTIN-03-L4-FIRMNR
               MOVE FAKTIN-IO-AREA (184:6) TO FAKTIN-03-L3-RESKNR
               MOVE FAKTIN-IO-AREA (10:1)  TO FAKTIN-03-L2-FAKTYP
               MOVE FAKTIN-IO-AREA (19:6)  TO FAKTIN-03-L1-ORDNR
               IF  FAKTIN-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  FAKTIN-03-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   WHEN  FAKTIN-03-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  FAKTIN-03-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  FAKTIN-03-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  FAKTIN-03-L4          TO THE-PRIOR-L4
               MOVE  FAKTIN-03-L3          TO THE-PRIOR-L3
               MOVE  FAKTIN-03-L2          TO THE-PRIOR-L2
               MOVE  FAKTIN-03-L1          TO THE-PRIOR-L1
               SET FAKTIN-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (16:30) TO KNAVN (1:30)
               MOVE KUNDEMA-IO-AREA (127:2) TO BETM (1:2)
               MOVE KUNDEMA-IO-AREA (162:2) TO KKAT-IO
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-04                        TO TRUE.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (127:1) TO MERKN (1:1)
               MOVE VAREMAS-IO-AREA (6:7)  TO EDBNR7 (1:7)
               MOVE VAREMAS-IO-AREA (108:3) TO AKART-IO
               MOVE VAREMAS-IO-AREA (146:1) TO SKART (1:1)
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-05                        TO TRUE.
 
       PRF-PRINT-LINE SECTION.
       PRF-PRINT-LINE-P.
           IF  PRF-BEFORE-SKIP > 0
               PERFORM PRF-SKIP-BEFORE
           END-IF
           IF  PRF-BEFORE-SPACE > 0
               PERFORM PRF-SPACE-BEFORE
               IF  PRF-AFTER-SKIP > 0
                   PERFORM PRF-SKIP-AFTER
               END-IF
               IF  PRF-AFTER-SPACE > 0
                   PERFORM PRF-SPACE-AFTER
               END-IF
           ELSE
               IF  PRF-AFTER-SKIP > 0
                   PERFORM PRF-SKIP-AFTER
               END-IF
               PERFORM PRF-SPACE-AFTER
           END-IF
           IF  PRF-LINE-COUNT NOT < PRF-MAX-LINES
               SET I-OF                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OF            TO TRUE
               END-IF
           END-IF.
 
       PRF-SKIP-BEFORE SECTION.
       PRF-SKIP-BEFORE-P.
           WRITE PRF-IO-PRINT           AFTER ADVANCING PAGE
           MOVE 1                          TO PRF-LINE-COUNT
           MOVE 0                          TO PRF-BEFORE-SKIP
           INITIALIZE PRF-IO-AREA.
 
       PRF-SPACE-BEFORE SECTION.
       PRF-SPACE-BEFORE-P.
           WRITE PRF-IO-PRINT           AFTER PRF-BEFORE-SPACE LINES
           ADD PRF-BEFORE-SPACE            TO PRF-LINE-COUNT
           MOVE SPACES TO PRF-IO-AREA
           INITIALIZE PRF-IO-AREA
           MOVE 0                          TO PRF-BEFORE-SPACE.
 
       PRF-SKIP-AFTER SECTION.
       PRF-SKIP-AFTER-P.
           WRITE PRF-IO-PRINT          BEFORE ADVANCING PAGE
           MOVE 1                          TO PRF-LINE-COUNT
           MOVE 0                          TO PRF-AFTER-SKIP
           INITIALIZE PRF-IO-AREA.
 
       PRF-SPACE-AFTER SECTION.
       PRF-SPACE-AFTER-P.
           WRITE PRF-IO-PRINT          BEFORE PRF-AFTER-SPACE LINES
           ADD PRF-AFTER-SPACE             TO PRF-LINE-COUNT
           INITIALIZE PRF-IO-AREA
           MOVE 0                          TO PRF-AFTER-SPACE.
 
       KNRTAB-LOAD SECTION.
       KNRTAB-LOAD-P.
           OPEN INPUT KNRTAB
           SET TABKNR-I                    TO 1
           PERFORM UNTIL KNRTAB-EOF
               READ KNRTAB
               AT END
                   SET KNRTAB-EOF          TO TRUE
               NOT AT END
                   MOVE KNRTAB-IO-AREA (1:6) TO TABKNR-ENTRY (TABKNR-I)
                   SET TABKNR-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE KNRTAB.
 
       KNTAB2-LOAD SECTION.
       KNTAB2-LOAD-P.
           OPEN INPUT KNTAB2
           SET TAB2KN-I                    TO 1
           PERFORM UNTIL KNTAB2-EOF
               READ KNTAB2
               AT END
                   SET KNTAB2-EOF          TO TRUE
               NOT AT END
                   MOVE KNTAB2-IO-AREA (1:6) TO TAB2KN-ENTRY (TAB2KN-I)
                   SET TAB2KN-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE KNTAB2.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-03 AND NOT-I-10)
               MOVE SPACES TO FAKTUT-IO-AREA
               INITIALIZE FAKTUT-IO-AREA
               MOVE FAKREC                 TO FAKTUT-IO-AREA (1:200)
      * GEBYR FOR MANUELL BESTILLING.
               WRITE FAKTUT-IO-AREA
           END-IF
           IF  (I-03 AND I-38 AND NOT-I-86)
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE RESKNR                 TO PRF-IO-AREA (2:6)
               MOVE KNAVN                  TO PRF-IO-AREA (9:30)
               MOVE 'ORDRE:'               TO PRF-IO-AREA (40:6)
               MOVE ORDNR                  TO PRF-IO-AREA (46:6)
               MOVE 'VARE:'                TO PRF-IO-AREA (53:5)
      *                        ALFAK     60
               MOVE ARTNR                  TO PRF-IO-AREA (58:20)
               MOVE 'ANT.KART:'            TO PRF-IO-AREA (79:9)
               MOVE AKART                  TO XO-50YN9
               MOVE XO-50YN9               TO PRF-IO-AREA (89:5)
               MOVE 'ANT.BEST:'            TO PRF-IO-AREA (95:9)
               MOVE BESENH                 TO XO-52YN9
               MOVE XO-52YN9               TO PRF-IO-AREA (105:8)
               MOVE 'ANBREKKSGEBYR'        TO PRF-IO-AREA (114:13)
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L4 AND I-23 AND NOT-I-86)
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE 'HAFNOR A/S              ' TO PRF-IO-AREA (1:24)
               MOVE 'KONTROLLISTE FAKTURA GEB' TO PRF-IO-AREA (25:24)
               MOVE 'YR HYDROTEXACO.         ' TO PRF-IO-AREA (49:24)
               MOVE 'FREMSTILT'            TO PRF-IO-AREA (73:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO PRF-IO-AREA (83:8)
               MOVE 01                     TO PRF-BEFORE-SKIP
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE 'KUNDENR'              TO PRF-IO-AREA (1:7)
               MOVE 'KUNDE NAVN'           TO PRF-IO-AREA (9:10)
               MOVE 'KAT'                  TO PRF-IO-AREA (40:3)
               MOVE 'BM'                   TO PRF-IO-AREA (44:2)
               MOVE 'GEBYR GRL.'           TO PRF-IO-AREA (49:10)
               MOVE 'BEREGNET'             TO PRF-IO-AREA (61:8)
               MOVE 'BELØP'                TO PRF-IO-AREA (81:5)
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE 'HAFNOR A/S              ' TO PRF-IO-AREA (1:24)
               MOVE 'KONTROLLISTE FAKTURA GEB' TO PRF-IO-AREA (25:24)
               MOVE 'YR HYDROTEXACO.         ' TO PRF-IO-AREA (49:24)
               MOVE 'FREMSTILT'            TO PRF-IO-AREA (73:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO PRF-IO-AREA (83:8)
               MOVE 01                     TO PRF-BEFORE-SKIP
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE 'KUNDENR'              TO PRF-IO-AREA (1:7)
               MOVE 'KUNDE NAVN'           TO PRF-IO-AREA (9:10)
               MOVE 'KAT'                  TO PRF-IO-AREA (40:3)
               MOVE 'BM'                   TO PRF-IO-AREA (44:2)
               MOVE 'GEBYR GRL.'           TO PRF-IO-AREA (49:10)
               MOVE 'BEREGNET'             TO PRF-IO-AREA (61:8)
               MOVE 'BELØP'                TO PRF-IO-AREA (81:5)
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-35 AND NOT-I-U1)
               MOVE SPACES TO FAKTUT-IO-AREA
               INITIALIZE FAKTUT-IO-AREA
               MOVE FAKREC                 TO FAKTUT-IO-AREA (1:200)
               MOVE FAKART                 TO FAKTUT-IO-AREA (11:1)
               MOVE 'L'                    TO FAKTUT-IO-AREA (25:1)
               MOVE SEQNR2-IO              TO FAKTUT-IO-AREA (26:4)
               MOVE '19965'                TO FAKTUT-IO-AREA (77:5)
               MOVE ' ORDREGEBYR         ' TO FAKTUT-IO-AREA (82:20)
               MOVE 'MANUELL BESTILLING  ' TO FAKTUT-IO-AREA (102:20)
               MOVE '          '           TO FAKTUT-IO-AREA (122:10)
               MOVE GEBANT                 TO XO-52P
               MOVE XO-52P-EF              TO FAKTUT-IO-AREA (133:4)
               MOVE GEBANT                 TO XO-52P
               MOVE XO-52P-EF              TO FAKTUT-IO-AREA (137:4)
               MOVE '9001883'              TO FAKTUT-IO-AREA (141:7)
               MOVE '000'                  TO FAKTUT-IO-AREA (148:3)
               MOVE '000'                  TO FAKTUT-IO-AREA (151:3)
               MOVE '000'                  TO FAKTUT-IO-AREA (154:3)
               MOVE MGEBYR-IO              TO FAKTUT-IO-AREA (157:9)
               MOVE NULL-X                 TO XO-72P
               MOVE XO-72P-EF              TO FAKTUT-IO-AREA (171:5)
               MOVE 'F'                    TO FAKTUT-IO-AREA (177:1)
               MOVE 'GEB'                  TO FAKTUT-IO-AREA (190:3)
      * PAKNINGS ANBREKK RECORDS
               WRITE FAKTUT-IO-AREA
           END-IF
           IF  (I-L2 AND I-39 AND NOT-I-U1)
               MOVE SPACES TO FAKTUT-IO-AREA
               INITIALIZE FAKTUT-IO-AREA
               MOVE FAKREC                 TO FAKTUT-IO-AREA (1:200)
               MOVE FAKART                 TO FAKTUT-IO-AREA (11:1)
               MOVE 'L'                    TO FAKTUT-IO-AREA (25:1)
               MOVE SEQNR2-IO              TO FAKTUT-IO-AREA (26:4)
               MOVE '19962'                TO FAKTUT-IO-AREA (77:5)
               MOVE ' ANBREKKSGEBYR      ' TO FAKTUT-IO-AREA (82:20)
               MOVE 'SPLITTING AV HELE KA' TO FAKTUT-IO-AREA (102:20)
               MOVE 'RTONGER   '           TO FAKTUT-IO-AREA (122:10)
               MOVE ANTABF                 TO XO-52P
               MOVE XO-52P-EF              TO FAKTUT-IO-AREA (133:4)
               MOVE ANTABF                 TO XO-52P
               MOVE XO-52P-EF              TO FAKTUT-IO-AREA (137:4)
               MOVE '9001840'              TO FAKTUT-IO-AREA (141:7)
               MOVE '000'                  TO FAKTUT-IO-AREA (148:3)
               MOVE '000'                  TO FAKTUT-IO-AREA (151:3)
               MOVE '000'                  TO FAKTUT-IO-AREA (154:3)
               MOVE BGEBYR-IO              TO FAKTUT-IO-AREA (157:9)
               MOVE NULL-X                 TO XO-72P
               MOVE XO-72P-EF              TO FAKTUT-IO-AREA (171:5)
               MOVE 'F'                    TO FAKTUT-IO-AREA (177:1)
               MOVE 'GEB'                  TO FAKTUT-IO-AREA (190:3)
      * FAKTURAGEBYR RECORDS.
               WRITE FAKTUT-IO-AREA
           END-IF
           IF  (I-L2 AND I-81 AND NOT-I-U1)
               MOVE SPACES TO FAKTUT-IO-AREA
               INITIALIZE FAKTUT-IO-AREA
               MOVE FAKREC                 TO FAKTUT-IO-AREA (1:200)
               MOVE FAKART                 TO FAKTUT-IO-AREA (11:1)
               MOVE 'L'                    TO FAKTUT-IO-AREA (25:1)
               MOVE SEQNR2-IO              TO FAKTUT-IO-AREA (26:4)
               MOVE '19963'                TO FAKTUT-IO-AREA (77:5)
               MOVE ' FAKTURAGEBYR       ' TO FAKTUT-IO-AREA (82:20)
               MOVE 'FAKTURABELØP UNDER: ' TO FAKTUT-IO-AREA (102:20)
               IF  (I-83)
                   MOVE 'KR.2000,- '       TO FAKTUT-IO-AREA (122:10)
               END-IF
               IF  (I-84)
                   MOVE 'KR.3000,- '       TO FAKTUT-IO-AREA (122:10)
               END-IF
               MOVE GEBANT                 TO XO-52P
               MOVE XO-52P-EF              TO FAKTUT-IO-AREA (133:4)
               MOVE GEBANT                 TO XO-52P
               MOVE XO-52P-EF              TO FAKTUT-IO-AREA (137:4)
               MOVE '9001867'              TO FAKTUT-IO-AREA (141:7)
               MOVE '000'                  TO FAKTUT-IO-AREA (148:3)
               MOVE '000'                  TO FAKTUT-IO-AREA (151:3)
               MOVE '000'                  TO FAKTUT-IO-AREA (154:3)
               MOVE FGEBYR-IO              TO FAKTUT-IO-AREA (157:9)
               MOVE NULL-X                 TO XO-72P
               MOVE XO-72P-EF              TO FAKTUT-IO-AREA (171:5)
               MOVE 'F'                    TO FAKTUT-IO-AREA (177:1)
               MOVE 'GEB'                  TO FAKTUT-IO-AREA (190:3)
      * AUTOGIROGEBYR RECORDS.
               WRITE FAKTUT-IO-AREA
           END-IF
           IF  (I-L2 AND I-22 AND NOT-I-U1)
               MOVE SPACES TO FAKTUT-IO-AREA
               INITIALIZE FAKTUT-IO-AREA
               MOVE FAKREC                 TO FAKTUT-IO-AREA (1:200)
               MOVE FAKART                 TO FAKTUT-IO-AREA (11:1)
               MOVE 'L'                    TO FAKTUT-IO-AREA (25:1)
               MOVE SEQNR4-IO              TO FAKTUT-IO-AREA (26:4)
               MOVE '19964'                TO FAKTUT-IO-AREA (77:5)
               MOVE ' FAKTURAGEBYR       ' TO FAKTUT-IO-AREA (82:20)
               MOVE 'BENYTTER IKKE AUTOGI' TO FAKTUT-IO-AREA (102:20)
               MOVE 'RO        '           TO FAKTUT-IO-AREA (122:10)
               MOVE GEBANT                 TO XO-52P
               MOVE XO-52P-EF              TO FAKTUT-IO-AREA (133:4)
               MOVE GEBANT                 TO XO-52P
               MOVE XO-52P-EF              TO FAKTUT-IO-AREA (137:4)
               MOVE '9001875'              TO FAKTUT-IO-AREA (141:7)
               MOVE '000'                  TO FAKTUT-IO-AREA (148:3)
               MOVE '000'                  TO FAKTUT-IO-AREA (151:3)
               MOVE '000'                  TO FAKTUT-IO-AREA (154:3)
               MOVE AGEBYR-IO              TO FAKTUT-IO-AREA (157:9)
               MOVE NULL-X                 TO XO-72P
               MOVE XO-72P-EF              TO FAKTUT-IO-AREA (171:5)
               MOVE 'F'                    TO FAKTUT-IO-AREA (177:1)
               MOVE 'GEB'                  TO FAKTUT-IO-AREA (190:3)
      * FAKTURABONUS RECORDS.
               WRITE FAKTUT-IO-AREA
           END-IF
           IF  (I-L2 AND I-82 AND NOT-I-U1)
               MOVE SPACES TO FAKTUT-IO-AREA
               INITIALIZE FAKTUT-IO-AREA
               MOVE FAKREC                 TO FAKTUT-IO-AREA (1:200)
               MOVE FAKART                 TO FAKTUT-IO-AREA (11:1)
               MOVE 'L'                    TO FAKTUT-IO-AREA (25:1)
               MOVE SEQNR3-IO              TO FAKTUT-IO-AREA (26:4)
               MOVE '90020'                TO FAKTUT-IO-AREA (77:5)
               MOVE ' 3                  ' TO FAKTUT-IO-AREA (82:20)
               MOVE 'ORDRE-RABATT ESSO   ' TO FAKTUT-IO-AREA (102:20)
               MOVE '          '           TO FAKTUT-IO-AREA (122:10)
               MOVE BONANT                 TO XO-52P
               MOVE XO-52P-EF              TO FAKTUT-IO-AREA (133:4)
               MOVE BONANT                 TO XO-52P
               MOVE XO-52P-EF              TO FAKTUT-IO-AREA (137:4)
               MOVE '9950028'              TO FAKTUT-IO-AREA (141:7)
               MOVE '000'                  TO FAKTUT-IO-AREA (148:3)
               MOVE '000'                  TO FAKTUT-IO-AREA (151:3)
               MOVE '000'                  TO FAKTUT-IO-AREA (154:3)
               MOVE FBONUS-IO              TO FAKTUT-IO-AREA (157:9)
               IF  (I-28)
                   MOVE '4'                TO FAKTUT-IO-AREA (166:1)
               END-IF
               MOVE NULL-X                 TO XO-72P
               MOVE XO-72P-EF              TO FAKTUT-IO-AREA (171:5)
               MOVE 'B'                    TO FAKTUT-IO-AREA (177:1)
               MOVE 'GEB'                  TO FAKTUT-IO-AREA (190:3)
               WRITE FAKTUT-IO-AREA
           END-IF
           IF  (I-L1 AND I-35 AND NOT-I-86)
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE RESKNR                 TO PRF-IO-AREA (2:6)
               MOVE KNAVN                  TO PRF-IO-AREA (9:30)
               MOVE KKAT                   TO XO-30U
               MOVE XO-30U (1:3)           TO PRF-IO-AREA (40:3)
               MOVE BETM                   TO PRF-IO-AREA (44:2)
               MOVE 'ORDRE:'               TO PRF-IO-AREA (48:6)
               MOVE ORDNR                  TO PRF-IO-AREA (54:6)
               MOVE 'BEST. GEBYR  '        TO PRF-IO-AREA (61:13)
               MOVE MGEBYR                 TO XO-72YY9R
               MOVE XO-72YY9R              TO PRF-IO-AREA (74:13)
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
           END-IF
           IF  (I-L2 AND I-39 AND NOT-I-86)
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE RESKNR                 TO PRF-IO-AREA (2:6)
               MOVE KNAVN                  TO PRF-IO-AREA (9:30)
               IF KKAT < 0
                 MOVE KKAT                 TO XO-30D
                 MOVE XO-30D (1:3)         TO PRF-IO-AREA (40:3)
               ELSE
                 MOVE KKAT                 TO XO-30U
                 MOVE XO-30U (1:3)         TO PRF-IO-AREA (40:3)
               END-IF
               MOVE BETM                   TO PRF-IO-AREA (44:2)
               MOVE FAKTYP                 TO PRF-IO-AREA (47:1)
               MOVE ANTABX                 TO XO-50YN9
               MOVE XO-50YN9               TO PRF-IO-AREA (51:5)
               MOVE 'STK'                  TO PRF-IO-AREA (57:3)
               MOVE 'ANBREKKSGEBYR'        TO PRF-IO-AREA (61:13)
               MOVE BELABK                 TO XO-72YY9R
               MOVE XO-72YY9R              TO PRF-IO-AREA (74:13)
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
           END-IF
           IF  (I-L2 AND I-81 AND NOT-I-86)
           OR  (I-L2 AND I-82 AND NOT-I-86)
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE RESKNR                 TO PRF-IO-AREA (2:6)
               MOVE KNAVN                  TO PRF-IO-AREA (9:30)
               IF KKAT < 0
                 MOVE KKAT                 TO XO-30D
                 MOVE XO-30D (1:3)         TO PRF-IO-AREA (40:3)
               ELSE
                 MOVE KKAT                 TO XO-30U
                 MOVE XO-30U (1:3)         TO PRF-IO-AREA (40:3)
               END-IF
               MOVE BETM                   TO PRF-IO-AREA (44:2)
               MOVE GEBGRL                 TO XO-72YY9R
               MOVE XO-72YY9R              TO PRF-IO-AREA (47:13)
               MOVE FAKTYP                 TO PRF-IO-AREA (47:1)
               IF  (I-81)
                   MOVE 'FAKTURAGEBYR'     TO PRF-IO-AREA (61:12)
      *                      82          72 "FAKTURABONUS"
               END-IF
               IF  (I-81)
                   MOVE FGEBYR             TO XO-72YY9R
                   MOVE XO-72YY9R          TO PRF-IO-AREA (74:13)
      *                      82FBONUSJ   86
               END-IF
               IF  (I-31)
                   MOVE 'GEBYRFRI ORDRE'   TO PRF-IO-AREA (88:14)
               END-IF
               IF  (I-32)
                   MOVE 'BONUSFRI VARE'    TO PRF-IO-AREA (103:13)
               END-IF
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
           END-IF
           IF  (I-L2 AND I-22 AND NOT-I-86)
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE RESKNR                 TO PRF-IO-AREA (2:6)
               MOVE KNAVN                  TO PRF-IO-AREA (9:30)
               IF KKAT < 0
                 MOVE KKAT                 TO XO-30D
                 MOVE XO-30D (1:3)         TO PRF-IO-AREA (40:3)
               ELSE
                 MOVE KKAT                 TO XO-30U
                 MOVE XO-30U (1:3)         TO PRF-IO-AREA (40:3)
               END-IF
               MOVE BETM                   TO PRF-IO-AREA (44:2)
               MOVE FAKTYP                 TO PRF-IO-AREA (47:1)
      *                        GEBGRLJ   59
               MOVE 'AUTOGIROGEBYR'        TO PRF-IO-AREA (60:13)
               MOVE AGEBYR                 TO XO-72YY9R
               MOVE XO-72YY9R              TO PRF-IO-AREA (74:13)
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
           END-IF
           IF  (I-L4 AND I-23 AND NOT-I-86)
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE 'ANTALL FAKTURAGEBYR'  TO PRF-IO-AREA (9:19)
               MOVE ANTGEB                 TO XO-50YY9
               MOVE XO-50YY9               TO PRF-IO-AREA (29:6)
               MOVE 'TOTAL FAKTURAGEBYR'   TO PRF-IO-AREA (37:18)
               MOVE TOTGEB                 TO XO-72YY9R
               MOVE XO-72YY9R              TO PRF-IO-AREA (56:13)
               MOVE 'SISTE FAKT. I MND. =' TO PRF-IO-AREA (70:20)
               IF  (I-87)
                   MOVE 'JA'               TO PRF-IO-AREA (90:2)
               END-IF
               IF  (NOT-I-87)
                   MOVE 'NEI'              TO PRF-IO-AREA (90:3)
               END-IF
               MOVE 1                      TO PRF-BEFORE-SPACE
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE 'HERAV ANT. ANBREKK '  TO PRF-IO-AREA (9:19)
               MOVE ANTABT                 TO XO-50YY9
               MOVE XO-50YY9               TO PRF-IO-AREA (29:6)
               MOVE 'HERAV BEL. ANBREKK'   TO PRF-IO-AREA (37:18)
               MOVE BELABT                 TO XO-72YY9R
               MOVE XO-72YY9R              TO PRF-IO-AREA (56:13)
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE 'HERAV ANT. BEST.GEB'  TO PRF-IO-AREA (9:19)
               MOVE ANTBGT                 TO XO-50YY9
               MOVE XO-50YY9               TO PRF-IO-AREA (29:6)
               MOVE 'HERAV BEL. BEST.GEB'  TO PRF-IO-AREA (37:19)
               MOVE BELBGT                 TO XO-72YY9R
               MOVE XO-72YY9R              TO PRF-IO-AREA (56:13)
      *       T  1     L4 23N86
      *                                  27 "ANTALL FAKTURABONUS"
      *                        ANTBON1   34
      *                                  54 "TOTAL FAKTURABONUS"
      *                        TOTBONJ   68
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
           END-IF
           IF  (I-L4 AND I-23 AND NOT-I-86)
           AND (I-U1)
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE '*** TESTKJØRING. GEB' TO PRF-IO-AREA (9:20)
               MOVE 'YR BLIR IKKE FAKTURE' TO PRF-IO-AREA (29:20)
               MOVE 'RERT. ***           ' TO PRF-IO-AREA (49:20)
               MOVE 1                      TO PRF-BEFORE-SPACE
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE '************************' TO PRF-IO-AREA (1:24)
               MOVE '************************' TO PRF-IO-AREA (25:24)
               MOVE 01                     TO PRF-BEFORE-SKIP
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE '***   AVSTEMNINGSTOTALER' TO PRF-IO-AREA (1:24)
               MOVE '    --- FAK056 ---   ***' TO PRF-IO-AREA (25:24)
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE '************************' TO PRF-IO-AREA (1:24)
               MOVE '************************' TO PRF-IO-AREA (25:24)
               MOVE 3                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE ANTINN                 TO XO-70YY9
               MOVE XO-70YY9               TO PRF-IO-AREA (4:9)
               MOVE 'FAKT. REC. LEST INN.    ' TO PRF-IO-AREA (14:24)
               MOVE 1                      TO PRF-BEFORE-SPACE
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE ANTLA0                 TO XO-50YY9
               MOVE XO-50YY9               TO PRF-IO-AREA (7:6)
               MOVE 'FAKT. REC. FJERNET.'  TO PRF-IO-AREA (14:19)
               MOVE 'VARELINJER M/0 I ANT.LEV' TO PRF-IO-AREA (34:24)
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               IF  (NOT-I-U1)
                   MOVE ANTGEB             TO XO-50YY9
                   MOVE XO-50YY9           TO PRF-IO-AREA (7:6)
               END-IF
               IF  (I-U1)
                   MOVE '0'                TO PRF-IO-AREA (12:1)
               END-IF
               MOVE 'FAKT.GEBYR RECORDS'   TO PRF-IO-AREA (14:18)
               IF  (NOT-I-U1)
                   MOVE TOTGEB             TO XO-72YY9R
                   MOVE XO-72YY9R          TO PRF-IO-AREA (38:13)
               END-IF
               IF  (I-U1)
                   MOVE '0,00 '            TO PRF-IO-AREA (46:5)
               END-IF
               MOVE 'FAKT.GEBYR BELØP. '   TO PRF-IO-AREA (53:18)
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               IF  (NOT-I-U1)
                   MOVE ANTBON             TO XO-50YY9
                   MOVE XO-50YY9           TO PRF-IO-AREA (7:6)
               END-IF
               IF  (I-U1)
                   MOVE '0'                TO PRF-IO-AREA (12:1)
               END-IF
               MOVE 'FAKT.BONUS RECORDS'   TO PRF-IO-AREA (14:18)
               IF  (NOT-I-U1)
                   MOVE TOTBON             TO XO-72YY9R
                   MOVE XO-72YY9R          TO PRF-IO-AREA (38:13)
               END-IF
               IF  (I-U1)
                   MOVE '0,00 '            TO PRF-IO-AREA (46:5)
               END-IF
               MOVE 'FAKT.BONUS BELØP. '   TO PRF-IO-AREA (53:18)
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               IF  (NOT-I-U1)
                   MOVE ANTF               TO XO-70YY9
                   MOVE XO-70YY9           TO PRF-IO-AREA (4:9)
               END-IF
               IF  (I-U1)
                   MOVE '0'                TO PRF-IO-AREA (12:1)
               END-IF
               MOVE 'FAKTURA GEBYR/BONUS RECO' TO PRF-IO-AREA (14:24)
               MOVE 'RDS DANNET.'          TO PRF-IO-AREA (38:11)
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE '       KJØRT'         TO PRF-IO-AREA (7:12)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO PRF-IO-AREA (20:8)
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
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
           PERFORM KNRTAB-LOAD
           PERFORM KNTAB2-LOAD
           INITIALIZE FAKPAR-DATA-FIELDS
           OPEN INPUT FAKPAR
           SET FAKTIN-LEVEL-INIT           TO TRUE
           INITIALIZE FAKTIN-DATA-FIELDS
           SET FAKTIN-EOF-OFF              TO TRUE
           SET FAKTIN-PROCESS              TO TRUE
           OPEN INPUT FAKTIN
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           INITIALIZE VAREMAS-DATA-FIELDS
           OPEN INPUT VAREMAS
           OPEN OUTPUT FAKTUT
           OPEN OUTPUT PRF
           INITIALIZE PRF-IO-AREA
           INITIALIZE PRF-DATA-FIELDS
           MOVE 57                         TO PRF-MAX-LINES.
           SET TABKNR-I                    TO 1
           SET TAB2KN-I                    TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FAKPAR
           CLOSE FAKTIN
           CLOSE KUNDEMA
           CLOSE VAREMAS
           CLOSE FAKTUT
           IF PRF-IO-AREA NOT = SPACES
             WRITE PRF-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO PRF-IO-AREA
           END-IF
           CLOSE PRF.
 
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
