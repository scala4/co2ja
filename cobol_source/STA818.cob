       IDENTIFICATION DIVISION.
       PROGRAM-ID. STA818R.
      **********************************************  Z-WIN-RPG2   ****
      **OBS ved endring excel på Report Web *****************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: STA818.rpg
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
           SELECT SALGF
               ASSIGN TO UT-S-SALGF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS SALGF-STATUS.
           SELECT VAGRMAS
               ASSIGN TO VAGRMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAGRMAS-STATUS
               RECORD KEY IS VAGRMAS-KEY1.
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
       FD PARAM
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  PARAM-IO-AREA.
           05  PARAM-IO-AREA-X             PICTURE X(80).
       FD SALGF
               BLOCK CONTAINS 340
               RECORD CONTAINS 170.
       01  SALGF-IO-AREA.
           05  SALGF-IO-AREA-X             PICTURE X(170).
       FD VAGRMAS
               RECORD CONTAINS 80.
       01  VAGRMAS-IO-AREA.
           05  VAGRMAS-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  VAGRMAS-KEY1            PICTURE X(8).
               10  FILLER                  PICTURE X(71).
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1            PICTURE X(9).
               10  FILLER                  PICTURE X(190).
      **************************************************************
      *  OBS. LIKT STA800 MEN HAR MED BETALINGSKODE OG
      *  OBS. RABATTGRUPPER PÅ KUNDEN                               *
      **************************************************************
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
           10  SALGF-STATUS                PICTURE 99 VALUE 0.
           10  VAGRMAS-STATUS              PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  BMFELT-XX-STATUS            PICTURE 99 VALUE 0.
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
               88  SALGF-EOF-OFF           VALUE '0'.
               88  SALGF-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SALGF-READ-OFF          VALUE '0'.
               88  SALGF-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SALGF-PROCESS-OFF       VALUE '0'.
               88  SALGF-PROCESS           VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  SALGF-LEVEL-INIT-OFF    VALUE '0'.
               88  SALGF-LEVEL-INIT        VALUE '1'.
           05  VAGRMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  BMFELT-XX-DATA-FIELDS.
               10  BETMAT                  PICTURE X(2).
               10  FILLER                  PICTURE X(255).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(2).
               10  BMTKST                  PICTURE X(24).
               10  FILLER                  PICTURE X(231).
           05  LDATA-XX REDEFINES BMFELT-XX-DATA-FIELDS.
               10  LONR                    PICTURE X(5).
               10  FILLER                  PICTURE X(252).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(5).
               10  LFIRMA                  PICTURE X(3).
               10  FILLER                  PICTURE X(249).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(8).
               10  LUNDGR                  PICTURE X(3).
               10  FILLER                  PICTURE X(246).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(11).
               10  LPROG                   PICTURE X(8).
               10  FILLER                  PICTURE X(238).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(19).
               10  LANTX-IO.
                   15  LANTX               PICTURE S9(3).
               10  FILLER                  PICTURE X(235).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(22).
               10  FINAVN                  PICTURE X(30).
               10  FILLER                  PICTURE X(205).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(52).
               10  LOPNVN                  PICTURE X(35).
               10  FILLER                  PICTURE X(170).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(87).
               10  LPRIID                  PICTURE X(4).
               10  FILLER                  PICTURE X(166).
      *     *  BESTILLINGSOPPGAVER (OVERSTYRING AV RBS-FILE) *    *
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(91).
               10  BJOBN                   PICTURE X(8).
               10  FILLER                  PICTURE X(158).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(99).
               10  BBEST                   PICTURE X(1).
               10  FILLER                  PICTURE X(157).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(100).
               10  BPERS                   PICTURE X(30).
               10  FILLER                  PICTURE X(127).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(130).
               10  BETTB                   PICTURE X(40).
               10  FILLER                  PICTURE X(87).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(170).
               10  BFORS                   PICTURE X(40).
               10  FILLER                  PICTURE X(47).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(210).
               10  BMEMO                   PICTURE X(40).
               10  FILLER                  PICTURE X(7).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(250).
               10  BANTX-IO.
                   15  BANTX               PICTURE S9(3).
               10  FILLER                  PICTURE X(4).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(253).
               10  BPCLAS                  PICTURE X(1).
               10  FILLER                  PICTURE X(3).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(254).
               10  BPRJE                   PICTURE X(3).
      * * END - RBS - DATASTRUKTUR FOR SUB-PROGRAM RBSH01 ********
      * * START RBS - PARAMETERKORT OVERSTYRING AV RBS-FILE  * *
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
      * * END   RBS - PARAMETERKORT OVERSTYRING AV RBS-FILE  * *
               10  PAAR-IO.
                   15  PAAR                PICTURE S9(2).
               10  PMND                    PICTURE X(2).
               10  A-IO.
                   15  A                   PICTURE S9(2).
           05  SALGF-LEVEL-01.
               10  SALGF-01-L5.
                   15  SALGF-01-L5-FIRMA   PICTURE X(3).
               10  SALGF-01-L4.
                   15  SALGF-01-L4-SELGN   PICTURE X(20).
               10  SALGF-01-L2.
                   15  SALGF-01-L2-RESNR   PICTURE X(6).
               10  SALGF-01-L1.
                   15  SALGF-01-L1-VGR     PICTURE X(5).
           05  SALGF-DATA-FIELDS.
               10  SVS-IO.
                   15  SVS                 PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  BEST-IO.
                   15  BEST                PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  EDB1                    PICTURE X(1).
               10  RABA-IO.
                   15  RABA                PICTURE S9(2)V9(1).
               10  RABB-IO.
                   15  RABB                PICTURE S9(2)V9(1).
               10  RABC-IO.
                   15  RABC                PICTURE S9(2)V9(1).
               10  FAKR                    PICTURE X(1).
               10  ENHPR-IO.
                   15  ENHPR               PICTURE S9(7)V9(2).
               10  MND                     PICTURE X(2).
               10  BK                      PICTURE X(1).
               10  RESNR                   PICTURE X(6).
               10  FIRMA                   PICTURE X(3).
               10  HND                     PICTURE X(3).
               10  AAR-IO.
                   15  AAR                 PICTURE S9(2).
               10  VGR                     PICTURE X(5).
               10  KREDK                   PICTURE X(1).
               10  ORDNR-IO.
                   15  ORDNR               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  SNR                     PICTURE X(3).
               10  SELG1                   PICTURE X(1).
               10  SELGN                   PICTURE X(20).
      *                                     131 160 NAVN
           05  VAGRMAS-DATA-FIELDS.
               10  FPROS-IO.
                   15  FPROS               PICTURE S9(3)V9(1).
               10  KONTO                   PICTURE X(4).
           05  KUNDEMA-DATA-FIELDS.
               10  NAVN                    PICTURE X(30).
               10  BETM                    PICTURE X(2).
               10  RABK                    PICTURE X(9).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L5            PICTURE X(3).
               10  THE-PRIOR-L4            PICTURE X(20).
               10  THE-PRIOR-L2            PICTURE X(6).
               10  THE-PRIOR-L1            PICTURE X(5).
           05  TEMPORARY-FIELDS.
               10  PAAR1-IO.
                   15  PAAR1               PICTURE S9(2).
               10  DMNDF-IO.
                   15  DMNDF               PICTURE S9(9)V9(2).
               10  DMND-IO.
                   15  DMND                PICTURE S9(9)V9(2).
               10  TOTF-IO.
                   15  TOTF                PICTURE S9(9)V9(2).
               10  TOT-IO.
                   15  TOT                 PICTURE S9(9)V9(2).
               10  SVSUT-IO.
                   15  SVSUT               PICTURE S9(7)V9(2).
               10  KEY9                    PICTURE X(9).
               10  KEY-X                   PICTURE X(8).
               10  VERDI-IO.
                   15  VERDI               PICTURE S9(8)V9(2).
               10  SUM-X-IO.
                   15  SUM-X               PICTURE S9(9)V9(2).
               10  NYPR-IO.
                   15  NYPR                PICTURE S9(7)V9(2).
               10  PROS-IO.
                   15  PROS                PICTURE S9(3)V9(1).
               10  BSVS-IO.
                   15  BSVS                PICTURE S9(11).
               10  MTSVS-IO.
                   15  MTSVS               PICTURE S9(9)V9(2).
               10  SSVS-IO.
                   15  SSVS                PICTURE S9(9)V9(2).
               10  TMSVS-IO.
                   15  TMSVS               PICTURE S9(9)V9(2).
               10  TSVS-IO.
                   15  TSVS                PICTURE S9(9)V9(2).
               10  STSVS-IO.
                   15  STSVS               PICTURE S9(9)V9(2).
               10  GTSVS-IO.
                   15  GTSVS               PICTURE S9(9)V9(2).
               10  L2MNDF-IO.
                   15  L2MNDF              PICTURE S9(10)V9(2).
               10  L2TOTF-IO.
                   15  L2TOTF              PICTURE S9(10)V9(2).
               10  L2MND-IO.
                   15  L2MND               PICTURE S9(10)V9(2).
               10  L2TOT-IO.
                   15  L2TOT               PICTURE S9(10)V9(2).
               10  L4MND-IO.
                   15  L4MND               PICTURE S9(10)V9(2).
               10  L4TOT-IO.
                   15  L4TOT               PICTURE S9(10)V9(2).
               10  L5MND-IO.
                   15  L5MND               PICTURE S9(10)V9(2).
               10  L5TOT-IO.
                   15  L5TOT               PICTURE S9(10)V9(2).
               10  L4MNDF-IO.
                   15  L4MNDF              PICTURE S9(10)V9(2).
               10  L4TOTF-IO.
                   15  L4TOTF              PICTURE S9(10)V9(2).
               10  L5MNDF-IO.
                   15  L5MNDF              PICTURE S9(10)V9(2).
               10  L5TOTF-IO.
                   15  L5TOTF              PICTURE S9(10)V9(2).
               10  L2BMA-ELGR-IO.
                   15  L2BMA-ELGR          PICTURE S9(6)V9(4).
               10  L2BM-IO.
                   15  L2BM                PICTURE S9(3)V9(1).
               10  L2BRM-IO.
                   15  L2BRM               PICTURE S9(3)V9(1).
               10  L2BIA-ELGR-IO.
                   15  L2BIA-ELGR          PICTURE S9(4)V9(4).
               10  L2BRF-IO.
                   15  L2BRF               PICTURE S9(3)V9(1).
               10  L2BRU-IO.
                   15  L2BRU               PICTURE S9(3)V9(1).
               10  L4BIA-ELGR-IO.
                   15  L4BIA-ELGR          PICTURE S9(4)V9(4).
               10  L4BM-IO.
                   15  L4BM                PICTURE S9(4)V9(4).
               10  L4BRF-IO.
                   15  L4BRF               PICTURE S9(3)V9(1).
               10  L4BRM-IO.
                   15  L4BRM               PICTURE S9(3)V9(1).
               10  L4BRU-IO.
                   15  L4BRU               PICTURE S9(3)V9(1).
               10  L4BMND-IO.
                   15  L4BMND              PICTURE S9(3)V9(1).
               10  L5BIA-ELGR-IO.
                   15  L5BIA-ELGR          PICTURE S9(4)V9(4).
               10  L5BM-IO.
                   15  L5BM                PICTURE S9(4)V9(4).
               10  L5BRF-IO.
                   15  L5BRF               PICTURE S9(3)V9(1).
               10  L5BRM-IO.
                   15  L5BRM               PICTURE S9(3)V9(1).
               10  L5BRU-IO.
                   15  L5BRU               PICTURE S9(3)V9(1).
               10  L5BMND-IO.
                   15  L5BMND              PICTURE S9(3)V9(1).
           05  EDITTING-FIELDS.
               10  XO-40YNZ                PICTURE ZZZZ.
               10  XO-102YY9R              PICTURE Z.ZZZ.ZZZ.ZZZ,99-.
               10  XO-31YY9R               PICTURE ZZZ,9-.
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
           SET NOT-I-03                    TO TRUE
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-06                    TO TRUE
 
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
 
           IF  SALGF-PROCESS
               SET SALGF-PROCESS-OFF       TO TRUE
               SET SALGF-READ              TO TRUE
           END-IF
 
           IF  SALGF-READ
           AND RECORD-SELECTED-OFF
               PERFORM SALGF-GET
               SET SALGF-READ-OFF          TO TRUE
               IF  NOT SALGF-EOF
                   SET SALGF-PROCESS       TO TRUE
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
 
           IF  SALGF-PROCESS
               PERFORM SALGF-IDSET
           END-IF
 
           IF  SALGF-PROCESS
               PERFORM SALGF-CHK-LEVEL
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
 
           IF  SALGF-PROCESS
               PERFORM SALGF-FLDOFF
               PERFORM SALGF-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  SALGF-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-L5)
               PERFORM RBSRUT-S
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
      **************************************************************
           END-IF
           SET NOT-I-51                    TO TRUE
           IF  PMND = '01'
               SET I-51                    TO TRUE
           END-IF
           SET NOT-I-52                    TO TRUE
           IF  PMND = '02'
               SET I-52                    TO TRUE
           END-IF
           SET NOT-I-53                    TO TRUE
           IF  PMND = '03'
               SET I-53                    TO TRUE
           END-IF
           SET NOT-I-54                    TO TRUE
           IF  PMND = '04'
               SET I-54                    TO TRUE
           END-IF
           SET NOT-I-55                    TO TRUE
           IF  PMND = '05'
               SET I-55                    TO TRUE
           END-IF
           SET NOT-I-56                    TO TRUE
           IF  PMND = '06'
               SET I-56                    TO TRUE
           END-IF
           SET NOT-I-57                    TO TRUE
           IF  PMND = '07'
               SET I-57                    TO TRUE
           END-IF
           SET NOT-I-58                    TO TRUE
           IF  PMND = '08'
               SET I-58                    TO TRUE
           END-IF
           SET NOT-I-59                    TO TRUE
           IF  PMND = '09'
               SET I-59                    TO TRUE
           END-IF
           SET NOT-I-60                    TO TRUE
           IF  PMND = '10'
               SET I-60                    TO TRUE
           END-IF
           SET NOT-I-61                    TO TRUE
           IF  PMND = '11'
               SET I-61                    TO TRUE
           END-IF
           SET NOT-I-62                    TO TRUE
           IF  PMND = '12'
               SET I-62                    TO TRUE
           END-IF
      ****
           IF  (I-03)
               SET NOT-I-96                TO TRUE
               IF  PAAR = 0
                   SET I-96                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-96)
               SUBTRACT 1 FROM PAAR    GIVING PAAR1
           END-IF
           IF  (I-03 AND I-96)
               MOVE 99                     TO PAAR1
           END-IF
           IF  (I-03)
               GO TO END-X-T
           END-IF
           IF  (I-L1)
               MOVE 0                      TO DMNDF
               MOVE 0                      TO DMND
               MOVE 0                      TO TOTF
               MOVE 0                      TO TOT
               MOVE 0                      TO SVSUT
           END-IF
           IF  (I-L2)
               MOVE 0                      TO L2MNDF
               MOVE 0                      TO L2MND
               MOVE 0                      TO L2BRM
               MOVE 0                      TO L2TOTF
               MOVE 0                      TO L2TOT
               MOVE 0                      TO L2BRU
               MOVE 0                      TO L2BRF
               MOVE 0                      TO MTSVS
               MOVE 0                      TO TSVS
               MOVE 0                      TO VERDI
           END-IF
           IF  (I-L4)
               MOVE 0                      TO STSVS
               MOVE 0                      TO SSVS
      *******
           END-IF
           IF  (I-L2)
               MOVE FIRMA                  TO KEY9 (1:3)
               MOVE RESNR                  TO KEY9 (4:6)
               MOVE KEY9                   TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-91                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-91            TO TRUE
                   PERFORM KUNDEMA-FLDSET
                   PERFORM KUNDEMA-IDSET
               END-READ
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *  COBOL SUBRUTINE FOR HENTING AV BETALINGSMÅTE-TEKST     *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
           END-IF
           IF  (NOT-I-L2)
               GO TO ENDBM-T
           END-IF
           MOVE BETM                       TO BETMAT
           CALL 'BETBETN' USING BMFELT-XX-DATA-FIELDS.
 
       ENDBM-T.
           IF  (I-L1)
               MOVE FIRMA                  TO KEY-X (1:3)
               MOVE VGR                    TO KEY-X (4:5)
               MOVE KEY-X                  TO VAGRMAS-KEY1
               READ VAGRMAS RECORD KEY IS VAGRMAS-KEY1
               INVALID KEY
                   SET I-70                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-70            TO TRUE
                   PERFORM VAGRMAS-FLDOFF
                   PERFORM VAGRMAS-FLDSET
                   PERFORM VAGRMAS-IDSET
               END-READ
           END-IF
           IF  (I-01 AND NOT-I-70 AND NOT-I-67)
               GO TO END-X-T
      *******
           END-IF
           SET NOT-I-24                    TO TRUE
      ***************************************************************     BRF110
      *  DERSOM KREDITNOTA, OG DET IKKE ER RETUR AV VARER SETTES    *     BRF110
      *  SELVKOST FOR KREDITNOTAN LIK NULL.                         *     BRF110
      *  DETTE GJELDER OGSÅ OM DET ER ERSTATNING.                   *     BRF110
      ***************************************************************     BRF110
           IF  (I-01)
               SET NOT-I-95                TO TRUE
               IF  EDB1 = '9'
                   SET I-95                TO TRUE
               END-IF
               SET NOT-I-15                TO TRUE
               IF  FAKR = '2'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-15)
               SET NOT-I-19                TO TRUE
               IF  KREDK = '4'
                   SET I-19                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-15 AND I-19)
               MOVE 0                      TO ENHPR
           END-IF
           IF  (I-01 AND I-15)
               SET NOT-I-18                TO TRUE
               IF  KREDK = '2'
                   SET I-18                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-15 AND NOT-I-18)
               SET NOT-I-18                TO TRUE
               IF  KREDK = '5'
                   SET I-18                TO TRUE
               END-IF
      ****
           END-IF
           SET NOT-I-20                    TO TRUE
           IF  MND = PMND
               SET I-20                    TO TRUE
           END-IF
           SET NOT-I-21                    TO TRUE
           IF  MND NOT > PMND
               SET I-21                    TO TRUE
           END-IF
           SET NOT-I-30                    TO TRUE
           IF  AAR = PAAR
               SET I-30                    TO TRUE
           END-IF
           SET NOT-I-40                    TO TRUE
           IF  AAR = PAAR1
               SET I-40                    TO TRUE
           END-IF
           IF  (I-01 AND NOT-I-08)
               MULTIPLY BEST BY ENHPR  GIVING VERDI
           END-IF
           IF  (I-01 AND I-08)
               ADD ENHPR TO ZERO       GIVING VERDI
      ****
           END-IF
           IF  (NOT-I-10)
               MULTIPLY RABA BY VERDI  GIVING SUM-X
               DIVIDE SUM-X BY 100     GIVING SUM-X ROUNDED
               SUBTRACT SUM-X              FROM VERDI
      ****
           END-IF
           IF  (NOT-I-11)
               MULTIPLY RABB BY VERDI  GIVING SUM-X
               DIVIDE SUM-X BY 100     GIVING SUM-X ROUNDED
               SUBTRACT SUM-X              FROM VERDI
      ****
           END-IF
           IF  (NOT-I-12)
               MULTIPLY RABC BY VERDI  GIVING SUM-X
               DIVIDE SUM-X BY 100     GIVING SUM-X ROUNDED
               SUBTRACT SUM-X              FROM VERDI
      *
           END-IF
           IF  (I-20 AND I-30 AND NOT-I-15)
               ADD VERDI                   TO DMND
           END-IF
           IF  (I-20 AND I-30 AND I-15)
               SUBTRACT VERDI              FROM DMND
      *
           END-IF
           IF  (I-21 AND I-30 AND NOT-I-15)
               ADD VERDI                   TO TOT
           END-IF
           IF  (I-21 AND I-30 AND I-15)
               SUBTRACT VERDI              FROM TOT
      *
      *
           END-IF
           IF  (I-20 AND I-40 AND NOT-I-15)
               ADD VERDI                   TO DMNDF
           END-IF
           IF  (I-20 AND I-40 AND I-15)
               SUBTRACT VERDI              FROM DMNDF
      *
           END-IF
           IF  (I-21 AND I-40 AND NOT-I-15)
               ADD VERDI                   TO TOTF
           END-IF
           IF  (I-21 AND I-40 AND I-15)
               SUBTRACT VERDI              FROM TOTF
      *
      ***************************************************************
      *  RUTINE FOR BEREGNING AV S.V.S.                             *
      *  DERSOM SVS=0  BEREGNES SVS UTFRA % I VAGRMAS.              *
      *     ER FORTJ.% I VAGRMAS 0 SETTES SVS TIL NETTOPRIS.        *
      ***************************************************************
           END-IF
           SET NOT-I-14                    TO TRUE
           IF  BK = '4'
               SET I-14                    TO TRUE
           END-IF
           IF  (I-07)
               OR  (NOT-I-16)
               OR  (I-14)
               GO TO SVSOK-T
           END-IF
           IF  (I-70)
               OR  (NOT-I-70 AND I-35)
               SET I-24                    TO TRUE
           END-IF
           IF  (I-24)
               GO TO SVSOK-T
           END-IF
           MOVE 0                          TO NYPR
      *
           SUBTRACT FPROS FROM 100     GIVING PROS
           MULTIPLY PROS BY VERDI      GIVING BSVS ROUNDED
           DIVIDE BSVS BY 100          GIVING SVSUT
           GO TO OKSVS-T.
 
       SVSOK-T.
      *
           IF  (NOT-I-08 AND I-21)
               MULTIPLY BEST BY SVS    GIVING SVSUT
           END-IF
           IF  (I-08 AND I-21)
               ADD SVS TO ZERO         GIVING SVSUT
           END-IF.
 
       OKSVS-T.
      *
           IF  (I-15 AND NOT-I-18 AND I-21)
               MOVE 0                      TO SVSUT
      ***********************************************************
      * VARER UTEN SVS OG FORTJ.PROS SETTES TIL 0 FORTJENESTE.  *
      ***********************************************************
           END-IF
           IF  (I-14 AND I-21)
               ADD VERDI TO ZERO       GIVING SVSUT
           END-IF
           IF  (I-24 AND I-21)
               ADD VERDI TO ZERO       GIVING SVSUT
      *
           END-IF
           IF  (NOT-I-15 AND I-20 AND I-30)
               ADD SVSUT                   TO MTSVS
           END-IF
           IF  (I-15 AND I-20 AND I-30)
               SUBTRACT SVSUT              FROM MTSVS
      *
           END-IF
           IF  (NOT-I-15 AND I-20 AND I-30)
               ADD SVSUT                   TO SSVS
           END-IF
           IF  (I-15 AND I-20 AND I-30)
               SUBTRACT SVSUT              FROM SSVS
      *
           END-IF
           IF  (NOT-I-15 AND I-20 AND I-30)
               ADD SVSUT                   TO TMSVS
           END-IF
           IF  (I-15 AND I-20 AND I-30)
               SUBTRACT SVSUT              FROM TMSVS
      *
           END-IF
           IF  (NOT-I-15 AND I-21 AND I-30)
               ADD SVSUT                   TO TSVS
           END-IF
           IF  (I-15 AND I-21 AND I-30)
               SUBTRACT SVSUT              FROM TSVS
      *
           END-IF
           IF  (NOT-I-15 AND I-21 AND I-30)
               ADD SVSUT                   TO STSVS
           END-IF
           IF  (I-15 AND I-21 AND I-30)
               SUBTRACT SVSUT              FROM STSVS
      *
           END-IF
           IF  (NOT-I-15 AND I-21 AND I-30)
               ADD SVSUT                   TO GTSVS
           END-IF
           IF  (I-15 AND I-21 AND I-30)
               SUBTRACT SVSUT              FROM GTSVS
           END-IF
           MOVE 0                          TO SVSUT.
 
       END-X-T.
      ******************************************************
      *0 50                GOTO SLUTT
      *
           CONTINUE.
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           IF  (NOT-I-89)
               MOVE ' '                    TO BBEST
           END-IF
           MOVE 'ST92F'                    TO LONR
           MOVE FIRMA                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'STA818  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF.
      ******************************************************
      **************************************************************
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1)
               ADD DMNDF                   TO L2MNDF
               ADD TOTF                    TO L2TOTF
      *
           END-IF
           IF  (I-L1)
               ADD DMND                    TO L2MND
               ADD TOT                     TO L2TOT
      *
           END-IF
           IF  (I-L1 AND I-60)
               SET NOT-I-60                TO TRUE
               IF  DMND = 0
                   SET I-60                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-60)
               SET NOT-I-60                TO TRUE
               IF  TOT = 0
                   SET I-60                TO TRUE
               END-IF
      *
           END-IF
           IF  (I-L1)
               ADD DMND                    TO L4MND
               ADD TOT                     TO L4TOT
      *
           END-IF
           IF  (I-L1)
               ADD DMND                    TO L5MND
               ADD TOT                     TO L5TOT
      *
           END-IF
           IF  (I-L2)
               ADD L2MNDF                  TO L4MNDF
               ADD L2TOTF                  TO L4TOTF
      *
           END-IF
           IF  (I-L2)
               ADD L2MNDF                  TO L5MNDF
               ADD L2TOTF                  TO L5TOTF
      *                 END OF HISTORIKK.
      ***************************************************
           END-IF
           IF  (I-L2)
               SET NOT-I-75                TO TRUE
               IF  L2MND = 0
                   SET I-75                TO TRUE
               END-IF
      *
           END-IF
           IF  (I-L2 AND NOT-I-75)
               DIVIDE MTSVS BY L2MND   GIVING L2BMA-ELGR
               MULTIPLY 100 BY L2BMA-ELGR GIVING L2BM
               SUBTRACT L2BM FROM 100  GIVING L2BRM
      *
           END-IF
           IF  (I-L2)
               SET NOT-I-73                TO TRUE
               IF  L2TOT = 0
                   SET I-73                TO TRUE
               END-IF
      *
           END-IF
           IF  (I-L2 AND NOT-I-73)
               DIVIDE TSVS BY L2TOT    GIVING L2BIA-ELGR
               MULTIPLY 100 BY L2BIA-ELGR GIVING L2BRF ROUNDED
               SUBTRACT L2BRF FROM 100 GIVING L2BRU
      *
           END-IF
           IF  (I-L4)
               SET NOT-I-81                TO TRUE
               IF  L4MND = 0
                   SET I-81                TO TRUE
               END-IF
               SET NOT-I-80                TO TRUE
               IF  L4TOT = 0
                   SET I-80                TO TRUE
               END-IF
      *
           END-IF
           IF  (I-L4 AND NOT-I-80)
               DIVIDE STSVS BY L4TOT   GIVING L4BIA-ELGR
           END-IF
           IF  (I-L4 AND NOT-I-81)
               DIVIDE SSVS BY L4MND    GIVING L4BM
           END-IF
           IF  (I-L4)
               MULTIPLY 100 BY L4BIA-ELGR GIVING L4BRF ROUNDED
           END-IF
           IF  (I-L4 AND NOT-I-81)
               MULTIPLY 100 BY L4BM    GIVING L4BRM ROUNDED
           END-IF
           IF  (I-L4)
               SUBTRACT L4BRF FROM 100 GIVING L4BRU
           END-IF
           IF  (I-L4 AND NOT-I-81)
               SUBTRACT L4BRM FROM 100 GIVING L4BMND
      *
           END-IF
           IF  (I-L5)
               SET NOT-I-78                TO TRUE
               IF  L5MND = 0
                   SET I-78                TO TRUE
               END-IF
               SET NOT-I-77                TO TRUE
               IF  L5TOT = 0
                   SET I-77                TO TRUE
               END-IF
      *
           END-IF
           IF  (I-L5 AND NOT-I-77)
               DIVIDE GTSVS BY L5TOT   GIVING L5BIA-ELGR
           END-IF
           IF  (I-L5 AND NOT-I-78)
               DIVIDE TMSVS BY L5MND   GIVING L5BM
           END-IF
           IF  (I-L5)
               MULTIPLY 100 BY L5BIA-ELGR GIVING L5BRF ROUNDED
               MULTIPLY 100 BY L5BM    GIVING L5BRM ROUNDED
               SUBTRACT L5BRF FROM 100 GIVING L5BRU
               SUBTRACT L5BRM FROM 100 GIVING L5BMND
      *
           END-IF
           .
 
       SLUTT-T.
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           CONTINUE.
 
       PARAM-GET SECTION.
       PARAM-GET-P.
           IF  PARAM-EOF-OFF
               READ PARAM
               AT END
                   SET PARAM-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
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
           WHEN ( PARAM-IO-AREA (1:1) = '9'
            AND   PARAM-IO-AREA (2:1) = '0' )
               MOVE PARAM-IO-AREA (9:2)    TO PAAR-IO
               INSPECT PAAR-IO REPLACING ALL ' ' BY '0'
               MOVE PARAM-IO-AREA (11:2)   TO PMND (1:2)
               MOVE PARAM-IO-AREA (11:2)   TO A-IO
               INSPECT A-IO REPLACING ALL ' ' BY '0'
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
             OR ( PARAM-IO-AREA (1:1) = '9'
            AND   PARAM-IO-AREA (2:1) = '0' )
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
           WHEN ( PARAM-IO-AREA (1:1) = '9'
            AND   PARAM-IO-AREA (2:1) = '0' )
               SET I-03                    TO TRUE
           END-EVALUATE.
 
       SALGF-GET SECTION.
       SALGF-GET-P.
           IF  SALGF-EOF-OFF
               READ SALGF
               AT END
                   SET SALGF-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       SALGF-FLDOFF SECTION.
       SALGF-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-16                TO TRUE
               SET NOT-I-08                TO TRUE
               SET NOT-I-10                TO TRUE
               SET NOT-I-11                TO TRUE
               SET NOT-I-12                TO TRUE
               SET NOT-I-07                TO TRUE
           END-EVALUATE.
 
       SALGF-FLDSET SECTION.
       SALGF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE SALGF-IO-AREA (3:5)    TO SVS-IO
               IF  SVS = ZERO
                   SET I-16                TO TRUE
               END-IF
               MOVE SALGF-IO-AREA (12:4)   TO BEST-IO
               IF  BEST = ZERO
                   SET I-08                TO TRUE
               END-IF
               MOVE SALGF-IO-AREA (16:1)   TO EDB1 (1:1)
               MOVE SALGF-IO-AREA (23:3)   TO RABA-IO
               INSPECT RABA-IO REPLACING ALL ' ' BY '0'
               IF  RABA = ZERO
                   SET I-10                TO TRUE
               END-IF
               MOVE SALGF-IO-AREA (26:3)   TO RABB-IO
               INSPECT RABB-IO REPLACING ALL ' ' BY '0'
               IF  RABB = ZERO
                   SET I-11                TO TRUE
               END-IF
               MOVE SALGF-IO-AREA (29:3)   TO RABC-IO
               INSPECT RABC-IO REPLACING ALL ' ' BY '0'
               IF  RABC = ZERO
                   SET I-12                TO TRUE
               END-IF
               MOVE SALGF-IO-AREA (41:1)   TO FAKR (1:1)
               MOVE SALGF-IO-AREA (32:9)   TO ENHPR-IO
               INSPECT ENHPR-IO REPLACING ALL ' ' BY '0'
               IF  ENHPR = ZERO
                   SET I-07                TO TRUE
               END-IF
               MOVE SALGF-IO-AREA (42:2)   TO MND (1:2)
               MOVE SALGF-IO-AREA (44:1)   TO BK (1:1)
               MOVE SALGF-IO-AREA (45:6)   TO RESNR (1:6)
               MOVE SALGF-IO-AREA (51:3)   TO FIRMA (1:3)
               MOVE SALGF-IO-AREA (54:3)   TO HND (1:3)
               MOVE SALGF-IO-AREA (58:2)   TO AAR-IO
               INSPECT AAR-IO REPLACING ALL ' ' BY '0'
               MOVE SALGF-IO-AREA (60:5)   TO VGR (1:5)
               MOVE SALGF-IO-AREA (66:1)   TO KREDK (1:1)
               MOVE SALGF-IO-AREA (71:4)   TO ORDNR-IO
               MOVE SALGF-IO-AREA (108:3)  TO SNR (1:3)
               MOVE SALGF-IO-AREA (111:1)  TO SELG1 (1:1)
               MOVE SALGF-IO-AREA (111:20) TO SELGN (1:20)
           END-EVALUATE.
 
       SALGF-IDSET SECTION.
       SALGF-IDSET-P.
           SET I-01                        TO TRUE.
 
       SALGF-CHK-LEVEL SECTION.
       SALGF-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO SALGF-LEVEL-01
               MOVE SALGF-IO-AREA (51:3)   TO SALGF-01-L5-FIRMA
               MOVE SALGF-IO-AREA (111:20) TO SALGF-01-L4-SELGN
               MOVE SALGF-IO-AREA (45:6)   TO SALGF-01-L2-RESNR
               MOVE SALGF-IO-AREA (60:5)   TO SALGF-01-L1-VGR
               IF  SALGF-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  SALGF-01-L5 NOT = THE-PRIOR-L5
                       PERFORM SETON-I-L5
                   WHEN  SALGF-01-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   WHEN  SALGF-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  SALGF-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  SALGF-01-L5           TO THE-PRIOR-L5
               MOVE  SALGF-01-L4           TO THE-PRIOR-L4
               MOVE  SALGF-01-L2           TO THE-PRIOR-L2
               MOVE  SALGF-01-L1           TO THE-PRIOR-L1
               SET SALGF-LEVEL-INIT        TO TRUE
           END-EVALUATE.
 
       VAGRMAS-FLDOFF SECTION.
       VAGRMAS-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-35                TO TRUE
               SET NOT-I-67                TO TRUE
           END-EVALUATE.
 
       VAGRMAS-FLDSET SECTION.
       VAGRMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAGRMAS-IO-AREA (77:4) TO FPROS-IO
               INSPECT FPROS-IO REPLACING ALL ' ' BY '0'
               IF  FPROS = ZERO
                   SET I-35                TO TRUE
               END-IF
               MOVE VAGRMAS-IO-AREA (69:4) TO KONTO (1:4)
               IF  KONTO = SPACES
                   SET I-67                TO TRUE
               END-IF
           END-EVALUATE.
 
       VAGRMAS-IDSET SECTION.
       VAGRMAS-IDSET-P.
           SET I-02                        TO TRUE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (16:30) TO NAVN (1:30)
               MOVE KUNDEMA-IO-AREA (127:2) TO BETM (1:2)
               MOVE KUNDEMA-IO-AREA (190:9) TO RABK (1:9)
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-06                        TO TRUE.
 
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
           IF  (I-L4 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE LOPNVN                 TO LISTE-IO-AREA (32:35)
               MOVE SELGN                  TO LISTE-IO-AREA (71:20)
               MOVE SNR                    TO LISTE-IO-AREA (93:3)
               IF  (I-51)
                   MOVE 'PR.JAN.     '     TO LISTE-IO-AREA (101:12)
               END-IF
               IF  (I-52)
                   MOVE 'PR.FEBRUAR  '     TO LISTE-IO-AREA (101:12)
               END-IF
               IF  (I-53)
                   MOVE 'PR.MARS     '     TO LISTE-IO-AREA (101:12)
               END-IF
               IF  (I-54)
                   MOVE 'PR.APRIL    '     TO LISTE-IO-AREA (101:12)
               END-IF
               IF  (I-55)
                   MOVE 'PR.MAI      '     TO LISTE-IO-AREA (101:12)
               END-IF
               IF  (I-56)
                   MOVE 'PR.JUNI     '     TO LISTE-IO-AREA (101:12)
               END-IF
               IF  (I-57)
                   MOVE 'PR.JULI     '     TO LISTE-IO-AREA (101:12)
               END-IF
               IF  (I-58)
                   MOVE 'PR.AUGUST   '     TO LISTE-IO-AREA (101:12)
               END-IF
               IF  (I-59)
                   MOVE 'PR.SEPTEMBER'     TO LISTE-IO-AREA (101:12)
               END-IF
               IF  (I-60)
                   MOVE 'PR.OKTOBER  '     TO LISTE-IO-AREA (101:12)
               END-IF
               IF  (I-61)
                   MOVE 'PR.NOVEMBER '     TO LISTE-IO-AREA (101:12)
               END-IF
               IF  (I-62)
                   MOVE 'PR.DESEMBER '     TO LISTE-IO-AREA (101:12)
               END-IF
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FREMSTILT'            TO LISTE-IO-AREA (100:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (113:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (125:4)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (129:4)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KUNDE'                TO LISTE-IO-AREA (1:5)
               MOVE 'KUNDENAVN'            TO LISTE-IO-AREA (11:9)
               MOVE 'SALG DM I FJOR'       TO LISTE-IO-AREA (41:14)
               MOVE 'SALG DM I ÅR'         TO LISTE-IO-AREA (61:12)
               MOVE 'BRFT.'                TO LISTE-IO-AREA (75:5)
               MOVE '  SALG IFJOR  '       TO LISTE-IO-AREA (85:14)
               MOVE 'SALG HIT IÅR'         TO LISTE-IO-AREA (105:12)
               MOVE 'BRFT.'                TO LISTE-IO-AREA (118:5)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF AND NOT-I-L4 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE LOPNVN                 TO LISTE-IO-AREA (32:35)
               MOVE SELGN                  TO LISTE-IO-AREA (71:20)
               MOVE SNR                    TO LISTE-IO-AREA (93:3)
               IF  (I-51)
                   MOVE 'PR.JAN.     '     TO LISTE-IO-AREA (101:12)
               END-IF
               IF  (I-52)
                   MOVE 'PR.FEBRUAR  '     TO LISTE-IO-AREA (101:12)
               END-IF
               IF  (I-53)
                   MOVE 'PR.MARS     '     TO LISTE-IO-AREA (101:12)
               END-IF
               IF  (I-54)
                   MOVE 'PR.APRIL    '     TO LISTE-IO-AREA (101:12)
               END-IF
               IF  (I-55)
                   MOVE 'PR.MAI      '     TO LISTE-IO-AREA (101:12)
               END-IF
               IF  (I-56)
                   MOVE 'PR.JUNI     '     TO LISTE-IO-AREA (101:12)
               END-IF
               IF  (I-57)
                   MOVE 'PR.JULI     '     TO LISTE-IO-AREA (101:12)
               END-IF
               IF  (I-58)
                   MOVE 'PR.AUGUST   '     TO LISTE-IO-AREA (101:12)
               END-IF
               IF  (I-59)
                   MOVE 'PR.SEPTEMBER'     TO LISTE-IO-AREA (101:12)
               END-IF
               IF  (I-60)
                   MOVE 'PR.OKTOBER  '     TO LISTE-IO-AREA (101:12)
               END-IF
               IF  (I-61)
                   MOVE 'PR.NOVEMBER '     TO LISTE-IO-AREA (101:12)
               END-IF
               IF  (I-62)
                   MOVE 'PR.DESEMBER '     TO LISTE-IO-AREA (101:12)
               END-IF
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FREMSTILT'            TO LISTE-IO-AREA (100:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (113:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (125:4)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (129:4)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KUNDE'                TO LISTE-IO-AREA (1:5)
               MOVE 'KUNDENAVN'            TO LISTE-IO-AREA (11:9)
               MOVE 'SALG DM I FJOR'       TO LISTE-IO-AREA (41:14)
               MOVE 'SALG DM I ÅR'         TO LISTE-IO-AREA (61:12)
               MOVE 'BRFT.'                TO LISTE-IO-AREA (75:5)
               MOVE '  SALG IFJOR  '       TO LISTE-IO-AREA (85:14)
               MOVE 'SALG HIT IÅR'         TO LISTE-IO-AREA (105:12)
               MOVE 'BRFT.'                TO LISTE-IO-AREA (118:5)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L2 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE RESNR                  TO LISTE-IO-AREA (1:6)
               IF  (NOT-I-91)
                   MOVE NAVN               TO LISTE-IO-AREA (8:30)
                   INITIALIZE NAVN
               END-IF
               IF  (I-91)
                   MOVE 'KUNDE UKJENT'     TO LISTE-IO-AREA (26:12)
               END-IF
               MOVE L2MNDF                 TO XO-102YY9R
               MOVE XO-102YY9R             TO LISTE-IO-AREA (38:17)
               INITIALIZE L2MNDF
      *                        MTSVS JB  54
               MOVE L2MND                  TO XO-102YY9R
               MOVE XO-102YY9R             TO LISTE-IO-AREA (56:17)
               INITIALIZE L2MND
               MOVE L2BRM                  TO XO-31YY9R
               MOVE XO-31YY9R              TO LISTE-IO-AREA (74:6)
               INITIALIZE L2BRM
               MOVE L2TOTF                 TO XO-102YY9R
               MOVE XO-102YY9R             TO LISTE-IO-AREA (82:17)
               INITIALIZE L2TOTF
      *                        TSVS  JB  98
               MOVE L2TOT                  TO XO-102YY9R
               MOVE XO-102YY9R             TO LISTE-IO-AREA (100:17)
               INITIALIZE L2TOT
               MOVE L2BRU                  TO XO-31YY9R
               MOVE XO-31YY9R              TO LISTE-IO-AREA (117:6)
               INITIALIZE L2BRU
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L2 AND NOT-I-86 AND NOT-I-91)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'BETALINGSMÅTE KAJO:'  TO LISTE-IO-AREA (2:19)
               MOVE BETM                   TO LISTE-IO-AREA (22:2)
               MOVE BMTKST                 TO LISTE-IO-AREA (27:24)
               MOVE 'GRUPPERABATT KODER:'  TO LISTE-IO-AREA (54:19)
               MOVE RABK                   TO LISTE-IO-AREA (74:9)
      * TOTALT PR. SELGER
      *******************
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L4 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALT PR.SELGER      ' TO LISTE-IO-AREA (9:22)
               MOVE L4MNDF                 TO XO-102YY9R
               MOVE XO-102YY9R             TO LISTE-IO-AREA (38:17)
               INITIALIZE L4MNDF
               MOVE L4MND                  TO XO-102YY9R
               MOVE XO-102YY9R             TO LISTE-IO-AREA (56:17)
               INITIALIZE L4MND
               MOVE L4BMND                 TO XO-31YY9R
               MOVE XO-31YY9R              TO LISTE-IO-AREA (74:6)
               INITIALIZE L4BMND
               MOVE L4TOTF                 TO XO-102YY9R
               MOVE XO-102YY9R             TO LISTE-IO-AREA (82:17)
               INITIALIZE L4TOTF
      *                        STSVS JB  98
               MOVE L4TOT                  TO XO-102YY9R
               MOVE XO-102YY9R             TO LISTE-IO-AREA (100:17)
               INITIALIZE L4TOT
               MOVE L4BRU                  TO XO-31YY9R
               MOVE XO-31YY9R              TO LISTE-IO-AREA (117:6)
               INITIALIZE L4BRU
      * TOTALT PR. FIRMA
      ******************
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L5 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'SUM SELGERE           ' TO LISTE-IO-AREA (9:22)
               MOVE L5MNDF                 TO XO-102YY9R
               MOVE XO-102YY9R             TO LISTE-IO-AREA (38:17)
               INITIALIZE L5MNDF
               MOVE L5MND                  TO XO-102YY9R
               MOVE XO-102YY9R             TO LISTE-IO-AREA (56:17)
               INITIALIZE L5MND
               MOVE L5BMND                 TO XO-31YY9R
               MOVE XO-31YY9R              TO LISTE-IO-AREA (74:6)
               INITIALIZE L5BMND
               MOVE L5TOTF                 TO XO-102YY9R
               MOVE XO-102YY9R             TO LISTE-IO-AREA (82:17)
               INITIALIZE L5TOTF
               MOVE L5TOT                  TO XO-102YY9R
               MOVE XO-102YY9R             TO LISTE-IO-AREA (100:17)
               INITIALIZE L5TOT
               MOVE L5BRU                  TO XO-31YY9R
               MOVE XO-31YY9R              TO LISTE-IO-AREA (117:6)
               INITIALIZE L5BRU
               MOVE 1                      TO LISTE-BEFORE-SPACE
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
           INITIALIZE PARAM-DATA-FIELDS
           SET PARAM-EOF-OFF               TO TRUE
           SET PARAM-PROCESS               TO TRUE
           OPEN INPUT PARAM
           SET SALGF-LEVEL-INIT            TO TRUE
           INITIALIZE SALGF-DATA-FIELDS
           SET SALGF-EOF-OFF               TO TRUE
           SET SALGF-PROCESS               TO TRUE
           OPEN INPUT SALGF
           INITIALIZE VAGRMAS-DATA-FIELDS
           OPEN INPUT VAGRMAS
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PARAM
           CLOSE SALGF
           CLOSE VAGRMAS
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
