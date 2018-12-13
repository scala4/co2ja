       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAK010R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAM MAK010.   PROGRAMMERT 12.05.1999                      *
      * PROGRAMMERER: ESPEN LARSEN                                    *
      * PROGRAMMET OPPDATERER FERDIGMELDTE SERVICEORDRE INN I         *
      * MASKIN.SERVICE.FILE                                           *
      * ER MASKINSERIENR UKJENT LEGGES DETTE INN I MASKIN.KORT.FILE   *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: MAK010.rpg
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
           SELECT ORDREFS
               ASSIGN TO UT-S-ORDREFS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ORDREFS-STATUS.
           SELECT MKMAST
               ASSIGN TO MKMAST
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS MKMAST-STATUS
               RECORD KEY IS MKMAST-KEY1.
           SELECT MKSERV
               ASSIGN TO MKSERV
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS MKSERV-STATUS
               RECORD KEY IS MKSERV-KEY1.
           SELECT OPPSMAS
               ASSIGN TO OPPSMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS OPPSMAS-STATUS
               RECORD KEY IS OPPSMAS-KEY1.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD ORDREFS
               BLOCK CONTAINS 328
               RECORD CONTAINS 164.
       01  ORDREFS-IO-AREA.
           05  ORDREFS-IO-AREA-X           PICTURE X(164).
       FD MKMAST
               RECORD CONTAINS 160.
       01  MKMAST-IO-AREA.
           05  MKMAST-IO-AREA-X.
               10  MKMAST-KEY1             PICTURE X(27).
               10  FILLER                  PICTURE X(133).
       FD MKSERV
               RECORD CONTAINS 800.
       01  MKSERV-IO-AREA.
           05  MKSERV-IO-AREA-X.
               10  MKSERV-KEY1             PICTURE X(30).
               10  FILLER                  PICTURE X(770).
       FD OPPSMAS
               RECORD CONTAINS 30.
       01  OPPSMAS-IO-AREA.
           05  OPPSMAS-IO-AREA-X.
               10  OPPSMAS-KEY1            PICTURE X(21).
               10  FILLER                  PICTURE X(9).
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       WORKING-STORAGE SECTION.
       77  TXA-MAX   VALUE 10              PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  TXA-TABLE.
               10  TXA-ENTRY
                                           OCCURS 10 TIMES
                                           INDEXED BY TXA-I
                                                      TXA-S.
                   15  TXA                 PICTURE X(70).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  ORDREFS-STATUS              PICTURE 99 VALUE 0.
           10  MKMAST-STATUS               PICTURE 99 VALUE 0.
           10  MKSERV-STATUS               PICTURE 99 VALUE 0.
           10  OPPSMAS-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  DATOER-XX-STATUS            PICTURE 99 VALUE 0.
           10  OPFELT-XX-STATUS            PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDREFS-EOF-OFF         VALUE '0'.
               88  ORDREFS-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDREFS-READ-OFF        VALUE '0'.
               88  ORDREFS-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDREFS-PROCESS-OFF     VALUE '0'.
               88  ORDREFS-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  ORDREFS-LEVEL-INIT-OFF  VALUE '0'.
               88  ORDREFS-LEVEL-INIT      VALUE '1'.
           05  MKMAST-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  MKSERV-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  OPPSMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  DATOER-XX-DATA-FIELDS.
               10  DATOK                   PICTURE X(1).
               10  FILLER                  PICTURE X(256).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(1).
               10  DATO6                   PICTURE X(6).
               10  FILLER                  PICTURE X(250).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(7).
               10  DMA8                    PICTURE X(8).
               10  FILLER                  PICTURE X(242).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(15).
               10  AMD8                    PICTURE X(8).
               10  FILLER                  PICTURE X(234).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(23).
               10  DATOM                   PICTURE X(57).
               10  FILLER                  PICTURE X(177).
           05  OPFELT-XX REDEFINES DATOER-XX-DATA-FIELDS.
               10  OARTNR                  PICTURE X(20).
               10  FILLER                  PICTURE X(237).
           05  LDATA-XX REDEFINES DATOER-XX-DATA-FIELDS.
               10  LONR                    PICTURE X(5).
               10  FILLER                  PICTURE X(252).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(5).
               10  LFIRMA                  PICTURE X(3).
               10  FILLER                  PICTURE X(249).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(8).
               10  LUNDGR                  PICTURE X(3).
               10  FILLER                  PICTURE X(246).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(11).
               10  LPROG                   PICTURE X(8).
               10  FILLER                  PICTURE X(238).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(19).
               10  LANTX-IO.
                   15  LANTX               PICTURE S9(3).
               10  FILLER                  PICTURE X(235).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(22).
               10  FINAVN                  PICTURE X(30).
               10  FILLER                  PICTURE X(205).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(52).
               10  LOPNVN                  PICTURE X(35).
               10  FILLER                  PICTURE X(170).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(87).
               10  LPRIID                  PICTURE X(4).
               10  FILLER                  PICTURE X(166).
      *     *  BESTILLINGSOPPGAVER (OVERSTYRING AV RBS-FILE) *    *
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(91).
               10  BJOBN                   PICTURE X(8).
               10  FILLER                  PICTURE X(158).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(99).
               10  BBEST                   PICTURE X(1).
               10  FILLER                  PICTURE X(157).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(100).
               10  BPERS                   PICTURE X(30).
               10  FILLER                  PICTURE X(127).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(130).
               10  BETTB                   PICTURE X(40).
               10  FILLER                  PICTURE X(87).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(170).
               10  BFORS                   PICTURE X(40).
               10  FILLER                  PICTURE X(47).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(210).
               10  BMEMO                   PICTURE X(40).
               10  FILLER                  PICTURE X(7).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(250).
               10  BANTX-IO.
                   15  BANTX               PICTURE S9(3).
               10  FILLER                  PICTURE X(4).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(253).
               10  BPCLAS                  PICTURE X(1).
               10  FILLER                  PICTURE X(3).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(254).
               10  BPRJE                   PICTURE X(3).
      *DSDS: DATA STRUCTURE FIELDS
           05  OPFELT-XX-DATA-FIELDS.
               10  OARTNR                  PICTURE X(20).
               10  FILLER                  PICTURE X(237).
           05  LDATA-XX REDEFINES OPFELT-XX-DATA-FIELDS.
               10  LONR                    PICTURE X(5).
               10  FILLER                  PICTURE X(252).
           05  FILLER REDEFINES OPFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(5).
               10  LFIRMA                  PICTURE X(3).
               10  FILLER                  PICTURE X(249).
           05  FILLER REDEFINES OPFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(8).
               10  LUNDGR                  PICTURE X(3).
               10  FILLER                  PICTURE X(246).
           05  FILLER REDEFINES OPFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(11).
               10  LPROG                   PICTURE X(8).
               10  FILLER                  PICTURE X(238).
           05  FILLER REDEFINES OPFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(19).
               10  LANTX-IO.
                   15  LANTX               PICTURE S9(3).
               10  FILLER                  PICTURE X(235).
           05  FILLER REDEFINES OPFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(22).
               10  FINAVN                  PICTURE X(30).
               10  FILLER                  PICTURE X(205).
           05  FILLER REDEFINES OPFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(52).
               10  LOPNVN                  PICTURE X(35).
               10  FILLER                  PICTURE X(170).
           05  FILLER REDEFINES OPFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(87).
               10  LPRIID                  PICTURE X(4).
               10  FILLER                  PICTURE X(166).
      *     *  BESTILLINGSOPPGAVER (OVERSTYRING AV RBS-FILE) *    *
           05  FILLER REDEFINES OPFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(91).
               10  BJOBN                   PICTURE X(8).
               10  FILLER                  PICTURE X(158).
           05  FILLER REDEFINES OPFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(99).
               10  BBEST                   PICTURE X(1).
               10  FILLER                  PICTURE X(157).
           05  FILLER REDEFINES OPFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(100).
               10  BPERS                   PICTURE X(30).
               10  FILLER                  PICTURE X(127).
           05  FILLER REDEFINES OPFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(130).
               10  BETTB                   PICTURE X(40).
               10  FILLER                  PICTURE X(87).
           05  FILLER REDEFINES OPFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(170).
               10  BFORS                   PICTURE X(40).
               10  FILLER                  PICTURE X(47).
           05  FILLER REDEFINES OPFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(210).
               10  BMEMO                   PICTURE X(40).
               10  FILLER                  PICTURE X(7).
           05  FILLER REDEFINES OPFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(250).
               10  BANTX-IO.
                   15  BANTX               PICTURE S9(3).
               10  FILLER                  PICTURE X(4).
           05  FILLER REDEFINES OPFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(253).
               10  BPCLAS                  PICTURE X(1).
               10  FILLER                  PICTURE X(3).
           05  FILLER REDEFINES OPFELT-XX-DATA-FIELDS.
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
           05  ORDREFS-LEVEL-01.
               10  ORDREFS-01-L2.
                   15  ORDREFS-01-L2-FIRMA PICTURE X(3).
               10  ORDREFS-01-L1.
                   15  ORDREFS-01-L1-ORDRE PICTURE X(6).
           05  ORDREFS-LEVEL-02.
               10  ORDREFS-02-L2.
                   15  ORDREFS-02-L2-FIRMA PICTURE X(3).
               10  ORDREFS-02-L1.
                   15  ORDREFS-02-L1-ORDRE PICTURE X(6).
           05  ORDREFS-LEVEL-03.
               10  ORDREFS-03-L2.
                   15  ORDREFS-03-L2-FIRMA PICTURE X(3).
               10  ORDREFS-03-L1.
                   15  ORDREFS-03-L1-ORDRE PICTURE X(6).
           05  ORDREFS-LEVEL-04.
               10  ORDREFS-04-L2.
                   15  ORDREFS-04-L2-FIRMA PICTURE X(3).
               10  ORDREFS-04-L1.
                   15  ORDREFS-04-L1-ORDRE PICTURE X(6).
           05  ORDREFS-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  ORDRE                   PICTURE X(6).
               10  KUNDNR                  PICTURE X(6).
               10  BK                      PICTURE X(1).
               10  OPPDTY                  PICTURE X(1).
               10  SERVTY                  PICTURE X(1).
               10  ORDATO-IO.
                   15  ORDATO              PICTURE S9(6).
               10  FERDIM                  PICTURE X(1).
               10  REGKL-IO.
                   15  REGKL               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  OPDATO-IO.
                   15  OPDATO              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  STATUS-X                PICTURE X(1).
               10  MEKNR                   PICTURE X(3).
               10  POSNR                   PICTURE X(3).
               10  ALF                     PICTURE X(3).
               10  TEKST                   PICTURE X(50).
               10  ARTNR                   PICTURE X(20).
               10  VARBET                  PICTURE X(30).
               10  SNRIND                  PICTURE X(7).
               10  SNR                     PICTURE X(20).
               10  EDBNR-IO.
                   15  EDBNR               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  VGR-IO.
                   15  VGR                 PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
           05  MKMAST-DATA-FIELDS.
               10  SSNR                    PICTURE X(3).
           05  MKSERV-DATA-FIELDS.
               10  FILLER                  PICTURE X.
           05  OPPSMAS-DATA-FIELDS.
               10  OEDBNR-IO.
                   15  OEDBNR              PICTURE S9(7).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  X-IO.
                   15  X                   PICTURE S9(1).
               10  F4                      PICTURE X(4).
               10  F7                      PICTURE X(7).
               10  MKKEY                   PICTURE X(27).
               10  SSNR2-IO.
                   15  SSNR2               PICTURE S9(3).
               10  OPNR14                  PICTURE X(14).
               10  F6                      PICTURE X(6).
               10  OPKEY                   PICTURE X(21).
               10  F27                     PICTURE X(27).
               10  MSKEY                   PICTURE X(30).
               10  SSNRA                   PICTURE X(3).
               10  SDATO                   PICTURE X(8).
           05  EDITTING-FIELDS.
               10  XO-40D                  PICTURE S9(4).
               10  XO-40U                  PICTURE 9(4).
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
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-04                    TO TRUE
           SET NOT-I-05                    TO TRUE
           SET NOT-I-06                    TO TRUE
           SET NOT-I-07                    TO TRUE
           SET NOT-I-08                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  ORDREFS-PROCESS
               SET ORDREFS-PROCESS-OFF     TO TRUE
               SET ORDREFS-READ            TO TRUE
           END-IF
 
           IF  ORDREFS-READ
           AND RECORD-SELECTED-OFF
               PERFORM ORDREFS-GET
               SET ORDREFS-READ-OFF        TO TRUE
               IF  NOT ORDREFS-EOF
                   SET ORDREFS-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  ORDREFS-PROCESS
               PERFORM ORDREFS-IDSET
           END-IF
 
           IF  ORDREFS-PROCESS
               PERFORM ORDREFS-CHK-LEVEL
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
 
           IF  ORDREFS-PROCESS
               PERFORM ORDREFS-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  ORDREFS-PROCESS
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
               SET NOT-I-70                TO TRUE
           END-IF
           IF  (I-L1)
               SET NOT-I-50                TO TRUE
               SET NOT-I-23                TO TRUE
               SUBTRACT X                  FROM X
           END-IF
           IF  (I-04)
               SET NOT-I-51                TO TRUE
               SET NOT-I-52                TO TRUE
               SET NOT-I-21                TO TRUE
           END-IF
           SET NOT-I-71                    TO TRUE
      *****************************************************************
      * RUTINE FOR Å SJEKKE OM ORDREN ER FERDIGMELDT SERVICE ORDRE.   *
      *****************************************************************
           IF  (I-01)
               SET NOT-I-11                TO TRUE
               IF  BK = 'S'
                   SET I-11                TO TRUE
               END-IF
               SET NOT-I-12                TO TRUE
               IF  FERDIM = '*'
                   SET I-12                TO TRUE
               END-IF
               SET NOT-I-13                TO TRUE
               IF  STATUS-X = 'U'
                   SET I-13                TO TRUE
               END-IF
               SET NOT-I-14                TO TRUE
               IF  OPPDTY = 'F'
                   SET I-14                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-11 AND I-12)
               AND (NOT-I-13)
               SET I-50                    TO TRUE
           END-IF
           IF  (NOT-I-50)
               GO TO SLUTT-T
           END-IF
           IF  (I-01 AND NOT-I-70)
               PERFORM RBSRUT-S
           END-IF
           IF  (I-01 AND NOT-I-70)
               SET I-71                    TO TRUE
           END-IF
           IF  (I-01)
               SET I-70                    TO TRUE
      *****************************************************************
      * RUTINE FOR VARELINJE MED POS.NR. 1 = SERIENR. IDENT.          *
      *****************************************************************
           END-IF
           IF  (NOT-I-04)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-21                    TO TRUE
           IF  POSNR = '001'
               SET I-21                    TO TRUE
           END-IF
           IF  (NOT-I-21)
               GO TO TEKRUT-T
           END-IF
           SET NOT-I-22                    TO TRUE
           IF  SNRIND = 'SERIENR'
               SET I-22                    TO TRUE
           END-IF
           IF  (NOT-I-22)
               SET I-23                    TO TRUE
           END-IF
           IF  (I-23)
               GO TO SLUTT-T
      *****************************************************************
      * RUTINE FOR Å SJEKK OM MASKIN.KORT FINNES PÅ DETTE SERIENR.    *
      *****************************************************************
           END-IF
           MOVE 'A'                        TO F4 (1:1)
           MOVE FIRMA                      TO F4 (2:3)
           MOVE F4                         TO F7 (1:4)
           MOVE ALF                        TO F7 (5:3)
           MOVE F7                         TO MKKEY (1:7)
           MOVE SNR                        TO MKKEY (8:20)
           MOVE MKKEY                      TO MKMAST-KEY1
           READ MKMAST RECORD KEY IS MKMAST-KEY1
           INVALID KEY
               SET I-30                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-30                TO TRUE
               PERFORM MKMAST-FLDSET
               PERFORM MKMAST-IDSET
           END-READ
           MOVE 0                          TO SSNR2
           IF  (NOT-I-30)
               MOVE SSNR                   TO SSNR2-IO
           END-IF
           ADD 1                           TO SSNR2
           IF  (I-30)
               SET I-51                    TO TRUE
           END-IF
           IF  (NOT-I-30)
               SET I-52                    TO TRUE
           END-IF
           PERFORM DTORUT-S
      *****************************************************************
      *    RUTINE FOR DANNE OPPSLAGSKEY.                              *
      *    1. DANNE OPPSLAGS ARTIKKELNR.                              *
      *****************************************************************
           IF  (NOT-I-30)
               GO TO TEKRUT-T
           END-IF
           MOVE ARTNR                      TO OARTNR
           CALL 'OPPSNR' USING OPFELT-XX-DATA-FIELDS
           MOVE OPFELT                     TO OPNR14 (1:0)
           MOVE FIRMA                      TO F6 (1:3)
           MOVE ALF                        TO F6 (4:3)
           MOVE F6                         TO F7 (1:6)
           MOVE ' '                        TO F7 (7:1)
           MOVE F7                         TO OPKEY (1:7)
           MOVE OPNR14                     TO OPKEY (8:14)
           MOVE OPKEY                      TO OPPSMAS-KEY1
           READ OPPSMAS RECORD KEY IS OPPSMAS-KEY1
           INVALID KEY
               SET I-32                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-32                TO TRUE
               PERFORM OPPSMAS-FLDSET
               PERFORM OPPSMAS-IDSET
           END-READ
      *****************************************************************
      * RUTINE FOR OPPSAMLING AV FRI TEKST I MASKIN.SERVICE.FILE      *
      *****************************************************************
           .
 
       TEKRUT-T.
           IF  (I-21)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-41                    TO TRUE
           IF  EDBNR > 0000000
               SET I-41                    TO TRUE
           END-IF
           IF  (I-41)
               GO TO SLUTT-T
           END-IF
           ADD 1                           TO X
           SET NOT-I-42                    TO TRUE
           IF  X > 10
               SET I-42                    TO TRUE
           END-IF
           IF  (I-42)
               GO TO SLUTT-T
           END-IF
           MOVE TEKST                      TO TXA (X) (1:50)
      *****************************************************************
           .
 
       SLUTT-T.
      *****************************************************************
      * RUTINE FOR Å SJEKKE MASKIN.SERVICE.FILE                       *
      *****************************************************************
           CONTINUE.
 
       DTORUT-S SECTION.
       DTORUT-S-P.
           MOVE 'A'                        TO DATOK
           MOVE ORDATO                     TO DATO6
           CALL 'DATO8SIF' USING DATOER-XX-DATA-FIELDS
           MOVE AMD8                       TO SDATO.
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE ' '                        TO BBEST
           MOVE 'MAK01'                    TO LONR
           MOVE FIRMA                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'MAK010'                   TO LPROG (1:6)
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF.
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1 AND NOT-I-50)
               GO TO ENDL1-T
           END-IF
           IF  (I-L1 AND I-23)
               GO TO ENDL1-T
           END-IF
           IF  (I-L1)
               MOVE MKKEY                  TO F27
               MOVE 'B'                    TO F27 (1:1)
               MOVE F27                    TO MSKEY (1:27)
               MOVE SSNR2                  TO SSNRA
      ** MLLzo
               MOVE '1'                    TO BW-A-2
               MULTIPLY 16                 BY BW-A
               MOVE SSNRA (3:1)            TO BW-B-2
               MULTIPLY 16                 BY BW-B
               MOVE BW-A-1                 TO BW-B-1
               DIVIDE 16                   INTO BW-B
               MOVE BW-B-2                 TO SSNRA (3:1)
               MOVE SSNRA                  TO MSKEY (28:3)
               MOVE MSKEY                  TO MKSERV-KEY1
               READ MKSERV RECORD KEY IS MKSERV-KEY1
               INVALID KEY
                   SET I-31                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-31            TO TRUE
                   PERFORM MKSERV-IDSET
               END-READ
           END-IF.
 
       ENDL1-T.
      *****************************************************************
      *                                                               *
      *     RUTINE FOR Å SNU ORDREDATO TIL ÅR MND DAG OG 4 SIFFERET ÅR
      *****************************************************************
           CONTINUE.
 
       ORDREFS-GET SECTION.
       ORDREFS-GET-P.
           IF  ORDREFS-EOF-OFF
               READ ORDREFS
               AT END
                   SET ORDREFS-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       ORDREFS-FLDSET SECTION.
       ORDREFS-FLDSET-P.
           EVALUATE TRUE
           WHEN ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) = ' '
            AND   ORDREFS-IO-AREA (20:1) = '1' )
               MOVE ORDREFS-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE ORDREFS-IO-AREA (5:6)  TO ORDRE (1:6)
               MOVE ORDREFS-IO-AREA (21:6) TO KUNDNR (1:6)
               MOVE ORDREFS-IO-AREA (92:1) TO BK (1:1)
               MOVE ORDREFS-IO-AREA (96:1) TO OPPDTY (1:1)
               MOVE ORDREFS-IO-AREA (100:1) TO SERVTY (1:1)
               MOVE ORDREFS-IO-AREA (136:6) TO ORDATO-IO
               INSPECT ORDATO-IO REPLACING ALL ' ' BY '0'
               MOVE ORDREFS-IO-AREA (149:1) TO FERDIM (1:1)
               MOVE ORDREFS-IO-AREA (153:4) TO REGKL-IO
               MOVE ORDREFS-IO-AREA (158:4) TO OPDATO-IO
               MOVE ORDREFS-IO-AREA (164:1) TO STATUS-X (1:1)
           WHEN ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) = ' '
            AND   ORDREFS-IO-AREA (20:1) = '2' )
               MOVE ORDREFS-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE ORDREFS-IO-AREA (5:6)  TO ORDRE (1:6)
               MOVE ORDREFS-IO-AREA (97:3) TO MEKNR (1:3)
           WHEN ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) = ' '
            AND   ORDREFS-IO-AREA (20:1) = '3' )
               MOVE ORDREFS-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE ORDREFS-IO-AREA (5:6)  TO ORDRE (1:6)
           WHEN ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) NOT = ' ' )
               MOVE ORDREFS-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE ORDREFS-IO-AREA (5:6)  TO ORDRE (1:6)
               MOVE ORDREFS-IO-AREA (17:3) TO POSNR (1:3)
               MOVE ORDREFS-IO-AREA (34:3) TO ALF (1:3)
               MOVE ORDREFS-IO-AREA (37:50) TO TEKST (1:50)
               MOVE ORDREFS-IO-AREA (37:20) TO ARTNR (1:20)
               MOVE ORDREFS-IO-AREA (57:30) TO VARBET (1:30)
               MOVE ORDREFS-IO-AREA (57:7) TO SNRIND (1:7)
               MOVE ORDREFS-IO-AREA (65:20) TO SNR (1:20)
               MOVE ORDREFS-IO-AREA (87:4) TO EDBNR-IO
               MOVE ORDREFS-IO-AREA (91:3) TO VGR-IO
           END-EVALUATE.
 
       ORDREFS-IDSET SECTION.
       ORDREFS-IDSET-P.
           EVALUATE TRUE
           WHEN ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) = ' '
            AND   ORDREFS-IO-AREA (20:1) = '1' )
               SET I-01                    TO TRUE
           WHEN ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) = ' '
            AND   ORDREFS-IO-AREA (20:1) = '2' )
               SET I-02                    TO TRUE
           WHEN ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) = ' '
            AND   ORDREFS-IO-AREA (20:1) = '3' )
               SET I-03                    TO TRUE
           WHEN ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) NOT = ' ' )
               SET I-04                    TO TRUE
           WHEN  OTHER
               SET I-05                    TO TRUE
           END-EVALUATE.
 
       ORDREFS-CHK-LEVEL SECTION.
       ORDREFS-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) = ' '
            AND   ORDREFS-IO-AREA (20:1) = '1' )
               MOVE LOW-VALUES             TO ORDREFS-LEVEL-01
               MOVE ORDREFS-IO-AREA (2:3)  TO ORDREFS-01-L2-FIRMA
               MOVE ORDREFS-IO-AREA (5:6)  TO ORDREFS-01-L1-ORDRE
               IF  ORDREFS-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDREFS-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  ORDREFS-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDREFS-01-L2         TO THE-PRIOR-L2
               MOVE  ORDREFS-01-L1         TO THE-PRIOR-L1
               SET ORDREFS-LEVEL-INIT      TO TRUE
           WHEN ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) = ' '
            AND   ORDREFS-IO-AREA (20:1) = '2' )
               MOVE LOW-VALUES             TO ORDREFS-LEVEL-02
               MOVE ORDREFS-IO-AREA (2:3)  TO ORDREFS-02-L2-FIRMA
               MOVE ORDREFS-IO-AREA (5:6)  TO ORDREFS-02-L1-ORDRE
               IF  ORDREFS-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDREFS-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  ORDREFS-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDREFS-02-L2         TO THE-PRIOR-L2
               MOVE  ORDREFS-02-L1         TO THE-PRIOR-L1
               SET ORDREFS-LEVEL-INIT      TO TRUE
           WHEN ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) = ' '
            AND   ORDREFS-IO-AREA (20:1) = '3' )
               MOVE LOW-VALUES             TO ORDREFS-LEVEL-03
               MOVE ORDREFS-IO-AREA (2:3)  TO ORDREFS-03-L2-FIRMA
               MOVE ORDREFS-IO-AREA (5:6)  TO ORDREFS-03-L1-ORDRE
               IF  ORDREFS-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDREFS-03-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  ORDREFS-03-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDREFS-03-L2         TO THE-PRIOR-L2
               MOVE  ORDREFS-03-L1         TO THE-PRIOR-L1
               SET ORDREFS-LEVEL-INIT      TO TRUE
           WHEN ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) NOT = ' ' )
               MOVE LOW-VALUES             TO ORDREFS-LEVEL-04
               MOVE ORDREFS-IO-AREA (2:3)  TO ORDREFS-04-L2-FIRMA
               MOVE ORDREFS-IO-AREA (5:6)  TO ORDREFS-04-L1-ORDRE
               IF  ORDREFS-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDREFS-04-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  ORDREFS-04-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDREFS-04-L2         TO THE-PRIOR-L2
               MOVE  ORDREFS-04-L1         TO THE-PRIOR-L1
               SET ORDREFS-LEVEL-INIT      TO TRUE
           WHEN OTHER
               CONTINUE
           END-EVALUATE.
 
       MKMAST-FLDSET SECTION.
       MKMAST-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE MKMAST-IO-AREA (130:3) TO SSNR (1:3)
           END-EVALUATE.
 
       MKMAST-IDSET SECTION.
       MKMAST-IDSET-P.
           SET I-06                        TO TRUE.
 
       MKSERV-IDSET SECTION.
       MKSERV-IDSET-P.
           SET I-07                        TO TRUE.
 
       OPPSMAS-FLDSET SECTION.
       OPPSMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE OPPSMAS-IO-AREA (23:7) TO OEDBNR-IO
               INSPECT OEDBNR-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       OPPSMAS-IDSET SECTION.
       OPPSMAS-IDSET-P.
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
           IF  (I-04 AND I-50 AND I-21)
           AND (NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ORDRE                  TO LISTE-IO-AREA (2:6)
               MOVE KUNDNR                 TO LISTE-IO-AREA (9:6)
               MOVE ORDATO                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (16:8)
               MOVE MEKNR                  TO LISTE-IO-AREA (27:3)
               MOVE SERVTY                 TO LISTE-IO-AREA (34:1)
               MOVE OPPDTY                 TO LISTE-IO-AREA (41:1)
               MOVE ALF                    TO LISTE-IO-AREA (46:3)
               MOVE ARTNR                  TO LISTE-IO-AREA (50:20)
               MOVE VARBET                 TO LISTE-IO-AREA (71:30)
               IF  (I-23)
                   MOVE 'FEIL. 1 VARELINJE UKJENT' TO LISTE-IO-AREA
                                                              (103:24)
               END-IF
               IF  (I-51)
                   MOVE 'MASKINKORT LAGT INN     ' TO LISTE-IO-AREA
                                                              (103:24)
               END-IF
               IF  (I-52)
                   MOVE 'MASKINKORT FUNNET       ' TO LISTE-IO-AREA
                                                              (103:24)
               END-IF
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-04 AND I-52 AND NOT-I-23)
               MOVE SSNR2-IO               TO MKMAST-IO-AREA (130:3)
               IF  (I-14)
                   MOVE OPPDTY             TO MKMAST-IO-AREA (134:1)
      *****************************************************************
      * DANNE NYE REC. MASKIN.KORT.FILE OM DETTE IKKE FINNES.         *
      *****************************************************************
               END-IF
               REWRITE MKMAST-IO-AREA
                   INVALID KEY
                       DISPLAY 'Bad REWRITE - file = MKMAST'
               END-REWRITE
           END-IF
           IF  (I-04 AND I-51 AND NOT-I-23)
               MOVE MKKEY                  TO MKMAST-IO-AREA (1:27)
               MOVE FIRMA                  TO MKMAST-IO-AREA (28:3)
               IF  (NOT-I-32)
                   MOVE OEDBNR-IO          TO MKMAST-IO-AREA (31:7)
               END-IF
               IF  (I-32)
                   MOVE '0000000'          TO MKMAST-IO-AREA (31:7)
               END-IF
               MOVE SDATO                  TO MKMAST-IO-AREA (38:8)
               MOVE FIRMA                  TO MKMAST-IO-AREA (46:3)
               MOVE KUNDNR                 TO MKMAST-IO-AREA (49:6)
               MOVE SDATO                  TO MKMAST-IO-AREA (55:8)
               MOVE '000000'               TO MKMAST-IO-AREA (63:6)
               MOVE 'S'                    TO MKMAST-IO-AREA (69:1)
               MOVE SSNR2-IO               TO MKMAST-IO-AREA (130:3)
               IF  (I-14)
                   MOVE OPPDTY             TO MKMAST-IO-AREA (134:1)
      *****************************************************************
      * DANNE NYE REC. MASKIN.KORT.FILE                               *
      *****************************************************************
               END-IF
               WRITE MKMAST-IO-AREA
                   INVALID KEY
                       DISPLAY 'Bad WRITE - file = MKMAST'
               END-WRITE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-01 AND I-50 AND I-71)
           AND (NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE '* * *  DAGENS FERDIGMELD' TO LISTE-IO-AREA (32:24)
               MOVE 'TE MASKIN-SERVICE ORDRE' TO LISTE-IO-AREA (56:23)
               MOVE '* * *'                TO LISTE-IO-AREA (90:5)
               MOVE 'KJØREDATO'            TO LISTE-IO-AREA (100:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (111:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (125:4)
               IF  (I-L2)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40U
               MOVE XO-40U (1:4)           TO LISTE-IO-AREA (129:4)
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
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ORDRE'                TO LISTE-IO-AREA (2:5)
               MOVE 'KUNDE'                TO LISTE-IO-AREA (9:5)
               MOVE 'ORD.DATO'             TO LISTE-IO-AREA (16:8)
               MOVE 'MEKNR'                TO LISTE-IO-AREA (25:5)
               MOVE 'S.TYPE'               TO LISTE-IO-AREA (32:6)
               MOVE 'O.TYPE'               TO LISTE-IO-AREA (39:6)
               MOVE 'ALF'                  TO LISTE-IO-AREA (46:3)
               MOVE 'ARTIKKELNUMMER'       TO LISTE-IO-AREA (50:14)
               MOVE 'SERIENR.FELT'         TO LISTE-IO-AREA (71:12)
               MOVE 'FEILMELDING OG MERKNADER' TO LISTE-IO-AREA
                                                              (103:24)
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
           IF  (I-50 AND I-OF AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE '* * *  DAGENS FERDIGMELD' TO LISTE-IO-AREA (32:24)
               MOVE 'TE MASKIN-SERVICE ORDRE' TO LISTE-IO-AREA (56:23)
               MOVE '* * *'                TO LISTE-IO-AREA (90:5)
               MOVE 'KJØREDATO'            TO LISTE-IO-AREA (100:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (111:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (125:4)
               IF  (I-L2)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40U
               MOVE XO-40U (1:4)           TO LISTE-IO-AREA (129:4)
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
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ORDRE'                TO LISTE-IO-AREA (2:5)
               MOVE 'KUNDE'                TO LISTE-IO-AREA (9:5)
               MOVE 'ORD.DATO'             TO LISTE-IO-AREA (16:8)
               MOVE 'MEKNR'                TO LISTE-IO-AREA (25:5)
               MOVE 'S.TYPE'               TO LISTE-IO-AREA (32:6)
               MOVE 'O.TYPE'               TO LISTE-IO-AREA (39:6)
               MOVE 'ALF'                  TO LISTE-IO-AREA (46:3)
               MOVE 'ARTIKKELNUMMER'       TO LISTE-IO-AREA (50:14)
               MOVE 'SERIENR.FELT'         TO LISTE-IO-AREA (71:12)
               MOVE 'FEILMELDING OG MERKNADER' TO LISTE-IO-AREA
                                                              (103:24)
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
           IF  (I-L1 AND I-50 AND NOT-I-23)
           AND (NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'SERVICE SEQ.NR'       TO LISTE-IO-AREA (6:14)
               MOVE SSNRA                  TO LISTE-IO-AREA (21:3)
               MOVE TXA (1)                TO LISTE-IO-AREA (31:70)
               IF  (I-31)
                   MOVE 'SERVICE RECORD LAGT INN ' TO LISTE-IO-AREA
                                                              (103:24)
               END-IF
               IF  (NOT-I-31)
                   MOVE 'SERVICE RECORD FINNES.  ' TO LISTE-IO-AREA
                                                              (103:24)
      *****************************************************************
      * OPPDATERING AV MASKIN.KORT.FILE                               *
      *****************************************************************
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L1 AND I-50 AND NOT-I-23)
           AND (I-31)
               MOVE MSKEY                  TO MKSERV-IO-AREA (1:30)
               MOVE SDATO                  TO MKSERV-IO-AREA (31:8)
               MOVE ORDRE                  TO MKSERV-IO-AREA (39:6)
               MOVE OPPDTY                 TO MKSERV-IO-AREA (45:1)
               MOVE SERVTY                 TO MKSERV-IO-AREA (46:1)
               MOVE MEKNR                  TO MKSERV-IO-AREA (47:3)
               MOVE 771                    TO BW-A
               PERFORM VARYING TXA-I FROM TXA-MAX BY -1
                         UNTIL TXA-I < 1
                   SUBTRACT 70           FROM BW-A
                   MOVE TXA-ENTRY (TXA-I)  TO MKSERV-IO-AREA (BW-A:70)
                   INITIALIZE TXA-ENTRY (TXA-I)
               END-PERFORM
               WRITE MKSERV-IO-AREA
                   INVALID KEY
                       DISPLAY 'Bad WRITE - file = MKSERV'
               END-WRITE
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
           SET ORDREFS-LEVEL-INIT          TO TRUE
           INITIALIZE ORDREFS-DATA-FIELDS
           SET ORDREFS-EOF-OFF             TO TRUE
           SET ORDREFS-PROCESS             TO TRUE
           OPEN INPUT ORDREFS
           INITIALIZE MKMAST-DATA-FIELDS
           OPEN I-O MKMAST
           OPEN INPUT MKSERV
           INITIALIZE OPPSMAS-DATA-FIELDS
           OPEN INPUT OPPSMAS
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           PERFORM VARYING TXA-I FROM 1 BY 1
                     UNTIL TXA-I > TXA-MAX
               INITIALIZE TXA (TXA-I)
           END-PERFORM
           SET TXA-I                       TO 1
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE ORDREFS
           CLOSE MKMAST
           CLOSE MKSERV
           CLOSE OPPSMAS
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
