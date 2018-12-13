       IDENTIFICATION DIVISION.
       PROGRAM-ID. BBS052R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAM: BBS052                          XX2000XXIRXXMT       *
      * KONVERTERING OG KONTROLLISTER INNBETALING FRA                 *
      * BBS.  HENTER BIL.NR FRA FIRMAF.                               *
      * E 30.05.95 ENDRET TYPE-TEKST FRA INKASSO TIL INNBET           *
      * E 04.06.96 TAKLER FAKTURA-KID (7-SIFRET)                      *
      * E 10.06.96 LEGGER UT PERIODE FOR FAKTURA-KID                  *
      * E 26.08.99 LEGGER UT * I KUNDENR VED FEIL I FAKT.-KID.        *
      * E 15.08.00 TAR MED AUTOGIRO-POSTER.                           *
      * E 12.12.01 BYTTET FAKTKID MED RELMAST.                        *
      * E 17.12.01 LESER RELMAST MED SEQ 001 FOR BET.SPES.            *
      * E 24.01.02 LEGGER UT REFNR, IKKE PERIODE, FOR AUTOGIRO MED 0  *
      *            I FØRSTE SIFFER.                                   *
      * E 15.10.02 SKRIVER UT BANKKONTO NÅR KUNDENR ER UKJENT.        *
      * E 06.02.03 SKRIVER UT RESKNR FOR BANKKONTO NÅR KNR UKJENT.    *
      * E 27.04.04 KORTET INN PÅ TEKST FØR BANKKONTO.                 *
      * E 14.09.05 TAKLER NEGATIVE BELØP.                             *
      * E 06.10.06 UTVIDET RESKBNK FRA 20 TIL 30 BYTE                 *
      * E 16.10.06 LEGGER UT BANKKONTO TIL BETALER I ANMERKNING       *
      * E 24.05.07 STARTER UTSKRIFT PÅ LINJE 3 PGA HULL TIL RINGPERM  *
      * E 25.03.09 TAKLER PREFIKS 962/963 FRA NY PURRERUTINE.         *
      * E 02.09.10 RETTET LENGDE PÅ SUMBEL I 88-REC                 MT*
      * E 22.11.11 LEGGER UT ADVARSEL OM Å SJEKKE BANK NÅR RELASJON MT*
      *            RESKNR/BANKKONTO HENTES FRA RESKBNK.               *
      * E 09.05.12 FORBEDRET MELDING FR 22.11.11                    MT*
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: BBS052.rpg
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
           SELECT BBSREC
               ASSIGN TO UT-S-BBSREC
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS BBSREC-STATUS.
           SELECT KUNDEMA
               ASSIGN TO KUNDEMA
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMA-STATUS
               RECORD KEY IS KUNDEMA-KEY1.
           SELECT RELMAST
               ASSIGN TO RELMAST
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS RELMAST-STATUS
               RECORD KEY IS RELMAST-KEY1.
           SELECT RESKBNK
               ASSIGN TO RESKBNK
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS RESKBNK-STATUS
               RECORD KEY IS RESKBNK-KEY1.
           SELECT LISTE2
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE2-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD BBSREC
               BLOCK CONTAINS 8000
               RECORD CONTAINS 80.
       01  BBSREC-IO-AREA.
           05  BBSREC-IO-AREA-X            PICTURE X(80).
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1            PICTURE X(9).
               10  FILLER                  PICTURE X(190).
       FD RELMAST
               RECORD CONTAINS 80.
       01  RELMAST-IO-AREA.
           05  RELMAST-IO-AREA-X.
               10  RELMAST-KEY1            PICTURE X(20).
               10  FILLER                  PICTURE X(60).
       FD RESKBNK
               RECORD CONTAINS 30.
       01  RESKBNK-IO-AREA.
           05  RESKBNK-IO-AREA-X.
               10  RESKBNK-KEY1            PICTURE X(14).
               10  FILLER                  PICTURE X(16).
      *BUGFILO O   F  80  80            PRINTERSYSLST
       FD LISTE2
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE2-IO-PRINT.
           05  LISTE2-IO-AREA-CONTROL      PICTURE X VALUE ' '.
        02 LISTE2-IO-AREA.
           05  LISTE2-IO-AREA-X            PICTURE X(132).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  BBSREC-STATUS               PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  RELMAST-STATUS              PICTURE 99 VALUE 0.
           10  RESKBNK-STATUS              PICTURE 99 VALUE 0.
           10  LISTE2-STATUS               PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  BBSREC-EOF-OFF          VALUE '0'.
               88  BBSREC-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BBSREC-READ-OFF         VALUE '0'.
               88  BBSREC-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BBSREC-PROCESS-OFF      VALUE '0'.
               88  BBSREC-PROCESS          VALUE '1'.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  RELMAST-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  RESKBNK-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  LISTE2-DATA-FIELDS.
               10  LISTE2-AFTER-SPACE      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE2-AFTER-SKIP       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE2-BEFORE-SPACE     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE2-BEFORE-SKIP      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE2-MAX-LINES        PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE2-LINE-COUNT       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE2-CLR-IO           PICTURE X VALUE 'Y'.
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
           05  BBSREC-DATA-FIELDS.
               10  REC020                  PICTURE X(80).
               10  FORTJE                  PICTURE X(4).
               10  BBSKTO                  PICTURE X(11).
               10  FIRMA                   PICTURE X(3).
               10  REC030                  PICTURE X(80).
               10  RTF1                    PICTURE X(5).
               10  RTF2                    PICTURE X(6).
               10  KNR                     PICTURE X(6).
               10  BILNR                   PICTURE X(6).
               10  INKPUR                  PICTURE X(2).
               10  AGREF1                  PICTURE X(1).
               10  AGREF6                  PICTURE X(5).
               10  PERI                    PICTURE X(2).
               10  AAR                     PICTURE X(1).
               10  FAKID                   PICTURE X(1).
               10  FKREF1                  PICTURE X(1).
               10  FKREF                   PICTURE X(6).
               10  TEGN                    PICTURE X(1).
               10  BEL-IO.
                   15  BEL                 PICTURE S9(7)V9(2).
               10  REC031                  PICTURE X(80).
               10  BNKKTO                  PICTURE X(11).
               10  REC088                  PICTURE X(80).
               10  BBSDAT                  PICTURE X(6).
               10  SUMBEL-IO.
                   15  SUMBEL              PICTURE S9(13)V9(2).
               10  SUMANT-IO.
                   15  SUMANT              PICTURE S9(8).
           05  KUNDEMA-DATA-FIELDS.
               10  KUNAVN                  PICTURE X(30).
           05  RELMAST-DATA-FIELDS.
               10  RELKNR                  PICTURE X(6).
           05  RESKBNK-DATA-FIELDS.
               10  RBKNR                   PICTURE X(6).
      *
           05  TEMPORARY-FIELDS.
               10  FFNR                    PICTURE X(3).
               10  FBILNR                  PICTURE X(6).
               10  RELKY4                  PICTURE X(4).
               10  RELKEY                  PICTURE X(20).
               10  RELK16                  PICTURE X(16).
               10  RESKEY                  PICTURE X(9).
               10  WBEL-IO.
                   15  WBEL                PICTURE S9(7)V9(2).
               10  TBEL-IO.
                   15  TBEL                PICTURE S9(7)V9(2).
               10  TANT-IO.
                   15  TANT                PICTURE S9(5).
               10  RBKEY                   PICTURE X(14).
           05  EDITTING-FIELDS.
               10  XO-72YY9R               PICTURE Z.ZZZ.ZZZ,99-.
               10  XO-50YY9R               PICTURE ZZ.ZZ9-.
               10  XO-132YY9R              PICTURE
                                                Z.ZZZ.ZZZ.ZZZ.ZZZ,99-.
               10  XO-80YY9R               PICTURE ZZ.ZZZ.ZZ9-.
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
               88  NOT-SET-I-OV            VALUE '0'.
               88  SET-I-OV                VALUE '1'.
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
           IF  NOT-SET-I-OV
               SET NOT-I-OV                TO TRUE
           END-IF
           SET NOT-SET-I-OV                TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-03                    TO TRUE
           SET NOT-I-04                    TO TRUE
           SET NOT-I-06                    TO TRUE
           SET NOT-I-05                    TO TRUE
           SET NOT-I-09                    TO TRUE
           SET NOT-I-10                    TO TRUE
           SET NOT-I-07                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  BBSREC-PROCESS
               SET BBSREC-PROCESS-OFF      TO TRUE
               SET BBSREC-READ             TO TRUE
           END-IF
 
           IF  BBSREC-READ
           AND RECORD-SELECTED-OFF
               PERFORM BBSREC-GET
               SET BBSREC-READ-OFF         TO TRUE
               IF  NOT BBSREC-EOF
                   PERFORM BBSREC-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET BBSREC-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  BBSREC-PROCESS
               PERFORM BBSREC-IDSET
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
 
           IF  BBSREC-PROCESS
               PERFORM BBSREC-FLDOFF
               PERFORM BBSREC-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           SET NOT-I-1ST                   TO TRUE
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-74                    TO TRUE
           SET NOT-I-77                    TO TRUE
      *  03                MOVE "REC020  "BUGFL2  8        LEDETXT DEBUG
      *  03      BUGFL2    DEBUGBUGFILO   REC020           VIS FELT/IND
           IF  (I-03)
               SET NOT-I-73                TO TRUE
               IF  FORTJE = 'NY01'
                   SET I-73                TO TRUE
               END-IF
               SET NOT-I-72                TO TRUE
               IF  FIRMA NOT = FFNR
                   SET I-72                TO TRUE
               END-IF
               MOVE FIRMA                  TO FFNR
           END-IF
           IF  (I-03 AND I-72)
               PERFORM RBSRUT-S
           END-IF
           IF  (I-03 AND I-72)
               MOVE 'ZZZZZZ'               TO FBILNR
               SET NOT-I-71                TO TRUE
      *  04                MOVE "REC030  "BUGFL2  8        LEDETXT DEBUG
      *  04      BUGFL2    DEBUGBUGFILO   REC030           VIS FELT/IND
           END-IF
           IF  (I-04)
               SET NOT-I-70                TO TRUE
               IF  BILNR NOT = FBILNR
                   SET I-70                TO TRUE
               END-IF
               MOVE BILNR                  TO FBILNR
           END-IF
           IF  (I-04 AND I-70 AND I-71)
               PERFORM EXCEPTION-OUTPUT
           END-IF
           IF  (I-04)
               SET I-71                    TO TRUE
               SET NOT-I-32                TO TRUE
               SET NOT-I-33                TO TRUE
      *
           END-IF
           IF  (I-04)
               SET NOT-I-32                TO TRUE
               IF  FAKID = 'F'
                   SET I-32                TO TRUE
               END-IF
      *  04                MOVE "FAKID   "BUGFL2  8        LEDETXT DEBUG
      *  04      BUGFL2    DEBUGBUGFILO   FAKID            VIS FELT/IND
           END-IF
           IF  (I-04 AND I-32)
               SET NOT-I-33                TO TRUE
               IF  FKREF1 = '0'
                   SET I-33                TO TRUE
               END-IF
               MOVE 'A'                    TO RELKY4 (1:1)
               MOVE FIRMA                  TO RELKY4 (2:3)
               MOVE RELKY4                 TO RELKEY (1:4)
               MOVE FKREF                  TO RELK16 (1:6)
               MOVE '000'                  TO RELK16 (14:3)
           END-IF
           IF  (I-04 AND I-33)
               MOVE '001'                  TO RELK16 (14:3)
           END-IF
           IF  (I-04 AND I-32)
               MOVE RELK16                 TO RELKEY (5:16)
           END-IF
           IF  (I-04)
               MOVE RELKEY                 TO RELMAST-KEY1
               READ RELMAST RECORD KEY IS RELMAST-KEY1
               INVALID KEY
                   SET I-22                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-22            TO TRUE
                   PERFORM RELMAST-FLDSET
                   PERFORM RELMAST-IDSET
               END-READ
      *  04                MOVE "RELKEY  "BUGFL2  8        LEDETXT DEBUG
      *  04      BUGFL2    DEBUGBUGFILO   RELKEY           VIS FELT/IND
           END-IF
           IF  (I-04)
               MOVE FIRMA                  TO RESKEY (1:3)
           END-IF
           IF  (I-04 AND NOT-I-32)
               MOVE KNR                    TO RESKEY (4:6)
           END-IF
           IF  (I-04 AND I-73 AND I-75)
               AND (I-76)
               MOVE RTF2                   TO RESKEY (4:6)
           END-IF
           IF  (I-04 AND I-32 AND NOT-I-22)
               MOVE RELKNR                 TO RESKEY (4:6)
           END-IF
           IF  (I-04 AND I-32 AND I-22)
               MOVE '      '               TO RESKEY (4:6)
           END-IF
           IF  (I-04)
               MOVE RESKEY                 TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-21                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-21            TO TRUE
                   PERFORM KUNDEMA-FLDSET
                   PERFORM KUNDEMA-IDSET
               END-READ
               SET NOT-I-30                TO TRUE
               IF  INKPUR = '60'
                   SET I-30                TO TRUE
               END-IF
               SET NOT-I-31                TO TRUE
               IF  INKPUR = '61'
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND NOT-I-31)
               SET NOT-I-31                TO TRUE
               IF  INKPUR = '62'
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND NOT-I-31)
               SET NOT-I-31                TO TRUE
               IF  INKPUR = '63'
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND NOT-I-73)
               SET NOT-I-77                TO TRUE
               IF  TEGN = '-'
                   SET I-77                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND NOT-I-77)
               MULTIPLY 1 BY BEL       GIVING WBEL
           END-IF
           IF  (I-04 AND I-77)
               MULTIPLY -1 BY BEL      GIVING WBEL
           END-IF
           IF  (I-04)
               ADD WBEL                    TO TBEL
               ADD 1                       TO TANT
           END-IF
           IF  (I-04 AND I-73)
               SET NOT-I-74                TO TRUE
               IF  AGREF1 = '6'
                   SET I-74                TO TRUE
               END-IF
      *  05                MOVE "REC088  "BUGFL2  8        LEDETXT DEBUG
      *  05      BUGFL2    DEBUGBUGFILO   REC088           VIS FELT/IND
      *  06                MOVE "REC031  "BUGFL2  8        LEDETXT DEBUG
      *  06      BUGFL2    DEBUGBUGFILO   REC031           VIS FELT/IND
      *  06 21             MOVELFIRMA     RBKEY  14
      *  06 21             MOVE BNKKTO    RBKEY
           END-IF
           IF  (I-06)
               MOVE FIRMA                  TO RBKEY (1:3)
               MOVE BNKKTO                 TO RBKEY (4:11)
               MOVE RBKEY                  TO RESKBNK-KEY1
               READ RESKBNK RECORD KEY IS RESKBNK-KEY1
               INVALID KEY
                   SET I-23                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-23            TO TRUE
                   PERFORM RESKBNK-FLDSET
                   PERFORM RESKBNK-IDSET
               END-READ
           END-IF
           IF  (I-06 AND NOT-I-23 AND I-32)
               SET NOT-I-78                TO TRUE
               IF  RELKNR = RBKNR
                   SET I-78                TO TRUE
               END-IF
           END-IF
           IF  (I-06 AND NOT-I-23 AND NOT-I-32)
               SET NOT-I-78                TO TRUE
               IF  KNR = RBKNR
                   SET I-78                TO TRUE
               END-IF
      *  06                MOVE "RBKEY   "BUGFL2  8        LEDETXT DEBUG
      *  06      BUGFL2    DEBUGBUGFILO   RBKEY            VIS FELT/IND
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           END-IF
           .
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE ' '                        TO BBEST
           MOVE 'BBS10'                    TO LONR
           MOVE FIRMA                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'BBS052  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF
           IF  (I-86)
               SET I-35                    TO TRUE
           END-IF.
      ******************************************************
 
       BBSREC-GET SECTION.
       BBSREC-GET-P.
           IF  BBSREC-EOF-OFF
               READ BBSREC
               AT END
                   SET BBSREC-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       BBSREC-FLDOFF SECTION.
       BBSREC-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( BBSREC-IO-AREA (7:1) = '3'
            AND   BBSREC-IO-AREA (8:1) = '0' )
               SET NOT-I-76                TO TRUE
               SET NOT-I-75                TO TRUE
           END-EVALUATE.
 
       BBSREC-FLDSET SECTION.
       BBSREC-FLDSET-P.
           EVALUATE TRUE
           WHEN ( BBSREC-IO-AREA (7:1) = '2'
            AND   BBSREC-IO-AREA (8:1) = '0' )
               MOVE BBSREC-IO-AREA (1:80)  TO REC020 (1:80)
               MOVE BBSREC-IO-AREA (01:4)  TO FORTJE (1:4)
               MOVE BBSREC-IO-AREA (70:11) TO BBSKTO (1:11)
               MOVE BBSREC-IO-AREA (57:3)  TO FIRMA (1:3)
           WHEN ( BBSREC-IO-AREA (7:1) = '3'
            AND   BBSREC-IO-AREA (8:1) = '0' )
               MOVE BBSREC-IO-AREA (1:80)  TO REC030 (1:80)
               MOVE BBSREC-IO-AREA (22:5)  TO RTF1 (1:5)
               IF  RTF1 = SPACES
                   SET I-76                TO TRUE
               END-IF
               MOVE BBSREC-IO-AREA (27:6)  TO RTF2 (1:6)
               MOVE BBSREC-IO-AREA (56:6)  TO KNR (1:6)
               IF  KNR = SPACES
                   SET I-75                TO TRUE
               END-IF
               MOVE BBSREC-IO-AREA (50:6)  TO BILNR (1:6)
               MOVE BBSREC-IO-AREA (62:2)  TO INKPUR (1:2)
               MOVE BBSREC-IO-AREA (62:1)  TO AGREF1 (1:1)
               MOVE BBSREC-IO-AREA (62:5)  TO AGREF6 (1:5)
               MOVE BBSREC-IO-AREA (64:2)  TO PERI (1:2)
               MOVE BBSREC-IO-AREA (66:1)  TO AAR (1:1)
               MOVE BBSREC-IO-AREA (67:1)  TO FAKID (1:1)
               MOVE BBSREC-IO-AREA (68:1)  TO FKREF1 (1:1)
               MOVE BBSREC-IO-AREA (68:6)  TO FKREF (1:6)
               MOVE BBSREC-IO-AREA (32:1)  TO TEGN (1:1)
               MOVE BBSREC-IO-AREA (41:9)  TO BEL-IO
               INSPECT BEL-IO REPLACING ALL ' ' BY '0'
           WHEN ( BBSREC-IO-AREA (7:1) = '3'
            AND   BBSREC-IO-AREA (8:1) = '1' )
               MOVE BBSREC-IO-AREA (1:80)  TO REC031 (1:80)
               MOVE BBSREC-IO-AREA (48:11) TO BNKKTO (1:11)
           WHEN ( BBSREC-IO-AREA (7:1) = '8'
            AND   BBSREC-IO-AREA (8:1) = '8' )
               MOVE BBSREC-IO-AREA (1:80)  TO REC088 (1:80)
               MOVE BBSREC-IO-AREA (42:6)  TO BBSDAT (1:6)
               MOVE BBSREC-IO-AREA (27:15) TO SUMBEL-IO
               INSPECT SUMBEL-IO REPLACING ALL ' ' BY '0'
               MOVE BBSREC-IO-AREA (9:8)   TO SUMANT-IO
               INSPECT SUMANT-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       BBSREC-IDCHK SECTION.
       BBSREC-IDCHK-P.
           EVALUATE TRUE
           WHEN ( BBSREC-IO-AREA (7:1) = '2'
            AND   BBSREC-IO-AREA (8:1) = '0' )
             OR ( BBSREC-IO-AREA (7:1) = '3'
            AND   BBSREC-IO-AREA (8:1) = '0' )
             OR ( BBSREC-IO-AREA (7:1) = '3'
            AND   BBSREC-IO-AREA (8:1) = '1' )
             OR ( BBSREC-IO-AREA (7:1) = '8'
            AND   BBSREC-IO-AREA (8:1) = '8' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       BBSREC-IDSET SECTION.
       BBSREC-IDSET-P.
           EVALUATE TRUE
           WHEN ( BBSREC-IO-AREA (7:1) = '2'
            AND   BBSREC-IO-AREA (8:1) = '0' )
               SET I-03                    TO TRUE
           WHEN ( BBSREC-IO-AREA (7:1) = '3'
            AND   BBSREC-IO-AREA (8:1) = '0' )
               SET I-04                    TO TRUE
           WHEN ( BBSREC-IO-AREA (7:1) = '3'
            AND   BBSREC-IO-AREA (8:1) = '1' )
               SET I-06                    TO TRUE
           WHEN ( BBSREC-IO-AREA (7:1) = '8'
            AND   BBSREC-IO-AREA (8:1) = '8' )
               SET I-05                    TO TRUE
           END-EVALUATE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (16:30) TO KUNAVN (1:30)
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-09                        TO TRUE.
 
       RELMAST-FLDSET SECTION.
       RELMAST-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RELMAST-IO-AREA (21:6) TO RELKNR (1:6)
           END-EVALUATE.
 
       RELMAST-IDSET SECTION.
       RELMAST-IDSET-P.
           SET I-10                        TO TRUE.
 
       RESKBNK-FLDSET SECTION.
       RESKBNK-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RESKBNK-IO-AREA (15:6) TO RBKNR (1:6)
           END-EVALUATE.
 
       RESKBNK-IDSET SECTION.
       RESKBNK-IDSET-P.
           SET I-07                        TO TRUE.
 
       LISTE2-PRINT-LINE SECTION.
       LISTE2-PRINT-LINE-P.
           IF  LISTE2-BEFORE-SKIP > 0
               PERFORM LISTE2-SKIP-BEFORE
           END-IF
           IF  LISTE2-BEFORE-SPACE > 0
               PERFORM LISTE2-SPACE-BEFORE
               IF  LISTE2-AFTER-SKIP > 0
                   PERFORM LISTE2-SKIP-AFTER
               END-IF
               IF  LISTE2-AFTER-SPACE > 0
                   PERFORM LISTE2-SPACE-AFTER
               END-IF
           ELSE
               IF  LISTE2-AFTER-SKIP > 0
                   PERFORM LISTE2-SKIP-AFTER
               END-IF
               PERFORM LISTE2-SPACE-AFTER
           END-IF
           IF  LISTE2-LINE-COUNT NOT < LISTE2-MAX-LINES
               SET I-OV                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OV            TO TRUE
               END-IF
           END-IF.
 
       LISTE2-SKIP-BEFORE SECTION.
       LISTE2-SKIP-BEFORE-P.
           WRITE LISTE2-IO-PRINT        AFTER ADVANCING PAGE
           MOVE 1                          TO LISTE2-LINE-COUNT
           MOVE 0                          TO LISTE2-BEFORE-SKIP
           INITIALIZE LISTE2-IO-AREA.
 
       LISTE2-SPACE-BEFORE SECTION.
       LISTE2-SPACE-BEFORE-P.
           IF LISTE2-BEFORE-SPACE > 0
             WRITE LISTE2-IO-PRINT      AFTER LISTE2-BEFORE-SPACE LINES
             ADD LISTE2-BEFORE-SPACE       TO LISTE2-LINE-COUNT
             MOVE SPACES TO LISTE2-IO-AREA
             INITIALIZE LISTE2-IO-AREA
           END-IF
           MOVE 0                          TO LISTE2-BEFORE-SPACE.
 
       LISTE2-SKIP-AFTER SECTION.
       LISTE2-SKIP-AFTER-P.
           WRITE LISTE2-IO-PRINT       BEFORE ADVANCING PAGE
           MOVE 1                          TO LISTE2-LINE-COUNT
           MOVE 0                          TO LISTE2-AFTER-SKIP
           INITIALIZE LISTE2-IO-AREA.
 
       LISTE2-SPACE-AFTER SECTION.
       LISTE2-SPACE-AFTER-P.
           IF LISTE2-AFTER-SPACE > 0
             WRITE LISTE2-IO-PRINT     BEFORE LISTE2-AFTER-SPACE LINES
             ADD LISTE2-AFTER-SPACE        TO LISTE2-LINE-COUNT
             INITIALIZE LISTE2-IO-AREA
           END-IF
           MOVE 0                          TO LISTE2-AFTER-SPACE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-04)
               IF LISTE2-BEFORE-SPACE > 0
               OR LISTE2-BEFORE-SKIP > 0
                 PERFORM LISTE2-PRINT-LINE
               END-IF
               IF  (I-73 AND I-75)
                   MOVE '*'                TO LISTE2-IO-AREA (10:1)
               END-IF
               MOVE KNR                    TO LISTE2-IO-AREA (12:6)
               IF  (I-32)
                   MOVE RELKNR             TO LISTE2-IO-AREA (12:6)
               END-IF
               IF  (I-73 AND I-75 AND NOT-I-76)
                   MOVE RTF1               TO LISTE2-IO-AREA (7:5)
               END-IF
               IF  (I-73 AND I-75 AND NOT-I-76)
                   MOVE RTF2               TO LISTE2-IO-AREA (12:6)
               END-IF
               IF  (I-73 AND I-75 AND I-76)
                   MOVE RTF2               TO LISTE2-IO-AREA (12:6)
               END-IF
               IF  (I-32 AND I-21)
                   MOVE '******'           TO LISTE2-IO-AREA (12:6)
               END-IF
               IF  (NOT-I-21)
                   MOVE KUNAVN             TO LISTE2-IO-AREA (21:30)
               END-IF
               IF  (I-21)
                   MOVE '** KUNDENR. UKJENT  ** ' TO LISTE2-IO-AREA
                                                               (21:23)
               END-IF
               IF  (I-73 AND I-75 AND NOT-I-76)
                   MOVE '** FEIL KID FOR BANKKON' TO LISTE2-IO-AREA
                                                               (21:23)
               END-IF
               IF  (I-73 AND I-75 AND NOT-I-76)
                   MOVE 'TO **  '          TO LISTE2-IO-AREA (44:7)
               END-IF
               IF  (I-30)
                   MOVE 'INNBET.'          TO LISTE2-IO-AREA (53:7)
               END-IF
               IF  (I-31)
                   MOVE 'PURRING'          TO LISTE2-IO-AREA (53:7)
               END-IF
               IF  (I-73)
                   MOVE 'AUTOGIRO'         TO LISTE2-IO-AREA (52:8)
               END-IF
               IF  (I-32 AND I-21)
                   MOVE '** KID UKJENT, REF.NR: ' TO LISTE2-IO-AREA
                                                               (21:23)
               END-IF
               IF  (I-32 AND I-21)
                   MOVE FKREF              TO LISTE2-IO-AREA (45:6)
               END-IF
               IF  (I-32)
                   MOVE 'FAKTURA'          TO LISTE2-IO-AREA (53:7)
               END-IF
               IF  (I-33)
                   MOVE 'BETSPES'          TO LISTE2-IO-AREA (53:7)
               END-IF
               IF  (NOT-I-32)
                   MOVE PERI               TO LISTE2-IO-AREA (64:2)
               END-IF
               IF  (NOT-I-32)
                   MOVE '.'                TO LISTE2-IO-AREA (66:1)
               END-IF
               IF  (NOT-I-32)
                   MOVE AAR                TO LISTE2-IO-AREA (67:1)
               END-IF
               IF  (I-32)
                   MOVE FKREF              TO LISTE2-IO-AREA (62:6)
               END-IF
               IF  (I-73 AND NOT-I-74 AND NOT-I-75)
                   MOVE '0'                TO LISTE2-IO-AREA (62:1)
               END-IF
               IF  (I-73 AND NOT-I-74 AND NOT-I-75)
                   MOVE AGREF6             TO LISTE2-IO-AREA (63:5)
               END-IF
               IF  (I-73 AND I-75)
                   MOVE 'UKJENT'           TO LISTE2-IO-AREA (62:6)
               END-IF
               MOVE WBEL                   TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE2-IO-AREA (71:13)
               MOVE 'FRA BANKKONTO:'       TO LISTE2-IO-AREA (90:14)
               MOVE 1                      TO LISTE2-BEFORE-SPACE
               IF LISTE2-AFTER-SPACE > 0
               OR I-LR
                 PERFORM LISTE2-PRINT-LINE
               END-IF
           END-IF
           IF  (I-06)
               IF  I-OV
                   PERFORM HEADING-OVERFLOW
                   SET NOT-I-OV            TO TRUE
               END-IF
               MOVE BNKKTO                 TO LISTE2-IO-AREA (111:11)
               PERFORM LISTE2-PRINT-LINE
           END-IF
           IF  (I-06 AND I-21 AND NOT-I-23)
           AND (NOT-I-73)
           OR  (I-06 AND NOT-I-23 AND NOT-I-78)
           AND (NOT-I-73)
               IF LISTE2-BEFORE-SPACE > 0
               OR LISTE2-BEFORE-SKIP > 0
                 PERFORM LISTE2-PRINT-LINE
               END-IF
               MOVE '??????'               TO LISTE2-IO-AREA (12:6)
               MOVE 'BANKKONTO TILHØRER RESK.' TO LISTE2-IO-AREA
                                                               (90:24)
               MOVE 'NR: '                 TO LISTE2-IO-AREA (114:4)
               MOVE RBKNR                  TO LISTE2-IO-AREA (118:6)
               MOVE '? SJEKK!'             TO LISTE2-IO-AREA (124:8)
      *       D 1      06N23N78
      *      AND      N73
      *                                 113 "BANKKONTO TILHØRER RESK."
      *                                 117 "NR: "
      *                        RBKNR    123
      *                                 131 "? SJEKK!"
               MOVE 1                      TO LISTE2-BEFORE-SPACE
               IF LISTE2-AFTER-SPACE > 0
               OR I-LR
                 PERFORM LISTE2-PRINT-LINE
               END-IF
           END-IF
           IF  (I-05)
               IF LISTE2-BEFORE-SPACE > 0
               OR LISTE2-BEFORE-SKIP > 0
                 PERFORM LISTE2-PRINT-LINE
               END-IF
               MOVE 'TOTALT FOR DELAVREGNING' TO LISTE2-IO-AREA (1:23)
               MOVE TBEL                   TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE2-IO-AREA (83:13)
               INITIALIZE TBEL
               MOVE TANT                   TO XO-50YY9R
               MOVE XO-50YY9R              TO LISTE2-IO-AREA (101:7)
               INITIALIZE TANT
               MOVE 2                      TO LISTE2-BEFORE-SPACE
               MOVE 1                      TO LISTE2-AFTER-SPACE
               IF LISTE2-AFTER-SPACE > 0
               OR I-LR
                 PERFORM LISTE2-PRINT-LINE
               END-IF
               IF LISTE2-BEFORE-SPACE > 0
               OR LISTE2-BEFORE-SKIP > 0
                 PERFORM LISTE2-PRINT-LINE
               END-IF
               MOVE 'TOTALT FOR FIRMA '    TO LISTE2-IO-AREA (1:17)
               MOVE FIRMA                  TO LISTE2-IO-AREA (18:3)
               MOVE ' BBSDATO '            TO LISTE2-IO-AREA (21:9)
               MOVE BBSDAT                 TO LISTE2-IO-AREA (30:6)
               MOVE SUMBEL                 TO XO-132YY9R
               MOVE XO-132YY9R             TO LISTE2-IO-AREA (75:21)
               MOVE SUMANT                 TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE2-IO-AREA (97:11)
      *****************************************************************
      * DUMMY-LINJE FOR Å LAGE REF. TIL DATAFELT (FJERNE FEILMELDING) *
      *****************************************************************
               MOVE 2                      TO LISTE2-BEFORE-SPACE
               MOVE 1                      TO LISTE2-AFTER-SPACE
               IF LISTE2-AFTER-SPACE > 0
               OR I-LR
                 PERFORM LISTE2-PRINT-LINE
               END-IF
           END-IF.
 
       EXCEPTION-OUTPUT SECTION.
       EXCEPTION-OUTPUT-P.
           IF  (I-04 AND I-70)
               IF LISTE2-BEFORE-SPACE > 0
               OR LISTE2-BEFORE-SKIP > 0
                 PERFORM LISTE2-PRINT-LINE
               END-IF
               MOVE 'TOTALT FOR DELAVREGNING' TO LISTE2-IO-AREA (1:23)
               MOVE TBEL                   TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE2-IO-AREA (83:13)
               INITIALIZE TBEL
               MOVE TANT                   TO XO-50YY9R
               MOVE XO-50YY9R              TO LISTE2-IO-AREA (101:7)
               INITIALIZE TANT
               MOVE 2                      TO LISTE2-BEFORE-SPACE
               MOVE 1                      TO LISTE2-AFTER-SPACE
               IF LISTE2-AFTER-SPACE > 0
               OR I-LR
                 PERFORM LISTE2-PRINT-LINE
               END-IF
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-04 AND I-70)
               IF LISTE2-BEFORE-SPACE > 0
               OR LISTE2-BEFORE-SKIP > 0
                 PERFORM LISTE2-PRINT-LINE
               END-IF
               MOVE FINAVN                 TO LISTE2-IO-AREA (1:30)
               MOVE 'OCR-INNBETALINGER'    TO LISTE2-IO-AREA (33:17)
               IF  (I-73)
                   MOVE 'AUTOGIRO-INNBETALINGER' TO LISTE2-IO-AREA
                                                               (28:22)
               END-IF
               MOVE 'OVERFØRT FRA B.B.S.'  TO LISTE2-IO-AREA (51:19)
               MOVE 'KONTO NR.'            TO LISTE2-IO-AREA (75:9)
               MOVE BBSKTO                 TO LISTE2-IO-AREA (86:11)
               MOVE 'BILAGSNR.'            TO LISTE2-IO-AREA (99:9)
               MOVE BILNR                  TO LISTE2-IO-AREA (110:6)
               MOVE 01                     TO LISTE2-BEFORE-SKIP
               MOVE 3                      TO LISTE2-BEFORE-SPACE
               MOVE 2                      TO LISTE2-AFTER-SPACE
               IF LISTE2-AFTER-SPACE > 0
               OR LISTE2-AFTER-SKIP > 0
               OR I-LR
                 PERFORM LISTE2-PRINT-LINE
               END-IF
               IF LISTE2-BEFORE-SPACE > 0
               OR LISTE2-BEFORE-SKIP > 0
                 PERFORM LISTE2-PRINT-LINE
               END-IF
               MOVE '          KUNDENR'    TO LISTE2-IO-AREA (2:17)
               MOVE 'KUNDENAVN'            TO LISTE2-IO-AREA (21:9)
               MOVE 'TYPE'                 TO LISTE2-IO-AREA (53:4)
               MOVE 'PER/REF'              TO LISTE2-IO-AREA (62:7)
               MOVE 'BELØP'                TO LISTE2-IO-AREA (78:5)
               MOVE 'ANMERKNINGER'         TO LISTE2-IO-AREA (90:12)
               MOVE 1                      TO LISTE2-AFTER-SPACE
               IF LISTE2-AFTER-SPACE > 0
               OR I-LR
                 PERFORM LISTE2-PRINT-LINE
               END-IF
               IF LISTE2-BEFORE-SPACE > 0
               OR LISTE2-BEFORE-SKIP > 0
                 PERFORM LISTE2-PRINT-LINE
               END-IF
               MOVE '------------------------' TO LISTE2-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE2-IO-AREA
                                                               (25:24)
               MOVE '------------------------' TO LISTE2-IO-AREA
                                                               (49:24)
               MOVE '------------------------' TO LISTE2-IO-AREA
                                                               (73:24)
               MOVE '------------------------' TO LISTE2-IO-AREA
                                                               (97:24)
               MOVE '------------'         TO LISTE2-IO-AREA (121:12)
               MOVE 1                      TO LISTE2-AFTER-SPACE
               IF LISTE2-AFTER-SPACE > 0
               OR I-LR
                 PERFORM LISTE2-PRINT-LINE
               END-IF
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OV)
               IF LISTE2-BEFORE-SPACE > 0
               OR LISTE2-BEFORE-SKIP > 0
                 PERFORM LISTE2-PRINT-LINE
               END-IF
               MOVE FINAVN                 TO LISTE2-IO-AREA (1:30)
               MOVE 'OCR-INNBETALINGER'    TO LISTE2-IO-AREA (33:17)
               IF  (I-73)
                   MOVE 'AUTOGIRO-INNBETALINGER' TO LISTE2-IO-AREA
                                                               (28:22)
               END-IF
               MOVE 'OVERFØRT FRA B.B.S.'  TO LISTE2-IO-AREA (51:19)
               MOVE 'KONTO NR.'            TO LISTE2-IO-AREA (75:9)
               MOVE BBSKTO                 TO LISTE2-IO-AREA (86:11)
               MOVE 'BILAGSNR.'            TO LISTE2-IO-AREA (99:9)
               MOVE BILNR                  TO LISTE2-IO-AREA (110:6)
               MOVE 01                     TO LISTE2-BEFORE-SKIP
               MOVE 3                      TO LISTE2-BEFORE-SPACE
               MOVE 2                      TO LISTE2-AFTER-SPACE
               IF LISTE2-AFTER-SPACE > 0
               OR LISTE2-AFTER-SKIP > 0
               OR I-LR
                 PERFORM LISTE2-PRINT-LINE
               END-IF
               IF LISTE2-BEFORE-SPACE > 0
               OR LISTE2-BEFORE-SKIP > 0
                 PERFORM LISTE2-PRINT-LINE
               END-IF
               MOVE '          KUNDENR'    TO LISTE2-IO-AREA (2:17)
               MOVE 'KUNDENAVN'            TO LISTE2-IO-AREA (21:9)
               MOVE 'TYPE'                 TO LISTE2-IO-AREA (53:4)
               MOVE 'PER/REF'              TO LISTE2-IO-AREA (62:7)
               MOVE 'BELØP'                TO LISTE2-IO-AREA (78:5)
               MOVE 'ANMERKNINGER'         TO LISTE2-IO-AREA (90:12)
               MOVE 1                      TO LISTE2-AFTER-SPACE
               IF LISTE2-AFTER-SPACE > 0
               OR I-LR
                 PERFORM LISTE2-PRINT-LINE
               END-IF
               IF LISTE2-BEFORE-SPACE > 0
               OR LISTE2-BEFORE-SKIP > 0
                 PERFORM LISTE2-PRINT-LINE
               END-IF
               MOVE '------------------------' TO LISTE2-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE2-IO-AREA
                                                               (25:24)
               MOVE '------------------------' TO LISTE2-IO-AREA
                                                               (49:24)
               MOVE '------------------------' TO LISTE2-IO-AREA
                                                               (73:24)
               MOVE '------------------------' TO LISTE2-IO-AREA
                                                               (97:24)
               MOVE '------------'         TO LISTE2-IO-AREA (121:12)
               MOVE 1                      TO LISTE2-AFTER-SPACE
               IF LISTE2-AFTER-SPACE > 0
               OR I-LR
                 PERFORM LISTE2-PRINT-LINE
               END-IF
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-U8 AND I-09 AND I-10)
           AND (I-35 AND I-98 AND I-07)
               IF LISTE2-BEFORE-SPACE > 0
               OR LISTE2-BEFORE-SKIP > 0
                 PERFORM LISTE2-PRINT-LINE
               END-IF
               MOVE PSDS                   TO LISTE2-IO-AREA (1:80)
               MOVE R                      TO LISTE2-IO-AREA (73:8)
               MOVE P-IO                   TO LISTE2-IO-AREA (78:3)
               MOVE S-IO                   TO LISTE2-IO-AREA (76:5)
               MOVE LONR                   TO LISTE2-IO-AREA (76:5)
               MOVE LFIRMA                 TO LISTE2-IO-AREA (78:3)
               MOVE LUNDGR                 TO LISTE2-IO-AREA (78:3)
               MOVE LPROG                  TO LISTE2-IO-AREA (73:8)
               MOVE LOPNVN                 TO LISTE2-IO-AREA (46:35)
               MOVE LPRIID                 TO LISTE2-IO-AREA (77:4)
               MOVE BJOBN                  TO LISTE2-IO-AREA (73:8)
               MOVE BBEST                  TO LISTE2-IO-AREA (80:1)
               MOVE BPERS                  TO LISTE2-IO-AREA (51:30)
               MOVE BETTB                  TO LISTE2-IO-AREA (41:40)
               MOVE BFORS                  TO LISTE2-IO-AREA (41:40)
               MOVE BMEMO                  TO LISTE2-IO-AREA (41:40)
               MOVE BANTX-IO               TO LISTE2-IO-AREA (78:3)
               MOVE BPCLAS                 TO LISTE2-IO-AREA (80:1)
               MOVE BPRJE                  TO LISTE2-IO-AREA (78:3)
               MOVE REC020                 TO LISTE2-IO-AREA (1:80)
               MOVE REC030                 TO LISTE2-IO-AREA (1:80)
               MOVE REC031                 TO LISTE2-IO-AREA (1:80)
               MOVE REC088                 TO LISTE2-IO-AREA (1:80)
               MOVE 1                      TO LISTE2-AFTER-SPACE
               IF LISTE2-AFTER-SPACE > 0
               OR I-LR
                 PERFORM LISTE2-PRINT-LINE
               END-IF
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
           INITIALIZE BBSREC-DATA-FIELDS
           SET BBSREC-EOF-OFF              TO TRUE
           SET BBSREC-PROCESS              TO TRUE
           OPEN INPUT BBSREC
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           INITIALIZE RELMAST-DATA-FIELDS
           OPEN INPUT RELMAST
           INITIALIZE RESKBNK-DATA-FIELDS
           OPEN INPUT RESKBNK
           OPEN OUTPUT LISTE2
           INITIALIZE LISTE2-IO-AREA
           INITIALIZE LISTE2-DATA-FIELDS
           MOVE 57                         TO LISTE2-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE BBSREC
           CLOSE KUNDEMA
           CLOSE RELMAST
           CLOSE RESKBNK
           IF LISTE2-IO-AREA NOT = SPACES
             WRITE LISTE2-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE2-IO-AREA
           END-IF
           CLOSE LISTE2.
 
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
