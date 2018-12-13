       IDENTIFICATION DIVISION.
       PROGRAM-ID. RSK415R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM.......: RSK415, AUTOGIRO TRANSMISJON.                *
      *  PROGRAMMERER..: MORTEN TUVRØNNINGEN                          *
      *  KJØRES I JOBB.: FAK45A                                       *
      *  LAGET DATO....: 13.12.01                                     *
      *  ENDRET........: 18.06.02 NY FEILMELDING                      *
      *                  27.01.04 LAGT INN NY TEKST I EGENREF OG      *
      *                           FREMMEDREF FELTENE.                 *
      *                  09.05.05 SKRIVER JCL NÅR U8 IKKE ER PÅ.      *
      *                  16.01.09 LAGT INN ÅR I OPPDRAGSNR (LØPENR)   *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RSK415.rpg
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
           SELECT PARTAB
               ASSIGN TO UT-S-PARTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PARTAB-STATUS.
           SELECT FIRTAB
               ASSIGN TO UT-S-FIRTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FIRTAB-STATUS.
           SELECT AUTOGIR
               ASSIGN TO UT-S-AUTOGIR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS AUTOGIR-STATUS.
           SELECT KUNDEMA
               ASSIGN TO KUNDEMA
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMA-STATUS
               RECORD KEY IS KUNDEMA-KEY1.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT PORDFIL
               ASSIGN TO PORDFIL
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS PORDFIL-STATUS
               RECORD KEY IS PORDFIL-KEY1.
           SELECT BBSFILE
               ASSIGN TO UT-S-BBSFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS BBSFILE-STATUS.
           SELECT LISTE1
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE1-STATUS.
           SELECT LISTE2
               ASSIGN TO UT-S-LISTE2
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE2-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD PARTAB
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  PARTAB-IO-AREA.
           05  PARTAB-IO-AREA-X            PICTURE X(80).
       FD FIRTAB
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  FIRTAB-IO-AREA.
           05  FIRTAB-IO-AREA-X            PICTURE X(80).
       FD AUTOGIR
               BLOCK CONTAINS 400
               RECORD CONTAINS 40.
       01  AUTOGIR-IO-AREA.
           05  AUTOGIR-IO-AREA-X           PICTURE X(40).
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1            PICTURE X(9).
               10  FILLER                  PICTURE X(190).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD PORDFIL
               RECORD CONTAINS 80.
       01  PORDFIL-IO-AREA.
           05  PORDFIL-IO-AREA-X.
               10  PORDFIL-KEY1            PICTURE X(2).
               10  FILLER                  PICTURE X(78).
       FD BBSFILE
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  BBSFILE-IO-AREA.
           05  BBSFILE-IO-AREA-X           PICTURE X(80).
       FD LISTE1
               BLOCK CONTAINS 81
               RECORD CONTAINS 81.
       01  LISTE1-IO-PRINT.
           05  LISTE1-IO-AREA-CONTROL      PICTURE X VALUE ' '.
        02 LISTE1-IO-AREA.
           05  LISTE1-IO-AREA-X            PICTURE X(80).
      *BUGFILO O   F  80  80            PRINTERSYSLST
       FD LISTE2
               BLOCK CONTAINS 81
               RECORD CONTAINS 81.
       01  LISTE2-IO-PRINT.
           05  LISTE2-IO-AREA-CONTROL      PICTURE X VALUE ' '.
        02 LISTE2-IO-AREA.
           05  LISTE2-IO-AREA-X            PICTURE X(80).
       WORKING-STORAGE SECTION.
       77  TABPAR-MAX   VALUE 1            PICTURE 9(4) USAGE BINARY.
       77  TABSNR-MAX   VALUE 1            PICTURE 9(4) USAGE BINARY.
       77  TABFIR-MAX   VALUE 10           PICTURE 9(4) USAGE BINARY.
       77  TABBBS-MAX   VALUE 10           PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  TABPAR-TABLE.
               10  TABPAR-ENTRY
                                           OCCURS 1 TIMES
                                           INDEXED BY TABPAR-I
                                                      TABPAR-S
                                                      TABSNR-I
                                                      TABSNR-S.
                   15  TABPAR              PICTURE X(2).
                   15  TABSNR              PICTURE X(22).
      * **START RBS - DATASTRUKTUR FOR SUB-PROGRAM RBS000 ********
           05  TABFIR-TABLE.
               10  TABFIR-ENTRY
                                           OCCURS 10 TIMES
                                           INDEXED BY TABFIR-I
                                                      TABFIR-S
                                                      TABBBS-I
                                                      TABBBS-S.
                   15  TABFIR              PICTURE X(3).
                   15  TABBBS              PICTURE X(40).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  PARTAB-STATUS               PICTURE 99 VALUE 0.
           10  FIRTAB-STATUS               PICTURE 99 VALUE 0.
           10  AUTOGIR-STATUS              PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  PORDFIL-STATUS              PICTURE 99 VALUE 0.
           10  BBSFILE-STATUS              PICTURE 99 VALUE 0.
           10  LISTE1-STATUS               PICTURE 99 VALUE 0.
           10  LISTE2-STATUS               PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
           10  PASPAR-XX-STATUS            PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  PARTAB-EOF-OFF          VALUE '0'.
               88  PARTAB-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FIRTAB-EOF-OFF          VALUE '0'.
               88  FIRTAB-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  AUTOGIR-EOF-OFF         VALUE '0'.
               88  AUTOGIR-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  AUTOGIR-READ-OFF        VALUE '0'.
               88  AUTOGIR-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  AUTOGIR-PROCESS-OFF     VALUE '0'.
               88  AUTOGIR-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  AUTOGIR-LEVEL-INIT-OFF  VALUE '0'.
               88  AUTOGIR-LEVEL-INIT      VALUE '1'.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  PORDFIL-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  LISTE1-DATA-FIELDS.
               10  LISTE1-AFTER-SPACE      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE1-AFTER-SKIP       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE1-BEFORE-SPACE     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE1-BEFORE-SKIP      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE1-MAX-LINES        PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE1-LINE-COUNT       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE1-CLR-IO           PICTURE X VALUE 'Y'.
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
           05  PASPAR-XX REDEFINES LDATA-XX-DATA-FIELDS.
               10  PDATO                   PICTURE X(6).
               10  FILLER                  PICTURE X(251).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(6).
               10  PENDR                   PICTURE X(1).
               10  FILLER                  PICTURE X(250).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(7).
               10  PPSW                    PICTURE X(8).
               10  FILLER                  PICTURE X(242).
      * * END - RBS - DATASTRUKTUR FOR SUB-PROGRAM RBSH01 ********
      *DSDS: DATA STRUCTURE FIELDS
           05  PASPAR-XX-DATA-FIELDS.
               10  PDATO                   PICTURE X(6).
               10  FILLER                  PICTURE X(9).
           05  FILLER REDEFINES PASPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(6).
               10  PENDR                   PICTURE X(1).
               10  FILLER                  PICTURE X(8).
           05  FILLER REDEFINES PASPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(7).
               10  PPSW                    PICTURE X(8).
      * * END - RBS - DATASTRUKTUR FOR SUB-PROGRAM RBSH01 ********
           05  AUTOGIR-LEVEL-01.
               10  AUTOGIR-01-L3.
                   15  AUTOGIR-01-L3-FNR   PICTURE X(3).
               10  AUTOGIR-01-L2.
                   15  AUTOGIR-01-L2-FORF  PICTURE X(6).
               10  AUTOGIR-01-L1.
                   15  AUTOGIR-01-L1-KNR   PICTURE X(6).
           05  AUTOGIR-DATA-FIELDS.
               10  FNR                     PICTURE X(3).
               10  KNR                     PICTURE X(6).
               10  KEY-X                   PICTURE X(9).
               10  REFNR                   PICTURE X(6).
               10  REFNR5                  PICTURE X(5).
               10  FORF                    PICTURE X(6).
               10  FORAX                   PICTURE X(2).
               10  FORMX                   PICTURE X(2).
               10  FORDX                   PICTURE X(2).
               10  BELO-ELGP-IO.
                   15  BELO-ELGP           PICTURE S9(8)V9(2).
               10  KODE                    PICTURE X(1).
           05  KUNDEMA-DATA-FIELDS.
               10  KNAVN                   PICTURE X(30).
               10  FORKNA                  PICTURE X(10).
               10  KBKTO-IO.
                   15  KBKTO               PICTURE S9(11).
           05  FIRMAF-DATA-FIELDS.
               10  FINA25                  PICTURE X(25).
               10  FIADR                   PICTURE X(30).
               10  FIPNR                   PICTURE X(4).
               10  FIPSTD                  PICTURE X(26).
               10  FBKTO-IO.
                   15  FBKTO               PICTURE S9(11).
           05  PORDFIL-DATA-FIELDS.
               10  BBSDD9-IO.
                   15  BBSDD9              PICTURE S9(2).
               10  BBSMM9-IO.
                   15  BBSMM9              PICTURE S9(2).
               10  BBSAA9-IO.
                   15  BBSAA9              PICTURE S9(2).
               10  BBSDTO-IO.
                   15  BBSDTO              PICTURE S9(6).
               10  BBSPSW                  PICTURE X(8).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(6).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  PARKEY                  PICTURE X(2).
               10  SNDNR                   PICTURE X(3).
               10  DATO                    PICTURE X(6).
               10  AA9-IO.
                   15  AA9                 PICTURE S9(2).
               10  AAR                     PICTURE X(2).
               10  DD                      PICTURE X(2).
               10  DD9-IO.
                   15  DD9                 PICTURE S9(2).
               10  DDMM                    PICTURE X(4).
               10  MM                      PICTURE X(2).
               10  MM9-IO.
                   15  MM9                 PICTURE S9(2).
               10  DAGDAG-IO.
                   15  DAGDAG              PICTURE S9(6).
               10  ANTDAG-IO.
                   15  ANTDAG              PICTURE S9(6).
               10  PSWKEY                  PICTURE X(2).
               10  GMLPSW                  PICTURE X(8).
               10  BBSDAG-IO.
                   15  BBSDAG              PICTURE S9(6).
               10  DIFDAG-IO.
                   15  DIFDAG              PICTURE S9(6).
               10  ANTRCF-IO.
                   15  ANTRCF              PICTURE S9(8).
               10  FEIANT-IO.
                   15  FEIANT              PICTURE S9(6).
               10  FEIBEL-IO.
                   15  FEIBEL              PICTURE S9(10)V9(2).
               10  TRANNR-IO.
                   15  TRANNR              PICTURE S9(7).
               10  ANTRCO-IO.
                   15  ANTRCO              PICTURE S9(8).
               10  SUMBEL-IO.
                   15  SUMBEL              PICTURE S9(13)V9(2).
               10  AVTALE                  PICTURE X(9).
               10  KUNPAR                  PICTURE X(31).
               10  KONAVN                  PICTURE X(30).
               10  REFTYP                  PICTURE X(1).
               10  BELL2-IO.
                   15  BELL2               PICTURE S9(13)V9(2).
               10  TRABEL-IO.
                   15  TRABEL              PICTURE S9(13)V9(2).
               10  FO-ELGRFF               PICTURE X(6).
               10  HJFF                    PICTURE X(4).
               10  SISFF                   PICTURE X(6).
               10  TOOTRA-IO.
                   15  TOOTRA              PICTURE S9(8).
               10  TOFTRA-IO.
                   15  TOFTRA              PICTURE S9(8).
               10  TOTBEL-IO.
                   15  TOTBEL              PICTURE S9(13)V9(2).
               10  NYPSW                   PICTURE X(8).
           05  EDITTING-FIELDS.
               10  XO-40YNZ                PICTURE ZZZZ.
               10  EDIT-FBKTO              PICTURE ZZZZ.ZZ.ZZZZZ.
               10  EDIT-KBKTO              PICTURE ZZZZ.ZZ.ZZZZZ.
               10  XO-82YY9R               PICTURE ZZ.ZZZ.ZZZ,99-.
               10  XO-132YY9R              PICTURE
                                                Z.ZZZ.ZZZ.ZZZ.ZZZ,99-.
               10  XO-80YY9R               PICTURE ZZ.ZZZ.ZZ9-.
               10  XO-60YN9R               PICTURE ZZZZZ9-.
               10  XO-102YY9R              PICTURE Z.ZZZ.ZZZ.ZZZ,99-.
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
           SET NOT-I-05                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-04                    TO TRUE
           SET NOT-I-06                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  AUTOGIR-PROCESS
               SET AUTOGIR-PROCESS-OFF     TO TRUE
               SET AUTOGIR-READ            TO TRUE
           END-IF
 
           IF  AUTOGIR-READ
           AND RECORD-SELECTED-OFF
               PERFORM AUTOGIR-GET
               SET AUTOGIR-READ-OFF        TO TRUE
               IF  NOT AUTOGIR-EOF
                   SET AUTOGIR-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  AUTOGIR-PROCESS
               PERFORM AUTOGIR-IDSET
           END-IF
 
           IF  AUTOGIR-PROCESS
               PERFORM AUTOGIR-CHK-LEVEL
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
 
           IF  AUTOGIR-PROCESS
               PERFORM AUTOGIR-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  AUTOGIR-PROCESS
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
               SET NOT-I-10                TO TRUE
           END-IF
           SET NOT-I-31                    TO TRUE
           SET NOT-I-51                    TO TRUE
           SET NOT-I-52                    TO TRUE
           IF  (NOT-I-34)
               SET I-30                    TO TRUE
           END-IF
           IF  (I-30)
               MOVE 'P1'                   TO PARKEY
               SET NOT-I-32                TO TRUE
               SET TABPAR-S                TO TABPAR-I
               PERFORM WITH TEST AFTER
                       VARYING TABPAR-I FROM 1 BY 1
                         UNTIL TABPAR-I >= TABPAR-MAX
                            OR I-32
                   IF  PARKEY = TABPAR (TABPAR-I)
                       SET I-32            TO TRUE
                       SET TABPAR-S        TO TABPAR-I
                   END-IF
               END-PERFORM
               SET TABPAR-I                TO TABPAR-S
               IF  I-32
               AND TABPAR-I NOT > TABSNR-MAX
                   SET TABSNR-I            TO TABPAR-I
               END-IF
           END-IF
           IF  (I-30 AND NOT-I-32)
               SET I-33                    TO TRUE
           END-IF
           IF  (I-33)
               GO TO SLUTT-T
           END-IF
           IF  (I-30)
               MOVE TABSNR(TABSNR-I) (20:3)  TO SNDNR
               MOVE UDATE                  TO DATO
               MOVE UDATE (5:2)            TO AA9-IO
               MOVE AA9                    TO AAR
               MOVE DATO (1:2)             TO DD
               MOVE DATO (1:2)             TO DD9
               MOVE DATO (1:4)             TO DDMM
               MOVE DDMM (3:2)             TO MM
               MOVE DDMM (3:2)             TO MM9-IO
               SUBTRACT 1                  FROM MM9
               MULTIPLY 360 BY AA9     GIVING DAGDAG
               MULTIPLY 30 BY MM9      GIVING ANTDAG
               ADD ANTDAG                  TO DAGDAG
               ADD DD9                     TO DAGDAG
               MOVE '01'                   TO PSWKEY
               MOVE PSWKEY                 TO PORDFIL-KEY1
               READ PORDFIL RECORD KEY IS PORDFIL-KEY1
               INVALID KEY
                   SET I-22                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-22            TO TRUE
                   PERFORM PORDFIL-FLDSET
                   PERFORM PORDFIL-IDSET
               END-READ
           END-IF
           IF  (I-30 AND I-22)
               SET I-LR                    TO TRUE
               GO TO SLUTT-T
           END-IF
           IF  (I-30)
               MOVE BBSPSW                 TO GMLPSW
               SUBTRACT 1                  FROM BBSMM9
               MULTIPLY 360 BY BBSAA9  GIVING BBSDAG
               MULTIPLY 30 BY BBSMM9   GIVING ANTDAG
               ADD ANTDAG                  TO BBSDAG
               ADD BBSDD9                  TO BBSDAG
               SUBTRACT BBSDAG FROM DAGDAG GIVING DIFDAG
               SET NOT-I-21                TO TRUE
               IF  DIFDAG > 20
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-30 AND I-21)
               PERFORM LAGPAS-S
           END-IF
           IF  (I-30)
               ADD 1                       TO ANTRCF
               SET I-34                    TO TRUE
               SET I-31                    TO TRUE
               SET NOT-I-30                TO TRUE
      *
           END-IF
           IF  (I-05)
               SET NOT-I-51                TO TRUE
               IF  KODE = 'N'
                   SET I-51                TO TRUE
               END-IF
               SET NOT-I-52                TO TRUE
               IF  KODE = 'U'
                   SET I-52                TO TRUE
               END-IF
               ADD 1                       TO FEIANT
               ADD BELO-ELGP               TO FEIBEL
               GO TO SLUTT-T
           END-IF
           IF  (I-L3)
               SET NOT-I-11                TO TRUE
               MOVE 0                      TO TRANNR
               MOVE 0                      TO ANTRCO
               MOVE 0                      TO SUMBEL
               MOVE 0                      TO PAGE0
               SET NOT-I-99                TO TRUE
               SET TABFIR-S                TO TABFIR-I
               PERFORM WITH TEST AFTER
                       VARYING TABFIR-I FROM 1 BY 1
                         UNTIL TABFIR-I >= TABFIR-MAX
                            OR I-99
                   IF  FNR = TABFIR (TABFIR-I)
                       SET I-99            TO TRUE
                       SET TABFIR-S        TO TABFIR-I
                   END-IF
               END-PERFORM
               SET TABFIR-I                TO TABFIR-S
               IF  I-99
               AND TABFIR-I NOT > TABBBS-MAX
                   SET TABBBS-I            TO TABFIR-I
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-99)
               SET I-86                    TO TRUE
           END-IF
           IF  (NOT-I-99)
               GO TO SLUTT-T
           END-IF
           IF  (I-L3)
               MOVE TABBBS(TABBBS-I) (1:9) TO AVTALE
               MOVE TABBBS(TABBBS-I) (10:31)  TO KUNPAR
               MOVE KUNPAR (1:30)          TO KONAVN
               MOVE KUNPAR (31:1)          TO REFTYP
               SET NOT-I-73                TO TRUE
               IF  REFTYP = 'R'
                   SET I-73                TO TRUE
               END-IF
               MOVE FNR                    TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-60                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-60            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
           END-IF
           IF  (I-L3 AND NOT-I-60)
               PERFORM RBSRUT-S
           END-IF
           IF  (I-L3 AND I-60)
               SET I-86                    TO TRUE
           END-IF
           IF  (I-86)
               GO TO SLUTT-T
           END-IF
           IF  (I-L2)
               MOVE 0                      TO BELL2
           END-IF
           IF  (I-L1)
               MOVE KEY-X                  TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-20                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-20            TO TRUE
                   PERFORM KUNDEMA-FLDOFF
                   PERFORM KUNDEMA-FLDSET
                   PERFORM KUNDEMA-IDSET
               END-READ
           END-IF
           IF  (I-L1 AND NOT-I-20)
               SET I-10                    TO TRUE
           END-IF
           IF  (NOT-I-73 AND I-71)
               SET NOT-I-10                TO TRUE
           END-IF
           IF  (I-10)
               ADD BELO-ELGP TO ZERO   GIVING TRABEL
               SET NOT-I-12                TO TRUE
               IF  TRABEL > 0
                   SET I-12                TO TRUE
               END-IF
           END-IF
           IF  (I-10 AND NOT-I-11 AND I-12)
               MOVE FORAX                  TO FO-ELGRFF (5:2)
               MOVE FORDX                  TO HJFF (1:2)
               MOVE FORMX                  TO HJFF (3:2)
               MOVE HJFF                   TO FO-ELGRFF (1:4)
           END-IF
           IF  (I-10 AND I-12)
               ADD 1                       TO TRANNR
               SET NOT-I-11                TO TRUE
               IF  TRANNR > 0
                   SET I-11                TO TRUE
               END-IF
               ADD TRABEL                  TO SUMBEL
               ADD TRABEL                  TO BELL2
           END-IF
           IF  (I-L3 AND I-10 AND I-12)
               ADD 1                       TO ANTRCO
           END-IF
           IF  (I-10 AND I-12)
               ADD 2                       TO ANTRCO
           END-IF
           IF  (I-71 AND NOT-I-73)
               OR  (I-20)
               OR  (NOT-I-12)
               ADD BELO-ELGP               TO FEIBEL
               ADD 1                       TO FEIANT
           END-IF
           IF  (I-01)
               GO TO SLUTT-T
           END-IF.
 
       SLUTT-T.
           CONTINUE.
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE ' '                        TO BBEST
           MOVE 'RES22'                    TO LONR
           MOVE FNR                        TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'RSK410  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF.
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS41E COBOL SUBRUTINE.   *
      ******************************************************
      *R         RBSRU8    BEGSR
      *R                   SETOF                     86     PRINTINDIKATOR.
      *R                   MOVE " "       BBEST             NO OVERSTYRING
      *R                   MOVE "RES22"   LONR              OPPGAVENR.
      *R                   MOVE FNR       LFIRMA            FIRMANR.
      *R                   MOVE "000"     LUNDGR            UNDERGRUPPE.
      *R                   MOVEL"RSK410  "LPROG             PROGRAMNAVN.
      *R                   Z-ADD0         LANTX             RESET ANT.X.
      *R                   MOVE "PRT1"    LPRIID            PRINTERIDENT
      *R                   CALL "ILBDSET0"             98   CALL COBOL -
      *R                   CALL "RBS41E"               98   SUBPROGRAM.
      *R                   PARM           LDATA             LINKDATA
      *R         LANTX     COMP 0                        86 LISTES IKKE.
      *R                   ENDSR
      ****************************************************************
      *** TRANSMISJON TIL BBS                                      ***
      ****************************************************************
      *****************************************************************
      *  RUTINE FOR Å HENTE NYTT PASSORD VED ENDRINGS-TIDSPUNKT.      *
      *****************************************************************
 
       LAGPAS-S SECTION.
       LAGPAS-S-P.
           SET NOT-I-96                    TO TRUE
           MOVE DATO                       TO PDATO
           MOVE '1'                        TO PENDR
           CALL 'ADGENPSW' USING PASPAR-XX-DATA-FIELDS
           MOVE PPSW                       TO NYPSW
      *R                   MOVE "DIFDAG  "BUGFL1  8        DISPLAY FIELD
      *R         BUGFL1    DEBUGBUGFILO   DIFDAG           VIS INDIKATOR
           .
      *** JCL
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L2 AND I-11)
               MOVE FORAX                  TO SISFF (5:2)
               MOVE FORDX                  TO HJFF (1:2)
               MOVE FORMX                  TO HJFF (3:2)
               MOVE HJFF                   TO SISFF (1:4)
           END-IF
           IF  (I-L3 AND I-11)
               ADD TRANNR TO ZERO      GIVING TOOTRA
               ADD TOOTRA                  TO TOFTRA
               ADD 1                       TO ANTRCO
               ADD ANTRCO                  TO ANTRCF
           END-IF
           IF  (I-L3)
               ADD SUMBEL                  TO TOTBEL
           END-IF.
 
       LR-CALCS SECTION.
       LR-CALCS-P.
           ADD 1                           TO ANTRCF
      *0                   MOVE "KNR     "BUGFL1  8        DISPLAY FIELD
      *0         BUGFL1    DEBUGBUGFILO   KNR              VIS INDIKATOR
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           .
 
       AUTOGIR-GET SECTION.
       AUTOGIR-GET-P.
           IF  AUTOGIR-EOF-OFF
               READ AUTOGIR
               AT END
                   SET AUTOGIR-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       AUTOGIR-FLDSET SECTION.
       AUTOGIR-FLDSET-P.
           EVALUATE TRUE
           WHEN ( AUTOGIR-IO-AREA (1:1) = 'A'
            AND   AUTOGIR-IO-AREA (2:1) = '2' )
               MOVE AUTOGIR-IO-AREA (3:3)  TO FNR (1:3)
               MOVE AUTOGIR-IO-AREA (6:6)  TO KNR (1:6)
               MOVE AUTOGIR-IO-AREA (3:9)  TO KEY-X (1:9)
               MOVE AUTOGIR-IO-AREA (12:6) TO REFNR (1:6)
               MOVE AUTOGIR-IO-AREA (13:5) TO REFNR5 (1:5)
               MOVE AUTOGIR-IO-AREA (18:6) TO FORF (1:6)
               MOVE AUTOGIR-IO-AREA (18:2) TO FORAX (1:2)
               MOVE AUTOGIR-IO-AREA (20:2) TO FORMX (1:2)
               MOVE AUTOGIR-IO-AREA (22:2) TO FORDX (1:2)
               MOVE AUTOGIR-IO-AREA (24:10) TO BELO-ELGP-IO
               INSPECT BELO-ELGP-IO REPLACING ALL ' ' BY '0'
           WHEN OTHER
               MOVE AUTOGIR-IO-AREA (3:3)  TO FNR (1:3)
               MOVE AUTOGIR-IO-AREA (6:6)  TO KNR (1:6)
               MOVE AUTOGIR-IO-AREA (3:9)  TO KEY-X (1:9)
               MOVE AUTOGIR-IO-AREA (12:6) TO REFNR (1:6)
               MOVE AUTOGIR-IO-AREA (13:5) TO REFNR5 (1:5)
               MOVE AUTOGIR-IO-AREA (18:6) TO FORF (1:6)
               MOVE AUTOGIR-IO-AREA (18:2) TO FORAX (1:2)
               MOVE AUTOGIR-IO-AREA (20:2) TO FORMX (1:2)
               MOVE AUTOGIR-IO-AREA (22:2) TO FORDX (1:2)
               MOVE AUTOGIR-IO-AREA (24:10) TO BELO-ELGP-IO
               INSPECT BELO-ELGP-IO REPLACING ALL ' ' BY '0'
               MOVE AUTOGIR-IO-AREA (34:1) TO KODE (1:1)
           END-EVALUATE.
 
       AUTOGIR-IDSET SECTION.
       AUTOGIR-IDSET-P.
           EVALUATE TRUE
           WHEN ( AUTOGIR-IO-AREA (1:1) = 'A'
            AND   AUTOGIR-IO-AREA (2:1) = '2' )
               SET I-01                    TO TRUE
           WHEN  OTHER
               SET I-05                    TO TRUE
           END-EVALUATE.
 
       AUTOGIR-CHK-LEVEL SECTION.
       AUTOGIR-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( AUTOGIR-IO-AREA (1:1) = 'A'
            AND   AUTOGIR-IO-AREA (2:1) = '2' )
               MOVE LOW-VALUES             TO AUTOGIR-LEVEL-01
               MOVE AUTOGIR-IO-AREA (3:3)  TO AUTOGIR-01-L3-FNR
               MOVE AUTOGIR-IO-AREA (18:6) TO AUTOGIR-01-L2-FORF
               MOVE AUTOGIR-IO-AREA (6:6)  TO AUTOGIR-01-L1-KNR
               IF  AUTOGIR-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  AUTOGIR-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  AUTOGIR-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  AUTOGIR-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  AUTOGIR-01-L3         TO THE-PRIOR-L3
               MOVE  AUTOGIR-01-L2         TO THE-PRIOR-L2
               MOVE  AUTOGIR-01-L1         TO THE-PRIOR-L1
               SET AUTOGIR-LEVEL-INIT      TO TRUE
           WHEN OTHER
               CONTINUE
           END-EVALUATE.
 
       KUNDEMA-FLDOFF SECTION.
       KUNDEMA-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-71                TO TRUE
           END-EVALUATE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (16:30) TO KNAVN (1:30)
               MOVE KUNDEMA-IO-AREA (16:10) TO FORKNA (1:10)
               MOVE KUNDEMA-IO-AREA (133:11) TO KBKTO-IO
               INSPECT KBKTO-IO REPLACING ALL ' ' BY '0'
               IF  KBKTO = ZERO
                   SET I-71                TO TRUE
               END-IF
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-02                        TO TRUE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (8:30)  TO FINAVN (1:30)
               MOVE FIRMAF-IO-AREA (8:25)  TO FINA25 (1:25)
               MOVE FIRMAF-IO-AREA (504:30) TO FIADR (1:30)
               MOVE FIRMAF-IO-AREA (534:4) TO FIPNR (1:4)
               MOVE FIRMAF-IO-AREA (538:26) TO FIPSTD (1:26)
               MOVE FIRMAF-IO-AREA (564:11) TO FBKTO-IO
               INSPECT FBKTO-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-04                        TO TRUE.
 
       PORDFIL-FLDSET SECTION.
       PORDFIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE PORDFIL-IO-AREA (3:2)  TO BBSDD9-IO
               INSPECT BBSDD9-IO REPLACING ALL ' ' BY '0'
               MOVE PORDFIL-IO-AREA (5:2)  TO BBSMM9-IO
               INSPECT BBSMM9-IO REPLACING ALL ' ' BY '0'
               MOVE PORDFIL-IO-AREA (7:2)  TO BBSAA9-IO
               INSPECT BBSAA9-IO REPLACING ALL ' ' BY '0'
               MOVE PORDFIL-IO-AREA (3:6)  TO BBSDTO-IO
               INSPECT BBSDTO-IO REPLACING ALL ' ' BY '0'
               MOVE PORDFIL-IO-AREA (9:8)  TO BBSPSW (1:8)
           END-EVALUATE.
 
       PORDFIL-IDSET SECTION.
       PORDFIL-IDSET-P.
           SET I-06                        TO TRUE.
 
       LISTE1-PRINT-LINE SECTION.
       LISTE1-PRINT-LINE-P.
           IF  LISTE1-BEFORE-SKIP > 0
               PERFORM LISTE1-SKIP-BEFORE
           END-IF
           IF  LISTE1-BEFORE-SPACE > 0
               PERFORM LISTE1-SPACE-BEFORE
               IF  LISTE1-AFTER-SKIP > 0
                   PERFORM LISTE1-SKIP-AFTER
               END-IF
               IF  LISTE1-AFTER-SPACE > 0
                   PERFORM LISTE1-SPACE-AFTER
               END-IF
           ELSE
               IF  LISTE1-AFTER-SKIP > 0
                   PERFORM LISTE1-SKIP-AFTER
               END-IF
               PERFORM LISTE1-SPACE-AFTER
           END-IF
           IF  LISTE1-LINE-COUNT NOT < LISTE1-MAX-LINES
               SET I-OF                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OF            TO TRUE
               END-IF
           END-IF.
 
       LISTE1-SKIP-BEFORE SECTION.
       LISTE1-SKIP-BEFORE-P.
           WRITE LISTE1-IO-PRINT        AFTER ADVANCING PAGE
           MOVE 1                          TO LISTE1-LINE-COUNT
           MOVE 0                          TO LISTE1-BEFORE-SKIP
           INITIALIZE LISTE1-IO-AREA.
 
       LISTE1-SPACE-BEFORE SECTION.
       LISTE1-SPACE-BEFORE-P.
           WRITE LISTE1-IO-PRINT        AFTER LISTE1-BEFORE-SPACE LINES
           ADD LISTE1-BEFORE-SPACE         TO LISTE1-LINE-COUNT
           MOVE SPACES TO LISTE1-IO-AREA
           INITIALIZE LISTE1-IO-AREA
           MOVE 0                          TO LISTE1-BEFORE-SPACE.
 
       LISTE1-SKIP-AFTER SECTION.
       LISTE1-SKIP-AFTER-P.
           WRITE LISTE1-IO-PRINT       BEFORE ADVANCING PAGE
           MOVE 1                          TO LISTE1-LINE-COUNT
           MOVE 0                          TO LISTE1-AFTER-SKIP
           INITIALIZE LISTE1-IO-AREA.
 
       LISTE1-SPACE-AFTER SECTION.
       LISTE1-SPACE-AFTER-P.
           WRITE LISTE1-IO-PRINT       BEFORE LISTE1-AFTER-SPACE LINES
           ADD LISTE1-AFTER-SPACE          TO LISTE1-LINE-COUNT
           INITIALIZE LISTE1-IO-AREA
           MOVE 0                          TO LISTE1-AFTER-SPACE.
 
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
               MOVE 7                      TO LISTE2-AFTER-SKIP
           END-IF.
 
       LISTE2-SKIP-BEFORE SECTION.
       LISTE2-SKIP-BEFORE-P.
           WRITE LISTE2-IO-PRINT        AFTER ADVANCING PAGE
           MOVE 1                          TO LISTE2-LINE-COUNT
           MOVE 0                          TO LISTE2-BEFORE-SKIP
           INITIALIZE LISTE2-IO-AREA.
 
       LISTE2-SPACE-BEFORE SECTION.
       LISTE2-SPACE-BEFORE-P.
           WRITE LISTE2-IO-PRINT        AFTER LISTE2-BEFORE-SPACE LINES
           ADD LISTE2-BEFORE-SPACE         TO LISTE2-LINE-COUNT
           MOVE SPACES TO LISTE2-IO-AREA
           INITIALIZE LISTE2-IO-AREA
           MOVE 0                          TO LISTE2-BEFORE-SPACE.
 
       LISTE2-SKIP-AFTER SECTION.
       LISTE2-SKIP-AFTER-P.
           WRITE LISTE2-IO-PRINT       BEFORE ADVANCING PAGE
           MOVE 1                          TO LISTE2-LINE-COUNT
           MOVE 0                          TO LISTE2-AFTER-SKIP
           INITIALIZE LISTE2-IO-AREA.
 
       LISTE2-SPACE-AFTER SECTION.
       LISTE2-SPACE-AFTER-P.
           WRITE LISTE2-IO-PRINT       BEFORE LISTE2-AFTER-SPACE LINES
           ADD LISTE2-AFTER-SPACE          TO LISTE2-LINE-COUNT
           INITIALIZE LISTE2-IO-AREA
           MOVE 0                          TO LISTE2-AFTER-SPACE.
 
       PARTAB-LOAD SECTION.
       PARTAB-LOAD-P.
           OPEN INPUT PARTAB
           SET TABPAR-I                    TO 1
           PERFORM UNTIL PARTAB-EOF
               READ PARTAB
               AT END
                   SET PARTAB-EOF          TO TRUE
               NOT AT END
                   MOVE PARTAB-IO-AREA (1:24) TO TABPAR-ENTRY
                                                            (TABPAR-I)
                   SET TABPAR-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE PARTAB.
 
       FIRTAB-LOAD SECTION.
       FIRTAB-LOAD-P.
           OPEN INPUT FIRTAB
           SET TABFIR-I                    TO 1
           PERFORM UNTIL FIRTAB-EOF
               READ FIRTAB
               AT END
                   SET FIRTAB-EOF          TO TRUE
               NOT AT END
                   MOVE FIRTAB-IO-AREA (1:43) TO TABFIR-ENTRY
                                                            (TABFIR-I)
                   SET TABFIR-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE FIRTAB.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-06 AND I-21 AND NOT-I-U8)
               MOVE DATO                   TO PORDFIL-IO-AREA (3:6)
               MOVE NYPSW                  TO PORDFIL-IO-AREA (9:8)
               REWRITE PORDFIL-IO-AREA
                   INVALID KEY
                       DISPLAY 'Bad REWRITE - file = PORDFIL'
               END-REWRITE
           END-IF
           IF  (I-31 AND NOT-I-U7 AND NOT-I-U8)
               MOVE SPACES TO BBSFILE-IO-AREA
               INITIALIZE BBSFILE-IO-AREA
               MOVE '//I0000117  JOB 1,'   TO BBSFILE-IO-AREA (1:18)
               MOVE '''AUTO-DATA'''          TO BBSFILE-IO-AREA (19:11)
               MOVE ',CLASS=T'             TO BBSFILE-IO-AREA (30:8)
               MOVE ',MSGCLASS=X,'         TO BBSFILE-IO-AREA (38:12)
               WRITE BBSFILE-IO-AREA
               MOVE SPACES TO BBSFILE-IO-AREA
               INITIALIZE BBSFILE-IO-AREA
               MOVE '//             '      TO BBSFILE-IO-AREA (1:15)
               MOVE 'USER=I000117'         TO BBSFILE-IO-AREA (16:12)
               IF  (NOT-I-21)
                   MOVE ',PASSWORD=        ' TO BBSFILE-IO-AREA (28:18)
               END-IF
               IF  (NOT-I-21)
                   MOVE GMLPSW             TO BBSFILE-IO-AREA (38:8)
               END-IF
               IF  (I-21)
                   MOVE ',PASSWORD=(        ' TO BBSFILE-IO-AREA
                                                               (28:19)
               END-IF
               IF  (I-21)
                   MOVE GMLPSW             TO BBSFILE-IO-AREA (39:8)
               END-IF
               IF  (I-21)
                   MOVE ','                TO BBSFILE-IO-AREA (47:1)
               END-IF
               IF  (I-21)
                   MOVE NYPSW              TO BBSFILE-IO-AREA (48:8)
               END-IF
               IF  (I-21)
                   MOVE ')'                TO BBSFILE-IO-AREA (56:1)
               END-IF
               WRITE BBSFILE-IO-AREA
               MOVE SPACES TO BBSFILE-IO-AREA
               INITIALIZE BBSFILE-IO-AREA
               MOVE '//PROC     EXEC   '   TO BBSFILE-IO-AREA (1:18)
               MOVE 'DIRDATNY'             TO BBSFILE-IO-AREA (19:8)
               MOVE ',LISTE=A'             TO BBSFILE-IO-AREA (27:8)
               MOVE ',FORMAT=DIRDATAN,'    TO BBSFILE-IO-AREA (35:17)
               WRITE BBSFILE-IO-AREA
               MOVE SPACES TO BBSFILE-IO-AREA
               INITIALIZE BBSFILE-IO-AREA
               MOVE '//             '      TO BBSFILE-IO-AREA (1:15)
               MOVE 'FILE='                TO BBSFILE-IO-AREA (16:5)
               MOVE '''BBSIK7.K0000117.K01''' TO BBSFILE-IO-AREA
                                                               (21:21)
               WRITE BBSFILE-IO-AREA
               MOVE SPACES TO BBSFILE-IO-AREA
               INITIALIZE BBSFILE-IO-AREA
               MOVE '//STEP2.SYS100  DD *' TO BBSFILE-IO-AREA (1:20)
      *** TEST JCL
               WRITE BBSFILE-IO-AREA
           END-IF
           IF  (I-31 AND I-U7 AND NOT-I-U8)
               MOVE SPACES TO BBSFILE-IO-AREA
               INITIALIZE BBSFILE-IO-AREA
               MOVE '//T0000032  JOB 1,'   TO BBSFILE-IO-AREA (1:18)
               MOVE '''AUTO-DATA'''          TO BBSFILE-IO-AREA (19:11)
               MOVE ',CLASS=T'             TO BBSFILE-IO-AREA (30:8)
               MOVE ',MSGCLASS=A,'         TO BBSFILE-IO-AREA (38:12)
               WRITE BBSFILE-IO-AREA
               MOVE SPACES TO BBSFILE-IO-AREA
               INITIALIZE BBSFILE-IO-AREA
               MOVE '//             '      TO BBSFILE-IO-AREA (1:15)
               MOVE 'USER=T000032'         TO BBSFILE-IO-AREA (16:12)
               MOVE ',PASSWORD=TEST    '   TO BBSFILE-IO-AREA (28:18)
               WRITE BBSFILE-IO-AREA
               MOVE SPACES TO BBSFILE-IO-AREA
               INITIALIZE BBSFILE-IO-AREA
               MOVE '//PROC     EXEC   '   TO BBSFILE-IO-AREA (1:18)
               MOVE 'DIRDATNY'             TO BBSFILE-IO-AREA (19:8)
               MOVE ',LISTE=A'             TO BBSFILE-IO-AREA (27:8)
               MOVE ',FORMAT=DIRDATAN,'    TO BBSFILE-IO-AREA (35:17)
               WRITE BBSFILE-IO-AREA
               MOVE SPACES TO BBSFILE-IO-AREA
               INITIALIZE BBSFILE-IO-AREA
               MOVE '//             '      TO BBSFILE-IO-AREA (1:15)
               MOVE 'FILE='                TO BBSFILE-IO-AREA (16:5)
               MOVE '''TSTIK2.K0000032.K01''' TO BBSFILE-IO-AREA
                                                               (21:21)
               WRITE BBSFILE-IO-AREA
               MOVE SPACES TO BBSFILE-IO-AREA
               INITIALIZE BBSFILE-IO-AREA
               MOVE '//STEP2.SYS100  DD *' TO BBSFILE-IO-AREA (1:20)
      *** STARTRECORD FORSENDELSE
               WRITE BBSFILE-IO-AREA
           END-IF
           IF  (I-31)
               MOVE SPACES TO BBSFILE-IO-AREA
               INITIALIZE BBSFILE-IO-AREA
               MOVE 'NY'                   TO BBSFILE-IO-AREA (1:2)
               MOVE '00'                   TO BBSFILE-IO-AREA (3:2)
               MOVE '00'                   TO BBSFILE-IO-AREA (5:2)
               MOVE '10'                   TO BBSFILE-IO-AREA (7:2)
               MOVE '00000117'             TO BBSFILE-IO-AREA (9:8)
               MOVE DD                     TO BBSFILE-IO-AREA (17:2)
               MOVE MM                     TO BBSFILE-IO-AREA (19:2)
               MOVE SNDNR                  TO BBSFILE-IO-AREA (21:3)
               MOVE AAR                    TO BBSFILE-IO-AREA (21:2)
               MOVE '00008080'             TO BBSFILE-IO-AREA (24:8)
               MOVE '0'                    TO BBSFILE-IO-AREA (32:1)
               MOVE '000000000000000000000000' TO BBSFILE-IO-AREA
                                                               (33:24)
               MOVE '000000000000000000000000' TO BBSFILE-IO-AREA
                                                               (57:24)
      *** STARTRECORD OPPDRAG
               WRITE BBSFILE-IO-AREA
           END-IF
           IF  (I-L3 AND I-11)
               MOVE SPACES TO BBSFILE-IO-AREA
               INITIALIZE BBSFILE-IO-AREA
               MOVE 'NY'                   TO BBSFILE-IO-AREA (1:2)
               MOVE '01'                   TO BBSFILE-IO-AREA (3:2)
               MOVE '00'                   TO BBSFILE-IO-AREA (5:2)
               MOVE '20'                   TO BBSFILE-IO-AREA (7:2)
               MOVE AVTALE                 TO BBSFILE-IO-AREA (9:9)
               MOVE DD                     TO BBSFILE-IO-AREA (18:2)
               MOVE MM                     TO BBSFILE-IO-AREA (20:2)
               MOVE SNDNR                  TO BBSFILE-IO-AREA (22:3)
               MOVE AAR                    TO BBSFILE-IO-AREA (22:2)
               MOVE FBKTO-IO               TO BBSFILE-IO-AREA (25:11)
               MOVE '000000000000000000000' TO BBSFILE-IO-AREA (36:21)
               MOVE '000000000000000000000000' TO BBSFILE-IO-AREA
                                                               (57:24)
      *** BELØPSPOST 1
               WRITE BBSFILE-IO-AREA
           END-IF
           IF  (I-01 AND I-10 AND I-12)
               MOVE SPACES TO BBSFILE-IO-AREA
               INITIALIZE BBSFILE-IO-AREA
               MOVE 'NY'                   TO BBSFILE-IO-AREA (1:2)
               MOVE '01'                   TO BBSFILE-IO-AREA (3:2)
               MOVE '02'                   TO BBSFILE-IO-AREA (5:2)
               MOVE '30'                   TO BBSFILE-IO-AREA (7:2)
               MOVE TRANNR-IO              TO BBSFILE-IO-AREA (9:7)
               MOVE FORDX                  TO BBSFILE-IO-AREA (16:2)
               MOVE FORMX                  TO BBSFILE-IO-AREA (18:2)
               MOVE FORAX                  TO BBSFILE-IO-AREA (20:2)
               IF  (NOT-I-73)
                   MOVE KBKTO-IO           TO BBSFILE-IO-AREA (22:11)
               END-IF
               IF  (I-73)
                   MOVE KNR                TO BBSFILE-IO-AREA (27:6)
               END-IF
               MOVE '00'                   TO BBSFILE-IO-AREA (33:2)
               MOVE TRABEL-IO              TO BBSFILE-IO-AREA (35:15)
      *                        KID       74
               MOVE '      '               TO BBSFILE-IO-AREA (50:6)
               MOVE KNR                    TO BBSFILE-IO-AREA (56:6)
               MOVE REFNR5                 TO BBSFILE-IO-AREA (62:5)
               MOVE '00000000'             TO BBSFILE-IO-AREA (67:8)
               MOVE '000000'               TO BBSFILE-IO-AREA (75:6)
      *** BELØPSPOST 2
               WRITE BBSFILE-IO-AREA
               MOVE SPACES TO BBSFILE-IO-AREA
               INITIALIZE BBSFILE-IO-AREA
               MOVE 'NY'                   TO BBSFILE-IO-AREA (1:2)
               MOVE '01'                   TO BBSFILE-IO-AREA (3:2)
               MOVE '02'                   TO BBSFILE-IO-AREA (5:2)
               MOVE '31'                   TO BBSFILE-IO-AREA (7:2)
               MOVE TRANNR-IO              TO BBSFILE-IO-AREA (9:7)
               MOVE FORKNA                 TO BBSFILE-IO-AREA (16:10)
               MOVE 'FAKTURA/INNBET.-NR:'  TO BBSFILE-IO-AREA (26:19)
               MOVE REFNR                  TO BBSFILE-IO-AREA (45:6)
               MOVE FINA25                 TO BBSFILE-IO-AREA (51:25)
               MOVE '00000'                TO BBSFILE-IO-AREA (76:5)
      *** SLUTTRECORD OPPDRAG
               WRITE BBSFILE-IO-AREA
           END-IF
           IF  (I-L1 AND I-02 AND I-10)
           AND (NOT-I-20 AND NOT-I-86 AND I-12)
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               IF  (NOT-I-73)
                   MOVE KBKTO              TO EDIT-KBKTO
                   MOVE EDIT-KBKTO         TO LISTE1-IO-AREA (11:13)
               END-IF
               IF  (I-73)
                   MOVE KNR                TO LISTE1-IO-AREA (12:6)
               END-IF
               MOVE BELO-ELGP              TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE1-IO-AREA (33:14)
               MOVE KNAVN                  TO LISTE1-IO-AREA (51:30)
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
           END-IF
           IF  (I-L1 AND I-02 AND I-10)
           AND (NOT-I-20 AND NOT-I-86 AND NOT-I-12)
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE '====>'                TO LISTE1-IO-AREA (1:5)
               IF  (NOT-I-73)
                   MOVE KBKTO              TO EDIT-KBKTO
                   MOVE EDIT-KBKTO         TO LISTE1-IO-AREA (11:13)
               END-IF
               IF  (I-73)
                   MOVE KNR                TO LISTE1-IO-AREA (12:6)
               END-IF
               MOVE BELO-ELGP              TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE1-IO-AREA (33:14)
               MOVE '<< NEGATIVT BELØP, IKKE ' TO LISTE1-IO-AREA
                                                               (51:24)
               MOVE 'MED >>'               TO LISTE1-IO-AREA (75:6)
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
           END-IF
           IF  (I-01 AND I-20 AND NOT-I-86)
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE '====>'                TO LISTE1-IO-AREA (1:5)
               IF  (NOT-I-73)
                   MOVE KBKTO              TO EDIT-KBKTO
                   MOVE EDIT-KBKTO         TO LISTE1-IO-AREA (11:13)
               END-IF
               IF  (I-73)
                   MOVE KNR                TO LISTE1-IO-AREA (12:6)
               END-IF
               MOVE BELO-ELGP              TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE1-IO-AREA (33:14)
               MOVE '<< UKJENT KUNDENR, IKKE ' TO LISTE1-IO-AREA
                                                               (51:24)
               MOVE 'MED >>'               TO LISTE1-IO-AREA (75:6)
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
           END-IF
           IF  (I-01 AND I-71 AND NOT-I-86)
           AND (NOT-I-73)
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE '====>'                TO LISTE1-IO-AREA (1:5)
               IF  (NOT-I-73)
                   MOVE KBKTO              TO EDIT-KBKTO
                   MOVE EDIT-KBKTO         TO LISTE1-IO-AREA (11:13)
               END-IF
               MOVE BELO-ELGP              TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE1-IO-AREA (33:14)
               MOVE '<< FEIL BANKKONTO, IKKE ' TO LISTE1-IO-AREA
                                                               (51:24)
               MOVE 'MED >>'               TO LISTE1-IO-AREA (75:6)
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
           END-IF
           IF  (I-05 AND NOT-I-86)
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'KUNDE ===>'           TO LISTE1-IO-AREA (1:10)
               MOVE KNR                    TO LISTE1-IO-AREA (12:6)
               MOVE BELO-ELGP              TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE1-IO-AREA (33:14)
               IF  (I-51)
                   MOVE '<< BELØP < EL = 0, IKKE ' TO LISTE1-IO-AREA
                                                               (51:24)
               END-IF
               IF  (I-51)
                   MOVE 'MED >>'           TO LISTE1-IO-AREA (75:6)
               END-IF
               IF  (I-52)
                   MOVE '<< SUM FAKTURA ULIK GIRO' TO LISTE1-IO-AREA
                                                               (51:24)
               END-IF
               IF  (I-52)
                   MOVE 'BEL.>>'           TO LISTE1-IO-AREA (75:6)
               END-IF
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L2 AND NOT-I-86)
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'A U T O G I R O - T R A ' TO LISTE1-IO-AREA
                                                               (11:24)
               MOVE 'N S M I S J O N - B B S' TO LISTE1-IO-AREA (35:23)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE1-IO-AREA (73:8)
               MOVE 01                     TO LISTE1-BEFORE-SKIP
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE '--------------'       TO LISTE1-IO-AREA (11:14)
               MOVE '------------------------' TO LISTE1-IO-AREA
                                                               (25:24)
               MOVE '------------------------' TO LISTE1-IO-AREA
                                                               (49:24)
               MOVE '--------'             TO LISTE1-IO-AREA (73:8)
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'SIDE:'                TO LISTE1-IO-AREA (57:5)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE1-IO-AREA (62:4)
               MOVE 2                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'FIRMANAVN, KONTAKTPERSON' TO LISTE1-IO-AREA
                                                               (11:24)
               MOVE 'OG ADRESSE:'          TO LISTE1-IO-AREA (36:11)
               MOVE 2                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE FINAVN                 TO LISTE1-IO-AREA (11:30)
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE KONAVN                 TO LISTE1-IO-AREA (11:30)
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE FIADR                  TO LISTE1-IO-AREA (11:30)
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE FIPNR                  TO LISTE1-IO-AREA (11:4)
               MOVE FIPSTD                 TO LISTE1-IO-AREA (15:26)
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'AVTALE ID'            TO LISTE1-IO-AREA (11:9)
               MOVE 'MOTTAGERS KTO. NR.'   TO LISTE1-IO-AREA (29:18)
               MOVE 'FORF. DATO'           TO LISTE1-IO-AREA (57:10)
      *                                  77 "INNBET.NR"
               MOVE 2                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE AVTALE                 TO LISTE1-IO-AREA (11:9)
               MOVE FBKTO                  TO EDIT-FBKTO
               MOVE EDIT-FBKTO             TO LISTE1-IO-AREA (29:13)
               MOVE FORDX                  TO LISTE1-IO-AREA (57:2)
               MOVE '.'                    TO LISTE1-IO-AREA (59:1)
               MOVE FORMX                  TO LISTE1-IO-AREA (60:2)
               MOVE '.'                    TO LISTE1-IO-AREA (62:1)
               MOVE FORAX                  TO LISTE1-IO-AREA (63:2)
      *                        REFNR     74
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'BETALERS'             TO LISTE1-IO-AREA (11:8)
               IF  (NOT-I-73)
                   MOVE 'KTO.NR.'          TO LISTE1-IO-AREA (20:7)
               END-IF
               IF  (I-73)
                   MOVE 'REF.NR.'          TO LISTE1-IO-AREA (20:7)
               END-IF
               MOVE 'BELØP'                TO LISTE1-IO-AREA (41:5)
               MOVE 'BETALERS NAVN           ' TO LISTE1-IO-AREA
                                                               (51:24)
               MOVE 2                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE '--------------'       TO LISTE1-IO-AREA (11:14)
               MOVE '------------------------' TO LISTE1-IO-AREA
                                                               (25:24)
               MOVE '------------------------' TO LISTE1-IO-AREA
                                                               (49:24)
               MOVE '--------'             TO LISTE1-IO-AREA (73:8)
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF AND NOT-I-L3 AND NOT-I-86)
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'A U T O G I R O - T R A ' TO LISTE1-IO-AREA
                                                               (11:24)
               MOVE 'N S M I S J O N - B B S' TO LISTE1-IO-AREA (35:23)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE1-IO-AREA (73:8)
               MOVE 01                     TO LISTE1-BEFORE-SKIP
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE '--------------'       TO LISTE1-IO-AREA (11:14)
               MOVE '------------------------' TO LISTE1-IO-AREA
                                                               (25:24)
               MOVE '------------------------' TO LISTE1-IO-AREA
                                                               (49:24)
               MOVE '--------'             TO LISTE1-IO-AREA (73:8)
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'SIDE:'                TO LISTE1-IO-AREA (57:5)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE1-IO-AREA (62:4)
               MOVE 2                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'FIRMANAVN, KONTAKTPERSON' TO LISTE1-IO-AREA
                                                               (11:24)
               MOVE 'OG ADRESSE:'          TO LISTE1-IO-AREA (36:11)
               MOVE 2                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE FINAVN                 TO LISTE1-IO-AREA (11:30)
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE KONAVN                 TO LISTE1-IO-AREA (11:30)
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE FIADR                  TO LISTE1-IO-AREA (11:30)
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE FIPNR                  TO LISTE1-IO-AREA (11:4)
               MOVE FIPSTD                 TO LISTE1-IO-AREA (15:26)
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'AVTALE ID'            TO LISTE1-IO-AREA (11:9)
               MOVE 'MOTTAGERS KTO. NR.'   TO LISTE1-IO-AREA (29:18)
               MOVE 'FORF. DATO'           TO LISTE1-IO-AREA (57:10)
      *                                  77 "INNBET.NR"
               MOVE 2                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE AVTALE                 TO LISTE1-IO-AREA (11:9)
               MOVE FBKTO                  TO EDIT-FBKTO
               MOVE EDIT-FBKTO             TO LISTE1-IO-AREA (29:13)
               MOVE FORDX                  TO LISTE1-IO-AREA (57:2)
               MOVE '.'                    TO LISTE1-IO-AREA (59:1)
               MOVE FORMX                  TO LISTE1-IO-AREA (60:2)
               MOVE '.'                    TO LISTE1-IO-AREA (62:1)
               MOVE FORAX                  TO LISTE1-IO-AREA (63:2)
      *                        REFNR     74
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'BETALERS'             TO LISTE1-IO-AREA (11:8)
               IF  (NOT-I-73)
                   MOVE 'KTO.NR.'          TO LISTE1-IO-AREA (20:7)
               END-IF
               IF  (I-73)
                   MOVE 'REF.NR.'          TO LISTE1-IO-AREA (20:7)
               END-IF
               MOVE 'BELØP'                TO LISTE1-IO-AREA (41:5)
               MOVE 'BETALERS NAVN           ' TO LISTE1-IO-AREA
                                                               (51:24)
               MOVE 2                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE '--------------'       TO LISTE1-IO-AREA (11:14)
               MOVE '------------------------' TO LISTE1-IO-AREA
                                                               (25:24)
               MOVE '------------------------' TO LISTE1-IO-AREA
                                                               (49:24)
               MOVE '--------'             TO LISTE1-IO-AREA (73:8)
               MOVE 1                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L3 AND I-11)
               MOVE SPACES TO BBSFILE-IO-AREA
               INITIALIZE BBSFILE-IO-AREA
               MOVE 'NY'                   TO BBSFILE-IO-AREA (1:2)
               MOVE '01'                   TO BBSFILE-IO-AREA (3:2)
               MOVE '00'                   TO BBSFILE-IO-AREA (5:2)
               MOVE '88'                   TO BBSFILE-IO-AREA (7:2)
               MOVE TOOTRA-IO              TO BBSFILE-IO-AREA (9:8)
               MOVE ANTRCO-IO              TO BBSFILE-IO-AREA (17:8)
               MOVE '00'                   TO BBSFILE-IO-AREA (25:2)
               MOVE SUMBEL-IO              TO BBSFILE-IO-AREA (27:15)
               MOVE FO-ELGRFF              TO BBSFILE-IO-AREA (42:6)
               MOVE SISFF                  TO BBSFILE-IO-AREA (48:6)
               MOVE '000'                  TO BBSFILE-IO-AREA (54:3)
               MOVE '000000000000000000000000' TO BBSFILE-IO-AREA
                                                               (57:24)
      *** SLUTTRECORD FORSENDELSE
               WRITE BBSFILE-IO-AREA
           END-IF
           IF  (I-LR)
               MOVE SPACES TO BBSFILE-IO-AREA
               INITIALIZE BBSFILE-IO-AREA
               MOVE 'NY'                   TO BBSFILE-IO-AREA (1:2)
               MOVE '00'                   TO BBSFILE-IO-AREA (3:2)
               MOVE '00'                   TO BBSFILE-IO-AREA (5:2)
               MOVE '89'                   TO BBSFILE-IO-AREA (7:2)
               MOVE TOFTRA-IO              TO BBSFILE-IO-AREA (9:8)
               MOVE ANTRCF-IO              TO BBSFILE-IO-AREA (17:8)
               MOVE '00'                   TO BBSFILE-IO-AREA (25:2)
               MOVE TOTBEL-IO              TO BBSFILE-IO-AREA (27:15)
               MOVE FO-ELGRFF              TO BBSFILE-IO-AREA (42:6)
               MOVE '000000000'            TO BBSFILE-IO-AREA (48:9)
               MOVE '000000000000000000000000' TO BBSFILE-IO-AREA
                                                               (57:24)
      ****************************************************************
      *** KVITTERING TIL FIRMA/KUNDE                               ***
      ****************************************************************
               WRITE BBSFILE-IO-AREA
           END-IF
           IF  (I-L2 AND NOT-I-86)
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'SUM BELØP PR FORFALL   :' TO LISTE1-IO-AREA (1:24)
               MOVE BELL2                  TO XO-132YY9R
               MOVE XO-132YY9R             TO LISTE1-IO-AREA (28:21)
               MOVE 2                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
           END-IF
           IF  (I-L3 AND NOT-I-86)
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'OPPDRAGSNUMMER         :' TO LISTE1-IO-AREA (1:24)
               MOVE DD                     TO LISTE1-IO-AREA (26:2)
               MOVE MM                     TO LISTE1-IO-AREA (28:2)
               MOVE SNDNR                  TO LISTE1-IO-AREA (30:3)
               MOVE AAR                    TO LISTE1-IO-AREA (30:2)
               MOVE 2                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'SUM RECORDS I OPPDRAG  :' TO LISTE1-IO-AREA (1:24)
               MOVE ANTRCO                 TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE1-IO-AREA (38:11)
               MOVE 2                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'SUM BELØP I OPPDRAG    :' TO LISTE1-IO-AREA (1:24)
               MOVE SUMBEL                 TO XO-132YY9R
               MOVE XO-132YY9R             TO LISTE1-IO-AREA (28:21)
               MOVE 2                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'ANT FEIL-POSTER        :' TO LISTE1-IO-AREA (1:24)
               MOVE FEIANT                 TO XO-60YN9R
               MOVE XO-60YN9R              TO LISTE1-IO-AREA (42:7)
               INITIALIZE FEIANT
               MOVE 2                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
               MOVE SPACES TO LISTE1-IO-AREA
               INITIALIZE LISTE1-IO-AREA
               MOVE 'SUM FEIL-POSTER        :' TO LISTE1-IO-AREA (1:24)
               MOVE FEIBEL                 TO XO-102YY9R
               MOVE XO-102YY9R             TO LISTE1-IO-AREA (32:17)
               INITIALIZE FEIBEL
      *** KVITTERING TOTALT FOR AUTODATA                           ***
               MOVE 2                      TO LISTE1-BEFORE-SPACE
               PERFORM LISTE1-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE '************************' TO LISTE2-IO-AREA
                                                               (17:24)
               MOVE '******************'   TO LISTE2-IO-AREA (41:18)
               MOVE 01                     TO LISTE2-BEFORE-SKIP
               MOVE 3                      TO LISTE2-BEFORE-SPACE
               MOVE 0                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE '*'                    TO LISTE2-IO-AREA (17:1)
               MOVE '*'                    TO LISTE2-IO-AREA (58:1)
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE '*'                    TO LISTE2-IO-AREA (17:1)
               MOVE 'AUTOGIRO KVITT.-LISTE.  ' TO LISTE2-IO-AREA
                                                               (23:24)
               MOVE '*'                    TO LISTE2-IO-AREA (58:1)
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE '*'                    TO LISTE2-IO-AREA (17:1)
               MOVE '*'                    TO LISTE2-IO-AREA (58:1)
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE '*'                    TO LISTE2-IO-AREA (17:1)
               MOVE 'LEVERES: OPERATØR       ' TO LISTE2-IO-AREA
                                                               (23:24)
               MOVE '*'                    TO LISTE2-IO-AREA (58:1)
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE '*'                    TO LISTE2-IO-AREA (17:1)
               MOVE '                        ' TO LISTE2-IO-AREA
                                                               (23:24)
               MOVE '*'                    TO LISTE2-IO-AREA (58:1)
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE '*'                    TO LISTE2-IO-AREA (17:1)
               MOVE '                        ' TO LISTE2-IO-AREA
                                                               (23:24)
               MOVE '*'                    TO LISTE2-IO-AREA (58:1)
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE '*'                    TO LISTE2-IO-AREA (17:1)
               MOVE 'GRUNNLAG FØLGESEDDEL    ' TO LISTE2-IO-AREA
                                                               (23:24)
               MOVE '*'                    TO LISTE2-IO-AREA (58:1)
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE '*'                    TO LISTE2-IO-AREA (17:1)
               MOVE '                        ' TO LISTE2-IO-AREA
                                                               (23:24)
               MOVE '*'                    TO LISTE2-IO-AREA (58:1)
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE '*                   ' TO LISTE2-IO-AREA (17:20)
               MOVE '*'                    TO LISTE2-IO-AREA (58:1)
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE '*     FREMSTILT'      TO LISTE2-IO-AREA (17:15)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE2-IO-AREA (33:8)
               MOVE '*'                    TO LISTE2-IO-AREA (58:1)
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE '*'                    TO LISTE2-IO-AREA (17:1)
               MOVE '*'                    TO LISTE2-IO-AREA (58:1)
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE '************************' TO LISTE2-IO-AREA
                                                               (17:24)
               MOVE '******************'   TO LISTE2-IO-AREA (41:18)
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE 'A U T O  D A T A - T O T' TO LISTE2-IO-AREA (1:24)
               MOVE 'A L E R'              TO LISTE2-IO-AREA (26:7)
               MOVE 01                     TO LISTE2-BEFORE-SKIP
               MOVE 3                      TO LISTE2-BEFORE-SPACE
               MOVE 3                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE 'A U T O G I R O - KVITTE' TO LISTE2-IO-AREA (1:24)
               MOVE 'RINGSLISTE'           TO LISTE2-IO-AREA (25:10)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE2-IO-AREA (36:8)
               MOVE 3                      TO LISTE2-BEFORE-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE 'KUNDEENHET-ID/SENTRALNR:' TO LISTE2-IO-AREA (1:24)
               MOVE ' 00000117               ' TO LISTE2-IO-AREA
                                                               (25:24)
               MOVE 2                      TO LISTE2-BEFORE-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE 'DATAAVSENDERS NAVN     :' TO LISTE2-IO-AREA (1:24)
               MOVE ' A/S AUTODATA           ' TO LISTE2-IO-AREA
                                                               (25:24)
               MOVE 2                      TO LISTE2-BEFORE-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE 'KONTAKTPERSON          :' TO LISTE2-IO-AREA (1:24)
               MOVE ' MORTEN TUVRØNNINGEN    ' TO LISTE2-IO-AREA
                                                               (25:24)
               MOVE 2                      TO LISTE2-BEFORE-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE 'TELEFONNR              :' TO LISTE2-IO-AREA (1:24)
               MOVE ' 23 17 20 30            ' TO LISTE2-IO-AREA
                                                               (25:24)
               MOVE 2                      TO LISTE2-BEFORE-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE 'STED                   :' TO LISTE2-IO-AREA (1:24)
               MOVE ' OSLO                   ' TO LISTE2-IO-AREA
                                                               (25:24)
               MOVE 2                      TO LISTE2-BEFORE-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE 'DATO                   :' TO LISTE2-IO-AREA (1:24)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE2-IO-AREA (26:8)
               MOVE 2                      TO LISTE2-BEFORE-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE 'DATAAVSEND. UNDERSKRIFT:' TO LISTE2-IO-AREA (1:24)
               MOVE ' """""""""""""""""""""""""""OEAØSUDRKIT" OLSE-OAE
                                                                   2:4
               MOVE 2                      TO LISTE2-BEFORE-SPACE
               MOVE 2                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE 'FORSENDELSESNR         :' TO LISTE2-IO-AREA (1:24)
               MOVE DD                     TO LISTE2-IO-AREA (26:2)
               MOVE MM                     TO LISTE2-IO-AREA (28:2)
               MOVE SNDNR                  TO LISTE2-IO-AREA (30:3)
               MOVE AAR                    TO LISTE2-IO-AREA (30:2)
               MOVE 2                      TO LISTE2-BEFORE-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE 'TOTALSUM PR FORSENDELSE:' TO LISTE2-IO-AREA (1:24)
               MOVE TOTBEL                 TO XO-132YY9R
               MOVE XO-132YY9R             TO LISTE2-IO-AREA (28:21)
               MOVE 2                      TO LISTE2-BEFORE-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE 'ANT REC PR FORSENDELSE :' TO LISTE2-IO-AREA (1:24)
               MOVE ANTRCF                 TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE2-IO-AREA (38:11)
               MOVE 2                      TO LISTE2-BEFORE-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE 'DENNE SIDEN FAXES IKKE. ' TO LISTE2-IO-AREA (1:24)
               MOVE 'BEHOLDES FOR Å BRUKE RET' TO LISTE2-IO-AREA
                                                               (25:24)
               MOVE 'T PASSORD VED EVT OMKJØR' TO LISTE2-IO-AREA
                                                               (49:24)
               MOVE 'ING.    '             TO LISTE2-IO-AREA (73:8)
               MOVE 01                     TO LISTE2-BEFORE-SKIP
               MOVE 3                      TO LISTE2-BEFORE-SPACE
               MOVE 0                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE 'JCL-PASSORD: '        TO LISTE2-IO-AREA (1:13)
               MOVE GMLPSW                 TO LISTE2-IO-AREA (14:8)
               MOVE ' SIST ENDRET: '       TO LISTE2-IO-AREA (22:14)
               MOVE BBSDTO                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE2-IO-AREA (36:8)
               IF  (I-21)
                   MOVE UDATE              TO EDIT-DATE
                   MOVE EDIT-DATE (7:8)    TO LISTE2-IO-AREA (36:8)
               END-IF
               IF  (I-21)
                   MOVE ' BYTTET TIL: '    TO LISTE2-IO-AREA (44:13)
               END-IF
               IF  (I-21)
                   MOVE NYPSW              TO LISTE2-IO-AREA (58:8)
               END-IF
               MOVE 2                      TO LISTE2-BEFORE-SPACE
               PERFORM LISTE2-PRINT-LINE
           END-IF
           IF  (I-LR AND I-22)
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE 'FEIL VED LES PORDFIL.   ' TO LISTE2-IO-AREA (1:24)
      *****************************************************************
      * DUMMY-LINJE FOR Å LAGE REF. TIL DATAFELT (FJERNE FEILMELDING) *
      *****************************************************************
               MOVE 2                      TO LISTE2-BEFORE-SPACE
               PERFORM LISTE2-PRINT-LINE
           END-IF
           IF  (I-U1 AND I-01 AND I-02)
           AND (I-04 AND I-98 AND I-99)
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE LONR                   TO LISTE2-IO-AREA (76:5)
               MOVE LFIRMA                 TO LISTE2-IO-AREA (78:3)
               MOVE LOPNVN                 TO LISTE2-IO-AREA (46:35)
               MOVE LUNDGR                 TO LISTE2-IO-AREA (78:3)
               MOVE LPROG                  TO LISTE2-IO-AREA (73:8)
               MOVE LPRIID                 TO LISTE2-IO-AREA (77:4)
               MOVE BBEST                  TO LISTE2-IO-AREA (80:1)
               MOVE PSDS                   TO LISTE2-IO-AREA (1:80)
               MOVE R                      TO LISTE2-IO-AREA (73:8)
               MOVE P-IO                   TO LISTE2-IO-AREA (78:3)
               MOVE S-IO                   TO LISTE2-IO-AREA (76:5)
               MOVE BJOBN                  TO LISTE2-IO-AREA (73:8)
               MOVE BBEST                  TO LISTE2-IO-AREA (80:1)
               MOVE BPERS                  TO LISTE2-IO-AREA (51:30)
               MOVE BETTB                  TO LISTE2-IO-AREA (41:40)
               MOVE BFORS                  TO LISTE2-IO-AREA (41:40)
               MOVE BMEMO                  TO LISTE2-IO-AREA (41:40)
               MOVE BANTX-IO               TO LISTE2-IO-AREA (78:3)
               MOVE PENDR                  TO LISTE2-IO-AREA (80:1)
               MOVE PDATO                  TO LISTE2-IO-AREA (75:6)
               MOVE BPCLAS                 TO LISTE2-IO-AREA (80:1)
               MOVE BPRJE                  TO LISTE2-IO-AREA (78:3)
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
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
           PERFORM PARTAB-LOAD
           PERFORM FIRTAB-LOAD
           SET AUTOGIR-LEVEL-INIT          TO TRUE
           INITIALIZE AUTOGIR-DATA-FIELDS
           SET AUTOGIR-EOF-OFF             TO TRUE
           SET AUTOGIR-PROCESS             TO TRUE
           OPEN INPUT AUTOGIR
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           INITIALIZE PORDFIL-DATA-FIELDS
           OPEN I-O PORDFIL
           OPEN OUTPUT BBSFILE
           OPEN OUTPUT LISTE1
           INITIALIZE LISTE1-IO-AREA
           INITIALIZE LISTE1-DATA-FIELDS
           MOVE 57                         TO LISTE1-MAX-LINES
           OPEN OUTPUT LISTE2
           INITIALIZE LISTE2-IO-AREA
           INITIALIZE LISTE2-DATA-FIELDS
           MOVE 57                         TO LISTE2-MAX-LINES.
           SET TABPAR-I                    TO 1
           SET TABFIR-I                    TO 1
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE AUTOGIR
           CLOSE KUNDEMA
           CLOSE FIRMAF
           CLOSE PORDFIL
           CLOSE BBSFILE
           IF LISTE1-IO-AREA NOT = SPACES
             WRITE LISTE1-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE1-IO-AREA
           END-IF
           CLOSE LISTE1
           IF LISTE2-IO-AREA NOT = SPACES
             WRITE LISTE2-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE2-IO-AREA
           END-IF
           CLOSE LISTE2.
 
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
