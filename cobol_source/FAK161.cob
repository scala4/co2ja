       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK161R.
      **********************************************  Z-WIN-RPG2   ****
      * UTGANGSPUNKT I PROG FAK093
      * FAKTURAFIL I INFOEASYFORMAT
      * ENDRING: 13.12.11 - LAGT INN CHAIN MOT RA 1 HVIS IKKE FUNNET PÅ
      *                     RA O PÅ ORDREM.
      *
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK161.rpg
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
           SELECT INNFIL
               ASSIGN TO UT-S-INNFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INNFIL-STATUS.
           SELECT KONTOMA
               ASSIGN TO KONTOMA
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KONTOMA-STATUS
               RECORD KEY IS KONTOMA-KEY1.
           SELECT KONTOKY
               ASSIGN TO KONTOKY
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KONTOKY-STATUS
               RECORD KEY IS KONTOKY-KEY1.
           SELECT ORDREM
               ASSIGN TO ORDREM
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS ORDREM-STATUS
               RECORD KEY IS ORDREM-KEY1.
           SELECT KUNDEMA
               ASSIGN TO KUNDEMA
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMA-STATUS
               RECORD KEY IS KUNDEMA-KEY1.
           SELECT KUNDEMX
               ASSIGN TO KUNDEMX
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMX-STATUS
               RECORD KEY IS KUNDEMX-KEY1.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT FAKPAR
               ASSIGN TO UT-S-FAKPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKPAR-STATUS.
           SELECT OUTFIL
               ASSIGN TO UT-S-OUTFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTFIL-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD INNFIL
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  INNFIL-IO-AREA.
           05  INNFIL-IO-AREA-X            PICTURE X(200).
       FD KONTOMA
               RECORD CONTAINS 60.
       01  KONTOMA-IO-AREA.
           05  KONTOMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KONTOMA-KEY1            PICTURE X(7).
               10  FILLER                  PICTURE X(52).
       FD KONTOKY
               RECORD CONTAINS 20.
       01  KONTOKY-IO-AREA.
           05  KONTOKY-IO-AREA-X.
               10  KONTOKY-KEY1            PICTURE X(12).
               10  FILLER                  PICTURE X(8).
       FD ORDREM
               RECORD CONTAINS 164.
       01  ORDREM-IO-AREA.
           05  ORDREM-IO-AREA-X.
               10  ORDREM-KEY1             PICTURE X(20).
               10  FILLER                  PICTURE X(144).
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1            PICTURE X(9).
               10  FILLER                  PICTURE X(190).
       FD KUNDEMX
               RECORD CONTAINS 200.
       01  KUNDEMX-IO-AREA.
           05  KUNDEMX-IO-AREA-X.
               10  KUNDEMX-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
      *UTFIL  O   F     316            ESDS
       FD FAKPAR
               BLOCK CONTAINS 200
               RECORD CONTAINS 200.
       01  FAKPAR-IO-AREA.
           05  FAKPAR-IO-AREA-X            PICTURE X(200).
       FD OUTFIL
               BLOCK CONTAINS 2200
               RECORD CONTAINS 1100.
       01  OUTFIL-IO-AREA.
           05  OUTFIL-IO-AREA-X            PICTURE X(1100).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  INNFIL-STATUS               PICTURE 99 VALUE 0.
           10  KONTOMA-STATUS              PICTURE 99 VALUE 0.
           10  KONTOKY-STATUS              PICTURE 99 VALUE 0.
           10  ORDREM-STATUS               PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  KUNDEMX-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  FAKPAR-STATUS               PICTURE 99 VALUE 0.
           10  OUTFIL-STATUS               PICTURE 99 VALUE 0.
           10  KIFELT-XX-STATUS            PICTURE 99 VALUE 0.
           10  MVFELT-XX-STATUS            PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  INNFIL-EOF-OFF          VALUE '0'.
               88  INNFIL-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNFIL-READ-OFF         VALUE '0'.
               88  INNFIL-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNFIL-PROCESS-OFF      VALUE '0'.
               88  INNFIL-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  INNFIL-LEVEL-INIT-OFF   VALUE '0'.
               88  INNFIL-LEVEL-INIT       VALUE '1'.
           05  KONTOMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  KONTOKY-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  ORDREM-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  KUNDEMX-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
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
           05  KIFELT-XX-DATA-FIELDS.
               10  KITALL                  PICTURE X(25).
               10  FILLER                  PICTURE X(8).
           05  FILLER REDEFINES KIFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(25).
               10  KITYPE                  PICTURE X(1).
               10  FILLER                  PICTURE X(7).
           05  FILLER REDEFINES KIFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(26).
               10  KIKTR                   PICTURE X(1).
               10  FILLER                  PICTURE X(6).
           05  MVFELT-XX REDEFINES KIFELT-XX-DATA-FIELDS.
               10  BUMVA-IO.
                   15  BUMVA               PICTURE S9(9)V9(2).
               10  FILLER                  PICTURE X(22).
           05  FILLER REDEFINES KIFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(11).
               10  BMMVA-IO.
                   15  BMMVA               PICTURE S9(9)V9(2).
               10  FILLER                  PICTURE X(11).
           05  FILLER REDEFINES KIFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(22).
               10  MVA-IO.
                   15  MVA                 PICTURE S9(9)V9(2).
      *DSDS: DATA STRUCTURE FIELDS
           05  MVFELT-XX-DATA-FIELDS.
               10  BUMVA-IO.
                   15  BUMVA               PICTURE S9(9)V9(2).
               10  FILLER                  PICTURE X(22).
           05  FILLER REDEFINES MVFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(11).
               10  BMMVA-IO.
                   15  BMMVA               PICTURE S9(9)V9(2).
               10  FILLER                  PICTURE X(11).
           05  FILLER REDEFINES MVFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(22).
               10  MVA-IO.
                   15  MVA                 PICTURE S9(9)V9(2).
           05  INNFIL-LEVEL-01.
               10  INNFIL-01-L5.
                   15  INNFIL-01-L5-FIRMA  PICTURE X(3).
               10  INNFIL-01-L4.
                   15  INNFIL-01-L4-FAKTNR PICTURE S9(6).
               10  INNFIL-01-L3.
                   15  INNFIL-01-L3-KUNDNR PICTURE X(6).
               10  INNFIL-01-L2.
                   15  INNFIL-01-L2-ORDNR  PICTURE X(6).
               10  INNFIL-01-L1.
                   15  INNFIL-01-L1-VGR    PICTURE X(5).
           05  INNFIL-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  KUNDNR                  PICTURE X(6).
               10  FNRKNR                  PICTURE X(9).
               10  FAKTNR-IO.
                   15  FAKTNR              PICTURE S9(6).
               10  FAKT1                   PICTURE X(1).
               10  ORDNR                   PICTURE X(6).
               10  ORD1                    PICTURE X(1).
               10  VGR                     PICTURE X(5).
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  EDBNR                   PICTURE X(7).
               10  EDB2                    PICTURE X(2).
               10  EDB3                    PICTURE X(3).
               10  ORDMOT                  PICTURE X(2).
               10  FK                      PICTURE X(1).
               10  KRTYPE                  PICTURE X(1).
               10  NTOSUM-IO.
                   15  NTOSUM              PICTURE S9(7)V9(2).
               10  NTOKR-IO.
                   15  NTOKR               PICTURE S9(7).
               10  NTOORE-IO.
                   15  NTOORE              PICTURE S9(2).
               10  FAKMND                  PICTURE X(6).
               10  FMND                    PICTURE X(2).
               10  FAKAA                   PICTURE X(4).
               10  AA2                     PICTURE X(2).
               10  ORDDTO                  PICTURE X(8).
               10  ORAAR-IO.
                   15  ORAAR               PICTURE S9(4).
               10  ORMND-IO.
                   15  ORMND               PICTURE S9(2).
               10  ORDAG-IO.
                   15  ORDAG               PICTURE S9(2).
               10  ANTLEV-IO.
                   15  ANTLEV              PICTURE S9(5)V9(2).
               10  SELVK-IO.
                   15  SELVK               PICTURE S9(7)V9(2).
               10  FAKOMG                  PICTURE X(1).
               10  KONTO                   PICTURE X(4).
               10  KONSF                   PICTURE X(3).
               10  BM                      PICTURE X(2).
               10  BTMA-ELGTE              PICTURE X(2).
           05  KONTOMA-DATA-FIELDS.
               10  KKONTO                  PICTURE X(4).
               10  KONAVN                  PICTURE X(35).
           05  KONTOKY-DATA-FIELDS.
               10  ADSKTO                  PICTURE X(4).
           05  ORDREM-DATA-FIELDS.
               10  MVAFRI                  PICTURE X(1).
               10  BK                      PICTURE X(1).
           05  KUNDEMA-DATA-FIELDS.
               10  KMNAVN                  PICTURE X(30).
      * ENDRET TIL LINJE UNDER, UTEN 10      46  75 KMADR1          10
               10  KMADR1                  PICTURE X(30).
               10  KMADR2                  PICTURE X(30).
               10  KMSTED                  PICTURE X(15).
               10  KMPNR                   PICTURE X(4).
               10  CBETMK                  PICTURE X(2).
               10  KREDGR-IO.
                   15  KREDGR              PICTURE S9(4).
               10  BANKGI                  PICTURE X(11).
               10  HND-IO.
                   15  HND                 PICTURE S9(3).
           05  KUNDEMX-DATA-FIELDS.
      *  RECART 1
               10  TL1                     PICTURE X(16).
               10  PERS1                   PICTURE X(30).
               10  PERS2                   PICTURE X(30).
               10  SNR                     PICTURE X(3).
               10  FKNR                    PICTURE X(6).
               10  ORGN1                   PICTURE X(9).
      *  RECART 2
               10  LEVAD1                  PICTURE X(30).
               10  LEVAD2                  PICTURE X(30).
               10  LEVAD3                  PICTURE X(30).
               10  LEVPNR                  PICTURE X(4).
               10  LPSTED                  PICTURE X(15).
               10  FORSMA                  PICTURE X(15).
               10  FAX                     PICTURE X(16).
      *  RECART 6
               10  EMAIL6                  PICTURE X(40).
      *  RECART A
               10  MAILA                   PICTURE X(40).
      *  RECART B
               10  MAILB                   PICTURE X(40).
      *  RECART C
               10  MAILC                   PICTURE X(40).
      *  RECART D
               10  MAILD                   PICTURE X(40).
      *  RECART E
               10  MAILE                   PICTURE X(40).
           05  FIRMAF-DATA-FIELDS.
               10  RNTPRO-IO.
                   15  RNTPRO              PICTURE S9(1)V9(2).
               10  RNTPRA-IO.
                   15  RNTPRA              PICTURE S9(1)V9(2).
               10  FFKODE                  PICTURE X(1).
               10  PRIFNR                  PICTURE X(1).
               10  KRFFD                   PICTURE X(1).
               10  KTOAVR                  PICTURE X(4).
               10  FTAKNR-IO.
                   15  FTAKNR              PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
           05  FAKPAR-DATA-FIELDS.
               10  PFADTO                  PICTURE X(6).
               10  PFDAG                   PICTURE X(2).
               10  PFMND                   PICTURE X(2).
               10  PFAAR                   PICTURE X(2).
               10  PFF15                   PICTURE X(6).
               10  PFF30                   PICTURE X(6).
               10  PFF45                   PICTURE X(6).
               10  PFF60                   PICTURE X(6).
               10  PFF90                   PICTURE X(6).
               10  PFF75                   PICTURE X(6).
               10  FO-ELGRSTE              PICTURE X(1).
               10  FAKTOM                  PICTURE X(2).
               10  UFADTO                  PICTURE X(6).
               10  UFDAG                   PICTURE X(2).
               10  UFMND                   PICTURE X(2).
               10  UFAAR                   PICTURE X(2).
               10  UFF15                   PICTURE X(6).
               10  UFF30                   PICTURE X(6).
               10  UFF45                   PICTURE X(6).
               10  UFF60                   PICTURE X(6).
               10  UFF90                   PICTURE X(6).
               10  UFF75                   PICTURE X(6).
               10  UFF105                  PICTURE X(6).
               10  UFF120                  PICTURE X(6).
               10  UFF150                  PICTURE X(6).
               10  UFF180                  PICTURE X(6).
               10  PFF105                  PICTURE X(6).
               10  PFF120                  PICTURE X(6).
               10  PFF150                  PICTURE X(6).
               10  PFF180                  PICTURE X(6).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L5            PICTURE X(3).
               10  THE-PRIOR-L4            PICTURE X(6).
               10  THE-PRIOR-L3            PICTURE X(6).
               10  THE-PRIOR-L2            PICTURE X(6).
               10  THE-PRIOR-L1            PICTURE X(5).
           05  TEMPORARY-FIELDS.
               10  FOFALL                  PICTURE X(6).
               10  UDDMM-IO.
                   15  UDDMM               PICTURE S9(4).
               10  UYY-IO.
                   15  UYY                 PICTURE S9(2).
               10  UDD-IO.
                   15  UDD                 PICTURE S9(2).
               10  UMM-IO.
                   15  UMM                 PICTURE S9(2).
               10  NULL92-IO.
                   15  NULL92              PICTURE S9(7)V9(2).
               10  PERIOD-IO.
                   15  PERIOD              PICTURE S9(2).
               10  SUML3-IO.
                   15  SUML3               PICTURE S9(7)V9(2).
               10  SUML3B-IO.
                   15  SUML3B              PICTURE S9(7).
               10  SUML3O-IO.
                   15  SUML3O              PICTURE S9(2).
               10  SUML3K-IO.
                   15  SUML3K              PICTURE S9(7)V9(2).
               10  SUML3E-IO.
                   15  SUML3E              PICTURE S9(7).
               10  SUML3G-IO.
                   15  SUML3G              PICTURE S9(2).
               10  SUM01M-IO.
                   15  SUM01M              PICTURE S9(7)V9(2).
               10  FNRRNR                  PICTURE X(9).
               10  KBNAVN                  PICTURE X(30).
               10  CBETM                   PICTURE X(2).
               10  KBADR1                  PICTURE X(30).
               10  KBADR2                  PICTURE X(30).
               10  KBSTED                  PICTURE X(15).
               10  KBPNR                   PICTURE X(4).
               10  KEY10                   PICTURE X(10).
               10  TLF                     PICTURE X(16).
               10  SNR1                    PICTURE X(3).
               10  ORGNR                   PICTURE X(9).
               10  KPERS1                  PICTURE X(30).
               10  KPERS2                  PICTURE X(30).
               10  KFKNR                   PICTURE X(6).
               10  KTONR                   PICTURE X(11).
               10  EMAIL                   PICTURE X(40).
               10  MAILA1                  PICTURE X(40).
               10  MAILB1                  PICTURE X(40).
               10  MAILC1                  PICTURE X(40).
               10  MAILD1                  PICTURE X(40).
               10  MAILE1                  PICTURE X(40).
               10  RAFNR                   PICTURE X(4).
               10  MORD                    PICTURE X(10).
               10  KEY20                   PICTURE X(20).
               10  HJKY1                   PICTURE X(4).
               10  HJKY2                   PICTURE X(8).
               10  KTOKY2                  PICTURE X(12).
               10  KEY-X                   PICTURE X(7).
               10  NSMVA-IO.
                   15  NSMVA               PICTURE S9(7)V9(2).
               10  MVA2-IO.
                   15  MVA2                PICTURE S9(7)V9(2).
               10  SUML3C                  PICTURE X(7).
               10  SUML3A                  PICTURE X(2).
               10  SUML3F                  PICTURE X(7).
               10  SUML3H                  PICTURE X(2).
               10  NORE-IO.
                   15  NORE                PICTURE S9(2).
               10  NOREA                   PICTURE X(2).
               10  F6                      PICTURE X(6).
               10  F7                      PICTURE X(7).
               10  FAKKID                  PICTURE X(7).
               10  FDDMM-IO.
                   15  FDDMM               PICTURE S9(4).
               10  FYY-IO.
                   15  FYY                 PICTURE S9(2).
               10  FDD-IO.
                   15  FDD                 PICTURE S9(2).
               10  FMM-IO.
                   15  FMM                 PICTURE S9(2).
               10  NTOM-IO.
                   15  NTOM                PICTURE S9(7)V9(2).
               10  NTO01E-IO.
                   15  NTO01E              PICTURE S9(7).
               10  NTO01F                  PICTURE X(7).
               10  NTO01G-IO.
                   15  NTO01G              PICTURE S9(2).
               10  NTO01H                  PICTURE X(2).
               10  NTO01U-IO.
                   15  NTO01U              PICTURE S9(7)V9(2).
               10  SUML3M-IO.
                   15  SUML3M              PICTURE S9(7)V9(2).
               10  SUML3U-IO.
                   15  SUML3U              PICTURE S9(7)V9(2).
               10  OREDIF-IO.
                   15  OREDIF              PICTURE S9(7)V9(2).
               10  OREDI1-IO.
                   15  OREDI1              PICTURE S9(7).
               10  OREDI2                  PICTURE X(7).
               10  OREDI3-IO.
                   15  OREDI3              PICTURE S9(2).
               10  OREDI4                  PICTURE X(2).
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
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-04                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-05                    TO TRUE
           SET NOT-I-06                    TO TRUE
           SET NOT-I-08                    TO TRUE
           SET NOT-I-07                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  INNFIL-PROCESS
               SET INNFIL-PROCESS-OFF      TO TRUE
               SET INNFIL-READ             TO TRUE
           END-IF
 
           IF  INNFIL-READ
           AND RECORD-SELECTED-OFF
               PERFORM INNFIL-GET
               SET INNFIL-READ-OFF         TO TRUE
               IF  NOT INNFIL-EOF
                   SET INNFIL-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  INNFIL-PROCESS
               PERFORM INNFIL-IDSET
           END-IF
 
           IF  INNFIL-PROCESS
               PERFORM INNFIL-CHK-LEVEL
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
 
           IF  INNFIL-PROCESS
               PERFORM INNFIL-FLDOFF
               PERFORM INNFIL-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  INNFIL-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-50                    TO TRUE
           SET NOT-I-11                    TO TRUE
           SET NOT-I-21                    TO TRUE
           SET NOT-I-25                    TO TRUE
           SET NOT-I-14                    TO TRUE
           SET NOT-I-12                    TO TRUE
           IF  (I-L3)
               SET NOT-I-47                TO TRUE
               SET NOT-I-48                TO TRUE
               SET NOT-I-34                TO TRUE
               SET NOT-I-35                TO TRUE
               SET NOT-I-36                TO TRUE
      *  L5                SETON                     43
      *
           END-IF
           IF  (I-L5)
               MOVE FIRMA                  TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-55                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-55            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
               SET NOT-I-95                TO TRUE
               IF  FFKODE = 'U'
                   SET I-95                TO TRUE
               END-IF
           END-IF
           IF  (I-L5 AND NOT-I-23)
               READ FAKPAR
               AT END
                   SET I-H0                TO TRUE
                   MOVE 'M'                TO E-R-R-O-R
               NOT AT END
                   PERFORM FAKPAR-FLDSET
                   PERFORM FAKPAR-IDSET
               END-READ
           END-IF
           IF  (I-L5)
               SET I-23                    TO TRUE
               SET NOT-I-98                TO TRUE
               IF  KRFFD = 'K'
                   SET I-98                TO TRUE
               END-IF
               MOVE UFADTO                 TO FOFALL
           END-IF
           MOVE UDATE (1:4)                TO UDDMM
           MOVE UDATE (5:2)                TO UYY-IO
           MOVE UDDMM (1:2)                TO UDD
           MOVE UDDMM (3:2)                TO UMM-IO
           MOVE 0                          TO NULL92
      *  LEGGE INN FORRIGE MÅNED I PERIODE
           SUBTRACT 1 FROM UMM         GIVING PERIOD
           SET NOT-I-58                    TO TRUE
           IF  UMM = 1
               SET I-58                    TO TRUE
           END-IF
           IF  (I-58)
               MOVE 12                     TO PERIOD
      *****************************************************************
      * NULLSTILLER                                                   *
      *****************************************************************
           END-IF
           IF  (I-L3)
               MOVE 0                      TO SUML3
               MOVE 0                      TO SUML3B
               MOVE 0                      TO SUML3O
               MOVE 0                      TO SUML3K
               MOVE 0                      TO SUML3E
               MOVE 0                      TO SUML3G
               MOVE 0                      TO SUM01M
      *  L3                Z-ADD0         SUML3M  92
      *  L3                Z-ADD0         SUML3U  92
      *****************************************************************
      * HENTE DATA FRA KUNDEMA OG KUNDEMX                             *
      *****************************************************************
           END-IF
           MOVE KONSF                      TO FNRRNR (1:3)
           MOVE KUNDNR                     TO FNRRNR (4:6)
           MOVE FNRRNR                     TO KUNDEMA-KEY1
           READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
           INVALID KEY
               SET I-81                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-81                TO TRUE
               PERFORM KUNDEMA-FLDSET
               PERFORM KUNDEMA-IDSET
           END-READ
           MOVE KMNAVN                     TO KBNAVN
           MOVE CBETMK                     TO CBETM
           MOVE KMADR1                     TO KBADR1
           MOVE KMADR2                     TO KBADR2
           MOVE KMSTED                     TO KBSTED
           MOVE KMPNR                      TO KBPNR
      *  RECART 1
           MOVE FNRRNR                     TO KEY10 (1:9)
           MOVE '1'                        TO KEY10 (10:1)
           MOVE KEY10                      TO KUNDEMX-KEY1
           READ KUNDEMX RECORD KEY IS KUNDEMX-KEY1
           INVALID KEY
               SET I-82                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-82                TO TRUE
               PERFORM KUNDEMX-FLDSET
               PERFORM KUNDEMX-IDSET
           END-READ
           IF  (I-01 AND NOT-I-82)
               MOVE TL1                    TO TLF
               MOVE SNR                    TO SNR1
               MOVE ORGN1                  TO ORGNR
               MOVE PERS1                  TO KPERS1
               MOVE PERS2                  TO KPERS2
               MOVE FKNR                   TO KFKNR
               MOVE BANKGI                 TO KTONR
      *  RECART 6
           END-IF
           IF  (I-01)
               MOVE '6'                    TO KEY10 (10:1)
               MOVE KEY10                  TO KUNDEMX-KEY1
               READ KUNDEMX RECORD KEY IS KUNDEMX-KEY1
               INVALID KEY
                   SET I-83                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-83            TO TRUE
                   PERFORM KUNDEMX-FLDSET
                   PERFORM KUNDEMX-IDSET
               END-READ
           END-IF
           IF  (I-01 AND NOT-I-83)
               MOVE EMAIL6                 TO EMAIL
      *  RECART A
           END-IF
           IF  (I-01)
               MOVE 'A'                    TO KEY10 (10:1)
               MOVE KEY10                  TO KUNDEMX-KEY1
               READ KUNDEMX RECORD KEY IS KUNDEMX-KEY1
               INVALID KEY
                   SET I-73                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-73            TO TRUE
                   PERFORM KUNDEMX-FLDSET
                   PERFORM KUNDEMX-IDSET
               END-READ
           END-IF
           IF  (I-01 AND NOT-I-73)
               MOVE MAILA                  TO MAILA1
      *  RECART B
           END-IF
           IF  (I-01)
               MOVE 'B'                    TO KEY10 (10:1)
               MOVE KEY10                  TO KUNDEMX-KEY1
               READ KUNDEMX RECORD KEY IS KUNDEMX-KEY1
               INVALID KEY
                   SET I-74                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-74            TO TRUE
                   PERFORM KUNDEMX-FLDSET
                   PERFORM KUNDEMX-IDSET
               END-READ
           END-IF
           IF  (I-01 AND NOT-I-74)
               MOVE MAILB                  TO MAILB1
      *  RECART C
           END-IF
           IF  (I-01)
               MOVE 'C'                    TO KEY10 (10:1)
               MOVE KEY10                  TO KUNDEMX-KEY1
               READ KUNDEMX RECORD KEY IS KUNDEMX-KEY1
               INVALID KEY
                   SET I-75                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-75            TO TRUE
                   PERFORM KUNDEMX-FLDSET
                   PERFORM KUNDEMX-IDSET
               END-READ
           END-IF
           IF  (I-01 AND NOT-I-75)
               MOVE MAILC                  TO MAILC1
      *  RECART D
           END-IF
           IF  (I-01)
               MOVE 'D'                    TO KEY10 (10:1)
               MOVE KEY10                  TO KUNDEMX-KEY1
               READ KUNDEMX RECORD KEY IS KUNDEMX-KEY1
               INVALID KEY
                   SET I-76                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-76            TO TRUE
                   PERFORM KUNDEMX-FLDSET
                   PERFORM KUNDEMX-IDSET
               END-READ
           END-IF
           IF  (I-01 AND NOT-I-76)
               MOVE MAILD                  TO MAILD1
      *  RECART E
           END-IF
           IF  (I-01)
               MOVE 'E'                    TO KEY10 (10:1)
               MOVE KEY10                  TO KUNDEMX-KEY1
               READ KUNDEMX RECORD KEY IS KUNDEMX-KEY1
               INVALID KEY
                   SET I-77                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-77            TO TRUE
                   PERFORM KUNDEMX-FLDSET
                   PERFORM KUNDEMX-IDSET
               END-READ
           END-IF
           IF  (I-01 AND NOT-I-77)
               MOVE MAILE                  TO MAILE1
      *  RECART 2
           END-IF
           IF  (I-01)
               MOVE '2'                    TO KEY10 (10:1)
               MOVE KEY10                  TO KUNDEMX-KEY1
               READ KUNDEMX RECORD KEY IS KUNDEMX-KEY1
               INVALID KEY
                   SET I-70                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-70            TO TRUE
                   PERFORM KUNDEMX-FLDSET
                   PERFORM KUNDEMX-IDSET
               END-READ
      *****************************************************************
      * HENTE DATA FRA ORDREMASTER.                                   *
      *****************************************************************
           END-IF
           MOVE 'O'                        TO RAFNR (1:1)
           MOVE FIRMA                      TO RAFNR (2:3)
           MOVE RAFNR                      TO MORD (1:4)
           MOVE ORDNR                      TO MORD (5:6)
           MOVE MORD                       TO KEY20 (1:10)
           MOVE '1'                        TO KEY20 (20:1)
           MOVE KEY20                      TO ORDREM-KEY1
           READ ORDREM RECORD KEY IS ORDREM-KEY1
           INVALID KEY
               SET I-20                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-20                TO TRUE
               PERFORM ORDREM-FLDSET
               PERFORM ORDREM-IDSET
           END-READ
           IF  (I-20)
               MOVE '1'                    TO KEY20 (1:1)
               MOVE KEY20                  TO ORDREM-KEY1
               READ ORDREM RECORD KEY IS ORDREM-KEY1
               INVALID KEY
                   SET I-20                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-20            TO TRUE
                   PERFORM ORDREM-FLDSET
                   PERFORM ORDREM-IDSET
               END-READ
           END-IF
           IF  (NOT-I-20)
               SET NOT-I-21                TO TRUE
               IF  MVAFRI = '1'
                   SET I-21                TO TRUE
               END-IF
               SET NOT-I-22                TO TRUE
               IF  BK = 'K'
                   SET I-22                TO TRUE
               END-IF
           END-IF
           IF  (I-21)
               SET NOT-I-25                TO TRUE
               IF  KONTO = '3000'
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-22)
               SET NOT-I-26                TO TRUE
               IF  KREDGR = 0
                   SET I-26                TO TRUE
               END-IF
      ***  HENTER ALTERNATIVT KONTONR FRA KONTOKY   ***
           END-IF
           MOVE KONSF                      TO HJKY1 (1:3)
           MOVE 'C'                        TO HJKY1 (4:1)
           MOVE KONTO                      TO HJKY2 (1:4)
           MOVE '    '                     TO KTOKY2 (9:4)
           MOVE HJKY1                      TO KTOKY2 (1:4)
           MOVE HJKY2                      TO KTOKY2 (5:8)
           MOVE KTOKY2                     TO KONTOKY-KEY1
           READ KONTOKY RECORD KEY IS KONTOKY-KEY1
           INVALID KEY
               SET I-13                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-13                TO TRUE
               PERFORM KONTOKY-FLDSET
               PERFORM KONTOKY-IDSET
           END-READ
           IF  (I-13)
               MOVE KONTO                  TO ADSKTO
      *  13                MOVE "*FF*"    ADSKTO
      *****************************************************************
      * CHAINER MOT KONTOMA FOR Å FINNE RIKTIG KONTO                  *
      *****************************************************************
           END-IF
           MOVE KONSF                      TO KEY-X (1:3)
           MOVE KONTO                      TO KEY-X (4:4)
      *                    MOVE ADSKTO    KEY
           MOVE KEY-X                      TO KONTOMA-KEY1
           READ KONTOMA RECORD KEY IS KONTOMA-KEY1
           INVALID KEY
               SET I-10                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-10                TO TRUE
               PERFORM KONTOMA-FLDSET
               PERFORM KONTOMA-IDSET
           END-READ
           IF  (NOT-I-10)
               SET NOT-I-11                TO TRUE
               IF  KKONTO = '8055'
                   SET I-11                TO TRUE
               END-IF
      *****************************************************************
      * CHAINER MOT KONTOMA FOR Å FINNE RIKTIG KONTO, KONTO FRA KONTOKY
      *****************************************************************
           END-IF
           IF  (I-10)
               MOVE KONSF                  TO KEY-X (1:3)
               MOVE ADSKTO                 TO KEY-X (4:4)
               MOVE KEY-X                  TO KONTOMA-KEY1
               READ KONTOMA RECORD KEY IS KONTOMA-KEY1
               INVALID KEY
                   SET I-17                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-17            TO TRUE
                   PERFORM KONTOMA-FLDSET
                   PERFORM KONTOMA-IDSET
               END-READ
      *****************************************************************
      * RUTINE FOR BEREGNING AV FAKTURADAG                         *
      *****************************************************************
           END-IF
           SET NOT-I-60                    TO TRUE
           IF  FAKOMG = 'A'
               SET I-60                    TO TRUE
           END-IF
           IF  (NOT-I-60)
               SET NOT-I-60                TO TRUE
               IF  FAKOMG = '1'
                   SET I-60                TO TRUE
               END-IF
           END-IF
           SET NOT-I-61                    TO TRUE
           IF  FAKOMG = 'B'
               SET I-61                    TO TRUE
           END-IF
           IF  (NOT-I-61)
               SET NOT-I-61                TO TRUE
               IF  FAKOMG = '2'
                   SET I-61                TO TRUE
               END-IF
           END-IF
           SET NOT-I-62                    TO TRUE
           IF  FAKOMG = 'C'
               SET I-62                    TO TRUE
           END-IF
           IF  (NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  FAKOMG = '3'
                   SET I-62                TO TRUE
               END-IF
           END-IF
           SET NOT-I-63                    TO TRUE
           IF  FAKOMG = 'D'
               SET I-63                    TO TRUE
           END-IF
           IF  (NOT-I-63)
               SET NOT-I-63                TO TRUE
               IF  FAKOMG = '4'
                   SET I-63                TO TRUE
               END-IF
           END-IF
           SET NOT-I-64                    TO TRUE
           IF  FMND = '01'
               SET I-64                    TO TRUE
           END-IF
           IF  (NOT-I-64)
               SET NOT-I-64                TO TRUE
               IF  FMND = '03'
                   SET I-64                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-64)
               SET NOT-I-64                TO TRUE
               IF  FMND = '05'
                   SET I-64                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-64)
               SET NOT-I-64                TO TRUE
               IF  FMND = '07'
                   SET I-64                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-64)
               SET NOT-I-64                TO TRUE
               IF  FMND = '08'
                   SET I-64                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-64)
               SET NOT-I-64                TO TRUE
               IF  FMND = '10'
                   SET I-64                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-64)
               SET NOT-I-64                TO TRUE
               IF  FMND = '12'
                   SET I-64                TO TRUE
               END-IF
           END-IF
           SET NOT-I-65                    TO TRUE
           IF  FMND = '02'
               SET I-65                    TO TRUE
           END-IF
           SET NOT-I-66                    TO TRUE
           IF  FMND = '04'
               SET I-66                    TO TRUE
           END-IF
           IF  (NOT-I-66)
               SET NOT-I-66                TO TRUE
               IF  FMND = '06'
                   SET I-66                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-66)
               SET NOT-I-66                TO TRUE
               IF  FMND = '09'
                   SET I-66                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-66)
               SET NOT-I-66                TO TRUE
               IF  FMND = '11'
                   SET I-66                TO TRUE
               END-IF
      *****************************************************************
      * DIVERSE SJEKKER OG UTREGNINGER                                *
      *****************************************************************
           END-IF
           SET NOT-I-27                    TO TRUE
           IF  KONTO = '3000'
               SET I-27                    TO TRUE
           END-IF
      *          KONTO     COMP "5000"                   41
      *          KONTO     COMP "5600"                   42
      *  27                MOVE "4300"    VKONTO  4
      *  41                MOVE "3000"    VKONTO  4
      *  42                MOVE "3130"    VKONTO  4
           SET NOT-I-12                    TO TRUE
           IF  ORD1 = '9'
               SET I-12                    TO TRUE
           END-IF
           SET NOT-I-18                    TO TRUE
           IF  EDB3 = '995'
               SET I-18                    TO TRUE
           END-IF
           IF  (NOT-I-18)
               SET NOT-I-18                TO TRUE
               IF  EDB2 = '94'
                   SET I-18                TO TRUE
               END-IF
           END-IF
           SET NOT-I-51                    TO TRUE
           IF  FAKT1 = '9'
               SET I-51                    TO TRUE
           END-IF
           SET NOT-I-53                    TO TRUE
           IF  FAKT1 = '0'
               SET I-53                    TO TRUE
           END-IF
           IF  (I-53 AND I-12)
               SET I-51                    TO TRUE
           END-IF
           IF  (I-11)
               OR  (I-21)
               SET I-50                    TO TRUE
           END-IF
           IF  (I-53)
               SET NOT-I-34                TO TRUE
               IF  KUNDNR NOT < '500190'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-53 AND I-34)
               SET NOT-I-35                TO TRUE
               IF  KUNDNR NOT > '500999'
                   SET I-35                TO TRUE
               END-IF
           END-IF
           IF  (I-53)
               SET NOT-I-36                TO TRUE
               IF  BM = '07'
                   SET I-36                TO TRUE
               END-IF
           END-IF
           IF  (I-11)
               SET NOT-I-14                TO TRUE
               IF  NTOSUM < 0
                   SET I-14                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-11 AND NOT-I-21)
               MULTIPLY 1,25 BY NTOSUM GIVING NSMVA
           END-IF
           IF  (I-50)
               ADD NTOSUM TO ZERO      GIVING NSMVA
           END-IF
           SUBTRACT NTOSUM FROM NSMVA  GIVING MVA2
      *
           ADD NTOSUM                      TO SUML3
           ADD NSMVA                       TO SUML3K
      *          SUML3     COMP 0,00                   54
      *
           MOVE SUML3 (1:7)                TO SUML3B
           MOVE SUML3B                     TO SUML3C
      ** MLLzo
           MOVE '1'                        TO BW-A-2
           MULTIPLY 16                     BY BW-A
           MOVE SUML3C (7:1)               TO BW-B-2
           MULTIPLY 16                     BY BW-B
           MOVE BW-A-1                     TO BW-B-1
           DIVIDE 16                       INTO BW-B
           MOVE BW-B-2                     TO SUML3C (7:1)
      *
           MOVE SUML3 (8:2)                TO SUML3O-IO
           MOVE SUML3O                     TO SUML3A
      ** MLLzo
           MOVE '1'                        TO BW-A-2
           MULTIPLY 16                     BY BW-A
           MOVE SUML3A (2:1)               TO BW-B-2
           MULTIPLY 16                     BY BW-B
           MOVE BW-A-1                     TO BW-B-1
           DIVIDE 16                       INTO BW-B
           MOVE BW-B-2                     TO SUML3A (2:1)
      *
           MOVE SUML3K (1:7)               TO SUML3E
           MOVE SUML3E                     TO SUML3F
      ** MLLzo
           MOVE '1'                        TO BW-A-2
           MULTIPLY 16                     BY BW-A
           MOVE SUML3F (7:1)               TO BW-B-2
           MULTIPLY 16                     BY BW-B
           MOVE BW-A-1                     TO BW-B-1
           DIVIDE 16                       INTO BW-B
           MOVE BW-B-2                     TO SUML3F (7:1)
      *
           MOVE SUML3K (8:2)               TO SUML3G-IO
           MOVE SUML3G                     TO SUML3H
      ** MLLzo
           MOVE '1'                        TO BW-A-2
           MULTIPLY 16                     BY BW-A
           MOVE SUML3H (2:1)               TO BW-B-2
           MULTIPLY 16                     BY BW-B
           MOVE BW-A-1                     TO BW-B-1
           DIVIDE 16                       INTO BW-B
           MOVE BW-B-2                     TO SUML3H (2:1)
      *
           ADD NTOORE TO ZERO          GIVING NORE
           MOVE NORE                       TO NOREA
      ** MLLzo
           MOVE '1'                        TO BW-A-2
           MULTIPLY 16                     BY BW-A
           MOVE NOREA (2:1)                TO BW-B-2
           MULTIPLY 16                     BY BW-B
           MOVE BW-A-1                     TO BW-B-1
           DIVIDE 16                       INTO BW-B
           MOVE BW-B-2                     TO NOREA (2:1)
      *
      *****************************************************************
      * BEREGNE MODULUS 10 KONTROLLSIFFER FOR FAKTURANUMMER           *
      *****************************************************************
           MOVE FAKTNR                     TO F6
      *R                   MLLZO"1"       F6                 FJERN ZONE
           MOVE F6                         TO F7 (1:6)
           MOVE '0'                        TO F7 (7:1)
           MOVE F7                         TO KITALL (1:7)
           MOVE 'T'                        TO KITYPE
           MOVE ' '                        TO KIKTR
           CALL 'MODULUS' USING KIFELT-XX-DATA-FIELDS
           MOVE F6                         TO FAKKID (1:6)
           MOVE KIKTR                      TO FAKKID (7:1)
      *
      *  INDIKATOR 95 BETYR AT FIRMNR"ET HAR UKENTLIG FORFALL.
      *
           IF  (I-53)
               GO TO ENDFO-T
           END-IF
           MOVE PFADTO                     TO FOFALL
           IF  (I-95)
               MOVE UFADTO                 TO FOFALL
      *****************************************************************
      *  INDIKATOR 98 BETYR AT KREDINOTAER SKAL HA ORGINAL BETM.
      *  IKKE OM ORDRE HAR KONTANT ELLER OPPKRAV (07 / 14)
      *****************************************************************
      *  98      BTMÅTE    COMP "07"                 9898   KONTANT
      *  98      BTMÅTE    COMP "14"                 9898   OPPKRAV.
      *  98      CBETM     COMP "07"                 9898   KONTANT
      *  98      CBETM     COMP "14"                 9898   OPPKRAV
      *  98                MOVE CBETM     BTMÅTE            KR.NOTA 915/940
      *  98                GOTO B2
      *                    GOTO C1
      *          B2        TAG
           END-IF
           SET NOT-I-56                    TO TRUE
           IF  BTMA-ELGTE = '09'
               SET I-56                    TO TRUE
           END-IF
           IF  (I-56)
               GO TO B09-T
           END-IF
           SET NOT-I-56                    TO TRUE
           IF  BTMA-ELGTE = '13'
               SET I-56                    TO TRUE
           END-IF
           IF  (I-56)
               GO TO B13-T
           END-IF
           SET NOT-I-56                    TO TRUE
           IF  BTMA-ELGTE = '21'
               SET I-56                    TO TRUE
           END-IF
           IF  (I-56)
               GO TO B21-T
           END-IF
           SET NOT-I-56                    TO TRUE
           IF  BTMA-ELGTE = '22'
               SET I-56                    TO TRUE
           END-IF
           IF  (I-56)
               GO TO B22-T
           END-IF
           SET NOT-I-56                    TO TRUE
           IF  BTMA-ELGTE = '23'
               SET I-56                    TO TRUE
           END-IF
           IF  (I-56)
               GO TO B23-T
           END-IF
           SET NOT-I-96                    TO TRUE
           IF  BTMA-ELGTE = '15'
               SET I-96                    TO TRUE
           END-IF
           IF  (I-96)
               GO TO B4-T
           END-IF
           SET NOT-I-96                    TO TRUE
           IF  BTMA-ELGTE = '31'
               SET I-96                    TO TRUE
           END-IF
           IF  (I-96)
               GO TO B4-T
           END-IF
           SET NOT-I-28                    TO TRUE
           IF  BTMA-ELGTE = '16'
               SET I-28                    TO TRUE
           END-IF
           IF  (NOT-I-28)
               SET NOT-I-28                TO TRUE
               IF  BTMA-ELGTE = '34'
                   SET I-28                TO TRUE
               END-IF
           END-IF
           IF  (I-28)
               GO TO B5-T
           END-IF
           SET NOT-I-29                    TO TRUE
           IF  BTMA-ELGTE = '07'
               SET I-29                    TO TRUE
           END-IF
           IF  (NOT-I-29)
               SET NOT-I-29                TO TRUE
               IF  BTMA-ELGTE = '14'
                   SET I-29                TO TRUE
               END-IF
           END-IF
           IF  (I-29)
               GO TO B6-T
           END-IF
           SET NOT-I-71                    TO TRUE
           IF  BTMA-ELGTE = '08'
               SET I-71                    TO TRUE
           END-IF
           IF  (NOT-I-71)
               SET NOT-I-71                TO TRUE
               IF  BTMA-ELGTE = '33'
                   SET I-71                TO TRUE
               END-IF
           END-IF
           IF  (I-71)
               GO TO B7-T
           END-IF
           SET NOT-I-57                    TO TRUE
           IF  BTMA-ELGTE = '18'
               SET I-57                    TO TRUE
           END-IF
           IF  (I-57)
               GO TO B6B-T
           END-IF
           SET NOT-I-59                    TO TRUE
           IF  BTMA-ELGTE = '20'
               SET I-59                    TO TRUE
           END-IF
           IF  (I-59)
               GO TO B6B-T
           END-IF
           SET NOT-I-30                    TO TRUE
           IF  BTMA-ELGTE = '10'
               SET I-30                    TO TRUE
           END-IF
           IF  (NOT-I-30)
               SET NOT-I-30                TO TRUE
               IF  BTMA-ELGTE = '05'
                   SET I-30                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-30)
               SET NOT-I-30                TO TRUE
               IF  BTMA-ELGTE = '32'
                   SET I-30                TO TRUE
               END-IF
           END-IF
           IF  (I-30)
               GO TO B7A-T
           END-IF
           SET NOT-I-30                    TO TRUE
           IF  BTMA-ELGTE = '17'
               SET I-30                    TO TRUE
           END-IF
           IF  (NOT-I-30)
               SET NOT-I-30                TO TRUE
               IF  BTMA-ELGTE = '35'
                   SET I-30                TO TRUE
               END-IF
           END-IF
           IF  (I-30)
               GO TO B8A-T
           END-IF
           SET NOT-I-09                    TO TRUE
           IF  BTMA-ELGTE = '02'
               SET I-09                    TO TRUE
           END-IF
           IF  (I-09)
               GO TO B6C-T
           END-IF
           SET NOT-I-09                    TO TRUE
           IF  BTMA-ELGTE = '12'
               SET I-09                    TO TRUE
           END-IF
           IF  (I-09)
               GO TO B6C-T
           END-IF
           SET NOT-I-09                    TO TRUE
           IF  BTMA-ELGTE = '30'
               SET I-09                    TO TRUE
           END-IF
           IF  (I-09)
               GO TO B6C-T
           END-IF
           SET NOT-I-16                    TO TRUE
           IF  BTMA-ELGTE = '25'
               SET I-16                    TO TRUE
           END-IF
           IF  (NOT-I-16)
               SET NOT-I-16                TO TRUE
               IF  BTMA-ELGTE = '36'
                   SET I-16                TO TRUE
               END-IF
           END-IF
           IF  (I-16)
               GO TO B9-T
           END-IF
           SET NOT-I-67                    TO TRUE
           IF  BTMA-ELGTE = '26'
               SET I-67                    TO TRUE
           END-IF
           IF  (NOT-I-67)
               SET NOT-I-67                TO TRUE
               IF  BTMA-ELGTE = '37'
                   SET I-67                TO TRUE
               END-IF
           END-IF
           IF  (I-67)
               GO TO B945-T
           END-IF
           SET NOT-I-94                    TO TRUE
           IF  BTMA-ELGTE = '27'
               SET I-94                    TO TRUE
           END-IF
           IF  (NOT-I-94)
               SET NOT-I-94                TO TRUE
               IF  BTMA-ELGTE = '38'
                   SET I-94                TO TRUE
               END-IF
           END-IF
           IF  (I-94)
               GO TO B975-T
           END-IF
           SET NOT-I-79                    TO TRUE
           IF  BTMA-ELGTE = '28'
               SET I-79                    TO TRUE
           END-IF
           IF  (NOT-I-79)
               SET NOT-I-79                TO TRUE
               IF  BTMA-ELGTE = '39'
                   SET I-79                TO TRUE
               END-IF
           END-IF
           IF  (I-79)
               GO TO B990-T
           END-IF
           GO TO B7B-T.
 
       B7A-T.
           SET NOT-I-31                    TO TRUE
           IF  PFDAG NOT > '15'
               SET I-31                    TO TRUE
           END-IF
           IF  (I-31)
               GO TO B7-T
           END-IF
           GO TO B7B-T.
 
       B8A-T.
           SET NOT-I-31                    TO TRUE
           IF  PFDAG NOT > '15'
               SET I-31                    TO TRUE
           END-IF
           IF  (I-31)
               GO TO B8-T
           END-IF
           GO TO B8B-T.
 
       B9-T.
           SET NOT-I-31                    TO TRUE
           IF  PFDAG NOT > '15'
               SET I-31                    TO TRUE
           END-IF
           IF  (I-31)
               GO TO B9A-T
           END-IF
           GO TO B9B-T.
 
       B945-T.
           SET NOT-I-31                    TO TRUE
           IF  PFDAG NOT > '15'
               SET I-31                    TO TRUE
           END-IF
           IF  (I-31)
               GO TO B945A-T
           END-IF
           GO TO B945B-T.
 
       B975-T.
           SET NOT-I-31                    TO TRUE
           IF  PFDAG NOT > '15'
               SET I-31                    TO TRUE
           END-IF
           IF  (I-31)
               GO TO B975A-T
           END-IF
           GO TO B975B-T.
 
       B990-T.
           SET NOT-I-31                    TO TRUE
           IF  PFDAG NOT > '15'
               SET I-31                    TO TRUE
           END-IF
           IF  (I-31)
               GO TO B990A-T
           END-IF
           GO TO B990B-T.
 
       B09-T.
           MOVE PFF90                      TO FOFALL
           IF  (I-95)
               MOVE UFF90                  TO FOFALL
           END-IF
           GO TO C1-T.
 
       B13-T.
           MOVE PFF120                     TO FOFALL
           IF  (I-95)
               MOVE UFF120                 TO FOFALL
           END-IF
           GO TO C1-T.
 
       B21-T.
           MOVE PFF120                     TO FOFALL
           IF  (I-95)
               MOVE UFF120                 TO FOFALL
           END-IF
           GO TO C1-T.
 
       B22-T.
           MOVE PFF150                     TO FOFALL
           IF  (I-95)
               MOVE UFF150                 TO FOFALL
           END-IF
           GO TO C1-T.
 
       B23-T.
           MOVE PFF180                     TO FOFALL
           IF  (I-95)
               MOVE UFF180                 TO FOFALL
           END-IF
           GO TO C1-T.
 
       B4-T.
           MOVE PFF60                      TO FOFALL
           IF  (I-95)
               MOVE UFF60                  TO FOFALL
           END-IF
           GO TO C1-T.
 
       B5-T.
           MOVE PFF90                      TO FOFALL
           IF  (I-95)
               MOVE UFF90                  TO FOFALL
           END-IF
           GO TO C1-T.
 
       B6-T.
           MOVE PFADTO                     TO FOFALL
           IF  (I-95)
               MOVE UFADTO                 TO FOFALL
           END-IF
           GO TO C1-T.
 
       B6B-T.
           MOVE PFF30                      TO FOFALL
           IF  (I-95)
               MOVE UFF30                  TO FOFALL
           END-IF
           GO TO C1-T.
 
       B6C-T.
           MOVE PFF15                      TO FOFALL
           IF  (I-95)
               MOVE UFF15                  TO FOFALL
           END-IF
           GO TO C1-T.
 
       B8-T.
           MOVE PFF75                      TO FOFALL
           IF  (I-95 AND NOT-I-30)
               MOVE UFF75                  TO FOFALL
           END-IF
           GO TO C1-T.
 
       B8B-T.
           MOVE PFF60                      TO FOFALL
           IF  (I-95 AND NOT-I-30)
               MOVE UFF60                  TO FOFALL
           END-IF
           GO TO C1-T.
 
       B7-T.
           MOVE PFF45                      TO FOFALL
           IF  (I-95 AND NOT-I-30)
               MOVE UFF45                  TO FOFALL
           END-IF
           GO TO C1-T.
 
       B7B-T.
           MOVE PFF30                      TO FOFALL
           IF  (I-95 AND NOT-I-30)
               MOVE UFF30                  TO FOFALL
           END-IF
           GO TO C1-T.
 
       B9A-T.
           MOVE PFF30                      TO FOFALL
           IF  (I-95 AND NOT-I-16)
               MOVE UFF30                  TO FOFALL
           END-IF
           GO TO C1-T.
 
       B9B-T.
           MOVE PFF15                      TO FOFALL
           IF  (I-95 AND NOT-I-16)
               MOVE UFF15                  TO FOFALL
           END-IF
           GO TO C1-T.
 
       B945A-T.
           MOVE PFF60                      TO FOFALL
           IF  (I-95 AND NOT-I-67)
               MOVE UFF60                  TO FOFALL
           END-IF
           GO TO C1-T.
 
       B945B-T.
           MOVE PFF45                      TO FOFALL
           IF  (I-95 AND NOT-I-67)
               MOVE UFF45                  TO FOFALL
           END-IF
           GO TO C1-T.
 
       B975A-T.
           MOVE PFF90                      TO FOFALL
           IF  (I-95 AND NOT-I-94)
               MOVE UFF90                  TO FOFALL
           END-IF
           GO TO C1-T.
 
       B975B-T.
           MOVE PFF75                      TO FOFALL
           IF  (I-95 AND NOT-I-94)
               MOVE UFF75                  TO FOFALL
           END-IF
           GO TO C1-T.
 
       B990A-T.
           MOVE PFF105                     TO FOFALL
           IF  (I-95 AND NOT-I-79)
               MOVE UFF105                 TO FOFALL
           END-IF
           GO TO C1-T.
 
       B990B-T.
           MOVE PFF90                      TO FOFALL
           IF  (I-95 AND NOT-I-79)
               MOVE UFF90                  TO FOFALL
           END-IF
           SET NOT-I-56                    TO TRUE
           IF  BTMA-ELGTE = '24'
               SET I-56                    TO TRUE
           END-IF.
 
       C1-T.
      *****************************************************************
           MOVE FOFALL (1:4)               TO FDDMM
           MOVE FOFALL (5:2)               TO FYY-IO
           MOVE FDDMM (1:2)                TO FDD
           MOVE FDDMM (3:2)                TO FMM-IO.
 
       ENDFO-T.
      *3         SUML3     COMP 0,00                   54
      *****************************************************************
      * SUBRUTINE FOR BEREGNING AV ORDRETOTAL MED AVRUNDING.          *
      *****************************************************************
           ADD NTOSUM TO ZERO          GIVING BUMVA
           MOVE 0                          TO BMMVA
           MOVE 0                          TO MVA
           CALL 'RUTMOMS2' USING MVFELT-XX-DATA-FIELDS
      *                    Z-ADDMVA       MVAAVR  92         MVA M/AVR
           ADD BMMVA TO ZERO           GIVING NTOM
           SET NOT-I-49                    TO TRUE
           IF  NTOM < 0,00
               SET I-49                    TO TRUE
           END-IF
      *
           MOVE NTOM (1:7)                 TO NTO01E
           MOVE NTO01E                     TO NTO01F
      ** MLLzo
           MOVE '1'                        TO BW-A-2
           MULTIPLY 16                     BY BW-A
           MOVE NTO01F (7:1)               TO BW-B-2
           MULTIPLY 16                     BY BW-B
           MOVE BW-A-1                     TO BW-B-1
           DIVIDE 16                       INTO BW-B
           MOVE BW-B-2                     TO NTO01F (7:1)
      *
           MOVE NTOM (8:2)                 TO NTO01G-IO
           MOVE NTO01G                     TO NTO01H
      ** MLLzo
           MOVE '1'                        TO BW-A-2
           MULTIPLY 16                     BY BW-A
           MOVE NTO01H (2:1)               TO BW-B-2
           MULTIPLY 16                     BY BW-B
           MOVE BW-A-1                     TO BW-B-1
           DIVIDE 16                       INTO BW-B
           MOVE BW-B-2                     TO NTO01H (2:1)
      *
           ADD NTOM                        TO SUM01M
      *****************************************************************
      * SUBRUTINE FOR BEREGNING AV ORDRETOTAL UTEN AVRUNDING          *
      *****************************************************************
           ADD NTOSUM TO ZERO          GIVING BUMVA
           MOVE 0                          TO BMMVA
           MOVE 99,99                      TO MVA
           CALL 'RUTMOMS2' USING MVFELT-XX-DATA-FIELDS
      *                    Z-ADDMVA       MVAAVR  92         MVA M/AVR
           ADD BMMVA TO ZERO           GIVING NTO01U
      *****************************************************************
      * SUBRUTINE FOR BEREGNING AV ORDRETOTAL MED AVRUNDING.          *
      *****************************************************************
           .
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L3)
               ADD SUML3 TO ZERO       GIVING BUMVA
               MOVE 0                      TO BMMVA
               MOVE 0                      TO MVA
               CALL 'RUTMOMS2' USING MVFELT-XX-DATA-FIELDS
      *3                   Z-ADDMVA       MOMSAR  92         MVA M/AVR
               ADD BMMVA TO ZERO       GIVING SUML3M
               SET NOT-I-54                TO TRUE
               IF  SUML3M < 0,00
                   SET I-54                TO TRUE
               END-IF
      *
           END-IF
           IF  (I-L3)
               MOVE SUML3M (1:7)           TO SUML3E
               MOVE SUML3E                 TO SUML3F
      ** MLLzo
               MOVE '1'                    TO BW-A-2
               MULTIPLY 16                 BY BW-A
               MOVE SUML3F (7:1)           TO BW-B-2
               MULTIPLY 16                 BY BW-B
               MOVE BW-A-1                 TO BW-B-1
               DIVIDE 16                   INTO BW-B
               MOVE BW-B-2                 TO SUML3F (7:1)
      *
           END-IF
           IF  (I-L3)
               MOVE SUML3M (8:2)           TO SUML3G-IO
               MOVE SUML3G                 TO SUML3H
      ** MLLzo
               MOVE '1'                    TO BW-A-2
               MULTIPLY 16                 BY BW-A
               MOVE SUML3H (2:1)           TO BW-B-2
               MULTIPLY 16                 BY BW-B
               MOVE BW-A-1                 TO BW-B-1
               DIVIDE 16                   INTO BW-B
               MOVE BW-B-2                 TO SUML3H (2:1)
      *****************************************************************
      * SUBRUTINE FOR BEREGNING AV ORDRETOTAL UTEN AVRUNDING          *
      *****************************************************************
           END-IF
           IF  (I-L3)
               ADD SUML3 TO ZERO       GIVING BUMVA
               MOVE 0                      TO BMMVA
               MOVE 99,99                  TO MVA
               CALL 'RUTMOMS2' USING MVFELT-XX-DATA-FIELDS
      *3                   Z-ADDMVA       MOMSAR  92         MVA M/AVR
               ADD BMMVA TO ZERO       GIVING SUML3U
      *****************************************************************
      * REGNE UT ØRE DIFF                                             *
      *****************************************************************
           END-IF
           IF  (I-L3)
               SUBTRACT SUM01M FROM SUML3M GIVING OREDIF
               SET NOT-I-47                TO TRUE
               SET NOT-I-48                TO TRUE
               IF  OREDIF < 0,00
                   SET I-47                TO TRUE
               END-IF
               IF  OREDIF = 0,00
                   SET I-48                TO TRUE
               END-IF
      *
           END-IF
           IF  (I-L3)
               MOVE OREDIF (1:7)           TO OREDI1
               MOVE OREDI1                 TO OREDI2
      ** MLLzo
               MOVE '1'                    TO BW-A-2
               MULTIPLY 16                 BY BW-A
               MOVE OREDI2 (7:1)           TO BW-B-2
               MULTIPLY 16                 BY BW-B
               MOVE BW-A-1                 TO BW-B-1
               DIVIDE 16                   INTO BW-B
               MOVE BW-B-2                 TO OREDI2 (7:1)
      *
           END-IF
           IF  (I-L3)
               MOVE OREDIF (8:2)           TO OREDI3-IO
               MOVE OREDI3                 TO OREDI4
      ** MLLzo
               MOVE '1'                    TO BW-A-2
               MULTIPLY 16                 BY BW-A
               MOVE OREDI4 (2:1)           TO BW-B-2
               MULTIPLY 16                 BY BW-B
               MOVE BW-A-1                 TO BW-B-1
               DIVIDE 16                   INTO BW-B
               MOVE BW-B-2                 TO OREDI4 (2:1)
           END-IF.
 
       INNFIL-GET SECTION.
       INNFIL-GET-P.
           IF  INNFIL-EOF-OFF
               READ INNFIL
               AT END
                   SET INNFIL-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INNFIL-FLDOFF SECTION.
       INNFIL-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-52                TO TRUE
           END-EVALUATE.
 
       INNFIL-FLDSET SECTION.
       INNFIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INNFIL-IO-AREA (1:3)   TO FIRMA (1:3)
               MOVE INNFIL-IO-AREA (4:6)   TO KUNDNR (1:6)
               MOVE INNFIL-IO-AREA (1:9)   TO FNRKNR (1:9)
               MOVE INNFIL-IO-AREA (10:6)  TO FAKTNR-IO
               INSPECT FAKTNR-IO REPLACING ALL ' ' BY '0'
               IF  FAKTNR = ZERO
                   SET I-52                TO TRUE
               END-IF
               MOVE INNFIL-IO-AREA (10:1)  TO FAKT1 (1:1)
               MOVE INNFIL-IO-AREA (16:6)  TO ORDNR (1:6)
               MOVE INNFIL-IO-AREA (16:1)  TO ORD1 (1:1)
               MOVE INNFIL-IO-AREA (22:5)  TO VGR (1:5)
               MOVE INNFIL-IO-AREA (27:3)  TO ALFA (1:3)
               MOVE INNFIL-IO-AREA (30:20) TO ARTNR (1:20)
               MOVE INNFIL-IO-AREA (50:7)  TO EDBNR (1:7)
               MOVE INNFIL-IO-AREA (50:2)  TO EDB2 (1:2)
               MOVE INNFIL-IO-AREA (50:3)  TO EDB3 (1:3)
               MOVE INNFIL-IO-AREA (57:2)  TO ORDMOT (1:2)
               MOVE INNFIL-IO-AREA (59:1)  TO FK (1:1)
               MOVE INNFIL-IO-AREA (60:1)  TO KRTYPE (1:1)
               MOVE INNFIL-IO-AREA (61:9)  TO NTOSUM-IO
               INSPECT NTOSUM-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (61:7)  TO NTOKR-IO
               INSPECT NTOKR-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (68:2)  TO NTOORE-IO
               INSPECT NTOORE-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (70:6)  TO FAKMND (1:6)
               MOVE INNFIL-IO-AREA (74:2)  TO FMND (1:2)
               MOVE INNFIL-IO-AREA (70:4)  TO FAKAA (1:4)
               MOVE INNFIL-IO-AREA (72:2)  TO AA2 (1:2)
               MOVE INNFIL-IO-AREA (76:8)  TO ORDDTO (1:8)
               MOVE INNFIL-IO-AREA (76:4)  TO ORAAR-IO
               INSPECT ORAAR-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (80:2)  TO ORMND-IO
               INSPECT ORMND-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (82:2)  TO ORDAG-IO
               INSPECT ORDAG-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (84:7)  TO ANTLEV-IO
               INSPECT ANTLEV-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (91:9)  TO SELVK-IO
               INSPECT SELVK-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (100:1) TO FAKOMG (1:1)
               MOVE INNFIL-IO-AREA (101:4) TO KONTO (1:4)
               MOVE INNFIL-IO-AREA (106:3) TO KONSF (1:3)
               MOVE INNFIL-IO-AREA (199:2) TO BM (1:2)
               MOVE INNFIL-IO-AREA (199:2) TO BTMA-ELGTE (1:2)
           END-EVALUATE.
 
       INNFIL-IDSET SECTION.
       INNFIL-IDSET-P.
           SET I-01                        TO TRUE.
 
       INNFIL-CHK-LEVEL SECTION.
       INNFIL-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO INNFIL-LEVEL-01
               MOVE INNFIL-IO-AREA (1:3)   TO INNFIL-01-L5-FIRMA
               MOVE INNFIL-IO-AREA (10:6)  TO INNFIL-01-L4-FAKTNR
               MOVE INNFIL-IO-AREA (4:6)   TO INNFIL-01-L3-KUNDNR
               MOVE INNFIL-IO-AREA (16:6)  TO INNFIL-01-L2-ORDNR
               MOVE INNFIL-IO-AREA (22:5)  TO INNFIL-01-L1-VGR
               IF  INNFIL-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INNFIL-01-L5 NOT = THE-PRIOR-L5
                       PERFORM SETON-I-L5
                   WHEN  INNFIL-01-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   WHEN  INNFIL-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  INNFIL-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INNFIL-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INNFIL-01-L5          TO THE-PRIOR-L5
               MOVE  INNFIL-01-L4          TO THE-PRIOR-L4
               MOVE  INNFIL-01-L3          TO THE-PRIOR-L3
               MOVE  INNFIL-01-L2          TO THE-PRIOR-L2
               MOVE  INNFIL-01-L1          TO THE-PRIOR-L1
               SET INNFIL-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       KONTOMA-FLDSET SECTION.
       KONTOMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KONTOMA-IO-AREA (45:4) TO KKONTO (1:4)
               MOVE KONTOMA-IO-AREA (10:35) TO KONAVN (1:35)
           END-EVALUATE.
 
       KONTOMA-IDSET SECTION.
       KONTOMA-IDSET-P.
           SET I-02                        TO TRUE.
 
       KONTOKY-FLDSET SECTION.
       KONTOKY-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KONTOKY-IO-AREA (13:4) TO ADSKTO (1:4)
           END-EVALUATE.
 
       KONTOKY-IDSET SECTION.
       KONTOKY-IDSET-P.
           SET I-04                        TO TRUE.
 
       ORDREM-FLDSET SECTION.
       ORDREM-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE ORDREM-IO-AREA (89:1)  TO MVAFRI (1:1)
               MOVE ORDREM-IO-AREA (92:1)  TO BK (1:1)
           END-EVALUATE.
 
       ORDREM-IDSET SECTION.
       ORDREM-IDSET-P.
           SET I-03                        TO TRUE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (16:30) TO KMNAVN (1:30)
               MOVE KUNDEMA-IO-AREA (46:30) TO KMADR1 (1:30)
               MOVE KUNDEMA-IO-AREA (76:30) TO KMADR2 (1:30)
               MOVE KUNDEMA-IO-AREA (106:15) TO KMSTED (1:15)
               MOVE KUNDEMA-IO-AREA (121:4) TO KMPNR (1:4)
               MOVE KUNDEMA-IO-AREA (127:2) TO CBETMK (1:2)
               MOVE KUNDEMA-IO-AREA (129:4) TO KREDGR-IO
               INSPECT KREDGR-IO REPLACING ALL ' ' BY '0'
               MOVE KUNDEMA-IO-AREA (133:11) TO BANKGI (1:11)
               MOVE KUNDEMA-IO-AREA (185:3) TO HND-IO
               INSPECT HND-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-05                        TO TRUE.
 
       KUNDEMX-FLDSET SECTION.
       KUNDEMX-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMX-IO-AREA (11:16) TO TL1 (1:16)
               MOVE KUNDEMX-IO-AREA (45:30) TO PERS1 (1:30)
               MOVE KUNDEMX-IO-AREA (75:30) TO PERS2 (1:30)
               MOVE KUNDEMX-IO-AREA (105:3) TO SNR (1:3)
               MOVE KUNDEMX-IO-AREA (161:6) TO FKNR (1:6)
               MOVE KUNDEMX-IO-AREA (180:9) TO ORGN1 (1:9)
               MOVE KUNDEMX-IO-AREA (11:30) TO LEVAD1 (1:30)
               MOVE KUNDEMX-IO-AREA (41:30) TO LEVAD2 (1:30)
               MOVE KUNDEMX-IO-AREA (71:30) TO LEVAD3 (1:30)
               MOVE KUNDEMX-IO-AREA (101:4) TO LEVPNR (1:4)
               MOVE KUNDEMX-IO-AREA (106:15) TO LPSTED (1:15)
               MOVE KUNDEMX-IO-AREA (121:15) TO FORSMA (1:15)
               MOVE KUNDEMX-IO-AREA (152:16) TO FAX (1:16)
               MOVE KUNDEMX-IO-AREA (11:40) TO EMAIL6 (1:40)
               MOVE KUNDEMX-IO-AREA (95:40) TO MAILA (1:40)
               MOVE KUNDEMX-IO-AREA (95:40) TO MAILB (1:40)
               MOVE KUNDEMX-IO-AREA (95:40) TO MAILC (1:40)
               MOVE KUNDEMX-IO-AREA (95:40) TO MAILD (1:40)
               MOVE KUNDEMX-IO-AREA (95:40) TO MAILE (1:40)
           END-EVALUATE.
 
       KUNDEMX-IDSET SECTION.
       KUNDEMX-IDSET-P.
           SET I-06                        TO TRUE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (136:3) TO RNTPRO-IO
               INSPECT RNTPRO-IO REPLACING ALL ' ' BY '0'
               MOVE FIRMAF-IO-AREA (154:3) TO RNTPRA-IO
               INSPECT RNTPRA-IO REPLACING ALL ' ' BY '0'
               MOVE FIRMAF-IO-AREA (779:1) TO FFKODE (1:1)
               MOVE FIRMAF-IO-AREA (703:1) TO PRIFNR (1:1)
               MOVE FIRMAF-IO-AREA (775:1) TO KRFFD (1:1)
               MOVE FIRMAF-IO-AREA (837:4) TO KTOAVR (1:4)
               MOVE FIRMAF-IO-AREA (896:5) TO FTAKNR-IO
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-08                        TO TRUE.
 
       FAKPAR-FLDSET SECTION.
       FAKPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKPAR-IO-AREA (18:6)  TO PFADTO (1:6)
               MOVE FAKPAR-IO-AREA (18:2)  TO PFDAG (1:2)
               MOVE FAKPAR-IO-AREA (20:2)  TO PFMND (1:2)
               MOVE FAKPAR-IO-AREA (22:2)  TO PFAAR (1:2)
               MOVE FAKPAR-IO-AREA (24:6)  TO PFF15 (1:6)
               MOVE FAKPAR-IO-AREA (30:6)  TO PFF30 (1:6)
               MOVE FAKPAR-IO-AREA (36:6)  TO PFF45 (1:6)
               MOVE FAKPAR-IO-AREA (42:6)  TO PFF60 (1:6)
               MOVE FAKPAR-IO-AREA (48:6)  TO PFF90 (1:6)
               MOVE FAKPAR-IO-AREA (54:6)  TO PFF75 (1:6)
               MOVE FAKPAR-IO-AREA (66:1)  TO FO-ELGRSTE (1:1)
               MOVE FAKPAR-IO-AREA (10:2)  TO FAKTOM (1:2)
               MOVE FAKPAR-IO-AREA (101:6) TO UFADTO (1:6)
               MOVE FAKPAR-IO-AREA (101:2) TO UFDAG (1:2)
               MOVE FAKPAR-IO-AREA (103:2) TO UFMND (1:2)
               MOVE FAKPAR-IO-AREA (105:2) TO UFAAR (1:2)
               MOVE FAKPAR-IO-AREA (107:6) TO UFF15 (1:6)
               MOVE FAKPAR-IO-AREA (113:6) TO UFF30 (1:6)
               MOVE FAKPAR-IO-AREA (119:6) TO UFF45 (1:6)
               MOVE FAKPAR-IO-AREA (125:6) TO UFF60 (1:6)
               MOVE FAKPAR-IO-AREA (131:6) TO UFF90 (1:6)
               MOVE FAKPAR-IO-AREA (137:6) TO UFF75 (1:6)
               MOVE FAKPAR-IO-AREA (143:6) TO UFF105 (1:6)
               MOVE FAKPAR-IO-AREA (149:6) TO UFF120 (1:6)
               MOVE FAKPAR-IO-AREA (155:6) TO UFF150 (1:6)
               MOVE FAKPAR-IO-AREA (161:6) TO UFF180 (1:6)
               MOVE FAKPAR-IO-AREA (167:6) TO PFF105 (1:6)
               MOVE FAKPAR-IO-AREA (173:6) TO PFF120 (1:6)
               MOVE FAKPAR-IO-AREA (179:6) TO PFF150 (1:6)
               MOVE FAKPAR-IO-AREA (185:6) TO PFF180 (1:6)
           END-EVALUATE.
 
       FAKPAR-IDSET SECTION.
       FAKPAR-IDSET-P.
           SET I-07                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               MOVE SPACES TO OUTFIL-IO-AREA
               INITIALIZE OUTFIL-IO-AREA
               MOVE '7'                    TO OUTFIL-IO-AREA (1:1)
               MOVE '    0'                TO OUTFIL-IO-AREA (2:5)
      *                        BUNKNRX    6
               MOVE FAKAA                  TO OUTFIL-IO-AREA (7:4)
               MOVE ' 1'                   TO OUTFIL-IO-AREA (11:2)
               MOVE UMM-IO                 TO OUTFIL-IO-AREA (11:2)
               IF  (I-U1)
                   MOVE PERIOD-IO          TO OUTFIL-IO-AREA (11:2)
               END-IF
               MOVE '         '            TO OUTFIL-IO-AREA (13:9)
      *                        BILNR     21
               MOVE FAKTNR-IO              TO OUTFIL-IO-AREA (16:6)
               MOVE '         '            TO OUTFIL-IO-AREA (22:9)
      *                        POSTNR    30
               MOVE '20180908'             TO OUTFIL-IO-AREA (31:8)
      *                        UYEAR X   34
      *                        UMONTHX   36
      *                        UDAY  X   38
               MOVE PFAAR                  TO OUTFIL-IO-AREA (33:2)
               MOVE PFMND                  TO OUTFIL-IO-AREA (35:2)
               MOVE PFDAG                  TO OUTFIL-IO-AREA (37:2)
               IF  (I-53)
                   MOVE ORAAR-IO           TO OUTFIL-IO-AREA (31:4)
               END-IF
               IF  (I-53)
                   MOVE ORMND-IO           TO OUTFIL-IO-AREA (35:2)
               END-IF
               IF  (I-53)
                   MOVE ORDAG-IO           TO OUTFIL-IO-AREA (37:2)
      *                        DATO      38
               END-IF
               MOVE '20      '             TO OUTFIL-IO-AREA (39:8)
               MOVE FYY-IO                 TO OUTFIL-IO-AREA (41:2)
               MOVE FMM-IO                 TO OUTFIL-IO-AREA (43:2)
               MOVE FDD-IO                 TO OUTFIL-IO-AREA (45:2)
      *                        FDATO     46
               MOVE ' 3'                   TO OUTFIL-IO-AREA (47:2)
               IF  (I-12)
                   MOVE ' 4'               TO OUTFIL-IO-AREA (47:2)
      *                        TK        48
               END-IF
               MOVE '3'                    TO OUTFIL-IO-AREA (49:1)
               IF  (I-21)
                   MOVE '5'                TO OUTFIL-IO-AREA (49:1)
      *                        MOMS      49
               END-IF
               MOVE 'H'                    TO OUTFIL-IO-AREA (50:1)
      *                        KTOKOD    50
               MOVE '      '               TO OUTFIL-IO-AREA (51:6)
               IF  (NOT-I-10)
                   MOVE KONTO              TO OUTFIL-IO-AREA (53:4)
               END-IF
               IF  (I-10)
                   MOVE KONTO              TO OUTFIL-IO-AREA (53:4)
               END-IF
               IF  (I-11)
                   MOVE '8055'             TO OUTFIL-IO-AREA (53:4)
               END-IF
               IF  (NOT-I-10 AND I-27)
                   MOVE KONTO              TO OUTFIL-IO-AREA (53:4)
               END-IF
               IF  (I-25)
                   MOVE '3100'             TO OUTFIL-IO-AREA (53:4)
               END-IF
               IF  (I-21)
                   MOVE '3130'             TO OUTFIL-IO-AREA (53:4)
               END-IF
               IF  (I-18)
                   MOVE '3000'             TO OUTFIL-IO-AREA (53:4)
      *                        KTONR     56
               END-IF
               MOVE '   '                  TO OUTFIL-IO-AREA (57:3)
      *                        DIMA      59
               MOVE '      '               TO OUTFIL-IO-AREA (60:6)
      *                        DIMB      65
               MOVE '      '               TO OUTFIL-IO-AREA (66:6)
      *                        DIMC      71
               MOVE '      '               TO OUTFIL-IO-AREA (72:6)
      *                        DIMD      77
               MOVE '              '       TO OUTFIL-IO-AREA (78:14)
      *                        ID        91
               MOVE '  '                   TO OUTFIL-IO-AREA (92:2)
      *                        REVOMR    93
               MOVE ' '                    TO OUTFIL-IO-AREA (94:1)
      *                        MOTKOD    94
               MOVE '     0'               TO OUTFIL-IO-AREA (95:6)
      *                        MOTKTO   100
               MOVE '   -      0.00'       TO OUTFIL-IO-AREA (101:14)
               IF  (I-51)
                   MOVE '          0.00'   TO OUTFIL-IO-AREA (101:14)
      *                        NOKBEL   114
      *                        NTOKR X  111
      *                        NOREA    114
               END-IF
               MOVE NTO01F                 TO OUTFIL-IO-AREA (105:7)
               MOVE NTO01H                 TO OUTFIL-IO-AREA (113:2)
               MOVE 'NOK  '                TO OUTFIL-IO-AREA (115:5)
      *                        VALUTA   119
               MOVE ' 100.000'             TO OUTFIL-IO-AREA (120:8)
      *                        VKURS    127
               MOVE '   -      0.00'       TO OUTFIL-IO-AREA (128:14)
               IF  (I-51)
                   MOVE '          0.00'   TO OUTFIL-IO-AREA (128:14)
      *                        BELOP    141
      *                        NTOKR X  138
      *                        NTOOREX  141
      *                        NOREA    141
               END-IF
               MOVE NTO01F                 TO OUTFIL-IO-AREA (132:7)
               MOVE NTO01H                 TO OUTFIL-IO-AREA (140:2)
               MOVE '            '         TO OUTFIL-IO-AREA (142:12)
      *                        TEKST    153
               MOVE '     0'               TO OUTFIL-IO-AREA (154:6)
      *                        HPROSJ   159
               MOVE '     0'               TO OUTFIL-IO-AREA (160:6)
      *                        PROSNR   165
               MOVE '        0'            TO OUTFIL-IO-AREA (166:9)
      *                        ARTNR    174
               MOVE '     0.00'            TO OUTFIL-IO-AREA (175:9)
      *                        ANTALL   183
               MOVE '     0'               TO OUTFIL-IO-AREA (184:6)
      *                        ANSNR    189
               MOVE '  0'                  TO OUTFIL-IO-AREA (190:3)
      *                        LTART    192
               MOVE '   0.00'              TO OUTFIL-IO-AREA (193:7)
      *                        TIMER    199
               MOVE ' 0'                   TO OUTFIL-IO-AREA (200:2)
      *                        LPER     201
               MOVE '              '       TO OUTFIL-IO-AREA (202:14)
      *                        REFNR    215
               MOVE '               '      TO OUTFIL-IO-AREA (216:15)
               MOVE '                 '    TO OUTFIL-IO-AREA (231:17)
      *                        KIDNR    247
               MOVE '      '               TO OUTFIL-IO-AREA (248:6)
      *                        SAKID    253
               MOVE '3'                    TO OUTFIL-IO-AREA (254:1)
               IF  (I-21)
                   MOVE '5'                TO OUTFIL-IO-AREA (254:1)
      *                        SMOMS    254
               END-IF
               MOVE '     0'               TO OUTFIL-IO-AREA (255:6)
      *                        SHKTO    260
               MOVE '  0'                  TO OUTFIL-IO-AREA (261:3)
      *                        SDIMA    263
               MOVE '     0'               TO OUTFIL-IO-AREA (264:6)
      *                        SDIMB    269
               MOVE '     0'               TO OUTFIL-IO-AREA (270:6)
      *                        SDIMC    275
               MOVE '     0'               TO OUTFIL-IO-AREA (276:6)
      *                        SDIMD    281
               MOVE '              '       TO OUTFIL-IO-AREA (282:14)
      *                        SID      295
               MOVE '     0'               TO OUTFIL-IO-AREA (296:6)
      *                        SHPROS   301
               MOVE '     0'               TO OUTFIL-IO-AREA (302:6)
      *                        SPROS    307
               MOVE '        0'            TO OUTFIL-IO-AREA (308:9)
      *                        SARTNR   316
               MOVE '                    ' TO OUTFIL-IO-AREA (317:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (337:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (357:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (377:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (397:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (417:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (437:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (457:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (477:20)
               MOVE '          '           TO OUTFIL-IO-AREA (497:10)
      *                        EYE      506
               MOVE '                  '   TO OUTFIL-IO-AREA (507:18)
      *                        IDNR     524
               MOVE '0'                    TO OUTFIL-IO-AREA (525:1)
      *                        TYPE     525
               MOVE ' '                    TO OUTFIL-IO-AREA (526:1)
      *                        VIDFAK   526
               MOVE '0'                    TO OUTFIL-IO-AREA (527:1)
      *                        FAKTYP   527
               MOVE '                  '   TO OUTFIL-IO-AREA (528:18)
      *                        ARTIKK   545
               MOVE '                    ' TO OUTFIL-IO-AREA (546:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (566:20)
               MOVE '          '           TO OUTFIL-IO-AREA (586:10)
      *                        DIV1     595
               MOVE '                    ' TO OUTFIL-IO-AREA (596:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (616:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (636:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (656:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (676:20)
      *                        DIV2     695
               MOVE '      0'              TO OUTFIL-IO-AREA (696:7)
      *                        DIV3     702
               MOVE '     0'               TO OUTFIL-IO-AREA (703:6)
      *                        KND      708
               MOVE '                    ' TO OUTFIL-IO-AREA (709:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (729:20)
               MOVE '          '           TO OUTFIL-IO-AREA (749:10)
      *                        TEKST2   758
      *                                 761 "  3"
               MOVE '  3'                  TO OUTFIL-IO-AREA (759:3)
               IF  (I-21)
                   MOVE '  5'              TO OUTFIL-IO-AREA (759:3)
      *                        MOMS2    761
               END-IF
               MOVE '  0'                  TO OUTFIL-IO-AREA (762:3)
      *                        SMOMS2   764
               MOVE '                    ' TO OUTFIL-IO-AREA (765:20)
      *                        ID2      784
               MOVE '                    ' TO OUTFIL-IO-AREA (785:20)
      *                        SID2     804
               MOVE '        0'            TO OUTFIL-IO-AREA (805:9)
      *                        HPROS2   813
               MOVE '        0'            TO OUTFIL-IO-AREA (814:9)
      *                        PROSN2   822
               MOVE '        0'            TO OUTFIL-IO-AREA (823:9)
      *                        SHPRO2   831
               MOVE '        0'            TO OUTFIL-IO-AREA (832:9)
      *                        SPROS2   840
               MOVE '        0'            TO OUTFIL-IO-AREA (841:9)
               IF  (NOT-I-10)
                   MOVE KONTO              TO OUTFIL-IO-AREA (846:4)
               END-IF
               IF  (I-10)
                   MOVE KONTO              TO OUTFIL-IO-AREA (846:4)
               END-IF
               IF  (I-11)
                   MOVE '8055'             TO OUTFIL-IO-AREA (846:4)
               END-IF
               IF  (NOT-I-10 AND I-27)
                   MOVE KONTO              TO OUTFIL-IO-AREA (846:4)
               END-IF
               IF  (I-25)
                   MOVE '3100'             TO OUTFIL-IO-AREA (846:4)
               END-IF
               IF  (I-21)
                   MOVE '3130'             TO OUTFIL-IO-AREA (846:4)
               END-IF
               IF  (I-18)
                   MOVE '3000'             TO OUTFIL-IO-AREA (846:4)
      *                        KTONR2   849
               END-IF
               MOVE '        0'            TO OUTFIL-IO-AREA (850:9)
      *                        DIMA2    858
               MOVE '        0'            TO OUTFIL-IO-AREA (859:9)
      *                        DIMB2    867
               MOVE '        0'            TO OUTFIL-IO-AREA (868:9)
      *                        DIMC2    876
               MOVE '        0'            TO OUTFIL-IO-AREA (877:9)
      *                        DIMD2    885
               MOVE '        0'            TO OUTFIL-IO-AREA (886:9)
      *                        MOTKT2   894
               MOVE '        0'            TO OUTFIL-IO-AREA (895:9)
      *                        SHKTO2   903
               MOVE '        0'            TO OUTFIL-IO-AREA (904:9)
      *                        SDIMA2   912
               MOVE '        0'            TO OUTFIL-IO-AREA (913:9)
      *                        SDIMB2   921
               MOVE '        0'            TO OUTFIL-IO-AREA (922:9)
      *                        SDIMC2   930
               MOVE '        0'            TO OUTFIL-IO-AREA (931:9)
      *                        SDIMD2   939
               MOVE '          0.000'      TO OUTFIL-IO-AREA (940:15)
      *                        MENGDE   954
               MOVE '                    ' TO OUTFIL-IO-AREA (955:20)
      *                        REFNR2   974
               MOVE '                    ' TO OUTFIL-IO-AREA (967:20)
      *                        SAKID2   986
               MOVE '   0.0000'            TO OUTFIL-IO-AREA (987:9)
      *                        VKURS2   995
               MOVE '                    ' TO OUTFIL-IO-AREA (996:20)
      *                        HPROS3  1015
               MOVE '                    ' TO OUTFIL-IO-AREA (1016:20)
      *                        PROSN3  1035
               MOVE '                    ' TO OUTFIL-IO-AREA (1036:20)
      *                        SHPRO3  1055
               MOVE '                    ' TO OUTFIL-IO-AREA (1056:20)
      *                        SPROS3  1075
               MOVE '        0'            TO OUTFIL-IO-AREA (1076:9)
      *                        KSTMVA  1084
               WRITE OUTFIL-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L5)
               MOVE SPACES TO OUTFIL-IO-AREA
               INITIALIZE OUTFIL-IO-AREA
               MOVE '9'                    TO OUTFIL-IO-AREA (1:1)
               MOVE '    0'                TO OUTFIL-IO-AREA (2:5)
      *                        BUNKNRX    6
               MOVE '20  '                 TO OUTFIL-IO-AREA (7:4)
               MOVE UYEAR                  TO OUTFIL-IO-AREA (9:2)
               MOVE ' 1'                   TO OUTFIL-IO-AREA (11:2)
               MOVE UMM-IO                 TO OUTFIL-IO-AREA (11:2)
               IF  (I-U1)
                   MOVE PERIOD-IO          TO OUTFIL-IO-AREA (11:2)
               END-IF
               MOVE 'FAKTURA     '         TO OUTFIL-IO-AREA (13:12)
               MOVE FIRMA                  TO OUTFIL-IO-AREA (22:3)
               WRITE OUTFIL-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L3 AND NOT-I-48)
      *                                  10 "ØRE DIFF  "
      *                        OREDIFX   26
               MOVE SPACES TO OUTFIL-IO-AREA
               INITIALIZE OUTFIL-IO-AREA
               MOVE '7'                    TO OUTFIL-IO-AREA (1:1)
               MOVE '    0'                TO OUTFIL-IO-AREA (2:5)
      *                        BUNKNRX    6
               MOVE FAKAA                  TO OUTFIL-IO-AREA (7:4)
               MOVE ' 1'                   TO OUTFIL-IO-AREA (11:2)
               MOVE UMM-IO                 TO OUTFIL-IO-AREA (11:2)
               IF  (I-U1)
                   MOVE PERIOD-IO          TO OUTFIL-IO-AREA (11:2)
               END-IF
               MOVE '         '            TO OUTFIL-IO-AREA (13:9)
      *                        BILNR     21
               MOVE FAKTNR-IO              TO OUTFIL-IO-AREA (16:6)
               MOVE '         '            TO OUTFIL-IO-AREA (22:9)
      *                        POSTNR    30
               MOVE '20180908'             TO OUTFIL-IO-AREA (31:8)
               MOVE PFAAR                  TO OUTFIL-IO-AREA (33:2)
               MOVE PFMND                  TO OUTFIL-IO-AREA (35:2)
               MOVE PFDAG                  TO OUTFIL-IO-AREA (37:2)
               IF  (I-53)
                   MOVE ORAAR-IO           TO OUTFIL-IO-AREA (31:4)
               END-IF
               IF  (I-53)
                   MOVE ORMND-IO           TO OUTFIL-IO-AREA (35:2)
               END-IF
               IF  (I-53)
                   MOVE ORDAG-IO           TO OUTFIL-IO-AREA (37:2)
      *                        DATO      38
               END-IF
               MOVE '20      '             TO OUTFIL-IO-AREA (39:8)
               MOVE FYY-IO                 TO OUTFIL-IO-AREA (41:2)
               MOVE FMM-IO                 TO OUTFIL-IO-AREA (43:2)
               MOVE FDD-IO                 TO OUTFIL-IO-AREA (45:2)
      *                        FDATO     46
               MOVE ' 3'                   TO OUTFIL-IO-AREA (47:2)
               IF  (I-12)
                   MOVE ' 4'               TO OUTFIL-IO-AREA (47:2)
      *                        TK        48
               END-IF
               MOVE '0'                    TO OUTFIL-IO-AREA (49:1)
      *                        MOMS      49
               MOVE 'H'                    TO OUTFIL-IO-AREA (50:1)
      *                        KTOKOD    50
               MOVE '      '               TO OUTFIL-IO-AREA (51:6)
               MOVE '7740'                 TO OUTFIL-IO-AREA (53:4)
      *                        KTONR     56
               MOVE '   '                  TO OUTFIL-IO-AREA (57:3)
      *                        DIMA      59
               MOVE '      '               TO OUTFIL-IO-AREA (60:6)
      *                        DIMB      65
               MOVE '      '               TO OUTFIL-IO-AREA (66:6)
      *                        DIMC      71
               MOVE '      '               TO OUTFIL-IO-AREA (72:6)
      *                        DIMD      77
               MOVE '              '       TO OUTFIL-IO-AREA (78:14)
      *                        ID        91
               MOVE '  '                   TO OUTFIL-IO-AREA (92:2)
      *                        REVOMR    93
               MOVE ' '                    TO OUTFIL-IO-AREA (94:1)
      *                        MOTKOD    94
               MOVE '     0'               TO OUTFIL-IO-AREA (95:6)
      *                        MOTKTO   100
               MOVE '   -      0.00'       TO OUTFIL-IO-AREA (101:14)
               IF  (I-51)
                   MOVE '          0.00'   TO OUTFIL-IO-AREA (101:14)
               END-IF
               IF  (I-47)
                   MOVE ' '                TO OUTFIL-IO-AREA (104:1)
               END-IF
               IF  (I-51 AND I-47)
                   MOVE '-'                TO OUTFIL-IO-AREA (104:1)
               END-IF
               IF  (I-54 AND I-47)
                   MOVE ' '                TO OUTFIL-IO-AREA (104:1)
               END-IF
               IF  (I-54 AND NOT-I-47)
                   MOVE '-'                TO OUTFIL-IO-AREA (104:1)
               END-IF
               IF  (I-53 AND I-47)
                   MOVE ' '                TO OUTFIL-IO-AREA (104:1)
               END-IF
               IF  (I-53 AND I-51 AND I-47)
                   MOVE ' '                TO OUTFIL-IO-AREA (104:1)
               END-IF
               IF  (I-53 AND I-51 AND NOT-I-47)
                   MOVE '-'                TO OUTFIL-IO-AREA (104:1)
      *                53 54 47         104 "3"
      *                53 54N47         104 "4"
      *                        NOKBEL   114
               END-IF
               MOVE OREDI2                 TO OUTFIL-IO-AREA (105:7)
               MOVE OREDI4                 TO OUTFIL-IO-AREA (113:2)
               MOVE 'NOK  '                TO OUTFIL-IO-AREA (115:5)
      *                        VALUTA   119
               MOVE ' 100.000'             TO OUTFIL-IO-AREA (120:8)
      *                        VKURS    127
               MOVE '   -      0.00'       TO OUTFIL-IO-AREA (128:14)
               IF  (I-51)
                   MOVE '          0.00'   TO OUTFIL-IO-AREA (128:14)
               END-IF
               IF  (I-47)
                   MOVE ' '                TO OUTFIL-IO-AREA (131:1)
               END-IF
               IF  (I-51 AND I-47)
                   MOVE '-'                TO OUTFIL-IO-AREA (131:1)
               END-IF
               IF  (I-54 AND I-47)
                   MOVE ' '                TO OUTFIL-IO-AREA (131:1)
               END-IF
               IF  (I-54 AND NOT-I-47)
                   MOVE '-'                TO OUTFIL-IO-AREA (131:1)
      *                        BELOP    141
               END-IF
               MOVE OREDI2                 TO OUTFIL-IO-AREA (132:7)
               MOVE OREDI4                 TO OUTFIL-IO-AREA (140:2)
               MOVE '            '         TO OUTFIL-IO-AREA (142:12)
      *                        TEKST    153
               MOVE '     0'               TO OUTFIL-IO-AREA (154:6)
      *                        HPROSJ   159
               MOVE '     0'               TO OUTFIL-IO-AREA (160:6)
      *                        PROSNR   165
               MOVE '        0'            TO OUTFIL-IO-AREA (166:9)
      *                        ARTNR    174
               MOVE '     0.00'            TO OUTFIL-IO-AREA (175:9)
      *                        ANTALL   183
               MOVE '     0'               TO OUTFIL-IO-AREA (184:6)
      *                        ANSNR    189
               MOVE '  0'                  TO OUTFIL-IO-AREA (190:3)
      *                        LTART    192
               MOVE '   0.00'              TO OUTFIL-IO-AREA (193:7)
      *                        TIMER    199
               MOVE ' 0'                   TO OUTFIL-IO-AREA (200:2)
      *                        LPER     201
               MOVE '              '       TO OUTFIL-IO-AREA (202:14)
      *                        REFNR    215
               MOVE '               '      TO OUTFIL-IO-AREA (216:15)
               MOVE '                 '    TO OUTFIL-IO-AREA (231:17)
      *                        KIDNR    247
               MOVE '      '               TO OUTFIL-IO-AREA (248:6)
      *                        SAKID    253
               MOVE '0'                    TO OUTFIL-IO-AREA (254:1)
      *                        SMOMS    254
               MOVE '     0'               TO OUTFIL-IO-AREA (255:6)
      *                        SHKTO    260
               MOVE '  0'                  TO OUTFIL-IO-AREA (261:3)
      *                        SDIMA    263
               MOVE '     0'               TO OUTFIL-IO-AREA (264:6)
      *                        SDIMB    269
               MOVE '     0'               TO OUTFIL-IO-AREA (270:6)
      *                        SDIMC    275
               MOVE '     0'               TO OUTFIL-IO-AREA (276:6)
      *                        SDIMD    281
               MOVE '              '       TO OUTFIL-IO-AREA (282:14)
      *                        SID      295
               MOVE '     0'               TO OUTFIL-IO-AREA (296:6)
      *                        SHPROS   301
               MOVE '     0'               TO OUTFIL-IO-AREA (302:6)
      *                        SPROS    307
               MOVE '        0'            TO OUTFIL-IO-AREA (308:9)
      *                        SARTNR   316
               MOVE '                    ' TO OUTFIL-IO-AREA (317:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (337:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (357:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (377:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (397:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (417:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (437:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (457:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (477:20)
               MOVE '          '           TO OUTFIL-IO-AREA (497:10)
      *                        EYE      506
               MOVE '                  '   TO OUTFIL-IO-AREA (507:18)
      *                        IDNR     524
               MOVE '0'                    TO OUTFIL-IO-AREA (525:1)
      *                        TYPE     525
               MOVE ' '                    TO OUTFIL-IO-AREA (526:1)
      *                        VIDFAK   526
               MOVE '0'                    TO OUTFIL-IO-AREA (527:1)
      *                        FAKTYP   527
               MOVE '                  '   TO OUTFIL-IO-AREA (528:18)
      *                        ARTIKK   545
               MOVE '                    ' TO OUTFIL-IO-AREA (546:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (566:20)
               MOVE '          '           TO OUTFIL-IO-AREA (586:10)
      *                        DIV1     595
               MOVE '                    ' TO OUTFIL-IO-AREA (596:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (616:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (636:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (656:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (676:20)
      *                        DIV2     695
               MOVE '      0'              TO OUTFIL-IO-AREA (696:7)
      *                        DIV3     702
               MOVE '     0'               TO OUTFIL-IO-AREA (703:6)
      *                        KND      708
               MOVE '                    ' TO OUTFIL-IO-AREA (709:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (729:20)
               MOVE '          '           TO OUTFIL-IO-AREA (749:10)
      *                        TEKST2   758
      *                                 761 "  3"
               MOVE '  0'                  TO OUTFIL-IO-AREA (759:3)
      *                        MOMS2    761
               MOVE '  0'                  TO OUTFIL-IO-AREA (762:3)
      *                        SMOMS2   764
               MOVE '                    ' TO OUTFIL-IO-AREA (765:20)
      *                        ID2      784
               MOVE '                    ' TO OUTFIL-IO-AREA (785:20)
      *                        SID2     804
               MOVE '        0'            TO OUTFIL-IO-AREA (805:9)
      *                        HPROS2   813
               MOVE '        0'            TO OUTFIL-IO-AREA (814:9)
      *                        PROSN2   822
               MOVE '        0'            TO OUTFIL-IO-AREA (823:9)
      *                        SHPRO2   831
               MOVE '        0'            TO OUTFIL-IO-AREA (832:9)
      *                        SPROS2   840
               MOVE '        0'            TO OUTFIL-IO-AREA (841:9)
               MOVE '7740'                 TO OUTFIL-IO-AREA (846:4)
      *                        KTONR2   849
               MOVE '        0'            TO OUTFIL-IO-AREA (850:9)
      *                        DIMA2    858
               MOVE '        0'            TO OUTFIL-IO-AREA (859:9)
      *                        DIMB2    867
               MOVE '        0'            TO OUTFIL-IO-AREA (868:9)
      *                        DIMC2    876
               MOVE '        0'            TO OUTFIL-IO-AREA (877:9)
      *                        DIMD2    885
               MOVE '        0'            TO OUTFIL-IO-AREA (886:9)
      *                        MOTKT2   894
               MOVE '        0'            TO OUTFIL-IO-AREA (895:9)
      *                        SHKTO2   903
               MOVE '        0'            TO OUTFIL-IO-AREA (904:9)
      *                        SDIMA2   912
               MOVE '        0'            TO OUTFIL-IO-AREA (913:9)
      *                        SDIMB2   921
               MOVE '        0'            TO OUTFIL-IO-AREA (922:9)
      *                        SDIMC2   930
               MOVE '        0'            TO OUTFIL-IO-AREA (931:9)
      *                        SDIMD2   939
               MOVE '          0.000'      TO OUTFIL-IO-AREA (940:15)
      *                        MENGDE   954
               MOVE '                    ' TO OUTFIL-IO-AREA (955:20)
      *                        REFNR2   974
               MOVE '                    ' TO OUTFIL-IO-AREA (967:20)
      *                        SAKID2   986
               MOVE '   0.0000'            TO OUTFIL-IO-AREA (987:9)
      *                        VKURS2   995
               MOVE '                    ' TO OUTFIL-IO-AREA (996:20)
      *                        HPROS3  1015
               MOVE '                    ' TO OUTFIL-IO-AREA (1016:20)
      *                        PROSN3  1035
               MOVE '                    ' TO OUTFIL-IO-AREA (1036:20)
      *                        SHPRO3  1055
               MOVE '                    ' TO OUTFIL-IO-AREA (1056:20)
      *                        SPROS3  1075
               MOVE '        0'            TO OUTFIL-IO-AREA (1076:9)
      *                        KSTMVA  1084
      *UTFIL  D        01
               WRITE OUTFIL-IO-AREA
           END-IF
           IF  (I-L3)
               MOVE SPACES TO OUTFIL-IO-AREA
               INITIALIZE OUTFIL-IO-AREA
               MOVE '7'                    TO OUTFIL-IO-AREA (1:1)
               MOVE '    0'                TO OUTFIL-IO-AREA (2:5)
      *                        BUNKNRX    6
               MOVE FAKAA                  TO OUTFIL-IO-AREA (7:4)
               MOVE ' 1'                   TO OUTFIL-IO-AREA (11:2)
               MOVE UMM-IO                 TO OUTFIL-IO-AREA (11:2)
               IF  (I-U1)
                   MOVE PERIOD-IO          TO OUTFIL-IO-AREA (11:2)
               END-IF
               MOVE '         '            TO OUTFIL-IO-AREA (13:9)
      *                        BILNR     21
               MOVE FAKTNR-IO              TO OUTFIL-IO-AREA (16:6)
               MOVE '         '            TO OUTFIL-IO-AREA (22:9)
               MOVE FAKTNR-IO              TO OUTFIL-IO-AREA (25:6)
      *                        POSTNR    30
               MOVE '20180908'             TO OUTFIL-IO-AREA (31:8)
      *                        UYEAR X   34
      *                        UMONTHX   36
      *                        UDAY  X   38
               MOVE PFAAR                  TO OUTFIL-IO-AREA (33:2)
               MOVE PFMND                  TO OUTFIL-IO-AREA (35:2)
               MOVE PFDAG                  TO OUTFIL-IO-AREA (37:2)
               IF  (I-53)
                   MOVE ORAAR-IO           TO OUTFIL-IO-AREA (31:4)
               END-IF
               IF  (I-53)
                   MOVE ORMND-IO           TO OUTFIL-IO-AREA (35:2)
               END-IF
               IF  (I-53)
                   MOVE ORDAG-IO           TO OUTFIL-IO-AREA (37:2)
      *                        DATO      38
               END-IF
               MOVE '20      '             TO OUTFIL-IO-AREA (39:8)
               MOVE FYY-IO                 TO OUTFIL-IO-AREA (41:2)
               MOVE FMM-IO                 TO OUTFIL-IO-AREA (43:2)
               MOVE FDD-IO                 TO OUTFIL-IO-AREA (45:2)
      *                        FDATO     46
               MOVE ' 3'                   TO OUTFIL-IO-AREA (47:2)
               IF  (I-12)
                   MOVE ' 4'               TO OUTFIL-IO-AREA (47:2)
      *                        TK        48
               END-IF
               MOVE '3'                    TO OUTFIL-IO-AREA (49:1)
               IF  (I-21)
                   MOVE '5'                TO OUTFIL-IO-AREA (49:1)
      *                        MOMS      49
               END-IF
               MOVE 'R'                    TO OUTFIL-IO-AREA (50:1)
      *                        KTOKOD    50
               MOVE '      '               TO OUTFIL-IO-AREA (51:6)
               MOVE KUNDNR                 TO OUTFIL-IO-AREA (51:6)
      *                        KTONR     56
               MOVE '  0'                  TO OUTFIL-IO-AREA (57:3)
      *                        DIMA      59
               MOVE '     0'               TO OUTFIL-IO-AREA (60:6)
      *                        DIMB      65
               MOVE '     0'               TO OUTFIL-IO-AREA (66:6)
      *                        DIMC      71
               MOVE '     0'               TO OUTFIL-IO-AREA (72:6)
      *                        DIMD      77
               MOVE '              '       TO OUTFIL-IO-AREA (78:14)
      *                        ID        91
               MOVE '  '                   TO OUTFIL-IO-AREA (92:2)
      *                        REVOMR    93
               MOVE ' '                    TO OUTFIL-IO-AREA (94:1)
      *                        MOTKOD    94
               MOVE '     0'               TO OUTFIL-IO-AREA (95:6)
      *                        MOTKTO   100
               MOVE '          0.00'       TO OUTFIL-IO-AREA (101:14)
               IF  (I-54)
                   MOVE '-'                TO OUTFIL-IO-AREA (104:1)
      *                        NOKBEL   114
      *                        NTOKR X  111
      *                        NTOOREX  114
      *                        SUML3C   111
      *                        SUML3A   114
               END-IF
               MOVE SUML3F                 TO OUTFIL-IO-AREA (105:7)
               MOVE SUML3H                 TO OUTFIL-IO-AREA (113:2)
               MOVE 'NOK  '                TO OUTFIL-IO-AREA (115:5)
      *                        VALUTA   119
               MOVE ' 100.000'             TO OUTFIL-IO-AREA (120:8)
      *                        VKURS    127
      *                        BELOP    141
      *                        NTOKR X  138
      *                        NTOOREX  141
               MOVE '          0.00'       TO OUTFIL-IO-AREA (128:14)
               IF  (I-54)
                   MOVE '-'                TO OUTFIL-IO-AREA (131:1)
      *                        SUML3C   138
      *                        SUML3A   141
               END-IF
               MOVE SUML3F                 TO OUTFIL-IO-AREA (132:7)
               MOVE SUML3H                 TO OUTFIL-IO-AREA (140:2)
      *                        SUML3MJ  141
               MOVE '            '         TO OUTFIL-IO-AREA (142:12)
      *                        TEKST    153
               MOVE '     0'               TO OUTFIL-IO-AREA (154:6)
      *                        HPROSJ   159
               MOVE '     0'               TO OUTFIL-IO-AREA (160:6)
      *                        PROSNR   165
               MOVE '        0'            TO OUTFIL-IO-AREA (166:9)
      *                        ARTNR    174
               MOVE '     0.00'            TO OUTFIL-IO-AREA (175:9)
      *                        ANTALL   183
               MOVE '     0'               TO OUTFIL-IO-AREA (184:6)
      *                        ANSNR    189
               MOVE '  0'                  TO OUTFIL-IO-AREA (190:3)
      *                        LTART    192
               MOVE '   0.00'              TO OUTFIL-IO-AREA (193:7)
      *                        TIMER    199
               MOVE ' 0'                   TO OUTFIL-IO-AREA (200:2)
      *                        LPER     201
               MOVE '              '       TO OUTFIL-IO-AREA (202:14)
      *                        REFNR    215
               MOVE '               '      TO OUTFIL-IO-AREA (216:15)
               MOVE '                 '    TO OUTFIL-IO-AREA (231:17)
      *                        KIDNR    247
               MOVE '      '               TO OUTFIL-IO-AREA (248:6)
      *                        SAKID    253
               MOVE '3'                    TO OUTFIL-IO-AREA (254:1)
               IF  (I-21)
                   MOVE '5'                TO OUTFIL-IO-AREA (254:1)
      *                        SMOMS    254
               END-IF
               MOVE '     0'               TO OUTFIL-IO-AREA (255:6)
      *                        SHKTO    260
               MOVE '  0'                  TO OUTFIL-IO-AREA (261:3)
      *                        SDIMA    263
               MOVE '     0'               TO OUTFIL-IO-AREA (264:6)
      *                        SDIMB    269
               MOVE '     0'               TO OUTFIL-IO-AREA (270:6)
      *                        SDIMC    275
               MOVE '     0'               TO OUTFIL-IO-AREA (276:6)
      *                        SDIMD    281
               MOVE '              '       TO OUTFIL-IO-AREA (282:14)
      *                        SID      295
               MOVE '     0'               TO OUTFIL-IO-AREA (296:6)
      *                        SHPROS   301
               MOVE '     0'               TO OUTFIL-IO-AREA (302:6)
      *                        SPROS    307
               MOVE '        0'            TO OUTFIL-IO-AREA (308:9)
      *                        SARTNR   316
               MOVE '                    ' TO OUTFIL-IO-AREA (317:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (337:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (357:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (377:20)
               IF  (NOT-I-53)
                   MOVE 'http://gw.autodata.n' TO OUTFIL-IO-AREA
                                                              (317:20)
               END-IF
               IF  (NOT-I-53)
                   MOVE 'o/faktura/test_faktu' TO OUTFIL-IO-AREA
                                                              (337:20)
               END-IF
               IF  (NOT-I-53)
                   MOVE 'ra.php?var=@AAA|@BBB' TO OUTFIL-IO-AREA
                                                              (357:20)
               END-IF
               IF  (NOT-I-53)
                   MOVE 'BBB|@CCCCCC         ' TO OUTFIL-IO-AREA
                                                              (377:20)
               END-IF
               IF  (NOT-I-53)
                   MOVE FIRMA              TO OUTFIL-IO-AREA (369:3)
               END-IF
               IF  (NOT-I-53)
                   MOVE FAKTNR-IO          TO OUTFIL-IO-AREA (374:6)
               END-IF
               IF  (NOT-I-53)
                   MOVE KUNDNR             TO OUTFIL-IO-AREA (382:6)
               END-IF
               MOVE '                    ' TO OUTFIL-IO-AREA (397:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (417:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (437:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (457:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (477:20)
               MOVE '          '           TO OUTFIL-IO-AREA (497:10)
      *                        EYE      506
               MOVE '                  '   TO OUTFIL-IO-AREA (507:18)
      *                        IDNR     524
               MOVE '0'                    TO OUTFIL-IO-AREA (525:1)
      *                        TYPE     525
               MOVE ' '                    TO OUTFIL-IO-AREA (526:1)
      *                        VIDFAK   526
               MOVE '0'                    TO OUTFIL-IO-AREA (527:1)
      *                        FAKTYP   527
               MOVE '                  '   TO OUTFIL-IO-AREA (528:18)
      *                        ARTIKK   545
               MOVE '                    ' TO OUTFIL-IO-AREA (546:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (566:20)
               MOVE '          '           TO OUTFIL-IO-AREA (586:10)
      *                        DIV1     595
               MOVE '                    ' TO OUTFIL-IO-AREA (596:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (616:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (636:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (656:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (676:20)
      *                        DIV2     695
               MOVE '      0'              TO OUTFIL-IO-AREA (696:7)
      *                        DIV3     702
               MOVE '     0'               TO OUTFIL-IO-AREA (703:6)
      *                        KND      708
               MOVE '                    ' TO OUTFIL-IO-AREA (709:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (729:20)
               MOVE '          '           TO OUTFIL-IO-AREA (749:10)
      *                        TEKST2   758
               MOVE '  3'                  TO OUTFIL-IO-AREA (759:3)
               IF  (I-21)
                   MOVE '  5'              TO OUTFIL-IO-AREA (759:3)
      *                        MOMS2    761
               END-IF
               MOVE '  0'                  TO OUTFIL-IO-AREA (762:3)
      *                        SMOMS2   764
               MOVE '                    ' TO OUTFIL-IO-AREA (765:20)
      *                        ID2      784
               MOVE '                    ' TO OUTFIL-IO-AREA (785:20)
      *                        SID2     804
               MOVE '        0'            TO OUTFIL-IO-AREA (805:9)
      *                        HPROS2   813
               MOVE '        0'            TO OUTFIL-IO-AREA (814:9)
      *                        PROSN2   822
               MOVE '        0'            TO OUTFIL-IO-AREA (823:9)
      *                        SHPRO2   831
               MOVE '        0'            TO OUTFIL-IO-AREA (832:9)
      *                        SPROS2   840
               MOVE '        0'            TO OUTFIL-IO-AREA (841:9)
               MOVE KUNDNR                 TO OUTFIL-IO-AREA (844:6)
      *                        KTONR2   849
               MOVE '        0'            TO OUTFIL-IO-AREA (850:9)
      *                        DIMA2    858
               MOVE '        0'            TO OUTFIL-IO-AREA (859:9)
      *                        DIMB2    867
               MOVE '        0'            TO OUTFIL-IO-AREA (868:9)
      *                        DIMC2    876
               MOVE '        0'            TO OUTFIL-IO-AREA (877:9)
      *                        DIMD2    885
               MOVE '        0'            TO OUTFIL-IO-AREA (886:9)
      *                        MOTKT2   894
               MOVE '        0'            TO OUTFIL-IO-AREA (895:9)
      *                        SHKTO2   903
               MOVE '        0'            TO OUTFIL-IO-AREA (904:9)
      *                        SDIMA2   912
               MOVE '        0'            TO OUTFIL-IO-AREA (913:9)
      *                        SDIMB2   921
               MOVE '        0'            TO OUTFIL-IO-AREA (922:9)
      *                        SDIMC2   930
               MOVE '        0'            TO OUTFIL-IO-AREA (931:9)
      *                        SDIMD2   939
               MOVE '          0.000'      TO OUTFIL-IO-AREA (940:15)
      *                        MENGDE   954
               MOVE '                    ' TO OUTFIL-IO-AREA (955:20)
      *                        REFNR2   974
               MOVE '                    ' TO OUTFIL-IO-AREA (967:20)
      *                        SAKID2   986
               MOVE '   0.0000'            TO OUTFIL-IO-AREA (987:9)
      *                        VKURS2   995
               MOVE '                    ' TO OUTFIL-IO-AREA (996:20)
      *                        HPROS3  1015
               MOVE '                    ' TO OUTFIL-IO-AREA (1016:20)
      *                        PROSN3  1035
               MOVE '                    ' TO OUTFIL-IO-AREA (1036:20)
      *                        SHPRO3  1055
               MOVE '                    ' TO OUTFIL-IO-AREA (1056:20)
      *                        SPROS3  1075
               MOVE '        0'            TO OUTFIL-IO-AREA (1076:9)
      *                        KSTMVA  1084
               WRITE OUTFIL-IO-AREA
           END-IF
           IF  (I-L3 AND I-35)
           OR  (I-L3 AND NOT-I-35 AND I-36)
               MOVE SPACES TO OUTFIL-IO-AREA
               INITIALIZE OUTFIL-IO-AREA
               MOVE '7'                    TO OUTFIL-IO-AREA (1:1)
               MOVE '    0'                TO OUTFIL-IO-AREA (2:5)
      *                        BUNKNRX    6
               MOVE FAKAA                  TO OUTFIL-IO-AREA (7:4)
               MOVE ' 1'                   TO OUTFIL-IO-AREA (11:2)
               MOVE UMM-IO                 TO OUTFIL-IO-AREA (11:2)
               IF  (I-U1)
                   MOVE PERIOD-IO          TO OUTFIL-IO-AREA (11:2)
               END-IF
               MOVE '         '            TO OUTFIL-IO-AREA (13:9)
      *                        BILNR     21
               MOVE FAKTNR-IO              TO OUTFIL-IO-AREA (16:6)
               MOVE '         '            TO OUTFIL-IO-AREA (22:9)
               MOVE FAKTNR-IO              TO OUTFIL-IO-AREA (25:6)
      *                        POSTNR    30
               MOVE '20180908'             TO OUTFIL-IO-AREA (31:8)
      *                        UYEAR X   34
      *                        UMONTHX   36
      *                        UDAY  X   38
               MOVE PFAAR                  TO OUTFIL-IO-AREA (33:2)
               MOVE PFMND                  TO OUTFIL-IO-AREA (35:2)
               MOVE PFDAG                  TO OUTFIL-IO-AREA (37:2)
               IF  (I-53)
                   MOVE ORAAR-IO           TO OUTFIL-IO-AREA (31:4)
               END-IF
               IF  (I-53)
                   MOVE ORMND-IO           TO OUTFIL-IO-AREA (35:2)
               END-IF
               IF  (I-53)
                   MOVE ORDAG-IO           TO OUTFIL-IO-AREA (37:2)
      *                        DATO      38
               END-IF
               MOVE '20      '             TO OUTFIL-IO-AREA (39:8)
               MOVE FYY-IO                 TO OUTFIL-IO-AREA (41:2)
               MOVE FMM-IO                 TO OUTFIL-IO-AREA (43:2)
               MOVE FDD-IO                 TO OUTFIL-IO-AREA (45:2)
      *                        FDATO     46
               MOVE ' 3'                   TO OUTFIL-IO-AREA (47:2)
               IF  (I-12)
                   MOVE ' 4'               TO OUTFIL-IO-AREA (47:2)
      *                        TK        48
               END-IF
               MOVE '3'                    TO OUTFIL-IO-AREA (49:1)
               IF  (I-21)
                   MOVE '5'                TO OUTFIL-IO-AREA (49:1)
      *                        MOMS      49
               END-IF
               MOVE 'R'                    TO OUTFIL-IO-AREA (50:1)
      *                        KTOKOD    50
               MOVE '      '               TO OUTFIL-IO-AREA (51:6)
               MOVE KUNDNR                 TO OUTFIL-IO-AREA (51:6)
      *                        KTONR     56
               MOVE '  0'                  TO OUTFIL-IO-AREA (57:3)
      *                        DIMA      59
               MOVE '     0'               TO OUTFIL-IO-AREA (60:6)
      *                        DIMB      65
               MOVE '     0'               TO OUTFIL-IO-AREA (66:6)
      *                        DIMC      71
               MOVE '     0'               TO OUTFIL-IO-AREA (72:6)
      *                        DIMD      77
               MOVE '              '       TO OUTFIL-IO-AREA (78:14)
      *                        ID        91
               MOVE '  '                   TO OUTFIL-IO-AREA (92:2)
      *                        REVOMR    93
               MOVE ' '                    TO OUTFIL-IO-AREA (94:1)
      *                        MOTKOD    94
               MOVE '     0'               TO OUTFIL-IO-AREA (95:6)
      *                        MOTKTO   100
               MOVE '   -      0.00'       TO OUTFIL-IO-AREA (101:14)
               IF  (I-54)
                   MOVE ' '                TO OUTFIL-IO-AREA (104:1)
      *                        NOKBEL   114
      *                        NTOKR X  111
      *                        NTOOREX  114
      *                        SUML3C   111
      *                        SUML3A   114
               END-IF
               MOVE SUML3F                 TO OUTFIL-IO-AREA (105:7)
               MOVE SUML3H                 TO OUTFIL-IO-AREA (113:2)
               MOVE 'NOK  '                TO OUTFIL-IO-AREA (115:5)
      *                        VALUTA   119
               MOVE ' 100.000'             TO OUTFIL-IO-AREA (120:8)
      *                        VKURS    127
      *                        BELOP    141
      *                        NTOKR X  138
      *                        NTOOREX  141
               MOVE '   -      0.00'       TO OUTFIL-IO-AREA (128:14)
               IF  (I-54)
                   MOVE ' '                TO OUTFIL-IO-AREA (131:1)
      *                        SUML3C   138
      *                        SUML3A   141
               END-IF
               MOVE SUML3F                 TO OUTFIL-IO-AREA (132:7)
               MOVE SUML3H                 TO OUTFIL-IO-AREA (140:2)
      *                        SUML3MJ  141
               MOVE '            '         TO OUTFIL-IO-AREA (142:12)
      *                        TEKST    153
               MOVE '     0'               TO OUTFIL-IO-AREA (154:6)
      *                        HPROSJ   159
               MOVE '     0'               TO OUTFIL-IO-AREA (160:6)
      *                        PROSNR   165
               MOVE '        0'            TO OUTFIL-IO-AREA (166:9)
      *                        ARTNR    174
               MOVE '     0.00'            TO OUTFIL-IO-AREA (175:9)
      *                        ANTALL   183
               MOVE '     0'               TO OUTFIL-IO-AREA (184:6)
      *                        ANSNR    189
               MOVE '  0'                  TO OUTFIL-IO-AREA (190:3)
      *                        LTART    192
               MOVE '   0.00'              TO OUTFIL-IO-AREA (193:7)
      *                        TIMER    199
               MOVE ' 0'                   TO OUTFIL-IO-AREA (200:2)
      *                        LPER     201
               MOVE '              '       TO OUTFIL-IO-AREA (202:14)
      *                        REFNR    215
               MOVE '               '      TO OUTFIL-IO-AREA (216:15)
               MOVE '                 '    TO OUTFIL-IO-AREA (231:17)
      *                        KIDNR    247
               MOVE '      '               TO OUTFIL-IO-AREA (248:6)
      *                        SAKID    253
               MOVE '3'                    TO OUTFIL-IO-AREA (254:1)
               IF  (I-21)
                   MOVE '5'                TO OUTFIL-IO-AREA (254:1)
      *                        SMOMS    254
               END-IF
               MOVE '     0'               TO OUTFIL-IO-AREA (255:6)
      *                        SHKTO    260
               MOVE '  0'                  TO OUTFIL-IO-AREA (261:3)
      *                        SDIMA    263
               MOVE '     0'               TO OUTFIL-IO-AREA (264:6)
      *                        SDIMB    269
               MOVE '     0'               TO OUTFIL-IO-AREA (270:6)
      *                        SDIMC    275
               MOVE '     0'               TO OUTFIL-IO-AREA (276:6)
      *                        SDIMD    281
               MOVE '              '       TO OUTFIL-IO-AREA (282:14)
      *                        SID      295
               MOVE '     0'               TO OUTFIL-IO-AREA (296:6)
      *                        SHPROS   301
               MOVE '     0'               TO OUTFIL-IO-AREA (302:6)
      *                        SPROS    307
               MOVE '        0'            TO OUTFIL-IO-AREA (308:9)
      *                        SARTNR   316
               MOVE '                    ' TO OUTFIL-IO-AREA (317:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (337:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (357:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (377:20)
               IF  (NOT-I-53)
                   MOVE 'http://gw.autodata.n' TO OUTFIL-IO-AREA
                                                              (317:20)
               END-IF
               IF  (NOT-I-53)
                   MOVE 'o/faktura/test_faktu' TO OUTFIL-IO-AREA
                                                              (337:20)
               END-IF
               IF  (NOT-I-53)
                   MOVE 'ra.php?var=@AAA|@BBB' TO OUTFIL-IO-AREA
                                                              (357:20)
               END-IF
               IF  (NOT-I-53)
                   MOVE 'BBB|@CCCCCC         ' TO OUTFIL-IO-AREA
                                                              (377:20)
               END-IF
               IF  (NOT-I-53)
                   MOVE FIRMA              TO OUTFIL-IO-AREA (369:3)
               END-IF
               IF  (NOT-I-53)
                   MOVE FAKTNR-IO          TO OUTFIL-IO-AREA (374:6)
               END-IF
               IF  (NOT-I-53)
                   MOVE KUNDNR             TO OUTFIL-IO-AREA (382:6)
               END-IF
               MOVE '                    ' TO OUTFIL-IO-AREA (397:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (417:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (437:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (457:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (477:20)
               MOVE '          '           TO OUTFIL-IO-AREA (497:10)
      *                        EYE      506
               MOVE '                  '   TO OUTFIL-IO-AREA (507:18)
      *                        IDNR     524
               MOVE '0'                    TO OUTFIL-IO-AREA (525:1)
      *                        TYPE     525
               MOVE ' '                    TO OUTFIL-IO-AREA (526:1)
      *                        VIDFAK   526
               MOVE '0'                    TO OUTFIL-IO-AREA (527:1)
      *                        FAKTYP   527
               MOVE '                  '   TO OUTFIL-IO-AREA (528:18)
      *                        ARTIKK   545
               MOVE '                    ' TO OUTFIL-IO-AREA (546:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (566:20)
               MOVE '          '           TO OUTFIL-IO-AREA (586:10)
      *                        DIV1     595
               MOVE '                    ' TO OUTFIL-IO-AREA (596:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (616:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (636:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (656:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (676:20)
      *                        DIV2     695
               MOVE '      0'              TO OUTFIL-IO-AREA (696:7)
      *                        DIV3     702
               MOVE '     0'               TO OUTFIL-IO-AREA (703:6)
      *                        KND      708
               MOVE '                    ' TO OUTFIL-IO-AREA (709:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (729:20)
               MOVE '          '           TO OUTFIL-IO-AREA (749:10)
      *                        TEKST2   758
               MOVE '  3'                  TO OUTFIL-IO-AREA (759:3)
               IF  (I-21)
                   MOVE '  5'              TO OUTFIL-IO-AREA (759:3)
      *                        MOMS2    761
               END-IF
               MOVE '  0'                  TO OUTFIL-IO-AREA (762:3)
      *                        SMOMS2   764
               MOVE '                    ' TO OUTFIL-IO-AREA (765:20)
      *                        ID2      784
               MOVE '                    ' TO OUTFIL-IO-AREA (785:20)
      *                        SID2     804
               MOVE '        0'            TO OUTFIL-IO-AREA (805:9)
      *                        HPROS2   813
               MOVE '        0'            TO OUTFIL-IO-AREA (814:9)
      *                        PROSN2   822
               MOVE '        0'            TO OUTFIL-IO-AREA (823:9)
      *                        SHPRO2   831
               MOVE '        0'            TO OUTFIL-IO-AREA (832:9)
      *                        SPROS2   840
               MOVE '        0'            TO OUTFIL-IO-AREA (841:9)
               MOVE KUNDNR                 TO OUTFIL-IO-AREA (844:6)
      *                        KTONR2   849
               MOVE '        0'            TO OUTFIL-IO-AREA (850:9)
      *                        DIMA2    858
               MOVE '        0'            TO OUTFIL-IO-AREA (859:9)
      *                        DIMB2    867
               MOVE '        0'            TO OUTFIL-IO-AREA (868:9)
      *                        DIMC2    876
               MOVE '        0'            TO OUTFIL-IO-AREA (877:9)
      *                        DIMD2    885
               MOVE '        0'            TO OUTFIL-IO-AREA (886:9)
      *                        MOTKT2   894
               MOVE '        0'            TO OUTFIL-IO-AREA (895:9)
      *                        SHKTO2   903
               MOVE '        0'            TO OUTFIL-IO-AREA (904:9)
      *                        SDIMA2   912
               MOVE '        0'            TO OUTFIL-IO-AREA (913:9)
      *                        SDIMB2   921
               MOVE '        0'            TO OUTFIL-IO-AREA (922:9)
      *                        SDIMC2   930
               MOVE '        0'            TO OUTFIL-IO-AREA (931:9)
      *                        SDIMD2   939
               MOVE '          0.000'      TO OUTFIL-IO-AREA (940:15)
      *                        MENGDE   954
               MOVE '                    ' TO OUTFIL-IO-AREA (955:20)
      *                        REFNR2   974
               MOVE '                    ' TO OUTFIL-IO-AREA (967:20)
      *                        SAKID2   986
               MOVE '   0.0000'            TO OUTFIL-IO-AREA (987:9)
      *                        VKURS2   995
               MOVE '                    ' TO OUTFIL-IO-AREA (996:20)
      *                        HPROS3  1015
               MOVE '                    ' TO OUTFIL-IO-AREA (1016:20)
      *                        PROSN3  1035
               MOVE '                    ' TO OUTFIL-IO-AREA (1036:20)
      *                        SHPRO3  1055
               MOVE '                    ' TO OUTFIL-IO-AREA (1056:20)
      *                        SPROS3  1075
               MOVE '        0'            TO OUTFIL-IO-AREA (1076:9)
      *                        KSTMVA  1084
               WRITE OUTFIL-IO-AREA
               MOVE SPACES TO OUTFIL-IO-AREA
               INITIALIZE OUTFIL-IO-AREA
               MOVE '7'                    TO OUTFIL-IO-AREA (1:1)
               MOVE '    0'                TO OUTFIL-IO-AREA (2:5)
      *                        BUNKNRX    6
               MOVE FAKAA                  TO OUTFIL-IO-AREA (7:4)
               MOVE ' 1'                   TO OUTFIL-IO-AREA (11:2)
               MOVE UMM-IO                 TO OUTFIL-IO-AREA (11:2)
               IF  (I-U1)
                   MOVE PERIOD-IO          TO OUTFIL-IO-AREA (11:2)
               END-IF
               MOVE '         '            TO OUTFIL-IO-AREA (13:9)
      *                        BILNR     21
               MOVE FAKTNR-IO              TO OUTFIL-IO-AREA (16:6)
               MOVE '         '            TO OUTFIL-IO-AREA (22:9)
               MOVE FAKTNR-IO              TO OUTFIL-IO-AREA (25:6)
      *                        POSTNR    30
               MOVE '20180908'             TO OUTFIL-IO-AREA (31:8)
      *                        UYEAR X   34
      *                        UMONTHX   36
      *                        UDAY  X   38
               MOVE PFAAR                  TO OUTFIL-IO-AREA (33:2)
               MOVE PFMND                  TO OUTFIL-IO-AREA (35:2)
               MOVE PFDAG                  TO OUTFIL-IO-AREA (37:2)
               IF  (I-53)
                   MOVE ORAAR-IO           TO OUTFIL-IO-AREA (31:4)
               END-IF
               IF  (I-53)
                   MOVE ORMND-IO           TO OUTFIL-IO-AREA (35:2)
               END-IF
               IF  (I-53)
                   MOVE ORDAG-IO           TO OUTFIL-IO-AREA (37:2)
      *                        DATO      38
               END-IF
               MOVE '20      '             TO OUTFIL-IO-AREA (39:8)
               MOVE FYY-IO                 TO OUTFIL-IO-AREA (41:2)
               MOVE FMM-IO                 TO OUTFIL-IO-AREA (43:2)
               MOVE FDD-IO                 TO OUTFIL-IO-AREA (45:2)
      *                        FDATO     46
               MOVE ' 3'                   TO OUTFIL-IO-AREA (47:2)
               IF  (I-12)
                   MOVE ' 4'               TO OUTFIL-IO-AREA (47:2)
      *                        TK        48
               END-IF
               MOVE '0'                    TO OUTFIL-IO-AREA (49:1)
      *                        MOMS      49
               MOVE 'H'                    TO OUTFIL-IO-AREA (50:1)
      *                        KTOKOD    50
               MOVE '  1900'               TO OUTFIL-IO-AREA (51:6)
      *                        KUNDNR    56
      *                        KTONR     56
               MOVE '  0'                  TO OUTFIL-IO-AREA (57:3)
      *                        DIMA      59
               MOVE '     0'               TO OUTFIL-IO-AREA (60:6)
      *                        DIMB      65
               MOVE '     0'               TO OUTFIL-IO-AREA (66:6)
      *                        DIMC      71
               MOVE '     0'               TO OUTFIL-IO-AREA (72:6)
      *                        DIMD      77
               MOVE '              '       TO OUTFIL-IO-AREA (78:14)
      *                        ID        91
               MOVE '  '                   TO OUTFIL-IO-AREA (92:2)
      *                        REVOMR    93
               MOVE ' '                    TO OUTFIL-IO-AREA (94:1)
      *                        MOTKOD    94
               MOVE '     0'               TO OUTFIL-IO-AREA (95:6)
      *                        MOTKTO   100
               MOVE '          0.00'       TO OUTFIL-IO-AREA (101:14)
               IF  (I-54)
                   MOVE '-'                TO OUTFIL-IO-AREA (104:1)
      *                        NOKBEL   114
      *                        NTOKR X  111
      *                        NTOOREX  114
      *                        SUML3C   111
      *                        SUML3A   114
               END-IF
               MOVE SUML3F                 TO OUTFIL-IO-AREA (105:7)
               MOVE SUML3H                 TO OUTFIL-IO-AREA (113:2)
               MOVE 'NOK  '                TO OUTFIL-IO-AREA (115:5)
      *                        VALUTA   119
               MOVE ' 100.000'             TO OUTFIL-IO-AREA (120:8)
      *                        VKURS    127
      *                        BELOP    141
      *                        NTOKR X  138
      *                        NTOOREX  141
               MOVE '          0.00'       TO OUTFIL-IO-AREA (128:14)
               IF  (I-54)
                   MOVE '-'                TO OUTFIL-IO-AREA (131:1)
      *                        SUML3C   138
      *                        SUML3A   141
               END-IF
               MOVE SUML3F                 TO OUTFIL-IO-AREA (132:7)
               MOVE SUML3H                 TO OUTFIL-IO-AREA (140:2)
      *                        SUML3MJ  141
               MOVE '            '         TO OUTFIL-IO-AREA (142:12)
      *                        TEKST    153
               MOVE '     0'               TO OUTFIL-IO-AREA (154:6)
      *                        HPROSJ   159
               MOVE '     0'               TO OUTFIL-IO-AREA (160:6)
      *                        PROSNR   165
               MOVE '        0'            TO OUTFIL-IO-AREA (166:9)
      *                        ARTNR    174
               MOVE '     0.00'            TO OUTFIL-IO-AREA (175:9)
      *                        ANTALL   183
               MOVE '     0'               TO OUTFIL-IO-AREA (184:6)
      *                        ANSNR    189
               MOVE '  0'                  TO OUTFIL-IO-AREA (190:3)
      *                        LTART    192
               MOVE '   0.00'              TO OUTFIL-IO-AREA (193:7)
      *                        TIMER    199
               MOVE ' 0'                   TO OUTFIL-IO-AREA (200:2)
      *                        LPER     201
               MOVE '              '       TO OUTFIL-IO-AREA (202:14)
      *                        REFNR    215
               MOVE '               '      TO OUTFIL-IO-AREA (216:15)
               MOVE '                 '    TO OUTFIL-IO-AREA (231:17)
      *                        KIDNR    247
               MOVE '      '               TO OUTFIL-IO-AREA (248:6)
      *                        SAKID    253
               MOVE '0'                    TO OUTFIL-IO-AREA (254:1)
      *                        SMOMS    254
               MOVE '     0'               TO OUTFIL-IO-AREA (255:6)
      *                        SHKTO    260
               MOVE '  0'                  TO OUTFIL-IO-AREA (261:3)
      *                        SDIMA    263
               MOVE '     0'               TO OUTFIL-IO-AREA (264:6)
      *                        SDIMB    269
               MOVE '     0'               TO OUTFIL-IO-AREA (270:6)
      *                        SDIMC    275
               MOVE '     0'               TO OUTFIL-IO-AREA (276:6)
      *                        SDIMD    281
               MOVE '              '       TO OUTFIL-IO-AREA (282:14)
      *                        SID      295
               MOVE '     0'               TO OUTFIL-IO-AREA (296:6)
      *                        SHPROS   301
               MOVE '     0'               TO OUTFIL-IO-AREA (302:6)
      *                        SPROS    307
               MOVE '        0'            TO OUTFIL-IO-AREA (308:9)
      *                        SARTNR   316
               MOVE '                    ' TO OUTFIL-IO-AREA (317:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (337:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (357:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (377:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (397:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (417:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (437:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (457:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (477:20)
               MOVE '          '           TO OUTFIL-IO-AREA (497:10)
      *                        EYE      506
               MOVE '                  '   TO OUTFIL-IO-AREA (507:18)
      *                        IDNR     524
               MOVE '0'                    TO OUTFIL-IO-AREA (525:1)
      *                        TYPE     525
               MOVE ' '                    TO OUTFIL-IO-AREA (526:1)
      *                        VIDFAK   526
               MOVE '0'                    TO OUTFIL-IO-AREA (527:1)
      *                        FAKTYP   527
               MOVE '                  '   TO OUTFIL-IO-AREA (528:18)
      *                        ARTIKK   545
               MOVE '                    ' TO OUTFIL-IO-AREA (546:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (566:20)
               MOVE '          '           TO OUTFIL-IO-AREA (586:10)
      *                        DIV1     595
               MOVE '                    ' TO OUTFIL-IO-AREA (596:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (616:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (636:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (656:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (676:20)
      *                        DIV2     695
               MOVE '      0'              TO OUTFIL-IO-AREA (696:7)
      *                        DIV3     702
               MOVE '     0'               TO OUTFIL-IO-AREA (703:6)
      *                        KND      708
               MOVE '                    ' TO OUTFIL-IO-AREA (709:20)
               MOVE '                    ' TO OUTFIL-IO-AREA (729:20)
               MOVE '          '           TO OUTFIL-IO-AREA (749:10)
      *                        TEKST2   758
               MOVE '  0'                  TO OUTFIL-IO-AREA (759:3)
      *                        MOMS2    761
               MOVE '  0'                  TO OUTFIL-IO-AREA (762:3)
      *                        SMOMS2   764
               MOVE '                    ' TO OUTFIL-IO-AREA (765:20)
      *                        ID2      784
               MOVE '                    ' TO OUTFIL-IO-AREA (785:20)
      *                        SID2     804
               MOVE '        0'            TO OUTFIL-IO-AREA (805:9)
      *                        HPROS2   813
               MOVE '        0'            TO OUTFIL-IO-AREA (814:9)
      *                        PROSN2   822
               MOVE '        0'            TO OUTFIL-IO-AREA (823:9)
      *                        SHPRO2   831
               MOVE '        0'            TO OUTFIL-IO-AREA (832:9)
      *                        SPROS2   840
               MOVE '     1900'            TO OUTFIL-IO-AREA (841:9)
      *                        KUNDNR   849
      *                        KTONR2   849
               MOVE '        0'            TO OUTFIL-IO-AREA (850:9)
      *                        DIMA2    858
               MOVE '        0'            TO OUTFIL-IO-AREA (859:9)
      *                        DIMB2    867
               MOVE '        0'            TO OUTFIL-IO-AREA (868:9)
      *                        DIMC2    876
               MOVE '        0'            TO OUTFIL-IO-AREA (877:9)
      *                        DIMD2    885
               MOVE '        0'            TO OUTFIL-IO-AREA (886:9)
      *                        MOTKT2   894
               MOVE '        0'            TO OUTFIL-IO-AREA (895:9)
      *                        SHKTO2   903
               MOVE '        0'            TO OUTFIL-IO-AREA (904:9)
      *                        SDIMA2   912
               MOVE '        0'            TO OUTFIL-IO-AREA (913:9)
      *                        SDIMB2   921
               MOVE '        0'            TO OUTFIL-IO-AREA (922:9)
      *                        SDIMC2   930
               MOVE '        0'            TO OUTFIL-IO-AREA (931:9)
      *                        SDIMD2   939
               MOVE '          0.000'      TO OUTFIL-IO-AREA (940:15)
      *                        MENGDE   954
               MOVE '                    ' TO OUTFIL-IO-AREA (955:20)
      *                        REFNR2   974
               MOVE '                    ' TO OUTFIL-IO-AREA (967:20)
      *                        SAKID2   986
               MOVE '   0.0000'            TO OUTFIL-IO-AREA (987:9)
      *                        VKURS2   995
               MOVE '                    ' TO OUTFIL-IO-AREA (996:20)
      *                        HPROS3  1015
               MOVE '                    ' TO OUTFIL-IO-AREA (1016:20)
      *                        PROSN3  1035
               MOVE '                    ' TO OUTFIL-IO-AREA (1036:20)
      *                        SHPRO3  1055
               MOVE '                    ' TO OUTFIL-IO-AREA (1056:20)
      *                        SPROS3  1075
               MOVE '        0'            TO OUTFIL-IO-AREA (1076:9)
      *                        KSTMVA  1084
      *UTFIL  T        L3
      *                        SUML3 1  114
               WRITE OUTFIL-IO-AREA
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
           SET INNFIL-LEVEL-INIT           TO TRUE
           INITIALIZE INNFIL-DATA-FIELDS
           SET INNFIL-EOF-OFF              TO TRUE
           SET INNFIL-PROCESS              TO TRUE
           OPEN INPUT INNFIL
           INITIALIZE KONTOMA-DATA-FIELDS
           OPEN INPUT KONTOMA
           INITIALIZE KONTOKY-DATA-FIELDS
           OPEN INPUT KONTOKY
           INITIALIZE ORDREM-DATA-FIELDS
           OPEN INPUT ORDREM
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           INITIALIZE KUNDEMX-DATA-FIELDS
           OPEN INPUT KUNDEMX
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           INITIALIZE FAKPAR-DATA-FIELDS
           OPEN INPUT FAKPAR
           OPEN OUTPUT OUTFIL.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INNFIL
           CLOSE KONTOMA
           CLOSE KONTOKY
           CLOSE ORDREM
           CLOSE KUNDEMA
           CLOSE KUNDEMX
           CLOSE FIRMAF
           CLOSE FAKPAR
           CLOSE OUTFIL.
 
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
