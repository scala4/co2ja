       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK092R.
      ******************************************* :   Z-WIN-RPG2   ****
      *
      * ENDRING: LAGT INN FIRMA I OUTFIL.
      * ENDRING: 13.10-17 - LAGT INN 1 POS MER I SELVK PÅ FILA.
      * ENDRING: 03.01-17 - LAGT INN nye fnr på Mekonomen
      * ENDRING: 23.06-16 - LAGT INN avgiftskode
      * ENDRING: 06.04-16 - LAGT INN KAT I FIL,UTVIDET RECORDLENGDE
      *
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK092.rpg
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
           SELECT ORDHIST
               ASSIGN TO ORDHIST
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS ORDHIST-STATUS
               RECORD KEY IS ORDHIST-KEY1.
           SELECT ORDNRM
               ASSIGN TO ORDNRM
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS ORDNRM-STATUS
               RECORD KEY IS ORDNRM-KEY1.
           SELECT OUTFIL
               ASSIGN TO OUTFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTFIL-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD INNFIL
               BLOCK CONTAINS 3000
               RECORD CONTAINS 300.
       01  INNFIL-IO-AREA.
           05  INNFIL-IO-AREA-X            PICTURE X(300).
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
       FD ORDHIST
               RECORD CONTAINS 164.
       01  ORDHIST-IO-AREA.
           05  ORDHIST-IO-AREA-X.
               10  ORDHIST-KEY1            PICTURE X(20).
               10  FILLER                  PICTURE X(144).
       FD ORDNRM
               RECORD CONTAINS 100.
       01  ORDNRM-IO-AREA.
           05  ORDNRM-IO-AREA-X.
               10  ORDNRM-KEY1             PICTURE X(9).
               10  FILLER                  PICTURE X(91).
       FD OUTFIL
               RECORD CONTAINS 300.
       01  OUTFIL-IO-AREA.
           05  OUTFIL-IO-AREA-X            PICTURE X(300).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  INNFIL-STATUS               PICTURE 99 VALUE 0.
           10  KONTOMA-STATUS              PICTURE 99 VALUE 0.
           10  KONTOKY-STATUS              PICTURE 99 VALUE 0.
           10  ORDREM-STATUS               PICTURE 99 VALUE 0.
           10  ORDHIST-STATUS              PICTURE 99 VALUE 0.
           10  ORDNRM-STATUS               PICTURE 99 VALUE 0.
           10  OUTFIL-STATUS               PICTURE 99 VALUE 0.
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
           05  ORDHIST-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  ORDNRM-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
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
               10  INNFIL-01-L2.
                   15  INNFIL-01-L2-FAKTNR PICTURE X(6).
               10  INNFIL-01-L1.
                   15  INNFIL-01-L1-ORDNR  PICTURE X(6).
           05  INNFIL-DATA-FIELDS.
               10  REC                     PICTURE X(150).
               10  FIRMA                   PICTURE X(3).
               10  KUNDNR                  PICTURE X(6).
               10  FAKTNR                  PICTURE X(6).
               10  FAKT1                   PICTURE X(1).
               10  ORDNR                   PICTURE X(6).
               10  ORD1                    PICTURE X(1).
               10  VGR                     PICTURE X(5).
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  ART1                    PICTURE X(1).
               10  BM                      PICTURE X(2).
               10  EDBNR                   PICTURE X(7).
               10  EDB2                    PICTURE X(2).
               10  EDB3                    PICTURE X(3).
               10  ORDMOT                  PICTURE X(2).
               10  FK                      PICTURE X(1).
               10  KRTYPE                  PICTURE X(1).
               10  NTOSUM-IO.
                   15  NTOSUM              PICTURE S9(7)V9(2).
               10  FAKMND                  PICTURE X(6).
               10  FMND                    PICTURE X(2).
               10  FAKOMG                  PICTURE X(1).
               10  ORDDTO                  PICTURE X(8).
               10  ORAAR                   PICTURE X(4).
               10  ORMND                   PICTURE X(2).
               10  ORDAG                   PICTURE X(2).
               10  ANTLEV-IO.
                   15  ANTLEV              PICTURE S9(5)V9(2).
               10  SELVK-IO.
                   15  SELVK               PICTURE S9(7)V9(2).
               10  NAVN                    PICTURE X(30).
               10  HND-IO.
                   15  HND                 PICTURE S9(3).
               10  KONTO                   PICTURE X(4).
               10  MERKN                   PICTURE X(1).
               10  KREDGR-IO.
                   15  KREDGR              PICTURE S9(4).
               10  KONSF                   PICTURE X(3).
               10  KKAT-IO.
                   15  KKAT                PICTURE S9(3).
           05  KONTOMA-DATA-FIELDS.
               10  KKONTO                  PICTURE X(4).
               10  KONAVN                  PICTURE X(35).
           05  KONTOKY-DATA-FIELDS.
               10  ADSKTO                  PICTURE X(4).
           05  ORDREM-DATA-FIELDS.
               10  DMVAF                   PICTURE X(1).
               10  DBK                     PICTURE X(1).
               10  DKRET                   PICTURE X(1).
           05  ORDHIST-DATA-FIELDS.
               10  MVAFRI                  PICTURE X(1).
               10  BK                      PICTURE X(1).
               10  KRETYP                  PICTURE X(1).
           05  ORDNRM-DATA-FIELDS.
               10  FMDAT-IO.
                   15  FMDAT               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(6).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  AVRL2-IO.
                   15  AVRL2               PICTURE S9(7)V9(2).
               10  AVRL1-IO.
                   15  AVRL1               PICTURE S9(7)V9(2).
               10  UAMOMS-IO.
                   15  UAMOMS              PICTURE S9(7)V9(2).
               10  MMVA-IO.
                   15  MMVA                PICTURE S9(7)V9(2).
               10  UMVA-IO.
                   15  UMVA                PICTURE S9(7)V9(2).
               10  MOMSL1-IO.
                   15  MOMSL1              PICTURE S9(7)V9(2).
               10  TOTSL1-IO.
                   15  TOTSL1              PICTURE S9(7)V9(2).
               10  TOTSUM-IO.
                   15  TOTSUM              PICTURE S9(7)V9(2).
               10  MEKOID                  PICTURE X(4).
               10  RAFNR                   PICTURE X(4).
               10  MORD                    PICTURE X(10).
               10  KEY20                   PICTURE X(20).
               10  KEY9                    PICTURE X(9).
               10  FDAT6-IO.
                   15  FDAT6               PICTURE S9(6).
               10  FMDAG                   PICTURE X(2).
               10  FMDAT4                  PICTURE X(4).
               10  FMMM                    PICTURE X(2).
               10  FMAA                    PICTURE X(2).
               10  HJKY1                   PICTURE X(4).
               10  HJKY2                   PICTURE X(8).
               10  KTOKY2                  PICTURE X(12).
               10  KEY-X                   PICTURE X(7).
               10  BRF-IO.
                   15  BRF                 PICTURE S9(7)V9(2).
               10  BRF1-IO.
                   15  BRF1                PICTURE S9(7)V9(2).
               10  BRFPR-IO.
                   15  BRFPR               PICTURE S9(3)V9(2).
               10  MOMSAR-IO.
                   15  MOMSAR              PICTURE S9(7)V9(2).
               10  MOMS-IO.
                   15  MOMS                PICTURE S9(7)V9(2).
               10  AVRORE-IO.
                   15  AVRORE              PICTURE S9(7)V9(2).
           05  EDITTING-FIELDS.
               10  EDIT-NTOSUM             PICTURE Z999999,99.
               10  EDIT-SELVK              PICTURE Z999999,99.
               10  EDIT-BRF                PICTURE Z999999,99.
               10  EDIT-BRFPR              PICTURE Z99,99.
               10  EDIT-ANTLEV             PICTURE Z9999,99.
               10  EDIT-AVRL1              PICTURE Z999999,99.
               10  EDIT-MOMS               PICTURE Z999999,99.
               10  EDIT-AVRORE             PICTURE Z999999,99.
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
           SET NOT-I-04                    TO TRUE
           SET NOT-I-06                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-05                    TO TRUE
 
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
           SET NOT-I-12                    TO TRUE
           SET NOT-I-14                    TO TRUE
           IF  (I-L2)
               MOVE 0                      TO AVRL2
           END-IF
           IF  (I-L1)
               MOVE 0                      TO AVRL1
               MOVE 0                      TO UAMOMS
               MOVE 0                      TO MMVA
               MOVE 0                      TO UMVA
               MOVE 0                      TO MOMSL1
               MOVE 0                      TO TOTSL1
           END-IF
           IF  (I-L2)
               MOVE 0                      TO TOTSUM
               MOVE '    '                 TO MEKOID
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
           MOVE KEY20                      TO ORDHIST-KEY1
           READ ORDHIST RECORD KEY IS ORDHIST-KEY1
           INVALID KEY
               SET I-20                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-20                TO TRUE
               PERFORM ORDHIST-FLDSET
               PERFORM ORDHIST-IDSET
           END-READ
           IF  (I-20)
               MOVE '1'                    TO KEY20 (1:1)
               MOVE KEY20                  TO ORDHIST-KEY1
               READ ORDHIST RECORD KEY IS ORDHIST-KEY1
               INVALID KEY
                   SET I-20                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-20            TO TRUE
                   PERFORM ORDHIST-FLDSET
                   PERFORM ORDHIST-IDSET
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
               SET NOT-I-75                TO TRUE
               IF  KRETYP = '2'
                   SET I-75                TO TRUE
               END-IF
               SET NOT-I-76                TO TRUE
               IF  KRETYP = '3'
                   SET I-76                TO TRUE
               END-IF
               SET NOT-I-77                TO TRUE
               IF  KRETYP = '4'
                   SET I-77                TO TRUE
               END-IF
               SET NOT-I-78                TO TRUE
               IF  KRETYP = '5'
                   SET I-78                TO TRUE
               END-IF
           END-IF
           IF  (I-20)
               MOVE 'O'                    TO KEY20 (1:1)
               MOVE KEY20                  TO ORDREM-KEY1
               READ ORDREM RECORD KEY IS ORDREM-KEY1
               INVALID KEY
                   SET I-23                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-23            TO TRUE
                   PERFORM ORDREM-FLDSET
                   PERFORM ORDREM-IDSET
               END-READ
           END-IF
           IF  (NOT-I-23 AND I-20)
               SET NOT-I-21                TO TRUE
               IF  DMVAF = '1'
                   SET I-21                TO TRUE
               END-IF
               SET NOT-I-22                TO TRUE
               IF  DBK = 'K'
                   SET I-22                TO TRUE
               END-IF
               SET NOT-I-75                TO TRUE
               IF  DKRET = '2'
                   SET I-75                TO TRUE
               END-IF
               SET NOT-I-76                TO TRUE
               IF  DKRET = '3'
                   SET I-76                TO TRUE
               END-IF
               SET NOT-I-77                TO TRUE
               IF  DKRET = '4'
                   SET I-77                TO TRUE
               END-IF
               SET NOT-I-78                TO TRUE
               IF  DKRET = '5'
                   SET I-78                TO TRUE
               END-IF
           END-IF
           IF  (I-21)
               SET NOT-I-25                TO TRUE
               IF  KONTO = '3010'
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-22)
               SET NOT-I-26                TO TRUE
               IF  KREDGR = 0
                   SET I-26                TO TRUE
               END-IF
      *****************************************************************
      * CHAIN MOT ORDNRM  FOR Å FINNE FM-DATO                         *
      *****************************************************************
           END-IF
           MOVE FIRMA                      TO KEY9 (1:3)
           MOVE ORDNR                      TO KEY9 (4:6)
           MOVE KEY9                       TO ORDNRM-KEY1
           READ ORDNRM RECORD KEY IS ORDNRM-KEY1
           INVALID KEY
               SET I-39                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-39                TO TRUE
               PERFORM ORDNRM-FLDSET
               PERFORM ORDNRM-IDSET
           END-READ
           IF  (NOT-I-39)
               ADD FMDAT TO ZERO       GIVING FDAT6
               MOVE FDAT6 (1:2)            TO FMDAG
               MOVE FDAT6 (1:4)            TO FMDAT4
               MOVE FMDAT4 (3:2)           TO FMMM
               MOVE FDAT6 (5:2)            TO FMAA
      *
      ***  HENTER ALTERNATIVT KONTONR FRA KONTOKY   ***
      *                    MOVELFIRMA     HJKY1   4
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
      *                    MOVELFIRMA     KEY     7
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
               IF  KKONTO = '8313'
                   SET I-11                TO TRUE
               END-IF
      *****************************************************************
      * CHAINER MOT KONTOMA FOR Å FINNE RIKTIG KONTO, KONTO FRA KONTOKY
      *****************************************************************
      *  10                MOVELFIRMA     KEY     7
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
      * N10      KKONTO    COMP "8313"                   11
      *****************************************************************
      * DIVERSE SJEKKER OG UTREGNINGER                                *
      *****************************************************************
           END-IF
           SET NOT-I-27                    TO TRUE
           IF  KONTO = '3010'
               SET I-27                    TO TRUE
           END-IF
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
           IF  (I-11)
               SET NOT-I-14                TO TRUE
               IF  NTOSUM < 0
                   SET I-14                TO TRUE
               END-IF
           END-IF
           SET NOT-I-15                    TO TRUE
           IF  FAKT1 = '9'
               SET I-15                    TO TRUE
           END-IF
           SET NOT-I-19                    TO TRUE
           IF  FAKT1 = '0'
               SET I-19                    TO TRUE
           END-IF
      ** MLLzo
           IF NTOSUM < 0
               MULTIPLY -1 BY NTOSUM
           END-IF
           IF  (NOT-I-12 AND NOT-I-18)
               ADD NTOSUM                  TO TOTSUM
           END-IF
           IF  (NOT-I-12 AND I-18)
               SUBTRACT NTOSUM             FROM TOTSUM
           END-IF
           IF  (I-12 AND NOT-I-18)
               SUBTRACT NTOSUM             FROM TOTSUM
           END-IF
           IF  (I-12 AND I-18)
               ADD NTOSUM                  TO TOTSUM
           END-IF
           IF  (NOT-I-12 AND NOT-I-18)
               ADD NTOSUM                  TO TOTSL1
           END-IF
           IF  (NOT-I-12 AND I-18)
               SUBTRACT NTOSUM             FROM TOTSL1
           END-IF
           IF  (I-12 AND NOT-I-18)
               SUBTRACT NTOSUM             FROM TOTSL1
           END-IF
           IF  (I-12 AND I-18)
               ADD NTOSUM                  TO TOTSL1
      *          NTOSUM    MULT 1,25      NSMVA   92
      *          NSMVA     SUB  NTOSUM    MVA     92
           END-IF
           IF  (NOT-I-12)
               SUBTRACT SELVK FROM NTOSUM GIVING BRF
           END-IF
           IF  (I-12)
               ADD SELVK TO NTOSUM     GIVING BRF
           END-IF
           SET NOT-I-16                    TO TRUE
           IF  NTOSUM = 0,00
               SET I-16                    TO TRUE
           END-IF
           IF  (NOT-I-16)
               MULTIPLY 100 BY BRF     GIVING BRF1
               DIVIDE BRF1 BY NTOSUM   GIVING BRFPR
           END-IF
           IF  (I-16)
               MOVE 0                      TO BRFPR
           END-IF
           IF  (I-11)
               OR  (I-21)
               SET I-50                    TO TRUE
           END-IF
           SET NOT-I-63                    TO TRUE
           IF  FIRMA = '156'
               SET I-63                    TO TRUE
           END-IF
           SET NOT-I-32                    TO TRUE
           IF  FIRMA = '100'
               SET I-32                    TO TRUE
           END-IF
           SET NOT-I-34                    TO TRUE
           IF  FIRMA = '143'
               SET I-34                    TO TRUE
           END-IF
           SET NOT-I-35                    TO TRUE
           IF  FIRMA = '196'
               SET I-35                    TO TRUE
           END-IF
           SET NOT-I-67                    TO TRUE
           IF  FIRMA = '212'
               SET I-67                    TO TRUE
           END-IF
           SET NOT-I-36                    TO TRUE
           IF  FIRMA = '226'
               SET I-36                    TO TRUE
           END-IF
           SET NOT-I-37                    TO TRUE
           IF  FIRMA = '326'
               SET I-37                    TO TRUE
           END-IF
           SET NOT-I-38                    TO TRUE
           IF  FIRMA = '392'
               SET I-38                    TO TRUE
           END-IF
           SET NOT-I-40                    TO TRUE
           IF  FIRMA = '419'
               SET I-40                    TO TRUE
           END-IF
           SET NOT-I-41                    TO TRUE
           IF  FIRMA = '435'
               SET I-41                    TO TRUE
           END-IF
           SET NOT-I-42                    TO TRUE
           IF  FIRMA = '443'
               SET I-42                    TO TRUE
           END-IF
           SET NOT-I-33                    TO TRUE
           IF  FIRMA = '763'
               SET I-33                    TO TRUE
           END-IF
           SET NOT-I-70                    TO TRUE
           IF  FIRMA = '474'
               SET I-70                    TO TRUE
           END-IF
           SET NOT-I-71                    TO TRUE
           IF  FIRMA = '494'
               SET I-71                    TO TRUE
           END-IF
           SET NOT-I-43                    TO TRUE
           IF  FIRMA = '526'
               SET I-43                    TO TRUE
           END-IF
      *          FIRMA     COMP "840"                    65
           SET NOT-I-66                    TO TRUE
           IF  FIRMA = '561'
               SET I-66                    TO TRUE
           END-IF
      *          FIRMA     COMP "863"                    69
           SET NOT-I-44                    TO TRUE
           IF  FIRMA = '590'
               SET I-44                    TO TRUE
           END-IF
           SET NOT-I-45                    TO TRUE
           IF  FIRMA = '594'
               SET I-45                    TO TRUE
           END-IF
      *          FIRMA     COMP "935"                    46
      *          FIRMA     COMP "937"                    47
           SET NOT-I-48                    TO TRUE
           IF  FIRMA = '627'
               SET I-48                    TO TRUE
           END-IF
           SET NOT-I-49                    TO TRUE
           IF  FIRMA = '632'
               SET I-49                    TO TRUE
           END-IF
      *          FIRMA     COMP "941"                    64
      *          FIRMA     COMP "952"                    51
           SET NOT-I-52                    TO TRUE
           IF  FIRMA = '646'
               SET I-52                    TO TRUE
           END-IF
           SET NOT-I-53                    TO TRUE
           IF  FIRMA = '651'
               SET I-53                    TO TRUE
           END-IF
           SET NOT-I-54                    TO TRUE
           IF  FIRMA = '300'
               SET I-54                    TO TRUE
           END-IF
           SET NOT-I-62                    TO TRUE
           IF  FIRMA = '679'
               SET I-62                    TO TRUE
           END-IF
           SET NOT-I-55                    TO TRUE
           IF  FIRMA = '972'
               SET I-55                    TO TRUE
           END-IF
           SET NOT-I-56                    TO TRUE
           IF  FIRMA = '684'
               SET I-56                    TO TRUE
           END-IF
           SET NOT-I-58                    TO TRUE
           IF  FIRMA = '700'
               SET I-58                    TO TRUE
           END-IF
           SET NOT-I-59                    TO TRUE
           IF  FIRMA = '894'
               SET I-59                    TO TRUE
           END-IF
           SET NOT-I-60                    TO TRUE
           IF  FIRMA = '985'
               SET I-60                    TO TRUE
           END-IF
           SET NOT-I-61                    TO TRUE
           IF  FIRMA = '934'
               SET I-61                    TO TRUE
           END-IF
      *          FIRMA     COMP "994"                    57
      *  30                MOVE "6015"    MEKOID  4
           IF  (I-63)
               MOVE '6064'                 TO MEKOID
           END-IF
           IF  (I-32)
               MOVE '6023'                 TO MEKOID
           END-IF
           IF  (I-33)
               MOVE '6028'                 TO MEKOID
           END-IF
           IF  (I-34)
               MOVE '6010'                 TO MEKOID
           END-IF
           IF  (I-35)
               MOVE '6052'                 TO MEKOID
           END-IF
           IF  (I-36)
               MOVE '6051'                 TO MEKOID
           END-IF
           IF  (I-37)
               MOVE '6006'                 TO MEKOID
           END-IF
           IF  (I-38)
               MOVE '6026'                 TO MEKOID
           END-IF
           IF  (I-40)
               MOVE '6013'                 TO MEKOID
           END-IF
           IF  (I-41)
               MOVE '6007'                 TO MEKOID
           END-IF
           IF  (I-42)
               MOVE '6041'                 TO MEKOID
           END-IF
           IF  (I-43)
               MOVE '6005'                 TO MEKOID
           END-IF
           IF  (I-44)
               MOVE '6004'                 TO MEKOID
           END-IF
           IF  (I-45)
               MOVE '6021'                 TO MEKOID
      *  46                MOVE "6003"    MEKOID  4
      *  47                MOVE "6012"    MEKOID  4
           END-IF
           IF  (I-48)
               MOVE '6024'                 TO MEKOID
      *  48                MOVE "5999"    MEKOID  4
           END-IF
           IF  (I-49)
               MOVE '6033'                 TO MEKOID
      *  64                MOVE "6070"    MEKOID  4
      *  51                MOVE "6039"    MEKOID  4
           END-IF
           IF  (I-52)
               MOVE '6014'                 TO MEKOID
           END-IF
           IF  (I-53)
               MOVE '6016'                 TO MEKOID
           END-IF
           IF  (I-54)
               MOVE '6017'                 TO MEKOID
           END-IF
           IF  (I-55)
               MOVE '6008'                 TO MEKOID
           END-IF
           IF  (I-56)
               MOVE '6009'                 TO MEKOID
      *  57                MOVE "6063"    MEKOID  4
           END-IF
           IF  (I-58)
               MOVE '6040'                 TO MEKOID
           END-IF
           IF  (I-59)
               MOVE '6042'                 TO MEKOID
           END-IF
           IF  (I-60)
               MOVE '6002'                 TO MEKOID
           END-IF
           IF  (I-61)
               MOVE '6031'                 TO MEKOID
           END-IF
           IF  (I-62)
               MOVE '6067'                 TO MEKOID
      *  63                MOVE "6044"    MEKOID  4
      *  64                MOVE "6024"    MEKOID  4
      *  65                MOVE "6050"    MEKOID  4
           END-IF
           IF  (I-66)
               MOVE '6053'                 TO MEKOID
           END-IF
           IF  (I-67)
               MOVE '6049'                 TO MEKOID
      *  69                MOVE "6054"    MEKOID  4
           END-IF
           IF  (I-70)
               MOVE '6058'                 TO MEKOID
           END-IF
           IF  (I-71)
               MOVE '6056'                 TO MEKOID
      *****************************************************************
      * RUTINE FOR BEREGNING AV FAKTURADAG                         *
      *****************************************************************
           END-IF
           SET NOT-I-80                    TO TRUE
           IF  FAKOMG = 'A'
               SET I-80                    TO TRUE
           END-IF
           IF  (NOT-I-80)
               SET NOT-I-80                TO TRUE
               IF  FAKOMG = '1'
                   SET I-80                TO TRUE
               END-IF
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  FAKOMG = 'B'
               SET I-81                    TO TRUE
           END-IF
           IF  (NOT-I-81)
               SET NOT-I-81                TO TRUE
               IF  FAKOMG = '2'
                   SET I-81                TO TRUE
               END-IF
           END-IF
           SET NOT-I-82                    TO TRUE
           IF  FAKOMG = 'C'
               SET I-82                    TO TRUE
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  FAKOMG = '3'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           SET NOT-I-83                    TO TRUE
           IF  FAKOMG = 'D'
               SET I-83                    TO TRUE
           END-IF
           IF  (NOT-I-83)
               SET NOT-I-83                TO TRUE
               IF  FAKOMG = '4'
                   SET I-83                TO TRUE
               END-IF
           END-IF
           SET NOT-I-84                    TO TRUE
           IF  FMND = '01'
               SET I-84                    TO TRUE
           END-IF
           IF  (NOT-I-84)
               SET NOT-I-84                TO TRUE
               IF  FMND = '03'
                   SET I-84                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-84)
               SET NOT-I-84                TO TRUE
               IF  FMND = '05'
                   SET I-84                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-84)
               SET NOT-I-84                TO TRUE
               IF  FMND = '07'
                   SET I-84                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-84)
               SET NOT-I-84                TO TRUE
               IF  FMND = '08'
                   SET I-84                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-84)
               SET NOT-I-84                TO TRUE
               IF  FMND = '10'
                   SET I-84                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-84)
               SET NOT-I-84                TO TRUE
               IF  FMND = '12'
                   SET I-84                TO TRUE
               END-IF
           END-IF
           SET NOT-I-85                    TO TRUE
           IF  FMND = '02'
               SET I-85                    TO TRUE
           END-IF
           SET NOT-I-86                    TO TRUE
           IF  FMND = '04'
               SET I-86                    TO TRUE
           END-IF
           IF  (NOT-I-86)
               SET NOT-I-86                TO TRUE
               IF  FMND = '06'
                   SET I-86                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-86)
               SET NOT-I-86                TO TRUE
               IF  FMND = '09'
                   SET I-86                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-86)
               SET NOT-I-86                TO TRUE
               IF  FMND = '11'
                   SET I-86                TO TRUE
               END-IF
      *****************************************************************
      * SUBRUTINE FOR BEREGNING AV FAKTURATOTAL MED AVRUNDING.        *
      *****************************************************************
           END-IF
           .
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1 AND I-50)
               GO TO ENDL1-T
           END-IF
           IF  (I-L1 AND I-19)
               ADD TOTSL1 TO ZERO      GIVING BUMVA
               MOVE 0                      TO BMMVA
               MOVE 0                      TO MVA
               CALL 'RUTMOMS2' USING MVFELT-XX-DATA-FIELDS
           END-IF
           IF  (I-L1 AND I-19)
               ADD MVA TO ZERO         GIVING MOMSL1 ROUNDED
               ADD BMMVA TO ZERO       GIVING MMVA
      *****************************************************************
      * SUBRUTINE FOR BEREGNING AV MOMS UTEN AVRUNDING.               *
      *****************************************************************
           END-IF
           IF  (I-L1 AND I-19)
               ADD TOTSL1 TO ZERO      GIVING BUMVA
               MOVE 0                      TO BMMVA
               MOVE 99,99                  TO MVA
               CALL 'RUTMOMS2' USING MVFELT-XX-DATA-FIELDS
           END-IF
           IF  (I-L1 AND I-19)
               ADD MVA TO ZERO         GIVING UAMOMS ROUNDED
               ADD BMMVA TO ZERO       GIVING UMVA
      *****************************************************************
      * BEREGNING AV ØREAVRUNDING PR.KONTANTFAKTURA                   *
      *****************************************************************
           END-IF
           IF  (I-L1 AND I-19)
               SUBTRACT UAMOMS FROM MOMSL1 GIVING AVRL1 ROUNDED
               SET NOT-I-94                TO TRUE
               IF  AVRL1 < 0
                   SET I-94                TO TRUE
               END-IF
           END-IF
           IF  (I-L1)
      ** MLLzo
               IF NTOSUM < 0
                   MULTIPLY -1 BY NTOSUM
               END-IF
           END-IF
           IF  (I-L1 AND I-19 AND NOT-I-94)
               ADD AVRL1                   TO AVRL2 ROUNDED
           END-IF
           IF  (I-L1 AND I-19 AND I-94)
               SUBTRACT AVRL1              FROM AVRL2 ROUNDED
           END-IF
           IF  (I-L1)
               SET NOT-I-95                TO TRUE
               IF  AVRL2 < 0
                   SET I-95                TO TRUE
               END-IF
      *****************************************************************
      * SUBRUTINE FOR BEREGNING AV FAKTURATOTAL MED AVRUNDING.        *
      *****************************************************************
           END-IF
           IF  (I-L2 AND I-50)
               GO TO ENDL1-T
      *2                   GOTO ENDL1
           END-IF
           IF  (I-L2)
               ADD TOTSUM TO ZERO      GIVING BUMVA
               MOVE 0                      TO BMMVA
               MOVE 0                      TO MVA
               CALL 'RUTMOMS2' USING MVFELT-XX-DATA-FIELDS
               ADD MVA TO ZERO         GIVING MOMSAR
      *****************************************************************
      * SUBRUTINE FOR BEREGNING AV MOMS UTEN AVRUNDING.               *
      *****************************************************************
           END-IF
           IF  (I-L2)
               ADD TOTSUM TO ZERO      GIVING BUMVA
               SET NOT-I-96                TO TRUE
               IF  TOTSUM < 0
                   SET I-96                TO TRUE
               END-IF
               MOVE 0                      TO BMMVA
               MOVE 99,99                  TO MVA
               CALL 'RUTMOMS2' USING MVFELT-XX-DATA-FIELDS
               ADD MVA TO ZERO         GIVING MOMS
      *****************************************************************
      * BEREGNING AV ØREAVRUNDING.                                    *
      *****************************************************************
           END-IF
           IF  (I-L2 AND NOT-I-19)
               SUBTRACT MOMS FROM MOMSAR GIVING AVRORE
               SET NOT-I-93                TO TRUE
               IF  AVRORE < 0
                   SET I-93                TO TRUE
               END-IF
           END-IF.
 
       ENDL1-T.
      *****************************************************************
           CONTINUE.
 
       INNFIL-GET SECTION.
       INNFIL-GET-P.
           IF  INNFIL-EOF-OFF
               READ INNFIL
               AT END
                   SET INNFIL-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INNFIL-FLDSET SECTION.
       INNFIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INNFIL-IO-AREA (1:150) TO REC (1:150)
               MOVE INNFIL-IO-AREA (1:3)   TO FIRMA (1:3)
               MOVE INNFIL-IO-AREA (4:6)   TO KUNDNR (1:6)
               MOVE INNFIL-IO-AREA (10:6)  TO FAKTNR (1:6)
               MOVE INNFIL-IO-AREA (10:1)  TO FAKT1 (1:1)
               MOVE INNFIL-IO-AREA (16:6)  TO ORDNR (1:6)
               MOVE INNFIL-IO-AREA (16:1)  TO ORD1 (1:1)
               MOVE INNFIL-IO-AREA (22:5)  TO VGR (1:5)
               MOVE INNFIL-IO-AREA (27:3)  TO ALFA (1:3)
               MOVE INNFIL-IO-AREA (30:20) TO ARTNR (1:20)
               MOVE INNFIL-IO-AREA (30:1)  TO ART1 (1:1)
               MOVE INNFIL-IO-AREA (199:2) TO BM (1:2)
               MOVE INNFIL-IO-AREA (50:7)  TO EDBNR (1:7)
               MOVE INNFIL-IO-AREA (50:2)  TO EDB2 (1:2)
               MOVE INNFIL-IO-AREA (50:3)  TO EDB3 (1:3)
               MOVE INNFIL-IO-AREA (57:2)  TO ORDMOT (1:2)
               MOVE INNFIL-IO-AREA (59:1)  TO FK (1:1)
               MOVE INNFIL-IO-AREA (60:1)  TO KRTYPE (1:1)
               MOVE INNFIL-IO-AREA (61:9)  TO NTOSUM-IO
               INSPECT NTOSUM-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (70:6)  TO FAKMND (1:6)
               MOVE INNFIL-IO-AREA (74:2)  TO FMND (1:2)
               MOVE INNFIL-IO-AREA (100:1) TO FAKOMG (1:1)
               MOVE INNFIL-IO-AREA (76:8)  TO ORDDTO (1:8)
               MOVE INNFIL-IO-AREA (76:4)  TO ORAAR (1:4)
               MOVE INNFIL-IO-AREA (80:2)  TO ORMND (1:2)
               MOVE INNFIL-IO-AREA (82:2)  TO ORDAG (1:2)
               MOVE INNFIL-IO-AREA (84:7)  TO ANTLEV-IO
               INSPECT ANTLEV-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (91:9)  TO SELVK-IO
               INSPECT SELVK-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (101:30) TO NAVN (1:30)
               MOVE INNFIL-IO-AREA (131:3) TO HND-IO
               INSPECT HND-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (134:4) TO KONTO (1:4)
               MOVE INNFIL-IO-AREA (138:1) TO MERKN (1:1)
               MOVE INNFIL-IO-AREA (139:4) TO KREDGR-IO
               INSPECT KREDGR-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (143:3) TO KONSF (1:3)
               MOVE INNFIL-IO-AREA (201:3) TO KKAT-IO
               INSPECT KKAT-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       INNFIL-IDSET SECTION.
       INNFIL-IDSET-P.
           SET I-01                        TO TRUE.
 
       INNFIL-CHK-LEVEL SECTION.
       INNFIL-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO INNFIL-LEVEL-01
               MOVE INNFIL-IO-AREA (10:6)  TO INNFIL-01-L2-FAKTNR
               MOVE INNFIL-IO-AREA (16:6)  TO INNFIL-01-L1-ORDNR
               IF  INNFIL-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INNFIL-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INNFIL-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
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
               MOVE ORDREM-IO-AREA (89:1)  TO DMVAF (1:1)
               MOVE ORDREM-IO-AREA (92:1)  TO DBK (1:1)
               MOVE ORDREM-IO-AREA (99:1)  TO DKRET (1:1)
           END-EVALUATE.
 
       ORDREM-IDSET SECTION.
       ORDREM-IDSET-P.
           SET I-06                        TO TRUE.
 
       ORDHIST-FLDSET SECTION.
       ORDHIST-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE ORDHIST-IO-AREA (89:1) TO MVAFRI (1:1)
               MOVE ORDHIST-IO-AREA (92:1) TO BK (1:1)
               MOVE ORDHIST-IO-AREA (99:1) TO KRETYP (1:1)
           END-EVALUATE.
 
       ORDHIST-IDSET SECTION.
       ORDHIST-IDSET-P.
           SET I-03                        TO TRUE.
 
       ORDNRM-FLDSET SECTION.
       ORDNRM-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE ORDNRM-IO-AREA (47:4)  TO FMDAT-IO
           END-EVALUATE.
 
       ORDNRM-IDSET SECTION.
       ORDNRM-IDSET-P.
           SET I-05                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-1P)
               MOVE SPACES TO OUTFIL-IO-AREA
               INITIALIZE OUTFIL-IO-AREA
               MOVE 'FAKTNR'               TO OUTFIL-IO-AREA (1:6)
               MOVE ';'                    TO OUTFIL-IO-AREA (7:1)
               MOVE 'ORDNR '               TO OUTFIL-IO-AREA (8:6)
               MOVE ';'                    TO OUTFIL-IO-AREA (14:1)
               MOVE 'ORDRE DATO'           TO OUTFIL-IO-AREA (15:10)
               MOVE ';'                    TO OUTFIL-IO-AREA (25:1)
               MOVE 'OM'                   TO OUTFIL-IO-AREA (26:2)
               MOVE ';'                    TO OUTFIL-IO-AREA (28:1)
               MOVE 'KUNDNR'               TO OUTFIL-IO-AREA (29:6)
               MOVE ';'                    TO OUTFIL-IO-AREA (35:1)
               MOVE 'KUNDENAVN '           TO OUTFIL-IO-AREA (36:10)
               MOVE ';'                    TO OUTFIL-IO-AREA (66:1)
               MOVE 'HND'                  TO OUTFIL-IO-AREA (67:3)
               MOVE ';'                    TO OUTFIL-IO-AREA (70:1)
               MOVE 'KNTO'                 TO OUTFIL-IO-AREA (71:4)
               MOVE ';'                    TO OUTFIL-IO-AREA (75:1)
               MOVE 'NETTOSUM  '           TO OUTFIL-IO-AREA (77:10)
               MOVE ';'                    TO OUTFIL-IO-AREA (87:1)
               MOVE 'MVA       '           TO OUTFIL-IO-AREA (89:10)
               MOVE ';'                    TO OUTFIL-IO-AREA (99:1)
               MOVE 'FNR'                  TO OUTFIL-IO-AREA (100:3)
               MOVE ';'                    TO OUTFIL-IO-AREA (103:1)
               MOVE 'ALF'                  TO OUTFIL-IO-AREA (104:3)
               MOVE ';'                    TO OUTFIL-IO-AREA (107:1)
               MOVE 'ARTIKELNR '           TO OUTFIL-IO-AREA (108:10)
               MOVE ';'                    TO OUTFIL-IO-AREA (128:1)
               MOVE 'VGR  '                TO OUTFIL-IO-AREA (129:5)
               MOVE ';'                    TO OUTFIL-IO-AREA (134:1)
               MOVE 'SELVKOST  '           TO OUTFIL-IO-AREA (136:10)
               MOVE ';'                    TO OUTFIL-IO-AREA (146:1)
               MOVE 'BRF I KR  '           TO OUTFIL-IO-AREA (147:10)
               MOVE ';'                    TO OUTFIL-IO-AREA (157:1)
               MOVE 'BRF % '               TO OUTFIL-IO-AREA (158:6)
               MOVE ';'                    TO OUTFIL-IO-AREA (164:1)
               MOVE 'M'                    TO OUTFIL-IO-AREA (165:1)
               MOVE ';'                    TO OUTFIL-IO-AREA (166:1)
               MOVE 'K'                    TO OUTFIL-IO-AREA (167:1)
               MOVE ';'                    TO OUTFIL-IO-AREA (168:1)
               MOVE 'KONTONAVN '           TO OUTFIL-IO-AREA (169:10)
               MOVE ';'                    TO OUTFIL-IO-AREA (204:1)
               MOVE 'ANT LEV'              TO OUTFIL-IO-AREA (206:7)
               MOVE ';'                    TO OUTFIL-IO-AREA (213:1)
               MOVE 'MEKO'                 TO OUTFIL-IO-AREA (214:4)
               MOVE ';'                    TO OUTFIL-IO-AREA (218:1)
               MOVE 'AVRUND.'              TO OUTFIL-IO-AREA (225:7)
               MOVE ';'                    TO OUTFIL-IO-AREA (232:1)
               MOVE 'FAK.DATO'             TO OUTFIL-IO-AREA (233:8)
               MOVE ';'                    TO OUTFIL-IO-AREA (241:1)
               MOVE 'BM'                   TO OUTFIL-IO-AREA (242:2)
               MOVE ';'                    TO OUTFIL-IO-AREA (244:1)
               MOVE 'KAT'                  TO OUTFIL-IO-AREA (245:3)
               MOVE ';'                    TO OUTFIL-IO-AREA (248:1)
               MOVE 'K'                    TO OUTFIL-IO-AREA (249:1)
               MOVE ';'                    TO OUTFIL-IO-AREA (250:1)
               MOVE 'KREDITERINGSTYPE     ' TO OUTFIL-IO-AREA (251:21)
               MOVE ';'                    TO OUTFIL-IO-AREA (272:1)
               MOVE 'AK'                   TO OUTFIL-IO-AREA (273:2)
               MOVE ';'                    TO OUTFIL-IO-AREA (275:1)
               MOVE 'FERD.MELDT'           TO OUTFIL-IO-AREA (276:10)
               MOVE ';'                    TO OUTFIL-IO-AREA (286:1)
               WRITE OUTFIL-IO-AREA
           END-IF
           IF  (I-01)
               MOVE SPACES TO OUTFIL-IO-AREA
               INITIALIZE OUTFIL-IO-AREA
               MOVE FAKTNR                 TO OUTFIL-IO-AREA (1:6)
               MOVE ';'                    TO OUTFIL-IO-AREA (7:1)
               MOVE ORDNR                  TO OUTFIL-IO-AREA (8:6)
               MOVE ';'                    TO OUTFIL-IO-AREA (14:1)
               MOVE ORDAG                  TO OUTFIL-IO-AREA (15:2)
               MOVE '/'                    TO OUTFIL-IO-AREA (17:1)
               MOVE ORMND                  TO OUTFIL-IO-AREA (18:2)
               MOVE '/'                    TO OUTFIL-IO-AREA (20:1)
               MOVE ORAAR                  TO OUTFIL-IO-AREA (21:4)
               MOVE ';'                    TO OUTFIL-IO-AREA (25:1)
               MOVE ORDMOT                 TO OUTFIL-IO-AREA (26:2)
               MOVE ';'                    TO OUTFIL-IO-AREA (28:1)
               MOVE KUNDNR                 TO OUTFIL-IO-AREA (29:6)
               MOVE ';'                    TO OUTFIL-IO-AREA (35:1)
               MOVE NAVN                   TO OUTFIL-IO-AREA (36:30)
               MOVE ';'                    TO OUTFIL-IO-AREA (66:1)
               MOVE HND-IO                 TO OUTFIL-IO-AREA (67:3)
               MOVE ';'                    TO OUTFIL-IO-AREA (70:1)
               IF  (NOT-I-10)
                   MOVE KKONTO             TO OUTFIL-IO-AREA (71:4)
               END-IF
               MOVE ';'                    TO OUTFIL-IO-AREA (75:1)
               IF  (I-10)
                   MOVE KONTO              TO OUTFIL-IO-AREA (71:4)
               END-IF
               MOVE ';'                    TO OUTFIL-IO-AREA (75:1)
               IF  (NOT-I-10 AND I-27)
                   MOVE KONTO              TO OUTFIL-IO-AREA (71:4)
               END-IF
               MOVE ';'                    TO OUTFIL-IO-AREA (75:1)
      *                   25 10          74 "3015"
               IF  (I-25)
                   MOVE '3015'             TO OUTFIL-IO-AREA (71:4)
               END-IF
               MOVE ';'                    TO OUTFIL-IO-AREA (75:1)
               MOVE NTOSUM                 TO EDIT-NTOSUM
               MOVE EDIT-NTOSUM            TO OUTFIL-IO-AREA (77:10)
               IF  (I-12)
                   MOVE '-'                TO OUTFIL-IO-AREA (76:1)
               END-IF
               IF  (I-18)
                   MOVE ' '                TO OUTFIL-IO-AREA (76:1)
               END-IF
               IF  (I-18 AND NOT-I-12)
                   MOVE '-'                TO OUTFIL-IO-AREA (76:1)
               END-IF
               IF  (I-18 AND I-12)
                   MOVE ' '                TO OUTFIL-IO-AREA (76:1)
               END-IF
               IF  (I-14)
                   MOVE '-'                TO OUTFIL-IO-AREA (76:1)
               END-IF
               MOVE ';'                    TO OUTFIL-IO-AREA (87:1)
               MOVE ';'                    TO OUTFIL-IO-AREA (99:1)
               MOVE FIRMA                  TO OUTFIL-IO-AREA (100:3)
               MOVE ';'                    TO OUTFIL-IO-AREA (103:1)
               MOVE ALFA                   TO OUTFIL-IO-AREA (104:3)
               MOVE ';'                    TO OUTFIL-IO-AREA (107:1)
               MOVE ARTNR                  TO OUTFIL-IO-AREA (108:20)
               MOVE ';'                    TO OUTFIL-IO-AREA (128:1)
               MOVE VGR                    TO OUTFIL-IO-AREA (129:5)
               MOVE ';'                    TO OUTFIL-IO-AREA (134:1)
               MOVE SELVK                  TO EDIT-SELVK
               MOVE EDIT-SELVK             TO OUTFIL-IO-AREA (136:10)
               IF  (I-12)
                   MOVE '-'                TO OUTFIL-IO-AREA (135:1)
               END-IF
               IF  (I-18)
                   MOVE ' '                TO OUTFIL-IO-AREA (135:1)
               END-IF
               IF  (I-18 AND NOT-I-12)
                   MOVE '-'                TO OUTFIL-IO-AREA (135:1)
               END-IF
               IF  (I-18 AND I-12)
                   MOVE ' '                TO OUTFIL-IO-AREA (135:1)
               END-IF
               MOVE ';'                    TO OUTFIL-IO-AREA (146:1)
               MOVE BRF                    TO EDIT-BRF
               MOVE EDIT-BRF               TO OUTFIL-IO-AREA (147:10)
               MOVE ';'                    TO OUTFIL-IO-AREA (157:1)
               MOVE BRFPR                  TO EDIT-BRFPR
               MOVE EDIT-BRFPR             TO OUTFIL-IO-AREA (158:6)
               MOVE ';'                    TO OUTFIL-IO-AREA (164:1)
               MOVE MERKN                  TO OUTFIL-IO-AREA (165:1)
               MOVE ';'                    TO OUTFIL-IO-AREA (166:1)
               IF  (I-22)
                   MOVE BK                 TO OUTFIL-IO-AREA (167:1)
      *                     N20BK       167
               END-IF
               MOVE ';'                    TO OUTFIL-IO-AREA (168:1)
               IF  (NOT-I-26)
                   MOVE ' '                TO OUTFIL-IO-AREA (167:1)
               END-IF
               MOVE ';'                    TO OUTFIL-IO-AREA (168:1)
               IF  (NOT-I-10)
                   MOVE KONAVN             TO OUTFIL-IO-AREA (169:35)
               END-IF
               IF  (NOT-I-17)
                   MOVE KONAVN             TO OUTFIL-IO-AREA (169:35)
               END-IF
               MOVE ';'                    TO OUTFIL-IO-AREA (204:1)
               IF  (I-17 AND I-10)
                   MOVE '* KONTONAVN IKKE FUNNET ' TO OUTFIL-IO-AREA
                                                              (170:24)
               END-IF
               MOVE ';'                    TO OUTFIL-IO-AREA (204:1)
               IF  (I-25)
                   MOVE 'SALG AVG. FRITT     ' TO OUTFIL-IO-AREA
                                                              (169:20)
               END-IF
               IF  (I-25)
                   MOVE '               '  TO OUTFIL-IO-AREA (189:15)
               END-IF
               MOVE ';'                    TO OUTFIL-IO-AREA (204:1)
               MOVE ANTLEV                 TO EDIT-ANTLEV
               MOVE EDIT-ANTLEV            TO OUTFIL-IO-AREA (205:8)
               MOVE ';'                    TO OUTFIL-IO-AREA (213:1)
      *                                 217 "****"
               MOVE MEKOID                 TO OUTFIL-IO-AREA (214:4)
               MOVE ';'                    TO OUTFIL-IO-AREA (218:1)
               MOVE ';'                    TO OUTFIL-IO-AREA (232:1)
               MOVE FAKMND                 TO OUTFIL-IO-AREA (233:6)
               IF  (I-80)
                   MOVE '08'               TO OUTFIL-IO-AREA (239:2)
               END-IF
               IF  (I-81)
                   MOVE '15'               TO OUTFIL-IO-AREA (239:2)
               END-IF
               IF  (I-82)
                   MOVE '23'               TO OUTFIL-IO-AREA (239:2)
               END-IF
               IF  (I-84 AND I-83)
                   MOVE '31'               TO OUTFIL-IO-AREA (239:2)
               END-IF
               IF  (I-85 AND I-83)
                   MOVE '28'               TO OUTFIL-IO-AREA (239:2)
               END-IF
               IF  (I-86 AND I-83)
                   MOVE '30'               TO OUTFIL-IO-AREA (239:2)
               END-IF
               MOVE ';'                    TO OUTFIL-IO-AREA (241:1)
               MOVE BM                     TO OUTFIL-IO-AREA (242:2)
               MOVE ';'                    TO OUTFIL-IO-AREA (244:1)
               MOVE KKAT-IO                TO OUTFIL-IO-AREA (245:3)
               MOVE ';'                    TO OUTFIL-IO-AREA (248:1)
               MOVE KRETYP                 TO OUTFIL-IO-AREA (249:1)
               MOVE ';'                    TO OUTFIL-IO-AREA (250:1)
               IF  (I-75)
                   MOVE 'RETUR AV VARER       ' TO OUTFIL-IO-AREA
                                                              (251:21)
               END-IF
               IF  (I-76)
                   MOVE 'FEIL RABATT/BELØP    ' TO OUTFIL-IO-AREA
                                                              (251:21)
               END-IF
               IF  (I-77)
                   MOVE 'BONUS                ' TO OUTFIL-IO-AREA
                                                              (251:21)
               END-IF
               IF  (I-78)
                   MOVE 'ERSTATNING           ' TO OUTFIL-IO-AREA
                                                              (251:21)
               END-IF
               MOVE ';'                    TO OUTFIL-IO-AREA (272:1)
               MOVE '6'                    TO OUTFIL-IO-AREA (274:1)
               IF  (I-21)
                   MOVE '4'                TO OUTFIL-IO-AREA (274:1)
               END-IF
               IF  (I-11)
                   MOVE '0'                TO OUTFIL-IO-AREA (274:1)
               END-IF
               MOVE ';'                    TO OUTFIL-IO-AREA (275:1)
               IF  (I-19)
                   MOVE ORDAG              TO OUTFIL-IO-AREA (276:2)
               END-IF
               IF  (I-19)
                   MOVE ORMND              TO OUTFIL-IO-AREA (279:2)
               END-IF
               IF  (I-19)
                   MOVE ORAAR              TO OUTFIL-IO-AREA (282:4)
               END-IF
               IF  (NOT-I-19)
                   MOVE FMDAG              TO OUTFIL-IO-AREA (276:2)
               END-IF
               MOVE '/'                    TO OUTFIL-IO-AREA (278:1)
               IF  (NOT-I-19)
                   MOVE FMMM               TO OUTFIL-IO-AREA (279:2)
               END-IF
               MOVE '/'                    TO OUTFIL-IO-AREA (281:1)
               MOVE '20'                   TO OUTFIL-IO-AREA (282:2)
               IF  (NOT-I-19)
                   MOVE FMAA               TO OUTFIL-IO-AREA (284:2)
               END-IF
               IF  (I-11)
                   MOVE '          '       TO OUTFIL-IO-AREA (276:10)
               END-IF
               MOVE ';'                    TO OUTFIL-IO-AREA (286:1)
               WRITE OUTFIL-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND NOT-I-50 AND I-19)
               MOVE SPACES TO OUTFIL-IO-AREA
               INITIALIZE OUTFIL-IO-AREA
               MOVE FAKTNR                 TO OUTFIL-IO-AREA (1:6)
               MOVE ';'                    TO OUTFIL-IO-AREA (7:1)
               MOVE ORDNR                  TO OUTFIL-IO-AREA (8:6)
               MOVE ';'                    TO OUTFIL-IO-AREA (14:1)
               MOVE ORDAG                  TO OUTFIL-IO-AREA (15:2)
               MOVE '/'                    TO OUTFIL-IO-AREA (17:1)
               MOVE ORMND                  TO OUTFIL-IO-AREA (18:2)
               MOVE '/'                    TO OUTFIL-IO-AREA (20:1)
               MOVE ORAAR                  TO OUTFIL-IO-AREA (21:4)
               MOVE ';'                    TO OUTFIL-IO-AREA (25:1)
               MOVE ORDMOT                 TO OUTFIL-IO-AREA (26:2)
               MOVE ';'                    TO OUTFIL-IO-AREA (28:1)
               MOVE KUNDNR                 TO OUTFIL-IO-AREA (29:6)
               MOVE ';'                    TO OUTFIL-IO-AREA (35:1)
               MOVE NAVN                   TO OUTFIL-IO-AREA (36:30)
               MOVE ';'                    TO OUTFIL-IO-AREA (66:1)
               MOVE HND-IO                 TO OUTFIL-IO-AREA (67:3)
               MOVE ';'                    TO OUTFIL-IO-AREA (70:1)
               MOVE 'OAVR'                 TO OUTFIL-IO-AREA (71:4)
               MOVE ';'                    TO OUTFIL-IO-AREA (75:1)
               MOVE ';'                    TO OUTFIL-IO-AREA (87:1)
      *                        UAMOMS    98 "0      ,  "
      *                      12          88 "-"
               MOVE ';'                    TO OUTFIL-IO-AREA (99:1)
               MOVE FIRMA                  TO OUTFIL-IO-AREA (100:3)
               MOVE ';'                    TO OUTFIL-IO-AREA (103:1)
               MOVE ';'                    TO OUTFIL-IO-AREA (107:1)
               MOVE ';'                    TO OUTFIL-IO-AREA (128:1)
               MOVE ';'                    TO OUTFIL-IO-AREA (134:1)
               MOVE ';'                    TO OUTFIL-IO-AREA (146:1)
               MOVE ';'                    TO OUTFIL-IO-AREA (157:1)
               MOVE ';'                    TO OUTFIL-IO-AREA (164:1)
               MOVE ';'                    TO OUTFIL-IO-AREA (165:1)
               MOVE ';'                    TO OUTFIL-IO-AREA (168:1)
               MOVE ';'                    TO OUTFIL-IO-AREA (204:1)
               MOVE ';'                    TO OUTFIL-IO-AREA (213:1)
               MOVE MEKOID                 TO OUTFIL-IO-AREA (214:4)
               MOVE ';'                    TO OUTFIL-IO-AREA (218:1)
               MOVE AVRL1                  TO EDIT-AVRL1
               MOVE EDIT-AVRL1             TO OUTFIL-IO-AREA (221:10)
               IF  (I-94)
                   MOVE '-'                TO OUTFIL-IO-AREA (219:1)
      *                      18         219 " "
               END-IF
               MOVE ';'                    TO OUTFIL-IO-AREA (232:1)
               MOVE FAKMND                 TO OUTFIL-IO-AREA (233:6)
               IF  (I-80)
                   MOVE '08'               TO OUTFIL-IO-AREA (239:2)
               END-IF
               IF  (I-81)
                   MOVE '15'               TO OUTFIL-IO-AREA (239:2)
               END-IF
               IF  (I-82)
                   MOVE '23'               TO OUTFIL-IO-AREA (239:2)
               END-IF
               IF  (I-84 AND I-83)
                   MOVE '31'               TO OUTFIL-IO-AREA (239:2)
               END-IF
               IF  (I-85 AND I-83)
                   MOVE '28'               TO OUTFIL-IO-AREA (239:2)
               END-IF
               IF  (I-86 AND I-83)
                   MOVE '30'               TO OUTFIL-IO-AREA (239:2)
               END-IF
               MOVE ';'                    TO OUTFIL-IO-AREA (241:1)
               MOVE BM                     TO OUTFIL-IO-AREA (242:2)
               MOVE ';'                    TO OUTFIL-IO-AREA (244:1)
               MOVE KKAT-IO                TO OUTFIL-IO-AREA (245:3)
               MOVE ';'                    TO OUTFIL-IO-AREA (248:1)
               MOVE KRETYP                 TO OUTFIL-IO-AREA (249:1)
               MOVE ';'                    TO OUTFIL-IO-AREA (250:1)
               IF  (I-75)
                   MOVE 'RETUR AV VARER       ' TO OUTFIL-IO-AREA
                                                              (251:21)
               END-IF
               IF  (I-76)
                   MOVE 'FEIL RABATT/BELØP    ' TO OUTFIL-IO-AREA
                                                              (251:21)
               END-IF
               IF  (I-77)
                   MOVE 'BONUS                ' TO OUTFIL-IO-AREA
                                                              (251:21)
               END-IF
               IF  (I-78)
                   MOVE 'ERSTATNING           ' TO OUTFIL-IO-AREA
                                                              (251:21)
               END-IF
               MOVE ';'                    TO OUTFIL-IO-AREA (272:1)
               WRITE OUTFIL-IO-AREA
           END-IF
           IF  (I-L2 AND NOT-I-50)
               MOVE SPACES TO OUTFIL-IO-AREA
               INITIALIZE OUTFIL-IO-AREA
               MOVE FAKTNR                 TO OUTFIL-IO-AREA (1:6)
               MOVE ';'                    TO OUTFIL-IO-AREA (7:1)
               MOVE ORDNR                  TO OUTFIL-IO-AREA (8:6)
               MOVE ';'                    TO OUTFIL-IO-AREA (14:1)
               MOVE ORDAG                  TO OUTFIL-IO-AREA (15:2)
               MOVE '/'                    TO OUTFIL-IO-AREA (17:1)
               MOVE ORMND                  TO OUTFIL-IO-AREA (18:2)
               MOVE '/'                    TO OUTFIL-IO-AREA (20:1)
               MOVE ORAAR                  TO OUTFIL-IO-AREA (21:4)
               MOVE ';'                    TO OUTFIL-IO-AREA (25:1)
               MOVE ORDMOT                 TO OUTFIL-IO-AREA (26:2)
               MOVE ';'                    TO OUTFIL-IO-AREA (28:1)
               MOVE KUNDNR                 TO OUTFIL-IO-AREA (29:6)
               MOVE ';'                    TO OUTFIL-IO-AREA (35:1)
               MOVE NAVN                   TO OUTFIL-IO-AREA (36:30)
               MOVE ';'                    TO OUTFIL-IO-AREA (66:1)
               MOVE HND-IO                 TO OUTFIL-IO-AREA (67:3)
               MOVE ';'                    TO OUTFIL-IO-AREA (70:1)
               MOVE 'MOMS'                 TO OUTFIL-IO-AREA (71:4)
               MOVE ';'                    TO OUTFIL-IO-AREA (75:1)
               MOVE ';'                    TO OUTFIL-IO-AREA (87:1)
      *                        MVA       98 "0      ,  "
               MOVE MOMS                   TO EDIT-MOMS
               MOVE EDIT-MOMS              TO OUTFIL-IO-AREA (89:10)
               IF  (I-12)
                   MOVE '-'                TO OUTFIL-IO-AREA (88:1)
               END-IF
               IF  (I-22)
                   MOVE ' '                TO OUTFIL-IO-AREA (88:1)
               END-IF
               IF  (I-15)
                   MOVE '-'                TO OUTFIL-IO-AREA (88:1)
               END-IF
               IF  (I-19)
                   MOVE ' '                TO OUTFIL-IO-AREA (88:1)
               END-IF
               IF  (I-96 AND I-19)
                   MOVE '-'                TO OUTFIL-IO-AREA (88:1)
               END-IF
               MOVE ';'                    TO OUTFIL-IO-AREA (99:1)
               MOVE FIRMA                  TO OUTFIL-IO-AREA (100:3)
               MOVE ';'                    TO OUTFIL-IO-AREA (103:1)
               MOVE ';'                    TO OUTFIL-IO-AREA (107:1)
               MOVE ';'                    TO OUTFIL-IO-AREA (128:1)
               MOVE ';'                    TO OUTFIL-IO-AREA (134:1)
               MOVE ';'                    TO OUTFIL-IO-AREA (146:1)
               MOVE ';'                    TO OUTFIL-IO-AREA (157:1)
               MOVE ';'                    TO OUTFIL-IO-AREA (164:1)
               MOVE ';'                    TO OUTFIL-IO-AREA (166:1)
               MOVE ';'                    TO OUTFIL-IO-AREA (168:1)
               MOVE ';'                    TO OUTFIL-IO-AREA (204:1)
               MOVE ';'                    TO OUTFIL-IO-AREA (213:1)
               MOVE MEKOID                 TO OUTFIL-IO-AREA (214:4)
               MOVE ';'                    TO OUTFIL-IO-AREA (218:1)
               IF  (NOT-I-19)
                   MOVE AVRORE             TO EDIT-AVRORE
                   MOVE EDIT-AVRORE        TO OUTFIL-IO-AREA (221:10)
               END-IF
               IF  (I-93)
                   MOVE '-'                TO OUTFIL-IO-AREA (219:1)
      *                      19AVRL2    230 "0      ,  "
      *                      95         219 " "
               END-IF
               MOVE ';'                    TO OUTFIL-IO-AREA (232:1)
               MOVE FAKMND                 TO OUTFIL-IO-AREA (233:6)
               IF  (I-80)
                   MOVE '08'               TO OUTFIL-IO-AREA (239:2)
               END-IF
               IF  (I-81)
                   MOVE '15'               TO OUTFIL-IO-AREA (239:2)
               END-IF
               IF  (I-82)
                   MOVE '23'               TO OUTFIL-IO-AREA (239:2)
               END-IF
               IF  (I-84 AND I-83)
                   MOVE '31'               TO OUTFIL-IO-AREA (239:2)
               END-IF
               IF  (I-85 AND I-83)
                   MOVE '28'               TO OUTFIL-IO-AREA (239:2)
               END-IF
               IF  (I-86 AND I-83)
                   MOVE '30'               TO OUTFIL-IO-AREA (239:2)
               END-IF
               MOVE ';'                    TO OUTFIL-IO-AREA (241:1)
               MOVE BM                     TO OUTFIL-IO-AREA (242:2)
               MOVE ';'                    TO OUTFIL-IO-AREA (244:1)
               MOVE KKAT-IO                TO OUTFIL-IO-AREA (245:3)
               MOVE ';'                    TO OUTFIL-IO-AREA (248:1)
               MOVE KRETYP                 TO OUTFIL-IO-AREA (249:1)
               MOVE ';'                    TO OUTFIL-IO-AREA (250:1)
               IF  (I-75)
                   MOVE 'RETUR AV VARER       ' TO OUTFIL-IO-AREA
                                                              (251:21)
               END-IF
               IF  (I-76)
                   MOVE 'FEIL RABATT/BELØP    ' TO OUTFIL-IO-AREA
                                                              (251:21)
               END-IF
               IF  (I-77)
                   MOVE 'BONUS                ' TO OUTFIL-IO-AREA
                                                              (251:21)
               END-IF
               IF  (I-78)
                   MOVE 'ERSTATNING           ' TO OUTFIL-IO-AREA
                                                              (251:21)
               END-IF
               MOVE ';'                    TO OUTFIL-IO-AREA (272:1)
               MOVE '6'                    TO OUTFIL-IO-AREA (274:1)
               IF  (I-21)
                   MOVE '4'                TO OUTFIL-IO-AREA (274:1)
               END-IF
               IF  (I-11)
                   MOVE '0'                TO OUTFIL-IO-AREA (274:1)
               END-IF
               MOVE ';'                    TO OUTFIL-IO-AREA (275:1)
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
           INITIALIZE ORDHIST-DATA-FIELDS
           OPEN INPUT ORDHIST
           INITIALIZE ORDNRM-DATA-FIELDS
           OPEN INPUT ORDNRM
           OPEN OUTPUT OUTFIL.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INNFIL
           CLOSE KONTOMA
           CLOSE KONTOKY
           CLOSE ORDREM
           CLOSE ORDHIST
           CLOSE ORDNRM
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
