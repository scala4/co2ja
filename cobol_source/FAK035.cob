       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK035R.
      ******************************************* :   Z-WIN-RPG2     **
      * PROGRAM FAK035
      * R E T T E L S E R.                                              *
      * 31/10-91 KREDITNOTA KONTANTSALG BLIR SAMLET PÅ SAMME MÅTE SOM   *
      *          FAKTURA KONTANTSALG. REF.NR. BLIR OGSÅ TILDELT PÅ SAMME*
      *          MÅTE.  REF. ESPEN                                      *
      * 19/11-91 1. KREDITNOTA PR. FIRMA MÅ ALLTID DANNE NYTT BRUDD.
      *          DETTE FOR Å FANGE OPP FIRMA SOM KUN HAR KR.NOTA PÅ
      *          KUNDE NR. 500190 (KONTANT).
      *  6/12-91 SKREVET OM RUTINEN FOR Å DANNE FAKTURA REF.NR.
      *          DA S & B IKKE VIL HA ENDRET REF.NR PÅ SINE KR.NOTA"
      *          KONTANT.
      * 23/03-93 LEGGER INN BONUSKODE FRA KUNDEARKIVET FOR HAFNOR.
      * 22/04-94 DATOTEST I BONUSRUTINE FOR HAFNOR.
      *  2/08-95 LEGGE UT RECORDS TIL BONUS KR.NOTA(FIRMA 918)
      * 13/10-95 FRAKT BMW-FORH. FAKTURERES BMW-NORGE.(FIRMA 918)
      *  5/01-96 BONUSRUTINE FOR TEAM GED.  OVERFØRT FRA FIRMA 941
      *          BONUSRUTINE FOR PER HAGEN FJERNET.  (910)
      * 16/01-96 TEST PÅ ALFA/VGR SOM GIR BONUS FOR FIRMA 942.
      *          SAMT LAGT INN BONUSGRUPPE Y = ESSO OG V = STATOIL
      *  3/02-96 BONUS-RUTINE TEAM-GED ER FJERNET.
      *          BONUS BLIR TILDELT I ORDRE-SYSTEMET PROG(ORDBONUS)
      * 12/07-96 LAGET RUTINE FOR MERKING AV ORDRE SOM SKAL HA
      *          HASTERGEBYR. (O.SØRENSEN PÅ SHELL-KJEDEKUNDER)
      * 08/11-96 BONUSKODE W PÅ HAFNOR=12 % (SAMME REGEL SOM T.
      * 14/02-97 BONUSKODE LAGT INN FOR FIRMA 604 (BV ANKER)
      * 24/02-97 HASTERGEBYR O.SØRENSEN UTGÅR.
      * 15/05-97 ALLTID SAMLEFAKT VED FAKTURAMÅTE 5 ELLER 6.
      * 10/10-97 5 % SENTRALBONUS PÅ NKL FOR SCANGROSS. 923
      * 10/11-97 5 % SENTRALBONUS PÅ NKL FOR SCANGROSS. 923 FJERNET.
      *  2/07-98 NY BONUSRUTINE FOR FIRMA  918. TAR MED FORD.
      * 21/01-00 KUN BONUS PÅ DEFA FOR FIRMA 918.
      * 04.02.00 KONSERNMODELL FOR HENTEING AV KUNDE.MASTER DATA.     *
      * 05.09.00 INNLEGGING AV BONUSGRUPPE A (SUBARU OG ROVER)        *
      * 04.10.00 FORD FJERNET SOM BONUSKUNDE FIRMA 918.               *
      * 04.10.00 BRUKER DERFOR IKKE KUNDEMX.                          *
      * 06.10.00 NYE BONUSGRUPPE 918. RADIO OG NAVIGASJON             *
      * 11.10.00 LAGT OM BUNUSRUTINEN. SØKER NÅ I BBONTAB.            *
      *               DETTE ER FAK.FAKBONUS                           *
      * 18.07.01 FAKTURAMÅTE 7 OG 8 MERKER RECORD MED R I POS 38.     *
      *          = SAMLEFAKTURA PR. KUNDEREF.                         *
      * 19.07.01 FAKTURAMÅTE 8 BLIR MERKET MED FAKTURAMÅTE "M" I      *
      *            FAKTURAREC. FOR OG SKILLE DISSE FRA ØVRIG SAMLEFAK.*
      *  4.10.01 FAKTURAMÅTE 9 BLIR MERKET MED FAKTURAMÅTE "M" I      *
      *            FAKTURAREC. FOR OG SKILLE DISSE FRA ØVRIG SAMLEFAK.*
      * 14.02.02 KONTANTRABATT RODIN. RECORD BLIR MERKET MED BONUSK   *
      *            UTREGNING I FAK075.                                *
      *  7.08.02 BETALINGSMÅTE 22 OG 47 TILDELES ORDREFAKTURA MÅTE.   *
      *          DISSE HAR FÅTT TILDELT ORDRENR. SOM REF.NR. FRA FØR. *
      *  6/12-02 BONUSKODE LAGT INN FOR FIRMA 942 (TEAM GED)
      * 18/04-03 RENTE/GEBYRORDRE BLIR SKAL IKKE PÅ SAMLEFAKTURA.
      * 02/10-03 ØKT BONUSTAB FRA 200 TIL 300.
      * 02/10-03 BONUS SØKER NÅ OGSÅ PÅ AVD. (VGR.4XXXX)
      *  1/12-03 SAMLEFAKTURA PR. ORDREKUNDENR (OM DET ER FAKTURAKUNDENR
      *  6/01-05 HAFNOR (950) ØNSKER OGSÅ Å BRUKE FAKTURABONUSRUTINEN.
      * 20/02-08 SAMLEFAKTURA RECORDS BLIR SELEKTERT HER.
      *          DET LIGGER OGSÅ SAMLEFAKTURARECORDS PÅ ORDREFILEN TIL
      *          ØNSKET FAKTURAPERIODE (HALVMÅNED / MÅNED).
      *          SAMLEFAKTURAKODE KAN BLI OVERFØRT FRA FAKTURAKUNDENR.
      * Firma 959 skal ha samlefakt. pr rekv.nr på kreditnota 201114  *
      *******************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK035.rpg
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
           SELECT FAKIN
               ASSIGN TO UT-S-FAKIN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKIN-STATUS.
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
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT SAMLEF
               ASSIGN TO UT-S-SAMLEF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS SAMLEF-STATUS.
           SELECT FAKOUT
               ASSIGN TO UT-S-FAKOUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKOUT-STATUS.
           SELECT FAKBON
               ASSIGN TO UT-S-FAKBON
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKBON-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD BBONTAB
               BLOCK CONTAINS 80
               RECORD CONTAINS 40.
       01  BBONTAB-IO-AREA.
           05  BBONTAB-IO-AREA-X           PICTURE X(40).
       FD FAKIN
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  FAKIN-IO-AREA.
           05  FAKIN-IO-AREA-X             PICTURE X(200).
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
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD SAMLEF
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  SAMLEF-IO-AREA.
           05  SAMLEF-IO-AREA-X            PICTURE X(200).
       FD FAKOUT
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  FAKOUT-IO-AREA.
           05  FAKOUT-IO-AREA-X            PICTURE X(200).
       FD FAKBON
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  FAKBON-IO-AREA.
           05  FAKBON-IO-AREA-X            PICTURE X(200).
       WORKING-STORAGE SECTION.
       77  TABFKV-MAX   VALUE 600          PICTURE 9(4) USAGE BINARY.
       77  TABPKN-MAX   VALUE 600          PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  TABFKV-TABLE.
               10  TABFKV-ENTRY
                                           OCCURS 600 TIMES
                                           INDEXED BY TABFKV-I
                                                      TABFKV-S
                                                      TABPKN-I
                                                      TABPKN-S.
                   15  TABFKV              PICTURE X(14).
                   15  TABPKN              PICTURE X(10).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  BBONTAB-STATUS              PICTURE 99 VALUE 0.
           10  FAKIN-STATUS                PICTURE 99 VALUE 0.
           10  FAKPAR-STATUS               PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  SAMLEF-STATUS               PICTURE 99 VALUE 0.
           10  FAKOUT-STATUS               PICTURE 99 VALUE 0.
           10  FAKBON-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  BBONTAB-EOF-OFF         VALUE '0'.
               88  BBONTAB-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKIN-EOF-OFF           VALUE '0'.
               88  FAKIN-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKIN-READ-OFF          VALUE '0'.
               88  FAKIN-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKIN-PROCESS-OFF       VALUE '0'.
               88  FAKIN-PROCESS           VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  FAKIN-LEVEL-INIT-OFF    VALUE '0'.
               88  FAKIN-LEVEL-INIT        VALUE '1'.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  FAKIN-LEVEL-02.
               10  FAKIN-02-L3.
                   15  FAKIN-02-L3-FIRMNR  PICTURE X(3).
               10  FAKIN-02-L2.
                   15  FAKIN-02-L2-RESKNR  PICTURE X(6).
               10  FAKIN-02-L1.
                   15  FAKIN-02-L1-ONR     PICTURE X(6).
           05  FAKIN-DATA-FIELDS.
               10  FIRMNR                  PICTURE X(3).
               10  RESKNR                  PICTURE X(6).
               10  RNR3F                   PICTURE X(3).
               10  FAKTYP                  PICTURE X(1).
      *                                      11  11 FAKART
               10  FAKMT                   PICTURE X(1).
               10  BETBET                  PICTURE X(2).
               10  ONR                     PICTURE X(6).
               10  RECART                  PICTURE X(1).
               10  FAKREF                  PICTURE X(6).
               10  KREF4F                  PICTURE X(4).
               10  HND                     PICTURE X(3).
               10  VGRAVD                  PICTURE X(1).
               10  ODAG                    PICTURE X(2).
               10  OMND                    PICTURE X(2).
               10  OAAR                    PICTURE X(2).
               10  VGR                     PICTURE X(5).
               10  EDBNR                   PICTURE X(7).
               10  SPREC                   PICTURE X(1).
               10  ORDKNR                  PICTURE X(6).
               10  FAKREC                  PICTURE X(200).
           05  FAKPAR-DATA-FIELDS.
               10  FAKPER                  PICTURE X(6).
               10  FUKPER                  PICTURE X(6).
           05  KUNDEMA-DATA-FIELDS.
               10  RESGRP                  PICTURE X(2).
               10  BONUS                   PICTURE X(1).
               10  CHND                    PICTURE X(3).
               10  CFAKTM                  PICTURE X(1).
      *                                     170 170 KOD3X
           05  FIRMAF-DATA-FIELDS.
               10  KONFNR                  PICTURE X(3).
               10  FFAKTY                  PICTURE X(1).
               10  FFKODE                  PICTURE X(1).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(6).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  NYFREF                  PICTURE X(6).
               10  RESKEY                  PICTURE X(9).
               10  PHND                    PICTURE X(3).
               10  FAKMTX                  PICTURE X(1).
               10  FIRKNR                  PICTURE X(9).
               10  FIKNVG                  PICTURE X(14).
               10  RNR3X                   PICTURE X(6).
               10  BBOPRO                  PICTURE X(4).
               10  BBOKNR                  PICTURE X(6).
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
           SET NOT-I-02                    TO TRUE
           SET NOT-I-01                    TO TRUE
           SET NOT-I-04                    TO TRUE
           SET NOT-I-05                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  FAKIN-PROCESS
               SET FAKIN-PROCESS-OFF       TO TRUE
               SET FAKIN-READ              TO TRUE
           END-IF
 
           IF  FAKIN-READ
           AND RECORD-SELECTED-OFF
               PERFORM FAKIN-GET
               SET FAKIN-READ-OFF          TO TRUE
               IF  NOT FAKIN-EOF
                   SET FAKIN-PROCESS       TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  FAKIN-PROCESS
               PERFORM FAKIN-IDSET
           END-IF
 
           IF  FAKIN-PROCESS
               PERFORM FAKIN-CHK-LEVEL
           END-IF
 
           IF I-1ST
               GO TO LR-INDICATOR-TEST
           END-IF.
 
       MAINLINE-TOTAL-CALCS.
           CONTINUE.
 
       LR-INDICATOR-TEST.
           IF  I-LR
               GO TO MAINLINE-TERMINATION
           END-IF
 
           IF  FAKIN-PROCESS
               PERFORM FAKIN-FLDOFF
               PERFORM FAKIN-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  FAKIN-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-47                    TO TRUE
           SET NOT-I-49                    TO TRUE
           IF  (I-L1)
               SET NOT-I-10                TO TRUE
               SET NOT-I-11                TO TRUE
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
           IF  (I-L1)
               SET NOT-I-24                TO TRUE
               IF  FAKTYP = 'K'
                   SET I-24                TO TRUE
               END-IF
           END-IF
           SET NOT-I-12                    TO TRUE
           IF  RECART = 'A'
               SET I-12                    TO TRUE
           END-IF
      *****************************************************************
      *    FIRMA SOM IKKE SKAL HA SAMLEFAKT PÅ KREDITNOTA KONTANT.    *
      *****************************************************************
           IF  (I-L3)
               SET NOT-I-27                TO TRUE
               IF  FIRMNR = '918'
                   SET I-27                TO TRUE
               END-IF
               SET NOT-I-63                TO TRUE
               IF  FIRMNR = '959'
                   SET I-63                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-63)
               SET NOT-I-63                TO TRUE
               IF  FIRMNR = '529'
                   SET I-63                TO TRUE
               END-IF
      *****************************************************************
      * HOVEDRUTINE OPPSLAG MOT FIRMAFILE.                            *
      *****************************************************************
           END-IF
           IF  (I-L3)
               MOVE FIRMNR                 TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-50                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-50            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
               SET NOT-I-51                TO TRUE
               IF  FFAKTY = '1'
                   SET I-51                TO TRUE
               END-IF
               SET NOT-I-55                TO TRUE
               IF  FFKODE = 'U'
                   SET I-55                TO TRUE
               END-IF
      *
           END-IF
           IF  (I-L1)
               SET NOT-I-19                TO TRUE
               SET NOT-I-18                TO TRUE
               IF  RESKNR > '500180'
                   SET I-19                TO TRUE
               END-IF
               IF  RESKNR = '500180'
                   SET I-18                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-19)
               SET NOT-I-18                TO TRUE
               IF  RESKNR NOT > '500500'
                   SET I-18                TO TRUE
               END-IF
           END-IF
           IF  (I-L1)
               SET NOT-I-15                TO TRUE
               IF  RESKNR = '500500'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-18 AND NOT-I-24)
               SET NOT-I-30                TO TRUE
               IF  BETBET = '07'
                   SET I-30                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-30 AND NOT-I-24)
               SET NOT-I-30                TO TRUE
               IF  BETBET = '14'
                   SET I-30                TO TRUE
               END-IF
      *****************************************************************
      *  RUTINE FOR Å ENDRE FAKTURA REF.NR.                           *
      *  KUNDENR. 500180-500500 TILDELES FAKTURAPERIODE.              *
      *  BETALINGSMÅTE 07 OG 14 (IKKE KUNDENR. 500180-500500)         *
      *        TILDELES ORDRENUMMER.                                  *
      *   S & B SKAL IKKE ENDRE FAKT.REFNR PÅ KR.NOTA.                *
      *****************************************************************
           END-IF
           IF  (I-12)
               GO TO ENDREF-T
           END-IF
           MOVE FAKREF                     TO NYFREF
           IF  (I-18)
               MOVE FAKPER                 TO NYFREF
           END-IF
           IF  (I-18 AND I-55)
               MOVE FUKPER                 TO NYFREF
           END-IF
           IF  (I-30)
               MOVE ONR                    TO NYFREF
           END-IF
           IF  (I-27 AND I-24)
               MOVE FAKREF                 TO NYFREF
           END-IF
           IF  (I-27 AND I-15)
               MOVE FAKREF                 TO NYFREF
           END-IF.
 
       ENDREF-T.
      *****************************************************************
      *  RUTINE FOR Å HENTE KUNDEOPPLYSNINGER.                        *
      *****************************************************************
           IF  (I-L2)
               MOVE RESKNR                 TO RESKEY (4:6)
               MOVE FIRMNR                 TO RESKEY (1:3)
               SET NOT-I-90                TO TRUE
               IF  KONFNR > '000'
                   SET I-90                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-90)
               MOVE KONFNR                 TO RESKEY (1:3)
           END-IF
           IF  (I-L2)
               MOVE RESKEY                 TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-90                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-90            TO TRUE
                   PERFORM KUNDEMA-FLDSET
                   PERFORM KUNDEMA-IDSET
               END-READ
      *****************************************************************
      *  RUTINE FOR Å SETTE INN HAND.DIST OM DETTE MANGLER            *
      *****************************************************************
           END-IF
           IF  (I-12)
               GO TO ENDHND-T
           END-IF
           MOVE HND                        TO PHND
           IF  (I-06 AND NOT-I-90)
               MOVE CHND                   TO PHND
           END-IF.
 
       ENDHND-T.
      *****************************************************************
      * RUTINE FOR Å SETTE INN KODE FOR FAKTURAMÅTE.                  *
      *             O = ORDREFAKT, S = SAMLEFAKT.                     *
      * ORDRE MED VAREADRESSE TILDELES ORDREFAKT.                     *
      * KONTANT KUNDENR. TILDELES SAMLEFAKT.                          *
      * KUNDER MED INNMELDT ORDREFAKT TILDELS ORDREFAKT.              *
      * ALLE ØVRIGE TILDELES SAMLEFAKT.                               *
      * Firma 959 skal ha samlefakt. pr rekv.nr på kreditnota 201114  *
      *****************************************************************
           IF  (NOT-I-L1)
               GO TO ENDFM-T
           END-IF
           MOVE FAKMT                      TO FAKMTX
           MOVE 'S'                        TO FAKMTX
           IF  (I-12)
               MOVE 'O'                    TO FAKMTX
           END-IF
           IF  (I-24)
               MOVE 'O'                    TO FAKMTX
           END-IF
           IF  (I-18 AND NOT-I-24)
               MOVE 'S'                    TO FAKMTX
           END-IF
           IF  (I-18 AND I-24 AND NOT-I-27)
               MOVE 'S'                    TO FAKMTX
           END-IF
           IF  (I-63 AND I-24)
               MOVE 'S'                    TO FAKMTX
           END-IF
           SET NOT-I-31                    TO TRUE
           IF  CFAKTM = '2'
               SET I-31                    TO TRUE
           END-IF
           IF  (I-31 AND NOT-I-90)
               MOVE 'O'                    TO FAKMTX
           END-IF
           SET NOT-I-31                    TO TRUE
           IF  CFAKTM = '5'
               SET I-31                    TO TRUE
           END-IF
           IF  (NOT-I-31)
               SET NOT-I-31                TO TRUE
               IF  CFAKTM = '6'
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-31)
               SET NOT-I-31                TO TRUE
               IF  CFAKTM = '7'
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-31)
               SET NOT-I-31                TO TRUE
               IF  CFAKTM = 'A'
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-31)
               SET NOT-I-31                TO TRUE
               IF  CFAKTM = 'B'
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (I-31 AND NOT-I-90)
               MOVE 'S'                    TO FAKMTX
           END-IF
           SET NOT-I-33                    TO TRUE
           IF  CFAKTM = '8'
               SET I-33                    TO TRUE
           END-IF
           IF  (NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  CFAKTM = '9'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  CFAKTM = 'C'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-33 AND NOT-I-90)
               MOVE 'M'                    TO FAKMTX
           END-IF
           SET NOT-I-31                    TO TRUE
           IF  FAKMT = '*'
               SET I-31                    TO TRUE
           END-IF
           IF  (I-31)
               MOVE 'O'                    TO FAKMTX
           END-IF
           SET NOT-I-31                    TO TRUE
           IF  SPREC = 'R'
               SET I-31                    TO TRUE
           END-IF
           IF  (I-31)
               MOVE 'O'                    TO FAKMTX
           END-IF.
 
       ENDFM-T.
           MOVE FAKMTX                     TO FAKMT
      *****************************************************************
      * RUTINE FOR Å SETTE INN KODE FOR SAMLEFAKTURA PR. KUNDEREF.    *
      * firma 959 skal ha samlefakt for kreditnota pr kunderef.       *
      *****************************************************************
           IF  (NOT-I-L1)
               GO TO ENDKR-T
           END-IF
           SET NOT-I-32                    TO TRUE
           IF  CFAKTM = '7'
               SET I-32                    TO TRUE
           END-IF
           IF  (NOT-I-32)
               SET NOT-I-32                TO TRUE
               IF  CFAKTM = '8'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-63 AND I-24)
               SET I-32                    TO TRUE
      *****************************************************************
      * RUTINE FOR Å SETTE INN KODE FOR SAMLEFAKTURA PR. ORDREKUNDENR *
      *****************************************************************
           END-IF
           SET NOT-I-34                    TO TRUE
           IF  CFAKTM = 'A'
               SET I-34                    TO TRUE
           END-IF
           IF  (NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  CFAKTM = 'B'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  CFAKTM = 'C'
                   SET I-34                TO TRUE
               END-IF
           END-IF.
 
       ENDKR-T.
      *****************************************************************
      * RUTINE FOR Å SE OM ORDEREN SKAL SAMLES TIL NESTE FAKTURERING. *
      * 1. ORDRE MERKET SOM ORDREFAKT. SKAL IKKE SAMLES.              *
      * 2. KREDITNOTA (UTENOM KUNDENR: 500180-500500) SKAL IKKE SAMLES*
      * -  FORØVERIG VIL ORDEREN ALTID LIGGE I ORDREFILE TIL DEN SKAL *
      *      FAKTURERES. MED UNTAK AV KONTANTSALG SOM BLIR TILDELT    *
      *      NYTT FAKTURAKUNDENUMMER.                                 *
      *****************************************************************
           IF  (NOT-I-L1)
               GO TO ENDSAM-T
           END-IF
           SET I-11                        TO TRUE
           SET NOT-I-10                    TO TRUE
           SET NOT-I-31                    TO TRUE
           IF  FAKMT = 'O'
               SET I-31                    TO TRUE
           END-IF
           IF  (I-31)
               GO TO ENDSAM-T
           END-IF
           IF  (I-24 AND NOT-I-18)
               GO TO ENDSAM-T
           END-IF
           SET NOT-I-10                    TO TRUE
           IF  CFAKTM = '1'
               SET I-10                    TO TRUE
           END-IF
           IF  (NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  CFAKTM = '6'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  CFAKTM = '7'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  CFAKTM = '8'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  CFAKTM = '9'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  CFAKTM = 'B'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  CFAKTM = 'C'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-10)
               SET NOT-I-11                TO TRUE
           END-IF.
 
       ENDSAM-T.
      *****************************************************************
      * BONUSRUTINE SOM SAMLER DATA TIL EN KVARTALSVIS KR.NOTA        *
      * ALLE RECORDS SOM VIL GI GRUNNLAG FOR BONUS LEGGES UT PÅ EN    *
      * EGEN FILE. "FAK035.BONUSREC."                                 *
      * 25.06.99 RENAULT/VOLVO BONUS FJERNET.                         *
      *****************************************************************
           SET NOT-I-60                    TO TRUE
           SET NOT-I-61                    TO TRUE
           SET NOT-I-62                    TO TRUE
           SET NOT-I-40                    TO TRUE
           IF  (I-12)
               GO TO ENDBON-T
      *****************************************************************
      * BACKBONUS RUTINE.   LAGET 11.10.2000                          *
      *   LEGGER INN KUNDENR FRA TABELL I FAKTURA BONUSRECORD.        *
      *   SØKER I TABELL MED FIRMANR,KUNDENR OG VAREGRUPPE.           *
      *   ALTERATIV SØK MED 3 FØRSTE SIFFER I KUNDENUMMER.            *
      *   20.10.2000 BRUKER ORDRE-KUNDENR.                            *
      *****************************************************************
           END-IF
           SET NOT-I-41                    TO TRUE
           IF  SPREC = 'R'
               SET I-41                    TO TRUE
           END-IF
           IF  (NOT-I-41)
               SET NOT-I-41                TO TRUE
               IF  SPREC = 'F'
                   SET I-41                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-41)
               SET NOT-I-41                TO TRUE
               IF  SPREC = 'B'
                   SET I-41                TO TRUE
               END-IF
           END-IF
           IF  (I-41)
               GO TO ENDBON-T
           END-IF
           SET NOT-I-41                    TO TRUE
           IF  FIRMNR = '918'
               SET I-41                    TO TRUE
           END-IF
           IF  (NOT-I-41)
               SET NOT-I-41                TO TRUE
               IF  FIRMNR = '950'
                   SET I-41                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-41)
               GO TO BON950-T
      *   SØKER I TABELL MED FIRMANR,KUNDENR OG VAREGRUPPE.           *
           END-IF
           MOVE FIRMNR                     TO FIRKNR (1:3)
           MOVE ORDKNR                     TO FIRKNR (4:6)
           MOVE FIRKNR                     TO FIKNVG (1:9)
           MOVE VGR                        TO FIKNVG (10:5)
           SET NOT-I-81                    TO TRUE
           SET TABFKV-S                    TO TABFKV-I
           PERFORM WITH TEST AFTER
                   VARYING TABFKV-I FROM 1 BY 1
                     UNTIL TABFKV-I >= TABFKV-MAX
                        OR I-81
               IF  FIKNVG = TABFKV (TABFKV-I)
                   SET I-81                TO TRUE
                   SET TABFKV-S            TO TABFKV-I
               END-IF
           END-PERFORM
           SET TABFKV-I                    TO TABFKV-S
           IF  I-81
           AND TABFKV-I NOT > TABPKN-MAX
               SET TABPKN-I                TO TABFKV-I
           END-IF
           IF  (I-81)
               GO TO BBORUT-T
      *   SØKER I TABELL MED FIRMANR,KUNDENR OG AVD.                  *
           END-IF
           MOVE FIRMNR                     TO FIRKNR (1:3)
           MOVE ORDKNR                     TO FIRKNR (4:6)
           MOVE FIRKNR                     TO FIKNVG (1:9)
           MOVE VGR                        TO FIKNVG (10:5)
           MOVE 'XXXX'                     TO FIKNVG (11:4)
           SET NOT-I-81                    TO TRUE
           SET TABFKV-S                    TO TABFKV-I
           PERFORM WITH TEST AFTER
                   VARYING TABFKV-I FROM 1 BY 1
                     UNTIL TABFKV-I >= TABFKV-MAX
                        OR I-81
               IF  FIKNVG = TABFKV (TABFKV-I)
                   SET I-81                TO TRUE
                   SET TABFKV-S            TO TABFKV-I
               END-IF
           END-PERFORM
           SET TABFKV-I                    TO TABFKV-S
           IF  I-81
           AND TABFKV-I NOT > TABPKN-MAX
               SET TABPKN-I                TO TABFKV-I
           END-IF
           IF  (I-81)
               GO TO BBORUT-T
      *   ALTERATIV SØK MED 3 FØRSTE SIFFER I KUNDENUMMER OG VGR.     *
           END-IF
           MOVE ORDKNR                     TO RNR3X
           MOVE 'XXX'                      TO RNR3X (4:3)
           MOVE RNR3X                      TO FIRKNR (4:6)
           MOVE FIRKNR                     TO FIKNVG (1:9)
           MOVE VGR                        TO FIKNVG (10:5)
           SET NOT-I-81                    TO TRUE
           SET TABFKV-S                    TO TABFKV-I
           PERFORM WITH TEST AFTER
                   VARYING TABFKV-I FROM 1 BY 1
                     UNTIL TABFKV-I >= TABFKV-MAX
                        OR I-81
               IF  FIKNVG = TABFKV (TABFKV-I)
                   SET I-81                TO TRUE
                   SET TABFKV-S            TO TABFKV-I
               END-IF
           END-PERFORM
           SET TABFKV-I                    TO TABFKV-S
           IF  I-81
           AND TABFKV-I NOT > TABPKN-MAX
               SET TABPKN-I                TO TABFKV-I
           END-IF
           IF  (I-81)
               GO TO BBORUT-T
      *   ALTERATIV SØK MED 3 FØRSTE SIFFER I KUNDENUMMER OG AVD.     *
           END-IF
           MOVE ORDKNR                     TO RNR3X
           MOVE 'XXX'                      TO RNR3X (4:3)
           MOVE RNR3X                      TO FIRKNR (4:6)
           MOVE FIRKNR                     TO FIKNVG (1:9)
           MOVE VGR                        TO FIKNVG (10:5)
           MOVE 'XXXX'                     TO FIKNVG (11:4)
           SET NOT-I-81                    TO TRUE
           SET TABFKV-S                    TO TABFKV-I
           PERFORM WITH TEST AFTER
                   VARYING TABFKV-I FROM 1 BY 1
                     UNTIL TABFKV-I >= TABFKV-MAX
                        OR I-81
               IF  FIKNVG = TABFKV (TABFKV-I)
                   SET I-81                TO TRUE
                   SET TABFKV-S            TO TABFKV-I
               END-IF
           END-PERFORM
           SET TABFKV-I                    TO TABFKV-S
           IF  I-81
           AND TABFKV-I NOT > TABPKN-MAX
               SET TABPKN-I                TO TABFKV-I
           END-IF
           IF  (I-81)
               GO TO BBORUT-T
           END-IF
           GO TO BON950-T
      *****************************************************************
           .
 
       BBORUT-T.
           MOVE TABPKN(TABPKN-I) (1:4)     TO BBOPRO
           MOVE TABPKN(TABPKN-I) (5:6)     TO BBOKNR
           SET I-60                        TO TRUE
           GO TO ENDBON-T
      *****************************************************************
      * FRAKTRUTINE SØRENSEN OG BALCHEN (FIRMA 918)                   *
      * FRAKT BMW-FORHANDLERE SKAL FAKTURERES BMW-NORGE.              *
      *       ALLE VITALE KUNDEDATA MÅ OVERFØRES FRA DEN NYE KUNDE.   *
      *****************************************************************
      *          FRAKTR    TAG
      *          BETBET    COMP "07"                     49 KONTANT
      * N49      BETBET    COMP "14"                     49 OPPKRAV
      *  49                GOTO ENDBON
      *                    SETON                     47      FRAKT.
      *  82                MOVE "103125"  FKUNDE  6         BMW-NORGE
      *  82N24             MOVE "D"       FFAKAR  1         BMW-NORGE
      *  82 24             MOVE "F"       FFAKAR            BMW-NORGE
      *  82                MOVE "S"       FFAKMT  1         BMW-NORGE
      *  82                MOVE "03"      FBETMT  2         BMW-NORGE
      *  82                MOVE "003"     FHND    3         BMW-NORGE
      *                    GOTO ENDBON
      *****************************************************************
      * BONUSRUTINE  HAFNOR.                                          *
      * ALLE RECORDS SOM VIL GI GRUNNLAG FOR BONUS MERKES.            *
      *****************************************************************
           .
 
       BON950-T.
           SET NOT-I-40                    TO TRUE
           SET NOT-I-41                    TO TRUE
           IF  FIRMNR = '950'
               SET I-41                    TO TRUE
           END-IF
           IF  (NOT-I-41)
               GO TO BON604-T
           END-IF
           SET NOT-I-42                    TO TRUE
           IF  BONUS = 'T'
               SET I-42                    TO TRUE
           END-IF
           IF  (NOT-I-42)
               SET NOT-I-42                TO TRUE
               IF  BONUS = 'W'
                   SET I-42                TO TRUE
               END-IF
           END-IF
           SET NOT-I-43                    TO TRUE
           IF  BONUS = 'U'
               SET I-43                    TO TRUE
           END-IF
           IF  (NOT-I-43)
               SET NOT-I-43                TO TRUE
               IF  BONUS = 'V'
                   SET I-43                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-43)
               SET NOT-I-43                TO TRUE
               IF  BONUS = 'Y'
                   SET I-43                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-42 AND NOT-I-43)
               GO TO ENDBON-T
           END-IF
           SET NOT-I-44                    TO TRUE
           IF  VGRAVD = '1'
               SET I-44                    TO TRUE
           END-IF
           IF  (NOT-I-44)
               SET NOT-I-44                TO TRUE
               IF  VGRAVD = '2'
                   SET I-44                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-44)
               SET NOT-I-44                TO TRUE
               IF  VGRAVD = '3'
                   SET I-44                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-44)
               SET NOT-I-44                TO TRUE
               IF  VGRAVD = '4'
                   SET I-44                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-44)
               SET NOT-I-44                TO TRUE
               IF  VGRAVD = '5'
                   SET I-44                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-44)
               SET NOT-I-44                TO TRUE
               IF  VGRAVD = '6'
                   SET I-44                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-44 AND I-43)
               SET NOT-I-44                TO TRUE
               IF  VGRAVD = '7'
                   SET I-44                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-44 AND I-43)
               SET NOT-I-44                TO TRUE
               IF  VGRAVD = '8'
                   SET I-44                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-44 AND I-43)
               SET NOT-I-44                TO TRUE
               IF  VGRAVD = '9'
                   SET I-44                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-44)
               GO TO ENDBON-T
           END-IF
           SET NOT-I-44                    TO TRUE
           IF  VGR = '19981'
               SET I-44                    TO TRUE
           END-IF
           IF  (NOT-I-44)
               SET NOT-I-44                TO TRUE
               IF  VGR = '19982'
                   SET I-44                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-44)
               SET NOT-I-44                TO TRUE
               IF  VGR = '19983'
                   SET I-44                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-44)
               SET NOT-I-44                TO TRUE
               IF  VGR = '19984'
                   SET I-44                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-44)
               SET NOT-I-44                TO TRUE
               IF  VGR = '19985'
                   SET I-44                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-44)
               SET NOT-I-44                TO TRUE
               IF  VGR = '19986'
                   SET I-44                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-44)
               SET NOT-I-44                TO TRUE
               IF  VGR = '19987'
                   SET I-44                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-44)
               SET NOT-I-44                TO TRUE
               IF  VGR = '19988'
                   SET I-44                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-44)
               SET NOT-I-44                TO TRUE
               IF  VGR = '19989'
                   SET I-44                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-44)
               SET NOT-I-44                TO TRUE
               IF  VGR = '19991'
                   SET I-44                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-44)
               SET NOT-I-44                TO TRUE
               IF  VGR = '19992'
                   SET I-44                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-44)
               SET NOT-I-44                TO TRUE
               IF  VGR = '19993'
                   SET I-44                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-44)
               SET NOT-I-44                TO TRUE
               IF  VGR = '19994'
                   SET I-44                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-44)
               SET NOT-I-44                TO TRUE
               IF  VGR = '19995'
                   SET I-44                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-44)
               SET NOT-I-44                TO TRUE
               IF  VGR = '19996'
                   SET I-44                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-44)
               SET NOT-I-44                TO TRUE
               IF  VGR = '19997'
                   SET I-44                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-44)
               SET NOT-I-44                TO TRUE
               IF  VGR = '19998'
                   SET I-44                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-44)
               SET NOT-I-44                TO TRUE
               IF  VGR = '19999'
                   SET I-44                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-44)
               SET NOT-I-44                TO TRUE
               IF  VGR = '55300'
                   SET I-44                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-44)
               SET NOT-I-44                TO TRUE
               IF  VGR = '55310'
                   SET I-44                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-44)
               SET NOT-I-44                TO TRUE
               IF  VGR = '55320'
                   SET I-44                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-44)
               SET NOT-I-44                TO TRUE
               IF  VGR = '55330'
                   SET I-44                TO TRUE
               END-IF
           END-IF
           IF  (I-44)
               GO TO ENDBON-T
           END-IF
           IF  (I-41 AND I-42)
               PERFORM BONDTO-S
           END-IF
           IF  (I-44)
               GO TO ENDBON-T
           END-IF
           SET I-40                        TO TRUE
           GO TO ENDBON-T
      *****************************************************************
           .
 
       BON604-T.
           SET NOT-I-40                    TO TRUE
           SET NOT-I-41                    TO TRUE
           IF  FIRMNR = '604'
               SET I-41                    TO TRUE
           END-IF
           IF  (NOT-I-41)
               GO TO BON915-T
           END-IF
           SET NOT-I-42                    TO TRUE
           IF  BONUS = 'T'
               SET I-42                    TO TRUE
           END-IF
           IF  (NOT-I-42)
               SET NOT-I-42                TO TRUE
               IF  BONUS = 'F'
                   SET I-42                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-42)
               SET NOT-I-42                TO TRUE
               IF  BONUS = 'C'
                   SET I-42                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-42)
               GO TO ENDBON-T
           END-IF
           SET NOT-I-44                    TO TRUE
           IF  VGR = '80200'
               SET I-44                    TO TRUE
           END-IF
           IF  (NOT-I-44)
               SET NOT-I-44                TO TRUE
               IF  VGR = '80500'
                   SET I-44                TO TRUE
               END-IF
           END-IF
           IF  (I-44)
               GO TO ENDBON-T
           END-IF
           SET I-40                        TO TRUE
           GO TO ENDBON-T
      *****************************************************************
           .
 
       BON915-T.
           SET NOT-I-40                    TO TRUE
           SET NOT-I-41                    TO TRUE
           IF  FIRMNR = '915'
               SET I-41                    TO TRUE
           END-IF
           IF  (NOT-I-41)
               GO TO ENDBON-T
           END-IF
           SET NOT-I-42                    TO TRUE
           IF  BONUS = 'A'
               SET I-42                    TO TRUE
           END-IF
           IF  (NOT-I-42)
               GO TO ENDBON-T
           END-IF
           SET NOT-I-44                    TO TRUE
           IF  VGR = '96999'
               SET I-44                    TO TRUE
           END-IF
           IF  (I-44)
               GO TO ENDBON-T
           END-IF
           SET I-40                        TO TRUE
           GO TO ENDBON-T
      *****************************************************************
           .
 
       ENDBON-T.
      *****************************************************************
      * HASTERGEBYR RUTINE                                            *
      * ALLE RECORDS SOM VIL GI HASTERGEBYR MERKES.                   *
      * UTGÅR 24/2-1997  TELFON FRA WETHAL.                           *
      *****************************************************************
           SET NOT-I-20                    TO TRUE
           SET NOT-I-21                    TO TRUE
           IF  FIRMNR = '923'
               SET I-21                    TO TRUE
           END-IF
           IF  (NOT-I-21)
               GO TO ENDHAG-T
           END-IF
           IF  (I-21)
               GO TO ENDHAG-T
           END-IF
           IF  (I-90)
               GO TO ENDHAG-T
           END-IF
           SET NOT-I-22                    TO TRUE
           IF  RESGRP = '99'
               SET I-22                    TO TRUE
           END-IF
           IF  (NOT-I-22)
               GO TO ENDHAG-T
           END-IF
           SET NOT-I-23                    TO TRUE
           IF  KREF4F = 'HAST'
               SET I-23                    TO TRUE
           END-IF
           IF  (I-23)
               SET I-20                    TO TRUE
           END-IF.
 
       ENDHAG-T.
      *****************************************************************
      * SUBRUTINE FOR VARER SOM NORMALT SKAL HA BONUS MEN SOM IKKE    *
      * SKAL HA BONUS I PERIODEN 20/4-94 TIL 31/8-94                  *
      * DETTE GJELDER HAFNOR MED BODUSKODE "T"                        *
      *****************************************************************
           CONTINUE.
 
       BONDTO-S SECTION.
       BONDTO-S-P.
           GO TO ENDDTO-T
           SET NOT-I-71                    TO TRUE
           IF  OAAR = '94'
               SET I-71                    TO TRUE
           END-IF
           IF  (NOT-I-71)
               GO TO ENDDTO-T
           END-IF
           SET NOT-I-71                    TO TRUE
           IF  OMND < '04'
               SET I-71                    TO TRUE
           END-IF
           IF  (I-71)
               GO TO ENDDTO-T
           END-IF
           SET NOT-I-71                    TO TRUE
           IF  OMND = '04'
               SET I-71                    TO TRUE
           END-IF
           IF  (I-71)
               SET NOT-I-72                TO TRUE
               IF  ODAG < '20'
                   SET I-72                TO TRUE
               END-IF
           END-IF
           IF  (I-71 AND I-72)
               GO TO ENDDTO-T
           END-IF
           SET NOT-I-71                    TO TRUE
           IF  OMND > '08'
               SET I-71                    TO TRUE
           END-IF
           IF  (I-71)
               GO TO ENDDTO-T
           END-IF
           SET NOT-I-44                    TO TRUE
           IF  EDBNR = '2729520'
               SET I-44                    TO TRUE
           END-IF
           IF  (NOT-I-44)
               SET NOT-I-44                TO TRUE
               IF  EDBNR = '2729539'
                   SET I-44                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-44)
               SET NOT-I-44                TO TRUE
               IF  EDBNR = '2730499'
                   SET I-44                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-44)
               SET NOT-I-44                TO TRUE
               IF  EDBNR = '2731665'
                   SET I-44                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-44)
               SET NOT-I-44                TO TRUE
               IF  EDBNR = '2729717'
                   SET I-44                TO TRUE
               END-IF
           END-IF.
 
       ENDDTO-T.
           CONTINUE.
      *****************************************************************
 
       FAKIN-GET SECTION.
       FAKIN-GET-P.
           IF  FAKIN-EOF-OFF
               READ FAKIN
               AT END
                   SET FAKIN-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FAKIN-FLDOFF SECTION.
       FAKIN-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-06                TO TRUE
           END-EVALUATE.
 
       FAKIN-FLDSET SECTION.
       FAKIN-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKIN-IO-AREA (1:3)    TO FIRMNR (1:3)
               MOVE FAKIN-IO-AREA (4:6)    TO RESKNR (1:6)
               MOVE FAKIN-IO-AREA (4:3)    TO RNR3F (1:3)
               MOVE FAKIN-IO-AREA (10:1)   TO FAKTYP (1:1)
               MOVE FAKIN-IO-AREA (12:1)   TO FAKMT (1:1)
               MOVE FAKIN-IO-AREA (14:2)   TO BETBET (1:2)
               MOVE FAKIN-IO-AREA (19:6)   TO ONR (1:6)
               MOVE FAKIN-IO-AREA (25:1)   TO RECART (1:1)
               MOVE FAKIN-IO-AREA (56:6)   TO FAKREF (1:6)
               MOVE FAKIN-IO-AREA (41:4)   TO KREF4F (1:4)
               MOVE FAKIN-IO-AREA (68:3)   TO HND (1:3)
               IF  HND = SPACES
                   SET I-06                TO TRUE
               END-IF
               MOVE FAKIN-IO-AREA (77:1)   TO VGRAVD (1:1)
               MOVE FAKIN-IO-AREA (71:2)   TO ODAG (1:2)
               MOVE FAKIN-IO-AREA (73:2)   TO OMND (1:2)
               MOVE FAKIN-IO-AREA (75:2)   TO OAAR (1:2)
               MOVE FAKIN-IO-AREA (77:5)   TO VGR (1:5)
               MOVE FAKIN-IO-AREA (141:7)  TO EDBNR (1:7)
               MOVE FAKIN-IO-AREA (177:1)  TO SPREC (1:1)
               MOVE FAKIN-IO-AREA (184:6)  TO ORDKNR (1:6)
               MOVE FAKIN-IO-AREA (1:200)  TO FAKREC (1:200)
           END-EVALUATE.
 
       FAKIN-IDSET SECTION.
       FAKIN-IDSET-P.
           SET I-02                        TO TRUE.
 
       FAKIN-CHK-LEVEL SECTION.
       FAKIN-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO FAKIN-LEVEL-02
               MOVE FAKIN-IO-AREA (1:3)    TO FAKIN-02-L3-FIRMNR
               MOVE FAKIN-IO-AREA (4:6)    TO FAKIN-02-L2-RESKNR
               MOVE FAKIN-IO-AREA (19:6)   TO FAKIN-02-L1-ONR
               IF  FAKIN-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  FAKIN-02-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  FAKIN-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  FAKIN-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  FAKIN-02-L3           TO THE-PRIOR-L3
               MOVE  FAKIN-02-L2           TO THE-PRIOR-L2
               MOVE  FAKIN-02-L1           TO THE-PRIOR-L1
               SET FAKIN-LEVEL-INIT        TO TRUE
           END-EVALUATE.
 
       FAKPAR-FLDSET SECTION.
       FAKPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKPAR-IO-AREA (18:6)  TO FAKPER (1:6)
               MOVE FAKPAR-IO-AREA (101:6) TO FUKPER (1:6)
           END-EVALUATE.
 
       FAKPAR-IDSET SECTION.
       FAKPAR-IDSET-P.
           SET I-01                        TO TRUE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (125:2) TO RESGRP (1:2)
               MOVE KUNDEMA-IO-AREA (166:1) TO BONUS (1:1)
               MOVE KUNDEMA-IO-AREA (185:3) TO CHND (1:3)
               MOVE KUNDEMA-IO-AREA (171:1) TO CFAKTM (1:1)
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-04                        TO TRUE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (1:3)   TO KONFNR (1:3)
               MOVE FIRMAF-IO-AREA (185:1) TO FFAKTY (1:1)
               MOVE FIRMAF-IO-AREA (779:1) TO FFKODE (1:1)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-05                        TO TRUE.
 
       BBONTAB-LOAD SECTION.
       BBONTAB-LOAD-P.
           OPEN INPUT BBONTAB
           SET TABFKV-I                    TO 1
           PERFORM UNTIL BBONTAB-EOF
               READ BBONTAB
               AT END
                   SET BBONTAB-EOF         TO TRUE
               NOT AT END
                   MOVE BBONTAB-IO-AREA (1:24) TO TABFKV-ENTRY
                                                            (TABFKV-I)
                   SET TABFKV-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE BBONTAB.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND I-10)
      *      OR        02 47
               MOVE SPACES TO SAMLEF-IO-AREA
               INITIALIZE SAMLEF-IO-AREA
               MOVE FAKREC                 TO SAMLEF-IO-AREA (1:200)
      *                      47FKUNDE     9
      *                        FAKART    11
      *                      47FFAKAR    11
               MOVE FAKMT                  TO SAMLEF-IO-AREA (12:1)
      *                      47FFAKMT    12
      *                      47FBETMT    15
               IF  (I-20)
                   MOVE 'H'                TO SAMLEF-IO-AREA (36:1)
               END-IF
               IF  (I-32)
                   MOVE 'R'                TO SAMLEF-IO-AREA (38:1)
               END-IF
               IF  (I-34)
                   MOVE 'B'                TO SAMLEF-IO-AREA (38:1)
               END-IF
               IF  (NOT-I-12)
                   MOVE NYFREF             TO SAMLEF-IO-AREA (56:6)
               END-IF
               IF  (NOT-I-12)
                   MOVE PHND               TO SAMLEF-IO-AREA (68:3)
      *                      47FHND      70
               END-IF
               IF  (I-40)
                   MOVE BONUS              TO SAMLEF-IO-AREA (176:1)
      *                      47FKUNDE   189
               END-IF
               WRITE SAMLEF-IO-AREA
           END-IF
           IF  (I-02 AND I-11 AND NOT-I-47)
               MOVE SPACES TO FAKOUT-IO-AREA
               INITIALIZE FAKOUT-IO-AREA
               MOVE FAKREC                 TO FAKOUT-IO-AREA (1:200)
      *                        FAKART    11
               MOVE FAKMT                  TO FAKOUT-IO-AREA (12:1)
               IF  (I-20)
                   MOVE 'H'                TO FAKOUT-IO-AREA (36:1)
               END-IF
               IF  (I-32)
                   MOVE 'R'                TO FAKOUT-IO-AREA (38:1)
               END-IF
               IF  (I-34)
                   MOVE 'B'                TO FAKOUT-IO-AREA (38:1)
               END-IF
               IF  (NOT-I-12)
                   MOVE NYFREF             TO FAKOUT-IO-AREA (56:6)
               END-IF
               IF  (NOT-I-12)
                   MOVE PHND               TO FAKOUT-IO-AREA (68:3)
               END-IF
               IF  (I-40)
                   MOVE BONUS              TO FAKOUT-IO-AREA (176:1)
               END-IF
               WRITE FAKOUT-IO-AREA
           END-IF
           IF  (I-02 AND I-60)
               MOVE SPACES TO FAKBON-IO-AREA
               INITIALIZE FAKBON-IO-AREA
               MOVE FAKREC                 TO FAKBON-IO-AREA (1:200)
               MOVE BBOKNR                 TO FAKBON-IO-AREA (4:6)
      *                        FAKART    11
               MOVE FAKMT                  TO FAKBON-IO-AREA (12:1)
               IF  (I-20)
                   MOVE 'H'                TO FAKBON-IO-AREA (36:1)
               END-IF
               IF  (I-32)
                   MOVE 'R'                TO FAKBON-IO-AREA (38:1)
               END-IF
               IF  (I-34)
                   MOVE 'B'                TO FAKBON-IO-AREA (38:1)
               END-IF
               IF  (NOT-I-12)
                   MOVE NYFREF             TO FAKBON-IO-AREA (56:6)
               END-IF
               IF  (NOT-I-12)
                   MOVE PHND               TO FAKBON-IO-AREA (68:3)
               END-IF
               MOVE 'T'                    TO FAKBON-IO-AREA (193:1)
               MOVE BBOPRO                 TO FAKBON-IO-AREA (194:4)
               WRITE FAKBON-IO-AREA
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
           PERFORM BBONTAB-LOAD
           SET FAKIN-LEVEL-INIT            TO TRUE
           INITIALIZE FAKIN-DATA-FIELDS
           SET FAKIN-EOF-OFF               TO TRUE
           SET FAKIN-PROCESS               TO TRUE
           OPEN INPUT FAKIN
           INITIALIZE FAKPAR-DATA-FIELDS
           OPEN INPUT FAKPAR
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           OPEN OUTPUT SAMLEF
           OPEN OUTPUT FAKOUT
           OPEN OUTPUT FAKBON.
           SET TABFKV-I                    TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FAKIN
           CLOSE FAKPAR
           CLOSE KUNDEMA
           CLOSE FIRMAF
           CLOSE SAMLEF
           CLOSE FAKOUT
           CLOSE FAKBON.
 
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
