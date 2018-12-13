       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORD970R.
      **********************************************  Z-WIN-RPG2   ****
      *       O R D R E R U T I N E P R O G R A M   O R D 9 7 0       *
      *       -------------------------------------------------       *
      *  1. DANNE RECORD SOM SKAL DANNE KUNDE.VGR.TOTAL.FILE          *
      *      24/4-2002 AV ESPEN LARSEN.                               *
      *      OBS: FAKTURA. SALGSDATA ER INPUT                         *
      * 15.12.03: NY RUTINE FOR Å FINNE PERIODE.                      *
      * 18.09.08: LAGT PÅ SELVKOST                                    *
      * 15.05.11: KONTANTSALG PERIODISERES SOM KREDITSALG             *
      * 13.05.13: LEGGER KONSERNFIRMANR PÅ UT FILE                    *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ORD970.rpg
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
           SELECT FAKSALG
               ASSIGN TO UT-S-FAKSALG
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKSALG-STATUS.
           SELECT AUTOPAR
               ASSIGN TO AUTOPAR
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS AUTOPAR-STATUS
               RECORD KEY IS AUTOPAR-KEY1.
           SELECT KUNDTOT
               ASSIGN TO UT-S-KUNDTOT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KUNDTOT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD FAKSALG
               BLOCK CONTAINS 320
               RECORD CONTAINS 160.
       01  FAKSALG-IO-AREA.
           05  FAKSALG-IO-AREA-X           PICTURE X(160).
       FD AUTOPAR
               RECORD CONTAINS 1000.
       01  AUTOPAR-IO-AREA.
           05  AUTOPAR-IO-AREA-X.
               10  AUTOPAR-KEY1            PICTURE X(3).
               10  FILLER                  PICTURE X(997).
       FD KUNDTOT
               BLOCK CONTAINS 88
               RECORD CONTAINS 44.
       01  KUNDTOT-IO-AREA.
           05  KUNDTOT-IO-AREA-X           PICTURE X(44).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  FAKSALG-STATUS              PICTURE 99 VALUE 0.
           10  AUTOPAR-STATUS              PICTURE 99 VALUE 0.
           10  KUNDTOT-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKSALG-EOF-OFF         VALUE '0'.
               88  FAKSALG-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKSALG-READ-OFF        VALUE '0'.
               88  FAKSALG-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKSALG-PROCESS-OFF     VALUE '0'.
               88  FAKSALG-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  FAKSALG-LEVEL-INIT-OFF  VALUE '0'.
               88  FAKSALG-LEVEL-INIT      VALUE '1'.
           05  AUTOPAR-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FAKSALG-LEVEL-01.
               10  FAKSALG-01-L1.
                   15  FAKSALG-01-L1-FIRMA PICTURE X(3).
           05  FAKSALG-DATA-FIELDS.
               10  KFIRMA                  PICTURE X(3).
               10  FIRMA                   PICTURE X(3).
               10  MND-IO.
                   15  MND                 PICTURE S9(2).
               10  AAR-IO.
                   15  AAR                 PICTURE S9(4).
               10  FK                      PICTURE X(1).
      *                                      44  45 LK
               10  KUNDE                   PICTURE X(6).
               10  OSIFF1                  PICTURE X(1).
               10  AVD                     PICTURE X(1).
               10  KTYP                    PICTURE X(1).
               10  VGR                     PICTURE X(5).
      *                                      56  58 KUNGRP
               10  VGRBEL-IO.
                   15  VGRBEL              PICTURE S9(7)V9(2).
               10  VGRSEL-IO.
                   15  VGRSEL              PICTURE S9(7)V9(2).
               10  OMG                     PICTURE X(1).
               10  OM                      PICTURE X(2).
           05  AUTOPAR-DATA-FIELDS.
      *                                       6   70APMND
      *                                      26  290APAAR
               10  APAAR-IO.
                   15  APAAR               PICTURE S9(4).
               10  APMND-IO.
                   15  APMND               PICTURE S9(2).
               10  APFOMG                  PICTURE X(1).
      *****************************************************************
      * HVILKEN FAKTURAOMGANGSNR. ER SIST KJØRT DENNE MND.            *
      * NY RUTINE HVOR SISTE FAKT.OMG. HENSYNTAS.                     *
      *****************************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  APKEY                   PICTURE X(3).
               10  FAAR-IO.
                   15  FAAR                PICTURE S9(4).
               10  PAAR-IO.
                   15  PAAR                PICTURE S9(4).
               10  PMND-IO.
                   15  PMND                PICTURE S9(2).
               10  POMG                    PICTURE X(1).
               10  FIRNR                   PICTURE X(3).
               10  NTOBEL-IO.
                   15  NTOBEL              PICTURE S9(7)V9(2).
               10  NTOSEL-IO.
                   15  NTOSEL              PICTURE S9(7)V9(2).
               10  BELDPA-ELG-IO.
                   15  BELDPA-ELG          PICTURE S9(9)V9(2).
               10  SELKPA-ELG-IO.
                   15  SELKPA-ELG          PICTURE S9(9)V9(2).
               10  BELDPF-IO.
                   15  BELDPF              PICTURE S9(9)V9(2).
               10  SELKPF-IO.
                   15  SELKPF              PICTURE S9(9)V9(2).
           05  EDITTING-FIELDS.
               10  XO-92P-EF.
                 15  XO-92P                PICTURE S9(9)V9(2) USAGE
                                                       PACKED-DECIMAL.
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
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  FAKSALG-PROCESS
               SET FAKSALG-PROCESS-OFF     TO TRUE
               SET FAKSALG-READ            TO TRUE
           END-IF
 
           IF  FAKSALG-READ
           AND RECORD-SELECTED-OFF
               PERFORM FAKSALG-GET
               SET FAKSALG-READ-OFF        TO TRUE
               IF  NOT FAKSALG-EOF
                   SET FAKSALG-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  FAKSALG-PROCESS
               PERFORM FAKSALG-IDSET
           END-IF
 
           IF  FAKSALG-PROCESS
               PERFORM FAKSALG-CHK-LEVEL
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
 
           IF  FAKSALG-PROCESS
               PERFORM FAKSALG-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  FAKSALG-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (NOT-I-09)
               MOVE 'B01'                  TO APKEY
               MOVE APKEY                  TO AUTOPAR-KEY1
               READ AUTOPAR RECORD KEY IS AUTOPAR-KEY1
               INVALID KEY
                   SET I-99                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-99            TO TRUE
                   PERFORM AUTOPAR-FLDSET
                   PERFORM AUTOPAR-IDSET
               END-READ
               SUBTRACT 1 FROM APAAR   GIVING FAAR
               ADD APAAR TO ZERO       GIVING PAAR
               ADD APMND TO ZERO       GIVING PMND
               MOVE APFOMG                 TO POMG
               SET I-09                    TO TRUE
      *****************************************************************
      * OPPSLAG MOT PARAMFIL.AUTODATA FOR HENTING AV AKTUELL DATO.    *
      * I januar må det tilpasse år og mnd.                           *
      * GAMMEL RUTINE. EN KJØRING PR. MND.                            *
      *****************************************************************
      * N09                MOVE "B01"     APKEY   3
      * N09      APKEY     CHAINAUTOPAR              99
      * N09      APMND     COMP 01                       11= JANUAR
      * N09 11   APAAR     SUB  2         FAAR    40       = FOREG. ÅR.
      * N09N11   APAAR     SUB  1         FAAR             = FOREG. ÅR.
      * N09 11   APAAR     SUB  1         PAAR    40       = INNV. ÅR.
      * N09N11             Z-ADDAPAAR     PAAR             = INNV. ÅR.
      * N09 11             Z-ADD12        PMND    20       = INNV.MND.
      * N09N11   APMND     SUB  1         PMND             = INNV.MND.
      * N09                SETON                     09
      *****************************************************************
      * OMDØPING AV FIRMANUMMER P.G.A. SAMMENSLÅING AV FIRMA.         *
      *          VENG KAN FJERNES ETTER 1.01.2003                     *
      *****************************************************************
           END-IF
           IF  (I-L1)
               MOVE FIRMA                  TO FIRNR
               SET NOT-I-77                TO TRUE
               IF  FIRMA = '957'
                   SET I-77                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-77)
               MOVE '956'                  TO FIRNR
      *****************************************************************
      *                 H O V E D R U T I N E .                       *
      *****************************************************************
      * RUTINE FOR Å FINNE SAMME PERIODE FOR I FJOR.                  *
      *****************************************************************
           END-IF
           SET NOT-I-15                    TO TRUE
           SET NOT-I-50                    TO TRUE
           SET NOT-I-13                    TO TRUE
           IF  AAR = PAAR
               SET I-13                    TO TRUE
           END-IF
           SET NOT-I-21                    TO TRUE
           IF  AAR = FAAR
               SET I-21                    TO TRUE
           END-IF
           SET NOT-I-22                    TO TRUE
           SET NOT-I-23                    TO TRUE
           IF  MND < PMND
               SET I-22                    TO TRUE
           END-IF
           IF  MND = PMND
               SET I-23                    TO TRUE
           END-IF
           SET NOT-I-44                    TO TRUE
           IF  OMG = 'D'
               SET I-44                    TO TRUE
           END-IF
           SET NOT-I-43                    TO TRUE
           IF  OMG = 'C'
               SET I-43                    TO TRUE
           END-IF
           SET NOT-I-42                    TO TRUE
           IF  OMG = 'B'
               SET I-42                    TO TRUE
           END-IF
           SET NOT-I-41                    TO TRUE
           IF  OMG = 'A'
               SET I-41                    TO TRUE
           END-IF
           IF  (I-01 AND I-44)
               MOVE '4'                    TO OMG
           END-IF
           IF  (I-01 AND I-43)
               MOVE '3'                    TO OMG
           END-IF
           IF  (I-01 AND I-42)
               MOVE '2'                    TO OMG
           END-IF
           IF  (I-01 AND I-41)
               MOVE '1'                    TO OMG
           END-IF
           SET NOT-I-24                    TO TRUE
           IF  OMG NOT > POMG
               SET I-24                    TO TRUE
           END-IF
           IF  (I-21 AND I-22)
               SET I-15                    TO TRUE
           END-IF
           IF  (I-21 AND I-23 AND I-24)
               SET I-15                    TO TRUE
           END-IF
           ADD VGRBEL TO ZERO          GIVING NTOBEL
           IF  (I-01)
               SET NOT-I-45                TO TRUE
               IF  FK = 'K'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-45)
               SET NOT-I-46                TO TRUE
               IF  KTYP = '3'
                   SET I-46                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-45 AND I-46)
               MOVE 0,00                   TO VGRSEL
           END-IF
           ADD VGRSEL TO ZERO          GIVING NTOSEL
      *****************************************************************
      *          RUTINE FOR Å NULLSTILLE SUMMER.                      *
      *****************************************************************
           IF  (I-01)
               MOVE 0,00                   TO BELDPA-ELG
               MOVE 0,00                   TO BELDPF
               MOVE 0,00                   TO SELKPA-ELG
               MOVE 0,00                   TO SELKPF
      *****************************************************************
      *         RUTINE FOR Å SJEKKE OM DATA SKAL VÆRE MED.            *
      *****************************************************************
      *  01      RUT       COMP "L"                      51= LAGEROVERF.
      *  01 51             GOTO SLUTT                      = SKAL IKKE VÆRE MED
      *****************************************************************
      *          RUTINE FOR Å DANNE SUMMER.                           *
      ****************************************************************
           END-IF
           IF  (I-01 AND I-13)
               ADD NTOBEL TO ZERO      GIVING BELDPA-ELG
               ADD NTOSEL TO ZERO      GIVING SELKPA-ELG
           END-IF
           IF  (I-01 AND I-15)
               ADD NTOBEL TO ZERO      GIVING BELDPF
               ADD NTOSEL TO ZERO      GIVING SELKPF
           END-IF
           IF  (I-01 AND I-13)
               SET I-50                    TO TRUE
           END-IF
           IF  (I-01 AND I-15)
               SET I-50                    TO TRUE
      *****************************************************************
      *          SLUTT     TAG                                          G
      *****************************************************************
      * KUNDETOTALER.                                                 *
      *****************************************************************
           END-IF
           .
 
       FAKSALG-GET SECTION.
       FAKSALG-GET-P.
           IF  FAKSALG-EOF-OFF
               READ FAKSALG
               AT END
                   SET FAKSALG-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FAKSALG-FLDSET SECTION.
       FAKSALG-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKSALG-IO-AREA (1:3)  TO KFIRMA (1:3)
               MOVE FAKSALG-IO-AREA (39:3) TO FIRMA (1:3)
               MOVE FAKSALG-IO-AREA (8:2)  TO MND-IO
               INSPECT MND-IO REPLACING ALL ' ' BY '0'
               MOVE FAKSALG-IO-AREA (4:4)  TO AAR-IO
               INSPECT AAR-IO REPLACING ALL ' ' BY '0'
               MOVE FAKSALG-IO-AREA (47:1) TO FK (1:1)
               MOVE FAKSALG-IO-AREA (10:6) TO KUNDE (1:6)
               MOVE FAKSALG-IO-AREA (33:1) TO OSIFF1 (1:1)
               MOVE FAKSALG-IO-AREA (46:1) TO AVD (1:1)
               MOVE FAKSALG-IO-AREA (48:1) TO KTYP (1:1)
               MOVE FAKSALG-IO-AREA (51:5) TO VGR (1:5)
               MOVE FAKSALG-IO-AREA (122:9) TO VGRBEL-IO
               INSPECT VGRBEL-IO REPLACING ALL ' ' BY '0'
               MOVE FAKSALG-IO-AREA (131:9) TO VGRSEL-IO
               INSPECT VGRSEL-IO REPLACING ALL ' ' BY '0'
               MOVE FAKSALG-IO-AREA (147:1) TO OMG (1:1)
               MOVE FAKSALG-IO-AREA (148:2) TO OM (1:2)
           END-EVALUATE.
 
       FAKSALG-IDSET SECTION.
       FAKSALG-IDSET-P.
           SET I-01                        TO TRUE.
 
       FAKSALG-CHK-LEVEL SECTION.
       FAKSALG-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO FAKSALG-LEVEL-01
               MOVE FAKSALG-IO-AREA (39:3) TO FAKSALG-01-L1-FIRMA
               IF  FAKSALG-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  FAKSALG-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  FAKSALG-01-L1         TO THE-PRIOR-L1
               SET FAKSALG-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       AUTOPAR-FLDSET SECTION.
       AUTOPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE AUTOPAR-IO-AREA (72:4) TO APAAR-IO
               INSPECT APAAR-IO REPLACING ALL ' ' BY '0'
               MOVE AUTOPAR-IO-AREA (76:2) TO APMND-IO
               INSPECT APMND-IO REPLACING ALL ' ' BY '0'
               MOVE AUTOPAR-IO-AREA (78:1) TO APFOMG (1:1)
           END-EVALUATE.
 
       AUTOPAR-IDSET SECTION.
       AUTOPAR-IDSET-P.
           SET I-03                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-50)
               MOVE SPACES TO KUNDTOT-IO-AREA
               INITIALIZE KUNDTOT-IO-AREA
               MOVE FIRNR                  TO KUNDTOT-IO-AREA (1:3)
               MOVE 'K'                    TO KUNDTOT-IO-AREA (4:1)
               MOVE KUNDE                  TO KUNDTOT-IO-AREA (5:6)
               MOVE '     '                TO KUNDTOT-IO-AREA (11:5)
               MOVE MND-IO                 TO KUNDTOT-IO-AREA (16:2)
               MOVE BELDPA-ELG             TO XO-92P
               MOVE XO-92P-EF              TO KUNDTOT-IO-AREA (18:6)
               MOVE BELDPF                 TO XO-92P
               MOVE XO-92P-EF              TO KUNDTOT-IO-AREA (24:6)
               MOVE SELKPA-ELG             TO XO-92P
               MOVE XO-92P-EF              TO KUNDTOT-IO-AREA (30:6)
               MOVE SELKPF                 TO XO-92P
               MOVE XO-92P-EF              TO KUNDTOT-IO-AREA (36:6)
               MOVE KFIRMA                 TO KUNDTOT-IO-AREA (42:3)
      *****************************************************************
      * ORDREMOT TOTALER                                              *
      *****************************************************************
               WRITE KUNDTOT-IO-AREA
               MOVE SPACES TO KUNDTOT-IO-AREA
               INITIALIZE KUNDTOT-IO-AREA
               MOVE FIRNR                  TO KUNDTOT-IO-AREA (1:3)
               MOVE 'O'                    TO KUNDTOT-IO-AREA (4:1)
               MOVE OM                     TO KUNDTOT-IO-AREA (5:2)
               MOVE '         '            TO KUNDTOT-IO-AREA (7:9)
               MOVE MND-IO                 TO KUNDTOT-IO-AREA (16:2)
               MOVE BELDPA-ELG             TO XO-92P
               MOVE XO-92P-EF              TO KUNDTOT-IO-AREA (18:6)
               MOVE BELDPF                 TO XO-92P
               MOVE XO-92P-EF              TO KUNDTOT-IO-AREA (24:6)
               MOVE SELKPA-ELG             TO XO-92P
               MOVE XO-92P-EF              TO KUNDTOT-IO-AREA (30:6)
               MOVE SELKPF                 TO XO-92P
               MOVE XO-92P-EF              TO KUNDTOT-IO-AREA (36:6)
               MOVE KFIRMA                 TO KUNDTOT-IO-AREA (42:3)
      *****************************************************************
      * AVDELINGS TOTALER                                             *
      *****************************************************************
               WRITE KUNDTOT-IO-AREA
               MOVE SPACES TO KUNDTOT-IO-AREA
               INITIALIZE KUNDTOT-IO-AREA
               MOVE FIRNR                  TO KUNDTOT-IO-AREA (1:3)
               MOVE 'L'                    TO KUNDTOT-IO-AREA (4:1)
               MOVE AVD                    TO KUNDTOT-IO-AREA (5:1)
               MOVE '          '           TO KUNDTOT-IO-AREA (6:10)
               MOVE MND-IO                 TO KUNDTOT-IO-AREA (16:2)
               MOVE BELDPA-ELG             TO XO-92P
               MOVE XO-92P-EF              TO KUNDTOT-IO-AREA (18:6)
               MOVE BELDPF                 TO XO-92P
               MOVE XO-92P-EF              TO KUNDTOT-IO-AREA (24:6)
               MOVE SELKPA-ELG             TO XO-92P
               MOVE XO-92P-EF              TO KUNDTOT-IO-AREA (30:6)
               MOVE SELKPF                 TO XO-92P
               MOVE XO-92P-EF              TO KUNDTOT-IO-AREA (36:6)
               MOVE KFIRMA                 TO KUNDTOT-IO-AREA (42:3)
      *****************************************************************
      * VAREGRUPPE TOTALER.                                           *
      *****************************************************************
               WRITE KUNDTOT-IO-AREA
               MOVE SPACES TO KUNDTOT-IO-AREA
               INITIALIZE KUNDTOT-IO-AREA
               MOVE FIRNR                  TO KUNDTOT-IO-AREA (1:3)
               MOVE 'V'                    TO KUNDTOT-IO-AREA (4:1)
               MOVE '      '               TO KUNDTOT-IO-AREA (5:6)
               MOVE VGR                    TO KUNDTOT-IO-AREA (11:5)
               MOVE MND-IO                 TO KUNDTOT-IO-AREA (16:2)
               MOVE BELDPA-ELG             TO XO-92P
               MOVE XO-92P-EF              TO KUNDTOT-IO-AREA (18:6)
               MOVE BELDPF                 TO XO-92P
               MOVE XO-92P-EF              TO KUNDTOT-IO-AREA (24:6)
               MOVE SELKPA-ELG             TO XO-92P
               MOVE XO-92P-EF              TO KUNDTOT-IO-AREA (30:6)
               MOVE SELKPF                 TO XO-92P
               MOVE XO-92P-EF              TO KUNDTOT-IO-AREA (36:6)
               MOVE KFIRMA                 TO KUNDTOT-IO-AREA (42:3)
      *****************************************************************
      * KUNDE/VAREGRUPPE TOTALER                                      *
      *****************************************************************
               WRITE KUNDTOT-IO-AREA
               MOVE SPACES TO KUNDTOT-IO-AREA
               INITIALIZE KUNDTOT-IO-AREA
               MOVE FIRNR                  TO KUNDTOT-IO-AREA (1:3)
               MOVE 'X'                    TO KUNDTOT-IO-AREA (4:1)
               MOVE KUNDE                  TO KUNDTOT-IO-AREA (5:6)
               MOVE VGR                    TO KUNDTOT-IO-AREA (11:5)
               MOVE MND-IO                 TO KUNDTOT-IO-AREA (16:2)
               MOVE BELDPA-ELG             TO XO-92P
               MOVE XO-92P-EF              TO KUNDTOT-IO-AREA (18:6)
               MOVE BELDPF                 TO XO-92P
               MOVE XO-92P-EF              TO KUNDTOT-IO-AREA (24:6)
               MOVE SELKPA-ELG             TO XO-92P
               MOVE XO-92P-EF              TO KUNDTOT-IO-AREA (30:6)
               MOVE SELKPF                 TO XO-92P
               MOVE XO-92P-EF              TO KUNDTOT-IO-AREA (36:6)
               MOVE KFIRMA                 TO KUNDTOT-IO-AREA (42:3)
      *****************************************************************
      * AVDELING /VAREGRUPPE TOTALER
      *****************************************************************
               WRITE KUNDTOT-IO-AREA
               MOVE SPACES TO KUNDTOT-IO-AREA
               INITIALIZE KUNDTOT-IO-AREA
               MOVE FIRNR                  TO KUNDTOT-IO-AREA (1:3)
               MOVE 'M'                    TO KUNDTOT-IO-AREA (4:1)
               MOVE 'AVD'                  TO KUNDTOT-IO-AREA (5:3)
               MOVE AVD                    TO KUNDTOT-IO-AREA (8:1)
               MOVE '  '                   TO KUNDTOT-IO-AREA (9:2)
               MOVE VGR                    TO KUNDTOT-IO-AREA (11:5)
               MOVE MND-IO                 TO KUNDTOT-IO-AREA (16:2)
               MOVE BELDPA-ELG             TO XO-92P
               MOVE XO-92P-EF              TO KUNDTOT-IO-AREA (18:6)
               MOVE BELDPF                 TO XO-92P
               MOVE XO-92P-EF              TO KUNDTOT-IO-AREA (24:6)
               MOVE SELKPA-ELG             TO XO-92P
               MOVE XO-92P-EF              TO KUNDTOT-IO-AREA (30:6)
               MOVE SELKPF                 TO XO-92P
               MOVE XO-92P-EF              TO KUNDTOT-IO-AREA (36:6)
               MOVE KFIRMA                 TO KUNDTOT-IO-AREA (42:3)
               WRITE KUNDTOT-IO-AREA
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
           SET FAKSALG-LEVEL-INIT          TO TRUE
           INITIALIZE FAKSALG-DATA-FIELDS
           SET FAKSALG-EOF-OFF             TO TRUE
           SET FAKSALG-PROCESS             TO TRUE
           OPEN INPUT FAKSALG
           INITIALIZE AUTOPAR-DATA-FIELDS
           OPEN INPUT AUTOPAR
           OPEN OUTPUT KUNDTOT.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FAKSALG
           CLOSE AUTOPAR
           CLOSE KUNDTOT.
 
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
