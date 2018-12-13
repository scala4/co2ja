       IDENTIFICATION DIVISION.
       PROGRAM-ID. VAS007R.
      **********************************************  Z-WIN-RPG2   ****
      * DANNE RECORD FRA ORDREMASTER TIL OPPDAT. VARE.STAT.MASTER.    *
      *       -------------------------------------------------       *
      *  1. LESE ORDREMASTER OG DANNE AJOURHOLDSRECORD.               *
      *  2. MÅNED OG ÅR HENTES FRA FAKTURA PARAMETER.                 *
      *  3. LAGEROVERFØRINGSORDRE BLIR IKKE TATT MED.                 *
      *  4. SKAFFEVARER EDB-NR 9 BLIR IKKE TATT MED.                  *
      *  5. ORDRE TIL RESTORDREMASTER BLIR IKKE TATT MED.             *
      *  6. VERKSTEDSUTAK BLIR IKKE TATT MED.                         *
      *  7. KREDITNOTA SOM IKKE ER RETUR AV VARER BLIR IKKE TATT MED. *
      *  7. RETUR TIL LEVERANDØR (BK=B) BLIR IKKE TATT MED.           *
      *  19.12.17 LAGT INNT TEST PÅ KOMIND, FJERNET HAFNOR            *
      *  26.04.05 FIRMA 633 DØPES OM TIL 608 VED UPSI 1.              *
      *  25.10.05 KODE FOR VARESETT LAGT UT I POS. 30                 *
      *  25.10.05 VARESETT RECORDS LEGGES UT PÅ EGEN FILE.            *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VAS007.rpg
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
           SELECT ORDREM
               ASSIGN TO ORDREM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS ORDREM-STATUS
               RECORD KEY IS ORDREM-KEY1.
           SELECT FAKPAR
               ASSIGN TO UT-S-FAKPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKPAR-STATUS.
           SELECT VARSTAA
               ASSIGN TO UT-S-VARSTAA
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VARSTAA-STATUS.
           SELECT VARSTVS
               ASSIGN TO UT-S-VARSTVS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VARSTVS-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD ORDREM
               RECORD CONTAINS 164.
       01  ORDREM-IO-AREA.
           05  ORDREM-IO-AREA-X.
               10  ORDREM-KEY1             PICTURE X(20).
               10  FILLER                  PICTURE X(144).
       FD FAKPAR
               BLOCK CONTAINS 200
               RECORD CONTAINS 200.
       01  FAKPAR-IO-AREA.
           05  FAKPAR-IO-AREA-X            PICTURE X(200).
       FD VARSTAA
               BLOCK CONTAINS 80
               RECORD CONTAINS 40.
       01  VARSTAA-IO-AREA.
           05  VARSTAA-IO-AREA-X           PICTURE X(40).
       FD VARSTVS
               BLOCK CONTAINS 100
               RECORD CONTAINS 50.
       01  VARSTVS-IO-AREA.
           05  VARSTVS-IO-AREA-X           PICTURE X(50).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  ORDREM-STATUS               PICTURE 99 VALUE 0.
           10  FAKPAR-STATUS               PICTURE 99 VALUE 0.
           10  VARSTAA-STATUS              PICTURE 99 VALUE 0.
           10  VARSTVS-STATUS              PICTURE 99 VALUE 0.
           10  DATOER-XX-STATUS            PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  ORDREM-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDREM-EOF-OFF          VALUE '0'.
               88  ORDREM-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDREM-READ-OFF         VALUE '0'.
               88  ORDREM-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDREM-PROCESS-OFF      VALUE '0'.
               88  ORDREM-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  ORDREM-LEVEL-INIT-OFF   VALUE '0'.
               88  ORDREM-LEVEL-INIT       VALUE '1'.
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
               10  FILLER                  PICTURE X(79).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(1).
               10  DATO6                   PICTURE X(6).
               10  FILLER                  PICTURE X(73).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(7).
               10  DMA8                    PICTURE X(8).
               10  FILLER                  PICTURE X(65).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(15).
               10  AMD8                    PICTURE X(8).
               10  FILLER                  PICTURE X(57).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(23).
               10  DATOM                   PICTURE X(57).
           05  ORDREM-LEVEL-01.
               10  ORDREM-01-L2.
                   15  ORDREM-01-L2-FIRMA  PICTURE X(3).
               10  ORDREM-01-L1.
                   15  ORDREM-01-L1-ORDNR  PICTURE X(6).
           05  ORDREM-LEVEL-04.
               10  ORDREM-04-L2.
                   15  ORDREM-04-L2-FIRMA  PICTURE X(3).
               10  ORDREM-04-L1.
                   15  ORDREM-04-L1-ORDNR  PICTURE X(6).
           05  ORDREM-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  ORDNR                   PICTURE X(6).
               10  KNR1                    PICTURE X(1).
               10  LKODE                   PICTURE X(2).
               10  BK                      PICTURE X(1).
               10  KRETYP                  PICTURE X(1).
               10  KOMIND                  PICTURE X(1).
               10  ORDATO-IO.
                   15  ORDATO              PICTURE S9(6).
               10  ORDDAG                  PICTURE X(2).
               10  ORDMND                  PICTURE X(2).
               10  ORDAAR                  PICTURE X(2).
               10  RUTID                   PICTURE X(1).
               10  STATUS-X                PICTURE X(1).
               10  POSNR                   PICTURE X(3).
               10  ANTBES-IO.
                   15  ANTBES              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ANTLEV-IO.
                   15  ANTLEV              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ALF                     PICTURE X(3).
               10  EDBNR-IO.
                   15  EDBNR               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  VGR-IO.
                   15  VGR                 PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  ORPRIS-IO.
                   15  ORPRIS              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ORRAB1-IO.
                   15  ORRAB1              PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  ORRAB2-IO.
                   15  ORRAB2              PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  ORRAB3-IO.
                   15  ORRAB3              PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  PRITIL-IO.
                   15  PRITIL              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  VARSET                  PICTURE X(1).
           05  FAKPAR-DATA-FIELDS.
               10  PARMND                  PICTURE X(4).
      *****************************************************************
      *                 H O V E D R U T I N E .                       *
      *****************************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  LAGERK                  PICTURE X(2).
               10  ORDAT2                  PICTURE X(6).
               10  ORDAT3-IO.
                   15  ORDAT3              PICTURE S9(6).
               10  OARMND                  PICTURE X(4).
               10  OARMN6                  PICTURE X(6).
               10  PARMN6                  PICTURE X(6).
               10  NETTO-IO.
                   15  NETTO               PICTURE S9(7)V9(2).
               10  SUM1-IO.
                   15  SUM1                PICTURE S9(9)V9(2).
               10  RABBEL-IO.
                   15  RABBEL              PICTURE S9(7)V9(2).
               10  NETSUM-IO.
                   15  NETSUM              PICTURE S9(7)V9(2).
               10  NETANT-IO.
                   15  NETANT              PICTURE S9(7)V9(2).
               10  PRITUT-IO.
                   15  PRITUT              PICTURE S9(7)V9(2).
           05  EDITTING-FIELDS.
               10  XO-70D                  PICTURE S9(7).
               10  XO-70U                  PICTURE 9(7).
               10  XO-50D                  PICTURE S9(5).
               10  XO-50U                  PICTURE 9(5).
               10  XO-60P-EF.
                 15  XO-60P                PICTURE S9(6) USAGE
                                                       PACKED-DECIMAL.
               10  XO-72P-EF.
                 15  XO-72P                PICTURE S9(7)V9(2) USAGE
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
           SET NOT-I-02                    TO TRUE
           SET NOT-I-04                    TO TRUE
           SET NOT-I-08                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  ORDREM-PROCESS
               SET ORDREM-PROCESS-OFF      TO TRUE
               SET ORDREM-READ             TO TRUE
           END-IF
 
           IF  ORDREM-READ
           AND RECORD-SELECTED-OFF
               PERFORM ORDREM-GET
               SET ORDREM-READ-OFF         TO TRUE
               IF  NOT ORDREM-EOF
                   PERFORM ORDREM-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET ORDREM-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  ORDREM-PROCESS
               PERFORM ORDREM-IDSET
           END-IF
 
           IF  ORDREM-PROCESS
               PERFORM ORDREM-CHK-LEVEL
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
 
           IF  ORDREM-PROCESS
               PERFORM ORDREM-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  ORDREM-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-L2 AND NOT-I-10)
               READ FAKPAR
               AT END
                   SET I-H0                TO TRUE
                   MOVE 'M'                TO E-R-R-O-R
               NOT AT END
                   PERFORM FAKPAR-FLDSET
                   PERFORM FAKPAR-IDSET
               END-READ
               SET I-10                    TO TRUE
           END-IF
           SET NOT-I-50                    TO TRUE
           SET NOT-I-51                    TO TRUE
           IF  (I-01)
               SET NOT-I-30                TO TRUE
               IF  KOMIND = 'J'
                   SET I-30                TO TRUE
               END-IF
      *  01      FIRMA     COMP "633"                    29  GIDSKEN
           END-IF
           IF  (I-01)
               SET NOT-I-31                TO TRUE
               IF  FIRMA = '658'
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-31)
               SET NOT-I-36                TO TRUE
               IF  LKODE = '15'
                   SET I-36                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-31 AND NOT-I-36)
               SET NOT-I-36                TO TRUE
               IF  LKODE = '13'
                   SET I-36                TO TRUE
               END-IF
           END-IF
           IF  (I-01)
               SET NOT-I-32                TO TRUE
               IF  FIRMA = '626'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-32)
               SET NOT-I-32                TO TRUE
               IF  FIRMA = '732'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-32)
               SET NOT-I-32                TO TRUE
               IF  FIRMA = '915'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-32)
               SET NOT-I-32                TO TRUE
               IF  LKODE = '15'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           SET NOT-I-33                    TO TRUE
           IF  FIRMA = '956'
               SET I-33                    TO TRUE
           END-IF
           IF  (I-33)
               SET NOT-I-34                TO TRUE
               IF  LKODE = '13'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-33 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  LKODE = '15'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-33 AND I-34)
               SET NOT-I-35                TO TRUE
               IF  ALF = 'VEN'
                   SET I-35                TO TRUE
               END-IF
           END-IF
           IF  (I-33 AND I-34 AND NOT-I-35)
               SET NOT-I-35                TO TRUE
               IF  ALF = 'GAB'
                   SET I-35                TO TRUE
               END-IF
           END-IF
           IF  (I-33 AND I-34 AND NOT-I-35)
               SET NOT-I-35                TO TRUE
               IF  ALF = 'TIL'
                   SET I-35                TO TRUE
               END-IF
           END-IF
           IF  (I-33 AND I-34 AND NOT-I-35)
               SET NOT-I-35                TO TRUE
               IF  ALF = 'BOS'
                   SET I-35                TO TRUE
               END-IF
           END-IF
           IF  (I-33 AND I-34 AND NOT-I-35)
               SET NOT-I-35                TO TRUE
               IF  ALF = 'CAT'
                   SET I-35                TO TRUE
               END-IF
           END-IF
           IF  (I-33 AND I-34 AND I-35)
               SET NOT-I-34                TO TRUE
           END-IF
           IF  (I-01)
               PERFORM INRUT1-S
           END-IF
           IF  (I-04)
               PERFORM INRUT4-S
           END-IF
           IF  (I-04)
               PERFORM OSURUT-S
      *****************************************************************
      *  SUBRUTINE FOR SETTING AV FELLES INDIKATORER PR. ORDRE.       *
      *****************************************************************
           END-IF
           .
 
       INRUT1-S SECTION.
       INRUT1-S-P.
           SET NOT-I-16                    TO TRUE
           SET NOT-I-17                    TO TRUE
           SET NOT-I-12                    TO TRUE
           IF  STATUS-X = 'U'
               SET I-12                    TO TRUE
           END-IF
           SET NOT-I-16                    TO TRUE
           IF  RUTID = 'L'
               SET I-16                    TO TRUE
           END-IF
           IF  (NOT-I-16)
               SET NOT-I-16                TO TRUE
               IF  STATUS-X = 'L'
                   SET I-16                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-16)
               SET NOT-I-16                TO TRUE
               IF  BK = 'B'
                   SET I-16                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-30)
               SET NOT-I-17                TO TRUE
               IF  STATUS-X = 'V'
                   SET I-17                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-30 AND NOT-I-17)
               SET NOT-I-17                TO TRUE
               IF  BK = 'V'
                   SET I-17                TO TRUE
               END-IF
           END-IF
           SET NOT-I-18                    TO TRUE
           IF  STATUS-X = 'X'
               SET I-18                    TO TRUE
           END-IF
           IF  (NOT-I-18)
               SET NOT-I-18                TO TRUE
               IF  BK = 'R'
                   SET I-18                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-18)
               SET NOT-I-18                TO TRUE
               IF  BK = 'F'
                   SET I-18                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-18)
               SET NOT-I-18                TO TRUE
               IF  BK = 'T'
                   SET I-18                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-18)
               SET NOT-I-18                TO TRUE
               IF  BK = 'O'
                   SET I-18                TO TRUE
               END-IF
           END-IF
           SET NOT-I-23                    TO TRUE
           IF  RUTID = 'K'
               SET I-23                    TO TRUE
           END-IF
           IF  (I-23)
               SET NOT-I-22                TO TRUE
               IF  KRETYP = '2'
                   SET I-22                TO TRUE
               END-IF
           END-IF
           MOVE LKODE                      TO LAGERK
      ***** RUTINE FOR Å SNU ORDREDATO TIL ÅR MND DAG       ******
           MOVE ORDATO                     TO ORDAT2
           MOVE ORDAAR                     TO ORDAT2 (1:2)
           MOVE ORDDAG                     TO ORDAT2 (5:2)
           MOVE ORDAT2                     TO ORDAT3-IO
      ***** RUTINE FOR Å SETTE INN FAKTURAPERIODE.          ******
      *****   DEN MÅ IKKE VÆRE LAVERE ENN SISTE FAKTURERTE MND. **
      *****   ETTER SISTE FAKTURA I MND BLIR PERIODEN ØKT MED 1 MND.
           MOVE ORDAT2 (1:4)               TO OARMND
      *
           MOVE 'B'                        TO DATOK
           MOVE ORDAT3                     TO DATO6
           CALL 'DATO8SIF' USING DATOER-XX-DATA-FIELDS
           MOVE AMD8 (1:6)                 TO OARMN6
      *
           MOVE 'B'                        TO DATOK
           MOVE PARMND                     TO DATO6 (1:4)
           MOVE '01'                       TO DATO6 (5:2)
           CALL 'DATO8SIF' USING DATOER-XX-DATA-FIELDS
           MOVE AMD8 (1:6)                 TO PARMN6
           SET NOT-I-25                    TO TRUE
           IF  OARMN6 < PARMN6
               SET I-25                    TO TRUE
           END-IF
           IF  (I-25)
               MOVE PARMND                 TO OARMND
           END-IF.
      *****************************************************************
      *  SUBRUTINE FOR SETTING AV FELLES INDIKATORER PR. VARELINJE.   *
      *****************************************************************
 
       INRUT4-S SECTION.
       INRUT4-S-P.
           SET NOT-I-24                    TO TRUE
           IF  EDBNR = 0000000
               SET I-24                    TO TRUE
           END-IF
           SET NOT-I-28                    TO TRUE
           IF  ANTLEV > 0,00
               SET I-28                    TO TRUE
           END-IF
      *          EDBNR     COMP 9000000              19    =SKAFFEVARE.
           SET NOT-I-59                    TO TRUE
           IF  PRITIL > 0,00
               SET I-59                    TO TRUE
           END-IF
           SET NOT-I-82                    TO TRUE
           IF  ANTBES > 0,00
               SET I-82                    TO TRUE
           END-IF.
      *****************************************************************
      *                SUBRUTINE FOR ORDRESUMMERING.                  *
      *****************************************************************
 
       OSURUT-S SECTION.
       OSURUT-S-P.
           IF  (I-12)
               GO TO ENDOSU-T
           END-IF
           IF  (I-16)
               GO TO ENDOSU-T
           END-IF
           IF  (I-17)
               GO TO ENDOSU-T
           END-IF
           IF  (I-18)
               GO TO ENDOSU-T
      *  19                GOTO ENDOSU                     SKAFFEVARE.
           END-IF
           IF  (I-23 AND NOT-I-22)
               GO TO ENDOSU-T
           END-IF
           IF  (I-24)
               GO TO ENDOSU-T
           END-IF
           IF  (NOT-I-28)
               GO TO ENDOSU-T
      ****** SUMMERING AV ORDRELINJE TIL ORDRE PRIS/RABATT.
      *  SNU BELØP DERSOM KREDIT-GEBYR ELLER ANNEN RETUR.
           END-IF
           IF  (I-82)
               MULTIPLY ANTLEV BY ORPRIS GIVING NETTO ROUNDED
           END-IF
           IF  (NOT-I-82)
               ADD ORPRIS TO ZERO      GIVING NETTO
           END-IF
           MULTIPLY ORRAB1 BY NETTO    GIVING SUM1
           DIVIDE SUM1 BY 100          GIVING RABBEL ROUNDED
           SUBTRACT RABBEL                 FROM NETTO
           MULTIPLY ORRAB2 BY NETTO    GIVING SUM1
           DIVIDE SUM1 BY 100          GIVING RABBEL ROUNDED
           SUBTRACT RABBEL                 FROM NETTO
           MULTIPLY ORRAB3 BY NETTO    GIVING SUM1
           DIVIDE SUM1 BY 100          GIVING RABBEL ROUNDED
           SUBTRACT RABBEL                 FROM NETTO
           IF  (NOT-I-23)
               ADD NETTO TO ZERO       GIVING NETSUM
               ADD ANTLEV TO ZERO      GIVING NETANT
           END-IF
           IF  (I-23)
               MULTIPLY -1 BY NETTO    GIVING NETSUM ROUNDED
               MULTIPLY -1 BY ANTLEV   GIVING NETANT ROUNDED
      ****** LEGG TIL NETTO PRISTILLEGG (FRAKTTILLEGG/PANT)  *****
           END-IF
           IF  (I-59)
               MULTIPLY ANTLEV BY PRITIL GIVING NETTO ROUNDED
           END-IF
           IF  (I-59 AND NOT-I-23)
               ADD NETTO TO ZERO       GIVING PRITUT
           END-IF
           IF  (I-59 AND I-23)
               SUBTRACT NETTO FROM ZERO GIVING PRITUT
           END-IF
           IF  (I-59)
               ADD PRITUT                  TO NETSUM
           END-IF
           SET I-50                        TO TRUE
           SET NOT-I-51                    TO TRUE
           IF  VARSET = 'V'
               SET I-51                    TO TRUE
           END-IF
           IF  (NOT-I-51)
               SET NOT-I-51                TO TRUE
               IF  VARSET = 'W'
                   SET I-51                TO TRUE
               END-IF
           END-IF.
 
       ENDOSU-T.
           CONTINUE.
 
       ORDREM-GET SECTION.
       ORDREM-GET-P.
           IF  ORDREM-EOF-OFF
               READ ORDREM
               AT END
                   SET ORDREM-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       ORDREM-FLDSET SECTION.
       ORDREM-FLDSET-P.
           EVALUATE TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '1' )
               MOVE ORDREM-IO-AREA (2:3)   TO FIRMA (1:3)
               MOVE ORDREM-IO-AREA (5:6)   TO ORDNR (1:6)
               MOVE ORDREM-IO-AREA (21:1)  TO KNR1 (1:1)
               MOVE ORDREM-IO-AREA (90:2)  TO LKODE (1:2)
               MOVE ORDREM-IO-AREA (92:1)  TO BK (1:1)
               MOVE ORDREM-IO-AREA (99:1)  TO KRETYP (1:1)
               MOVE ORDREM-IO-AREA (105:1) TO KOMIND (1:1)
               MOVE ORDREM-IO-AREA (136:6) TO ORDATO-IO
               INSPECT ORDATO-IO REPLACING ALL ' ' BY '0'
               MOVE ORDREM-IO-AREA (136:2) TO ORDDAG (1:2)
               MOVE ORDREM-IO-AREA (138:2) TO ORDMND (1:2)
               MOVE ORDREM-IO-AREA (140:2) TO ORDAAR (1:2)
               MOVE ORDREM-IO-AREA (157:1) TO RUTID (1:1)
               MOVE ORDREM-IO-AREA (164:1) TO STATUS-X (1:1)
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) NOT = ' ' )
               MOVE ORDREM-IO-AREA (2:3)   TO FIRMA (1:3)
               MOVE ORDREM-IO-AREA (5:6)   TO ORDNR (1:6)
               MOVE ORDREM-IO-AREA (17:3)  TO POSNR (1:3)
               MOVE ORDREM-IO-AREA (21:4)  TO ANTBES-IO
               MOVE ORDREM-IO-AREA (29:4)  TO ANTLEV-IO
               MOVE ORDREM-IO-AREA (34:3)  TO ALF (1:3)
               MOVE ORDREM-IO-AREA (87:4)  TO EDBNR-IO
               MOVE ORDREM-IO-AREA (91:3)  TO VGR-IO
               MOVE ORDREM-IO-AREA (94:5)  TO ORPRIS-IO
               MOVE ORDREM-IO-AREA (99:2)  TO ORRAB1-IO
               MOVE ORDREM-IO-AREA (101:2) TO ORRAB2-IO
               MOVE ORDREM-IO-AREA (103:2) TO ORRAB3-IO
               MOVE ORDREM-IO-AREA (126:4) TO PRITIL-IO
               MOVE ORDREM-IO-AREA (135:1) TO VARSET (1:1)
           END-EVALUATE.
 
       ORDREM-IDCHK SECTION.
       ORDREM-IDCHK-P.
           EVALUATE TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '1' )
             OR ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) NOT = '1' )
             OR ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) NOT = ' ' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       ORDREM-IDSET SECTION.
       ORDREM-IDSET-P.
           EVALUATE TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '1' )
               SET I-01                    TO TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) NOT = '1' )
               SET I-02                    TO TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) NOT = ' ' )
               SET I-04                    TO TRUE
           END-EVALUATE.
 
       ORDREM-CHK-LEVEL SECTION.
       ORDREM-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '1' )
               MOVE LOW-VALUES             TO ORDREM-LEVEL-01
               MOVE ORDREM-IO-AREA (2:3)   TO ORDREM-01-L2-FIRMA
               MOVE ORDREM-IO-AREA (5:6)   TO ORDREM-01-L1-ORDNR
               IF  ORDREM-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDREM-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  ORDREM-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDREM-01-L2          TO THE-PRIOR-L2
               MOVE  ORDREM-01-L1          TO THE-PRIOR-L1
               SET ORDREM-LEVEL-INIT       TO TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) NOT = '1' )
               CONTINUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) NOT = ' ' )
               MOVE LOW-VALUES             TO ORDREM-LEVEL-04
               MOVE ORDREM-IO-AREA (2:3)   TO ORDREM-04-L2-FIRMA
               MOVE ORDREM-IO-AREA (5:6)   TO ORDREM-04-L1-ORDNR
               IF  ORDREM-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDREM-04-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  ORDREM-04-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDREM-04-L2          TO THE-PRIOR-L2
               MOVE  ORDREM-04-L1          TO THE-PRIOR-L1
               SET ORDREM-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       FAKPAR-FLDSET SECTION.
       FAKPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKPAR-IO-AREA (97:4)  TO PARMND (1:4)
           END-EVALUATE.
 
       FAKPAR-IDSET SECTION.
       FAKPAR-IDSET-P.
           SET I-08                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-04 AND I-50)
               MOVE SPACES TO VARSTAA-IO-AREA
               INITIALIZE VARSTAA-IO-AREA
               MOVE 'E'                    TO VARSTAA-IO-AREA (1:1)
               MOVE FIRMA                  TO VARSTAA-IO-AREA (2:3)
      *                   U1 29           4 "608"
               MOVE EDBNR                  TO XO-70U
               MOVE XO-70U (1:7)           TO VARSTAA-IO-AREA (5:7)
               MOVE '00'                   TO VARSTAA-IO-AREA (12:2)
               IF  (I-31 AND I-36)
                   MOVE LAGERK             TO VARSTAA-IO-AREA (12:2)
               END-IF
               IF  (I-32)
                   MOVE LAGERK             TO VARSTAA-IO-AREA (12:2)
               END-IF
               IF  (I-33 AND I-34)
                   MOVE LAGERK             TO VARSTAA-IO-AREA (12:2)
               END-IF
               MOVE ALF                    TO VARSTAA-IO-AREA (14:3)
               MOVE VGR                    TO XO-50U
               MOVE XO-50U (1:5)           TO VARSTAA-IO-AREA (17:5)
               MOVE ORDAT3                 TO XO-60P
               MOVE XO-60P-EF              TO VARSTAA-IO-AREA (22:4)
               MOVE OARMND                 TO VARSTAA-IO-AREA (26:4)
               IF  (I-51)
                   MOVE VARSET             TO VARSTAA-IO-AREA (30:1)
               END-IF
               MOVE NETANT                 TO XO-72P
               MOVE XO-72P-EF              TO VARSTAA-IO-AREA (31:5)
               MOVE NETSUM                 TO XO-72P
               MOVE XO-72P-EF              TO VARSTAA-IO-AREA (36:5)
               WRITE VARSTAA-IO-AREA
           END-IF
           IF  (I-04 AND I-50 AND I-51)
               MOVE SPACES TO VARSTVS-IO-AREA
               INITIALIZE VARSTVS-IO-AREA
               MOVE 'E'                    TO VARSTVS-IO-AREA (1:1)
               MOVE FIRMA                  TO VARSTVS-IO-AREA (2:3)
      *                   U1 29           4 "608"
               IF EDBNR < 0
                 MOVE EDBNR                TO XO-70D
                 MOVE XO-70D (1:7)         TO VARSTVS-IO-AREA (5:7)
               ELSE
                 MOVE EDBNR                TO XO-70U
                 MOVE XO-70U (1:7)         TO VARSTVS-IO-AREA (5:7)
               END-IF
               MOVE '00'                   TO VARSTVS-IO-AREA (12:2)
               IF  (I-31 AND I-36)
                   MOVE LAGERK             TO VARSTVS-IO-AREA (12:2)
               END-IF
               IF  (I-32)
                   MOVE LAGERK             TO VARSTVS-IO-AREA (12:2)
               END-IF
               IF  (I-33 AND I-34)
                   MOVE LAGERK             TO VARSTVS-IO-AREA (12:2)
               END-IF
               MOVE ALF                    TO VARSTVS-IO-AREA (14:3)
               IF VGR < 0
                 MOVE VGR                  TO XO-50D
                 MOVE XO-50D (1:5)         TO VARSTVS-IO-AREA (17:5)
               ELSE
                 MOVE VGR                  TO XO-50U
                 MOVE XO-50U (1:5)         TO VARSTVS-IO-AREA (17:5)
               END-IF
               MOVE ORDAT3                 TO XO-60P
               MOVE XO-60P-EF              TO VARSTVS-IO-AREA (22:4)
               MOVE OARMND                 TO VARSTVS-IO-AREA (26:4)
               MOVE VARSET                 TO VARSTVS-IO-AREA (30:1)
               MOVE NETANT                 TO XO-72P
               MOVE XO-72P-EF              TO VARSTVS-IO-AREA (31:5)
               MOVE NETSUM                 TO XO-72P
               MOVE XO-72P-EF              TO VARSTVS-IO-AREA (36:5)
               MOVE ORDNR                  TO VARSTVS-IO-AREA (41:6)
               MOVE POSNR                  TO VARSTVS-IO-AREA (47:3)
               WRITE VARSTVS-IO-AREA
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
           SET ORDREM-LEVEL-INIT           TO TRUE
           INITIALIZE ORDREM-DATA-FIELDS
           SET ORDREM-EOF-OFF              TO TRUE
           SET ORDREM-PROCESS              TO TRUE
           OPEN INPUT ORDREM
           INITIALIZE FAKPAR-DATA-FIELDS
           OPEN INPUT FAKPAR
           OPEN OUTPUT VARSTAA
           OPEN OUTPUT VARSTVS.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE ORDREM
           CLOSE FAKPAR
           CLOSE VARSTAA
           CLOSE VARSTVS.
 
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
