       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORD082R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM....: ORD082                                          *
      *  PROGRAMERER: ESPEN LARSEN                                    *
      *  PROGRAMT...: 23.03.01                                        *
      *  RETTET.....: 25.04.05                                        *
      *                                                               *
      *  PROGRAMMET SELEKTERER RECORDS FRA LAGER.OVERF.FILE OG        *
      *  OG DANNE RECORDS TIL LAGEROVERFØRINGS STATISTIKK.            *
      *  25.04.05 FIRMA 658 MJØSBIL SKAL KOSTPRIS I FRA OG TIL LAGER. *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ORD082.rpg
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
           SELECT TABELL
               ASSIGN TO UT-S-TABELL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TABELL-STATUS.
           SELECT TABKNR
               ASSIGN TO UT-S-TABKNR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TABKNR-STATUS.
           SELECT PARAM
               ASSIGN TO UT-S-PARAM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PARAM-STATUS.
           SELECT LAGOVF
               ASSIGN TO UT-S-LAGOVF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LAGOVF-STATUS.
           SELECT OUTREC
               ASSIGN TO UT-S-OUTREC
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTREC-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD TABELL
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  TABELL-IO-AREA.
           05  TABELL-IO-AREA-X            PICTURE X(80).
       FD TABKNR
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  TABKNR-IO-AREA.
           05  TABKNR-IO-AREA-X            PICTURE X(80).
       FD PARAM
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  PARAM-IO-AREA.
           05  PARAM-IO-AREA-X             PICTURE X(80).
       FD LAGOVF
               BLOCK CONTAINS 200
               RECORD CONTAINS 100.
       01  LAGOVF-IO-AREA.
           05  LAGOVF-IO-AREA-X            PICTURE X(100).
       FD OUTREC
               BLOCK CONTAINS 240
               RECORD CONTAINS 120.
       01  OUTREC-IO-AREA.
           05  OUTREC-IO-AREA-X            PICTURE X(120).
       WORKING-STORAGE SECTION.
       77  FIRTAB-MAX   VALUE 40           PICTURE 9(4) USAGE BINARY.
       77  KNRTAB-MAX   VALUE 200          PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  FIRTAB-TABLE.
               10  FIRTAB-ENTRY
                                           OCCURS 40 TIMES
                                           INDEXED BY FIRTAB-I
                                                      FIRTAB-S.
                   15  FIRTAB              PICTURE X(3).
           05  KNRTAB-TABLE.
               10  KNRTAB-ENTRY
                                           OCCURS 200 TIMES
                                           INDEXED BY KNRTAB-I
                                                      KNRTAB-S.
                   15  KNRTAB              PICTURE X(6).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  TABELL-STATUS               PICTURE 99 VALUE 0.
           10  TABKNR-STATUS               PICTURE 99 VALUE 0.
           10  PARAM-STATUS                PICTURE 99 VALUE 0.
           10  LAGOVF-STATUS               PICTURE 99 VALUE 0.
           10  OUTREC-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  TABELL-EOF-OFF          VALUE '0'.
               88  TABELL-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  TABKNR-EOF-OFF          VALUE '0'.
               88  TABKNR-EOF              VALUE '1'.
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
               88  LAGOVF-EOF-OFF          VALUE '0'.
               88  LAGOVF-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  LAGOVF-READ-OFF         VALUE '0'.
               88  LAGOVF-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  LAGOVF-PROCESS-OFF      VALUE '0'.
               88  LAGOVF-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  LAGOVF-LEVEL-INIT-OFF   VALUE '0'.
               88  LAGOVF-LEVEL-INIT       VALUE '1'.
           05  PARAM-DATA-FIELDS.
               10  PFIRMA                  PICTURE X(3).
               10  PAVD                    PICTURE X(1).
               10  PKUNDE                  PICTURE X(6).
               10  PAR                     PICTURE X(2).
               10  FMND-IO.
                   15  FMND                PICTURE S9(2).
               10  TMND-IO.
                   15  TMND                PICTURE S9(2).
           05  LAGOVF-LEVEL-02.
               10  LAGOVF-02-L2.
                   15  LAGOVF-02-L2-FFNR   PICTURE X(3).
                   15  LAGOVF-02-L2-TFNR   PICTURE X(3).
               10  LAGOVF-02-L1.
                   15  LAGOVF-02-L1-ORDNR  PICTURE X(6).
           05  LAGOVF-DATA-FIELDS.
               10  RECART                  PICTURE X(1).
               10  OVFREC                  PICTURE X(100).
               10  FFNR                    PICTURE X(3).
               10  TFNR                    PICTURE X(3).
               10  KUNDNR                  PICTURE X(6).
               10  KNRFLK                  PICTURE X(2).
               10  KNRTLK                  PICTURE X(2).
               10  ORDNR                   PICTURE X(6).
               10  ODATO                   PICTURE X(6).
               10  MND-IO.
                   15  MND                 PICTURE S9(2).
               10  AAR                     PICTURE X(2).
               10  ANTLEV-IO.
                   15  ANTLEV              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  FEDBNR                  PICTURE X(7).
               10  TEDBNR                  PICTURE X(7).
               10  FVGR                    PICTURE X(5).
               10  TVGR                    PICTURE X(5).
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  FPRIS-IO.
                   15  FPRIS               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  TPRIS-IO.
                   15  TPRIS               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  RAB1-IO.
                   15  RAB1                PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  RAB2-IO.
                   15  RAB2                PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  RAB3-IO.
                   15  RAB3                PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  OM                      PICTURE X(2).
               10  AVD                     PICTURE X(1).
               10  FLK                     PICTURE X(2).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(6).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  TILBEL-IO.
                   15  TILBEL              PICTURE S9(7)V9(2).
               10  SUM1-IO.
                   15  SUM1                PICTURE S9(9)V9(2).
               10  RABBEL-IO.
                   15  RABBEL              PICTURE S9(7)V9(4).
               10  FRABEL-IO.
                   15  FRABEL              PICTURE S9(7)V9(2).
               10  FRAANT-IO.
                   15  FRAANT              PICTURE S9(5)V9(2).
           05  EDITTING-FIELDS.
               10  XO-52P-EF.
                 15  XO-52P                PICTURE S9(5)V9(2) USAGE
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
                   SET PARAM-PROCESS       TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LAGOVF-PROCESS
               SET LAGOVF-PROCESS-OFF      TO TRUE
               SET LAGOVF-READ             TO TRUE
           END-IF
 
           IF  LAGOVF-READ
           AND RECORD-SELECTED-OFF
               PERFORM LAGOVF-GET
               SET LAGOVF-READ-OFF         TO TRUE
               IF  NOT LAGOVF-EOF
                   SET LAGOVF-PROCESS      TO TRUE
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
 
           IF  LAGOVF-PROCESS
               PERFORM LAGOVF-IDSET
           END-IF
 
           IF  LAGOVF-PROCESS
               PERFORM LAGOVF-CHK-LEVEL
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
 
           IF  PARAM-PROCESS
               PERFORM PARAM-FLDSET
           END-IF
 
           IF  LAGOVF-PROCESS
               PERFORM LAGOVF-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  LAGOVF-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-10                    TO TRUE
      *****************************************************************
      * PARAMETER RUTINE.                                             *
      *   SETT OM VI SELEKTER FRA TABELL ELLER PARAMETER.             *
      *****************************************************************
           IF  (I-01)
               SET NOT-I-11                TO TRUE
               IF  PAVD > ' '
                   SET I-11                TO TRUE
               END-IF
               SET NOT-I-12                TO TRUE
               IF  PKUNDE > '      '
                   SET I-12                TO TRUE
               END-IF
               SET NOT-I-13                TO TRUE
               IF  PKUNDE = 'TABELL'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-13)
               SET NOT-I-12                TO TRUE
           END-IF
           IF  (I-01)
               GO TO SLUTT-T
      *****************************************************************
      * SELEKSJONSRUTINE.                                             *
      *****************************************************************
           END-IF
           SET NOT-I-20                    TO TRUE
           IF  FFNR = PFIRMA
               SET I-20                    TO TRUE
           END-IF
           IF  (NOT-I-20)
               SET NOT-I-20                TO TRUE
               IF  TFNR = PFIRMA
                   SET I-20                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-20)
               SET NOT-I-21                TO TRUE
               SET FIRTAB-S                TO 1
               PERFORM WITH TEST AFTER
                       VARYING FIRTAB-I FROM 1 BY 1
                         UNTIL FIRTAB-I >= FIRTAB-MAX
                            OR I-21
                   IF  FFNR = FIRTAB (FIRTAB-I)
                       SET I-21            TO TRUE
                       SET FIRTAB-S        TO FIRTAB-I
                   END-IF
               END-PERFORM
           END-IF
           IF  (NOT-I-20 AND NOT-I-21)
               SET NOT-I-21                TO TRUE
               SET FIRTAB-S                TO 1
               PERFORM WITH TEST AFTER
                       VARYING FIRTAB-I FROM 1 BY 1
                         UNTIL FIRTAB-I >= FIRTAB-MAX
                            OR I-21
                   IF  TFNR = FIRTAB (FIRTAB-I)
                       SET I-21            TO TRUE
                       SET FIRTAB-S        TO FIRTAB-I
                   END-IF
               END-PERFORM
           END-IF
           IF  (NOT-I-20 AND NOT-I-21)
               GO TO SLUTT-T
           END-IF
           IF  (I-11)
               SET NOT-I-21                TO TRUE
               IF  AVD = PAVD
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-11 AND NOT-I-21)
               GO TO SLUTT-T
           END-IF
           IF  (I-12)
               SET NOT-I-21                TO TRUE
               IF  KUNDNR = PKUNDE
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-12 AND NOT-I-21)
               GO TO SLUTT-T
           END-IF
           IF  (I-13)
               SET NOT-I-21                TO TRUE
               SET KNRTAB-S                TO 1
               PERFORM WITH TEST AFTER
                       VARYING KNRTAB-I FROM 1 BY 1
                         UNTIL KNRTAB-I >= KNRTAB-MAX
                            OR I-21
                   IF  KUNDNR = KNRTAB (KNRTAB-I)
                       SET I-21            TO TRUE
                       SET KNRTAB-S        TO KNRTAB-I
                   END-IF
               END-PERFORM
           END-IF
           IF  (I-13 AND NOT-I-21)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-22                    TO TRUE
           IF  AAR = PAR
               SET I-22                    TO TRUE
           END-IF
           IF  (NOT-I-22)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-15                    TO TRUE
           IF  MND < FMND
               SET I-15                    TO TRUE
           END-IF
           IF  (I-15)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-20                    TO TRUE
           IF  MND > TMND
               SET I-20                    TO TRUE
           END-IF
           IF  (I-20)
               GO TO SLUTT-T
      *          LAGER     COMP "PT"                     25 PRIS TILEGG
           END-IF
           SET I-10                        TO TRUE
      *****************************************************************
      * SETT INDIKATOR FOR LAGEROVERFØRINGSTYPE.                      *
      *****************************************************************
           SET NOT-I-31                    TO TRUE
           IF  RECART = 'L'
               SET I-31                    TO TRUE
           END-IF
           SET NOT-I-32                    TO TRUE
           IF  RECART = 'V'
               SET I-32                    TO TRUE
           END-IF
      *****************************************************************
      * RUTINE FOR BEREGNING AV NETTOBELØP TIL LAGER.                 *
      *****************************************************************
           MULTIPLY ANTLEV BY TPRIS    GIVING TILBEL ROUNDED
           MULTIPLY RAB1 BY TILBEL     GIVING SUM1
           DIVIDE SUM1 BY 100          GIVING RABBEL
           SUBTRACT RABBEL                 FROM TILBEL ROUNDED
           MULTIPLY RAB2 BY TILBEL     GIVING SUM1
           DIVIDE SUM1 BY 100          GIVING RABBEL
           SUBTRACT RABBEL                 FROM TILBEL ROUNDED
           MULTIPLY RAB3 BY TILBEL     GIVING SUM1
           DIVIDE SUM1 BY 100          GIVING RABBEL
           SUBTRACT RABBEL                 FROM TILBEL ROUNDED
      *****************************************************************
      * RUTINE FOR BEREGNING AV NETTOBELØP FRA LAGER.                 *
      *****************************************************************
           SET NOT-I-61                    TO TRUE
           IF  FPRIS = 0,00
               SET I-61                    TO TRUE
           END-IF
           IF  (NOT-I-61)
               MULTIPLY ANTLEV BY FPRIS GIVING FRABEL ROUNDED
           END-IF
           IF  (I-61)
               ADD TILBEL TO ZERO      GIVING FRABEL
           END-IF
           MULTIPLY -1 BY FRABEL       GIVING FRABEL
           MULTIPLY -1 BY ANTLEV       GIVING FRAANT
      *****************************************************************
      * PÅ FIRMA 658 SKAL BELØP FRA OG TIL VÆRE DET SAMME.            *
      *    HER BRUKES SLEVKOSTPRIS OM DETTE IKKE ER 0,00              *
      *****************************************************************
           SET NOT-I-58                    TO TRUE
           IF  TFNR = '658'
               SET I-58                    TO TRUE
           END-IF
           IF  (I-58)
               ADD FRABEL TO ZERO      GIVING TILBEL
      *****************************************************************
           END-IF
           .
 
       SLUTT-T.
      *****************************************************************
      * OUTPUTRECORD FRA LAGER.   (F I POS 120)                       *
      *****************************************************************
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
           WHEN ANY
               MOVE PARAM-IO-AREA (16:3)   TO PFIRMA (1:3)
               MOVE PARAM-IO-AREA (24:1)   TO PAVD (1:1)
               MOVE PARAM-IO-AREA (34:6)   TO PKUNDE (1:6)
               MOVE PARAM-IO-AREA (44:2)   TO PAR (1:2)
               MOVE PARAM-IO-AREA (55:2)   TO FMND-IO
               INSPECT FMND-IO REPLACING ALL ' ' BY '0'
               MOVE PARAM-IO-AREA (66:2)   TO TMND-IO
               INSPECT TMND-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       PARAM-IDSET SECTION.
       PARAM-IDSET-P.
           SET I-01                        TO TRUE.
 
       LAGOVF-GET SECTION.
       LAGOVF-GET-P.
           IF  LAGOVF-EOF-OFF
               READ LAGOVF
               AT END
                   SET LAGOVF-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       LAGOVF-FLDSET SECTION.
       LAGOVF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LAGOVF-IO-AREA (1:1)   TO RECART (1:1)
               MOVE LAGOVF-IO-AREA (1:100) TO OVFREC (1:100)
               MOVE LAGOVF-IO-AREA (2:3)   TO FFNR (1:3)
               MOVE LAGOVF-IO-AREA (5:3)   TO TFNR (1:3)
               MOVE LAGOVF-IO-AREA (8:6)   TO KUNDNR (1:6)
               MOVE LAGOVF-IO-AREA (10:2)  TO KNRFLK (1:2)
               MOVE LAGOVF-IO-AREA (12:2)  TO KNRTLK (1:2)
               MOVE LAGOVF-IO-AREA (14:6)  TO ORDNR (1:6)
               MOVE LAGOVF-IO-AREA (20:6)  TO ODATO (1:6)
               MOVE LAGOVF-IO-AREA (22:2)  TO MND-IO
               INSPECT MND-IO REPLACING ALL ' ' BY '0'
               MOVE LAGOVF-IO-AREA (24:2)  TO AAR (1:2)
               MOVE LAGOVF-IO-AREA (26:4)  TO ANTLEV-IO
               MOVE LAGOVF-IO-AREA (30:7)  TO FEDBNR (1:7)
               MOVE LAGOVF-IO-AREA (37:7)  TO TEDBNR (1:7)
               MOVE LAGOVF-IO-AREA (44:5)  TO FVGR (1:5)
               MOVE LAGOVF-IO-AREA (49:5)  TO TVGR (1:5)
               MOVE LAGOVF-IO-AREA (54:3)  TO ALFA (1:3)
               MOVE LAGOVF-IO-AREA (57:20) TO ARTNR (1:20)
               MOVE LAGOVF-IO-AREA (77:5)  TO FPRIS-IO
               MOVE LAGOVF-IO-AREA (82:5)  TO TPRIS-IO
               MOVE LAGOVF-IO-AREA (87:2)  TO RAB1-IO
               MOVE LAGOVF-IO-AREA (89:2)  TO RAB2-IO
               MOVE LAGOVF-IO-AREA (91:2)  TO RAB3-IO
               MOVE LAGOVF-IO-AREA (93:2)  TO OM (1:2)
               MOVE LAGOVF-IO-AREA (95:1)  TO AVD (1:1)
               MOVE LAGOVF-IO-AREA (96:2)  TO FLK (1:2)
           END-EVALUATE.
 
       LAGOVF-IDSET SECTION.
       LAGOVF-IDSET-P.
           SET I-02                        TO TRUE.
 
       LAGOVF-CHK-LEVEL SECTION.
       LAGOVF-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO LAGOVF-LEVEL-02
               MOVE LAGOVF-IO-AREA (2:3)   TO LAGOVF-02-L2-FFNR
               MOVE LAGOVF-IO-AREA (5:3)   TO LAGOVF-02-L2-TFNR
               MOVE LAGOVF-IO-AREA (14:6)  TO LAGOVF-02-L1-ORDNR
               IF  LAGOVF-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  LAGOVF-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  LAGOVF-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  LAGOVF-02-L2          TO THE-PRIOR-L2
               MOVE  LAGOVF-02-L1          TO THE-PRIOR-L1
               SET LAGOVF-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       TABELL-LOAD SECTION.
       TABELL-LOAD-P.
           OPEN INPUT TABELL
           SET FIRTAB-I                    TO 1
           PERFORM UNTIL TABELL-EOF
               READ TABELL
               AT END
                   SET TABELL-EOF          TO TRUE
               NOT AT END
                   MOVE TABELL-IO-AREA (1:3) TO FIRTAB-ENTRY (FIRTAB-I)
                   SET FIRTAB-I            UP BY 1
                   MOVE TABELL-IO-AREA (4:3) TO FIRTAB-ENTRY (FIRTAB-I)
                   SET FIRTAB-I            UP BY 1
                   MOVE TABELL-IO-AREA (7:3) TO FIRTAB-ENTRY (FIRTAB-I)
                   SET FIRTAB-I            UP BY 1
                   MOVE TABELL-IO-AREA (10:3) TO FIRTAB-ENTRY
                                                            (FIRTAB-I)
                   SET FIRTAB-I            UP BY 1
                   MOVE TABELL-IO-AREA (13:3) TO FIRTAB-ENTRY
                                                            (FIRTAB-I)
                   SET FIRTAB-I            UP BY 1
                   MOVE TABELL-IO-AREA (16:3) TO FIRTAB-ENTRY
                                                            (FIRTAB-I)
                   SET FIRTAB-I            UP BY 1
                   MOVE TABELL-IO-AREA (19:3) TO FIRTAB-ENTRY
                                                            (FIRTAB-I)
                   SET FIRTAB-I            UP BY 1
                   MOVE TABELL-IO-AREA (22:3) TO FIRTAB-ENTRY
                                                            (FIRTAB-I)
                   SET FIRTAB-I            UP BY 1
                   MOVE TABELL-IO-AREA (25:3) TO FIRTAB-ENTRY
                                                            (FIRTAB-I)
                   SET FIRTAB-I            UP BY 1
                   MOVE TABELL-IO-AREA (28:3) TO FIRTAB-ENTRY
                                                            (FIRTAB-I)
                   SET FIRTAB-I            UP BY 1
                   MOVE TABELL-IO-AREA (31:3) TO FIRTAB-ENTRY
                                                            (FIRTAB-I)
                   SET FIRTAB-I            UP BY 1
                   MOVE TABELL-IO-AREA (34:3) TO FIRTAB-ENTRY
                                                            (FIRTAB-I)
                   SET FIRTAB-I            UP BY 1
                   MOVE TABELL-IO-AREA (37:3) TO FIRTAB-ENTRY
                                                            (FIRTAB-I)
                   SET FIRTAB-I            UP BY 1
                   MOVE TABELL-IO-AREA (40:3) TO FIRTAB-ENTRY
                                                            (FIRTAB-I)
                   SET FIRTAB-I            UP BY 1
                   MOVE TABELL-IO-AREA (43:3) TO FIRTAB-ENTRY
                                                            (FIRTAB-I)
                   SET FIRTAB-I            UP BY 1
                   MOVE TABELL-IO-AREA (46:3) TO FIRTAB-ENTRY
                                                            (FIRTAB-I)
                   SET FIRTAB-I            UP BY 1
                   MOVE TABELL-IO-AREA (49:3) TO FIRTAB-ENTRY
                                                            (FIRTAB-I)
                   SET FIRTAB-I            UP BY 1
                   MOVE TABELL-IO-AREA (52:3) TO FIRTAB-ENTRY
                                                            (FIRTAB-I)
                   SET FIRTAB-I            UP BY 1
                   MOVE TABELL-IO-AREA (55:3) TO FIRTAB-ENTRY
                                                            (FIRTAB-I)
                   SET FIRTAB-I            UP BY 1
                   MOVE TABELL-IO-AREA (58:3) TO FIRTAB-ENTRY
                                                            (FIRTAB-I)
                   SET FIRTAB-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE TABELL.
 
       TABKNR-LOAD SECTION.
       TABKNR-LOAD-P.
           OPEN INPUT TABKNR
           SET KNRTAB-I                    TO 1
           PERFORM UNTIL TABKNR-EOF
               READ TABKNR
               AT END
                   SET TABKNR-EOF          TO TRUE
               NOT AT END
                   MOVE TABKNR-IO-AREA (1:6) TO KNRTAB-ENTRY (KNRTAB-I)
                   SET KNRTAB-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE TABKNR.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND I-10)
               MOVE SPACES TO OUTREC-IO-AREA
               INITIALIZE OUTREC-IO-AREA
               MOVE OVFREC                 TO OUTREC-IO-AREA (1:100)
               MOVE FRAANT                 TO XO-52P
               MOVE XO-52P-EF              TO OUTREC-IO-AREA (26:4)
               MOVE FRABEL                 TO XO-72P
               MOVE XO-72P-EF              TO OUTREC-IO-AREA (101:5)
               MOVE FFNR                   TO OUTREC-IO-AREA (106:3)
               IF  (I-32)
                   MOVE FLK                TO OUTREC-IO-AREA (109:2)
               END-IF
               IF  (I-31)
                   MOVE KNRFLK             TO OUTREC-IO-AREA (109:2)
               END-IF
               MOVE 'F'                    TO OUTREC-IO-AREA (120:1)
      *****************************************************************
      * OUTPUTRECORD TIL LAGER.   (T I POS 120)                       *
      *****************************************************************
               WRITE OUTREC-IO-AREA
               MOVE SPACES TO OUTREC-IO-AREA
               INITIALIZE OUTREC-IO-AREA
               MOVE OVFREC                 TO OUTREC-IO-AREA (1:100)
               MOVE TILBEL                 TO XO-72P
               MOVE XO-72P-EF              TO OUTREC-IO-AREA (101:5)
               MOVE TFNR                   TO OUTREC-IO-AREA (106:3)
               IF  (I-32)
                   MOVE '10'               TO OUTREC-IO-AREA (109:2)
               END-IF
               IF  (I-31)
                   MOVE KNRTLK             TO OUTREC-IO-AREA (109:2)
               END-IF
               MOVE 'T'                    TO OUTREC-IO-AREA (120:1)
               WRITE OUTREC-IO-AREA
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
           PERFORM TABELL-LOAD
           PERFORM TABKNR-LOAD
           INITIALIZE PARAM-DATA-FIELDS
           SET PARAM-EOF-OFF               TO TRUE
           SET PARAM-PROCESS               TO TRUE
           OPEN INPUT PARAM
           SET LAGOVF-LEVEL-INIT           TO TRUE
           INITIALIZE LAGOVF-DATA-FIELDS
           SET LAGOVF-EOF-OFF              TO TRUE
           SET LAGOVF-PROCESS              TO TRUE
           OPEN INPUT LAGOVF
           OPEN OUTPUT OUTREC.
           SET FIRTAB-I                    TO 1
           SET KNRTAB-I                    TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PARAM
           CLOSE LAGOVF
           CLOSE OUTREC.
 
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
