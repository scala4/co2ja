       IDENTIFICATION DIVISION.
       PROGRAM-ID. VAR710R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM VAR710 AV ESPEN LARSEN.  25.04.2002           *
      *  MERGER VAREMAS OG VARETIL OG DENNER EN HJELPEFILE     *
      *  MED DATA FRA BEGGE FILER. DANNER KOMP. OPPS.NR.      *
      *  FJERNE BLANKE OG KONTROLLTEGN I ARTNR.                *
      **********************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VAR710.rpg
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
           SELECT SVAREM
               ASSIGN TO UT-S-SVAREM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS SVAREM-STATUS.
           SELECT SVARET
               ASSIGN TO UT-S-SVARET
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS SVARET-STATUS.
           SELECT OUTPUT-X
               ASSIGN TO UT-S-OUTPUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTPUT-X-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD SVAREM
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  SVAREM-IO-AREA.
           05  SVAREM-IO-AREA-X            PICTURE X(200).
       FD SVARET
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  SVARET-IO-AREA.
           05  SVARET-IO-AREA-X            PICTURE X(200).
       FD OUTPUT-X
               BLOCK CONTAINS 360
               RECORD CONTAINS 180.
       01  OUTPUT-X-IO-AREA.
           05  OUTPUT-X-IO-AREA-X          PICTURE X(180).
       WORKING-STORAGE SECTION.
       77  ARA-MAX   VALUE 20              PICTURE 9(4) USAGE BINARY.
       77  ARO-MAX   VALUE 20              PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  ARA-TABLE.
               10  ARA-ENTRY
                                           OCCURS 20 TIMES
                                           INDEXED BY ARA-I
                                                      ARA-S.
                   15  ARA                 PICTURE X(1).
           05  ARO-TABLE.
               10  ARO-ENTRY
                                           OCCURS 20 TIMES
                                           INDEXED BY ARO-I
                                                      ARO-S.
                   15  ARO                 PICTURE X(1).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  SVAREM-STATUS               PICTURE 99 VALUE 0.
           10  SVARET-STATUS               PICTURE 99 VALUE 0.
           10  OUTPUT-X-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  SVAREM-EOF-OFF          VALUE '0'.
               88  SVAREM-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SVAREM-READ-OFF         VALUE '0'.
               88  SVAREM-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SVAREM-PROCESS-OFF      VALUE '0'.
               88  SVAREM-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  SVAREM-LEVEL-INIT-OFF   VALUE '0'.
               88  SVAREM-LEVEL-INIT       VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SVARET-EOF-OFF          VALUE '0'.
               88  SVARET-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SVARET-READ-OFF         VALUE '0'.
               88  SVARET-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SVARET-PROCESS-OFF      VALUE '0'.
               88  SVARET-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  SVARET-LEVEL-INIT-OFF   VALUE '0'.
               88  SVARET-LEVEL-INIT       VALUE '1'.
           05  SVAREM-LEVEL-01.
               10  SVAREM-01-L2.
                   15  SVAREM-01-L2-FNR    PICTURE X(3).
               10  SVAREM-01-L1.
                   15  SVAREM-01-L1-EDBNR  PICTURE X(7).
           05  SVAREM-DATA-FIELDS.
               10  FNR                     PICTURE X(3).
               10  EDBNR                   PICTURE X(7).
               10  ARTNR                   PICTURE X(20).
               10  REC                     PICTURE X(170).
           05  SVAREM-MP                   PICTURE X(10).
           05  SVAREM-MC                   PICTURE X(10).
           05  SVAREM-M-01             REDEFINES SVAREM-MC.
               10  SVAREM-M-01-M2.
                   15  SVAREM-M-01-M2-FNR-G.
                       20  SVAREM-M-01-M2-FNR PICTURE X(3).
               10  SVAREM-M-01-M1.
                   15  SVAREM-M-01-M1-EDBNR-G.
                       20  SVAREM-M-01-M1-EDBNR PICTURE X(7).
           05  SVARET-LEVEL-02.
               10  SVARET-02-L2.
                   15  SVARET-02-L2-FNR    PICTURE X(3).
               10  SVARET-02-L1.
                   15  SVARET-02-L1-EDBNR  PICTURE X(7).
           05  SVARET-DATA-FIELDS.
               10  VPRIS-IO.
                   15  VPRIS               PICTURE S9(7)V9(2).
               10  ECO                     PICTURE X(1).
      *
      *****************************************************************
      * HOVEDRUTINE-                                                  *
      *****************************************************************
           05  SVARET-MP                   PICTURE X(10).
           05  SVARET-MC                   PICTURE X(10).
           05  SVARET-M-02             REDEFINES SVARET-MC.
               10  SVARET-M-02-M2.
                   15  SVARET-M-02-M2-FNR-G.
                       20  SVARET-M-02-M2-FNR PICTURE X(3).
               10  SVARET-M-02-M1.
                   15  SVARET-M-02-M1-EDBNR-G.
                       20  SVARET-M-02-M1-EDBNR PICTURE X(7).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(7).
           05  TEMPORARY-FIELDS.
               10  VPRIS2-IO.
                   15  VPRIS2              PICTURE S9(7)V9(2).
               10  ECO1                    PICTURE X(1).
               10  MAXANT-IO.
                   15  MAXANT              PICTURE S9(2).
               10  X-IO.
                   15  X                   PICTURE S9(2).
               10  Z-IO.
                   15  Z                   PICTURE S9(2).
               10  ANR                     PICTURE X(1).
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
               88  NOT-CALL-MATCH-RECS     VALUE '0'.
               88  CALL-MATCH-RECS         VALUE '1'.
           05  FILLER                      PICTURE X.
               88  NOT-SET-I-MR            VALUE '0'.
               88  SET-I-MR                VALUE '1'.
           05  FILLER                      PICTURE X.
               88  NOT-IN-DETAIL-OUTPUT    VALUE '0'.
               88  IN-DETAIL-OUTPUT        VALUE '1'.
           05  FILLER                      PICTURE X.
               88  RECORD-SELECTED-OFF     VALUE '0'.
               88  RECORD-SELECTED         VALUE '1'.
           05  E-R-R-O-R                   PICTURE X(12).
           05  MOVEA-COUNT                 PICTURE 9(4) USAGE BINARY.
           05  MOVEA-SIZE1                 PICTURE 9(4) USAGE BINARY.
           05  MOVEA-SA1                   PICTURE 9(4) USAGE BINARY.
           05  MOVEA-SIZE2                 PICTURE 9(4) USAGE BINARY.
           05  MOVEA-SA2                   PICTURE 9(4) USAGE BINARY.
           05  MOVEA-LENGTH                PICTURE 9(4) USAGE BINARY.
           05  MOVEA-OFFSET                PICTURE 9(4) USAGE BINARY.
           05  MOVEA-TEMP                  PICTURE X(256).
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
 
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
 
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
           IF  SVAREM-PROCESS
               SET SVAREM-PROCESS-OFF      TO TRUE
               SET SVAREM-READ             TO TRUE
           END-IF
 
           IF  SVAREM-READ
               PERFORM SVAREM-GET
               SET SVAREM-READ-OFF         TO TRUE
               IF  NOT SVAREM-EOF
                   PERFORM SVAREM-MATCH-SET
               END-IF
           END-IF
 
           IF  SVARET-PROCESS
               SET SVARET-PROCESS-OFF      TO TRUE
               SET SVARET-READ             TO TRUE
           END-IF
 
           IF  SVARET-READ
               PERFORM SVARET-GET
               SET SVARET-READ-OFF         TO TRUE
               IF  NOT SVARET-EOF
                   PERFORM SVARET-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  SVAREM-PROCESS
               PERFORM SVAREM-IDSET
           END-IF
 
           IF  SVARET-PROCESS
               PERFORM SVARET-IDSET
           END-IF
 
           IF  SVAREM-PROCESS
               PERFORM SVAREM-CHK-LEVEL
           END-IF
 
           IF  SVARET-PROCESS
               PERFORM SVARET-CHK-LEVEL
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
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  SVAREM-PROCESS
               PERFORM SVAREM-FLDSET
           END-IF
 
           IF  SVARET-PROCESS
               PERFORM SVARET-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  SVAREM-PROCESS
           OR  SVARET-PROCESS
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
               MOVE 0,00                   TO VPRIS2
               SET NOT-I-51                TO TRUE
           END-IF
           IF  (I-01)
               SET I-51                    TO TRUE
           END-IF
           IF  (I-02)
               ADD VPRIS TO ZERO       GIVING VPRIS2
               MOVE ECO                    TO ECO1
               GO TO SLUTT-T
      *****************************************************************
      * RUTINE FOR DANNE OPPSLAGSNUMMER AV ARTIKKELNUMMER.           *
      *****************************************************************
           END-IF
           MOVE 20                         TO MAXANT
           MOVE 1                          TO MOVEA-SA1 MOVEA-SA2
           MOVE 20                         TO MOVEA-SIZE1
           MULTIPLY ARA-MAX BY 1 GIVING MOVEA-SIZE2
           IF  MOVEA-SIZE1 > MOVEA-SIZE2
               MOVE MOVEA-SIZE2            TO MOVEA-SIZE1
           END-IF
           MOVE ARTNR
                    TO ARA-TABLE (MOVEA-SA2:MOVEA-SIZE2)
           SUBTRACT X                      FROM X
           SUBTRACT Z                      FROM Z.
 
       RUTA-T.
           ADD 1                           TO X
           SET NOT-I-21                    TO TRUE
           IF  X > MAXANT
               SET I-21                    TO TRUE
           END-IF
           IF  (I-21)
               GO TO SLUTT-T
           END-IF
           MOVE ARA (X)                    TO ANR
           SET NOT-I-40                    TO TRUE
           IF  ANR = '.'
               SET I-40                    TO TRUE
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '&'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '*'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = ','
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '+'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '-'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '_'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '/'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = ')'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '('
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '""""
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '='
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = '%'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  ANR = ' '
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (I-40)
               GO TO RUTA-T
      *      RUTINE FOR OPPBYGGING AV OPPSLAGSNUMMER           *
           END-IF
           ADD 1                           TO Z
           SET NOT-I-22                    TO TRUE
           IF  Z > MAXANT
               SET I-22                    TO TRUE
           END-IF
           IF  (I-22)
               GO TO SLUTT-T
           END-IF
           MOVE ANR                        TO ARO (Z)
           GO TO RUTA-T.
 
       SLUTT-T.
           CONTINUE.
 
       SVAREM-GET SECTION.
       SVAREM-GET-P.
           IF  SVAREM-EOF-OFF
               READ SVAREM
               AT END
                   SET SVAREM-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       SVAREM-FLDSET SECTION.
       SVAREM-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE SVAREM-IO-AREA (3:3)   TO FNR (1:3)
               MOVE SVAREM-IO-AREA (6:7)   TO EDBNR (1:7)
               MOVE SVAREM-IO-AREA (16:20) TO ARTNR (1:20)
               MOVE SVAREM-IO-AREA (1:170) TO REC (1:170)
           END-EVALUATE.
 
       SVAREM-IDSET SECTION.
       SVAREM-IDSET-P.
           SET I-01                        TO TRUE.
 
       SVAREM-CHK-LEVEL SECTION.
       SVAREM-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO SVAREM-LEVEL-01
               MOVE SVAREM-IO-AREA (3:3)   TO SVAREM-01-L2-FNR
               MOVE SVAREM-IO-AREA (6:7)   TO SVAREM-01-L1-EDBNR
               IF  SVAREM-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  SVAREM-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  SVAREM-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  SVAREM-01-L2          TO THE-PRIOR-L2
               MOVE  SVAREM-01-L1          TO THE-PRIOR-L1
               SET SVAREM-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       SVAREM-MATCH-SET SECTION.
       SVAREM-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE SVAREM-IO-AREA (3:3)   TO SVAREM-M-01-M2-FNR
               MOVE SVAREM-IO-AREA (6:7)   TO SVAREM-M-01-M1-EDBNR
           END-EVALUATE.
 
       SVARET-GET SECTION.
       SVARET-GET-P.
           IF  SVARET-EOF-OFF
               READ SVARET
               AT END
                   SET SVARET-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       SVARET-FLDSET SECTION.
       SVARET-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE SVARET-IO-AREA (3:3)   TO FNR (1:3)
               MOVE SVARET-IO-AREA (6:7)   TO EDBNR (1:7)
               MOVE SVARET-IO-AREA (180:9) TO VPRIS-IO
               INSPECT VPRIS-IO REPLACING ALL ' ' BY '0'
               MOVE SVARET-IO-AREA (200:1) TO ECO (1:1)
           END-EVALUATE.
 
       SVARET-IDSET SECTION.
       SVARET-IDSET-P.
           SET I-02                        TO TRUE.
 
       SVARET-CHK-LEVEL SECTION.
       SVARET-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO SVARET-LEVEL-02
               MOVE SVARET-IO-AREA (3:3)   TO SVARET-02-L2-FNR
               MOVE SVARET-IO-AREA (6:7)   TO SVARET-02-L1-EDBNR
               IF  SVARET-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  SVARET-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  SVARET-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  SVARET-02-L2          TO THE-PRIOR-L2
               MOVE  SVARET-02-L1          TO THE-PRIOR-L1
               SET SVARET-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       SVARET-MATCH-SET SECTION.
       SVARET-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE SVARET-IO-AREA (3:3)   TO SVARET-M-02-M2-FNR
               MOVE SVARET-IO-AREA (6:7)   TO SVARET-M-02-M1-EDBNR
           END-EVALUATE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  SVAREM-EOF
               MOVE HIGH-VALUES            TO SVAREM-MC
                                              SVAREM-MP
           END-IF
           IF  SVARET-EOF
               MOVE HIGH-VALUES            TO SVARET-MC
                                              SVARET-MP
           END-IF
           IF  SVAREM-MC < SVAREM-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  SVARET-MC < SVARET-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  SVAREM-MC < SVARET-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET SVAREM-PROCESS      TO TRUE
                   MOVE SVAREM-MC          TO SVAREM-MP
                   IF  SVAREM-MC = SVARET-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  SVARET-MC < SVAREM-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET SVARET-PROCESS      TO TRUE
                   MOVE SVARET-MC          TO SVARET-MP
                   IF  SVARET-MC = SVAREM-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  SVAREM-MC = SVARET-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET SVAREM-PROCESS      TO TRUE
                   MOVE SVAREM-MC          TO SVAREM-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-51)
               MOVE SPACES TO OUTPUT-X-IO-AREA
               INITIALIZE OUTPUT-X-IO-AREA
               MOVE REC                    TO OUTPUT-X-IO-AREA (1:170)
               MOVE 36                     TO BW-A
               PERFORM VARYING ARO-I FROM ARO-MAX BY -1
                         UNTIL ARO-I < 1
                   SUBTRACT 1            FROM BW-A
                   MOVE ARO-ENTRY (ARO-I)  TO OUTPUT-X-IO-AREA (BW-A:1)
                   INITIALIZE ARO-ENTRY (ARO-I)
               END-PERFORM
               MOVE VPRIS2-IO              TO OUTPUT-X-IO-AREA (171:9)
               MOVE ECO1                   TO OUTPUT-X-IO-AREA (180:1)
               INITIALIZE ECO1
               WRITE OUTPUT-X-IO-AREA
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
           SET SVAREM-LEVEL-INIT           TO TRUE
           INITIALIZE SVAREM-DATA-FIELDS
           SET SVAREM-EOF-OFF              TO TRUE
           SET SVAREM-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO SVAREM-MC
                                              SVAREM-MP
           OPEN INPUT SVAREM
           SET SVARET-LEVEL-INIT           TO TRUE
           INITIALIZE SVARET-DATA-FIELDS
           SET SVARET-EOF-OFF              TO TRUE
           SET SVARET-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO SVARET-MC
                                              SVARET-MP
           OPEN INPUT SVARET
           OPEN OUTPUT OUTPUT-X.
           PERFORM VARYING ARA-I FROM 1 BY 1
                     UNTIL ARA-I > ARA-MAX
               INITIALIZE ARA (ARA-I)
           END-PERFORM
           SET ARA-I                       TO 1
           PERFORM VARYING ARO-I FROM 1 BY 1
                     UNTIL ARO-I > ARO-MAX
               INITIALIZE ARO (ARO-I)
           END-PERFORM
           SET ARO-I                       TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE SVAREM
           CLOSE SVARET
           CLOSE OUTPUT-X.
 
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
 
       SETOFF-I-M SECTION.
           SET NOT-I-M1                    TO TRUE.
           SET NOT-I-M2                    TO TRUE.
           SET NOT-I-M3                    TO TRUE.
           SET NOT-I-M4                    TO TRUE.
           SET NOT-I-M5                    TO TRUE.
           SET NOT-I-M6                    TO TRUE.
           SET NOT-I-M7                    TO TRUE.
           SET NOT-I-M8                    TO TRUE.
           SET NOT-I-M9                    TO TRUE.
 
       SETON-I-M9 SECTION.
           SET I-M9                        TO TRUE.
           PERFORM SETON-I-M8.
 
       SETON-I-M8 SECTION.
           SET I-M8                        TO TRUE.
           PERFORM SETON-I-M7.
 
       SETON-I-M7 SECTION.
           SET I-M7                        TO TRUE.
           PERFORM SETON-I-M6.
 
       SETON-I-M6 SECTION.
           SET I-M6                        TO TRUE.
           PERFORM SETON-I-M5.
 
       SETON-I-M5 SECTION.
           SET I-M5                        TO TRUE.
           PERFORM SETON-I-M4.
 
       SETON-I-M4 SECTION.
           SET I-M4                        TO TRUE.
           PERFORM SETON-I-M3.
 
       SETON-I-M3 SECTION.
           SET I-M3                        TO TRUE.
           PERFORM SETON-I-M2.
 
       SETON-I-M2 SECTION.
           SET I-M2                        TO TRUE.
           PERFORM SETON-I-M1.
 
       SETON-I-M1 SECTION.
           SET I-M1                        TO TRUE.
 
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
