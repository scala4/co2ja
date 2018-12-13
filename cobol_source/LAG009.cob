       IDENTIFICATION DIVISION.
       PROGRAM-ID. LAG009R.
      **********************************************  Z-WIN-RPG2   ****
      *  DANNE SUMFILE TIL LAGERSTYRING ON-LINE  *
      ********************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: LAG009.rpg
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
           SELECT LAGSTYR
               ASSIGN TO UT-S-LAGSTYR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LAGSTYR-STATUS.
           SELECT OUTPUT-X
               ASSIGN TO UT-S-OUTPUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTPUT-X-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD LAGSTYR
               BLOCK CONTAINS 3100
               RECORD CONTAINS 31.
       01  LAGSTYR-IO-AREA.
           05  LAGSTYR-IO-AREA-X           PICTURE X(31).
       FD OUTPUT-X
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  OUTPUT-X-IO-AREA.
           05  OUTPUT-X-IO-AREA-X          PICTURE X(200).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  LAGSTYR-STATUS              PICTURE 99 VALUE 0.
           10  OUTPUT-X-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  LAGSTYR-EOF-OFF         VALUE '0'.
               88  LAGSTYR-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  LAGSTYR-READ-OFF        VALUE '0'.
               88  LAGSTYR-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  LAGSTYR-PROCESS-OFF     VALUE '0'.
               88  LAGSTYR-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  LAGSTYR-LEVEL-INIT-OFF  VALUE '0'.
               88  LAGSTYR-LEVEL-INIT      VALUE '1'.
           05  LAGSTYR-LEVEL-01.
               10  LAGSTYR-01-L2.
                   15  LAGSTYR-01-L2-FIRMA PICTURE X(3).
               10  LAGSTYR-01-L1.
                   15  LAGSTYR-01-L1-LEVR  PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
           05  LAGSTYR-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  ALF                     PICTURE X(3).
               10  SELP-IO.
                   15  SELP                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  VGR                     PICTURE X(5).
               10  RAB1-IO.
                   15  RAB1                PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  LEVP-IO.
                   15  LEVP                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  LEVR-IO.
                   15  LEVR                PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  FORSL-IO.
                   15  FORSL               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
      *
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(4).
           05  TEMPORARY-FIELDS.
               10  BESTN-IO.
                   15  BESTN               PICTURE S9(5).
               10  POSN-IO.
                   15  POSN                PICTURE S9(4).
               10  NULL3-IO.
                   15  NULL3               PICTURE S9(2)V9(1).
               10  NULL7-IO.
                   15  NULL7               PICTURE S9(7).
               10  NULL9-IO.
                   15  NULL9               PICTURE S9(7)V9(2).
               10  NULL8-IO.
                   15  NULL8               PICTURE S9(6)V9(2).
               10  NULL6-IO.
                   15  NULL6               PICTURE S9(6).
               10  PRIS3-IO.
                   15  PRIS3               PICTURE S9(7)V9(2).
               10  RAB-IO.
                   15  RAB                 PICTURE S9(2)V9(1).
               10  BEST-IO.
                   15  BEST                PICTURE S9(7).
               10  PRIS1-IO.
                   15  PRIS1               PICTURE S9(5)V9(2).
               10  PRIS2-IO.
                   15  PRIS2               PICTURE S9(7)V9(2).
               10  SUM-X-IO.
                   15  SUM-X               PICTURE S9(6)V9(2).
               10  PRISS-IO.
                   15  PRISS               PICTURE S9(9)V9(2).
               10  FORSS-IO.
                   15  FORSS               PICTURE S9(9)V9(2).
           05  EDITTING-FIELDS.
               10  XO-70P-EF.
                 15  XO-70P                PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  XO-72P-EF.
                 15  XO-72P                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-21P-EF.
                 15  XO-21P                PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  XO-62P-EF.
                 15  XO-62P                PICTURE S9(6)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-60P-EF.
                 15  XO-60P                PICTURE S9(6) USAGE
                                                       PACKED-DECIMAL.
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
 
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  LAGSTYR-PROCESS
               SET LAGSTYR-PROCESS-OFF     TO TRUE
               SET LAGSTYR-READ            TO TRUE
           END-IF
 
           IF  LAGSTYR-READ
           AND RECORD-SELECTED-OFF
               PERFORM LAGSTYR-GET
               SET LAGSTYR-READ-OFF        TO TRUE
               IF  NOT LAGSTYR-EOF
                   SET LAGSTYR-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  LAGSTYR-PROCESS
               PERFORM LAGSTYR-IDSET
           END-IF
 
           IF  LAGSTYR-PROCESS
               PERFORM LAGSTYR-CHK-LEVEL
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
 
           IF  LAGSTYR-PROCESS
               PERFORM LAGSTYR-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  LAGSTYR-PROCESS
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
               SET NOT-I-10                TO TRUE
               MOVE 0                      TO BESTN
               MOVE 0                      TO POSN
           END-IF
           IF  (I-L1)
               MOVE 0                      TO PRISS
               MOVE 0                      TO FORSS
               ADD 1                       TO BESTN
      *
           END-IF
           IF  (I-L1)
               MOVE 0                      TO NULL3
               MOVE 0                      TO NULL7
               MOVE 0                      TO NULL9
               MOVE 0                      TO NULL8
               MOVE 0                      TO NULL6
      *
           END-IF
           IF  (I-L2)
               SET NOT-I-11                TO TRUE
               IF  FIRMA = '614'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-11)
               SET NOT-I-11                TO TRUE
               IF  FIRMA = '628'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-11)
               SET NOT-I-11                TO TRUE
               IF  FIRMA = '646'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-11)
               SET NOT-I-11                TO TRUE
               IF  FIRMA = '728'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-11)
               SET NOT-I-11                TO TRUE
               IF  FIRMA = '940'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-11)
               SET NOT-I-11                TO TRUE
               IF  FIRMA = '764'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-11)
               SET NOT-I-11                TO TRUE
               IF  FIRMA = '903'
                   SET I-11                TO TRUE
               END-IF
      *  L2N11   FIRMA     COMP "905"                    11
           END-IF
           IF  (I-L2 AND NOT-I-11)
               SET NOT-I-11                TO TRUE
               IF  FIRMA = '908'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-11)
               SET NOT-I-11                TO TRUE
               IF  FIRMA = '915'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-11)
               SET NOT-I-11                TO TRUE
               IF  FIRMA = '923'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-11)
               SET NOT-I-11                TO TRUE
               IF  FIRMA = '932'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-11)
               SET NOT-I-11                TO TRUE
               IF  FIRMA = '938'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-11)
               SET NOT-I-11                TO TRUE
               IF  FIRMA = '627'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-11)
               SET NOT-I-11                TO TRUE
               IF  FIRMA = '958'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-11)
               SET NOT-I-11                TO TRUE
               IF  FIRMA = '970'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-11)
               SET NOT-I-11                TO TRUE
               IF  FIRMA = '972'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-11)
               SET NOT-I-11                TO TRUE
               IF  FIRMA = '978'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-11)
               SET NOT-I-11                TO TRUE
               IF  FIRMA = '982'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-11)
               SET NOT-I-11                TO TRUE
               IF  FIRMA = '985'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-11)
               SET NOT-I-11                TO TRUE
               IF  FIRMA = '885'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-11)
               SET NOT-I-11                TO TRUE
               IF  FIRMA = '855'
                   SET I-11                TO TRUE
               END-IF
      *
           END-IF
           IF  (I-L2)
               SET NOT-I-10                TO TRUE
               IF  FIRMA = '918'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-01)
               MOVE 0                      TO PRIS3
               ADD RAB1 TO ZERO        GIVING RAB
               ADD FORSL TO ZERO       GIVING BEST
               ADD LEVP TO ZERO        GIVING PRIS1
           END-IF
           IF  (I-01 AND I-11)
               ADD SELP TO ZERO        GIVING PRIS1
      *
           END-IF
           IF  (I-01)
               MULTIPLY PRIS1 BY BEST  GIVING PRIS3
           END-IF
           IF  (I-01 AND I-10)
               MULTIPLY RAB BY PRIS3   GIVING PRIS2
               DIVIDE PRIS2 BY 100     GIVING SUM-X
               SUBTRACT SUM-X              FROM PRIS3
      *
           END-IF
           IF  (I-01)
               ADD PRIS3                   TO PRISS
               ADD FORSL                   TO FORSS
      *
           END-IF
           .
 
       LAGSTYR-GET SECTION.
       LAGSTYR-GET-P.
           IF  LAGSTYR-EOF-OFF
               READ LAGSTYR
               AT END
                   SET LAGSTYR-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       LAGSTYR-FLDSET SECTION.
       LAGSTYR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LAGSTYR-IO-AREA (1:3)  TO FIRMA (1:3)
               MOVE LAGSTYR-IO-AREA (4:3)  TO ALF (1:3)
               MOVE LAGSTYR-IO-AREA (7:5)  TO SELP-IO
               MOVE LAGSTYR-IO-AREA (12:5) TO VGR (1:5)
               MOVE LAGSTYR-IO-AREA (17:2) TO RAB1-IO
               MOVE LAGSTYR-IO-AREA (19:5) TO LEVP-IO
               MOVE LAGSTYR-IO-AREA (24:4) TO LEVR-IO
               MOVE LAGSTYR-IO-AREA (28:4) TO FORSL-IO
           END-EVALUATE.
 
       LAGSTYR-IDSET SECTION.
       LAGSTYR-IDSET-P.
           SET I-01                        TO TRUE.
 
       LAGSTYR-CHK-LEVEL SECTION.
       LAGSTYR-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO LAGSTYR-LEVEL-01
               MOVE LAGSTYR-IO-AREA (1:3)  TO LAGSTYR-01-L2-FIRMA
               MOVE LAGSTYR-IO-AREA (24:4) TO LAGSTYR-01-L1-LEVR
               IF  LAGSTYR-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  LAGSTYR-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  LAGSTYR-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  LAGSTYR-01-L2         TO THE-PRIOR-L2
               MOVE  LAGSTYR-01-L1         TO THE-PRIOR-L1
               SET LAGSTYR-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1)
               MOVE SPACES TO OUTPUT-X-IO-AREA
               INITIALIZE OUTPUT-X-IO-AREA
               MOVE '8'                    TO OUTPUT-X-IO-AREA (1:1)
               MOVE FIRMA                  TO OUTPUT-X-IO-AREA (2:3)
               MOVE BESTN-IO               TO OUTPUT-X-IO-AREA (5:5)
               MOVE POSN-IO                TO OUTPUT-X-IO-AREA (10:4)
               MOVE NULL7                  TO XO-70P
               MOVE XO-70P-EF              TO OUTPUT-X-IO-AREA (75:4)
               MOVE NULL9                  TO XO-72P
               MOVE XO-72P-EF              TO OUTPUT-X-IO-AREA (79:5)
               MOVE NULL3                  TO XO-21P
               MOVE XO-21P-EF              TO OUTPUT-X-IO-AREA (85:2)
               MOVE NULL3                  TO XO-21P
               MOVE XO-21P-EF              TO OUTPUT-X-IO-AREA (87:2)
               MOVE NULL8                  TO XO-62P
               MOVE XO-62P-EF              TO OUTPUT-X-IO-AREA (89:5)
               MOVE NULL6                  TO XO-60P
               MOVE XO-60P-EF              TO OUTPUT-X-IO-AREA (94:4)
               MOVE PRISS                  TO XO-92P
               MOVE XO-92P-EF              TO OUTPUT-X-IO-AREA (98:6)
               INITIALIZE PRISS
               MOVE FORSS                  TO XO-92P
               MOVE XO-92P-EF              TO OUTPUT-X-IO-AREA (104:6)
               INITIALIZE FORSS
               MOVE LEVR                   TO XO-70P
               MOVE XO-70P-EF              TO OUTPUT-X-IO-AREA (110:4)
               MOVE UDATE                  TO XO-60P
               MOVE XO-60P-EF              TO OUTPUT-X-IO-AREA (142:4)
               MOVE NULL6                  TO XO-60P
               MOVE XO-60P-EF              TO OUTPUT-X-IO-AREA (146:4)
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
           MOVE 1                          TO LR-CHECK
           SET LAGSTYR-LEVEL-INIT          TO TRUE
           INITIALIZE LAGSTYR-DATA-FIELDS
           SET LAGSTYR-EOF-OFF             TO TRUE
           SET LAGSTYR-PROCESS             TO TRUE
           OPEN INPUT LAGSTYR
           OPEN OUTPUT OUTPUT-X.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE LAGSTYR
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
