       IDENTIFICATION DIVISION.
       PROGRAM-ID. LAG007R.
      **********************************************  Z-WIN-RPG2   ****
      *  DANNE SUMFILE TIL LAGERSTYRING ON-LINE  *
      ********************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: LAG007.rpg
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
       DATA DIVISION.
       FILE SECTION.
       FD LAGSTYR
               BLOCK CONTAINS 9000
               RECORD CONTAINS 200.
       01  LAGSTYR-IO-AREA.
           05  LAGSTYR-IO-AREA-X           PICTURE X(200).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  LAGSTYR-STATUS              PICTURE 99 VALUE 0.
 
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
               10  RECART                  PICTURE X(1).
               10  FIRMA                   PICTURE X(3).
               10  BESTN                   PICTURE X(5).
               10  POSN                    PICTURE X(4).
               10  LEVR-IO.
                   15  LEVR                PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  REC                     PICTURE X(200).
      *
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(4).
           05  TEMPORARY-FIELDS.
               10  BEST-IO.
                   15  BEST                PICTURE S9(5).
               10  POS-X-IO.
                   15  POS-X               PICTURE S9(4).
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
           CONTINUE.
 
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
               MOVE 0                      TO BEST
               MOVE 0                      TO POS-X
           END-IF
           IF  (I-L1)
               ADD 1                       TO BEST
           END-IF.
 
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
               MOVE LAGSTYR-IO-AREA (1:1)  TO RECART (1:1)
               MOVE LAGSTYR-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE LAGSTYR-IO-AREA (5:5)  TO BESTN (1:5)
               MOVE LAGSTYR-IO-AREA (10:4) TO POSN (1:4)
               MOVE LAGSTYR-IO-AREA (110:4) TO LEVR-IO
               MOVE LAGSTYR-IO-AREA (1:200) TO REC (1:200)
           END-EVALUATE.
 
       LAGSTYR-IDSET SECTION.
       LAGSTYR-IDSET-P.
           SET I-01                        TO TRUE.
 
       LAGSTYR-CHK-LEVEL SECTION.
       LAGSTYR-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO LAGSTYR-LEVEL-01
               MOVE LAGSTYR-IO-AREA (2:3)  TO LAGSTYR-01-L2-FIRMA
               MOVE LAGSTYR-IO-AREA (110:4) TO LAGSTYR-01-L1-LEVR
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
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               MOVE REC                    TO LAGSTYR-IO-AREA (1:200)
               MOVE BEST-IO                TO LAGSTYR-IO-AREA (5:5)
               MOVE POS-X-IO               TO LAGSTYR-IO-AREA (10:4)
               REWRITE LAGSTYR-IO-AREA
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
           OPEN I-O LAGSTYR.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE LAGSTYR.
 
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
