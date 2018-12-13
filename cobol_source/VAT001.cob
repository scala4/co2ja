       IDENTIFICATION DIVISION.
       PROGRAM-ID. VAT001R.
      ******************************************* :   Z-WIN-RPG2P    **
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VAT001.rpg
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
           SELECT KOPIUT
               ASSIGN TO UT-S-KOPIUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KOPIUT-STATUS.
           SELECT VARETIL
               ASSIGN TO VARETIL
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VARETIL-STATUS
               RECORD KEY IS VARETIL-KEY1.
       DATA DIVISION.
       FILE SECTION.
       FD KOPIUT
               BLOCK CONTAINS 9600
               RECORD CONTAINS 200.
       01  KOPIUT-IO-AREA.
           05  KOPIUT-IO-AREA-X            PICTURE X(200).
       FD VARETIL
               RECORD CONTAINS 200.
       01  VARETIL-IO-AREA.
           05  VARETIL-IO-AREA-X.
               10  VARETIL-KEY1            PICTURE X(12).
               10  FILLER                  PICTURE X(188).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  KOPIUT-STATUS               PICTURE 99 VALUE 0.
           10  VARETIL-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  KOPIUT-EOF-OFF          VALUE '0'.
               88  KOPIUT-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KOPIUT-READ-OFF         VALUE '0'.
               88  KOPIUT-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KOPIUT-PROCESS-OFF      VALUE '0'.
               88  KOPIUT-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  KOPIUT-LEVEL-INIT-OFF   VALUE '0'.
               88  KOPIUT-LEVEL-INIT       VALUE '1'.
           05  VARETIL-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  KOPIUT-LEVEL-01.
               10  KOPIUT-01-L1.
                   15  KOPIUT-01-L1-FIRMA  PICTURE X(3).
           05  KOPIUT-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  EDBNR                   PICTURE X(7).
               10  SVS-IO.
                   15  SVS                 PICTURE S9(7)V9(2).
               10  ANTI-IO.
                   15  ANTI                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ANTU-IO.
                   15  ANTU                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  LAG13-IO.
                   15  LAG13               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  LAG93-IO.
                   15  LAG93               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  LAG15-IO.
                   15  LAG15               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  LAG17-IO.
                   15  LAG17               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  LAG92-IO.
                   15  LAG92               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  LAG18-IO.
                   15  LAG18               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
           05  VARETIL-DATA-FIELDS.
               10  FILLER                  PICTURE X.
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  KEY1                    PICTURE X(5).
               10  KEY-X                   PICTURE X(12).
               10  NULL9-IO.
                   15  NULL9               PICTURE S9(7)V9(2).
               10  NULL5-IO.
                   15  NULL5               PICTURE S9(5).
               10  ANTKOR-IO.
                   15  ANTKOR              PICTURE S9(5).
               10  ANTNYE-IO.
                   15  ANTNYE              PICTURE S9(5).
           05  EDITTING-FIELDS.
               10  XO-72P-EF.
                 15  XO-72P                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-50P-EF.
                 15  XO-50P                PICTURE S9(5) USAGE
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
           IF  KOPIUT-PROCESS
               SET KOPIUT-PROCESS-OFF      TO TRUE
               SET KOPIUT-READ             TO TRUE
           END-IF
 
           IF  KOPIUT-READ
           AND RECORD-SELECTED-OFF
               PERFORM KOPIUT-GET
               SET KOPIUT-READ-OFF         TO TRUE
               IF  NOT KOPIUT-EOF
                   SET KOPIUT-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  KOPIUT-PROCESS
               PERFORM KOPIUT-IDSET
           END-IF
 
           IF  KOPIUT-PROCESS
               PERFORM KOPIUT-CHK-LEVEL
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
 
           IF  KOPIUT-PROCESS
               PERFORM KOPIUT-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  KOPIUT-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           MOVE '80'                       TO KEY1 (1:2)
           MOVE FIRMA                      TO KEY1 (3:3)
           MOVE KEY1                       TO KEY-X (1:5)
           MOVE EDBNR                      TO KEY-X (6:7)
           MOVE KEY-X                      TO VARETIL-KEY1
           READ VARETIL RECORD KEY IS VARETIL-KEY1
           INVALID KEY
               SET I-16                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-16                TO TRUE
               PERFORM VARETIL-IDSET
           END-READ
           MOVE 0                          TO NULL9
           MOVE 0                          TO NULL5
           IF  (NOT-I-16)
               ADD 1                       TO ANTKOR
           END-IF
           IF  (I-16)
               ADD 1                       TO ANTNYE
           END-IF.
 
       KOPIUT-GET SECTION.
       KOPIUT-GET-P.
           IF  KOPIUT-EOF-OFF
               READ KOPIUT
               AT END
                   SET KOPIUT-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KOPIUT-FLDSET SECTION.
       KOPIUT-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KOPIUT-IO-AREA (3:3)   TO FIRMA (1:3)
               MOVE KOPIUT-IO-AREA (6:7)   TO EDBNR (1:7)
               MOVE KOPIUT-IO-AREA (66:9)  TO SVS-IO
               INSPECT SVS-IO REPLACING ALL ' ' BY '0'
               MOVE KOPIUT-IO-AREA (97:5)  TO ANTI-IO
               MOVE KOPIUT-IO-AREA (102:5) TO ANTU-IO
               MOVE KOPIUT-IO-AREA (179:3) TO LAG13-IO
               MOVE KOPIUT-IO-AREA (182:3) TO LAG93-IO
               MOVE KOPIUT-IO-AREA (185:3) TO LAG15-IO
               MOVE KOPIUT-IO-AREA (188:3) TO LAG17-IO
               MOVE KOPIUT-IO-AREA (191:3) TO LAG92-IO
               MOVE KOPIUT-IO-AREA (194:3) TO LAG18-IO
           END-EVALUATE.
 
       KOPIUT-IDSET SECTION.
       KOPIUT-IDSET-P.
           SET I-01                        TO TRUE.
 
       KOPIUT-CHK-LEVEL SECTION.
       KOPIUT-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO KOPIUT-LEVEL-01
               MOVE KOPIUT-IO-AREA (3:3)   TO KOPIUT-01-L1-FIRMA
               IF  KOPIUT-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  KOPIUT-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  KOPIUT-01-L1          TO THE-PRIOR-L1
               SET KOPIUT-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       VARETIL-IDSET SECTION.
       VARETIL-IDSET-P.
           SET I-02                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND NOT-I-U1)
           AND (NOT-I-16)
      *                        UDATE     79P
               MOVE ANTI                   TO XO-72P
               MOVE XO-72P-EF              TO VARETIL-IO-AREA (83:5)
               MOVE ANTU                   TO XO-72P
               MOVE XO-72P-EF              TO VARETIL-IO-AREA (88:5)
               MOVE NULL9                  TO XO-72P
               MOVE XO-72P-EF              TO VARETIL-IO-AREA (93:5)
               MOVE LAG13                  TO XO-50P
               MOVE XO-50P-EF              TO VARETIL-IO-AREA (98:3)
               MOVE LAG93                  TO XO-50P
               MOVE XO-50P-EF              TO VARETIL-IO-AREA (101:3)
               MOVE LAG15                  TO XO-50P
               MOVE XO-50P-EF              TO VARETIL-IO-AREA (104:3)
               MOVE LAG17                  TO XO-50P
               MOVE XO-50P-EF              TO VARETIL-IO-AREA (107:3)
               MOVE LAG92                  TO XO-50P
               MOVE XO-50P-EF              TO VARETIL-IO-AREA (110:3)
               MOVE LAG18                  TO XO-50P
               MOVE XO-50P-EF              TO VARETIL-IO-AREA (113:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO VARETIL-IO-AREA (116:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO VARETIL-IO-AREA (119:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO VARETIL-IO-AREA (122:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO VARETIL-IO-AREA (125:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO VARETIL-IO-AREA (128:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO VARETIL-IO-AREA (131:3)
               MOVE SVS                    TO XO-72P
               MOVE XO-72P-EF              TO VARETIL-IO-AREA (134:5)
               REWRITE VARETIL-IO-AREA
                   INVALID KEY
                       DISPLAY 'Bad REWRITE - file = VARETIL'
               END-REWRITE
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
           SET KOPIUT-LEVEL-INIT           TO TRUE
           INITIALIZE KOPIUT-DATA-FIELDS
           SET KOPIUT-EOF-OFF              TO TRUE
           SET KOPIUT-PROCESS              TO TRUE
           OPEN INPUT KOPIUT
           OPEN I-O VARETIL.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE KOPIUT
           CLOSE VARETIL.
 
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
