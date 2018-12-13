       IDENTIFICATION DIVISION.
       PROGRAM-ID. VAR110R.
      **********************************************  Z-WIN-RPG2   ****
      *   PROGRAM VAR110                                              *
      *   OPPDATERING AV BESTILLT ANTALL OG ANTALL I REST FOR FIRMA   *
      *   923 AVDELIN 5 VALLØ                                         *
      *   INPUT FRA PROGRAM VAR090.                                   *
      *   14.04.97  STEIN SANDVOLD                                    *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VAR110.rpg
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
           SELECT UPDFILE
               ASSIGN TO UT-S-UPDFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS UPDFILE-STATUS.
           SELECT VARETIL
               ASSIGN TO VARETIL
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VARETIL-STATUS
               RECORD KEY IS VARETIL-KEY1.
       DATA DIVISION.
       FILE SECTION.
       FD UPDFILE
               BLOCK CONTAINS 4000
               RECORD CONTAINS 40.
       01  UPDFILE-IO-AREA.
           05  UPDFILE-IO-AREA-X           PICTURE X(40).
       FD VARETIL
               RECORD CONTAINS 200.
       01  VARETIL-IO-AREA.
           05  VARETIL-IO-AREA-X.
               10  VARETIL-KEY1            PICTURE X(12).
               10  FILLER                  PICTURE X(188).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  UPDFILE-STATUS              PICTURE 99 VALUE 0.
           10  VARETIL-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  UPDFILE-EOF-OFF         VALUE '0'.
               88  UPDFILE-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  UPDFILE-READ-OFF        VALUE '0'.
               88  UPDFILE-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  UPDFILE-PROCESS-OFF     VALUE '0'.
               88  UPDFILE-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  UPDFILE-LEVEL-INIT-OFF  VALUE '0'.
               88  UPDFILE-LEVEL-INIT      VALUE '1'.
           05  VARETIL-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  UPDFILE-LEVEL-01.
               10  UPDFILE-01-L2.
                   15  UPDFILE-01-L2-FIRMA PICTURE X(3).
               10  UPDFILE-01-L1.
                   15  UPDFILE-01-L1-EDBNR PICTURE X(7).
           05  UPDFILE-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  EDBNR                   PICTURE X(7).
               10  KEY-X                   PICTURE X(10).
               10  IREST2-IO.
                   15  IREST2              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  IBEST2-IO.
                   15  IBEST2              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  IREST3-IO.
                   15  IREST3              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  IBEST3-IO.
                   15  IBEST3              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  IREST4-IO.
                   15  IREST4              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  IBEST4-IO.
                   15  IBEST4              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  IREST5-IO.
                   15  IREST5              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  IBEST5-IO.
                   15  IBEST5              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
           05  VARETIL-DATA-FIELDS.
      *****************************************************************
      * NULLSTILLING AV ALLE FELT VED NYTT EDB-NR.                    *
      *****************************************************************
               10  FILLER                  PICTURE X.
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(7).
           05  TEMPORARY-FIELDS.
               10  VREST2-IO.
                   15  VREST2              PICTURE S9(5).
               10  VBEST2-IO.
                   15  VBEST2              PICTURE S9(5).
               10  VREST3-IO.
                   15  VREST3              PICTURE S9(5).
               10  VBEST3-IO.
                   15  VBEST3              PICTURE S9(5).
               10  VREST4-IO.
                   15  VREST4              PICTURE S9(5).
               10  VBEST4-IO.
                   15  VBEST4              PICTURE S9(5).
               10  VREST5-IO.
                   15  VREST5              PICTURE S9(5).
               10  VBEST5-IO.
                   15  VBEST5              PICTURE S9(5).
               10  IREST2-N-IO.
                   15  IREST2-N            PICTURE S9(5).
               10  IBEST2-N-IO.
                   15  IBEST2-N            PICTURE S9(5).
               10  IREST3-N-IO.
                   15  IREST3-N            PICTURE S9(5).
               10  IBEST3-N-IO.
                   15  IBEST3-N            PICTURE S9(5).
               10  IREST4-N-IO.
                   15  IREST4-N            PICTURE S9(5).
               10  IBEST4-N-IO.
                   15  IBEST4-N            PICTURE S9(5).
               10  IREST5-N-IO.
                   15  IREST5-N            PICTURE S9(5).
               10  IBEST5-N-IO.
                   15  IBEST5-N            PICTURE S9(5).
               10  NULL5-IO.
                   15  NULL5               PICTURE S9(5).
               10  KEY12                   PICTURE X(12).
           05  EDITTING-FIELDS.
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
           IF  UPDFILE-PROCESS
               SET UPDFILE-PROCESS-OFF     TO TRUE
               SET UPDFILE-READ            TO TRUE
           END-IF
 
           IF  UPDFILE-READ
           AND RECORD-SELECTED-OFF
               PERFORM UPDFILE-GET
               SET UPDFILE-READ-OFF        TO TRUE
               IF  NOT UPDFILE-EOF
                   SET UPDFILE-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  UPDFILE-PROCESS
               PERFORM UPDFILE-IDSET
           END-IF
 
           IF  UPDFILE-PROCESS
               PERFORM UPDFILE-CHK-LEVEL
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
 
           IF  UPDFILE-PROCESS
               PERFORM UPDFILE-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  UPDFILE-PROCESS
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
               SET NOT-I-30                TO TRUE
               IF  FIRMA = '923'
                   SET I-30                TO TRUE
               END-IF
               SET NOT-I-31                TO TRUE
               IF  FIRMA = '658'
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (I-L1)
               MOVE 0                      TO VREST2
               MOVE 0                      TO VBEST2
               MOVE 0                      TO VREST3
               MOVE 0                      TO VBEST3
               MOVE 0                      TO VREST4
               MOVE 0                      TO VBEST4
               MOVE 0                      TO VREST5
               MOVE 0                      TO VBEST5
      *****************************************************************
      * FLYTTE DATA TIL VAREMASTER.                                   *
      *****************************************************************
           END-IF
           IF  (I-01)
               MOVE IREST2                 TO IREST2-N
               MOVE IREST2-N-IO            TO VREST2-IO
               MOVE IBEST2                 TO IBEST2-N
               MOVE IBEST2-N-IO            TO VBEST2-IO
               MOVE IREST3                 TO IREST3-N
               MOVE IREST3-N-IO            TO VREST3-IO
               MOVE IBEST3                 TO IBEST3-N
               MOVE IBEST3-N-IO            TO VBEST3-IO
               MOVE IREST4                 TO IREST4-N
               MOVE IREST4-N-IO            TO VREST4-IO
               MOVE IBEST4                 TO IBEST4-N
               MOVE IBEST4-N-IO            TO VBEST4-IO
               MOVE IREST5                 TO IREST5-N
               MOVE IREST5-N-IO            TO VREST5-IO
               MOVE IBEST5                 TO IBEST5-N
               MOVE IBEST5-N-IO            TO VBEST5-IO
               SET NOT-I-25                TO TRUE
               IF  VREST2 > 0
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-25)
               SET NOT-I-25                TO TRUE
               IF  VREST3 > 0
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-25)
               SET NOT-I-25                TO TRUE
               IF  VREST4 > 0
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-25)
               SET NOT-I-25                TO TRUE
               IF  VREST5 > 0
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-25)
               SET NOT-I-25                TO TRUE
               IF  VBEST2 > 0
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-25)
               SET NOT-I-25                TO TRUE
               IF  VBEST3 > 0
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-25)
               SET NOT-I-25                TO TRUE
               IF  VBEST4 > 0
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-25)
               SET NOT-I-25                TO TRUE
               IF  VBEST5 > 0
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-25)
               MOVE 0                      TO NULL5
           END-IF
           IF  (I-01)
               MOVE '81'                   TO KEY12 (1:2)
               MOVE KEY-X                  TO KEY12 (3:10)
               MOVE KEY12                  TO VARETIL-KEY1
               READ VARETIL RECORD KEY IS VARETIL-KEY1
               INVALID KEY
                   SET I-40                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-40            TO TRUE
                   PERFORM VARETIL-IDSET
               END-READ
           END-IF.
 
       UPDFILE-GET SECTION.
       UPDFILE-GET-P.
           IF  UPDFILE-EOF-OFF
               READ UPDFILE
               AT END
                   SET UPDFILE-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       UPDFILE-FLDSET SECTION.
       UPDFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE UPDFILE-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE UPDFILE-IO-AREA (6:7)  TO EDBNR (1:7)
               MOVE UPDFILE-IO-AREA (3:10) TO KEY-X (1:10)
               MOVE UPDFILE-IO-AREA (13:3) TO IREST2-IO
               MOVE UPDFILE-IO-AREA (16:3) TO IBEST2-IO
               MOVE UPDFILE-IO-AREA (19:3) TO IREST3-IO
               MOVE UPDFILE-IO-AREA (22:3) TO IBEST3-IO
               MOVE UPDFILE-IO-AREA (25:3) TO IREST4-IO
               MOVE UPDFILE-IO-AREA (28:3) TO IBEST4-IO
               MOVE UPDFILE-IO-AREA (31:3) TO IREST5-IO
               MOVE UPDFILE-IO-AREA (34:3) TO IBEST5-IO
           END-EVALUATE.
 
       UPDFILE-IDSET SECTION.
       UPDFILE-IDSET-P.
           SET I-01                        TO TRUE.
 
       UPDFILE-CHK-LEVEL SECTION.
       UPDFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO UPDFILE-LEVEL-01
               MOVE UPDFILE-IO-AREA (3:3)  TO UPDFILE-01-L2-FIRMA
               MOVE UPDFILE-IO-AREA (6:7)  TO UPDFILE-01-L1-EDBNR
               IF  UPDFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  UPDFILE-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  UPDFILE-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  UPDFILE-01-L2         TO THE-PRIOR-L2
               MOVE  UPDFILE-01-L1         TO THE-PRIOR-L1
               SET UPDFILE-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       VARETIL-IDSET SECTION.
       VARETIL-IDSET-P.
           SET I-02                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND NOT-I-40)
               MOVE VREST2                 TO XO-50P
               MOVE XO-50P-EF              TO VARETIL-IO-AREA (22:3)
               MOVE VBEST2                 TO XO-50P
               MOVE XO-50P-EF              TO VARETIL-IO-AREA (25:3)
               IF  (I-30)
                   MOVE VREST5             TO XO-50P
                   MOVE XO-50P-EF          TO VARETIL-IO-AREA (72:3)
               END-IF
               IF  (I-30)
                   MOVE VBEST5             TO XO-50P
                   MOVE XO-50P-EF          TO VARETIL-IO-AREA (75:3)
               END-IF
               IF  (I-31)
                   MOVE VREST4             TO XO-50P
                   MOVE XO-50P-EF          TO VARETIL-IO-AREA (72:3)
               END-IF
               IF  (I-31)
                   MOVE VBEST4             TO XO-50P
                   MOVE XO-50P-EF          TO VARETIL-IO-AREA (75:3)
               END-IF
               REWRITE VARETIL-IO-AREA
                   INVALID KEY
                       DISPLAY 'Bad REWRITE - file = VARETIL'
               END-REWRITE
           END-IF
           IF  (I-01 AND I-40 AND I-25)
               MOVE '81'                   TO VARETIL-IO-AREA (1:2)
               MOVE KEY-X                  TO VARETIL-IO-AREA (3:10)
               MOVE '      '               TO VARETIL-IO-AREA (13:6)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO VARETIL-IO-AREA (19:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO VARETIL-IO-AREA (22:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO VARETIL-IO-AREA (25:3)
               MOVE VREST2                 TO XO-50P
               MOVE XO-50P-EF              TO VARETIL-IO-AREA (22:3)
               MOVE VBEST2                 TO XO-50P
               MOVE XO-50P-EF              TO VARETIL-IO-AREA (25:3)
               MOVE '          '           TO VARETIL-IO-AREA (28:10)
               MOVE '      '               TO VARETIL-IO-AREA (38:6)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO VARETIL-IO-AREA (44:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO VARETIL-IO-AREA (47:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO VARETIL-IO-AREA (50:3)
               MOVE '          '           TO VARETIL-IO-AREA (53:10)
               MOVE '      '               TO VARETIL-IO-AREA (63:6)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO VARETIL-IO-AREA (69:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO VARETIL-IO-AREA (72:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO VARETIL-IO-AREA (75:3)
               IF  (I-30)
                   MOVE VREST5             TO XO-50P
                   MOVE XO-50P-EF          TO VARETIL-IO-AREA (72:3)
               END-IF
               IF  (I-30)
                   MOVE VBEST5             TO XO-50P
                   MOVE XO-50P-EF          TO VARETIL-IO-AREA (75:3)
               END-IF
               IF  (I-31)
                   MOVE VREST4             TO XO-50P
                   MOVE XO-50P-EF          TO VARETIL-IO-AREA (72:3)
               END-IF
               IF  (I-31)
                   MOVE VBEST4             TO XO-50P
                   MOVE XO-50P-EF          TO VARETIL-IO-AREA (75:3)
               END-IF
               MOVE '          '           TO VARETIL-IO-AREA (78:10)
               MOVE '      '               TO VARETIL-IO-AREA (88:6)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO VARETIL-IO-AREA (94:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO VARETIL-IO-AREA (97:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO VARETIL-IO-AREA (100:3)
               IF  (NOT-I-31)
                   MOVE VREST4             TO XO-50P
                   MOVE XO-50P-EF          TO VARETIL-IO-AREA (97:3)
               END-IF
               IF  (NOT-I-31)
                   MOVE VBEST4             TO XO-50P
                   MOVE XO-50P-EF          TO VARETIL-IO-AREA (100:3)
               END-IF
               MOVE '          '           TO VARETIL-IO-AREA (103:10)
               MOVE '      '               TO VARETIL-IO-AREA (113:6)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO VARETIL-IO-AREA (119:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO VARETIL-IO-AREA (122:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO VARETIL-IO-AREA (125:3)
               MOVE '          '           TO VARETIL-IO-AREA (128:10)
               MOVE '      '               TO VARETIL-IO-AREA (138:6)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO VARETIL-IO-AREA (144:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO VARETIL-IO-AREA (147:3)
               MOVE NULL5                  TO XO-50P
               MOVE XO-50P-EF              TO VARETIL-IO-AREA (150:3)
               MOVE '          '           TO VARETIL-IO-AREA (153:10)
               MOVE '                    ' TO VARETIL-IO-AREA (163:20)
               MOVE '                  '   TO VARETIL-IO-AREA (183:18)
               WRITE VARETIL-IO-AREA
                   INVALID KEY
                       DISPLAY 'Bad WRITE - file = VARETIL'
               END-WRITE
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
           SET UPDFILE-LEVEL-INIT          TO TRUE
           INITIALIZE UPDFILE-DATA-FIELDS
           SET UPDFILE-EOF-OFF             TO TRUE
           SET UPDFILE-PROCESS             TO TRUE
           OPEN INPUT UPDFILE
           OPEN I-O VARETIL.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE UPDFILE
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
