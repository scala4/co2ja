       IDENTIFICATION DIVISION.
       PROGRAM-ID. VAR111R.
      **********************************************  Z-WIN-RPG2   ****
      *   PROGRAM VAR111                                              *
      *   NULLSTILLE AV BESTILLT ANTALL OG ANTALL I REST FOR FIRMA    *
      *   923 AVDELIN 5 VALLØ                                         *
      *   INPUT VARETIL.                                              *
      *   04.07.97  ELIN                                              *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VAR111.rpg
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
           SELECT ENRLIM
               ASSIGN TO ENRLIM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ENRLIM-STATUS.
           SELECT VARETIL
               ASSIGN TO VARETIL
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VARETIL-STATUS
               RECORD KEY IS VARETIL-KEY1.
       DATA DIVISION.
       FILE SECTION.
       FD ENRLIM
               RECORD CONTAINS 80.
       01  ENRLIM-IO-AREA.
           05  ENRLIM-IO-AREA-X.
               10  ENRLIM-KEY1.
                   15  ENRLIM-KEY1N        PICTURE S9(12).
               10  FILLER                  PICTURE X(68).
      *ISTE   O   F 132 132     OF     PRINTERSYSLST
       FD VARETIL
               RECORD CONTAINS 200.
       01  VARETIL-IO-AREA.
           05  VARETIL-IO-AREA-X.
               10  VARETIL-KEY1            PICTURE X(12).
               10  FILLER                  PICTURE X(188).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  ENRLIM-STATUS               PICTURE 99 VALUE 0.
           10  VARETIL-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  ENRLIM-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  ENRLIM-EOF-OFF          VALUE '0'.
               88  ENRLIM-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ENRLIM-READ-OFF         VALUE '0'.
               88  ENRLIM-READ             VALUE '1'.
           05  ENRLIM-LOW-KEY              PICTURE X(12).
           05  ENRLIM-HIGH-KEY             PICTURE X(12).
           05  VARETIL-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  VARETIL-EOF-OFF         VALUE '0'.
               88  VARETIL-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VARETIL-READ-OFF        VALUE '0'.
               88  VARETIL-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VARETIL-PROCESS-OFF     VALUE '0'.
               88  VARETIL-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  VARETIL-LEVEL-INIT-OFF  VALUE '0'.
               88  VARETIL-LEVEL-INIT      VALUE '1'.
           05  VARETIL-LEVEL-02.
               10  VARETIL-02-L2.
                   15  VARETIL-02-L2-FIRMA PICTURE X(3).
               10  VARETIL-02-L1.
                   15  VARETIL-02-L1-EDBNR PICTURE X(7).
           05  VARETIL-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  EDBNR                   PICTURE X(7).
      *****************************************************************
      * NULLSTILLING AV ANT IBEST OG ANT IREST                      *
      *****************************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(7).
           05  TEMPORARY-FIELDS.
               10  VREST-IO.
                   15  VREST               PICTURE S9(5).
               10  VBEST-IO.
                   15  VBEST               PICTURE S9(5).
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
           SET NOT-I-02                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  VARETIL-PROCESS
               SET VARETIL-PROCESS-OFF     TO TRUE
               SET VARETIL-READ            TO TRUE
           END-IF
 
           IF  VARETIL-READ
           AND RECORD-SELECTED-OFF
               PERFORM VARETIL-GET
               SET VARETIL-READ-OFF        TO TRUE
               IF  NOT VARETIL-EOF
                   SET VARETIL-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  VARETIL-PROCESS
               PERFORM VARETIL-IDSET
           END-IF
 
           IF  VARETIL-PROCESS
               PERFORM VARETIL-CHK-LEVEL
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
 
           IF  VARETIL-PROCESS
               PERFORM VARETIL-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  VARETIL-PROCESS
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
               MOVE 0                      TO VREST
               MOVE 0                      TO VBEST
           END-IF
           SET NOT-I-20                    TO TRUE
           IF  (I-L2)
               SET NOT-I-10                TO TRUE
               IF  FIRMA = '626'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  FIRMA = '658'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  FIRMA = '923'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  FIRMA = '732'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-10)
               SET NOT-I-LR                TO TRUE
               SET NOT-I-10                TO TRUE
               IF  FIRMA > '956'
                   SET I-LR                TO TRUE
               END-IF
               IF  FIRMA = '956'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10)
               GO TO SLUTT-T
           END-IF
           SET I-20                        TO TRUE.
 
       SLUTT-T.
           CONTINUE.
 
       VARETIL-GET SECTION.
       VARETIL-GET-P.
           IF  VARETIL-EOF-OFF
               PERFORM WITH TEST AFTER
                 UNTIL ENRLIM-READ-OFF
                    OR ENRLIM-EOF
                   IF  ENRLIM-READ
                       SET ENRLIM-READ-OFF TO TRUE
                       READ ENRLIM
                       AT END
                           SET ENRLIM-EOF  TO TRUE
                           SET VARETIL-EOF TO TRUE
                           SUBTRACT 1    FROM LR-CHECK
                       NOT AT END
                           MOVE ENRLIM-IO-AREA (1:4) TO VARETIL-KEY1
                       END-READ
                   END-IF
                   IF  ENRLIM-EOF-OFF
                   AND ENRLIM-READ-OFF
                       READ VARETIL
                       INVALID KEY
                           SET I-H0        TO TRUE
                           MOVE 'N'        TO E-R-R-O-R
                       END-READ
                   END-IF
               END-PERFORM
           END-IF.
 
       VARETIL-FLDSET SECTION.
       VARETIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VARETIL-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE VARETIL-IO-AREA (6:7)  TO EDBNR (1:7)
           END-EVALUATE.
 
       VARETIL-IDSET SECTION.
       VARETIL-IDSET-P.
           SET I-02                        TO TRUE.
 
       VARETIL-CHK-LEVEL SECTION.
       VARETIL-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO VARETIL-LEVEL-02
               MOVE VARETIL-IO-AREA (3:3)  TO VARETIL-02-L2-FIRMA
               MOVE VARETIL-IO-AREA (6:7)  TO VARETIL-02-L1-EDBNR
               IF  VARETIL-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VARETIL-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  VARETIL-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  VARETIL-02-L2         TO THE-PRIOR-L2
               MOVE  VARETIL-02-L1         TO THE-PRIOR-L1
               SET VARETIL-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND I-20)
               MOVE VREST                  TO XO-50P
               MOVE XO-50P-EF              TO VARETIL-IO-AREA (22:3)
               MOVE VBEST                  TO XO-50P
               MOVE XO-50P-EF              TO VARETIL-IO-AREA (25:3)
               MOVE VREST                  TO XO-50P
               MOVE XO-50P-EF              TO VARETIL-IO-AREA (72:3)
               MOVE VBEST                  TO XO-50P
               MOVE XO-50P-EF              TO VARETIL-IO-AREA (75:3)
               MOVE VREST                  TO XO-50P
               MOVE XO-50P-EF              TO VARETIL-IO-AREA (97:3)
               MOVE VBEST                  TO XO-50P
               MOVE XO-50P-EF              TO VARETIL-IO-AREA (100:3)
      *ISTE   D  101   1P
      *      OR        OF
      *                                  48 "NULLSTILLE VALLØ       "
      *       D  1     1P
      *      OR        OF
      *                                  48 "-----------------------"
      *       D  1     02 20
      *                        FIRMA      3
      *                        EDBNR     15
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
           SET ENRLIM-EOF-OFF              TO TRUE
           SET ENRLIM-READ                 TO TRUE
           OPEN INPUT ENRLIM
           SET VARETIL-LEVEL-INIT          TO TRUE
           INITIALIZE VARETIL-DATA-FIELDS
           SET VARETIL-EOF-OFF             TO TRUE
           SET VARETIL-PROCESS             TO TRUE
           OPEN I-O VARETIL.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE ENRLIM
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
