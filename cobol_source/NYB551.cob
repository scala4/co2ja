       IDENTIFICATION DIVISION.
       PROGRAM-ID. NYB551R.
      **********************************************  Z-WIN-RPG2   ****
      *            REORG AV BESTILLINGSMASTER         *
      *                       STEIN SANDVOLD 02.10.89.*
      *************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: NYB551.rpg
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
           SELECT BESTILL
               ASSIGN TO UT-S-BESTILL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS BESTILL-STATUS.
           SELECT NYEBES
               ASSIGN TO UT-S-NYEBES
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS NYEBES-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD BESTILL
               BLOCK CONTAINS 4050
               RECORD CONTAINS 150.
       01  BESTILL-IO-AREA.
           05  BESTILL-IO-AREA-X           PICTURE X(150).
       FD NYEBES
               BLOCK CONTAINS 4050
               RECORD CONTAINS 150.
       01  NYEBES-IO-AREA.
           05  NYEBES-IO-AREA-X            PICTURE X(150).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  BESTILL-STATUS              PICTURE 99 VALUE 0.
           10  NYEBES-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  BESTILL-EOF-OFF         VALUE '0'.
               88  BESTILL-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BESTILL-READ-OFF        VALUE '0'.
               88  BESTILL-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BESTILL-PROCESS-OFF     VALUE '0'.
               88  BESTILL-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  BESTILL-LEVEL-INIT-OFF  VALUE '0'.
               88  BESTILL-LEVEL-INIT      VALUE '1'.
           05  BESTILL-LEVEL-01.
               10  BESTILL-01-L2.
                   15  BESTILL-01-L2-FNR   PICTURE X(3).
               10  BESTILL-01-L1.
                   15  BESTILL-01-L1-BNR   PICTURE X(5).
           05  BESTILL-DATA-FIELDS.
               10  REC                     PICTURE X(150).
               10  KEY-X                   PICTURE X(16).
               10  FNR                     PICTURE X(3).
               10  BNR                     PICTURE X(5).
               10  ANTAL-IO.
                   15  ANTAL               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  RA                      PICTURE X(1).
               10  TEKST                   PICTURE X(1).
               10  LIST                    PICTURE X(50).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(5).
           05  TEMPORARY-FIELDS.
               10  ANTD-IO.
                   15  ANTD                PICTURE S9(6).
               10  ANT-IO.
                   15  ANT                 PICTURE S9(6).
               10  ANTS-IO.
                   15  ANTS                PICTURE S9(6).
               10  ANTN-IO.
                   15  ANTN                PICTURE S9(6).
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
           IF  BESTILL-PROCESS
               SET BESTILL-PROCESS-OFF     TO TRUE
               SET BESTILL-READ            TO TRUE
           END-IF
 
           IF  BESTILL-READ
           AND RECORD-SELECTED-OFF
               PERFORM BESTILL-GET
               SET BESTILL-READ-OFF        TO TRUE
               IF  NOT BESTILL-EOF
                   PERFORM BESTILL-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET BESTILL-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  BESTILL-PROCESS
               PERFORM BESTILL-IDSET
           END-IF
 
           IF  BESTILL-PROCESS
               PERFORM BESTILL-CHK-LEVEL
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
 
           IF  BESTILL-PROCESS
               PERFORM BESTILL-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  BESTILL-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-11                    TO TRUE
           SET NOT-I-12                    TO TRUE
           SET NOT-I-13                    TO TRUE
           SET NOT-I-14                    TO TRUE
           SET NOT-I-20                    TO TRUE
           IF  (I-L1)
               SET NOT-I-15                TO TRUE
               MOVE 0                      TO ANTD
           END-IF
           ADD 1                           TO ANT
           IF  (I-02)
               ADD 1                       TO ANTS
               GO TO SLUTT-T
           END-IF
           SET NOT-I-11                    TO TRUE
           IF  RA = '1'
               SET I-11                    TO TRUE
           END-IF
           SET NOT-I-12                    TO TRUE
           IF  RA = '2'
               SET I-12                    TO TRUE
           END-IF
           SET NOT-I-13                    TO TRUE
           IF  RA = '3'
               SET I-13                    TO TRUE
           END-IF
           SET NOT-I-14                    TO TRUE
           IF  RA = '4'
               SET I-14                    TO TRUE
           END-IF
           IF  (NOT-I-14)
               SET NOT-I-14                TO TRUE
               IF  RA = '5'
                   SET I-14                TO TRUE
               END-IF
      *
      *
           END-IF
           IF  (I-14)
               SET NOT-I-16                TO TRUE
               IF  TEKST = ' '
                   SET I-16                TO TRUE
               END-IF
           END-IF
           SET NOT-I-17                    TO TRUE
           IF  ANTD = 0
               SET I-17                    TO TRUE
           END-IF
      *
      *
           IF  (I-11 AND I-17)
               OR  (I-12 AND I-17)
               OR  (I-13 AND I-17)
               OR  (NOT-I-16 AND I-17 AND I-14)
               ADD 1                       TO ANTS
               GO TO SLUTT-T
           END-IF
           IF  (I-14)
               SET NOT-I-10                TO TRUE
               IF  ANTAL = 0
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-14 AND I-10 AND I-16)
               ADD 1                       TO ANTS
               GO TO SLUTT-T
           END-IF
           IF  (I-14)
               ADD 1                       TO ANTD
      *
           END-IF
           SET I-20                        TO TRUE
      *
           ADD 1                           TO ANTN.
 
       SLUTT-T.
           CONTINUE.
 
       BESTILL-GET SECTION.
       BESTILL-GET-P.
           IF  BESTILL-EOF-OFF
               READ BESTILL
               AT END
                   SET BESTILL-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       BESTILL-FLDSET SECTION.
       BESTILL-FLDSET-P.
           EVALUATE TRUE
           WHEN ( BESTILL-IO-AREA (5:1) NOT = '9' )
               MOVE BESTILL-IO-AREA (1:150) TO REC (1:150)
               MOVE BESTILL-IO-AREA (1:16) TO KEY-X (1:16)
               MOVE BESTILL-IO-AREA (2:3)  TO FNR (1:3)
               MOVE BESTILL-IO-AREA (5:5)  TO BNR (1:5)
               MOVE BESTILL-IO-AREA (89:5) TO ANTAL-IO
               MOVE BESTILL-IO-AREA (150:1) TO RA (1:1)
               MOVE BESTILL-IO-AREA (124:1) TO TEKST (1:1)
               MOVE BESTILL-IO-AREA (1:50) TO LIST (1:50)
           WHEN ( BESTILL-IO-AREA (5:1) = '9' )
               MOVE BESTILL-IO-AREA (1:50) TO LIST (1:50)
           END-EVALUATE.
 
       BESTILL-IDCHK SECTION.
       BESTILL-IDCHK-P.
           EVALUATE TRUE
           WHEN ( BESTILL-IO-AREA (5:1) NOT = '9' )
             OR ( BESTILL-IO-AREA (5:1) = '9' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       BESTILL-IDSET SECTION.
       BESTILL-IDSET-P.
           EVALUATE TRUE
           WHEN ( BESTILL-IO-AREA (5:1) NOT = '9' )
               SET I-01                    TO TRUE
           WHEN ( BESTILL-IO-AREA (5:1) = '9' )
               SET I-02                    TO TRUE
           END-EVALUATE.
 
       BESTILL-CHK-LEVEL SECTION.
       BESTILL-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( BESTILL-IO-AREA (5:1) NOT = '9' )
               MOVE LOW-VALUES             TO BESTILL-LEVEL-01
               MOVE BESTILL-IO-AREA (2:3)  TO BESTILL-01-L2-FNR
               MOVE BESTILL-IO-AREA (5:5)  TO BESTILL-01-L1-BNR
               IF  BESTILL-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  BESTILL-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  BESTILL-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  BESTILL-01-L2         TO THE-PRIOR-L2
               MOVE  BESTILL-01-L1         TO THE-PRIOR-L1
               SET BESTILL-LEVEL-INIT      TO TRUE
           WHEN ( BESTILL-IO-AREA (5:1) = '9' )
               CONTINUE
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-20)
               MOVE SPACES TO NYEBES-IO-AREA
               INITIALIZE NYEBES-IO-AREA
               MOVE REC                    TO NYEBES-IO-AREA (1:150)
               WRITE NYEBES-IO-AREA
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
           SET BESTILL-LEVEL-INIT          TO TRUE
           INITIALIZE BESTILL-DATA-FIELDS
           SET BESTILL-EOF-OFF             TO TRUE
           SET BESTILL-PROCESS             TO TRUE
           OPEN INPUT BESTILL
           OPEN OUTPUT NYEBES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE BESTILL
           CLOSE NYEBES.
 
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
