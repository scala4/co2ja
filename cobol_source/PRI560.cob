       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRI560R.
      **********************************************  Z-WIN-RPG2   ****
      * DANNING AV PRISLISTERECORDS VANLIG 48 LINJERS PAPIR.        *
      * UPSI BLANK  NU1  = U/SIDEHOPP.                              *
      * UPSI 1       U1  = SIDEHOPP PR. ALFAKODE                    *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: PRI560.rpg
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
           SELECT INF
               ASSIGN TO UT-S-INF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INF-STATUS.
           SELECT UTF
               ASSIGN TO UT-S-UTF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS UTF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD INF
               BLOCK CONTAINS 4067
               RECORD CONTAINS 83.
       01  INF-IO-AREA.
           05  INF-IO-AREA-X               PICTURE X(83).
       FD UTF
               BLOCK CONTAINS 4050
               RECORD CONTAINS 90.
       01  UTF-IO-AREA.
           05  UTF-IO-AREA-X               PICTURE X(90).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  INF-STATUS                  PICTURE 99 VALUE 0.
           10  UTF-STATUS                  PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  INF-EOF-OFF             VALUE '0'.
               88  INF-EOF                 VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INF-READ-OFF            VALUE '0'.
               88  INF-READ                VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INF-PROCESS-OFF         VALUE '0'.
               88  INF-PROCESS             VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  INF-LEVEL-INIT-OFF      VALUE '0'.
               88  INF-LEVEL-INIT          VALUE '1'.
           05  INF-LEVEL-01.
               10  INF-01-L1.
                   15  INF-01-L1-ALF       PICTURE X(3).
           05  INF-DATA-FIELDS.
               10  REC                     PICTURE X(83).
               10  ALF                     PICTURE X(3).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  POS-X                   PICTURE X(1).
               10  SNR-IO.
                   15  SNR                 PICTURE S9(4).
               10  LNR-IO.
                   15  LNR                 PICTURE S9(2).
               10  POSA-IO.
                   15  POSA                PICTURE S9(1).
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
           IF  INF-PROCESS
               SET INF-PROCESS-OFF         TO TRUE
               SET INF-READ                TO TRUE
           END-IF
 
           IF  INF-READ
           AND RECORD-SELECTED-OFF
               PERFORM INF-GET
               SET INF-READ-OFF            TO TRUE
               IF  NOT INF-EOF
                   SET INF-PROCESS         TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  INF-PROCESS
               PERFORM INF-IDSET
           END-IF
 
           IF  INF-PROCESS
               PERFORM INF-CHK-LEVEL
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
 
           IF  INF-PROCESS
               PERFORM INF-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  INF-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (NOT-I-30)
               MOVE '1'                    TO POS-X
               MOVE 1                      TO SNR
               SET I-30                    TO TRUE
           END-IF
           IF  (I-L1 AND I-U1)
               SUBTRACT LNR                FROM LNR
               ADD 1                       TO SNR
               MOVE 1                      TO POS-X
               SET NOT-I-25                TO TRUE
               SET NOT-I-26                TO TRUE
               MOVE 0                      TO POSA
      **
           END-IF
           IF  (I-25)
               SUBTRACT LNR                FROM LNR
           END-IF
           IF  (I-26)
               ADD 1                       TO SNR
           END-IF
           IF  (I-25)
               MOVE '2'                    TO POS-X
           END-IF
           IF  (I-26)
               MOVE '1'                    TO POS-X
           END-IF
           SET NOT-I-25                    TO TRUE
           SET NOT-I-26                    TO TRUE
           ADD 1                           TO LNR
      *
           SET NOT-I-40                    TO TRUE
           IF  LNR NOT < 36
               SET I-40                    TO TRUE
           END-IF
           IF  (I-40)
               ADD 1                       TO POSA
               SET NOT-I-41                TO TRUE
               IF  POSA NOT < 2
                   SET I-41                TO TRUE
               END-IF
           END-IF
           IF  (I-41)
               MOVE 0                      TO POSA
               SET I-25                    TO TRUE
               SET I-26                    TO TRUE
           END-IF
           IF  (I-40)
               SET I-25                    TO TRUE
           END-IF
           SET NOT-I-40                    TO TRUE
           SET NOT-I-41                    TO TRUE.
 
       INF-GET SECTION.
       INF-GET-P.
           IF  INF-EOF-OFF
               READ INF
               AT END
                   SET INF-EOF             TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INF-FLDSET SECTION.
       INF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INF-IO-AREA (1:83)     TO REC (1:83)
               MOVE INF-IO-AREA (4:3)      TO ALF (1:3)
           END-EVALUATE.
 
       INF-IDSET SECTION.
       INF-IDSET-P.
           SET I-01                        TO TRUE.
 
       INF-CHK-LEVEL SECTION.
       INF-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO INF-LEVEL-01
               MOVE INF-IO-AREA (4:3)      TO INF-01-L1-ALF
               IF  INF-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INF-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INF-01-L1             TO THE-PRIOR-L1
               SET INF-LEVEL-INIT          TO TRUE
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               MOVE SPACES TO UTF-IO-AREA
               INITIALIZE UTF-IO-AREA
               MOVE REC                    TO UTF-IO-AREA (1:83)
               MOVE SNR-IO                 TO UTF-IO-AREA (84:4)
               MOVE LNR-IO                 TO UTF-IO-AREA (88:2)
               MOVE POS-X                  TO UTF-IO-AREA (90:1)
               WRITE UTF-IO-AREA
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
           SET INF-LEVEL-INIT              TO TRUE
           INITIALIZE INF-DATA-FIELDS
           SET INF-EOF-OFF                 TO TRUE
           SET INF-PROCESS                 TO TRUE
           OPEN INPUT INF
           OPEN OUTPUT UTF.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INF
           CLOSE UTF.
 
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
