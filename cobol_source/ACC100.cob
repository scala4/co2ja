       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACC100R.
      **********************************************  Z-WIN-RPG2      *
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ACC100.rpg
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
           SELECT TAPEIN
               ASSIGN TO UT-S-TAPEIN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TAPEIN-STATUS.
           SELECT OUTFILE
               ASSIGN TO UT-S-OUTFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD TAPEIN
               BLOCK CONTAINS 2008
               RECORD CONTAINS 2008.
       01  TAPEIN-IO-AREA.
           05  TAPEIN-IO-AREA-X            PICTURE X(2008).
       FD OUTFILE
               BLOCK CONTAINS 4050
               RECORD CONTAINS 90.
       01  OUTFILE-IO-AREA.
           05  OUTFILE-IO-AREA-X           PICTURE X(90).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  TAPEIN-STATUS               PICTURE 99 VALUE 0.
           10  OUTFILE-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  TAPEIN-EOF-OFF          VALUE '0'.
               88  TAPEIN-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  TAPEIN-READ-OFF         VALUE '0'.
               88  TAPEIN-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  TAPEIN-PROCESS-OFF      VALUE '0'.
               88  TAPEIN-PROCESS          VALUE '1'.
           05  TAPEIN-DATA-FIELDS.
               10  DATO                    PICTURE X(8).
               10  START-X-IO.
                   15  START-X             PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  STOP-X-IO.
                   15  STOP-X              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  USER-X                  PICTURE X(16).
      *                                      31  32 SIGN
               10  USER1                   PICTURE X(5).
               10  NAVN                    PICTURE X(8).
               10  NAVN1                   PICTURE X(2).
               10  NAVN2                   PICTURE X(4).
               10  NUMMER-IO.
                   15  NUMMER              PICTURE S9(4) USAGE BINARY.
               10  RECID                   PICTURE X(1).
               10  PART                    PICTURE X(2).
               10  STEP                    PICTURE X(1).
               10  PROGR                   PICTURE X(8).
               10  ESIDE-IO.
                   15  ESIDE               PICTURE S9(4) USAGE BINARY.
               10  CPUTID-IO.
                   15  CPUTID              PICTURE S9(9) USAGE BINARY.
               10  VENTE-IO.
                   15  VENTE               PICTURE S9(9) USAGE BINARY.
               10  LINJE-IO.
                   15  LINJE               PICTURE S9(9) USAGE BINARY.
               10  FORM                    PICTURE X(4).
               10  LKOPI-IO.
                   15  LKOPI               PICTURE S9(9) USAGE BINARY.
      *                                   B  69  700SIDE
               10  SKOPI-IO.
                   15  SKOPI               PICTURE S9(4) USAGE BINARY.
               10  PRT                     PICTURE X(3).
      *       CC  03
      **
           05  TEMPORARY-FIELDS.
               10  KEY-X                   PICTURE X(13).
               10  NUM1-IO.
                   15  NUM1                PICTURE S9(4).
               10  CPU-IO.
                   15  CPU                 PICTURE S9(8).
               10  VENTE1-IO.
                   15  VENTE1              PICTURE S9(8).
               10  SIDE1-IO.
                   15  SIDE1               PICTURE S9(4).
               10  CPU1-IO.
                   15  CPU1                PICTURE S9(4)V9(2).
               10  VENTE2-IO.
                   15  VENTE2              PICTURE S9(4)V9(2).
               10  LINJE1-IO.
                   15  LINJE1              PICTURE S9(8).
               10  KOPIL-IO.
                   15  KOPIL               PICTURE S9(8).
               10  KOPIS-IO.
                   15  KOPIS               PICTURE S9(4).
           05  EDITTING-FIELDS.
               10  XO-70D                  PICTURE S9(7).
               10  XO-70U                  PICTURE 9(7).
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
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  TAPEIN-PROCESS
               SET TAPEIN-PROCESS-OFF      TO TRUE
               SET TAPEIN-READ             TO TRUE
           END-IF
 
           IF  TAPEIN-READ
           AND RECORD-SELECTED-OFF
               PERFORM TAPEIN-GET
               SET TAPEIN-READ-OFF         TO TRUE
               IF  NOT TAPEIN-EOF
                   PERFORM TAPEIN-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET TAPEIN-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  TAPEIN-PROCESS
               PERFORM TAPEIN-IDSET
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
 
           IF  TAPEIN-PROCESS
               PERFORM TAPEIN-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           SET NOT-I-1ST                   TO TRUE
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-81                    TO TRUE
           IF  NAVN1 = 'AU'
               SET I-81                    TO TRUE
           END-IF
           IF  (I-81)
               SET NOT-I-81                TO TRUE
               IF  USER1 = '802,C'
                   SET I-81                TO TRUE
               END-IF
           END-IF
           SET NOT-I-82                    TO TRUE
           IF  NAVN1 = 'PW'
               SET I-82                    TO TRUE
           END-IF
           IF  (I-82)
               SET NOT-I-82                TO TRUE
               IF  USER1 = '803,C'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           SET NOT-I-83                    TO TRUE
           IF  NAVN1 = 'FX'
               SET I-83                    TO TRUE
           END-IF
           IF  (I-83)
               SET NOT-I-84                TO TRUE
               IF  NAVN2 = 'FX01'
                   SET I-84                TO TRUE
               END-IF
           END-IF
           IF  (I-83 AND I-84)
               SET NOT-I-83                TO TRUE
           END-IF
           IF  (I-83)
               SET NOT-I-83                TO TRUE
               IF  USER1 = '202,L'
                   SET I-83                TO TRUE
               END-IF
      **
           END-IF
           IF  (I-81)
               MOVE '802,C,'               TO KEY-X (1:6)
               MOVE '00000,O'              TO KEY-X (7:7)
               MOVE KEY-X                  TO USER-X (1:13)
      *
           END-IF
           IF  (I-82)
               MOVE '803,C,'               TO KEY-X (1:6)
               MOVE '00000,O'              TO KEY-X (7:7)
               MOVE KEY-X                  TO USER-X (1:13)
      *
           END-IF
           IF  (I-83)
               MOVE '202,L,'               TO KEY-X (1:6)
               MOVE '00000,O'              TO KEY-X (7:7)
               MOVE KEY-X                  TO USER-X (1:13)
      **
      **
           END-IF
           ADD NUMMER TO ZERO          GIVING NUM1
           IF  (I-01)
               ADD CPUTID TO ZERO      GIVING CPU
               ADD VENTE TO ZERO       GIVING VENTE1
               ADD ESIDE TO ZERO       GIVING SIDE1
               DIVIDE CPU BY 300       GIVING CPU1
               DIVIDE VENTE1 BY 300    GIVING VENTE2
           END-IF
           IF  (I-02)
               ADD LINJE TO ZERO       GIVING LINJE1
      *  02                Z-ADDSIDE      SIDE1   40       ANTALL SIDER.
           END-IF
           IF  (I-02)
               ADD LKOPI TO ZERO       GIVING KOPIL
               ADD SKOPI TO ZERO       GIVING KOPIS
      *
           END-IF
           .
 
       TAPEIN-GET SECTION.
       TAPEIN-GET-P.
           IF  TAPEIN-EOF-OFF
               READ TAPEIN
               AT END
                   SET TAPEIN-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       TAPEIN-FLDSET SECTION.
       TAPEIN-FLDSET-P.
           EVALUATE TRUE
           WHEN ( TAPEIN-IO-AREA (43:1) = 'E' )
               MOVE TAPEIN-IO-AREA (1:8)   TO DATO (1:8)
               MOVE TAPEIN-IO-AREA (9:4)   TO START-X-IO
               MOVE TAPEIN-IO-AREA (13:4)  TO STOP-X-IO
               MOVE TAPEIN-IO-AREA (17:16) TO USER-X (1:16)
               MOVE TAPEIN-IO-AREA (17:5)  TO USER1 (1:5)
               MOVE TAPEIN-IO-AREA (33:8)  TO NAVN (1:8)
               MOVE TAPEIN-IO-AREA (33:2)  TO NAVN1 (1:2)
               MOVE TAPEIN-IO-AREA (33:4)  TO NAVN2 (1:4)
               MOVE TAPEIN-IO-AREA (41:2)  TO NUMMER-IO
               MOVE TAPEIN-IO-AREA (43:1)  TO RECID (1:1)
               MOVE TAPEIN-IO-AREA (97:2)  TO PART (1:2)
               MOVE TAPEIN-IO-AREA (100:1) TO STEP (1:1)
               MOVE TAPEIN-IO-AREA (105:8) TO PROGR (1:8)
               MOVE TAPEIN-IO-AREA (61:2)  TO ESIDE-IO
               MOVE TAPEIN-IO-AREA (117:4) TO CPUTID-IO
               MOVE TAPEIN-IO-AREA (125:4) TO VENTE-IO
           WHEN ( TAPEIN-IO-AREA (43:1) = 'L' )
               MOVE TAPEIN-IO-AREA (1:8)   TO DATO (1:8)
               MOVE TAPEIN-IO-AREA (9:4)   TO START-X-IO
               MOVE TAPEIN-IO-AREA (13:4)  TO STOP-X-IO
               MOVE TAPEIN-IO-AREA (17:16) TO USER-X (1:16)
               MOVE TAPEIN-IO-AREA (17:5)  TO USER1 (1:5)
               MOVE TAPEIN-IO-AREA (33:8)  TO NAVN (1:8)
               MOVE TAPEIN-IO-AREA (33:2)  TO NAVN1 (1:2)
               MOVE TAPEIN-IO-AREA (33:4)  TO NAVN2 (1:4)
               MOVE TAPEIN-IO-AREA (41:2)  TO NUMMER-IO
               MOVE TAPEIN-IO-AREA (43:1)  TO RECID (1:1)
               MOVE TAPEIN-IO-AREA (53:4)  TO LINJE-IO
               MOVE TAPEIN-IO-AREA (61:4)  TO FORM (1:4)
               MOVE TAPEIN-IO-AREA (65:4)  TO LKOPI-IO
               MOVE TAPEIN-IO-AREA (71:2)  TO SKOPI-IO
               MOVE TAPEIN-IO-AREA (46:3)  TO PRT (1:3)
           END-EVALUATE.
 
       TAPEIN-IDCHK SECTION.
       TAPEIN-IDCHK-P.
           EVALUATE TRUE
           WHEN ( TAPEIN-IO-AREA (43:1) = 'E' )
             OR ( TAPEIN-IO-AREA (43:1) = 'L' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       TAPEIN-IDSET SECTION.
       TAPEIN-IDSET-P.
           EVALUATE TRUE
           WHEN ( TAPEIN-IO-AREA (43:1) = 'E' )
               SET I-01                    TO TRUE
           WHEN ( TAPEIN-IO-AREA (43:1) = 'L' )
               SET I-02                    TO TRUE
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               MOVE SPACES TO OUTFILE-IO-AREA
               INITIALIZE OUTFILE-IO-AREA
               MOVE RECID                  TO OUTFILE-IO-AREA (1:1)
               MOVE DATO                   TO OUTFILE-IO-AREA (2:8)
               MOVE START-X                TO XO-70U
               MOVE XO-70U (1:7)           TO OUTFILE-IO-AREA (10:7)
               IF STOP-X < 0
                 MOVE STOP-X               TO XO-70D
                 MOVE XO-70D (1:7)         TO OUTFILE-IO-AREA (17:7)
               ELSE
                 MOVE STOP-X               TO XO-70U
                 MOVE XO-70U (1:7)         TO OUTFILE-IO-AREA (17:7)
               END-IF
               MOVE USER-X                 TO OUTFILE-IO-AREA (24:16)
               MOVE NAVN                   TO OUTFILE-IO-AREA (40:8)
               MOVE NUM1-IO                TO OUTFILE-IO-AREA (48:4)
               INITIALIZE NUM1-IO
               MOVE CPU1-IO                TO OUTFILE-IO-AREA (52:6)
               INITIALIZE CPU1-IO
               MOVE VENTE2-IO              TO OUTFILE-IO-AREA (58:6)
               INITIALIZE VENTE2-IO
               MOVE PART                   TO OUTFILE-IO-AREA (64:2)
               MOVE PROGR                  TO OUTFILE-IO-AREA (66:8)
               MOVE STEP                   TO OUTFILE-IO-AREA (76:1)
               MOVE SIDE1-IO               TO OUTFILE-IO-AREA (77:4)
               INITIALIZE SIDE1-IO
               WRITE OUTFILE-IO-AREA
           END-IF
           IF  (I-02)
               MOVE SPACES TO OUTFILE-IO-AREA
               INITIALIZE OUTFILE-IO-AREA
               MOVE RECID                  TO OUTFILE-IO-AREA (1:1)
               MOVE DATO                   TO OUTFILE-IO-AREA (2:8)
               IF START-X < 0
                 MOVE START-X              TO XO-70D
                 MOVE XO-70D (1:7)         TO OUTFILE-IO-AREA (10:7)
               ELSE
                 MOVE START-X              TO XO-70U
                 MOVE XO-70U (1:7)         TO OUTFILE-IO-AREA (10:7)
               END-IF
               IF STOP-X < 0
                 MOVE STOP-X               TO XO-70D
                 MOVE XO-70D (1:7)         TO OUTFILE-IO-AREA (17:7)
               ELSE
                 MOVE STOP-X               TO XO-70U
                 MOVE XO-70U (1:7)         TO OUTFILE-IO-AREA (17:7)
               END-IF
               MOVE USER-X                 TO OUTFILE-IO-AREA (24:16)
               MOVE NAVN                   TO OUTFILE-IO-AREA (40:8)
               MOVE NUM1-IO                TO OUTFILE-IO-AREA (48:4)
               INITIALIZE NUM1-IO
               MOVE LINJE1-IO              TO OUTFILE-IO-AREA (52:8)
               INITIALIZE LINJE1-IO
               MOVE KOPIL-IO               TO OUTFILE-IO-AREA (64:8)
               INITIALIZE KOPIL-IO
               MOVE KOPIS-IO               TO OUTFILE-IO-AREA (72:4)
               INITIALIZE KOPIS-IO
               MOVE FORM                   TO OUTFILE-IO-AREA (76:4)
               MOVE PRT                    TO OUTFILE-IO-AREA (80:3)
               WRITE OUTFILE-IO-AREA
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
           INITIALIZE TAPEIN-DATA-FIELDS
           SET TAPEIN-EOF-OFF              TO TRUE
           SET TAPEIN-PROCESS              TO TRUE
           OPEN INPUT TAPEIN
           OPEN OUTPUT OUTFILE.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE TAPEIN
           CLOSE OUTFILE.
 
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
