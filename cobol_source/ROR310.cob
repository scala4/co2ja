       IDENTIFICATION DIVISION.
       PROGRAM-ID. ROR310R.
      **********************************************  Z-WIN-RPG2   ****
      *  DANNE REC. FOR RESTORDRE PR. VGR. PR. AVD.      *
      *  TILPASSET NY REST.ORDRE.MASTER                  *
      ****************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ROR310.rpg
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
           SELECT VAREMAS
               ASSIGN TO VAREMAS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VAREMAS-STATUS.
           SELECT REOREG
               ASSIGN TO UT-S-REOREG
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS REOREG-STATUS.
           SELECT REOUT
               ASSIGN TO UT-S-REOUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS REOUT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD VAREMAS
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X           PICTURE X(200).
       FD REOREG
               BLOCK CONTAINS 320
               RECORD CONTAINS 160.
       01  REOREG-IO-AREA.
           05  REOREG-IO-AREA-X            PICTURE X(160).
       FD REOUT
               BLOCK CONTAINS 100
               RECORD CONTAINS 25.
       01  REOUT-IO-AREA.
           05  REOUT-IO-AREA-X             PICTURE X(25).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  REOREG-STATUS               PICTURE 99 VALUE 0.
           10  REOUT-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREMAS-EOF-OFF         VALUE '0'.
               88  VAREMAS-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREMAS-READ-OFF        VALUE '0'.
               88  VAREMAS-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREMAS-PROCESS-OFF     VALUE '0'.
               88  VAREMAS-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  REOREG-EOF-OFF          VALUE '0'.
               88  REOREG-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  REOREG-READ-OFF         VALUE '0'.
               88  REOREG-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  REOREG-PROCESS-OFF      VALUE '0'.
               88  REOREG-PROCESS          VALUE '1'.
           05  VAREMAS-DATA-FIELDS.
               10  FNR                     PICTURE X(3).
               10  EDBNR                   PICTURE X(7).
               10  VGRAVD                  PICTURE X(1).
               10  VGR                     PICTURE X(5).
               10  SLETT                   PICTURE X(1).
           05  VAREMAS-MP                  PICTURE X(10).
           05  VAREMAS-MC                  PICTURE X(10).
           05  VAREMAS-M-01            REDEFINES VAREMAS-MC.
               10  VAREMAS-M-01-M2.
                   15  VAREMAS-M-01-M2-FNR-G.
                       20  VAREMAS-M-01-M2-FNR PICTURE X(3).
               10  VAREMAS-M-01-M1.
                   15  VAREMAS-M-01-M1-EDBNR-G.
                       20  VAREMAS-M-01-M1-EDBNR PICTURE X(7).
           05  REOREG-DATA-FIELDS.
               10  AVD                     PICTURE X(1).
               10  ANT                     PICTURE X(4).
               10  BELRAB                  PICTURE X(11).
           05  REOREG-MP                   PICTURE X(10).
           05  REOREG-MC                   PICTURE X(10).
           05  REOREG-M-02             REDEFINES REOREG-MC.
               10  REOREG-M-02-M2.
                   15  REOREG-M-02-M2-FNR-G.
                       20  REOREG-M-02-M2-FNR PICTURE X(3).
               10  REOREG-M-02-M1.
                   15  REOREG-M-02-M1-EDBNR-G.
                       20  REOREG-M-02-M1-EDBNR PICTURE X(7).
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
           IF  VAREMAS-PROCESS
               SET VAREMAS-PROCESS-OFF     TO TRUE
               SET VAREMAS-READ            TO TRUE
           END-IF
 
           IF  VAREMAS-READ
               PERFORM VAREMAS-GET
               SET VAREMAS-READ-OFF        TO TRUE
               IF  NOT VAREMAS-EOF
                   PERFORM VAREMAS-MATCH-SET
               END-IF
           END-IF
 
           IF  REOREG-PROCESS
               SET REOREG-PROCESS-OFF      TO TRUE
               SET REOREG-READ             TO TRUE
           END-IF
 
           IF  REOREG-READ
               PERFORM REOREG-GET
               SET REOREG-READ-OFF         TO TRUE
               IF  NOT REOREG-EOF
                   PERFORM REOREG-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  VAREMAS-PROCESS
               PERFORM VAREMAS-IDSET
           END-IF
 
           IF  REOREG-PROCESS
               PERFORM REOREG-IDSET
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
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  VAREMAS-PROCESS
               PERFORM VAREMAS-FLDOFF
               PERFORM VAREMAS-FLDSET
           END-IF
 
           IF  REOREG-PROCESS
               PERFORM REOREG-FLDSET
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
           SET NOT-I-18                    TO TRUE
           IF  FNR = '918'
               SET I-18                    TO TRUE
           END-IF.
 
       VAREMAS-GET SECTION.
       VAREMAS-GET-P.
           IF  VAREMAS-EOF-OFF
               READ VAREMAS
               AT END
                   SET VAREMAS-EOF         TO TRUE
               END-READ
           END-IF.
 
       VAREMAS-FLDOFF SECTION.
       VAREMAS-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-08                TO TRUE
           END-EVALUATE.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (3:3)  TO FNR (1:3)
               MOVE VAREMAS-IO-AREA (6:7)  TO EDBNR (1:7)
               MOVE VAREMAS-IO-AREA (118:1) TO VGRAVD (1:1)
               MOVE VAREMAS-IO-AREA (118:5) TO VGR (1:5)
               MOVE VAREMAS-IO-AREA (128:1) TO SLETT (1:1)
               IF  SLETT = SPACES
                   SET I-08                TO TRUE
               END-IF
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-01                        TO TRUE.
 
       VAREMAS-MATCH-SET SECTION.
       VAREMAS-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (3:3)  TO VAREMAS-M-01-M2-FNR
               MOVE VAREMAS-IO-AREA (6:7)  TO VAREMAS-M-01-M1-EDBNR
           END-EVALUATE.
 
       REOREG-GET SECTION.
       REOREG-GET-P.
           IF  REOREG-EOF-OFF
               READ REOREG
               AT END
                   SET REOREG-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       REOREG-FLDSET SECTION.
       REOREG-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE REOREG-IO-AREA (1:3)   TO FNR (1:3)
               MOVE REOREG-IO-AREA (4:1)   TO AVD (1:1)
               MOVE REOREG-IO-AREA (41:7)  TO EDBNR (1:7)
               MOVE REOREG-IO-AREA (79:4)  TO ANT (1:4)
               MOVE REOREG-IO-AREA (137:11) TO BELRAB (1:11)
           END-EVALUATE.
 
       REOREG-IDSET SECTION.
       REOREG-IDSET-P.
           SET I-02                        TO TRUE.
 
       REOREG-MATCH-SET SECTION.
       REOREG-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE REOREG-IO-AREA (1:3)   TO REOREG-M-02-M2-FNR
               MOVE REOREG-IO-AREA (41:7)  TO REOREG-M-02-M1-EDBNR
           END-EVALUATE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  VAREMAS-EOF
               MOVE HIGH-VALUES            TO VAREMAS-MC
                                              VAREMAS-MP
           END-IF
           IF  REOREG-EOF
               MOVE HIGH-VALUES            TO REOREG-MC
                                              REOREG-MP
           END-IF
           IF  VAREMAS-MC < VAREMAS-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  REOREG-MC < REOREG-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  VAREMAS-MC < REOREG-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VAREMAS-PROCESS     TO TRUE
                   MOVE VAREMAS-MC         TO VAREMAS-MP
                   IF  VAREMAS-MC = REOREG-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  REOREG-MC < VAREMAS-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET REOREG-PROCESS      TO TRUE
                   MOVE REOREG-MC          TO REOREG-MP
                   IF  REOREG-MC = VAREMAS-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  VAREMAS-MC = REOREG-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VAREMAS-PROCESS     TO TRUE
                   MOVE VAREMAS-MC         TO VAREMAS-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND I-MR AND I-08)
               MOVE SPACES TO REOUT-IO-AREA
               INITIALIZE REOUT-IO-AREA
               MOVE AVD                    TO REOUT-IO-AREA (1:1)
               IF  (I-18)
                   MOVE VGRAVD             TO REOUT-IO-AREA (1:1)
               END-IF
               MOVE FNR                    TO REOUT-IO-AREA (2:3)
               MOVE ANT                    TO REOUT-IO-AREA (5:4)
               MOVE BELRAB                 TO REOUT-IO-AREA (9:11)
               MOVE VGR                    TO REOUT-IO-AREA (21:5)
               WRITE REOUT-IO-AREA
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
           INITIALIZE VAREMAS-DATA-FIELDS
           SET VAREMAS-EOF-OFF             TO TRUE
           SET VAREMAS-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO VAREMAS-MC
                                              VAREMAS-MP
           OPEN INPUT VAREMAS
           INITIALIZE REOREG-DATA-FIELDS
           SET REOREG-EOF-OFF              TO TRUE
           SET REOREG-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO REOREG-MC
                                              REOREG-MP
           OPEN INPUT REOREG
           OPEN OUTPUT REOUT.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE VAREMAS
           CLOSE REOREG
           CLOSE REOUT.
 
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
