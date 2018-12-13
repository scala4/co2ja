       IDENTIFICATION DIVISION.
       PROGRAM-ID. FOM003R.
      **********************************************  Z-WIN-RPG2   ****
      *  MERGE  FAKSALG FIL MED FIL M/ORDRE OG RESTORDRE
      *********************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FOM003.rpg
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
           SELECT FAKFIL
               ASSIGN TO UT-S-FAKFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKFIL-STATUS.
           SELECT ORDFIL
               ASSIGN TO UT-S-ORDFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ORDFIL-STATUS.
           SELECT UTFIL
               ASSIGN TO UT-S-UTFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS UTFIL-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD FAKFIL
               BLOCK CONTAINS 320
               RECORD CONTAINS 160.
       01  FAKFIL-IO-AREA.
           05  FAKFIL-IO-AREA-X            PICTURE X(160).
       FD ORDFIL
               BLOCK CONTAINS 340
               RECORD CONTAINS 170.
       01  ORDFIL-IO-AREA.
           05  ORDFIL-IO-AREA-X            PICTURE X(170).
       FD UTFIL
               BLOCK CONTAINS 320
               RECORD CONTAINS 160.
       01  UTFIL-IO-AREA.
           05  UTFIL-IO-AREA-X             PICTURE X(160).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  FAKFIL-STATUS               PICTURE 99 VALUE 0.
           10  ORDFIL-STATUS               PICTURE 99 VALUE 0.
           10  UTFIL-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKFIL-EOF-OFF          VALUE '0'.
               88  FAKFIL-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKFIL-READ-OFF         VALUE '0'.
               88  FAKFIL-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKFIL-PROCESS-OFF      VALUE '0'.
               88  FAKFIL-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  FAKFIL-LEVEL-INIT-OFF   VALUE '0'.
               88  FAKFIL-LEVEL-INIT       VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDFIL-EOF-OFF          VALUE '0'.
               88  ORDFIL-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDFIL-READ-OFF         VALUE '0'.
               88  ORDFIL-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDFIL-PROCESS-OFF      VALUE '0'.
               88  ORDFIL-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  ORDFIL-LEVEL-INIT-OFF   VALUE '0'.
               88  ORDFIL-LEVEL-INIT       VALUE '1'.
           05  FAKFIL-LEVEL-01.
               10  FAKFIL-01-L1.
                   15  FAKFIL-01-L1-FIRMA  PICTURE X(3).
           05  FAKFIL-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  ORDNR                   PICTURE X(6).
               10  FAKREC                  PICTURE X(160).
           05  FAKFIL-MP                   PICTURE X(9).
           05  FAKFIL-MC                   PICTURE X(9).
           05  FAKFIL-M-01             REDEFINES FAKFIL-MC.
               10  FAKFIL-M-01-M2.
                   15  FAKFIL-M-01-M2-FIRMA-G.
                       20  FAKFIL-M-01-M2-FIRMA PICTURE X(3).
               10  FAKFIL-M-01-M1.
                   15  FAKFIL-M-01-M1-ORDNR-G.
                       20  FAKFIL-M-01-M1-ORDNR PICTURE X(6).
           05  ORDFIL-LEVEL-02.
               10  ORDFIL-02-L1.
                   15  ORDFIL-02-L1-FIRMA  PICTURE X(3).
           05  ORDFIL-DATA-FIELDS.
               10  ORDNR1                  PICTURE X(6).
               10  ORDREC                  PICTURE X(160).
      *  02 MR   AUTOP     COMP "X"                      40
      *  02 MRN40AUTOP     COMP "L"                      40
           05  ORDFIL-MP                   PICTURE X(9).
           05  ORDFIL-MC                   PICTURE X(9).
           05  ORDFIL-M-02             REDEFINES ORDFIL-MC.
               10  ORDFIL-M-02-M2.
                   15  ORDFIL-M-02-M2-FIRMA-G.
                       20  ORDFIL-M-02-M2-FIRMA PICTURE X(3).
               10  ORDFIL-M-02-M1.
                   15  ORDFIL-M-02-M1-ORDNR1-G.
                       20  ORDFIL-M-02-M1-ORDNR1 PICTURE X(6).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
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
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  FAKFIL-PROCESS
               SET FAKFIL-PROCESS-OFF      TO TRUE
               SET FAKFIL-READ             TO TRUE
           END-IF
 
           IF  FAKFIL-READ
               PERFORM FAKFIL-GET
               SET FAKFIL-READ-OFF         TO TRUE
               IF  NOT FAKFIL-EOF
                   PERFORM FAKFIL-MATCH-SET
               END-IF
           END-IF
 
           IF  ORDFIL-PROCESS
               SET ORDFIL-PROCESS-OFF      TO TRUE
               SET ORDFIL-READ             TO TRUE
           END-IF
 
           IF  ORDFIL-READ
               PERFORM ORDFIL-GET
               SET ORDFIL-READ-OFF         TO TRUE
               IF  NOT ORDFIL-EOF
                   PERFORM ORDFIL-MATCH-SET
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
 
           IF  FAKFIL-PROCESS
               PERFORM FAKFIL-IDSET
           END-IF
 
           IF  ORDFIL-PROCESS
               PERFORM ORDFIL-IDSET
           END-IF
 
           IF  FAKFIL-PROCESS
               PERFORM FAKFIL-CHK-LEVEL
           END-IF
 
           IF  ORDFIL-PROCESS
               PERFORM ORDFIL-CHK-LEVEL
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
 
           IF  FAKFIL-PROCESS
               PERFORM FAKFIL-FLDSET
           END-IF
 
           IF  ORDFIL-PROCESS
               PERFORM ORDFIL-FLDSET
           END-IF
 
           IF  FAKFIL-PROCESS
           OR  ORDFIL-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       FAKFIL-GET SECTION.
       FAKFIL-GET-P.
           IF  FAKFIL-EOF-OFF
               READ FAKFIL
               AT END
                   SET FAKFIL-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FAKFIL-FLDSET SECTION.
       FAKFIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKFIL-IO-AREA (1:3)   TO FIRMA (1:3)
               MOVE FAKFIL-IO-AREA (33:6)  TO ORDNR (1:6)
               MOVE FAKFIL-IO-AREA (1:160) TO FAKREC (1:160)
           END-EVALUATE.
 
       FAKFIL-IDSET SECTION.
       FAKFIL-IDSET-P.
           SET I-01                        TO TRUE.
 
       FAKFIL-CHK-LEVEL SECTION.
       FAKFIL-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO FAKFIL-LEVEL-01
               MOVE FAKFIL-IO-AREA (1:3)   TO FAKFIL-01-L1-FIRMA
               IF  FAKFIL-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  FAKFIL-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  FAKFIL-01-L1          TO THE-PRIOR-L1
               SET FAKFIL-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       FAKFIL-MATCH-SET SECTION.
       FAKFIL-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKFIL-IO-AREA (1:3)   TO FAKFIL-M-01-M2-FIRMA
               MOVE FAKFIL-IO-AREA (33:6)  TO FAKFIL-M-01-M1-ORDNR
           END-EVALUATE.
 
       ORDFIL-GET SECTION.
       ORDFIL-GET-P.
           IF  ORDFIL-EOF-OFF
               READ ORDFIL
               AT END
                   SET ORDFIL-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       ORDFIL-FLDSET SECTION.
       ORDFIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE ORDFIL-IO-AREA (1:3)   TO FIRMA (1:3)
               MOVE ORDFIL-IO-AREA (33:6)  TO ORDNR1 (1:6)
               MOVE ORDFIL-IO-AREA (1:160) TO ORDREC (1:160)
           END-EVALUATE.
 
       ORDFIL-IDSET SECTION.
       ORDFIL-IDSET-P.
           SET I-02                        TO TRUE.
 
       ORDFIL-CHK-LEVEL SECTION.
       ORDFIL-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO ORDFIL-LEVEL-02
               MOVE ORDFIL-IO-AREA (1:3)   TO ORDFIL-02-L1-FIRMA
               IF  ORDFIL-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDFIL-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDFIL-02-L1          TO THE-PRIOR-L1
               SET ORDFIL-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       ORDFIL-MATCH-SET SECTION.
       ORDFIL-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE ORDFIL-IO-AREA (1:3)   TO ORDFIL-M-02-M2-FIRMA
               MOVE ORDFIL-IO-AREA (33:6)  TO ORDFIL-M-02-M1-ORDNR1
           END-EVALUATE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  FAKFIL-EOF
               MOVE HIGH-VALUES            TO FAKFIL-MC
                                              FAKFIL-MP
           END-IF
           IF  ORDFIL-EOF
               MOVE HIGH-VALUES            TO ORDFIL-MC
                                              ORDFIL-MP
           END-IF
           IF  FAKFIL-MC < FAKFIL-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  ORDFIL-MC < ORDFIL-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  FAKFIL-MC < ORDFIL-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FAKFIL-PROCESS      TO TRUE
                   MOVE FAKFIL-MC          TO FAKFIL-MP
                   IF  FAKFIL-MC = ORDFIL-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  ORDFIL-MC < FAKFIL-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET ORDFIL-PROCESS      TO TRUE
                   MOVE ORDFIL-MC          TO ORDFIL-MP
                   IF  ORDFIL-MC = FAKFIL-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  FAKFIL-MC = ORDFIL-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FAKFIL-PROCESS      TO TRUE
                   MOVE FAKFIL-MC          TO FAKFIL-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               MOVE SPACES TO UTFIL-IO-AREA
               INITIALIZE UTFIL-IO-AREA
               MOVE FAKREC                 TO UTFIL-IO-AREA (1:160)
               WRITE UTFIL-IO-AREA
           END-IF
           IF  (I-02 AND NOT-I-MR)
               MOVE SPACES TO UTFIL-IO-AREA
               INITIALIZE UTFIL-IO-AREA
               MOVE ORDREC                 TO UTFIL-IO-AREA (1:160)
               WRITE UTFIL-IO-AREA
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
           SET FAKFIL-LEVEL-INIT           TO TRUE
           INITIALIZE FAKFIL-DATA-FIELDS
           SET FAKFIL-EOF-OFF              TO TRUE
           SET FAKFIL-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO FAKFIL-MC
                                              FAKFIL-MP
           OPEN INPUT FAKFIL
           SET ORDFIL-LEVEL-INIT           TO TRUE
           INITIALIZE ORDFIL-DATA-FIELDS
           SET ORDFIL-EOF-OFF              TO TRUE
           SET ORDFIL-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO ORDFIL-MC
                                              ORDFIL-MP
           OPEN INPUT ORDFIL
           OPEN OUTPUT UTFIL.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FAKFIL
           CLOSE ORDFIL
           CLOSE UTFIL.
 
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
