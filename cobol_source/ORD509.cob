       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORD509R.
      **********************************************  Z-WIN-RPG2   ****
      *                                                              *
      *    OPPDATERING AV SERVICEPROSENTFILE.                        *
      *                                                              *
      ****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ORD509.rpg
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
           SELECT GMAST
               ASSIGN TO UT-S-GMAST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS GMAST-STATUS.
           SELECT TRANS
               ASSIGN TO UT-S-TRANS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TRANS-STATUS.
           SELECT NYMAST
               ASSIGN TO UT-S-NYMAST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS NYMAST-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD GMAST
               BLOCK CONTAINS 1950
               RECORD CONTAINS 30.
       01  GMAST-IO-AREA.
           05  GMAST-IO-AREA-X             PICTURE X(30).
       FD TRANS
               BLOCK CONTAINS 1950
               RECORD CONTAINS 30.
       01  TRANS-IO-AREA.
           05  TRANS-IO-AREA-X             PICTURE X(30).
       FD NYMAST
               BLOCK CONTAINS 1950
               RECORD CONTAINS 30.
       01  NYMAST-IO-AREA.
           05  NYMAST-IO-AREA-X            PICTURE X(30).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  GMAST-STATUS                PICTURE 99 VALUE 0.
           10  TRANS-STATUS                PICTURE 99 VALUE 0.
           10  NYMAST-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  GMAST-EOF-OFF           VALUE '0'.
               88  GMAST-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  GMAST-READ-OFF          VALUE '0'.
               88  GMAST-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  GMAST-PROCESS-OFF       VALUE '0'.
               88  GMAST-PROCESS           VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  TRANS-EOF-OFF           VALUE '0'.
               88  TRANS-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  TRANS-READ-OFF          VALUE '0'.
               88  TRANS-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  TRANS-PROCESS-OFF       VALUE '0'.
               88  TRANS-PROCESS           VALUE '1'.
           05  GMAST-DATA-FIELDS.
               10  REC1                    PICTURE X(30).
               10  FNR                     PICTURE X(3).
           05  TRANS-DATA-FIELDS.
               10  REC2                    PICTURE X(30).
           05  TEMPORARY-FIELDS.
               10  ANTG-IO.
                   15  ANTG                PICTURE S9(3).
               10  ANTT-IO.
                   15  ANTT                PICTURE S9(3).
               10  ANT-IO.
                   15  ANT                 PICTURE S9(3).
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
           IF  GMAST-PROCESS
               SET GMAST-PROCESS-OFF       TO TRUE
               SET GMAST-READ              TO TRUE
           END-IF
 
           IF  GMAST-READ
           AND RECORD-SELECTED-OFF
               PERFORM GMAST-GET
               SET GMAST-READ-OFF          TO TRUE
               IF  NOT GMAST-EOF
                   PERFORM GMAST-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET GMAST-PROCESS       TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  TRANS-PROCESS
               SET TRANS-PROCESS-OFF       TO TRUE
               SET TRANS-READ              TO TRUE
           END-IF
 
           IF  TRANS-READ
           AND RECORD-SELECTED-OFF
               PERFORM TRANS-GET
               SET TRANS-READ-OFF          TO TRUE
               IF  NOT TRANS-EOF
                   PERFORM TRANS-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET TRANS-PROCESS       TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  GMAST-PROCESS
               PERFORM GMAST-IDSET
           END-IF
 
           IF  TRANS-PROCESS
               PERFORM TRANS-IDSET
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
 
           IF  GMAST-PROCESS
               PERFORM GMAST-FLDSET
           END-IF
 
           IF  TRANS-PROCESS
               PERFORM TRANS-FLDSET
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
           IF  (I-01)
               ADD 1                       TO ANTG
           END-IF
           IF  (I-02)
               ADD 1                       TO ANTT
           END-IF
           ADD 1                           TO ANT.
 
       GMAST-GET SECTION.
       GMAST-GET-P.
           IF  GMAST-EOF-OFF
               READ GMAST
               AT END
                   SET GMAST-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       GMAST-FLDSET SECTION.
       GMAST-FLDSET-P.
           EVALUATE TRUE
           WHEN ( GMAST-IO-AREA (1:1) = '7' )
               MOVE GMAST-IO-AREA (1:30)   TO REC1 (1:30)
               MOVE GMAST-IO-AREA (28:3)   TO FNR (1:3)
           END-EVALUATE.
 
       GMAST-IDCHK SECTION.
       GMAST-IDCHK-P.
           EVALUATE TRUE
           WHEN ( GMAST-IO-AREA (1:1) = '7' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       GMAST-IDSET SECTION.
       GMAST-IDSET-P.
           EVALUATE TRUE
           WHEN ( GMAST-IO-AREA (1:1) = '7' )
               SET I-01                    TO TRUE
           END-EVALUATE.
 
       TRANS-GET SECTION.
       TRANS-GET-P.
           IF  TRANS-EOF-OFF
               READ TRANS
               AT END
                   SET TRANS-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       TRANS-FLDSET SECTION.
       TRANS-FLDSET-P.
           EVALUATE TRUE
           WHEN ( TRANS-IO-AREA (1:1) = '7' )
               MOVE TRANS-IO-AREA (1:30)   TO REC2 (1:30)
               MOVE TRANS-IO-AREA (28:3)   TO FNR (1:3)
           END-EVALUATE.
 
       TRANS-IDCHK SECTION.
       TRANS-IDCHK-P.
           EVALUATE TRUE
           WHEN ( TRANS-IO-AREA (1:1) = '7' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       TRANS-IDSET SECTION.
       TRANS-IDSET-P.
           EVALUATE TRUE
           WHEN ( TRANS-IO-AREA (1:1) = '7' )
               SET I-02                    TO TRUE
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               MOVE SPACES TO NYMAST-IO-AREA
               INITIALIZE NYMAST-IO-AREA
               MOVE REC1                   TO NYMAST-IO-AREA (1:30)
               WRITE NYMAST-IO-AREA
           END-IF
           IF  (I-02)
               MOVE SPACES TO NYMAST-IO-AREA
               INITIALIZE NYMAST-IO-AREA
               MOVE REC2                   TO NYMAST-IO-AREA (1:30)
               WRITE NYMAST-IO-AREA
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
           INITIALIZE GMAST-DATA-FIELDS
           SET GMAST-EOF-OFF               TO TRUE
           SET GMAST-PROCESS               TO TRUE
           OPEN INPUT GMAST
           INITIALIZE TRANS-DATA-FIELDS
           SET TRANS-EOF-OFF               TO TRUE
           SET TRANS-PROCESS               TO TRUE
           OPEN INPUT TRANS
           OPEN OUTPUT NYMAST.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE GMAST
           CLOSE TRANS
           CLOSE NYMAST.
 
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
