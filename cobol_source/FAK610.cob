       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK610R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAM: FAK610  MERGE OG DANNE FAKT.MASTER.                  *
      *          ALLE FAKTURARAFILER INN I EN FILE.                   *
      *  30/08-01 PROGRAMMERT AV ESPEN LARSEN                         *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK610.rpg
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
           SELECT FKREC1
               ASSIGN TO UT-S-FKREC1
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FKREC1-STATUS.
           SELECT FAREC
               ASSIGN TO UT-S-FAREC
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAREC-STATUS.
           SELECT FVREC
               ASSIGN TO UT-S-FVREC
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FVREC-STATUS.
           SELECT FMAST
               ASSIGN TO UT-S-FMAST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FMAST-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD FKREC1
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  FKREC1-IO-AREA.
           05  FKREC1-IO-AREA-X            PICTURE X(200).
       FD FAREC
               BLOCK CONTAINS 300
               RECORD CONTAINS 150.
       01  FAREC-IO-AREA.
           05  FAREC-IO-AREA-X             PICTURE X(150).
       FD FVREC
               BLOCK CONTAINS 300
               RECORD CONTAINS 150.
       01  FVREC-IO-AREA.
           05  FVREC-IO-AREA-X             PICTURE X(150).
       FD FMAST
               BLOCK CONTAINS 300
               RECORD CONTAINS 150.
       01  FMAST-IO-AREA.
           05  FMAST-IO-AREA-X             PICTURE X(150).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  FKREC1-STATUS               PICTURE 99 VALUE 0.
           10  FAREC-STATUS                PICTURE 99 VALUE 0.
           10  FVREC-STATUS                PICTURE 99 VALUE 0.
           10  FMAST-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  FKREC1-EOF-OFF          VALUE '0'.
               88  FKREC1-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FKREC1-READ-OFF         VALUE '0'.
               88  FKREC1-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FKREC1-PROCESS-OFF      VALUE '0'.
               88  FKREC1-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAREC-EOF-OFF           VALUE '0'.
               88  FAREC-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAREC-READ-OFF          VALUE '0'.
               88  FAREC-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAREC-PROCESS-OFF       VALUE '0'.
               88  FAREC-PROCESS           VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FVREC-EOF-OFF           VALUE '0'.
               88  FVREC-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FVREC-READ-OFF          VALUE '0'.
               88  FVREC-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FVREC-PROCESS-OFF       VALUE '0'.
               88  FVREC-PROCESS           VALUE '1'.
           05  FKREC1-DATA-FIELDS.
               10  FNRKNR                  PICTURE X(9).
               10  BM                      PICTURE X(2).
               10  KEY1B                   PICTURE X(14).
               10  REC1                    PICTURE X(150).
           05  FKREC1-MP                   PICTURE X(25).
           05  FKREC1-MC                   PICTURE X(25).
           05  FKREC1-M-01             REDEFINES FKREC1-MC.
               10  FKREC1-M-01-M3.
                   15  FKREC1-M-01-M3-FNRKNR-G.
                       20  FKREC1-M-01-M3-FNRKNR PICTURE X(9).
               10  FKREC1-M-01-M2.
                   15  FKREC1-M-01-M2-BM-G.
                       20  FKREC1-M-01-M2-BM PICTURE X(2).
               10  FKREC1-M-01-M1.
                   15  FKREC1-M-01-M1-KEY1B-G.
                       20  FKREC1-M-01-M1-KEY1B PICTURE X(14).
           05  FAREC-DATA-FIELDS.
               10  REC2                    PICTURE X(150).
           05  FAREC-MP                    PICTURE X(25).
           05  FAREC-MC                    PICTURE X(25).
           05  FAREC-M-02              REDEFINES FAREC-MC.
               10  FAREC-M-02-M3.
                   15  FAREC-M-02-M3-FNRKNR-G.
                       20  FAREC-M-02-M3-FNRKNR PICTURE X(9).
               10  FAREC-M-02-M2.
                   15  FAREC-M-02-M2-BM-G.
                       20  FAREC-M-02-M2-BM PICTURE X(2).
               10  FAREC-M-02-M1.
                   15  FAREC-M-02-M1-KEY1B-G.
                       20  FAREC-M-02-M1-KEY1B PICTURE X(14).
           05  FVREC-DATA-FIELDS.
               10  REC3                    PICTURE X(150).
      *****************************************************************
      * RUTINE FOR OG TELLING AV ANTALL                               *
      *****************************************************************
           05  FVREC-MP                    PICTURE X(25).
           05  FVREC-MC                    PICTURE X(25).
           05  FVREC-M-03              REDEFINES FVREC-MC.
               10  FVREC-M-03-M3.
                   15  FVREC-M-03-M3-FNRKNR-G.
                       20  FVREC-M-03-M3-FNRKNR PICTURE X(9).
               10  FVREC-M-03-M2.
                   15  FVREC-M-03-M2-BM-G.
                       20  FVREC-M-03-M2-BM PICTURE X(2).
               10  FVREC-M-03-M1.
                   15  FVREC-M-03-M1-KEY1B-G.
                       20  FVREC-M-03-M1-KEY1B PICTURE X(14).
           05  TEMPORARY-FIELDS.
               10  ANTFAK-IO.
                   15  ANTFAK              PICTURE S9(7).
               10  ANTADR-IO.
                   15  ANTADR              PICTURE S9(7).
               10  ANTLIN-IO.
                   15  ANTLIN              PICTURE S9(7).
               10  TOTREC-IO.
                   15  TOTREC              PICTURE S9(7).
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
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  FKREC1-PROCESS
               SET FKREC1-PROCESS-OFF      TO TRUE
               SET FKREC1-READ             TO TRUE
           END-IF
 
           IF  FKREC1-READ
               PERFORM FKREC1-GET
               SET FKREC1-READ-OFF         TO TRUE
               IF  NOT FKREC1-EOF
                   PERFORM FKREC1-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM FKREC1-MATCH-SET
               END-IF
           END-IF
 
           IF  FAREC-PROCESS
               SET FAREC-PROCESS-OFF       TO TRUE
               SET FAREC-READ              TO TRUE
           END-IF
 
           IF  FAREC-READ
               PERFORM FAREC-GET
               SET FAREC-READ-OFF          TO TRUE
               IF  NOT FAREC-EOF
                   PERFORM FAREC-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM FAREC-MATCH-SET
               END-IF
           END-IF
 
           IF  FVREC-PROCESS
               SET FVREC-PROCESS-OFF       TO TRUE
               SET FVREC-READ              TO TRUE
           END-IF
 
           IF  FVREC-READ
               PERFORM FVREC-GET
               SET FVREC-READ-OFF          TO TRUE
               IF  NOT FVREC-EOF
                   PERFORM FVREC-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM FVREC-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  FKREC1-PROCESS
               PERFORM FKREC1-IDSET
           END-IF
 
           IF  FAREC-PROCESS
               PERFORM FAREC-IDSET
           END-IF
 
           IF  FVREC-PROCESS
               PERFORM FVREC-IDSET
           END-IF
 
           IF I-1ST
               GO TO LR-INDICATOR-TEST
           END-IF.
 
       MAINLINE-TOTAL-CALCS.
           IF  I-LR
               PERFORM LR-CALCS
           END-IF.
 
       LR-INDICATOR-TEST.
           IF  I-LR
               GO TO MAINLINE-TERMINATION
           END-IF
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  FKREC1-PROCESS
               PERFORM FKREC1-FLDSET
           END-IF
 
           IF  FAREC-PROCESS
               PERFORM FAREC-FLDSET
           END-IF
 
           IF  FVREC-PROCESS
               PERFORM FVREC-FLDSET
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
               ADD 1                       TO ANTFAK
           END-IF
           IF  (I-02)
               ADD 1                       TO ANTADR
           END-IF
           IF  (I-03)
               ADD 1                       TO ANTLIN
           END-IF.
 
       LR-CALCS SECTION.
       LR-CALCS-P.
           ADD ANTADR TO ANTFAK        GIVING TOTREC
           ADD ANTLIN                      TO TOTREC
      *****************************************************************
           .
 
       FKREC1-GET SECTION.
       FKREC1-GET-P.
           IF  FKREC1-EOF-OFF
               READ FKREC1
               AT END
                   SET FKREC1-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FKREC1-FLDSET SECTION.
       FKREC1-FLDSET-P.
           EVALUATE TRUE
           WHEN ( FKREC1-IO-AREA (22:1) = '0' )
               MOVE FKREC1-IO-AREA (1:9)   TO FNRKNR (1:9)
               MOVE FKREC1-IO-AREA (148:2) TO BM (1:2)
               MOVE FKREC1-IO-AREA (10:14) TO KEY1B (1:14)
               MOVE FKREC1-IO-AREA (1:150) TO REC1 (1:150)
           END-EVALUATE.
 
       FKREC1-IDCHK SECTION.
       FKREC1-IDCHK-P.
           EVALUATE TRUE
           WHEN ( FKREC1-IO-AREA (22:1) = '0' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       FKREC1-IDSET SECTION.
       FKREC1-IDSET-P.
           EVALUATE TRUE
           WHEN ( FKREC1-IO-AREA (22:1) = '0' )
               SET I-01                    TO TRUE
           END-EVALUATE.
 
       FKREC1-MATCH-SET SECTION.
       FKREC1-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( FKREC1-IO-AREA (22:1) = '0' )
               MOVE FKREC1-IO-AREA (1:9)   TO FKREC1-M-01-M3-FNRKNR
               MOVE FKREC1-IO-AREA (148:2) TO FKREC1-M-01-M2-BM
               MOVE FKREC1-IO-AREA (10:14) TO FKREC1-M-01-M1-KEY1B
           END-EVALUATE.
 
       FAREC-GET SECTION.
       FAREC-GET-P.
           IF  FAREC-EOF-OFF
               READ FAREC
               AT END
                   SET FAREC-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FAREC-FLDSET SECTION.
       FAREC-FLDSET-P.
           EVALUATE TRUE
           WHEN ( FAREC-IO-AREA (22:1) = '1' )
               MOVE FAREC-IO-AREA (1:9)    TO FNRKNR (1:9)
               MOVE FAREC-IO-AREA (148:2)  TO BM (1:2)
               MOVE FAREC-IO-AREA (10:14)  TO KEY1B (1:14)
               MOVE FAREC-IO-AREA (1:150)  TO REC2 (1:150)
           END-EVALUATE.
 
       FAREC-IDCHK SECTION.
       FAREC-IDCHK-P.
           EVALUATE TRUE
           WHEN ( FAREC-IO-AREA (22:1) = '1' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       FAREC-IDSET SECTION.
       FAREC-IDSET-P.
           EVALUATE TRUE
           WHEN ( FAREC-IO-AREA (22:1) = '1' )
               SET I-02                    TO TRUE
           END-EVALUATE.
 
       FAREC-MATCH-SET SECTION.
       FAREC-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( FAREC-IO-AREA (22:1) = '1' )
               MOVE FAREC-IO-AREA (1:9)    TO FAREC-M-02-M3-FNRKNR
               MOVE FAREC-IO-AREA (148:2)  TO FAREC-M-02-M2-BM
               MOVE FAREC-IO-AREA (10:14)  TO FAREC-M-02-M1-KEY1B
           END-EVALUATE.
 
       FVREC-GET SECTION.
       FVREC-GET-P.
           IF  FVREC-EOF-OFF
               READ FVREC
               AT END
                   SET FVREC-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FVREC-FLDSET SECTION.
       FVREC-FLDSET-P.
           EVALUATE TRUE
           WHEN ( FVREC-IO-AREA (22:1) = '2' )
               MOVE FVREC-IO-AREA (1:9)    TO FNRKNR (1:9)
               MOVE FVREC-IO-AREA (148:2)  TO BM (1:2)
               MOVE FVREC-IO-AREA (10:14)  TO KEY1B (1:14)
               MOVE FVREC-IO-AREA (1:150)  TO REC3 (1:150)
           END-EVALUATE.
 
       FVREC-IDCHK SECTION.
       FVREC-IDCHK-P.
           EVALUATE TRUE
           WHEN ( FVREC-IO-AREA (22:1) = '2' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       FVREC-IDSET SECTION.
       FVREC-IDSET-P.
           EVALUATE TRUE
           WHEN ( FVREC-IO-AREA (22:1) = '2' )
               SET I-03                    TO TRUE
           END-EVALUATE.
 
       FVREC-MATCH-SET SECTION.
       FVREC-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( FVREC-IO-AREA (22:1) = '2' )
               MOVE FVREC-IO-AREA (1:9)    TO FVREC-M-03-M3-FNRKNR
               MOVE FVREC-IO-AREA (148:2)  TO FVREC-M-03-M2-BM
               MOVE FVREC-IO-AREA (10:14)  TO FVREC-M-03-M1-KEY1B
           END-EVALUATE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  FKREC1-EOF
               MOVE HIGH-VALUES            TO FKREC1-MC
                                              FKREC1-MP
           END-IF
           IF  FAREC-EOF
               MOVE HIGH-VALUES            TO FAREC-MC
                                              FAREC-MP
           END-IF
           IF  FVREC-EOF
               MOVE HIGH-VALUES            TO FVREC-MC
                                              FVREC-MP
           END-IF
           IF  FKREC1-MC < FKREC1-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  FAREC-MC < FAREC-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  FVREC-MC < FVREC-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  FKREC1-MC < FAREC-MC
            AND  FKREC1-MC < FVREC-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FKREC1-PROCESS      TO TRUE
                   MOVE FKREC1-MC          TO FKREC1-MP
                   IF  FKREC1-MC = FAREC-MP
                     OR  FKREC1-MC = FVREC-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  FAREC-MC < FKREC1-MC
            AND  FAREC-MC < FVREC-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FAREC-PROCESS       TO TRUE
                   MOVE FAREC-MC           TO FAREC-MP
                   IF  FAREC-MC = FKREC1-MP
                     OR  FAREC-MC = FVREC-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  FVREC-MC < FKREC1-MC
            AND  FVREC-MC < FAREC-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FVREC-PROCESS       TO TRUE
                   MOVE FVREC-MC           TO FVREC-MP
                   IF  FVREC-MC = FKREC1-MP
                     OR  FVREC-MC = FAREC-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  FKREC1-MC = FAREC-MC
             OR  FKREC1-MC = FVREC-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FKREC1-PROCESS      TO TRUE
                   MOVE FKREC1-MC          TO FKREC1-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           WHEN  FAREC-MC = FVREC-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FAREC-PROCESS       TO TRUE
                   MOVE FAREC-MC           TO FAREC-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               MOVE SPACES TO FMAST-IO-AREA
               INITIALIZE FMAST-IO-AREA
               MOVE REC1                   TO FMAST-IO-AREA (1:150)
               WRITE FMAST-IO-AREA
           END-IF
           IF  (I-02)
               MOVE SPACES TO FMAST-IO-AREA
               INITIALIZE FMAST-IO-AREA
               MOVE REC2                   TO FMAST-IO-AREA (1:150)
               WRITE FMAST-IO-AREA
           END-IF
           IF  (I-03)
               MOVE SPACES TO FMAST-IO-AREA
               INITIALIZE FMAST-IO-AREA
               MOVE REC3                   TO FMAST-IO-AREA (1:150)
               WRITE FMAST-IO-AREA
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
           MOVE 3                          TO LR-CHECK
           INITIALIZE FKREC1-DATA-FIELDS
           SET FKREC1-EOF-OFF              TO TRUE
           SET FKREC1-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO FKREC1-MC
                                              FKREC1-MP
           OPEN INPUT FKREC1
           INITIALIZE FAREC-DATA-FIELDS
           SET FAREC-EOF-OFF               TO TRUE
           SET FAREC-PROCESS               TO TRUE
           MOVE LOW-VALUES                 TO FAREC-MC
                                              FAREC-MP
           OPEN INPUT FAREC
           INITIALIZE FVREC-DATA-FIELDS
           SET FVREC-EOF-OFF               TO TRUE
           SET FVREC-PROCESS               TO TRUE
           MOVE LOW-VALUES                 TO FVREC-MC
                                              FVREC-MP
           OPEN INPUT FVREC
           OPEN OUTPUT FMAST.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FKREC1
           CLOSE FAREC
           CLOSE FVREC
           CLOSE FMAST.
 
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
