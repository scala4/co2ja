       IDENTIFICATION DIVISION.
       PROGRAM-ID. NYP250R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAM: NYP250                                    *
      * DANNE VSAM/KSDS FILE MED 1 RECORD PR EDB-NR.       *
      * DENNE FILE BENYTTES AV DECA.                       *
      * OM DET ER FLERE RECORDS PÅ SAMME EDB-NR, VIL DEN   *
      * RECORD MED DET HØYESTE REF.NR. BLI LAGT INN.       *
      ******************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: NYP250.rpg
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
           SELECT PRISSEQ
               ASSIGN TO UT-S-PRISSEQ
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PRISSEQ-STATUS.
           SELECT NYEPRIS
               ASSIGN TO NYEPRIS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS NYEPRIS-STATUS.
           SELECT TOTALER
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TOTALER-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD PRISSEQ
               BLOCK CONTAINS 800
               RECORD CONTAINS 80.
       01  PRISSEQ-IO-AREA.
           05  PRISSEQ-IO-AREA-X           PICTURE X(80).
       FD NYEPRIS
               RECORD CONTAINS 80.
       01  NYEPRIS-IO-AREA.
           05  NYEPRIS-IO-AREA-X           PICTURE X(80).
       FD TOTALER
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  TOTALER-IO-PRINT.
           05  TOTALER-IO-AREA-CONTROL     PICTURE X VALUE ' '.
        02 TOTALER-IO-AREA.
           05  TOTALER-IO-AREA-X           PICTURE X(132).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  PRISSEQ-STATUS              PICTURE 99 VALUE 0.
           10  NYEPRIS-STATUS              PICTURE 99 VALUE 0.
           10  TOTALER-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  PRISSEQ-EOF-OFF         VALUE '0'.
               88  PRISSEQ-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PRISSEQ-READ-OFF        VALUE '0'.
               88  PRISSEQ-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PRISSEQ-PROCESS-OFF     VALUE '0'.
               88  PRISSEQ-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  PRISSEQ-LEVEL-INIT-OFF  VALUE '0'.
               88  PRISSEQ-LEVEL-INIT      VALUE '1'.
           05  TOTALER-DATA-FIELDS.
               10  TOTALER-AFTER-SPACE     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-AFTER-SKIP      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-BEFORE-SPACE    PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-BEFORE-SKIP     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-MAX-LINES       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-LINE-COUNT      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-CLR-IO          PICTURE X VALUE 'Y'.
           05  PRISSEQ-LEVEL-01.
               10  PRISSEQ-01-L2.
                   15  PRISSEQ-01-L2-FIRMA PICTURE X(3).
               10  PRISSEQ-01-L1.
                   15  PRISSEQ-01-L1-EDBNR PICTURE X(7).
           05  PRISSEQ-DATA-FIELDS.
               10  RECART                  PICTURE X(1).
               10  FIRMA                   PICTURE X(3).
               10  REFNR                   PICTURE X(5).
               10  SEQNR                   PICTURE X(4).
               10  EDBNR                   PICTURE X(7).
               10  RECC                    PICTURE X(60).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(7).
           05  TEMPORARY-FIELDS.
               10  ANTL1-IO.
                   15  ANTL1               PICTURE S9(5).
               10  ANTNL1-IO.
                   15  ANTNL1              PICTURE S9(5).
               10  ANT-IO.
                   15  ANT                 PICTURE S9(5).
           05  EDITTING-FIELDS.
               10  XO-50YY9                PICTURE ZZ.ZZ9.
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
           IF  PRISSEQ-PROCESS
               SET PRISSEQ-PROCESS-OFF     TO TRUE
               SET PRISSEQ-READ            TO TRUE
           END-IF
 
           IF  PRISSEQ-READ
           AND RECORD-SELECTED-OFF
               PERFORM PRISSEQ-GET
               SET PRISSEQ-READ-OFF        TO TRUE
               IF  NOT PRISSEQ-EOF
                   PERFORM PRISSEQ-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET PRISSEQ-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  PRISSEQ-PROCESS
               PERFORM PRISSEQ-IDSET
           END-IF
 
           IF  PRISSEQ-PROCESS
               PERFORM PRISSEQ-CHK-LEVEL
           END-IF
 
           IF I-1ST
               GO TO LR-INDICATOR-TEST
           END-IF.
 
       MAINLINE-TOTAL-CALCS.
           PERFORM TOTAL-OUTPUT.
 
       LR-INDICATOR-TEST.
           IF  I-LR
               GO TO MAINLINE-TERMINATION
           END-IF
 
           IF  PRISSEQ-PROCESS
               PERFORM PRISSEQ-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  PRISSEQ-PROCESS
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
               ADD 1                       TO ANTL1
           END-IF
           IF  (NOT-I-L1)
               ADD 1                       TO ANTNL1
           END-IF
           IF  (I-01)
               ADD 1                       TO ANT
           END-IF.
 
       PRISSEQ-GET SECTION.
       PRISSEQ-GET-P.
           IF  PRISSEQ-EOF-OFF
               READ PRISSEQ
               AT END
                   SET PRISSEQ-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       PRISSEQ-FLDSET SECTION.
       PRISSEQ-FLDSET-P.
           EVALUATE TRUE
           WHEN ( PRISSEQ-IO-AREA (1:1) = 'C' )
               MOVE PRISSEQ-IO-AREA (1:1)  TO RECART (1:1)
               MOVE PRISSEQ-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE PRISSEQ-IO-AREA (5:5)  TO REFNR (1:5)
               MOVE PRISSEQ-IO-AREA (10:4) TO SEQNR (1:4)
               MOVE PRISSEQ-IO-AREA (14:7) TO EDBNR (1:7)
               MOVE PRISSEQ-IO-AREA (21:60) TO RECC (1:60)
           END-EVALUATE.
 
       PRISSEQ-IDCHK SECTION.
       PRISSEQ-IDCHK-P.
           EVALUATE TRUE
           WHEN ( PRISSEQ-IO-AREA (1:1) = 'C' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       PRISSEQ-IDSET SECTION.
       PRISSEQ-IDSET-P.
           EVALUATE TRUE
           WHEN ( PRISSEQ-IO-AREA (1:1) = 'C' )
               SET I-01                    TO TRUE
           END-EVALUATE.
 
       PRISSEQ-CHK-LEVEL SECTION.
       PRISSEQ-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( PRISSEQ-IO-AREA (1:1) = 'C' )
               MOVE LOW-VALUES             TO PRISSEQ-LEVEL-01
               MOVE PRISSEQ-IO-AREA (2:3)  TO PRISSEQ-01-L2-FIRMA
               MOVE PRISSEQ-IO-AREA (14:7) TO PRISSEQ-01-L1-EDBNR
               IF  PRISSEQ-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  PRISSEQ-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  PRISSEQ-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  PRISSEQ-01-L2         TO THE-PRIOR-L2
               MOVE  PRISSEQ-01-L1         TO THE-PRIOR-L1
               SET PRISSEQ-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       TOTALER-PRINT-LINE SECTION.
       TOTALER-PRINT-LINE-P.
           IF  TOTALER-BEFORE-SKIP > 0
               PERFORM TOTALER-SKIP-BEFORE
           END-IF
           IF  TOTALER-BEFORE-SPACE > 0
               PERFORM TOTALER-SPACE-BEFORE
               IF  TOTALER-AFTER-SKIP > 0
                   PERFORM TOTALER-SKIP-AFTER
               END-IF
               IF  TOTALER-AFTER-SPACE > 0
                   PERFORM TOTALER-SPACE-AFTER
               END-IF
           ELSE
               IF  TOTALER-AFTER-SKIP > 0
                   PERFORM TOTALER-SKIP-AFTER
               END-IF
               PERFORM TOTALER-SPACE-AFTER
           END-IF
           IF  TOTALER-LINE-COUNT NOT < TOTALER-MAX-LINES
               MOVE 7                      TO TOTALER-AFTER-SKIP
           END-IF.
 
       TOTALER-SKIP-BEFORE SECTION.
       TOTALER-SKIP-BEFORE-P.
           WRITE TOTALER-IO-PRINT       AFTER ADVANCING PAGE
           MOVE 1                          TO TOTALER-LINE-COUNT
           MOVE 0                          TO TOTALER-BEFORE-SKIP
           INITIALIZE TOTALER-IO-AREA.
 
       TOTALER-SPACE-BEFORE SECTION.
       TOTALER-SPACE-BEFORE-P.
           WRITE TOTALER-IO-PRINT       AFTER TOTALER-BEFORE-SPACE
                                                                 LINES
           ADD TOTALER-BEFORE-SPACE        TO TOTALER-LINE-COUNT
           MOVE SPACES TO TOTALER-IO-AREA
           INITIALIZE TOTALER-IO-AREA
           MOVE 0                          TO TOTALER-BEFORE-SPACE.
 
       TOTALER-SKIP-AFTER SECTION.
       TOTALER-SKIP-AFTER-P.
           WRITE TOTALER-IO-PRINT      BEFORE ADVANCING PAGE
           MOVE 1                          TO TOTALER-LINE-COUNT
           MOVE 0                          TO TOTALER-AFTER-SKIP
           INITIALIZE TOTALER-IO-AREA.
 
       TOTALER-SPACE-AFTER SECTION.
       TOTALER-SPACE-AFTER-P.
           WRITE TOTALER-IO-PRINT      BEFORE TOTALER-AFTER-SPACE LINES
           ADD TOTALER-AFTER-SPACE         TO TOTALER-LINE-COUNT
           INITIALIZE TOTALER-IO-AREA
           MOVE 0                          TO TOTALER-AFTER-SPACE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-L1)
               MOVE SPACES TO NYEPRIS-IO-AREA
               INITIALIZE NYEPRIS-IO-AREA
               MOVE RECART                 TO NYEPRIS-IO-AREA (1:1)
               MOVE FIRMA                  TO NYEPRIS-IO-AREA (2:3)
               MOVE EDBNR                  TO NYEPRIS-IO-AREA (5:7)
               MOVE REFNR                  TO NYEPRIS-IO-AREA (12:5)
               MOVE SEQNR                  TO NYEPRIS-IO-AREA (17:4)
               MOVE RECC                   TO NYEPRIS-IO-AREA (21:60)
               WRITE NYEPRIS-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE ANT                    TO XO-50YY9
               MOVE XO-50YY9               TO TOTALER-IO-AREA (5:6)
               MOVE 'RECORDS PÅ PRIS.MASTER' TO TOTALER-IO-AREA (13:22)
               MOVE 01                     TO TOTALER-BEFORE-SKIP
               MOVE 3                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE ANTNL1                 TO XO-50YY9
               MOVE XO-50YY9               TO TOTALER-IO-AREA (5:6)
               MOVE 'ER FJERNET NÅ'        TO TOTALER-IO-AREA (13:13)
               MOVE 'DUBLETT PÅ EDB.NR'    TO TOTALER-IO-AREA (41:17)
               MOVE 3                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE ANTL1                  TO XO-50YY9
               MOVE XO-50YY9               TO TOTALER-IO-AREA (5:6)
               MOVE 'RECORDS TIL NYE.PRISER' TO TOTALER-IO-AREA (13:22)
               MOVE 3                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE 'KJØREDATO ER'         TO TOTALER-IO-AREA (12:12)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO TOTALER-IO-AREA (25:8)
               MOVE 2                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
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
           SET PRISSEQ-LEVEL-INIT          TO TRUE
           INITIALIZE PRISSEQ-DATA-FIELDS
           SET PRISSEQ-EOF-OFF             TO TRUE
           SET PRISSEQ-PROCESS             TO TRUE
           OPEN INPUT PRISSEQ
           OPEN OUTPUT NYEPRIS
           OPEN OUTPUT TOTALER
           INITIALIZE TOTALER-IO-AREA
           INITIALIZE TOTALER-DATA-FIELDS
           MOVE 57                         TO TOTALER-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PRISSEQ
           CLOSE NYEPRIS
           IF TOTALER-IO-AREA NOT = SPACES
             WRITE TOTALER-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO TOTALER-IO-AREA
           END-IF
           CLOSE TOTALER.
 
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
