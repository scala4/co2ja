       IDENTIFICATION DIVISION.
       PROGRAM-ID. VRG047R.
      **********************************************  Z-WIN-RPG2      *
      * PROGRAM............ VRG047               XX2000XXIRXXEN       *
      * PROGRAMMERER....... ESPEN LARSEN JCL=XVSAM34X                 *
      * PROGRAMERT......... 13/3-1992                                 *
      * PROGRAMMET MERGER VARE.OVERF.ALTERNATIV.FILE MED VAREKEY.FILE *
      * PÅ ALTERNATIV ALFA OG OPPSLAGSNR FOR Å FINNE EDB-NR.          *
      * DANNER OPPDATERINGSFILE TIL VAREMASTER.                       *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VRG047.rpg
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
           SELECT OPPSMAS
               ASSIGN TO OPPSMAS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OPPSMAS-STATUS.
           SELECT VOAINN
               ASSIGN TO UT-S-VOAINN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VOAINN-STATUS.
           SELECT VOAUT
               ASSIGN TO UT-S-VOAUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VOAUT-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD OPPSMAS
               RECORD CONTAINS 30.
       01  OPPSMAS-IO-AREA.
           05  OPPSMAS-IO-AREA-X           PICTURE X(30).
       FD VOAINN
               BLOCK CONTAINS 600
               RECORD CONTAINS 60.
       01  VOAINN-IO-AREA.
           05  VOAINN-IO-AREA-X            PICTURE X(60).
       FD VOAUT
               BLOCK CONTAINS 200
               RECORD CONTAINS 20.
       01  VOAUT-IO-AREA.
           05  VOAUT-IO-AREA-X             PICTURE X(20).
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  OPPSMAS-STATUS              PICTURE 99 VALUE 0.
           10  VOAINN-STATUS               PICTURE 99 VALUE 0.
           10  VOAUT-STATUS                PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  OPPSMAS-EOF-OFF         VALUE '0'.
               88  OPPSMAS-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  OPPSMAS-READ-OFF        VALUE '0'.
               88  OPPSMAS-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  OPPSMAS-PROCESS-OFF     VALUE '0'.
               88  OPPSMAS-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VOAINN-EOF-OFF          VALUE '0'.
               88  VOAINN-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VOAINN-READ-OFF         VALUE '0'.
               88  VOAINN-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VOAINN-PROCESS-OFF      VALUE '0'.
               88  VOAINN-PROCESS          VALUE '1'.
           05  LISTE-DATA-FIELDS.
               10  LISTE-AFTER-SPACE       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE-AFTER-SKIP        PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE-BEFORE-SPACE      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE-BEFORE-SKIP       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE-MAX-LINES         PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE-LINE-COUNT        PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE-CLR-IO            PICTURE X VALUE 'Y'.
           05  OPPSMAS-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  ALFART                  PICTURE X(18).
               10  RECART                  PICTURE X(1).
               10  OPPEDB                  PICTURE X(7).
           05  OPPSMAS-MP                  PICTURE X(21).
           05  OPPSMAS-MC                  PICTURE X(21).
           05  OPPSMAS-M-01            REDEFINES OPPSMAS-MC.
               10  OPPSMAS-M-01-M2.
                   15  OPPSMAS-M-01-M2-FIRMA-G.
                       20  OPPSMAS-M-01-M2-FIRMA PICTURE X(3).
               10  OPPSMAS-M-01-M1.
                   15  OPPSMAS-M-01-M1-ALFART-G.
                       20  OPPSMAS-M-01-M1-ALFART PICTURE X(18).
           05  VOAINN-DATA-FIELDS.
               10  FRAFNR                  PICTURE X(3).
               10  FRAEDB                  PICTURE X(7).
               10  TILFNR                  PICTURE X(3).
               10  ALTEDB                  PICTURE X(18).
               10  TILEDB                  PICTURE X(7).
               10  MRKFRA                  PICTURE X(1).
           05  VOAINN-MP                   PICTURE X(21).
           05  VOAINN-MC                   PICTURE X(21).
           05  VOAINN-M-02             REDEFINES VOAINN-MC.
               10  VOAINN-M-02-M2.
                   15  VOAINN-M-02-M2-TILFNR-G.
                       20  VOAINN-M-02-M2-TILFNR PICTURE X(3).
               10  VOAINN-M-02-M1.
                   15  VOAINN-M-02-M1-ALTEDB-G.
                       20  VOAINN-M-02-M1-ALTEDB PICTURE X(18).
           05  TEMPORARY-FIELDS.
               10  ANT01-IO.
                   15  ANT01               PICTURE S9(7).
               10  ANT02-IO.
                   15  ANT02               PICTURE S9(7).
               10  ANTNMR-IO.
                   15  ANTNMR              PICTURE S9(7).
               10  ANTLIK-IO.
                   15  ANTLIK              PICTURE S9(7).
               10  ANTOK-IO.
                   15  ANTOK               PICTURE S9(7).
           05  EDITTING-FIELDS.
               10  XO-70YY9                PICTURE Z.ZZZ.ZZ9.
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
           IF  OPPSMAS-PROCESS
               SET OPPSMAS-PROCESS-OFF     TO TRUE
               SET OPPSMAS-READ            TO TRUE
           END-IF
 
           IF  OPPSMAS-READ
               PERFORM OPPSMAS-GET
               SET OPPSMAS-READ-OFF        TO TRUE
               IF  NOT OPPSMAS-EOF
                   PERFORM OPPSMAS-MATCH-SET
               END-IF
           END-IF
 
           IF  VOAINN-PROCESS
               SET VOAINN-PROCESS-OFF      TO TRUE
               SET VOAINN-READ             TO TRUE
           END-IF
 
           IF  VOAINN-READ
               PERFORM VOAINN-GET
               SET VOAINN-READ-OFF         TO TRUE
               IF  NOT VOAINN-EOF
                   PERFORM VOAINN-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  OPPSMAS-PROCESS
               PERFORM OPPSMAS-IDSET
           END-IF
 
           IF  VOAINN-PROCESS
               PERFORM VOAINN-IDSET
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
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  OPPSMAS-PROCESS
               PERFORM OPPSMAS-FLDSET
           END-IF
 
           IF  VOAINN-PROCESS
               PERFORM VOAINN-FLDSET
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
           SET NOT-I-50                    TO TRUE
           SET NOT-I-52                    TO TRUE
           SET NOT-I-55                    TO TRUE
           IF  (I-01)
               SET NOT-I-11                TO TRUE
               IF  RECART = '1'
                   SET I-11                TO TRUE
               END-IF
               ADD 1                       TO ANT01
               GO TO SLUTT-T
           END-IF
           IF  (I-02)
               ADD 1                       TO ANT02
      *********************************
           END-IF
           SET NOT-I-55                    TO TRUE
           IF  MRKFRA > 'A'
               SET I-55                    TO TRUE
           END-IF
           IF  (I-02 AND I-55)
               GO TO SLUTT-T
      *********************************
           END-IF
           IF  (I-02 AND NOT-I-MR)
               ADD 1                       TO ANTNMR
               GO TO SLUTT-T
      *********************************
           END-IF
           IF  (I-02 AND I-MR AND NOT-I-11)
               ADD 1                       TO ANTNMR
               GO TO SLUTT-T
      *********************************
           END-IF
           SET NOT-I-52                    TO TRUE
           IF  OPPEDB = TILEDB
               SET I-52                    TO TRUE
           END-IF
           IF  (I-02 AND I-52)
               ADD 1                       TO ANTLIK
               GO TO SLUTT-T
      *********************************
           END-IF
           SET I-50                        TO TRUE
           IF  (I-02 AND I-50)
               ADD 1                       TO ANTOK
           END-IF.
 
       SLUTT-T.
      ******************************************************
           CONTINUE.
 
       OPPSMAS-GET SECTION.
       OPPSMAS-GET-P.
           IF  OPPSMAS-EOF-OFF
               READ OPPSMAS
               AT END
                   SET OPPSMAS-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       OPPSMAS-FLDSET SECTION.
       OPPSMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE OPPSMAS-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE OPPSMAS-IO-AREA (5:18) TO ALFART (1:18)
               MOVE OPPSMAS-IO-AREA (1:1)  TO RECART (1:1)
               MOVE OPPSMAS-IO-AREA (23:7) TO OPPEDB (1:7)
           END-EVALUATE.
 
       OPPSMAS-IDSET SECTION.
       OPPSMAS-IDSET-P.
           SET I-01                        TO TRUE.
 
       OPPSMAS-MATCH-SET SECTION.
       OPPSMAS-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE OPPSMAS-IO-AREA (2:3)  TO OPPSMAS-M-01-M2-FIRMA
               MOVE OPPSMAS-IO-AREA (5:18) TO OPPSMAS-M-01-M1-ALFART
           END-EVALUATE.
 
       VOAINN-GET SECTION.
       VOAINN-GET-P.
           IF  VOAINN-EOF-OFF
               READ VOAINN
               AT END
                   SET VOAINN-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VOAINN-FLDSET SECTION.
       VOAINN-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VOAINN-IO-AREA (1:3)   TO FRAFNR (1:3)
               MOVE VOAINN-IO-AREA (4:7)   TO FRAEDB (1:7)
               MOVE VOAINN-IO-AREA (42:3)  TO TILFNR (1:3)
               MOVE VOAINN-IO-AREA (18:18) TO ALTEDB (1:18)
               MOVE VOAINN-IO-AREA (45:7)  TO TILEDB (1:7)
               MOVE VOAINN-IO-AREA (59:1)  TO MRKFRA (1:1)
           END-EVALUATE.
 
       VOAINN-IDSET SECTION.
       VOAINN-IDSET-P.
           SET I-02                        TO TRUE.
 
       VOAINN-MATCH-SET SECTION.
       VOAINN-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE VOAINN-IO-AREA (42:3)  TO VOAINN-M-02-M2-TILFNR
               MOVE VOAINN-IO-AREA (18:18) TO VOAINN-M-02-M1-ALTEDB
           END-EVALUATE.
 
       LISTE-PRINT-LINE SECTION.
       LISTE-PRINT-LINE-P.
           IF  LISTE-BEFORE-SKIP > 0
               PERFORM LISTE-SKIP-BEFORE
           END-IF
           IF  LISTE-BEFORE-SPACE > 0
               PERFORM LISTE-SPACE-BEFORE
               IF  LISTE-AFTER-SKIP > 0
                   PERFORM LISTE-SKIP-AFTER
               END-IF
               IF  LISTE-AFTER-SPACE > 0
                   PERFORM LISTE-SPACE-AFTER
               END-IF
           ELSE
               IF  LISTE-AFTER-SKIP > 0
                   PERFORM LISTE-SKIP-AFTER
               END-IF
               PERFORM LISTE-SPACE-AFTER
           END-IF
           IF  LISTE-LINE-COUNT NOT < LISTE-MAX-LINES
               MOVE 7                      TO LISTE-AFTER-SKIP
           END-IF.
 
       LISTE-SKIP-BEFORE SECTION.
       LISTE-SKIP-BEFORE-P.
           WRITE LISTE-IO-PRINT         AFTER ADVANCING PAGE
           MOVE 1                          TO LISTE-LINE-COUNT
           MOVE 0                          TO LISTE-BEFORE-SKIP
           INITIALIZE LISTE-IO-AREA.
 
       LISTE-SPACE-BEFORE SECTION.
       LISTE-SPACE-BEFORE-P.
           WRITE LISTE-IO-PRINT         AFTER LISTE-BEFORE-SPACE LINES
           ADD LISTE-BEFORE-SPACE          TO LISTE-LINE-COUNT
           MOVE SPACES TO LISTE-IO-AREA
           INITIALIZE LISTE-IO-AREA
           MOVE 0                          TO LISTE-BEFORE-SPACE.
 
       LISTE-SKIP-AFTER SECTION.
       LISTE-SKIP-AFTER-P.
           WRITE LISTE-IO-PRINT        BEFORE ADVANCING PAGE
           MOVE 1                          TO LISTE-LINE-COUNT
           MOVE 0                          TO LISTE-AFTER-SKIP
           INITIALIZE LISTE-IO-AREA.
 
       LISTE-SPACE-AFTER SECTION.
       LISTE-SPACE-AFTER-P.
           WRITE LISTE-IO-PRINT        BEFORE LISTE-AFTER-SPACE LINES
           ADD LISTE-AFTER-SPACE           TO LISTE-LINE-COUNT
           INITIALIZE LISTE-IO-AREA
           MOVE 0                          TO LISTE-AFTER-SPACE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  OPPSMAS-EOF
               MOVE HIGH-VALUES            TO OPPSMAS-MC
                                              OPPSMAS-MP
           END-IF
           IF  VOAINN-EOF
               MOVE HIGH-VALUES            TO VOAINN-MC
                                              VOAINN-MP
           END-IF
           IF  OPPSMAS-MC < OPPSMAS-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  VOAINN-MC < VOAINN-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  OPPSMAS-MC < VOAINN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET OPPSMAS-PROCESS     TO TRUE
                   MOVE OPPSMAS-MC         TO OPPSMAS-MP
                   IF  OPPSMAS-MC = VOAINN-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  VOAINN-MC < OPPSMAS-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VOAINN-PROCESS      TO TRUE
                   MOVE VOAINN-MC          TO VOAINN-MP
                   IF  VOAINN-MC = OPPSMAS-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  OPPSMAS-MC = VOAINN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET OPPSMAS-PROCESS     TO TRUE
                   MOVE OPPSMAS-MC         TO OPPSMAS-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND I-55)
               MOVE SPACES TO VOAUT-IO-AREA
               INITIALIZE VOAUT-IO-AREA
               MOVE FRAFNR                 TO VOAUT-IO-AREA (1:3)
               MOVE FRAEDB                 TO VOAUT-IO-AREA (4:7)
               MOVE '       '              TO VOAUT-IO-AREA (11:7)
               MOVE MRKFRA                 TO VOAUT-IO-AREA (18:1)
               MOVE '1'                    TO VOAUT-IO-AREA (20:1)
               WRITE VOAUT-IO-AREA
           END-IF
           IF  (I-02 AND I-50)
               MOVE SPACES TO VOAUT-IO-AREA
               INITIALIZE VOAUT-IO-AREA
               MOVE TILFNR                 TO VOAUT-IO-AREA (1:3)
               MOVE TILEDB                 TO VOAUT-IO-AREA (4:7)
               MOVE OPPEDB                 TO VOAUT-IO-AREA (11:7)
               MOVE '  '                   TO VOAUT-IO-AREA (18:2)
               MOVE '2'                    TO VOAUT-IO-AREA (20:1)
               WRITE VOAUT-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'AVSTEMMING PROG. VRG047 ' TO LISTE-IO-AREA (1:24)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (1:1)
               MOVE '*'                    TO LISTE-IO-AREA (48:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*  ANT OPPSLAGS-RECORD L' TO LISTE-IO-AREA (1:24)
               MOVE 'EST. .  '             TO LISTE-IO-AREA (25:8)
               MOVE ANT01                  TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (37:9)
               MOVE '*'                    TO LISTE-IO-AREA (48:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*  ANT RECORDS M/ALT.NR.' TO LISTE-IO-AREA (1:24)
               MOVE ' LEST.  '             TO LISTE-IO-AREA (25:8)
               MOVE ANT02                  TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (37:9)
               MOVE '*'                    TO LISTE-IO-AREA (48:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*  ANT IKKE I VAREKEY.FI' TO LISTE-IO-AREA (1:24)
               MOVE 'LE.     '             TO LISTE-IO-AREA (25:8)
               MOVE ANTNMR                 TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (37:9)
               MOVE '*'                    TO LISTE-IO-AREA (48:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*  ANT REC. MED SAMME ED' TO LISTE-IO-AREA (1:24)
               MOVE 'BNR.    '             TO LISTE-IO-AREA (25:8)
               MOVE ANTLIK                 TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (37:9)
               MOVE '*'                    TO LISTE-IO-AREA (48:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*  ANT RECORDS OPPDAT. M' TO LISTE-IO-AREA (1:24)
               MOVE 'ED ALT.EDB'           TO LISTE-IO-AREA (25:10)
               MOVE ANTOK                  TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (37:9)
               MOVE '*'                    TO LISTE-IO-AREA (48:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (1:1)
               MOVE '*'                    TO LISTE-IO-AREA (48:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               MOVE 01                     TO LISTE-AFTER-SKIP
               PERFORM LISTE-PRINT-LINE
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
           INITIALIZE OPPSMAS-DATA-FIELDS
           SET OPPSMAS-EOF-OFF             TO TRUE
           SET OPPSMAS-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO OPPSMAS-MC
                                              OPPSMAS-MP
           OPEN INPUT OPPSMAS
           INITIALIZE VOAINN-DATA-FIELDS
           SET VOAINN-EOF-OFF              TO TRUE
           SET VOAINN-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO VOAINN-MC
                                              VOAINN-MP
           OPEN INPUT VOAINN
           OPEN OUTPUT VOAUT
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE OPPSMAS
           CLOSE VOAINN
           CLOSE VOAUT
           IF LISTE-IO-AREA NOT = SPACES
             WRITE LISTE-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE-IO-AREA
           END-IF
           CLOSE LISTE.
 
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
