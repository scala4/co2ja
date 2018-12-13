       IDENTIFICATION DIVISION.
       PROGRAM-ID. VOS160R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAM.......: VOS160, MERGER ORDRE MED OBJEKT FOR Å SLETTE  *
      *                         OBJEKT SOM IKKE HAR ORDRE.            *
      *  PROGRAMMERER..: MORTEN TUVRØNNINGEN                          *
      *  KJØRES I JOBB.: VOS08A                                       *
      *  LAGET DATO....: 28.01.03                                     *
      *  ENDRET DATO...:                                              *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VOS160.rpg
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
           SELECT VERKOBJ
               ASSIGN TO UT-S-VERKOBJ
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VERKOBJ-STATUS.
           SELECT VERKORD
               ASSIGN TO UT-S-VERKORD
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VERKORD-STATUS.
           SELECT SANOBJ
               ASSIGN TO UT-S-SANOBJ
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS SANOBJ-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD VERKOBJ
               BLOCK CONTAINS 1600
               RECORD CONTAINS 160.
       01  VERKOBJ-IO-AREA.
           05  VERKOBJ-IO-AREA-X           PICTURE X(160).
       FD VERKORD
               BLOCK CONTAINS 2400
               RECORD CONTAINS 24.
       01  VERKORD-IO-AREA.
           05  VERKORD-IO-AREA-X           PICTURE X(24).
       FD SANOBJ
               BLOCK CONTAINS 1600
               RECORD CONTAINS 160.
       01  SANOBJ-IO-AREA.
           05  SANOBJ-IO-AREA-X            PICTURE X(160).
      *BUGFILO O   F  80  80            PRINTERSYSLST
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
           10  VERKOBJ-STATUS              PICTURE 99 VALUE 0.
           10  VERKORD-STATUS              PICTURE 99 VALUE 0.
           10  SANOBJ-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  VERKOBJ-EOF-OFF         VALUE '0'.
               88  VERKOBJ-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VERKOBJ-READ-OFF        VALUE '0'.
               88  VERKOBJ-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VERKOBJ-PROCESS-OFF     VALUE '0'.
               88  VERKOBJ-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  VERKOBJ-LEVEL-INIT-OFF  VALUE '0'.
               88  VERKOBJ-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VERKORD-EOF-OFF         VALUE '0'.
               88  VERKORD-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VERKORD-READ-OFF        VALUE '0'.
               88  VERKORD-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VERKORD-PROCESS-OFF     VALUE '0'.
               88  VERKORD-PROCESS         VALUE '1'.
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
           05  VERKOBJ-LEVEL-01.
               10  VERKOBJ-01-L2.
                   15  VERKOBJ-01-L2-OBJFIR PICTURE X(3).
           05  VERKOBJ-DATA-FIELDS.
               10  OBJREC                  PICTURE X(160).
               10  OBJFIR                  PICTURE X(3).
               10  OBJREG                  PICTURE X(15).
               10  OBJKUN                  PICTURE X(6).
           05  VERKOBJ-MP                  PICTURE X(24).
           05  VERKOBJ-MC                  PICTURE X(24).
           05  VERKOBJ-M-01            REDEFINES VERKOBJ-MC.
               10  VERKOBJ-M-01-M3.
                   15  VERKOBJ-M-01-M3-OBJFIR-G.
                       20  VERKOBJ-M-01-M3-OBJFIR PICTURE X(3).
               10  VERKOBJ-M-01-M2.
                   15  VERKOBJ-M-01-M2-OBJREG-G.
                       20  VERKOBJ-M-01-M2-OBJREG PICTURE X(15).
               10  VERKOBJ-M-01-M1.
                   15  VERKOBJ-M-01-M1-OBJKUN-G.
                       20  VERKOBJ-M-01-M1-OBJKUN PICTURE X(6).
           05  VERKORD-DATA-FIELDS.
               10  ORDFIR                  PICTURE X(3).
               10  ORDREG                  PICTURE X(15).
               10  ORDKUN                  PICTURE X(6).
      ******************** CALC **************************************
           05  VERKORD-MP                  PICTURE X(24).
           05  VERKORD-MC                  PICTURE X(24).
           05  VERKORD-M-02            REDEFINES VERKORD-MC.
               10  VERKORD-M-02-M3.
                   15  VERKORD-M-02-M3-ORDFIR-G.
                       20  VERKORD-M-02-M3-ORDFIR PICTURE X(3).
               10  VERKORD-M-02-M2.
                   15  VERKORD-M-02-M2-ORDREG-G.
                       20  VERKORD-M-02-M2-ORDREG PICTURE X(15).
               10  VERKORD-M-02-M1.
                   15  VERKORD-M-02-M1-ORDKUN-G.
                       20  VERKORD-M-02-M1-ORDKUN PICTURE X(6).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  ANTILR-IO.
                   15  ANTILR              PICTURE S9(8).
               10  ANTINN-IO.
                   15  ANTINN              PICTURE S9(7).
               10  ANTSLR-IO.
                   15  ANTSLR              PICTURE S9(8).
               10  ANTSAN-IO.
                   15  ANTSAN              PICTURE S9(7).
               10  ANTULR-IO.
                   15  ANTULR              PICTURE S9(8).
               10  ANTUT-IO.
                   15  ANTUT               PICTURE S9(7).
           05  EDITTING-FIELDS.
               10  XO-70YY9R               PICTURE Z.ZZZ.ZZ9-.
               10  XO-80YY9R               PICTURE ZZ.ZZZ.ZZ9-.
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
           IF  VERKOBJ-PROCESS
               SET VERKOBJ-PROCESS-OFF     TO TRUE
               SET VERKOBJ-READ            TO TRUE
           END-IF
 
           IF  VERKOBJ-READ
               PERFORM VERKOBJ-GET
               SET VERKOBJ-READ-OFF        TO TRUE
               IF  NOT VERKOBJ-EOF
                   PERFORM VERKOBJ-MATCH-SET
               END-IF
           END-IF
 
           IF  VERKORD-PROCESS
               SET VERKORD-PROCESS-OFF     TO TRUE
               SET VERKORD-READ            TO TRUE
           END-IF
 
           IF  VERKORD-READ
               PERFORM VERKORD-GET
               SET VERKORD-READ-OFF        TO TRUE
               IF  NOT VERKORD-EOF
                   PERFORM VERKORD-MATCH-SET
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
 
           IF  VERKOBJ-PROCESS
               PERFORM VERKOBJ-IDSET
           END-IF
 
           IF  VERKORD-PROCESS
               PERFORM VERKORD-IDSET
           END-IF
 
           IF  VERKOBJ-PROCESS
               PERFORM VERKOBJ-CHK-LEVEL
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
 
           IF  VERKOBJ-PROCESS
               PERFORM VERKOBJ-FLDSET
           END-IF
 
           IF  VERKORD-PROCESS
               PERFORM VERKORD-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  VERKOBJ-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-01)
               ADD 1                       TO ANTILR
               ADD 1                       TO ANTINN
           END-IF
           IF  (I-01 AND NOT-I-MR)
               ADD 1                       TO ANTSLR
               ADD 1                       TO ANTSAN
           END-IF
           IF  (I-01 AND I-MR)
               ADD 1                       TO ANTULR
               ADD 1                       TO ANTUT
           END-IF.
 
       VERKOBJ-GET SECTION.
       VERKOBJ-GET-P.
           IF  VERKOBJ-EOF-OFF
               READ VERKOBJ
               AT END
                   SET VERKOBJ-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VERKOBJ-FLDSET SECTION.
       VERKOBJ-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VERKOBJ-IO-AREA (1:160) TO OBJREC (1:160)
               MOVE VERKOBJ-IO-AREA (1:3)  TO OBJFIR (1:3)
               MOVE VERKOBJ-IO-AREA (4:15) TO OBJREG (1:15)
               MOVE VERKOBJ-IO-AREA (19:6) TO OBJKUN (1:6)
           END-EVALUATE.
 
       VERKOBJ-IDSET SECTION.
       VERKOBJ-IDSET-P.
           SET I-01                        TO TRUE.
 
       VERKOBJ-CHK-LEVEL SECTION.
       VERKOBJ-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO VERKOBJ-LEVEL-01
               MOVE VERKOBJ-IO-AREA (1:3)  TO VERKOBJ-01-L2-OBJFIR
               IF  VERKOBJ-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VERKOBJ-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   END-EVALUATE
               END-IF
               MOVE  VERKOBJ-01-L2         TO THE-PRIOR-L2
               SET VERKOBJ-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       VERKOBJ-MATCH-SET SECTION.
       VERKOBJ-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE VERKOBJ-IO-AREA (1:3)  TO VERKOBJ-M-01-M3-OBJFIR
               MOVE VERKOBJ-IO-AREA (4:15) TO VERKOBJ-M-01-M2-OBJREG
               MOVE VERKOBJ-IO-AREA (19:6) TO VERKOBJ-M-01-M1-OBJKUN
           END-EVALUATE.
 
       VERKORD-GET SECTION.
       VERKORD-GET-P.
           IF  VERKORD-EOF-OFF
               READ VERKORD
               AT END
                   SET VERKORD-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VERKORD-FLDSET SECTION.
       VERKORD-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VERKORD-IO-AREA (1:3)  TO ORDFIR (1:3)
               MOVE VERKORD-IO-AREA (4:15) TO ORDREG (1:15)
               MOVE VERKORD-IO-AREA (19:6) TO ORDKUN (1:6)
           END-EVALUATE.
 
       VERKORD-IDSET SECTION.
       VERKORD-IDSET-P.
           SET I-02                        TO TRUE.
 
       VERKORD-MATCH-SET SECTION.
       VERKORD-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE VERKORD-IO-AREA (1:3)  TO VERKORD-M-02-M3-ORDFIR
               MOVE VERKORD-IO-AREA (4:15) TO VERKORD-M-02-M2-ORDREG
               MOVE VERKORD-IO-AREA (19:6) TO VERKORD-M-02-M1-ORDKUN
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
           IF  VERKOBJ-EOF
               MOVE HIGH-VALUES            TO VERKOBJ-MC
                                              VERKOBJ-MP
           END-IF
           IF  VERKORD-EOF
               MOVE HIGH-VALUES            TO VERKORD-MC
                                              VERKORD-MP
           END-IF
           IF  VERKOBJ-MC < VERKOBJ-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  VERKORD-MC < VERKORD-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  VERKOBJ-MC < VERKORD-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VERKOBJ-PROCESS     TO TRUE
                   MOVE VERKOBJ-MC         TO VERKOBJ-MP
                   IF  VERKOBJ-MC = VERKORD-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  VERKORD-MC < VERKOBJ-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VERKORD-PROCESS     TO TRUE
                   MOVE VERKORD-MC         TO VERKORD-MP
                   IF  VERKORD-MC = VERKOBJ-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  VERKOBJ-MC = VERKORD-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VERKOBJ-PROCESS     TO TRUE
                   MOVE VERKOBJ-MC         TO VERKOBJ-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-MR)
               MOVE SPACES TO SANOBJ-IO-AREA
               INITIALIZE SANOBJ-IO-AREA
               MOVE OBJREC                 TO SANOBJ-IO-AREA (1:160)
               WRITE SANOBJ-IO-AREA
           END-IF
           IF  (I-01 AND NOT-I-MR AND I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE OBJFIR                 TO LISTE-IO-AREA (1:3)
               MOVE OBJREG                 TO LISTE-IO-AREA (6:15)
               MOVE OBJKUN                 TO LISTE-IO-AREA (25:6)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L2)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMA -> :'           TO LISTE-IO-AREA (1:10)
               MOVE OBJFIR                 TO LISTE-IO-AREA (17:3)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'INN      :'           TO LISTE-IO-AREA (1:10)
               MOVE ANTINN                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (11:10)
               INITIALIZE ANTINN
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'SLETTET  :'           TO LISTE-IO-AREA (1:10)
               MOVE ANTSAN                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (11:10)
               INITIALIZE ANTSAN
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'UT       :'           TO LISTE-IO-AREA (1:10)
               MOVE ANTUT                  TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (11:10)
               INITIALIZE ANTUT
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTAL ** :'           TO LISTE-IO-AREA (1:10)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'INN      :'           TO LISTE-IO-AREA (1:10)
               MOVE ANTILR                 TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE-IO-AREA (10:11)
               INITIALIZE ANTILR
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'SLETTET  :'           TO LISTE-IO-AREA (1:10)
               MOVE ANTSLR                 TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE-IO-AREA (10:11)
               INITIALIZE ANTSLR
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'UT       :'           TO LISTE-IO-AREA (1:10)
               MOVE ANTULR                 TO XO-80YY9R
               MOVE XO-80YY9R              TO LISTE-IO-AREA (10:11)
               INITIALIZE ANTULR
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR AND I-U8 AND I-02)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ' '                    TO LISTE-IO-AREA (10:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
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
           SET VERKOBJ-LEVEL-INIT          TO TRUE
           INITIALIZE VERKOBJ-DATA-FIELDS
           SET VERKOBJ-EOF-OFF             TO TRUE
           SET VERKOBJ-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO VERKOBJ-MC
                                              VERKOBJ-MP
           OPEN INPUT VERKOBJ
           INITIALIZE VERKORD-DATA-FIELDS
           SET VERKORD-EOF-OFF             TO TRUE
           SET VERKORD-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO VERKORD-MC
                                              VERKORD-MP
           OPEN INPUT VERKORD
           OPEN OUTPUT SANOBJ
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE VERKOBJ
           CLOSE VERKORD
           CLOSE SANOBJ
           IF LISTE-IO-AREA NOT = SPACES
             WRITE LISTE-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE-IO-AREA
           END-IF
           CLOSE LISTE.
 
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
