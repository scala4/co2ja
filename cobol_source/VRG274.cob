       IDENTIFICATION DIVISION.
       PROGRAM-ID. VRG274R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAMNAVN: VRG274, PLUKKER UT FIRMA.                        *
      * LAGET AV   : M. TUVRØNNINGEN.                                 *
      * ENDRINGER  : 01.12.00 UTVIDET ANTALLSFELT.                    *
      * FÅR        : VAREMASTER (ANTALLSBEHOLDNINGER OG SELVKOST),    *
      *              PARAMFIL (FIRMA)                                 *
      * GJØR       : MERGER OG PLUKKER UT VED MATCH.                  *
      * GIR        : BEHOLDNINGSFILE FOR AKTUELL MÅNED.               *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VRG274.rpg
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
           SELECT FIRPARM
               ASSIGN TO UT-S-FIRPARM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FIRPARM-STATUS.
           SELECT VAREREI
               ASSIGN TO VAREREI
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VAREREI-STATUS.
           SELECT VAREREO
               ASSIGN TO UT-S-VAREREO
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VAREREO-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD FIRPARM
               BLOCK CONTAINS 30
               RECORD CONTAINS 3.
       01  FIRPARM-IO-AREA.
           05  FIRPARM-IO-AREA-X           PICTURE X(3).
       FD VAREREI
               RECORD CONTAINS 200.
       01  VAREREI-IO-AREA.
           05  VAREREI-IO-AREA-X           PICTURE X(200).
       FD VAREREO
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  VAREREO-IO-AREA.
           05  VAREREO-IO-AREA-X           PICTURE X(200).
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
           10  FIRPARM-STATUS              PICTURE 99 VALUE 0.
           10  VAREREI-STATUS              PICTURE 99 VALUE 0.
           10  VAREREO-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  FIRPARM-EOF-OFF         VALUE '0'.
               88  FIRPARM-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FIRPARM-READ-OFF        VALUE '0'.
               88  FIRPARM-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FIRPARM-PROCESS-OFF     VALUE '0'.
               88  FIRPARM-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  FIRPARM-LEVEL-INIT-OFF  VALUE '0'.
               88  FIRPARM-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREREI-EOF-OFF         VALUE '0'.
               88  VAREREI-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREREI-READ-OFF        VALUE '0'.
               88  VAREREI-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREREI-PROCESS-OFF     VALUE '0'.
               88  VAREREI-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  VAREREI-LEVEL-INIT-OFF  VALUE '0'.
               88  VAREREI-LEVEL-INIT      VALUE '1'.
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
           05  FIRPARM-LEVEL-01.
               10  FIRPARM-01-L1.
                   15  FIRPARM-01-L1-FIRMA PICTURE X(3).
           05  FIRPARM-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
           05  FIRPARM-MP                  PICTURE X(3).
           05  FIRPARM-MC                  PICTURE X(3).
           05  FIRPARM-M-01            REDEFINES FIRPARM-MC.
               10  FIRPARM-M-01-M1.
                   15  FIRPARM-M-01-M1-FIRMA-G.
                       20  FIRPARM-M-01-M1-FIRMA PICTURE X(3).
           05  VAREREI-LEVEL-02.
               10  VAREREI-02-L1.
                   15  VAREREI-02-L1-FIRMA PICTURE X(3).
           05  VAREREI-DATA-FIELDS.
               10  REC200                  PICTURE X(200).
           05  VAREREI-MP                  PICTURE X(3).
           05  VAREREI-MC                  PICTURE X(3).
           05  VAREREI-M-02            REDEFINES VAREREI-MC.
               10  VAREREI-M-02-M1.
                   15  VAREREI-M-02-M1-FIRMA-G.
                       20  VAREREI-M-02-M1-FIRMA PICTURE X(3).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  ANTL1-IO.
                   15  ANTL1               PICTURE S9(9).
           05  EDITTING-FIELDS.
               10  XO-90YY9R               PICTURE ZZZ.ZZZ.ZZ9-.
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
               88  NOT-SET-I-OV            VALUE '0'.
               88  SET-I-OV                VALUE '1'.
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
 
           PERFORM HEADING-OUTPUT
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
           PERFORM HEADING-OUTPUT
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
           IF  NOT-SET-I-OV
               SET NOT-I-OV                TO TRUE
           END-IF
           SET NOT-SET-I-OV                TO TRUE
 
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
           IF  FIRPARM-PROCESS
               SET FIRPARM-PROCESS-OFF     TO TRUE
               SET FIRPARM-READ            TO TRUE
           END-IF
 
           IF  FIRPARM-READ
               PERFORM FIRPARM-GET
               SET FIRPARM-READ-OFF        TO TRUE
               IF  NOT FIRPARM-EOF
                   PERFORM FIRPARM-MATCH-SET
               END-IF
           END-IF
 
           IF  VAREREI-PROCESS
               SET VAREREI-PROCESS-OFF     TO TRUE
               SET VAREREI-READ            TO TRUE
           END-IF
 
           IF  VAREREI-READ
               PERFORM VAREREI-GET
               SET VAREREI-READ-OFF        TO TRUE
               IF  NOT VAREREI-EOF
                   PERFORM VAREREI-MATCH-SET
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
 
           IF  FIRPARM-PROCESS
               PERFORM FIRPARM-IDSET
           END-IF
 
           IF  VAREREI-PROCESS
               PERFORM VAREREI-IDSET
           END-IF
 
           IF  FIRPARM-PROCESS
               PERFORM FIRPARM-CHK-LEVEL
           END-IF
 
           IF  VAREREI-PROCESS
               PERFORM VAREREI-CHK-LEVEL
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
           PERFORM HEADING-OVERFLOW
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  FIRPARM-PROCESS
               PERFORM FIRPARM-FLDSET
           END-IF
 
           IF  VAREREI-PROCESS
               PERFORM VAREREI-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  FIRPARM-PROCESS
           OR  VAREREI-PROCESS
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
               SET NOT-I-10                TO TRUE
           END-IF
           IF  (I-02 AND I-MR)
               ADD 1                       TO ANTL1
               SET NOT-I-10                TO TRUE
               IF  ANTL1 NOT = 0
                   SET I-10                TO TRUE
               END-IF
           END-IF.
 
       FIRPARM-GET SECTION.
       FIRPARM-GET-P.
           IF  FIRPARM-EOF-OFF
               READ FIRPARM
               AT END
                   SET FIRPARM-EOF         TO TRUE
               END-READ
           END-IF.
 
       FIRPARM-FLDSET SECTION.
       FIRPARM-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRPARM-IO-AREA (1:3)  TO FIRMA (1:3)
           END-EVALUATE.
 
       FIRPARM-IDSET SECTION.
       FIRPARM-IDSET-P.
           SET I-01                        TO TRUE.
 
       FIRPARM-CHK-LEVEL SECTION.
       FIRPARM-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO FIRPARM-LEVEL-01
               MOVE FIRPARM-IO-AREA (1:3)  TO FIRPARM-01-L1-FIRMA
               IF  FIRPARM-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  FIRPARM-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  FIRPARM-01-L1         TO THE-PRIOR-L1
               SET FIRPARM-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       FIRPARM-MATCH-SET SECTION.
       FIRPARM-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRPARM-IO-AREA (1:3)  TO FIRPARM-M-01-M1-FIRMA
           END-EVALUATE.
 
       VAREREI-GET SECTION.
       VAREREI-GET-P.
           IF  VAREREI-EOF-OFF
               READ VAREREI
               AT END
                   SET VAREREI-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VAREREI-FLDSET SECTION.
       VAREREI-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREREI-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE VAREREI-IO-AREA (1:200) TO REC200 (1:200)
           END-EVALUATE.
 
       VAREREI-IDSET SECTION.
       VAREREI-IDSET-P.
           SET I-02                        TO TRUE.
 
       VAREREI-CHK-LEVEL SECTION.
       VAREREI-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO VAREREI-LEVEL-02
               MOVE VAREREI-IO-AREA (3:3)  TO VAREREI-02-L1-FIRMA
               IF  VAREREI-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VAREREI-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  VAREREI-02-L1         TO THE-PRIOR-L1
               SET VAREREI-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       VAREREI-MATCH-SET SECTION.
       VAREREI-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREREI-IO-AREA (3:3)  TO VAREREI-M-02-M1-FIRMA
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
               SET I-OV                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OV            TO TRUE
               END-IF
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
           IF  FIRPARM-EOF
               MOVE HIGH-VALUES            TO FIRPARM-MC
                                              FIRPARM-MP
           END-IF
           IF  VAREREI-EOF
               MOVE HIGH-VALUES            TO VAREREI-MC
                                              VAREREI-MP
           END-IF
           IF  FIRPARM-MC < FIRPARM-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  VAREREI-MC < VAREREI-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  FIRPARM-MC < VAREREI-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FIRPARM-PROCESS     TO TRUE
                   MOVE FIRPARM-MC         TO FIRPARM-MP
                   IF  FIRPARM-MC = VAREREI-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  VAREREI-MC < FIRPARM-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VAREREI-PROCESS     TO TRUE
                   MOVE VAREREI-MC         TO VAREREI-MP
                   IF  VAREREI-MC = FIRPARM-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  FIRPARM-MC = VAREREI-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FIRPARM-PROCESS     TO TRUE
                   MOVE FIRPARM-MC         TO FIRPARM-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND I-MR)
               MOVE SPACES TO VAREREO-IO-AREA
               INITIALIZE VAREREO-IO-AREA
               MOVE REC200                 TO VAREREO-IO-AREA (1:200)
               WRITE VAREREO-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMA MED PÅ OPPFATERING' TO LISTE-IO-AREA (1:24)
               MOVE ' AV AVD.LAGER PR VGRP.  ' TO LISTE-IO-AREA (25:24)
               MOVE 'PROGRAM VRG274.         ' TO LISTE-IO-AREA (49:24)
               IF  I-U1
                   MOVE 01                 TO LISTE-BEFORE-SKIP
                   MOVE 3                  TO LISTE-BEFORE-SPACE
                   MOVE 3                  TO LISTE-AFTER-SPACE
                   PERFORM LISTE-PRINT-LINE
               END-IF
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OV)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMA MED PÅ OPPFATERING' TO LISTE-IO-AREA (1:24)
               MOVE ' AV AVD.LAGER PR VGRP.  ' TO LISTE-IO-AREA (25:24)
               MOVE 'PROGRAM VRG274.         ' TO LISTE-IO-AREA (49:24)
               IF  I-U1
                   MOVE 01                 TO LISTE-BEFORE-SKIP
                   MOVE 3                  TO LISTE-BEFORE-SPACE
                   MOVE 3                  TO LISTE-AFTER-SPACE
                   PERFORM LISTE-PRINT-LINE
               END-IF
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-10)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMA:'               TO LISTE-IO-AREA (5:6)
               MOVE FIRMA                  TO LISTE-IO-AREA (13:3)
               MOVE 'ANT:'                 TO LISTE-IO-AREA (17:4)
               MOVE ANTL1                  TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (29:12)
               INITIALIZE ANTL1
               IF  I-U1
                   MOVE 1                  TO LISTE-BEFORE-SPACE
                   PERFORM LISTE-PRINT-LINE
               END-IF
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
           SET FIRPARM-LEVEL-INIT          TO TRUE
           INITIALIZE FIRPARM-DATA-FIELDS
           SET FIRPARM-EOF-OFF             TO TRUE
           SET FIRPARM-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO FIRPARM-MC
                                              FIRPARM-MP
           OPEN INPUT FIRPARM
           SET VAREREI-LEVEL-INIT          TO TRUE
           INITIALIZE VAREREI-DATA-FIELDS
           SET VAREREI-EOF-OFF             TO TRUE
           SET VAREREI-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO VAREREI-MC
                                              VAREREI-MP
           OPEN INPUT VAREREI
           OPEN OUTPUT VAREREO
           IF I-U1
               OPEN OUTPUT LISTE
           END-IF
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FIRPARM
           CLOSE VAREREI
           CLOSE VAREREO
           IF I-U1
               CLOSE LISTE
           END-IF.
 
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
