       IDENTIFICATION DIVISION.
       PROGRAM-ID. VOS110R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM.......: VOS110, MERGER IKKE FERDIGE ORDRE FRA        * VOS10003
      *                          VERKSTEDSYSTEM MOT ORDRE.            * VOS10004
      *  PROGRAMMERER..: MORTEN TUVRØNNINGEN                          * VOS10005
      *  KJØRES I JOBB.: VOS09A/UB, IKKE FERDIGE VERKSTEDORDRE.       * VOS10006
      *  LAGET DATO....: 22.03.00                                     * VOS10007
      *  ENDRET........: 17.09.02 ENDRE KVITT.LISTE                   * VOS10008
      *  RETTET........:                                              * VOS10009
      *  INPUT.........: SEKVENSIELL VERKSTED ORDRENR FILE (VERKSAN), * VOS10010
      *                  SEKVENSIELL VERKSTED ORDRE FILE (VERK030).   * VOS10011
      *  BEHANDLING....: SANERER ORDRE ""SOM ER GÅTT UT PÅ DATO"".      * VOS10012
      *  OUTPUT........: SANERT SEKV. VERKSTED ORDRE FILE (VERK030).  * VOS10013
      ***************************************************************** VOS10014
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VOS110.rpg
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
           SELECT ORDREMI
               ASSIGN TO UT-S-ORDREMI
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ORDREMI-STATUS.
           SELECT ORDNRFI
               ASSIGN TO UT-S-ORDNRFI
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ORDNRFI-STATUS.
           SELECT ORDREMO
               ASSIGN TO UT-S-ORDREMO
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ORDREMO-STATUS.
           SELECT KLISTEO
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KLISTEO-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD ORDREMI
               BLOCK CONTAINS 328
               RECORD CONTAINS 164.
       01  ORDREMI-IO-AREA.
           05  ORDREMI-IO-AREA-X           PICTURE X(164).
       FD ORDNRFI
               BLOCK CONTAINS 200
               RECORD CONTAINS 100.
       01  ORDNRFI-IO-AREA.
           05  ORDNRFI-IO-AREA-X           PICTURE X(100).
       FD ORDREMO
               BLOCK CONTAINS 328
               RECORD CONTAINS 164.
       01  ORDREMO-IO-AREA.
           05  ORDREMO-IO-AREA-X           PICTURE X(164).
       FD KLISTEO
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  KLISTEO-IO-PRINT.
           05  KLISTEO-IO-AREA-CONTROL     PICTURE X VALUE ' '.
        02 KLISTEO-IO-AREA.
           05  KLISTEO-IO-AREA-X           PICTURE X(132).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  ORDREMI-STATUS              PICTURE 99 VALUE 0.
           10  ORDNRFI-STATUS              PICTURE 99 VALUE 0.
           10  ORDREMO-STATUS              PICTURE 99 VALUE 0.
           10  KLISTEO-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDREMI-EOF-OFF         VALUE '0'.
               88  ORDREMI-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDREMI-READ-OFF        VALUE '0'.
               88  ORDREMI-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDREMI-PROCESS-OFF     VALUE '0'.
               88  ORDREMI-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDNRFI-EOF-OFF         VALUE '0'.
               88  ORDNRFI-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDNRFI-READ-OFF        VALUE '0'.
               88  ORDNRFI-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDNRFI-PROCESS-OFF     VALUE '0'.
               88  ORDNRFI-PROCESS         VALUE '1'.
           05  KLISTEO-DATA-FIELDS.
               10  KLISTEO-AFTER-SPACE     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  KLISTEO-AFTER-SKIP      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  KLISTEO-BEFORE-SPACE    PICTURE 9(4) BINARY
                                           VALUE 0.
               10  KLISTEO-BEFORE-SKIP     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  KLISTEO-MAX-LINES       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  KLISTEO-LINE-COUNT      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  KLISTEO-CLR-IO          PICTURE X VALUE 'Y'.
           05  ORDREMI-DATA-FIELDS.
               10  ORMKEY                  PICTURE X(9).
               10  ORMREC                  PICTURE X(164).
           05  ORDREMI-MP                  PICTURE X(9).
           05  ORDREMI-MC                  PICTURE X(9).
           05  ORDREMI-M-01            REDEFINES ORDREMI-MC.
               10  ORDREMI-M-01-M1.
                   15  ORDREMI-M-01-M1-ORMKEY-G.
                       20  ORDREMI-M-01-M1-ORMKEY PICTURE X(9).
           05  ORDNRFI-DATA-FIELDS.
               10  ORNKEY                  PICTURE X(9).
           05  ORDNRFI-MP                  PICTURE X(9).
           05  ORDNRFI-MC                  PICTURE X(9).
           05  ORDNRFI-M-02            REDEFINES ORDNRFI-MC.
               10  ORDNRFI-M-02-M1.
                   15  ORDNRFI-M-02-M1-ORNKEY-G.
                       20  ORDNRFI-M-02-M1-ORNKEY PICTURE X(9).
           05  TEMPORARY-FIELDS.
               10  ANTINN-IO.
                   15  ANTINN              PICTURE S9(7).
               10  ANTSAN-IO.
                   15  ANTSAN              PICTURE S9(7).
               10  ANTUT-IO.
                   15  ANTUT               PICTURE S9(7).
               10  ANTORD-IO.
                   15  ANTORD              PICTURE S9(7).
           05  EDITTING-FIELDS.
               10  XO-70YY9R               PICTURE Z.ZZZ.ZZ9-.
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
               88  NOT-SET-I-OF            VALUE '0'.
               88  SET-I-OF                VALUE '1'.
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
           IF  NOT-SET-I-OF
               SET NOT-I-OF                TO TRUE
           END-IF
           SET NOT-SET-I-OF                TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  ORDREMI-PROCESS
               SET ORDREMI-PROCESS-OFF     TO TRUE
               SET ORDREMI-READ            TO TRUE
           END-IF
 
           IF  ORDREMI-READ
               PERFORM ORDREMI-GET
               SET ORDREMI-READ-OFF        TO TRUE
               IF  NOT ORDREMI-EOF
                   PERFORM ORDREMI-MATCH-SET
               END-IF
           END-IF
 
           IF  ORDNRFI-PROCESS
               SET ORDNRFI-PROCESS-OFF     TO TRUE
               SET ORDNRFI-READ            TO TRUE
           END-IF
 
           IF  ORDNRFI-READ
               PERFORM ORDNRFI-GET
               SET ORDNRFI-READ-OFF        TO TRUE
               IF  NOT ORDNRFI-EOF
                   PERFORM ORDNRFI-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  ORDREMI-PROCESS
               PERFORM ORDREMI-IDSET
           END-IF
 
           IF  ORDNRFI-PROCESS
               PERFORM ORDNRFI-IDSET
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
 
           IF  ORDREMI-PROCESS
               PERFORM ORDREMI-FLDSET
           END-IF
 
           IF  ORDNRFI-PROCESS
               PERFORM ORDNRFI-FLDSET
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
               ADD 1                       TO ANTINN
           END-IF
           IF  (I-01 AND I-MR)
               ADD 1                       TO ANTSAN
           END-IF
           IF  (I-01 AND NOT-I-MR)
               ADD 1                       TO ANTUT
           END-IF
           IF  (I-02)
               ADD 1                       TO ANTORD
           END-IF.
 
       ORDREMI-GET SECTION.
       ORDREMI-GET-P.
           IF  ORDREMI-EOF-OFF
               READ ORDREMI
               AT END
                   SET ORDREMI-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       ORDREMI-FLDSET SECTION.
       ORDREMI-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE ORDREMI-IO-AREA (2:9)  TO ORMKEY (1:9)
               MOVE ORDREMI-IO-AREA (1:164) TO ORMREC (1:164)
           END-EVALUATE.
 
       ORDREMI-IDSET SECTION.
       ORDREMI-IDSET-P.
           SET I-01                        TO TRUE.
 
       ORDREMI-MATCH-SET SECTION.
       ORDREMI-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE ORDREMI-IO-AREA (2:9)  TO ORDREMI-M-01-M1-ORMKEY
           END-EVALUATE.
 
       ORDNRFI-GET SECTION.
       ORDNRFI-GET-P.
           IF  ORDNRFI-EOF-OFF
               READ ORDNRFI
               AT END
                   SET ORDNRFI-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       ORDNRFI-FLDSET SECTION.
       ORDNRFI-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE ORDNRFI-IO-AREA (2:9)  TO ORNKEY (1:9)
           END-EVALUATE.
 
       ORDNRFI-IDSET SECTION.
       ORDNRFI-IDSET-P.
           SET I-02                        TO TRUE.
 
       ORDNRFI-MATCH-SET SECTION.
       ORDNRFI-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE ORDNRFI-IO-AREA (2:9)  TO ORDNRFI-M-02-M1-ORNKEY
           END-EVALUATE.
 
       KLISTEO-PRINT-LINE SECTION.
       KLISTEO-PRINT-LINE-P.
           IF  KLISTEO-BEFORE-SKIP > 0
               PERFORM KLISTEO-SKIP-BEFORE
           END-IF
           IF  KLISTEO-BEFORE-SPACE > 0
               PERFORM KLISTEO-SPACE-BEFORE
               IF  KLISTEO-AFTER-SKIP > 0
                   PERFORM KLISTEO-SKIP-AFTER
               END-IF
               IF  KLISTEO-AFTER-SPACE > 0
                   PERFORM KLISTEO-SPACE-AFTER
               END-IF
           ELSE
               IF  KLISTEO-AFTER-SKIP > 0
                   PERFORM KLISTEO-SKIP-AFTER
               END-IF
               PERFORM KLISTEO-SPACE-AFTER
           END-IF
           IF  KLISTEO-LINE-COUNT NOT < KLISTEO-MAX-LINES
               SET I-OF                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OF            TO TRUE
               END-IF
           END-IF.
 
       KLISTEO-SKIP-BEFORE SECTION.
       KLISTEO-SKIP-BEFORE-P.
           WRITE KLISTEO-IO-PRINT       AFTER ADVANCING PAGE
           MOVE 1                          TO KLISTEO-LINE-COUNT
           MOVE 0                          TO KLISTEO-BEFORE-SKIP
           INITIALIZE KLISTEO-IO-AREA.
 
       KLISTEO-SPACE-BEFORE SECTION.
       KLISTEO-SPACE-BEFORE-P.
           WRITE KLISTEO-IO-PRINT       AFTER KLISTEO-BEFORE-SPACE
                                                                 LINES
           ADD KLISTEO-BEFORE-SPACE        TO KLISTEO-LINE-COUNT
           MOVE SPACES TO KLISTEO-IO-AREA
           INITIALIZE KLISTEO-IO-AREA
           MOVE 0                          TO KLISTEO-BEFORE-SPACE.
 
       KLISTEO-SKIP-AFTER SECTION.
       KLISTEO-SKIP-AFTER-P.
           WRITE KLISTEO-IO-PRINT      BEFORE ADVANCING PAGE
           MOVE 1                          TO KLISTEO-LINE-COUNT
           MOVE 0                          TO KLISTEO-AFTER-SKIP
           INITIALIZE KLISTEO-IO-AREA.
 
       KLISTEO-SPACE-AFTER SECTION.
       KLISTEO-SPACE-AFTER-P.
           WRITE KLISTEO-IO-PRINT      BEFORE KLISTEO-AFTER-SPACE LINES
           ADD KLISTEO-AFTER-SPACE         TO KLISTEO-LINE-COUNT
           INITIALIZE KLISTEO-IO-AREA
           MOVE 0                          TO KLISTEO-AFTER-SPACE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  ORDREMI-EOF
               MOVE HIGH-VALUES            TO ORDREMI-MC
                                              ORDREMI-MP
           END-IF
           IF  ORDNRFI-EOF
               MOVE HIGH-VALUES            TO ORDNRFI-MC
                                              ORDNRFI-MP
           END-IF
           IF  ORDREMI-MC < ORDREMI-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  ORDNRFI-MC < ORDNRFI-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  ORDREMI-MC < ORDNRFI-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET ORDREMI-PROCESS     TO TRUE
                   MOVE ORDREMI-MC         TO ORDREMI-MP
                   IF  ORDREMI-MC = ORDNRFI-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  ORDNRFI-MC < ORDREMI-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET ORDNRFI-PROCESS     TO TRUE
                   MOVE ORDNRFI-MC         TO ORDNRFI-MP
                   IF  ORDNRFI-MC = ORDREMI-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  ORDREMI-MC = ORDNRFI-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET ORDREMI-PROCESS     TO TRUE
                   MOVE ORDREMI-MC         TO ORDREMI-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (NOT-I-MR AND I-01)
               MOVE SPACES TO ORDREMO-IO-AREA
               INITIALIZE ORDREMO-IO-AREA
               MOVE ORMREC                 TO ORDREMO-IO-AREA (1:164)
               WRITE ORDREMO-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'KONTROLL-LISTE MERGING A' TO KLISTEO-IO-AREA
                                                                (1:24)
               MOVE 'V VERKSTED-ORDRE.       ' TO KLISTEO-IO-AREA
                                                               (25:24)
               MOVE 'DATO:'                TO KLISTEO-IO-AREA (53:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO KLISTEO-IO-AREA (58:8)
               MOVE 'JOB=VOS09A '          TO KLISTEO-IO-AREA (95:11)
               MOVE 'PROGRAM=VOS110'       TO KLISTEO-IO-AREA (107:14)
               IF  I-U1
                   MOVE 01                 TO KLISTEO-BEFORE-SKIP
                   MOVE 2                  TO KLISTEO-BEFORE-SPACE
                   MOVE 2                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'KONTROLL-LISTE MERGING A' TO KLISTEO-IO-AREA
                                                                (1:24)
               MOVE 'V VERKSTED-ORDRE.       ' TO KLISTEO-IO-AREA
                                                               (25:24)
               MOVE 'DATO:'                TO KLISTEO-IO-AREA (53:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO KLISTEO-IO-AREA (58:8)
               MOVE 'JOB=VOS09A '          TO KLISTEO-IO-AREA (95:11)
               MOVE 'PROGRAM=VOS110'       TO KLISTEO-IO-AREA (107:14)
               IF  I-U1
                   MOVE 01                 TO KLISTEO-BEFORE-SKIP
                   MOVE 2                  TO KLISTEO-BEFORE-SPACE
                   MOVE 2                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ANTALL VERK030         :' TO KLISTEO-IO-AREA
                                                                (1:24)
               MOVE ANTINN                 TO XO-70YY9R
               MOVE XO-70YY9R              TO KLISTEO-IO-AREA (31:10)
               IF  I-U1
                   MOVE 1                  TO KLISTEO-BEFORE-SPACE
                   MOVE 1                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ANTALL FERDIGE         :' TO KLISTEO-IO-AREA
                                                                (1:24)
               MOVE ANTSAN                 TO XO-70YY9R
               MOVE XO-70YY9R              TO KLISTEO-IO-AREA (31:10)
               IF  I-U1
                   MOVE 1                  TO KLISTEO-BEFORE-SPACE
                   MOVE 1                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ANTALL IKKE FERDIGE    :' TO KLISTEO-IO-AREA
                                                                (1:24)
               MOVE ANTUT                  TO XO-70YY9R
               MOVE XO-70YY9R              TO KLISTEO-IO-AREA (31:10)
               IF  I-U1
                   MOVE 1                  TO KLISTEO-BEFORE-SPACE
                   MOVE 1                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'ANTALL ORDNRM          :' TO KLISTEO-IO-AREA
                                                                (1:24)
               MOVE ANTORD                 TO XO-70YY9R
               MOVE XO-70YY9R              TO KLISTEO-IO-AREA (31:10)
               IF  I-U1
                   MOVE 1                  TO KLISTEO-BEFORE-SPACE
                   MOVE 1                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
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
           MOVE 2                          TO LR-CHECK
           INITIALIZE ORDREMI-DATA-FIELDS
           SET ORDREMI-EOF-OFF             TO TRUE
           SET ORDREMI-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO ORDREMI-MC
                                              ORDREMI-MP
           OPEN INPUT ORDREMI
           INITIALIZE ORDNRFI-DATA-FIELDS
           SET ORDNRFI-EOF-OFF             TO TRUE
           SET ORDNRFI-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO ORDNRFI-MC
                                              ORDNRFI-MP
           OPEN INPUT ORDNRFI
           OPEN OUTPUT ORDREMO
           IF I-U1
               OPEN OUTPUT KLISTEO
           END-IF
           INITIALIZE KLISTEO-IO-AREA
           INITIALIZE KLISTEO-DATA-FIELDS
           MOVE 57                         TO KLISTEO-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE ORDREMI
           CLOSE ORDNRFI
           CLOSE ORDREMO
           IF I-U1
               CLOSE KLISTEO
           END-IF.
 
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
