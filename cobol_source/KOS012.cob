       IDENTIFICATION DIVISION.
       PROGRAM-ID. KOS012R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM.......: KOS012, SANERING AV KASSE.TERMINAL           *
      *  PROGRAMMERER..: MORTEN TUVRØNNINGEN                          *
      *  KJØRES I JOBB.: VSA.KASSTER                                  *
      *  LAGET DATO....: 13.03.12                                     *
      *  ENDRET........: 20.05.12 HVIS ""FAST"" RECORD SLETTES DEN IKKE *
      *                  12.06.12 ENDRET PRIMARY FILE TIL FIRMAKT     *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: KOS012.rpg
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
           SELECT FIRMAKT
               ASSIGN TO UT-S-FIRMAKT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FIRMAKT-STATUS.
           SELECT KASSTEI
               ASSIGN TO UT-S-KASSTEI
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KASSTEI-STATUS.
           SELECT KASSTEO
               ASSIGN TO UT-S-KASSTEO
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KASSTEO-STATUS.
           SELECT KLISTEO
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KLISTEO-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD FIRMAKT
               BLOCK CONTAINS 40
               RECORD CONTAINS 4.
       01  FIRMAKT-IO-AREA.
           05  FIRMAKT-IO-AREA-X           PICTURE X(4).
       FD KASSTEI
               BLOCK CONTAINS 3000
               RECORD CONTAINS 300.
       01  KASSTEI-IO-AREA.
           05  KASSTEI-IO-AREA-X           PICTURE X(300).
       FD KASSTEO
               BLOCK CONTAINS 3000
               RECORD CONTAINS 300.
       01  KASSTEO-IO-AREA.
           05  KASSTEO-IO-AREA-X           PICTURE X(300).
      *BUGFILO O   F  80  80            PRINTERSYSLST
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
           10  FIRMAKT-STATUS              PICTURE 99 VALUE 0.
           10  KASSTEI-STATUS              PICTURE 99 VALUE 0.
           10  KASSTEO-STATUS              PICTURE 99 VALUE 0.
           10  KLISTEO-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  FIRMAKT-EOF-OFF         VALUE '0'.
               88  FIRMAKT-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FIRMAKT-READ-OFF        VALUE '0'.
               88  FIRMAKT-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FIRMAKT-PROCESS-OFF     VALUE '0'.
               88  FIRMAKT-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KASSTEI-EOF-OFF         VALUE '0'.
               88  KASSTEI-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KASSTEI-READ-OFF        VALUE '0'.
               88  KASSTEI-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KASSTEI-PROCESS-OFF     VALUE '0'.
               88  KASSTEI-PROCESS         VALUE '1'.
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
           05  FIRMAKT-DATA-FIELDS.
               10  FIRKEY                  PICTURE X(3).
           05  FIRMAKT-MP                  PICTURE X(3).
           05  FIRMAKT-MC                  PICTURE X(3).
           05  FIRMAKT-M-01            REDEFINES FIRMAKT-MC.
               10  FIRMAKT-M-01-M1.
                   15  FIRMAKT-M-01-M1-FIRKEY-G.
                       20  FIRMAKT-M-01-M1-FIRKEY PICTURE X(3).
           05  KASSTEI-DATA-FIELDS.
               10  BELO-ELGP-IO.
                   15  BELO-ELGP           PICTURE S9(9)V9(2).
               10  STATUS-X                PICTURE X(1).
               10  REC120                  PICTURE X(120).
               10  REC300                  PICTURE X(180).
           05  KASSTEI-MP                  PICTURE X(3).
           05  KASSTEI-MC                  PICTURE X(3).
           05  KASSTEI-M-02            REDEFINES KASSTEI-MC.
               10  KASSTEI-M-02-M1.
                   15  KASSTEI-M-02-M1-FIRKEY-G.
                       20  KASSTEI-M-02-M1-FIRKEY PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  ANTINN-IO.
                   15  ANTINN              PICTURE S9(7).
               10  ANTSAN-IO.
                   15  ANTSAN              PICTURE S9(7).
               10  ANTUT-IO.
                   15  ANTUT               PICTURE S9(7).
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
           IF  FIRMAKT-PROCESS
               SET FIRMAKT-PROCESS-OFF     TO TRUE
               SET FIRMAKT-READ            TO TRUE
           END-IF
 
           IF  FIRMAKT-READ
               PERFORM FIRMAKT-GET
               SET FIRMAKT-READ-OFF        TO TRUE
               IF  NOT FIRMAKT-EOF
                   PERFORM FIRMAKT-MATCH-SET
               END-IF
           END-IF
 
           IF  KASSTEI-PROCESS
               SET KASSTEI-PROCESS-OFF     TO TRUE
               SET KASSTEI-READ            TO TRUE
           END-IF
 
           IF  KASSTEI-READ
               PERFORM KASSTEI-GET
               SET KASSTEI-READ-OFF        TO TRUE
               IF  NOT KASSTEI-EOF
                   PERFORM KASSTEI-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  FIRMAKT-PROCESS
               PERFORM FIRMAKT-IDSET
           END-IF
 
           IF  KASSTEI-PROCESS
               PERFORM KASSTEI-IDSET
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
 
           IF  FIRMAKT-PROCESS
               PERFORM FIRMAKT-FLDSET
           END-IF
 
           IF  KASSTEI-PROCESS
               PERFORM KASSTEI-FLDSET
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
           SET NOT-I-10                    TO TRUE
           IF  (I-02)
               ADD 1                       TO ANTINN
           END-IF
           IF  (I-02 AND NOT-I-MR)
               SET I-10                    TO TRUE
           END-IF
           IF  (I-02 AND NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  STATUS-X = 'O'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  BELO-ELGP = 0
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-MR AND I-10)
               SET NOT-I-10                TO TRUE
               IF  STATUS-X NOT = 'F'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-10)
               ADD 1                       TO ANTSAN
      *                    MOVE "REC120  "BUGFL1  8        LEDETXT DEBUG
      *          BUGFL1    DEBUGBUGFILO   REC120           VIS FELT/IND
           END-IF
           IF  (I-02 AND NOT-I-10)
               ADD 1                       TO ANTUT
           END-IF.
 
       FIRMAKT-GET SECTION.
       FIRMAKT-GET-P.
           IF  FIRMAKT-EOF-OFF
               READ FIRMAKT
               AT END
                   SET FIRMAKT-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FIRMAKT-FLDSET SECTION.
       FIRMAKT-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAKT-IO-AREA (1:3)  TO FIRKEY (1:3)
           END-EVALUATE.
 
       FIRMAKT-IDSET SECTION.
       FIRMAKT-IDSET-P.
           SET I-01                        TO TRUE.
 
       FIRMAKT-MATCH-SET SECTION.
       FIRMAKT-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAKT-IO-AREA (1:3)  TO FIRMAKT-M-01-M1-FIRKEY
           END-EVALUATE.
 
       KASSTEI-GET SECTION.
       KASSTEI-GET-P.
           IF  KASSTEI-EOF-OFF
               READ KASSTEI
               AT END
                   SET KASSTEI-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KASSTEI-FLDSET SECTION.
       KASSTEI-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KASSTEI-IO-AREA (1:3)  TO FIRKEY (1:3)
               MOVE KASSTEI-IO-AREA (22:11) TO BELO-ELGP-IO
               INSPECT BELO-ELGP-IO REPLACING ALL ' ' BY '0'
               MOVE KASSTEI-IO-AREA (33:1) TO STATUS-X (1:1)
               MOVE KASSTEI-IO-AREA (1:120) TO REC120 (1:120)
               MOVE KASSTEI-IO-AREA (121:180) TO REC300 (1:180)
           END-EVALUATE.
 
       KASSTEI-IDSET SECTION.
       KASSTEI-IDSET-P.
           SET I-02                        TO TRUE.
 
       KASSTEI-MATCH-SET SECTION.
       KASSTEI-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE KASSTEI-IO-AREA (1:3)  TO KASSTEI-M-02-M1-FIRKEY
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
           IF  FIRMAKT-EOF
               MOVE HIGH-VALUES            TO FIRMAKT-MC
                                              FIRMAKT-MP
           END-IF
           IF  KASSTEI-EOF
               MOVE HIGH-VALUES            TO KASSTEI-MC
                                              KASSTEI-MP
           END-IF
           IF  FIRMAKT-MC < FIRMAKT-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  KASSTEI-MC < KASSTEI-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  FIRMAKT-MC < KASSTEI-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FIRMAKT-PROCESS     TO TRUE
                   MOVE FIRMAKT-MC         TO FIRMAKT-MP
                   IF  FIRMAKT-MC = KASSTEI-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  KASSTEI-MC < FIRMAKT-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET KASSTEI-PROCESS     TO TRUE
                   MOVE KASSTEI-MC         TO KASSTEI-MP
                   IF  KASSTEI-MC = FIRMAKT-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  FIRMAKT-MC = KASSTEI-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FIRMAKT-PROCESS     TO TRUE
                   MOVE FIRMAKT-MC         TO FIRMAKT-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND NOT-I-10)
               MOVE SPACES TO KASSTEO-IO-AREA
               INITIALIZE KASSTEO-IO-AREA
               MOVE REC120                 TO KASSTEO-IO-AREA (1:120)
               MOVE REC300                 TO KASSTEO-IO-AREA (121:180)
               WRITE KASSTEO-IO-AREA
           END-IF
           IF  (I-02 AND I-10 AND I-U2)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'SANERT:'              TO KLISTEO-IO-AREA (1:7)
               MOVE REC120                 TO KLISTEO-IO-AREA (13:120)
               IF  I-U1
                   MOVE 1                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-10 AND I-U2)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'LAGRET:'              TO KLISTEO-IO-AREA (1:7)
               MOVE REC120                 TO KLISTEO-IO-AREA (13:120)
               IF  I-U1
                   MOVE 1                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'KONTROLL-LISTE SANERING ' TO KLISTEO-IO-AREA
                                                                (1:24)
               MOVE 'AV KASSE.TERMINAL       ' TO KLISTEO-IO-AREA
                                                               (25:24)
               MOVE 'DATO:'                TO KLISTEO-IO-AREA (53:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO KLISTEO-IO-AREA (58:8)
               MOVE 'JOB=KASSTER'          TO KLISTEO-IO-AREA (95:11)
               MOVE 'PROGRAM=KOS012'       TO KLISTEO-IO-AREA (107:14)
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
               MOVE 'KONTROLL-LISTE SANERING ' TO KLISTEO-IO-AREA
                                                                (1:24)
               MOVE 'AV KASSE.TERMINAL       ' TO KLISTEO-IO-AREA
                                                               (25:24)
               MOVE 'DATO:'                TO KLISTEO-IO-AREA (53:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO KLISTEO-IO-AREA (58:8)
               MOVE 'JOB=KASSTER'          TO KLISTEO-IO-AREA (95:11)
               MOVE 'PROGRAM=KOS012'       TO KLISTEO-IO-AREA (107:14)
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
               MOVE 'ANTALL FØR SANERING    :' TO KLISTEO-IO-AREA
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
               MOVE 'ANTALL SANERT          :' TO KLISTEO-IO-AREA
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
               MOVE 'ANTALL ETTER SANERING  :' TO KLISTEO-IO-AREA
                                                                (1:24)
               MOVE ANTUT                  TO XO-70YY9R
               MOVE XO-70YY9R              TO KLISTEO-IO-AREA (31:10)
               IF  I-U1
                   MOVE 1                  TO KLISTEO-BEFORE-SPACE
                   MOVE 1                  TO KLISTEO-AFTER-SPACE
                   PERFORM KLISTEO-PRINT-LINE
               END-IF
           END-IF
           IF  (I-LR AND I-01)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE ' '                    TO KLISTEO-IO-AREA (24:1)
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
           INITIALIZE FIRMAKT-DATA-FIELDS
           SET FIRMAKT-EOF-OFF             TO TRUE
           SET FIRMAKT-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO FIRMAKT-MC
                                              FIRMAKT-MP
           OPEN INPUT FIRMAKT
           INITIALIZE KASSTEI-DATA-FIELDS
           SET KASSTEI-EOF-OFF             TO TRUE
           SET KASSTEI-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO KASSTEI-MC
                                              KASSTEI-MP
           OPEN INPUT KASSTEI
           OPEN OUTPUT KASSTEO
           IF I-U1
               OPEN OUTPUT KLISTEO
           END-IF
           INITIALIZE KLISTEO-IO-AREA
           INITIALIZE KLISTEO-DATA-FIELDS
           MOVE 57                         TO KLISTEO-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FIRMAKT
           CLOSE KASSTEI
           CLOSE KASSTEO
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
