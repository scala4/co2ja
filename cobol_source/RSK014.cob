       IDENTIFICATION DIVISION.
       PROGRAM-ID. RSK014R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM.......: RSK014, SANERING AV RESKONTROLOGG.           *
      *  PROGRAMMERER..: MORTEN TUVRØNNINGEN                          *
      *  KJØRES I JOBB.: RESLOGG, REORG. AV RESKNR.LOGG.              *
      *  LAGET DATO....: 15.10.03                                     *
      *  ENDRET........:                                              *
      *  RETTET........:                                              *
      *  INPUT.........: KUNDEMASTER OG RESKONTROLOGG.                *
      *  BEHANDLING....: SANERER RECORDS UTEN MATCH.                  *
      *  OUTPUT........: SANERT RESKONTROLOGG.                        *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RSK014.rpg
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
           SELECT RESLOGG
               ASSIGN TO RESLOGG
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS RESLOGG-STATUS
               RECORD KEY IS RESLOGG-KEY1.
           SELECT KUNDEMA
               ASSIGN TO KUNDEMA
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMA-STATUS
               RECORD KEY IS KUNDEMA-KEY1.
           SELECT SANFILE
               ASSIGN TO UT-S-SANFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS SANFILE-STATUS.
           SELECT KLISTEO
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KLISTEO-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD RESLOGG
               RECORD CONTAINS 500.
       01  RESLOGG-IO-AREA.
           05  RESLOGG-IO-AREA-X.
               10  RESLOGG-KEY1.
                   15  RESLOGG-KEY1N       PICTURE S9(11).
               10  FILLER                  PICTURE X(489).
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1.
                   15  KUNDEMA-KEY1N       PICTURE S9(9).
               10  FILLER                  PICTURE X(190).
       FD SANFILE
               BLOCK CONTAINS 5000
               RECORD CONTAINS 500.
       01  SANFILE-IO-AREA.
           05  SANFILE-IO-AREA-X           PICTURE X(500).
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
           10  RESLOGG-STATUS              PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  SANFILE-STATUS              PICTURE 99 VALUE 0.
           10  KLISTEO-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  RESLOGG-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  RESLOGG-EOF-OFF         VALUE '0'.
               88  RESLOGG-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESLOGG-READ-OFF        VALUE '0'.
               88  RESLOGG-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESLOGG-PROCESS-OFF     VALUE '0'.
               88  RESLOGG-PROCESS         VALUE '1'.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  KUNDEMA-EOF-OFF         VALUE '0'.
               88  KUNDEMA-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KUNDEMA-READ-OFF        VALUE '0'.
               88  KUNDEMA-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KUNDEMA-PROCESS-OFF     VALUE '0'.
               88  KUNDEMA-PROCESS         VALUE '1'.
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
           05  RESLOGG-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  RESKNR                  PICTURE X(6).
               10  REC250                  PICTURE X(250).
               10  REC500                  PICTURE X(250).
               10  REC060                  PICTURE X(60).
           05  RESLOGG-MP                  PICTURE X(9).
           05  RESLOGG-MC                  PICTURE X(9).
           05  RESLOGG-M-01            REDEFINES RESLOGG-MC.
               10  RESLOGG-M-01-M2.
                   15  RESLOGG-M-01-M2-FIRMA-G.
                       20  RESLOGG-M-01-M2-FIRMA PICTURE X(3).
               10  RESLOGG-M-01-M1.
                   15  RESLOGG-M-01-M1-RESKNR-G.
                       20  RESLOGG-M-01-M1-RESKNR PICTURE X(6).
           05  KUNDEMA-DATA-FIELDS.
               10  FILLER                  PICTURE X.
           05  KUNDEMA-MP                  PICTURE X(9).
           05  KUNDEMA-MC                  PICTURE X(9).
           05  KUNDEMA-M-02            REDEFINES KUNDEMA-MC.
               10  KUNDEMA-M-02-M2.
                   15  KUNDEMA-M-02-M2-FIRMA-G.
                       20  KUNDEMA-M-02-M2-FIRMA PICTURE X(3).
               10  KUNDEMA-M-02-M1.
                   15  KUNDEMA-M-02-M1-RESKNR-G.
                       20  KUNDEMA-M-02-M1-RESKNR PICTURE X(6).
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
           IF  RESLOGG-PROCESS
               SET RESLOGG-PROCESS-OFF     TO TRUE
               SET RESLOGG-READ            TO TRUE
           END-IF
 
           IF  RESLOGG-READ
               PERFORM RESLOGG-GET
               SET RESLOGG-READ-OFF        TO TRUE
               IF  NOT RESLOGG-EOF
                   PERFORM RESLOGG-MATCH-SET
               END-IF
           END-IF
 
           IF  KUNDEMA-PROCESS
               SET KUNDEMA-PROCESS-OFF     TO TRUE
               SET KUNDEMA-READ            TO TRUE
           END-IF
 
           IF  KUNDEMA-READ
               PERFORM KUNDEMA-GET
               SET KUNDEMA-READ-OFF        TO TRUE
               IF  NOT KUNDEMA-EOF
                   PERFORM KUNDEMA-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  RESLOGG-PROCESS
               PERFORM RESLOGG-IDSET
           END-IF
 
           IF  KUNDEMA-PROCESS
               PERFORM KUNDEMA-IDSET
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
 
           IF  RESLOGG-PROCESS
               PERFORM RESLOGG-FLDSET
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
           IF  (I-01 AND NOT-I-MR)
               ADD 1                       TO ANTSAN
      *  01NMR             MOVE "ANTSAN  "BUGFL1  8        LEDETXT DEBUG
      *  01NMR   BUGFL1    DEBUGBUGFILO   ANTSAN           VIS FELT/IND
      *  01NMR             MOVE "REC500  "BUGFL1  8        LEDETXT DEBUG
      *  01NMR   BUGFL1    DEBUGBUGFILO   REC500           VIS FELT/IND
           END-IF
           IF  (I-01 AND I-MR)
               ADD 1                       TO ANTUT
           END-IF.
 
       RESLOGG-GET SECTION.
       RESLOGG-GET-P.
           IF  RESLOGG-EOF-OFF
               READ RESLOGG
               AT END
                   SET RESLOGG-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RESLOGG-FLDSET SECTION.
       RESLOGG-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RESLOGG-IO-AREA (1:3)  TO FIRMA (1:3)
               MOVE RESLOGG-IO-AREA (4:6)  TO RESKNR (1:6)
               MOVE RESLOGG-IO-AREA (1:250) TO REC250 (1:250)
               MOVE RESLOGG-IO-AREA (251:250) TO REC500 (1:250)
               MOVE RESLOGG-IO-AREA (1:60) TO REC060 (1:60)
           END-EVALUATE.
 
       RESLOGG-IDSET SECTION.
       RESLOGG-IDSET-P.
           SET I-01                        TO TRUE.
 
       RESLOGG-MATCH-SET SECTION.
       RESLOGG-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE RESLOGG-IO-AREA (1:3)  TO RESLOGG-M-01-M2-FIRMA
               MOVE RESLOGG-IO-AREA (4:6)  TO RESLOGG-M-01-M1-RESKNR
           END-EVALUATE.
 
       KUNDEMA-GET SECTION.
       KUNDEMA-GET-P.
           IF  KUNDEMA-EOF-OFF
               READ KUNDEMA
               AT END
                   SET KUNDEMA-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-02                        TO TRUE.
 
       KUNDEMA-MATCH-SET SECTION.
       KUNDEMA-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (3:3)  TO KUNDEMA-M-02-M2-FIRMA
               MOVE KUNDEMA-IO-AREA (6:6)  TO KUNDEMA-M-02-M1-RESKNR
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
           IF  RESLOGG-EOF
               MOVE HIGH-VALUES            TO RESLOGG-MC
                                              RESLOGG-MP
           END-IF
           IF  KUNDEMA-EOF
               MOVE HIGH-VALUES            TO KUNDEMA-MC
                                              KUNDEMA-MP
           END-IF
           IF  RESLOGG-MC < RESLOGG-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  KUNDEMA-MC < KUNDEMA-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  RESLOGG-MC < KUNDEMA-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET RESLOGG-PROCESS     TO TRUE
                   MOVE RESLOGG-MC         TO RESLOGG-MP
                   IF  RESLOGG-MC = KUNDEMA-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  KUNDEMA-MC < RESLOGG-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET KUNDEMA-PROCESS     TO TRUE
                   MOVE KUNDEMA-MC         TO KUNDEMA-MP
                   IF  KUNDEMA-MC = RESLOGG-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  RESLOGG-MC = KUNDEMA-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET RESLOGG-PROCESS     TO TRUE
                   MOVE RESLOGG-MC         TO RESLOGG-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-MR)
               MOVE SPACES TO SANFILE-IO-AREA
               INITIALIZE SANFILE-IO-AREA
               MOVE REC250                 TO SANFILE-IO-AREA (1:250)
               MOVE REC500                 TO SANFILE-IO-AREA (251:250)
               WRITE SANFILE-IO-AREA
           END-IF
           IF  (I-01 AND NOT-I-MR AND I-U2)
               MOVE SPACES TO KLISTEO-IO-AREA
               INITIALIZE KLISTEO-IO-AREA
               MOVE 'SANERT:'              TO KLISTEO-IO-AREA (1:7)
               MOVE REC060                 TO KLISTEO-IO-AREA (21:60)
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
               MOVE 'AV RESKONTROLOGG        ' TO KLISTEO-IO-AREA
                                                               (25:24)
               MOVE 'DATO:'                TO KLISTEO-IO-AREA (53:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO KLISTEO-IO-AREA (58:8)
               MOVE 'JOB=RESLOGG'          TO KLISTEO-IO-AREA (95:11)
               MOVE 'PROGRAM=RSK014'       TO KLISTEO-IO-AREA (107:14)
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
               MOVE 'AV RESKONTROLOGG        ' TO KLISTEO-IO-AREA
                                                               (25:24)
               MOVE 'DATO:'                TO KLISTEO-IO-AREA (53:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO KLISTEO-IO-AREA (58:8)
               MOVE 'JOB=RESLOGG'          TO KLISTEO-IO-AREA (95:11)
               MOVE 'PROGRAM=RSK014'       TO KLISTEO-IO-AREA (107:14)
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
           IF  (I-LR AND I-02)
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
           INITIALIZE RESLOGG-DATA-FIELDS
           SET RESLOGG-EOF-OFF             TO TRUE
           SET RESLOGG-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO RESLOGG-MC
                                              RESLOGG-MP
           OPEN INPUT RESLOGG
           SET KUNDEMA-EOF-OFF             TO TRUE
           SET KUNDEMA-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO KUNDEMA-MC
                                              KUNDEMA-MP
           OPEN INPUT KUNDEMA
           OPEN OUTPUT SANFILE
           IF I-U1
               OPEN OUTPUT KLISTEO
           END-IF
           INITIALIZE KLISTEO-IO-AREA
           INITIALIZE KLISTEO-DATA-FIELDS
           MOVE 57                         TO KLISTEO-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE RESLOGG
           CLOSE KUNDEMA
           CLOSE SANFILE
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
