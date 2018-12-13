       IDENTIFICATION DIVISION.
       PROGRAM-ID. VAR022R.
      **********************************************  Z-WIN-RPG2   ****
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VAR022.rpg
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
           SELECT INNPUT2
               ASSIGN TO UT-S-INNPUT2
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INNPUT2-STATUS.
           SELECT INNPUT
               ASSIGN TO UT-S-INNPUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INNPUT-STATUS.
           SELECT UTFILE
               ASSIGN TO UT-S-UTFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS UTFILE-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD INNPUT2
               BLOCK CONTAINS 8000
               RECORD CONTAINS 80.
       01  INNPUT2-IO-AREA.
           05  INNPUT2-IO-AREA-X           PICTURE X(80).
       FD INNPUT
               BLOCK CONTAINS 9000
               RECORD CONTAINS 100.
       01  INNPUT-IO-AREA.
           05  INNPUT-IO-AREA-X            PICTURE X(100).
       FD UTFILE
               BLOCK CONTAINS 9000
               RECORD CONTAINS 100.
       01  UTFILE-IO-AREA.
           05  UTFILE-IO-AREA-X            PICTURE X(100).
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
           10  INNPUT2-STATUS              PICTURE 99 VALUE 0.
           10  INNPUT-STATUS               PICTURE 99 VALUE 0.
           10  UTFILE-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  INNPUT2-EOF-OFF         VALUE '0'.
               88  INNPUT2-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNPUT2-READ-OFF        VALUE '0'.
               88  INNPUT2-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNPUT2-PROCESS-OFF     VALUE '0'.
               88  INNPUT2-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNPUT-EOF-OFF          VALUE '0'.
               88  INNPUT-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNPUT-READ-OFF         VALUE '0'.
               88  INNPUT-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNPUT-PROCESS-OFF      VALUE '0'.
               88  INNPUT-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  INNPUT-LEVEL-INIT-OFF   VALUE '0'.
               88  INNPUT-LEVEL-INIT       VALUE '1'.
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
           05  INNPUT2-DATA-FIELDS.
               10  FNR                     PICTURE X(3).
               10  TILF                    PICTURE X(3).
               10  KNR                     PICTURE X(6).
               10  KODE                    PICTURE X(1).
               10  LEV-IO.
                   15  LEV                 PICTURE S9(6).
           05  INNPUT2-MP                  PICTURE X(9).
           05  INNPUT2-MC                  PICTURE X(9).
           05  INNPUT2-M-02            REDEFINES INNPUT2-MC.
               10  INNPUT2-M-02-M2.
                   15  INNPUT2-M-02-M2-FNR-G.
                       20  INNPUT2-M-02-M2-FNR PICTURE X(3).
               10  INNPUT2-M-02-M1.
                   15  INNPUT2-M-02-M1-LEV-G.
                       20  INNPUT2-M-02-M1-LEV PICTURE S9(6).
           05  INNPUT-LEVEL-03.
               10  INNPUT-03-L2.
                   15  INNPUT-03-L2-FIRMA  PICTURE X(3).
               10  INNPUT-03-L1.
                   15  INNPUT-03-L1-LEVNR  PICTURE S9(6).
           05  INNPUT-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  EDBNR                   PICTURE X(7).
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  FORSL-IO.
                   15  FORSL               PICTURE S9(6).
               10  LEVNR-IO.
                   15  LEVNR               PICTURE S9(6).
               10  REC                     PICTURE X(100).
           05  INNPUT-MP                   PICTURE X(9).
           05  INNPUT-MC                   PICTURE X(9).
           05  INNPUT-M-03             REDEFINES INNPUT-MC.
               10  INNPUT-M-03-M2.
                   15  INNPUT-M-03-M2-FIRMA-G.
                       20  INNPUT-M-03-M2-FIRMA PICTURE X(3).
               10  INNPUT-M-03-M1.
                   15  INNPUT-M-03-M1-LEVNR-G.
                       20  INNPUT-M-03-M1-LEVNR PICTURE S9(6).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  FTIL                    PICTURE X(3).
               10  FKNR                    PICTURE X(6).
           05  EDITTING-FIELDS.
               10  XO-60YY9R               PICTURE ZZZ.ZZ9-.
               10  XO-60YNZ                PICTURE ZZZZZZ.
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
 
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
           IF  NOT-SET-I-OF
               SET NOT-I-OF                TO TRUE
           END-IF
           SET NOT-SET-I-OF                TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  INNPUT2-PROCESS
               SET INNPUT2-PROCESS-OFF     TO TRUE
               SET INNPUT2-READ            TO TRUE
           END-IF
 
           IF  INNPUT2-READ
               PERFORM INNPUT2-GET
               SET INNPUT2-READ-OFF        TO TRUE
               IF  NOT INNPUT2-EOF
                   PERFORM INNPUT2-MATCH-SET
               END-IF
           END-IF
 
           IF  INNPUT-PROCESS
               SET INNPUT-PROCESS-OFF      TO TRUE
               SET INNPUT-READ             TO TRUE
           END-IF
 
           IF  INNPUT-READ
               PERFORM INNPUT-GET
               SET INNPUT-READ-OFF         TO TRUE
               IF  NOT INNPUT-EOF
                   PERFORM INNPUT-MATCH-SET
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
 
           IF  INNPUT2-PROCESS
               PERFORM INNPUT2-IDSET
           END-IF
 
           IF  INNPUT-PROCESS
               PERFORM INNPUT-IDSET
           END-IF
 
           IF  INNPUT-PROCESS
               PERFORM INNPUT-CHK-LEVEL
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
           PERFORM DETAIL-OVERFLOW
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  INNPUT2-PROCESS
               PERFORM INNPUT2-FLDSET
           END-IF
 
           IF  INNPUT-PROCESS
               PERFORM INNPUT-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  INNPUT-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-02 AND I-MR)
               SET NOT-I-10                TO TRUE
               IF  KODE = 'V'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-03)
               SET NOT-I-10                TO TRUE
               IF  LEVNR = 0
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-MR AND I-L1)
               MOVE TILF                   TO FTIL
               MOVE KNR                    TO FKNR
           END-IF.
 
       INNPUT2-GET SECTION.
       INNPUT2-GET-P.
           IF  INNPUT2-EOF-OFF
               READ INNPUT2
               AT END
                   SET INNPUT2-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INNPUT2-FLDSET SECTION.
       INNPUT2-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INNPUT2-IO-AREA (1:3)  TO FNR (1:3)
               MOVE INNPUT2-IO-AREA (4:3)  TO TILF (1:3)
               MOVE INNPUT2-IO-AREA (7:6)  TO KNR (1:6)
               MOVE INNPUT2-IO-AREA (13:1) TO KODE (1:1)
               MOVE INNPUT2-IO-AREA (32:6) TO LEV-IO
               INSPECT LEV-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       INNPUT2-IDSET SECTION.
       INNPUT2-IDSET-P.
           SET I-02                        TO TRUE.
 
       INNPUT2-MATCH-SET SECTION.
       INNPUT2-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE INNPUT2-IO-AREA (1:3)  TO INNPUT2-M-02-M2-FNR
               MOVE INNPUT2-IO-AREA (32:6) TO INNPUT2-M-02-M1-LEV
           END-EVALUATE.
 
       INNPUT-GET SECTION.
       INNPUT-GET-P.
           IF  INNPUT-EOF-OFF
               READ INNPUT
               AT END
                   SET INNPUT-EOF          TO TRUE
               END-READ
           END-IF.
 
       INNPUT-FLDSET SECTION.
       INNPUT-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INNPUT-IO-AREA (1:3)   TO FIRMA (1:3)
               MOVE INNPUT-IO-AREA (4:7)   TO EDBNR (1:7)
               MOVE INNPUT-IO-AREA (11:3)  TO ALFA (1:3)
               MOVE INNPUT-IO-AREA (14:20) TO ARTNR (1:20)
               MOVE INNPUT-IO-AREA (34:6)  TO FORSL-IO
               INSPECT FORSL-IO REPLACING ALL ' ' BY '0'
               MOVE INNPUT-IO-AREA (41:6)  TO LEVNR-IO
               INSPECT LEVNR-IO REPLACING ALL ' ' BY '0'
               MOVE INNPUT-IO-AREA (1:100) TO REC (1:100)
           END-EVALUATE.
 
       INNPUT-IDSET SECTION.
       INNPUT-IDSET-P.
           SET I-03                        TO TRUE.
 
       INNPUT-CHK-LEVEL SECTION.
       INNPUT-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO INNPUT-LEVEL-03
               MOVE INNPUT-IO-AREA (1:3)   TO INNPUT-03-L2-FIRMA
               MOVE INNPUT-IO-AREA (41:6)  TO INNPUT-03-L1-LEVNR
               IF  INNPUT-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INNPUT-03-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INNPUT-03-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INNPUT-03-L2          TO THE-PRIOR-L2
               MOVE  INNPUT-03-L1          TO THE-PRIOR-L1
               SET INNPUT-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       INNPUT-MATCH-SET SECTION.
       INNPUT-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE INNPUT-IO-AREA (1:3)   TO INNPUT-M-03-M2-FIRMA
               MOVE INNPUT-IO-AREA (41:6)  TO INNPUT-M-03-M1-LEVNR
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
               SET I-OF                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OF            TO TRUE
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
           IF  INNPUT2-EOF
               MOVE HIGH-VALUES            TO INNPUT2-MC
                                              INNPUT2-MP
           END-IF
           IF  INNPUT-EOF
               MOVE HIGH-VALUES            TO INNPUT-MC
                                              INNPUT-MP
           END-IF
           IF  INNPUT2-MC < INNPUT2-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  INNPUT-MC < INNPUT-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  INNPUT2-MC < INNPUT-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET INNPUT2-PROCESS     TO TRUE
                   MOVE INNPUT2-MC         TO INNPUT2-MP
                   IF  INNPUT2-MC = INNPUT-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  INNPUT-MC < INNPUT2-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET INNPUT-PROCESS      TO TRUE
                   MOVE INNPUT-MC          TO INNPUT-MP
                   IF  INNPUT-MC = INNPUT2-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  INNPUT2-MC = INNPUT-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET INNPUT2-PROCESS     TO TRUE
                   MOVE INNPUT2-MC         TO INNPUT2-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-03 AND I-MR AND NOT-I-10)
               MOVE SPACES TO UTFILE-IO-AREA
               INITIALIZE UTFILE-IO-AREA
               MOVE REC                    TO UTFILE-IO-AREA (1:100)
               MOVE FTIL                   TO UTFILE-IO-AREA (47:3)
               MOVE FKNR                   TO UTFILE-IO-AREA (50:6)
               WRITE UTFILE-IO-AREA
           END-IF
           IF  (I-1P)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FORSLAGSFILE M/FNR,KNR ' TO LISTE-IO-AREA (26:23)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '-----------------------' TO LISTE-IO-AREA (26:23)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIR   EDBNR   ALF  ARTN' TO LISTE-IO-AREA (1:23)
               MOVE 'N                      ' TO LISTE-IO-AREA (24:23)
               MOVE '    FORSL       LEV    ' TO LISTE-IO-AREA (47:23)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '-----------------------' TO LISTE-IO-AREA (26:23)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-03 AND I-MR AND NOT-I-10)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FIRMA                  TO LISTE-IO-AREA (1:3)
               MOVE EDBNR                  TO LISTE-IO-AREA (8:7)
               MOVE ALFA                   TO LISTE-IO-AREA (16:3)
               MOVE ARTNR                  TO LISTE-IO-AREA (20:20)
               MOVE FORSL                  TO XO-60YY9R
               MOVE XO-60YY9R              TO LISTE-IO-AREA (43:8)
               MOVE LEV                    TO XO-60YNZ
               MOVE XO-60YNZ               TO LISTE-IO-AREA (55:6)
               MOVE TILF                   TO LISTE-IO-AREA (63:3)
               MOVE KNR                    TO LISTE-IO-AREA (70:6)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       DETAIL-OVERFLOW SECTION.
       DETAIL-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FORSLAGSFILE M/FNR,KNR ' TO LISTE-IO-AREA (26:23)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '-----------------------' TO LISTE-IO-AREA (26:23)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIR   EDBNR   ALF  ARTN' TO LISTE-IO-AREA (1:23)
               MOVE 'N                      ' TO LISTE-IO-AREA (24:23)
               MOVE '    FORSL       LEV    ' TO LISTE-IO-AREA (47:23)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '-----------------------' TO LISTE-IO-AREA (26:23)
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
           MOVE 1                          TO LR-CHECK
           INITIALIZE INNPUT2-DATA-FIELDS
           SET INNPUT2-EOF-OFF             TO TRUE
           SET INNPUT2-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO INNPUT2-MC
                                              INNPUT2-MP
           OPEN INPUT INNPUT2
           SET INNPUT-LEVEL-INIT           TO TRUE
           INITIALIZE INNPUT-DATA-FIELDS
           SET INNPUT-EOF-OFF              TO TRUE
           SET INNPUT-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO INNPUT-MC
                                              INNPUT-MP
           OPEN INPUT INNPUT
           OPEN OUTPUT UTFILE
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INNPUT2
           CLOSE INNPUT
           CLOSE UTFILE
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
