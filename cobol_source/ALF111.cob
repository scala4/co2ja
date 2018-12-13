       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALF111R.
      **********************************************  Z-WIN-RPG2   ****
      *   DANNER NYE REC. TIL BEST.ALFA.FILE                       *
      ****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ALF111.rpg
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
           SELECT BESTALF
               ASSIGN TO BESTALF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS BESTALF-STATUS
               RECORD KEY IS BESTALF-KEY1.
           SELECT AFILE
               ASSIGN TO UT-S-AFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS AFILE-STATUS.
           SELECT AFILENY
               ASSIGN TO UT-S-AFILENY
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS AFILENY-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD BESTALF
               RECORD CONTAINS 20.
       01  BESTALF-IO-AREA.
           05  BESTALF-IO-AREA-X.
               10  BESTALF-KEY1.
                   15  BESTALF-KEY1N       PICTURE S9(11).
               10  FILLER                  PICTURE X(9).
       FD AFILE
               BLOCK CONTAINS 4100
               RECORD CONTAINS 20.
       01  AFILE-IO-AREA.
           05  AFILE-IO-AREA-X             PICTURE X(20).
       FD AFILENY
               BLOCK CONTAINS 4100
               RECORD CONTAINS 20.
       01  AFILENY-IO-AREA.
           05  AFILENY-IO-AREA-X           PICTURE X(20).
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
           10  BESTALF-STATUS              PICTURE 99 VALUE 0.
           10  AFILE-STATUS                PICTURE 99 VALUE 0.
           10  AFILENY-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  BESTALF-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  BESTALF-EOF-OFF         VALUE '0'.
               88  BESTALF-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BESTALF-READ-OFF        VALUE '0'.
               88  BESTALF-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BESTALF-PROCESS-OFF     VALUE '0'.
               88  BESTALF-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  AFILE-EOF-OFF           VALUE '0'.
               88  AFILE-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  AFILE-READ-OFF          VALUE '0'.
               88  AFILE-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  AFILE-PROCESS-OFF       VALUE '0'.
               88  AFILE-PROCESS           VALUE '1'.
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
           05  BESTALF-DATA-FIELDS.
               10  KEY-X                   PICTURE X(11).
               10  RECA                    PICTURE X(20).
           05  BESTALF-MP                  PICTURE X(11).
           05  BESTALF-MC                  PICTURE X(11).
           05  BESTALF-M-01            REDEFINES BESTALF-MC.
               10  BESTALF-M-01-M1.
                   15  BESTALF-M-01-M1-KEY-X-G.
                       20  BESTALF-M-01-M1-KEY-X PICTURE X(11).
           05  AFILE-DATA-FIELDS.
               10  KEY2                    PICTURE X(11).
               10  REC                     PICTURE X(20).
               10  FNR                     PICTURE X(3).
               10  VGR                     PICTURE X(5).
               10  ALFA                    PICTURE X(3).
      *
           05  AFILE-MP                    PICTURE X(11).
           05  AFILE-MC                    PICTURE X(11).
           05  AFILE-M-02              REDEFINES AFILE-MC.
               10  AFILE-M-02-M1.
                   15  AFILE-M-02-M1-KEY2-G.
                       20  AFILE-M-02-M1-KEY2 PICTURE X(11).
           05  TEMPORARY-FIELDS.
               10  ANTNM-IO.
                   15  ANTNM               PICTURE S9(5).
               10  ANTMR-IO.
                   15  ANTMR               PICTURE S9(5).
               10  ANT01-IO.
                   15  ANT01               PICTURE S9(5).
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
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  AFILE-PROCESS
               SET AFILE-PROCESS-OFF       TO TRUE
               SET AFILE-READ              TO TRUE
           END-IF
 
           IF  AFILE-READ
               PERFORM AFILE-GET
               SET AFILE-READ-OFF          TO TRUE
               IF  NOT AFILE-EOF
                   PERFORM AFILE-MATCH-SET
               END-IF
           END-IF
 
           IF  BESTALF-PROCESS
               SET BESTALF-PROCESS-OFF     TO TRUE
               SET BESTALF-READ            TO TRUE
           END-IF
 
           IF  BESTALF-READ
               PERFORM BESTALF-GET
               SET BESTALF-READ-OFF        TO TRUE
               IF  NOT BESTALF-EOF
                   PERFORM BESTALF-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  BESTALF-PROCESS
               PERFORM BESTALF-IDSET
           END-IF
 
           IF  AFILE-PROCESS
               PERFORM AFILE-IDSET
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
           PERFORM DETAIL-OVERFLOW
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  BESTALF-PROCESS
               PERFORM BESTALF-FLDSET
           END-IF
 
           IF  AFILE-PROCESS
               PERFORM AFILE-FLDSET
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
           IF  (I-02 AND NOT-I-MR)
               ADD 1                       TO ANTNM
           END-IF
           IF  (I-01 AND I-MR)
               ADD 1                       TO ANTMR
           END-IF
           IF  (I-01)
               ADD 1                       TO ANT01
           END-IF.
 
       BESTALF-GET SECTION.
       BESTALF-GET-P.
           IF  BESTALF-EOF-OFF
               READ BESTALF
               AT END
                   SET BESTALF-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       BESTALF-FLDSET SECTION.
       BESTALF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE BESTALF-IO-AREA (1:11) TO KEY-X (1:11)
               MOVE BESTALF-IO-AREA (1:20) TO RECA (1:20)
           END-EVALUATE.
 
       BESTALF-IDSET SECTION.
       BESTALF-IDSET-P.
           SET I-01                        TO TRUE.
 
       BESTALF-MATCH-SET SECTION.
       BESTALF-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE BESTALF-IO-AREA (1:11) TO BESTALF-M-01-M1-KEY-X
           END-EVALUATE.
 
       AFILE-GET SECTION.
       AFILE-GET-P.
           IF  AFILE-EOF-OFF
               READ AFILE
               AT END
                   SET AFILE-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       AFILE-FLDSET SECTION.
       AFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE AFILE-IO-AREA (1:11)   TO KEY2 (1:11)
               MOVE AFILE-IO-AREA (1:20)   TO REC (1:20)
               MOVE AFILE-IO-AREA (1:3)    TO FNR (1:3)
               MOVE AFILE-IO-AREA (4:5)    TO VGR (1:5)
               MOVE AFILE-IO-AREA (9:3)    TO ALFA (1:3)
           END-EVALUATE.
 
       AFILE-IDSET SECTION.
       AFILE-IDSET-P.
           SET I-02                        TO TRUE.
 
       AFILE-MATCH-SET SECTION.
       AFILE-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE AFILE-IO-AREA (1:11)   TO AFILE-M-02-M1-KEY2
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
           IF  BESTALF-EOF
               MOVE HIGH-VALUES            TO BESTALF-MC
                                              BESTALF-MP
           END-IF
           IF  AFILE-EOF
               MOVE HIGH-VALUES            TO AFILE-MC
                                              AFILE-MP
           END-IF
           IF  BESTALF-MC < BESTALF-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  AFILE-MC < AFILE-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  BESTALF-MC < AFILE-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET BESTALF-PROCESS     TO TRUE
                   MOVE BESTALF-MC         TO BESTALF-MP
                   IF  BESTALF-MC = AFILE-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  AFILE-MC < BESTALF-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET AFILE-PROCESS       TO TRUE
                   MOVE AFILE-MC           TO AFILE-MP
                   IF  AFILE-MC = BESTALF-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  BESTALF-MC = AFILE-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET BESTALF-PROCESS     TO TRUE
                   MOVE BESTALF-MC         TO BESTALF-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND NOT-I-MR)
               MOVE SPACES TO AFILENY-IO-AREA
               INITIALIZE AFILENY-IO-AREA
               MOVE REC                    TO AFILENY-IO-AREA (1:20)
               WRITE AFILENY-IO-AREA
           END-IF
           IF  (I-01 AND I-MR)
               MOVE SPACES TO AFILENY-IO-AREA
               INITIALIZE AFILENY-IO-AREA
               MOVE RECA                   TO AFILENY-IO-AREA (1:20)
               WRITE AFILENY-IO-AREA
           END-IF
           IF  (I-1P)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'NYE ALFA/VGR TIL BA01  ' TO LISTE-IO-AREA (2:23)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '-----------------------' TO LISTE-IO-AREA (2:23)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-02 AND NOT-I-MR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FNR                    TO LISTE-IO-AREA (1:3)
               MOVE VGR                    TO LISTE-IO-AREA (6:5)
               MOVE ALFA                   TO LISTE-IO-AREA (13:3)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       DETAIL-OVERFLOW SECTION.
       DETAIL-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'NYE ALFA/VGR TIL BA01  ' TO LISTE-IO-AREA (2:23)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '-----------------------' TO LISTE-IO-AREA (2:23)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL NYE TIL FILEN' TO LISTE-IO-AREA (5:20)
               MOVE ANTNM                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (25:6)
               MOVE 2                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL GAMLE TOTALT ' TO LISTE-IO-AREA (5:20)
               MOVE ANT01                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (25:6)
               MOVE 2                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL GAMLE TILBAKE' TO LISTE-IO-AREA (5:20)
               MOVE ANTMR                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (25:6)
               MOVE 2                      TO LISTE-BEFORE-SPACE
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
           INITIALIZE BESTALF-DATA-FIELDS
           SET BESTALF-EOF-OFF             TO TRUE
           SET BESTALF-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO BESTALF-MC
                                              BESTALF-MP
           OPEN INPUT BESTALF
           INITIALIZE AFILE-DATA-FIELDS
           SET AFILE-EOF-OFF               TO TRUE
           SET AFILE-PROCESS               TO TRUE
           MOVE LOW-VALUES                 TO AFILE-MC
                                              AFILE-MP
           OPEN INPUT AFILE
           OPEN OUTPUT AFILENY
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE BESTALF
           CLOSE AFILE
           CLOSE AFILENY
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
