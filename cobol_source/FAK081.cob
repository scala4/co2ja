       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK081R.
      **********************************************  Z-WIN-RPG2   ****
      *     OPPDATERING AV FAKT.KONT.VAREREC.MASTER.                 *
      * 15/2-94 ESPEN LARSEN                                         *
      *     NULLSTILLING I FAK080 (VANLIG FAKT)                      *
      ****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK081.rpg
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
           SELECT FKINN
               ASSIGN TO UT-S-FKINN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FKINN-STATUS.
           SELECT INNPUT
               ASSIGN TO UT-S-INNPUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INNPUT-STATUS.
           SELECT FKUT
               ASSIGN TO UT-S-FKUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FKUT-STATUS.
           SELECT PRINT-X
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PRINT-X-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD FKINN
               BLOCK CONTAINS 4100
               RECORD CONTAINS 82.
       01  FKINN-IO-AREA.
           05  FKINN-IO-AREA-X             PICTURE X(82).
       FD INNPUT
               BLOCK CONTAINS 4100
               RECORD CONTAINS 82.
       01  INNPUT-IO-AREA.
           05  INNPUT-IO-AREA-X            PICTURE X(82).
       FD FKUT
               BLOCK CONTAINS 4100
               RECORD CONTAINS 82.
       01  FKUT-IO-AREA.
           05  FKUT-IO-AREA-X              PICTURE X(82).
       FD PRINT-X
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  PRINT-X-IO-PRINT.
           05  PRINT-X-IO-AREA-CONTROL     PICTURE X VALUE ' '.
        02 PRINT-X-IO-AREA.
           05  PRINT-X-IO-AREA-X           PICTURE X(132).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  FKINN-STATUS                PICTURE 99 VALUE 0.
           10  INNPUT-STATUS               PICTURE 99 VALUE 0.
           10  FKUT-STATUS                 PICTURE 99 VALUE 0.
           10  PRINT-X-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  FKINN-EOF-OFF           VALUE '0'.
               88  FKINN-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FKINN-READ-OFF          VALUE '0'.
               88  FKINN-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FKINN-PROCESS-OFF       VALUE '0'.
               88  FKINN-PROCESS           VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNPUT-EOF-OFF          VALUE '0'.
               88  INNPUT-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNPUT-READ-OFF         VALUE '0'.
               88  INNPUT-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNPUT-PROCESS-OFF      VALUE '0'.
               88  INNPUT-PROCESS          VALUE '1'.
           05  PRINT-X-DATA-FIELDS.
               10  PRINT-X-AFTER-SPACE     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINT-X-AFTER-SKIP      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINT-X-BEFORE-SPACE    PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINT-X-BEFORE-SKIP     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINT-X-MAX-LINES       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINT-X-LINE-COUNT      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINT-X-CLR-IO          PICTURE X VALUE 'Y'.
           05  FKINN-DATA-FIELDS.
               10  REC1                    PICTURE X(82).
               10  FKMND                   PICTURE X(2).
               10  FKAAR                   PICTURE X(2).
           05  FKINN-MP                    PICTURE X(4).
           05  FKINN-MC                    PICTURE X(4).
           05  FKINN-M-01              REDEFINES FKINN-MC.
               10  FKINN-M-01-M2.
                   15  FKINN-M-01-M2-FKAAR-G.
                       20  FKINN-M-01-M2-FKAAR PICTURE X(2).
               10  FKINN-M-01-M1.
                   15  FKINN-M-01-M1-FKMND-G.
                       20  FKINN-M-01-M1-FKMND PICTURE X(2).
           05  INNPUT-DATA-FIELDS.
               10  REC2                    PICTURE X(82).
      *****************************************************************
      * TELLING AV ANTALL RECORDS.                                    *
      *****************************************************************
           05  INNPUT-MP                   PICTURE X(4).
           05  INNPUT-MC                   PICTURE X(4).
           05  INNPUT-M-02             REDEFINES INNPUT-MC.
               10  INNPUT-M-02-M2.
                   15  INNPUT-M-02-M2-FKAAR-G.
                       20  INNPUT-M-02-M2-FKAAR PICTURE X(2).
               10  INNPUT-M-02-M1.
                   15  INNPUT-M-02-M1-FKMND-G.
                       20  INNPUT-M-02-M1-FKMND PICTURE X(2).
           05  TEMPORARY-FIELDS.
               10  ANT1-IO.
                   15  ANT1                PICTURE S9(8).
               10  ANT2-IO.
                   15  ANT2                PICTURE S9(8).
               10  ANT3-IO.
                   15  ANT3                PICTURE S9(8).
           05  EDITTING-FIELDS.
               10  XO-80YYZ                PICTURE ZZ.ZZZ.ZZZ.
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
           IF  FKINN-PROCESS
               SET FKINN-PROCESS-OFF       TO TRUE
               SET FKINN-READ              TO TRUE
           END-IF
 
           IF  FKINN-READ
               PERFORM FKINN-GET
               SET FKINN-READ-OFF          TO TRUE
               IF  NOT FKINN-EOF
                   PERFORM FKINN-MATCH-SET
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
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  FKINN-PROCESS
               PERFORM FKINN-IDSET
           END-IF
 
           IF  INNPUT-PROCESS
               PERFORM INNPUT-IDSET
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
 
           IF  FKINN-PROCESS
               PERFORM FKINN-FLDSET
           END-IF
 
           IF  INNPUT-PROCESS
               PERFORM INNPUT-FLDSET
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
               ADD 1                       TO ANT1
           END-IF
           IF  (I-02)
               ADD 1                       TO ANT2
           END-IF
           IF  (I-01)
               ADD 1                       TO ANT3
           END-IF
           IF  (I-02)
               ADD 1                       TO ANT3
           END-IF.
 
       FKINN-GET SECTION.
       FKINN-GET-P.
           IF  FKINN-EOF-OFF
               READ FKINN
               AT END
                   SET FKINN-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FKINN-FLDSET SECTION.
       FKINN-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FKINN-IO-AREA (1:82)   TO REC1 (1:82)
               MOVE FKINN-IO-AREA (42:2)   TO FKMND (1:2)
               MOVE FKINN-IO-AREA (58:2)   TO FKAAR (1:2)
           END-EVALUATE.
 
       FKINN-IDSET SECTION.
       FKINN-IDSET-P.
           SET I-01                        TO TRUE.
 
       FKINN-MATCH-SET SECTION.
       FKINN-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE FKINN-IO-AREA (58:2)   TO FKINN-M-01-M2-FKAAR
               MOVE FKINN-IO-AREA (42:2)   TO FKINN-M-01-M1-FKMND
           END-EVALUATE.
 
       INNPUT-GET SECTION.
       INNPUT-GET-P.
           IF  INNPUT-EOF-OFF
               READ INNPUT
               AT END
                   SET INNPUT-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INNPUT-FLDSET SECTION.
       INNPUT-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INNPUT-IO-AREA (1:82)  TO REC2 (1:82)
               MOVE INNPUT-IO-AREA (42:2)  TO FKMND (1:2)
               MOVE INNPUT-IO-AREA (58:2)  TO FKAAR (1:2)
           END-EVALUATE.
 
       INNPUT-IDSET SECTION.
       INNPUT-IDSET-P.
           SET I-02                        TO TRUE.
 
       INNPUT-MATCH-SET SECTION.
       INNPUT-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE INNPUT-IO-AREA (58:2)  TO INNPUT-M-02-M2-FKAAR
               MOVE INNPUT-IO-AREA (42:2)  TO INNPUT-M-02-M1-FKMND
           END-EVALUATE.
 
       PRINT-X-PRINT-LINE SECTION.
       PRINT-X-PRINT-LINE-P.
           IF  PRINT-X-BEFORE-SKIP > 0
               PERFORM PRINT-X-SKIP-BEFORE
           END-IF
           IF  PRINT-X-BEFORE-SPACE > 0
               PERFORM PRINT-X-SPACE-BEFORE
               IF  PRINT-X-AFTER-SKIP > 0
                   PERFORM PRINT-X-SKIP-AFTER
               END-IF
               IF  PRINT-X-AFTER-SPACE > 0
                   PERFORM PRINT-X-SPACE-AFTER
               END-IF
           ELSE
               IF  PRINT-X-AFTER-SKIP > 0
                   PERFORM PRINT-X-SKIP-AFTER
               END-IF
               PERFORM PRINT-X-SPACE-AFTER
           END-IF
           IF  PRINT-X-LINE-COUNT NOT < PRINT-X-MAX-LINES
               MOVE 7                      TO PRINT-X-AFTER-SKIP
           END-IF.
 
       PRINT-X-SKIP-BEFORE SECTION.
       PRINT-X-SKIP-BEFORE-P.
           WRITE PRINT-X-IO-PRINT       AFTER ADVANCING PAGE
           MOVE 1                          TO PRINT-X-LINE-COUNT
           MOVE 0                          TO PRINT-X-BEFORE-SKIP
           INITIALIZE PRINT-X-IO-AREA.
 
       PRINT-X-SPACE-BEFORE SECTION.
       PRINT-X-SPACE-BEFORE-P.
           WRITE PRINT-X-IO-PRINT       AFTER PRINT-X-BEFORE-SPACE
                                                                 LINES
           ADD PRINT-X-BEFORE-SPACE        TO PRINT-X-LINE-COUNT
           MOVE SPACES TO PRINT-X-IO-AREA
           INITIALIZE PRINT-X-IO-AREA
           MOVE 0                          TO PRINT-X-BEFORE-SPACE.
 
       PRINT-X-SKIP-AFTER SECTION.
       PRINT-X-SKIP-AFTER-P.
           WRITE PRINT-X-IO-PRINT      BEFORE ADVANCING PAGE
           MOVE 1                          TO PRINT-X-LINE-COUNT
           MOVE 0                          TO PRINT-X-AFTER-SKIP
           INITIALIZE PRINT-X-IO-AREA.
 
       PRINT-X-SPACE-AFTER SECTION.
       PRINT-X-SPACE-AFTER-P.
           WRITE PRINT-X-IO-PRINT      BEFORE PRINT-X-AFTER-SPACE LINES
           ADD PRINT-X-AFTER-SPACE         TO PRINT-X-LINE-COUNT
           INITIALIZE PRINT-X-IO-AREA
           MOVE 0                          TO PRINT-X-AFTER-SPACE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  FKINN-EOF
               MOVE HIGH-VALUES            TO FKINN-MC
                                              FKINN-MP
           END-IF
           IF  INNPUT-EOF
               MOVE HIGH-VALUES            TO INNPUT-MC
                                              INNPUT-MP
           END-IF
           IF  FKINN-MC < FKINN-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  INNPUT-MC < INNPUT-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  FKINN-MC < INNPUT-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FKINN-PROCESS       TO TRUE
                   MOVE FKINN-MC           TO FKINN-MP
                   IF  FKINN-MC = INNPUT-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  INNPUT-MC < FKINN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET INNPUT-PROCESS      TO TRUE
                   MOVE INNPUT-MC          TO INNPUT-MP
                   IF  INNPUT-MC = FKINN-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  FKINN-MC = INNPUT-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FKINN-PROCESS       TO TRUE
                   MOVE FKINN-MC           TO FKINN-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               MOVE SPACES TO FKUT-IO-AREA
               INITIALIZE FKUT-IO-AREA
               MOVE REC1                   TO FKUT-IO-AREA (1:82)
               WRITE FKUT-IO-AREA
           END-IF
           IF  (I-02)
               MOVE SPACES TO FKUT-IO-AREA
               INITIALIZE FKUT-IO-AREA
               MOVE REC2                   TO FKUT-IO-AREA (1:82)
               WRITE FKUT-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE 'AVSTEMNINGSTOTALER FRA ' TO PRINT-X-IO-AREA (2:23)
               MOVE 'F A K 0 8 1'          TO PRINT-X-IO-AREA (25:11)
               MOVE 'PR.'                  TO PRINT-X-IO-AREA (38:3)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO PRINT-X-IO-AREA (41:8)
               MOVE 01                     TO PRINT-X-BEFORE-SKIP
               PERFORM PRINT-X-PRINT-LINE
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE 'FAKTURA KONTANTSALGS RUT' TO PRINT-X-IO-AREA
                                                                (6:24)
               MOVE 'INE VARERECORDS.  '   TO PRINT-X-IO-AREA (30:18)
               MOVE 3                      TO PRINT-X-BEFORE-SPACE
               PERFORM PRINT-X-PRINT-LINE
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE 'GML MASTER    :'      TO PRINT-X-IO-AREA (6:15)
               MOVE ANT1                   TO XO-80YYZ
               MOVE XO-80YYZ               TO PRINT-X-IO-AREA (21:10)
               MOVE 2                      TO PRINT-X-BEFORE-SPACE
               PERFORM PRINT-X-PRINT-LINE
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE 'NYE RECORDS   :'      TO PRINT-X-IO-AREA (6:15)
               MOVE ANT2                   TO XO-80YYZ
               MOVE XO-80YYZ               TO PRINT-X-IO-AREA (21:10)
               MOVE 2                      TO PRINT-X-BEFORE-SPACE
               PERFORM PRINT-X-PRINT-LINE
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE 'NY  MASTER    :'      TO PRINT-X-IO-AREA (6:15)
               MOVE ANT3                   TO XO-80YYZ
               MOVE XO-80YYZ               TO PRINT-X-IO-AREA (21:10)
               MOVE 2                      TO PRINT-X-BEFORE-SPACE
               PERFORM PRINT-X-PRINT-LINE
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE '************************' TO PRINT-X-IO-AREA
                                                                (1:24)
               MOVE '************************' TO PRINT-X-IO-AREA
                                                               (25:24)
               MOVE '****************'     TO PRINT-X-IO-AREA (48:16)
               MOVE 2                      TO PRINT-X-BEFORE-SPACE
               PERFORM PRINT-X-PRINT-LINE
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
           INITIALIZE FKINN-DATA-FIELDS
           SET FKINN-EOF-OFF               TO TRUE
           SET FKINN-PROCESS               TO TRUE
           MOVE LOW-VALUES                 TO FKINN-MC
                                              FKINN-MP
           OPEN INPUT FKINN
           INITIALIZE INNPUT-DATA-FIELDS
           SET INNPUT-EOF-OFF              TO TRUE
           SET INNPUT-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO INNPUT-MC
                                              INNPUT-MP
           OPEN INPUT INNPUT
           OPEN OUTPUT FKUT
           OPEN OUTPUT PRINT-X
           INITIALIZE PRINT-X-IO-AREA
           INITIALIZE PRINT-X-DATA-FIELDS
           MOVE 57                         TO PRINT-X-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FKINN
           CLOSE INNPUT
           CLOSE FKUT
           IF PRINT-X-IO-AREA NOT = SPACES
             WRITE PRINT-X-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO PRINT-X-IO-AREA
           END-IF
           CLOSE PRINT-X.
 
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
