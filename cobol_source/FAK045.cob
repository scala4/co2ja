       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK045R.
      ******************************************* :   Z-WIN-RPG2     **
      * PROGRAM: FAK045    OPPDATERING AV SAMLEFAKTURA.               *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK045.rpg
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
           SELECT GMLSAM
               ASSIGN TO UT-S-GMLSAM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS GMLSAM-STATUS.
           SELECT SAMFAK
               ASSIGN TO UT-S-SAMFAK
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS SAMFAK-STATUS.
           SELECT NYSAMF
               ASSIGN TO UT-S-NYSAMF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS NYSAMF-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD GMLSAM
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  GMLSAM-IO-AREA.
           05  GMLSAM-IO-AREA-X            PICTURE X(200).
       FD SAMFAK
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  SAMFAK-IO-AREA.
           05  SAMFAK-IO-AREA-X            PICTURE X(200).
       FD NYSAMF
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  NYSAMF-IO-AREA.
           05  NYSAMF-IO-AREA-X            PICTURE X(200).
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
           10  GMLSAM-STATUS               PICTURE 99 VALUE 0.
           10  SAMFAK-STATUS               PICTURE 99 VALUE 0.
           10  NYSAMF-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLSAM-EOF-OFF          VALUE '0'.
               88  GMLSAM-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLSAM-READ-OFF         VALUE '0'.
               88  GMLSAM-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLSAM-PROCESS-OFF      VALUE '0'.
               88  GMLSAM-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SAMFAK-EOF-OFF          VALUE '0'.
               88  SAMFAK-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SAMFAK-READ-OFF         VALUE '0'.
               88  SAMFAK-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SAMFAK-PROCESS-OFF      VALUE '0'.
               88  SAMFAK-PROCESS          VALUE '1'.
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
           05  GMLSAM-DATA-FIELDS.
               10  REC1                    PICTURE X(200).
               10  FNRKNR                  PICTURE X(9).
               10  ORDNR                   PICTURE X(6).
           05  GMLSAM-MP                   PICTURE X(15).
           05  GMLSAM-MC                   PICTURE X(15).
           05  GMLSAM-M-01             REDEFINES GMLSAM-MC.
               10  GMLSAM-M-01-M2.
                   15  GMLSAM-M-01-M2-FNRKNR-G.
                       20  GMLSAM-M-01-M2-FNRKNR PICTURE X(9).
               10  GMLSAM-M-01-M1.
                   15  GMLSAM-M-01-M1-ORDNR-G.
                       20  GMLSAM-M-01-M1-ORDNR PICTURE X(6).
           05  SAMFAK-DATA-FIELDS.
               10  REC2                    PICTURE X(200).
           05  SAMFAK-MP                   PICTURE X(15).
           05  SAMFAK-MC                   PICTURE X(15).
           05  SAMFAK-M-02             REDEFINES SAMFAK-MC.
               10  SAMFAK-M-02-M2.
                   15  SAMFAK-M-02-M2-FNRKNR-G.
                       20  SAMFAK-M-02-M2-FNRKNR PICTURE X(9).
               10  SAMFAK-M-02-M1.
                   15  SAMFAK-M-02-M1-ORDNR-G.
                       20  SAMFAK-M-02-M1-ORDNR PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  AKKGML-IO.
                   15  AKKGML              PICTURE S9(6).
               10  AKKSAM-IO.
                   15  AKKSAM              PICTURE S9(6).
               10  AKKNY-IO.
                   15  AKKNY               PICTURE S9(6).
           05  EDITTING-FIELDS.
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
           IF  GMLSAM-PROCESS
               SET GMLSAM-PROCESS-OFF      TO TRUE
               SET GMLSAM-READ             TO TRUE
           END-IF
 
           IF  GMLSAM-READ
               PERFORM GMLSAM-GET
               SET GMLSAM-READ-OFF         TO TRUE
               IF  NOT GMLSAM-EOF
                   PERFORM GMLSAM-MATCH-SET
               END-IF
           END-IF
 
           IF  SAMFAK-PROCESS
               SET SAMFAK-PROCESS-OFF      TO TRUE
               SET SAMFAK-READ             TO TRUE
           END-IF
 
           IF  SAMFAK-READ
               PERFORM SAMFAK-GET
               SET SAMFAK-READ-OFF         TO TRUE
               IF  NOT SAMFAK-EOF
                   PERFORM SAMFAK-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  GMLSAM-PROCESS
               PERFORM GMLSAM-IDSET
           END-IF
 
           IF  SAMFAK-PROCESS
               PERFORM SAMFAK-IDSET
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
 
           IF  GMLSAM-PROCESS
               PERFORM GMLSAM-FLDSET
           END-IF
 
           IF  SAMFAK-PROCESS
               PERFORM SAMFAK-FLDSET
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
               ADD 1                       TO AKKGML
           END-IF
           IF  (I-02)
               ADD 1                       TO AKKSAM
           END-IF
           ADD 1                           TO AKKNY.
 
       GMLSAM-GET SECTION.
       GMLSAM-GET-P.
           IF  GMLSAM-EOF-OFF
               READ GMLSAM
               AT END
                   SET GMLSAM-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       GMLSAM-FLDSET SECTION.
       GMLSAM-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE GMLSAM-IO-AREA (1:200) TO REC1 (1:200)
               MOVE GMLSAM-IO-AREA (1:9)   TO FNRKNR (1:9)
               MOVE GMLSAM-IO-AREA (19:6)  TO ORDNR (1:6)
           END-EVALUATE.
 
       GMLSAM-IDSET SECTION.
       GMLSAM-IDSET-P.
           SET I-01                        TO TRUE.
 
       GMLSAM-MATCH-SET SECTION.
       GMLSAM-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE GMLSAM-IO-AREA (1:9)   TO GMLSAM-M-01-M2-FNRKNR
               MOVE GMLSAM-IO-AREA (19:6)  TO GMLSAM-M-01-M1-ORDNR
           END-EVALUATE.
 
       SAMFAK-GET SECTION.
       SAMFAK-GET-P.
           IF  SAMFAK-EOF-OFF
               READ SAMFAK
               AT END
                   SET SAMFAK-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       SAMFAK-FLDSET SECTION.
       SAMFAK-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE SAMFAK-IO-AREA (1:200) TO REC2 (1:200)
               MOVE SAMFAK-IO-AREA (1:9)   TO FNRKNR (1:9)
               MOVE SAMFAK-IO-AREA (19:6)  TO ORDNR (1:6)
           END-EVALUATE.
 
       SAMFAK-IDSET SECTION.
       SAMFAK-IDSET-P.
           SET I-02                        TO TRUE.
 
       SAMFAK-MATCH-SET SECTION.
       SAMFAK-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE SAMFAK-IO-AREA (1:9)   TO SAMFAK-M-02-M2-FNRKNR
               MOVE SAMFAK-IO-AREA (19:6)  TO SAMFAK-M-02-M1-ORDNR
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
           IF  GMLSAM-EOF
               MOVE HIGH-VALUES            TO GMLSAM-MC
                                              GMLSAM-MP
           END-IF
           IF  SAMFAK-EOF
               MOVE HIGH-VALUES            TO SAMFAK-MC
                                              SAMFAK-MP
           END-IF
           IF  GMLSAM-MC < GMLSAM-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  SAMFAK-MC < SAMFAK-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  GMLSAM-MC < SAMFAK-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET GMLSAM-PROCESS      TO TRUE
                   MOVE GMLSAM-MC          TO GMLSAM-MP
                   IF  GMLSAM-MC = SAMFAK-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  SAMFAK-MC < GMLSAM-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET SAMFAK-PROCESS      TO TRUE
                   MOVE SAMFAK-MC          TO SAMFAK-MP
                   IF  SAMFAK-MC = GMLSAM-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  GMLSAM-MC = SAMFAK-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET GMLSAM-PROCESS      TO TRUE
                   MOVE GMLSAM-MC          TO GMLSAM-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               MOVE SPACES TO NYSAMF-IO-AREA
               INITIALIZE NYSAMF-IO-AREA
               MOVE REC1                   TO NYSAMF-IO-AREA (1:200)
               WRITE NYSAMF-IO-AREA
           END-IF
           IF  (I-02)
               MOVE SPACES TO NYSAMF-IO-AREA
               INITIALIZE NYSAMF-IO-AREA
               MOVE REC2                   TO NYSAMF-IO-AREA (1:200)
               WRITE NYSAMF-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '***   AVSTEMNINGSTOTALER' TO LISTE-IO-AREA (1:24)
               MOVE '    --- FAK045 ---   ***' TO LISTE-IO-AREA (25:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT GML SAMLEFAKT '   TO LISTE-IO-AREA (3:18)
               MOVE AKKGML                 TO XO-60YNZ
               MOVE XO-60YNZ               TO LISTE-IO-AREA (23:6)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT SAMLEFAKT D.P.'   TO LISTE-IO-AREA (3:18)
               MOVE AKKSAM                 TO XO-60YNZ
               MOVE XO-60YNZ               TO LISTE-IO-AREA (23:6)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT NY SAMLEFAKT  '   TO LISTE-IO-AREA (3:18)
               MOVE AKKNY                  TO XO-60YNZ
               MOVE XO-60YNZ               TO LISTE-IO-AREA (23:6)
               MOVE 2                      TO LISTE-AFTER-SPACE
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
           INITIALIZE GMLSAM-DATA-FIELDS
           SET GMLSAM-EOF-OFF              TO TRUE
           SET GMLSAM-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO GMLSAM-MC
                                              GMLSAM-MP
           OPEN INPUT GMLSAM
           INITIALIZE SAMFAK-DATA-FIELDS
           SET SAMFAK-EOF-OFF              TO TRUE
           SET SAMFAK-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO SAMFAK-MC
                                              SAMFAK-MP
           OPEN INPUT SAMFAK
           OPEN OUTPUT NYSAMF
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE GMLSAM
           CLOSE SAMFAK
           CLOSE NYSAMF
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
