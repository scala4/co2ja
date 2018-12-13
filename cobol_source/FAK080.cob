       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK080R.
      ******************************************* :   Z-WIN-RPG2     **
      *     OPPDATERING / KOPIERING AV VAREREC.MASTER.        * *
      * 15/2-94 TATT MED VAREREC. FRA FAKT.KONTANT.RUTINEN    * *
      ********************************************************* *
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK080.rpg
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
           SELECT FAKPAR
               ASSIGN TO UT-S-FAKPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKPAR-STATUS.
           SELECT INNF
               ASSIGN TO UT-S-INNF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INNF-STATUS.
           SELECT INNPUT
               ASSIGN TO UT-S-INNPUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INNPUT-STATUS.
           SELECT FKINN
               ASSIGN TO UT-S-FKINN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FKINN-STATUS.
           SELECT OUTF
               ASSIGN TO UT-S-OUTF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTF-STATUS.
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
       FD FAKPAR
               BLOCK CONTAINS 200
               RECORD CONTAINS 200.
       01  FAKPAR-IO-AREA.
           05  FAKPAR-IO-AREA-X            PICTURE X(200).
       FD INNF
               BLOCK CONTAINS 4100
               RECORD CONTAINS 82.
       01  INNF-IO-AREA.
           05  INNF-IO-AREA-X              PICTURE X(82).
       FD INNPUT
               BLOCK CONTAINS 4100
               RECORD CONTAINS 82.
       01  INNPUT-IO-AREA.
           05  INNPUT-IO-AREA-X            PICTURE X(82).
       FD FKINN
               BLOCK CONTAINS 4100
               RECORD CONTAINS 82.
       01  FKINN-IO-AREA.
           05  FKINN-IO-AREA-X             PICTURE X(82).
       FD OUTF
               BLOCK CONTAINS 4100
               RECORD CONTAINS 82.
       01  OUTF-IO-AREA.
           05  OUTF-IO-AREA-X              PICTURE X(82).
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
           10  FAKPAR-STATUS               PICTURE 99 VALUE 0.
           10  INNF-STATUS                 PICTURE 99 VALUE 0.
           10  INNPUT-STATUS               PICTURE 99 VALUE 0.
           10  FKINN-STATUS                PICTURE 99 VALUE 0.
           10  OUTF-STATUS                 PICTURE 99 VALUE 0.
           10  FKUT-STATUS                 PICTURE 99 VALUE 0.
           10  PRINT-X-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKPAR-EOF-OFF          VALUE '0'.
               88  FAKPAR-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKPAR-READ-OFF         VALUE '0'.
               88  FAKPAR-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKPAR-PROCESS-OFF      VALUE '0'.
               88  FAKPAR-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNF-EOF-OFF            VALUE '0'.
               88  INNF-EOF                VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNF-READ-OFF           VALUE '0'.
               88  INNF-READ               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNF-PROCESS-OFF        VALUE '0'.
               88  INNF-PROCESS            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNPUT-EOF-OFF          VALUE '0'.
               88  INNPUT-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNPUT-READ-OFF         VALUE '0'.
               88  INNPUT-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNPUT-PROCESS-OFF      VALUE '0'.
               88  INNPUT-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FKINN-EOF-OFF           VALUE '0'.
               88  FKINN-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FKINN-READ-OFF          VALUE '0'.
               88  FKINN-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FKINN-PROCESS-OFF       VALUE '0'.
               88  FKINN-PROCESS           VALUE '1'.
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
           05  FAKPAR-DATA-FIELDS.
               10  FOMGNR                  PICTURE X(1).
               10  PAAR                    PICTURE X(2).
               10  PMND                    PICTURE X(2).
           05  INNF-DATA-FIELDS.
               10  REC1                    PICTURE X(82).
               10  OMGNR1                  PICTURE X(1).
           05  INNF-MP                     PICTURE X(1).
           05  INNF-MC                     PICTURE X(1).
           05  INNF-M-02               REDEFINES INNF-MC.
               10  INNF-M-02-M1.
                   15  INNF-M-02-M1-OMGNR1-G.
                       20  INNF-M-02-M1-OMGNR1 PICTURE X(1).
           05  INNPUT-DATA-FIELDS.
               10  REC2                    PICTURE X(82).
               10  OMGNR2                  PICTURE X(1).
           05  INNPUT-MP                   PICTURE X(1).
           05  INNPUT-MC                   PICTURE X(1).
           05  INNPUT-M-03             REDEFINES INNPUT-MC.
               10  INNPUT-M-03-M1.
                   15  INNPUT-M-03-M1-OMGNR2-G.
                       20  INNPUT-M-03-M1-OMGNR2 PICTURE X(1).
           05  FKINN-DATA-FIELDS.
               10  REC3                    PICTURE X(82).
               10  FKMND                   PICTURE X(2).
               10  FKAAR                   PICTURE X(2).
               10  OMGNR3                  PICTURE X(1).
      *****************************************************************
      * FAKTURA PARAMETER RUTINEN.                                    *
      *****************************************************************
           05  FKINN-MP                    PICTURE X(1).
           05  FKINN-MC                    PICTURE X(1).
           05  FKINN-M-04              REDEFINES FKINN-MC.
               10  FKINN-M-04-M1.
                   15  FKINN-M-04-M1-OMGNR3-G.
                       20  FKINN-M-04-M1-OMGNR3 PICTURE X(1).
           05  TEMPORARY-FIELDS.
               10  ANT1-IO.
                   15  ANT1                PICTURE S9(8).
               10  ANT2-IO.
                   15  ANT2                PICTURE S9(8).
               10  ANT3-IO.
                   15  ANT3                PICTURE S9(8).
               10  ANT4-IO.
                   15  ANT4                PICTURE S9(8).
               10  ANT4DM-IO.
                   15  ANT4DM              PICTURE S9(8).
               10  ANTK-IO.
                   15  ANTK                PICTURE S9(8).
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
           SET NOT-I-03                    TO TRUE
           SET NOT-I-04                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  INNF-PROCESS
               SET INNF-PROCESS-OFF        TO TRUE
               SET INNF-READ               TO TRUE
           END-IF
 
           IF  INNF-READ
               PERFORM INNF-GET
               SET INNF-READ-OFF           TO TRUE
               IF  NOT INNF-EOF
                   PERFORM INNF-MATCH-SET
               END-IF
           END-IF
 
           IF  FAKPAR-PROCESS
               SET FAKPAR-PROCESS-OFF      TO TRUE
               SET FAKPAR-READ             TO TRUE
           END-IF
 
           IF  FAKPAR-READ
           AND RECORD-SELECTED-OFF
               PERFORM FAKPAR-GET
               SET FAKPAR-READ-OFF         TO TRUE
               IF  NOT FAKPAR-EOF
                   SET FAKPAR-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
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
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  FAKPAR-PROCESS
               PERFORM FAKPAR-IDSET
           END-IF
 
           IF  INNF-PROCESS
               PERFORM INNF-IDSET
           END-IF
 
           IF  INNPUT-PROCESS
               PERFORM INNPUT-IDSET
           END-IF
 
           IF  FKINN-PROCESS
               PERFORM FKINN-IDSET
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
 
           IF  FAKPAR-PROCESS
               PERFORM FAKPAR-FLDSET
           END-IF
 
           IF  INNF-PROCESS
               PERFORM INNF-FLDSET
           END-IF
 
           IF  INNPUT-PROCESS
               PERFORM INNPUT-FLDSET
           END-IF
 
           IF  FKINN-PROCESS
               PERFORM FKINN-FLDSET
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
               SET NOT-I-21                TO TRUE
               IF  FOMGNR = '1'
                   SET I-21                TO TRUE
               END-IF
               SET NOT-I-22                TO TRUE
               IF  FOMGNR = '2'
                   SET I-22                TO TRUE
               END-IF
               SET NOT-I-23                TO TRUE
               IF  FOMGNR = '3'
                   SET I-23                TO TRUE
               END-IF
               SET NOT-I-24                TO TRUE
               IF  FOMGNR = '4'
                   SET I-24                TO TRUE
               END-IF
               SET NOT-I-25                TO TRUE
               IF  FOMGNR = '5'
                   SET I-25                TO TRUE
               END-IF
               GO TO SLUTT-T
      *****************************************************************
      * RUTINE FOR FAKT.VAREREC. NULLSTILLES VED 1. FAKT PR. MND.     *
      *****************************************************************
           END-IF
           IF  (I-02 AND NOT-I-21 AND NOT-I-MR)
               ADD 1                       TO ANT1
           END-IF
           IF  (I-03)
               ADD 1                       TO ANT2
           END-IF
           IF  (I-02 AND NOT-I-21 AND NOT-I-MR)
               ADD 1                       TO ANT3
           END-IF
           IF  (I-03)
               ADD 1                       TO ANT3
      *****************************************************************
      *  RUTINE FOR Å TA MED RECORDS FRA FAKTURA KONTANT RUTINE       *
      *  RECORDS FOR INNEVÆRENDE MÅNED BLIR MED.                      *
      *****************************************************************
           END-IF
           SET NOT-I-90                    TO TRUE
           SET NOT-I-91                    TO TRUE
           IF  (NOT-I-04)
               GO TO SLUTT-T
           END-IF
           ADD 1                           TO ANT4
           SET NOT-I-90                    TO TRUE
           IF  FKAAR = PAAR
               SET I-90                    TO TRUE
           END-IF
           IF  (I-90)
               SET NOT-I-91                TO TRUE
               IF  FKMND = PMND
                   SET I-91                TO TRUE
               END-IF
           END-IF
           IF  (I-91)
               ADD 1                       TO ANT4DM
               ADD 1                       TO ANT3
           END-IF
           IF  (NOT-I-91)
               ADD 1                       TO ANTK
           END-IF.
 
       SLUTT-T.
           CONTINUE.
 
       FAKPAR-GET SECTION.
       FAKPAR-GET-P.
           IF  FAKPAR-EOF-OFF
               READ FAKPAR
               AT END
                   SET FAKPAR-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FAKPAR-FLDSET SECTION.
       FAKPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKPAR-IO-AREA (3:1)   TO FOMGNR (1:1)
               MOVE FAKPAR-IO-AREA (6:2)   TO PAAR (1:2)
               MOVE FAKPAR-IO-AREA (8:2)   TO PMND (1:2)
           END-EVALUATE.
 
       FAKPAR-IDSET SECTION.
       FAKPAR-IDSET-P.
           SET I-01                        TO TRUE.
 
       INNF-GET SECTION.
       INNF-GET-P.
           IF  INNF-EOF-OFF
               READ INNF
               AT END
                   SET INNF-EOF            TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INNF-FLDSET SECTION.
       INNF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INNF-IO-AREA (1:82)    TO REC1 (1:82)
               MOVE INNF-IO-AREA (68:1)    TO OMGNR1 (1:1)
           END-EVALUATE.
 
       INNF-IDSET SECTION.
       INNF-IDSET-P.
           SET I-02                        TO TRUE.
 
       INNF-MATCH-SET SECTION.
       INNF-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE INNF-IO-AREA (68:1)    TO INNF-M-02-M1-OMGNR1
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
               MOVE INNPUT-IO-AREA (68:1)  TO OMGNR2 (1:1)
           END-EVALUATE.
 
       INNPUT-IDSET SECTION.
       INNPUT-IDSET-P.
           SET I-03                        TO TRUE.
 
       INNPUT-MATCH-SET SECTION.
       INNPUT-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE INNPUT-IO-AREA (68:1)  TO INNPUT-M-03-M1-OMGNR2
           END-EVALUATE.
 
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
               MOVE FKINN-IO-AREA (1:82)   TO REC3 (1:82)
               MOVE FKINN-IO-AREA (42:2)   TO FKMND (1:2)
               MOVE FKINN-IO-AREA (58:2)   TO FKAAR (1:2)
               MOVE FKINN-IO-AREA (68:1)   TO OMGNR3 (1:1)
           END-EVALUATE.
 
       FKINN-IDSET SECTION.
       FKINN-IDSET-P.
           SET I-04                        TO TRUE.
 
       FKINN-MATCH-SET SECTION.
       FKINN-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE FKINN-IO-AREA (68:1)   TO FKINN-M-04-M1-OMGNR3
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
           IF  INNF-EOF
               MOVE HIGH-VALUES            TO INNF-MC
                                              INNF-MP
           END-IF
           IF  INNPUT-EOF
               MOVE HIGH-VALUES            TO INNPUT-MC
                                              INNPUT-MP
           END-IF
           IF  FKINN-EOF
               MOVE HIGH-VALUES            TO FKINN-MC
                                              FKINN-MP
           END-IF
           IF  INNF-MC < INNF-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  INNPUT-MC < INNPUT-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  FKINN-MC < FKINN-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  INNF-MC < INNPUT-MC
            AND  INNF-MC < FKINN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET INNF-PROCESS        TO TRUE
                   MOVE INNF-MC            TO INNF-MP
                   IF  INNF-MC = INNPUT-MP
                     OR  INNF-MC = FKINN-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  INNPUT-MC < INNF-MC
            AND  INNPUT-MC < FKINN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET INNPUT-PROCESS      TO TRUE
                   MOVE INNPUT-MC          TO INNPUT-MP
                   IF  INNPUT-MC = INNF-MP
                     OR  INNPUT-MC = FKINN-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  FKINN-MC < INNF-MC
            AND  FKINN-MC < INNPUT-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FKINN-PROCESS       TO TRUE
                   MOVE FKINN-MC           TO FKINN-MP
                   IF  FKINN-MC = INNF-MP
                     OR  FKINN-MC = INNPUT-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  INNF-MC = INNPUT-MC
             OR  INNF-MC = FKINN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET INNF-PROCESS        TO TRUE
                   MOVE INNF-MC            TO INNF-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           WHEN  INNPUT-MC = FKINN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET INNPUT-PROCESS      TO TRUE
                   MOVE INNPUT-MC          TO INNPUT-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND NOT-I-21 AND NOT-I-MR)
               MOVE SPACES TO OUTF-IO-AREA
               INITIALIZE OUTF-IO-AREA
               MOVE REC1                   TO OUTF-IO-AREA (1:82)
               WRITE OUTF-IO-AREA
           END-IF
           IF  (I-03)
               MOVE SPACES TO OUTF-IO-AREA
               INITIALIZE OUTF-IO-AREA
               MOVE REC2                   TO OUTF-IO-AREA (1:82)
               WRITE OUTF-IO-AREA
           END-IF
           IF  (I-04 AND I-91)
               MOVE SPACES TO OUTF-IO-AREA
               INITIALIZE OUTF-IO-AREA
               MOVE REC3                   TO OUTF-IO-AREA (1:82)
               IF  (I-21)
                   MOVE 'A'                TO OUTF-IO-AREA (68:1)
               END-IF
               IF  (I-22)
                   MOVE 'B'                TO OUTF-IO-AREA (68:1)
               END-IF
               IF  (I-23)
                   MOVE 'C'                TO OUTF-IO-AREA (68:1)
               END-IF
               IF  (I-24)
                   MOVE 'D'                TO OUTF-IO-AREA (68:1)
               END-IF
               IF  (I-25)
                   MOVE 'E'                TO OUTF-IO-AREA (68:1)
               END-IF
               WRITE OUTF-IO-AREA
           END-IF
           IF  (I-04 AND NOT-I-91)
               MOVE SPACES TO FKUT-IO-AREA
               INITIALIZE FKUT-IO-AREA
               MOVE REC3                   TO FKUT-IO-AREA (1:82)
               WRITE FKUT-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE '***   AVSTEMNINGSTOTALER' TO PRINT-X-IO-AREA
                                                                (1:24)
               MOVE 'F A K 0 8 0'          TO PRINT-X-IO-AREA (26:11)
               MOVE 01                     TO PRINT-X-BEFORE-SKIP
               PERFORM PRINT-X-PRINT-LINE
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE '************************' TO PRINT-X-IO-AREA
                                                                (1:24)
               MOVE '************************' TO PRINT-X-IO-AREA
                                                               (25:24)
               MOVE '***************'      TO PRINT-X-IO-AREA (49:15)
               MOVE 1                      TO PRINT-X-BEFORE-SPACE
               PERFORM PRINT-X-PRINT-LINE
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE 'GAMMEL MASTER NULLSTILLE' TO PRINT-X-IO-AREA
                                                                (6:24)
               MOVE 'S VED FØRSTE GANGS'   TO PRINT-X-IO-AREA (30:18)
               MOVE 3                      TO PRINT-X-BEFORE-SPACE
               PERFORM PRINT-X-PRINT-LINE
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE 'KJØRING PR MÅNED.'    TO PRINT-X-IO-AREA (6:17)
               MOVE 1                      TO PRINT-X-BEFORE-SPACE
               PERFORM PRINT-X-PRINT-LINE
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE 'GAMMEL MASTER :'      TO PRINT-X-IO-AREA (6:15)
               MOVE ANT1                   TO XO-80YYZ
               MOVE XO-80YYZ               TO PRINT-X-IO-AREA (21:10)
               MOVE 3                      TO PRINT-X-BEFORE-SPACE
               PERFORM PRINT-X-PRINT-LINE
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE 'DENNE OMGANG  :'      TO PRINT-X-IO-AREA (6:15)
               MOVE ANT2                   TO XO-80YYZ
               MOVE XO-80YYZ               TO PRINT-X-IO-AREA (21:10)
               MOVE 2                      TO PRINT-X-BEFORE-SPACE
               PERFORM PRINT-X-PRINT-LINE
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE 'FRA FAKT.KONT.:'      TO PRINT-X-IO-AREA (6:15)
               MOVE ANT4DM                 TO XO-80YYZ
               MOVE XO-80YYZ               TO PRINT-X-IO-AREA (21:10)
               MOVE 2                      TO PRINT-X-BEFORE-SPACE
               PERFORM PRINT-X-PRINT-LINE
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE 'NY MASTER     :'      TO PRINT-X-IO-AREA (6:15)
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
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE 'FAKTURA KONTANTSALGS RUT' TO PRINT-X-IO-AREA
                                                                (6:24)
               MOVE 'INE VARERECORDS.  '   TO PRINT-X-IO-AREA (30:18)
               MOVE 3                      TO PRINT-X-BEFORE-SPACE
               PERFORM PRINT-X-PRINT-LINE
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE 'GML FAKT.KONT.:'      TO PRINT-X-IO-AREA (6:15)
               MOVE ANT4                   TO XO-80YYZ
               MOVE XO-80YYZ               TO PRINT-X-IO-AREA (21:10)
               MOVE 2                      TO PRINT-X-BEFORE-SPACE
               PERFORM PRINT-X-PRINT-LINE
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE 'FJERNET NÅ.   :'      TO PRINT-X-IO-AREA (6:15)
               MOVE ANT4DM                 TO XO-80YYZ
               MOVE XO-80YYZ               TO PRINT-X-IO-AREA (21:10)
               MOVE 2                      TO PRINT-X-BEFORE-SPACE
               PERFORM PRINT-X-PRINT-LINE
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE 'NY  FAKT.KONT.:'      TO PRINT-X-IO-AREA (6:15)
               MOVE ANTK                   TO XO-80YYZ
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
           MOVE 4                          TO LR-CHECK
           INITIALIZE FAKPAR-DATA-FIELDS
           SET FAKPAR-EOF-OFF              TO TRUE
           SET FAKPAR-PROCESS              TO TRUE
           OPEN INPUT FAKPAR
           INITIALIZE INNF-DATA-FIELDS
           SET INNF-EOF-OFF                TO TRUE
           SET INNF-PROCESS                TO TRUE
           MOVE LOW-VALUES                 TO INNF-MC
                                              INNF-MP
           OPEN INPUT INNF
           INITIALIZE INNPUT-DATA-FIELDS
           SET INNPUT-EOF-OFF              TO TRUE
           SET INNPUT-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO INNPUT-MC
                                              INNPUT-MP
           OPEN INPUT INNPUT
           INITIALIZE FKINN-DATA-FIELDS
           SET FKINN-EOF-OFF               TO TRUE
           SET FKINN-PROCESS               TO TRUE
           MOVE LOW-VALUES                 TO FKINN-MC
                                              FKINN-MP
           OPEN INPUT FKINN
           OPEN OUTPUT OUTF
           OPEN OUTPUT FKUT
           OPEN OUTPUT PRINT-X
           INITIALIZE PRINT-X-IO-AREA
           INITIALIZE PRINT-X-DATA-FIELDS
           MOVE 57                         TO PRINT-X-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FAKPAR
           CLOSE INNF
           CLOSE INNPUT
           CLOSE FKINN
           CLOSE OUTF
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
