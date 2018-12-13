       IDENTIFICATION DIVISION.
       PROGRAM-ID. RKO025R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM.......: RSK025, OPPDATERING AV REGFILE.              *
      *  E 15.04.11....: DAGENS TRANSER UTVIDET TIL 240               *
      *  E 05.10.12....: SKRIVER AVSTEMMINGSFILE                      * 000004
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RKO025.rpg
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
           SELECT GMLREG
               ASSIGN TO UT-S-GMLREG
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS GMLREG-STATUS.
           SELECT DAGREG
               ASSIGN TO UT-S-DAGREG
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS DAGREG-STATUS.
           SELECT NYREG
               ASSIGN TO UT-S-NYREG
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS NYREG-STATUS.
           SELECT AVSTEMM
               ASSIGN TO UT-S-AVSTEMM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS AVSTEMM-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD GMLREG
               BLOCK CONTAINS 9360
               RECORD CONTAINS 120.
       01  GMLREG-IO-AREA.
           05  GMLREG-IO-AREA-X            PICTURE X(120).
       FD DAGREG
               BLOCK CONTAINS 240
               RECORD CONTAINS 240.
       01  DAGREG-IO-AREA.
           05  DAGREG-IO-AREA-X            PICTURE X(240).
       FD NYREG
               BLOCK CONTAINS 9360
               RECORD CONTAINS 120.
       01  NYREG-IO-AREA.
           05  NYREG-IO-AREA-X             PICTURE X(120).
       FD AVSTEMM
               BLOCK CONTAINS 120
               RECORD CONTAINS 120.
       01  AVSTEMM-IO-AREA.
           05  AVSTEMM-IO-AREA-X           PICTURE X(120).
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
           10  GMLREG-STATUS               PICTURE 99 VALUE 0.
           10  DAGREG-STATUS               PICTURE 99 VALUE 0.
           10  NYREG-STATUS                PICTURE 99 VALUE 0.
           10  AVSTEMM-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLREG-EOF-OFF          VALUE '0'.
               88  GMLREG-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLREG-READ-OFF         VALUE '0'.
               88  GMLREG-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLREG-PROCESS-OFF      VALUE '0'.
               88  GMLREG-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  DAGREG-EOF-OFF          VALUE '0'.
               88  DAGREG-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  DAGREG-READ-OFF         VALUE '0'.
               88  DAGREG-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  DAGREG-PROCESS-OFF      VALUE '0'.
               88  DAGREG-PROCESS          VALUE '1'.
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
           05  GMLREG-DATA-FIELDS.
               10  REC1                    PICTURE X(120).
               10  RA1                     PICTURE X(2).
               10  BEL1-IO.
                   15  BEL1                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  REGN1                   PICTURE X(1).
           05  GMLREG-MP                   PICTURE X(2).
           05  GMLREG-MC                   PICTURE X(2).
           05  GMLREG-M-01             REDEFINES GMLREG-MC.
               10  GMLREG-M-01-M1.
                   15  GMLREG-M-01-M1-RA1-G.
                       20  GMLREG-M-01-M1-RA1 PICTURE X(2).
           05  DAGREG-DATA-FIELDS.
               10  REC2                    PICTURE X(120).
               10  RA2                     PICTURE X(2).
               10  BEL2-IO.
                   15  BEL2                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  TEGN2                   PICTURE X(1).
           05  DAGREG-MP                   PICTURE X(2).
           05  DAGREG-MC                   PICTURE X(2).
           05  DAGREG-M-02             REDEFINES DAGREG-MC.
               10  DAGREG-M-02-M1.
                   15  DAGREG-M-02-M1-RA2-G.
                       20  DAGREG-M-02-M1-RA2 PICTURE X(2).
           05  TEMPORARY-FIELDS.
               10  F6-IO.
                   15  F6                  PICTURE S9(6).
               10  DD-IO.
                   15  DD                  PICTURE S9(2).
               10  AA-IO.
                   15  AA                  PICTURE S9(2).
               10  DDATO                   PICTURE X(6).
               10  DDATO8                  PICTURE X(8).
               10  TIDSP-IO.
                   15  TIDSP               PICTURE S9(6).
               10  TOTREC-IO.
                   15  TOTREC              PICTURE S9(8).
               10  TOTBEL-IO.
                   15  TOTBEL              PICTURE S9(9)V9(2).
               10  LRRGAN-IO.
                   15  LRRGAN              PICTURE S9(13).
               10  LRRGBE-IO.
                   15  LRRGBE              PICTURE S9(13)V9(2).
               10  LRRGBU-IO.
                   15  LRRGBU              PICTURE S9(13)V9(2).
           05  EDITTING-FIELDS.
               10  XO-80YNZ                PICTURE ZZZZZZZZ.
               10  EDIT-TOTBEL             PICTURE ZZZ.ZZZ.ZZZ,ZZ-.
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
           IF  GMLREG-PROCESS
               SET GMLREG-PROCESS-OFF      TO TRUE
               SET GMLREG-READ             TO TRUE
           END-IF
 
           IF  GMLREG-READ
               PERFORM GMLREG-GET
               SET GMLREG-READ-OFF         TO TRUE
               IF  NOT GMLREG-EOF
                   PERFORM GMLREG-MATCH-SET
               END-IF
           END-IF
 
           IF  DAGREG-PROCESS
               SET DAGREG-PROCESS-OFF      TO TRUE
               SET DAGREG-READ             TO TRUE
           END-IF
 
           IF  DAGREG-READ
               PERFORM DAGREG-GET
               SET DAGREG-READ-OFF         TO TRUE
               IF  NOT DAGREG-EOF
                   PERFORM DAGREG-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  GMLREG-PROCESS
               PERFORM GMLREG-IDSET
           END-IF
 
           IF  DAGREG-PROCESS
               PERFORM DAGREG-IDSET
           END-IF
 
           IF I-1ST
               GO TO LR-INDICATOR-TEST
           END-IF.
 
       MAINLINE-TOTAL-CALCS.
           IF  I-LR
               PERFORM LR-CALCS
           END-IF
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
 
           IF  GMLREG-PROCESS
               PERFORM GMLREG-FLDOFF
               PERFORM GMLREG-FLDSET
           END-IF
 
           IF  DAGREG-PROCESS
               PERFORM DAGREG-FLDOFF
               PERFORM DAGREG-FLDSET
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
           IF  (I-88)
               SET NOT-I-89                TO TRUE
           END-IF
           IF  (NOT-I-88)
               SET I-88                    TO TRUE
               SET I-89                    TO TRUE
           END-IF
           IF  (I-89)
               MOVE UDATE                  TO F6-IO
               MOVE F6 (1:2)               TO DD
               MOVE F6 (5:2)               TO AA-IO
      ** MLLzo
               IF AA < 0
                   MULTIPLY -1 BY AA
               END-IF
               MOVE F6                     TO DDATO
               MOVE AA                     TO DDATO (1:2)
               MOVE DD                     TO DDATO (5:2)
               SET NOT-I-31                TO TRUE
               IF  AA > 80
                   SET I-31                TO TRUE
               END-IF
               MOVE DDATO                  TO DDATO8 (3:6)
           END-IF
           IF  (I-89 AND I-31)
               MOVE '19'                   TO DDATO8 (1:2)
           END-IF
           IF  (I-89 AND NOT-I-31)
               MOVE '20'                   TO DDATO8 (1:2)
           END-IF
           IF  (I-89)
               ACCEPT SYSTEM-TIME-X      FROM TIME
               MOVE SYSTEM-TIME            TO TIDSP (1:6)
           END-IF
           IF  (I-01)
               ADD 1                       TO TOTREC
           END-IF
           IF  (I-01 AND I-10)
               ADD BEL1                    TO TOTBEL
           END-IF
           IF  (I-01 AND NOT-I-10)
               SUBTRACT BEL1               FROM TOTBEL
           END-IF
           IF  (I-02)
               ADD 1                       TO TOTREC
           END-IF
           IF  (I-02 AND I-11)
               ADD BEL2                    TO TOTBEL
           END-IF
           IF  (I-02 AND NOT-I-11)
               SUBTRACT BEL2               FROM TOTBEL
           END-IF.
 
       LR-CALCS SECTION.
       LR-CALCS-P.
           ADD TOTREC TO ZERO          GIVING LRRGAN
           ADD TOTBEL TO ZERO          GIVING LRRGBE
           ADD TOTBEL TO ZERO          GIVING LRRGBU.
 
       GMLREG-GET SECTION.
       GMLREG-GET-P.
           IF  GMLREG-EOF-OFF
               READ GMLREG
               AT END
                   SET GMLREG-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       GMLREG-FLDOFF SECTION.
       GMLREG-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-10                TO TRUE
           END-EVALUATE.
 
       GMLREG-FLDSET SECTION.
       GMLREG-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE GMLREG-IO-AREA (1:120) TO REC1 (1:120)
               MOVE GMLREG-IO-AREA (1:2)   TO RA1 (1:2)
               MOVE GMLREG-IO-AREA (27:5)  TO BEL1-IO
               MOVE GMLREG-IO-AREA (32:1)  TO REGN1 (1:1)
               IF  REGN1 = SPACES
                   SET I-10                TO TRUE
               END-IF
           END-EVALUATE.
 
       GMLREG-IDSET SECTION.
       GMLREG-IDSET-P.
           SET I-01                        TO TRUE.
 
       GMLREG-MATCH-SET SECTION.
       GMLREG-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE GMLREG-IO-AREA (1:2)   TO GMLREG-M-01-M1-RA1
           END-EVALUATE.
 
       DAGREG-GET SECTION.
       DAGREG-GET-P.
           IF  DAGREG-EOF-OFF
               READ DAGREG
               AT END
                   SET DAGREG-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       DAGREG-FLDOFF SECTION.
       DAGREG-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-11                TO TRUE
           END-EVALUATE.
 
       DAGREG-FLDSET SECTION.
       DAGREG-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE DAGREG-IO-AREA (1:120) TO REC2 (1:120)
               MOVE DAGREG-IO-AREA (1:2)   TO RA2 (1:2)
               MOVE DAGREG-IO-AREA (27:5)  TO BEL2-IO
               MOVE DAGREG-IO-AREA (32:1)  TO TEGN2 (1:1)
               IF  TEGN2 = SPACES
                   SET I-11                TO TRUE
               END-IF
           END-EVALUATE.
 
       DAGREG-IDSET SECTION.
       DAGREG-IDSET-P.
           SET I-02                        TO TRUE.
 
       DAGREG-MATCH-SET SECTION.
       DAGREG-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE DAGREG-IO-AREA (1:2)   TO DAGREG-M-02-M1-RA2
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
           IF  GMLREG-EOF
               MOVE HIGH-VALUES            TO GMLREG-MC
                                              GMLREG-MP
           END-IF
           IF  DAGREG-EOF
               MOVE HIGH-VALUES            TO DAGREG-MC
                                              DAGREG-MP
           END-IF
           IF  GMLREG-MC < GMLREG-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  DAGREG-MC < DAGREG-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  GMLREG-MC < DAGREG-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET GMLREG-PROCESS      TO TRUE
                   MOVE GMLREG-MC          TO GMLREG-MP
                   IF  GMLREG-MC = DAGREG-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  DAGREG-MC < GMLREG-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET DAGREG-PROCESS      TO TRUE
                   MOVE DAGREG-MC          TO DAGREG-MP
                   IF  DAGREG-MC = GMLREG-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  GMLREG-MC = DAGREG-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET GMLREG-PROCESS      TO TRUE
                   MOVE GMLREG-MC          TO GMLREG-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               MOVE SPACES TO NYREG-IO-AREA
               INITIALIZE NYREG-IO-AREA
               MOVE REC1                   TO NYREG-IO-AREA (1:120)
               WRITE NYREG-IO-AREA
           END-IF
           IF  (I-02)
               MOVE SPACES TO NYREG-IO-AREA
               INITIALIZE NYREG-IO-AREA
               MOVE REC2                   TO NYREG-IO-AREA (1:120)
               WRITE NYREG-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO AVSTEMM-IO-AREA
               INITIALIZE AVSTEMM-IO-AREA
               MOVE DDATO8                 TO AVSTEMM-IO-AREA (1:8)
               MOVE '040'                  TO AVSTEMM-IO-AREA (9:3)
               MOVE 'REG'                  TO AVSTEMM-IO-AREA (12:3)
               MOVE 'RKO025'               TO AVSTEMM-IO-AREA (15:6)
               MOVE '*RKO025*'             TO AVSTEMM-IO-AREA (21:8)
               MOVE TIDSP-IO               TO AVSTEMM-IO-AREA (30:6)
               MOVE LRRGAN-IO              TO AVSTEMM-IO-AREA (36:13)
               MOVE LRRGBE-IO              TO AVSTEMM-IO-AREA (49:15)
               MOVE LRRGBU-IO              TO AVSTEMM-IO-AREA (64:15)
               MOVE DDATO8                 TO AVSTEMM-IO-AREA (79:8)
               WRITE AVSTEMM-IO-AREA
               MOVE SPACES TO AVSTEMM-IO-AREA
               INITIALIZE AVSTEMM-IO-AREA
               MOVE '********'             TO AVSTEMM-IO-AREA (1:8)
               MOVE '040'                  TO AVSTEMM-IO-AREA (9:3)
               MOVE 'REG'                  TO AVSTEMM-IO-AREA (12:3)
               MOVE '******'               TO AVSTEMM-IO-AREA (15:6)
               MOVE '*RKO025*'             TO AVSTEMM-IO-AREA (21:8)
               MOVE TIDSP-IO               TO AVSTEMM-IO-AREA (30:6)
               MOVE LRRGAN-IO              TO AVSTEMM-IO-AREA (36:13)
               MOVE LRRGBE-IO              TO AVSTEMM-IO-AREA (49:15)
               MOVE LRRGBU-IO              TO AVSTEMM-IO-AREA (64:15)
               MOVE DDATO8                 TO AVSTEMM-IO-AREA (79:8)
               WRITE AVSTEMM-IO-AREA
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'AVSTEMMING PROG. RKO025 ' TO LISTE-IO-AREA (1:24)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-BEFORE-SPACE
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT REC NY REGFILE'   TO LISTE-IO-AREA (3:18)
               MOVE TOTREC                 TO XO-80YNZ
               MOVE XO-80YNZ               TO LISTE-IO-AREA (28:8)
               INITIALIZE TOTREC
               MOVE 'DATO'                 TO LISTE-IO-AREA (46:4)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (52:8)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'BELØP   NY REGFILE'   TO LISTE-IO-AREA (3:18)
               MOVE TOTBEL                 TO EDIT-TOTBEL
               MOVE EDIT-TOTBEL            TO LISTE-IO-AREA (22:15)
               INITIALIZE TOTBEL
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
           INITIALIZE GMLREG-DATA-FIELDS
           SET GMLREG-EOF-OFF              TO TRUE
           SET GMLREG-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO GMLREG-MC
                                              GMLREG-MP
           OPEN INPUT GMLREG
           INITIALIZE DAGREG-DATA-FIELDS
           SET DAGREG-EOF-OFF              TO TRUE
           SET DAGREG-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO DAGREG-MC
                                              DAGREG-MP
           OPEN INPUT DAGREG
           OPEN OUTPUT NYREG
           OPEN OUTPUT AVSTEMM
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE GMLREG
           CLOSE DAGREG
           CLOSE NYREG
           CLOSE AVSTEMM
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
