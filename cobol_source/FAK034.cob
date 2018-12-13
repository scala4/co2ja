       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK034R.
      ******************************************* :   Z-WIN-RPG2     **
      *  PROGRAM FAK034                                               *
      * PRINTER UT AVSTEMMINGSLISTE FØR FAKTURERING.                  *
      * BRUDD OGSÅ PÅ KUNDENR. DA TELLING AV RENTE/GEBYR ORDRE BLIR   *
      * FEIL, DA DISSE HAR SAMME ORDRENUMMER.                         *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK034.rpg
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
           SELECT PARAM
               ASSIGN TO UT-S-PARAM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PARAM-STATUS.
           SELECT FAKREC
               ASSIGN TO UT-S-FAKREC
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKREC-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD PARAM
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  PARAM-IO-AREA.
           05  PARAM-IO-AREA-X             PICTURE X(80).
       FD FAKREC
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  FAKREC-IO-AREA.
           05  FAKREC-IO-AREA-X            PICTURE X(200).
       FD LISTE
               BLOCK CONTAINS 91
               RECORD CONTAINS 91.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(90).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  PARAM-STATUS                PICTURE 99 VALUE 0.
           10  FAKREC-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-EOF-OFF           VALUE '0'.
               88  PARAM-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-READ-OFF          VALUE '0'.
               88  PARAM-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-PROCESS-OFF       VALUE '0'.
               88  PARAM-PROCESS           VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKREC-EOF-OFF          VALUE '0'.
               88  FAKREC-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKREC-READ-OFF         VALUE '0'.
               88  FAKREC-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKREC-PROCESS-OFF      VALUE '0'.
               88  FAKREC-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  FAKREC-LEVEL-INIT-OFF   VALUE '0'.
               88  FAKREC-LEVEL-INIT       VALUE '1'.
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
           05  PARAM-DATA-FIELDS.
               10  PFIRMA                  PICTURE X(3).
               10  OTOTAL                  PICTURE X(1).
               10  RENTER                  PICTURE X(1).
           05  FAKREC-LEVEL-01.
               10  FAKREC-01-L2.
                   15  FAKREC-01-L2-FIRMNR PICTURE X(3).
               10  FAKREC-01-L1.
                   15  FAKREC-01-L1-KUNDNR PICTURE X(6).
                   15  FAKREC-01-L1-ORDRNR PICTURE X(6).
           05  FAKREC-DATA-FIELDS.
               10  FIRMNR                  PICTURE X(3).
               10  KUNDNR                  PICTURE X(6).
               10  FAKTYP                  PICTURE X(1).
               10  ORDRNR                  PICTURE X(6).
               10  RECART                  PICTURE X(1).
               10  ANTLEV-IO.
                   15  ANTLEV              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  EDBNR2                  PICTURE X(2).
               10  EDBNR3                  PICTURE X(3).
               10  RAB1-IO.
                   15  RAB1                PICTURE S9(2)V9(1).
               10  RAB2-IO.
                   15  RAB2                PICTURE S9(2)V9(1).
               10  RAB3-IO.
                   15  RAB3                PICTURE S9(2)V9(1).
               10  PRIS-IO.
                   15  PRIS                PICTURE S9(7)V9(2).
               10  RENGEB                  PICTURE X(1).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(12).
           05  TEMPORARY-FIELDS.
               10  ANT-IO.
                   15  ANT                 PICTURE S9(5).
               10  ANTFAK-IO.
                   15  ANTFAK              PICTURE S9(5).
               10  ANTKRE-IO.
                   15  ANTKRE              PICTURE S9(5).
               10  ANTVAD-IO.
                   15  ANTVAD              PICTURE S9(5).
               10  ORDSUM-IO.
                   15  ORDSUM              PICTURE S9(7)V9(2).
               10  LINSUM-IO.
                   15  LINSUM              PICTURE S9(7)V9(2).
               10  SUM-X-IO.
                   15  SUM-X               PICTURE S9(10)V9(2).
               10  FIRSUM-IO.
                   15  FIRSUM              PICTURE S9(8)V9(2).
               10  ANT10F-IO.
                   15  ANT10F              PICTURE S9(5).
               10  ANT11F-IO.
                   15  ANT11F              PICTURE S9(5).
               10  ANT12F-IO.
                   15  ANT12F              PICTURE S9(5).
               10  ANTF-IO.
                   15  ANTF                PICTURE S9(5).
               10  ANTOF-IO.
                   15  ANTOF               PICTURE S9(5).
               10  TOTSUM-IO.
                   15  TOTSUM              PICTURE S9(8)V9(2).
               10  ANT10T-IO.
                   15  ANT10T              PICTURE S9(6).
               10  ANT11T-IO.
                   15  ANT11T              PICTURE S9(6).
               10  ANT12T-IO.
                   15  ANT12T              PICTURE S9(6).
               10  ANTT-IO.
                   15  ANTT                PICTURE S9(6).
               10  ANTOT-IO.
                   15  ANTOT               PICTURE S9(5).
           05  EDITTING-FIELDS.
               10  XO-50YY9                PICTURE ZZ.ZZ9.
               10  XO-72YY9R               PICTURE Z.ZZZ.ZZZ,99-.
               10  XO-82YY9R               PICTURE ZZ.ZZZ.ZZZ,99-.
               10  XO-60YY9                PICTURE ZZZ.ZZ9.
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
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
           PERFORM HEADING-OUTPUT
           IF  NOT-SET-I-OF
               SET NOT-I-OF                TO TRUE
           END-IF
           SET NOT-SET-I-OF                TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-02                    TO TRUE
           SET NOT-I-01                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  PARAM-PROCESS
               SET PARAM-PROCESS-OFF       TO TRUE
               SET PARAM-READ              TO TRUE
           END-IF
 
           IF  PARAM-READ
           AND RECORD-SELECTED-OFF
               PERFORM PARAM-GET
               SET PARAM-READ-OFF          TO TRUE
               IF  NOT PARAM-EOF
                   SET PARAM-PROCESS       TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  FAKREC-PROCESS
               SET FAKREC-PROCESS-OFF      TO TRUE
               SET FAKREC-READ             TO TRUE
           END-IF
 
           IF  FAKREC-READ
           AND RECORD-SELECTED-OFF
               PERFORM FAKREC-GET
               SET FAKREC-READ-OFF         TO TRUE
               IF  NOT FAKREC-EOF
                   SET FAKREC-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  PARAM-PROCESS
               PERFORM PARAM-IDSET
           END-IF
 
           IF  FAKREC-PROCESS
               PERFORM FAKREC-IDSET
           END-IF
 
           IF  FAKREC-PROCESS
               PERFORM FAKREC-CHK-LEVEL
           END-IF
 
           IF I-1ST
               GO TO LR-INDICATOR-TEST
           END-IF.
 
       MAINLINE-TOTAL-CALCS.
           PERFORM TOTAL-CALCS
           PERFORM TOTAL-OUTPUT.
 
       LR-INDICATOR-TEST.
           IF  I-LR
               GO TO MAINLINE-TERMINATION
           END-IF
           PERFORM HEADING-OVERFLOW
 
           IF  PARAM-PROCESS
               PERFORM PARAM-FLDSET
           END-IF
 
           IF  FAKREC-PROCESS
               PERFORM FAKREC-FLDOFF
               PERFORM FAKREC-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  FAKREC-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-02)
               SET NOT-I-20                TO TRUE
               IF  PFIRMA = 'ALT'
                   SET I-20                TO TRUE
               END-IF
               SET NOT-I-21                TO TRUE
               IF  OTOTAL = 'J'
                   SET I-21                TO TRUE
               END-IF
               SET NOT-I-22                TO TRUE
               IF  RENTER = 'J'
                   SET I-22                TO TRUE
               END-IF
               GO TO SLUTT-T
           END-IF
           SET NOT-I-35                    TO TRUE
           IF  (NOT-I-20)
               SET NOT-I-LR                TO TRUE
               SET NOT-I-34                TO TRUE
               IF  FIRMNR > PFIRMA
                   SET I-LR                TO TRUE
               END-IF
               IF  FIRMNR = PFIRMA
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-20 AND NOT-I-34)
               SET I-35                    TO TRUE
           END-IF
           SET NOT-I-33                    TO TRUE
           IF  RENGEB = 'R'
               SET I-33                    TO TRUE
           END-IF
           IF  (NOT-I-22 AND I-33)
               SET I-35                    TO TRUE
           END-IF
           IF  (I-L1)
               SUBTRACT ANT                FROM ANT
               SUBTRACT ANTFAK             FROM ANTFAK
               SUBTRACT ANTKRE             FROM ANTKRE
               SUBTRACT ANTVAD             FROM ANTVAD
               SUBTRACT ORDSUM             FROM ORDSUM
           END-IF
           IF  (I-35)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-10                    TO TRUE
           IF  FAKTYP = 'F'
               SET I-10                    TO TRUE
           END-IF
           SET NOT-I-11                    TO TRUE
           IF  FAKTYP = 'K'
               SET I-11                    TO TRUE
           END-IF
           SET NOT-I-12                    TO TRUE
           IF  RECART = 'A'
               SET I-12                    TO TRUE
           END-IF
           ADD 1                           TO ANT
           IF  (I-12)
               ADD 1                       TO ANTVAD
               GO TO SLUTT-T
           END-IF
           IF  (I-10)
               ADD 1                       TO ANTFAK
           END-IF
           IF  (I-11)
               ADD 1                       TO ANTKRE
      *******   UTREGNING A ORDRESUM   ************
           END-IF
           SET NOT-I-94                    TO TRUE
           IF  EDBNR2 = '94'
               SET I-94                    TO TRUE
           END-IF
           IF  (NOT-I-94)
               SET NOT-I-94                TO TRUE
               IF  EDBNR3 = '995'
                   SET I-94                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-09)
               MULTIPLY ANTLEV BY PRIS GIVING LINSUM ROUNDED
           END-IF
           IF  (I-09)
               ADD PRIS TO ZERO        GIVING LINSUM
           END-IF
           IF  (I-94)
               MULTIPLY -1 BY LINSUM   GIVING LINSUM
           END-IF
           MULTIPLY RAB1 BY LINSUM     GIVING SUM-X ROUNDED
           DIVIDE SUM-X BY 100         GIVING SUM-X ROUNDED
           SUBTRACT SUM-X                  FROM LINSUM
           MULTIPLY RAB2 BY LINSUM     GIVING SUM-X ROUNDED
           DIVIDE SUM-X BY 100         GIVING SUM-X ROUNDED
           SUBTRACT SUM-X                  FROM LINSUM
           MULTIPLY RAB3 BY LINSUM     GIVING SUM-X ROUNDED
           DIVIDE SUM-X BY 100         GIVING SUM-X ROUNDED
           SUBTRACT SUM-X                  FROM LINSUM
           IF  (I-10)
               ADD LINSUM                  TO ORDSUM
           END-IF
           IF  (I-11)
               SUBTRACT LINSUM             FROM ORDSUM
           END-IF.
 
       SLUTT-T.
      *********************************************
           CONTINUE.
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1 AND NOT-I-35)
               ADD ORDSUM                  TO FIRSUM
               ADD ANTFAK                  TO ANT10F
               ADD ANTKRE                  TO ANT11F
               ADD ANTVAD                  TO ANT12F
               ADD ANT                     TO ANTF
               ADD 1                       TO ANTOF
           END-IF
           IF  (I-L2)
               ADD FIRSUM                  TO TOTSUM
               ADD ANT10F                  TO ANT10T
               ADD ANT11F                  TO ANT11T
               ADD ANT12F                  TO ANT12T
               ADD ANTF                    TO ANTT
               ADD ANTOF                   TO ANTOT
           END-IF.
 
       PARAM-GET SECTION.
       PARAM-GET-P.
           IF  PARAM-EOF-OFF
               READ PARAM
               AT END
                   SET PARAM-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       PARAM-FLDSET SECTION.
       PARAM-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE PARAM-IO-AREA (13:3)   TO PFIRMA (1:3)
               MOVE PARAM-IO-AREA (30:1)   TO OTOTAL (1:1)
               MOVE PARAM-IO-AREA (46:1)   TO RENTER (1:1)
           END-EVALUATE.
 
       PARAM-IDSET SECTION.
       PARAM-IDSET-P.
           SET I-02                        TO TRUE.
 
       FAKREC-GET SECTION.
       FAKREC-GET-P.
           IF  FAKREC-EOF-OFF
               READ FAKREC
               AT END
                   SET FAKREC-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FAKREC-FLDOFF SECTION.
       FAKREC-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-09                TO TRUE
           END-EVALUATE.
 
       FAKREC-FLDSET SECTION.
       FAKREC-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKREC-IO-AREA (1:3)   TO FIRMNR (1:3)
               MOVE FAKREC-IO-AREA (4:6)   TO KUNDNR (1:6)
               MOVE FAKREC-IO-AREA (10:1)  TO FAKTYP (1:1)
               MOVE FAKREC-IO-AREA (19:6)  TO ORDRNR (1:6)
               MOVE FAKREC-IO-AREA (25:1)  TO RECART (1:1)
               MOVE FAKREC-IO-AREA (137:4) TO ANTLEV-IO
               IF  ANTLEV = ZERO
                   SET I-09                TO TRUE
               END-IF
               MOVE FAKREC-IO-AREA (141:2) TO EDBNR2 (1:2)
               MOVE FAKREC-IO-AREA (141:3) TO EDBNR3 (1:3)
               MOVE FAKREC-IO-AREA (148:3) TO RAB1-IO
               INSPECT RAB1-IO REPLACING ALL ' ' BY '0'
               MOVE FAKREC-IO-AREA (151:3) TO RAB2-IO
               INSPECT RAB2-IO REPLACING ALL ' ' BY '0'
               MOVE FAKREC-IO-AREA (154:3) TO RAB3-IO
               INSPECT RAB3-IO REPLACING ALL ' ' BY '0'
               MOVE FAKREC-IO-AREA (157:9) TO PRIS-IO
               INSPECT PRIS-IO REPLACING ALL ' ' BY '0'
               MOVE FAKREC-IO-AREA (177:1) TO RENGEB (1:1)
           END-EVALUATE.
 
       FAKREC-IDSET SECTION.
       FAKREC-IDSET-P.
           SET I-01                        TO TRUE.
 
       FAKREC-CHK-LEVEL SECTION.
       FAKREC-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO FAKREC-LEVEL-01
               MOVE FAKREC-IO-AREA (1:3)   TO FAKREC-01-L2-FIRMNR
               MOVE FAKREC-IO-AREA (4:6)   TO FAKREC-01-L1-KUNDNR
               MOVE FAKREC-IO-AREA (19:6)  TO FAKREC-01-L1-ORDRNR
               IF  FAKREC-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  FAKREC-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  FAKREC-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  FAKREC-01-L2          TO THE-PRIOR-L2
               MOVE  FAKREC-01-L1          TO THE-PRIOR-L1
               SET FAKREC-LEVEL-INIT       TO TRUE
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
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '***   AVSTEMNINGSTOTALER' TO LISTE-IO-AREA (1:24)
               MOVE '   --- FAK034 ---    ***' TO LISTE-IO-AREA (25:24)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (53:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-BEFORE-SPACE
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMA'                TO LISTE-IO-AREA (1:5)
               MOVE 'ORDRE'                TO LISTE-IO-AREA (8:5)
               MOVE 'ANTALL'               TO LISTE-IO-AREA (20:6)
               MOVE 'ANTALL'               TO LISTE-IO-AREA (30:6)
               MOVE 'ANTALL'               TO LISTE-IO-AREA (40:6)
               MOVE 'ANTALL'               TO LISTE-IO-AREA (50:6)
               MOVE 'ORDRE '               TO LISTE-IO-AREA (65:6)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'NR.'                  TO LISTE-IO-AREA (3:3)
               MOVE 'NR.'                  TO LISTE-IO-AREA (10:3)
               MOVE 'REC.'                 TO LISTE-IO-AREA (22:4)
               MOVE 'FAKT.'                TO LISTE-IO-AREA (31:5)
               MOVE 'KR.NOT'               TO LISTE-IO-AREA (40:6)
               MOVE 'V.ADR.'               TO LISTE-IO-AREA (50:6)
               MOVE 'SUM'                  TO LISTE-IO-AREA (68:3)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '***   AVSTEMNINGSTOTALER' TO LISTE-IO-AREA (1:24)
               MOVE '   --- FAK034 ---    ***' TO LISTE-IO-AREA (25:24)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (53:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-BEFORE-SPACE
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMA'                TO LISTE-IO-AREA (1:5)
               MOVE 'ORDRE'                TO LISTE-IO-AREA (8:5)
               MOVE 'ANTALL'               TO LISTE-IO-AREA (20:6)
               MOVE 'ANTALL'               TO LISTE-IO-AREA (30:6)
               MOVE 'ANTALL'               TO LISTE-IO-AREA (40:6)
               MOVE 'ANTALL'               TO LISTE-IO-AREA (50:6)
               MOVE 'ORDRE '               TO LISTE-IO-AREA (65:6)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'NR.'                  TO LISTE-IO-AREA (3:3)
               MOVE 'NR.'                  TO LISTE-IO-AREA (10:3)
               MOVE 'REC.'                 TO LISTE-IO-AREA (22:4)
               MOVE 'FAKT.'                TO LISTE-IO-AREA (31:5)
               MOVE 'KR.NOT'               TO LISTE-IO-AREA (40:6)
               MOVE 'V.ADR.'               TO LISTE-IO-AREA (50:6)
               MOVE 'SUM'                  TO LISTE-IO-AREA (68:3)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-21 AND NOT-I-35)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FIRMNR                 TO LISTE-IO-AREA (3:3)
               MOVE ORDRNR                 TO LISTE-IO-AREA (7:6)
               MOVE ANT                    TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (20:6)
               MOVE ANTFAK                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (30:6)
               MOVE ANTKRE                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (40:6)
               MOVE ANTVAD                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (50:6)
               MOVE ORDSUM                 TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (58:13)
               MOVE '*'                    TO LISTE-IO-AREA (72:1)
               MOVE FIRSUM                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (77:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L2)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FIRMNR                 TO LISTE-IO-AREA (3:3)
               MOVE ANTOF                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (7:6)
               INITIALIZE ANTOF
               MOVE ANTF                   TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (20:6)
               INITIALIZE ANTF
               MOVE ANT10F                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (30:6)
               INITIALIZE ANT10F
               MOVE ANT11F                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (40:6)
               INITIALIZE ANT11F
               MOVE ANT12F                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (50:6)
               INITIALIZE ANT12F
               MOVE FIRSUM                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (57:14)
               INITIALIZE FIRSUM
               MOVE '**'                   TO LISTE-IO-AREA (72:2)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '********'             TO LISTE-IO-AREA (18:8)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTAL'                TO LISTE-IO-AREA (1:5)
               MOVE ANTOT                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (7:6)
               INITIALIZE ANTOT
               MOVE ANTT                   TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (19:7)
               INITIALIZE ANTT
               MOVE ANT10T                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (29:7)
               INITIALIZE ANT10T
               MOVE ANT11T                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (39:7)
               INITIALIZE ANT11T
               MOVE ANT12T                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (49:7)
               INITIALIZE ANT12T
               MOVE TOTSUM                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (57:14)
               INITIALIZE TOTSUM
               MOVE '***'                  TO LISTE-IO-AREA (72:3)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '********'             TO LISTE-IO-AREA (18:8)
               MOVE 1                      TO LISTE-BEFORE-SPACE
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
           INITIALIZE PARAM-DATA-FIELDS
           SET PARAM-EOF-OFF               TO TRUE
           SET PARAM-PROCESS               TO TRUE
           OPEN INPUT PARAM
           SET FAKREC-LEVEL-INIT           TO TRUE
           INITIALIZE FAKREC-DATA-FIELDS
           SET FAKREC-EOF-OFF              TO TRUE
           SET FAKREC-PROCESS              TO TRUE
           OPEN INPUT FAKREC
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PARAM
           CLOSE FAKREC
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
