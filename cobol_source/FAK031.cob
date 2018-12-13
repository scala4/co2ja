       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK031R.
      *                                          XX2000XXIRXXMT       *
      **********************************************  Z-WIN-RPG2   ****
      *****************************************************************
      *  PROGRAM: FAK031    LAGET AV: ESPEN LARSEN 5.2.2005           *
      *  KOPIERING AV FAKTURARECORDS MED FJERNING AV FAKTURERTE.      *
      *  DETTE PROGRAM ERSTATTER FAK025 OG TAR MED GJENNOMFAKT.REC.   *
      *  FRA FAK50AD (FIRMA 608).                                     *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK031.rpg
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
           SELECT INF
               ASSIGN TO UT-S-INF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INF-STATUS.
           SELECT FAKPAR
               ASSIGN TO UT-S-FAKPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKPAR-STATUS.
           SELECT INF2
               ASSIGN TO UT-S-INF2
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INF2-STATUS.
           SELECT OUTF
               ASSIGN TO UT-S-OUTF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTF-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD INF
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  INF-IO-AREA.
           05  INF-IO-AREA-X               PICTURE X(200).
       FD FAKPAR
               BLOCK CONTAINS 200
               RECORD CONTAINS 200.
       01  FAKPAR-IO-AREA.
           05  FAKPAR-IO-AREA-X            PICTURE X(200).
       FD INF2
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  INF2-IO-AREA.
           05  INF2-IO-AREA-X              PICTURE X(200).
       FD OUTF
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  OUTF-IO-AREA.
           05  OUTF-IO-AREA-X              PICTURE X(200).
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
           10  INF-STATUS                  PICTURE 99 VALUE 0.
           10  FAKPAR-STATUS               PICTURE 99 VALUE 0.
           10  INF2-STATUS                 PICTURE 99 VALUE 0.
           10  OUTF-STATUS                 PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  INF-EOF-OFF             VALUE '0'.
               88  INF-EOF                 VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INF-READ-OFF            VALUE '0'.
               88  INF-READ                VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INF-PROCESS-OFF         VALUE '0'.
               88  INF-PROCESS             VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  INF-LEVEL-INIT-OFF      VALUE '0'.
               88  INF-LEVEL-INIT          VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INF2-EOF-OFF            VALUE '0'.
               88  INF2-EOF                VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INF2-READ-OFF           VALUE '0'.
               88  INF2-READ               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INF2-PROCESS-OFF        VALUE '0'.
               88  INF2-PROCESS            VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  INF2-LEVEL-INIT-OFF     VALUE '0'.
               88  INF2-LEVEL-INIT         VALUE '1'.
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
           05  INF-LEVEL-01.
               10  INF-01-L3.
                   15  INF-01-L3-FNR       PICTURE X(3).
               10  INF-01-L2.
                   15  INF-01-L2-ORDNR     PICTURE X(6).
               10  INF-01-L1.
                   15  INF-01-L1-KNR       PICTURE X(6).
           05  INF-DATA-FIELDS.
               10  REC                     PICTURE X(200).
               10  FNR                     PICTURE X(3).
               10  ORDNR                   PICTURE X(6).
               10  KNR                     PICTURE X(6).
               10  FAKODE                  PICTURE X(1).
           05  FAKPAR-DATA-FIELDS.
               10  FOMGNR                  PICTURE X(1).
               10  PFAKP                   PICTURE X(4).
               10  PAAR                    PICTURE X(2).
               10  PMND                    PICTURE X(2).
               10  FAKPER-IO.
                   15  FAKPER              PICTURE S9(6).
      *****************************************************************
      *  FAKTURAPARAMETER RUTINE.                                     *
      *****************************************************************
           05  INF2-LEVEL-02.
               10  INF2-02-L3.
                   15  INF2-02-L3-FNR      PICTURE X(3).
               10  INF2-02-L2.
                   15  INF2-02-L2-ORDNR    PICTURE X(6).
               10  INF2-02-L1.
                   15  INF2-02-L1-KNR      PICTURE X(6).
           05  INF2-DATA-FIELDS.
               10  REC2                    PICTURE X(200).
               10  FAKP                    PICTURE X(4).
      *                                     183 183 FAKOD2
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(6).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  SEQNR-IO.
                   15  SEQNR               PICTURE S9(4).
               10  ANT01I-IO.
                   15  ANT01I              PICTURE S9(6).
               10  ANT01F-IO.
                   15  ANT01F              PICTURE S9(6).
               10  ANT01S-IO.
                   15  ANT01S              PICTURE S9(6).
               10  ANT02I-IO.
                   15  ANT02I              PICTURE S9(6).
               10  ANT02F-IO.
                   15  ANT02F              PICTURE S9(6).
               10  ANT02U-IO.
                   15  ANT02U              PICTURE S9(6).
           05  EDITTING-FIELDS.
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
           SET NOT-I-03                    TO TRUE
           SET NOT-I-02                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  INF-PROCESS
               SET INF-PROCESS-OFF         TO TRUE
               SET INF-READ                TO TRUE
           END-IF
 
           IF  INF-READ
           AND RECORD-SELECTED-OFF
               PERFORM INF-GET
               SET INF-READ-OFF            TO TRUE
               IF  NOT INF-EOF
                   SET INF-PROCESS         TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  INF2-PROCESS
               SET INF2-PROCESS-OFF        TO TRUE
               SET INF2-READ               TO TRUE
           END-IF
 
           IF  INF2-READ
           AND RECORD-SELECTED-OFF
               PERFORM INF2-GET
               SET INF2-READ-OFF           TO TRUE
               IF  NOT INF2-EOF
                   SET INF2-PROCESS        TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  INF-PROCESS
               PERFORM INF-IDSET
           END-IF
 
           IF  INF2-PROCESS
               PERFORM INF2-IDSET
           END-IF
 
           IF  INF-PROCESS
               PERFORM INF-CHK-LEVEL
           END-IF
 
           IF  INF2-PROCESS
               PERFORM INF2-CHK-LEVEL
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
 
           IF  INF-PROCESS
               PERFORM INF-FLDSET
           END-IF
 
           IF  INF2-PROCESS
               PERFORM INF2-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  INF-PROCESS
           OR  INF2-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-10)
               GO TO ENDPAR-T
           END-IF
           READ FAKPAR
           AT END
               SET I-99                    TO TRUE
           NOT AT END
               SET NOT-I-99                TO TRUE
               PERFORM FAKPAR-FLDSET
               PERFORM FAKPAR-IDSET
           END-READ
           SET I-10                        TO TRUE.
 
       ENDPAR-T.
           IF  (NOT-I-01 AND NOT-I-02)
               GO TO SLUTT-T
      *****************************************************************
      * RUTINE FOR ON-LINE.FAKT.FILE.                                 *
      *****************************************************************
           END-IF
           IF  (I-L1)
               SUBTRACT SEQNR              FROM SEQNR
           END-IF
           IF  (I-01)
               SET NOT-I-12                TO TRUE
               IF  FAKODE = 'F'
                   SET I-12                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-12)
               ADD 1                       TO SEQNR
           END-IF
           IF  (I-01)
               ADD 1                       TO ANT01I
           END-IF
           IF  (I-01 AND NOT-I-12)
               ADD 1                       TO ANT01F
           END-IF
           IF  (I-01 AND I-12)
               ADD 1                       TO ANT01S
           END-IF
           IF  (I-01)
               GO TO SLUTT-T
      *****************************************************************
      * RUTINE FOR FAKT.FRA.KONTANT.SAMLE.MASTER                      *
      *****************************************************************
           END-IF
           IF  (I-02)
               SET NOT-I-22                TO TRUE
               IF  FAKP > PFAKP
                   SET I-22                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-22)
               ADD 1                       TO SEQNR
           END-IF
           IF  (I-02)
               ADD 1                       TO ANT02I
           END-IF
           IF  (I-02 AND NOT-I-22)
               ADD 1                       TO ANT02F
           END-IF
           IF  (I-02 AND I-22)
               ADD 1                       TO ANT02U
           END-IF.
 
       SLUTT-T.
           CONTINUE.
 
       INF-GET SECTION.
       INF-GET-P.
           IF  INF-EOF-OFF
               READ INF
               AT END
                   SET INF-EOF             TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INF-FLDSET SECTION.
       INF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INF-IO-AREA (1:200)    TO REC (1:200)
               MOVE INF-IO-AREA (1:3)      TO FNR (1:3)
               MOVE INF-IO-AREA (19:6)     TO ORDNR (1:6)
               MOVE INF-IO-AREA (184:6)    TO KNR (1:6)
               MOVE INF-IO-AREA (183:1)    TO FAKODE (1:1)
           END-EVALUATE.
 
       INF-IDSET SECTION.
       INF-IDSET-P.
           SET I-01                        TO TRUE.
 
       INF-CHK-LEVEL SECTION.
       INF-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO INF-LEVEL-01
               MOVE INF-IO-AREA (1:3)      TO INF-01-L3-FNR
               MOVE INF-IO-AREA (19:6)     TO INF-01-L2-ORDNR
               MOVE INF-IO-AREA (184:6)    TO INF-01-L1-KNR
               IF  INF-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INF-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  INF-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INF-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INF-01-L3             TO THE-PRIOR-L3
               MOVE  INF-01-L2             TO THE-PRIOR-L2
               MOVE  INF-01-L1             TO THE-PRIOR-L1
               SET INF-LEVEL-INIT          TO TRUE
           END-EVALUATE.
 
       FAKPAR-FLDSET SECTION.
       FAKPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKPAR-IO-AREA (3:1)   TO FOMGNR (1:1)
               MOVE FAKPAR-IO-AREA (6:4)   TO PFAKP (1:4)
               MOVE FAKPAR-IO-AREA (6:2)   TO PAAR (1:2)
               MOVE FAKPAR-IO-AREA (8:2)   TO PMND (1:2)
               MOVE FAKPAR-IO-AREA (12:6)  TO FAKPER-IO
               INSPECT FAKPER-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       FAKPAR-IDSET SECTION.
       FAKPAR-IDSET-P.
           SET I-03                        TO TRUE.
 
       INF2-GET SECTION.
       INF2-GET-P.
           IF  INF2-EOF-OFF
               READ INF2
               AT END
                   SET INF2-EOF            TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INF2-FLDSET SECTION.
       INF2-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INF2-IO-AREA (1:200)   TO REC2 (1:200)
               MOVE INF2-IO-AREA (1:3)     TO FNR (1:3)
               MOVE INF2-IO-AREA (19:6)    TO ORDNR (1:6)
               MOVE INF2-IO-AREA (167:4)   TO FAKP (1:4)
               MOVE INF2-IO-AREA (184:6)   TO KNR (1:6)
           END-EVALUATE.
 
       INF2-IDSET SECTION.
       INF2-IDSET-P.
           SET I-02                        TO TRUE.
 
       INF2-CHK-LEVEL SECTION.
       INF2-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO INF2-LEVEL-02
               MOVE INF2-IO-AREA (1:3)     TO INF2-02-L3-FNR
               MOVE INF2-IO-AREA (19:6)    TO INF2-02-L2-ORDNR
               MOVE INF2-IO-AREA (184:6)   TO INF2-02-L1-KNR
               IF  INF2-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INF2-02-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  INF2-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INF2-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INF2-02-L3            TO THE-PRIOR-L3
               MOVE  INF2-02-L2            TO THE-PRIOR-L2
               MOVE  INF2-02-L1            TO THE-PRIOR-L1
               SET INF2-LEVEL-INIT         TO TRUE
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
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND NOT-I-12)
               MOVE SPACES TO OUTF-IO-AREA
               INITIALIZE OUTF-IO-AREA
               MOVE REC                    TO OUTF-IO-AREA (1:200)
               MOVE SEQNR-IO               TO OUTF-IO-AREA (26:4)
               WRITE OUTF-IO-AREA
           END-IF
           IF  (I-02 AND NOT-I-22)
               MOVE SPACES TO OUTF-IO-AREA
               INITIALIZE OUTF-IO-AREA
               MOVE REC2                   TO OUTF-IO-AREA (1:200)
               MOVE SEQNR-IO               TO OUTF-IO-AREA (26:4)
               WRITE OUTF-IO-AREA
           END-IF
           IF  (I-02 AND NOT-I-22)
               MOVE 'F'                    TO INF2-IO-AREA (183:1)
               REWRITE INF2-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE '************************' TO LISTE-IO-AREA (49:24)
               MOVE '************************' TO LISTE-IO-AREA (73:24)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '***   AVSTEMNINGSTOTALER' TO LISTE-IO-AREA (1:24)
               MOVE '   --- FAK031 ---    ***' TO LISTE-IO-AREA (25:24)
               MOVE 'PR'                   TO LISTE-IO-AREA (51:2)
               MOVE PAAR                   TO LISTE-IO-AREA (54:2)
               MOVE PMND                   TO LISTE-IO-AREA (56:2)
               MOVE '-'                    TO LISTE-IO-AREA (58:1)
               MOVE FOMGNR                 TO LISTE-IO-AREA (59:1)
               MOVE FAKPER                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (62:8)
               MOVE 'FREMSTILT'            TO LISTE-IO-AREA (73:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (83:8)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE '************************' TO LISTE-IO-AREA (49:24)
               MOVE '************************' TO LISTE-IO-AREA (73:24)
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANT01I                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (4:7)
               MOVE 'FAKT.REC.LEST.         ' TO LISTE-IO-AREA (12:23)
               MOVE 2                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANT01F                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (4:7)
               MOVE 'FAKT.REC. TIL FAKT.DENN' TO LISTE-IO-AREA (12:23)
               MOVE 'E FAKTURA OMG.         ' TO LISTE-IO-AREA (35:23)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANT01S                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (4:7)
               MOVE 'FAKT.REC. FAKT. FOREGÅE' TO LISTE-IO-AREA (12:23)
               MOVE 'NDE FAKTURA OMGANG.    ' TO LISTE-IO-AREA (35:23)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANT02I                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (4:7)
               MOVE 'FAKT.GJENNOMFAKT.REC. L' TO LISTE-IO-AREA (12:23)
               MOVE 'EST.                   ' TO LISTE-IO-AREA (35:23)
               MOVE 2                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANT02F                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (4:7)
               MOVE 'FAKT.GJENNOMFAKT.REC. T' TO LISTE-IO-AREA (12:23)
               MOVE 'IL FAKT.DENNE FAKT.OMG.' TO LISTE-IO-AREA (35:23)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANT02U                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (4:7)
               MOVE 'FAKT.GJENNOMFAKT.REC. T' TO LISTE-IO-AREA (12:23)
               MOVE 'IL NESTE FAKT.OMG.     ' TO LISTE-IO-AREA (35:23)
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
           SET INF-LEVEL-INIT              TO TRUE
           INITIALIZE INF-DATA-FIELDS
           SET INF-EOF-OFF                 TO TRUE
           SET INF-PROCESS                 TO TRUE
           OPEN INPUT INF
           INITIALIZE FAKPAR-DATA-FIELDS
           OPEN INPUT FAKPAR
           SET INF2-LEVEL-INIT             TO TRUE
           INITIALIZE INF2-DATA-FIELDS
           SET INF2-EOF-OFF                TO TRUE
           SET INF2-PROCESS                TO TRUE
           OPEN I-O INF2
           OPEN OUTPUT OUTF
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INF
           CLOSE FAKPAR
           CLOSE INF2
           CLOSE OUTF
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
