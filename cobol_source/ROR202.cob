       IDENTIFICATION DIVISION.
       PROGRAM-ID. ROR202R.
      **********************************************  Z-WIN-RPG2   ****
      *  ROR202   PROGRAMMERT AV ESPEN LARSEN 30.08.95     *
      *  ---------------------------------------------     *
      *  DANNER  REST.ORDRE.MASTER MED BRUDD.NR.           *
      ******************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ROR202.rpg
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
           SELECT RESTMAS
               ASSIGN TO RESTMAS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS RESTMAS-STATUS
               RECORD KEY IS RESTMAS-KEY1.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD INF
               BLOCK CONTAINS 320
               RECORD CONTAINS 160.
       01  INF-IO-AREA.
           05  INF-IO-AREA-X               PICTURE X(160).
       FD RESTMAS
               RECORD CONTAINS 160.
       01  RESTMAS-IO-AREA.
           05  RESTMAS-IO-AREA-X.
               10  RESTMAS-KEY1.
                   15  RESTMAS-KEY1N       PICTURE S9(17).
               10  FILLER                  PICTURE X(143).
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
           10  RESTMAS-STATUS              PICTURE 99 VALUE 0.
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
           05  RESTMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
               10  INF-01-L5.
                   15  INF-01-L5-FIRM      PICTURE X(3).
               10  INF-01-L4.
                   15  INF-01-L4-AVD       PICTURE X(1).
               10  INF-01-L3.
                   15  INF-01-L3-RECTYP    PICTURE X(1).
               10  INF-01-L2.
                   15  INF-01-L2-RKNR      PICTURE X(6).
               10  INF-01-L1.
                   15  INF-01-L1-BRUDD     PICTURE X(11).
           05  INF-DATA-FIELDS.
               10  INREC                   PICTURE X(160).
               10  FIRM                    PICTURE X(3).
               10  AVD                     PICTURE X(1).
               10  RECTYP                  PICTURE X(1).
               10  RKNR                    PICTURE X(6).
               10  BRUDD                   PICTURE X(11).
               10  STATUS-X                PICTURE X(1).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L5            PICTURE X(3).
               10  THE-PRIOR-L4            PICTURE X(1).
               10  THE-PRIOR-L3            PICTURE X(1).
               10  THE-PRIOR-L2            PICTURE X(6).
               10  THE-PRIOR-L1            PICTURE X(11).
           05  TEMPORARY-FIELDS.
               10  NULL3-IO.
                   15  NULL3               PICTURE S9(3).
               10  BRUDNR-IO.
                   15  BRUDNR              PICTURE S9(3).
               10  SEQNR-IO.
                   15  SEQNR               PICTURE S9(3).
               10  SEQNR2-IO.
                   15  SEQNR2              PICTURE S9(4).
               10  ANTORD-IO.
                   15  ANTORD              PICTURE S9(5).
               10  ANTREC-IO.
                   15  ANTREC              PICTURE S9(5).
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
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
 
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
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  INF-PROCESS
               PERFORM INF-IDSET
           END-IF
 
           IF  INF-PROCESS
               PERFORM INF-CHK-LEVEL
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
 
           PERFORM DETAIL-CALCS
           IF  INF-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-L4)
               MOVE 0                      TO NULL3
           END-IF
           IF  (I-L2)
               SUBTRACT BRUDNR             FROM BRUDNR
           END-IF
           IF  (I-L1)
               SUBTRACT SEQNR              FROM SEQNR
               SUBTRACT SEQNR2             FROM SEQNR2
      *****************************************************************
      *  RUTINE FOR NY REST.ORDRE.MASTER                              *
      *****************************************************************
           END-IF
           IF  (I-L1)
               ADD 1                       TO BRUDNR
           END-IF
           ADD 1                           TO SEQNR2
           SET NOT-I-99                    TO TRUE
           IF  SEQNR2 > 999
               SET I-99                    TO TRUE
           END-IF
           IF  (I-99)
               ADD 1                       TO BRUDNR
               SUBTRACT SEQNR              FROM SEQNR
               SUBTRACT SEQNR2             FROM SEQNR2
               SET NOT-I-99                TO TRUE
           END-IF
           ADD 1                           TO SEQNR
           IF  (I-L1)
               ADD 1                       TO ANTORD
           END-IF
           ADD 1                           TO ANTREC
      *****************************************************************
      * MERKE REKORD SOM ER LEVERINGSKLARE, FOR Å DANNE SØKEFILE.     *
      *       TILBUDS-ORDRE LEGGES ALTID UT SOM LEVERINGSKLAR.        *
      *       ORDREBEKREFT. LEGGES ALTID UT SOM LEVERINGSKLAR.        *
      *****************************************************************
           SET NOT-I-50                    TO TRUE
           IF  STATUS-X = 'F'
               SET I-50                    TO TRUE
           END-IF
           IF  (NOT-I-50)
               SET NOT-I-50                TO TRUE
               IF  STATUS-X = 'B'
                   SET I-50                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-50)
               SET NOT-I-50                TO TRUE
               IF  RECTYP = 'T'
                   SET I-50                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-50)
               SET NOT-I-50                TO TRUE
               IF  RECTYP = 'O'
                   SET I-50                TO TRUE
               END-IF
           END-IF.
 
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
               MOVE INF-IO-AREA (1:160)    TO INREC (1:160)
               MOVE INF-IO-AREA (1:3)      TO FIRM (1:3)
               MOVE INF-IO-AREA (4:1)      TO AVD (1:1)
               MOVE INF-IO-AREA (5:1)      TO RECTYP (1:1)
               MOVE INF-IO-AREA (6:6)      TO RKNR (1:6)
               MOVE INF-IO-AREA (24:11)    TO BRUDD (1:11)
               MOVE INF-IO-AREA (156:1)    TO STATUS-X (1:1)
           END-EVALUATE.
 
       INF-IDSET SECTION.
       INF-IDSET-P.
           SET I-01                        TO TRUE.
 
       INF-CHK-LEVEL SECTION.
       INF-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO INF-LEVEL-01
               MOVE INF-IO-AREA (1:3)      TO INF-01-L5-FIRM
               MOVE INF-IO-AREA (4:1)      TO INF-01-L4-AVD
               MOVE INF-IO-AREA (5:1)      TO INF-01-L3-RECTYP
               MOVE INF-IO-AREA (6:6)      TO INF-01-L2-RKNR
               MOVE INF-IO-AREA (24:11)    TO INF-01-L1-BRUDD
               IF  INF-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INF-01-L5 NOT = THE-PRIOR-L5
                       PERFORM SETON-I-L5
                   WHEN  INF-01-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   WHEN  INF-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  INF-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INF-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INF-01-L5             TO THE-PRIOR-L5
               MOVE  INF-01-L4             TO THE-PRIOR-L4
               MOVE  INF-01-L3             TO THE-PRIOR-L3
               MOVE  INF-01-L2             TO THE-PRIOR-L2
               MOVE  INF-01-L1             TO THE-PRIOR-L1
               SET INF-LEVEL-INIT          TO TRUE
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
           IF  (I-01)
               MOVE SPACES TO RESTMAS-IO-AREA
               INITIALIZE RESTMAS-IO-AREA
               MOVE INREC                  TO RESTMAS-IO-AREA (1:160)
               MOVE BRUDNR-IO              TO RESTMAS-IO-AREA (12:3)
               MOVE SEQNR-IO               TO RESTMAS-IO-AREA (15:3)
               MOVE NULL3-IO               TO RESTMAS-IO-AREA (18:3)
               MOVE NULL3-IO               TO RESTMAS-IO-AREA (21:3)
               MOVE ' '                    TO RESTMAS-IO-AREA (156:1)
               IF  (I-50)
                   MOVE 'L'                TO RESTMAS-IO-AREA (157:1)
               END-IF
               MOVE '  '                   TO RESTMAS-IO-AREA (159:2)
               WRITE RESTMAS-IO-AREA
           END-IF
           IF  (I-01 AND I-99)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FIRM                   TO LISTE-IO-AREA (1:3)
               MOVE AVD                    TO LISTE-IO-AREA (4:1)
               MOVE RECTYP                 TO LISTE-IO-AREA (5:1)
               MOVE RKNR                   TO LISTE-IO-AREA (6:6)
               MOVE BRUDNR-IO              TO LISTE-IO-AREA (12:3)
               MOVE SEQNR2-IO              TO LISTE-IO-AREA (14:4)
               MOVE 'SEQ.NR STØRRE ENN 999   ' TO LISTE-IO-AREA (22:24)
               MOVE 'RECORD IKKE LAGT UT.    ' TO LISTE-IO-AREA (47:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'AVSTEMMING PROG. ROR202 ' TO LISTE-IO-AREA (1:24)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-BEFORE-SPACE
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALT ANTALL RECORDS' TO LISTE-IO-AREA (1:21)
               MOVE 'PÅ RESTORDREMASTER'   TO LISTE-IO-AREA (23:18)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (43:8)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT. REST ORDRE         ' TO LISTE-IO-AREA (3:24)
               MOVE ANTORD                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (27:6)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT. REST RECORDS       ' TO LISTE-IO-AREA (3:24)
               MOVE ANTREC                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (27:6)
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
           SET INF-LEVEL-INIT              TO TRUE
           INITIALIZE INF-DATA-FIELDS
           SET INF-EOF-OFF                 TO TRUE
           SET INF-PROCESS                 TO TRUE
           OPEN INPUT INF
           OPEN OUTPUT RESTMAS
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INF
           CLOSE RESTMAS
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
