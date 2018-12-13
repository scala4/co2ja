       IDENTIFICATION DIVISION.
       PROGRAM-ID. VAK056R.
      **********************************************  Z-WIN-RPG2      *
      * PROGRAM.....: VAK056                               *
      * PROGRAMERE..: ESPEN LARSEN                         *
      * PROGRAMERT..:  2.05.97                             *
      * SISTE KORR..: 15.05.11                             *
      * DANNER NY VARE.KONTO.KURANT.                       *
      * NY SALDORECORD (SEQ.NR.001) DANNES.                *
      * NYE SEQ.NR PÅ ALLE RECORD DANNES.                  *
      * UTVIDET VAREKON TIL 100 POS                        *
      ******************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VAK056.rpg
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
           SELECT HJFILE
               ASSIGN TO UT-S-HJFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS HJFILE-STATUS.
           SELECT KOPIIN
               ASSIGN TO UT-S-KOPIIN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KOPIIN-STATUS.
           SELECT VAREKON
               ASSIGN TO VAREKON
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS VAREKON-STATUS
               RECORD KEY IS VAREKON-KEY1.
           SELECT TOTALER
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TOTALER-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD HJFILE
               BLOCK CONTAINS 48
               RECORD CONTAINS 24.
       01  HJFILE-IO-AREA.
           05  HJFILE-IO-AREA-X            PICTURE X(24).
       FD KOPIIN
               BLOCK CONTAINS 228
               RECORD CONTAINS 114.
       01  KOPIIN-IO-AREA.
           05  KOPIIN-IO-AREA-X            PICTURE X(114).
       FD VAREKON
               RECORD CONTAINS 100.
       01  VAREKON-IO-AREA.
           05  VAREKON-IO-AREA-X.
               10  VAREKON-KEY1.
                   15  VAREKON-KEY1N       PICTURE S9(15).
               10  FILLER                  PICTURE X(85).
       FD TOTALER
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  TOTALER-IO-PRINT.
           05  TOTALER-IO-AREA-CONTROL     PICTURE X VALUE ' '.
        02 TOTALER-IO-AREA.
           05  TOTALER-IO-AREA-X           PICTURE X(132).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  HJFILE-STATUS               PICTURE 99 VALUE 0.
           10  KOPIIN-STATUS               PICTURE 99 VALUE 0.
           10  VAREKON-STATUS              PICTURE 99 VALUE 0.
           10  TOTALER-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  HJFILE-EOF-OFF          VALUE '0'.
               88  HJFILE-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  HJFILE-READ-OFF         VALUE '0'.
               88  HJFILE-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  HJFILE-PROCESS-OFF      VALUE '0'.
               88  HJFILE-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  HJFILE-LEVEL-INIT-OFF   VALUE '0'.
               88  HJFILE-LEVEL-INIT       VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KOPIIN-EOF-OFF          VALUE '0'.
               88  KOPIIN-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KOPIIN-READ-OFF         VALUE '0'.
               88  KOPIIN-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KOPIIN-PROCESS-OFF      VALUE '0'.
               88  KOPIIN-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  KOPIIN-LEVEL-INIT-OFF   VALUE '0'.
               88  KOPIIN-LEVEL-INIT       VALUE '1'.
           05  VAREKON-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  TOTALER-DATA-FIELDS.
               10  TOTALER-AFTER-SPACE     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-AFTER-SKIP      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-BEFORE-SPACE    PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-BEFORE-SKIP     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-MAX-LINES       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-LINE-COUNT      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-CLR-IO          PICTURE X VALUE 'Y'.
           05  HJFILE-LEVEL-01.
               10  HJFILE-01-L2.
                   15  HJFILE-01-L2-FIRM   PICTURE S9(3).
               10  HJFILE-01-L1.
                   15  HJFILE-01-L1-EDBN   PICTURE S9(7).
           05  HJFILE-DATA-FIELDS.
               10  FIRM-IO.
                   15  FIRM                PICTURE S9(3).
               10  EDBN-IO.
                   15  EDBN                PICTURE S9(7).
               10  ANTN-IO.
                   15  ANTN                PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
           05  HJFILE-MP                   PICTURE X(10).
           05  HJFILE-MC                   PICTURE X(10).
           05  HJFILE-M-01             REDEFINES HJFILE-MC.
               10  HJFILE-M-01-M2.
                   15  HJFILE-M-01-M2-FIRM-G.
                       20  HJFILE-M-01-M2-FIRM PICTURE S9(3).
               10  HJFILE-M-01-M1.
                   15  HJFILE-M-01-M1-EDBN-G.
                       20  HJFILE-M-01-M1-EDBN PICTURE S9(7).
           05  KOPIIN-LEVEL-02.
               10  KOPIIN-02-L2.
                   15  KOPIIN-02-L2-FIRM   PICTURE S9(3).
               10  KOPIIN-02-L1.
                   15  KOPIIN-02-L1-EDBN   PICTURE S9(7).
           05  KOPIIN-DATA-FIELDS.
               10  KONREC                  PICTURE X(100).
           05  KOPIIN-MP                   PICTURE X(10).
           05  KOPIIN-MC                   PICTURE X(10).
           05  KOPIIN-M-02             REDEFINES KOPIIN-MC.
               10  KOPIIN-M-02-M2.
                   15  KOPIIN-M-02-M2-FIRM-G.
                       20  KOPIIN-M-02-M2-FIRM PICTURE S9(3).
               10  KOPIIN-M-02-M1.
                   15  KOPIIN-M-02-M1-EDBN-G.
                       20  KOPIIN-M-02-M1-EDBN PICTURE S9(7).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(7).
           05  TEMPORARY-FIELDS.
               10  SEQNR-IO.
                   15  SEQNR               PICTURE S9(5).
               10  SSEQNR-IO.
                   15  SSEQNR              PICTURE S9(5).
               10  ANTN2-IO.
                   15  ANTN2               PICTURE S9(4).
               10  ANTUT-IO.
                   15  ANTUT               PICTURE S9(8).
               10  ANTUTT-IO.
                   15  ANTUTT              PICTURE S9(8).
           05  EDITTING-FIELDS.
               10  XO-50P-EF.
                 15  XO-50P                PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  XO-80YY9                PICTURE ZZ.ZZZ.ZZ9.
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
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  HJFILE-PROCESS
               SET HJFILE-PROCESS-OFF      TO TRUE
               SET HJFILE-READ             TO TRUE
           END-IF
 
           IF  HJFILE-READ
               PERFORM HJFILE-GET
               SET HJFILE-READ-OFF         TO TRUE
               IF  NOT HJFILE-EOF
                   PERFORM HJFILE-MATCH-SET
               END-IF
           END-IF
 
           IF  KOPIIN-PROCESS
               SET KOPIIN-PROCESS-OFF      TO TRUE
               SET KOPIIN-READ             TO TRUE
           END-IF
 
           IF  KOPIIN-READ
               PERFORM KOPIIN-GET
               SET KOPIIN-READ-OFF         TO TRUE
               IF  NOT KOPIIN-EOF
                   PERFORM KOPIIN-MATCH-SET
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
 
           IF  HJFILE-PROCESS
               PERFORM HJFILE-IDSET
           END-IF
 
           IF  KOPIIN-PROCESS
               PERFORM KOPIIN-IDSET
           END-IF
 
           IF  HJFILE-PROCESS
               PERFORM HJFILE-CHK-LEVEL
           END-IF
 
           IF  KOPIIN-PROCESS
               PERFORM KOPIIN-CHK-LEVEL
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
 
           IF  HJFILE-PROCESS
               PERFORM HJFILE-FLDOFF
               PERFORM HJFILE-FLDSET
           END-IF
 
           IF  KOPIIN-PROCESS
               PERFORM KOPIIN-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  HJFILE-PROCESS
           OR  KOPIIN-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-50                    TO TRUE
           IF  (I-01)
               SET NOT-I-98                TO TRUE
               SET NOT-I-99                TO TRUE
      *****************************************************************
      * RUTINE VED NYTT EDB-NR.                                       *
      *****************************************************************
           END-IF
           IF  (I-01 AND I-10)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               MOVE 0                      TO SEQNR
      *****************************************************************
      * RUTINE FOR Å SJEKKE ANTALL RECORDS SOM PR. EDB.NR.            *
      *   ER ANTALLET 99999 MÅ VI FJERNE FØRSTE RECORD FOR Å FÅ PLASS *
      *   TIL NY SALDORECORDS.                                        *
      *****************************************************************
           END-IF
           IF  (I-01)
               SET NOT-I-98                TO TRUE
               IF  ANTN > 99999
                   SET I-98                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-98)
               ADD ANTN TO ZERO        GIVING SSEQNR
           END-IF
           IF  (I-01 AND I-98)
               MOVE 99999                  TO SSEQNR
           END-IF
           IF  (I-01)
               ADD ANTN TO ZERO        GIVING ANTN2
               GO TO SLUTT-T
           END-IF
           SET NOT-I-97                    TO TRUE
           IF  FIRM = 996
               SET I-97                    TO TRUE
           END-IF
      *****************************************************************
      * RUTINE FOR Å DANNE NYTT SEQ.NR PÅ VARE.KONTO.KURANT.          *
      *****************************************************************
           IF  (I-02)
               SET NOT-I-99                TO TRUE
               IF  ANTN2 > 99999
                   SET I-99                TO TRUE
               END-IF
               SUBTRACT 1                  FROM ANTN2
           END-IF
           IF  (I-02 AND I-99)
               GO TO SLUTT-T
           END-IF
           IF  (I-02)
               ADD 1                       TO SEQNR
               SET NOT-I-50                TO TRUE
               IF  SEQNR = 1
                   SET I-50                TO TRUE
               END-IF
               ADD 1                       TO ANTUT
               ADD 1                       TO ANTUTT
           END-IF.
 
       SLUTT-T.
      *****************************************************************
           CONTINUE.
 
       HJFILE-GET SECTION.
       HJFILE-GET-P.
           IF  HJFILE-EOF-OFF
               READ HJFILE
               AT END
                   SET HJFILE-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       HJFILE-FLDOFF SECTION.
       HJFILE-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-10                TO TRUE
           END-EVALUATE.
 
       HJFILE-FLDSET SECTION.
       HJFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE HJFILE-IO-AREA (1:3)   TO FIRM-IO
               INSPECT FIRM-IO REPLACING ALL ' ' BY '0'
               MOVE HJFILE-IO-AREA (4:7)   TO EDBN-IO
               INSPECT EDBN-IO REPLACING ALL ' ' BY '0'
               MOVE HJFILE-IO-AREA (19:3)  TO ANTN-IO
               IF  ANTN = ZERO
                   SET I-10                TO TRUE
               END-IF
           END-EVALUATE.
 
       HJFILE-IDSET SECTION.
       HJFILE-IDSET-P.
           SET I-01                        TO TRUE.
 
       HJFILE-CHK-LEVEL SECTION.
       HJFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO HJFILE-LEVEL-01
               MOVE HJFILE-IO-AREA (1:3)   TO HJFILE-01-L2-FIRM
               MOVE HJFILE-IO-AREA (4:7)   TO HJFILE-01-L1-EDBN
               IF  HJFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  HJFILE-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  HJFILE-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  HJFILE-01-L2          TO THE-PRIOR-L2
               MOVE  HJFILE-01-L1          TO THE-PRIOR-L1
               SET HJFILE-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       HJFILE-MATCH-SET SECTION.
       HJFILE-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE HJFILE-IO-AREA (1:3)   TO HJFILE-M-01-M2-FIRM
               MOVE HJFILE-IO-AREA (4:7)   TO HJFILE-M-01-M1-EDBN
           END-EVALUATE.
 
       KOPIIN-GET SECTION.
       KOPIIN-GET-P.
           IF  KOPIIN-EOF-OFF
               READ KOPIIN
               AT END
                   SET KOPIIN-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KOPIIN-FLDSET SECTION.
       KOPIIN-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KOPIIN-IO-AREA (1:100) TO KONREC (1:100)
               MOVE KOPIIN-IO-AREA (1:3)   TO FIRM-IO
               INSPECT FIRM-IO REPLACING ALL ' ' BY '0'
               MOVE KOPIIN-IO-AREA (4:7)   TO EDBN-IO
               INSPECT EDBN-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       KOPIIN-IDSET SECTION.
       KOPIIN-IDSET-P.
           SET I-02                        TO TRUE.
 
       KOPIIN-CHK-LEVEL SECTION.
       KOPIIN-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO KOPIIN-LEVEL-02
               MOVE KOPIIN-IO-AREA (1:3)   TO KOPIIN-02-L2-FIRM
               MOVE KOPIIN-IO-AREA (4:7)   TO KOPIIN-02-L1-EDBN
               IF  KOPIIN-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  KOPIIN-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  KOPIIN-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  KOPIIN-02-L2          TO THE-PRIOR-L2
               MOVE  KOPIIN-02-L1          TO THE-PRIOR-L1
               SET KOPIIN-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       KOPIIN-MATCH-SET SECTION.
       KOPIIN-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE KOPIIN-IO-AREA (1:3)   TO KOPIIN-M-02-M2-FIRM
               MOVE KOPIIN-IO-AREA (4:7)   TO KOPIIN-M-02-M1-EDBN
           END-EVALUATE.
 
       TOTALER-PRINT-LINE SECTION.
       TOTALER-PRINT-LINE-P.
           IF  TOTALER-BEFORE-SKIP > 0
               PERFORM TOTALER-SKIP-BEFORE
           END-IF
           IF  TOTALER-BEFORE-SPACE > 0
               PERFORM TOTALER-SPACE-BEFORE
               IF  TOTALER-AFTER-SKIP > 0
                   PERFORM TOTALER-SKIP-AFTER
               END-IF
               IF  TOTALER-AFTER-SPACE > 0
                   PERFORM TOTALER-SPACE-AFTER
               END-IF
           ELSE
               IF  TOTALER-AFTER-SKIP > 0
                   PERFORM TOTALER-SKIP-AFTER
               END-IF
               PERFORM TOTALER-SPACE-AFTER
           END-IF
           IF  TOTALER-LINE-COUNT NOT < TOTALER-MAX-LINES
               SET I-OF                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OF            TO TRUE
               END-IF
           END-IF.
 
       TOTALER-SKIP-BEFORE SECTION.
       TOTALER-SKIP-BEFORE-P.
           WRITE TOTALER-IO-PRINT       AFTER ADVANCING PAGE
           MOVE 1                          TO TOTALER-LINE-COUNT
           MOVE 0                          TO TOTALER-BEFORE-SKIP
           INITIALIZE TOTALER-IO-AREA.
 
       TOTALER-SPACE-BEFORE SECTION.
       TOTALER-SPACE-BEFORE-P.
           WRITE TOTALER-IO-PRINT       AFTER TOTALER-BEFORE-SPACE
                                                                 LINES
           ADD TOTALER-BEFORE-SPACE        TO TOTALER-LINE-COUNT
           MOVE SPACES TO TOTALER-IO-AREA
           INITIALIZE TOTALER-IO-AREA
           MOVE 0                          TO TOTALER-BEFORE-SPACE.
 
       TOTALER-SKIP-AFTER SECTION.
       TOTALER-SKIP-AFTER-P.
           WRITE TOTALER-IO-PRINT      BEFORE ADVANCING PAGE
           MOVE 1                          TO TOTALER-LINE-COUNT
           MOVE 0                          TO TOTALER-AFTER-SKIP
           INITIALIZE TOTALER-IO-AREA.
 
       TOTALER-SPACE-AFTER SECTION.
       TOTALER-SPACE-AFTER-P.
           WRITE TOTALER-IO-PRINT      BEFORE TOTALER-AFTER-SPACE LINES
           ADD TOTALER-AFTER-SPACE         TO TOTALER-LINE-COUNT
           INITIALIZE TOTALER-IO-AREA
           MOVE 0                          TO TOTALER-AFTER-SPACE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  HJFILE-EOF
               MOVE HIGH-VALUES            TO HJFILE-MC
                                              HJFILE-MP
           END-IF
           IF  KOPIIN-EOF
               MOVE HIGH-VALUES            TO KOPIIN-MC
                                              KOPIIN-MP
           END-IF
           IF  HJFILE-MC < HJFILE-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  KOPIIN-MC < KOPIIN-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  HJFILE-MC < KOPIIN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET HJFILE-PROCESS      TO TRUE
                   MOVE HJFILE-MC          TO HJFILE-MP
                   IF  HJFILE-MC = KOPIIN-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  KOPIIN-MC < HJFILE-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET KOPIIN-PROCESS      TO TRUE
                   MOVE KOPIIN-MC          TO KOPIIN-MP
                   IF  KOPIIN-MC = HJFILE-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  HJFILE-MC = KOPIIN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET HJFILE-PROCESS      TO TRUE
                   MOVE HJFILE-MC          TO HJFILE-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND NOT-I-99)
               MOVE SPACES TO VAREKON-IO-AREA
               INITIALIZE VAREKON-IO-AREA
               MOVE KONREC                 TO VAREKON-IO-AREA (1:100)
               MOVE SEQNR-IO               TO VAREKON-IO-AREA (11:5)
               IF  (NOT-I-50)
                   MOVE SEQNR              TO XO-50P
                   MOVE XO-50P-EF          TO VAREKON-IO-AREA (16:3)
               END-IF
               IF  (I-50)
                   MOVE SSEQNR             TO XO-50P
                   MOVE XO-50P-EF          TO VAREKON-IO-AREA (16:3)
               END-IF
               WRITE VAREKON-IO-AREA
           END-IF
           IF  (I-1P)
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE '      REORG.'         TO TOTALER-IO-AREA (4:12)
               MOVE 'AV VARE.KONTO.KURANT' TO TOTALER-IO-AREA (17:20)
               MOVE 'PR.'                  TO TOTALER-IO-AREA (38:3)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO TOTALER-IO-AREA (43:8)
               MOVE 01                     TO TOTALER-BEFORE-SKIP
               MOVE 1                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE 'FIRMA'                TO TOTALER-IO-AREA (1:5)
               MOVE 'TOT LAGT UT'          TO TOTALER-IO-AREA (10:11)
               MOVE 2                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
           END-IF.
 
       DETAIL-OVERFLOW SECTION.
       DETAIL-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE '      REORG.'         TO TOTALER-IO-AREA (4:12)
               MOVE 'AV VARE.KONTO.KURANT' TO TOTALER-IO-AREA (17:20)
               MOVE 'PR.'                  TO TOTALER-IO-AREA (38:3)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO TOTALER-IO-AREA (43:8)
               MOVE 01                     TO TOTALER-BEFORE-SKIP
               MOVE 1                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE 'FIRMA'                TO TOTALER-IO-AREA (1:5)
               MOVE 'TOT LAGT UT'          TO TOTALER-IO-AREA (10:11)
               MOVE 2                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L2)
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE FIRM-IO                TO TOTALER-IO-AREA (3:3)
               MOVE ANTUT                  TO XO-80YY9
               MOVE XO-80YY9               TO TOTALER-IO-AREA (11:10)
               INITIALIZE ANTUT
               MOVE 1                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE 'TOTAL'                TO TOTALER-IO-AREA (1:5)
               MOVE ANTUTT                 TO XO-80YY9
               MOVE XO-80YY9               TO TOTALER-IO-AREA (11:10)
               MOVE 1                      TO TOTALER-BEFORE-SPACE
               PERFORM TOTALER-PRINT-LINE
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
           SET HJFILE-LEVEL-INIT           TO TRUE
           INITIALIZE HJFILE-DATA-FIELDS
           SET HJFILE-EOF-OFF              TO TRUE
           SET HJFILE-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO HJFILE-MC
                                              HJFILE-MP
           OPEN INPUT HJFILE
           SET KOPIIN-LEVEL-INIT           TO TRUE
           INITIALIZE KOPIIN-DATA-FIELDS
           SET KOPIIN-EOF-OFF              TO TRUE
           SET KOPIIN-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO KOPIIN-MC
                                              KOPIIN-MP
           OPEN INPUT KOPIIN
           OPEN OUTPUT VAREKON
           OPEN OUTPUT TOTALER
           INITIALIZE TOTALER-IO-AREA
           INITIALIZE TOTALER-DATA-FIELDS
           MOVE 57                         TO TOTALER-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE HJFILE
           CLOSE KOPIIN
           CLOSE VAREKON
           IF TOTALER-IO-AREA NOT = SPACES
             WRITE TOTALER-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO TOTALER-IO-AREA
           END-IF
           CLOSE TOTALER.
 
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
