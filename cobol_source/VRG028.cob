       IDENTIFICATION DIVISION.
       PROGRAM-ID. VRG028R.
      **********************************************  Z-WIN-RPG2   ****
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VRG028.rpg
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
           SELECT KUNDEMA
               ASSIGN TO KUNDEMA
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMA-STATUS
               RECORD KEY IS KUNDEMA-KEY1.
           SELECT KUNDEMX
               ASSIGN TO KUNDEMX
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMX-STATUS
               RECORD KEY IS KUNDEMX-KEY1.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT OUTFILE
               ASSIGN TO UT-S-OUTFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTFILE-STATUS.
           SELECT PRINT-X
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PRINT-X-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1.
                   15  KUNDEMA-KEY1N       PICTURE S9(9).
               10  FILLER                  PICTURE X(190).
       FD KUNDEMX
               RECORD CONTAINS 200.
       01  KUNDEMX-IO-AREA.
           05  KUNDEMX-IO-AREA-X.
               10  KUNDEMX-KEY1.
                   15  KUNDEMX-KEY1N       PICTURE S9(10).
               10  FILLER                  PICTURE X(190).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD OUTFILE
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  OUTFILE-IO-AREA.
           05  OUTFILE-IO-AREA-X           PICTURE X(200).
       FD PRINT-X
               BLOCK CONTAINS 121
               RECORD CONTAINS 121.
       01  PRINT-X-IO-PRINT.
           05  PRINT-X-IO-AREA-CONTROL     PICTURE X VALUE ' '.
        02 PRINT-X-IO-AREA.
           05  PRINT-X-IO-AREA-X           PICTURE X(120).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  KUNDEMX-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  OUTFILE-STATUS              PICTURE 99 VALUE 0.
           10  PRINT-X-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  KUNDEMA-EOF-OFF         VALUE '0'.
               88  KUNDEMA-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KUNDEMA-READ-OFF        VALUE '0'.
               88  KUNDEMA-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KUNDEMA-PROCESS-OFF     VALUE '0'.
               88  KUNDEMA-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  KUNDEMA-LEVEL-INIT-OFF  VALUE '0'.
               88  KUNDEMA-LEVEL-INIT      VALUE '1'.
           05  KUNDEMX-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  KUNDEMX-EOF-OFF         VALUE '0'.
               88  KUNDEMX-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KUNDEMX-READ-OFF        VALUE '0'.
               88  KUNDEMX-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KUNDEMX-PROCESS-OFF     VALUE '0'.
               88  KUNDEMX-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  KUNDEMX-LEVEL-INIT-OFF  VALUE '0'.
               88  KUNDEMX-LEVEL-INIT      VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
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
           05  KUNDEMA-LEVEL-01.
               10  KUNDEMA-01-L2.
                   15  KUNDEMA-01-L2-FIRMA PICTURE X(3).
               10  KUNDEMA-01-L1.
                   15  KUNDEMA-01-L1-RESKNR PICTURE X(6).
           05  KUNDEMA-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  RESKNR                  PICTURE X(6).
           05  KUNDEMA-MP                  PICTURE X(9).
           05  KUNDEMA-MC                  PICTURE X(9).
           05  KUNDEMA-M-01            REDEFINES KUNDEMA-MC.
               10  KUNDEMA-M-01-M2.
                   15  KUNDEMA-M-01-M2-FIRMA-G.
                       20  KUNDEMA-M-01-M2-FIRMA PICTURE X(3).
               10  KUNDEMA-M-01-M1.
                   15  KUNDEMA-M-01-M1-RESKNR-G.
                       20  KUNDEMA-M-01-M1-RESKNR PICTURE X(6).
           05  KUNDEMX-LEVEL-02.
               10  KUNDEMX-02-L2.
                   15  KUNDEMX-02-L2-FIRMA PICTURE X(3).
               10  KUNDEMX-02-L1.
                   15  KUNDEMX-02-L1-RESKNR PICTURE X(6).
           05  KUNDEMX-DATA-FIELDS.
               10  RECORD-X                PICTURE X(200).
               10  KXDATA                  PICTURE X(190).
           05  KUNDEMX-MP                  PICTURE X(9).
           05  KUNDEMX-MC                  PICTURE X(9).
           05  KUNDEMX-M-02            REDEFINES KUNDEMX-MC.
               10  KUNDEMX-M-02-M2.
                   15  KUNDEMX-M-02-M2-FIRMA-G.
                       20  KUNDEMX-M-02-M2-FIRMA PICTURE X(3).
               10  KUNDEMX-M-02-M1.
                   15  KUNDEMX-M-02-M1-RESKNR-G.
                       20  KUNDEMX-M-02-M1-RESKNR PICTURE X(6).
           05  FIRMAF-DATA-FIELDS.
               10  FINAVN                  PICTURE X(30).
               10  FIRMSL                  PICTURE X(1).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  ANTL1-IO.
                   15  ANTL1               PICTURE S9(7).
               10  ANTL1T-IO.
                   15  ANTL1T              PICTURE S9(7).
               10  ANTLR-IO.
                   15  ANTLR               PICTURE S9(7).
               10  ANTLRT-IO.
                   15  ANTLRT              PICTURE S9(7).
           05  EDITTING-FIELDS.
               10  XO-70YY9                PICTURE Z.ZZZ.ZZ9.
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
           IF  NOT-SET-I-OF
               SET NOT-I-OF                TO TRUE
           END-IF
           SET NOT-SET-I-OF                TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  KUNDEMA-PROCESS
               SET KUNDEMA-PROCESS-OFF     TO TRUE
               SET KUNDEMA-READ            TO TRUE
           END-IF
 
           IF  KUNDEMA-READ
               PERFORM KUNDEMA-GET
               SET KUNDEMA-READ-OFF        TO TRUE
               IF  NOT KUNDEMA-EOF
                   PERFORM KUNDEMA-MATCH-SET
               END-IF
           END-IF
 
           IF  KUNDEMX-PROCESS
               SET KUNDEMX-PROCESS-OFF     TO TRUE
               SET KUNDEMX-READ            TO TRUE
           END-IF
 
           IF  KUNDEMX-READ
               PERFORM KUNDEMX-GET
               SET KUNDEMX-READ-OFF        TO TRUE
               IF  NOT KUNDEMX-EOF
                   PERFORM KUNDEMX-MATCH-SET
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
 
           IF  KUNDEMA-PROCESS
               PERFORM KUNDEMA-IDSET
           END-IF
 
           IF  KUNDEMX-PROCESS
               PERFORM KUNDEMX-IDSET
           END-IF
 
           IF  KUNDEMA-PROCESS
               PERFORM KUNDEMA-CHK-LEVEL
           END-IF
 
           IF  KUNDEMX-PROCESS
               PERFORM KUNDEMX-CHK-LEVEL
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
           PERFORM HEADING-OVERFLOW
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  KUNDEMA-PROCESS
               PERFORM KUNDEMA-FLDSET
           END-IF
 
           IF  KUNDEMX-PROCESS
               PERFORM KUNDEMX-FLDOFF
               PERFORM KUNDEMX-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  KUNDEMA-PROCESS
           OR  KUNDEMX-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-L2)
               PERFORM FISLET-S
           END-IF
           IF  (I-01)
               GO TO SLUTT-T
           END-IF
           ADD 1                           TO ANTL1
           ADD 1                           TO ANTL1T
           IF  (NOT-I-98 AND NOT-I-08 AND I-MR)
               ADD 1                       TO ANTLR
               ADD 1                       TO ANTLRT
           END-IF.
 
       SLUTT-T.
      ******************************************************
      *    SUBRUTINE FOR SLETTING AV HELE FIRMA            *
      ******************************************************
           CONTINUE.
 
       FISLET-S SECTION.
       FISLET-S-P.
           SET NOT-I-98                    TO TRUE
           MOVE FIRMA                      TO FIRMAF-KEY1
           READ FIRMAF RECORD KEY IS FIRMAF-KEY1
           INVALID KEY
               SET I-96                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-96                TO TRUE
               PERFORM FIRMAF-FLDSET
               PERFORM FIRMAF-IDSET
           END-READ
           IF  (I-96)
               SET I-98                    TO TRUE
           END-IF
           IF  (NOT-I-96)
               SET NOT-I-98                TO TRUE
               IF  FIRMSL = 'S'
                   SET I-98                TO TRUE
               END-IF
           END-IF.
      ******************************************************
 
       KUNDEMA-GET SECTION.
       KUNDEMA-GET-P.
           IF  KUNDEMA-EOF-OFF
               READ KUNDEMA
               AT END
                   SET KUNDEMA-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE KUNDEMA-IO-AREA (6:6)  TO RESKNR (1:6)
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-01                        TO TRUE.
 
       KUNDEMA-CHK-LEVEL SECTION.
       KUNDEMA-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO KUNDEMA-LEVEL-01
               MOVE KUNDEMA-IO-AREA (3:3)  TO KUNDEMA-01-L2-FIRMA
               MOVE KUNDEMA-IO-AREA (6:6)  TO KUNDEMA-01-L1-RESKNR
               IF  KUNDEMA-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  KUNDEMA-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  KUNDEMA-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  KUNDEMA-01-L2         TO THE-PRIOR-L2
               MOVE  KUNDEMA-01-L1         TO THE-PRIOR-L1
               SET KUNDEMA-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       KUNDEMA-MATCH-SET SECTION.
       KUNDEMA-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (3:3)  TO KUNDEMA-M-01-M2-FIRMA
               MOVE KUNDEMA-IO-AREA (6:6)  TO KUNDEMA-M-01-M1-RESKNR
           END-EVALUATE.
 
       KUNDEMX-GET SECTION.
       KUNDEMX-GET-P.
           IF  KUNDEMX-EOF-OFF
               READ KUNDEMX
               AT END
                   SET KUNDEMX-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KUNDEMX-FLDOFF SECTION.
       KUNDEMX-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-08                TO TRUE
           END-EVALUATE.
 
       KUNDEMX-FLDSET SECTION.
       KUNDEMX-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMX-IO-AREA (1:200) TO RECORD-X (1:200)
               MOVE KUNDEMX-IO-AREA (1:3)  TO FIRMA (1:3)
               MOVE KUNDEMX-IO-AREA (4:6)  TO RESKNR (1:6)
               MOVE KUNDEMX-IO-AREA (11:190) TO KXDATA (1:190)
               IF  KXDATA = SPACES
                   SET I-08                TO TRUE
               END-IF
           END-EVALUATE.
 
       KUNDEMX-IDSET SECTION.
       KUNDEMX-IDSET-P.
           SET I-02                        TO TRUE.
 
       KUNDEMX-CHK-LEVEL SECTION.
       KUNDEMX-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO KUNDEMX-LEVEL-02
               MOVE KUNDEMX-IO-AREA (1:3)  TO KUNDEMX-02-L2-FIRMA
               MOVE KUNDEMX-IO-AREA (4:6)  TO KUNDEMX-02-L1-RESKNR
               IF  KUNDEMX-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  KUNDEMX-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  KUNDEMX-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  KUNDEMX-02-L2         TO THE-PRIOR-L2
               MOVE  KUNDEMX-02-L1         TO THE-PRIOR-L1
               SET KUNDEMX-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       KUNDEMX-MATCH-SET SECTION.
       KUNDEMX-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMX-IO-AREA (1:3)  TO KUNDEMX-M-02-M2-FIRMA
               MOVE KUNDEMX-IO-AREA (4:6)  TO KUNDEMX-M-02-M1-RESKNR
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (8:30)  TO FINAVN (1:30)
               MOVE FIRMAF-IO-AREA (123:1) TO FIRMSL (1:1)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-03                        TO TRUE.
 
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
               SET I-OF                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OF            TO TRUE
               END-IF
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
           IF  KUNDEMA-EOF
               MOVE HIGH-VALUES            TO KUNDEMA-MC
                                              KUNDEMA-MP
           END-IF
           IF  KUNDEMX-EOF
               MOVE HIGH-VALUES            TO KUNDEMX-MC
                                              KUNDEMX-MP
           END-IF
           IF  KUNDEMA-MC < KUNDEMA-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  KUNDEMX-MC < KUNDEMX-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  KUNDEMA-MC < KUNDEMX-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET KUNDEMA-PROCESS     TO TRUE
                   MOVE KUNDEMA-MC         TO KUNDEMA-MP
                   IF  KUNDEMA-MC = KUNDEMX-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  KUNDEMX-MC < KUNDEMA-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET KUNDEMX-PROCESS     TO TRUE
                   MOVE KUNDEMX-MC         TO KUNDEMX-MP
                   IF  KUNDEMX-MC = KUNDEMA-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  KUNDEMA-MC = KUNDEMX-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET KUNDEMA-PROCESS     TO TRUE
                   MOVE KUNDEMA-MC         TO KUNDEMA-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND NOT-I-98 AND NOT-I-08)
           AND (I-MR)
               MOVE SPACES TO OUTFILE-IO-AREA
               INITIALIZE OUTFILE-IO-AREA
               MOVE RECORD-X               TO OUTFILE-IO-AREA (1:200)
               WRITE OUTFILE-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P)
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE 'AVSTEMMING PROG. VRG028 ' TO PRINT-X-IO-AREA
                                                                (1:24)
               MOVE 01                     TO PRINT-X-BEFORE-SKIP
               MOVE 3                      TO PRINT-X-BEFORE-SPACE
               MOVE 3                      TO PRINT-X-AFTER-SPACE
               PERFORM PRINT-X-PRINT-LINE
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE 'KUNDE.XTRA VSA.VAREMAS' TO PRINT-X-IO-AREA (9:22)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO PRINT-X-IO-AREA (35:8)
               MOVE 2                      TO PRINT-X-AFTER-SPACE
               PERFORM PRINT-X-PRINT-LINE
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE 'FIRMA'                TO PRINT-X-IO-AREA (1:5)
               MOVE 'ANTALL'               TO PRINT-X-IO-AREA (15:6)
               MOVE 'FIRMANAVN'            TO PRINT-X-IO-AREA (26:9)
               MOVE 'MERKNADER'            TO PRINT-X-IO-AREA (57:9)
               MOVE 'ANT.OK.'              TO PRINT-X-IO-AREA (74:7)
               MOVE 2                      TO PRINT-X-AFTER-SPACE
               PERFORM PRINT-X-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE 'AVSTEMMING PROG. VRG028 ' TO PRINT-X-IO-AREA
                                                                (1:24)
               MOVE 01                     TO PRINT-X-BEFORE-SKIP
               MOVE 3                      TO PRINT-X-BEFORE-SPACE
               MOVE 3                      TO PRINT-X-AFTER-SPACE
               PERFORM PRINT-X-PRINT-LINE
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE 'KUNDE.XTRA VSA.VAREMAS' TO PRINT-X-IO-AREA (9:22)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO PRINT-X-IO-AREA (35:8)
               MOVE 2                      TO PRINT-X-AFTER-SPACE
               PERFORM PRINT-X-PRINT-LINE
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE 'FIRMA'                TO PRINT-X-IO-AREA (1:5)
               MOVE 'ANTALL'               TO PRINT-X-IO-AREA (15:6)
               MOVE 'FIRMANAVN'            TO PRINT-X-IO-AREA (26:9)
               MOVE 'MERKNADER'            TO PRINT-X-IO-AREA (57:9)
               MOVE 'ANT.OK.'              TO PRINT-X-IO-AREA (74:7)
               MOVE 2                      TO PRINT-X-AFTER-SPACE
               PERFORM PRINT-X-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L2)
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE FIRMA                  TO PRINT-X-IO-AREA (3:3)
               MOVE ANTL1                  TO XO-70YY9
               MOVE XO-70YY9               TO PRINT-X-IO-AREA (12:9)
               INITIALIZE ANTL1
               IF  (NOT-I-96)
                   MOVE FINAVN             TO PRINT-X-IO-AREA (26:30)
               END-IF
               IF  (I-98)
                   MOVE 'SLETTET'          TO PRINT-X-IO-AREA (57:7)
               END-IF
               MOVE ANTLR                  TO XO-70YY9
               MOVE XO-70YY9               TO PRINT-X-IO-AREA (72:9)
               INITIALIZE ANTLR
               MOVE 1                      TO PRINT-X-AFTER-SPACE
               PERFORM PRINT-X-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE '***'                  TO PRINT-X-IO-AREA (3:3)
               MOVE ANTL1T                 TO XO-70YY9
               MOVE XO-70YY9               TO PRINT-X-IO-AREA (12:9)
               MOVE 'RECORDS LEST.   '     TO PRINT-X-IO-AREA (26:16)
               MOVE ANTLRT                 TO XO-70YY9
               MOVE XO-70YY9               TO PRINT-X-IO-AREA (72:9)
               MOVE 'RECORDS KOPIERT.'     TO PRINT-X-IO-AREA (86:16)
               MOVE 1                      TO PRINT-X-BEFORE-SPACE
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
           SET KUNDEMA-LEVEL-INIT          TO TRUE
           INITIALIZE KUNDEMA-DATA-FIELDS
           SET KUNDEMA-EOF-OFF             TO TRUE
           SET KUNDEMA-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO KUNDEMA-MC
                                              KUNDEMA-MP
           OPEN INPUT KUNDEMA
           SET KUNDEMX-LEVEL-INIT          TO TRUE
           INITIALIZE KUNDEMX-DATA-FIELDS
           SET KUNDEMX-EOF-OFF             TO TRUE
           SET KUNDEMX-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO KUNDEMX-MC
                                              KUNDEMX-MP
           OPEN INPUT KUNDEMX
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           OPEN OUTPUT OUTFILE
           OPEN OUTPUT PRINT-X
           INITIALIZE PRINT-X-IO-AREA
           INITIALIZE PRINT-X-DATA-FIELDS
           MOVE 57                         TO PRINT-X-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE KUNDEMA
           CLOSE KUNDEMX
           CLOSE FIRMAF
           CLOSE OUTFILE
           IF PRINT-X-IO-AREA NOT = SPACES
             WRITE PRINT-X-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO PRINT-X-IO-AREA
           END-IF
           CLOSE PRINT-X.
 
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
