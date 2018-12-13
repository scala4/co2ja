       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK410R.
      **********************************************  Z-WIN-RPG2   ****
      *                                          XX2000XXIRXXMT      *
      *  MERGE KONTANT.FAKTURA.FILE MED KONTANT.KASSER FOR Å         *
      *  HENT FAKTURA KUNDENR OG HOVEDBOKSKONTO.                     *
      * 11.02.94 ESPEN LARSEN                                        *
      ****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK410.rpg
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
           SELECT KASSEF
               ASSIGN TO KASSEF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS KASSEF-STATUS
               RECORD KEY IS KASSEF-KEY1.
           SELECT KONTFAK
               ASSIGN TO UT-S-KONTFAK
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KONTFAK-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD KASSEF
               RECORD CONTAINS 240.
       01  KASSEF-IO-AREA.
           05  KASSEF-IO-AREA-X.
               10  KASSEF-KEY1.
                   15  KASSEF-KEY1N        PICTURE S9(5).
               10  FILLER                  PICTURE X(235).
       FD KONTFAK
               BLOCK CONTAINS 400
               RECORD CONTAINS 40.
       01  KONTFAK-IO-AREA.
           05  KONTFAK-IO-AREA-X           PICTURE X(40).
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
           10  KASSEF-STATUS               PICTURE 99 VALUE 0.
           10  KONTFAK-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  KASSEF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  KASSEF-EOF-OFF          VALUE '0'.
               88  KASSEF-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KASSEF-READ-OFF         VALUE '0'.
               88  KASSEF-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KASSEF-PROCESS-OFF      VALUE '0'.
               88  KASSEF-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  KASSEF-LEVEL-INIT-OFF   VALUE '0'.
               88  KASSEF-LEVEL-INIT       VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KONTFAK-EOF-OFF         VALUE '0'.
               88  KONTFAK-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KONTFAK-READ-OFF        VALUE '0'.
               88  KONTFAK-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KONTFAK-PROCESS-OFF     VALUE '0'.
               88  KONTFAK-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  KONTFAK-LEVEL-INIT-OFF  VALUE '0'.
               88  KONTFAK-LEVEL-INIT      VALUE '1'.
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
           05  KASSEF-LEVEL-01.
               10  KASSEF-01-L1.
                   15  KASSEF-01-L1-FIRMA  PICTURE X(3).
           05  KASSEF-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  KASSE                   PICTURE X(2).
               10  FAKKNR                  PICTURE X(6).
               10  HBKONT                  PICTURE X(4).
           05  KASSEF-MP                   PICTURE X(5).
           05  KASSEF-MC                   PICTURE X(5).
           05  KASSEF-M-01             REDEFINES KASSEF-MC.
               10  KASSEF-M-01-M2.
                   15  KASSEF-M-01-M2-FIRMA-G.
                       20  KASSEF-M-01-M2-FIRMA PICTURE X(3).
               10  KASSEF-M-01-M1.
                   15  KASSEF-M-01-M1-KASSE-G.
                       20  KASSEF-M-01-M1-KASSE PICTURE X(2).
           05  KONTFAK-LEVEL-02.
               10  KONTFAK-02-L1.
                   15  KONTFAK-02-L1-FIRMA PICTURE X(3).
           05  KONTFAK-DATA-FIELDS.
      ************************************************************
      * HOVEDRUTINE.                                             *
      ************************************************************
               10  FILLER                  PICTURE X.
           05  KONTFAK-MP                  PICTURE X(5).
           05  KONTFAK-MC                  PICTURE X(5).
           05  KONTFAK-M-02            REDEFINES KONTFAK-MC.
               10  KONTFAK-M-02-M2.
                   15  KONTFAK-M-02-M2-FIRMA-G.
                       20  KONTFAK-M-02-M2-FIRMA PICTURE X(3).
               10  KONTFAK-M-02-M1.
                   15  KONTFAK-M-02-M1-KASSE-G.
                       20  KONTFAK-M-02-M1-KASSE PICTURE X(2).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  ANT01-IO.
                   15  ANT01               PICTURE S9(5).
               10  TOT01-IO.
                   15  TOT01               PICTURE S9(5).
               10  ANT02-IO.
                   15  ANT02               PICTURE S9(5).
               10  TOT02-IO.
                   15  TOT02               PICTURE S9(5).
               10  ANTF-IO.
                   15  ANTF                PICTURE S9(5).
               10  TOTF-IO.
                   15  TOTF                PICTURE S9(5).
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
               88  NOT-SET-I-OV            VALUE '0'.
               88  SET-I-OV                VALUE '1'.
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
           IF  NOT-SET-I-OV
               SET NOT-I-OV                TO TRUE
           END-IF
           SET NOT-SET-I-OV                TO TRUE
 
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
           IF  KASSEF-PROCESS
               SET KASSEF-PROCESS-OFF      TO TRUE
               SET KASSEF-READ             TO TRUE
           END-IF
 
           IF  KASSEF-READ
               PERFORM KASSEF-GET
               SET KASSEF-READ-OFF         TO TRUE
               IF  NOT KASSEF-EOF
                   PERFORM KASSEF-MATCH-SET
               END-IF
           END-IF
 
           IF  KONTFAK-PROCESS
               SET KONTFAK-PROCESS-OFF     TO TRUE
               SET KONTFAK-READ            TO TRUE
           END-IF
 
           IF  KONTFAK-READ
               PERFORM KONTFAK-GET
               SET KONTFAK-READ-OFF        TO TRUE
               IF  NOT KONTFAK-EOF
                   PERFORM KONTFAK-MATCH-SET
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
 
           IF  KASSEF-PROCESS
               PERFORM KASSEF-IDSET
           END-IF
 
           IF  KONTFAK-PROCESS
               PERFORM KONTFAK-IDSET
           END-IF
 
           IF  KASSEF-PROCESS
               PERFORM KASSEF-CHK-LEVEL
           END-IF
 
           IF  KONTFAK-PROCESS
               PERFORM KONTFAK-CHK-LEVEL
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
 
           IF  KASSEF-PROCESS
               PERFORM KASSEF-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  KASSEF-PROCESS
           OR  KONTFAK-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-01)
               ADD 1                       TO ANT01
               ADD 1                       TO TOT01
               GO TO SLUTT-T
           END-IF
           ADD 1                           TO ANT02
           ADD 1                           TO TOT02
           IF  (NOT-I-MR)
               ADD 1                       TO ANTF
               ADD 1                       TO TOTF
           END-IF.
 
       SLUTT-T.
      *****************************************************************
      * KONTANT.FAKTURA.FILE.                                         *
      *****************************************************************
           CONTINUE.
 
       KASSEF-GET SECTION.
       KASSEF-GET-P.
           IF  KASSEF-EOF-OFF
               READ KASSEF
               AT END
                   SET KASSEF-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KASSEF-FLDSET SECTION.
       KASSEF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KASSEF-IO-AREA (1:3)   TO FIRMA (1:3)
               MOVE KASSEF-IO-AREA (4:2)   TO KASSE (1:2)
               MOVE KASSEF-IO-AREA (158:6) TO FAKKNR (1:6)
               MOVE KASSEF-IO-AREA (164:4) TO HBKONT (1:4)
           END-EVALUATE.
 
       KASSEF-IDSET SECTION.
       KASSEF-IDSET-P.
           SET I-01                        TO TRUE.
 
       KASSEF-CHK-LEVEL SECTION.
       KASSEF-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO KASSEF-LEVEL-01
               MOVE KASSEF-IO-AREA (1:3)   TO KASSEF-01-L1-FIRMA
               IF  KASSEF-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  KASSEF-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  KASSEF-01-L1          TO THE-PRIOR-L1
               SET KASSEF-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       KASSEF-MATCH-SET SECTION.
       KASSEF-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE KASSEF-IO-AREA (1:3)   TO KASSEF-M-01-M2-FIRMA
               MOVE KASSEF-IO-AREA (4:2)   TO KASSEF-M-01-M1-KASSE
           END-EVALUATE.
 
       KONTFAK-GET SECTION.
       KONTFAK-GET-P.
           IF  KONTFAK-EOF-OFF
               READ KONTFAK
               AT END
                   SET KONTFAK-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KONTFAK-IDSET SECTION.
       KONTFAK-IDSET-P.
           SET I-02                        TO TRUE.
 
       KONTFAK-CHK-LEVEL SECTION.
       KONTFAK-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO KONTFAK-LEVEL-02
               MOVE KONTFAK-IO-AREA (1:3)  TO KONTFAK-02-L1-FIRMA
               IF  KONTFAK-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  KONTFAK-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  KONTFAK-02-L1         TO THE-PRIOR-L1
               SET KONTFAK-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       KONTFAK-MATCH-SET SECTION.
       KONTFAK-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE KONTFAK-IO-AREA (1:3)  TO KONTFAK-M-02-M2-FIRMA
               MOVE KONTFAK-IO-AREA (30:2) TO KONTFAK-M-02-M1-KASSE
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
               SET I-OV                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OV            TO TRUE
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
           IF  KASSEF-EOF
               MOVE HIGH-VALUES            TO KASSEF-MC
                                              KASSEF-MP
           END-IF
           IF  KONTFAK-EOF
               MOVE HIGH-VALUES            TO KONTFAK-MC
                                              KONTFAK-MP
           END-IF
           IF  KASSEF-MC < KASSEF-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  KONTFAK-MC < KONTFAK-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  KASSEF-MC < KONTFAK-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET KASSEF-PROCESS      TO TRUE
                   MOVE KASSEF-MC          TO KASSEF-MP
                   IF  KASSEF-MC = KONTFAK-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  KONTFAK-MC < KASSEF-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET KONTFAK-PROCESS     TO TRUE
                   MOVE KONTFAK-MC         TO KONTFAK-MP
                   IF  KONTFAK-MC = KASSEF-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  KASSEF-MC = KONTFAK-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET KASSEF-PROCESS      TO TRUE
                   MOVE KASSEF-MC          TO KASSEF-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND I-MR)
               MOVE FAKKNR                 TO KONTFAK-IO-AREA (10:6)
               MOVE HBKONT                 TO KONTFAK-IO-AREA (16:4)
      *****************************************************************
      * TOTALSUMMER PR. FIRMA OG GRANDTOTAL.                          *
      *****************************************************************
               REWRITE KONTFAK-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '***  STATESTIKK  MERGE' TO LISTE-IO-AREA (3:22)
               MOVE ' KONT.FAKT.REC MOT KONT' TO LISTE-IO-AREA (28:23)
               MOVE 'ANT.KASSER      ***'  TO LISTE-IO-AREA (51:19)
               MOVE 'DATO='                TO LISTE-IO-AREA (80:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (85:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMA'                TO LISTE-IO-AREA (1:5)
               MOVE 'ANT.KASSER'           TO LISTE-IO-AREA (11:10)
               MOVE 'ANT.FAKT.REC'         TO LISTE-IO-AREA (29:12)
               MOVE 'ANT.FEIL KASSE'       TO LISTE-IO-AREA (47:14)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OV)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '***  STATESTIKK  MERGE' TO LISTE-IO-AREA (3:22)
               MOVE ' KONT.FAKT.REC MOT KONT' TO LISTE-IO-AREA (28:23)
               MOVE 'ANT.KASSER      ***'  TO LISTE-IO-AREA (51:19)
               MOVE 'DATO='                TO LISTE-IO-AREA (80:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (85:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMA'                TO LISTE-IO-AREA (1:5)
               MOVE 'ANT.KASSER'           TO LISTE-IO-AREA (11:10)
               MOVE 'ANT.FAKT.REC'         TO LISTE-IO-AREA (29:12)
               MOVE 'ANT.FEIL KASSE'       TO LISTE-IO-AREA (47:14)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FIRMA                  TO LISTE-IO-AREA (3:3)
               MOVE ANT01                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (15:6)
               INITIALIZE ANT01
               MOVE ANT02                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (35:6)
               INITIALIZE ANT02
               MOVE ANTF                   TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (55:6)
               INITIALIZE ANTF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '***'                  TO LISTE-IO-AREA (3:3)
               MOVE TOT01                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (15:6)
               MOVE TOT02                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (35:6)
               MOVE TOTF                   TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (55:6)
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
           SET KASSEF-LEVEL-INIT           TO TRUE
           INITIALIZE KASSEF-DATA-FIELDS
           SET KASSEF-EOF-OFF              TO TRUE
           SET KASSEF-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO KASSEF-MC
                                              KASSEF-MP
           OPEN INPUT KASSEF
           SET KONTFAK-LEVEL-INIT          TO TRUE
           SET KONTFAK-EOF-OFF             TO TRUE
           SET KONTFAK-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO KONTFAK-MC
                                              KONTFAK-MP
           OPEN I-O KONTFAK
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE KASSEF
           CLOSE KONTFAK
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
