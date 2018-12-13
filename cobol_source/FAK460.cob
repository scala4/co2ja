       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK460R.
      **********************************************  Z-WIN-RPG2   ****
      *  FJERNE RECORD SOM ER FAKTURERT PÅ KONTANT.ORDRE.FILE        *
      * 14.02.94 ESPEN LARSEN                                        *
      ****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK460.rpg
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
           SELECT KONTFAK
               ASSIGN TO UT-S-KONTFAK
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KONTFAK-STATUS.
           SELECT KONTORD
               ASSIGN TO KONTORD
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS KONTORD-STATUS
               RECORD KEY IS KONTORD-KEY1.
           SELECT UTREC
               ASSIGN TO UT-S-UTREC
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS UTREC-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD KONTFAK
               BLOCK CONTAINS 400
               RECORD CONTAINS 40.
       01  KONTFAK-IO-AREA.
           05  KONTFAK-IO-AREA-X           PICTURE X(40).
       FD KONTORD
               RECORD CONTAINS 40.
       01  KONTORD-IO-AREA.
           05  KONTORD-IO-AREA-X.
               10  KONTORD-KEY1.
                   15  KONTORD-KEY1N       PICTURE S9(10).
               10  FILLER                  PICTURE X(30).
       FD UTREC
               BLOCK CONTAINS 400
               RECORD CONTAINS 40.
       01  UTREC-IO-AREA.
           05  UTREC-IO-AREA-X             PICTURE X(40).
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
           10  KONTFAK-STATUS              PICTURE 99 VALUE 0.
           10  KONTORD-STATUS              PICTURE 99 VALUE 0.
           10  UTREC-STATUS                PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
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
           05  KONTORD-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  KONTORD-EOF-OFF         VALUE '0'.
               88  KONTORD-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KONTORD-READ-OFF        VALUE '0'.
               88  KONTORD-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KONTORD-PROCESS-OFF     VALUE '0'.
               88  KONTORD-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  KONTORD-LEVEL-INIT-OFF  VALUE '0'.
               88  KONTORD-LEVEL-INIT      VALUE '1'.
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
           05  KONTFAK-LEVEL-01.
               10  KONTFAK-01-L1.
                   15  KONTFAK-01-L1-FIRMA PICTURE X(3).
           05  KONTFAK-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  ORDNR                   PICTURE X(6).
           05  KONTFAK-MP                  PICTURE X(9).
           05  KONTFAK-MC                  PICTURE X(9).
           05  KONTFAK-M-01            REDEFINES KONTFAK-MC.
               10  KONTFAK-M-01-M2.
                   15  KONTFAK-M-01-M2-FIRMA-G.
                       20  KONTFAK-M-01-M2-FIRMA PICTURE X(3).
               10  KONTFAK-M-01-M1.
                   15  KONTFAK-M-01-M1-ORDNR-G.
                       20  KONTFAK-M-01-M1-ORDNR PICTURE X(6).
           05  KONTORD-LEVEL-02.
               10  KONTORD-02-L1.
                   15  KONTORD-02-L1-FIRMA PICTURE X(3).
           05  KONTORD-LEVEL-03.
               10  KONTORD-03-L1.
                   15  KONTORD-03-L1-FIRMA PICTURE X(3).
           05  KONTORD-DATA-FIELDS.
               10  REC02                   PICTURE X(40).
               10  REC03                   PICTURE X(40).
      ************************************************************
      * HOVEDRUTINE.                                             *
      ************************************************************
           05  KONTORD-MP                  PICTURE X(9).
           05  KONTORD-MC                  PICTURE X(9).
           05  KONTORD-M-02            REDEFINES KONTORD-MC.
               10  KONTORD-M-02-M2.
                   15  KONTORD-M-02-M2-FIRMA-G.
                       20  KONTORD-M-02-M2-FIRMA PICTURE X(3).
               10  KONTORD-M-02-M1.
                   15  KONTORD-M-02-M1-ORDNR-G.
                       20  KONTORD-M-02-M1-ORDNR PICTURE X(6).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  ANT02-IO.
                   15  ANT02               PICTURE S9(5).
               10  ANT02S-IO.
                   15  ANT02S              PICTURE S9(5).
               10  TOT02-IO.
                   15  TOT02               PICTURE S9(5).
               10  TOT02S-IO.
                   15  TOT02S              PICTURE S9(5).
               10  ANT03-IO.
                   15  ANT03               PICTURE S9(5).
               10  TOT03-IO.
                   15  TOT03               PICTURE S9(5).
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
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
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
 
           IF  KONTORD-PROCESS
               SET KONTORD-PROCESS-OFF     TO TRUE
               SET KONTORD-READ            TO TRUE
           END-IF
 
           IF  KONTORD-READ
               PERFORM KONTORD-GET
               SET KONTORD-READ-OFF        TO TRUE
               IF  NOT KONTORD-EOF
                   PERFORM KONTORD-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM KONTORD-MATCH-SET
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
 
           IF  KONTFAK-PROCESS
               PERFORM KONTFAK-IDSET
           END-IF
 
           IF  KONTORD-PROCESS
               PERFORM KONTORD-IDSET
           END-IF
 
           IF  KONTFAK-PROCESS
               PERFORM KONTFAK-CHK-LEVEL
           END-IF
 
           IF  KONTORD-PROCESS
               PERFORM KONTORD-CHK-LEVEL
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
 
           IF  KONTFAK-PROCESS
               PERFORM KONTFAK-FLDSET
           END-IF
 
           IF  KONTORD-PROCESS
               PERFORM KONTORD-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  KONTFAK-PROCESS
           OR  KONTORD-PROCESS
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
               GO TO SLUTT-T
           END-IF
           IF  (I-02)
               ADD 1                       TO ANT02
           END-IF
           IF  (I-02 AND I-MR)
               ADD 1                       TO ANT02S
           END-IF
           IF  (I-02)
               ADD 1                       TO TOT02
           END-IF
           IF  (I-02 AND I-MR)
               ADD 1                       TO TOT02S
           END-IF
           IF  (I-02)
               GO TO SLUTT-T
           END-IF
           IF  (I-03)
               ADD 1                       TO ANT03
               ADD 1                       TO TOT03
           END-IF.
 
       SLUTT-T.
      *****************************************************************
      * NY KONTANT.ORDRE FILE SEQ.KOPI                                *
      *****************************************************************
           CONTINUE.
 
       KONTFAK-GET SECTION.
       KONTFAK-GET-P.
           IF  KONTFAK-EOF-OFF
               READ KONTFAK
               AT END
                   SET KONTFAK-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KONTFAK-FLDSET SECTION.
       KONTFAK-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KONTFAK-IO-AREA (1:3)  TO FIRMA (1:3)
               MOVE KONTFAK-IO-AREA (4:6)  TO ORDNR (1:6)
           END-EVALUATE.
 
       KONTFAK-IDSET SECTION.
       KONTFAK-IDSET-P.
           SET I-01                        TO TRUE.
 
       KONTFAK-CHK-LEVEL SECTION.
       KONTFAK-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO KONTFAK-LEVEL-01
               MOVE KONTFAK-IO-AREA (1:3)  TO KONTFAK-01-L1-FIRMA
               IF  KONTFAK-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  KONTFAK-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  KONTFAK-01-L1         TO THE-PRIOR-L1
               SET KONTFAK-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       KONTFAK-MATCH-SET SECTION.
       KONTFAK-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE KONTFAK-IO-AREA (1:3)  TO KONTFAK-M-01-M2-FIRMA
               MOVE KONTFAK-IO-AREA (4:6)  TO KONTFAK-M-01-M1-ORDNR
           END-EVALUATE.
 
       KONTORD-GET SECTION.
       KONTORD-GET-P.
           IF  KONTORD-EOF-OFF
               READ KONTORD
               AT END
                   SET KONTORD-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KONTORD-FLDSET SECTION.
       KONTORD-FLDSET-P.
           EVALUATE TRUE
           WHEN ( KONTORD-IO-AREA (4:1) = '0' )
               MOVE KONTORD-IO-AREA (1:3)  TO FIRMA (1:3)
               MOVE KONTORD-IO-AREA (5:6)  TO ORDNR (1:6)
               MOVE KONTORD-IO-AREA (1:40) TO REC02 (1:40)
           WHEN ( KONTORD-IO-AREA (4:1) NOT = '0' )
               MOVE KONTORD-IO-AREA (1:3)  TO FIRMA (1:3)
               MOVE KONTORD-IO-AREA (1:40) TO REC03 (1:40)
           END-EVALUATE.
 
       KONTORD-IDCHK SECTION.
       KONTORD-IDCHK-P.
           EVALUATE TRUE
           WHEN ( KONTORD-IO-AREA (4:1) = '0' )
             OR ( KONTORD-IO-AREA (4:1) NOT = '0' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       KONTORD-IDSET SECTION.
       KONTORD-IDSET-P.
           EVALUATE TRUE
           WHEN ( KONTORD-IO-AREA (4:1) = '0' )
               SET I-02                    TO TRUE
           WHEN ( KONTORD-IO-AREA (4:1) NOT = '0' )
               SET I-03                    TO TRUE
           END-EVALUATE.
 
       KONTORD-CHK-LEVEL SECTION.
       KONTORD-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( KONTORD-IO-AREA (4:1) = '0' )
               MOVE LOW-VALUES             TO KONTORD-LEVEL-02
               MOVE KONTORD-IO-AREA (1:3)  TO KONTORD-02-L1-FIRMA
               IF  KONTORD-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  KONTORD-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  KONTORD-02-L1         TO THE-PRIOR-L1
               SET KONTORD-LEVEL-INIT      TO TRUE
           WHEN ( KONTORD-IO-AREA (4:1) NOT = '0' )
               MOVE LOW-VALUES             TO KONTORD-LEVEL-03
               MOVE KONTORD-IO-AREA (1:3)  TO KONTORD-03-L1-FIRMA
               IF  KONTORD-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  KONTORD-03-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  KONTORD-03-L1         TO THE-PRIOR-L1
               SET KONTORD-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       KONTORD-MATCH-SET SECTION.
       KONTORD-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( KONTORD-IO-AREA (4:1) = '0' )
               MOVE KONTORD-IO-AREA (1:3)  TO KONTORD-M-02-M2-FIRMA
               MOVE KONTORD-IO-AREA (5:6)  TO KONTORD-M-02-M1-ORDNR
           WHEN ( KONTORD-IO-AREA (4:1) NOT = '0' )
               SET NOT-CALL-MATCH-RECS     TO TRUE
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
           IF  KONTFAK-EOF
               MOVE HIGH-VALUES            TO KONTFAK-MC
                                              KONTFAK-MP
           END-IF
           IF  KONTORD-EOF
               MOVE HIGH-VALUES            TO KONTORD-MC
                                              KONTORD-MP
           END-IF
           IF  KONTFAK-MC < KONTFAK-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  KONTORD-MC < KONTORD-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  KONTFAK-MC < KONTORD-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET KONTFAK-PROCESS     TO TRUE
                   MOVE KONTFAK-MC         TO KONTFAK-MP
                   IF  KONTFAK-MC = KONTORD-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  KONTORD-MC < KONTFAK-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET KONTORD-PROCESS     TO TRUE
                   MOVE KONTORD-MC         TO KONTORD-MP
                   IF  KONTORD-MC = KONTFAK-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  KONTFAK-MC = KONTORD-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET KONTFAK-PROCESS     TO TRUE
                   MOVE KONTFAK-MC         TO KONTFAK-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND NOT-I-MR)
               MOVE SPACES TO UTREC-IO-AREA
               INITIALIZE UTREC-IO-AREA
               MOVE REC02                  TO UTREC-IO-AREA (1:40)
               WRITE UTREC-IO-AREA
           END-IF
           IF  (I-03)
               MOVE SPACES TO UTREC-IO-AREA
               INITIALIZE UTREC-IO-AREA
               MOVE REC03                  TO UTREC-IO-AREA (1:40)
      *****************************************************************
      * TOTALSUMMER PR. FIRMA OG GRANDTOTAL.                          *
      *****************************************************************
               WRITE UTREC-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '***  STATESTIKK  OVER' TO LISTE-IO-AREA (4:21)
               MOVE 'ANTALL  RECORDS PÅ KONTA' TO LISTE-IO-AREA (27:24)
               MOVE 'NT.ORDRE.FILE   ***'  TO LISTE-IO-AREA (51:19)
               MOVE 'DATO='                TO LISTE-IO-AREA (80:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (85:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMA'                TO LISTE-IO-AREA (1:5)
               MOVE 'ANT.O-REC'            TO LISTE-IO-AREA (12:9)
               MOVE 'ANT.S-REC'            TO LISTE-IO-AREA (32:9)
               MOVE 'ANT.SLETTET'          TO LISTE-IO-AREA (50:11)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OV)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '***  STATESTIKK  OVER' TO LISTE-IO-AREA (4:21)
               MOVE 'ANTALL  RECORDS PÅ KONTA' TO LISTE-IO-AREA (27:24)
               MOVE 'NT.ORDRE.FILE   ***'  TO LISTE-IO-AREA (51:19)
               MOVE 'DATO='                TO LISTE-IO-AREA (80:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (85:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMA'                TO LISTE-IO-AREA (1:5)
               MOVE 'ANT.O-REC'            TO LISTE-IO-AREA (12:9)
               MOVE 'ANT.S-REC'            TO LISTE-IO-AREA (32:9)
               MOVE 'ANT.SLETTET'          TO LISTE-IO-AREA (50:11)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FIRMA                  TO LISTE-IO-AREA (3:3)
               MOVE ANT02                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (15:6)
               INITIALIZE ANT02
               MOVE ANT03                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (35:6)
               INITIALIZE ANT03
               MOVE ANT02S                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (55:6)
               INITIALIZE ANT02S
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '***'                  TO LISTE-IO-AREA (3:3)
               MOVE TOT02                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (15:6)
               MOVE TOT03                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (35:6)
               MOVE TOT02S                 TO XO-50YY9
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
           SET KONTFAK-LEVEL-INIT          TO TRUE
           INITIALIZE KONTFAK-DATA-FIELDS
           SET KONTFAK-EOF-OFF             TO TRUE
           SET KONTFAK-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO KONTFAK-MC
                                              KONTFAK-MP
           OPEN INPUT KONTFAK
           SET KONTORD-LEVEL-INIT          TO TRUE
           INITIALIZE KONTORD-DATA-FIELDS
           SET KONTORD-EOF-OFF             TO TRUE
           SET KONTORD-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO KONTORD-MC
                                              KONTORD-MP
           OPEN INPUT KONTORD
           OPEN OUTPUT UTREC
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE KONTFAK
           CLOSE KONTORD
           CLOSE UTREC
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
