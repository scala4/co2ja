       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORD185R.
      **********************************************  Z-WIN-RPG2   ****
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ORD185.rpg
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
           SELECT INNFIL
               ASSIGN TO UT-S-INNFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INNFIL-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
           SELECT OUTPUT-X
               ASSIGN TO UT-S-OUTPUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTPUT-X-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD INNFIL
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  INNFIL-IO-AREA.
           05  INNFIL-IO-AREA-X            PICTURE X(200).
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       FD OUTPUT-X
               BLOCK CONTAINS 200
               RECORD CONTAINS 100.
       01  OUTPUT-X-IO-AREA.
           05  OUTPUT-X-IO-AREA-X          PICTURE X(100).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  INNFIL-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  OUTPUT-X-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  INNFIL-EOF-OFF          VALUE '0'.
               88  INNFIL-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNFIL-READ-OFF         VALUE '0'.
               88  INNFIL-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNFIL-PROCESS-OFF      VALUE '0'.
               88  INNFIL-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  INNFIL-LEVEL-INIT-OFF   VALUE '0'.
               88  INNFIL-LEVEL-INIT       VALUE '1'.
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
           05  INNFIL-LEVEL-01.
               10  INNFIL-01-L3.
                   15  INNFIL-01-L3-FIRMA  PICTURE X(3).
               10  INNFIL-01-L2.
                   15  INNFIL-01-L2-KNR    PICTURE X(6).
               10  INNFIL-01-L1.
                   15  INNFIL-01-L1-VGR    PICTURE X(5).
           05  INNFIL-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
      *                                       4   6 ALFA
      *                                       7  26 ARTNR
               10  ANTLEV-IO.
                   15  ANTLEV              PICTURE S9(5)V9(2).
      *                                      64  722OPRIS
               10  ORDNR                   PICTURE X(6).
               10  ORDNR1                  PICTURE X(1).
               10  VGR                     PICTURE X(5).
      *                                     102 107 AAMMDD
               10  KNR                     PICTURE X(6).
               10  SUMBEL-IO.
                   15  SUMBEL              PICTURE S9(7)V9(2).
               10  AAR-IO.
                   15  AAR                 PICTURE S9(2).
               10  MND-IO.
                   15  MND                 PICTURE S9(2).
               10  DAG-IO.
                   15  DAG                 PICTURE S9(2).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(6).
               10  THE-PRIOR-L1            PICTURE X(5).
           05  TEMPORARY-FIELDS.
               10  ANTVGR-IO.
                   15  ANTVGR              PICTURE S9(7)V9(2).
               10  SUMVGR-IO.
                   15  SUMVGR              PICTURE S9(7)V9(2).
               10  ANTVGP-IO.
                   15  ANTVGP              PICTURE S9(7)V9(2).
               10  SUMVGP-IO.
                   15  SUMVGP              PICTURE S9(7)V9(2).
               10  ANTVGN-IO.
                   15  ANTVGN              PICTURE S9(7)V9(2).
               10  SUMVGN-IO.
                   15  SUMVGN              PICTURE S9(7)V9(2).
               10  ANT-IO.
                   15  ANT                 PICTURE S9(6).
               10  DAGORD-IO.
                   15  DAGORD              PICTURE S9(6).
               10  ANT9-IO.
                   15  ANT9                PICTURE S9(6).
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
 
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  INNFIL-PROCESS
               SET INNFIL-PROCESS-OFF      TO TRUE
               SET INNFIL-READ             TO TRUE
           END-IF
 
           IF  INNFIL-READ
           AND RECORD-SELECTED-OFF
               PERFORM INNFIL-GET
               SET INNFIL-READ-OFF         TO TRUE
               IF  NOT INNFIL-EOF
                   SET INNFIL-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  INNFIL-PROCESS
               PERFORM INNFIL-IDSET
           END-IF
 
           IF  INNFIL-PROCESS
               PERFORM INNFIL-CHK-LEVEL
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
 
           IF  INNFIL-PROCESS
               PERFORM INNFIL-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  INNFIL-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-10                    TO TRUE
           SET NOT-I-16                    TO TRUE
           SET NOT-I-17                    TO TRUE
           SET NOT-I-18                    TO TRUE
           SET NOT-I-39                    TO TRUE
           SET NOT-I-40                    TO TRUE
      *  L1                SETOF                     3940
      *****************************************************************
      * NULLSTILLER                                                   *
      *****************************************************************
           IF  (I-L1)
               MOVE 0                      TO ANTVGR
               MOVE 0                      TO SUMVGR
               MOVE 0                      TO ANTVGP
               MOVE 0                      TO SUMVGP
               MOVE 0                      TO ANTVGN
               MOVE 0                      TO SUMVGN
      *****************************************************************
      * SJEKK PÅ FIRMANR                                              *
      *****************************************************************
           END-IF
           SET NOT-I-95                    TO TRUE
           IF  FIRMA = '963'
               SET I-95                    TO TRUE
           END-IF
           IF  (NOT-I-95)
               SET NOT-I-95                TO TRUE
               IF  FIRMA = '604'
                   SET I-95                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-95)
               SET NOT-I-95                TO TRUE
               IF  FIRMA = '615'
                   SET I-95                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-95)
               SET NOT-I-95                TO TRUE
               IF  FIRMA = '634'
                   SET I-95                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-95)
               SET NOT-I-95                TO TRUE
               IF  FIRMA = '690'
                   SET I-95                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-95)
               SET NOT-I-95                TO TRUE
               IF  FIRMA = '717'
                   SET I-95                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-95)
               SET NOT-I-95                TO TRUE
               IF  FIRMA = '738'
                   SET I-95                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-95)
               SET NOT-I-95                TO TRUE
               IF  FIRMA = '842'
                   SET I-95                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-95)
               SET NOT-I-95                TO TRUE
               IF  FIRMA = '912'
                   SET I-95                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-95)
               SET NOT-I-95                TO TRUE
               IF  FIRMA = '447'
                   SET I-95                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-95)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               ADD 1                       TO ANT
      *
           END-IF
           SET NOT-I-25                    TO TRUE
           IF  VGR = '     '
               SET I-25                    TO TRUE
           END-IF
           IF  (NOT-I-25)
               SET NOT-I-25                TO TRUE
               IF  VGR = '00000'
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-25)
               GO TO SLUTT-T
      *
           END-IF
           SET NOT-I-14                    TO TRUE
           IF  AAR = UYEAR
               SET I-14                    TO TRUE
           END-IF
           IF  (NOT-I-14)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-17                    TO TRUE
           IF  MND = UMONTH
               SET I-17                    TO TRUE
           END-IF
           IF  (NOT-I-17)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-18                    TO TRUE
           IF  DAG NOT > UDAY
               SET I-18                    TO TRUE
           END-IF
           IF  (NOT-I-18)
               GO TO SLUTT-T
      *
           END-IF
           SET NOT-I-16                    TO TRUE
           IF  ORDNR1 = '9'
               SET I-16                    TO TRUE
           END-IF
           ADD 1                           TO DAGORD
           IF  (I-16)
               ADD 1                       TO ANT9
      *****************************************************************
      * SUMMERER PR VAREGRUPPE                                        *
      *****************************************************************
           END-IF
           IF  (NOT-I-16)
               ADD ANTLEV                  TO ANTVGP
               ADD SUMBEL                  TO SUMVGP
           END-IF
           IF  (I-16)
               ADD ANTLEV                  TO ANTVGN
               ADD SUMBEL                  TO SUMVGN
           END-IF
           SET I-10                        TO TRUE.
 
       SLUTT-T.
           CONTINUE.
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1)
               SUBTRACT ANTVGN FROM ANTVGP GIVING ANTVGR
               SUBTRACT SUMVGN FROM SUMVGP GIVING SUMVGR
      * SJEKKER FOR NEGATIVE TALL
           END-IF
           IF  (I-L1)
               SET NOT-I-39                TO TRUE
               IF  SUMVGR < 0
                   SET I-39                TO TRUE
               END-IF
               SET NOT-I-40                TO TRUE
               IF  ANTVGR < 0
                   SET I-40                TO TRUE
               END-IF
           END-IF.
 
       INNFIL-GET SECTION.
       INNFIL-GET-P.
           IF  INNFIL-EOF-OFF
               READ INNFIL
               AT END
                   SET INNFIL-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INNFIL-FLDSET SECTION.
       INNFIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INNFIL-IO-AREA (1:3)   TO FIRMA (1:3)
               MOVE INNFIL-IO-AREA (57:7)  TO ANTLEV-IO
               INSPECT ANTLEV-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (73:6)  TO ORDNR (1:6)
               MOVE INNFIL-IO-AREA (73:1)  TO ORDNR1 (1:1)
               MOVE INNFIL-IO-AREA (79:5)  TO VGR (1:5)
               MOVE INNFIL-IO-AREA (108:6) TO KNR (1:6)
               MOVE INNFIL-IO-AREA (150:9) TO SUMBEL-IO
               INSPECT SUMBEL-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (161:2) TO AAR-IO
               INSPECT AAR-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (163:2) TO MND-IO
               INSPECT MND-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (165:2) TO DAG-IO
               INSPECT DAG-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       INNFIL-IDSET SECTION.
       INNFIL-IDSET-P.
           SET I-01                        TO TRUE.
 
       INNFIL-CHK-LEVEL SECTION.
       INNFIL-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO INNFIL-LEVEL-01
               MOVE INNFIL-IO-AREA (1:3)   TO INNFIL-01-L3-FIRMA
               MOVE INNFIL-IO-AREA (108:6) TO INNFIL-01-L2-KNR
               MOVE INNFIL-IO-AREA (79:5)  TO INNFIL-01-L1-VGR
               IF  INNFIL-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INNFIL-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  INNFIL-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INNFIL-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INNFIL-01-L3          TO THE-PRIOR-L3
               MOVE  INNFIL-01-L2          TO THE-PRIOR-L2
               MOVE  INNFIL-01-L1          TO THE-PRIOR-L1
               SET INNFIL-LEVEL-INIT       TO TRUE
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
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-10)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FIRMA                  TO LISTE-IO-AREA (1:3)
               MOVE KNR                    TO LISTE-IO-AREA (5:6)
               MOVE VGR                    TO LISTE-IO-AREA (12:5)
               IF  (I-40)
                   MOVE '-'                TO LISTE-IO-AREA (17:1)
               END-IF
               MOVE ANTVGR-IO              TO LISTE-IO-AREA (20:9)
               IF  (I-39)
                   MOVE '-'                TO LISTE-IO-AREA (29:1)
               END-IF
               MOVE SUMVGR-IO              TO LISTE-IO-AREA (32:9)
               MOVE ORDNR                  TO LISTE-IO-AREA (43:6)
               MOVE AAR-IO                 TO LISTE-IO-AREA (51:2)
               MOVE MND-IO                 TO LISTE-IO-AREA (54:2)
               MOVE DAG-IO                 TO LISTE-IO-AREA (57:2)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANT                    TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (4:7)
               MOVE 'ORDRE TOTALT.          ' TO LISTE-IO-AREA (14:23)
               MOVE 2                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE DAGORD                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (4:7)
               MOVE 'ORDRE    . I DAG.      ' TO LISTE-IO-AREA (14:23)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANT9                   TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (4:7)
               MOVE 'ORDRE KRED I DAG.      ' TO LISTE-IO-AREA (14:23)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L1 AND I-10)
               MOVE SPACES TO OUTPUT-X-IO-AREA
               INITIALIZE OUTPUT-X-IO-AREA
               MOVE FIRMA                  TO OUTPUT-X-IO-AREA (1:3)
               MOVE KNR                    TO OUTPUT-X-IO-AREA (4:6)
               MOVE VGR                    TO OUTPUT-X-IO-AREA (10:5)
               MOVE ANTVGR-IO              TO OUTPUT-X-IO-AREA (15:9)
               MOVE SUMVGR-IO              TO OUTPUT-X-IO-AREA (24:9)
               MOVE '  '                   TO OUTPUT-X-IO-AREA (33:2)
               IF  (I-40)
                   MOVE '-'                TO OUTPUT-X-IO-AREA (33:1)
               END-IF
               IF  (I-39)
                   MOVE '-'                TO OUTPUT-X-IO-AREA (34:1)
               END-IF
               WRITE OUTPUT-X-IO-AREA
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
           SET INNFIL-LEVEL-INIT           TO TRUE
           INITIALIZE INNFIL-DATA-FIELDS
           SET INNFIL-EOF-OFF              TO TRUE
           SET INNFIL-PROCESS              TO TRUE
           OPEN INPUT INNFIL
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES
           OPEN OUTPUT OUTPUT-X.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INNFIL
           IF LISTE-IO-AREA NOT = SPACES
             WRITE LISTE-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE-IO-AREA
           END-IF
           CLOSE LISTE
           CLOSE OUTPUT-X.
 
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
