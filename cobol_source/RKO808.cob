       IDENTIFICATION DIVISION.
       PROGRAM-ID. RKO808R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM: RKO808, ENDRER DATO ++ I HHT TABELL                 *
      *  ENDR...: LAGT INN PERIODE 201108 PÅ ALLE MED FEIL I DATO FOR *
      *           SOGB FIRMA (FEIL FRA EYE-SHARE)                     *
      *           29.12.16 UTVIDET LINJENR I KTOKUR                   *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RKO808.rpg
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
           SELECT RESTAB
               ASSIGN TO UT-S-RESTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RESTAB-STATUS.
           SELECT KTOKURI
               ASSIGN TO UT-S-KTOKURI
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KTOKURI-STATUS.
           SELECT KTOKURO
               ASSIGN TO UT-S-KTOKURO
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KTOKURO-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD RESTAB
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  RESTAB-IO-AREA.
           05  RESTAB-IO-AREA-X            PICTURE X(80).
       FD KTOKURI
               BLOCK CONTAINS 1200
               RECORD CONTAINS 120.
       01  KTOKURI-IO-AREA.
           05  KTOKURI-IO-AREA-X           PICTURE X(120).
       FD KTOKURO
               BLOCK CONTAINS 1200
               RECORD CONTAINS 120.
       01  KTOKURO-IO-AREA.
           05  KTOKURO-IO-AREA-X           PICTURE X(120).
      *BUGFILO O   F  80  80            PRINTERSYSLST
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       WORKING-STORAGE SECTION.
       77  TABBNR-MAX   VALUE 20           PICTURE 9(4) USAGE BINARY.
       77  TABDTO-MAX   VALUE 20           PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  TABBNR-TABLE.
               10  TABBNR-ENTRY
                                           OCCURS 20 TIMES
                                           INDEXED BY TABBNR-I
                                                      TABBNR-S
                                                      TABDTO-I
                                                      TABDTO-S.
                   15  TABBNR              PICTURE S9(9).
                   15  TABDTO              PICTURE X(12).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  RESTAB-STATUS               PICTURE 99 VALUE 0.
           10  KTOKURI-STATUS              PICTURE 99 VALUE 0.
           10  KTOKURO-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  RESTAB-EOF-OFF          VALUE '0'.
               88  RESTAB-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KTOKURI-EOF-OFF         VALUE '0'.
               88  KTOKURI-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KTOKURI-READ-OFF        VALUE '0'.
               88  KTOKURI-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KTOKURI-PROCESS-OFF     VALUE '0'.
               88  KTOKURI-PROCESS         VALUE '1'.
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
           05  KTOKURI-DATA-FIELDS.
               10  REC120                  PICTURE X(120).
               10  FNR1                    PICTURE X(3).
               10  GBILDT-IO.
                   15  GBILDT              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  BILNRP-IO.
                   15  BILNRP              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  GFFDTO-IO.
                   15  GFFDTO              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  GMBPR                   PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  ANTLES-IO.
                   15  ANTLES              PICTURE S9(9).
               10  FNRBNR-IO.
                   15  FNRBNR              PICTURE S9(9).
               10  BILNRN-IO.
                   15  BILNRN              PICTURE S9(6).
               10  BILNRP-N-IO.
                   15  BILNRP-N            PICTURE S9(7).
               10  BILNRX                  PICTURE X(6).
               10  NBILDT-IO.
                   15  NBILDT              PICTURE S9(6).
               10  NFFDTO-IO.
                   15  NFFDTO              PICTURE S9(6).
               10  BILP4                   PICTURE X(4).
               10  BILPR-IO.
                   15  BILPR               PICTURE S9(6).
               10  NYBPR-IO.
                   15  NYBPR               PICTURE S9(6).
               10  ANTKOR-IO.
                   15  ANTKOR              PICTURE S9(9).
           05  EDITTING-FIELDS.
               10  XO-60P-EF.
                 15  XO-60P                PICTURE S9(6) USAGE
                                                       PACKED-DECIMAL.
               10  XO-70D                  PICTURE S9(7).
               10  XO-70U                  PICTURE 9(7).
               10  XO-90YY9R               PICTURE ZZZ.ZZZ.ZZ9-.
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
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  KTOKURI-PROCESS
               SET KTOKURI-PROCESS-OFF     TO TRUE
               SET KTOKURI-READ            TO TRUE
           END-IF
 
           IF  KTOKURI-READ
           AND RECORD-SELECTED-OFF
               PERFORM KTOKURI-GET
               SET KTOKURI-READ-OFF        TO TRUE
               IF  NOT KTOKURI-EOF
                   SET KTOKURI-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  KTOKURI-PROCESS
               PERFORM KTOKURI-IDSET
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
 
           IF  KTOKURI-PROCESS
               PERFORM KTOKURI-FLDSET
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
           ADD 1                           TO ANTLES
           MOVE FNR1                       TO FNRBNR (1:3)
           MOVE BILNRP                     TO BILNRP-N
           MOVE BILNRP-N-IO (2:6)          TO BILNRN-IO
           MOVE BILNRN                     TO BILNRX
           MOVE BILNRX                     TO FNRBNR-IO (4:6)
      *                    MOVE "FNRBNR  "BUGFL2  8        LEDETXT DEBUG
      *          BUGFL2    DEBUGBUGFILO   FNRBNR           VIS FELT/IND
           SET NOT-I-10                    TO TRUE
           SET TABBNR-S                    TO TABBNR-I
           PERFORM WITH TEST AFTER
                   VARYING TABBNR-I FROM 1 BY 1
                     UNTIL TABBNR-I >= TABBNR-MAX
                        OR I-10
               IF  FNRBNR = TABBNR (TABBNR-I)
                   SET I-10                TO TRUE
                   SET TABBNR-S            TO TABBNR-I
               END-IF
           END-PERFORM
           SET TABBNR-I                    TO TABBNR-S
           IF  I-10
           AND TABBNR-I NOT > TABDTO-MAX
               SET TABDTO-I                TO TABBNR-I
           END-IF
           IF  (NOT-I-10)
               GO TO SLUTT-T
           END-IF
           MOVE TABDTO(TABDTO-I) (1:6)     TO NBILDT
           MOVE TABDTO(TABDTO-I) (7:6)     TO NFFDTO-IO
           MOVE NBILDT (1:4)               TO BILP4
           MOVE '20'                       TO BILPR (1:2)
           MOVE BILP4                      TO BILPR-IO (3:4)
           ADD BILPR TO ZERO           GIVING NYBPR
           MOVE 201108                     TO NYBPR
           ADD 1                           TO ANTKOR.
 
       SLUTT-T.
      *
           CONTINUE.
 
       KTOKURI-GET SECTION.
       KTOKURI-GET-P.
           IF  KTOKURI-EOF-OFF
               READ KTOKURI
               AT END
                   SET KTOKURI-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KTOKURI-FLDSET SECTION.
       KTOKURI-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KTOKURI-IO-AREA (1:120) TO REC120 (1:120)
               MOVE KTOKURI-IO-AREA (1:3)  TO FNR1 (1:3)
               MOVE KTOKURI-IO-AREA (19:4) TO GBILDT-IO
               MOVE KTOKURI-IO-AREA (23:4) TO BILNRP-IO
               MOVE KTOKURI-IO-AREA (31:4) TO GFFDTO-IO
               MOVE KTOKURI-IO-AREA (59:6) TO GMBPR (1:6)
           END-EVALUATE.
 
       KTOKURI-IDSET SECTION.
       KTOKURI-IDSET-P.
           SET I-01                        TO TRUE.
 
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
 
       RESTAB-LOAD SECTION.
       RESTAB-LOAD-P.
           OPEN INPUT RESTAB
           SET TABBNR-I                    TO 1
           PERFORM UNTIL RESTAB-EOF
               READ RESTAB
               AT END
                   SET RESTAB-EOF          TO TRUE
               NOT AT END
                   MOVE RESTAB-IO-AREA (1:21) TO TABBNR-ENTRY
                                                            (TABBNR-I)
                   SET TABBNR-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE RESTAB.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               MOVE SPACES TO KTOKURO-IO-AREA
               INITIALIZE KTOKURO-IO-AREA
               MOVE REC120                 TO KTOKURO-IO-AREA (1:120)
               IF  (I-10)
                   MOVE NBILDT             TO XO-60P
                   MOVE XO-60P-EF          TO KTOKURO-IO-AREA (19:4)
               END-IF
               IF  (I-10)
                   MOVE NFFDTO             TO XO-60P
                   MOVE XO-60P-EF          TO KTOKURO-IO-AREA (31:4)
               END-IF
               IF  (I-10)
                   MOVE '20'               TO KTOKURO-IO-AREA (57:2)
               END-IF
               IF  (I-10)
                   MOVE NYBPR-IO           TO KTOKURO-IO-AREA (59:6)
               END-IF
               WRITE KTOKURO-IO-AREA
           END-IF
           IF  (I-01 AND I-10)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE REC120                 TO LISTE-IO-AREA (1:120)
               MOVE 'FØR   '               TO LISTE-IO-AREA (125:6)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
      *                        REC120   120
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE GBILDT                 TO XO-70U
               MOVE XO-70U (1:7)           TO LISTE-IO-AREA (16:7)
               IF GFFDTO < 0
                 MOVE GFFDTO               TO XO-70D
                 MOVE XO-70D (1:7)         TO LISTE-IO-AREA (28:7)
               ELSE
                 MOVE GFFDTO               TO XO-70U
                 MOVE XO-70U (1:7)         TO LISTE-IO-AREA (28:7)
               END-IF
               MOVE '20'                   TO LISTE-IO-AREA (57:2)
               MOVE GMBPR                  TO LISTE-IO-AREA (59:6)
               MOVE 'FØR   '               TO LISTE-IO-AREA (125:6)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE REC120                 TO LISTE-IO-AREA (1:120)
               MOVE NBILDT                 TO XO-60P
               MOVE XO-60P-EF              TO LISTE-IO-AREA (19:4)
               MOVE NFFDTO                 TO XO-60P
               MOVE XO-60P-EF              TO LISTE-IO-AREA (31:4)
               MOVE '20'                   TO LISTE-IO-AREA (57:2)
               MOVE NYBPR-IO               TO LISTE-IO-AREA (59:6)
               MOVE 'ETTER '               TO LISTE-IO-AREA (125:6)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
      *                        REC120   120
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE NBILDT-IO              TO LISTE-IO-AREA (17:6)
               MOVE NFFDTO-IO              TO LISTE-IO-AREA (29:6)
               MOVE '20'                   TO LISTE-IO-AREA (57:2)
               MOVE NYBPR-IO               TO LISTE-IO-AREA (59:6)
               MOVE 'ETTER '               TO LISTE-IO-AREA (125:6)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'PROGRAM RKO808          ' TO LISTE-IO-AREA (1:24)
               MOVE 'KORRIGERING AV RESK.NR  ' TO LISTE-IO-AREA (25:24)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '....+...10....+...20' TO LISTE-IO-AREA (1:20)
               MOVE '....+...30....+...40' TO LISTE-IO-AREA (21:20)
               MOVE '....+...50....+...60' TO LISTE-IO-AREA (41:20)
               MOVE '....+...70....+...80' TO LISTE-IO-AREA (61:20)
               MOVE '....+...90....+..100' TO LISTE-IO-AREA (81:20)
               MOVE '....+..120....+..130' TO LISTE-IO-AREA (91:20)
               MOVE 2                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'PROGRAM RKO808          ' TO LISTE-IO-AREA (1:24)
               MOVE 'KORRIGERING AV RESK.NR  ' TO LISTE-IO-AREA (25:24)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '....+...10....+...20' TO LISTE-IO-AREA (1:20)
               MOVE '....+...30....+...40' TO LISTE-IO-AREA (21:20)
               MOVE '....+...50....+...60' TO LISTE-IO-AREA (41:20)
               MOVE '....+...70....+...80' TO LISTE-IO-AREA (61:20)
               MOVE '....+...90....+..100' TO LISTE-IO-AREA (81:20)
               MOVE '....+..120....+..130' TO LISTE-IO-AREA (91:20)
               MOVE 2                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT LEST:'            TO LISTE-IO-AREA (2:9)
               MOVE ANTLES                 TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (19:12)
               MOVE 2                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT KORR:'            TO LISTE-IO-AREA (2:9)
               MOVE ANTKOR                 TO XO-90YY9R
               MOVE XO-90YY9R              TO LISTE-IO-AREA (19:12)
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
           MOVE 1                          TO LR-CHECK
           PERFORM RESTAB-LOAD
           INITIALIZE KTOKURI-DATA-FIELDS
           SET KTOKURI-EOF-OFF             TO TRUE
           SET KTOKURI-PROCESS             TO TRUE
           OPEN INPUT KTOKURI
           OPEN OUTPUT KTOKURO
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           SET TABBNR-I                    TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE KTOKURI
           CLOSE KTOKURO
           IF LISTE-IO-AREA NOT = SPACES
             WRITE LISTE-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE-IO-AREA
           END-IF
           CLOSE LISTE.
 
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
