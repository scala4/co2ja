       IDENTIFICATION DIVISION.
       PROGRAM-ID. RKO038R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM: RKO038, ENDRER DATO I HHT TABELL                    *
      *  ENDR...:                                                     *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RKO038.rpg
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
           SELECT GMLRESK
               ASSIGN TO UT-S-GMLRESK
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS GMLRESK-STATUS.
           SELECT OUTRESK
               ASSIGN TO UT-S-OUTRESK
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTRESK-STATUS.
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
       FD GMLRESK
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  GMLRESK-IO-AREA.
           05  GMLRESK-IO-AREA-X           PICTURE X(200).
       FD OUTRESK
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  OUTRESK-IO-AREA.
           05  OUTRESK-IO-AREA-X           PICTURE X(200).
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
           10  GMLRESK-STATUS              PICTURE 99 VALUE 0.
           10  OUTRESK-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  RESTAB-EOF-OFF          VALUE '0'.
               88  RESTAB-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLRESK-EOF-OFF         VALUE '0'.
               88  GMLRESK-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLRESK-READ-OFF        VALUE '0'.
               88  GMLRESK-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLRESK-PROCESS-OFF     VALUE '0'.
               88  GMLRESK-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  GMLRESK-LEVEL-INIT-OFF  VALUE '0'.
               88  GMLRESK-LEVEL-INIT      VALUE '1'.
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
           05  GMLRESK-LEVEL-01.
               10  GMLRESK-01-L2.
                   15  GMLRESK-01-L2-FNR1  PICTURE X(3).
           05  GMLRESK-DATA-FIELDS.
               10  REC200                  PICTURE X(200).
               10  REC1H                   PICTURE X(100).
               10  REC2H                   PICTURE X(100).
               10  FNR1                    PICTURE X(3).
               10  BILNR                   PICTURE X(6).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  ANTLES-IO.
                   15  ANTLES              PICTURE S9(9).
               10  FNRBNR-IO.
                   15  FNRBNR              PICTURE S9(9).
               10  NBILDT                  PICTURE X(6).
               10  NFFDTO                  PICTURE X(6).
               10  ANTKOR-IO.
                   15  ANTKOR              PICTURE S9(9).
           05  EDITTING-FIELDS.
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
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  GMLRESK-PROCESS
               SET GMLRESK-PROCESS-OFF     TO TRUE
               SET GMLRESK-READ            TO TRUE
           END-IF
 
           IF  GMLRESK-READ
           AND RECORD-SELECTED-OFF
               PERFORM GMLRESK-GET
               SET GMLRESK-READ-OFF        TO TRUE
               IF  NOT GMLRESK-EOF
                   SET GMLRESK-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  GMLRESK-PROCESS
               PERFORM GMLRESK-IDSET
           END-IF
 
           IF  GMLRESK-PROCESS
               PERFORM GMLRESK-CHK-LEVEL
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
 
           IF  GMLRESK-PROCESS
               PERFORM GMLRESK-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  GMLRESK-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           ADD 1                           TO ANTLES
           MOVE FNR1                       TO FNRBNR (1:3)
           MOVE BILNR                      TO FNRBNR-IO (4:6)
           SET NOT-I-20                    TO TRUE
           SET TABBNR-S                    TO TABBNR-I
           PERFORM WITH TEST AFTER
                   VARYING TABBNR-I FROM 1 BY 1
                     UNTIL TABBNR-I >= TABBNR-MAX
                        OR I-20
               IF  FNRBNR = TABBNR (TABBNR-I)
                   SET I-20                TO TRUE
                   SET TABBNR-S            TO TABBNR-I
               END-IF
           END-PERFORM
           SET TABBNR-I                    TO TABBNR-S
           IF  I-20
           AND TABBNR-I NOT > TABDTO-MAX
               SET TABDTO-I                TO TABBNR-I
           END-IF
           IF  (NOT-I-20)
               GO TO SLUTT-T
           END-IF
           MOVE TABDTO(TABDTO-I) (1:6)     TO NBILDT
           MOVE TABDTO(TABDTO-I) (7:6)     TO NFFDTO
           ADD 1                           TO ANTKOR.
 
       SLUTT-T.
      *
           CONTINUE.
 
       GMLRESK-GET SECTION.
       GMLRESK-GET-P.
           IF  GMLRESK-EOF-OFF
               READ GMLRESK
               AT END
                   SET GMLRESK-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       GMLRESK-FLDSET SECTION.
       GMLRESK-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE GMLRESK-IO-AREA (1:200) TO REC200 (1:200)
               MOVE GMLRESK-IO-AREA (1:100) TO REC1H (1:100)
               MOVE GMLRESK-IO-AREA (101:100) TO REC2H (1:100)
               MOVE GMLRESK-IO-AREA (3:3)  TO FNR1 (1:3)
               MOVE GMLRESK-IO-AREA (30:6) TO BILNR (1:6)
           END-EVALUATE.
 
       GMLRESK-IDSET SECTION.
       GMLRESK-IDSET-P.
           SET I-01                        TO TRUE.
 
       GMLRESK-CHK-LEVEL SECTION.
       GMLRESK-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO GMLRESK-LEVEL-01
               MOVE GMLRESK-IO-AREA (3:3)  TO GMLRESK-01-L2-FNR1
               IF  GMLRESK-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  GMLRESK-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   END-EVALUATE
               END-IF
               MOVE  GMLRESK-01-L2         TO THE-PRIOR-L2
               SET GMLRESK-LEVEL-INIT      TO TRUE
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
               MOVE SPACES TO OUTRESK-IO-AREA
               INITIALIZE OUTRESK-IO-AREA
               MOVE REC200                 TO OUTRESK-IO-AREA (1:200)
               IF  (I-20)
                   MOVE NBILDT             TO OUTRESK-IO-AREA (24:6)
               END-IF
               IF  (I-20)
                   MOVE NFFDTO             TO OUTRESK-IO-AREA (42:6)
               END-IF
               WRITE OUTRESK-IO-AREA
           END-IF
           IF  (I-01 AND I-20)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE REC1H                  TO LISTE-IO-AREA (1:100)
               MOVE 'POS 101-200 FØR ENDRING ' TO LISTE-IO-AREA
                                                              (102:24)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE REC2H                  TO LISTE-IO-AREA (1:100)
               MOVE 'POS 101-200 FØR ENDRING ' TO LISTE-IO-AREA
                                                              (102:24)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE REC1H                  TO LISTE-IO-AREA (1:100)
               MOVE NBILDT                 TO LISTE-IO-AREA (24:6)
               MOVE NFFDTO                 TO LISTE-IO-AREA (42:6)
               MOVE 'POS 001-100 ETTER ENDR. ' TO LISTE-IO-AREA
                                                              (102:24)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE REC2H                  TO LISTE-IO-AREA (1:100)
               MOVE 'POS 101-200 ETTER ENDR. ' TO LISTE-IO-AREA
                                                              (102:24)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'PROGRAM RKO038          ' TO LISTE-IO-AREA (1:24)
               MOVE 'KORRIGERING AV BILAGS OG' TO LISTE-IO-AREA (25:24)
               MOVE 'FORFALLSDATO     RKO038 ' TO LISTE-IO-AREA (49:24)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (73:8)
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
               MOVE 2                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'PROGRAM RKO038          ' TO LISTE-IO-AREA (1:24)
               MOVE 'KORRIGERING AV BILAGS OG' TO LISTE-IO-AREA (25:24)
               MOVE 'FORFALLSDATO     RKO038 ' TO LISTE-IO-AREA (49:24)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (73:8)
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
           SET GMLRESK-LEVEL-INIT          TO TRUE
           INITIALIZE GMLRESK-DATA-FIELDS
           SET GMLRESK-EOF-OFF             TO TRUE
           SET GMLRESK-PROCESS             TO TRUE
           OPEN INPUT GMLRESK
           OPEN OUTPUT OUTRESK
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           SET TABBNR-I                    TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE GMLRESK
           CLOSE OUTRESK
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
