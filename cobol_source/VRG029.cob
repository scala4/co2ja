       IDENTIFICATION DIVISION.
       PROGRAM-ID. VRG029R.
      **********************************************  Z-WIN-RPG2   ****
      *ENDR.: 07.12.98 TAKLER ÅR 2000.
      *                UPSI-1 ER PÅ: PRINT ÅRSTALL.
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VRG029.rpg
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
           SELECT BUDMAS
               ASSIGN TO BUDMAS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS BUDMAS-STATUS
               RECORD KEY IS BUDMAS-KEY1.
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
       FD BUDMAS
               RECORD CONTAINS 82.
       01  BUDMAS-IO-AREA.
           05  BUDMAS-IO-AREA-X.
               10  BUDMAS-KEY1.
                   15  BUDMAS-KEY1N        PICTURE S9(14).
               10  FILLER                  PICTURE X(68).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD OUTFILE
               BLOCK CONTAINS 7790
               RECORD CONTAINS 82.
       01  OUTFILE-IO-AREA.
           05  OUTFILE-IO-AREA-X           PICTURE X(82).
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
           10  BUDMAS-STATUS               PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  OUTFILE-STATUS              PICTURE 99 VALUE 0.
           10  PRINT-X-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  BUDMAS-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  BUDMAS-EOF-OFF          VALUE '0'.
               88  BUDMAS-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BUDMAS-READ-OFF         VALUE '0'.
               88  BUDMAS-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BUDMAS-PROCESS-OFF      VALUE '0'.
               88  BUDMAS-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  BUDMAS-LEVEL-INIT-OFF   VALUE '0'.
               88  BUDMAS-LEVEL-INIT       VALUE '1'.
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
           05  BUDMAS-LEVEL-01.
               10  BUDMAS-01-L1.
                   15  BUDMAS-01-L1-FIRMA  PICTURE X(3).
           05  BUDMAS-DATA-FIELDS.
               10  RECORD-X                PICTURE X(82).
               10  ART                     PICTURE X(1).
               10  FIRMA                   PICTURE X(3).
               10  AARF                    PICTURE X(2).
           05  FIRMAF-DATA-FIELDS.
               10  FINAVN                  PICTURE X(30).
               10  FIRMSL                  PICTURE X(1).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  AARLIM-IO.
                   15  AARLIM              PICTURE S9(4).
               10  AAR-IO.
                   15  AAR                 PICTURE S9(2).
               10  AAR4-IO.
                   15  AAR4                PICTURE S9(4).
               10  ANTL1-IO.
                   15  ANTL1               PICTURE S9(7).
               10  ANTL1K-IO.
                   15  ANTL1K              PICTURE S9(7).
               10  ANTLR-IO.
                   15  ANTLR               PICTURE S9(7).
               10  ANTLRK-IO.
                   15  ANTLRK              PICTURE S9(7).
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
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  BUDMAS-PROCESS
               SET BUDMAS-PROCESS-OFF      TO TRUE
               SET BUDMAS-READ             TO TRUE
           END-IF
 
           IF  BUDMAS-READ
           AND RECORD-SELECTED-OFF
               PERFORM BUDMAS-GET
               SET BUDMAS-READ-OFF         TO TRUE
               IF  NOT BUDMAS-EOF
                   SET BUDMAS-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  BUDMAS-PROCESS
               PERFORM BUDMAS-IDSET
           END-IF
 
           IF  BUDMAS-PROCESS
               PERFORM BUDMAS-CHK-LEVEL
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
 
           IF  BUDMAS-PROCESS
               PERFORM BUDMAS-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  BUDMAS-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-90                    TO TRUE
           IF  (I-L1)
               PERFORM FISLET-S
           END-IF
           IF  (I-L1)
               SET NOT-I-19                TO TRUE
               SET NOT-I-20                TO TRUE
               IF  UYEAR NOT > 80
                   SET I-20                TO TRUE
               END-IF
               IF  UYEAR > 80
                   SET I-19                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-19)
               MOVE 19                     TO AARLIM (1:2)
           END-IF
           IF  (I-L1 AND I-20)
               MOVE 20                     TO AARLIM (1:2)
           END-IF
           IF  (I-L1)
               MOVE UYEAR                  TO AARLIM-IO (3:2)
               SUBTRACT 3                  FROM AARLIM
           END-IF
           SET NOT-I-22                    TO TRUE
           IF  AARF = '**'
               SET I-22                    TO TRUE
           END-IF
           IF  (NOT-I-22)
               MOVE AARF                   TO AAR-IO
               SET NOT-I-19                TO TRUE
               SET NOT-I-20                TO TRUE
               IF  AAR NOT > 80
                   SET I-20                TO TRUE
               END-IF
               IF  AAR > 80
                   SET I-19                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-22 AND I-19)
               MOVE 19                     TO AAR4 (1:2)
           END-IF
           IF  (NOT-I-22 AND I-20)
               MOVE 20                     TO AAR4 (1:2)
           END-IF
           IF  (NOT-I-22)
               MOVE AAR                    TO AAR4-IO (3:2)
               SET NOT-I-90                TO TRUE
               IF  AAR4 < AARLIM
                   SET I-90                TO TRUE
               END-IF
           END-IF
           ADD 1                           TO ANTL1
           IF  (NOT-I-98 AND NOT-I-90)
               ADD 1                       TO ANTL1K
           END-IF
           ADD 1                           TO ANTLR
           IF  (NOT-I-98 AND NOT-I-90)
               ADD 1                       TO ANTLRK
      ******************************************************
      *    SUBRUTINE FOR SLETTING AV HELE FIRMA            *
      ******************************************************
           END-IF
           .
 
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
           IF  (NOT-I-96)
               SET NOT-I-98                TO TRUE
               IF  FIRMSL = 'S'
                   SET I-98                TO TRUE
               END-IF
           END-IF
           IF  (I-96)
               SET I-98                    TO TRUE
           END-IF.
      ******************************************************
 
       BUDMAS-GET SECTION.
       BUDMAS-GET-P.
           IF  BUDMAS-EOF-OFF
               READ BUDMAS
               AT END
                   SET BUDMAS-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       BUDMAS-FLDSET SECTION.
       BUDMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE BUDMAS-IO-AREA (1:82)  TO RECORD-X (1:82)
               MOVE BUDMAS-IO-AREA (1:1)   TO ART (1:1)
               MOVE BUDMAS-IO-AREA (2:3)   TO FIRMA (1:3)
               MOVE BUDMAS-IO-AREA (5:2)   TO AARF (1:2)
           END-EVALUATE.
 
       BUDMAS-IDSET SECTION.
       BUDMAS-IDSET-P.
           SET I-01                        TO TRUE.
 
       BUDMAS-CHK-LEVEL SECTION.
       BUDMAS-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO BUDMAS-LEVEL-01
               MOVE BUDMAS-IO-AREA (2:3)   TO BUDMAS-01-L1-FIRMA
               IF  BUDMAS-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  BUDMAS-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  BUDMAS-01-L1          TO THE-PRIOR-L1
               SET BUDMAS-LEVEL-INIT       TO TRUE
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
           SET I-02                        TO TRUE.
 
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
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND NOT-I-98 AND NOT-I-90)
               MOVE SPACES TO OUTFILE-IO-AREA
               INITIALIZE OUTFILE-IO-AREA
               MOVE RECORD-X               TO OUTFILE-IO-AREA (1:82)
               WRITE OUTFILE-IO-AREA
           END-IF
           IF  (I-01 AND I-U1)
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE AARF                   TO PRINT-X-IO-AREA (79:2)
               IF  (I-90)
                   MOVE AAR4-IO            TO PRINT-X-IO-AREA (87:4)
               END-IF
               IF  (I-90)
                   MOVE AAR4-IO            TO PRINT-X-IO-AREA (87:4)
               END-IF
               IF  (I-02 AND I-U1)
                   MOVE ' '                TO PRINT-X-IO-AREA (100:1)
               END-IF
               MOVE 1                      TO PRINT-X-AFTER-SPACE
               PERFORM PRINT-X-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P)
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE 'AVSTEMMING PROG. VRG029 ' TO PRINT-X-IO-AREA
                                                                (1:24)
               MOVE 01                     TO PRINT-X-BEFORE-SKIP
               MOVE 3                      TO PRINT-X-BEFORE-SPACE
               MOVE 3                      TO PRINT-X-AFTER-SPACE
               PERFORM PRINT-X-PRINT-LINE
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE 'BUDSJETT.MASTER'      TO PRINT-X-IO-AREA (9:15)
               MOVE 'XVSAM26J'             TO PRINT-X-IO-AREA (26:8)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO PRINT-X-IO-AREA (35:8)
               MOVE 2                      TO PRINT-X-AFTER-SPACE
               PERFORM PRINT-X-PRINT-LINE
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE 'FIRMA'                TO PRINT-X-IO-AREA (1:5)
               MOVE 'RA'                   TO PRINT-X-IO-AREA (7:2)
               MOVE 'ANT.LEST'             TO PRINT-X-IO-AREA (13:8)
               MOVE 'FIRMANAVN'            TO PRINT-X-IO-AREA (26:9)
               MOVE 'MERKNADER'            TO PRINT-X-IO-AREA (57:9)
               MOVE 'ANT.KOPIERT'          TO PRINT-X-IO-AREA (68:11)
               IF  (I-U1)
                   MOVE 'SANÅR'            TO PRINT-X-IO-AREA (86:5)
               END-IF
               MOVE 2                      TO PRINT-X-AFTER-SPACE
               PERFORM PRINT-X-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE 'AVSTEMMING PROG. VRG029 ' TO PRINT-X-IO-AREA
                                                                (1:24)
               MOVE 01                     TO PRINT-X-BEFORE-SKIP
               MOVE 3                      TO PRINT-X-BEFORE-SPACE
               MOVE 3                      TO PRINT-X-AFTER-SPACE
               PERFORM PRINT-X-PRINT-LINE
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE 'BUDSJETT.MASTER'      TO PRINT-X-IO-AREA (9:15)
               MOVE 'XVSAM26J'             TO PRINT-X-IO-AREA (26:8)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO PRINT-X-IO-AREA (35:8)
               MOVE 2                      TO PRINT-X-AFTER-SPACE
               PERFORM PRINT-X-PRINT-LINE
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE 'FIRMA'                TO PRINT-X-IO-AREA (1:5)
               MOVE 'RA'                   TO PRINT-X-IO-AREA (7:2)
               MOVE 'ANT.LEST'             TO PRINT-X-IO-AREA (13:8)
               MOVE 'FIRMANAVN'            TO PRINT-X-IO-AREA (26:9)
               MOVE 'MERKNADER'            TO PRINT-X-IO-AREA (57:9)
               MOVE 'ANT.KOPIERT'          TO PRINT-X-IO-AREA (68:11)
               IF  (I-U1)
                   MOVE 'SANÅR'            TO PRINT-X-IO-AREA (86:5)
               END-IF
               MOVE 2                      TO PRINT-X-AFTER-SPACE
               PERFORM PRINT-X-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1)
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE FIRMA                  TO PRINT-X-IO-AREA (3:3)
               MOVE ART                    TO PRINT-X-IO-AREA (8:1)
               MOVE ANTL1                  TO XO-70YY9
               MOVE XO-70YY9               TO PRINT-X-IO-AREA (12:9)
               INITIALIZE ANTL1
               IF  (NOT-I-96)
                   MOVE FINAVN             TO PRINT-X-IO-AREA (26:30)
               END-IF
               IF  (I-98)
                   MOVE 'SLETTET'          TO PRINT-X-IO-AREA (57:7)
               END-IF
               MOVE ANTL1K                 TO XO-70YY9
               MOVE XO-70YY9               TO PRINT-X-IO-AREA (70:9)
               INITIALIZE ANTL1K
               IF  (I-U1)
                   MOVE AARLIM-IO          TO PRINT-X-IO-AREA (87:4)
               END-IF
               MOVE 1                      TO PRINT-X-AFTER-SPACE
               PERFORM PRINT-X-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE '***'                  TO PRINT-X-IO-AREA (3:3)
               MOVE ANTLR                  TO XO-70YY9
               MOVE XO-70YY9               TO PRINT-X-IO-AREA (12:9)
               MOVE 'RECORDS LEST INN'     TO PRINT-X-IO-AREA (26:16)
               MOVE 1                      TO PRINT-X-BEFORE-SPACE
               PERFORM PRINT-X-PRINT-LINE
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE '***'                  TO PRINT-X-IO-AREA (3:3)
               MOVE ANTLRK                 TO XO-70YY9
               MOVE XO-70YY9               TO PRINT-X-IO-AREA (12:9)
               MOVE 'RECORDS KOPIERT.'     TO PRINT-X-IO-AREA (26:16)
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
           MOVE 1                          TO LR-CHECK
           SET BUDMAS-LEVEL-INIT           TO TRUE
           INITIALIZE BUDMAS-DATA-FIELDS
           SET BUDMAS-EOF-OFF              TO TRUE
           SET BUDMAS-PROCESS              TO TRUE
           OPEN INPUT BUDMAS
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
           CLOSE BUDMAS
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
