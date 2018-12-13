       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAR001R.
      **********************************************  Z-WIN-RPG2   ****
      * STANDARD PARAMETERINNLEGGING PÅ DISK MED PRINT.     *
      * INPUT = INTIL 20 PARAMETERKORT (PARAM)              *
      *         UBEGRENSET LIMIT OG TABELKORT.              *
      *******************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: PAR001.rpg
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
           SELECT PARAM
               ASSIGN TO UT-S-PARAM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PARAM-STATUS.
           SELECT PARAMF
               ASSIGN TO UT-S-PARAMF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PARAMF-STATUS.
           SELECT PLIMIT
               ASSIGN TO UT-S-PLIMIT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PLIMIT-STATUS.
           SELECT PTAB1
               ASSIGN TO UT-S-PTAB1
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PTAB1-STATUS.
           SELECT PTAB2
               ASSIGN TO UT-S-PTAB2
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PTAB2-STATUS.
           SELECT PTAB3
               ASSIGN TO UT-S-PTAB3
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PTAB3-STATUS.
           SELECT PTAB4
               ASSIGN TO UT-S-PTAB4
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PTAB4-STATUS.
           SELECT PTAB5
               ASSIGN TO UT-S-PTAB5
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PTAB5-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD PARAM
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  PARAM-IO-AREA.
           05  PARAM-IO-AREA-X             PICTURE X(80).
       FD PARAMF
               BLOCK CONTAINS 1200
               RECORD CONTAINS 1200.
       01  PARAMF-IO-AREA.
           05  PARAMF-IO-AREA-X            PICTURE X(1200).
       FD PLIMIT
               BLOCK CONTAINS 60
               RECORD CONTAINS 60.
       01  PLIMIT-IO-AREA.
           05  PLIMIT-IO-AREA-X            PICTURE X(60).
       FD PTAB1
               BLOCK CONTAINS 1920
               RECORD CONTAINS 60.
       01  PTAB1-IO-AREA.
           05  PTAB1-IO-AREA-X             PICTURE X(60).
       FD PTAB2
               BLOCK CONTAINS 1920
               RECORD CONTAINS 60.
       01  PTAB2-IO-AREA.
           05  PTAB2-IO-AREA-X             PICTURE X(60).
       FD PTAB3
               BLOCK CONTAINS 1920
               RECORD CONTAINS 60.
       01  PTAB3-IO-AREA.
           05  PTAB3-IO-AREA-X             PICTURE X(60).
       FD PTAB4
               BLOCK CONTAINS 1920
               RECORD CONTAINS 60.
       01  PTAB4-IO-AREA.
           05  PTAB4-IO-AREA-X             PICTURE X(60).
       FD PTAB5
               BLOCK CONTAINS 1920
               RECORD CONTAINS 60.
       01  PTAB5-IO-AREA.
           05  PTAB5-IO-AREA-X             PICTURE X(60).
       FD LISTE
               BLOCK CONTAINS 81
               RECORD CONTAINS 81.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(80).
       WORKING-STORAGE SECTION.
       77  ARP-MAX   VALUE 20              PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  ARP-TABLE.
               10  ARP-ENTRY
                                           OCCURS 20 TIMES
                                           INDEXED BY ARP-I
                                                      ARP-S.
                   15  ARP                 PICTURE X(60).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  PARAM-STATUS                PICTURE 99 VALUE 0.
           10  PARAMF-STATUS               PICTURE 99 VALUE 0.
           10  PLIMIT-STATUS               PICTURE 99 VALUE 0.
           10  PTAB1-STATUS                PICTURE 99 VALUE 0.
           10  PTAB2-STATUS                PICTURE 99 VALUE 0.
           10  PTAB3-STATUS                PICTURE 99 VALUE 0.
           10  PTAB4-STATUS                PICTURE 99 VALUE 0.
           10  PTAB5-STATUS                PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-EOF-OFF           VALUE '0'.
               88  PARAM-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-READ-OFF          VALUE '0'.
               88  PARAM-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-PROCESS-OFF       VALUE '0'.
               88  PARAM-PROCESS           VALUE '1'.
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
           05  PARAM-DATA-FIELDS.
               10  PREC                    PICTURE X(80).
               10  DATA-X                  PICTURE X(60).
               10  TYPE-X                  PICTURE X(5).
      ********************************************************
      * BYGG OPP OUTPUTRECORD I ARRAY.                       *
      ********************************************************
           05  TEMPORARY-FIELDS.
               10  X-IO.
                   15  X                   PICTURE S9(2).
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
           05  MOVEA-COUNT                 PICTURE 9(4) USAGE BINARY.
           05  MOVEA-SIZE1                 PICTURE 9(4) USAGE BINARY.
           05  MOVEA-SA1                   PICTURE 9(4) USAGE BINARY.
           05  MOVEA-SIZE2                 PICTURE 9(4) USAGE BINARY.
           05  MOVEA-SA2                   PICTURE 9(4) USAGE BINARY.
           05  MOVEA-LENGTH                PICTURE 9(4) USAGE BINARY.
           05  MOVEA-OFFSET                PICTURE 9(4) USAGE BINARY.
           05  MOVEA-TEMP                  PICTURE X(256).
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
           IF  PARAM-PROCESS
               SET PARAM-PROCESS-OFF       TO TRUE
               SET PARAM-READ              TO TRUE
           END-IF
 
           IF  PARAM-READ
           AND RECORD-SELECTED-OFF
               PERFORM PARAM-GET
               SET PARAM-READ-OFF          TO TRUE
               IF  NOT PARAM-EOF
                   SET PARAM-PROCESS       TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  PARAM-PROCESS
               PERFORM PARAM-IDSET
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
 
           IF  PARAM-PROCESS
               PERFORM PARAM-FLDSET
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
           IF  (I-91)
               SET I-90                    TO TRUE
           END-IF
           SET NOT-I-91                    TO TRUE
           SET NOT-I-11                    TO TRUE
           IF  TYPE-X = 'TAB01'
               SET I-11                    TO TRUE
           END-IF
           SET NOT-I-12                    TO TRUE
           IF  TYPE-X = 'TAB02'
               SET I-12                    TO TRUE
           END-IF
           SET NOT-I-13                    TO TRUE
           IF  TYPE-X = 'TAB03'
               SET I-13                    TO TRUE
           END-IF
           SET NOT-I-21                    TO TRUE
           IF  TYPE-X = 'TAB04'
               SET I-21                    TO TRUE
           END-IF
           SET NOT-I-22                    TO TRUE
           IF  TYPE-X = 'TAB05'
               SET I-22                    TO TRUE
           END-IF
           SET NOT-I-14                    TO TRUE
           IF  TYPE-X = 'PARAM'
               SET I-14                    TO TRUE
           END-IF
           SET NOT-I-15                    TO TRUE
           IF  TYPE-X = 'LIMIT'
               SET I-15                    TO TRUE
           END-IF
           SET NOT-I-16                    TO TRUE
           IF  TYPE-X = 'TEKST'
               SET I-16                    TO TRUE
           END-IF
           IF  (I-14)
               ADD 1                       TO X
               MOVE 1                      TO MOVEA-SA1
               COMPUTE MOVEA-SA2 = 60 * ( X - 1 ) + 1
               MOVE 60                     TO MOVEA-SIZE1
               COMPUTE MOVEA-SIZE2 = ARP-MAX * 60 - MOVEA-SA2 + 1
               IF  MOVEA-SIZE1 > MOVEA-SIZE2
                   MOVE MOVEA-SIZE2        TO MOVEA-SIZE1
               END-IF
               MOVE DATA-X
                        TO ARP-TABLE (MOVEA-SA2:MOVEA-SIZE2)
           END-IF
           IF  (NOT-I-90)
               SET I-91                    TO TRUE
           END-IF
           IF  (I-91)
               ACCEPT SYSTEM-TIME-X      FROM TIME
               MOVE SYSTEM-TIME            TO TID (1:6)
               MOVE SYSTEM-DATE            TO TID (7:6)
           END-IF.
 
       PARAM-GET SECTION.
       PARAM-GET-P.
           IF  PARAM-EOF-OFF
               READ PARAM
               AT END
                   SET PARAM-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       PARAM-FLDSET SECTION.
       PARAM-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE PARAM-IO-AREA (1:80)   TO PREC (1:80)
               MOVE PARAM-IO-AREA (11:60)  TO DATA-X (1:60)
               MOVE PARAM-IO-AREA (1:5)    TO TYPE-X (1:5)
           END-EVALUATE.
 
       PARAM-IDSET SECTION.
       PARAM-IDSET-P.
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
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-11)
               MOVE SPACES TO PTAB1-IO-AREA
               INITIALIZE PTAB1-IO-AREA
               MOVE DATA-X                 TO PTAB1-IO-AREA (1:60)
               WRITE PTAB1-IO-AREA
           END-IF
           IF  (I-01 AND I-12)
               MOVE SPACES TO PTAB2-IO-AREA
               INITIALIZE PTAB2-IO-AREA
               MOVE DATA-X                 TO PTAB2-IO-AREA (1:60)
               WRITE PTAB2-IO-AREA
           END-IF
           IF  (I-01 AND I-13)
               MOVE SPACES TO PTAB3-IO-AREA
               INITIALIZE PTAB3-IO-AREA
               MOVE DATA-X                 TO PTAB3-IO-AREA (1:60)
               WRITE PTAB3-IO-AREA
           END-IF
           IF  (I-01 AND I-21)
               MOVE SPACES TO PTAB4-IO-AREA
               INITIALIZE PTAB4-IO-AREA
               MOVE DATA-X                 TO PTAB4-IO-AREA (1:60)
               WRITE PTAB4-IO-AREA
           END-IF
           IF  (I-01 AND I-22)
               MOVE SPACES TO PTAB5-IO-AREA
               INITIALIZE PTAB5-IO-AREA
               MOVE DATA-X                 TO PTAB5-IO-AREA (1:60)
               WRITE PTAB5-IO-AREA
           END-IF
           IF  (I-01 AND I-15)
               MOVE SPACES TO PLIMIT-IO-AREA
               INITIALIZE PLIMIT-IO-AREA
               MOVE DATA-X                 TO PLIMIT-IO-AREA (1:60)
               WRITE PLIMIT-IO-AREA
           END-IF
           IF  (I-01)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE PREC                   TO LISTE-IO-AREA (1:80)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-91)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '* **  A/S AUTO-DATA'  TO LISTE-IO-AREA (2:19)
               MOVE 'PARAMETERKONTROLL.'   TO LISTE-IO-AREA (23:18)
               MOVE 'FREMKJØRT'            TO LISTE-IO-AREA (43:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (53:8)
               MOVE 'KL.'                  TO LISTE-IO-AREA (62:3)
               MOVE TID                    TO EDIT-DATE
               MOVE EDIT-DATE (4:11)       TO LISTE-IO-AREA (62:11)
               MOVE '** *'                 TO LISTE-IO-AREA (76:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '    0    1    1    2' TO LISTE-IO-AREA (1:20)
               MOVE '    2    3    3    4' TO LISTE-IO-AREA (21:20)
               MOVE '    4    5    5    6' TO LISTE-IO-AREA (41:20)
               MOVE '    6    7    7    8' TO LISTE-IO-AREA (61:20)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '....5....0....5....0' TO LISTE-IO-AREA (1:20)
               MOVE '....5....0....5....0' TO LISTE-IO-AREA (21:20)
               MOVE '....5....0....5....0' TO LISTE-IO-AREA (41:20)
               MOVE '....5....0....5....0' TO LISTE-IO-AREA (61:20)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '* **  A/S AUTO-DATA'  TO LISTE-IO-AREA (2:19)
               MOVE 'PARAMETERKONTROLL.'   TO LISTE-IO-AREA (23:18)
               MOVE 'FREMKJØRT'            TO LISTE-IO-AREA (43:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (53:8)
               MOVE 'KL.'                  TO LISTE-IO-AREA (62:3)
               MOVE TID                    TO EDIT-DATE
               MOVE EDIT-DATE (4:11)       TO LISTE-IO-AREA (62:11)
               MOVE '** *'                 TO LISTE-IO-AREA (76:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '    0    1    1    2' TO LISTE-IO-AREA (1:20)
               MOVE '    2    3    3    4' TO LISTE-IO-AREA (21:20)
               MOVE '    4    5    5    6' TO LISTE-IO-AREA (41:20)
               MOVE '    6    7    7    8' TO LISTE-IO-AREA (61:20)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '....5....0....5....0' TO LISTE-IO-AREA (1:20)
               MOVE '....5....0....5....0' TO LISTE-IO-AREA (21:20)
               MOVE '....5....0....5....0' TO LISTE-IO-AREA (41:20)
               MOVE '....5....0....5....0' TO LISTE-IO-AREA (61:20)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO PARAMF-IO-AREA
               INITIALIZE PARAMF-IO-AREA
               MOVE 1201                   TO BW-A
               PERFORM VARYING ARP-I FROM ARP-MAX BY -1
                         UNTIL ARP-I < 1
                   SUBTRACT 60           FROM BW-A
                   MOVE ARP-ENTRY (ARP-I)  TO PARAMF-IO-AREA (BW-A:60)
               END-PERFORM
               WRITE PARAMF-IO-AREA
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '    0    1    1    2' TO LISTE-IO-AREA (1:20)
               MOVE '    2    3    3    4' TO LISTE-IO-AREA (21:20)
               MOVE '    4    5    5    6' TO LISTE-IO-AREA (41:20)
               MOVE '    6    7    7    8' TO LISTE-IO-AREA (61:20)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '....5....0....5....0' TO LISTE-IO-AREA (1:20)
               MOVE '....5....0....5....0' TO LISTE-IO-AREA (21:20)
               MOVE '....5....0....5....0' TO LISTE-IO-AREA (41:20)
               MOVE '....5....0....5....0' TO LISTE-IO-AREA (61:20)
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
           INITIALIZE PARAM-DATA-FIELDS
           SET PARAM-EOF-OFF               TO TRUE
           SET PARAM-PROCESS               TO TRUE
           OPEN INPUT PARAM
           OPEN OUTPUT PARAMF
           OPEN OUTPUT PLIMIT
           OPEN OUTPUT PTAB1
           OPEN OUTPUT PTAB2
           OPEN OUTPUT PTAB3
           OPEN OUTPUT PTAB4
           OPEN OUTPUT PTAB5
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           PERFORM VARYING ARP-I FROM 1 BY 1
                     UNTIL ARP-I > ARP-MAX
               INITIALIZE ARP (ARP-I)
           END-PERFORM
           SET ARP-I                       TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PARAM
           CLOSE PARAMF
           CLOSE PLIMIT
           CLOSE PTAB1
           CLOSE PTAB2
           CLOSE PTAB3
           CLOSE PTAB4
           CLOSE PTAB5
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
