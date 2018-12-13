       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAR010R.
      **********************************************  Z-WIN-RPG2   ****
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: PAR010.rpg
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
           SELECT NAVN
               ASSIGN TO UT-S-NAVN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS NAVN-STATUS.
           SELECT PKORT
               ASSIGN TO UT-S-PKORT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PKORT-STATUS.
           SELECT MNDPAR
               ASSIGN TO UT-S-MNDPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS MNDPAR-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD NAVN
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  NAVN-IO-AREA.
           05  NAVN-IO-AREA-X              PICTURE X(80).
       FD PKORT
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  PKORT-IO-AREA.
           05  PKORT-IO-AREA-X             PICTURE X(80).
       FD MNDPAR
               BLOCK CONTAINS 100
               RECORD CONTAINS 100.
       01  MNDPAR-IO-AREA.
           05  MNDPAR-IO-AREA-X            PICTURE X(100).
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       WORKING-STORAGE SECTION.
       77  TABMND-MAX   VALUE 12           PICTURE 9(4) USAGE BINARY.
       77  TABNAV-MAX   VALUE 12           PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  TABMND-TABLE.
               10  TABMND-ENTRY
                                           OCCURS 12 TIMES
                                           INDEXED BY TABMND-I
                                                      TABMND-S
                                                      TABNAV-I
                                                      TABNAV-S.
                   15  TABMND              PICTURE X(2).
                   15  TABNAV              PICTURE X(44).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  NAVN-STATUS                 PICTURE 99 VALUE 0.
           10  PKORT-STATUS                PICTURE 99 VALUE 0.
           10  MNDPAR-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  NAVN-EOF-OFF            VALUE '0'.
               88  NAVN-EOF                VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PKORT-EOF-OFF           VALUE '0'.
               88  PKORT-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PKORT-READ-OFF          VALUE '0'.
               88  PKORT-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PKORT-PROCESS-OFF       VALUE '0'.
               88  PKORT-PROCESS           VALUE '1'.
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
           05  PKORT-DATA-FIELDS.
               10  A-ELGR4                 PICTURE X(4).
               10  MND                     PICTURE X(2).
               10  A-ELGR2                 PICTURE X(2).
           05  TEMPORARY-FIELDS.
               10  KVNR                    PICTURE X(1).
               10  MNDNAV                  PICTURE X(9).
               10  NR7                     PICTURE X(7).
               10  PDATO                   PICTURE X(6).
               10  PDATON-IO.
                   15  PDATON              PICTURE S9(6).
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
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  PKORT-PROCESS
               SET PKORT-PROCESS-OFF       TO TRUE
               SET PKORT-READ              TO TRUE
           END-IF
 
           IF  PKORT-READ
           AND RECORD-SELECTED-OFF
               PERFORM PKORT-GET
               SET PKORT-READ-OFF          TO TRUE
               IF  NOT PKORT-EOF
                   PERFORM PKORT-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET PKORT-PROCESS       TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  PKORT-PROCESS
               PERFORM PKORT-IDSET
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
 
           IF  PKORT-PROCESS
               PERFORM PKORT-FLDSET
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
           SET NOT-I-20                    TO TRUE
           SET NOT-I-25                    TO TRUE
           SET NOT-I-10                    TO TRUE
           SET TABMND-S                    TO TABMND-I
           PERFORM WITH TEST AFTER
                   VARYING TABMND-I FROM 1 BY 1
                     UNTIL TABMND-I >= TABMND-MAX
                        OR I-10
               IF  MND = TABMND (TABMND-I)
                   SET I-10                TO TRUE
                   SET TABMND-S            TO TABMND-I
               END-IF
           END-PERFORM
           SET TABMND-I                    TO TABMND-S
           IF  I-10
           AND TABMND-I NOT > TABNAV-MAX
               SET TABNAV-I                TO TABMND-I
           END-IF
           IF  (NOT-I-10)
               SET I-20                    TO TRUE
           END-IF
           SET NOT-I-11                    TO TRUE
           IF  MND < '01'
               SET I-11                    TO TRUE
           END-IF
           SET NOT-I-12                    TO TRUE
           IF  MND > '12'
               SET I-12                    TO TRUE
           END-IF
           IF  (I-11)
               OR  (I-12)
               SET I-20                    TO TRUE
           END-IF
           SET NOT-I-14                    TO TRUE
           IF  A-ELGR4 < '1998'
               SET I-14                    TO TRUE
           END-IF
           SET NOT-I-15                    TO TRUE
           IF  A-ELGR4 > '2030'
               SET I-15                    TO TRUE
           END-IF
           IF  (I-14)
               OR  (I-15)
               SET I-20                    TO TRUE
           END-IF
           MOVE TABNAV(TABNAV-I) (44:1)    TO KVNR
           MOVE TABNAV(TABNAV-I) (1:9)     TO MNDNAV
           MOVE TABNAV(TABNAV-I) (38:7)    TO NR7
           MOVE NR7 (1:6)                  TO PDATO
           MOVE A-ELGR2                    TO PDATO (5:2)
           MOVE PDATO                      TO PDATON-IO
           SET NOT-I-16                    TO TRUE
           IF  KVNR = ' '
               SET I-16                    TO TRUE
           END-IF.
 
       PKORT-GET SECTION.
       PKORT-GET-P.
           IF  PKORT-EOF-OFF
               READ PKORT
               AT END
                   SET PKORT-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       PKORT-FLDSET SECTION.
       PKORT-FLDSET-P.
           EVALUATE TRUE
           WHEN ( PKORT-IO-AREA (1:1) = '9'
            AND   PKORT-IO-AREA (2:1) = '0' )
               MOVE PKORT-IO-AREA (4:4)    TO A-ELGR4 (1:4)
               MOVE PKORT-IO-AREA (9:2)    TO MND (1:2)
               MOVE PKORT-IO-AREA (6:2)    TO A-ELGR2 (1:2)
           END-EVALUATE.
 
       PKORT-IDCHK SECTION.
       PKORT-IDCHK-P.
           EVALUATE TRUE
           WHEN ( PKORT-IO-AREA (1:1) = '9'
            AND   PKORT-IO-AREA (2:1) = '0' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       PKORT-IDSET SECTION.
       PKORT-IDSET-P.
           EVALUATE TRUE
           WHEN ( PKORT-IO-AREA (1:1) = '9'
            AND   PKORT-IO-AREA (2:1) = '0' )
               SET I-01                    TO TRUE
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
 
       NAVN-LOAD SECTION.
       NAVN-LOAD-P.
           OPEN INPUT NAVN
           SET TABMND-I                    TO 1
           PERFORM UNTIL NAVN-EOF
               READ NAVN
               AT END
                   SET NAVN-EOF            TO TRUE
               NOT AT END
                   MOVE NAVN-IO-AREA (1:46) TO TABMND-ENTRY (TABMND-I)
                   SET TABMND-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE NAVN.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO MNDPAR-IO-AREA
               INITIALIZE MNDPAR-IO-AREA
               MOVE '90'                   TO MNDPAR-IO-AREA (1:2)
               MOVE A-ELGR4                TO MNDPAR-IO-AREA (3:4)
               MOVE MND                    TO MNDPAR-IO-AREA (7:2)
               MOVE TABNAV (TABNAV-I)      TO MNDPAR-IO-AREA (9:44)
               MOVE A-ELGR2                TO MNDPAR-IO-AREA (50:2)
               WRITE MNDPAR-IO-AREA
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'P A R A M E T E R'    TO LISTE-IO-AREA (10:17)
               MOVE 'F I L E'              TO LISTE-IO-AREA (28:7)
               MOVE 'S A L G S'            TO LISTE-IO-AREA (38:9)
               MOVE 'O P P G A V E R'      TO LISTE-IO-AREA (48:15)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (69:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-BEFORE-SPACE
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ÅRSTALL'              TO LISTE-IO-AREA (10:7)
               MOVE A-ELGR4                TO LISTE-IO-AREA (30:4)
               IF  (I-25)
                   MOVE 'FEIL €R'          TO LISTE-IO-AREA (50:7)
               END-IF
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'MND. NR.'             TO LISTE-IO-AREA (10:8)
               MOVE MND                    TO LISTE-IO-AREA (30:2)
               IF  (I-20)
                   MOVE 'FEIL MND'         TO LISTE-IO-AREA (51:8)
               END-IF
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'PERIODEDATO'          TO LISTE-IO-AREA (10:11)
               MOVE PDATON                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (30:8)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'MND. NAVN.'           TO LISTE-IO-AREA (10:10)
               MOVE MNDNAV                 TO LISTE-IO-AREA (30:9)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KVARTAL'              TO LISTE-IO-AREA (10:7)
               IF  (NOT-I-16)
                   MOVE KVNR               TO LISTE-IO-AREA (30:1)
               END-IF
               IF  (NOT-I-16)
                   MOVE '. KVARTAL'        TO LISTE-IO-AREA (31:9)
               END-IF
               IF  (I-16)
                   MOVE 'IKKE KVARTALSLUTT' TO LISTE-IO-AREA (50:17)
               END-IF
               MOVE 2                      TO LISTE-AFTER-SPACE
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
           PERFORM NAVN-LOAD
           INITIALIZE PKORT-DATA-FIELDS
           SET PKORT-EOF-OFF               TO TRUE
           SET PKORT-PROCESS               TO TRUE
           OPEN INPUT PKORT
           OPEN OUTPUT MNDPAR
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           SET TABMND-I                    TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PKORT
           CLOSE MNDPAR
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
