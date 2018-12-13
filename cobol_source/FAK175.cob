       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK175R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM.......: FAK175, DANNER RELMAST AV FAKTKID.           *
      *  PROGRAMMERER..: MORTEN TUVRØNNINGEN                          *
      *  KJØRES I JOBB.: FAK.FAK10A                                   *
      *  LAGET DATO....: 13.09.02                                     *
      *  ENDRET........:                                              *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK175.rpg
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
           SELECT FAKTKID
               ASSIGN TO UT-S-FAKTKID
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKTKID-STATUS.
           SELECT KIDRELF
               ASSIGN TO UT-S-KIDRELF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KIDRELF-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD FAKTKID
               BLOCK CONTAINS 4000
               RECORD CONTAINS 40.
       01  FAKTKID-IO-AREA.
           05  FAKTKID-IO-AREA-X           PICTURE X(40).
       FD KIDRELF
               BLOCK CONTAINS 160
               RECORD CONTAINS 80.
       01  KIDRELF-IO-AREA.
           05  KIDRELF-IO-AREA-X           PICTURE X(80).
      *BUGFILO O   F  80  80            PRINTERSYSLST
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
           10  FAKTKID-STATUS              PICTURE 99 VALUE 0.
           10  KIDRELF-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKTKID-EOF-OFF         VALUE '0'.
               88  FAKTKID-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKTKID-READ-OFF        VALUE '0'.
               88  FAKTKID-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKTKID-PROCESS-OFF     VALUE '0'.
               88  FAKTKID-PROCESS         VALUE '1'.
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
           05  FAKTKID-DATA-FIELDS.
               10  FIRMNR                  PICTURE X(3).
               10  REFNR1                  PICTURE X(1).
               10  REFNR                   PICTURE X(6).
               10  RESKNR                  PICTURE X(6).
               10  FAKBEL-IO.
                   15  FAKBEL              PICTURE S9(7)V9(2).
               10  FFAAR                   PICTURE X(2).
               10  FFMND                   PICTURE X(2).
               10  FFDAG                   PICTURE X(2).
           05  TEMPORARY-FIELDS.
               10  ANT01-IO.
                   15  ANT01               PICTURE S9(7).
               10  ANT01F-IO.
                   15  ANT01F              PICTURE S9(7).
               10  ANT01G-IO.
                   15  ANT01G              PICTURE S9(7).
               10  ANTTOT-IO.
                   15  ANTTOT              PICTURE S9(7).
           05  EDITTING-FIELDS.
               10  XO-70YN9R               PICTURE ZZZZZZ9-.
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
 
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  FAKTKID-PROCESS
               SET FAKTKID-PROCESS-OFF     TO TRUE
               SET FAKTKID-READ            TO TRUE
           END-IF
 
           IF  FAKTKID-READ
           AND RECORD-SELECTED-OFF
               PERFORM FAKTKID-GET
               SET FAKTKID-READ-OFF        TO TRUE
               IF  NOT FAKTKID-EOF
                   SET FAKTKID-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  FAKTKID-PROCESS
               PERFORM FAKTKID-IDSET
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
 
           IF  FAKTKID-PROCESS
               PERFORM FAKTKID-FLDSET
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
           IF  (I-01)
               PERFORM FAKKID-S
      *  01                MOVE "X       "BUGFL2  8        LEDETXT DEBUG
      *  01      BUGFL2    DEBUGBUGFILO   X                VIS FELT/IND
      *
           END-IF
           .
 
       FAKKID-S SECTION.
       FAKKID-S-P.
           SET NOT-I-10                    TO TRUE
           IF  REFNR1 > '0'
               SET I-10                    TO TRUE
           END-IF
           ADD 1                           TO ANT01
           IF  (NOT-I-10)
               ADD 1                       TO ANT01F
           END-IF
           IF  (I-10)
               ADD 1                       TO ANT01G
               ADD 1                       TO ANTTOT
           END-IF.
      *
 
       FAKTKID-GET SECTION.
       FAKTKID-GET-P.
           IF  FAKTKID-EOF-OFF
               READ FAKTKID
               AT END
                   SET FAKTKID-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FAKTKID-FLDSET SECTION.
       FAKTKID-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKTKID-IO-AREA (2:3)  TO FIRMNR (1:3)
               MOVE FAKTKID-IO-AREA (5:1)  TO REFNR1 (1:1)
               MOVE FAKTKID-IO-AREA (5:6)  TO REFNR (1:6)
               MOVE FAKTKID-IO-AREA (12:6) TO RESKNR (1:6)
               MOVE FAKTKID-IO-AREA (18:9) TO FAKBEL-IO
               INSPECT FAKBEL-IO REPLACING ALL ' ' BY '0'
               MOVE FAKTKID-IO-AREA (27:2) TO FFAAR (1:2)
               MOVE FAKTKID-IO-AREA (29:2) TO FFMND (1:2)
               MOVE FAKTKID-IO-AREA (31:2) TO FFDAG (1:2)
           END-EVALUATE.
 
       FAKTKID-IDSET SECTION.
       FAKTKID-IDSET-P.
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
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-10)
               MOVE SPACES TO KIDRELF-IO-AREA
               INITIALIZE KIDRELF-IO-AREA
               MOVE 'A'                    TO KIDRELF-IO-AREA (1:1)
               MOVE FIRMNR                 TO KIDRELF-IO-AREA (2:3)
               MOVE REFNR                  TO KIDRELF-IO-AREA (5:6)
               MOVE '       '              TO KIDRELF-IO-AREA (11:7)
               MOVE '000'                  TO KIDRELF-IO-AREA (18:3)
               MOVE RESKNR                 TO KIDRELF-IO-AREA (21:6)
               MOVE REFNR                  TO KIDRELF-IO-AREA (27:6)
               MOVE FAKBEL-IO              TO KIDRELF-IO-AREA (33:9)
               MOVE FFAAR                  TO KIDRELF-IO-AREA (42:2)
               MOVE FFMND                  TO KIDRELF-IO-AREA (44:2)
               MOVE FFDAG                  TO KIDRELF-IO-AREA (46:2)
               MOVE 'F'                    TO KIDRELF-IO-AREA (48:1)
               MOVE FAKBEL-IO              TO KIDRELF-IO-AREA (50:9)
               MOVE UYEAR                  TO KIDRELF-IO-AREA (59:2)
               MOVE UMONTH                 TO KIDRELF-IO-AREA (61:2)
               MOVE UDAY                   TO KIDRELF-IO-AREA (63:2)
               WRITE KIDRELF-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'DANNING AV RELMAST FRA :' TO LISTE-IO-AREA (1:24)
               MOVE ' FAKTKID                ' TO LISTE-IO-AREA (25:24)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (73:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL LEST            :' TO LISTE-IO-AREA (1:24)
               MOVE ANT01                  TO XO-70YN9R
               MOVE XO-70YN9R              TO LISTE-IO-AREA (33:8)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL FEIL FAKTURAKID :' TO LISTE-IO-AREA (1:24)
               MOVE ANT01F                 TO XO-70YN9R
               MOVE XO-70YN9R              TO LISTE-IO-AREA (33:8)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL GODKJENT FAKTKID:' TO LISTE-IO-AREA (1:24)
               MOVE ANT01G                 TO XO-70YN9R
               MOVE XO-70YN9R              TO LISTE-IO-AREA (33:8)
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
           INITIALIZE FAKTKID-DATA-FIELDS
           SET FAKTKID-EOF-OFF             TO TRUE
           SET FAKTKID-PROCESS             TO TRUE
           OPEN INPUT FAKTKID
           OPEN OUTPUT KIDRELF
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FAKTKID
           CLOSE KIDRELF
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
