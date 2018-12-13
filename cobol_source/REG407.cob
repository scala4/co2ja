       IDENTIFICATION DIVISION.
       PROGRAM-ID. REG407R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAMNAVN: REG407, LISTER LOG-FILE                          *
      * LAGET AV   : MORTEN TUVRØNNINGEN                              *
      * ENDRINGER  : XX.XX.XX                                         *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: REG407.rpg
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
           SELECT AUTOLOG
               ASSIGN TO UT-S-AUTOLOG
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS AUTOLOG-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD AUTOLOG
               BLOCK CONTAINS 4096
               RECORD CONTAINS 4096.
       01  AUTOLOG-IO-AREA.
           05  AUTOLOG-IO-AREA-X           PICTURE X(4096).
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
           10  AUTOLOG-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  AUTOLOG-EOF-OFF         VALUE '0'.
               88  AUTOLOG-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  AUTOLOG-READ-OFF        VALUE '0'.
               88  AUTOLOG-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  AUTOLOG-PROCESS-OFF     VALUE '0'.
               88  AUTOLOG-PROCESS         VALUE '1'.
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
           05  AUTOLOG-DATA-FIELDS.
               10  ALPROG                  PICTURE X(8).
               10  ALFILE                  PICTURE X(8).
               10  ALDATO                  PICTURE X(6).
               10  ALKLOK                  PICTURE X(6).
               10  ALUSER                  PICTURE X(8).
               10  ALFIRM                  PICTURE X(3).
               10  ALSYS                   PICTURE X(3).
               10  ALDIV                   PICTURE X(5).
               10  ALAKSE                  PICTURE X(1).
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
           IF  AUTOLOG-PROCESS
               SET AUTOLOG-PROCESS-OFF     TO TRUE
               SET AUTOLOG-READ            TO TRUE
           END-IF
 
           IF  AUTOLOG-READ
           AND RECORD-SELECTED-OFF
               PERFORM AUTOLOG-GET
               SET AUTOLOG-READ-OFF        TO TRUE
               IF  NOT AUTOLOG-EOF
                   SET AUTOLOG-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  AUTOLOG-PROCESS
               PERFORM AUTOLOG-IDSET
           END-IF
 
           IF I-1ST
               GO TO LR-INDICATOR-TEST
           END-IF.
 
       MAINLINE-TOTAL-CALCS.
           CONTINUE.
 
       LR-INDICATOR-TEST.
           IF  I-LR
               GO TO MAINLINE-TERMINATION
           END-IF
           PERFORM HEADING-OVERFLOW
 
           IF  AUTOLOG-PROCESS
               PERFORM AUTOLOG-FLDSET
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
           IF  (NOT-I-99 AND I-77)
               SET I-99                    TO TRUE
           END-IF
           IF  (I-99 AND I-77)
               SET NOT-I-77                TO TRUE
           END-IF
           IF  (NOT-I-99)
               SET I-77                    TO TRUE
      *                    MOVE "REC037  "BUGFL2  8        LEDETXT DEBUG
      *          BUGFL2    DEBUGBUGFILO   REC037           VIS FELT/IND
           END-IF
           .
 
       AUTOLOG-GET SECTION.
       AUTOLOG-GET-P.
           IF  AUTOLOG-EOF-OFF
               READ AUTOLOG
               AT END
                   SET AUTOLOG-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       AUTOLOG-FLDSET SECTION.
       AUTOLOG-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE AUTOLOG-IO-AREA (1:8)  TO ALPROG (1:8)
               MOVE AUTOLOG-IO-AREA (9:8)  TO ALFILE (1:8)
               MOVE AUTOLOG-IO-AREA (17:6) TO ALDATO (1:6)
               MOVE AUTOLOG-IO-AREA (23:6) TO ALKLOK (1:6)
               MOVE AUTOLOG-IO-AREA (29:8) TO ALUSER (1:8)
               MOVE AUTOLOG-IO-AREA (37:3) TO ALFIRM (1:3)
               MOVE AUTOLOG-IO-AREA (40:3) TO ALSYS (1:3)
               MOVE AUTOLOG-IO-AREA (43:5) TO ALDIV (1:5)
               MOVE AUTOLOG-IO-AREA (48:1) TO ALAKSE (1:1)
           END-EVALUATE.
 
       AUTOLOG-IDSET SECTION.
       AUTOLOG-IDSET-P.
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
           IF  (I-01)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ALFIRM                 TO LISTE-IO-AREA (1:3)
               MOVE ALPROG                 TO LISTE-IO-AREA (5:8)
               MOVE ALFILE                 TO LISTE-IO-AREA (15:8)
               MOVE ALUSER                 TO LISTE-IO-AREA (25:8)
               MOVE ALDATO                 TO LISTE-IO-AREA (35:6)
               MOVE ALKLOK                 TO LISTE-IO-AREA (43:6)
               MOVE ALSYS                  TO LISTE-IO-AREA (51:3)
               MOVE ALDIV                  TO LISTE-IO-AREA (56:5)
               MOVE ALAKSE                 TO LISTE-IO-AREA (63:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-77)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'PROGRAMLOGG AUTODATA LOG' TO LISTE-IO-AREA (1:24)
               MOVE '-FILE                   ' TO LISTE-IO-AREA (25:24)
               MOVE '        PROG. REG407    ' TO LISTE-IO-AREA (49:24)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (73:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIR'                  TO LISTE-IO-AREA (1:3)
               MOVE 'PROGRAM '             TO LISTE-IO-AREA (5:8)
               MOVE 'FILENAME'             TO LISTE-IO-AREA (15:8)
               MOVE 'USERID  '             TO LISTE-IO-AREA (25:8)
               MOVE 'DATO  '               TO LISTE-IO-AREA (35:6)
               MOVE 'KLOKKE'               TO LISTE-IO-AREA (43:6)
               MOVE 'SYS'                  TO LISTE-IO-AREA (51:3)
               MOVE 'DIV  '                TO LISTE-IO-AREA (56:5)
               MOVE 'A'                    TO LISTE-IO-AREA (63:1)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'PROGRAMLOGG AUTODATA LOG' TO LISTE-IO-AREA (1:24)
               MOVE '-FILE                   ' TO LISTE-IO-AREA (25:24)
               MOVE '        PROG. REG407    ' TO LISTE-IO-AREA (49:24)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (73:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIR'                  TO LISTE-IO-AREA (1:3)
               MOVE 'PROGRAM '             TO LISTE-IO-AREA (5:8)
               MOVE 'FILENAME'             TO LISTE-IO-AREA (15:8)
               MOVE 'USERID  '             TO LISTE-IO-AREA (25:8)
               MOVE 'DATO  '               TO LISTE-IO-AREA (35:6)
               MOVE 'KLOKKE'               TO LISTE-IO-AREA (43:6)
               MOVE 'SYS'                  TO LISTE-IO-AREA (51:3)
               MOVE 'DIV  '                TO LISTE-IO-AREA (56:5)
               MOVE 'A'                    TO LISTE-IO-AREA (63:1)
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
           INITIALIZE AUTOLOG-DATA-FIELDS
           SET AUTOLOG-EOF-OFF             TO TRUE
           SET AUTOLOG-PROCESS             TO TRUE
           OPEN INPUT AUTOLOG
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE AUTOLOG
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
