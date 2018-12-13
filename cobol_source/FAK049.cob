       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK049R.
      ****XRES18UM ****************************** :   Z-WIN-RPG2     **
      * PROGRAM: FAK049    DANNE TEKSTRECORD TIL FAKTURA PURREGEBYR   *
      *                    PÅ GRUNNLAG AV FAKTURA SOM ER PURRET SISTE *
      *                    MÅNED.                                     *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK049.rpg
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
           SELECT PURREC
               ASSIGN TO UT-S-PURREC
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PURREC-STATUS.
           SELECT PURFAK
               ASSIGN TO UT-S-PURFAK
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PURFAK-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD PURREC
               BLOCK CONTAINS 400
               RECORD CONTAINS 40.
       01  PURREC-IO-AREA.
           05  PURREC-IO-AREA-X            PICTURE X(40).
       FD PURFAK
               BLOCK CONTAINS 600
               RECORD CONTAINS 150.
       01  PURFAK-IO-AREA.
           05  PURFAK-IO-AREA-X            PICTURE X(150).
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
           10  PURREC-STATUS               PICTURE 99 VALUE 0.
           10  PURFAK-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  PURREC-EOF-OFF          VALUE '0'.
               88  PURREC-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PURREC-READ-OFF         VALUE '0'.
               88  PURREC-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PURREC-PROCESS-OFF      VALUE '0'.
               88  PURREC-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  PURREC-LEVEL-INIT-OFF   VALUE '0'.
               88  PURREC-LEVEL-INIT       VALUE '1'.
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
           05  PURREC-LEVEL-01.
               10  PURREC-01-L1.
                   15  PURREC-01-L1-FNRKNR PICTURE X(9).
           05  PURREC-DATA-FIELDS.
               10  FNRKNR                  PICTURE X(9).
               10  FAKNR                   PICTURE X(6).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(9).
           05  TEMPORARY-FIELDS.
               10  ANTFAK-IO.
                   15  ANTFAK              PICTURE S9(6).
               10  FAKNR1                  PICTURE X(6).
               10  ANTX6                   PICTURE X(1).
               10  FAKNR2                  PICTURE X(6).
               10  FAKNR3                  PICTURE X(6).
               10  FAKNR4                  PICTURE X(6).
               10  FAKNR5                  PICTURE X(6).
               10  FAKNR6                  PICTURE X(6).
               10  FAKNR7                  PICTURE X(6).
               10  FAKNR8                  PICTURE X(6).
               10  FAKNR9                  PICTURE X(6).
               10  FAKNR0                  PICTURE X(6).
               10  FAKNRA                  PICTURE X(6).
               10  FAKNRB                  PICTURE X(6).
               10  FAKNRC                  PICTURE X(6).
               10  FAKNRD                  PICTURE X(6).
               10  FAKNRE                  PICTURE X(6).
               10  FAKNRF                  PICTURE X(6).
               10  FAKNRG                  PICTURE X(6).
               10  FAKNRH                  PICTURE X(6).
               10  ANTFLE-IO.
                   15  ANTFLE              PICTURE S9(6).
               10  ANTTOT-IO.
                   15  ANTTOT              PICTURE S9(6).
           05  EDITTING-FIELDS.
               10  XO-60YNZ                PICTURE ZZZZZZ.
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
           IF  PURREC-PROCESS
               SET PURREC-PROCESS-OFF      TO TRUE
               SET PURREC-READ             TO TRUE
           END-IF
 
           IF  PURREC-READ
           AND RECORD-SELECTED-OFF
               PERFORM PURREC-GET
               SET PURREC-READ-OFF         TO TRUE
               IF  NOT PURREC-EOF
                   SET PURREC-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  PURREC-PROCESS
               PERFORM PURREC-IDSET
           END-IF
 
           IF  PURREC-PROCESS
               PERFORM PURREC-CHK-LEVEL
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
 
           IF  PURREC-PROCESS
               PERFORM PURREC-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  PURREC-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-L1)
               SUBTRACT ANTFAK             FROM ANTFAK
               SET NOT-I-29                TO TRUE
               MOVE '      '               TO FAKNR1
               MOVE '      '               TO FAKNR2
               MOVE '      '               TO FAKNR3
               MOVE '      '               TO FAKNR4
               MOVE '      '               TO FAKNR5
               MOVE '      '               TO FAKNR6
               MOVE '      '               TO FAKNR7
               MOVE '      '               TO FAKNR8
               MOVE '      '               TO FAKNR9
               MOVE '      '               TO FAKNR0
               MOVE '      '               TO FAKNRA
               MOVE '      '               TO FAKNRB
               MOVE '      '               TO FAKNRC
               MOVE '      '               TO FAKNRD
               MOVE '      '               TO FAKNRE
               MOVE '      '               TO FAKNRF
               MOVE '      '               TO FAKNRG
               MOVE '      '               TO FAKNRH
           END-IF
           IF  (I-01)
               ADD 1                       TO ANTFAK
               SET NOT-I-11                TO TRUE
               IF  ANTFAK = 1
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-11)
               MOVE FAKNR                  TO FAKNR1
               MOVE '1'                    TO ANTX6
           END-IF
           IF  (I-01)
               SET NOT-I-12                TO TRUE
               IF  ANTFAK = 2
                   SET I-12                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-12)
               MOVE FAKNR                  TO FAKNR2
           END-IF
           IF  (I-01)
               SET NOT-I-13                TO TRUE
               IF  ANTFAK = 3
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-13)
               MOVE FAKNR                  TO FAKNR3
           END-IF
           IF  (I-01)
               SET NOT-I-14                TO TRUE
               IF  ANTFAK = 4
                   SET I-14                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-14)
               MOVE FAKNR                  TO FAKNR4
           END-IF
           IF  (I-01)
               SET NOT-I-15                TO TRUE
               IF  ANTFAK = 5
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-15)
               MOVE FAKNR                  TO FAKNR5
           END-IF
           IF  (I-01)
               SET NOT-I-16                TO TRUE
               IF  ANTFAK = 6
                   SET I-16                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-16)
               MOVE FAKNR                  TO FAKNR6
           END-IF
           IF  (I-01)
               SET NOT-I-17                TO TRUE
               IF  ANTFAK = 7
                   SET I-17                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-17)
               MOVE FAKNR                  TO FAKNR7
               MOVE '2'                    TO ANTX6
           END-IF
           IF  (I-01)
               SET NOT-I-18                TO TRUE
               IF  ANTFAK = 8
                   SET I-18                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-18)
               MOVE FAKNR                  TO FAKNR8
           END-IF
           IF  (I-01)
               SET NOT-I-19                TO TRUE
               IF  ANTFAK = 9
                   SET I-19                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-19)
               MOVE FAKNR                  TO FAKNR9
           END-IF
           IF  (I-01)
               SET NOT-I-20                TO TRUE
               IF  ANTFAK = 10
                   SET I-20                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-20)
               MOVE FAKNR                  TO FAKNR0
           END-IF
           IF  (I-01)
               SET NOT-I-21                TO TRUE
               IF  ANTFAK = 11
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-21)
               MOVE FAKNR                  TO FAKNRA
           END-IF
           IF  (I-01)
               SET NOT-I-22                TO TRUE
               IF  ANTFAK = 12
                   SET I-22                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-22)
               MOVE FAKNR                  TO FAKNRB
           END-IF
           IF  (I-01)
               SET NOT-I-23                TO TRUE
               IF  ANTFAK = 13
                   SET I-23                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-23)
               MOVE FAKNR                  TO FAKNRC
               MOVE '3'                    TO ANTX6
           END-IF
           IF  (I-01)
               SET NOT-I-24                TO TRUE
               IF  ANTFAK = 14
                   SET I-24                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-24)
               MOVE FAKNR                  TO FAKNRD
           END-IF
           IF  (I-01)
               SET NOT-I-25                TO TRUE
               IF  ANTFAK = 15
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-25)
               MOVE FAKNR                  TO FAKNRE
           END-IF
           IF  (I-01)
               SET NOT-I-26                TO TRUE
               IF  ANTFAK = 16
                   SET I-26                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-26)
               MOVE FAKNR                  TO FAKNRF
           END-IF
           IF  (I-01)
               SET NOT-I-27                TO TRUE
               IF  ANTFAK = 17
                   SET I-27                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-27)
               MOVE FAKNR                  TO FAKNRG
           END-IF
           IF  (I-01)
               SET NOT-I-29                TO TRUE
               SET NOT-I-28                TO TRUE
               IF  ANTFAK > 18
                   SET I-29                TO TRUE
               END-IF
               IF  ANTFAK = 18
                   SET I-28                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-28)
               MOVE FAKNR                  TO FAKNRH
           END-IF
           IF  (I-01 AND I-29)
               MOVE '+FLERE'               TO FAKNRH
           END-IF.
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1 AND I-29)
               ADD 1                       TO ANTFLE
           END-IF
           IF  (I-L1)
               ADD 1                       TO ANTTOT
           END-IF.
 
       PURREC-GET SECTION.
       PURREC-GET-P.
           IF  PURREC-EOF-OFF
               READ PURREC
               AT END
                   SET PURREC-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       PURREC-FLDSET SECTION.
       PURREC-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE PURREC-IO-AREA (3:9)   TO FNRKNR (1:9)
               MOVE PURREC-IO-AREA (12:6)  TO FAKNR (1:6)
           END-EVALUATE.
 
       PURREC-IDSET SECTION.
       PURREC-IDSET-P.
           SET I-01                        TO TRUE.
 
       PURREC-CHK-LEVEL SECTION.
       PURREC-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO PURREC-LEVEL-01
               MOVE PURREC-IO-AREA (3:9)   TO PURREC-01-L1-FNRKNR
               IF  PURREC-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  PURREC-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  PURREC-01-L1          TO THE-PRIOR-L1
               SET PURREC-LEVEL-INIT       TO TRUE
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
           IF  (I-L1)
               MOVE SPACES TO PURFAK-IO-AREA
               INITIALIZE PURFAK-IO-AREA
               MOVE FNRKNR                 TO PURFAK-IO-AREA (1:9)
               MOVE 'FAKT.NR:'             TO PURFAK-IO-AREA (10:8)
               MOVE FAKNR1                 TO PURFAK-IO-AREA (19:6)
               MOVE FAKNR2                 TO PURFAK-IO-AREA (26:6)
               MOVE FAKNR3                 TO PURFAK-IO-AREA (33:6)
               MOVE FAKNR4                 TO PURFAK-IO-AREA (40:6)
               MOVE FAKNR5                 TO PURFAK-IO-AREA (47:6)
               MOVE FAKNR6                 TO PURFAK-IO-AREA (54:6)
               MOVE FAKNR7                 TO PURFAK-IO-AREA (61:6)
               MOVE FAKNR8                 TO PURFAK-IO-AREA (68:6)
               MOVE FAKNR9                 TO PURFAK-IO-AREA (75:6)
               MOVE FAKNR0                 TO PURFAK-IO-AREA (82:6)
               MOVE FAKNRA                 TO PURFAK-IO-AREA (89:6)
               MOVE FAKNRB                 TO PURFAK-IO-AREA (96:6)
               MOVE FAKNRC                 TO PURFAK-IO-AREA (103:6)
               MOVE FAKNRD                 TO PURFAK-IO-AREA (110:6)
               MOVE FAKNRE                 TO PURFAK-IO-AREA (117:6)
               MOVE FAKNRF                 TO PURFAK-IO-AREA (124:6)
               MOVE FAKNRG                 TO PURFAK-IO-AREA (131:6)
               MOVE FAKNRH                 TO PURFAK-IO-AREA (138:6)
               MOVE ANTX6                  TO PURFAK-IO-AREA (150:1)
               WRITE PURFAK-IO-AREA
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '***   AVSTEMNINGSTOTALER' TO LISTE-IO-AREA (1:24)
               MOVE '    --- FAK049 ---   ***' TO LISTE-IO-AREA (25:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT PURREFAKREC.  '   TO LISTE-IO-AREA (3:18)
               MOVE ANTTOT                 TO XO-60YNZ
               MOVE XO-60YNZ               TO LISTE-IO-AREA (23:6)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT OVER 18 FAKT. '   TO LISTE-IO-AREA (3:18)
               MOVE ANTFLE                 TO XO-60YNZ
               MOVE XO-60YNZ               TO LISTE-IO-AREA (23:6)
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
           SET PURREC-LEVEL-INIT           TO TRUE
           INITIALIZE PURREC-DATA-FIELDS
           SET PURREC-EOF-OFF              TO TRUE
           SET PURREC-PROCESS              TO TRUE
           OPEN INPUT PURREC
           OPEN OUTPUT PURFAK
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PURREC
           CLOSE PURFAK
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
