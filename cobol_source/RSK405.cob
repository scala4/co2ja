       IDENTIFICATION DIVISION.
       PROGRAM-ID. RSK405R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM.......: RSK405, DANNER AUTOGIRO-TRANSER FRA RELMAST. *
      *  PROGRAMMERER..: MORTEN TUVRØNNINGEN                          *
      *  KJØRES I JOBB.: RES10AF                                      *
      *  LAGET DATO....: 13.12.01                                     *
      *  ENDRET........: 18.06.02 MANGLET DESIMALER I SUMBEL.         *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RSK405.rpg
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
           SELECT RELMAST
               ASSIGN TO UT-S-RELMAST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RELMAST-STATUS.
           SELECT SYSPARM
               ASSIGN TO SYSPARM
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS SYSPARM-STATUS
               RECORD KEY IS SYSPARM-KEY1.
           SELECT AUTOGIR
               ASSIGN TO UT-S-AUTOGIR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS AUTOGIR-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD RELMAST
               BLOCK CONTAINS 800
               RECORD CONTAINS 80.
       01  RELMAST-IO-AREA.
           05  RELMAST-IO-AREA-X           PICTURE X(80).
       FD SYSPARM
               RECORD CONTAINS 160.
       01  SYSPARM-IO-AREA.
           05  SYSPARM-IO-AREA-X.
               10  SYSPARM-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(150).
       FD AUTOGIR
               BLOCK CONTAINS 400
               RECORD CONTAINS 40.
       01  AUTOGIR-IO-AREA.
           05  AUTOGIR-IO-AREA-X           PICTURE X(40).
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
           10  RELMAST-STATUS              PICTURE 99 VALUE 0.
           10  SYSPARM-STATUS              PICTURE 99 VALUE 0.
           10  AUTOGIR-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  RELMAST-EOF-OFF         VALUE '0'.
               88  RELMAST-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RELMAST-READ-OFF        VALUE '0'.
               88  RELMAST-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RELMAST-PROCESS-OFF     VALUE '0'.
               88  RELMAST-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  RELMAST-LEVEL-INIT-OFF  VALUE '0'.
               88  RELMAST-LEVEL-INIT      VALUE '1'.
           05  SYSPARM-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  RELMAST-LEVEL-01.
               10  RELMAST-01-L2.
                   15  RELMAST-01-L2-RELFNR PICTURE X(3).
               10  RELMAST-01-L1.
                   15  RELMAST-01-L1-RELNR PICTURE X(6).
           05  RELMAST-DATA-FIELDS.
               10  RELFNR                  PICTURE X(3).
               10  RELNR1                  PICTURE X(1).
               10  RELNR                   PICTURE X(6).
               10  RELKNR                  PICTURE X(6).
               10  FAKBEL-IO.
                   15  FAKBEL              PICTURE S9(7)V9(2).
               10  RELFFD                  PICTURE X(6).
               10  RELKD                   PICTURE X(1).
               10  RELBEL-IO.
                   15  RELBEL              PICTURE S9(8)V9(2).
           05  SYSPARM-DATA-FIELDS.
               10  SYSFMG                  PICTURE X(1).
      *
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  SYSKEY                  PICTURE X(10).
               10  SUMBEL-IO.
                   15  SUMBEL              PICTURE S9(8)V9(2).
               10  SUMBU1-IO.
                   15  SUMBU1              PICTURE S9(8).
               10  ANT01-IO.
                   15  ANT01               PICTURE S9(7).
               10  ANT01G-IO.
                   15  ANT01G              PICTURE S9(7).
           05  EDITTING-FIELDS.
               10  XO-82YY9R               PICTURE ZZ.ZZZ.ZZZ,99-.
               10  XO-80YY9R               PICTURE ZZ.ZZZ.ZZ9-.
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
 
           PERFORM HEADING-OUTPUT
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
           PERFORM HEADING-OUTPUT
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
           SET NOT-I-04                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  RELMAST-PROCESS
               SET RELMAST-PROCESS-OFF     TO TRUE
               SET RELMAST-READ            TO TRUE
           END-IF
 
           IF  RELMAST-READ
           AND RECORD-SELECTED-OFF
               PERFORM RELMAST-GET
               SET RELMAST-READ-OFF        TO TRUE
               IF  NOT RELMAST-EOF
                   SET RELMAST-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  RELMAST-PROCESS
               PERFORM RELMAST-IDSET
           END-IF
 
           IF  RELMAST-PROCESS
               PERFORM RELMAST-CHK-LEVEL
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
 
           IF  RELMAST-PROCESS
               PERFORM RELMAST-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  RELMAST-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-L2)
               SET NOT-I-45                TO TRUE
               SET NOT-I-46                TO TRUE
               MOVE RELFNR                 TO SYSKEY (1:3)
               MOVE 'RES*011'              TO SYSKEY (4:7)
               MOVE SYSKEY                 TO SYSPARM-KEY1
               READ SYSPARM RECORD KEY IS SYSPARM-KEY1
               INVALID KEY
                   SET I-45                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-45            TO TRUE
                   PERFORM SYSPARM-FLDSET
                   PERFORM SYSPARM-IDSET
               END-READ
           END-IF
           IF  (I-L2 AND NOT-I-45)
               SET NOT-I-46                TO TRUE
               IF  SYSFMG = 'J'
                   SET I-46                TO TRUE
               END-IF
           END-IF
           IF  (I-L1)
               SET NOT-I-30                TO TRUE
               SET NOT-I-31                TO TRUE
           END-IF
           IF  (I-L1 AND NOT-I-U1)
               SUBTRACT SUMBEL             FROM SUMBEL
           END-IF
           IF  (I-L1 AND I-U1)
               SUBTRACT SUMBU1             FROM SUMBU1
           END-IF
           SET NOT-I-40                    TO TRUE
           IF  (I-46)
               SET NOT-I-40                TO TRUE
               IF  RELNR1 = '0'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (I-46 AND I-40)
               SET NOT-I-40                TO TRUE
               IF  RELKD = 'A'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           ADD 1                           TO ANT01
           IF  (I-40)
               ADD 1                       TO ANT01G
           END-IF
           IF  (I-40 AND NOT-I-U1)
               ADD FAKBEL                  TO SUMBEL
               SET NOT-I-31                TO TRUE
               IF  SUMBEL > 0
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (I-40 AND I-U1)
               ADD FAKBEL                  TO SUMBU1
               SET NOT-I-31                TO TRUE
               IF  SUMBU1 > 0
                   SET I-31                TO TRUE
               END-IF
      *  01      BUGFL2    DEBUGBUGFILO   RELNR            VIS FELT/IND
      *  01                MOVE "RELNR   "BUGFL2  8        LEDETXT DEBUG
           END-IF
           .
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1 AND I-31 AND NOT-I-U1)
               SET NOT-I-30                TO TRUE
               IF  SUMBEL = RELBEL
                   SET I-30                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-31 AND I-U1)
               SET NOT-I-30                TO TRUE
               IF  SUMBU1 = RELBEL
                   SET I-30                TO TRUE
               END-IF
      *
           END-IF
           .
 
       RELMAST-GET SECTION.
       RELMAST-GET-P.
           IF  RELMAST-EOF-OFF
               READ RELMAST
               AT END
                   SET RELMAST-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RELMAST-FLDSET SECTION.
       RELMAST-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RELMAST-IO-AREA (2:3)  TO RELFNR (1:3)
               MOVE RELMAST-IO-AREA (5:1)  TO RELNR1 (1:1)
               MOVE RELMAST-IO-AREA (5:6)  TO RELNR (1:6)
               MOVE RELMAST-IO-AREA (21:6) TO RELKNR (1:6)
               MOVE RELMAST-IO-AREA (33:9) TO FAKBEL-IO
               INSPECT FAKBEL-IO REPLACING ALL ' ' BY '0'
               MOVE RELMAST-IO-AREA (42:6) TO RELFFD (1:6)
               MOVE RELMAST-IO-AREA (48:1) TO RELKD (1:1)
               MOVE RELMAST-IO-AREA (49:10) TO RELBEL-IO
               INSPECT RELBEL-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       RELMAST-IDSET SECTION.
       RELMAST-IDSET-P.
           SET I-01                        TO TRUE.
 
       RELMAST-CHK-LEVEL SECTION.
       RELMAST-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO RELMAST-LEVEL-01
               MOVE RELMAST-IO-AREA (2:3)  TO RELMAST-01-L2-RELFNR
               MOVE RELMAST-IO-AREA (5:6)  TO RELMAST-01-L1-RELNR
               IF  RELMAST-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  RELMAST-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  RELMAST-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  RELMAST-01-L2         TO THE-PRIOR-L2
               MOVE  RELMAST-01-L1         TO THE-PRIOR-L1
               SET RELMAST-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       SYSPARM-FLDSET SECTION.
       SYSPARM-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE SYSPARM-IO-AREA (068:1) TO SYSFMG (1:1)
           END-EVALUATE.
 
       SYSPARM-IDSET SECTION.
       SYSPARM-IDSET-P.
           SET I-04                        TO TRUE.
 
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
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'DANNING AV AUTOGIRO FRA ' TO LISTE-IO-AREA (1:24)
               MOVE 'RELMAST.                ' TO LISTE-IO-AREA (25:24)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (73:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-30)
               MOVE SPACES TO AUTOGIR-IO-AREA
               INITIALIZE AUTOGIR-IO-AREA
               MOVE 'A2'                   TO AUTOGIR-IO-AREA (1:2)
               MOVE RELFNR                 TO AUTOGIR-IO-AREA (3:3)
               MOVE RELKNR                 TO AUTOGIR-IO-AREA (6:6)
               MOVE RELNR                  TO AUTOGIR-IO-AREA (12:6)
               MOVE RELFFD                 TO AUTOGIR-IO-AREA (18:6)
               MOVE RELBEL-IO              TO AUTOGIR-IO-AREA (24:10)
               WRITE AUTOGIR-IO-AREA
           END-IF
           IF  (I-L1 AND NOT-I-30)
               MOVE SPACES TO AUTOGIR-IO-AREA
               INITIALIZE AUTOGIR-IO-AREA
               MOVE 'F2'                   TO AUTOGIR-IO-AREA (1:2)
               MOVE RELFNR                 TO AUTOGIR-IO-AREA (3:3)
               MOVE RELKNR                 TO AUTOGIR-IO-AREA (6:6)
               MOVE RELNR                  TO AUTOGIR-IO-AREA (12:6)
               MOVE RELFFD                 TO AUTOGIR-IO-AREA (18:6)
               MOVE RELBEL-IO              TO AUTOGIR-IO-AREA (24:10)
               IF  (NOT-I-31)
                   MOVE 'N'                TO AUTOGIR-IO-AREA (34:1)
               END-IF
               IF  (I-31)
                   MOVE 'U'                TO AUTOGIR-IO-AREA (34:1)
               END-IF
               WRITE AUTOGIR-IO-AREA
           END-IF
           IF  (I-L1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'REC:'                 TO LISTE-IO-AREA (1:4)
               IF  (I-30)
                   MOVE '*'                TO LISTE-IO-AREA (6:1)
               END-IF
               MOVE RELFNR                 TO LISTE-IO-AREA (8:3)
               MOVE RELKNR                 TO LISTE-IO-AREA (15:6)
               MOVE RELNR                  TO LISTE-IO-AREA (25:6)
               MOVE RELFFD                 TO LISTE-IO-AREA (35:6)
               MOVE RELBEL                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (47:14)
               IF  (NOT-I-U1)
                   MOVE SUMBEL             TO XO-82YY9R
                   MOVE XO-82YY9R          TO LISTE-IO-AREA (67:14)
               END-IF
               IF  (I-U1)
                   MOVE SUMBU1             TO XO-80YY9R
                   MOVE XO-80YY9R          TO LISTE-IO-AREA (70:11)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL LEST            :' TO LISTE-IO-AREA (1:24)
               MOVE ANT01                  TO XO-70YN9R
               MOVE XO-70YN9R              TO LISTE-IO-AREA (33:8)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL GODKJENT        :' TO LISTE-IO-AREA (1:24)
               MOVE ANT01G                 TO XO-70YN9R
               MOVE XO-70YN9R              TO LISTE-IO-AREA (33:8)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR AND I-01 AND I-04)
           AND (I-U4)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ' '                    TO LISTE-IO-AREA (24:1)
               MOVE 1                      TO LISTE-BEFORE-SPACE
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
           SET RELMAST-LEVEL-INIT          TO TRUE
           INITIALIZE RELMAST-DATA-FIELDS
           SET RELMAST-EOF-OFF             TO TRUE
           SET RELMAST-PROCESS             TO TRUE
           OPEN INPUT RELMAST
           INITIALIZE SYSPARM-DATA-FIELDS
           OPEN INPUT SYSPARM
           OPEN OUTPUT AUTOGIR
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE RELMAST
           CLOSE SYSPARM
           CLOSE AUTOGIR
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
