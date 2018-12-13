       IDENTIFICATION DIVISION.
       PROGRAM-ID. RSK130R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM.......: RSK130, DANNER RELMAST AV RELFILE.           *
      *  PROGRAMMERER..: MORTEN TUVRØNNINGEN                          *
      *  KJØRES I JOBB.: RES.XRES12/22UF                              *
      *  LAGET DATO....: 13.09.02                                     *
      *  ENDRET........: 25.03.09 TAKLER PREFIKS 962/963 FRA NY PURRE-*
      *                           RUTINE.                             *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RSK130.rpg
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
           SELECT RELFILE
               ASSIGN TO UT-S-RELFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RELFILE-STATUS.
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
       FD RELFILE
               BLOCK CONTAINS 9420
               RECORD CONTAINS 60.
       01  RELFILE-IO-AREA.
           05  RELFILE-IO-AREA-X           PICTURE X(60).
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
           10  RELFILE-STATUS              PICTURE 99 VALUE 0.
           10  KIDRELF-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  RELFILE-EOF-OFF         VALUE '0'.
               88  RELFILE-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RELFILE-READ-OFF        VALUE '0'.
               88  RELFILE-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RELFILE-PROCESS-OFF     VALUE '0'.
               88  RELFILE-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  RELFILE-LEVEL-INIT-OFF  VALUE '0'.
               88  RELFILE-LEVEL-INIT      VALUE '1'.
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
           05  RELFILE-LEVEL-01.
               10  RELFILE-01-L4.
                   15  RELFILE-01-L4-FNR   PICTURE X(3).
               10  RELFILE-01-L3.
                   15  RELFILE-01-L3-RNR   PICTURE X(6).
               10  RELFILE-01-L2.
                   15  RELFILE-01-L2-BNR   PICTURE X(6).
               10  RELFILE-01-L1.
                   15  RELFILE-01-L1-RBTO  PICTURE S9(8)V9(2).
           05  RELFILE-DATA-FIELDS.
               10  FNR                     PICTURE X(3).
               10  RNR                     PICTURE X(6).
               10  BNR                     PICTURE X(6).
               10  BNR3                    PICTURE X(3).
               10  RFNR                    PICTURE X(6).
               10  RBEL-IO.
                   15  RBEL                PICTURE S9(7)V9(2).
               10  RBTO-IO.
                   15  RBTO                PICTURE S9(8)V9(2).
               10  TKODE                   PICTURE X(2).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L4            PICTURE X(3).
               10  THE-PRIOR-L3            PICTURE X(6).
               10  THE-PRIOR-L2            PICTURE X(6).
               10  THE-PRIOR-L1            PICTURE X(10).
           05  TEMPORARY-FIELDS.
               10  ANTFAK-IO.
                   15  ANTFAK              PICTURE S9(3).
               10  ANT01-IO.
                   15  ANT01               PICTURE S9(7).
               10  ANT01I-IO.
                   15  ANT01I              PICTURE S9(7).
               10  ANT01P-IO.
                   15  ANT01P              PICTURE S9(7).
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
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  RELFILE-PROCESS
               SET RELFILE-PROCESS-OFF     TO TRUE
               SET RELFILE-READ            TO TRUE
           END-IF
 
           IF  RELFILE-READ
           AND RECORD-SELECTED-OFF
               PERFORM RELFILE-GET
               SET RELFILE-READ-OFF        TO TRUE
               IF  NOT RELFILE-EOF
                   PERFORM RELFILE-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET RELFILE-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  RELFILE-PROCESS
               PERFORM RELFILE-IDSET
           END-IF
 
           IF  RELFILE-PROCESS
               PERFORM RELFILE-CHK-LEVEL
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
 
           IF  RELFILE-PROCESS
               PERFORM RELFILE-FLDOFF
               PERFORM RELFILE-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  RELFILE-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-01)
               PERFORM RELFIL-S
      *  01                MOVE "FNR     "BUGFL2  8        LEDETXT DEBUG
      *  01      BUGFL2    DEBUGBUGFILO   FNR              VIS FELT/IND
      *
      *
           END-IF
           .
 
       RELFIL-S SECTION.
       RELFIL-S-P.
           IF  (I-L1)
               SUBTRACT ANTFAK             FROM ANTFAK
           END-IF
           ADD 1                           TO ANTFAK
           SET NOT-I-20                    TO TRUE
           SET NOT-I-21                    TO TRUE
           SET NOT-I-20                    TO TRUE
           IF  BNR3 = '960'
               SET I-20                    TO TRUE
           END-IF
           SET NOT-I-21                    TO TRUE
           IF  BNR3 = '961'
               SET I-21                    TO TRUE
           END-IF
           IF  (NOT-I-21)
               SET NOT-I-21                TO TRUE
               IF  BNR3 = '962'
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-21)
               SET NOT-I-21                TO TRUE
               IF  BNR3 = '963'
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-07)
               SET NOT-I-22                TO TRUE
               SET NOT-I-23                TO TRUE
               IF  RBEL NOT < 0
                   SET I-22                TO TRUE
               END-IF
               IF  RBEL < 0
                   SET I-23                TO TRUE
               END-IF
           END-IF
           ADD 1                           TO ANT01
           IF  (I-20)
               ADD 1                       TO ANT01I
           END-IF
           IF  (I-21)
               ADD 1                       TO ANT01P
           END-IF.
      *
 
       RELFILE-GET SECTION.
       RELFILE-GET-P.
           IF  RELFILE-EOF-OFF
               READ RELFILE
               AT END
                   SET RELFILE-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RELFILE-FLDOFF SECTION.
       RELFILE-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( RELFILE-IO-AREA (1:1) = '4'
            AND   RELFILE-IO-AREA (2:1) = '0' )
               SET NOT-I-07                TO TRUE
           END-EVALUATE.
 
       RELFILE-FLDSET SECTION.
       RELFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ( RELFILE-IO-AREA (1:1) = '4'
            AND   RELFILE-IO-AREA (2:1) = '0' )
               MOVE RELFILE-IO-AREA (3:3)  TO FNR (1:3)
               MOVE RELFILE-IO-AREA (6:6)  TO RNR (1:6)
               MOVE RELFILE-IO-AREA (12:6) TO BNR (1:6)
               MOVE RELFILE-IO-AREA (12:3) TO BNR3 (1:3)
               MOVE RELFILE-IO-AREA (18:6) TO RFNR (1:6)
               MOVE RELFILE-IO-AREA (24:9) TO RBEL-IO
               INSPECT RBEL-IO REPLACING ALL ' ' BY '0'
               MOVE RELFILE-IO-AREA (39:10) TO RBTO-IO
               INSPECT RBTO-IO REPLACING ALL ' ' BY '0'
               MOVE RELFILE-IO-AREA (59:2) TO TKODE (1:2)
               IF  TKODE = SPACES
                   SET I-07                TO TRUE
               END-IF
           END-EVALUATE.
 
       RELFILE-IDCHK SECTION.
       RELFILE-IDCHK-P.
           EVALUATE TRUE
           WHEN ( RELFILE-IO-AREA (1:1) = '4'
            AND   RELFILE-IO-AREA (2:1) = '0' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       RELFILE-IDSET SECTION.
       RELFILE-IDSET-P.
           EVALUATE TRUE
           WHEN ( RELFILE-IO-AREA (1:1) = '4'
            AND   RELFILE-IO-AREA (2:1) = '0' )
               SET I-01                    TO TRUE
           END-EVALUATE.
 
       RELFILE-CHK-LEVEL SECTION.
       RELFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( RELFILE-IO-AREA (1:1) = '4'
            AND   RELFILE-IO-AREA (2:1) = '0' )
               MOVE LOW-VALUES             TO RELFILE-LEVEL-01
               MOVE RELFILE-IO-AREA (3:3)  TO RELFILE-01-L4-FNR
               MOVE RELFILE-IO-AREA (6:6)  TO RELFILE-01-L3-RNR
               MOVE RELFILE-IO-AREA (12:6) TO RELFILE-01-L2-BNR
               MOVE RELFILE-IO-AREA (39:10) TO RELFILE-01-L1-RBTO
               IF  RELFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  RELFILE-01-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   WHEN  RELFILE-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  RELFILE-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  RELFILE-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  RELFILE-01-L4         TO THE-PRIOR-L4
               MOVE  RELFILE-01-L3         TO THE-PRIOR-L3
               MOVE  RELFILE-01-L2         TO THE-PRIOR-L2
               MOVE  RELFILE-01-L1         TO THE-PRIOR-L1
               SET RELFILE-LEVEL-INIT      TO TRUE
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
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               MOVE SPACES TO KIDRELF-IO-AREA
               INITIALIZE KIDRELF-IO-AREA
               MOVE 'A'                    TO KIDRELF-IO-AREA (1:1)
               MOVE FNR                    TO KIDRELF-IO-AREA (2:3)
               MOVE BNR                    TO KIDRELF-IO-AREA (5:6)
               MOVE RNR                    TO KIDRELF-IO-AREA (11:6)
               MOVE ' '                    TO KIDRELF-IO-AREA (17:1)
               MOVE ANTFAK-IO              TO KIDRELF-IO-AREA (18:3)
               MOVE RNR                    TO KIDRELF-IO-AREA (21:6)
               MOVE RFNR                   TO KIDRELF-IO-AREA (27:6)
               MOVE RBEL-IO                TO KIDRELF-IO-AREA (33:9)
      *                        FFAAR     43
      *                        FFMND     45
      *                        FFDAG     47
               IF  (I-20)
                   MOVE 'I'                TO KIDRELF-IO-AREA (48:1)
               END-IF
               IF  (I-21)
                   MOVE 'P'                TO KIDRELF-IO-AREA (48:1)
               END-IF
               MOVE RBTO-IO                TO KIDRELF-IO-AREA (49:10)
               MOVE UYEAR                  TO KIDRELF-IO-AREA (59:2)
               MOVE UMONTH                 TO KIDRELF-IO-AREA (61:2)
               MOVE UDAY                   TO KIDRELF-IO-AREA (63:2)
               IF  (I-22)
                   MOVE '21'               TO KIDRELF-IO-AREA (65:2)
               END-IF
               IF  (I-23)
                   MOVE '26'               TO KIDRELF-IO-AREA (65:2)
               END-IF
               WRITE KIDRELF-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'DANNING AV RELMAST FRA :' TO LISTE-IO-AREA (1:24)
               MOVE ' RELFILE                ' TO LISTE-IO-AREA (25:24)
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
               MOVE 'ANTALL FRA GIRORUTINE  :' TO LISTE-IO-AREA (1:24)
               MOVE ANT01I                 TO XO-70YN9R
               MOVE XO-70YN9R              TO LISTE-IO-AREA (33:8)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL FRA PURRERUTINE :' TO LISTE-IO-AREA (1:24)
               MOVE ANT01P                 TO XO-70YN9R
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
           SET RELFILE-LEVEL-INIT          TO TRUE
           INITIALIZE RELFILE-DATA-FIELDS
           SET RELFILE-EOF-OFF             TO TRUE
           SET RELFILE-PROCESS             TO TRUE
           OPEN INPUT RELFILE
           OPEN OUTPUT KIDRELF
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE RELFILE
           CLOSE KIDRELF
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
