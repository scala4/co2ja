       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORD029R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAMMET PLUKKER DAGENS ORDRE - * I POS 148                *
      ****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ORD029.rpg
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
           SELECT ORDREFS
               ASSIGN TO UT-S-ORDREFS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ORDREFS-STATUS.
           SELECT UTORDRE
               ASSIGN TO UT-S-UTORDRE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS UTORDRE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD ORDREFS
               BLOCK CONTAINS 4100
               RECORD CONTAINS 164.
       01  ORDREFS-IO-AREA.
           05  ORDREFS-IO-AREA-X           PICTURE X(164).
      *ISTE   O   F 132 132     OF     PRINTERSYSLST
       FD UTORDRE
               BLOCK CONTAINS 4100
               RECORD CONTAINS 164.
       01  UTORDRE-IO-AREA.
           05  UTORDRE-IO-AREA-X           PICTURE X(164).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  ORDREFS-STATUS              PICTURE 99 VALUE 0.
           10  UTORDRE-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDREFS-EOF-OFF         VALUE '0'.
               88  ORDREFS-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDREFS-READ-OFF        VALUE '0'.
               88  ORDREFS-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDREFS-PROCESS-OFF     VALUE '0'.
               88  ORDREFS-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  ORDREFS-LEVEL-INIT-OFF  VALUE '0'.
               88  ORDREFS-LEVEL-INIT      VALUE '1'.
           05  ORDREFS-LEVEL-01.
               10  ORDREFS-01-L2.
                   15  ORDREFS-01-L2-FIRMA PICTURE X(3).
               10  ORDREFS-01-L1.
                   15  ORDREFS-01-L1-ORDNR PICTURE X(6).
           05  ORDREFS-LEVEL-02.
               10  ORDREFS-02-L2.
                   15  ORDREFS-02-L2-FIRMA PICTURE X(3).
               10  ORDREFS-02-L1.
                   15  ORDREFS-02-L1-ORDNR PICTURE X(6).
           05  ORDREFS-LEVEL-03.
               10  ORDREFS-03-L2.
                   15  ORDREFS-03-L2-FIRMA PICTURE X(3).
               10  ORDREFS-03-L1.
                   15  ORDREFS-03-L1-ORDNR PICTURE X(6).
           05  ORDREFS-LEVEL-04.
               10  ORDREFS-04-L2.
                   15  ORDREFS-04-L2-FIRMA PICTURE X(3).
               10  ORDREFS-04-L1.
                   15  ORDREFS-04-L1-ORDNR PICTURE X(6).
           05  ORDREFS-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  ORDNR                   PICTURE X(6).
               10  SEQ1                    PICTURE X(1).
               10  SELGKP                  PICTURE X(1).
               10  STATUS-X                PICTURE X(1).
               10  OHREC1                  PICTURE X(164).
               10  SEQ2                    PICTURE X(1).
               10  OHREC2                  PICTURE X(164).
               10  OHREC3                  PICTURE X(164).
               10  SEQ3                    PICTURE X(1).
               10  OVREC                   PICTURE X(164).
               10  SEQ4                    PICTURE X(1).
      *
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(6).
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
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-04                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  ORDREFS-PROCESS
               SET ORDREFS-PROCESS-OFF     TO TRUE
               SET ORDREFS-READ            TO TRUE
           END-IF
 
           IF  ORDREFS-READ
           AND RECORD-SELECTED-OFF
               PERFORM ORDREFS-GET
               SET ORDREFS-READ-OFF        TO TRUE
               IF  NOT ORDREFS-EOF
                   PERFORM ORDREFS-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET ORDREFS-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  ORDREFS-PROCESS
               PERFORM ORDREFS-IDSET
           END-IF
 
           IF  ORDREFS-PROCESS
               PERFORM ORDREFS-CHK-LEVEL
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
 
           IF  ORDREFS-PROCESS
               PERFORM ORDREFS-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  ORDREFS-PROCESS
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
               SET NOT-I-21                TO TRUE
               IF  SELGKP = '*'
                   SET I-21                TO TRUE
               END-IF
           END-IF.
 
       ORDREFS-GET SECTION.
       ORDREFS-GET-P.
           IF  ORDREFS-EOF-OFF
               READ ORDREFS
               AT END
                   SET ORDREFS-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       ORDREFS-FLDSET SECTION.
       ORDREFS-FLDSET-P.
           EVALUATE TRUE
           WHEN ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) = ' '
            AND   ORDREFS-IO-AREA (20:1) = '1' )
               MOVE ORDREFS-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE ORDREFS-IO-AREA (5:6)  TO ORDNR (1:6)
               MOVE ORDREFS-IO-AREA (20:1) TO SEQ1 (1:1)
               MOVE ORDREFS-IO-AREA (148:1) TO SELGKP (1:1)
               MOVE ORDREFS-IO-AREA (164:1) TO STATUS-X (1:1)
               MOVE ORDREFS-IO-AREA (1:164) TO OHREC1 (1:164)
           WHEN ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) = ' '
            AND   ORDREFS-IO-AREA (20:1) = '2' )
               MOVE ORDREFS-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE ORDREFS-IO-AREA (5:6)  TO ORDNR (1:6)
               MOVE ORDREFS-IO-AREA (20:1) TO SEQ2 (1:1)
               MOVE ORDREFS-IO-AREA (1:164) TO OHREC2 (1:164)
           WHEN ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) = ' '
            AND   ORDREFS-IO-AREA (20:1) = '3' )
               MOVE ORDREFS-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE ORDREFS-IO-AREA (5:6)  TO ORDNR (1:6)
               MOVE ORDREFS-IO-AREA (1:164) TO OHREC3 (1:164)
               MOVE ORDREFS-IO-AREA (20:1) TO SEQ3 (1:1)
           WHEN ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) NOT = ' ' )
               MOVE ORDREFS-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE ORDREFS-IO-AREA (5:6)  TO ORDNR (1:6)
               MOVE ORDREFS-IO-AREA (1:164) TO OVREC (1:164)
               MOVE ORDREFS-IO-AREA (20:1) TO SEQ4 (1:1)
           END-EVALUATE.
 
       ORDREFS-IDCHK SECTION.
       ORDREFS-IDCHK-P.
           EVALUATE TRUE
           WHEN ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) = ' '
            AND   ORDREFS-IO-AREA (20:1) = '1' )
             OR ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) = ' '
            AND   ORDREFS-IO-AREA (20:1) = '2' )
             OR ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) = ' '
            AND   ORDREFS-IO-AREA (20:1) = '3' )
             OR ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) NOT = ' ' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       ORDREFS-IDSET SECTION.
       ORDREFS-IDSET-P.
           EVALUATE TRUE
           WHEN ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) = ' '
            AND   ORDREFS-IO-AREA (20:1) = '1' )
               SET I-01                    TO TRUE
           WHEN ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) = ' '
            AND   ORDREFS-IO-AREA (20:1) = '2' )
               SET I-02                    TO TRUE
           WHEN ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) = ' '
            AND   ORDREFS-IO-AREA (20:1) = '3' )
               SET I-03                    TO TRUE
           WHEN ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) NOT = ' ' )
               SET I-04                    TO TRUE
           END-EVALUATE.
 
       ORDREFS-CHK-LEVEL SECTION.
       ORDREFS-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) = ' '
            AND   ORDREFS-IO-AREA (20:1) = '1' )
               MOVE LOW-VALUES             TO ORDREFS-LEVEL-01
               MOVE ORDREFS-IO-AREA (2:3)  TO ORDREFS-01-L2-FIRMA
               MOVE ORDREFS-IO-AREA (5:6)  TO ORDREFS-01-L1-ORDNR
               IF  ORDREFS-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDREFS-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  ORDREFS-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDREFS-01-L2         TO THE-PRIOR-L2
               MOVE  ORDREFS-01-L1         TO THE-PRIOR-L1
               SET ORDREFS-LEVEL-INIT      TO TRUE
           WHEN ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) = ' '
            AND   ORDREFS-IO-AREA (20:1) = '2' )
               MOVE LOW-VALUES             TO ORDREFS-LEVEL-02
               MOVE ORDREFS-IO-AREA (2:3)  TO ORDREFS-02-L2-FIRMA
               MOVE ORDREFS-IO-AREA (5:6)  TO ORDREFS-02-L1-ORDNR
               IF  ORDREFS-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDREFS-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  ORDREFS-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDREFS-02-L2         TO THE-PRIOR-L2
               MOVE  ORDREFS-02-L1         TO THE-PRIOR-L1
               SET ORDREFS-LEVEL-INIT      TO TRUE
           WHEN ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) = ' '
            AND   ORDREFS-IO-AREA (20:1) = '3' )
               MOVE LOW-VALUES             TO ORDREFS-LEVEL-03
               MOVE ORDREFS-IO-AREA (2:3)  TO ORDREFS-03-L2-FIRMA
               MOVE ORDREFS-IO-AREA (5:6)  TO ORDREFS-03-L1-ORDNR
               IF  ORDREFS-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDREFS-03-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  ORDREFS-03-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDREFS-03-L2         TO THE-PRIOR-L2
               MOVE  ORDREFS-03-L1         TO THE-PRIOR-L1
               SET ORDREFS-LEVEL-INIT      TO TRUE
           WHEN ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) NOT = ' ' )
               MOVE LOW-VALUES             TO ORDREFS-LEVEL-04
               MOVE ORDREFS-IO-AREA (2:3)  TO ORDREFS-04-L2-FIRMA
               MOVE ORDREFS-IO-AREA (5:6)  TO ORDREFS-04-L1-ORDNR
               IF  ORDREFS-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDREFS-04-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  ORDREFS-04-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDREFS-04-L2         TO THE-PRIOR-L2
               MOVE  ORDREFS-04-L1         TO THE-PRIOR-L1
               SET ORDREFS-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-21)
               MOVE SPACES TO UTORDRE-IO-AREA
               INITIALIZE UTORDRE-IO-AREA
               MOVE OHREC1                 TO UTORDRE-IO-AREA (1:164)
               WRITE UTORDRE-IO-AREA
           END-IF
           IF  (I-02 AND I-21)
               MOVE SPACES TO UTORDRE-IO-AREA
               INITIALIZE UTORDRE-IO-AREA
               MOVE OHREC2                 TO UTORDRE-IO-AREA (1:164)
               WRITE UTORDRE-IO-AREA
           END-IF
           IF  (I-03 AND I-21)
               MOVE SPACES TO UTORDRE-IO-AREA
               INITIALIZE UTORDRE-IO-AREA
               MOVE OHREC3                 TO UTORDRE-IO-AREA (1:164)
               WRITE UTORDRE-IO-AREA
           END-IF
           IF  (I-04 AND I-21)
               MOVE SPACES TO UTORDRE-IO-AREA
               INITIALIZE UTORDRE-IO-AREA
               MOVE OVREC                  TO UTORDRE-IO-AREA (1:164)
      *ISTE   H  101   1P
      *                                  24 "------------------------"
      *                                  48 "------------------------"
      *                                  72 "------------------------"
      *                                  96 "------------------------"
      *                                 120 "------------------------"
      *                                 132 "------------"
      *       D  1     01 21
      *                        FIRMA      3
      *                        ORDNR     11
      *                        SEQ1      13
      *       D  1     02 21
      *                        FIRMA      3
      *                        ORDNR     11
      *                        SEQ2      13
      *       D  1     03 21
      *                        FIRMA      3
      *                        ORDNR     11
      *                        SEQ3      13
      *       D  1     04 21
      *                        FIRMA      3
      *                        ORDNR     11
      *                                  13 "4"
               WRITE UTORDRE-IO-AREA
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
           SET ORDREFS-LEVEL-INIT          TO TRUE
           INITIALIZE ORDREFS-DATA-FIELDS
           SET ORDREFS-EOF-OFF             TO TRUE
           SET ORDREFS-PROCESS             TO TRUE
           OPEN INPUT ORDREFS
           OPEN OUTPUT UTORDRE.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE ORDREFS
           CLOSE UTORDRE.
 
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
