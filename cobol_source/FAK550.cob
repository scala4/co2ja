       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK550R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM....: FAK550                                          *
      *  PROGRAMERER: STEIN SANDVOLD                                  *
      *  PROGRAMERT.: 10.01.2007                                      *
      *        .....:            AUTOMAT FAKTURA, KAN BRUKES TIL FLERE*
      *        .....:            LAGT UT FAKTURA FILE TIL PUNCH       *
      *         ....:            OUTPUT PÅ VSAM/ESDS FILE.            *
      *                          UPSI 1 LEGGER OGSÅ UT PÅ PUNCH.      *
      * OBS:ORD072 OG CICSHT01 MÅ OGSÅ RETTES NÅR NYE.                *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK550.rpg
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
           SELECT INFILE
               ASSIGN TO UT-S-INFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INFILE-STATUS.
           SELECT PUNCHUT
               ASSIGN TO UT-S-PUNCHUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PUNCHUT-STATUS.
           SELECT DISKUT
               ASSIGN TO DISKUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS DISKUT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD INFILE
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  INFILE-IO-AREA.
           05  INFILE-IO-AREA-X            PICTURE X(80).
       FD PUNCHUT
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  PUNCHUT-IO-AREA.
           05  PUNCHUT-IO-AREA-X           PICTURE X(80).
       FD DISKUT
               RECORD CONTAINS 80.
       01  DISKUT-IO-AREA.
           05  DISKUT-IO-AREA-X            PICTURE X(80).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  INFILE-STATUS               PICTURE 99 VALUE 0.
           10  PUNCHUT-STATUS              PICTURE 99 VALUE 0.
           10  DISKUT-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  INFILE-EOF-OFF          VALUE '0'.
               88  INFILE-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INFILE-READ-OFF         VALUE '0'.
               88  INFILE-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INFILE-PROCESS-OFF      VALUE '0'.
               88  INFILE-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  INFILE-LEVEL-INIT-OFF   VALUE '0'.
               88  INFILE-LEVEL-INIT       VALUE '1'.
           05  INFILE-LEVEL-01.
               10  INFILE-01-L1.
                   15  INFILE-01-L1-FIRMA  PICTURE X(3).
           05  INFILE-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  KNR                     PICTURE X(6).
      *****************************************************************
      *                                                               *
      *****************************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  FAKT-IO.
                   15  FAKT                PICTURE S9(5).
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
           IF  INFILE-PROCESS
               SET INFILE-PROCESS-OFF      TO TRUE
               SET INFILE-READ             TO TRUE
           END-IF
 
           IF  INFILE-READ
           AND RECORD-SELECTED-OFF
               PERFORM INFILE-GET
               SET INFILE-READ-OFF         TO TRUE
               IF  NOT INFILE-EOF
                   SET INFILE-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  INFILE-PROCESS
               PERFORM INFILE-IDSET
           END-IF
 
           IF  INFILE-PROCESS
               PERFORM INFILE-CHK-LEVEL
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
 
           IF  INFILE-PROCESS
               PERFORM INFILE-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  INFILE-PROCESS
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
               SET NOT-I-10                TO TRUE
               IF  FIRMA = '927'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-10)
               MOVE 1                      TO FAKT
           END-IF.
 
       INFILE-GET SECTION.
       INFILE-GET-P.
           IF  INFILE-EOF-OFF
               READ INFILE
               AT END
                   SET INFILE-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INFILE-FLDSET SECTION.
       INFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INFILE-IO-AREA (3:3)   TO FIRMA (1:3)
               MOVE INFILE-IO-AREA (6:6)   TO KNR (1:6)
           END-EVALUATE.
 
       INFILE-IDSET SECTION.
       INFILE-IDSET-P.
           SET I-01                        TO TRUE.
 
       INFILE-CHK-LEVEL SECTION.
       INFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO INFILE-LEVEL-01
               MOVE INFILE-IO-AREA (3:3)   TO INFILE-01-L1-FIRMA
               IF  INFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INFILE-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INFILE-01-L1          TO THE-PRIOR-L1
               SET INFILE-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-U1 AND I-10)
               MOVE SPACES TO PUNCHUT-IO-AREA
               INITIALIZE PUNCHUT-IO-AREA
               MOVE FIRMA                  TO PUNCHUT-IO-AREA (1:3)
               MOVE '001'                  TO PUNCHUT-IO-AREA (4:3)
               MOVE KNR                    TO PUNCHUT-IO-AREA (7:6)
               MOVE 'DIVLEIEVERK      '    TO PUNCHUT-IO-AREA (13:17)
               MOVE FAKT-IO                TO PUNCHUT-IO-AREA (30:5)
               MOVE '000000000'            TO PUNCHUT-IO-AREA (35:9)
               IF  I-U1
                   WRITE PUNCHUT-IO-AREA
               END-IF
           END-IF
           IF  (I-01 AND I-10)
               MOVE SPACES TO DISKUT-IO-AREA
               INITIALIZE DISKUT-IO-AREA
               MOVE FIRMA                  TO DISKUT-IO-AREA (1:3)
               MOVE '001'                  TO DISKUT-IO-AREA (4:3)
               MOVE KNR                    TO DISKUT-IO-AREA (7:6)
               MOVE 'DIVLEIEVERK      '    TO DISKUT-IO-AREA (13:17)
               MOVE FAKT-IO                TO DISKUT-IO-AREA (30:5)
               MOVE '000000000'            TO DISKUT-IO-AREA (35:9)
               WRITE DISKUT-IO-AREA
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
           SET INFILE-LEVEL-INIT           TO TRUE
           INITIALIZE INFILE-DATA-FIELDS
           SET INFILE-EOF-OFF              TO TRUE
           SET INFILE-PROCESS              TO TRUE
           OPEN INPUT INFILE
           IF I-U1
               OPEN OUTPUT PUNCHUT
           END-IF
           OPEN OUTPUT DISKUT.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INFILE
           IF I-U1
               CLOSE PUNCHUT
           END-IF
           CLOSE DISKUT.
 
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
