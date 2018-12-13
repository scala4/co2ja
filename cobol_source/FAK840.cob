       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK840R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM: FAK840                                        *
      *  DANNER FAKTURA-KID RECORD FRA FAKTURA RESKRECORD.      *
      ***********************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK840.rpg
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
           SELECT FAKPAR
               ASSIGN TO UT-S-FAKPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKPAR-STATUS.
           SELECT FAKRES
               ASSIGN TO UT-S-FAKRES
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKRES-STATUS.
           SELECT KIDRELF
               ASSIGN TO UT-S-KIDRELF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KIDRELF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD FAKPAR
               BLOCK CONTAINS 200
               RECORD CONTAINS 200.
       01  FAKPAR-IO-AREA.
           05  FAKPAR-IO-AREA-X            PICTURE X(200).
       FD FAKRES
               BLOCK CONTAINS 140
               RECORD CONTAINS 70.
       01  FAKRES-IO-AREA.
           05  FAKRES-IO-AREA-X            PICTURE X(70).
       FD KIDRELF
               BLOCK CONTAINS 80
               RECORD CONTAINS 40.
       01  KIDRELF-IO-AREA.
           05  KIDRELF-IO-AREA-X           PICTURE X(40).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  FAKPAR-STATUS               PICTURE 99 VALUE 0.
           10  FAKRES-STATUS               PICTURE 99 VALUE 0.
           10  KIDRELF-STATUS              PICTURE 99 VALUE 0.
           10  KIFELT-XX-STATUS            PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKRES-EOF-OFF          VALUE '0'.
               88  FAKRES-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKRES-READ-OFF         VALUE '0'.
               88  FAKRES-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKRES-PROCESS-OFF      VALUE '0'.
               88  FAKRES-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  FAKRES-LEVEL-INIT-OFF   VALUE '0'.
               88  FAKRES-LEVEL-INIT       VALUE '1'.
      *PSDS: DATA STRUCTURE FIELDS
           05  PSDS-DATA-FIELDS.
               10  FILLER                  PICTURE X(28).
               10  R                       PICTURE X(8).
               10  FILLER                  PICTURE X(44).
           05  FILLER REDEFINES PSDS-DATA-FIELDS.
               10  FILLER                  PICTURE X(36).
               10  P-IO.
                   15  P                   PICTURE S9(3).
               10  FILLER                  PICTURE X(41).
           05  FILLER REDEFINES PSDS-DATA-FIELDS.
               10  FILLER                  PICTURE X(10).
               10  S-IO.
                   15  S                   PICTURE S9(5).
               10  FILLER                  PICTURE X(65).
      *DSDS: DATA STRUCTURE FIELDS
           05  KIFELT-XX-DATA-FIELDS.
               10  KITALL                  PICTURE X(25).
               10  FILLER                  PICTURE X(2).
           05  FILLER REDEFINES KIFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(25).
               10  KITYPE                  PICTURE X(1).
               10  KIKTR                   PICTURE X(1).
           05  FAKPAR-DATA-FIELDS.
               10  FAKTOM                  PICTURE X(2).
               10  PFAAR                   PICTURE X(2).
           05  FAKRES-LEVEL-02.
               10  FAKRES-02-L1.
                   15  FAKRES-02-L1-FIRMNR PICTURE X(3).
           05  FAKRES-DATA-FIELDS.
               10  RESKNR                  PICTURE X(6).
               10  FAKTNR                  PICTURE X(6).
               10  FFAAR                   PICTURE X(2).
               10  FFMND                   PICTURE X(2).
               10  FFDAG                   PICTURE X(2).
               10  FAKBEL-IO.
                   15  FAKBEL              PICTURE S9(7)V9(2).
               10  FIRMNR                  PICTURE X(3).
      *****************************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  FAKKID                  PICTURE X(7).
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
           SET NOT-I-02                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  FAKRES-PROCESS
               SET FAKRES-PROCESS-OFF      TO TRUE
               SET FAKRES-READ             TO TRUE
           END-IF
 
           IF  FAKRES-READ
           AND RECORD-SELECTED-OFF
               PERFORM FAKRES-GET
               SET FAKRES-READ-OFF         TO TRUE
               IF  NOT FAKRES-EOF
                   SET FAKRES-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  FAKRES-PROCESS
               PERFORM FAKRES-IDSET
           END-IF
 
           IF  FAKRES-PROCESS
               PERFORM FAKRES-CHK-LEVEL
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
 
           IF  FAKRES-PROCESS
               PERFORM FAKRES-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  FAKRES-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-50                    TO TRUE
      *****************************************************************
      * LESE FAKTURAPARAMTER FOR Å FINNE FAKTURAOMGANGSNR.            *
      *****************************************************************
           IF  (I-L1 AND NOT-I-22)
               READ FAKPAR
               AT END
                   SET I-H0                TO TRUE
                   MOVE 'M'                TO E-R-R-O-R
               NOT AT END
                   PERFORM FAKPAR-FLDSET
                   PERFORM FAKPAR-IDSET
               END-READ
           END-IF
           IF  (I-L1)
               SET I-22                    TO TRUE
      *****************************************************************
      * BEREGNE MODULUS 10 KONTROLLSIFFER FOR FAKTURA-KID             *
      *****************************************************************
           END-IF
           MOVE FAKTNR                     TO FAKKID (1:6)
           MOVE '0'                        TO FAKKID (7:1)
           MOVE FAKKID                     TO KITALL (1:7)
           MOVE 'T'                        TO KITYPE
           MOVE ' '                        TO KIKTR
           CALL 'MODULUS' USING KIFELT-XX-DATA-FIELDS
           MOVE FAKTNR                     TO FAKKID (1:6)
           MOVE KIKTR                      TO FAKKID (7:1)
      *****************************************************************
           IF  (I-02)
               SET I-50                    TO TRUE
           END-IF.
 
       FAKPAR-FLDSET SECTION.
       FAKPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKPAR-IO-AREA (10:2)  TO FAKTOM (1:2)
               MOVE FAKPAR-IO-AREA (22:2)  TO PFAAR (1:2)
           END-EVALUATE.
 
       FAKPAR-IDSET SECTION.
       FAKPAR-IDSET-P.
           SET I-01                        TO TRUE.
 
       FAKRES-GET SECTION.
       FAKRES-GET-P.
           IF  FAKRES-EOF-OFF
               READ FAKRES
               AT END
                   SET FAKRES-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FAKRES-FLDSET SECTION.
       FAKRES-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKRES-IO-AREA (5:6)   TO RESKNR (1:6)
               MOVE FAKRES-IO-AREA (17:6)  TO FAKTNR (1:6)
               MOVE FAKRES-IO-AREA (29:2)  TO FFAAR (1:2)
               MOVE FAKRES-IO-AREA (31:2)  TO FFMND (1:2)
               MOVE FAKRES-IO-AREA (33:2)  TO FFDAG (1:2)
               MOVE FAKRES-IO-AREA (35:9)  TO FAKBEL-IO
               INSPECT FAKBEL-IO REPLACING ALL ' ' BY '0'
               MOVE FAKRES-IO-AREA (44:3)  TO FIRMNR (1:3)
           END-EVALUATE.
 
       FAKRES-IDSET SECTION.
       FAKRES-IDSET-P.
           SET I-02                        TO TRUE.
 
       FAKRES-CHK-LEVEL SECTION.
       FAKRES-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO FAKRES-LEVEL-02
               MOVE FAKRES-IO-AREA (44:3)  TO FAKRES-02-L1-FIRMNR
               IF  FAKRES-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  FAKRES-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  FAKRES-02-L1          TO THE-PRIOR-L1
               SET FAKRES-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-02 AND I-50)
               MOVE SPACES TO KIDRELF-IO-AREA
               INITIALIZE KIDRELF-IO-AREA
               MOVE 'F'                    TO KIDRELF-IO-AREA (1:1)
               MOVE FIRMNR                 TO KIDRELF-IO-AREA (2:3)
               MOVE FAKKID                 TO KIDRELF-IO-AREA (5:7)
               MOVE RESKNR                 TO KIDRELF-IO-AREA (12:6)
               MOVE FAKBEL-IO              TO KIDRELF-IO-AREA (18:9)
               MOVE FFAAR                  TO KIDRELF-IO-AREA (27:2)
               MOVE FFMND                  TO KIDRELF-IO-AREA (29:2)
               MOVE FFDAG                  TO KIDRELF-IO-AREA (31:2)
               MOVE PFAAR                  TO KIDRELF-IO-AREA (37:2)
               MOVE FAKTOM                 TO KIDRELF-IO-AREA (39:2)
               WRITE KIDRELF-IO-AREA
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
           INITIALIZE FAKPAR-DATA-FIELDS
           OPEN INPUT FAKPAR
           SET FAKRES-LEVEL-INIT           TO TRUE
           INITIALIZE FAKRES-DATA-FIELDS
           SET FAKRES-EOF-OFF              TO TRUE
           SET FAKRES-PROCESS              TO TRUE
           OPEN INPUT FAKRES
           OPEN OUTPUT KIDRELF.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FAKPAR
           CLOSE FAKRES
           CLOSE KIDRELF.
 
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
