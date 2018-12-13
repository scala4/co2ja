       IDENTIFICATION DIVISION.
       PROGRAM-ID. LAG018R.
      **********************************************  Z-WIN-RPG2   ****
      *  DANNE SUMFILE TIL LAGERSTYRING ON-LINE  *
      ********************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: LAG018.rpg
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
           SELECT LAGSTYR
               ASSIGN TO UT-S-LAGSTYR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LAGSTYR-STATUS.
           SELECT VARETIL
               ASSIGN TO VARETIL
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VARETIL-STATUS
               RECORD KEY IS VARETIL-KEY1.
       DATA DIVISION.
       FILE SECTION.
       FD LAGSTYR
               BLOCK CONTAINS 5000
               RECORD CONTAINS 250.
       01  LAGSTYR-IO-AREA.
           05  LAGSTYR-IO-AREA-X           PICTURE X(250).
       FD VARETIL
               RECORD CONTAINS 200.
       01  VARETIL-IO-AREA.
           05  VARETIL-IO-AREA-X.
               10  VARETIL-KEY1            PICTURE X(12).
               10  FILLER                  PICTURE X(188).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  LAGSTYR-STATUS              PICTURE 99 VALUE 0.
           10  VARETIL-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  LAGSTYR-EOF-OFF         VALUE '0'.
               88  LAGSTYR-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  LAGSTYR-READ-OFF        VALUE '0'.
               88  LAGSTYR-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  LAGSTYR-PROCESS-OFF     VALUE '0'.
               88  LAGSTYR-PROCESS         VALUE '1'.
           05  VARETIL-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  LAGSTYR-DATA-FIELDS.
               10  RECART                  PICTURE X(2).
               10  FIRMA                   PICTURE X(3).
               10  EDBNR                   PICTURE X(7).
               10  KEY-X                   PICTURE X(10).
           05  VARETIL-DATA-FIELDS.
               10  REC                     PICTURE X(2).
               10  LEVR-IO.
                   15  LEVR                PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  LEV2-IO.
                   15  LEV2                PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  PROD4                   PICTURE X(1).
      *
           05  TEMPORARY-FIELDS.
               10  KEY12                   PICTURE X(12).
           05  EDITTING-FIELDS.
               10  XO-70P-EF.
                 15  XO-70P                PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
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
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  LAGSTYR-PROCESS
               SET LAGSTYR-PROCESS-OFF     TO TRUE
               SET LAGSTYR-READ            TO TRUE
           END-IF
 
           IF  LAGSTYR-READ
           AND RECORD-SELECTED-OFF
               PERFORM LAGSTYR-GET
               SET LAGSTYR-READ-OFF        TO TRUE
               IF  NOT LAGSTYR-EOF
                   SET LAGSTYR-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  LAGSTYR-PROCESS
               PERFORM LAGSTYR-IDSET
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
 
           IF  LAGSTYR-PROCESS
               PERFORM LAGSTYR-FLDSET
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
               MOVE '80'                   TO KEY12 (1:2)
               MOVE KEY-X                  TO KEY12 (3:10)
               MOVE KEY12                  TO VARETIL-KEY1
               READ VARETIL RECORD KEY IS VARETIL-KEY1
               INVALID KEY
                   SET I-50                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-50            TO TRUE
                   PERFORM VARETIL-FLDSET
                   PERFORM VARETIL-IDSET
               END-READ
           END-IF.
 
       LAGSTYR-GET SECTION.
       LAGSTYR-GET-P.
           IF  LAGSTYR-EOF-OFF
               READ LAGSTYR
               AT END
                   SET LAGSTYR-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       LAGSTYR-FLDSET SECTION.
       LAGSTYR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LAGSTYR-IO-AREA (1:2)  TO RECART (1:2)
               MOVE LAGSTYR-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE LAGSTYR-IO-AREA (6:7)  TO EDBNR (1:7)
               MOVE LAGSTYR-IO-AREA (3:10) TO KEY-X (1:10)
           END-EVALUATE.
 
       LAGSTYR-IDSET SECTION.
       LAGSTYR-IDSET-P.
           SET I-01                        TO TRUE.
 
       VARETIL-FLDSET SECTION.
       VARETIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VARETIL-IO-AREA (1:2)  TO REC (1:2)
               MOVE VARETIL-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE VARETIL-IO-AREA (6:7)  TO EDBNR (1:7)
               MOVE VARETIL-IO-AREA (68:4) TO LEVR-IO
               MOVE VARETIL-IO-AREA (72:4) TO LEV2-IO
               MOVE VARETIL-IO-AREA (176:1) TO PROD4 (1:1)
           END-EVALUATE.
 
       VARETIL-IDSET SECTION.
       VARETIL-IDSET-P.
           SET I-02                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND NOT-I-50)
               MOVE LEVR                   TO XO-70P
               MOVE XO-70P-EF              TO LAGSTYR-IO-AREA (192:4)
               MOVE LEV2                   TO XO-70P
               MOVE XO-70P-EF              TO LAGSTYR-IO-AREA (196:4)
               REWRITE LAGSTYR-IO-AREA
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
           INITIALIZE LAGSTYR-DATA-FIELDS
           SET LAGSTYR-EOF-OFF             TO TRUE
           SET LAGSTYR-PROCESS             TO TRUE
           OPEN I-O LAGSTYR
           INITIALIZE VARETIL-DATA-FIELDS
           OPEN INPUT VARETIL.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE LAGSTYR
           CLOSE VARETIL.
 
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
