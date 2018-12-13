       IDENTIFICATION DIVISION.
       PROGRAM-ID. EDI025R.
      **********************************************  Z-WIN-RPG2   ****
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: EDI025.rpg
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
           SELECT PARAM
               ASSIGN TO UT-S-PARAM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PARAM-STATUS.
           SELECT FRAKTED
               ASSIGN TO UT-S-FRAKTED
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FRAKTED-STATUS.
           SELECT OUTPUN
               ASSIGN TO UT-S-OUTPUN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTPUN-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD PARAM
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  PARAM-IO-AREA.
           05  PARAM-IO-AREA-X             PICTURE X(80).
       FD FRAKTED
               BLOCK CONTAINS 4000
               RECORD CONTAINS 2000.
       01  FRAKTED-IO-AREA.
           05  FRAKTED-IO-AREA-X           PICTURE X(2000).
       FD OUTPUN
               BLOCK CONTAINS 128
               RECORD CONTAINS 128.
       01  OUTPUN-IO-AREA.
           05  OUTPUN-IO-AREA-X            PICTURE X(128).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  PARAM-STATUS                PICTURE 99 VALUE 0.
           10  FRAKTED-STATUS              PICTURE 99 VALUE 0.
           10  OUTPUN-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-EOF-OFF           VALUE '0'.
               88  PARAM-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-READ-OFF          VALUE '0'.
               88  PARAM-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-PROCESS-OFF       VALUE '0'.
               88  PARAM-PROCESS           VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FRAKTED-EOF-OFF         VALUE '0'.
               88  FRAKTED-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FRAKTED-READ-OFF        VALUE '0'.
               88  FRAKTED-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FRAKTED-PROCESS-OFF     VALUE '0'.
               88  FRAKTED-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  FRAKTED-LEVEL-INIT-OFF  VALUE '0'.
               88  FRAKTED-LEVEL-INIT      VALUE '1'.
           05  PARAM-DATA-FIELDS.
               10  PFNR                    PICTURE X(3).
           05  FRAKTED-LEVEL-01.
               10  FRAKTED-01-L2.
                   15  FRAKTED-01-L2-FNR   PICTURE X(3).
               10  FRAKTED-01-L1.
                   15  FRAKTED-01-L1-KUNDE PICTURE X(6).
           05  FRAKTED-DATA-FIELDS.
               10  FNR                     PICTURE X(3).
               10  ORDRE                   PICTURE X(6).
               10  MNAVN                   PICTURE X(30).
               10  MADR                    PICTURE X(30).
               10  KUNDE                   PICTURE X(6).
               10  RUTE                    PICTURE X(2).
               10  BETAL                   PICTURE X(1).
               10  BRUTTO-IO.
                   15  BRUTTO              PICTURE S9(4)V9(1).
               10  DATO-IO.
                   15  DATO                PICTURE S9(6).
               10  MM-IO.
                   15  MM                  PICTURE S9(2).
               10  FSUM-IO.
                   15  FSUM                PICTURE S9(6).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  MND-IO.
                   15  MND                 PICTURE S9(2).
               10  L1ARUT-IO.
                   15  L1ARUT              PICTURE S9(6).
               10  L1SUM-IO.
                   15  L1SUM               PICTURE S9(6).
           05  EDITTING-FIELDS.
               10  XO-60YYZ                PICTURE ZZZ.ZZZ.
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
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
           PERFORM HEADING-OUTPUT
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-02                    TO TRUE
           SET NOT-I-01                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  PARAM-PROCESS
               SET PARAM-PROCESS-OFF       TO TRUE
               SET PARAM-READ              TO TRUE
           END-IF
 
           IF  PARAM-READ
           AND RECORD-SELECTED-OFF
               PERFORM PARAM-GET
               SET PARAM-READ-OFF          TO TRUE
               IF  NOT PARAM-EOF
                   PERFORM PARAM-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET PARAM-PROCESS       TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  FRAKTED-PROCESS
               SET FRAKTED-PROCESS-OFF     TO TRUE
               SET FRAKTED-READ            TO TRUE
           END-IF
 
           IF  FRAKTED-READ
           AND RECORD-SELECTED-OFF
               PERFORM FRAKTED-GET
               SET FRAKTED-READ-OFF        TO TRUE
               IF  NOT FRAKTED-EOF
                   SET FRAKTED-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  PARAM-PROCESS
               PERFORM PARAM-IDSET
           END-IF
 
           IF  FRAKTED-PROCESS
               PERFORM FRAKTED-IDSET
           END-IF
 
           IF  FRAKTED-PROCESS
               PERFORM FRAKTED-CHK-LEVEL
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
 
           IF  PARAM-PROCESS
               PERFORM PARAM-FLDSET
           END-IF
 
           IF  FRAKTED-PROCESS
               PERFORM FRAKTED-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  FRAKTED-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-40                    TO TRUE
           IF  UMONTH = 01
               SET I-40                    TO TRUE
           END-IF
           IF  (I-40)
               MOVE 12                     TO MND-IO
           END-IF
           IF  (NOT-I-40)
               SUBTRACT 1 FROM UMONTH  GIVING MND
      * * * * * *
           END-IF
           SET NOT-I-61                    TO TRUE
           IF  FNR = PFNR
               SET I-61                    TO TRUE
           END-IF
           IF  (NOT-I-61)
               GO TO END-X-T
           END-IF
           SET NOT-I-61                    TO TRUE
           IF  MM = MND
               SET I-61                    TO TRUE
           END-IF
           IF  (NOT-I-61)
               GO TO END-X-T
           END-IF
           SET NOT-I-62                    TO TRUE
           IF  BETAL = 'X'
               SET I-62                    TO TRUE
           END-IF
           ADD 1                           TO L1ARUT
           IF  (NOT-I-62)
               ADD FSUM                    TO L1SUM
           END-IF.
 
       END-X-T.
           CONTINUE.
 
       PARAM-GET SECTION.
       PARAM-GET-P.
           IF  PARAM-EOF-OFF
               READ PARAM
               AT END
                   SET PARAM-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       PARAM-FLDSET SECTION.
       PARAM-FLDSET-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = '9'
            AND   PARAM-IO-AREA (2:1) = '0' )
               MOVE PARAM-IO-AREA (8:3)    TO PFNR (1:3)
           END-EVALUATE.
 
       PARAM-IDCHK SECTION.
       PARAM-IDCHK-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = '9'
            AND   PARAM-IO-AREA (2:1) = '0' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       PARAM-IDSET SECTION.
       PARAM-IDSET-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = '9'
            AND   PARAM-IO-AREA (2:1) = '0' )
               SET I-02                    TO TRUE
           END-EVALUATE.
 
       FRAKTED-GET SECTION.
       FRAKTED-GET-P.
           IF  FRAKTED-EOF-OFF
               READ FRAKTED
               AT END
                   SET FRAKTED-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FRAKTED-FLDSET SECTION.
       FRAKTED-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FRAKTED-IO-AREA (4:3)  TO FNR (1:3)
               MOVE FRAKTED-IO-AREA (12:6) TO ORDRE (1:6)
               MOVE FRAKTED-IO-AREA (211:30) TO MNAVN (1:30)
               MOVE FRAKTED-IO-AREA (241:30) TO MADR (1:30)
               MOVE FRAKTED-IO-AREA (342:6) TO KUNDE (1:6)
               MOVE FRAKTED-IO-AREA (401:2) TO RUTE (1:2)
               MOVE FRAKTED-IO-AREA (432:1) TO BETAL (1:1)
               MOVE FRAKTED-IO-AREA (1001:5) TO BRUTTO-IO
               INSPECT BRUTTO-IO REPLACING ALL ' ' BY '0'
               MOVE FRAKTED-IO-AREA (1045:6) TO DATO-IO
               INSPECT DATO-IO REPLACING ALL ' ' BY '0'
               MOVE FRAKTED-IO-AREA (1047:2) TO MM-IO
               INSPECT MM-IO REPLACING ALL ' ' BY '0'
               MOVE FRAKTED-IO-AREA (1620:6) TO FSUM-IO
               INSPECT FSUM-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       FRAKTED-IDSET SECTION.
       FRAKTED-IDSET-P.
           SET I-01                        TO TRUE.
 
       FRAKTED-CHK-LEVEL SECTION.
       FRAKTED-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO FRAKTED-LEVEL-01
               MOVE FRAKTED-IO-AREA (4:3)  TO FRAKTED-01-L2-FNR
               MOVE FRAKTED-IO-AREA (342:6) TO FRAKTED-01-L1-KUNDE
               IF  FRAKTED-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  FRAKTED-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  FRAKTED-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  FRAKTED-01-L2         TO THE-PRIOR-L2
               MOVE  FRAKTED-01-L1         TO THE-PRIOR-L1
               SET FRAKTED-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-61)
               MOVE SPACES TO OUTPUN-IO-AREA
               INITIALIZE OUTPUN-IO-AREA
               MOVE DATO                   TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO OUTPUN-IO-AREA (1:8)
               MOVE ';'                    TO OUTPUN-IO-AREA (9:1)
               MOVE KUNDE                  TO OUTPUN-IO-AREA (10:6)
               MOVE ';'                    TO OUTPUN-IO-AREA (16:1)
               MOVE MNAVN                  TO OUTPUN-IO-AREA (17:30)
               MOVE ';'                    TO OUTPUN-IO-AREA (47:1)
               MOVE MADR                   TO OUTPUN-IO-AREA (48:30)
               MOVE ';'                    TO OUTPUN-IO-AREA (78:1)
               MOVE L1ARUT                 TO XO-60YYZ
               MOVE XO-60YYZ               TO OUTPUN-IO-AREA (81:7)
               INITIALIZE L1ARUT
               MOVE ';'                    TO OUTPUN-IO-AREA (88:1)
               MOVE L1SUM                  TO XO-60YYZ
               MOVE XO-60YYZ               TO OUTPUN-IO-AREA (94:7)
               INITIALIZE L1SUM
               MOVE ';'                    TO OUTPUN-IO-AREA (101:1)
               MOVE RUTE                   TO OUTPUN-IO-AREA (102:2)
               MOVE ';'                    TO OUTPUN-IO-AREA (104:1)
               IF  (NOT-I-62)
                   MOVE 'ANFØR'            TO OUTPUN-IO-AREA (105:5)
               END-IF
               MOVE ';'                    TO OUTPUN-IO-AREA (110:1)
               MOVE ORDRE                  TO OUTPUN-IO-AREA (111:6)
               WRITE OUTPUN-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P)
               MOVE SPACES TO OUTPUN-IO-AREA
               INITIALIZE OUTPUN-IO-AREA
               MOVE 'DATO;'                TO OUTPUN-IO-AREA (1:5)
               MOVE 'KUNDE;'               TO OUTPUN-IO-AREA (6:6)
               MOVE 'ADRESSE;'             TO OUTPUN-IO-AREA (12:8)
               MOVE 'ADRESSE;'             TO OUTPUN-IO-AREA (20:8)
               MOVE 'ANT.DROP;'            TO OUTPUN-IO-AREA (28:9)
               MOVE 'BELØP;RUTE'           TO OUTPUN-IO-AREA (37:10)
               WRITE OUTPUN-IO-AREA
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
           MOVE 2                          TO LR-CHECK
           INITIALIZE PARAM-DATA-FIELDS
           SET PARAM-EOF-OFF               TO TRUE
           SET PARAM-PROCESS               TO TRUE
           OPEN INPUT PARAM
           SET FRAKTED-LEVEL-INIT          TO TRUE
           INITIALIZE FRAKTED-DATA-FIELDS
           SET FRAKTED-EOF-OFF             TO TRUE
           SET FRAKTED-PROCESS             TO TRUE
           OPEN INPUT FRAKTED
           OPEN OUTPUT OUTPUN.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PARAM
           CLOSE FRAKTED
           CLOSE OUTPUN.
 
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
