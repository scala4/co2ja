       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRI503R.
      **********************************************  Z-WIN-RPG2   ****
      * DANNING AV PRISLISTERECORDS. UTEN SIDEHOPP PR AVD.      *
      * UPSI BLANK  NU1  = VANLIG 48 LINJERS LISTEPAPIR.        *
      * UPSI 1       U1  =  72 LINJERS PAPIR.                   *
      * 5/5-95 HAFNOR SKAL HA 10 % PÅSLAG PÅ VARE I AVD. SOM    *
      *        SOM ER MERKET MED PÅ BEST.                       *
      *21/1-04 HAFNOR SKAL IKKE LENGRE HA 10% PÅSLAG PÅ NOE.    *
      ***********************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: PRI503.rpg
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
           SELECT PKORT
               ASSIGN TO UT-S-PKORT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PKORT-STATUS.
           SELECT INF
               ASSIGN TO UT-S-INF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INF-STATUS.
           SELECT UTF
               ASSIGN TO UT-S-UTF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS UTF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD PKORT
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  PKORT-IO-AREA.
           05  PKORT-IO-AREA-X             PICTURE X(80).
       FD INF
               BLOCK CONTAINS 2400
               RECORD CONTAINS 200.
       01  INF-IO-AREA.
           05  INF-IO-AREA-X               PICTURE X(200).
       FD UTF
               BLOCK CONTAINS 9440
               RECORD CONTAINS 80.
       01  UTF-IO-AREA.
           05  UTF-IO-AREA-X               PICTURE X(80).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  PKORT-STATUS                PICTURE 99 VALUE 0.
           10  INF-STATUS                  PICTURE 99 VALUE 0.
           10  UTF-STATUS                  PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  PKORT-EOF-OFF           VALUE '0'.
               88  PKORT-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PKORT-READ-OFF          VALUE '0'.
               88  PKORT-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PKORT-PROCESS-OFF       VALUE '0'.
               88  PKORT-PROCESS           VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INF-EOF-OFF             VALUE '0'.
               88  INF-EOF                 VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INF-READ-OFF            VALUE '0'.
               88  INF-READ                VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INF-PROCESS-OFF         VALUE '0'.
               88  INF-PROCESS             VALUE '1'.
           05  PKORT-DATA-FIELDS.
               10  PROS-IO.
                   15  PROS                PICTURE S9(3).
           05  INF-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  ALF                     PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  VBET                    PICTURE X(23).
               10  PRIS-IO.
                   15  PRIS                PICTURE S9(7)V9(2).
               10  KDATO-IO.
                   15  KDATO               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  PT                      PICTURE X(1).
               10  VGR                     PICTURE X(5).
               10  AVD                     PICTURE X(1).
               10  MERK                    PICTURE X(1).
           05  TEMPORARY-FIELDS.
               10  NYPRO-IO.
                   15  NYPRO               PICTURE S9(1)V9(2).
               10  POS-X                   PICTURE X(1).
               10  SNR-IO.
                   15  SNR                 PICTURE S9(4).
               10  PRIS2-IO.
                   15  PRIS2               PICTURE S9(7)V9(2).
               10  PRIS3                   PICTURE X(9).
               10  LNR-IO.
                   15  LNR                 PICTURE S9(2).
               10  POSA-IO.
                   15  POSA                PICTURE S9(1).
               10  NULL-X-IO.
                   15  NULL-X              PICTURE S9(5)V9(2).
           05  EDITTING-FIELDS.
               10  XO-30YNZ                PICTURE ZZZ.
               10  XO-52P-EF.
                 15  XO-52P                PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-70P-EF.
                 15  XO-70P                PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  XO-72P-EF.
                 15  XO-72P                PICTURE S9(7)V9(2) USAGE
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
           SET NOT-I-02                    TO TRUE
           SET NOT-I-01                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  PKORT-PROCESS
               SET PKORT-PROCESS-OFF       TO TRUE
               SET PKORT-READ              TO TRUE
           END-IF
 
           IF  PKORT-READ
           AND RECORD-SELECTED-OFF
               PERFORM PKORT-GET
               SET PKORT-READ-OFF          TO TRUE
               IF  NOT PKORT-EOF
                   PERFORM PKORT-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET PKORT-PROCESS       TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  INF-PROCESS
               SET INF-PROCESS-OFF         TO TRUE
               SET INF-READ                TO TRUE
           END-IF
 
           IF  INF-READ
           AND RECORD-SELECTED-OFF
               PERFORM INF-GET
               SET INF-READ-OFF            TO TRUE
               IF  NOT INF-EOF
                   PERFORM INF-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET INF-PROCESS         TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  PKORT-PROCESS
               PERFORM PKORT-IDSET
           END-IF
 
           IF  INF-PROCESS
               PERFORM INF-IDSET
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
 
           IF  PKORT-PROCESS
               PERFORM PKORT-FLDOFF
               PERFORM PKORT-FLDSET
           END-IF
 
           IF  INF-PROCESS
               PERFORM INF-FLDSET
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
           IF  (I-02)
               DIVIDE PROS BY 100      GIVING NYPRO ROUNDED
               ADD 1,00                    TO NYPRO
               MOVE '1'                    TO POS-X
               MOVE 1                      TO SNR
               GO TO SLUTT-T
      *****************************************************************
      * PRISPÅSLAG PÅ 10 % OM DET ER SKAFFEVARE I AVD. 1 HOS HAFNOR.  *
      *****************************************************************
      *          FIRMA     COMP "950"                    95  = HAFNOR
      * N95                GOTO XPRUT1
      *          AVD       COMP "1"                      95  = AVD. 1
      * N95                GOTO XPRUT1
      *          MERK      COMP "2"                      95  = PÅ BEST.
      * N95                GOTO XPRUT1
      *          PRIS      MULT 1,10      PRIS    92H
      *          XPRUT1    TAG
      *****************************************************************
      *  VANLIG PRISBEREGNING.                                        *
      *****************************************************************
           END-IF
           IF  (I-08)
               GO TO START-X-T
           END-IF
           MULTIPLY NYPRO BY PRIS      GIVING PRIS2
           ADD 0,09                        TO PRIS2
           MOVE PRIS2                      TO PRIS3
           MOVE 0                          TO PRIS3 (9:1)
           MOVE PRIS3                      TO PRIS-IO
           SET I-15                        TO TRUE.
 
       START-X-T.
           SET NOT-I-45                    TO TRUE
      *
           IF  (I-25)
               SUBTRACT LNR                FROM LNR
           END-IF
           IF  (I-26)
               ADD 1                       TO SNR
           END-IF
           IF  (I-25)
               MOVE '2'                    TO POS-X
           END-IF
           IF  (I-26)
               MOVE '1'                    TO POS-X
           END-IF
           SET NOT-I-25                    TO TRUE
           SET NOT-I-26                    TO TRUE
           ADD 1                           TO LNR
      *
           IF  (NOT-I-U1)
               SET NOT-I-40                TO TRUE
               IF  LNR NOT < 35
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (I-U1)
               SET NOT-I-40                TO TRUE
               IF  LNR NOT < 60
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (I-40)
               ADD 1                       TO POSA
               SET NOT-I-41                TO TRUE
               IF  POSA NOT < 2
                   SET I-41                TO TRUE
               END-IF
           END-IF
           IF  (I-41)
               MOVE 0                      TO POSA
               SET I-25                    TO TRUE
               SET I-26                    TO TRUE
               SET I-45                    TO TRUE
           END-IF
           IF  (I-40)
               SET I-25                    TO TRUE
           END-IF
           SET NOT-I-40                    TO TRUE
           SET NOT-I-41                    TO TRUE.
 
       SLUTT-T.
           MOVE 0,00                       TO NULL-X.
 
       PKORT-GET SECTION.
       PKORT-GET-P.
           IF  PKORT-EOF-OFF
               READ PKORT
               AT END
                   SET PKORT-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       PKORT-FLDOFF SECTION.
       PKORT-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( PKORT-IO-AREA (1:1) = '9'
            AND   PKORT-IO-AREA (2:1) = '0' )
               SET NOT-I-08                TO TRUE
           END-EVALUATE.
 
       PKORT-FLDSET SECTION.
       PKORT-FLDSET-P.
           EVALUATE TRUE
           WHEN ( PKORT-IO-AREA (1:1) = '9'
            AND   PKORT-IO-AREA (2:1) = '0' )
               MOVE PKORT-IO-AREA (20:3)   TO PROS-IO
               INSPECT PROS-IO REPLACING ALL ' ' BY '0'
               IF  PROS = ZERO
                   SET I-08                TO TRUE
               END-IF
           END-EVALUATE.
 
       PKORT-IDCHK SECTION.
       PKORT-IDCHK-P.
           EVALUATE TRUE
           WHEN ( PKORT-IO-AREA (1:1) = '9'
            AND   PKORT-IO-AREA (2:1) = '0' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       PKORT-IDSET SECTION.
       PKORT-IDSET-P.
           EVALUATE TRUE
           WHEN ( PKORT-IO-AREA (1:1) = '9'
            AND   PKORT-IO-AREA (2:1) = '0' )
               SET I-02                    TO TRUE
           END-EVALUATE.
 
       INF-GET SECTION.
       INF-GET-P.
           IF  INF-EOF-OFF
               READ INF
               AT END
                   SET INF-EOF             TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INF-FLDSET SECTION.
       INF-FLDSET-P.
           EVALUATE TRUE
           WHEN ( INF-IO-AREA (1:1) = '7' )
               MOVE INF-IO-AREA (3:3)      TO FIRMA (1:3)
               MOVE INF-IO-AREA (13:3)     TO ALF (1:3)
               MOVE INF-IO-AREA (16:20)    TO ARTNR (1:20)
               MOVE INF-IO-AREA (36:23)    TO VBET (1:23)
               MOVE INF-IO-AREA (75:9)     TO PRIS-IO
               INSPECT PRIS-IO REPLACING ALL ' ' BY '0'
               MOVE INF-IO-AREA (84:4)     TO KDATO-IO
               MOVE INF-IO-AREA (95:1)     TO PT (1:1)
               MOVE INF-IO-AREA (118:5)    TO VGR (1:5)
               MOVE INF-IO-AREA (118:1)    TO AVD (1:1)
               MOVE INF-IO-AREA (127:1)    TO MERK (1:1)
           END-EVALUATE.
 
       INF-IDCHK SECTION.
       INF-IDCHK-P.
           EVALUATE TRUE
           WHEN ( INF-IO-AREA (1:1) = '7' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       INF-IDSET SECTION.
       INF-IDSET-P.
           EVALUATE TRUE
           WHEN ( INF-IO-AREA (1:1) = '7' )
               SET I-01                    TO TRUE
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-45 AND I-15)
               MOVE SPACES TO UTF-IO-AREA
               INITIALIZE UTF-IO-AREA
               MOVE SNR-IO                 TO UTF-IO-AREA (1:4)
               MOVE '01'                   TO UTF-IO-AREA (5:2)
               MOVE POS-X                  TO UTF-IO-AREA (7:1)
      *                                  10 "***"
      *                                  22 "AVDELING"
      *                        AVD       24
      *                                  30 "***"
               IF  (NOT-I-15)
                   MOVE '* PRIS INKL.'     TO UTF-IO-AREA (31:12)
               END-IF
               IF  (NOT-I-15)
                   MOVE PROS               TO XO-30YNZ
                   MOVE XO-30YNZ           TO UTF-IO-AREA (43:3)
               END-IF
               IF  (NOT-I-15)
                   MOVE 'PROS. *'          TO UTF-IO-AREA (47:7)
               END-IF
               MOVE NULL-X                 TO XO-52P
               MOVE XO-52P-EF              TO UTF-IO-AREA (55:4)
               MOVE FIRMA                  TO UTF-IO-AREA (61:3)
               MOVE KDATO                  TO XO-70P
               MOVE XO-70P-EF              TO UTF-IO-AREA (64:4)
               WRITE UTF-IO-AREA
               MOVE SPACES TO UTF-IO-AREA
               INITIALIZE UTF-IO-AREA
               MOVE SNR-IO                 TO UTF-IO-AREA (1:4)
               MOVE '02'                   TO UTF-IO-AREA (5:2)
               MOVE POS-X                  TO UTF-IO-AREA (7:1)
      *                                  10 "***"
      *                                  30 "**************** ***"
               IF  (I-15)
                   MOVE '************************' TO UTF-IO-AREA
                                                               (31:24)
               END-IF
               MOVE NULL-X                 TO XO-52P
               MOVE XO-52P-EF              TO UTF-IO-AREA (55:4)
               MOVE FIRMA                  TO UTF-IO-AREA (61:3)
               MOVE KDATO                  TO XO-70P
               MOVE XO-70P-EF              TO UTF-IO-AREA (64:4)
               WRITE UTF-IO-AREA
           END-IF
           IF  (I-01)
               MOVE SPACES TO UTF-IO-AREA
               INITIALIZE UTF-IO-AREA
               MOVE SNR-IO                 TO UTF-IO-AREA (1:4)
               MOVE LNR-IO                 TO UTF-IO-AREA (5:2)
               MOVE POS-X                  TO UTF-IO-AREA (7:1)
               MOVE ALF                    TO UTF-IO-AREA (8:3)
               MOVE ARTNR                  TO UTF-IO-AREA (11:20)
               MOVE VBET                   TO UTF-IO-AREA (32:23)
               MOVE PRIS                   TO XO-72P
               MOVE XO-72P-EF              TO UTF-IO-AREA (54:5)
               MOVE PT                     TO UTF-IO-AREA (59:1)
               MOVE FIRMA                  TO UTF-IO-AREA (61:3)
               MOVE KDATO                  TO XO-70P
               MOVE XO-70P-EF              TO UTF-IO-AREA (64:4)
               MOVE VGR                    TO UTF-IO-AREA (68:5)
               WRITE UTF-IO-AREA
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
           INITIALIZE PKORT-DATA-FIELDS
           SET PKORT-EOF-OFF               TO TRUE
           SET PKORT-PROCESS               TO TRUE
           OPEN INPUT PKORT
           INITIALIZE INF-DATA-FIELDS
           SET INF-EOF-OFF                 TO TRUE
           SET INF-PROCESS                 TO TRUE
           OPEN INPUT INF
           OPEN OUTPUT UTF.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PKORT
           CLOSE INF
           CLOSE UTF.
 
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
