       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK070R.
      ******************************************* :   Z-WIN-RPG2     **
      *  PROGRAM: FAK070                                              *
      *  LAGE BRUDDNR. PR. FAKTURA.                                   *
      *  LAGE HJELPEFILE FOR FAKTURA OVER 21 LINJER (FAKTURA FLERSIDE)*
      *  LAGE HJELPEFILE FOR FAKTURA MED 0 I LEVERT.                  *
      * 22.12.05  FJERNET TEST PÅ FAKTURAFLERSIDE (11)
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK070.rpg
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
           SELECT FAKTIN
               ASSIGN TO UT-S-FAKTIN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKTIN-STATUS.
           SELECT FLOPP
               ASSIGN TO UT-S-FLOPP
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FLOPP-STATUS.
           SELECT NULLORD
               ASSIGN TO UT-S-NULLORD
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS NULLORD-STATUS.
           SELECT FAKTUT
               ASSIGN TO UT-S-FAKTUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKTUT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD FAKTIN
               BLOCK CONTAINS 430
               RECORD CONTAINS 215.
       01  FAKTIN-IO-AREA.
           05  FAKTIN-IO-AREA-X            PICTURE X(215).
       FD FLOPP
               BLOCK CONTAINS 400
               RECORD CONTAINS 40.
       01  FLOPP-IO-AREA.
           05  FLOPP-IO-AREA-X             PICTURE X(40).
       FD NULLORD
               BLOCK CONTAINS 400
               RECORD CONTAINS 40.
       01  NULLORD-IO-AREA.
           05  NULLORD-IO-AREA-X           PICTURE X(40).
       FD FAKTUT
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  FAKTUT-IO-AREA.
           05  FAKTUT-IO-AREA-X            PICTURE X(200).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  FAKTIN-STATUS               PICTURE 99 VALUE 0.
           10  FLOPP-STATUS                PICTURE 99 VALUE 0.
           10  NULLORD-STATUS              PICTURE 99 VALUE 0.
           10  FAKTUT-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKTIN-EOF-OFF          VALUE '0'.
               88  FAKTIN-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKTIN-READ-OFF         VALUE '0'.
               88  FAKTIN-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKTIN-PROCESS-OFF      VALUE '0'.
               88  FAKTIN-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  FAKTIN-LEVEL-INIT-OFF   VALUE '0'.
               88  FAKTIN-LEVEL-INIT       VALUE '1'.
           05  FAKTIN-LEVEL-02.
               10  FAKTIN-02-L3.
                   15  FAKTIN-02-L3-FIRMNR PICTURE X(3).
               10  FAKTIN-02-L2.
                   15  FAKTIN-02-L2-RESKNR PICTURE X(6).
                   15  FAKTIN-02-L2-FAKTYP PICTURE X(1).
                   15  FAKTIN-02-L2-FAKMTE PICTURE X(1).
                   15  FAKTIN-02-L2-AVGKOD PICTURE X(1).
                   15  FAKTIN-02-L2-BETBET PICTURE X(2).
               10  FAKTIN-02-L1.
                   15  FAKTIN-02-L1-ONR    PICTURE X(6).
           05  FAKTIN-DATA-FIELDS.
               10  FAKREC                  PICTURE X(200).
               10  FIRMNR                  PICTURE X(3).
               10  RESKNR                  PICTURE X(6).
               10  FAKTYP                  PICTURE X(1).
      *                                      11  11 FAKARTL2
               10  FAKMTE                  PICTURE X(1).
               10  AVGKOD                  PICTURE X(1).
               10  BETBET                  PICTURE X(2).
               10  ONR                     PICTURE X(6).
               10  RECART                  PICTURE X(1).
               10  FAKKEY                  PICTURE X(24).
               10  BESENH-IO.
                   15  BESENH              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  LEVENH-IO.
                   15  LEVENH              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  EDBNR                   PICTURE X(7).
               10  RNOTA                   PICTURE X(1).
               10  KUNREF                  PICTURE X(15).
      *****************************************************************
      * HOVEDRUTINE: BESTEMME RECORDTYPER/ ORDRETYPER.                *
      *****************************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(11).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  KUFREF                  PICTURE X(15).
               10  BRUDNR-IO.
                   15  BRUDNR              PICTURE S9(5).
               10  ANTLIN-IO.
                   15  ANTLIN              PICTURE S9(5).
           05  EDITTING-FIELDS.
               10  XO-50P-EF.
                 15  XO-50P                PICTURE S9(5) USAGE
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
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  FAKTIN-PROCESS
               SET FAKTIN-PROCESS-OFF      TO TRUE
               SET FAKTIN-READ             TO TRUE
           END-IF
 
           IF  FAKTIN-READ
           AND RECORD-SELECTED-OFF
               PERFORM FAKTIN-GET
               SET FAKTIN-READ-OFF         TO TRUE
               IF  NOT FAKTIN-EOF
                   SET FAKTIN-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  FAKTIN-PROCESS
               PERFORM FAKTIN-IDSET
           END-IF
 
           IF  FAKTIN-PROCESS
               PERFORM FAKTIN-CHK-LEVEL
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
 
           IF  FAKTIN-PROCESS
               PERFORM FAKTIN-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  FAKTIN-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-25                    TO TRUE
           SET NOT-I-40                    TO TRUE
           SET NOT-I-20                    TO TRUE
           IF  RECART = 'A'
               SET I-20                    TO TRUE
           END-IF
           SET NOT-I-21                    TO TRUE
           IF  KUNREF NOT = KUFREF
               SET I-21                    TO TRUE
           END-IF
           MOVE KUNREF                     TO KUFREF
           SET NOT-I-22                    TO TRUE
           IF  FAKMTE = 'O'
               SET I-22                    TO TRUE
           END-IF
           SET NOT-I-23                    TO TRUE
           IF  LEVENH NOT = 0,00
               SET I-23                    TO TRUE
           END-IF
           SET NOT-I-24                    TO TRUE
           IF  BESENH = 0,00
               SET I-24                    TO TRUE
           END-IF
           IF  (NOT-I-20)
               SET NOT-I-25                TO TRUE
               IF  EDBNR = '       '
                   SET I-25                TO TRUE
               END-IF
           END-IF
           SET NOT-I-26                    TO TRUE
           IF  RNOTA = 'R'
               SET I-26                    TO TRUE
           END-IF
      *****************************************************************
      * RUTINE FOR Å LAGE BRUDD-NR. PR. FAKTURA/KREDITNOTA            *
      *****************************************************************
           IF  (I-L2)
               MOVE 1                      TO BRUDNR
               SET I-40                    TO TRUE
           END-IF
           IF  (I-L1 AND NOT-I-L2 AND I-22)
               ADD 1                       TO BRUDNR
               SET I-40                    TO TRUE
           END-IF
           IF  (I-L1 AND NOT-I-L2 AND NOT-I-22)
               AND (I-21)
               ADD 1                       TO BRUDNR
           END-IF
           IF  (I-L1 AND NOT-I-L2 AND I-22)
               AND (I-21)
               SET I-40                    TO TRUE
           END-IF
           IF  (I-L1)
               SET I-30                    TO TRUE
           END-IF
           IF  (I-23)
               SET NOT-I-30                TO TRUE
           END-IF
           IF  (I-26)
               SET NOT-I-30                TO TRUE
      *****************************************************************
      * RUTINE FOR Å FJERNE VARELINJER MED 0 I LEVERT ANTALL          *
      *  SOM IKKE ER VAREADRESSE ELLER TEKSTLINJE.                    *
      *  IKKE RENTENOTA VARELINJER.                                   *
      *****************************************************************
           END-IF
           SET I-11                        TO TRUE
           IF  (NOT-I-20 AND NOT-I-23 AND NOT-I-24)
               AND (NOT-I-25 AND NOT-I-26)
               SET NOT-I-11                TO TRUE
           END-IF
           IF  (I-40)
               SET NOT-I-31                TO TRUE
               MOVE 0                      TO ANTLIN
           END-IF
           IF  (I-L1)
               ADD 1                       TO ANTLIN
           END-IF
           IF  (I-20)
               GO TO ENDTEL-T
           END-IF
           IF  (NOT-I-11)
               GO TO ENDTEL-T
           END-IF
           ADD 1                           TO ANTLIN.
 
       ENDTEL-T.
           CONTINUE.
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1)
               SET NOT-I-50                TO TRUE
           END-IF
           IF  (I-L1 AND NOT-I-L2 AND I-22)
               SET I-50                    TO TRUE
           END-IF
           IF  (I-L2)
               SET I-50                    TO TRUE
           END-IF
           IF  (I-L1 AND I-50)
               SET NOT-I-31                TO TRUE
               IF  ANTLIN > 21
                   SET I-31                TO TRUE
               END-IF
           END-IF.
 
       FAKTIN-GET SECTION.
       FAKTIN-GET-P.
           IF  FAKTIN-EOF-OFF
               READ FAKTIN
               AT END
                   SET FAKTIN-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FAKTIN-FLDSET SECTION.
       FAKTIN-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKTIN-IO-AREA (1:200) TO FAKREC (1:200)
               MOVE FAKTIN-IO-AREA (1:3)   TO FIRMNR (1:3)
               MOVE FAKTIN-IO-AREA (4:6)   TO RESKNR (1:6)
               MOVE FAKTIN-IO-AREA (10:1)  TO FAKTYP (1:1)
               MOVE FAKTIN-IO-AREA (12:1)  TO FAKMTE (1:1)
               MOVE FAKTIN-IO-AREA (13:1)  TO AVGKOD (1:1)
               MOVE FAKTIN-IO-AREA (14:2)  TO BETBET (1:2)
               MOVE FAKTIN-IO-AREA (19:6)  TO ONR (1:6)
               MOVE FAKTIN-IO-AREA (25:1)  TO RECART (1:1)
               MOVE FAKTIN-IO-AREA (1:24)  TO FAKKEY (1:24)
               MOVE FAKTIN-IO-AREA (133:4) TO BESENH-IO
               MOVE FAKTIN-IO-AREA (137:4) TO LEVENH-IO
               MOVE FAKTIN-IO-AREA (141:7) TO EDBNR (1:7)
               MOVE FAKTIN-IO-AREA (177:1) TO RNOTA (1:1)
               MOVE FAKTIN-IO-AREA (201:15) TO KUNREF (1:15)
           END-EVALUATE.
 
       FAKTIN-IDSET SECTION.
       FAKTIN-IDSET-P.
           SET I-02                        TO TRUE.
 
       FAKTIN-CHK-LEVEL SECTION.
       FAKTIN-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO FAKTIN-LEVEL-02
               MOVE FAKTIN-IO-AREA (1:3)   TO FAKTIN-02-L3-FIRMNR
               MOVE FAKTIN-IO-AREA (4:6)   TO FAKTIN-02-L2-RESKNR
               MOVE FAKTIN-IO-AREA (10:1)  TO FAKTIN-02-L2-FAKTYP
               MOVE FAKTIN-IO-AREA (12:1)  TO FAKTIN-02-L2-FAKMTE
               MOVE FAKTIN-IO-AREA (13:1)  TO FAKTIN-02-L2-AVGKOD
               MOVE FAKTIN-IO-AREA (14:2)  TO FAKTIN-02-L2-BETBET
               MOVE FAKTIN-IO-AREA (19:6)  TO FAKTIN-02-L1-ONR
               IF  FAKTIN-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  FAKTIN-02-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  FAKTIN-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  FAKTIN-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  FAKTIN-02-L3          TO THE-PRIOR-L3
               MOVE  FAKTIN-02-L2          TO THE-PRIOR-L2
               MOVE  FAKTIN-02-L1          TO THE-PRIOR-L1
               SET FAKTIN-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND I-11)
               MOVE SPACES TO FAKTUT-IO-AREA
               INITIALIZE FAKTUT-IO-AREA
               MOVE FAKREC                 TO FAKTUT-IO-AREA (1:200)
               MOVE BRUDNR                 TO XO-50P
               MOVE XO-50P-EF              TO FAKTUT-IO-AREA (16:3)
               WRITE FAKTUT-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-30)
               MOVE SPACES TO NULLORD-IO-AREA
               INITIALIZE NULLORD-IO-AREA
               MOVE FAKKEY                 TO NULLORD-IO-AREA (1:24)
               MOVE BRUDNR                 TO XO-50P
               MOVE XO-50P-EF              TO NULLORD-IO-AREA (16:3)
               MOVE ONR                    TO NULLORD-IO-AREA (19:6)
               MOVE 'U'                    TO NULLORD-IO-AREA (25:1)
               WRITE NULLORD-IO-AREA
           END-IF
           IF  (I-L1 AND I-50 AND I-31)
               MOVE SPACES TO FLOPP-IO-AREA
               INITIALIZE FLOPP-IO-AREA
               MOVE FAKKEY                 TO FLOPP-IO-AREA (1:24)
               MOVE BRUDNR                 TO XO-50P
               MOVE XO-50P-EF              TO FLOPP-IO-AREA (16:3)
               MOVE ' '                    TO FLOPP-IO-AREA (25:1)
               WRITE FLOPP-IO-AREA
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
           SET FAKTIN-LEVEL-INIT           TO TRUE
           INITIALIZE FAKTIN-DATA-FIELDS
           SET FAKTIN-EOF-OFF              TO TRUE
           SET FAKTIN-PROCESS              TO TRUE
           OPEN INPUT FAKTIN
           OPEN OUTPUT FLOPP
           OPEN OUTPUT NULLORD
           OPEN OUTPUT FAKTUT.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FAKTIN
           CLOSE FLOPP
           CLOSE NULLORD
           CLOSE FAKTUT.
 
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
