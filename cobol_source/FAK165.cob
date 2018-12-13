       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK165R.
      **********************************************  Z-WIN-RPG2   ****
      *  DANNE KR.NOTA STATISTIKKRECORD PR. FIRMA.               *
      *  DENNE RECORD BENYTTES I FAKTURA STATISTIKKEN.           *
      ************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK165.rpg
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
           SELECT VAREINN
               ASSIGN TO UT-S-VAREINN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VAREINN-STATUS.
           SELECT STATREC
               ASSIGN TO UT-S-STATREC
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS STATREC-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD FAKPAR
               BLOCK CONTAINS 200
               RECORD CONTAINS 200.
       01  FAKPAR-IO-AREA.
           05  FAKPAR-IO-AREA-X            PICTURE X(200).
       FD VAREINN
               BLOCK CONTAINS 4100
               RECORD CONTAINS 82.
       01  VAREINN-IO-AREA.
           05  VAREINN-IO-AREA-X           PICTURE X(82).
       FD STATREC
               BLOCK CONTAINS 500
               RECORD CONTAINS 50.
       01  STATREC-IO-AREA.
           05  STATREC-IO-AREA-X           PICTURE X(50).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  FAKPAR-STATUS               PICTURE 99 VALUE 0.
           10  VAREINN-STATUS              PICTURE 99 VALUE 0.
           10  STATREC-STATUS              PICTURE 99 VALUE 0.
           10  VLFELT-XX-STATUS            PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKPAR-EOF-OFF          VALUE '0'.
               88  FAKPAR-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKPAR-READ-OFF         VALUE '0'.
               88  FAKPAR-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKPAR-PROCESS-OFF      VALUE '0'.
               88  FAKPAR-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREINN-EOF-OFF         VALUE '0'.
               88  VAREINN-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREINN-READ-OFF        VALUE '0'.
               88  VAREINN-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREINN-PROCESS-OFF     VALUE '0'.
               88  VAREINN-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  VAREINN-LEVEL-INIT-OFF  VALUE '0'.
               88  VAREINN-LEVEL-INIT      VALUE '1'.
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
           05  VLFELT-XX-DATA-FIELDS.
               10  VLANT-IO.
                   15  VLANT               PICTURE S9(7)V9(2).
               10  FILLER                  PICTURE X(38).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(9).
               10  VLBEL-IO.
                   15  VLBEL               PICTURE S9(9)V9(2).
               10  FILLER                  PICTURE X(27).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(20).
               10  VLPT-IO.
                   15  VLPT                PICTURE S9(9)V9(2).
               10  FILLER                  PICTURE X(16).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(31).
               10  VLRAB1-IO.
                   15  VLRAB1              PICTURE S9(2)V9(1).
               10  FILLER                  PICTURE X(13).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(34).
               10  VLRAB2-IO.
                   15  VLRAB2              PICTURE S9(2)V9(1).
               10  FILLER                  PICTURE X(10).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(37).
               10  VLRAB3-IO.
                   15  VLRAB3              PICTURE S9(2)V9(1).
               10  FILLER                  PICTURE X(7).
           05  FILLER REDEFINES VLFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(40).
               10  VLEDB-IO.
                   15  VLEDB               PICTURE S9(7).
           05  FAKPAR-DATA-FIELDS.
               10  PFAMND                  PICTURE X(2).
           05  VAREINN-LEVEL-02.
               10  VAREINN-02-L3.
                   15  VAREINN-02-L3-FIRMA PICTURE X(3).
               10  VAREINN-02-L2.
                   15  VAREINN-02-L2-KRTYPE PICTURE X(1).
               10  VAREINN-02-L1.
                   15  VAREINN-02-L1-ORDNR PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
           05  VAREINN-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  KRTYPE                  PICTURE X(1).
               10  ANTLEV-IO.
                   15  ANTLEV              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  EDBNR-IO.
                   15  EDBNR               PICTURE S9(7).
               10  EDB2F                   PICTURE X(2).
               10  EDB3F                   PICTURE X(3).
               10  ORDNR-IO.
                   15  ORDNR               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  RAB1-IO.
                   15  RAB1                PICTURE S9(2)V9(1).
               10  RAB2-IO.
                   15  RAB2                PICTURE S9(2)V9(1).
               10  RAB3-IO.
                   15  RAB3                PICTURE S9(2)V9(1).
               10  PRIS-IO.
                   15  PRIS                PICTURE S9(7)V9(2).
               10  MND                     PICTURE X(2).
               10  LAGER                   PICTURE X(2).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(1).
               10  THE-PRIOR-L1            PICTURE X(4).
           05  TEMPORARY-FIELDS.
               10  NYPRIS-IO.
                   15  NYPRIS              PICTURE S9(7)V9(2).
               10  ANTLEV-N-IO.
                   15  ANTLEV-N            PICTURE S9(5)V9(2).
               10  NTOBEL-IO.
                   15  NTOBEL              PICTURE S9(7)V9(2).
               10  ANTO92-IO.
                   15  ANTO92              PICTURE S9(5).
               10  ANTO93-IO.
                   15  ANTO93              PICTURE S9(5).
               10  ANTO94-IO.
                   15  ANTO94              PICTURE S9(5).
               10  ANTO95-IO.
                   15  ANTO95              PICTURE S9(5).
               10  ANTL92-IO.
                   15  ANTL92              PICTURE S9(5).
               10  ANTL93-IO.
                   15  ANTL93              PICTURE S9(5).
               10  ANTL94-IO.
                   15  ANTL94              PICTURE S9(5).
               10  ANTL95-IO.
                   15  ANTL95              PICTURE S9(5).
               10  SUMB92-IO.
                   15  SUMB92              PICTURE S9(7)V9(2).
               10  SUMB93-IO.
                   15  SUMB93              PICTURE S9(7)V9(2).
               10  SUMB94-IO.
                   15  SUMB94              PICTURE S9(7)V9(2).
               10  SUMB95-IO.
                   15  SUMB95              PICTURE S9(7)V9(2).
           05  EDITTING-FIELDS.
               10  XO-50P-EF.
                 15  XO-50P                PICTURE S9(5) USAGE
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
           IF  FAKPAR-PROCESS
               SET FAKPAR-PROCESS-OFF      TO TRUE
               SET FAKPAR-READ             TO TRUE
           END-IF
 
           IF  FAKPAR-READ
           AND RECORD-SELECTED-OFF
               PERFORM FAKPAR-GET
               SET FAKPAR-READ-OFF         TO TRUE
               IF  NOT FAKPAR-EOF
                   SET FAKPAR-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  VAREINN-PROCESS
               SET VAREINN-PROCESS-OFF     TO TRUE
               SET VAREINN-READ            TO TRUE
           END-IF
 
           IF  VAREINN-READ
           AND RECORD-SELECTED-OFF
               PERFORM VAREINN-GET
               SET VAREINN-READ-OFF        TO TRUE
               IF  NOT VAREINN-EOF
                   SET VAREINN-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  FAKPAR-PROCESS
               PERFORM FAKPAR-IDSET
           END-IF
 
           IF  VAREINN-PROCESS
               PERFORM VAREINN-IDSET
           END-IF
 
           IF  VAREINN-PROCESS
               PERFORM VAREINN-CHK-LEVEL
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
 
           IF  FAKPAR-PROCESS
               PERFORM FAKPAR-FLDSET
           END-IF
 
           IF  VAREINN-PROCESS
               PERFORM VAREINN-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  VAREINN-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-L3)
               SET NOT-I-50                TO TRUE
           END-IF
           IF  (I-01)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-25                    TO TRUE
           IF  MND = PFAMND
               SET I-25                    TO TRUE
           END-IF
           IF  (NOT-I-25)
               GO TO SLUTT-T
      *****************************************************************
      * HVIS EDBNR STARTER MED 94 ELLER 995 MÅ VI SNU BELØPET         *
      * FORDI DETTE BLIR SNUDD I SUBPROGRAMMET NETTOSUM.              *
      *****************************************************************
           END-IF
           MOVE PRIS                       TO NYPRIS-IO
           SET NOT-I-33                    TO TRUE
           IF  EDB3F = '995'
               SET I-33                    TO TRUE
           END-IF
           IF  (NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  EDB2F = '94'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-33)
               MULTIPLY -1 BY PRIS     GIVING NYPRIS
      ********************************************************
      *    SUBRUTINE FOR Å REGNE UT NETTO VARELINESUM.       *
      ********************************************************
      *          LAGER     COMP "PT"                     30 PRISTILEGG
      *  30                MOVE PRIS      NTOBEL            NETTOBEL.
      *  30                GOTO NTOEND                      FERDIG.
           END-IF
           MOVE ANTLEV                     TO ANTLEV-N
           MOVE ANTLEV-N-IO                TO VLANT-IO (3:7)
           MOVE NYPRIS                     TO VLBEL-IO (3:9)
           MOVE 0                          TO VLPT-IO (11:1)
           MOVE RAB1                       TO VLRAB1-IO
           MOVE RAB2                       TO VLRAB2-IO
           MOVE RAB3                       TO VLRAB3-IO
           MOVE EDBNR                      TO VLEDB-IO
           CALL 'NETTOSUM' USING VLFELT-XX-DATA-FIELDS
           MOVE VLBEL (3:9)                TO NTOBEL-IO
      *          NTOEND    TAG
      *****************************************************************
      * RUTINE FOR FASTSETTE KR.NOTA TYPER.                           *
      *****************************************************************
           IF  (I-L2)
               SET NOT-I-92                TO TRUE
               IF  KRTYPE = '2'
                   SET I-92                TO TRUE
               END-IF
               SET NOT-I-93                TO TRUE
               IF  KRTYPE = '3'
                   SET I-93                TO TRUE
               END-IF
               SET NOT-I-94                TO TRUE
               IF  KRTYPE = '4'
                   SET I-94                TO TRUE
               END-IF
               SET NOT-I-95                TO TRUE
               IF  KRTYPE = '5'
                   SET I-95                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-92 AND NOT-I-93)
               AND (NOT-I-94 AND NOT-I-95)
               SET I-92                    TO TRUE
           END-IF
           SET I-50                        TO TRUE
           IF  (I-L1 AND I-92)
               ADD 1                       TO ANTO92
           END-IF
           IF  (I-L1 AND I-93)
               ADD 1                       TO ANTO93
           END-IF
           IF  (I-L1 AND I-94)
               ADD 1                       TO ANTO94
           END-IF
           IF  (I-L1 AND I-95)
               ADD 1                       TO ANTO95
      *
           END-IF
           IF  (I-92)
               ADD 1                       TO ANTL92
           END-IF
           IF  (I-93)
               ADD 1                       TO ANTL93
           END-IF
           IF  (I-94)
               ADD 1                       TO ANTL94
           END-IF
           IF  (I-95)
               ADD 1                       TO ANTL95
      *
           END-IF
           IF  (I-92)
               ADD NTOBEL                  TO SUMB92
           END-IF
           IF  (I-93)
               ADD NTOBEL                  TO SUMB93
           END-IF
           IF  (I-94)
               ADD NTOBEL                  TO SUMB94
           END-IF
           IF  (I-95)
               ADD NTOBEL                  TO SUMB95
           END-IF.
 
       SLUTT-T.
           CONTINUE.
 
       FAKPAR-GET SECTION.
       FAKPAR-GET-P.
           IF  FAKPAR-EOF-OFF
               READ FAKPAR
               AT END
                   SET FAKPAR-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FAKPAR-FLDSET SECTION.
       FAKPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKPAR-IO-AREA (20:2)  TO PFAMND (1:2)
           END-EVALUATE.
 
       FAKPAR-IDSET SECTION.
       FAKPAR-IDSET-P.
           SET I-01                        TO TRUE.
 
       VAREINN-GET SECTION.
       VAREINN-GET-P.
           IF  VAREINN-EOF-OFF
               READ VAREINN
               AT END
                   SET VAREINN-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VAREINN-FLDSET SECTION.
       VAREINN-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREINN-IO-AREA (51:3) TO FIRMA (1:3)
               MOVE VAREINN-IO-AREA (66:1) TO KRTYPE (1:1)
               MOVE VAREINN-IO-AREA (12:4) TO ANTLEV-IO
               MOVE VAREINN-IO-AREA (16:7) TO EDBNR-IO
               INSPECT EDBNR-IO REPLACING ALL ' ' BY '0'
               MOVE VAREINN-IO-AREA (16:2) TO EDB2F (1:2)
               MOVE VAREINN-IO-AREA (16:3) TO EDB3F (1:3)
               MOVE VAREINN-IO-AREA (71:4) TO ORDNR-IO
               MOVE VAREINN-IO-AREA (23:3) TO RAB1-IO
               INSPECT RAB1-IO REPLACING ALL ' ' BY '0'
               MOVE VAREINN-IO-AREA (26:3) TO RAB2-IO
               INSPECT RAB2-IO REPLACING ALL ' ' BY '0'
               MOVE VAREINN-IO-AREA (29:3) TO RAB3-IO
               INSPECT RAB3-IO REPLACING ALL ' ' BY '0'
               MOVE VAREINN-IO-AREA (32:9) TO PRIS-IO
               INSPECT PRIS-IO REPLACING ALL ' ' BY '0'
               MOVE VAREINN-IO-AREA (42:2) TO MND (1:2)
               MOVE VAREINN-IO-AREA (69:2) TO LAGER (1:2)
           END-EVALUATE.
 
       VAREINN-IDSET SECTION.
       VAREINN-IDSET-P.
           SET I-02                        TO TRUE.
 
       VAREINN-CHK-LEVEL SECTION.
       VAREINN-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO VAREINN-LEVEL-02
               MOVE VAREINN-IO-AREA (51:3) TO VAREINN-02-L3-FIRMA
               MOVE VAREINN-IO-AREA (66:1) TO VAREINN-02-L2-KRTYPE
               MOVE VAREINN-IO-AREA (71:4) TO VAREINN-02-L1-ORDNR
               IF  VAREINN-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VAREINN-02-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  VAREINN-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  VAREINN-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  VAREINN-02-L3         TO THE-PRIOR-L3
               MOVE  VAREINN-02-L2         TO THE-PRIOR-L2
               MOVE  VAREINN-02-L1         TO THE-PRIOR-L1
               SET VAREINN-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L3 AND I-50)
               MOVE SPACES TO STATREC-IO-AREA
               INITIALIZE STATREC-IO-AREA
               MOVE FIRMA                  TO STATREC-IO-AREA (1:3)
               MOVE ANTO92                 TO XO-50P
               MOVE XO-50P-EF              TO STATREC-IO-AREA (4:3)
               INITIALIZE ANTO92
               MOVE ANTO93                 TO XO-50P
               MOVE XO-50P-EF              TO STATREC-IO-AREA (7:3)
               INITIALIZE ANTO93
               MOVE ANTO94                 TO XO-50P
               MOVE XO-50P-EF              TO STATREC-IO-AREA (10:3)
               INITIALIZE ANTO94
               MOVE ANTO95                 TO XO-50P
               MOVE XO-50P-EF              TO STATREC-IO-AREA (13:3)
               INITIALIZE ANTO95
               MOVE ANTL92                 TO XO-50P
               MOVE XO-50P-EF              TO STATREC-IO-AREA (16:3)
               INITIALIZE ANTL92
               MOVE ANTL93                 TO XO-50P
               MOVE XO-50P-EF              TO STATREC-IO-AREA (19:3)
               INITIALIZE ANTL93
               MOVE ANTL94                 TO XO-50P
               MOVE XO-50P-EF              TO STATREC-IO-AREA (22:3)
               INITIALIZE ANTL94
               MOVE ANTL95                 TO XO-50P
               MOVE XO-50P-EF              TO STATREC-IO-AREA (25:3)
               INITIALIZE ANTL95
               MOVE SUMB92                 TO XO-72P
               MOVE XO-72P-EF              TO STATREC-IO-AREA (28:5)
               INITIALIZE SUMB92
               MOVE SUMB93                 TO XO-72P
               MOVE XO-72P-EF              TO STATREC-IO-AREA (33:5)
               INITIALIZE SUMB93
               MOVE SUMB94                 TO XO-72P
               MOVE XO-72P-EF              TO STATREC-IO-AREA (38:5)
               INITIALIZE SUMB94
               MOVE SUMB95                 TO XO-72P
               MOVE XO-72P-EF              TO STATREC-IO-AREA (43:5)
               INITIALIZE SUMB95
               WRITE STATREC-IO-AREA
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
           INITIALIZE FAKPAR-DATA-FIELDS
           SET FAKPAR-EOF-OFF              TO TRUE
           SET FAKPAR-PROCESS              TO TRUE
           OPEN INPUT FAKPAR
           SET VAREINN-LEVEL-INIT          TO TRUE
           INITIALIZE VAREINN-DATA-FIELDS
           SET VAREINN-EOF-OFF             TO TRUE
           SET VAREINN-PROCESS             TO TRUE
           OPEN INPUT VAREINN
           OPEN OUTPUT STATREC.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FAKPAR
           CLOSE VAREINN
           CLOSE STATREC.
 
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
