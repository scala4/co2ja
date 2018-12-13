       IDENTIFICATION DIVISION.
       PROGRAM-ID. VAT022R.
      **********************************************  Z-WIN-RPG2   ****
      ** HENTE ALFA,ART.,VGR. FRA VAREMAS
      ** GJELDER  V B E K  UT I FRA AUTO.TRANSER1
      *********************************************************************
      *                                          XX2000XXIRXXSS
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VAT022.rpg
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
           SELECT INPUT-X
               ASSIGN TO UT-S-INPUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INPUT-X-STATUS.
           SELECT VAREMAS
               ASSIGN TO VAREMAS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VAREMAS-STATUS.
           SELECT OUTPUT-X
               ASSIGN TO UT-S-OUTPUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTPUT-X-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD INPUT-X
               BLOCK CONTAINS 160
               RECORD CONTAINS 80.
       01  INPUT-X-IO-AREA.
           05  INPUT-X-IO-AREA-X           PICTURE X(80).
       FD VAREMAS
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X           PICTURE X(200).
       FD OUTPUT-X
               BLOCK CONTAINS 240
               RECORD CONTAINS 120.
       01  OUTPUT-X-IO-AREA.
           05  OUTPUT-X-IO-AREA-X          PICTURE X(120).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  INPUT-X-STATUS              PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  OUTPUT-X-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  INPUT-X-EOF-OFF         VALUE '0'.
               88  INPUT-X-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INPUT-X-READ-OFF        VALUE '0'.
               88  INPUT-X-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INPUT-X-PROCESS-OFF     VALUE '0'.
               88  INPUT-X-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  INPUT-X-LEVEL-INIT-OFF  VALUE '0'.
               88  INPUT-X-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREMAS-EOF-OFF         VALUE '0'.
               88  VAREMAS-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREMAS-READ-OFF        VALUE '0'.
               88  VAREMAS-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREMAS-PROCESS-OFF     VALUE '0'.
               88  VAREMAS-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  VAREMAS-LEVEL-INIT-OFF  VALUE '0'.
               88  VAREMAS-LEVEL-INIT      VALUE '1'.
           05  INPUT-X-LEVEL-01.
               10  INPUT-X-01-L2.
                   15  INPUT-X-01-L2-FIRMA PICTURE X(3).
               10  INPUT-X-01-L1.
                   15  INPUT-X-01-L1-EDBNR PICTURE X(7).
           05  INPUT-X-DATA-FIELDS.
               10  REC                     PICTURE X(80).
               10  FIRMA                   PICTURE X(3).
               10  EDBNR                   PICTURE X(7).
               10  ANT-IO.
                   15  ANT                 PICTURE S9(6)V9(2).
               10  INNP-IO.
                   15  INNP                PICTURE S9(7)V9(2).
           05  INPUT-X-MP                  PICTURE X(10).
           05  INPUT-X-MC                  PICTURE X(10).
           05  INPUT-X-M-01            REDEFINES INPUT-X-MC.
               10  INPUT-X-M-01-M2.
                   15  INPUT-X-M-01-M2-FIRMA-G.
                       20  INPUT-X-M-01-M2-FIRMA PICTURE X(3).
               10  INPUT-X-M-01-M1.
                   15  INPUT-X-M-01-M1-EDBNR-G.
                       20  INPUT-X-M-01-M1-EDBNR PICTURE X(7).
           05  VAREMAS-LEVEL-02.
               10  VAREMAS-02-L2.
                   15  VAREMAS-02-L2-FIRMA PICTURE X(3).
               10  VAREMAS-02-L1.
                   15  VAREMAS-02-L1-EDBNR PICTURE X(7).
           05  VAREMAS-DATA-FIELDS.
               10  KEY-X                   PICTURE X(10).
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  BETEGN                  PICTURE X(30).
               10  SELVK-IO.
                   15  SELVK               PICTURE S9(7)V9(2).
               10  UTSALG-IO.
                   15  UTSALG              PICTURE S9(7)V9(2).
               10  ALTNR                   PICTURE X(7).
               10  PT                      PICTURE X(1).
               10  BC                      PICTURE X(1).
               10  AVD                     PICTURE X(1).
               10  VGR                     PICTURE X(5).
               10  STAT                    PICTURE X(3).
               10  MERK                    PICTURE X(1).
               10  PRODGR                  PICTURE X(7).
               10  VT                      PICTURE X(1).
               10  LOCAT                   PICTURE X(6).
               10  UTGAR                   PICTURE X(1).
      *
      *****************************************************************
      * RUTINE FOR Å SJEKKE OM VERDI ER STØRRE EN 10 MILL.            *
      * ER DEN DET TREKKES ALT OVER 10 MILL I FRA.                    *
      *****************************************************************
           05  VAREMAS-MP                  PICTURE X(10).
           05  VAREMAS-MC                  PICTURE X(10).
           05  VAREMAS-M-02            REDEFINES VAREMAS-MC.
               10  VAREMAS-M-02-M2.
                   15  VAREMAS-M-02-M2-FIRMA-G.
                       20  VAREMAS-M-02-M2-FIRMA PICTURE X(3).
               10  VAREMAS-M-02-M1.
                   15  VAREMAS-M-02-M1-EDBNR-G.
                       20  VAREMAS-M-02-M1-EDBNR PICTURE X(7).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(7).
           05  TEMPORARY-FIELDS.
               10  VERDI1-IO.
                   15  VERDI1              PICTURE S9(10)V9(2).
               10  VERDI-IO.
                   15  VERDI               PICTURE S9(7)V9(2).
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
               88  NOT-CALL-MATCH-RECS     VALUE '0'.
               88  CALL-MATCH-RECS         VALUE '1'.
           05  FILLER                      PICTURE X.
               88  NOT-SET-I-MR            VALUE '0'.
               88  SET-I-MR                VALUE '1'.
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
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  VAREMAS-PROCESS
               SET VAREMAS-PROCESS-OFF     TO TRUE
               SET VAREMAS-READ            TO TRUE
           END-IF
 
           IF  VAREMAS-READ
               PERFORM VAREMAS-GET
               SET VAREMAS-READ-OFF        TO TRUE
               IF  NOT VAREMAS-EOF
                   PERFORM VAREMAS-MATCH-SET
               END-IF
           END-IF
 
           IF  INPUT-X-PROCESS
               SET INPUT-X-PROCESS-OFF     TO TRUE
               SET INPUT-X-READ            TO TRUE
           END-IF
 
           IF  INPUT-X-READ
               PERFORM INPUT-X-GET
               SET INPUT-X-READ-OFF        TO TRUE
               IF  NOT INPUT-X-EOF
                   PERFORM INPUT-X-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  INPUT-X-PROCESS
               PERFORM INPUT-X-IDSET
           END-IF
 
           IF  VAREMAS-PROCESS
               PERFORM VAREMAS-IDSET
           END-IF
 
           IF  INPUT-X-PROCESS
               PERFORM INPUT-X-CHK-LEVEL
           END-IF
 
           IF  VAREMAS-PROCESS
               PERFORM VAREMAS-CHK-LEVEL
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
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  INPUT-X-PROCESS
               PERFORM INPUT-X-FLDOFF
               PERFORM INPUT-X-FLDSET
           END-IF
 
           IF  VAREMAS-PROCESS
               PERFORM VAREMAS-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  INPUT-X-PROCESS
           OR  VAREMAS-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-MR AND NOT-I-50)
               MULTIPLY INNP BY ANT    GIVING VERDI1 ROUNDED
           END-IF
           IF  (I-MR AND I-50)
               MULTIPLY SELVK BY ANT   GIVING VERDI1 ROUNDED
           END-IF.
 
       LOOP1-T.
           IF  (I-MR)
               SET NOT-I-11                TO TRUE
               IF  VERDI1 NOT < 10000000
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-MR AND I-11)
               SUBTRACT 10000000           FROM VERDI1
               GO TO LOOP1-T
      *****************************************************************
           END-IF
           IF  (I-MR)
               ADD VERDI1 TO ZERO      GIVING VERDI
           END-IF.
 
       INPUT-X-GET SECTION.
       INPUT-X-GET-P.
           IF  INPUT-X-EOF-OFF
               READ INPUT-X
               AT END
                   SET INPUT-X-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INPUT-X-FLDOFF SECTION.
       INPUT-X-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-50                TO TRUE
           END-EVALUATE.
 
       INPUT-X-FLDSET SECTION.
       INPUT-X-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INPUT-X-IO-AREA (1:80) TO REC (1:80)
               MOVE INPUT-X-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE INPUT-X-IO-AREA (18:7) TO EDBNR (1:7)
               MOVE INPUT-X-IO-AREA (25:8) TO ANT-IO
               INSPECT ANT-IO REPLACING ALL ' ' BY '0'
               MOVE INPUT-X-IO-AREA (62:9) TO INNP-IO
               INSPECT INNP-IO REPLACING ALL ' ' BY '0'
               IF  INNP = ZERO
                   SET I-50                TO TRUE
               END-IF
           END-EVALUATE.
 
       INPUT-X-IDSET SECTION.
       INPUT-X-IDSET-P.
           SET I-01                        TO TRUE.
 
       INPUT-X-CHK-LEVEL SECTION.
       INPUT-X-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO INPUT-X-LEVEL-01
               MOVE INPUT-X-IO-AREA (3:3)  TO INPUT-X-01-L2-FIRMA
               MOVE INPUT-X-IO-AREA (18:7) TO INPUT-X-01-L1-EDBNR
               IF  INPUT-X-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INPUT-X-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INPUT-X-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INPUT-X-01-L2         TO THE-PRIOR-L2
               MOVE  INPUT-X-01-L1         TO THE-PRIOR-L1
               SET INPUT-X-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       INPUT-X-MATCH-SET SECTION.
       INPUT-X-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE INPUT-X-IO-AREA (3:3)  TO INPUT-X-M-01-M2-FIRMA
               MOVE INPUT-X-IO-AREA (18:7) TO INPUT-X-M-01-M1-EDBNR
           END-EVALUATE.
 
       VAREMAS-GET SECTION.
       VAREMAS-GET-P.
           IF  VAREMAS-EOF-OFF
               READ VAREMAS
               AT END
                   SET VAREMAS-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE VAREMAS-IO-AREA (6:7)  TO EDBNR (1:7)
               MOVE VAREMAS-IO-AREA (3:10) TO KEY-X (1:10)
               MOVE VAREMAS-IO-AREA (13:3) TO ALFA (1:3)
               MOVE VAREMAS-IO-AREA (16:20) TO ARTNR (1:20)
               MOVE VAREMAS-IO-AREA (36:30) TO BETEGN (1:30)
               MOVE VAREMAS-IO-AREA (66:9) TO SELVK-IO
               INSPECT SELVK-IO REPLACING ALL ' ' BY '0'
               MOVE VAREMAS-IO-AREA (75:9) TO UTSALG-IO
               INSPECT UTSALG-IO REPLACING ALL ' ' BY '0'
               MOVE VAREMAS-IO-AREA (88:7) TO ALTNR (1:7)
               MOVE VAREMAS-IO-AREA (95:1) TO PT (1:1)
               MOVE VAREMAS-IO-AREA (96:1) TO BC (1:1)
               MOVE VAREMAS-IO-AREA (118:1) TO AVD (1:1)
               MOVE VAREMAS-IO-AREA (118:5) TO VGR (1:5)
               MOVE VAREMAS-IO-AREA (123:3) TO STAT (1:3)
               MOVE VAREMAS-IO-AREA (127:1) TO MERK (1:1)
               MOVE VAREMAS-IO-AREA (146:7) TO PRODGR (1:7)
               MOVE VAREMAS-IO-AREA (170:1) TO VT (1:1)
               MOVE VAREMAS-IO-AREA (140:6) TO LOCAT (1:6)
               MOVE VAREMAS-IO-AREA (127:1) TO UTGAR (1:1)
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-02                        TO TRUE.
 
       VAREMAS-CHK-LEVEL SECTION.
       VAREMAS-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO VAREMAS-LEVEL-02
               MOVE VAREMAS-IO-AREA (3:3)  TO VAREMAS-02-L2-FIRMA
               MOVE VAREMAS-IO-AREA (6:7)  TO VAREMAS-02-L1-EDBNR
               IF  VAREMAS-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VAREMAS-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  VAREMAS-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  VAREMAS-02-L2         TO THE-PRIOR-L2
               MOVE  VAREMAS-02-L1         TO THE-PRIOR-L1
               SET VAREMAS-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       VAREMAS-MATCH-SET SECTION.
       VAREMAS-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (3:3)  TO VAREMAS-M-02-M2-FIRMA
               MOVE VAREMAS-IO-AREA (6:7)  TO VAREMAS-M-02-M1-EDBNR
           END-EVALUATE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  INPUT-X-EOF
               MOVE HIGH-VALUES            TO INPUT-X-MC
                                              INPUT-X-MP
           END-IF
           IF  VAREMAS-EOF
               MOVE HIGH-VALUES            TO VAREMAS-MC
                                              VAREMAS-MP
           END-IF
           IF  INPUT-X-MC < INPUT-X-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  VAREMAS-MC < VAREMAS-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  INPUT-X-MC < VAREMAS-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET INPUT-X-PROCESS     TO TRUE
                   MOVE INPUT-X-MC         TO INPUT-X-MP
                   IF  INPUT-X-MC = VAREMAS-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  VAREMAS-MC < INPUT-X-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VAREMAS-PROCESS     TO TRUE
                   MOVE VAREMAS-MC         TO VAREMAS-MP
                   IF  VAREMAS-MC = INPUT-X-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  INPUT-X-MC = VAREMAS-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET INPUT-X-PROCESS     TO TRUE
                   MOVE INPUT-X-MC         TO INPUT-X-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-MR)
               MOVE SPACES TO OUTPUT-X-IO-AREA
               INITIALIZE OUTPUT-X-IO-AREA
               MOVE REC                    TO OUTPUT-X-IO-AREA (1:80)
               MOVE ARTNR                  TO OUTPUT-X-IO-AREA (81:20)
               MOVE ALFA                   TO OUTPUT-X-IO-AREA (101:3)
               MOVE VGR                    TO OUTPUT-X-IO-AREA (104:5)
               MOVE VERDI-IO               TO OUTPUT-X-IO-AREA (109:9)
               WRITE OUTPUT-X-IO-AREA
           END-IF
           IF  (I-01 AND NOT-I-MR)
               MOVE SPACES TO OUTPUT-X-IO-AREA
               INITIALIZE OUTPUT-X-IO-AREA
               MOVE REC                    TO OUTPUT-X-IO-AREA (1:80)
               WRITE OUTPUT-X-IO-AREA
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
           SET INPUT-X-LEVEL-INIT          TO TRUE
           INITIALIZE INPUT-X-DATA-FIELDS
           SET INPUT-X-EOF-OFF             TO TRUE
           SET INPUT-X-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO INPUT-X-MC
                                              INPUT-X-MP
           OPEN INPUT INPUT-X
           SET VAREMAS-LEVEL-INIT          TO TRUE
           INITIALIZE VAREMAS-DATA-FIELDS
           SET VAREMAS-EOF-OFF             TO TRUE
           SET VAREMAS-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO VAREMAS-MC
                                              VAREMAS-MP
           OPEN INPUT VAREMAS
           OPEN OUTPUT OUTPUT-X.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INPUT-X
           CLOSE VAREMAS
           CLOSE OUTPUT-X.
 
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
 
       SETOFF-I-M SECTION.
           SET NOT-I-M1                    TO TRUE.
           SET NOT-I-M2                    TO TRUE.
           SET NOT-I-M3                    TO TRUE.
           SET NOT-I-M4                    TO TRUE.
           SET NOT-I-M5                    TO TRUE.
           SET NOT-I-M6                    TO TRUE.
           SET NOT-I-M7                    TO TRUE.
           SET NOT-I-M8                    TO TRUE.
           SET NOT-I-M9                    TO TRUE.
 
       SETON-I-M9 SECTION.
           SET I-M9                        TO TRUE.
           PERFORM SETON-I-M8.
 
       SETON-I-M8 SECTION.
           SET I-M8                        TO TRUE.
           PERFORM SETON-I-M7.
 
       SETON-I-M7 SECTION.
           SET I-M7                        TO TRUE.
           PERFORM SETON-I-M6.
 
       SETON-I-M6 SECTION.
           SET I-M6                        TO TRUE.
           PERFORM SETON-I-M5.
 
       SETON-I-M5 SECTION.
           SET I-M5                        TO TRUE.
           PERFORM SETON-I-M4.
 
       SETON-I-M4 SECTION.
           SET I-M4                        TO TRUE.
           PERFORM SETON-I-M3.
 
       SETON-I-M3 SECTION.
           SET I-M3                        TO TRUE.
           PERFORM SETON-I-M2.
 
       SETON-I-M2 SECTION.
           SET I-M2                        TO TRUE.
           PERFORM SETON-I-M1.
 
       SETON-I-M1 SECTION.
           SET I-M1                        TO TRUE.
 
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
