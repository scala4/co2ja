       IDENTIFICATION DIVISION.
       PROGRAM-ID. NYP100R.
      **********************************************  Z-WIN-RPG2   ****
      *  UTPLUKK NYPRISMASTER TIL LISTER   *******
      ********************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: NYP100.rpg
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
           SELECT PRISMAS
               ASSIGN TO UT-S-PRISMAS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PRISMAS-STATUS.
           SELECT VAREMAS
               ASSIGN TO VAREMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAREMAS-STATUS
               RECORD KEY IS VAREMAS-KEY1.
           SELECT NYPRIS
               ASSIGN TO UT-S-NYPRIS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS NYPRIS-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD PRISMAS
               BLOCK CONTAINS 4000
               RECORD CONTAINS 80.
       01  PRISMAS-IO-AREA.
           05  PRISMAS-IO-AREA-X           PICTURE X(80).
       FD VAREMAS
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X.
               10  VAREMAS-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       FD NYPRIS
               BLOCK CONTAINS 4080
               RECORD CONTAINS 85.
       01  NYPRIS-IO-AREA.
           05  NYPRIS-IO-AREA-X            PICTURE X(85).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  PRISMAS-STATUS              PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  NYPRIS-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  PRISMAS-EOF-OFF         VALUE '0'.
               88  PRISMAS-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PRISMAS-READ-OFF        VALUE '0'.
               88  PRISMAS-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PRISMAS-PROCESS-OFF     VALUE '0'.
               88  PRISMAS-PROCESS         VALUE '1'.
           05  VAREMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  PRISMAS-DATA-FIELDS.
               10  RA                      PICTURE X(1).
               10  FIRMA                   PICTURE X(3).
               10  REF                     PICTURE X(5).
               10  SEQ                     PICTURE X(4).
               10  EDBNR                   PICTURE X(7).
               10  NYSVS-IO.
                   15  NYSVS               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  UTSALG-IO.
                   15  UTSALG              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  NYLEVP-IO.
                   15  NYLEVP              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  DATO-IO.
                   15  DATO                PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  OPPDAT                  PICTURE X(1).
           05  VAREMAS-DATA-FIELDS.
               10  VALF                    PICTURE X(3).
               10  VPRIS-IO.
                   15  VPRIS               PICTURE S9(7)V9(2).
               10  PT                      PICTURE X(1).
               10  VVGR                    PICTURE X(5).
               10  VAVD                    PICTURE X(1).
           05  TEMPORARY-FIELDS.
               10  OPPD1                   PICTURE X(1).
               10  NULL-X-IO.
                   15  NULL-X              PICTURE S9(9).
               10  PDATO-IO.
                   15  PDATO               PICTURE S9(6).
               10  DATO-N-IO.
                   15  DATO-N              PICTURE S9(7).
               10  KEY-X                   PICTURE X(10).
           05  EDITTING-FIELDS.
               10  XO-72P-EF.
                 15  XO-72P                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-90P-EF.
                 15  XO-90P                PICTURE S9(9) USAGE
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
           IF  PRISMAS-PROCESS
               SET PRISMAS-PROCESS-OFF     TO TRUE
               SET PRISMAS-READ            TO TRUE
           END-IF
 
           IF  PRISMAS-READ
           AND RECORD-SELECTED-OFF
               PERFORM PRISMAS-GET
               SET PRISMAS-READ-OFF        TO TRUE
               IF  NOT PRISMAS-EOF
                   SET PRISMAS-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  PRISMAS-PROCESS
               PERFORM PRISMAS-IDSET
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
 
           IF  PRISMAS-PROCESS
               PERFORM PRISMAS-FLDSET
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
           SET NOT-I-10                    TO TRUE
           SET NOT-I-40                    TO TRUE
           SET NOT-I-41                    TO TRUE
           SET NOT-I-45                    TO TRUE
           SET NOT-I-40                    TO TRUE
           IF  RA = 'A'
               SET I-40                    TO TRUE
           END-IF
           IF  (I-40)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-41                    TO TRUE
           IF  RA = 'B'
               SET I-41                    TO TRUE
           END-IF
           IF  (I-41)
               MOVE OPPDAT                 TO OPPD1
               GO TO SLUTT-T
      ******************************************************
           END-IF
           SET NOT-I-45                    TO TRUE
           IF  OPPD1 = '5'
               SET I-45                    TO TRUE
           END-IF
           IF  (NOT-I-45)
               SET NOT-I-45                TO TRUE
               IF  OPPD1 = '1'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-45)
               GO TO SLUTT-T
      *
           END-IF
           SET NOT-I-10                    TO TRUE
           IF  RA = 'C'
               SET I-10                    TO TRUE
           END-IF
           IF  (NOT-I-10)
               GO TO SLUTT-T
           END-IF
           MOVE 0                          TO NULL-X
           MOVE DATO                       TO DATO-N
           MOVE DATO-N-IO (2:6)            TO PDATO-IO
           MOVE FIRMA                      TO KEY-X (1:3)
           MOVE EDBNR                      TO KEY-X (4:7)
           MOVE KEY-X                      TO VAREMAS-KEY1
           READ VAREMAS RECORD KEY IS VAREMAS-KEY1
           INVALID KEY
               SET I-20                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-20                TO TRUE
               PERFORM VAREMAS-FLDSET
               PERFORM VAREMAS-IDSET
           END-READ.
 
       SLUTT-T.
           CONTINUE.
 
       PRISMAS-GET SECTION.
       PRISMAS-GET-P.
           IF  PRISMAS-EOF-OFF
               READ PRISMAS
               AT END
                   SET PRISMAS-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       PRISMAS-FLDSET SECTION.
       PRISMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE PRISMAS-IO-AREA (1:1)  TO RA (1:1)
               MOVE PRISMAS-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE PRISMAS-IO-AREA (5:5)  TO REF (1:5)
               MOVE PRISMAS-IO-AREA (10:4) TO SEQ (1:4)
               MOVE PRISMAS-IO-AREA (14:7) TO EDBNR (1:7)
               MOVE PRISMAS-IO-AREA (26:5) TO NYSVS-IO
               MOVE PRISMAS-IO-AREA (36:5) TO UTSALG-IO
               MOVE PRISMAS-IO-AREA (46:5) TO NYLEVP-IO
               MOVE PRISMAS-IO-AREA (51:4) TO DATO-IO
               MOVE PRISMAS-IO-AREA (80:1) TO OPPDAT (1:1)
           END-EVALUATE.
 
       PRISMAS-IDSET SECTION.
       PRISMAS-IDSET-P.
           SET I-01                        TO TRUE.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (13:3) TO VALF (1:3)
               MOVE VAREMAS-IO-AREA (75:9) TO VPRIS-IO
               INSPECT VPRIS-IO REPLACING ALL ' ' BY '0'
               MOVE VAREMAS-IO-AREA (95:1) TO PT (1:1)
               MOVE VAREMAS-IO-AREA (118:5) TO VVGR (1:5)
               MOVE VAREMAS-IO-AREA (118:1) TO VAVD (1:1)
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-02                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-10 AND NOT-I-20)
               MOVE SPACES TO NYPRIS-IO-AREA
               INITIALIZE NYPRIS-IO-AREA
               MOVE '2P'                   TO NYPRIS-IO-AREA (1:2)
               MOVE FIRMA                  TO NYPRIS-IO-AREA (3:3)
               MOVE EDBNR                  TO NYPRIS-IO-AREA (6:7)
               MOVE VAVD                   TO NYPRIS-IO-AREA (13:1)
               MOVE REF                    TO NYPRIS-IO-AREA (14:5)
               MOVE VVGR                   TO NYPRIS-IO-AREA (19:5)
               MOVE VALF                   TO NYPRIS-IO-AREA (24:3)
               MOVE UDATE                  TO NYPRIS-IO-AREA (27:6)
               MOVE PDATO-IO               TO NYPRIS-IO-AREA (33:6)
               MOVE NYSVS                  TO XO-72P
               MOVE XO-72P-EF              TO NYPRIS-IO-AREA (39:5)
               MOVE UTSALG                 TO XO-72P
               MOVE XO-72P-EF              TO NYPRIS-IO-AREA (44:5)
               MOVE NYLEVP                 TO XO-72P
               MOVE XO-72P-EF              TO NYPRIS-IO-AREA (49:5)
               MOVE NULL-X                 TO XO-90P
               MOVE XO-90P-EF              TO NYPRIS-IO-AREA (60:5)
               MOVE NULL-X                 TO XO-90P
               MOVE XO-90P-EF              TO NYPRIS-IO-AREA (65:5)
               WRITE NYPRIS-IO-AREA
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
           INITIALIZE PRISMAS-DATA-FIELDS
           SET PRISMAS-EOF-OFF             TO TRUE
           SET PRISMAS-PROCESS             TO TRUE
           OPEN INPUT PRISMAS
           INITIALIZE VAREMAS-DATA-FIELDS
           OPEN INPUT VAREMAS
           OPEN OUTPUT NYPRIS.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PRISMAS
           CLOSE VAREMAS
           CLOSE NYPRIS.
 
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
