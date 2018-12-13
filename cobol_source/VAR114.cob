       IDENTIFICATION DIVISION.
       PROGRAM-ID. VAR114R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAMERT AV: ESPEN LARSEN  18.06.2004                       *
      * DANNER EN OPPSLAGSRECORD PR.FIRMA SOM LIGGER I TABELL.        *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VAR114.rpg
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
           SELECT FNRFIL
               ASSIGN TO UT-S-FNRFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FNRFIL-STATUS.
           SELECT RECINN
               ASSIGN TO UT-S-RECINN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RECINN-STATUS.
           SELECT OUTPUT-X
               ASSIGN TO UT-S-OUTPUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTPUT-X-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD FNRFIL
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  FNRFIL-IO-AREA.
           05  FNRFIL-IO-AREA-X            PICTURE X(80).
       FD RECINN
               BLOCK CONTAINS 120
               RECORD CONTAINS 60.
       01  RECINN-IO-AREA.
           05  RECINN-IO-AREA-X            PICTURE X(60).
       FD OUTPUT-X
               BLOCK CONTAINS 120
               RECORD CONTAINS 60.
       01  OUTPUT-X-IO-AREA.
           05  OUTPUT-X-IO-AREA-X          PICTURE X(60).
       WORKING-STORAGE SECTION.
       77  AR1-MAX   VALUE 100             PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  AR1-TABLE.
               10  AR1-ENTRY
                                           OCCURS 100 TIMES
                                           INDEXED BY AR1-I
                                                      AR1-S.
                   15  AR1                 PICTURE X(3).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  FNRFIL-STATUS               PICTURE 99 VALUE 0.
           10  RECINN-STATUS               PICTURE 99 VALUE 0.
           10  OUTPUT-X-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  FNRFIL-EOF-OFF          VALUE '0'.
               88  FNRFIL-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FNRFIL-READ-OFF         VALUE '0'.
               88  FNRFIL-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FNRFIL-PROCESS-OFF      VALUE '0'.
               88  FNRFIL-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RECINN-EOF-OFF          VALUE '0'.
               88  RECINN-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RECINN-READ-OFF         VALUE '0'.
               88  RECINN-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RECINN-PROCESS-OFF      VALUE '0'.
               88  RECINN-PROCESS          VALUE '1'.
           05  FNRFIL-DATA-FIELDS.
               10  FNRNR                   PICTURE X(3).
           05  RECINN-DATA-FIELDS.
               10  INNREC                  PICTURE X(60).
      *****************************************************************
      * BYGG OPP ARRAY FRA INPUT FIRMA RECORDS FILE.                  *
      *****************************************************************
           05  TEMPORARY-FIELDS.
               10  X-IO.
                   15  X                   PICTURE S9(3).
               10  AFNR-IO.
                   15  AFNR                PICTURE S9(3).
               10  FIRMA                   PICTURE X(3).
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
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  FNRFIL-PROCESS
               SET FNRFIL-PROCESS-OFF      TO TRUE
               SET FNRFIL-READ             TO TRUE
           END-IF
 
           IF  FNRFIL-READ
           AND RECORD-SELECTED-OFF
               PERFORM FNRFIL-GET
               SET FNRFIL-READ-OFF         TO TRUE
               IF  NOT FNRFIL-EOF
                   SET FNRFIL-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  RECINN-PROCESS
               SET RECINN-PROCESS-OFF      TO TRUE
               SET RECINN-READ             TO TRUE
           END-IF
 
           IF  RECINN-READ
           AND RECORD-SELECTED-OFF
               PERFORM RECINN-GET
               SET RECINN-READ-OFF         TO TRUE
               IF  NOT RECINN-EOF
                   SET RECINN-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  FNRFIL-PROCESS
               PERFORM FNRFIL-IDSET
           END-IF
 
           IF  RECINN-PROCESS
               PERFORM RECINN-IDSET
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
 
           IF  FNRFIL-PROCESS
               PERFORM FNRFIL-FLDSET
           END-IF
 
           IF  RECINN-PROCESS
               PERFORM RECINN-FLDSET
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
           IF  (I-01 AND NOT-I-21)
               MOVE 0                      TO X
           END-IF
           IF  (I-01)
               SET I-21                    TO TRUE
               ADD 1                       TO X
               MOVE FNRNR                  TO AR1 (X)
               MOVE X                      TO AFNR-IO
               GO TO SLUTT-T
      *****************************************************************
      * KOPIERE RECORDS. EN PR. FIRMA SOM LIGGER I ARRAY.             *
      *****************************************************************
           END-IF
           IF  (I-02)
               MOVE 0                      TO X
           END-IF.
 
       LOOPA-T.
           IF  (I-02)
               SET NOT-I-22                TO TRUE
               ADD 1                       TO X
               MOVE AR1 (X)                TO FIRMA
               SET NOT-I-22                TO TRUE
               IF  X NOT > AFNR
                   SET I-22                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-22)
               PERFORM EXCEPTION-OUTPUT
               GO TO LOOPA-T
           END-IF
           IF  (I-02)
               SET NOT-I-22                TO TRUE
      *****************************************************************
           END-IF
           .
 
       SLUTT-T.
           CONTINUE.
 
       FNRFIL-GET SECTION.
       FNRFIL-GET-P.
           IF  FNRFIL-EOF-OFF
               READ FNRFIL
               AT END
                   SET FNRFIL-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FNRFIL-FLDSET SECTION.
       FNRFIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FNRFIL-IO-AREA (7:3)   TO FNRNR (1:3)
           END-EVALUATE.
 
       FNRFIL-IDSET SECTION.
       FNRFIL-IDSET-P.
           SET I-01                        TO TRUE.
 
       RECINN-GET SECTION.
       RECINN-GET-P.
           IF  RECINN-EOF-OFF
               READ RECINN
               AT END
                   SET RECINN-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RECINN-FLDSET SECTION.
       RECINN-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RECINN-IO-AREA (1:60)  TO INNREC (1:60)
           END-EVALUATE.
 
       RECINN-IDSET SECTION.
       RECINN-IDSET-P.
           SET I-02                        TO TRUE.
 
       EXCEPTION-OUTPUT SECTION.
       EXCEPTION-OUTPUT-P.
           IF  (I-02 AND I-22)
               MOVE SPACES TO OUTPUT-X-IO-AREA
               INITIALIZE OUTPUT-X-IO-AREA
               MOVE INNREC                 TO OUTPUT-X-IO-AREA (1:60)
               MOVE FIRMA                  TO OUTPUT-X-IO-AREA (2:3)
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
           INITIALIZE FNRFIL-DATA-FIELDS
           SET FNRFIL-EOF-OFF              TO TRUE
           SET FNRFIL-PROCESS              TO TRUE
           OPEN INPUT FNRFIL
           INITIALIZE RECINN-DATA-FIELDS
           SET RECINN-EOF-OFF              TO TRUE
           SET RECINN-PROCESS              TO TRUE
           OPEN INPUT RECINN
           OPEN OUTPUT OUTPUT-X.
           PERFORM VARYING AR1-I FROM 1 BY 1
                     UNTIL AR1-I > AR1-MAX
               INITIALIZE AR1 (AR1-I)
           END-PERFORM
           SET AR1-I                       TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FNRFIL
           CLOSE RECINN
           CLOSE OUTPUT-X.
 
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
