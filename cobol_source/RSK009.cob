       IDENTIFICATION DIVISION.
       PROGRAM-ID. RSK009R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM.......: RSK009, SNUR BELØP PÅ BET.SPES. FRA RELMAST. *
      *  PROGRAMMERER..: MORTEN TUVRØNNINGEN                          *
      *  KJØRES I JOBB.: DOP.XDOP12UD                                 *
      *  LAGET DATO....: 28.12.01                                     *
      *  E 23.02.10....: SNUR BELØP FOR RELNR 961049 (TEST 399 MED NY *
      *                  PURRERUTINE)                                 *
      *  E 21.04.10....: TATT UT SNUING AV BELØP FOR RELNR 961049     *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RSK009.rpg
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
           SELECT RELFILI
               ASSIGN TO UT-S-RELFILI
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RELFILI-STATUS.
           SELECT RELFILO
               ASSIGN TO UT-S-RELFILO
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RELFILO-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD RELFILI
               BLOCK CONTAINS 9420
               RECORD CONTAINS 60.
       01  RELFILI-IO-AREA.
           05  RELFILI-IO-AREA-X           PICTURE X(60).
      *BUGFILO O   F  80  80            PRINTERSYSLST
       FD RELFILO
               BLOCK CONTAINS 9420
               RECORD CONTAINS 60.
       01  RELFILO-IO-AREA.
           05  RELFILO-IO-AREA-X           PICTURE X(60).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  RELFILI-STATUS              PICTURE 99 VALUE 0.
           10  RELFILO-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  RELFILI-EOF-OFF         VALUE '0'.
               88  RELFILI-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RELFILI-READ-OFF        VALUE '0'.
               88  RELFILI-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RELFILI-PROCESS-OFF     VALUE '0'.
               88  RELFILI-PROCESS         VALUE '1'.
           05  RELFILI-DATA-FIELDS.
               10  REC60                   PICTURE X(60).
               10  RELNR1                  PICTURE X(1).
      *                                      12  17 RELNR
               10  RBEL-IO.
                   15  RBEL                PICTURE S9(7)V9(2).
               10  RBTO-IO.
                   15  RBTO                PICTURE S9(8)V9(2).
           05  TEMPORARY-FIELDS.
               10  HJRBEL-IO.
                   15  HJRBEL              PICTURE S9(7)V9(2).
               10  HJRBTO-IO.
                   15  HJRBTO              PICTURE S9(8)V9(2).
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
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  RELFILI-PROCESS
               SET RELFILI-PROCESS-OFF     TO TRUE
               SET RELFILI-READ            TO TRUE
           END-IF
 
           IF  RELFILI-READ
           AND RECORD-SELECTED-OFF
               PERFORM RELFILI-GET
               SET RELFILI-READ-OFF        TO TRUE
               IF  NOT RELFILI-EOF
                   SET RELFILI-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  RELFILI-PROCESS
               PERFORM RELFILI-IDSET
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
 
           IF  RELFILI-PROCESS
               PERFORM RELFILI-FLDSET
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
           IF  RELNR1 = '0'
               SET I-10                    TO TRUE
           END-IF
      * N10      RELNR     COMP "961049"                 10 SNU TEGN
      *  10                MOVE "REC60   "BUGFL2  8        LEDETXT DEBUG
      *  10      BUGFL2    DEBUGBUGFILO   REC60            VIS FELT/IND
           IF  (I-10)
               MULTIPLY -1 BY RBEL     GIVING HJRBEL
               MULTIPLY -1 BY RBTO     GIVING HJRBTO
      *
           END-IF
           .
 
       RELFILI-GET SECTION.
       RELFILI-GET-P.
           IF  RELFILI-EOF-OFF
               READ RELFILI
               AT END
                   SET RELFILI-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RELFILI-FLDSET SECTION.
       RELFILI-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RELFILI-IO-AREA (1:60) TO REC60 (1:60)
               MOVE RELFILI-IO-AREA (12:1) TO RELNR1 (1:1)
               MOVE RELFILI-IO-AREA (24:9) TO RBEL-IO
               INSPECT RBEL-IO REPLACING ALL ' ' BY '0'
               MOVE RELFILI-IO-AREA (39:10) TO RBTO-IO
               INSPECT RBTO-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       RELFILI-IDSET SECTION.
       RELFILI-IDSET-P.
           SET I-01                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               MOVE SPACES TO RELFILO-IO-AREA
               INITIALIZE RELFILO-IO-AREA
               MOVE REC60                  TO RELFILO-IO-AREA (1:60)
               IF  (I-10)
                   MOVE HJRBEL-IO          TO RELFILO-IO-AREA (24:9)
               END-IF
               IF  (I-10)
                   MOVE HJRBTO-IO          TO RELFILO-IO-AREA (39:10)
               END-IF
               WRITE RELFILO-IO-AREA
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
           INITIALIZE RELFILI-DATA-FIELDS
           SET RELFILI-EOF-OFF             TO TRUE
           SET RELFILI-PROCESS             TO TRUE
           OPEN INPUT RELFILI
           OPEN OUTPUT RELFILO.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE RELFILI
           CLOSE RELFILO.
 
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
