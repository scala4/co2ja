       IDENTIFICATION DIVISION.
       PROGRAM-ID. VAS071R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAMMET DANNER HJELPEFILE FRA FAKT.VAREREC.             *
      * PAKKER UT ORDRENUMMER.                                     *
      **************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VAS071.rpg
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
           SELECT FAKVARE
               ASSIGN TO UT-S-FAKVARE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKVARE-STATUS.
           SELECT FAKVARH
               ASSIGN TO UT-S-FAKVARH
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKVARH-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD FAKVARE
               BLOCK CONTAINS 60
               RECORD CONTAINS 30.
       01  FAKVARE-IO-AREA.
           05  FAKVARE-IO-AREA-X           PICTURE X(30).
       FD FAKVARH
               BLOCK CONTAINS 62
               RECORD CONTAINS 31.
       01  FAKVARH-IO-AREA.
           05  FAKVARH-IO-AREA-X           PICTURE X(31).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  FAKVARE-STATUS              PICTURE 99 VALUE 0.
           10  FAKVARH-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKVARE-EOF-OFF         VALUE '0'.
               88  FAKVARE-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKVARE-READ-OFF        VALUE '0'.
               88  FAKVARE-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKVARE-PROCESS-OFF     VALUE '0'.
               88  FAKVARE-PROCESS         VALUE '1'.
           05  FAKVARE-DATA-FIELDS.
               10  REC1                    PICTURE X(30).
               10  ORDNR-IO.
                   15  ORDNR               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
           05  TEMPORARY-FIELDS.
               10  ORDNRN-IO.
                   15  ORDNRN              PICTURE S9(6).
               10  ORDNR-N-IO.
                   15  ORDNR-N             PICTURE S9(7).
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
           IF  FAKVARE-PROCESS
               SET FAKVARE-PROCESS-OFF     TO TRUE
               SET FAKVARE-READ            TO TRUE
           END-IF
 
           IF  FAKVARE-READ
           AND RECORD-SELECTED-OFF
               PERFORM FAKVARE-GET
               SET FAKVARE-READ-OFF        TO TRUE
               IF  NOT FAKVARE-EOF
                   SET FAKVARE-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  FAKVARE-PROCESS
               PERFORM FAKVARE-IDSET
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
 
           IF  FAKVARE-PROCESS
               PERFORM FAKVARE-FLDSET
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
           SET NOT-I-60                    TO TRUE
           MOVE ORDNR                      TO ORDNR-N
           MOVE ORDNR-N-IO (2:6)           TO ORDNRN-IO
           SET NOT-I-60                    TO TRUE
           IF  ORDNRN > 0
               SET I-60                    TO TRUE
           END-IF.
 
       FAKVARE-GET SECTION.
       FAKVARE-GET-P.
           IF  FAKVARE-EOF-OFF
               READ FAKVARE
               AT END
                   SET FAKVARE-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FAKVARE-FLDSET SECTION.
       FAKVARE-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKVARE-IO-AREA (1:30) TO REC1 (1:30)
               MOVE FAKVARE-IO-AREA (4:4)  TO ORDNR-IO
           END-EVALUATE.
 
       FAKVARE-IDSET SECTION.
       FAKVARE-IDSET-P.
           SET I-01                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-60)
               MOVE SPACES TO FAKVARH-IO-AREA
               INITIALIZE FAKVARH-IO-AREA
               MOVE REC1                   TO FAKVARH-IO-AREA (1:30)
               MOVE ORDNRN-IO              TO FAKVARH-IO-AREA (4:6)
               MOVE 'O'                    TO FAKVARH-IO-AREA (31:1)
               WRITE FAKVARH-IO-AREA
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
           INITIALIZE FAKVARE-DATA-FIELDS
           SET FAKVARE-EOF-OFF             TO TRUE
           SET FAKVARE-PROCESS             TO TRUE
           OPEN INPUT FAKVARE
           OPEN OUTPUT FAKVARH.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FAKVARE
           CLOSE FAKVARH.
 
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
