       IDENTIFICATION DIVISION.
       PROGRAM-ID. NYB500R.
      **********************************************  Z-WIN-RPG2   ****
      *            REORG AV TILGANGSMASTER.           *
      *                       STEIN SANDVOLD 19.11.86.*
      *************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: NYB500.rpg
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
           SELECT TILMAS
               ASSIGN TO UT-S-TILMAS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TILMAS-STATUS.
           SELECT TILMAST
               ASSIGN TO TILMAST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS TILMAST-STATUS
               RECORD KEY IS TILMAST-KEY1.
       DATA DIVISION.
       FILE SECTION.
       FD TILMAS
               BLOCK CONTAINS 4080
               RECORD CONTAINS 120.
       01  TILMAS-IO-AREA.
           05  TILMAS-IO-AREA-X            PICTURE X(120).
       FD TILMAST
               RECORD CONTAINS 120.
       01  TILMAST-IO-AREA.
           05  TILMAST-IO-AREA-X.
               10  TILMAST-KEY1.
                   15  TILMAST-KEY1N       PICTURE S9(13).
               10  FILLER                  PICTURE X(107).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  TILMAS-STATUS               PICTURE 99 VALUE 0.
           10  TILMAST-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  TILMAS-EOF-OFF          VALUE '0'.
               88  TILMAS-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  TILMAS-READ-OFF         VALUE '0'.
               88  TILMAS-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  TILMAS-PROCESS-OFF      VALUE '0'.
               88  TILMAS-PROCESS          VALUE '1'.
           05  TILMAST-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  TILMAS-DATA-FIELDS.
               10  REC                     PICTURE X(120).
               10  ANTAL-IO.
                   15  ANTAL               PICTURE S9(6)V9(2).
               10  PKODE                   PICTURE X(2).
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
           IF  TILMAS-PROCESS
               SET TILMAS-PROCESS-OFF      TO TRUE
               SET TILMAS-READ             TO TRUE
           END-IF
 
           IF  TILMAS-READ
           AND RECORD-SELECTED-OFF
               PERFORM TILMAS-GET
               SET TILMAS-READ-OFF         TO TRUE
               IF  NOT TILMAS-EOF
                   SET TILMAS-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  TILMAS-PROCESS
               PERFORM TILMAS-IDSET
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
 
           IF  TILMAS-PROCESS
               PERFORM TILMAS-FLDOFF
               PERFORM TILMAS-FLDSET
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
           IF  (I-10)
               SET NOT-I-11                TO TRUE
               IF  PKODE = 'DP'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-10 AND I-11)
               SET NOT-I-10                TO TRUE
           END-IF.
 
       TILMAS-GET SECTION.
       TILMAS-GET-P.
           IF  TILMAS-EOF-OFF
               READ TILMAS
               AT END
                   SET TILMAS-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       TILMAS-FLDOFF SECTION.
       TILMAS-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( TILMAS-IO-AREA (2:1) = '1' )
             OR ( TILMAS-IO-AREA (2:1) = '2' )
             OR ( TILMAS-IO-AREA (2:1) = '3' )
             OR ( TILMAS-IO-AREA (2:1) = '4' )
             OR ( TILMAS-IO-AREA (2:1) = '5' )
             OR ( TILMAS-IO-AREA (2:1) = '6' )
             OR ( TILMAS-IO-AREA (2:1) = '7' )
             OR ( TILMAS-IO-AREA (2:1) = '8' )
             OR ( TILMAS-IO-AREA (2:1) = '9' )
               SET NOT-I-10                TO TRUE
           END-EVALUATE.
 
       TILMAS-FLDSET SECTION.
       TILMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ( TILMAS-IO-AREA (2:1) = '1' )
             OR ( TILMAS-IO-AREA (2:1) = '2' )
             OR ( TILMAS-IO-AREA (2:1) = '3' )
             OR ( TILMAS-IO-AREA (2:1) = '4' )
             OR ( TILMAS-IO-AREA (2:1) = '5' )
             OR ( TILMAS-IO-AREA (2:1) = '6' )
             OR ( TILMAS-IO-AREA (2:1) = '7' )
             OR ( TILMAS-IO-AREA (2:1) = '8' )
             OR ( TILMAS-IO-AREA (2:1) = '9' )
               MOVE TILMAS-IO-AREA (1:120) TO REC (1:120)
               MOVE TILMAS-IO-AREA (30:8)  TO ANTAL-IO
               INSPECT ANTAL-IO REPLACING ALL ' ' BY '0'
               IF  ANTAL = ZERO
                   SET I-10                TO TRUE
               END-IF
               MOVE TILMAS-IO-AREA (44:2)  TO PKODE (1:2)
           END-EVALUATE.
 
       TILMAS-IDSET SECTION.
       TILMAS-IDSET-P.
           EVALUATE TRUE
           WHEN ( TILMAS-IO-AREA (2:1) = '1' )
             OR ( TILMAS-IO-AREA (2:1) = '2' )
             OR ( TILMAS-IO-AREA (2:1) = '3' )
             OR ( TILMAS-IO-AREA (2:1) = '4' )
             OR ( TILMAS-IO-AREA (2:1) = '5' )
             OR ( TILMAS-IO-AREA (2:1) = '6' )
             OR ( TILMAS-IO-AREA (2:1) = '7' )
             OR ( TILMAS-IO-AREA (2:1) = '8' )
             OR ( TILMAS-IO-AREA (2:1) = '9' )
               SET I-01                    TO TRUE
           WHEN  OTHER
               SET I-02                    TO TRUE
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND NOT-I-10 AND I-U1)
               MOVE SPACES TO TILMAST-IO-AREA
               INITIALIZE TILMAST-IO-AREA
               MOVE REC                    TO TILMAST-IO-AREA (1:120)
               WRITE TILMAST-IO-AREA
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
           INITIALIZE TILMAS-DATA-FIELDS
           SET TILMAS-EOF-OFF              TO TRUE
           SET TILMAS-PROCESS              TO TRUE
           OPEN INPUT TILMAS
           OPEN OUTPUT TILMAST.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE TILMAS
           CLOSE TILMAST.
 
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
