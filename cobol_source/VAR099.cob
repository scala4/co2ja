       IDENTIFICATION DIVISION.
       PROGRAM-ID. VAR099R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM VAR099 AV ESPEN LARSEN 16.05.2004              *
      *  UTPLUKK TIL OPPSLAGSRECORD VIA LIMIT.                  *
      ***********************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VAR099.rpg
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
           SELECT OPPLIM
               ASSIGN TO OPPLIM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OPPLIM-STATUS.
           SELECT OPPSMAS
               ASSIGN TO OPPSMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS OPPSMAS-STATUS
               RECORD KEY IS OPPSMAS-KEY1.
           SELECT UTOPPS
               ASSIGN TO UT-S-UTOPPS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS UTOPPS-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD OPPLIM
               RECORD CONTAINS 80.
       01  OPPLIM-IO-AREA.
           05  OPPLIM-IO-AREA-X.
               10  OPPLIM-KEY1             PICTURE X(21).
               10  FILLER                  PICTURE X(59).
       FD OPPSMAS
               RECORD CONTAINS 30.
       01  OPPSMAS-IO-AREA.
           05  OPPSMAS-IO-AREA-X.
               10  OPPSMAS-KEY1            PICTURE X(21).
               10  FILLER                  PICTURE X(9).
       FD UTOPPS
               BLOCK CONTAINS 60
               RECORD CONTAINS 30.
       01  UTOPPS-IO-AREA.
           05  UTOPPS-IO-AREA-X            PICTURE X(30).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  OPPLIM-STATUS               PICTURE 99 VALUE 0.
           10  OPPSMAS-STATUS              PICTURE 99 VALUE 0.
           10  UTOPPS-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  OPPLIM-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  OPPLIM-EOF-OFF          VALUE '0'.
               88  OPPLIM-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  OPPLIM-READ-OFF         VALUE '0'.
               88  OPPLIM-READ             VALUE '1'.
           05  OPPLIM-LOW-KEY              PICTURE X(21).
           05  OPPLIM-HIGH-KEY             PICTURE X(21).
           05  OPPSMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  OPPSMAS-EOF-OFF         VALUE '0'.
               88  OPPSMAS-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  OPPSMAS-READ-OFF        VALUE '0'.
               88  OPPSMAS-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  OPPSMAS-PROCESS-OFF     VALUE '0'.
               88  OPPSMAS-PROCESS         VALUE '1'.
           05  OPPSMAS-DATA-FIELDS.
      *                                       1   1 RECART
               10  REC                     PICTURE X(30).
      *                    SETOF                     12
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
           IF  OPPSMAS-PROCESS
               SET OPPSMAS-PROCESS-OFF     TO TRUE
               SET OPPSMAS-READ            TO TRUE
           END-IF
 
           IF  OPPSMAS-READ
           AND RECORD-SELECTED-OFF
               PERFORM OPPSMAS-GET
               SET OPPSMAS-READ-OFF        TO TRUE
               IF  NOT OPPSMAS-EOF
                   SET OPPSMAS-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  OPPSMAS-PROCESS
               PERFORM OPPSMAS-IDSET
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
 
           IF  OPPSMAS-PROCESS
               PERFORM OPPSMAS-FLDSET
           END-IF
 
           SET NOT-I-1ST                   TO TRUE
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       OPPSMAS-GET SECTION.
       OPPSMAS-GET-P.
           IF  OPPSMAS-EOF-OFF
               PERFORM WITH TEST AFTER
                 UNTIL OPPLIM-READ-OFF
                    OR OPPLIM-EOF
                   IF  OPPLIM-READ
                       SET OPPLIM-READ-OFF TO TRUE
                       READ OPPLIM
                       AT END
                           SET OPPLIM-EOF  TO TRUE
                           SET OPPSMAS-EOF TO TRUE
                           SUBTRACT 1    FROM LR-CHECK
                       NOT AT END
                           MOVE OPPLIM-IO-AREA (1:4) TO OPPSMAS-KEY1
                       END-READ
                   END-IF
                   IF  OPPLIM-EOF-OFF
                   AND OPPLIM-READ-OFF
                       READ OPPSMAS
                       INVALID KEY
                           SET I-H0        TO TRUE
                           MOVE 'N'        TO E-R-R-O-R
                       END-READ
                   END-IF
               END-PERFORM
           END-IF.
 
       OPPSMAS-FLDSET SECTION.
       OPPSMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE OPPSMAS-IO-AREA (1:30) TO REC (1:30)
           END-EVALUATE.
 
       OPPSMAS-IDSET SECTION.
       OPPSMAS-IDSET-P.
           SET I-01                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               MOVE SPACES TO UTOPPS-IO-AREA
               INITIALIZE UTOPPS-IO-AREA
               MOVE REC                    TO UTOPPS-IO-AREA (1:30)
               WRITE UTOPPS-IO-AREA
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
           SET OPPLIM-EOF-OFF              TO TRUE
           SET OPPLIM-READ                 TO TRUE
           OPEN INPUT OPPLIM
           INITIALIZE OPPSMAS-DATA-FIELDS
           SET OPPSMAS-EOF-OFF             TO TRUE
           SET OPPSMAS-PROCESS             TO TRUE
           OPEN INPUT OPPSMAS
           OPEN OUTPUT UTOPPS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE OPPLIM
           CLOSE OPPSMAS
           CLOSE UTOPPS.
 
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
