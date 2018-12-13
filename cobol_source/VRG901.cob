       IDENTIFICATION DIVISION.
       PROGRAM-ID. VRG901R.
      **********************************************  Z-WIN-RPG2   ****
      * STND. HURTIG UTPLUKKSPROGRAM MED LIMITKORT FRA EDB-NR.       *
      ****************************************************************
      *                                          XX2000XXIRXXSS
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VRG901.rpg
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
           SELECT ENRLIM
               ASSIGN TO ENRLIM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ENRLIM-STATUS.
           SELECT VARETIL
               ASSIGN TO VARETIL
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VARETIL-STATUS
               RECORD KEY IS VARETIL-KEY1.
           SELECT OUTF
               ASSIGN TO UT-S-OUTF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD ENRLIM
               RECORD CONTAINS 80.
       01  ENRLIM-IO-AREA.
           05  ENRLIM-IO-AREA-X.
               10  ENRLIM-KEY1.
                   15  ENRLIM-KEY1N        PICTURE S9(12).
               10  FILLER                  PICTURE X(68).
       FD VARETIL
               RECORD CONTAINS 200.
       01  VARETIL-IO-AREA.
           05  VARETIL-IO-AREA-X.
               10  VARETIL-KEY1            PICTURE X(12).
               10  FILLER                  PICTURE X(188).
       FD OUTF
               BLOCK CONTAINS 2400
               RECORD CONTAINS 200.
       01  OUTF-IO-AREA.
           05  OUTF-IO-AREA-X              PICTURE X(200).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  ENRLIM-STATUS               PICTURE 99 VALUE 0.
           10  VARETIL-STATUS              PICTURE 99 VALUE 0.
           10  OUTF-STATUS                 PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  ENRLIM-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  ENRLIM-EOF-OFF          VALUE '0'.
               88  ENRLIM-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ENRLIM-READ-OFF         VALUE '0'.
               88  ENRLIM-READ             VALUE '1'.
           05  ENRLIM-LOW-KEY              PICTURE X(12).
           05  ENRLIM-HIGH-KEY             PICTURE X(12).
           05  VARETIL-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  VARETIL-EOF-OFF         VALUE '0'.
               88  VARETIL-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VARETIL-READ-OFF        VALUE '0'.
               88  VARETIL-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VARETIL-PROCESS-OFF     VALUE '0'.
               88  VARETIL-PROCESS         VALUE '1'.
           05  VARETIL-DATA-FIELDS.
               10  REC                     PICTURE X(200).
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
           IF  VARETIL-PROCESS
               SET VARETIL-PROCESS-OFF     TO TRUE
               SET VARETIL-READ            TO TRUE
           END-IF
 
           IF  VARETIL-READ
           AND RECORD-SELECTED-OFF
               PERFORM VARETIL-GET
               SET VARETIL-READ-OFF        TO TRUE
               IF  NOT VARETIL-EOF
                   PERFORM VARETIL-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET VARETIL-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  VARETIL-PROCESS
               PERFORM VARETIL-IDSET
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
 
           IF  VARETIL-PROCESS
               PERFORM VARETIL-FLDSET
           END-IF
 
           SET NOT-I-1ST                   TO TRUE
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       VARETIL-GET SECTION.
       VARETIL-GET-P.
           IF  VARETIL-EOF-OFF
               PERFORM WITH TEST AFTER
                 UNTIL ENRLIM-READ-OFF
                    OR ENRLIM-EOF
                   IF  ENRLIM-READ
                       SET ENRLIM-READ-OFF TO TRUE
                       READ ENRLIM
                       AT END
                           SET ENRLIM-EOF  TO TRUE
                           SET VARETIL-EOF TO TRUE
                       NOT AT END
                           MOVE ENRLIM-IO-AREA (1:4) TO VARETIL-KEY1
                       END-READ
                   END-IF
                   IF  ENRLIM-EOF-OFF
                   AND ENRLIM-READ-OFF
                       READ VARETIL
                       INVALID KEY
                           SET I-H0        TO TRUE
                           MOVE 'N'        TO E-R-R-O-R
                       END-READ
                   END-IF
               END-PERFORM
           END-IF.
 
       VARETIL-FLDSET SECTION.
       VARETIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ( VARETIL-IO-AREA (1:1) = '8'
            AND   VARETIL-IO-AREA (2:1) = '0' )
               MOVE VARETIL-IO-AREA (1:200) TO REC (1:200)
           END-EVALUATE.
 
       VARETIL-IDCHK SECTION.
       VARETIL-IDCHK-P.
           EVALUATE TRUE
           WHEN ( VARETIL-IO-AREA (1:1) = '8'
            AND   VARETIL-IO-AREA (2:1) = '0' )
             OR ( VARETIL-IO-AREA (1:1) NOT = '8'
            AND   VARETIL-IO-AREA (2:1) NOT = '0' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       VARETIL-IDSET SECTION.
       VARETIL-IDSET-P.
           EVALUATE TRUE
           WHEN ( VARETIL-IO-AREA (1:1) = '8'
            AND   VARETIL-IO-AREA (2:1) = '0' )
               SET I-01                    TO TRUE
           WHEN ( VARETIL-IO-AREA (1:1) NOT = '8'
            AND   VARETIL-IO-AREA (2:1) NOT = '0' )
               SET I-02                    TO TRUE
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               MOVE SPACES TO OUTF-IO-AREA
               INITIALIZE OUTF-IO-AREA
               MOVE REC                    TO OUTF-IO-AREA (1:200)
               WRITE OUTF-IO-AREA
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
           SET ENRLIM-EOF-OFF              TO TRUE
           SET ENRLIM-READ                 TO TRUE
           OPEN INPUT ENRLIM
           INITIALIZE VARETIL-DATA-FIELDS
           SET VARETIL-EOF-OFF             TO TRUE
           SET VARETIL-PROCESS             TO TRUE
           OPEN INPUT VARETIL
           OPEN OUTPUT OUTF.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE ENRLIM
           CLOSE VARETIL
           CLOSE OUTF.
 
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
