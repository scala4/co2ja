       IDENTIFICATION DIVISION.
       PROGRAM-ID. RSK125R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM.......: RSK125                                       *
      *  DANNE NY RELFILE FRA INKASSO OG PURRERUTINE.                 *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RSK125.rpg
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
           SELECT GMLFILE
               ASSIGN TO UT-S-GMLFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS GMLFILE-STATUS.
           SELECT INKFILE
               ASSIGN TO UT-S-INKFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INKFILE-STATUS.
           SELECT NYFILE
               ASSIGN TO UT-S-NYFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS NYFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD GMLFILE
               BLOCK CONTAINS 2640
               RECORD CONTAINS 60.
       01  GMLFILE-IO-AREA.
           05  GMLFILE-IO-AREA-X           PICTURE X(60).
       FD INKFILE
               BLOCK CONTAINS 2640
               RECORD CONTAINS 60.
       01  INKFILE-IO-AREA.
           05  INKFILE-IO-AREA-X           PICTURE X(60).
       FD NYFILE
               BLOCK CONTAINS 2640
               RECORD CONTAINS 60.
       01  NYFILE-IO-AREA.
           05  NYFILE-IO-AREA-X            PICTURE X(60).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  GMLFILE-STATUS              PICTURE 99 VALUE 0.
           10  INKFILE-STATUS              PICTURE 99 VALUE 0.
           10  NYFILE-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLFILE-EOF-OFF         VALUE '0'.
               88  GMLFILE-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLFILE-READ-OFF        VALUE '0'.
               88  GMLFILE-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLFILE-PROCESS-OFF     VALUE '0'.
               88  GMLFILE-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INKFILE-EOF-OFF         VALUE '0'.
               88  INKFILE-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INKFILE-READ-OFF        VALUE '0'.
               88  INKFILE-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INKFILE-PROCESS-OFF     VALUE '0'.
               88  INKFILE-PROCESS         VALUE '1'.
           05  GMLFILE-DATA-FIELDS.
               10  REC1                    PICTURE X(60).
               10  KNRG                    PICTURE X(9).
               10  INKG                    PICTURE X(6).
           05  GMLFILE-MP                  PICTURE X(15).
           05  GMLFILE-MC                  PICTURE X(15).
           05  GMLFILE-M-01            REDEFINES GMLFILE-MC.
               10  GMLFILE-M-01-M2.
                   15  GMLFILE-M-01-M2-KNRG-G.
                       20  GMLFILE-M-01-M2-KNRG PICTURE X(9).
               10  GMLFILE-M-01-M1.
                   15  GMLFILE-M-01-M1-INKG-G.
                       20  GMLFILE-M-01-M1-INKG PICTURE X(6).
           05  INKFILE-DATA-FIELDS.
               10  REC2                    PICTURE X(60).
               10  KNRI                    PICTURE X(9).
               10  INKI                    PICTURE X(6).
           05  INKFILE-MP                  PICTURE X(15).
           05  INKFILE-MC                  PICTURE X(15).
           05  INKFILE-M-02            REDEFINES INKFILE-MC.
               10  INKFILE-M-02-M2.
                   15  INKFILE-M-02-M2-KNRI-G.
                       20  INKFILE-M-02-M2-KNRI PICTURE X(9).
               10  INKFILE-M-02-M1.
                   15  INKFILE-M-02-M1-INKI-G.
                       20  INKFILE-M-02-M1-INKI PICTURE X(6).
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
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  GMLFILE-PROCESS
               SET GMLFILE-PROCESS-OFF     TO TRUE
               SET GMLFILE-READ            TO TRUE
           END-IF
 
           IF  GMLFILE-READ
               PERFORM GMLFILE-GET
               SET GMLFILE-READ-OFF        TO TRUE
               IF  NOT GMLFILE-EOF
                   PERFORM GMLFILE-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM GMLFILE-MATCH-SET
               END-IF
           END-IF
 
           IF  INKFILE-PROCESS
               SET INKFILE-PROCESS-OFF     TO TRUE
               SET INKFILE-READ            TO TRUE
           END-IF
 
           IF  INKFILE-READ
               PERFORM INKFILE-GET
               SET INKFILE-READ-OFF        TO TRUE
               IF  NOT INKFILE-EOF
                   PERFORM INKFILE-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM INKFILE-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  GMLFILE-PROCESS
               PERFORM GMLFILE-IDSET
           END-IF
 
           IF  INKFILE-PROCESS
               PERFORM INKFILE-IDSET
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
 
           IF  GMLFILE-PROCESS
               PERFORM GMLFILE-FLDSET
           END-IF
 
           IF  INKFILE-PROCESS
               PERFORM INKFILE-FLDSET
           END-IF
 
           SET NOT-I-1ST                   TO TRUE
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       GMLFILE-GET SECTION.
       GMLFILE-GET-P.
           IF  GMLFILE-EOF-OFF
               READ GMLFILE
               AT END
                   SET GMLFILE-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       GMLFILE-FLDSET SECTION.
       GMLFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ( GMLFILE-IO-AREA (1:1) = '4'
            AND   GMLFILE-IO-AREA (2:1) = '0' )
               MOVE GMLFILE-IO-AREA (1:60) TO REC1 (1:60)
               MOVE GMLFILE-IO-AREA (3:9)  TO KNRG (1:9)
               MOVE GMLFILE-IO-AREA (12:6) TO INKG (1:6)
           END-EVALUATE.
 
       GMLFILE-IDCHK SECTION.
       GMLFILE-IDCHK-P.
           EVALUATE TRUE
           WHEN ( GMLFILE-IO-AREA (1:1) = '4'
            AND   GMLFILE-IO-AREA (2:1) = '0' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       GMLFILE-IDSET SECTION.
       GMLFILE-IDSET-P.
           EVALUATE TRUE
           WHEN ( GMLFILE-IO-AREA (1:1) = '4'
            AND   GMLFILE-IO-AREA (2:1) = '0' )
               SET I-01                    TO TRUE
           END-EVALUATE.
 
       GMLFILE-MATCH-SET SECTION.
       GMLFILE-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( GMLFILE-IO-AREA (1:1) = '4'
            AND   GMLFILE-IO-AREA (2:1) = '0' )
               MOVE GMLFILE-IO-AREA (3:9)  TO GMLFILE-M-01-M2-KNRG
               MOVE GMLFILE-IO-AREA (12:6) TO GMLFILE-M-01-M1-INKG
           END-EVALUATE.
 
       INKFILE-GET SECTION.
       INKFILE-GET-P.
           IF  INKFILE-EOF-OFF
               READ INKFILE
               AT END
                   SET INKFILE-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INKFILE-FLDSET SECTION.
       INKFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ( INKFILE-IO-AREA (1:1) = '4'
            AND   INKFILE-IO-AREA (2:1) = '0' )
               MOVE INKFILE-IO-AREA (1:60) TO REC2 (1:60)
               MOVE INKFILE-IO-AREA (3:9)  TO KNRI (1:9)
               MOVE INKFILE-IO-AREA (12:6) TO INKI (1:6)
           END-EVALUATE.
 
       INKFILE-IDCHK SECTION.
       INKFILE-IDCHK-P.
           EVALUATE TRUE
           WHEN ( INKFILE-IO-AREA (1:1) = '4'
            AND   INKFILE-IO-AREA (2:1) = '0' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       INKFILE-IDSET SECTION.
       INKFILE-IDSET-P.
           EVALUATE TRUE
           WHEN ( INKFILE-IO-AREA (1:1) = '4'
            AND   INKFILE-IO-AREA (2:1) = '0' )
               SET I-02                    TO TRUE
           END-EVALUATE.
 
       INKFILE-MATCH-SET SECTION.
       INKFILE-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( INKFILE-IO-AREA (1:1) = '4'
            AND   INKFILE-IO-AREA (2:1) = '0' )
               MOVE INKFILE-IO-AREA (3:9)  TO INKFILE-M-02-M2-KNRI
               MOVE INKFILE-IO-AREA (12:6) TO INKFILE-M-02-M1-INKI
           END-EVALUATE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  GMLFILE-EOF
               MOVE HIGH-VALUES            TO GMLFILE-MC
                                              GMLFILE-MP
           END-IF
           IF  INKFILE-EOF
               MOVE HIGH-VALUES            TO INKFILE-MC
                                              INKFILE-MP
           END-IF
           IF  GMLFILE-MC < GMLFILE-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  INKFILE-MC < INKFILE-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  GMLFILE-MC < INKFILE-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET GMLFILE-PROCESS     TO TRUE
                   MOVE GMLFILE-MC         TO GMLFILE-MP
                   IF  GMLFILE-MC = INKFILE-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  INKFILE-MC < GMLFILE-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET INKFILE-PROCESS     TO TRUE
                   MOVE INKFILE-MC         TO INKFILE-MP
                   IF  INKFILE-MC = GMLFILE-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  GMLFILE-MC = INKFILE-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET GMLFILE-PROCESS     TO TRUE
                   MOVE GMLFILE-MC         TO GMLFILE-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND NOT-I-MR)
           OR  (I-02)
               MOVE SPACES TO NYFILE-IO-AREA
               INITIALIZE NYFILE-IO-AREA
               IF  (I-01 AND NOT-I-MR)
                   MOVE REC1               TO NYFILE-IO-AREA (1:60)
               END-IF
               IF  (I-02)
                   MOVE REC2               TO NYFILE-IO-AREA (1:60)
               END-IF
               WRITE NYFILE-IO-AREA
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
           INITIALIZE GMLFILE-DATA-FIELDS
           SET GMLFILE-EOF-OFF             TO TRUE
           SET GMLFILE-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO GMLFILE-MC
                                              GMLFILE-MP
           OPEN INPUT GMLFILE
           INITIALIZE INKFILE-DATA-FIELDS
           SET INKFILE-EOF-OFF             TO TRUE
           SET INKFILE-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO INKFILE-MC
                                              INKFILE-MP
           OPEN INPUT INKFILE
           OPEN OUTPUT NYFILE.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE GMLFILE
           CLOSE INKFILE
           CLOSE NYFILE.
 
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
