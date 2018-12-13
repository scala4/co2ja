       IDENTIFICATION DIVISION.
       PROGRAM-ID. RSK015R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM.......: RSK015                                       *
      *  E 01.07.2010  : LAGT INN "*O" I SIGNATUR PÅ OCR-TRANSER      *
      *  E 15.04.2011  : UTVIDET REGFIL FRA 120 TIL 240 BYTE          *
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RSK015.rpg
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
           SELECT UTNRELF
               ASSIGN TO UT-S-UTNRELF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS UTNRELF-STATUS.
           SELECT FRARELF
               ASSIGN TO UT-S-FRARELF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FRARELF-STATUS.
           SELECT TOTRESK
               ASSIGN TO UT-S-TOTRESK
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TOTRESK-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD UTNRELF
               BLOCK CONTAINS 240
               RECORD CONTAINS 240.
       01  UTNRELF-IO-AREA.
           05  UTNRELF-IO-AREA-X           PICTURE X(240).
       FD FRARELF
               BLOCK CONTAINS 240
               RECORD CONTAINS 240.
       01  FRARELF-IO-AREA.
           05  FRARELF-IO-AREA-X           PICTURE X(240).
       FD TOTRESK
               BLOCK CONTAINS 240
               RECORD CONTAINS 240.
       01  TOTRESK-IO-AREA.
           05  TOTRESK-IO-AREA-X           PICTURE X(240).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  UTNRELF-STATUS              PICTURE 99 VALUE 0.
           10  FRARELF-STATUS              PICTURE 99 VALUE 0.
           10  TOTRESK-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  UTNRELF-EOF-OFF         VALUE '0'.
               88  UTNRELF-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  UTNRELF-READ-OFF        VALUE '0'.
               88  UTNRELF-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  UTNRELF-PROCESS-OFF     VALUE '0'.
               88  UTNRELF-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FRARELF-EOF-OFF         VALUE '0'.
               88  FRARELF-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FRARELF-READ-OFF        VALUE '0'.
               88  FRARELF-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FRARELF-PROCESS-OFF     VALUE '0'.
               88  FRARELF-PROCESS         VALUE '1'.
           05  UTNRELF-DATA-FIELDS.
               10  REC1                    PICTURE X(240).
               10  RA1                     PICTURE X(2).
           05  UTNRELF-MP                  PICTURE X(2).
           05  UTNRELF-MC                  PICTURE X(2).
           05  UTNRELF-M-01            REDEFINES UTNRELF-MC.
               10  UTNRELF-M-01-M1.
                   15  UTNRELF-M-01-M1-RA1-G.
                       20  UTNRELF-M-01-M1-RA1 PICTURE X(2).
           05  FRARELF-DATA-FIELDS.
               10  REC2                    PICTURE X(240).
               10  RA2                     PICTURE X(2).
           05  FRARELF-MP                  PICTURE X(2).
           05  FRARELF-MC                  PICTURE X(2).
           05  FRARELF-M-02            REDEFINES FRARELF-MC.
               10  FRARELF-M-02-M1.
                   15  FRARELF-M-02-M1-RA2-G.
                       20  FRARELF-M-02-M1-RA2 PICTURE X(2).
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
           IF  UTNRELF-PROCESS
               SET UTNRELF-PROCESS-OFF     TO TRUE
               SET UTNRELF-READ            TO TRUE
           END-IF
 
           IF  UTNRELF-READ
               PERFORM UTNRELF-GET
               SET UTNRELF-READ-OFF        TO TRUE
               IF  NOT UTNRELF-EOF
                   PERFORM UTNRELF-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM UTNRELF-MATCH-SET
               END-IF
           END-IF
 
           IF  FRARELF-PROCESS
               SET FRARELF-PROCESS-OFF     TO TRUE
               SET FRARELF-READ            TO TRUE
           END-IF
 
           IF  FRARELF-READ
               PERFORM FRARELF-GET
               SET FRARELF-READ-OFF        TO TRUE
               IF  NOT FRARELF-EOF
                   PERFORM FRARELF-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM FRARELF-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  UTNRELF-PROCESS
               PERFORM UTNRELF-IDSET
           END-IF
 
           IF  FRARELF-PROCESS
               PERFORM FRARELF-IDSET
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
 
           IF  UTNRELF-PROCESS
               PERFORM UTNRELF-FLDSET
           END-IF
 
           IF  FRARELF-PROCESS
               PERFORM FRARELF-FLDSET
           END-IF
 
           SET NOT-I-1ST                   TO TRUE
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       UTNRELF-GET SECTION.
       UTNRELF-GET-P.
           IF  UTNRELF-EOF-OFF
               READ UTNRELF
               AT END
                   SET UTNRELF-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       UTNRELF-FLDSET SECTION.
       UTNRELF-FLDSET-P.
           EVALUATE TRUE
           WHEN ( UTNRELF-IO-AREA (1:1) = '2'
            AND   UTNRELF-IO-AREA (2:1) = '1' )
               MOVE UTNRELF-IO-AREA (1:240) TO REC1 (1:240)
               MOVE UTNRELF-IO-AREA (1:2)  TO RA1 (1:2)
           END-EVALUATE.
 
       UTNRELF-IDCHK SECTION.
       UTNRELF-IDCHK-P.
           EVALUATE TRUE
           WHEN ( UTNRELF-IO-AREA (1:1) = '2'
            AND   UTNRELF-IO-AREA (2:1) = '1' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       UTNRELF-IDSET SECTION.
       UTNRELF-IDSET-P.
           EVALUATE TRUE
           WHEN ( UTNRELF-IO-AREA (1:1) = '2'
            AND   UTNRELF-IO-AREA (2:1) = '1' )
               SET I-01                    TO TRUE
           END-EVALUATE.
 
       UTNRELF-MATCH-SET SECTION.
       UTNRELF-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( UTNRELF-IO-AREA (1:1) = '2'
            AND   UTNRELF-IO-AREA (2:1) = '1' )
               MOVE UTNRELF-IO-AREA (1:2)  TO UTNRELF-M-01-M1-RA1
           END-EVALUATE.
 
       FRARELF-GET SECTION.
       FRARELF-GET-P.
           IF  FRARELF-EOF-OFF
               READ FRARELF
               AT END
                   SET FRARELF-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FRARELF-FLDSET SECTION.
       FRARELF-FLDSET-P.
           EVALUATE TRUE
           WHEN ( FRARELF-IO-AREA (1:1) = '2'
            AND   FRARELF-IO-AREA (2:1) = '1' )
               MOVE FRARELF-IO-AREA (1:240) TO REC2 (1:240)
               MOVE FRARELF-IO-AREA (1:2)  TO RA2 (1:2)
           END-EVALUATE.
 
       FRARELF-IDCHK SECTION.
       FRARELF-IDCHK-P.
           EVALUATE TRUE
           WHEN ( FRARELF-IO-AREA (1:1) = '2'
            AND   FRARELF-IO-AREA (2:1) = '1' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       FRARELF-IDSET SECTION.
       FRARELF-IDSET-P.
           EVALUATE TRUE
           WHEN ( FRARELF-IO-AREA (1:1) = '2'
            AND   FRARELF-IO-AREA (2:1) = '1' )
               SET I-02                    TO TRUE
           END-EVALUATE.
 
       FRARELF-MATCH-SET SECTION.
       FRARELF-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( FRARELF-IO-AREA (1:1) = '2'
            AND   FRARELF-IO-AREA (2:1) = '1' )
               MOVE FRARELF-IO-AREA (1:2)  TO FRARELF-M-02-M1-RA2
           END-EVALUATE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  UTNRELF-EOF
               MOVE HIGH-VALUES            TO UTNRELF-MC
                                              UTNRELF-MP
           END-IF
           IF  FRARELF-EOF
               MOVE HIGH-VALUES            TO FRARELF-MC
                                              FRARELF-MP
           END-IF
           IF  UTNRELF-MC < UTNRELF-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  FRARELF-MC < FRARELF-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  UTNRELF-MC < FRARELF-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET UTNRELF-PROCESS     TO TRUE
                   MOVE UTNRELF-MC         TO UTNRELF-MP
                   IF  UTNRELF-MC = FRARELF-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  FRARELF-MC < UTNRELF-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FRARELF-PROCESS     TO TRUE
                   MOVE FRARELF-MC         TO FRARELF-MP
                   IF  FRARELF-MC = UTNRELF-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  UTNRELF-MC = FRARELF-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET UTNRELF-PROCESS     TO TRUE
                   MOVE UTNRELF-MC         TO UTNRELF-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               MOVE SPACES TO TOTRESK-IO-AREA
               INITIALIZE TOTRESK-IO-AREA
               MOVE REC1                   TO TOTRESK-IO-AREA (1:240)
               WRITE TOTRESK-IO-AREA
           END-IF
           IF  (I-02)
               MOVE SPACES TO TOTRESK-IO-AREA
               INITIALIZE TOTRESK-IO-AREA
               MOVE REC2                   TO TOTRESK-IO-AREA (1:240)
               MOVE '*O'                   TO TOTRESK-IO-AREA (85:2)
               WRITE TOTRESK-IO-AREA
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
           INITIALIZE UTNRELF-DATA-FIELDS
           SET UTNRELF-EOF-OFF             TO TRUE
           SET UTNRELF-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO UTNRELF-MC
                                              UTNRELF-MP
           OPEN INPUT UTNRELF
           INITIALIZE FRARELF-DATA-FIELDS
           SET FRARELF-EOF-OFF             TO TRUE
           SET FRARELF-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO FRARELF-MC
                                              FRARELF-MP
           OPEN INPUT FRARELF
           OPEN OUTPUT TOTRESK.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE UTNRELF
           CLOSE FRARELF
           CLOSE TOTRESK.
 
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
