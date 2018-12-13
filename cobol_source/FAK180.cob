       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK180R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAMMET BLANDER FAK075 VAREREC OG FERDIGMELDTE VERKTØYKASSER  *
      *********************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK180.rpg
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
           SELECT VAREREC
               ASSIGN TO UT-S-VAREREC
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VAREREC-STATUS.
           SELECT VAREREC-TMP
               ASSIGN TO UT-S-VAREREC-TMP
               ACCESS MODE  IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VAREREC-STATUS.
           SELECT SORT-WK1
               ASSIGN TO UT-S-SORT-WK1.
           SELECT VERKTO-ELGY
               ASSIGN TO UT-S-VERKTO-ELGY
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VERKTO-ELGY-STATUS.
           SELECT VAREUT
               ASSIGN TO UT-S-VAREUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VAREUT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD VAREREC
               BLOCK CONTAINS 4100
               RECORD CONTAINS 82.
       01  VAREREC-IO-AREA.
           05  VAREREC-IO-AREA-X           PICTURE X(82).
       SD SORT-WK1
           DATA RECORD IS SORT-REC1.
       01  SORT-REC1.
           05  FILLER                  PIC X(15).
           05  SORT-WK1-K1             PIC X(7).
           05  FILLER                  PIC X(29).
           05  SORT-WK1-K2             PIC X(3).
           05  FILLER                  PIC X(30).
       FD VERKTO-ELGY
               BLOCK CONTAINS 2660
               RECORD CONTAINS 70.
       01  VERKTO-ELGY-IO-AREA.
           05  VERKTO-ELGY-IO-AREA-X       PICTURE X(70).
       FD VAREUT
               BLOCK CONTAINS 2660
               RECORD CONTAINS 70.
       01  VAREUT-IO-AREA.
           05  VAREUT-IO-AREA-X            PICTURE X(70).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  VAREREC-STATUS              PICTURE 99 VALUE 0.
           10  VERKTO-ELGY-STATUS          PICTURE 99 VALUE 0.
           10  VAREUT-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREREC-EOF-OFF         VALUE '0'.
               88  VAREREC-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREREC-READ-OFF        VALUE '0'.
               88  VAREREC-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREREC-PROCESS-OFF     VALUE '0'.
               88  VAREREC-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VERKTO-ELGY-EOF-OFF     VALUE '0'.
               88  VERKTO-ELGY-EOF         VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VERKTO-ELGY-READ-OFF    VALUE '0'.
               88  VERKTO-ELGY-READ        VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VERKTO-ELGY-PROCESS-OFF VALUE '0'.
               88  VERKTO-ELGY-PROCESS     VALUE '1'.
           05  VAREREC-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  EDBNR                   PICTURE X(7).
               10  REC1                    PICTURE X(70).
           05  VAREREC-MP                  PICTURE X(10).
           05  VAREREC-MC                  PICTURE X(10).
           05  VAREREC-M-01            REDEFINES VAREREC-MC.
               10  VAREREC-M-01-M2.
                   15  VAREREC-M-01-M2-FIRMA-G.
                       20  VAREREC-M-01-M2-FIRMA PICTURE X(3).
               10  VAREREC-M-01-M1.
                   15  VAREREC-M-01-M1-EDBNR-G.
                       20  VAREREC-M-01-M1-EDBNR PICTURE X(7).
           05  VERKTO-ELGY-DATA-FIELDS.
               10  REC2                    PICTURE X(70).
           05  VERKTO-ELGY-MP              PICTURE X(10).
           05  VERKTO-ELGY-MC              PICTURE X(10).
           05  VERKTO-ELGY-M-02        REDEFINES VERKTO-ELGY-MC.
               10  VERKTO-ELGY-M-02-M2.
                   15  VERKTO-ELGY-M-02-M2-FIRMA-G.
                       20  VERKTO-ELGY-M-02-M2-FIRMA PICTURE X(3).
               10  VERKTO-ELGY-M-02-M1.
                   15  VERKTO-ELGY-M-02-M1-EDBNR-G.
                       20  VERKTO-ELGY-M-02-M1-EDBNR PICTURE X(7).
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
           IF  VAREREC-PROCESS
               SET VAREREC-PROCESS-OFF     TO TRUE
               SET VAREREC-READ            TO TRUE
           END-IF
 
           IF  VAREREC-READ
               PERFORM VAREREC-GET
               SET VAREREC-READ-OFF        TO TRUE
               IF  NOT VAREREC-EOF
                   PERFORM VAREREC-MATCH-SET
               END-IF
           END-IF
 
           IF  VERKTO-ELGY-PROCESS
               SET VERKTO-ELGY-PROCESS-OFF TO TRUE
               SET VERKTO-ELGY-READ        TO TRUE
           END-IF
 
           IF  VERKTO-ELGY-READ
               PERFORM VERKTO-ELGY-GET
               SET VERKTO-ELGY-READ-OFF    TO TRUE
               IF  NOT VERKTO-ELGY-EOF
                   PERFORM VERKTO-ELGY-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  VAREREC-PROCESS
               PERFORM VAREREC-IDSET
           END-IF
 
           IF  VERKTO-ELGY-PROCESS
               PERFORM VERKTO-ELGY-IDSET
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
 
           IF  VAREREC-PROCESS
               PERFORM VAREREC-FLDSET
           END-IF
 
           IF  VERKTO-ELGY-PROCESS
               PERFORM VERKTO-ELGY-FLDSET
           END-IF
 
           SET NOT-I-1ST                   TO TRUE
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       VAREREC-GET SECTION.
       VAREREC-GET-P.
           IF  VAREREC-EOF-OFF
               READ VAREREC-TMP
               AT END
                   SET VAREREC-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VAREREC-FLDSET SECTION.
       VAREREC-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREREC-IO-AREA (51:3) TO FIRMA (1:3)
               MOVE VAREREC-IO-AREA (16:7) TO EDBNR (1:7)
               MOVE VAREREC-IO-AREA (1:70) TO REC1 (1:70)
           END-EVALUATE.
 
       VAREREC-IDSET SECTION.
       VAREREC-IDSET-P.
           SET I-01                        TO TRUE.
 
       VAREREC-MATCH-SET SECTION.
       VAREREC-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREREC-IO-AREA (51:3) TO VAREREC-M-01-M2-FIRMA
               MOVE VAREREC-IO-AREA (16:7) TO VAREREC-M-01-M1-EDBNR
           END-EVALUATE.
 
       VERKTO-ELGY-GET SECTION.
       VERKTO-ELGY-GET-P.
           IF  VERKTO-ELGY-EOF-OFF
               READ VERKTO-ELGY
               AT END
                   SET VERKTO-ELGY-EOF     TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VERKTO-ELGY-FLDSET SECTION.
       VERKTO-ELGY-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VERKTO-ELGY-IO-AREA (51:3) TO FIRMA (1:3)
               MOVE VERKTO-ELGY-IO-AREA (16:7) TO EDBNR (1:7)
               MOVE VERKTO-ELGY-IO-AREA (1:70) TO REC2 (1:70)
           END-EVALUATE.
 
       VERKTO-ELGY-IDSET SECTION.
       VERKTO-ELGY-IDSET-P.
           SET I-02                        TO TRUE.
 
       VERKTO-ELGY-MATCH-SET SECTION.
       VERKTO-ELGY-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE VERKTO-ELGY-IO-AREA (51:3) TO
                                             VERKTO-ELGY-M-02-M2-FIRMA
               MOVE VERKTO-ELGY-IO-AREA (16:7) TO
                                             VERKTO-ELGY-M-02-M1-EDBNR
           END-EVALUATE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  VAREREC-EOF
               MOVE HIGH-VALUES            TO VAREREC-MC
                                              VAREREC-MP
           END-IF
           IF  VERKTO-ELGY-EOF
               MOVE HIGH-VALUES            TO VERKTO-ELGY-MC
                                              VERKTO-ELGY-MP
           END-IF
           IF  VAREREC-MC < VAREREC-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  VERKTO-ELGY-MC < VERKTO-ELGY-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  VAREREC-MC < VERKTO-ELGY-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VAREREC-PROCESS     TO TRUE
                   MOVE VAREREC-MC         TO VAREREC-MP
                   IF  VAREREC-MC = VERKTO-ELGY-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  VERKTO-ELGY-MC < VAREREC-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VERKTO-ELGY-PROCESS TO TRUE
                   MOVE VERKTO-ELGY-MC     TO VERKTO-ELGY-MP
                   IF  VERKTO-ELGY-MC = VAREREC-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  VAREREC-MC = VERKTO-ELGY-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VAREREC-PROCESS     TO TRUE
                   MOVE VAREREC-MC         TO VAREREC-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               MOVE SPACES TO VAREUT-IO-AREA
               INITIALIZE VAREUT-IO-AREA
               MOVE REC1                   TO VAREUT-IO-AREA (1:70)
               WRITE VAREUT-IO-AREA
           END-IF
           IF  (I-02)
               MOVE SPACES TO VAREUT-IO-AREA
               INITIALIZE VAREUT-IO-AREA
               MOVE REC2                   TO VAREUT-IO-AREA (1:70)
               WRITE VAREUT-IO-AREA
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
           INITIALIZE VAREREC-DATA-FIELDS
           SET VAREREC-EOF-OFF             TO TRUE
           SET VAREREC-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO VAREREC-MC
                                              VAREREC-MP
           SORT SORT-WK1
               ASCENDING  SORT-WK1-K1
               ASCENDING  SORT-WK1-K2
               USING  VAREREC
               GIVING VAREREC-TMP.
           OPEN INPUT  VAREREC-TMP.
           INITIALIZE VERKTO-ELGY-DATA-FIELDS
           SET VERKTO-ELGY-EOF-OFF         TO TRUE
           SET VERKTO-ELGY-PROCESS         TO TRUE
           MOVE LOW-VALUES                 TO VERKTO-ELGY-MC
                                              VERKTO-ELGY-MP
           OPEN INPUT VERKTO-ELGY
           OPEN OUTPUT VAREUT.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE VAREREC-TMP
           CLOSE VERKTO-ELGY
           CLOSE VAREUT.
 
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
