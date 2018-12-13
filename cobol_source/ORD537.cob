       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORD537R.
      **********************************************  Z-WIN-RPG2   ****
      **  DANNE EN FILE SOM INNEHOLDER ALLE ARTIKKLER SOM      **
      **  INNGÅR I ET KOMPLETT SALGSSETT.                      **
      ***********************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ORD537.rpg
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
           SELECT INNPUT
               ASSIGN TO UT-S-INNPUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INNPUT-STATUS.
           SELECT INNFILE
               ASSIGN TO UT-S-INNFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INNFILE-STATUS.
           SELECT OUTF
               ASSIGN TO UT-S-OUTF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD INNPUT
               BLOCK CONTAINS 2660
               RECORD CONTAINS 70.
       01  INNPUT-IO-AREA.
           05  INNPUT-IO-AREA-X            PICTURE X(70).
       FD INNFILE
               BLOCK CONTAINS 2000
               RECORD CONTAINS 25.
       01  INNFILE-IO-AREA.
           05  INNFILE-IO-AREA-X           PICTURE X(25).
       FD OUTF
               BLOCK CONTAINS 2660
               RECORD CONTAINS 70.
       01  OUTF-IO-AREA.
           05  OUTF-IO-AREA-X              PICTURE X(70).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  INNPUT-STATUS               PICTURE 99 VALUE 0.
           10  INNFILE-STATUS              PICTURE 99 VALUE 0.
           10  OUTF-STATUS                 PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  INNPUT-EOF-OFF          VALUE '0'.
               88  INNPUT-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNPUT-READ-OFF         VALUE '0'.
               88  INNPUT-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNPUT-PROCESS-OFF      VALUE '0'.
               88  INNPUT-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNFILE-EOF-OFF         VALUE '0'.
               88  INNFILE-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNFILE-READ-OFF        VALUE '0'.
               88  INNFILE-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNFILE-PROCESS-OFF     VALUE '0'.
               88  INNFILE-PROCESS         VALUE '1'.
           05  INNPUT-DATA-FIELDS.
               10  RA                      PICTURE X(1).
               10  BEST-IO.
                   15  BEST                PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  EDBNR                   PICTURE X(7).
               10  FIRMA                   PICTURE X(3).
           05  INNPUT-MP                   PICTURE X(10).
           05  INNPUT-MC                   PICTURE X(10).
           05  INNPUT-M-01             REDEFINES INNPUT-MC.
               10  INNPUT-M-01-M2.
                   15  INNPUT-M-01-M2-FIRMA-G.
                       20  INNPUT-M-01-M2-FIRMA PICTURE X(3).
               10  INNPUT-M-01-M1.
                   15  INNPUT-M-01-M1-EDBNR-G.
                       20  INNPUT-M-01-M1-EDBNR PICTURE X(7).
           05  INNFILE-DATA-FIELDS.
               10  NYNR                    PICTURE X(7).
               10  ANTALL-IO.
                   15  ANTALL              PICTURE S9(4).
           05  INNFILE-MP                  PICTURE X(10).
           05  INNFILE-MC                  PICTURE X(10).
           05  INNFILE-M-02            REDEFINES INNFILE-MC.
               10  INNFILE-M-02-M2.
                   15  INNFILE-M-02-M2-FIRMA-G.
                       20  INNFILE-M-02-M2-FIRMA PICTURE X(3).
               10  INNFILE-M-02-M1.
                   15  INNFILE-M-02-M1-EDBNR-G.
                       20  INNFILE-M-02-M1-EDBNR PICTURE X(7).
           05  TEMPORARY-FIELDS.
               10  TOTSUM-IO.
                   15  TOTSUM              PICTURE S9(5)V9(2).
           05  EDITTING-FIELDS.
               10  XO-52P-EF.
                 15  XO-52P                PICTURE S9(5)V9(2) USAGE
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
           IF  INNPUT-PROCESS
               SET INNPUT-PROCESS-OFF      TO TRUE
               SET INNPUT-READ             TO TRUE
           END-IF
 
           IF  INNPUT-READ
               PERFORM INNPUT-GET
               SET INNPUT-READ-OFF         TO TRUE
               IF  NOT INNPUT-EOF
                   PERFORM INNPUT-MATCH-SET
               END-IF
           END-IF
 
           IF  INNFILE-PROCESS
               SET INNFILE-PROCESS-OFF     TO TRUE
               SET INNFILE-READ            TO TRUE
           END-IF
 
           IF  INNFILE-READ
               PERFORM INNFILE-GET
               SET INNFILE-READ-OFF        TO TRUE
               IF  NOT INNFILE-EOF
                   PERFORM INNFILE-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  INNPUT-PROCESS
               PERFORM INNPUT-IDSET
           END-IF
 
           IF  INNFILE-PROCESS
               PERFORM INNFILE-IDSET
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
 
           IF  INNPUT-PROCESS
               PERFORM INNPUT-FLDSET
           END-IF
 
           IF  INNFILE-PROCESS
               PERFORM INNFILE-FLDSET
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
           IF  (I-01)
               SET NOT-I-80                TO TRUE
               IF  RA = 'V'
                   SET I-80                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-MR)
               MULTIPLY ANTALL BY BEST GIVING TOTSUM
           END-IF.
 
       INNPUT-GET SECTION.
       INNPUT-GET-P.
           IF  INNPUT-EOF-OFF
               READ INNPUT
               AT END
                   SET INNPUT-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INNPUT-FLDSET SECTION.
       INNPUT-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INNPUT-IO-AREA (1:1)   TO RA (1:1)
               MOVE INNPUT-IO-AREA (12:4)  TO BEST-IO
               MOVE INNPUT-IO-AREA (16:7)  TO EDBNR (1:7)
               MOVE INNPUT-IO-AREA (51:3)  TO FIRMA (1:3)
           END-EVALUATE.
 
       INNPUT-IDSET SECTION.
       INNPUT-IDSET-P.
           SET I-01                        TO TRUE.
 
       INNPUT-MATCH-SET SECTION.
       INNPUT-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE INNPUT-IO-AREA (51:3)  TO INNPUT-M-01-M2-FIRMA
               MOVE INNPUT-IO-AREA (16:7)  TO INNPUT-M-01-M1-EDBNR
           END-EVALUATE.
 
       INNFILE-GET SECTION.
       INNFILE-GET-P.
           IF  INNFILE-EOF-OFF
               READ INNFILE
               AT END
                   SET INNFILE-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INNFILE-FLDSET SECTION.
       INNFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INNFILE-IO-AREA (1:3)  TO FIRMA (1:3)
               MOVE INNFILE-IO-AREA (4:7)  TO EDBNR (1:7)
               MOVE INNFILE-IO-AREA (11:7) TO NYNR (1:7)
               MOVE INNFILE-IO-AREA (18:4) TO ANTALL-IO
               INSPECT ANTALL-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       INNFILE-IDSET SECTION.
       INNFILE-IDSET-P.
           SET I-02                        TO TRUE.
 
       INNFILE-MATCH-SET SECTION.
       INNFILE-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE INNFILE-IO-AREA (1:3)  TO INNFILE-M-02-M2-FIRMA
               MOVE INNFILE-IO-AREA (4:7)  TO INNFILE-M-02-M1-EDBNR
           END-EVALUATE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  INNPUT-EOF
               MOVE HIGH-VALUES            TO INNPUT-MC
                                              INNPUT-MP
           END-IF
           IF  INNFILE-EOF
               MOVE HIGH-VALUES            TO INNFILE-MC
                                              INNFILE-MP
           END-IF
           IF  INNPUT-MC < INNPUT-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  INNFILE-MC < INNFILE-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  INNPUT-MC < INNFILE-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET INNPUT-PROCESS      TO TRUE
                   MOVE INNPUT-MC          TO INNPUT-MP
                   IF  INNPUT-MC = INNFILE-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  INNFILE-MC < INNPUT-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET INNFILE-PROCESS     TO TRUE
                   MOVE INNFILE-MC         TO INNFILE-MP
                   IF  INNFILE-MC = INNPUT-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  INNPUT-MC = INNFILE-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET INNPUT-PROCESS      TO TRUE
                   MOVE INNPUT-MC          TO INNPUT-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND I-MR AND NOT-I-80)
               MOVE SPACES TO OUTF-IO-AREA
               INITIALIZE OUTF-IO-AREA
               MOVE '8'                    TO OUTF-IO-AREA (1:1)
               MOVE TOTSUM                 TO XO-52P
               MOVE XO-52P-EF              TO OUTF-IO-AREA (12:4)
               MOVE NYNR                   TO OUTF-IO-AREA (16:7)
               MOVE FIRMA                  TO OUTF-IO-AREA (51:3)
               WRITE OUTF-IO-AREA
           END-IF
           IF  (I-01 AND I-80)
               MOVE SPACES TO OUTF-IO-AREA
               INITIALIZE OUTF-IO-AREA
               MOVE '8'                    TO OUTF-IO-AREA (1:1)
               MOVE BEST                   TO XO-52P
               MOVE XO-52P-EF              TO OUTF-IO-AREA (12:4)
               MOVE EDBNR                  TO OUTF-IO-AREA (16:7)
               MOVE FIRMA                  TO OUTF-IO-AREA (51:3)
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
           MOVE 2                          TO LR-CHECK
           INITIALIZE INNPUT-DATA-FIELDS
           SET INNPUT-EOF-OFF              TO TRUE
           SET INNPUT-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO INNPUT-MC
                                              INNPUT-MP
           OPEN INPUT INNPUT
           INITIALIZE INNFILE-DATA-FIELDS
           SET INNFILE-EOF-OFF             TO TRUE
           SET INNFILE-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO INNFILE-MC
                                              INNFILE-MP
           OPEN INPUT INNFILE
           OPEN OUTPUT OUTF.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INNPUT
           CLOSE INNFILE
           CLOSE OUTF.
 
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
