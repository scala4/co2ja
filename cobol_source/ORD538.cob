       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORD538R.
      **********************************************  Z-WIN-RPG2   ****
      **       OPPSAMLING TIL VARESTAT-FILE.              **
      ******************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ORD538.rpg
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
           SELECT INNF
               ASSIGN TO UT-S-INNF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INNF-STATUS.
           SELECT GMLINN
               ASSIGN TO UT-S-GMLINN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS GMLINN-STATUS.
           SELECT NYUT
               ASSIGN TO UT-S-NYUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS NYUT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD INNF
               BLOCK CONTAINS 2660
               RECORD CONTAINS 70.
       01  INNF-IO-AREA.
           05  INNF-IO-AREA-X              PICTURE X(70).
       FD GMLINN
               BLOCK CONTAINS 2660
               RECORD CONTAINS 70.
       01  GMLINN-IO-AREA.
           05  GMLINN-IO-AREA-X            PICTURE X(70).
       FD NYUT
               BLOCK CONTAINS 2660
               RECORD CONTAINS 70.
       01  NYUT-IO-AREA.
           05  NYUT-IO-AREA-X              PICTURE X(70).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  INNF-STATUS                 PICTURE 99 VALUE 0.
           10  GMLINN-STATUS               PICTURE 99 VALUE 0.
           10  NYUT-STATUS                 PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  INNF-EOF-OFF            VALUE '0'.
               88  INNF-EOF                VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNF-READ-OFF           VALUE '0'.
               88  INNF-READ               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNF-PROCESS-OFF        VALUE '0'.
               88  INNF-PROCESS            VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  INNF-LEVEL-INIT-OFF     VALUE '0'.
               88  INNF-LEVEL-INIT         VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLINN-EOF-OFF          VALUE '0'.
               88  GMLINN-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLINN-READ-OFF         VALUE '0'.
               88  GMLINN-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLINN-PROCESS-OFF      VALUE '0'.
               88  GMLINN-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  GMLINN-LEVEL-INIT-OFF   VALUE '0'.
               88  GMLINN-LEVEL-INIT       VALUE '1'.
           05  INNF-LEVEL-01.
               10  INNF-01-L1.
                   15  INNF-01-L1-FIRMA    PICTURE X(3).
           05  INNF-DATA-FIELDS.
               10  ANTALL-IO.
                   15  ANTALL              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  EDBNR                   PICTURE X(7).
               10  FIRMA                   PICTURE X(3).
           05  INNF-MP                     PICTURE X(10).
           05  INNF-MC                     PICTURE X(10).
           05  INNF-M-01               REDEFINES INNF-MC.
               10  INNF-M-01-M2.
                   15  INNF-M-01-M2-FIRMA-G.
                       20  INNF-M-01-M2-FIRMA PICTURE X(3).
               10  INNF-M-01-M1.
                   15  INNF-M-01-M1-EDBNR-G.
                       20  INNF-M-01-M1-EDBNR PICTURE X(7).
           05  GMLINN-LEVEL-02.
               10  GMLINN-02-L1.
                   15  GMLINN-02-L1-FIRMA  PICTURE X(3).
           05  GMLINN-DATA-FIELDS.
               10  GMLREC                  PICTURE X(70).
           05  GMLINN-MP                   PICTURE X(10).
           05  GMLINN-MC                   PICTURE X(10).
           05  GMLINN-M-02             REDEFINES GMLINN-MC.
               10  GMLINN-M-02-M2.
                   15  GMLINN-M-02-M2-FIRMA-G.
                       20  GMLINN-M-02-M2-FIRMA PICTURE X(3).
               10  GMLINN-M-02-M1.
                   15  GMLINN-M-02-M1-EDBNR-G.
                       20  GMLINN-M-02-M1-EDBNR PICTURE X(7).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
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
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  INNF-PROCESS
               SET INNF-PROCESS-OFF        TO TRUE
               SET INNF-READ               TO TRUE
           END-IF
 
           IF  INNF-READ
               PERFORM INNF-GET
               SET INNF-READ-OFF           TO TRUE
               IF  NOT INNF-EOF
                   PERFORM INNF-MATCH-SET
               END-IF
           END-IF
 
           IF  GMLINN-PROCESS
               SET GMLINN-PROCESS-OFF      TO TRUE
               SET GMLINN-READ             TO TRUE
           END-IF
 
           IF  GMLINN-READ
               PERFORM GMLINN-GET
               SET GMLINN-READ-OFF         TO TRUE
               IF  NOT GMLINN-EOF
                   PERFORM GMLINN-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  INNF-PROCESS
               PERFORM INNF-IDSET
           END-IF
 
           IF  GMLINN-PROCESS
               PERFORM GMLINN-IDSET
           END-IF
 
           IF  INNF-PROCESS
               PERFORM INNF-CHK-LEVEL
           END-IF
 
           IF  GMLINN-PROCESS
               PERFORM GMLINN-CHK-LEVEL
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
 
           IF  INNF-PROCESS
               PERFORM INNF-FLDOFF
               PERFORM INNF-FLDSET
           END-IF
 
           IF  GMLINN-PROCESS
               PERFORM GMLINN-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  INNF-PROCESS
           OR  GMLINN-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-01 AND I-49)
               SUBTRACT ANTALL FROM ZERO GIVING ANTALL
           END-IF.
 
       INNF-GET SECTION.
       INNF-GET-P.
           IF  INNF-EOF-OFF
               READ INNF
               AT END
                   SET INNF-EOF            TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INNF-FLDOFF SECTION.
       INNF-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-51                TO TRUE
               SET NOT-I-49                TO TRUE
               SET NOT-I-50                TO TRUE
           END-EVALUATE.
 
       INNF-FLDSET SECTION.
       INNF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INNF-IO-AREA (12:4)    TO ANTALL-IO
               IF  ANTALL = ZERO
                   SET I-50                TO TRUE
               END-IF
               IF  ANTALL > ZERO
                   SET I-51                TO TRUE
               END-IF
               IF  ANTALL < ZERO
                   SET I-49                TO TRUE
               END-IF
               MOVE INNF-IO-AREA (16:7)    TO EDBNR (1:7)
               MOVE INNF-IO-AREA (51:3)    TO FIRMA (1:3)
           END-EVALUATE.
 
       INNF-IDSET SECTION.
       INNF-IDSET-P.
           SET I-01                        TO TRUE.
 
       INNF-CHK-LEVEL SECTION.
       INNF-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO INNF-LEVEL-01
               MOVE INNF-IO-AREA (51:3)    TO INNF-01-L1-FIRMA
               IF  INNF-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INNF-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INNF-01-L1            TO THE-PRIOR-L1
               SET INNF-LEVEL-INIT         TO TRUE
           END-EVALUATE.
 
       INNF-MATCH-SET SECTION.
       INNF-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE INNF-IO-AREA (51:3)    TO INNF-M-01-M2-FIRMA
               MOVE INNF-IO-AREA (16:7)    TO INNF-M-01-M1-EDBNR
           END-EVALUATE.
 
       GMLINN-GET SECTION.
       GMLINN-GET-P.
           IF  GMLINN-EOF-OFF
               READ GMLINN
               AT END
                   SET GMLINN-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       GMLINN-FLDSET SECTION.
       GMLINN-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE GMLINN-IO-AREA (1:70)  TO GMLREC (1:70)
               MOVE GMLINN-IO-AREA (16:7)  TO EDBNR (1:7)
               MOVE GMLINN-IO-AREA (51:3)  TO FIRMA (1:3)
           END-EVALUATE.
 
       GMLINN-IDSET SECTION.
       GMLINN-IDSET-P.
           SET I-02                        TO TRUE.
 
       GMLINN-CHK-LEVEL SECTION.
       GMLINN-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO GMLINN-LEVEL-02
               MOVE GMLINN-IO-AREA (51:3)  TO GMLINN-02-L1-FIRMA
               IF  GMLINN-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  GMLINN-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  GMLINN-02-L1          TO THE-PRIOR-L1
               SET GMLINN-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       GMLINN-MATCH-SET SECTION.
       GMLINN-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE GMLINN-IO-AREA (51:3)  TO GMLINN-M-02-M2-FIRMA
               MOVE GMLINN-IO-AREA (16:7)  TO GMLINN-M-02-M1-EDBNR
           END-EVALUATE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  INNF-EOF
               MOVE HIGH-VALUES            TO INNF-MC
                                              INNF-MP
           END-IF
           IF  GMLINN-EOF
               MOVE HIGH-VALUES            TO GMLINN-MC
                                              GMLINN-MP
           END-IF
           IF  INNF-MC < INNF-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  GMLINN-MC < GMLINN-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  INNF-MC < GMLINN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET INNF-PROCESS        TO TRUE
                   MOVE INNF-MC            TO INNF-MP
                   IF  INNF-MC = GMLINN-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  GMLINN-MC < INNF-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET GMLINN-PROCESS      TO TRUE
                   MOVE GMLINN-MC          TO GMLINN-MP
                   IF  GMLINN-MC = INNF-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  INNF-MC = GMLINN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET INNF-PROCESS        TO TRUE
                   MOVE INNF-MC            TO INNF-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND NOT-I-50)
               MOVE SPACES TO NYUT-IO-AREA
               INITIALIZE NYUT-IO-AREA
               MOVE '8'                    TO NYUT-IO-AREA (1:1)
               MOVE ANTALL                 TO XO-52P
               MOVE XO-52P-EF              TO NYUT-IO-AREA (12:4)
               MOVE EDBNR                  TO NYUT-IO-AREA (16:7)
               IF  (I-51)
                   MOVE '1'                TO NYUT-IO-AREA (41:1)
               END-IF
               MOVE FIRMA                  TO NYUT-IO-AREA (51:3)
               WRITE NYUT-IO-AREA
           END-IF
           IF  (I-02)
               MOVE SPACES TO NYUT-IO-AREA
               INITIALIZE NYUT-IO-AREA
               MOVE GMLREC                 TO NYUT-IO-AREA (1:70)
               WRITE NYUT-IO-AREA
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
           SET INNF-LEVEL-INIT             TO TRUE
           INITIALIZE INNF-DATA-FIELDS
           SET INNF-EOF-OFF                TO TRUE
           SET INNF-PROCESS                TO TRUE
           MOVE LOW-VALUES                 TO INNF-MC
                                              INNF-MP
           OPEN INPUT INNF
           SET GMLINN-LEVEL-INIT           TO TRUE
           INITIALIZE GMLINN-DATA-FIELDS
           SET GMLINN-EOF-OFF              TO TRUE
           SET GMLINN-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO GMLINN-MC
                                              GMLINN-MP
           OPEN INPUT GMLINN
           OPEN OUTPUT NYUT.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INNF
           CLOSE GMLINN
           CLOSE NYUT.
 
       SETOFF-I-L SECTION.
           SET NOT-I-L1                    TO TRUE.
           SET NOT-I-L2                    TO TRUE.
           SET NOT-I-L3                    TO TRUE.
           SET NOT-I-L4                    TO TRUE.
           SET NOT-I-L5                    TO TRUE.
           SET NOT-I-L6                    TO TRUE.
           SET NOT-I-L7                    TO TRUE.
           SET NOT-I-L8                    TO TRUE.
           SET NOT-I-L9                    TO TRUE.
 
       SETON-I-L9 SECTION.
           SET I-L9                        TO TRUE.
           PERFORM SETON-I-L8.
 
       SETON-I-L8 SECTION.
           SET I-L8                        TO TRUE.
           PERFORM SETON-I-L7.
 
       SETON-I-L7 SECTION.
           SET I-L7                        TO TRUE.
           PERFORM SETON-I-L6.
 
       SETON-I-L6 SECTION.
           SET I-L6                        TO TRUE.
           PERFORM SETON-I-L5.
 
       SETON-I-L5 SECTION.
           SET I-L5                        TO TRUE.
           PERFORM SETON-I-L4.
 
       SETON-I-L4 SECTION.
           SET I-L4                        TO TRUE.
           PERFORM SETON-I-L3.
 
       SETON-I-L3 SECTION.
           SET I-L3                        TO TRUE.
           PERFORM SETON-I-L2.
 
       SETON-I-L2 SECTION.
           SET I-L2                        TO TRUE.
           PERFORM SETON-I-L1.
 
       SETON-I-L1 SECTION.
           SET I-L1                        TO TRUE.
 
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
