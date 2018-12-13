       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORD866R.
      **********************************************  Z-WIN-RPG2   ****
      *       O R D R E R U T I N E P R O G R A M   O R D 8 6 6       *
      *       -------------------------------------------------       *
      *  2. DANNE RECORD SALG.TOTAL.FILE                              *
      *      05/4-2002 AV ESPEN LARSEN.                               *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ORD866.rpg
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
           SELECT SALGTOT
               ASSIGN TO SALGTOT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS SALGTOT-STATUS
               RECORD KEY IS SALGTOT-KEY1.
       DATA DIVISION.
       FILE SECTION.
       FD INNF
               BLOCK CONTAINS 42
               RECORD CONTAINS 21.
       01  INNF-IO-AREA.
           05  INNF-IO-AREA-X              PICTURE X(21).
       FD SALGTOT
               RECORD CONTAINS 30.
       01  SALGTOT-IO-AREA.
           05  SALGTOT-IO-AREA-X.
               10  SALGTOT-KEY1.
                   15  SALGTOT-KEY1N       PICTURE S9(9).
               10  FILLER                  PICTURE X(21).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  INNF-STATUS                 PICTURE 99 VALUE 0.
           10  SALGTOT-STATUS              PICTURE 99 VALUE 0.
 
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
           05  SALGTOT-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  INNF-LEVEL-01.
               10  INNF-01-L4.
                   15  INNF-01-L4-FIRMA    PICTURE X(3).
               10  INNF-01-L3.
                   15  INNF-01-L3-TOTTYP   PICTURE X(1).
               10  INNF-01-L2.
                   15  INNF-01-L2-PERTYP   PICTURE X(1).
               10  INNF-01-L1.
                   15  INNF-01-L1-PER      PICTURE X(4).
           05  INNF-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  TOTTYP                  PICTURE X(1).
               10  PERTYP                  PICTURE X(1).
               10  PER                     PICTURE X(4).
               10  SALDPA-ELG-IO.
                   15  SALDPA-ELG          PICTURE S9(9)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  SALDPF-IO.
                   15  SALDPF              PICTURE S9(9)V9(2) USAGE
                                                       PACKED-DECIMAL.
      *****************************************************************
      *                 H O V E D R U T I N E .                       *
      *****************************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L4            PICTURE X(3).
               10  THE-PRIOR-L3            PICTURE X(1).
               10  THE-PRIOR-L2            PICTURE X(1).
               10  THE-PRIOR-L1            PICTURE X(4).
           05  TEMPORARY-FIELDS.
               10  BELDPA-ELG-IO.
                   15  BELDPA-ELG          PICTURE S9(9)V9(2).
               10  BELDPF-IO.
                   15  BELDPF              PICTURE S9(9)V9(2).
               10  BELAKA-ELG-IO.
                   15  BELAKA-ELG          PICTURE S9(9)V9(2).
               10  BELAKF-IO.
                   15  BELAKF              PICTURE S9(9)V9(2).
               10  SUMDPA-ELG-IO.
                   15  SUMDPA-ELG          PICTURE S9(9).
               10  SUMDPF-IO.
                   15  SUMDPF              PICTURE S9(9).
               10  SUMAKA-ELG-IO.
                   15  SUMAKA-ELG          PICTURE S9(9).
               10  SUMAKF-IO.
                   15  SUMAKF              PICTURE S9(9).
           05  EDITTING-FIELDS.
               10  XO-90P-EF.
                 15  XO-90P                PICTURE S9(9) USAGE
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
 
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
 
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
           AND RECORD-SELECTED-OFF
               PERFORM INNF-GET
               SET INNF-READ-OFF           TO TRUE
               IF  NOT INNF-EOF
                   SET INNF-PROCESS        TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  INNF-PROCESS
               PERFORM INNF-IDSET
           END-IF
 
           IF  INNF-PROCESS
               PERFORM INNF-CHK-LEVEL
           END-IF
 
           IF I-1ST
               GO TO LR-INDICATOR-TEST
           END-IF.
 
       MAINLINE-TOTAL-CALCS.
           PERFORM TOTAL-CALCS
           PERFORM TOTAL-OUTPUT.
 
       LR-INDICATOR-TEST.
           IF  I-LR
               GO TO MAINLINE-TERMINATION
           END-IF
 
           IF  INNF-PROCESS
               PERFORM INNF-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  INNF-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-L1)
               SUBTRACT BELDPA-ELG         FROM BELDPA-ELG
               SUBTRACT BELDPF             FROM BELDPF
           END-IF
           IF  (I-01)
               ADD SALDPA-ELG              TO BELDPA-ELG
               ADD SALDPF                  TO BELDPF
      *
           END-IF
           IF  (I-L2)
               SUBTRACT BELAKA-ELG         FROM BELAKA-ELG
               SUBTRACT BELAKF             FROM BELAKF
           END-IF
           IF  (I-01)
               ADD SALDPA-ELG              TO BELAKA-ELG
               ADD SALDPF                  TO BELAKF
      *****************************************************************
      *                 T O T A L R U T I N E .                       *
      *****************************************************************
           END-IF
           .
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1)
               ADD BELDPA-ELG TO ZERO  GIVING SUMDPA-ELG ROUNDED
               ADD BELDPF TO ZERO      GIVING SUMDPF ROUNDED
      *
           END-IF
           IF  (I-L1)
               ADD BELAKA-ELG TO ZERO  GIVING SUMAKA-ELG ROUNDED
               ADD BELAKF TO ZERO      GIVING SUMAKF ROUNDED
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
 
       INNF-FLDSET SECTION.
       INNF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INNF-IO-AREA (1:3)     TO FIRMA (1:3)
               MOVE INNF-IO-AREA (4:1)     TO TOTTYP (1:1)
               MOVE INNF-IO-AREA (5:1)     TO PERTYP (1:1)
               MOVE INNF-IO-AREA (6:4)     TO PER (1:4)
               MOVE INNF-IO-AREA (10:6)    TO SALDPA-ELG-IO
               MOVE INNF-IO-AREA (16:6)    TO SALDPF-IO
           END-EVALUATE.
 
       INNF-IDSET SECTION.
       INNF-IDSET-P.
           SET I-01                        TO TRUE.
 
       INNF-CHK-LEVEL SECTION.
       INNF-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO INNF-LEVEL-01
               MOVE INNF-IO-AREA (1:3)     TO INNF-01-L4-FIRMA
               MOVE INNF-IO-AREA (4:1)     TO INNF-01-L3-TOTTYP
               MOVE INNF-IO-AREA (5:1)     TO INNF-01-L2-PERTYP
               MOVE INNF-IO-AREA (6:4)     TO INNF-01-L1-PER
               IF  INNF-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INNF-01-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   WHEN  INNF-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  INNF-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INNF-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INNF-01-L4            TO THE-PRIOR-L4
               MOVE  INNF-01-L3            TO THE-PRIOR-L3
               MOVE  INNF-01-L2            TO THE-PRIOR-L2
               MOVE  INNF-01-L1            TO THE-PRIOR-L1
               SET INNF-LEVEL-INIT         TO TRUE
           END-EVALUATE.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1)
               MOVE SPACES TO SALGTOT-IO-AREA
               INITIALIZE SALGTOT-IO-AREA
               MOVE FIRMA                  TO SALGTOT-IO-AREA (1:3)
               MOVE TOTTYP                 TO SALGTOT-IO-AREA (4:1)
               MOVE PERTYP                 TO SALGTOT-IO-AREA (5:1)
               MOVE PER                    TO SALGTOT-IO-AREA (6:4)
               MOVE SUMDPA-ELG             TO XO-90P
               MOVE XO-90P-EF              TO SALGTOT-IO-AREA (10:5)
               MOVE SUMDPF                 TO XO-90P
               MOVE XO-90P-EF              TO SALGTOT-IO-AREA (15:5)
               MOVE SUMAKA-ELG             TO XO-90P
               MOVE XO-90P-EF              TO SALGTOT-IO-AREA (20:5)
               MOVE SUMAKF                 TO XO-90P
               MOVE XO-90P-EF              TO SALGTOT-IO-AREA (25:5)
               WRITE SALGTOT-IO-AREA
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
           SET INNF-LEVEL-INIT             TO TRUE
           INITIALIZE INNF-DATA-FIELDS
           SET INNF-EOF-OFF                TO TRUE
           SET INNF-PROCESS                TO TRUE
           OPEN INPUT INNF
           OPEN OUTPUT SALGTOT.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INNF
           CLOSE SALGTOT.
 
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
