       IDENTIFICATION DIVISION.
       PROGRAM-ID. NYB301R.
      **********************************************  Z-WIN-RPG2   ****
      *  UTPLUKK FRA BEST.MASTER- DANNE LAGER.LEVR.FILE              *
      *  ELIN NØSTERBERGET OKT.93,JUNI 94 CHAIN NYEBEST              *
      ****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: NYB301.rpg
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
           SELECT INFILE
               ASSIGN TO UT-S-INFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INFILE-STATUS.
           SELECT NYEBEST
               ASSIGN TO NYEBEST
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS NYEBEST-STATUS
               RECORD KEY IS NYEBEST-KEY1.
           SELECT VARETIL
               ASSIGN TO VARETIL
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VARETIL-STATUS
               RECORD KEY IS VARETIL-KEY1.
           SELECT UTFILE
               ASSIGN TO UT-S-UTFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS UTFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD INFILE
               BLOCK CONTAINS 6000
               RECORD CONTAINS 150.
       01  INFILE-IO-AREA.
           05  INFILE-IO-AREA-X            PICTURE X(150).
       FD NYEBEST
               RECORD CONTAINS 150.
       01  NYEBEST-IO-AREA.
           05  NYEBEST-IO-AREA-X.
               10  NYEBEST-KEY1            PICTURE X(16).
               10  FILLER                  PICTURE X(134).
       FD VARETIL
               RECORD CONTAINS 200.
       01  VARETIL-IO-AREA.
           05  VARETIL-IO-AREA-X.
               10  VARETIL-KEY1            PICTURE X(12).
               10  FILLER                  PICTURE X(188).
       FD UTFILE
               RECORD CONTAINS 80.
       01  UTFILE-IO-AREA.
           05  UTFILE-IO-AREA-X            PICTURE X(80).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  INFILE-STATUS               PICTURE 99 VALUE 0.
           10  NYEBEST-STATUS              PICTURE 99 VALUE 0.
           10  VARETIL-STATUS              PICTURE 99 VALUE 0.
           10  UTFILE-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  INFILE-EOF-OFF          VALUE '0'.
               88  INFILE-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INFILE-READ-OFF         VALUE '0'.
               88  INFILE-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INFILE-PROCESS-OFF      VALUE '0'.
               88  INFILE-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  INFILE-LEVEL-INIT-OFF   VALUE '0'.
               88  INFILE-LEVEL-INIT       VALUE '1'.
           05  NYEBEST-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  VARETIL-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  INFILE-LEVEL-02.
               10  INFILE-02-L3.
                   15  INFILE-02-L3-FIRMA  PICTURE X(3).
               10  INFILE-02-L2.
                   15  INFILE-02-L2-LEVNR  PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  INFILE-02-L1.
                   15  INFILE-02-L1-BESNR  PICTURE S9(5).
           05  INFILE-DATA-FIELDS.
               10  KEY1                    PICTURE X(10).
               10  FIRMA                   PICTURE X(3).
               10  BESNR-IO.
                   15  BESNR               PICTURE S9(5).
               10  POSNR-IO.
                   15  POSNR               PICTURE S9(4).
               10  LAAR                    PICTURE X(2).
               10  LUKE                    PICTURE X(2).
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  EDBNR-IO.
                   15  EDBNR               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  BANT-IO.
                   15  BANT                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  TANT-IO.
                   15  TANT                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  BESDAT-IO.
                   15  BESDAT              PICTURE S9(6).
               10  TILDAT-IO.
                   15  TILDAT              PICTURE S9(6).
               10  TEKST                   PICTURE X(1).
               10  LEVNR-IO.
                   15  LEVNR               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  BEKR                    PICTURE X(1).
               10  SLETT                   PICTURE X(1).
               10  RECART                  PICTURE X(1).
           05  NYEBEST-DATA-FIELDS.
               10  FABRIK                  PICTURE X(1).
           05  VARETIL-DATA-FIELDS.
               10  VARTNR                  PICTURE X(20).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(4).
               10  THE-PRIOR-L1            PICTURE X(5).
           05  TEMPORARY-FIELDS.
               10  SEQ-IO.
                   15  SEQ                 PICTURE S9(3).
               10  LEV-IO.
                   15  LEV                 PICTURE S9(11).
               10  LEV6-IO.
                   15  LEV6                PICTURE S9(6).
               10  KEY-X                   PICTURE X(16).
               10  KEY5                    PICTURE X(5).
               10  KEY12                   PICTURE X(12).
               10  EDBNR-N-IO.
                   15  EDBNR-N             PICTURE S9(7).
           05  EDITTING-FIELDS.
               10  XO-110P-EF.
                 15  XO-110P               PICTURE S9(11) USAGE
                                                       PACKED-DECIMAL.
               10  XO-70P-EF.
                 15  XO-70P                PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  XO-50P-EF.
                 15  XO-50P                PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  XO-40P-EF.
                 15  XO-40P                PICTURE S9(4) USAGE
                                                       PACKED-DECIMAL.
               10  XO-60P-EF.
                 15  XO-60P                PICTURE S9(6) USAGE
                                                       PACKED-DECIMAL.
               10  XO-72P-EF.
                 15  XO-72P                PICTURE S9(7)V9(2) USAGE
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
 
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-04                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  INFILE-PROCESS
               SET INFILE-PROCESS-OFF      TO TRUE
               SET INFILE-READ             TO TRUE
           END-IF
 
           IF  INFILE-READ
           AND RECORD-SELECTED-OFF
               PERFORM INFILE-GET
               SET INFILE-READ-OFF         TO TRUE
               IF  NOT INFILE-EOF
                   SET INFILE-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  INFILE-PROCESS
               PERFORM INFILE-IDSET
           END-IF
 
           IF  INFILE-PROCESS
               PERFORM INFILE-CHK-LEVEL
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
 
           IF  INFILE-PROCESS
               PERFORM INFILE-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  INFILE-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-L2)
               MOVE 0                      TO SEQ
               MOVE 00000                  TO LEV-IO (7:5)
               ADD LEVNR TO ZERO       GIVING LEV6
               MOVE LEV6                   TO LEV (1:6)
           END-IF
           IF  (I-L1)
               MOVE KEY1                   TO KEY-X (1:10)
               MOVE '000001'               TO KEY-X (11:6)
               MOVE KEY-X                  TO NYEBEST-KEY1
               READ NYEBEST RECORD KEY IS NYEBEST-KEY1
               INVALID KEY
                   SET I-11                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-11            TO TRUE
                   PERFORM NYEBEST-FLDSET
                   PERFORM NYEBEST-IDSET
               END-READ
           END-IF
           ADD 1                           TO SEQ
           SET NOT-I-35                    TO TRUE
           IF  SEQ = 999
               SET I-35                    TO TRUE
           END-IF
           IF  (I-35)
               ADD 1                       TO LEV
           END-IF
           SET NOT-I-10                    TO TRUE
           IF  TEKST = ' '
               SET I-10                    TO TRUE
           END-IF
           SET NOT-I-15                    TO TRUE
           IF  FIRMA = '913'
               SET I-15                    TO TRUE
           END-IF
           IF  (I-15)
               MOVE '80'                   TO KEY5 (1:2)
               MOVE FIRMA                  TO KEY5 (3:3)
               MOVE KEY5                   TO KEY12 (1:5)
               MOVE EDBNR                  TO EDBNR-N
               MOVE EDBNR-N-IO             TO KEY12 (6:7)
               MOVE KEY12                  TO VARETIL-KEY1
               READ VARETIL RECORD KEY IS VARETIL-KEY1
               INVALID KEY
                   SET I-12                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-12            TO TRUE
                   PERFORM VARETIL-FLDOFF
                   PERFORM VARETIL-FLDSET
                   PERFORM VARETIL-IDSET
               END-READ
           END-IF.
 
       INFILE-GET SECTION.
       INFILE-GET-P.
           IF  INFILE-EOF-OFF
               READ INFILE
               AT END
                   SET INFILE-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INFILE-FLDSET SECTION.
       INFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INFILE-IO-AREA (1:10)  TO KEY1 (1:10)
               MOVE INFILE-IO-AREA (2:3)   TO FIRMA (1:3)
               MOVE INFILE-IO-AREA (5:5)   TO BESNR-IO
               INSPECT BESNR-IO REPLACING ALL ' ' BY '0'
               MOVE INFILE-IO-AREA (11:4)  TO POSNR-IO
               INSPECT POSNR-IO REPLACING ALL ' ' BY '0'
               MOVE INFILE-IO-AREA (18:2)  TO LAAR (1:2)
               MOVE INFILE-IO-AREA (20:2)  TO LUKE (1:2)
               MOVE INFILE-IO-AREA (22:3)  TO ALFA (1:3)
               MOVE INFILE-IO-AREA (25:20) TO ARTNR (1:20)
               MOVE INFILE-IO-AREA (75:4)  TO EDBNR-IO
               MOVE INFILE-IO-AREA (89:5)  TO BANT-IO
               MOVE INFILE-IO-AREA (94:5)  TO TANT-IO
               MOVE INFILE-IO-AREA (104:6) TO BESDAT-IO
               INSPECT BESDAT-IO REPLACING ALL ' ' BY '0'
               MOVE INFILE-IO-AREA (110:6) TO TILDAT-IO
               INSPECT TILDAT-IO REPLACING ALL ' ' BY '0'
               MOVE INFILE-IO-AREA (124:1) TO TEKST (1:1)
               MOVE INFILE-IO-AREA (131:4) TO LEVNR-IO
               MOVE INFILE-IO-AREA (142:1) TO BEKR (1:1)
               MOVE INFILE-IO-AREA (149:1) TO SLETT (1:1)
               MOVE INFILE-IO-AREA (150:1) TO RECART (1:1)
           END-EVALUATE.
 
       INFILE-IDSET SECTION.
       INFILE-IDSET-P.
           SET I-02                        TO TRUE.
 
       INFILE-CHK-LEVEL SECTION.
       INFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO INFILE-LEVEL-02
               MOVE INFILE-IO-AREA (2:3)   TO INFILE-02-L3-FIRMA
               MOVE INFILE-IO-AREA (131:4) TO INFILE-02-L2-LEVNR
               MOVE INFILE-IO-AREA (5:5)   TO INFILE-02-L1-BESNR
               IF  INFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INFILE-02-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  INFILE-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INFILE-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INFILE-02-L3          TO THE-PRIOR-L3
               MOVE  INFILE-02-L2          TO THE-PRIOR-L2
               MOVE  INFILE-02-L1          TO THE-PRIOR-L1
               SET INFILE-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       NYEBEST-FLDSET SECTION.
       NYEBEST-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE NYEBEST-IO-AREA (106:1) TO FABRIK (1:1)
           END-EVALUATE.
 
       NYEBEST-IDSET SECTION.
       NYEBEST-IDSET-P.
           SET I-03                        TO TRUE.
 
       VARETIL-FLDOFF SECTION.
       VARETIL-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-25                TO TRUE
           END-EVALUATE.
 
       VARETIL-FLDSET SECTION.
       VARETIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VARETIL-IO-AREA (13:20) TO VARTNR (1:20)
               IF  VARTNR = SPACES
                   SET I-25                TO TRUE
               END-IF
           END-EVALUATE.
 
       VARETIL-IDSET SECTION.
       VARETIL-IDSET-P.
           SET I-04                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND I-10)
               MOVE SPACES TO UTFILE-IO-AREA
               INITIALIZE UTFILE-IO-AREA
               MOVE '4'                    TO UTFILE-IO-AREA (1:1)
               MOVE FIRMA                  TO UTFILE-IO-AREA (2:3)
               MOVE LEV                    TO XO-110P
               MOVE XO-110P-EF             TO UTFILE-IO-AREA (5:6)
               MOVE SEQ-IO                 TO UTFILE-IO-AREA (11:3)
               MOVE LEVNR                  TO XO-70P
               MOVE XO-70P-EF              TO UTFILE-IO-AREA (14:4)
               MOVE BESNR                  TO XO-50P
               MOVE XO-50P-EF              TO UTFILE-IO-AREA (18:3)
               MOVE POSNR                  TO XO-40P
               MOVE XO-40P-EF              TO UTFILE-IO-AREA (21:3)
               MOVE BESDAT                 TO XO-60P
               MOVE XO-60P-EF              TO UTFILE-IO-AREA (24:4)
               MOVE LAAR                   TO UTFILE-IO-AREA (28:2)
               MOVE LUKE                   TO UTFILE-IO-AREA (30:2)
               MOVE BANT                   TO XO-72P
               MOVE XO-72P-EF              TO UTFILE-IO-AREA (32:5)
               MOVE ALFA                   TO UTFILE-IO-AREA (37:3)
               MOVE ARTNR                  TO UTFILE-IO-AREA (40:20)
               IF  (I-15 AND NOT-I-25 AND NOT-I-12)
                   MOVE VARTNR             TO UTFILE-IO-AREA (40:20)
               END-IF
               MOVE TANT                   TO XO-72P
               MOVE XO-72P-EF              TO UTFILE-IO-AREA (60:5)
               MOVE TILDAT                 TO XO-60P
               MOVE XO-60P-EF              TO UTFILE-IO-AREA (65:4)
               MOVE EDBNR                  TO XO-70P
               MOVE XO-70P-EF              TO UTFILE-IO-AREA (69:4)
               MOVE FABRIK                 TO UTFILE-IO-AREA (73:1)
               MOVE BEKR                   TO UTFILE-IO-AREA (74:1)
               WRITE UTFILE-IO-AREA
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
           SET INFILE-LEVEL-INIT           TO TRUE
           INITIALIZE INFILE-DATA-FIELDS
           SET INFILE-EOF-OFF              TO TRUE
           SET INFILE-PROCESS              TO TRUE
           OPEN INPUT INFILE
           INITIALIZE NYEBEST-DATA-FIELDS
           OPEN INPUT NYEBEST
           INITIALIZE VARETIL-DATA-FIELDS
           OPEN INPUT VARETIL
           OPEN OUTPUT UTFILE.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INFILE
           CLOSE NYEBEST
           CLOSE VARETIL
           CLOSE UTFILE.
 
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
