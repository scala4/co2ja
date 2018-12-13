       IDENTIFICATION DIVISION.
       PROGRAM-ID. NYB300R.
      **********************************************  Z-WIN-RPG2   ****
      *   PROGRAM  NYB300                                                      *
      *   UTLISTING AV VARETILGANGSLISTE MED MANKO                             *
      *   RECORDS FRA VARETILGANGSFILE                                         *
      **************************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: NYB300.rpg
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
           SELECT TILFILE
               ASSIGN TO UT-S-TILFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TILFILE-STATUS.
           SELECT VAREMAS
               ASSIGN TO VAREMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAREMAS-STATUS
               RECORD KEY IS VAREMAS-KEY1.
           SELECT OUTFIL
               ASSIGN TO UT-S-OUTFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTFIL-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD TILFILE
               BLOCK CONTAINS 4050
               RECORD CONTAINS 150.
       01  TILFILE-IO-AREA.
           05  TILFILE-IO-AREA-X           PICTURE X(150).
       FD VAREMAS
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X.
               10  VAREMAS-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       FD OUTFIL
               BLOCK CONTAINS 8000
               RECORD CONTAINS 80.
       01  OUTFIL-IO-AREA.
           05  OUTFIL-IO-AREA-X            PICTURE X(80).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  TILFILE-STATUS              PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  OUTFIL-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  TILFILE-EOF-OFF         VALUE '0'.
               88  TILFILE-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  TILFILE-READ-OFF        VALUE '0'.
               88  TILFILE-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  TILFILE-PROCESS-OFF     VALUE '0'.
               88  TILFILE-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  TILFILE-LEVEL-INIT-OFF  VALUE '0'.
               88  TILFILE-LEVEL-INIT      VALUE '1'.
           05  VAREMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  TILFILE-LEVEL-01.
               10  TILFILE-01-L3.
                   15  TILFILE-01-L3-FIRMA PICTURE X(3).
               10  TILFILE-01-L2.
                   15  TILFILE-01-L2-LEV   PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  TILFILE-01-L1.
                   15  TILFILE-01-L1-TILGNR PICTURE S9(5).
           05  TILFILE-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  BPRIS-IO.
                   15  BPRIS               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  LANT-IO.
                   15  LANT                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  EDBNR-IO.
                   15  EDBNR               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  TILGNR-IO.
                   15  TILGNR              PICTURE S9(5).
               10  PSEDD                   PICTURE X(6).
               10  PSED5                   PICTURE X(5).
               10  LEV-IO.
                   15  LEV                 PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  IPRIS-IO.
                   15  IPRIS               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
           05  VAREMAS-DATA-FIELDS.
               10  LEVP-IO.
                   15  LEVP                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  PRIST-IO.
                   15  PRIST               PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
      *
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(4).
               10  THE-PRIOR-L1            PICTURE X(5).
           05  TEMPORARY-FIELDS.
               10  SEQ-IO.
                   15  SEQ                 PICTURE S9(3).
               10  KEY1                    PICTURE X(10).
               10  EDB1-IO.
                   15  EDB1                PICTURE S9(7).
               10  LEV11-IO.
                   15  LEV11               PICTURE S9(11).
               10  LEV6-IO.
                   15  LEV6                PICTURE S9(6).
               10  SUMINN-IO.
                   15  SUMINN              PICTURE S9(8)V9(2).
               10  PANT-IO.
                   15  PANT                PICTURE S9(8)V9(2).
               10  SUMLEV-IO.
                   15  SUMLEV              PICTURE S9(8)V9(2).
               10  INNSUM-IO.
                   15  INNSUM              PICTURE S9(11)V9(2).
               10  LEVSUM-IO.
                   15  LEVSUM              PICTURE S9(11)V9(2).
               10  ORNR-IO.
                   15  ORNR                PICTURE S9(6).
               10  NULL6-IO.
                   15  NULL6               PICTURE S9(6).
               10  NULL10-IO.
                   15  NULL10              PICTURE S9(10).
               10  NULL11-IO.
                   15  NULL11              PICTURE S9(13).
               10  DAT-IO.
                   15  DAT                 PICTURE S9(4).
               10  DATO-IO.
                   15  DATO                PICTURE S9(6).
           05  EDITTING-FIELDS.
               10  XO-110P-EF.
                 15  XO-110P               PICTURE S9(11) USAGE
                                                       PACKED-DECIMAL.
               10  XO-100P-EF.
                 15  XO-100P               PICTURE S9(10) USAGE
                                                       PACKED-DECIMAL.
               10  XO-70P-EF.
                 15  XO-70P                PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  XO-60P-EF.
                 15  XO-60P                PICTURE S9(6) USAGE
                                                       PACKED-DECIMAL.
               10  XO-50P-EF.
                 15  XO-50P                PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  XO-112P-EF.
                 15  XO-112P               PICTURE S9(11)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-130P-EF.
                 15  XO-130P               PICTURE S9(13) USAGE
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
           SET NOT-I-04                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  TILFILE-PROCESS
               SET TILFILE-PROCESS-OFF     TO TRUE
               SET TILFILE-READ            TO TRUE
           END-IF
 
           IF  TILFILE-READ
           AND RECORD-SELECTED-OFF
               PERFORM TILFILE-GET
               SET TILFILE-READ-OFF        TO TRUE
               IF  NOT TILFILE-EOF
                   SET TILFILE-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  TILFILE-PROCESS
               PERFORM TILFILE-IDSET
           END-IF
 
           IF  TILFILE-PROCESS
               PERFORM TILFILE-CHK-LEVEL
           END-IF
 
           IF I-1ST
               GO TO LR-INDICATOR-TEST
           END-IF.
 
       MAINLINE-TOTAL-CALCS.
           PERFORM TOTAL-OUTPUT.
 
       LR-INDICATOR-TEST.
           IF  I-LR
               GO TO MAINLINE-TERMINATION
           END-IF
 
           IF  TILFILE-PROCESS
               PERFORM TILFILE-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  TILFILE-PROCESS
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
           END-IF
           IF  (I-L1)
               ADD 1                       TO SEQ
           END-IF
           MOVE FIRMA                      TO KEY1 (1:3)
           ADD EDBNR TO ZERO           GIVING EDB1
           MOVE EDB1                       TO KEY1 (4:7)
           MOVE KEY1                       TO VAREMAS-KEY1
           READ VAREMAS RECORD KEY IS VAREMAS-KEY1
           INVALID KEY
               SET I-71                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-71                TO TRUE
               PERFORM VAREMAS-FLDSET
               PERFORM VAREMAS-IDSET
           END-READ
           MOVE 00000                      TO LEV11-IO (7:5)
           MOVE PSED5                      TO LEV11-IO (7:5)
           ADD LEV TO ZERO             GIVING LEV6
           MOVE LEV6                       TO LEV11 (1:6)
           MULTIPLY IPRIS BY LANT      GIVING SUMINN
           MULTIPLY PRIST BY LANT      GIVING PANT
           IF  (NOT-I-71)
               MULTIPLY LEVP BY LANT   GIVING SUMLEV
           END-IF
           IF  (I-71)
               MULTIPLY BPRIS BY LANT  GIVING SUMLEV
           END-IF
           ADD SUMINN                      TO INNSUM
           ADD PANT                        TO INNSUM
           ADD SUMLEV                      TO LEVSUM
           ADD PANT                        TO LEVSUM
           SET NOT-I-50                    TO TRUE
           IF  PSEDD = '      '
               SET I-50                    TO TRUE
           END-IF
           IF  (I-50)
               MOVE 0                      TO ORNR
           END-IF
           IF  (NOT-I-50)
               MOVE PSEDD                  TO ORNR-IO
           END-IF
           MOVE 0                          TO NULL6
           MOVE 0                          TO NULL10
           MOVE 0                          TO NULL11
           MOVE UYEAR                      TO DAT (1:2)
           MOVE UMONTH                     TO DAT-IO (3:2)
           MOVE UDAY                       TO DATO-IO (5:2)
           MOVE DAT                        TO DATO (1:4).
 
       TILFILE-GET SECTION.
       TILFILE-GET-P.
           IF  TILFILE-EOF-OFF
               READ TILFILE
               AT END
                   SET TILFILE-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       TILFILE-FLDSET SECTION.
       TILFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE TILFILE-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE TILFILE-IO-AREA (79:5) TO BPRIS-IO
               MOVE TILFILE-IO-AREA (99:5) TO LANT-IO
               MOVE TILFILE-IO-AREA (75:4) TO EDBNR-IO
               MOVE TILFILE-IO-AREA (135:5) TO TILGNR-IO
               INSPECT TILGNR-IO REPLACING ALL ' ' BY '0'
               MOVE TILFILE-IO-AREA (104:6) TO PSEDD (1:6)
               MOVE TILFILE-IO-AREA (105:5) TO PSED5 (1:5)
               MOVE TILFILE-IO-AREA (131:4) TO LEV-IO
               MOVE TILFILE-IO-AREA (143:5) TO IPRIS-IO
           END-EVALUATE.
 
       TILFILE-IDSET SECTION.
       TILFILE-IDSET-P.
           SET I-01                        TO TRUE.
 
       TILFILE-CHK-LEVEL SECTION.
       TILFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO TILFILE-LEVEL-01
               MOVE TILFILE-IO-AREA (2:3)  TO TILFILE-01-L3-FIRMA
               MOVE TILFILE-IO-AREA (131:4) TO TILFILE-01-L2-LEV
               MOVE TILFILE-IO-AREA (135:5) TO TILFILE-01-L1-TILGNR
               IF  TILFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  TILFILE-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  TILFILE-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  TILFILE-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  TILFILE-01-L3         TO THE-PRIOR-L3
               MOVE  TILFILE-01-L2         TO THE-PRIOR-L2
               MOVE  TILFILE-01-L1         TO THE-PRIOR-L1
               SET TILFILE-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (165:5) TO LEVP-IO
               MOVE VAREMAS-IO-AREA (161:4) TO PRIST-IO
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-04                        TO TRUE.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1)
               MOVE SPACES TO OUTFIL-IO-AREA
               INITIALIZE OUTFIL-IO-AREA
               MOVE '1'                    TO OUTFIL-IO-AREA (1:1)
               MOVE FIRMA                  TO OUTFIL-IO-AREA (2:3)
               MOVE LEV11                  TO XO-110P
               MOVE XO-110P-EF             TO OUTFIL-IO-AREA (5:6)
               MOVE SEQ-IO                 TO OUTFIL-IO-AREA (11:3)
               MOVE NULL10                 TO XO-100P
               MOVE XO-100P-EF             TO OUTFIL-IO-AREA (14:6)
               MOVE LEV                    TO XO-70P
               MOVE XO-70P-EF              TO OUTFIL-IO-AREA (20:4)
               MOVE ORNR                   TO XO-60P
               MOVE XO-60P-EF              TO OUTFIL-IO-AREA (24:4)
               INITIALIZE ORNR
               MOVE TILGNR                 TO XO-50P
               MOVE XO-50P-EF              TO OUTFIL-IO-AREA (28:3)
               MOVE DATO                   TO XO-60P
               MOVE XO-60P-EF              TO OUTFIL-IO-AREA (31:4)
               MOVE INNSUM                 TO XO-112P
               MOVE XO-112P-EF             TO OUTFIL-IO-AREA (35:7)
               INITIALIZE INNSUM
               MOVE LEVSUM                 TO XO-112P
               MOVE XO-112P-EF             TO OUTFIL-IO-AREA (42:7)
               INITIALIZE LEVSUM
               MOVE NULL6                  TO XO-60P
               MOVE XO-60P-EF              TO OUTFIL-IO-AREA (49:4)
               MOVE NULL11                 TO XO-130P
               MOVE XO-130P-EF             TO OUTFIL-IO-AREA (53:7)
               WRITE OUTFIL-IO-AREA
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
           SET TILFILE-LEVEL-INIT          TO TRUE
           INITIALIZE TILFILE-DATA-FIELDS
           SET TILFILE-EOF-OFF             TO TRUE
           SET TILFILE-PROCESS             TO TRUE
           OPEN INPUT TILFILE
           INITIALIZE VAREMAS-DATA-FIELDS
           OPEN INPUT VAREMAS
           OPEN OUTPUT OUTFIL.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE TILFILE
           CLOSE VAREMAS
           CLOSE OUTFIL.
 
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
