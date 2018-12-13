       IDENTIFICATION DIVISION.
       PROGRAM-ID. VAK006R.
      ******************************************* :   Z-WIN-RPG2P    **
      *  DANNE FIL PÅ PUN FRA TEANS=VARS                              *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VAK006.rpg
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
           SELECT INNFIL
               ASSIGN TO UT-S-INNFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INNFIL-STATUS.
           SELECT KUNDEMA
               ASSIGN TO KUNDEMA
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMA-STATUS
               RECORD KEY IS KUNDEMA-KEY1.
           SELECT VAREMAS
               ASSIGN TO VAREMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAREMAS-STATUS
               RECORD KEY IS VAREMAS-KEY1.
           SELECT OUTPUN
               ASSIGN TO UT-S-OUTPUN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTPUN-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD INNFIL
               BLOCK CONTAINS 2500
               RECORD CONTAINS 25.
       01  INNFIL-IO-AREA.
           05  INNFIL-IO-AREA-X            PICTURE X(25).
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1            PICTURE X(9).
               10  FILLER                  PICTURE X(190).
       FD VAREMAS
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X.
               10  VAREMAS-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       FD OUTPUN
               BLOCK CONTAINS 128
               RECORD CONTAINS 128.
       01  OUTPUN-IO-AREA.
           05  OUTPUN-IO-AREA-X            PICTURE X(128).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  INNFIL-STATUS               PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  OUTPUN-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  INNFIL-EOF-OFF          VALUE '0'.
               88  INNFIL-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNFIL-READ-OFF         VALUE '0'.
               88  INNFIL-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNFIL-PROCESS-OFF      VALUE '0'.
               88  INNFIL-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  INNFIL-LEVEL-INIT-OFF   VALUE '0'.
               88  INNFIL-LEVEL-INIT       VALUE '1'.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  VAREMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  INNFIL-LEVEL-01.
               10  INNFIL-01-L3.
                   15  INNFIL-01-L3-FIRM   PICTURE X(3).
               10  INNFIL-01-L2.
                   15  INNFIL-01-L2-KUND   PICTURE X(6).
               10  INNFIL-01-L1.
                   15  INNFIL-01-L1-EDBNR  PICTURE X(7).
           05  INNFIL-DATA-FIELDS.
               10  FIRM                    PICTURE X(3).
               10  KUND                    PICTURE X(6).
               10  EDBNR                   PICTURE X(7).
               10  ANT-IO.
                   15  ANT                 PICTURE S9(9)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  REC                     PICTURE X(25).
           05  KUNDEMA-DATA-FIELDS.
               10  KNAVN                   PICTURE X(30).
           05  VAREMAS-DATA-FIELDS.
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  BETEG                   PICTURE X(30).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(6).
               10  THE-PRIOR-L1            PICTURE X(7).
           05  TEMPORARY-FIELDS.
               10  KKEY                    PICTURE X(9).
               10  KEY-X                   PICTURE X(10).
           05  EDITTING-FIELDS.
               10  XO-92YY9R               PICTURE ZZZ.ZZZ.ZZZ,99-.
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
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  INNFIL-PROCESS
               SET INNFIL-PROCESS-OFF      TO TRUE
               SET INNFIL-READ             TO TRUE
           END-IF
 
           IF  INNFIL-READ
           AND RECORD-SELECTED-OFF
               PERFORM INNFIL-GET
               SET INNFIL-READ-OFF         TO TRUE
               IF  NOT INNFIL-EOF
                   SET INNFIL-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  INNFIL-PROCESS
               PERFORM INNFIL-IDSET
           END-IF
 
           IF  INNFIL-PROCESS
               PERFORM INNFIL-CHK-LEVEL
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
 
           IF  INNFIL-PROCESS
               PERFORM INNFIL-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  INNFIL-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-L3)
               MOVE FIRM                   TO KKEY (1:3)
           END-IF
           IF  (I-L2)
               MOVE KUND                   TO KKEY (4:6)
               MOVE KKEY                   TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-10                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-10            TO TRUE
                   PERFORM KUNDEMA-FLDSET
                   PERFORM KUNDEMA-IDSET
               END-READ
           END-IF
           IF  (I-L3)
               MOVE FIRM                   TO KEY-X (1:3)
           END-IF
           IF  (I-L1)
               MOVE EDBNR                  TO KEY-X (4:7)
               MOVE KEY-X                  TO VAREMAS-KEY1
               READ VAREMAS RECORD KEY IS VAREMAS-KEY1
               INVALID KEY
                   SET I-11                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-11            TO TRUE
                   PERFORM VAREMAS-FLDSET
                   PERFORM VAREMAS-IDSET
               END-READ
           END-IF.
 
       INNFIL-GET SECTION.
       INNFIL-GET-P.
           IF  INNFIL-EOF-OFF
               READ INNFIL
               AT END
                   SET INNFIL-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INNFIL-FLDSET SECTION.
       INNFIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INNFIL-IO-AREA (1:3)   TO FIRM (1:3)
               MOVE INNFIL-IO-AREA (4:6)   TO KUND (1:6)
               MOVE INNFIL-IO-AREA (13:7)  TO EDBNR (1:7)
               MOVE INNFIL-IO-AREA (20:6)  TO ANT-IO
               MOVE INNFIL-IO-AREA (1:25)  TO REC (1:25)
           END-EVALUATE.
 
       INNFIL-IDSET SECTION.
       INNFIL-IDSET-P.
           SET I-01                        TO TRUE.
 
       INNFIL-CHK-LEVEL SECTION.
       INNFIL-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO INNFIL-LEVEL-01
               MOVE INNFIL-IO-AREA (1:3)   TO INNFIL-01-L3-FIRM
               MOVE INNFIL-IO-AREA (4:6)   TO INNFIL-01-L2-KUND
               MOVE INNFIL-IO-AREA (13:7)  TO INNFIL-01-L1-EDBNR
               IF  INNFIL-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INNFIL-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  INNFIL-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INNFIL-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INNFIL-01-L3          TO THE-PRIOR-L3
               MOVE  INNFIL-01-L2          TO THE-PRIOR-L2
               MOVE  INNFIL-01-L1          TO THE-PRIOR-L1
               SET INNFIL-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (16:30) TO KNAVN (1:30)
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-02                        TO TRUE.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (13:3) TO ALFA (1:3)
               MOVE VAREMAS-IO-AREA (16:20) TO ARTNR (1:20)
               MOVE VAREMAS-IO-AREA (36:30) TO BETEG (1:30)
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-03                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               MOVE SPACES TO OUTPUN-IO-AREA
               INITIALIZE OUTPUN-IO-AREA
               MOVE KUND                   TO OUTPUN-IO-AREA (1:6)
               MOVE ';'                    TO OUTPUN-IO-AREA (7:1)
               IF  (NOT-I-10)
                   MOVE KNAVN              TO OUTPUN-IO-AREA (8:30)
               END-IF
               MOVE ';'                    TO OUTPUN-IO-AREA (38:1)
               IF  (NOT-I-11)
                   MOVE ALFA               TO OUTPUN-IO-AREA (39:3)
               END-IF
               MOVE ';'                    TO OUTPUN-IO-AREA (42:1)
               IF  (NOT-I-11)
                   MOVE ARTNR              TO OUTPUN-IO-AREA (43:20)
               END-IF
               MOVE ';'                    TO OUTPUN-IO-AREA (63:1)
               MOVE ANT                    TO XO-92YY9R
               MOVE XO-92YY9R              TO OUTPUN-IO-AREA (65:15)
               MOVE ';'                    TO OUTPUN-IO-AREA (80:1)
               MOVE EDBNR                  TO OUTPUN-IO-AREA (81:7)
               MOVE ';'                    TO OUTPUN-IO-AREA (88:1)
               IF  (NOT-I-11)
                   MOVE BETEG              TO OUTPUN-IO-AREA (89:30)
               END-IF
               MOVE ';'                    TO OUTPUN-IO-AREA (119:1)
               WRITE OUTPUN-IO-AREA
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
           SET INNFIL-LEVEL-INIT           TO TRUE
           INITIALIZE INNFIL-DATA-FIELDS
           SET INNFIL-EOF-OFF              TO TRUE
           SET INNFIL-PROCESS              TO TRUE
           OPEN INPUT INNFIL
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           INITIALIZE VAREMAS-DATA-FIELDS
           OPEN INPUT VAREMAS
           OPEN OUTPUT OUTPUN.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INNFIL
           CLOSE KUNDEMA
           CLOSE VAREMAS
           CLOSE OUTPUN.
 
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
