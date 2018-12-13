       IDENTIFICATION DIVISION.
       PROGRAM-ID. NYB140R.
      **********************************************  Z-WIN-RPG2   ****
      *   PROGRAM  NYB140                                                     *
      *   UTLISTING AV BESTILLINGer til ipc foma                              *
      *************************************************************************
      *                                          XX2000XXOKXXSS
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: NYB140.rpg
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
           SELECT BESTM
               ASSIGN TO UT-S-BESTM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS BESTM-STATUS.
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
           SELECT OUTFILE
               ASSIGN TO UT-S-OUTFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD BESTM
               BLOCK CONTAINS 4096
               RECORD CONTAINS 128.
       01  BESTM-IO-AREA.
           05  BESTM-IO-AREA-X             PICTURE X(128).
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
      **************************************************************
       FD OUTFILE
               BLOCK CONTAINS 1900
               RECORD CONTAINS 190.
       01  OUTFILE-IO-AREA.
           05  OUTFILE-IO-AREA-X           PICTURE X(190).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  BESTM-STATUS                PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  OUTFILE-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  BESTM-EOF-OFF           VALUE '0'.
               88  BESTM-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BESTM-READ-OFF          VALUE '0'.
               88  BESTM-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BESTM-PROCESS-OFF       VALUE '0'.
               88  BESTM-PROCESS           VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  BESTM-LEVEL-INIT-OFF    VALUE '0'.
               88  BESTM-LEVEL-INIT        VALUE '1'.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  VAREMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  BESTM-LEVEL-01.
               10  BESTM-01-L3.
                   15  BESTM-01-L3-FIRMA   PICTURE X(3).
               10  BESTM-01-L2.
                   15  BESTM-01-L2-BEST    PICTURE X(5).
               10  BESTM-01-L1.
                   15  BESTM-01-L1-LEVR    PICTURE X(6).
           05  BESTM-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  BEST                    PICTURE X(5).
               10  POS-X                   PICTURE X(4).
               10  LEVR                    PICTURE X(6).
               10  LEVA-ELGR               PICTURE X(2).
               10  LEVUKE                  PICTURE X(2).
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  VNAVN                   PICTURE X(30).
               10  EDBNR-IO.
                   15  EDBNR               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  RAB1-IO.
                   15  RAB1                PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  RAB2-IO.
                   15  RAB2                PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  VT                      PICTURE X(1).
               10  BANT-IO.
                   15  BANT                PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
           05  KUNDEMA-DATA-FIELDS.
               10  NAVN1                   PICTURE X(30).
           05  VAREMAS-DATA-FIELDS.
               10  SKOST-IO.
                   15  SKOST               PICTURE S9(7)V9(2).
               10  LPRIS-IO.
                   15  LPRIS               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(5).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  LEVKEY                  PICTURE X(9).
               10  VARKEY                  PICTURE X(10).
               10  EDBNR-N-IO.
                   15  EDBNR-N             PICTURE S9(7).
               10  LPRI-IO.
                   15  LPRI                PICTURE S9(7)V9(2).
               10  LPRIE-IO.
                   15  LPRIE               PICTURE S9(5)V9(4).
               10  SUM-X-IO.
                   15  SUM-X               PICTURE S9(7)V9(2).
               10  BESTOT-IO.
                   15  BESTOT              PICTURE S9(7)V9(2).
               10  HJLP2-IO.
                   15  HJLP2               PICTURE S9(7)V9(2).
               10  HJLP1-IO.
                   15  HJLP1               PICTURE S9(8)V9(2).
           05  EDITTING-FIELDS.
               10  EDIT-BANT               PICTURE ZZZZZZ9.
               10  EDIT-LPRI               PICTURE ZZZZZZ9,99.
               10  EDIT-LPRIE              PICTURE ZZZZ9,9999.
               10  EDIT-RAB1               PICTURE Z9,9.
               10  EDIT-SKOST              PICTURE ZZZZZZ9,99.
               10  EDIT-BESTOT             PICTURE ZZZZZZ9,99.
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
 
           PERFORM HEADING-OUTPUT
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
           PERFORM HEADING-OUTPUT
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
           IF  BESTM-PROCESS
               SET BESTM-PROCESS-OFF       TO TRUE
               SET BESTM-READ              TO TRUE
           END-IF
 
           IF  BESTM-READ
           AND RECORD-SELECTED-OFF
               PERFORM BESTM-GET
               SET BESTM-READ-OFF          TO TRUE
               IF  NOT BESTM-EOF
                   SET BESTM-PROCESS       TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  BESTM-PROCESS
               PERFORM BESTM-IDSET
           END-IF
 
           IF  BESTM-PROCESS
               PERFORM BESTM-CHK-LEVEL
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
 
           IF  BESTM-PROCESS
               PERFORM BESTM-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  BESTM-PROCESS
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
               MOVE FIRMA                  TO LEVKEY (1:3)
               MOVE FIRMA                  TO VARKEY (1:3)
           END-IF
           IF  (I-L1)
               MOVE LEVR                   TO LEVKEY (4:6)
               MOVE LEVKEY                 TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-20                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-20            TO TRUE
                   PERFORM KUNDEMA-FLDSET
                   PERFORM KUNDEMA-IDSET
               END-READ
      *                    Z-ADDEDBNR     EDB     70
           END-IF
           MOVE EDBNR                      TO EDBNR-N
           MOVE EDBNR-N-IO                 TO VARKEY (4:7)
           MOVE VARKEY                     TO VAREMAS-KEY1
           READ VAREMAS RECORD KEY IS VAREMAS-KEY1
           INVALID KEY
               SET I-21                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-21                TO TRUE
               PERFORM VAREMAS-FLDSET
               PERFORM VAREMAS-IDSET
           END-READ
           SET NOT-I-31                    TO TRUE
           IF  VT = 'L'
               SET I-31                    TO TRUE
           END-IF
           SET NOT-I-30                    TO TRUE
           IF  VT = '0'
               SET I-30                    TO TRUE
           END-IF
           IF  (NOT-I-21 AND I-30)
               ADD LPRIS TO ZERO       GIVING LPRI
           END-IF
           IF  (NOT-I-21 AND I-31)
               ADD LPRIS TO ZERO       GIVING LPRIE
           END-IF
           IF  (NOT-I-21)
               MULTIPLY SKOST BY BANT  GIVING SUM-X
               ADD SUM-X                   TO BESTOT
               MOVE 0                      TO HJLP2
               SET NOT-I-35                TO TRUE
               IF  RAB1 = 0
                   SET I-35                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-21 AND NOT-I-35)
               MULTIPLY RAB1 BY BESTOT GIVING HJLP1
               DIVIDE HJLP1 BY 100     GIVING HJLP2
           END-IF
           IF  (NOT-I-21)
               SUBTRACT HJLP2              FROM BESTOT
           END-IF.
 
       BESTM-GET SECTION.
       BESTM-GET-P.
           IF  BESTM-EOF-OFF
               READ BESTM
               AT END
                   SET BESTM-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       BESTM-FLDSET SECTION.
       BESTM-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE BESTM-IO-AREA (2:3)    TO FIRMA (1:3)
               MOVE BESTM-IO-AREA (5:5)    TO BEST (1:5)
               MOVE BESTM-IO-AREA (10:4)   TO POS-X (1:4)
               MOVE BESTM-IO-AREA (15:6)   TO LEVR (1:6)
               MOVE BESTM-IO-AREA (23:2)   TO LEVA-ELGR (1:2)
               MOVE BESTM-IO-AREA (25:2)   TO LEVUKE (1:2)
               MOVE BESTM-IO-AREA (33:3)   TO ALFA (1:3)
               MOVE BESTM-IO-AREA (36:20)  TO ARTNR (1:20)
               MOVE BESTM-IO-AREA (57:30)  TO VNAVN (1:30)
               MOVE BESTM-IO-AREA (87:4)   TO EDBNR-IO
               MOVE BESTM-IO-AREA (91:2)   TO RAB1-IO
               MOVE BESTM-IO-AREA (93:2)   TO RAB2-IO
               MOVE BESTM-IO-AREA (95:1)   TO VT (1:1)
               MOVE BESTM-IO-AREA (101:4)  TO BANT-IO
           END-EVALUATE.
 
       BESTM-IDSET SECTION.
       BESTM-IDSET-P.
           SET I-01                        TO TRUE.
 
       BESTM-CHK-LEVEL SECTION.
       BESTM-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO BESTM-LEVEL-01
               MOVE BESTM-IO-AREA (2:3)    TO BESTM-01-L3-FIRMA
               MOVE BESTM-IO-AREA (5:5)    TO BESTM-01-L2-BEST
               MOVE BESTM-IO-AREA (15:6)   TO BESTM-01-L1-LEVR
               IF  BESTM-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  BESTM-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  BESTM-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  BESTM-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  BESTM-01-L3           TO THE-PRIOR-L3
               MOVE  BESTM-01-L2           TO THE-PRIOR-L2
               MOVE  BESTM-01-L1           TO THE-PRIOR-L1
               SET BESTM-LEVEL-INIT        TO TRUE
           END-EVALUATE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (16:30) TO NAVN1 (1:30)
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-02                        TO TRUE.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (66:9) TO SKOST-IO
               INSPECT SKOST-IO REPLACING ALL ' ' BY '0'
               MOVE VAREMAS-IO-AREA (165:5) TO LPRIS-IO
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-03                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               MOVE SPACES TO OUTFILE-IO-AREA
               INITIALIZE OUTFILE-IO-AREA
               MOVE BEST                   TO OUTFILE-IO-AREA (1:5)
               MOVE ';PO;PUR-PURCHASING ORDER' TO OUTFILE-IO-AREA
                                                                (6:24)
               MOVE ';'                    TO OUTFILE-IO-AREA (30:1)
               MOVE LEVUKE                 TO OUTFILE-IO-AREA (31:2)
               MOVE ' '                    TO OUTFILE-IO-AREA (33:1)
               MOVE LEVA-ELGR              TO OUTFILE-IO-AREA (34:2)
               MOVE ';'                    TO OUTFILE-IO-AREA (36:1)
               MOVE POS-X                  TO OUTFILE-IO-AREA (37:4)
               MOVE ';'                    TO OUTFILE-IO-AREA (41:1)
               MOVE LEVR                   TO OUTFILE-IO-AREA (42:6)
               MOVE ';'                    TO OUTFILE-IO-AREA (48:1)
               MOVE ARTNR                  TO OUTFILE-IO-AREA (49:20)
               MOVE ';'                    TO OUTFILE-IO-AREA (69:1)
               MOVE VNAVN                  TO OUTFILE-IO-AREA (70:30)
               MOVE ';NR;'                 TO OUTFILE-IO-AREA (100:4)
               MOVE BANT                   TO EDIT-BANT
               MOVE EDIT-BANT              TO OUTFILE-IO-AREA (106:7)
               MOVE ';'                    TO OUTFILE-IO-AREA (113:1)
               IF  (I-31)
                   MOVE 'EUR'              TO OUTFILE-IO-AREA (114:3)
               END-IF
               IF  (I-30)
                   MOVE 'NOK'              TO OUTFILE-IO-AREA (114:3)
               END-IF
               MOVE ';'                    TO OUTFILE-IO-AREA (117:1)
               IF  (I-30)
                   MOVE LPRI               TO EDIT-LPRI
                   MOVE EDIT-LPRI          TO OUTFILE-IO-AREA (118:10)
                   INITIALIZE LPRI
               END-IF
               IF  (I-31)
                   MOVE LPRIE              TO EDIT-LPRIE
                   MOVE EDIT-LPRIE         TO OUTFILE-IO-AREA (118:10)
                   INITIALIZE LPRIE
               END-IF
               MOVE ';'                    TO OUTFILE-IO-AREA (128:1)
               MOVE RAB1                   TO EDIT-RAB1
               MOVE EDIT-RAB1              TO OUTFILE-IO-AREA (129:4)
               MOVE ';'                    TO OUTFILE-IO-AREA (138:1)
               MOVE SKOST                  TO EDIT-SKOST
               MOVE EDIT-SKOST             TO OUTFILE-IO-AREA (139:10)
               INITIALIZE SKOST
               MOVE ';'                    TO OUTFILE-IO-AREA (149:1)
               MOVE BESTOT                 TO EDIT-BESTOT
               MOVE EDIT-BESTOT            TO OUTFILE-IO-AREA (150:10)
               INITIALIZE BESTOT
               MOVE ';'                    TO OUTFILE-IO-AREA (160:1)
               MOVE NAVN1                  TO OUTFILE-IO-AREA (161:30)
               WRITE OUTFILE-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P)
               MOVE SPACES TO OUTFILE-IO-AREA
               INITIALIZE OUTFILE-IO-AREA
               MOVE 'DOCUMENT NUMBER;TYPE;' TO OUTFILE-IO-AREA (1:21)
               MOVE 'MOVEMENT TYPE;'       TO OUTFILE-IO-AREA (22:14)
               MOVE 'DELIVERY DATE; ROW;'  TO OUTFILE-IO-AREA (36:19)
               MOVE 'SUPPLIER;PART NUMBER;' TO OUTFILE-IO-AREA (55:21)
               MOVE '   ITEM DESCRIPTION;' TO OUTFILE-IO-AREA (76:20)
               MOVE 'UM;QUANTITY;CURRENCY;' TO OUTFILE-IO-AREA (96:21)
               MOVE 'UNITARY PRICE;DISCOUNT;' TO OUTFILE-IO-AREA
                                                              (117:23)
               MOVE 'NET UNITARY CONDITION;' TO OUTFILE-IO-AREA
                                                              (140:22)
               MOVE 'NET PURCHASING VALUE' TO OUTFILE-IO-AREA (161:20)
               WRITE OUTFILE-IO-AREA
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
           SET BESTM-LEVEL-INIT            TO TRUE
           INITIALIZE BESTM-DATA-FIELDS
           SET BESTM-EOF-OFF               TO TRUE
           SET BESTM-PROCESS               TO TRUE
           OPEN INPUT BESTM
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           INITIALIZE VAREMAS-DATA-FIELDS
           OPEN INPUT VAREMAS
           OPEN OUTPUT OUTFILE.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE BESTM
           CLOSE KUNDEMA
           CLOSE VAREMAS
           CLOSE OUTFILE.
 
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
