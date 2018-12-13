       IDENTIFICATION DIVISION.
       PROGRAM-ID. KAM260R.
      **********************************************  Z-WIN-RPG2   ****
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: KAM260.rpg
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
           SELECT KAMMINN
               ASSIGN TO UT-S-KAMMINN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KAMMINN-STATUS.
           SELECT KAMMAST
               ASSIGN TO KAMMAST
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KAMMAST-STATUS
               RECORD KEY IS KAMMAST-KEY1.
           SELECT OUTPUT-X
               ASSIGN TO UT-S-OUTPUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTPUT-X-STATUS.
           SELECT OUTPUN
               ASSIGN TO UT-S-OUTPUN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTPUN-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD KAMMINN
               BLOCK CONTAINS 8000
               RECORD CONTAINS 40.
       01  KAMMINN-IO-AREA.
           05  KAMMINN-IO-AREA-X           PICTURE X(40).
       FD KAMMAST
               RECORD CONTAINS 40.
       01  KAMMAST-IO-AREA.
           05  KAMMAST-IO-AREA-X.
               10  KAMMAST-KEY1            PICTURE X(13).
               10  FILLER                  PICTURE X(27).
       FD OUTPUT-X
               BLOCK CONTAINS 8000
               RECORD CONTAINS 40.
       01  OUTPUT-X-IO-AREA.
           05  OUTPUT-X-IO-AREA-X          PICTURE X(40).
       FD OUTPUN
               BLOCK CONTAINS 128
               RECORD CONTAINS 128.
       01  OUTPUN-IO-AREA.
           05  OUTPUN-IO-AREA-X            PICTURE X(128).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  KAMMINN-STATUS              PICTURE 99 VALUE 0.
           10  KAMMAST-STATUS              PICTURE 99 VALUE 0.
           10  OUTPUT-X-STATUS             PICTURE 99 VALUE 0.
           10  OUTPUN-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  KAMMINN-EOF-OFF         VALUE '0'.
               88  KAMMINN-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KAMMINN-READ-OFF        VALUE '0'.
               88  KAMMINN-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KAMMINN-PROCESS-OFF     VALUE '0'.
               88  KAMMINN-PROCESS         VALUE '1'.
           05  KAMMAST-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  KAMMINN-DATA-FIELDS.
               10  KEY-X                   PICTURE X(13).
               10  REC                     PICTURE X(40).
               10  REC2                    PICTURE X(27).
               10  FIRMA                   PICTURE X(3).
               10  EDBNR-IO.
                   15  EDBNR               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  KNR                     PICTURE X(6).
               10  FRADTO-IO.
                   15  FRADTO              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  TILDTO-IO.
                   15  TILDTO              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  PRIS-IO.
                   15  PRIS                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  RABTIL                  PICTURE X(1).
               10  ANT-IO.
                   15  ANT                 PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  RAB1-IO.
                   15  RAB1                PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  RAB2-IO.
                   15  RAB2                PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
           05  KAMMAST-DATA-FIELDS.
               10  FILLER                  PICTURE X.
           05  TEMPORARY-FIELDS.
               10  EDB1                    PICTURE X(7).
               10  EDBNR-N-IO.
                   15  EDBNR-N             PICTURE S9(7).
               10  EDB2-IO.
                   15  EDB2                PICTURE S9(7).
               10  EDBNR1-IO.
                   15  EDBNR1              PICTURE S9(7).
               10  FRA1                    PICTURE X(6).
               10  FRADTO-N-IO.
                   15  FRADTO-N            PICTURE S9(7).
               10  FRA2-IO.
                   15  FRA2                PICTURE S9(6).
               10  FDTO-IO.
                   15  FDTO                PICTURE S9(6).
               10  TIL1                    PICTURE X(6).
               10  TILDTO-N-IO.
                   15  TILDTO-N            PICTURE S9(7).
               10  TIL2-IO.
                   15  TIL2                PICTURE S9(6).
               10  TDTO-IO.
                   15  TDTO                PICTURE S9(6).
               10  PRIS1-IO.
                   15  PRIS1               PICTURE S9(7)V9(2).
               10  PRIS-N-IO.
                   15  PRIS-N              PICTURE S9(7)V9(2).
               10  PRIS2-IO.
                   15  PRIS2               PICTURE S9(7)V9(2).
               10  ANT1                    PICTURE X(4).
               10  ANT-N-IO.
                   15  ANT-N               PICTURE S9(5).
               10  ANT2-IO.
                   15  ANT2                PICTURE S9(4).
               10  ANTALL-IO.
                   15  ANTALL              PICTURE S9(4).
           05  EDITTING-FIELDS.
               10  EDIT-EDBNR1             PICTURE Z999999.
               10  EDIT-FDTO               PICTURE Z99999.
               10  EDIT-TDTO               PICTURE Z99999.
               10  EDIT-PRIS2              PICTURE Z999999,99.
               10  EDIT-ANTALL             PICTURE Z999.
               10  EDIT-RAB1               PICTURE Z9,9.
               10  EDIT-RAB2               PICTURE Z9,9.
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
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  KAMMINN-PROCESS
               SET KAMMINN-PROCESS-OFF     TO TRUE
               SET KAMMINN-READ            TO TRUE
           END-IF
 
           IF  KAMMINN-READ
           AND RECORD-SELECTED-OFF
               PERFORM KAMMINN-GET
               SET KAMMINN-READ-OFF        TO TRUE
               IF  NOT KAMMINN-EOF
                   SET KAMMINN-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  KAMMINN-PROCESS
               PERFORM KAMMINN-IDSET
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
 
           IF  KAMMINN-PROCESS
               PERFORM KAMMINN-FLDSET
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
               MOVE KEY-X                  TO KAMMAST-KEY1
               READ KAMMAST RECORD KEY IS KAMMAST-KEY1
               INVALID KEY
                   SET I-25                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-25            TO TRUE
                   PERFORM KAMMAST-IDSET
               END-READ
      *
           END-IF
           MOVE EDBNR                      TO EDBNR-N
           MOVE EDBNR-N-IO                 TO EDB1
           MOVE EDB1                       TO EDB2-IO
           ADD EDB2 TO ZERO            GIVING EDBNR1
      *
           MOVE FRADTO                     TO FRADTO-N
           MOVE FRADTO-N-IO (2:6)          TO FRA1
           MOVE FRA1                       TO FRA2-IO
           ADD FRA2 TO ZERO            GIVING FDTO
      *
           MOVE TILDTO                     TO TILDTO-N
           MOVE TILDTO-N-IO (2:6)          TO TIL1
           MOVE TIL1                       TO TIL2-IO
           ADD TIL2 TO ZERO            GIVING TDTO
      *
           MOVE PRIS                       TO PRIS-N
           MOVE PRIS-N-IO                  TO PRIS1-IO
           ADD PRIS1 TO ZERO           GIVING PRIS2
      *
           MOVE ANT                        TO ANT-N
           MOVE ANT-N-IO (2:4)             TO ANT1
           MOVE ANT1                       TO ANT2-IO
           ADD ANT2 TO ZERO            GIVING ANTALL.
 
       KAMMINN-GET SECTION.
       KAMMINN-GET-P.
           IF  KAMMINN-EOF-OFF
               READ KAMMINN
               AT END
                   SET KAMMINN-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KAMMINN-FLDSET SECTION.
       KAMMINN-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KAMMINN-IO-AREA (1:13) TO KEY-X (1:13)
               MOVE KAMMINN-IO-AREA (1:40) TO REC (1:40)
               MOVE KAMMINN-IO-AREA (14:27) TO REC2 (1:27)
               MOVE KAMMINN-IO-AREA (1:3)  TO FIRMA (1:3)
               MOVE KAMMINN-IO-AREA (4:4)  TO EDBNR-IO
               MOVE KAMMINN-IO-AREA (8:6)  TO KNR (1:6)
               MOVE KAMMINN-IO-AREA (14:4) TO FRADTO-IO
               MOVE KAMMINN-IO-AREA (18:4) TO TILDTO-IO
               MOVE KAMMINN-IO-AREA (22:5) TO PRIS-IO
               MOVE KAMMINN-IO-AREA (27:1) TO RABTIL (1:1)
               MOVE KAMMINN-IO-AREA (28:3) TO ANT-IO
               MOVE KAMMINN-IO-AREA (31:2) TO RAB1-IO
               MOVE KAMMINN-IO-AREA (33:2) TO RAB2-IO
           END-EVALUATE.
 
       KAMMINN-IDSET SECTION.
       KAMMINN-IDSET-P.
           SET I-01                        TO TRUE.
 
       KAMMAST-IDSET SECTION.
       KAMMAST-IDSET-P.
           SET I-02                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND NOT-I-25)
               MOVE REC2                   TO KAMMAST-IO-AREA (14:27)
               REWRITE KAMMAST-IO-AREA
                   INVALID KEY
                       DISPLAY 'Bad REWRITE - file = KAMMAST'
               END-REWRITE
           END-IF
           IF  (I-01 AND I-25)
               MOVE SPACES TO OUTPUT-X-IO-AREA
               INITIALIZE OUTPUT-X-IO-AREA
               MOVE REC                    TO OUTPUT-X-IO-AREA (1:40)
               WRITE OUTPUT-X-IO-AREA
           END-IF
           IF  (I-1P)
               MOVE SPACES TO OUTPUN-IO-AREA
               INITIALIZE OUTPUN-IO-AREA
               MOVE 'FNR'                  TO OUTPUN-IO-AREA (1:3)
               MOVE ';'                    TO OUTPUN-IO-AREA (4:1)
               MOVE 'EDBNR  '              TO OUTPUN-IO-AREA (5:7)
               MOVE ';'                    TO OUTPUN-IO-AREA (12:1)
               MOVE 'KUNDE '               TO OUTPUN-IO-AREA (13:6)
               MOVE ';'                    TO OUTPUN-IO-AREA (19:1)
               MOVE 'FRA DT'               TO OUTPUN-IO-AREA (20:6)
               MOVE ';'                    TO OUTPUN-IO-AREA (26:1)
               MOVE 'TIL DT'               TO OUTPUN-IO-AREA (27:6)
               MOVE ';'                    TO OUTPUN-IO-AREA (33:1)
               MOVE 'KAMP.PRIS '           TO OUTPUN-IO-AREA (34:10)
               MOVE ';'                    TO OUTPUN-IO-AREA (44:1)
      *                                  45 "
               MOVE ';'                    TO OUTPUN-IO-AREA (46:1)
               MOVE 'ANT.'                 TO OUTPUN-IO-AREA (47:4)
               MOVE ';'                    TO OUTPUN-IO-AREA (51:1)
               MOVE 'RAB1'                 TO OUTPUN-IO-AREA (52:4)
               MOVE ';'                    TO OUTPUN-IO-AREA (56:1)
               MOVE 'RAB2'                 TO OUTPUN-IO-AREA (57:4)
               MOVE ';'                    TO OUTPUN-IO-AREA (61:1)
      *                                  64 "ALF"
      *                                  65 ";"
      *                                  85 "ARTIKKELNR"
      *                                  86 ";"
      *                                 100 "VAREBETEGNELSE"
      *                                 101 ";"
               WRITE OUTPUN-IO-AREA
           END-IF
           IF  (I-01 AND I-25)
               MOVE SPACES TO OUTPUN-IO-AREA
               INITIALIZE OUTPUN-IO-AREA
               MOVE FIRMA                  TO OUTPUN-IO-AREA (1:3)
               MOVE ';'                    TO OUTPUN-IO-AREA (4:1)
               MOVE EDBNR1                 TO EDIT-EDBNR1
               MOVE EDIT-EDBNR1            TO OUTPUN-IO-AREA (5:7)
               MOVE ';'                    TO OUTPUN-IO-AREA (12:1)
               MOVE KNR                    TO OUTPUN-IO-AREA (13:6)
               MOVE ';'                    TO OUTPUN-IO-AREA (19:1)
               MOVE FDTO                   TO EDIT-FDTO
               MOVE EDIT-FDTO              TO OUTPUN-IO-AREA (20:6)
               MOVE ';'                    TO OUTPUN-IO-AREA (26:1)
               MOVE TDTO                   TO EDIT-TDTO
               MOVE EDIT-TDTO              TO OUTPUN-IO-AREA (27:6)
               MOVE ';'                    TO OUTPUN-IO-AREA (33:1)
               MOVE PRIS2                  TO EDIT-PRIS2
               MOVE EDIT-PRIS2             TO OUTPUN-IO-AREA (34:10)
               MOVE ';'                    TO OUTPUN-IO-AREA (44:1)
               MOVE RABTIL                 TO OUTPUN-IO-AREA (45:1)
               MOVE ';'                    TO OUTPUN-IO-AREA (46:1)
               MOVE ANTALL                 TO EDIT-ANTALL
               MOVE EDIT-ANTALL            TO OUTPUN-IO-AREA (47:4)
               MOVE ';'                    TO OUTPUN-IO-AREA (51:1)
               MOVE RAB1                   TO EDIT-RAB1
               MOVE EDIT-RAB1              TO OUTPUN-IO-AREA (52:4)
               MOVE ';'                    TO OUTPUN-IO-AREA (56:1)
               MOVE RAB2                   TO EDIT-RAB2
               MOVE EDIT-RAB2              TO OUTPUN-IO-AREA (57:4)
               MOVE ';'                    TO OUTPUN-IO-AREA (61:1)
               MOVE 'NY        '           TO OUTPUN-IO-AREA (62:10)
               MOVE ';'                    TO OUTPUN-IO-AREA (72:1)
      *                     N30ALFA      64
      *                                  65 ";"
      *                     N30ARTNR     85
      *                                  86 ";"
      *                     N30VNAVN    116
      *                                 117 ";"
               WRITE OUTPUN-IO-AREA
           END-IF
           IF  (I-01 AND NOT-I-25)
               MOVE SPACES TO OUTPUN-IO-AREA
               INITIALIZE OUTPUN-IO-AREA
               MOVE FIRMA                  TO OUTPUN-IO-AREA (1:3)
               MOVE ';'                    TO OUTPUN-IO-AREA (4:1)
               MOVE EDBNR1                 TO EDIT-EDBNR1
               MOVE EDIT-EDBNR1            TO OUTPUN-IO-AREA (5:7)
               MOVE ';'                    TO OUTPUN-IO-AREA (12:1)
               MOVE KNR                    TO OUTPUN-IO-AREA (13:6)
               MOVE ';'                    TO OUTPUN-IO-AREA (19:1)
               MOVE FDTO                   TO EDIT-FDTO
               MOVE EDIT-FDTO              TO OUTPUN-IO-AREA (20:6)
               MOVE ';'                    TO OUTPUN-IO-AREA (26:1)
               MOVE TDTO                   TO EDIT-TDTO
               MOVE EDIT-TDTO              TO OUTPUN-IO-AREA (27:6)
               MOVE ';'                    TO OUTPUN-IO-AREA (33:1)
               MOVE PRIS2                  TO EDIT-PRIS2
               MOVE EDIT-PRIS2             TO OUTPUN-IO-AREA (34:10)
               MOVE ';'                    TO OUTPUN-IO-AREA (44:1)
               MOVE RABTIL                 TO OUTPUN-IO-AREA (45:1)
               MOVE ';'                    TO OUTPUN-IO-AREA (46:1)
               MOVE ANTALL                 TO EDIT-ANTALL
               MOVE EDIT-ANTALL            TO OUTPUN-IO-AREA (47:4)
               MOVE ';'                    TO OUTPUN-IO-AREA (51:1)
               MOVE RAB1                   TO EDIT-RAB1
               MOVE EDIT-RAB1              TO OUTPUN-IO-AREA (52:4)
               MOVE ';'                    TO OUTPUN-IO-AREA (56:1)
               MOVE RAB2                   TO EDIT-RAB2
               MOVE EDIT-RAB2              TO OUTPUN-IO-AREA (57:4)
               MOVE ';'                    TO OUTPUN-IO-AREA (61:1)
               MOVE 'OPPDATERT '           TO OUTPUN-IO-AREA (62:10)
               MOVE ';'                    TO OUTPUN-IO-AREA (72:1)
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
           INITIALIZE KAMMINN-DATA-FIELDS
           SET KAMMINN-EOF-OFF             TO TRUE
           SET KAMMINN-PROCESS             TO TRUE
           OPEN INPUT KAMMINN
           OPEN I-O KAMMAST
           OPEN OUTPUT OUTPUT-X
           OPEN OUTPUT OUTPUN.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE KAMMINN
           CLOSE KAMMAST
           CLOSE OUTPUT-X
           CLOSE OUTPUN.
 
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
