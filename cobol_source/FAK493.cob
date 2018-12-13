       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK493R.
      *****************************************************************
      **********************************************  Z-WIN-RPG2   ****
      *  DANNE RESKONTRO FOR KONTANTKUNDER                            *
      *E 27.05.08 LAGER TRANS 11 FOR KREDITNOTA OG 36 TRANS FOR    MT *
      *           "UTBET" NÅR ORDRENR STARTER PÅ 9.                MT *
      * LAGT INN BIL.ART I POS 77                                  SS *
      *E 11.12.08 LAGT INN BETALINGSMÅTE I INPUTFILE OG OUTPUTFILE MT *
      *E 22.01.09 LAGT INN "07" I BETALINGSMÅTE FAST I POS 61-62   MT *
      *E 27.11.13 LAGT INN ORDRENR I REFNR                         MT *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK493.rpg
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
           SELECT ENRLIM
               ASSIGN TO ENRLIM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ENRLIM-STATUS.
           SELECT INFILE
               ASSIGN TO INFILE
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS INFILE-STATUS
               RECORD KEY IS INFILE-KEY1.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT RESK2UT
               ASSIGN TO UT-S-RESK2UT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RESK2UT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD ENRLIM
               RECORD CONTAINS 80.
       01  ENRLIM-IO-AREA.
           05  ENRLIM-IO-AREA-X.
               10  ENRLIM-KEY1.
                   15  ENRLIM-KEY1N        PICTURE S9(11).
               10  FILLER                  PICTURE X(69).
       FD INFILE
               RECORD CONTAINS 146.
       01  INFILE-IO-AREA.
           05  INFILE-IO-AREA-X.
               10  INFILE-KEY1             PICTURE X(11).
               10  FILLER                  PICTURE X(135).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD RESK2UT
               BLOCK CONTAINS 8000
               RECORD CONTAINS 80.
       01  RESK2UT-IO-AREA.
           05  RESK2UT-IO-AREA-X           PICTURE X(80).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  ENRLIM-STATUS               PICTURE 99 VALUE 0.
           10  INFILE-STATUS               PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  RESK2UT-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  ENRLIM-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  ENRLIM-EOF-OFF          VALUE '0'.
               88  ENRLIM-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ENRLIM-READ-OFF         VALUE '0'.
               88  ENRLIM-READ             VALUE '1'.
           05  ENRLIM-LOW-KEY              PICTURE X(11).
           05  ENRLIM-HIGH-KEY             PICTURE X(11).
           05  INFILE-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
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
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  INFILE-LEVEL-01.
               10  INFILE-01-L1.
                   15  INFILE-01-L1-FNR    PICTURE X(3).
           05  INFILE-DATA-FIELDS.
               10  DAG                     PICTURE X(2).
               10  MND                     PICTURE X(2).
               10  AAR                     PICTURE X(2).
               10  FAKNR                   PICTURE X(6).
               10  KUNDE                   PICTURE X(6).
               10  KNR1                    PICTURE X(1).
               10  BEL1-IO.
                   15  BEL1                PICTURE S9(4).
               10  BEL2-IO.
                   15  BEL2                PICTURE S9(3).
               10  BEL3-IO.
                   15  BEL3                PICTURE S9(2).
               10  FNR                     PICTURE X(3).
      *                                      84  84 SIGN
               10  LINTYP                  PICTURE X(2).
               10  ONR                     PICTURE X(6).
           05  FIRMAF-DATA-FIELDS.
               10  KONFNR                  PICTURE X(3).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  FAKDAG                  PICTURE X(2).
               10  FAKMND                  PICTURE X(2).
               10  FAKAAR                  PICTURE X(2).
               10  FORDAG                  PICTURE X(2).
               10  FORMND                  PICTURE X(2).
               10  FORAAR                  PICTURE X(2).
               10  FAKTNR                  PICTURE X(6).
               10  KNR                     PICTURE X(6).
               10  KUN1                    PICTURE X(1).
               10  AREAY1-IO.
                   15  AREAY1              PICTURE S9(7).
               10  AREAX1-IO.
                   15  AREAX1              PICTURE S9(7)V9(2).
               10  BELOP-IO.
                   15  BELOP               PICTURE S9(7)V9(2).
               10  NYVAL-IO.
                   15  NYVAL               PICTURE S9(7)V9(2).
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
           MOVE FNR                        TO FIRMAF-KEY1
           READ FIRMAF RECORD KEY IS FIRMAF-KEY1
           INVALID KEY
               SET I-55                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-55                TO TRUE
               PERFORM FIRMAF-FLDSET
               PERFORM FIRMAF-IDSET
           END-READ
           IF  (NOT-I-55)
               SET NOT-I-25                TO TRUE
               IF  KONFNR > '000'
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-55 AND I-25)
               MOVE KONFNR                 TO FNR
           END-IF
           SET NOT-I-50                    TO TRUE
           SET NOT-I-51                    TO TRUE
      *          LINTYP    COMP "01"                     71
           SET NOT-I-72                    TO TRUE
           IF  LINTYP = '02'
               SET I-72                    TO TRUE
           END-IF
           SET NOT-I-73                    TO TRUE
           IF  LINTYP = '03'
               SET I-73                    TO TRUE
           END-IF
      *          LINTYP    COMP "04"                     74
           SET NOT-I-75                    TO TRUE
           IF  LINTYP = '05'
               SET I-75                    TO TRUE
           END-IF
           SET NOT-I-78                    TO TRUE
           IF  LINTYP = '08'
               SET I-78                    TO TRUE
           END-IF
      *          LINTYP    COMP "09"                     79
      *          LINTYP    COMP "10"                     80
      *          LINTYP    COMP "11"                     81
      *          LINTYP    COMP "12"                     82
      *          LINTYP    COMP "13"                     83
           IF  (I-72)
               MOVE DAG                    TO FAKDAG
               MOVE MND                    TO FAKMND
               MOVE AAR                    TO FAKAAR
           END-IF
           IF  (I-73)
               MOVE DAG                    TO FORDAG
               MOVE MND                    TO FORMND
               MOVE AAR                    TO FORAAR
           END-IF
           IF  (I-75)
               MOVE FAKNR                  TO FAKTNR
           END-IF
           IF  (I-78)
               MOVE KUNDE                  TO KNR
               MOVE KNR1                   TO KUN1
               MOVE BEL2                   TO AREAY1-IO (5:3)
               MOVE BEL1                   TO AREAY1 (1:4)
               MOVE BEL3                   TO AREAX1-IO (8:2)
               MOVE AREAY1                 TO AREAX1 (1:7)
               MULTIPLY 1 BY AREAX1    GIVING BELOP
               SET NOT-I-11                TO TRUE
               IF  ONR > '899999'
                   SET I-11                TO TRUE
               END-IF
               MOVE 0                      TO NYVAL
               SET NOT-I-51                TO TRUE
               IF  KUN1 = '5'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-78 AND NOT-I-51)
               SET I-50                    TO TRUE
      *******************************************************
           END-IF
           .
 
       INFILE-GET SECTION.
       INFILE-GET-P.
           IF  INFILE-EOF-OFF
               PERFORM WITH TEST AFTER
                 UNTIL ENRLIM-READ-OFF
                    OR ENRLIM-EOF
                   IF  ENRLIM-READ
                       SET ENRLIM-READ-OFF TO TRUE
                       READ ENRLIM
                       AT END
                           SET ENRLIM-EOF  TO TRUE
                           SET INFILE-EOF  TO TRUE
                       NOT AT END
                           MOVE ENRLIM-IO-AREA (1:4) TO INFILE-KEY1
                       END-READ
                   END-IF
                   IF  ENRLIM-EOF-OFF
                   AND ENRLIM-READ-OFF
                       READ INFILE
                       INVALID KEY
                           SET I-H0        TO TRUE
                           MOVE 'N'        TO E-R-R-O-R
                       END-READ
                   END-IF
               END-PERFORM
           END-IF.
 
       INFILE-FLDSET SECTION.
       INFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INFILE-IO-AREA (29:2)  TO DAG (1:2)
               MOVE INFILE-IO-AREA (32:2)  TO MND (1:2)
               MOVE INFILE-IO-AREA (35:2)  TO AAR (1:2)
               MOVE INFILE-IO-AREA (78:6)  TO FAKNR (1:6)
               MOVE INFILE-IO-AREA (35:6)  TO KUNDE (1:6)
               MOVE INFILE-IO-AREA (35:1)  TO KNR1 (1:1)
               MOVE INFILE-IO-AREA (73:4)  TO BEL1-IO
               INSPECT BEL1-IO REPLACING ALL ' ' BY '0'
               MOVE INFILE-IO-AREA (78:3)  TO BEL2-IO
               INSPECT BEL2-IO REPLACING ALL ' ' BY '0'
               MOVE INFILE-IO-AREA (82:2)  TO BEL3-IO
               INSPECT BEL3-IO REPLACING ALL ' ' BY '0'
               MOVE INFILE-IO-AREA (1:3)   TO FNR (1:3)
               MOVE INFILE-IO-AREA (10:2)  TO LINTYP (1:2)
               MOVE INFILE-IO-AREA (15:6)  TO ONR (1:6)
           END-EVALUATE.
 
       INFILE-IDSET SECTION.
       INFILE-IDSET-P.
           SET I-01                        TO TRUE.
 
       INFILE-CHK-LEVEL SECTION.
       INFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO INFILE-LEVEL-01
               MOVE INFILE-IO-AREA (1:3)   TO INFILE-01-L1-FNR
               IF  INFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INFILE-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INFILE-01-L1          TO THE-PRIOR-L1
               SET INFILE-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (1:3)   TO KONFNR (1:3)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-02                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-50)
               MOVE SPACES TO RESK2UT-IO-AREA
               INITIALIZE RESK2UT-IO-AREA
               MOVE '02'                   TO RESK2UT-IO-AREA (1:2)
               MOVE FNR                    TO RESK2UT-IO-AREA (3:3)
               MOVE KNR                    TO RESK2UT-IO-AREA (6:6)
               MOVE FAKTNR                 TO RESK2UT-IO-AREA (20:6)
               MOVE '01'                   TO RESK2UT-IO-AREA (12:2)
               IF  (I-11)
                   MOVE '11'               TO RESK2UT-IO-AREA (12:2)
               END-IF
               MOVE FAKAAR                 TO RESK2UT-IO-AREA (18:2)
               MOVE FAKMND                 TO RESK2UT-IO-AREA (16:2)
               MOVE FAKDAG                 TO RESK2UT-IO-AREA (14:2)
               MOVE FAKTNR                 TO RESK2UT-IO-AREA (26:6)
               MOVE ONR                    TO RESK2UT-IO-AREA (26:6)
               MOVE FORAAR                 TO RESK2UT-IO-AREA (36:2)
               MOVE FORMND                 TO RESK2UT-IO-AREA (34:2)
               MOVE FORDAG                 TO RESK2UT-IO-AREA (32:2)
               MOVE BELOP-IO               TO RESK2UT-IO-AREA (38:9)
               MOVE '+'                    TO RESK2UT-IO-AREA (47:1)
               IF  (I-11)
                   MOVE '-'                TO RESK2UT-IO-AREA (47:1)
               END-IF
               MOVE '1'                    TO RESK2UT-IO-AREA (48:1)
               MOVE '07'                   TO RESK2UT-IO-AREA (49:2)
               MOVE '07'                   TO RESK2UT-IO-AREA (61:2)
               MOVE NYVAL-IO               TO RESK2UT-IO-AREA (65:9)
               MOVE ' '                    TO RESK2UT-IO-AREA (76:1)
               MOVE '2'                    TO RESK2UT-IO-AREA (77:1)
               IF  (I-11)
                   MOVE '3'                TO RESK2UT-IO-AREA (77:1)
               END-IF
               WRITE RESK2UT-IO-AREA
               MOVE SPACES TO RESK2UT-IO-AREA
               INITIALIZE RESK2UT-IO-AREA
               MOVE '02'                   TO RESK2UT-IO-AREA (1:2)
               MOVE FNR                    TO RESK2UT-IO-AREA (3:3)
               MOVE KNR                    TO RESK2UT-IO-AREA (6:6)
               MOVE FAKTNR                 TO RESK2UT-IO-AREA (20:6)
               MOVE '31'                   TO RESK2UT-IO-AREA (12:2)
               IF  (I-11)
                   MOVE '36'               TO RESK2UT-IO-AREA (12:2)
               END-IF
               MOVE FAKAAR                 TO RESK2UT-IO-AREA (18:2)
               MOVE FAKMND                 TO RESK2UT-IO-AREA (16:2)
               MOVE FAKDAG                 TO RESK2UT-IO-AREA (14:2)
               MOVE FAKTNR                 TO RESK2UT-IO-AREA (26:6)
               MOVE ONR                    TO RESK2UT-IO-AREA (26:6)
               MOVE FORAAR                 TO RESK2UT-IO-AREA (36:2)
               MOVE FORMND                 TO RESK2UT-IO-AREA (34:2)
               MOVE FORDAG                 TO RESK2UT-IO-AREA (32:2)
               MOVE BELOP-IO               TO RESK2UT-IO-AREA (38:9)
               MOVE '-'                    TO RESK2UT-IO-AREA (47:1)
               IF  (I-11)
                   MOVE '+'                TO RESK2UT-IO-AREA (47:1)
               END-IF
               MOVE '1'                    TO RESK2UT-IO-AREA (48:1)
               MOVE '07'                   TO RESK2UT-IO-AREA (49:2)
               MOVE '07'                   TO RESK2UT-IO-AREA (61:2)
               MOVE NYVAL-IO               TO RESK2UT-IO-AREA (65:9)
               MOVE ' '                    TO RESK2UT-IO-AREA (76:1)
               MOVE '2'                    TO RESK2UT-IO-AREA (77:1)
               IF  (I-11)
                   MOVE '3'                TO RESK2UT-IO-AREA (77:1)
               END-IF
               WRITE RESK2UT-IO-AREA
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
           SET ENRLIM-EOF-OFF              TO TRUE
           SET ENRLIM-READ                 TO TRUE
           OPEN INPUT ENRLIM
           SET INFILE-LEVEL-INIT           TO TRUE
           INITIALIZE INFILE-DATA-FIELDS
           SET INFILE-EOF-OFF              TO TRUE
           SET INFILE-PROCESS              TO TRUE
           OPEN INPUT INFILE
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           OPEN OUTPUT RESK2UT.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE ENRLIM
           CLOSE INFILE
           CLOSE FIRMAF
           CLOSE RESK2UT.
 
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
