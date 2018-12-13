       IDENTIFICATION DIVISION.
       PROGRAM-ID. RSK725R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM: RSK725                                        *
      *  DANNER REGNSKAP OG RESKONTROREC FOR LØNSETHAGEN KOMM.  *
      *E 23.10.00 AVG. FRI OMS BLE BEHANDLET SOM PLIKTIG JFR    *
      *                GAUPSETH. KONTO POS 6-11 = 003000 = PLI. *
      *                          KONTO POS 6-11 = 003110 = FRI. *
      *E 26.10.00 TATT UT AVG. FRI OMS.                         *
      *E 09.02.01 RETTET MVA-% TIL 24.                          *
      ***********************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RSK725.rpg
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
           SELECT RESFILE
               ASSIGN TO UT-S-RESFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RESFILE-STATUS.
           SELECT REGFILE
               ASSIGN TO UT-S-REGFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS REGFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD INNFIL
               BLOCK CONTAINS 800
               RECORD CONTAINS 80.
       01  INNFIL-IO-AREA.
           05  INNFIL-IO-AREA-X            PICTURE X(80).
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1            PICTURE X(9).
               10  FILLER                  PICTURE X(190).
       FD RESFILE
               BLOCK CONTAINS 140
               RECORD CONTAINS 70.
       01  RESFILE-IO-AREA.
           05  RESFILE-IO-AREA-X           PICTURE X(70).
      *ISTE   O   F 132 132     OF     PRINTERSYSLST
       FD REGFILE
               BLOCK CONTAINS 160
               RECORD CONTAINS 80.
       01  REGFILE-IO-AREA.
           05  REGFILE-IO-AREA-X           PICTURE X(80).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  INNFIL-STATUS               PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  RESFILE-STATUS              PICTURE 99 VALUE 0.
           10  REGFILE-STATUS              PICTURE 99 VALUE 0.
 
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
           05  INNFIL-LEVEL-01.
               10  INNFIL-01-L1.
                   15  INNFIL-01-L1-FAKNR  PICTURE S9(6).
           05  INNFIL-DATA-FIELDS.
               10  MVAK                    PICTURE X(1).
               10  BDAG-IO.
                   15  BDAG                PICTURE S9(2).
               10  BMND-IO.
                   15  BMND                PICTURE S9(2).
               10  BAAR-IO.
                   15  BAAR                PICTURE S9(2).
               10  KODE                    PICTURE X(1).
               10  BELD-IO.
                   15  BELD                PICTURE S9(8)V9(2).
               10  BELK-IO.
                   15  BELK                PICTURE S9(8)V9(2).
      *                                      27  28 MVAK
               10  KNR                     PICTURE X(6).
               10  FAKNR-IO.
                   15  FAKNR               PICTURE S9(6).
               10  FFDAG-IO.
                   15  FFDAG               PICTURE S9(2).
               10  FFMND-IO.
                   15  FFMND               PICTURE S9(2).
               10  FFAAR-IO.
                   15  FFAAR               PICTURE S9(2).
           05  KUNDEMA-DATA-FIELDS.
               10  BM                      PICTURE X(2).
               10  HND                     PICTURE X(3).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  REC-IO.
                   15  REC                 PICTURE S9(5).
               10  KKEY                    PICTURE X(9).
               10  KUNDE                   PICTURE X(6).
               10  BELOP-IO.
                   15  BELOP               PICTURE S9(7)V9(2).
               10  MOMSX1-IO.
                   15  MOMSX1              PICTURE S9(7)V9(2).
               10  SUMA-IO.
                   15  SUMA                PICTURE S9(7)V9(2).
               10  NETTO-IO.
                   15  NETTO               PICTURE S9(7)V9(2).
               10  L1MOMS-IO.
                   15  L1MOMS              PICTURE S9(7)V9(2).
               10  L1NET-IO.
                   15  L1NET               PICTURE S9(7)V9(2).
               10  BELOPS-IO.
                   15  BELOPS              PICTURE S9(7)V9(2).
               10  L1BELS-IO.
                   15  L1BELS              PICTURE S9(7)V9(2).
               10  NULL9-IO.
                   15  NULL9               PICTURE S9(7)V9(2).
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
           PERFORM TOTAL-CALCS
           PERFORM TOTAL-OUTPUT.
 
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
           IF  (I-L1)
               MOVE 0                      TO REC
           END-IF
           IF  (I-01)
               ADD 1                       TO REC
      *  01                MOVEL"998"     KKEY    9
           END-IF
           IF  (I-01)
               MOVE '511'                  TO KKEY (1:3)
               MOVE KNR                    TO KUNDE
      *  01                MOVEL"1"       KUNDE
           END-IF
           IF  (I-01)
               MOVE KUNDE                  TO KKEY (4:6)
               MOVE KKEY                   TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-25                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-25            TO TRUE
                   PERFORM KUNDEMA-FLDSET
                   PERFORM KUNDEMA-IDSET
               END-READ
               SET NOT-I-08                TO TRUE
               SET NOT-I-91                TO TRUE
               IF  KODE = '0'
                   SET I-91                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-91)
               GO TO SLUTT-T
      *  01      KODE      COMP "1"                      90  KONT
      *  01N90   KODE      COMP "8"                      90  KONT
      *  01N90   KODE      COMP "9"                      90  KONT
      *  01      MVAK      COMP "1"                      08  MOMSFRITT
           END-IF
           IF  (I-01)
               SET NOT-I-30                TO TRUE
               IF  BELK NOT = 0
                   SET I-30                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-30)
               ADD BELK TO ZERO        GIVING BELOP
           END-IF
           IF  (I-01 AND NOT-I-30)
               ADD BELD TO ZERO        GIVING BELOP
           END-IF
           IF  (I-01)
               MOVE 0                      TO MOMSX1
           END-IF
           IF  (I-01 AND NOT-I-08)
               DIVIDE BELOP BY 125     GIVING SUMA ROUNDED
               MULTIPLY 25 BY SUMA     GIVING MOMSX1
           END-IF
           IF  (I-01)
               SUBTRACT MOMSX1 FROM BELOP GIVING NETTO
           END-IF
           IF  (I-01 AND I-30)
               MULTIPLY -1 BY NETTO    GIVING NETTO
               MULTIPLY -1 BY MOMSX1   GIVING MOMSX1
           END-IF
           IF  (I-01)
               ADD MOMSX1                  TO L1MOMS
               ADD NETTO                   TO L1NET
      *
           END-IF
           IF  (I-01 AND NOT-I-30)
               ADD BELOP TO ZERO       GIVING BELOPS
           END-IF
           IF  (I-01 AND I-30)
               MULTIPLY -1 BY BELOP    GIVING BELOPS
           END-IF
           IF  (I-01)
               ADD BELOPS                  TO L1BELS
      *
           END-IF
           IF  (I-01)
               MOVE 0                      TO NULL9
           END-IF.
 
       SLUTT-T.
      *
           CONTINUE.
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1)
               SET NOT-I-50                TO TRUE
               IF  L1NET < 0,00
                   SET I-50                TO TRUE
               END-IF
               SET NOT-I-51                TO TRUE
               IF  L1MOMS < 0,00
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-50)
               MULTIPLY -1 BY L1NET    GIVING L1NET
           END-IF
           IF  (I-L1 AND I-51)
               MULTIPLY -1 BY L1MOMS   GIVING L1MOMS
           END-IF
           IF  (I-L1)
               SET NOT-I-66                TO TRUE
               IF  REC > 0
                   SET I-66                TO TRUE
               END-IF
      *
           END-IF
           .
 
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
               MOVE INNFIL-IO-AREA (09:1)  TO MVAK (1:1)
               MOVE INNFIL-IO-AREA (17:2)  TO BDAG-IO
               INSPECT BDAG-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (19:2)  TO BMND-IO
               INSPECT BMND-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (21:2)  TO BAAR-IO
               INSPECT BAAR-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (26:1)  TO KODE (1:1)
               MOVE INNFIL-IO-AREA (29:10) TO BELD-IO
               INSPECT BELD-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (39:10) TO BELK-IO
               INSPECT BELK-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (55:6)  TO KNR (1:6)
               MOVE INNFIL-IO-AREA (11:6)  TO FAKNR-IO
               INSPECT FAKNR-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (49:2)  TO FFDAG-IO
               INSPECT FFDAG-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (51:2)  TO FFMND-IO
               INSPECT FFMND-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (53:2)  TO FFAAR-IO
               INSPECT FFAAR-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       INNFIL-IDSET SECTION.
       INNFIL-IDSET-P.
           SET I-01                        TO TRUE.
 
       INNFIL-CHK-LEVEL SECTION.
       INNFIL-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO INNFIL-LEVEL-01
               MOVE INNFIL-IO-AREA (11:6)  TO INNFIL-01-L1-FAKNR
               IF  INNFIL-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INNFIL-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INNFIL-01-L1          TO THE-PRIOR-L1
               SET INNFIL-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (127:2) TO BM (1:2)
               MOVE KUNDEMA-IO-AREA (185:3) TO HND (1:3)
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-03                        TO TRUE.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-66)
               MOVE SPACES TO REGFILE-IO-AREA
               INITIALIZE REGFILE-IO-AREA
               MOVE '01'                   TO REGFILE-IO-AREA (1:2)
               MOVE FAKNR-IO               TO REGFILE-IO-AREA (3:6)
               IF  (I-30)
                   MOVE '9'                TO REGFILE-IO-AREA (3:1)
               END-IF
               IF  (NOT-I-30)
                   MOVE '2'                TO REGFILE-IO-AREA (9:1)
               END-IF
               IF  (I-30)
                   MOVE '3'                TO REGFILE-IO-AREA (9:1)
               END-IF
               MOVE BAAR-IO                TO REGFILE-IO-AREA (10:2)
               MOVE BMND-IO                TO REGFILE-IO-AREA (12:2)
               MOVE BDAG-IO                TO REGFILE-IO-AREA (14:2)
               MOVE KUNDE                  TO REGFILE-IO-AREA (16:6)
               MOVE '511'                  TO REGFILE-IO-AREA (22:3)
               MOVE '5'                    TO REGFILE-IO-AREA (31:1)
               MOVE '10000'                TO REGFILE-IO-AREA (32:5)
               MOVE L1NET-IO               TO REGFILE-IO-AREA (37:9)
               INITIALIZE L1NET-IO
               IF  (I-50)
                   MOVE '-'                TO REGFILE-IO-AREA (46:1)
               END-IF
               MOVE NULL9-IO               TO REGFILE-IO-AREA (47:9)
               MOVE FFAAR-IO               TO REGFILE-IO-AREA (67:2)
               MOVE FFMND-IO               TO REGFILE-IO-AREA (65:2)
               MOVE FFDAG-IO               TO REGFILE-IO-AREA (63:2)
               MOVE HND                    TO REGFILE-IO-AREA (74:3)
               MOVE 'F'                    TO REGFILE-IO-AREA (77:1)
               MOVE '5'                    TO REGFILE-IO-AREA (78:1)
               MOVE ' '                    TO REGFILE-IO-AREA (79:1)
               MOVE '1'                    TO REGFILE-IO-AREA (80:1)
               WRITE REGFILE-IO-AREA
           END-IF
           IF  (I-L1 AND I-66 AND NOT-I-08)
               MOVE SPACES TO REGFILE-IO-AREA
               INITIALIZE REGFILE-IO-AREA
               MOVE '01'                   TO REGFILE-IO-AREA (1:2)
               MOVE FAKNR-IO               TO REGFILE-IO-AREA (3:6)
      *                      30           3 "9"
               IF  (NOT-I-30)
                   MOVE '2'                TO REGFILE-IO-AREA (9:1)
               END-IF
               IF  (I-30)
                   MOVE '3'                TO REGFILE-IO-AREA (9:1)
               END-IF
               MOVE BAAR-IO                TO REGFILE-IO-AREA (10:2)
               MOVE BMND-IO                TO REGFILE-IO-AREA (12:2)
               MOVE BDAG-IO                TO REGFILE-IO-AREA (14:2)
               MOVE KUNDE                  TO REGFILE-IO-AREA (16:6)
      *                                  24 "998"
               MOVE '511'                  TO REGFILE-IO-AREA (22:3)
               MOVE '  '                   TO REGFILE-IO-AREA (30:2)
               MOVE '2201 '                TO REGFILE-IO-AREA (32:5)
               MOVE L1MOMS-IO              TO REGFILE-IO-AREA (37:9)
               INITIALIZE L1MOMS-IO
               IF  (I-51)
                   MOVE '-'                TO REGFILE-IO-AREA (46:1)
               END-IF
               MOVE FFAAR-IO               TO REGFILE-IO-AREA (67:2)
               MOVE FFMND-IO               TO REGFILE-IO-AREA (65:2)
               MOVE FFDAG-IO               TO REGFILE-IO-AREA (63:2)
               MOVE HND                    TO REGFILE-IO-AREA (74:3)
               MOVE 'F'                    TO REGFILE-IO-AREA (77:1)
               MOVE '5'                    TO REGFILE-IO-AREA (78:1)
               MOVE '1'                    TO REGFILE-IO-AREA (80:1)
               WRITE REGFILE-IO-AREA
           END-IF
           IF  (I-L1 AND I-66)
               MOVE SPACES TO RESFILE-IO-AREA
               INITIALIZE RESFILE-IO-AREA
               MOVE '02'                   TO RESFILE-IO-AREA (1:2)
               IF  (NOT-I-30)
                   MOVE '21'               TO RESFILE-IO-AREA (3:2)
               END-IF
               IF  (I-30)
                   MOVE '26'               TO RESFILE-IO-AREA (3:2)
               END-IF
               MOVE KUNDE                  TO RESFILE-IO-AREA (5:6)
               MOVE BAAR-IO                TO RESFILE-IO-AREA (11:2)
               MOVE BMND-IO                TO RESFILE-IO-AREA (13:2)
               MOVE BDAG-IO                TO RESFILE-IO-AREA (15:2)
               MOVE FAKNR-IO               TO RESFILE-IO-AREA (17:6)
               IF  (I-30)
                   MOVE '9'                TO RESFILE-IO-AREA (17:1)
               END-IF
               MOVE FAKNR-IO               TO RESFILE-IO-AREA (23:6)
               IF  (I-30)
                   MOVE '9'                TO RESFILE-IO-AREA (23:1)
               END-IF
               MOVE FFAAR-IO               TO RESFILE-IO-AREA (29:2)
               MOVE FFMND-IO               TO RESFILE-IO-AREA (31:2)
               MOVE FFDAG-IO               TO RESFILE-IO-AREA (33:2)
               MOVE L1BELS-IO              TO RESFILE-IO-AREA (35:9)
               INITIALIZE L1BELS-IO
               MOVE '511'                  TO RESFILE-IO-AREA (44:3)
               IF  (NOT-I-30)
                   MOVE '2'                TO RESFILE-IO-AREA (47:1)
               END-IF
               IF  (I-30)
                   MOVE '3'                TO RESFILE-IO-AREA (47:1)
               END-IF
               MOVE BM                     TO RESFILE-IO-AREA (48:2)
               MOVE '1'                    TO RESFILE-IO-AREA (50:1)
               MOVE ' '                    TO RESFILE-IO-AREA (69:1)
               MOVE ' '                    TO RESFILE-IO-AREA (70:1)
               WRITE RESFILE-IO-AREA
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
           OPEN OUTPUT RESFILE
           OPEN OUTPUT REGFILE.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INNFIL
           CLOSE KUNDEMA
           CLOSE RESFILE
           CLOSE REGFILE.
 
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
