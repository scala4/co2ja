       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK130R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAM: FAK130                                               *
      * SAMMESLÅING AV REGNSKAPSDATA FRA FAKTURA.                     *
      *  5/ 3/99 NY RUTINE FOR OMDØPING AV FIRMANUMMER.               *
      * 28/ 6/99 NY RUTINE FOR OMDØPING AV KUNDENR.                   *
      *  1/ 9/00 NY RUTINE FOR HENTING AV KJEDEFIRMA OG KOSTNADSTED.  *
      *  1/ 9/00 ØKT RECORDLENGDE FRA 80 TIL 100 PÅ OUTFILE.          *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK130.rpg
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
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT KJEDTAB
               ASSIGN TO KJEDTAB
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KJEDTAB-STATUS
               RECORD KEY IS KJEDTAB-KEY1.
           SELECT OUTFILE
               ASSIGN TO UT-S-OUTFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD INFILE
               BLOCK CONTAINS 160
               RECORD CONTAINS 80.
       01  INFILE-IO-AREA.
           05  INFILE-IO-AREA-X            PICTURE X(80).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD KJEDTAB
               RECORD CONTAINS 120.
       01  KJEDTAB-IO-AREA.
           05  KJEDTAB-IO-AREA-X.
               10  KJEDTAB-KEY1            PICTURE X(4).
               10  FILLER                  PICTURE X(116).
       FD OUTFILE
               BLOCK CONTAINS 200
               RECORD CONTAINS 100.
       01  OUTFILE-IO-AREA.
           05  OUTFILE-IO-AREA-X           PICTURE X(100).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  INFILE-STATUS               PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  KJEDTAB-STATUS              PICTURE 99 VALUE 0.
           10  OUTFILE-STATUS              PICTURE 99 VALUE 0.
 
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
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  KJEDTAB-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  INFILE-LEVEL-01.
               10  INFILE-01-L6.
                   15  INFILE-01-L6-FIRMNR PICTURE X(3).
               10  INFILE-01-L5.
                   15  INFILE-01-L5-RESKNR PICTURE X(6).
               10  INFILE-01-L4.
                   15  INFILE-01-L4-BNR    PICTURE X(6).
               10  INFILE-01-L3.
                   15  INFILE-01-L3-HDIST  PICTURE X(3).
               10  INFILE-01-L2.
                   15  INFILE-01-L2-VGR    PICTURE X(5).
               10  INFILE-01-L1.
                   15  INFILE-01-L1-AVD    PICTURE X(1).
           05  INFILE-DATA-FIELDS.
               10  BNR                     PICTURE X(6).
               10  A1                      PICTURE X(7).
               10  RESKNR                  PICTURE X(6).
               10  RNR2S                   PICTURE X(2).
               10  RESNR1                  PICTURE X(1).
               10  FIRMNR                  PICTURE X(3).
               10  A2                      PICTURE X(7).
               10  VGR                     PICTURE X(5).
               10  BEL-IO.
                   15  BEL                 PICTURE S9(7)V9(2).
               10  SIGN-X                  PICTURE X(1).
               10  KOSTPR-IO.
                   15  KOSTPR              PICTURE S9(7)V9(2).
               10  A3                      PICTURE X(18).
               10  HDIST                   PICTURE X(3).
               10  A4                      PICTURE X(3).
               10  AVD                     PICTURE X(1).
           05  FIRMAF-DATA-FIELDS.
               10  KONFNR                  PICTURE X(3).
           05  KJEDTAB-DATA-FIELDS.
               10  KJEFNR                  PICTURE X(3).
               10  KJESTE                  PICTURE X(4).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L6            PICTURE X(3).
               10  THE-PRIOR-L5            PICTURE X(6).
               10  THE-PRIOR-L4            PICTURE X(6).
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(5).
               10  THE-PRIOR-L1            PICTURE X(1).
           05  TEMPORARY-FIELDS.
               10  REGFNR                  PICTURE X(3).
               10  KNSTED                  PICTURE X(4).
               10  FNRAVD                  PICTURE X(4).
               10  KUNDNR                  PICTURE X(6).
               10  BEL1-IO.
                   15  BEL1                PICTURE S9(7)V9(2).
               10  BEL2-IO.
                   15  BEL2                PICTURE S9(7)V9(2).
               10  KOST1-IO.
                   15  KOST1               PICTURE S9(7)V9(2).
               10  KOST2-IO.
                   15  KOST2               PICTURE S9(7)V9(2).
               10  VGBEL-IO.
                   15  VGBEL               PICTURE S9(7)V9(2).
               10  VGKOST-IO.
                   15  VGKOST              PICTURE S9(7)V9(2).
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
           PERFORM TOTAL-CALCS
           PERFORM TOTAL-OUTPUT.
 
       LR-INDICATOR-TEST.
           IF  I-LR
               GO TO MAINLINE-TERMINATION
           END-IF
 
           IF  INFILE-PROCESS
               PERFORM INFILE-FLDOFF
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
           IF  (I-L6)
               MOVE FIRMNR                 TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-55                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-55            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
               SET NOT-I-90                TO TRUE
               IF  KONFNR > '000'
                   SET I-90                TO TRUE
               END-IF
      *****************************************************************
      * RUTINE FOR ENDRING AV FIRMANR. I REGNSKAPSRECORD.             *
      * LAGT INN OVERFØRING FRA KONSERN FIRMANR I FIRMAFILE 15.02.2000*
      *****************************************************************
           END-IF
           MOVE FIRMNR                     TO REGFNR
           IF  (I-90)
               MOVE KONFNR                 TO REGFNR
      *****************************************************************
      * RUTINE FOR KJEDER:  HENTE DATA FRA KJEDE.AVD.TABELL 1.09.2000 *
      * HENTE FIRMANR. HOVEDKONTOR/KJEDE                              *
      * HENTE KOSTNADSTED KJEDE.                                      *
      *****************************************************************
           END-IF
           IF  (I-L1)
               MOVE '    '                 TO KNSTED
               MOVE FIRMNR                 TO FNRAVD (1:3)
               MOVE AVD                    TO FNRAVD (4:1)
               MOVE FNRAVD                 TO KJEDTAB-KEY1
               READ KJEDTAB RECORD KEY IS KJEDTAB-KEY1
               INVALID KEY
                   SET I-56                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-56            TO TRUE
                   PERFORM KJEDTAB-FLDSET
                   PERFORM KJEDTAB-IDSET
               END-READ
           END-IF
           IF  (I-56)
               GO TO ENDKJE-T
           END-IF
           MOVE KJEFNR                     TO REGFNR
           MOVE KJESTE                     TO KNSTED.
 
       ENDKJE-T.
      *****************************************************************
      * RUTINE FOR ENDRING AV KUNDENR. I RESKONTRORECORD.             *
      * LAGT INN FOR: BILVAREHUSET NOR.  28/6/1999                    *
      *****************************************************************
           MOVE RESKNR                     TO KUNDNR
           SET NOT-I-31                    TO TRUE
           IF  FIRMNR = '694'
               SET I-31                    TO TRUE
           END-IF
           IF  (NOT-I-31)
               SET NOT-I-31                TO TRUE
               IF  FIRMNR = '695'
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-31)
               SET NOT-I-31                TO TRUE
               IF  FIRMNR = '696'
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-31)
               SET NOT-I-31                TO TRUE
               IF  FIRMNR = '697'
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-31)
               SET NOT-I-31                TO TRUE
               IF  FIRMNR = '698'
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-31)
               SET NOT-I-31                TO TRUE
               IF  FIRMNR = '699'
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (I-31)
               SET NOT-I-32                TO TRUE
               IF  RESNR1 = '1'
                   SET I-32                TO TRUE
               END-IF
               SET NOT-I-33                TO TRUE
               IF  RESNR1 = '9'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-31 AND I-32 AND NOT-I-09)
               MOVE '11'                   TO KUNDNR (1:2)
           END-IF
           IF  (I-31 AND I-33 AND NOT-I-09)
               MOVE '91'                   TO KUNDNR (1:2)
      *****************************************************************
           END-IF
           SET NOT-I-17                    TO TRUE
           IF  SIGN-X = '-'
               SET I-17                    TO TRUE
           END-IF
           IF  (I-17)
               MULTIPLY -1 BY BEL      GIVING BEL1
           END-IF
           IF  (NOT-I-17)
               MULTIPLY +1 BY BEL      GIVING BEL1
           END-IF
           ADD BEL1                        TO BEL2
           IF  (I-17)
               MULTIPLY -1 BY KOSTPR   GIVING KOST1
           END-IF
           IF  (NOT-I-17)
               MULTIPLY +1 BY KOSTPR   GIVING KOST1
           END-IF
           ADD KOST1                       TO KOST2.
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1)
               ADD BEL2 TO ZERO        GIVING VGBEL
               SET NOT-I-25                TO TRUE
               IF  VGBEL < 0
                   SET I-25                TO TRUE
               END-IF
      ** MLLzo
               IF VGBEL < 0
                   MULTIPLY -1 BY VGBEL
               END-IF
               SUBTRACT BEL2               FROM BEL2
               ADD KOST2 TO ZERO       GIVING VGKOST
      ** MLLzo
               IF VGKOST < 0
                   MULTIPLY -1 BY VGKOST
               END-IF
               SUBTRACT KOST2              FROM KOST2
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
 
       INFILE-FLDOFF SECTION.
       INFILE-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-09                TO TRUE
           END-EVALUATE.
 
       INFILE-FLDSET SECTION.
       INFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INFILE-IO-AREA (3:6)   TO BNR (1:6)
               MOVE INFILE-IO-AREA (9:7)   TO A1 (1:7)
               MOVE INFILE-IO-AREA (16:6)  TO RESKNR (1:6)
               MOVE INFILE-IO-AREA (20:2)  TO RNR2S (1:2)
               IF  RNR2S = SPACES
                   SET I-09                TO TRUE
               END-IF
               MOVE INFILE-IO-AREA (16:1)  TO RESNR1 (1:1)
               MOVE INFILE-IO-AREA (22:3)  TO FIRMNR (1:3)
               MOVE INFILE-IO-AREA (25:7)  TO A2 (1:7)
               MOVE INFILE-IO-AREA (32:5)  TO VGR (1:5)
               MOVE INFILE-IO-AREA (37:9)  TO BEL-IO
               INSPECT BEL-IO REPLACING ALL ' ' BY '0'
               MOVE INFILE-IO-AREA (46:1)  TO SIGN-X (1:1)
               MOVE INFILE-IO-AREA (47:9)  TO KOSTPR-IO
               INSPECT KOSTPR-IO REPLACING ALL ' ' BY '0'
               MOVE INFILE-IO-AREA (56:18) TO A3 (1:18)
               MOVE INFILE-IO-AREA (74:3)  TO HDIST (1:3)
               MOVE INFILE-IO-AREA (77:3)  TO A4 (1:3)
               MOVE INFILE-IO-AREA (80:1)  TO AVD (1:1)
           END-EVALUATE.
 
       INFILE-IDSET SECTION.
       INFILE-IDSET-P.
           SET I-01                        TO TRUE.
 
       INFILE-CHK-LEVEL SECTION.
       INFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO INFILE-LEVEL-01
               MOVE INFILE-IO-AREA (22:3)  TO INFILE-01-L6-FIRMNR
               MOVE INFILE-IO-AREA (16:6)  TO INFILE-01-L5-RESKNR
               MOVE INFILE-IO-AREA (3:6)   TO INFILE-01-L4-BNR
               MOVE INFILE-IO-AREA (74:3)  TO INFILE-01-L3-HDIST
               MOVE INFILE-IO-AREA (32:5)  TO INFILE-01-L2-VGR
               MOVE INFILE-IO-AREA (80:1)  TO INFILE-01-L1-AVD
               IF  INFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INFILE-01-L6 NOT = THE-PRIOR-L6
                       PERFORM SETON-I-L6
                   WHEN  INFILE-01-L5 NOT = THE-PRIOR-L5
                       PERFORM SETON-I-L5
                   WHEN  INFILE-01-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   WHEN  INFILE-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  INFILE-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INFILE-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INFILE-01-L6          TO THE-PRIOR-L6
               MOVE  INFILE-01-L5          TO THE-PRIOR-L5
               MOVE  INFILE-01-L4          TO THE-PRIOR-L4
               MOVE  INFILE-01-L3          TO THE-PRIOR-L3
               MOVE  INFILE-01-L2          TO THE-PRIOR-L2
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
           SET I-03                        TO TRUE.
 
       KJEDTAB-FLDSET SECTION.
       KJEDTAB-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KJEDTAB-IO-AREA (5:3)  TO KJEFNR (1:3)
               MOVE KJEDTAB-IO-AREA (8:4)  TO KJESTE (1:4)
           END-EVALUATE.
 
       KJEDTAB-IDSET SECTION.
       KJEDTAB-IDSET-P.
           SET I-04                        TO TRUE.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1)
               MOVE SPACES TO OUTFILE-IO-AREA
               INITIALIZE OUTFILE-IO-AREA
               MOVE '01'                   TO OUTFILE-IO-AREA (1:2)
               MOVE BNR                    TO OUTFILE-IO-AREA (3:6)
               MOVE A1                     TO OUTFILE-IO-AREA (9:7)
               MOVE KUNDNR                 TO OUTFILE-IO-AREA (16:6)
               MOVE REGFNR                 TO OUTFILE-IO-AREA (22:3)
               MOVE A2                     TO OUTFILE-IO-AREA (25:7)
               MOVE VGR                    TO OUTFILE-IO-AREA (32:5)
               MOVE VGBEL-IO               TO OUTFILE-IO-AREA (37:9)
               INITIALIZE VGBEL-IO
               IF  (I-25)
                   MOVE '-'                TO OUTFILE-IO-AREA (46:1)
               END-IF
               MOVE VGKOST-IO              TO OUTFILE-IO-AREA (47:9)
               INITIALIZE VGKOST-IO
               MOVE A3                     TO OUTFILE-IO-AREA (56:18)
               MOVE HDIST                  TO OUTFILE-IO-AREA (74:3)
               MOVE A4                     TO OUTFILE-IO-AREA (77:3)
               MOVE AVD                    TO OUTFILE-IO-AREA (80:1)
               MOVE KNSTED                 TO OUTFILE-IO-AREA (81:4)
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
           SET INFILE-LEVEL-INIT           TO TRUE
           INITIALIZE INFILE-DATA-FIELDS
           SET INFILE-EOF-OFF              TO TRUE
           SET INFILE-PROCESS              TO TRUE
           OPEN INPUT INFILE
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           INITIALIZE KJEDTAB-DATA-FIELDS
           OPEN INPUT KJEDTAB
           OPEN OUTPUT OUTFILE.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INFILE
           CLOSE FIRMAF
           CLOSE KJEDTAB
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
