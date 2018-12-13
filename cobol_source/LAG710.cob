       IDENTIFICATION DIVISION.
       PROGRAM-ID. LAG710R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAMMET LESER LAGERSTYRINGSFILE OG SUMMERER TOTALT ANTALL *
      * SOLGT (12 MND) PR. VGR/ALFA. DENNE SUM SAMMEN MED INNPUT FRA *
      * STYTAB FRA SPM DANNER RECORDS TIL PROGRAM STY220.            *
      ****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: LAG710.rpg
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
           SELECT STYTAB
               ASSIGN TO UT-S-STYTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS STYTAB-STATUS.
           SELECT STYFIL
               ASSIGN TO UT-S-STYFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS STYFIL-STATUS.
           SELECT SKATFIL
               ASSIGN TO UT-S-SKATFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS SKATFIL-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD STYTAB
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  STYTAB-IO-AREA.
           05  STYTAB-IO-AREA-X            PICTURE X(80).
       FD STYFIL
               BLOCK CONTAINS 5000
               RECORD CONTAINS 250.
       01  STYFIL-IO-AREA.
           05  STYFIL-IO-AREA-X            PICTURE X(250).
       FD SKATFIL
               BLOCK CONTAINS 4080
               RECORD CONTAINS 80.
       01  SKATFIL-IO-AREA.
           05  SKATFIL-IO-AREA-X           PICTURE X(80).
       WORKING-STORAGE SECTION.
       77  TABSTY-MAX   VALUE 100          PICTURE 9(4) USAGE BINARY.
       77  TABKAT-MAX   VALUE 100          PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  TABSTY-TABLE.
               10  TABSTY-ENTRY
                                           OCCURS 100 TIMES
                                           INDEXED BY TABSTY-I
                                                      TABSTY-S
                                                      TABKAT-I
                                                      TABKAT-S.
                   15  TABSTY              PICTURE X(11).
                   15  TABKAT              PICTURE X(28).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  STYTAB-STATUS               PICTURE 99 VALUE 0.
           10  STYFIL-STATUS               PICTURE 99 VALUE 0.
           10  SKATFIL-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  STYTAB-EOF-OFF          VALUE '0'.
               88  STYTAB-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  STYFIL-EOF-OFF          VALUE '0'.
               88  STYFIL-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  STYFIL-READ-OFF         VALUE '0'.
               88  STYFIL-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  STYFIL-PROCESS-OFF      VALUE '0'.
               88  STYFIL-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  STYFIL-LEVEL-INIT-OFF   VALUE '0'.
               88  STYFIL-LEVEL-INIT       VALUE '1'.
           05  STYFIL-LEVEL-01.
               10  STYFIL-01-L2.
                   15  STYFIL-01-L2-FNR    PICTURE X(3).
               10  STYFIL-01-L1.
                   15  STYFIL-01-L1-ALF    PICTURE X(3).
                   15  STYFIL-01-L1-VGR    PICTURE X(5).
           05  STYFIL-DATA-FIELDS.
               10  FNR                     PICTURE X(3).
               10  ALF                     PICTURE X(3).
               10  VGR                     PICTURE X(5).
      *                                       2   2 IÅR             09
               10  ANT-IO.
                   15  ANT                 PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(8).
           05  TEMPORARY-FIELDS.
               10  SKEY1                   PICTURE X(8).
               10  SKEY                    PICTURE X(11).
               10  ANTL1-IO.
                   15  ANTL1               PICTURE S9(9).
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
           IF  STYFIL-PROCESS
               SET STYFIL-PROCESS-OFF      TO TRUE
               SET STYFIL-READ             TO TRUE
           END-IF
 
           IF  STYFIL-READ
           AND RECORD-SELECTED-OFF
               PERFORM STYFIL-GET
               SET STYFIL-READ-OFF         TO TRUE
               IF  NOT STYFIL-EOF
                   SET STYFIL-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  STYFIL-PROCESS
               PERFORM STYFIL-IDSET
           END-IF
 
           IF  STYFIL-PROCESS
               PERFORM STYFIL-CHK-LEVEL
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
 
           IF  STYFIL-PROCESS
               PERFORM STYFIL-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  STYFIL-PROCESS
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
               MOVE FNR                    TO SKEY1 (1:3)
               MOVE VGR                    TO SKEY1 (4:5)
               MOVE SKEY1                  TO SKEY (1:8)
               MOVE ALF                    TO SKEY (9:3)
               SET NOT-I-20                TO TRUE
               SET TABSTY-S                TO TABSTY-I
               PERFORM WITH TEST AFTER
                       VARYING TABSTY-I FROM 1 BY 1
                         UNTIL TABSTY-I >= TABSTY-MAX
                            OR I-20
                   IF  SKEY = TABSTY (TABSTY-I)
                       SET I-20            TO TRUE
                       SET TABSTY-S        TO TABSTY-I
                   END-IF
               END-PERFORM
               SET TABSTY-I                TO TABSTY-S
               IF  I-20
               AND TABSTY-I NOT > TABKAT-MAX
                   SET TABKAT-I            TO TABSTY-I
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-20)
               MOVE '       '              TO SKEY (5:7)
               SET NOT-I-20                TO TRUE
               SET TABSTY-S                TO TABSTY-I
               PERFORM WITH TEST AFTER
                       VARYING TABSTY-I FROM 1 BY 1
                         UNTIL TABSTY-I >= TABSTY-MAX
                            OR I-20
                   IF  SKEY = TABSTY (TABSTY-I)
                       SET I-20            TO TRUE
                       SET TABSTY-S        TO TABSTY-I
                   END-IF
               END-PERFORM
               SET TABSTY-I                TO TABSTY-S
               IF  I-20
               AND TABSTY-I NOT > TABKAT-MAX
                   SET TABKAT-I            TO TABSTY-I
               END-IF
           END-IF
           IF  (I-01 AND I-20)
               ADD ANT                     TO ANTL1
      *  01 20 09ANTL1     ADD  ANT       ANTL1   90        SUMMERING. HVIS GML.
           END-IF
           .
 
       STYFIL-GET SECTION.
       STYFIL-GET-P.
           IF  STYFIL-EOF-OFF
               READ STYFIL
               AT END
                   SET STYFIL-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       STYFIL-FLDSET SECTION.
       STYFIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE STYFIL-IO-AREA (3:3)   TO FNR (1:3)
               MOVE STYFIL-IO-AREA (13:3)  TO ALF (1:3)
               MOVE STYFIL-IO-AREA (94:5)  TO VGR (1:5)
               MOVE STYFIL-IO-AREA (204:5) TO ANT-IO
           END-EVALUATE.
 
       STYFIL-IDSET SECTION.
       STYFIL-IDSET-P.
           SET I-01                        TO TRUE.
 
       STYFIL-CHK-LEVEL SECTION.
       STYFIL-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO STYFIL-LEVEL-01
               MOVE STYFIL-IO-AREA (3:3)   TO STYFIL-01-L2-FNR
               MOVE STYFIL-IO-AREA (13:3)  TO STYFIL-01-L1-ALF
               MOVE STYFIL-IO-AREA (94:5)  TO STYFIL-01-L1-VGR
               IF  STYFIL-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  STYFIL-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  STYFIL-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  STYFIL-01-L2          TO THE-PRIOR-L2
               MOVE  STYFIL-01-L1          TO THE-PRIOR-L1
               SET STYFIL-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       STYTAB-LOAD SECTION.
       STYTAB-LOAD-P.
           OPEN INPUT STYTAB
           SET TABSTY-I                    TO 1
           PERFORM UNTIL STYTAB-EOF
               READ STYTAB
               AT END
                   SET STYTAB-EOF          TO TRUE
               NOT AT END
                   MOVE STYTAB-IO-AREA (1:39) TO TABSTY-ENTRY
                                                            (TABSTY-I)
                   SET TABSTY-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE STYTAB.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-20)
               MOVE SPACES TO SKATFIL-IO-AREA
               INITIALIZE SKATFIL-IO-AREA
               MOVE 'S'                    TO SKATFIL-IO-AREA (1:1)
               MOVE FNR                    TO SKATFIL-IO-AREA (2:3)
               MOVE VGR                    TO SKATFIL-IO-AREA (5:5)
               MOVE ALF                    TO SKATFIL-IO-AREA (10:3)
               MOVE TABKAT (TABKAT-I)      TO SKATFIL-IO-AREA (13:28)
               MOVE ANTL1                  TO XO-90P
               MOVE XO-90P-EF              TO SKATFIL-IO-AREA (76:5)
               INITIALIZE ANTL1
               WRITE SKATFIL-IO-AREA
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
           PERFORM STYTAB-LOAD
           SET STYFIL-LEVEL-INIT           TO TRUE
           INITIALIZE STYFIL-DATA-FIELDS
           SET STYFIL-EOF-OFF              TO TRUE
           SET STYFIL-PROCESS              TO TRUE
           OPEN INPUT STYFIL
           OPEN OUTPUT SKATFIL.
           SET TABSTY-I                    TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE STYFIL
           CLOSE SKATFIL.
 
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
