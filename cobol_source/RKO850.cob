       IDENTIFICATION DIVISION.
       PROGRAM-ID. RKO850R.
      **********************************************  Z-WIN-RPG2   ****
      * NY VERSJON AV RSK.RSK850                 ***TXT***OK MT
      *  PROGRAM.......: RKO850, DANNER RESKONTRO KEY-FILE PÅ BILAGS- *
      *                          DATO.                                *
      *  PROGRAMMERER..: MORTEN TUVRØNNINGEN                          *
      *  KJØRES I JOBB.: DOP53K                                       *
      *  LAGET DATO....: 19.08.96                                     *
      *  ENDR 22.09.05 : FRA RSK850, NY RECORDLENGDE FRA 89->200      *
      *  E    22.09.05 : FORFALLSDATO MED I KEYFILE                   *
      *  INPUT.........: RESKONTROMASTER (RESKMAS).                   *
      *  BEHANDLING....: FLYTTER KEY-FELT INN I KEY-FILE.             *
      *  OUTPUT........: RESKONTRO KEY FILE (RESKKEY).                *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RKO850.rpg
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
           SELECT RESKOMA
               ASSIGN TO RESKOMA
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS RESKOMA-STATUS
               RECORD KEY IS RESKOMA-KEY1.
           SELECT RESKKEY
               ASSIGN TO UT-S-RESKKEY
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RESKKEY-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD RESKOMA
               RECORD CONTAINS 200.
       01  RESKOMA-IO-AREA.
           05  RESKOMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  RESKOMA-KEY1.
                   15  RESKOMA-KEY1N       PICTURE S9(14).
               10  FILLER                  PICTURE X(185).
       FD RESKKEY
               BLOCK CONTAINS 240
               RECORD CONTAINS 24.
       01  RESKKEY-IO-AREA.
           05  RESKKEY-IO-AREA-X           PICTURE X(24).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  RESKOMA-STATUS              PICTURE 99 VALUE 0.
           10  RESKKEY-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  RESKOMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKOMA-EOF-OFF         VALUE '0'.
               88  RESKOMA-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKOMA-READ-OFF        VALUE '0'.
               88  RESKOMA-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKOMA-PROCESS-OFF     VALUE '0'.
               88  RESKOMA-PROCESS         VALUE '1'.
           05  RESKOMA-DATA-FIELDS.
               10  RESFIR                  PICTURE X(3).
               10  RESNR-IO.
                   15  RESNR               PICTURE S9(6).
               10  RESLNR-IO.
                   15  RESLNR              PICTURE S9(5).
               10  RESBDT                  PICTURE X(6).
               10  RESBAR                  PICTURE X(2).
               10  RESBNR-IO.
                   15  RESBNR              PICTURE S9(6).
               10  RESFAR                  PICTURE X(2).
               10  RESFDT                  PICTURE X(6).
               10  RESBA                   PICTURE X(1).
           05  TEMPORARY-FIELDS.
               10  DATO8                   PICTURE X(8).
               10  BILDTO-IO.
                   15  BILDTO              PICTURE S9(8).
               10  FFDATO-IO.
                   15  FFDATO              PICTURE S9(8).
           05  EDITTING-FIELDS.
               10  XO-60P-EF.
                 15  XO-60P                PICTURE S9(6) USAGE
                                                       PACKED-DECIMAL.
               10  XO-80P-EF.
                 15  XO-80P                PICTURE S9(8) USAGE
                                                       PACKED-DECIMAL.
               10  XO-50P-EF.
                 15  XO-50P                PICTURE S9(5) USAGE
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
           SET NOT-I-01                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  RESKOMA-PROCESS
               SET RESKOMA-PROCESS-OFF     TO TRUE
               SET RESKOMA-READ            TO TRUE
           END-IF
 
           IF  RESKOMA-READ
           AND RECORD-SELECTED-OFF
               PERFORM RESKOMA-GET
               SET RESKOMA-READ-OFF        TO TRUE
               IF  NOT RESKOMA-EOF
                   SET RESKOMA-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  RESKOMA-PROCESS
               PERFORM RESKOMA-IDSET
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
 
           IF  RESKOMA-PROCESS
               PERFORM RESKOMA-FLDSET
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
           SET NOT-I-20                    TO TRUE
           SET NOT-I-21                    TO TRUE
           IF  RESBAR NOT < '80'
               SET I-20                    TO TRUE
           END-IF
           IF  RESBAR < '80'
               SET I-21                    TO TRUE
           END-IF
           MOVE RESBDT                     TO DATO8 (3:6)
           IF  (I-20)
               MOVE '19'                   TO DATO8 (1:2)
           END-IF
           IF  (I-21)
               MOVE '20'                   TO DATO8 (1:2)
           END-IF
           MOVE DATO8                      TO BILDTO-IO
           SET NOT-I-30                    TO TRUE
           SET NOT-I-31                    TO TRUE
           IF  RESFAR NOT < '80'
               SET I-30                    TO TRUE
           END-IF
           IF  RESFAR < '80'
               SET I-31                    TO TRUE
           END-IF
           MOVE RESFDT                     TO DATO8 (3:6)
           IF  (I-30)
               MOVE '19'                   TO DATO8 (1:2)
           END-IF
           IF  (I-31)
               MOVE '20'                   TO DATO8 (1:2)
           END-IF
           MOVE DATO8                      TO FFDATO-IO.
 
       RESKOMA-GET SECTION.
       RESKOMA-GET-P.
           IF  RESKOMA-EOF-OFF
               READ RESKOMA
               AT END
                   SET RESKOMA-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RESKOMA-FLDSET SECTION.
       RESKOMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RESKOMA-IO-AREA (3:3)  TO RESFIR (1:3)
               MOVE RESKOMA-IO-AREA (6:6)  TO RESNR-IO
               INSPECT RESNR-IO REPLACING ALL ' ' BY '0'
               MOVE RESKOMA-IO-AREA (12:5) TO RESLNR-IO
               INSPECT RESLNR-IO REPLACING ALL ' ' BY '0'
               MOVE RESKOMA-IO-AREA (24:6) TO RESBDT (1:6)
               MOVE RESKOMA-IO-AREA (24:2) TO RESBAR (1:2)
               MOVE RESKOMA-IO-AREA (30:6) TO RESBNR-IO
               INSPECT RESBNR-IO REPLACING ALL ' ' BY '0'
               MOVE RESKOMA-IO-AREA (42:2) TO RESFAR (1:2)
               MOVE RESKOMA-IO-AREA (42:6) TO RESFDT (1:6)
               MOVE RESKOMA-IO-AREA (74:1) TO RESBA (1:1)
           END-EVALUATE.
 
       RESKOMA-IDSET SECTION.
       RESKOMA-IDSET-P.
           SET I-01                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               MOVE SPACES TO RESKKEY-IO-AREA
               INITIALIZE RESKKEY-IO-AREA
               MOVE RESFIR                 TO RESKKEY-IO-AREA (1:3)
               MOVE '2B'                   TO RESKKEY-IO-AREA (4:2)
               MOVE RESNR                  TO XO-60P
               MOVE XO-60P-EF              TO RESKKEY-IO-AREA (6:4)
               MOVE BILDTO                 TO XO-80P
               MOVE XO-80P-EF              TO RESKKEY-IO-AREA (10:5)
               MOVE RESBA                  TO RESKKEY-IO-AREA (15:1)
               MOVE RESBNR                 TO XO-60P
               MOVE XO-60P-EF              TO RESKKEY-IO-AREA (16:4)
               MOVE RESLNR                 TO XO-50P
               MOVE XO-50P-EF              TO RESKKEY-IO-AREA (20:3)
               MOVE '  '                   TO RESKKEY-IO-AREA (23:2)
               WRITE RESKKEY-IO-AREA
               MOVE SPACES TO RESKKEY-IO-AREA
               INITIALIZE RESKKEY-IO-AREA
               MOVE RESFIR                 TO RESKKEY-IO-AREA (1:3)
               MOVE '2F'                   TO RESKKEY-IO-AREA (4:2)
               MOVE RESNR                  TO XO-60P
               MOVE XO-60P-EF              TO RESKKEY-IO-AREA (6:4)
               MOVE FFDATO                 TO XO-80P
               MOVE XO-80P-EF              TO RESKKEY-IO-AREA (10:5)
               MOVE RESBA                  TO RESKKEY-IO-AREA (15:1)
               MOVE RESBNR                 TO XO-60P
               MOVE XO-60P-EF              TO RESKKEY-IO-AREA (16:4)
               MOVE RESLNR                 TO XO-50P
               MOVE XO-50P-EF              TO RESKKEY-IO-AREA (20:3)
               MOVE '  '                   TO RESKKEY-IO-AREA (23:2)
               WRITE RESKKEY-IO-AREA
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
           INITIALIZE RESKOMA-DATA-FIELDS
           SET RESKOMA-EOF-OFF             TO TRUE
           SET RESKOMA-PROCESS             TO TRUE
           OPEN INPUT RESKOMA
           OPEN OUTPUT RESKKEY.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE RESKOMA
           CLOSE RESKKEY.
 
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
