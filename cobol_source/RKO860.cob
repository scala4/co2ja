       IDENTIFICATION DIVISION.
       PROGRAM-ID. RKO860R.
      **********************************************  Z-WIN-RPG2P     *
      *    KONV. IFRA RSK860 UTVIDET RECORD.     ***TXT***OK SS***    *
      *  PROGRAM.......: RKO860, DANNER KONTOKURANT KEY-FILE.         *
      *  PROGRAMMERER..: MORTEN TUVRØNNINGEN                          *
      *  KJØRES I JOBB.: DOP53L                                       *
      *  LAGET DATO....: 17.06.99                                     *
      *  E 03.01.17  BH: ENDRET LENGDE PÅ LINJENR I RESKHIS           *
      *  E 10.02.02    : DANNER KEY FOR ALLE RESKHIS-RECS.            *
      *  E 10.02.02    : NY KEY MED BILNR                             *
      *  E 01.09.06    : NY KEY MED BILNR/RESKNR FOR SPØRRING I HBOK  *
      *  INPUT.........: KONTOKURANT (RESKHIS).                       *
      *  BEHANDLING....: FLYTTER KEY-FELT INN I KEY-FILE.             *
      *                  DANNER SØKE-KEY PÅ REFNR FOR LEVERANDØRER.   *
      *                  UTELUKKER SALDORECORDS (0 I REFNR) OG RECORDS*
      *                  MED BLANK I BILAGSART.                       *
      *  OUTPUT........: RESKHIS.KEY.FILE (REKOKEY).                  *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RKO860.rpg
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
           SELECT RESKHIS
               ASSIGN TO RESKHIS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS RESKHIS-STATUS
               RECORD KEY IS RESKHIS-KEY1.
           SELECT REKOKEY
               ASSIGN TO UT-S-REKOKEY
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS REKOKEY-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD RESKHIS
               RECORD CONTAINS 120.
       01  RESKHIS-IO-AREA.
           05  RESKHIS-IO-AREA-X.
               10  RESKHIS-KEY1.
                   15  RESKHIS-KEY1N       PICTURE S9(16).
               10  FILLER                  PICTURE X(104).
       FD REKOKEY
               BLOCK CONTAINS 250
               RECORD CONTAINS 25.
       01  REKOKEY-IO-AREA.
           05  REKOKEY-IO-AREA-X           PICTURE X(25).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  RESKHIS-STATUS              PICTURE 99 VALUE 0.
           10  REKOKEY-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  RESKHIS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKHIS-EOF-OFF         VALUE '0'.
               88  RESKHIS-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKHIS-READ-OFF        VALUE '0'.
               88  RESKHIS-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKHIS-PROCESS-OFF     VALUE '0'.
               88  RESKHIS-PROCESS         VALUE '1'.
           05  RESKHIS-DATA-FIELDS.
               10  RESFIR-IO.
                   15  RESFIR              PICTURE S9(3).
               10  RESRNR-IO.
                   15  RESRNR              PICTURE S9(6).
               10  RESR1                   PICTURE X(1).
               10  RESLNR-IO.
                   15  RESLNR              PICTURE S9(7).
               10  RESBDT-IO.
                   15  RESBDT              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  RESBNR-IO.
                   15  RESBNR              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  RESREF-IO.
                   15  RESREF              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  RESBEL-IO.
                   15  RESBEL              PICTURE S9(9)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  RESBAR                  PICTURE X(1).
           05  TEMPORARY-FIELDS.
               10  FIRTYP-IO.
                   15  FIRTYP              PICTURE S9(4).
               10  FIRTY2-IO.
                   15  FIRTY2              PICTURE S9(4).
               10  FIRTY3-IO.
                   15  FIRTY3              PICTURE S9(4).
               10  REFRNR-IO.
                   15  REFRNR              PICTURE S9(12).
               10  RNRREF-IO.
                   15  RNRREF              PICTURE S9(12).
               10  BNRLNR-IO.
                   15  BNRLNR              PICTURE S9(13).
               10  BNRRNR-IO.
                   15  BNRRNR              PICTURE S9(13).
               10  BDTLNR-IO.
                   15  BDTLNR              PICTURE S9(13).
           05  EDITTING-FIELDS.
               10  XO-40P-EF.
                 15  XO-40P                PICTURE S9(4) USAGE
                                                       PACKED-DECIMAL.
               10  XO-120P-EF.
                 15  XO-120P               PICTURE S9(12) USAGE
                                                       PACKED-DECIMAL.
               10  XO-130P-EF.
                 15  XO-130P               PICTURE S9(13) USAGE
                                                       PACKED-DECIMAL.
               10  XO-92P-EF.
                 15  XO-92P                PICTURE S9(9)V9(2) USAGE
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
           IF  RESKHIS-PROCESS
               SET RESKHIS-PROCESS-OFF     TO TRUE
               SET RESKHIS-READ            TO TRUE
           END-IF
 
           IF  RESKHIS-READ
           AND RECORD-SELECTED-OFF
               PERFORM RESKHIS-GET
               SET RESKHIS-READ-OFF        TO TRUE
               IF  NOT RESKHIS-EOF
                   SET RESKHIS-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  RESKHIS-PROCESS
               PERFORM RESKHIS-IDSET
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
 
           IF  RESKHIS-PROCESS
               PERFORM RESKHIS-FLDSET
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
           SET NOT-I-10                    TO TRUE
           SET NOT-I-11                    TO TRUE
           SET NOT-I-12                    TO TRUE
           SET NOT-I-10                    TO TRUE
           IF  RESR1 > '7'
               SET I-10                    TO TRUE
           END-IF
           IF  (NOT-I-10)
               SET NOT-I-12                TO TRUE
               IF  RESR1 = '2'
                   SET I-12                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-10 AND NOT-I-12)
               SET I-11                    TO TRUE
      *          RESR1     COMP "9"                      10
      * N10                GOTO SLUTT
      *          RESREF    COMP 0                    10
      * N10                GOTO SLUTT
      *          RESBAR    COMP "0"                  10  10
      * N10                GOTO SLUTT
           END-IF
           MULTIPLY 10 BY RESFIR       GIVING FIRTYP
           ADD 1                           TO FIRTYP
           ADD 1 TO FIRTYP             GIVING FIRTY2
           ADD 1 TO FIRTY2             GIVING FIRTY3
           MULTIPLY 1000000 BY RESREF  GIVING REFRNR
           ADD RESRNR                      TO REFRNR
           MULTIPLY 1000000 BY RESRNR  GIVING RNRREF
           ADD RESREF                      TO RNRREF
           MULTIPLY 10000000 BY RESBNR GIVING BNRLNR
           ADD RESLNR                      TO BNRLNR
           SET NOT-I-13                    TO TRUE
           IF  RESBNR > 0
               SET I-13                    TO TRUE
           END-IF
           IF  (NOT-I-13)
               GO TO SLUTT-T
           END-IF
           MULTIPLY 10000000 BY RESBNR GIVING BNRRNR
           ADD RESRNR                      TO BNRRNR
           MULTIPLY 10000000 BY RESBDT GIVING BDTLNR
           ADD RESLNR                      TO BDTLNR.
 
       SLUTT-T.
      *EKOKEY D        01 10
           CONTINUE.
 
       RESKHIS-GET SECTION.
       RESKHIS-GET-P.
           IF  RESKHIS-EOF-OFF
               READ RESKHIS
               AT END
                   SET RESKHIS-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RESKHIS-FLDSET SECTION.
       RESKHIS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RESKHIS-IO-AREA (1:3)  TO RESFIR-IO
               INSPECT RESFIR-IO REPLACING ALL ' ' BY '0'
               MOVE RESKHIS-IO-AREA (4:6)  TO RESRNR-IO
               INSPECT RESRNR-IO REPLACING ALL ' ' BY '0'
               MOVE RESKHIS-IO-AREA (4:1)  TO RESR1 (1:1)
               MOVE RESKHIS-IO-AREA (10:7) TO RESLNR-IO
               INSPECT RESLNR-IO REPLACING ALL ' ' BY '0'
               MOVE RESKHIS-IO-AREA (19:4) TO RESBDT-IO
               MOVE RESKHIS-IO-AREA (23:4) TO RESBNR-IO
               MOVE RESKHIS-IO-AREA (27:4) TO RESREF-IO
               MOVE RESKHIS-IO-AREA (35:6) TO RESBEL-IO
               MOVE RESKHIS-IO-AREA (49:1) TO RESBAR (1:1)
           END-EVALUATE.
 
       RESKHIS-IDSET SECTION.
       RESKHIS-IDSET-P.
           SET I-01                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               MOVE SPACES TO REKOKEY-IO-AREA
               INITIALIZE REKOKEY-IO-AREA
               MOVE FIRTYP                 TO XO-40P
               MOVE XO-40P-EF              TO REKOKEY-IO-AREA (1:3)
               IF  (I-10)
                   MOVE 'L'                TO REKOKEY-IO-AREA (4:1)
               END-IF
               IF  (I-11)
                   MOVE 'K'                TO REKOKEY-IO-AREA (4:1)
               END-IF
               IF  (I-12)
                   MOVE 'A'                TO REKOKEY-IO-AREA (4:1)
               END-IF
               MOVE REFRNR                 TO XO-120P
               MOVE XO-120P-EF             TO REKOKEY-IO-AREA (5:7)
               MOVE RESBAR                 TO REKOKEY-IO-AREA (12:1)
               MOVE BNRLNR                 TO XO-130P
               MOVE XO-130P-EF             TO REKOKEY-IO-AREA (13:7)
               MOVE RESBEL                 TO XO-92P
               MOVE XO-92P-EF              TO REKOKEY-IO-AREA (20:6)
      *       D        01 10
               WRITE REKOKEY-IO-AREA
               MOVE SPACES TO REKOKEY-IO-AREA
               INITIALIZE REKOKEY-IO-AREA
               MOVE FIRTY2                 TO XO-40P
               MOVE XO-40P-EF              TO REKOKEY-IO-AREA (1:3)
               IF  (I-10)
                   MOVE 'L'                TO REKOKEY-IO-AREA (4:1)
               END-IF
               IF  (I-11)
                   MOVE 'K'                TO REKOKEY-IO-AREA (4:1)
               END-IF
               IF  (I-12)
                   MOVE 'A'                TO REKOKEY-IO-AREA (4:1)
               END-IF
               MOVE RESBEL                 TO XO-92P
               MOVE XO-92P-EF              TO REKOKEY-IO-AREA (5:6)
               MOVE RNRREF                 TO XO-120P
               MOVE XO-120P-EF             TO REKOKEY-IO-AREA (11:7)
               MOVE RESBAR                 TO REKOKEY-IO-AREA (18:1)
               MOVE BNRLNR                 TO XO-130P
               MOVE XO-130P-EF             TO REKOKEY-IO-AREA (19:7)
               WRITE REKOKEY-IO-AREA
           END-IF
           IF  (I-01 AND I-13)
               MOVE SPACES TO REKOKEY-IO-AREA
               INITIALIZE REKOKEY-IO-AREA
               MOVE FIRTY3                 TO XO-40P
               MOVE XO-40P-EF              TO REKOKEY-IO-AREA (1:3)
               MOVE 'B'                    TO REKOKEY-IO-AREA (4:1)
               MOVE BNRRNR                 TO XO-130P
               MOVE XO-130P-EF             TO REKOKEY-IO-AREA (5:7)
               MOVE RESBAR                 TO REKOKEY-IO-AREA (12:1)
               MOVE BDTLNR                 TO XO-130P
               MOVE XO-130P-EF             TO REKOKEY-IO-AREA (13:7)
               MOVE RESBEL                 TO XO-92P
               MOVE XO-92P-EF              TO REKOKEY-IO-AREA (20:6)
               WRITE REKOKEY-IO-AREA
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
           INITIALIZE RESKHIS-DATA-FIELDS
           SET RESKHIS-EOF-OFF             TO TRUE
           SET RESKHIS-PROCESS             TO TRUE
           OPEN INPUT RESKHIS
           OPEN OUTPUT REKOKEY.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE RESKHIS
           CLOSE REKOKEY.
 
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
