       IDENTIFICATION DIVISION.
       PROGRAM-ID. RSK718R.
      **********************************************  Z-WIN-RPG2   ****
      *   PROGRAM RSK718                                             *
      *   E 22.10.14 RESKONTROPOSTER FRA DINDEL                      *
      *   OUTPUT RECORD:
      *   POS   1 -  2: RECORDART=02
      *         3 -  5: FIRMANR (FIR)
      *         6 - 11: RESKONTRONR (RESKNR)
      *        12 - 13: TRANSAKSJONEKODE (TK)
      *        14 - 19: BILAGSDATO (BILDTO)
      *        20 - 25: BILAGSNR (BILNR.)
      *        26 - 31: REFERANSENR (REFNR.)
      *        32 - 37: FORFALLSDATO (FFDATO)
      *        38 - 46: BELØP (BELØP----)
      *        47 - 47: FORTEGN (T)
      *        48 - 60: UBRUKT
      *        61 - 62: BETALINGSMÅTE (BM)
      *        63 - 63: UBRUKT
      *        64 - 73: VALUTABELØP (VALBEL----)
      *        74 - 76: VALUTATYPE (VAL)                                ********
      *        77 - 77: BILAGSART (A)                                   ********
      *        78 - 80: UBRUKT                                          ********
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RSK718.rpg
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
           SELECT KORTIN
               ASSIGN TO UT-S-KORTIN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KORTIN-STATUS.
           SELECT RESFIL
               ASSIGN TO UT-S-RESFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RESFIL-STATUS.
           SELECT OUTFIL
               ASSIGN TO UT-S-OUTFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTFIL-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD KORTIN
               BLOCK CONTAINS 8000
               RECORD CONTAINS 200.
       01  KORTIN-IO-AREA.
           05  KORTIN-IO-AREA-X            PICTURE X(200).
       FD RESFIL
               BLOCK CONTAINS 8000
               RECORD CONTAINS 80.
       01  RESFIL-IO-AREA.
           05  RESFIL-IO-AREA-X            PICTURE X(80).
       FD OUTFIL
               BLOCK CONTAINS 8000
               RECORD CONTAINS 80.
       01  OUTFIL-IO-AREA.
           05  OUTFIL-IO-AREA-X            PICTURE X(80).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  KORTIN-STATUS               PICTURE 99 VALUE 0.
           10  RESFIL-STATUS               PICTURE 99 VALUE 0.
           10  OUTFIL-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  KORTIN-EOF-OFF          VALUE '0'.
               88  KORTIN-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KORTIN-READ-OFF         VALUE '0'.
               88  KORTIN-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KORTIN-PROCESS-OFF      VALUE '0'.
               88  KORTIN-PROCESS          VALUE '1'.
           05  KORTIN-DATA-FIELDS.
               10  REC080                  PICTURE X(80).
               10  BDAG                    PICTURE X(2).
               10  BMND                    PICTURE X(2).
               10  BAAR                    PICTURE X(2).
               10  BIL5                    PICTURE X(5).
               10  KR-IO.
                   15  KR                  PICTURE S9(7).
               10  ORE                     PICTURE X(2).
               10  TEGN                    PICTURE X(1).
               10  TEKST                   PICTURE X(4).
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
           IF  KORTIN-PROCESS
               SET KORTIN-PROCESS-OFF      TO TRUE
               SET KORTIN-READ             TO TRUE
           END-IF
 
           IF  KORTIN-READ
           AND RECORD-SELECTED-OFF
               PERFORM KORTIN-GET
               SET KORTIN-READ-OFF         TO TRUE
               IF  NOT KORTIN-EOF
                   SET KORTIN-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  KORTIN-PROCESS
               PERFORM KORTIN-IDSET
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
 
           IF  KORTIN-PROCESS
               PERFORM KORTIN-FLDSET
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
           IF  TEGN = '-'
               SET I-10                    TO TRUE
           END-IF
           SET NOT-I-13                    TO TRUE
           IF  TEKST = 'Fakt'
               SET I-13                    TO TRUE
           END-IF
           SET NOT-I-14                    TO TRUE
           IF  TEKST = 'Kred'
               SET I-14                    TO TRUE
           END-IF
           SET NOT-I-15                    TO TRUE
           IF  TEKST = '20 d'
               SET I-15                    TO TRUE
           END-IF.
 
       KORTIN-GET SECTION.
       KORTIN-GET-P.
           IF  KORTIN-EOF-OFF
               READ KORTIN
               AT END
                   SET KORTIN-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KORTIN-FLDSET SECTION.
       KORTIN-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KORTIN-IO-AREA (1:80)  TO REC080 (1:80)
               MOVE KORTIN-IO-AREA (1:2)   TO BDAG (1:2)
               MOVE KORTIN-IO-AREA (4:2)   TO BMND (1:2)
               MOVE KORTIN-IO-AREA (9:2)   TO BAAR (1:2)
               MOVE KORTIN-IO-AREA (11:5)  TO BIL5 (1:5)
               MOVE KORTIN-IO-AREA (37:7)  TO KR-IO
               INSPECT KR-IO REPLACING ALL ' ' BY '0'
               MOVE KORTIN-IO-AREA (45:2)  TO ORE (1:2)
               MOVE KORTIN-IO-AREA (31:1)  TO TEGN (1:1)
               MOVE KORTIN-IO-AREA (47:4)  TO TEKST (1:4)
           END-EVALUATE.
 
       KORTIN-IDSET SECTION.
       KORTIN-IDSET-P.
           SET I-01                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               MOVE SPACES TO RESFIL-IO-AREA
               INITIALIZE RESFIL-IO-AREA
               MOVE '02511'                TO RESFIL-IO-AREA (1:5)
      *                                   5 "02998"
               MOVE '100010'               TO RESFIL-IO-AREA (6:6)
               IF  (I-15)
                   MOVE '100011'           TO RESFIL-IO-AREA (6:6)
               END-IF
               MOVE '01'                   TO RESFIL-IO-AREA (12:2)
               IF  (I-14)
                   MOVE '11'               TO RESFIL-IO-AREA (12:2)
               END-IF
               IF  (I-15)
                   MOVE '01'               TO RESFIL-IO-AREA (12:2)
               END-IF
               MOVE BDAG                   TO RESFIL-IO-AREA (14:2)
               MOVE BMND                   TO RESFIL-IO-AREA (16:2)
               MOVE BAAR                   TO RESFIL-IO-AREA (18:2)
               MOVE '5'                    TO RESFIL-IO-AREA (20:1)
               MOVE BIL5                   TO RESFIL-IO-AREA (21:5)
               MOVE '5'                    TO RESFIL-IO-AREA (26:1)
               MOVE BIL5                   TO RESFIL-IO-AREA (27:5)
               MOVE BDAG                   TO RESFIL-IO-AREA (32:2)
               MOVE BMND                   TO RESFIL-IO-AREA (34:2)
               MOVE BAAR                   TO RESFIL-IO-AREA (36:2)
               MOVE KR-IO                  TO RESFIL-IO-AREA (38:7)
               MOVE ORE                    TO RESFIL-IO-AREA (45:2)
               MOVE TEGN                   TO RESFIL-IO-AREA (47:1)
      *                                  62 "07"
               WRITE RESFIL-IO-AREA
           END-IF
           IF  (I-01)
               MOVE SPACES TO OUTFIL-IO-AREA
               INITIALIZE OUTFIL-IO-AREA
               MOVE ' '                    TO OUTFIL-IO-AREA (9:1)
               MOVE BDAG                   TO OUTFIL-IO-AREA (17:2)
               MOVE BMND                   TO OUTFIL-IO-AREA (19:2)
               MOVE BAAR                   TO OUTFIL-IO-AREA (21:2)
               MOVE '1'                    TO OUTFIL-IO-AREA (26:1)
               MOVE KR-IO                  TO OUTFIL-IO-AREA (30:7)
               MOVE ORE                    TO OUTFIL-IO-AREA (37:2)
               IF  (I-14)
                   MOVE '000000000'        TO OUTFIL-IO-AREA (30:9)
               END-IF
               IF  (I-10)
                   MOVE '000000000'        TO OUTFIL-IO-AREA (30:9)
               END-IF
               MOVE '000000000'            TO OUTFIL-IO-AREA (40:9)
               IF  (I-14)
                   MOVE KR-IO              TO OUTFIL-IO-AREA (40:7)
               END-IF
               IF  (I-14)
                   MOVE ORE                TO OUTFIL-IO-AREA (47:2)
               END-IF
               IF  (I-10)
                   MOVE KR-IO              TO OUTFIL-IO-AREA (40:7)
               END-IF
               IF  (I-10)
                   MOVE ORE                TO OUTFIL-IO-AREA (47:2)
               END-IF
               MOVE '100010'               TO OUTFIL-IO-AREA (55:6)
               IF  (I-15)
                   MOVE '100011'           TO OUTFIL-IO-AREA (55:6)
               END-IF
               MOVE '500000'               TO OUTFIL-IO-AREA (11:6)
               MOVE BIL5                   TO OUTFIL-IO-AREA (12:5)
               MOVE BDAG                   TO OUTFIL-IO-AREA (49:2)
               MOVE BMND                   TO OUTFIL-IO-AREA (51:2)
               MOVE BAAR                   TO OUTFIL-IO-AREA (53:2)
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
           INITIALIZE KORTIN-DATA-FIELDS
           SET KORTIN-EOF-OFF              TO TRUE
           SET KORTIN-PROCESS              TO TRUE
           OPEN INPUT KORTIN
           OPEN OUTPUT RESFIL
           OPEN OUTPUT OUTFIL.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE KORTIN
           CLOSE RESFIL
           CLOSE OUTFIL.
 
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
