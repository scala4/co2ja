       IDENTIFICATION DIVISION.
       PROGRAM-ID. STA180R.
      **********************************************  Z-WIN-RPG2   ****
      *  OPPDATERE SALGSTSAT ANT/BEL FRA FAKT.VAREREC.       *
      *  RECORDS MED KODE "PT" I LAGERKODE = PRISTILEGG.     *
      *  DISSE RECORDS SKAL IKKE OPPDATERE ANTALL, DA DETTE  *
      *  VIL FREMKOMME SOM DOBBELT SALG I ANTALL PÅ VAREN.   *
      *  FILEN ER UTVIDET FRA 136 TIL 145 BYTES. 27/8-91     *
      *  DETTE FOR Å FÅ MED SALG DENNE MND IFJOR I ANT. OG   *
      *  BELØP. ANTALL I REST BLIR NÅ FJERNET.               *
      *  SISTE RETTELSE 27/08-1991 ESPEN LARSEN              *
      ********************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: STA180.rpg
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
           SELECT TRANSER
               ASSIGN TO UT-S-TRANSER
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TRANSER-STATUS.
           SELECT GMLMAST
               ASSIGN TO UT-S-GMLMAST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS GMLMAST-STATUS.
           SELECT PARFILE
               ASSIGN TO UT-S-PARFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PARFILE-STATUS.
           SELECT NYMAST
               ASSIGN TO UT-S-NYMAST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS NYMAST-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD TRANSER
               BLOCK CONTAINS 9430
               RECORD CONTAINS 82.
       01  TRANSER-IO-AREA.
           05  TRANSER-IO-AREA-X           PICTURE X(82).
       FD GMLMAST
               BLOCK CONTAINS 4350
               RECORD CONTAINS 145.
       01  GMLMAST-IO-AREA.
           05  GMLMAST-IO-AREA-X           PICTURE X(145).
       FD PARFILE
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  PARFILE-IO-AREA.
           05  PARFILE-IO-AREA-X           PICTURE X(80).
      *
       FD NYMAST
               BLOCK CONTAINS 4350
               RECORD CONTAINS 145.
       01  NYMAST-IO-AREA.
           05  NYMAST-IO-AREA-X            PICTURE X(145).
       WORKING-STORAGE SECTION.
       77  ANI-MAX   VALUE 12              PICTURE 9(4) USAGE BINARY.
       77  ANU-MAX   VALUE 12              PICTURE 9(4) USAGE BINARY.
       77  BLI-MAX   VALUE 12              PICTURE 9(4) USAGE BINARY.
       77  BLU-MAX   VALUE 12              PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  ANI-TABLE.
               10  ANI-ENTRY
                                           OCCURS 12 TIMES
                                           INDEXED BY ANI-I
                                                      ANI-S.
                   15  ANI                 PICTURE S9(5)V9(2).
           05  ANU-TABLE.
               10  ANU-ENTRY
                                           OCCURS 12 TIMES
                                           INDEXED BY ANU-I
                                                      ANU-S.
                   15  ANU                 PICTURE S9(5)V9(2).
           05  BLI-TABLE.
               10  BLI-ENTRY
                                           OCCURS 12 TIMES
                                           INDEXED BY BLI-I
                                                      BLI-S.
                   15  BLI                 PICTURE S9(7)V9(2).
      *
           05  BLU-TABLE.
               10  BLU-ENTRY
                                           OCCURS 12 TIMES
                                           INDEXED BY BLU-I
                                                      BLU-S.
                   15  BLU                 PICTURE S9(7)V9(2).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  TRANSER-STATUS              PICTURE 99 VALUE 0.
           10  GMLMAST-STATUS              PICTURE 99 VALUE 0.
           10  PARFILE-STATUS              PICTURE 99 VALUE 0.
           10  NYMAST-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  TRANSER-EOF-OFF         VALUE '0'.
               88  TRANSER-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  TRANSER-READ-OFF        VALUE '0'.
               88  TRANSER-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  TRANSER-PROCESS-OFF     VALUE '0'.
               88  TRANSER-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  TRANSER-LEVEL-INIT-OFF  VALUE '0'.
               88  TRANSER-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLMAST-EOF-OFF         VALUE '0'.
               88  GMLMAST-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLMAST-READ-OFF        VALUE '0'.
               88  GMLMAST-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLMAST-PROCESS-OFF     VALUE '0'.
               88  GMLMAST-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  GMLMAST-LEVEL-INIT-OFF  VALUE '0'.
               88  GMLMAST-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARFILE-EOF-OFF         VALUE '0'.
               88  PARFILE-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARFILE-READ-OFF        VALUE '0'.
               88  PARFILE-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARFILE-PROCESS-OFF     VALUE '0'.
               88  PARFILE-PROCESS         VALUE '1'.
           05  TRANSER-LEVEL-02.
               10  TRANSER-02-L3.
                   15  TRANSER-02-L3-FNR   PICTURE X(3).
               10  TRANSER-02-L2.
                   15  TRANSER-02-L2-KNR   PICTURE X(6).
               10  TRANSER-02-L1.
                   15  TRANSER-02-L1-VGR   PICTURE X(5).
           05  TRANSER-DATA-FIELDS.
               10  TANT-IO.
                   15  TANT                PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  TRAB1-IO.
                   15  TRAB1               PICTURE S9(2)V9(1).
               10  TRAB2-IO.
                   15  TRAB2               PICTURE S9(2)V9(1).
               10  TRAB3-IO.
                   15  TRAB3               PICTURE S9(2)V9(1).
               10  TPRIS-IO.
                   15  TPRIS               PICTURE S9(7)V9(2).
               10  TFK                     PICTURE X(1).
               10  KNR                     PICTURE X(6).
               10  FNR                     PICTURE X(3).
               10  VGR                     PICTURE X(5).
               10  TKTYP                   PICTURE X(1).
               10  LAGKOD                  PICTURE X(2).
           05  TRANSER-MP                  PICTURE X(14).
           05  TRANSER-MC                  PICTURE X(14).
           05  TRANSER-M-02            REDEFINES TRANSER-MC.
               10  TRANSER-M-02-M3.
                   15  TRANSER-M-02-M3-FNR-G.
                       20  TRANSER-M-02-M3-FNR PICTURE X(3).
               10  TRANSER-M-02-M2.
                   15  TRANSER-M-02-M2-KNR-G.
                       20  TRANSER-M-02-M2-KNR PICTURE X(6).
               10  TRANSER-M-02-M1.
                   15  TRANSER-M-02-M1-VGR-G.
                       20  TRANSER-M-02-M1-VGR PICTURE X(5).
           05  GMLMAST-LEVEL-03.
               10  GMLMAST-03-L3.
                   15  GMLMAST-03-L3-FNR   PICTURE X(3).
               10  GMLMAST-03-L2.
                   15  GMLMAST-03-L2-KNR   PICTURE X(6).
               10  GMLMAST-03-L1.
                   15  GMLMAST-03-L1-VGR   PICTURE X(5).
           05  GMLMAST-DATA-FIELDS.
               10  MAKKFA-IO.
                   15  MAKKFA              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XI-ANI-GRP.
                   15  XI-ANI              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  MAKKFB-IO.
                   15  MAKKFB              PICTURE S9(9)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XI-BLI-GRP.
                   15  XI-BLI              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
           05  GMLMAST-MP                  PICTURE X(14).
           05  GMLMAST-MC                  PICTURE X(14).
           05  GMLMAST-M-03            REDEFINES GMLMAST-MC.
               10  GMLMAST-M-03-M3.
                   15  GMLMAST-M-03-M3-FNR-G.
                       20  GMLMAST-M-03-M3-FNR PICTURE X(3).
               10  GMLMAST-M-03-M2.
                   15  GMLMAST-M-03-M2-KNR-G.
                       20  GMLMAST-M-03-M2-KNR PICTURE X(6).
               10  GMLMAST-M-03-M1.
                   15  GMLMAST-M-03-M1-VGR-G.
                       20  GMLMAST-M-03-M1-VGR PICTURE X(5).
           05  PARFILE-DATA-FIELDS.
               10  PMND                    PICTURE X(2).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(6).
               10  THE-PRIOR-L1            PICTURE X(5).
           05  TEMPORARY-FIELDS.
               10  L1AAFJ-IO.
                   15  L1AAFJ              PICTURE S9(7)V9(2).
               10  L1BAFJ-IO.
                   15  L1BAFJ              PICTURE S9(9)V9(2).
               10  L1ADMF-IO.
                   15  L1ADMF              PICTURE S9(5)V9(2).
               10  L1BDMF-IO.
                   15  L1BDMF              PICTURE S9(7)V9(2).
               10  RES1-IO.
                   15  RES1                PICTURE S9(7)V9(2).
               10  NETTO-IO.
                   15  NETTO               PICTURE S9(7)V9(2).
               10  RES2-IO.
                   15  RES2                PICTURE S9(9)V9(2).
               10  RES3-IO.
                   15  RES3                PICTURE S9(7)V9(2).
               10  L1ANT-IO.
                   15  L1ANT               PICTURE S9(5)V9(2).
               10  L1BEL-IO.
                   15  L1BEL               PICTURE S9(7)V9(2).
               10  X-IO.
                   15  X                   PICTURE S9(2).
               10  Y-IO.
                   15  Y                   PICTURE S9(2).
           05  EDITTING-FIELDS.
               10  XO-72P-EF.
                 15  XO-72P                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-52P-EF.
                 15  XO-52P                PICTURE S9(5)V9(2) USAGE
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
               88  NOT-CALL-MATCH-RECS     VALUE '0'.
               88  CALL-MATCH-RECS         VALUE '1'.
           05  FILLER                      PICTURE X.
               88  NOT-SET-I-MR            VALUE '0'.
               88  SET-I-MR                VALUE '1'.
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
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-01                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  TRANSER-PROCESS
               SET TRANSER-PROCESS-OFF     TO TRUE
               SET TRANSER-READ            TO TRUE
           END-IF
 
           IF  TRANSER-READ
               PERFORM TRANSER-GET
               SET TRANSER-READ-OFF        TO TRUE
               IF  NOT TRANSER-EOF
                   PERFORM TRANSER-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM TRANSER-MATCH-SET
               END-IF
           END-IF
 
           IF  GMLMAST-PROCESS
               SET GMLMAST-PROCESS-OFF     TO TRUE
               SET GMLMAST-READ            TO TRUE
           END-IF
 
           IF  GMLMAST-READ
               PERFORM GMLMAST-GET
               SET GMLMAST-READ-OFF        TO TRUE
               IF  NOT GMLMAST-EOF
                   PERFORM GMLMAST-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM GMLMAST-MATCH-SET
               END-IF
           END-IF
 
           IF  PARFILE-PROCESS
               SET PARFILE-PROCESS-OFF     TO TRUE
               SET PARFILE-READ            TO TRUE
           END-IF
 
           IF  PARFILE-READ
           AND RECORD-SELECTED-OFF
               PERFORM PARFILE-GET
               SET PARFILE-READ-OFF        TO TRUE
               IF  NOT PARFILE-EOF
                   PERFORM PARFILE-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET PARFILE-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  TRANSER-PROCESS
               PERFORM TRANSER-IDSET
           END-IF
 
           IF  GMLMAST-PROCESS
               PERFORM GMLMAST-IDSET
           END-IF
 
           IF  PARFILE-PROCESS
               PERFORM PARFILE-IDSET
           END-IF
 
           IF  TRANSER-PROCESS
               PERFORM TRANSER-CHK-LEVEL
           END-IF
 
           IF  GMLMAST-PROCESS
               PERFORM GMLMAST-CHK-LEVEL
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
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  TRANSER-PROCESS
               PERFORM TRANSER-FLDOFF
               PERFORM TRANSER-FLDSET
           END-IF
 
           IF  GMLMAST-PROCESS
               PERFORM GMLMAST-FLDSET
           END-IF
 
           IF  PARFILE-PROCESS
               PERFORM PARFILE-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  TRANSER-PROCESS
           OR  GMLMAST-PROCESS
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
               SET NOT-I-31                TO TRUE
               SET NOT-I-32                TO TRUE
               SET NOT-I-33                TO TRUE
      *
           END-IF
           IF  (I-01)
               SET NOT-I-21                TO TRUE
               IF  PMND = '01'
                   SET I-21                TO TRUE
               END-IF
               GO TO SLUTT-T
      *
           END-IF
           IF  (I-L1)
               MOVE 0                      TO L1ANT
               MOVE 0                      TO L1BEL
               MOVE 0                      TO L1AAFJ
               MOVE 0                      TO L1BAFJ
               MOVE 0                      TO L1ADMF
               MOVE 0                      TO L1BDMF
               PERFORM VARYING ANU-I FROM 1 BY 1
                         UNTIL ANU-I > ANU-MAX
                   MOVE 0                  TO ANU (ANU-I)
               END-PERFORM
               SET ANU-I                   TO 1
               PERFORM VARYING BLU-I FROM 1 BY 1
                         UNTIL BLU-I > BLU-MAX
                   MOVE 0                  TO BLU (BLU-I)
               END-PERFORM
               SET BLU-I                   TO 1
      *
      * REGNE UT NETTOPRIS FRA RABATTER
      *
           END-IF
           IF  (I-02)
               MULTIPLY TPRIS BY TANT  GIVING RES1 ROUNDED
               SET NOT-I-22                TO TRUE
               IF  RES1 = 0
                   SET I-22                TO TRUE
               END-IF
      * RETTET 03.09.2013 0 I ANTALL = 0 I BELØP.  STEIN SANDVOLD
      *  02 08             Z-ADDTPRIS     RES1           22
           END-IF
           IF  (I-02 AND I-08)
               MOVE 0                      TO RES1
               SET NOT-I-22                TO TRUE
               IF  RES1 = 0
                   SET I-22                TO TRUE
               END-IF
      *
           END-IF
           IF  (I-02 AND I-22)
               MOVE RES1                   TO NETTO-IO
               GO TO A1-T
      *
           END-IF
           IF  (I-02 AND NOT-I-11)
               MULTIPLY TRAB1 BY RES1  GIVING RES2 ROUNDED
               DIVIDE RES2 BY 100      GIVING RES3 ROUNDED
               SUBTRACT RES3 FROM RES1 GIVING NETTO
           END-IF
           IF  (I-02 AND I-11)
               MOVE RES1                   TO NETTO-IO
           END-IF
           IF  (I-02 AND NOT-I-12)
               MOVE NETTO                  TO RES1-IO
               MULTIPLY TRAB2 BY RES1  GIVING RES2 ROUNDED
               DIVIDE RES2 BY 100      GIVING RES3 ROUNDED
               SUBTRACT RES3 FROM RES1 GIVING NETTO
           END-IF
           IF  (I-02 AND NOT-I-13)
               MOVE NETTO                  TO RES1-IO
               MULTIPLY TRAB3 BY RES1  GIVING RES2 ROUNDED
               DIVIDE RES2 BY 100      GIVING RES3 ROUNDED
               SUBTRACT RES3 FROM RES1 GIVING NETTO
           END-IF.
 
       A1-T.
      ******************************************************
      * ADDERE ANTALL OG BELØP                             *
      * RECORDS MED LAGERKODE ="PT" SKAL IKKE ADDERE ANTALL*
      ******************************************************
           IF  (I-02)
               SET NOT-I-20                TO TRUE
               IF  LAGKOD = 'PT'
                   SET I-20                TO TRUE
               END-IF
               SET NOT-I-23                TO TRUE
               IF  TFK = '1'
                   SET I-23                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-23 AND NOT-I-20)
               ADD TANT                    TO L1ANT
           END-IF
           IF  (I-02 AND I-23)
               ADD NETTO                   TO L1BEL
           END-IF
           IF  (I-02 AND NOT-I-23)
               SET NOT-I-24                TO TRUE
               IF  TKTYP = '2'
                   SET I-24                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-23 AND I-24)
               AND (NOT-I-20)
               SUBTRACT TANT               FROM L1ANT
           END-IF
           IF  (I-02 AND NOT-I-23)
               SUBTRACT NETTO              FROM L1BEL
           END-IF
           IF  (I-02)
               GO TO SLUTT-T
      *
      *  FLYTTING TIL NY MASTER, AKKUMULERTE FELTER NULLSTILLES FOR JANUAR.
      *
           END-IF
           MOVE 1                          TO X
           MOVE 0                          TO Y
           IF  (I-03)
               ADD ANI (X)                 TO L1ADMF
               ADD ANI (X)                 TO L1AAFJ
           END-IF
           IF  (I-03 AND NOT-I-21)
               ADD MAKKFA                  TO L1AAFJ
           END-IF
           IF  (I-03)
               ADD BLI (X)                 TO L1BDMF
               ADD BLI (X)                 TO L1BAFJ
           END-IF
           IF  (I-03 AND NOT-I-21)
               ADD MAKKFB                  TO L1BAFJ
      *
           END-IF
           .
 
       LOOP-T.
           IF  (I-03)
               ADD 1                       TO X
               ADD 1                       TO Y
               ADD ANI (X) TO ZERO     GIVING ANU (Y)
               ADD BLI (X) TO ZERO     GIVING BLU (Y)
               SET NOT-I-50                TO TRUE
               IF  X NOT < 12
                   SET I-50                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-50)
               GO TO LOOP-T
      *
           END-IF
           .
 
       SLUTT-T.
      *
      *
           CONTINUE.
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1)
               SET NOT-I-31                TO TRUE
               IF  FNR = '915'
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-31)
               SET NOT-I-33                TO TRUE
               IF  VGR = '80200'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-33)
               GO TO L1END-T
      *
           END-IF
           IF  (I-L1)
               ADD L1ANT TO ZERO       GIVING ANU (12)
               ADD L1BEL TO ZERO       GIVING BLU (12)
               SET NOT-I-32                TO TRUE
               IF  ANU (1) = 0
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-32)
               SET NOT-I-32                TO TRUE
               IF  ANU (2) = 0
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-32)
               SET NOT-I-32                TO TRUE
               IF  ANU (3) = 0
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-32)
               SET NOT-I-32                TO TRUE
               IF  ANU (4) = 0
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-32)
               SET NOT-I-32                TO TRUE
               IF  ANU (5) = 0
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-32)
               SET NOT-I-32                TO TRUE
               IF  ANU (6) = 0
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-32)
               SET NOT-I-32                TO TRUE
               IF  ANU (7) = 0
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-32)
               SET NOT-I-32                TO TRUE
               IF  ANU (8) = 0
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-32)
               SET NOT-I-32                TO TRUE
               IF  ANU (9) = 0
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-32)
               SET NOT-I-32                TO TRUE
               IF  ANU (10) = 0
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-32)
               SET NOT-I-32                TO TRUE
               IF  ANU (11) = 0
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-32)
               SET NOT-I-32                TO TRUE
               IF  ANU (12) = 0
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-32)
               SET NOT-I-32                TO TRUE
               IF  L1AAFJ = 0
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-32)
               GO TO L1END-T
      *
           END-IF
           IF  (I-L1 AND I-32)
               SET NOT-I-33                TO TRUE
               IF  BLU (1) = 0
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-32 AND I-33)
               SET NOT-I-33                TO TRUE
               IF  BLU (2) = 0
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-32 AND I-33)
               SET NOT-I-33                TO TRUE
               IF  BLU (3) = 0
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-32 AND I-33)
               SET NOT-I-33                TO TRUE
               IF  BLU (4) = 0
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-32 AND I-33)
               SET NOT-I-33                TO TRUE
               IF  BLU (5) = 0
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-32 AND I-33)
               SET NOT-I-33                TO TRUE
               IF  BLU (6) = 0
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-32 AND I-33)
               SET NOT-I-33                TO TRUE
               IF  BLU (7) = 0
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-32 AND I-33)
               SET NOT-I-33                TO TRUE
               IF  BLU (8) = 0
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-32 AND I-33)
               SET NOT-I-33                TO TRUE
               IF  BLU (9) = 0
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-32 AND I-33)
               SET NOT-I-33                TO TRUE
               IF  BLU (10) = 0
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-32 AND I-33)
               SET NOT-I-33                TO TRUE
               IF  BLU (11) = 0
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-32 AND I-33)
               SET NOT-I-33                TO TRUE
               IF  BLU (12) = 0
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-32 AND I-33)
               SET NOT-I-33                TO TRUE
               IF  L1BAFJ = 0
                   SET I-33                TO TRUE
               END-IF
           END-IF.
 
       L1END-T.
      *
           CONTINUE.
 
       TRANSER-GET SECTION.
       TRANSER-GET-P.
           IF  TRANSER-EOF-OFF
               READ TRANSER
               AT END
                   SET TRANSER-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       TRANSER-FLDOFF SECTION.
       TRANSER-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( TRANSER-IO-AREA (1:1) = '8' )
               SET NOT-I-08                TO TRUE
               SET NOT-I-11                TO TRUE
               SET NOT-I-12                TO TRUE
               SET NOT-I-13                TO TRUE
               SET NOT-I-09                TO TRUE
           END-EVALUATE.
 
       TRANSER-FLDSET SECTION.
       TRANSER-FLDSET-P.
           EVALUATE TRUE
           WHEN ( TRANSER-IO-AREA (1:1) = '8' )
               MOVE TRANSER-IO-AREA (12:4) TO TANT-IO
               IF  TANT = ZERO
                   SET I-08                TO TRUE
               END-IF
               MOVE TRANSER-IO-AREA (23:3) TO TRAB1-IO
               INSPECT TRAB1-IO REPLACING ALL ' ' BY '0'
               IF  TRAB1 = ZERO
                   SET I-11                TO TRUE
               END-IF
               MOVE TRANSER-IO-AREA (26:3) TO TRAB2-IO
               INSPECT TRAB2-IO REPLACING ALL ' ' BY '0'
               IF  TRAB2 = ZERO
                   SET I-12                TO TRUE
               END-IF
               MOVE TRANSER-IO-AREA (29:3) TO TRAB3-IO
               INSPECT TRAB3-IO REPLACING ALL ' ' BY '0'
               IF  TRAB3 = ZERO
                   SET I-13                TO TRUE
               END-IF
               MOVE TRANSER-IO-AREA (32:9) TO TPRIS-IO
               INSPECT TPRIS-IO REPLACING ALL ' ' BY '0'
               IF  TPRIS = ZERO
                   SET I-09                TO TRUE
               END-IF
               MOVE TRANSER-IO-AREA (41:1) TO TFK (1:1)
               MOVE TRANSER-IO-AREA (45:6) TO KNR (1:6)
               MOVE TRANSER-IO-AREA (51:3) TO FNR (1:3)
               MOVE TRANSER-IO-AREA (60:5) TO VGR (1:5)
               MOVE TRANSER-IO-AREA (66:1) TO TKTYP (1:1)
               MOVE TRANSER-IO-AREA (69:2) TO LAGKOD (1:2)
           END-EVALUATE.
 
       TRANSER-IDCHK SECTION.
       TRANSER-IDCHK-P.
           EVALUATE TRUE
           WHEN ( TRANSER-IO-AREA (1:1) = '8' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       TRANSER-IDSET SECTION.
       TRANSER-IDSET-P.
           EVALUATE TRUE
           WHEN ( TRANSER-IO-AREA (1:1) = '8' )
               SET I-02                    TO TRUE
           END-EVALUATE.
 
       TRANSER-CHK-LEVEL SECTION.
       TRANSER-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( TRANSER-IO-AREA (1:1) = '8' )
               MOVE LOW-VALUES             TO TRANSER-LEVEL-02
               MOVE TRANSER-IO-AREA (51:3) TO TRANSER-02-L3-FNR
               MOVE TRANSER-IO-AREA (45:6) TO TRANSER-02-L2-KNR
               MOVE TRANSER-IO-AREA (60:5) TO TRANSER-02-L1-VGR
               IF  TRANSER-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  TRANSER-02-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  TRANSER-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  TRANSER-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  TRANSER-02-L3         TO THE-PRIOR-L3
               MOVE  TRANSER-02-L2         TO THE-PRIOR-L2
               MOVE  TRANSER-02-L1         TO THE-PRIOR-L1
               SET TRANSER-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       TRANSER-MATCH-SET SECTION.
       TRANSER-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( TRANSER-IO-AREA (1:1) = '8' )
               MOVE TRANSER-IO-AREA (51:3) TO TRANSER-M-02-M3-FNR
               MOVE TRANSER-IO-AREA (45:6) TO TRANSER-M-02-M2-KNR
               MOVE TRANSER-IO-AREA (60:5) TO TRANSER-M-02-M1-VGR
           END-EVALUATE.
 
       GMLMAST-GET SECTION.
       GMLMAST-GET-P.
           IF  GMLMAST-EOF-OFF
               READ GMLMAST
               AT END
                   SET GMLMAST-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       GMLMAST-FLDSET SECTION.
       GMLMAST-FLDSET-P.
           EVALUATE TRUE
           WHEN ( GMLMAST-IO-AREA (1:1) = '8'
            AND   GMLMAST-IO-AREA (2:1) = '2' )
               MOVE GMLMAST-IO-AREA (3:3)  TO FNR (1:3)
               MOVE GMLMAST-IO-AREA (6:6)  TO KNR (1:6)
               MOVE GMLMAST-IO-AREA (12:5) TO VGR (1:5)
               MOVE GMLMAST-IO-AREA (17:5) TO MAKKFA-IO
               MOVE 70                     TO BW-A
               PERFORM VARYING ANI-I FROM ANI-MAX BY -1
                         UNTIL ANI-I < 1
                   SUBTRACT 4            FROM BW-A
                   MOVE GMLMAST-IO-AREA (BW-A:4) TO XI-ANI-GRP
                   MOVE XI-ANI             TO ANI (ANI-I)
               END-PERFORM
               MOVE GMLMAST-IO-AREA (70:6) TO MAKKFB-IO
               MOVE 136                    TO BW-A
               PERFORM VARYING BLI-I FROM BLI-MAX BY -1
                         UNTIL BLI-I < 1
                   SUBTRACT 5            FROM BW-A
                   MOVE GMLMAST-IO-AREA (BW-A:5) TO XI-BLI-GRP
                   MOVE XI-BLI             TO BLI (BLI-I)
               END-PERFORM
           END-EVALUATE.
 
       GMLMAST-IDCHK SECTION.
       GMLMAST-IDCHK-P.
           EVALUATE TRUE
           WHEN ( GMLMAST-IO-AREA (1:1) = '8'
            AND   GMLMAST-IO-AREA (2:1) = '2' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       GMLMAST-IDSET SECTION.
       GMLMAST-IDSET-P.
           EVALUATE TRUE
           WHEN ( GMLMAST-IO-AREA (1:1) = '8'
            AND   GMLMAST-IO-AREA (2:1) = '2' )
               SET I-03                    TO TRUE
           END-EVALUATE.
 
       GMLMAST-CHK-LEVEL SECTION.
       GMLMAST-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( GMLMAST-IO-AREA (1:1) = '8'
            AND   GMLMAST-IO-AREA (2:1) = '2' )
               MOVE LOW-VALUES             TO GMLMAST-LEVEL-03
               MOVE GMLMAST-IO-AREA (3:3)  TO GMLMAST-03-L3-FNR
               MOVE GMLMAST-IO-AREA (6:6)  TO GMLMAST-03-L2-KNR
               MOVE GMLMAST-IO-AREA (12:5) TO GMLMAST-03-L1-VGR
               IF  GMLMAST-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  GMLMAST-03-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  GMLMAST-03-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  GMLMAST-03-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  GMLMAST-03-L3         TO THE-PRIOR-L3
               MOVE  GMLMAST-03-L2         TO THE-PRIOR-L2
               MOVE  GMLMAST-03-L1         TO THE-PRIOR-L1
               SET GMLMAST-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       GMLMAST-MATCH-SET SECTION.
       GMLMAST-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( GMLMAST-IO-AREA (1:1) = '8'
            AND   GMLMAST-IO-AREA (2:1) = '2' )
               MOVE GMLMAST-IO-AREA (3:3)  TO GMLMAST-M-03-M3-FNR
               MOVE GMLMAST-IO-AREA (6:6)  TO GMLMAST-M-03-M2-KNR
               MOVE GMLMAST-IO-AREA (12:5) TO GMLMAST-M-03-M1-VGR
           END-EVALUATE.
 
       PARFILE-GET SECTION.
       PARFILE-GET-P.
           IF  PARFILE-EOF-OFF
               READ PARFILE
               AT END
                   SET PARFILE-EOF         TO TRUE
               END-READ
           END-IF.
 
       PARFILE-FLDSET SECTION.
       PARFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ( PARFILE-IO-AREA (1:1) = 'P' )
               MOVE PARFILE-IO-AREA (2:2)  TO PMND (1:2)
           END-EVALUATE.
 
       PARFILE-IDCHK SECTION.
       PARFILE-IDCHK-P.
           EVALUATE TRUE
           WHEN ( PARFILE-IO-AREA (1:1) = 'P' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       PARFILE-IDSET SECTION.
       PARFILE-IDSET-P.
           EVALUATE TRUE
           WHEN ( PARFILE-IO-AREA (1:1) = 'P' )
               SET I-01                    TO TRUE
           END-EVALUATE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  TRANSER-EOF
               MOVE HIGH-VALUES            TO TRANSER-MC
                                              TRANSER-MP
           END-IF
           IF  GMLMAST-EOF
               MOVE HIGH-VALUES            TO GMLMAST-MC
                                              GMLMAST-MP
           END-IF
           IF  TRANSER-MC < TRANSER-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  GMLMAST-MC < GMLMAST-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  TRANSER-MC < GMLMAST-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET TRANSER-PROCESS     TO TRUE
                   MOVE TRANSER-MC         TO TRANSER-MP
                   IF  TRANSER-MC = GMLMAST-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  GMLMAST-MC < TRANSER-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET GMLMAST-PROCESS     TO TRUE
                   MOVE GMLMAST-MC         TO GMLMAST-MP
                   IF  GMLMAST-MC = TRANSER-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  TRANSER-MC = GMLMAST-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET TRANSER-PROCESS     TO TRUE
                   MOVE TRANSER-MC         TO TRANSER-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND NOT-I-33)
               MOVE SPACES TO NYMAST-IO-AREA
               INITIALIZE NYMAST-IO-AREA
               MOVE '82'                   TO NYMAST-IO-AREA (1:2)
               MOVE FNR                    TO NYMAST-IO-AREA (3:3)
               MOVE KNR                    TO NYMAST-IO-AREA (6:6)
               MOVE VGR                    TO NYMAST-IO-AREA (12:5)
               MOVE L1AAFJ                 TO XO-72P
               MOVE XO-72P-EF              TO NYMAST-IO-AREA (17:5)
               MOVE 70                     TO BW-A
               PERFORM VARYING ANU-I FROM ANU-MAX BY -1
                         UNTIL ANU-I < 1
                   SUBTRACT 4            FROM BW-A
                   MOVE ANU (ANU-I)        TO XO-52P
                   MOVE XO-52P-EF          TO NYMAST-IO-AREA (BW-A:4)
               END-PERFORM
               MOVE L1BAFJ                 TO XO-92P
               MOVE XO-92P-EF              TO NYMAST-IO-AREA (70:6)
               MOVE 136                    TO BW-A
               PERFORM VARYING BLU-I FROM BLU-MAX BY -1
                         UNTIL BLU-I < 1
                   SUBTRACT 5            FROM BW-A
                   MOVE BLU (BLU-I)        TO XO-72P
                   MOVE XO-72P-EF          TO NYMAST-IO-AREA (BW-A:5)
               END-PERFORM
               MOVE L1ADMF                 TO XO-52P
               MOVE XO-52P-EF              TO NYMAST-IO-AREA (137:4)
               MOVE L1BDMF                 TO XO-72P
               MOVE XO-72P-EF              TO NYMAST-IO-AREA (141:5)
               WRITE NYMAST-IO-AREA
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
           MOVE 2                          TO LR-CHECK
           SET TRANSER-LEVEL-INIT          TO TRUE
           INITIALIZE TRANSER-DATA-FIELDS
           SET TRANSER-EOF-OFF             TO TRUE
           SET TRANSER-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO TRANSER-MC
                                              TRANSER-MP
           OPEN INPUT TRANSER
           SET GMLMAST-LEVEL-INIT          TO TRUE
           INITIALIZE GMLMAST-DATA-FIELDS
           SET GMLMAST-EOF-OFF             TO TRUE
           SET GMLMAST-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO GMLMAST-MC
                                              GMLMAST-MP
           OPEN INPUT GMLMAST
           INITIALIZE PARFILE-DATA-FIELDS
           SET PARFILE-EOF-OFF             TO TRUE
           SET PARFILE-PROCESS             TO TRUE
           OPEN INPUT PARFILE
           OPEN OUTPUT NYMAST.
           PERFORM VARYING ANI-I FROM 1 BY 1
                     UNTIL ANI-I > ANI-MAX
               INITIALIZE ANI (ANI-I)
           END-PERFORM
           SET ANI-I                       TO 1
           PERFORM VARYING ANU-I FROM 1 BY 1
                     UNTIL ANU-I > ANU-MAX
               INITIALIZE ANU (ANU-I)
           END-PERFORM
           SET ANU-I                       TO 1
           PERFORM VARYING BLI-I FROM 1 BY 1
                     UNTIL BLI-I > BLI-MAX
               INITIALIZE BLI (BLI-I)
           END-PERFORM
           SET BLI-I                       TO 1
           PERFORM VARYING BLU-I FROM 1 BY 1
                     UNTIL BLU-I > BLU-MAX
               INITIALIZE BLU (BLU-I)
           END-PERFORM
           SET BLU-I                       TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE TRANSER
           CLOSE GMLMAST
           CLOSE PARFILE
           CLOSE NYMAST.
 
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
 
       SETOFF-I-M SECTION.
           SET NOT-I-M1                    TO TRUE.
           SET NOT-I-M2                    TO TRUE.
           SET NOT-I-M3                    TO TRUE.
           SET NOT-I-M4                    TO TRUE.
           SET NOT-I-M5                    TO TRUE.
           SET NOT-I-M6                    TO TRUE.
           SET NOT-I-M7                    TO TRUE.
           SET NOT-I-M8                    TO TRUE.
           SET NOT-I-M9                    TO TRUE.
 
       SETON-I-M9 SECTION.
           SET I-M9                        TO TRUE.
           PERFORM SETON-I-M8.
 
       SETON-I-M8 SECTION.
           SET I-M8                        TO TRUE.
           PERFORM SETON-I-M7.
 
       SETON-I-M7 SECTION.
           SET I-M7                        TO TRUE.
           PERFORM SETON-I-M6.
 
       SETON-I-M6 SECTION.
           SET I-M6                        TO TRUE.
           PERFORM SETON-I-M5.
 
       SETON-I-M5 SECTION.
           SET I-M5                        TO TRUE.
           PERFORM SETON-I-M4.
 
       SETON-I-M4 SECTION.
           SET I-M4                        TO TRUE.
           PERFORM SETON-I-M3.
 
       SETON-I-M3 SECTION.
           SET I-M3                        TO TRUE.
           PERFORM SETON-I-M2.
 
       SETON-I-M2 SECTION.
           SET I-M2                        TO TRUE.
           PERFORM SETON-I-M1.
 
       SETON-I-M1 SECTION.
           SET I-M1                        TO TRUE.
 
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
