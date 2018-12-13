       IDENTIFICATION DIVISION.
       PROGRAM-ID. LAG724R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAMMET UPPDATERER LAGERSTYRINGSFILE MED KATEGORIKODE OG  *
      * OMREGNINGSPROSENT. ALLE RECORDS HAR FRA FORHÅNDSINNSATT KODE *
      * OG PROSENT.(A 100) SLIK ATT KUN ENDRING BLIR OPPDATERT.      *
      *     29.06.2011 fjernet test på fnr 875 -EN           *
      *     09.08.2011 fjernet test på fnr 226 -EN           *
      *     06.09.2013 fjernet test på fnr 138,233 -EN       *
      ****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: LAG724.rpg
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
           SELECT SKATFIL
               ASSIGN TO UT-S-SKATFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS SKATFIL-STATUS.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT VARETIL
               ASSIGN TO VARETIL
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VARETIL-STATUS
               RECORD KEY IS VARETIL-KEY1.
           SELECT STYFIL
               ASSIGN TO UT-S-STYFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS STYFIL-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD SKATFIL
               BLOCK CONTAINS 4080
               RECORD CONTAINS 80.
       01  SKATFIL-IO-AREA.
           05  SKATFIL-IO-AREA-X           PICTURE X(80).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD VARETIL
               RECORD CONTAINS 200.
       01  VARETIL-IO-AREA.
           05  VARETIL-IO-AREA-X.
               10  VARETIL-KEY1            PICTURE X(12).
               10  FILLER                  PICTURE X(188).
      *ISTE   O   F 132 132            PRINTERSYSLST
       FD STYFIL
               BLOCK CONTAINS 5000
               RECORD CONTAINS 250.
       01  STYFIL-IO-AREA.
           05  STYFIL-IO-AREA-X            PICTURE X(250).
       WORKING-STORAGE SECTION.
       77  ARA-MAX   VALUE 4               PICTURE 9(4) USAGE BINARY.
       77  ARO-MAX   VALUE 4               PICTURE 9(4) USAGE BINARY.
       77  ARF-MAX   VALUE 4               PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  ARA-TABLE.
               10  ARA-ENTRY
                                           OCCURS 4 TIMES
                                           INDEXED BY ARA-I
                                                      ARA-S.
                   15  ARA                 PICTURE X(1).
           05  ARO-TABLE.
               10  ARO-ENTRY
                                           OCCURS 4 TIMES
                                           INDEXED BY ARO-I
                                                      ARO-S.
                   15  ARO                 PICTURE S9(3).
           05  ARF-TABLE.
               10  ARF-ENTRY
                                           OCCURS 4 TIMES
                                           INDEXED BY ARF-I
                                                      ARF-S.
                   15  ARF                 PICTURE S9(3).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  SKATFIL-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  VARETIL-STATUS              PICTURE 99 VALUE 0.
           10  STYFIL-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  SKATFIL-EOF-OFF         VALUE '0'.
               88  SKATFIL-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SKATFIL-READ-OFF        VALUE '0'.
               88  SKATFIL-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SKATFIL-PROCESS-OFF     VALUE '0'.
               88  SKATFIL-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  SKATFIL-LEVEL-INIT-OFF  VALUE '0'.
               88  SKATFIL-LEVEL-INIT      VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  VARETIL-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  SKATFIL-LEVEL-01.
               10  SKATFIL-01-L2.
                   15  SKATFIL-01-L2-FNR   PICTURE X(3).
               10  SKATFIL-01-L1.
                   15  SKATFIL-01-L1-VGR   PICTURE X(5).
                   15  SKATFIL-01-L1-ALF   PICTURE X(3).
           05  SKATFIL-DATA-FIELDS.
               10  FNR                     PICTURE X(3).
               10  VGR                     PICTURE X(5).
               10  ALF                     PICTURE X(3).
               10  ANTTOT-IO.
                   15  ANTTOT              PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
           05  SKATFIL-MP                  PICTURE X(11).
           05  SKATFIL-MC                  PICTURE X(11).
           05  SKATFIL-M-01            REDEFINES SKATFIL-MC.
               10  SKATFIL-M-01-M3.
                   15  SKATFIL-M-01-M3-FNR-G.
                       20  SKATFIL-M-01-M3-FNR PICTURE X(3).
               10  SKATFIL-M-01-M2.
                   15  SKATFIL-M-01-M2-VGR-G.
                       20  SKATFIL-M-01-M2-VGR PICTURE X(5).
               10  SKATFIL-M-01-M1.
                   15  SKATFIL-M-01-M1-ALF-G.
                       20  SKATFIL-M-01-M1-ALF PICTURE X(3).
           05  FIRMAF-DATA-FIELDS.
               10  FRUT                    PICTURE X(1).
               10  FFORS-IO.
                   15  FFORS               PICTURE S9(2).
               10  FMIN                    PICTURE X(1).
               10  FFORPK                  PICTURE X(1).
               10  FRESTO                  PICTURE X(1).
               10  NULLS-X                 PICTURE X(1).
           05  VARETIL-DATA-FIELDS.
               10  REC                     PICTURE X(2).
               10  MAXVT-IO.
                   15  MAXVT               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
           05  STYFIL-LEVEL-02.
               10  STYFIL-02-L2.
                   15  STYFIL-02-L2-FNR    PICTURE X(3).
               10  STYFIL-02-L1.
                   15  STYFIL-02-L1-ALF    PICTURE X(3).
                   15  STYFIL-02-L1-VGR    PICTURE X(5).
           05  STYFIL-DATA-FIELDS.
               10  KEYVM                   PICTURE X(10).
               10  LIST                    PICTURE X(98).
               10  MERK                    PICTURE X(1).
               10  ANTIN-IO.
                   15  ANTIN               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ANTUT-IO.
                   15  ANTUT               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  KINNH-IO.
                   15  KINNH               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  REST-IO.
                   15  REST                PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  ANTBES-IO.
                   15  ANTBES              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  MINB-IO.
                   15  MINB                PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  ANT-IO.
                   15  ANT                 PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  REB                     PICTURE X(1).
               10  ANT3M-IO.
                   15  ANT3M               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  GJSNIT-IO.
                   15  GJSNIT              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  FP-IO.
                   15  FP                  PICTURE S9(1)V9(2).
               10  FK-IO.
                   15  FK                  PICTURE S9(1)V9(2).
               10  LTID-IO.
                   15  LTID                PICTURE S9(2)V9(2).
           05  STYFIL-MP                   PICTURE X(11).
           05  STYFIL-MC                   PICTURE X(11).
           05  STYFIL-M-02             REDEFINES STYFIL-MC.
               10  STYFIL-M-02-M3.
                   15  STYFIL-M-02-M3-FNR-G.
                       20  STYFIL-M-02-M3-FNR PICTURE X(3).
               10  STYFIL-M-02-M2.
                   15  STYFIL-M-02-M2-VGR-G.
                       20  STYFIL-M-02-M2-VGR PICTURE X(5).
               10  STYFIL-M-02-M1.
                   15  STYFIL-M-02-M1-ALF-G.
                       20  STYFIL-M-02-M1-ALF PICTURE X(3).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(8).
           05  TEMPORARY-FIELDS.
               10  X-IO.
                   15  X                   PICTURE S9(1).
               10  ANTAKK-IO.
                   15  ANTAKK              PICTURE S9(9).
               10  PROFAK-IO.
                   15  PROFAK              PICTURE S9(3).
               10  ANT2-IO.
                   15  ANT2                PICTURE S9(9).
               10  ANTAK2-IO.
                   15  ANTAK2              PICTURE S9(9).
               10  ANT100-IO.
                   15  ANT100              PICTURE S9(11).
               10  PROS-IO.
                   15  PROS                PICTURE S9(3).
               10  KATTYP                  PICTURE X(1).
               10  FPKAT-IO.
                   15  FPKAT               PICTURE S9(1)V9(2).
               10  FPKAT2-IO.
                   15  FPKAT2              PICTURE S9(1)V9(4).
               10  WTILL-IO.
                   15  WTILL               PICTURE S9(7)V9(2).
               10  FTOT-IO.
                   15  FTOT                PICTURE S9(7)V9(2).
               10  ANTFP1-IO.
                   15  ANTFP1              PICTURE S9(7)V9(2).
               10  ANTFK1-IO.
                   15  ANTFK1              PICTURE S9(7)V9(2).
               10  ANTFP-IO.
                   15  ANTFP               PICTURE S9(7).
               10  ANTFK-IO.
                   15  ANTFK               PICTURE S9(7).
               10  BEH-IO.
                   15  BEH                 PICTURE S9(5)V9(2).
               10  FORSL-IO.
                   15  FORSL               PICTURE S9(6).
               10  HFORS-IO.
                   15  HFORS               PICTURE S9(7)V9(2).
               10  HKINNH-IO.
                   15  HKINNH              PICTURE S9(5)V9(1).
               10  ANTFOR-IO.
                   15  ANTFOR              PICTURE S9(7)V9(2).
               10  KEY12                   PICTURE X(12).
               10  HBEH-IO.
                   15  HBEH                PICTURE S9(4)V9(2).
               10  SUM1-IO.
                   15  SUM1                PICTURE S9(6).
           05  EDITTING-FIELDS.
               10  XO-50P-EF.
                 15  XO-50P                PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  XO-70P-EF.
                 15  XO-70P                PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  XO-60P-EF.
                 15  XO-60P                PICTURE S9(6) USAGE
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
           SET NOT-I-03                    TO TRUE
           SET NOT-I-06                    TO TRUE
           SET NOT-I-02                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  SKATFIL-PROCESS
               SET SKATFIL-PROCESS-OFF     TO TRUE
               SET SKATFIL-READ            TO TRUE
           END-IF
 
           IF  SKATFIL-READ
               PERFORM SKATFIL-GET
               SET SKATFIL-READ-OFF        TO TRUE
               IF  NOT SKATFIL-EOF
                   PERFORM SKATFIL-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM SKATFIL-MATCH-SET
               END-IF
           END-IF
 
           IF  STYFIL-PROCESS
               SET STYFIL-PROCESS-OFF      TO TRUE
               SET STYFIL-READ             TO TRUE
           END-IF
 
           IF  STYFIL-READ
               PERFORM STYFIL-GET
               SET STYFIL-READ-OFF         TO TRUE
               IF  NOT STYFIL-EOF
                   PERFORM STYFIL-MATCH-SET
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
 
           IF  SKATFIL-PROCESS
               PERFORM SKATFIL-IDSET
           END-IF
 
           IF  STYFIL-PROCESS
               PERFORM STYFIL-IDSET
           END-IF
 
           IF  SKATFIL-PROCESS
               PERFORM SKATFIL-CHK-LEVEL
           END-IF
 
           IF  STYFIL-PROCESS
               PERFORM STYFIL-CHK-LEVEL
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
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  SKATFIL-PROCESS
               PERFORM SKATFIL-FLDOFF
               PERFORM SKATFIL-FLDSET
           END-IF
 
           IF  STYFIL-PROCESS
               PERFORM STYFIL-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  SKATFIL-PROCESS
           OR  STYFIL-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-L2)
               MOVE FNR                    TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-41                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-41            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
           END-IF
           IF  (I-L1)
               MOVE 1                      TO X
               SUBTRACT ANTAKK             FROM ANTAKK
      *************************************************************
      * **    0 I SALG SISTE ÅR SKAL HA 0 I FORSLAG.
           END-IF
           IF  (I-L2)
               SET NOT-I-94                TO TRUE
               IF  NULLS-X = 'J'
                   SET I-94                TO TRUE
               END-IF
      *************************************************************
      * **    SPESIALRUTINER
      *  L2      FNR       COMP "938"                    65 C&B
           END-IF
           IF  (I-L2)
               SET NOT-I-61                TO TRUE
               IF  FNR = '199'
                   SET I-61                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-61)
               SET NOT-I-61                TO TRUE
               IF  FNR = '974'
                   SET I-61                TO TRUE
               END-IF
           END-IF
           IF  (I-L2)
               SET NOT-I-88                TO TRUE
               IF  FNR = '918'
                   SET I-88                TO TRUE
               END-IF
               SET NOT-I-96                TO TRUE
               IF  FNR = '963'
                   SET I-96                TO TRUE
               END-IF
               SET NOT-I-83                TO TRUE
               IF  FNR = '915'
                   SET I-83                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-83)
               SET NOT-I-83                TO TRUE
               IF  FNR = '918'
                   SET I-83                TO TRUE
               END-IF
           END-IF
           IF  (I-L2)
               SET NOT-I-40                TO TRUE
               IF  FNR = '855'
                   SET I-40                TO TRUE
               END-IF
               SET NOT-I-39                TO TRUE
               IF  FNR = '764'
                   SET I-39                TO TRUE
               END-IF
               SET NOT-I-33                TO TRUE
               IF  FNR = '909'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '778'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '763'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '971'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '994'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '740'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '754'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '332'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '791'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '625'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '460'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '391'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '977'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '732'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '981'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '845'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '976'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '658'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '638'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '652'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '659'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '630'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '426'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '972'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '958'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '674'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '399'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '967'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '690'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '969'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '626'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '834'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '429'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '928'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '218'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '939'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '985'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '951'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '992'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '395'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '938'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '984'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '724'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '682'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '521'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '492'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '806'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '858'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '956'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '614'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '502'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '540'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '928'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '924'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '416'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '100'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '143'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '156'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '196'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '207'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '209'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '212'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '226'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '326'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '392'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '419'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '435'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '443'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '474'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '494'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '526'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '561'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '590'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '594'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '627'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '632'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '646'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '651'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '675'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '679'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '684'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '700'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '894'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '934'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '300'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FNR = '953'
                   SET I-33                TO TRUE
               END-IF
      *  **
      *  ** VIL HA OPP TIL MAXBEH
      *  **
           END-IF
           IF  (I-L2)
               SET NOT-I-34                TO TRUE
               IF  FNR = '630'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  FNR = '690'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  FNR = '958'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  FNR = '218'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  FNR = '956'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  FNR = '199'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  FNR = '974'
                   SET I-34                TO TRUE
               END-IF
      *  **
      *  ** VIL HA OPP TIL minbeh
      *  **
           END-IF
           IF  (I-L2)
               SET NOT-I-79                TO TRUE
               IF  FNR = '938'
                   SET I-79                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-79)
               SET NOT-I-79                TO TRUE
               IF  FNR = '627'
                   SET I-79                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-79)
               SET NOT-I-79                TO TRUE
               IF  FNR = '658'
                   SET I-79                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-79)
               SET NOT-I-79                TO TRUE
               IF  FNR = '638'
                   SET I-79                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-79)
               SET NOT-I-79                TO TRUE
               IF  FNR = '652'
                   SET I-79                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-79)
               SET NOT-I-79                TO TRUE
               IF  FNR = '659'
                   SET I-79                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-79)
               SET NOT-I-79                TO TRUE
               IF  FNR = '985'
                   SET I-79                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-79)
               SET NOT-I-79                TO TRUE
               IF  FNR = '732'
                   SET I-79                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-79)
               SET NOT-I-79                TO TRUE
               IF  FNR = '614'
                   SET I-79                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-79)
               SET NOT-I-79                TO TRUE
               IF  FNR = '939'
                   SET I-79                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-79)
               SET NOT-I-79                TO TRUE
               IF  FNR = '951'
                   SET I-79                TO TRUE
               END-IF
           END-IF
           IF  (I-L2)
               SET NOT-I-73                TO TRUE
               IF  FNR = '956'
                   SET I-73                TO TRUE
               END-IF
      *  **
      *  ** AMUNDSEN VIL HA MIN.BEH PÅ ALFAKODE MEK OG TIL.
      *  **
           END-IF
           IF  (I-L2)
               SET NOT-I-77                TO TRUE
               IF  FNR = '658'
                   SET I-77                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-77)
               SET NOT-I-77                TO TRUE
               IF  FNR = '659'
                   SET I-77                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-77)
               SET NOT-I-77                TO TRUE
               IF  FNR = '652'
                   SET I-77                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-77)
               SET NOT-I-77                TO TRUE
               IF  FNR = '638'
                   SET I-77                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-77)
               SET NOT-I-77                TO TRUE
               IF  FNR = '985'
                   SET I-77                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-77)
               SET NOT-I-77                TO TRUE
               IF  FNR = '614'
                   SET I-77                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-77)
               SET NOT-I-33                TO TRUE
               SET NOT-I-78                TO TRUE
               IF  ALF = 'MEK'
                   SET I-78                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-77 AND I-78)
               SET I-33                    TO TRUE
           END-IF
           IF  (I-73)
               SET NOT-I-78                TO TRUE
               IF  REB = 'R'
                   SET I-78                TO TRUE
               END-IF
           END-IF
           IF  (I-73 AND I-78)
               SET I-33                    TO TRUE
      *  **
      * **    TEST MINIMUMSBEHOLDNING.
           END-IF
           IF  (I-L2)
               SET NOT-I-87                TO TRUE
               IF  FMIN = 'J'
                   SET I-87                TO TRUE
               END-IF
      *************************************************************
      * **    SPESIALRUTINER AUTOPARTS
           END-IF
           IF  (I-L2)
               SET NOT-I-97                TO TRUE
               IF  FNR = '608'
                   SET I-97                TO TRUE
               END-IF
               SET NOT-I-42                TO TRUE
               IF  FNR = '740'
                   SET I-42                TO TRUE
               END-IF
      *************************************************************
      * **    0 I SALG SISTE ÅR MEN NOE I REST SKAL HA FORSLAG.
           END-IF
           IF  (I-L2)
               SET NOT-I-95                TO TRUE
               IF  FRESTO = 'J'
                   SET I-95                TO TRUE
               END-IF
      ******************************************************************
           END-IF
           IF  (I-01)
               PERFORM OMSRUT-S
           END-IF
           IF  (I-01)
               GO TO END-X-T
           END-IF
           IF  (I-02)
               SET NOT-I-60                TO TRUE
           END-IF
           IF  (I-02 AND NOT-I-MR)
               GO TO SLUTT-T
      *  02N09             GOTO SLUTT                       NY ARTIKKEL.
           END-IF
           PERFORM KATRUT-S.
 
       SLUTT-T.
           IF  (I-61)
               PERFORM FORDE-S
           END-IF
           IF  (I-88)
               SET NOT-I-61                TO TRUE
               IF  VGR = '34010'
                   SET I-61                TO TRUE
               END-IF
           END-IF
           IF  (I-88 AND NOT-I-61)
               SET NOT-I-61                TO TRUE
               IF  VGR = '37610'
                   SET I-61                TO TRUE
               END-IF
           END-IF
           IF  (I-88 AND NOT-I-61)
               SET NOT-I-61                TO TRUE
               IF  VGR = '36200'
                   SET I-61                TO TRUE
               END-IF
           END-IF
           IF  (I-88 AND NOT-I-61)
               SET NOT-I-61                TO TRUE
               IF  VGR = '32020'
                   SET I-61                TO TRUE
               END-IF
           END-IF
           IF  (I-88 AND NOT-I-61)
               SET NOT-I-61                TO TRUE
               IF  VGR = '37530'
                   SET I-61                TO TRUE
               END-IF
           END-IF
           IF  (I-88 AND NOT-I-61)
               SET NOT-I-61                TO TRUE
               IF  VGR = '37220'
                   SET I-61                TO TRUE
               END-IF
           END-IF
           IF  (I-88 AND NOT-I-61)
               SET NOT-I-61                TO TRUE
               IF  VGR = '37310'
                   SET I-61                TO TRUE
               END-IF
           END-IF
           IF  (I-88 AND NOT-I-61)
               SET NOT-I-61                TO TRUE
               IF  VGR = '33020'
                   SET I-61                TO TRUE
               END-IF
           END-IF
           IF  (I-88 AND NOT-I-61)
               SET NOT-I-61                TO TRUE
               IF  VGR = '92020'
                   SET I-61                TO TRUE
               END-IF
           END-IF
           IF  (I-88 AND NOT-I-61)
               SET NOT-I-61                TO TRUE
               IF  VGR = '37320'
                   SET I-61                TO TRUE
               END-IF
           END-IF
           IF  (I-88 AND NOT-I-61)
               SET NOT-I-61                TO TRUE
               IF  VGR = '37510'
                   SET I-61                TO TRUE
               END-IF
           END-IF
           IF  (I-88 AND NOT-I-61)
               SET NOT-I-61                TO TRUE
               IF  VGR = '33030'
                   SET I-61                TO TRUE
               END-IF
           END-IF
           IF  (I-88 AND NOT-I-61)
               SET NOT-I-61                TO TRUE
               IF  VGR = '37120'
                   SET I-61                TO TRUE
               END-IF
           END-IF
           IF  (I-88 AND NOT-I-61)
               SET NOT-I-61                TO TRUE
               IF  VGR = '36210'
                   SET I-61                TO TRUE
               END-IF
           END-IF
           IF  (I-88 AND NOT-I-61)
               SET NOT-I-61                TO TRUE
               IF  VGR = '36220'
                   SET I-61                TO TRUE
               END-IF
           END-IF
           IF  (I-88 AND NOT-I-61)
               SET NOT-I-61                TO TRUE
               IF  VGR = '36250'
                   SET I-61                TO TRUE
               END-IF
           END-IF
           IF  (I-88 AND NOT-I-61)
               SET NOT-I-61                TO TRUE
               IF  VGR = '37115'
                   SET I-61                TO TRUE
               END-IF
           END-IF
           IF  (I-88 AND I-61)
               PERFORM FORDE-S
           END-IF
           IF  (I-33)
               GO TO MEK00-T
           END-IF
           SET NOT-I-30                    TO TRUE
           IF  KATTYP = ' '
               SET I-30                    TO TRUE
           END-IF
           IF  (I-30)
               MOVE 'A'                    TO KATTYP
      *          PROFAK    COMP 0                        90
           END-IF
           PERFORM KATEGO-S
           PERFORM NYFOR-S
           SET NOT-I-91                    TO TRUE
           IF  FFORPK = 'J'
               SET I-91                    TO TRUE
           END-IF
           SET NOT-I-43                    TO TRUE
           IF  KINNH < 0
               SET I-43                    TO TRUE
           END-IF
           SET NOT-I-44                    TO TRUE
           SET NOT-I-98                    TO TRUE
           IF  MINB < 0
               SET I-44                    TO TRUE
           END-IF
           IF  MINB = 0
               SET I-98                    TO TRUE
           END-IF
           IF  (I-43)
               MULTIPLY -1 BY KINNH    GIVING KINNH
           END-IF
           IF  (I-44)
               MULTIPLY -1 BY MINB     GIVING MINB
           END-IF
           IF  (I-87)
               PERFORM MINRUT-S
           END-IF
           IF  (I-91)
               PERFORM INNRUT-S
           END-IF
           IF  (I-97)
               PERFORM MINRUT-S
           END-IF
           IF  (I-97)
               GO TO END-X-T
      *  42N98             EXSR MINRUT
      *  42N98             GOTO END
      * **    0 I SALG SISTE ÅR SKAL HA 0 I FORSLAG
           END-IF
           IF  (I-94)
               SET NOT-I-32                TO TRUE
               IF  ANT = 0
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-94 AND I-32)
               MOVE 0                      TO FORSL
      * **    ANTALL SOLGT MINDRE ENN FORS SKAL HA 0 I FORSLAG.
           END-IF
           SET NOT-I-32                    TO TRUE
           IF  FFORS = 0
               SET I-32                    TO TRUE
           END-IF
           IF  (I-32)
               GO TO IKKE0-T
           END-IF
           SET NOT-I-32                    TO TRUE
           IF  ANT NOT > FFORS
               SET I-32                    TO TRUE
           END-IF
           IF  (I-32)
               MOVE 0                      TO FORSL
           END-IF
           IF  (I-32 AND I-39)
               ADD REST TO ZERO        GIVING FORSL
           END-IF.
 
       IKKE0-T.
      * **    0 I SALG SISTE ÅR MEN NOE I REST SKAL HA FORSLAG.
           IF  (I-95)
               SET NOT-I-32                TO TRUE
               IF  ANT = 0
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-95 AND I-32)
               SET NOT-I-32                TO TRUE
               IF  REST > 0
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-95 AND I-32)
               MOVE 1                      TO FORSL
      *
      * **    SPESIAL RUTINE RODIN og sogb og Bema
      *                    SETOF                       12
           END-IF
           IF  (I-83)
               SET NOT-I-66                TO TRUE
               IF  REST > 0
                   SET I-66                TO TRUE
               END-IF
           END-IF
           IF  (I-83 AND I-66)
               ADD REST TO ZERO        GIVING FORSL
               GO TO END-X-T
           END-IF
           IF  (I-39 AND I-66)
               ADD REST TO ZERO        GIVING FORSL
               GO TO END-X-T
      *
      * **    SPESIAL RUTINE VAN LINE
      *
      * **    SPESIAL RUTINE KCL
           END-IF
           IF  (I-40)
               SET NOT-I-32                TO TRUE
               IF  REST > 0
                   SET I-32                TO TRUE
               END-IF
               SET NOT-I-36                TO TRUE
               IF  MERK = '1'
                   SET I-36                TO TRUE
               END-IF
           END-IF
           IF  (I-40 AND NOT-I-36)
               SET NOT-I-36                TO TRUE
               IF  MERK = '2'
                   SET I-36                TO TRUE
               END-IF
           END-IF
           IF  (I-40 AND NOT-I-36)
               SET NOT-I-36                TO TRUE
               IF  MERK = '4'
                   SET I-36                TO TRUE
               END-IF
           END-IF
           IF  (I-40 AND I-36 AND NOT-I-32)
               MOVE 0                      TO FORSL
           END-IF
           IF  (I-40 AND I-32)
               SUBTRACT ANTUT FROM ANTIN GIVING BEH
               SUBTRACT ANTBES FROM REST GIVING FORSL
               SUBTRACT BEH                FROM FORSL
               SET NOT-I-82                TO TRUE
               IF  FORSL > 0
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (I-40 AND I-32 AND NOT-I-82)
               MOVE 0                      TO FORSL
           END-IF
           IF  (I-40)
               GO TO MEK00-T
      *                                                                 000297
      *                                                                 000298
           END-IF
           SET NOT-I-31                    TO TRUE
           IF  MERK = '2'
               SET I-31                    TO TRUE
           END-IF
           IF  (I-31)
               MOVE 0                      TO FORSL
           END-IF
           IF  (I-39 AND I-31)
               ADD REST TO ZERO        GIVING FORSL
      * **    0 I SALG SISTE ÅR MEN NOE I REST SKAL HA FORSLAG.
           END-IF
           IF  (I-96)
               SET NOT-I-32                TO TRUE
               IF  REST > 0
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-96 AND I-32)
               MOVE 1                      TO FORSL
      *
           END-IF
           .
 
       MEK00-T.
           IF  (I-33)
               PERFORM MEK01-S
           END-IF.
 
       END-X-T.
      ****************************************************************
      * SUBRUTINE FOR Å BYGGE OPP OMSETNINGSARRAY AKKUMULERT.        *
      ****************************************************************
           CONTINUE.
 
       OMSRUT-S SECTION.
       OMSRUT-S-P.
           ADD ARO (1)                     TO ARO (2)
           ADD ARO (2)                     TO ARO (3)
           ADD ARO (3)                     TO ARO (4).
      ***************************************************************
      * SUBRUTINE FOR SUMMERING, SAMT BEREGNING AV KATEGORI.        *
      * PROSENTEN UTREGNES ETTER ATT HALVPARTEN AV ANTALL SOLGT     *
      * PÅ SISTE VARE ER LAGT TIL AKKUMULERT SOLGT TIL NÅ.          *
      ***************************************************************
 
       KATRUT-S SECTION.
       KATRUT-S-P.
           MOVE 0                          TO PROFAK
           DIVIDE ANT BY 2             GIVING ANT2
           ADD ANT2 TO ANTAKK          GIVING ANTAK2
           ADD ANT                         TO ANTAKK
           MULTIPLY 100 BY ANTAK2      GIVING ANT100
           IF  (I-08)
               MOVE 0                      TO PROS
           END-IF
           IF  (NOT-I-08)
               DIVIDE ANT100 BY ANTTOT GIVING PROS ROUNDED
           END-IF.
 
       PROLOP-T.
           SET NOT-I-51                    TO TRUE
           IF  PROS NOT > ARO (X)
               SET I-51                    TO TRUE
           END-IF
           IF  (NOT-I-51)
               ADD 1                       TO X
           END-IF
           SET NOT-I-52                    TO TRUE
           IF  X NOT < 4
               SET I-52                    TO TRUE
           END-IF
           IF  (I-52)
               MOVE 4                      TO X
               SET I-51                    TO TRUE
           END-IF
           IF  (NOT-I-51)
               GO TO PROLOP-T
           END-IF
           MOVE ARA (X)                    TO KATTYP
           MOVE ARF (X)                    TO PROFAK-IO
           SET NOT-I-60                    TO TRUE
           IF  X > 1
               SET I-60                    TO TRUE
           END-IF.
      *******************************************************************
      *    SUBRUTINE FOR OMREGNING AV F.P. ETTER SERVICEKATEGORIPROSENT.*
      *******************************************************************
 
       KATEGO-S SECTION.
       KATEGO-S-P.
           IF  (I-90)
               ADD FP TO ZERO          GIVING FPKAT
               GO TO ENDKAT-T
           END-IF
           SUBTRACT LTID FROM FP       GIVING FPKAT
           DIVIDE FPKAT BY 100         GIVING FPKAT2
           MULTIPLY PROFAK BY FPKAT2   GIVING FPKAT ROUNDED
           ADD LTID                        TO FPKAT
      **         PROFAK    COMP 0                        35BEST. VED 0 I BEH.
           .
 
       ENDKAT-T.
           CONTINUE.
      ****************************************************************
      * SUBRUTINE FOR Å REGNE UT NYTT FORSLAG.                       *
      ****************************************************************
 
       NYFOR-S SECTION.
       NYFOR-S-P.
           MOVE 0                          TO WTILL
           MOVE 0                          TO FTOT
           MULTIPLY FPKAT BY GJSNIT    GIVING ANTFP1 ROUNDED
           MULTIPLY FK BY GJSNIT       GIVING ANTFK1 ROUNDED
           ADD ANTFP1 TO ZERO          GIVING ANTFP ROUNDED
           ADD ANTFK1 TO ZERO          GIVING ANTFK ROUNDED
      * **  ** ** ** ** ** ** ** ** *** ** ** *
           SET NOT-I-75                    TO TRUE
           IF  ANTFK < 1
               SET I-75                    TO TRUE
           END-IF
           IF  (I-75)
               MOVE 1                      TO ANTFK
      * **    SPESIALRUTINE VOGT     ****** ***
      *  SR 39      ANTFP     ADD  REST      ANTFP
           END-IF
           SUBTRACT ANTUT FROM ANTIN   GIVING BEH
           SET NOT-I-70                    TO TRUE
           IF  ANTFP < BEH
               SET I-70                    TO TRUE
           END-IF
           IF  (I-70)
               MULTIPLY 1,1 BY ANTFP   GIVING ANTFP ROUNDED
           END-IF
           ADD ANTFK TO ANTFP          GIVING FTOT ROUNDED
           ADD ANTBES TO BEH           GIVING WTILL ROUNDED
           SET NOT-I-71                    TO TRUE
           IF  WTILL NOT > ANTFP
               SET I-71                    TO TRUE
           END-IF
           IF  (I-71)
               SET NOT-I-72                TO TRUE
               IF  FTOT < 1
                   SET I-72                TO TRUE
               END-IF
           END-IF
           IF  (I-71 AND I-72)
               MOVE 1                      TO ANTFP-IO (7:1)
           END-IF
           MOVE 0                          TO FORSL
           IF  (I-71)
               ADD ANTFP TO ANTFK      GIVING FORSL ROUNDED
               SUBTRACT WTILL              FROM FORSL ROUNDED
           END-IF
           SET NOT-I-74                    TO TRUE
           IF  FORSL < 0
               SET I-74                    TO TRUE
           END-IF
           IF  (I-74)
               MOVE 0                      TO FORSL
           END-IF.
      ****************************************************************
      * SUBRUTINE FOR Å REGNE UT NY FAKTOR                           *
      ****************************************************************
 
       INNRUT-S SECTION.
       INNRUT-S-P.
           SET NOT-I-80                    TO TRUE
           IF  KINNH = 0
               SET I-80                    TO TRUE
           END-IF
           IF  (NOT-I-80)
               SET NOT-I-80                TO TRUE
               IF  FORSL = 0
                   SET I-80                TO TRUE
               END-IF
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  BEH = 0
               SET I-81                    TO TRUE
           END-IF
           IF  (I-80)
               GO TO SLUTTK-T
           END-IF
           ADD FORSL TO ZERO           GIVING HFORS
           MOVE 0                          TO HKINNH
           MOVE 0                          TO ANTFOR
           DIVIDE KINNH BY 2           GIVING HKINNH.
 
       LOOP-T.
           SET NOT-I-85                    TO TRUE
           IF  HFORS NOT < KINNH
               SET I-85                    TO TRUE
           END-IF
           IF  (I-85)
               SUBTRACT KINNH              FROM HFORS
               ADD 1                       TO ANTFOR
               GO TO LOOP-T
           END-IF
           SET NOT-I-86                    TO TRUE
           IF  HFORS NOT < HKINNH
               SET I-86                    TO TRUE
           END-IF
           IF  (I-86)
               ADD 1                       TO ANTFOR
           END-IF
           SET NOT-I-82                    TO TRUE
           IF  ANTFOR = 0
               SET I-82                    TO TRUE
           END-IF
           IF  (I-81 AND I-82)
               MOVE 1                      TO ANTFOR
           END-IF
           MULTIPLY ANTFOR BY KINNH    GIVING FORSL ROUNDED
      * * UTG.   FORSL     SUB  ANTBES    FORSL        82
      * * UTG.   FORSL     SUB  BEH       FORSL        82
      * *82                Z-ADD0         FORSL
           .
 
       SLUTTK-T.
           CONTINUE.
      ****************************************************************
      * SUBRUTINE FOR Å REGNE UT NY FAKTOR                           *
      ****************************************************************
 
       FORDE-S SECTION.
       FORDE-S-P.
           MOVE '81'                       TO KEY12 (1:2)
           MOVE KEYVM                      TO KEY12 (3:10)
           SET NOT-I-64                    TO TRUE
           MOVE KEY12                      TO VARETIL-KEY1
           READ VARETIL RECORD KEY IS VARETIL-KEY1
           INVALID KEY
               SET I-62                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-62                TO TRUE
               PERFORM VARETIL-FLDSET
               PERFORM VARETIL-IDSET
           END-READ
           IF  (NOT-I-62)
               SET NOT-I-64                TO TRUE
               IF  MAXVT > 0
                   SET I-64                TO TRUE
               END-IF
               ADD MAXVT TO ZERO       GIVING KINNH
           END-IF
           IF  (I-62)
               MOVE 0                      TO KINNH
           END-IF
           SET I-33                        TO TRUE.
      ****************************************************************
      * SUBRUTINE FOR Å REGNE UT NY FAKTOR                           *
      ****************************************************************
 
       MINRUT-S SECTION.
       MINRUT-S-P.
           IF  (I-97)
               MOVE 0                      TO FORSL
           END-IF
           SET NOT-I-80                    TO TRUE
           IF  MINB = 0
               SET I-80                    TO TRUE
           END-IF
           IF  (I-80)
               GO TO SLUTTM-T
           END-IF
           IF  (I-87)
               ADD FORSL TO BEH        GIVING HBEH
               ADD ANTBES                  TO HBEH
               SET NOT-I-84                TO TRUE
               IF  MINB > HBEH
                   SET I-84                TO TRUE
               END-IF
           END-IF
           IF  (I-87 AND NOT-I-84)
               GO TO SLUTTM-T
           END-IF
           IF  (NOT-I-84)
               SET NOT-I-84                TO TRUE
               IF  MINB > BEH
                   SET I-84                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-84)
               GO TO SLUTTM-T
           END-IF
           SUBTRACT BEH FROM MINB      GIVING FORSL.
 
       SLUTTM-T.
           CONTINUE.
      ****************************************************************
      * SUBRUTINE FOR Å LAGE BEST FORSL. MEKONOMEN
      ****************************************************************
 
       MEK01-S SECTION.
       MEK01-S-P.
           MOVE 0                          TO FORSL
           SET NOT-I-80                    TO TRUE
           IF  MINB = 0
               SET I-80                    TO TRUE
           END-IF
           IF  (I-80)
               GO TO SLUMEK-T
           END-IF
           SUBTRACT ANTUT FROM ANTIN   GIVING BEH
           SET NOT-I-47                    TO TRUE
           SET NOT-I-49                    TO TRUE
           IF  KINNH > 0
               SET I-47                    TO TRUE
           END-IF
           IF  KINNH = 0
               SET I-49                    TO TRUE
           END-IF
           ADD ANTBES TO BEH           GIVING SUM1
           SET NOT-I-48                    TO TRUE
           IF  SUM1 NOT < MINB
               SET I-48                    TO TRUE
           END-IF
           IF  (I-48)
               MOVE 0                      TO FORSL
           END-IF
           IF  (NOT-I-48 AND I-47)
               ADD KINNH TO ZERO       GIVING FORSL
           END-IF
           IF  (NOT-I-48 AND I-47 AND I-34)
               SUBTRACT BEH FROM KINNH GIVING FORSL
           END-IF
           IF  (NOT-I-48 AND I-49)
               ADD MINB TO ZERO        GIVING FORSL
           END-IF
           IF  (NOT-I-48 AND I-79 AND NOT-I-47)
               SUBTRACT BEH FROM MINB  GIVING FORSL
           END-IF.
 
       SLUMEK-T.
           CONTINUE.
      *ISTE   D        02
      *                        LIST     100
 
       SKATFIL-GET SECTION.
       SKATFIL-GET-P.
           IF  SKATFIL-EOF-OFF
               READ SKATFIL
               AT END
                   SET SKATFIL-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       SKATFIL-FLDOFF SECTION.
       SKATFIL-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( SKATFIL-IO-AREA (1:1) = 'S' )
               SET NOT-I-08                TO TRUE
           END-EVALUATE.
 
       SKATFIL-FLDSET SECTION.
       SKATFIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ( SKATFIL-IO-AREA (1:1) = 'S' )
               MOVE SKATFIL-IO-AREA (2:3)  TO FNR (1:3)
               MOVE SKATFIL-IO-AREA (5:5)  TO VGR (1:5)
               MOVE SKATFIL-IO-AREA (10:3) TO ALF (1:3)
               MOVE SKATFIL-IO-AREA (13:1) TO ARA (1) (1:1)
               MOVE SKATFIL-IO-AREA (14:3) TO ARO (1) (1:3)
               MOVE SKATFIL-IO-AREA (17:3) TO ARF (1) (1:3)
               MOVE SKATFIL-IO-AREA (20:1) TO ARA (2) (1:1)
               MOVE SKATFIL-IO-AREA (21:3) TO ARO (2) (1:3)
               MOVE SKATFIL-IO-AREA (24:3) TO ARF (2) (1:3)
               MOVE SKATFIL-IO-AREA (27:1) TO ARA (3) (1:1)
               MOVE SKATFIL-IO-AREA (28:3) TO ARO (3) (1:3)
               MOVE SKATFIL-IO-AREA (31:3) TO ARF (3) (1:3)
               MOVE SKATFIL-IO-AREA (34:1) TO ARA (4) (1:1)
               MOVE SKATFIL-IO-AREA (35:3) TO ARO (4) (1:3)
               MOVE SKATFIL-IO-AREA (38:3) TO ARF (4) (1:3)
               MOVE SKATFIL-IO-AREA (76:5) TO ANTTOT-IO
               IF  ANTTOT = ZERO
                   SET I-08                TO TRUE
               END-IF
           END-EVALUATE.
 
       SKATFIL-IDCHK SECTION.
       SKATFIL-IDCHK-P.
           EVALUATE TRUE
           WHEN ( SKATFIL-IO-AREA (1:1) = 'S' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       SKATFIL-IDSET SECTION.
       SKATFIL-IDSET-P.
           EVALUATE TRUE
           WHEN ( SKATFIL-IO-AREA (1:1) = 'S' )
               SET I-01                    TO TRUE
           END-EVALUATE.
 
       SKATFIL-CHK-LEVEL SECTION.
       SKATFIL-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( SKATFIL-IO-AREA (1:1) = 'S' )
               MOVE LOW-VALUES             TO SKATFIL-LEVEL-01
               MOVE SKATFIL-IO-AREA (2:3)  TO SKATFIL-01-L2-FNR
               MOVE SKATFIL-IO-AREA (5:5)  TO SKATFIL-01-L1-VGR
               MOVE SKATFIL-IO-AREA (10:3) TO SKATFIL-01-L1-ALF
               IF  SKATFIL-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  SKATFIL-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  SKATFIL-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  SKATFIL-01-L2         TO THE-PRIOR-L2
               MOVE  SKATFIL-01-L1         TO THE-PRIOR-L1
               SET SKATFIL-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       SKATFIL-MATCH-SET SECTION.
       SKATFIL-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( SKATFIL-IO-AREA (1:1) = 'S' )
               MOVE SKATFIL-IO-AREA (2:3)  TO SKATFIL-M-01-M3-FNR
               MOVE SKATFIL-IO-AREA (5:5)  TO SKATFIL-M-01-M2-VGR
               MOVE SKATFIL-IO-AREA (10:3) TO SKATFIL-M-01-M1-ALF
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (915:1) TO FRUT (1:1)
               MOVE FIRMAF-IO-AREA (919:2) TO FFORS-IO
               INSPECT FFORS-IO REPLACING ALL ' ' BY '0'
               MOVE FIRMAF-IO-AREA (916:1) TO FMIN (1:1)
               MOVE FIRMAF-IO-AREA (917:1) TO FFORPK (1:1)
               MOVE FIRMAF-IO-AREA (918:1) TO FRESTO (1:1)
               MOVE FIRMAF-IO-AREA (921:1) TO NULLS-X (1:1)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-03                        TO TRUE.
 
       VARETIL-FLDSET SECTION.
       VARETIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VARETIL-IO-AREA (1:2)  TO REC (1:2)
               MOVE VARETIL-IO-AREA (163:3) TO MAXVT-IO
           END-EVALUATE.
 
       VARETIL-IDSET SECTION.
       VARETIL-IDSET-P.
           SET I-06                        TO TRUE.
 
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
               MOVE STYFIL-IO-AREA (3:10)  TO KEYVM (1:10)
               MOVE STYFIL-IO-AREA (13:3)  TO ALF (1:3)
               MOVE STYFIL-IO-AREA (94:5)  TO VGR (1:5)
               MOVE STYFIL-IO-AREA (3:98)  TO LIST (1:98)
               MOVE STYFIL-IO-AREA (77:1)  TO MERK (1:1)
               MOVE STYFIL-IO-AREA (78:5)  TO ANTIN-IO
               MOVE STYFIL-IO-AREA (83:5)  TO ANTUT-IO
               MOVE STYFIL-IO-AREA (88:3)  TO KINNH-IO
               MOVE STYFIL-IO-AREA (91:3)  TO REST-IO
               MOVE STYFIL-IO-AREA (104:4) TO ANTBES-IO
               MOVE STYFIL-IO-AREA (134:3) TO MINB-IO
               MOVE STYFIL-IO-AREA (204:5) TO ANT-IO
               MOVE STYFIL-IO-AREA (200:1) TO REB (1:1)
               MOVE STYFIL-IO-AREA (209:5) TO ANT3M-IO
               MOVE STYFIL-IO-AREA (229:4) TO GJSNIT-IO
               MOVE STYFIL-IO-AREA (240:3) TO FP-IO
               INSPECT FP-IO REPLACING ALL ' ' BY '0'
               MOVE STYFIL-IO-AREA (243:3) TO FK-IO
               INSPECT FK-IO REPLACING ALL ' ' BY '0'
               MOVE STYFIL-IO-AREA (246:4) TO LTID-IO
               INSPECT LTID-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       STYFIL-IDSET SECTION.
       STYFIL-IDSET-P.
           SET I-02                        TO TRUE.
 
       STYFIL-CHK-LEVEL SECTION.
       STYFIL-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO STYFIL-LEVEL-02
               MOVE STYFIL-IO-AREA (3:3)   TO STYFIL-02-L2-FNR
               MOVE STYFIL-IO-AREA (13:3)  TO STYFIL-02-L1-ALF
               MOVE STYFIL-IO-AREA (94:5)  TO STYFIL-02-L1-VGR
               IF  STYFIL-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  STYFIL-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  STYFIL-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  STYFIL-02-L2          TO THE-PRIOR-L2
               MOVE  STYFIL-02-L1          TO THE-PRIOR-L1
               SET STYFIL-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       STYFIL-MATCH-SET SECTION.
       STYFIL-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE STYFIL-IO-AREA (3:3)   TO STYFIL-M-02-M3-FNR
               MOVE STYFIL-IO-AREA (94:5)  TO STYFIL-M-02-M2-VGR
               MOVE STYFIL-IO-AREA (13:3)  TO STYFIL-M-02-M1-ALF
           END-EVALUATE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  SKATFIL-EOF
               MOVE HIGH-VALUES            TO SKATFIL-MC
                                              SKATFIL-MP
           END-IF
           IF  STYFIL-EOF
               MOVE HIGH-VALUES            TO STYFIL-MC
                                              STYFIL-MP
           END-IF
           IF  SKATFIL-MC < SKATFIL-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  STYFIL-MC < STYFIL-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  SKATFIL-MC < STYFIL-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET SKATFIL-PROCESS     TO TRUE
                   MOVE SKATFIL-MC         TO SKATFIL-MP
                   IF  SKATFIL-MC = STYFIL-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  STYFIL-MC < SKATFIL-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET STYFIL-PROCESS      TO TRUE
                   MOVE STYFIL-MC          TO STYFIL-MP
                   IF  STYFIL-MC = SKATFIL-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  SKATFIL-MC = STYFIL-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET SKATFIL-PROCESS     TO TRUE
                   MOVE SKATFIL-MC         TO SKATFIL-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02)
               IF  (I-61)
                   MOVE KINNH              TO XO-50P
                   MOVE XO-50P-EF          TO STYFIL-IO-AREA (88:3)
               END-IF
               MOVE ANTFP                  TO XO-70P
               MOVE XO-70P-EF              TO STYFIL-IO-AREA (137:4)
               MOVE ANTFK                  TO XO-70P
               MOVE XO-70P-EF              TO STYFIL-IO-AREA (141:4)
               MOVE FORSL                  TO XO-60P
               MOVE XO-60P-EF              TO STYFIL-IO-AREA (229:4)
               MOVE KATTYP                 TO STYFIL-IO-AREA (250:1)
               REWRITE STYFIL-IO-AREA
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
           SET SKATFIL-LEVEL-INIT          TO TRUE
           INITIALIZE SKATFIL-DATA-FIELDS
           SET SKATFIL-EOF-OFF             TO TRUE
           SET SKATFIL-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO SKATFIL-MC
                                              SKATFIL-MP
           OPEN INPUT SKATFIL
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           INITIALIZE VARETIL-DATA-FIELDS
           OPEN INPUT VARETIL
           SET STYFIL-LEVEL-INIT           TO TRUE
           INITIALIZE STYFIL-DATA-FIELDS
           SET STYFIL-EOF-OFF              TO TRUE
           SET STYFIL-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO STYFIL-MC
                                              STYFIL-MP
           OPEN I-O STYFIL.
           PERFORM VARYING ARA-I FROM 1 BY 1
                     UNTIL ARA-I > ARA-MAX
               INITIALIZE ARA (ARA-I)
           END-PERFORM
           SET ARA-I                       TO 1
           PERFORM VARYING ARO-I FROM 1 BY 1
                     UNTIL ARO-I > ARO-MAX
               INITIALIZE ARO (ARO-I)
           END-PERFORM
           SET ARO-I                       TO 1
           PERFORM VARYING ARF-I FROM 1 BY 1
                     UNTIL ARF-I > ARF-MAX
               INITIALIZE ARF (ARF-I)
           END-PERFORM
           SET ARF-I                       TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE SKATFIL
           CLOSE FIRMAF
           CLOSE VARETIL
           CLOSE STYFIL.
 
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
