       IDENTIFICATION DIVISION.
       PROGRAM-ID. VAS020R.
      **********************************************  Z-WIN-RPG2   ****
      *   MERGE VARE.MASTER OG VARE.STAT.MASTER            *
      *   LEGGE INN SALGS.STATISTIKKDATA I OUTPUT.         *
      *   21 10 LAGT INN TEST PÅ TILGANGSDATO              *
      *   DIFF. MELLOM LAGERVERDI OG ABC1C2C3 ER DESIMALER *
      ******************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VAS020.rpg
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
           SELECT VARSTAM
               ASSIGN TO UT-S-VARSTAM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VARSTAM-STATUS.
           SELECT VAREMAS
               ASSIGN TO UT-S-VAREMAS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VAREMAS-STATUS.
           SELECT PARAM
               ASSIGN TO UT-S-PARAM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PARAM-STATUS.
           SELECT LAGPAR
               ASSIGN TO UT-S-LAGPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LAGPAR-STATUS.
           SELECT VARETIL
               ASSIGN TO VARETIL
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VARETIL-STATUS
               RECORD KEY IS VARETIL-KEY1.
           SELECT STATREC
               ASSIGN TO UT-S-STATREC
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS STATREC-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD VARSTAM
               BLOCK CONTAINS 9000
               RECORD CONTAINS 500.
       01  VARSTAM-IO-AREA.
           05  VARSTAM-IO-AREA-X           PICTURE X(500).
       FD VAREMAS
               BLOCK CONTAINS 9000
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X           PICTURE X(200).
       FD PARAM
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  PARAM-IO-AREA.
           05  PARAM-IO-AREA-X             PICTURE X(80).
       FD LAGPAR
               BLOCK CONTAINS 200
               RECORD CONTAINS 200.
       01  LAGPAR-IO-AREA.
           05  LAGPAR-IO-AREA-X            PICTURE X(200).
       FD VARETIL
               RECORD CONTAINS 200.
       01  VARETIL-IO-AREA.
           05  VARETIL-IO-AREA-X.
               10  VARETIL-KEY1            PICTURE X(12).
               10  FILLER                  PICTURE X(188).
       FD STATREC
               BLOCK CONTAINS 9000
               RECORD CONTAINS 250.
       01  STATREC-IO-AREA.
           05  STATREC-IO-AREA-X           PICTURE X(250).
       WORKING-STORAGE SECTION.
       77  A1A-MAX   VALUE 12              PICTURE 9(4) USAGE BINARY.
       77  A2A-MAX   VALUE 12              PICTURE 9(4) USAGE BINARY.
       77  A3A-MAX   VALUE 12              PICTURE 9(4) USAGE BINARY.
       77  A4A-MAX   VALUE 12              PICTURE 9(4) USAGE BINARY.
       77  ASA-MAX   VALUE 48              PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  A1A-TABLE.
               10  A1A-ENTRY
                                           OCCURS 12 TIMES
                                           INDEXED BY A1A-I
                                                      A1A-S.
                   15  A1A                 PICTURE S9(7)V9(2).
           05  A2A-TABLE.
               10  A2A-ENTRY
                                           OCCURS 12 TIMES
                                           INDEXED BY A2A-I
                                                      A2A-S.
                   15  A2A                 PICTURE S9(7)V9(2).
           05  A3A-TABLE.
               10  A3A-ENTRY
                                           OCCURS 12 TIMES
                                           INDEXED BY A3A-I
                                                      A3A-S.
                   15  A3A                 PICTURE S9(7)V9(2).
           05  A4A-TABLE.
               10  A4A-ENTRY
                                           OCCURS 12 TIMES
                                           INDEXED BY A4A-I
                                                      A4A-S.
                   15  A4A                 PICTURE S9(7)V9(2).
           05  ASA-TABLE.
               10  ASA-ENTRY
                                           OCCURS 48 TIMES
                                           INDEXED BY ASA-I
                                                      ASA-S.
                   15  ASA                 PICTURE S9(7)V9(2).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  VARSTAM-STATUS              PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  PARAM-STATUS                PICTURE 99 VALUE 0.
           10  LAGPAR-STATUS               PICTURE 99 VALUE 0.
           10  VARETIL-STATUS              PICTURE 99 VALUE 0.
           10  STATREC-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  VARSTAM-EOF-OFF         VALUE '0'.
               88  VARSTAM-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VARSTAM-READ-OFF        VALUE '0'.
               88  VARSTAM-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VARSTAM-PROCESS-OFF     VALUE '0'.
               88  VARSTAM-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  VARSTAM-LEVEL-INIT-OFF  VALUE '0'.
               88  VARSTAM-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREMAS-EOF-OFF         VALUE '0'.
               88  VAREMAS-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREMAS-READ-OFF        VALUE '0'.
               88  VAREMAS-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREMAS-PROCESS-OFF     VALUE '0'.
               88  VAREMAS-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  VAREMAS-LEVEL-INIT-OFF  VALUE '0'.
               88  VAREMAS-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-EOF-OFF           VALUE '0'.
               88  PARAM-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-READ-OFF          VALUE '0'.
               88  PARAM-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-PROCESS-OFF       VALUE '0'.
               88  PARAM-PROCESS           VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  LAGPAR-EOF-OFF          VALUE '0'.
               88  LAGPAR-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  LAGPAR-READ-OFF         VALUE '0'.
               88  LAGPAR-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  LAGPAR-PROCESS-OFF      VALUE '0'.
               88  LAGPAR-PROCESS          VALUE '1'.
           05  VARETIL-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  VARSTAM-LEVEL-01.
               10  VARSTAM-01-L2.
                   15  VARSTAM-01-L2-FIRMA PICTURE X(3).
               10  VARSTAM-01-L1.
                   15  VARSTAM-01-L1-EDBNR PICTURE X(7).
           05  VARSTAM-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  EDBNR                   PICTURE X(7).
               10  KEY-X                   PICTURE X(10).
               10  LK                      PICTURE X(2).
               10  XI-A1A-GRP.
                   15  XI-A1A              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XI-A2A-GRP.
                   15  XI-A2A              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XI-A3A-GRP.
                   15  XI-A3A              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XI-A4A-GRP.
                   15  XI-A4A              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
           05  VARSTAM-MP                  PICTURE X(10).
           05  VARSTAM-MC                  PICTURE X(10).
           05  VARSTAM-M-01            REDEFINES VARSTAM-MC.
               10  VARSTAM-M-01-M2.
                   15  VARSTAM-M-01-M2-EDBNR-G.
                       20  VARSTAM-M-01-M2-EDBNR PICTURE X(7).
               10  VARSTAM-M-01-M1.
                   15  VARSTAM-M-01-M1-FIRMA-G.
                       20  VARSTAM-M-01-M1-FIRMA PICTURE X(3).
           05  VAREMAS-LEVEL-02.
               10  VAREMAS-02-L2.
                   15  VAREMAS-02-L2-FIRMA PICTURE X(3).
               10  VAREMAS-02-L1.
                   15  VAREMAS-02-L1-EDBNR PICTURE X(7).
           05  VAREMAS-DATA-FIELDS.
               10  KEYV                    PICTURE X(10).
               10  ANTINN-IO.
                   15  ANTINN              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ANTUT-IO.
                   15  ANTUT               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  AAR                     PICTURE X(2).
               10  NYOPPR                  PICTURE X(4).
               10  TILDAT-IO.
                   15  TILDAT              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  VARREC                  PICTURE X(200).
               10  LAG13-IO.
                   15  LAG13               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  LAG93-IO.
                   15  LAG93               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  LAG15-IO.
                   15  LAG15               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  LAG17-IO.
                   15  LAG17               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  LAG92-IO.
                   15  LAG92               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  LAG18-IO.
                   15  LAG18               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
           05  VAREMAS-MP                  PICTURE X(10).
           05  VAREMAS-MC                  PICTURE X(10).
           05  VAREMAS-M-02            REDEFINES VAREMAS-MC.
               10  VAREMAS-M-02-M2.
                   15  VAREMAS-M-02-M2-EDBNR-G.
                       20  VAREMAS-M-02-M2-EDBNR PICTURE X(7).
               10  VAREMAS-M-02-M1.
                   15  VAREMAS-M-02-M1-FIRMA-G.
                       20  VAREMAS-M-02-M1-FIRMA PICTURE X(3).
           05  PARAM-DATA-FIELDS.
               10  PUSALG                  PICTURE X(1).
               10  PUSMND                  PICTURE X(2).
               10  PTILD                   PICTURE X(1).
               10  PUVBEH                  PICTURE X(1).
               10  OMSHST-IO.
                   15  OMSHST              PICTURE S9(2).
               10  PAAR                    PICTURE X(2).
               10  PNYPER                  PICTURE X(4).
               10  PLK                     PICTURE X(2).
               10  PARTIL-IO.
                   15  PARTIL              PICTURE S9(6).
           05  LAGPAR-DATA-FIELDS.
               10  M-IO.
                   15  M                   PICTURE S9(2).
           05  VARETIL-DATA-FIELDS.
               10  LOC13                   PICTURE X(6).
               10  MIN13-IO.
                   15  MIN13               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  LOC93                   PICTURE X(6).
               10  MIN93-IO.
                   15  MIN93               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  LOC15                   PICTURE X(6).
               10  MIN15-IO.
                   15  MIN15               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  LOC17                   PICTURE X(6).
               10  MIN17-IO.
                   15  MIN17               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  LOC92                   PICTURE X(6).
               10  MIN92-IO.
                   15  MIN92               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  LOC18                   PICTURE X(6).
               10  MIN18-IO.
                   15  MIN18               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(7).
           05  TEMPORARY-FIELDS.
               10  KEY12                   PICTURE X(12).
               10  BEHOLD-IO.
                   15  BEHOLD              PICTURE S9(7)V9(2).
               10  LAG10-IO.
                   15  LAG10               PICTURE S9(5).
               10  DATOT-IO.
                   15  DATOT               PICTURE S9(6).
               10  DDAG-IO.
                   15  DDAG                PICTURE S9(2).
               10  DAAR-IO.
                   15  DAAR                PICTURE S9(2).
               10  AAROPP                  PICTURE X(6).
               10  PAAROP                  PICTURE X(6).
               10  OMSTAL-IO.
                   15  OMSTAL              PICTURE S9(9).
               10  X-IO.
                   15  X                   PICTURE S9(2).
               10  Y-IO.
                   15  Y                   PICTURE S9(2).
               10  SALDMD-IO.
                   15  SALDMD              PICTURE S9(7)V9(2).
               10  SAL6MD-IO.
                   15  SAL6MD              PICTURE S9(7)V9(2).
               10  SA12MD-IO.
                   15  SA12MD              PICTURE S9(7)V9(2).
               10  SA24MD-IO.
                   15  SA24MD              PICTURE S9(7)V9(2).
               10  SA36MD-IO.
                   15  SA36MD              PICTURE S9(7)V9(2).
           05  EDITTING-FIELDS.
               10  XO-50P-EF.
                 15  XO-50P                PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  XO-60P-EF.
                 15  XO-60P                PICTURE S9(6) USAGE
                                                       PACKED-DECIMAL.
               10  XO-72P-EF.
                 15  XO-72P                PICTURE S9(7)V9(2) USAGE
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
           SET NOT-I-99                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-06                    TO TRUE
           SET NOT-I-07                    TO TRUE
           SET NOT-I-08                    TO TRUE
           SET NOT-I-10                    TO TRUE
           SET NOT-I-09                    TO TRUE
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  VARSTAM-PROCESS
               SET VARSTAM-PROCESS-OFF     TO TRUE
               SET VARSTAM-READ            TO TRUE
           END-IF
 
           IF  VARSTAM-READ
               PERFORM VARSTAM-GET
               SET VARSTAM-READ-OFF        TO TRUE
               IF  NOT VARSTAM-EOF
                   PERFORM VARSTAM-MATCH-SET
               END-IF
           END-IF
 
           IF  VAREMAS-PROCESS
               SET VAREMAS-PROCESS-OFF     TO TRUE
               SET VAREMAS-READ            TO TRUE
           END-IF
 
           IF  VAREMAS-READ
               PERFORM VAREMAS-GET
               SET VAREMAS-READ-OFF        TO TRUE
               IF  NOT VAREMAS-EOF
                   PERFORM VAREMAS-MATCH-SET
               END-IF
           END-IF
 
           IF  PARAM-PROCESS
               SET PARAM-PROCESS-OFF       TO TRUE
               SET PARAM-READ              TO TRUE
           END-IF
 
           IF  PARAM-READ
           AND RECORD-SELECTED-OFF
               PERFORM PARAM-GET
               SET PARAM-READ-OFF          TO TRUE
               IF  NOT PARAM-EOF
                   PERFORM PARAM-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET PARAM-PROCESS       TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LAGPAR-PROCESS
               SET LAGPAR-PROCESS-OFF      TO TRUE
               SET LAGPAR-READ             TO TRUE
           END-IF
 
           IF  LAGPAR-READ
           AND RECORD-SELECTED-OFF
               PERFORM LAGPAR-GET
               SET LAGPAR-READ-OFF         TO TRUE
               IF  NOT LAGPAR-EOF
                   SET LAGPAR-PROCESS      TO TRUE
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
 
           IF  VARSTAM-PROCESS
               PERFORM VARSTAM-IDSET
           END-IF
 
           IF  VAREMAS-PROCESS
               PERFORM VAREMAS-IDSET
           END-IF
 
           IF  PARAM-PROCESS
               PERFORM PARAM-IDSET
           END-IF
 
           IF  LAGPAR-PROCESS
               PERFORM LAGPAR-IDSET
           END-IF
 
           IF  VARSTAM-PROCESS
               PERFORM VARSTAM-CHK-LEVEL
           END-IF
 
           IF  VAREMAS-PROCESS
               PERFORM VAREMAS-CHK-LEVEL
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
 
           IF  VARSTAM-PROCESS
               PERFORM VARSTAM-FLDSET
           END-IF
 
           IF  VAREMAS-PROCESS
               PERFORM VAREMAS-FLDOFF
               PERFORM VAREMAS-FLDSET
           END-IF
 
           IF  PARAM-PROCESS
               PERFORM PARAM-FLDSET
           END-IF
 
           IF  LAGPAR-PROCESS
               PERFORM LAGPAR-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  VARSTAM-PROCESS
           OR  VAREMAS-PROCESS
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
               MOVE 0                      TO SALDMD
               MOVE 0                      TO SAL6MD
               MOVE 0                      TO SA12MD
               MOVE 0                      TO SA24MD
               MOVE 0                      TO SA36MD
           END-IF
           IF  (I-06)
               SET NOT-I-51                TO TRUE
               IF  PUSALG = 'J'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-06 AND I-51)
               SET NOT-I-52                TO TRUE
               IF  PUSMND = '06'
                   SET I-52                TO TRUE
               END-IF
               SET NOT-I-53                TO TRUE
               IF  PUSMND = '12'
                   SET I-53                TO TRUE
               END-IF
               SET NOT-I-54                TO TRUE
               IF  PUSMND = '24'
                   SET I-54                TO TRUE
               END-IF
               SET NOT-I-55                TO TRUE
               IF  PUSMND = '36'
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-06)
               SET NOT-I-95                TO TRUE
               IF  PTILD = 'J'
                   SET I-95                TO TRUE
               END-IF
           END-IF
           IF  (I-07)
               SET NOT-I-59                TO TRUE
               IF  PUVBEH = 'N'
                   SET I-59                TO TRUE
               END-IF
               SET NOT-I-56                TO TRUE
               IF  OMSHST > 0
                   SET I-56                TO TRUE
               END-IF
      *  01      LK        COMP "13"                     49 TEST LAGKODETIGHET
      * N49                GOTO SLUTT
           END-IF
           IF  (I-08 AND I-51)
               OR  (I-08 AND I-56)
               SET NOT-I-58                TO TRUE
               IF  PNYPER > '    '
                   SET I-58                TO TRUE
               END-IF
           END-IF
           IF  (I-06)
               GO TO SLUTT-T
           END-IF
           IF  (I-07)
               GO TO SLUTT-T
           END-IF
           IF  (I-08)
               GO TO SLUTT-T
           END-IF
           IF  (I-09)
               GO TO SLUTT-T
           END-IF
           IF  (I-10)
               GO TO SLUTT-T
      *  99                GOTO SLUTT
           END-IF
           IF  (I-01)
               GO TO STARUT-T
           END-IF
           IF  (I-02 AND NOT-I-U1)
               MOVE '81'                   TO KEY12 (1:2)
               MOVE KEYV                   TO KEY12 (3:10)
               MOVE KEY12                  TO VARETIL-KEY1
               READ VARETIL RECORD KEY IS VARETIL-KEY1
               INVALID KEY
                   SET I-99                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-99            TO TRUE
                   PERFORM VARETIL-FLDSET
                   PERFORM VARETIL-IDSET
               END-READ
           END-IF
           IF  (I-02)
               SET NOT-I-50                TO TRUE
           END-IF
           SET NOT-I-20                    TO TRUE
           IF  EDBNR = '9000000'
               SET I-20                    TO TRUE
           END-IF
           IF  (I-20)
               GO TO SLUTT-T
      *********  REGNE UT BEHOLDNING.                    *************
           END-IF
           SUBTRACT ANTUT FROM ANTINN  GIVING BEHOLD
           SET NOT-I-21                    TO TRUE
           IF  BEHOLD > 0
               SET I-21                    TO TRUE
           END-IF
           SUBTRACT LAG13 FROM BEHOLD  GIVING LAG10
           SUBTRACT LAG93                  FROM LAG10
           SUBTRACT LAG15                  FROM LAG10
           SUBTRACT LAG17                  FROM LAG10
           SUBTRACT LAG92                  FROM LAG10
           SUBTRACT LAG18                  FROM LAG10
           IF  (I-U1)
               ADD LAG10 TO ZERO       GIVING BEHOLD
               SET NOT-I-21                TO TRUE
               IF  BEHOLD NOT = 0
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-U2)
               ADD LAG13 TO ZERO       GIVING BEHOLD
               SET NOT-I-21                TO TRUE
               IF  BEHOLD NOT = 0
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-U3)
               ADD LAG93 TO ZERO       GIVING BEHOLD
               SET NOT-I-21                TO TRUE
               IF  BEHOLD NOT = 0
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-U4)
               ADD LAG15 TO ZERO       GIVING BEHOLD
               SET NOT-I-21                TO TRUE
               IF  BEHOLD NOT = 0
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-U5)
               ADD LAG17 TO ZERO       GIVING BEHOLD
               SET NOT-I-21                TO TRUE
               IF  BEHOLD NOT = 0
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-U6)
               ADD LAG92 TO ZERO       GIVING BEHOLD
               SET NOT-I-21                TO TRUE
               IF  BEHOLD NOT = 0
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-U7)
               ADD LAG18 TO ZERO       GIVING BEHOLD
               SET NOT-I-21                TO TRUE
               IF  BEHOLD NOT = 0
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-59 AND NOT-I-21)
               GO TO SLUTT-T
           END-IF
           ADD TILDAT TO ZERO          GIVING DATOT
           MOVE DATOT (1:2)                TO DDAG
           MOVE DATOT (5:2)                TO DAAR-IO
           MOVE DAAR                       TO DATOT (1:2)
           MOVE DDAG                       TO DATOT-IO (5:2)
           IF  (I-95)
               SET NOT-I-22                TO TRUE
               IF  DATOT NOT < PARTIL
                   SET I-22                TO TRUE
               END-IF
           END-IF
           IF  (I-95 AND I-22)
               GO TO SLUTT-T
           END-IF
           IF  (I-58)
               SET NOT-I-23                TO TRUE
               IF  AAR < '80'
                   SET I-23                TO TRUE
               END-IF
           END-IF
           IF  (I-58 AND I-23)
               MOVE '20'                   TO AAROPP (1:2)
           END-IF
           IF  (I-58 AND NOT-I-23)
               MOVE '19'                   TO AAROPP (1:2)
           END-IF
           IF  (I-58)
               MOVE NYOPPR                 TO AAROPP (3:4)
               SET NOT-I-24                TO TRUE
               IF  PAAR < '80'
                   SET I-24                TO TRUE
               END-IF
           END-IF
           IF  (I-58 AND I-24)
               MOVE '20'                   TO PAAROP (1:2)
           END-IF
           IF  (I-58 AND NOT-I-24)
               MOVE '19'                   TO PAAROP (1:2)
           END-IF
           IF  (I-58)
               MOVE PNYPER                 TO PAAROP (3:4)
               SET NOT-I-22                TO TRUE
               IF  AAROPP > PAAROP
                   SET I-22                TO TRUE
               END-IF
           END-IF
           IF  (I-58 AND I-22)
               GO TO SLUTT-T
           END-IF
           IF  (I-52 AND I-MR AND I-31)
               GO TO SLUTT-T
           END-IF
           IF  (I-53 AND I-MR AND I-32)
               GO TO SLUTT-T
           END-IF
           IF  (I-54 AND I-MR AND I-33)
               GO TO SLUTT-T
           END-IF
           IF  (I-55 AND I-MR AND I-34)
               GO TO SLUTT-T
           END-IF
           IF  (I-02)
               SET I-50                    TO TRUE
      *****************************************************************
      * RUTINE FOR Å LEGGE UT RECORD MED LAV OMSETINGSHASTIGHET.      *
      *****************************************************************
           END-IF
           IF  (NOT-I-02)
               GO TO SLUTT-T
           END-IF
           IF  (NOT-I-56)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-50                    TO TRUE
           IF  (NOT-I-21)
               GO TO SLUTT-T
           END-IF
           DIVIDE SA12MD BY BEHOLD     GIVING OMSTAL ROUNDED
           SET NOT-I-50                    TO TRUE
           IF  OMSTAL NOT > OMSHST
               SET I-50                    TO TRUE
           END-IF
           IF  (I-02)
               GO TO SLUTT-T
           END-IF.
 
       STARUT-T.
      *********  FLYTTE ALLE ARRAY"S SAMMEN TIL EN ARRAY  ************
           PERFORM VARYING ASA-I FROM 1 BY 1
                     UNTIL ASA-I > ASA-MAX
               MOVE 0                      TO ASA (ASA-I)
           END-PERFORM
           SET ASA-I                       TO 1
           SET ASA-I                       TO 1
           PERFORM VARYING A4A-I FROM 1 BY 1
                     UNTIL A4A-I > A4A-MAX
                        OR ASA-I > ASA-MAX
               ADD A4A (A4A-I)  TO ZERO GIVING ASA (ASA-I)
               SET ASA-I                UP BY 1
           END-PERFORM
           SET ASA-I                       TO 1
           ADD A3A (1) TO ZERO         GIVING ASA (13)
           ADD A3A (2) TO ZERO         GIVING ASA (14)
           ADD A3A (3) TO ZERO         GIVING ASA (15)
           ADD A3A (4) TO ZERO         GIVING ASA (16)
           ADD A3A (5) TO ZERO         GIVING ASA (17)
           ADD A3A (6) TO ZERO         GIVING ASA (18)
           ADD A3A (7) TO ZERO         GIVING ASA (19)
           ADD A3A (8) TO ZERO         GIVING ASA (20)
           ADD A3A (9) TO ZERO         GIVING ASA (21)
           ADD A3A (10) TO ZERO        GIVING ASA (22)
           ADD A3A (11) TO ZERO        GIVING ASA (23)
           ADD A3A (12) TO ZERO        GIVING ASA (24)
           ADD A2A (1) TO ZERO         GIVING ASA (25)
           ADD A2A (2) TO ZERO         GIVING ASA (26)
           ADD A2A (3) TO ZERO         GIVING ASA (27)
           ADD A2A (4) TO ZERO         GIVING ASA (28)
           ADD A2A (5) TO ZERO         GIVING ASA (29)
           ADD A2A (6) TO ZERO         GIVING ASA (30)
           ADD A2A (7) TO ZERO         GIVING ASA (31)
           ADD A2A (8) TO ZERO         GIVING ASA (32)
           ADD A2A (9) TO ZERO         GIVING ASA (33)
           ADD A2A (10) TO ZERO        GIVING ASA (34)
           ADD A2A (11) TO ZERO        GIVING ASA (35)
           ADD A2A (12) TO ZERO        GIVING ASA (36)
           ADD A1A (1) TO ZERO         GIVING ASA (37)
           ADD A1A (2) TO ZERO         GIVING ASA (38)
           ADD A1A (3) TO ZERO         GIVING ASA (39)
           ADD A1A (4) TO ZERO         GIVING ASA (40)
           ADD A1A (5) TO ZERO         GIVING ASA (41)
           ADD A1A (6) TO ZERO         GIVING ASA (42)
           ADD A1A (7) TO ZERO         GIVING ASA (43)
           ADD A1A (8) TO ZERO         GIVING ASA (44)
           ADD A1A (9) TO ZERO         GIVING ASA (45)
           ADD A1A (10) TO ZERO        GIVING ASA (46)
           ADD A1A (11) TO ZERO        GIVING ASA (47)
           ADD A1A (12) TO ZERO        GIVING ASA (48)
      *********  FINN INNEVÆRENDE MND I 4 ÅR"S PERIODEN  *************
           ADD 36 TO M                 GIVING X
      *          UMONTH    ADD  36        X       20        LEGG PÅ 3 ÅR.
           ADD 1                           TO X
           SET NOT-I-10                    TO TRUE
           IF  X > 48
               SET I-10                    TO TRUE
           END-IF
           IF  (I-10)
               SUBTRACT 12                 FROM X
           END-IF
           SUBTRACT 1 FROM X           GIVING Y
      *********  FINN SALG I ANT. INNEVÆRENDE MND.       *************
           ADD ASA (X) TO ZERO         GIVING SALDMD
      *********  FINN SALG I ANT. SISTE  6 MND.          *************
           ADD ASA (Y) TO ZERO         GIVING SAL6MD
           SUBTRACT 1                      FROM Y
           ADD ASA (Y)                     TO SAL6MD
           SUBTRACT 1                      FROM Y
           ADD ASA (Y)                     TO SAL6MD
           SUBTRACT 1                      FROM Y
           ADD ASA (Y)                     TO SAL6MD
           SUBTRACT 1                      FROM Y
           ADD ASA (Y)                     TO SAL6MD
           SUBTRACT 1                      FROM Y
           ADD ASA (Y)                     TO SAL6MD
           SET NOT-I-31                    TO TRUE
           IF  SAL6MD > 0
               SET I-31                    TO TRUE
           END-IF
      *********  FINN SALG I ANT. SISTE 12 MND.          *************
           ADD SAL6MD TO ZERO          GIVING SA12MD
           SUBTRACT 1                      FROM Y
           ADD ASA (Y)                     TO SA12MD
           SUBTRACT 1                      FROM Y
           ADD ASA (Y)                     TO SA12MD
           SUBTRACT 1                      FROM Y
           ADD ASA (Y)                     TO SA12MD
           SUBTRACT 1                      FROM Y
           ADD ASA (Y)                     TO SA12MD
           SUBTRACT 1                      FROM Y
           ADD ASA (Y)                     TO SA12MD
           SUBTRACT 1                      FROM Y
           ADD ASA (Y)                     TO SA12MD
           SET NOT-I-32                    TO TRUE
           IF  SA12MD > 0
               SET I-32                    TO TRUE
           END-IF
      *********  FINN SALG I ANT. SISTE 24 MND.          *************
           ADD SA12MD TO ZERO          GIVING SA24MD
           SUBTRACT 1                      FROM Y
           ADD ASA (Y)                     TO SA24MD
           SUBTRACT 1                      FROM Y
           ADD ASA (Y)                     TO SA24MD
           SUBTRACT 1                      FROM Y
           ADD ASA (Y)                     TO SA24MD
           SUBTRACT 1                      FROM Y
           ADD ASA (Y)                     TO SA24MD
           SUBTRACT 1                      FROM Y
           ADD ASA (Y)                     TO SA24MD
           SUBTRACT 1                      FROM Y
           ADD ASA (Y)                     TO SA24MD
           SUBTRACT 1                      FROM Y
           ADD ASA (Y)                     TO SA24MD
           SUBTRACT 1                      FROM Y
           ADD ASA (Y)                     TO SA24MD
           SUBTRACT 1                      FROM Y
           ADD ASA (Y)                     TO SA24MD
           SUBTRACT 1                      FROM Y
           ADD ASA (Y)                     TO SA24MD
           SUBTRACT 1                      FROM Y
           ADD ASA (Y)                     TO SA24MD
           SUBTRACT 1                      FROM Y
           ADD ASA (Y)                     TO SA24MD
           SET NOT-I-33                    TO TRUE
           IF  SA24MD > 0
               SET I-33                    TO TRUE
           END-IF
      *********  FINN SALG I ANT. SISTE 36 MND.          *************
           ADD SA24MD TO ZERO          GIVING SA36MD
           SUBTRACT 1                      FROM Y
           ADD ASA (Y)                     TO SA36MD
           SUBTRACT 1                      FROM Y
           ADD ASA (Y)                     TO SA36MD
           SUBTRACT 1                      FROM Y
           ADD ASA (Y)                     TO SA36MD
           SUBTRACT 1                      FROM Y
           ADD ASA (Y)                     TO SA36MD
           SUBTRACT 1                      FROM Y
           ADD ASA (Y)                     TO SA36MD
           SUBTRACT 1                      FROM Y
           ADD ASA (Y)                     TO SA36MD
           SUBTRACT 1                      FROM Y
           ADD ASA (Y)                     TO SA36MD
           SUBTRACT 1                      FROM Y
           ADD ASA (Y)                     TO SA36MD
           SUBTRACT 1                      FROM Y
           ADD ASA (Y)                     TO SA36MD
           SUBTRACT 1                      FROM Y
           ADD ASA (Y)                     TO SA36MD
           SUBTRACT 1                      FROM Y
           ADD ASA (Y)                     TO SA36MD
           SUBTRACT 1                      FROM Y
           ADD ASA (Y)                     TO SA36MD
           SET NOT-I-34                    TO TRUE
           IF  SA36MD > 0
               SET I-34                    TO TRUE
           END-IF.
 
       SLUTT-T.
           CONTINUE.
 
       VARSTAM-GET SECTION.
       VARSTAM-GET-P.
           IF  VARSTAM-EOF-OFF
               READ VARSTAM
               AT END
                   SET VARSTAM-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VARSTAM-FLDSET SECTION.
       VARSTAM-FLDSET-P.
           EVALUATE TRUE
           WHEN ( VARSTAM-IO-AREA (12:1) = '0'
            AND   VARSTAM-IO-AREA (13:1) = '0' )
               MOVE VARSTAM-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE VARSTAM-IO-AREA (5:7)  TO EDBNR (1:7)
               MOVE VARSTAM-IO-AREA (2:10) TO KEY-X (1:10)
               MOVE VARSTAM-IO-AREA (12:2) TO LK (1:2)
               MOVE 91                     TO BW-A
               PERFORM VARYING A1A-I FROM A1A-MAX BY -1
                         UNTIL A1A-I < 1
                   SUBTRACT 5            FROM BW-A
                   MOVE VARSTAM-IO-AREA (BW-A:5) TO XI-A1A-GRP
                   MOVE XI-A1A             TO A1A (A1A-I)
               END-PERFORM
               MOVE 203                    TO BW-A
               PERFORM VARYING A2A-I FROM A2A-MAX BY -1
                         UNTIL A2A-I < 1
                   SUBTRACT 5            FROM BW-A
                   MOVE VARSTAM-IO-AREA (BW-A:5) TO XI-A2A-GRP
                   MOVE XI-A2A             TO A2A (A2A-I)
               END-PERFORM
               MOVE 315                    TO BW-A
               PERFORM VARYING A3A-I FROM A3A-MAX BY -1
                         UNTIL A3A-I < 1
                   SUBTRACT 5            FROM BW-A
                   MOVE VARSTAM-IO-AREA (BW-A:5) TO XI-A3A-GRP
                   MOVE XI-A3A             TO A3A (A3A-I)
               END-PERFORM
               MOVE 427                    TO BW-A
               PERFORM VARYING A4A-I FROM A4A-MAX BY -1
                         UNTIL A4A-I < 1
                   SUBTRACT 5            FROM BW-A
                   MOVE VARSTAM-IO-AREA (BW-A:5) TO XI-A4A-GRP
                   MOVE XI-A4A             TO A4A (A4A-I)
               END-PERFORM
           END-EVALUATE.
 
       VARSTAM-IDSET SECTION.
       VARSTAM-IDSET-P.
           EVALUATE TRUE
           WHEN ( VARSTAM-IO-AREA (12:1) = '0'
            AND   VARSTAM-IO-AREA (13:1) = '0' )
               SET I-01                    TO TRUE
           WHEN  OTHER
               SET I-99                    TO TRUE
           END-EVALUATE.
 
       VARSTAM-CHK-LEVEL SECTION.
       VARSTAM-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( VARSTAM-IO-AREA (12:1) = '0'
            AND   VARSTAM-IO-AREA (13:1) = '0' )
               MOVE LOW-VALUES             TO VARSTAM-LEVEL-01
               MOVE VARSTAM-IO-AREA (2:3)  TO VARSTAM-01-L2-FIRMA
               MOVE VARSTAM-IO-AREA (5:7)  TO VARSTAM-01-L1-EDBNR
               IF  VARSTAM-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VARSTAM-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  VARSTAM-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  VARSTAM-01-L2         TO THE-PRIOR-L2
               MOVE  VARSTAM-01-L1         TO THE-PRIOR-L1
               SET VARSTAM-LEVEL-INIT      TO TRUE
           WHEN OTHER
               CONTINUE
           END-EVALUATE.
 
       VARSTAM-MATCH-SET SECTION.
       VARSTAM-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( VARSTAM-IO-AREA (12:1) = '0'
            AND   VARSTAM-IO-AREA (13:1) = '0' )
               MOVE VARSTAM-IO-AREA (5:7)  TO VARSTAM-M-01-M2-EDBNR
               MOVE VARSTAM-IO-AREA (2:3)  TO VARSTAM-M-01-M1-FIRMA
           WHEN OTHER
               SET NOT-CALL-MATCH-RECS     TO TRUE
           END-EVALUATE.
 
       VAREMAS-GET SECTION.
       VAREMAS-GET-P.
           IF  VAREMAS-EOF-OFF
               READ VAREMAS
               AT END
                   SET VAREMAS-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VAREMAS-FLDOFF SECTION.
       VAREMAS-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-41                TO TRUE
               SET NOT-I-42                TO TRUE
               SET NOT-I-43                TO TRUE
               SET NOT-I-44                TO TRUE
               SET NOT-I-45                TO TRUE
               SET NOT-I-46                TO TRUE
           END-EVALUATE.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE VAREMAS-IO-AREA (6:7)  TO EDBNR (1:7)
               MOVE VAREMAS-IO-AREA (3:10) TO KEYV (1:10)
               MOVE VAREMAS-IO-AREA (97:5) TO ANTINN-IO
               MOVE VAREMAS-IO-AREA (102:5) TO ANTUT-IO
               MOVE VAREMAS-IO-AREA (171:2) TO AAR (1:2)
               MOVE VAREMAS-IO-AREA (171:4) TO NYOPPR (1:4)
               MOVE VAREMAS-IO-AREA (156:4) TO TILDAT-IO
               MOVE VAREMAS-IO-AREA (1:200) TO VARREC (1:200)
               MOVE VAREMAS-IO-AREA (179:3) TO LAG13-IO
               IF  LAG13 = ZERO
                   SET I-41                TO TRUE
               END-IF
               MOVE VAREMAS-IO-AREA (182:3) TO LAG93-IO
               IF  LAG93 = ZERO
                   SET I-42                TO TRUE
               END-IF
               MOVE VAREMAS-IO-AREA (185:3) TO LAG15-IO
               IF  LAG15 = ZERO
                   SET I-43                TO TRUE
               END-IF
               MOVE VAREMAS-IO-AREA (188:3) TO LAG17-IO
               IF  LAG17 = ZERO
                   SET I-44                TO TRUE
               END-IF
               MOVE VAREMAS-IO-AREA (191:3) TO LAG92-IO
               IF  LAG92 = ZERO
                   SET I-45                TO TRUE
               END-IF
               MOVE VAREMAS-IO-AREA (194:3) TO LAG18-IO
               IF  LAG18 = ZERO
                   SET I-46                TO TRUE
               END-IF
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-02                        TO TRUE.
 
       VAREMAS-CHK-LEVEL SECTION.
       VAREMAS-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO VAREMAS-LEVEL-02
               MOVE VAREMAS-IO-AREA (3:3)  TO VAREMAS-02-L2-FIRMA
               MOVE VAREMAS-IO-AREA (6:7)  TO VAREMAS-02-L1-EDBNR
               IF  VAREMAS-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VAREMAS-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  VAREMAS-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  VAREMAS-02-L2         TO THE-PRIOR-L2
               MOVE  VAREMAS-02-L1         TO THE-PRIOR-L1
               SET VAREMAS-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       VAREMAS-MATCH-SET SECTION.
       VAREMAS-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (6:7)  TO VAREMAS-M-02-M2-EDBNR
               MOVE VAREMAS-IO-AREA (3:3)  TO VAREMAS-M-02-M1-FIRMA
           END-EVALUATE.
 
       PARAM-GET SECTION.
       PARAM-GET-P.
           IF  PARAM-EOF-OFF
               READ PARAM
               AT END
                   SET PARAM-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       PARAM-FLDSET SECTION.
       PARAM-FLDSET-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'P'
            AND   PARAM-IO-AREA (2:1) = '0'
            AND   PARAM-IO-AREA (3:1) = '1' )
               MOVE PARAM-IO-AREA (31:1)   TO PUSALG (1:1)
               MOVE PARAM-IO-AREA (61:2)   TO PUSMND (1:2)
               MOVE PARAM-IO-AREA (70:1)   TO PTILD (1:1)
           WHEN ( PARAM-IO-AREA (1:1) = 'P'
            AND   PARAM-IO-AREA (2:1) = '0'
            AND   PARAM-IO-AREA (3:1) = '2' )
               MOVE PARAM-IO-AREA (42:1)   TO PUVBEH (1:1)
               MOVE PARAM-IO-AREA (61:2)   TO OMSHST-IO
               INSPECT OMSHST-IO REPLACING ALL ' ' BY '0'
           WHEN ( PARAM-IO-AREA (1:1) = 'P'
            AND   PARAM-IO-AREA (2:1) = '0'
            AND   PARAM-IO-AREA (3:1) = '3' )
               MOVE PARAM-IO-AREA (49:2)   TO PAAR (1:2)
               MOVE PARAM-IO-AREA (49:4)   TO PNYPER (1:4)
               MOVE PARAM-IO-AREA (60:2)   TO PLK (1:2)
               MOVE PARAM-IO-AREA (65:6)   TO PARTIL-IO
               INSPECT PARTIL-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       PARAM-IDCHK SECTION.
       PARAM-IDCHK-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'P'
            AND   PARAM-IO-AREA (2:1) = '0'
            AND   PARAM-IO-AREA (3:1) = '1' )
             OR ( PARAM-IO-AREA (1:1) = 'P'
            AND   PARAM-IO-AREA (2:1) = '0'
            AND   PARAM-IO-AREA (3:1) = '2' )
             OR ( PARAM-IO-AREA (1:1) = 'P'
            AND   PARAM-IO-AREA (2:1) = '0'
            AND   PARAM-IO-AREA (3:1) = '3' )
             OR ( PARAM-IO-AREA (1:1) = 'P'
            AND   PARAM-IO-AREA (2:1) = '*'
            AND   PARAM-IO-AREA (3:1) = '*' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       PARAM-IDSET SECTION.
       PARAM-IDSET-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'P'
            AND   PARAM-IO-AREA (2:1) = '0'
            AND   PARAM-IO-AREA (3:1) = '1' )
               SET I-06                    TO TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'P'
            AND   PARAM-IO-AREA (2:1) = '0'
            AND   PARAM-IO-AREA (3:1) = '2' )
               SET I-07                    TO TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'P'
            AND   PARAM-IO-AREA (2:1) = '0'
            AND   PARAM-IO-AREA (3:1) = '3' )
               SET I-08                    TO TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'P'
            AND   PARAM-IO-AREA (2:1) = '*'
            AND   PARAM-IO-AREA (3:1) = '*' )
               SET I-10                    TO TRUE
           END-EVALUATE.
 
       LAGPAR-GET SECTION.
       LAGPAR-GET-P.
           IF  LAGPAR-EOF-OFF
               READ LAGPAR
               AT END
                   SET LAGPAR-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       LAGPAR-FLDSET SECTION.
       LAGPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LAGPAR-IO-AREA (5:2)   TO M-IO
               INSPECT M-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       LAGPAR-IDSET SECTION.
       LAGPAR-IDSET-P.
           SET I-09                        TO TRUE.
 
       VARETIL-FLDSET SECTION.
       VARETIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VARETIL-IO-AREA (13:6) TO LOC13 (1:6)
               MOVE VARETIL-IO-AREA (19:3) TO MIN13-IO
               MOVE VARETIL-IO-AREA (38:6) TO LOC93 (1:6)
               MOVE VARETIL-IO-AREA (44:3) TO MIN93-IO
               MOVE VARETIL-IO-AREA (63:6) TO LOC15 (1:6)
               MOVE VARETIL-IO-AREA (69:3) TO MIN15-IO
               MOVE VARETIL-IO-AREA (88:6) TO LOC17 (1:6)
               MOVE VARETIL-IO-AREA (94:3) TO MIN17-IO
               MOVE VARETIL-IO-AREA (113:6) TO LOC92 (1:6)
               MOVE VARETIL-IO-AREA (119:3) TO MIN92-IO
               MOVE VARETIL-IO-AREA (138:6) TO LOC18 (1:6)
               MOVE VARETIL-IO-AREA (144:3) TO MIN18-IO
           END-EVALUATE.
 
       VARETIL-IDSET SECTION.
       VARETIL-IDSET-P.
           SET I-03                        TO TRUE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  VARSTAM-EOF
               MOVE HIGH-VALUES            TO VARSTAM-MC
                                              VARSTAM-MP
           END-IF
           IF  VAREMAS-EOF
               MOVE HIGH-VALUES            TO VAREMAS-MC
                                              VAREMAS-MP
           END-IF
           IF  VARSTAM-MC < VARSTAM-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  VAREMAS-MC < VAREMAS-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  VARSTAM-MC < VAREMAS-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VARSTAM-PROCESS     TO TRUE
                   MOVE VARSTAM-MC         TO VARSTAM-MP
                   IF  VARSTAM-MC = VAREMAS-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  VAREMAS-MC < VARSTAM-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VAREMAS-PROCESS     TO TRUE
                   MOVE VAREMAS-MC         TO VAREMAS-MP
                   IF  VAREMAS-MC = VARSTAM-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  VARSTAM-MC = VAREMAS-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VARSTAM-PROCESS     TO TRUE
                   MOVE VARSTAM-MC         TO VARSTAM-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND I-50)
               MOVE SPACES TO STATREC-IO-AREA
               INITIALIZE STATREC-IO-AREA
               MOVE VARREC                 TO STATREC-IO-AREA (1:200)
               IF  (I-U2)
                   MOVE LOC13              TO STATREC-IO-AREA (140:6)
               END-IF
               IF  (I-U2)
                   MOVE MIN13              TO XO-50P
                   MOVE XO-50P-EF          TO STATREC-IO-AREA (197:3)
               END-IF
               IF  (I-U3)
                   MOVE LOC93              TO STATREC-IO-AREA (140:6)
               END-IF
               IF  (I-U3)
                   MOVE MIN93              TO XO-50P
                   MOVE XO-50P-EF          TO STATREC-IO-AREA (197:3)
               END-IF
               IF  (I-U4)
                   MOVE LOC15              TO STATREC-IO-AREA (140:6)
               END-IF
               IF  (I-U4)
                   MOVE MIN15              TO XO-50P
                   MOVE XO-50P-EF          TO STATREC-IO-AREA (197:3)
               END-IF
               IF  (I-U5)
                   MOVE LOC17              TO STATREC-IO-AREA (140:6)
               END-IF
               IF  (I-U5)
                   MOVE MIN17              TO XO-50P
                   MOVE XO-50P-EF          TO STATREC-IO-AREA (197:3)
               END-IF
               IF  (I-U6)
                   MOVE LOC92              TO STATREC-IO-AREA (140:6)
               END-IF
               IF  (I-U6)
                   MOVE MIN92              TO XO-50P
                   MOVE XO-50P-EF          TO STATREC-IO-AREA (197:3)
               END-IF
               IF  (I-U7)
                   MOVE LOC18              TO STATREC-IO-AREA (140:6)
               END-IF
               IF  (I-U7)
                   MOVE MIN18              TO XO-50P
                   MOVE XO-50P-EF          TO STATREC-IO-AREA (197:3)
               END-IF
               MOVE DATOT                  TO XO-60P
               MOVE XO-60P-EF              TO STATREC-IO-AREA (156:4)
               MOVE SALDMD                 TO XO-72P
               MOVE XO-72P-EF              TO STATREC-IO-AREA (201:5)
               INITIALIZE SALDMD
               MOVE SAL6MD                 TO XO-72P
               MOVE XO-72P-EF              TO STATREC-IO-AREA (206:5)
               INITIALIZE SAL6MD
               MOVE SA12MD                 TO XO-72P
               MOVE XO-72P-EF              TO STATREC-IO-AREA (211:5)
               INITIALIZE SA12MD
               MOVE SA24MD                 TO XO-72P
               MOVE XO-72P-EF              TO STATREC-IO-AREA (216:5)
               INITIALIZE SA24MD
               MOVE SA36MD                 TO XO-72P
               MOVE XO-72P-EF              TO STATREC-IO-AREA (221:5)
               INITIALIZE SA36MD
               MOVE BEHOLD                 TO XO-72P
               MOVE XO-72P-EF              TO STATREC-IO-AREA (246:5)
               WRITE STATREC-IO-AREA
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
           MOVE 4                          TO LR-CHECK
           SET VARSTAM-LEVEL-INIT          TO TRUE
           INITIALIZE VARSTAM-DATA-FIELDS
           SET VARSTAM-EOF-OFF             TO TRUE
           SET VARSTAM-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO VARSTAM-MC
                                              VARSTAM-MP
           OPEN INPUT VARSTAM
           SET VAREMAS-LEVEL-INIT          TO TRUE
           INITIALIZE VAREMAS-DATA-FIELDS
           SET VAREMAS-EOF-OFF             TO TRUE
           SET VAREMAS-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO VAREMAS-MC
                                              VAREMAS-MP
           OPEN INPUT VAREMAS
           INITIALIZE PARAM-DATA-FIELDS
           SET PARAM-EOF-OFF               TO TRUE
           SET PARAM-PROCESS               TO TRUE
           OPEN INPUT PARAM
           INITIALIZE LAGPAR-DATA-FIELDS
           SET LAGPAR-EOF-OFF              TO TRUE
           SET LAGPAR-PROCESS              TO TRUE
           OPEN INPUT LAGPAR
           INITIALIZE VARETIL-DATA-FIELDS
           OPEN INPUT VARETIL
           OPEN OUTPUT STATREC.
           PERFORM VARYING A1A-I FROM 1 BY 1
                     UNTIL A1A-I > A1A-MAX
               INITIALIZE A1A (A1A-I)
           END-PERFORM
           SET A1A-I                       TO 1
           PERFORM VARYING A2A-I FROM 1 BY 1
                     UNTIL A2A-I > A2A-MAX
               INITIALIZE A2A (A2A-I)
           END-PERFORM
           SET A2A-I                       TO 1
           PERFORM VARYING A3A-I FROM 1 BY 1
                     UNTIL A3A-I > A3A-MAX
               INITIALIZE A3A (A3A-I)
           END-PERFORM
           SET A3A-I                       TO 1
           PERFORM VARYING A4A-I FROM 1 BY 1
                     UNTIL A4A-I > A4A-MAX
               INITIALIZE A4A (A4A-I)
           END-PERFORM
           SET A4A-I                       TO 1
           PERFORM VARYING ASA-I FROM 1 BY 1
                     UNTIL ASA-I > ASA-MAX
               INITIALIZE ASA (ASA-I)
           END-PERFORM
           SET ASA-I                       TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE VARSTAM
           CLOSE VAREMAS
           CLOSE PARAM
           CLOSE LAGPAR
           CLOSE VARETIL
           CLOSE STATREC.
 
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
