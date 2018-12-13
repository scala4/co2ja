       IDENTIFICATION DIVISION.
       PROGRAM-ID. RKO048R.
      **********************************************  Z-WIN-RPG2P     *
      * KOPI av RKO.RKO047
      *  PROGRAM.......: RKO048                                       *
      *  PROGRAMMET DANNER NY TEST-RESKONTROMASTER I FAK.XFAK12E      *
      * Endring: xx.xx.xx  tekst
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RKO048.rpg
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
           SELECT LINJENR
               ASSIGN TO UT-S-LINJENR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LINJENR-STATUS.
           SELECT RSKINF
               ASSIGN TO UT-S-RSKINF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RSKINF-STATUS.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT RESKOMT
               ASSIGN TO RESKOMT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS RESKOMT-STATUS
               RECORD KEY IS RESKOMT-KEY1.
           SELECT AFRECF
               ASSIGN TO UT-S-AFRECF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS AFRECF-STATUS.
           SELECT SUMFILE
               ASSIGN TO UT-S-SUMFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS SUMFILE-STATUS.
           SELECT AVSTEMM
               ASSIGN TO UT-S-AVSTEMM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS AVSTEMM-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD LINJENR
               BLOCK CONTAINS 200
               RECORD CONTAINS 20.
       01  LINJENR-IO-AREA.
           05  LINJENR-IO-AREA-X           PICTURE X(20).
       FD RSKINF
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  RSKINF-IO-AREA.
           05  RSKINF-IO-AREA-X            PICTURE X(200).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
      *ESKOMA O   F     200            KSDS                           U1
       FD RESKOMT
               RECORD CONTAINS 200.
       01  RESKOMT-IO-AREA.
           05  RESKOMT-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  RESKOMT-KEY1.
                   15  RESKOMT-KEY1N       PICTURE S9(14).
               10  FILLER                  PICTURE X(185).
       FD AFRECF
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  AFRECF-IO-AREA.
           05  AFRECF-IO-AREA-X            PICTURE X(200).
       FD SUMFILE
               BLOCK CONTAINS 200
               RECORD CONTAINS 20.
       01  SUMFILE-IO-AREA.
           05  SUMFILE-IO-AREA-X           PICTURE X(20).
       FD AVSTEMM
               BLOCK CONTAINS 120
               RECORD CONTAINS 120.
       01  AVSTEMM-IO-AREA.
           05  AVSTEMM-IO-AREA-X           PICTURE X(120).
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  LINJENR-STATUS              PICTURE 99 VALUE 0.
           10  RSKINF-STATUS               PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  RESKOMT-STATUS              PICTURE 99 VALUE 0.
           10  AFRECF-STATUS               PICTURE 99 VALUE 0.
           10  SUMFILE-STATUS              PICTURE 99 VALUE 0.
           10  AVSTEMM-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  LINJENR-EOF-OFF         VALUE '0'.
               88  LINJENR-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  LINJENR-READ-OFF        VALUE '0'.
               88  LINJENR-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  LINJENR-PROCESS-OFF     VALUE '0'.
               88  LINJENR-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  LINJENR-LEVEL-INIT-OFF  VALUE '0'.
               88  LINJENR-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RSKINF-EOF-OFF          VALUE '0'.
               88  RSKINF-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RSKINF-READ-OFF         VALUE '0'.
               88  RSKINF-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RSKINF-PROCESS-OFF      VALUE '0'.
               88  RSKINF-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  RSKINF-LEVEL-INIT-OFF   VALUE '0'.
               88  RSKINF-LEVEL-INIT       VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  RESKOMT-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  LISTE-DATA-FIELDS.
               10  LISTE-AFTER-SPACE       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE-AFTER-SKIP        PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE-BEFORE-SPACE      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE-BEFORE-SKIP       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE-MAX-LINES         PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE-LINE-COUNT        PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE-CLR-IO            PICTURE X VALUE 'Y'.
           05  LINJENR-LEVEL-01.
               10  LINJENR-01-L2.
                   15  LINJENR-01-L2-FNR1  PICTURE X(3).
               10  LINJENR-01-L1.
                   15  LINJENR-01-L1-RES1  PICTURE X(6).
           05  LINJENR-DATA-FIELDS.
               10  FNR1                    PICTURE X(3).
               10  RES1                    PICTURE X(6).
      *                                      12  16 LINJE
               10  AFPOST                  PICTURE X(1).
               10  REF0                    PICTURE X(1).
           05  LINJENR-MP                  PICTURE X(9).
           05  LINJENR-MC                  PICTURE X(9).
           05  LINJENR-M-01            REDEFINES LINJENR-MC.
               10  LINJENR-M-01-M2.
                   15  LINJENR-M-01-M2-FNR1-G.
                       20  LINJENR-M-01-M2-FNR1 PICTURE X(3).
               10  LINJENR-M-01-M1.
                   15  LINJENR-M-01-M1-RES1-G.
                       20  LINJENR-M-01-M1-RES1 PICTURE X(6).
           05  RSKINF-LEVEL-02.
               10  RSKINF-02-L2.
                   15  RSKINF-02-L2-FNR1   PICTURE X(3).
               10  RSKINF-02-L1.
                   15  RSKINF-02-L1-RES1   PICTURE X(6).
           05  RSKINF-DATA-FIELDS.
               10  REC                     PICTURE X(200).
               10  BEL-IO.
                   15  BEL                 PICTURE S9(7)V9(2).
               10  VAL-IO.
                   15  VAL                 PICTURE S9(8)V9(2).
               10  BELU-IO.
                   15  BELU                PICTURE S9(11)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  VALU-IO.
                   15  VALU                PICTURE S9(11)V9(4) USAGE
                                                       PACKED-DECIMAL.
           05  RSKINF-MP                   PICTURE X(9).
           05  RSKINF-MC                   PICTURE X(9).
           05  RSKINF-M-02             REDEFINES RSKINF-MC.
               10  RSKINF-M-02-M2.
                   15  RSKINF-M-02-M2-FNR1-G.
                       20  RSKINF-M-02-M2-FNR1 PICTURE X(3).
               10  RSKINF-M-02-M1.
                   15  RSKINF-M-02-M1-RES1-G.
                       20  RSKINF-M-02-M1-RES1 PICTURE X(6).
           05  FIRMAF-DATA-FIELDS.
      *                                     784 784 KREDGR
               10  FILLER                  PICTURE X.
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  SUM-X-IO.
                   15  SUM-X               PICTURE S9(7)V9(2).
               10  TOTREC-IO.
                   15  TOTREC              PICTURE S9(6).
               10  TOTBEL-IO.
                   15  TOTBEL              PICTURE S9(9)V9(2).
               10  TOTVAL-IO.
                   15  TOTVAL              PICTURE S9(10)V9(2).
               10  TOTBEU-IO.
                   15  TOTBEU              PICTURE S9(11)V9(2).
               10  TOTVAU-IO.
                   15  TOTVAU              PICTURE S9(11)V9(4).
               10  TOT0R-IO.
                   15  TOT0R               PICTURE S9(6).
               10  DTO6-IO.
                   15  DTO6                PICTURE S9(6).
               10  DD-IO.
                   15  DD                  PICTURE S9(2).
               10  AA-IO.
                   15  AA                  PICTURE S9(2).
               10  DDATO                   PICTURE X(6).
               10  DDATO8                  PICTURE X(8).
               10  TIDSP-IO.
                   15  TIDSP               PICTURE S9(6).
               10  LRRSAN-IO.
                   15  LRRSAN              PICTURE S9(13).
               10  LRRSBE-IO.
                   15  LRRSBE              PICTURE S9(13)V9(2).
               10  LRRSBU-IO.
                   15  LRRSBU              PICTURE S9(13)V9(2).
           05  EDITTING-FIELDS.
               10  XO-72P-EF.
                 15  XO-72P                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-60YNZ                PICTURE ZZZZZZ.
               10  EDIT-TOTBEL             PICTURE ZZZ.ZZZ.ZZZ,ZZ-.
               10  XO-102YY9R              PICTURE Z.ZZZ.ZZZ.ZZZ,99-.
               10  XO-112YY9R              PICTURE ZZ.ZZZ.ZZZ.ZZZ,99-.
               10  XO-114YY9R              PICTURE
                                                 ZZ.ZZZ.ZZZ.ZZZ,9999-.
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
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  LINJENR-PROCESS
               SET LINJENR-PROCESS-OFF     TO TRUE
               SET LINJENR-READ            TO TRUE
           END-IF
 
           IF  LINJENR-READ
               PERFORM LINJENR-GET
               SET LINJENR-READ-OFF        TO TRUE
               IF  NOT LINJENR-EOF
                   PERFORM LINJENR-MATCH-SET
               END-IF
           END-IF
 
           IF  RSKINF-PROCESS
               SET RSKINF-PROCESS-OFF      TO TRUE
               SET RSKINF-READ             TO TRUE
           END-IF
 
           IF  RSKINF-READ
               PERFORM RSKINF-GET
               SET RSKINF-READ-OFF         TO TRUE
               IF  NOT RSKINF-EOF
                   PERFORM RSKINF-MATCH-SET
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
 
           IF  LINJENR-PROCESS
               PERFORM LINJENR-IDSET
           END-IF
 
           IF  RSKINF-PROCESS
               PERFORM RSKINF-IDSET
           END-IF
 
           IF  LINJENR-PROCESS
               PERFORM LINJENR-CHK-LEVEL
           END-IF
 
           IF  RSKINF-PROCESS
               PERFORM RSKINF-CHK-LEVEL
           END-IF
 
           IF I-1ST
               GO TO LR-INDICATOR-TEST
           END-IF.
 
       MAINLINE-TOTAL-CALCS.
           IF  I-LR
               PERFORM LR-CALCS
           END-IF
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
 
           IF  LINJENR-PROCESS
               PERFORM LINJENR-FLDOFF
               PERFORM LINJENR-FLDSET
           END-IF
 
           IF  RSKINF-PROCESS
               PERFORM RSKINF-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  LINJENR-PROCESS
           OR  RSKINF-PROCESS
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
               SET NOT-I-60                TO TRUE
               SET NOT-I-14                TO TRUE
               IF  AFPOST = 'X'
                   SET I-14                TO TRUE
               END-IF
           END-IF
           IF  (I-L2)
               SET NOT-I-50                TO TRUE
           END-IF
           IF  (I-L2 AND I-50)
               MOVE FNR1                   TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-04                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-04            TO TRUE
                   PERFORM FIRMAF-IDSET
               END-READ
      *  L2N04   KREDGR    COMP "J"                      50 SKAL SUMERES
           END-IF
           IF  (I-L1)
               MOVE 0                      TO SUM-X
               SET NOT-I-60                TO TRUE
               IF  RES1 < '500000'
                   SET I-60                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-90 AND I-60)
               ADD BEL                     TO SUM-X
      *
           END-IF
           IF  (I-02 AND I-90)
               ADD 1                       TO TOTREC
               ADD BEL                     TO TOTBEL
               ADD VAL                     TO TOTVAL
               ADD BELU                    TO TOTBEU
               ADD VALU                    TO TOTVAU
           END-IF
           IF  (I-02 AND NOT-I-90)
               ADD 1                       TO TOT0R
           END-IF
           IF  (I-88)
               SET NOT-I-89                TO TRUE
           END-IF
           IF  (NOT-I-88)
               SET I-88                    TO TRUE
               SET I-89                    TO TRUE
           END-IF
           IF  (I-89)
               MOVE UDATE                  TO DTO6-IO
               MOVE DTO6 (1:2)             TO DD
               MOVE DTO6 (5:2)             TO AA-IO
      ** MLLzo
               IF AA < 0
                   MULTIPLY -1 BY AA
               END-IF
               MOVE DTO6                   TO DDATO
               MOVE AA                     TO DDATO (1:2)
               MOVE DD                     TO DDATO (5:2)
               SET NOT-I-31                TO TRUE
               IF  AA > 80
                   SET I-31                TO TRUE
               END-IF
               MOVE DDATO                  TO DDATO8 (3:6)
           END-IF
           IF  (I-89 AND I-31)
               MOVE '19'                   TO DDATO8 (1:2)
           END-IF
           IF  (I-89 AND NOT-I-31)
               MOVE '20'                   TO DDATO8 (1:2)
           END-IF
           IF  (I-89)
               ACCEPT SYSTEM-TIME-X      FROM TIME
               MOVE SYSTEM-TIME            TO TIDSP (1:6)
      *  89                MOVE "DDATO8  "BUGFL1  8        DISPLAY FIELD
      *  89      BUGFL1    DEBUGBUGFILO   DDATO8           VIS INDIKATOR
           END-IF
           .
 
       LR-CALCS SECTION.
       LR-CALCS-P.
           ADD TOTREC TO ZERO          GIVING LRRSAN
           ADD TOTBEL TO ZERO          GIVING LRRSBE
           ADD TOTBEU TO ZERO          GIVING LRRSBU.
 
       LINJENR-GET SECTION.
       LINJENR-GET-P.
           IF  LINJENR-EOF-OFF
               READ LINJENR
               AT END
                   SET LINJENR-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       LINJENR-FLDOFF SECTION.
       LINJENR-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-90                TO TRUE
           END-EVALUATE.
 
       LINJENR-FLDSET SECTION.
       LINJENR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LINJENR-IO-AREA (3:3)  TO FNR1 (1:3)
               MOVE LINJENR-IO-AREA (6:6)  TO RES1 (1:6)
               MOVE LINJENR-IO-AREA (19:1) TO AFPOST (1:1)
               MOVE LINJENR-IO-AREA (20:1) TO REF0 (1:1)
               IF  REF0 = SPACES
                   SET I-90                TO TRUE
               END-IF
           END-EVALUATE.
 
       LINJENR-IDSET SECTION.
       LINJENR-IDSET-P.
           SET I-01                        TO TRUE.
 
       LINJENR-CHK-LEVEL SECTION.
       LINJENR-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO LINJENR-LEVEL-01
               MOVE LINJENR-IO-AREA (3:3)  TO LINJENR-01-L2-FNR1
               MOVE LINJENR-IO-AREA (6:6)  TO LINJENR-01-L1-RES1
               IF  LINJENR-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  LINJENR-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  LINJENR-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  LINJENR-01-L2         TO THE-PRIOR-L2
               MOVE  LINJENR-01-L1         TO THE-PRIOR-L1
               SET LINJENR-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       LINJENR-MATCH-SET SECTION.
       LINJENR-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE LINJENR-IO-AREA (3:3)  TO LINJENR-M-01-M2-FNR1
               MOVE LINJENR-IO-AREA (6:6)  TO LINJENR-M-01-M1-RES1
           END-EVALUATE.
 
       RSKINF-GET SECTION.
       RSKINF-GET-P.
           IF  RSKINF-EOF-OFF
               READ RSKINF
               AT END
                   SET RSKINF-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RSKINF-FLDSET SECTION.
       RSKINF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RSKINF-IO-AREA (1:200) TO REC (1:200)
               MOVE RSKINF-IO-AREA (3:3)   TO FNR1 (1:3)
               MOVE RSKINF-IO-AREA (6:6)   TO RES1 (1:6)
               MOVE RSKINF-IO-AREA (48:9)  TO BEL-IO
               INSPECT BEL-IO REPLACING ALL ' ' BY '0'
               MOVE RSKINF-IO-AREA (64:10) TO VAL-IO
               INSPECT VAL-IO REPLACING ALL ' ' BY '0'
               MOVE RSKINF-IO-AREA (114:7) TO BELU-IO
               MOVE RSKINF-IO-AREA (121:8) TO VALU-IO
           END-EVALUATE.
 
       RSKINF-IDSET SECTION.
       RSKINF-IDSET-P.
           SET I-02                        TO TRUE.
 
       RSKINF-CHK-LEVEL SECTION.
       RSKINF-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO RSKINF-LEVEL-02
               MOVE RSKINF-IO-AREA (3:3)   TO RSKINF-02-L2-FNR1
               MOVE RSKINF-IO-AREA (6:6)   TO RSKINF-02-L1-RES1
               IF  RSKINF-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  RSKINF-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  RSKINF-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  RSKINF-02-L2          TO THE-PRIOR-L2
               MOVE  RSKINF-02-L1          TO THE-PRIOR-L1
               SET RSKINF-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       RSKINF-MATCH-SET SECTION.
       RSKINF-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE RSKINF-IO-AREA (3:3)   TO RSKINF-M-02-M2-FNR1
               MOVE RSKINF-IO-AREA (6:6)   TO RSKINF-M-02-M1-RES1
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-03                        TO TRUE.
 
       LISTE-PRINT-LINE SECTION.
       LISTE-PRINT-LINE-P.
           IF  LISTE-BEFORE-SKIP > 0
               PERFORM LISTE-SKIP-BEFORE
           END-IF
           IF  LISTE-BEFORE-SPACE > 0
               PERFORM LISTE-SPACE-BEFORE
               IF  LISTE-AFTER-SKIP > 0
                   PERFORM LISTE-SKIP-AFTER
               END-IF
               IF  LISTE-AFTER-SPACE > 0
                   PERFORM LISTE-SPACE-AFTER
               END-IF
           ELSE
               IF  LISTE-AFTER-SKIP > 0
                   PERFORM LISTE-SKIP-AFTER
               END-IF
               PERFORM LISTE-SPACE-AFTER
           END-IF
           IF  LISTE-LINE-COUNT NOT < LISTE-MAX-LINES
               MOVE 7                      TO LISTE-AFTER-SKIP
           END-IF.
 
       LISTE-SKIP-BEFORE SECTION.
       LISTE-SKIP-BEFORE-P.
           WRITE LISTE-IO-PRINT         AFTER ADVANCING PAGE
           MOVE 1                          TO LISTE-LINE-COUNT
           MOVE 0                          TO LISTE-BEFORE-SKIP
           INITIALIZE LISTE-IO-AREA.
 
       LISTE-SPACE-BEFORE SECTION.
       LISTE-SPACE-BEFORE-P.
           WRITE LISTE-IO-PRINT         AFTER LISTE-BEFORE-SPACE LINES
           ADD LISTE-BEFORE-SPACE          TO LISTE-LINE-COUNT
           MOVE SPACES TO LISTE-IO-AREA
           INITIALIZE LISTE-IO-AREA
           MOVE 0                          TO LISTE-BEFORE-SPACE.
 
       LISTE-SKIP-AFTER SECTION.
       LISTE-SKIP-AFTER-P.
           WRITE LISTE-IO-PRINT        BEFORE ADVANCING PAGE
           MOVE 1                          TO LISTE-LINE-COUNT
           MOVE 0                          TO LISTE-AFTER-SKIP
           INITIALIZE LISTE-IO-AREA.
 
       LISTE-SPACE-AFTER SECTION.
       LISTE-SPACE-AFTER-P.
           WRITE LISTE-IO-PRINT        BEFORE LISTE-AFTER-SPACE LINES
           ADD LISTE-AFTER-SPACE           TO LISTE-LINE-COUNT
           INITIALIZE LISTE-IO-AREA
           MOVE 0                          TO LISTE-AFTER-SPACE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  LINJENR-EOF
               MOVE HIGH-VALUES            TO LINJENR-MC
                                              LINJENR-MP
           END-IF
           IF  RSKINF-EOF
               MOVE HIGH-VALUES            TO RSKINF-MC
                                              RSKINF-MP
           END-IF
           IF  LINJENR-MC < LINJENR-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  RSKINF-MC < RSKINF-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  LINJENR-MC < RSKINF-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET LINJENR-PROCESS     TO TRUE
                   MOVE LINJENR-MC         TO LINJENR-MP
                   IF  LINJENR-MC = RSKINF-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  RSKINF-MC < LINJENR-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET RSKINF-PROCESS      TO TRUE
                   MOVE RSKINF-MC          TO RSKINF-MP
                   IF  RSKINF-MC = LINJENR-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  LINJENR-MC = RSKINF-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET LINJENR-PROCESS     TO TRUE
                   MOVE LINJENR-MC         TO LINJENR-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND I-90)
               MOVE SPACES TO RESKOMT-IO-AREA
               INITIALIZE RESKOMT-IO-AREA
               MOVE REC                    TO RESKOMT-IO-AREA (1:200)
      *                        LINJE     21
               MOVE ' '                    TO RESKOMT-IO-AREA (89:1)
               WRITE RESKOMT-IO-AREA
           END-IF
           IF  (I-02 AND NOT-I-90 AND I-14)
               MOVE SPACES TO AFRECF-IO-AREA
               INITIALIZE AFRECF-IO-AREA
               MOVE REC                    TO AFRECF-IO-AREA (1:200)
               WRITE AFRECF-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-60)
               MOVE SPACES TO SUMFILE-IO-AREA
               INITIALIZE SUMFILE-IO-AREA
               MOVE '1'                    TO SUMFILE-IO-AREA (1:1)
               MOVE FNR1                   TO SUMFILE-IO-AREA (2:3)
               MOVE RES1                   TO SUMFILE-IO-AREA (5:6)
               MOVE SUM-X                  TO XO-72P
               MOVE XO-72P-EF              TO SUMFILE-IO-AREA (11:5)
               INITIALIZE SUM-X
               WRITE SUMFILE-IO-AREA
           END-IF
           IF  (I-LR)
               MOVE SPACES TO AVSTEMM-IO-AREA
               INITIALIZE AVSTEMM-IO-AREA
               MOVE DDATO8                 TO AVSTEMM-IO-AREA (1:8)
               MOVE '080'                  TO AVSTEMM-IO-AREA (9:3)
               MOVE 'RES'                  TO AVSTEMM-IO-AREA (12:3)
               MOVE 'RKO047'               TO AVSTEMM-IO-AREA (15:6)
               MOVE '*RKO047*'             TO AVSTEMM-IO-AREA (21:8)
               MOVE TIDSP-IO               TO AVSTEMM-IO-AREA (30:6)
               MOVE LRRSAN-IO              TO AVSTEMM-IO-AREA (36:13)
               MOVE LRRSBE-IO              TO AVSTEMM-IO-AREA (49:15)
               MOVE LRRSBU-IO              TO AVSTEMM-IO-AREA (64:15)
               MOVE DDATO8                 TO AVSTEMM-IO-AREA (79:8)
               WRITE AVSTEMM-IO-AREA
               MOVE SPACES TO AVSTEMM-IO-AREA
               INITIALIZE AVSTEMM-IO-AREA
               MOVE '********'             TO AVSTEMM-IO-AREA (1:8)
               MOVE '080'                  TO AVSTEMM-IO-AREA (9:3)
               MOVE 'RES'                  TO AVSTEMM-IO-AREA (12:3)
               MOVE '******'               TO AVSTEMM-IO-AREA (15:6)
               MOVE '*RKO047*'             TO AVSTEMM-IO-AREA (21:8)
               MOVE TIDSP-IO               TO AVSTEMM-IO-AREA (30:6)
               MOVE LRRSAN-IO              TO AVSTEMM-IO-AREA (36:13)
               MOVE LRRSBE-IO              TO AVSTEMM-IO-AREA (49:15)
               MOVE LRRSBU-IO              TO AVSTEMM-IO-AREA (64:15)
               MOVE DDATO8                 TO AVSTEMM-IO-AREA (79:8)
               WRITE AVSTEMM-IO-AREA
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'AVSTEMMING PROG. RKO047 ' TO LISTE-IO-AREA (1:24)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-BEFORE-SPACE
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT REC SALDO NULL '  TO LISTE-IO-AREA (2:19)
               MOVE TOT0R                  TO XO-60YNZ
               MOVE XO-60YNZ               TO LISTE-IO-AREA (25:6)
               INITIALIZE TOT0R
               MOVE 'SOM ER FJERNET'       TO LISTE-IO-AREA (34:14)
               IF  (I-14)
                   MOVE 'SAVET TIL AF.SALDOL.' TO LISTE-IO-AREA (51:20)
               END-IF
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT REC NY RESKFILE'  TO LISTE-IO-AREA (2:19)
               MOVE TOTREC                 TO XO-60YNZ
               MOVE XO-60YNZ               TO LISTE-IO-AREA (25:6)
               INITIALIZE TOTREC
               MOVE 'RSK047 KJØRT'         TO LISTE-IO-AREA (34:12)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (48:8)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'BELØP  NY  RESKFILE'  TO LISTE-IO-AREA (2:19)
               MOVE 'VALUTA & VAREKOST'    TO LISTE-IO-AREA (44:17)
               MOVE TOTBEL                 TO EDIT-TOTBEL
               MOVE EDIT-TOTBEL            TO LISTE-IO-AREA (23:15)
               INITIALIZE TOTBEL
               MOVE TOTVAL                 TO XO-102YY9R
               MOVE XO-102YY9R             TO LISTE-IO-AREA (44:17)
               INITIALIZE TOTVAL
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE TOTBEU                 TO XO-112YY9R
               MOVE XO-112YY9R             TO LISTE-IO-AREA (20:18)
               INITIALIZE TOTBEU
               MOVE TOTVAU                 TO XO-114YY9R
               MOVE XO-114YY9R             TO LISTE-IO-AREA (43:20)
               INITIALIZE TOTVAU
      * DUMMY-LINJE
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR AND I-U8 AND I-U7)
           AND (I-01 AND I-03 AND I-04)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ' '                    TO LISTE-IO-AREA (20:1)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
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
           SET LINJENR-LEVEL-INIT          TO TRUE
           INITIALIZE LINJENR-DATA-FIELDS
           SET LINJENR-EOF-OFF             TO TRUE
           SET LINJENR-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO LINJENR-MC
                                              LINJENR-MP
           OPEN INPUT LINJENR
           SET RSKINF-LEVEL-INIT           TO TRUE
           INITIALIZE RSKINF-DATA-FIELDS
           SET RSKINF-EOF-OFF              TO TRUE
           SET RSKINF-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO RSKINF-MC
                                              RSKINF-MP
           OPEN INPUT RSKINF
           OPEN INPUT FIRMAF
           OPEN OUTPUT RESKOMT
           OPEN OUTPUT AFRECF
           OPEN OUTPUT SUMFILE
           OPEN OUTPUT AVSTEMM
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE LINJENR
           CLOSE RSKINF
           CLOSE FIRMAF
           CLOSE RESKOMT
           CLOSE AFRECF
           CLOSE SUMFILE
           CLOSE AVSTEMM
           IF LISTE-IO-AREA NOT = SPACES
             WRITE LISTE-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE-IO-AREA
           END-IF
           CLOSE LISTE.
 
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
