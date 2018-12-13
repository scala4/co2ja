       IDENTIFICATION DIVISION.
       PROGRAM-ID. VAR486R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAMMET LESER VSAM "VARETIL" OG DANNER EN NY REC. FOR    *
      *  HVER OVERSTOCK LOCATION DERSOM DENNE IKKE ER SPACES.        *
      *  HENTER ALFA.ARTNR.VAREBETGN.LOC. OG BEREGNER BEHOLDNING.    *
      ****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VAR486.rpg
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
           SELECT PARAM
               ASSIGN TO UT-S-PARAM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PARAM-STATUS.
           SELECT VARETIL
               ASSIGN TO VARETIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS VARETIL-STATUS
               RECORD KEY IS VARETIL-KEY1.
           SELECT VAREMAS
               ASSIGN TO VAREMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAREMAS-STATUS
               RECORD KEY IS VAREMAS-KEY1.
           SELECT OUTFILE
               ASSIGN TO UT-S-OUTFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD PARAM
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  PARAM-IO-AREA.
           05  PARAM-IO-AREA-X             PICTURE X(80).
       FD VARETIL
               RECORD CONTAINS 200.
       01  VARETIL-IO-AREA.
           05  VARETIL-IO-AREA-X.
               10  VARETIL-KEY1.
                   15  VARETIL-KEY1N       PICTURE S9(12).
               10  FILLER                  PICTURE X(188).
       FD VAREMAS
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X.
               10  VAREMAS-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       FD OUTFILE
               BLOCK CONTAINS 164
               RECORD CONTAINS 82.
       01  OUTFILE-IO-AREA.
           05  OUTFILE-IO-AREA-X           PICTURE X(82).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  PARAM-STATUS                PICTURE 99 VALUE 0.
           10  VARETIL-STATUS              PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  OUTFILE-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-EOF-OFF           VALUE '0'.
               88  PARAM-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-READ-OFF          VALUE '0'.
               88  PARAM-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-PROCESS-OFF       VALUE '0'.
               88  PARAM-PROCESS           VALUE '1'.
           05  VARETIL-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  VARETIL-EOF-OFF         VALUE '0'.
               88  VARETIL-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VARETIL-READ-OFF        VALUE '0'.
               88  VARETIL-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VARETIL-PROCESS-OFF     VALUE '0'.
               88  VARETIL-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  VARETIL-LEVEL-INIT-OFF  VALUE '0'.
               88  VARETIL-LEVEL-INIT      VALUE '1'.
           05  VAREMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  PARAM-DATA-FIELDS.
               10  PFIRM                   PICTURE X(3).
           05  VARETIL-LEVEL-02.
               10  VARETIL-02-L2.
                   15  VARETIL-02-L2-VFIRMA PICTURE X(3).
               10  VARETIL-02-L1.
                   15  VARETIL-02-L1-VKEY  PICTURE X(10).
           05  VARETIL-DATA-FIELDS.
               10  VFIRMA                  PICTURE X(3).
               10  VKEY                    PICTURE X(10).
               10  LOC1                    PICTURE X(12).
               10  LOC2                    PICTURE X(12).
               10  LOC3                    PICTURE X(12).
               10  LOC4                    PICTURE X(12).
               10  LOC5                    PICTURE X(12).
               10  LOC6                    PICTURE X(12).
               10  LOC7                    PICTURE X(12).
               10  LOC8                    PICTURE X(12).
               10  LOC9                    PICTURE X(12).
               10  LOC10                   PICTURE X(12).
               10  LOC11                   PICTURE X(12).
           05  VAREMAS-DATA-FIELDS.
               10  VALFA                   PICTURE X(3).
               10  VARTNR                  PICTURE X(20).
               10  VBETGN                  PICTURE X(30).
               10  VANTIN-IO.
                   15  VANTIN              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  VANTUT-IO.
                   15  VANTUT              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  VLOC                    PICTURE X(6).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(10).
           05  TEMPORARY-FIELDS.
               10  BEHOLD-IO.
                   15  BEHOLD              PICTURE S9(6).
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
           SET NOT-I-02                    TO TRUE
           SET NOT-I-09                    TO TRUE
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
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
 
           IF  VARETIL-PROCESS
               SET VARETIL-PROCESS-OFF     TO TRUE
               SET VARETIL-READ            TO TRUE
           END-IF
 
           IF  VARETIL-READ
           AND RECORD-SELECTED-OFF
               PERFORM VARETIL-GET
               SET VARETIL-READ-OFF        TO TRUE
               IF  NOT VARETIL-EOF
                   PERFORM VARETIL-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET VARETIL-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  PARAM-PROCESS
               PERFORM PARAM-IDSET
           END-IF
 
           IF  VARETIL-PROCESS
               PERFORM VARETIL-IDSET
           END-IF
 
           IF  VARETIL-PROCESS
               PERFORM VARETIL-CHK-LEVEL
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
 
           IF  PARAM-PROCESS
               PERFORM PARAM-FLDOFF
               PERFORM PARAM-FLDSET
           END-IF
 
           IF  VARETIL-PROCESS
               PERFORM VARETIL-FLDOFF
               PERFORM VARETIL-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  VARETIL-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-10                    TO TRUE
           IF  (I-01)
               GO TO SLUTT-T
           END-IF
           IF  (I-09)
               SET I-LR                    TO TRUE
           END-IF
           IF  (NOT-I-02)
               GO TO SLUTT-T
           END-IF
           IF  (I-L2)
               SET NOT-I-11                TO TRUE
               IF  VFIRMA = PFIRM
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-99)
               SET I-11                    TO TRUE
           END-IF
           IF  (NOT-I-11)
               GO TO SLUTT-T
           END-IF
           IF  (I-L1)
               MOVE VKEY                   TO VAREMAS-KEY1
               READ VAREMAS RECORD KEY IS VAREMAS-KEY1
               INVALID KEY
                   SET I-12                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-12            TO TRUE
                   PERFORM VAREMAS-FLDSET
                   PERFORM VAREMAS-IDSET
               END-READ
           END-IF
           IF  (I-12)
               GO TO SLUTT-T
           END-IF
           SET I-10                        TO TRUE
           SUBTRACT VANTUT FROM VANTIN GIVING BEHOLD.
 
       SLUTT-T.
           CONTINUE.
 
       PARAM-GET SECTION.
       PARAM-GET-P.
           IF  PARAM-EOF-OFF
               READ PARAM
               AT END
                   SET PARAM-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       PARAM-FLDOFF SECTION.
       PARAM-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = '9' )
               SET NOT-I-99                TO TRUE
           END-EVALUATE.
 
       PARAM-FLDSET SECTION.
       PARAM-FLDSET-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = '9' )
               MOVE PARAM-IO-AREA (11:3)   TO PFIRM (1:3)
               IF  PFIRM = SPACES
                   SET I-99                TO TRUE
               END-IF
           END-EVALUATE.
 
       PARAM-IDCHK SECTION.
       PARAM-IDCHK-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = '9' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       PARAM-IDSET SECTION.
       PARAM-IDSET-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = '9' )
               SET I-01                    TO TRUE
           END-EVALUATE.
 
       VARETIL-GET SECTION.
       VARETIL-GET-P.
           IF  VARETIL-EOF-OFF
               READ VARETIL
               AT END
                   SET VARETIL-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VARETIL-FLDOFF SECTION.
       VARETIL-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( VARETIL-IO-AREA (1:1) = '7' )
               SET NOT-I-20                TO TRUE
               SET NOT-I-21                TO TRUE
               SET NOT-I-22                TO TRUE
               SET NOT-I-23                TO TRUE
               SET NOT-I-24                TO TRUE
               SET NOT-I-25                TO TRUE
               SET NOT-I-26                TO TRUE
               SET NOT-I-27                TO TRUE
               SET NOT-I-28                TO TRUE
               SET NOT-I-29                TO TRUE
               SET NOT-I-30                TO TRUE
           END-EVALUATE.
 
       VARETIL-FLDSET SECTION.
       VARETIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ( VARETIL-IO-AREA (1:1) = '7' )
               MOVE VARETIL-IO-AREA (3:3)  TO VFIRMA (1:3)
               MOVE VARETIL-IO-AREA (3:10) TO VKEY (1:10)
               MOVE VARETIL-IO-AREA (13:12) TO LOC1 (1:12)
               IF  LOC1 = SPACES
                   SET I-20                TO TRUE
               END-IF
               MOVE VARETIL-IO-AREA (25:12) TO LOC2 (1:12)
               IF  LOC2 = SPACES
                   SET I-21                TO TRUE
               END-IF
               MOVE VARETIL-IO-AREA (37:12) TO LOC3 (1:12)
               IF  LOC3 = SPACES
                   SET I-22                TO TRUE
               END-IF
               MOVE VARETIL-IO-AREA (49:12) TO LOC4 (1:12)
               IF  LOC4 = SPACES
                   SET I-23                TO TRUE
               END-IF
               MOVE VARETIL-IO-AREA (61:12) TO LOC5 (1:12)
               IF  LOC5 = SPACES
                   SET I-24                TO TRUE
               END-IF
               MOVE VARETIL-IO-AREA (73:12) TO LOC6 (1:12)
               IF  LOC6 = SPACES
                   SET I-25                TO TRUE
               END-IF
               MOVE VARETIL-IO-AREA (85:12) TO LOC7 (1:12)
               IF  LOC7 = SPACES
                   SET I-26                TO TRUE
               END-IF
               MOVE VARETIL-IO-AREA (97:12) TO LOC8 (1:12)
               IF  LOC8 = SPACES
                   SET I-27                TO TRUE
               END-IF
               MOVE VARETIL-IO-AREA (109:12) TO LOC9 (1:12)
               IF  LOC9 = SPACES
                   SET I-28                TO TRUE
               END-IF
               MOVE VARETIL-IO-AREA (121:12) TO LOC10 (1:12)
               IF  LOC10 = SPACES
                   SET I-29                TO TRUE
               END-IF
               MOVE VARETIL-IO-AREA (133:12) TO LOC11 (1:12)
               IF  LOC11 = SPACES
                   SET I-30                TO TRUE
               END-IF
           END-EVALUATE.
 
       VARETIL-IDCHK SECTION.
       VARETIL-IDCHK-P.
           EVALUATE TRUE
           WHEN ( VARETIL-IO-AREA (1:1) = '7' )
             OR ( VARETIL-IO-AREA (1:1) = '8' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       VARETIL-IDSET SECTION.
       VARETIL-IDSET-P.
           EVALUATE TRUE
           WHEN ( VARETIL-IO-AREA (1:1) = '7' )
               SET I-02                    TO TRUE
           WHEN ( VARETIL-IO-AREA (1:1) = '8' )
               SET I-09                    TO TRUE
           END-EVALUATE.
 
       VARETIL-CHK-LEVEL SECTION.
       VARETIL-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( VARETIL-IO-AREA (1:1) = '7' )
               MOVE LOW-VALUES             TO VARETIL-LEVEL-02
               MOVE VARETIL-IO-AREA (3:3)  TO VARETIL-02-L2-VFIRMA
               MOVE VARETIL-IO-AREA (3:10) TO VARETIL-02-L1-VKEY
               IF  VARETIL-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VARETIL-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  VARETIL-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  VARETIL-02-L2         TO THE-PRIOR-L2
               MOVE  VARETIL-02-L1         TO THE-PRIOR-L1
               SET VARETIL-LEVEL-INIT      TO TRUE
           WHEN ( VARETIL-IO-AREA (1:1) = '8' )
               CONTINUE
           END-EVALUATE.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (13:3) TO VALFA (1:3)
               MOVE VAREMAS-IO-AREA (16:20) TO VARTNR (1:20)
               MOVE VAREMAS-IO-AREA (36:30) TO VBETGN (1:30)
               MOVE VAREMAS-IO-AREA (97:5) TO VANTIN-IO
               MOVE VAREMAS-IO-AREA (102:5) TO VANTUT-IO
               MOVE VAREMAS-IO-AREA (140:6) TO VLOC (1:6)
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-03                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND I-10 AND NOT-I-20)
               MOVE SPACES TO OUTFILE-IO-AREA
               INITIALIZE OUTFILE-IO-AREA
               MOVE '10'                   TO OUTFILE-IO-AREA (1:2)
               MOVE VFIRMA                 TO OUTFILE-IO-AREA (3:3)
               MOVE VALFA                  TO OUTFILE-IO-AREA (6:3)
               MOVE VARTNR                 TO OUTFILE-IO-AREA (9:20)
               MOVE VBETGN                 TO OUTFILE-IO-AREA (29:30)
               MOVE BEHOLD-IO              TO OUTFILE-IO-AREA (59:6)
               MOVE VLOC                   TO OUTFILE-IO-AREA (65:6)
               MOVE LOC1                   TO OUTFILE-IO-AREA (71:12)
               WRITE OUTFILE-IO-AREA
           END-IF
           IF  (I-02 AND I-10 AND NOT-I-21)
               MOVE SPACES TO OUTFILE-IO-AREA
               INITIALIZE OUTFILE-IO-AREA
               MOVE '10'                   TO OUTFILE-IO-AREA (1:2)
               MOVE VFIRMA                 TO OUTFILE-IO-AREA (3:3)
               MOVE VALFA                  TO OUTFILE-IO-AREA (6:3)
               MOVE VARTNR                 TO OUTFILE-IO-AREA (9:20)
               MOVE VBETGN                 TO OUTFILE-IO-AREA (29:30)
               MOVE BEHOLD-IO              TO OUTFILE-IO-AREA (59:6)
               MOVE VLOC                   TO OUTFILE-IO-AREA (65:6)
               MOVE LOC2                   TO OUTFILE-IO-AREA (71:12)
               WRITE OUTFILE-IO-AREA
           END-IF
           IF  (I-02 AND I-10 AND NOT-I-22)
               MOVE SPACES TO OUTFILE-IO-AREA
               INITIALIZE OUTFILE-IO-AREA
               MOVE '10'                   TO OUTFILE-IO-AREA (1:2)
               MOVE VFIRMA                 TO OUTFILE-IO-AREA (3:3)
               MOVE VALFA                  TO OUTFILE-IO-AREA (6:3)
               MOVE VARTNR                 TO OUTFILE-IO-AREA (9:20)
               MOVE VBETGN                 TO OUTFILE-IO-AREA (29:30)
               MOVE BEHOLD-IO              TO OUTFILE-IO-AREA (59:6)
               MOVE VLOC                   TO OUTFILE-IO-AREA (65:6)
               MOVE LOC3                   TO OUTFILE-IO-AREA (71:12)
               WRITE OUTFILE-IO-AREA
           END-IF
           IF  (I-02 AND I-10 AND NOT-I-23)
               MOVE SPACES TO OUTFILE-IO-AREA
               INITIALIZE OUTFILE-IO-AREA
               MOVE '10'                   TO OUTFILE-IO-AREA (1:2)
               MOVE VFIRMA                 TO OUTFILE-IO-AREA (3:3)
               MOVE VALFA                  TO OUTFILE-IO-AREA (6:3)
               MOVE VARTNR                 TO OUTFILE-IO-AREA (9:20)
               MOVE VBETGN                 TO OUTFILE-IO-AREA (29:30)
               MOVE BEHOLD-IO              TO OUTFILE-IO-AREA (59:6)
               MOVE VLOC                   TO OUTFILE-IO-AREA (65:6)
               MOVE LOC4                   TO OUTFILE-IO-AREA (71:12)
               WRITE OUTFILE-IO-AREA
           END-IF
           IF  (I-02 AND I-10 AND NOT-I-24)
               MOVE SPACES TO OUTFILE-IO-AREA
               INITIALIZE OUTFILE-IO-AREA
               MOVE '10'                   TO OUTFILE-IO-AREA (1:2)
               MOVE VFIRMA                 TO OUTFILE-IO-AREA (3:3)
               MOVE VALFA                  TO OUTFILE-IO-AREA (6:3)
               MOVE VARTNR                 TO OUTFILE-IO-AREA (9:20)
               MOVE VBETGN                 TO OUTFILE-IO-AREA (29:30)
               MOVE BEHOLD-IO              TO OUTFILE-IO-AREA (59:6)
               MOVE VLOC                   TO OUTFILE-IO-AREA (65:6)
               MOVE LOC5                   TO OUTFILE-IO-AREA (71:12)
               WRITE OUTFILE-IO-AREA
           END-IF
           IF  (I-02 AND I-10 AND NOT-I-25)
               MOVE SPACES TO OUTFILE-IO-AREA
               INITIALIZE OUTFILE-IO-AREA
               MOVE '10'                   TO OUTFILE-IO-AREA (1:2)
               MOVE VFIRMA                 TO OUTFILE-IO-AREA (3:3)
               MOVE VALFA                  TO OUTFILE-IO-AREA (6:3)
               MOVE VARTNR                 TO OUTFILE-IO-AREA (9:20)
               MOVE VBETGN                 TO OUTFILE-IO-AREA (29:30)
               MOVE BEHOLD-IO              TO OUTFILE-IO-AREA (59:6)
               MOVE VLOC                   TO OUTFILE-IO-AREA (65:6)
               MOVE LOC6                   TO OUTFILE-IO-AREA (71:12)
               WRITE OUTFILE-IO-AREA
           END-IF
           IF  (I-02 AND I-10 AND NOT-I-26)
               MOVE SPACES TO OUTFILE-IO-AREA
               INITIALIZE OUTFILE-IO-AREA
               MOVE '10'                   TO OUTFILE-IO-AREA (1:2)
               MOVE VFIRMA                 TO OUTFILE-IO-AREA (3:3)
               MOVE VALFA                  TO OUTFILE-IO-AREA (6:3)
               MOVE VARTNR                 TO OUTFILE-IO-AREA (9:20)
               MOVE VBETGN                 TO OUTFILE-IO-AREA (29:30)
               MOVE BEHOLD-IO              TO OUTFILE-IO-AREA (59:6)
               MOVE VLOC                   TO OUTFILE-IO-AREA (65:6)
               MOVE LOC7                   TO OUTFILE-IO-AREA (71:12)
               WRITE OUTFILE-IO-AREA
           END-IF
           IF  (I-02 AND I-10 AND NOT-I-27)
               MOVE SPACES TO OUTFILE-IO-AREA
               INITIALIZE OUTFILE-IO-AREA
               MOVE '10'                   TO OUTFILE-IO-AREA (1:2)
               MOVE VFIRMA                 TO OUTFILE-IO-AREA (3:3)
               MOVE VALFA                  TO OUTFILE-IO-AREA (6:3)
               MOVE VARTNR                 TO OUTFILE-IO-AREA (9:20)
               MOVE VBETGN                 TO OUTFILE-IO-AREA (29:30)
               MOVE BEHOLD-IO              TO OUTFILE-IO-AREA (59:6)
               MOVE VLOC                   TO OUTFILE-IO-AREA (65:6)
               MOVE LOC8                   TO OUTFILE-IO-AREA (71:12)
               WRITE OUTFILE-IO-AREA
           END-IF
           IF  (I-02 AND I-10 AND NOT-I-28)
               MOVE SPACES TO OUTFILE-IO-AREA
               INITIALIZE OUTFILE-IO-AREA
               MOVE '10'                   TO OUTFILE-IO-AREA (1:2)
               MOVE VFIRMA                 TO OUTFILE-IO-AREA (3:3)
               MOVE VALFA                  TO OUTFILE-IO-AREA (6:3)
               MOVE VARTNR                 TO OUTFILE-IO-AREA (9:20)
               MOVE VBETGN                 TO OUTFILE-IO-AREA (29:30)
               MOVE BEHOLD-IO              TO OUTFILE-IO-AREA (59:6)
               MOVE VLOC                   TO OUTFILE-IO-AREA (65:6)
               MOVE LOC9                   TO OUTFILE-IO-AREA (71:12)
               WRITE OUTFILE-IO-AREA
           END-IF
           IF  (I-02 AND I-10 AND NOT-I-29)
               MOVE SPACES TO OUTFILE-IO-AREA
               INITIALIZE OUTFILE-IO-AREA
               MOVE '10'                   TO OUTFILE-IO-AREA (1:2)
               MOVE VFIRMA                 TO OUTFILE-IO-AREA (3:3)
               MOVE VALFA                  TO OUTFILE-IO-AREA (6:3)
               MOVE VARTNR                 TO OUTFILE-IO-AREA (9:20)
               MOVE VBETGN                 TO OUTFILE-IO-AREA (29:30)
               MOVE BEHOLD-IO              TO OUTFILE-IO-AREA (59:6)
               MOVE VLOC                   TO OUTFILE-IO-AREA (65:6)
               MOVE LOC10                  TO OUTFILE-IO-AREA (71:12)
               WRITE OUTFILE-IO-AREA
           END-IF
           IF  (I-02 AND I-10 AND NOT-I-30)
               MOVE SPACES TO OUTFILE-IO-AREA
               INITIALIZE OUTFILE-IO-AREA
               MOVE '10'                   TO OUTFILE-IO-AREA (1:2)
               MOVE VFIRMA                 TO OUTFILE-IO-AREA (3:3)
               MOVE VALFA                  TO OUTFILE-IO-AREA (6:3)
               MOVE VARTNR                 TO OUTFILE-IO-AREA (9:20)
               MOVE VBETGN                 TO OUTFILE-IO-AREA (29:30)
               MOVE BEHOLD-IO              TO OUTFILE-IO-AREA (59:6)
               MOVE VLOC                   TO OUTFILE-IO-AREA (65:6)
               MOVE LOC11                  TO OUTFILE-IO-AREA (71:12)
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
           MOVE 2                          TO LR-CHECK
           INITIALIZE PARAM-DATA-FIELDS
           SET PARAM-EOF-OFF               TO TRUE
           SET PARAM-PROCESS               TO TRUE
           OPEN INPUT PARAM
           SET VARETIL-LEVEL-INIT          TO TRUE
           INITIALIZE VARETIL-DATA-FIELDS
           SET VARETIL-EOF-OFF             TO TRUE
           SET VARETIL-PROCESS             TO TRUE
           OPEN INPUT VARETIL
           INITIALIZE VAREMAS-DATA-FIELDS
           OPEN INPUT VAREMAS
           OPEN OUTPUT OUTFILE.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PARAM
           CLOSE VARETIL
           CLOSE VAREMAS
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
