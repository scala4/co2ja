       IDENTIFICATION DIVISION.
       PROGRAM-ID. RKO045R.
      **********************************************  Z-WIN-RPG2   ****
      * NY VERSJON AV RSK.RSK045                 ***TXT***OK MT
      *  PROGRAM....: RKO045 - FØR SEPT.05-RSK045                     *
      *  PROGRAM.......: RKO045                                       *
      * FJERNER ALLE RESKONTROPOSTER SOM GÅR I NULL.                  *
      * 3/12-93  POSTER SOM SKAL VÆRE MED PÅ ALDERSFORDELT SALDOLISTE *
      *          LEGGES UT PÅ EN EGEN FILE.                           *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RKO045.rpg
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
           SELECT REFNULL
               ASSIGN TO UT-S-REFNULL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS REFNULL-STATUS.
           SELECT RSKINF
               ASSIGN TO UT-S-RSKINF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RSKINF-STATUS.
           SELECT RSKUTF
               ASSIGN TO UT-S-RSKUTF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RSKUTF-STATUS.
           SELECT AFRECF
               ASSIGN TO UT-S-AFRECF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS AFRECF-STATUS.
           SELECT LINJENR
               ASSIGN TO UT-S-LINJENR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LINJENR-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD REFNULL
               BLOCK CONTAINS 4100
               RECORD CONTAINS 20.
       01  REFNULL-IO-AREA.
           05  REFNULL-IO-AREA-X           PICTURE X(20).
       FD RSKINF
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  RSKINF-IO-AREA.
           05  RSKINF-IO-AREA-X            PICTURE X(200).
       FD RSKUTF
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  RSKUTF-IO-AREA.
           05  RSKUTF-IO-AREA-X            PICTURE X(200).
       FD AFRECF
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  AFRECF-IO-AREA.
           05  AFRECF-IO-AREA-X            PICTURE X(200).
       FD LINJENR
               BLOCK CONTAINS 4100
               RECORD CONTAINS 20.
       01  LINJENR-IO-AREA.
           05  LINJENR-IO-AREA-X           PICTURE X(20).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  REFNULL-STATUS              PICTURE 99 VALUE 0.
           10  RSKINF-STATUS               PICTURE 99 VALUE 0.
           10  RSKUTF-STATUS               PICTURE 99 VALUE 0.
           10  AFRECF-STATUS               PICTURE 99 VALUE 0.
           10  LINJENR-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  REFNULL-EOF-OFF         VALUE '0'.
               88  REFNULL-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  REFNULL-READ-OFF        VALUE '0'.
               88  REFNULL-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  REFNULL-PROCESS-OFF     VALUE '0'.
               88  REFNULL-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  REFNULL-LEVEL-INIT-OFF  VALUE '0'.
               88  REFNULL-LEVEL-INIT      VALUE '1'.
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
           05  REFNULL-LEVEL-01.
               10  REFNULL-01-L2.
                   15  REFNULL-01-L2-FNR1  PICTURE X(3).
               10  REFNULL-01-L1.
                   15  REFNULL-01-L1-RES1  PICTURE X(6).
           05  REFNULL-DATA-FIELDS.
               10  FNR1                    PICTURE X(3).
               10  RES1                    PICTURE X(6).
               10  REF1                    PICTURE X(6).
               10  AFPOST                  PICTURE X(1).
               10  REF0                    PICTURE X(1).
           05  REFNULL-MP                  PICTURE X(16).
           05  REFNULL-MC                  PICTURE X(16).
           05  REFNULL-M-01            REDEFINES REFNULL-MC.
               10  REFNULL-M-01-M4.
                   15  REFNULL-M-01-M4-FNR1-G.
                       20  REFNULL-M-01-M4-FNR1 PICTURE X(3).
               10  REFNULL-M-01-M3.
                   15  REFNULL-M-01-M3-RES1-G.
                       20  REFNULL-M-01-M3-RES1 PICTURE X(6).
               10  REFNULL-M-01-M2.
                   15  REFNULL-M-01-M2-REF0-G.
                       20  REFNULL-M-01-M2-REF0 PICTURE X(1).
               10  REFNULL-M-01-M1.
                   15  REFNULL-M-01-M1-REF1-G.
                       20  REFNULL-M-01-M1-REF1 PICTURE X(6).
           05  RSKINF-LEVEL-02.
               10  RSKINF-02-L2.
                   15  RSKINF-02-L2-FNR2   PICTURE X(3).
               10  RSKINF-02-L1.
                   15  RSKINF-02-L1-RES2   PICTURE X(6).
           05  RSKINF-DATA-FIELDS.
               10  REC2                    PICTURE X(200).
               10  FNR2                    PICTURE X(3).
               10  RES2                    PICTURE X(6).
               10  REF2                    PICTURE X(6).
           05  RSKINF-MP                   PICTURE X(16).
           05  RSKINF-MC                   PICTURE X(16).
           05  RSKINF-M-02             REDEFINES RSKINF-MC.
               10  RSKINF-M-02-M4.
                   15  RSKINF-M-02-M4-FNR2-G.
                       20  RSKINF-M-02-M4-FNR2 PICTURE X(3).
               10  RSKINF-M-02-M3.
                   15  RSKINF-M-02-M3-RES2-G.
                       20  RSKINF-M-02-M3-RES2 PICTURE X(6).
               10  RSKINF-M-02-M2.
                   15  RSKINF-M-02-M2-REF0-G.
                       20  RSKINF-M-02-M2-REF0 PICTURE X(1).
               10  RSKINF-M-02-M1.
                   15  RSKINF-M-02-M1-REF2-G.
                       20  RSKINF-M-02-M1-REF2 PICTURE X(6).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  LINJE-IO.
                   15  LINJE               PICTURE S9(5).
               10  LINJEP-IO.
                   15  LINJEP              PICTURE S9(5).
               10  LINJER-IO.
                   15  LINJER              PICTURE S9(5).
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
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  REFNULL-PROCESS
               SET REFNULL-PROCESS-OFF     TO TRUE
               SET REFNULL-READ            TO TRUE
           END-IF
 
           IF  REFNULL-READ
               PERFORM REFNULL-GET
               SET REFNULL-READ-OFF        TO TRUE
               IF  NOT REFNULL-EOF
                   PERFORM REFNULL-MATCH-SET
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
 
           IF  REFNULL-PROCESS
               PERFORM REFNULL-IDSET
           END-IF
 
           IF  RSKINF-PROCESS
               PERFORM RSKINF-IDSET
           END-IF
 
           IF  REFNULL-PROCESS
               PERFORM REFNULL-CHK-LEVEL
           END-IF
 
           IF  RSKINF-PROCESS
               PERFORM RSKINF-CHK-LEVEL
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
 
           IF  REFNULL-PROCESS
               PERFORM REFNULL-FLDSET
           END-IF
 
           IF  RSKINF-PROCESS
               PERFORM RSKINF-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  REFNULL-PROCESS
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
           IF  (I-01)
               SET NOT-I-14                TO TRUE
               IF  AFPOST = 'X'
                   SET I-14                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-MR)
               ADD 1                       TO LINJE
               MOVE LINJE                  TO LINJEP-IO
           END-IF.
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1)
               MOVE LINJE                  TO LINJER-IO
               MOVE 0                      TO LINJE
               SET NOT-I-90                TO TRUE
               IF  REF0 = '9'
                   SET I-90                TO TRUE
               END-IF
           END-IF.
 
       REFNULL-GET SECTION.
       REFNULL-GET-P.
           IF  REFNULL-EOF-OFF
               READ REFNULL
               AT END
                   SET REFNULL-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       REFNULL-FLDSET SECTION.
       REFNULL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE REFNULL-IO-AREA (3:3)  TO FNR1 (1:3)
               MOVE REFNULL-IO-AREA (6:6)  TO RES1 (1:6)
               MOVE REFNULL-IO-AREA (12:6) TO REF1 (1:6)
               MOVE REFNULL-IO-AREA (19:1) TO AFPOST (1:1)
               MOVE REFNULL-IO-AREA (20:1) TO REF0 (1:1)
           END-EVALUATE.
 
       REFNULL-IDSET SECTION.
       REFNULL-IDSET-P.
           SET I-01                        TO TRUE.
 
       REFNULL-CHK-LEVEL SECTION.
       REFNULL-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO REFNULL-LEVEL-01
               MOVE REFNULL-IO-AREA (3:3)  TO REFNULL-01-L2-FNR1
               MOVE REFNULL-IO-AREA (6:6)  TO REFNULL-01-L1-RES1
               IF  REFNULL-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  REFNULL-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  REFNULL-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  REFNULL-01-L2         TO THE-PRIOR-L2
               MOVE  REFNULL-01-L1         TO THE-PRIOR-L1
               SET REFNULL-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       REFNULL-MATCH-SET SECTION.
       REFNULL-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE REFNULL-IO-AREA (3:3)  TO REFNULL-M-01-M4-FNR1
               MOVE REFNULL-IO-AREA (6:6)  TO REFNULL-M-01-M3-RES1
               MOVE REFNULL-IO-AREA (20:1) TO REFNULL-M-01-M2-REF0
               MOVE REFNULL-IO-AREA (12:6) TO REFNULL-M-01-M1-REF1
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
               MOVE RSKINF-IO-AREA (1:200) TO REC2 (1:200)
               MOVE RSKINF-IO-AREA (3:3)   TO FNR2 (1:3)
               MOVE RSKINF-IO-AREA (6:6)   TO RES2 (1:6)
               MOVE RSKINF-IO-AREA (36:6)  TO REF2 (1:6)
               MOVE RSKINF-IO-AREA (89:1)  TO REF0 (1:1)
           END-EVALUATE.
 
       RSKINF-IDSET SECTION.
       RSKINF-IDSET-P.
           SET I-02                        TO TRUE.
 
       RSKINF-CHK-LEVEL SECTION.
       RSKINF-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO RSKINF-LEVEL-02
               MOVE RSKINF-IO-AREA (3:3)   TO RSKINF-02-L2-FNR2
               MOVE RSKINF-IO-AREA (6:6)   TO RSKINF-02-L1-RES2
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
               MOVE RSKINF-IO-AREA (3:3)   TO RSKINF-M-02-M4-FNR2
               MOVE RSKINF-IO-AREA (6:6)   TO RSKINF-M-02-M3-RES2
               MOVE RSKINF-IO-AREA (89:1)  TO RSKINF-M-02-M2-REF0
               MOVE RSKINF-IO-AREA (36:6)  TO RSKINF-M-02-M1-REF2
           END-EVALUATE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  REFNULL-EOF
               MOVE HIGH-VALUES            TO REFNULL-MC
                                              REFNULL-MP
           END-IF
           IF  RSKINF-EOF
               MOVE HIGH-VALUES            TO RSKINF-MC
                                              RSKINF-MP
           END-IF
           IF  REFNULL-MC < REFNULL-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  RSKINF-MC < RSKINF-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  REFNULL-MC < RSKINF-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET REFNULL-PROCESS     TO TRUE
                   MOVE REFNULL-MC         TO REFNULL-MP
                   IF  REFNULL-MC = RSKINF-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  RSKINF-MC < REFNULL-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET RSKINF-PROCESS      TO TRUE
                   MOVE RSKINF-MC          TO RSKINF-MP
                   IF  RSKINF-MC = REFNULL-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  REFNULL-MC = RSKINF-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET REFNULL-PROCESS     TO TRUE
                   MOVE REFNULL-MC         TO REFNULL-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND NOT-I-MR)
               MOVE SPACES TO RSKUTF-IO-AREA
               INITIALIZE RSKUTF-IO-AREA
               MOVE REC2                   TO RSKUTF-IO-AREA (1:200)
               MOVE LINJEP-IO              TO RSKUTF-IO-AREA (12:5)
               WRITE RSKUTF-IO-AREA
           END-IF
           IF  (I-02 AND I-MR AND I-14)
               MOVE SPACES TO AFRECF-IO-AREA
               INITIALIZE AFRECF-IO-AREA
               MOVE REC2                   TO AFRECF-IO-AREA (1:200)
               WRITE AFRECF-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1)
               MOVE SPACES TO LINJENR-IO-AREA
               INITIALIZE LINJENR-IO-AREA
               MOVE '63'                   TO LINJENR-IO-AREA (1:2)
               MOVE FNR2                   TO LINJENR-IO-AREA (3:3)
               MOVE RES2                   TO LINJENR-IO-AREA (6:6)
               MOVE LINJER-IO              TO LINJENR-IO-AREA (12:5)
               MOVE AFPOST                 TO LINJENR-IO-AREA (19:1)
               IF  (I-90)
                   MOVE REF0               TO LINJENR-IO-AREA (20:1)
               END-IF
               WRITE LINJENR-IO-AREA
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
           SET REFNULL-LEVEL-INIT          TO TRUE
           INITIALIZE REFNULL-DATA-FIELDS
           SET REFNULL-EOF-OFF             TO TRUE
           SET REFNULL-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO REFNULL-MC
                                              REFNULL-MP
           OPEN INPUT REFNULL
           SET RSKINF-LEVEL-INIT           TO TRUE
           INITIALIZE RSKINF-DATA-FIELDS
           SET RSKINF-EOF-OFF              TO TRUE
           SET RSKINF-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO RSKINF-MC
                                              RSKINF-MP
           OPEN INPUT RSKINF
           OPEN OUTPUT RSKUTF
           OPEN OUTPUT AFRECF
           OPEN OUTPUT LINJENR.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE REFNULL
           CLOSE RSKINF
           CLOSE RSKUTF
           CLOSE AFRECF
           CLOSE LINJENR.
 
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
