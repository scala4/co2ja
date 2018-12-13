       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORD130R.
      **********************************************  Z-WIN-RPG2   ****
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ORD130.rpg
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
           SELECT FIRSUMF
               ASSIGN TO FIRSUMF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS FIRSUMF-STATUS
               RECORD KEY IS FIRSUMF-KEY1.
           SELECT SUMFIL
               ASSIGN TO UT-S-SUMFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS SUMFIL-STATUS.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT NYFILE
               ASSIGN TO UT-S-NYFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS NYFILE-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD FIRSUMF
               RECORD CONTAINS 80.
       01  FIRSUMF-IO-AREA.
           05  FIRSUMF-IO-AREA-X.
               10  FIRSUMF-KEY1.
                   15  FIRSUMF-KEY1N       PICTURE S9(11).
               10  FILLER                  PICTURE X(69).
       FD SUMFIL
               BLOCK CONTAINS 160
               RECORD CONTAINS 80.
       01  SUMFIL-IO-AREA.
           05  SUMFIL-IO-AREA-X            PICTURE X(80).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD NYFILE
               BLOCK CONTAINS 160
               RECORD CONTAINS 80.
       01  NYFILE-IO-AREA.
           05  NYFILE-IO-AREA-X            PICTURE X(80).
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
           10  FIRSUMF-STATUS              PICTURE 99 VALUE 0.
           10  SUMFIL-STATUS               PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  NYFILE-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FIRSUMF-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  FIRSUMF-EOF-OFF         VALUE '0'.
               88  FIRSUMF-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FIRSUMF-READ-OFF        VALUE '0'.
               88  FIRSUMF-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FIRSUMF-PROCESS-OFF     VALUE '0'.
               88  FIRSUMF-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  FIRSUMF-LEVEL-INIT-OFF  VALUE '0'.
               88  FIRSUMF-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SUMFIL-EOF-OFF          VALUE '0'.
               88  SUMFIL-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SUMFIL-READ-OFF         VALUE '0'.
               88  SUMFIL-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SUMFIL-PROCESS-OFF      VALUE '0'.
               88  SUMFIL-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  SUMFIL-LEVEL-INIT-OFF   VALUE '0'.
               88  SUMFIL-LEVEL-INIT       VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
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
           05  FIRSUMF-LEVEL-01.
               10  FIRSUMF-01-L1.
                   15  FIRSUMF-01-L1-FNR   PICTURE X(3).
           05  FIRSUMF-DATA-FIELDS.
               10  KEY1                    PICTURE X(11).
               10  FNR                     PICTURE X(3).
               10  REC1                    PICTURE X(80).
           05  FIRSUMF-MP                  PICTURE X(11).
           05  FIRSUMF-MC                  PICTURE X(11).
           05  FIRSUMF-M-01            REDEFINES FIRSUMF-MC.
               10  FIRSUMF-M-01-M1.
                   15  FIRSUMF-M-01-M1-KEY1-G.
                       20  FIRSUMF-M-01-M1-KEY1 PICTURE X(11).
           05  SUMFIL-LEVEL-02.
               10  SUMFIL-02-L1.
                   15  SUMFIL-02-L1-FNR    PICTURE X(3).
           05  SUMFIL-DATA-FIELDS.
               10  KEY2                    PICTURE X(11).
               10  REC2                    PICTURE X(53).
               10  PAAR                    PICTURE X(2).
               10  PMND                    PICTURE X(2).
               10  PDAG                    PICTURE X(2).
               10  ORDSUM-IO.
                   15  ORDSUM              PICTURE S9(11) USAGE
                                                       PACKED-DECIMAL.
               10  ORDANT-IO.
                   15  ORDANT              PICTURE S9(11) USAGE
                                                       PACKED-DECIMAL.
               10  LAGVER-IO.
                   15  LAGVER              PICTURE S9(11) USAGE
                                                       PACKED-DECIMAL.
               10  SVSSUM-IO.
                   15  SVSSUM              PICTURE S9(11) USAGE
                                                       PACKED-DECIMAL.
           05  SUMFIL-MP                   PICTURE X(11).
           05  SUMFIL-MC                   PICTURE X(11).
           05  SUMFIL-M-02             REDEFINES SUMFIL-MC.
               10  SUMFIL-M-02-M1.
                   15  SUMFIL-M-02-M1-KEY2-G.
                       20  SUMFIL-M-02-M1-KEY2 PICTURE X(11).
           05  FIRMAF-DATA-FIELDS.
               10  KONFNR                  PICTURE X(3).
               10  FINAVN                  PICTURE X(30).
      *****************************************************************
      * SJEKK OM DET OGSÅ SKAL LAGES KONSERN RECORDS.                 *
      *****************************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  EDITTING-FIELDS.
               10  XO-110YY9               PICTURE ZZ.ZZZ.ZZZ.ZZ9.
               10  XO-110YY9R              PICTURE ZZ.ZZZ.ZZZ.ZZ9-.
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
               88  NOT-SET-I-OF            VALUE '0'.
               88  SET-I-OF                VALUE '1'.
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
 
           PERFORM HEADING-OUTPUT
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
           PERFORM HEADING-OUTPUT
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
           IF  NOT-SET-I-OF
               SET NOT-I-OF                TO TRUE
           END-IF
           SET NOT-SET-I-OF                TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
           SET NOT-I-06                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-04                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  FIRSUMF-PROCESS
               SET FIRSUMF-PROCESS-OFF     TO TRUE
               SET FIRSUMF-READ            TO TRUE
           END-IF
 
           IF  FIRSUMF-READ
               PERFORM FIRSUMF-GET
               SET FIRSUMF-READ-OFF        TO TRUE
               IF  NOT FIRSUMF-EOF
                   PERFORM FIRSUMF-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM FIRSUMF-MATCH-SET
               END-IF
           END-IF
 
           IF  SUMFIL-PROCESS
               SET SUMFIL-PROCESS-OFF      TO TRUE
               SET SUMFIL-READ             TO TRUE
           END-IF
 
           IF  SUMFIL-READ
               PERFORM SUMFIL-GET
               SET SUMFIL-READ-OFF         TO TRUE
               IF  NOT SUMFIL-EOF
                   PERFORM SUMFIL-MATCH-SET
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
 
           IF  FIRSUMF-PROCESS
               PERFORM FIRSUMF-IDSET
           END-IF
 
           IF  SUMFIL-PROCESS
               PERFORM SUMFIL-IDSET
           END-IF
 
           IF  FIRSUMF-PROCESS
               PERFORM FIRSUMF-CHK-LEVEL
           END-IF
 
           IF  SUMFIL-PROCESS
               PERFORM SUMFIL-CHK-LEVEL
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
           PERFORM HEADING-OVERFLOW
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  FIRSUMF-PROCESS
               PERFORM FIRSUMF-FLDSET
           END-IF
 
           IF  SUMFIL-PROCESS
               PERFORM SUMFIL-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  FIRSUMF-PROCESS
           OR  SUMFIL-PROCESS
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
               SET NOT-I-15                TO TRUE
               MOVE FNR                    TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-11                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-11            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
           END-IF
           IF  (I-L1 AND NOT-I-11)
               SET NOT-I-14                TO TRUE
               IF  KONFNR > '000'
                   SET I-14                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-11 AND I-14)
               SET NOT-I-15                TO TRUE
               IF  FNR NOT = KONFNR
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  FNR = '693'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  FNR = '918'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  FNR = '940'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  FNR = '965'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  FNR = '855'
                   SET I-15                TO TRUE
               END-IF
      *****************************************************************
           END-IF
           IF  (I-02)
               SET NOT-I-12                TO TRUE
           END-IF
           IF  (I-02 AND NOT-I-10)
               SET I-12                    TO TRUE
           END-IF
           IF  (I-02)
               SET I-10                    TO TRUE
      *  02                Z-ADD0         NULL11 110
           END-IF
           .
 
       FIRSUMF-GET SECTION.
       FIRSUMF-GET-P.
           IF  FIRSUMF-EOF-OFF
               READ FIRSUMF
               AT END
                   SET FIRSUMF-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FIRSUMF-FLDSET SECTION.
       FIRSUMF-FLDSET-P.
           EVALUATE TRUE
           WHEN ( FIRSUMF-IO-AREA (4:1) NOT = 'K' )
               MOVE FIRSUMF-IO-AREA (1:11) TO KEY1 (1:11)
               MOVE FIRSUMF-IO-AREA (1:3)  TO FNR (1:3)
               MOVE FIRSUMF-IO-AREA (1:80) TO REC1 (1:80)
           END-EVALUATE.
 
       FIRSUMF-IDCHK SECTION.
       FIRSUMF-IDCHK-P.
           EVALUATE TRUE
           WHEN ( FIRSUMF-IO-AREA (4:1) NOT = 'K' )
             OR ( FIRSUMF-IO-AREA (4:1) = 'K' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       FIRSUMF-IDSET SECTION.
       FIRSUMF-IDSET-P.
           EVALUATE TRUE
           WHEN ( FIRSUMF-IO-AREA (4:1) NOT = 'K' )
               SET I-01                    TO TRUE
           WHEN ( FIRSUMF-IO-AREA (4:1) = 'K' )
               SET I-06                    TO TRUE
           END-EVALUATE.
 
       FIRSUMF-CHK-LEVEL SECTION.
       FIRSUMF-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( FIRSUMF-IO-AREA (4:1) NOT = 'K' )
               MOVE LOW-VALUES             TO FIRSUMF-LEVEL-01
               MOVE FIRSUMF-IO-AREA (1:3)  TO FIRSUMF-01-L1-FNR
               IF  FIRSUMF-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  FIRSUMF-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  FIRSUMF-01-L1         TO THE-PRIOR-L1
               SET FIRSUMF-LEVEL-INIT      TO TRUE
           WHEN ( FIRSUMF-IO-AREA (4:1) = 'K' )
               CONTINUE
           END-EVALUATE.
 
       FIRSUMF-MATCH-SET SECTION.
       FIRSUMF-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( FIRSUMF-IO-AREA (4:1) NOT = 'K' )
               MOVE FIRSUMF-IO-AREA (1:11) TO FIRSUMF-M-01-M1-KEY1
           WHEN ( FIRSUMF-IO-AREA (4:1) = 'K' )
               SET NOT-CALL-MATCH-RECS     TO TRUE
           END-EVALUATE.
 
       SUMFIL-GET SECTION.
       SUMFIL-GET-P.
           IF  SUMFIL-EOF-OFF
               READ SUMFIL
               AT END
                   SET SUMFIL-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       SUMFIL-FLDSET SECTION.
       SUMFIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE SUMFIL-IO-AREA (1:11)  TO KEY2 (1:11)
               MOVE SUMFIL-IO-AREA (1:53)  TO REC2 (1:53)
               MOVE SUMFIL-IO-AREA (1:3)   TO FNR (1:3)
               MOVE SUMFIL-IO-AREA (6:2)   TO PAAR (1:2)
               MOVE SUMFIL-IO-AREA (8:2)   TO PMND (1:2)
               MOVE SUMFIL-IO-AREA (10:2)  TO PDAG (1:2)
               MOVE SUMFIL-IO-AREA (12:6)  TO ORDSUM-IO
               MOVE SUMFIL-IO-AREA (18:6)  TO ORDANT-IO
               MOVE SUMFIL-IO-AREA (24:6)  TO LAGVER-IO
               MOVE SUMFIL-IO-AREA (48:6)  TO SVSSUM-IO
           END-EVALUATE.
 
       SUMFIL-IDSET SECTION.
       SUMFIL-IDSET-P.
           SET I-02                        TO TRUE.
 
       SUMFIL-CHK-LEVEL SECTION.
       SUMFIL-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO SUMFIL-LEVEL-02
               MOVE SUMFIL-IO-AREA (1:3)   TO SUMFIL-02-L1-FNR
               IF  SUMFIL-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  SUMFIL-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  SUMFIL-02-L1          TO THE-PRIOR-L1
               SET SUMFIL-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       SUMFIL-MATCH-SET SECTION.
       SUMFIL-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE SUMFIL-IO-AREA (1:11)  TO SUMFIL-M-02-M1-KEY2
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (1:3)   TO KONFNR (1:3)
               MOVE FIRMAF-IO-AREA (8:30)  TO FINAVN (1:30)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-04                        TO TRUE.
 
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
               SET I-OF                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OF            TO TRUE
               END-IF
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
           IF  FIRSUMF-EOF
               MOVE HIGH-VALUES            TO FIRSUMF-MC
                                              FIRSUMF-MP
           END-IF
           IF  SUMFIL-EOF
               MOVE HIGH-VALUES            TO SUMFIL-MC
                                              SUMFIL-MP
           END-IF
           IF  FIRSUMF-MC < FIRSUMF-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  SUMFIL-MC < SUMFIL-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  FIRSUMF-MC < SUMFIL-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FIRSUMF-PROCESS     TO TRUE
                   MOVE FIRSUMF-MC         TO FIRSUMF-MP
                   IF  FIRSUMF-MC = SUMFIL-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  SUMFIL-MC < FIRSUMF-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET SUMFIL-PROCESS      TO TRUE
                   MOVE SUMFIL-MC          TO SUMFIL-MP
                   IF  SUMFIL-MC = FIRSUMF-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  FIRSUMF-MC = SUMFIL-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FIRSUMF-PROCESS     TO TRUE
                   MOVE FIRSUMF-MC         TO FIRSUMF-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND NOT-I-MR)
               MOVE SPACES TO NYFILE-IO-AREA
               INITIALIZE NYFILE-IO-AREA
               MOVE REC1                   TO NYFILE-IO-AREA (1:80)
               WRITE NYFILE-IO-AREA
           END-IF
           IF  (I-02)
               MOVE SPACES TO NYFILE-IO-AREA
               INITIALIZE NYFILE-IO-AREA
               MOVE REC2                   TO NYFILE-IO-AREA (1:53)
               WRITE NYFILE-IO-AREA
           END-IF
           IF  (I-02 AND I-15)
               MOVE SPACES TO NYFILE-IO-AREA
               INITIALIZE NYFILE-IO-AREA
               MOVE REC2                   TO NYFILE-IO-AREA (1:53)
               MOVE KONFNR                 TO NYFILE-IO-AREA (1:3)
               MOVE 'K'                    TO NYFILE-IO-AREA (4:1)
               WRITE NYFILE-IO-AREA
           END-IF
           IF  (I-02)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE KONFNR                 TO LISTE-IO-AREA (2:3)
               MOVE FNR                    TO LISTE-IO-AREA (6:3)
               IF  (I-15)
                   MOVE '*'                TO LISTE-IO-AREA (9:1)
               END-IF
               IF  (NOT-I-11)
                   MOVE FINAVN             TO LISTE-IO-AREA (10:30)
               END-IF
               IF  (I-11)
                   MOVE '** FIRMA UKJENT **' TO LISTE-IO-AREA (10:18)
               END-IF
               MOVE ORDANT                 TO XO-110YY9
               MOVE XO-110YY9              TO LISTE-IO-AREA (41:14)
               MOVE ORDSUM                 TO XO-110YY9R
               MOVE XO-110YY9R             TO LISTE-IO-AREA (56:15)
               MOVE LAGVER                 TO XO-110YY9R
               MOVE XO-110YY9R             TO LISTE-IO-AREA (72:15)
               MOVE SVSSUM                 TO XO-110YY9R
               MOVE XO-110YY9R             TO LISTE-IO-AREA (88:15)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-02 AND I-12)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'SUM IKKE FM. ORDRE PR.  ' TO LISTE-IO-AREA (7:24)
               MOVE PDAG                   TO LISTE-IO-AREA (33:2)
               MOVE '.'                    TO LISTE-IO-AREA (35:1)
               MOVE PMND                   TO LISTE-IO-AREA (36:2)
               MOVE '.'                    TO LISTE-IO-AREA (38:1)
               MOVE PAAR                   TO LISTE-IO-AREA (39:2)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KON'                  TO LISTE-IO-AREA (2:3)
               MOVE 'FNR'                  TO LISTE-IO-AREA (6:3)
               MOVE 'FIRMA NAVN'           TO LISTE-IO-AREA (10:10)
               MOVE ' ANTALL'              TO LISTE-IO-AREA (48:7)
               MOVE 'BELØP '               TO LISTE-IO-AREA (65:6)
               MOVE 'LAGERVERDI '          TO LISTE-IO-AREA (76:11)
               MOVE 'SUM SVS'              TO LISTE-IO-AREA (96:7)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'SUM IKKE FM. ORDRE PR.  ' TO LISTE-IO-AREA (7:24)
               MOVE PDAG                   TO LISTE-IO-AREA (33:2)
               MOVE '.'                    TO LISTE-IO-AREA (35:1)
               MOVE PMND                   TO LISTE-IO-AREA (36:2)
               MOVE '.'                    TO LISTE-IO-AREA (38:1)
               MOVE PAAR                   TO LISTE-IO-AREA (39:2)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KON'                  TO LISTE-IO-AREA (2:3)
               MOVE 'FNR'                  TO LISTE-IO-AREA (6:3)
               MOVE 'FIRMA NAVN'           TO LISTE-IO-AREA (10:10)
               MOVE ' ANTALL'              TO LISTE-IO-AREA (48:7)
               MOVE 'BELØP '               TO LISTE-IO-AREA (65:6)
               MOVE 'LAGERVERDI '          TO LISTE-IO-AREA (76:11)
               MOVE 'SUM SVS'              TO LISTE-IO-AREA (96:7)
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
           SET FIRSUMF-LEVEL-INIT          TO TRUE
           INITIALIZE FIRSUMF-DATA-FIELDS
           SET FIRSUMF-EOF-OFF             TO TRUE
           SET FIRSUMF-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO FIRSUMF-MC
                                              FIRSUMF-MP
           OPEN INPUT FIRSUMF
           SET SUMFIL-LEVEL-INIT           TO TRUE
           INITIALIZE SUMFIL-DATA-FIELDS
           SET SUMFIL-EOF-OFF              TO TRUE
           SET SUMFIL-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO SUMFIL-MC
                                              SUMFIL-MP
           OPEN INPUT SUMFIL
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           OPEN OUTPUT NYFILE
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FIRSUMF
           CLOSE SUMFIL
           CLOSE FIRMAF
           CLOSE NYFILE
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
