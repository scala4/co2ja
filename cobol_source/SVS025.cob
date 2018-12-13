       IDENTIFICATION DIVISION.
       PROGRAM-ID. SVS025R.
      **********************************************  Z-WIN-RPG2   ****
      *  DANNE SVS-FILE TIL SALG OG INNKJØPSSTAT.
      *  OG PRINTE  SKAFFEVARELISTE .
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: SVS025.rpg
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
           SELECT REGPAR
               ASSIGN TO UT-S-REGPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS REGPAR-STATUS.
           SELECT SVSIN
               ASSIGN TO UT-S-SVSIN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS SVSIN-STATUS.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT SVSOUT
               ASSIGN TO UT-S-SVSOUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS SVSOUT-STATUS.
           SELECT PRTF
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PRTF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD REGPAR
               BLOCK CONTAINS 100
               RECORD CONTAINS 100.
       01  REGPAR-IO-AREA.
           05  REGPAR-IO-AREA-X            PICTURE X(100).
       FD SVSIN
               BLOCK CONTAINS 4680
               RECORD CONTAINS 65.
       01  SVSIN-IO-AREA.
           05  SVSIN-IO-AREA-X             PICTURE X(65).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD SVSOUT
               BLOCK CONTAINS 4100
               RECORD CONTAINS 20.
       01  SVSOUT-IO-AREA.
           05  SVSOUT-IO-AREA-X            PICTURE X(20).
       FD PRTF
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  PRTF-IO-PRINT.
           05  PRTF-IO-AREA-CONTROL        PICTURE X VALUE ' '.
        02 PRTF-IO-AREA.
           05  PRTF-IO-AREA-X              PICTURE X(132).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  REGPAR-STATUS               PICTURE 99 VALUE 0.
           10  SVSIN-STATUS                PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  SVSOUT-STATUS               PICTURE 99 VALUE 0.
           10  PRTF-STATUS                 PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  REGPAR-EOF-OFF          VALUE '0'.
               88  REGPAR-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  REGPAR-READ-OFF         VALUE '0'.
               88  REGPAR-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  REGPAR-PROCESS-OFF      VALUE '0'.
               88  REGPAR-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SVSIN-EOF-OFF           VALUE '0'.
               88  SVSIN-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SVSIN-READ-OFF          VALUE '0'.
               88  SVSIN-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SVSIN-PROCESS-OFF       VALUE '0'.
               88  SVSIN-PROCESS           VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  SVSIN-LEVEL-INIT-OFF    VALUE '0'.
               88  SVSIN-LEVEL-INIT        VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  PRTF-DATA-FIELDS.
               10  PRTF-AFTER-SPACE        PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRTF-AFTER-SKIP         PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRTF-BEFORE-SPACE       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRTF-BEFORE-SKIP        PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRTF-MAX-LINES          PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRTF-LINE-COUNT         PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRTF-CLR-IO             PICTURE X VALUE 'Y'.
           05  REGPAR-DATA-FIELDS.
               10  PER                     PICTURE X(13).
           05  SVSIN-LEVEL-02.
               10  SVSIN-02-L2.
                   15  SVSIN-02-L2-FIRMA   PICTURE X(3).
               10  SVSIN-02-L1.
                   15  SVSIN-02-L1-AVD     PICTURE X(1).
           05  SVSIN-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  AVD                     PICTURE X(1).
               10  VGR                     PICTURE X(5).
               10  SVSBEL-IO.
                   15  SVSBEL              PICTURE S9(8)V9(2).
               10  SKABEL-IO.
                   15  SKABEL              PICTURE S9(8)V9(2).
               10  PRO-IO.
                   15  PRO                 PICTURE S9(3)V9(1).
               10  NAVN                    PICTURE X(30).
           05  FIRMAF-DATA-FIELDS.
               10  FNR                     PICTURE X(3).
               10  FINAVN                  PICTURE X(30).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(1).
           05  TEMPORARY-FIELDS.
               10  SUM1-IO.
                   15  SUM1                PICTURE S9(1)V9(3).
               10  SUM2-IO.
                   15  SUM2                PICTURE S9(1)V9(3).
               10  NETTO-IO.
                   15  NETTO               PICTURE S9(8)V9(2).
               10  NYBEL-IO.
                   15  NYBEL               PICTURE S9(8)V9(2).
               10  SASVS-IO.
                   15  SASVS               PICTURE S9(9)V9(2).
               10  SASKA-IO.
                   15  SASKA               PICTURE S9(9)V9(2).
               10  SASKAS-IO.
                   15  SASKAS              PICTURE S9(9)V9(2).
               10  GTSVS-IO.
                   15  GTSVS               PICTURE S9(9)V9(2).
               10  GTSKA-IO.
                   15  GTSKA               PICTURE S9(9)V9(2).
               10  GTSKAS-IO.
                   15  GTSKAS              PICTURE S9(9)V9(2).
               10  FORTJA-IO.
                   15  FORTJA              PICTURE S9(9)V9(2).
               10  SUM3-IO.
                   15  SUM3                PICTURE S9(11)V9(2).
               10  SAPRO-IO.
                   15  SAPRO               PICTURE S9(4)V9(1).
               10  FORTJG-IO.
                   15  FORTJG              PICTURE S9(9)V9(2).
               10  SUM4-IO.
                   15  SUM4                PICTURE S9(11)V9(2).
               10  GTPRO-IO.
                   15  GTPRO               PICTURE S9(4)V9(1).
               10  OPPGNR                  PICTURE X(10).
               10  FIRMNR                  PICTURE X(3).
               10  FIRMNA                  PICTURE X(30).
           05  EDITTING-FIELDS.
               10  XO-82YYZR               PICTURE ZZ.ZZZ.ZZZ,ZZ-.
               10  XO-31YYZR               PICTURE ZZZ,Z-.
               10  XO-92YY9R               PICTURE ZZZ.ZZZ.ZZZ,99-.
               10  XO-41YY9R               PICTURE Z.ZZZ,9-.
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
           IF  NOT-SET-I-OF
               SET NOT-I-OF                TO TRUE
           END-IF
           SET NOT-SET-I-OF                TO TRUE
 
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
           IF  REGPAR-PROCESS
               SET REGPAR-PROCESS-OFF      TO TRUE
               SET REGPAR-READ             TO TRUE
           END-IF
 
           IF  REGPAR-READ
           AND RECORD-SELECTED-OFF
               PERFORM REGPAR-GET
               SET REGPAR-READ-OFF         TO TRUE
               IF  NOT REGPAR-EOF
                   PERFORM REGPAR-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET REGPAR-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  SVSIN-PROCESS
               SET SVSIN-PROCESS-OFF       TO TRUE
               SET SVSIN-READ              TO TRUE
           END-IF
 
           IF  SVSIN-READ
           AND RECORD-SELECTED-OFF
               PERFORM SVSIN-GET
               SET SVSIN-READ-OFF          TO TRUE
               IF  NOT SVSIN-EOF
                   PERFORM SVSIN-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET SVSIN-PROCESS       TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  REGPAR-PROCESS
               PERFORM REGPAR-IDSET
           END-IF
 
           IF  SVSIN-PROCESS
               PERFORM SVSIN-IDSET
           END-IF
 
           IF  SVSIN-PROCESS
               PERFORM SVSIN-CHK-LEVEL
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
           PERFORM DETAIL-OVERFLOW
 
           IF  REGPAR-PROCESS
               PERFORM REGPAR-FLDSET
           END-IF
 
           IF  SVSIN-PROCESS
               PERFORM SVSIN-FLDOFF
               PERFORM SVSIN-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  SVSIN-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (NOT-I-02)
               GO TO UT-T
           END-IF
           IF  (I-L2)
               MOVE FIRMA                  TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-11                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-11            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
               PERFORM SIDE1-S
           END-IF
           SET NOT-I-10                    TO TRUE
      *
      *  **  OM DET ER INNMELDT BRF.PROSENT SKAL DENNE BEREGNES.
      *  10/12/92
           IF  (I-05 AND I-06 AND I-07)
               SET I-10                    TO TRUE
           END-IF
           IF  (I-10)
               GO TO UT-T
           END-IF
           IF  (NOT-I-07)
               DIVIDE PRO BY 100       GIVING SUM1 ROUNDED
               SUBTRACT SUM1 FROM 1    GIVING SUM2
               MULTIPLY SUM2 BY SKABEL GIVING NETTO ROUNDED
           END-IF
           IF  (I-07)
               ADD SKABEL TO ZERO      GIVING NETTO
           END-IF
           ADD NETTO TO SVSBEL         GIVING NYBEL
           ADD SVSBEL                      TO SASVS
           ADD SKABEL                      TO SASKA
           ADD NETTO                       TO SASKAS
           ADD SVSBEL                      TO GTSVS
           ADD SKABEL                      TO GTSKA
           ADD NETTO                       TO GTSKAS.
 
       UT-T.
           SET NOT-I-15                    TO TRUE
           IF  SASKA = 0,00
               SET I-15                    TO TRUE
           END-IF.
 
       SIDE1-S SECTION.
       SIDE1-S-P.
           SET I-86                        TO TRUE
           MOVE 'SVS025'                   TO OPPGNR (5:6)
           MOVE FNR                        TO FIRMNR
           MOVE FINAVN                     TO FIRMNA
           PERFORM EXCEPTION-OUTPUT
           SET NOT-I-86                    TO TRUE.
      ****************************************************************
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1)
               SUBTRACT SASKAS FROM SASKA GIVING FORTJA
               MULTIPLY 100 BY FORTJA  GIVING SUM3
           END-IF
           IF  (I-L1 AND NOT-I-15)
               DIVIDE SUM3 BY SASKA    GIVING SAPRO ROUNDED
           END-IF
           IF  (I-L2)
               SUBTRACT GTSKAS FROM GTSKA GIVING FORTJG
               MULTIPLY 100 BY FORTJG  GIVING SUM4
               SET NOT-I-16                TO TRUE
               IF  SUM4 = 0
                   SET I-16                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-16)
               DIVIDE SUM4 BY GTSKA    GIVING GTPRO ROUNDED
      ****************************************************************
      *  SUBRUTINE FOR PRINTING AV SIDE 1 PR. FIRMA                  *
      ****************************************************************
           END-IF
           .
 
       REGPAR-GET SECTION.
       REGPAR-GET-P.
           IF  REGPAR-EOF-OFF
               READ REGPAR
               AT END
                   SET REGPAR-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       REGPAR-FLDSET SECTION.
       REGPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ( REGPAR-IO-AREA (1:1) = '9'
            AND   REGPAR-IO-AREA (2:1) = '0' )
               MOVE REGPAR-IO-AREA (11:13) TO PER (1:13)
           END-EVALUATE.
 
       REGPAR-IDCHK SECTION.
       REGPAR-IDCHK-P.
           EVALUATE TRUE
           WHEN ( REGPAR-IO-AREA (1:1) = '9'
            AND   REGPAR-IO-AREA (2:1) = '0' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       REGPAR-IDSET SECTION.
       REGPAR-IDSET-P.
           EVALUATE TRUE
           WHEN ( REGPAR-IO-AREA (1:1) = '9'
            AND   REGPAR-IO-AREA (2:1) = '0' )
               SET I-01                    TO TRUE
           END-EVALUATE.
 
       SVSIN-GET SECTION.
       SVSIN-GET-P.
           IF  SVSIN-EOF-OFF
               READ SVSIN
               AT END
                   SET SVSIN-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       SVSIN-FLDOFF SECTION.
       SVSIN-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( SVSIN-IO-AREA (1:1) = '4' )
               SET NOT-I-05                TO TRUE
               SET NOT-I-06                TO TRUE
               SET NOT-I-07                TO TRUE
           END-EVALUATE.
 
       SVSIN-FLDSET SECTION.
       SVSIN-FLDSET-P.
           EVALUATE TRUE
           WHEN ( SVSIN-IO-AREA (1:1) = '4' )
               MOVE SVSIN-IO-AREA (2:3)    TO FIRMA (1:3)
               MOVE SVSIN-IO-AREA (5:1)    TO AVD (1:1)
               MOVE SVSIN-IO-AREA (5:5)    TO VGR (1:5)
               MOVE SVSIN-IO-AREA (10:10)  TO SVSBEL-IO
               INSPECT SVSBEL-IO REPLACING ALL ' ' BY '0'
               IF  SVSBEL = ZERO
                   SET I-05                TO TRUE
               END-IF
               MOVE SVSIN-IO-AREA (20:10)  TO SKABEL-IO
               INSPECT SKABEL-IO REPLACING ALL ' ' BY '0'
               IF  SKABEL = ZERO
                   SET I-06                TO TRUE
               END-IF
               MOVE SVSIN-IO-AREA (30:4)   TO PRO-IO
               INSPECT PRO-IO REPLACING ALL ' ' BY '0'
               IF  PRO = ZERO
                   SET I-07                TO TRUE
               END-IF
               MOVE SVSIN-IO-AREA (34:30)  TO NAVN (1:30)
           END-EVALUATE.
 
       SVSIN-IDCHK SECTION.
       SVSIN-IDCHK-P.
           EVALUATE TRUE
           WHEN ( SVSIN-IO-AREA (1:1) = '4' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       SVSIN-IDSET SECTION.
       SVSIN-IDSET-P.
           EVALUATE TRUE
           WHEN ( SVSIN-IO-AREA (1:1) = '4' )
               SET I-02                    TO TRUE
           END-EVALUATE.
 
       SVSIN-CHK-LEVEL SECTION.
       SVSIN-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( SVSIN-IO-AREA (1:1) = '4' )
               MOVE LOW-VALUES             TO SVSIN-LEVEL-02
               MOVE SVSIN-IO-AREA (2:3)    TO SVSIN-02-L2-FIRMA
               MOVE SVSIN-IO-AREA (5:1)    TO SVSIN-02-L1-AVD
               IF  SVSIN-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  SVSIN-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  SVSIN-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  SVSIN-02-L2           TO THE-PRIOR-L2
               MOVE  SVSIN-02-L1           TO THE-PRIOR-L1
               SET SVSIN-LEVEL-INIT        TO TRUE
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (5:3)   TO FNR (1:3)
               MOVE FIRMAF-IO-AREA (8:30)  TO FINAVN (1:30)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-03                        TO TRUE.
 
       PRTF-PRINT-LINE SECTION.
       PRTF-PRINT-LINE-P.
           IF  PRTF-BEFORE-SKIP > 0
               PERFORM PRTF-SKIP-BEFORE
           END-IF
           IF  PRTF-BEFORE-SPACE > 0
               PERFORM PRTF-SPACE-BEFORE
               IF  PRTF-AFTER-SKIP > 0
                   PERFORM PRTF-SKIP-AFTER
               END-IF
               IF  PRTF-AFTER-SPACE > 0
                   PERFORM PRTF-SPACE-AFTER
               END-IF
           ELSE
               IF  PRTF-AFTER-SKIP > 0
                   PERFORM PRTF-SKIP-AFTER
               END-IF
               PERFORM PRTF-SPACE-AFTER
           END-IF
           IF  PRTF-LINE-COUNT NOT < PRTF-MAX-LINES
               SET I-OF                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OF            TO TRUE
               END-IF
           END-IF.
 
       PRTF-SKIP-BEFORE SECTION.
       PRTF-SKIP-BEFORE-P.
           WRITE PRTF-IO-PRINT          AFTER ADVANCING PAGE
           MOVE 1                          TO PRTF-LINE-COUNT
           MOVE 0                          TO PRTF-BEFORE-SKIP
           INITIALIZE PRTF-IO-AREA.
 
       PRTF-SPACE-BEFORE SECTION.
       PRTF-SPACE-BEFORE-P.
           WRITE PRTF-IO-PRINT          AFTER PRTF-BEFORE-SPACE LINES
           ADD PRTF-BEFORE-SPACE           TO PRTF-LINE-COUNT
           MOVE SPACES TO PRTF-IO-AREA
           INITIALIZE PRTF-IO-AREA
           MOVE 0                          TO PRTF-BEFORE-SPACE.
 
       PRTF-SKIP-AFTER SECTION.
       PRTF-SKIP-AFTER-P.
           WRITE PRTF-IO-PRINT         BEFORE ADVANCING PAGE
           MOVE 1                          TO PRTF-LINE-COUNT
           MOVE 0                          TO PRTF-AFTER-SKIP
           INITIALIZE PRTF-IO-AREA.
 
       PRTF-SPACE-AFTER SECTION.
       PRTF-SPACE-AFTER-P.
           WRITE PRTF-IO-PRINT         BEFORE PRTF-AFTER-SPACE LINES
           ADD PRTF-AFTER-SPACE            TO PRTF-LINE-COUNT
           INITIALIZE PRTF-IO-AREA
           MOVE 0                          TO PRTF-AFTER-SPACE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND NOT-I-10)
               MOVE SPACES TO SVSOUT-IO-AREA
               INITIALIZE SVSOUT-IO-AREA
               MOVE '6'                    TO SVSOUT-IO-AREA (1:1)
               MOVE FIRMA                  TO SVSOUT-IO-AREA (2:3)
               MOVE VGR                    TO SVSOUT-IO-AREA (5:5)
               MOVE NYBEL-IO               TO SVSOUT-IO-AREA (10:10)
               WRITE SVSOUT-IO-AREA
           END-IF
           IF  (I-L1)
               MOVE SPACES TO PRTF-IO-AREA
               INITIALIZE PRTF-IO-AREA
               MOVE FINAVN                 TO PRTF-IO-AREA (2:30)
               MOVE 'S K A F F E V A R E'  TO PRTF-IO-AREA (36:19)
               MOVE 'L I S T E   O G'      TO PRTF-IO-AREA (56:15)
               MOVE 'S V S'                TO PRTF-IO-AREA (72:5)
               MOVE PER                    TO PRTF-IO-AREA (102:13)
               MOVE 01                     TO PRTF-BEFORE-SKIP
               MOVE 2                      TO PRTF-AFTER-SPACE
               PERFORM PRTF-PRINT-LINE
               MOVE SPACES TO PRTF-IO-AREA
               INITIALIZE PRTF-IO-AREA
               MOVE 'SOLGTE VARERS'        TO PRTF-IO-AREA (61:13)
               MOVE 'SKAFFEVARERS'         TO PRTF-IO-AREA (79:12)
               MOVE 'FORTJ-'               TO PRTF-IO-AREA (95:6)
               MOVE 'SKAFFEVARERS'         TO PRTF-IO-AREA (106:12)
               MOVE 1                      TO PRTF-AFTER-SPACE
               PERFORM PRTF-PRINT-LINE
               MOVE SPACES TO PRTF-IO-AREA
               INITIALIZE PRTF-IO-AREA
               MOVE 'VRG.'                 TO PRTF-IO-AREA (1:4)
               MOVE 'VAREBETEGNELSE'       TO PRTF-IO-AREA (10:14)
               MOVE 'FAKTURABELØP'         TO PRTF-IO-AREA (45:12)
               MOVE 'SELVKOST'             TO PRTF-IO-AREA (66:8)
               MOVE 'UTSALGSPRIS   PROSENT' TO PRTF-IO-AREA (80:21)
               MOVE 'SELVKOST'             TO PRTF-IO-AREA (110:8)
               MOVE 1                      TO PRTF-AFTER-SPACE
               PERFORM PRTF-PRINT-LINE
               MOVE SPACES TO PRTF-IO-AREA
               INITIALIZE PRTF-IO-AREA
               MOVE '------------------------' TO PRTF-IO-AREA (1:24)
               MOVE '------------------------' TO PRTF-IO-AREA (25:24)
               MOVE '------------------------' TO PRTF-IO-AREA (49:24)
               MOVE '------------------------' TO PRTF-IO-AREA (73:24)
               MOVE '-----------------------' TO PRTF-IO-AREA (97:23)
               MOVE 2                      TO PRTF-AFTER-SPACE
               PERFORM PRTF-PRINT-LINE
           END-IF
           IF  (I-02 AND NOT-I-10)
               MOVE SPACES TO PRTF-IO-AREA
               INITIALIZE PRTF-IO-AREA
               MOVE VGR                    TO PRTF-IO-AREA (2:5)
               MOVE NAVN                   TO PRTF-IO-AREA (10:30)
               MOVE SVSBEL                 TO XO-82YYZR
               MOVE XO-82YYZR              TO PRTF-IO-AREA (61:14)
               MOVE SKABEL                 TO XO-82YYZR
               MOVE XO-82YYZR              TO PRTF-IO-AREA (78:14)
               MOVE PRO                    TO XO-31YYZR
               MOVE XO-31YYZR              TO PRTF-IO-AREA (96:6)
               MOVE NETTO                  TO XO-82YYZR
               MOVE XO-82YYZR              TO PRTF-IO-AREA (105:14)
               MOVE 2                      TO PRTF-AFTER-SPACE
               PERFORM PRTF-PRINT-LINE
           END-IF.
 
       DETAIL-OVERFLOW SECTION.
       DETAIL-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO PRTF-IO-AREA
               INITIALIZE PRTF-IO-AREA
               MOVE FINAVN                 TO PRTF-IO-AREA (2:30)
               MOVE 'S K A F F E V A R E'  TO PRTF-IO-AREA (36:19)
               MOVE 'L I S T E   O G'      TO PRTF-IO-AREA (56:15)
               MOVE 'S V S'                TO PRTF-IO-AREA (72:5)
               MOVE PER                    TO PRTF-IO-AREA (102:13)
               MOVE 01                     TO PRTF-BEFORE-SKIP
               MOVE 2                      TO PRTF-AFTER-SPACE
               PERFORM PRTF-PRINT-LINE
               MOVE SPACES TO PRTF-IO-AREA
               INITIALIZE PRTF-IO-AREA
               MOVE 'SOLGTE VARERS'        TO PRTF-IO-AREA (61:13)
               MOVE 'SKAFFEVARERS'         TO PRTF-IO-AREA (79:12)
               MOVE 'FORTJ-'               TO PRTF-IO-AREA (95:6)
               MOVE 'SKAFFEVARERS'         TO PRTF-IO-AREA (106:12)
               MOVE 1                      TO PRTF-AFTER-SPACE
               PERFORM PRTF-PRINT-LINE
               MOVE SPACES TO PRTF-IO-AREA
               INITIALIZE PRTF-IO-AREA
               MOVE 'VRG.'                 TO PRTF-IO-AREA (1:4)
               MOVE 'VAREBETEGNELSE'       TO PRTF-IO-AREA (10:14)
               MOVE 'FAKTURABELØP'         TO PRTF-IO-AREA (45:12)
               MOVE 'SELVKOST'             TO PRTF-IO-AREA (66:8)
               MOVE 'UTSALGSPRIS   PROSENT' TO PRTF-IO-AREA (80:21)
               MOVE 'SELVKOST'             TO PRTF-IO-AREA (110:8)
               MOVE 1                      TO PRTF-AFTER-SPACE
               PERFORM PRTF-PRINT-LINE
               MOVE SPACES TO PRTF-IO-AREA
               INITIALIZE PRTF-IO-AREA
               MOVE '------------------------' TO PRTF-IO-AREA (1:24)
               MOVE '------------------------' TO PRTF-IO-AREA (25:24)
               MOVE '------------------------' TO PRTF-IO-AREA (49:24)
               MOVE '------------------------' TO PRTF-IO-AREA (73:24)
               MOVE '-----------------------' TO PRTF-IO-AREA (97:23)
               MOVE 2                      TO PRTF-AFTER-SPACE
               PERFORM PRTF-PRINT-LINE
           END-IF.
 
       EXCEPTION-OUTPUT SECTION.
       EXCEPTION-OUTPUT-P.
           IF  (I-86)
               MOVE SPACES TO PRTF-IO-AREA
               INITIALIZE PRTF-IO-AREA
               MOVE '************************' TO PRTF-IO-AREA (17:24)
               MOVE '******************'   TO PRTF-IO-AREA (41:18)
               MOVE 01                     TO PRTF-BEFORE-SKIP
               MOVE 3                      TO PRTF-BEFORE-SPACE
               MOVE 1                      TO PRTF-AFTER-SPACE
               PERFORM PRTF-PRINT-LINE
               MOVE SPACES TO PRTF-IO-AREA
               INITIALIZE PRTF-IO-AREA
               MOVE '*'                    TO PRTF-IO-AREA (17:1)
               MOVE '*'                    TO PRTF-IO-AREA (58:1)
               MOVE 1                      TO PRTF-AFTER-SPACE
               PERFORM PRTF-PRINT-LINE
               MOVE SPACES TO PRTF-IO-AREA
               INITIALIZE PRTF-IO-AREA
               MOVE '*'                    TO PRTF-IO-AREA (17:1)
               MOVE 'OPPGAVENR. 231     PROG.' TO PRTF-IO-AREA (23:24)
               MOVE OPPGNR                 TO PRTF-IO-AREA (43:10)
               MOVE '*'                    TO PRTF-IO-AREA (58:1)
               MOVE 1                      TO PRTF-AFTER-SPACE
               PERFORM PRTF-PRINT-LINE
               MOVE SPACES TO PRTF-IO-AREA
               INITIALIZE PRTF-IO-AREA
               MOVE '*'                    TO PRTF-IO-AREA (17:1)
               MOVE 'SKAFFEVARELISTE  SVS    ' TO PRTF-IO-AREA (23:24)
               MOVE '*'                    TO PRTF-IO-AREA (58:1)
               MOVE 1                      TO PRTF-AFTER-SPACE
               PERFORM PRTF-PRINT-LINE
               MOVE SPACES TO PRTF-IO-AREA
               INITIALIZE PRTF-IO-AREA
               MOVE '*'                    TO PRTF-IO-AREA (17:1)
               MOVE '                        ' TO PRTF-IO-AREA (23:24)
               MOVE '*'                    TO PRTF-IO-AREA (58:1)
               MOVE 1                      TO PRTF-AFTER-SPACE
               PERFORM PRTF-PRINT-LINE
               MOVE SPACES TO PRTF-IO-AREA
               INITIALIZE PRTF-IO-AREA
               MOVE '*'                    TO PRTF-IO-AREA (17:1)
               MOVE '*'                    TO PRTF-IO-AREA (58:1)
               MOVE 1                      TO PRTF-AFTER-SPACE
               PERFORM PRTF-PRINT-LINE
               MOVE SPACES TO PRTF-IO-AREA
               INITIALIZE PRTF-IO-AREA
               MOVE '*     DENNE OPPGAVE'  TO PRTF-IO-AREA (17:19)
               MOVE '*'                    TO PRTF-IO-AREA (58:1)
               MOVE 1                      TO PRTF-AFTER-SPACE
               PERFORM PRTF-PRINT-LINE
               MOVE SPACES TO PRTF-IO-AREA
               INITIALIZE PRTF-IO-AREA
               MOVE '*     FREMSTILT'      TO PRTF-IO-AREA (17:15)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO PRTF-IO-AREA (33:8)
               MOVE '*'                    TO PRTF-IO-AREA (58:1)
               MOVE 1                      TO PRTF-AFTER-SPACE
               PERFORM PRTF-PRINT-LINE
               MOVE SPACES TO PRTF-IO-AREA
               INITIALIZE PRTF-IO-AREA
               MOVE '*     TILHØRER FIRMA' TO PRTF-IO-AREA (17:20)
               MOVE FIRMNR                 TO PRTF-IO-AREA (38:3)
               MOVE '*'                    TO PRTF-IO-AREA (58:1)
               MOVE 1                      TO PRTF-AFTER-SPACE
               PERFORM PRTF-PRINT-LINE
               MOVE SPACES TO PRTF-IO-AREA
               INITIALIZE PRTF-IO-AREA
               MOVE '*'                    TO PRTF-IO-AREA (17:1)
               MOVE '*'                    TO PRTF-IO-AREA (58:1)
               MOVE 1                      TO PRTF-AFTER-SPACE
               PERFORM PRTF-PRINT-LINE
               MOVE SPACES TO PRTF-IO-AREA
               INITIALIZE PRTF-IO-AREA
               MOVE '*'                    TO PRTF-IO-AREA (17:1)
               MOVE FIRMNA                 TO PRTF-IO-AREA (23:30)
               MOVE '*'                    TO PRTF-IO-AREA (58:1)
               MOVE 1                      TO PRTF-AFTER-SPACE
               PERFORM PRTF-PRINT-LINE
               MOVE SPACES TO PRTF-IO-AREA
               INITIALIZE PRTF-IO-AREA
               MOVE '*'                    TO PRTF-IO-AREA (17:1)
               MOVE '*'                    TO PRTF-IO-AREA (58:1)
               MOVE 1                      TO PRTF-AFTER-SPACE
               PERFORM PRTF-PRINT-LINE
               MOVE SPACES TO PRTF-IO-AREA
               INITIALIZE PRTF-IO-AREA
               MOVE '*'                    TO PRTF-IO-AREA (17:1)
               MOVE '*'                    TO PRTF-IO-AREA (58:1)
               MOVE 1                      TO PRTF-AFTER-SPACE
               PERFORM PRTF-PRINT-LINE
               MOVE SPACES TO PRTF-IO-AREA
               INITIALIZE PRTF-IO-AREA
               MOVE '************************' TO PRTF-IO-AREA (17:24)
               MOVE '******************'   TO PRTF-IO-AREA (41:18)
               MOVE 1                      TO PRTF-AFTER-SPACE
               PERFORM PRTF-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1)
               MOVE SPACES TO PRTF-IO-AREA
               INITIALIZE PRTF-IO-AREA
               MOVE 'SUM'                  TO PRTF-IO-AREA (2:3)
               MOVE AVD                    TO PRTF-IO-AREA (6:1)
               MOVE SASVS                  TO XO-92YY9R
               MOVE XO-92YY9R              TO PRTF-IO-AREA (60:15)
               INITIALIZE SASVS
               MOVE SASKA                  TO XO-92YY9R
               MOVE XO-92YY9R              TO PRTF-IO-AREA (77:15)
               INITIALIZE SASKA
               MOVE SAPRO                  TO XO-41YY9R
               MOVE XO-41YY9R              TO PRTF-IO-AREA (94:8)
               INITIALIZE SAPRO
               MOVE SASKAS                 TO XO-92YY9R
               MOVE XO-92YY9R              TO PRTF-IO-AREA (104:15)
               INITIALIZE SASKAS
               MOVE 2                      TO PRTF-BEFORE-SPACE
               MOVE 0                      TO PRTF-AFTER-SPACE
               PERFORM PRTF-PRINT-LINE
           END-IF
           IF  (I-L2)
               MOVE SPACES TO PRTF-IO-AREA
               INITIALIZE PRTF-IO-AREA
               MOVE 'GR. TOT.'             TO PRTF-IO-AREA (2:8)
               MOVE GTSVS                  TO XO-92YY9R
               MOVE XO-92YY9R              TO PRTF-IO-AREA (60:15)
               INITIALIZE GTSVS
               MOVE GTSKA                  TO XO-92YY9R
               MOVE XO-92YY9R              TO PRTF-IO-AREA (77:15)
               INITIALIZE GTSKA
               MOVE GTPRO                  TO XO-41YY9R
               MOVE XO-41YY9R              TO PRTF-IO-AREA (94:8)
               INITIALIZE GTPRO
               MOVE GTSKAS                 TO XO-92YY9R
               MOVE XO-92YY9R              TO PRTF-IO-AREA (104:15)
               INITIALIZE GTSKAS
      *******************************************
      *  PRINTRUTINE FOR FØRSTESIDE PR. FIRMA.  *
      *******************************************
               MOVE 01                     TO PRTF-BEFORE-SKIP
               MOVE 0                      TO PRTF-AFTER-SPACE
               PERFORM PRTF-PRINT-LINE
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
           INITIALIZE REGPAR-DATA-FIELDS
           SET REGPAR-EOF-OFF              TO TRUE
           SET REGPAR-PROCESS              TO TRUE
           OPEN INPUT REGPAR
           SET SVSIN-LEVEL-INIT            TO TRUE
           INITIALIZE SVSIN-DATA-FIELDS
           SET SVSIN-EOF-OFF               TO TRUE
           SET SVSIN-PROCESS               TO TRUE
           OPEN INPUT SVSIN
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           OPEN OUTPUT SVSOUT
           OPEN OUTPUT PRTF
           INITIALIZE PRTF-IO-AREA
           INITIALIZE PRTF-DATA-FIELDS
           MOVE 57                         TO PRTF-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE REGPAR
           CLOSE SVSIN
           CLOSE FIRMAF
           CLOSE SVSOUT
           IF PRTF-IO-AREA NOT = SPACES
             WRITE PRTF-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO PRTF-IO-AREA
           END-IF
           CLOSE PRTF.
 
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
