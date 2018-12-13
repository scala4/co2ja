       IDENTIFICATION DIVISION.
       PROGRAM-ID. VRG275R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAMNAVN: VRG275, BEREGNER LAGERBEHOLDNING PR VAREGRUPPE   *
      *                      PR AVDELING.                             *
      * LAGET AV   : M. TUVRØNNINGEN.                                 *
      * ENDRINGER  :                                                  *
      * REGPAR     : REGNSKAPSPARAMETER (AKTUELL PERIODE),            *
      *              VAREMASTER (ANTALLSBEHOLDNINGER OG SELVKOST),    *
      *              FIRMAF (REGNSKAPSMÅNED),                         *
      * GJØR       : BEREGNER LAGERVERDI FOR HVER VARGEGRUPPE FOR     *
      *              INNEN HVER AVDELING.                             *
      * GIR        : BEHOLDNINGSFILE FOR AKTUELL MÅNED.               *
      *              KVITTERINGSLISTE.                                *
      *****************************************************************
      *
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VRG275.rpg
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
           SELECT VAREREC
               ASSIGN TO UT-S-VAREREC
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VAREREC-STATUS.
           SELECT FAKFILO
               ASSIGN TO UT-S-FAKFILO
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKFILO-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD REGPAR
               BLOCK CONTAINS 100
               RECORD CONTAINS 100.
       01  REGPAR-IO-AREA.
           05  REGPAR-IO-AREA-X            PICTURE X(100).
       FD VAREREC
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  VAREREC-IO-AREA.
           05  VAREREC-IO-AREA-X           PICTURE X(200).
       FD FAKFILO
               BLOCK CONTAINS 300
               RECORD CONTAINS 30.
       01  FAKFILO-IO-AREA.
           05  FAKFILO-IO-AREA-X           PICTURE X(30).
      *BUGFILO O   F  80  80            PRINTERSYSLST
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
           10  REGPAR-STATUS               PICTURE 99 VALUE 0.
           10  VAREREC-STATUS              PICTURE 99 VALUE 0.
           10  FAKFILO-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
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
               88  VAREREC-EOF-OFF         VALUE '0'.
               88  VAREREC-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREREC-READ-OFF        VALUE '0'.
               88  VAREREC-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREREC-PROCESS-OFF     VALUE '0'.
               88  VAREREC-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  VAREREC-LEVEL-INIT-OFF  VALUE '0'.
               88  VAREREC-LEVEL-INIT      VALUE '1'.
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
           05  REGPAR-DATA-FIELDS.
               10  PMND                    PICTURE X(2).
               10  PAAR                    PICTURE X(2).
               10  PARH                    PICTURE X(2).
           05  VAREREC-LEVEL-02.
               10  VAREREC-02-L2.
                   15  VAREREC-02-L2-FIRMA PICTURE X(3).
               10  VAREREC-02-L1.
                   15  VAREREC-02-L1-VGR   PICTURE X(5).
           05  VAREREC-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  SKAF                    PICTURE X(1).
               10  SELVK-IO.
                   15  SELVK               PICTURE S9(7)V9(2).
               10  ANTIN-IO.
                   15  ANTIN               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ANTUT-IO.
                   15  ANTUT               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  PRITYP                  PICTURE X(1).
               10  VGR                     PICTURE X(5).
               10  SLETT                   PICTURE X(1).
               10  PRITIL-IO.
                   15  PRITIL              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  BEH13-IO.
                   15  BEH13               PICTURE S9(3)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  BEH93-IO.
                   15  BEH93               PICTURE S9(3)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  BEH15-IO.
                   15  BEH15               PICTURE S9(3)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  BEH17-IO.
                   15  BEH17               PICTURE S9(3)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  BEH92-IO.
                   15  BEH92               PICTURE S9(3)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  BEH18-IO.
                   15  BEH18               PICTURE S9(3)V9(2) USAGE
                                                       PACKED-DECIMAL.
      *
      *  01                MOVE "PMND    "BUGFL1  8        DISPLAY FIELD
      *  01      BUGFL1    DEBUGBUGFILO   PMND             VIS INDIKATOR
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(5).
           05  TEMPORARY-FIELDS.
               10  BEH10-IO.
                   15  BEH10               PICTURE S9(9)V9(2).
               10  VERDI-IO.
                   15  VERDI               PICTURE S9(9)V9(2).
               10  L1V10-IO.
                   15  L1V10               PICTURE S9(9)V9(2).
               10  L1V13-IO.
                   15  L1V13               PICTURE S9(9)V9(2).
               10  L1V15-IO.
                   15  L1V15               PICTURE S9(9)V9(2).
               10  L1V17-IO.
                   15  L1V17               PICTURE S9(9)V9(2).
               10  L1V18-IO.
                   15  L1V18               PICTURE S9(9)V9(2).
               10  L1V92-IO.
                   15  L1V92               PICTURE S9(9)V9(2).
               10  L1V93-IO.
                   15  L1V93               PICTURE S9(9)V9(2).
               10  L1V10U-IO.
                   15  L1V10U              PICTURE S9(11).
               10  L1V13U-IO.
                   15  L1V13U              PICTURE S9(11).
               10  L1V15U-IO.
                   15  L1V15U              PICTURE S9(11).
               10  L1V17U-IO.
                   15  L1V17U              PICTURE S9(11).
               10  L1V18U-IO.
                   15  L1V18U              PICTURE S9(11).
               10  L1V92U-IO.
                   15  L1V92U              PICTURE S9(11).
               10  L1V93U-IO.
                   15  L1V93U              PICTURE S9(11).
               10  L2V10-IO.
                   15  L2V10               PICTURE S9(11).
               10  L2V13-IO.
                   15  L2V13               PICTURE S9(11).
               10  L2V15-IO.
                   15  L2V15               PICTURE S9(11).
               10  L2V17-IO.
                   15  L2V17               PICTURE S9(11).
               10  L2V18-IO.
                   15  L2V18               PICTURE S9(11).
               10  L2V92-IO.
                   15  L2V92               PICTURE S9(11).
               10  L2V93-IO.
                   15  L2V93               PICTURE S9(11).
           05  EDITTING-FIELDS.
               10  XO-110YY9R              PICTURE ZZ.ZZZ.ZZZ.ZZ9-.
               10  XO-110P-EF.
                 15  XO-110P               PICTURE S9(11) USAGE
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
               88  NOT-SET-I-OV            VALUE '0'.
               88  SET-I-OV                VALUE '1'.
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
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
           PERFORM HEADING-OUTPUT
           IF  NOT-SET-I-OV
               SET NOT-I-OV                TO TRUE
           END-IF
           SET NOT-SET-I-OV                TO TRUE
 
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
           IF  REGPAR-PROCESS
               SET REGPAR-PROCESS-OFF      TO TRUE
               SET REGPAR-READ             TO TRUE
           END-IF
 
           IF  REGPAR-READ
           AND RECORD-SELECTED-OFF
               PERFORM REGPAR-GET
               SET REGPAR-READ-OFF         TO TRUE
               IF  NOT REGPAR-EOF
                   SET REGPAR-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  VAREREC-PROCESS
               SET VAREREC-PROCESS-OFF     TO TRUE
               SET VAREREC-READ            TO TRUE
           END-IF
 
           IF  VAREREC-READ
           AND RECORD-SELECTED-OFF
               PERFORM VAREREC-GET
               SET VAREREC-READ-OFF        TO TRUE
               IF  NOT VAREREC-EOF
                   SET VAREREC-PROCESS     TO TRUE
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
 
           IF  VAREREC-PROCESS
               PERFORM VAREREC-IDSET
           END-IF
 
           IF  VAREREC-PROCESS
               PERFORM VAREREC-CHK-LEVEL
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
           PERFORM HEADING-OVERFLOW
 
           IF  REGPAR-PROCESS
               PERFORM REGPAR-FLDSET
           END-IF
 
           IF  VAREREC-PROCESS
               PERFORM VAREREC-FLDOFF
               PERFORM VAREREC-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  VAREREC-PROCESS
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
               GO TO SLUTT-T
      *  L2      FIRMA     COMP "938"                    38
           END-IF
           IF  (I-L2)
               SET NOT-I-38                TO TRUE
               IF  FIRMA = '627'
                   SET I-38                TO TRUE
               END-IF
           END-IF
           IF  (I-L1)
               SET NOT-I-13                TO TRUE
               SET NOT-I-15                TO TRUE
               SET NOT-I-17                TO TRUE
               SET NOT-I-18                TO TRUE
               SET NOT-I-92                TO TRUE
               SET NOT-I-93                TO TRUE
      *    NEDSKREVEDE VARER SKAL MED.
           END-IF
           IF  (I-02)
               SET NOT-I-70                TO TRUE
               IF  SKAF = '9'
                   SET I-70                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-70)
               SET NOT-I-70                TO TRUE
               IF  SLETT = 'S'
                   SET I-70                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-70)
               OR  (I-02 AND I-41)
               GO TO SLUTT-T
           END-IF
           IF  (I-02)
               SUBTRACT ANTUT FROM ANTIN GIVING BEH10 ROUNDED
               SUBTRACT BEH13              FROM BEH10
               SUBTRACT BEH15              FROM BEH10
               SUBTRACT BEH17              FROM BEH10
               SUBTRACT BEH18              FROM BEH10
               SUBTRACT BEH92              FROM BEH10
               SUBTRACT BEH93              FROM BEH10
               MULTIPLY SELVK BY BEH10 GIVING VERDI ROUNDED
               ADD VERDI                   TO L1V10
               SET NOT-I-10                TO TRUE
               IF  L1V10 NOT = 0
                   SET I-10                TO TRUE
               END-IF
               MULTIPLY SELVK BY BEH13 GIVING VERDI ROUNDED
               ADD VERDI                   TO L1V13
               SET NOT-I-13                TO TRUE
               IF  L1V13 NOT = 0
                   SET I-13                TO TRUE
               END-IF
               MULTIPLY SELVK BY BEH15 GIVING VERDI ROUNDED
               ADD VERDI                   TO L1V15
               SET NOT-I-15                TO TRUE
               IF  L1V15 NOT = 0
                   SET I-15                TO TRUE
               END-IF
               MULTIPLY SELVK BY BEH17 GIVING VERDI ROUNDED
               ADD VERDI                   TO L1V17
               SET NOT-I-17                TO TRUE
               IF  L1V17 NOT = 0
                   SET I-17                TO TRUE
               END-IF
               MULTIPLY SELVK BY BEH18 GIVING VERDI ROUNDED
               ADD VERDI                   TO L1V18
               SET NOT-I-18                TO TRUE
               IF  L1V18 NOT = 0
                   SET I-18                TO TRUE
               END-IF
               MULTIPLY SELVK BY BEH92 GIVING VERDI ROUNDED
               ADD VERDI                   TO L1V92
               SET NOT-I-92                TO TRUE
               IF  L1V92 NOT = 0
                   SET I-92                TO TRUE
               END-IF
               MULTIPLY SELVK BY BEH93 GIVING VERDI ROUNDED
               ADD VERDI                   TO L1V93
               SET NOT-I-93                TO TRUE
               IF  L1V93 NOT = 0
                   SET I-93                TO TRUE
               END-IF
               SET NOT-I-71                TO TRUE
               IF  PRITYP = ' '
                   SET I-71                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-71)
               SET NOT-I-71                TO TRUE
               IF  PRITYP = 'I'
                   SET I-71                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-71)
               SET NOT-I-71                TO TRUE
               IF  PRITYP = 'S'
                   SET I-71                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-71)
               OR  (I-02 AND I-38)
               GO TO SLUTT-T
           END-IF
           IF  (I-02)
               MULTIPLY PRITIL BY BEH10 GIVING VERDI ROUNDED
               ADD VERDI                   TO L1V10
               SET NOT-I-13                TO TRUE
               IF  L1V10 NOT = 0
                   SET I-13                TO TRUE
               END-IF
               MULTIPLY PRITIL BY BEH13 GIVING VERDI ROUNDED
               ADD VERDI                   TO L1V13
               SET NOT-I-13                TO TRUE
               IF  L1V13 NOT = 0
                   SET I-13                TO TRUE
               END-IF
               MULTIPLY PRITIL BY BEH15 GIVING VERDI ROUNDED
               ADD VERDI                   TO L1V15
               SET NOT-I-15                TO TRUE
               IF  L1V15 NOT = 0
                   SET I-15                TO TRUE
               END-IF
               MULTIPLY PRITIL BY BEH17 GIVING VERDI ROUNDED
               ADD VERDI                   TO L1V17
               SET NOT-I-17                TO TRUE
               IF  L1V17 NOT = 0
                   SET I-17                TO TRUE
               END-IF
               MULTIPLY PRITIL BY BEH18 GIVING VERDI ROUNDED
               ADD VERDI                   TO L1V18
               SET NOT-I-18                TO TRUE
               IF  L1V18 NOT = 0
                   SET I-18                TO TRUE
               END-IF
               MULTIPLY PRITIL BY BEH92 GIVING VERDI ROUNDED
               ADD VERDI                   TO L1V92
               SET NOT-I-92                TO TRUE
               IF  L1V92 NOT = 0
                   SET I-92                TO TRUE
               END-IF
               MULTIPLY PRITIL BY BEH93 GIVING VERDI ROUNDED
               ADD VERDI                   TO L1V93
               SET NOT-I-93                TO TRUE
               IF  L1V93 NOT = 0
                   SET I-93                TO TRUE
               END-IF
           END-IF.
 
       SLUTT-T.
           CONTINUE.
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1)
               ADD L1V10 TO ZERO       GIVING L1V10U ROUNDED
               ADD L1V13 TO ZERO       GIVING L1V13U ROUNDED
               ADD L1V15 TO ZERO       GIVING L1V15U ROUNDED
               ADD L1V17 TO ZERO       GIVING L1V17U ROUNDED
               ADD L1V18 TO ZERO       GIVING L1V18U ROUNDED
               ADD L1V92 TO ZERO       GIVING L1V92U ROUNDED
               ADD L1V93 TO ZERO       GIVING L1V93U ROUNDED
               ADD L1V10                   TO L2V10
               ADD L1V13                   TO L2V13
               ADD L1V15                   TO L2V15
               ADD L1V17                   TO L2V17
               ADD L1V18                   TO L2V18
               ADD L1V92                   TO L2V92
               ADD L1V93                   TO L2V93
               MOVE 0                      TO L1V10
               MOVE 0                      TO L1V13
               MOVE 0                      TO L1V15
               MOVE 0                      TO L1V17
               MOVE 0                      TO L1V18
               MOVE 0                      TO L1V92
               MOVE 0                      TO L1V93
           END-IF.
 
       REGPAR-GET SECTION.
       REGPAR-GET-P.
           IF  REGPAR-EOF-OFF
               READ REGPAR
               AT END
                   SET REGPAR-EOF          TO TRUE
               END-READ
           END-IF.
 
       REGPAR-FLDSET SECTION.
       REGPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE REGPAR-IO-AREA (5:2)   TO PMND (1:2)
               MOVE REGPAR-IO-AREA (3:2)   TO PAAR (1:2)
               MOVE REGPAR-IO-AREA (20:2)  TO PARH (1:2)
           END-EVALUATE.
 
       REGPAR-IDSET SECTION.
       REGPAR-IDSET-P.
           SET I-01                        TO TRUE.
 
       VAREREC-GET SECTION.
       VAREREC-GET-P.
           IF  VAREREC-EOF-OFF
               READ VAREREC
               AT END
                   SET VAREREC-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VAREREC-FLDOFF SECTION.
       VAREREC-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-41                TO TRUE
               SET NOT-I-70                TO TRUE
           END-EVALUATE.
 
       VAREREC-FLDSET SECTION.
       VAREREC-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREREC-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE VAREREC-IO-AREA (6:1)  TO SKAF (1:1)
               MOVE VAREREC-IO-AREA (66:9) TO SELVK-IO
               INSPECT SELVK-IO REPLACING ALL ' ' BY '0'
               IF  SELVK = ZERO
                   SET I-41                TO TRUE
               END-IF
               MOVE VAREREC-IO-AREA (97:5) TO ANTIN-IO
               MOVE VAREREC-IO-AREA (102:5) TO ANTUT-IO
               MOVE VAREREC-IO-AREA (107:1) TO PRITYP (1:1)
               MOVE VAREREC-IO-AREA (118:5) TO VGR (1:5)
               MOVE VAREREC-IO-AREA (128:1) TO SLETT (1:1)
               MOVE VAREREC-IO-AREA (161:4) TO PRITIL-IO
               IF  PRITIL = ZERO
                   SET I-70                TO TRUE
               END-IF
               MOVE VAREREC-IO-AREA (179:3) TO BEH13-IO
               MOVE VAREREC-IO-AREA (182:3) TO BEH93-IO
               MOVE VAREREC-IO-AREA (185:3) TO BEH15-IO
               MOVE VAREREC-IO-AREA (188:3) TO BEH17-IO
               MOVE VAREREC-IO-AREA (191:3) TO BEH92-IO
               MOVE VAREREC-IO-AREA (194:3) TO BEH18-IO
           END-EVALUATE.
 
       VAREREC-IDSET SECTION.
       VAREREC-IDSET-P.
           SET I-02                        TO TRUE.
 
       VAREREC-CHK-LEVEL SECTION.
       VAREREC-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO VAREREC-LEVEL-02
               MOVE VAREREC-IO-AREA (3:3)  TO VAREREC-02-L2-FIRMA
               MOVE VAREREC-IO-AREA (118:5) TO VAREREC-02-L1-VGR
               IF  VAREREC-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VAREREC-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  VAREREC-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  VAREREC-02-L2         TO THE-PRIOR-L2
               MOVE  VAREREC-02-L1         TO THE-PRIOR-L1
               SET VAREREC-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
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
               SET I-OV                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OV            TO TRUE
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
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L2)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '* AVDELINGSVIS LAGER   *' TO LISTE-IO-AREA (25:24)
               MOVE '* FIRMA'              TO LISTE-IO-AREA (49:7)
               MOVE FIRMA                  TO LISTE-IO-AREA (57:3)
               MOVE '*'                    TO LISTE-IO-AREA (72:1)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '***    KONTROLL-LISTE OV' TO LISTE-IO-AREA (1:24)
               MOVE 'ER LAGERVERDI PR FIRMA V' TO LISTE-IO-AREA (25:24)
               MOVE 'ED MÅNEDSLUTT. PROGRAM V' TO LISTE-IO-AREA (49:24)
               MOVE 'RG275   JOB DOP80UM     ' TO LISTE-IO-AREA (73:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE '************************' TO LISTE-IO-AREA (49:24)
               MOVE '************************' TO LISTE-IO-AREA (73:24)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'VGRP'                 TO LISTE-IO-AREA (2:4)
               MOVE '      LAGER 10'       TO LISTE-IO-AREA (7:14)
               MOVE '      LAGER 13'       TO LISTE-IO-AREA (22:14)
               MOVE '      LAGER 15'       TO LISTE-IO-AREA (37:14)
               MOVE '      LAGER 17'       TO LISTE-IO-AREA (52:14)
               MOVE '      LAGER 18'       TO LISTE-IO-AREA (67:14)
               MOVE '      LAGER 92'       TO LISTE-IO-AREA (82:14)
               MOVE '      LAGER 93'       TO LISTE-IO-AREA (97:14)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OV)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '* AVDELINGSVIS LAGER   *' TO LISTE-IO-AREA (25:24)
               MOVE '* FIRMA'              TO LISTE-IO-AREA (49:7)
               MOVE FIRMA                  TO LISTE-IO-AREA (57:3)
               MOVE '*'                    TO LISTE-IO-AREA (72:1)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '***    KONTROLL-LISTE OV' TO LISTE-IO-AREA (1:24)
               MOVE 'ER LAGERVERDI PR FIRMA V' TO LISTE-IO-AREA (25:24)
               MOVE 'ED MÅNEDSLUTT. PROGRAM V' TO LISTE-IO-AREA (49:24)
               MOVE 'RG275   JOB DOP80UM     ' TO LISTE-IO-AREA (73:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE '************************' TO LISTE-IO-AREA (49:24)
               MOVE '************************' TO LISTE-IO-AREA (73:24)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'VGRP'                 TO LISTE-IO-AREA (2:4)
               MOVE '      LAGER 10'       TO LISTE-IO-AREA (7:14)
               MOVE '      LAGER 13'       TO LISTE-IO-AREA (22:14)
               MOVE '      LAGER 15'       TO LISTE-IO-AREA (37:14)
               MOVE '      LAGER 17'       TO LISTE-IO-AREA (52:14)
               MOVE '      LAGER 18'       TO LISTE-IO-AREA (67:14)
               MOVE '      LAGER 92'       TO LISTE-IO-AREA (82:14)
               MOVE '      LAGER 93'       TO LISTE-IO-AREA (97:14)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE VGR                    TO LISTE-IO-AREA (1:5)
               MOVE L1V10U                 TO XO-110YY9R
               MOVE XO-110YY9R             TO LISTE-IO-AREA (6:15)
               MOVE L1V13U                 TO XO-110YY9R
               MOVE XO-110YY9R             TO LISTE-IO-AREA (21:15)
               MOVE L1V15U                 TO XO-110YY9R
               MOVE XO-110YY9R             TO LISTE-IO-AREA (36:15)
               MOVE L1V17U                 TO XO-110YY9R
               MOVE XO-110YY9R             TO LISTE-IO-AREA (51:15)
               MOVE L1V18U                 TO XO-110YY9R
               MOVE XO-110YY9R             TO LISTE-IO-AREA (66:15)
               MOVE L1V92U                 TO XO-110YY9R
               MOVE XO-110YY9R             TO LISTE-IO-AREA (81:15)
               MOVE L1V93U                 TO XO-110YY9R
               MOVE XO-110YY9R             TO LISTE-IO-AREA (96:15)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L2)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'SUM'                  TO LISTE-IO-AREA (1:3)
               MOVE L2V10                  TO XO-110YY9R
               MOVE XO-110YY9R             TO LISTE-IO-AREA (6:15)
               INITIALIZE L2V10
               MOVE L2V13                  TO XO-110YY9R
               MOVE XO-110YY9R             TO LISTE-IO-AREA (21:15)
               INITIALIZE L2V13
               MOVE L2V15                  TO XO-110YY9R
               MOVE XO-110YY9R             TO LISTE-IO-AREA (36:15)
               INITIALIZE L2V15
               MOVE L2V17                  TO XO-110YY9R
               MOVE XO-110YY9R             TO LISTE-IO-AREA (51:15)
               INITIALIZE L2V17
               MOVE L2V18                  TO XO-110YY9R
               MOVE XO-110YY9R             TO LISTE-IO-AREA (66:15)
               INITIALIZE L2V18
               MOVE L2V92                  TO XO-110YY9R
               MOVE XO-110YY9R             TO LISTE-IO-AREA (81:15)
               INITIALIZE L2V92
               MOVE L2V93                  TO XO-110YY9R
               MOVE XO-110YY9R             TO LISTE-IO-AREA (96:15)
               INITIALIZE L2V93
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L1 AND I-10)
               MOVE SPACES TO FAKFILO-IO-AREA
               INITIALIZE FAKFILO-IO-AREA
               MOVE FIRMA                  TO FAKFILO-IO-AREA (3:3)
               MOVE VGR                    TO FAKFILO-IO-AREA (6:5)
               MOVE '10'                   TO FAKFILO-IO-AREA (11:2)
               MOVE L1V10U                 TO XO-110P
               MOVE XO-110P-EF             TO FAKFILO-IO-AREA (13:6)
               INITIALIZE L1V10U
               MOVE PARH                   TO FAKFILO-IO-AREA (25:2)
               MOVE PAAR                   TO FAKFILO-IO-AREA (27:2)
               MOVE PMND                   TO FAKFILO-IO-AREA (29:2)
               WRITE FAKFILO-IO-AREA
           END-IF
           IF  (I-L1 AND I-13)
               MOVE SPACES TO FAKFILO-IO-AREA
               INITIALIZE FAKFILO-IO-AREA
               MOVE FIRMA                  TO FAKFILO-IO-AREA (3:3)
               MOVE VGR                    TO FAKFILO-IO-AREA (6:5)
               MOVE '13'                   TO FAKFILO-IO-AREA (11:2)
               MOVE L1V13U                 TO XO-110P
               MOVE XO-110P-EF             TO FAKFILO-IO-AREA (13:6)
               INITIALIZE L1V13U
               MOVE PARH                   TO FAKFILO-IO-AREA (25:2)
               MOVE PAAR                   TO FAKFILO-IO-AREA (27:2)
               MOVE PMND                   TO FAKFILO-IO-AREA (29:2)
               WRITE FAKFILO-IO-AREA
           END-IF
           IF  (I-L1 AND I-15)
               MOVE SPACES TO FAKFILO-IO-AREA
               INITIALIZE FAKFILO-IO-AREA
               MOVE FIRMA                  TO FAKFILO-IO-AREA (3:3)
               MOVE VGR                    TO FAKFILO-IO-AREA (6:5)
               MOVE '15'                   TO FAKFILO-IO-AREA (11:2)
               MOVE L1V15U                 TO XO-110P
               MOVE XO-110P-EF             TO FAKFILO-IO-AREA (13:6)
               INITIALIZE L1V15U
               MOVE PARH                   TO FAKFILO-IO-AREA (25:2)
               MOVE PAAR                   TO FAKFILO-IO-AREA (27:2)
               MOVE PMND                   TO FAKFILO-IO-AREA (29:2)
               WRITE FAKFILO-IO-AREA
           END-IF
           IF  (I-L1 AND I-17)
               MOVE SPACES TO FAKFILO-IO-AREA
               INITIALIZE FAKFILO-IO-AREA
               MOVE FIRMA                  TO FAKFILO-IO-AREA (3:3)
               MOVE VGR                    TO FAKFILO-IO-AREA (6:5)
               MOVE '17'                   TO FAKFILO-IO-AREA (11:2)
               MOVE L1V17U                 TO XO-110P
               MOVE XO-110P-EF             TO FAKFILO-IO-AREA (13:6)
               INITIALIZE L1V17U
               MOVE PARH                   TO FAKFILO-IO-AREA (25:2)
               MOVE PAAR                   TO FAKFILO-IO-AREA (27:2)
               MOVE PMND                   TO FAKFILO-IO-AREA (29:2)
               WRITE FAKFILO-IO-AREA
           END-IF
           IF  (I-L1 AND I-18)
               MOVE SPACES TO FAKFILO-IO-AREA
               INITIALIZE FAKFILO-IO-AREA
               MOVE FIRMA                  TO FAKFILO-IO-AREA (3:3)
               MOVE VGR                    TO FAKFILO-IO-AREA (6:5)
               MOVE '18'                   TO FAKFILO-IO-AREA (11:2)
               MOVE L1V18U                 TO XO-110P
               MOVE XO-110P-EF             TO FAKFILO-IO-AREA (13:6)
               INITIALIZE L1V18U
               MOVE PARH                   TO FAKFILO-IO-AREA (25:2)
               MOVE PAAR                   TO FAKFILO-IO-AREA (27:2)
               MOVE PMND                   TO FAKFILO-IO-AREA (29:2)
               WRITE FAKFILO-IO-AREA
           END-IF
           IF  (I-L1 AND I-92)
               MOVE SPACES TO FAKFILO-IO-AREA
               INITIALIZE FAKFILO-IO-AREA
               MOVE FIRMA                  TO FAKFILO-IO-AREA (3:3)
               MOVE VGR                    TO FAKFILO-IO-AREA (6:5)
               MOVE '92'                   TO FAKFILO-IO-AREA (11:2)
               MOVE L1V92U                 TO XO-110P
               MOVE XO-110P-EF             TO FAKFILO-IO-AREA (13:6)
               INITIALIZE L1V92U
               MOVE PARH                   TO FAKFILO-IO-AREA (25:2)
               MOVE PAAR                   TO FAKFILO-IO-AREA (27:2)
               MOVE PMND                   TO FAKFILO-IO-AREA (29:2)
               WRITE FAKFILO-IO-AREA
           END-IF
           IF  (I-L1 AND I-93)
               MOVE SPACES TO FAKFILO-IO-AREA
               INITIALIZE FAKFILO-IO-AREA
               MOVE FIRMA                  TO FAKFILO-IO-AREA (3:3)
               MOVE VGR                    TO FAKFILO-IO-AREA (6:5)
               MOVE '93'                   TO FAKFILO-IO-AREA (11:2)
               MOVE L1V92U                 TO XO-110P
               MOVE XO-110P-EF             TO FAKFILO-IO-AREA (13:6)
               INITIALIZE L1V92U
               MOVE PARH                   TO FAKFILO-IO-AREA (25:2)
               MOVE PAAR                   TO FAKFILO-IO-AREA (27:2)
               MOVE PMND                   TO FAKFILO-IO-AREA (29:2)
               WRITE FAKFILO-IO-AREA
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
           INITIALIZE REGPAR-DATA-FIELDS
           SET REGPAR-EOF-OFF              TO TRUE
           SET REGPAR-PROCESS              TO TRUE
           OPEN INPUT REGPAR
           SET VAREREC-LEVEL-INIT          TO TRUE
           INITIALIZE VAREREC-DATA-FIELDS
           SET VAREREC-EOF-OFF             TO TRUE
           SET VAREREC-PROCESS             TO TRUE
           OPEN INPUT VAREREC
           OPEN OUTPUT FAKFILO
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE REGPAR
           CLOSE VAREREC
           CLOSE FAKFILO
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
