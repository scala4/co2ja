       IDENTIFICATION DIVISION.
       PROGRAM-ID. VAT311R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAM....: VAT311                                           *
      * PROGRAMERER: ESPEN LARSEN                                     *
      * PROGRAMERT.: 22.01.1992                                       *
      * SIST RETTET: 22.01.1992     AV: ESPEN LARSEN                  *
      * OPPGAVE....: OVERFØRE BEHOLDNING + KORRIGERINGER FRA          *
      *              VARET.MASTER TIL VARE.MASTER.TELLING.            *
      *                                                               *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VAT311.rpg
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
           SELECT VARETIN
               ASSIGN TO UT-S-VARETIN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VARETIN-STATUS.
           SELECT VAREMTL
               ASSIGN TO VAREMTL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VAREMTL-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD VARETIN
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  VARETIN-IO-AREA.
           05  VARETIN-IO-AREA-X           PICTURE X(200).
       FD VAREMTL
               RECORD CONTAINS 200.
       01  VAREMTL-IO-AREA.
           05  VAREMTL-IO-AREA-X           PICTURE X(200).
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
           10  VARETIN-STATUS              PICTURE 99 VALUE 0.
           10  VAREMTL-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  VARETIN-EOF-OFF         VALUE '0'.
               88  VARETIN-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VARETIN-READ-OFF        VALUE '0'.
               88  VARETIN-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VARETIN-PROCESS-OFF     VALUE '0'.
               88  VARETIN-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  VARETIN-LEVEL-INIT-OFF  VALUE '0'.
               88  VARETIN-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREMTL-EOF-OFF         VALUE '0'.
               88  VAREMTL-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREMTL-READ-OFF        VALUE '0'.
               88  VAREMTL-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREMTL-PROCESS-OFF     VALUE '0'.
               88  VAREMTL-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  VAREMTL-LEVEL-INIT-OFF  VALUE '0'.
               88  VAREMTL-LEVEL-INIT      VALUE '1'.
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
           05  VARETIN-LEVEL-01.
               10  VARETIN-01-L1.
                   15  VARETIN-01-L1-FEKEY PICTURE X(10).
           05  VARETIN-DATA-FIELDS.
               10  FEKEY                   PICTURE X(10).
               10  TANTIN-IO.
                   15  TANTIN              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  TANTUT-IO.
                   15  TANTUT              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  TKORIN-IO.
                   15  TKORIN              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  TBEH13-IO.
                   15  TBEH13              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  TBEH93-IO.
                   15  TBEH93              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  TBEH15-IO.
                   15  TBEH15              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  TBEH17-IO.
                   15  TBEH17              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  TBEH92-IO.
                   15  TBEH92              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  TBEH18-IO.
                   15  TBEH18              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  TKOR13-IO.
                   15  TKOR13              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  TKOR93-IO.
                   15  TKOR93              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  TKOR15-IO.
                   15  TKOR15              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  TKOR17-IO.
                   15  TKOR17              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  TKOR92-IO.
                   15  TKOR92              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  TKOR18-IO.
                   15  TKOR18              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
           05  VARETIN-MP                  PICTURE X(10).
           05  VARETIN-MC                  PICTURE X(10).
           05  VARETIN-M-01            REDEFINES VARETIN-MC.
               10  VARETIN-M-01-M1.
                   15  VARETIN-M-01-M1-FEKEY-G.
                       20  VARETIN-M-01-M1-FEKEY PICTURE X(10).
           05  VAREMTL-LEVEL-02.
               10  VAREMTL-02-L1.
                   15  VAREMTL-02-L1-VMKEY PICTURE X(10).
           05  VAREMTL-DATA-FIELDS.
               10  VMKEY                   PICTURE X(10).
               10  MANTI-IO.
                   15  MANTI               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  MANTU-IO.
                   15  MANTU               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
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
           05  VAREMTL-MP                  PICTURE X(10).
           05  VAREMTL-MC                  PICTURE X(10).
           05  VAREMTL-M-02            REDEFINES VAREMTL-MC.
               10  VAREMTL-M-02-M1.
                   15  VAREMTL-M-02-M1-VMKEY-G.
                       20  VAREMTL-M-02-M1-VMKEY PICTURE X(10).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(10).
           05  TEMPORARY-FIELDS.
               10  ANTI-IO.
                   15  ANTI                PICTURE S9(7).
               10  ANTK-IO.
                   15  ANTK                PICTURE S9(7).
           05  EDITTING-FIELDS.
               10  XO-72P-EF.
                 15  XO-72P                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-50P-EF.
                 15  XO-50P                PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  XO-70YY9                PICTURE Z.ZZZ.ZZ9.
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
           IF  VARETIN-PROCESS
               SET VARETIN-PROCESS-OFF     TO TRUE
               SET VARETIN-READ            TO TRUE
           END-IF
 
           IF  VARETIN-READ
               PERFORM VARETIN-GET
               SET VARETIN-READ-OFF        TO TRUE
               IF  NOT VARETIN-EOF
                   PERFORM VARETIN-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM VARETIN-MATCH-SET
               END-IF
           END-IF
 
           IF  VAREMTL-PROCESS
               SET VAREMTL-PROCESS-OFF     TO TRUE
               SET VAREMTL-READ            TO TRUE
           END-IF
 
           IF  VAREMTL-READ
               PERFORM VAREMTL-GET
               SET VAREMTL-READ-OFF        TO TRUE
               IF  NOT VAREMTL-EOF
                   PERFORM VAREMTL-MATCH-SET
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
 
           IF  VARETIN-PROCESS
               PERFORM VARETIN-IDSET
           END-IF
 
           IF  VAREMTL-PROCESS
               PERFORM VAREMTL-IDSET
           END-IF
 
           IF  VARETIN-PROCESS
               PERFORM VARETIN-CHK-LEVEL
           END-IF
 
           IF  VAREMTL-PROCESS
               PERFORM VAREMTL-CHK-LEVEL
           END-IF
 
           IF I-1ST
               GO TO LR-INDICATOR-TEST
           END-IF.
 
       MAINLINE-TOTAL-CALCS.
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
 
           IF  VARETIN-PROCESS
               PERFORM VARETIN-FLDSET
           END-IF
 
           IF  VAREMTL-PROCESS
               PERFORM VAREMTL-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  VARETIN-PROCESS
           OR  VAREMTL-PROCESS
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
               SET NOT-I-20                TO TRUE
           END-IF
           IF  (I-01)
               ADD 1                       TO ANTI
               GO TO SLUTT-T
           END-IF
           IF  (I-02 AND NOT-I-MR)
               GO TO SLUTT-T
      *****************************************************************
      *  RUTINE FOR Å REGNE UT NYE BEHOLDNINGER PÅ VARER SOM ER KORR. *
      *  ETTER FROSSING.                                              *
      *****************************************************************
      *          TKORIN    COMP 0                    1111
      * N11                GOTO SLUTT                       INGEN KORR.
           END-IF
           ADD TKORIN                      TO TANTIN
           ADD TKOR13                      TO TBEH13
           ADD TKOR93                      TO TBEH93
           ADD TKOR15                      TO TBEH15
           ADD TKOR17                      TO TBEH17
           ADD TKOR92                      TO TBEH92
           ADD TKOR18                      TO TBEH18
      *****************************************************************
      * RUTINE FOR OG OVERFØRE NYE BEHOLDNINGER TIL                   *
      * VARE.MASTER.TELLING.                                          *
      *****************************************************************
           ADD TANTIN TO ZERO          GIVING MANTI
           ADD TANTUT TO ZERO          GIVING MANTU
           ADD TBEH13 TO ZERO          GIVING LAG13
           ADD TBEH93 TO ZERO          GIVING LAG93
           ADD TBEH15 TO ZERO          GIVING LAG15
           ADD TBEH17 TO ZERO          GIVING LAG17
           ADD TBEH92 TO ZERO          GIVING LAG92
           ADD TBEH18 TO ZERO          GIVING LAG18
           ADD 1                           TO ANTK
           IF  (NOT-I-U1)
               SET I-20                    TO TRUE
           END-IF.
 
       SLUTT-T.
           CONTINUE.
 
       VARETIN-GET SECTION.
       VARETIN-GET-P.
           IF  VARETIN-EOF-OFF
               READ VARETIN
               AT END
                   SET VARETIN-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VARETIN-FLDSET SECTION.
       VARETIN-FLDSET-P.
           EVALUATE TRUE
           WHEN ( VARETIN-IO-AREA (1:1) = '8' )
               MOVE VARETIN-IO-AREA (3:10) TO FEKEY (1:10)
               MOVE VARETIN-IO-AREA (83:5) TO TANTIN-IO
               MOVE VARETIN-IO-AREA (88:5) TO TANTUT-IO
               MOVE VARETIN-IO-AREA (93:5) TO TKORIN-IO
               MOVE VARETIN-IO-AREA (98:3) TO TBEH13-IO
               MOVE VARETIN-IO-AREA (101:3) TO TBEH93-IO
               MOVE VARETIN-IO-AREA (104:3) TO TBEH15-IO
               MOVE VARETIN-IO-AREA (107:3) TO TBEH17-IO
               MOVE VARETIN-IO-AREA (110:3) TO TBEH92-IO
               MOVE VARETIN-IO-AREA (113:3) TO TBEH18-IO
               MOVE VARETIN-IO-AREA (116:3) TO TKOR13-IO
               MOVE VARETIN-IO-AREA (119:3) TO TKOR93-IO
               MOVE VARETIN-IO-AREA (122:3) TO TKOR15-IO
               MOVE VARETIN-IO-AREA (125:3) TO TKOR17-IO
               MOVE VARETIN-IO-AREA (128:3) TO TKOR92-IO
               MOVE VARETIN-IO-AREA (131:3) TO TKOR18-IO
           END-EVALUATE.
 
       VARETIN-IDCHK SECTION.
       VARETIN-IDCHK-P.
           EVALUATE TRUE
           WHEN ( VARETIN-IO-AREA (1:1) = '8' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       VARETIN-IDSET SECTION.
       VARETIN-IDSET-P.
           EVALUATE TRUE
           WHEN ( VARETIN-IO-AREA (1:1) = '8' )
               SET I-01                    TO TRUE
           END-EVALUATE.
 
       VARETIN-CHK-LEVEL SECTION.
       VARETIN-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( VARETIN-IO-AREA (1:1) = '8' )
               MOVE LOW-VALUES             TO VARETIN-LEVEL-01
               MOVE VARETIN-IO-AREA (3:10) TO VARETIN-01-L1-FEKEY
               IF  VARETIN-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VARETIN-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  VARETIN-01-L1         TO THE-PRIOR-L1
               SET VARETIN-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       VARETIN-MATCH-SET SECTION.
       VARETIN-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( VARETIN-IO-AREA (1:1) = '8' )
               MOVE VARETIN-IO-AREA (3:10) TO VARETIN-M-01-M1-FEKEY
           END-EVALUATE.
 
       VAREMTL-GET SECTION.
       VAREMTL-GET-P.
           IF  VAREMTL-EOF-OFF
               READ VAREMTL
               AT END
                   SET VAREMTL-EOF         TO TRUE
               END-READ
           END-IF.
 
       VAREMTL-FLDSET SECTION.
       VAREMTL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMTL-IO-AREA (3:10) TO VMKEY (1:10)
               MOVE VAREMTL-IO-AREA (97:5) TO MANTI-IO
               MOVE VAREMTL-IO-AREA (102:5) TO MANTU-IO
               MOVE VAREMTL-IO-AREA (179:3) TO LAG13-IO
               MOVE VAREMTL-IO-AREA (182:3) TO LAG93-IO
               MOVE VAREMTL-IO-AREA (185:3) TO LAG15-IO
               MOVE VAREMTL-IO-AREA (188:3) TO LAG17-IO
               MOVE VAREMTL-IO-AREA (191:3) TO LAG92-IO
               MOVE VAREMTL-IO-AREA (194:3) TO LAG18-IO
           END-EVALUATE.
 
       VAREMTL-IDSET SECTION.
       VAREMTL-IDSET-P.
           SET I-02                        TO TRUE.
 
       VAREMTL-CHK-LEVEL SECTION.
       VAREMTL-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO VAREMTL-LEVEL-02
               MOVE VAREMTL-IO-AREA (3:10) TO VAREMTL-02-L1-VMKEY
               IF  VAREMTL-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VAREMTL-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  VAREMTL-02-L1         TO THE-PRIOR-L1
               SET VAREMTL-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       VAREMTL-MATCH-SET SECTION.
       VAREMTL-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMTL-IO-AREA (3:10) TO VAREMTL-M-02-M1-VMKEY
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
           IF  VARETIN-EOF
               MOVE HIGH-VALUES            TO VARETIN-MC
                                              VARETIN-MP
           END-IF
           IF  VAREMTL-EOF
               MOVE HIGH-VALUES            TO VAREMTL-MC
                                              VAREMTL-MP
           END-IF
           IF  VARETIN-MC < VARETIN-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  VAREMTL-MC < VAREMTL-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  VARETIN-MC < VAREMTL-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VARETIN-PROCESS     TO TRUE
                   MOVE VARETIN-MC         TO VARETIN-MP
                   IF  VARETIN-MC = VAREMTL-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  VAREMTL-MC < VARETIN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VAREMTL-PROCESS     TO TRUE
                   MOVE VAREMTL-MC         TO VAREMTL-MP
                   IF  VAREMTL-MC = VARETIN-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  VARETIN-MC = VAREMTL-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VARETIN-PROCESS     TO TRUE
                   MOVE VARETIN-MC         TO VARETIN-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND I-20)
               MOVE MANTI                  TO XO-72P
               MOVE XO-72P-EF              TO VAREMTL-IO-AREA (97:5)
               MOVE MANTU                  TO XO-72P
               MOVE XO-72P-EF              TO VAREMTL-IO-AREA (102:5)
               MOVE LAG13                  TO XO-50P
               MOVE XO-50P-EF              TO VAREMTL-IO-AREA (179:3)
               MOVE LAG93                  TO XO-50P
               MOVE XO-50P-EF              TO VAREMTL-IO-AREA (182:3)
               MOVE LAG15                  TO XO-50P
               MOVE XO-50P-EF              TO VAREMTL-IO-AREA (185:3)
               MOVE LAG17                  TO XO-50P
               MOVE XO-50P-EF              TO VAREMTL-IO-AREA (188:3)
               MOVE LAG92                  TO XO-50P
               MOVE XO-50P-EF              TO VAREMTL-IO-AREA (191:3)
               MOVE LAG18                  TO XO-50P
               MOVE XO-50P-EF              TO VAREMTL-IO-AREA (194:3)
               REWRITE VAREMTL-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTI                   TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (12:9)
               MOVE 'ANT. RECORDS LEST.'   TO LISTE-IO-AREA (23:18)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTK                   TO XO-70YY9
               MOVE XO-70YY9               TO LISTE-IO-AREA (12:9)
               MOVE 'ANT. RECORDS KORR.'   TO LISTE-IO-AREA (23:18)
               IF  (I-U1)
                   MOVE '** TEST U/KORR. **' TO LISTE-IO-AREA (53:18)
               END-IF
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
           MOVE 1                          TO LR-CHECK
           SET VARETIN-LEVEL-INIT          TO TRUE
           INITIALIZE VARETIN-DATA-FIELDS
           SET VARETIN-EOF-OFF             TO TRUE
           SET VARETIN-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO VARETIN-MC
                                              VARETIN-MP
           OPEN INPUT VARETIN
           SET VAREMTL-LEVEL-INIT          TO TRUE
           INITIALIZE VAREMTL-DATA-FIELDS
           SET VAREMTL-EOF-OFF             TO TRUE
           SET VAREMTL-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO VAREMTL-MC
                                              VAREMTL-MP
           OPEN I-O VAREMTL
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE VARETIN
           CLOSE VAREMTL
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
