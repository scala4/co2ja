       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK051R.
      **********************************************  Z-WIN-RPG2   ****
      * KOPI AV FAK041
      * ENDRING: FJERNET TEST P� FIRMANR.LIGGER I LIMIT.
      *
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK051.rpg
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
           SELECT FAKLIM
               ASSIGN TO FAKLIM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKLIM-STATUS.
           SELECT PAR
               ASSIGN TO UT-S-PAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PAR-STATUS.
           SELECT FAKSALG
               ASSIGN TO FAKSALG
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FAKSALG-STATUS
               RECORD KEY IS FAKSALG-KEY1.
           SELECT OUTFIL
               ASSIGN TO UT-S-OUTFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTFIL-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD FAKLIM
               RECORD CONTAINS 80.
       01  FAKLIM-IO-AREA.
           05  FAKLIM-IO-AREA-X.
               10  FAKLIM-KEY1             PICTURE X(20).
               10  FILLER                  PICTURE X(60).
       FD PAR
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  PAR-IO-AREA.
           05  PAR-IO-AREA-X               PICTURE X(80).
       FD FAKSALG
               RECORD CONTAINS 160.
       01  FAKSALG-IO-AREA.
           05  FAKSALG-IO-AREA-X.
               10  FAKSALG-KEY1            PICTURE X(20).
               10  FILLER                  PICTURE X(140).
       FD OUTFIL
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  OUTFIL-IO-AREA.
           05  OUTFIL-IO-AREA-X            PICTURE X(200).
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
           10  FAKLIM-STATUS               PICTURE 99 VALUE 0.
           10  PAR-STATUS                  PICTURE 99 VALUE 0.
           10  FAKSALG-STATUS              PICTURE 99 VALUE 0.
           10  OUTFIL-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FAKLIM-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKLIM-EOF-OFF          VALUE '0'.
               88  FAKLIM-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKLIM-READ-OFF         VALUE '0'.
               88  FAKLIM-READ             VALUE '1'.
           05  FAKLIM-LOW-KEY              PICTURE X(20).
           05  FAKLIM-HIGH-KEY             PICTURE X(20).
           05  FILLER                      PIC X VALUE '0'.
               88  PAR-EOF-OFF             VALUE '0'.
               88  PAR-EOF                 VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PAR-READ-OFF            VALUE '0'.
               88  PAR-READ                VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PAR-PROCESS-OFF         VALUE '0'.
               88  PAR-PROCESS             VALUE '1'.
           05  FAKSALG-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKSALG-EOF-OFF         VALUE '0'.
               88  FAKSALG-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKSALG-READ-OFF        VALUE '0'.
               88  FAKSALG-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKSALG-PROCESS-OFF     VALUE '0'.
               88  FAKSALG-PROCESS         VALUE '1'.
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
           05  PAR-DATA-FIELDS.
               10  FRAMND                  PICTURE X(6).
               10  TILMND                  PICTURE X(6).
           05  FAKSALG-DATA-FIELDS.
               10  KONS                    PICTURE X(3).
               10  FAKMND                  PICTURE X(6).
               10  KUNDNR                  PICTURE X(6).
               10  SEQNR                   PICTURE X(5).
               10  ORDDTO                  PICTURE X(8).
               10  AA                      PICTURE X(2).
               10  MM                      PICTURE X(2).
               10  DD                      PICTURE X(2).
               10  TTMM                    PICTURE X(4).
               10  ORDNR                   PICTURE X(6).
               10  FIRMA                   PICTURE X(3).
               10  BM                      PICTURE X(2).
               10  LK                      PICTURE X(2).
               10  AVD                     PICTURE X(1).
               10  FK                      PICTURE X(1).
               10  KRTYPE                  PICTURE X(1).
               10  VGR                     PICTURE X(5).
               10  EDBNR                   PICTURE X(7).
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  ANTLEV-IO.
                   15  ANTLEV              PICTURE S9(5)V9(2).
               10  OPRIS-IO.
                   15  OPRIS               PICTURE S9(7)V9(2).
               10  ORAB1-IO.
                   15  ORAB1               PICTURE S9(12)V9(1).
               10  ORAB2-IO.
                   15  ORAB2               PICTURE S9(12)V9(1).
               10  ORAB3-IO.
                   15  ORAB3               PICTURE S9(2)V9(1).
               10  NTOSUM-IO.
                   15  NTOSUM              PICTURE S9(7)V9(2).
               10  SELVK-IO.
                   15  SELVK               PICTURE S9(7)V9(2).
               10  FAKTNR                  PICTURE X(6).
               10  ORDTYP                  PICTURE X(1).
               10  FAKOMG                  PICTURE X(1).
               10  ORDMOT                  PICTURE X(2).
           05  TEMPORARY-FIELDS.
               10  ANT-IO.
                   15  ANT                 PICTURE S9(8).
               10  SELVK1-IO.
                   15  SELVK1              PICTURE S9(7)V9(2).
               10  OPRIMM-IO.
                   15  OPRIMM              PICTURE S9(7)V9(2).
               10  ANTF-IO.
                   15  ANTF                PICTURE S9(8).
           05  EDITTING-FIELDS.
               10  XO-80YY9                PICTURE ZZ.ZZZ.ZZ9.
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
           SET NOT-I-02                    TO TRUE
           SET NOT-I-01                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  PAR-PROCESS
               SET PAR-PROCESS-OFF         TO TRUE
               SET PAR-READ                TO TRUE
           END-IF
 
           IF  PAR-READ
           AND RECORD-SELECTED-OFF
               PERFORM PAR-GET
               SET PAR-READ-OFF            TO TRUE
               IF  NOT PAR-EOF
                   PERFORM PAR-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET PAR-PROCESS         TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  FAKSALG-PROCESS
               SET FAKSALG-PROCESS-OFF     TO TRUE
               SET FAKSALG-READ            TO TRUE
           END-IF
 
           IF  FAKSALG-READ
           AND RECORD-SELECTED-OFF
               PERFORM FAKSALG-GET
               SET FAKSALG-READ-OFF        TO TRUE
               IF  NOT FAKSALG-EOF
                   SET FAKSALG-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  PAR-PROCESS
               PERFORM PAR-IDSET
           END-IF
 
           IF  FAKSALG-PROCESS
               PERFORM FAKSALG-IDSET
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
 
           IF  PAR-PROCESS
               PERFORM PAR-FLDSET
           END-IF
 
           IF  FAKSALG-PROCESS
               PERFORM FAKSALG-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           SET NOT-I-1ST                   TO TRUE
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-01)
               SET NOT-I-50                TO TRUE
      *****************************************************************
      * DIV SJEKKER                                                   *
      *****************************************************************
           END-IF
           ADD 1                           TO ANT
           SET NOT-I-11                    TO TRUE
           IF  FAKMND < FRAMND
               SET I-11                    TO TRUE
           END-IF
           IF  (I-11)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-12                    TO TRUE
           IF  FAKMND > TILMND
               SET I-12                    TO TRUE
           END-IF
           IF  (I-12)
               GO TO SLUTT-T
      *****************************************************************
           END-IF
           SET NOT-I-21                    TO TRUE
           IF  ANTLEV = 0
               SET I-21                    TO TRUE
           END-IF
           IF  (I-21)
               ADD SELVK TO ZERO       GIVING SELVK1
           END-IF
           IF  (NOT-I-21)
               DIVIDE SELVK BY ANTLEV  GIVING SELVK1
           END-IF
           MULTIPLY 1,25 BY OPRIS      GIVING OPRIMM
           SET NOT-I-63                    TO TRUE
           IF  NTOSUM < 0,00
               SET I-63                    TO TRUE
           END-IF
           IF  (I-01)
               SET I-50                    TO TRUE
           END-IF
           IF  (I-50)
               ADD 1                       TO ANTF
           END-IF.
 
       SLUTT-T.
           CONTINUE.
 
       PAR-GET SECTION.
       PAR-GET-P.
           IF  PAR-EOF-OFF
               READ PAR
               AT END
                   SET PAR-EOF             TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       PAR-FLDSET SECTION.
       PAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ( PAR-IO-AREA (1:1) = 'P' )
               MOVE PAR-IO-AREA (22:6)     TO FRAMND (1:6)
               MOVE PAR-IO-AREA (44:6)     TO TILMND (1:6)
           END-EVALUATE.
 
       PAR-IDCHK SECTION.
       PAR-IDCHK-P.
           EVALUATE TRUE
           WHEN ( PAR-IO-AREA (1:1) = 'P' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       PAR-IDSET SECTION.
       PAR-IDSET-P.
           EVALUATE TRUE
           WHEN ( PAR-IO-AREA (1:1) = 'P' )
               SET I-02                    TO TRUE
           END-EVALUATE.
 
       FAKSALG-GET SECTION.
       FAKSALG-GET-P.
           IF  FAKSALG-EOF-OFF
               PERFORM WITH TEST AFTER
                 UNTIL FAKLIM-READ-OFF
                    OR FAKLIM-EOF
                   IF  FAKLIM-READ
                       SET FAKLIM-READ-OFF TO TRUE
                       READ FAKLIM
                       AT END
                           SET FAKLIM-EOF  TO TRUE
                           SET FAKSALG-EOF TO TRUE
                           SUBTRACT 1    FROM LR-CHECK
                       NOT AT END
                           MOVE FAKLIM-IO-AREA (1:4) TO FAKSALG-KEY1
                       END-READ
                   END-IF
                   IF  FAKLIM-EOF-OFF
                   AND FAKLIM-READ-OFF
                       READ FAKSALG
                       INVALID KEY
                           SET I-H0        TO TRUE
                           MOVE 'N'        TO E-R-R-O-R
                       END-READ
                   END-IF
               END-PERFORM
           END-IF.
 
       FAKSALG-FLDSET SECTION.
       FAKSALG-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKSALG-IO-AREA (1:3)  TO KONS (1:3)
               MOVE FAKSALG-IO-AREA (4:6)  TO FAKMND (1:6)
               MOVE FAKSALG-IO-AREA (10:6) TO KUNDNR (1:6)
               MOVE FAKSALG-IO-AREA (16:5) TO SEQNR (1:5)
               MOVE FAKSALG-IO-AREA (21:8) TO ORDDTO (1:8)
               MOVE FAKSALG-IO-AREA (23:2) TO AA (1:2)
               MOVE FAKSALG-IO-AREA (25:2) TO MM (1:2)
               MOVE FAKSALG-IO-AREA (27:2) TO DD (1:2)
               MOVE FAKSALG-IO-AREA (29:4) TO TTMM (1:4)
               MOVE FAKSALG-IO-AREA (33:6) TO ORDNR (1:6)
               MOVE FAKSALG-IO-AREA (39:3) TO FIRMA (1:3)
               MOVE FAKSALG-IO-AREA (42:2) TO BM (1:2)
               MOVE FAKSALG-IO-AREA (44:2) TO LK (1:2)
               MOVE FAKSALG-IO-AREA (46:1) TO AVD (1:1)
               MOVE FAKSALG-IO-AREA (47:1) TO FK (1:1)
               MOVE FAKSALG-IO-AREA (48:1) TO KRTYPE (1:1)
               MOVE FAKSALG-IO-AREA (51:5) TO VGR (1:5)
               MOVE FAKSALG-IO-AREA (60:7) TO EDBNR (1:7)
               MOVE FAKSALG-IO-AREA (67:3) TO ALFA (1:3)
               MOVE FAKSALG-IO-AREA (70:20) TO ARTNR (1:20)
               MOVE FAKSALG-IO-AREA (97:7) TO ANTLEV-IO
               INSPECT ANTLEV-IO REPLACING ALL ' ' BY '0'
               MOVE FAKSALG-IO-AREA (104:9) TO OPRIS-IO
               INSPECT OPRIS-IO REPLACING ALL ' ' BY '0'
               MOVE FAKSALG-IO-AREA (103:13) TO ORAB1-IO
               INSPECT ORAB1-IO REPLACING ALL ' ' BY '0'
               MOVE FAKSALG-IO-AREA (106:13) TO ORAB2-IO
               INSPECT ORAB2-IO REPLACING ALL ' ' BY '0'
               MOVE FAKSALG-IO-AREA (119:3) TO ORAB3-IO
               INSPECT ORAB3-IO REPLACING ALL ' ' BY '0'
               MOVE FAKSALG-IO-AREA (122:9) TO NTOSUM-IO
               INSPECT NTOSUM-IO REPLACING ALL ' ' BY '0'
               MOVE FAKSALG-IO-AREA (131:9) TO SELVK-IO
               INSPECT SELVK-IO REPLACING ALL ' ' BY '0'
               MOVE FAKSALG-IO-AREA (140:6) TO FAKTNR (1:6)
               MOVE FAKSALG-IO-AREA (146:1) TO ORDTYP (1:1)
               MOVE FAKSALG-IO-AREA (147:1) TO FAKOMG (1:1)
               MOVE FAKSALG-IO-AREA (148:2) TO ORDMOT (1:2)
           END-EVALUATE.
 
       FAKSALG-IDSET SECTION.
       FAKSALG-IDSET-P.
           SET I-01                        TO TRUE.
 
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
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-50)
               MOVE SPACES TO OUTFIL-IO-AREA
               INITIALIZE OUTFIL-IO-AREA
               MOVE FIRMA                  TO OUTFIL-IO-AREA (1:3)
               MOVE ALFA                   TO OUTFIL-IO-AREA (4:3)
               MOVE ARTNR                  TO OUTFIL-IO-AREA (7:20)
               MOVE ANTLEV-IO              TO OUTFIL-IO-AREA (57:7)
               MOVE OPRIS-IO               TO OUTFIL-IO-AREA (64:9)
               MOVE ORDNR                  TO OUTFIL-IO-AREA (73:6)
               MOVE VGR                    TO OUTFIL-IO-AREA (79:5)
               MOVE SELVK1-IO              TO OUTFIL-IO-AREA (84:9)
               MOVE OPRIMM-IO              TO OUTFIL-IO-AREA (93:9)
               MOVE AA                     TO OUTFIL-IO-AREA (102:2)
               MOVE MM                     TO OUTFIL-IO-AREA (104:2)
               MOVE DD                     TO OUTFIL-IO-AREA (106:2)
               MOVE KUNDNR                 TO OUTFIL-IO-AREA (108:6)
               MOVE ORDMOT                 TO OUTFIL-IO-AREA (114:2)
               MOVE EDBNR                  TO OUTFIL-IO-AREA (116:7)
               MOVE BM                     TO OUTFIL-IO-AREA (123:2)
               MOVE AVD                    TO OUTFIL-IO-AREA (125:1)
               MOVE FAKOMG                 TO OUTFIL-IO-AREA (126:1)
               MOVE ORAB1-IO               TO OUTFIL-IO-AREA (131:13)
               MOVE ORAB2-IO               TO OUTFIL-IO-AREA (134:13)
               MOVE ORAB3-IO               TO OUTFIL-IO-AREA (147:3)
               MOVE FAKTNR                 TO OUTFIL-IO-AREA (151:6)
               MOVE FK                     TO OUTFIL-IO-AREA (157:1)
               MOVE KRTYPE                 TO OUTFIL-IO-AREA (158:1)
               MOVE NTOSUM-IO              TO OUTFIL-IO-AREA (159:9)
               MOVE LK                     TO OUTFIL-IO-AREA (168:2)
               MOVE TTMM                   TO OUTFIL-IO-AREA (170:4)
               MOVE 'R'                    TO OUTFIL-IO-AREA (174:1)
               IF  (I-63)
                   MOVE '-'                TO OUTFIL-IO-AREA (175:1)
               END-IF
               MOVE KONS                   TO OUTFIL-IO-AREA (176:3)
               WRITE OUTFIL-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANT                    TO XO-80YY9
               MOVE XO-80YY9               TO LISTE-IO-AREA (11:10)
               MOVE 'RECORDS LEST.      '  TO LISTE-IO-AREA (24:19)
               MOVE 2                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTF                   TO XO-80YY9
               MOVE XO-80YY9               TO LISTE-IO-AREA (11:10)
               IF  (NOT-I-U1)
                   MOVE 'RECORDS KORRIGERT  ' TO LISTE-IO-AREA (24:19)
               END-IF
               IF  (I-U1)
                   MOVE 'RECORDS SELEKTERT  ' TO LISTE-IO-AREA (24:19)
               END-IF
               MOVE 2                      TO LISTE-BEFORE-SPACE
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
           SET FAKLIM-EOF-OFF              TO TRUE
           SET FAKLIM-READ                 TO TRUE
           OPEN INPUT FAKLIM
           INITIALIZE PAR-DATA-FIELDS
           SET PAR-EOF-OFF                 TO TRUE
           SET PAR-PROCESS                 TO TRUE
           OPEN INPUT PAR
           INITIALIZE FAKSALG-DATA-FIELDS
           SET FAKSALG-EOF-OFF             TO TRUE
           SET FAKSALG-PROCESS             TO TRUE
           OPEN INPUT FAKSALG
           OPEN OUTPUT OUTFIL
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FAKLIM
           CLOSE PAR
           CLOSE FAKSALG
           CLOSE OUTFIL
           IF LISTE-IO-AREA NOT = SPACES
             WRITE LISTE-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE-IO-AREA
           END-IF
           CLOSE LISTE.
 
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
