       IDENTIFICATION DIVISION.
       PROGRAM-ID. OPP099R.
      **********************************************  Z-WIN-RPG2      *
      *****************************************************************
      * PROGRAM.....OPP099                                            *
      * PROG........ESPEN LARSEN                                      *
      * PROG.DATO...14.09.1996                                        *
      * MERGER ALT.OPPSKEY MOT VAREMASTER OG LEGGER UT KUN DE SOM     *
      *        FINNES I VARE.MASTER.                                  *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: OPP099.rpg
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
           SELECT VAREMAS
               ASSIGN TO VAREMAS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VAREMAS-STATUS.
           SELECT ALTREC
               ASSIGN TO UT-S-ALTREC
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ALTREC-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD VAREMAS
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X           PICTURE X(200).
       FD ALTREC
               BLOCK CONTAINS 60
               RECORD CONTAINS 30.
       01  ALTREC-IO-AREA.
           05  ALTREC-IO-AREA-X            PICTURE X(30).
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
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  ALTREC-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREMAS-EOF-OFF         VALUE '0'.
               88  VAREMAS-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREMAS-READ-OFF        VALUE '0'.
               88  VAREMAS-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAREMAS-PROCESS-OFF     VALUE '0'.
               88  VAREMAS-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ALTREC-EOF-OFF          VALUE '0'.
               88  ALTREC-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ALTREC-READ-OFF         VALUE '0'.
               88  ALTREC-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ALTREC-PROCESS-OFF      VALUE '0'.
               88  ALTREC-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  ALTREC-LEVEL-INIT-OFF   VALUE '0'.
               88  ALTREC-LEVEL-INIT       VALUE '1'.
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
           05  VAREMAS-DATA-FIELDS.
               10  VFNR                    PICTURE X(3).
               10  VEDBNR                  PICTURE X(7).
           05  VAREMAS-MP                  PICTURE X(10).
           05  VAREMAS-MC                  PICTURE X(10).
           05  VAREMAS-M-01            REDEFINES VAREMAS-MC.
               10  VAREMAS-M-01-M2.
                   15  VAREMAS-M-01-M2-VFNR-G.
                       20  VAREMAS-M-01-M2-VFNR PICTURE X(3).
               10  VAREMAS-M-01-M1.
                   15  VAREMAS-M-01-M1-VEDBNR-G.
                       20  VAREMAS-M-01-M1-VEDBNR PICTURE X(7).
           05  ALTREC-LEVEL-02.
               10  ALTREC-02-L1.
                   15  ALTREC-02-L1-FNR    PICTURE X(3).
           05  ALTREC-DATA-FIELDS.
               10  FNR                     PICTURE X(3).
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(14).
               10  EDBNR                   PICTURE X(7).
           05  ALTREC-MP                   PICTURE X(10).
           05  ALTREC-MC                   PICTURE X(10).
           05  ALTREC-M-02             REDEFINES ALTREC-MC.
               10  ALTREC-M-02-M2.
                   15  ALTREC-M-02-M2-FNR-G.
                       20  ALTREC-M-02-M2-FNR PICTURE X(3).
               10  ALTREC-M-02-M1.
                   15  ALTREC-M-02-M1-EDBNR-G.
                       20  ALTREC-M-02-M1-EDBNR PICTURE X(7).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  ANTALT-IO.
                   15  ANTALT              PICTURE S9(8).
               10  ANTNMR-IO.
                   15  ANTNMR              PICTURE S9(8).
               10  ANTALR-IO.
                   15  ANTALR              PICTURE S9(8).
               10  ANTNLR-IO.
                   15  ANTNLR              PICTURE S9(8).
           05  EDITTING-FIELDS.
               10  XO-40YNZ                PICTURE ZZZZ.
               10  XO-80YY9                PICTURE ZZ.ZZZ.ZZ9.
           05  PREDEFINED-FIELDS.
               10  PAGE0                   PICTURE S9(4) USAGE BINARY.
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
           SET NOT-I-02                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
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
 
           IF  ALTREC-PROCESS
               SET ALTREC-PROCESS-OFF      TO TRUE
               SET ALTREC-READ             TO TRUE
           END-IF
 
           IF  ALTREC-READ
               PERFORM ALTREC-GET
               SET ALTREC-READ-OFF         TO TRUE
               IF  NOT ALTREC-EOF
                   PERFORM ALTREC-MATCH-SET
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
 
           IF  VAREMAS-PROCESS
               PERFORM VAREMAS-IDSET
           END-IF
 
           IF  ALTREC-PROCESS
               PERFORM ALTREC-IDSET
           END-IF
 
           IF  ALTREC-PROCESS
               PERFORM ALTREC-CHK-LEVEL
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
           PERFORM HEADING-OVERFLOW
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  VAREMAS-PROCESS
               PERFORM VAREMAS-FLDSET
           END-IF
 
           IF  ALTREC-PROCESS
               PERFORM ALTREC-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  ALTREC-PROCESS
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
           END-IF
           IF  (I-02)
               ADD 1                       TO ANTALT
           END-IF
           IF  (I-02 AND NOT-I-MR)
               ADD 1                       TO ANTNMR
           END-IF
           IF  (I-02)
               ADD 1                       TO ANTALR
           END-IF
           IF  (I-02 AND NOT-I-MR)
               ADD 1                       TO ANTNLR
           END-IF.
 
       SLUTT-T.
      ******************************************************
           CONTINUE.
 
       VAREMAS-GET SECTION.
       VAREMAS-GET-P.
           IF  VAREMAS-EOF-OFF
               READ VAREMAS
               AT END
                   SET VAREMAS-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (3:3)  TO VFNR (1:3)
               MOVE VAREMAS-IO-AREA (6:7)  TO VEDBNR (1:7)
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-01                        TO TRUE.
 
       VAREMAS-MATCH-SET SECTION.
       VAREMAS-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (3:3)  TO VAREMAS-M-01-M2-VFNR
               MOVE VAREMAS-IO-AREA (6:7)  TO VAREMAS-M-01-M1-VEDBNR
           END-EVALUATE.
 
       ALTREC-GET SECTION.
       ALTREC-GET-P.
           IF  ALTREC-EOF-OFF
               READ ALTREC
               AT END
                   SET ALTREC-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       ALTREC-FLDSET SECTION.
       ALTREC-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE ALTREC-IO-AREA (2:3)   TO FNR (1:3)
               MOVE ALTREC-IO-AREA (5:3)   TO ALFA (1:3)
               MOVE ALTREC-IO-AREA (9:14)  TO ARTNR (1:14)
               MOVE ALTREC-IO-AREA (23:7)  TO EDBNR (1:7)
           END-EVALUATE.
 
       ALTREC-IDSET SECTION.
       ALTREC-IDSET-P.
           SET I-02                        TO TRUE.
 
       ALTREC-CHK-LEVEL SECTION.
       ALTREC-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO ALTREC-LEVEL-02
               MOVE ALTREC-IO-AREA (2:3)   TO ALTREC-02-L1-FNR
               IF  ALTREC-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ALTREC-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ALTREC-02-L1          TO THE-PRIOR-L1
               SET ALTREC-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       ALTREC-MATCH-SET SECTION.
       ALTREC-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE ALTREC-IO-AREA (2:3)   TO ALTREC-M-02-M2-FNR
               MOVE ALTREC-IO-AREA (23:7)  TO ALTREC-M-02-M1-EDBNR
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
           IF  VAREMAS-EOF
               MOVE HIGH-VALUES            TO VAREMAS-MC
                                              VAREMAS-MP
           END-IF
           IF  ALTREC-EOF
               MOVE HIGH-VALUES            TO ALTREC-MC
                                              ALTREC-MP
           END-IF
           IF  VAREMAS-MC < VAREMAS-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  ALTREC-MC < ALTREC-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  VAREMAS-MC < ALTREC-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VAREMAS-PROCESS     TO TRUE
                   MOVE VAREMAS-MC         TO VAREMAS-MP
                   IF  VAREMAS-MC = ALTREC-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  ALTREC-MC < VAREMAS-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET ALTREC-PROCESS      TO TRUE
                   MOVE ALTREC-MC          TO ALTREC-MP
                   IF  ALTREC-MC = VAREMAS-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  VAREMAS-MC = ALTREC-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VAREMAS-PROCESS     TO TRUE
                   MOVE VAREMAS-MC         TO VAREMAS-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND NOT-I-MR)
               MOVE 'S'                    TO ALTREC-IO-AREA (30:1)
               REWRITE ALTREC-IO-AREA
           END-IF
           IF  (I-02 AND NOT-I-MR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ALFA                   TO LISTE-IO-AREA (1:3)
               MOVE ARTNR                  TO LISTE-IO-AREA (6:14)
               MOVE EDBNR                  TO LISTE-IO-AREA (27:7)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FNR                    TO LISTE-IO-AREA (28:3)
               MOVE 'ALTERNATIVE OPPSLAGSNR ' TO LISTE-IO-AREA (40:23)
               MOVE 'SOM IKKE ER I VAREMASTER' TO LISTE-IO-AREA (63:24)
               MOVE 'DATO'                 TO LISTE-IO-AREA (90:4)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (95:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (105:4)
               IF  (I-L1)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (109:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ALFA ARTIKKELNUMMER    ' TO LISTE-IO-AREA (1:23)
               MOVE 'EDBNUMMER      '      TO LISTE-IO-AREA (27:15)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF AND NOT-I-L1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FNR                    TO LISTE-IO-AREA (28:3)
               MOVE 'ALTERNATIVE OPPSLAGSNR ' TO LISTE-IO-AREA (40:23)
               MOVE 'SOM IKKE ER I VAREMASTER' TO LISTE-IO-AREA (63:24)
               MOVE 'DATO'                 TO LISTE-IO-AREA (90:4)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (95:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (105:4)
               IF  (I-L1)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (109:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ALFA ARTIKKELNUMMER    ' TO LISTE-IO-AREA (1:23)
               MOVE 'EDBNUMMER      '      TO LISTE-IO-AREA (27:15)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT. ALT. OPPS.REC.'  TO LISTE-IO-AREA (2:19)
               MOVE ANTALT                 TO XO-80YY9
               MOVE XO-80YY9               TO LISTE-IO-AREA (21:10)
               INITIALIZE ANTALT
               MOVE 'ANT. FJERNET NÅ'      TO LISTE-IO-AREA (36:15)
               MOVE ANTNMR                 TO XO-80YY9
               MOVE XO-80YY9               TO LISTE-IO-AREA (51:10)
               INITIALIZE ANTNMR
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT. ALT. REC.TOT. '  TO LISTE-IO-AREA (2:19)
               MOVE ANTALR                 TO XO-80YY9
               MOVE XO-80YY9               TO LISTE-IO-AREA (21:10)
               INITIALIZE ANTALR
               MOVE 'ANT. FJERNET TOT.'    TO LISTE-IO-AREA (34:17)
               MOVE ANTNLR                 TO XO-80YY9
               MOVE XO-80YY9               TO LISTE-IO-AREA (51:10)
               INITIALIZE ANTNLR
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
           INITIALIZE VAREMAS-DATA-FIELDS
           SET VAREMAS-EOF-OFF             TO TRUE
           SET VAREMAS-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO VAREMAS-MC
                                              VAREMAS-MP
           OPEN INPUT VAREMAS
           SET ALTREC-LEVEL-INIT           TO TRUE
           INITIALIZE ALTREC-DATA-FIELDS
           SET ALTREC-EOF-OFF              TO TRUE
           SET ALTREC-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO ALTREC-MC
                                              ALTREC-MP
           OPEN I-O ALTREC
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE VAREMAS
           CLOSE ALTREC
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
