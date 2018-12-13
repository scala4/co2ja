       IDENTIFICATION DIVISION.
       PROGRAM-ID. BST115R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAMMET DANNER EN RECORD PR BESTILLER DERSOM FIRMA       *
      *  FINNES I TABELL, HVIS IKKE FIRMA I TABELL DANNES EN TOTAL   *
      *  LISTE TIL "BESTILLER"                                       *
      *  DERSOM UPSI-1  PRINTES ALLE SOM IKKE FINNES I STYFILE       *
      *                 PÅ EGEN LISTE  "BESTILLER"                   *
      ****************************************************************
      *                                          XX2000XXIRXXSS
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: BST115.rpg
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
           SELECT STYFILE
               ASSIGN TO STYFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS STYFILE-STATUS
               RECORD KEY IS STYFILE-KEY1.
           SELECT BEST
               ASSIGN TO UT-S-BEST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS BEST-STATUS.
           SELECT FNRTAB
               ASSIGN TO UT-S-FNRTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FNRTAB-STATUS.
           SELECT BESTUT
               ASSIGN TO UT-S-BESTUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS BESTUT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD STYFILE
               RECORD CONTAINS 80.
       01  STYFILE-IO-AREA.
           05  STYFILE-IO-AREA-X.
               10  STYFILE-KEY1            PICTURE X(11).
               10  FILLER                  PICTURE X(69).
       FD BEST
               BLOCK CONTAINS 4096
               RECORD CONTAINS 128.
       01  BEST-IO-AREA.
           05  BEST-IO-AREA-X              PICTURE X(128).
       FD FNRTAB
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  FNRTAB-IO-AREA.
           05  FNRTAB-IO-AREA-X            PICTURE X(80).
       FD BESTUT
               BLOCK CONTAINS 4080
               RECORD CONTAINS 136.
       01  BESTUT-IO-AREA.
           05  BESTUT-IO-AREA-X            PICTURE X(136).
       WORKING-STORAGE SECTION.
       77  TABFNR-MAX   VALUE 50           PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  TABFNR-TABLE.
               10  TABFNR-ENTRY
                                           OCCURS 50 TIMES
                                           INDEXED BY TABFNR-I
                                                      TABFNR-S.
                   15  TABFNR              PICTURE X(3).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  STYFILE-STATUS              PICTURE 99 VALUE 0.
           10  BEST-STATUS                 PICTURE 99 VALUE 0.
           10  FNRTAB-STATUS               PICTURE 99 VALUE 0.
           10  BESTUT-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  STYFILE-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  STYFILE-EOF-OFF         VALUE '0'.
               88  STYFILE-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  STYFILE-READ-OFF        VALUE '0'.
               88  STYFILE-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  STYFILE-PROCESS-OFF     VALUE '0'.
               88  STYFILE-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  STYFILE-LEVEL-INIT-OFF  VALUE '0'.
               88  STYFILE-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BEST-EOF-OFF            VALUE '0'.
               88  BEST-EOF                VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BEST-READ-OFF           VALUE '0'.
               88  BEST-READ               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BEST-PROCESS-OFF        VALUE '0'.
               88  BEST-PROCESS            VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  BEST-LEVEL-INIT-OFF     VALUE '0'.
               88  BEST-LEVEL-INIT         VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FNRTAB-EOF-OFF          VALUE '0'.
               88  FNRTAB-EOF              VALUE '1'.
           05  STYFILE-LEVEL-01.
               10  STYFILE-01-L1.
                   15  STYFILE-01-L1-FIRMA PICTURE X(3).
           05  STYFILE-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  VGR                     PICTURE X(5).
               10  ALFA                    PICTURE X(3).
               10  B1                      PICTURE X(9).
               10  B2                      PICTURE X(9).
               10  B3                      PICTURE X(9).
               10  B4                      PICTURE X(9).
           05  STYFILE-MP                  PICTURE X(11).
           05  STYFILE-MC                  PICTURE X(11).
           05  STYFILE-M-01            REDEFINES STYFILE-MC.
               10  STYFILE-M-01-M3.
                   15  STYFILE-M-01-M3-FIRMA-G.
                       20  STYFILE-M-01-M3-FIRMA PICTURE X(3).
               10  STYFILE-M-01-M2.
                   15  STYFILE-M-01-M2-VGR-G.
                       20  STYFILE-M-01-M2-VGR PICTURE X(5).
               10  STYFILE-M-01-M1.
                   15  STYFILE-M-01-M1-ALFA-G.
                       20  STYFILE-M-01-M1-ALFA PICTURE X(3).
           05  BEST-LEVEL-02.
               10  BEST-02-L1.
                   15  BEST-02-L1-FIRMA    PICTURE X(3).
           05  BEST-DATA-FIELDS.
               10  REC                     PICTURE X(128).
           05  BEST-MP                     PICTURE X(11).
           05  BEST-MC                     PICTURE X(11).
           05  BEST-M-02               REDEFINES BEST-MC.
               10  BEST-M-02-M3.
                   15  BEST-M-02-M3-FIRMA-G.
                       20  BEST-M-02-M3-FIRMA PICTURE X(3).
               10  BEST-M-02-M2.
                   15  BEST-M-02-M2-VGR-G.
                       20  BEST-M-02-M2-VGR PICTURE X(5).
               10  BEST-M-02-M1.
                   15  BEST-M-02-M1-ALFA-G.
                       20  BEST-M-02-M1-ALFA PICTURE X(3).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
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
 
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
 
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
           IF  STYFILE-PROCESS
               SET STYFILE-PROCESS-OFF     TO TRUE
               SET STYFILE-READ            TO TRUE
           END-IF
 
           IF  STYFILE-READ
               PERFORM STYFILE-GET
               SET STYFILE-READ-OFF        TO TRUE
               IF  NOT STYFILE-EOF
                   PERFORM STYFILE-MATCH-SET
               END-IF
           END-IF
 
           IF  BEST-PROCESS
               SET BEST-PROCESS-OFF        TO TRUE
               SET BEST-READ               TO TRUE
           END-IF
 
           IF  BEST-READ
               PERFORM BEST-GET
               SET BEST-READ-OFF           TO TRUE
               IF  NOT BEST-EOF
                   PERFORM BEST-MATCH-SET
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
 
           IF  STYFILE-PROCESS
               PERFORM STYFILE-IDSET
           END-IF
 
           IF  BEST-PROCESS
               PERFORM BEST-IDSET
           END-IF
 
           IF  STYFILE-PROCESS
               PERFORM STYFILE-CHK-LEVEL
           END-IF
 
           IF  BEST-PROCESS
               PERFORM BEST-CHK-LEVEL
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
 
           IF  STYFILE-PROCESS
               PERFORM STYFILE-FLDOFF
               PERFORM STYFILE-FLDSET
           END-IF
 
           IF  BEST-PROCESS
               PERFORM BEST-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  STYFILE-PROCESS
           OR  BEST-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-70                    TO TRUE
           SET NOT-I-71                    TO TRUE
           SET NOT-I-72                    TO TRUE
           SET NOT-I-73                    TO TRUE
           SET NOT-I-74                    TO TRUE
      *
      *  TEST OM FIRMA SKAL PRINTES PR BESTILLER.
           IF  (I-L1)
               SET NOT-I-77                TO TRUE
               SET TABFNR-S                TO TABFNR-I
               PERFORM WITH TEST AFTER
                       VARYING TABFNR-I FROM 1 BY 1
                         UNTIL TABFNR-I >= TABFNR-MAX
                            OR I-77
                   IF  FIRMA = TABFNR (TABFNR-I)
                       SET I-77            TO TRUE
                       SET TABFNR-S        TO TABFNR-I
                   END-IF
               END-PERFORM
           END-IF
           IF  (I-02 AND NOT-I-77)
               SET I-70                    TO TRUE
               GO TO TOTLST-T
      *  LISTE PR "BESTILLER" (IKKE MAKKER I STYFILE)
           END-IF
           IF  (I-01)
               GO TO SLUTT-T
           END-IF
           IF  (I-02 AND NOT-I-MR AND I-U1)
               SET I-70                    TO TRUE
           END-IF
           IF  (I-70)
               PERFORM EXCEPTION-OUTPUT
           END-IF
           IF  (I-02 AND NOT-I-MR)
               GO TO SLUTT-T
      *  LISTE PR 1.BESTILLER.
           END-IF
           IF  (NOT-I-81)
               SET I-71                    TO TRUE
           END-IF
           IF  (I-71)
               PERFORM EXCEPTION-OUTPUT
      *  LISTE PR 2.BESTILLER.
           END-IF
           IF  (NOT-I-82)
               SET I-72                    TO TRUE
           END-IF
           IF  (I-72)
               PERFORM EXCEPTION-OUTPUT
      *  LISTE PR 3.BESTILLER.
           END-IF
           IF  (NOT-I-83)
               SET I-73                    TO TRUE
           END-IF
           IF  (I-73)
               PERFORM EXCEPTION-OUTPUT
      *  LISTE PR 4.BESTILLER.
           END-IF
           IF  (NOT-I-84)
               SET I-74                    TO TRUE
           END-IF
           IF  (I-74)
               PERFORM EXCEPTION-OUTPUT
      *  DERSOM BLANK I BEST. PRINTES GRUPPEN PÅ "BESTILLER"
           END-IF
           IF  (I-81 AND I-82 AND I-83)
               AND (I-84)
               SET I-70                    TO TRUE
           END-IF.
 
       TOTLST-T.
           IF  (I-70)
               PERFORM EXCEPTION-OUTPUT
      *
           END-IF
           .
 
       SLUTT-T.
           CONTINUE.
 
       STYFILE-GET SECTION.
       STYFILE-GET-P.
           IF  STYFILE-EOF-OFF
               READ STYFILE
               AT END
                   SET STYFILE-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       STYFILE-FLDOFF SECTION.
       STYFILE-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-81                TO TRUE
               SET NOT-I-82                TO TRUE
               SET NOT-I-83                TO TRUE
               SET NOT-I-84                TO TRUE
           END-EVALUATE.
 
       STYFILE-FLDSET SECTION.
       STYFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE STYFILE-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE STYFILE-IO-AREA (5:5)  TO VGR (1:5)
               MOVE STYFILE-IO-AREA (10:3) TO ALFA (1:3)
               MOVE STYFILE-IO-AREA (23:9) TO B1 (1:9)
               IF  B1 = SPACES
                   SET I-81                TO TRUE
               END-IF
               MOVE STYFILE-IO-AREA (32:9) TO B2 (1:9)
               IF  B2 = SPACES
                   SET I-82                TO TRUE
               END-IF
               MOVE STYFILE-IO-AREA (41:9) TO B3 (1:9)
               IF  B3 = SPACES
                   SET I-83                TO TRUE
               END-IF
               MOVE STYFILE-IO-AREA (50:9) TO B4 (1:9)
               IF  B4 = SPACES
                   SET I-84                TO TRUE
               END-IF
           END-EVALUATE.
 
       STYFILE-IDSET SECTION.
       STYFILE-IDSET-P.
           SET I-01                        TO TRUE.
 
       STYFILE-CHK-LEVEL SECTION.
       STYFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO STYFILE-LEVEL-01
               MOVE STYFILE-IO-AREA (2:3)  TO STYFILE-01-L1-FIRMA
               IF  STYFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  STYFILE-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  STYFILE-01-L1         TO THE-PRIOR-L1
               SET STYFILE-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       STYFILE-MATCH-SET SECTION.
       STYFILE-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE STYFILE-IO-AREA (2:3)  TO STYFILE-M-01-M3-FIRMA
               MOVE STYFILE-IO-AREA (5:5)  TO STYFILE-M-01-M2-VGR
               MOVE STYFILE-IO-AREA (10:3) TO STYFILE-M-01-M1-ALFA
           END-EVALUATE.
 
       BEST-GET SECTION.
       BEST-GET-P.
           IF  BEST-EOF-OFF
               READ BEST
               AT END
                   SET BEST-EOF            TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       BEST-FLDSET SECTION.
       BEST-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE BEST-IO-AREA (1:128)   TO REC (1:128)
               MOVE BEST-IO-AREA (2:3)     TO FIRMA (1:3)
               MOVE BEST-IO-AREA (119:5)   TO VGR (1:5)
               MOVE BEST-IO-AREA (33:3)    TO ALFA (1:3)
           END-EVALUATE.
 
       BEST-IDSET SECTION.
       BEST-IDSET-P.
           SET I-02                        TO TRUE.
 
       BEST-CHK-LEVEL SECTION.
       BEST-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO BEST-LEVEL-02
               MOVE BEST-IO-AREA (2:3)     TO BEST-02-L1-FIRMA
               IF  BEST-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  BEST-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  BEST-02-L1            TO THE-PRIOR-L1
               SET BEST-LEVEL-INIT         TO TRUE
           END-EVALUATE.
 
       BEST-MATCH-SET SECTION.
       BEST-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE BEST-IO-AREA (2:3)     TO BEST-M-02-M3-FIRMA
               MOVE BEST-IO-AREA (119:5)   TO BEST-M-02-M2-VGR
               MOVE BEST-IO-AREA (33:3)    TO BEST-M-02-M1-ALFA
           END-EVALUATE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  STYFILE-EOF
               MOVE HIGH-VALUES            TO STYFILE-MC
                                              STYFILE-MP
           END-IF
           IF  BEST-EOF
               MOVE HIGH-VALUES            TO BEST-MC
                                              BEST-MP
           END-IF
           IF  STYFILE-MC < STYFILE-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  BEST-MC < BEST-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  STYFILE-MC < BEST-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET STYFILE-PROCESS     TO TRUE
                   MOVE STYFILE-MC         TO STYFILE-MP
                   IF  STYFILE-MC = BEST-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  BEST-MC < STYFILE-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET BEST-PROCESS        TO TRUE
                   MOVE BEST-MC            TO BEST-MP
                   IF  BEST-MC = STYFILE-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  STYFILE-MC = BEST-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET STYFILE-PROCESS     TO TRUE
                   MOVE STYFILE-MC         TO STYFILE-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       FNRTAB-LOAD SECTION.
       FNRTAB-LOAD-P.
           OPEN INPUT FNRTAB
           SET TABFNR-I                    TO 1
           PERFORM UNTIL FNRTAB-EOF
               READ FNRTAB
               AT END
                   SET FNRTAB-EOF          TO TRUE
               NOT AT END
                   MOVE FNRTAB-IO-AREA (1:3) TO TABFNR-ENTRY (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE FNRTAB-IO-AREA (4:3) TO TABFNR-ENTRY (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE FNRTAB-IO-AREA (7:3) TO TABFNR-ENTRY (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE FNRTAB-IO-AREA (10:3) TO TABFNR-ENTRY
                                                            (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE FNRTAB-IO-AREA (13:3) TO TABFNR-ENTRY
                                                            (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE FNRTAB-IO-AREA (16:3) TO TABFNR-ENTRY
                                                            (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE FNRTAB-IO-AREA (19:3) TO TABFNR-ENTRY
                                                            (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE FNRTAB-IO-AREA (22:3) TO TABFNR-ENTRY
                                                            (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE FNRTAB-IO-AREA (25:3) TO TABFNR-ENTRY
                                                            (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE FNRTAB-IO-AREA (28:3) TO TABFNR-ENTRY
                                                            (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE FNRTAB-IO-AREA (31:3) TO TABFNR-ENTRY
                                                            (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE FNRTAB-IO-AREA (34:3) TO TABFNR-ENTRY
                                                            (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE FNRTAB-IO-AREA (37:3) TO TABFNR-ENTRY
                                                            (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE FNRTAB-IO-AREA (40:3) TO TABFNR-ENTRY
                                                            (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE FNRTAB-IO-AREA (43:3) TO TABFNR-ENTRY
                                                            (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE FNRTAB-IO-AREA (46:3) TO TABFNR-ENTRY
                                                            (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE FNRTAB-IO-AREA (49:3) TO TABFNR-ENTRY
                                                            (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE FNRTAB-IO-AREA (52:3) TO TABFNR-ENTRY
                                                            (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE FNRTAB-IO-AREA (55:3) TO TABFNR-ENTRY
                                                            (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE FNRTAB-IO-AREA (58:3) TO TABFNR-ENTRY
                                                            (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE FNRTAB-IO-AREA (61:3) TO TABFNR-ENTRY
                                                            (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE FNRTAB-IO-AREA (64:3) TO TABFNR-ENTRY
                                                            (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE FNRTAB-IO-AREA (67:3) TO TABFNR-ENTRY
                                                            (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE FNRTAB-IO-AREA (70:3) TO TABFNR-ENTRY
                                                            (TABFNR-I)
                   SET TABFNR-I            UP BY 1
                   MOVE FNRTAB-IO-AREA (73:3) TO TABFNR-ENTRY
                                                            (TABFNR-I)
                   SET TABFNR-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE FNRTAB.
 
       EXCEPTION-OUTPUT SECTION.
       EXCEPTION-OUTPUT-P.
           IF  (I-02)
               MOVE SPACES TO BESTUT-IO-AREA
               INITIALIZE BESTUT-IO-AREA
               MOVE REC                    TO BESTUT-IO-AREA (9:128)
               MOVE FIRMA                  TO BESTUT-IO-AREA (1:3)
               IF  (I-71)
                   MOVE B1                 TO BESTUT-IO-AREA (4:9)
               END-IF
               IF  (I-72)
                   MOVE B2                 TO BESTUT-IO-AREA (4:9)
               END-IF
               IF  (I-73)
                   MOVE B3                 TO BESTUT-IO-AREA (4:9)
               END-IF
               IF  (I-74)
                   MOVE B4                 TO BESTUT-IO-AREA (4:9)
               END-IF
               IF  (I-70)
                   MOVE 'BESTILLER'        TO BESTUT-IO-AREA (4:9)
               END-IF
               WRITE BESTUT-IO-AREA
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
           SET STYFILE-LEVEL-INIT          TO TRUE
           INITIALIZE STYFILE-DATA-FIELDS
           SET STYFILE-EOF-OFF             TO TRUE
           SET STYFILE-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO STYFILE-MC
                                              STYFILE-MP
           OPEN INPUT STYFILE
           SET BEST-LEVEL-INIT             TO TRUE
           INITIALIZE BEST-DATA-FIELDS
           SET BEST-EOF-OFF                TO TRUE
           SET BEST-PROCESS                TO TRUE
           MOVE LOW-VALUES                 TO BEST-MC
                                              BEST-MP
           OPEN INPUT BEST
           PERFORM FNRTAB-LOAD
           OPEN OUTPUT BESTUT.
           SET TABFNR-I                    TO 1.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE STYFILE
           CLOSE BEST
           CLOSE BESTUT.
 
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
