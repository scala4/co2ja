       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK005R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAM....FAK005.                                            *
      * PROGRAMERER..ESPEN LARSEN  2.12.2005                          *
      * OPPDATERING AV FAKTURA.GRP. FRA FAKTURANR.FILE.               *
      * DETTE ER FIRMA SOM SKAL FAKTURERE PÅ NESTE FAKTURERING.       *
      * PROGRAMMET KJØRES I JOBB. FAK05UU.                            *
      * KUN FIRMA MED FAKT.NR. OG EPL4-NR. BLIR OPPDAT. MEDE FAKT.GRP.*
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK005.rpg
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
           SELECT FAKTAB
               ASSIGN TO UT-S-FAKTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKTAB-STATUS.
           SELECT FNRIN
               ASSIGN TO UT-S-FNRIN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FNRIN-STATUS.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD FAKTAB
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  FAKTAB-IO-AREA.
           05  FAKTAB-IO-AREA-X            PICTURE X(80).
       FD FNRIN
               BLOCK CONTAINS 1050
               RECORD CONTAINS 50.
       01  FNRIN-IO-AREA.
           05  FNRIN-IO-AREA-X             PICTURE X(50).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1.
                   15  FIRMAF-KEY1N        PICTURE S9(3).
               10  FILLER                  PICTURE X(994).
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       WORKING-STORAGE SECTION.
       77  TABFNR-MAX   VALUE 500          PICTURE 9(4) USAGE BINARY.
       77  TABGRF-MAX   VALUE 500          PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  TABFNR-TABLE.
               10  TABFNR-ENTRY
                                           OCCURS 500 TIMES
                                           INDEXED BY TABFNR-I
                                                      TABFNR-S
                                                      TABGRF-I
                                                      TABGRF-S.
                   15  TABFNR              PICTURE X(3).
                   15  TABGRF              PICTURE X(8).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  FAKTAB-STATUS               PICTURE 99 VALUE 0.
           10  FNRIN-STATUS                PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKTAB-EOF-OFF          VALUE '0'.
               88  FAKTAB-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FNRIN-EOF-OFF           VALUE '0'.
               88  FNRIN-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FNRIN-READ-OFF          VALUE '0'.
               88  FNRIN-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FNRIN-PROCESS-OFF       VALUE '0'.
               88  FNRIN-PROCESS           VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  FNRIN-LEVEL-INIT-OFF    VALUE '0'.
               88  FNRIN-LEVEL-INIT        VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  FIRMAF-EOF-OFF          VALUE '0'.
               88  FIRMAF-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FIRMAF-READ-OFF         VALUE '0'.
               88  FIRMAF-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FIRMAF-PROCESS-OFF      VALUE '0'.
               88  FIRMAF-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  FIRMAF-LEVEL-INIT-OFF   VALUE '0'.
               88  FIRMAF-LEVEL-INIT       VALUE '1'.
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
           05  FNRIN-LEVEL-01.
               10  FNRIN-01-L1.
                   15  FNRIN-01-L1-FNR     PICTURE X(3).
           05  FNRIN-DATA-FIELDS.
               10  FNR                     PICTURE X(3).
           05  FNRIN-MP                    PICTURE X(3).
           05  FNRIN-MC                    PICTURE X(3).
           05  FNRIN-M-01              REDEFINES FNRIN-MC.
               10  FNRIN-M-01-M1.
                   15  FNRIN-M-01-M1-FNR-G.
                       20  FNRIN-M-01-M1-FNR PICTURE X(3).
           05  FIRMAF-LEVEL-04.
               10  FIRMAF-04-L1.
                   15  FIRMAF-04-L1-FNR    PICTURE X(3).
           05  FIRMAF-DATA-FIELDS.
               10  FINAVN                  PICTURE X(30).
      *                                     503 503 FAKGRP
               10  SLETT                   PICTURE X(1).
           05  FIRMAF-MP                   PICTURE X(3).
           05  FIRMAF-MC                   PICTURE X(3).
           05  FIRMAF-M-04             REDEFINES FIRMAF-MC.
               10  FIRMAF-M-04-M1.
                   15  FIRMAF-M-04-M1-FNR-G.
                       20  FIRMAF-M-04-M1-FNR PICTURE X(3).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  ANTL-IO.
                   15  ANTL                PICTURE S9(5).
               10  ANTFK-IO.
                   15  ANTFK               PICTURE S9(5).
           05  EDITTING-FIELDS.
               10  XO-50YY9                PICTURE ZZ.ZZ9.
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
           SET NOT-I-04                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  FNRIN-PROCESS
               SET FNRIN-PROCESS-OFF       TO TRUE
               SET FNRIN-READ              TO TRUE
           END-IF
 
           IF  FNRIN-READ
               PERFORM FNRIN-GET
               SET FNRIN-READ-OFF          TO TRUE
               IF  NOT FNRIN-EOF
                   PERFORM FNRIN-MATCH-SET
               END-IF
           END-IF
 
           IF  FIRMAF-PROCESS
               SET FIRMAF-PROCESS-OFF      TO TRUE
               SET FIRMAF-READ             TO TRUE
           END-IF
 
           IF  FIRMAF-READ
               PERFORM FIRMAF-GET
               SET FIRMAF-READ-OFF         TO TRUE
               IF  NOT FIRMAF-EOF
                   PERFORM FIRMAF-MATCH-SET
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
 
           IF  FNRIN-PROCESS
               PERFORM FNRIN-IDSET
           END-IF
 
           IF  FIRMAF-PROCESS
               PERFORM FIRMAF-IDSET
           END-IF
 
           IF  FNRIN-PROCESS
               PERFORM FNRIN-CHK-LEVEL
           END-IF
 
           IF  FIRMAF-PROCESS
               PERFORM FIRMAF-CHK-LEVEL
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
 
           IF  FNRIN-PROCESS
               PERFORM FNRIN-FLDSET
           END-IF
 
           IF  FIRMAF-PROCESS
               PERFORM FIRMAF-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  FNRIN-PROCESS
           OR  FIRMAF-PROCESS
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
               SET NOT-I-29                TO TRUE
               SET NOT-I-44                TO TRUE
               SET NOT-I-11                TO TRUE
           END-IF
           IF  (I-01)
               SET NOT-I-29                TO TRUE
               SET TABFNR-S                TO TABFNR-I
               PERFORM WITH TEST AFTER
                       VARYING TABFNR-I FROM 1 BY 1
                         UNTIL TABFNR-I >= TABFNR-MAX
                            OR I-29
                   IF  FNR = TABFNR (TABFNR-I)
                       SET I-29            TO TRUE
                       SET TABFNR-S        TO TABFNR-I
                   END-IF
               END-PERFORM
               SET TABFNR-I                TO TABFNR-S
               IF  I-29
               AND TABFNR-I NOT > TABGRF-MAX
                   SET TABGRF-I            TO TABFNR-I
               END-IF
           END-IF
           IF  (I-04)
               ADD 1                       TO ANTL
               SET NOT-I-11                TO TRUE
               IF  SLETT = 'S'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND I-MR AND I-29)
               AND (NOT-I-11)
               SET I-44                    TO TRUE
           END-IF
           IF  (I-04 AND I-44)
               ADD 1                       TO ANTFK
           END-IF.
 
       FNRIN-GET SECTION.
       FNRIN-GET-P.
           IF  FNRIN-EOF-OFF
               READ FNRIN
               AT END
                   SET FNRIN-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FNRIN-FLDSET SECTION.
       FNRIN-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FNRIN-IO-AREA (3:3)    TO FNR (1:3)
           END-EVALUATE.
 
       FNRIN-IDSET SECTION.
       FNRIN-IDSET-P.
           SET I-01                        TO TRUE.
 
       FNRIN-CHK-LEVEL SECTION.
       FNRIN-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO FNRIN-LEVEL-01
               MOVE FNRIN-IO-AREA (3:3)    TO FNRIN-01-L1-FNR
               IF  FNRIN-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  FNRIN-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  FNRIN-01-L1           TO THE-PRIOR-L1
               SET FNRIN-LEVEL-INIT        TO TRUE
           END-EVALUATE.
 
       FNRIN-MATCH-SET SECTION.
       FNRIN-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE FNRIN-IO-AREA (3:3)    TO FNRIN-M-01-M1-FNR
           END-EVALUATE.
 
       FIRMAF-GET SECTION.
       FIRMAF-GET-P.
           IF  FIRMAF-EOF-OFF
               READ FIRMAF
               AT END
                   SET FIRMAF-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (5:3)   TO FNR (1:3)
               MOVE FIRMAF-IO-AREA (8:30)  TO FINAVN (1:30)
               MOVE FIRMAF-IO-AREA (123:1) TO SLETT (1:1)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-04                        TO TRUE.
 
       FIRMAF-CHK-LEVEL SECTION.
       FIRMAF-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO FIRMAF-LEVEL-04
               MOVE FIRMAF-IO-AREA (5:3)   TO FIRMAF-04-L1-FNR
               IF  FIRMAF-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  FIRMAF-04-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  FIRMAF-04-L1          TO THE-PRIOR-L1
               SET FIRMAF-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       FIRMAF-MATCH-SET SECTION.
       FIRMAF-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (5:3)   TO FIRMAF-M-04-M1-FNR
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
           IF  FNRIN-EOF
               MOVE HIGH-VALUES            TO FNRIN-MC
                                              FNRIN-MP
           END-IF
           IF  FIRMAF-EOF
               MOVE HIGH-VALUES            TO FIRMAF-MC
                                              FIRMAF-MP
           END-IF
           IF  FNRIN-MC < FNRIN-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  FIRMAF-MC < FIRMAF-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  FNRIN-MC < FIRMAF-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FNRIN-PROCESS       TO TRUE
                   MOVE FNRIN-MC           TO FNRIN-MP
                   IF  FNRIN-MC = FIRMAF-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  FIRMAF-MC < FNRIN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FIRMAF-PROCESS      TO TRUE
                   MOVE FIRMAF-MC          TO FIRMAF-MP
                   IF  FIRMAF-MC = FNRIN-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  FNRIN-MC = FIRMAF-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FNRIN-PROCESS       TO TRUE
                   MOVE FNRIN-MC           TO FNRIN-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       FAKTAB-LOAD SECTION.
       FAKTAB-LOAD-P.
           OPEN INPUT FAKTAB
           SET TABFNR-I                    TO 1
           PERFORM UNTIL FAKTAB-EOF
               READ FAKTAB
               AT END
                   SET FAKTAB-EOF          TO TRUE
               NOT AT END
                   MOVE FAKTAB-IO-AREA (1:11) TO TABFNR-ENTRY
                                                            (TABFNR-I)
                   SET TABFNR-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE FAKTAB.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-04)
               IF  (NOT-I-44)
                   MOVE ' '                TO FIRMAF-IO-AREA (503:1)
               END-IF
               IF  (I-44)
                   MOVE '2'                TO FIRMAF-IO-AREA (503:1)
               END-IF
               REWRITE FIRMAF-IO-AREA
           END-IF
           IF  (I-04 AND I-MR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FNR                    TO LISTE-IO-AREA (6:3)
               MOVE FINAVN                 TO LISTE-IO-AREA (10:30)
               IF  (I-44)
                   MOVE '2'                TO LISTE-IO-AREA (44:1)
               END-IF
               IF  (NOT-I-44)
                   MOVE 'X'                TO LISTE-IO-AREA (44:1)
               END-IF
               IF  (I-29)
                   MOVE TABGRF (TABGRF-I)  TO LISTE-IO-AREA (48:8)
               END-IF
               IF  (I-44)
                   MOVE 'OPPDATERT'        TO LISTE-IO-AREA (63:9)
               END-IF
               IF  (I-11)
                   MOVE 'SLETT DENNE.'     TO LISTE-IO-AREA (63:12)
               END-IF
               IF  (NOT-I-11 AND NOT-I-29)
                   MOVE 'EPL4-NR. MANGLER.' TO LISTE-IO-AREA (76:17)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'OPPDATERING AV FAKT.GRP.' TO LISTE-IO-AREA (7:24)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (33:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMANR'              TO LISTE-IO-AREA (2:7)
               MOVE 'FIRMA NAVN'           TO LISTE-IO-AREA (10:10)
               MOVE 'FAK.GR'               TO LISTE-IO-AREA (41:6)
               MOVE 'EPL4-NR.'             TO LISTE-IO-AREA (48:8)
               MOVE 'MERKNAD. '            TO LISTE-IO-AREA (63:9)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'OPPDATERING AV FAKT.GRP.' TO LISTE-IO-AREA (7:24)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (33:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMANR'              TO LISTE-IO-AREA (2:7)
               MOVE 'FIRMA NAVN'           TO LISTE-IO-AREA (10:10)
               MOVE 'FAK.GR'               TO LISTE-IO-AREA (41:6)
               MOVE 'EPL4-NR.'             TO LISTE-IO-AREA (48:8)
               MOVE 'MERKNAD. '            TO LISTE-IO-AREA (63:9)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTFK                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (35:6)
               MOVE '  FIRMA FAKTURAKLAR'  TO LISTE-IO-AREA (42:19)
               MOVE 2                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTL                   TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (35:6)
               MOVE '  FIRMA BEHANDLET.'   TO LISTE-IO-AREA (42:18)
               MOVE 1                      TO LISTE-BEFORE-SPACE
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
           PERFORM FAKTAB-LOAD
           SET FNRIN-LEVEL-INIT            TO TRUE
           INITIALIZE FNRIN-DATA-FIELDS
           SET FNRIN-EOF-OFF               TO TRUE
           SET FNRIN-PROCESS               TO TRUE
           MOVE LOW-VALUES                 TO FNRIN-MC
                                              FNRIN-MP
           OPEN INPUT FNRIN
           SET FIRMAF-LEVEL-INIT           TO TRUE
           INITIALIZE FIRMAF-DATA-FIELDS
           SET FIRMAF-EOF-OFF              TO TRUE
           SET FIRMAF-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO FIRMAF-MC
                                              FIRMAF-MP
           OPEN I-O FIRMAF
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           SET TABFNR-I                    TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FNRIN
           CLOSE FIRMAF
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
