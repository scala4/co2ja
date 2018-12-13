       IDENTIFICATION DIVISION.
       PROGRAM-ID. KOS110R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM.......: KOS110, MERKING AV POSTER AUTOMATISK BOKFØRT *
      *                          I KASSEOPPGJØR NÅR U1 ER AV          *
      *  PROGRAMMERER..: MORTEN TUVRØNNINGEN                          *
      *  KJØRES I JOBB.: FAK50A1                                      *
      *  LAGET DATO....: 13.12.11                                     *
      *  ENDRET........: 19.04.12 SKREVET ANTALL RECORDS + RBS-ETIKETT* 000007
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: KOS110.rpg
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
           SELECT MERKFIL
               ASSIGN TO UT-S-MERKFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS MERKFIL-STATUS.
           SELECT KASSOPP
               ASSIGN TO KASSOPP
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS KASSOPP-STATUS
               RECORD KEY IS KASSOPP-KEY1.
           SELECT LISTE
               ASSIGN TO UT-S-LISTE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD MERKFIL
               BLOCK CONTAINS 160
               RECORD CONTAINS 160.
       01  MERKFIL-IO-AREA.
           05  MERKFIL-IO-AREA-X           PICTURE X(160).
       FD KASSOPP
               RECORD CONTAINS 400.
       01  KASSOPP-IO-AREA.
           05  KASSOPP-IO-AREA-X.
               10  KASSOPP-KEY1.
                   15  KASSOPP-KEY1N       PICTURE S9(18).
               10  FILLER                  PICTURE X(382).
      *BUGFILO O   F 132 132            PRINTERSYS020
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
           10  MERKFIL-STATUS              PICTURE 99 VALUE 0.
           10  KASSOPP-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  MERKFIL-EOF-OFF         VALUE '0'.
               88  MERKFIL-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  MERKFIL-READ-OFF        VALUE '0'.
               88  MERKFIL-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  MERKFIL-PROCESS-OFF     VALUE '0'.
               88  MERKFIL-PROCESS         VALUE '1'.
           05  KASSOPP-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  KASSOPP-EOF-OFF         VALUE '0'.
               88  KASSOPP-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KASSOPP-READ-OFF        VALUE '0'.
               88  KASSOPP-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KASSOPP-PROCESS-OFF     VALUE '0'.
               88  KASSOPP-PROCESS         VALUE '1'.
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
      *PSDS: DATA STRUCTURE FIELDS
           05  PSDS-DATA-FIELDS.
               10  FILLER                  PICTURE X(28).
               10  R                       PICTURE X(8).
               10  FILLER                  PICTURE X(44).
           05  FILLER REDEFINES PSDS-DATA-FIELDS.
               10  FILLER                  PICTURE X(36).
               10  P-IO.
                   15  P                   PICTURE S9(3).
               10  FILLER                  PICTURE X(41).
           05  FILLER REDEFINES PSDS-DATA-FIELDS.
               10  FILLER                  PICTURE X(10).
               10  S-IO.
                   15  S                   PICTURE S9(5).
               10  FILLER                  PICTURE X(65).
      *DSDS: DATA STRUCTURE FIELDS
           05  LDATA-XX-DATA-FIELDS.
               10  LONR                    PICTURE X(5).
               10  FILLER                  PICTURE X(252).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(5).
               10  LFIRMA                  PICTURE X(3).
               10  FILLER                  PICTURE X(249).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(8).
               10  LUNDGR                  PICTURE X(3).
               10  FILLER                  PICTURE X(246).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(11).
               10  LPROG                   PICTURE X(8).
               10  FILLER                  PICTURE X(238).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(19).
               10  LANTX-IO.
                   15  LANTX               PICTURE S9(3).
               10  FILLER                  PICTURE X(235).
      *                                      23  52 FINAVN
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(52).
               10  LOPNVN                  PICTURE X(35).
               10  FILLER                  PICTURE X(170).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(87).
               10  LPRIID                  PICTURE X(4).
               10  FILLER                  PICTURE X(166).
      *     *  BESTILLINGSOPPGAVER (OVERSTYRING AV RBS-FILE) *    *
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(91).
               10  BJOBN                   PICTURE X(8).
               10  FILLER                  PICTURE X(158).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(99).
               10  BBEST                   PICTURE X(1).
               10  FILLER                  PICTURE X(157).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(100).
               10  BPERS                   PICTURE X(30).
               10  FILLER                  PICTURE X(127).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(130).
               10  BETTB                   PICTURE X(40).
               10  FILLER                  PICTURE X(87).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(170).
               10  BFORS                   PICTURE X(40).
               10  FILLER                  PICTURE X(47).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(210).
               10  BMEMO                   PICTURE X(40).
               10  FILLER                  PICTURE X(7).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(250).
               10  BANTX-IO.
                   15  BANTX               PICTURE S9(3).
               10  FILLER                  PICTURE X(4).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(253).
               10  BPCLAS                  PICTURE X(1).
               10  FILLER                  PICTURE X(3).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(254).
               10  BPRJE                   PICTURE X(3).
      * * END - RBS - DATASTRUKTUR FOR SUB-PROGRAM RBSH01 ********
      * * START RBS - PARAMETERKORT OVERSTYRING AV RBS-FILE  * *
           05  MERKFIL-DATA-FIELDS.
               10  KASKEY                  PICTURE X(16).
               10  REC132                  PICTURE X(132).
           05  MERKFIL-MP                  PICTURE X(16).
           05  MERKFIL-MC                  PICTURE X(16).
           05  MERKFIL-M-01            REDEFINES MERKFIL-MC.
               10  MERKFIL-M-01-M1.
                   15  MERKFIL-M-01-M1-KASKEY-G.
                       20  MERKFIL-M-01-M1-KASKEY PICTURE X(16).
           05  KASSOPP-DATA-FIELDS.
               10  FILLER                  PICTURE X.
           05  KASSOPP-MP                  PICTURE X(16).
           05  KASSOPP-MC                  PICTURE X(16).
           05  KASSOPP-M-02            REDEFINES KASSOPP-MC.
               10  KASSOPP-M-02-M1.
                   15  KASSOPP-M-02-M1-KASKEY-G.
                       20  KASSOPP-M-02-M1-KASKEY PICTURE X(16).
           05  TEMPORARY-FIELDS.
               10  ANTR01-IO.
                   15  ANTR01              PICTURE S9(5).
               10  ANTR02-IO.
                   15  ANTR02              PICTURE S9(5).
               10  ANTRMR-IO.
                   15  ANTRMR              PICTURE S9(5).
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
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  MERKFIL-PROCESS
               SET MERKFIL-PROCESS-OFF     TO TRUE
               SET MERKFIL-READ            TO TRUE
           END-IF
 
           IF  MERKFIL-READ
               PERFORM MERKFIL-GET
               SET MERKFIL-READ-OFF        TO TRUE
               IF  NOT MERKFIL-EOF
                   PERFORM MERKFIL-MATCH-SET
               END-IF
           END-IF
 
           IF  KASSOPP-PROCESS
               SET KASSOPP-PROCESS-OFF     TO TRUE
               SET KASSOPP-READ            TO TRUE
           END-IF
 
           IF  KASSOPP-READ
               PERFORM KASSOPP-GET
               SET KASSOPP-READ-OFF        TO TRUE
               IF  NOT KASSOPP-EOF
                   PERFORM KASSOPP-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  MERKFIL-PROCESS
               PERFORM MERKFIL-IDSET
           END-IF
 
           IF  KASSOPP-PROCESS
               PERFORM KASSOPP-IDSET
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
           PERFORM DETAIL-OVERFLOW
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  MERKFIL-PROCESS
               PERFORM MERKFIL-FLDSET
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
           SET NOT-I-12                    TO TRUE
           IF  (I-11)
               SET I-10                    TO TRUE
           END-IF
           IF  (NOT-I-10)
               PERFORM RBSRUT-S
           END-IF
           IF  (NOT-I-10)
               SET I-11                    TO TRUE
               SET I-12                    TO TRUE
           END-IF
           IF  (I-01)
               ADD 1                       TO ANTR01
           END-IF
           IF  (I-02)
               ADD 1                       TO ANTR02
           END-IF
           IF  (I-02 AND I-MR)
               ADD 1                       TO ANTRMR
               SET NOT-I-50                TO TRUE
               IF  ANTRMR > 0
                   SET I-50                TO TRUE
               END-IF
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           END-IF
           .
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE ' '                        TO BBEST
           MOVE 'REG58'                    TO LONR
           MOVE '399'                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'KOS110  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF.
 
       MERKFIL-GET SECTION.
       MERKFIL-GET-P.
           IF  MERKFIL-EOF-OFF
               READ MERKFIL
               AT END
                   SET MERKFIL-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       MERKFIL-FLDSET SECTION.
       MERKFIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE MERKFIL-IO-AREA (1:16) TO KASKEY (1:16)
               MOVE MERKFIL-IO-AREA (1:132) TO REC132 (1:132)
           END-EVALUATE.
 
       MERKFIL-IDSET SECTION.
       MERKFIL-IDSET-P.
           SET I-01                        TO TRUE.
 
       MERKFIL-MATCH-SET SECTION.
       MERKFIL-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE MERKFIL-IO-AREA (1:16) TO MERKFIL-M-01-M1-KASKEY
           END-EVALUATE.
 
       KASSOPP-GET SECTION.
       KASSOPP-GET-P.
           IF  KASSOPP-EOF-OFF
               READ KASSOPP
               AT END
                   SET KASSOPP-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KASSOPP-IDSET SECTION.
       KASSOPP-IDSET-P.
           SET I-02                        TO TRUE.
 
       KASSOPP-MATCH-SET SECTION.
       KASSOPP-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE KASSOPP-IO-AREA (1:16) TO KASSOPP-M-02-M1-KASKEY
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
           IF  MERKFIL-EOF
               MOVE HIGH-VALUES            TO MERKFIL-MC
                                              MERKFIL-MP
           END-IF
           IF  KASSOPP-EOF
               MOVE HIGH-VALUES            TO KASSOPP-MC
                                              KASSOPP-MP
           END-IF
           IF  MERKFIL-MC < MERKFIL-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  KASSOPP-MC < KASSOPP-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  MERKFIL-MC < KASSOPP-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET MERKFIL-PROCESS     TO TRUE
                   MOVE MERKFIL-MC         TO MERKFIL-MP
                   IF  MERKFIL-MC = KASSOPP-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  KASSOPP-MC < MERKFIL-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET KASSOPP-PROCESS     TO TRUE
                   MOVE KASSOPP-MC         TO KASSOPP-MP
                   IF  KASSOPP-MC = MERKFIL-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  MERKFIL-MC = KASSOPP-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET MERKFIL-PROCESS     TO TRUE
                   MOVE MERKFIL-MC         TO MERKFIL-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND I-MR AND NOT-I-U1)
               MOVE 'J'                    TO KASSOPP-IO-AREA (123:1)
               REWRITE KASSOPP-IO-AREA
           END-IF
           IF  (I-12)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               IF  (NOT-I-U1)
                   MOVE 'MERKING AV BOKFØRTE POST' TO LISTE-IO-AREA
                                                                (1:24)
               END-IF
               IF  (I-U1)
                   MOVE 'LISTING AV BOKFØRTE POST' TO LISTE-IO-AREA
                                                                (1:24)
               END-IF
               MOVE 'ER I KASSEOPPGJØR       ' TO LISTE-IO-AREA (25:24)
               MOVE 'DATO'                 TO LISTE-IO-AREA (68:4)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (73:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '<-----------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------   REC132   ' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '----------->'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-01 AND I-MR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE REC132                 TO LISTE-IO-AREA (1:132)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-02 AND I-MR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE REC132                 TO LISTE-IO-AREA (1:132)
               IF  (NOT-I-U1)
                   MOVE 'J'                TO LISTE-IO-AREA (123:1)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR AND I-98 AND I-U8)
           AND (I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE PSDS                   TO LISTE-IO-AREA (41:80)
               MOVE R                      TO LISTE-IO-AREA (113:8)
               MOVE P-IO                   TO LISTE-IO-AREA (118:3)
               MOVE S-IO                   TO LISTE-IO-AREA (116:5)
               MOVE LONR                   TO LISTE-IO-AREA (116:5)
               MOVE LFIRMA                 TO LISTE-IO-AREA (118:3)
               MOVE LUNDGR                 TO LISTE-IO-AREA (118:3)
               MOVE LPROG                  TO LISTE-IO-AREA (113:8)
               MOVE LOPNVN                 TO LISTE-IO-AREA (86:35)
               MOVE LANTX-IO               TO LISTE-IO-AREA (118:3)
               MOVE LPRIID                 TO LISTE-IO-AREA (117:4)
               MOVE BJOBN                  TO LISTE-IO-AREA (113:8)
               MOVE BBEST                  TO LISTE-IO-AREA (120:1)
               MOVE BPERS                  TO LISTE-IO-AREA (91:30)
               MOVE BETTB                  TO LISTE-IO-AREA (81:40)
               MOVE BFORS                  TO LISTE-IO-AREA (81:40)
               MOVE BMEMO                  TO LISTE-IO-AREA (81:40)
               MOVE BANTX-IO               TO LISTE-IO-AREA (118:3)
               MOVE BPCLAS                 TO LISTE-IO-AREA (120:1)
               MOVE BPRJE                  TO LISTE-IO-AREA (118:3)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       DETAIL-OVERFLOW SECTION.
       DETAIL-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               IF  (NOT-I-U1)
                   MOVE 'MERKING AV BOKFØRTE POST' TO LISTE-IO-AREA
                                                                (1:24)
               END-IF
               IF  (I-U1)
                   MOVE 'LISTING AV BOKFØRTE POST' TO LISTE-IO-AREA
                                                                (1:24)
               END-IF
               MOVE 'ER I KASSEOPPGJØR       ' TO LISTE-IO-AREA (25:24)
               MOVE 'DATO'                 TO LISTE-IO-AREA (68:4)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (73:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '<-----------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------   REC132   ' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '----------->'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'Antall på MERKFIL      :' TO LISTE-IO-AREA (1:24)
               MOVE ANTR01                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (25:6)
               MOVE 2                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'Antall på KASSOPP      :' TO LISTE-IO-AREA (1:24)
               MOVE ANTR01                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (25:6)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               IF  (NOT-I-U1)
                   MOVE 'Antall merket ferdig   :' TO LISTE-IO-AREA
                                                                (1:24)
               END-IF
               IF  (I-U1)
                   MOVE 'Antall ferdig bokført  :' TO LISTE-IO-AREA
                                                                (1:24)
               END-IF
               MOVE ANTRMR                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (25:6)
               IF  (I-U1)
                   MOVE ' uten oppdatering.      ' TO LISTE-IO-AREA
                                                               (31:24)
               END-IF
               IF  (NOT-I-U1)
                   MOVE ' med oppdatering.       ' TO LISTE-IO-AREA
                                                               (31:24)
               END-IF
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR AND NOT-I-50)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '>>>>>>>>>>>>>>>>>>>>>>> ' TO LISTE-IO-AREA (1:24)
               MOVE ' Ingen bokføringer idag ' TO LISTE-IO-AREA (25:24)
               MOVE ' <<<<<<<<<<<<<<<<<<<<<<<' TO LISTE-IO-AREA (49:24)
      * DUMMY-LINJE FOR Å FJERNE KOMPILERINGSFEIL
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
           INITIALIZE MERKFIL-DATA-FIELDS
           SET MERKFIL-EOF-OFF             TO TRUE
           SET MERKFIL-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO MERKFIL-MC
                                              MERKFIL-MP
           OPEN INPUT MERKFIL
           SET KASSOPP-EOF-OFF             TO TRUE
           SET KASSOPP-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO KASSOPP-MC
                                              KASSOPP-MP
           OPEN I-O KASSOPP
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE MERKFIL
           CLOSE KASSOPP
           IF LISTE-IO-AREA NOT = SPACES
             WRITE LISTE-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE-IO-AREA
           END-IF
           CLOSE LISTE.
 
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
