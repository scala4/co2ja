       IDENTIFICATION DIVISION.
       PROGRAM-ID. VRG022R.
      **********************************************  Z-WIN-RPG2      *
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VRG022.rpg
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
           SELECT SLETTF
               ASSIGN TO UT-S-SLETTF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS SLETTF-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD SLETTF
               BLOCK CONTAINS 9400
               RECORD CONTAINS 200.
       01  SLETTF-IO-AREA.
           05  SLETTF-IO-AREA-X            PICTURE X(200).
      * **START RBS - DATASTRUKTUR FOR SUB-PROGRAM RBS000 ********
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
           10  SLETTF-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  SLETTF-EOF-OFF          VALUE '0'.
               88  SLETTF-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SLETTF-READ-OFF         VALUE '0'.
               88  SLETTF-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SLETTF-PROCESS-OFF      VALUE '0'.
               88  SLETTF-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  SLETTF-LEVEL-INIT-OFF   VALUE '0'.
               88  SLETTF-LEVEL-INIT       VALUE '1'.
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
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(22).
               10  FINAVN                  PICTURE X(30).
               10  FILLER                  PICTURE X(205).
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
           05  SLETTF-LEVEL-01.
               10  SLETTF-01-L3.
                   15  SLETTF-01-L3-FIRMA  PICTURE X(3).
               10  SLETTF-01-L2.
                   15  SLETTF-01-L2-AVD    PICTURE X(1).
               10  SLETTF-01-L1.
                   15  SLETTF-01-L1-VG     PICTURE X(4).
           05  SLETTF-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  ENR                     PICTURE X(7).
               10  VGR                     PICTURE X(5).
               10  AVD                     PICTURE X(1).
               10  VG                      PICTURE X(4).
               10  ALF                     PICTURE X(3).
               10  ANR                     PICTURE X(20).
               10  ANAVN                   PICTURE X(30).
               10  SPRIS-IO.
                   15  SPRIS               PICTURE S9(7)V9(2).
               10  SUPNR                   PICTURE X(7).
               10  AINN-IO.
                   15  AINN                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  AUT-IO.
                   15  AUT                 PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  UTLEVR                  PICTURE X(2).
               10  LOCAT                   PICTURE X(6).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(1).
               10  THE-PRIOR-L1            PICTURE X(4).
           05  TEMPORARY-FIELDS.
               10  BEH-IO.
                   15  BEH                 PICTURE S9(7).
           05  EDITTING-FIELDS.
               10  XO-72YN9                PICTURE ZZZZZZZ,99.
               10  XO-70YN9                PICTURE ZZZZZZ9.
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
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  SLETTF-PROCESS
               SET SLETTF-PROCESS-OFF      TO TRUE
               SET SLETTF-READ             TO TRUE
           END-IF
 
           IF  SLETTF-READ
           AND RECORD-SELECTED-OFF
               PERFORM SLETTF-GET
               SET SLETTF-READ-OFF         TO TRUE
               IF  NOT SLETTF-EOF
                   SET SLETTF-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  SLETTF-PROCESS
               PERFORM SLETTF-IDSET
           END-IF
 
           IF  SLETTF-PROCESS
               PERFORM SLETTF-CHK-LEVEL
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
 
           IF  SLETTF-PROCESS
               PERFORM SLETTF-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  SLETTF-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-L3)
               PERFORM RBSRUT-S
           END-IF
           IF  (I-L3)
               SET NOT-I-93                TO TRUE
               IF  FIRMA = '937'
                   SET I-93                TO TRUE
               END-IF
           END-IF
           SET NOT-I-54                    TO TRUE
           IF  UTLEVR = 'U '
               SET I-54                    TO TRUE
           END-IF
           IF  (I-54)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-55                    TO TRUE
           IF  UTLEVR = 'UU'
               SET I-55                    TO TRUE
           END-IF
      ******************************************************
      *    BEREGNING AV BEHOLDNING I ANTALL.               *
      ******************************************************
           SUBTRACT AUT FROM AINN      GIVING BEH
           SET NOT-I-10                    TO TRUE
           IF  BEH < 0
               SET I-10                    TO TRUE
           END-IF
           IF  (I-10)
               MOVE 0                      TO BEH
           END-IF
           SET NOT-I-12                    TO TRUE
           IF  BEH > 0
               SET I-12                    TO TRUE
           END-IF
      ******************************************************
      *    BEREGNING AV BEHOLDNINGSVERDI.                  *
      ******************************************************
      *          SPRIS     MULT BEH       VERDI  112
      *          VVERDI    ADD  VERDI     VVERDI 112
      *          TVERDI    ADD  VERDI     TVERDI 112
           .
 
       SLUTT-T.
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           CONTINUE.
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE '  '                       TO BBEST
           MOVE 'VAR28'                    TO LONR
           MOVE FIRMA                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'VRG022  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF.
      ******************************************************
 
       SLETTF-GET SECTION.
       SLETTF-GET-P.
           IF  SLETTF-EOF-OFF
               READ SLETTF
               AT END
                   SET SLETTF-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       SLETTF-FLDSET SECTION.
       SLETTF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE SLETTF-IO-AREA (3:3)   TO FIRMA (1:3)
               MOVE SLETTF-IO-AREA (6:7)   TO ENR (1:7)
               MOVE SLETTF-IO-AREA (118:5) TO VGR (1:5)
               MOVE SLETTF-IO-AREA (118:1) TO AVD (1:1)
               MOVE SLETTF-IO-AREA (119:4) TO VG (1:4)
               MOVE SLETTF-IO-AREA (13:3)  TO ALF (1:3)
               MOVE SLETTF-IO-AREA (16:20) TO ANR (1:20)
               MOVE SLETTF-IO-AREA (36:30) TO ANAVN (1:30)
               MOVE SLETTF-IO-AREA (66:9)  TO SPRIS-IO
               INSPECT SPRIS-IO REPLACING ALL ' ' BY '0'
               MOVE SLETTF-IO-AREA (88:7)  TO SUPNR (1:7)
               MOVE SLETTF-IO-AREA (97:5)  TO AINN-IO
               MOVE SLETTF-IO-AREA (102:5) TO AUT-IO
               MOVE SLETTF-IO-AREA (127:2) TO UTLEVR (1:2)
               MOVE SLETTF-IO-AREA (140:6) TO LOCAT (1:6)
           END-EVALUATE.
 
       SLETTF-IDSET SECTION.
       SLETTF-IDSET-P.
           SET I-01                        TO TRUE.
 
       SLETTF-CHK-LEVEL SECTION.
       SLETTF-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO SLETTF-LEVEL-01
               MOVE SLETTF-IO-AREA (3:3)   TO SLETTF-01-L3-FIRMA
               MOVE SLETTF-IO-AREA (118:1) TO SLETTF-01-L2-AVD
               MOVE SLETTF-IO-AREA (119:4) TO SLETTF-01-L1-VG
               IF  SLETTF-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  SLETTF-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  SLETTF-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  SLETTF-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  SLETTF-01-L3          TO THE-PRIOR-L3
               MOVE  SLETTF-01-L2          TO THE-PRIOR-L2
               MOVE  SLETTF-01-L1          TO THE-PRIOR-L1
               SET SLETTF-LEVEL-INIT       TO TRUE
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
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND NOT-I-L1 AND NOT-I-86)
           AND (NOT-I-54)
           OR  (I-01 AND I-L1 AND NOT-I-86)
           AND (NOT-I-54)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE LOCAT                  TO LISTE-IO-AREA (1:6)
               MOVE ALF                    TO LISTE-IO-AREA (8:3)
               MOVE ANR                    TO LISTE-IO-AREA (12:20)
               MOVE ANAVN                  TO LISTE-IO-AREA (33:30)
               MOVE ENR                    TO LISTE-IO-AREA (64:7)
               MOVE SUPNR                  TO LISTE-IO-AREA (72:7)
               MOVE SPRIS                  TO XO-72YN9
               MOVE XO-72YN9               TO LISTE-IO-AREA (80:10)
               MOVE BEH                    TO XO-70YN9
               MOVE XO-70YN9               TO LISTE-IO-AREA (94:7)
      *                        VERDI 1  113
               MOVE VGR                    TO LISTE-IO-AREA (115:5)
      *                      12         132 "BEHOLDNING."
               EVALUATE TRUE
               WHEN (I-01 AND NOT-I-L1 AND NOT-I-86)
               AND  (NOT-I-54)
                   MOVE 1                  TO LISTE-AFTER-SPACE
               WHEN (I-01 AND I-L1 AND NOT-I-86)
               AND  (NOT-I-54)
                   MOVE 1                  TO LISTE-BEFORE-SPACE
                   MOVE 1                  TO LISTE-AFTER-SPACE
               END-EVALUATE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-01 AND I-55 AND NOT-I-86)
           AND (NOT-I-54)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '** OVENFORSTÅENDE ARTIKK' TO LISTE-IO-AREA (25:24)
               MOVE 'EL ER SLETTET ELLER UTGÅ' TO LISTE-IO-AREA (49:24)
               MOVE 'RMELDT FRA LEVERANDØR,- ' TO LISTE-IO-AREA (73:24)
               MOVE 'MERKET MED UTGÅR I EGET ' TO LISTE-IO-AREA (97:24)
               MOVE 'ARKIV. **'            TO LISTE-IO-AREA (121:9)
      *       T  1     L1N86N54
      *                                 100 "SUM VERDI VAREGRUPPE"
      *                        VVERDI1B 115
      *       T 2      L3N86
      *                                 100 "SUM VERDI TOTALT    "
      *                        TVERDI1B 115
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L2 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE LOPNVN                 TO LISTE-IO-AREA (32:35)
               MOVE 'DATO'                 TO LISTE-IO-AREA (90:4)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (95:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'LOCAT. ALF'           TO LISTE-IO-AREA (1:10)
               MOVE 'ARTIKKELNUMMER'       TO LISTE-IO-AREA (12:14)
               MOVE 'VAREBETEGNELSE'       TO LISTE-IO-AREA (33:14)
               MOVE 'EDB-NR. ALT-NR.'      TO LISTE-IO-AREA (64:15)
               MOVE 'KOSTPRIS       BEH.'  TO LISTE-IO-AREA (82:19)
      *                                 113 "SUM VERDI"
               MOVE 'VGR  ANMERKNING.'     TO LISTE-IO-AREA (117:16)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE LOPNVN                 TO LISTE-IO-AREA (32:35)
               MOVE 'DATO'                 TO LISTE-IO-AREA (90:4)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (95:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'LOCAT. ALF'           TO LISTE-IO-AREA (1:10)
               MOVE 'ARTIKKELNUMMER'       TO LISTE-IO-AREA (12:14)
               MOVE 'VAREBETEGNELSE'       TO LISTE-IO-AREA (33:14)
               MOVE 'EDB-NR. ALT-NR.'      TO LISTE-IO-AREA (64:15)
               MOVE 'KOSTPRIS       BEH.'  TO LISTE-IO-AREA (82:19)
      *                                 113 "SUM VERDI"
               MOVE 'VGR  ANMERKNING.'     TO LISTE-IO-AREA (117:16)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
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
           SET SLETTF-LEVEL-INIT           TO TRUE
           INITIALIZE SLETTF-DATA-FIELDS
           SET SLETTF-EOF-OFF              TO TRUE
           SET SLETTF-PROCESS              TO TRUE
           OPEN INPUT SLETTF
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE SLETTF
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
