       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK120R.
      ******************************************* :   Z-WIN-RPG2     **
      * 11.01.08 - COMP UTEN ENDRING PGA BETBET ENDRING.              *
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK120.rpg
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
           SELECT FAKPAR
               ASSIGN TO UT-S-FAKPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKPAR-STATUS.
           SELECT RESFILE
               ASSIGN TO UT-S-RESFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RESFILE-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD FAKPAR
               BLOCK CONTAINS 200
               RECORD CONTAINS 200.
       01  FAKPAR-IO-AREA.
           05  FAKPAR-IO-AREA-X            PICTURE X(200).
       FD RESFILE
               BLOCK CONTAINS 140
               RECORD CONTAINS 70.
       01  RESFILE-IO-AREA.
           05  RESFILE-IO-AREA-X           PICTURE X(70).
      * **START RBS - DATASTRUKTUR FOR SUB-PROGRAM RBS000 ********
       FD LISTE
               BLOCK CONTAINS 121
               RECORD CONTAINS 121.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(120).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  FAKPAR-STATUS               PICTURE 99 VALUE 0.
           10  RESFILE-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  BMFELT-XX-STATUS            PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  RESFILE-EOF-OFF         VALUE '0'.
               88  RESFILE-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESFILE-READ-OFF        VALUE '0'.
               88  RESFILE-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESFILE-PROCESS-OFF     VALUE '0'.
               88  RESFILE-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  RESFILE-LEVEL-INIT-OFF  VALUE '0'.
               88  RESFILE-LEVEL-INIT      VALUE '1'.
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
           05  BMFELT-XX-DATA-FIELDS.
               10  BETMAT                  PICTURE X(2).
               10  FILLER                  PICTURE X(255).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(2).
               10  BMTKST                  PICTURE X(24).
               10  FILLER                  PICTURE X(231).
           05  LDATA-XX REDEFINES BMFELT-XX-DATA-FIELDS.
               10  LONR                    PICTURE X(5).
               10  FILLER                  PICTURE X(252).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(5).
               10  LFIRMA                  PICTURE X(3).
               10  FILLER                  PICTURE X(249).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(8).
               10  LUNDGR                  PICTURE X(3).
               10  FILLER                  PICTURE X(246).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(11).
               10  LPROG                   PICTURE X(8).
               10  FILLER                  PICTURE X(238).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(19).
               10  LANTX-IO.
                   15  LANTX               PICTURE S9(3).
               10  FILLER                  PICTURE X(235).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(22).
               10  FINAVN                  PICTURE X(30).
               10  FILLER                  PICTURE X(205).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(52).
               10  LOPNVN                  PICTURE X(35).
               10  FILLER                  PICTURE X(170).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(87).
               10  LPRIID                  PICTURE X(4).
               10  FILLER                  PICTURE X(166).
      *     *  BESTILLINGSOPPGAVER (OVERSTYRING AV RBS-FILE) *    *
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(91).
               10  BJOBN                   PICTURE X(8).
               10  FILLER                  PICTURE X(158).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(99).
               10  BBEST                   PICTURE X(1).
               10  FILLER                  PICTURE X(157).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(100).
               10  BPERS                   PICTURE X(30).
               10  FILLER                  PICTURE X(127).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(130).
               10  BETTB                   PICTURE X(40).
               10  FILLER                  PICTURE X(87).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(170).
               10  BFORS                   PICTURE X(40).
               10  FILLER                  PICTURE X(47).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(210).
               10  BMEMO                   PICTURE X(40).
               10  FILLER                  PICTURE X(7).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(250).
               10  BANTX-IO.
                   15  BANTX               PICTURE S9(3).
               10  FILLER                  PICTURE X(4).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(253).
               10  BPCLAS                  PICTURE X(1).
               10  FILLER                  PICTURE X(3).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(254).
               10  BPRJE                   PICTURE X(3).
      * * END - RBS - DATASTRUKTUR FOR SUB-PROGRAM RBSH01 ********
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
           05  FAKPAR-DATA-FIELDS.
               10  FAKPER-IO.
                   15  FAKPER              PICTURE S9(6).
           05  RESFILE-LEVEL-01.
               10  RESFILE-01-L2.
                   15  RESFILE-01-L2-FIRMA PICTURE X(3).
               10  RESFILE-01-L1.
                   15  RESFILE-01-L1-BETKOD PICTURE X(2).
           05  RESFILE-DATA-FIELDS.
               10  BELO-ELGP-IO.
                   15  BELO-ELGP           PICTURE S9(7)V9(2).
               10  FIRMA                   PICTURE X(3).
               10  BETKOD                  PICTURE X(2).
               10  FAAR                    PICTURE X(2).
               10  FMND                    PICTURE X(2).
               10  FDAG                    PICTURE X(2).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(2).
           05  TEMPORARY-FIELDS.
               10  L1BEL-IO.
                   15  L1BEL               PICTURE S9(8)V9(2).
               10  L2BEL-IO.
                   15  L2BEL               PICTURE S9(8)V9(2).
           05  EDITTING-FIELDS.
               10  XO-82YY9R               PICTURE ZZ.ZZZ.ZZZ,99-.
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
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
           PERFORM HEADING-OUTPUT
           IF  NOT-SET-I-OF
               SET NOT-I-OF                TO TRUE
           END-IF
           SET NOT-SET-I-OF                TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-04                    TO TRUE
           SET NOT-I-01                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  RESFILE-PROCESS
               SET RESFILE-PROCESS-OFF     TO TRUE
               SET RESFILE-READ            TO TRUE
           END-IF
 
           IF  RESFILE-READ
           AND RECORD-SELECTED-OFF
               PERFORM RESFILE-GET
               SET RESFILE-READ-OFF        TO TRUE
               IF  NOT RESFILE-EOF
                   SET RESFILE-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  RESFILE-PROCESS
               PERFORM RESFILE-IDSET
           END-IF
 
           IF  RESFILE-PROCESS
               PERFORM RESFILE-CHK-LEVEL
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
 
           IF  RESFILE-PROCESS
               PERFORM RESFILE-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  RESFILE-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-L2)
               PERFORM RBSRUT-S
           END-IF
           IF  (I-L2 AND NOT-I-99)
               READ FAKPAR
               AT END
                   SET I-H0                TO TRUE
                   MOVE 'M'                TO E-R-R-O-R
               NOT AT END
                   PERFORM FAKPAR-FLDSET
                   PERFORM FAKPAR-IDSET
               END-READ
           END-IF
           IF  (I-L2)
               SET I-99                    TO TRUE
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *  COBOL SUBRUTINE FOR HENTING AV BETALINGSM�TE-TEKST     *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
           END-IF
           IF  (NOT-I-L1)
               GO TO ENDBM-T
           END-IF
           MOVE BETKOD                     TO BETMAT
           CALL 'BETBETN' USING BMFELT-XX-DATA-FIELDS.
 
       ENDBM-T.
      *****************************************************
      *   SUMERING AV ALLE BETALINGSM�TER PR. FIRMA.      *
      *****************************************************
           IF  (I-L1)
               SUBTRACT L1BEL              FROM L1BEL
           END-IF
           IF  (I-L2)
               SUBTRACT L2BEL              FROM L2BEL
           END-IF
           IF  (I-01)
               ADD BELO-ELGP               TO L1BEL
               ADD BELO-ELGP               TO L2BEL
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           END-IF
           .
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE ' '                        TO BBEST
           MOVE 'FAK03'                    TO LONR
           MOVE FIRMA                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'FAK120  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF.
      ******************************************************
 
       FAKPAR-FLDSET SECTION.
       FAKPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKPAR-IO-AREA (18:6)  TO FAKPER-IO
               INSPECT FAKPER-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       FAKPAR-IDSET SECTION.
       FAKPAR-IDSET-P.
           SET I-04                        TO TRUE.
 
       RESFILE-GET SECTION.
       RESFILE-GET-P.
           IF  RESFILE-EOF-OFF
               READ RESFILE
               AT END
                   SET RESFILE-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RESFILE-FLDSET SECTION.
       RESFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RESFILE-IO-AREA (35:9) TO BELO-ELGP-IO
               INSPECT BELO-ELGP-IO REPLACING ALL ' ' BY '0'
               MOVE RESFILE-IO-AREA (44:3) TO FIRMA (1:3)
               MOVE RESFILE-IO-AREA (48:2) TO BETKOD (1:2)
               MOVE RESFILE-IO-AREA (29:2) TO FAAR (1:2)
               MOVE RESFILE-IO-AREA (31:2) TO FMND (1:2)
               MOVE RESFILE-IO-AREA (33:2) TO FDAG (1:2)
           END-EVALUATE.
 
       RESFILE-IDSET SECTION.
       RESFILE-IDSET-P.
           SET I-01                        TO TRUE.
 
       RESFILE-CHK-LEVEL SECTION.
       RESFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO RESFILE-LEVEL-01
               MOVE RESFILE-IO-AREA (44:3) TO RESFILE-01-L2-FIRMA
               MOVE RESFILE-IO-AREA (48:2) TO RESFILE-01-L1-BETKOD
               IF  RESFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  RESFILE-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  RESFILE-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  RESFILE-01-L2         TO THE-PRIOR-L2
               MOVE  RESFILE-01-L1         TO THE-PRIOR-L1
               SET RESFILE-LEVEL-INIT      TO TRUE
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
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L2 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE LOPNVN                 TO LISTE-IO-AREA (32:35)
               MOVE 'PR'                   TO LISTE-IO-AREA (75:2)
               MOVE FAKPER                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (78:8)
               MOVE 'FREMSTILT'            TO LISTE-IO-AREA (91:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (101:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KODE'                 TO LISTE-IO-AREA (20:4)
               MOVE 'BETALINGSM�TE'        TO LISTE-IO-AREA (27:13)
               MOVE 'BEL�P INK.MOMS'       TO LISTE-IO-AREA (72:14)
               MOVE 'FORFALLSDATO'         TO LISTE-IO-AREA (94:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF AND NOT-I-L2 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE LOPNVN                 TO LISTE-IO-AREA (32:35)
               MOVE 'PR'                   TO LISTE-IO-AREA (75:2)
               MOVE FAKPER                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (78:8)
               MOVE 'FREMSTILT'            TO LISTE-IO-AREA (91:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (101:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KODE'                 TO LISTE-IO-AREA (20:4)
               MOVE 'BETALINGSM�TE'        TO LISTE-IO-AREA (27:13)
               MOVE 'BEL�P INK.MOMS'       TO LISTE-IO-AREA (72:14)
               MOVE 'FORFALLSDATO'         TO LISTE-IO-AREA (94:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE BETKOD                 TO LISTE-IO-AREA (21:2)
               MOVE BMTKST                 TO LISTE-IO-AREA (27:24)
               MOVE L1BEL                  TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (73:14)
               MOVE FDAG                   TO LISTE-IO-AREA (96:2)
               MOVE '.'                    TO LISTE-IO-AREA (98:1)
               MOVE FMND                   TO LISTE-IO-AREA (99:2)
               MOVE '.'                    TO LISTE-IO-AREA (101:1)
               MOVE FAAR                   TO LISTE-IO-AREA (102:2)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L2 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ' **'                  TO LISTE-IO-AREA (20:3)
               MOVE 'T O T A L T             ' TO LISTE-IO-AREA (28:24)
               MOVE L2BEL                  TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (73:14)
               MOVE 1                      TO LISTE-BEFORE-SPACE
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
           INITIALIZE FAKPAR-DATA-FIELDS
           OPEN INPUT FAKPAR
           SET RESFILE-LEVEL-INIT          TO TRUE
           INITIALIZE RESFILE-DATA-FIELDS
           SET RESFILE-EOF-OFF             TO TRUE
           SET RESFILE-PROCESS             TO TRUE
           OPEN INPUT RESFILE
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FAKPAR
           CLOSE RESFILE
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
