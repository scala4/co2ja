       IDENTIFICATION DIVISION.
       PROGRAM-ID. RGN008R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAMMET DANNER FIRMA-VGR/KONTONR. TABELL                   *
      * HVIS KONTONR. I VGR-MASTER FINNES I KONTOPLAN.                *
      * ENDRING 08.02.02: LAGER LISTE TIL HVER FIRMA MED FEILMELDINGER.        *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RGN008.rpg
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
           SELECT VAGRMAS
               ASSIGN TO VAGRMAS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS VAGRMAS-STATUS
               RECORD KEY IS VAGRMAS-KEY1.
           SELECT KONTOMA
               ASSIGN TO KONTOMA
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KONTOMA-STATUS
               RECORD KEY IS KONTOMA-KEY1.
           SELECT VGRKTO
               ASSIGN TO UT-S-VGRKTO
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VGRKTO-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD VAGRMAS
               RECORD CONTAINS 80.
       01  VAGRMAS-IO-AREA.
           05  VAGRMAS-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  VAGRMAS-KEY1.
                   15  VAGRMAS-KEY1N       PICTURE S9(8).
               10  FILLER                  PICTURE X(71).
       FD KONTOMA
               RECORD CONTAINS 60.
       01  KONTOMA-IO-AREA.
           05  KONTOMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KONTOMA-KEY1            PICTURE X(7).
               10  FILLER                  PICTURE X(52).
       FD VGRKTO
               BLOCK CONTAINS 1536
               RECORD CONTAINS 16.
       01  VGRKTO-IO-AREA.
           05  VGRKTO-IO-AREA-X            PICTURE X(16).
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
           10  VAGRMAS-STATUS              PICTURE 99 VALUE 0.
           10  KONTOMA-STATUS              PICTURE 99 VALUE 0.
           10  VGRKTO-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  VAGRMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  VAGRMAS-EOF-OFF         VALUE '0'.
               88  VAGRMAS-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAGRMAS-READ-OFF        VALUE '0'.
               88  VAGRMAS-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VAGRMAS-PROCESS-OFF     VALUE '0'.
               88  VAGRMAS-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  VAGRMAS-LEVEL-INIT-OFF  VALUE '0'.
               88  VAGRMAS-LEVEL-INIT      VALUE '1'.
           05  KONTOMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  VAGRMAS-LEVEL-01.
               10  VAGRMAS-01-L1.
                   15  VAGRMAS-01-L1-FNR   PICTURE X(3).
           05  VAGRMAS-DATA-FIELDS.
               10  FNR                     PICTURE X(3).
               10  VGR                     PICTURE X(5).
               10  FNRVGR                  PICTURE X(8).
               10  KTO8                    PICTURE X(8).
               10  KTO4                    PICTURE X(4).
           05  KONTOMA-DATA-FIELDS.
               10  FILLER                  PICTURE X.
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  FNRKTO                  PICTURE X(7).
               10  ANT-IO.
                   15  ANT                 PICTURE S9(4).
               10  ANTF-IO.
                   15  ANTF                PICTURE S9(4).
           05  EDITTING-FIELDS.
               10  XO-40YY9                PICTURE Z.ZZ9.
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
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  VAGRMAS-PROCESS
               SET VAGRMAS-PROCESS-OFF     TO TRUE
               SET VAGRMAS-READ            TO TRUE
           END-IF
 
           IF  VAGRMAS-READ
           AND RECORD-SELECTED-OFF
               PERFORM VAGRMAS-GET
               SET VAGRMAS-READ-OFF        TO TRUE
               IF  NOT VAGRMAS-EOF
                   SET VAGRMAS-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  VAGRMAS-PROCESS
               PERFORM VAGRMAS-IDSET
           END-IF
 
           IF  VAGRMAS-PROCESS
               PERFORM VAGRMAS-CHK-LEVEL
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
 
           IF  VAGRMAS-PROCESS
               PERFORM VAGRMAS-FLDOFF
               PERFORM VAGRMAS-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  VAGRMAS-PROCESS
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
               SET NOT-I-13                TO TRUE
           END-IF
           SET NOT-I-10                    TO TRUE
           SET NOT-I-11                    TO TRUE
           SET NOT-I-12                    TO TRUE
           IF  (I-09)
               GO TO SLUTT-T
           END-IF
           MOVE FNR                        TO FNRKTO (1:3)
           MOVE KTO4                       TO FNRKTO (4:4)
           MOVE FNRKTO                     TO KONTOMA-KEY1
           READ KONTOMA RECORD KEY IS KONTOMA-KEY1
           INVALID KEY
               SET I-11                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-11                TO TRUE
               PERFORM KONTOMA-IDSET
           END-READ
           IF  (NOT-I-11)
               SET I-10                    TO TRUE
           END-IF
           ADD 1                           TO ANT
           IF  (I-11)
               ADD 1                       TO ANTF
           END-IF
           IF  (I-11 AND NOT-I-13)
               PERFORM RBSRUT-S
           END-IF.
 
       SLUTT-T.
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           CONTINUE.
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET I-12                        TO TRUE
           SET I-13                        TO TRUE
           MOVE '  '                       TO BBEST
           MOVE 'REG98'                    TO LONR
           MOVE FNR                        TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'RGN008  '                 TO LPROG
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS.
 
       VAGRMAS-GET SECTION.
       VAGRMAS-GET-P.
           IF  VAGRMAS-EOF-OFF
               READ VAGRMAS
               AT END
                   SET VAGRMAS-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VAGRMAS-FLDOFF SECTION.
       VAGRMAS-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-09                TO TRUE
           END-EVALUATE.
 
       VAGRMAS-FLDSET SECTION.
       VAGRMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAGRMAS-IO-AREA (3:3)  TO FNR (1:3)
               MOVE VAGRMAS-IO-AREA (6:5)  TO VGR (1:5)
               MOVE VAGRMAS-IO-AREA (3:8)  TO FNRVGR (1:8)
               MOVE VAGRMAS-IO-AREA (69:8) TO KTO8 (1:8)
               IF  KTO8 = SPACES
                   SET I-09                TO TRUE
               END-IF
               MOVE VAGRMAS-IO-AREA (69:4) TO KTO4 (1:4)
           END-EVALUATE.
 
       VAGRMAS-IDSET SECTION.
       VAGRMAS-IDSET-P.
           SET I-01                        TO TRUE.
 
       VAGRMAS-CHK-LEVEL SECTION.
       VAGRMAS-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO VAGRMAS-LEVEL-01
               MOVE VAGRMAS-IO-AREA (3:3)  TO VAGRMAS-01-L1-FNR
               IF  VAGRMAS-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VAGRMAS-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  VAGRMAS-01-L1         TO THE-PRIOR-L1
               SET VAGRMAS-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       KONTOMA-IDSET SECTION.
       KONTOMA-IDSET-P.
           SET I-02                        TO TRUE.
 
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
           IF  (I-01 AND I-10)
               MOVE SPACES TO VGRKTO-IO-AREA
               INITIALIZE VGRKTO-IO-AREA
               MOVE FNRVGR                 TO VGRKTO-IO-AREA (1:8)
               MOVE KTO8                   TO VGRKTO-IO-AREA (9:8)
               WRITE VGRKTO-IO-AREA
           END-IF
           IF  (I-12)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KONVERTERING FRA VAREGRU' TO LISTE-IO-AREA (1:24)
               MOVE 'PPE TIL HOVEDBOKSKONTO J' TO LISTE-IO-AREA (25:24)
               MOVE 'FR PROGRAM """"""""""""""""""""""AR"   "T IT-OAE
                                                                   4:4
               MOVE 'FRAMSTILT:'           TO LISTE-IO-AREA (109:10)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (113:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'VAREGRUPPE'           TO LISTE-IO-AREA (1:10)
               MOVE 'HOVEDBOKSKONTO'       TO LISTE-IO-AREA (13:14)
               MOVE 'MERKNAD'              TO LISTE-IO-AREA (29:7)
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
           END-IF
           IF  (I-01 AND NOT-I-09 AND I-11)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE VGR                    TO LISTE-IO-AREA (2:5)
               MOVE KTO4                   TO LISTE-IO-AREA (13:4)
               MOVE '*** IKKE I KONTOPLAN ***' TO LISTE-IO-AREA (29:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       DETAIL-OVERFLOW SECTION.
       DETAIL-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KONVERTERING FRA VAREGRU' TO LISTE-IO-AREA (1:24)
               MOVE 'PPE TIL HOVEDBOKSKONTO J' TO LISTE-IO-AREA (25:24)
               MOVE 'FR PROGRAM """"""""""""""""""""""AR"   "T IT-OAE
                                                                   4:4
               MOVE 'FRAMSTILT:'           TO LISTE-IO-AREA (109:10)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (113:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'VAREGRUPPE'           TO LISTE-IO-AREA (1:10)
               MOVE 'HOVEDBOKSKONTO'       TO LISTE-IO-AREA (13:14)
               MOVE 'MERKNAD'              TO LISTE-IO-AREA (29:7)
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
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-13)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*** OBS! *** HOVEDBOKSKO' TO LISTE-IO-AREA (1:24)
               MOVE 'NTO SOM VISES ER FRA AUT' TO LISTE-IO-AREA (25:24)
               MOVE 'ODATAS KONTOPLAN.       ' TO LISTE-IO-AREA (49:24)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL VAREGRUPPER RELAT' TO LISTE-IO-AREA (1:24)
               MOVE 'ERT TIL HOVEDBOKSKONTO :' TO LISTE-IO-AREA (25:24)
               MOVE ANT                    TO XO-40YY9
               MOVE XO-40YY9               TO LISTE-IO-AREA (51:5)
               INITIALIZE ANT
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL VAREGRUPPER MED K' TO LISTE-IO-AREA (1:24)
               MOVE 'ONTONR UTENOM KONTOPLAN:' TO LISTE-IO-AREA (25:24)
               MOVE ANTF                   TO XO-40YY9
               MOVE XO-40YY9               TO LISTE-IO-AREA (51:5)
               INITIALIZE ANTF
      *****************************************************************
      * DUMMY                                                         *
      *****************************************************************
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR AND I-02 AND I-98)
           AND (I-U1 AND I-U2)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE PSDS                   TO LISTE-IO-AREA (1:80)
               MOVE R                      TO LISTE-IO-AREA (73:8)
               MOVE P-IO                   TO LISTE-IO-AREA (78:3)
               MOVE S-IO                   TO LISTE-IO-AREA (76:5)
               MOVE LONR                   TO LISTE-IO-AREA (76:5)
               MOVE LFIRMA                 TO LISTE-IO-AREA (78:3)
               MOVE LUNDGR                 TO LISTE-IO-AREA (78:3)
               MOVE LOPNVN                 TO LISTE-IO-AREA (46:35)
               MOVE LPRIID                 TO LISTE-IO-AREA (77:4)
               MOVE LANTX-IO               TO LISTE-IO-AREA (78:3)
               MOVE FINAVN                 TO LISTE-IO-AREA (51:30)
               MOVE BJOBN                  TO LISTE-IO-AREA (73:8)
               MOVE BBEST                  TO LISTE-IO-AREA (80:1)
               MOVE BPERS                  TO LISTE-IO-AREA (51:30)
               MOVE BETTB                  TO LISTE-IO-AREA (41:40)
               MOVE BFORS                  TO LISTE-IO-AREA (41:40)
               MOVE BMEMO                  TO LISTE-IO-AREA (41:40)
               MOVE BANTX-IO               TO LISTE-IO-AREA (78:3)
               MOVE LPROG                  TO LISTE-IO-AREA (73:8)
               MOVE BPCLAS                 TO LISTE-IO-AREA (80:1)
               MOVE BPRJE                  TO LISTE-IO-AREA (78:3)
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
           SET VAGRMAS-LEVEL-INIT          TO TRUE
           INITIALIZE VAGRMAS-DATA-FIELDS
           SET VAGRMAS-EOF-OFF             TO TRUE
           SET VAGRMAS-PROCESS             TO TRUE
           OPEN INPUT VAGRMAS
           OPEN INPUT KONTOMA
           OPEN OUTPUT VGRKTO
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE VAGRMAS
           CLOSE KONTOMA
           CLOSE VGRKTO
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
