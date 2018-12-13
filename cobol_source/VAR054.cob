       IDENTIFICATION DIVISION.
       PROGRAM-ID. VAR054R.
      **********************************************  Z-WIN-RPG2   ****
      **OBS ved endring i excel på Report Web *****************
      **KVITTERINGSLISTE BEHOLDNINGSKORRIGERING.
      ** GJELDER  V B E K  UT I FRA AUTO.TRANSER1
      ******************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VAR054.rpg
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
           SELECT INPUT-X
               ASSIGN TO UT-S-INPUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INPUT-X-STATUS.
           SELECT VAREMAS
               ASSIGN TO VAREMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAREMAS-STATUS
               RECORD KEY IS VAREMAS-KEY1.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
           SELECT OUTPUT-X
               ASSIGN TO UT-S-OUTPUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTPUT-X-STATUS.
           SELECT SUMFILE
               ASSIGN TO UT-S-SUMFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS SUMFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD INPUT-X
               BLOCK CONTAINS 4000
               RECORD CONTAINS 80.
       01  INPUT-X-IO-AREA.
           05  INPUT-X-IO-AREA-X           PICTURE X(80).
       FD VAREMAS
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X.
               10  VAREMAS-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       FD OUTPUT-X
               BLOCK CONTAINS 5000
               RECORD CONTAINS 50.
       01  OUTPUT-X-IO-AREA.
           05  OUTPUT-X-IO-AREA-X          PICTURE X(50).
      * **START RBS - DATASTRUKTUR FOR SUB-PROGRAM RBS000 ********
       FD SUMFILE
               BLOCK CONTAINS 8100
               RECORD CONTAINS 60.
       01  SUMFILE-IO-AREA.
           05  SUMFILE-IO-AREA-X           PICTURE X(60).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  INPUT-X-STATUS              PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  OUTPUT-X-STATUS             PICTURE 99 VALUE 0.
           10  SUMFILE-STATUS              PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  INPUT-X-EOF-OFF         VALUE '0'.
               88  INPUT-X-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INPUT-X-READ-OFF        VALUE '0'.
               88  INPUT-X-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INPUT-X-PROCESS-OFF     VALUE '0'.
               88  INPUT-X-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  INPUT-X-LEVEL-INIT-OFF  VALUE '0'.
               88  INPUT-X-LEVEL-INIT      VALUE '1'.
           05  VAREMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  INPUT-X-LEVEL-01.
               10  INPUT-X-01-L2.
                   15  INPUT-X-01-L2-FIRMA PICTURE X(3).
               10  INPUT-X-01-L1.
                   15  INPUT-X-01-L1-REFNR PICTURE S9(6).
           05  INPUT-X-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  RECART                  PICTURE X(2).
               10  TERM                    PICTURE X(4).
               10  EDBNR                   PICTURE X(7).
               10  KODE                    PICTURE X(1).
               10  ANT-IO.
                   15  ANT                 PICTURE S9(6)V9(2).
               10  SIGN-X                  PICTURE X(1).
               10  LAGER                   PICTURE X(2).
               10  INITI                   PICTURE X(2).
               10  REFNR-IO.
                   15  REFNR               PICTURE S9(6).
               10  MERKN                   PICTURE X(16).
               10  INNPR-IO.
                   15  INNPR               PICTURE S9(7)V9(2).
           05  VAREMAS-DATA-FIELDS.
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  BETEGN                  PICTURE X(30).
               10  SELVK-IO.
                   15  SELVK               PICTURE S9(7)V9(2).
               10  UTSALG-IO.
                   15  UTSALG              PICTURE S9(7)V9(2).
               10  ALTNR                   PICTURE X(7).
               10  PT                      PICTURE X(1).
               10  BC                      PICTURE X(1).
               10  AVD                     PICTURE X(1).
               10  VGR                     PICTURE X(5).
               10  STAT                    PICTURE X(3).
               10  MERK                    PICTURE X(1).
               10  PRODGR                  PICTURE X(7).
               10  VT                      PICTURE X(1).
               10  LOCAT                   PICTURE X(6).
               10  UTGAR                   PICTURE X(1).
      *
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  OPPSLG                  PICTURE X(10).
               10  INNPRI-IO.
                   15  INNPRI              PICTURE S9(7)V9(2).
               10  REFSUM-IO.
                   15  REFSUM              PICTURE S9(9)V9(2).
               10  VATSUM-IO.
                   15  VATSUM              PICTURE S9(9)V9(2).
           05  EDITTING-FIELDS.
               10  XO-60YN9                PICTURE ZZZZZ9.
               10  XO-72YNZ                PICTURE ZZZZZZZ,ZZ.
               10  XO-62YNZ                PICTURE ZZZZZZ,ZZ.
               10  XO-92YNZR               PICTURE ZZZZZZZZZ,ZZ-.
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
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
           PERFORM HEADING-OUTPUT
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
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
           IF  INPUT-X-PROCESS
               SET INPUT-X-PROCESS-OFF     TO TRUE
               SET INPUT-X-READ            TO TRUE
           END-IF
 
           IF  INPUT-X-READ
           AND RECORD-SELECTED-OFF
               PERFORM INPUT-X-GET
               SET INPUT-X-READ-OFF        TO TRUE
               IF  NOT INPUT-X-EOF
                   SET INPUT-X-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  INPUT-X-PROCESS
               PERFORM INPUT-X-IDSET
           END-IF
 
           IF  INPUT-X-PROCESS
               PERFORM INPUT-X-CHK-LEVEL
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
 
           IF  INPUT-X-PROCESS
               PERFORM INPUT-X-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  INPUT-X-PROCESS
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
               MOVE 0                      TO REFSUM
      *
           END-IF
           SET NOT-I-40                    TO TRUE
           SET NOT-I-22                    TO TRUE
           IF  RECART = 'V2'
               SET I-22                    TO TRUE
           END-IF
           SET NOT-I-23                    TO TRUE
           IF  RECART = 'V3'
               SET I-23                    TO TRUE
           END-IF
           SET NOT-I-24                    TO TRUE
           IF  RECART = 'V4'
               SET I-24                    TO TRUE
           END-IF
           SET NOT-I-25                    TO TRUE
           IF  RECART = 'V5'
               SET I-25                    TO TRUE
           END-IF
           SET NOT-I-30                    TO TRUE
           IF  SIGN-X = '+'
               SET I-30                    TO TRUE
           END-IF
           IF  (I-L2)
               PERFORM RBSRUT-S
      *
           END-IF
           MOVE FIRMA                      TO OPPSLG (1:3)
           MOVE EDBNR                      TO OPPSLG (4:7)
           MOVE OPPSLG                     TO VAREMAS-KEY1
           READ VAREMAS RECORD KEY IS VAREMAS-KEY1
           INVALID KEY
               SET I-15                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-15                TO TRUE
               PERFORM VAREMAS-FLDSET
               PERFORM VAREMAS-IDSET
           END-READ
           IF  (I-15)
               GO TO SLUTT-T
           END-IF
           IF  (I-22)
               MULTIPLY INNPR BY ANT   GIVING INNPRI
           END-IF
           IF  (NOT-I-22)
               MULTIPLY SELVK BY ANT   GIVING INNPRI
           END-IF
           IF  (I-30)
               ADD INNPRI                  TO REFSUM
           END-IF
           IF  (NOT-I-30)
               SUBTRACT INNPRI             FROM REFSUM
           END-IF
           IF  (I-30 AND I-22)
               ADD INNPRI                  TO VATSUM
           END-IF
           IF  (NOT-I-30 AND I-22)
               SUBTRACT INNPRI             FROM VATSUM
           END-IF
           SET I-40                        TO TRUE
      *
           .
 
       SLUTT-T.
      *
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           CONTINUE.
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE ' '                        TO BBEST
           MOVE 'VAR52'                    TO LONR
           MOVE FIRMA                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'VAR054  '                 TO LPROG
           MOVE 'XVAR52A '                 TO BJOBN
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF.
      ******************************************************
 
       INPUT-X-GET SECTION.
       INPUT-X-GET-P.
           IF  INPUT-X-EOF-OFF
               READ INPUT-X
               AT END
                   SET INPUT-X-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INPUT-X-FLDSET SECTION.
       INPUT-X-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INPUT-X-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE INPUT-X-IO-AREA (6:2)  TO RECART (1:2)
               MOVE INPUT-X-IO-AREA (8:4)  TO TERM (1:4)
               MOVE INPUT-X-IO-AREA (18:7) TO EDBNR (1:7)
               MOVE INPUT-X-IO-AREA (17:1) TO KODE (1:1)
               MOVE INPUT-X-IO-AREA (25:8) TO ANT-IO
               INSPECT ANT-IO REPLACING ALL ' ' BY '0'
               MOVE INPUT-X-IO-AREA (33:1) TO SIGN-X (1:1)
               MOVE INPUT-X-IO-AREA (34:2) TO LAGER (1:2)
               MOVE INPUT-X-IO-AREA (36:2) TO INITI (1:2)
               MOVE INPUT-X-IO-AREA (38:6) TO REFNR-IO
               INSPECT REFNR-IO REPLACING ALL ' ' BY '0'
               MOVE INPUT-X-IO-AREA (44:16) TO MERKN (1:16)
               MOVE INPUT-X-IO-AREA (62:9) TO INNPR-IO
               INSPECT INNPR-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       INPUT-X-IDSET SECTION.
       INPUT-X-IDSET-P.
           SET I-01                        TO TRUE.
 
       INPUT-X-CHK-LEVEL SECTION.
       INPUT-X-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO INPUT-X-LEVEL-01
               MOVE INPUT-X-IO-AREA (3:3)  TO INPUT-X-01-L2-FIRMA
               MOVE INPUT-X-IO-AREA (38:6) TO INPUT-X-01-L1-REFNR
               IF  INPUT-X-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INPUT-X-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INPUT-X-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INPUT-X-01-L2         TO THE-PRIOR-L2
               MOVE  INPUT-X-01-L1         TO THE-PRIOR-L1
               SET INPUT-X-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (13:3) TO ALFA (1:3)
               MOVE VAREMAS-IO-AREA (16:20) TO ARTNR (1:20)
               MOVE VAREMAS-IO-AREA (36:30) TO BETEGN (1:30)
               MOVE VAREMAS-IO-AREA (66:9) TO SELVK-IO
               INSPECT SELVK-IO REPLACING ALL ' ' BY '0'
               MOVE VAREMAS-IO-AREA (75:9) TO UTSALG-IO
               INSPECT UTSALG-IO REPLACING ALL ' ' BY '0'
               MOVE VAREMAS-IO-AREA (88:7) TO ALTNR (1:7)
               MOVE VAREMAS-IO-AREA (95:1) TO PT (1:1)
               MOVE VAREMAS-IO-AREA (96:1) TO BC (1:1)
               MOVE VAREMAS-IO-AREA (118:1) TO AVD (1:1)
               MOVE VAREMAS-IO-AREA (118:5) TO VGR (1:5)
               MOVE VAREMAS-IO-AREA (123:3) TO STAT (1:3)
               MOVE VAREMAS-IO-AREA (127:1) TO MERK (1:1)
               MOVE VAREMAS-IO-AREA (146:7) TO PRODGR (1:7)
               MOVE VAREMAS-IO-AREA (170:1) TO VT (1:1)
               MOVE VAREMAS-IO-AREA (140:6) TO LOCAT (1:6)
               MOVE VAREMAS-IO-AREA (127:1) TO UTGAR (1:1)
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
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
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND NOT-I-86 AND I-40)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ALFA                   TO LISTE-IO-AREA (1:3)
               MOVE ARTNR                  TO LISTE-IO-AREA (5:20)
               MOVE VGR                    TO LISTE-IO-AREA (26:5)
               MOVE LOCAT                  TO LISTE-IO-AREA (32:6)
               MOVE INNPRI                 TO XO-72YNZ
               MOVE XO-72YNZ               TO LISTE-IO-AREA (77:10)
               IF  (NOT-I-22)
                   MOVE SELVK              TO XO-72YNZ
                   MOVE XO-72YNZ           TO LISTE-IO-AREA (43:10)
               END-IF
               IF  (I-22)
                   MOVE INNPR              TO XO-72YNZ
                   MOVE XO-72YNZ           TO LISTE-IO-AREA (43:10)
               END-IF
               MOVE LAGER                  TO LISTE-IO-AREA (70:2)
               MOVE ANT                    TO XO-62YNZ
               MOVE XO-62YNZ               TO LISTE-IO-AREA (55:9)
               MOVE SIGN-X                 TO LISTE-IO-AREA (66:1)
               IF  (I-22)
                   MOVE 'VATI '            TO LISTE-IO-AREA (89:5)
               END-IF
               IF  (I-23)
                   MOVE 'VERK '            TO LISTE-IO-AREA (89:5)
               END-IF
               IF  (I-24)
                   MOVE 'KORR '            TO LISTE-IO-AREA (89:5)
               END-IF
               IF  (I-25)
                   MOVE 'UTTAK'            TO LISTE-IO-AREA (89:5)
               END-IF
               MOVE MERKN                  TO LISTE-IO-AREA (95:16)
               MOVE INITI                  TO LISTE-IO-AREA (121:2)
               MOVE TERM                   TO LISTE-IO-AREA (129:4)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-01 AND I-25)
               MOVE SPACES TO OUTPUT-X-IO-AREA
               INITIALIZE OUTPUT-X-IO-AREA
               MOVE FIRMA                  TO OUTPUT-X-IO-AREA (2:3)
               MOVE EDBNR                  TO OUTPUT-X-IO-AREA (5:7)
               MOVE ANT-IO                 TO OUTPUT-X-IO-AREA (14:8)
               MOVE VGR                    TO OUTPUT-X-IO-AREA (27:5)
               MOVE SIGN-X                 TO OUTPUT-X-IO-AREA (35:1)
               WRITE OUTPUT-X-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L1 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE LOPNVN                 TO LISTE-IO-AREA (32:35)
               MOVE 'REFNUMMER:'           TO LISTE-IO-AREA (71:10)
               MOVE REFNR                  TO XO-60YN9
               MOVE XO-60YN9               TO LISTE-IO-AREA (82:6)
               MOVE 'DATO.'                TO LISTE-IO-AREA (114:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (120:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '++++++++++++++++++++++++' TO LISTE-IO-AREA (1:24)
               MOVE '++++++++++++++++++++++++' TO LISTE-IO-AREA (25:24)
               MOVE '++++++++++++++++++++++++' TO LISTE-IO-AREA (49:24)
               MOVE '++++++++++++++++++++++++' TO LISTE-IO-AREA (73:24)
               MOVE '++++++++++++++++++++++++' TO LISTE-IO-AREA (97:24)
               MOVE '++++++++++++++'       TO LISTE-IO-AREA (119:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ALF ARTIKKELNUMMER'   TO LISTE-IO-AREA (1:18)
               MOVE 'VGR.  LOCATION'       TO LISTE-IO-AREA (26:14)
               MOVE 'KOST/INNPRIS'         TO LISTE-IO-AREA (41:12)
               MOVE 'ANTALL P/M LAGER'     TO LISTE-IO-AREA (58:16)
               MOVE 'INNKJØPSSUM'          TO LISTE-IO-AREA (75:11)
               MOVE 'KODE'                 TO LISTE-IO-AREA (89:4)
               MOVE 'MERKNADER'            TO LISTE-IO-AREA (95:9)
               MOVE 'INITIALER'            TO LISTE-IO-AREA (119:9)
               MOVE 'TERM'                 TO LISTE-IO-AREA (129:4)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '++++++++++++++++++++++++' TO LISTE-IO-AREA (1:24)
               MOVE '++++++++++++++++++++++++' TO LISTE-IO-AREA (25:24)
               MOVE '++++++++++++++++++++++++' TO LISTE-IO-AREA (49:24)
               MOVE '++++++++++++++++++++++++' TO LISTE-IO-AREA (73:24)
               MOVE '++++++++++++++++++++++++' TO LISTE-IO-AREA (97:24)
               MOVE '++++++++++++++'       TO LISTE-IO-AREA (119:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OV AND NOT-I-L1 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE LOPNVN                 TO LISTE-IO-AREA (32:35)
               MOVE 'REFNUMMER:'           TO LISTE-IO-AREA (71:10)
               MOVE REFNR                  TO XO-60YN9
               MOVE XO-60YN9               TO LISTE-IO-AREA (82:6)
               MOVE 'DATO.'                TO LISTE-IO-AREA (114:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (120:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '++++++++++++++++++++++++' TO LISTE-IO-AREA (1:24)
               MOVE '++++++++++++++++++++++++' TO LISTE-IO-AREA (25:24)
               MOVE '++++++++++++++++++++++++' TO LISTE-IO-AREA (49:24)
               MOVE '++++++++++++++++++++++++' TO LISTE-IO-AREA (73:24)
               MOVE '++++++++++++++++++++++++' TO LISTE-IO-AREA (97:24)
               MOVE '++++++++++++++'       TO LISTE-IO-AREA (119:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ALF ARTIKKELNUMMER'   TO LISTE-IO-AREA (1:18)
               MOVE 'VGR.  LOCATION'       TO LISTE-IO-AREA (26:14)
               MOVE 'KOST/INNPRIS'         TO LISTE-IO-AREA (41:12)
               MOVE 'ANTALL P/M LAGER'     TO LISTE-IO-AREA (58:16)
               MOVE 'INNKJØPSSUM'          TO LISTE-IO-AREA (75:11)
               MOVE 'KODE'                 TO LISTE-IO-AREA (89:4)
               MOVE 'MERKNADER'            TO LISTE-IO-AREA (95:9)
               MOVE 'INITIALER'            TO LISTE-IO-AREA (119:9)
               MOVE 'TERM'                 TO LISTE-IO-AREA (129:4)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '++++++++++++++++++++++++' TO LISTE-IO-AREA (1:24)
               MOVE '++++++++++++++++++++++++' TO LISTE-IO-AREA (25:24)
               MOVE '++++++++++++++++++++++++' TO LISTE-IO-AREA (49:24)
               MOVE '++++++++++++++++++++++++' TO LISTE-IO-AREA (73:24)
               MOVE '++++++++++++++++++++++++' TO LISTE-IO-AREA (97:24)
               MOVE '++++++++++++++'       TO LISTE-IO-AREA (119:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALT FOR REFNUMMER' TO LISTE-IO-AREA (51:20)
               MOVE REFSUM                 TO XO-92YNZR
               MOVE XO-92YNZR              TO LISTE-IO-AREA (75:13)
               INITIALIZE REFSUM
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L1)
               MOVE SPACES TO SUMFILE-IO-AREA
               INITIALIZE SUMFILE-IO-AREA
               MOVE 'V2'                   TO SUMFILE-IO-AREA (1:2)
               MOVE FIRMA                  TO SUMFILE-IO-AREA (3:3)
               MOVE REFNR-IO               TO SUMFILE-IO-AREA (6:6)
               INITIALIZE REFNR-IO
               MOVE UDATE                  TO SUMFILE-IO-AREA (12:6)
               MOVE INITI                  TO SUMFILE-IO-AREA (18:2)
               MOVE TERM                   TO SUMFILE-IO-AREA (20:4)
               MOVE LAGER                  TO SUMFILE-IO-AREA (24:2)
               MOVE VATSUM-IO              TO SUMFILE-IO-AREA (27:11)
               INITIALIZE VATSUM-IO
               MOVE MERKN                  TO SUMFILE-IO-AREA (38:16)
               WRITE SUMFILE-IO-AREA
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
           SET INPUT-X-LEVEL-INIT          TO TRUE
           INITIALIZE INPUT-X-DATA-FIELDS
           SET INPUT-X-EOF-OFF             TO TRUE
           SET INPUT-X-PROCESS             TO TRUE
           OPEN INPUT INPUT-X
           INITIALIZE VAREMAS-DATA-FIELDS
           OPEN INPUT VAREMAS
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES
           OPEN OUTPUT OUTPUT-X
           OPEN OUTPUT SUMFILE.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INPUT-X
           CLOSE VAREMAS
           IF LISTE-IO-AREA NOT = SPACES
             WRITE LISTE-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE-IO-AREA
           END-IF
           CLOSE LISTE
           CLOSE OUTPUT-X
           CLOSE SUMFILE.
 
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
