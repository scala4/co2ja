       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRI002R.
      **OBS ved endring i excel på Report Web *****************
      **********************************************  Z-WIN-RPG2   ****
      *  UTPLUKK FRA TRANSFILE-VAREARKIVET.    *************
      ******************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: PRI002.rpg
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
           SELECT VTRANS
               ASSIGN TO UT-S-VTRANS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VTRANS-STATUS.
           SELECT PRISMAS
               ASSIGN TO UT-S-PRISMAS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PRISMAS-STATUS.
           SELECT PRISREC
               ASSIGN TO UT-S-PRISREC
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PRISREC-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD VTRANS
               BLOCK CONTAINS 204
               RECORD CONTAINS 204.
       01  VTRANS-IO-AREA.
           05  VTRANS-IO-AREA-X            PICTURE X(204).
       FD PRISMAS
               BLOCK CONTAINS 8000
               RECORD CONTAINS 80.
       01  PRISMAS-IO-AREA.
           05  PRISMAS-IO-AREA-X           PICTURE X(80).
       FD PRISREC
               BLOCK CONTAINS 8000
               RECORD CONTAINS 80.
       01  PRISREC-IO-AREA.
           05  PRISREC-IO-AREA-X           PICTURE X(80).
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
           10  VTRANS-STATUS               PICTURE 99 VALUE 0.
           10  PRISMAS-STATUS              PICTURE 99 VALUE 0.
           10  PRISREC-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  VTRANS-EOF-OFF          VALUE '0'.
               88  VTRANS-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VTRANS-READ-OFF         VALUE '0'.
               88  VTRANS-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  VTRANS-PROCESS-OFF      VALUE '0'.
               88  VTRANS-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  VTRANS-LEVEL-INIT-OFF   VALUE '0'.
               88  VTRANS-LEVEL-INIT       VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PRISMAS-EOF-OFF         VALUE '0'.
               88  PRISMAS-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PRISMAS-READ-OFF        VALUE '0'.
               88  PRISMAS-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PRISMAS-PROCESS-OFF     VALUE '0'.
               88  PRISMAS-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  PRISMAS-LEVEL-INIT-OFF  VALUE '0'.
               88  PRISMAS-LEVEL-INIT      VALUE '1'.
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
           05  VTRANS-LEVEL-01.
               10  VTRANS-01-L2.
                   15  VTRANS-01-L2-FNR    PICTURE X(3).
               10  VTRANS-01-L1.
                   15  VTRANS-01-L1-REFN   PICTURE X(5).
           05  VTRANS-DATA-FIELDS.
               10  RA                      PICTURE X(2).
               10  FNR                     PICTURE X(3).
               10  REFN                    PICTURE X(5).
               10  VGR                     PICTURE X(5).
               10  ALFA                    PICTURE X(3).
               10  DATOE-IO.
                   15  DATOE               PICTURE S9(6).
               10  UTG                     PICTURE X(1).
           05  VTRANS-MP                   PICTURE X(8).
           05  VTRANS-MC                   PICTURE X(8).
           05  VTRANS-M-01             REDEFINES VTRANS-MC.
               10  VTRANS-M-01-M2.
                   15  VTRANS-M-01-M2-FNR-G.
                       20  VTRANS-M-01-M2-FNR PICTURE X(3).
               10  VTRANS-M-01-M1.
                   15  VTRANS-M-01-M1-REFN-G.
                       20  VTRANS-M-01-M1-REFN PICTURE X(5).
           05  PRISMAS-LEVEL-03.
               10  PRISMAS-03-L2.
                   15  PRISMAS-03-L2-FIRMA PICTURE X(3).
               10  PRISMAS-03-L1.
                   15  PRISMAS-03-L1-REF   PICTURE X(5).
           05  PRISMAS-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  REF                     PICTURE X(5).
               10  PDATO-IO.
                   15  PDATO               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  REC                     PICTURE X(80).
           05  PRISMAS-MP                  PICTURE X(8).
           05  PRISMAS-MC                  PICTURE X(8).
           05  PRISMAS-M-03            REDEFINES PRISMAS-MC.
               10  PRISMAS-M-03-M2.
                   15  PRISMAS-M-03-M2-FIRMA-G.
                       20  PRISMAS-M-03-M2-FIRMA PICTURE X(3).
               10  PRISMAS-M-03-M1.
                   15  PRISMAS-M-03-M1-REF-G.
                       20  PRISMAS-M-03-M1-REF PICTURE X(5).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(5).
           05  TEMPORARY-FIELDS.
               10  PRIDAT-IO.
                   15  PRIDAT              PICTURE S9(6).
           05  EDITTING-FIELDS.
               10  XO-60P-EF.
                 15  XO-60P                PICTURE S9(6) USAGE
                                                       PACKED-DECIMAL.
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
           IF  NOT-SET-I-OV
               SET NOT-I-OV                TO TRUE
           END-IF
           SET NOT-SET-I-OV                TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  VTRANS-PROCESS
               SET VTRANS-PROCESS-OFF      TO TRUE
               SET VTRANS-READ             TO TRUE
           END-IF
 
           IF  VTRANS-READ
               PERFORM VTRANS-GET
               SET VTRANS-READ-OFF         TO TRUE
               IF  NOT VTRANS-EOF
                   PERFORM VTRANS-MATCH-SET
               END-IF
           END-IF
 
           IF  PRISMAS-PROCESS
               SET PRISMAS-PROCESS-OFF     TO TRUE
               SET PRISMAS-READ            TO TRUE
           END-IF
 
           IF  PRISMAS-READ
               PERFORM PRISMAS-GET
               SET PRISMAS-READ-OFF        TO TRUE
               IF  NOT PRISMAS-EOF
                   PERFORM PRISMAS-MATCH-SET
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
 
           IF  VTRANS-PROCESS
               PERFORM VTRANS-IDSET
           END-IF
 
           IF  PRISMAS-PROCESS
               PERFORM PRISMAS-IDSET
           END-IF
 
           IF  VTRANS-PROCESS
               PERFORM VTRANS-CHK-LEVEL
           END-IF
 
           IF  PRISMAS-PROCESS
               PERFORM PRISMAS-CHK-LEVEL
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
 
           IF  VTRANS-PROCESS
               PERFORM VTRANS-FLDSET
           END-IF
 
           IF  PRISMAS-PROCESS
               PERFORM PRISMAS-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  VTRANS-PROCESS
           OR  PRISMAS-PROCESS
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
               SET NOT-I-50                TO TRUE
           END-IF
           IF  (I-MR AND I-01 AND NOT-I-50)
               PERFORM RBSRUT-S
           END-IF
           IF  (I-MR AND I-01 AND NOT-I-50)
               SET I-50                    TO TRUE
           END-IF
           IF  (I-MR AND I-01)
               SET NOT-I-20                TO TRUE
               IF  UTG = 'U'
                   SET I-20                TO TRUE
               END-IF
           END-IF
           IF  (I-MR AND I-03)
               ADD PDATO TO ZERO       GIVING PRIDAT
               SET NOT-I-10                TO TRUE
               IF  DATOE = PRIDAT
                   SET I-10                TO TRUE
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
           MOVE 'PRI02'                    TO LONR
           MOVE FIRMA                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'PRI002  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF.
      ******************************************************
 
       VTRANS-GET SECTION.
       VTRANS-GET-P.
           IF  VTRANS-EOF-OFF
               READ VTRANS
               AT END
                   SET VTRANS-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       VTRANS-FLDSET SECTION.
       VTRANS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VTRANS-IO-AREA (1:2)   TO RA (1:2)
               MOVE VTRANS-IO-AREA (3:3)   TO FNR (1:3)
               MOVE VTRANS-IO-AREA (14:5)  TO REFN (1:5)
               MOVE VTRANS-IO-AREA (19:5)  TO VGR (1:5)
               MOVE VTRANS-IO-AREA (24:3)  TO ALFA (1:3)
               MOVE VTRANS-IO-AREA (33:6)  TO DATOE-IO
               INSPECT DATOE-IO REPLACING ALL ' ' BY '0'
               MOVE VTRANS-IO-AREA (71:1)  TO UTG (1:1)
           END-EVALUATE.
 
       VTRANS-IDSET SECTION.
       VTRANS-IDSET-P.
           SET I-01                        TO TRUE.
 
       VTRANS-CHK-LEVEL SECTION.
       VTRANS-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO VTRANS-LEVEL-01
               MOVE VTRANS-IO-AREA (3:3)   TO VTRANS-01-L2-FNR
               MOVE VTRANS-IO-AREA (14:5)  TO VTRANS-01-L1-REFN
               IF  VTRANS-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  VTRANS-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  VTRANS-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  VTRANS-01-L2          TO THE-PRIOR-L2
               MOVE  VTRANS-01-L1          TO THE-PRIOR-L1
               SET VTRANS-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       VTRANS-MATCH-SET SECTION.
       VTRANS-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE VTRANS-IO-AREA (3:3)   TO VTRANS-M-01-M2-FNR
               MOVE VTRANS-IO-AREA (14:5)  TO VTRANS-M-01-M1-REFN
           END-EVALUATE.
 
       PRISMAS-GET SECTION.
       PRISMAS-GET-P.
           IF  PRISMAS-EOF-OFF
               READ PRISMAS
               AT END
                   SET PRISMAS-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       PRISMAS-FLDSET SECTION.
       PRISMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE PRISMAS-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE PRISMAS-IO-AREA (5:5)  TO REF (1:5)
               MOVE PRISMAS-IO-AREA (51:4) TO PDATO-IO
               MOVE PRISMAS-IO-AREA (1:80) TO REC (1:80)
           END-EVALUATE.
 
       PRISMAS-IDSET SECTION.
       PRISMAS-IDSET-P.
           SET I-03                        TO TRUE.
 
       PRISMAS-CHK-LEVEL SECTION.
       PRISMAS-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO PRISMAS-LEVEL-03
               MOVE PRISMAS-IO-AREA (2:3)  TO PRISMAS-03-L2-FIRMA
               MOVE PRISMAS-IO-AREA (5:5)  TO PRISMAS-03-L1-REF
               IF  PRISMAS-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  PRISMAS-03-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  PRISMAS-03-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  PRISMAS-03-L2         TO THE-PRIOR-L2
               MOVE  PRISMAS-03-L1         TO THE-PRIOR-L1
               SET PRISMAS-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       PRISMAS-MATCH-SET SECTION.
       PRISMAS-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE PRISMAS-IO-AREA (2:3)  TO PRISMAS-M-03-M2-FIRMA
               MOVE PRISMAS-IO-AREA (5:5)  TO PRISMAS-M-03-M1-REF
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
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  VTRANS-EOF
               MOVE HIGH-VALUES            TO VTRANS-MC
                                              VTRANS-MP
           END-IF
           IF  PRISMAS-EOF
               MOVE HIGH-VALUES            TO PRISMAS-MC
                                              PRISMAS-MP
           END-IF
           IF  VTRANS-MC < VTRANS-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  PRISMAS-MC < PRISMAS-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  VTRANS-MC < PRISMAS-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VTRANS-PROCESS      TO TRUE
                   MOVE VTRANS-MC          TO VTRANS-MP
                   IF  VTRANS-MC = PRISMAS-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  PRISMAS-MC < VTRANS-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET PRISMAS-PROCESS     TO TRUE
                   MOVE PRISMAS-MC         TO PRISMAS-MP
                   IF  PRISMAS-MC = VTRANS-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  VTRANS-MC = PRISMAS-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET VTRANS-PROCESS      TO TRUE
                   MOVE VTRANS-MC          TO VTRANS-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-MR AND I-01 AND NOT-I-86)
           AND (I-20)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FNR                    TO LISTE-IO-AREA (1:3)
               MOVE REFN                   TO LISTE-IO-AREA (6:5)
               MOVE ALFA                   TO LISTE-IO-AREA (13:3)
               MOVE VGR                    TO LISTE-IO-AREA (18:5)
               MOVE 'REFNUMMER FJERNET'    TO LISTE-IO-AREA (34:17)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (NOT-I-MR AND I-03)
               MOVE SPACES TO PRISREC-IO-AREA
               INITIALIZE PRISREC-IO-AREA
               MOVE REC                    TO PRISREC-IO-AREA (1:80)
               WRITE PRISREC-IO-AREA
           END-IF
           IF  (I-MR AND I-03 AND NOT-I-20)
               MOVE SPACES TO PRISREC-IO-AREA
               INITIALIZE PRISREC-IO-AREA
               MOVE REC                    TO PRISREC-IO-AREA (1:80)
               IF  (NOT-I-10)
                   MOVE DATOE              TO XO-60P
                   MOVE XO-60P-EF          TO PRISREC-IO-AREA (51:4)
               END-IF
               WRITE PRISREC-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-MR AND NOT-I-86 AND I-01)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE '*** REFNUMMER MERKET' TO LISTE-IO-AREA (45:20)
               MOVE '/ENDRET PÅ PRI1  '    TO LISTE-IO-AREA (67:17)
               MOVE 'DATO ='               TO LISTE-IO-AREA (90:6)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (97:8)
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
               MOVE '--------------'       TO LISTE-IO-AREA (119:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OV AND NOT-I-MR AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE '*** REFNUMMER MERKET' TO LISTE-IO-AREA (45:20)
               MOVE '/ENDRET PÅ PRI1  '    TO LISTE-IO-AREA (67:17)
               MOVE 'DATO ='               TO LISTE-IO-AREA (90:6)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (97:8)
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
               MOVE '--------------'       TO LISTE-IO-AREA (119:14)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-MR AND I-L1 AND NOT-I-86)
           AND (NOT-I-20 AND NOT-I-10)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FNR                    TO LISTE-IO-AREA (1:3)
               MOVE REFN                   TO LISTE-IO-AREA (6:5)
               MOVE ALFA                   TO LISTE-IO-AREA (13:3)
               MOVE VGR                    TO LISTE-IO-AREA (18:5)
               MOVE DATOE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (26:8)
               MOVE 'NY ENDRINGSDATO'      TO LISTE-IO-AREA (36:15)
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
           MOVE 2                          TO LR-CHECK
           SET VTRANS-LEVEL-INIT           TO TRUE
           INITIALIZE VTRANS-DATA-FIELDS
           SET VTRANS-EOF-OFF              TO TRUE
           SET VTRANS-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO VTRANS-MC
                                              VTRANS-MP
           OPEN INPUT VTRANS
           SET PRISMAS-LEVEL-INIT          TO TRUE
           INITIALIZE PRISMAS-DATA-FIELDS
           SET PRISMAS-EOF-OFF             TO TRUE
           SET PRISMAS-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO PRISMAS-MC
                                              PRISMAS-MP
           OPEN INPUT PRISMAS
           OPEN OUTPUT PRISREC
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE VTRANS
           CLOSE PRISMAS
           CLOSE PRISREC
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
