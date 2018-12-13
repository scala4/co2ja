       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRI320R.
      **OBS ved endring i excel på Report Web *****************
      **********************************************  Z-WIN-RPG2   ****
      *  KVITTERINGSLISTE PRISENDRINGER              **
      *  FJERNING AV KITTERINGSMERKE ( 1 I POS.73 )  **
      * 10.1.12 : REFNR PÅ HVER LINJE PGA RWEB SORT - EN  **
      *************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: PRI320.rpg
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
           SELECT INNPUT
               ASSIGN TO UT-S-INNPUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INNPUT-STATUS.
           SELECT INNPUT2
               ASSIGN TO UT-S-INNPUT2
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INNPUT2-STATUS.
           SELECT VAREMAS
               ASSIGN TO VAREMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAREMAS-STATUS
               RECORD KEY IS VAREMAS-KEY1.
           SELECT PRISMAS
               ASSIGN TO PRISMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS PRISMAS-STATUS
               RECORD KEY IS PRISMAS-KEY1.
           SELECT PRINTF
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PRINTF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD INNPUT
               BLOCK CONTAINS 9600
               RECORD CONTAINS 80.
       01  INNPUT-IO-AREA.
           05  INNPUT-IO-AREA-X            PICTURE X(80).
       FD INNPUT2
               BLOCK CONTAINS 9600
               RECORD CONTAINS 80.
       01  INNPUT2-IO-AREA.
           05  INNPUT2-IO-AREA-X           PICTURE X(80).
       FD VAREMAS
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X.
               10  VAREMAS-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       FD PRISMAS
               RECORD CONTAINS 80.
       01  PRISMAS-IO-AREA.
           05  PRISMAS-IO-AREA-X.
               10  PRISMAS-KEY1            PICTURE X(13).
               10  FILLER                  PICTURE X(67).
      * **START RBS - DATASTRUKTUR FOR SUB-PROGRAM RBS000 ********
       FD PRINTF
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  PRINTF-IO-PRINT.
           05  PRINTF-IO-AREA-CONTROL      PICTURE X VALUE ' '.
        02 PRINTF-IO-AREA.
           05  PRINTF-IO-AREA-X            PICTURE X(132).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  INNPUT-STATUS               PICTURE 99 VALUE 0.
           10  INNPUT2-STATUS              PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  PRISMAS-STATUS              PICTURE 99 VALUE 0.
           10  PRINTF-STATUS               PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  INNPUT-EOF-OFF          VALUE '0'.
               88  INNPUT-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNPUT-READ-OFF         VALUE '0'.
               88  INNPUT-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNPUT-PROCESS-OFF      VALUE '0'.
               88  INNPUT-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  INNPUT-LEVEL-INIT-OFF   VALUE '0'.
               88  INNPUT-LEVEL-INIT       VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNPUT2-EOF-OFF         VALUE '0'.
               88  INNPUT2-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNPUT2-READ-OFF        VALUE '0'.
               88  INNPUT2-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNPUT2-PROCESS-OFF     VALUE '0'.
               88  INNPUT2-PROCESS         VALUE '1'.
           05  VAREMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  PRISMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  PRINTF-DATA-FIELDS.
               10  PRINTF-AFTER-SPACE      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINTF-AFTER-SKIP       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINTF-BEFORE-SPACE     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINTF-BEFORE-SKIP      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINTF-MAX-LINES        PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINTF-LINE-COUNT       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINTF-CLR-IO           PICTURE X VALUE 'Y'.
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
           05  INNPUT-LEVEL-01.
               10  INNPUT-01-L3.
                   15  INNPUT-01-L3-FIRMA  PICTURE X(3).
               10  INNPUT-01-L2.
                   15  INNPUT-01-L2-REFPER PICTURE X(2).
               10  INNPUT-01-L1.
                   15  INNPUT-01-L1-REFNR  PICTURE X(5).
           05  INNPUT-DATA-FIELDS.
               10  KEY13                   PICTURE X(13).
               10  RA                      PICTURE X(1).
               10  MERG                    PICTURE X(8).
               10  FIRMA                   PICTURE X(3).
               10  REFNR                   PICTURE X(5).
               10  SEQ                     PICTURE X(4).
               10  EDBNR                   PICTURE X(7).
               10  NYSVS-IO.
                   15  NYSVS               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  UTSALG-IO.
                   15  UTSALG              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  NYLEVP-IO.
                   15  NYLEVP              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ENDRES-IO.
                   15  ENDRES              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  KVITT                   PICTURE X(1).
               10  TERM                    PICTURE X(4).
               10  REFPER                  PICTURE X(2).
               10  OPPDAT                  PICTURE X(1).
           05  INNPUT-MP                   PICTURE X(8).
           05  INNPUT-MC                   PICTURE X(8).
           05  INNPUT-M-01             REDEFINES INNPUT-MC.
               10  INNPUT-M-01-M1.
                   15  INNPUT-M-01-M1-MERG-G.
                       20  INNPUT-M-01-M1-MERG PICTURE X(8).
           05  INNPUT2-DATA-FIELDS.
               10  FILLER                  PICTURE X.
           05  INNPUT2-MP                  PICTURE X(8).
           05  INNPUT2-MC                  PICTURE X(8).
           05  INNPUT2-M-04            REDEFINES INNPUT2-MC.
               10  INNPUT2-M-04-M1.
                   15  INNPUT2-M-04-M1-MERG-G.
                       20  INNPUT2-M-04-M1-MERG PICTURE X(8).
           05  VAREMAS-DATA-FIELDS.
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  VPRIS-IO.
                   15  VPRIS               PICTURE S9(7)V9(2).
               10  PT                      PICTURE X(1).
               10  VGR                     PICTURE X(5).
               10  AVD                     PICTURE X(1).
           05  PRISMAS-DATA-FIELDS.
               10  FILLER                  PICTURE X.
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(2).
               10  THE-PRIOR-L1            PICTURE X(5).
           05  TEMPORARY-FIELDS.
               10  KEY-X                   PICTURE X(10).
               10  NSVS-IO.
                   15  NSVS                PICTURE S9(7)V9(2).
               10  NUTP-IO.
                   15  NUTP                PICTURE S9(7)V9(2).
               10  NLEVP-IO.
                   15  NLEVP               PICTURE S9(7)V9(2).
               10  EDATO-IO.
                   15  EDATO               PICTURE S9(6).
           05  EDITTING-FIELDS.
               10  XO-72YYZ                PICTURE Z.ZZZ.ZZZ,ZZ.
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
           SET NOT-I-04                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  INNPUT-PROCESS
               SET INNPUT-PROCESS-OFF      TO TRUE
               SET INNPUT-READ             TO TRUE
           END-IF
 
           IF  INNPUT-READ
               PERFORM INNPUT-GET
               SET INNPUT-READ-OFF         TO TRUE
               IF  NOT INNPUT-EOF
                   PERFORM INNPUT-MATCH-SET
               END-IF
           END-IF
 
           IF  INNPUT2-PROCESS
               SET INNPUT2-PROCESS-OFF     TO TRUE
               SET INNPUT2-READ            TO TRUE
           END-IF
 
           IF  INNPUT2-READ
               PERFORM INNPUT2-GET
               SET INNPUT2-READ-OFF        TO TRUE
               IF  NOT INNPUT2-EOF
                   PERFORM INNPUT2-MATCH-SET
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
 
           IF  INNPUT-PROCESS
               PERFORM INNPUT-IDSET
           END-IF
 
           IF  INNPUT2-PROCESS
               PERFORM INNPUT2-IDSET
           END-IF
 
           IF  INNPUT-PROCESS
               PERFORM INNPUT-CHK-LEVEL
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
 
           IF  INNPUT-PROCESS
               PERFORM INNPUT-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  INNPUT-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-30                    TO TRUE
           IF  KVITT = '1'
               SET I-30                    TO TRUE
           END-IF
           IF  (NOT-I-30)
               GO TO SLUTT-T
           END-IF
           IF  (I-L3 AND I-30)
               PERFORM RBSRUT-S
           END-IF
           MOVE KEY13                      TO PRISMAS-KEY1
           READ PRISMAS RECORD KEY IS PRISMAS-KEY1
           INVALID KEY
               SET I-14                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-14                TO TRUE
               PERFORM PRISMAS-IDSET
           END-READ
           IF  (I-14)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-20                    TO TRUE
           IF  OPPDAT = 'J'
               SET I-20                    TO TRUE
           END-IF
           SET NOT-I-25                    TO TRUE
           IF  OPPDAT = 'F'
               SET I-25                    TO TRUE
           END-IF
      *          TYPE      COMP "1"                      12
      *  12      UTGÅR     COMP "U"                      15
      *          LEVPRI    COMP "J"                      16
           MOVE FIRMA                      TO KEY-X (1:3)
           MOVE EDBNR                      TO KEY-X (4:7)
           SET NOT-I-13                    TO TRUE
           MOVE KEY-X                      TO VAREMAS-KEY1
           READ VAREMAS RECORD KEY IS VAREMAS-KEY1
           INVALID KEY
               SET I-11                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-11                TO TRUE
               PERFORM VAREMAS-FLDSET
               PERFORM VAREMAS-IDSET
           END-READ
           IF  (NOT-I-11)
               SET NOT-I-13                TO TRUE
               IF  NYSVS > VPRIS
                   SET I-13                TO TRUE
               END-IF
      *  11                SETON                         86
           END-IF
           ADD NYSVS TO ZERO           GIVING NSVS
           ADD UTSALG TO ZERO          GIVING NUTP
           ADD NYLEVP TO ZERO          GIVING NLEVP
           ADD ENDRES TO ZERO          GIVING EDATO.
 
       SLUTT-T.
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           CONTINUE.
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE ' '                        TO BBEST
           MOVE 'PRI02'                    TO LONR
           MOVE FIRMA                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'PRI320  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF.
      ******************************************************
 
       INNPUT-GET SECTION.
       INNPUT-GET-P.
           IF  INNPUT-EOF-OFF
               READ INNPUT
               AT END
                   SET INNPUT-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INNPUT-FLDSET SECTION.
       INNPUT-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INNPUT-IO-AREA (1:13)  TO KEY13 (1:13)
               MOVE INNPUT-IO-AREA (1:1)   TO RA (1:1)
               MOVE INNPUT-IO-AREA (2:8)   TO MERG (1:8)
               MOVE INNPUT-IO-AREA (2:3)   TO FIRMA (1:3)
               MOVE INNPUT-IO-AREA (5:5)   TO REFNR (1:5)
               MOVE INNPUT-IO-AREA (10:4)  TO SEQ (1:4)
               MOVE INNPUT-IO-AREA (14:7)  TO EDBNR (1:7)
               MOVE INNPUT-IO-AREA (26:5)  TO NYSVS-IO
               MOVE INNPUT-IO-AREA (36:5)  TO UTSALG-IO
               MOVE INNPUT-IO-AREA (46:5)  TO NYLEVP-IO
               MOVE INNPUT-IO-AREA (51:4)  TO ENDRES-IO
               MOVE INNPUT-IO-AREA (73:1)  TO KVITT (1:1)
               MOVE INNPUT-IO-AREA (74:4)  TO TERM (1:4)
               MOVE INNPUT-IO-AREA (78:2)  TO REFPER (1:2)
               MOVE INNPUT-IO-AREA (80:1)  TO OPPDAT (1:1)
           END-EVALUATE.
 
       INNPUT-IDSET SECTION.
       INNPUT-IDSET-P.
           SET I-01                        TO TRUE.
 
       INNPUT-CHK-LEVEL SECTION.
       INNPUT-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO INNPUT-LEVEL-01
               MOVE INNPUT-IO-AREA (2:3)   TO INNPUT-01-L3-FIRMA
               MOVE INNPUT-IO-AREA (78:2)  TO INNPUT-01-L2-REFPER
               MOVE INNPUT-IO-AREA (5:5)   TO INNPUT-01-L1-REFNR
               IF  INNPUT-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INNPUT-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  INNPUT-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INNPUT-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INNPUT-01-L3          TO THE-PRIOR-L3
               MOVE  INNPUT-01-L2          TO THE-PRIOR-L2
               MOVE  INNPUT-01-L1          TO THE-PRIOR-L1
               SET INNPUT-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       INNPUT-MATCH-SET SECTION.
       INNPUT-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE INNPUT-IO-AREA (2:8)   TO INNPUT-M-01-M1-MERG
           END-EVALUATE.
 
       INNPUT2-GET SECTION.
       INNPUT2-GET-P.
           IF  INNPUT2-EOF-OFF
               READ INNPUT2
               AT END
                   SET INNPUT2-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INNPUT2-IDSET SECTION.
       INNPUT2-IDSET-P.
           SET I-04                        TO TRUE.
 
       INNPUT2-MATCH-SET SECTION.
       INNPUT2-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE INNPUT2-IO-AREA (2:8)  TO INNPUT2-M-04-M1-MERG
           END-EVALUATE.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (13:3) TO ALFA (1:3)
               MOVE VAREMAS-IO-AREA (16:20) TO ARTNR (1:20)
               MOVE VAREMAS-IO-AREA (75:9) TO VPRIS-IO
               INSPECT VPRIS-IO REPLACING ALL ' ' BY '0'
               MOVE VAREMAS-IO-AREA (95:1) TO PT (1:1)
               MOVE VAREMAS-IO-AREA (118:5) TO VGR (1:5)
               MOVE VAREMAS-IO-AREA (118:1) TO AVD (1:1)
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-02                        TO TRUE.
 
       PRISMAS-IDSET SECTION.
       PRISMAS-IDSET-P.
           SET I-03                        TO TRUE.
 
       PRINTF-PRINT-LINE SECTION.
       PRINTF-PRINT-LINE-P.
           IF  PRINTF-BEFORE-SKIP > 0
               PERFORM PRINTF-SKIP-BEFORE
           END-IF
           IF  PRINTF-BEFORE-SPACE > 0
               PERFORM PRINTF-SPACE-BEFORE
               IF  PRINTF-AFTER-SKIP > 0
                   PERFORM PRINTF-SKIP-AFTER
               END-IF
               IF  PRINTF-AFTER-SPACE > 0
                   PERFORM PRINTF-SPACE-AFTER
               END-IF
           ELSE
               IF  PRINTF-AFTER-SKIP > 0
                   PERFORM PRINTF-SKIP-AFTER
               END-IF
               PERFORM PRINTF-SPACE-AFTER
           END-IF
           IF  PRINTF-LINE-COUNT NOT < PRINTF-MAX-LINES
               SET I-OV                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OV            TO TRUE
               END-IF
           END-IF.
 
       PRINTF-SKIP-BEFORE SECTION.
       PRINTF-SKIP-BEFORE-P.
           WRITE PRINTF-IO-PRINT        AFTER ADVANCING PAGE
           MOVE 1                          TO PRINTF-LINE-COUNT
           MOVE 0                          TO PRINTF-BEFORE-SKIP
           INITIALIZE PRINTF-IO-AREA.
 
       PRINTF-SPACE-BEFORE SECTION.
       PRINTF-SPACE-BEFORE-P.
           WRITE PRINTF-IO-PRINT        AFTER PRINTF-BEFORE-SPACE LINES
           ADD PRINTF-BEFORE-SPACE         TO PRINTF-LINE-COUNT
           MOVE SPACES TO PRINTF-IO-AREA
           INITIALIZE PRINTF-IO-AREA
           MOVE 0                          TO PRINTF-BEFORE-SPACE.
 
       PRINTF-SKIP-AFTER SECTION.
       PRINTF-SKIP-AFTER-P.
           WRITE PRINTF-IO-PRINT       BEFORE ADVANCING PAGE
           MOVE 1                          TO PRINTF-LINE-COUNT
           MOVE 0                          TO PRINTF-AFTER-SKIP
           INITIALIZE PRINTF-IO-AREA.
 
       PRINTF-SPACE-AFTER SECTION.
       PRINTF-SPACE-AFTER-P.
           WRITE PRINTF-IO-PRINT       BEFORE PRINTF-AFTER-SPACE LINES
           ADD PRINTF-AFTER-SPACE          TO PRINTF-LINE-COUNT
           INITIALIZE PRINTF-IO-AREA
           MOVE 0                          TO PRINTF-AFTER-SPACE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  INNPUT-EOF
               MOVE HIGH-VALUES            TO INNPUT-MC
                                              INNPUT-MP
           END-IF
           IF  INNPUT2-EOF
               MOVE HIGH-VALUES            TO INNPUT2-MC
                                              INNPUT2-MP
           END-IF
           IF  INNPUT-MC < INNPUT-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  INNPUT2-MC < INNPUT2-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  INNPUT-MC < INNPUT2-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET INNPUT-PROCESS      TO TRUE
                   MOVE INNPUT-MC          TO INNPUT-MP
                   IF  INNPUT-MC = INNPUT2-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  INNPUT2-MC < INNPUT-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET INNPUT2-PROCESS     TO TRUE
                   MOVE INNPUT2-MC         TO INNPUT2-MP
                   IF  INNPUT2-MC = INNPUT-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  INNPUT-MC = INNPUT2-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET INNPUT-PROCESS      TO TRUE
                   MOVE INNPUT-MC          TO INNPUT-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND NOT-I-14 AND I-30)
           AND (NOT-I-MR AND NOT-I-86)
               MOVE ' '                    TO PRISMAS-IO-AREA (73:1)
               REWRITE PRISMAS-IO-AREA
                   INVALID KEY
                       DISPLAY 'Bad REWRITE - file = PRISMAS'
               END-REWRITE
           END-IF
           IF  (I-01 AND NOT-I-14 AND I-30)
           AND (NOT-I-86)
      *                      L1REFNR      5
               MOVE SPACES TO PRINTF-IO-AREA
               INITIALIZE PRINTF-IO-AREA
               MOVE REFNR                  TO PRINTF-IO-AREA (1:5)
               MOVE VGR                    TO PRINTF-IO-AREA (10:5)
               MOVE ALFA                   TO PRINTF-IO-AREA (19:3)
               MOVE ARTNR                  TO PRINTF-IO-AREA (24:20)
               MOVE NSVS                   TO XO-72YYZ
               MOVE XO-72YYZ               TO PRINTF-IO-AREA (46:12)
               MOVE NUTP                   TO XO-72YYZ
               MOVE XO-72YYZ               TO PRINTF-IO-AREA (60:12)
               MOVE NLEVP                  TO XO-72YYZ
               MOVE XO-72YYZ               TO PRINTF-IO-AREA (74:12)
               MOVE PT                     TO PRINTF-IO-AREA (87:1)
               IF  (NOT-I-20)
                   MOVE EDATO              TO EDIT-DATE
                   MOVE EDIT-DATE (7:8)    TO PRINTF-IO-AREA (91:8)
               END-IF
               IF  (I-20)
                   MOVE 'ER OPPDAT.'       TO PRINTF-IO-AREA (90:10)
               END-IF
               IF  (I-25)
                   MOVE 'UGYLDIG ENDRING,- AVIST' TO PRINTF-IO-AREA
                                                              (102:23)
               END-IF
               IF  (I-MR)
                   MOVE 'IKKE AVSLUTTET M/STREK ' TO PRINTF-IO-AREA
                                                              (102:23)
      *                   12N15         124 "KORR AV PRISFILE -DATO "
      *                   12 15         124 "KORR AV PRISFILE -UTGÅR"
               END-IF
               IF  (I-13)
                   MOVE 'OBS.SVS'          TO PRINTF-IO-AREA (126:7)
               END-IF
               MOVE 1                      TO PRINTF-AFTER-SPACE
               PERFORM PRINTF-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L2 AND I-30 AND NOT-I-86)
               MOVE SPACES TO PRINTF-IO-AREA
               INITIALIZE PRINTF-IO-AREA
               MOVE FINAVN                 TO PRINTF-IO-AREA (1:30)
               MOVE 'AVD.NR='              TO PRINTF-IO-AREA (34:7)
               MOVE AVD                    TO PRINTF-IO-AREA (41:1)
               MOVE '**** DAGENS PUNCHEDE' TO PRINTF-IO-AREA (45:20)
               MOVE 'PRISENDRINGER ***'    TO PRINTF-IO-AREA (67:17)
               MOVE 'DATO ='               TO PRINTF-IO-AREA (90:6)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO PRINTF-IO-AREA (97:8)
               MOVE 'REFERANSE-PERSON ='   TO PRINTF-IO-AREA (109:18)
               MOVE REFPER                 TO PRINTF-IO-AREA (128:2)
               MOVE 01                     TO PRINTF-BEFORE-SKIP
               MOVE 1                      TO PRINTF-AFTER-SPACE
               PERFORM PRINTF-PRINT-LINE
               MOVE SPACES TO PRINTF-IO-AREA
               INITIALIZE PRINTF-IO-AREA
               MOVE 'NY'                   TO PRINTF-IO-AREA (46:2)
               MOVE 'NY'                   TO PRINTF-IO-AREA (62:2)
               MOVE 'NY'                   TO PRINTF-IO-AREA (74:2)
               MOVE 'P'                    TO PRINTF-IO-AREA (87:1)
               MOVE 1                      TO PRINTF-AFTER-SPACE
               PERFORM PRINTF-PRINT-LINE
               MOVE SPACES TO PRINTF-IO-AREA
               INITIALIZE PRINTF-IO-AREA
               MOVE 'REFNR    VGR      ALFA' TO PRINTF-IO-AREA (1:22)
               MOVE 'ARIKKELNR '           TO PRINTF-IO-AREA (24:10)
               MOVE 'SELVKOSTPRIS'         TO PRINTF-IO-AREA (46:12)
               MOVE 'UTSALGPRIS'           TO PRINTF-IO-AREA (62:10)
               MOVE 'LEV.DØR PRIS'         TO PRINTF-IO-AREA (74:12)
               MOVE 'T'                    TO PRINTF-IO-AREA (87:1)
               MOVE 'DATO ENDRES'          TO PRINTF-IO-AREA (89:11)
               MOVE 'MERKNADER'            TO PRINTF-IO-AREA (105:9)
               MOVE 1                      TO PRINTF-AFTER-SPACE
               PERFORM PRINTF-PRINT-LINE
               MOVE SPACES TO PRINTF-IO-AREA
               INITIALIZE PRINTF-IO-AREA
               MOVE '------------------------' TO PRINTF-IO-AREA (1:24)
               MOVE '------------------------' TO PRINTF-IO-AREA
                                                               (25:24)
               MOVE '------------------------' TO PRINTF-IO-AREA
                                                               (49:24)
               MOVE '------------------------' TO PRINTF-IO-AREA
                                                               (73:24)
               MOVE '------------------------' TO PRINTF-IO-AREA
                                                               (97:24)
               MOVE '--------------'       TO PRINTF-IO-AREA (119:14)
               MOVE 2                      TO PRINTF-AFTER-SPACE
               PERFORM PRINTF-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OV AND NOT-I-L2 AND I-30)
           AND (NOT-I-86)
               MOVE SPACES TO PRINTF-IO-AREA
               INITIALIZE PRINTF-IO-AREA
               MOVE FINAVN                 TO PRINTF-IO-AREA (1:30)
               MOVE 'AVD.NR='              TO PRINTF-IO-AREA (34:7)
               MOVE AVD                    TO PRINTF-IO-AREA (41:1)
               MOVE '**** DAGENS PUNCHEDE' TO PRINTF-IO-AREA (45:20)
               MOVE 'PRISENDRINGER ***'    TO PRINTF-IO-AREA (67:17)
               MOVE 'DATO ='               TO PRINTF-IO-AREA (90:6)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO PRINTF-IO-AREA (97:8)
               MOVE 'REFERANSE-PERSON ='   TO PRINTF-IO-AREA (109:18)
               MOVE REFPER                 TO PRINTF-IO-AREA (128:2)
               MOVE 01                     TO PRINTF-BEFORE-SKIP
               MOVE 1                      TO PRINTF-AFTER-SPACE
               PERFORM PRINTF-PRINT-LINE
               MOVE SPACES TO PRINTF-IO-AREA
               INITIALIZE PRINTF-IO-AREA
               MOVE 'NY'                   TO PRINTF-IO-AREA (46:2)
               MOVE 'NY'                   TO PRINTF-IO-AREA (62:2)
               MOVE 'NY'                   TO PRINTF-IO-AREA (74:2)
               MOVE 'P'                    TO PRINTF-IO-AREA (87:1)
               MOVE 1                      TO PRINTF-AFTER-SPACE
               PERFORM PRINTF-PRINT-LINE
               MOVE SPACES TO PRINTF-IO-AREA
               INITIALIZE PRINTF-IO-AREA
               MOVE 'REFNR    VGR      ALFA' TO PRINTF-IO-AREA (1:22)
               MOVE 'ARIKKELNR '           TO PRINTF-IO-AREA (24:10)
               MOVE 'SELVKOSTPRIS'         TO PRINTF-IO-AREA (46:12)
               MOVE 'UTSALGPRIS'           TO PRINTF-IO-AREA (62:10)
               MOVE 'LEV.DØR PRIS'         TO PRINTF-IO-AREA (74:12)
               MOVE 'T'                    TO PRINTF-IO-AREA (87:1)
               MOVE 'DATO ENDRES'          TO PRINTF-IO-AREA (89:11)
               MOVE 'MERKNADER'            TO PRINTF-IO-AREA (105:9)
               MOVE 1                      TO PRINTF-AFTER-SPACE
               PERFORM PRINTF-PRINT-LINE
               MOVE SPACES TO PRINTF-IO-AREA
               INITIALIZE PRINTF-IO-AREA
               MOVE '------------------------' TO PRINTF-IO-AREA (1:24)
               MOVE '------------------------' TO PRINTF-IO-AREA
                                                               (25:24)
               MOVE '------------------------' TO PRINTF-IO-AREA
                                                               (49:24)
               MOVE '------------------------' TO PRINTF-IO-AREA
                                                               (73:24)
               MOVE '------------------------' TO PRINTF-IO-AREA
                                                               (97:24)
               MOVE '--------------'       TO PRINTF-IO-AREA (119:14)
               MOVE 2                      TO PRINTF-AFTER-SPACE
               PERFORM PRINTF-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND NOT-I-86)
               MOVE 1                      TO PRINTF-AFTER-SPACE
               PERFORM PRINTF-PRINT-LINE
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
           SET INNPUT-LEVEL-INIT           TO TRUE
           INITIALIZE INNPUT-DATA-FIELDS
           SET INNPUT-EOF-OFF              TO TRUE
           SET INNPUT-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO INNPUT-MC
                                              INNPUT-MP
           OPEN INPUT INNPUT
           SET INNPUT2-EOF-OFF             TO TRUE
           SET INNPUT2-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO INNPUT2-MC
                                              INNPUT2-MP
           OPEN INPUT INNPUT2
           INITIALIZE VAREMAS-DATA-FIELDS
           OPEN INPUT VAREMAS
           OPEN I-O PRISMAS
           OPEN OUTPUT PRINTF
           INITIALIZE PRINTF-IO-AREA
           INITIALIZE PRINTF-DATA-FIELDS
           MOVE 57                         TO PRINTF-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INNPUT
           CLOSE INNPUT2
           CLOSE VAREMAS
           CLOSE PRISMAS
           IF PRINTF-IO-AREA NOT = SPACES
             WRITE PRINTF-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO PRINTF-IO-AREA
           END-IF
           CLOSE PRINTF.
 
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
