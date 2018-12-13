       IDENTIFICATION DIVISION.
       PROGRAM-ID. OPS030R.
      **********************************************  Z-WIN-RPG2      *
      ***********************************************************
      *  PROGRAMMET DANNER OPPSLAGSARKIV UTFRA VAREARKIVET, OG  *
      *  UTFRA ALTERNATIVE OPPSLAGSFILE, OG KUNDENR-ORDRENR.    *
      *  VED LIKE KEY PÅ SAMME FILE BLIR SISTE LAGT UT.         *
      *  VED LIKE KEY MELLOM FILENE BLIR ALTERNATIV LAGT UT.    *
      *  28/10-92 PROGRAMMET ER NÅ RETTET SLIK ATT HVIST LIK KEY*
      *  MELLOM FILENE BLIR OPPSLAGSNR. FRA VAREARKIV LAGT UT.  *
      * 02.02.04 SLETTER RECORD PÅ FIRMA MED SLETTEMERKE.       *
      ***********************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: OPS030.rpg
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
           SELECT MINFILE
               ASSIGN TO UT-S-MINFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS MINFILE-STATUS.
           SELECT RELREC
               ASSIGN TO UT-S-RELREC
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RELREC-STATUS.
           SELECT ALTONR
               ASSIGN TO UT-S-ALTONR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ALTONR-STATUS.
           SELECT VAREMAS
               ASSIGN TO VAREMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAREMAS-STATUS
               RECORD KEY IS VAREMAS-KEY1.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT OPPSMAS
               ASSIGN TO OPPSMAS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OPPSMAS-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD MINFILE
               BLOCK CONTAINS 360
               RECORD CONTAINS 36.
       01  MINFILE-IO-AREA-2.
           05  MINFILE-IO-AREA-X           PICTURE X(36).
       FD RELREC
               BLOCK CONTAINS 300
               RECORD CONTAINS 30.
       01  RELREC-IO-AREA.
           05  RELREC-IO-AREA-X            PICTURE X(30).
       FD ALTONR
               BLOCK CONTAINS 300
               RECORD CONTAINS 30.
       01  ALTONR-IO-AREA-2.
           05  ALTONR-IO-AREA-X            PICTURE X(30).
       FD VAREMAS
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X.
               10  VAREMAS-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD OPPSMAS
               RECORD CONTAINS 30.
       01  OPPSMAS-IO-AREA.
           05  OPPSMAS-IO-AREA-X           PICTURE X(30).
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
           10  MINFILE-STATUS              PICTURE 99 VALUE 0.
           10  RELREC-STATUS               PICTURE 99 VALUE 0.
           10  ALTONR-STATUS               PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  OPPSMAS-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  MINFILE-EOF-OFF         VALUE '0'.
               88  MINFILE-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  MINFILE-READ-OFF        VALUE '0'.
               88  MINFILE-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  MINFILE-PROCESS-OFF     VALUE '0'.
               88  MINFILE-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  MINFILE-LEVEL-INIT-OFF  VALUE '0'.
               88  MINFILE-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  MINFILE-AHEAD-EOF-OFF   VALUE '0'.
               88  MINFILE-AHEAD-EOF       VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  MINFILE-AHEAD-READ-OFF  VALUE '0'.
               88  MINFILE-AHEAD-READ      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RELREC-EOF-OFF          VALUE '0'.
               88  RELREC-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RELREC-READ-OFF         VALUE '0'.
               88  RELREC-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RELREC-PROCESS-OFF      VALUE '0'.
               88  RELREC-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ALTONR-EOF-OFF          VALUE '0'.
               88  ALTONR-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ALTONR-READ-OFF         VALUE '0'.
               88  ALTONR-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ALTONR-PROCESS-OFF      VALUE '0'.
               88  ALTONR-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  ALTONR-LEVEL-INIT-OFF   VALUE '0'.
               88  ALTONR-LEVEL-INIT       VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ALTONR-AHEAD-EOF-OFF    VALUE '0'.
               88  ALTONR-AHEAD-EOF        VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ALTONR-AHEAD-READ-OFF   VALUE '0'.
               88  ALTONR-AHEAD-READ       VALUE '1'.
           05  VAREMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
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
           05  MINFILE-LEVEL-01.
               10  MINFILE-01-L2.
                   15  MINFILE-01-L2-FIRMA PICTURE X(3).
               10  MINFILE-01-L1.
                   15  MINFILE-01-L1-OKEY  PICTURE X(21).
           05  MINFILE-DATA-FIELDS.
               10  OKEY                    PICTURE X(21).
               10  FIRMA                   PICTURE X(3).
               10  OPSNR                   PICTURE X(18).
               10  EDBNR                   PICTURE X(7).
               10  E1                      PICTURE X(1).
               10  NXTKEY                  PICTURE X(21).
           05  MINFILE-MP                  PICTURE X(21).
           05  MINFILE-MC                  PICTURE X(21).
           05  MINFILE-M-01            REDEFINES MINFILE-MC.
               10  MINFILE-M-01-M1.
                   15  MINFILE-M-01-M1-OKEY-G.
                       20  MINFILE-M-01-M1-OKEY PICTURE X(21).
           05  RELREC-DATA-FIELDS.
               10  RKEY                    PICTURE X(21).
               10  RREC                    PICTURE X(30).
           05  RELREC-MP                   PICTURE X(21).
           05  RELREC-MC                   PICTURE X(21).
           05  RELREC-M-04             REDEFINES RELREC-MC.
               10  RELREC-M-04-M1.
                   15  RELREC-M-04-M1-RKEY-G.
                       20  RELREC-M-04-M1-RKEY PICTURE X(21).
           05  ALTONR-LEVEL-02.
               10  ALTONR-02-L2.
                   15  ALTONR-02-L2-FIRMA  PICTURE X(3).
               10  ALTONR-02-L1.
                   15  ALTONR-02-L1-OKEY   PICTURE X(21).
           05  ALTONR-DATA-FIELDS.
               10  RECL                    PICTURE X(30).
               10  EL1                     PICTURE X(1).
               10  NEXTK                   PICTURE X(21).
           05  ALTONR-MP                   PICTURE X(21).
           05  ALTONR-MC                   PICTURE X(21).
           05  ALTONR-M-02             REDEFINES ALTONR-MC.
               10  ALTONR-M-02-M1.
                   15  ALTONR-M-02-M1-OKEY-G.
                       20  ALTONR-M-02-M1-OKEY PICTURE X(21).
           05  VAREMAS-DATA-FIELDS.
               10  VALFA                   PICTURE X(3).
               10  VARTNR                  PICTURE X(20).
               10  VNAVN                   PICTURE X(30).
           05  FIRMAF-DATA-FIELDS.
               10  FIRMSL                  PICTURE X(1).
      *
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(21).
           05  TEMPORARY-FIELDS.
               10  ANT01-IO.
                   15  ANT01               PICTURE S9(8).
               10  ANT02-IO.
                   15  ANT02               PICTURE S9(8).
               10  ANT04-IO.
                   15  ANT04               PICTURE S9(8).
               10  ANT01S-IO.
                   15  ANT01S              PICTURE S9(8).
               10  ANT02S-IO.
                   15  ANT02S              PICTURE S9(8).
               10  ANT04S-IO.
                   15  ANT04S              PICTURE S9(8).
               10  VARKEY                  PICTURE X(10).
           05  EDITTING-FIELDS.
               10  XO-40YNZ                PICTURE ZZZZ.
               10  XO-80YY9                PICTURE ZZ.ZZZ.ZZ9.
           05  PREDEFINED-FIELDS.
               10  PAGE0                   PICTURE S9(4) USAGE BINARY.
           05  MINFILE-IO-AREA.
               10  FILLER                  PICTURE X(36).
           05  ALTONR-IO-AREA.
               10  FILLER                  PICTURE X(30).
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
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-05                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  MINFILE-PROCESS
               SET MINFILE-PROCESS-OFF     TO TRUE
               SET MINFILE-READ            TO TRUE
           END-IF
 
           IF  MINFILE-READ
               PERFORM MINFILE-GET
               SET MINFILE-READ-OFF        TO TRUE
               IF  NOT MINFILE-EOF
                   PERFORM MINFILE-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM MINFILE-MATCH-SET
               END-IF
           END-IF
 
           IF  RELREC-PROCESS
               SET RELREC-PROCESS-OFF      TO TRUE
               SET RELREC-READ             TO TRUE
           END-IF
 
           IF  RELREC-READ
               PERFORM RELREC-GET
               SET RELREC-READ-OFF         TO TRUE
               IF  NOT RELREC-EOF
                   PERFORM RELREC-MATCH-SET
               END-IF
           END-IF
 
           IF  ALTONR-PROCESS
               SET ALTONR-PROCESS-OFF      TO TRUE
               SET ALTONR-READ             TO TRUE
           END-IF
 
           IF  ALTONR-READ
               PERFORM ALTONR-GET
               SET ALTONR-READ-OFF         TO TRUE
               IF  NOT ALTONR-EOF
                   PERFORM ALTONR-MATCH-SET
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
 
           IF  MINFILE-PROCESS
               PERFORM MINFILE-IDSET
           END-IF
 
           IF  RELREC-PROCESS
               PERFORM RELREC-IDSET
           END-IF
 
           IF  ALTONR-PROCESS
               PERFORM ALTONR-IDSET
           END-IF
 
           IF  MINFILE-PROCESS
               PERFORM MINFILE-CHK-LEVEL
           END-IF
 
           IF  ALTONR-PROCESS
               PERFORM ALTONR-CHK-LEVEL
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
 
           IF  MINFILE-PROCESS
               PERFORM MINFILE-FLDSET
           END-IF
 
           IF  RELREC-PROCESS
               PERFORM RELREC-FLDSET
           END-IF
 
           IF  ALTONR-PROCESS
               PERFORM ALTONR-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  MINFILE-PROCESS
           OR  ALTONR-PROCESS
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
               SET NOT-I-35                TO TRUE
               SET NOT-I-95                TO TRUE
               PERFORM FISLET-S
      *****************************************************************
      * RUTINE FOR TELLING AV RECORDS.                                *
      *****************************************************************
           END-IF
           IF  (I-01)
               ADD 1                       TO ANT01
           END-IF
           IF  (I-02)
               ADD 1                       TO ANT02
           END-IF
           IF  (I-04)
               ADD 1                       TO ANT04
           END-IF
           IF  (I-01 AND I-95)
               ADD 1                       TO ANT01S
           END-IF
           IF  (I-02 AND I-95)
               ADD 1                       TO ANT02S
           END-IF
           IF  (I-04 AND I-95)
               ADD 1                       TO ANT04S
      *****************************************************************
           END-IF
           IF  (I-L1)
               SET NOT-I-50                TO TRUE
           END-IF
           SET NOT-I-40                    TO TRUE
           IF  (I-61)
               SET NOT-I-60                TO TRUE
               SET NOT-I-61                TO TRUE
           END-IF
           IF  (I-04)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               SET NOT-I-90                TO TRUE
               IF  E1 = '9'
                   SET I-90                TO TRUE
               END-IF
               SET NOT-I-10                TO TRUE
               IF  OKEY = NXTKEY
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-02)
               SET NOT-I-90                TO TRUE
               IF  EL1 = '9'
                   SET I-90                TO TRUE
               END-IF
               SET NOT-I-10                TO TRUE
               IF  OKEY = NEXTK
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-10)
               SET I-40                    TO TRUE
               SET I-50                    TO TRUE
           END-IF
           IF  (I-02 AND I-MR)
               SET I-40                    TO TRUE
           END-IF
           IF  (I-L2)
               SET I-60                    TO TRUE
           END-IF
           IF  (I-50 AND I-60 AND NOT-I-90)
               SET I-61                    TO TRUE
           END-IF
           IF  (I-61)
               PERFORM RBSRUT-S
           END-IF
           IF  (I-50 AND NOT-I-90 AND NOT-I-35)
               PERFORM VARRUT-S
           END-IF.
 
       SLUTT-T.
      ******************************************************
      *    SUBRUTINE FOR OPPSLAG PÅ VAREARKIV.             *
      ******************************************************
           CONTINUE.
 
       VARRUT-S SECTION.
       VARRUT-S-P.
           MOVE FIRMA                      TO VARKEY (1:3)
           MOVE EDBNR                      TO VARKEY (4:7)
           MOVE VARKEY                     TO VAREMAS-KEY1
           READ VAREMAS RECORD KEY IS VAREMAS-KEY1
           INVALID KEY
               SET I-44                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-44                TO TRUE
               PERFORM VAREMAS-FLDSET
               PERFORM VAREMAS-IDSET
           END-READ.
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE ' '                        TO BBEST
           MOVE 'VAR12'                    TO LONR
           MOVE FIRMA                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'OPS030  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF
           IF  (I-86)
               SET I-35                    TO TRUE
           END-IF.
      ******************************************************
      *    SUBRUTINE FOR SLETTING AV HELE FIRMA            *
      ******************************************************
 
       FISLET-S SECTION.
       FISLET-S-P.
           MOVE FIRMA                      TO FIRMAF-KEY1
           READ FIRMAF RECORD KEY IS FIRMAF-KEY1
           INVALID KEY
               SET I-96                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-96                TO TRUE
               PERFORM FIRMAF-FLDSET
               PERFORM FIRMAF-IDSET
           END-READ
           IF  (NOT-I-96)
               SET NOT-I-95                TO TRUE
               IF  FIRMSL = 'S'
                   SET I-95                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-96 AND NOT-I-95)
               SET NOT-I-95                TO TRUE
               IF  FIRMSL = 'R'
                   SET I-95                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-95)
               SET NOT-I-95                TO TRUE
               IF  FIRMA NOT > '000'
                   SET I-95                TO TRUE
               END-IF
           END-IF.
      ******************************************************
 
       MINFILE-GET SECTION.
       MINFILE-GET-P.
           IF  MINFILE-EOF-OFF
               IF  MINFILE-AHEAD-EOF-OFF
                   IF  MINFILE-AHEAD-READ-OFF
                       SET MINFILE-AHEAD-READ TO TRUE
                       READ MINFILE
                       AT END
                           SET MINFILE-AHEAD-EOF TO TRUE
                           INITIALIZE MINFILE-IO-AREA-2
                       END-READ
                   END-IF
                   MOVE MINFILE-IO-AREA-2  TO MINFILE-IO-AREA
                   IF  MINFILE-AHEAD-EOF-OFF
                       READ MINFILE
                       AT END
                           SET MINFILE-AHEAD-EOF TO TRUE
                           INITIALIZE MINFILE-IO-AREA-2
                       END-READ
                   ELSE
                       SET MINFILE-EOF     TO TRUE
                       SUBTRACT 1        FROM LR-CHECK
                   END-IF
                   PERFORM MINFILE-AHEAD-FLDSET
               ELSE
                   SET MINFILE-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-IF
           END-IF.
 
       MINFILE-FLDSET SECTION.
       MINFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ( MINFILE-IO-AREA (1:1) = '1' )
               MOVE MINFILE-IO-AREA (2:21) TO OKEY (1:21)
               MOVE MINFILE-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE MINFILE-IO-AREA (5:18) TO OPSNR (1:18)
               MOVE MINFILE-IO-AREA (29:7) TO EDBNR (1:7)
               MOVE MINFILE-IO-AREA (29:1) TO E1 (1:1)
           END-EVALUATE.
 
       MINFILE-IDCHK SECTION.
       MINFILE-IDCHK-P.
           EVALUATE TRUE
           WHEN ( MINFILE-IO-AREA (1:1) = '1' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       MINFILE-IDSET SECTION.
       MINFILE-IDSET-P.
           EVALUATE TRUE
           WHEN ( MINFILE-IO-AREA (1:1) = '1' )
               SET I-01                    TO TRUE
           END-EVALUATE.
 
       MINFILE-CHK-LEVEL SECTION.
       MINFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( MINFILE-IO-AREA (1:1) = '1' )
               MOVE LOW-VALUES             TO MINFILE-LEVEL-01
               MOVE MINFILE-IO-AREA (2:3)  TO MINFILE-01-L2-FIRMA
               MOVE MINFILE-IO-AREA (2:21) TO MINFILE-01-L1-OKEY
               IF  MINFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  MINFILE-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  MINFILE-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  MINFILE-01-L2         TO THE-PRIOR-L2
               MOVE  MINFILE-01-L1         TO THE-PRIOR-L1
               SET MINFILE-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       MINFILE-MATCH-SET SECTION.
       MINFILE-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( MINFILE-IO-AREA (1:1) = '1' )
               MOVE MINFILE-IO-AREA (2:21) TO MINFILE-M-01-M1-OKEY
           END-EVALUATE.
 
       MINFILE-AHEAD-FLDSET SECTION.
       MINFILE-AHEAD-FLDSET-P.
           MOVE MINFILE-IO-AREA-2 (2:21)   TO NXTKEY (1:21).
 
       RELREC-GET SECTION.
       RELREC-GET-P.
           IF  RELREC-EOF-OFF
               READ RELREC
               AT END
                   SET RELREC-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RELREC-FLDSET SECTION.
       RELREC-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RELREC-IO-AREA (2:21)  TO RKEY (1:21)
               MOVE RELREC-IO-AREA (1:30)  TO RREC (1:30)
           END-EVALUATE.
 
       RELREC-IDSET SECTION.
       RELREC-IDSET-P.
           SET I-04                        TO TRUE.
 
       RELREC-MATCH-SET SECTION.
       RELREC-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE RELREC-IO-AREA (2:21)  TO RELREC-M-04-M1-RKEY
           END-EVALUATE.
 
       ALTONR-GET SECTION.
       ALTONR-GET-P.
           IF  ALTONR-EOF-OFF
               IF  ALTONR-AHEAD-EOF-OFF
                   IF  ALTONR-AHEAD-READ-OFF
                       SET ALTONR-AHEAD-READ TO TRUE
                       READ ALTONR
                       AT END
                           SET ALTONR-AHEAD-EOF TO TRUE
                           INITIALIZE ALTONR-IO-AREA-2
                       END-READ
                   END-IF
                   MOVE ALTONR-IO-AREA-2   TO ALTONR-IO-AREA
                   IF  ALTONR-AHEAD-EOF-OFF
                       READ ALTONR
                       AT END
                           SET ALTONR-AHEAD-EOF TO TRUE
                           INITIALIZE ALTONR-IO-AREA-2
                       END-READ
                   ELSE
                       SET ALTONR-EOF      TO TRUE
                       SUBTRACT 1        FROM LR-CHECK
                   END-IF
                   PERFORM ALTONR-AHEAD-FLDSET
               ELSE
                   SET ALTONR-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-IF
           END-IF.
 
       ALTONR-FLDSET SECTION.
       ALTONR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE ALTONR-IO-AREA (2:21)  TO OKEY (1:21)
               MOVE ALTONR-IO-AREA (2:3)   TO FIRMA (1:3)
               MOVE ALTONR-IO-AREA (5:18)  TO OPSNR (1:18)
               MOVE ALTONR-IO-AREA (23:7)  TO EDBNR (1:7)
               MOVE ALTONR-IO-AREA (1:30)  TO RECL (1:30)
               MOVE ALTONR-IO-AREA (23:1)  TO EL1 (1:1)
           END-EVALUATE.
 
       ALTONR-IDSET SECTION.
       ALTONR-IDSET-P.
           SET I-02                        TO TRUE.
 
       ALTONR-CHK-LEVEL SECTION.
       ALTONR-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO ALTONR-LEVEL-02
               MOVE ALTONR-IO-AREA (2:3)   TO ALTONR-02-L2-FIRMA
               MOVE ALTONR-IO-AREA (2:21)  TO ALTONR-02-L1-OKEY
               IF  ALTONR-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ALTONR-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  ALTONR-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ALTONR-02-L2          TO THE-PRIOR-L2
               MOVE  ALTONR-02-L1          TO THE-PRIOR-L1
               SET ALTONR-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       ALTONR-MATCH-SET SECTION.
       ALTONR-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE ALTONR-IO-AREA (2:21)  TO ALTONR-M-02-M1-OKEY
           END-EVALUATE.
 
       ALTONR-AHEAD-FLDSET SECTION.
       ALTONR-AHEAD-FLDSET-P.
           MOVE ALTONR-IO-AREA-2 (2:21)    TO NEXTK (1:21).
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (13:3) TO VALFA (1:3)
               MOVE VAREMAS-IO-AREA (16:20) TO VARTNR (1:20)
               MOVE VAREMAS-IO-AREA (36:30) TO VNAVN (1:30)
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-03                        TO TRUE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (8:30)  TO FINAVN (1:30)
               MOVE FIRMAF-IO-AREA (123:1) TO FIRMSL (1:1)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-05                        TO TRUE.
 
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
           IF  MINFILE-EOF
               MOVE HIGH-VALUES            TO MINFILE-MC
                                              MINFILE-MP
           END-IF
           IF  RELREC-EOF
               MOVE HIGH-VALUES            TO RELREC-MC
                                              RELREC-MP
           END-IF
           IF  ALTONR-EOF
               MOVE HIGH-VALUES            TO ALTONR-MC
                                              ALTONR-MP
           END-IF
           IF  MINFILE-MC < MINFILE-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  RELREC-MC < RELREC-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  ALTONR-MC < ALTONR-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  MINFILE-MC < RELREC-MC
            AND  MINFILE-MC < ALTONR-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET MINFILE-PROCESS     TO TRUE
                   MOVE MINFILE-MC         TO MINFILE-MP
                   IF  MINFILE-MC = RELREC-MP
                     OR  MINFILE-MC = ALTONR-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  RELREC-MC < MINFILE-MC
            AND  RELREC-MC < ALTONR-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET RELREC-PROCESS      TO TRUE
                   MOVE RELREC-MC          TO RELREC-MP
                   IF  RELREC-MC = MINFILE-MP
                     OR  RELREC-MC = ALTONR-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  ALTONR-MC < MINFILE-MC
            AND  ALTONR-MC < RELREC-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET ALTONR-PROCESS      TO TRUE
                   MOVE ALTONR-MC          TO ALTONR-MP
                   IF  ALTONR-MC = MINFILE-MP
                     OR  ALTONR-MC = RELREC-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  MINFILE-MC = RELREC-MC
             OR  MINFILE-MC = ALTONR-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET MINFILE-PROCESS     TO TRUE
                   MOVE MINFILE-MC         TO MINFILE-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           WHEN  RELREC-MC = ALTONR-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET RELREC-PROCESS      TO TRUE
                   MOVE RELREC-MC          TO RELREC-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-L1 AND I-50 AND NOT-I-90)
           AND (NOT-I-35)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ' '                    TO LISTE-IO-AREA (2:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-50 AND NOT-I-90 AND NOT-I-35)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               IF  (NOT-I-44)
                   MOVE VALFA              TO LISTE-IO-AREA (1:3)
               END-IF
               IF  (NOT-I-44)
                   MOVE VARTNR             TO LISTE-IO-AREA (6:20)
               END-IF
               IF  (NOT-I-44)
                   MOVE VNAVN              TO LISTE-IO-AREA (28:30)
               END-IF
               MOVE OPSNR                  TO LISTE-IO-AREA (61:18)
               MOVE EDBNR                  TO LISTE-IO-AREA (85:7)
               IF  (I-40)
                   MOVE 'OPPSLAGSNR. FORKASTET.' TO LISTE-IO-AREA
                                                               (94:22)
               END-IF
               IF  (NOT-I-40)
                   MOVE 'OPPSLAGSNR. BEHOLDT.  ' TO LISTE-IO-AREA
                                                               (94:22)
               END-IF
               IF  (I-02)
                   MOVE 'ALTERNATIVT NR.'  TO LISTE-IO-AREA (117:15)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-01 AND NOT-I-40 AND NOT-I-95)
               MOVE SPACES TO OPPSMAS-IO-AREA
               INITIALIZE OPPSMAS-IO-AREA
               MOVE '1'                    TO OPPSMAS-IO-AREA (1:1)
               MOVE OKEY                   TO OPPSMAS-IO-AREA (2:21)
               MOVE EDBNR                  TO OPPSMAS-IO-AREA (23:7)
               WRITE OPPSMAS-IO-AREA
           END-IF
           IF  (I-02 AND NOT-I-40 AND NOT-I-95)
               MOVE SPACES TO OPPSMAS-IO-AREA
               INITIALIZE OPPSMAS-IO-AREA
               MOVE RECL                   TO OPPSMAS-IO-AREA (1:30)
               WRITE OPPSMAS-IO-AREA
           END-IF
           IF  (I-04 AND NOT-I-95)
               MOVE SPACES TO OPPSMAS-IO-AREA
               INITIALIZE OPPSMAS-IO-AREA
               MOVE RREC                   TO OPPSMAS-IO-AREA (1:30)
               WRITE OPPSMAS-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-61 AND NOT-I-35)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE LOPNVN                 TO LISTE-IO-AREA (36:35)
               MOVE 'KJØREDATO'            TO LISTE-IO-AREA (93:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (103:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (113:4)
               IF  (I-61)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (117:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ALF'                  TO LISTE-IO-AREA (1:3)
               MOVE 'ARTIKKELNUMMER'       TO LISTE-IO-AREA (6:14)
               MOVE 'VARENAVN'             TO LISTE-IO-AREA (28:8)
               MOVE 'OPPSLAGSNUMMER'       TO LISTE-IO-AREA (61:14)
               MOVE 'EDB-NR.'              TO LISTE-IO-AREA (85:7)
               MOVE 'FEILMELDING'          TO LISTE-IO-AREA (94:11)
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
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE LOPNVN                 TO LISTE-IO-AREA (36:35)
               MOVE 'KJØREDATO'            TO LISTE-IO-AREA (93:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (103:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (113:4)
               IF  (I-61)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (117:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ALF'                  TO LISTE-IO-AREA (1:3)
               MOVE 'ARTIKKELNUMMER'       TO LISTE-IO-AREA (6:14)
               MOVE 'VARENAVN'             TO LISTE-IO-AREA (28:8)
               MOVE 'OPPSLAGSNUMMER'       TO LISTE-IO-AREA (61:14)
               MOVE 'EDB-NR.'              TO LISTE-IO-AREA (85:7)
               MOVE 'FEILMELDING'          TO LISTE-IO-AREA (94:11)
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
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALSUMMER VAREKEY.FILE' TO LISTE-IO-AREA (1:24)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (27:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL OPPSL.REC. LEST  ' TO LISTE-IO-AREA (1:24)
               MOVE ANT01                  TO XO-80YY9
               MOVE XO-80YY9               TO LISTE-IO-AREA (25:10)
               MOVE 'ANTALL FJERNET NÅ '   TO LISTE-IO-AREA (36:18)
               MOVE ANT01S                 TO XO-80YY9
               MOVE XO-80YY9               TO LISTE-IO-AREA (54:10)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL ALT.KEY REC.LEST ' TO LISTE-IO-AREA (1:24)
               MOVE ANT02                  TO XO-80YY9
               MOVE XO-80YY9               TO LISTE-IO-AREA (25:10)
               MOVE 'ANTALL FJERNET NÅ '   TO LISTE-IO-AREA (36:18)
               MOVE ANT02S                 TO XO-80YY9
               MOVE XO-80YY9               TO LISTE-IO-AREA (54:10)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL REL.RECORDS LEST ' TO LISTE-IO-AREA (1:24)
               MOVE ANT04                  TO XO-80YY9
               MOVE XO-80YY9               TO LISTE-IO-AREA (25:10)
               MOVE 'ANTALL FJERNET NÅ '   TO LISTE-IO-AREA (36:18)
               MOVE ANT04S                 TO XO-80YY9
               MOVE XO-80YY9               TO LISTE-IO-AREA (54:10)
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
           MOVE 3                          TO LR-CHECK
           SET MINFILE-LEVEL-INIT          TO TRUE
           SET MINFILE-AHEAD-EOF-OFF       TO TRUE
           SET MINFILE-AHEAD-READ-OFF      TO TRUE
           INITIALIZE MINFILE-DATA-FIELDS
           SET MINFILE-EOF-OFF             TO TRUE
           SET MINFILE-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO MINFILE-MC
                                              MINFILE-MP
           OPEN INPUT MINFILE
           INITIALIZE RELREC-DATA-FIELDS
           SET RELREC-EOF-OFF              TO TRUE
           SET RELREC-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO RELREC-MC
                                              RELREC-MP
           OPEN INPUT RELREC
           SET ALTONR-LEVEL-INIT           TO TRUE
           SET ALTONR-AHEAD-EOF-OFF        TO TRUE
           SET ALTONR-AHEAD-READ-OFF       TO TRUE
           INITIALIZE ALTONR-DATA-FIELDS
           SET ALTONR-EOF-OFF              TO TRUE
           SET ALTONR-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO ALTONR-MC
                                              ALTONR-MP
           OPEN INPUT ALTONR
           INITIALIZE VAREMAS-DATA-FIELDS
           OPEN INPUT VAREMAS
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           OPEN OUTPUT OPPSMAS
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE MINFILE
           CLOSE RELREC
           CLOSE ALTONR
           CLOSE VAREMAS
           CLOSE FIRMAF
           CLOSE OPPSMAS
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
