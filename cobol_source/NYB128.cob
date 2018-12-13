       IDENTIFICATION DIVISION.
       PROGRAM-ID. NYB128R.
      **********************************************  Z-WIN-RPG2   ****
      *   PROGRAM  NYB128                                                     *
      *   UTLISTING AV BESTILLINGARKIVET PR LEVERANDØR PURRELISTE             *
      *   U2 -VISER SELVKOST ISTEDET FOR PRIS UTENLANDSK MYNT,                *
      *       SAMT BEREGNER TOTALER PR LEVERANDØR OG PR FIRMA.                *
      *************************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: NYB128.rpg
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
           SELECT PARAM
               ASSIGN TO UT-S-PARAM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PARAM-STATUS.
           SELECT BESTM
               ASSIGN TO UT-S-BESTM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS BESTM-STATUS.
           SELECT KUNDEMA
               ASSIGN TO KUNDEMA
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMA-STATUS
               RECORD KEY IS KUNDEMA-KEY1.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD PARAM
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  PARAM-IO-AREA.
           05  PARAM-IO-AREA-X             PICTURE X(80).
       FD BESTM
               BLOCK CONTAINS 4080
               RECORD CONTAINS 136.
       01  BESTM-IO-AREA.
           05  BESTM-IO-AREA-X             PICTURE X(136).
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1            PICTURE X(9).
               10  FILLER                  PICTURE X(190).
      **************************************************************
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
           10  PARAM-STATUS                PICTURE 99 VALUE 0.
           10  BESTM-STATUS                PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-EOF-OFF           VALUE '0'.
               88  PARAM-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-READ-OFF          VALUE '0'.
               88  PARAM-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARAM-PROCESS-OFF       VALUE '0'.
               88  PARAM-PROCESS           VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BESTM-EOF-OFF           VALUE '0'.
               88  BESTM-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BESTM-READ-OFF          VALUE '0'.
               88  BESTM-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BESTM-PROCESS-OFF       VALUE '0'.
               88  BESTM-PROCESS           VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  BESTM-LEVEL-INIT-OFF    VALUE '0'.
               88  BESTM-LEVEL-INIT        VALUE '1'.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
      * * START RBS - PARAMETERKORT OVERSTYRING AV RBS-FILE  * *
           05  PARAM-DATA-FIELDS.
               10  PJOBN                   PICTURE X(8).
               10  PRKODE                  PICTURE X(1).
               10  PPERS                   PICTURE X(30).
               10  PANTX-IO.
                   15  PANTX               PICTURE S9(3).
               10  PETTB                   PICTURE X(40).
               10  PFORS                   PICTURE X(40).
               10  PMEMO                   PICTURE X(40).
      * * END   RBS - PARAMETERKORT OVERSTYRING AV RBS-FILE  * *
               10  PFIRMA                  PICTURE X(3).
               10  PRINTK                  PICTURE X(1).
           05  BESTM-LEVEL-01.
               10  BESTM-01-L4.
                   15  BESTM-01-L4-FIRMA   PICTURE X(3).
               10  BESTM-01-L3.
                   15  BESTM-01-L3-BNAVN   PICTURE X(9).
               10  BESTM-01-L2.
                   15  BESTM-01-L2-LEVR    PICTURE X(6).
               10  BESTM-01-L1.
                   15  BESTM-01-L1-BEST    PICTURE X(5).
           05  BESTM-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  BNAVN                   PICTURE X(9).
               10  BEST                    PICTURE X(5).
               10  POS-X-IO.
                   15  POS-X               PICTURE S9(4).
               10  AVD                     PICTURE X(1).
               10  LEVR                    PICTURE X(6).
               10  A-ELGR                  PICTURE X(2).
               10  MND                     PICTURE X(2).
               10  DAG                     PICTURE X(2).
               10  B                       PICTURE X(2).
               10  LEVA-ELGR-IO.
                   15  LEVA-ELGR           PICTURE S9(2).
               10  LEVUKE                  PICTURE X(2).
               10  FAB                     PICTURE X(1).
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  EDBNR-IO.
                   15  EDBNR               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  RAB1-IO.
                   15  RAB1                PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  RAB2-IO.
                   15  RAB2                PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  VT                      PICTURE X(1).
               10  UPRIS-IO.
                   15  UPRIS               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  BANT-IO.
                   15  BANT                PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  TANT-IO.
                   15  TANT                PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  TDATO                   PICTURE X(6).
               10  TA-ELGR                 PICTURE X(2).
               10  TMND                    PICTURE X(2).
               10  TDAG-IO.
                   15  TDAG                PICTURE S9(2).
               10  BKREFT                  PICTURE X(1).
               10  PUKE                    PICTURE X(2).
               10  SKOST-IO.
                   15  SKOST               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
           05  KUNDEMA-DATA-FIELDS.
               10  NAVN1                   PICTURE X(30).
               10  NAVN2                   PICTURE X(30).
               10  ADR                     PICTURE X(30).
               10  PSTED                   PICTURE X(15).
               10  PNR                     PICTURE X(4).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L4            PICTURE X(3).
               10  THE-PRIOR-L3            PICTURE X(9).
               10  THE-PRIOR-L2            PICTURE X(6).
               10  THE-PRIOR-L1            PICTURE X(5).
           05  TEMPORARY-FIELDS.
               10  LEVKEY                  PICTURE X(9).
               10  SUM-X-IO.
                   15  SUM-X               PICTURE S9(7)V9(2).
               10  LEVTOT-IO.
                   15  LEVTOT              PICTURE S9(8)V9(2).
               10  FNRTOT-IO.
                   15  FNRTOT              PICTURE S9(8)V9(2).
           05  EDITTING-FIELDS.
               10  XO-40YNZ                PICTURE ZZZZ.
               10  XO-70D                  PICTURE S9(7).
               10  XO-70U                  PICTURE 9(7).
               10  EDIT-RAB1               PICTURE Z9,9.
               10  EDIT-RAB2               PICTURE Z9,9.
               10  EDIT-UPRIS              PICTURE ZZZZZZ9,99.
               10  EDIT-SKOST              PICTURE ZZZZZZ9,99.
               10  XO-70YNZ                PICTURE ZZZZZZZ.
               10  XO-20YNZ                PICTURE ZZ.
               10  XO-82YY9                PICTURE ZZ.ZZZ.ZZZ,99.
           05  PREDEFINED-FIELDS.
               10  PAGE0                   PICTURE S9(4) USAGE BINARY.
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
           SET NOT-I-81                    TO TRUE
           SET NOT-I-82                    TO TRUE
           SET NOT-I-83                    TO TRUE
           SET NOT-I-84                    TO TRUE
           SET NOT-I-10                    TO TRUE
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  PARAM-PROCESS
               SET PARAM-PROCESS-OFF       TO TRUE
               SET PARAM-READ              TO TRUE
           END-IF
 
           IF  PARAM-READ
           AND RECORD-SELECTED-OFF
               PERFORM PARAM-GET
               SET PARAM-READ-OFF          TO TRUE
               IF  NOT PARAM-EOF
                   PERFORM PARAM-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET PARAM-PROCESS       TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  BESTM-PROCESS
               SET BESTM-PROCESS-OFF       TO TRUE
               SET BESTM-READ              TO TRUE
           END-IF
 
           IF  BESTM-READ
           AND RECORD-SELECTED-OFF
               PERFORM BESTM-GET
               SET BESTM-READ-OFF          TO TRUE
               IF  NOT BESTM-EOF
                   SET BESTM-PROCESS       TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  PARAM-PROCESS
               PERFORM PARAM-IDSET
           END-IF
 
           IF  BESTM-PROCESS
               PERFORM BESTM-IDSET
           END-IF
 
           IF  BESTM-PROCESS
               PERFORM BESTM-CHK-LEVEL
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
 
           IF  PARAM-PROCESS
               PERFORM PARAM-FLDOFF
               PERFORM PARAM-FLDSET
           END-IF
 
           IF  BESTM-PROCESS
               PERFORM BESTM-FLDOFF
               PERFORM BESTM-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  BESTM-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-60                    TO TRUE
           IF  (I-10)
               SET NOT-I-06                TO TRUE
               IF  PRINTK = 'S'
                   SET I-06                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-01)
               GO TO SLUTT-T
           END-IF
           IF  (I-L4 AND NOT-I-08)
               SET NOT-I-61                TO TRUE
               IF  FIRMA NOT = PFIRMA
                   SET I-61                TO TRUE
               END-IF
           END-IF
           IF  (I-61)
               GO TO SLUTT-T
           END-IF
           IF  (I-L3)
               PERFORM RBSRUT-S
           END-IF
           IF  (I-86)
               GO TO SLUTT-T
           END-IF
           IF  (I-L4)
               MOVE FIRMA                  TO LEVKEY (1:3)
           END-IF
           IF  (I-L2)
               MOVE LEVR                   TO LEVKEY (4:6)
               MOVE LEVKEY                 TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-20                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-20            TO TRUE
                   PERFORM KUNDEMA-FLDSET
                   PERFORM KUNDEMA-IDSET
               END-READ
           END-IF
           SET I-60                        TO TRUE
           SET NOT-I-25                    TO TRUE
           SET NOT-I-30                    TO TRUE
           IF  LEVA-ELGR < UYEAR
               SET I-25                    TO TRUE
           END-IF
           IF  LEVA-ELGR = UYEAR
               SET I-30                    TO TRUE
           END-IF
           IF  (I-30 AND NOT-I-25)
               SET NOT-I-25                TO TRUE
               IF  LEVUKE < PUKE
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-25)
               SET NOT-I-60                TO TRUE
           END-IF
           IF  (NOT-I-60)
               GO TO SLUTT-T
           END-IF
           IF  (I-06)
               MULTIPLY SKOST BY BANT  GIVING SUM-X
               ADD SUM-X                   TO LEVTOT
               ADD SUM-X                   TO FNRTOT
           END-IF
           SET NOT-I-90                    TO TRUE
           IF  BKREFT = 'B'
               SET I-90                    TO TRUE
           END-IF
      *  ÅR 2000 TEST.
           SET NOT-I-71                    TO TRUE
           IF  UYEAR NOT < 00
               SET I-71                    TO TRUE
           END-IF
           IF  (I-71)
               SET NOT-I-71                TO TRUE
               IF  UYEAR < 85
                   SET I-71                TO TRUE
               END-IF
           END-IF
           IF  (I-71)
               SET NOT-I-72                TO TRUE
               IF  LEVA-ELGR NOT > 99
                   SET I-72                TO TRUE
               END-IF
           END-IF
           IF  (I-71 AND I-72)
               SET NOT-I-72                TO TRUE
               IF  LEVA-ELGR > 85
                   SET I-72                TO TRUE
               END-IF
           END-IF
           IF  (I-71 AND I-72)
               SET I-25                    TO TRUE
               GO TO SLUTT-T
      *
           END-IF
           .
 
       SLUTT-T.
      **************************************************************
      *    RUTINE FOR OVERSTYRING AV RBS-FILE VED BESTILLINGSJOB"S *
      **************************************************************
           IF  (I-81)
               MOVE PJOBN                  TO BJOBN
               SET NOT-I-89                TO TRUE
               IF  PRKODE = 'B'
                   SET I-89                TO TRUE
               END-IF
               MOVE PRKODE                 TO BBEST
           END-IF
           IF  (I-81 AND I-89)
               MOVE PPERS                  TO BPERS
               MOVE PANTX                  TO BANTX-IO
           END-IF
           IF  (I-82 AND I-89)
               MOVE PETTB                  TO BETTB
           END-IF
           IF  (I-83 AND I-89)
               MOVE PFORS                  TO BFORS
           END-IF
           IF  (I-84 AND I-89)
               MOVE PMEMO                  TO BMEMO
      **************************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           END-IF
           .
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           IF  (NOT-I-89)
               MOVE ' '                    TO BBEST
           END-IF
           MOVE 'BES15'                    TO LONR
           MOVE FIRMA                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'NYB128  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF.
      ******************************************************
 
       PARAM-GET SECTION.
       PARAM-GET-P.
           IF  PARAM-EOF-OFF
               READ PARAM
               AT END
                   SET PARAM-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       PARAM-FLDOFF SECTION.
       PARAM-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = '9' )
               SET NOT-I-08                TO TRUE
           END-EVALUATE.
 
       PARAM-FLDSET SECTION.
       PARAM-FLDSET-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '1' )
               MOVE PARAM-IO-AREA (8:8)    TO PJOBN (1:8)
               MOVE PARAM-IO-AREA (19:1)   TO PRKODE (1:1)
               MOVE PARAM-IO-AREA (32:30)  TO PPERS (1:30)
               MOVE PARAM-IO-AREA (69:3)   TO PANTX-IO
               INSPECT PANTX-IO REPLACING ALL ' ' BY '0'
           WHEN ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '2' )
               MOVE PARAM-IO-AREA (21:40)  TO PETTB (1:40)
           WHEN ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '3' )
               MOVE PARAM-IO-AREA (21:40)  TO PFORS (1:40)
           WHEN ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '4' )
               MOVE PARAM-IO-AREA (21:40)  TO PMEMO (1:40)
           WHEN ( PARAM-IO-AREA (1:1) = '9' )
               MOVE PARAM-IO-AREA (12:3)   TO PFIRMA (1:3)
               IF  PFIRMA = SPACES
                   SET I-08                TO TRUE
               END-IF
               MOVE PARAM-IO-AREA (44:1)   TO PRINTK (1:1)
           END-EVALUATE.
 
       PARAM-IDCHK SECTION.
       PARAM-IDCHK-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '1' )
             OR ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '2' )
             OR ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '3' )
             OR ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '4' )
             OR ( PARAM-IO-AREA (1:1) = '9' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       PARAM-IDSET SECTION.
       PARAM-IDSET-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '1' )
               SET I-81                    TO TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '2' )
               SET I-82                    TO TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '3' )
               SET I-83                    TO TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '4' )
               SET I-84                    TO TRUE
           WHEN ( PARAM-IO-AREA (1:1) = '9' )
               SET I-10                    TO TRUE
           END-EVALUATE.
 
       BESTM-GET SECTION.
       BESTM-GET-P.
           IF  BESTM-EOF-OFF
               READ BESTM
               AT END
                   SET BESTM-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       BESTM-FLDOFF SECTION.
       BESTM-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-11                TO TRUE
               SET NOT-I-05                TO TRUE
           END-EVALUATE.
 
       BESTM-FLDSET SECTION.
       BESTM-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE BESTM-IO-AREA (1:3)    TO FIRMA (1:3)
               MOVE BESTM-IO-AREA (4:9)    TO BNAVN (1:9)
               MOVE BESTM-IO-AREA (13:5)   TO BEST (1:5)
               MOVE BESTM-IO-AREA (18:4)   TO POS-X-IO
               INSPECT POS-X-IO REPLACING ALL ' ' BY '0'
               MOVE BESTM-IO-AREA (22:1)   TO AVD (1:1)
               MOVE BESTM-IO-AREA (23:6)   TO LEVR (1:6)
               MOVE BESTM-IO-AREA (35:2)   TO A-ELGR (1:2)
               MOVE BESTM-IO-AREA (37:2)   TO MND (1:2)
               MOVE BESTM-IO-AREA (39:2)   TO DAG (1:2)
               MOVE BESTM-IO-AREA (29:2)   TO B (1:2)
               MOVE BESTM-IO-AREA (31:2)   TO LEVA-ELGR-IO
               INSPECT LEVA-ELGR-IO REPLACING ALL ' ' BY '0'
               MOVE BESTM-IO-AREA (33:2)   TO LEVUKE (1:2)
               MOVE BESTM-IO-AREA (44:1)   TO FAB (1:1)
               MOVE BESTM-IO-AREA (41:3)   TO ALFA (1:3)
               MOVE BESTM-IO-AREA (45:20)  TO ARTNR (1:20)
               MOVE BESTM-IO-AREA (95:4)   TO EDBNR-IO
               MOVE BESTM-IO-AREA (99:2)   TO RAB1-IO
               MOVE BESTM-IO-AREA (101:2)  TO RAB2-IO
               MOVE BESTM-IO-AREA (103:1)  TO VT (1:1)
               MOVE BESTM-IO-AREA (104:5)  TO UPRIS-IO
               MOVE BESTM-IO-AREA (109:4)  TO BANT-IO
               MOVE BESTM-IO-AREA (113:4)  TO TANT-IO
               IF  TANT = ZERO
                   SET I-11                TO TRUE
               END-IF
               MOVE BESTM-IO-AREA (117:6)  TO TDATO (1:6)
               MOVE BESTM-IO-AREA (117:2)  TO TA-ELGR (1:2)
               MOVE BESTM-IO-AREA (119:2)  TO TMND (1:2)
               MOVE BESTM-IO-AREA (121:2)  TO TDAG-IO
               INSPECT TDAG-IO REPLACING ALL ' ' BY '0'
               MOVE BESTM-IO-AREA (124:1)  TO BKREFT (1:1)
               MOVE BESTM-IO-AREA (125:2)  TO PUKE (1:2)
               MOVE BESTM-IO-AREA (132:5)  TO SKOST-IO
               IF  SKOST = ZERO
                   SET I-05                TO TRUE
               END-IF
           END-EVALUATE.
 
       BESTM-IDSET SECTION.
       BESTM-IDSET-P.
           SET I-01                        TO TRUE.
 
       BESTM-CHK-LEVEL SECTION.
       BESTM-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO BESTM-LEVEL-01
               MOVE BESTM-IO-AREA (1:3)    TO BESTM-01-L4-FIRMA
               MOVE BESTM-IO-AREA (4:9)    TO BESTM-01-L3-BNAVN
               MOVE BESTM-IO-AREA (23:6)   TO BESTM-01-L2-LEVR
               MOVE BESTM-IO-AREA (13:5)   TO BESTM-01-L1-BEST
               IF  BESTM-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  BESTM-01-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   WHEN  BESTM-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  BESTM-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  BESTM-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  BESTM-01-L4           TO THE-PRIOR-L4
               MOVE  BESTM-01-L3           TO THE-PRIOR-L3
               MOVE  BESTM-01-L2           TO THE-PRIOR-L2
               MOVE  BESTM-01-L1           TO THE-PRIOR-L1
               SET BESTM-LEVEL-INIT        TO TRUE
           END-EVALUATE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (16:30) TO NAVN1 (1:30)
               MOVE KUNDEMA-IO-AREA (46:30) TO NAVN2 (1:30)
               MOVE KUNDEMA-IO-AREA (76:30) TO ADR (1:30)
               MOVE KUNDEMA-IO-AREA (106:15) TO PSTED (1:15)
               MOVE KUNDEMA-IO-AREA (121:4) TO PNR (1:4)
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
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
           IF  (I-L1 AND I-60 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ' '                    TO LISTE-IO-AREA (1:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-01 AND I-60 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE BEST                   TO LISTE-IO-AREA (1:5)
               MOVE POS-X-IO               TO LISTE-IO-AREA (7:4)
               MOVE DAG                    TO LISTE-IO-AREA (12:2)
               MOVE '.'                    TO LISTE-IO-AREA (14:1)
               MOVE MND                    TO LISTE-IO-AREA (15:2)
               MOVE '.'                    TO LISTE-IO-AREA (17:1)
               MOVE A-ELGR                 TO LISTE-IO-AREA (18:2)
               MOVE AVD                    TO LISTE-IO-AREA (22:1)
               MOVE B                      TO LISTE-IO-AREA (26:2)
               MOVE LEVUKE                 TO LISTE-IO-AREA (31:2)
               MOVE LEVA-ELGR-IO           TO LISTE-IO-AREA (35:2)
               MOVE ALFA                   TO LISTE-IO-AREA (42:3)
               MOVE ARTNR                  TO LISTE-IO-AREA (46:20)
               MOVE EDBNR                  TO XO-70U
               MOVE XO-70U (1:7)           TO LISTE-IO-AREA (67:7)
               MOVE RAB1                   TO EDIT-RAB1
               MOVE EDIT-RAB1              TO LISTE-IO-AREA (75:4)
               MOVE RAB2                   TO EDIT-RAB2
               MOVE EDIT-RAB2              TO LISTE-IO-AREA (80:4)
               MOVE VT                     TO LISTE-IO-AREA (85:1)
               IF  (NOT-I-06)
                   MOVE UPRIS              TO EDIT-UPRIS
                   MOVE EDIT-UPRIS         TO LISTE-IO-AREA (87:10)
               END-IF
               IF  (I-06)
                   MOVE SKOST              TO EDIT-SKOST
                   MOVE EDIT-SKOST         TO LISTE-IO-AREA (87:10)
               END-IF
               IF  (I-06 AND I-05)
                   MOVE '*'                TO LISTE-IO-AREA (99:1)
               END-IF
               MOVE BANT                   TO XO-70YNZ
               MOVE XO-70YNZ               TO LISTE-IO-AREA (102:7)
               IF  (NOT-I-11)
                   MOVE TANT               TO XO-70YNZ
                   MOVE XO-70YNZ           TO LISTE-IO-AREA (109:7)
               END-IF
               IF  (NOT-I-11)
                   MOVE TDAG               TO XO-20YNZ
                   MOVE XO-20YNZ           TO LISTE-IO-AREA (117:2)
               END-IF
               IF  (NOT-I-11)
                   MOVE TMND               TO LISTE-IO-AREA (120:2)
               END-IF
               IF  (NOT-I-11)
                   MOVE TA-ELGR            TO LISTE-IO-AREA (123:2)
               END-IF
               IF  (NOT-I-11)
                   MOVE '.'                TO LISTE-IO-AREA (119:1)
               END-IF
               IF  (NOT-I-11)
                   MOVE '.'                TO LISTE-IO-AREA (122:1)
               END-IF
               IF  (I-90)
                   MOVE 'B'                TO LISTE-IO-AREA (127:1)
               END-IF
               IF  (I-25)
                   MOVE '****'             TO LISTE-IO-AREA (129:4)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L2 AND I-60 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE LOPNVN                 TO LISTE-IO-AREA (32:35)
               MOVE 'OPPG=BES15'           TO LISTE-IO-AREA (71:10)
               MOVE 'LEVERES:'             TO LISTE-IO-AREA (83:8)
               MOVE BNAVN                  TO LISTE-IO-AREA (93:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (105:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (124:4)
               IF  (I-L3)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (129:4)
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
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'BEST  POS  BESTILL'   TO LISTE-IO-AREA (2:18)
               MOVE 'LEV LEV BET'          TO LISTE-IO-AREA (30:11)
               MOVE 'V'                    TO LISTE-IO-AREA (85:1)
               IF  (NOT-I-06)
                   MOVE 'PRIS'             TO LISTE-IO-AREA (90:4)
               END-IF
               IF  (I-06)
                   MOVE 'U'                TO LISTE-IO-AREA (99:1)
               END-IF
               MOVE 'BEST   TILG   TILGANG' TO LISTE-IO-AREA (104:21)
               MOVE 'B'                    TO LISTE-IO-AREA (127:1)
               MOVE 'IKKE'                 TO LISTE-IO-AREA (129:4)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'NR   NR    DATO  AVD' TO LISTE-IO-AREA (4:20)
               MOVE 'BEST UKE  ÅR BET ALF ART' TO LISTE-IO-AREA (25:24)
               MOVE 'IKKELNUMMER'          TO LISTE-IO-AREA (49:11)
               MOVE 'EDB-NR.  RAB  RAB T'  TO LISTE-IO-AREA (67:19)
               IF  (NOT-I-06)
                   MOVE 'UTL. MYNT'        TO LISTE-IO-AREA (88:9)
               END-IF
               IF  (I-06)
                   MOVE 'SELVKOST  S'      TO LISTE-IO-AREA (89:11)
               END-IF
               MOVE 'ANTALL ANTALL    DATO' TO LISTE-IO-AREA (103:21)
               MOVE 'K ANK.'               TO LISTE-IO-AREA (127:6)
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
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE LEVR                   TO LISTE-IO-AREA (1:6)
               IF  (NOT-I-20)
                   MOVE NAVN1              TO LISTE-IO-AREA (10:30)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               IF  (NOT-I-20)
                   MOVE NAVN2              TO LISTE-IO-AREA (10:30)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               IF  (NOT-I-20)
                   MOVE ADR                TO LISTE-IO-AREA (10:30)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               IF  (NOT-I-20)
                   MOVE PSTED              TO LISTE-IO-AREA (14:15)
               END-IF
               IF  (NOT-I-20)
                   MOVE PNR                TO LISTE-IO-AREA (10:4)
               END-IF
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
               MOVE 'OPPG=BES15'           TO LISTE-IO-AREA (71:10)
               MOVE 'LEVERES:'             TO LISTE-IO-AREA (83:8)
               MOVE BNAVN                  TO LISTE-IO-AREA (93:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (105:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (124:4)
               IF  (I-L3)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (129:4)
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
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'BEST  POS  BESTILL'   TO LISTE-IO-AREA (2:18)
               MOVE 'LEV LEV BET'          TO LISTE-IO-AREA (30:11)
               MOVE 'V'                    TO LISTE-IO-AREA (85:1)
               IF  (NOT-I-06)
                   MOVE 'PRIS'             TO LISTE-IO-AREA (90:4)
               END-IF
               IF  (I-06)
                   MOVE 'U'                TO LISTE-IO-AREA (99:1)
               END-IF
               MOVE 'BEST   TILG   TILGANG' TO LISTE-IO-AREA (104:21)
               MOVE 'B'                    TO LISTE-IO-AREA (127:1)
               MOVE 'IKKE'                 TO LISTE-IO-AREA (129:4)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'NR   NR    DATO  AVD' TO LISTE-IO-AREA (4:20)
               MOVE 'BEST UKE  ÅR BET ALF ART' TO LISTE-IO-AREA (25:24)
               MOVE 'IKKELNUMMER'          TO LISTE-IO-AREA (49:11)
               MOVE 'EDB-NR.  RAB  RAB T'  TO LISTE-IO-AREA (67:19)
               IF  (NOT-I-06)
                   MOVE 'UTL. MYNT'        TO LISTE-IO-AREA (88:9)
               END-IF
               IF  (I-06)
                   MOVE 'SELVKOST  S'      TO LISTE-IO-AREA (89:11)
               END-IF
               MOVE 'ANTALL ANTALL    DATO' TO LISTE-IO-AREA (103:21)
               MOVE 'K ANK.'               TO LISTE-IO-AREA (127:6)
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
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE LEVR                   TO LISTE-IO-AREA (1:6)
               IF  (NOT-I-20)
                   MOVE NAVN1              TO LISTE-IO-AREA (10:30)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               IF  (NOT-I-20)
                   MOVE NAVN2              TO LISTE-IO-AREA (10:30)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               IF  (NOT-I-20)
                   MOVE ADR                TO LISTE-IO-AREA (10:30)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               IF  (NOT-I-20)
                   MOVE PSTED              TO LISTE-IO-AREA (14:15)
               END-IF
               IF  (NOT-I-20)
                   MOVE PNR                TO LISTE-IO-AREA (10:4)
               END-IF
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L2 AND I-06 AND NOT-I-61)
           AND (NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'LEVERANDØRTOTAL I BEST' TO LISTE-IO-AREA (1:22)
               MOVE 'ILLING FOR'           TO LISTE-IO-AREA (23:10)
               MOVE LEVR                   TO LISTE-IO-AREA (34:6)
               IF  (NOT-I-20)
                   MOVE NAVN1              TO LISTE-IO-AREA (41:30)
               END-IF
               MOVE 'N.KR.'                TO LISTE-IO-AREA (78:5)
               MOVE LEVTOT                 TO XO-82YY9
               MOVE XO-82YY9               TO LISTE-IO-AREA (84:13)
               INITIALIZE LEVTOT
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L3 AND I-06 AND NOT-I-61)
           AND (NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALT FOR BESTILLER' TO LISTE-IO-AREA (3:20)
               MOVE 'PR.'                  TO LISTE-IO-AREA (24:3)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (28:8)
               MOVE FNRTOT                 TO XO-82YY9
               MOVE XO-82YY9               TO LISTE-IO-AREA (84:13)
               INITIALIZE FNRTOT
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
           INITIALIZE PARAM-DATA-FIELDS
           SET PARAM-EOF-OFF               TO TRUE
           SET PARAM-PROCESS               TO TRUE
           OPEN INPUT PARAM
           SET BESTM-LEVEL-INIT            TO TRUE
           INITIALIZE BESTM-DATA-FIELDS
           SET BESTM-EOF-OFF               TO TRUE
           SET BESTM-PROCESS               TO TRUE
           OPEN INPUT BESTM
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PARAM
           CLOSE BESTM
           CLOSE KUNDEMA
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
