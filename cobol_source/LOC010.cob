       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOC010R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAM: LOC010 AV ESPEN LARSEN 30.03.2000                    *
      * REORGANISERER LOCMAST.                                        *
      *****************************************************************
      *                                          XX2000XXIRXXSS
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: LOC010.rpg
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
           SELECT PAR
               ASSIGN TO UT-S-PAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PAR-STATUS.
           SELECT LOCMAST
               ASSIGN TO LOCMAST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS LOCMAST-STATUS
               RECORD KEY IS LOCMAST-KEY1.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT VAREMAS
               ASSIGN TO VAREMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAREMAS-STATUS
               RECORD KEY IS VAREMAS-KEY1.
           SELECT HJFILE
               ASSIGN TO HJFILE
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS HJFILE-STATUS
               RECORD KEY IS HJFILE-KEY1.
           SELECT OUTFILE
               ASSIGN TO UT-S-OUTFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTFILE-STATUS.
           SELECT PRINT-X
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PRINT-X-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD PAR
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  PAR-IO-AREA.
           05  PAR-IO-AREA-X               PICTURE X(80).
       FD LOCMAST
               RECORD CONTAINS 100.
       01  LOCMAST-IO-AREA.
           05  LOCMAST-IO-AREA-X.
               10  LOCMAST-KEY1.
                   15  LOCMAST-KEY1N       PICTURE S9(12).
               10  FILLER                  PICTURE X(88).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD VAREMAS
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X.
               10  VAREMAS-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       FD HJFILE
               RECORD CONTAINS 20.
       01  HJFILE-IO-AREA.
           05  HJFILE-IO-AREA-X.
               10  HJFILE-KEY1             PICTURE X(16).
               10  FILLER                  PICTURE X(4).
       FD OUTFILE
               BLOCK CONTAINS 200
               RECORD CONTAINS 100.
       01  OUTFILE-IO-AREA.
           05  OUTFILE-IO-AREA-X           PICTURE X(100).
       FD PRINT-X
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  PRINT-X-IO-PRINT.
           05  PRINT-X-IO-AREA-CONTROL     PICTURE X VALUE ' '.
        02 PRINT-X-IO-AREA.
           05  PRINT-X-IO-AREA-X           PICTURE X(132).
       WORKING-STORAGE SECTION.
       77  ARL-MAX   VALUE 400             PICTURE 9(4) USAGE BINARY.
       77  ARB-MAX   VALUE 400             PICTURE 9(4) USAGE BINARY.
       77  ARD-MAX   VALUE 400             PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  ARL-TABLE.
               10  ARL-ENTRY
                                           OCCURS 400 TIMES
                                           INDEXED BY ARL-I
                                                      ARL-S.
                   15  ARL                 PICTURE X(6).
           05  ARB-TABLE.
               10  ARB-ENTRY
                                           OCCURS 400 TIMES
                                           INDEXED BY ARB-I
                                                      ARB-S.
                   15  ARB                 PICTURE S9(7)V9(2).
      * **START RBS - DATASTRUKTUR FOR SUB-PROGRAM RBS000 ********
           05  ARD-TABLE.
               10  ARD-ENTRY
                                           OCCURS 400 TIMES
                                           INDEXED BY ARD-I
                                                      ARD-S.
                   15  ARD                 PICTURE X(9).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  PAR-STATUS                  PICTURE 99 VALUE 0.
           10  LOCMAST-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  HJFILE-STATUS               PICTURE 99 VALUE 0.
           10  OUTFILE-STATUS              PICTURE 99 VALUE 0.
           10  PRINT-X-STATUS              PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  PAR-EOF-OFF             VALUE '0'.
               88  PAR-EOF                 VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PAR-READ-OFF            VALUE '0'.
               88  PAR-READ                VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PAR-PROCESS-OFF         VALUE '0'.
               88  PAR-PROCESS             VALUE '1'.
           05  LOCMAST-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  LOCMAST-EOF-OFF         VALUE '0'.
               88  LOCMAST-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  LOCMAST-READ-OFF        VALUE '0'.
               88  LOCMAST-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  LOCMAST-PROCESS-OFF     VALUE '0'.
               88  LOCMAST-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  LOCMAST-LEVEL-INIT-OFF  VALUE '0'.
               88  LOCMAST-LEVEL-INIT      VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  VAREMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  HJFILE-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  PRINT-X-DATA-FIELDS.
               10  PRINT-X-AFTER-SPACE     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINT-X-AFTER-SKIP      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINT-X-BEFORE-SPACE    PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINT-X-BEFORE-SKIP     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINT-X-MAX-LINES       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINT-X-LINE-COUNT      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINT-X-CLR-IO          PICTURE X VALUE 'Y'.
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
           05  PAR-DATA-FIELDS.
               10  PFIRMA                  PICTURE X(3).
           05  LOCMAST-LEVEL-01.
               10  LOCMAST-01-L2.
                   15  LOCMAST-01-L2-FIRMA PICTURE X(3).
               10  LOCMAST-01-L1.
                   15  LOCMAST-01-L1-EDBNR PICTURE X(7).
           05  LOCMAST-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  EDBNR                   PICTURE X(7).
               10  EDBKEY                  PICTURE X(10).
               10  FLREC1                  PICTURE X(20).
               10  FLLOC1                  PICTURE X(6).
               10  FLBEH1-IO.
                   15  FLBEH1              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  FLDIV1                  PICTURE X(9).
               10  FLREC2                  PICTURE X(20).
               10  FLLOC2                  PICTURE X(6).
               10  FLBEH2-IO.
                   15  FLBEH2              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  FLDIV2                  PICTURE X(9).
               10  FLREC3                  PICTURE X(20).
               10  FLLOC3                  PICTURE X(6).
               10  FLBEH3-IO.
                   15  FLBEH3              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  FLDIV3                  PICTURE X(9).
               10  FLREC4                  PICTURE X(20).
               10  FLLOC4                  PICTURE X(6).
               10  FLBEH4-IO.
                   15  FLBEH4              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  FLDIV4                  PICTURE X(9).
           05  FIRMAF-DATA-FIELDS.
               10  FIRMSL                  PICTURE X(1).
           05  VAREMAS-DATA-FIELDS.
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  ANTINN-IO.
                   15  ANTINN              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ANTUT-IO.
                   15  ANTUT               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  LLOC                    PICTURE X(6).
               10  LAG13-IO.
                   15  LAG13               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  LAG93-IO.
                   15  LAG93               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  LAG15-IO.
                   15  LAG15               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  LAG17-IO.
                   15  LAG17               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  LAG92-IO.
                   15  LAG92               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  LAG18-IO.
                   15  LAG18               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
           05  HJFILE-DATA-FIELDS.
               10  ANTVL-IO.
                   15  ANTVL               PICTURE S9(4).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(7).
           05  TEMPORARY-FIELDS.
               10  ANTL2-IO.
                   15  ANTL2               PICTURE S9(7).
               10  ANTLS-IO.
                   15  ANTLS               PICTURE S9(7).
               10  ANTLU-IO.
                   15  ANTLU               PICTURE S9(7).
               10  FBEHL2-IO.
                   15  FBEHL2              PICTURE S9(8)V9(2).
               10  FBEH-IO.
                   15  FBEH                PICTURE S9(7)V9(2).
               10  VBEH-IO.
                   15  VBEH                PICTURE S9(7)V9(2).
               10  X-IO.
                   15  X                   PICTURE S9(3).
               10  NULL-X-IO.
                   15  NULL-X              PICTURE S9(7)V9(2).
               10  F9                      PICTURE X(9).
               10  LOCKEY                  PICTURE X(16).
               10  FLBEH1-N-IO.
                   15  FLBEH1-N            PICTURE S9(7)V9(2).
               10  FLBEH2-N-IO.
                   15  FLBEH2-N            PICTURE S9(7)V9(2).
               10  FLBEH3-N-IO.
                   15  FLBEH3-N            PICTURE S9(7)V9(2).
               10  FLBEH4-N-IO.
                   15  FLBEH4-N            PICTURE S9(7)V9(2).
               10  DIFF-IO.
                   15  DIFF                PICTURE S9(7)V9(2).
               10  Y-IO.
                   15  Y                   PICTURE S9(3).
               10  SEQNR-IO.
                   15  SEQNR               PICTURE S9(2).
               10  FLOC1                   PICTURE X(6).
               10  FBEH1-IO.
                   15  FBEH1               PICTURE S9(7)V9(2).
               10  FDIV1                   PICTURE X(9).
               10  FLOC2                   PICTURE X(6).
               10  FBEH2-IO.
                   15  FBEH2               PICTURE S9(7)V9(2).
               10  FDIV2                   PICTURE X(9).
               10  FLOC3                   PICTURE X(6).
               10  FBEH3-IO.
                   15  FBEH3               PICTURE S9(7)V9(2).
               10  FDIV3                   PICTURE X(9).
               10  FLOC4                   PICTURE X(6).
               10  FBEH4-IO.
                   15  FBEH4               PICTURE S9(7)V9(2).
               10  FDIV4                   PICTURE X(9).
           05  EDITTING-FIELDS.
               10  XO-72P-EF.
                 15  XO-72P                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-72YY9R               PICTURE Z.ZZZ.ZZZ,99-.
               10  XO-70YY9                PICTURE Z.ZZZ.ZZ9.
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
           SET NOT-I-10                    TO TRUE
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-04                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  PAR-PROCESS
               SET PAR-PROCESS-OFF         TO TRUE
               SET PAR-READ                TO TRUE
           END-IF
 
           IF  PAR-READ
           AND RECORD-SELECTED-OFF
               PERFORM PAR-GET
               SET PAR-READ-OFF            TO TRUE
               IF  NOT PAR-EOF
                   PERFORM PAR-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET PAR-PROCESS         TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LOCMAST-PROCESS
               SET LOCMAST-PROCESS-OFF     TO TRUE
               SET LOCMAST-READ            TO TRUE
           END-IF
 
           IF  LOCMAST-READ
           AND RECORD-SELECTED-OFF
               PERFORM LOCMAST-GET
               SET LOCMAST-READ-OFF        TO TRUE
               IF  NOT LOCMAST-EOF
                   SET LOCMAST-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  PAR-PROCESS
               PERFORM PAR-IDSET
           END-IF
 
           IF  LOCMAST-PROCESS
               PERFORM LOCMAST-IDSET
           END-IF
 
           IF  LOCMAST-PROCESS
               PERFORM LOCMAST-CHK-LEVEL
           END-IF
 
           IF I-1ST
               GO TO LR-INDICATOR-TEST
           END-IF.
 
       MAINLINE-TOTAL-CALCS.
           PERFORM TOTAL-CALCS
           PERFORM TOTAL-OUTPUT.
 
       LR-INDICATOR-TEST.
           IF  I-LR
               GO TO MAINLINE-TERMINATION
           END-IF
           PERFORM HEADING-OVERFLOW
 
           IF  PAR-PROCESS
               PERFORM PAR-FLDOFF
               PERFORM PAR-FLDSET
           END-IF
 
           IF  LOCMAST-PROCESS
               PERFORM LOCMAST-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  LOCMAST-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-10)
               GO TO UT-T
      ******** FIRMA TEST,  BLANK ER ALLE FIRMA SKAL PRINTES. ***
           END-IF
           IF  (I-L2 AND I-09)
               SET I-22                    TO TRUE
           END-IF
           IF  (I-L2 AND NOT-I-09)
               SET NOT-I-22                TO TRUE
               IF  FIRMA = PFIRMA
                   SET I-22                TO TRUE
               END-IF
      ******** HVIS VENG SKAL BARE DIFF PRINTES               ***
           END-IF
           IF  (I-L2)
               SET NOT-I-28                TO TRUE
               IF  FIRMA = '956'
                   SET I-28                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-22)
               PERFORM RBSRUT-S
           END-IF
           IF  (I-L2)
               PERFORM FISLET-S
      *****************************************************************
           END-IF
           IF  (I-L2)
               SUBTRACT ANTL2              FROM ANTL2
               SUBTRACT ANTLS              FROM ANTLS
               SUBTRACT ANTLU              FROM ANTLU
               SUBTRACT FBEHL2             FROM FBEHL2
           END-IF
           IF  (I-98)
               GO TO SLUTT-T
           END-IF
           IF  (I-L1)
               SUBTRACT FBEH               FROM FBEH
               SUBTRACT VBEH               FROM VBEH
               SUBTRACT X                  FROM X
               PERFORM VARYING ARL-I FROM 1 BY 1
                         UNTIL ARL-I > ARL-MAX
                   MOVE '      '           TO ARL (ARL-I)
               END-PERFORM
               MOVE 0,00                   TO NULL-X-IO (4:6)
               PERFORM VARYING ARB-I FROM 1 BY 1
                         UNTIL ARB-I > ARB-MAX
                   MOVE NULL-X             TO ARB (ARB-I)
               END-PERFORM
               MOVE '        '             TO F9 (1:8)
               MOVE ' '                    TO F9 (9:1)
               PERFORM VARYING ARD-I FROM 1 BY 1
                         UNTIL ARD-I > ARD-MAX
                   MOVE F9                 TO ARD (ARD-I)
               END-PERFORM
               SET NOT-I-71                TO TRUE
               SET NOT-I-80                TO TRUE
           END-IF
           IF  (I-01)
               ADD 1                       TO ANTL2
      *****************************************************************
      * SUBRUTINE FOR KONTROLL MOT VAREMASTER.                        *
      *****************************************************************
           END-IF
           IF  (I-L1)
               MOVE EDBKEY                 TO VAREMAS-KEY1
               READ VAREMAS RECORD KEY IS VAREMAS-KEY1
               INVALID KEY
                   SET I-21                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-21            TO TRUE
                   PERFORM VAREMAS-FLDSET
                   PERFORM VAREMAS-IDSET
               END-READ
           END-IF
           IF  (I-21)
               GO TO SLUTT-T
           END-IF
           IF  (I-28)
               SET NOT-I-27                TO TRUE
               IF  LLOC = 'FLYT  '
                   SET I-27                TO TRUE
               END-IF
           END-IF
           IF  (I-28 AND NOT-I-27)
               GO TO SLUTT-T
           END-IF
           IF  (I-L1)
               SUBTRACT ANTUT FROM ANTINN GIVING VBEH
               SUBTRACT LAG13              FROM VBEH
               SUBTRACT LAG93              FROM VBEH
               SUBTRACT LAG15              FROM VBEH
               SUBTRACT LAG17              FROM VBEH
               SUBTRACT LAG92              FROM VBEH
               SUBTRACT LAG18              FROM VBEH
      *****************************************************************
      * RUTINE FOR SUMMERING OG REORGANISERING AV LOCMAST RECORD.     *
      *****************************************************************
           END-IF
           ADD FLBEH1                      TO FBEH
           ADD FLBEH2                      TO FBEH
           ADD FLBEH3                      TO FBEH
           ADD FLBEH4                      TO FBEH
           ADD FLBEH1                      TO FBEHL2
           ADD FLBEH2                      TO FBEHL2
           ADD FLBEH3                      TO FBEHL2
           ADD FLBEH4                      TO FBEHL2
      *****************************************************************
      * RUTINE FOR REORGANISERING.                                    *
      *****************************************************************
           MOVE EDBKEY                     TO LOCKEY (1:10)
           MOVE FLLOC1                     TO LOCKEY (11:6)
           SET NOT-I-31                    TO TRUE
           IF  FLBEH1 NOT = 0,00
               SET I-31                    TO TRUE
           END-IF
           IF  (NOT-I-31)
               PERFORM HJRUT-S
      *          FIRMA     COMP "956"                    51 VENG
      *  51      FBEH      COMP 0,00                     51 IKKE SLETTE
      *  51N31             SETON                         31 IKKE SLETT
           END-IF
           IF  (I-31)
               ADD 1                       TO X
               MOVE FLLOC1                 TO ARL (X)
               MOVE FLBEH1                 TO FLBEH1-N
               MOVE FLBEH1-N-IO            TO ARB (X)
               MOVE FLDIV1                 TO ARD (X)
      *                                                               *
           END-IF
           MOVE FLLOC2                     TO LOCKEY (11:6)
           SET NOT-I-31                    TO TRUE
           IF  FLBEH2 NOT = 0,00
               SET I-31                    TO TRUE
           END-IF
           IF  (NOT-I-31)
               PERFORM HJRUT-S
           END-IF
           IF  (I-31)
               ADD 1                       TO X
               MOVE FLLOC2                 TO ARL (X)
               MOVE FLBEH2                 TO FLBEH2-N
               MOVE FLBEH2-N-IO            TO ARB (X)
               MOVE FLDIV2                 TO ARD (X)
      *                                                               *
           END-IF
           MOVE FLLOC3                     TO LOCKEY (11:6)
           SET NOT-I-31                    TO TRUE
           IF  FLBEH3 NOT = 0,00
               SET I-31                    TO TRUE
           END-IF
           IF  (NOT-I-31)
               PERFORM HJRUT-S
           END-IF
           IF  (I-31)
               ADD 1                       TO X
               MOVE FLLOC3                 TO ARL (X)
               MOVE FLBEH3                 TO FLBEH3-N
               MOVE FLBEH3-N-IO            TO ARB (X)
               MOVE FLDIV3                 TO ARD (X)
      *                                                               *
           END-IF
           MOVE FLLOC4                     TO LOCKEY (11:6)
           SET NOT-I-31                    TO TRUE
           IF  FLBEH4 NOT = 0,00
               SET I-31                    TO TRUE
           END-IF
           IF  (NOT-I-31)
               PERFORM HJRUT-S
           END-IF
           IF  (I-31)
               ADD 1                       TO X
               MOVE FLLOC4                 TO ARL (X)
               MOVE FLBEH4                 TO FLBEH4-N
               MOVE FLBEH4-N-IO            TO ARB (X)
               MOVE FLDIV4                 TO ARD (X)
      *****************************************************************
      * SJEKKER OM BLANK I LOCATION, 4 FØRSTE LOC                     *
      *****************************************************************
           END-IF
           IF  (I-L1)
               SET NOT-I-29                TO TRUE
               IF  FLLOC1 = '      '
                   SET I-29                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-29)
               SET NOT-I-29                TO TRUE
               IF  FLLOC2 = '      '
                   SET I-29                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-29)
               SET NOT-I-29                TO TRUE
               IF  FLLOC3 = '      '
                   SET I-29                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-29)
               SET NOT-I-29                TO TRUE
               IF  FLLOC4 = '      '
                   SET I-29                TO TRUE
               END-IF
      *****************************************************************
      * TELLERUTINE.                                                  *
      *****************************************************************
           END-IF
           .
 
       SLUTT-T.
           IF  (I-21)
               ADD 1                       TO ANTLS
           END-IF
           IF  (NOT-I-21 AND I-98)
               ADD 1                       TO ANTLS
           END-IF.
 
       UT-T.
      *****************************************************************
      * TOTALRUTINE PR. EDBNR.                                        *
      *****************************************************************
           CONTINUE.
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE ' '                        TO BBEST
           MOVE 'LOC10'                    TO LONR
           MOVE FIRMA                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'LOC010'                   TO LPROG (1:6)
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF
           IF  (I-86)
               SET NOT-I-22                TO TRUE
           END-IF.
      ******************************************************
      *    SUBRUTINE FOR SLETTING AV HELE FIRMA            *
      ******************************************************
 
       FISLET-S SECTION.
       FISLET-S-P.
           SET NOT-I-98                    TO TRUE
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
               SET NOT-I-98                TO TRUE
               IF  FIRMSL = 'S'
                   SET I-98                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-96 AND NOT-I-98)
               SET NOT-I-98                TO TRUE
               IF  FIRMSL = 'R'
                   SET I-98                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-98)
               SET NOT-I-98                TO TRUE
               IF  FIRMA NOT > '000'
                   SET I-98                TO TRUE
               END-IF
           END-IF
           IF  (I-96)
               SET I-98                    TO TRUE
           END-IF.
      ******************************************************
      *    SUBRUTINE FOR Å SE OM LOCATION ER I ORDREMASTER.*
      *    ER DEN DET SKAL VI BEHOLDE LOCATIONEN SELV OM   *
      *    DEN HAR BEHOLDNING 0.                           *
      ******************************************************
 
       HJRUT-S SECTION.
       HJRUT-S-P.
           SET NOT-I-92                    TO TRUE
           MOVE LOCKEY                     TO HJFILE-KEY1
           READ HJFILE RECORD KEY IS HJFILE-KEY1
           INVALID KEY
               SET I-91                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-91                TO TRUE
               PERFORM HJFILE-FLDSET
               PERFORM HJFILE-IDSET
           END-READ
           IF  (NOT-I-91)
               SET NOT-I-92                TO TRUE
               IF  ANTVL > 0
                   SET I-92                TO TRUE
               END-IF
           END-IF
           IF  (I-92)
               SET I-31                    TO TRUE
           END-IF.
      ******************************************************
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1)
               SET NOT-I-72                TO TRUE
           END-IF
           IF  (I-L1 AND I-98)
               GO TO ENDL1-T
           END-IF
           IF  (I-L1 AND I-21)
               GO TO ENDL1-T
           END-IF
           IF  (I-L1)
               SUBTRACT FBEH FROM VBEH GIVING DIFF
               SET NOT-I-71                TO TRUE
               IF  DIFF NOT = 0
                   SET I-71                TO TRUE
               END-IF
               SUBTRACT Y                  FROM Y
               SUBTRACT SEQNR              FROM SEQNR
               SET NOT-I-72                TO TRUE
               IF  X = 0
                   SET I-72                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-72)
               ADD 1                       TO ANTLS
               GO TO ENDL1-T
           END-IF.
 
       SLOP1-T.
           IF  (I-L1)
               SET NOT-I-70                TO TRUE
               ADD 1                       TO Y
               SET NOT-I-73                TO TRUE
               IF  Y > X
                   SET I-73                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-73)
               GO TO SLOPX-T
           END-IF
           IF  (I-L1)
               MOVE ARL (Y)                TO FLOC1
               MOVE ARB (Y)                TO FBEH1-IO
               MOVE ARD (Y)                TO FDIV1
               ADD 1                       TO Y
               MOVE ARL (Y)                TO FLOC2
               MOVE ARB (Y)                TO FBEH2-IO
               MOVE ARD (Y)                TO FDIV2
               ADD 1                       TO Y
               MOVE ARL (Y)                TO FLOC3
               MOVE ARB (Y)                TO FBEH3-IO
               MOVE ARD (Y)                TO FDIV3
               ADD 1                       TO Y
               MOVE ARL (Y)                TO FLOC4
               MOVE ARB (Y)                TO FBEH4-IO
               MOVE ARD (Y)                TO FDIV4
               SET I-70                    TO TRUE
               SET I-80                    TO TRUE
           END-IF
           IF  (I-L1 AND I-70)
               ADD 1                       TO ANTLU
               ADD 1                       TO SEQNR
               PERFORM EXCEPTION-OUTPUT
               GO TO SLOP1-T
           END-IF.
 
       SLOPX-T.
           CONTINUE.
 
       ENDL1-T.
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           CONTINUE.
 
       PAR-GET SECTION.
       PAR-GET-P.
           IF  PAR-EOF-OFF
               READ PAR
               AT END
                   SET PAR-EOF             TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       PAR-FLDOFF SECTION.
       PAR-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( PAR-IO-AREA (1:1) = 'P' )
               SET NOT-I-09                TO TRUE
           END-EVALUATE.
 
       PAR-FLDSET SECTION.
       PAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ( PAR-IO-AREA (1:1) = 'P' )
               MOVE PAR-IO-AREA (13:3)     TO PFIRMA (1:3)
               IF  PFIRMA = SPACES
                   SET I-09                TO TRUE
               END-IF
           END-EVALUATE.
 
       PAR-IDCHK SECTION.
       PAR-IDCHK-P.
           EVALUATE TRUE
           WHEN ( PAR-IO-AREA (1:1) = 'P' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       PAR-IDSET SECTION.
       PAR-IDSET-P.
           EVALUATE TRUE
           WHEN ( PAR-IO-AREA (1:1) = 'P' )
               SET I-10                    TO TRUE
           END-EVALUATE.
 
       LOCMAST-GET SECTION.
       LOCMAST-GET-P.
           IF  LOCMAST-EOF-OFF
               READ LOCMAST
               AT END
                   SET LOCMAST-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       LOCMAST-FLDSET SECTION.
       LOCMAST-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOCMAST-IO-AREA (1:3)  TO FIRMA (1:3)
               MOVE LOCMAST-IO-AREA (4:7)  TO EDBNR (1:7)
               MOVE LOCMAST-IO-AREA (1:10) TO EDBKEY (1:10)
               MOVE LOCMAST-IO-AREA (13:20) TO FLREC1 (1:20)
               MOVE LOCMAST-IO-AREA (13:6) TO FLLOC1 (1:6)
               MOVE LOCMAST-IO-AREA (19:5) TO FLBEH1-IO
               MOVE LOCMAST-IO-AREA (24:9) TO FLDIV1 (1:9)
               MOVE LOCMAST-IO-AREA (33:20) TO FLREC2 (1:20)
               MOVE LOCMAST-IO-AREA (33:6) TO FLLOC2 (1:6)
               MOVE LOCMAST-IO-AREA (39:5) TO FLBEH2-IO
               MOVE LOCMAST-IO-AREA (44:9) TO FLDIV2 (1:9)
               MOVE LOCMAST-IO-AREA (53:20) TO FLREC3 (1:20)
               MOVE LOCMAST-IO-AREA (53:6) TO FLLOC3 (1:6)
               MOVE LOCMAST-IO-AREA (59:5) TO FLBEH3-IO
               MOVE LOCMAST-IO-AREA (64:9) TO FLDIV3 (1:9)
               MOVE LOCMAST-IO-AREA (73:20) TO FLREC4 (1:20)
               MOVE LOCMAST-IO-AREA (73:6) TO FLLOC4 (1:6)
               MOVE LOCMAST-IO-AREA (79:5) TO FLBEH4-IO
               MOVE LOCMAST-IO-AREA (84:9) TO FLDIV4 (1:9)
           END-EVALUATE.
 
       LOCMAST-IDSET SECTION.
       LOCMAST-IDSET-P.
           SET I-01                        TO TRUE.
 
       LOCMAST-CHK-LEVEL SECTION.
       LOCMAST-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO LOCMAST-LEVEL-01
               MOVE LOCMAST-IO-AREA (1:3)  TO LOCMAST-01-L2-FIRMA
               MOVE LOCMAST-IO-AREA (4:7)  TO LOCMAST-01-L1-EDBNR
               IF  LOCMAST-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  LOCMAST-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  LOCMAST-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  LOCMAST-01-L2         TO THE-PRIOR-L2
               MOVE  LOCMAST-01-L1         TO THE-PRIOR-L1
               SET LOCMAST-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (8:30)  TO FINAVN (1:30)
               MOVE FIRMAF-IO-AREA (123:1) TO FIRMSL (1:1)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-02                        TO TRUE.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (13:3) TO ALFA (1:3)
               MOVE VAREMAS-IO-AREA (16:20) TO ARTNR (1:20)
               MOVE VAREMAS-IO-AREA (97:5) TO ANTINN-IO
               MOVE VAREMAS-IO-AREA (102:5) TO ANTUT-IO
               MOVE VAREMAS-IO-AREA (140:6) TO LLOC (1:6)
               MOVE VAREMAS-IO-AREA (179:3) TO LAG13-IO
               MOVE VAREMAS-IO-AREA (182:3) TO LAG93-IO
               MOVE VAREMAS-IO-AREA (185:3) TO LAG15-IO
               MOVE VAREMAS-IO-AREA (188:3) TO LAG17-IO
               MOVE VAREMAS-IO-AREA (191:3) TO LAG92-IO
               MOVE VAREMAS-IO-AREA (194:3) TO LAG18-IO
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-03                        TO TRUE.
 
       HJFILE-FLDSET SECTION.
       HJFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE HJFILE-IO-AREA (17:4)  TO ANTVL-IO
               INSPECT ANTVL-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       HJFILE-IDSET SECTION.
       HJFILE-IDSET-P.
           SET I-04                        TO TRUE.
 
       PRINT-X-PRINT-LINE SECTION.
       PRINT-X-PRINT-LINE-P.
           IF  PRINT-X-BEFORE-SKIP > 0
               PERFORM PRINT-X-SKIP-BEFORE
           END-IF
           IF  PRINT-X-BEFORE-SPACE > 0
               PERFORM PRINT-X-SPACE-BEFORE
               IF  PRINT-X-AFTER-SKIP > 0
                   PERFORM PRINT-X-SKIP-AFTER
               END-IF
               IF  PRINT-X-AFTER-SPACE > 0
                   PERFORM PRINT-X-SPACE-AFTER
               END-IF
           ELSE
               IF  PRINT-X-AFTER-SKIP > 0
                   PERFORM PRINT-X-SKIP-AFTER
               END-IF
               PERFORM PRINT-X-SPACE-AFTER
           END-IF
           IF  PRINT-X-LINE-COUNT NOT < PRINT-X-MAX-LINES
               SET I-OF                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OF            TO TRUE
               END-IF
           END-IF.
 
       PRINT-X-SKIP-BEFORE SECTION.
       PRINT-X-SKIP-BEFORE-P.
           WRITE PRINT-X-IO-PRINT       AFTER ADVANCING PAGE
           MOVE 1                          TO PRINT-X-LINE-COUNT
           MOVE 0                          TO PRINT-X-BEFORE-SKIP
           INITIALIZE PRINT-X-IO-AREA.
 
       PRINT-X-SPACE-BEFORE SECTION.
       PRINT-X-SPACE-BEFORE-P.
           WRITE PRINT-X-IO-PRINT       AFTER PRINT-X-BEFORE-SPACE
                                                                 LINES
           ADD PRINT-X-BEFORE-SPACE        TO PRINT-X-LINE-COUNT
           MOVE SPACES TO PRINT-X-IO-AREA
           INITIALIZE PRINT-X-IO-AREA
           MOVE 0                          TO PRINT-X-BEFORE-SPACE.
 
       PRINT-X-SKIP-AFTER SECTION.
       PRINT-X-SKIP-AFTER-P.
           WRITE PRINT-X-IO-PRINT      BEFORE ADVANCING PAGE
           MOVE 1                          TO PRINT-X-LINE-COUNT
           MOVE 0                          TO PRINT-X-AFTER-SKIP
           INITIALIZE PRINT-X-IO-AREA.
 
       PRINT-X-SPACE-AFTER SECTION.
       PRINT-X-SPACE-AFTER-P.
           WRITE PRINT-X-IO-PRINT      BEFORE PRINT-X-AFTER-SPACE LINES
           ADD PRINT-X-AFTER-SPACE         TO PRINT-X-LINE-COUNT
           INITIALIZE PRINT-X-IO-AREA
           MOVE 0                          TO PRINT-X-AFTER-SPACE.
 
       EXCEPTION-OUTPUT SECTION.
       EXCEPTION-OUTPUT-P.
           IF  (I-70)
               MOVE SPACES TO OUTFILE-IO-AREA
               INITIALIZE OUTFILE-IO-AREA
               MOVE EDBKEY                 TO OUTFILE-IO-AREA (1:10)
               MOVE SEQNR-IO               TO OUTFILE-IO-AREA (11:2)
               MOVE FLOC1                  TO OUTFILE-IO-AREA (13:6)
               MOVE FBEH1                  TO XO-72P
               MOVE XO-72P-EF              TO OUTFILE-IO-AREA (19:5)
               MOVE FDIV1                  TO OUTFILE-IO-AREA (24:9)
               MOVE FLOC2                  TO OUTFILE-IO-AREA (33:6)
               MOVE FBEH2                  TO XO-72P
               MOVE XO-72P-EF              TO OUTFILE-IO-AREA (39:5)
               MOVE FDIV2                  TO OUTFILE-IO-AREA (44:9)
               MOVE FLOC3                  TO OUTFILE-IO-AREA (53:6)
               MOVE FBEH3                  TO XO-72P
               MOVE XO-72P-EF              TO OUTFILE-IO-AREA (59:5)
               MOVE FDIV3                  TO OUTFILE-IO-AREA (64:9)
               MOVE FLOC4                  TO OUTFILE-IO-AREA (73:6)
               MOVE FBEH4                  TO XO-72P
               MOVE XO-72P-EF              TO OUTFILE-IO-AREA (79:5)
               MOVE FDIV4                  TO OUTFILE-IO-AREA (84:9)
               WRITE OUTFILE-IO-AREA
           END-IF
           IF  (I-70 AND I-U2 AND I-22)
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE SEQNR-IO               TO PRINT-X-IO-AREA (23:2)
               MOVE EDBNR                  TO PRINT-X-IO-AREA (26:7)
               MOVE FLOC1                  TO PRINT-X-IO-AREA (34:6)
               MOVE FBEH1                  TO XO-72YY9R
               MOVE XO-72YY9R              TO PRINT-X-IO-AREA (55:13)
               MOVE 1                      TO PRINT-X-AFTER-SPACE
               PERFORM PRINT-X-PRINT-LINE
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE EDBNR                  TO PRINT-X-IO-AREA (26:7)
               MOVE FLOC2                  TO PRINT-X-IO-AREA (34:6)
               MOVE FBEH2                  TO XO-72YY9R
               MOVE XO-72YY9R              TO PRINT-X-IO-AREA (55:13)
               MOVE 1                      TO PRINT-X-AFTER-SPACE
               PERFORM PRINT-X-PRINT-LINE
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE EDBNR                  TO PRINT-X-IO-AREA (26:7)
               MOVE FLOC3                  TO PRINT-X-IO-AREA (34:6)
               MOVE FBEH3                  TO XO-72YY9R
               MOVE XO-72YY9R              TO PRINT-X-IO-AREA (55:13)
               MOVE 1                      TO PRINT-X-AFTER-SPACE
               PERFORM PRINT-X-PRINT-LINE
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE EDBNR                  TO PRINT-X-IO-AREA (26:7)
               MOVE FLOC4                  TO PRINT-X-IO-AREA (34:6)
               MOVE FBEH4                  TO XO-72YY9R
               MOVE XO-72YY9R              TO PRINT-X-IO-AREA (55:13)
               MOVE 1                      TO PRINT-X-AFTER-SPACE
               PERFORM PRINT-X-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L2 AND I-22)
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE FINAVN                 TO PRINT-X-IO-AREA (1:30)
               MOVE 'REORGANISERING FLYTENDE ' TO PRINT-X-IO-AREA
                                                               (32:24)
               MOVE 'LAGER.'               TO PRINT-X-IO-AREA (56:6)
               MOVE 'FREMSTILT'            TO PRINT-X-IO-AREA (71:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO PRINT-X-IO-AREA (81:8)
               MOVE 01                     TO PRINT-X-BEFORE-SKIP
               MOVE 2                      TO PRINT-X-AFTER-SPACE
               PERFORM PRINT-X-PRINT-LINE
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE 'ALF'                  TO PRINT-X-IO-AREA (1:3)
               MOVE 'ARTIKKELNUMMER'       TO PRINT-X-IO-AREA (5:14)
               MOVE 'EDBNR  '              TO PRINT-X-IO-AREA (26:7)
               MOVE 'LAGLOC'               TO PRINT-X-IO-AREA (34:6)
               MOVE 'VARE BEHOLD'          TO PRINT-X-IO-AREA (42:11)
               MOVE 'FLYT BEHOLD'          TO PRINT-X-IO-AREA (56:11)
               MOVE 'MERKNADER'            TO PRINT-X-IO-AREA (69:9)
               MOVE 2                      TO PRINT-X-AFTER-SPACE
               PERFORM PRINT-X-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE FINAVN                 TO PRINT-X-IO-AREA (1:30)
               MOVE 'REORGANISERING FLYTENDE ' TO PRINT-X-IO-AREA
                                                               (32:24)
               MOVE 'LAGER.'               TO PRINT-X-IO-AREA (56:6)
               MOVE 'FREMSTILT'            TO PRINT-X-IO-AREA (71:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO PRINT-X-IO-AREA (81:8)
               MOVE 01                     TO PRINT-X-BEFORE-SKIP
               MOVE 2                      TO PRINT-X-AFTER-SPACE
               PERFORM PRINT-X-PRINT-LINE
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE 'ALF'                  TO PRINT-X-IO-AREA (1:3)
               MOVE 'ARTIKKELNUMMER'       TO PRINT-X-IO-AREA (5:14)
               MOVE 'EDBNR  '              TO PRINT-X-IO-AREA (26:7)
               MOVE 'LAGLOC'               TO PRINT-X-IO-AREA (34:6)
               MOVE 'VARE BEHOLD'          TO PRINT-X-IO-AREA (42:11)
               MOVE 'FLYT BEHOLD'          TO PRINT-X-IO-AREA (56:11)
               MOVE 'MERKNADER'            TO PRINT-X-IO-AREA (69:9)
               MOVE 2                      TO PRINT-X-AFTER-SPACE
               PERFORM PRINT-X-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-80 AND NOT-I-U1)
           AND (I-22 AND NOT-I-28)
           OR  (I-L1 AND I-72 AND NOT-I-U1)
           AND (I-22)
           OR  (I-L1 AND I-71 AND I-U1)
           AND (I-22)
           OR  (I-L1 AND I-71 AND NOT-I-U1)
           AND (I-22 AND I-28)
           OR  (I-L1 AND I-29 AND NOT-I-U1)
           AND (I-22 AND I-28)
           OR  (I-L1 AND I-80 AND I-U2)
           AND (I-22 AND NOT-I-28)
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE ALFA                   TO PRINT-X-IO-AREA (1:3)
               MOVE ARTNR                  TO PRINT-X-IO-AREA (5:20)
               MOVE EDBNR                  TO PRINT-X-IO-AREA (26:7)
               MOVE LLOC                   TO PRINT-X-IO-AREA (34:6)
               MOVE VBEH                   TO XO-72YY9R
               MOVE XO-72YY9R              TO PRINT-X-IO-AREA (41:13)
               MOVE FBEH                   TO XO-72YY9R
               MOVE XO-72YY9R              TO PRINT-X-IO-AREA (55:13)
               IF  (I-72)
                   MOVE 'SLETTET. FL.LAGER = 0   ' TO PRINT-X-IO-AREA
                                                               (69:24)
               END-IF
               IF  (I-71)
                   MOVE 'BEHOLDNINGS DIFFERANSE  ' TO PRINT-X-IO-AREA
                                                               (69:24)
               END-IF
               IF  (I-28 AND I-29)
                   MOVE 'BLANK I LOCATION' TO PRINT-X-IO-AREA (95:16)
      *                        FBEHL21  120
               END-IF
               EVALUATE TRUE
               WHEN (I-L1 AND I-80 AND NOT-I-U1)
               AND  (I-22 AND NOT-I-28)
                   MOVE 1                  TO PRINT-X-AFTER-SPACE
               WHEN (I-L1 AND I-72 AND NOT-I-U1)
               AND  (I-22)
                   MOVE 1                  TO PRINT-X-AFTER-SPACE
               WHEN (I-L1 AND I-71 AND I-U1)
               AND  (I-22)
                   MOVE 1                  TO PRINT-X-AFTER-SPACE
               WHEN (I-L1 AND I-71 AND NOT-I-U1)
               AND  (I-22 AND I-28)
                   MOVE 1                  TO PRINT-X-AFTER-SPACE
               WHEN (I-L1 AND I-29 AND NOT-I-U1)
               AND  (I-22 AND I-28)
                   MOVE 1                  TO PRINT-X-AFTER-SPACE
               WHEN (I-L1 AND I-80 AND I-U2)
               AND  (I-22 AND NOT-I-28)
                   MOVE 2                  TO PRINT-X-AFTER-SPACE
               END-EVALUATE
               PERFORM PRINT-X-PRINT-LINE
           END-IF
           IF  (I-L2 AND I-22)
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE '***'                  TO PRINT-X-IO-AREA (1:3)
               MOVE ANTL2                  TO XO-70YY9
               MOVE XO-70YY9               TO PRINT-X-IO-AREA (12:9)
               INITIALIZE ANTL2
               MOVE 'LOCREC LEST INN.'     TO PRINT-X-IO-AREA (26:16)
               MOVE 1                      TO PRINT-X-AFTER-SPACE
               PERFORM PRINT-X-PRINT-LINE
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE '***'                  TO PRINT-X-IO-AREA (1:3)
               MOVE ANTLS                  TO XO-70YY9
               MOVE XO-70YY9               TO PRINT-X-IO-AREA (12:9)
               INITIALIZE ANTLS
               MOVE 'LOCREC SLETTET  '     TO PRINT-X-IO-AREA (26:16)
               MOVE 1                      TO PRINT-X-AFTER-SPACE
               PERFORM PRINT-X-PRINT-LINE
               MOVE SPACES TO PRINT-X-IO-AREA
               INITIALIZE PRINT-X-IO-AREA
               MOVE '***'                  TO PRINT-X-IO-AREA (1:3)
               MOVE ANTLU                  TO XO-70YY9
               MOVE XO-70YY9               TO PRINT-X-IO-AREA (12:9)
               INITIALIZE ANTLU
               MOVE 'LOCREC LAGT UT  '     TO PRINT-X-IO-AREA (26:16)
      *       T  1     L2 22
      *                                   3 "***"
      *                        FBEHL21   20
      *                                  41 "TOTALSUM ANTALL "
               MOVE 1                      TO PRINT-X-AFTER-SPACE
               PERFORM PRINT-X-PRINT-LINE
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
           INITIALIZE PAR-DATA-FIELDS
           SET PAR-EOF-OFF                 TO TRUE
           SET PAR-PROCESS                 TO TRUE
           OPEN INPUT PAR
           SET LOCMAST-LEVEL-INIT          TO TRUE
           INITIALIZE LOCMAST-DATA-FIELDS
           SET LOCMAST-EOF-OFF             TO TRUE
           SET LOCMAST-PROCESS             TO TRUE
           OPEN INPUT LOCMAST
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           INITIALIZE VAREMAS-DATA-FIELDS
           OPEN INPUT VAREMAS
           INITIALIZE HJFILE-DATA-FIELDS
           OPEN INPUT HJFILE
           OPEN OUTPUT OUTFILE
           OPEN OUTPUT PRINT-X
           INITIALIZE PRINT-X-IO-AREA
           INITIALIZE PRINT-X-DATA-FIELDS
           MOVE 57                         TO PRINT-X-MAX-LINES.
           PERFORM VARYING ARL-I FROM 1 BY 1
                     UNTIL ARL-I > ARL-MAX
               INITIALIZE ARL (ARL-I)
           END-PERFORM
           SET ARL-I                       TO 1
           PERFORM VARYING ARB-I FROM 1 BY 1
                     UNTIL ARB-I > ARB-MAX
               INITIALIZE ARB (ARB-I)
           END-PERFORM
           SET ARB-I                       TO 1
           PERFORM VARYING ARD-I FROM 1 BY 1
                     UNTIL ARD-I > ARD-MAX
               INITIALIZE ARD (ARD-I)
           END-PERFORM
           SET ARD-I                       TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PAR
           CLOSE LOCMAST
           CLOSE FIRMAF
           CLOSE VAREMAS
           CLOSE HJFILE
           CLOSE OUTFILE
           IF PRINT-X-IO-AREA NOT = SPACES
             WRITE PRINT-X-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO PRINT-X-IO-AREA
           END-IF
           CLOSE PRINT-X.
 
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
