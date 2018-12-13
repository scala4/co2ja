       IDENTIFICATION DIVISION.
       PROGRAM-ID. VAT023R.
      *********************************************************************
      **KVITTERINGSLISTE VBEK.SAMLE.FILE  MÅNEDLIG
      ** GJELDER  V B E K  UT I FRA AUTO.TRANSER1
      *********************************************************************
      *                                          XX2000XXIRXXSS
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VAT023.rpg
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
           SELECT FAKPAR
               ASSIGN TO UT-S-FAKPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKPAR-STATUS.
           SELECT INPUT-X
               ASSIGN TO UT-S-INPUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INPUT-X-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
           SELECT SUMFIL
               ASSIGN TO UT-S-SUMFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS SUMFIL-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD PARAM
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  PARAM-IO-AREA.
           05  PARAM-IO-AREA-X             PICTURE X(80).
       FD FAKPAR
               BLOCK CONTAINS 200
               RECORD CONTAINS 200.
       01  FAKPAR-IO-AREA.
           05  FAKPAR-IO-AREA-X            PICTURE X(200).
       FD INPUT-X
               BLOCK CONTAINS 9000
               RECORD CONTAINS 120.
       01  INPUT-X-IO-AREA.
           05  INPUT-X-IO-AREA-X           PICTURE X(120).
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
      * **START RBS - DATASTRUKTUR FOR SUB-PROGRAM RBS000 ********
       FD SUMFIL
               BLOCK CONTAINS 160
               RECORD CONTAINS 80.
       01  SUMFIL-IO-AREA.
           05  SUMFIL-IO-AREA-X            PICTURE X(80).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  PARAM-STATUS                PICTURE 99 VALUE 0.
           10  FAKPAR-STATUS               PICTURE 99 VALUE 0.
           10  INPUT-X-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  SUMFIL-STATUS               PICTURE 99 VALUE 0.
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
               10  PMND                    PICTURE X(2).
               10  PNAVN                   PICTURE X(10).
           05  FAKPAR-DATA-FIELDS.
               10  PFADTO                  PICTURE X(6).
               10  PFDAG                   PICTURE X(2).
               10  PFMND                   PICTURE X(2).
               10  PFAAR                   PICTURE X(2).
      **************************************************************
      *    RUTINE FOR OVERSTYRING AV RBS-FILE VED BESTILLINGSJOB"S *
      **************************************************************
           05  INPUT-X-LEVEL-01.
               10  INPUT-X-01-L5.
                   15  INPUT-X-01-L5-FIRMA PICTURE X(3).
               10  INPUT-X-01-L4.
                   15  INPUT-X-01-L4-AVD   PICTURE X(1).
               10  INPUT-X-01-L3.
                   15  INPUT-X-01-L3-VGR   PICTURE X(5).
               10  INPUT-X-01-L2.
                   15  INPUT-X-01-L2-ALFA  PICTURE X(3).
               10  INPUT-X-01-L1.
                   15  INPUT-X-01-L1-EDBNR PICTURE X(7).
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
               10  MND                     PICTURE X(2).
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  VGR                     PICTURE X(5).
               10  AVD                     PICTURE X(1).
               10  VERDI-IO.
                   15  VERDI               PICTURE S9(7)V9(2).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L5            PICTURE X(3).
               10  THE-PRIOR-L4            PICTURE X(1).
               10  THE-PRIOR-L3            PICTURE X(5).
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(7).
           05  TEMPORARY-FIELDS.
               10  NULL11-IO.
                   15  NULL11              PICTURE S9(9)V9(2).
               10  A1DMND-IO.
                   15  A1DMND              PICTURE S9(7).
               10  A1AKK-IO.
                   15  A1AKK               PICTURE S9(7).
               10  A1MV-IO.
                   15  A1MV                PICTURE S9(7).
               10  A1AKKV-IO.
                   15  A1AKKV              PICTURE S9(7).
               10  A1L3M-IO.
                   15  A1L3M               PICTURE S9(7).
               10  A1L3A-IO.
                   15  A1L3A               PICTURE S9(7).
               10  A1L3MV-IO.
                   15  A1L3MV              PICTURE S9(7).
               10  A1L3AV-IO.
                   15  A1L3AV              PICTURE S9(7).
               10  A1L4M-IO.
                   15  A1L4M               PICTURE S9(7).
               10  A1L4A-IO.
                   15  A1L4A               PICTURE S9(7).
               10  A1L4MV-IO.
                   15  A1L4MV              PICTURE S9(7).
               10  A1L4AV-IO.
                   15  A1L4AV              PICTURE S9(7).
               10  A1TOT1-IO.
                   15  A1TOT1              PICTURE S9(7).
               10  A1TOT2-IO.
                   15  A1TOT2              PICTURE S9(7).
               10  A1TOT3-IO.
                   15  A1TOT3              PICTURE S9(7).
               10  A1TOT4-IO.
                   15  A1TOT4              PICTURE S9(7).
               10  A1TOT5-IO.
                   15  A1TOT5              PICTURE S9(11).
               10  V2MND-IO.
                   15  V2MND               PICTURE S9(7).
               10  V2AKK-IO.
                   15  V2AKK               PICTURE S9(7).
               10  V2MV-IO.
                   15  V2MV                PICTURE S9(7).
               10  V2AV-IO.
                   15  V2AV                PICTURE S9(7).
               10  V2L3M-IO.
                   15  V2L3M               PICTURE S9(7).
               10  V2L3A-IO.
                   15  V2L3A               PICTURE S9(7).
               10  V2L3MV-IO.
                   15  V2L3MV              PICTURE S9(7).
               10  V2L3AV-IO.
                   15  V2L3AV              PICTURE S9(7).
               10  V2L4M-IO.
                   15  V2L4M               PICTURE S9(7).
               10  V2L4A-IO.
                   15  V2L4A               PICTURE S9(7).
               10  V2L4MV-IO.
                   15  V2L4MV              PICTURE S9(7).
               10  V2L4AV-IO.
                   15  V2L4AV              PICTURE S9(7).
               10  V2TOT1-IO.
                   15  V2TOT1              PICTURE S9(7).
               10  V2TOT2-IO.
                   15  V2TOT2              PICTURE S9(7).
               10  V2TOT3-IO.
                   15  V2TOT3              PICTURE S9(7).
               10  V2TOT4-IO.
                   15  V2TOT4              PICTURE S9(7).
               10  V3MND-IO.
                   15  V3MND               PICTURE S9(7).
               10  V3AKK-IO.
                   15  V3AKK               PICTURE S9(7).
               10  V3MV-IO.
                   15  V3MV                PICTURE S9(7).
               10  V3AV-IO.
                   15  V3AV                PICTURE S9(7).
               10  V3L3M-IO.
                   15  V3L3M               PICTURE S9(7).
               10  V3L3A-IO.
                   15  V3L3A               PICTURE S9(7).
               10  V3L3MV-IO.
                   15  V3L3MV              PICTURE S9(7).
               10  V3L3AV-IO.
                   15  V3L3AV              PICTURE S9(7).
               10  V3L4M-IO.
                   15  V3L4M               PICTURE S9(7).
               10  V3L4A-IO.
                   15  V3L4A               PICTURE S9(7).
               10  V3L4MV-IO.
                   15  V3L4MV              PICTURE S9(7).
               10  V3L4AV-IO.
                   15  V3L4AV              PICTURE S9(7).
               10  V3TOT1-IO.
                   15  V3TOT1              PICTURE S9(7).
               10  V3TOT2-IO.
                   15  V3TOT2              PICTURE S9(7).
               10  V3TOT3-IO.
                   15  V3TOT3              PICTURE S9(7).
               10  V3TOT4-IO.
                   15  V3TOT4              PICTURE S9(7).
               10  V4DMND-IO.
                   15  V4DMND              PICTURE S9(7).
               10  V4AKK-IO.
                   15  V4AKK               PICTURE S9(7).
               10  V4MV-IO.
                   15  V4MV                PICTURE S9(7).
               10  V4AV-IO.
                   15  V4AV                PICTURE S9(7).
               10  V4L3M-IO.
                   15  V4L3M               PICTURE S9(7).
               10  V4L3A-IO.
                   15  V4L3A               PICTURE S9(7).
               10  V4L3MV-IO.
                   15  V4L3MV              PICTURE S9(7).
               10  V4L3AV-IO.
                   15  V4L3AV              PICTURE S9(7).
               10  V4L4M-IO.
                   15  V4L4M               PICTURE S9(7).
               10  V4L4A-IO.
                   15  V4L4A               PICTURE S9(7).
               10  V4L4MV-IO.
                   15  V4L4MV              PICTURE S9(7).
               10  V4L4AV-IO.
                   15  V4L4AV              PICTURE S9(7).
               10  V4TOT1-IO.
                   15  V4TOT1              PICTURE S9(7).
               10  V4TOT2-IO.
                   15  V4TOT2              PICTURE S9(7).
               10  V4TOT3-IO.
                   15  V4TOT3              PICTURE S9(7).
               10  V4TOT4-IO.
                   15  V4TOT4              PICTURE S9(7).
               10  V4TOT5-IO.
                   15  V4TOT5              PICTURE S9(11).
               10  V5DMND-IO.
                   15  V5DMND              PICTURE S9(7).
               10  V5AKK-IO.
                   15  V5AKK               PICTURE S9(7).
               10  V5MV-IO.
                   15  V5MV                PICTURE S9(7).
               10  V5AV-IO.
                   15  V5AV                PICTURE S9(7).
               10  V5L3M-IO.
                   15  V5L3M               PICTURE S9(7).
               10  V5L3A-IO.
                   15  V5L3A               PICTURE S9(7).
               10  V5L3MV-IO.
                   15  V5L3MV              PICTURE S9(7).
               10  V5L3AV-IO.
                   15  V5L3AV              PICTURE S9(7).
               10  V5L4M-IO.
                   15  V5L4M               PICTURE S9(7).
               10  V5L4A-IO.
                   15  V5L4A               PICTURE S9(7).
               10  V5L4MV-IO.
                   15  V5L4MV              PICTURE S9(7).
               10  V5L4AV-IO.
                   15  V5L4AV              PICTURE S9(7).
               10  V5TOT1-IO.
                   15  V5TOT1              PICTURE S9(7).
               10  V5TOT2-IO.
                   15  V5TOT2              PICTURE S9(7).
               10  V5TOT3-IO.
                   15  V5TOT3              PICTURE S9(7).
               10  V5TOT4-IO.
                   15  V5TOT4              PICTURE S9(7).
           05  EDITTING-FIELDS.
               10  XO-40YNZ                PICTURE ZZZZ.
               10  XO-70YY9R               PICTURE Z.ZZZ.ZZ9-.
               10  XO-92P-EF.
                 15  XO-92P                PICTURE S9(9)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-110P-EF.
                 15  XO-110P               PICTURE S9(11) USAGE
                                                       PACKED-DECIMAL.
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
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
           PERFORM HEADING-OUTPUT
           IF  NOT-SET-I-OV
               SET NOT-I-OV                TO TRUE
           END-IF
           SET NOT-SET-I-OV                TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-81                    TO TRUE
           SET NOT-I-82                    TO TRUE
           SET NOT-I-83                    TO TRUE
           SET NOT-I-84                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-09                    TO TRUE
           SET NOT-I-01                    TO TRUE
 
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
 
           IF  PARAM-PROCESS
               PERFORM PARAM-IDSET
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
 
           IF  PARAM-PROCESS
               PERFORM PARAM-FLDSET
           END-IF
 
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
           END-IF
           IF  (I-L5)
               MOVE 0                      TO NULL11
               MOVE 0                      TO A1TOT1
               MOVE 0                      TO A1TOT2
               MOVE 0                      TO V2TOT1
               MOVE 0                      TO V2TOT2
               MOVE 0                      TO V3TOT1
               MOVE 0                      TO V3TOT2
               MOVE 0                      TO V4TOT1
               MOVE 0                      TO V4TOT2
               MOVE 0                      TO V5TOT1
               MOVE 0                      TO V5TOT2
               MOVE 0                      TO A1TOT3
               MOVE 0                      TO A1TOT4
               MOVE 0                      TO V2TOT3
               MOVE 0                      TO V2TOT4
               MOVE 0                      TO V3TOT3
               MOVE 0                      TO V3TOT4
               MOVE 0                      TO V4TOT3
               MOVE 0                      TO V4TOT4
               MOVE 0                      TO V5TOT3
               MOVE 0                      TO V5TOT4
      *
      *        NULLSTILLE BRUDDSUMMER FOR AVDELINGEN.
      *
           END-IF
           IF  (I-L4)
               MOVE 0                      TO A1L4M
               MOVE 0                      TO A1L4A
               MOVE 0                      TO A1L4MV
               MOVE 0                      TO A1L4AV
               MOVE 0                      TO V2L4M
               MOVE 0                      TO V2L4A
               MOVE 0                      TO V2L4MV
               MOVE 0                      TO V2L4AV
               MOVE 0                      TO V3L4M
               MOVE 0                      TO V3L4A
               MOVE 0                      TO V3L4MV
               MOVE 0                      TO V3L4AV
               MOVE 0                      TO V4L4M
               MOVE 0                      TO V4L4A
               MOVE 0                      TO V4L4MV
               MOVE 0                      TO V4L4AV
               MOVE 0                      TO V5L4M
               MOVE 0                      TO V5L4A
               MOVE 0                      TO V5L4MV
               MOVE 0                      TO V5L4AV
      *
      *        NULLSTILLE BRUDDSUMMER FOR VAREGRUPPEN.
      *
           END-IF
           IF  (I-L3)
               MOVE 0                      TO A1L3M
               MOVE 0                      TO A1L3A
               MOVE 0                      TO A1L3MV
               MOVE 0                      TO A1L3AV
               MOVE 0                      TO V2L3M
               MOVE 0                      TO V2L3A
               MOVE 0                      TO V2L3MV
               MOVE 0                      TO V2L3AV
               MOVE 0                      TO V3L3M
               MOVE 0                      TO V3L3A
               MOVE 0                      TO V3L3MV
               MOVE 0                      TO V3L3AV
               MOVE 0                      TO V4L3M
               MOVE 0                      TO V4L3A
               MOVE 0                      TO V4L3MV
               MOVE 0                      TO V4L3AV
               MOVE 0                      TO V5L3M
               MOVE 0                      TO V5L3A
               MOVE 0                      TO V5L3MV
               MOVE 0                      TO V5L3AV
      *
      *        NULLSTILLE BRUDDSUMMER FOR ARTIKKELEN.
      *
           END-IF
           IF  (I-L1)
               MOVE 0                      TO A1DMND
               MOVE 0                      TO A1AKK
               MOVE 0                      TO A1MV
               MOVE 0                      TO A1AKKV
               MOVE 0                      TO V2MND
               MOVE 0                      TO V2AKK
               MOVE 0                      TO V2MV
               MOVE 0                      TO V2AV
               MOVE 0                      TO V3MND
               MOVE 0                      TO V3AKK
               MOVE 0                      TO V3MV
               MOVE 0                      TO V3AV
               MOVE 0                      TO V4DMND
               MOVE 0                      TO V4AKK
               MOVE 0                      TO V4MV
               MOVE 0                      TO V4AV
               MOVE 0                      TO V5DMND
               MOVE 0                      TO V5AKK
               MOVE 0                      TO V5MV
               MOVE 0                      TO V5AV
           END-IF
           IF  (I-03 AND NOT-I-59)
               READ FAKPAR
               AT END
                   SET I-H0                TO TRUE
                   MOVE 'M'                TO E-R-R-O-R
               NOT AT END
                   PERFORM FAKPAR-FLDSET
                   PERFORM FAKPAR-IDSET
               END-READ
           END-IF
           IF  (I-03)
               SET I-59                    TO TRUE
           END-IF
           SET NOT-I-10                    TO TRUE
           IF  MND = '  '
               SET I-10                    TO TRUE
           END-IF
           IF  (I-10)
               MOVE 01                     TO MND
           END-IF
           SET NOT-I-11                    TO TRUE
           IF  MND = PMND
               SET I-11                    TO TRUE
           END-IF
           SET NOT-I-22                    TO TRUE
           IF  RECART = 'V2'
               SET I-22                    TO TRUE
           END-IF
           SET NOT-I-23                    TO TRUE
           IF  RECART = 'V3'
               SET I-23                    TO TRUE
           END-IF
           IF  (NOT-I-23)
               SET NOT-I-23                TO TRUE
               IF  RECART = 'V5'
                   SET I-23                TO TRUE
               END-IF
           END-IF
           SET NOT-I-24                    TO TRUE
           IF  RECART = 'V4'
               SET I-24                    TO TRUE
           END-IF
           SET NOT-I-26                    TO TRUE
           IF  RECART = 'A1'
               SET I-26                    TO TRUE
           END-IF
           SET NOT-I-30                    TO TRUE
           IF  SIGN-X = '+'
               SET I-30                    TO TRUE
           END-IF
           IF  (I-L2)
               SET NOT-I-40                TO TRUE
               IF  ALFA = '  '
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (I-L5)
               PERFORM RBSRUT-S
      *
      *     T E K O   -DENNE MND. OG AKKUMULERT.
      *
           END-IF
           IF  (I-30 AND I-26 AND I-11)
               ADD ANT                     TO A1DMND
           END-IF
           IF  (NOT-I-30 AND I-26 AND I-11)
               SUBTRACT ANT                FROM A1DMND
           END-IF
           IF  (I-30 AND I-26)
               ADD ANT                     TO A1AKK
           END-IF
           IF  (NOT-I-30 AND I-26)
               SUBTRACT ANT                FROM A1AKK
      *
           END-IF
           IF  (I-30 AND I-26 AND I-11)
               ADD VERDI                   TO A1MV
           END-IF
           IF  (NOT-I-30 AND I-26 AND I-11)
               SUBTRACT VERDI              FROM A1MV
           END-IF
           IF  (I-30 AND I-26)
               ADD VERDI                   TO A1AKKV
           END-IF
           IF  (NOT-I-30 AND I-26)
               SUBTRACT VERDI              FROM A1AKKV
      *
      *          TOTALER FOR VAREGRUPPE
      *
           END-IF
           IF  (I-30 AND I-26 AND I-11)
               ADD ANT                     TO A1L3M
           END-IF
           IF  (NOT-I-30 AND I-26 AND I-11)
               SUBTRACT ANT                FROM A1L3M
           END-IF
           IF  (I-30 AND I-26)
               ADD ANT                     TO A1L3A
           END-IF
           IF  (NOT-I-30 AND I-26)
               SUBTRACT ANT                FROM A1L3A
      *
           END-IF
           IF  (I-30 AND I-26 AND I-11)
               ADD VERDI                   TO A1L3MV
           END-IF
           IF  (NOT-I-30 AND I-26 AND I-11)
               SUBTRACT VERDI              FROM A1L3MV
           END-IF
           IF  (I-30 AND I-26)
               ADD VERDI                   TO A1L3AV
           END-IF
           IF  (NOT-I-30 AND I-26)
               SUBTRACT VERDI              FROM A1L3AV
      *
      *          TOTALER FOR AVDELINGEN
      *
           END-IF
           IF  (I-30 AND I-26 AND I-11)
               ADD ANT                     TO A1L4M
           END-IF
           IF  (NOT-I-30 AND I-26 AND I-11)
               SUBTRACT ANT                FROM A1L4M
           END-IF
           IF  (I-30 AND I-26)
               ADD ANT                     TO A1L4A
           END-IF
           IF  (NOT-I-30 AND I-26)
               SUBTRACT ANT                FROM A1L4A
      *
           END-IF
           IF  (I-30 AND I-26 AND I-11)
               ADD VERDI                   TO A1L4MV
           END-IF
           IF  (NOT-I-30 AND I-26 AND I-11)
               SUBTRACT VERDI              FROM A1L4MV
           END-IF
           IF  (I-30 AND I-26)
               ADD VERDI                   TO A1L4AV
           END-IF
           IF  (NOT-I-30 AND I-26)
               SUBTRACT VERDI              FROM A1L4AV
      *
      *          TOTALER FOR FIRMA
      *
           END-IF
           IF  (I-30 AND I-26 AND I-11)
               ADD ANT                     TO A1TOT1
           END-IF
           IF  (NOT-I-30 AND I-26 AND I-11)
               SUBTRACT ANT                FROM A1TOT1
           END-IF
           IF  (I-30 AND I-26)
               ADD ANT                     TO A1TOT2
           END-IF
           IF  (NOT-I-30 AND I-26)
               SUBTRACT ANT                FROM A1TOT2
      *
           END-IF
           IF  (I-30 AND I-26 AND I-11)
               ADD VERDI                   TO A1TOT3
           END-IF
           IF  (NOT-I-30 AND I-26 AND I-11)
               SUBTRACT VERDI              FROM A1TOT3
           END-IF
           IF  (I-30 AND I-26)
               ADD VERDI                   TO A1TOT4
           END-IF
           IF  (NOT-I-30 AND I-26)
               SUBTRACT VERDI              FROM A1TOT4
           END-IF
           ADD A1TOT4 TO ZERO          GIVING A1TOT5
      *
      *     V A T I +  -DENNE MND. OG AKKUMULERT.
      *
           IF  (I-30 AND I-22 AND I-11)
               ADD ANT                     TO V2MND
           END-IF
           IF  (I-30 AND I-22)
               ADD ANT                     TO V2AKK
      *
           END-IF
           IF  (I-30 AND I-22 AND I-11)
               ADD VERDI                   TO V2MV
           END-IF
           IF  (I-30 AND I-22)
               ADD VERDI                   TO V2AV
      *
      *          TOTALER FOR VAREGRUPPE
      *
           END-IF
           IF  (I-30 AND I-22 AND I-11)
               ADD ANT                     TO V2L3M
           END-IF
           IF  (I-30 AND I-22)
               ADD ANT                     TO V2L3A
      *
           END-IF
           IF  (I-30 AND I-22 AND I-11)
               ADD VERDI                   TO V2L3MV
           END-IF
           IF  (I-30 AND I-22)
               ADD VERDI                   TO V2L3AV
      *
      *          TOTALER FOR AVDELINGEN
      *
           END-IF
           IF  (I-30 AND I-22 AND I-11)
               ADD ANT                     TO V2L4M
           END-IF
           IF  (I-30 AND I-22)
               ADD ANT                     TO V2L4A
      *
           END-IF
           IF  (I-30 AND I-22 AND I-11)
               ADD VERDI                   TO V2L4MV
           END-IF
           IF  (I-30 AND I-22)
               ADD VERDI                   TO V2L4AV
      *
      *          TOTALER FOR FIRMA
      *
           END-IF
           IF  (I-30 AND I-22 AND I-11)
               ADD ANT                     TO V2TOT1
           END-IF
           IF  (I-30 AND I-22)
               ADD ANT                     TO V2TOT2
      *
           END-IF
           IF  (I-30 AND I-22 AND I-11)
               ADD VERDI                   TO V2TOT3
           END-IF
           IF  (I-30 AND I-22)
               ADD VERDI                   TO V2TOT4
      *
      *     V A T I -  -DENNE MND. OG AKKUMULERT.
      *
           END-IF
           IF  (NOT-I-30 AND I-22 AND I-11)
               SUBTRACT ANT                FROM V3MND
           END-IF
           IF  (NOT-I-30 AND I-22)
               SUBTRACT ANT                FROM V3AKK
      *
           END-IF
           IF  (NOT-I-30 AND I-22 AND I-11)
               SUBTRACT VERDI              FROM V3MV
           END-IF
           IF  (NOT-I-30 AND I-22)
               SUBTRACT VERDI              FROM V3AV
      *
      *          TOTALER FOR VAREGRUPPE
      *
           END-IF
           IF  (NOT-I-30 AND I-22 AND I-11)
               SUBTRACT ANT                FROM V3L3M
           END-IF
           IF  (NOT-I-30 AND I-22)
               SUBTRACT ANT                FROM V3L3A
      *
           END-IF
           IF  (NOT-I-30 AND I-22 AND I-11)
               SUBTRACT VERDI              FROM V3L3MV
           END-IF
           IF  (NOT-I-30 AND I-22)
               SUBTRACT VERDI              FROM V3L3AV
      *
      *          TOTALER FOR AVDELINGEN
      *
           END-IF
           IF  (NOT-I-30 AND I-22 AND I-11)
               SUBTRACT ANT                FROM V3L4M
           END-IF
           IF  (NOT-I-30 AND I-22)
               SUBTRACT ANT                FROM V3L4A
      *
           END-IF
           IF  (NOT-I-30 AND I-22 AND I-11)
               SUBTRACT VERDI              FROM V3L4MV
           END-IF
           IF  (NOT-I-30 AND I-22)
               SUBTRACT VERDI              FROM V3L4AV
      *
      *          TOTALER FOR FIRMA
      *
           END-IF
           IF  (NOT-I-30 AND I-22 AND I-11)
               SUBTRACT ANT                FROM V3TOT1
           END-IF
           IF  (NOT-I-30 AND I-22)
               SUBTRACT ANT                FROM V3TOT2
      *
           END-IF
           IF  (NOT-I-30 AND I-22 AND I-11)
               SUBTRACT VERDI              FROM V3TOT3
           END-IF
           IF  (NOT-I-30 AND I-22)
               SUBTRACT VERDI              FROM V3TOT4
      *
      *     K O R R   -DENNE MND. OG AKKUMULERT.
      *
           END-IF
           IF  (I-30 AND I-24 AND I-11)
               ADD ANT                     TO V4DMND
           END-IF
           IF  (NOT-I-30 AND I-24 AND I-11)
               SUBTRACT ANT                FROM V4DMND
           END-IF
           IF  (I-30 AND I-24)
               ADD ANT                     TO V4AKK
           END-IF
           IF  (NOT-I-30 AND I-24)
               SUBTRACT ANT                FROM V4AKK
      *
           END-IF
           IF  (I-30 AND I-24 AND I-11)
               ADD VERDI                   TO V4MV
           END-IF
           IF  (NOT-I-30 AND I-24 AND I-11)
               SUBTRACT VERDI              FROM V4MV
           END-IF
           IF  (I-30 AND I-24)
               ADD VERDI                   TO V4AV
           END-IF
           IF  (NOT-I-30 AND I-24)
               SUBTRACT VERDI              FROM V4AV
      *
      *          TOTALER FOR VAREGRUPPE
      *
           END-IF
           IF  (I-30 AND I-24 AND I-11)
               ADD ANT                     TO V4L3M
           END-IF
           IF  (NOT-I-30 AND I-24 AND I-11)
               SUBTRACT ANT                FROM V4L3M
           END-IF
           IF  (I-30 AND I-24)
               ADD ANT                     TO V4L3A
           END-IF
           IF  (NOT-I-30 AND I-24)
               SUBTRACT ANT                FROM V4L3A
      *
           END-IF
           IF  (I-30 AND I-24 AND I-11)
               ADD VERDI                   TO V4L3MV
           END-IF
           IF  (NOT-I-30 AND I-24 AND I-11)
               SUBTRACT VERDI              FROM V4L3MV
           END-IF
           IF  (I-30 AND I-24)
               ADD VERDI                   TO V4L3AV
           END-IF
           IF  (NOT-I-30 AND I-24)
               SUBTRACT VERDI              FROM V4L3AV
      *
      *          TOTALER FOR AVDELINGEN
      *
           END-IF
           IF  (I-30 AND I-24 AND I-11)
               ADD ANT                     TO V4L4M
           END-IF
           IF  (NOT-I-30 AND I-24 AND I-11)
               SUBTRACT ANT                FROM V4L4M
           END-IF
           IF  (I-30 AND I-24)
               ADD ANT                     TO V4L4A
           END-IF
           IF  (NOT-I-30 AND I-24)
               SUBTRACT ANT                FROM V4L4A
      *
           END-IF
           IF  (I-30 AND I-24 AND I-11)
               ADD VERDI                   TO V4L4MV
           END-IF
           IF  (NOT-I-30 AND I-24 AND I-11)
               SUBTRACT VERDI              FROM V4L4MV
           END-IF
           IF  (I-30 AND I-24)
               ADD VERDI                   TO V4L4AV
           END-IF
           IF  (NOT-I-30 AND I-24)
               SUBTRACT VERDI              FROM V4L4AV
      *
      *          TOTALER FOR FIRMA
      *
           END-IF
           IF  (I-30 AND I-24 AND I-11)
               ADD ANT                     TO V4TOT1
           END-IF
           IF  (NOT-I-30 AND I-24 AND I-11)
               SUBTRACT ANT                FROM V4TOT1
           END-IF
           IF  (I-30 AND I-24)
               ADD ANT                     TO V4TOT2
           END-IF
           IF  (NOT-I-30 AND I-24)
               SUBTRACT ANT                FROM V4TOT2
      *
           END-IF
           IF  (I-30 AND I-24 AND I-11)
               ADD VERDI                   TO V4TOT3
           END-IF
           IF  (NOT-I-30 AND I-24 AND I-11)
               SUBTRACT VERDI              FROM V4TOT3
           END-IF
           IF  (I-30 AND I-24)
               ADD VERDI                   TO V4TOT4
           END-IF
           IF  (NOT-I-30 AND I-24)
               SUBTRACT VERDI              FROM V4TOT4
           END-IF
           ADD V4TOT4 TO ZERO          GIVING V4TOT5
      *
      *     V E R K / U T T A K   -DENNE MND. OG AKKUMULERT.
      *
           IF  (I-30 AND I-23 AND I-11)
               ADD ANT                     TO V5DMND
           END-IF
           IF  (NOT-I-30 AND I-23 AND I-11)
               SUBTRACT ANT                FROM V5DMND
           END-IF
           IF  (I-30 AND I-23)
               ADD ANT                     TO V5AKK
           END-IF
           IF  (NOT-I-30 AND I-23)
               SUBTRACT ANT                FROM V5AKK
      *
           END-IF
           IF  (I-30 AND I-23 AND I-11)
               ADD VERDI                   TO V5MV
           END-IF
           IF  (NOT-I-30 AND I-23 AND I-11)
               SUBTRACT VERDI              FROM V5MV
           END-IF
           IF  (I-30 AND I-23)
               ADD VERDI                   TO V5AV
           END-IF
           IF  (NOT-I-30 AND I-23)
               SUBTRACT VERDI              FROM V5AV
      *
      *          TOTALER FOR VAREGRUPPE
      *
           END-IF
           IF  (I-30 AND I-23 AND I-11)
               ADD ANT                     TO V5L3M
           END-IF
           IF  (NOT-I-30 AND I-23 AND I-11)
               SUBTRACT ANT                FROM V5L3M
           END-IF
           IF  (I-30 AND I-23)
               ADD ANT                     TO V5L3A
           END-IF
           IF  (NOT-I-30 AND I-23)
               SUBTRACT ANT                FROM V5L3A
      *
           END-IF
           IF  (I-30 AND I-23 AND I-11)
               ADD VERDI                   TO V5L3MV
           END-IF
           IF  (NOT-I-30 AND I-23 AND I-11)
               SUBTRACT VERDI              FROM V5L3MV
           END-IF
           IF  (I-30 AND I-23)
               ADD VERDI                   TO V5L3AV
           END-IF
           IF  (NOT-I-30 AND I-23)
               SUBTRACT VERDI              FROM V5L3AV
      *
      *          TOTALER FOR AVDELINGEN
      *
           END-IF
           IF  (I-30 AND I-23 AND I-11)
               ADD ANT                     TO V5L4M
           END-IF
           IF  (NOT-I-30 AND I-23 AND I-11)
               SUBTRACT ANT                FROM V5L4M
           END-IF
           IF  (I-30 AND I-23)
               ADD ANT                     TO V5L4A
           END-IF
           IF  (NOT-I-30 AND I-23)
               SUBTRACT ANT                FROM V5L4A
      *
           END-IF
           IF  (I-30 AND I-23 AND I-11)
               ADD VERDI                   TO V5L4MV
           END-IF
           IF  (NOT-I-30 AND I-23 AND I-11)
               SUBTRACT VERDI              FROM V5L4MV
           END-IF
           IF  (I-30 AND I-23)
               ADD VERDI                   TO V5L4AV
           END-IF
           IF  (NOT-I-30 AND I-23)
               SUBTRACT VERDI              FROM V5L4AV
      *
      *          TOTALER FOR FIRMA
      *
           END-IF
           IF  (I-30 AND I-23 AND I-11)
               ADD ANT                     TO V5TOT1
           END-IF
           IF  (NOT-I-30 AND I-23 AND I-11)
               SUBTRACT ANT                FROM V5TOT1
           END-IF
           IF  (I-30 AND I-23)
               ADD ANT                     TO V5TOT2
           END-IF
           IF  (NOT-I-30 AND I-23)
               SUBTRACT ANT                FROM V5TOT2
      *
           END-IF
           IF  (I-30 AND I-23 AND I-11)
               ADD VERDI                   TO V5TOT3
           END-IF
           IF  (NOT-I-30 AND I-23 AND I-11)
               SUBTRACT VERDI              FROM V5TOT3
           END-IF
           IF  (I-30 AND I-23)
               ADD VERDI                   TO V5TOT4
           END-IF
           IF  (NOT-I-30 AND I-23)
               SUBTRACT VERDI              FROM V5TOT4
      ******************************************************
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
           MOVE 'BKO03'                    TO LONR
           MOVE FIRMA                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'VAT023  '                 TO LPROG
           MOVE 'XDOP82AM'                 TO BJOBN
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
           WHEN ( PARAM-IO-AREA (1:1) = '9'
            AND   PARAM-IO-AREA (2:1) = '0' )
               MOVE PARAM-IO-AREA (15:2)   TO PMND (1:2)
               MOVE PARAM-IO-AREA (18:10)  TO PNAVN (1:10)
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
             OR ( PARAM-IO-AREA (1:1) = '9'
            AND   PARAM-IO-AREA (2:1) = '0' )
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
           WHEN ( PARAM-IO-AREA (1:1) = '9'
            AND   PARAM-IO-AREA (2:1) = '0' )
               SET I-03                    TO TRUE
           END-EVALUATE.
 
       FAKPAR-FLDSET SECTION.
       FAKPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKPAR-IO-AREA (18:6)  TO PFADTO (1:6)
               MOVE FAKPAR-IO-AREA (18:2)  TO PFDAG (1:2)
               MOVE FAKPAR-IO-AREA (20:2)  TO PFMND (1:2)
               MOVE FAKPAR-IO-AREA (22:2)  TO PFAAR (1:2)
           END-EVALUATE.
 
       FAKPAR-IDSET SECTION.
       FAKPAR-IDSET-P.
           SET I-09                        TO TRUE.
 
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
               MOVE INPUT-X-IO-AREA (60:2) TO MND (1:2)
               MOVE INPUT-X-IO-AREA (101:3) TO ALFA (1:3)
               MOVE INPUT-X-IO-AREA (81:20) TO ARTNR (1:20)
               MOVE INPUT-X-IO-AREA (104:5) TO VGR (1:5)
               MOVE INPUT-X-IO-AREA (104:1) TO AVD (1:1)
               MOVE INPUT-X-IO-AREA (109:9) TO VERDI-IO
               INSPECT VERDI-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       INPUT-X-IDSET SECTION.
       INPUT-X-IDSET-P.
           SET I-01                        TO TRUE.
 
       INPUT-X-CHK-LEVEL SECTION.
       INPUT-X-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO INPUT-X-LEVEL-01
               MOVE INPUT-X-IO-AREA (3:3)  TO INPUT-X-01-L5-FIRMA
               MOVE INPUT-X-IO-AREA (104:1) TO INPUT-X-01-L4-AVD
               MOVE INPUT-X-IO-AREA (104:5) TO INPUT-X-01-L3-VGR
               MOVE INPUT-X-IO-AREA (101:3) TO INPUT-X-01-L2-ALFA
               MOVE INPUT-X-IO-AREA (18:7) TO INPUT-X-01-L1-EDBNR
               IF  INPUT-X-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INPUT-X-01-L5 NOT = THE-PRIOR-L5
                       PERFORM SETON-I-L5
                   WHEN  INPUT-X-01-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   WHEN  INPUT-X-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  INPUT-X-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INPUT-X-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INPUT-X-01-L5         TO THE-PRIOR-L5
               MOVE  INPUT-X-01-L4         TO THE-PRIOR-L4
               MOVE  INPUT-X-01-L3         TO THE-PRIOR-L3
               MOVE  INPUT-X-01-L2         TO THE-PRIOR-L2
               MOVE  INPUT-X-01-L1         TO THE-PRIOR-L1
               SET INPUT-X-LEVEL-INIT      TO TRUE
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
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L5 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE LOPNVN                 TO LISTE-IO-AREA (32:35)
               MOVE 'FOR'                  TO LISTE-IO-AREA (68:3)
               MOVE PNAVN                  TO LISTE-IO-AREA (72:10)
               MOVE 'DATO.'                TO LISTE-IO-AREA (96:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (103:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (123:4)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (129:4)
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
               MOVE 'TEKO'                 TO LISTE-IO-AREA (39:4)
               MOVE 'VATI +'               TO LISTE-IO-AREA (60:6)
               MOVE 'VATI -'               TO LISTE-IO-AREA (82:6)
               MOVE 'KORR'                 TO LISTE-IO-AREA (103:4)
               MOVE 'VERK/UTTAK'           TO LISTE-IO-AREA (121:10)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'D.MND'                TO LISTE-IO-AREA (34:5)
               MOVE 'AKKUM'                TO LISTE-IO-AREA (44:5)
               MOVE 'D.MND'                TO LISTE-IO-AREA (55:5)
               MOVE 'AKKUM'                TO LISTE-IO-AREA (65:5)
               MOVE 'D.MND'                TO LISTE-IO-AREA (77:5)
               MOVE 'AKKUM'                TO LISTE-IO-AREA (87:5)
               MOVE 'D.MND'                TO LISTE-IO-AREA (98:5)
               MOVE 'AKKUM'                TO LISTE-IO-AREA (108:5)
               MOVE 'D.MND'                TO LISTE-IO-AREA (119:5)
               MOVE 'AKKUM'                TO LISTE-IO-AREA (128:5)
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
           IF  (I-OV AND NOT-I-L5 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE LOPNVN                 TO LISTE-IO-AREA (32:35)
               MOVE 'FOR'                  TO LISTE-IO-AREA (68:3)
               MOVE PNAVN                  TO LISTE-IO-AREA (72:10)
               MOVE 'DATO.'                TO LISTE-IO-AREA (96:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (103:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (123:4)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (129:4)
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
               MOVE 'TEKO'                 TO LISTE-IO-AREA (39:4)
               MOVE 'VATI +'               TO LISTE-IO-AREA (60:6)
               MOVE 'VATI -'               TO LISTE-IO-AREA (82:6)
               MOVE 'KORR'                 TO LISTE-IO-AREA (103:4)
               MOVE 'VERK/UTTAK'           TO LISTE-IO-AREA (121:10)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'D.MND'                TO LISTE-IO-AREA (34:5)
               MOVE 'AKKUM'                TO LISTE-IO-AREA (44:5)
               MOVE 'D.MND'                TO LISTE-IO-AREA (55:5)
               MOVE 'AKKUM'                TO LISTE-IO-AREA (65:5)
               MOVE 'D.MND'                TO LISTE-IO-AREA (77:5)
               MOVE 'AKKUM'                TO LISTE-IO-AREA (87:5)
               MOVE 'D.MND'                TO LISTE-IO-AREA (98:5)
               MOVE 'AKKUM'                TO LISTE-IO-AREA (108:5)
               MOVE 'D.MND'                TO LISTE-IO-AREA (119:5)
               MOVE 'AKKUM'                TO LISTE-IO-AREA (128:5)
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
               MOVE ALFA                   TO LISTE-IO-AREA (1:3)
               MOVE ARTNR                  TO LISTE-IO-AREA (5:20)
               MOVE 'ANT:'                 TO LISTE-IO-AREA (26:4)
               MOVE A1DMND                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (29:10)
               INITIALIZE A1DMND
               MOVE A1AKK                  TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (39:10)
               INITIALIZE A1AKK
               MOVE V2MND                  TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (50:10)
               INITIALIZE V2MND
               MOVE V2AKK                  TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (60:10)
               INITIALIZE V2AKK
               MOVE V3MND                  TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (72:10)
               INITIALIZE V3MND
               MOVE V3AKK                  TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (82:10)
               INITIALIZE V3AKK
               MOVE V4DMND                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (93:10)
               INITIALIZE V4DMND
               MOVE V4AKK                  TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (103:10)
               INITIALIZE V4AKK
               MOVE V5DMND                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (114:10)
               INITIALIZE V5DMND
               MOVE V5AKK                  TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (123:10)
               INITIALIZE V5AKK
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               IF  (I-40)
                   MOVE 'EDBNR='           TO LISTE-IO-AREA (5:6)
               END-IF
               IF  (I-40)
                   MOVE EDBNR              TO LISTE-IO-AREA (11:7)
               END-IF
               MOVE 'VERDI:'               TO LISTE-IO-AREA (24:6)
               MOVE A1MV                   TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (29:10)
               INITIALIZE A1MV
               MOVE A1AKKV                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (39:10)
               INITIALIZE A1AKKV
               MOVE V2MV                   TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (50:10)
               INITIALIZE V2MV
               MOVE V2AV                   TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (60:10)
               INITIALIZE V2AV
               MOVE V3MV                   TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (72:10)
               INITIALIZE V3MV
               MOVE V3AV                   TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (82:10)
               INITIALIZE V3AV
               MOVE V4MV                   TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (93:10)
               INITIALIZE V4MV
               MOVE V4AV                   TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (103:10)
               INITIALIZE V4AV
               MOVE V5MV                   TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (114:10)
               INITIALIZE V5MV
               MOVE V5AV                   TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (123:10)
               INITIALIZE V5AV
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L3 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALT FOR VGR: '     TO LISTE-IO-AREA (1:16)
               MOVE VGR                    TO LISTE-IO-AREA (17:5)
               MOVE 'ANT:'                 TO LISTE-IO-AREA (26:4)
               MOVE A1L3M                  TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (29:10)
               INITIALIZE A1L3M
               MOVE A1L3A                  TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (39:10)
               INITIALIZE A1L3A
               MOVE V2L3M                  TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (50:10)
               INITIALIZE V2L3M
               MOVE V2L3A                  TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (60:10)
               INITIALIZE V2L3A
               MOVE V3L3M                  TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (72:10)
               INITIALIZE V3L3M
               MOVE V3L3A                  TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (82:10)
               INITIALIZE V3L3A
               MOVE V4L3M                  TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (93:10)
               INITIALIZE V4L3M
               MOVE V4L3A                  TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (103:10)
               INITIALIZE V4L3A
               MOVE V5L3M                  TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (114:10)
               INITIALIZE V5L3M
               MOVE V5L3A                  TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (123:10)
               INITIALIZE V5L3A
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'VERDI:'               TO LISTE-IO-AREA (24:6)
               MOVE A1L3MV                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (29:10)
               INITIALIZE A1L3MV
               MOVE A1L3AV                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (39:10)
               INITIALIZE A1L3AV
               MOVE V2L3MV                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (50:10)
               INITIALIZE V2L3MV
               MOVE V2L3AV                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (60:10)
               INITIALIZE V2L3AV
               MOVE V3L3MV                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (72:10)
               INITIALIZE V3L3MV
               MOVE V3L3AV                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (82:10)
               INITIALIZE V3L3AV
               MOVE V4L3MV                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (93:10)
               INITIALIZE V4L3MV
               MOVE V4L3AV                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (103:10)
               INITIALIZE V4L3AV
               MOVE V5L3MV                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (114:10)
               INITIALIZE V5L3MV
               MOVE V5L3AV                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (123:10)
               INITIALIZE V5L3AV
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L4 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALT FOR AVD: '     TO LISTE-IO-AREA (1:16)
               MOVE AVD                    TO LISTE-IO-AREA (21:1)
               MOVE 'ANT:'                 TO LISTE-IO-AREA (26:4)
               MOVE A1L4M                  TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (29:10)
               INITIALIZE A1L4M
               MOVE A1L4A                  TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (39:10)
               INITIALIZE A1L4A
               MOVE V2L4M                  TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (50:10)
               INITIALIZE V2L4M
               MOVE V2L4A                  TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (60:10)
               INITIALIZE V2L4A
               MOVE V3L4M                  TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (72:10)
               INITIALIZE V3L4M
               MOVE V3L4A                  TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (82:10)
               INITIALIZE V3L4A
               MOVE V4L4M                  TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (93:10)
               INITIALIZE V4L4M
               MOVE V4L4A                  TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (103:10)
               INITIALIZE V4L4A
               MOVE V5L4M                  TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (114:10)
               INITIALIZE V5L4M
               MOVE V5L4A                  TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (123:10)
               INITIALIZE V5L4A
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'VERDI:'               TO LISTE-IO-AREA (24:6)
               MOVE A1L4MV                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (29:10)
               INITIALIZE A1L4MV
               MOVE A1L4AV                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (39:10)
               INITIALIZE A1L4AV
               MOVE V2L4MV                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (50:10)
               INITIALIZE V2L4MV
               MOVE V2L4AV                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (60:10)
               INITIALIZE V2L4AV
               MOVE V3L4MV                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (72:10)
               INITIALIZE V3L4MV
               MOVE V3L4AV                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (82:10)
               INITIALIZE V3L4AV
               MOVE V4L4MV                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (93:10)
               INITIALIZE V4L4MV
               MOVE V4L4AV                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (103:10)
               INITIALIZE V4L4AV
               MOVE V5L4MV                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (114:10)
               INITIALIZE V5L4MV
               MOVE V5L4AV                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (123:10)
               INITIALIZE V5L4AV
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L5 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'T O T A L T  FOR FIRMA' TO LISTE-IO-AREA (2:22)
               MOVE 'ANT:'                 TO LISTE-IO-AREA (26:4)
               MOVE A1TOT1                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (29:10)
               INITIALIZE A1TOT1
               MOVE A1TOT2                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (39:10)
               INITIALIZE A1TOT2
               MOVE V2TOT1                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (50:10)
               INITIALIZE V2TOT1
               MOVE V2TOT2                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (60:10)
               INITIALIZE V2TOT2
               MOVE V3TOT1                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (72:10)
               INITIALIZE V3TOT1
               MOVE V3TOT2                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (82:10)
               INITIALIZE V3TOT2
               MOVE V4TOT1                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (93:10)
               INITIALIZE V4TOT1
               MOVE V4TOT2                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (103:10)
               INITIALIZE V4TOT2
               MOVE V5TOT1                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (114:10)
               INITIALIZE V5TOT1
               MOVE V5TOT2                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (123:10)
               INITIALIZE V5TOT2
               MOVE 2                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'VERDI:'               TO LISTE-IO-AREA (24:6)
               MOVE A1TOT3                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (29:10)
               INITIALIZE A1TOT3
               MOVE A1TOT4                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (39:10)
               INITIALIZE A1TOT4
               MOVE V2TOT3                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (50:10)
               INITIALIZE V2TOT3
               MOVE V2TOT4                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (60:10)
               INITIALIZE V2TOT4
               MOVE V3TOT3                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (72:10)
               INITIALIZE V3TOT3
               MOVE V3TOT4                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (82:10)
               INITIALIZE V3TOT4
               MOVE V4TOT3                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (93:10)
               INITIALIZE V4TOT3
               MOVE V4TOT4                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (103:10)
               INITIALIZE V4TOT4
               MOVE V5TOT3                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (114:10)
               INITIALIZE V5TOT3
               MOVE V5TOT4                 TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (123:10)
               INITIALIZE V5TOT4
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L5)
               MOVE SPACES TO SUMFIL-IO-AREA
               INITIALIZE SUMFIL-IO-AREA
               MOVE FIRMA                  TO SUMFIL-IO-AREA (1:3)
               MOVE '*M'                   TO SUMFIL-IO-AREA (4:2)
               MOVE PFAAR                  TO SUMFIL-IO-AREA (6:2)
               MOVE PFMND                  TO SUMFIL-IO-AREA (8:2)
               MOVE PFDAG                  TO SUMFIL-IO-AREA (10:2)
               MOVE NULL11                 TO XO-92P
               MOVE XO-92P-EF              TO SUMFIL-IO-AREA (12:6)
               MOVE NULL11                 TO XO-92P
               MOVE XO-92P-EF              TO SUMFIL-IO-AREA (18:6)
               MOVE NULL11                 TO XO-92P
               MOVE XO-92P-EF              TO SUMFIL-IO-AREA (24:6)
               MOVE A1TOT5                 TO XO-110P
               MOVE XO-110P-EF             TO SUMFIL-IO-AREA (30:6)
               INITIALIZE A1TOT5
               MOVE V4TOT5                 TO XO-110P
               MOVE XO-110P-EF             TO SUMFIL-IO-AREA (36:6)
               INITIALIZE V4TOT5
               MOVE NULL11                 TO XO-92P
               MOVE XO-92P-EF              TO SUMFIL-IO-AREA (42:6)
               WRITE SUMFIL-IO-AREA
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
           INITIALIZE FAKPAR-DATA-FIELDS
           OPEN INPUT FAKPAR
           SET INPUT-X-LEVEL-INIT          TO TRUE
           INITIALIZE INPUT-X-DATA-FIELDS
           SET INPUT-X-EOF-OFF             TO TRUE
           SET INPUT-X-PROCESS             TO TRUE
           OPEN INPUT INPUT-X
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES
           OPEN OUTPUT SUMFIL.
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PARAM
           CLOSE FAKPAR
           CLOSE INPUT-X
           IF LISTE-IO-AREA NOT = SPACES
             WRITE LISTE-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE-IO-AREA
           END-IF
           CLOSE LISTE
           CLOSE SUMFIL.
 
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
