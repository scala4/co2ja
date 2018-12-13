       IDENTIFICATION DIVISION.
       PROGRAM-ID. VOS129R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAM   : VOS129, VERKSTEDORDRE - IKKE FERDIG PR DATO.      *
      *           : DANNER SUMFILE TIL STA12.                         *
      *                     TILSVARER PROGRAM ORD129.                 *
      * LAGET     : 29.09.03                                          *
      * ENDRET    : 29.09.03                                          *
      * INPUT     : PARAMETER (PAR),                                  *
      *             ORDREMASTER (ORDREM).                             *
      * BEHANDLING: SKRIVER IKKE FERDIGE VERKSTEDORDRE PR DATO FRA    *
      *             PARAMETER FOR KUNDER SOM HAR VALGT RAPPORTEN I RBS*
      * OUTPUT    : LISTE MED IKKE FERDIGE ORDRE.                     *
      * OUTPUT    : LEGGER UT FIRMATOTALER P≈ EGEN FILE.              *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VOS129.rpg
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
           SELECT FAKPAR
               ASSIGN TO UT-S-FAKPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKPAR-STATUS.
           SELECT ORDREM
               ASSIGN TO UT-S-ORDREM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ORDREM-STATUS.
           SELECT SUMFILV
               ASSIGN TO UT-S-SUMFILV
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS SUMFILV-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD PAR
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  PAR-IO-AREA.
           05  PAR-IO-AREA-X               PICTURE X(80).
       FD FAKPAR
               BLOCK CONTAINS 200
               RECORD CONTAINS 200.
       01  FAKPAR-IO-AREA.
           05  FAKPAR-IO-AREA-X            PICTURE X(200).
       FD ORDREM
               RECORD CONTAINS 164.
       01  ORDREM-IO-AREA.
           05  ORDREM-IO-AREA-X            PICTURE X(164).
       FD SUMFILV
               BLOCK CONTAINS 160
               RECORD CONTAINS 80.
       01  SUMFILV-IO-AREA.
           05  SUMFILV-IO-AREA-X           PICTURE X(80).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  PAR-STATUS                  PICTURE 99 VALUE 0.
           10  FAKPAR-STATUS               PICTURE 99 VALUE 0.
           10  ORDREM-STATUS               PICTURE 99 VALUE 0.
           10  SUMFILV-STATUS              PICTURE 99 VALUE 0.
           10  DATOER-XX-STATUS            PICTURE 99 VALUE 0.
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
           05  FILLER                      PIC X VALUE '0'.
               88  ORDREM-EOF-OFF          VALUE '0'.
               88  ORDREM-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDREM-READ-OFF         VALUE '0'.
               88  ORDREM-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDREM-PROCESS-OFF      VALUE '0'.
               88  ORDREM-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  ORDREM-LEVEL-INIT-OFF   VALUE '0'.
               88  ORDREM-LEVEL-INIT       VALUE '1'.
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
           05  DATOER-XX-DATA-FIELDS.
               10  DATOK                   PICTURE X(1).
               10  FILLER                  PICTURE X(256).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(1).
               10  DATO6                   PICTURE X(6).
               10  FILLER                  PICTURE X(250).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(7).
               10  DMA8                    PICTURE X(8).
               10  FILLER                  PICTURE X(242).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(15).
               10  AMD8                    PICTURE X(8).
               10  FILLER                  PICTURE X(234).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(23).
               10  DATOM                   PICTURE X(57).
               10  FILLER                  PICTURE X(177).
           05  LDATA-XX REDEFINES DATOER-XX-DATA-FIELDS.
               10  LONR                    PICTURE X(5).
               10  FILLER                  PICTURE X(252).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(5).
               10  LFIRMA                  PICTURE X(3).
               10  FILLER                  PICTURE X(249).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(8).
               10  LUNDGR                  PICTURE X(3).
               10  FILLER                  PICTURE X(246).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(11).
               10  LPROG                   PICTURE X(8).
               10  FILLER                  PICTURE X(238).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(19).
               10  LANTX-IO.
                   15  LANTX               PICTURE S9(3).
               10  FILLER                  PICTURE X(235).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(22).
               10  FINAVN                  PICTURE X(30).
               10  FILLER                  PICTURE X(205).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(52).
               10  LOPNVN                  PICTURE X(35).
               10  FILLER                  PICTURE X(170).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(87).
               10  LPRIID                  PICTURE X(4).
               10  FILLER                  PICTURE X(166).
      *     *  BESTILLINGSOPPGAVER (OVERSTYRING AV RBS-FILE) *    *
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(91).
               10  BJOBN                   PICTURE X(8).
               10  FILLER                  PICTURE X(158).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(99).
               10  BBEST                   PICTURE X(1).
               10  FILLER                  PICTURE X(157).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(100).
               10  BPERS                   PICTURE X(30).
               10  FILLER                  PICTURE X(127).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(130).
               10  BETTB                   PICTURE X(40).
               10  FILLER                  PICTURE X(87).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(170).
               10  BFORS                   PICTURE X(40).
               10  FILLER                  PICTURE X(47).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(210).
               10  BMEMO                   PICTURE X(40).
               10  FILLER                  PICTURE X(7).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(250).
               10  BANTX-IO.
                   15  BANTX               PICTURE S9(3).
               10  FILLER                  PICTURE X(4).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(253).
               10  BPCLAS                  PICTURE X(1).
               10  FILLER                  PICTURE X(3).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
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
           05  PAR-DATA-FIELDS.
               10  PFIRMA                  PICTURE X(3).
               10  PDATO-IO.
                   15  PDATO               PICTURE S9(6).
               10  PDAG                    PICTURE X(2).
               10  PMND                    PICTURE X(2).
               10  PA-ELGR                 PICTURE X(2).
               10  PORDNR                  PICTURE X(6).
               10  ALLE                    PICTURE X(1).
               10  PAVD                    PICTURE X(1).
               10  POM                     PICTURE X(2).
           05  FAKPAR-DATA-FIELDS.
               10  PFADTO                  PICTURE X(6).
               10  PFDAG                   PICTURE X(2).
               10  PFMND                   PICTURE X(2).
               10  PFAAR                   PICTURE X(2).
           05  ORDREM-LEVEL-01.
               10  ORDREM-01-L3.
                   15  ORDREM-01-L3-FIRMA  PICTURE X(3).
               10  ORDREM-01-L2.
                   15  ORDREM-01-L2-OAVD   PICTURE X(1).
               10  ORDREM-01-L1.
                   15  ORDREM-01-L1-ORDNR  PICTURE X(6).
           05  ORDREM-LEVEL-02.
               10  ORDREM-02-L3.
                   15  ORDREM-02-L3-FIRMA  PICTURE X(3).
               10  ORDREM-02-L2.
                   15  ORDREM-02-L2-OAVD   PICTURE X(1).
               10  ORDREM-02-L1.
                   15  ORDREM-02-L1-ORDNR  PICTURE X(6).
           05  ORDREM-LEVEL-03.
               10  ORDREM-03-L3.
                   15  ORDREM-03-L3-FIRMA  PICTURE X(3).
               10  ORDREM-03-L2.
                   15  ORDREM-03-L2-OAVD   PICTURE X(1).
               10  ORDREM-03-L1.
                   15  ORDREM-03-L1-ORDNR  PICTURE X(6).
           05  ORDREM-LEVEL-04.
               10  ORDREM-04-L3.
                   15  ORDREM-04-L3-FIRMA  PICTURE X(3).
               10  ORDREM-04-L2.
                   15  ORDREM-04-L2-OAVD   PICTURE X(1).
               10  ORDREM-04-L1.
                   15  ORDREM-04-L1-ORDNR  PICTURE X(6).
           05  ORDREM-DATA-FIELDS.
               10  OAVD                    PICTURE X(1).
               10  FIRMA                   PICTURE X(3).
               10  ORDNR                   PICTURE X(6).
               10  KUNDNR                  PICTURE X(6).
               10  KNAVN1                  PICTURE X(30).
               10  KNAVN2                  PICTURE X(30).
               10  KTSIFF                  PICTURE X(1).
               10  SKAF                    PICTURE X(1).
               10  AVD                     PICTURE X(1).
               10  ORDATO-IO.
                   15  ORDATO              PICTURE S9(6).
               10  ORDMND                  PICTURE X(2).
               10  ORDDAG                  PICTURE X(2).
               10  ORDAAR                  PICTURE X(2).
               10  ORDMOT                  PICTURE X(2).
               10  STATUS-X                PICTURE X(1).
               10  AVNAVN                  PICTURE X(11).
               10  REKVNR                  PICTURE X(15).
               10  FORSM                   PICTURE X(15).
               10  KADR                    PICTURE X(30).
               10  POSTNR                  PICTURE X(4).
               10  PSTED                   PICTURE X(15).
               10  VAADR1                  PICTURE X(30).
               10  VAADR2                  PICTURE X(30).
               10  VAADR3                  PICTURE X(30).
               10  ANTBES-IO.
                   15  ANTBES              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ANTRES-IO.
                   15  ANTRES              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ALF                     PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  VARB20                  PICTURE X(20).
               10  EDBNR-IO.
                   15  EDBNR               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  ORPRIS-IO.
                   15  ORPRIS              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ORRAB1-IO.
                   15  ORRAB1              PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  ORRAB2-IO.
                   15  ORRAB2              PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  ORRAB3-IO.
                   15  ORRAB3              PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  PRITIL-IO.
                   15  PRITIL              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
      ******** INDIKATOR SETOF"S **********
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(1).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  ANTORD-IO.
                   15  ANTORD              PICTURE S9(5).
               10  ANTOL3-IO.
                   15  ANTOL3              PICTURE S9(5).
               10  FIRSL2-IO.
                   15  FIRSL2              PICTURE S9(11).
               10  FIRAL2-IO.
                   15  FIRAL2              PICTURE S9(11).
               10  NULL11-IO.
                   15  NULL11              PICTURE S9(11).
               10  PS4                     PICTURE X(4).
               10  PSDATO                  PICTURE X(8).
               10  OSDATO                  PICTURE X(8).
               10  ANTLEV-IO.
                   15  ANTLEV              PICTURE S9(5)V9(2).
               10  TILSUM-IO.
                   15  TILSUM              PICTURE S9(7)V9(2).
               10  SUM1-IO.
                   15  SUM1                PICTURE S9(7)V9(2).
               10  SUM2-IO.
                   15  SUM2                PICTURE S9(9)V9(2).
               10  ORDTOT-IO.
                   15  ORDTOT              PICTURE S9(7)V9(2).
               10  FIRTOT-IO.
                   15  FIRTOT              PICTURE S9(8)V9(2).
               10  AVDTOT-IO.
                   15  AVDTOT              PICTURE S9(8)V9(2).
           05  EDITTING-FIELDS.
               10  XO-110P-EF.
                 15  XO-110P               PICTURE S9(11) USAGE
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
 
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-10                    TO TRUE
           SET NOT-I-09                    TO TRUE
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
 
           IF  ORDREM-PROCESS
               SET ORDREM-PROCESS-OFF      TO TRUE
               SET ORDREM-READ             TO TRUE
           END-IF
 
           IF  ORDREM-READ
           AND RECORD-SELECTED-OFF
               PERFORM ORDREM-GET
               SET ORDREM-READ-OFF         TO TRUE
               IF  NOT ORDREM-EOF
                   PERFORM ORDREM-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET ORDREM-PROCESS      TO TRUE
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
 
           IF  ORDREM-PROCESS
               PERFORM ORDREM-IDSET
           END-IF
 
           IF  ORDREM-PROCESS
               PERFORM ORDREM-CHK-LEVEL
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
 
           IF  PAR-PROCESS
               PERFORM PAR-FLDOFF
               PERFORM PAR-FLDSET
           END-IF
 
           IF  ORDREM-PROCESS
               PERFORM ORDREM-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  ORDREM-PROCESS
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
               SET NOT-I-37                TO TRUE
      *  L3      FIRMA     COMP "905"                    45 AUTOUTSTYR ÿ
      *  L3N45   FIRMA     COMP "931"                    45 AUTOUTSTYR V
           END-IF
           IF  (I-L1)
               SET NOT-I-50                TO TRUE
               SET NOT-I-51                TO TRUE
               SET NOT-I-30                TO TRUE
               SET NOT-I-32                TO TRUE
               SET NOT-I-33                TO TRUE
               SET NOT-I-34                TO TRUE
               SET NOT-I-35                TO TRUE
               SET NOT-I-36                TO TRUE
           END-IF
           IF  (I-L3)
               MOVE 0                      TO FIRTOT
               MOVE 0                      TO ANTORD
           END-IF
           IF  (I-L2)
               MOVE 0                      TO AVDTOT
           END-IF
           IF  (I-L1)
               MOVE 0                      TO ORDTOT
           END-IF
           SET NOT-I-34                    TO TRUE
      ********* PARAMETERTEST *************
           IF  (I-10 AND NOT-I-59)
               READ FAKPAR
               AT END
                   SET I-H0                TO TRUE
                   MOVE 'M'                TO E-R-R-O-R
               NOT AT END
                   PERFORM FAKPAR-FLDSET
                   PERFORM FAKPAR-IDSET
               END-READ
           END-IF
           IF  (I-10)
               SET I-59                    TO TRUE
               PERFORM PARRUT-S
           END-IF
           IF  (I-10)
               GO TO UT-T
      *****************************************************************
      *        FIRMA TEST,  BLANK ER ALLE FIRMA                       *
      *  FIRMA 905 OG FIRMA 931 (AUTOUTSTYR) SKAL IKKE V∆RE MED.      *
      *****************************************************************
           END-IF
           IF  (I-L3 AND I-22)
               SET I-31                    TO TRUE
           END-IF
           IF  (I-L3 AND NOT-I-22)
               SET NOT-I-LR                TO TRUE
               SET NOT-I-31                TO TRUE
               IF  FIRMA > PFIRMA
                   SET I-LR                TO TRUE
               END-IF
               IF  FIRMA = PFIRMA
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-31)
               GO TO UT-T
      ******** ER DET AVDELINGSTEST   ?  ***************
           END-IF
           IF  (I-01 AND NOT-I-20 AND NOT-I-26)
               SET NOT-I-36                TO TRUE
               IF  AVD = PAVD
                   SET I-36                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-20 AND NOT-I-36 AND NOT-I-26)
               GO TO UT-T
      ******** ER DET ORDRE,MOTAGER   ?  ***************
           END-IF
           IF  (I-01 AND NOT-I-25)
               SET NOT-I-36                TO TRUE
               IF  ORDMOT = POM
                   SET I-36                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-25 AND NOT-I-36)
               GO TO UT-T
      ******** ER ORDEREN FERDIGMELDT ?  ***************
           END-IF
           IF  (I-01)
               SET NOT-I-32                TO TRUE
               IF  STATUS-X = 'R'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-32)
               SET NOT-I-32                TO TRUE
               IF  STATUS-X = 'K'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-32)
               SET NOT-I-32                TO TRUE
               IF  STATUS-X = 'P'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-32)
               SET NOT-I-32                TO TRUE
               IF  STATUS-X = 'S'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-32)
               SET NOT-I-32                TO TRUE
               IF  STATUS-X = 'J'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-32)
               SET NOT-I-32                TO TRUE
               IF  STATUS-X < 'A'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-32)
               GO TO UT-T
           END-IF
           IF  (I-01)
               SET NOT-I-61                TO TRUE
               SET NOT-I-62                TO TRUE
               SET NOT-I-63                TO TRUE
               SET NOT-I-64                TO TRUE
               SET NOT-I-65                TO TRUE
               SET NOT-I-66                TO TRUE
               SET NOT-I-67                TO TRUE
               SET NOT-I-68                TO TRUE
               SET NOT-I-69                TO TRUE
               SET NOT-I-61                TO TRUE
               IF  STATUS-X = 'R'
                   SET I-61                TO TRUE
               END-IF
               SET NOT-I-62                TO TRUE
               IF  STATUS-X = 'K'
                   SET I-62                TO TRUE
               END-IF
               SET NOT-I-63                TO TRUE
               IF  STATUS-X = 'P'
                   SET I-63                TO TRUE
               END-IF
               SET NOT-I-64                TO TRUE
               IF  STATUS-X = 'S'
                   SET I-64                TO TRUE
               END-IF
               SET NOT-I-65                TO TRUE
               IF  STATUS-X = 'J'
                   SET I-65                TO TRUE
               END-IF
               SET NOT-I-66                TO TRUE
               IF  STATUS-X < 'A'
                   SET I-66                TO TRUE
               END-IF
               SET NOT-I-67                TO TRUE
               IF  STATUS-X = 'U'
                   SET I-67                TO TRUE
               END-IF
               SET NOT-I-68                TO TRUE
               IF  STATUS-X = 'M'
                   SET I-68                TO TRUE
               END-IF
               SET NOT-I-69                TO TRUE
               IF  STATUS-X = 'L'
                   SET I-69                TO TRUE
               END-IF
      ******** ER ORDREDATO GAMMEL NOK ? KUN HVIS  DATO I PARAMETER. *
      ******** UPSI 8 = KUN DENNE DATO.                             *
           END-IF
           IF  (I-01 AND NOT-I-23)
               PERFORM DATRUT-S
           END-IF
           IF  (I-01 AND NOT-I-23 AND NOT-I-U8)
               SET NOT-I-30                TO TRUE
               IF  OSDATO NOT > PSDATO
                   SET I-30                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-23 AND I-U8)
               SET NOT-I-30                TO TRUE
               IF  ORDATO = PDATO
                   SET I-30                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-23 AND NOT-I-30)
               GO TO UT-T
      ******** ORDRENUMMER TEST *********
           END-IF
           IF  (I-01 AND NOT-I-21)
               SET NOT-I-30                TO TRUE
               IF  ORDNR = PORDNR
                   SET I-30                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-21 AND NOT-I-30)
               GO TO UT-T
      ******** DIVERSE  ***********"
           END-IF
           IF  (I-01)
               SET I-51                    TO TRUE
           END-IF
           IF  (I-03)
               SET I-33                    TO TRUE
           END-IF
           IF  (I-04 AND NOT-I-35)
               SET I-34                    TO TRUE
               SET I-35                    TO TRUE
               SET I-37                    TO TRUE
           END-IF
           IF  (I-04 AND I-34)
               ADD 1                       TO ANTORD
           END-IF
           IF  (I-04 AND I-30)
               PERFORM SUMRUT-S
           END-IF.
 
       UT-T.
      *****************************************************************
      * RUTINE FOR ≈ FANGE OPP IKKE FERDIGMELDTE ORDRE, SOM IKKE      *
      * HAR VARELINJER.                                               *
      *****************************************************************
           CONTINUE.
 
       PARRUT-S SECTION.
       PARRUT-S-P.
           MOVE 0                          TO NULL11
           SET NOT-I-26                    TO TRUE
           IF  PAVD = 'J'
               SET I-26                    TO TRUE
           END-IF
           MOVE PFAAR                      TO DATO6 (1:2)
           MOVE PFMND                      TO PS4 (1:2)
           MOVE PFDAG                      TO PS4 (3:2)
           MOVE PS4                        TO DATO6 (3:4)
           MOVE 'B'                        TO DATOK
           CALL 'DATO8SIF' USING DATOER-XX-DATA-FIELDS
           MOVE AMD8                       TO PSDATO.
      *****************************************************************
      * SUBRUTINE FOR ≈ SNU ORDRE-DATO TIL ≈≈≈≈MMDD                   *
      * ≈R 2000 SUBRUTINE FOR ≈ HENTE ≈RHUNDRE.                       *
      *****************************************************************
 
       DATRUT-S SECTION.
       DATRUT-S-P.
           MOVE ORDAAR                     TO DATO6 (1:2)
           MOVE ORDMND                     TO PS4 (1:2)
           MOVE ORDDAG                     TO PS4 (3:2)
           MOVE PS4                        TO DATO6 (3:4)
           MOVE 'B'                        TO DATOK
           CALL 'DATO8SIF' USING DATOER-XX-DATA-FIELDS
           MOVE AMD8                       TO OSDATO.
      **********************************************
      *  SUBRUTINE FOR ≈ SUMMERE ORDRE.            *
      **********************************************
 
       SUMRUT-S SECTION.
       SUMRUT-S-P.
           SUBTRACT ANTRES FROM ANTBES GIVING ANTLEV
           SET NOT-I-40                    TO TRUE
           IF  ANTLEV NOT > 0,00
               SET I-40                    TO TRUE
           END-IF
           IF  (I-40)
               MOVE 0,00                   TO ANTLEV
           END-IF
           SET NOT-I-41                    TO TRUE
           IF  ORPRIS = 0,00
               SET I-41                    TO TRUE
           END-IF
           SET NOT-I-42                    TO TRUE
           IF  ORDNR > '899999'
               SET I-42                    TO TRUE
           END-IF
           IF  (I-40)
               GO TO SUMEND-T
      * ********
           END-IF
           IF  (NOT-I-40)
               MULTIPLY PRITIL BY ANTLEV GIVING TILSUM
      *  40                Z-ADDPRITIL    TILSUM
           END-IF
           IF  (NOT-I-40)
               MULTIPLY ORPRIS BY ANTLEV GIVING SUM1
      *  40                Z-ADDORPRIS    SUM1
           END-IF
           MULTIPLY ORRAB1 BY SUM1     GIVING SUM2
           DIVIDE SUM2 BY 100          GIVING SUM2
           SUBTRACT SUM2                   FROM SUM1
           MULTIPLY ORRAB2 BY SUM1     GIVING SUM2
           DIVIDE SUM2 BY 100          GIVING SUM2
           SUBTRACT SUM2                   FROM SUM1
           MULTIPLY ORRAB3 BY SUM1     GIVING SUM2
           DIVIDE SUM2 BY 100          GIVING SUM2
           SUBTRACT SUM2                   FROM SUM1
           IF  (NOT-I-42)
               ADD SUM1                    TO ORDTOT
           END-IF
           IF  (I-42)
               SUBTRACT SUM1               FROM ORDTOT
           END-IF
           IF  (NOT-I-42)
               ADD SUM1                    TO FIRTOT
           END-IF
           IF  (I-42)
               SUBTRACT SUM1               FROM FIRTOT
           END-IF
           IF  (NOT-I-42)
               ADD SUM1                    TO AVDTOT
           END-IF
           IF  (I-42)
               SUBTRACT SUM1               FROM AVDTOT
           END-IF
           IF  (NOT-I-42)
               ADD TILSUM                  TO ORDTOT
           END-IF
           IF  (I-42)
               SUBTRACT TILSUM             FROM ORDTOT
           END-IF
           IF  (NOT-I-42)
               ADD TILSUM                  TO FIRTOT
           END-IF
           IF  (I-42)
               SUBTRACT TILSUM             FROM FIRTOT
           END-IF.
 
       SUMEND-T.
           CONTINUE.
      **********************************************
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1 AND I-51 AND NOT-I-35)
               SET I-50                    TO TRUE
           END-IF
           IF  (I-L1 AND I-50)
               ADD 1                       TO ANTOL3
           END-IF
           IF  (I-L3)
               ADD FIRTOT TO ZERO      GIVING FIRSL2 ROUNDED
               ADD ANTORD TO ZERO      GIVING FIRAL2
      *****************************************************************
      * SUBRUTINE FOR ≈ SNU PARAMETER-DATO TIL ≈≈≈≈MMDD.              *
      * ≈R 2000 SUBRUTINE FOR ≈ HENTE ≈RHUNDRE.                       *
      *****************************************************************
           END-IF
           .
 
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
               SET NOT-I-22                TO TRUE
               SET NOT-I-23                TO TRUE
               SET NOT-I-21                TO TRUE
               SET NOT-I-20                TO TRUE
               SET NOT-I-25                TO TRUE
           END-EVALUATE.
 
       PAR-FLDSET SECTION.
       PAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ( PAR-IO-AREA (1:1) = 'P' )
               MOVE PAR-IO-AREA (13:3)     TO PFIRMA (1:3)
               IF  PFIRMA = SPACES
                   SET I-22                TO TRUE
               END-IF
               MOVE PAR-IO-AREA (33:6)     TO PDATO-IO
               INSPECT PDATO-IO REPLACING ALL ' ' BY '0'
               IF  PDATO = ZERO
                   SET I-23                TO TRUE
               END-IF
               MOVE PAR-IO-AREA (33:2)     TO PDAG (1:2)
               MOVE PAR-IO-AREA (35:2)     TO PMND (1:2)
               MOVE PAR-IO-AREA (37:2)     TO PA-ELGR (1:2)
               MOVE PAR-IO-AREA (48:6)     TO PORDNR (1:6)
               IF  PORDNR = SPACES
                   SET I-21                TO TRUE
               END-IF
               MOVE PAR-IO-AREA (66:1)     TO ALLE (1:1)
               MOVE PAR-IO-AREA (72:1)     TO PAVD (1:1)
               IF  PAVD = SPACES
                   SET I-20                TO TRUE
               END-IF
               MOVE PAR-IO-AREA (77:2)     TO POM (1:2)
               IF  POM = SPACES
                   SET I-25                TO TRUE
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
 
       ORDREM-GET SECTION.
       ORDREM-GET-P.
           IF  ORDREM-EOF-OFF
               READ ORDREM
               AT END
                   SET ORDREM-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       ORDREM-FLDSET SECTION.
       ORDREM-FLDSET-P.
           EVALUATE TRUE
           WHEN ( ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '1' )
               MOVE ORDREM-IO-AREA (1:1)   TO OAVD (1:1)
               MOVE ORDREM-IO-AREA (2:3)   TO FIRMA (1:3)
               MOVE ORDREM-IO-AREA (5:6)   TO ORDNR (1:6)
               MOVE ORDREM-IO-AREA (21:6)  TO KUNDNR (1:6)
               MOVE ORDREM-IO-AREA (27:30) TO KNAVN1 (1:30)
               MOVE ORDREM-IO-AREA (57:30) TO KNAVN2 (1:30)
               MOVE ORDREM-IO-AREA (87:1)  TO KTSIFF (1:1)
               MOVE ORDREM-IO-AREA (93:1)  TO SKAF (1:1)
               MOVE ORDREM-IO-AREA (98:1)  TO AVD (1:1)
               MOVE ORDREM-IO-AREA (136:6) TO ORDATO-IO
               INSPECT ORDATO-IO REPLACING ALL ' ' BY '0'
               MOVE ORDREM-IO-AREA (138:2) TO ORDMND (1:2)
               MOVE ORDREM-IO-AREA (136:2) TO ORDDAG (1:2)
               MOVE ORDREM-IO-AREA (140:2) TO ORDAAR (1:2)
               MOVE ORDREM-IO-AREA (142:2) TO ORDMOT (1:2)
               MOVE ORDREM-IO-AREA (164:1) TO STATUS-X (1:1)
           WHEN ( ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '2' )
               MOVE ORDREM-IO-AREA (1:1)   TO OAVD (1:1)
               MOVE ORDREM-IO-AREA (2:3)   TO FIRMA (1:3)
               MOVE ORDREM-IO-AREA (5:6)   TO ORDNR (1:6)
               MOVE ORDREM-IO-AREA (50:11) TO AVNAVN (1:11)
               MOVE ORDREM-IO-AREA (67:15) TO REKVNR (1:15)
               MOVE ORDREM-IO-AREA (82:15) TO FORSM (1:15)
               MOVE ORDREM-IO-AREA (101:30) TO KADR (1:30)
               MOVE ORDREM-IO-AREA (131:4) TO POSTNR (1:4)
               MOVE ORDREM-IO-AREA (135:15) TO PSTED (1:15)
           WHEN ( ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '3' )
               MOVE ORDREM-IO-AREA (1:1)   TO OAVD (1:1)
               MOVE ORDREM-IO-AREA (2:3)   TO FIRMA (1:3)
               MOVE ORDREM-IO-AREA (5:6)   TO ORDNR (1:6)
               MOVE ORDREM-IO-AREA (21:30) TO VAADR1 (1:30)
               MOVE ORDREM-IO-AREA (51:30) TO VAADR2 (1:30)
               MOVE ORDREM-IO-AREA (81:30) TO VAADR3 (1:30)
           WHEN ( ORDREM-IO-AREA (19:1) NOT = ' ' )
               MOVE ORDREM-IO-AREA (1:1)   TO OAVD (1:1)
               MOVE ORDREM-IO-AREA (2:3)   TO FIRMA (1:3)
               MOVE ORDREM-IO-AREA (5:6)   TO ORDNR (1:6)
               MOVE ORDREM-IO-AREA (21:4)  TO ANTBES-IO
               MOVE ORDREM-IO-AREA (25:4)  TO ANTRES-IO
               MOVE ORDREM-IO-AREA (34:3)  TO ALF (1:3)
               MOVE ORDREM-IO-AREA (37:20) TO ARTNR (1:20)
               MOVE ORDREM-IO-AREA (57:20) TO VARB20 (1:20)
               MOVE ORDREM-IO-AREA (87:4)  TO EDBNR-IO
               MOVE ORDREM-IO-AREA (94:5)  TO ORPRIS-IO
               MOVE ORDREM-IO-AREA (99:2)  TO ORRAB1-IO
               MOVE ORDREM-IO-AREA (101:2) TO ORRAB2-IO
               MOVE ORDREM-IO-AREA (103:2) TO ORRAB3-IO
               MOVE ORDREM-IO-AREA (126:4) TO PRITIL-IO
           END-EVALUATE.
 
       ORDREM-IDCHK SECTION.
       ORDREM-IDCHK-P.
           EVALUATE TRUE
           WHEN ( ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '1' )
             OR ( ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '2' )
             OR ( ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '3' )
             OR ( ORDREM-IO-AREA (19:1) NOT = ' ' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       ORDREM-IDSET SECTION.
       ORDREM-IDSET-P.
           EVALUATE TRUE
           WHEN ( ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '1' )
               SET I-01                    TO TRUE
           WHEN ( ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '2' )
               SET I-02                    TO TRUE
           WHEN ( ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '3' )
               SET I-03                    TO TRUE
           WHEN ( ORDREM-IO-AREA (19:1) NOT = ' ' )
               SET I-04                    TO TRUE
           END-EVALUATE.
 
       ORDREM-CHK-LEVEL SECTION.
       ORDREM-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '1' )
               MOVE LOW-VALUES             TO ORDREM-LEVEL-01
               MOVE ORDREM-IO-AREA (2:3)   TO ORDREM-01-L3-FIRMA
               MOVE ORDREM-IO-AREA (1:1)   TO ORDREM-01-L2-OAVD
               MOVE ORDREM-IO-AREA (5:6)   TO ORDREM-01-L1-ORDNR
               IF  ORDREM-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDREM-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  ORDREM-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  ORDREM-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDREM-01-L3          TO THE-PRIOR-L3
               MOVE  ORDREM-01-L2          TO THE-PRIOR-L2
               MOVE  ORDREM-01-L1          TO THE-PRIOR-L1
               SET ORDREM-LEVEL-INIT       TO TRUE
           WHEN ( ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '2' )
               MOVE LOW-VALUES             TO ORDREM-LEVEL-02
               MOVE ORDREM-IO-AREA (2:3)   TO ORDREM-02-L3-FIRMA
               MOVE ORDREM-IO-AREA (1:1)   TO ORDREM-02-L2-OAVD
               MOVE ORDREM-IO-AREA (5:6)   TO ORDREM-02-L1-ORDNR
               IF  ORDREM-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDREM-02-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  ORDREM-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  ORDREM-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDREM-02-L3          TO THE-PRIOR-L3
               MOVE  ORDREM-02-L2          TO THE-PRIOR-L2
               MOVE  ORDREM-02-L1          TO THE-PRIOR-L1
               SET ORDREM-LEVEL-INIT       TO TRUE
           WHEN ( ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '3' )
               MOVE LOW-VALUES             TO ORDREM-LEVEL-03
               MOVE ORDREM-IO-AREA (2:3)   TO ORDREM-03-L3-FIRMA
               MOVE ORDREM-IO-AREA (1:1)   TO ORDREM-03-L2-OAVD
               MOVE ORDREM-IO-AREA (5:6)   TO ORDREM-03-L1-ORDNR
               IF  ORDREM-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDREM-03-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  ORDREM-03-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  ORDREM-03-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDREM-03-L3          TO THE-PRIOR-L3
               MOVE  ORDREM-03-L2          TO THE-PRIOR-L2
               MOVE  ORDREM-03-L1          TO THE-PRIOR-L1
               SET ORDREM-LEVEL-INIT       TO TRUE
           WHEN ( ORDREM-IO-AREA (19:1) NOT = ' ' )
               MOVE LOW-VALUES             TO ORDREM-LEVEL-04
               MOVE ORDREM-IO-AREA (2:3)   TO ORDREM-04-L3-FIRMA
               MOVE ORDREM-IO-AREA (1:1)   TO ORDREM-04-L2-OAVD
               MOVE ORDREM-IO-AREA (5:6)   TO ORDREM-04-L1-ORDNR
               IF  ORDREM-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDREM-04-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  ORDREM-04-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  ORDREM-04-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDREM-04-L3          TO THE-PRIOR-L3
               MOVE  ORDREM-04-L2          TO THE-PRIOR-L2
               MOVE  ORDREM-04-L1          TO THE-PRIOR-L1
               SET ORDREM-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L3 AND I-37)
               MOVE SPACES TO SUMFILV-IO-AREA
               INITIALIZE SUMFILV-IO-AREA
               MOVE FIRMA                  TO SUMFILV-IO-AREA (1:3)
               MOVE '*M'                   TO SUMFILV-IO-AREA (4:2)
               MOVE PFAAR                  TO SUMFILV-IO-AREA (6:2)
               MOVE PFMND                  TO SUMFILV-IO-AREA (8:2)
               MOVE PFDAG                  TO SUMFILV-IO-AREA (10:2)
               MOVE FIRSL2                 TO XO-110P
               MOVE XO-110P-EF             TO SUMFILV-IO-AREA (12:6)
               MOVE FIRAL2                 TO XO-110P
               MOVE XO-110P-EF             TO SUMFILV-IO-AREA (18:6)
               MOVE NULL11                 TO XO-110P
               MOVE XO-110P-EF             TO SUMFILV-IO-AREA (24:6)
               MOVE NULL11                 TO XO-110P
               MOVE XO-110P-EF             TO SUMFILV-IO-AREA (30:6)
               MOVE NULL11                 TO XO-110P
               MOVE XO-110P-EF             TO SUMFILV-IO-AREA (36:6)
               MOVE NULL11                 TO XO-110P
               MOVE XO-110P-EF             TO SUMFILV-IO-AREA (42:6)
               MOVE NULL11                 TO XO-110P
               MOVE XO-110P-EF             TO SUMFILV-IO-AREA (48:6)
               WRITE SUMFILV-IO-AREA
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
           INITIALIZE FAKPAR-DATA-FIELDS
           OPEN INPUT FAKPAR
           SET ORDREM-LEVEL-INIT           TO TRUE
           INITIALIZE ORDREM-DATA-FIELDS
           SET ORDREM-EOF-OFF              TO TRUE
           SET ORDREM-PROCESS              TO TRUE
           OPEN INPUT ORDREM
           OPEN OUTPUT SUMFILV.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PAR
           CLOSE FAKPAR
           CLOSE ORDREM
           CLOSE SUMFILV.
 
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
