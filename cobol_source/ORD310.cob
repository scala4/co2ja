       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORD310R.
      **********************************************  Z-WIN-RPG2   ****
      * UTSKRIFT AV UTAKS-ORDRE (KOMMISJONSVARER)                     *
      * FOR ALLE SØNNAKKUNDER                                         *
      * INPUT BLIR OPPDATERT DAGLIG I ORD110 (ORDRERUTINEN)           *
      * 14/8-95 MERKE RECORD SOM BLIR PRINTET MED * I POS. 100.       *
      *         DETTE ER EN LISTIDIKATOR, SLIK ATT ORDRE SOM ER       *
      *         FERDIGMELDT ETTER SISTE FAKT. KOMMER MED PÅ NESTE     *
      *         MND. RAPPORT.                                         *
      * 25/8-98 4 SIFFER ÅRSTALL VIA IBM SUBRUT ILNY224               *
      * 31/8-04 TEKSTLINJE OM MANUELL ORDRE.                          *
      * 12/9/18 FJERNET CALL ILNY224 OG SATT "20" I ÅRHUNDR*
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ORD310.rpg
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
           SELECT UTAKORD
               ASSIGN TO UT-S-UTAKORD
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS UTAKORD-STATUS.
           SELECT VAREMAS
               ASSIGN TO VAREMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAREMAS-STATUS
               RECORD KEY IS VAREMAS-KEY1.
           SELECT RABMAST
               ASSIGN TO RABMAST
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS RABMAST-STATUS
               RECORD KEY IS RABMAST-KEY1.
           SELECT UTAKUTF
               ASSIGN TO UT-S-UTAKUTF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS UTAKUTF-STATUS.
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
       FD UTAKORD
               BLOCK CONTAINS 240
               RECORD CONTAINS 120.
       01  UTAKORD-IO-AREA.
           05  UTAKORD-IO-AREA-X           PICTURE X(120).
       FD VAREMAS
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X.
               10  VAREMAS-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       FD RABMAST
               RECORD CONTAINS 40.
       01  RABMAST-IO-AREA.
           05  RABMAST-IO-AREA-X.
               10  RABMAST-KEY1            PICTURE X(20).
               10  FILLER                  PICTURE X(20).
       FD UTAKUTF
               BLOCK CONTAINS 240
               RECORD CONTAINS 120.
       01  UTAKUTF-IO-AREA.
           05  UTAKUTF-IO-AREA-X           PICTURE X(120).
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
           10  UTAKORD-STATUS              PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  RABMAST-STATUS              PICTURE 99 VALUE 0.
           10  UTAKUTF-STATUS              PICTURE 99 VALUE 0.
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
               88  UTAKORD-EOF-OFF         VALUE '0'.
               88  UTAKORD-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  UTAKORD-READ-OFF        VALUE '0'.
               88  UTAKORD-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  UTAKORD-PROCESS-OFF     VALUE '0'.
               88  UTAKORD-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  UTAKORD-LEVEL-INIT-OFF  VALUE '0'.
               88  UTAKORD-LEVEL-INIT      VALUE '1'.
           05  VAREMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  RABMAST-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  PARAM-DATA-FIELDS.
               10  PMMAA-IO.
                   15  PMMAA               PICTURE S9(4).
               10  PMM-IO.
                   15  PMM                 PICTURE S9(2).
               10  PAA-IO.
                   15  PAA                 PICTURE S9(2).
               10  SMMAA-IO.
                   15  SMMAA               PICTURE S9(4).
               10  SMM-IO.
                   15  SMM                 PICTURE S9(2).
               10  SAA-IO.
                   15  SAA                 PICTURE S9(2).
           05  UTAKORD-LEVEL-02.
               10  UTAKORD-02-L3.
                   15  UTAKORD-02-L3-FIRM  PICTURE X(3).
               10  UTAKORD-02-L2.
                   15  UTAKORD-02-L2-ALFA  PICTURE X(3).
               10  UTAKORD-02-L1.
                   15  UTAKORD-02-L1-EDBNR PICTURE X(7).
           05  UTAKORD-DATA-FIELDS.
               10  FIRM                    PICTURE X(3).
               10  ORDNR                   PICTURE X(6).
               10  EDBNR                   PICTURE X(7).
               10  BK                      PICTURE X(1).
               10  ANTLEV-IO.
                   15  ANTLEV              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(30).
               10  ORDPRI-IO.
                   15  ORDPRI              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ORAB1-IO.
                   15  ORAB1               PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  ORAB2-IO.
                   15  ORAB2               PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  ORAB3-IO.
                   15  ORAB3               PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  VGR                     PICTURE X(5).
               10  KUNDNR                  PICTURE X(6).
               10  ODATO-IO.
                   15  ODATO               PICTURE S9(6).
               10  OMMAA-IO.
                   15  OMMAA               PICTURE S9(4).
               10  OMM-IO.
                   15  OMM                 PICTURE S9(2).
               10  OAA-IO.
                   15  OAA                 PICTURE S9(2).
               10  ORDMOT                  PICTURE X(2).
               10  PRITIL-IO.
                   15  PRITIL              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  TYPE-X                  PICTURE X(1).
               10  LSTIND                  PICTURE X(1).
               10  MANUEL                  PICTURE X(1).
               10  UTREC                   PICTURE X(120).
           05  VAREMAS-DATA-FIELDS.
               10  LEVPRI-IO.
                   15  LEVPRI              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
           05  RABMAST-DATA-FIELDS.
               10  RRAB1-IO.
                   15  RRAB1               PICTURE S9(2)V9(1).
               10  RRAB2-IO.
                   15  RRAB2               PICTURE S9(2)V9(1).
               10  RRAB3-IO.
                   15  RRAB3               PICTURE S9(2)V9(1).
               10  NETFAK-IO.
                   15  NETFAK              PICTURE S9(1)V9(4).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(7).
           05  TEMPORARY-FIELDS.
               10  YEAR2                   PICTURE X(2).
               10  PAA4                    PICTURE X(4).
               10  SAA4                    PICTURE X(4).
               10  OAA4                    PICTURE X(4).
               10  ANT1-IO.
                   15  ANT1                PICTURE S9(7).
               10  SUM11-IO.
                   15  SUM11               PICTURE S9(7)V9(2).
               10  SUMBR3-IO.
                   15  SUMBR3              PICTURE S9(7)V9(2).
               10  SUMT11-IO.
                   15  SUMT11              PICTURE S9(5)V9(2).
               10  ANT2-IO.
                   15  ANT2                PICTURE S9(7).
               10  SUM12-IO.
                   15  SUM12               PICTURE S9(7)V9(2).
               10  SUMT12-IO.
                   15  SUMT12              PICTURE S9(5)V9(2).
               10  ANT3-IO.
                   15  ANT3                PICTURE S9(7).
               10  SUM13-IO.
                   15  SUM13               PICTURE S9(7)V9(2).
               10  SUMT13-IO.
                   15  SUMT13              PICTURE S9(6)V9(2).
               10  VARKEY                  PICTURE X(10).
               10  RABK9                   PICTURE X(9).
               10  RABK8                   PICTURE X(8).
               10  RABK17                  PICTURE X(17).
               10  RABKEY                  PICTURE X(20).
               10  ANT-IO.
                   15  ANT                 PICTURE S9(5).
               10  SUM1-IO.
                   15  SUM1                PICTURE S9(6)V9(2).
               10  SUMT1-IO.
                   15  SUMT1               PICTURE S9(5)V9(2).
               10  SUMBR1-IO.
                   15  SUMBR1              PICTURE S9(6)V9(2).
               10  SUM2-IO.
                   15  SUM2                PICTURE S9(9)V9(2).
               10  SUM3-IO.
                   15  SUM3                PICTURE S9(7)V9(2).
               10  YEAR4                   PICTURE X(4).
           05  EDITTING-FIELDS.
               10  EDIT-PMMAA              PICTURE ZZ.ZZ.
               10  XO-40YNZ                PICTURE ZZZZ.
               10  XO-50YY9R               PICTURE ZZ.ZZ9-.
               10  XO-72YY9                PICTURE Z.ZZZ.ZZZ,99.
               10  XO-21YY9                PICTURE ZZ,9.
               10  XO-21YYZ                PICTURE ZZ,Z.
               10  XO-62YY9R               PICTURE ZZZ.ZZZ,99-.
               10  XO-52YY9R               PICTURE ZZ.ZZZ,99-.
               10  XO-70YY9R               PICTURE Z.ZZZ.ZZ9-.
               10  XO-72YY9R               PICTURE Z.ZZZ.ZZZ,99-.
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
           IF  PARAM-PROCESS
               SET PARAM-PROCESS-OFF       TO TRUE
               SET PARAM-READ              TO TRUE
           END-IF
 
           IF  PARAM-READ
           AND RECORD-SELECTED-OFF
               PERFORM PARAM-GET
               SET PARAM-READ-OFF          TO TRUE
               IF  NOT PARAM-EOF
                   SET PARAM-PROCESS       TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  UTAKORD-PROCESS
               SET UTAKORD-PROCESS-OFF     TO TRUE
               SET UTAKORD-READ            TO TRUE
           END-IF
 
           IF  UTAKORD-READ
           AND RECORD-SELECTED-OFF
               PERFORM UTAKORD-GET
               SET UTAKORD-READ-OFF        TO TRUE
               IF  NOT UTAKORD-EOF
                   SET UTAKORD-PROCESS     TO TRUE
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
 
           IF  UTAKORD-PROCESS
               PERFORM UTAKORD-IDSET
           END-IF
 
           IF  UTAKORD-PROCESS
               PERFORM UTAKORD-CHK-LEVEL
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
 
           IF  PARAM-PROCESS
               PERFORM PARAM-FLDSET
           END-IF
 
           IF  UTAKORD-PROCESS
               PERFORM UTAKORD-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  UTAKORD-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-25                    TO TRUE
           SET NOT-I-26                    TO TRUE
           SET NOT-I-29                    TO TRUE
           SET NOT-I-22                    TO TRUE
           SET NOT-I-22                    TO TRUE
           SET NOT-I-32                    TO TRUE
           SET NOT-I-24                    TO TRUE
           IF  (I-L1)
               SET NOT-I-41                TO TRUE
           END-IF
           IF  (I-L2)
               SET NOT-I-42                TO TRUE
           END-IF
           IF  (I-L3)
               SET NOT-I-43                TO TRUE
           END-IF
           IF  (I-01)
               MOVE PAA                    TO YEAR2
               PERFORM AARRUT-S
           END-IF
           IF  (I-01)
               MOVE YEAR4                  TO PAA4
               MOVE SAA                    TO YEAR2
               PERFORM AARRUT-S
           END-IF
           IF  (I-01)
               MOVE YEAR4                  TO SAA4
           END-IF
           IF  (NOT-I-02)
               GO TO SLUTT-T
           END-IF
           IF  (I-L3)
               PERFORM RBSRUT-S
           END-IF
           MOVE OAA                        TO YEAR2
           PERFORM AARRUT-S
           MOVE YEAR4                      TO OAA4
      *****************************************************************
      * RUTINE FOR Å PLUKKE UT VARELINJER SOM SKAL PRINTES.           *
      * ALLE VARELINJER FRA INNEVÆRENDE PERIODE + ALLE VARELINJER     *
      * FRA TIDLIGERE PERIODER SOM IKKE HAR VÆRT MED PÅ OPPGAVEN      *
      * TIDLIGERE.                                                    *
      *****************************************************************
           SET NOT-I-25                    TO TRUE
           IF  OMMAA = PMMAA
               SET I-25                    TO TRUE
           END-IF
           IF  (NOT-I-25)
               SET NOT-I-24                TO TRUE
               SET NOT-I-22                TO TRUE
               IF  OAA4 < PAA4
                   SET I-24                TO TRUE
               END-IF
               IF  OAA4 = PAA4
                   SET I-22                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-25 AND I-22)
               SET NOT-I-24                TO TRUE
               IF  OMM NOT > PMM
                   SET I-24                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-25 AND I-24)
               SET NOT-I-23                TO TRUE
               IF  LSTIND = '*'
                   SET I-23                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-25 AND I-24 AND NOT-I-23)
               SET I-25                    TO TRUE
           END-IF
           IF  (I-25)
               SET I-41                    TO TRUE
               SET I-42                    TO TRUE
               SET I-43                    TO TRUE
           END-IF
           SET NOT-I-29                    TO TRUE
           IF  MANUEL = 'X'
               SET I-29                    TO TRUE
           END-IF
      *****************************************************************
      * RUTINE FOR Å SLETTE GAMLE PERIODER.                           *
      *****************************************************************
           SET NOT-I-26                    TO TRUE
           IF  OMMAA = SMMAA
               SET I-26                    TO TRUE
           END-IF
           IF  (NOT-I-26)
               SET NOT-I-24                TO TRUE
               SET NOT-I-22                TO TRUE
               IF  OAA4 < SAA4
                   SET I-24                TO TRUE
               END-IF
               IF  OAA4 = SAA4
                   SET I-22                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-26 AND I-22)
               SET NOT-I-24                TO TRUE
               IF  OMM NOT > SMM
                   SET I-24                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-26 AND I-24)
               SET I-26                    TO TRUE
      *****************************************************************
           END-IF
           IF  (I-L1)
               PERFORM VARRUT-S
           END-IF
           IF  (I-L1)
               PERFORM RABRUT-S
           END-IF
           IF  (I-25)
               PERFORM SUMRUT-S
           END-IF
           IF  (I-25)
               ADD ANT                     TO ANT1
               ADD SUM1                    TO SUM11
               ADD SUMBR1                  TO SUMBR3
               ADD SUMT1                   TO SUMT11
           END-IF.
 
       SLUTT-T.
           CONTINUE.
 
       VARRUT-S SECTION.
       VARRUT-S-P.
           MOVE FIRM                       TO VARKEY (1:3)
           MOVE EDBNR                      TO VARKEY (4:7)
           MOVE VARKEY                     TO VAREMAS-KEY1
           READ VAREMAS RECORD KEY IS VAREMAS-KEY1
           INVALID KEY
               SET I-31                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-31                TO TRUE
               PERFORM VAREMAS-FLDSET
               PERFORM VAREMAS-IDSET
           END-READ.
      ******************************************************
      *  SUBRUTINE FOR Å HENTE LEVRANDØRRAB. I RAB.MASTER  *
      ******************************************************
 
       RABRUT-S SECTION.
       RABRUT-S-P.
           MOVE FIRM                       TO RABK9 (1:3)
           MOVE 'LLLLLL'                   TO RABK9 (4:6)
           MOVE VGR                        TO RABK8 (1:5)
           MOVE ALFA                       TO RABK8 (6:3)
           MOVE RABK9                      TO RABK17 (1:9)
           MOVE RABK8                      TO RABK17 (10:8)
           MOVE RABK17                     TO RABKEY (1:17)
           MOVE '   '                      TO RABKEY (18:3)
           MOVE RABKEY                     TO RABMAST-KEY1
           READ RABMAST RECORD KEY IS RABMAST-KEY1
           INVALID KEY
               SET I-32                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-32                TO TRUE
               PERFORM RABMAST-FLDSET
               PERFORM RABMAST-IDSET
           END-READ.
      ******************************************************
      *  SUBRUTINE FOR Å SUMMER TOTAL PR. VARELINJE.       *
      ******************************************************
 
       SUMRUT-S SECTION.
       SUMRUT-S-P.
           ADD ANTLEV TO ZERO          GIVING ANT
           SET NOT-I-33                    TO TRUE
           IF  TYPE-X = 'K'
               SET I-33                    TO TRUE
           END-IF
           IF  (I-33)
               DIVIDE ANT BY -1        GIVING ANT
           END-IF
           IF  (NOT-I-31)
               MULTIPLY LEVPRI BY ANT  GIVING SUM1 ROUNDED
           END-IF
           IF  (I-31)
               MULTIPLY ORDPRI BY ANT  GIVING SUM1 ROUNDED
           END-IF
           MULTIPLY PRITIL BY ANT      GIVING SUMT1
           ADD SUM1 TO ZERO            GIVING SUMBR1
           IF  (I-32)
               MOVE 0                      TO RRAB1
               MOVE 0                      TO RRAB2
               MOVE 0                      TO RRAB3
           END-IF
           MULTIPLY RRAB1 BY SUM1      GIVING SUM2 ROUNDED
           DIVIDE SUM2 BY 100          GIVING SUM3 ROUNDED
           SUBTRACT SUM3                   FROM SUM1 ROUNDED
           MULTIPLY RRAB2 BY SUM1      GIVING SUM2 ROUNDED
           DIVIDE SUM2 BY 100          GIVING SUM3 ROUNDED
           SUBTRACT SUM3                   FROM SUM1 ROUNDED
           MULTIPLY RRAB3 BY SUM1      GIVING SUM2 ROUNDED
           DIVIDE SUM2 BY 100          GIVING SUM3 ROUNDED
           SUBTRACT SUM3                   FROM SUM1 ROUNDED.
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE ' '                        TO BBEST
           MOVE 'ORD31'                    TO LONR
           MOVE FIRM                       TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'ORD310  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF.
      ******************************************************
      *    SUBRUTINE FOR 4 SIFFER ÅRSTALL IBM SUBRUT.      *
      ******************************************************
 
       AARRUT-S SECTION.
       AARRUT-S-P.
           MOVE '20'                       TO YEAR4 (1:2)
           MOVE YEAR2                      TO YEAR4 (3:2).
      ******************************************************
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1 AND I-41)
               ADD ANT1                    TO ANT2
               ADD SUM11                   TO SUM12
               ADD SUMT11                  TO SUMT12
           END-IF
           IF  (I-L2 AND I-42)
               ADD ANT2                    TO ANT3
               ADD SUM12                   TO SUM13
               ADD SUMT12                  TO SUMT13
      ******************************************************
      *  SUBRUTINE FOR Å HENTE LEVRANDØRPRIS I VAREMASTER  *
      ******************************************************
           END-IF
           .
 
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
           WHEN ANY
               MOVE PARAM-IO-AREA (30:4)   TO PMMAA-IO
               INSPECT PMMAA-IO REPLACING ALL ' ' BY '0'
               MOVE PARAM-IO-AREA (30:2)   TO PMM-IO
               INSPECT PMM-IO REPLACING ALL ' ' BY '0'
               MOVE PARAM-IO-AREA (32:2)   TO PAA-IO
               INSPECT PAA-IO REPLACING ALL ' ' BY '0'
               MOVE PARAM-IO-AREA (59:4)   TO SMMAA-IO
               INSPECT SMMAA-IO REPLACING ALL ' ' BY '0'
               MOVE PARAM-IO-AREA (59:2)   TO SMM-IO
               INSPECT SMM-IO REPLACING ALL ' ' BY '0'
               MOVE PARAM-IO-AREA (61:2)   TO SAA-IO
               INSPECT SAA-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       PARAM-IDSET SECTION.
       PARAM-IDSET-P.
           SET I-01                        TO TRUE.
 
       UTAKORD-GET SECTION.
       UTAKORD-GET-P.
           IF  UTAKORD-EOF-OFF
               READ UTAKORD
               AT END
                   SET UTAKORD-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       UTAKORD-FLDSET SECTION.
       UTAKORD-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE UTAKORD-IO-AREA (2:3)  TO FIRM (1:3)
               MOVE UTAKORD-IO-AREA (5:6)  TO ORDNR (1:6)
               MOVE UTAKORD-IO-AREA (11:7) TO EDBNR (1:7)
               MOVE UTAKORD-IO-AREA (18:1) TO BK (1:1)
               MOVE UTAKORD-IO-AREA (19:5) TO ANTLEV-IO
               MOVE UTAKORD-IO-AREA (24:3) TO ALFA (1:3)
               MOVE UTAKORD-IO-AREA (27:30) TO ARTNR (1:30)
               MOVE UTAKORD-IO-AREA (57:5) TO ORDPRI-IO
               MOVE UTAKORD-IO-AREA (62:2) TO ORAB1-IO
               MOVE UTAKORD-IO-AREA (64:2) TO ORAB2-IO
               MOVE UTAKORD-IO-AREA (66:2) TO ORAB3-IO
               MOVE UTAKORD-IO-AREA (73:5) TO VGR (1:5)
               MOVE UTAKORD-IO-AREA (78:6) TO KUNDNR (1:6)
               MOVE UTAKORD-IO-AREA (84:6) TO ODATO-IO
               INSPECT ODATO-IO REPLACING ALL ' ' BY '0'
               MOVE UTAKORD-IO-AREA (86:4) TO OMMAA-IO
               INSPECT OMMAA-IO REPLACING ALL ' ' BY '0'
               MOVE UTAKORD-IO-AREA (86:2) TO OMM-IO
               INSPECT OMM-IO REPLACING ALL ' ' BY '0'
               MOVE UTAKORD-IO-AREA (88:2) TO OAA-IO
               INSPECT OAA-IO REPLACING ALL ' ' BY '0'
               MOVE UTAKORD-IO-AREA (90:2) TO ORDMOT (1:2)
               MOVE UTAKORD-IO-AREA (92:4) TO PRITIL-IO
               MOVE UTAKORD-IO-AREA (96:1) TO TYPE-X (1:1)
               MOVE UTAKORD-IO-AREA (100:1) TO LSTIND (1:1)
               MOVE UTAKORD-IO-AREA (99:1) TO LSTIND (1:1)
               MOVE UTAKORD-IO-AREA (116:1) TO MANUEL (1:1)
               MOVE UTAKORD-IO-AREA (1:120) TO UTREC (1:120)
           END-EVALUATE.
 
       UTAKORD-IDSET SECTION.
       UTAKORD-IDSET-P.
           SET I-02                        TO TRUE.
 
       UTAKORD-CHK-LEVEL SECTION.
       UTAKORD-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO UTAKORD-LEVEL-02
               MOVE UTAKORD-IO-AREA (2:3)  TO UTAKORD-02-L3-FIRM
               MOVE UTAKORD-IO-AREA (24:3) TO UTAKORD-02-L2-ALFA
               MOVE UTAKORD-IO-AREA (11:7) TO UTAKORD-02-L1-EDBNR
               IF  UTAKORD-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  UTAKORD-02-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  UTAKORD-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  UTAKORD-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  UTAKORD-02-L3         TO THE-PRIOR-L3
               MOVE  UTAKORD-02-L2         TO THE-PRIOR-L2
               MOVE  UTAKORD-02-L1         TO THE-PRIOR-L1
               SET UTAKORD-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (165:5) TO LEVPRI-IO
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-03                        TO TRUE.
 
       RABMAST-FLDSET SECTION.
       RABMAST-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RABMAST-IO-AREA (22:3) TO RRAB1-IO
               INSPECT RRAB1-IO REPLACING ALL ' ' BY '0'
               MOVE RABMAST-IO-AREA (25:3) TO RRAB2-IO
               INSPECT RRAB2-IO REPLACING ALL ' ' BY '0'
               MOVE RABMAST-IO-AREA (28:3) TO RRAB3-IO
               INSPECT RRAB3-IO REPLACING ALL ' ' BY '0'
               MOVE RABMAST-IO-AREA (31:5) TO NETFAK-IO
               INSPECT NETFAK-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       RABMAST-IDSET SECTION.
       RABMAST-IDSET-P.
           SET I-04                        TO TRUE.
 
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
           IF  (I-02 AND I-25 AND I-29)
           AND (NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (7:1)
               MOVE 'MANUELL UTLEVERINGSORDRE' TO LISTE-IO-AREA (13:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-02 AND I-25 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ALFA                   TO LISTE-IO-AREA (1:3)
               MOVE ARTNR                  TO LISTE-IO-AREA (7:30)
               MOVE ODATO                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (39:8)
               MOVE ORDNR                  TO LISTE-IO-AREA (49:6)
               MOVE KUNDNR                 TO LISTE-IO-AREA (57:6)
               MOVE ANT                    TO XO-50YY9R
               MOVE XO-50YY9R              TO LISTE-IO-AREA (68:7)
               INITIALIZE ANT
               MOVE LEVPRI                 TO XO-72YY9
               MOVE XO-72YY9               TO LISTE-IO-AREA (77:12)
               IF  (NOT-I-32)
                   MOVE RRAB1              TO XO-21YY9
                   MOVE XO-21YY9           TO LISTE-IO-AREA (91:4)
               END-IF
               IF  (NOT-I-32)
                   MOVE RRAB2              TO XO-21YYZ
                   MOVE XO-21YYZ           TO LISTE-IO-AREA (97:4)
               END-IF
               IF  (NOT-I-32)
                   MOVE RRAB3              TO XO-21YYZ
                   MOVE XO-21YYZ           TO LISTE-IO-AREA (103:4)
               END-IF
               MOVE SUM1                   TO XO-62YY9R
               MOVE XO-62YY9R              TO LISTE-IO-AREA (111:11)
               INITIALIZE SUM1
               MOVE SUMT1                  TO XO-52YY9R
               MOVE XO-52YY9R              TO LISTE-IO-AREA (123:10)
               INITIALIZE SUMT1
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-02 AND NOT-I-26)
               MOVE SPACES TO UTAKUTF-IO-AREA
               INITIALIZE UTAKUTF-IO-AREA
               MOVE UTREC                  TO UTAKUTF-IO-AREA (1:120)
               WRITE UTAKUTF-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L2 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE LOPNVN                 TO LISTE-IO-AREA (51:35)
               MOVE ' FOR PERIODE '        TO LISTE-IO-AREA (87:13)
               MOVE PMMAA                  TO EDIT-PMMAA
               MOVE EDIT-PMMAA             TO LISTE-IO-AREA (101:5)
               MOVE 'SIDE '                TO LISTE-IO-AREA (117:5)
               IF  (I-L3)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (122:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ALFA'                 TO LISTE-IO-AREA (1:4)
               MOVE 'ARTIKKELNUMMER'       TO LISTE-IO-AREA (7:14)
               MOVE 'ORDRE'                TO LISTE-IO-AREA (42:5)
               MOVE 'ORDRE'                TO LISTE-IO-AREA (49:5)
               MOVE 'KUNDE'                TO LISTE-IO-AREA (57:5)
               MOVE 'ANTALL'               TO LISTE-IO-AREA (69:6)
               MOVE 'PRIS'                 TO LISTE-IO-AREA (83:4)
               MOVE 'RAB'                  TO LISTE-IO-AREA (92:3)
               MOVE 'RAB'                  TO LISTE-IO-AREA (98:3)
               MOVE 'RAB'                  TO LISTE-IO-AREA (104:3)
               MOVE 'NETTO SUM'            TO LISTE-IO-AREA (113:9)
               MOVE ' FRAKT TIL'           TO LISTE-IO-AREA (123:10)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KODE'                 TO LISTE-IO-AREA (1:4)
               MOVE 'DATO.'                TO LISTE-IO-AREA (42:5)
               MOVE 'NUM.'                 TO LISTE-IO-AREA (49:4)
               MOVE 'NUM.'                 TO LISTE-IO-AREA (57:4)
               MOVE 'LEVERT'               TO LISTE-IO-AREA (69:6)
               MOVE 'PR.STK.'              TO LISTE-IO-AREA (82:7)
               MOVE ' 1 '                  TO LISTE-IO-AREA (92:3)
               MOVE ' 2 '                  TO LISTE-IO-AREA (98:3)
               MOVE ' 3 '                  TO LISTE-IO-AREA (104:3)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE LOPNVN                 TO LISTE-IO-AREA (51:35)
               MOVE ' FOR PERIODE '        TO LISTE-IO-AREA (87:13)
               MOVE PMMAA                  TO EDIT-PMMAA
               MOVE EDIT-PMMAA             TO LISTE-IO-AREA (101:5)
               MOVE 'SIDE '                TO LISTE-IO-AREA (117:5)
               IF  (I-L3)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (122:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ALFA'                 TO LISTE-IO-AREA (1:4)
               MOVE 'ARTIKKELNUMMER'       TO LISTE-IO-AREA (7:14)
               MOVE 'ORDRE'                TO LISTE-IO-AREA (42:5)
               MOVE 'ORDRE'                TO LISTE-IO-AREA (49:5)
               MOVE 'KUNDE'                TO LISTE-IO-AREA (57:5)
               MOVE 'ANTALL'               TO LISTE-IO-AREA (69:6)
               MOVE 'PRIS'                 TO LISTE-IO-AREA (83:4)
               MOVE 'RAB'                  TO LISTE-IO-AREA (92:3)
               MOVE 'RAB'                  TO LISTE-IO-AREA (98:3)
               MOVE 'RAB'                  TO LISTE-IO-AREA (104:3)
               MOVE 'NETTO SUM'            TO LISTE-IO-AREA (113:9)
               MOVE ' FRAKT TIL'           TO LISTE-IO-AREA (123:10)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KODE'                 TO LISTE-IO-AREA (1:4)
               MOVE 'DATO.'                TO LISTE-IO-AREA (42:5)
               MOVE 'NUM.'                 TO LISTE-IO-AREA (49:4)
               MOVE 'NUM.'                 TO LISTE-IO-AREA (57:4)
               MOVE 'LEVERT'               TO LISTE-IO-AREA (69:6)
               MOVE 'PR.STK.'              TO LISTE-IO-AREA (82:7)
               MOVE ' 1 '                  TO LISTE-IO-AREA (92:3)
               MOVE ' 2 '                  TO LISTE-IO-AREA (98:3)
               MOVE ' 3 '                  TO LISTE-IO-AREA (104:3)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-41 AND NOT-I-86)
               IF  I-OF
                   PERFORM HEADING-OVERFLOW
                   SET NOT-I-OF            TO TRUE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ARTIKKEL TOTALT'      TO LISTE-IO-AREA (48:15)
               MOVE ANT1                   TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (65:10)
               INITIALIZE ANT1
               MOVE '*'                    TO LISTE-IO-AREA (106:1)
               MOVE SUM11                  TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (109:13)
               INITIALIZE SUM11
               MOVE SUMT11                 TO XO-52YY9R
               MOVE XO-52YY9R              TO LISTE-IO-AREA (123:10)
               INITIALIZE SUMT11
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L2 AND I-42 AND NOT-I-86)
               IF  I-OF
                   PERFORM HEADING-OVERFLOW
                   SET NOT-I-OF            TO TRUE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ALFAKODE TOTALT'      TO LISTE-IO-AREA (48:15)
               MOVE ANT2                   TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (65:10)
               INITIALIZE ANT2
               MOVE '**'                   TO LISTE-IO-AREA (105:2)
               MOVE SUM12                  TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (109:13)
               INITIALIZE SUM12
               MOVE SUMT12                 TO XO-52YY9R
               MOVE XO-52YY9R              TO LISTE-IO-AREA (123:10)
               INITIALIZE SUMT12
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L3 AND I-43 AND NOT-I-86)
               IF  I-OF
                   PERFORM HEADING-OVERFLOW
                   SET NOT-I-OF            TO TRUE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMA TOTALT   '      TO LISTE-IO-AREA (48:15)
               MOVE ANT3                   TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (65:10)
               MOVE '***'                  TO LISTE-IO-AREA (104:3)
               MOVE SUM13                  TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (109:13)
               INITIALIZE SUM13
               MOVE SUMT13                 TO XO-62YY9R
               MOVE XO-62YY9R              TO LISTE-IO-AREA (122:11)
               INITIALIZE SUMT13
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               IF  I-OF
                   PERFORM HEADING-OVERFLOW
                   SET NOT-I-OF            TO TRUE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMA TOTALT   '      TO LISTE-IO-AREA (48:15)
               MOVE ANT3                   TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (65:10)
               INITIALIZE ANT3
               MOVE 'BRUTTO SUM'           TO LISTE-IO-AREA (97:10)
               MOVE SUMBR3                 TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (109:13)
               INITIALIZE SUMBR3
               MOVE 2                      TO LISTE-AFTER-SPACE
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
           SET UTAKORD-LEVEL-INIT          TO TRUE
           INITIALIZE UTAKORD-DATA-FIELDS
           SET UTAKORD-EOF-OFF             TO TRUE
           SET UTAKORD-PROCESS             TO TRUE
           OPEN INPUT UTAKORD
           INITIALIZE VAREMAS-DATA-FIELDS
           OPEN INPUT VAREMAS
           INITIALIZE RABMAST-DATA-FIELDS
           OPEN INPUT RABMAST
           OPEN OUTPUT UTAKUTF
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PARAM
           CLOSE UTAKORD
           CLOSE VAREMAS
           CLOSE RABMAST
           CLOSE UTAKUTF
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
