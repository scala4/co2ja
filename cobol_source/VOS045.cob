       IDENTIFICATION DIVISION.
       PROGRAM-ID. VOS045R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM.......: VOS045, UTSKRIFT MEKANIKER STATISTIKK.       *
      *  PROGRAMMERER..: MORTEN TUVRØNNINGEN                          *
      *  KJØRES I JOBB.: VOS02A                                       *
      *  LAGET DATO....: 10.10.96                                     *
      *  ENDRET........: 01.10.97 LAGT INN USPI 8 FOR Å STYRE HVILKEN *
      *                           PR. DATO RAPPORTEN SKAL SKRIVES.    *
      *                  14.06.99 NULLET AKKUMULATOR PÅ L2.           *
      *                  23.03.00 AKKUMULERER TIMER IÅR/IFJOR.        *
      *                           VISER TALL PR AVDELING.             *
      *                  31.01.02 ØKT TABELL FRA 10 TIL 20.           *
      *                  04.08.06 RETTET AKKUMULETE TALL FOR FJOR PÅ  *
      *                           DELER. FEIL FRA 1996.               *
      *  INPUT.........: MEK.STAT.FILE,                               *
      *                  DATO-FILE.                                   *
      *  OUTPUT........: MEKANIKER STATISTIKK.                        *
      *                  UPSI 8 ER PÅ : RAPPORT PR KALENDERDATO.      *
      *                  UPSI 8 ER AV : RAPPORT PR FERDIGMELDINGSDATO.*
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VOS045.rpg
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
           SELECT MEKTAB
               ASSIGN TO UT-S-MEKTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS MEKTAB-STATUS.
           SELECT MEKDATO
               ASSIGN TO UT-S-MEKDATO
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS MEKDATO-STATUS.
           SELECT MEKSTAT
               ASSIGN TO UT-S-MEKSTAT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS MEKSTAT-STATUS.
           SELECT LONNMAS
               ASSIGN TO LONNMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS LONNMAS-STATUS
               RECORD KEY IS LONNMAS-KEY1.
           SELECT MEKLIST
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS MEKLIST-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD MEKTAB
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  MEKTAB-IO-AREA.
           05  MEKTAB-IO-AREA-X            PICTURE X(80).
       FD MEKDATO
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  MEKDATO-IO-AREA.
           05  MEKDATO-IO-AREA-X           PICTURE X(80).
       FD MEKSTAT
               BLOCK CONTAINS 1200
               RECORD CONTAINS 120.
       01  MEKSTAT-IO-AREA.
           05  MEKSTAT-IO-AREA-X           PICTURE X(120).
      *MEKSTAO O   F 800  80    2       DISK40 SYS030S
       FD LONNMAS
               RECORD CONTAINS 1025.
       01  LONNMAS-IO-AREA.
           05  LONNMAS-IO-AREA-X.
               10  LONNMAS-KEY1            PICTURE X(6).
               10  FILLER                  PICTURE X(1019).
      *FLISTEO O   F  80  80            PRINTERSYSLST
       FD MEKLIST
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  MEKLIST-IO-PRINT.
           05  MEKLIST-IO-AREA-CONTROL     PICTURE X VALUE ' '.
        02 MEKLIST-IO-AREA.
           05  MEKLIST-IO-AREA-X           PICTURE X(132).
       WORKING-STORAGE SECTION.
       77  TABMEK-MAX   VALUE 40           PICTURE 9(4) USAGE BINARY.
       77  TABFIL-MAX   VALUE 40           PICTURE 9(4) USAGE BINARY.
       01  TABLES.
      * **START RBS - DATASTRUKTUR FOR SUB-PROGRAM RBS000 ********
      *PSDS       SDS
      *                                    *ROUTINE R
      *                                    *PARMS   P
      *                                    *STATUS  S
           05  TABMEK-TABLE.
               10  TABMEK-ENTRY
                                           OCCURS 40 TIMES
                                           INDEXED BY TABMEK-I
                                                      TABMEK-S
                                                      TABFIL-I
                                                      TABFIL-S.
                   15  TABMEK              PICTURE X(3).
                   15  TABFIL              PICTURE X(7).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  MEKTAB-STATUS               PICTURE 99 VALUE 0.
           10  MEKDATO-STATUS              PICTURE 99 VALUE 0.
           10  MEKSTAT-STATUS              PICTURE 99 VALUE 0.
           10  LONNMAS-STATUS              PICTURE 99 VALUE 0.
           10  MEKLIST-STATUS              PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  MEKTAB-EOF-OFF          VALUE '0'.
               88  MEKTAB-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  MEKDATO-EOF-OFF         VALUE '0'.
               88  MEKDATO-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  MEKDATO-READ-OFF        VALUE '0'.
               88  MEKDATO-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  MEKDATO-PROCESS-OFF     VALUE '0'.
               88  MEKDATO-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  MEKSTAT-EOF-OFF         VALUE '0'.
               88  MEKSTAT-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  MEKSTAT-READ-OFF        VALUE '0'.
               88  MEKSTAT-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  MEKSTAT-PROCESS-OFF     VALUE '0'.
               88  MEKSTAT-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  MEKSTAT-LEVEL-INIT-OFF  VALUE '0'.
               88  MEKSTAT-LEVEL-INIT      VALUE '1'.
           05  LONNMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  MEKLIST-DATA-FIELDS.
               10  MEKLIST-AFTER-SPACE     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  MEKLIST-AFTER-SKIP      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  MEKLIST-BEFORE-SPACE    PICTURE 9(4) BINARY
                                           VALUE 0.
               10  MEKLIST-BEFORE-SKIP     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  MEKLIST-MAX-LINES       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  MEKLIST-LINE-COUNT      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  MEKLIST-CLR-IO          PICTURE X VALUE 'Y'.
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
               10  FIRNVN                  PICTURE X(30).
               10  FILLER                  PICTURE X(205).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(87).
               10  LPRIID                  PICTURE X(4).
               10  FILLER                  PICTURE X(166).
      *     *  BESTILLINGSOPPGAVER (OVERSTYRING AV RBS-FILE) *    *
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(99).
               10  BBEST                   PICTURE X(1).
               10  FILLER                  PICTURE X(157).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(100).
               10  BFILL                   PICTURE X(157).
           05  MEKDATO-DATA-FIELDS.
               10  MDKLK-IO.
                   15  MDKLK               PICTURE S9(6).
               10  MDDTO                   PICTURE X(8).
               10  MDAAR                   PICTURE X(4).
               10  MDMND                   PICTURE X(2).
               10  MDDAG                   PICTURE X(2).
               10  MDFIR                   PICTURE X(3).
           05  MEKSTAT-LEVEL-02.
               10  MEKSTAT-02-L3.
                   15  MEKSTAT-02-L3-MSFNR PICTURE X(3).
               10  MEKSTAT-02-L2.
                   15  MEKSTAT-02-L2-MSAVD PICTURE X(4).
               10  MEKSTAT-02-L1.
                   15  MEKSTAT-02-L1-MSMEK PICTURE X(3).
           05  MEKSTAT-DATA-FIELDS.
      *                                       1 120 MSREC
               10  MSFNR                   PICTURE X(3).
               10  MSMEK                   PICTURE X(3).
               10  MSLMKY                  PICTURE X(6).
               10  MSFDTO                  PICTURE X(8).
               10  MSFAAR                  PICTURE X(4).
               10  MSFMND                  PICTURE X(2).
               10  MSOBA-IO.
                   15  MSOBA               PICTURE S9(9)V9(2).
               10  MSOBD-IO.
                   15  MSOBD               PICTURE S9(9)V9(2).
               10  MSKBA-IO.
                   15  MSKBA               PICTURE S9(9)V9(2).
               10  MSKBD-IO.
                   15  MSKBD               PICTURE S9(9)V9(2).
      *                                      67  610MSANT
               10  MSTIM-IO.
                   15  MSTIM               PICTURE S9(7)V9(2).
               10  MSAVD                   PICTURE X(4).
               10  MSKDTO                  PICTURE X(8).
               10  MSKAAR                  PICTURE X(4).
               10  MSKMND                  PICTURE X(2).
           05  LONNMAS-DATA-FIELDS.
               10  LMNAVN                  PICTURE X(30).
      *****************************************************************
      * HOUSEKEEPING.                                                 *
      *****************************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(4).
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  TIDSP-IO.
                   15  TIDSP               PICTURE S9(6).
               10  MEKNVN                  PICTURE X(30).
               10  OTDD-IO.
                   15  OTDD                PICTURE S9(7)V9(2).
               10  OBDDA-IO.
                   15  OBDDA               PICTURE S9(9)V9(2).
               10  OBDDD-IO.
                   15  OBDDD               PICTURE S9(9)V9(2).
               10  KBDDA-IO.
                   15  KBDDA               PICTURE S9(9)V9(2).
               10  KBDDD-IO.
                   15  KBDDD               PICTURE S9(9)V9(2).
               10  OTMA-ELG-IO.
                   15  OTMA-ELG            PICTURE S9(7)V9(2).
               10  OBMA-ELGA-IO.
                   15  OBMA-ELGA           PICTURE S9(9)V9(2).
               10  OBMA-ELGD-IO.
                   15  OBMA-ELGD           PICTURE S9(9)V9(2).
               10  OBMFA-IO.
                   15  OBMFA               PICTURE S9(9)V9(2).
               10  OBMFD-IO.
                   15  OBMFD               PICTURE S9(9)V9(2).
               10  OTIA-ELG-IO.
                   15  OTIA-ELG            PICTURE S9(7)V9(2).
               10  OBIA-ELGA-IO.
                   15  OBIA-ELGA           PICTURE S9(9)V9(2).
               10  OBIA-ELGD-IO.
                   15  OBIA-ELGD           PICTURE S9(9)V9(2).
               10  OTIF-IO.
                   15  OTIF                PICTURE S9(7)V9(2).
               10  OBIFA-IO.
                   15  OBIFA               PICTURE S9(9)V9(2).
               10  OBIFD-IO.
                   15  OBIFD               PICTURE S9(9)V9(2).
               10  OADDU-IO.
                   15  OADDU               PICTURE S9(6).
               10  OTDDU-IO.
                   15  OTDDU               PICTURE S9(8).
               10  OTIFU-IO.
                   15  OTIFU               PICTURE S9(8).
               10  OBDDAU-IO.
                   15  OBDDAU              PICTURE S9(10).
               10  OBDDDU-IO.
                   15  OBDDDU              PICTURE S9(10).
               10  KBDDAU-IO.
                   15  KBDDAU              PICTURE S9(10).
               10  KBDDDU-IO.
                   15  KBDDDU              PICTURE S9(10).
               10  NBDDAU-IO.
                   15  NBDDAU              PICTURE S9(10).
               10  NBDDDU-IO.
                   15  NBDDDU              PICTURE S9(10).
               10  OAMA-ELGU-IO.
                   15  OAMA-ELGU           PICTURE S9(6).
               10  OTMA-ELGU-IO.
                   15  OTMA-ELGU           PICTURE S9(8).
               10  OTIA-ELGU-IO.
                   15  OTIA-ELGU           PICTURE S9(8).
               10  OBMA-ELGAU-IO.
                   15  OBMA-ELGAU          PICTURE S9(10).
               10  OBMA-ELGDU-IO.
                   15  OBMA-ELGDU          PICTURE S9(10).
               10  OBIA-ELGAU-IO.
                   15  OBIA-ELGAU          PICTURE S9(10).
               10  OBIA-ELGDU-IO.
                   15  OBIA-ELGDU          PICTURE S9(10).
               10  OBIFAU-IO.
                   15  OBIFAU              PICTURE S9(10).
               10  OBIFDU-IO.
                   15  OBIFDU              PICTURE S9(10).
               10  L2ANT1-IO.
                   15  L2ANT1              PICTURE S9(6).
               10  L2TIM1-IO.
                   15  L2TIM1              PICTURE S9(8).
               10  L2TIA-ELGR-IO.
                   15  L2TIA-ELGR          PICTURE S9(8).
               10  L2TIFJ-IO.
                   15  L2TIFJ              PICTURE S9(8).
               10  L2KR1-IO.
                   15  L2KR1               PICTURE S9(10).
               10  L2KR2-IO.
                   15  L2KR2               PICTURE S9(10).
               10  L2KR3-IO.
                   15  L2KR3               PICTURE S9(10).
               10  L2KR4-IO.
                   15  L2KR4               PICTURE S9(10).
               10  L2KR5-IO.
                   15  L2KR5               PICTURE S9(10).
               10  L2KR6-IO.
                   15  L2KR6               PICTURE S9(10).
               10  L2ANT2-IO.
                   15  L2ANT2              PICTURE S9(6).
               10  L2TIM2-IO.
                   15  L2TIM2              PICTURE S9(8).
               10  L2KR7-IO.
                   15  L2KR7               PICTURE S9(10).
               10  L2KR8-IO.
                   15  L2KR8               PICTURE S9(10).
               10  L2KR9-IO.
                   15  L2KR9               PICTURE S9(10).
               10  L2KR10-IO.
                   15  L2KR10              PICTURE S9(10).
               10  L2KR11-IO.
                   15  L2KR11              PICTURE S9(10).
               10  L2KR12-IO.
                   15  L2KR12              PICTURE S9(10).
               10  L3ANT1-IO.
                   15  L3ANT1              PICTURE S9(6).
               10  L3TIM1-IO.
                   15  L3TIM1              PICTURE S9(8).
               10  L3TIA-ELGR-IO.
                   15  L3TIA-ELGR          PICTURE S9(8).
               10  L3TIFJ-IO.
                   15  L3TIFJ              PICTURE S9(8).
               10  L3KR1-IO.
                   15  L3KR1               PICTURE S9(10).
               10  L3KR2-IO.
                   15  L3KR2               PICTURE S9(10).
               10  L3KR3-IO.
                   15  L3KR3               PICTURE S9(10).
               10  L3KR4-IO.
                   15  L3KR4               PICTURE S9(10).
               10  L3KR5-IO.
                   15  L3KR5               PICTURE S9(10).
               10  L3KR6-IO.
                   15  L3KR6               PICTURE S9(10).
               10  L3ANT2-IO.
                   15  L3ANT2              PICTURE S9(6).
               10  L3TIM2-IO.
                   15  L3TIM2              PICTURE S9(8).
               10  L3KR7-IO.
                   15  L3KR7               PICTURE S9(10).
               10  L3KR8-IO.
                   15  L3KR8               PICTURE S9(10).
               10  L3KR9-IO.
                   15  L3KR9               PICTURE S9(10).
               10  L3KR10-IO.
                   15  L3KR10              PICTURE S9(10).
               10  L3KR11-IO.
                   15  L3KR11              PICTURE S9(10).
               10  L3KR12-IO.
                   15  L3KR12              PICTURE S9(10).
               10  OADD-IO.
                   15  OADD                PICTURE S9(6).
               10  OAMA-ELG-IO.
                   15  OAMA-ELG            PICTURE S9(6).
           05  EDITTING-FIELDS.
               10  XO-40YNZ                PICTURE ZZZZ.
               10  EDIT-MDKLK              PICTURE ZZZZZZ.
               10  XO-80YNZR               PICTURE ZZZZZZZZ-.
               10  XO-100YY9R              PICTURE Z.ZZZ.ZZZ.ZZ9-.
           05  PREDEFINED-FIELDS.
               10  PAGE1                   PICTURE S9(4) USAGE BINARY.
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
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  MEKDATO-PROCESS
               SET MEKDATO-PROCESS-OFF     TO TRUE
               SET MEKDATO-READ            TO TRUE
           END-IF
 
           IF  MEKDATO-READ
           AND RECORD-SELECTED-OFF
               PERFORM MEKDATO-GET
               SET MEKDATO-READ-OFF        TO TRUE
               IF  NOT MEKDATO-EOF
                   SET MEKDATO-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  MEKSTAT-PROCESS
               SET MEKSTAT-PROCESS-OFF     TO TRUE
               SET MEKSTAT-READ            TO TRUE
           END-IF
 
           IF  MEKSTAT-READ
           AND RECORD-SELECTED-OFF
               PERFORM MEKSTAT-GET
               SET MEKSTAT-READ-OFF        TO TRUE
               IF  NOT MEKSTAT-EOF
                   SET MEKSTAT-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  MEKDATO-PROCESS
               PERFORM MEKDATO-IDSET
           END-IF
 
           IF  MEKSTAT-PROCESS
               PERFORM MEKSTAT-IDSET
           END-IF
 
           IF  MEKSTAT-PROCESS
               PERFORM MEKSTAT-CHK-LEVEL
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
 
           IF  MEKDATO-PROCESS
               PERFORM MEKDATO-FLDOFF
               PERFORM MEKDATO-FLDSET
           END-IF
 
           IF  MEKSTAT-PROCESS
               PERFORM MEKSTAT-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  MEKSTAT-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-99)
               SET NOT-I-99                TO TRUE
           END-IF
           IF  (NOT-I-98)
               SET I-98                    TO TRUE
               SET I-99                    TO TRUE
           END-IF
           IF  (I-99)
               ACCEPT SYSTEM-TIME-X      FROM TIME
               MOVE SYSTEM-TIME            TO TIDSP (1:6)
      *****************************************************************
      * LES PARAM-FILE                                                *
      *****************************************************************
           END-IF
           IF  (I-01)
               SET NOT-I-30                TO TRUE
               IF  MDFIR = '***'
                   SET I-30                TO TRUE
               END-IF
               GO TO SLUTT-T
           END-IF
           IF  (I-L1)
               MOVE 0                      TO OADDU
               MOVE 0                      TO OTDDU
               MOVE 0                      TO OTIFU
               MOVE 0                      TO OBDDAU
               MOVE 0                      TO OBDDDU
               MOVE 0                      TO KBDDAU
               MOVE 0                      TO KBDDDU
               MOVE 0                      TO NBDDAU
               MOVE 0                      TO NBDDDU
               MOVE 0                      TO OAMA-ELGU
               MOVE 0                      TO OTMA-ELGU
               MOVE 0                      TO OTIA-ELGU
               MOVE 0                      TO OBMA-ELGAU
               MOVE 0                      TO OBMA-ELGDU
               MOVE 0                      TO OBIA-ELGAU
               MOVE 0                      TO OBIA-ELGDU
               MOVE 0                      TO OBIFAU
               MOVE 0                      TO OBIFDU
      *****************************************************************
      * SJEKKER OM DET ER BESTILT LISTE FOR ET FIRMA, OG AVSLUTTER/   *
      * HOPPER OVER ELLER BEHANDLER I HENHOLD TIL PARAMETER.          *
      *****************************************************************
           END-IF
           IF  (I-L3)
               SET NOT-I-11                TO TRUE
               SET NOT-I-97                TO TRUE
           END-IF
           IF  (I-L3 AND NOT-I-30)
               SET NOT-I-31                TO TRUE
               SET NOT-I-32                TO TRUE
               IF  MSFNR > MDFIR
                   SET I-31                TO TRUE
               END-IF
               IF  MSFNR < MDFIR
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-30 AND I-31)
               SET I-LR                    TO TRUE
           END-IF
           IF  (I-LR)
               GO TO SLUTT-T
           END-IF
           IF  (NOT-I-30 AND I-32)
               GO TO SLUTT-T
      *****************************************************************
      * HENTER FILE MED MEK.-NAVN FRA SYSIPT.                         *
      *****************************************************************
           END-IF
           IF  (I-L3)
               SET NOT-I-10                TO TRUE
               SET TABMEK-S                TO TABMEK-I
               PERFORM WITH TEST AFTER
                       VARYING TABMEK-I FROM 1 BY 1
                         UNTIL TABMEK-I >= TABMEK-MAX
                            OR I-10
                   IF  MSFNR = TABMEK (TABMEK-I)
                       SET I-10            TO TRUE
                       SET TABMEK-S        TO TABMEK-I
                   END-IF
               END-PERFORM
               SET TABMEK-I                TO TABMEK-S
               IF  I-10
               AND TABMEK-I NOT > TABFIL-MAX
                   SET TABFIL-I            TO TABMEK-I
               END-IF
           END-IF
           IF  (I-L3 AND I-10)
               SET NOT-I-11                TO TRUE
               IF  TABFIL(TABFIL-I) = 'LONNMAS'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-L3)
               PERFORM RBSRUT-S
      *
           END-IF
           IF  (I-L2)
               MOVE 0                      TO L2ANT1
               MOVE 0                      TO L2TIM1
               MOVE 0                      TO L2TIFJ
               MOVE 0                      TO L2KR1
               MOVE 0                      TO L2KR2
               MOVE 0                      TO L2KR3
               MOVE 0                      TO L2KR4
               MOVE 0                      TO L2KR5
               MOVE 0                      TO L2KR6
               MOVE 0                      TO L2ANT2
               MOVE 0                      TO L2TIM2
               MOVE 0                      TO L2TIA-ELGR
               MOVE 0                      TO L2KR7
               MOVE 0                      TO L2KR8
               MOVE 0                      TO L2KR9
               MOVE 0                      TO L2KR10
               MOVE 0                      TO L2KR11
               MOVE 0                      TO L2KR12
      *
           END-IF
           IF  (I-L3)
               MOVE 0                      TO L3ANT1
               MOVE 0                      TO L3TIM1
               MOVE 0                      TO L3TIFJ
               MOVE 0                      TO L3KR1
               MOVE 0                      TO L3KR2
               MOVE 0                      TO L3KR3
               MOVE 0                      TO L3KR4
               MOVE 0                      TO L3KR5
               MOVE 0                      TO L3KR6
               MOVE 0                      TO L3ANT2
               MOVE 0                      TO L3TIM2
               MOVE 0                      TO L3TIA-ELGR
               MOVE 0                      TO L3KR7
               MOVE 0                      TO L3KR8
               MOVE 0                      TO L3KR9
               MOVE 0                      TO L3KR10
               MOVE 0                      TO L3KR11
               MOVE 0                      TO L3KR12
      *****************************************************************
      * HENTER MEKANIKERNAVN FRA LONNMAS                              *
      *****************************************************************
           END-IF
           IF  (I-L1 AND I-11)
               MOVE MSLMKY                 TO LONNMAS-KEY1
               READ LONNMAS RECORD KEY IS LONNMAS-KEY1
               INVALID KEY
                   SET I-12                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-12            TO TRUE
                   PERFORM LONNMAS-FLDSET
                   PERFORM LONNMAS-IDSET
               END-READ
           END-IF
           IF  (I-L1 AND I-11 AND NOT-I-12)
               MOVE LMNAVN                 TO MEKNVN
      *****************************************************************
      * FINNER PERIODE IÅR/IFJOR: STØRRE, MINDRE, SAMME               *
      * - U8: BRUKER FERDIGMELDINGSDATO. HVIS DENNE IKKE FINNES BRUKES*
      *       DATO FOR SISTE OPPDATERING.                             *
      * -NU8: BRUKER KALENDERDATO FOR KJØRING.                        *
      * - 20: STØRRE ENN PARAMETER-DATO, SKAL IKKE VÆRE MED.          *
      * - 22: DAGENS                                                  *
      * - 21: MINDRE ENN PARAMETER-DATO:                              *
      *       - 23 IFJOR                                              *
      *       - 24 IÅR                                                *
      * AKKUMULERER                                                   *
      *****************************************************************
           END-IF
           IF  (NOT-I-U8 AND I-02)
               SET NOT-I-20                TO TRUE
               SET NOT-I-22                TO TRUE
               IF  MSKDTO > MDDTO
                   SET I-20                TO TRUE
               END-IF
               IF  MSKDTO = MDDTO
                   SET I-22                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-U8 AND I-02 AND I-20)
               GO TO SLUTT-T
           END-IF
           IF  (NOT-I-U8 AND I-02)
               SET NOT-I-20                TO TRUE
               SET NOT-I-23                TO TRUE
               SET NOT-I-24                TO TRUE
               IF  MSKAAR > MDAAR
                   SET I-20                TO TRUE
               END-IF
               IF  MSKAAR < MDAAR
                   SET I-23                TO TRUE
               END-IF
               IF  MSKAAR = MDAAR
                   SET I-24                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-U8 AND I-02 AND I-20)
               GO TO SLUTT-T
           END-IF
           IF  (NOT-I-U8 AND I-02)
               SET NOT-I-25                TO TRUE
               SET NOT-I-27                TO TRUE
               IF  MSKMND > MDMND
                   SET I-25                TO TRUE
               END-IF
               IF  MSKMND = MDMND
                   SET I-27                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-U8 AND I-02 AND I-25)
               GO TO SLUTT-T
           END-IF
           IF  (I-U8 AND I-02)
               SET NOT-I-20                TO TRUE
               SET NOT-I-22                TO TRUE
               IF  MSFDTO > MDDTO
                   SET I-20                TO TRUE
               END-IF
               IF  MSFDTO = MDDTO
                   SET I-22                TO TRUE
               END-IF
           END-IF
           IF  (I-U8 AND I-02 AND I-20)
               GO TO SLUTT-T
           END-IF
           IF  (I-U8 AND I-02)
               SET NOT-I-20                TO TRUE
               SET NOT-I-23                TO TRUE
               SET NOT-I-24                TO TRUE
               IF  MSFAAR > MDAAR
                   SET I-20                TO TRUE
               END-IF
               IF  MSFAAR < MDAAR
                   SET I-23                TO TRUE
               END-IF
               IF  MSFAAR = MDAAR
                   SET I-24                TO TRUE
               END-IF
           END-IF
           IF  (I-U8 AND I-02 AND I-20)
               GO TO SLUTT-T
           END-IF
           IF  (I-U8 AND I-02)
               SET NOT-I-25                TO TRUE
               SET NOT-I-27                TO TRUE
               IF  MSFMND > MDMND
                   SET I-25                TO TRUE
               END-IF
               IF  MSFMND = MDMND
                   SET I-27                TO TRUE
               END-IF
           END-IF
           IF  (I-U8 AND I-02 AND I-25)
               GO TO SLUTT-T
      *
      *                 DAGENS
           END-IF
           IF  (I-02 AND I-22)
               ADD MSTIM                   TO OTDD
               ADD MSOBA                   TO OBDDA
               ADD MSOBD                   TO OBDDD
               ADD MSKBA                   TO KBDDA
               ADD MSKBD                   TO KBDDD
      *
      *                 DENNE MÅNED IÅR
           END-IF
           IF  (I-02 AND I-24 AND I-27)
               ADD MSTIM                   TO OTMA-ELG
               ADD MSOBA                   TO OBMA-ELGA
               ADD MSOBD                   TO OBMA-ELGD
               ADD MSKBA                   TO OBMA-ELGA
               ADD MSKBD                   TO OBMA-ELGD
      *                 DENNE MÅNED IFJOR
           END-IF
           IF  (I-02 AND I-23 AND I-27)
               ADD MSOBA                   TO OBMFA
               ADD MSOBD                   TO OBMFD
               ADD MSKBA                   TO OBMFA
               ADD MSKBD                   TO OBMFD
      *                 DETTE ÅRET
           END-IF
           IF  (I-02 AND I-24)
               ADD MSTIM                   TO OTIA-ELG
               ADD MSOBA                   TO OBIA-ELGA
               ADD MSOBD                   TO OBIA-ELGD
               ADD MSKBA                   TO OBIA-ELGA
               ADD MSKBD                   TO OBIA-ELGD
      *                 IFJOR
           END-IF
           IF  (I-02 AND I-23)
               ADD MSTIM                   TO OTIF
               ADD MSOBA                   TO OBIFA
               ADD MSOBD                   TO OBIFD
               ADD MSKBA                   TO OBIFA
               ADD MSKBD                   TO OBIFD
           END-IF.
 
       SLUTT-T.
      * MEKANIKERTOTAL LINJE 1
           CONTINUE.
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-97                    TO TRUE
           SET NOT-I-99                    TO TRUE
           MOVE ' '                        TO BBEST
           MOVE 'VOS02'                    TO LONR
           MOVE MSFNR                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'VOS045  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-99                    TO TRUE
           IF  LANTX = 0
               SET I-99                    TO TRUE
           END-IF
           IF  (NOT-I-99)
               SET I-97                    TO TRUE
           END-IF.
      *****************************************************************
      * DAGENS VERKSTEDORDRE                                          *
      *****************************************************************
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1)
               ADD OADD TO ZERO        GIVING OADDU ROUNDED
               ADD OTDD TO ZERO        GIVING OTDDU ROUNDED
               ADD OTIF TO ZERO        GIVING OTIFU ROUNDED
               ADD OBDDA TO ZERO       GIVING OBDDAU ROUNDED
               ADD OBDDD TO ZERO       GIVING OBDDDU ROUNDED
               ADD KBDDA TO ZERO       GIVING KBDDAU ROUNDED
               ADD KBDDD TO ZERO       GIVING KBDDDU ROUNDED
               ADD KBDDAU TO OBDDAU    GIVING NBDDAU ROUNDED
               ADD KBDDDU TO OBDDDU    GIVING NBDDDU ROUNDED
      * MEKANIKERTOTAL LINJE 2
           END-IF
           IF  (I-L1)
               ADD OAMA-ELG TO ZERO    GIVING OAMA-ELGU ROUNDED
               ADD OTMA-ELG TO ZERO    GIVING OTMA-ELGU ROUNDED
               ADD OTIA-ELG TO ZERO    GIVING OTIA-ELGU ROUNDED
               ADD OBMA-ELGA TO ZERO   GIVING OBMA-ELGAU ROUNDED
               ADD OBMA-ELGD TO ZERO   GIVING OBMA-ELGDU ROUNDED
               ADD OBIA-ELGA TO ZERO   GIVING OBIA-ELGAU ROUNDED
               ADD OBIA-ELGD TO ZERO   GIVING OBIA-ELGDU ROUNDED
               ADD OBIFA TO ZERO       GIVING OBIFAU ROUNDED
               ADD OBIFD TO ZERO       GIVING OBIFDU ROUNDED
      * AVD.-TOTAL LINJE 1
           END-IF
           IF  (I-L1)
               ADD OADDU                   TO L2ANT1 ROUNDED
               ADD OTDDU                   TO L2TIM1 ROUNDED
               ADD OTIA-ELG                TO L2TIA-ELGR ROUNDED
               ADD OTIF                    TO L2TIFJ ROUNDED
               ADD OBDDAU                  TO L2KR1 ROUNDED
               ADD OBDDDU                  TO L2KR2 ROUNDED
               ADD KBDDAU                  TO L2KR3 ROUNDED
               ADD KBDDDU                  TO L2KR4 ROUNDED
           END-IF
           IF  (I-L2)
               ADD L2KR3 TO L2KR1      GIVING L2KR5 ROUNDED
               ADD L2KR4 TO L2KR2      GIVING L2KR6 ROUNDED
      * AVD.-TOTAL LINJE 2
           END-IF
           IF  (I-L1)
               ADD OAMA-ELGU               TO L2ANT2 ROUNDED
               ADD OTMA-ELGU               TO L2TIM2 ROUNDED
               ADD OBMA-ELGAU              TO L2KR7 ROUNDED
               ADD OBMA-ELGDU              TO L2KR8 ROUNDED
               ADD OBIA-ELGAU              TO L2KR9 ROUNDED
               ADD OBIA-ELGDU              TO L2KR10 ROUNDED
               ADD OBIFAU                  TO L2KR11 ROUNDED
               ADD OBIFDU                  TO L2KR12 ROUNDED
      * FIRMATOTAL LINJE 1
           END-IF
           IF  (I-L1)
               ADD OADDU                   TO L3ANT1 ROUNDED
               ADD OTDDU                   TO L3TIM1 ROUNDED
               ADD OTIA-ELG                TO L3TIA-ELGR ROUNDED
               ADD OTIF                    TO L3TIFJ ROUNDED
               ADD OBDDAU                  TO L3KR1 ROUNDED
               ADD OBDDDU                  TO L3KR2 ROUNDED
               ADD KBDDAU                  TO L3KR3 ROUNDED
               ADD KBDDDU                  TO L3KR4 ROUNDED
           END-IF
           IF  (I-L3)
               ADD L3KR3 TO L3KR1      GIVING L3KR5 ROUNDED
               ADD L3KR4 TO L3KR2      GIVING L3KR6 ROUNDED
      * FIRMATOTAL LINJE 2
           END-IF
           IF  (I-L1)
               ADD OAMA-ELGU               TO L3ANT2 ROUNDED
               ADD OTMA-ELGU               TO L3TIM2 ROUNDED
               ADD OBMA-ELGAU              TO L3KR7 ROUNDED
               ADD OBMA-ELGDU              TO L3KR8 ROUNDED
               ADD OBIA-ELGAU              TO L3KR9 ROUNDED
               ADD OBIA-ELGDU              TO L3KR10 ROUNDED
               ADD OBIFAU                  TO L3KR11 ROUNDED
               ADD OBIFDU                  TO L3KR12 ROUNDED
      *2                   MOVE "OBIFA   "BUGFL1  8        DISPLAY FIELD
      *2         BUGFL1    DEBUGFLISTEO   OBIFA            VIS INDIKATOR
           END-IF
           IF  (I-L1)
               MOVE 0                      TO OADD
               MOVE 0                      TO OAMA-ELG
               MOVE 0                      TO OTDD
               MOVE 0                      TO OTMA-ELG
               MOVE 0                      TO OTIF
               MOVE 0                      TO OTIA-ELG
               MOVE 0                      TO OBDDA
               MOVE 0                      TO OBDDD
               MOVE 0                      TO KBDDA
               MOVE 0                      TO KBDDD
               MOVE 0                      TO OBMA-ELGA
               MOVE 0                      TO OBMA-ELGD
               MOVE 0                      TO OBMFA
               MOVE 0                      TO OBMFD
               MOVE 0                      TO OBIA-ELGA
               MOVE 0                      TO OBIA-ELGD
               MOVE 0                      TO OBIFA
               MOVE 0                      TO OBIFD
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           END-IF
           .
 
       MEKDATO-GET SECTION.
       MEKDATO-GET-P.
           IF  MEKDATO-EOF-OFF
               READ MEKDATO
               AT END
                   SET MEKDATO-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       MEKDATO-FLDOFF SECTION.
       MEKDATO-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-21                TO TRUE
           END-EVALUATE.
 
       MEKDATO-FLDSET SECTION.
       MEKDATO-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE MEKDATO-IO-AREA (04:6) TO MDKLK-IO
               INSPECT MDKLK-IO REPLACING ALL ' ' BY '0'
               IF  MDKLK = ZERO
                   SET I-21                TO TRUE
               END-IF
               MOVE MEKDATO-IO-AREA (16:8) TO MDDTO (1:8)
               MOVE MEKDATO-IO-AREA (16:4) TO MDAAR (1:4)
               MOVE MEKDATO-IO-AREA (20:2) TO MDMND (1:2)
               MOVE MEKDATO-IO-AREA (22:2) TO MDDAG (1:2)
               MOVE MEKDATO-IO-AREA (29:3) TO MDFIR (1:3)
           END-EVALUATE.
 
       MEKDATO-IDSET SECTION.
       MEKDATO-IDSET-P.
           SET I-01                        TO TRUE.
 
       MEKSTAT-GET SECTION.
       MEKSTAT-GET-P.
           IF  MEKSTAT-EOF-OFF
               READ MEKSTAT
               AT END
                   SET MEKSTAT-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       MEKSTAT-FLDSET SECTION.
       MEKSTAT-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE MEKSTAT-IO-AREA (1:3)  TO MSFNR (1:3)
               MOVE MEKSTAT-IO-AREA (4:3)  TO MSMEK (1:3)
               MOVE MEKSTAT-IO-AREA (1:6)  TO MSLMKY (1:6)
               MOVE MEKSTAT-IO-AREA (7:8)  TO MSFDTO (1:8)
               MOVE MEKSTAT-IO-AREA (7:4)  TO MSFAAR (1:4)
               MOVE MEKSTAT-IO-AREA (11:2) TO MSFMND (1:2)
               MOVE MEKSTAT-IO-AREA (15:11) TO MSOBA-IO
               INSPECT MSOBA-IO REPLACING ALL ' ' BY '0'
               MOVE MEKSTAT-IO-AREA (26:11) TO MSOBD-IO
               INSPECT MSOBD-IO REPLACING ALL ' ' BY '0'
               MOVE MEKSTAT-IO-AREA (37:11) TO MSKBA-IO
               INSPECT MSKBA-IO REPLACING ALL ' ' BY '0'
               MOVE MEKSTAT-IO-AREA (48:11) TO MSKBD-IO
               INSPECT MSKBD-IO REPLACING ALL ' ' BY '0'
               MOVE MEKSTAT-IO-AREA (62:9) TO MSTIM-IO
               INSPECT MSTIM-IO REPLACING ALL ' ' BY '0'
               MOVE MEKSTAT-IO-AREA (71:4) TO MSAVD (1:4)
               MOVE MEKSTAT-IO-AREA (107:8) TO MSKDTO (1:8)
               MOVE MEKSTAT-IO-AREA (107:4) TO MSKAAR (1:4)
               MOVE MEKSTAT-IO-AREA (111:2) TO MSKMND (1:2)
           END-EVALUATE.
 
       MEKSTAT-IDSET SECTION.
       MEKSTAT-IDSET-P.
           SET I-02                        TO TRUE.
 
       MEKSTAT-CHK-LEVEL SECTION.
       MEKSTAT-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO MEKSTAT-LEVEL-02
               MOVE MEKSTAT-IO-AREA (1:3)  TO MEKSTAT-02-L3-MSFNR
               MOVE MEKSTAT-IO-AREA (71:4) TO MEKSTAT-02-L2-MSAVD
               MOVE MEKSTAT-IO-AREA (4:3)  TO MEKSTAT-02-L1-MSMEK
               IF  MEKSTAT-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  MEKSTAT-02-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  MEKSTAT-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  MEKSTAT-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  MEKSTAT-02-L3         TO THE-PRIOR-L3
               MOVE  MEKSTAT-02-L2         TO THE-PRIOR-L2
               MOVE  MEKSTAT-02-L1         TO THE-PRIOR-L1
               SET MEKSTAT-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       LONNMAS-FLDSET SECTION.
       LONNMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LONNMAS-IO-AREA (12:30) TO LMNAVN (1:30)
           END-EVALUATE.
 
       LONNMAS-IDSET SECTION.
       LONNMAS-IDSET-P.
           SET I-03                        TO TRUE.
 
       MEKLIST-PRINT-LINE SECTION.
       MEKLIST-PRINT-LINE-P.
           IF  MEKLIST-BEFORE-SKIP > 0
               PERFORM MEKLIST-SKIP-BEFORE
           END-IF
           IF  MEKLIST-BEFORE-SPACE > 0
               PERFORM MEKLIST-SPACE-BEFORE
               IF  MEKLIST-AFTER-SKIP > 0
                   PERFORM MEKLIST-SKIP-AFTER
               END-IF
               IF  MEKLIST-AFTER-SPACE > 0
                   PERFORM MEKLIST-SPACE-AFTER
               END-IF
           ELSE
               IF  MEKLIST-AFTER-SKIP > 0
                   PERFORM MEKLIST-SKIP-AFTER
               END-IF
               PERFORM MEKLIST-SPACE-AFTER
           END-IF
           IF  MEKLIST-LINE-COUNT NOT < MEKLIST-MAX-LINES
               SET I-OF                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OF            TO TRUE
               END-IF
           END-IF.
 
       MEKLIST-SKIP-BEFORE SECTION.
       MEKLIST-SKIP-BEFORE-P.
           WRITE MEKLIST-IO-PRINT       AFTER ADVANCING PAGE
           MOVE 1                          TO MEKLIST-LINE-COUNT
           MOVE 0                          TO MEKLIST-BEFORE-SKIP
           INITIALIZE MEKLIST-IO-AREA.
 
       MEKLIST-SPACE-BEFORE SECTION.
       MEKLIST-SPACE-BEFORE-P.
           WRITE MEKLIST-IO-PRINT       AFTER MEKLIST-BEFORE-SPACE
                                                                 LINES
           ADD MEKLIST-BEFORE-SPACE        TO MEKLIST-LINE-COUNT
           MOVE SPACES TO MEKLIST-IO-AREA
           INITIALIZE MEKLIST-IO-AREA
           MOVE 0                          TO MEKLIST-BEFORE-SPACE.
 
       MEKLIST-SKIP-AFTER SECTION.
       MEKLIST-SKIP-AFTER-P.
           WRITE MEKLIST-IO-PRINT      BEFORE ADVANCING PAGE
           MOVE 1                          TO MEKLIST-LINE-COUNT
           MOVE 0                          TO MEKLIST-AFTER-SKIP
           INITIALIZE MEKLIST-IO-AREA.
 
       MEKLIST-SPACE-AFTER SECTION.
       MEKLIST-SPACE-AFTER-P.
           WRITE MEKLIST-IO-PRINT      BEFORE MEKLIST-AFTER-SPACE LINES
           ADD MEKLIST-AFTER-SPACE         TO MEKLIST-LINE-COUNT
           INITIALIZE MEKLIST-IO-AREA
           MOVE 0                          TO MEKLIST-AFTER-SPACE.
 
       MEKTAB-LOAD SECTION.
       MEKTAB-LOAD-P.
           OPEN INPUT MEKTAB
           SET TABMEK-I                    TO 1
           PERFORM UNTIL MEKTAB-EOF
               READ MEKTAB
               AT END
                   SET MEKTAB-EOF          TO TRUE
               NOT AT END
                   MOVE MEKTAB-IO-AREA (1:10) TO TABMEK-ENTRY
                                                            (TABMEK-I)
                   SET TABMEK-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE MEKTAB.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-U6 AND I-U7 AND I-U8)
           AND (I-03)
               MOVE SPACES TO MEKLIST-IO-AREA
               INITIALIZE MEKLIST-IO-AREA
               MOVE LONR                   TO MEKLIST-IO-AREA (116:5)
               MOVE LFIRMA                 TO MEKLIST-IO-AREA (118:3)
               MOVE LUNDGR                 TO MEKLIST-IO-AREA (118:3)
               MOVE LPROG                  TO MEKLIST-IO-AREA (113:8)
               MOVE LANTX-IO               TO MEKLIST-IO-AREA (118:3)
               MOVE LPRIID                 TO MEKLIST-IO-AREA (117:4)
               MOVE BBEST                  TO MEKLIST-IO-AREA (120:1)
      *                        BFILL    120
               MOVE 1                      TO MEKLIST-BEFORE-SPACE
               MOVE 1                      TO MEKLIST-AFTER-SPACE
               PERFORM MEKLIST-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L2 AND I-97)
               MOVE SPACES TO MEKLIST-IO-AREA
               INITIALIZE MEKLIST-IO-AREA
               MOVE 'DATO:'                TO MEKLIST-IO-AREA (82:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO MEKLIST-IO-AREA (88:8)
               MOVE 'TIDSPUNKT:'           TO MEKLIST-IO-AREA (99:10)
               MOVE TIDSP                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO MEKLIST-IO-AREA (110:8)
               MOVE 'SIDE:'                TO MEKLIST-IO-AREA (122:5)
               ADD 1                       TO PAGE1
               MOVE PAGE1                  TO XO-40YNZ
               MOVE XO-40YNZ               TO MEKLIST-IO-AREA (127:4)
               MOVE 01                     TO MEKLIST-BEFORE-SKIP
               MOVE 2                      TO MEKLIST-AFTER-SPACE
               PERFORM MEKLIST-PRINT-LINE
               MOVE SPACES TO MEKLIST-IO-AREA
               INITIALIZE MEKLIST-IO-AREA
               MOVE MSFNR                  TO MEKLIST-IO-AREA (1:3)
               MOVE FIRNVN                 TO MEKLIST-IO-AREA (5:30)
               MOVE 'VERKSTEDORDRE. '      TO MEKLIST-IO-AREA (50:15)
               MOVE 'MEKANIKER-STATISTIKK.' TO MEKLIST-IO-AREA (65:21)
               MOVE 'JOBB=VOS02A'          TO MEKLIST-IO-AREA (104:11)
               MOVE 'PROGRAM=VOS045'       TO MEKLIST-IO-AREA (117:14)
               MOVE 2                      TO MEKLIST-AFTER-SPACE
               PERFORM MEKLIST-PRINT-LINE
               MOVE SPACES TO MEKLIST-IO-AREA
               INITIALIZE MEKLIST-IO-AREA
               MOVE 'AVDELING '            TO MEKLIST-IO-AREA (1:9)
               MOVE MSAVD                  TO MEKLIST-IO-AREA (10:4)
               MOVE 'FERDIGMELDTE ORDRE PR' TO MEKLIST-IO-AREA (50:21)
               MOVE MDDAG                  TO MEKLIST-IO-AREA (72:2)
               MOVE '.'                    TO MEKLIST-IO-AREA (74:1)
               MOVE MDMND                  TO MEKLIST-IO-AREA (75:2)
               MOVE '.'                    TO MEKLIST-IO-AREA (77:1)
               MOVE MDAAR                  TO MEKLIST-IO-AREA (78:4)
               IF  (NOT-I-21)
                   MOVE MDKLK              TO EDIT-MDKLK
                   MOVE '  :  :  '         TO MEKLIST-IO-AREA (84:8)
                   MOVE EDIT-MDKLK (1:2)   TO MEKLIST-IO-AREA (84:2)
                   MOVE EDIT-MDKLK (3:2)   TO MEKLIST-IO-AREA (87:2)
                   MOVE EDIT-MDKLK (5:2)   TO MEKLIST-IO-AREA (90:2)
               END-IF
               MOVE 2                      TO MEKLIST-AFTER-SPACE
               PERFORM MEKLIST-PRINT-LINE
               MOVE SPACES TO MEKLIST-IO-AREA
               INITIALIZE MEKLIST-IO-AREA
               MOVE '*ANTALL  '            TO MEKLIST-IO-AREA (37:9)
               MOVE '*ANTALL  '            TO MEKLIST-IO-AREA (46:9)
               MOVE '*ORDR/NET BEL'        TO MEKLIST-IO-AREA (55:13)
               MOVE '*ORDR/NET BEL'        TO MEKLIST-IO-AREA (68:13)
               MOVE '*KRED/NET BEL'        TO MEKLIST-IO-AREA (81:13)
               MOVE '*KRED/NET BEL'        TO MEKLIST-IO-AREA (94:13)
               MOVE '*NETTO BELØP '        TO MEKLIST-IO-AREA (107:13)
               MOVE '*NETTO BELØP '        TO MEKLIST-IO-AREA (120:13)
               MOVE 1                      TO MEKLIST-AFTER-SPACE
               PERFORM MEKLIST-PRINT-LINE
               MOVE SPACES TO MEKLIST-IO-AREA
               INITIALIZE MEKLIST-IO-AREA
               MOVE 'MEK'                  TO MEKLIST-IO-AREA (1:3)
               MOVE '*TIMER   '            TO MEKLIST-IO-AREA (37:9)
               MOVE '*TIMER   '            TO MEKLIST-IO-AREA (46:9)
               MOVE '*ARBEID IDAG/'        TO MEKLIST-IO-AREA (55:13)
               MOVE '*DELER IDAG/ '        TO MEKLIST-IO-AREA (68:13)
               MOVE '*ARBEID IDAG/'        TO MEKLIST-IO-AREA (81:13)
               MOVE '*DELER IDAG/ '        TO MEKLIST-IO-AREA (94:13)
               MOVE '*ARBEID IDAG/'        TO MEKLIST-IO-AREA (107:13)
               MOVE '*DELER IDAG/ '        TO MEKLIST-IO-AREA (120:13)
               MOVE 1                      TO MEKLIST-AFTER-SPACE
               PERFORM MEKLIST-PRINT-LINE
               MOVE SPACES TO MEKLIST-IO-AREA
               INITIALIZE MEKLIST-IO-AREA
               MOVE 'NR '                  TO MEKLIST-IO-AREA (1:3)
               MOVE 'MEKANIKER NAVN'       TO MEKLIST-IO-AREA (6:14)
               MOVE '*IDAG/MND'            TO MEKLIST-IO-AREA (37:9)
               MOVE '*IFJO/ÅR '            TO MEKLIST-IO-AREA (46:9)
               MOVE '*DENNE MND   '        TO MEKLIST-IO-AREA (55:13)
               MOVE '*DENNE MND   '        TO MEKLIST-IO-AREA (68:13)
               MOVE '*I ÅR        '        TO MEKLIST-IO-AREA (81:13)
               MOVE '*I ÅR        '        TO MEKLIST-IO-AREA (94:13)
               MOVE '*I FJOR      '        TO MEKLIST-IO-AREA (107:13)
               MOVE '*I FJOR      '        TO MEKLIST-IO-AREA (120:13)
               MOVE 1                      TO MEKLIST-AFTER-SPACE
               PERFORM MEKLIST-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF AND I-97)
               MOVE SPACES TO MEKLIST-IO-AREA
               INITIALIZE MEKLIST-IO-AREA
               MOVE 'DATO:'                TO MEKLIST-IO-AREA (82:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO MEKLIST-IO-AREA (88:8)
               MOVE 'TIDSPUNKT:'           TO MEKLIST-IO-AREA (99:10)
               MOVE TIDSP                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO MEKLIST-IO-AREA (110:8)
               MOVE 'SIDE:'                TO MEKLIST-IO-AREA (122:5)
               ADD 1                       TO PAGE1
               MOVE PAGE1                  TO XO-40YNZ
               MOVE XO-40YNZ               TO MEKLIST-IO-AREA (127:4)
               MOVE 01                     TO MEKLIST-BEFORE-SKIP
               MOVE 2                      TO MEKLIST-AFTER-SPACE
               PERFORM MEKLIST-PRINT-LINE
               MOVE SPACES TO MEKLIST-IO-AREA
               INITIALIZE MEKLIST-IO-AREA
               MOVE MSFNR                  TO MEKLIST-IO-AREA (1:3)
               MOVE FIRNVN                 TO MEKLIST-IO-AREA (5:30)
               MOVE 'VERKSTEDORDRE. '      TO MEKLIST-IO-AREA (50:15)
               MOVE 'MEKANIKER-STATISTIKK.' TO MEKLIST-IO-AREA (65:21)
               MOVE 'JOBB=VOS02A'          TO MEKLIST-IO-AREA (104:11)
               MOVE 'PROGRAM=VOS045'       TO MEKLIST-IO-AREA (117:14)
               MOVE 2                      TO MEKLIST-AFTER-SPACE
               PERFORM MEKLIST-PRINT-LINE
               MOVE SPACES TO MEKLIST-IO-AREA
               INITIALIZE MEKLIST-IO-AREA
               MOVE 'AVDELING '            TO MEKLIST-IO-AREA (1:9)
               MOVE MSAVD                  TO MEKLIST-IO-AREA (10:4)
               MOVE 'FERDIGMELDTE ORDRE PR' TO MEKLIST-IO-AREA (50:21)
               MOVE MDDAG                  TO MEKLIST-IO-AREA (72:2)
               MOVE '.'                    TO MEKLIST-IO-AREA (74:1)
               MOVE MDMND                  TO MEKLIST-IO-AREA (75:2)
               MOVE '.'                    TO MEKLIST-IO-AREA (77:1)
               MOVE MDAAR                  TO MEKLIST-IO-AREA (78:4)
               IF  (NOT-I-21)
                   MOVE MDKLK              TO EDIT-MDKLK
                   MOVE '  :  :  '         TO MEKLIST-IO-AREA (84:8)
                   MOVE EDIT-MDKLK (1:2)   TO MEKLIST-IO-AREA (84:2)
                   MOVE EDIT-MDKLK (3:2)   TO MEKLIST-IO-AREA (87:2)
                   MOVE EDIT-MDKLK (5:2)   TO MEKLIST-IO-AREA (90:2)
               END-IF
               MOVE 2                      TO MEKLIST-AFTER-SPACE
               PERFORM MEKLIST-PRINT-LINE
               MOVE SPACES TO MEKLIST-IO-AREA
               INITIALIZE MEKLIST-IO-AREA
               MOVE '*ANTALL  '            TO MEKLIST-IO-AREA (37:9)
               MOVE '*ANTALL  '            TO MEKLIST-IO-AREA (46:9)
               MOVE '*ORDR/NET BEL'        TO MEKLIST-IO-AREA (55:13)
               MOVE '*ORDR/NET BEL'        TO MEKLIST-IO-AREA (68:13)
               MOVE '*KRED/NET BEL'        TO MEKLIST-IO-AREA (81:13)
               MOVE '*KRED/NET BEL'        TO MEKLIST-IO-AREA (94:13)
               MOVE '*NETTO BELØP '        TO MEKLIST-IO-AREA (107:13)
               MOVE '*NETTO BELØP '        TO MEKLIST-IO-AREA (120:13)
               MOVE 1                      TO MEKLIST-AFTER-SPACE
               PERFORM MEKLIST-PRINT-LINE
               MOVE SPACES TO MEKLIST-IO-AREA
               INITIALIZE MEKLIST-IO-AREA
               MOVE 'MEK'                  TO MEKLIST-IO-AREA (1:3)
               MOVE '*TIMER   '            TO MEKLIST-IO-AREA (37:9)
               MOVE '*TIMER   '            TO MEKLIST-IO-AREA (46:9)
               MOVE '*ARBEID IDAG/'        TO MEKLIST-IO-AREA (55:13)
               MOVE '*DELER IDAG/ '        TO MEKLIST-IO-AREA (68:13)
               MOVE '*ARBEID IDAG/'        TO MEKLIST-IO-AREA (81:13)
               MOVE '*DELER IDAG/ '        TO MEKLIST-IO-AREA (94:13)
               MOVE '*ARBEID IDAG/'        TO MEKLIST-IO-AREA (107:13)
               MOVE '*DELER IDAG/ '        TO MEKLIST-IO-AREA (120:13)
               MOVE 1                      TO MEKLIST-AFTER-SPACE
               PERFORM MEKLIST-PRINT-LINE
               MOVE SPACES TO MEKLIST-IO-AREA
               INITIALIZE MEKLIST-IO-AREA
               MOVE 'NR '                  TO MEKLIST-IO-AREA (1:3)
               MOVE 'MEKANIKER NAVN'       TO MEKLIST-IO-AREA (6:14)
               MOVE '*IDAG/MND'            TO MEKLIST-IO-AREA (37:9)
               MOVE '*IFJO/ÅR '            TO MEKLIST-IO-AREA (46:9)
               MOVE '*DENNE MND   '        TO MEKLIST-IO-AREA (55:13)
               MOVE '*DENNE MND   '        TO MEKLIST-IO-AREA (68:13)
               MOVE '*I ÅR        '        TO MEKLIST-IO-AREA (81:13)
               MOVE '*I ÅR        '        TO MEKLIST-IO-AREA (94:13)
               MOVE '*I FJOR      '        TO MEKLIST-IO-AREA (107:13)
               MOVE '*I FJOR      '        TO MEKLIST-IO-AREA (120:13)
               MOVE 1                      TO MEKLIST-AFTER-SPACE
               PERFORM MEKLIST-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-97)
               MOVE SPACES TO MEKLIST-IO-AREA
               INITIALIZE MEKLIST-IO-AREA
               MOVE MSMEK                  TO MEKLIST-IO-AREA (1:3)
               IF  (I-11 AND NOT-I-12)
                   MOVE MEKNVN             TO MEKLIST-IO-AREA (6:30)
               END-IF
               IF  (I-11 AND I-12)
                   MOVE '***** UKJENT MEKANIKER *' TO MEKLIST-IO-AREA
                                                                (6:24)
               END-IF
               IF  (I-11 AND I-12)
                   MOVE '****  '           TO MEKLIST-IO-AREA (30:6)
               END-IF
               IF  (NOT-I-11)
                   MOVE '***** UKJENT MEKANIKER *' TO MEKLIST-IO-AREA
                                                                (6:24)
               END-IF
               IF  (NOT-I-11)
                   MOVE '****  '           TO MEKLIST-IO-AREA (30:6)
      *                        OADDU MB  45
               END-IF
               MOVE OTDDU                  TO XO-80YNZR
               MOVE XO-80YNZR              TO MEKLIST-IO-AREA (37:9)
               INITIALIZE OTDDU
               MOVE OTIFU                  TO XO-80YNZR
               MOVE XO-80YNZR              TO MEKLIST-IO-AREA (46:9)
               INITIALIZE OTIFU
               MOVE OBDDAU                 TO XO-100YY9R
               MOVE XO-100YY9R             TO MEKLIST-IO-AREA (54:14)
               INITIALIZE OBDDAU
               MOVE OBDDDU                 TO XO-100YY9R
               MOVE XO-100YY9R             TO MEKLIST-IO-AREA (67:14)
               INITIALIZE OBDDDU
               MOVE KBDDAU                 TO XO-100YY9R
               MOVE XO-100YY9R             TO MEKLIST-IO-AREA (80:14)
               INITIALIZE KBDDAU
               MOVE KBDDDU                 TO XO-100YY9R
               MOVE XO-100YY9R             TO MEKLIST-IO-AREA (93:14)
               INITIALIZE KBDDDU
               MOVE NBDDAU                 TO XO-100YY9R
               MOVE XO-100YY9R             TO MEKLIST-IO-AREA (106:14)
               INITIALIZE NBDDAU
               MOVE NBDDDU                 TO XO-100YY9R
               MOVE XO-100YY9R             TO MEKLIST-IO-AREA (119:14)
               INITIALIZE NBDDDU
               MOVE 1                      TO MEKLIST-BEFORE-SPACE
               MOVE 1                      TO MEKLIST-AFTER-SPACE
               PERFORM MEKLIST-PRINT-LINE
      *                        OAMÅU MB  45
               MOVE SPACES TO MEKLIST-IO-AREA
               INITIALIZE MEKLIST-IO-AREA
               MOVE OTMA-ELGU              TO XO-80YNZR
               MOVE XO-80YNZR              TO MEKLIST-IO-AREA (37:9)
               INITIALIZE OTMA-ELGU
               MOVE OTIA-ELGU              TO XO-80YNZR
               MOVE XO-80YNZR              TO MEKLIST-IO-AREA (46:9)
               INITIALIZE OTIA-ELGU
               MOVE OBMA-ELGAU             TO XO-100YY9R
               MOVE XO-100YY9R             TO MEKLIST-IO-AREA (54:14)
               INITIALIZE OBMA-ELGAU
               MOVE OBMA-ELGDU             TO XO-100YY9R
               MOVE XO-100YY9R             TO MEKLIST-IO-AREA (67:14)
               INITIALIZE OBMA-ELGDU
               MOVE OBIA-ELGAU             TO XO-100YY9R
               MOVE XO-100YY9R             TO MEKLIST-IO-AREA (80:14)
               INITIALIZE OBIA-ELGAU
               MOVE OBIA-ELGDU             TO XO-100YY9R
               MOVE XO-100YY9R             TO MEKLIST-IO-AREA (93:14)
               INITIALIZE OBIA-ELGDU
               MOVE OBIFAU                 TO XO-100YY9R
               MOVE XO-100YY9R             TO MEKLIST-IO-AREA (106:14)
               INITIALIZE OBIFAU
               MOVE OBIFDU                 TO XO-100YY9R
               MOVE XO-100YY9R             TO MEKLIST-IO-AREA (119:14)
               INITIALIZE OBIFDU
               MOVE 1                      TO MEKLIST-AFTER-SPACE
               PERFORM MEKLIST-PRINT-LINE
           END-IF
           IF  (I-L2 AND I-97)
               MOVE SPACES TO MEKLIST-IO-AREA
               INITIALIZE MEKLIST-IO-AREA
               MOVE '***** TOT AVD '       TO MEKLIST-IO-AREA (1:14)
               MOVE MSAVD                  TO MEKLIST-IO-AREA (15:4)
               MOVE ' *****'               TO MEKLIST-IO-AREA (19:6)
      *                        L2ANT1MB  45
               MOVE L2TIM1                 TO XO-80YNZR
               MOVE XO-80YNZR              TO MEKLIST-IO-AREA (37:9)
               INITIALIZE L2TIM1
               MOVE L2TIFJ                 TO XO-80YNZR
               MOVE XO-80YNZR              TO MEKLIST-IO-AREA (46:9)
               INITIALIZE L2TIFJ
               MOVE L2KR1                  TO XO-100YY9R
               MOVE XO-100YY9R             TO MEKLIST-IO-AREA (54:14)
               INITIALIZE L2KR1
               MOVE L2KR2                  TO XO-100YY9R
               MOVE XO-100YY9R             TO MEKLIST-IO-AREA (67:14)
               INITIALIZE L2KR2
               MOVE L2KR3                  TO XO-100YY9R
               MOVE XO-100YY9R             TO MEKLIST-IO-AREA (80:14)
               INITIALIZE L2KR3
               MOVE L2KR4                  TO XO-100YY9R
               MOVE XO-100YY9R             TO MEKLIST-IO-AREA (93:14)
               INITIALIZE L2KR4
               MOVE L2KR5                  TO XO-100YY9R
               MOVE XO-100YY9R             TO MEKLIST-IO-AREA (106:14)
               INITIALIZE L2KR5
               MOVE L2KR6                  TO XO-100YY9R
               MOVE XO-100YY9R             TO MEKLIST-IO-AREA (119:14)
               INITIALIZE L2KR6
               MOVE 1                      TO MEKLIST-BEFORE-SPACE
               MOVE 1                      TO MEKLIST-AFTER-SPACE
               PERFORM MEKLIST-PRINT-LINE
      *                        L2ANT2MB  45
               MOVE SPACES TO MEKLIST-IO-AREA
               INITIALIZE MEKLIST-IO-AREA
               MOVE L2TIM2                 TO XO-80YNZR
               MOVE XO-80YNZR              TO MEKLIST-IO-AREA (37:9)
               INITIALIZE L2TIM2
               MOVE L2TIA-ELGR             TO XO-80YNZR
               MOVE XO-80YNZR              TO MEKLIST-IO-AREA (46:9)
               INITIALIZE L2TIA-ELGR
               MOVE L2KR7                  TO XO-100YY9R
               MOVE XO-100YY9R             TO MEKLIST-IO-AREA (54:14)
               INITIALIZE L2KR7
               MOVE L2KR8                  TO XO-100YY9R
               MOVE XO-100YY9R             TO MEKLIST-IO-AREA (67:14)
               INITIALIZE L2KR8
               MOVE L2KR9                  TO XO-100YY9R
               MOVE XO-100YY9R             TO MEKLIST-IO-AREA (80:14)
               INITIALIZE L2KR9
               MOVE L2KR10                 TO XO-100YY9R
               MOVE XO-100YY9R             TO MEKLIST-IO-AREA (93:14)
               INITIALIZE L2KR10
               MOVE L2KR11                 TO XO-100YY9R
               MOVE XO-100YY9R             TO MEKLIST-IO-AREA (106:14)
               INITIALIZE L2KR11
               MOVE L2KR12                 TO XO-100YY9R
               MOVE XO-100YY9R             TO MEKLIST-IO-AREA (119:14)
               INITIALIZE L2KR12
               MOVE 1                      TO MEKLIST-AFTER-SPACE
               PERFORM MEKLIST-PRINT-LINE
           END-IF
           IF  (I-L3 AND I-97)
               MOVE SPACES TO MEKLIST-IO-AREA
               INITIALIZE MEKLIST-IO-AREA
               MOVE '*****  FIRMATOTAL  *****' TO MEKLIST-IO-AREA
                                                                (1:24)
      *                        L3ANT1MB  45
               MOVE L3TIM1                 TO XO-80YNZR
               MOVE XO-80YNZR              TO MEKLIST-IO-AREA (37:9)
               INITIALIZE L3TIM1
               MOVE L3TIFJ                 TO XO-80YNZR
               MOVE XO-80YNZR              TO MEKLIST-IO-AREA (46:9)
               INITIALIZE L3TIFJ
               MOVE L3KR1                  TO XO-100YY9R
               MOVE XO-100YY9R             TO MEKLIST-IO-AREA (54:14)
               INITIALIZE L3KR1
               MOVE L3KR2                  TO XO-100YY9R
               MOVE XO-100YY9R             TO MEKLIST-IO-AREA (67:14)
               INITIALIZE L3KR2
               MOVE L3KR3                  TO XO-100YY9R
               MOVE XO-100YY9R             TO MEKLIST-IO-AREA (80:14)
               INITIALIZE L3KR3
               MOVE L3KR4                  TO XO-100YY9R
               MOVE XO-100YY9R             TO MEKLIST-IO-AREA (93:14)
               INITIALIZE L3KR4
               MOVE L3KR5                  TO XO-100YY9R
               MOVE XO-100YY9R             TO MEKLIST-IO-AREA (106:14)
               INITIALIZE L3KR5
               MOVE L3KR6                  TO XO-100YY9R
               MOVE XO-100YY9R             TO MEKLIST-IO-AREA (119:14)
               INITIALIZE L3KR6
               MOVE 1                      TO MEKLIST-BEFORE-SPACE
               MOVE 1                      TO MEKLIST-AFTER-SPACE
               PERFORM MEKLIST-PRINT-LINE
      *                        L3ANT2MB  45
               MOVE SPACES TO MEKLIST-IO-AREA
               INITIALIZE MEKLIST-IO-AREA
               MOVE L3TIM2                 TO XO-80YNZR
               MOVE XO-80YNZR              TO MEKLIST-IO-AREA (37:9)
               INITIALIZE L3TIM2
               MOVE L3TIA-ELGR             TO XO-80YNZR
               MOVE XO-80YNZR              TO MEKLIST-IO-AREA (46:9)
               INITIALIZE L3TIA-ELGR
               MOVE L3KR7                  TO XO-100YY9R
               MOVE XO-100YY9R             TO MEKLIST-IO-AREA (54:14)
               INITIALIZE L3KR7
               MOVE L3KR8                  TO XO-100YY9R
               MOVE XO-100YY9R             TO MEKLIST-IO-AREA (67:14)
               INITIALIZE L3KR8
               MOVE L3KR9                  TO XO-100YY9R
               MOVE XO-100YY9R             TO MEKLIST-IO-AREA (80:14)
               INITIALIZE L3KR9
               MOVE L3KR10                 TO XO-100YY9R
               MOVE XO-100YY9R             TO MEKLIST-IO-AREA (93:14)
               INITIALIZE L3KR10
               MOVE L3KR11                 TO XO-100YY9R
               MOVE XO-100YY9R             TO MEKLIST-IO-AREA (106:14)
               INITIALIZE L3KR11
               MOVE L3KR12                 TO XO-100YY9R
               MOVE XO-100YY9R             TO MEKLIST-IO-AREA (119:14)
               INITIALIZE L3KR12
      * TEST-LINJER
      *       D 11     L1 97
      *                        MSMEK      3
      *                        MEKNVN    35
      *                        MSANT M   45
      *                        MSTIM M   45
      *                        MSOBA J   67
      *                        MSOBD J   80
      *                        MSKBA J   93
      *                        MSKBD J  106
      *       D 11     97
      *                        MSREC    120
      * DUMMY-LINJE FOR Å FJERNE KOMPILERINGSFEIL
               MOVE 1                      TO MEKLIST-AFTER-SPACE
               PERFORM MEKLIST-PRINT-LINE
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
           PERFORM MEKTAB-LOAD
           INITIALIZE MEKDATO-DATA-FIELDS
           SET MEKDATO-EOF-OFF             TO TRUE
           SET MEKDATO-PROCESS             TO TRUE
           OPEN INPUT MEKDATO
           SET MEKSTAT-LEVEL-INIT          TO TRUE
           INITIALIZE MEKSTAT-DATA-FIELDS
           SET MEKSTAT-EOF-OFF             TO TRUE
           SET MEKSTAT-PROCESS             TO TRUE
           OPEN INPUT MEKSTAT
           INITIALIZE LONNMAS-DATA-FIELDS
           OPEN INPUT LONNMAS
           OPEN OUTPUT MEKLIST
           INITIALIZE MEKLIST-IO-AREA
           INITIALIZE MEKLIST-DATA-FIELDS
           MOVE 57                         TO MEKLIST-MAX-LINES.
           SET TABMEK-I                    TO 1
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE MEKDATO
           CLOSE MEKSTAT
           CLOSE LONNMAS
           IF MEKLIST-IO-AREA NOT = SPACES
             WRITE MEKLIST-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO MEKLIST-IO-AREA
           END-IF
           CLOSE MEKLIST.
 
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
