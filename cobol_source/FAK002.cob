       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK002R.
      **********************************************  Z-WIN-RPG2   ****
      * OPPDATERING AV PARAMETER OG FAKTURANRFILE FAKTURARUTINE.
      * OPPDATERING AV PARAMETERFILE LAGERSTYRING ER FLYTTET TIL FAK003
      * 21.10.98 ÅRHUNDRE LAGT UT PÅ LISTE OG I FILE. AAR100
      * 10.04.04 ADVARSEL NÅR FAKTURANR ER STØRRE ENN 895000
      ******************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK002.rpg
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
           SELECT KORTAB
               ASSIGN TO UT-S-KORTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KORTAB-STATUS.
           SELECT MNDTAB
               ASSIGN TO UT-S-MNDTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS MNDTAB-STATUS.
           SELECT KORTIN
               ASSIGN TO UT-S-KORTIN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KORTIN-STATUS.
           SELECT FAKPAR
               ASSIGN TO UT-S-FAKPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKPAR-STATUS.
           SELECT FNRIN
               ASSIGN TO UT-S-FNRIN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FNRIN-STATUS.
           SELECT FNRUT
               ASSIGN TO UT-S-FNRUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FNRUT-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD KORTAB
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  KORTAB-IO-AREA.
           05  KORTAB-IO-AREA-X            PICTURE X(80).
       FD MNDTAB
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  MNDTAB-IO-AREA.
           05  MNDTAB-IO-AREA-X            PICTURE X(80).
       FD KORTIN
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  KORTIN-IO-AREA.
           05  KORTIN-IO-AREA-X            PICTURE X(80).
       FD FAKPAR
               BLOCK CONTAINS 200
               RECORD CONTAINS 200.
       01  FAKPAR-IO-AREA.
           05  FAKPAR-IO-AREA-X            PICTURE X(200).
       FD FNRIN
               BLOCK CONTAINS 1050
               RECORD CONTAINS 50.
       01  FNRIN-IO-AREA.
           05  FNRIN-IO-AREA-X             PICTURE X(50).
       FD FNRUT
               BLOCK CONTAINS 1050
               RECORD CONTAINS 50.
       01  FNRUT-IO-AREA.
           05  FNRUT-IO-AREA-X             PICTURE X(50).
      * ID F  C  H  L    FUNCTION OF INDICATORS
      * 01            PARAMETERKORT.
      * 02            FIRMAKORT MED FØRSTE FAKTURANR/KREDITNOTANR.
      * 03            FAKTURANUMMER-FILE.
      *       10      FEIL INNKASSOPERIODE.
      *       11      JA I FØRSTE KJØRING I MND.
      *       12      NEI I FØRSTE KJØRING I MND.
      *       13      FEIL SVAR I FØRSTE KJØRING I MND
      *       14      JA I SAMLEFAKTURERING.
      *       15      NEI I SAMLEFAKTURERING.
      *       16      FEIL SVAR I SAMLEFAKTURERING.
      *       17      FORFALL 30 DAGER IKKE I TABELL.
      *       18      FORFALL 45 DAGER IKKE I TABELL.
      *       19      FORFALL 60 DAGER IKKE I TABELL.
      *       20      FORFALL 90 DAGER IKKE I TABELL.
      *       21      FAKTURAOMGANG 1,FØRSTE KJØRING I ÅRET
      *       22      FUNNET PERIODE I MNDTABELL.
      *       23      FEIL I FAKTURANR. I MND.
      *       24      FORFALL 15 DAGER IKKE I TABELL.
      *       25      FEIL KODE I FAKTURAGRUPPE.
      *       26      ALLE FIRMA SKAL HA FAKTURA,
      *       27      FORFALL 75 DAGER IKKE I TABELL.
      *       34      FORFALL 105 DAGER IKKE I TABELL.
      *       35      FORFALL 120 DAGER IKKE I TABELL.
      *       36      FORFALL 150 DAGER IKKE I TABELL.
      *       37      FORFALL 180 DAGER IKKE I TABELL.
      *       38      SISTE (4) FAKTURAKJØRING I MND.
      *       53      FEIL SVAR I SISTE FAKT I MND.
      *       55      FEIL SVAR I SISTE FAKT I MND.
      *
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       WORKING-STORAGE SECTION.
       77  TABPER-MAX   VALUE 192          PICTURE 9(4) USAGE BINARY.
       77  TABDTO-MAX   VALUE 192          PICTURE 9(4) USAGE BINARY.
       77  TABMNR-MAX   VALUE 12           PICTURE 9(4) USAGE BINARY.
       77  TABMND-MAX   VALUE 12           PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  TABPER-TABLE.
               10  TABPER-ENTRY
                                           OCCURS 192 TIMES
                                           INDEXED BY TABPER-I
                                                      TABPER-S
                                                      TABDTO-I
                                                      TABDTO-S.
                   15  TABPER              PICTURE S9(4).
                   15  TABDTO              PICTURE X(6).
           05  TABMNR-TABLE.
               10  TABMNR-ENTRY
                                           OCCURS 12 TIMES
                                           INDEXED BY TABMNR-I
                                                      TABMNR-S
                                                      TABMND-I
                                                      TABMND-S.
                   15  TABMNR              PICTURE S9(2).
                   15  TABMND              PICTURE X(9).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  KORTAB-STATUS               PICTURE 99 VALUE 0.
           10  MNDTAB-STATUS               PICTURE 99 VALUE 0.
           10  KORTIN-STATUS               PICTURE 99 VALUE 0.
           10  FAKPAR-STATUS               PICTURE 99 VALUE 0.
           10  FNRIN-STATUS                PICTURE 99 VALUE 0.
           10  FNRUT-STATUS                PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  KORTAB-EOF-OFF          VALUE '0'.
               88  KORTAB-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  MNDTAB-EOF-OFF          VALUE '0'.
               88  MNDTAB-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KORTIN-EOF-OFF          VALUE '0'.
               88  KORTIN-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KORTIN-READ-OFF         VALUE '0'.
               88  KORTIN-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KORTIN-PROCESS-OFF      VALUE '0'.
               88  KORTIN-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FNRIN-EOF-OFF           VALUE '0'.
               88  FNRIN-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FNRIN-READ-OFF          VALUE '0'.
               88  FNRIN-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FNRIN-PROCESS-OFF       VALUE '0'.
               88  FNRIN-PROCESS           VALUE '1'.
           05  MELDING-IO-AREA.
               10  MELDING-IO-AREA-X       PICTURE X(40).
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
           05  KORTIN-DATA-FIELDS.
               10  PERIOD-IO.
                   15  PERIOD              PICTURE S9(4).
               10  INKNR-IO.
                   15  INKNR               PICTURE S9(2).
               10  FAKONR                  PICTURE X(2).
               10  FFAONR                  PICTURE X(2).
               10  SRDTO-IO.
                   15  SRDTO               PICTURE S9(6).
               10  SRDAG                   PICTURE X(2).
               10  FMNDKJ                  PICTURE X(3).
               10  SAMLEF                  PICTURE X(3).
               10  FNRMND                  PICTURE X(1).
               10  FAKGRP                  PICTURE X(1).
               10  IKKEF1                  PICTURE X(3).
               10  IKKEF2                  PICTURE X(3).
               10  IKKEF3                  PICTURE X(3).
               10  SFIMND                  PICTURE X(3).
               10  OPID                    PICTURE X(9).
               10  FIRMNR                  PICTURE X(3).
               10  PFFNR-IO.
                   15  PFFNR               PICTURE S9(6).
               10  PFKNR-IO.
                   15  PFKNR               PICTURE S9(6).
               10  SLETTE                  PICTURE X(1).
           05  KORTIN-MP                   PICTURE X(3).
           05  KORTIN-MC                   PICTURE X(3).
           05  KORTIN-M-02             REDEFINES KORTIN-MC.
               10  KORTIN-M-02-M1.
                   15  KORTIN-M-02-M1-FIRMNR-G.
                       20  KORTIN-M-02-M1-FIRMNR PICTURE X(3).
           05  FNRIN-DATA-FIELDS.
               10  SFNR-IO.
                   15  SFNR                PICTURE S9(6).
               10  SKNR-IO.
                   15  SKNR                PICTURE S9(6).
               10  GMLFOM-IO.
                   15  GMLFOM              PICTURE S9(4).
               10  FFAMND-IO.
                   15  FFAMND              PICTURE S9(6).
               10  FKRMND-IO.
                   15  FKRMND              PICTURE S9(6).
      **   KONTROLL AV PARAMETERKORTET.           **
           05  FNRIN-MP                    PICTURE X(3).
           05  FNRIN-MC                    PICTURE X(3).
           05  FNRIN-M-03              REDEFINES FNRIN-MC.
               10  FNRIN-M-03-M1.
                   15  FNRIN-M-03-M1-FIRMNR-G.
                       20  FNRIN-M-03-M1-FIRMNR PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  MELD1                   PICTURE X(33).
               10  MELD1A                  PICTURE X(12).
               10  MELD1B                  PICTURE X(28).
               10  MELD1C                  PICTURE X(16).
               10  PERAAR-IO.
                   15  PERAAR              PICTURE S9(2).
               10  AAR100                  PICTURE X(2).
               10  REST-IO.
                   15  REST                PICTURE SV9(1).
               10  PERPER-IO.
                   15  PERPER              PICTURE S9(2).
               10  DFO-IO.
                   15  DFO                 PICTURE S9(2).
               10  FFO-IO.
                   15  FFO                 PICTURE S9(2).
               10  NYFOM-IO.
                   15  NYFOM               PICTURE S9(4).
               10  PERNUM-IO.
                   15  PERNUM              PICTURE S9(4).
               10  FUPER-IO.
                   15  FUPER               PICTURE S9(6).
               10  FFU15-IO.
                   15  FFU15               PICTURE S9(6).
               10  FFU30-IO.
                   15  FFU30               PICTURE S9(6).
               10  FFU45-IO.
                   15  FFU45               PICTURE S9(6).
               10  FFU60-IO.
                   15  FFU60               PICTURE S9(6).
               10  FFU75-IO.
                   15  FFU75               PICTURE S9(6).
               10  FFU90-IO.
                   15  FFU90               PICTURE S9(6).
               10  FFU105-IO.
                   15  FFU105              PICTURE S9(6).
               10  FFU120-IO.
                   15  FFU120              PICTURE S9(6).
               10  FFU150-IO.
                   15  FFU150              PICTURE S9(6).
               10  FFU180-IO.
                   15  FFU180              PICTURE S9(6).
               10  FPER-IO.
                   15  FPER                PICTURE S9(6).
               10  FFD15-IO.
                   15  FFD15               PICTURE S9(6).
               10  FFD30-IO.
                   15  FFD30               PICTURE S9(6).
               10  FFD45-IO.
                   15  FFD45               PICTURE S9(6).
               10  FFD60-IO.
                   15  FFD60               PICTURE S9(6).
               10  FFD75-IO.
                   15  FFD75               PICTURE S9(6).
               10  FFD90-IO.
                   15  FFD90               PICTURE S9(6).
               10  FFD105-IO.
                   15  FFD105              PICTURE S9(6).
               10  FFD120-IO.
                   15  FFD120              PICTURE S9(6).
               10  FFD150-IO.
                   15  FFD150              PICTURE S9(6).
               10  FFD180-IO.
                   15  FFD180              PICTURE S9(6).
               10  PERMND-IO.
                   15  PERMND              PICTURE S9(2).
               10  PSFNR-IO.
                   15  PSFNR               PICTURE S9(6).
               10  PSKNR-IO.
                   15  PSKNR               PICTURE S9(6).
               10  FOMDIF-IO.
                   15  FOMDIF              PICTURE S9(4).
               10  FFNR-IO.
                   15  FFNR                PICTURE S9(6).
               10  FKNR-IO.
                   15  FKNR                PICTURE S9(6).
               10  PERN2-IO.
                   15  PERN2               PICTURE S9(2).
               10  PERN3-IO.
                   15  PERN3               PICTURE S9(3).
           05  EDITTING-FIELDS.
               10  EDIT-PERIOD             PICTURE ZZ.ZZ.
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
 
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
           SET IN-DETAIL-OUTPUT            TO TRUE
           PERFORM DETAIL-OUTPUT
           SET NOT-IN-DETAIL-OUTPUT        TO TRUE
           IF  NOT-SET-I-OF
               SET NOT-I-OF                TO TRUE
           END-IF
           SET NOT-SET-I-OF                TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-61                    TO TRUE
           SET NOT-I-62                    TO TRUE
           SET NOT-I-63                    TO TRUE
           SET NOT-I-64                    TO TRUE
           SET NOT-I-65                    TO TRUE
           SET NOT-I-66                    TO TRUE
           SET NOT-I-67                    TO TRUE
           SET NOT-I-68                    TO TRUE
           SET NOT-I-69                    TO TRUE
           SET NOT-I-70                    TO TRUE
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  KORTIN-PROCESS
               SET KORTIN-PROCESS-OFF      TO TRUE
               SET KORTIN-READ             TO TRUE
           END-IF
 
           IF  KORTIN-READ
               PERFORM KORTIN-GET
               SET KORTIN-READ-OFF         TO TRUE
               IF  NOT KORTIN-EOF
                   PERFORM KORTIN-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM KORTIN-MATCH-SET
               END-IF
           END-IF
 
           IF  FNRIN-PROCESS
               SET FNRIN-PROCESS-OFF       TO TRUE
               SET FNRIN-READ              TO TRUE
           END-IF
 
           IF  FNRIN-READ
               PERFORM FNRIN-GET
               SET FNRIN-READ-OFF          TO TRUE
               IF  NOT FNRIN-EOF
                   PERFORM FNRIN-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM FNRIN-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  KORTIN-PROCESS
               PERFORM KORTIN-IDSET
           END-IF
 
           IF  FNRIN-PROCESS
               PERFORM FNRIN-IDSET
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
           PERFORM DETAIL-OVERFLOW
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  KORTIN-PROCESS
               PERFORM KORTIN-FLDOFF
               PERFORM KORTIN-FLDSET
           END-IF
 
           IF  FNRIN-PROCESS
               PERFORM FNRIN-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           SET NOT-I-1ST                   TO TRUE
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-02)
               MOVE 'SKAL '                TO MELD1 (1:5)
               MOVE 'FIRMANR.'             TO MELD1A (1:8)
               MOVE FIRMNR                 TO MELD1A (10:3)
               MOVE MELD1A                 TO MELD1B (1:12)
               MOVE 'KORRIGER'             TO MELD1C (1:8)
               MOVE 'ES ?    '             TO MELD1C (9:8)
               MOVE MELD1C                 TO MELD1B (13:16)
               MOVE MELD1B                 TO MELD1 (6:28)
               DISPLAY MELD1
               ACCEPT MELD1
           END-IF
           IF  (NOT-I-01)
               GO TO PKEND-T
           END-IF
           MOVE PERIOD (1:2)               TO PERAAR
           SET NOT-I-80                    TO TRUE
           IF  PERAAR NOT < 80
               SET I-80                    TO TRUE
           END-IF
           IF  (I-80)
               MOVE '19'                   TO AAR100
           END-IF
           IF  (NOT-I-80)
               MOVE '20'                   TO AAR100
      *
      *  DERSOM KJØRING PR 8. ELLER 23. ØKES PERIODE MED EN FOR
      *  FIRMA"ER UTEN UKENTLIG PERIODISERING.
      *  UKENTLIG = INKNR, HALVMÅNEDLIG = PERPER.
      *
           END-IF
           DIVIDE INKNR BY 2           GIVING REST
           ADD REST TO INKNR           GIVING PERPER ROUNDED
      *
           SET NOT-I-11                    TO TRUE
           IF  FMNDKJ = 'JA '
               SET I-11                    TO TRUE
           END-IF
           IF  (NOT-I-11)
               SET NOT-I-11                TO TRUE
               IF  FMNDKJ = ' JA'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           SET NOT-I-12                    TO TRUE
           IF  FMNDKJ = 'NEI'
               SET I-12                    TO TRUE
           END-IF
           IF  (NOT-I-11 AND NOT-I-12)
               SET I-13                    TO TRUE
           END-IF
           SET NOT-I-14                    TO TRUE
           IF  SAMLEF = 'JA '
               SET I-14                    TO TRUE
           END-IF
           IF  (NOT-I-14)
               SET NOT-I-14                TO TRUE
               IF  SAMLEF = ' JA'
                   SET I-14                TO TRUE
               END-IF
           END-IF
           SET NOT-I-15                    TO TRUE
           IF  SAMLEF = 'NEI'
               SET I-15                    TO TRUE
           END-IF
           IF  (NOT-I-14 AND NOT-I-15)
               SET I-16                    TO TRUE
           END-IF
           SET NOT-I-51                    TO TRUE
           IF  SFIMND = 'JA '
               SET I-51                    TO TRUE
           END-IF
           IF  (NOT-I-51)
               SET NOT-I-51                TO TRUE
               IF  SFIMND = ' JA'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           SET NOT-I-52                    TO TRUE
           IF  SFIMND = 'NEI'
               SET I-52                    TO TRUE
           END-IF
           IF  (NOT-I-51 AND NOT-I-52)
               SET I-53                    TO TRUE
           END-IF
           SET NOT-I-54                    TO TRUE
           IF  SRDAG > '27 '
               SET I-54                    TO TRUE
           END-IF
           IF  (I-52 AND I-54)
               SET I-55                    TO TRUE
           END-IF
           SET NOT-I-56                    TO TRUE
           IF  SRDAG = '15 '
               SET I-56                    TO TRUE
           END-IF
           SET NOT-I-21                    TO TRUE
           IF  FAKONR = '01'
               SET I-21                    TO TRUE
           END-IF
           SET NOT-I-23                    TO TRUE
           IF  FNRMND < '1'
               SET I-23                    TO TRUE
           END-IF
           IF  (NOT-I-23)
               SET NOT-I-23                TO TRUE
               SET NOT-I-38                TO TRUE
               IF  FNRMND > '4'
                   SET I-23                TO TRUE
               END-IF
               IF  FNRMND = '4'
                   SET I-38                TO TRUE
               END-IF
           END-IF
           SET NOT-I-40                    TO TRUE
           IF  FAKGRP = '0'
               SET I-40                    TO TRUE
           END-IF
           SET NOT-I-41                    TO TRUE
           IF  FAKGRP = '1'
               SET I-41                    TO TRUE
           END-IF
           SET NOT-I-42                    TO TRUE
           IF  FAKGRP = '2'
               SET I-42                    TO TRUE
           END-IF
           IF  (NOT-I-40 AND NOT-I-41 AND NOT-I-42)
               SET I-25                    TO TRUE
           END-IF
           MOVE FAKONR                     TO DFO-IO
           MOVE FFAONR                     TO FFO-IO
           IF  (NOT-I-21)
               SUBTRACT 1                  FROM DFO
               SET NOT-I-28                TO TRUE
               IF  DFO NOT = FFO
                   SET I-28                TO TRUE
               END-IF
           END-IF
           IF  (I-21)
               SET NOT-I-28                TO TRUE
               IF  DFO NOT < FFO
                   SET I-28                TO TRUE
               END-IF
      *
      ******************************************************
           END-IF
           .
 
       PKEND-T.
      *
      *  FINNE PERIODE-DATO"ER UKENTLIG.
      *
           IF  (NOT-I-01)
               GO TO PFEND-T
           END-IF
           MOVE PERIOD                     TO NYFOM-IO
           MOVE FAKONR                     TO NYFOM-IO (3:2)
           MOVE PERIOD                     TO PERNUM-IO
           PERFORM PERRUT-S
           IF  (I-93)
               MOVE TABDTO(TABDTO-I)       TO FUPER-IO
           END-IF
           IF  (NOT-I-93)
               SET I-10                    TO TRUE
           END-IF
           ADD 2                           TO PERNUM
           PERFORM PERRUT-S
           IF  (I-93)
               MOVE TABDTO(TABDTO-I)       TO FFU15-IO
           END-IF
           IF  (NOT-I-93)
               SET I-24                    TO TRUE
           END-IF
           ADD 2                           TO PERNUM
           PERFORM PERRUT-S
           IF  (I-93)
               MOVE TABDTO(TABDTO-I)       TO FFU30-IO
           END-IF
           IF  (NOT-I-93)
               SET I-17                    TO TRUE
           END-IF
           ADD 2                           TO PERNUM
           PERFORM PERRUT-S
           IF  (I-93)
               MOVE TABDTO(TABDTO-I)       TO FFU45-IO
           END-IF
           IF  (NOT-I-93)
               SET I-18                    TO TRUE
           END-IF
           ADD 2                           TO PERNUM
           PERFORM PERRUT-S
           IF  (I-93)
               MOVE TABDTO(TABDTO-I)       TO FFU60-IO
           END-IF
           IF  (NOT-I-93)
               SET I-19                    TO TRUE
           END-IF
           ADD 2                           TO PERNUM
           PERFORM PERRUT-S
           IF  (I-93)
               MOVE TABDTO(TABDTO-I)       TO FFU75-IO
           END-IF
           IF  (NOT-I-93)
               SET I-27                    TO TRUE
           END-IF
           ADD 2                           TO PERNUM
           PERFORM PERRUT-S
           IF  (I-93)
               MOVE TABDTO(TABDTO-I)       TO FFU90-IO
           END-IF
           IF  (NOT-I-93)
               SET I-20                    TO TRUE
           END-IF
           ADD 2                           TO PERNUM
           PERFORM PERRUT-S
           IF  (I-93)
               MOVE TABDTO(TABDTO-I)       TO FFU105-IO
           END-IF
           IF  (NOT-I-93)
               SET I-34                    TO TRUE
           END-IF
           ADD 2                           TO PERNUM
           PERFORM PERRUT-S
           IF  (I-93)
               MOVE TABDTO(TABDTO-I)       TO FFU120-IO
           END-IF
           IF  (NOT-I-93)
               SET I-35                    TO TRUE
           END-IF
           ADD 4                           TO PERNUM
           PERFORM PERRUT-S
           IF  (I-93)
               MOVE TABDTO(TABDTO-I)       TO FFU150-IO
           END-IF
           IF  (NOT-I-93)
               SET I-36                    TO TRUE
           END-IF
           ADD 4                           TO PERNUM
           PERFORM PERRUT-S
           IF  (I-93)
               MOVE TABDTO(TABDTO-I)       TO FFU180-IO
           END-IF
           IF  (NOT-I-93)
               SET I-37                    TO TRUE
      *
      *  FINNE PERIODE-DATO"ER HALVMÅNEDLIG.
      *
           END-IF
           MOVE PERIOD                     TO PERNUM-IO
           MOVE PERPER                     TO PERNUM-IO (3:2)
           PERFORM PERRUT-S
           IF  (I-93)
               MOVE TABDTO(TABDTO-I)       TO FPER-IO
           END-IF
           IF  (NOT-I-93)
               SET I-10                    TO TRUE
           END-IF
           ADD 2                           TO PERNUM
           PERFORM PERRUT-S
           IF  (I-93)
               MOVE TABDTO(TABDTO-I)       TO FFD15-IO
           END-IF
           IF  (NOT-I-93)
               SET I-24                    TO TRUE
           END-IF
           ADD 2                           TO PERNUM
           PERFORM PERRUT-S
           IF  (I-93)
               MOVE TABDTO(TABDTO-I)       TO FFD30-IO
           END-IF
           IF  (NOT-I-93)
               SET I-17                    TO TRUE
           END-IF
           ADD 2                           TO PERNUM
           PERFORM PERRUT-S
           IF  (I-93)
               MOVE TABDTO(TABDTO-I)       TO FFD45-IO
           END-IF
           IF  (NOT-I-93)
               SET I-18                    TO TRUE
           END-IF
           ADD 2                           TO PERNUM
           PERFORM PERRUT-S
           IF  (I-93)
               MOVE TABDTO(TABDTO-I)       TO FFD60-IO
           END-IF
           IF  (NOT-I-93)
               SET I-19                    TO TRUE
           END-IF
           ADD 2                           TO PERNUM
           PERFORM PERRUT-S
           IF  (I-93)
               MOVE TABDTO(TABDTO-I)       TO FFD75-IO
           END-IF
           IF  (NOT-I-93)
               SET I-27                    TO TRUE
           END-IF
           ADD 2                           TO PERNUM
           PERFORM PERRUT-S
           IF  (I-93)
               MOVE TABDTO(TABDTO-I)       TO FFD90-IO
           END-IF
           IF  (NOT-I-93)
               SET I-20                    TO TRUE
           END-IF
           ADD 2                           TO PERNUM
           PERFORM PERRUT-S
           IF  (I-93)
               MOVE TABDTO(TABDTO-I)       TO FFD105-IO
           END-IF
           IF  (NOT-I-93)
               SET I-34                    TO TRUE
           END-IF
           ADD 2                           TO PERNUM
           PERFORM PERRUT-S
           IF  (I-93)
               MOVE TABDTO(TABDTO-I)       TO FFD120-IO
           END-IF
           IF  (NOT-I-93)
               SET I-35                    TO TRUE
           END-IF
           ADD 4                           TO PERNUM
           PERFORM PERRUT-S
           IF  (I-93)
               MOVE TABDTO(TABDTO-I)       TO FFD150-IO
           END-IF
           IF  (NOT-I-93)
               SET I-36                    TO TRUE
           END-IF
           ADD 4                           TO PERNUM
           PERFORM PERRUT-S
           IF  (I-93)
               MOVE TABDTO(TABDTO-I)       TO FFD180-IO
           END-IF
           IF  (NOT-I-93)
               SET I-37                    TO TRUE
           END-IF.
 
       PFEND-T.
      *   HENTE  FAKTURAMND. OG NAVN
           IF  (NOT-I-01)
               GO TO PMEND-T
           END-IF
           ADD 3 TO PERPER             GIVING PERMND
           DIVIDE PERMND BY 4          GIVING PERMND
           SET NOT-I-22                    TO TRUE
           SET TABMNR-S                    TO TABMNR-I
           PERFORM WITH TEST AFTER
                   VARYING TABMNR-I FROM 1 BY 1
                     UNTIL TABMNR-I >= TABMNR-MAX
                        OR I-22
               IF  PERMND = TABMNR (TABMNR-I)
                   SET I-22                TO TRUE
                   SET TABMNR-S            TO TABMNR-I
               END-IF
           END-PERFORM
           SET TABMNR-I                    TO TABMNR-S
           IF  I-22
           AND TABMNR-I NOT > TABMND-MAX
               SET TABMND-I                TO TABMNR-I
           END-IF.
 
       PMEND-T.
      *    RUTINE FOR OPPDATERING AV FAKTURA/KREDITNOTANR.
           IF  (NOT-I-02)
               GO TO FNYEND-T
           END-IF
           SET NOT-I-33                    TO TRUE
           IF  SLETTE = 'S'
               SET I-33                    TO TRUE
           END-IF
           SUBTRACT 1 FROM PFFNR       GIVING PSFNR
           SUBTRACT 1 FROM PFKNR       GIVING PSKNR.
 
       FNYEND-T.
           IF  (NOT-I-03)
               GO TO FNREND-T
           END-IF
           SUBTRACT GMLFOM FROM NYFOM  GIVING FOMDIF
           SET NOT-I-31                    TO TRUE
           IF  FOMDIF = 1
               SET I-31                    TO TRUE
           END-IF
           IF  (NOT-I-31 AND NOT-I-21)
               SET I-32                    TO TRUE
           END-IF
           SET NOT-I-77                    TO TRUE
           SET NOT-I-79                    TO TRUE
           ADD 1 TO SFNR               GIVING FFNR
           ADD 1 TO SKNR               GIVING FKNR
           SET NOT-I-79                    TO TRUE
           IF  FKNR NOT < 959700
               SET I-79                    TO TRUE
           END-IF
           SET NOT-I-77                    TO TRUE
           IF  FFNR NOT < 895000
               SET I-77                    TO TRUE
           END-IF.
 
       FNREND-T.
      *    SUBRUTINE TABELLOPPSLAG PERIODETABELL.
           CONTINUE.
 
       PERRUT-S SECTION.
       PERRUT-S-P.
           MOVE PERNUM (3:2)               TO PERN2-IO
           SUBTRACT 48 FROM PERN2      GIVING PERN3
           SET NOT-I-91                    TO TRUE
           SET NOT-I-92                    TO TRUE
           IF  PERN3 > 0
               SET I-91                    TO TRUE
           END-IF
           IF  PERN3 = 0
               SET I-92                    TO TRUE
           END-IF
           IF  (I-91 AND NOT-I-92)
               ADD 100                     TO PERN3
               SUBTRACT PERN2              FROM PERNUM
               ADD PERN3                   TO PERNUM
           END-IF
           SET NOT-I-93                    TO TRUE
           SET TABPER-S                    TO TABPER-I
           PERFORM WITH TEST AFTER
                   VARYING TABPER-I FROM 1 BY 1
                     UNTIL TABPER-I >= TABPER-MAX
                        OR I-93
               IF  PERNUM = TABPER (TABPER-I)
                   SET I-93                TO TRUE
                   SET TABPER-S            TO TABPER-I
               END-IF
           END-PERFORM
           SET TABPER-I                    TO TABPER-S
           IF  I-93
           AND TABPER-I NOT > TABDTO-MAX
               SET TABDTO-I                TO TABPER-I
           END-IF.
 
       KORTIN-GET SECTION.
       KORTIN-GET-P.
           IF  KORTIN-EOF-OFF
               READ KORTIN
               AT END
                   SET KORTIN-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KORTIN-FLDOFF SECTION.
       KORTIN-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = '9' )
               SET NOT-I-26                TO TRUE
           END-EVALUATE.
 
       KORTIN-FLDSET SECTION.
       KORTIN-FLDSET-P.
           EVALUATE TRUE
           WHEN ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = '1' )
               MOVE KORTIN-IO-AREA (60:4)  TO PERIOD-IO
               INSPECT PERIOD-IO REPLACING ALL ' ' BY '0'
               MOVE KORTIN-IO-AREA (62:2)  TO INKNR-IO
               INSPECT INKNR-IO REPLACING ALL ' ' BY '0'
           WHEN ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = '2' )
               MOVE KORTIN-IO-AREA (60:2)  TO FAKONR (1:2)
           WHEN ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = '3' )
               MOVE KORTIN-IO-AREA (60:2)  TO FFAONR (1:2)
           WHEN ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = '4' )
               MOVE KORTIN-IO-AREA (60:6)  TO SRDTO-IO
               INSPECT SRDTO-IO REPLACING ALL ' ' BY '0'
               MOVE KORTIN-IO-AREA (60:2)  TO SRDAG (1:2)
           WHEN ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = '5' )
               MOVE KORTIN-IO-AREA (60:3)  TO FMNDKJ (1:3)
           WHEN ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = '6' )
               MOVE KORTIN-IO-AREA (60:3)  TO SAMLEF (1:3)
           WHEN ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = '7' )
               MOVE KORTIN-IO-AREA (60:1)  TO FNRMND (1:1)
           WHEN ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = '8' )
               MOVE KORTIN-IO-AREA (60:1)  TO FAKGRP (1:1)
           WHEN ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = '9' )
               MOVE KORTIN-IO-AREA (60:3)  TO IKKEF1 (1:3)
               IF  IKKEF1 = SPACES
                   SET I-26                TO TRUE
               END-IF
               MOVE KORTIN-IO-AREA (64:3)  TO IKKEF2 (1:3)
               MOVE KORTIN-IO-AREA (68:3)  TO IKKEF3 (1:3)
           WHEN ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = 'A' )
               MOVE KORTIN-IO-AREA (60:3)  TO SFIMND (1:3)
           WHEN ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = 'X' )
               MOVE KORTIN-IO-AREA (60:9)  TO OPID (1:9)
           WHEN ( KORTIN-IO-AREA (1:1) = '9'
            AND   KORTIN-IO-AREA (2:1) = '1' )
               MOVE KORTIN-IO-AREA (3:3)   TO FIRMNR (1:3)
               MOVE KORTIN-IO-AREA (6:6)   TO PFFNR-IO
               INSPECT PFFNR-IO REPLACING ALL ' ' BY '0'
               MOVE KORTIN-IO-AREA (12:6)  TO PFKNR-IO
               INSPECT PFKNR-IO REPLACING ALL ' ' BY '0'
               MOVE KORTIN-IO-AREA (19:1)  TO SLETTE (1:1)
           END-EVALUATE.
 
       KORTIN-IDCHK SECTION.
       KORTIN-IDCHK-P.
           EVALUATE TRUE
           WHEN ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = '1' )
             OR ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = '2' )
             OR ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = '3' )
             OR ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = '4' )
             OR ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = '5' )
             OR ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = '6' )
             OR ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = '7' )
             OR ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = '8' )
             OR ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = '9' )
             OR ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = 'A' )
             OR ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = 'X' )
             OR ( KORTIN-IO-AREA (1:1) = '9'
            AND   KORTIN-IO-AREA (2:1) = '1' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       KORTIN-IDSET SECTION.
       KORTIN-IDSET-P.
           EVALUATE TRUE
           WHEN ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = '1' )
               SET I-61                    TO TRUE
           WHEN ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = '2' )
               SET I-62                    TO TRUE
           WHEN ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = '3' )
               SET I-63                    TO TRUE
           WHEN ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = '4' )
               SET I-64                    TO TRUE
           WHEN ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = '5' )
               SET I-65                    TO TRUE
           WHEN ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = '6' )
               SET I-66                    TO TRUE
           WHEN ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = '7' )
               SET I-67                    TO TRUE
           WHEN ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = '8' )
               SET I-68                    TO TRUE
           WHEN ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = '9' )
               SET I-69                    TO TRUE
           WHEN ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = 'A' )
               SET I-70                    TO TRUE
           WHEN ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = 'X' )
               SET I-01                    TO TRUE
           WHEN ( KORTIN-IO-AREA (1:1) = '9'
            AND   KORTIN-IO-AREA (2:1) = '1' )
               SET I-02                    TO TRUE
           END-EVALUATE.
 
       KORTIN-MATCH-SET SECTION.
       KORTIN-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = '1' )
               SET NOT-CALL-MATCH-RECS     TO TRUE
           WHEN ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = '2' )
               SET NOT-CALL-MATCH-RECS     TO TRUE
           WHEN ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = '3' )
               SET NOT-CALL-MATCH-RECS     TO TRUE
           WHEN ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = '4' )
               SET NOT-CALL-MATCH-RECS     TO TRUE
           WHEN ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = '5' )
               SET NOT-CALL-MATCH-RECS     TO TRUE
           WHEN ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = '6' )
               SET NOT-CALL-MATCH-RECS     TO TRUE
           WHEN ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = '7' )
               SET NOT-CALL-MATCH-RECS     TO TRUE
           WHEN ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = '8' )
               SET NOT-CALL-MATCH-RECS     TO TRUE
           WHEN ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = '9' )
               SET NOT-CALL-MATCH-RECS     TO TRUE
           WHEN ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = 'A' )
               SET NOT-CALL-MATCH-RECS     TO TRUE
           WHEN ( KORTIN-IO-AREA (1:1) = 'P'
            AND   KORTIN-IO-AREA (6:1) = 'X' )
               SET NOT-CALL-MATCH-RECS     TO TRUE
           WHEN ( KORTIN-IO-AREA (1:1) = '9'
            AND   KORTIN-IO-AREA (2:1) = '1' )
               MOVE KORTIN-IO-AREA (3:3)   TO KORTIN-M-02-M1-FIRMNR
           END-EVALUATE.
 
       FNRIN-GET SECTION.
       FNRIN-GET-P.
           IF  FNRIN-EOF-OFF
               READ FNRIN
               AT END
                   SET FNRIN-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FNRIN-FLDSET SECTION.
       FNRIN-FLDSET-P.
           EVALUATE TRUE
           WHEN ( FNRIN-IO-AREA (1:1) = '9'
            AND   FNRIN-IO-AREA (2:1) = '1' )
               MOVE FNRIN-IO-AREA (3:3)    TO FIRMNR (1:3)
               MOVE FNRIN-IO-AREA (18:6)   TO SFNR-IO
               INSPECT SFNR-IO REPLACING ALL ' ' BY '0'
               MOVE FNRIN-IO-AREA (24:6)   TO SKNR-IO
               INSPECT SKNR-IO REPLACING ALL ' ' BY '0'
               MOVE FNRIN-IO-AREA (30:4)   TO GMLFOM-IO
               INSPECT GMLFOM-IO REPLACING ALL ' ' BY '0'
               MOVE FNRIN-IO-AREA (39:6)   TO FFAMND-IO
               INSPECT FFAMND-IO REPLACING ALL ' ' BY '0'
               MOVE FNRIN-IO-AREA (45:6)   TO FKRMND-IO
               INSPECT FKRMND-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       FNRIN-IDCHK SECTION.
       FNRIN-IDCHK-P.
           EVALUATE TRUE
           WHEN ( FNRIN-IO-AREA (1:1) = '9'
            AND   FNRIN-IO-AREA (2:1) = '1' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       FNRIN-IDSET SECTION.
       FNRIN-IDSET-P.
           EVALUATE TRUE
           WHEN ( FNRIN-IO-AREA (1:1) = '9'
            AND   FNRIN-IO-AREA (2:1) = '1' )
               SET I-03                    TO TRUE
           END-EVALUATE.
 
       FNRIN-MATCH-SET SECTION.
       FNRIN-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( FNRIN-IO-AREA (1:1) = '9'
            AND   FNRIN-IO-AREA (2:1) = '1' )
               MOVE FNRIN-IO-AREA (3:3)    TO FNRIN-M-03-M1-FIRMNR
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
           IF  KORTIN-EOF
               MOVE HIGH-VALUES            TO KORTIN-MC
                                              KORTIN-MP
           END-IF
           IF  FNRIN-EOF
               MOVE HIGH-VALUES            TO FNRIN-MC
                                              FNRIN-MP
           END-IF
           IF  KORTIN-MC < KORTIN-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  FNRIN-MC < FNRIN-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  KORTIN-MC < FNRIN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET KORTIN-PROCESS      TO TRUE
                   MOVE KORTIN-MC          TO KORTIN-MP
                   IF  KORTIN-MC = FNRIN-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  FNRIN-MC < KORTIN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FNRIN-PROCESS       TO TRUE
                   MOVE FNRIN-MC           TO FNRIN-MP
                   IF  FNRIN-MC = KORTIN-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  KORTIN-MC = FNRIN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET KORTIN-PROCESS      TO TRUE
                   MOVE KORTIN-MC          TO KORTIN-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       KORTAB-LOAD SECTION.
       KORTAB-LOAD-P.
           OPEN INPUT KORTAB
           SET TABPER-I                    TO 1
           PERFORM UNTIL KORTAB-EOF
               READ KORTAB
               AT END
                   SET KORTAB-EOF          TO TRUE
               NOT AT END
                   MOVE KORTAB-IO-AREA (1:10) TO TABPER-ENTRY
                                                            (TABPER-I)
                   SET TABPER-I            UP BY 1
                   MOVE KORTAB-IO-AREA (11:10) TO TABPER-ENTRY
                                                            (TABPER-I)
                   SET TABPER-I            UP BY 1
                   MOVE KORTAB-IO-AREA (21:10) TO TABPER-ENTRY
                                                            (TABPER-I)
                   SET TABPER-I            UP BY 1
                   MOVE KORTAB-IO-AREA (31:10) TO TABPER-ENTRY
                                                            (TABPER-I)
                   SET TABPER-I            UP BY 1
                   MOVE KORTAB-IO-AREA (41:10) TO TABPER-ENTRY
                                                            (TABPER-I)
                   SET TABPER-I            UP BY 1
                   MOVE KORTAB-IO-AREA (51:10) TO TABPER-ENTRY
                                                            (TABPER-I)
                   SET TABPER-I            UP BY 1
                   MOVE KORTAB-IO-AREA (61:10) TO TABPER-ENTRY
                                                            (TABPER-I)
                   SET TABPER-I            UP BY 1
                   MOVE KORTAB-IO-AREA (71:10) TO TABPER-ENTRY
                                                            (TABPER-I)
                   SET TABPER-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE KORTAB.
 
       MNDTAB-LOAD SECTION.
       MNDTAB-LOAD-P.
           OPEN INPUT MNDTAB
           SET TABMNR-I                    TO 1
           PERFORM UNTIL MNDTAB-EOF
               READ MNDTAB
               AT END
                   SET MNDTAB-EOF          TO TRUE
               NOT AT END
                   MOVE MNDTAB-IO-AREA (1:11) TO TABMNR-ENTRY
                                                            (TABMNR-I)
                   SET TABMNR-I            UP BY 1
                   MOVE MNDTAB-IO-AREA (12:11) TO TABMNR-ENTRY
                                                            (TABMNR-I)
                   SET TABMNR-I            UP BY 1
                   MOVE MNDTAB-IO-AREA (23:11) TO TABMNR-ENTRY
                                                            (TABMNR-I)
                   SET TABMNR-I            UP BY 1
                   MOVE MNDTAB-IO-AREA (34:11) TO TABMNR-ENTRY
                                                            (TABMNR-I)
                   SET TABMNR-I            UP BY 1
                   MOVE MNDTAB-IO-AREA (45:11) TO TABMNR-ENTRY
                                                            (TABMNR-I)
                   SET TABMNR-I            UP BY 1
                   MOVE MNDTAB-IO-AREA (56:11) TO TABMNR-ENTRY
                                                            (TABMNR-I)
                   SET TABMNR-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE MNDTAB.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'PARAMETERE FAKTURARUTINE' TO LISTE-IO-AREA (11:24)
               MOVE 'ER KORRIGERT'         TO LISTE-IO-AREA (38:12)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (52:8)
               MOVE 'AV'                   TO LISTE-IO-AREA (62:2)
               MOVE OPID                   TO LISTE-IO-AREA (88:9)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'T E K S T'            TO LISTE-IO-AREA (7:9)
               MOVE 'K O D E'              TO LISTE-IO-AREA (27:7)
               MOVE 'A N M E R K N I N G E R' TO LISTE-IO-AREA (40:23)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FAKTURAGRUPPE'        TO LISTE-IO-AREA (1:13)
               MOVE FAKGRP                 TO LISTE-IO-AREA (27:1)
               IF  (I-25)
                   MOVE 'UGYLDIG KODE. ANTAR 0.' TO LISTE-IO-AREA
                                                               (39:22)
               END-IF
               IF  (I-40)
                   MOVE 'ALLE FIRMAER'     TO LISTE-IO-AREA (40:12)
               END-IF
               IF  (I-41)
                   MOVE 'KUN GRUPPE 1'     TO LISTE-IO-AREA (40:12)
               END-IF
               IF  (I-42)
                   MOVE 'KUN GRUPPE 2'     TO LISTE-IO-AREA (40:12)
               END-IF
               IF  (NOT-I-26)
                   MOVE '**  IKKE FIRMANUMMER =' TO LISTE-IO-AREA
                                                               (39:22)
               END-IF
               IF  (NOT-I-26)
                   MOVE IKKEF1             TO LISTE-IO-AREA (62:3)
               END-IF
               IF  (NOT-I-26)
                   MOVE IKKEF2             TO LISTE-IO-AREA (66:3)
               END-IF
               IF  (NOT-I-26)
                   MOVE IKKEF3             TO LISTE-IO-AREA (70:3)
      *       D  2     01 38 26
      *                                  13 "F E I L *****"
      *                                  40 "GOODYEAR FIRMA 921, SKAL"
      *                                  65 "IKKE FAKTURERE VED SISTE"
      *                                  90 "FAKTURAOMGANG PR. MND.  "
               END-IF
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'INKASSOPER. UKENTLIG:' TO LISTE-IO-AREA (1:21)
               MOVE PERIOD                 TO EDIT-PERIOD
               MOVE EDIT-PERIOD            TO LISTE-IO-AREA (23:5)
               MOVE 'INKASSOPER. 1/2 MND-LIG:' TO LISTE-IO-AREA (31:24)
               MOVE PERIOD                 TO EDIT-PERIOD
               MOVE EDIT-PERIOD            TO LISTE-IO-AREA (56:5)
               MOVE PERPER-IO              TO LISTE-IO-AREA (59:2)
               IF  (I-10)
                   MOVE 'FEIL I PARAMETER ELLER' TO LISTE-IO-AREA
                                                               (64:22)
               END-IF
               IF  (I-10)
                   MOVE 'TABELL'           TO LISTE-IO-AREA (87:6)
               END-IF
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FAKTURAOMGANGSNR. NÅ' TO LISTE-IO-AREA (1:20)
               MOVE FAKONR                 TO LISTE-IO-AREA (27:2)
               IF  (I-21)
                   MOVE 'FØRSTE KJØRING I ÅRET' TO LISTE-IO-AREA
                                                               (40:21)
               END-IF
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FAKTURAOMGANGSNR. SIST' TO LISTE-IO-AREA (1:22)
               MOVE FFAONR                 TO LISTE-IO-AREA (27:2)
               IF  (I-28)
                   MOVE 'FAKTURAOMGANGSNR.FEIL' TO LISTE-IO-AREA
                                                               (40:21)
               END-IF
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'SISTE REGISTRERINGSDATO' TO LISTE-IO-AREA (1:23)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'SOM SKAL FAKTURERES'  TO LISTE-IO-AREA (1:19)
               MOVE SRDTO                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (27:8)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FØRSTE KJØRING I MÅNEDEN' TO LISTE-IO-AREA (1:24)
               MOVE FMNDKJ                 TO LISTE-IO-AREA (27:3)
               IF  (I-13)
                   MOVE 'FEIL KODE.     ANTAR NEI' TO LISTE-IO-AREA
                                                               (41:24)
               END-IF
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'SKAL SAMLEFAKTURAER MED' TO LISTE-IO-AREA (1:23)
               MOVE SAMLEF                 TO LISTE-IO-AREA (27:3)
               IF  (I-56 AND NOT-I-14)
                   MOVE 'IKKE SAMLEFAKT. PR. 15? ' TO LISTE-IO-AREA
                                                               (41:24)
               END-IF
               IF  (I-55 AND NOT-I-14)
                   MOVE 'IKKE SAMLEFAKT. PR. 31? ' TO LISTE-IO-AREA
                                                               (41:24)
               END-IF
               IF  (I-16)
                   MOVE 'FEIL KODE.     ANTAR NEI' TO LISTE-IO-AREA
                                                               (41:24)
               END-IF
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ER DET SISTE FAKTURERING' TO LISTE-IO-AREA (1:24)
               MOVE 'FOR DENNE MND. '      TO LISTE-IO-AREA (25:15)
               MOVE SFIMND                 TO LISTE-IO-AREA (41:3)
               IF  (NOT-I-53)
                   MOVE 'VED JA, BLIR SAMLEFAKT. ' TO LISTE-IO-AREA
                                                               (47:24)
               END-IF
               IF  (NOT-I-53)
                   MOVE 'PR. MND. FAKTURERT.' TO LISTE-IO-AREA (72:19)
               END-IF
               IF  (I-53)
                   MOVE 'FEIL KODE.     ANTAR NEI' TO LISTE-IO-AREA
                                                               (47:24)
               END-IF
               IF  (I-55)
                   MOVE 'FEIL KODE. KJ.DAG > 27. ' TO LISTE-IO-AREA
                                                               (47:24)
               END-IF
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FAKTURANR I'          TO LISTE-IO-AREA (1:11)
               IF  (I-22)
                   MOVE TABMND (TABMND-I)  TO LISTE-IO-AREA (13:9)
               END-IF
               MOVE AAR100                 TO LISTE-IO-AREA (23:2)
               MOVE PERAAR-IO              TO LISTE-IO-AREA (25:2)
               MOVE FNRMND                 TO LISTE-IO-AREA (29:1)
               IF  (I-23)
                   MOVE 'FEIL NR.'         TO LISTE-IO-AREA (42:8)
               END-IF
               IF  (I-23)
                   MOVE 'RETT OPP OG KJØR OM' TO LISTE-IO-AREA (58:19)
               END-IF
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'UKENTLIG    1/2 MND-LIG' TO LISTE-IO-AREA (27:23)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '--------    -----------' TO LISTE-IO-AREA (27:23)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'PERIODEDATO'          TO LISTE-IO-AREA (1:11)
               MOVE FUPER                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (27:8)
               MOVE FPER                   TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (40:8)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FORFALL 15 DAGER'     TO LISTE-IO-AREA (1:16)
               MOVE FFU15                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (27:8)
               MOVE FFD15                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (40:8)
               IF  (I-24)
                   MOVE 'IKKE I TABELLEN.' TO LISTE-IO-AREA (52:16)
               END-IF
               IF  (I-24)
                   MOVE 'RETT TAB.TABFFALL OG KJØ' TO LISTE-IO-AREA
                                                               (70:24)
               END-IF
               IF  (I-24)
                   MOVE 'R OM'             TO LISTE-IO-AREA (94:4)
               END-IF
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FORFALL 30 DAGER'     TO LISTE-IO-AREA (1:16)
               MOVE FFU30                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (27:8)
               MOVE FFD30                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (40:8)
               IF  (I-17)
                   MOVE 'IKKE I TABELLEN.' TO LISTE-IO-AREA (52:16)
               END-IF
               IF  (I-17)
                   MOVE 'RETT TAB.TABFFALL OG KJØ' TO LISTE-IO-AREA
                                                               (70:24)
               END-IF
               IF  (I-17)
                   MOVE 'R OM'             TO LISTE-IO-AREA (94:4)
               END-IF
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FORFALL 45 DAGER'     TO LISTE-IO-AREA (1:16)
               MOVE FFU45                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (27:8)
               MOVE FFD45                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (40:8)
               IF  (I-18)
                   MOVE 'IKKE I TABELLEN.' TO LISTE-IO-AREA (52:16)
               END-IF
               IF  (I-18)
                   MOVE 'RETT TAB.TABFFALL OG KJØ' TO LISTE-IO-AREA
                                                               (70:24)
               END-IF
               IF  (I-18)
                   MOVE 'R OM'             TO LISTE-IO-AREA (94:4)
               END-IF
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FORFALL 60 DAGER'     TO LISTE-IO-AREA (1:16)
               MOVE FFU60                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (27:8)
               MOVE FFD60                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (40:8)
               IF  (I-19)
                   MOVE 'IKKE I TABELLEN.' TO LISTE-IO-AREA (52:16)
               END-IF
               IF  (I-19)
                   MOVE 'RETT TAB.TABFFALL OG KJØ' TO LISTE-IO-AREA
                                                               (70:24)
               END-IF
               IF  (I-19)
                   MOVE 'R OM'             TO LISTE-IO-AREA (94:4)
               END-IF
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FORFALL 75 DAGER'     TO LISTE-IO-AREA (1:16)
               MOVE FFU75                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (27:8)
               MOVE FFD75                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (40:8)
               IF  (I-27)
                   MOVE 'IKKE I TABELLEN.' TO LISTE-IO-AREA (52:16)
               END-IF
               IF  (I-27)
                   MOVE 'RETT TAB.TABFFALL OG KJØ' TO LISTE-IO-AREA
                                                               (70:24)
               END-IF
               IF  (I-27)
                   MOVE 'R OM'             TO LISTE-IO-AREA (94:4)
               END-IF
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FORFALL 90 DAGER'     TO LISTE-IO-AREA (1:16)
               MOVE FFU90                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (27:8)
               MOVE FFD90                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (40:8)
               IF  (I-20)
                   MOVE 'IKKE I TABELLEN.' TO LISTE-IO-AREA (52:16)
               END-IF
               IF  (I-20)
                   MOVE 'RETT TAB.TABFFALL OG KJØ' TO LISTE-IO-AREA
                                                               (70:24)
               END-IF
               IF  (I-20)
                   MOVE 'R OM'             TO LISTE-IO-AREA (94:4)
               END-IF
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FORFALL 105 DAGER'    TO LISTE-IO-AREA (1:17)
               MOVE FFU105                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (27:8)
               MOVE FFD105                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (40:8)
               IF  (I-34)
                   MOVE 'IKKE I TABELLEN.' TO LISTE-IO-AREA (52:16)
               END-IF
               IF  (I-34)
                   MOVE 'RETT TAB.TABFFALL OG KJØ' TO LISTE-IO-AREA
                                                               (70:24)
               END-IF
               IF  (I-34)
                   MOVE 'R OM'             TO LISTE-IO-AREA (94:4)
               END-IF
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FORFALL 120 DAGER'    TO LISTE-IO-AREA (1:17)
               MOVE FFU120                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (27:8)
               MOVE FFD120                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (40:8)
               IF  (I-35)
                   MOVE 'IKKE I TABELLEN.' TO LISTE-IO-AREA (52:16)
               END-IF
               IF  (I-35)
                   MOVE 'RETT TAB.TABFFALL OG KJØ' TO LISTE-IO-AREA
                                                               (70:24)
               END-IF
               IF  (I-35)
                   MOVE 'R OM'             TO LISTE-IO-AREA (94:4)
               END-IF
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FORFALL 150 DAGER'    TO LISTE-IO-AREA (1:17)
               MOVE FFU150                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (27:8)
               MOVE FFD150                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (40:8)
               IF  (I-36)
                   MOVE 'IKKE I TABELLEN.' TO LISTE-IO-AREA (52:16)
               END-IF
               IF  (I-36)
                   MOVE 'RETT TAB.TABFFALL OG KJØ' TO LISTE-IO-AREA
                                                               (70:24)
               END-IF
               IF  (I-36)
                   MOVE 'R OM'             TO LISTE-IO-AREA (94:4)
               END-IF
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FORFALL 180 DAGER'    TO LISTE-IO-AREA (1:17)
               MOVE FFU180                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (27:8)
               MOVE FFD180                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (40:8)
               IF  (I-37)
                   MOVE 'IKKE I TABELLEN.' TO LISTE-IO-AREA (52:16)
               END-IF
               IF  (I-37)
                   MOVE 'RETT TAB.TABFFALL OG KJØ' TO LISTE-IO-AREA
                                                               (70:24)
               END-IF
               IF  (I-37)
                   MOVE 'R OM'             TO LISTE-IO-AREA (94:4)
               END-IF
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-01)
               MOVE SPACES TO FAKPAR-IO-AREA
               INITIALIZE FAKPAR-IO-AREA
               MOVE '90'                   TO FAKPAR-IO-AREA (1:2)
               MOVE FNRMND                 TO FAKPAR-IO-AREA (3:1)
               MOVE '.'                    TO FAKPAR-IO-AREA (4:1)
               MOVE PERAAR-IO              TO FAKPAR-IO-AREA (6:2)
               MOVE PERMND-IO              TO FAKPAR-IO-AREA (8:2)
               MOVE FAKONR                 TO FAKPAR-IO-AREA (10:2)
               MOVE SRDTO-IO               TO FAKPAR-IO-AREA (12:6)
               MOVE FPER-IO                TO FAKPAR-IO-AREA (18:6)
               MOVE FFD15-IO               TO FAKPAR-IO-AREA (24:6)
               MOVE FFD30-IO               TO FAKPAR-IO-AREA (30:6)
               MOVE FFD45-IO               TO FAKPAR-IO-AREA (36:6)
               MOVE FFD60-IO               TO FAKPAR-IO-AREA (42:6)
               MOVE FFD90-IO               TO FAKPAR-IO-AREA (48:6)
               MOVE FFD75-IO               TO FAKPAR-IO-AREA (54:6)
               MOVE PERAAR-IO              TO FAKPAR-IO-AREA (64:2)
               MOVE PERPER-IO              TO FAKPAR-IO-AREA (63:2)
               MOVE '0'                    TO FAKPAR-IO-AREA (62:1)
               IF  (NOT-I-11)
                   MOVE 'N'                TO FAKPAR-IO-AREA (66:1)
               END-IF
               IF  (I-11)
                   MOVE 'J'                TO FAKPAR-IO-AREA (66:1)
               END-IF
               IF  (NOT-I-14)
                   MOVE 'N'                TO FAKPAR-IO-AREA (67:1)
               END-IF
               IF  (I-14)
                   MOVE 'J'                TO FAKPAR-IO-AREA (67:1)
               END-IF
               IF  (NOT-I-51)
                   MOVE 'N'                TO FAKPAR-IO-AREA (68:1)
               END-IF
               IF  (I-51)
                   MOVE 'J'                TO FAKPAR-IO-AREA (68:1)
               END-IF
               IF  (I-22)
                   MOVE TABMND (TABMND-I)  TO FAKPAR-IO-AREA (74:9)
               END-IF
               MOVE FFAONR                 TO FAKPAR-IO-AREA (89:2)
               IF  (NOT-I-25)
                   MOVE FAKGRP             TO FAKPAR-IO-AREA (91:1)
               END-IF
               IF  (I-25)
                   MOVE '0'                TO FAKPAR-IO-AREA (91:1)
               END-IF
               MOVE AAR100                 TO FAKPAR-IO-AREA (95:2)
               MOVE PERAAR-IO              TO FAKPAR-IO-AREA (97:2)
               MOVE PERMND-IO              TO FAKPAR-IO-AREA (99:2)
               MOVE FUPER-IO               TO FAKPAR-IO-AREA (101:6)
               MOVE FFU15-IO               TO FAKPAR-IO-AREA (107:6)
               MOVE FFU30-IO               TO FAKPAR-IO-AREA (113:6)
               MOVE FFU45-IO               TO FAKPAR-IO-AREA (119:6)
               MOVE FFU60-IO               TO FAKPAR-IO-AREA (125:6)
               MOVE FFU90-IO               TO FAKPAR-IO-AREA (131:6)
               MOVE FFU75-IO               TO FAKPAR-IO-AREA (137:6)
               MOVE FFU105-IO              TO FAKPAR-IO-AREA (143:6)
               MOVE FFU120-IO              TO FAKPAR-IO-AREA (149:6)
               MOVE FFU150-IO              TO FAKPAR-IO-AREA (155:6)
               MOVE FFU180-IO              TO FAKPAR-IO-AREA (161:6)
               MOVE FFD105-IO              TO FAKPAR-IO-AREA (167:6)
               MOVE FFD120-IO              TO FAKPAR-IO-AREA (173:6)
               MOVE FFD150-IO              TO FAKPAR-IO-AREA (179:6)
               MOVE FFD180-IO              TO FAKPAR-IO-AREA (185:6)
               MOVE IKKEF1                 TO FAKPAR-IO-AREA (191:3)
               MOVE IKKEF2                 TO FAKPAR-IO-AREA (194:3)
               MOVE IKKEF3                 TO FAKPAR-IO-AREA (197:3)
               WRITE FAKPAR-IO-AREA
           END-IF
           IF  (I-01)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMANR.'             TO LISTE-IO-AREA (2:8)
               MOVE 'FØRSTE FAKTURANR.'    TO LISTE-IO-AREA (20:17)
               MOVE 'FØRSTE KR.NOTANR.'    TO LISTE-IO-AREA (39:17)
               MOVE 'ANMERKNINGER.'        TO LISTE-IO-AREA (60:13)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-03 AND NOT-I-MR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FIRMNR                 TO LISTE-IO-AREA (6:3)
               MOVE FFNR-IO                TO LISTE-IO-AREA (20:6)
               MOVE FKNR-IO                TO LISTE-IO-AREA (39:6)
               MOVE 'OPPDATERT I FAKTURANR.' TO LISTE-IO-AREA (60:22)
               MOVE 'FILE.'                TO LISTE-IO-AREA (82:5)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-02 AND NOT-I-33 AND NOT-I-MR)
           OR  (I-03 AND I-MR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FIRMNR                 TO LISTE-IO-AREA (6:3)
               MOVE PFFNR-IO               TO LISTE-IO-AREA (20:6)
               MOVE PFKNR-IO               TO LISTE-IO-AREA (39:6)
               IF  (NOT-I-MR)
                   MOVE 'LEST INN I FAKTURANR.' TO LISTE-IO-AREA
                                                               (60:21)
               END-IF
               IF  (NOT-I-MR)
                   MOVE 'FILE FRA KORT.'   TO LISTE-IO-AREA (81:14)
               END-IF
               IF  (I-MR)
                   MOVE 'KORRIGERT  FAKTURANR.' TO LISTE-IO-AREA
                                                               (60:21)
               END-IF
               IF  (I-MR)
                   MOVE 'FILE FRA KORT.'   TO LISTE-IO-AREA (81:14)
               END-IF
               IF  (I-MR)
                   MOVE 'SISTE NR. VAR'    TO LISTE-IO-AREA (97:13)
               END-IF
               IF  (I-MR)
                   MOVE SFNR-IO            TO LISTE-IO-AREA (111:6)
               END-IF
               IF  (I-MR)
                   MOVE SKNR-IO            TO LISTE-IO-AREA (118:6)
               END-IF
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-02 AND I-33 AND I-MR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FIRMNR                 TO LISTE-IO-AREA (6:3)
               MOVE 'ER SLETTET FRA FAKTURA' TO LISTE-IO-AREA (60:22)
               MOVE 'NR.FILE.'             TO LISTE-IO-AREA (82:8)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-03 AND I-77)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (31:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '* FAKTURANR. ER NÅ SÅ  *' TO LISTE-IO-AREA (31:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '* HØYT AT DET DET  BØR *' TO LISTE-IO-AREA (31:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '* RETTES TIL 100000    *' TO LISTE-IO-AREA (31:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (31:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-03 AND I-79)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (31:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '* KREDITNR.  ER NÅ SÅ  *' TO LISTE-IO-AREA (31:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '* HØYT AT DET DET  BØR *' TO LISTE-IO-AREA (31:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '* RETTES TIL 900000    *' TO LISTE-IO-AREA (31:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (31:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-03 AND NOT-I-MR)
               MOVE SPACES TO FNRUT-IO-AREA
               INITIALIZE FNRUT-IO-AREA
               MOVE '91'                   TO FNRUT-IO-AREA (1:2)
               MOVE FIRMNR                 TO FNRUT-IO-AREA (3:3)
               MOVE FFNR-IO                TO FNRUT-IO-AREA (6:6)
               MOVE FKNR-IO                TO FNRUT-IO-AREA (12:6)
               MOVE SFNR-IO                TO FNRUT-IO-AREA (18:6)
               MOVE SKNR-IO                TO FNRUT-IO-AREA (24:6)
               MOVE NYFOM-IO               TO FNRUT-IO-AREA (30:4)
               MOVE FFAMND-IO              TO FNRUT-IO-AREA (39:6)
               MOVE FKRMND-IO              TO FNRUT-IO-AREA (45:6)
               WRITE FNRUT-IO-AREA
           END-IF
           IF  (I-02 AND NOT-I-33)
               MOVE SPACES TO FNRUT-IO-AREA
               INITIALIZE FNRUT-IO-AREA
               MOVE '91'                   TO FNRUT-IO-AREA (1:2)
               MOVE FIRMNR                 TO FNRUT-IO-AREA (3:3)
               MOVE PFFNR-IO               TO FNRUT-IO-AREA (6:6)
               MOVE PFKNR-IO               TO FNRUT-IO-AREA (12:6)
               MOVE PSFNR-IO               TO FNRUT-IO-AREA (18:6)
               MOVE PSKNR-IO               TO FNRUT-IO-AREA (24:6)
               MOVE NYFOM-IO               TO FNRUT-IO-AREA (30:4)
               MOVE PFFNR-IO               TO FNRUT-IO-AREA (39:6)
               MOVE PFKNR-IO               TO FNRUT-IO-AREA (45:6)
               WRITE FNRUT-IO-AREA
           END-IF.
 
       DETAIL-OVERFLOW SECTION.
       DETAIL-OVERFLOW-P.
           IF  (I-OF AND NOT-I-01)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMANR.'             TO LISTE-IO-AREA (2:8)
               MOVE 'FØRSTE FAKTURANR.'    TO LISTE-IO-AREA (20:17)
               MOVE 'FØRSTE KR.NOTANR.'    TO LISTE-IO-AREA (39:17)
               MOVE 'ANMERKNINGER.'        TO LISTE-IO-AREA (60:13)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR AND I-32)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FAKTURANR.FILE ER IKKE' TO LISTE-IO-AREA (11:22)
               MOVE 'FRA FOREGÅENDE PERIODE.' TO LISTE-IO-AREA (34:23)
               MOVE GMLFOM-IO              TO LISTE-IO-AREA (61:4)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ER BACK-UP FRA FOREGÅEND' TO LISTE-IO-AREA (11:24)
               MOVE 'E PERIODE KJØRT,'     TO LISTE-IO-AREA (35:16)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ELLER ER FAKTURAOMGANGS' TO LISTE-IO-AREA (11:23)
               MOVE 'NR. FEIL DENNE GANG.' TO LISTE-IO-AREA (34:20)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KJØR IKKE VIDERE FØR' TO LISTE-IO-AREA (11:20)
               MOVE 'DETTE ER AVKLART OG'  TO LISTE-IO-AREA (32:19)
               MOVE 'RETTET.'              TO LISTE-IO-AREA (52:7)
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
           PERFORM KORTAB-LOAD
           PERFORM MNDTAB-LOAD
           INITIALIZE KORTIN-DATA-FIELDS
           SET KORTIN-EOF-OFF              TO TRUE
           SET KORTIN-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO KORTIN-MC
                                              KORTIN-MP
           OPEN INPUT KORTIN
           OPEN OUTPUT FAKPAR
           INITIALIZE FNRIN-DATA-FIELDS
           SET FNRIN-EOF-OFF               TO TRUE
           SET FNRIN-PROCESS               TO TRUE
           MOVE LOW-VALUES                 TO FNRIN-MC
                                              FNRIN-MP
           OPEN INPUT FNRIN
           OPEN OUTPUT FNRUT
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           SET TABPER-I                    TO 1
           SET TABMNR-I                    TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE KORTIN
           CLOSE FAKPAR
           CLOSE FNRIN
           CLOSE FNRUT
           IF LISTE-IO-AREA NOT = SPACES
             WRITE LISTE-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE-IO-AREA
           END-IF
           CLOSE LISTE.
 
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
