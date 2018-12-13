       IDENTIFICATION DIVISION.
       PROGRAM-ID. RKO220R.
      *    KONV. IFRA RSK220 UTVIDET RECORD.     ***TXT***ok ss***    *
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM.......: RKO220, DANNER PRINTFILE KONTOKURANT OG     *
      *                          SALDOFILE TIL NYTT ÅR.              *
      *  E 21.07.98: HENTER UTSKR.-FREKVENS FRA SYSPARM, IKKE FIRMAF.*
      *  E 16.10.98: TILPASSET ÅR 2000.                              *
      *  E 21.10.98: ENDRET TEKST FOR AVSTEMMING.                    *
      *              TATT UT UTSKRIFT AV 2 EKS.                      *
      *              ERSTATTER PROGRAM RSK228.                       *
      *              DANNER AVSLUNINGSDATOER I STEDET FOR Å LESE INN.*
      *              BRUKER IKKE RESKONTROPARAMETER LENGER.          *
      *  E 11.12.98: FEILRETTING: KONT U/BEVEG I PERIODEN BLE FEIL.  *
      *  E 10.01.99: NY AVSTEMMINGSTEKST.                            *
      *  E 28.05.99: TOTALSUM EKS SANERTE FIRMA.                     *
      *  E 01.09.05: LAGT INN SALDO I VALUTABELØP.                   *
      * E 23.05.08 NYSBEL ER UTVIDET TIL 11 MED 2 DESIMALER       MT *
      * E 27.11.12 TILPASSET UTVIDEDE BELØPSFELT                  MT *
      *            SKRIVER AVSTEMMINGSFIL.                           *
      *            RETTET FEIL I AKKUMULERING AV SLETTEDE RECORDS.   *
      *            TELLER MED SLETTEMERKEDE RECORDS FRA RKO219.      *
      *            OVERFØRER RECORDS MED TILHØRIGHET LENGER FRAM ENN *
      *            NESTE ÅR I ÅRSSKIFTET I STEDET FOR Å SALDERE DEM. *
      *  FÅR       : REGNSKAPSPARAMETER (REGPAR),                    *
      *              KONTOKURANT (KTOKURI),                          *
      *              FIRMAFILE (FIRMAF),                             *
      *              PARAMETERFILE FOR FIRMA (SYSPARM),              *
      *  GJØR      : DANNER FILE TIL PRINT FOR DE FIRMA SOM HAR      *
      *              BESTILT UTSKRIFT AV KONTOKURANT.                *
      *              DANNER FILE MED AVSLUTNINGSDATOER.              *
      *              OVERFØRER POSTERINGER SOM HØRER TIL NYTT ÅR I   *
      *              SISTE KJØRING HVERT ÅR (MÅNED 12).              *
      *  GIR       : KVITTERINGSLISTE MED AVSTEMMINGSTOTALER (PRINT),*
      *              POSTER TIL PRINT (KTOKURO),                     *
      *              POSTER TIL NYTT ÅR (NYTTÅR),                    *
      *              FILE MED AVSLUTNINGSDATOER (PERIODE).           *
      ****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RKO220.rpg
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
           SELECT REGPAR
               ASSIGN TO UT-S-REGPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS REGPAR-STATUS.
           SELECT KTOKURI
               ASSIGN TO UT-S-KTOKURI
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KTOKURI-STATUS.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT SYSPARM
               ASSIGN TO SYSPARM
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS SYSPARM-STATUS
               RECORD KEY IS SYSPARM-KEY1.
           SELECT PRINT-1
               ASSIGN TO UT-S-PRINT1
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PRINT-1-STATUS.
           SELECT PRINT-2
               ASSIGN TO UT-S-PRINT2
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PRINT-2-STATUS.
           SELECT KTOKURO
               ASSIGN TO UT-S-KTOKURO
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KTOKURO-STATUS.
           SELECT NYTTA-ELGR
               ASSIGN TO UT-S-NYTTA-ELGR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS NYTTA-ELGR-STATUS.
           SELECT PERIODE
               ASSIGN TO UT-S-PERIODE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PERIODE-STATUS.
           SELECT AVSTEMO
               ASSIGN TO UT-S-AVSTEMO
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS AVSTEMO-STATUS.
           SELECT BUGFILO
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS BUGFILO-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD REGPAR
               BLOCK CONTAINS 100
               RECORD CONTAINS 100.
       01  REGPAR-IO-AREA.
           05  REGPAR-IO-AREA-X            PICTURE X(100).
       FD KTOKURI
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  KTOKURI-IO-AREA.
           05  KTOKURI-IO-AREA-X           PICTURE X(200).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD SYSPARM
               RECORD CONTAINS 160.
       01  SYSPARM-IO-AREA.
           05  SYSPARM-IO-AREA-X.
               10  SYSPARM-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(150).
       FD PRINT-1
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  PRINT-1-IO-PRINT.
           05  PRINT-1-IO-AREA-CONTROL     PICTURE X VALUE ' '.
        02 PRINT-1-IO-AREA.
           05  PRINT-1-IO-AREA-X           PICTURE X(132).
       FD PRINT-2
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  PRINT-2-IO-PRINT.
           05  PRINT-2-IO-AREA-CONTROL     PICTURE X VALUE ' '.
        02 PRINT-2-IO-AREA.
           05  PRINT-2-IO-AREA-X           PICTURE X(132).
       FD KTOKURO
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  KTOKURO-IO-AREA.
           05  KTOKURO-IO-AREA-X           PICTURE X(200).
       FD NYTTA-ELGR
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  NYTTA-ELGR-IO-AREA.
           05  NYTTA-ELGR-IO-AREA-X        PICTURE X(200).
       FD PERIODE
               BLOCK CONTAINS 4050
               RECORD CONTAINS 90.
       01  PERIODE-IO-AREA.
           05  PERIODE-IO-AREA-X           PICTURE X(90).
       FD AVSTEMO
               BLOCK CONTAINS 120
               RECORD CONTAINS 120.
       01  AVSTEMO-IO-AREA.
           05  AVSTEMO-IO-AREA-X           PICTURE X(120).
       FD BUGFILO
               BLOCK CONTAINS 81
               RECORD CONTAINS 81.
       01  BUGFILO-IO-PRINT.
           05  BUGFILO-IO-AREA-CONTROL     PICTURE X VALUE ' '.
        02 BUGFILO-IO-AREA.
           05  BUGFILO-IO-AREA-X           PICTURE X(80).
       WORKING-STORAGE SECTION.
       77  ARA-MAX   VALUE 12              PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  ARA-TABLE.
               10  ARA-ENTRY
                                           OCCURS 12 TIMES
                                           INDEXED BY ARA-I
                                                      ARA-S.
                   15  ARA                 PICTURE X(6).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  REGPAR-STATUS               PICTURE 99 VALUE 0.
           10  KTOKURI-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  SYSPARM-STATUS              PICTURE 99 VALUE 0.
           10  PRINT-1-STATUS              PICTURE 99 VALUE 0.
           10  PRINT-2-STATUS              PICTURE 99 VALUE 0.
           10  KTOKURO-STATUS              PICTURE 99 VALUE 0.
           10  NYTTA-ELGR-STATUS           PICTURE 99 VALUE 0.
           10  PERIODE-STATUS              PICTURE 99 VALUE 0.
           10  AVSTEMO-STATUS              PICTURE 99 VALUE 0.
           10  BUGFILO-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  REGPAR-EOF-OFF          VALUE '0'.
               88  REGPAR-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  REGPAR-READ-OFF         VALUE '0'.
               88  REGPAR-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  REGPAR-PROCESS-OFF      VALUE '0'.
               88  REGPAR-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KTOKURI-EOF-OFF         VALUE '0'.
               88  KTOKURI-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KTOKURI-READ-OFF        VALUE '0'.
               88  KTOKURI-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KTOKURI-PROCESS-OFF     VALUE '0'.
               88  KTOKURI-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  KTOKURI-LEVEL-INIT-OFF  VALUE '0'.
               88  KTOKURI-LEVEL-INIT      VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  SYSPARM-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  PRINT-1-DATA-FIELDS.
               10  PRINT-1-AFTER-SPACE     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINT-1-AFTER-SKIP      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINT-1-BEFORE-SPACE    PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINT-1-BEFORE-SKIP     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINT-1-MAX-LINES       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINT-1-LINE-COUNT      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINT-1-CLR-IO          PICTURE X VALUE 'Y'.
           05  PRINT-2-DATA-FIELDS.
               10  PRINT-2-AFTER-SPACE     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINT-2-AFTER-SKIP      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINT-2-BEFORE-SPACE    PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINT-2-BEFORE-SKIP     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINT-2-MAX-LINES       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINT-2-LINE-COUNT      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINT-2-CLR-IO          PICTURE X VALUE 'Y'.
           05  BUGFILO-DATA-FIELDS.
               10  BUGFILO-AFTER-SPACE     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  BUGFILO-AFTER-SKIP      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  BUGFILO-BEFORE-SPACE    PICTURE 9(4) BINARY
                                           VALUE 0.
               10  BUGFILO-BEFORE-SKIP     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  BUGFILO-MAX-LINES       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  BUGFILO-LINE-COUNT      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  BUGFILO-CLR-IO          PICTURE X VALUE 'Y'.
           05  REGPAR-DATA-FIELDS.
               10  MNDNR-IO.
                   15  MNDNR               PICTURE S9(2).
      *                                       3   40REGÅR
               10  A-ELGR                  PICTURE X(2).
               10  ARH                     PICTURE X(2).
               10  ARHAAR                  PICTURE X(4).
           05  KTOKURI-LEVEL-02.
               10  KTOKURI-02-L5.
                   15  KTOKURI-02-L5-FIRMA PICTURE X(3).
               10  KTOKURI-02-L4.
                   15  KTOKURI-02-L4-RESK  PICTURE X(6).
           05  KTOKURI-LEVEL-03.
               10  KTOKURI-03-L5.
                   15  KTOKURI-03-L5-FIRMA PICTURE X(3).
               10  KTOKURI-03-L4.
                   15  KTOKURI-03-L4-RESK  PICTURE X(6).
           05  KTOKURI-DATA-FIELDS.
               10  RA                      PICTURE X(2).
               10  FIRMA                   PICTURE X(3).
               10  RESK                    PICTURE X(6).
               10  SDATO                   PICTURE X(6).
               10  SBEL-IO.
                   15  SBEL                PICTURE S9(8)V9(2).
               10  SVBEL-IO.
                   15  SVBEL               PICTURE S9(9)V9(2).
               10  SBELU-IO.
                   15  SBELU               PICTURE S9(11)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  SVBELU-IO.
                   15  SVBELU              PICTURE S9(11)V9(4) USAGE
                                                       PACKED-DECIMAL.
               10  SSLETT                  PICTURE X(1).
               10  BELO-ELGP-IO.
                   15  BELO-ELGP           PICTURE S9(7)V9(2).
               10  VBELO-ELGP-IO.
                   15  VBELO-ELGP          PICTURE S9(8)V9(2).
               10  BELO-ELGPU-IO.
                   15  BELO-ELGPU          PICTURE S9(11)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  VBELO-ELGU-IO.
                   15  VBELO-ELGU          PICTURE S9(11)V9(4) USAGE
                                                       PACKED-DECIMAL.
               10  KTOPER-IO.
                   15  KTOPER              PICTURE S9(6).
               10  KTOA-ELGR4-IO.
                   15  KTOA-ELGR4          PICTURE S9(4).
               10  TSLETT                  PICTURE X(1).
               10  REC                     PICTURE X(200).
           05  FIRMAF-DATA-FIELDS.
               10  FINAVN                  PICTURE X(30).
               10  FSLETT                  PICTURE X(1).
      *                                     164 164 KODEF
           05  SYSPARM-DATA-FIELDS.
               10  KODEF                   PICTURE X(1).
      ****************************************************************
      * HENTER DAGENS DATO                                           *
      ****************************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L5            PICTURE X(3).
               10  THE-PRIOR-L4            PICTURE X(6).
           05  TEMP-REMAINDER              PICTURE S9(9)V9(9).
           05  TEMPORARY-FIELDS.
               10  KLOKKE-IO.
                   15  KLOKKE              PICTURE S9(6).
               10  DDMMAA                  PICTURE X(6).
               10  DD                      PICTURE X(2).
               10  MMAA                    PICTURE X(4).
               10  MM                      PICTURE X(2).
               10  AA                      PICTURE X(2).
               10  PDTO4-IO.
                   15  PDTO4               PICTURE S9(4).
               10  PRDDTO-IO.
                   15  PRDDTO              PICTURE S9(8).
               10  PRDKLK-IO.
                   15  PRDKLK              PICTURE S9(6).
               10  ANTALL-IO.
                   15  ANTALL              PICTURE S9(9).
               10  TOTBEL-IO.
                   15  TOTBEL              PICTURE S9(11)V9(2).
               10  TOTBEU-IO.
                   15  TOTBEU              PICTURE S9(11)V9(2).
               10  SALDOD                  PICTURE X(6).
               10  PRO-ELGVE1              PICTURE X(4).
               10  PRO-ELGVE2              PICTURE X(4).
               10  SYSKEY                  PICTURE X(10).
               10  KJMND-IO.
                   15  KJMND               PICTURE S9(2).
               10  SLMND-IO.
                   15  SLMND               PICTURE S9(2).
               10  FRAPER-IO.
                   15  FRAPER              PICTURE S9(6).
               10  TILPER-IO.
                   15  TILPER              PICTURE S9(6).
               10  X-IO.
                   15  X                   PICTURE S9(2).
               10  FSBEL-IO.
                   15  FSBEL               PICTURE S9(9)V9(2).
               10  FSBELU-IO.
                   15  FSBELU              PICTURE S9(11)V9(2).
               10  FSVBEL-IO.
                   15  FSVBEL              PICTURE S9(9)V9(2).
               10  FSVBEU-IO.
                   15  FSVBEU              PICTURE S9(11)V9(4).
               10  M-IO.
                   15  M                   PICTURE S9(2).
               10  BUGFL1                  PICTURE X(8).
               10  ANTSLE-IO.
                   15  ANTSLE              PICTURE S9(9).
               10  SLEBEL-IO.
                   15  SLEBEL              PICTURE S9(11)V9(2).
               10  SLEBEU-IO.
                   15  SLEBEU              PICTURE S9(11)V9(2).
               10  ANTAKT-IO.
                   15  ANTAKT              PICTURE S9(9).
               10  ANTUNA-ELG-IO.
                   15  ANTUNA-ELG          PICTURE S9(9).
               10  ANYSAL-IO.
                   15  ANYSAL              PICTURE S9(9).
               10  LRNYBE-IO.
                   15  LRNYBE              PICTURE S9(11)V9(2).
               10  LRNYBU-IO.
                   15  LRNYBU              PICTURE S9(11)V9(2).
               10  PERD-IO.
                   15  PERD                PICTURE S9(2).
               10  PER2-IO.
                   15  PER2                PICTURE S9(4).
               10  PERM-IO.
                   15  PERM                PICTURE S9(2).
               10  PERA-ELG-IO.
                   15  PERA-ELG            PICTURE S9(2).
               10  NETBEL-IO.
                   15  NETBEL              PICTURE S9(11)V9(2).
               10  NETBEU-IO.
                   15  NETBEU              PICTURE S9(11)V9(2).
               10  NYA-ELGRAN-IO.
                   15  NYA-ELGRAN          PICTURE S9(9).
               10  NYA-ELGRBE-IO.
                   15  NYA-ELGRBE          PICTURE S9(11)V9(2).
               10  NYA-ELGRBU-IO.
                   15  NYA-ELGRBU          PICTURE S9(11)V9(2).
               10  LRRAKT-IO.
                   15  LRRAKT              PICTURE S9(13).
               10  LRRABE-IO.
                   15  LRRABE              PICTURE S9(13)V9(2).
               10  LRRABU-IO.
                   15  LRRABU              PICTURE S9(13)V9(2).
               10  REGPER-IO.
                   15  REGPER              PICTURE S9(6).
               10  REGA-ELGR4-IO.
                   15  REGA-ELGR4          PICTURE S9(4).
               10  NYTA-ELGR4-IO.
                   15  NYTA-ELGR4          PICTURE S9(4).
               10  HJNUM4-IO.
                   15  HJNUM4              PICTURE S9(2)V9(2).
               10  HJNUM-IO.
                   15  HJNUM               PICTURE S9(2).
               10  ANTINA-ELG-IO.
                   15  ANTINA-ELG          PICTURE S9(9).
               10  TOTBNG-IO.
                   15  TOTBNG              PICTURE S9(9)V9(2).
               10  TOTBNN-IO.
                   15  TOTBNN              PICTURE S9(11)V9(2).
               10  ANTSAL-IO.
                   15  ANTSAL              PICTURE S9(9).
               10  SALBNG-IO.
                   15  SALBNG              PICTURE S9(9)V9(2).
               10  SALBNN-IO.
                   15  SALBNN              PICTURE S9(11)V9(2).
               10  NYSBEL-IO.
                   15  NYSBEL              PICTURE S9(9)V9(2).
               10  NYSVBE-IO.
                   15  NYSVBE              PICTURE S9(9)V9(2).
               10  NYSBEU-IO.
                   15  NYSBEU              PICTURE S9(11)V9(2).
               10  NYSVBU-IO.
                   15  NYSVBU              PICTURE S9(11)V9(4).
               10  NETBNG-IO.
                   15  NETBNG              PICTURE S9(9)V9(2).
               10  NETBNN-IO.
                   15  NETBNN              PICTURE S9(11)V9(2).
               10  ANTTRA-ELG-IO.
                   15  ANTTRA-ELG          PICTURE S9(9).
               10  TRABEL-IO.
                   15  TRABEL              PICTURE S9(11)V9(2).
               10  TRABEU-IO.
                   15  TRABEU              PICTURE S9(11)V9(2).
           05  EDITTING-FIELDS.
               10  XO-112P-EF.
                 15  XO-112P               PICTURE S9(11)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-114P-EF.
                 15  XO-114P               PICTURE S9(11)V9(4) USAGE
                                                       PACKED-DECIMAL.
               10  XO-80P-EF.
                 15  XO-80P                PICTURE S9(8) USAGE
                                                       PACKED-DECIMAL.
               10  XO-60P-EF.
                 15  XO-60P                PICTURE S9(6) USAGE
                                                       PACKED-DECIMAL.
               10  XO-112YYZR              PICTURE ZZ.ZZZ.ZZZ.ZZZ,ZZ-.
               10  XO-90YY9                PICTURE ZZZ.ZZZ.ZZ9.
               10  XO-92YYZR               PICTURE ZZZ.ZZZ.ZZZ,ZZ-.
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
           SET NOT-I-06                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-04                    TO TRUE
           SET NOT-I-07                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  REGPAR-PROCESS
               SET REGPAR-PROCESS-OFF      TO TRUE
               SET REGPAR-READ             TO TRUE
           END-IF
 
           IF  REGPAR-READ
           AND RECORD-SELECTED-OFF
               PERFORM REGPAR-GET
               SET REGPAR-READ-OFF         TO TRUE
               IF  NOT REGPAR-EOF
                   PERFORM REGPAR-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET REGPAR-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  KTOKURI-PROCESS
               SET KTOKURI-PROCESS-OFF     TO TRUE
               SET KTOKURI-READ            TO TRUE
           END-IF
 
           IF  KTOKURI-READ
           AND RECORD-SELECTED-OFF
               PERFORM KTOKURI-GET
               SET KTOKURI-READ-OFF        TO TRUE
               IF  NOT KTOKURI-EOF
                   PERFORM KTOKURI-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET KTOKURI-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  REGPAR-PROCESS
               PERFORM REGPAR-IDSET
           END-IF
 
           IF  KTOKURI-PROCESS
               PERFORM KTOKURI-IDSET
           END-IF
 
           IF  KTOKURI-PROCESS
               PERFORM KTOKURI-CHK-LEVEL
           END-IF
 
           IF I-1ST
               GO TO LR-INDICATOR-TEST
           END-IF.
 
       MAINLINE-TOTAL-CALCS.
           PERFORM TOTAL-CALCS
           IF  I-LR
               PERFORM LR-CALCS
           END-IF
           PERFORM TOTAL-OUTPUT.
 
       LR-INDICATOR-TEST.
           IF  I-LR
               GO TO MAINLINE-TERMINATION
           END-IF
           PERFORM HEADING-OVERFLOW
 
           IF  REGPAR-PROCESS
               PERFORM REGPAR-FLDSET
           END-IF
 
           IF  KTOKURI-PROCESS
               PERFORM KTOKURI-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  KTOKURI-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-91)
               SET NOT-I-92                TO TRUE
           END-IF
           IF  (NOT-I-91)
               ACCEPT SYSTEM-TIME-X      FROM TIME
               MOVE SYSTEM-TIME            TO KLOKKE (1:6)
               MOVE UDATE                  TO DDMMAA
               MOVE DDMMAA (1:2)           TO DD
               MOVE DDMMAA (3:4)           TO MMAA
               MOVE MMAA (1:2)             TO MM
               MOVE MMAA (3:2)             TO AA
               MOVE 20                     TO PDTO4 (1:2)
               MOVE AA                     TO PDTO4-IO (3:2)
               MOVE PDTO4                  TO PRDDTO (1:4)
               MOVE MM                     TO PDTO4 (1:2)
               MOVE DD                     TO PDTO4-IO (3:2)
               MOVE PDTO4                  TO PRDDTO-IO (5:4)
               ACCEPT SYSTEM-TIME-X      FROM TIME
               MOVE SYSTEM-TIME            TO PRDKLK (1:6)
               SET I-91                    TO TRUE
               SET I-92                    TO TRUE
      ****************************************************************
      *  SETTER INDIKATOR FOR MÅNED.                                 *
      ****************************************************************
           END-IF
           IF  (I-06)
               PERFORM PARRUT-S
           END-IF
           IF  (I-06)
               SET NOT-I-71                TO TRUE
               IF  MNDNR = 01
                   SET I-71                TO TRUE
               END-IF
               SET NOT-I-75                TO TRUE
               IF  MNDNR = 03
                   SET I-75                TO TRUE
               END-IF
               SET NOT-I-76                TO TRUE
               IF  MNDNR = 06
                   SET I-76                TO TRUE
               END-IF
               SET NOT-I-77                TO TRUE
               IF  MNDNR = 12
                   SET I-77                TO TRUE
               END-IF
               GO TO SLUTT-T
      ****************************************************************
      *  BEREGNING AV AVSTEMMINGSTOTALER.                            *
      ****************************************************************
           END-IF
           IF  (I-02)
               ADD 1                       TO ANTALL
               ADD SBEL                    TO TOTBEL ROUNDED
               ADD SBELU                   TO TOTBEU ROUNDED
           END-IF
           IF  (I-03)
               ADD 1                       TO ANTALL
               ADD BELO-ELGP               TO TOTBEL
               ADD BELO-ELGPU              TO TOTBEU
      ******************************************************
           END-IF
           IF  (I-02)
               MOVE SDATO                  TO SALDOD
      ****************************************************************
      * ENDRE SALDODATOEN PÅ INNGÅENDE SALDO DERSOM FØRSTE KJØRING   *
      ****************************************************************
           END-IF
           IF  (I-02)
               MOVE '1231'                 TO PRO-ELGVE1
               MOVE SDATO (3:4)            TO PRO-ELGVE2
               SET NOT-I-74                TO TRUE
               IF  PRO-ELGVE1 = PRO-ELGVE2
                   SET I-74                TO TRUE
               END-IF
      ****************************************************************
           END-IF
           IF  (I-02 AND I-74)
               MOVE A-ELGR                 TO SALDOD (1:2)
               MOVE '0101'                 TO SALDOD (3:4)
           END-IF
           IF  (I-L5)
               SET NOT-I-90                TO TRUE
           END-IF
           IF  (I-L4)
               SET NOT-I-86                TO TRUE
               MOVE 0                      TO FSBEL
               MOVE 0                      TO FSBELU
               MOVE 0                      TO FSVBEL
               MOVE 0                      TO FSVBEU
               MOVE 0                      TO NYSBEL
               MOVE 0                      TO NYSBEU
               MOVE 0                      TO NYSVBE
               MOVE 0                      TO NYSVBU
               SET NOT-I-15                TO TRUE
      ****************************************************************
           END-IF
           IF  (I-L5)
               GO TO FIRMAS-T
           END-IF
           IF  (I-90)
               GO TO PRINTU-T
           END-IF
           GO TO SLUTT-T
      ****************************************************************
           .
 
       FIRMAS-T.
           SET NOT-I-21                    TO TRUE
           SET NOT-I-22                    TO TRUE
           SET NOT-I-23                    TO TRUE
           SET NOT-I-24                    TO TRUE
           SET NOT-I-25                    TO TRUE
           SET NOT-I-26                    TO TRUE
           SET NOT-I-27                    TO TRUE
           SET NOT-I-28                    TO TRUE
           SET NOT-I-29                    TO TRUE
           SET NOT-I-32                    TO TRUE
           SET NOT-I-33                    TO TRUE
           SET NOT-I-34                    TO TRUE
           IF  (I-L5)
               MOVE FIRMA                  TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-20                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-20            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
           END-IF
           IF  (I-L5 AND NOT-I-20)
               SET NOT-I-39                TO TRUE
               IF  FSLETT = 'S'
                   SET I-39                TO TRUE
               END-IF
           END-IF
           IF  (I-20)
               GO TO SLUTT-T
           END-IF
           IF  (I-L5)
               MOVE FIRMA                  TO SYSKEY (1:3)
               MOVE 'RES*011'              TO SYSKEY (4:7)
               MOVE SYSKEY                 TO SYSPARM-KEY1
               READ SYSPARM RECORD KEY IS SYSPARM-KEY1
               INVALID KEY
                   SET I-36                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-36            TO TRUE
                   PERFORM SYSPARM-FLDSET
                   PERFORM SYSPARM-IDSET
               END-READ
           END-IF
           IF  (I-36 AND I-L5)
               MOVE '4'                    TO KODEF
      *  U8 36             MOVE "SYSKEY  "BUGFL1  8        LEDETXT DEBUG
      *  U8 36   BUGFL1    DEBUGBUGFILO   SYSKEY           VIS FELT/IND
           END-IF
           SET NOT-I-19                    TO TRUE
           IF  KODEF = '0'
               SET I-19                    TO TRUE
           END-IF
           IF  (I-19)
               GO TO SLUTT-T
           END-IF
           SET I-90                        TO TRUE
           SET NOT-I-32                    TO TRUE
           IF  MNDNR = 3
               SET I-32                    TO TRUE
           END-IF
           IF  (NOT-I-32)
               SET NOT-I-33                TO TRUE
               IF  MNDNR = 6
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-33)
               SET I-32                    TO TRUE
           END-IF
           IF  (NOT-I-32)
               SET NOT-I-32                TO TRUE
               IF  MNDNR = 9
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-32)
               SET NOT-I-34                TO TRUE
               IF  MNDNR = 12
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-34)
               SET I-32                    TO TRUE
               SET I-33                    TO TRUE
           END-IF
           SET NOT-I-21                    TO TRUE
           IF  KODEF = '1'
               SET I-21                    TO TRUE
           END-IF.
 
       MNDLIG-T.
           IF  (I-21)
               ADD MNDNR TO ZERO       GIVING KJMND
               ADD MNDNR TO ZERO       GIVING SLMND
               GO TO PRINTS-T
           END-IF
           IF  (I-32)
               SET NOT-I-22                TO TRUE
               IF  KODEF = '2'
                   SET I-22                TO TRUE
               END-IF
           END-IF.
 
       KVART-T.
           IF  (I-22)
               SUBTRACT 2 FROM MNDNR   GIVING KJMND
               ADD MNDNR TO ZERO       GIVING SLMND
               GO TO PRINTS-T
           END-IF
           IF  (I-33)
               SET NOT-I-23                TO TRUE
               IF  KODEF = '3'
                   SET I-23                TO TRUE
               END-IF
           END-IF
           IF  (I-23)
               SUBTRACT 5 FROM MNDNR   GIVING KJMND
               ADD MNDNR TO ZERO       GIVING SLMND
               GO TO PRINTS-T
           END-IF
           IF  (I-34)
               SET NOT-I-24                TO TRUE
               IF  KODEF = '4'
                   SET I-24                TO TRUE
               END-IF
           END-IF.
 
       A-ELGRLIG-T.
           IF  (I-24)
               MOVE 1                      TO KJMND
               MOVE 12                     TO SLMND
               GO TO PRINTS-T
           END-IF
           SET NOT-I-25                    TO TRUE
           IF  KODEF = '5'
               SET I-25                    TO TRUE
           END-IF
           IF  (I-25 AND NOT-I-34)
               SET I-21                    TO TRUE
           END-IF
           IF  (I-21)
               GO TO MNDLIG-T
           END-IF
           IF  (I-25 AND I-34)
               SET I-24                    TO TRUE
           END-IF
           IF  (I-24)
               GO TO A-ELGRLIG-T
           END-IF
           SET NOT-I-26                    TO TRUE
           IF  KODEF = '6'
               SET I-26                    TO TRUE
           END-IF
           IF  (I-26 AND I-32 AND NOT-I-34)
               SET I-22                    TO TRUE
           END-IF
           IF  (I-22)
               GO TO KVART-T
           END-IF
           IF  (I-26 AND I-34)
               SET I-24                    TO TRUE
           END-IF
           IF  (I-24)
               GO TO A-ELGRLIG-T
           END-IF
           SET NOT-I-90                    TO TRUE
           GO TO SLUTT-T
      *
      ******** BEREGNING AV PERIODE PR KUNDE. ************************
           .
 
       PRINTS-T.
           MOVE REGPER                     TO FRAPER
           MOVE KJMND                      TO FRAPER-IO (5:2)
           MOVE REGPER                     TO TILPER
           MOVE SLMND                      TO TILPER-IO (5:2)
           ADD SLMND TO ZERO           GIVING X
      *  U8                MOVE "FRAPER  "BUGFL1  8        LEDETXT DEBUG
      *  U8      BUGFL1    DEBUGBUGFILO   FRAPER           VIS FELT/IND
      *
      ******** KONTROLL PR REC PÅ SAMME KUNDE.  **********************
           .
 
       PRINTU-T.
           SET NOT-I-97                    TO TRUE
           SET NOT-I-98                    TO TRUE
           SET NOT-I-99                    TO TRUE
           IF  (I-03)
               SET NOT-I-99                TO TRUE
               SET NOT-I-96                TO TRUE
               IF  KTOPER NOT < FRAPER
                   SET I-99                TO TRUE
               END-IF
               IF  KTOPER < FRAPER
                   SET I-96                TO TRUE
               END-IF
           END-IF
           IF  (I-99)
               SET NOT-I-98                TO TRUE
               IF  KTOPER > TILPER
                   SET I-98                TO TRUE
               END-IF
           END-IF
           IF  (I-77)
               SET NOT-I-97                TO TRUE
               IF  KTOA-ELGR4 > REGA-ELGR4
                   SET I-97                TO TRUE
               END-IF
           END-IF
           IF  (I-97)
               OR  (I-98 AND I-95)
               SET NOT-I-99                TO TRUE
           END-IF
           IF  (I-02)
               ADD SBEL TO ZERO        GIVING FSBEL
               SET NOT-I-94                TO TRUE
               SET NOT-I-95                TO TRUE
               IF  FSBEL NOT = 0
                   SET I-94                TO TRUE
               END-IF
               IF  FSBEL = 0
                   SET I-95                TO TRUE
               END-IF
               ADD SBELU TO ZERO       GIVING FSBELU
               ADD SVBEL TO ZERO       GIVING FSVBEL
               ADD SVBELU TO ZERO      GIVING FSVBEU
           END-IF
           IF  (I-03 AND I-96)
               ADD BELO-ELGP               TO FSBEL
               SET NOT-I-94                TO TRUE
               SET NOT-I-95                TO TRUE
               IF  FSBEL NOT = 0
                   SET I-94                TO TRUE
               END-IF
               IF  FSBEL = 0
                   SET I-95                TO TRUE
               END-IF
               ADD BELO-ELGPU              TO FSBELU
               ADD VBELO-ELGP              TO FSVBEL
               ADD VBELO-ELGU              TO FSVBEU
      ******** UTLEGGELSE AV SALDORECORD.  ***************************
           END-IF
           IF  (I-99 AND NOT-I-15)
               SUBTRACT 1 FROM KJMND   GIVING M
               SET NOT-I-72                TO TRUE
               IF  M NOT > 0
                   SET I-72                TO TRUE
               END-IF
               PERFORM EXCEPTION-OUTPUT
               SET I-15                    TO TRUE
      ****************************************************************
           END-IF
           .
 
       SLUTT-T.
           IF  (I-02)
               SET NOT-I-27                TO TRUE
               IF  SSLETT = 'S'
                   SET I-27                TO TRUE
               END-IF
           END-IF
           IF  (I-03)
               SET NOT-I-28                TO TRUE
               IF  TSLETT = 'S'
                   SET I-28                TO TRUE
               END-IF
           END-IF
           IF  (I-20)
               OR  (I-27)
               OR  (I-28)
               OR  (I-39)
               SET I-29                    TO TRUE
           END-IF
           IF  (I-77 AND NOT-I-29)
               PERFORM NYSAL-S
           END-IF
           SET NOT-I-85                    TO TRUE
           IF  (I-90 AND I-99 AND NOT-I-98)
               SET I-85                    TO TRUE
           END-IF
           IF  (I-85)
               SET I-86                    TO TRUE
           END-IF
           IF  (I-U8 AND I-39 AND I-L5)
               MOVE 'FIRMA   '             TO BUGFL1
           END-IF
           IF  (I-U8 AND I-39)
               MOVE 'REC     '             TO BUGFL1
           END-IF
           IF  (I-02 AND I-29)
               ADD 1                       TO ANTSLE
               ADD SBEL                    TO SLEBEL ROUNDED
               ADD SBELU                   TO SLEBEU ROUNDED
           END-IF
           IF  (I-03 AND I-29)
               ADD 1                       TO ANTSLE
               ADD BELO-ELGP               TO SLEBEL
               ADD BELO-ELGPU              TO SLEBEU
           END-IF
           IF  (I-02 AND NOT-I-29)
               ADD 1                       TO ANTAKT
           END-IF
           IF  (I-03 AND NOT-I-29)
               ADD 1                       TO ANTAKT
      *  29                MOVE "ANTSLE  "BUGFL1  8        LEDETXT DEBUG
      *  29    L5BUGFL1    DEBUGBUGFILO   ANTSLE           VIS FELT/IND
      ****************************************************************
      * TESTE OM DET ER BEVEGELSE PÅ KUNDEN,HVIS IKKE LEGGES         *
      * SALDOREC UT DERSOM NYTT ÅR.                                  *
      ****************************************************************
           END-IF
           .
 
       PARRUT-S SECTION.
       PARRUT-S-P.
           MOVE ARHAAR                     TO REGPER (1:4)
           MOVE MNDNR                      TO REGPER-IO (5:2)
           MOVE ARHAAR                     TO REGA-ELGR4-IO
           ADD 1 TO REGA-ELGR4         GIVING NYTA-ELGR4
           DIVIDE REGA-ELGR4 BY 4      GIVING HJNUM4
                                    REMAINDER TEMP-REMAINDER
           MOVE TEMP-REMAINDER             TO HJNUM
           SET NOT-I-70                    TO TRUE
           IF  HJNUM > 0
               SET I-70                    TO TRUE
           END-IF
           MOVE A-ELGR                     TO ARA-TABLE (1:2)
           MOVE '0131'                     TO ARA (1) (3:4)
           IF  (NOT-I-70)
               MOVE '0228'                 TO ARA (2) (3:4)
           END-IF
           IF  (I-70)
               MOVE '0229'                 TO ARA (2) (3:4)
           END-IF
           MOVE '0331'                     TO ARA (3) (3:4)
           MOVE '0430'                     TO ARA (4) (3:4)
           MOVE '0531'                     TO ARA (5) (3:4)
           MOVE '0630'                     TO ARA (6) (3:4)
           MOVE '0731'                     TO ARA (7) (3:4)
           MOVE '0831'                     TO ARA (8) (3:4)
           MOVE '0930'                     TO ARA (9) (3:4)
           MOVE '1031'                     TO ARA (10) (3:4)
           MOVE '1130'                     TO ARA (11) (3:4)
           MOVE '1231'                     TO ARA (12) (3:4).
      ****************************************************************
      *  SUBRUTINE FOR OVERFØRING AV SALDO TIL NESTE ÅR              *
      ****************************************************************
 
       NYSAL-S SECTION.
       NYSAL-S-P.
           IF  (I-02)
               ADD 1                       TO ANTINA-ELG
           END-IF
           IF  (I-03)
               ADD 1                       TO ANTINA-ELG
           END-IF
           IF  (I-02)
               ADD SBEL                    TO TOTBNG
               ADD SBELU                   TO TOTBNN
           END-IF
           IF  (I-03)
               ADD BELO-ELGP               TO TOTBNG
               ADD BELO-ELGPU              TO TOTBNN
      *
      *R 03      KTOÅR4    COMP NYTÅR4                   37 E:31.08.12
           END-IF
           IF  (I-03)
               SET NOT-I-38                TO TRUE
               SET NOT-I-37                TO TRUE
               IF  KTOA-ELGR4 > NYTA-ELGR4
                   SET I-38                TO TRUE
               END-IF
               IF  KTOA-ELGR4 = NYTA-ELGR4
                   SET I-37                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-38)
               SET I-37                    TO TRUE
           END-IF
           IF  (I-U8 AND I-38)
               MOVE 'REC     '             TO BUGFL1
           END-IF
           IF  (I-03 AND NOT-I-37)
               ADD 1                       TO ANTSAL
           END-IF
           IF  (I-02 AND I-29)
               ADD SBEL                    TO SALBNG
               ADD SBELU                   TO SALBNN
           END-IF
           IF  (I-03 AND I-29)
               ADD BELO-ELGP               TO SALBNG
               ADD BELO-ELGPU              TO SALBNN
           END-IF
           IF  (I-02)
               ADD SBEL TO ZERO        GIVING NYSBEL
               ADD SVBEL TO ZERO       GIVING NYSVBE
               ADD SBELU TO ZERO       GIVING NYSBEU
               ADD SVBELU TO ZERO      GIVING NYSVBU
           END-IF
           IF  (I-03 AND NOT-I-37)
               ADD BELO-ELGP               TO NYSBEL
               ADD VBELO-ELGP              TO NYSVBE
               ADD BELO-ELGPU              TO NYSBEU
               ADD VBELO-ELGU              TO NYSVBU
      *
           END-IF
           IF  (I-03 AND I-37)
               ADD 1                       TO ANTUNA-ELG
           END-IF
           IF  (I-02)
               ADD SBEL                    TO NETBNG
               ADD SBELU                   TO NETBNN
           END-IF
           IF  (I-03)
               ADD BELO-ELGP               TO NETBNG
               ADD BELO-ELGPU              TO NETBNN
      *
           END-IF
           IF  (I-03 AND I-37)
               ADD 1                       TO ANTTRA-ELG
               ADD BELO-ELGP               TO TRABEL
               ADD BELO-ELGPU              TO TRABEU
           END-IF.
      ****************************************************************
      *  OUTPUT                                                      *
      ****************************************************************
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L4)
               SET NOT-I-73                TO TRUE
               IF  RA = '30'
                   SET I-73                TO TRUE
               END-IF
           END-IF
           IF  (I-L4 AND I-73 AND I-71)
               AND (I-21)
               PERFORM EXCEPTION-OUTPUT
           END-IF
           IF  (I-L4 AND I-73 AND I-75)
               AND (I-22)
               PERFORM EXCEPTION-OUTPUT
           END-IF
           IF  (I-L4 AND I-73 AND I-76)
               AND (I-23)
               PERFORM EXCEPTION-OUTPUT
           END-IF
           IF  (I-L4 AND I-90 AND NOT-I-15)
               AND (I-94)
               PERFORM EXCEPTION-OUTPUT
           END-IF
           IF  (I-L4 AND I-73 AND I-77)
               AND (I-24)
               PERFORM EXCEPTION-OUTPUT
           END-IF
           IF  (I-L4 AND I-77 AND NOT-I-29)
               ADD 1                       TO ANTUNA-ELG
               ADD 1                       TO ANYSAL
               ADD NYSBEL                  TO LRNYBE
               ADD NYSBEU                  TO LRNYBU
      ******** PERIODEDATO TIL UTSKRIFTSPROGRAM   ***************
           END-IF
           IF  (I-L4 AND I-86)
               MOVE ARA (X) (5:2)          TO PERD-IO
               MOVE ARA (X) (1:4)          TO PER2
               MOVE PER2 (3:2)             TO PERM-IO
               MOVE PER2 (1:2)             TO PERA-ELG
      ****************************************************************
      *  BEREGNING AV BELØP FOR SLETTEDE FIRMA OG AVSTEMMINGER       *
      ****************************************************************
           END-IF
           .
 
       LR-CALCS SECTION.
       LR-CALCS-P.
           SUBTRACT SLEBEL FROM TOTBEL GIVING NETBEL ROUNDED
           SUBTRACT SLEBEU FROM TOTBEU GIVING NETBEU ROUNDED
           ADD ANTTRA-ELG TO ANYSAL    GIVING NYA-ELGRAN
           ADD LRNYBE TO TRABEL        GIVING NYA-ELGRBE ROUNDED
           ADD LRNYBU TO TRABEU        GIVING NYA-ELGRBU ROUNDED
      *R                   Z-ADDANTALL    LRANT  130
      *R                   Z-ADDTOTBEL    LRTBEL 152
      *R                   Z-ADDTOTBEU    LRTBEU 152
      *R                   Z-ADDANTSLE    LRRSAN 130
      *R                   Z-ADDSLEBEL    LRRSBE 152
      *R                   Z-ADDSLEBEU    LRRSBU 152
           ADD ANTAKT TO ZERO          GIVING LRRAKT
           ADD NETBEL TO ZERO          GIVING LRRABE
           ADD NETBEU TO ZERO          GIVING LRRABU
      ****************************************************************
      *  SUBRUTINE FOR OVERFØRING AV SALDO TIL NESTE ÅR              *
      ****************************************************************
           .
 
       REGPAR-GET SECTION.
       REGPAR-GET-P.
           IF  REGPAR-EOF-OFF
               READ REGPAR
               AT END
                   SET REGPAR-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       REGPAR-FLDSET SECTION.
       REGPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ( REGPAR-IO-AREA (1:1) = '9' )
               MOVE REGPAR-IO-AREA (9:2)   TO MNDNR-IO
               INSPECT MNDNR-IO REPLACING ALL ' ' BY '0'
               MOVE REGPAR-IO-AREA (3:2)   TO A-ELGR (1:2)
               MOVE REGPAR-IO-AREA (20:2)  TO ARH (1:2)
               MOVE REGPAR-IO-AREA (20:4)  TO ARHAAR (1:4)
           END-EVALUATE.
 
       REGPAR-IDCHK SECTION.
       REGPAR-IDCHK-P.
           EVALUATE TRUE
           WHEN ( REGPAR-IO-AREA (1:1) = '9' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       REGPAR-IDSET SECTION.
       REGPAR-IDSET-P.
           EVALUATE TRUE
           WHEN ( REGPAR-IO-AREA (1:1) = '9' )
               SET I-06                    TO TRUE
           END-EVALUATE.
 
       KTOKURI-GET SECTION.
       KTOKURI-GET-P.
           IF  KTOKURI-EOF-OFF
               READ KTOKURI
               AT END
                   SET KTOKURI-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KTOKURI-FLDSET SECTION.
       KTOKURI-FLDSET-P.
           EVALUATE TRUE
           WHEN ( KTOKURI-IO-AREA (1:1) = '3'
            AND   KTOKURI-IO-AREA (2:1) = '0' )
               MOVE KTOKURI-IO-AREA (1:2)  TO RA (1:2)
               MOVE KTOKURI-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE KTOKURI-IO-AREA (6:6)  TO RESK (1:6)
               MOVE KTOKURI-IO-AREA (20:6) TO SDATO (1:6)
               MOVE KTOKURI-IO-AREA (38:10) TO SBEL-IO
               INSPECT SBEL-IO REPLACING ALL ' ' BY '0'
               MOVE KTOKURI-IO-AREA (50:11) TO SVBEL-IO
               INSPECT SVBEL-IO REPLACING ALL ' ' BY '0'
               MOVE KTOKURI-IO-AREA (114:7) TO SBELU-IO
               MOVE KTOKURI-IO-AREA (121:8) TO SVBELU-IO
               MOVE KTOKURI-IO-AREA (187:1) TO SSLETT (1:1)
           WHEN ( KTOKURI-IO-AREA (1:1) = '3'
            AND   KTOKURI-IO-AREA (2:1) = '1' )
               MOVE KTOKURI-IO-AREA (1:2)  TO RA (1:2)
               MOVE KTOKURI-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE KTOKURI-IO-AREA (6:6)  TO RESK (1:6)
               MOVE KTOKURI-IO-AREA (39:9) TO BELO-ELGP-IO
               INSPECT BELO-ELGP-IO REPLACING ALL ' ' BY '0'
               MOVE KTOKURI-IO-AREA (51:10) TO VBELO-ELGP-IO
               INSPECT VBELO-ELGP-IO REPLACING ALL ' ' BY '0'
               MOVE KTOKURI-IO-AREA (114:7) TO BELO-ELGPU-IO
               MOVE KTOKURI-IO-AREA (121:8) TO VBELO-ELGU-IO
               MOVE KTOKURI-IO-AREA (84:6) TO KTOPER-IO
               INSPECT KTOPER-IO REPLACING ALL ' ' BY '0'
               MOVE KTOKURI-IO-AREA (84:4) TO KTOA-ELGR4-IO
               INSPECT KTOA-ELGR4-IO REPLACING ALL ' ' BY '0'
               MOVE KTOKURI-IO-AREA (187:1) TO TSLETT (1:1)
               MOVE KTOKURI-IO-AREA (1:200) TO REC (1:200)
           END-EVALUATE.
 
       KTOKURI-IDCHK SECTION.
       KTOKURI-IDCHK-P.
           EVALUATE TRUE
           WHEN ( KTOKURI-IO-AREA (1:1) = '3'
            AND   KTOKURI-IO-AREA (2:1) = '0' )
             OR ( KTOKURI-IO-AREA (1:1) = '3'
            AND   KTOKURI-IO-AREA (2:1) = '1' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       KTOKURI-IDSET SECTION.
       KTOKURI-IDSET-P.
           EVALUATE TRUE
           WHEN ( KTOKURI-IO-AREA (1:1) = '3'
            AND   KTOKURI-IO-AREA (2:1) = '0' )
               SET I-02                    TO TRUE
           WHEN ( KTOKURI-IO-AREA (1:1) = '3'
            AND   KTOKURI-IO-AREA (2:1) = '1' )
               SET I-03                    TO TRUE
           END-EVALUATE.
 
       KTOKURI-CHK-LEVEL SECTION.
       KTOKURI-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( KTOKURI-IO-AREA (1:1) = '3'
            AND   KTOKURI-IO-AREA (2:1) = '0' )
               MOVE LOW-VALUES             TO KTOKURI-LEVEL-02
               MOVE KTOKURI-IO-AREA (3:3)  TO KTOKURI-02-L5-FIRMA
               MOVE KTOKURI-IO-AREA (6:6)  TO KTOKURI-02-L4-RESK
               IF  KTOKURI-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  KTOKURI-02-L5 NOT = THE-PRIOR-L5
                       PERFORM SETON-I-L5
                   WHEN  KTOKURI-02-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   END-EVALUATE
               END-IF
               MOVE  KTOKURI-02-L5         TO THE-PRIOR-L5
               MOVE  KTOKURI-02-L4         TO THE-PRIOR-L4
               SET KTOKURI-LEVEL-INIT      TO TRUE
           WHEN ( KTOKURI-IO-AREA (1:1) = '3'
            AND   KTOKURI-IO-AREA (2:1) = '1' )
               MOVE LOW-VALUES             TO KTOKURI-LEVEL-03
               MOVE KTOKURI-IO-AREA (3:3)  TO KTOKURI-03-L5-FIRMA
               MOVE KTOKURI-IO-AREA (6:6)  TO KTOKURI-03-L4-RESK
               IF  KTOKURI-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  KTOKURI-03-L5 NOT = THE-PRIOR-L5
                       PERFORM SETON-I-L5
                   WHEN  KTOKURI-03-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   END-EVALUATE
               END-IF
               MOVE  KTOKURI-03-L5         TO THE-PRIOR-L5
               MOVE  KTOKURI-03-L4         TO THE-PRIOR-L4
               SET KTOKURI-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (8:30)  TO FINAVN (1:30)
               MOVE FIRMAF-IO-AREA (123:1) TO FSLETT (1:1)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-04                        TO TRUE.
 
       SYSPARM-FLDSET SECTION.
       SYSPARM-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE SYSPARM-IO-AREA (038:1) TO KODEF (1:1)
           END-EVALUATE.
 
       SYSPARM-IDSET SECTION.
       SYSPARM-IDSET-P.
           SET I-07                        TO TRUE.
 
       PRINT-1-PRINT-LINE SECTION.
       PRINT-1-PRINT-LINE-P.
           IF  PRINT-1-BEFORE-SKIP > 0
               PERFORM PRINT-1-SKIP-BEFORE
           END-IF
           IF  PRINT-1-BEFORE-SPACE > 0
               PERFORM PRINT-1-SPACE-BEFORE
               IF  PRINT-1-AFTER-SKIP > 0
                   PERFORM PRINT-1-SKIP-AFTER
               END-IF
               IF  PRINT-1-AFTER-SPACE > 0
                   PERFORM PRINT-1-SPACE-AFTER
               END-IF
           ELSE
               IF  PRINT-1-AFTER-SKIP > 0
                   PERFORM PRINT-1-SKIP-AFTER
               END-IF
               PERFORM PRINT-1-SPACE-AFTER
           END-IF
           IF  PRINT-1-LINE-COUNT NOT < PRINT-1-MAX-LINES
               SET I-OF                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OF            TO TRUE
               END-IF
           END-IF.
 
       PRINT-1-SKIP-BEFORE SECTION.
       PRINT-1-SKIP-BEFORE-P.
           WRITE PRINT-1-IO-PRINT       AFTER ADVANCING PAGE
           MOVE 1                          TO PRINT-1-LINE-COUNT
           MOVE 0                          TO PRINT-1-BEFORE-SKIP
           INITIALIZE PRINT-1-IO-AREA.
 
       PRINT-1-SPACE-BEFORE SECTION.
       PRINT-1-SPACE-BEFORE-P.
           WRITE PRINT-1-IO-PRINT       AFTER PRINT-1-BEFORE-SPACE
                                                                 LINES
           ADD PRINT-1-BEFORE-SPACE        TO PRINT-1-LINE-COUNT
           MOVE SPACES TO PRINT-1-IO-AREA
           INITIALIZE PRINT-1-IO-AREA
           MOVE 0                          TO PRINT-1-BEFORE-SPACE.
 
       PRINT-1-SKIP-AFTER SECTION.
       PRINT-1-SKIP-AFTER-P.
           WRITE PRINT-1-IO-PRINT      BEFORE ADVANCING PAGE
           MOVE 1                          TO PRINT-1-LINE-COUNT
           MOVE 0                          TO PRINT-1-AFTER-SKIP
           INITIALIZE PRINT-1-IO-AREA.
 
       PRINT-1-SPACE-AFTER SECTION.
       PRINT-1-SPACE-AFTER-P.
           WRITE PRINT-1-IO-PRINT      BEFORE PRINT-1-AFTER-SPACE LINES
           ADD PRINT-1-AFTER-SPACE         TO PRINT-1-LINE-COUNT
           INITIALIZE PRINT-1-IO-AREA
           MOVE 0                          TO PRINT-1-AFTER-SPACE.
 
       PRINT-2-PRINT-LINE SECTION.
       PRINT-2-PRINT-LINE-P.
           IF  PRINT-2-BEFORE-SKIP > 0
               PERFORM PRINT-2-SKIP-BEFORE
           END-IF
           IF  PRINT-2-BEFORE-SPACE > 0
               PERFORM PRINT-2-SPACE-BEFORE
               IF  PRINT-2-AFTER-SKIP > 0
                   PERFORM PRINT-2-SKIP-AFTER
               END-IF
               IF  PRINT-2-AFTER-SPACE > 0
                   PERFORM PRINT-2-SPACE-AFTER
               END-IF
           ELSE
               IF  PRINT-2-AFTER-SKIP > 0
                   PERFORM PRINT-2-SKIP-AFTER
               END-IF
               PERFORM PRINT-2-SPACE-AFTER
           END-IF
           IF  PRINT-2-LINE-COUNT NOT < PRINT-2-MAX-LINES
               MOVE 7                      TO PRINT-2-AFTER-SKIP
           END-IF.
 
       PRINT-2-SKIP-BEFORE SECTION.
       PRINT-2-SKIP-BEFORE-P.
           WRITE PRINT-2-IO-PRINT       AFTER ADVANCING PAGE
           MOVE 1                          TO PRINT-2-LINE-COUNT
           MOVE 0                          TO PRINT-2-BEFORE-SKIP
           INITIALIZE PRINT-2-IO-AREA.
 
       PRINT-2-SPACE-BEFORE SECTION.
       PRINT-2-SPACE-BEFORE-P.
           WRITE PRINT-2-IO-PRINT       AFTER PRINT-2-BEFORE-SPACE
                                                                 LINES
           ADD PRINT-2-BEFORE-SPACE        TO PRINT-2-LINE-COUNT
           MOVE SPACES TO PRINT-2-IO-AREA
           INITIALIZE PRINT-2-IO-AREA
           MOVE 0                          TO PRINT-2-BEFORE-SPACE.
 
       PRINT-2-SKIP-AFTER SECTION.
       PRINT-2-SKIP-AFTER-P.
           WRITE PRINT-2-IO-PRINT      BEFORE ADVANCING PAGE
           MOVE 1                          TO PRINT-2-LINE-COUNT
           MOVE 0                          TO PRINT-2-AFTER-SKIP
           INITIALIZE PRINT-2-IO-AREA.
 
       PRINT-2-SPACE-AFTER SECTION.
       PRINT-2-SPACE-AFTER-P.
           WRITE PRINT-2-IO-PRINT      BEFORE PRINT-2-AFTER-SPACE LINES
           ADD PRINT-2-AFTER-SPACE         TO PRINT-2-LINE-COUNT
           INITIALIZE PRINT-2-IO-AREA
           MOVE 0                          TO PRINT-2-AFTER-SPACE.
 
       BUGFILO-PRINT-LINE SECTION.
       BUGFILO-PRINT-LINE-P.
           IF  BUGFILO-BEFORE-SKIP > 0
               PERFORM BUGFILO-SKIP-BEFORE
           END-IF
           IF  BUGFILO-BEFORE-SPACE > 0
               PERFORM BUGFILO-SPACE-BEFORE
               IF  BUGFILO-AFTER-SKIP > 0
                   PERFORM BUGFILO-SKIP-AFTER
               END-IF
               IF  BUGFILO-AFTER-SPACE > 0
                   PERFORM BUGFILO-SPACE-AFTER
               END-IF
           ELSE
               IF  BUGFILO-AFTER-SKIP > 0
                   PERFORM BUGFILO-SKIP-AFTER
               END-IF
               PERFORM BUGFILO-SPACE-AFTER
           END-IF
           IF  BUGFILO-LINE-COUNT NOT < BUGFILO-MAX-LINES
               MOVE 7                      TO BUGFILO-AFTER-SKIP
           END-IF.
 
       BUGFILO-SKIP-BEFORE SECTION.
       BUGFILO-SKIP-BEFORE-P.
           WRITE BUGFILO-IO-PRINT       AFTER ADVANCING PAGE
           MOVE 1                          TO BUGFILO-LINE-COUNT
           MOVE 0                          TO BUGFILO-BEFORE-SKIP
           INITIALIZE BUGFILO-IO-AREA.
 
       BUGFILO-SPACE-BEFORE SECTION.
       BUGFILO-SPACE-BEFORE-P.
           WRITE BUGFILO-IO-PRINT       AFTER BUGFILO-BEFORE-SPACE
                                                                 LINES
           ADD BUGFILO-BEFORE-SPACE        TO BUGFILO-LINE-COUNT
           MOVE SPACES TO BUGFILO-IO-AREA
           INITIALIZE BUGFILO-IO-AREA
           MOVE 0                          TO BUGFILO-BEFORE-SPACE.
 
       BUGFILO-SKIP-AFTER SECTION.
       BUGFILO-SKIP-AFTER-P.
           WRITE BUGFILO-IO-PRINT      BEFORE ADVANCING PAGE
           MOVE 1                          TO BUGFILO-LINE-COUNT
           MOVE 0                          TO BUGFILO-AFTER-SKIP
           INITIALIZE BUGFILO-IO-AREA.
 
       BUGFILO-SPACE-AFTER SECTION.
       BUGFILO-SPACE-AFTER-P.
           WRITE BUGFILO-IO-PRINT      BEFORE BUGFILO-AFTER-SPACE LINES
           ADD BUGFILO-AFTER-SPACE         TO BUGFILO-LINE-COUNT
           INITIALIZE BUGFILO-IO-AREA
           MOVE 0                          TO BUGFILO-AFTER-SPACE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-03 AND I-85)
               MOVE SPACES TO KTOKURO-IO-AREA
               INITIALIZE KTOKURO-IO-AREA
               MOVE REC                    TO KTOKURO-IO-AREA (1:200)
               MOVE VBELO-ELGP-IO          TO KTOKURO-IO-AREA (51:10)
               MOVE BELO-ELGPU             TO XO-112P
               MOVE XO-112P-EF             TO KTOKURO-IO-AREA (114:7)
               MOVE VBELO-ELGU             TO XO-114P
               MOVE XO-114P-EF             TO KTOKURO-IO-AREA (121:8)
               WRITE KTOKURO-IO-AREA
           END-IF
           IF  (I-03 AND I-37 AND NOT-I-29)
      * LEGGE INN PERIODE I SALDOREC
               MOVE SPACES TO NYTTA-ELGR-IO-AREA
               INITIALIZE NYTTA-ELGR-IO-AREA
               MOVE REC                    TO NYTTA-ELGR-IO-AREA
                                                               (1:200)
               MOVE VBELO-ELGP-IO          TO NYTTA-ELGR-IO-AREA
                                                               (51:10)
               MOVE BELO-ELGPU             TO XO-112P
               MOVE XO-112P-EF             TO NYTTA-ELGR-IO-AREA
                                                               (114:7)
               MOVE VBELO-ELGU             TO XO-114P
               MOVE XO-114P-EF             TO NYTTA-ELGR-IO-AREA
                                                               (121:8)
               MOVE PRDDTO                 TO XO-80P
               MOVE XO-80P-EF              TO NYTTA-ELGR-IO-AREA
                                                               (178:5)
               MOVE PRDKLK                 TO XO-60P
               MOVE XO-60P-EF              TO NYTTA-ELGR-IO-AREA
                                                               (183:4)
               MOVE 'Å'                    TO NYTTA-ELGR-IO-AREA
                                                               (187:1)
               WRITE NYTTA-ELGR-IO-AREA
           END-IF
           IF  (I-L5)
               MOVE SPACES TO PRINT-1-IO-AREA
               INITIALIZE PRINT-1-IO-AREA
               MOVE FIRMA                  TO PRINT-1-IO-AREA (1:3)
               MOVE FINAVN                 TO PRINT-1-IO-AREA (6:30)
               IF  (I-90)
                   MOVE 'SKAL PRINTES NÅ.' TO PRINT-1-IO-AREA (37:16)
               END-IF
               IF  (NOT-I-90)
                   MOVE 'PRINTES IKKE NÅ.' TO PRINT-1-IO-AREA (37:16)
               END-IF
               MOVE 'KTO.KODE ='           TO PRINT-1-IO-AREA (56:10)
               MOVE KODEF                  TO PRINT-1-IO-AREA (67:1)
               IF  (I-20)
                   MOVE '*'                TO PRINT-1-IO-AREA (67:1)
               END-IF
               IF  (I-20)
                   MOVE 'FEIL I FIRMAF         ' TO PRINT-1-IO-AREA
                                                               (69:22)
               END-IF
               IF  (I-36)
                   MOVE 'FEIL I SYSPARM        ' TO PRINT-1-IO-AREA
                                                               (69:22)
               END-IF
               IF  (I-36 AND I-20)
                   MOVE 'FEIL I FIRMAF&SYSPARM ' TO PRINT-1-IO-AREA
                                                               (69:22)
               END-IF
               IF  (I-20)
                   MOVE 'BENYTTER KODE 4.' TO PRINT-1-IO-AREA (92:16)
               END-IF
               MOVE 1                      TO PRINT-1-AFTER-SPACE
               PERFORM PRINT-1-PRINT-LINE
           END-IF.
 
       EXCEPTION-OUTPUT SECTION.
       EXCEPTION-OUTPUT-P.
           MOVE SPACES TO KTOKURO-IO-AREA
           INITIALIZE KTOKURO-IO-AREA
           MOVE '30'                       TO KTOKURO-IO-AREA (1:2)
           MOVE FIRMA                      TO KTOKURO-IO-AREA (3:3)
           MOVE RESK                       TO KTOKURO-IO-AREA (6:6)
           MOVE '80'                       TO KTOKURO-IO-AREA (18:2)
           IF  (I-99 AND NOT-I-71 AND NOT-I-72)
               MOVE ARA (M)                TO KTOKURO-IO-AREA (20:6)
           END-IF
           IF  (I-72)
               MOVE SALDOD                 TO KTOKURO-IO-AREA (20:6)
               INITIALIZE SALDOD
           END-IF
           MOVE FSBEL-IO                   TO KTOKURO-IO-AREA (37:11)
           INITIALIZE FSBEL-IO
           MOVE FSVBEL-IO                  TO KTOKURO-IO-AREA (50:11)
           MOVE FSBELU                     TO XO-112P
           MOVE XO-112P-EF                 TO KTOKURO-IO-AREA (114:7)
           INITIALIZE FSBELU
           MOVE FSVBEU                     TO XO-114P
           MOVE XO-114P-EF                 TO KTOKURO-IO-AREA (121:8)
               WRITE KTOKURO-IO-AREA.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-92)
               MOVE SPACES TO PRINT-1-IO-AREA
               INITIALIZE PRINT-1-IO-AREA
               MOVE 'UTSKRIFTSOVERSIKT FOR KO' TO PRINT-1-IO-AREA
                                                                (1:24)
               MOVE 'KONTOKURANTER'        TO PRINT-1-IO-AREA (25:13)
               MOVE 'DATO:'                TO PRINT-1-IO-AREA (41:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO PRINT-1-IO-AREA (46:8)
               MOVE 'KLOKKE:'              TO PRINT-1-IO-AREA (56:7)
               MOVE KLOKKE                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO PRINT-1-IO-AREA (63:8)
               MOVE 'RKO220'               TO PRINT-1-IO-AREA (75:6)
               MOVE 01                     TO PRINT-1-BEFORE-SKIP
               MOVE 2                      TO PRINT-1-AFTER-SPACE
               PERFORM PRINT-1-PRINT-LINE
               MOVE SPACES TO PRINT-1-IO-AREA
               INITIALIZE PRINT-1-IO-AREA
               MOVE '************************' TO PRINT-1-IO-AREA
                                                                (1:24)
               MOVE '************************' TO PRINT-1-IO-AREA
                                                               (25:24)
               MOVE '********************' TO PRINT-1-IO-AREA (43:20)
               MOVE 1                      TO PRINT-1-AFTER-SPACE
               PERFORM PRINT-1-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO PRINT-1-IO-AREA
               INITIALIZE PRINT-1-IO-AREA
               MOVE 'UTSKRIFTSOVERSIKT FOR KO' TO PRINT-1-IO-AREA
                                                                (1:24)
               MOVE 'KONTOKURANTER'        TO PRINT-1-IO-AREA (25:13)
               MOVE 'DATO:'                TO PRINT-1-IO-AREA (41:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO PRINT-1-IO-AREA (46:8)
               MOVE 'KLOKKE:'              TO PRINT-1-IO-AREA (56:7)
               MOVE KLOKKE                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO PRINT-1-IO-AREA (63:8)
               MOVE 'RKO220'               TO PRINT-1-IO-AREA (75:6)
               MOVE 01                     TO PRINT-1-BEFORE-SKIP
               MOVE 2                      TO PRINT-1-AFTER-SPACE
               PERFORM PRINT-1-PRINT-LINE
               MOVE SPACES TO PRINT-1-IO-AREA
               INITIALIZE PRINT-1-IO-AREA
               MOVE '************************' TO PRINT-1-IO-AREA
                                                                (1:24)
               MOVE '************************' TO PRINT-1-IO-AREA
                                                               (25:24)
               MOVE '********************' TO PRINT-1-IO-AREA (43:20)
               MOVE 1                      TO PRINT-1-AFTER-SPACE
               PERFORM PRINT-1-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L4 AND I-77 AND NOT-I-29)
               MOVE SPACES TO NYTTA-ELGR-IO-AREA
               INITIALIZE NYTTA-ELGR-IO-AREA
               MOVE '30'                   TO NYTTA-ELGR-IO-AREA (1:2)
               MOVE FIRMA                  TO NYTTA-ELGR-IO-AREA (3:3)
               MOVE RESK                   TO NYTTA-ELGR-IO-AREA (6:6)
               MOVE '80'                   TO NYTTA-ELGR-IO-AREA (18:2)
               MOVE ARA (12)               TO NYTTA-ELGR-IO-AREA (20:6)
               MOVE NYSBEL-IO              TO NYTTA-ELGR-IO-AREA
                                                               (37:11)
               MOVE NYSVBE-IO              TO NYTTA-ELGR-IO-AREA
                                                               (50:11)
               MOVE ARH                    TO NYTTA-ELGR-IO-AREA (82:2)
               MOVE ARHAAR                 TO NYTTA-ELGR-IO-AREA (84:4)
               MOVE '12'                   TO NYTTA-ELGR-IO-AREA (88:2)
               MOVE NYSBEU                 TO XO-112P
               MOVE XO-112P-EF             TO NYTTA-ELGR-IO-AREA
                                                               (114:7)
               MOVE NYSVBU                 TO XO-114P
               MOVE XO-114P-EF             TO NYTTA-ELGR-IO-AREA
                                                               (121:8)
               MOVE PRDDTO                 TO XO-80P
               MOVE XO-80P-EF              TO NYTTA-ELGR-IO-AREA
                                                               (178:5)
               MOVE PRDKLK                 TO XO-60P
               MOVE XO-60P-EF              TO NYTTA-ELGR-IO-AREA
                                                               (183:4)
               MOVE 'Å'                    TO NYTTA-ELGR-IO-AREA
                                                               (187:1)
               MOVE 'NYÅR'                 TO NYTTA-ELGR-IO-AREA
                                                               (188:4)
               MOVE PRDDTO                 TO XO-80P
               MOVE XO-80P-EF              TO NYTTA-ELGR-IO-AREA
                                                               (192:5)
               MOVE PRDKLK                 TO XO-60P
               MOVE XO-60P-EF              TO NYTTA-ELGR-IO-AREA
                                                               (197:4)
               WRITE NYTTA-ELGR-IO-AREA
           END-IF
           IF  (I-L4 AND I-86)
               MOVE SPACES TO PERIODE-IO-AREA
               INITIALIZE PERIODE-IO-AREA
               MOVE '5'                    TO PERIODE-IO-AREA (1:1)
               MOVE FIRMA                  TO PERIODE-IO-AREA (2:3)
               MOVE RESK                   TO PERIODE-IO-AREA (5:6)
               MOVE PERD-IO                TO PERIODE-IO-AREA (11:2)
               MOVE PERM-IO                TO PERIODE-IO-AREA (13:2)
               MOVE PERA-ELG-IO            TO PERIODE-IO-AREA (15:2)
               MOVE 89                     TO BW-A
               PERFORM VARYING ARA-I FROM ARA-MAX BY -1
                         UNTIL ARA-I < 1
                   SUBTRACT 6            FROM BW-A
                   MOVE ARA-ENTRY (ARA-I)  TO PERIODE-IO-AREA (BW-A:6)
               END-PERFORM
               WRITE PERIODE-IO-AREA
           END-IF
           IF  (I-LR)
               MOVE SPACES TO AVSTEMO-IO-AREA
               INITIALIZE AVSTEMO-IO-AREA
               MOVE PRDDTO-IO              TO AVSTEMO-IO-AREA (1:8)
               MOVE '040'                  TO AVSTEMO-IO-AREA (9:3)
               MOVE 'MRK'                  TO AVSTEMO-IO-AREA (12:3)
               MOVE 'RKO220'               TO AVSTEMO-IO-AREA (15:6)
               MOVE '*RKO220*'             TO AVSTEMO-IO-AREA (21:8)
               MOVE PRDKLK-IO              TO AVSTEMO-IO-AREA (30:6)
               MOVE LRRAKT-IO              TO AVSTEMO-IO-AREA (36:13)
               MOVE LRRABE-IO              TO AVSTEMO-IO-AREA (49:15)
               MOVE LRRABU-IO              TO AVSTEMO-IO-AREA (64:15)
               MOVE PRDDTO-IO              TO AVSTEMO-IO-AREA (79:8)
               WRITE AVSTEMO-IO-AREA
               MOVE SPACES TO AVSTEMO-IO-AREA
               INITIALIZE AVSTEMO-IO-AREA
               MOVE '********'             TO AVSTEMO-IO-AREA (1:8)
               MOVE '040'                  TO AVSTEMO-IO-AREA (9:3)
               MOVE 'MRK'                  TO AVSTEMO-IO-AREA (12:3)
               MOVE '******'               TO AVSTEMO-IO-AREA (15:6)
               MOVE '*RKO220*'             TO AVSTEMO-IO-AREA (21:8)
               MOVE PRDKLK-IO              TO AVSTEMO-IO-AREA (30:6)
               MOVE LRRAKT-IO              TO AVSTEMO-IO-AREA (36:13)
               MOVE LRRABE-IO              TO AVSTEMO-IO-AREA (49:15)
               MOVE LRRABU-IO              TO AVSTEMO-IO-AREA (64:15)
               MOVE PRDDTO-IO              TO AVSTEMO-IO-AREA (79:8)
               WRITE AVSTEMO-IO-AREA
           END-IF
           IF  (I-LR)
               MOVE SPACES TO PRINT-2-IO-AREA
               INITIALIZE PRINT-2-IO-AREA
               MOVE 'AVSTEMMINGSTOTALER FOR K' TO PRINT-2-IO-AREA
                                                                (1:24)
               MOVE 'ONTOKURANTER'         TO PRINT-2-IO-AREA (25:12)
               MOVE 'DATO:'                TO PRINT-2-IO-AREA (39:5)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO PRINT-2-IO-AREA (44:8)
               MOVE 'KLOKKE:'              TO PRINT-2-IO-AREA (54:7)
               MOVE KLOKKE                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO PRINT-2-IO-AREA (61:8)
               MOVE 'RKO220'               TO PRINT-2-IO-AREA (75:6)
               MOVE 01                     TO PRINT-2-BEFORE-SKIP
               MOVE 3                      TO PRINT-2-BEFORE-SPACE
               MOVE 2                      TO PRINT-2-AFTER-SPACE
               PERFORM PRINT-2-PRINT-LINE
               MOVE SPACES TO PRINT-2-IO-AREA
               INITIALIZE PRINT-2-IO-AREA
               MOVE '************************' TO PRINT-2-IO-AREA
                                                                (1:24)
               MOVE '************************' TO PRINT-2-IO-AREA
                                                               (25:24)
               MOVE '************************' TO PRINT-2-IO-AREA
                                                               (49:24)
               MOVE '************************' TO PRINT-2-IO-AREA
                                                               (57:24)
               MOVE 1                      TO PRINT-2-AFTER-SPACE
               PERFORM PRINT-2-PRINT-LINE
               MOVE SPACES TO PRINT-2-IO-AREA
               INITIALIZE PRINT-2-IO-AREA
               MOVE '*'                    TO PRINT-2-IO-AREA (1:1)
               MOVE '*'                    TO PRINT-2-IO-AREA (80:1)
               MOVE 1                      TO PRINT-2-AFTER-SPACE
               PERFORM PRINT-2-PRINT-LINE
               MOVE SPACES TO PRINT-2-IO-AREA
               INITIALIZE PRINT-2-IO-AREA
               MOVE '*'                    TO PRINT-2-IO-AREA (1:1)
               MOVE '* Avstemming for periode' TO PRINT-2-IO-AREA
                                                                (1:24)
               MOVE ': '                   TO PRINT-2-IO-AREA (25:2)
               MOVE ARHAAR                 TO PRINT-2-IO-AREA (27:4)
               MOVE '/'                    TO PRINT-2-IO-AREA (31:1)
               MOVE MNDNR-IO               TO PRINT-2-IO-AREA (32:2)
               MOVE '*'                    TO PRINT-2-IO-AREA (80:1)
               MOVE 1                      TO PRINT-2-AFTER-SPACE
               PERFORM PRINT-2-PRINT-LINE
               MOVE SPACES TO PRINT-2-IO-AREA
               INITIALIZE PRINT-2-IO-AREA
               MOVE '*'                    TO PRINT-2-IO-AREA (1:1)
               MOVE '*'                    TO PRINT-2-IO-AREA (80:1)
               MOVE 1                      TO PRINT-2-AFTER-SPACE
               PERFORM PRINT-2-PRINT-LINE
               MOVE SPACES TO PRINT-2-IO-AREA
               INITIALIZE PRINT-2-IO-AREA
               MOVE '*'                    TO PRINT-2-IO-AREA (1:1)
               MOVE 'BELØP '               TO PRINT-2-IO-AREA (25:6)
               MOVE 'ANTALL'               TO PRINT-2-IO-AREA (45:6)
               MOVE 'UTV BELØP '           TO PRINT-2-IO-AREA (61:10)
               MOVE '*'                    TO PRINT-2-IO-AREA (80:1)
               MOVE 1                      TO PRINT-2-AFTER-SPACE
               PERFORM PRINT-2-PRINT-LINE
               MOVE SPACES TO PRINT-2-IO-AREA
               INITIALIZE PRINT-2-IO-AREA
               MOVE '*'                    TO PRINT-2-IO-AREA (1:1)
               MOVE '* INN    :'           TO PRINT-2-IO-AREA (1:10)
               MOVE TOTBEL                 TO XO-112YYZR
               MOVE XO-112YYZR             TO PRINT-2-IO-AREA (13:18)
               MOVE ANTALL                 TO XO-90YY9
               MOVE XO-90YY9               TO PRINT-2-IO-AREA (40:11)
               MOVE TOTBEU                 TO XO-112YYZR
               MOVE XO-112YYZR             TO PRINT-2-IO-AREA (53:18)
               MOVE '*'                    TO PRINT-2-IO-AREA (80:1)
               MOVE 1                      TO PRINT-2-AFTER-SPACE
               PERFORM PRINT-2-PRINT-LINE
               MOVE SPACES TO PRINT-2-IO-AREA
               INITIALIZE PRINT-2-IO-AREA
               MOVE '*'                    TO PRINT-2-IO-AREA (1:1)
               MOVE '* SLETTET:'           TO PRINT-2-IO-AREA (1:10)
               MOVE SLEBEL                 TO XO-112YYZR
               MOVE XO-112YYZR             TO PRINT-2-IO-AREA (13:18)
               MOVE ANTSLE                 TO XO-90YY9
               MOVE XO-90YY9               TO PRINT-2-IO-AREA (40:11)
               MOVE SLEBEU                 TO XO-112YYZR
               MOVE XO-112YYZR             TO PRINT-2-IO-AREA (53:18)
               MOVE '1)'                   TO PRINT-2-IO-AREA (72:2)
               MOVE '*'                    TO PRINT-2-IO-AREA (80:1)
               MOVE 1                      TO PRINT-2-AFTER-SPACE
               PERFORM PRINT-2-PRINT-LINE
               MOVE SPACES TO PRINT-2-IO-AREA
               INITIALIZE PRINT-2-IO-AREA
               MOVE '*'                    TO PRINT-2-IO-AREA (1:1)
               MOVE '* AKTIVE :'           TO PRINT-2-IO-AREA (1:10)
               MOVE NETBEL                 TO XO-112YYZR
               MOVE XO-112YYZR             TO PRINT-2-IO-AREA (13:18)
               MOVE ANTAKT                 TO XO-90YY9
               MOVE XO-90YY9               TO PRINT-2-IO-AREA (40:11)
               MOVE NETBEU                 TO XO-112YYZR
               MOVE XO-112YYZR             TO PRINT-2-IO-AREA (53:18)
               MOVE '*'                    TO PRINT-2-IO-AREA (80:1)
               MOVE 1                      TO PRINT-2-AFTER-SPACE
               PERFORM PRINT-2-PRINT-LINE
               MOVE SPACES TO PRINT-2-IO-AREA
               INITIALIZE PRINT-2-IO-AREA
               MOVE '*'                    TO PRINT-2-IO-AREA (1:1)
               MOVE '*'                    TO PRINT-2-IO-AREA (80:1)
               MOVE 1                      TO PRINT-2-AFTER-SPACE
               PERFORM PRINT-2-PRINT-LINE
               MOVE SPACES TO PRINT-2-IO-AREA
               INITIALIZE PRINT-2-IO-AREA
               MOVE '* 1) Slettemerket. Slett' TO PRINT-2-IO-AREA
                                                                (1:24)
               MOVE 'emerkede records vil ikk' TO PRINT-2-IO-AREA
                                                               (25:24)
               MOVE 'e bli overført til nytt ' TO PRINT-2-IO-AREA
                                                               (49:24)
               MOVE 'år.    *'             TO PRINT-2-IO-AREA (73:8)
               MOVE 1                      TO PRINT-2-AFTER-SPACE
               PERFORM PRINT-2-PRINT-LINE
               MOVE SPACES TO PRINT-2-IO-AREA
               INITIALIZE PRINT-2-IO-AREA
               MOVE '*'                    TO PRINT-2-IO-AREA (1:1)
               MOVE '*'                    TO PRINT-2-IO-AREA (80:1)
               MOVE 1                      TO PRINT-2-AFTER-SPACE
               PERFORM PRINT-2-PRINT-LINE
               MOVE SPACES TO PRINT-2-IO-AREA
               INITIALIZE PRINT-2-IO-AREA
               MOVE '* Beløp og antall på lin' TO PRINT-2-IO-AREA
                                                                (1:24)
               MOVE 'jen for AKTIVE transaksj' TO PRINT-2-IO-AREA
                                                               (25:24)
               MOVE 'oner skal stemme med bel' TO PRINT-2-IO-AREA
                                                               (49:24)
               MOVE 'øp og  *'             TO PRINT-2-IO-AREA (73:8)
               MOVE 1                      TO PRINT-2-AFTER-SPACE
               PERFORM PRINT-2-PRINT-LINE
               MOVE SPACES TO PRINT-2-IO-AREA
               INITIALIZE PRINT-2-IO-AREA
               MOVE '* antall fra RKO219 i RE' TO PRINT-2-IO-AREA
                                                                (1:24)
               MOVE 'S50AM (denne jobben).   ' TO PRINT-2-IO-AREA
                                                               (25:24)
               MOVE '*'                    TO PRINT-2-IO-AREA (80:1)
               MOVE 1                      TO PRINT-2-AFTER-SPACE
               PERFORM PRINT-2-PRINT-LINE
               MOVE SPACES TO PRINT-2-IO-AREA
               INITIALIZE PRINT-2-IO-AREA
               MOVE '*'                    TO PRINT-2-IO-AREA (1:1)
               MOVE '*'                    TO PRINT-2-IO-AREA (80:1)
               MOVE 1                      TO PRINT-2-AFTER-SPACE
               PERFORM PRINT-2-PRINT-LINE
               MOVE SPACES TO PRINT-2-IO-AREA
               INITIALIZE PRINT-2-IO-AREA
               MOVE '************************' TO PRINT-2-IO-AREA
                                                                (1:24)
               MOVE '************************' TO PRINT-2-IO-AREA
                                                               (25:24)
               MOVE '************************' TO PRINT-2-IO-AREA
                                                               (49:24)
               MOVE '************************' TO PRINT-2-IO-AREA
                                                               (57:24)
               MOVE 1                      TO PRINT-2-AFTER-SPACE
               PERFORM PRINT-2-PRINT-LINE
           END-IF
           IF  (I-LR AND I-77)
               MOVE SPACES TO PRINT-2-IO-AREA
               INITIALIZE PRINT-2-IO-AREA
               MOVE '* Overføring til nytt år' TO PRINT-2-IO-AREA
                                                                (1:24)
               MOVE '. Saldert pr 31.12.     ' TO PRINT-2-IO-AREA
                                                               (25:24)
               MOVE '*'                    TO PRINT-2-IO-AREA (80:1)
               MOVE 1                      TO PRINT-2-AFTER-SPACE
               PERFORM PRINT-2-PRINT-LINE
               MOVE SPACES TO PRINT-2-IO-AREA
               INITIALIZE PRINT-2-IO-AREA
               MOVE '*'                    TO PRINT-2-IO-AREA (1:1)
               MOVE '*'                    TO PRINT-2-IO-AREA (80:1)
               MOVE 1                      TO PRINT-2-AFTER-SPACE
               PERFORM PRINT-2-PRINT-LINE
               MOVE SPACES TO PRINT-2-IO-AREA
               INITIALIZE PRINT-2-IO-AREA
               MOVE '*'                    TO PRINT-2-IO-AREA (1:1)
               MOVE 'BELØP '               TO PRINT-2-IO-AREA (25:6)
               MOVE 'ANTALL '              TO PRINT-2-IO-AREA (44:7)
               MOVE 'UTV BELØP '           TO PRINT-2-IO-AREA (61:10)
               MOVE '*'                    TO PRINT-2-IO-AREA (80:1)
               MOVE 1                      TO PRINT-2-AFTER-SPACE
               PERFORM PRINT-2-PRINT-LINE
               MOVE SPACES TO PRINT-2-IO-AREA
               INITIALIZE PRINT-2-IO-AREA
               MOVE '*'                    TO PRINT-2-IO-AREA (1:1)
               MOVE '*'                    TO PRINT-2-IO-AREA (80:1)
               MOVE 1                      TO PRINT-2-AFTER-SPACE
               PERFORM PRINT-2-PRINT-LINE
               MOVE SPACES TO PRINT-2-IO-AREA
               INITIALIZE PRINT-2-IO-AREA
               MOVE '*'                    TO PRINT-2-IO-AREA (1:1)
               MOVE '* SALDERT:'           TO PRINT-2-IO-AREA (1:10)
               MOVE SALBNG                 TO XO-92YYZR
               MOVE XO-92YYZR              TO PRINT-2-IO-AREA (16:15)
               MOVE ANTSAL                 TO XO-90YY9
               MOVE XO-90YY9               TO PRINT-2-IO-AREA (40:11)
               MOVE SALBNN                 TO XO-112YYZR
               MOVE XO-112YYZR             TO PRINT-2-IO-AREA (53:18)
               MOVE '*'                    TO PRINT-2-IO-AREA (80:1)
               MOVE 1                      TO PRINT-2-AFTER-SPACE
               PERFORM PRINT-2-PRINT-LINE
               MOVE SPACES TO PRINT-2-IO-AREA
               INITIALIZE PRINT-2-IO-AREA
               MOVE '*'                    TO PRINT-2-IO-AREA (1:1)
               MOVE '* NY IB  :'           TO PRINT-2-IO-AREA (1:10)
               MOVE LRNYBE                 TO XO-112YYZR
               MOVE XO-112YYZR             TO PRINT-2-IO-AREA (13:18)
               MOVE ANYSAL                 TO XO-90YY9
               MOVE XO-90YY9               TO PRINT-2-IO-AREA (40:11)
               MOVE LRNYBU                 TO XO-112YYZR
               MOVE XO-112YYZR             TO PRINT-2-IO-AREA (53:18)
               MOVE ' 1)      *'           TO PRINT-2-IO-AREA (71:10)
               MOVE 1                      TO PRINT-2-AFTER-SPACE
               PERFORM PRINT-2-PRINT-LINE
               MOVE SPACES TO PRINT-2-IO-AREA
               INITIALIZE PRINT-2-IO-AREA
               MOVE '*'                    TO PRINT-2-IO-AREA (1:1)
               MOVE '* TRANSER:'           TO PRINT-2-IO-AREA (1:10)
               MOVE TRABEL                 TO XO-112YYZR
               MOVE XO-112YYZR             TO PRINT-2-IO-AREA (13:18)
               MOVE ANTTRA-ELG             TO XO-90YY9
               MOVE XO-90YY9               TO PRINT-2-IO-AREA (40:11)
               MOVE TRABEU                 TO XO-112YYZR
               MOVE XO-112YYZR             TO PRINT-2-IO-AREA (53:18)
               MOVE ' 2)      *'           TO PRINT-2-IO-AREA (71:10)
               MOVE 1                      TO PRINT-2-AFTER-SPACE
               PERFORM PRINT-2-PRINT-LINE
               MOVE SPACES TO PRINT-2-IO-AREA
               INITIALIZE PRINT-2-IO-AREA
               MOVE '*'                    TO PRINT-2-IO-AREA (1:1)
               MOVE '* NYTT ÅR:'           TO PRINT-2-IO-AREA (1:10)
               MOVE NYA-ELGRBE             TO XO-112YYZR
               MOVE XO-112YYZR             TO PRINT-2-IO-AREA (13:18)
               MOVE NYA-ELGRAN             TO XO-90YY9
               MOVE XO-90YY9               TO PRINT-2-IO-AREA (40:11)
               MOVE NYA-ELGRBU             TO XO-112YYZR
               MOVE XO-112YYZR             TO PRINT-2-IO-AREA (53:18)
               MOVE '*'                    TO PRINT-2-IO-AREA (80:1)
               MOVE 1                      TO PRINT-2-AFTER-SPACE
               PERFORM PRINT-2-PRINT-LINE
               MOVE SPACES TO PRINT-2-IO-AREA
               INITIALIZE PRINT-2-IO-AREA
               MOVE '*'                    TO PRINT-2-IO-AREA (1:1)
               MOVE '*'                    TO PRINT-2-IO-AREA (80:1)
               MOVE 1                      TO PRINT-2-AFTER-SPACE
               PERFORM PRINT-2-PRINT-LINE
               MOVE SPACES TO PRINT-2-IO-AREA
               INITIALIZE PRINT-2-IO-AREA
               MOVE '* Beløp på linjen for NY' TO PRINT-2-IO-AREA
                                                                (1:24)
               MOVE 'TT ÅR skal stemme med RK' TO PRINT-2-IO-AREA
                                                               (25:24)
               MOVE '003 i DOP12UD påfølgende' TO PRINT-2-IO-AREA
                                                               (49:24)
               MOVE '*'                    TO PRINT-2-IO-AREA (80:1)
               MOVE 1                      TO PRINT-2-AFTER-SPACE
               PERFORM PRINT-2-PRINT-LINE
               MOVE SPACES TO PRINT-2-IO-AREA
               INITIALIZE PRINT-2-IO-AREA
               MOVE '* arbeidsdag.           ' TO PRINT-2-IO-AREA
                                                                (1:24)
               MOVE '*'                    TO PRINT-2-IO-AREA (80:1)
               MOVE 1                      TO PRINT-2-AFTER-SPACE
               PERFORM PRINT-2-PRINT-LINE
               MOVE SPACES TO PRINT-2-IO-AREA
               INITIALIZE PRINT-2-IO-AREA
               MOVE '* 1) Saldorecords art 30' TO PRINT-2-IO-AREA
                                                                (1:24)
               MOVE '*'                    TO PRINT-2-IO-AREA (80:1)
               MOVE 1                      TO PRINT-2-AFTER-SPACE
               PERFORM PRINT-2-PRINT-LINE
               MOVE SPACES TO PRINT-2-IO-AREA
               INITIALIZE PRINT-2-IO-AREA
               MOVE '* 2) Transrecords art 31' TO PRINT-2-IO-AREA
                                                                (1:24)
               MOVE '*'                    TO PRINT-2-IO-AREA (80:1)
               MOVE 1                      TO PRINT-2-AFTER-SPACE
               PERFORM PRINT-2-PRINT-LINE
               MOVE SPACES TO PRINT-2-IO-AREA
               INITIALIZE PRINT-2-IO-AREA
               MOVE '************************' TO PRINT-2-IO-AREA
                                                                (1:24)
               MOVE '************************' TO PRINT-2-IO-AREA
                                                               (25:24)
               MOVE '************************' TO PRINT-2-IO-AREA
                                                               (49:24)
               MOVE '************************' TO PRINT-2-IO-AREA
                                                               (57:24)
      ****************************************************************
      * DUMMY-LINJE FOR Å FJERNE FEILMELDING.                        *
      ****************************************************************
               MOVE 1                      TO PRINT-2-AFTER-SPACE
               PERFORM PRINT-2-PRINT-LINE
           END-IF
           IF  (I-LR AND I-U8 AND I-U7)
           AND (I-U6 AND I-U5 AND I-U4)
           AND (I-04 AND I-07)
               MOVE SPACES TO PRINT-2-IO-AREA
               INITIALIZE PRINT-2-IO-AREA
               MOVE HJNUM4-IO              TO PRINT-2-IO-AREA (117:4)
               MOVE 1                      TO PRINT-2-AFTER-SPACE
               PERFORM PRINT-2-PRINT-LINE
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
           INITIALIZE REGPAR-DATA-FIELDS
           SET REGPAR-EOF-OFF              TO TRUE
           SET REGPAR-PROCESS              TO TRUE
           OPEN INPUT REGPAR
           SET KTOKURI-LEVEL-INIT          TO TRUE
           INITIALIZE KTOKURI-DATA-FIELDS
           SET KTOKURI-EOF-OFF             TO TRUE
           SET KTOKURI-PROCESS             TO TRUE
           OPEN INPUT KTOKURI
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           INITIALIZE SYSPARM-DATA-FIELDS
           OPEN INPUT SYSPARM
           OPEN OUTPUT PRINT-1
           INITIALIZE PRINT-1-IO-AREA
           INITIALIZE PRINT-1-DATA-FIELDS
           MOVE 57                         TO PRINT-1-MAX-LINES
           OPEN OUTPUT PRINT-2
           INITIALIZE PRINT-2-IO-AREA
           INITIALIZE PRINT-2-DATA-FIELDS
           MOVE 57                         TO PRINT-2-MAX-LINES
           OPEN OUTPUT KTOKURO
           OPEN OUTPUT NYTTA-ELGR
           OPEN OUTPUT PERIODE
           OPEN OUTPUT AVSTEMO
           IF I-U8
               OPEN OUTPUT BUGFILO
           END-IF
           INITIALIZE BUGFILO-IO-AREA
           INITIALIZE BUGFILO-DATA-FIELDS
           MOVE 57                         TO BUGFILO-MAX-LINES.
           PERFORM VARYING ARA-I FROM 1 BY 1
                     UNTIL ARA-I > ARA-MAX
               INITIALIZE ARA (ARA-I)
           END-PERFORM
           SET ARA-I                       TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE REGPAR
           CLOSE KTOKURI
           CLOSE FIRMAF
           CLOSE SYSPARM
           IF PRINT-1-IO-AREA NOT = SPACES
             WRITE PRINT-1-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO PRINT-1-IO-AREA
           END-IF
           CLOSE PRINT-1
           IF PRINT-2-IO-AREA NOT = SPACES
             WRITE PRINT-2-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO PRINT-2-IO-AREA
           END-IF
           CLOSE PRINT-2
           CLOSE KTOKURO
           CLOSE NYTTA-ELGR
           CLOSE PERIODE
           CLOSE AVSTEMO
           IF I-U8
               CLOSE BUGFILO
           END-IF.
 
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
