       IDENTIFICATION DIVISION.
       PROGRAM-ID. RKO230R.
      **********************************************  Z-WIN-RPG2   ****
      *    KONV. IFRA RSK230 UTVIDET RECORD.     ***TXT***ok ss***    *
      *                      MEN IKKE TEKSTFELTET                   MT*
      **OBS ved endring excel på Report Web *****************
      * PROGRAM  RKO230                                                *       *
      * UTSKRIFT AV KONTOKURRANT PÅ DIVERSE KONSERNSELSKAPER           *       *
      * E 21.07.98: HENTER VALUTA-KODE FRA SYSPARM I STEDET FOR FIRMAF.*
      * E 15.10.03: SKRIVER RAPPORTFORSIDE MED RBS.                    *
      * E 26.03.07 BRUKER TEKST FRA RESKONTROFILEN                     *
      * E 23.05.08 NYSAL ER UTVIDET TIL 11 MED 2 DESIMALER          EN *
      * E 30.10.08 LAGT UT ORGNR PÅ LEVERANDØR FRA KUNDEMX          MT *
      * E 29.04.10 FLYTTET POSTSTED EN POS TIL HØYRE                MT *
      * E 11.05.10 Navn og Adr med * foran i pos 1 og 2             EN *
      * E 04.08.10 Navn og Adr med = foran i pos 1 og 2             EN *
      * E 11.08.10 TAR MED VALUTASALDO                              MT *
      *            LAGET EGEN KOLONN MED VALUTATYPE OG BELØP        MT *
      * E 13.08.10 ...OG UTVIDET TEKSTFELTET....                    MT *
      **************************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RKO230.rpg
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
            IS SKIP-CHANNEL-1
            IS SKIP-CHANNEL-2
            IS SKIP-CHANNEL-3
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
           SELECT PERDATO
               ASSIGN TO UT-S-PERDATO
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PERDATO-STATUS.
           SELECT RESKF
               ASSIGN TO UT-S-RESKF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RESKF-STATUS.
           SELECT KUNDEMA
               ASSIGN TO KUNDEMA
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMA-STATUS
               RECORD KEY IS KUNDEMA-KEY1.
           SELECT KUNDEMX
               ASSIGN TO KUNDEMX
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMX-STATUS
               RECORD KEY IS KUNDEMX-KEY1.
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
           SELECT KONTOK
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KONTOK-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD PERDATO
               BLOCK CONTAINS 4050
               RECORD CONTAINS 90.
       01  PERDATO-IO-AREA.
           05  PERDATO-IO-AREA-X           PICTURE X(90).
       FD RESKF
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  RESKF-IO-AREA.
           05  RESKF-IO-AREA-X             PICTURE X(200).
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1            PICTURE X(9).
               10  FILLER                  PICTURE X(190).
       FD KUNDEMX
               RECORD CONTAINS 200.
       01  KUNDEMX-IO-AREA.
           05  KUNDEMX-IO-AREA-X.
               10  KUNDEMX-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
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
      *BUGFILO O   F  80  80            PRINTERSYSLST
       FD KONTOK
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  KONTOK-IO-PRINT.
           05  KONTOK-IO-AREA-CONTROL      PICTURE X VALUE ' '.
        02 KONTOK-IO-AREA.
           05  KONTOK-IO-AREA-X            PICTURE X(132).
       WORKING-STORAGE SECTION.
       77  ARA-MAX   VALUE 12              PICTURE 9(4) USAGE BINARY.
       77  ARB-MAX   VALUE 12              PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  ARA-TABLE.
               10  ARA-ENTRY
                                           OCCURS 12 TIMES
                                           INDEXED BY ARA-I
                                                      ARA-S
                                                      ARB-I
                                                      ARB-S.
                   15  ARA                 PICTURE X(4).
                   15  ARB                 PICTURE X(2).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  PERDATO-STATUS              PICTURE 99 VALUE 0.
           10  RESKF-STATUS                PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  KUNDEMX-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  SYSPARM-STATUS              PICTURE 99 VALUE 0.
           10  KONTOK-STATUS               PICTURE 99 VALUE 0.
           10  TKDATA-XX-STATUS            PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
           10  VALPAR-XX-STATUS            PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  PERDATO-EOF-OFF         VALUE '0'.
               88  PERDATO-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PERDATO-READ-OFF        VALUE '0'.
               88  PERDATO-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PERDATO-PROCESS-OFF     VALUE '0'.
               88  PERDATO-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKF-EOF-OFF           VALUE '0'.
               88  RESKF-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKF-READ-OFF          VALUE '0'.
               88  RESKF-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKF-PROCESS-OFF       VALUE '0'.
               88  RESKF-PROCESS           VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  RESKF-LEVEL-INIT-OFF    VALUE '0'.
               88  RESKF-LEVEL-INIT        VALUE '1'.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  KUNDEMX-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  SYSPARM-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  KONTOK-DATA-FIELDS.
               10  KONTOK-AFTER-SPACE      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  KONTOK-AFTER-SKIP       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  KONTOK-BEFORE-SPACE     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  KONTOK-BEFORE-SKIP      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  KONTOK-MAX-LINES        PICTURE 9(4) BINARY
                                           VALUE 0.
               10  KONTOK-LINE-COUNT       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  KONTOK-CLR-IO           PICTURE X VALUE 'Y'.
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
           05  TKDATA-XX-DATA-FIELDS.
               10  TKTK                    PICTURE X(2).
               10  FILLER                  PICTURE X(255).
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(2).
               10  TKTEXT                  PICTURE X(7).
               10  FILLER                  PICTURE X(248).
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(9).
               10  TKBILN                  PICTURE X(6).
               10  FILLER                  PICTURE X(242).
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(15).
               10  TKREFN                  PICTURE X(6).
               10  FILLER                  PICTURE X(236).
           05  LDATA-XX REDEFINES TKDATA-XX-DATA-FIELDS.
               10  LONR                    PICTURE X(5).
               10  FILLER                  PICTURE X(252).
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(5).
               10  LFIRMA                  PICTURE X(3).
               10  FILLER                  PICTURE X(249).
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(8).
               10  LUNDGR                  PICTURE X(3).
               10  FILLER                  PICTURE X(246).
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(11).
               10  LPROG                   PICTURE X(8).
               10  FILLER                  PICTURE X(238).
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(19).
               10  LANTX-IO.
                   15  LANTX               PICTURE S9(3).
               10  FILLER                  PICTURE X(235).
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(22).
               10  FIRNVN                  PICTURE X(30).
               10  FILLER                  PICTURE X(205).
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(87).
               10  LPRIID                  PICTURE X(4).
               10  FILLER                  PICTURE X(166).
      *     *  BESTILLINGSOPPGAVER (OVERSTYRING AV RBS-FILE) *    *
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(91).
               10  BJOBB                   PICTURE X(8).
               10  FILLER                  PICTURE X(158).
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(99).
               10  BBEST                   PICTURE X(1).
               10  FILLER                  PICTURE X(157).
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(100).
               10  BFILL1                  PICTURE X(50).
               10  FILLER                  PICTURE X(107).
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(150).
               10  BFILL2                  PICTURE X(50).
               10  FILLER                  PICTURE X(57).
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(200).
               10  BFILL3                  PICTURE X(57).
      * * END - RBS - DATASTRUKTUR FOR SUB-PROGRAM RBSH01 ********
      * * START - DATASTRUKTUR FOR SUB-PROGRAM ADVALUTA ********
           05  VALPAR-XX REDEFINES TKDATA-XX-DATA-FIELDS.
               10  AKSEKD                  PICTURE X(1).
               10  FILLER                  PICTURE X(256).
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(1).
               10  ADVK                    PICTURE X(1).
               10  FILLER                  PICTURE X(255).
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(2).
               10  STDVK                   PICTURE X(3).
               10  FILLER                  PICTURE X(252).
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(5).
               10  NORVN                   PICTURE X(20).
               10  FILLER                  PICTURE X(232).
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(25).
               10  STDVN                   PICTURE X(20).
               10  FILLER                  PICTURE X(212).
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(45).
               10  LANDKD                  PICTURE X(2).
               10  FILLER                  PICTURE X(210).
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(47).
               10  LAND                    PICTURE X(20).
               10  FILLER                  PICTURE X(190).
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(67).
               10  VALIX-IO.
                   15  VALIX               PICTURE S9(3).
               10  FILLER                  PICTURE X(187).
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(70).
               10  DIV                     PICTURE X(10).
               10  FILLER                  PICTURE X(177).
      * * END - DATASTRUKTUR FOR SUB-PROGRAM ADVALUTA ********
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
               10  FILLER                  PICTURE X(91).
               10  BJOBB                   PICTURE X(8).
               10  FILLER                  PICTURE X(158).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(99).
               10  BBEST                   PICTURE X(1).
               10  FILLER                  PICTURE X(157).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(100).
               10  BFILL1                  PICTURE X(50).
               10  FILLER                  PICTURE X(107).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(150).
               10  BFILL2                  PICTURE X(50).
               10  FILLER                  PICTURE X(57).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(200).
               10  BFILL3                  PICTURE X(57).
      * * END - RBS - DATASTRUKTUR FOR SUB-PROGRAM RBSH01 ********
      * * START - DATASTRUKTUR FOR SUB-PROGRAM ADVALUTA ********
           05  VALPAR-XX REDEFINES LDATA-XX-DATA-FIELDS.
               10  AKSEKD                  PICTURE X(1).
               10  FILLER                  PICTURE X(256).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(1).
               10  ADVK                    PICTURE X(1).
               10  FILLER                  PICTURE X(255).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(2).
               10  STDVK                   PICTURE X(3).
               10  FILLER                  PICTURE X(252).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(5).
               10  NORVN                   PICTURE X(20).
               10  FILLER                  PICTURE X(232).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(25).
               10  STDVN                   PICTURE X(20).
               10  FILLER                  PICTURE X(212).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(45).
               10  LANDKD                  PICTURE X(2).
               10  FILLER                  PICTURE X(210).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(47).
               10  LAND                    PICTURE X(20).
               10  FILLER                  PICTURE X(190).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(67).
               10  VALIX-IO.
                   15  VALIX               PICTURE S9(3).
               10  FILLER                  PICTURE X(187).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(70).
               10  DIV                     PICTURE X(10).
               10  FILLER                  PICTURE X(177).
      * * END - DATASTRUKTUR FOR SUB-PROGRAM ADVALUTA ********
      *DSDS: DATA STRUCTURE FIELDS
           05  VALPAR-XX-DATA-FIELDS.
               10  AKSEKD                  PICTURE X(1).
               10  FILLER                  PICTURE X(79).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(1).
               10  ADVK                    PICTURE X(1).
               10  FILLER                  PICTURE X(78).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(2).
               10  STDVK                   PICTURE X(3).
               10  FILLER                  PICTURE X(75).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(5).
               10  NORVN                   PICTURE X(20).
               10  FILLER                  PICTURE X(55).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(25).
               10  STDVN                   PICTURE X(20).
               10  FILLER                  PICTURE X(35).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(45).
               10  LANDKD                  PICTURE X(2).
               10  FILLER                  PICTURE X(33).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(47).
               10  LAND                    PICTURE X(20).
               10  FILLER                  PICTURE X(13).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(67).
               10  VALIX-IO.
                   15  VALIX               PICTURE S9(3).
               10  FILLER                  PICTURE X(10).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(70).
               10  DIV                     PICTURE X(10).
      * * END - DATASTRUKTUR FOR SUB-PROGRAM ADVALUTA ********
           05  PERDATO-DATA-FIELDS.
               10  PFIRM                   PICTURE X(3).
               10  PRESK                   PICTURE X(6).
               10  DATO-IO.
                   15  DATO                PICTURE S9(6).
           05  PERDATO-MP                  PICTURE X(9).
           05  PERDATO-MC                  PICTURE X(9).
           05  PERDATO-M-09            REDEFINES PERDATO-MC.
               10  PERDATO-M-09-M2.
                   15  PERDATO-M-09-M2-PFIRM-G.
                       20  PERDATO-M-09-M2-PFIRM PICTURE X(3).
               10  PERDATO-M-09-M1.
                   15  PERDATO-M-09-M1-PRESK-G.
                       20  PERDATO-M-09-M1-PRESK PICTURE X(6).
           05  RESKF-LEVEL-01.
               10  RESKF-01-L4.
                   15  RESKF-01-L4-FIRMA   PICTURE X(3).
               10  RESKF-01-L3.
                   15  RESKF-01-L3-RESK1   PICTURE X(1).
               10  RESKF-01-L2.
                   15  RESKF-01-L2-RESKNR  PICTURE X(6).
           05  RESKF-LEVEL-02.
               10  RESKF-02-L4.
                   15  RESKF-02-L4-FIRMA   PICTURE X(3).
               10  RESKF-02-L3.
                   15  RESKF-02-L3-RESK1   PICTURE X(1).
               10  RESKF-02-L2.
                   15  RESKF-02-L2-RESKNR  PICTURE X(6).
               10  RESKF-02-L1.
                   15  RESKF-02-L1-BILNR   PICTURE X(6).
           05  RESKF-DATA-FIELDS.
               10  REC132                  PICTURE X(132).
               10  FIRMA                   PICTURE X(3).
               10  RESK1                   PICTURE X(1).
               10  RESKNR                  PICTURE X(6).
               10  RESKEY                  PICTURE X(9).
               10  STK                     PICTURE X(2).
               10  SA-ELGR                 PICTURE X(2).
               10  SMND                    PICTURE X(2).
               10  SDAG                    PICTURE X(2).
               10  SBEL-IO.
                   15  SBEL                PICTURE S9(8)V9(2).
               10  SBELV-IO.
                   15  SBELV               PICTURE S9(9)V9(2).
               10  INGBEV                  PICTURE X(1).
               10  TRKODE                  PICTURE X(2).
               10  BILA-ELGR               PICTURE X(2).
               10  BILMND                  PICTURE X(2).
               10  BILDAG                  PICTURE X(2).
               10  BILNR                   PICTURE X(6).
               10  REFNR                   PICTURE X(6).
               10  FORA-ELGR               PICTURE X(2).
               10  FORMND                  PICTURE X(2).
               10  FORDAG                  PICTURE X(2).
               10  BELO-ELGP-IO.
                   15  BELO-ELGP           PICTURE S9(7)V9(2).
               10  VALUTA-IO.
                   15  VALUTA              PICTURE S9(8)V9(2).
               10  BILART                  PICTURE X(1).
               10  VT                      PICTURE X(1).
               10  REGA-ELGM               PICTURE X(4).
               10  REGMND                  PICTURE X(2).
               10  TEKSTR                  PICTURE X(24).
           05  RESKF-MP                    PICTURE X(9).
           05  RESKF-MC                    PICTURE X(9).
           05  RESKF-M-01              REDEFINES RESKF-MC.
               10  RESKF-M-01-M2.
                   15  RESKF-M-01-M2-FIRMA-G.
                       20  RESKF-M-01-M2-FIRMA PICTURE X(3).
               10  RESKF-M-01-M1.
                   15  RESKF-M-01-M1-RESKNR-G.
                       20  RESKF-M-01-M1-RESKNR PICTURE X(6).
           05  RESKF-M-02              REDEFINES RESKF-MC.
               10  RESKF-M-02-M2.
                   15  RESKF-M-02-M2-FIRMA-G.
                       20  RESKF-M-02-M2-FIRMA PICTURE X(3).
               10  RESKF-M-02-M1.
                   15  RESKF-M-02-M1-RESKNR-G.
                       20  RESKF-M-02-M1-RESKNR PICTURE X(6).
           05  KUNDEMA-DATA-FIELDS.
               10  HAND1                   PICTURE X(1).
               10  NAVN1                   PICTURE X(30).
      *                                      46  75 NAVN2           10
               10  NAVN2                   PICTURE X(30).
      *                                      76 105 ADRES           11
               10  ADRES                   PICTURE X(30).
               10  PSTED                   PICTURE X(15).
               10  PNR                     PICTURE X(4).
               10  BM                      PICTURE X(2).
               10  KRGR-IO.
                   15  KRGR                PICTURE S9(4).
               10  ALTNR                   PICTURE X(6).
           05  KUNDEMX-DATA-FIELDS.
               10  KORGNR                  PICTURE X(9).
      ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *****
           05  FIRMAF-DATA-FIELDS.
               10  HEAD1                   PICTURE X(50).
               10  HEAD2                   PICTURE X(50).
               10  HEAD3                   PICTURE X(50).
               10  HEAD4                   PICTURE X(50).
      *                                     116 116 VKODE
           05  SYSPARM-DATA-FIELDS.
               10  VKODE                   PICTURE X(1).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L4            PICTURE X(3).
               10  THE-PRIOR-L3            PICTURE X(1).
               10  THE-PRIOR-L2            PICTURE X(6).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  SYSKEY                  PICTURE X(10).
               10  KUXKEY                  PICTURE X(10).
               10  VALTYP                  PICTURE X(3).
               10  KEYARA                  PICTURE X(4).
               10  X-IO.
                   15  X                   PICTURE S9(2).
               10  DAG                     PICTURE X(2).
               10  FA-ELGM                 PICTURE X(4).
               10  FMND                    PICTURE X(2).
               10  NYSAL-IO.
                   15  NYSAL               PICTURE S9(9)V9(2).
               10  NYSALV-IO.
                   15  NYSALV              PICTURE S9(9)V9(2).
               10  TOTBEV-IO.
                   15  TOTBEV              PICTURE S9(10)V9(2).
               10  SUBBEV-IO.
                   15  SUBBEV              PICTURE S9(10)V9(2).
               10  BILSUM-IO.
                   15  BILSUM              PICTURE S9(7)V9(2).
               10  VALSUM-IO.
                   15  VALSUM              PICTURE S9(8)V9(2).
               10  TEKST                   PICTURE X(24).
           05  EDITTING-FIELDS.
               10  XO-40YNZ                PICTURE ZZZZ.
               10  XO-82YY9                PICTURE ZZ.ZZZ.ZZZ,99.
               10  XO-92YY9R               PICTURE ZZZ.ZZZ.ZZZ,99-.
               10  XO-72YY9                PICTURE Z.ZZZ.ZZZ,99.
               10  XO-82YY9R               PICTURE ZZ.ZZZ.ZZZ,99-.
               10  XO-72YY9R               PICTURE Z.ZZZ.ZZZ,99-.
               10  XO-82YYZR               PICTURE ZZ.ZZZ.ZZZ,ZZ-.
               10  XO-92YY9                PICTURE ZZZ.ZZZ.ZZZ,99.
               10  XO-102YY9R              PICTURE Z.ZZZ.ZZZ.ZZZ,99-.
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
           SET NOT-I-09                    TO TRUE
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-14                    TO TRUE
           SET NOT-I-06                    TO TRUE
           SET NOT-I-12                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  PERDATO-PROCESS
               SET PERDATO-PROCESS-OFF     TO TRUE
               SET PERDATO-READ            TO TRUE
           END-IF
 
           IF  PERDATO-READ
               PERFORM PERDATO-GET
               SET PERDATO-READ-OFF        TO TRUE
               IF  NOT PERDATO-EOF
                   PERFORM PERDATO-MATCH-SET
               END-IF
           END-IF
 
           IF  RESKF-PROCESS
               SET RESKF-PROCESS-OFF       TO TRUE
               SET RESKF-READ              TO TRUE
           END-IF
 
           IF  RESKF-READ
               PERFORM RESKF-GET
               SET RESKF-READ-OFF          TO TRUE
               IF  NOT RESKF-EOF
                   PERFORM RESKF-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM RESKF-MATCH-SET
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
 
           IF  PERDATO-PROCESS
               PERFORM PERDATO-IDSET
           END-IF
 
           IF  RESKF-PROCESS
               PERFORM RESKF-IDSET
           END-IF
 
           IF  RESKF-PROCESS
               PERFORM RESKF-CHK-LEVEL
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
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  PERDATO-PROCESS
               PERFORM PERDATO-FLDSET
           END-IF
 
           IF  RESKF-PROCESS
               PERFORM RESKF-FLDOFF
               PERFORM RESKF-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  RESKF-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-09)
               GO TO UT-T
           END-IF
           IF  (I-L4)
               SUBTRACT TOTBEV             FROM TOTBEV
           END-IF
           IF  (I-L3)
               SUBTRACT SUBBEV             FROM SUBBEV
           END-IF
           IF  (I-L2)
               SUBTRACT NYSAL              FROM NYSAL
               SUBTRACT NYSALV             FROM NYSALV
           END-IF
           IF  (I-L1)
               SUBTRACT BILSUM             FROM BILSUM
               SUBTRACT VALSUM             FROM VALSUM
               SET NOT-I-60                TO TRUE
               SET NOT-I-36                TO TRUE
           END-IF
           IF  (I-L2)
               SET NOT-I-18                TO TRUE
               SET NOT-I-19                TO TRUE
               SET NOT-I-20                TO TRUE
               SET NOT-I-16                TO TRUE
               SET NOT-I-95                TO TRUE
               SET NOT-I-30                TO TRUE
           END-IF
           IF  (I-L4)
               SET NOT-I-25                TO TRUE
           END-IF
           SET NOT-I-41                    TO TRUE
           SET NOT-I-42                    TO TRUE
           SET NOT-I-43                    TO TRUE
           IF  (I-01)
               SET NOT-I-95                TO TRUE
               IF  INGBEV = '*'
                   SET I-95                TO TRUE
               END-IF
           END-IF
           IF  (I-L4)
               SET NOT-I-90                TO TRUE
               SET NOT-I-LR                TO TRUE
               SET NOT-I-96                TO TRUE
               IF  FIRMA > '001'
                   SET I-90                TO TRUE
               END-IF
               IF  FIRMA < '001'
                   SET I-LR                TO TRUE
               END-IF
               IF  FIRMA = '001'
                   SET I-96                TO TRUE
               END-IF
           END-IF
           IF  (I-L4 AND I-96)
               SET I-90                    TO TRUE
           END-IF
           IF  (I-L4)
               MOVE FIRMA                  TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-22                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-22            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
               MOVE FIRMA                  TO SYSKEY (1:3)
               MOVE 'REGA011'              TO SYSKEY (4:7)
               MOVE SYSKEY                 TO SYSPARM-KEY1
               READ SYSPARM RECORD KEY IS SYSPARM-KEY1
               INVALID KEY
                   SET I-22                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-22            TO TRUE
                   PERFORM SYSPARM-FLDSET
                   PERFORM SYSPARM-IDSET
               END-READ
           END-IF
           IF  (I-L4 AND NOT-I-22)
               SET NOT-I-25                TO TRUE
               IF  VKODE = 'J'
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-L4)
               PERFORM RBSRUT-S
           END-IF
           IF  (I-L2)
               MOVE RESKEY                 TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-17                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-17            TO TRUE
                   PERFORM KUNDEMA-IDCHK
                   PERFORM KUNDEMA-FLDOFF
                   PERFORM KUNDEMA-FLDSET
                   PERFORM KUNDEMA-IDSET
               END-READ
           END-IF
           IF  (I-L2 AND NOT-I-17)
               SET I-16                    TO TRUE
           END-IF
           IF  (I-L2)
               MOVE 0                      TO PAGE0
               SET NOT-I-18                TO TRUE
               IF  RESK1 = '9'
                   SET I-18                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-18)
               MOVE RESKEY                 TO KUXKEY (1:9)
               MOVE '1'                    TO KUXKEY (10:1)
               MOVE KUXKEY                 TO KUNDEMX-KEY1
               READ KUNDEMX RECORD KEY IS KUNDEMX-KEY1
               INVALID KEY
                   SET I-21                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-21            TO TRUE
                   PERFORM KUNDEMX-FLDOFF
                   PERFORM KUNDEMX-FLDSET
                   PERFORM KUNDEMX-IDSET
               END-READ
           END-IF
           IF  (I-L2 AND NOT-I-17)
               SET NOT-I-19                TO TRUE
               IF  HAND1 = '0'
                   SET I-19                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-18 AND I-19)
               AND (I-25)
               SET I-20                    TO TRUE
           END-IF
           IF  (I-02)
               MOVE '   '                  TO VALTYP
      *  02 20   VT        COMP " "                  23
           END-IF
           IF  (I-02 AND I-20)
               PERFORM VALRUT-S
      *  02                MOVE "REC132  "BUGFL1  8         DISPLAY FIELD
      *  02      BUGFL1    DEBUGBUGFILO   REC132            VIS INDIKATOR
      *  02                MOVE "VALTYP  "BUGFL1  8         DISPLAY FIELD
      *  02      BUGFL1    DEBUGBUGFILO   VALTYP            VIS INDIKATOR
           END-IF
           IF  (I-01)
               ADD SBEL                    TO NYSAL
               SET NOT-I-72                TO TRUE
               IF  NYSAL < 0
                   SET I-72                TO TRUE
               END-IF
               ADD SBELV                   TO NYSALV
               ADD SBEL                    TO TOTBEV
               ADD SBEL                    TO SUBBEV
               GO TO UT-T
      ***
           END-IF
           IF  (I-02 AND I-30)
               SET NOT-I-31                TO TRUE
               IF  FA-ELGM = REGA-ELGM
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (I-02)
               MOVE FA-ELGM                TO KEYARA
               MOVE 1                      TO X
           END-IF
           IF  (I-02 AND NOT-I-31 AND I-30)
               SET NOT-I-36                TO TRUE
               SET ARA-S                   TO 1
               PERFORM WITH TEST AFTER
                       VARYING ARA-I FROM X BY 1
                         UNTIL ARA-I >= ARA-MAX
                            OR I-36
                   IF  KEYARA = ARA (ARA-I)
                       SET I-36            TO TRUE
                       SET ARA-S           TO ARA-I
                   END-IF
               END-PERFORM
               SET X                       TO ARA-S
           END-IF
           IF  (I-02 AND NOT-I-31 AND I-36)
               MOVE ARB (X)                TO DAG
           END-IF
           IF  (I-02 AND NOT-I-31 AND I-30)
               PERFORM EXCEPTION-OUTPUT
           END-IF
           IF  (I-02 AND NOT-I-30)
               SET I-30                    TO TRUE
           END-IF
           IF  (I-02)
               MOVE REGA-ELGM              TO FA-ELGM
               MOVE REGMND                 TO FMND
      ***
           END-IF
           IF  (I-02)
               ADD BELO-ELGP               TO NYSAL
               ADD VALUTA                  TO NYSALV
               SET NOT-I-72                TO TRUE
               IF  NYSAL < 0,00
                   SET I-72                TO TRUE
               END-IF
               ADD BELO-ELGP               TO TOTBEV
               ADD BELO-ELGP               TO SUBBEV
      ******************************************************
           END-IF
           IF  (I-02 AND NOT-I-35)
               SET NOT-I-35                TO TRUE
               IF  NAVN1 = TEKSTR
                   SET I-35                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-35)
               MOVE TEKSTR                 TO TEKST
           END-IF
           IF  (I-02 AND I-35)
               PERFORM TKRUT-S
      *
           END-IF
           IF  (I-02)
               SET NOT-I-60                TO TRUE
               SET NOT-I-65                TO TRUE
               IF  BILART NOT > '3'
                   SET I-65                TO TRUE
               END-IF
               IF  BILART > '3'
                   SET I-60                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-60)
               ADD BELO-ELGP               TO BILSUM
           END-IF
           IF  (I-02 AND I-60 AND I-20)
               ADD VALUTA                  TO VALSUM
           END-IF.
 
       UT-T.
           CONTINUE.
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           MOVE ' '                        TO BBEST
           MOVE 'RES20'                    TO LONR
           MOVE FIRMA                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'RKO230  '                 TO LPROG
           MOVE 'RES50AM '                 TO BJOBB
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS.
      ******************************************************
      *    SUBRUTINE FOR CALL AV COBOL SUBRUTINE RESTRAN.  *
      *    DENNE RUTINE HENTER RESKONTRO TEKST.            *
      ******************************************************
 
       TKRUT-S SECTION.
       TKRUT-S-P.
           MOVE TRKODE                     TO TKTK
           MOVE '       '                  TO TKTEXT
           MOVE BILNR                      TO TKBILN
           MOVE REFNR                      TO TKREFN
           CALL 'RESTRAN' USING TKDATA-XX-DATA-FIELDS
           MOVE TKTEXT                     TO TEKST (1:7).
      ******************************************************
      *****************************************************************
      *  RUTINE FOR Å HENTE VALUTAKODER.                              *
      *****************************************************************
 
       VALRUT-S SECTION.
       VALRUT-S-P.
           SET NOT-I-96                    TO TRUE
           MOVE 'A'                        TO AKSEKD
           MOVE VT                         TO ADVK
           CALL 'ADVALUTA' USING VALPAR-XX-DATA-FIELDS
           MOVE STDVK                      TO VALTYP
           IF  (NOT-I-96)
               MOVE VT                     TO VALTYP (3:1)
           END-IF.
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1 AND I-60)
               SET NOT-I-61                TO TRUE
               SET NOT-I-62                TO TRUE
               IF  BILSUM > 0,00
                   SET I-61                TO TRUE
               END-IF
               IF  BILSUM < 0,00
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L2)
               SUBTRACT 1                  FROM PAGE0
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           END-IF
           .
 
       PERDATO-GET SECTION.
       PERDATO-GET-P.
           IF  PERDATO-EOF-OFF
               READ PERDATO
               AT END
                   SET PERDATO-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       PERDATO-FLDSET SECTION.
       PERDATO-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE PERDATO-IO-AREA (2:3)  TO PFIRM (1:3)
               MOVE PERDATO-IO-AREA (5:6)  TO PRESK (1:6)
               MOVE PERDATO-IO-AREA (11:6) TO DATO-IO
               INSPECT DATO-IO REPLACING ALL ' ' BY '0'
               MOVE PERDATO-IO-AREA (17:4) TO ARA (1) (1:4)
               MOVE PERDATO-IO-AREA (21:2) TO ARB (1) (1:2)
               MOVE PERDATO-IO-AREA (23:4) TO ARA (2) (1:4)
               MOVE PERDATO-IO-AREA (27:2) TO ARB (2) (1:2)
               MOVE PERDATO-IO-AREA (29:4) TO ARA (3) (1:4)
               MOVE PERDATO-IO-AREA (33:2) TO ARB (3) (1:2)
               MOVE PERDATO-IO-AREA (35:4) TO ARA (4) (1:4)
               MOVE PERDATO-IO-AREA (39:2) TO ARB (4) (1:2)
               MOVE PERDATO-IO-AREA (41:4) TO ARA (5) (1:4)
               MOVE PERDATO-IO-AREA (45:2) TO ARB (5) (1:2)
               MOVE PERDATO-IO-AREA (47:4) TO ARA (6) (1:4)
               MOVE PERDATO-IO-AREA (51:2) TO ARB (6) (1:2)
               MOVE PERDATO-IO-AREA (53:4) TO ARA (7) (1:4)
               MOVE PERDATO-IO-AREA (57:2) TO ARB (7) (1:2)
               MOVE PERDATO-IO-AREA (59:4) TO ARA (8) (1:4)
               MOVE PERDATO-IO-AREA (63:2) TO ARB (8) (1:2)
               MOVE PERDATO-IO-AREA (65:4) TO ARA (9) (1:4)
               MOVE PERDATO-IO-AREA (69:2) TO ARB (9) (1:2)
               MOVE PERDATO-IO-AREA (71:4) TO ARA (10) (1:4)
               MOVE PERDATO-IO-AREA (75:2) TO ARB (10) (1:2)
               MOVE PERDATO-IO-AREA (77:4) TO ARA (11) (1:4)
               MOVE PERDATO-IO-AREA (81:2) TO ARB (11) (1:2)
               MOVE PERDATO-IO-AREA (83:4) TO ARA (12) (1:4)
               MOVE PERDATO-IO-AREA (87:2) TO ARB (12) (1:2)
           END-EVALUATE.
 
       PERDATO-IDSET SECTION.
       PERDATO-IDSET-P.
           SET I-09                        TO TRUE.
 
       PERDATO-MATCH-SET SECTION.
       PERDATO-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE PERDATO-IO-AREA (2:3)  TO PERDATO-M-09-M2-PFIRM
               MOVE PERDATO-IO-AREA (5:6)  TO PERDATO-M-09-M1-PRESK
           END-EVALUATE.
 
       RESKF-GET SECTION.
       RESKF-GET-P.
           IF  RESKF-EOF-OFF
               READ RESKF
               AT END
                   SET RESKF-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RESKF-FLDOFF SECTION.
       RESKF-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( RESKF-IO-AREA (1:1) = '3'
            AND   RESKF-IO-AREA (2:1) = '0' )
               SET NOT-I-04                TO TRUE
           WHEN ( RESKF-IO-AREA (1:1) = '3'
            AND   RESKF-IO-AREA (2:1) = '1' )
               SET NOT-I-05                TO TRUE
               SET NOT-I-35                TO TRUE
           END-EVALUATE.
 
       RESKF-FLDSET SECTION.
       RESKF-FLDSET-P.
           EVALUATE TRUE
           WHEN ( RESKF-IO-AREA (1:1) = '3'
            AND   RESKF-IO-AREA (2:1) = '0' )
               MOVE RESKF-IO-AREA (1:132)  TO REC132 (1:132)
               MOVE RESKF-IO-AREA (3:3)    TO FIRMA (1:3)
               MOVE RESKF-IO-AREA (6:1)    TO RESK1 (1:1)
               MOVE RESKF-IO-AREA (6:6)    TO RESKNR (1:6)
               MOVE RESKF-IO-AREA (3:9)    TO RESKEY (1:9)
               MOVE RESKF-IO-AREA (18:2)   TO STK (1:2)
               MOVE RESKF-IO-AREA (20:2)   TO SA-ELGR (1:2)
               MOVE RESKF-IO-AREA (22:2)   TO SMND (1:2)
               MOVE RESKF-IO-AREA (24:2)   TO SDAG (1:2)
               MOVE RESKF-IO-AREA (38:10)  TO SBEL-IO
               INSPECT SBEL-IO REPLACING ALL ' ' BY '0'
               IF  SBEL < ZERO
                   SET I-04                TO TRUE
               END-IF
               MOVE RESKF-IO-AREA (50:11)  TO SBELV-IO
               INSPECT SBELV-IO REPLACING ALL ' ' BY '0'
               MOVE RESKF-IO-AREA (48:1)   TO INGBEV (1:1)
           WHEN ( RESKF-IO-AREA (1:1) = '3'
            AND   RESKF-IO-AREA (2:1) = '1' )
               MOVE RESKF-IO-AREA (18:2)   TO TRKODE (1:2)
               MOVE RESKF-IO-AREA (6:1)    TO RESK1 (1:1)
               MOVE RESKF-IO-AREA (6:6)    TO RESKNR (1:6)
               MOVE RESKF-IO-AREA (3:3)    TO FIRMA (1:3)
               MOVE RESKF-IO-AREA (3:9)    TO RESKEY (1:9)
               MOVE RESKF-IO-AREA (20:2)   TO BILA-ELGR (1:2)
               MOVE RESKF-IO-AREA (22:2)   TO BILMND (1:2)
               MOVE RESKF-IO-AREA (24:2)   TO BILDAG (1:2)
               MOVE RESKF-IO-AREA (26:6)   TO BILNR (1:6)
               MOVE RESKF-IO-AREA (12:6)   TO REFNR (1:6)
               MOVE RESKF-IO-AREA (33:2)   TO FORA-ELGR (1:2)
               MOVE RESKF-IO-AREA (35:2)   TO FORMND (1:2)
               MOVE RESKF-IO-AREA (37:2)   TO FORDAG (1:2)
               MOVE RESKF-IO-AREA (39:9)   TO BELO-ELGP-IO
               INSPECT BELO-ELGP-IO REPLACING ALL ' ' BY '0'
               IF  BELO-ELGP < ZERO
                   SET I-05                TO TRUE
               END-IF
               MOVE RESKF-IO-AREA (51:10)  TO VALUTA-IO
               INSPECT VALUTA-IO REPLACING ALL ' ' BY '0'
               MOVE RESKF-IO-AREA (32:1)   TO BILART (1:1)
               MOVE RESKF-IO-AREA (70:1)   TO VT (1:1)
               MOVE RESKF-IO-AREA (86:4)   TO REGA-ELGM (1:4)
               MOVE RESKF-IO-AREA (88:2)   TO REGMND (1:2)
               MOVE RESKF-IO-AREA (90:24)  TO TEKSTR (1:24)
               IF  TEKSTR = SPACES
                   SET I-35                TO TRUE
               END-IF
           END-EVALUATE.
 
       RESKF-IDCHK SECTION.
       RESKF-IDCHK-P.
           EVALUATE TRUE
           WHEN ( RESKF-IO-AREA (1:1) = '3'
            AND   RESKF-IO-AREA (2:1) = '0' )
             OR ( RESKF-IO-AREA (1:1) = '3'
            AND   RESKF-IO-AREA (2:1) = '1' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       RESKF-IDSET SECTION.
       RESKF-IDSET-P.
           EVALUATE TRUE
           WHEN ( RESKF-IO-AREA (1:1) = '3'
            AND   RESKF-IO-AREA (2:1) = '0' )
               SET I-01                    TO TRUE
           WHEN ( RESKF-IO-AREA (1:1) = '3'
            AND   RESKF-IO-AREA (2:1) = '1' )
               SET I-02                    TO TRUE
           END-EVALUATE.
 
       RESKF-CHK-LEVEL SECTION.
       RESKF-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( RESKF-IO-AREA (1:1) = '3'
            AND   RESKF-IO-AREA (2:1) = '0' )
               MOVE LOW-VALUES             TO RESKF-LEVEL-01
               MOVE RESKF-IO-AREA (3:3)    TO RESKF-01-L4-FIRMA
               MOVE RESKF-IO-AREA (6:1)    TO RESKF-01-L3-RESK1
               MOVE RESKF-IO-AREA (6:6)    TO RESKF-01-L2-RESKNR
               IF  RESKF-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  RESKF-01-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   WHEN  RESKF-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  RESKF-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   END-EVALUATE
               END-IF
               MOVE  RESKF-01-L4           TO THE-PRIOR-L4
               MOVE  RESKF-01-L3           TO THE-PRIOR-L3
               MOVE  RESKF-01-L2           TO THE-PRIOR-L2
               SET RESKF-LEVEL-INIT        TO TRUE
           WHEN ( RESKF-IO-AREA (1:1) = '3'
            AND   RESKF-IO-AREA (2:1) = '1' )
               MOVE LOW-VALUES             TO RESKF-LEVEL-02
               MOVE RESKF-IO-AREA (3:3)    TO RESKF-02-L4-FIRMA
               MOVE RESKF-IO-AREA (6:1)    TO RESKF-02-L3-RESK1
               MOVE RESKF-IO-AREA (6:6)    TO RESKF-02-L2-RESKNR
               MOVE RESKF-IO-AREA (26:6)   TO RESKF-02-L1-BILNR
               IF  RESKF-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  RESKF-02-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   WHEN  RESKF-02-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  RESKF-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  RESKF-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  RESKF-02-L4           TO THE-PRIOR-L4
               MOVE  RESKF-02-L3           TO THE-PRIOR-L3
               MOVE  RESKF-02-L2           TO THE-PRIOR-L2
               MOVE  RESKF-02-L1           TO THE-PRIOR-L1
               SET RESKF-LEVEL-INIT        TO TRUE
           END-EVALUATE.
 
       RESKF-MATCH-SET SECTION.
       RESKF-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( RESKF-IO-AREA (1:1) = '3'
            AND   RESKF-IO-AREA (2:1) = '0' )
               MOVE RESKF-IO-AREA (3:3)    TO RESKF-M-01-M2-FIRMA
               MOVE RESKF-IO-AREA (6:6)    TO RESKF-M-01-M1-RESKNR
           WHEN ( RESKF-IO-AREA (1:1) = '3'
            AND   RESKF-IO-AREA (2:1) = '1' )
               MOVE RESKF-IO-AREA (3:3)    TO RESKF-M-02-M2-FIRMA
               MOVE RESKF-IO-AREA (6:6)    TO RESKF-M-02-M1-RESKNR
           END-EVALUATE.
 
       KUNDEMA-FLDOFF SECTION.
       KUNDEMA-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( KUNDEMA-IO-AREA (1:1) = '8'
            AND   KUNDEMA-IO-AREA (2:1) = '0' )
               SET NOT-I-08                TO TRUE
               SET NOT-I-07                TO TRUE
           END-EVALUATE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ( KUNDEMA-IO-AREA (1:1) = '8'
            AND   KUNDEMA-IO-AREA (2:1) = '0' )
               MOVE KUNDEMA-IO-AREA (185:1) TO HAND1 (1:1)
               MOVE KUNDEMA-IO-AREA (6:1)  TO RESK1 (1:1)
               MOVE KUNDEMA-IO-AREA (16:30) TO NAVN1 (1:30)
               MOVE KUNDEMA-IO-AREA (46:30) TO NAVN2 (1:30)
               MOVE KUNDEMA-IO-AREA (76:30) TO ADRES (1:30)
               MOVE KUNDEMA-IO-AREA (106:15) TO PSTED (1:15)
               MOVE KUNDEMA-IO-AREA (121:4) TO PNR (1:4)
               MOVE KUNDEMA-IO-AREA (127:2) TO BM (1:2)
               MOVE KUNDEMA-IO-AREA (129:4) TO KRGR-IO
               INSPECT KRGR-IO REPLACING ALL ' ' BY '0'
               IF  KRGR = ZERO
                   SET I-08                TO TRUE
               END-IF
               MOVE KUNDEMA-IO-AREA (172:6) TO ALTNR (1:6)
               IF  ALTNR = SPACES
                   SET I-07                TO TRUE
               END-IF
           END-EVALUATE.
 
       KUNDEMA-IDCHK SECTION.
       KUNDEMA-IDCHK-P.
           EVALUATE TRUE
           WHEN ( KUNDEMA-IO-AREA (1:1) = '8'
            AND   KUNDEMA-IO-AREA (2:1) = '0' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           EVALUATE TRUE
           WHEN ( KUNDEMA-IO-AREA (1:1) = '8'
            AND   KUNDEMA-IO-AREA (2:1) = '0' )
               SET I-03                    TO TRUE
           END-EVALUATE.
 
       KUNDEMX-FLDOFF SECTION.
       KUNDEMX-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-15                TO TRUE
           END-EVALUATE.
 
       KUNDEMX-FLDSET SECTION.
       KUNDEMX-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMX-IO-AREA (180:9) TO KORGNR (1:9)
               IF  KORGNR = SPACES
                   SET I-15                TO TRUE
               END-IF
           END-EVALUATE.
 
       KUNDEMX-IDSET SECTION.
       KUNDEMX-IDSET-P.
           SET I-14                        TO TRUE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (204:50) TO HEAD1 (1:50)
               MOVE FIRMAF-IO-AREA (254:50) TO HEAD2 (1:50)
               MOVE FIRMAF-IO-AREA (304:50) TO HEAD3 (1:50)
               MOVE FIRMAF-IO-AREA (354:50) TO HEAD4 (1:50)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-06                        TO TRUE.
 
       SYSPARM-FLDSET SECTION.
       SYSPARM-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE SYSPARM-IO-AREA (137:1) TO VKODE (1:1)
           END-EVALUATE.
 
       SYSPARM-IDSET SECTION.
       SYSPARM-IDSET-P.
           SET I-12                        TO TRUE.
 
       KONTOK-PRINT-LINE SECTION.
       KONTOK-PRINT-LINE-P.
           IF  KONTOK-BEFORE-SKIP > 0
               PERFORM KONTOK-SKIP-BEFORE
           END-IF
           IF  KONTOK-BEFORE-SPACE > 0
               PERFORM KONTOK-SPACE-BEFORE
               IF  KONTOK-AFTER-SKIP > 0
                   PERFORM KONTOK-SKIP-AFTER
               END-IF
               IF  KONTOK-AFTER-SPACE > 0
                   PERFORM KONTOK-SPACE-AFTER
               END-IF
           ELSE
               IF  KONTOK-AFTER-SKIP > 0
                   PERFORM KONTOK-SKIP-AFTER
               END-IF
               PERFORM KONTOK-SPACE-AFTER
           END-IF
           IF  KONTOK-LINE-COUNT NOT < KONTOK-MAX-LINES
               SET I-OF                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OF            TO TRUE
               END-IF
           END-IF.
 
       KONTOK-SKIP-BEFORE SECTION.
       KONTOK-SKIP-BEFORE-P.
           EVALUATE TRUE
           WHEN KONTOK-BEFORE-SKIP = 1
               WRITE KONTOK-IO-PRINT    AFTER ADVANCING SKIP-CHANNEL-1
           WHEN KONTOK-BEFORE-SKIP = 2
               WRITE KONTOK-IO-PRINT    AFTER ADVANCING SKIP-CHANNEL-2
           WHEN KONTOK-BEFORE-SKIP = 3
               WRITE KONTOK-IO-PRINT    AFTER ADVANCING SKIP-CHANNEL-3
           END-EVALUATE
           MOVE 1                          TO KONTOK-LINE-COUNT
           MOVE 0                          TO KONTOK-BEFORE-SKIP
           INITIALIZE KONTOK-IO-AREA.
 
       KONTOK-SPACE-BEFORE SECTION.
       KONTOK-SPACE-BEFORE-P.
           WRITE KONTOK-IO-PRINT        AFTER KONTOK-BEFORE-SPACE LINES
           ADD KONTOK-BEFORE-SPACE         TO KONTOK-LINE-COUNT
           MOVE SPACES TO KONTOK-IO-AREA
           INITIALIZE KONTOK-IO-AREA
           MOVE 0                          TO KONTOK-BEFORE-SPACE.
 
       KONTOK-SKIP-AFTER SECTION.
       KONTOK-SKIP-AFTER-P.
           EVALUATE TRUE
           WHEN KONTOK-AFTER-SKIP = 1
               WRITE KONTOK-IO-PRINT   BEFORE ADVANCING SKIP-CHANNEL-1
           WHEN KONTOK-AFTER-SKIP = 2
               WRITE KONTOK-IO-PRINT   BEFORE ADVANCING SKIP-CHANNEL-2
           WHEN KONTOK-AFTER-SKIP = 3
               WRITE KONTOK-IO-PRINT   BEFORE ADVANCING SKIP-CHANNEL-3
           END-EVALUATE
           MOVE 1                          TO KONTOK-LINE-COUNT
           MOVE 0                          TO KONTOK-AFTER-SKIP
           INITIALIZE KONTOK-IO-AREA.
 
       KONTOK-SPACE-AFTER SECTION.
       KONTOK-SPACE-AFTER-P.
           WRITE KONTOK-IO-PRINT       BEFORE KONTOK-AFTER-SPACE LINES
           ADD KONTOK-AFTER-SPACE          TO KONTOK-LINE-COUNT
           INITIALIZE KONTOK-IO-AREA
           MOVE 0                          TO KONTOK-AFTER-SPACE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  PERDATO-EOF
               MOVE HIGH-VALUES            TO PERDATO-MC
                                              PERDATO-MP
           END-IF
           IF  RESKF-EOF
               MOVE HIGH-VALUES            TO RESKF-MC
                                              RESKF-MP
           END-IF
           IF  PERDATO-MC < PERDATO-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  RESKF-MC < RESKF-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  PERDATO-MC < RESKF-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET PERDATO-PROCESS     TO TRUE
                   MOVE PERDATO-MC         TO PERDATO-MP
                   IF  PERDATO-MC = RESKF-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  RESKF-MC < PERDATO-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET RESKF-PROCESS       TO TRUE
                   MOVE RESKF-MC           TO RESKF-MP
                   IF  RESKF-MC = PERDATO-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  PERDATO-MC = RESKF-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET PERDATO-PROCESS     TO TRUE
                   MOVE PERDATO-MC         TO PERDATO-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-90)
               MOVE SPACES TO KONTOK-IO-AREA
               INITIALIZE KONTOK-IO-AREA
               MOVE SDAG                   TO KONTOK-IO-AREA (1:2)
               MOVE '.'                    TO KONTOK-IO-AREA (3:1)
               MOVE SMND                   TO KONTOK-IO-AREA (4:2)
               MOVE '.'                    TO KONTOK-IO-AREA (6:1)
               MOVE SA-ELGR                TO KONTOK-IO-AREA (7:2)
               MOVE 'SALDO  '              TO KONTOK-IO-AREA (26:7)
               IF  (NOT-I-04)
                   MOVE SBEL               TO XO-82YY9
                   MOVE XO-82YY9           TO KONTOK-IO-AREA (64:13)
               END-IF
               IF  (I-04)
                   MOVE SBEL               TO XO-82YY9
                   MOVE XO-82YY9           TO KONTOK-IO-AREA (77:13)
               END-IF
               MOVE SBELV                  TO XO-92YY9R
               MOVE XO-92YY9R              TO KONTOK-IO-AREA (107:15)
               MOVE 1                      TO KONTOK-AFTER-SPACE
               PERFORM KONTOK-PRINT-LINE
           END-IF
           IF  (I-02 AND I-65)
               MOVE SPACES TO KONTOK-IO-AREA
               INITIALIZE KONTOK-IO-AREA
               MOVE BILDAG                 TO KONTOK-IO-AREA (1:2)
               MOVE '.'                    TO KONTOK-IO-AREA (3:1)
               MOVE BILMND                 TO KONTOK-IO-AREA (4:2)
               MOVE '.'                    TO KONTOK-IO-AREA (6:1)
               MOVE BILA-ELGR              TO KONTOK-IO-AREA (7:2)
               MOVE BILART                 TO KONTOK-IO-AREA (10:1)
               MOVE BILNR                  TO KONTOK-IO-AREA (12:6)
               MOVE REFNR                  TO KONTOK-IO-AREA (19:6)
               MOVE TEKST                  TO KONTOK-IO-AREA (26:24)
               INITIALIZE TEKST
               IF  (NOT-I-05)
                   MOVE BELO-ELGP          TO XO-72YY9
                   MOVE XO-72YY9           TO KONTOK-IO-AREA (65:12)
               END-IF
               IF  (I-05)
                   MOVE BELO-ELGP          TO XO-72YY9
                   MOVE XO-72YY9           TO KONTOK-IO-AREA (78:12)
               END-IF
               MOVE FORDAG                 TO KONTOK-IO-AREA (91:2)
               MOVE '.'                    TO KONTOK-IO-AREA (93:1)
               MOVE FORMND                 TO KONTOK-IO-AREA (94:2)
               MOVE '.'                    TO KONTOK-IO-AREA (96:1)
               MOVE FORA-ELGR              TO KONTOK-IO-AREA (97:2)
               MOVE VALTYP                 TO KONTOK-IO-AREA (101:3)
               IF  (I-20)
                   MOVE VALUTA             TO XO-82YY9R
                   MOVE XO-82YY9R          TO KONTOK-IO-AREA (108:14)
               END-IF
               MOVE 1                      TO KONTOK-BEFORE-SPACE
               PERFORM KONTOK-PRINT-LINE
           END-IF
           IF  (I-02 AND I-60)
               MOVE SPACES TO KONTOK-IO-AREA
               INITIALIZE KONTOK-IO-AREA
               MOVE BILDAG                 TO KONTOK-IO-AREA (1:2)
               MOVE '.'                    TO KONTOK-IO-AREA (3:1)
               MOVE BILMND                 TO KONTOK-IO-AREA (4:2)
               MOVE '.'                    TO KONTOK-IO-AREA (6:1)
               MOVE BILA-ELGR              TO KONTOK-IO-AREA (7:2)
               MOVE BILART                 TO KONTOK-IO-AREA (10:1)
               MOVE BILNR                  TO KONTOK-IO-AREA (12:6)
               MOVE REFNR                  TO KONTOK-IO-AREA (19:6)
               MOVE TEKST                  TO KONTOK-IO-AREA (26:24)
               INITIALIZE TEKST
               IF  (NOT-I-20)
                   MOVE BELO-ELGP          TO XO-72YY9R
                   MOVE XO-72YY9R          TO KONTOK-IO-AREA (51:13)
               END-IF
               IF  (I-20)
                   MOVE VALUTA             TO XO-82YY9R
                   MOVE XO-82YY9R          TO KONTOK-IO-AREA (50:14)
               END-IF
               MOVE FORDAG                 TO KONTOK-IO-AREA (91:2)
               MOVE '.'                    TO KONTOK-IO-AREA (93:1)
               MOVE FORMND                 TO KONTOK-IO-AREA (94:2)
               MOVE '.'                    TO KONTOK-IO-AREA (96:1)
               MOVE FORA-ELGR              TO KONTOK-IO-AREA (97:2)
               MOVE VALTYP                 TO KONTOK-IO-AREA (101:3)
               MOVE 1                      TO KONTOK-BEFORE-SPACE
               PERFORM KONTOK-PRINT-LINE
           END-IF.
 
       EXCEPTION-OUTPUT SECTION.
       EXCEPTION-OUTPUT-P.
           IF  (I-02 AND NOT-I-31)
               MOVE SPACES TO KONTOK-IO-AREA
               INITIALIZE KONTOK-IO-AREA
               MOVE 'SALDO PR.'            TO KONTOK-IO-AREA (12:9)
               MOVE DAG                    TO KONTOK-IO-AREA (22:2)
               MOVE '/'                    TO KONTOK-IO-AREA (24:1)
               MOVE FMND                   TO KONTOK-IO-AREA (25:2)
               IF  (NOT-I-72)
                   MOVE NYSAL              TO XO-92YY9
                   MOVE XO-92YY9           TO KONTOK-IO-AREA (63:14)
               END-IF
               IF  (I-72)
                   MOVE NYSAL              TO XO-92YY9
                   MOVE XO-92YY9           TO KONTOK-IO-AREA (76:14)
               END-IF
               MOVE NYSALV                 TO XO-92YY9R
               MOVE XO-92YY9R              TO KONTOK-IO-AREA (107:15)
               MOVE 1                      TO KONTOK-BEFORE-SPACE
               MOVE 1                      TO KONTOK-AFTER-SPACE
               PERFORM KONTOK-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L2 AND I-90)
               MOVE SPACES TO KONTOK-IO-AREA
               INITIALIZE KONTOK-IO-AREA
               MOVE HEAD1                  TO KONTOK-IO-AREA (24:50)
               MOVE 01                     TO KONTOK-BEFORE-SKIP
               MOVE 1                      TO KONTOK-AFTER-SPACE
               PERFORM KONTOK-PRINT-LINE
               MOVE SPACES TO KONTOK-IO-AREA
               INITIALIZE KONTOK-IO-AREA
               MOVE HEAD2                  TO KONTOK-IO-AREA (24:50)
               MOVE 1                      TO KONTOK-AFTER-SPACE
               PERFORM KONTOK-PRINT-LINE
               MOVE SPACES TO KONTOK-IO-AREA
               INITIALIZE KONTOK-IO-AREA
               MOVE HEAD3                  TO KONTOK-IO-AREA (24:50)
               MOVE 1                      TO KONTOK-AFTER-SPACE
               PERFORM KONTOK-PRINT-LINE
               MOVE SPACES TO KONTOK-IO-AREA
               INITIALIZE KONTOK-IO-AREA
               MOVE HEAD4                  TO KONTOK-IO-AREA (24:50)
               MOVE 1                      TO KONTOK-AFTER-SPACE
               PERFORM KONTOK-PRINT-LINE
               MOVE SPACES TO KONTOK-IO-AREA
               INITIALIZE KONTOK-IO-AREA
               MOVE 'K O N T O U T D R A G' TO KONTOK-IO-AREA (10:21)
               MOVE 02                     TO KONTOK-BEFORE-SKIP
               MOVE 2                      TO KONTOK-AFTER-SPACE
               PERFORM KONTOK-PRINT-LINE
               MOVE SPACES TO KONTOK-IO-AREA
               INITIALIZE KONTOK-IO-AREA
               MOVE 'VENNLIGST OPPBEVAR DENNE' TO KONTOK-IO-AREA
                                                               (10:24)
               MOVE 3                      TO KONTOK-AFTER-SPACE
               PERFORM KONTOK-PRINT-LINE
               MOVE SPACES TO KONTOK-IO-AREA
               INITIALIZE KONTOK-IO-AREA
               MOVE '*'                    TO KONTOK-IO-AREA (1:1)
               IF  (NOT-I-17)
                   MOVE NAVN1              TO KONTOK-IO-AREA (10:30)
               END-IF
               IF  (I-18 AND NOT-I-21)
                   MOVE 'NO '              TO KONTOK-IO-AREA (41:3)
               END-IF
               IF  (I-18 AND NOT-I-21)
                   MOVE KORGNR             TO KONTOK-IO-AREA (44:9)
               END-IF
               IF  (I-18 AND I-21)
                   MOVE '*MANGLER*'        TO KONTOK-IO-AREA (44:9)
               END-IF
               IF  (I-15 AND I-18 AND NOT-I-21)
                   MOVE '*MANGLER*'        TO KONTOK-IO-AREA (44:9)
               END-IF
               IF  (I-18 AND NOT-I-21)
                   MOVE ' MVA'             TO KONTOK-IO-AREA (53:4)
               END-IF
               IF  (I-18 AND I-21)
                   MOVE ' (ORG.NR IKKE I KAJ2)' TO KONTOK-IO-AREA
                                                               (57:21)
               END-IF
               IF  (I-15 AND I-18 AND NOT-I-21)
                   MOVE ' (ORG.NR IKKE I KAJ2)' TO KONTOK-IO-AREA
                                                               (57:21)
      *       H  1     L2N10 90
      *      OR        OFN10NL2
               END-IF
               MOVE 1                      TO KONTOK-AFTER-SPACE
               PERFORM KONTOK-PRINT-LINE
           END-IF
           IF  (I-L2 AND I-90)
               MOVE SPACES TO KONTOK-IO-AREA
               INITIALIZE KONTOK-IO-AREA
               MOVE '='                    TO KONTOK-IO-AREA (1:1)
               IF  (NOT-I-17)
                   MOVE NAVN2              TO KONTOK-IO-AREA (10:30)
      *       H  1     L2N11 90
      * *    OR        OFN11NL2
               END-IF
               MOVE 1                      TO KONTOK-AFTER-SPACE
               PERFORM KONTOK-PRINT-LINE
               MOVE SPACES TO KONTOK-IO-AREA
               INITIALIZE KONTOK-IO-AREA
               MOVE '=='                   TO KONTOK-IO-AREA (1:2)
               IF  (NOT-I-17)
                   MOVE ADRES              TO KONTOK-IO-AREA (10:30)
               END-IF
               MOVE 1                      TO KONTOK-AFTER-SPACE
               PERFORM KONTOK-PRINT-LINE
           END-IF
           IF  (I-L2 AND I-90)
               MOVE SPACES TO KONTOK-IO-AREA
               INITIALIZE KONTOK-IO-AREA
               MOVE '**'                   TO KONTOK-IO-AREA (1:2)
               IF  (NOT-I-17)
                   MOVE PNR                TO KONTOK-IO-AREA (10:4)
               END-IF
               IF  (NOT-I-17)
                   MOVE PSTED              TO KONTOK-IO-AREA (15:15)
               END-IF
               MOVE 1                      TO KONTOK-AFTER-SPACE
               PERFORM KONTOK-PRINT-LINE
               MOVE SPACES TO KONTOK-IO-AREA
               INITIALIZE KONTOK-IO-AREA
               IF  (NOT-I-07)
                   MOVE 'ALT RNR.'         TO KONTOK-IO-AREA (2:8)
               END-IF
               IF  (NOT-I-07)
                   MOVE ALTNR              TO KONTOK-IO-AREA (11:6)
               END-IF
               MOVE 'BETM'                 TO KONTOK-IO-AREA (19:4)
               MOVE BM                     TO KONTOK-IO-AREA (24:2)
               IF  (I-08)
                   MOVE '*'                TO KONTOK-IO-AREA (27:1)
               END-IF
               MOVE 'RESKNR'               TO KONTOK-IO-AREA (31:6)
               MOVE RESKNR                 TO KONTOK-IO-AREA (38:6)
               MOVE 'DATO'                 TO KONTOK-IO-AREA (48:4)
               MOVE DATO                   TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO KONTOK-IO-AREA (53:8)
               MOVE 'SIDE'                 TO KONTOK-IO-AREA (113:4)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO KONTOK-IO-AREA (118:4)
               MOVE 03                     TO KONTOK-BEFORE-SKIP
               MOVE 2                      TO KONTOK-AFTER-SPACE
               PERFORM KONTOK-PRINT-LINE
               MOVE SPACES TO KONTOK-IO-AREA
               INITIALIZE KONTOK-IO-AREA
               MOVE 'BILAGS  B BILAGS REFER.' TO KONTOK-IO-AREA (2:23)
               MOVE 'BETALINGS-    '       TO KONTOK-IO-AREA (51:14)
               MOVE 'FORFALLS'             TO KONTOK-IO-AREA (91:8)
               IF  (I-18 AND I-19 AND I-25)
                   MOVE 'VALUTA'           TO KONTOK-IO-AREA (100:6)
               END-IF
               MOVE 1                      TO KONTOK-AFTER-SPACE
               PERFORM KONTOK-PRINT-LINE
               MOVE SPACES TO KONTOK-IO-AREA
               INITIALIZE KONTOK-IO-AREA
               MOVE 'DATO   A  NR.    NR.' TO KONTOK-IO-AREA (3:20)
               MOVE 'TEKST                   ' TO KONTOK-IO-AREA
                                                               (26:24)
               MOVE 'SPESIFIKASJON.'       TO KONTOK-IO-AREA (51:14)
               MOVE 'D E B E T  K R E D I T' TO KONTOK-IO-AREA (67:22)
               MOVE 'DATO'                 TO KONTOK-IO-AREA (93:4)
               IF  (I-18 AND I-19 AND I-25)
                   MOVE 'TYPE'             TO KONTOK-IO-AREA (101:4)
               END-IF
               IF  (I-18 AND I-19 AND I-25)
                   MOVE '    VALUTABELØP'  TO KONTOK-IO-AREA (107:15)
               END-IF
               MOVE 2                      TO KONTOK-AFTER-SPACE
               PERFORM KONTOK-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF AND NOT-I-L2)
               MOVE SPACES TO KONTOK-IO-AREA
               INITIALIZE KONTOK-IO-AREA
               MOVE HEAD1                  TO KONTOK-IO-AREA (24:50)
               MOVE 01                     TO KONTOK-BEFORE-SKIP
               MOVE 1                      TO KONTOK-AFTER-SPACE
               PERFORM KONTOK-PRINT-LINE
               MOVE SPACES TO KONTOK-IO-AREA
               INITIALIZE KONTOK-IO-AREA
               MOVE HEAD2                  TO KONTOK-IO-AREA (24:50)
               MOVE 1                      TO KONTOK-AFTER-SPACE
               PERFORM KONTOK-PRINT-LINE
               MOVE SPACES TO KONTOK-IO-AREA
               INITIALIZE KONTOK-IO-AREA
               MOVE HEAD3                  TO KONTOK-IO-AREA (24:50)
               MOVE 1                      TO KONTOK-AFTER-SPACE
               PERFORM KONTOK-PRINT-LINE
               MOVE SPACES TO KONTOK-IO-AREA
               INITIALIZE KONTOK-IO-AREA
               MOVE HEAD4                  TO KONTOK-IO-AREA (24:50)
               MOVE 1                      TO KONTOK-AFTER-SPACE
               PERFORM KONTOK-PRINT-LINE
               MOVE SPACES TO KONTOK-IO-AREA
               INITIALIZE KONTOK-IO-AREA
               MOVE 'K O N T O U T D R A G' TO KONTOK-IO-AREA (10:21)
               MOVE 02                     TO KONTOK-BEFORE-SKIP
               MOVE 2                      TO KONTOK-AFTER-SPACE
               PERFORM KONTOK-PRINT-LINE
               MOVE SPACES TO KONTOK-IO-AREA
               INITIALIZE KONTOK-IO-AREA
               MOVE 'VENNLIGST OPPBEVAR DENNE' TO KONTOK-IO-AREA
                                                               (10:24)
               MOVE 3                      TO KONTOK-AFTER-SPACE
               PERFORM KONTOK-PRINT-LINE
               MOVE SPACES TO KONTOK-IO-AREA
               INITIALIZE KONTOK-IO-AREA
               MOVE '*'                    TO KONTOK-IO-AREA (1:1)
               IF  (NOT-I-17)
                   MOVE NAVN1              TO KONTOK-IO-AREA (10:30)
               END-IF
               IF  (I-18 AND NOT-I-21)
                   MOVE 'NO '              TO KONTOK-IO-AREA (41:3)
               END-IF
               IF  (I-18 AND NOT-I-21)
                   MOVE KORGNR             TO KONTOK-IO-AREA (44:9)
               END-IF
               IF  (I-18 AND I-21)
                   MOVE '*MANGLER*'        TO KONTOK-IO-AREA (44:9)
               END-IF
               IF  (I-15 AND I-18 AND NOT-I-21)
                   MOVE '*MANGLER*'        TO KONTOK-IO-AREA (44:9)
               END-IF
               IF  (I-18 AND NOT-I-21)
                   MOVE ' MVA'             TO KONTOK-IO-AREA (53:4)
               END-IF
               IF  (I-18 AND I-21)
                   MOVE ' (ORG.NR IKKE I KAJ2)' TO KONTOK-IO-AREA
                                                               (57:21)
               END-IF
               IF  (I-15 AND I-18 AND NOT-I-21)
                   MOVE ' (ORG.NR IKKE I KAJ2)' TO KONTOK-IO-AREA
                                                               (57:21)
      *       H  1     L2N10 90
      *      OR        OFN10NL2
               END-IF
               MOVE 1                      TO KONTOK-AFTER-SPACE
               PERFORM KONTOK-PRINT-LINE
           END-IF
           IF  (I-OF AND NOT-I-L2)
               MOVE SPACES TO KONTOK-IO-AREA
               INITIALIZE KONTOK-IO-AREA
               MOVE '='                    TO KONTOK-IO-AREA (1:1)
               IF  (NOT-I-17)
                   MOVE NAVN2              TO KONTOK-IO-AREA (10:30)
      *       H  1     L2N11 90
      * *    OR        OFN11NL2
               END-IF
               MOVE 1                      TO KONTOK-AFTER-SPACE
               PERFORM KONTOK-PRINT-LINE
               MOVE SPACES TO KONTOK-IO-AREA
               INITIALIZE KONTOK-IO-AREA
               MOVE '=='                   TO KONTOK-IO-AREA (1:2)
               IF  (NOT-I-17)
                   MOVE ADRES              TO KONTOK-IO-AREA (10:30)
               END-IF
               MOVE 1                      TO KONTOK-AFTER-SPACE
               PERFORM KONTOK-PRINT-LINE
           END-IF
           IF  (I-OF AND NOT-I-L2)
               MOVE SPACES TO KONTOK-IO-AREA
               INITIALIZE KONTOK-IO-AREA
               MOVE '**'                   TO KONTOK-IO-AREA (1:2)
               IF  (NOT-I-17)
                   MOVE PNR                TO KONTOK-IO-AREA (10:4)
               END-IF
               IF  (NOT-I-17)
                   MOVE PSTED              TO KONTOK-IO-AREA (15:15)
               END-IF
               MOVE 1                      TO KONTOK-AFTER-SPACE
               PERFORM KONTOK-PRINT-LINE
               MOVE SPACES TO KONTOK-IO-AREA
               INITIALIZE KONTOK-IO-AREA
               IF  (NOT-I-07)
                   MOVE 'ALT RNR.'         TO KONTOK-IO-AREA (2:8)
               END-IF
               IF  (NOT-I-07)
                   MOVE ALTNR              TO KONTOK-IO-AREA (11:6)
               END-IF
               MOVE 'BETM'                 TO KONTOK-IO-AREA (19:4)
               MOVE BM                     TO KONTOK-IO-AREA (24:2)
               IF  (I-08)
                   MOVE '*'                TO KONTOK-IO-AREA (27:1)
               END-IF
               MOVE 'RESKNR'               TO KONTOK-IO-AREA (31:6)
               MOVE RESKNR                 TO KONTOK-IO-AREA (38:6)
               MOVE 'DATO'                 TO KONTOK-IO-AREA (48:4)
               MOVE DATO                   TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO KONTOK-IO-AREA (53:8)
               MOVE 'SIDE'                 TO KONTOK-IO-AREA (113:4)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO KONTOK-IO-AREA (118:4)
               MOVE 03                     TO KONTOK-BEFORE-SKIP
               MOVE 2                      TO KONTOK-AFTER-SPACE
               PERFORM KONTOK-PRINT-LINE
               MOVE SPACES TO KONTOK-IO-AREA
               INITIALIZE KONTOK-IO-AREA
               MOVE 'BILAGS  B BILAGS REFER.' TO KONTOK-IO-AREA (2:23)
               MOVE 'BETALINGS-    '       TO KONTOK-IO-AREA (51:14)
               MOVE 'FORFALLS'             TO KONTOK-IO-AREA (91:8)
               IF  (I-18 AND I-19 AND I-25)
                   MOVE 'VALUTA'           TO KONTOK-IO-AREA (100:6)
               END-IF
               MOVE 1                      TO KONTOK-AFTER-SPACE
               PERFORM KONTOK-PRINT-LINE
               MOVE SPACES TO KONTOK-IO-AREA
               INITIALIZE KONTOK-IO-AREA
               MOVE 'DATO   A  NR.    NR.' TO KONTOK-IO-AREA (3:20)
               MOVE 'TEKST                   ' TO KONTOK-IO-AREA
                                                               (26:24)
               MOVE 'SPESIFIKASJON.'       TO KONTOK-IO-AREA (51:14)
               MOVE 'D E B E T  K R E D I T' TO KONTOK-IO-AREA (67:22)
               MOVE 'DATO'                 TO KONTOK-IO-AREA (93:4)
               IF  (I-18 AND I-19 AND I-25)
                   MOVE 'TYPE'             TO KONTOK-IO-AREA (101:4)
               END-IF
               IF  (I-18 AND I-19 AND I-25)
                   MOVE '    VALUTABELØP'  TO KONTOK-IO-AREA (107:15)
               END-IF
               MOVE 2                      TO KONTOK-AFTER-SPACE
               PERFORM KONTOK-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-60)
               MOVE SPACES TO KONTOK-IO-AREA
               INITIALIZE KONTOK-IO-AREA
               IF  (I-61)
                   MOVE BILSUM             TO XO-72YY9
                   MOVE XO-72YY9           TO KONTOK-IO-AREA (65:12)
                   INITIALIZE BILSUM
               END-IF
               IF  (I-62)
                   MOVE BILSUM             TO XO-72YY9
                   MOVE XO-72YY9           TO KONTOK-IO-AREA (78:12)
                   INITIALIZE BILSUM
               END-IF
               IF  (I-20)
                   MOVE VALSUM             TO XO-82YYZR
                   MOVE XO-82YYZR          TO KONTOK-IO-AREA (108:14)
                   INITIALIZE VALSUM
               END-IF
               MOVE 0                      TO KONTOK-AFTER-SPACE
               PERFORM KONTOK-PRINT-LINE
           END-IF
           IF  (I-L2 AND I-90 AND I-95)
               MOVE SPACES TO KONTOK-IO-AREA
               INITIALIZE KONTOK-IO-AREA
               MOVE 'INGEN BEVEGELSER I DENNE' TO KONTOK-IO-AREA
                                                               (10:24)
               MOVE 'PERIODE'              TO KONTOK-IO-AREA (35:7)
               MOVE 3                      TO KONTOK-BEFORE-SPACE
               MOVE 1                      TO KONTOK-AFTER-SPACE
               PERFORM KONTOK-PRINT-LINE
           END-IF
           IF  (I-L2 AND I-90)
               MOVE SPACES TO KONTOK-IO-AREA
               INITIALIZE KONTOK-IO-AREA
               MOVE 'NY SALDO   ***'       TO KONTOK-IO-AREA (12:14)
               IF  (NOT-I-72)
                   MOVE NYSAL              TO XO-92YY9
                   MOVE XO-92YY9           TO KONTOK-IO-AREA (63:14)
                   INITIALIZE NYSAL
               END-IF
               IF  (I-72)
                   MOVE NYSAL              TO XO-92YY9
                   MOVE XO-92YY9           TO KONTOK-IO-AREA (76:14)
                   INITIALIZE NYSAL
               END-IF
               MOVE NYSALV                 TO XO-92YY9R
               MOVE XO-92YY9R              TO KONTOK-IO-AREA (107:15)
               MOVE 2                      TO KONTOK-BEFORE-SPACE
               PERFORM KONTOK-PRINT-LINE
           END-IF
           IF  (I-L3 AND I-90)
               MOVE SPACES TO KONTOK-IO-AREA
               INITIALIZE KONTOK-IO-AREA
               MOVE 'SUBTOTAL FOR RESK.GR. ' TO KONTOK-IO-AREA (3:22)
               MOVE RESK1                  TO KONTOK-IO-AREA (24:1)
               MOVE SUBBEV                 TO XO-102YY9R
               MOVE XO-102YY9R             TO KONTOK-IO-AREA (25:17)
               INITIALIZE SUBBEV
               MOVE 01                     TO KONTOK-BEFORE-SKIP
               MOVE 3                      TO KONTOK-BEFORE-SPACE
               PERFORM KONTOK-PRINT-LINE
           END-IF
           IF  (I-L4 AND I-90)
               MOVE SPACES TO KONTOK-IO-AREA
               INITIALIZE KONTOK-IO-AREA
               MOVE 'F I R M A T O T A L   ' TO KONTOK-IO-AREA (3:22)
               MOVE TOTBEV                 TO XO-102YY9R
               MOVE XO-102YY9R             TO KONTOK-IO-AREA (25:17)
               INITIALIZE TOTBEV
      *****************************************************************
      * DUMMY-LINJE FOR Å FJERNE FEILMELDING.                         *
      *****************************************************************
               MOVE 2                      TO KONTOK-BEFORE-SPACE
               PERFORM KONTOK-PRINT-LINE
           END-IF
           IF  (I-LR AND I-U8 AND I-U7)
           AND (I-03 AND I-06 AND I-12)
           AND (I-14)
           AND (I-16 AND I-41 AND I-42)
           AND (I-43 AND I-98 AND I-94)
               MOVE SPACES TO KONTOK-IO-AREA
               INITIALIZE KONTOK-IO-AREA
               MOVE PSDS                   TO KONTOK-IO-AREA (1:80)
               MOVE R                      TO KONTOK-IO-AREA (73:8)
               MOVE P-IO                   TO KONTOK-IO-AREA (78:3)
               MOVE S-IO                   TO KONTOK-IO-AREA (76:5)
               MOVE TKTK                   TO KONTOK-IO-AREA (79:2)
               MOVE TKBILN                 TO KONTOK-IO-AREA (75:6)
               MOVE TKREFN                 TO KONTOK-IO-AREA (75:6)
               MOVE STK                    TO KONTOK-IO-AREA (79:2)
               MOVE LONR                   TO KONTOK-IO-AREA (76:5)
               MOVE LFIRMA                 TO KONTOK-IO-AREA (78:3)
               MOVE LUNDGR                 TO KONTOK-IO-AREA (78:3)
               MOVE LPROG                  TO KONTOK-IO-AREA (73:8)
               MOVE LANTX-IO               TO KONTOK-IO-AREA (78:3)
               MOVE LPRIID                 TO KONTOK-IO-AREA (77:4)
               MOVE BJOBB                  TO KONTOK-IO-AREA (73:8)
               MOVE BBEST                  TO KONTOK-IO-AREA (80:1)
               MOVE BFILL1                 TO KONTOK-IO-AREA (31:50)
               MOVE BFILL2                 TO KONTOK-IO-AREA (31:50)
               MOVE BFILL3                 TO KONTOK-IO-AREA (24:57)
               MOVE FIRNVN                 TO KONTOK-IO-AREA (51:30)
               MOVE AKSEKD                 TO KONTOK-IO-AREA (80:1)
               MOVE ADVK                   TO KONTOK-IO-AREA (80:1)
               MOVE NORVN                  TO KONTOK-IO-AREA (61:20)
               MOVE STDVN                  TO KONTOK-IO-AREA (61:20)
               MOVE LANDKD                 TO KONTOK-IO-AREA (79:2)
               MOVE LAND                   TO KONTOK-IO-AREA (61:20)
               MOVE VALIX-IO               TO KONTOK-IO-AREA (78:3)
               MOVE DIV                    TO KONTOK-IO-AREA (71:10)
               MOVE AKSEKD                 TO KONTOK-IO-AREA (80:1)
               MOVE ADVK                   TO KONTOK-IO-AREA (80:1)
               MOVE REC132                 TO KONTOK-IO-AREA (1:132)
               MOVE 1                      TO KONTOK-AFTER-SPACE
               PERFORM KONTOK-PRINT-LINE
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
           INITIALIZE PERDATO-DATA-FIELDS
           SET PERDATO-EOF-OFF             TO TRUE
           SET PERDATO-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO PERDATO-MC
                                              PERDATO-MP
           OPEN INPUT PERDATO
           SET RESKF-LEVEL-INIT            TO TRUE
           INITIALIZE RESKF-DATA-FIELDS
           SET RESKF-EOF-OFF               TO TRUE
           SET RESKF-PROCESS               TO TRUE
           MOVE LOW-VALUES                 TO RESKF-MC
                                              RESKF-MP
           OPEN INPUT RESKF
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           INITIALIZE KUNDEMX-DATA-FIELDS
           OPEN INPUT KUNDEMX
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           INITIALIZE SYSPARM-DATA-FIELDS
           OPEN INPUT SYSPARM
           OPEN OUTPUT KONTOK
           INITIALIZE KONTOK-IO-AREA
           INITIALIZE KONTOK-DATA-FIELDS
           MOVE 57                         TO KONTOK-MAX-LINES.
           PERFORM VARYING ARA-I FROM 1 BY 1
                     UNTIL ARA-I > ARA-MAX
               INITIALIZE ARA (ARA-I)
               INITIALIZE ARB (ARA-I)
           END-PERFORM
           SET ARA-I                       TO 1
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PERDATO
           CLOSE RESKF
           CLOSE KUNDEMA
           CLOSE KUNDEMX
           CLOSE FIRMAF
           CLOSE SYSPARM
           IF KONTOK-IO-AREA NOT = SPACES
             WRITE KONTOK-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO KONTOK-IO-AREA
           END-IF
           CLOSE KONTOK.
 
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
