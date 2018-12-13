       IDENTIFICATION DIVISION.
       PROGRAM-ID. RSK010R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM.......: RSK010, PLUKKER UT RELATERTE FAKTURAER OG    *
      *                  KREDITNOTAER FRA RELFILE, BASERT PÅ RELA-    *
      *                  TERINGSNR TILDELT VED GIROUTSKRIFT.          *
      *  PROGRAMMERER..:                                              *
      *  KJØRES I JOBB.: DOP12UD                                      *
      *  LAGET DATO....:                                              *
      *  ENDRET........: 13.11.95 MERKNAD OM DUPLIKAT TRANS. HVIS SUM *
      *                           STEMMER, GÅR TRANSEN VIDERE, HVIS   *
      *                           IKKE AVVISES DUPLIKATET.            *
      *                  25.04.96 LEGGER INN SEKV.NR FOR MOTKONTOTEKST*
      *                  04.06.96 TAKLER 7-SIFRET KID                 *
      *                  28.07.97 TILPASSET BRUK AV ALT. KONTONR.     *
      *                  02.09.97 TATT UT VEKSEL/AKSEPT-NR OG MASKIN- *
      *                           ORDRENR.                            *
      *                  05.06.98 FEILRETTING: ALT.KTO FEIL.          *
      *                  01.08.01 MERKER LINJER MED DUPLIKATER.       *
      *                  09.09.02 TATT UT ALT SOM HAR MED RABATT OG   *
      *                           RENTER Å GJØRE. LAGET NY MATCH PÅ   *
      *                           BELØP. NYE TEKSTER.                 *
      *                  11.09.02 RETTET FEIL: BEHANDLER IKKE FEIL-   *
      *                           TRANSER SOM DUPLIKAT.               *
      *                  27.10.03 ENDRET LINJESKIFT.                  *
      *                  14.09.05 TAKLER NEGATIVE BELØP FRA BBS       *
      *                  16.09.05 DUPLIKATER BLE BEHANDLET FEIL.      *
      *                  15.03.06 SKRIVER FIMANR TIL FIL HVIS FEIL    *
      *                           ELLER ANMERKNING PÅ BETALINGEN.     *
      *                           SKRIVER OPPGAVE REG36 NÅR U8 PÅ.    *
      *                  15.04.11 UTVIDET REGREC OG LAGT INN BELØP I  *
      *                           NYE UTVIDEDE FELT. NYE FELT BRUKES  *
      *                           NÅR U1 AR AV.                       *
      *  INPUT.........: INNBETALINGER FRA BANK OG POST (TILRELF).    *
      *  BEHANDLING....: LESER INNBETALINGER OG SAMMENLIGNER BELØP I  *
      *                  INNBETALING MOT TOTALBELØP PÅ RELFILE. HVIS  *
      *                  DISSE ER LIKE LAGES TRANSER FRA RELFILE,     *
      *                  ELLERS SKRIVES FEILLINJE.                    *
      *  OUTPUT........: TRANSER SOM MATCHER EN INNBETALING (FRARELF).*
      *****************************************************************
      *ILRELF IPEAF9360 120    2       DISK40 SYS011S
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RSK010.rpg
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
           SELECT TILRELF
               ASSIGN TO UT-S-TILRELF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TILRELF-STATUS.
           SELECT RELFILE
               ASSIGN TO UT-S-RELFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RELFILE-STATUS.
           SELECT RELMAST
               ASSIGN TO RELMAST
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS RELMAST-STATUS
               RECORD KEY IS RELMAST-KEY1.
           SELECT SYSPARM
               ASSIGN TO SYSPARM
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS SYSPARM-STATUS
               RECORD KEY IS SYSPARM-KEY1.
           SELECT KONTOMA
               ASSIGN TO KONTOMA
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KONTOMA-STATUS
               RECORD KEY IS KONTOMA-KEY1.
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
           SELECT FRARELF
               ASSIGN TO UT-S-FRARELF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FRARELF-STATUS.
           SELECT FEILFIL
               ASSIGN TO UT-S-FEILFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FEILFIL-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD TILRELF
               BLOCK CONTAINS 240
               RECORD CONTAINS 240.
       01  TILRELF-IO-AREA-2.
           05  TILRELF-IO-AREA-X           PICTURE X(240).
       FD RELFILE
               BLOCK CONTAINS 9420
               RECORD CONTAINS 60.
       01  RELFILE-IO-AREA.
           05  RELFILE-IO-AREA-X           PICTURE X(60).
       FD RELMAST
               RECORD CONTAINS 80.
       01  RELMAST-IO-AREA.
           05  RELMAST-IO-AREA-X.
               10  RELMAST-KEY1            PICTURE X(20).
               10  FILLER                  PICTURE X(60).
       FD SYSPARM
               RECORD CONTAINS 160.
       01  SYSPARM-IO-AREA.
           05  SYSPARM-IO-AREA-X.
               10  SYSPARM-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(150).
       FD KONTOMA
               RECORD CONTAINS 60.
       01  KONTOMA-IO-AREA.
           05  KONTOMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KONTOMA-KEY1            PICTURE X(7).
               10  FILLER                  PICTURE X(52).
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1            PICTURE X(9).
               10  FILLER                  PICTURE X(190).
      *RARELF O   F9360 120    2       DISK40 SYS013S                 U7
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       FD FRARELF
               BLOCK CONTAINS 240
               RECORD CONTAINS 240.
       01  FRARELF-IO-AREA.
           05  FRARELF-IO-AREA-X           PICTURE X(240).
      *BUGFILO O   F  80  80            PRINTERSYSLST
      * **START RBS - DATASTRUKTUR FOR SUB-PROGRAM RBS000 ********
       FD FEILFIL
               BLOCK CONTAINS 30
               RECORD CONTAINS 3.
       01  FEILFIL-IO-AREA.
           05  FEILFIL-IO-AREA-X           PICTURE X(3).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  TILRELF-STATUS              PICTURE 99 VALUE 0.
           10  RELFILE-STATUS              PICTURE 99 VALUE 0.
           10  RELMAST-STATUS              PICTURE 99 VALUE 0.
           10  SYSPARM-STATUS              PICTURE 99 VALUE 0.
           10  KONTOMA-STATUS              PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  FRARELF-STATUS              PICTURE 99 VALUE 0.
           10  FEILFIL-STATUS              PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  TILRELF-EOF-OFF         VALUE '0'.
               88  TILRELF-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  TILRELF-READ-OFF        VALUE '0'.
               88  TILRELF-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  TILRELF-PROCESS-OFF     VALUE '0'.
               88  TILRELF-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  TILRELF-LEVEL-INIT-OFF  VALUE '0'.
               88  TILRELF-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  TILRELF-AHEAD-EOF-OFF   VALUE '0'.
               88  TILRELF-AHEAD-EOF       VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  TILRELF-AHEAD-READ-OFF  VALUE '0'.
               88  TILRELF-AHEAD-READ      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RELFILE-EOF-OFF         VALUE '0'.
               88  RELFILE-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RELFILE-READ-OFF        VALUE '0'.
               88  RELFILE-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RELFILE-PROCESS-OFF     VALUE '0'.
               88  RELFILE-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  RELFILE-LEVEL-INIT-OFF  VALUE '0'.
               88  RELFILE-LEVEL-INIT      VALUE '1'.
           05  RELMAST-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  SYSPARM-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  KONTOMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  TILRELF-LEVEL-01.
               10  TILRELF-01-L4.
                   15  TILRELF-01-L4-FIRM  PICTURE X(3).
               10  TILRELF-01-L3.
                   15  TILRELF-01-L3-RESKNR PICTURE X(6).
               10  TILRELF-01-L2.
                   15  TILRELF-01-L2-REFNR PICTURE X(6).
               10  TILRELF-01-L1.
                   15  TILRELF-01-L1-BELCH PICTURE X(9).
           05  TILRELF-DATA-FIELDS.
               10  REC120                  PICTURE X(120).
               10  FIRM                    PICTURE X(3).
               10  BLGART                  PICTURE X(1).
               10  BLGNR                   PICTURE X(6).
               10  BLGDTO                  PICTURE X(6).
               10  RESKNR                  PICTURE X(6).
               10  BELO-ELGP-IO.
                   15  BELO-ELGP           PICTURE S9(7)V9(2).
               10  BELCH                   PICTURE X(9).
               10  MOTKTO                  PICTURE X(4).
               10  REFNR                   PICTURE X(6).
               10  FAKID                   PICTURE X(1).
               10  FORFDT                  PICTURE X(6).
               10  TXTSEQ                  PICTURE X(1).
               10  TEKST                   PICTURE X(24).
               10  TIL132-IO.
                   15  TIL132              PICTURE S9(11)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  VALUTA                  PICTURE X(8).
               10  VT3                     PICTURE X(3).
               10  OPPHAV                  PICTURE X(4).
               10  PRDDTO                  PICTURE X(5).
               10  PRDKLK                  PICTURE X(4).
               10  NXTFIR                  PICTURE X(3).
               10  NXTRSK                  PICTURE X(6).
               10  NXTREF                  PICTURE X(6).
               10  NXTBEL-IO.
                   15  NXTBEL              PICTURE S9(7)V9(2).
           05  TILRELF-MP                  PICTURE X(24).
           05  TILRELF-MC                  PICTURE X(24).
           05  TILRELF-M-01            REDEFINES TILRELF-MC.
               10  TILRELF-M-01-M4.
                   15  TILRELF-M-01-M4-FIRM-G.
                       20  TILRELF-M-01-M4-FIRM PICTURE X(3).
               10  TILRELF-M-01-M3.
                   15  TILRELF-M-01-M3-RESKNR-G.
                       20  TILRELF-M-01-M3-RESKNR PICTURE X(6).
               10  TILRELF-M-01-M2.
                   15  TILRELF-M-01-M2-REFNR-G.
                       20  TILRELF-M-01-M2-REFNR PICTURE X(6).
               10  TILRELF-M-01-M1.
                   15  TILRELF-M-01-M1-BELCH-G.
                       20  TILRELF-M-01-M1-BELCH PICTURE X(9).
           05  RELFILE-LEVEL-02.
               10  RELFILE-02-L4.
                   15  RELFILE-02-L4-FNR   PICTURE X(3).
               10  RELFILE-02-L3.
                   15  RELFILE-02-L3-RNR   PICTURE X(6).
               10  RELFILE-02-L2.
                   15  RELFILE-02-L2-BNR   PICTURE X(6).
               10  RELFILE-02-L1.
                   15  RELFILE-02-L1-BTOCH PICTURE X(9).
           05  RELFILE-DATA-FIELDS.
               10  REC060                  PICTURE X(60).
               10  FNR                     PICTURE X(3).
               10  RNR                     PICTURE X(6).
               10  BNR                     PICTURE X(6).
               10  RFNR                    PICTURE X(6).
               10  BEL-IO.
                   15  BEL                 PICTURE S9(7)V9(2).
               10  BTOCH                   PICTURE X(9).
               10  TKODE                   PICTURE X(2).
           05  RELFILE-MP                  PICTURE X(24).
           05  RELFILE-MC                  PICTURE X(24).
           05  RELFILE-M-02            REDEFINES RELFILE-MC.
               10  RELFILE-M-02-M4.
                   15  RELFILE-M-02-M4-FNR-G.
                       20  RELFILE-M-02-M4-FNR PICTURE X(3).
               10  RELFILE-M-02-M3.
                   15  RELFILE-M-02-M3-RNR-G.
                       20  RELFILE-M-02-M3-RNR PICTURE X(6).
               10  RELFILE-M-02-M2.
                   15  RELFILE-M-02-M2-BNR-G.
                       20  RELFILE-M-02-M2-BNR PICTURE X(6).
               10  RELFILE-M-02-M1.
                   15  RELFILE-M-02-M1-BTOCH-G.
                       20  RELFILE-M-02-M1-BTOCH PICTURE X(9).
           05  RELMAST-DATA-FIELDS.
               10  RMRESK                  PICTURE X(6).
               10  RMBEL-IO.
                   15  RMBEL               PICTURE S9(7)V9(2).
           05  SYSPARM-DATA-FIELDS.
               10  SYSALT                  PICTURE X(1).
               10  SYSOKD                  PICTURE X(1).
               10  SYSOKT                  PICTURE X(4).
           05  KONTOMA-DATA-FIELDS.
               10  KTONAV                  PICTURE X(35).
               10  ALTKTO                  PICTURE X(4).
           05  KUNDEMA-DATA-FIELDS.
               10  FILLER                  PICTURE X.
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L4            PICTURE X(3).
               10  THE-PRIOR-L3            PICTURE X(6).
               10  THE-PRIOR-L2            PICTURE X(6).
               10  THE-PRIOR-L1            PICTURE X(9).
           05  TEMPORARY-FIELDS.
               10  BELL4A-IO.
                   15  BELL4A              PICTURE S9(8)V9(2).
               10  BEUL4A-IO.
                   15  BEUL4A              PICTURE S9(11)V9(2).
               10  BELL4B-IO.
                   15  BELL4B              PICTURE S9(8)V9(2).
               10  BEUL4B-IO.
                   15  BEUL4B              PICTURE S9(11)V9(2).
               10  DIFKTO                  PICTURE X(4).
               10  SYSKEY                  PICTURE X(10).
               10  KTOKEY                  PICTURE X(7).
               10  DIFNAV                  PICTURE X(35).
               10  DIFAKT                  PICTURE X(4).
               10  ANTL-IO.
                   15  ANTL                PICTURE S9(3).
               10  BLGPRT-IO.
                   15  BLGPRT              PICTURE S9(6).
               10  R25132-IO.
                   15  R25132              PICTURE S9(11)V9(2).
               10  R12132-IO.
                   15  R12132              PICTURE S9(11)V9(2).
               10  DIFFL3-IO.
                   15  DIFFL3              PICTURE S9(8)V9(2).
               10  DIFFPR-IO.
                   15  DIFFPR              PICTURE S9(8)V9(2).
               10  DIFUL3-IO.
                   15  DIFUL3              PICTURE S9(11)V9(2).
               10  DIFFPU-IO.
                   15  DIFFPU              PICTURE S9(11)V9(2).
               10  RELKY4                  PICTURE X(4).
               10  RELKEY                  PICTURE X(20).
               10  RELK16                  PICTURE X(16).
               10  FAKBEL-IO.
                   15  FAKBEL              PICTURE S9(7)V9(2).
               10  FAK132-IO.
                   15  FAK132              PICTURE S9(11)V9(2).
               10  KUNKEY                  PICTURE X(9).
           05  EDITTING-FIELDS.
               10  XO-112P-EF.
                 15  XO-112P               PICTURE S9(11)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  EDIT-BELO-ELGP          PICTURE Z.ZZZ.ZZZ,ZZ-.
               10  XO-72YY9R               PICTURE Z.ZZZ.ZZZ,99-.
               10  XO-112D                 PICTURE S9(11)V9(2).
               10  XO-112U                 PICTURE 9(11)V9(2).
               10  XO-112YY9R              PICTURE ZZ.ZZZ.ZZZ.ZZZ,99-.
               10  EDIT-BELL4A             PICTURE ZZ.ZZZ.ZZZ,ZZ-.
               10  EDIT-BEUL4A             PICTURE ZZZZZ.ZZZ.ZZZ,ZZ-.
               10  EDIT-BELL4B             PICTURE ZZ.ZZZ.ZZZ,ZZ-.
               10  EDIT-DIFFPR             PICTURE ZZ.ZZZ.ZZZ,ZZ-.
               10  EDIT-DIFFPU             PICTURE ZZZZZ.ZZZ.ZZZ,ZZ-.
           05  TILRELF-IO-AREA.
               10  FILLER                  PICTURE X(240).
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
           SET NOT-I-02                    TO TRUE
           SET NOT-I-10                    TO TRUE
           SET NOT-I-05                    TO TRUE
           SET NOT-I-06                    TO TRUE
           SET NOT-I-09                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  TILRELF-PROCESS
               SET TILRELF-PROCESS-OFF     TO TRUE
               SET TILRELF-READ            TO TRUE
           END-IF
 
           IF  TILRELF-READ
               PERFORM TILRELF-GET
               SET TILRELF-READ-OFF        TO TRUE
               IF  NOT TILRELF-EOF
                   PERFORM TILRELF-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM TILRELF-MATCH-SET
               END-IF
           END-IF
 
           IF  RELFILE-PROCESS
               SET RELFILE-PROCESS-OFF     TO TRUE
               SET RELFILE-READ            TO TRUE
           END-IF
 
           IF  RELFILE-READ
               PERFORM RELFILE-GET
               SET RELFILE-READ-OFF        TO TRUE
               IF  NOT RELFILE-EOF
                   PERFORM RELFILE-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM RELFILE-MATCH-SET
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
 
           IF  TILRELF-PROCESS
               PERFORM TILRELF-IDSET
           END-IF
 
           IF  RELFILE-PROCESS
               PERFORM RELFILE-IDSET
           END-IF
 
           IF  TILRELF-PROCESS
               PERFORM TILRELF-CHK-LEVEL
           END-IF
 
           IF  RELFILE-PROCESS
               PERFORM RELFILE-CHK-LEVEL
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
 
           IF  TILRELF-PROCESS
               PERFORM TILRELF-FLDSET
           END-IF
 
           IF  RELFILE-PROCESS
               PERFORM RELFILE-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  TILRELF-PROCESS
           OR  RELFILE-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-50                    TO TRUE
           SET NOT-I-51                    TO TRUE
           IF  (I-01)
               SET NOT-I-22                TO TRUE
               SET NOT-I-24                TO TRUE
               SET NOT-I-62                TO TRUE
               SET NOT-I-70                TO TRUE
               SET NOT-I-71                TO TRUE
               SET NOT-I-72                TO TRUE
               SET NOT-I-73                TO TRUE
               SET NOT-I-74                TO TRUE
               SET NOT-I-75                TO TRUE
               SET NOT-I-76                TO TRUE
           END-IF
           IF  (I-91)
               SET NOT-I-90                TO TRUE
               SET NOT-I-91                TO TRUE
           END-IF
           IF  (I-L4)
               SET I-90                    TO TRUE
               SET NOT-I-92                TO TRUE
               MOVE 0                      TO BELL4A
               MOVE 0                      TO BEUL4A
               MOVE 0                      TO BELL4B
               MOVE 0                      TO BEUL4B
      ***  LESER RESKONTROPARAMETER OG SJEKKER OM AVVIST OCR SKAL
      ***  POSTERES.
           END-IF
           IF  (I-L4)
               SET NOT-I-63                TO TRUE
               SET NOT-I-64                TO TRUE
               SET NOT-I-65                TO TRUE
               SET NOT-I-66                TO TRUE
               SET NOT-I-88                TO TRUE
               MOVE '    '                 TO DIFKTO
           END-IF
           IF  (I-L4 AND I-01)
               MOVE FIRM                   TO SYSKEY (1:3)
           END-IF
           IF  (I-L4 AND I-02)
               MOVE FNR                    TO SYSKEY (1:3)
           END-IF
           IF  (I-L4)
               MOVE 'RES*011'              TO SYSKEY (4:7)
               MOVE SYSKEY                 TO SYSPARM-KEY1
               READ SYSPARM RECORD KEY IS SYSPARM-KEY1
               INVALID KEY
                   SET I-63                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-63            TO TRUE
                   PERFORM SYSPARM-FLDSET
                   PERFORM SYSPARM-IDSET
               END-READ
           END-IF
           IF  (I-L4 AND NOT-I-63)
               SET NOT-I-64                TO TRUE
               IF  SYSOKD = 'B'
                   SET I-64                TO TRUE
               END-IF
           END-IF
           IF  (I-L4 AND NOT-I-63 AND NOT-I-64)
               SET NOT-I-65                TO TRUE
               IF  SYSOKD = 'J'
                   SET I-65                TO TRUE
               END-IF
           END-IF
           IF  (I-L4 AND NOT-I-63 AND I-65)
               SET NOT-I-65                TO TRUE
               SET NOT-I-66                TO TRUE
               IF  SYSOKT NOT > '0999'
                   SET I-66                TO TRUE
               END-IF
               IF  SYSOKT > '0999'
                   SET I-65                TO TRUE
               END-IF
           END-IF
           IF  (I-L4 AND NOT-I-63)
               MOVE SYSOKT                 TO DIFKTO
      ***  LESER REGNSKAPSPARAMETER OG SJEKKER OM ALT.KTO BENYTTES  ***
           END-IF
           IF  (I-L4)
               SET NOT-I-60                TO TRUE
               SET NOT-I-61                TO TRUE
           END-IF
           IF  (I-L4 AND I-01)
               MOVE FIRM                   TO SYSKEY (1:3)
           END-IF
           IF  (I-L4 AND I-02)
               MOVE FNR                    TO SYSKEY (1:3)
           END-IF
           IF  (I-L4)
               MOVE 'REGA011'              TO SYSKEY (4:7)
               MOVE SYSKEY                 TO SYSPARM-KEY1
               READ SYSPARM RECORD KEY IS SYSPARM-KEY1
               INVALID KEY
                   SET I-60                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-60            TO TRUE
                   PERFORM SYSPARM-FLDSET
                   PERFORM SYSPARM-IDSET
               END-READ
           END-IF
           IF  (I-L4 AND NOT-I-60)
               SET NOT-I-61                TO TRUE
               IF  SYSALT = 'J'
                   SET I-61                TO TRUE
               END-IF
      ***  HENTER ALTERNATIVT KONTONR FRA KONTOMA HVIS DET BENYTTES  ***
           END-IF
           IF  (I-L4)
               SET NOT-I-62                TO TRUE
           END-IF
           IF  (I-L4 AND I-65 AND I-01)
               MOVE FIRM                   TO KTOKEY (1:3)
           END-IF
           IF  (I-L4 AND I-65 AND I-02)
               MOVE FNR                    TO KTOKEY (1:3)
           END-IF
           IF  (I-L4 AND I-65)
               MOVE DIFKTO                 TO KTOKEY (4:4)
               MOVE KTOKEY                 TO KONTOMA-KEY1
               READ KONTOMA RECORD KEY IS KONTOMA-KEY1
               INVALID KEY
                   SET I-62                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-62            TO TRUE
                   PERFORM KONTOMA-FLDSET
                   PERFORM KONTOMA-IDSET
               END-READ
           END-IF
           IF  (I-L4 AND I-65 AND I-62)
               MOVE '*UKJENT*'             TO DIFNAV (1:8)
           END-IF
           IF  (I-L4 AND I-65 AND NOT-I-62)
               MOVE KTONAV                 TO DIFNAV
           END-IF
           IF  (I-L4 AND I-65 AND I-61)
               MOVE ALTKTO                 TO DIFAKT
           END-IF
           IF  (I-L4 AND I-65 AND I-62)
               SET I-66                    TO TRUE
               SET NOT-I-65                TO TRUE
           END-IF
           IF  (I-L4)
               SET NOT-I-62                TO TRUE
           END-IF
           IF  (I-01 AND I-90)
               SET I-91                    TO TRUE
               SET I-92                    TO TRUE
               PERFORM RBSRUT-S
           END-IF
           IF  (I-L1)
               SET NOT-I-11                TO TRUE
               SET NOT-I-12                TO TRUE
               SET NOT-I-25                TO TRUE
               SUBTRACT ANTL               FROM ANTL
           END-IF
           IF  (I-01 AND I-11)
               SET I-12                    TO TRUE
           END-IF
           IF  (I-01 AND I-12)
               SET I-88                    TO TRUE
           END-IF
           IF  (I-01)
               SET NOT-I-11                TO TRUE
               IF  FIRM = NXTFIR
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-11)
               SET NOT-I-11                TO TRUE
               IF  RESKNR = NXTRSK
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-11)
               SET NOT-I-11                TO TRUE
               IF  REFNR = NXTREF
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-11)
               SET NOT-I-11                TO TRUE
               IF  BELO-ELGP = NXTBEL
                   SET I-11                TO TRUE
               END-IF
           END-IF
           MOVE BLGDTO                     TO BLGPRT-IO
           IF  (I-01)
               ADD BELO-ELGP               TO BELL4A
               ADD TIL132                  TO BEUL4A
      ***  HENTER ALTERNATIVT KONTONR FRA KONTOMA HVIS DET BENYTTES  ***
           END-IF
           IF  (I-01 AND I-61)
               MOVE FIRM                   TO KTOKEY (1:3)
               MOVE MOTKTO                 TO KTOKEY (4:4)
               MOVE KTOKEY                 TO KONTOMA-KEY1
               READ KONTOMA RECORD KEY IS KONTOMA-KEY1
               INVALID KEY
                   SET I-62                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-62            TO TRUE
                   PERFORM KONTOMA-FLDSET
                   PERFORM KONTOMA-IDSET
               END-READ
           END-IF
           IF  (I-01)
               SET NOT-I-50                TO TRUE
               IF  FAKID = 'F'
                   SET I-50                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-50)
               PERFORM FKRUT-S
           END-IF
           IF  (I-01 AND I-50 AND NOT-I-22)
               AND (NOT-I-51)
               SET NOT-I-11                TO TRUE
           END-IF
           IF  (I-01 AND I-22 AND NOT-I-65)
               AND (NOT-I-51)
               SET NOT-I-11                TO TRUE
           END-IF
           IF  (I-50 AND NOT-I-22 AND NOT-I-51)
               AND (I-01)
               SET I-88                    TO TRUE
           END-IF
           IF  (I-01 AND I-22)
               SET I-88                    TO TRUE
           END-IF
           IF  (I-01 AND I-50)
               GO TO SLUTT-T
           END-IF
           IF  (I-01 AND NOT-I-MR)
               PERFORM LESKUN-S
           END-IF
           IF  (I-01 AND NOT-I-MR AND NOT-I-24)
               AND (NOT-I-50)
               SET I-70                    TO TRUE
           END-IF
           IF  (I-01 AND NOT-I-MR AND NOT-I-24)
               AND (NOT-I-64 AND NOT-I-65)
               SET I-71                    TO TRUE
           END-IF
           IF  (I-01 AND NOT-I-MR AND NOT-I-24)
               AND (I-64)
               OR  (I-01 AND NOT-I-MR AND NOT-I-24)
               AND (I-65)
               SET I-72                    TO TRUE
           END-IF
           IF  (I-01 AND NOT-I-MR AND I-24)
               AND (NOT-I-65)
               SET I-73                    TO TRUE
           END-IF
           IF  (I-01 AND NOT-I-MR AND I-24)
               AND (I-65)
               SET I-74                    TO TRUE
           END-IF
           IF  (I-01 AND I-71)
               OR  (I-01 AND I-73)
               SET NOT-I-11                TO TRUE
           END-IF
           IF  (I-02 AND I-MR)
               SET I-25                    TO TRUE
           END-IF
           IF  (I-01 AND I-70)
               SET I-88                    TO TRUE
           END-IF
           IF  (I-01 AND I-71)
               SET I-88                    TO TRUE
           END-IF
           IF  (I-01 AND I-72)
               SET I-88                    TO TRUE
           END-IF
           IF  (I-01 AND I-73)
               SET I-88                    TO TRUE
           END-IF
           IF  (I-01 AND I-74)
               SET I-88                    TO TRUE
           END-IF
           IF  (I-02 AND I-25)
               ADD BEL                     TO BELL4B
      *  "BEL" SKAL ERSTATTES AV UTVIDET BELØP I RELFILE
           END-IF
           IF  (I-02 AND I-25)
               ADD BEL TO ZERO         GIVING R25132
               ADD R25132                  TO BEUL4B
           END-IF
           IF  (I-02 AND I-12 AND I-MR)
               ADD BEL                     TO BELL4B
               ADD BEL TO ZERO         GIVING R12132
           END-IF
           IF  (I-01 AND I-72)
               ADD BELO-ELGP               TO BELL4B
               ADD TIL132                  TO BEUL4B
           END-IF.
 
       SLUTT-T.
      *4 88                MOVE "REC120  "BUGFL2  8        LEDETXT DEBUG
      *4 88      BUGFL2    DEBUGBUGFILO   REC120           VIS FELT/IND
           CONTINUE.
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE ' '                        TO BBEST
           MOVE 'REG01'                    TO LONR
           IF  (I-U8)
               MOVE 'REG36'                TO LONR
           END-IF
           MOVE FIRM                       TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'RSK010  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF.
      *
      *****************************************************************
      * RUTINE FOR HÅNDTERING AV FAKTURA-KID                          *
      *****************************************************************
 
       FKRUT-S SECTION.
       FKRUT-S-P.
           MOVE 'A'                        TO RELKY4 (1:1)
           MOVE FIRM                       TO RELKY4 (2:3)
           MOVE RELKY4                     TO RELKEY (1:4)
           MOVE REFNR                      TO RELK16 (1:6)
           MOVE '000'                      TO RELK16 (14:3)
           MOVE RELK16                     TO RELKEY (5:16)
           MOVE RELKEY                     TO RELMAST-KEY1
           READ RELMAST RECORD KEY IS RELMAST-KEY1
           INVALID KEY
               SET I-22                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-22                TO TRUE
               PERFORM RELMAST-FLDSET
               PERFORM RELMAST-IDSET
           END-READ
           IF  (NOT-I-22)
               MULTIPLY -1 BY RMBEL    GIVING FAKBEL
               SET NOT-I-51                TO TRUE
               IF  BELO-ELGP = FAKBEL
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-22 AND I-51)
               ADD FAKBEL                  TO BELL4B
               ADD FAKBEL TO ZERO      GIVING FAK132
               ADD FAK132                  TO BEUL4B
           END-IF
           IF  (NOT-I-22 AND NOT-I-51 AND I-64)
               SET I-75                    TO TRUE
           END-IF
           IF  (NOT-I-22 AND NOT-I-51 AND I-65)
               SET I-75                    TO TRUE
           END-IF
           IF  (I-75)
               ADD BELO-ELGP               TO BELL4B
               ADD TIL132                  TO BEUL4B
           END-IF
           IF  (I-22 AND I-65)
               SET I-76                    TO TRUE
           END-IF.
      *
      *****************************************************************
      * LESER KUNDEMASTER OG SJEKKER GYLDIG RESKNR                    *
      *****************************************************************
 
       LESKUN-S SECTION.
       LESKUN-S-P.
           MOVE FIRM                       TO KUNKEY (1:3)
           MOVE RESKNR                     TO KUNKEY (4:6)
           MOVE KUNKEY                     TO KUNDEMA-KEY1
           READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
           INVALID KEY
               SET I-24                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-24                TO TRUE
               PERFORM KUNDEMA-IDSET
           END-READ.
      * MATCHENDE GIRO - RIKTIG GIROKID OG RIKTIG GIROBELØP
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L4)
               SUBTRACT BELL4B FROM BELL4A GIVING DIFFL3
               SET NOT-I-20                TO TRUE
               IF  DIFFL3 = 0
                   SET I-20                TO TRUE
               END-IF
               MOVE DIFFL3                 TO DIFFPR-IO
               SUBTRACT BEUL4B FROM BEUL4A GIVING DIFUL3
               SET NOT-I-20                TO TRUE
               IF  DIFUL3 = 0
                   SET I-20                TO TRUE
               END-IF
               MOVE DIFUL3                 TO DIFFPU-IO
               MOVE 0,00                   TO DIFFL3
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           END-IF
           .
 
       TILRELF-GET SECTION.
       TILRELF-GET-P.
           IF  TILRELF-EOF-OFF
               IF  TILRELF-AHEAD-EOF-OFF
                   IF  TILRELF-AHEAD-READ-OFF
                       SET TILRELF-AHEAD-READ TO TRUE
                       READ TILRELF
                       AT END
                           SET TILRELF-AHEAD-EOF TO TRUE
                           INITIALIZE TILRELF-IO-AREA-2
                       END-READ
                   END-IF
                   MOVE TILRELF-IO-AREA-2  TO TILRELF-IO-AREA
                   IF  TILRELF-AHEAD-EOF-OFF
                       READ TILRELF
                       AT END
                           SET TILRELF-AHEAD-EOF TO TRUE
                           INITIALIZE TILRELF-IO-AREA-2
                       END-READ
                   ELSE
                       SET TILRELF-EOF     TO TRUE
                       SUBTRACT 1        FROM LR-CHECK
                   END-IF
                   PERFORM TILRELF-AHEAD-FLDSET
               ELSE
                   SET TILRELF-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-IF
           END-IF.
 
       TILRELF-FLDSET SECTION.
       TILRELF-FLDSET-P.
           EVALUATE TRUE
           WHEN ( TILRELF-IO-AREA (1:1) = '2'
            AND   TILRELF-IO-AREA (2:1) = '1' )
               MOVE TILRELF-IO-AREA (1:120) TO REC120 (1:120)
               MOVE TILRELF-IO-AREA (3:3)  TO FIRM (1:3)
               MOVE TILRELF-IO-AREA (6:1)  TO BLGART (1:1)
               MOVE TILRELF-IO-AREA (7:6)  TO BLGNR (1:6)
               MOVE TILRELF-IO-AREA (13:6) TO BLGDTO (1:6)
               MOVE TILRELF-IO-AREA (19:6) TO RESKNR (1:6)
               MOVE TILRELF-IO-AREA (27:9) TO BELO-ELGP-IO
               INSPECT BELO-ELGP-IO REPLACING ALL ' ' BY '0'
               MOVE TILRELF-IO-AREA (27:9) TO BELCH (1:9)
               MOVE TILRELF-IO-AREA (37:4) TO MOTKTO (1:4)
               MOVE TILRELF-IO-AREA (41:6) TO REFNR (1:6)
               MOVE TILRELF-IO-AREA (47:1) TO FAKID (1:1)
               MOVE TILRELF-IO-AREA (59:6) TO FORFDT (1:6)
               MOVE TILRELF-IO-AREA (96:1) TO TXTSEQ (1:1)
               MOVE TILRELF-IO-AREA (97:24) TO TEKST (1:24)
               MOVE TILRELF-IO-AREA (121:7) TO TIL132-IO
               MOVE TILRELF-IO-AREA (128:8) TO VALUTA (1:8)
               MOVE TILRELF-IO-AREA (136:3) TO VT3 (1:3)
               MOVE TILRELF-IO-AREA (228:4) TO OPPHAV (1:4)
               MOVE TILRELF-IO-AREA (232:5) TO PRDDTO (1:5)
               MOVE TILRELF-IO-AREA (237:4) TO PRDKLK (1:4)
           END-EVALUATE.
 
       TILRELF-IDCHK SECTION.
       TILRELF-IDCHK-P.
           EVALUATE TRUE
           WHEN ( TILRELF-IO-AREA (1:1) = '2'
            AND   TILRELF-IO-AREA (2:1) = '1' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       TILRELF-IDSET SECTION.
       TILRELF-IDSET-P.
           EVALUATE TRUE
           WHEN ( TILRELF-IO-AREA (1:1) = '2'
            AND   TILRELF-IO-AREA (2:1) = '1' )
               SET I-01                    TO TRUE
           END-EVALUATE.
 
       TILRELF-CHK-LEVEL SECTION.
       TILRELF-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( TILRELF-IO-AREA (1:1) = '2'
            AND   TILRELF-IO-AREA (2:1) = '1' )
               MOVE LOW-VALUES             TO TILRELF-LEVEL-01
               MOVE TILRELF-IO-AREA (3:3)  TO TILRELF-01-L4-FIRM
               MOVE TILRELF-IO-AREA (19:6) TO TILRELF-01-L3-RESKNR
               MOVE TILRELF-IO-AREA (41:6) TO TILRELF-01-L2-REFNR
               MOVE TILRELF-IO-AREA (27:9) TO TILRELF-01-L1-BELCH
               IF  TILRELF-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  TILRELF-01-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   WHEN  TILRELF-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  TILRELF-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  TILRELF-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  TILRELF-01-L4         TO THE-PRIOR-L4
               MOVE  TILRELF-01-L3         TO THE-PRIOR-L3
               MOVE  TILRELF-01-L2         TO THE-PRIOR-L2
               MOVE  TILRELF-01-L1         TO THE-PRIOR-L1
               SET TILRELF-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       TILRELF-MATCH-SET SECTION.
       TILRELF-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( TILRELF-IO-AREA (1:1) = '2'
            AND   TILRELF-IO-AREA (2:1) = '1' )
               MOVE TILRELF-IO-AREA (3:3)  TO TILRELF-M-01-M4-FIRM
               MOVE TILRELF-IO-AREA (19:6) TO TILRELF-M-01-M3-RESKNR
               MOVE TILRELF-IO-AREA (41:6) TO TILRELF-M-01-M2-REFNR
               MOVE TILRELF-IO-AREA (27:9) TO TILRELF-M-01-M1-BELCH
           END-EVALUATE.
 
       TILRELF-AHEAD-FLDSET SECTION.
       TILRELF-AHEAD-FLDSET-P.
           MOVE TILRELF-IO-AREA-2 (3:3)    TO NXTFIR (1:3)
           MOVE TILRELF-IO-AREA-2 (19:6)   TO NXTRSK (1:6)
           MOVE TILRELF-IO-AREA-2 (41:6)   TO NXTREF (1:6)
           MOVE TILRELF-IO-AREA-2 (27:9)   TO NXTBEL-IO.
 
       RELFILE-GET SECTION.
       RELFILE-GET-P.
           IF  RELFILE-EOF-OFF
               READ RELFILE
               AT END
                   SET RELFILE-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RELFILE-FLDSET SECTION.
       RELFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ( RELFILE-IO-AREA (1:1) = '4'
            AND   RELFILE-IO-AREA (2:1) = '0' )
               MOVE RELFILE-IO-AREA (1:60) TO REC060 (1:60)
               MOVE RELFILE-IO-AREA (3:3)  TO FNR (1:3)
               MOVE RELFILE-IO-AREA (6:6)  TO RNR (1:6)
               MOVE RELFILE-IO-AREA (12:6) TO BNR (1:6)
               MOVE RELFILE-IO-AREA (18:6) TO RFNR (1:6)
               MOVE RELFILE-IO-AREA (24:9) TO BEL-IO
               INSPECT BEL-IO REPLACING ALL ' ' BY '0'
               MOVE RELFILE-IO-AREA (40:9) TO BTOCH (1:9)
               MOVE RELFILE-IO-AREA (59:2) TO TKODE (1:2)
           END-EVALUATE.
 
       RELFILE-IDCHK SECTION.
       RELFILE-IDCHK-P.
           EVALUATE TRUE
           WHEN ( RELFILE-IO-AREA (1:1) = '4'
            AND   RELFILE-IO-AREA (2:1) = '0' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       RELFILE-IDSET SECTION.
       RELFILE-IDSET-P.
           EVALUATE TRUE
           WHEN ( RELFILE-IO-AREA (1:1) = '4'
            AND   RELFILE-IO-AREA (2:1) = '0' )
               SET I-02                    TO TRUE
           END-EVALUATE.
 
       RELFILE-CHK-LEVEL SECTION.
       RELFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( RELFILE-IO-AREA (1:1) = '4'
            AND   RELFILE-IO-AREA (2:1) = '0' )
               MOVE LOW-VALUES             TO RELFILE-LEVEL-02
               MOVE RELFILE-IO-AREA (3:3)  TO RELFILE-02-L4-FNR
               MOVE RELFILE-IO-AREA (6:6)  TO RELFILE-02-L3-RNR
               MOVE RELFILE-IO-AREA (12:6) TO RELFILE-02-L2-BNR
               MOVE RELFILE-IO-AREA (40:9) TO RELFILE-02-L1-BTOCH
               IF  RELFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  RELFILE-02-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   WHEN  RELFILE-02-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  RELFILE-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  RELFILE-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  RELFILE-02-L4         TO THE-PRIOR-L4
               MOVE  RELFILE-02-L3         TO THE-PRIOR-L3
               MOVE  RELFILE-02-L2         TO THE-PRIOR-L2
               MOVE  RELFILE-02-L1         TO THE-PRIOR-L1
               SET RELFILE-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       RELFILE-MATCH-SET SECTION.
       RELFILE-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( RELFILE-IO-AREA (1:1) = '4'
            AND   RELFILE-IO-AREA (2:1) = '0' )
               MOVE RELFILE-IO-AREA (3:3)  TO RELFILE-M-02-M4-FNR
               MOVE RELFILE-IO-AREA (6:6)  TO RELFILE-M-02-M3-RNR
               MOVE RELFILE-IO-AREA (12:6) TO RELFILE-M-02-M2-BNR
               MOVE RELFILE-IO-AREA (40:9) TO RELFILE-M-02-M1-BTOCH
           END-EVALUATE.
 
       RELMAST-FLDSET SECTION.
       RELMAST-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RELMAST-IO-AREA (21:6) TO RMRESK (1:6)
               MOVE RELMAST-IO-AREA (33:9) TO RMBEL-IO
               INSPECT RMBEL-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       RELMAST-IDSET SECTION.
       RELMAST-IDSET-P.
           SET I-10                        TO TRUE.
 
       SYSPARM-FLDSET SECTION.
       SYSPARM-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE SYSPARM-IO-AREA (026:1) TO SYSALT (1:1)
               MOVE SYSPARM-IO-AREA (063:1) TO SYSOKD (1:1)
               MOVE SYSPARM-IO-AREA (064:4) TO SYSOKT (1:4)
           END-EVALUATE.
 
       SYSPARM-IDSET SECTION.
       SYSPARM-IDSET-P.
           SET I-05                        TO TRUE.
 
       KONTOMA-FLDSET SECTION.
       KONTOMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KONTOMA-IO-AREA (10:35) TO KTONAV (1:35)
               MOVE KONTOMA-IO-AREA (45:4) TO ALTKTO (1:4)
           END-EVALUATE.
 
       KONTOMA-IDSET SECTION.
       KONTOMA-IDSET-P.
           SET I-06                        TO TRUE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-09                        TO TRUE.
 
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
           IF  TILRELF-EOF
               MOVE HIGH-VALUES            TO TILRELF-MC
                                              TILRELF-MP
           END-IF
           IF  RELFILE-EOF
               MOVE HIGH-VALUES            TO RELFILE-MC
                                              RELFILE-MP
           END-IF
           IF  TILRELF-MC < TILRELF-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  RELFILE-MC < RELFILE-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  TILRELF-MC < RELFILE-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET TILRELF-PROCESS     TO TRUE
                   MOVE TILRELF-MC         TO TILRELF-MP
                   IF  TILRELF-MC = RELFILE-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  RELFILE-MC < TILRELF-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET RELFILE-PROCESS     TO TRUE
                   MOVE RELFILE-MC         TO RELFILE-MP
                   IF  RELFILE-MC = TILRELF-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  TILRELF-MC = RELFILE-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET TILRELF-PROCESS     TO TRUE
                   MOVE TILRELF-MC         TO TILRELF-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-25)
               MOVE SPACES TO FRARELF-IO-AREA
               INITIALIZE FRARELF-IO-AREA
               MOVE '21'                   TO FRARELF-IO-AREA (1:2)
               MOVE FIRM                   TO FRARELF-IO-AREA (3:3)
               MOVE BLGART                 TO FRARELF-IO-AREA (6:1)
               MOVE BLGNR                  TO FRARELF-IO-AREA (7:6)
               MOVE BLGDTO                 TO FRARELF-IO-AREA (13:6)
               MOVE RESKNR                 TO FRARELF-IO-AREA (19:6)
               MOVE '  '                   TO FRARELF-IO-AREA (25:2)
               MOVE BEL-IO                 TO FRARELF-IO-AREA (27:9)
               MOVE ' '                    TO FRARELF-IO-AREA (36:1)
               MOVE MOTKTO                 TO FRARELF-IO-AREA (37:4)
               MOVE RFNR                   TO FRARELF-IO-AREA (41:6)
               MOVE FORFDT                 TO FRARELF-IO-AREA (59:6)
               MOVE ' '                    TO FRARELF-IO-AREA (65:1)
               MOVE '31'                   TO FRARELF-IO-AREA (83:2)
               MOVE TKODE                  TO FRARELF-IO-AREA (88:2)
               MOVE TXTSEQ                 TO FRARELF-IO-AREA (96:1)
               MOVE TEKST                  TO FRARELF-IO-AREA (97:24)
               MOVE R25132                 TO XO-112P
               MOVE XO-112P-EF             TO FRARELF-IO-AREA (121:7)
               MOVE VALUTA                 TO FRARELF-IO-AREA (128:8)
               MOVE VT3                    TO FRARELF-IO-AREA (136:3)
               MOVE OPPHAV                 TO FRARELF-IO-AREA (228:4)
               MOVE 'R'                    TO FRARELF-IO-AREA (231:1)
               MOVE PRDDTO                 TO FRARELF-IO-AREA (232:5)
               MOVE PRDKLK                 TO FRARELF-IO-AREA (237:4)
      * DUPLIKAT GIRO - DOBBEL INNBETALING
               IF  I-U7
                   WRITE FRARELF-IO-AREA
               END-IF
           END-IF
           IF  (I-02 AND I-12 AND I-MR)
               MOVE SPACES TO FRARELF-IO-AREA
               INITIALIZE FRARELF-IO-AREA
               MOVE '21'                   TO FRARELF-IO-AREA (1:2)
               MOVE FIRM                   TO FRARELF-IO-AREA (3:3)
               MOVE BLGART                 TO FRARELF-IO-AREA (6:1)
               MOVE BLGNR                  TO FRARELF-IO-AREA (7:6)
               MOVE BLGDTO                 TO FRARELF-IO-AREA (13:6)
               MOVE RESKNR                 TO FRARELF-IO-AREA (19:6)
               MOVE '  '                   TO FRARELF-IO-AREA (25:2)
               MOVE BEL-IO                 TO FRARELF-IO-AREA (27:9)
               MOVE ' '                    TO FRARELF-IO-AREA (36:1)
               MOVE MOTKTO                 TO FRARELF-IO-AREA (37:4)
               MOVE RFNR                   TO FRARELF-IO-AREA (41:6)
               MOVE FORFDT                 TO FRARELF-IO-AREA (59:6)
               MOVE ' '                    TO FRARELF-IO-AREA (65:1)
               MOVE '31'                   TO FRARELF-IO-AREA (83:2)
               MOVE TKODE                  TO FRARELF-IO-AREA (88:2)
               MOVE TXTSEQ                 TO FRARELF-IO-AREA (96:1)
               MOVE TEKST                  TO FRARELF-IO-AREA (97:24)
               MOVE R12132                 TO XO-112P
               MOVE XO-112P-EF             TO FRARELF-IO-AREA (121:7)
               MOVE VALUTA                 TO FRARELF-IO-AREA (128:8)
               MOVE VT3                    TO FRARELF-IO-AREA (136:3)
               MOVE OPPHAV                 TO FRARELF-IO-AREA (228:4)
               MOVE 'R'                    TO FRARELF-IO-AREA (231:1)
               MOVE PRDDTO                 TO FRARELF-IO-AREA (232:5)
               MOVE PRDKLK                 TO FRARELF-IO-AREA (237:4)
      * MATCHENDE FAKTURA - RIKTIG FAKTURAKID OG RIKTIG FAKTURABELØP
               IF  I-U7
                   WRITE FRARELF-IO-AREA
               END-IF
           END-IF
           IF  (I-01 AND I-50 AND I-51)
               MOVE SPACES TO FRARELF-IO-AREA
               INITIALIZE FRARELF-IO-AREA
               MOVE '21'                   TO FRARELF-IO-AREA (1:2)
               MOVE FIRM                   TO FRARELF-IO-AREA (3:3)
               MOVE BLGART                 TO FRARELF-IO-AREA (6:1)
               MOVE BLGNR                  TO FRARELF-IO-AREA (7:6)
               MOVE BLGDTO                 TO FRARELF-IO-AREA (13:6)
               MOVE RMRESK                 TO FRARELF-IO-AREA (19:6)
               MOVE '  '                   TO FRARELF-IO-AREA (25:2)
               MOVE FAKBEL-IO              TO FRARELF-IO-AREA (27:9)
               MOVE ' '                    TO FRARELF-IO-AREA (36:1)
               MOVE MOTKTO                 TO FRARELF-IO-AREA (37:4)
               MOVE REFNR                  TO FRARELF-IO-AREA (41:6)
               MOVE '      '               TO FRARELF-IO-AREA (47:6)
               MOVE '      '               TO FRARELF-IO-AREA (53:6)
               MOVE FORFDT                 TO FRARELF-IO-AREA (59:6)
               MOVE ' '                    TO FRARELF-IO-AREA (65:1)
               MOVE '31'                   TO FRARELF-IO-AREA (83:2)
               MOVE TXTSEQ                 TO FRARELF-IO-AREA (96:1)
               MOVE TEKST                  TO FRARELF-IO-AREA (97:24)
               MOVE FAK132                 TO XO-112P
               MOVE XO-112P-EF             TO FRARELF-IO-AREA (121:7)
               INITIALIZE FAK132
               MOVE VALUTA                 TO FRARELF-IO-AREA (128:8)
               MOVE VT3                    TO FRARELF-IO-AREA (136:3)
               MOVE OPPHAV                 TO FRARELF-IO-AREA (228:4)
               MOVE 'F'                    TO FRARELF-IO-AREA (231:1)
               MOVE PRDDTO                 TO FRARELF-IO-AREA (232:5)
               MOVE PRDKLK                 TO FRARELF-IO-AREA (237:4)
      * IKKE MATCHENDE GIRO - RIKTIG RESKNR OG FEIL BELØP
               IF  I-U7
                   WRITE FRARELF-IO-AREA
               END-IF
           END-IF
           IF  (I-01 AND I-72)
               MOVE SPACES TO FRARELF-IO-AREA
               INITIALIZE FRARELF-IO-AREA
               MOVE '21'                   TO FRARELF-IO-AREA (1:2)
               MOVE FIRM                   TO FRARELF-IO-AREA (3:3)
               MOVE BLGART                 TO FRARELF-IO-AREA (6:1)
               MOVE BLGNR                  TO FRARELF-IO-AREA (7:6)
               MOVE BLGDTO                 TO FRARELF-IO-AREA (13:6)
               MOVE RESKNR                 TO FRARELF-IO-AREA (19:6)
               MOVE '  '                   TO FRARELF-IO-AREA (25:2)
               MOVE BELO-ELGP-IO           TO FRARELF-IO-AREA (27:9)
               MOVE ' '                    TO FRARELF-IO-AREA (36:1)
               MOVE MOTKTO                 TO FRARELF-IO-AREA (37:4)
               MOVE BLGNR                  TO FRARELF-IO-AREA (41:6)
               MOVE '      '               TO FRARELF-IO-AREA (47:6)
               MOVE '      '               TO FRARELF-IO-AREA (53:6)
               MOVE FORFDT                 TO FRARELF-IO-AREA (59:6)
               MOVE ' '                    TO FRARELF-IO-AREA (65:1)
               MOVE '31'                   TO FRARELF-IO-AREA (83:2)
               MOVE TXTSEQ                 TO FRARELF-IO-AREA (96:1)
               MOVE TEKST                  TO FRARELF-IO-AREA (97:24)
               MOVE TIL132                 TO XO-112P
               MOVE XO-112P-EF             TO FRARELF-IO-AREA (121:7)
               MOVE VALUTA                 TO FRARELF-IO-AREA (128:8)
               MOVE VT3                    TO FRARELF-IO-AREA (136:3)
               MOVE OPPHAV                 TO FRARELF-IO-AREA (228:4)
               MOVE PRDDTO                 TO FRARELF-IO-AREA (232:5)
               MOVE PRDKLK                 TO FRARELF-IO-AREA (237:4)
      * IKKE MATCHENDE FAKTURA - RIKTIG KID OG FEIL BELØP
               IF  I-U7
                   WRITE FRARELF-IO-AREA
               END-IF
           END-IF
           IF  (I-01 AND I-75)
               MOVE SPACES TO FRARELF-IO-AREA
               INITIALIZE FRARELF-IO-AREA
               MOVE '21'                   TO FRARELF-IO-AREA (1:2)
               MOVE FIRM                   TO FRARELF-IO-AREA (3:3)
               MOVE BLGART                 TO FRARELF-IO-AREA (6:1)
               MOVE BLGNR                  TO FRARELF-IO-AREA (7:6)
               MOVE BLGDTO                 TO FRARELF-IO-AREA (13:6)
               MOVE RESKNR                 TO FRARELF-IO-AREA (19:6)
               MOVE '  '                   TO FRARELF-IO-AREA (25:2)
               MOVE BELO-ELGP-IO           TO FRARELF-IO-AREA (27:9)
               MOVE ' '                    TO FRARELF-IO-AREA (36:1)
               MOVE MOTKTO                 TO FRARELF-IO-AREA (37:4)
               MOVE REFNR                  TO FRARELF-IO-AREA (41:6)
               MOVE '      '               TO FRARELF-IO-AREA (47:6)
               MOVE '      '               TO FRARELF-IO-AREA (53:6)
               MOVE FORFDT                 TO FRARELF-IO-AREA (59:6)
               MOVE ' '                    TO FRARELF-IO-AREA (65:1)
               MOVE '31'                   TO FRARELF-IO-AREA (83:2)
               MOVE TXTSEQ                 TO FRARELF-IO-AREA (96:1)
               MOVE TEKST                  TO FRARELF-IO-AREA (97:24)
               MOVE TIL132                 TO XO-112P
               MOVE XO-112P-EF             TO FRARELF-IO-AREA (121:7)
               MOVE VALUTA                 TO FRARELF-IO-AREA (128:8)
               MOVE VT3                    TO FRARELF-IO-AREA (136:3)
               MOVE OPPHAV                 TO FRARELF-IO-AREA (228:4)
               MOVE PRDDTO                 TO FRARELF-IO-AREA (232:5)
               MOVE PRDKLK                 TO FRARELF-IO-AREA (237:4)
      * IKKE MATCHENDE GIRO
               IF  I-U7
                   WRITE FRARELF-IO-AREA
               END-IF
           END-IF
           IF  (I-01 AND I-74)
               MOVE SPACES TO FRARELF-IO-AREA
               INITIALIZE FRARELF-IO-AREA
               MOVE '21'                   TO FRARELF-IO-AREA (1:2)
               MOVE FIRM                   TO FRARELF-IO-AREA (3:3)
               MOVE BLGART                 TO FRARELF-IO-AREA (6:1)
               MOVE BLGNR                  TO FRARELF-IO-AREA (7:6)
               MOVE BLGDTO                 TO FRARELF-IO-AREA (13:6)
               MOVE DIFKTO                 TO FRARELF-IO-AREA (19:4)
               MOVE '    '                 TO FRARELF-IO-AREA (23:4)
               MOVE BELO-ELGP-IO           TO FRARELF-IO-AREA (27:9)
               MOVE ' '                    TO FRARELF-IO-AREA (36:1)
               MOVE MOTKTO                 TO FRARELF-IO-AREA (37:4)
               MOVE BLGNR                  TO FRARELF-IO-AREA (41:6)
               MOVE '      '               TO FRARELF-IO-AREA (47:6)
               MOVE '      '               TO FRARELF-IO-AREA (53:6)
               MOVE FORFDT                 TO FRARELF-IO-AREA (59:6)
               MOVE ' '                    TO FRARELF-IO-AREA (65:1)
               MOVE '31'                   TO FRARELF-IO-AREA (83:2)
               MOVE TXTSEQ                 TO FRARELF-IO-AREA (96:1)
               MOVE TEKST                  TO FRARELF-IO-AREA (97:24)
               MOVE TIL132                 TO XO-112P
               MOVE XO-112P-EF             TO FRARELF-IO-AREA (121:7)
               MOVE VALUTA                 TO FRARELF-IO-AREA (128:8)
               MOVE VT3                    TO FRARELF-IO-AREA (136:3)
               MOVE OPPHAV                 TO FRARELF-IO-AREA (228:4)
               MOVE PRDDTO                 TO FRARELF-IO-AREA (232:5)
               MOVE PRDKLK                 TO FRARELF-IO-AREA (237:4)
      * IKKE MATCHENDE FAKTURA
               IF  I-U7
                   WRITE FRARELF-IO-AREA
               END-IF
           END-IF
           IF  (I-01 AND I-76)
               MOVE SPACES TO FRARELF-IO-AREA
               INITIALIZE FRARELF-IO-AREA
               MOVE '21'                   TO FRARELF-IO-AREA (1:2)
               MOVE FIRM                   TO FRARELF-IO-AREA (3:3)
               MOVE BLGART                 TO FRARELF-IO-AREA (6:1)
               MOVE BLGNR                  TO FRARELF-IO-AREA (7:6)
               MOVE BLGDTO                 TO FRARELF-IO-AREA (13:6)
               MOVE DIFKTO                 TO FRARELF-IO-AREA (19:4)
               MOVE '    '                 TO FRARELF-IO-AREA (23:4)
               MOVE BELO-ELGP-IO           TO FRARELF-IO-AREA (27:9)
               MOVE ' '                    TO FRARELF-IO-AREA (36:1)
               MOVE MOTKTO                 TO FRARELF-IO-AREA (37:4)
               MOVE BLGNR                  TO FRARELF-IO-AREA (41:6)
               MOVE '      '               TO FRARELF-IO-AREA (47:6)
               MOVE '      '               TO FRARELF-IO-AREA (53:6)
               MOVE FORFDT                 TO FRARELF-IO-AREA (59:6)
               MOVE ' '                    TO FRARELF-IO-AREA (65:1)
               MOVE '31'                   TO FRARELF-IO-AREA (83:2)
               MOVE TXTSEQ                 TO FRARELF-IO-AREA (96:1)
               MOVE TEKST                  TO FRARELF-IO-AREA (97:24)
               MOVE TIL132                 TO XO-112P
               MOVE XO-112P-EF             TO FRARELF-IO-AREA (121:7)
               MOVE VALUTA                 TO FRARELF-IO-AREA (128:8)
               MOVE VT3                    TO FRARELF-IO-AREA (136:3)
               MOVE OPPHAV                 TO FRARELF-IO-AREA (228:4)
               MOVE PRDDTO                 TO FRARELF-IO-AREA (232:5)
               MOVE PRDKLK                 TO FRARELF-IO-AREA (237:4)
               IF  I-U7
                   WRITE FRARELF-IO-AREA
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-86 AND I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE BLGART                 TO LISTE-IO-AREA (2:1)
               MOVE BLGNR                  TO LISTE-IO-AREA (5:6)
               MOVE BLGPRT                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (13:8)
               MOVE RESKNR                 TO LISTE-IO-AREA (23:6)
               MOVE BELO-ELGP              TO EDIT-BELO-ELGP
               MOVE EDIT-BELO-ELGP         TO LISTE-IO-AREA (31:13)
               MOVE MOTKTO                 TO LISTE-IO-AREA (46:4)
               IF  (I-61 AND NOT-I-62)
                   MOVE ALTKTO             TO LISTE-IO-AREA (46:4)
               END-IF
               MOVE REFNR                  TO LISTE-IO-AREA (52:6)
               IF  (I-70)
                   MOVE '* FEIL BELØP '    TO LISTE-IO-AREA (59:13)
               END-IF
               IF  (I-70)
                   MOVE 'DETTE REFNR  '    TO LISTE-IO-AREA (72:13)
               END-IF
               IF  (I-71)
                   MOVE ' AVVIST.'         TO LISTE-IO-AREA (85:8)
               END-IF
               IF  (I-71)
                   MOVE BELO-ELGP          TO EDIT-BELO-ELGP
                   MOVE EDIT-BELO-ELGP     TO LISTE-IO-AREA (93:13)
               END-IF
               IF  (I-71)
                   MOVE ' MÅ BOKFØRES MANUELT.   ' TO LISTE-IO-AREA
                                                              (106:24)
               END-IF
               IF  (I-72)
                   MOVE BELO-ELGP          TO EDIT-BELO-ELGP
                   MOVE EDIT-BELO-ELGP     TO LISTE-IO-AREA (93:13)
               END-IF
               IF  (I-72)
                   MOVE ' BOKFØRT MED REFNR=BILNR' TO LISTE-IO-AREA
                                                              (106:24)
               END-IF
               IF  (I-73)
                   MOVE '* UKJENT GIRO. ER AVVIST' TO LISTE-IO-AREA
                                                               (59:24)
               END-IF
               IF  (I-73)
                   MOVE ' OG MÅ BOKFØRES MANUELT.' TO LISTE-IO-AREA
                                                               (83:24)
               END-IF
               IF  (I-74)
                   MOVE '* UKJENT GIRO. ER BOKFØR' TO LISTE-IO-AREA
                                                               (59:24)
               END-IF
               IF  (I-74)
                   MOVE 'T PÅ OCR DIFF.-KONTO MED' TO LISTE-IO-AREA
                                                               (83:24)
               END-IF
               IF  (I-74)
                   MOVE ' REF.NR LIK BIL.NR.     ' TO LISTE-IO-AREA
                                                              (107:24)
               END-IF
               IF  (I-12)
                   MOVE '* DUPLIKAT/DOBBEL BETALI' TO LISTE-IO-AREA
                                                               (59:24)
               END-IF
               IF  (I-12)
                   MOVE 'NG. BOKFØRT.            ' TO LISTE-IO-AREA
                                                               (83:24)
               END-IF
               IF  (I-12)
                   MOVE '                        ' TO LISTE-IO-AREA
                                                              (107:24)
               END-IF
               IF  (I-50 AND NOT-I-22 AND NOT-I-51)
                   MOVE '* FEIL FAKTURABELØP. ' TO LISTE-IO-AREA
                                                               (59:21)
               END-IF
               IF  (I-50 AND NOT-I-22 AND NOT-I-51)
                   MOVE 'AVVIST.'          TO LISTE-IO-AREA (80:7)
               END-IF
               IF  (I-50 AND NOT-I-22 AND NOT-I-51)
                   MOVE BELO-ELGP          TO EDIT-BELO-ELGP
                   MOVE EDIT-BELO-ELGP     TO LISTE-IO-AREA (87:13)
               END-IF
               IF  (I-50 AND NOT-I-22 AND NOT-I-51)
                   MOVE ' MÅ BOKFØRES MANUELT.   ' TO LISTE-IO-AREA
                                                              (100:24)
               END-IF
               IF  (I-50 AND I-75)
                   MOVE '       '          TO LISTE-IO-AREA (80:7)
               END-IF
               IF  (I-50 AND I-75)
                   MOVE ' BOKFØR'          TO LISTE-IO-AREA (100:7)
               END-IF
               IF  (I-50 AND I-75)
                   MOVE 'T.                      ' TO LISTE-IO-AREA
                                                              (107:24)
      *                50 75            130 "T MED REFNR LIK BILNR.  "
               END-IF
               IF  (I-22 AND NOT-I-65)
                   MOVE '* UKJENT KID. ER AVVIST ' TO LISTE-IO-AREA
                                                               (59:24)
               END-IF
               IF  (I-22 AND NOT-I-65)
                   MOVE 'OG MÅ BOKFØRES MANUELT. ' TO LISTE-IO-AREA
                                                               (83:24)
               END-IF
               IF  (I-22 AND I-65)
                   MOVE '* UKJENT FAKTURA. ER BOK' TO LISTE-IO-AREA
                                                               (59:24)
               END-IF
               IF  (I-22 AND I-65)
                   MOVE 'FØRT PÅ OCR DIFF.-KONTO ' TO LISTE-IO-AREA
                                                               (83:24)
               END-IF
               IF  (I-22 AND I-65 AND NOT-I-61)
                   MOVE DIFKTO             TO LISTE-IO-AREA (107:4)
               END-IF
               IF  (I-22 AND I-65 AND I-61)
                   MOVE DIFAKT             TO LISTE-IO-AREA (107:4)
               END-IF
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 0                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-01 AND NOT-I-86)
           AND (NOT-I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE BLGART                 TO LISTE-IO-AREA (2:1)
               MOVE BLGNR                  TO LISTE-IO-AREA (5:6)
               MOVE BLGPRT                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (13:8)
               MOVE RESKNR                 TO LISTE-IO-AREA (23:6)
               MOVE BELO-ELGP              TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (31:13)
               MOVE MOTKTO                 TO LISTE-IO-AREA (46:4)
               IF  (I-61 AND NOT-I-62)
                   MOVE ALTKTO             TO LISTE-IO-AREA (46:4)
               END-IF
               MOVE REFNR                  TO LISTE-IO-AREA (52:6)
               IF  (I-70)
                   MOVE '* FEIL BELØP '    TO LISTE-IO-AREA (59:13)
               END-IF
               IF  (I-70)
                   MOVE 'DETTE REFNR  '    TO LISTE-IO-AREA (72:13)
               END-IF
               IF  (I-71)
                   MOVE ' AVVIST.'         TO LISTE-IO-AREA (85:8)
               END-IF
               IF  (I-71)
                   MOVE TIL132             TO XO-112U
                   MOVE XO-112U (1:13)     TO LISTE-IO-AREA (93:13)
               END-IF
               IF  (I-71)
                   MOVE ' MÅ BOKFØRES MANUELT.   ' TO LISTE-IO-AREA
                                                              (106:24)
               END-IF
               IF  (I-72)
                   MOVE TIL132             TO XO-112YY9R
                   MOVE XO-112YY9R         TO LISTE-IO-AREA (88:18)
               END-IF
               IF  (I-72)
                   MOVE ' BOKFØRT MED REFNR=BILNR' TO LISTE-IO-AREA
                                                              (106:24)
               END-IF
               IF  (I-73)
                   MOVE '* UKJENT GIRO. ER AVVIST' TO LISTE-IO-AREA
                                                               (59:24)
               END-IF
               IF  (I-73)
                   MOVE ' OG MÅ BOKFØRES MANUELT.' TO LISTE-IO-AREA
                                                               (83:24)
               END-IF
               IF  (I-74)
                   MOVE '* UKJENT GIRO. ER BOKFØR' TO LISTE-IO-AREA
                                                               (59:24)
               END-IF
               IF  (I-74)
                   MOVE 'T PÅ OCR DIFF.-KONTO MED' TO LISTE-IO-AREA
                                                               (83:24)
               END-IF
               IF  (I-74)
                   MOVE ' REF.NR LIK BIL.NR.     ' TO LISTE-IO-AREA
                                                              (107:24)
               END-IF
               IF  (I-12)
                   MOVE '* DUPLIKAT/DOBBEL BETALI' TO LISTE-IO-AREA
                                                               (59:24)
               END-IF
               IF  (I-12)
                   MOVE 'NG. BOKFØRT.            ' TO LISTE-IO-AREA
                                                               (83:24)
               END-IF
               IF  (I-12)
                   MOVE '                        ' TO LISTE-IO-AREA
                                                              (107:24)
               END-IF
               IF  (I-50 AND NOT-I-22 AND NOT-I-51)
                   MOVE '* FEIL FAKTURABELØP. ' TO LISTE-IO-AREA
                                                               (59:21)
               END-IF
               IF  (I-50 AND NOT-I-22 AND NOT-I-51)
                   MOVE 'AVVIST.'          TO LISTE-IO-AREA (80:7)
               END-IF
               IF  (I-50 AND NOT-I-22 AND NOT-I-51)
                   IF TIL132 < 0
                     MOVE TIL132           TO XO-112D
                     MOVE XO-112D (1:13)   TO LISTE-IO-AREA (87:13)
                   ELSE
                     MOVE TIL132           TO XO-112U
                     MOVE XO-112U (1:13)   TO LISTE-IO-AREA (87:13)
                   END-IF
               END-IF
               IF  (I-50 AND NOT-I-22 AND NOT-I-51)
                   MOVE ' MÅ BOKFØRES MANUELT.   ' TO LISTE-IO-AREA
                                                              (100:24)
               END-IF
               IF  (I-50 AND I-75)
                   MOVE '       '          TO LISTE-IO-AREA (80:7)
               END-IF
               IF  (I-50 AND I-75)
                   MOVE ' BOKFØR'          TO LISTE-IO-AREA (100:7)
               END-IF
               IF  (I-50 AND I-75)
                   MOVE 'T.                      ' TO LISTE-IO-AREA
                                                              (107:24)
      *                50 75            130 "T MED REFNR LIK BILNR.  "
               END-IF
               IF  (I-22 AND NOT-I-65)
                   MOVE '* UKJENT KID. ER AVVIST ' TO LISTE-IO-AREA
                                                               (59:24)
               END-IF
               IF  (I-22 AND NOT-I-65)
                   MOVE 'OG MÅ BOKFØRES MANUELT. ' TO LISTE-IO-AREA
                                                               (83:24)
               END-IF
               IF  (I-22 AND I-65)
                   MOVE '* UKJENT FAKTURA. ER BOK' TO LISTE-IO-AREA
                                                               (59:24)
               END-IF
               IF  (I-22 AND I-65)
                   MOVE 'FØRT PÅ OCR DIFF.-KONTO ' TO LISTE-IO-AREA
                                                               (83:24)
               END-IF
               IF  (I-22 AND I-65 AND NOT-I-61)
                   MOVE DIFKTO             TO LISTE-IO-AREA (107:4)
               END-IF
               IF  (I-22 AND I-65 AND I-61)
                   MOVE DIFAKT             TO LISTE-IO-AREA (107:4)
               END-IF
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 0                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR AND I-98 AND I-U8)
           AND (I-05 AND I-06 AND I-10)
           AND (I-09)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE PSDS                   TO LISTE-IO-AREA (41:80)
               MOVE R                      TO LISTE-IO-AREA (113:8)
               MOVE P-IO                   TO LISTE-IO-AREA (118:3)
               MOVE S-IO                   TO LISTE-IO-AREA (116:5)
               MOVE LONR                   TO LISTE-IO-AREA (116:5)
               MOVE LFIRMA                 TO LISTE-IO-AREA (118:3)
               MOVE LUNDGR                 TO LISTE-IO-AREA (118:3)
               MOVE LPROG                  TO LISTE-IO-AREA (113:8)
               MOVE LOPNVN                 TO LISTE-IO-AREA (86:35)
               MOVE LANTX-IO               TO LISTE-IO-AREA (118:3)
               MOVE LPRIID                 TO LISTE-IO-AREA (117:4)
               MOVE BJOBN                  TO LISTE-IO-AREA (113:8)
               MOVE BBEST                  TO LISTE-IO-AREA (120:1)
               MOVE BPERS                  TO LISTE-IO-AREA (91:30)
               MOVE BETTB                  TO LISTE-IO-AREA (81:40)
               MOVE BFORS                  TO LISTE-IO-AREA (81:40)
               MOVE BMEMO                  TO LISTE-IO-AREA (81:40)
               MOVE BANTX-IO               TO LISTE-IO-AREA (118:3)
               MOVE BPCLAS                 TO LISTE-IO-AREA (120:1)
               MOVE BPRJE                  TO LISTE-IO-AREA (118:3)
               MOVE REC120                 TO LISTE-IO-AREA (1:120)
               MOVE REC060                 TO LISTE-IO-AREA (61:60)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-91 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMANAVN'            TO LISTE-IO-AREA (3:9)
               MOVE FINAVN                 TO LISTE-IO-AREA (15:30)
               MOVE 'KONTROLL OG FEILLISTE' TO LISTE-IO-AREA (48:21)
               MOVE 'RELATERTE INNBETALINGER' TO LISTE-IO-AREA (70:23)
               MOVE 'KJØREDATO'            TO LISTE-IO-AREA (94:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (107:8)
      *                                 128 "SIDE"
      *                      L4PAGE  Z  132
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
               MOVE 'BA  BLGNR.   BLG.DTO' TO LISTE-IO-AREA (1:20)
               MOVE 'RESKNR'               TO LISTE-IO-AREA (23:6)
               MOVE 'B E L Ø P'            TO LISTE-IO-AREA (34:9)
               MOVE 'MKTO  REFNR.'         TO LISTE-IO-AREA (46:12)
               MOVE 'F E I L A N M E R K N I ' TO LISTE-IO-AREA (59:24)
               MOVE 'N G'                  TO LISTE-IO-AREA (83:3)
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
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF AND NOT-I-91 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMANAVN'            TO LISTE-IO-AREA (3:9)
               MOVE FINAVN                 TO LISTE-IO-AREA (15:30)
               MOVE 'KONTROLL OG FEILLISTE' TO LISTE-IO-AREA (48:21)
               MOVE 'RELATERTE INNBETALINGER' TO LISTE-IO-AREA (70:23)
               MOVE 'KJØREDATO'            TO LISTE-IO-AREA (94:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (107:8)
      *                                 128 "SIDE"
      *                      L4PAGE  Z  132
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
               MOVE 'BA  BLGNR.   BLG.DTO' TO LISTE-IO-AREA (1:20)
               MOVE 'RESKNR'               TO LISTE-IO-AREA (23:6)
               MOVE 'B E L Ø P'            TO LISTE-IO-AREA (34:9)
               MOVE 'MKTO  REFNR.'         TO LISTE-IO-AREA (46:12)
               MOVE 'F E I L A N M E R K N I ' TO LISTE-IO-AREA (59:24)
               MOVE 'N G'                  TO LISTE-IO-AREA (83:3)
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
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L4 AND I-92 AND NOT-I-86)
           AND (I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'SUM FOR FIRMA:'       TO LISTE-IO-AREA (1:14)
               MOVE FIRM                   TO LISTE-IO-AREA (16:3)
               MOVE 'TOTAL:'               TO LISTE-IO-AREA (23:6)
               MOVE BELL4A                 TO EDIT-BELL4A
               MOVE EDIT-BELL4A            TO LISTE-IO-AREA (30:14)
               INITIALIZE BELL4A
               MOVE 2                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L4 AND I-92 AND NOT-I-86)
           AND (NOT-I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'SUM FOR FIRMA:'       TO LISTE-IO-AREA (1:14)
               MOVE FIRM                   TO LISTE-IO-AREA (16:3)
               MOVE 'TOTAL:'               TO LISTE-IO-AREA (23:6)
               MOVE BEUL4A                 TO EDIT-BEUL4A
               MOVE EDIT-BEUL4A            TO LISTE-IO-AREA (30:17)
               INITIALIZE BEUL4A
      *                        BEUL4B B  43 "  .   .   ,0 -"
      *                        BEUL4AJB  43
               MOVE 2                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L4 AND I-92 AND NOT-I-86)
           AND (I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'AUTO-POSTERING PÅ RESKON' TO LISTE-IO-AREA (1:24)
               MOVE 'TRO:'                 TO LISTE-IO-AREA (25:4)
               MOVE BELL4B                 TO EDIT-BELL4B
               MOVE EDIT-BELL4B            TO LISTE-IO-AREA (30:14)
               INITIALIZE BELL4B
               MOVE 'BOKFØRINGER MED REFNR=BI' TO LISTE-IO-AREA (74:24)
               MOVE 'LNR MÅ FØLGES OPP I PROG' TO LISTE-IO-AREA (98:24)
               MOVE 'RAM RESA.'            TO LISTE-IO-AREA (122:9)
               MOVE 0                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L4 AND I-92 AND NOT-I-86)
           AND (NOT-I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'AUTO-POSTERING PÅ RESKON' TO LISTE-IO-AREA (1:24)
               MOVE 'TRO:'                 TO LISTE-IO-AREA (25:4)
               MOVE BEUL4A                 TO EDIT-BEUL4A
               MOVE EDIT-BEUL4A            TO LISTE-IO-AREA (30:17)
               INITIALIZE BEUL4A
      *                        BEUL4B B  43 "  .   .   ,0 -"
      *                        BEUL4BJB  43
               MOVE 'BOKFØRINGER MED REFNR=BI' TO LISTE-IO-AREA (74:24)
               MOVE 'LNR MÅ FØLGES OPP I PROG' TO LISTE-IO-AREA (98:24)
               MOVE 'RAM RESA.'            TO LISTE-IO-AREA (122:9)
               MOVE 0                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L4 AND I-92 AND NOT-I-86)
           AND (I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               IF  (I-20)
                   MOVE 'INGEN AVVISTE TRANSER I ' TO LISTE-IO-AREA
                                                                (1:24)
               END-IF
               IF  (I-20)
                   MOVE 'KJØRINGEN.              ' TO LISTE-IO-AREA
                                                               (25:24)
               END-IF
               IF  (NOT-I-20)
                   MOVE 'DIFFERANSE/FEILSUM      ' TO LISTE-IO-AREA
                                                                (1:24)
               END-IF
               IF  (NOT-I-20)
                   MOVE '   :'             TO LISTE-IO-AREA (25:4)
               END-IF
               IF  (NOT-I-20)
                   MOVE DIFFPR             TO EDIT-DIFFPR
                   MOVE EDIT-DIFFPR        TO LISTE-IO-AREA (30:14)
               END-IF
               IF  (NOT-I-65 AND NOT-I-20)
                   MOVE 'MÅ BOKFØRES MANUELT MED ' TO LISTE-IO-AREA
                                                               (45:24)
               END-IF
               IF  (NOT-I-65 AND NOT-I-20)
                   MOVE 'TILDELT BIL.NR FRA DENNE' TO LISTE-IO-AREA
                                                               (69:24)
               END-IF
               IF  (NOT-I-65 AND NOT-I-20)
                   MOVE ' LISTEN.                ' TO LISTE-IO-AREA
                                                               (93:24)
               END-IF
               IF  (I-65 AND NOT-I-20)
                   MOVE 'ER AUTO-POSTERT PÅ KONTO' TO LISTE-IO-AREA
                                                               (45:24)
               END-IF
               IF  (I-65 AND NOT-I-20 AND NOT-I-61)
                   MOVE DIFKTO             TO LISTE-IO-AREA (70:4)
               END-IF
               IF  (I-65 AND NOT-I-20 AND I-61)
                   MOVE DIFAKT             TO LISTE-IO-AREA (70:4)
               END-IF
               IF  (I-65 AND NOT-I-20)
                   MOVE DIFNAV             TO LISTE-IO-AREA (75:35)
               END-IF
               MOVE 0                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L4 AND I-92 AND NOT-I-86)
           AND (I-66)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'DIFF.-KONTO FOR OCR MANG' TO LISTE-IO-AREA (1:24)
               MOVE 'LER/ ER FEIL (SE PROGRAM' TO LISTE-IO-AREA (25:24)
               MOVE ' RK01). OPPGITT KONTO : ' TO LISTE-IO-AREA (49:24)
               IF  (NOT-I-61)
                   MOVE DIFKTO             TO LISTE-IO-AREA (73:4)
               END-IF
               IF  (I-61)
                   MOVE DIFAKT             TO LISTE-IO-AREA (73:4)
               END-IF
               MOVE 0                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L4 AND I-92 AND NOT-I-86)
           AND (NOT-I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               IF  (I-20)
                   MOVE 'INGEN AVVISTE TRANSER I ' TO LISTE-IO-AREA
                                                                (1:24)
               END-IF
               IF  (I-20)
                   MOVE 'KJØRINGEN.              ' TO LISTE-IO-AREA
                                                               (25:24)
               END-IF
               IF  (NOT-I-20)
                   MOVE 'DIFFERANSE/FEILSUM      ' TO LISTE-IO-AREA
                                                                (1:24)
               END-IF
               IF  (NOT-I-20)
                   MOVE '   :'             TO LISTE-IO-AREA (25:4)
               END-IF
               IF  (NOT-I-20)
                   MOVE DIFFPU             TO EDIT-DIFFPU
                   MOVE EDIT-DIFFPU        TO LISTE-IO-AREA (30:17)
      *               N20      DIFFPU    43 "  .   .   ,0 -"
      *               N20      DIFFPUJ   43
               END-IF
               IF  (NOT-I-65 AND NOT-I-20)
                   MOVE 'MÅ BOKFØRES MANUELT MED ' TO LISTE-IO-AREA
                                                               (48:24)
               END-IF
               IF  (NOT-I-65 AND NOT-I-20)
                   MOVE 'TILDELT BIL.NR FRA DENNE' TO LISTE-IO-AREA
                                                               (72:24)
               END-IF
               IF  (NOT-I-65 AND NOT-I-20)
                   MOVE ' LISTEN.                ' TO LISTE-IO-AREA
                                                               (96:24)
               END-IF
               IF  (I-65 AND NOT-I-20)
                   MOVE 'ER AUTO-POSTERT PÅ KONTO' TO LISTE-IO-AREA
                                                               (48:24)
               END-IF
               IF  (I-65 AND NOT-I-20 AND NOT-I-61)
                   MOVE DIFKTO             TO LISTE-IO-AREA (73:4)
               END-IF
               IF  (I-65 AND NOT-I-20 AND I-61)
                   MOVE DIFAKT             TO LISTE-IO-AREA (73:4)
               END-IF
               IF  (I-65 AND NOT-I-20)
                   MOVE DIFNAV             TO LISTE-IO-AREA (78:35)
               END-IF
               MOVE 0                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L4 AND I-92 AND NOT-I-86)
           AND (I-66)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'DIFF.-KONTO FOR OCR MANG' TO LISTE-IO-AREA (1:24)
               MOVE 'LER/ ER FEIL (SE PROGRAM' TO LISTE-IO-AREA (25:24)
               MOVE ' RK01). OPPGITT KONTO : ' TO LISTE-IO-AREA (49:24)
               IF  (NOT-I-61)
                   MOVE DIFKTO             TO LISTE-IO-AREA (73:4)
               END-IF
               IF  (I-61)
                   MOVE DIFAKT             TO LISTE-IO-AREA (73:4)
      * DUMMY-LINJE FOR Å FJERNE KOMPILERINGSFEIL
               END-IF
               MOVE 0                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L4 AND I-88)
               MOVE SPACES TO FEILFIL-IO-AREA
               INITIALIZE FEILFIL-IO-AREA
               MOVE FIRM                   TO FEILFIL-IO-AREA (1:3)
               IF  I-U7
                   WRITE FEILFIL-IO-AREA
               END-IF
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
           SET TILRELF-LEVEL-INIT          TO TRUE
           SET TILRELF-AHEAD-EOF-OFF       TO TRUE
           SET TILRELF-AHEAD-READ-OFF      TO TRUE
           INITIALIZE TILRELF-DATA-FIELDS
           SET TILRELF-EOF-OFF             TO TRUE
           SET TILRELF-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO TILRELF-MC
                                              TILRELF-MP
           OPEN INPUT TILRELF
           SET RELFILE-LEVEL-INIT          TO TRUE
           INITIALIZE RELFILE-DATA-FIELDS
           SET RELFILE-EOF-OFF             TO TRUE
           SET RELFILE-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO RELFILE-MC
                                              RELFILE-MP
           OPEN INPUT RELFILE
           INITIALIZE RELMAST-DATA-FIELDS
           OPEN INPUT RELMAST
           INITIALIZE SYSPARM-DATA-FIELDS
           OPEN INPUT SYSPARM
           INITIALIZE KONTOMA-DATA-FIELDS
           OPEN INPUT KONTOMA
           OPEN INPUT KUNDEMA
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES
           IF I-U7
               OPEN OUTPUT FRARELF
           END-IF
           IF I-U7
               OPEN OUTPUT FEILFIL
           END-IF.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE TILRELF
           CLOSE RELFILE
           CLOSE RELMAST
           CLOSE SYSPARM
           CLOSE KONTOMA
           CLOSE KUNDEMA
           IF LISTE-IO-AREA NOT = SPACES
             WRITE LISTE-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE-IO-AREA
           END-IF
           CLOSE LISTE
           IF I-U7
               CLOSE FRARELF
           END-IF
           IF I-U7
               CLOSE FEILFIL
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
