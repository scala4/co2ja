       IDENTIFICATION DIVISION.
       PROGRAM-ID. REG820R.
      **********************************************  Z-WIN-RPG2   ****
      * NY VERSJON AV REG420                     ***TXT***OK
      *  PROGRAM....: REG820 - FØR SEPT.05-REG420                     *
      *   PROGRAM  R E G 8 2 0    A U T O - P O S T E R I N G.                 *
      *   LESER DAGENS RESKONTRORECORDS+REGNSKAPSRECORDS OG SELEKTERER         *
      *   RECORDS SOM ER GRUNNLAG FOR AUTO-POSTERING I FØLGE BILAGSNR.FILE.    *
      *   DET BLIR OGSÅ PRINTET BILAGSOPPGAVE. MED TOTALSUMMER.                *
      *   VAREGRUPPE BLIR OMDØPT FRA FIRMA-VGR TIL FIRMA-VGR.                  *
      *   ER SISTE SIFFER I VGR BLANK ER DETTE EN HOVEDBOKSKONTO.              *
      * 30/09-93 BLANK UT KTOKL. FRA FAKTURA REGNSKAPSREC.                     *
      * 13/01-94 TAR MED KOSTNADSSTED FRA TABELL NÅR KONTO=HOVEDBOKSKTO        *
      *         KOSTNADSSTED LEGGES I PARAMETER ETTER HOVEDBOKSKONTO           *
      *         MED EN BLANK I MELLOM.                                         *
      * 13/01-94 TAR MED KOSTNADSSTED FRA TABELL NÅR KONTO=HOVEDBOKSKTO        *
      * 08/05-96 UTVIDET VAREGRUPPETABELL TIL 600                              *
      * 04/06-96 LEGGER UT FAKTURA-KID                                         *
      * 06/06-96 LEGGER IKKE UT FAKTURA-KID FOR KREDITNOTA.                    *
      * 18/06-96 LEGGER UT FAKTURA-KID KUN HVIS FIRMAET ØNSKER, JFR RG02       *
      * 31/10-96 UTVIDET VAREGRUPPETABELL TIL 650                              *
      * 07/02-97 UTVIDET VAREGRUPPETABELL TIL 700                              *
      * 08/04-97 UTVIDET VAREGRUPPETABELL TIL 750                              *
      *  5/05-97 UTVIDET VAREGRUPPETABELL FRA 750 TIL 800                      *
      *  5/05-97 UTVIDET BILAGSTABELL FRA 1200 TIL 1500                        *
      * 24/07-97 TILPASSET BRUK AV ALTERNATIVT KONTONR.                        *
      *  5/08-98 UTVIDET VAREGRUPPETABELL FRA 800 TIL 850                      *
      *  7/01-00 UTVIDET VAREGRUPPETABELL FRA 850 TIL 900                      *
      *  18/08-00 LAGT OM FRA SYSINPUT FRA-TIL TABELL TIL VSAMFILE.            *
      *  02/10-03 FLYTTER ØREAVRUNDING (HOVEDBOKSPOSTER UTENOM MVA OG          *
      *           SALG (SNUS TIL KJØP) TIL VAREKJØP (3XXXXX).                  *
      *  03/10-03 FEILRETTING FRA 02.10: AK=0 FOR ØREAVRUNDING                 *
      *           OG INDIKATOR 42 FOR HOVEDBOKSKONTO SKRUS AV                  *
      *           NÅR ØREAVRUNDING GJØRES OM TIL VAREKONTO.                    *
      *  18/11-03 LEGGER ØREAVRUNDING I HOVEDBOKSKONTO HVIS XXXXX=HBOK         *
      *  16.09.05 LEGGER UT FAKTURA-KID FOR KREDITNOTA JFR RG02                *
      *  06.03.06 LEGGER "A" (MVA GRL) I AVGIFTSKODE NÅR AVG.KODE ER "6"       *
      *  30.05.06 SKILLER UT KONTROLLDEL PÅ EGEN LISTE                         *
      *  21.02.07 UTVIDET VAREGRUPPE/KONTO-TAB FRA 8 TIL 18                    *
      *  23.04.08: ENDRET EDITERING I AVSTEMMINGSSUM                   *
      *  09.06.10 UTVIDET VAREGRUPPE/KONTO-TAB FRA 18 TIL 36           *       *
      * 15.04.11 UTVIDET REGNSKAPSFIL TIL 240 OG LAGT INN PROD.TIDSPKT
      * 03.10.12 SKRIVER AVSTEMMINGSFIL
      * 10.10.12 HENTER DATO OG KLOKK VED LR SIDEN CALCULATE IKKE BLIR
      *          KJØRT VED TOM FILE.
      **************************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: REG820.rpg
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
           SELECT BMTAB
               ASSIGN TO UT-S-BMTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS BMTAB-STATUS.
           SELECT TKTAB
               ASSIGN TO UT-S-TKTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TKTAB-STATUS.
           SELECT BILAGFI
               ASSIGN TO UT-S-BILAGFI
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS BILAGFI-STATUS.
           SELECT SELRESK
               ASSIGN TO UT-S-SELRESK
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS SELRESK-STATUS.
           SELECT SELREGN
               ASSIGN TO UT-S-SELREGN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS SELREGN-STATUS.
           SELECT FAKTKID
               ASSIGN TO FAKTKID
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FAKTKID-STATUS
               RECORD KEY IS FAKTKID-KEY1.
           SELECT KIDFILE
               ASSIGN TO KIDFILE
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KIDFILE-STATUS
               RECORD KEY IS KIDFILE-KEY1.
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
           SELECT NYERESK
               ASSIGN TO UT-S-NYERESK
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS NYERESK-STATUS.
           SELECT NYEREGN
               ASSIGN TO UT-S-NYEREGN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS NYEREGN-STATUS.
           SELECT AVSTEMM
               ASSIGN TO UT-S-AVSTEMM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS AVSTEMM-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
           SELECT LISTE2
               ASSIGN TO UT-S-LISTE2
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE2-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD BMTAB
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  BMTAB-IO-AREA.
           05  BMTAB-IO-AREA-X             PICTURE X(80).
       FD TKTAB
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  TKTAB-IO-AREA.
           05  TKTAB-IO-AREA-X             PICTURE X(80).
       FD BILAGFI
               BLOCK CONTAINS 396
               RECORD CONTAINS 396.
       01  BILAGFI-IO-AREA.
           05  BILAGFI-IO-AREA-X           PICTURE X(396).
       FD SELRESK
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  SELRESK-IO-AREA.
           05  SELRESK-IO-AREA-X           PICTURE X(200).
       FD SELREGN
               BLOCK CONTAINS 240
               RECORD CONTAINS 240.
       01  SELREGN-IO-AREA.
           05  SELREGN-IO-AREA-X           PICTURE X(240).
       FD FAKTKID
               RECORD CONTAINS 40.
       01  FAKTKID-IO-AREA.
           05  FAKTKID-IO-AREA-X.
               10  FAKTKID-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(30).
       FD KIDFILE
               RECORD CONTAINS 100.
       01  KIDFILE-IO-AREA.
           05  KIDFILE-IO-AREA-X.
               10  KIDFILE-KEY1            PICTURE X(21).
               10  FILLER                  PICTURE X(79).
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
       FD NYERESK
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  NYERESK-IO-AREA.
           05  NYERESK-IO-AREA-X           PICTURE X(200).
       FD NYEREGN
               BLOCK CONTAINS 240
               RECORD CONTAINS 240.
       01  NYEREGN-IO-AREA.
           05  NYEREGN-IO-AREA-X           PICTURE X(240).
       FD AVSTEMM
               BLOCK CONTAINS 120
               RECORD CONTAINS 120.
       01  AVSTEMM-IO-AREA.
           05  AVSTEMM-IO-AREA-X           PICTURE X(120).
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
      *BUGFILO O   F  80  80            PRINTERSYSLST
       FD LISTE2
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE2-IO-PRINT.
           05  LISTE2-IO-AREA-CONTROL      PICTURE X VALUE ' '.
        02 LISTE2-IO-AREA.
           05  LISTE2-IO-AREA-X            PICTURE X(132).
       WORKING-STORAGE SECTION.
       77  TABBMK-MAX   VALUE 36           PICTURE 9(4) USAGE BINARY.
       77  TABBML-MAX   VALUE 36           PICTURE 9(4) USAGE BINARY.
       77  TABTKF-MAX   VALUE 19           PICTURE 9(4) USAGE BINARY.
       77  TABTKT-MAX   VALUE 19           PICTURE 9(4) USAGE BINARY.
       77  ARF-MAX   VALUE 36              PICTURE 9(4) USAGE BINARY.
       77  ART-MAX   VALUE 36              PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  TABBMK-TABLE.
               10  TABBMK-ENTRY
                                           OCCURS 36 TIMES
                                           INDEXED BY TABBMK-I
                                                      TABBMK-S
                                                      TABBML-I
                                                      TABBML-S.
                   15  TABBMK              PICTURE X(2).
                   15  TABBML              PICTURE X(2).
           05  TABTKF-TABLE.
               10  TABTKF-ENTRY
                                           OCCURS 19 TIMES
                                           INDEXED BY TABTKF-I
                                                      TABTKF-S
                                                      TABTKT-I
                                                      TABTKT-S.
                   15  TABTKF              PICTURE X(2).
                   15  TABTKT              PICTURE X(2).
           05  ARF-TABLE.
               10  ARF-ENTRY
                                           OCCURS 36 TIMES
                                           INDEXED BY ARF-I
                                                      ARF-S.
                   15  ARF                 PICTURE X(5).
      * **START RBS - DATASTRUKTUR FOR SUB-PROGRAM RBS000 ********
           05  ART-TABLE.
               10  ART-ENTRY
                                           OCCURS 36 TIMES
                                           INDEXED BY ART-I
                                                      ART-S.
                   15  ART                 PICTURE X(5).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  BMTAB-STATUS                PICTURE 99 VALUE 0.
           10  TKTAB-STATUS                PICTURE 99 VALUE 0.
           10  BILAGFI-STATUS              PICTURE 99 VALUE 0.
           10  SELRESK-STATUS              PICTURE 99 VALUE 0.
           10  SELREGN-STATUS              PICTURE 99 VALUE 0.
           10  FAKTKID-STATUS              PICTURE 99 VALUE 0.
           10  KIDFILE-STATUS              PICTURE 99 VALUE 0.
           10  SYSPARM-STATUS              PICTURE 99 VALUE 0.
           10  KONTOMA-STATUS              PICTURE 99 VALUE 0.
           10  NYERESK-STATUS              PICTURE 99 VALUE 0.
           10  NYEREGN-STATUS              PICTURE 99 VALUE 0.
           10  AVSTEMM-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  LISTE2-STATUS               PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  BMTAB-EOF-OFF           VALUE '0'.
               88  BMTAB-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  TKTAB-EOF-OFF           VALUE '0'.
               88  TKTAB-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BILAGFI-EOF-OFF         VALUE '0'.
               88  BILAGFI-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BILAGFI-READ-OFF        VALUE '0'.
               88  BILAGFI-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BILAGFI-PROCESS-OFF     VALUE '0'.
               88  BILAGFI-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  BILAGFI-LEVEL-INIT-OFF  VALUE '0'.
               88  BILAGFI-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SELRESK-EOF-OFF         VALUE '0'.
               88  SELRESK-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SELRESK-READ-OFF        VALUE '0'.
               88  SELRESK-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SELRESK-PROCESS-OFF     VALUE '0'.
               88  SELRESK-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  SELRESK-LEVEL-INIT-OFF  VALUE '0'.
               88  SELRESK-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SELREGN-EOF-OFF         VALUE '0'.
               88  SELREGN-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SELREGN-READ-OFF        VALUE '0'.
               88  SELREGN-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  SELREGN-PROCESS-OFF     VALUE '0'.
               88  SELREGN-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  SELREGN-LEVEL-INIT-OFF  VALUE '0'.
               88  SELREGN-LEVEL-INIT      VALUE '1'.
           05  FAKTKID-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  KIDFILE-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  SYSPARM-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  KONTOMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  LISTE2-DATA-FIELDS.
               10  LISTE2-AFTER-SPACE      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE2-AFTER-SKIP       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE2-BEFORE-SPACE     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE2-BEFORE-SKIP      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE2-MAX-LINES        PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE2-LINE-COUNT       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  LISTE2-CLR-IO           PICTURE X VALUE 'Y'.
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
           05  BILAGFI-LEVEL-07.
               10  BILAGFI-07-L3.
                   15  BILAGFI-07-L3-TILFNR PICTURE X(3).
               10  BILAGFI-07-L2.
                   15  BILAGFI-07-L2-FINR  PICTURE X(3).
               10  BILAGFI-07-L1.
                   15  BILAGFI-07-L1-BINR  PICTURE X(6).
           05  BILAGFI-DATA-FIELDS.
               10  FINR                    PICTURE X(3).
               10  BINR                    PICTURE X(6).
               10  TILFNR                  PICTURE X(3).
               10  TBART                   PICTURE X(1).
               10  TBILNR                  PICTURE X(6).
               10  TRESNR                  PICTURE X(6).
               10  FRESNR                  PICTURE X(6).
               10  TAVD1                   PICTURE X(1).
               10  TAVD                    PICTURE X(4).
           05  BILAGFI-MP                  PICTURE X(12).
           05  BILAGFI-MC                  PICTURE X(12).
           05  BILAGFI-M-07            REDEFINES BILAGFI-MC.
               10  BILAGFI-M-07-M3.
                   15  BILAGFI-M-07-M3-TILFNR-G.
                       20  BILAGFI-M-07-M3-TILFNR PICTURE X(3).
               10  BILAGFI-M-07-M2.
                   15  BILAGFI-M-07-M2-FINR-G.
                       20  BILAGFI-M-07-M2-FINR PICTURE X(3).
               10  BILAGFI-M-07-M1.
                   15  BILAGFI-M-07-M1-BINR-G.
                       20  BILAGFI-M-07-M1-BINR PICTURE X(6).
           05  SELRESK-LEVEL-01.
               10  SELRESK-01-L3.
                   15  SELRESK-01-L3-TILFNR PICTURE X(3).
               10  SELRESK-01-L2.
                   15  SELRESK-01-L2-FINR  PICTURE X(3).
               10  SELRESK-01-L1.
                   15  SELRESK-01-L1-BINR  PICTURE X(6).
           05  SELRESK-DATA-FIELDS.
               10  RESREC                  PICTURE X(200).
               10  REFNR                   PICTURE X(6).
               10  TK                      PICTURE X(2).
               10  BLDAT1                  PICTURE X(6).
               10  BLAAR1                  PICTURE X(2).
               10  BLDAG1                  PICTURE X(2).
               10  BIART                   PICTURE X(1).
               10  FFDAT1                  PICTURE X(6).
               10  FFAAR1                  PICTURE X(2).
               10  FFDAG1                  PICTURE X(2).
               10  RESBEL-IO.
                   15  RESBEL              PICTURE S9(7)V9(2).
               10  RESBM                   PICTURE X(2).
               10  RSB132-IO.
                   15  RSB132              PICTURE S9(11)V9(2) USAGE
                                                       PACKED-DECIMAL.
      *                                   P 121 1284RSV154
           05  SELRESK-MP                  PICTURE X(12).
           05  SELRESK-MC                  PICTURE X(12).
           05  SELRESK-M-01            REDEFINES SELRESK-MC.
               10  SELRESK-M-01-M3.
                   15  SELRESK-M-01-M3-TILFNR-G.
                       20  SELRESK-M-01-M3-TILFNR PICTURE X(3).
               10  SELRESK-M-01-M2.
                   15  SELRESK-M-01-M2-FINR-G.
                       20  SELRESK-M-01-M2-FINR PICTURE X(3).
               10  SELRESK-M-01-M1.
                   15  SELRESK-M-01-M1-BINR-G.
                       20  SELRESK-M-01-M1-BINR PICTURE X(6).
           05  SELREGN-LEVEL-02.
               10  SELREGN-02-L3.
                   15  SELREGN-02-L3-TILFNR PICTURE X(3).
               10  SELREGN-02-L2.
                   15  SELREGN-02-L2-FINR  PICTURE X(3).
               10  SELREGN-02-L1.
                   15  SELREGN-02-L1-BINR  PICTURE X(6).
           05  SELREGN-DATA-FIELDS.
               10  REGREC                  PICTURE X(240).
               10  VKONTO                  PICTURE X(8).
               10  REGBEL-IO.
                   15  REGBEL              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  REGSIG                  PICTURE X(1).
               10  HKONTO                  PICTURE X(8).
               10  HKONT6                  PICTURE X(6).
               10  FRAVGR                  PICTURE X(5).
               10  HKL                     PICTURE X(1).
               10  REGAK                   PICTURE X(1).
               10  REGBM                   PICTURE X(2).
               10  RGB132-IO.
                   15  RGB132              PICTURE S9(11)V9(2) USAGE
                                                       PACKED-DECIMAL.
      *                                   P 128 1354RGV154
      *****************************************************************
      * FAKTURAKID-FILE                                               *
      *****************************************************************
           05  SELREGN-MP                  PICTURE X(12).
           05  SELREGN-MC                  PICTURE X(12).
           05  SELREGN-M-02            REDEFINES SELREGN-MC.
               10  SELREGN-M-02-M3.
                   15  SELREGN-M-02-M3-TILFNR-G.
                       20  SELREGN-M-02-M3-TILFNR PICTURE X(3).
               10  SELREGN-M-02-M2.
                   15  SELREGN-M-02-M2-FINR-G.
                       20  SELREGN-M-02-M2-FINR PICTURE X(3).
               10  SELREGN-M-02-M1.
                   15  SELREGN-M-02-M1-BINR-G.
                       20  SELREGN-M-02-M1-BINR PICTURE X(6).
           05  FAKTKID-DATA-FIELDS.
               10  FKKID                   PICTURE X(7).
      *****************************************************************
      * UTBET.KID.FILE                                                *
      *****************************************************************
           05  KIDFILE-DATA-FIELDS.
      *****************************************************************
      * REGNSKAPSPARAMETER - FASTE                                    *
      *****************************************************************
               10  FILLER                  PICTURE X.
           05  SYSPARM-DATA-FIELDS.
               10  FIRKID                  PICTURE X(1).
               10  SYSALT                  PICTURE X(1).
           05  KONTOMA-DATA-FIELDS.
               10  ALTKTO                  PICTURE X(4).
      *****************************************************************
      * HENTER DAGENS DATO
      *****************************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
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
               10  SYSKY6                  PICTURE X(6).
               10  SYSKY4                  PICTURE X(4).
               10  SYSKEY                  PICTURE X(10).
               10  ANTBIL-IO.
                   15  ANTBIL              PICTURE S9(5).
               10  TFNRXX                  PICTURE X(3).
               10  ARGANT-IO.
                   15  ARGANT              PICTURE S9(13).
               10  ARGBEL-IO.
                   15  ARGBEL              PICTURE S9(13)V9(2).
               10  ARGBEU-IO.
                   15  ARGBEU              PICTURE S9(13)V9(2).
               10  ARSANT-IO.
                   15  ARSANT              PICTURE S9(13).
               10  ARSBEL-IO.
                   15  ARSBEL              PICTURE S9(13)V9(2).
               10  ARSBEU-IO.
                   15  ARSBEU              PICTURE S9(13)V9(2).
               10  FAKKRE                  PICTURE X(1).
               10  RESTOT-IO.
                   15  RESTOT              PICTURE S9(8)V9(2).
               10  RESTOU-IO.
                   15  RESTOU              PICTURE S9(11)V9(2).
               10  RESANT-IO.
                   15  RESANT              PICTURE S9(5).
               10  BDATO1                  PICTURE X(6).
               10  FDATO1                  PICTURE X(6).
               10  KREANT-IO.
                   15  KREANT              PICTURE S9(5).
               10  FAKKY4                  PICTURE X(4).
               10  FAKKEY                  PICTURE X(10).
               10  FKDANT-IO.
                   15  FKDANT              PICTURE S9(5).
               10  KIDK09                  PICTURE X(9).
               10  KIDK12                  PICTURE X(12).
               10  KIDKEY                  PICTURE X(21).
               10  KIDANT-IO.
                   15  KIDANT              PICTURE S9(5).
               10  KIDDUP-IO.
                   15  KIDDUP              PICTURE S9(5).
               10  RELANT-IO.
                   15  RELANT              PICTURE S9(5).
               10  BINRXX                  PICTURE X(6).
               10  REGSUM-IO.
                   15  REGSUM              PICTURE S9(7)V9(2).
               10  REGANT-IO.
                   15  REGANT              PICTURE S9(5).
               10  REGTOT-IO.
                   15  REGTOT              PICTURE S9(8)V9(2).
               10  REGTOU-IO.
                   15  REGTOU              PICTURE S9(11)V9(2).
               10  NULL10-IO.
                   15  NULL10              PICTURE S9(10).
               10  NULL15-IO.
                   15  NULL15              PICTURE S9(15).
               10  X-IO.
                   15  X                   PICTURE S9(2).
               10  W5                      PICTURE X(5).
               10  TILVGR                  PICTURE X(5).
               10  TEST1                   PICTURE X(1).
               10  W7                      PICTURE X(7).
               10  W8                      PICTURE X(8).
               10  WKOST                   PICTURE X(4).
               10  TILKTO                  PICTURE X(4).
               10  TILSTD                  PICTURE X(4).
               10  KTOKEY                  PICTURE X(7).
           05  EDITTING-FIELDS.
               10  XO-112P-EF.
                 15  XO-112P               PICTURE S9(11)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-150P-EF.
                 15  XO-150P               PICTURE S9(15) USAGE
                                                       PACKED-DECIMAL.
               10  XO-80P-EF.
                 15  XO-80P                PICTURE S9(8) USAGE
                                                       PACKED-DECIMAL.
               10  XO-60P-EF.
                 15  XO-60P                PICTURE S9(6) USAGE
                                                       PACKED-DECIMAL.
               10  XO-100P-EF.
                 15  XO-100P               PICTURE S9(10) USAGE
                                                       PACKED-DECIMAL.
               10  XO-40D                  PICTURE S9(4).
               10  XO-40U                  PICTURE 9(4).
               10  XO-72YY9R               PICTURE Z.ZZZ.ZZZ,99-.
               10  XO-72YY9                PICTURE Z.ZZZ.ZZZ,99.
               10  EDIT-RESANT             PICTURE ZZZZZ.
               10  XO-50YY9                PICTURE ZZ.ZZ9.
               10  EDIT-RESTOT             PICTURE ZZZZZZZZ,ZZ-.
               10  EDIT-RESTOU             PICTURE ZZZZZZZZZZZ,ZZ-.
               10  EDIT-REGANT             PICTURE ZZZZZ.
               10  EDIT-REGTOT             PICTURE ZZZZZZZZ,ZZ-.
               10  EDIT-REGTOU             PICTURE ZZZZZZZZZZZ,ZZ-.
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
           SET NOT-I-07                    TO TRUE
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-04                    TO TRUE
           SET NOT-I-05                    TO TRUE
           SET NOT-I-06                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  BILAGFI-PROCESS
               SET BILAGFI-PROCESS-OFF     TO TRUE
               SET BILAGFI-READ            TO TRUE
           END-IF
 
           IF  BILAGFI-READ
               PERFORM BILAGFI-GET
               SET BILAGFI-READ-OFF        TO TRUE
               IF  NOT BILAGFI-EOF
                   PERFORM BILAGFI-MATCH-SET
               END-IF
           END-IF
 
           IF  SELRESK-PROCESS
               SET SELRESK-PROCESS-OFF     TO TRUE
               SET SELRESK-READ            TO TRUE
           END-IF
 
           IF  SELRESK-READ
               PERFORM SELRESK-GET
               SET SELRESK-READ-OFF        TO TRUE
               IF  NOT SELRESK-EOF
                   PERFORM SELRESK-MATCH-SET
               END-IF
           END-IF
 
           IF  SELREGN-PROCESS
               SET SELREGN-PROCESS-OFF     TO TRUE
               SET SELREGN-READ            TO TRUE
           END-IF
 
           IF  SELREGN-READ
               PERFORM SELREGN-GET
               SET SELREGN-READ-OFF        TO TRUE
               IF  NOT SELREGN-EOF
                   PERFORM SELREGN-MATCH-SET
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
 
           IF  BILAGFI-PROCESS
               PERFORM BILAGFI-IDSET
           END-IF
 
           IF  SELRESK-PROCESS
               PERFORM SELRESK-IDSET
           END-IF
 
           IF  SELREGN-PROCESS
               PERFORM SELREGN-IDSET
           END-IF
 
           IF  BILAGFI-PROCESS
               PERFORM BILAGFI-CHK-LEVEL
           END-IF
 
           IF  SELRESK-PROCESS
               PERFORM SELRESK-CHK-LEVEL
           END-IF
 
           IF  SELREGN-PROCESS
               PERFORM SELREGN-CHK-LEVEL
           END-IF
 
           IF I-1ST
               GO TO LR-INDICATOR-TEST
           END-IF.
 
       MAINLINE-TOTAL-CALCS.
           IF  I-LR
               PERFORM LR-CALCS
           END-IF
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
 
           IF  BILAGFI-PROCESS
               PERFORM BILAGFI-FLDOFF
               PERFORM BILAGFI-FLDSET
           END-IF
 
           IF  SELRESK-PROCESS
               PERFORM SELRESK-FLDSET
           END-IF
 
           IF  SELREGN-PROCESS
               PERFORM SELREGN-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  BILAGFI-PROCESS
           OR  SELRESK-PROCESS
           OR  SELREGN-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (NOT-I-90)
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
               SET I-90                    TO TRUE
           END-IF
           IF  (I-L1)
               SET NOT-I-50                TO TRUE
      **********************************************************
      *  SJEKK OM KID PÅ UTBETALINGSTRANS SKAL LAGES (LAGE     *
      *  KIDFILE-RECORD).                                      *
      **********************************************************
           END-IF
           IF  (I-L3)
               SET NOT-I-81                TO TRUE
               SET NOT-I-82                TO TRUE
               SET NOT-I-83                TO TRUE
               SET NOT-I-84                TO TRUE
               MOVE TILFNR                 TO SYSKY6 (1:3)
               MOVE 'REG'                  TO SYSKY6 (4:3)
               MOVE 'A011'                 TO SYSKY4
               MOVE SYSKY6                 TO SYSKEY (1:6)
               MOVE SYSKY4                 TO SYSKEY (7:4)
               MOVE SYSKEY                 TO SYSPARM-KEY1
               READ SYSPARM RECORD KEY IS SYSPARM-KEY1
               INVALID KEY
                   SET I-81                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-81            TO TRUE
                   PERFORM SYSPARM-FLDSET
                   PERFORM SYSPARM-IDSET
               END-READ
           END-IF
           IF  (I-L3 AND I-81)
               SET I-82                    TO TRUE
           END-IF
           IF  (I-L3 AND NOT-I-81)
               SET NOT-I-82                TO TRUE
               SET NOT-I-83                TO TRUE
               IF  FIRKID NOT = 'J'
                   SET I-82                TO TRUE
               END-IF
               IF  FIRKID = 'J'
                   SET I-83                TO TRUE
               END-IF
               SET NOT-I-84                TO TRUE
               IF  SYSALT = 'J'
                   SET I-84                TO TRUE
               END-IF
      **********************************************************
      * MATCHER BILAGSFIL MED DAGENS TRANSER.                  *
      **********************************************************
           END-IF
           IF  (NOT-I-MR)
               GO TO SLUTT-T
           END-IF
           IF  (I-MR)
               SET I-50                    TO TRUE
      *  07                MOVE "ARF     "BUGFL1  8        DISPLAY FIELD
      *  07      BUGFL1    DEBUGBUGFILO   ARF              VIS INDIKATOR
      *  07                MOVE "ART     "BUGFL1  8        DISPLAY FIELD
      *  07      BUGFL1    DEBUGBUGFILO   ART              VIS INDIKATOR
           END-IF
           IF  (I-L1 AND I-50)
               ADD 1                       TO ANTBIL
      ******************************************************
           END-IF
           IF  (NOT-I-50)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-51                    TO TRUE
           IF  TILFNR = TFNRXX
               SET I-51                    TO TRUE
           END-IF
           IF  (NOT-I-51)
               MOVE TILFNR                 TO TFNRXX
               MOVE 0                      TO REGSUM
               PERFORM RBSRUT-S
           END-IF
           IF  (I-01)
               PERFORM RESRUT-S
           END-IF
           IF  (I-02)
               PERFORM REGRUT-S
           END-IF
           IF  (I-02)
               SET NOT-I-45                TO TRUE
               SET NOT-I-87                TO TRUE
           END-IF
           IF  (I-02 AND I-35 AND NOT-I-32)
               PERFORM VGRRUT-S
           END-IF
           IF  (I-02 AND I-84 AND I-32)
               OR  (I-02 AND I-84 AND I-39)
               OR  (I-02 AND I-84 AND I-42)
               PERFORM KTORUT-S
           END-IF.
 
       SLUTT-T.
           CONTINUE.
 
       RESRUT-S SECTION.
       RESRUT-S-P.
           SET NOT-I-23                    TO TRUE
           SET NOT-I-24                    TO TRUE
           SET NOT-I-25                    TO TRUE
           SET NOT-I-20                    TO TRUE
           IF  BINR NOT < '900000'
               SET I-20                    TO TRUE
           END-IF
           IF  (I-20)
               MOVE 'K'                    TO FAKKRE
           END-IF
           IF  (NOT-I-20)
               MOVE 'F'                    TO FAKKRE
           END-IF
           MULTIPLY -1 BY RESBEL       GIVING RESBEL
           MULTIPLY -1 BY RSB132       GIVING RSB132
           ADD RESBEL                      TO RESTOT
           ADD RSB132                      TO RESTOU
           ADD 1                           TO RESANT
           SET NOT-I-21                    TO TRUE
           SET TABBMK-S                    TO TABBMK-I
           PERFORM WITH TEST AFTER
                   VARYING TABBMK-I FROM 1 BY 1
                     UNTIL TABBMK-I >= TABBMK-MAX
                        OR I-21
               IF  RESBM = TABBMK (TABBMK-I)
                   SET I-21                TO TRUE
                   SET TABBMK-S            TO TABBMK-I
               END-IF
           END-PERFORM
           SET TABBMK-I                    TO TABBMK-S
           IF  I-21
           AND TABBMK-I NOT > TABBML-MAX
               SET TABBML-I                TO TABBMK-I
           END-IF
           IF  (I-21)
               MOVE TABBML(TABBML-I)       TO RESBM
           END-IF
           SET NOT-I-22                    TO TRUE
           SET TABTKF-S                    TO TABTKF-I
           PERFORM WITH TEST AFTER
                   VARYING TABTKF-I FROM 1 BY 1
                     UNTIL TABTKF-I >= TABTKF-MAX
                        OR I-22
               IF  TK = TABTKF (TABTKF-I)
                   SET I-22                TO TRUE
                   SET TABTKF-S            TO TABTKF-I
               END-IF
           END-PERFORM
           SET TABTKF-I                    TO TABTKF-S
           IF  I-22
           AND TABTKF-I NOT > TABTKT-MAX
               SET TABTKT-I                TO TABTKF-I
           END-IF
           IF  (I-22)
               MOVE TABTKT(TABTKT-I)       TO TK
           END-IF
           MOVE BLDAT1                     TO BDATO1
           MOVE BLDAG1                     TO BDATO1 (1:2)
           MOVE BLAAR1                     TO BDATO1 (5:2)
           MOVE FFDAT1                     TO FDATO1
           MOVE FFDAG1                     TO FDATO1 (1:2)
           MOVE FFAAR1                     TO FDATO1 (5:2)
           IF  (I-20)
               ADD 1                       TO KREANT
           END-IF
           IF  (I-82)
               GO TO ENDRES-T
      * LES FAKTKID
           END-IF
           MOVE 'F'                        TO FAKKY4 (1:1)
           MOVE FINR                       TO FAKKY4 (2:3)
           MOVE FAKKY4                     TO FAKKEY (1:4)
           MOVE REFNR                      TO FAKKEY (5:6)
           MOVE FAKKEY                     TO FAKTKID-KEY1
           READ FAKTKID RECORD KEY IS FAKTKID-KEY1
           INVALID KEY
               SET I-23                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-23                TO TRUE
               PERFORM FAKTKID-FLDSET
               PERFORM FAKTKID-IDSET
           END-READ
           IF  (I-23)
               ADD 1                       TO FKDANT
               GO TO ENDRES-T
      * LES KIDFILE, SJEKK DUPLIKATER
           END-IF
           MOVE TILFNR                     TO KIDK09 (1:3)
           MOVE TRESNR                     TO KIDK09 (4:6)
           MOVE TBILNR                     TO KIDK12 (1:6)
           MOVE REFNR                      TO KIDK12 (7:6)
           MOVE KIDK09                     TO KIDKEY (1:9)
           MOVE KIDK12                     TO KIDKEY (10:12)
           MOVE KIDKEY                     TO KIDFILE-KEY1
           READ KIDFILE RECORD KEY IS KIDFILE-KEY1
           INVALID KEY
               SET I-24                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-24                TO TRUE
               PERFORM KIDFILE-IDSET
           END-READ
           IF  (NOT-I-24)
               SET I-25                    TO TRUE
           END-IF
           IF  (I-24)
               ADD 1                       TO KIDANT
           END-IF
           IF  (I-25)
               ADD 1                       TO KIDDUP
           END-IF
           IF  (NOT-I-50)
               ADD 1                       TO RELANT
           END-IF.
 
       ENDRES-T.
           CONTINUE.
      **********************************************************
      *  SUBRUTINE FOR ENDRING AV REGNSKAPSRECORDS.            *
      **********************************************************
 
       REGRUT-S SECTION.
       REGRUT-S-P.
           SET NOT-I-38                    TO TRUE
           SET NOT-I-39                    TO TRUE
           SET NOT-I-51                    TO TRUE
           IF  BINR NOT = BINRXX
               SET I-51                    TO TRUE
           END-IF
           IF  (I-51)
               MOVE BINR                   TO BINRXX
           END-IF
           SET NOT-I-31                    TO TRUE
           IF  REGSIG = '-'
               SET I-31                    TO TRUE
           END-IF
           IF  (I-31)
               MOVE ' '                    TO REGSIG
           END-IF
           IF  (NOT-I-31)
               MOVE '-'                    TO REGSIG
           END-IF
           IF  (I-31)
               ADD REGBEL                  TO REGSUM
           END-IF
           IF  (NOT-I-31)
               SUBTRACT REGBEL             FROM REGSUM
           END-IF
           SET NOT-I-61                    TO TRUE
           IF  REGSUM NOT < 0
               SET I-61                    TO TRUE
           END-IF
           ADD 1                           TO REGANT
           IF  (I-31)
               ADD REGBEL                  TO REGTOT
           END-IF
           IF  (NOT-I-31)
               SUBTRACT REGBEL             FROM REGTOT
           END-IF
           IF  (I-31)
               ADD RGB132                  TO REGTOU
           END-IF
           IF  (NOT-I-31)
               SUBTRACT RGB132             FROM REGTOU
           END-IF
           SET NOT-I-21                    TO TRUE
           SET TABBMK-S                    TO TABBMK-I
           PERFORM WITH TEST AFTER
                   VARYING TABBMK-I FROM 1 BY 1
                     UNTIL TABBMK-I >= TABBMK-MAX
                        OR I-21
               IF  REGBM = TABBMK (TABBMK-I)
                   SET I-21                TO TRUE
                   SET TABBMK-S            TO TABBMK-I
               END-IF
           END-PERFORM
           SET TABBMK-I                    TO TABBMK-S
           IF  I-21
           AND TABBMK-I NOT > TABBML-MAX
               SET TABBML-I                TO TABBMK-I
           END-IF
           IF  (I-21)
               MOVE TABBML(TABBML-I)       TO REGBM
           END-IF
           MOVE 0                          TO NULL10
           MOVE 0                          TO NULL15
           MOVE TRESNR                     TO VKONTO (1:6)
           MOVE '  '                       TO VKONTO (7:2)
           SET NOT-I-35                    TO TRUE
           IF  HKL = '5'
               SET I-35                    TO TRUE
           END-IF
           IF  (I-35)
               MOVE '3'                    TO HKONTO (1:1)
           END-IF
           SET NOT-I-32                    TO TRUE
           IF  HKONTO = '2201    '
               SET I-32                    TO TRUE
           END-IF
           IF  (I-32)
               MOVE '2200    '             TO HKONTO
           END-IF
           IF  (NOT-I-32 AND NOT-I-35)
               SET I-38                    TO TRUE
           END-IF
           IF  (I-38)
               MOVE 1                      TO X
               MOVE 'XXXXX'                TO W5
               SET NOT-I-41                TO TRUE
               SET ARF-S                   TO 1
               PERFORM WITH TEST AFTER
                       VARYING ARF-I FROM X BY 1
                         UNTIL ARF-I >= ARF-MAX
                            OR I-41
                   IF  W5 = ARF (ARF-I)
                       SET I-41            TO TRUE
                       SET ARF-S           TO ARF-I
                   END-IF
               END-PERFORM
               SET X                       TO ARF-S
               MOVE ART (X) (5:1)          TO TEST1
               SET NOT-I-39                TO TRUE
               IF  TEST1 = ' '
                   SET I-39                TO TRUE
               END-IF
           END-IF
           IF  (I-38 AND NOT-I-39)
               MOVE '3'                    TO HKONT6 (1:1)
               MOVE ART (X)                TO HKONT6 (2:5)
           END-IF
           IF  (I-38 AND I-39)
               MOVE ART (X)                TO HKONT6 (1:5)
               MOVE '  '                   TO HKONT6 (5:2)
           END-IF
           IF  (I-38 AND NOT-I-41)
               MOVE '310000'               TO HKONT6
           END-IF
           IF  (I-38)
               MOVE HKONT6                 TO HKONTO (1:6)
               MOVE '  '                   TO HKONTO (7:2)
           END-IF
           IF  (I-38 AND I-39)
               MOVE TAVD                   TO HKONTO (5:4)
           END-IF
           IF  (I-38)
               MOVE '0'                    TO REGAK
               SET NOT-I-42                TO TRUE
           END-IF
           IF  (I-32)
               MOVE '0'                    TO REGAK
           END-IF
           SET NOT-I-36                    TO TRUE
           IF  REGAK = '6'
               SET I-36                    TO TRUE
           END-IF
           SET NOT-I-34                    TO TRUE
           IF  REGAK = '4'
               SET I-34                    TO TRUE
           END-IF
           IF  (I-36)
               MOVE 'A'                    TO REGAK
           END-IF
           IF  (I-34)
               MOVE '0'                    TO REGAK
           END-IF.
      **********************************************************
      *  SUBRUTINE FOR ENDRING AV VAREGRUPPE.                  *
      *  HVIS SISTE SIFFER I VGR ER BLANK, ER DET H.BOK KONTO  *
      *        DETTE BENYTTES PÅ RENTENOTA OL.                 *
      **********************************************************
 
       VGRRUT-S SECTION.
       VGRRUT-S-P.
           MOVE 1                          TO X
           SET NOT-I-41                    TO TRUE
           SET ARF-S                       TO 1
           PERFORM WITH TEST AFTER
                   VARYING ARF-I FROM X BY 1
                     UNTIL ARF-I >= ARF-MAX
                        OR I-41
               IF  FRAVGR = ARF (ARF-I)
                   SET I-41                TO TRUE
                   SET ARF-S               TO ARF-I
               END-IF
           END-PERFORM
           SET X                           TO ARF-S
           IF  (NOT-I-41)
               MOVE 1                      TO X
               MOVE 'XXXXX'                TO W5
               SET NOT-I-41                TO TRUE
               SET ARF-S                   TO 1
               PERFORM WITH TEST AFTER
                       VARYING ARF-I FROM X BY 1
                         UNTIL ARF-I >= ARF-MAX
                            OR I-41
                   IF  W5 = ARF (ARF-I)
                       SET I-41            TO TRUE
                       SET ARF-S           TO ARF-I
                   END-IF
               END-PERFORM
               SET X                       TO ARF-S
           END-IF
           IF  (I-41)
               MOVE ART (X)                TO TILVGR
           END-IF
           IF  (NOT-I-41)
               MOVE FRAVGR                 TO TILVGR
               SET I-45                    TO TRUE
           END-IF
           MOVE TILVGR (5:1)               TO TEST1
           SET NOT-I-42                    TO TRUE
           IF  TEST1 = ' '
               SET I-42                    TO TRUE
           END-IF
           IF  (NOT-I-42)
               MOVE TILVGR                 TO W7 (1:5)
           END-IF
           IF  (I-42)
               MOVE TILVGR                 TO W8 (1:5)
               MOVE TAVD                   TO WKOST
               MOVE WKOST                  TO W8 (5:4)
           END-IF
           IF  (NOT-I-42)
               MOVE W7                     TO HKONTO (2:7)
           END-IF
           IF  (I-42)
               MOVE W8                     TO HKONTO
           END-IF.
      *****************************************************************
      * SUBRUTINE FOR Å HENTE ALTERNATIVT KONTONR FRA KONTOMA.        *
      *****************************************************************
 
       KTORUT-S SECTION.
       KTORUT-S-P.
           MOVE HKONTO (1:4)               TO TILKTO
           MOVE HKONTO (5:4)               TO TILSTD
           MOVE TILFNR                     TO KTOKEY (1:3)
           MOVE TILKTO                     TO KTOKEY (4:4)
           MOVE KTOKEY                     TO KONTOMA-KEY1
           READ KONTOMA RECORD KEY IS KONTOMA-KEY1
           INVALID KEY
               SET I-85                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-85                TO TRUE
               PERFORM KONTOMA-FLDSET
               PERFORM KONTOMA-IDSET
           END-READ
           IF  (NOT-I-85)
               SET I-87                    TO TRUE
           END-IF.
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE ' '                        TO BBEST
           MOVE 'REG32'                    TO LONR
           MOVE TILFNR                     TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'REG820  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF.
      *****************************************************************
 
       LR-CALCS SECTION.
       LR-CALCS-P.
           ADD REGANT TO ZERO          GIVING ARGANT
           ADD REGTOT TO ZERO          GIVING ARGBEL
           ADD REGTOU TO ZERO          GIVING ARGBEU
           ADD RESANT TO ZERO          GIVING ARSANT
           ADD RESTOT TO ZERO          GIVING ARSBEL
           ADD RESTOU TO ZERO          GIVING ARSBEU
           MOVE UDATE                      TO DDMMAA
           MOVE DDMMAA (1:2)               TO DD
           MOVE DDMMAA (3:4)               TO MMAA
           MOVE MMAA (1:2)                 TO MM
           MOVE MMAA (3:2)                 TO AA
           MOVE 20                         TO PDTO4 (1:2)
           MOVE AA                         TO PDTO4-IO (3:2)
           MOVE PDTO4                      TO PRDDTO (1:4)
           MOVE MM                         TO PDTO4 (1:2)
           MOVE DD                         TO PDTO4-IO (3:2)
           MOVE PDTO4                      TO PRDDTO-IO (5:4)
           ACCEPT SYSTEM-TIME-X          FROM TIME
           MOVE SYSTEM-TIME                TO PRDKLK (1:6)
      **********************************************************
      *  SUBRUTINE FOR ENDRING AV RESKONTRORECORDS.            *
      *                                                        *
      **********************************************************
           .
 
       BILAGFI-GET SECTION.
       BILAGFI-GET-P.
           IF  BILAGFI-EOF-OFF
               READ BILAGFI
               AT END
                   SET BILAGFI-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       BILAGFI-FLDOFF SECTION.
       BILAGFI-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-37                TO TRUE
           END-EVALUATE.
 
       BILAGFI-FLDSET SECTION.
       BILAGFI-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE BILAGFI-IO-AREA (1:3)  TO FINR (1:3)
               MOVE BILAGFI-IO-AREA (5:6)  TO BINR (1:6)
               MOVE BILAGFI-IO-AREA (11:3) TO TILFNR (1:3)
               MOVE BILAGFI-IO-AREA (14:1) TO TBART (1:1)
               MOVE BILAGFI-IO-AREA (15:6) TO TBILNR (1:6)
               MOVE BILAGFI-IO-AREA (21:6) TO TRESNR (1:6)
               MOVE BILAGFI-IO-AREA (27:6) TO FRESNR (1:6)
               MOVE BILAGFI-IO-AREA (33:1) TO TAVD1 (1:1)
               IF  TAVD1 = SPACES
                   SET I-37                TO TRUE
               END-IF
               MOVE BILAGFI-IO-AREA (33:4) TO TAVD (1:4)
               MOVE 217                    TO BW-A
               PERFORM VARYING ARF-I FROM ARF-MAX BY -1
                         UNTIL ARF-I < 1
                   SUBTRACT 5            FROM BW-A
                   MOVE BILAGFI-IO-AREA (BW-A:5) TO ARF-ENTRY (ARF-I)
               END-PERFORM
               MOVE 397                    TO BW-A
               PERFORM VARYING ART-I FROM ART-MAX BY -1
                         UNTIL ART-I < 1
                   SUBTRACT 5            FROM BW-A
                   MOVE BILAGFI-IO-AREA (BW-A:5) TO ART-ENTRY (ART-I)
               END-PERFORM
           END-EVALUATE.
 
       BILAGFI-IDSET SECTION.
       BILAGFI-IDSET-P.
           SET I-07                        TO TRUE.
 
       BILAGFI-CHK-LEVEL SECTION.
       BILAGFI-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO BILAGFI-LEVEL-07
               MOVE BILAGFI-IO-AREA (11:3) TO BILAGFI-07-L3-TILFNR
               MOVE BILAGFI-IO-AREA (1:3)  TO BILAGFI-07-L2-FINR
               MOVE BILAGFI-IO-AREA (5:6)  TO BILAGFI-07-L1-BINR
               IF  BILAGFI-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  BILAGFI-07-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  BILAGFI-07-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  BILAGFI-07-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  BILAGFI-07-L3         TO THE-PRIOR-L3
               MOVE  BILAGFI-07-L2         TO THE-PRIOR-L2
               MOVE  BILAGFI-07-L1         TO THE-PRIOR-L1
               SET BILAGFI-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       BILAGFI-MATCH-SET SECTION.
       BILAGFI-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE BILAGFI-IO-AREA (11:3) TO BILAGFI-M-07-M3-TILFNR
               MOVE BILAGFI-IO-AREA (1:3)  TO BILAGFI-M-07-M2-FINR
               MOVE BILAGFI-IO-AREA (5:6)  TO BILAGFI-M-07-M1-BINR
           END-EVALUATE.
 
       SELRESK-GET SECTION.
       SELRESK-GET-P.
           IF  SELRESK-EOF-OFF
               READ SELRESK
               AT END
                   SET SELRESK-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       SELRESK-FLDSET SECTION.
       SELRESK-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE SELRESK-IO-AREA (1:200) TO RESREC (1:200)
               MOVE SELRESK-IO-AREA (3:3)  TO FINR (1:3)
               MOVE SELRESK-IO-AREA (12:6) TO REFNR (1:6)
               MOVE SELRESK-IO-AREA (18:2) TO TK (1:2)
               MOVE SELRESK-IO-AREA (20:6) TO BLDAT1 (1:6)
               MOVE SELRESK-IO-AREA (20:2) TO BLAAR1 (1:2)
               MOVE SELRESK-IO-AREA (24:2) TO BLDAG1 (1:2)
               MOVE SELRESK-IO-AREA (26:6) TO BINR (1:6)
               MOVE SELRESK-IO-AREA (32:1) TO BIART (1:1)
               MOVE SELRESK-IO-AREA (33:6) TO FFDAT1 (1:6)
               MOVE SELRESK-IO-AREA (33:2) TO FFAAR1 (1:2)
               MOVE SELRESK-IO-AREA (37:2) TO FFDAG1 (1:2)
               MOVE SELRESK-IO-AREA (39:9) TO RESBEL-IO
               INSPECT RESBEL-IO REPLACING ALL ' ' BY '0'
               MOVE SELRESK-IO-AREA (49:2) TO RESBM (1:2)
               MOVE SELRESK-IO-AREA (86:3) TO TILFNR (1:3)
               MOVE SELRESK-IO-AREA (114:7) TO RSB132-IO
           END-EVALUATE.
 
       SELRESK-IDSET SECTION.
       SELRESK-IDSET-P.
           SET I-01                        TO TRUE.
 
       SELRESK-CHK-LEVEL SECTION.
       SELRESK-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO SELRESK-LEVEL-01
               MOVE SELRESK-IO-AREA (86:3) TO SELRESK-01-L3-TILFNR
               MOVE SELRESK-IO-AREA (3:3)  TO SELRESK-01-L2-FINR
               MOVE SELRESK-IO-AREA (26:6) TO SELRESK-01-L1-BINR
               IF  SELRESK-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  SELRESK-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  SELRESK-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  SELRESK-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  SELRESK-01-L3         TO THE-PRIOR-L3
               MOVE  SELRESK-01-L2         TO THE-PRIOR-L2
               MOVE  SELRESK-01-L1         TO THE-PRIOR-L1
               SET SELRESK-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       SELRESK-MATCH-SET SECTION.
       SELRESK-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE SELRESK-IO-AREA (86:3) TO SELRESK-M-01-M3-TILFNR
               MOVE SELRESK-IO-AREA (3:3)  TO SELRESK-M-01-M2-FINR
               MOVE SELRESK-IO-AREA (26:6) TO SELRESK-M-01-M1-BINR
           END-EVALUATE.
 
       SELREGN-GET SECTION.
       SELREGN-GET-P.
           IF  SELREGN-EOF-OFF
               READ SELREGN
               AT END
                   SET SELREGN-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       SELREGN-FLDSET SECTION.
       SELREGN-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE SELREGN-IO-AREA (1:240) TO REGREC (1:240)
               MOVE SELREGN-IO-AREA (3:3)  TO FINR (1:3)
               MOVE SELREGN-IO-AREA (6:1)  TO BIART (1:1)
               MOVE SELREGN-IO-AREA (7:6)  TO BINR (1:6)
               MOVE SELREGN-IO-AREA (19:8) TO VKONTO (1:8)
               MOVE SELREGN-IO-AREA (27:5) TO REGBEL-IO
               MOVE SELREGN-IO-AREA (32:1) TO REGSIG (1:1)
               MOVE SELREGN-IO-AREA (47:8) TO HKONTO (1:8)
               MOVE SELREGN-IO-AREA (47:6) TO HKONT6 (1:6)
               MOVE SELREGN-IO-AREA (48:5) TO FRAVGR (1:5)
               MOVE SELREGN-IO-AREA (47:1) TO HKL (1:1)
               MOVE SELREGN-IO-AREA (56:1) TO REGAK (1:1)
               MOVE SELREGN-IO-AREA (86:2) TO REGBM (1:2)
               MOVE SELREGN-IO-AREA (118:3) TO TILFNR (1:3)
               MOVE SELREGN-IO-AREA (121:7) TO RGB132-IO
           END-EVALUATE.
 
       SELREGN-IDSET SECTION.
       SELREGN-IDSET-P.
           SET I-02                        TO TRUE.
 
       SELREGN-CHK-LEVEL SECTION.
       SELREGN-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO SELREGN-LEVEL-02
               MOVE SELREGN-IO-AREA (118:3) TO SELREGN-02-L3-TILFNR
               MOVE SELREGN-IO-AREA (3:3)  TO SELREGN-02-L2-FINR
               MOVE SELREGN-IO-AREA (7:6)  TO SELREGN-02-L1-BINR
               IF  SELREGN-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  SELREGN-02-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  SELREGN-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  SELREGN-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  SELREGN-02-L3         TO THE-PRIOR-L3
               MOVE  SELREGN-02-L2         TO THE-PRIOR-L2
               MOVE  SELREGN-02-L1         TO THE-PRIOR-L1
               SET SELREGN-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       SELREGN-MATCH-SET SECTION.
       SELREGN-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE SELREGN-IO-AREA (118:3) TO SELREGN-M-02-M3-TILFNR
               MOVE SELREGN-IO-AREA (3:3)  TO SELREGN-M-02-M2-FINR
               MOVE SELREGN-IO-AREA (7:6)  TO SELREGN-M-02-M1-BINR
           END-EVALUATE.
 
       FAKTKID-FLDSET SECTION.
       FAKTKID-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKTKID-IO-AREA (5:7)  TO FKKID (1:7)
           END-EVALUATE.
 
       FAKTKID-IDSET SECTION.
       FAKTKID-IDSET-P.
           SET I-03                        TO TRUE.
 
       KIDFILE-IDSET SECTION.
       KIDFILE-IDSET-P.
           SET I-04                        TO TRUE.
 
       SYSPARM-FLDSET SECTION.
       SYSPARM-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE SYSPARM-IO-AREA (24:1) TO FIRKID (1:1)
               MOVE SYSPARM-IO-AREA (026:1) TO SYSALT (1:1)
           END-EVALUATE.
 
       SYSPARM-IDSET SECTION.
       SYSPARM-IDSET-P.
           SET I-05                        TO TRUE.
 
       KONTOMA-FLDSET SECTION.
       KONTOMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KONTOMA-IO-AREA (45:4) TO ALTKTO (1:4)
           END-EVALUATE.
 
       KONTOMA-IDSET SECTION.
       KONTOMA-IDSET-P.
           SET I-06                        TO TRUE.
 
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
 
       LISTE2-PRINT-LINE SECTION.
       LISTE2-PRINT-LINE-P.
           IF  LISTE2-BEFORE-SKIP > 0
               PERFORM LISTE2-SKIP-BEFORE
           END-IF
           IF  LISTE2-BEFORE-SPACE > 0
               PERFORM LISTE2-SPACE-BEFORE
               IF  LISTE2-AFTER-SKIP > 0
                   PERFORM LISTE2-SKIP-AFTER
               END-IF
               IF  LISTE2-AFTER-SPACE > 0
                   PERFORM LISTE2-SPACE-AFTER
               END-IF
           ELSE
               IF  LISTE2-AFTER-SKIP > 0
                   PERFORM LISTE2-SKIP-AFTER
               END-IF
               PERFORM LISTE2-SPACE-AFTER
           END-IF
           IF  LISTE2-LINE-COUNT NOT < LISTE2-MAX-LINES
               MOVE 7                      TO LISTE2-AFTER-SKIP
           END-IF.
 
       LISTE2-SKIP-BEFORE SECTION.
       LISTE2-SKIP-BEFORE-P.
           WRITE LISTE2-IO-PRINT        AFTER ADVANCING PAGE
           MOVE 1                          TO LISTE2-LINE-COUNT
           MOVE 0                          TO LISTE2-BEFORE-SKIP
           INITIALIZE LISTE2-IO-AREA.
 
       LISTE2-SPACE-BEFORE SECTION.
       LISTE2-SPACE-BEFORE-P.
           WRITE LISTE2-IO-PRINT        AFTER LISTE2-BEFORE-SPACE LINES
           ADD LISTE2-BEFORE-SPACE         TO LISTE2-LINE-COUNT
           MOVE SPACES TO LISTE2-IO-AREA
           INITIALIZE LISTE2-IO-AREA
           MOVE 0                          TO LISTE2-BEFORE-SPACE.
 
       LISTE2-SKIP-AFTER SECTION.
       LISTE2-SKIP-AFTER-P.
           WRITE LISTE2-IO-PRINT       BEFORE ADVANCING PAGE
           MOVE 1                          TO LISTE2-LINE-COUNT
           MOVE 0                          TO LISTE2-AFTER-SKIP
           INITIALIZE LISTE2-IO-AREA.
 
       LISTE2-SPACE-AFTER SECTION.
       LISTE2-SPACE-AFTER-P.
           WRITE LISTE2-IO-PRINT       BEFORE LISTE2-AFTER-SPACE LINES
           ADD LISTE2-AFTER-SPACE          TO LISTE2-LINE-COUNT
           INITIALIZE LISTE2-IO-AREA
           MOVE 0                          TO LISTE2-AFTER-SPACE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  BILAGFI-EOF
               MOVE HIGH-VALUES            TO BILAGFI-MC
                                              BILAGFI-MP
           END-IF
           IF  SELRESK-EOF
               MOVE HIGH-VALUES            TO SELRESK-MC
                                              SELRESK-MP
           END-IF
           IF  SELREGN-EOF
               MOVE HIGH-VALUES            TO SELREGN-MC
                                              SELREGN-MP
           END-IF
           IF  BILAGFI-MC < BILAGFI-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  SELRESK-MC < SELRESK-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  SELREGN-MC < SELREGN-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  BILAGFI-MC < SELRESK-MC
            AND  BILAGFI-MC < SELREGN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET BILAGFI-PROCESS     TO TRUE
                   MOVE BILAGFI-MC         TO BILAGFI-MP
                   IF  BILAGFI-MC = SELRESK-MP
                     OR  BILAGFI-MC = SELREGN-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  SELRESK-MC < BILAGFI-MC
            AND  SELRESK-MC < SELREGN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET SELRESK-PROCESS     TO TRUE
                   MOVE SELRESK-MC         TO SELRESK-MP
                   IF  SELRESK-MC = BILAGFI-MP
                     OR  SELRESK-MC = SELREGN-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  SELREGN-MC < BILAGFI-MC
            AND  SELREGN-MC < SELRESK-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET SELREGN-PROCESS     TO TRUE
                   MOVE SELREGN-MC         TO SELREGN-MP
                   IF  SELREGN-MC = BILAGFI-MP
                     OR  SELREGN-MC = SELRESK-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  BILAGFI-MC = SELRESK-MC
             OR  BILAGFI-MC = SELREGN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET BILAGFI-PROCESS     TO TRUE
                   MOVE BILAGFI-MC         TO BILAGFI-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           WHEN  SELRESK-MC = SELREGN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET SELRESK-PROCESS     TO TRUE
                   MOVE SELRESK-MC         TO SELRESK-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       BMTAB-LOAD SECTION.
       BMTAB-LOAD-P.
           OPEN INPUT BMTAB
           SET TABBMK-I                    TO 1
           PERFORM UNTIL BMTAB-EOF
               READ BMTAB
               AT END
                   SET BMTAB-EOF           TO TRUE
               NOT AT END
                   MOVE BMTAB-IO-AREA (1:4) TO TABBMK-ENTRY (TABBMK-I)
                   SET TABBMK-I            UP BY 1
                   MOVE BMTAB-IO-AREA (5:4) TO TABBMK-ENTRY (TABBMK-I)
                   SET TABBMK-I            UP BY 1
                   MOVE BMTAB-IO-AREA (9:4) TO TABBMK-ENTRY (TABBMK-I)
                   SET TABBMK-I            UP BY 1
                   MOVE BMTAB-IO-AREA (13:4) TO TABBMK-ENTRY (TABBMK-I)
                   SET TABBMK-I            UP BY 1
                   MOVE BMTAB-IO-AREA (17:4) TO TABBMK-ENTRY (TABBMK-I)
                   SET TABBMK-I            UP BY 1
                   MOVE BMTAB-IO-AREA (21:4) TO TABBMK-ENTRY (TABBMK-I)
                   SET TABBMK-I            UP BY 1
                   MOVE BMTAB-IO-AREA (25:4) TO TABBMK-ENTRY (TABBMK-I)
                   SET TABBMK-I            UP BY 1
                   MOVE BMTAB-IO-AREA (29:4) TO TABBMK-ENTRY (TABBMK-I)
                   SET TABBMK-I            UP BY 1
                   MOVE BMTAB-IO-AREA (33:4) TO TABBMK-ENTRY (TABBMK-I)
                   SET TABBMK-I            UP BY 1
                   MOVE BMTAB-IO-AREA (37:4) TO TABBMK-ENTRY (TABBMK-I)
                   SET TABBMK-I            UP BY 1
                   MOVE BMTAB-IO-AREA (41:4) TO TABBMK-ENTRY (TABBMK-I)
                   SET TABBMK-I            UP BY 1
                   MOVE BMTAB-IO-AREA (45:4) TO TABBMK-ENTRY (TABBMK-I)
                   SET TABBMK-I            UP BY 1
                   MOVE BMTAB-IO-AREA (49:4) TO TABBMK-ENTRY (TABBMK-I)
                   SET TABBMK-I            UP BY 1
                   MOVE BMTAB-IO-AREA (53:4) TO TABBMK-ENTRY (TABBMK-I)
                   SET TABBMK-I            UP BY 1
                   MOVE BMTAB-IO-AREA (57:4) TO TABBMK-ENTRY (TABBMK-I)
                   SET TABBMK-I            UP BY 1
                   MOVE BMTAB-IO-AREA (61:4) TO TABBMK-ENTRY (TABBMK-I)
                   SET TABBMK-I            UP BY 1
                   MOVE BMTAB-IO-AREA (65:4) TO TABBMK-ENTRY (TABBMK-I)
                   SET TABBMK-I            UP BY 1
                   MOVE BMTAB-IO-AREA (69:4) TO TABBMK-ENTRY (TABBMK-I)
                   SET TABBMK-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE BMTAB.
 
       TKTAB-LOAD SECTION.
       TKTAB-LOAD-P.
           OPEN INPUT TKTAB
           SET TABTKF-I                    TO 1
           PERFORM UNTIL TKTAB-EOF
               READ TKTAB
               AT END
                   SET TKTAB-EOF           TO TRUE
               NOT AT END
                   MOVE TKTAB-IO-AREA (1:4) TO TABTKF-ENTRY (TABTKF-I)
                   SET TABTKF-I            UP BY 1
                   MOVE TKTAB-IO-AREA (5:4) TO TABTKF-ENTRY (TABTKF-I)
                   SET TABTKF-I            UP BY 1
                   MOVE TKTAB-IO-AREA (9:4) TO TABTKF-ENTRY (TABTKF-I)
                   SET TABTKF-I            UP BY 1
                   MOVE TKTAB-IO-AREA (13:4) TO TABTKF-ENTRY (TABTKF-I)
                   SET TABTKF-I            UP BY 1
                   MOVE TKTAB-IO-AREA (17:4) TO TABTKF-ENTRY (TABTKF-I)
                   SET TABTKF-I            UP BY 1
                   MOVE TKTAB-IO-AREA (21:4) TO TABTKF-ENTRY (TABTKF-I)
                   SET TABTKF-I            UP BY 1
                   MOVE TKTAB-IO-AREA (25:4) TO TABTKF-ENTRY (TABTKF-I)
                   SET TABTKF-I            UP BY 1
                   MOVE TKTAB-IO-AREA (29:4) TO TABTKF-ENTRY (TABTKF-I)
                   SET TABTKF-I            UP BY 1
                   MOVE TKTAB-IO-AREA (33:4) TO TABTKF-ENTRY (TABTKF-I)
                   SET TABTKF-I            UP BY 1
                   MOVE TKTAB-IO-AREA (37:4) TO TABTKF-ENTRY (TABTKF-I)
                   SET TABTKF-I            UP BY 1
                   MOVE TKTAB-IO-AREA (41:4) TO TABTKF-ENTRY (TABTKF-I)
                   SET TABTKF-I            UP BY 1
                   MOVE TKTAB-IO-AREA (45:4) TO TABTKF-ENTRY (TABTKF-I)
                   SET TABTKF-I            UP BY 1
                   MOVE TKTAB-IO-AREA (49:4) TO TABTKF-ENTRY (TABTKF-I)
                   SET TABTKF-I            UP BY 1
                   MOVE TKTAB-IO-AREA (53:4) TO TABTKF-ENTRY (TABTKF-I)
                   SET TABTKF-I            UP BY 1
                   MOVE TKTAB-IO-AREA (57:4) TO TABTKF-ENTRY (TABTKF-I)
                   SET TABTKF-I            UP BY 1
                   MOVE TKTAB-IO-AREA (61:4) TO TABTKF-ENTRY (TABTKF-I)
                   SET TABTKF-I            UP BY 1
                   MOVE TKTAB-IO-AREA (65:4) TO TABTKF-ENTRY (TABTKF-I)
                   SET TABTKF-I            UP BY 1
                   MOVE TKTAB-IO-AREA (69:4) TO TABTKF-ENTRY (TABTKF-I)
                   SET TABTKF-I            UP BY 1
                   MOVE TKTAB-IO-AREA (73:4) TO TABTKF-ENTRY (TABTKF-I)
                   SET TABTKF-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE TKTAB.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-50)
               MOVE SPACES TO NYERESK-IO-AREA
               INITIALIZE NYERESK-IO-AREA
               MOVE RESREC                 TO NYERESK-IO-AREA (1:200)
               MOVE TILFNR                 TO NYERESK-IO-AREA (3:3)
               MOVE TRESNR                 TO NYERESK-IO-AREA (6:6)
               MOVE TK                     TO NYERESK-IO-AREA (18:2)
               MOVE TBILNR                 TO NYERESK-IO-AREA (26:6)
               MOVE TBART                  TO NYERESK-IO-AREA (32:1)
               MOVE RESBEL-IO              TO NYERESK-IO-AREA (39:9)
               MOVE ' '                    TO NYERESK-IO-AREA (48:1)
               MOVE RESBM                  TO NYERESK-IO-AREA (49:2)
               MOVE '   '                  TO NYERESK-IO-AREA (86:3)
               MOVE RSB132                 TO XO-112P
               MOVE XO-112P-EF             TO NYERESK-IO-AREA (114:7)
               MOVE NULL15                 TO XO-150P
               MOVE XO-150P-EF             TO NYERESK-IO-AREA (121:8)
               MOVE 'A'                    TO NYERESK-IO-AREA (191:1)
               MOVE PRDDTO                 TO XO-80P
               MOVE XO-80P-EF              TO NYERESK-IO-AREA (192:5)
               MOVE PRDKLK                 TO XO-60P
               MOVE XO-60P-EF              TO NYERESK-IO-AREA (197:4)
               WRITE NYERESK-IO-AREA
           END-IF
           IF  (I-01 AND I-50 AND I-24)
           AND (I-83)
               MOVE KIDKEY                 TO KIDFILE-IO-AREA (1:21)
               MOVE 'F'                    TO KIDFILE-IO-AREA (22:1)
               MOVE FKKID                  TO KIDFILE-IO-AREA (23:7)
               MOVE DD                     TO KIDFILE-IO-AREA (72:2)
               MOVE MM                     TO KIDFILE-IO-AREA (74:2)
               MOVE AA                     TO KIDFILE-IO-AREA (76:2)
               WRITE KIDFILE-IO-AREA
                   INVALID KEY
                       DISPLAY 'Bad WRITE - file = KIDFILE'
               END-WRITE
           END-IF
           IF  (I-02 AND I-50)
               MOVE SPACES TO NYEREGN-IO-AREA
               INITIALIZE NYEREGN-IO-AREA
               MOVE REGREC                 TO NYEREGN-IO-AREA (1:240)
               MOVE TILFNR                 TO NYEREGN-IO-AREA (3:3)
               MOVE TBART                  TO NYEREGN-IO-AREA (6:1)
               MOVE TBILNR                 TO NYEREGN-IO-AREA (7:6)
               MOVE VKONTO                 TO NYEREGN-IO-AREA (19:8)
               MOVE REGSIG                 TO NYEREGN-IO-AREA (32:1)
               MOVE NULL10                 TO XO-100P
               MOVE XO-100P-EF             TO NYEREGN-IO-AREA (33:6)
               MOVE ' '                    TO NYEREGN-IO-AREA (40:1)
               MOVE HKONTO                 TO NYEREGN-IO-AREA (47:8)
               MOVE REGAK                  TO NYEREGN-IO-AREA (56:1)
               MOVE '      '               TO NYEREGN-IO-AREA (57:6)
               MOVE TAVD                   TO NYEREGN-IO-AREA (63:4)
               MOVE '     '                TO NYEREGN-IO-AREA (67:5)
               MOVE '123'                  TO NYEREGN-IO-AREA (81:3)
               MOVE REGBM                  TO NYEREGN-IO-AREA (86:2)
               IF  (NOT-I-37)
                   MOVE TAVD1              TO NYEREGN-IO-AREA (94:1)
               END-IF
               MOVE '++'                   TO NYEREGN-IO-AREA (95:2)
               MOVE 'AUTO-POSTERING.         ' TO NYEREGN-IO-AREA
                                                               (97:24)
               MOVE NULL15                 TO XO-150P
               MOVE XO-150P-EF             TO NYEREGN-IO-AREA (128:8)
               MOVE 'A'                    TO NYEREGN-IO-AREA (231:1)
               MOVE PRDDTO                 TO XO-80P
               MOVE XO-80P-EF              TO NYEREGN-IO-AREA (232:5)
               MOVE PRDKLK                 TO XO-60P
               MOVE XO-60P-EF              TO NYEREGN-IO-AREA (237:4)
               WRITE NYEREGN-IO-AREA
           END-IF
           IF  (I-01 AND I-50 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'R E S K O N T R O'    TO LISTE-IO-AREA (2:17)
               MOVE 'P O S T E R I N G .'  TO LISTE-IO-AREA (20:19)
               MOVE 'BILAGSART'            TO LISTE-IO-AREA (50:9)
               MOVE TBART                  TO LISTE-IO-AREA (60:1)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'BILAGS  BILAGS'       TO LISTE-IO-AREA (5:14)
               MOVE 'RESK.   F'            TO LISTE-IO-AREA (23:9)
               MOVE 'FORFALLS  REF.'       TO LISTE-IO-AREA (53:14)
               IF  (NOT-I-20 AND I-83)
                   MOVE 'FAKTURA'          TO LISTE-IO-AREA (72:7)
               END-IF
               IF  (I-20 AND I-83)
                   MOVE 'KR.NOTA'          TO LISTE-IO-AREA (72:7)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'NUMMER  DATO  '       TO LISTE-IO-AREA (5:14)
               MOVE 'NUMMER  K'            TO LISTE-IO-AREA (23:9)
               MOVE 'BILAGSTOTAL   BM'     TO LISTE-IO-AREA (35:16)
               MOVE 'DATO      NUMMER'     TO LISTE-IO-AREA (53:16)
               IF  (I-83)
                   MOVE 'KID'              TO LISTE-IO-AREA (72:3)
               END-IF
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE TBILNR                 TO LISTE-IO-AREA (5:6)
               MOVE BDATO1                 TO LISTE-IO-AREA (13:6)
               MOVE TRESNR                 TO LISTE-IO-AREA (23:6)
               MOVE FAKKRE                 TO LISTE-IO-AREA (31:1)
               MOVE RESBEL                 TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (34:13)
               MOVE RESBM                  TO LISTE-IO-AREA (49:2)
               MOVE FDATO1                 TO LISTE-IO-AREA (53:6)
               MOVE REFNR                  TO LISTE-IO-AREA (63:6)
               IF  (I-23 AND I-83)
                   MOVE 'MANGLER'          TO LISTE-IO-AREA (72:7)
               END-IF
               IF  (NOT-I-23 AND I-83)
                   MOVE FKKID              TO LISTE-IO-AREA (72:7)
               END-IF
               IF  (I-82)
                   MOVE '       '          TO LISTE-IO-AREA (72:7)
               END-IF
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-02 AND I-50 AND I-51)
           AND (NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'R E G N S K A P S'    TO LISTE-IO-AREA (2:17)
               MOVE 'P O S T E R I N G .'  TO LISTE-IO-AREA (20:19)
               MOVE 'BILAGSART'            TO LISTE-IO-AREA (50:9)
               MOVE TBART                  TO LISTE-IO-AREA (60:1)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KONTO    D'           TO LISTE-IO-AREA (5:10)
               MOVE 'KONTO    D'           TO LISTE-IO-AREA (20:10)
               MOVE 'AVGIFTS'              TO LISTE-IO-AREA (50:7)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'NUMMER   K'           TO LISTE-IO-AREA (5:10)
               MOVE 'NUMMER   K'           TO LISTE-IO-AREA (20:10)
               MOVE 'BELØP'                TO LISTE-IO-AREA (41:5)
               MOVE 'KODE'                 TO LISTE-IO-AREA (51:4)
               MOVE 'AVD'                  TO LISTE-IO-AREA (57:3)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-02 AND I-50 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE VKONTO                 TO LISTE-IO-AREA (5:8)
               IF  (NOT-I-31)
                   MOVE 'K'                TO LISTE-IO-AREA (14:1)
               END-IF
               IF  (I-31)
                   MOVE 'D'                TO LISTE-IO-AREA (14:1)
               END-IF
               MOVE HKONTO                 TO LISTE-IO-AREA (20:8)
               IF  (I-84 AND I-87)
                   MOVE ALTKTO             TO LISTE-IO-AREA (20:4)
               END-IF
               IF  (I-84 AND I-87)
                   MOVE TILSTD             TO LISTE-IO-AREA (24:4)
               END-IF
               IF  (I-32 AND I-84 AND NOT-I-87)
                   MOVE 'UKJE'             TO LISTE-IO-AREA (24:4)
               END-IF
               IF  (I-42 AND I-84 AND NOT-I-87)
                   MOVE 'UKJE'             TO LISTE-IO-AREA (24:4)
               END-IF
               IF  (I-31)
                   MOVE 'K'                TO LISTE-IO-AREA (29:1)
               END-IF
               IF  (NOT-I-31)
                   MOVE 'D'                TO LISTE-IO-AREA (29:1)
               END-IF
               MOVE REGBEL                 TO XO-72YY9
               MOVE XO-72YY9               TO LISTE-IO-AREA (34:12)
               MOVE REGAK                  TO LISTE-IO-AREA (52:1)
               IF  (NOT-I-37)
                   MOVE TAVD               TO LISTE-IO-AREA (57:4)
               END-IF
               IF  (I-45)
                   MOVE 'VGR.'             TO LISTE-IO-AREA (62:4)
               END-IF
               IF  (I-45)
                   MOVE FRAVGR             TO LISTE-IO-AREA (66:5)
               END-IF
               IF  (I-45)
                   MOVE 'IKKE I VGR.KONV.TABELL.' TO LISTE-IO-AREA
                                                               (72:23)
               END-IF
               IF  (NOT-I-32 AND NOT-I-35)
                   MOVE 'ØREAVRUNDING           ' TO LISTE-IO-AREA
                                                               (72:23)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR AND I-98 AND I-U8)
           AND (I-61 AND I-03 AND I-04)
           AND (I-05 AND I-06 AND I-07)
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE PSDS                   TO LISTE2-IO-AREA (41:80)
               MOVE R                      TO LISTE2-IO-AREA (113:8)
               MOVE P-IO                   TO LISTE2-IO-AREA (118:3)
               MOVE S-IO                   TO LISTE2-IO-AREA (116:5)
               MOVE LONR                   TO LISTE2-IO-AREA (116:5)
               MOVE LFIRMA                 TO LISTE2-IO-AREA (118:3)
               MOVE LUNDGR                 TO LISTE2-IO-AREA (118:3)
               MOVE LPROG                  TO LISTE2-IO-AREA (113:8)
               MOVE LPRIID                 TO LISTE2-IO-AREA (117:4)
               MOVE BJOBN                  TO LISTE2-IO-AREA (113:8)
               MOVE BBEST                  TO LISTE2-IO-AREA (120:1)
               MOVE BPERS                  TO LISTE2-IO-AREA (91:30)
               MOVE BETTB                  TO LISTE2-IO-AREA (81:40)
               MOVE BFORS                  TO LISTE2-IO-AREA (81:40)
               MOVE BMEMO                  TO LISTE2-IO-AREA (81:40)
               MOVE BANTX-IO               TO LISTE2-IO-AREA (118:3)
               MOVE BPCLAS                 TO LISTE2-IO-AREA (120:1)
               MOVE BPRJE                  TO LISTE2-IO-AREA (118:3)
               MOVE 1                      TO LISTE2-BEFORE-SPACE
               MOVE 1                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L1 AND I-50 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE LOPNVN                 TO LISTE-IO-AREA (32:35)
               MOVE 'FREMSTILT'            TO LISTE-IO-AREA (70:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (80:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (90:4)
               IF  (I-L3)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40U
               MOVE XO-40U (1:4)           TO LISTE-IO-AREA (95:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF AND NOT-I-L1 AND I-50)
           AND (NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE LOPNVN                 TO LISTE-IO-AREA (32:35)
               MOVE 'FREMSTILT'            TO LISTE-IO-AREA (70:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (80:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (90:4)
               IF  (I-L3)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40U
               MOVE XO-40U (1:4)           TO LISTE-IO-AREA (95:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO AVSTEMM-IO-AREA
               INITIALIZE AVSTEMM-IO-AREA
               MOVE PRDDTO-IO              TO AVSTEMM-IO-AREA (1:8)
               MOVE '020'                  TO AVSTEMM-IO-AREA (9:3)
               MOVE 'REG'                  TO AVSTEMM-IO-AREA (12:3)
               MOVE 'REG820'               TO AVSTEMM-IO-AREA (15:6)
               MOVE '*REG820*'             TO AVSTEMM-IO-AREA (21:8)
               MOVE PRDKLK-IO              TO AVSTEMM-IO-AREA (30:6)
               MOVE ARGANT-IO              TO AVSTEMM-IO-AREA (36:13)
               MOVE ARGBEL-IO              TO AVSTEMM-IO-AREA (49:15)
               MOVE ARGBEU-IO              TO AVSTEMM-IO-AREA (64:15)
               MOVE PRDDTO-IO              TO AVSTEMM-IO-AREA (79:8)
               WRITE AVSTEMM-IO-AREA
               MOVE SPACES TO AVSTEMM-IO-AREA
               INITIALIZE AVSTEMM-IO-AREA
               MOVE '********'             TO AVSTEMM-IO-AREA (1:8)
               MOVE '020'                  TO AVSTEMM-IO-AREA (9:3)
               MOVE 'REG'                  TO AVSTEMM-IO-AREA (12:3)
               MOVE '******'               TO AVSTEMM-IO-AREA (15:6)
               MOVE '*REG820*'             TO AVSTEMM-IO-AREA (21:8)
               MOVE PRDKLK-IO              TO AVSTEMM-IO-AREA (30:6)
               MOVE ARGANT-IO              TO AVSTEMM-IO-AREA (36:13)
               MOVE ARGBEL-IO              TO AVSTEMM-IO-AREA (49:15)
               MOVE ARGBEU-IO              TO AVSTEMM-IO-AREA (64:15)
               MOVE PRDDTO-IO              TO AVSTEMM-IO-AREA (79:8)
               WRITE AVSTEMM-IO-AREA
               MOVE SPACES TO AVSTEMM-IO-AREA
               INITIALIZE AVSTEMM-IO-AREA
               MOVE PRDDTO-IO              TO AVSTEMM-IO-AREA (1:8)
               MOVE '020'                  TO AVSTEMM-IO-AREA (9:3)
               MOVE 'RES'                  TO AVSTEMM-IO-AREA (12:3)
               MOVE 'REG820'               TO AVSTEMM-IO-AREA (15:6)
               MOVE '*REG820*'             TO AVSTEMM-IO-AREA (21:8)
               MOVE PRDKLK-IO              TO AVSTEMM-IO-AREA (30:6)
               MOVE ARSANT-IO              TO AVSTEMM-IO-AREA (36:13)
               MOVE ARSBEL-IO              TO AVSTEMM-IO-AREA (49:15)
               MOVE ARSBEU-IO              TO AVSTEMM-IO-AREA (64:15)
               MOVE PRDDTO-IO              TO AVSTEMM-IO-AREA (79:8)
               WRITE AVSTEMM-IO-AREA
               MOVE SPACES TO AVSTEMM-IO-AREA
               INITIALIZE AVSTEMM-IO-AREA
               MOVE '********'             TO AVSTEMM-IO-AREA (1:8)
               MOVE '020'                  TO AVSTEMM-IO-AREA (9:3)
               MOVE 'RES'                  TO AVSTEMM-IO-AREA (12:3)
               MOVE '******'               TO AVSTEMM-IO-AREA (15:6)
               MOVE '*REG820*'             TO AVSTEMM-IO-AREA (21:8)
               MOVE PRDKLK-IO              TO AVSTEMM-IO-AREA (30:6)
               MOVE ARSANT-IO              TO AVSTEMM-IO-AREA (36:13)
               MOVE ARSBEL-IO              TO AVSTEMM-IO-AREA (49:15)
               MOVE ARSBEU-IO              TO AVSTEMM-IO-AREA (64:15)
               MOVE PRDDTO-IO              TO AVSTEMM-IO-AREA (79:8)
               WRITE AVSTEMM-IO-AREA
           END-IF
           IF  (I-L1 AND I-50 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '** REGNSKAPSTOTAL'    TO LISTE-IO-AREA (2:17)
               MOVE REGSUM                 TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (34:13)
               INITIALIZE REGSUM
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*** AUTO-POSTERT'     TO LISTE-IO-AREA (1:16)
               MOVE 'FRA FIRMA'            TO LISTE-IO-AREA (18:9)
               MOVE FINR                   TO LISTE-IO-AREA (28:3)
               MOVE 'BILAGSART'            TO LISTE-IO-AREA (32:9)
               MOVE BIART                  TO LISTE-IO-AREA (42:1)
               MOVE 'BILAGSNR'             TO LISTE-IO-AREA (44:8)
               MOVE BINR                   TO LISTE-IO-AREA (53:6)
               MOVE 'RESKONTRONR'          TO LISTE-IO-AREA (60:11)
               MOVE FRESNR                 TO LISTE-IO-AREA (72:6)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE 'AVSTEMMING PROG. REG820 ' TO LISTE2-IO-AREA (1:24)
               MOVE 01                     TO LISTE2-BEFORE-SKIP
               MOVE 3                      TO LISTE2-BEFORE-SPACE
               MOVE 3                      TO LISTE2-AFTER-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE 'TOTALSUMMER FRA'      TO LISTE2-IO-AREA (2:15)
               MOVE 'AUTO-POSTERING.'      TO LISTE2-IO-AREA (18:15)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE2-IO-AREA (34:8)
               MOVE 'PROG. REG820'         TO LISTE2-IO-AREA (46:12)
               MOVE 1                      TO LISTE2-BEFORE-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE 'SKAL VÆRE MED I DAGLIG' TO LISTE2-IO-AREA (2:22)
               MOVE 'AVSTEMMING.'          TO LISTE2-IO-AREA (25:11)
               MOVE 2                      TO LISTE2-BEFORE-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE 'TIL RESKONTRO'        TO LISTE2-IO-AREA (2:13)
               MOVE 1                      TO LISTE2-BEFORE-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE 'ANTALL'               TO LISTE2-IO-AREA (16:6)
               MOVE RESANT                 TO EDIT-RESANT
               MOVE EDIT-RESANT            TO LISTE2-IO-AREA (24:5)
               MOVE 'ANT KID'              TO LISTE2-IO-AREA (31:7)
               MOVE KIDANT                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE2-IO-AREA (38:6)
               MOVE 'DUPLIKAT KID'         TO LISTE2-IO-AREA (46:12)
               MOVE KIDDUP                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE2-IO-AREA (58:6)
               MOVE 'MANGLER FAKTKID'      TO LISTE2-IO-AREA (66:15)
               MOVE FKDANT                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE2-IO-AREA (81:6)
               MOVE 'MANGLER RELASJON'     TO LISTE2-IO-AREA (89:16)
               MOVE RELANT                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE2-IO-AREA (105:6)
               MOVE 'KR. NOTA'             TO LISTE2-IO-AREA (113:8)
               MOVE KREANT                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE2-IO-AREA (121:6)
               MOVE 1                      TO LISTE2-BEFORE-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE 'BELØP1'               TO LISTE2-IO-AREA (16:6)
               MOVE RESTOT                 TO EDIT-RESTOT
               MOVE EDIT-RESTOT            TO LISTE2-IO-AREA (29:12)
               MOVE 2                      TO LISTE2-BEFORE-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE 'UTVIDET'              TO LISTE2-IO-AREA (15:7)
               MOVE RESTOU                 TO EDIT-RESTOU
               MOVE EDIT-RESTOU            TO LISTE2-IO-AREA (26:15)
               MOVE 1                      TO LISTE2-BEFORE-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE 'TIL REGNSKAP '        TO LISTE2-IO-AREA (2:13)
               MOVE 1                      TO LISTE2-BEFORE-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE 'ANTALL'               TO LISTE2-IO-AREA (16:6)
               MOVE REGANT                 TO EDIT-REGANT
               MOVE EDIT-REGANT            TO LISTE2-IO-AREA (24:5)
               MOVE 1                      TO LISTE2-BEFORE-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE 'BELØP2'               TO LISTE2-IO-AREA (16:6)
               MOVE REGTOT                 TO EDIT-REGTOT
               MOVE EDIT-REGTOT            TO LISTE2-IO-AREA (29:12)
               MOVE 1                      TO LISTE2-BEFORE-SPACE
               PERFORM LISTE2-PRINT-LINE
               MOVE SPACES TO LISTE2-IO-AREA
               INITIALIZE LISTE2-IO-AREA
               MOVE 'UTVIDET'              TO LISTE2-IO-AREA (15:7)
               MOVE REGTOU                 TO EDIT-REGTOU
               MOVE EDIT-REGTOU            TO LISTE2-IO-AREA (26:15)
      * DUMMY-LINJE FOR Å FJERNE KOMPILERINGSFEIL
               MOVE 1                      TO LISTE2-BEFORE-SPACE
               PERFORM LISTE2-PRINT-LINE
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
           MOVE 3                          TO LR-CHECK
           PERFORM BMTAB-LOAD
           PERFORM TKTAB-LOAD
           SET BILAGFI-LEVEL-INIT          TO TRUE
           INITIALIZE BILAGFI-DATA-FIELDS
           SET BILAGFI-EOF-OFF             TO TRUE
           SET BILAGFI-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO BILAGFI-MC
                                              BILAGFI-MP
           OPEN INPUT BILAGFI
           SET SELRESK-LEVEL-INIT          TO TRUE
           INITIALIZE SELRESK-DATA-FIELDS
           SET SELRESK-EOF-OFF             TO TRUE
           SET SELRESK-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO SELRESK-MC
                                              SELRESK-MP
           OPEN INPUT SELRESK
           SET SELREGN-LEVEL-INIT          TO TRUE
           INITIALIZE SELREGN-DATA-FIELDS
           SET SELREGN-EOF-OFF             TO TRUE
           SET SELREGN-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO SELREGN-MC
                                              SELREGN-MP
           OPEN INPUT SELREGN
           INITIALIZE FAKTKID-DATA-FIELDS
           OPEN INPUT FAKTKID
           OPEN INPUT KIDFILE
           INITIALIZE SYSPARM-DATA-FIELDS
           OPEN INPUT SYSPARM
           INITIALIZE KONTOMA-DATA-FIELDS
           OPEN INPUT KONTOMA
           OPEN OUTPUT NYERESK
           OPEN OUTPUT NYEREGN
           OPEN OUTPUT AVSTEMM
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES
           OPEN OUTPUT LISTE2
           INITIALIZE LISTE2-IO-AREA
           INITIALIZE LISTE2-DATA-FIELDS
           MOVE 57                         TO LISTE2-MAX-LINES.
           SET TABBMK-I                    TO 1
           SET TABTKF-I                    TO 1
           PERFORM VARYING ARF-I FROM 1 BY 1
                     UNTIL ARF-I > ARF-MAX
               INITIALIZE ARF (ARF-I)
           END-PERFORM
           SET ARF-I                       TO 1
           PERFORM VARYING ART-I FROM 1 BY 1
                     UNTIL ART-I > ART-MAX
               INITIALIZE ART (ART-I)
           END-PERFORM
           SET ART-I                       TO 1
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE BILAGFI
           CLOSE SELRESK
           CLOSE SELREGN
           CLOSE FAKTKID
           CLOSE KIDFILE
           CLOSE SYSPARM
           CLOSE KONTOMA
           CLOSE NYERESK
           CLOSE NYEREGN
           CLOSE AVSTEMM
           IF LISTE-IO-AREA NOT = SPACES
             WRITE LISTE-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE-IO-AREA
           END-IF
           CLOSE LISTE
           IF LISTE2-IO-AREA NOT = SPACES
             WRITE LISTE2-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE2-IO-AREA
           END-IF
           CLOSE LISTE2.
 
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
