       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK170R.
      **********************************************  Z-WIN-RPG2   ****
      * UTPLUKK TIL FAKTURA STATISTIKK FRA REGNSKAPSRECORD.           *
      *  03.08.94   ESPEN LARSEN                                      *
      *   3.10.94   LAGT INN KR.NOTA FORDELINGSOPPGAVE.               *
      *  14.02.95   SUM FOR AVGIFTSFRITT SALG.                        *
      *  23.04.97   TOTALSUM KONTO 5000 HOVEDBOK.                     *
      *  25.07.97: TILPASSET BRUK AV ALT. KONTONR. MT.                *
      *  19.08.98: BLANKER ""TILKONTO"" PÅ LISTEN NÅR ""TILKONTO"" ER     *
      *            BLANK I INPUTFILE FOR FIRMA MED ALT.-KONTO PLAN.   *
      *            HENTER KONTO FOR SALG FRA SYSPARM.                 *
      *   3.08.01: ØKT TOTALSUMMER FRA 10,2 TIL 11,2                  *
      *  20.08.01: RETTET FEIL I ODATO VED OG FLYTTE FILE KONTOMA     *
      *            ETTER DATASTRUKTURE I INPUT. DET SER UT SOM OM     *
      *            DATASTRUKTURE LEGGER SEG OVER ORDREDATO I INPUT.   *
      *   6.03.06: UPSI LISTER UT DATALJER.                           *
      *   6.03.06: TOTALSUMMER PR. HOVEDBOKSKONTO.                    *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK170.rpg
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
           SELECT INFILE
               ASSIGN TO UT-S-INFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INFILE-STATUS.
           SELECT FAKPAR
               ASSIGN TO UT-S-FAKPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKPAR-STATUS.
           SELECT STATREC
               ASSIGN TO UT-S-STATREC
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS STATREC-STATUS.
           SELECT VAGRMAS
               ASSIGN TO VAGRMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAGRMAS-STATUS
               RECORD KEY IS VAGRMAS-KEY1.
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
           SELECT STAT
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS STAT-STATUS.
           SELECT LISTE
               ASSIGN TO UT-S-LISTE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD INFILE
               BLOCK CONTAINS 400
               RECORD CONTAINS 40.
       01  INFILE-IO-AREA.
           05  INFILE-IO-AREA-X            PICTURE X(40).
       FD FAKPAR
               BLOCK CONTAINS 200
               RECORD CONTAINS 200.
       01  FAKPAR-IO-AREA.
           05  FAKPAR-IO-AREA-X            PICTURE X(200).
       FD STATREC
               BLOCK CONTAINS 500
               RECORD CONTAINS 50.
       01  STATREC-IO-AREA.
           05  STATREC-IO-AREA-X           PICTURE X(50).
       FD VAGRMAS
               RECORD CONTAINS 80.
       01  VAGRMAS-IO-AREA.
           05  VAGRMAS-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  VAGRMAS-KEY1            PICTURE X(8).
               10  FILLER                  PICTURE X(71).
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
       FD STAT
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  STAT-IO-PRINT.
           05  STAT-IO-AREA-CONTROL        PICTURE X VALUE ' '.
        02 STAT-IO-AREA.
           05  STAT-IO-AREA-X              PICTURE X(132).
      *BUGFILO O   F  80  80            PRINTERSYSLST
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       WORKING-STORAGE SECTION.
       77  ARK-MAX   VALUE 30              PICTURE 9(4) USAGE BINARY.
       77  ARV-MAX   VALUE 30              PICTURE 9(4) USAGE BINARY.
       77  ARP-MAX   VALUE 30              PICTURE 9(4) USAGE BINARY.
       77  ARF-MAX   VALUE 30              PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  ARK-TABLE.
               10  ARK-ENTRY
                                           OCCURS 30 TIMES
                                           INDEXED BY ARK-I
                                                      ARK-S
                                                      ARV-I
                                                      ARV-S.
                   15  ARK                 PICTURE X(4).
                   15  ARV                 PICTURE X(4).
           05  ARP-TABLE.
               10  ARP-ENTRY
                                           OCCURS 30 TIMES
                                           INDEXED BY ARP-I
                                                      ARP-S.
                   15  ARP                 PICTURE S9(8)V9(2).
      * **START RBS - DATASTRUKTUR FOR SUB-PROGRAM RBS000 ********
           05  ARF-TABLE.
               10  ARF-ENTRY
                                           OCCURS 30 TIMES
                                           INDEXED BY ARF-I
                                                      ARF-S.
                   15  ARF                 PICTURE S9(8)V9(2).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  INFILE-STATUS               PICTURE 99 VALUE 0.
           10  FAKPAR-STATUS               PICTURE 99 VALUE 0.
           10  STATREC-STATUS              PICTURE 99 VALUE 0.
           10  VAGRMAS-STATUS              PICTURE 99 VALUE 0.
           10  SYSPARM-STATUS              PICTURE 99 VALUE 0.
           10  KONTOMA-STATUS              PICTURE 99 VALUE 0.
           10  STAT-STATUS                 PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  INFILE-EOF-OFF          VALUE '0'.
               88  INFILE-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INFILE-READ-OFF         VALUE '0'.
               88  INFILE-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INFILE-PROCESS-OFF      VALUE '0'.
               88  INFILE-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  INFILE-LEVEL-INIT-OFF   VALUE '0'.
               88  INFILE-LEVEL-INIT       VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKPAR-EOF-OFF          VALUE '0'.
               88  FAKPAR-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKPAR-READ-OFF         VALUE '0'.
               88  FAKPAR-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKPAR-PROCESS-OFF      VALUE '0'.
               88  FAKPAR-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  STATREC-EOF-OFF         VALUE '0'.
               88  STATREC-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  STATREC-READ-OFF        VALUE '0'.
               88  STATREC-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  STATREC-PROCESS-OFF     VALUE '0'.
               88  STATREC-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  STATREC-LEVEL-INIT-OFF  VALUE '0'.
               88  STATREC-LEVEL-INIT      VALUE '1'.
           05  VAGRMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  SYSPARM-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  KONTOMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  STAT-DATA-FIELDS.
               10  STAT-AFTER-SPACE        PICTURE 9(4) BINARY
                                           VALUE 0.
               10  STAT-AFTER-SKIP         PICTURE 9(4) BINARY
                                           VALUE 0.
               10  STAT-BEFORE-SPACE       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  STAT-BEFORE-SKIP        PICTURE 9(4) BINARY
                                           VALUE 0.
               10  STAT-MAX-LINES          PICTURE 9(4) BINARY
                                           VALUE 0.
               10  STAT-LINE-COUNT         PICTURE 9(4) BINARY
                                           VALUE 0.
               10  STAT-CLR-IO             PICTURE X VALUE 'Y'.
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
           05  INFILE-LEVEL-02.
               10  INFILE-02-L3.
                   15  INFILE-02-L3-FIRMA  PICTURE X(3).
               10  INFILE-02-L2.
                   15  INFILE-02-L2-AVD    PICTURE X(1).
               10  INFILE-02-L1.
                   15  INFILE-02-L1-VNR    PICTURE X(4).
           05  INFILE-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  VGR                     PICTURE X(5).
               10  VNR                     PICTURE X(4).
               10  AVD                     PICTURE X(1).
               10  BILDAT                  PICTURE X(6).
               10  BILMND                  PICTURE X(2).
               10  BEL-IO.
                   15  BEL                 PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  SIGN-X                  PICTURE X(1).
               10  TYPE-X                  PICTURE X(1).
               10  FAKREC                  PICTURE X(1).
               10  AVGKOD                  PICTURE X(1).
           05  INFILE-MP                   PICTURE X(3).
           05  INFILE-MC                   PICTURE X(3).
           05  INFILE-M-02             REDEFINES INFILE-MC.
               10  INFILE-M-02-M1.
                   15  INFILE-M-02-M1-FIRMA-G.
                       20  INFILE-M-02-M1-FIRMA PICTURE X(3).
           05  FAKPAR-DATA-FIELDS.
               10  ODATO-IO.
                   15  ODATO               PICTURE S9(6).
               10  PFADTO                  PICTURE X(6).
               10  MND                     PICTURE X(9).
           05  STATREC-LEVEL-03.
               10  STATREC-03-L3.
                   15  STATREC-03-L3-FIRMA PICTURE X(3).
           05  STATREC-DATA-FIELDS.
               10  ANTO2-IO.
                   15  ANTO2               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  ANTO3-IO.
                   15  ANTO3               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  ANTO4-IO.
                   15  ANTO4               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  ANTO5-IO.
                   15  ANTO5               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  ANTL2-IO.
                   15  ANTL2               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  ANTL3-IO.
                   15  ANTL3               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  ANTL4-IO.
                   15  ANTL4               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  ANTL5-IO.
                   15  ANTL5               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  SUMB2-IO.
                   15  SUMB2               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  SUMB3-IO.
                   15  SUMB3               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  SUMB4-IO.
                   15  SUMB4               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  SUMB5-IO.
                   15  SUMB5               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
           05  STATREC-MP                  PICTURE X(3).
           05  STATREC-MC                  PICTURE X(3).
           05  STATREC-M-03            REDEFINES STATREC-MC.
               10  STATREC-M-03-M1.
                   15  STATREC-M-03-M1-FIRMA-G.
                       20  STATREC-M-03-M1-FIRMA PICTURE X(3).
           05  VAGRMAS-DATA-FIELDS.
               10  VGRNA                   PICTURE X(40).
               10  TILKTO                  PICTURE X(4).
           05  SYSPARM-DATA-FIELDS.
               10  SYSALT                  PICTURE X(1).
               10  SALPLI                  PICTURE X(4).
               10  SALFRI                  PICTURE X(4).
           05  KONTOMA-DATA-FIELDS.
               10  ALTKTO                  PICTURE X(4).
               10  KTONAV                  PICTURE X(35).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(1).
               10  THE-PRIOR-L1            PICTURE X(4).
           05  TEMPORARY-FIELDS.
               10  VSFL3-IO.
                   15  VSFL3               PICTURE S9(9)V9(2).
               10  VSPL3-IO.
                   15  VSPL3               PICTURE S9(9)V9(2).
               10  X-IO.
                   15  X                   PICTURE S9(2).
               10  SYSKEY                  PICTURE X(10).
               10  KTOKEY                  PICTURE X(7).
               10  ALTSAL                  PICTURE X(4).
               10  ALTFRI                  PICTURE X(4).
               10  FAK-IO.
                   15  FAK                 PICTURE S9(8)V9(2).
               10  KRE-IO.
                   15  KRE                 PICTURE S9(8)V9(2).
               10  FAKM-IO.
                   15  FAKM                PICTURE S9(9)V9(2).
               10  KREM-IO.
                   15  KREM                PICTURE S9(8)V9(2).
               10  FAKLM-IO.
                   15  FAKLM               PICTURE S9(9)V9(2).
               10  KRELM-IO.
                   15  KRELM               PICTURE S9(8)V9(2).
               10  VGRKEY                  PICTURE X(8).
               10  HBKTO                   PICTURE X(4).
               10  FAKP-IO.
                   15  FAKP                PICTURE S9(8)V9(2).
               10  KREP-IO.
                   15  KREP                PICTURE S9(8)V9(2).
               10  FAKT-IO.
                   15  FAKT                PICTURE S9(8)V9(2).
               10  KRET-IO.
                   15  KRET                PICTURE S9(8)V9(2).
               10  FAKMT-IO.
                   15  FAKMT               PICTURE S9(8)V9(2).
               10  KREMT-IO.
                   15  KREMT               PICTURE S9(8)V9(2).
               10  FAKGP-IO.
                   15  FAKGP               PICTURE S9(9)V9(2).
               10  KREGP-IO.
                   15  KREGP               PICTURE S9(8)V9(2).
               10  FAKGM-IO.
                   15  FAKGM               PICTURE S9(9)V9(2).
               10  KREGM-IO.
                   15  KREGM               PICTURE S9(8)V9(2).
               10  FAKGPF-IO.
                   15  FAKGPF              PICTURE S9(8)V9(2).
               10  KREGPF-IO.
                   15  KREGPF              PICTURE S9(8)V9(2).
               10  FAKGMF-IO.
                   15  FAKGMF              PICTURE S9(8)V9(2).
               10  KREGMF-IO.
                   15  KREGMF              PICTURE S9(8)V9(2).
               10  FAKGM8-IO.
                   15  FAKGM8              PICTURE S9(9)V9(2).
               10  KREGM8-IO.
                   15  KREGM8              PICTURE S9(8)V9(2).
               10  FAKLP-IO.
                   15  FAKLP               PICTURE S9(9)V9(2).
               10  KRELP-IO.
                   15  KRELP               PICTURE S9(8)V9(2).
               10  SUM11-IO.
                   15  SUM11               PICTURE S9(8)V9(2).
               10  SUML3-IO.
                   15  SUML3               PICTURE S9(8)V9(2).
               10  SUML38-IO.
                   15  SUML38              PICTURE S9(8)V9(2).
               10  SUM500-IO.
                   15  SUM500              PICTURE S9(8)V9(2).
           05  EDITTING-FIELDS.
               10  XO-72YY9R               PICTURE Z.ZZZ.ZZZ,99-.
               10  XO-82YY9R               PICTURE ZZ.ZZZ.ZZZ,99-.
               10  XO-92YY9R               PICTURE ZZZ.ZZZ.ZZZ,99-.
               10  XO-50YY9                PICTURE ZZ.ZZ9.
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
           SET NOT-I-02                    TO TRUE
           SET NOT-I-01                    TO TRUE
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
           IF  INFILE-PROCESS
               SET INFILE-PROCESS-OFF      TO TRUE
               SET INFILE-READ             TO TRUE
           END-IF
 
           IF  INFILE-READ
               PERFORM INFILE-GET
               SET INFILE-READ-OFF         TO TRUE
               IF  NOT INFILE-EOF
                   PERFORM INFILE-MATCH-SET
               END-IF
           END-IF
 
           IF  FAKPAR-PROCESS
               SET FAKPAR-PROCESS-OFF      TO TRUE
               SET FAKPAR-READ             TO TRUE
           END-IF
 
           IF  FAKPAR-READ
           AND RECORD-SELECTED-OFF
               PERFORM FAKPAR-GET
               SET FAKPAR-READ-OFF         TO TRUE
               IF  NOT FAKPAR-EOF
                   SET FAKPAR-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  STATREC-PROCESS
               SET STATREC-PROCESS-OFF     TO TRUE
               SET STATREC-READ            TO TRUE
           END-IF
 
           IF  STATREC-READ
               PERFORM STATREC-GET
               SET STATREC-READ-OFF        TO TRUE
               IF  NOT STATREC-EOF
                   PERFORM STATREC-MATCH-SET
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
 
           IF  INFILE-PROCESS
               PERFORM INFILE-IDSET
           END-IF
 
           IF  FAKPAR-PROCESS
               PERFORM FAKPAR-IDSET
           END-IF
 
           IF  STATREC-PROCESS
               PERFORM STATREC-IDSET
           END-IF
 
           IF  INFILE-PROCESS
               PERFORM INFILE-CHK-LEVEL
           END-IF
 
           IF  STATREC-PROCESS
               PERFORM STATREC-CHK-LEVEL
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
 
           IF  INFILE-PROCESS
               PERFORM INFILE-FLDSET
           END-IF
 
           IF  FAKPAR-PROCESS
               PERFORM FAKPAR-FLDSET
           END-IF
 
           IF  STATREC-PROCESS
               PERFORM STATREC-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  INFILE-PROCESS
           OR  STATREC-PROCESS
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
               SET NOT-I-50                TO TRUE
      *****************************************************************
      * NULLSTILLE ARRAYS VED NYTT FIRMA.                             *
      *****************************************************************
           END-IF
           IF  (NOT-I-L3)
               GO TO ENDAR0-T
           END-IF
           IF  (I-L3)
               MOVE 0,00                   TO VSFL3
               MOVE 0,00                   TO VSPL3
               MOVE 0                      TO X
               PERFORM VARYING ARK-I FROM 1 BY 1
                         UNTIL ARK-I > ARK-MAX
                   MOVE '    '             TO ARK (ARK-I)
               END-PERFORM
               PERFORM VARYING ARP-I FROM 1 BY 1
                         UNTIL ARP-I > ARP-MAX
                   MOVE 0,00               TO ARP (ARP-I)
               END-PERFORM
               SET ARP-I                   TO 1
               PERFORM VARYING ARF-I FROM 1 BY 1
                         UNTIL ARF-I > ARF-MAX
                   MOVE 0,00               TO ARF (ARF-I)
               END-PERFORM
               SET ARF-I                   TO 1
           END-IF.
 
       ENDAR0-T.
      *****************************************************************
           IF  (I-L1)
               SET NOT-I-88                TO TRUE
               MOVE 0                      TO FAKM
               MOVE 0                      TO KREM
           END-IF
           IF  (I-L2)
               MOVE 0                      TO FAKT
               MOVE 0                      TO KRET
           END-IF
           IF  (I-L1)
               MOVE 0                      TO FAKP
               MOVE 0                      TO KREP
           END-IF
           IF  (I-L2)
               MOVE 0                      TO FAKMT
               MOVE 0                      TO KREMT
           END-IF
           IF  (I-L3)
               MOVE 0                      TO FAKGP
               MOVE 0                      TO KREGP
               MOVE 0                      TO FAKGM
               MOVE 0                      TO KREGM
               MOVE 0                      TO FAKGPF
               MOVE 0                      TO KREGPF
               MOVE 0                      TO FAKGMF
               MOVE 0                      TO KREGMF
               MOVE 0                      TO FAKGM8
               MOVE 0                      TO KREGM8
               PERFORM RBSRUT-S
      ***  LESER REGNSKAPSPARAMETER OG SJEKKER OM ALT.KTO BENYTTES  ***
           END-IF
           IF  (I-L3)
               SET NOT-I-40                TO TRUE
               SET NOT-I-41                TO TRUE
               SET NOT-I-42                TO TRUE
               MOVE FIRMA                  TO SYSKEY (1:3)
               MOVE 'REGA011'              TO SYSKEY (4:7)
               MOVE SYSKEY                 TO SYSPARM-KEY1
               READ SYSPARM RECORD KEY IS SYSPARM-KEY1
               INVALID KEY
                   SET I-40                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-40            TO TRUE
                   PERFORM SYSPARM-FLDSET
                   PERFORM SYSPARM-IDSET
               END-READ
           END-IF
           IF  (I-L3 AND NOT-I-40)
               SET NOT-I-41                TO TRUE
               IF  SYSALT = 'J'
                   SET I-41                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND I-41)
               MOVE FIRMA                  TO KTOKEY (1:3)
               MOVE SALPLI                 TO KTOKEY (4:4)
               MOVE KTOKEY                 TO KONTOMA-KEY1
               READ KONTOMA RECORD KEY IS KONTOMA-KEY1
               INVALID KEY
                   SET I-42                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-42            TO TRUE
                   PERFORM KONTOMA-FLDSET
                   PERFORM KONTOMA-IDSET
               END-READ
           END-IF
           IF  (I-L3 AND I-41 AND NOT-I-42)
               MOVE ALTKTO                 TO ALTSAL
           END-IF
           IF  (I-L3 AND I-41 AND I-42)
               MOVE '????'                 TO ALTSAL
           END-IF
           IF  (I-L3 AND I-41)
               MOVE SALFRI                 TO KTOKEY (4:4)
               MOVE KTOKEY                 TO KONTOMA-KEY1
               READ KONTOMA RECORD KEY IS KONTOMA-KEY1
               INVALID KEY
                   SET I-42                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-42            TO TRUE
                   PERFORM KONTOMA-FLDSET
                   PERFORM KONTOMA-IDSET
               END-READ
           END-IF
           IF  (I-L3 AND I-41 AND NOT-I-42)
               MOVE ALTKTO                 TO ALTFRI
           END-IF
           IF  (I-L3 AND I-41 AND I-42)
               MOVE '????'                 TO ALTFRI
           END-IF
           IF  (I-01)
               GO TO SLUTT-T
           END-IF
           IF  (I-03)
               SET I-50                    TO TRUE
               GO TO SLUTT-T
           END-IF
           IF  (I-02)
               MOVE 0                      TO FAK
               MOVE 0                      TO KRE
               SET NOT-I-30                TO TRUE
               IF  TYPE-X NOT = '3'
                   SET I-30                TO TRUE
               END-IF
               SET NOT-I-31                TO TRUE
               IF  TYPE-X = '3'
                   SET I-31                TO TRUE
               END-IF
               SET NOT-I-33                TO TRUE
               IF  SIGN-X = '-'
                   SET I-33                TO TRUE
               END-IF
               SET NOT-I-34                TO TRUE
               IF  AVGKOD = '4'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-30 AND NOT-I-33)
               ADD BEL TO ZERO         GIVING FAK
           END-IF
           IF  (I-02 AND I-30 AND I-33)
               SUBTRACT BEL FROM ZERO  GIVING FAK
           END-IF
           IF  (I-02 AND I-31 AND NOT-I-33)
               ADD BEL TO ZERO         GIVING KRE
           END-IF
           IF  (I-02 AND I-31 AND I-33)
               SUBTRACT BEL FROM ZERO  GIVING KRE
           END-IF
           IF  (I-02)
               ADD FAK                     TO FAKM
               ADD KRE                     TO KREM
               ADD FAK                     TO FAKLM
               ADD KRE                     TO KRELM
           END-IF
           IF  (I-L1)
               MOVE FIRMA                  TO VGRKEY (1:3)
               MOVE VGR                    TO VGRKEY (4:5)
               MOVE VGRKEY                 TO VAGRMAS-KEY1
               READ VAGRMAS RECORD KEY IS VAGRMAS-KEY1
               INVALID KEY
                   SET I-21                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-21            TO TRUE
                   PERFORM VAGRMAS-FLDOFF
                   PERFORM VAGRMAS-FLDSET
                   PERFORM VAGRMAS-IDSET
               END-READ
           END-IF
           IF  (I-L1 AND NOT-I-21 AND NOT-I-08)
               SET I-88                    TO TRUE
               MOVE TILKTO                 TO HBKTO
      ***  HENTER ALTERNATIVT KONTONR FRA KONTOMA HVIS DET BENYTTES  ***
           END-IF
           IF  (I-L1)
               SET NOT-I-43                TO TRUE
               SET NOT-I-44                TO TRUE
           END-IF
           IF  (I-L1 AND I-41 AND NOT-I-21)
               AND (I-88 AND I-41 AND NOT-I-21)
               MOVE FIRMA                  TO KTOKEY (1:3)
               MOVE TILKTO                 TO KTOKEY (4:4)
               MOVE KTOKEY                 TO KONTOMA-KEY1
               READ KONTOMA RECORD KEY IS KONTOMA-KEY1
               INVALID KEY
                   SET I-43                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-43            TO TRUE
                   PERFORM KONTOMA-FLDSET
                   PERFORM KONTOMA-IDSET
               END-READ
           END-IF
           IF  (I-L1 AND I-41 AND NOT-I-21)
               AND (I-88 AND NOT-I-43)
               SET I-44                    TO TRUE
           END-IF
           IF  (I-L1 AND I-41 AND I-44)
               MOVE ALTKTO                 TO HBKTO
           END-IF
           IF  (I-02)
               SET NOT-I-12                TO TRUE
               SET NOT-I-12                TO TRUE
               IF  FAKREC = '*'
                   SET I-12                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-12)
               ADD FAK                     TO FAKP
               ADD KRE                     TO KREP
               ADD FAK                     TO FAKT
               ADD KRE                     TO KRET
           END-IF
           IF  (I-02)
               ADD FAK                     TO FAKMT
               ADD KRE                     TO KREMT
           END-IF
           IF  (I-02 AND I-12)
               ADD FAK                     TO FAKGP
               ADD KRE                     TO KREGP
           END-IF
           IF  (I-02)
               ADD FAK                     TO FAKGM
               ADD KRE                     TO KREGM
           END-IF
           IF  (I-02 AND I-12 AND I-34)
               ADD FAK                     TO FAKGPF
               ADD KRE                     TO KREGPF
           END-IF
           IF  (I-02 AND I-34)
               ADD FAK                     TO FAKGMF
               ADD KRE                     TO KREGMF
           END-IF
           IF  (I-02 AND I-88)
               ADD FAK                     TO FAKGM8
               ADD KRE                     TO KREGM8
      * * *  GRANDTOTALER.   * * *
           END-IF
           IF  (I-02 AND I-12)
               ADD FAK                     TO FAKLP
               ADD KRE                     TO KRELP
      *****************************************************************
      * RUTINE FOR SUMMERING PR. HOVEDBOKSKONTO.                      *
      *****************************************************************
           END-IF
           IF  (I-02)
               ADD KRE TO FAK          GIVING SUM11
           END-IF
           IF  (I-02 AND NOT-I-88 AND I-34)
               ADD SUM11                   TO VSFL3
           END-IF
           IF  (I-02 AND NOT-I-88 AND NOT-I-34)
               ADD SUM11                   TO VSPL3
           END-IF
           IF  (I-02 AND NOT-I-88)
               GO TO ENDAR1-T
           END-IF
           IF  (I-02)
               MOVE 0                      TO X
           END-IF.
 
       BEGAR1-T.
           IF  (I-02)
               ADD 1                       TO X
               SET NOT-I-11                TO TRUE
               IF  X > 30
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-11)
               GO TO ENDAR1-T
           END-IF
           IF  (I-02)
               SET NOT-I-12                TO TRUE
               IF  HBKTO = ARK (X)
                   SET I-12                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-12)
               SET NOT-I-13                TO TRUE
               IF  ARK (X) = '    '
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-12 AND NOT-I-13)
               GO TO BEGAR1-T
           END-IF
           IF  (I-02 AND NOT-I-12 AND I-13)
               MOVE HBKTO                  TO ARK (X)
               MOVE TILKTO                 TO ARV (X)
           END-IF
           IF  (I-02 AND NOT-I-34)
               ADD SUM11                   TO ARP (X)
           END-IF
           IF  (I-02 AND I-34)
               ADD SUM11                   TO ARF (X)
           END-IF.
 
       ENDAR1-T.
      *****************************************************************
           CONTINUE.
 
       SLUTT-T.
           CONTINUE.
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE ' '                        TO BBEST
           MOVE 'FAK02'                    TO LONR
           MOVE FIRMA                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'FAK170  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF.
      ******************************************************
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1)
               SET I-17                    TO TRUE
           END-IF
           IF  (I-L1 AND NOT-I-86)
               PERFORM EXCEPTION-OUTPUT
           END-IF
           IF  (I-L1)
               SET NOT-I-17                TO TRUE
      *****************************************************************
      * RUTINE FOR PRINING AV TOTALSUMMER PR AVD.                     *
      *****************************************************************
           END-IF
           IF  (I-L2)
               SET I-16                    TO TRUE
           END-IF
           IF  (I-L2 AND NOT-I-86)
               PERFORM EXCEPTION-OUTPUT
           END-IF
           IF  (I-L2)
               SET NOT-I-16                TO TRUE
      *****************************************************************
      * RUTINE FOR Å REGNE UT TOTALSUM KONTO 5000 I HOVEDBOK.         *
      *****************************************************************
           END-IF
           IF  (I-L3)
               ADD KREGM TO FAKGM      GIVING SUML3
               ADD KREGM8 TO FAKGM8    GIVING SUML38
               SUBTRACT SUML38 FROM SUML3 GIVING SUM500
      *****************************************************************
      * RUTINE FOR PRINING AV SUMMER PR. HOVEDBOKSKONTO.              *
      *****************************************************************
           END-IF
           IF  (I-L3)
               SET I-14                    TO TRUE
           END-IF
           IF  (I-L3 AND I-14 AND NOT-I-86)
               PERFORM EXCEPTION-OUTPUT
           END-IF
           IF  (I-L3)
               SET NOT-I-14                TO TRUE
               MOVE 0                      TO X
           END-IF.
 
       BEGAR9-T.
           IF  (I-L3)
               ADD 1                       TO X
               SET NOT-I-11                TO TRUE
               IF  X > 30
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND I-11)
               GO TO ENDAR9-T
           END-IF
           IF  (I-L3)
               SET NOT-I-13                TO TRUE
               IF  ARK (X) = '    '
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND I-13)
               GO TO ENDAR9-T
           END-IF
           IF  (I-L3)
               MOVE FIRMA                  TO KTOKEY (1:3)
               MOVE ARV (X)                TO KTOKEY (4:4)
               MOVE KTOKEY                 TO KONTOMA-KEY1
               READ KONTOMA RECORD KEY IS KONTOMA-KEY1
               INVALID KEY
                   SET I-43                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-43            TO TRUE
                   PERFORM KONTOMA-FLDSET
                   PERFORM KONTOMA-IDSET
               END-READ
               SET I-15                    TO TRUE
           END-IF
           IF  (I-L3 AND I-15 AND NOT-I-86)
               PERFORM EXCEPTION-OUTPUT
           END-IF
           IF  (I-L3)
               SET NOT-I-15                TO TRUE
               GO TO BEGAR9-T
           END-IF.
 
       ENDAR9-T.
           IF  (I-L3)
               SET I-19                    TO TRUE
           END-IF
           IF  (I-L3 AND I-19 AND NOT-I-86)
               PERFORM EXCEPTION-OUTPUT
           END-IF
           IF  (I-L3)
               SET NOT-I-19                TO TRUE
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           END-IF
           .
 
       INFILE-GET SECTION.
       INFILE-GET-P.
           IF  INFILE-EOF-OFF
               READ INFILE
               AT END
                   SET INFILE-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INFILE-FLDSET SECTION.
       INFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INFILE-IO-AREA (1:3)   TO FIRMA (1:3)
               MOVE INFILE-IO-AREA (4:5)   TO VGR (1:5)
               MOVE INFILE-IO-AREA (5:4)   TO VNR (1:4)
               MOVE INFILE-IO-AREA (4:1)   TO AVD (1:1)
               MOVE INFILE-IO-AREA (9:6)   TO BILDAT (1:6)
               MOVE INFILE-IO-AREA (11:2)  TO BILMND (1:2)
               MOVE INFILE-IO-AREA (15:5)  TO BEL-IO
               MOVE INFILE-IO-AREA (20:1)  TO SIGN-X (1:1)
               MOVE INFILE-IO-AREA (21:1)  TO TYPE-X (1:1)
               MOVE INFILE-IO-AREA (23:1)  TO FAKREC (1:1)
               MOVE INFILE-IO-AREA (40:1)  TO AVGKOD (1:1)
           END-EVALUATE.
 
       INFILE-IDSET SECTION.
       INFILE-IDSET-P.
           SET I-02                        TO TRUE.
 
       INFILE-CHK-LEVEL SECTION.
       INFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO INFILE-LEVEL-02
               MOVE INFILE-IO-AREA (1:3)   TO INFILE-02-L3-FIRMA
               MOVE INFILE-IO-AREA (4:1)   TO INFILE-02-L2-AVD
               MOVE INFILE-IO-AREA (5:4)   TO INFILE-02-L1-VNR
               IF  INFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INFILE-02-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  INFILE-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INFILE-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INFILE-02-L3          TO THE-PRIOR-L3
               MOVE  INFILE-02-L2          TO THE-PRIOR-L2
               MOVE  INFILE-02-L1          TO THE-PRIOR-L1
               SET INFILE-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       INFILE-MATCH-SET SECTION.
       INFILE-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE INFILE-IO-AREA (1:3)   TO INFILE-M-02-M1-FIRMA
           END-EVALUATE.
 
       FAKPAR-GET SECTION.
       FAKPAR-GET-P.
           IF  FAKPAR-EOF-OFF
               READ FAKPAR
               AT END
                   SET FAKPAR-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FAKPAR-FLDSET SECTION.
       FAKPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKPAR-IO-AREA (12:6)  TO ODATO-IO
               INSPECT ODATO-IO REPLACING ALL ' ' BY '0'
               MOVE FAKPAR-IO-AREA (18:6)  TO PFADTO (1:6)
               MOVE FAKPAR-IO-AREA (74:9)  TO MND (1:9)
           END-EVALUATE.
 
       FAKPAR-IDSET SECTION.
       FAKPAR-IDSET-P.
           SET I-01                        TO TRUE.
 
       STATREC-GET SECTION.
       STATREC-GET-P.
           IF  STATREC-EOF-OFF
               READ STATREC
               AT END
                   SET STATREC-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       STATREC-FLDSET SECTION.
       STATREC-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE STATREC-IO-AREA (1:3)  TO FIRMA (1:3)
               MOVE STATREC-IO-AREA (4:3)  TO ANTO2-IO
               MOVE STATREC-IO-AREA (7:3)  TO ANTO3-IO
               MOVE STATREC-IO-AREA (10:3) TO ANTO4-IO
               MOVE STATREC-IO-AREA (13:3) TO ANTO5-IO
               MOVE STATREC-IO-AREA (16:3) TO ANTL2-IO
               MOVE STATREC-IO-AREA (19:3) TO ANTL3-IO
               MOVE STATREC-IO-AREA (22:3) TO ANTL4-IO
               MOVE STATREC-IO-AREA (25:3) TO ANTL5-IO
               MOVE STATREC-IO-AREA (28:5) TO SUMB2-IO
               MOVE STATREC-IO-AREA (33:5) TO SUMB3-IO
               MOVE STATREC-IO-AREA (38:5) TO SUMB4-IO
               MOVE STATREC-IO-AREA (43:5) TO SUMB5-IO
           END-EVALUATE.
 
       STATREC-IDSET SECTION.
       STATREC-IDSET-P.
           SET I-03                        TO TRUE.
 
       STATREC-CHK-LEVEL SECTION.
       STATREC-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO STATREC-LEVEL-03
               MOVE STATREC-IO-AREA (1:3)  TO STATREC-03-L3-FIRMA
               IF  STATREC-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  STATREC-03-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   END-EVALUATE
               END-IF
               MOVE  STATREC-03-L3         TO THE-PRIOR-L3
               SET STATREC-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       STATREC-MATCH-SET SECTION.
       STATREC-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE STATREC-IO-AREA (1:3)  TO STATREC-M-03-M1-FIRMA
           END-EVALUATE.
 
       VAGRMAS-FLDOFF SECTION.
       VAGRMAS-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-08                TO TRUE
           END-EVALUATE.
 
       VAGRMAS-FLDSET SECTION.
       VAGRMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAGRMAS-IO-AREA (11:40) TO VGRNA (1:40)
               MOVE VAGRMAS-IO-AREA (69:4) TO TILKTO (1:4)
               IF  TILKTO = SPACES
                   SET I-08                TO TRUE
               END-IF
           END-EVALUATE.
 
       VAGRMAS-IDSET SECTION.
       VAGRMAS-IDSET-P.
           SET I-04                        TO TRUE.
 
       SYSPARM-FLDSET SECTION.
       SYSPARM-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE SYSPARM-IO-AREA (026:1) TO SYSALT (1:1)
               MOVE SYSPARM-IO-AREA (037:4) TO SALPLI (1:4)
               MOVE SYSPARM-IO-AREA (041:4) TO SALFRI (1:4)
           END-EVALUATE.
 
       SYSPARM-IDSET SECTION.
       SYSPARM-IDSET-P.
           SET I-05                        TO TRUE.
 
       KONTOMA-FLDSET SECTION.
       KONTOMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KONTOMA-IO-AREA (45:4) TO ALTKTO (1:4)
               MOVE KONTOMA-IO-AREA (10:35) TO KTONAV (1:35)
           END-EVALUATE.
 
       KONTOMA-IDSET SECTION.
       KONTOMA-IDSET-P.
           SET I-06                        TO TRUE.
 
       STAT-PRINT-LINE SECTION.
       STAT-PRINT-LINE-P.
           IF  STAT-BEFORE-SKIP > 0
               PERFORM STAT-SKIP-BEFORE
           END-IF
           IF  STAT-BEFORE-SPACE > 0
               PERFORM STAT-SPACE-BEFORE
               IF  STAT-AFTER-SKIP > 0
                   PERFORM STAT-SKIP-AFTER
               END-IF
               IF  STAT-AFTER-SPACE > 0
                   PERFORM STAT-SPACE-AFTER
               END-IF
           ELSE
               IF  STAT-AFTER-SKIP > 0
                   PERFORM STAT-SKIP-AFTER
               END-IF
               PERFORM STAT-SPACE-AFTER
           END-IF
           IF  STAT-LINE-COUNT NOT < STAT-MAX-LINES
               SET I-OF                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OF            TO TRUE
               END-IF
           END-IF.
 
       STAT-SKIP-BEFORE SECTION.
       STAT-SKIP-BEFORE-P.
           WRITE STAT-IO-PRINT          AFTER ADVANCING PAGE
           MOVE 1                          TO STAT-LINE-COUNT
           MOVE 0                          TO STAT-BEFORE-SKIP
           INITIALIZE STAT-IO-AREA.
 
       STAT-SPACE-BEFORE SECTION.
       STAT-SPACE-BEFORE-P.
           WRITE STAT-IO-PRINT          AFTER STAT-BEFORE-SPACE LINES
           ADD STAT-BEFORE-SPACE           TO STAT-LINE-COUNT
           MOVE SPACES TO STAT-IO-AREA
           INITIALIZE STAT-IO-AREA
           MOVE 0                          TO STAT-BEFORE-SPACE.
 
       STAT-SKIP-AFTER SECTION.
       STAT-SKIP-AFTER-P.
           WRITE STAT-IO-PRINT         BEFORE ADVANCING PAGE
           MOVE 1                          TO STAT-LINE-COUNT
           MOVE 0                          TO STAT-AFTER-SKIP
           INITIALIZE STAT-IO-AREA.
 
       STAT-SPACE-AFTER SECTION.
       STAT-SPACE-AFTER-P.
           WRITE STAT-IO-PRINT         BEFORE STAT-AFTER-SPACE LINES
           ADD STAT-AFTER-SPACE            TO STAT-LINE-COUNT
           INITIALIZE STAT-IO-AREA
           MOVE 0                          TO STAT-AFTER-SPACE.
 
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
               MOVE 7                      TO LISTE-AFTER-SKIP
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
           IF  INFILE-EOF
               MOVE HIGH-VALUES            TO INFILE-MC
                                              INFILE-MP
           END-IF
           IF  STATREC-EOF
               MOVE HIGH-VALUES            TO STATREC-MC
                                              STATREC-MP
           END-IF
           IF  INFILE-MC < INFILE-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  STATREC-MC < STATREC-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  INFILE-MC < STATREC-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET INFILE-PROCESS      TO TRUE
                   MOVE INFILE-MC          TO INFILE-MP
                   IF  INFILE-MC = STATREC-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  STATREC-MC < INFILE-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET STATREC-PROCESS     TO TRUE
                   MOVE STATREC-MC         TO STATREC-MP
                   IF  STATREC-MC = INFILE-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  INFILE-MC = STATREC-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET INFILE-PROCESS      TO TRUE
                   MOVE INFILE-MC          TO INFILE-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND I-88 AND I-U1)
               MOVE SPACES TO STAT-IO-AREA
               INITIALIZE STAT-IO-AREA
               MOVE VGR                    TO STAT-IO-AREA (2:5)
               MOVE '= KONTO '             TO STAT-IO-AREA (8:8)
               MOVE HBKTO                  TO STAT-IO-AREA (16:4)
               MOVE BILDAT                 TO STAT-IO-AREA (21:6)
               MOVE TYPE-X                 TO STAT-IO-AREA (28:1)
               MOVE SIGN-X                 TO STAT-IO-AREA (30:1)
               MOVE FAKREC                 TO STAT-IO-AREA (32:1)
               MOVE BEL                    TO XO-72YY9R
               MOVE XO-72YY9R              TO STAT-IO-AREA (50:13)
               MOVE 1                      TO STAT-AFTER-SPACE
               PERFORM STAT-PRINT-LINE
           END-IF
           IF  (I-U6 AND I-U7 AND I-U8)
           AND (I-04 AND I-05 AND I-06)
           AND (I-98 AND I-05 AND I-06)
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
               MOVE LPRIID                 TO LISTE-IO-AREA (117:4)
               MOVE BBEST                  TO LISTE-IO-AREA (120:1)
               MOVE PSDS                   TO LISTE-IO-AREA (41:80)
               MOVE R                      TO LISTE-IO-AREA (113:8)
               MOVE P-IO                   TO LISTE-IO-AREA (118:3)
               MOVE S-IO                   TO LISTE-IO-AREA (116:5)
               MOVE FINAVN                 TO LISTE-IO-AREA (91:30)
               MOVE LOPNVN                 TO LISTE-IO-AREA (86:35)
               MOVE BJOBN                  TO LISTE-IO-AREA (113:8)
               MOVE BPERS                  TO LISTE-IO-AREA (91:30)
               MOVE BETTB                  TO LISTE-IO-AREA (81:40)
               MOVE BFORS                  TO LISTE-IO-AREA (81:40)
               MOVE BFORS                  TO LISTE-IO-AREA (81:40)
               MOVE BMEMO                  TO LISTE-IO-AREA (81:40)
               MOVE BANTX-IO               TO LISTE-IO-AREA (118:3)
               MOVE PFADTO                 TO LISTE-IO-AREA (115:6)
               MOVE BILDAT                 TO LISTE-IO-AREA (115:6)
               MOVE BILMND                 TO LISTE-IO-AREA (119:2)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       EXCEPTION-OUTPUT SECTION.
       EXCEPTION-OUTPUT-P.
           IF  (I-17 AND NOT-I-86)
               MOVE SPACES TO STAT-IO-AREA
               INITIALIZE STAT-IO-AREA
               MOVE VGR                    TO STAT-IO-AREA (2:5)
               IF  (NOT-I-21)
                   MOVE VGRNA              TO STAT-IO-AREA (8:40)
               END-IF
               MOVE FAKP                   TO XO-82YY9R
               MOVE XO-82YY9R              TO STAT-IO-AREA (49:14)
               INITIALIZE FAKP
               MOVE KREP                   TO XO-82YY9R
               MOVE XO-82YY9R              TO STAT-IO-AREA (67:14)
               INITIALIZE KREP
               MOVE FAKM                   TO XO-92YY9R
               MOVE XO-92YY9R              TO STAT-IO-AREA (88:15)
               INITIALIZE FAKM
               MOVE KREM                   TO XO-82YY9R
               MOVE XO-82YY9R              TO STAT-IO-AREA (107:14)
               INITIALIZE KREM
               MOVE TILKTO                 TO STAT-IO-AREA (122:4)
               IF  (I-41 AND NOT-I-44)
                   MOVE '????'             TO STAT-IO-AREA (122:4)
               END-IF
               IF  (I-41 AND I-44)
                   MOVE ALTKTO             TO STAT-IO-AREA (122:4)
               END-IF
               IF  (I-41 AND NOT-I-88)
                   MOVE '    '             TO STAT-IO-AREA (122:4)
               END-IF
               MOVE 1                      TO STAT-AFTER-SPACE
               PERFORM STAT-PRINT-LINE
           END-IF
           IF  (I-16 AND NOT-I-86)
               MOVE SPACES TO STAT-IO-AREA
               INITIALIZE STAT-IO-AREA
               MOVE 'TOTAL'                TO STAT-IO-AREA (2:5)
               MOVE FAKT                   TO XO-82YY9R
               MOVE XO-82YY9R              TO STAT-IO-AREA (49:14)
               INITIALIZE FAKT
               MOVE KRET                   TO XO-82YY9R
               MOVE XO-82YY9R              TO STAT-IO-AREA (67:14)
               INITIALIZE KRET
               MOVE FAKMT                  TO XO-82YY9R
               MOVE XO-82YY9R              TO STAT-IO-AREA (89:14)
               INITIALIZE FAKMT
               MOVE KREMT                  TO XO-82YY9R
               MOVE XO-82YY9R              TO STAT-IO-AREA (107:14)
               INITIALIZE KREMT
               MOVE 2                      TO STAT-BEFORE-SPACE
               PERFORM STAT-PRINT-LINE
           END-IF
           IF  (I-14 AND NOT-I-86)
               MOVE SPACES TO STAT-IO-AREA
               INITIALIZE STAT-IO-AREA
               MOVE FINAVN                 TO STAT-IO-AREA (1:30)
               MOVE LOPNVN                 TO STAT-IO-AREA (32:35)
               MOVE 'TOTALSUMMER PR. KONTI ' TO STAT-IO-AREA (65:22)
               MOVE MND                    TO STAT-IO-AREA (90:9)
               MOVE 'SISTE O.DATO'         TO STAT-IO-AREA (100:12)
               MOVE ODATO                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO STAT-IO-AREA (113:8)
               MOVE 01                     TO STAT-BEFORE-SKIP
               MOVE 1                      TO STAT-AFTER-SPACE
               PERFORM STAT-PRINT-LINE
               MOVE SPACES TO STAT-IO-AREA
               INITIALIZE STAT-IO-AREA
               MOVE '------------------------' TO STAT-IO-AREA (1:24)
               MOVE '------------------------' TO STAT-IO-AREA (25:24)
               MOVE '------------------------' TO STAT-IO-AREA (49:24)
               MOVE '------------------------' TO STAT-IO-AREA (73:24)
               MOVE '------------------------' TO STAT-IO-AREA (97:24)
               MOVE 1                      TO STAT-AFTER-SPACE
               PERFORM STAT-PRINT-LINE
      *                                  74 "DENNE  FAKTURAOMGANG"
               MOVE SPACES TO STAT-IO-AREA
               INITIALIZE STAT-IO-AREA
               MOVE 'HITTIL DENNE MÅNED'   TO STAT-IO-AREA (95:18)
               MOVE 1                      TO STAT-AFTER-SPACE
               PERFORM STAT-PRINT-LINE
               MOVE SPACES TO STAT-IO-AREA
               INITIALIZE STAT-IO-AREA
               MOVE 'HOVEDBOKSKONTO'       TO STAT-IO-AREA (21:14)
               MOVE 'SUM AVG.PLIKTIG'      TO STAT-IO-AREA (87:15)
               MOVE 'SUM AVG.FRITT'        TO STAT-IO-AREA (107:13)
               MOVE 1                      TO STAT-AFTER-SPACE
               PERFORM STAT-PRINT-LINE
               MOVE SPACES TO STAT-IO-AREA
               INITIALIZE STAT-IO-AREA
               MOVE '------------------------' TO STAT-IO-AREA (1:24)
               MOVE '------------------------' TO STAT-IO-AREA (25:24)
               MOVE '------------------------' TO STAT-IO-AREA (49:24)
               MOVE '------------------------' TO STAT-IO-AREA (73:24)
               MOVE '------------------------' TO STAT-IO-AREA (97:24)
               MOVE 2                      TO STAT-AFTER-SPACE
               PERFORM STAT-PRINT-LINE
           END-IF
           IF  (I-15 AND NOT-I-86)
               MOVE SPACES TO STAT-IO-AREA
               INITIALIZE STAT-IO-AREA
               MOVE '    TOTALT'           TO STAT-IO-AREA (2:10)
               MOVE 'HOVEDBOKSKONTO   '    TO STAT-IO-AREA (13:17)
               MOVE ARK (X)                TO STAT-IO-AREA (31:4)
               IF  (NOT-I-43)
                   MOVE KTONAV             TO STAT-IO-AREA (37:35)
               END-IF
               MOVE ARP (X)                TO XO-82YY9R
               MOVE XO-82YY9R              TO STAT-IO-AREA (89:14)
               MOVE ARF (X)                TO XO-82YY9R
               MOVE XO-82YY9R              TO STAT-IO-AREA (107:14)
               MOVE 1                      TO STAT-AFTER-SPACE
               PERFORM STAT-PRINT-LINE
           END-IF
           IF  (I-19 AND NOT-I-86)
               MOVE SPACES TO STAT-IO-AREA
               INITIALIZE STAT-IO-AREA
               MOVE '    TOTALT'           TO STAT-IO-AREA (2:10)
               MOVE ' NORMALT VARESALG'    TO STAT-IO-AREA (12:17)
               MOVE 'KTO.'                 TO STAT-IO-AREA (33:4)
               IF  (NOT-I-41)
                   MOVE SALPLI             TO STAT-IO-AREA (38:4)
               END-IF
               IF  (I-41)
                   MOVE ALTSAL             TO STAT-IO-AREA (38:4)
               END-IF
               MOVE ' OG '                 TO STAT-IO-AREA (42:4)
               IF  (NOT-I-41)
                   MOVE SALFRI             TO STAT-IO-AREA (46:4)
               END-IF
               IF  (I-41)
                   MOVE ALTFRI             TO STAT-IO-AREA (46:4)
               END-IF
               MOVE ' I HOVEDBOK '         TO STAT-IO-AREA (50:12)
               MOVE VSPL3                  TO XO-92YY9R
               MOVE XO-92YY9R              TO STAT-IO-AREA (88:15)
               MOVE VSFL3                  TO XO-92YY9R
               MOVE XO-92YY9R              TO STAT-IO-AREA (106:15)
               MOVE 1                      TO STAT-AFTER-SPACE
               PERFORM STAT-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF)
           OR  (I-L2 AND NOT-I-OF AND NOT-I-86)
               MOVE SPACES TO STAT-IO-AREA
               INITIALIZE STAT-IO-AREA
               MOVE FINAVN                 TO STAT-IO-AREA (1:30)
               MOVE LOPNVN                 TO STAT-IO-AREA (32:35)
               MOVE 'INK. KONT.KASSEOPPGJØR' TO STAT-IO-AREA (65:22)
               MOVE MND                    TO STAT-IO-AREA (90:9)
               MOVE 'SISTE O.DATO'         TO STAT-IO-AREA (100:12)
               MOVE ODATO                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO STAT-IO-AREA (113:8)
               MOVE 01                     TO STAT-BEFORE-SKIP
               MOVE 1                      TO STAT-AFTER-SPACE
               PERFORM STAT-PRINT-LINE
               MOVE SPACES TO STAT-IO-AREA
               INITIALIZE STAT-IO-AREA
               MOVE '------------------------' TO STAT-IO-AREA (1:24)
               MOVE '------------------------' TO STAT-IO-AREA (25:24)
               MOVE '------------------------' TO STAT-IO-AREA (49:24)
               MOVE '------------------------' TO STAT-IO-AREA (73:24)
               MOVE '------------------------' TO STAT-IO-AREA (97:24)
               MOVE '-----'                TO STAT-IO-AREA (121:5)
               MOVE 1                      TO STAT-AFTER-SPACE
               PERFORM STAT-PRINT-LINE
               MOVE SPACES TO STAT-IO-AREA
               INITIALIZE STAT-IO-AREA
               MOVE 'DENNE  FAKTURAOMGANG' TO STAT-IO-AREA (55:20)
               MOVE 'HITTIL DENNE MÅNED'   TO STAT-IO-AREA (95:18)
               MOVE ' TIL '                TO STAT-IO-AREA (121:5)
               MOVE 1                      TO STAT-AFTER-SPACE
               PERFORM STAT-PRINT-LINE
               MOVE SPACES TO STAT-IO-AREA
               INITIALIZE STAT-IO-AREA
               MOVE 'VAREGR VAREGRUPPENAVN' TO STAT-IO-AREA (1:21)
               MOVE 'FAKTURASUM'           TO STAT-IO-AREA (52:10)
               MOVE 'KR.NOTASUM'           TO STAT-IO-AREA (70:10)
               MOVE 'FAKTURASUM'           TO STAT-IO-AREA (92:10)
               MOVE 'KR.NOTASUM'           TO STAT-IO-AREA (110:10)
               MOVE ' KONT'                TO STAT-IO-AREA (121:5)
               MOVE 1                      TO STAT-AFTER-SPACE
               PERFORM STAT-PRINT-LINE
               MOVE SPACES TO STAT-IO-AREA
               INITIALIZE STAT-IO-AREA
               MOVE '------------------------' TO STAT-IO-AREA (1:24)
               MOVE '------------------------' TO STAT-IO-AREA (25:24)
               MOVE '------------------------' TO STAT-IO-AREA (49:24)
               MOVE '------------------------' TO STAT-IO-AREA (73:24)
               MOVE '------------------------' TO STAT-IO-AREA (97:24)
               MOVE '-----'                TO STAT-IO-AREA (121:5)
               MOVE 1                      TO STAT-AFTER-SPACE
               PERFORM STAT-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L3 AND NOT-I-86)
               MOVE SPACES TO STAT-IO-AREA
               INITIALIZE STAT-IO-AREA
               MOVE FINAVN                 TO STAT-IO-AREA (1:30)
               MOVE LOPNVN                 TO STAT-IO-AREA (32:35)
               MOVE 'INK. KONT.KASSEOPPGJØR' TO STAT-IO-AREA (65:22)
               MOVE MND                    TO STAT-IO-AREA (90:9)
               MOVE 'SISTE O.DATO'         TO STAT-IO-AREA (100:12)
               MOVE ODATO                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO STAT-IO-AREA (113:8)
               MOVE 01                     TO STAT-BEFORE-SKIP
               MOVE 1                      TO STAT-AFTER-SPACE
               PERFORM STAT-PRINT-LINE
               MOVE SPACES TO STAT-IO-AREA
               INITIALIZE STAT-IO-AREA
               MOVE '------------------------' TO STAT-IO-AREA (1:24)
               MOVE '------------------------' TO STAT-IO-AREA (25:24)
               MOVE '------------------------' TO STAT-IO-AREA (49:24)
               MOVE '------------------------' TO STAT-IO-AREA (73:24)
               MOVE '------------------------' TO STAT-IO-AREA (97:24)
               MOVE 1                      TO STAT-AFTER-SPACE
               PERFORM STAT-PRINT-LINE
               MOVE SPACES TO STAT-IO-AREA
               INITIALIZE STAT-IO-AREA
               MOVE 'DENNE  FAKTURAOMGANG' TO STAT-IO-AREA (55:20)
               MOVE 'HITTIL DENNE MÅNED'   TO STAT-IO-AREA (95:18)
               MOVE 1                      TO STAT-AFTER-SPACE
               PERFORM STAT-PRINT-LINE
               MOVE SPACES TO STAT-IO-AREA
               INITIALIZE STAT-IO-AREA
               MOVE 'VAREGR VAREGRUPPENAVN' TO STAT-IO-AREA (1:21)
               MOVE 'FAKTURASUM'           TO STAT-IO-AREA (52:10)
               MOVE 'KR.NOTASUM'           TO STAT-IO-AREA (70:10)
               MOVE 'FAKTURASUM'           TO STAT-IO-AREA (92:10)
               MOVE 'KR.NOTASUM'           TO STAT-IO-AREA (110:10)
               MOVE 1                      TO STAT-AFTER-SPACE
               PERFORM STAT-PRINT-LINE
               MOVE SPACES TO STAT-IO-AREA
               INITIALIZE STAT-IO-AREA
               MOVE '------------------------' TO STAT-IO-AREA (1:24)
               MOVE '------------------------' TO STAT-IO-AREA (25:24)
               MOVE '------------------------' TO STAT-IO-AREA (49:24)
               MOVE '------------------------' TO STAT-IO-AREA (73:24)
               MOVE '------------------------' TO STAT-IO-AREA (97:24)
               MOVE 1                      TO STAT-AFTER-SPACE
               PERFORM STAT-PRINT-LINE
               MOVE SPACES TO STAT-IO-AREA
               INITIALIZE STAT-IO-AREA
               MOVE 'FIRMATOTAL'           TO STAT-IO-AREA (2:10)
               MOVE FAKGP                  TO XO-92YY9R
               MOVE XO-92YY9R              TO STAT-IO-AREA (48:15)
               INITIALIZE FAKGP
               MOVE KREGP                  TO XO-82YY9R
               MOVE XO-82YY9R              TO STAT-IO-AREA (67:14)
               INITIALIZE KREGP
               MOVE FAKGM                  TO XO-92YY9R
               MOVE XO-92YY9R              TO STAT-IO-AREA (88:15)
               INITIALIZE FAKGM
               MOVE KREGM                  TO XO-82YY9R
               MOVE XO-82YY9R              TO STAT-IO-AREA (107:14)
               INITIALIZE KREGM
               MOVE 2                      TO STAT-BEFORE-SPACE
               MOVE 2                      TO STAT-AFTER-SPACE
               PERFORM STAT-PRINT-LINE
               MOVE SPACES TO STAT-IO-AREA
               INITIALIZE STAT-IO-AREA
               MOVE '    TOTALT'           TO STAT-IO-AREA (2:10)
               MOVE 'AVGIFTSFRITT SALG.    ' TO STAT-IO-AREA (13:22)
               MOVE FAKGPF                 TO XO-82YY9R
               MOVE XO-82YY9R              TO STAT-IO-AREA (49:14)
               INITIALIZE FAKGPF
               MOVE KREGPF                 TO XO-82YY9R
               MOVE XO-82YY9R              TO STAT-IO-AREA (67:14)
               INITIALIZE KREGPF
               MOVE FAKGMF                 TO XO-82YY9R
               MOVE XO-82YY9R              TO STAT-IO-AREA (89:14)
               INITIALIZE FAKGMF
               MOVE KREGMF                 TO XO-82YY9R
               MOVE XO-82YY9R              TO STAT-IO-AREA (107:14)
               INITIALIZE KREGMF
               MOVE 2                      TO STAT-AFTER-SPACE
               PERFORM STAT-PRINT-LINE
               MOVE SPACES TO STAT-IO-AREA
               INITIALIZE STAT-IO-AREA
               MOVE '    TOTALT'           TO STAT-IO-AREA (2:10)
               MOVE 'FAKT. UTENOM KTO.'    TO STAT-IO-AREA (13:17)
               IF  (NOT-I-41)
                   MOVE SALPLI             TO STAT-IO-AREA (31:4)
               END-IF
               IF  (I-41)
                   MOVE ALTSAL             TO STAT-IO-AREA (31:4)
               END-IF
               MOVE ' OG '                 TO STAT-IO-AREA (35:4)
               IF  (NOT-I-41)
                   MOVE SALFRI             TO STAT-IO-AREA (39:4)
               END-IF
               IF  (I-41)
                   MOVE ALTFRI             TO STAT-IO-AREA (39:4)
               END-IF
               MOVE FAKGM8                 TO XO-92YY9R
               MOVE XO-92YY9R              TO STAT-IO-AREA (88:15)
               INITIALIZE FAKGM8
               MOVE KREGM8                 TO XO-82YY9R
               MOVE XO-82YY9R              TO STAT-IO-AREA (107:14)
               INITIALIZE KREGM8
               MOVE 2                      TO STAT-AFTER-SPACE
               PERFORM STAT-PRINT-LINE
               MOVE SPACES TO STAT-IO-AREA
               INITIALIZE STAT-IO-AREA
               MOVE '    TOTALT'           TO STAT-IO-AREA (2:10)
               MOVE ' NORMALT VARESALG'    TO STAT-IO-AREA (12:17)
               MOVE 'KTO.'                 TO STAT-IO-AREA (33:4)
               IF  (NOT-I-41)
                   MOVE SALPLI             TO STAT-IO-AREA (38:4)
               END-IF
               IF  (I-41)
                   MOVE ALTSAL             TO STAT-IO-AREA (38:4)
               END-IF
               MOVE ' OG '                 TO STAT-IO-AREA (42:4)
               IF  (NOT-I-41)
                   MOVE SALFRI             TO STAT-IO-AREA (46:4)
               END-IF
               IF  (I-41)
                   MOVE ALTFRI             TO STAT-IO-AREA (46:4)
               END-IF
               MOVE ' I HOVEDBOK '         TO STAT-IO-AREA (50:12)
               MOVE SUM500                 TO XO-82YY9R
               MOVE XO-82YY9R              TO STAT-IO-AREA (89:14)
               INITIALIZE SUM500
               MOVE 3                      TO STAT-AFTER-SPACE
               PERFORM STAT-PRINT-LINE
           END-IF
           IF  (I-L3 AND I-50 AND NOT-I-86)
               MOVE SPACES TO STAT-IO-AREA
               INITIALIZE STAT-IO-AREA
               MOVE 'KREDITORDRE FORDELING' TO STAT-IO-AREA (11:21)
               MOVE 'STATISTIKK.'          TO STAT-IO-AREA (33:11)
               MOVE 2                      TO STAT-BEFORE-SPACE
               MOVE 2                      TO STAT-AFTER-SPACE
               PERFORM STAT-PRINT-LINE
               MOVE SPACES TO STAT-IO-AREA
               INITIALIZE STAT-IO-AREA
               MOVE 'OBS. KREDITORDRE I'   TO STAT-IO-AREA (11:18)
               MOVE 'KONTANTSALG SYSTEMET' TO STAT-IO-AREA (30:20)
               MOVE 'BLIR POSTERT TIL'     TO STAT-IO-AREA (51:16)
               MOVE 'MINUS PÅ KONTANTSALGS' TO STAT-IO-AREA (68:21)
               MOVE 'FAKTURA.'             TO STAT-IO-AREA (90:8)
               MOVE 1                      TO STAT-AFTER-SPACE
               PERFORM STAT-PRINT-LINE
               MOVE SPACES TO STAT-IO-AREA
               INITIALIZE STAT-IO-AREA
               MOVE '     DETTE MEDFØRER'  TO STAT-IO-AREA (11:19)
               MOVE 'ATT KR.NOTASUM IKKE'  TO STAT-IO-AREA (31:19)
               MOVE 'KAN AVSTEMMES MOT'    TO STAT-IO-AREA (51:17)
               MOVE ' DENNE FORDELINGS STA' TO STAT-IO-AREA (68:21)
               MOVE 'TISTIKK.'             TO STAT-IO-AREA (89:8)
               MOVE 2                      TO STAT-AFTER-SPACE
               PERFORM STAT-PRINT-LINE
               MOVE SPACES TO STAT-IO-AREA
               INITIALIZE STAT-IO-AREA
               MOVE 'KREDITORDRE TYPE    ' TO STAT-IO-AREA (11:20)
               MOVE 'ANT. ORDRE'           TO STAT-IO-AREA (36:10)
               MOVE 'ANT. LINJER'          TO STAT-IO-AREA (50:11)
               MOVE 'BELØP '               TO STAT-IO-AREA (70:6)
               MOVE 2                      TO STAT-AFTER-SPACE
               PERFORM STAT-PRINT-LINE
               MOVE SPACES TO STAT-IO-AREA
               INITIALIZE STAT-IO-AREA
               MOVE '2 = RETUR AV VARER  ' TO STAT-IO-AREA (11:20)
               MOVE ANTO2                  TO XO-50YY9
               MOVE XO-50YY9               TO STAT-IO-AREA (40:6)
               MOVE ANTL2                  TO XO-50YY9
               MOVE XO-50YY9               TO STAT-IO-AREA (55:6)
               MOVE SUMB2                  TO XO-72YY9R
               MOVE XO-72YY9R              TO STAT-IO-AREA (63:13)
               MOVE 2                      TO STAT-AFTER-SPACE
               PERFORM STAT-PRINT-LINE
               MOVE SPACES TO STAT-IO-AREA
               INITIALIZE STAT-IO-AREA
               MOVE '3 = FEIL RAB/PRIS   ' TO STAT-IO-AREA (11:20)
               MOVE ANTO3                  TO XO-50YY9
               MOVE XO-50YY9               TO STAT-IO-AREA (40:6)
               MOVE ANTL3                  TO XO-50YY9
               MOVE XO-50YY9               TO STAT-IO-AREA (55:6)
               MOVE SUMB3                  TO XO-72YY9R
               MOVE XO-72YY9R              TO STAT-IO-AREA (63:13)
               MOVE 2                      TO STAT-AFTER-SPACE
               PERFORM STAT-PRINT-LINE
               MOVE SPACES TO STAT-IO-AREA
               INITIALIZE STAT-IO-AREA
               MOVE '4 = BONUS           ' TO STAT-IO-AREA (11:20)
               MOVE ANTO4                  TO XO-50YY9
               MOVE XO-50YY9               TO STAT-IO-AREA (40:6)
               MOVE ANTL4                  TO XO-50YY9
               MOVE XO-50YY9               TO STAT-IO-AREA (55:6)
               MOVE SUMB4                  TO XO-72YY9R
               MOVE XO-72YY9R              TO STAT-IO-AREA (63:13)
               MOVE 2                      TO STAT-AFTER-SPACE
               PERFORM STAT-PRINT-LINE
               MOVE SPACES TO STAT-IO-AREA
               INITIALIZE STAT-IO-AREA
               MOVE '5 = ERSTATNING      ' TO STAT-IO-AREA (11:20)
               MOVE ANTO5                  TO XO-50YY9
               MOVE XO-50YY9               TO STAT-IO-AREA (40:6)
               MOVE ANTL5                  TO XO-50YY9
               MOVE XO-50YY9               TO STAT-IO-AREA (55:6)
               MOVE SUMB5                  TO XO-72YY9R
               MOVE XO-72YY9R              TO STAT-IO-AREA (63:13)
               MOVE 2                      TO STAT-AFTER-SPACE
               PERFORM STAT-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '***   AVSTEMNINGSTOTALER' TO LISTE-IO-AREA (1:24)
               MOVE ' FAK170 '             TO LISTE-IO-AREA (25:8)
               MOVE 'F A K T U R A / K R E D' TO LISTE-IO-AREA (33:23)
               MOVE 'I T N O T A S T A T I S' TO LISTE-IO-AREA (57:23)
               MOVE 'T I K K'              TO LISTE-IO-AREA (81:7)
               MOVE MND                    TO LISTE-IO-AREA (90:9)
               MOVE 'SISTE O.DATO'         TO LISTE-IO-AREA (100:12)
               MOVE ODATO                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (113:8)
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
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'DENNE  FAKTURAOMGANG' TO LISTE-IO-AREA (55:20)
               MOVE 'HITTIL DENNE MÅNED'   TO LISTE-IO-AREA (95:18)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'VAREGR VAREGRUPPENAVN' TO LISTE-IO-AREA (1:21)
               MOVE 'FAKTURASUM'           TO LISTE-IO-AREA (52:10)
               MOVE 'KR.NOTASUM'           TO LISTE-IO-AREA (70:10)
               MOVE 'FAKTURASUM'           TO LISTE-IO-AREA (92:10)
               MOVE 'KR.NOTASUM'           TO LISTE-IO-AREA (110:10)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'GRANDTOTAL'           TO LISTE-IO-AREA (2:10)
               MOVE FAKLP                  TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (48:15)
               MOVE KRELP                  TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (67:14)
               MOVE FAKLM                  TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (88:15)
               MOVE KRELM                  TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (107:14)
      *****************************************************************
      * DUMMY-LINJE FOR Å LAGE REF. TIL DATAFELT (FJERNE FEILMELDING) *
      *****************************************************************
               MOVE 2                      TO LISTE-BEFORE-SPACE
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
           MOVE 3                          TO LR-CHECK
           SET INFILE-LEVEL-INIT           TO TRUE
           INITIALIZE INFILE-DATA-FIELDS
           SET INFILE-EOF-OFF              TO TRUE
           SET INFILE-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO INFILE-MC
                                              INFILE-MP
           OPEN INPUT INFILE
           INITIALIZE FAKPAR-DATA-FIELDS
           SET FAKPAR-EOF-OFF              TO TRUE
           SET FAKPAR-PROCESS              TO TRUE
           OPEN INPUT FAKPAR
           SET STATREC-LEVEL-INIT          TO TRUE
           INITIALIZE STATREC-DATA-FIELDS
           SET STATREC-EOF-OFF             TO TRUE
           SET STATREC-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO STATREC-MC
                                              STATREC-MP
           OPEN INPUT STATREC
           INITIALIZE VAGRMAS-DATA-FIELDS
           OPEN INPUT VAGRMAS
           INITIALIZE SYSPARM-DATA-FIELDS
           OPEN INPUT SYSPARM
           INITIALIZE KONTOMA-DATA-FIELDS
           OPEN INPUT KONTOMA
           OPEN OUTPUT STAT
           INITIALIZE STAT-IO-AREA
           INITIALIZE STAT-DATA-FIELDS
           MOVE 57                         TO STAT-MAX-LINES
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           PERFORM VARYING ARK-I FROM 1 BY 1
                     UNTIL ARK-I > ARK-MAX
               INITIALIZE ARK (ARK-I)
               INITIALIZE ARV (ARK-I)
           END-PERFORM
           SET ARK-I                       TO 1
           PERFORM VARYING ARP-I FROM 1 BY 1
                     UNTIL ARP-I > ARP-MAX
               INITIALIZE ARP (ARP-I)
           END-PERFORM
           SET ARP-I                       TO 1
           PERFORM VARYING ARF-I FROM 1 BY 1
                     UNTIL ARF-I > ARF-MAX
               INITIALIZE ARF (ARF-I)
           END-PERFORM
           SET ARF-I                       TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INFILE
           CLOSE FAKPAR
           CLOSE STATREC
           CLOSE VAGRMAS
           CLOSE SYSPARM
           CLOSE KONTOMA
           IF STAT-IO-AREA NOT = SPACES
             WRITE STAT-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO STAT-IO-AREA
           END-IF
           CLOSE STAT
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
