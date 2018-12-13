       IDENTIFICATION DIVISION.
       PROGRAM-ID. RKO361R.
      **********************************************  Z-WIN-RPG2P     *
      *    KONV. IFRA RSK361 UTVIDET RECORD.     ***TXT***ok ss***    *
      *  PROGRAM: RKO361                                        *
      *  JOBB   : RES18UM                                       *
      *  PRINTE RENTELISTE, SAMT DANNE FAKTURARECORD"S.         *
      *  ALLE RECORD BLIR MERKET MED "IKKE GEBYR" FOR Å         *
      *  HINDRE ATT FAKTURAGEBYR BLIR DANNET I TILLEGG.         *
      *  ALLE RECORD BLIR MERKET MED "R" I POS 177              *
      *** E N D R I N G E R *************************************
      *  E.L. 1.12.90 PURREGEBYR BLIR OGSÅ DANNET.              *
      *  E.L. 7.10.91 INKASSOGEBYR BLIR DANNET.                 *
      *  E.L.29.11.91 HINDRET FAKTURARECORD MED 0 I GEBYR.      *
      *  E.L.23.03.93 NY FAKTURARECORD.                         *
      *  E.L.23.09.96 EDITERER ANT GEBYS FELT (VAR FEIL).       *
      *  M.T.13.03.97 FEILRETTING: SKREV PURREGEBYR PÅ FEIL     *
      *               FIRMA NÅR NESTE FIRMA IKKE HADDE KUNDER   *
      *               MED PÅLØPTE RENTER.                       *
      *  M.T.10.10.97 SETTER ANMERKNING NÅR KUNDEN IKKE SKAL    *
      *               RENTEBEREGNES OG/ELLER NÅR AVVIKENDE      *
      *               RENTE BLIR BRUKT.                         *
      *  M.T.16.10.97 BRUKER KUNDENS BM FOR RENTENOTA VENG (956)*
      *  M.T.17.10.97 BRUKER KUNDENS BM FOR RENTENOTA AD (399)  *
      *  M.T.03.12.97 BRUKER KUNDENS BM FOR RENTENOTA FOSS (985)*
      *  M.T.29.12.97 BRUKER 15 DAGER POSTGIRO FOR VENG (956)   *
      *  M.T.09.10.98 TATT UT SJEKK PÅ BM FOR 900, 910, 930,    *
      *               935 OG 937.                               *
      *  M.T.09.10.01 TATT UT SJEKK PÅ BM FOR 956, 957.         *
      *  M.T.04.09.01 UTVIDET LINK-OMRÅDE.                      *
      *  M.T.05.06.02 TATT I SIDESKIFT-REC I FAKTUT.            *
      *  M.T.06.09.02 SCANGROSS SKAL HA 30 DAGERS FORFALL PÅ    *
      *               RENTENOTA.                                *
      *  E.L.01.02.05 LAGT INN SEQ.NR. I FAKT.REC.              *
      *  E.L.29.04.09 LAGT INN TEKSTRECORDS SOM SPESIFISERER    *
      *               FAKTURA NUMMER SOM HAR GITT PURREGEBYR.   *
      *               MAKS 3 RECORD MED 6 FAKTURANR.            *
      *               FAKTURANR. HENTES FRA PURRERUTINEN.       *
      ***********************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RKO361.rpg
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
           SELECT KUNDEMA
               ASSIGN TO KUNDEMA
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMA-STATUS
               RECORD KEY IS KUNDEMA-KEY1.
           SELECT KUNDET
               ASSIGN TO UT-S-KUNDET
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KUNDET-STATUS.
           SELECT RENTEGR
               ASSIGN TO UT-S-RENTEGR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RENTEGR-STATUS.
           SELECT PURFAK
               ASSIGN TO UT-S-PURFAK
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PURFAK-STATUS.
           SELECT MNDPAR
               ASSIGN TO UT-S-MNDPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS MNDPAR-STATUS.
           SELECT RESPAR
               ASSIGN TO UT-S-RESPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RESPAR-STATUS.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT FAKTUT
               ASSIGN TO UT-S-FAKTUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKTUT-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
      *KUNDEMA IP  F4000 200    2       DISK40 SYS011S
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1.
                   15  KUNDEMA-KEY1N       PICTURE S9(9).
               10  FILLER                  PICTURE X(190).
       FD KUNDET
               BLOCK CONTAINS 4080
               RECORD CONTAINS 30.
       01  KUNDET-IO-AREA.
           05  KUNDET-IO-AREA-X            PICTURE X(30).
       FD RENTEGR
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  RENTEGR-IO-AREA.
           05  RENTEGR-IO-AREA-X           PICTURE X(200).
       FD PURFAK
               BLOCK CONTAINS 300
               RECORD CONTAINS 150.
       01  PURFAK-IO-AREA.
           05  PURFAK-IO-AREA-X            PICTURE X(150).
       FD MNDPAR
               BLOCK CONTAINS 100
               RECORD CONTAINS 100.
       01  MNDPAR-IO-AREA.
           05  MNDPAR-IO-AREA-X            PICTURE X(100).
       FD RESPAR
               BLOCK CONTAINS 300
               RECORD CONTAINS 300.
       01  RESPAR-IO-AREA.
           05  RESPAR-IO-AREA-X            PICTURE X(300).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD FAKTUT
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  FAKTUT-IO-AREA.
           05  FAKTUT-IO-AREA-X            PICTURE X(200).
      *BUGFILO O   F  80  80            PRINTERSYSLST
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
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  KUNDET-STATUS               PICTURE 99 VALUE 0.
           10  RENTEGR-STATUS              PICTURE 99 VALUE 0.
           10  PURFAK-STATUS               PICTURE 99 VALUE 0.
           10  MNDPAR-STATUS               PICTURE 99 VALUE 0.
           10  RESPAR-STATUS               PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  FAKTUT-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  TKDATA-XX-STATUS            PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  KUNDEMA-EOF-OFF         VALUE '0'.
               88  KUNDEMA-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KUNDEMA-READ-OFF        VALUE '0'.
               88  KUNDEMA-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KUNDEMA-PROCESS-OFF     VALUE '0'.
               88  KUNDEMA-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  KUNDEMA-LEVEL-INIT-OFF  VALUE '0'.
               88  KUNDEMA-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KUNDET-EOF-OFF          VALUE '0'.
               88  KUNDET-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KUNDET-READ-OFF         VALUE '0'.
               88  KUNDET-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KUNDET-PROCESS-OFF      VALUE '0'.
               88  KUNDET-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  KUNDET-LEVEL-INIT-OFF   VALUE '0'.
               88  KUNDET-LEVEL-INIT       VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RENTEGR-EOF-OFF         VALUE '0'.
               88  RENTEGR-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RENTEGR-READ-OFF        VALUE '0'.
               88  RENTEGR-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RENTEGR-PROCESS-OFF     VALUE '0'.
               88  RENTEGR-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  RENTEGR-LEVEL-INIT-OFF  VALUE '0'.
               88  RENTEGR-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PURFAK-EOF-OFF          VALUE '0'.
               88  PURFAK-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PURFAK-READ-OFF         VALUE '0'.
               88  PURFAK-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PURFAK-PROCESS-OFF      VALUE '0'.
               88  PURFAK-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  PURFAK-LEVEL-INIT-OFF   VALUE '0'.
               88  PURFAK-LEVEL-INIT       VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  MNDPAR-EOF-OFF          VALUE '0'.
               88  MNDPAR-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  MNDPAR-READ-OFF         VALUE '0'.
               88  MNDPAR-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  MNDPAR-PROCESS-OFF      VALUE '0'.
               88  MNDPAR-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESPAR-EOF-OFF          VALUE '0'.
               88  RESPAR-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESPAR-READ-OFF         VALUE '0'.
               88  RESPAR-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESPAR-PROCESS-OFF      VALUE '0'.
               88  RESPAR-PROCESS          VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
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
      * **START RBS - DATASTRUKTUR FOR SUB-PROGRAM RBS000 ********
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
               10  FINAVN                  PICTURE X(30).
               10  FILLER                  PICTURE X(205).
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(52).
               10  LOPNVN                  PICTURE X(35).
               10  FILLER                  PICTURE X(170).
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(87).
               10  LPRIID                  PICTURE X(4).
               10  FILLER                  PICTURE X(166).
      *     *  BESTILLINGSOPPGAVER (OVERSTYRING AV RBS-FILE) *    *
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(91).
               10  BJOBN                   PICTURE X(8).
               10  FILLER                  PICTURE X(158).
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(99).
               10  BBEST                   PICTURE X(1).
               10  FILLER                  PICTURE X(157).
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(100).
               10  BPERS                   PICTURE X(30).
               10  FILLER                  PICTURE X(127).
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(130).
               10  BETTB                   PICTURE X(40).
               10  FILLER                  PICTURE X(87).
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(170).
               10  BFORS                   PICTURE X(40).
               10  FILLER                  PICTURE X(47).
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(210).
               10  BMEMO                   PICTURE X(40).
               10  FILLER                  PICTURE X(7).
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(250).
               10  BANTX-IO.
                   15  BANTX               PICTURE S9(3).
               10  FILLER                  PICTURE X(4).
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(253).
               10  BPCLAS                  PICTURE X(1).
               10  FILLER                  PICTURE X(3).
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
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
           05  KUNDEMA-LEVEL-04.
               10  KUNDEMA-04-L2.
                   15  KUNDEMA-04-L2-FIRMA PICTURE X(3).
               10  KUNDEMA-04-L1.
                   15  KUNDEMA-04-L1-KUNDE PICTURE X(6).
           05  KUNDEMA-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  KUNDE                   PICTURE X(6).
               10  KNAVN1                  PICTURE X(30).
               10  KNAVN2                  PICTURE X(30).
               10  ADR                     PICTURE X(30).
               10  POST                    PICTURE X(15).
               10  POSTNR                  PICTURE X(4).
               10  BETM                    PICTURE X(2).
               10  IPURRE-IO.
                   15  IPURRE              PICTURE S9(1).
               10  IRENTE                  PICTURE X(1).
               10  HND                     PICTURE X(3).
               10  ANTPUR-IO.
                   15  ANTPUR              PICTURE S9(1).
               10  ANTINK-IO.
                   15  ANTINK              PICTURE S9(1).
           05  KUNDEMA-MP                  PICTURE X(9).
           05  KUNDEMA-MC                  PICTURE X(9).
           05  KUNDEMA-M-04            REDEFINES KUNDEMA-MC.
               10  KUNDEMA-M-04-M2.
                   15  KUNDEMA-M-04-M2-FIRMA-G.
                       20  KUNDEMA-M-04-M2-FIRMA PICTURE X(3).
               10  KUNDEMA-M-04-M1.
                   15  KUNDEMA-M-04-M1-KUNDE-G.
                       20  KUNDEMA-M-04-M1-KUNDE PICTURE X(6).
           05  KUNDET-LEVEL-01.
               10  KUNDET-01-L2.
                   15  KUNDET-01-L2-FIRMA  PICTURE X(3).
               10  KUNDET-01-L1.
                   15  KUNDET-01-L1-KUNDE  PICTURE X(6).
           05  KUNDET-DATA-FIELDS.
               10  KUNDEB-IO.
                   15  KUNDEB              PICTURE S9(7)V9(2).
               10  KUNDEA-IO.
                   15  KUNDEA              PICTURE S9(7)V9(2).
           05  KUNDET-MP                   PICTURE X(9).
           05  KUNDET-MC                   PICTURE X(9).
           05  KUNDET-M-01             REDEFINES KUNDET-MC.
               10  KUNDET-M-01-M2.
                   15  KUNDET-M-01-M2-FIRMA-G.
                       20  KUNDET-M-01-M2-FIRMA PICTURE X(3).
               10  KUNDET-M-01-M1.
                   15  KUNDET-M-01-M1-KUNDE-G.
                       20  KUNDET-M-01-M1-KUNDE PICTURE X(6).
           05  RENTEGR-LEVEL-02.
               10  RENTEGR-02-L2.
                   15  RENTEGR-02-L2-FIRMA PICTURE X(3).
               10  RENTEGR-02-L1.
                   15  RENTEGR-02-L1-KUNDE PICTURE X(6).
           05  RENTEGR-DATA-FIELDS.
               10  REFNR                   PICTURE X(6).
               10  BILA-ELGR               PICTURE X(2).
               10  BILMND                  PICTURE X(2).
               10  BILDAG                  PICTURE X(2).
               10  BILNR                   PICTURE X(6).
               10  FORA-ELGR               PICTURE X(2).
               10  FORMND                  PICTURE X(2).
               10  FORDAG                  PICTURE X(2).
               10  BELO-ELGP-IO.
                   15  BELO-ELGP           PICTURE S9(7)V9(2).
               10  MERKE                   PICTURE X(1).
               10  MERKE2                  PICTURE X(1).
               10  RBEL-IO.
                   15  RBEL                PICTURE S9(7)V9(2).
               10  RDAGER-IO.
                   15  RDAGER              PICTURE S9(4).
               10  RBELAV-IO.
                   15  RBELAV              PICTURE S9(7)V9(2).
               10  TRKODE                  PICTURE X(2).
           05  RENTEGR-MP                  PICTURE X(9).
           05  RENTEGR-MC                  PICTURE X(9).
           05  RENTEGR-M-02            REDEFINES RENTEGR-MC.
               10  RENTEGR-M-02-M2.
                   15  RENTEGR-M-02-M2-FIRMA-G.
                       20  RENTEGR-M-02-M2-FIRMA PICTURE X(3).
               10  RENTEGR-M-02-M1.
                   15  RENTEGR-M-02-M1-KUNDE-G.
                       20  RENTEGR-M-02-M1-KUNDE PICTURE X(6).
           05  PURFAK-LEVEL-07.
               10  PURFAK-07-L2.
                   15  PURFAK-07-L2-FIRMA  PICTURE X(3).
               10  PURFAK-07-L1.
                   15  PURFAK-07-L1-KUNDE  PICTURE X(6).
           05  PURFAK-DATA-FIELDS.
               10  FTEKST                  PICTURE X(8).
               10  FNR61                   PICTURE X(41).
               10  FNR62                   PICTURE X(41).
               10  FNR63                   PICTURE X(41).
               10  ANTX6                   PICTURE X(1).
           05  PURFAK-MP                   PICTURE X(9).
           05  PURFAK-MC                   PICTURE X(9).
           05  PURFAK-M-07             REDEFINES PURFAK-MC.
               10  PURFAK-M-07-M2.
                   15  PURFAK-M-07-M2-FIRMA-G.
                       20  PURFAK-M-07-M2-FIRMA PICTURE X(3).
               10  PURFAK-M-07-M1.
                   15  PURFAK-M-07-M1-KUNDE-G.
                       20  PURFAK-M-07-M1-KUNDE PICTURE X(6).
           05  MNDPAR-DATA-FIELDS.
               10  A-ELGR                  PICTURE X(4).
               10  MNDNVN                  PICTURE X(9).
           05  RESPAR-DATA-FIELDS.
               10  PERDAT                  PICTURE X(6).
               10  PERIOD                  PICTURE X(6).
           05  FIRMAF-DATA-FIELDS.
               10  RVGR                    PICTURE X(5).
               10  EDB-IO.
                   15  EDB                 PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  FTYPE                   PICTURE X(1).
               10  NOTAB-IO.
                   15  NOTAB               PICTURE S9(3).
               10  FPGEB                   PICTURE X(1).
               10  FPVGR                   PICTURE X(5).
               10  FPEDB                   PICTURE X(7).
               10  PGEBEL-IO.
                   15  PGEBEL              PICTURE S9(3).
               10  IGEBEL-IO.
                   15  IGEBEL              PICTURE S9(3).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  BLANKE                  PICTURE X(18).
               10  BLA10                   PICTURE X(10).
               10  ANTA-IO.
                   15  ANTA                PICTURE S9(2).
               10  ANT-IO.
                   15  ANT                 PICTURE S9(3).
               10  PBELK-IO.
                   15  PBELK               PICTURE S9(7)V9(2).
               10  IBELK-IO.
                   15  IBELK               PICTURE S9(7)V9(2).
               10  SBELK-IO.
                   15  SBELK               PICTURE S9(7)V9(2).
               10  SEQNR-IO.
                   15  SEQNR               PICTURE S9(4).
               10  SEQNT1-IO.
                   15  SEQNT1              PICTURE S9(4).
               10  SEQNT2-IO.
                   15  SEQNT2              PICTURE S9(4).
               10  SEQNT3-IO.
                   15  SEQNT3              PICTURE S9(4).
               10  SEQNT4-IO.
                   15  SEQNT4              PICTURE S9(4).
               10  SEQNT5-IO.
                   15  SEQNT5              PICTURE S9(4).
               10  SEQNT6-IO.
                   15  SEQNT6              PICTURE S9(4).
               10  FIRBEL-IO.
                   15  FIRBEL              PICTURE S9(8)V9(2).
               10  IKKERB-IO.
                   15  IKKERB              PICTURE S9(8)V9(2).
               10  NULL7-IO.
                   15  NULL7               PICTURE S9(5)V9(2).
               10  NULL9-IO.
                   15  NULL9               PICTURE S9(7)V9(2).
               10  PBELF-IO.
                   15  PBELF               PICTURE S9(7)V9(2).
               10  IBELF-IO.
                   15  IBELF               PICTURE S9(7)V9(2).
               10  FAKART                  PICTURE X(1).
               10  ANTPUF-IO.
                   15  ANTPUF              PICTURE S9(5)V9(2).
               10  PBEL-IO.
                   15  PBEL                PICTURE S9(7)V9(2).
               10  PBELGT-IO.
                   15  PBELGT              PICTURE S9(9)V9(2).
               10  ANTIKF-IO.
                   15  ANTIKF              PICTURE S9(5)V9(2).
               10  IBEL-IO.
                   15  IBEL                PICTURE S9(7)V9(2).
               10  IBELGT-IO.
                   15  IBELGT              PICTURE S9(9)V9(2).
               10  TESTB-IO.
                   15  TESTB               PICTURE S9(7)V9(2).
               10  BELATX                  PICTURE X(18).
               10  TXT10                   PICTURE X(10).
               10  FIRBGT-IO.
                   15  FIRBGT              PICTURE S9(9)V9(2).
               10  IKKEGT-IO.
                   15  IKKEGT              PICTURE S9(9)V9(2).
               10  SEQNR1-IO.
                   15  SEQNR1              PICTURE S9(4).
               10  SEQNR2-IO.
                   15  SEQNR2              PICTURE S9(4).
               10  TEKST                   PICTURE X(7).
           05  EDITTING-FIELDS.
               10  XO-40D                  PICTURE S9(4).
               10  XO-40U                  PICTURE 9(4).
               10  XO-72YNZR               PICTURE ZZZZZZZ,ZZ-.
               10  XO-40YYZ                PICTURE Z.ZZZ.
               10  XO-82YY9R               PICTURE ZZ.ZZZ.ZZZ,99-.
               10  XO-72YY9R               PICTURE Z.ZZZ.ZZZ,99-.
               10  XO-92YY9R               PICTURE ZZZ.ZZZ.ZZZ,99-.
               10  XO-52P-EF.
                 15  XO-52P                PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-72P-EF.
                 15  XO-72P                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-70D                  PICTURE S9(7).
               10  XO-70U                  PICTURE 9(7).
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
           IF  NOT-SET-I-OV
               SET NOT-I-OV                TO TRUE
           END-IF
           SET NOT-SET-I-OV                TO TRUE
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-04                    TO TRUE
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-07                    TO TRUE
           SET NOT-I-05                    TO TRUE
           SET NOT-I-06                    TO TRUE
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  KUNDEMA-PROCESS
               SET KUNDEMA-PROCESS-OFF     TO TRUE
               SET KUNDEMA-READ            TO TRUE
           END-IF
 
           IF  KUNDEMA-READ
               PERFORM KUNDEMA-GET
               SET KUNDEMA-READ-OFF        TO TRUE
               IF  NOT KUNDEMA-EOF
                   PERFORM KUNDEMA-MATCH-SET
               END-IF
           END-IF
 
           IF  KUNDET-PROCESS
               SET KUNDET-PROCESS-OFF      TO TRUE
               SET KUNDET-READ             TO TRUE
           END-IF
 
           IF  KUNDET-READ
               PERFORM KUNDET-GET
               SET KUNDET-READ-OFF         TO TRUE
               IF  NOT KUNDET-EOF
                   PERFORM KUNDET-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM KUNDET-MATCH-SET
               END-IF
           END-IF
 
           IF  RENTEGR-PROCESS
               SET RENTEGR-PROCESS-OFF     TO TRUE
               SET RENTEGR-READ            TO TRUE
           END-IF
 
           IF  RENTEGR-READ
               PERFORM RENTEGR-GET
               SET RENTEGR-READ-OFF        TO TRUE
               IF  NOT RENTEGR-EOF
                   PERFORM RENTEGR-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM RENTEGR-MATCH-SET
               END-IF
           END-IF
 
           IF  PURFAK-PROCESS
               SET PURFAK-PROCESS-OFF      TO TRUE
               SET PURFAK-READ             TO TRUE
           END-IF
 
           IF  PURFAK-READ
               PERFORM PURFAK-GET
               SET PURFAK-READ-OFF         TO TRUE
               IF  NOT PURFAK-EOF
                   PERFORM PURFAK-MATCH-SET
               END-IF
           END-IF
 
           IF  MNDPAR-PROCESS
               SET MNDPAR-PROCESS-OFF      TO TRUE
               SET MNDPAR-READ             TO TRUE
           END-IF
 
           IF  MNDPAR-READ
           AND RECORD-SELECTED-OFF
               PERFORM MNDPAR-GET
               SET MNDPAR-READ-OFF         TO TRUE
               IF  NOT MNDPAR-EOF
                   SET MNDPAR-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  RESPAR-PROCESS
               SET RESPAR-PROCESS-OFF      TO TRUE
               SET RESPAR-READ             TO TRUE
           END-IF
 
           IF  RESPAR-READ
           AND RECORD-SELECTED-OFF
               PERFORM RESPAR-GET
               SET RESPAR-READ-OFF         TO TRUE
               IF  NOT RESPAR-EOF
                   SET RESPAR-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
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
 
           IF  KUNDEMA-PROCESS
               PERFORM KUNDEMA-IDSET
           END-IF
 
           IF  KUNDET-PROCESS
               PERFORM KUNDET-IDSET
           END-IF
 
           IF  RENTEGR-PROCESS
               PERFORM RENTEGR-IDSET
           END-IF
 
           IF  PURFAK-PROCESS
               PERFORM PURFAK-IDSET
           END-IF
 
           IF  MNDPAR-PROCESS
               PERFORM MNDPAR-IDSET
           END-IF
 
           IF  RESPAR-PROCESS
               PERFORM RESPAR-IDSET
           END-IF
 
           IF  KUNDEMA-PROCESS
               PERFORM KUNDEMA-CHK-LEVEL
           END-IF
 
           IF  KUNDET-PROCESS
               PERFORM KUNDET-CHK-LEVEL
           END-IF
 
           IF  RENTEGR-PROCESS
               PERFORM RENTEGR-CHK-LEVEL
           END-IF
 
           IF  PURFAK-PROCESS
               PERFORM PURFAK-CHK-LEVEL
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
 
           IF  KUNDEMA-PROCESS
               PERFORM KUNDEMA-FLDSET
           END-IF
 
           IF  KUNDET-PROCESS
               PERFORM KUNDET-FLDSET
           END-IF
 
           IF  RENTEGR-PROCESS
               PERFORM RENTEGR-FLDSET
           END-IF
 
           IF  PURFAK-PROCESS
               PERFORM PURFAK-FLDSET
           END-IF
 
           IF  MNDPAR-PROCESS
               PERFORM MNDPAR-FLDSET
           END-IF
 
           IF  RESPAR-PROCESS
               PERFORM RESPAR-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  KUNDEMA-PROCESS
           OR  KUNDET-PROCESS
           OR  RENTEGR-PROCESS
           OR  PURFAK-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-05)
               MOVE '        '             TO BLANKE (1:8)
               MOVE '        '             TO BLA10 (1:8)
               MOVE '  '                   TO BLA10 (9:2)
               MOVE BLA10                  TO BLANKE (9:10)
           END-IF
           IF  (I-05)
               OR  (I-06)
               GO TO SLUTT-T
           END-IF
           IF  (I-L2)
               SET NOT-I-80                TO TRUE
               SET NOT-I-81                TO TRUE
           END-IF
           IF  (I-L1)
               SET NOT-I-11                TO TRUE
               SET NOT-I-12                TO TRUE
               SET NOT-I-19                TO TRUE
               SET NOT-I-15                TO TRUE
               SET NOT-I-16                TO TRUE
               SET NOT-I-17                TO TRUE
               SET NOT-I-50                TO TRUE
               SET NOT-I-51                TO TRUE
               SET NOT-I-52                TO TRUE
               SET NOT-I-53                TO TRUE
               SET NOT-I-55                TO TRUE
               SET NOT-I-91                TO TRUE
               SET NOT-I-93                TO TRUE
               SET NOT-I-94                TO TRUE
      *
           END-IF
           SET NOT-I-30                    TO TRUE
           SET NOT-I-80                    TO TRUE
           IF  (I-60)
               OR  (I-L1)
               MOVE 0                      TO ANTA
           END-IF
           IF  (I-L1)
               MOVE 0                      TO ANT
               MOVE 0                      TO PBELK
               MOVE 0                      TO IBELK
               MOVE 0                      TO SBELK
               MOVE 0                      TO SEQNR
               MOVE 0                      TO SEQNT1
               MOVE 0                      TO SEQNT2
               MOVE 0                      TO SEQNT3
               MOVE 0                      TO SEQNT4
               MOVE 0                      TO SEQNT5
               MOVE 0                      TO SEQNT6
           END-IF
           IF  (I-L2)
               MOVE 0                      TO FIRBEL
               MOVE 0                      TO IKKERB
               MOVE 0                      TO NULL7
               MOVE 0                      TO NULL9
               MOVE 0                      TO PBELF
               MOVE 0                      TO IBELF
           END-IF
           SET NOT-I-60                    TO TRUE
      *******************************************************************
      *  RUTINE FOR OPPSLAG MOT FIRMAFILE.                   *
      *******************************************************************
           IF  (I-L2)
               MOVE FIRMA                  TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-10                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-10            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
               SET NOT-I-40                TO TRUE
               IF  FPGEB = 'J'
                   SET I-40                TO TRUE
               END-IF
               SET NOT-I-69                TO TRUE
               IF  IGEBEL > PGEBEL
                   SET I-69                TO TRUE
               END-IF
               SET NOT-I-66                TO TRUE
               IF  FTYPE = '1'
                   SET I-66                TO TRUE
               END-IF
               SET NOT-I-67                TO TRUE
               IF  FTYPE = '2'
                   SET I-67                TO TRUE
               END-IF
               MOVE ' '                    TO FAKART
           END-IF
           IF  (I-L2 AND I-66)
               MOVE 'D'                    TO FAKART
           END-IF
           IF  (I-L2 AND I-67)
               MOVE 'F'                    TO FAKART
      *******************************************************************
      *    FIRMA SOM ALLTID SKAL HA 15 DAGER FORFALL PÅ RENTENOTA 35    *
      *    FIRMA SOM ALLTID SKAL HA 30 DAGER FORFALL PÅ RENTENOTA 36    *
      *******************************************************************
           END-IF
           IF  (I-L2)
               SET NOT-I-35                TO TRUE
               SET NOT-I-36                TO TRUE
               SET NOT-I-35                TO TRUE
               IF  FIRMA = '915'
                   SET I-35                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-35)
               SET NOT-I-35                TO TRUE
               IF  FIRMA = '970'
                   SET I-35                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-35)
               SET NOT-I-35                TO TRUE
               IF  FIRMA = '977'
                   SET I-35                TO TRUE
               END-IF
           END-IF
           IF  (I-L2)
               SET NOT-I-36                TO TRUE
               IF  FIRMA = '923'
                   SET I-36                TO TRUE
               END-IF
      ********************************************************
      *  RUTINE FOR PURREGEBYR. (1. OG 2 GRADS PURRING.)     *
      ********************************************************
           END-IF
           IF  (I-04 AND NOT-I-69)
               ADD ANTINK                  TO ANTPUR
               MOVE 0                      TO ANTINK
           END-IF
           IF  (I-04)
               SET NOT-I-62                TO TRUE
               SET NOT-I-59                TO TRUE
               SET NOT-I-61                TO TRUE
               IF  ANTPUR > 1
                   SET I-62                TO TRUE
               END-IF
               IF  ANTPUR < 1
                   SET I-59                TO TRUE
               END-IF
               IF  ANTPUR = 1
                   SET I-61                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND I-62)
               MOVE 2                      TO ANTPUF
           END-IF
           IF  (I-04 AND I-61)
               MOVE 1                      TO ANTPUF
           END-IF
           IF  (I-04 AND I-59)
               MOVE 0                      TO ANTPUF
           END-IF
           IF  (I-04)
               SET NOT-I-41                TO TRUE
               IF  ANTPUF > 0
                   SET I-41                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND NOT-I-40)
               SET NOT-I-41                TO TRUE
           END-IF
           IF  (I-04)
               SET NOT-I-42                TO TRUE
               IF  IPURRE = 1
                   SET I-42                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND NOT-I-42)
               SET NOT-I-42                TO TRUE
               IF  IPURRE = 2
                   SET I-42                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND NOT-I-42)
               SET NOT-I-42                TO TRUE
               IF  PGEBEL = 0
                   SET I-42                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND I-42)
               SET NOT-I-41                TO TRUE
           END-IF
           IF  (I-04 AND I-41)
               ADD PGEBEL TO ZERO      GIVING PBEL
               MULTIPLY ANTPUF BY PBEL GIVING PBELK
               ADD PBELK                   TO PBELF
               ADD PBELK                   TO PBELGT
      *****************************************************************
      * RUTINE FOR Å DANNE FAKTURARECORDS MED FAKTURANR. SOM ER PURRET*
      *****************************************************************
           END-IF
           IF  (NOT-I-07)
               GO TO ENDFPR-T
           END-IF
           IF  (I-07 AND NOT-I-41)
               GO TO ENDFPR-T
           END-IF
           SET NOT-I-91                    TO TRUE
           IF  ANTX6 = '1'
               SET I-91                    TO TRUE
           END-IF
           SET NOT-I-93                    TO TRUE
           IF  ANTX6 = '2'
               SET I-93                    TO TRUE
           END-IF
           SET NOT-I-94                    TO TRUE
           IF  ANTX6 = '3'
               SET I-94                    TO TRUE
           END-IF.
 
       ENDFPR-T.
           IF  (I-07)
               GO TO SLUTT-T
      ********************************************************
      *  RUTINE FOR INKASSOGEBYR. (3. GRADS PURRING)         *
      ********************************************************
           END-IF
           IF  (I-04)
               SET NOT-I-65                TO TRUE
               SET NOT-I-63                TO TRUE
               SET NOT-I-64                TO TRUE
               IF  ANTINK > 1
                   SET I-65                TO TRUE
               END-IF
               IF  ANTINK < 1
                   SET I-63                TO TRUE
               END-IF
               IF  ANTINK = 1
                   SET I-64                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND I-65)
               MOVE 2                      TO ANTIKF
           END-IF
           IF  (I-04 AND I-64)
               MOVE 1                      TO ANTIKF
           END-IF
           IF  (I-04 AND I-63)
               MOVE 0                      TO ANTIKF
           END-IF
           IF  (I-04)
               SET NOT-I-43                TO TRUE
               IF  ANTIKF > 0
                   SET I-43                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND NOT-I-40)
               SET NOT-I-43                TO TRUE
           END-IF
           IF  (I-04)
               SET NOT-I-42                TO TRUE
               IF  IPURRE = 1
                   SET I-42                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND NOT-I-42)
               SET NOT-I-42                TO TRUE
               IF  IPURRE = 2
                   SET I-42                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND NOT-I-42)
               SET NOT-I-42                TO TRUE
               IF  IGEBEL = 0
                   SET I-42                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND I-42)
               SET NOT-I-43                TO TRUE
           END-IF
           IF  (I-04 AND I-43)
               ADD IGEBEL TO ZERO      GIVING IBEL
           END-IF
           IF  (I-04 AND I-41)
               MULTIPLY ANTIKF BY IBEL GIVING IBELK
               ADD IBELK                   TO IBELF
               ADD IBELK                   TO IBELGT
      *  04                MOVE "KUNDE   "BUGFL1  8        DISPLAY FIELD
      *  04      BUGFL1    DEBUGBUGFILO   KUNDE            VIS INDIKATOR
      ********************************************************
      *  SAMLERUTINE FOR PURRE OG INKASSOGEBYR.              *
      ********************************************************
           END-IF
           IF  (I-04)
               SET NOT-I-49                TO TRUE
           END-IF
           IF  (I-04 AND I-41)
               OR  (I-04 AND I-43)
               SET I-49                    TO TRUE
           END-IF
           IF  (I-04)
               ADD IBELK TO PBELK      GIVING SBELK
      ********************************************************
      *  RUTINE FOR BETALINGSBETINGELSE.                     *
      ********************************************************
           END-IF
           IF  (I-04)
               SET NOT-I-08                TO TRUE
               IF  BETM = '07'
                   SET I-08                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND NOT-I-08)
               SET NOT-I-08                TO TRUE
               IF  BETM = '14'
                   SET I-08                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND I-08)
               MOVE '02'                   TO BETM
           END-IF
           IF  (I-04)
               SET NOT-I-19                TO TRUE
               IF  BETM = '19'
                   SET I-19                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND I-19)
               MOVE '03'                   TO BETM
           END-IF
           IF  (I-04 AND I-35)
               MOVE '02'                   TO BETM
           END-IF
           IF  (I-04 AND I-36)
               MOVE '03'                   TO BETM
           END-IF
           IF  (I-04)
               GO TO SLUTT-T
      ********************************************************
      *  RUTINE FOR RENTEBEREGNING.                          *
      ********************************************************
           END-IF
           IF  (NOT-I-MR)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               SET NOT-I-21                TO TRUE
               IF  IRENTE = 'A'
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-21)
               ADD KUNDEB TO ZERO      GIVING TESTB
           END-IF
           IF  (I-01 AND I-21)
               ADD KUNDEA TO ZERO      GIVING TESTB
           END-IF
           IF  (I-01)
               SET NOT-I-12                TO TRUE
               IF  NOTAB > TESTB
                   SET I-12                TO TRUE
               END-IF
           END-IF
           IF  (I-12)
               GO TO SLUTT-T
      *
           END-IF
           IF  (I-01)
               SET NOT-I-11                TO TRUE
               IF  IRENTE = '1'
                   SET I-11                TO TRUE
               END-IF
      *  01                SETON                     1121   TEST 11/21
           END-IF
           IF  (I-01 AND I-11)
               MOVE '* BELAST'             TO BELATX (1:8)
               MOVE 'ES IKKE '             TO TXT10 (1:8)
               MOVE '* '                   TO TXT10 (9:2)
               MOVE TXT10                  TO BELATX (9:10)
           END-IF
           IF  (I-01 AND NOT-I-11)
               MOVE BLANKE                 TO BELATX
      *
           END-IF
           IF  (I-01 AND NOT-I-11 AND NOT-I-21)
               ADD KUNDEB                  TO FIRBEL
           END-IF
           IF  (I-01 AND NOT-I-11 AND I-21)
               ADD KUNDEA                  TO FIRBEL
           END-IF
           IF  (I-01 AND NOT-I-11 AND NOT-I-21)
               ADD KUNDEB                  TO FIRBGT
           END-IF
           IF  (I-01 AND NOT-I-11 AND I-21)
               ADD KUNDEA                  TO FIRBGT
           END-IF
           IF  (I-01 AND I-11 AND NOT-I-21)
               ADD KUNDEB                  TO IKKERB
           END-IF
           IF  (I-01 AND I-11 AND I-21)
               ADD KUNDEA                  TO IKKERB
           END-IF
           IF  (I-01 AND I-11 AND NOT-I-21)
               ADD KUNDEB                  TO IKKEGT
           END-IF
           IF  (I-01 AND I-11 AND I-21)
               ADD KUNDEA                  TO IKKEGT
           END-IF
           IF  (I-01 AND NOT-I-11)
               SET I-15                    TO TRUE
           END-IF
           IF  (I-01 AND I-15)
               MOVE 1                      TO SEQNR1
               MOVE 2                      TO SEQNR2
               MOVE 2                      TO SEQNR
           END-IF
           IF  (I-01)
               SET I-16                    TO TRUE
               GO TO SLUTT-T
      ********************************************************
           END-IF
           IF  (I-02 AND NOT-I-21)
               SET NOT-I-13                TO TRUE
               IF  MERKE = 'U'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-21)
               SET NOT-I-13                TO TRUE
               IF  MERKE2 = 'U'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-13)
               GO TO SLUTT-T
           END-IF
           SET I-30                        TO TRUE
           IF  (I-02 AND I-15 AND I-30)
               ADD 1                       TO SEQNR
      ********************************************************
      *  RUTINE FOR TEKST PÅ RENTELISTA.                     *
      ********************************************************
           END-IF
           IF  (I-02)
               PERFORM TKRUT-S
      ****  UTSKRIFTSINDIKATORER  *************************
           END-IF
           ADD 1                           TO ANT
           ADD 1                           TO ANTA
           SET NOT-I-50                    TO TRUE
           IF  ANT = 1
               SET I-50                    TO TRUE
           END-IF
           SET NOT-I-51                    TO TRUE
           IF  ANT = 2
               SET I-51                    TO TRUE
           END-IF
           SET NOT-I-52                    TO TRUE
           IF  ANT = 3
               SET I-52                    TO TRUE
           END-IF
           SET NOT-I-55                    TO TRUE
           SET NOT-I-53                    TO TRUE
           IF  ANT > 4
               SET I-55                    TO TRUE
           END-IF
           IF  ANT = 4
               SET I-53                    TO TRUE
           END-IF
           SET NOT-I-60                    TO TRUE
           IF  ANTA = 19
               SET I-60                    TO TRUE
           END-IF.
 
       SLUTT-T.
      ******************************************************
      *    INDIKATOR SETTING TIL SLUTT                     *
      ******************************************************
           IF  (I-41)
               OR  (I-49 AND NOT-I-81)
               OR  (I-16)
               SET I-17                    TO TRUE
           END-IF
           IF  (I-17 AND NOT-I-81)
               SET I-80                    TO TRUE
           END-IF
           IF  (I-80)
               PERFORM RBSRUT-S
           END-IF
           IF  (I-80)
               SET I-81                    TO TRUE
      *
           END-IF
           IF  (I-12)
               SET NOT-I-50                TO TRUE
               SET NOT-I-51                TO TRUE
               SET NOT-I-52                TO TRUE
               SET NOT-I-53                TO TRUE
               SET NOT-I-55                TO TRUE
      ******************************************************
      *   TOTALRUTINE FOR PRINTING AV PURREGEBYR.          *
      ******************************************************
           END-IF
           .
 
       TKRUT-S SECTION.
       TKRUT-S-P.
           MOVE TRKODE                     TO TKTK
           MOVE '       '                  TO TKTEXT
           MOVE BILNR                      TO TKBILN
           MOVE REFNR                      TO TKREFN
           CALL 'RESTRAN' USING TKDATA-XX-DATA-FIELDS
           MOVE TKTEXT                     TO TEKST.
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE ' '                        TO BBEST
           MOVE 'RES09'                    TO LONR
           MOVE FIRMA                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'RSK361  '                 TO LPROG
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
               SET NOT-I-61                TO TRUE
           END-IF
           IF  (I-L1 AND I-41)
               SET I-61                    TO TRUE
           END-IF
           IF  (I-L1 AND I-43)
               SET I-61                    TO TRUE
           END-IF
           IF  (I-L1 AND I-50)
               SET NOT-I-61                TO TRUE
           END-IF
           IF  (I-L1 AND I-51)
               SET NOT-I-61                TO TRUE
           END-IF
           IF  (I-L1 AND I-52)
               SET NOT-I-61                TO TRUE
           END-IF
           IF  (I-L1 AND I-53)
               SET NOT-I-61                TO TRUE
           END-IF
           IF  (I-L1 AND I-55)
               SET NOT-I-61                TO TRUE
           END-IF
           IF  (I-L1 AND I-49 AND NOT-I-15)
               ADD 1 TO SEQNR          GIVING SEQNT1
           END-IF
           IF  (I-L1 AND I-41)
               ADD 2 TO SEQNR          GIVING SEQNT2
           END-IF
           IF  (I-L1 AND I-41 AND I-91)
               ADD 3 TO SEQNR          GIVING SEQNT4
           END-IF
           IF  (I-L1 AND I-41 AND I-93)
               ADD 4 TO SEQNR          GIVING SEQNT5
           END-IF
           IF  (I-L1 AND I-41 AND I-94)
               ADD 5 TO SEQNR          GIVING SEQNT6
           END-IF
           IF  (I-L1 AND I-43)
               ADD 3 TO SEQNR          GIVING SEQNT3
           END-IF
           IF  (I-L1 AND I-43 AND I-41 AND I-91)
               ADD 4 TO SEQNR          GIVING SEQNT3
           END-IF
           IF  (I-L1 AND I-43 AND I-41 AND I-93)
               ADD 5 TO SEQNR          GIVING SEQNT3
           END-IF
           IF  (I-L1 AND I-43 AND I-41 AND I-94)
               ADD 6 TO SEQNR          GIVING SEQNT3
      ******************************************************
      *    SUBRUTINE FOR CALL AV COBOL SUBRUTINE RESTRAN.  *
      *    DENNE RUTINE HENTER RESKONTRO TEKST.            *
      ******************************************************
           END-IF
           .
 
       KUNDEMA-GET SECTION.
       KUNDEMA-GET-P.
           IF  KUNDEMA-EOF-OFF
               READ KUNDEMA
               AT END
                   SET KUNDEMA-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE KUNDEMA-IO-AREA (6:6)  TO KUNDE (1:6)
               MOVE KUNDEMA-IO-AREA (16:30) TO KNAVN1 (1:30)
               MOVE KUNDEMA-IO-AREA (46:30) TO KNAVN2 (1:30)
               MOVE KUNDEMA-IO-AREA (76:30) TO ADR (1:30)
               MOVE KUNDEMA-IO-AREA (106:15) TO POST (1:15)
               MOVE KUNDEMA-IO-AREA (121:4) TO POSTNR (1:4)
               MOVE KUNDEMA-IO-AREA (127:2) TO BETM (1:2)
               MOVE KUNDEMA-IO-AREA (164:1) TO IPURRE-IO
               INSPECT IPURRE-IO REPLACING ALL ' ' BY '0'
               MOVE KUNDEMA-IO-AREA (165:1) TO IRENTE (1:1)
               MOVE KUNDEMA-IO-AREA (185:3) TO HND (1:3)
               MOVE KUNDEMA-IO-AREA (188:1) TO ANTPUR-IO
               INSPECT ANTPUR-IO REPLACING ALL ' ' BY '0'
               MOVE KUNDEMA-IO-AREA (160:1) TO ANTINK-IO
               INSPECT ANTINK-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-04                        TO TRUE.
 
       KUNDEMA-CHK-LEVEL SECTION.
       KUNDEMA-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO KUNDEMA-LEVEL-04
               MOVE KUNDEMA-IO-AREA (3:3)  TO KUNDEMA-04-L2-FIRMA
               MOVE KUNDEMA-IO-AREA (6:6)  TO KUNDEMA-04-L1-KUNDE
               IF  KUNDEMA-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  KUNDEMA-04-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  KUNDEMA-04-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  KUNDEMA-04-L2         TO THE-PRIOR-L2
               MOVE  KUNDEMA-04-L1         TO THE-PRIOR-L1
               SET KUNDEMA-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       KUNDEMA-MATCH-SET SECTION.
       KUNDEMA-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (3:3)  TO KUNDEMA-M-04-M2-FIRMA
               MOVE KUNDEMA-IO-AREA (6:6)  TO KUNDEMA-M-04-M1-KUNDE
           END-EVALUATE.
 
       KUNDET-GET SECTION.
       KUNDET-GET-P.
           IF  KUNDET-EOF-OFF
               READ KUNDET
               AT END
                   SET KUNDET-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KUNDET-FLDSET SECTION.
       KUNDET-FLDSET-P.
           EVALUATE TRUE
           WHEN ( KUNDET-IO-AREA (1:1) = '3'
            AND   KUNDET-IO-AREA (2:1) = '1' )
               MOVE KUNDET-IO-AREA (3:3)   TO FIRMA (1:3)
               MOVE KUNDET-IO-AREA (6:6)   TO KUNDE (1:6)
               MOVE KUNDET-IO-AREA (12:9)  TO KUNDEB-IO
               INSPECT KUNDEB-IO REPLACING ALL ' ' BY '0'
               MOVE KUNDET-IO-AREA (22:9)  TO KUNDEA-IO
               INSPECT KUNDEA-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       KUNDET-IDCHK SECTION.
       KUNDET-IDCHK-P.
           EVALUATE TRUE
           WHEN ( KUNDET-IO-AREA (1:1) = '3'
            AND   KUNDET-IO-AREA (2:1) = '1' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       KUNDET-IDSET SECTION.
       KUNDET-IDSET-P.
           EVALUATE TRUE
           WHEN ( KUNDET-IO-AREA (1:1) = '3'
            AND   KUNDET-IO-AREA (2:1) = '1' )
               SET I-01                    TO TRUE
           END-EVALUATE.
 
       KUNDET-CHK-LEVEL SECTION.
       KUNDET-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( KUNDET-IO-AREA (1:1) = '3'
            AND   KUNDET-IO-AREA (2:1) = '1' )
               MOVE LOW-VALUES             TO KUNDET-LEVEL-01
               MOVE KUNDET-IO-AREA (3:3)   TO KUNDET-01-L2-FIRMA
               MOVE KUNDET-IO-AREA (6:6)   TO KUNDET-01-L1-KUNDE
               IF  KUNDET-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  KUNDET-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  KUNDET-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  KUNDET-01-L2          TO THE-PRIOR-L2
               MOVE  KUNDET-01-L1          TO THE-PRIOR-L1
               SET KUNDET-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       KUNDET-MATCH-SET SECTION.
       KUNDET-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( KUNDET-IO-AREA (1:1) = '3'
            AND   KUNDET-IO-AREA (2:1) = '1' )
               MOVE KUNDET-IO-AREA (3:3)   TO KUNDET-M-01-M2-FIRMA
               MOVE KUNDET-IO-AREA (6:6)   TO KUNDET-M-01-M1-KUNDE
           END-EVALUATE.
 
       RENTEGR-GET SECTION.
       RENTEGR-GET-P.
           IF  RENTEGR-EOF-OFF
               READ RENTEGR
               AT END
                   SET RENTEGR-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RENTEGR-FLDSET SECTION.
       RENTEGR-FLDSET-P.
           EVALUATE TRUE
           WHEN ( RENTEGR-IO-AREA (1:1) = '3'
            AND   RENTEGR-IO-AREA (2:1) = '1' )
               MOVE RENTEGR-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE RENTEGR-IO-AREA (6:6)  TO KUNDE (1:6)
               MOVE RENTEGR-IO-AREA (12:6) TO REFNR (1:6)
               MOVE RENTEGR-IO-AREA (20:2) TO BILA-ELGR (1:2)
               MOVE RENTEGR-IO-AREA (22:2) TO BILMND (1:2)
               MOVE RENTEGR-IO-AREA (24:2) TO BILDAG (1:2)
               MOVE RENTEGR-IO-AREA (26:6) TO BILNR (1:6)
               MOVE RENTEGR-IO-AREA (33:2) TO FORA-ELGR (1:2)
               MOVE RENTEGR-IO-AREA (35:2) TO FORMND (1:2)
               MOVE RENTEGR-IO-AREA (37:2) TO FORDAG (1:2)
               MOVE RENTEGR-IO-AREA (39:9) TO BELO-ELGP-IO
               INSPECT BELO-ELGP-IO REPLACING ALL ' ' BY '0'
               MOVE RENTEGR-IO-AREA (48:1) TO MERKE (1:1)
               MOVE RENTEGR-IO-AREA (49:1) TO MERKE2 (1:1)
               MOVE RENTEGR-IO-AREA (61:9) TO RBEL-IO
               INSPECT RBEL-IO REPLACING ALL ' ' BY '0'
               MOVE RENTEGR-IO-AREA (71:4) TO RDAGER-IO
               INSPECT RDAGER-IO REPLACING ALL ' ' BY '0'
               MOVE RENTEGR-IO-AREA (78:9) TO RBELAV-IO
               INSPECT RBELAV-IO REPLACING ALL ' ' BY '0'
               MOVE RENTEGR-IO-AREA (87:2) TO TRKODE (1:2)
           END-EVALUATE.
 
       RENTEGR-IDCHK SECTION.
       RENTEGR-IDCHK-P.
           EVALUATE TRUE
           WHEN ( RENTEGR-IO-AREA (1:1) = '3'
            AND   RENTEGR-IO-AREA (2:1) = '1' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       RENTEGR-IDSET SECTION.
       RENTEGR-IDSET-P.
           EVALUATE TRUE
           WHEN ( RENTEGR-IO-AREA (1:1) = '3'
            AND   RENTEGR-IO-AREA (2:1) = '1' )
               SET I-02                    TO TRUE
           END-EVALUATE.
 
       RENTEGR-CHK-LEVEL SECTION.
       RENTEGR-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( RENTEGR-IO-AREA (1:1) = '3'
            AND   RENTEGR-IO-AREA (2:1) = '1' )
               MOVE LOW-VALUES             TO RENTEGR-LEVEL-02
               MOVE RENTEGR-IO-AREA (3:3)  TO RENTEGR-02-L2-FIRMA
               MOVE RENTEGR-IO-AREA (6:6)  TO RENTEGR-02-L1-KUNDE
               IF  RENTEGR-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  RENTEGR-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  RENTEGR-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  RENTEGR-02-L2         TO THE-PRIOR-L2
               MOVE  RENTEGR-02-L1         TO THE-PRIOR-L1
               SET RENTEGR-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       RENTEGR-MATCH-SET SECTION.
       RENTEGR-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( RENTEGR-IO-AREA (1:1) = '3'
            AND   RENTEGR-IO-AREA (2:1) = '1' )
               MOVE RENTEGR-IO-AREA (3:3)  TO RENTEGR-M-02-M2-FIRMA
               MOVE RENTEGR-IO-AREA (6:6)  TO RENTEGR-M-02-M1-KUNDE
           END-EVALUATE.
 
       PURFAK-GET SECTION.
       PURFAK-GET-P.
           IF  PURFAK-EOF-OFF
               READ PURFAK
               AT END
                   SET PURFAK-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       PURFAK-FLDSET SECTION.
       PURFAK-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE PURFAK-IO-AREA (1:3)   TO FIRMA (1:3)
               MOVE PURFAK-IO-AREA (4:6)   TO KUNDE (1:6)
               MOVE PURFAK-IO-AREA (10:8)  TO FTEKST (1:8)
               MOVE PURFAK-IO-AREA (19:41) TO FNR61 (1:41)
               MOVE PURFAK-IO-AREA (61:41) TO FNR62 (1:41)
               MOVE PURFAK-IO-AREA (103:41) TO FNR63 (1:41)
               MOVE PURFAK-IO-AREA (150:1) TO ANTX6 (1:1)
           END-EVALUATE.
 
       PURFAK-IDSET SECTION.
       PURFAK-IDSET-P.
           SET I-07                        TO TRUE.
 
       PURFAK-CHK-LEVEL SECTION.
       PURFAK-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO PURFAK-LEVEL-07
               MOVE PURFAK-IO-AREA (1:3)   TO PURFAK-07-L2-FIRMA
               MOVE PURFAK-IO-AREA (4:6)   TO PURFAK-07-L1-KUNDE
               IF  PURFAK-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  PURFAK-07-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  PURFAK-07-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  PURFAK-07-L2          TO THE-PRIOR-L2
               MOVE  PURFAK-07-L1          TO THE-PRIOR-L1
               SET PURFAK-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       PURFAK-MATCH-SET SECTION.
       PURFAK-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE PURFAK-IO-AREA (1:3)   TO PURFAK-M-07-M2-FIRMA
               MOVE PURFAK-IO-AREA (4:6)   TO PURFAK-M-07-M1-KUNDE
           END-EVALUATE.
 
       MNDPAR-GET SECTION.
       MNDPAR-GET-P.
           IF  MNDPAR-EOF-OFF
               READ MNDPAR
               AT END
                   SET MNDPAR-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       MNDPAR-FLDSET SECTION.
       MNDPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE MNDPAR-IO-AREA (3:4)   TO A-ELGR (1:4)
               MOVE MNDPAR-IO-AREA (9:9)   TO MNDNVN (1:9)
           END-EVALUATE.
 
       MNDPAR-IDSET SECTION.
       MNDPAR-IDSET-P.
           SET I-05                        TO TRUE.
 
       RESPAR-GET SECTION.
       RESPAR-GET-P.
           IF  RESPAR-EOF-OFF
               READ RESPAR
               AT END
                   SET RESPAR-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RESPAR-FLDSET SECTION.
       RESPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RESPAR-IO-AREA (7:6)   TO PERDAT (1:6)
               MOVE RESPAR-IO-AREA (13:6)  TO PERIOD (1:6)
           END-EVALUATE.
 
       RESPAR-IDSET SECTION.
       RESPAR-IDSET-P.
           SET I-06                        TO TRUE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (140:5) TO RVGR (1:5)
               MOVE FIRMAF-IO-AREA (160:4) TO EDB-IO
               MOVE FIRMAF-IO-AREA (145:1) TO FTYPE (1:1)
               MOVE FIRMAF-IO-AREA (146:3) TO NOTAB-IO
               INSPECT NOTAB-IO REPLACING ALL ' ' BY '0'
               MOVE FIRMAF-IO-AREA (190:1) TO FPGEB (1:1)
               MOVE FIRMAF-IO-AREA (191:5) TO FPVGR (1:5)
               MOVE FIRMAF-IO-AREA (196:7) TO FPEDB (1:7)
               MOVE FIRMAF-IO-AREA (852:3) TO PGEBEL-IO
               INSPECT PGEBEL-IO REPLACING ALL ' ' BY '0'
               MOVE FIRMAF-IO-AREA (855:3) TO IGEBEL-IO
               INSPECT IGEBEL-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-03                        TO TRUE.
 
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
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  KUNDEMA-EOF
               MOVE HIGH-VALUES            TO KUNDEMA-MC
                                              KUNDEMA-MP
           END-IF
           IF  KUNDET-EOF
               MOVE HIGH-VALUES            TO KUNDET-MC
                                              KUNDET-MP
           END-IF
           IF  RENTEGR-EOF
               MOVE HIGH-VALUES            TO RENTEGR-MC
                                              RENTEGR-MP
           END-IF
           IF  PURFAK-EOF
               MOVE HIGH-VALUES            TO PURFAK-MC
                                              PURFAK-MP
           END-IF
           IF  KUNDEMA-MC < KUNDEMA-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  KUNDET-MC < KUNDET-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  RENTEGR-MC < RENTEGR-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  PURFAK-MC < PURFAK-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  KUNDEMA-MC < KUNDET-MC
            AND  KUNDEMA-MC < RENTEGR-MC
            AND  KUNDEMA-MC < PURFAK-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET KUNDEMA-PROCESS     TO TRUE
                   MOVE KUNDEMA-MC         TO KUNDEMA-MP
                   IF  KUNDEMA-MC = KUNDET-MP
                     OR  KUNDEMA-MC = RENTEGR-MP
                     OR  KUNDEMA-MC = PURFAK-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  KUNDET-MC < KUNDEMA-MC
            AND  KUNDET-MC < RENTEGR-MC
            AND  KUNDET-MC < PURFAK-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET KUNDET-PROCESS      TO TRUE
                   MOVE KUNDET-MC          TO KUNDET-MP
                   IF  KUNDET-MC = KUNDEMA-MP
                     OR  KUNDET-MC = RENTEGR-MP
                     OR  KUNDET-MC = PURFAK-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  RENTEGR-MC < KUNDEMA-MC
            AND  RENTEGR-MC < KUNDET-MC
            AND  RENTEGR-MC < PURFAK-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET RENTEGR-PROCESS     TO TRUE
                   MOVE RENTEGR-MC         TO RENTEGR-MP
                   IF  RENTEGR-MC = KUNDEMA-MP
                     OR  RENTEGR-MC = KUNDET-MP
                     OR  RENTEGR-MC = PURFAK-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  PURFAK-MC < KUNDEMA-MC
            AND  PURFAK-MC < KUNDET-MC
            AND  PURFAK-MC < RENTEGR-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET PURFAK-PROCESS      TO TRUE
                   MOVE PURFAK-MC          TO PURFAK-MP
                   IF  PURFAK-MC = KUNDEMA-MP
                     OR  PURFAK-MC = KUNDET-MP
                     OR  PURFAK-MC = RENTEGR-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  KUNDEMA-MC = KUNDET-MC
             OR  KUNDEMA-MC = RENTEGR-MC
             OR  KUNDEMA-MC = PURFAK-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET KUNDEMA-PROCESS     TO TRUE
                   MOVE KUNDEMA-MC         TO KUNDEMA-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           WHEN  KUNDET-MC = RENTEGR-MC
             OR  KUNDET-MC = PURFAK-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET KUNDET-PROCESS      TO TRUE
                   MOVE KUNDET-MC          TO KUNDET-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           WHEN  RENTEGR-MC = PURFAK-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET RENTEGR-PROCESS     TO TRUE
                   MOVE RENTEGR-MC         TO RENTEGR-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND I-30 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               IF  (I-50)
                   MOVE KUNDE              TO LISTE-IO-AREA (1:6)
               END-IF
               IF  (I-50)
                   MOVE KNAVN1             TO LISTE-IO-AREA (8:30)
               END-IF
               IF  (I-51)
                   MOVE KNAVN2             TO LISTE-IO-AREA (8:30)
               END-IF
               IF  (I-52)
                   MOVE ADR                TO LISTE-IO-AREA (8:30)
               END-IF
               IF  (I-53)
                   MOVE POSTNR             TO LISTE-IO-AREA (8:4)
               END-IF
               IF  (I-53)
                   MOVE POST               TO LISTE-IO-AREA (13:15)
               END-IF
               MOVE TEKST                  TO LISTE-IO-AREA (40:7)
               MOVE REFNR                  TO LISTE-IO-AREA (51:6)
               MOVE FORDAG                 TO LISTE-IO-AREA (58:2)
               MOVE '.'                    TO LISTE-IO-AREA (60:1)
               MOVE FORMND                 TO LISTE-IO-AREA (61:2)
               MOVE '.'                    TO LISTE-IO-AREA (63:1)
               MOVE FORA-ELGR              TO LISTE-IO-AREA (64:2)
               MOVE BELO-ELGP              TO XO-72YNZR
               MOVE XO-72YNZR              TO LISTE-IO-AREA (67:11)
               MOVE BILNR                  TO LISTE-IO-AREA (80:6)
               MOVE BILDAG                 TO LISTE-IO-AREA (87:2)
               MOVE '.'                    TO LISTE-IO-AREA (89:1)
               MOVE BILMND                 TO LISTE-IO-AREA (90:2)
               MOVE '.'                    TO LISTE-IO-AREA (92:1)
               MOVE BILA-ELGR              TO LISTE-IO-AREA (93:2)
               MOVE RDAGER                 TO XO-40YYZ
               MOVE XO-40YYZ               TO LISTE-IO-AREA (97:5)
               IF  (NOT-I-21)
                   MOVE RBEL               TO XO-72YNZR
                   MOVE XO-72YNZR          TO LISTE-IO-AREA (102:11)
               END-IF
               IF  (I-21)
                   MOVE RBELAV             TO XO-72YNZR
                   MOVE XO-72YNZR          TO LISTE-IO-AREA (102:11)
               END-IF
               IF  (I-21 AND I-50)
                   MOVE '* AVVIKENDE RENTE ' TO LISTE-IO-AREA (115:18)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR AND I-03 AND I-10)
           OR  (I-60 AND I-98 AND I-U8)
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
               MOVE TKTK                   TO LISTE-IO-AREA (119:2)
               MOVE TKBILN                 TO LISTE-IO-AREA (115:6)
               MOVE TKREFN                 TO LISTE-IO-AREA (115:6)
               MOVE BPCLAS                 TO LISTE-IO-AREA (120:1)
               MOVE BPRJE                  TO LISTE-IO-AREA (118:3)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-01 AND I-15)
               MOVE SPACES TO FAKTUT-IO-AREA
               INITIALIZE FAKTUT-IO-AREA
               MOVE FIRMA                  TO FAKTUT-IO-AREA (1:3)
               MOVE KUNDE                  TO FAKTUT-IO-AREA (4:6)
               MOVE 'F'                    TO FAKTUT-IO-AREA (10:1)
               MOVE FAKART                 TO FAKTUT-IO-AREA (11:1)
               MOVE 'F'                    TO FAKTUT-IO-AREA (13:1)
               MOVE BETM                   TO FAKTUT-IO-AREA (14:2)
               MOVE PERIOD                 TO FAKTUT-IO-AREA (19:6)
               MOVE SEQNR1-IO              TO FAKTUT-IO-AREA (26:4)
               MOVE 'A'                    TO FAKTUT-IO-AREA (25:1)
               IF  (NOT-I-49)
                   MOVE 'R E N T E N O T A ' TO FAKTUT-IO-AREA (46:18)
               END-IF
               IF  (I-49)
                   MOVE 'RENTE OG PURREGEBYR ' TO FAKTUT-IO-AREA
                                                               (47:20)
               END-IF
               MOVE PERDAT                 TO FAKTUT-IO-AREA (71:6)
               IF  (NOT-I-49)
                   MOVE 'FOR INNBETALINGER I' TO FAKTUT-IO-AREA (77:19)
               END-IF
               IF  (I-49)
                   MOVE 'INNBET. OG PURRET I' TO FAKTUT-IO-AREA (77:19)
               END-IF
               MOVE MNDNVN                 TO FAKTUT-IO-AREA (97:9)
               MOVE NULL7                  TO XO-52P
               MOVE XO-52P-EF              TO FAKTUT-IO-AREA (133:4)
               MOVE NULL7                  TO XO-52P
               MOVE XO-52P-EF              TO FAKTUT-IO-AREA (137:4)
               MOVE '1'                    TO FAKTUT-IO-AREA (166:1)
               MOVE NULL9                  TO XO-72P
               MOVE XO-72P-EF              TO FAKTUT-IO-AREA (171:5)
               MOVE 'R'                    TO FAKTUT-IO-AREA (177:1)
               MOVE 'N'                    TO FAKTUT-IO-AREA (178:1)
               MOVE '1'                    TO FAKTUT-IO-AREA (181:1)
               MOVE FTYPE                  TO FAKTUT-IO-AREA (182:1)
               MOVE KUNDE                  TO FAKTUT-IO-AREA (184:6)
               WRITE FAKTUT-IO-AREA
               MOVE SPACES TO FAKTUT-IO-AREA
               INITIALIZE FAKTUT-IO-AREA
               MOVE FIRMA                  TO FAKTUT-IO-AREA (1:3)
               MOVE KUNDE                  TO FAKTUT-IO-AREA (4:6)
               MOVE 'F'                    TO FAKTUT-IO-AREA (10:1)
               MOVE FAKART                 TO FAKTUT-IO-AREA (11:1)
               MOVE 'F'                    TO FAKTUT-IO-AREA (13:1)
               MOVE BETM                   TO FAKTUT-IO-AREA (14:2)
               MOVE PERIOD                 TO FAKTUT-IO-AREA (19:6)
               MOVE 'L'                    TO FAKTUT-IO-AREA (25:1)
               MOVE SEQNR2-IO              TO FAKTUT-IO-AREA (26:4)
               MOVE '5'                    TO FAKTUT-IO-AREA (67:1)
               MOVE HND                    TO FAKTUT-IO-AREA (68:3)
               MOVE PERDAT                 TO FAKTUT-IO-AREA (71:6)
               MOVE 'VÅR FAKT.   FORFALT ' TO FAKTUT-IO-AREA (82:20)
               MOVE 'INNBETALT      BELØP' TO FAKTUT-IO-AREA (102:20)
               MOVE 'REF.NR.'              TO FAKTUT-IO-AREA (125:7)
               MOVE NULL7                  TO XO-52P
               MOVE XO-52P-EF              TO FAKTUT-IO-AREA (133:4)
               MOVE NULL7                  TO XO-52P
               MOVE XO-52P-EF              TO FAKTUT-IO-AREA (137:4)
               MOVE '1'                    TO FAKTUT-IO-AREA (166:1)
               MOVE NULL9                  TO XO-72P
               MOVE XO-72P-EF              TO FAKTUT-IO-AREA (171:5)
               MOVE 'R'                    TO FAKTUT-IO-AREA (177:1)
               MOVE 'N'                    TO FAKTUT-IO-AREA (178:1)
               MOVE '1'                    TO FAKTUT-IO-AREA (181:1)
               MOVE FTYPE                  TO FAKTUT-IO-AREA (182:1)
               MOVE KUNDE                  TO FAKTUT-IO-AREA (184:6)
               WRITE FAKTUT-IO-AREA
           END-IF
           IF  (I-02 AND I-30 AND I-15)
               MOVE SPACES TO FAKTUT-IO-AREA
               INITIALIZE FAKTUT-IO-AREA
               MOVE FIRMA                  TO FAKTUT-IO-AREA (1:3)
               MOVE KUNDE                  TO FAKTUT-IO-AREA (4:6)
               MOVE 'F'                    TO FAKTUT-IO-AREA (10:1)
               MOVE FAKART                 TO FAKTUT-IO-AREA (11:1)
               MOVE 'F'                    TO FAKTUT-IO-AREA (13:1)
               MOVE BETM                   TO FAKTUT-IO-AREA (14:2)
               MOVE PERIOD                 TO FAKTUT-IO-AREA (19:6)
               MOVE 'L'                    TO FAKTUT-IO-AREA (25:1)
               MOVE SEQNR-IO               TO FAKTUT-IO-AREA (26:4)
               MOVE '5'                    TO FAKTUT-IO-AREA (67:1)
               MOVE HND                    TO FAKTUT-IO-AREA (68:3)
               MOVE PERDAT                 TO FAKTUT-IO-AREA (71:6)
               MOVE RVGR                   TO FAKTUT-IO-AREA (77:5)
               MOVE REFNR                  TO FAKTUT-IO-AREA (85:6)
               MOVE FORDAG                 TO FAKTUT-IO-AREA (93:2)
               MOVE '.'                    TO FAKTUT-IO-AREA (95:1)
               MOVE FORMND                 TO FAKTUT-IO-AREA (96:2)
               MOVE '.'                    TO FAKTUT-IO-AREA (98:1)
               MOVE FORA-ELGR              TO FAKTUT-IO-AREA (99:2)
               MOVE BILDAG                 TO FAKTUT-IO-AREA (102:2)
               MOVE '.'                    TO FAKTUT-IO-AREA (104:1)
               MOVE BILMND                 TO FAKTUT-IO-AREA (105:2)
               MOVE '.'                    TO FAKTUT-IO-AREA (107:1)
               MOVE BILA-ELGR              TO FAKTUT-IO-AREA (108:2)
               MOVE BELO-ELGP              TO XO-72YNZR
               MOVE XO-72YNZR              TO FAKTUT-IO-AREA (112:11)
               MOVE BILNR                  TO FAKTUT-IO-AREA (125:6)
               MOVE NULL7                  TO XO-52P
               MOVE XO-52P-EF              TO FAKTUT-IO-AREA (133:4)
               MOVE NULL7                  TO XO-52P
               MOVE XO-52P-EF              TO FAKTUT-IO-AREA (137:4)
               MOVE EDB                    TO XO-70U
               MOVE XO-70U (1:7)           TO FAKTUT-IO-AREA (141:7)
               IF  (NOT-I-21)
                   MOVE RBEL-IO            TO FAKTUT-IO-AREA (157:9)
               END-IF
               IF  (I-21)
                   MOVE RBELAV-IO          TO FAKTUT-IO-AREA (157:9)
               END-IF
               MOVE '1'                    TO FAKTUT-IO-AREA (166:1)
               MOVE NULL9                  TO XO-72P
               MOVE XO-72P-EF              TO FAKTUT-IO-AREA (171:5)
               MOVE 'R'                    TO FAKTUT-IO-AREA (177:1)
               MOVE 'N'                    TO FAKTUT-IO-AREA (178:1)
               MOVE '1'                    TO FAKTUT-IO-AREA (181:1)
               MOVE FTYPE                  TO FAKTUT-IO-AREA (182:1)
               MOVE KUNDE                  TO FAKTUT-IO-AREA (184:6)
               WRITE FAKTUT-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-80 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE 'R E N T E O P P G A V E' TO LISTE-IO-AREA (40:23)
               MOVE 'FOR'                  TO LISTE-IO-AREA (69:3)
               MOVE MNDNVN                 TO LISTE-IO-AREA (73:9)
               MOVE A-ELGR                 TO LISTE-IO-AREA (83:4)
               MOVE 'FRAMSTILT'            TO LISTE-IO-AREA (99:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (109:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (120:4)
               IF  (I-80)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40U
               MOVE XO-40U (1:4)           TO LISTE-IO-AREA (124:4)
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
               MOVE '---------------'      TO LISTE-IO-AREA (118:15)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KUNDE  KUNDENAVN OG'  TO LISTE-IO-AREA (1:19)
               MOVE 'ADRESSE'              TO LISTE-IO-AREA (21:7)
               MOVE '---- GRUNNLAG FOR RENTE' TO LISTE-IO-AREA (40:23)
               MOVE 'BEREGNING ----   - INN' TO LISTE-IO-AREA (63:22)
               MOVE 'BETALING -  RENTE'    TO LISTE-IO-AREA (85:17)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'NR.'                  TO LISTE-IO-AREA (1:3)
               MOVE 'BILAGSART  BILAG  FORFAL' TO LISTE-IO-AREA (40:24)
               MOVE 'L       BELØP   BILAG' TO LISTE-IO-AREA (64:21)
               MOVE 'DATO    DAGER    RENTE' TO LISTE-IO-AREA (89:22)
               MOVE 'ANMERKNINGER.'        TO LISTE-IO-AREA (116:13)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '---------------'      TO LISTE-IO-AREA (118:15)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OV AND NOT-I-80 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE 'R E N T E O P P G A V E' TO LISTE-IO-AREA (40:23)
               MOVE 'FOR'                  TO LISTE-IO-AREA (69:3)
               MOVE MNDNVN                 TO LISTE-IO-AREA (73:9)
               MOVE A-ELGR                 TO LISTE-IO-AREA (83:4)
               MOVE 'FRAMSTILT'            TO LISTE-IO-AREA (99:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (109:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (120:4)
               IF  (I-80)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40U
               MOVE XO-40U (1:4)           TO LISTE-IO-AREA (124:4)
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
               MOVE '---------------'      TO LISTE-IO-AREA (118:15)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KUNDE  KUNDENAVN OG'  TO LISTE-IO-AREA (1:19)
               MOVE 'ADRESSE'              TO LISTE-IO-AREA (21:7)
               MOVE '---- GRUNNLAG FOR RENTE' TO LISTE-IO-AREA (40:23)
               MOVE 'BEREGNING ----   - INN' TO LISTE-IO-AREA (63:22)
               MOVE 'BETALING -  RENTE'    TO LISTE-IO-AREA (85:17)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'NR.'                  TO LISTE-IO-AREA (1:3)
               MOVE 'BILAGSART  BILAG  FORFAL' TO LISTE-IO-AREA (40:24)
               MOVE 'L       BELØP   BILAG' TO LISTE-IO-AREA (64:21)
               MOVE 'DATO    DAGER    RENTE' TO LISTE-IO-AREA (89:22)
               MOVE 'ANMERKNINGER.'        TO LISTE-IO-AREA (116:13)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '---------------'      TO LISTE-IO-AREA (118:15)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-49 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               IF  (I-61)
                   MOVE KUNDE              TO LISTE-IO-AREA (1:6)
               END-IF
               IF  (I-61)
                   MOVE KNAVN1             TO LISTE-IO-AREA (8:30)
               END-IF
               IF  (I-50)
                   MOVE KNAVN2             TO LISTE-IO-AREA (8:30)
               END-IF
               IF  (I-51)
                   MOVE ADR                TO LISTE-IO-AREA (8:30)
               END-IF
               IF  (I-52)
                   MOVE POSTNR             TO LISTE-IO-AREA (8:4)
               END-IF
               IF  (I-52)
                   MOVE POST               TO LISTE-IO-AREA (13:15)
               END-IF
               MOVE ANTPUR-IO              TO LISTE-IO-AREA (41:1)
               MOVE 'STK. PURREGEBYR.'     TO LISTE-IO-AREA (43:16)
               MOVE ANTINK-IO              TO LISTE-IO-AREA (61:1)
               MOVE 'STK. INK.GEBYR.'      TO LISTE-IO-AREA (63:15)
               MOVE SBELK                  TO XO-72YNZR
               MOVE XO-72YNZR              TO LISTE-IO-AREA (102:11)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L1 AND I-50 AND I-17)
           AND (NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               IF  (NOT-I-49)
                   MOVE KNAVN2             TO LISTE-IO-AREA (8:30)
               END-IF
               IF  (I-49)
                   MOVE ADR                TO LISTE-IO-AREA (8:30)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L1 AND I-51 AND I-17)
           AND (NOT-I-86)
           OR  (I-L1 AND I-50 AND I-17)
           AND (NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               IF  (NOT-I-49)
                   MOVE ADR                TO LISTE-IO-AREA (8:30)
               END-IF
               IF  (I-49)
                   MOVE POSTNR             TO LISTE-IO-AREA (8:4)
               END-IF
               IF  (I-49)
                   MOVE POST               TO LISTE-IO-AREA (13:15)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L1 AND I-52 AND I-17)
           AND (NOT-I-49 AND NOT-I-86)
           OR  (I-L1 AND I-50 AND I-17)
           AND (NOT-I-49 AND NOT-I-86)
           OR  (I-L1 AND I-51 AND I-17)
           AND (NOT-I-49 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE POSTNR                 TO LISTE-IO-AREA (8:4)
               MOVE POST                   TO LISTE-IO-AREA (13:15)
               MOVE 'SUM RENTENOTA'        TO LISTE-IO-AREA (87:13)
               IF  (NOT-I-21)
                   MOVE KUNDEB             TO XO-72YNZR
                   MOVE XO-72YNZR          TO LISTE-IO-AREA (102:11)
               END-IF
               IF  (I-21)
                   MOVE KUNDEA             TO XO-72YNZR
                   MOVE XO-72YNZR          TO LISTE-IO-AREA (102:11)
               END-IF
               MOVE '*'                    TO LISTE-IO-AREA (113:1)
               MOVE BELATX                 TO LISTE-IO-AREA (115:18)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L1 AND I-53 AND I-17)
           OR  (I-L1 AND I-55 AND I-17)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'SUM RENTENOTA'        TO LISTE-IO-AREA (87:13)
               IF  (NOT-I-21)
                   MOVE KUNDEB             TO XO-72YNZR
                   MOVE XO-72YNZR          TO LISTE-IO-AREA (102:11)
               END-IF
               IF  (I-21)
                   MOVE KUNDEA             TO XO-72YNZR
                   MOVE XO-72YNZR          TO LISTE-IO-AREA (102:11)
               END-IF
               MOVE '*'                    TO LISTE-IO-AREA (113:1)
               MOVE BELATX                 TO LISTE-IO-AREA (115:18)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L2 AND I-81 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMATOTAL RENTENOTA SOM' TO LISTE-IO-AREA (62:24)
               MOVE 'IKKE FAKT.   '        TO LISTE-IO-AREA (87:13)
               MOVE IKKERB                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (99:14)
               MOVE '**'                   TO LISTE-IO-AREA (113:2)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMATOTAL RENTENOTA    ' TO LISTE-IO-AREA (62:24)
               MOVE 'TIL FAKT.    '        TO LISTE-IO-AREA (87:13)
               MOVE FIRBEL                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (99:14)
               MOVE '**'                   TO LISTE-IO-AREA (113:2)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMATOTAL PURREGEBYR   ' TO LISTE-IO-AREA (62:24)
               MOVE 'TIL FAKT.    '        TO LISTE-IO-AREA (87:13)
               MOVE PBELF                  TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (100:13)
               MOVE '**'                   TO LISTE-IO-AREA (113:2)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMATOTAL INKASSOGEBYR ' TO LISTE-IO-AREA (62:24)
               MOVE 'TIL FAKT.    '        TO LISTE-IO-AREA (87:13)
               MOVE IBELF                  TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (100:13)
               MOVE '**'                   TO LISTE-IO-AREA (113:2)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'GRANDTOTAL RENTENOTA SOM' TO LISTE-IO-AREA (62:24)
               MOVE 'IKKE BEL.    '        TO LISTE-IO-AREA (87:13)
               MOVE IKKEGT                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (98:15)
               MOVE '***'                  TO LISTE-IO-AREA (113:3)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'GRANDTOTAL RENTENOTA    ' TO LISTE-IO-AREA (62:24)
               MOVE 'TIL FAKT.    '        TO LISTE-IO-AREA (87:13)
               MOVE FIRBGT                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (98:15)
               MOVE '***'                  TO LISTE-IO-AREA (113:3)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'GRANDTOTAL PURREGEBYR   ' TO LISTE-IO-AREA (62:24)
               MOVE 'TIL FAKT.    '        TO LISTE-IO-AREA (87:13)
               MOVE PBELGT                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (98:15)
               MOVE '***'                  TO LISTE-IO-AREA (113:3)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'GRANDTOTAL INKASSOGEBYR ' TO LISTE-IO-AREA (62:24)
               MOVE 'TIL FAKT.    '        TO LISTE-IO-AREA (87:13)
               MOVE IBELGT                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (98:15)
               MOVE '***'                  TO LISTE-IO-AREA (113:3)
      * DUMMY-LINJE FOR Å FJERNE KOMPILERINGSFEIL
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L1 AND I-49 AND NOT-I-15)
               MOVE SPACES TO FAKTUT-IO-AREA
               INITIALIZE FAKTUT-IO-AREA
               MOVE FIRMA                  TO FAKTUT-IO-AREA (1:3)
               MOVE KUNDE                  TO FAKTUT-IO-AREA (4:6)
               MOVE 'F'                    TO FAKTUT-IO-AREA (10:1)
               MOVE FAKART                 TO FAKTUT-IO-AREA (11:1)
               MOVE 'F'                    TO FAKTUT-IO-AREA (13:1)
               MOVE BETM                   TO FAKTUT-IO-AREA (14:2)
               MOVE PERIOD                 TO FAKTUT-IO-AREA (19:6)
               MOVE 'A'                    TO FAKTUT-IO-AREA (25:1)
               MOVE SEQNT1-IO              TO FAKTUT-IO-AREA (26:4)
               MOVE 'P U R R E G E B Y R ' TO FAKTUT-IO-AREA (47:20)
               MOVE PERDAT                 TO FAKTUT-IO-AREA (71:6)
               MOVE '   VARSEL UTSENDT I'  TO FAKTUT-IO-AREA (77:19)
               MOVE MNDNVN                 TO FAKTUT-IO-AREA (97:9)
               MOVE NULL7                  TO XO-52P
               MOVE XO-52P-EF              TO FAKTUT-IO-AREA (133:4)
               MOVE NULL7                  TO XO-52P
               MOVE XO-52P-EF              TO FAKTUT-IO-AREA (137:4)
               MOVE '1'                    TO FAKTUT-IO-AREA (166:1)
               MOVE NULL9                  TO XO-72P
               MOVE XO-72P-EF              TO FAKTUT-IO-AREA (171:5)
               MOVE 'R'                    TO FAKTUT-IO-AREA (177:1)
               MOVE 'N'                    TO FAKTUT-IO-AREA (178:1)
               MOVE '1'                    TO FAKTUT-IO-AREA (181:1)
               MOVE FTYPE                  TO FAKTUT-IO-AREA (182:1)
               MOVE KUNDE                  TO FAKTUT-IO-AREA (184:6)
               WRITE FAKTUT-IO-AREA
           END-IF
           IF  (I-L1 AND I-41)
               MOVE SPACES TO FAKTUT-IO-AREA
               INITIALIZE FAKTUT-IO-AREA
               MOVE FIRMA                  TO FAKTUT-IO-AREA (1:3)
               MOVE KUNDE                  TO FAKTUT-IO-AREA (4:6)
               MOVE 'F'                    TO FAKTUT-IO-AREA (10:1)
               MOVE FAKART                 TO FAKTUT-IO-AREA (11:1)
               MOVE 'F'                    TO FAKTUT-IO-AREA (13:1)
               MOVE BETM                   TO FAKTUT-IO-AREA (14:2)
               MOVE PERIOD                 TO FAKTUT-IO-AREA (19:6)
               MOVE 'L'                    TO FAKTUT-IO-AREA (25:1)
               MOVE SEQNT2-IO              TO FAKTUT-IO-AREA (26:4)
               MOVE '5'                    TO FAKTUT-IO-AREA (67:1)
               MOVE HND                    TO FAKTUT-IO-AREA (68:3)
               MOVE PERDAT                 TO FAKTUT-IO-AREA (71:6)
               MOVE FPVGR                  TO FAKTUT-IO-AREA (77:5)
               MOVE 'STK. GEBYR FOR '      TO FAKTUT-IO-AREA (82:15)
               MOVE 'PURREBREV I'          TO FAKTUT-IO-AREA (102:11)
               MOVE MNDNVN                 TO FAKTUT-IO-AREA (114:9)
               MOVE ANTPUF                 TO XO-52P
               MOVE XO-52P-EF              TO FAKTUT-IO-AREA (133:4)
               MOVE ANTPUF                 TO XO-52P
               MOVE XO-52P-EF              TO FAKTUT-IO-AREA (137:4)
               MOVE FPEDB                  TO FAKTUT-IO-AREA (141:7)
               MOVE PBEL-IO                TO FAKTUT-IO-AREA (157:9)
               MOVE '1'                    TO FAKTUT-IO-AREA (166:1)
               MOVE NULL9                  TO XO-72P
               MOVE XO-72P-EF              TO FAKTUT-IO-AREA (171:5)
               MOVE 'R'                    TO FAKTUT-IO-AREA (177:1)
               MOVE 'N'                    TO FAKTUT-IO-AREA (178:1)
               MOVE '1'                    TO FAKTUT-IO-AREA (181:1)
               MOVE FTYPE                  TO FAKTUT-IO-AREA (182:1)
               MOVE KUNDE                  TO FAKTUT-IO-AREA (184:6)
               WRITE FAKTUT-IO-AREA
           END-IF
           IF  (I-L1 AND I-41 AND I-91)
               MOVE SPACES TO FAKTUT-IO-AREA
               INITIALIZE FAKTUT-IO-AREA
               MOVE FIRMA                  TO FAKTUT-IO-AREA (1:3)
               MOVE KUNDE                  TO FAKTUT-IO-AREA (4:6)
               MOVE 'F'                    TO FAKTUT-IO-AREA (10:1)
               MOVE FAKART                 TO FAKTUT-IO-AREA (11:1)
               MOVE 'F'                    TO FAKTUT-IO-AREA (13:1)
               MOVE BETM                   TO FAKTUT-IO-AREA (14:2)
               MOVE PERIOD                 TO FAKTUT-IO-AREA (19:6)
               MOVE 'L'                    TO FAKTUT-IO-AREA (25:1)
               MOVE SEQNT4-IO              TO FAKTUT-IO-AREA (26:4)
               MOVE ' '                    TO FAKTUT-IO-AREA (37:1)
               MOVE '5'                    TO FAKTUT-IO-AREA (67:1)
               MOVE PERDAT                 TO FAKTUT-IO-AREA (71:6)
               MOVE FTEKST                 TO FAKTUT-IO-AREA (82:8)
               MOVE FNR61                  TO FAKTUT-IO-AREA (90:41)
               MOVE NULL7                  TO XO-52P
               MOVE XO-52P-EF              TO FAKTUT-IO-AREA (133:4)
               MOVE NULL7                  TO XO-52P
               MOVE XO-52P-EF              TO FAKTUT-IO-AREA (137:4)
               MOVE '1'                    TO FAKTUT-IO-AREA (166:1)
               MOVE NULL9                  TO XO-72P
               MOVE XO-72P-EF              TO FAKTUT-IO-AREA (171:5)
               MOVE 'R'                    TO FAKTUT-IO-AREA (177:1)
               MOVE 'N'                    TO FAKTUT-IO-AREA (178:1)
               MOVE '1'                    TO FAKTUT-IO-AREA (181:1)
               MOVE FTYPE                  TO FAKTUT-IO-AREA (182:1)
               MOVE KUNDE                  TO FAKTUT-IO-AREA (184:6)
               WRITE FAKTUT-IO-AREA
           END-IF
           IF  (I-L1 AND I-41 AND I-93)
               MOVE SPACES TO FAKTUT-IO-AREA
               INITIALIZE FAKTUT-IO-AREA
               MOVE FIRMA                  TO FAKTUT-IO-AREA (1:3)
               MOVE KUNDE                  TO FAKTUT-IO-AREA (4:6)
               MOVE 'F'                    TO FAKTUT-IO-AREA (10:1)
               MOVE FAKART                 TO FAKTUT-IO-AREA (11:1)
               MOVE 'F'                    TO FAKTUT-IO-AREA (13:1)
               MOVE BETM                   TO FAKTUT-IO-AREA (14:2)
               MOVE PERIOD                 TO FAKTUT-IO-AREA (19:6)
               MOVE 'L'                    TO FAKTUT-IO-AREA (25:1)
               MOVE SEQNT5-IO              TO FAKTUT-IO-AREA (26:4)
               MOVE ' '                    TO FAKTUT-IO-AREA (37:1)
               MOVE '5'                    TO FAKTUT-IO-AREA (67:1)
               MOVE PERDAT                 TO FAKTUT-IO-AREA (71:6)
               MOVE FTEKST                 TO FAKTUT-IO-AREA (82:8)
               MOVE FNR62                  TO FAKTUT-IO-AREA (90:41)
               MOVE NULL7                  TO XO-52P
               MOVE XO-52P-EF              TO FAKTUT-IO-AREA (133:4)
               MOVE NULL7                  TO XO-52P
               MOVE XO-52P-EF              TO FAKTUT-IO-AREA (137:4)
               MOVE '1'                    TO FAKTUT-IO-AREA (166:1)
               MOVE NULL9                  TO XO-72P
               MOVE XO-72P-EF              TO FAKTUT-IO-AREA (171:5)
               MOVE 'R'                    TO FAKTUT-IO-AREA (177:1)
               MOVE 'N'                    TO FAKTUT-IO-AREA (178:1)
               MOVE '1'                    TO FAKTUT-IO-AREA (181:1)
               MOVE FTYPE                  TO FAKTUT-IO-AREA (182:1)
               MOVE KUNDE                  TO FAKTUT-IO-AREA (184:6)
               WRITE FAKTUT-IO-AREA
           END-IF
           IF  (I-L1 AND I-41 AND I-94)
               MOVE SPACES TO FAKTUT-IO-AREA
               INITIALIZE FAKTUT-IO-AREA
               MOVE FIRMA                  TO FAKTUT-IO-AREA (1:3)
               MOVE KUNDE                  TO FAKTUT-IO-AREA (4:6)
               MOVE 'F'                    TO FAKTUT-IO-AREA (10:1)
               MOVE FAKART                 TO FAKTUT-IO-AREA (11:1)
               MOVE 'F'                    TO FAKTUT-IO-AREA (13:1)
               MOVE BETM                   TO FAKTUT-IO-AREA (14:2)
               MOVE PERIOD                 TO FAKTUT-IO-AREA (19:6)
               MOVE 'L'                    TO FAKTUT-IO-AREA (25:1)
               MOVE SEQNT6-IO              TO FAKTUT-IO-AREA (26:4)
               MOVE ' '                    TO FAKTUT-IO-AREA (37:1)
               MOVE '5'                    TO FAKTUT-IO-AREA (67:1)
               MOVE PERDAT                 TO FAKTUT-IO-AREA (71:6)
               MOVE FTEKST                 TO FAKTUT-IO-AREA (82:8)
               MOVE FNR63                  TO FAKTUT-IO-AREA (90:41)
               MOVE NULL7                  TO XO-52P
               MOVE XO-52P-EF              TO FAKTUT-IO-AREA (133:4)
               MOVE NULL7                  TO XO-52P
               MOVE XO-52P-EF              TO FAKTUT-IO-AREA (137:4)
               MOVE '1'                    TO FAKTUT-IO-AREA (166:1)
               MOVE NULL9                  TO XO-72P
               MOVE XO-72P-EF              TO FAKTUT-IO-AREA (171:5)
               MOVE 'R'                    TO FAKTUT-IO-AREA (177:1)
               MOVE 'N'                    TO FAKTUT-IO-AREA (178:1)
               MOVE '1'                    TO FAKTUT-IO-AREA (181:1)
               MOVE FTYPE                  TO FAKTUT-IO-AREA (182:1)
               MOVE KUNDE                  TO FAKTUT-IO-AREA (184:6)
               WRITE FAKTUT-IO-AREA
           END-IF
           IF  (I-L1 AND I-43)
               MOVE SPACES TO FAKTUT-IO-AREA
               INITIALIZE FAKTUT-IO-AREA
               MOVE FIRMA                  TO FAKTUT-IO-AREA (1:3)
               MOVE KUNDE                  TO FAKTUT-IO-AREA (4:6)
               MOVE 'F'                    TO FAKTUT-IO-AREA (10:1)
               MOVE FAKART                 TO FAKTUT-IO-AREA (11:1)
               MOVE 'F'                    TO FAKTUT-IO-AREA (13:1)
               MOVE BETM                   TO FAKTUT-IO-AREA (14:2)
               MOVE PERIOD                 TO FAKTUT-IO-AREA (19:6)
               MOVE 'L'                    TO FAKTUT-IO-AREA (25:1)
               MOVE SEQNT3-IO              TO FAKTUT-IO-AREA (26:4)
               MOVE '5'                    TO FAKTUT-IO-AREA (67:1)
               MOVE HND                    TO FAKTUT-IO-AREA (68:3)
               MOVE PERDAT                 TO FAKTUT-IO-AREA (71:6)
               MOVE FPVGR                  TO FAKTUT-IO-AREA (77:5)
               MOVE 'STK. GEBYR FOR BET'   TO FAKTUT-IO-AREA (82:18)
               MOVE 'ALINGSOPPFORDRING I'  TO FAKTUT-IO-AREA (103:19)
               MOVE MNDNVN                 TO FAKTUT-IO-AREA (123:9)
               MOVE ANTIKF                 TO XO-52P
               MOVE XO-52P-EF              TO FAKTUT-IO-AREA (133:4)
               MOVE ANTIKF                 TO XO-52P
               MOVE XO-52P-EF              TO FAKTUT-IO-AREA (137:4)
               MOVE FPEDB                  TO FAKTUT-IO-AREA (141:7)
               MOVE IBEL-IO                TO FAKTUT-IO-AREA (157:9)
               MOVE '1'                    TO FAKTUT-IO-AREA (166:1)
               MOVE NULL9                  TO XO-72P
               MOVE XO-72P-EF              TO FAKTUT-IO-AREA (171:5)
               MOVE 'R'                    TO FAKTUT-IO-AREA (177:1)
               MOVE 'N'                    TO FAKTUT-IO-AREA (178:1)
               MOVE '1'                    TO FAKTUT-IO-AREA (181:1)
               MOVE FTYPE                  TO FAKTUT-IO-AREA (182:1)
               MOVE KUNDE                  TO FAKTUT-IO-AREA (184:6)
               WRITE FAKTUT-IO-AREA
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
           MOVE 6                          TO LR-CHECK
           SET KUNDEMA-LEVEL-INIT          TO TRUE
           INITIALIZE KUNDEMA-DATA-FIELDS
           SET KUNDEMA-EOF-OFF             TO TRUE
           SET KUNDEMA-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO KUNDEMA-MC
                                              KUNDEMA-MP
           OPEN INPUT KUNDEMA
           SET KUNDET-LEVEL-INIT           TO TRUE
           INITIALIZE KUNDET-DATA-FIELDS
           SET KUNDET-EOF-OFF              TO TRUE
           SET KUNDET-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO KUNDET-MC
                                              KUNDET-MP
           OPEN INPUT KUNDET
           SET RENTEGR-LEVEL-INIT          TO TRUE
           INITIALIZE RENTEGR-DATA-FIELDS
           SET RENTEGR-EOF-OFF             TO TRUE
           SET RENTEGR-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO RENTEGR-MC
                                              RENTEGR-MP
           OPEN INPUT RENTEGR
           SET PURFAK-LEVEL-INIT           TO TRUE
           INITIALIZE PURFAK-DATA-FIELDS
           SET PURFAK-EOF-OFF              TO TRUE
           SET PURFAK-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO PURFAK-MC
                                              PURFAK-MP
           OPEN INPUT PURFAK
           INITIALIZE MNDPAR-DATA-FIELDS
           SET MNDPAR-EOF-OFF              TO TRUE
           SET MNDPAR-PROCESS              TO TRUE
           OPEN INPUT MNDPAR
           INITIALIZE RESPAR-DATA-FIELDS
           SET RESPAR-EOF-OFF              TO TRUE
           SET RESPAR-PROCESS              TO TRUE
           OPEN INPUT RESPAR
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           OPEN OUTPUT FAKTUT
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE KUNDEMA
           CLOSE KUNDET
           CLOSE RENTEGR
           CLOSE PURFAK
           CLOSE MNDPAR
           CLOSE RESPAR
           CLOSE FIRMAF
           CLOSE FAKTUT
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
