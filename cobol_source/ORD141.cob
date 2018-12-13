       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORD141R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM....: ORD141                                          *
      *  PROGRAMERER: ESPEN LARSEN                                    *
      *  PROGRAMERT.: 22.03.2004                                      *
      *  SIST RETTET: 14.04.2004                                      *
      *                                                               *
      *  PROGRAMMET TREKKER UT ALLE PLUKK/HENTE-ORDRE OG DANNER DATA  *
      *  FOR Å DANNE VENTE.ORDRE.FILER.                               *
      *  UPSI 1 PRINTER LISTE.                                        *
      ******************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ORD141.rpg
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
           SELECT ORDREM
               ASSIGN TO ORDREM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS ORDREM-STATUS
               RECORD KEY IS ORDREM-KEY1.
           SELECT VORDFIL
               ASSIGN TO UT-S-VORDFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VORDFIL-STATUS.
           SELECT VORDSF
               ASSIGN TO UT-S-VORDSF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VORDSF-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD PAR
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  PAR-IO-AREA.
           05  PAR-IO-AREA-X               PICTURE X(80).
       FD ORDREM
               RECORD CONTAINS 164.
       01  ORDREM-IO-AREA.
           05  ORDREM-IO-AREA-X.
               10  ORDREM-KEY1             PICTURE X(20).
               10  FILLER                  PICTURE X(144).
       FD VORDFIL
               BLOCK CONTAINS 160
               RECORD CONTAINS 80.
       01  VORDFIL-IO-AREA.
           05  VORDFIL-IO-AREA-X           PICTURE X(80).
       FD VORDSF
               BLOCK CONTAINS 120
               RECORD CONTAINS 60.
       01  VORDSF-IO-AREA.
           05  VORDSF-IO-AREA-X            PICTURE X(60).
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
           10  PAR-STATUS                  PICTURE 99 VALUE 0.
           10  ORDREM-STATUS               PICTURE 99 VALUE 0.
           10  VORDFIL-STATUS              PICTURE 99 VALUE 0.
           10  VORDSF-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  DATOER-XX-STATUS            PICTURE 99 VALUE 0.
           10  OPFELT-XX-STATUS            PICTURE 99 VALUE 0.
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
           05  ORDREM-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
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
           05  OPFELT-XX REDEFINES DATOER-XX-DATA-FIELDS.
               10  OARTNR                  PICTURE X(20).
               10  FILLER                  PICTURE X(237).
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
           05  OPFELT-XX-DATA-FIELDS.
               10  OARTNR                  PICTURE X(20).
               10  FILLER                  PICTURE X(237).
           05  LDATA-XX REDEFINES OPFELT-XX-DATA-FIELDS.
               10  LONR                    PICTURE X(5).
               10  FILLER                  PICTURE X(252).
           05  FILLER REDEFINES OPFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(5).
               10  LFIRMA                  PICTURE X(3).
               10  FILLER                  PICTURE X(249).
           05  FILLER REDEFINES OPFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(8).
               10  LUNDGR                  PICTURE X(3).
               10  FILLER                  PICTURE X(246).
           05  FILLER REDEFINES OPFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(11).
               10  LPROG                   PICTURE X(8).
               10  FILLER                  PICTURE X(238).
           05  FILLER REDEFINES OPFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(19).
               10  LANTX-IO.
                   15  LANTX               PICTURE S9(3).
               10  FILLER                  PICTURE X(235).
           05  FILLER REDEFINES OPFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(22).
               10  FINAVN                  PICTURE X(30).
               10  FILLER                  PICTURE X(205).
           05  FILLER REDEFINES OPFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(52).
               10  LOPNVN                  PICTURE X(35).
               10  FILLER                  PICTURE X(170).
           05  FILLER REDEFINES OPFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(87).
               10  LPRIID                  PICTURE X(4).
               10  FILLER                  PICTURE X(166).
      *     *  BESTILLINGSOPPGAVER (OVERSTYRING AV RBS-FILE) *    *
           05  FILLER REDEFINES OPFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(91).
               10  BJOBN                   PICTURE X(8).
               10  FILLER                  PICTURE X(158).
           05  FILLER REDEFINES OPFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(99).
               10  BBEST                   PICTURE X(1).
               10  FILLER                  PICTURE X(157).
           05  FILLER REDEFINES OPFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(100).
               10  BPERS                   PICTURE X(30).
               10  FILLER                  PICTURE X(127).
           05  FILLER REDEFINES OPFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(130).
               10  BETTB                   PICTURE X(40).
               10  FILLER                  PICTURE X(87).
           05  FILLER REDEFINES OPFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(170).
               10  BFORS                   PICTURE X(40).
               10  FILLER                  PICTURE X(47).
           05  FILLER REDEFINES OPFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(210).
               10  BMEMO                   PICTURE X(40).
               10  FILLER                  PICTURE X(7).
           05  FILLER REDEFINES OPFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(250).
               10  BANTX-IO.
                   15  BANTX               PICTURE S9(3).
               10  FILLER                  PICTURE X(4).
           05  FILLER REDEFINES OPFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(253).
               10  BPCLAS                  PICTURE X(1).
               10  FILLER                  PICTURE X(3).
           05  FILLER REDEFINES OPFELT-XX-DATA-FIELDS.
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
               10  PAR-XX                  PICTURE X(2).
               10  PORDNR                  PICTURE X(6).
           05  ORDREM-LEVEL-01.
               10  ORDREM-01-L2.
                   15  ORDREM-01-L2-FIRMA  PICTURE X(3).
               10  ORDREM-01-L1.
                   15  ORDREM-01-L1-ORDNR  PICTURE X(6).
           05  ORDREM-LEVEL-02.
               10  ORDREM-02-L2.
                   15  ORDREM-02-L2-FIRMA  PICTURE X(3).
               10  ORDREM-02-L1.
                   15  ORDREM-02-L1-ORDNR  PICTURE X(6).
           05  ORDREM-LEVEL-03.
               10  ORDREM-03-L2.
                   15  ORDREM-03-L2-FIRMA  PICTURE X(3).
               10  ORDREM-03-L1.
                   15  ORDREM-03-L1-ORDNR  PICTURE X(6).
           05  ORDREM-LEVEL-04.
               10  ORDREM-04-L2.
                   15  ORDREM-04-L2-FIRMA  PICTURE X(3).
               10  ORDREM-04-L1.
                   15  ORDREM-04-L1-ORDNR  PICTURE X(6).
           05  ORDREM-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  ORDNR                   PICTURE X(6).
               10  KUNDNR                  PICTURE X(6).
               10  KNAVN1                  PICTURE X(30).
               10  KNAV1F                  PICTURE X(20).
               10  KNAVN2                  PICTURE X(30).
               10  KTSIFF                  PICTURE X(1).
               10  DIRREG                  PICTURE X(1).
               10  FRITT                   PICTURE X(1).
               10  LAGER                   PICTURE X(2).
               10  BK                      PICTURE X(1).
               10  SKAF                    PICTURE X(1).
               10  BETM                    PICTURE X(2).
               10  GEBYR                   PICTURE X(1).
               10  FRAKT                   PICTURE X(1).
               10  AVD                     PICTURE X(1).
               10  KRETYP                  PICTURE X(1).
               10  REST                    PICTURE X(1).
               10  PRIKOD                  PICTURE X(1).
               10  STAM                    PICTURE X(1).
               10  KIS                     PICTURE X(1).
               10  KOMUTA                  PICTURE X(1).
               10  ORDATO-IO.
                   15  ORDATO              PICTURE S9(6).
               10  ORDMND                  PICTURE X(2).
               10  ORDDAG                  PICTURE X(2).
               10  ORDAAR                  PICTURE X(2).
               10  ORDMOT                  PICTURE X(2).
               10  TERMID                  PICTURE X(4).
               10  SELGKP                  PICTURE X(1).
               10  FERDIM                  PICTURE X(1).
               10  FAKTNR                  PICTURE X(2).
               10  KONKOD                  PICTURE X(1).
               10  REGKL-IO.
                   15  REGKL               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  RUTID                   PICTURE X(1).
               10  OPDATO-IO.
                   15  OPDATO              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  ANTPRT-IO.
                   15  ANTPRT              PICTURE S9(2).
               10  STATUS-X                PICTURE X(1).
               10  OHREC1                  PICTURE X(164).
               10  FAKREF                  PICTURE X(6).
               10  AVNAVN                  PICTURE X(11).
               10  OKKNR                   PICTURE X(6).
               10  REKVNR                  PICTURE X(15).
               10  FORSM                   PICTURE X(15).
               10  FORSM3                  PICTURE X(3).
               10  HND                     PICTURE X(3).
               10  KADR                    PICTURE X(30).
               10  POSTNR                  PICTURE X(4).
               10  PSTED                   PICTURE X(15).
               10  OHREC2                  PICTURE X(164).
               10  VAADR1                  PICTURE X(30).
               10  VADR1F                  PICTURE X(20).
               10  VAADR2                  PICTURE X(30).
               10  VAADR3                  PICTURE X(30).
               10  VAADR4                  PICTURE X(20).
               10  OHREC3                  PICTURE X(164).
               10  LAGLOC                  PICTURE X(6).
               10  POSNR-IO.
                   15  POSNR               PICTURE S9(3).
               10  ANTBES-IO.
                   15  ANTBES              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ANTRES-IO.
                   15  ANTRES              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  NOREST                  PICTURE X(1).
               10  ALF                     PICTURE X(3).
               10  TEKST                   PICTURE X(50).
               10  ARTNR                   PICTURE X(20).
               10  VARBET                  PICTURE X(30).
               10  VARB20                  PICTURE X(20).
               10  EDBNR-IO.
                   15  EDBNR               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  VGR-IO.
                   15  VGR                 PICTURE S9(5) USAGE
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
               10  RGPRIS-IO.
                   15  RGPRIS              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  RGRAB1-IO.
                   15  RGRAB1              PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  RGRAB2-IO.
                   15  RGRAB2              PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  RGRAB3-IO.
                   15  RGRAB3              PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  VEIPRI-IO.
                   15  VEIPRI              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  KOSPRI-IO.
                   15  KOSPRI              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  PRITIL-IO.
                   15  PRITIL              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  KODATO-IO.
                   15  KODATO              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  KOSIGN                  PICTURE X(2).
               10  KOSTAT                  PICTURE X(1).
               10  PRITYP                  PICTURE X(1).
               10  OVREC                   PICTURE X(164).
      ******** INDIKATOR SETOF"S **********
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  REGTMS-IO.
                   15  REGTMS              PICTURE S9(6).
               10  REGKL-N-IO.
                   15  REGKL-N             PICTURE S9(7).
               10  F6                      PICTURE X(6).
               10  REGTM                   PICTURE X(4).
               10  REGTMN-IO.
                   15  REGTMN              PICTURE S9(4).
               10  ANTORD-IO.
                   15  ANTORD              PICTURE S9(5).
               10  OPNR17                  PICTURE X(17).
               10  OPKEY                   PICTURE X(20).
               10  PSDATO                  PICTURE X(8).
               10  OSDATO                  PICTURE X(8).
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
           05  EDITTING-FIELDS.
               10  XO-40D                  PICTURE S9(4).
               10  XO-40U                  PICTURE 9(4).
               10  EDIT-REGTMN             PICTURE Z9.99.
               10  XO-72YY9                PICTURE Z.ZZZ.ZZZ,99.
               10  XO-50YY9                PICTURE ZZ.ZZ9.
               10  XO-82YY9                PICTURE ZZ.ZZZ.ZZZ,99.
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
           SET NOT-I-10                    TO TRUE
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
           PERFORM TOTAL-OUTPUT.
 
       LR-INDICATOR-TEST.
           IF  I-LR
               GO TO MAINLINE-TERMINATION
           END-IF
           PERFORM HEADING-OVERFLOW
 
           IF  PAR-PROCESS
               PERFORM PAR-FLDOFF
               PERFORM PAR-FLDSET
           END-IF
 
           IF  ORDREM-PROCESS
               PERFORM ORDREM-FLDOFF
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
           IF  (I-L2)
               SET NOT-I-52                TO TRUE
               SET NOT-I-53                TO TRUE
           END-IF
           IF  (I-L1)
               SET NOT-I-50                TO TRUE
               SET NOT-I-30                TO TRUE
               SET NOT-I-32                TO TRUE
               SET NOT-I-39                TO TRUE
               SET NOT-I-33                TO TRUE
               SET NOT-I-34                TO TRUE
               SET NOT-I-35                TO TRUE
           END-IF
           IF  (I-L2)
               MOVE 0                      TO FIRTOT
           END-IF
           IF  (I-L1)
               MOVE 0                      TO ORDTOT
           END-IF
           SET NOT-I-34                    TO TRUE
           SET NOT-I-51                    TO TRUE
      ********* PARAMETERTEST *************
           IF  (I-10)
               PERFORM PARRUT-S
           END-IF
           IF  (I-10)
               GO TO UT-T
      ******** FIRMA TEST,  BLANK ER ALLE FIRMA **************
           END-IF
           IF  (I-L2 AND I-22)
               SET I-31                    TO TRUE
           END-IF
           IF  (I-L2 AND NOT-I-22)
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
      ******** ER DETTE PLUKKORDRE/HETEORDRE?  *********
           END-IF
           IF  (I-01)
               SET NOT-I-32                TO TRUE
               IF  SKAF = 'P'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-32)
               SET NOT-I-32                TO TRUE
               IF  SKAF = 'H'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-32)
               SET NOT-I-32                TO TRUE
               IF  SKAF = 'V'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-32)
               GO TO UT-T
           END-IF
           IF  (I-01)
               SET NOT-I-39                TO TRUE
               IF  STATUS-X = 'U'
                   SET I-39                TO TRUE
               END-IF
           END-IF
           IF  (I-39)
               GO TO UT-T
           END-IF
           IF  (I-01)
               SET I-50                    TO TRUE
           END-IF
           IF  (I-01 AND NOT-I-52)
               SET I-51                    TO TRUE
               SET I-52                    TO TRUE
               SET I-53                    TO TRUE
           END-IF
           IF  (I-51 AND I-U1)
               PERFORM RBSRUT-S
      *****************************************************************
      * RUTINE FOR Å DANNE REG.TIDSPUNKT SOM TIME OG MINUTT.          *
      *****************************************************************
           END-IF
           IF  (I-01)
               MOVE REGKL                  TO REGKL-N
               MOVE REGKL-N-IO (2:6)       TO REGTMS-IO
               MOVE REGTMS                 TO F6
               MOVE F6 (1:4)               TO REGTM
               MOVE REGTM                  TO REGTMN-IO
      ******** ER ORDREDATO GAMMEL NOK ? KUN HVIST DATO I PARAMETER. *
      ******** UPSI 8 = KUN DENNE DATO.                             *
           END-IF
           IF  (I-01)
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
           IF  (I-23)
               SET I-30                    TO TRUE
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
           IF  (I-03)
               SET I-33                    TO TRUE
           END-IF
           IF  (I-04 AND NOT-I-35)
               SET I-34                    TO TRUE
               SET I-35                    TO TRUE
           END-IF
           IF  (I-04 AND I-34)
               ADD 1                       TO ANTORD
           END-IF
           IF  (I-04 AND I-30)
               PERFORM SUMRUT-S
      *****************************************************************
      * RUTINE FOR ALFA/ARTIKKELNUMMER. LAG OPPSLAGSNUMMER FØRST.     *
      *****************************************************************
           END-IF
           IF  (NOT-I-04)
               GO TO ENDONR-T
           END-IF
           SET NOT-I-38                    TO TRUE
           IF  EDBNR > 0
               SET I-38                    TO TRUE
           END-IF
           IF  (NOT-I-38)
               GO TO ENDONR-T
           END-IF
           MOVE ARTNR                      TO OARTNR
           IF  (I-04)
               CALL 'OPPSNR' USING OPFELT-XX-DATA-FIELDS
           END-IF
           MOVE OPFELT                     TO OPNR17 (1:0)
           MOVE ALF                        TO OPKEY (1:3)
           MOVE OPNR17                     TO OPKEY (4:17).
 
       ENDONR-T.
      *****************************************************************
           CONTINUE.
 
       UT-T.
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           CONTINUE.
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE ' '                        TO BBEST
           MOVE 'ORD72'                    TO LONR
           MOVE FIRMA                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'ORD141'                   TO LPROG (1:6)
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF.
      **********************************************
      *  SUBRUTINE FOR PARAMETERKORT.              *
      **********************************************
 
       PARRUT-S SECTION.
       PARRUT-S-P.
           MOVE 'A'                        TO DATOK
           MOVE PDATO                      TO DATO6
           CALL 'DATO8SIF' USING DATOER-XX-DATA-FIELDS
           MOVE AMD8                       TO PSDATO.
      *****************************************************************
      *  SUBRUTINE FOR Å SNU ORDREDATO SAMT OG HENTE 4 SIFFERER AR.
      *****************************************************************
 
       DATRUT-S SECTION.
       DATRUT-S-P.
           MOVE 'A'                        TO DATOK
           MOVE ORDATO                     TO DATO6
           CALL 'DATO8SIF' USING DATOER-XX-DATA-FIELDS
           MOVE AMD8                       TO OSDATO.
      **********************************************
      *  SUBRUTINE FOR Å SUMMERE ORDRE.            *
      **********************************************
 
       SUMRUT-S SECTION.
       SUMRUT-S-P.
           SET NOT-I-40                    TO TRUE
           IF  ANTBES NOT > 0,00
               SET I-40                    TO TRUE
           END-IF
           IF  (I-40)
               GO TO SUMEND-T
           END-IF
           SET NOT-I-42                    TO TRUE
           IF  ORDNR > '899999'
               SET I-42                    TO TRUE
           END-IF
      * ********
           MULTIPLY PRITIL BY ANTBES   GIVING TILSUM
           IF  (NOT-I-40)
               MULTIPLY ORPRIS BY ANTBES GIVING SUM1
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
               MOVE PAR-IO-AREA (37:2)     TO PAR-XX (1:2)
               MOVE PAR-IO-AREA (48:6)     TO PORDNR (1:6)
               IF  PORDNR = SPACES
                   SET I-21                TO TRUE
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
 
       ORDREM-GET SECTION.
       ORDREM-GET-P.
           IF  ORDREM-EOF-OFF
               READ ORDREM
               AT END
                   SET ORDREM-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       ORDREM-FLDOFF SECTION.
       ORDREM-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '1' )
               SET NOT-I-27                TO TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '2' )
               SET NOT-I-29                TO TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '3' )
               SET NOT-I-28                TO TRUE
           END-EVALUATE.
 
       ORDREM-FLDSET SECTION.
       ORDREM-FLDSET-P.
           EVALUATE TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '1' )
               MOVE ORDREM-IO-AREA (2:3)   TO FIRMA (1:3)
               MOVE ORDREM-IO-AREA (5:6)   TO ORDNR (1:6)
               MOVE ORDREM-IO-AREA (21:6)  TO KUNDNR (1:6)
               MOVE ORDREM-IO-AREA (27:30) TO KNAVN1 (1:30)
               MOVE ORDREM-IO-AREA (27:20) TO KNAV1F (1:20)
               IF  KNAV1F = SPACES
                   SET I-27                TO TRUE
               END-IF
               MOVE ORDREM-IO-AREA (57:30) TO KNAVN2 (1:30)
               MOVE ORDREM-IO-AREA (87:1)  TO KTSIFF (1:1)
               MOVE ORDREM-IO-AREA (88:1)  TO DIRREG (1:1)
               MOVE ORDREM-IO-AREA (89:1)  TO FRITT (1:1)
               MOVE ORDREM-IO-AREA (90:2)  TO LAGER (1:2)
               MOVE ORDREM-IO-AREA (92:1)  TO BK (1:1)
               MOVE ORDREM-IO-AREA (93:1)  TO SKAF (1:1)
               MOVE ORDREM-IO-AREA (94:2)  TO BETM (1:2)
               MOVE ORDREM-IO-AREA (96:1)  TO GEBYR (1:1)
               MOVE ORDREM-IO-AREA (97:1)  TO FRAKT (1:1)
               MOVE ORDREM-IO-AREA (98:1)  TO AVD (1:1)
               MOVE ORDREM-IO-AREA (99:1)  TO KRETYP (1:1)
               MOVE ORDREM-IO-AREA (100:1) TO REST (1:1)
               MOVE ORDREM-IO-AREA (101:1) TO PRIKOD (1:1)
               MOVE ORDREM-IO-AREA (102:1) TO STAM (1:1)
               MOVE ORDREM-IO-AREA (103:1) TO KIS (1:1)
               MOVE ORDREM-IO-AREA (105:1) TO KOMUTA (1:1)
               MOVE ORDREM-IO-AREA (136:6) TO ORDATO-IO
               INSPECT ORDATO-IO REPLACING ALL ' ' BY '0'
               MOVE ORDREM-IO-AREA (138:2) TO ORDMND (1:2)
               MOVE ORDREM-IO-AREA (136:2) TO ORDDAG (1:2)
               MOVE ORDREM-IO-AREA (140:2) TO ORDAAR (1:2)
               MOVE ORDREM-IO-AREA (142:2) TO ORDMOT (1:2)
               MOVE ORDREM-IO-AREA (144:4) TO TERMID (1:4)
               MOVE ORDREM-IO-AREA (148:1) TO SELGKP (1:1)
               MOVE ORDREM-IO-AREA (149:1) TO FERDIM (1:1)
               MOVE ORDREM-IO-AREA (150:2) TO FAKTNR (1:2)
               MOVE ORDREM-IO-AREA (152:1) TO KONKOD (1:1)
               MOVE ORDREM-IO-AREA (153:4) TO REGKL-IO
               MOVE ORDREM-IO-AREA (157:1) TO RUTID (1:1)
               MOVE ORDREM-IO-AREA (158:4) TO OPDATO-IO
               MOVE ORDREM-IO-AREA (162:2) TO ANTPRT-IO
               INSPECT ANTPRT-IO REPLACING ALL ' ' BY '0'
               MOVE ORDREM-IO-AREA (164:1) TO STATUS-X (1:1)
               MOVE ORDREM-IO-AREA (1:164) TO OHREC1 (1:164)
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '2' )
               MOVE ORDREM-IO-AREA (2:3)   TO FIRMA (1:3)
               MOVE ORDREM-IO-AREA (5:6)   TO ORDNR (1:6)
               MOVE ORDREM-IO-AREA (44:6)  TO FAKREF (1:6)
               MOVE ORDREM-IO-AREA (50:11) TO AVNAVN (1:11)
               MOVE ORDREM-IO-AREA (61:6)  TO OKKNR (1:6)
               MOVE ORDREM-IO-AREA (67:15) TO REKVNR (1:15)
               IF  REKVNR = SPACES
                   SET I-29                TO TRUE
               END-IF
               MOVE ORDREM-IO-AREA (82:15) TO FORSM (1:15)
               MOVE ORDREM-IO-AREA (82:3)  TO FORSM3 (1:3)
               MOVE ORDREM-IO-AREA (97:3)  TO HND (1:3)
               MOVE ORDREM-IO-AREA (101:30) TO KADR (1:30)
               MOVE ORDREM-IO-AREA (131:4) TO POSTNR (1:4)
               MOVE ORDREM-IO-AREA (135:15) TO PSTED (1:15)
               MOVE ORDREM-IO-AREA (1:164) TO OHREC2 (1:164)
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '3' )
               MOVE ORDREM-IO-AREA (2:3)   TO FIRMA (1:3)
               MOVE ORDREM-IO-AREA (5:6)   TO ORDNR (1:6)
               MOVE ORDREM-IO-AREA (21:30) TO VAADR1 (1:30)
               MOVE ORDREM-IO-AREA (21:20) TO VADR1F (1:20)
               IF  VADR1F = SPACES
                   SET I-28                TO TRUE
               END-IF
               MOVE ORDREM-IO-AREA (51:30) TO VAADR2 (1:30)
               MOVE ORDREM-IO-AREA (81:30) TO VAADR3 (1:30)
               MOVE ORDREM-IO-AREA (111:20) TO VAADR4 (1:20)
               MOVE ORDREM-IO-AREA (1:164) TO OHREC3 (1:164)
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) NOT = ' ' )
               MOVE ORDREM-IO-AREA (2:3)   TO FIRMA (1:3)
               MOVE ORDREM-IO-AREA (5:6)   TO ORDNR (1:6)
               MOVE ORDREM-IO-AREA (11:6)  TO LAGLOC (1:6)
               MOVE ORDREM-IO-AREA (17:3)  TO POSNR-IO
               INSPECT POSNR-IO REPLACING ALL ' ' BY '0'
               MOVE ORDREM-IO-AREA (21:4)  TO ANTBES-IO
               MOVE ORDREM-IO-AREA (25:4)  TO ANTRES-IO
               MOVE ORDREM-IO-AREA (33:1)  TO NOREST (1:1)
               MOVE ORDREM-IO-AREA (34:3)  TO ALF (1:3)
               MOVE ORDREM-IO-AREA (37:50) TO TEKST (1:50)
               MOVE ORDREM-IO-AREA (37:20) TO ARTNR (1:20)
               MOVE ORDREM-IO-AREA (57:30) TO VARBET (1:30)
               MOVE ORDREM-IO-AREA (57:20) TO VARB20 (1:20)
               MOVE ORDREM-IO-AREA (87:4)  TO EDBNR-IO
               MOVE ORDREM-IO-AREA (91:3)  TO VGR-IO
               MOVE ORDREM-IO-AREA (94:5)  TO ORPRIS-IO
               MOVE ORDREM-IO-AREA (99:2)  TO ORRAB1-IO
               MOVE ORDREM-IO-AREA (101:2) TO ORRAB2-IO
               MOVE ORDREM-IO-AREA (103:2) TO ORRAB3-IO
               MOVE ORDREM-IO-AREA (105:5) TO RGPRIS-IO
               MOVE ORDREM-IO-AREA (110:2) TO RGRAB1-IO
               MOVE ORDREM-IO-AREA (112:2) TO RGRAB2-IO
               MOVE ORDREM-IO-AREA (114:2) TO RGRAB3-IO
               MOVE ORDREM-IO-AREA (116:5) TO VEIPRI-IO
               MOVE ORDREM-IO-AREA (121:5) TO KOSPRI-IO
               MOVE ORDREM-IO-AREA (126:4) TO PRITIL-IO
               MOVE ORDREM-IO-AREA (130:4) TO KODATO-IO
               MOVE ORDREM-IO-AREA (134:2) TO KOSIGN (1:2)
               MOVE ORDREM-IO-AREA (136:1) TO KOSTAT (1:1)
               MOVE ORDREM-IO-AREA (137:1) TO PRITYP (1:1)
               MOVE ORDREM-IO-AREA (1:164) TO OVREC (1:164)
           END-EVALUATE.
 
       ORDREM-IDCHK SECTION.
       ORDREM-IDCHK-P.
           EVALUATE TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '1' )
             OR ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '2' )
             OR ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '3' )
             OR ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) NOT = ' ' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       ORDREM-IDSET SECTION.
       ORDREM-IDSET-P.
           EVALUATE TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '1' )
               SET I-01                    TO TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '2' )
               SET I-02                    TO TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '3' )
               SET I-03                    TO TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) NOT = ' ' )
               SET I-04                    TO TRUE
           END-EVALUATE.
 
       ORDREM-CHK-LEVEL SECTION.
       ORDREM-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '1' )
               MOVE LOW-VALUES             TO ORDREM-LEVEL-01
               MOVE ORDREM-IO-AREA (2:3)   TO ORDREM-01-L2-FIRMA
               MOVE ORDREM-IO-AREA (5:6)   TO ORDREM-01-L1-ORDNR
               IF  ORDREM-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDREM-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  ORDREM-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDREM-01-L2          TO THE-PRIOR-L2
               MOVE  ORDREM-01-L1          TO THE-PRIOR-L1
               SET ORDREM-LEVEL-INIT       TO TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '2' )
               MOVE LOW-VALUES             TO ORDREM-LEVEL-02
               MOVE ORDREM-IO-AREA (2:3)   TO ORDREM-02-L2-FIRMA
               MOVE ORDREM-IO-AREA (5:6)   TO ORDREM-02-L1-ORDNR
               IF  ORDREM-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDREM-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  ORDREM-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDREM-02-L2          TO THE-PRIOR-L2
               MOVE  ORDREM-02-L1          TO THE-PRIOR-L1
               SET ORDREM-LEVEL-INIT       TO TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) = ' '
            AND   ORDREM-IO-AREA (20:1) = '3' )
               MOVE LOW-VALUES             TO ORDREM-LEVEL-03
               MOVE ORDREM-IO-AREA (2:3)   TO ORDREM-03-L2-FIRMA
               MOVE ORDREM-IO-AREA (5:6)   TO ORDREM-03-L1-ORDNR
               IF  ORDREM-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDREM-03-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  ORDREM-03-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDREM-03-L2          TO THE-PRIOR-L2
               MOVE  ORDREM-03-L1          TO THE-PRIOR-L1
               SET ORDREM-LEVEL-INIT       TO TRUE
           WHEN ( ORDREM-IO-AREA (1:1) = 'O'
            AND   ORDREM-IO-AREA (19:1) NOT = ' ' )
               MOVE LOW-VALUES             TO ORDREM-LEVEL-04
               MOVE ORDREM-IO-AREA (2:3)   TO ORDREM-04-L2-FIRMA
               MOVE ORDREM-IO-AREA (5:6)   TO ORDREM-04-L1-ORDNR
               IF  ORDREM-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDREM-04-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  ORDREM-04-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDREM-04-L2          TO THE-PRIOR-L2
               MOVE  ORDREM-04-L1          TO THE-PRIOR-L1
               SET ORDREM-LEVEL-INIT       TO TRUE
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
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND I-50)
               MOVE SPACES TO VORDFIL-IO-AREA
               INITIALIZE VORDFIL-IO-AREA
               MOVE FIRMA                  TO VORDFIL-IO-AREA (1:3)
               MOVE ORDNR                  TO VORDFIL-IO-AREA (4:6)
               MOVE '1'                    TO VORDFIL-IO-AREA (10:1)
               MOVE OSDATO                 TO VORDFIL-IO-AREA (11:8)
               MOVE REGTM                  TO VORDFIL-IO-AREA (19:4)
               MOVE KUNDNR                 TO VORDFIL-IO-AREA (23:6)
               MOVE ORDATO-IO              TO VORDFIL-IO-AREA (30:6)
               MOVE REKVNR                 TO VORDFIL-IO-AREA (50:15)
               MOVE ORDMOT                 TO VORDFIL-IO-AREA (65:2)
               MOVE AVD                    TO VORDFIL-IO-AREA (67:1)
               WRITE VORDFIL-IO-AREA
           END-IF
           IF  (I-01 AND I-50)
               MOVE SPACES TO VORDSF-IO-AREA
               INITIALIZE VORDSF-IO-AREA
               MOVE FIRMA                  TO VORDSF-IO-AREA (1:3)
               MOVE KUNDNR                 TO VORDSF-IO-AREA (4:6)
               MOVE 'K'                    TO VORDSF-IO-AREA (24:1)
               MOVE ORDNR                  TO VORDSF-IO-AREA (25:6)
               MOVE OSDATO                 TO VORDSF-IO-AREA (31:8)
               MOVE REGTM                  TO VORDSF-IO-AREA (39:4)
               MOVE KUNDNR                 TO VORDSF-IO-AREA (43:6)
               MOVE ORDMOT                 TO VORDSF-IO-AREA (49:2)
               MOVE AVD                    TO VORDSF-IO-AREA (51:1)
               WRITE VORDSF-IO-AREA
           END-IF
           IF  (I-01 AND I-50 AND NOT-I-27)
               MOVE SPACES TO VORDSF-IO-AREA
               INITIALIZE VORDSF-IO-AREA
               MOVE FIRMA                  TO VORDSF-IO-AREA (1:3)
               MOVE KNAV1F                 TO VORDSF-IO-AREA (4:20)
               MOVE 'N'                    TO VORDSF-IO-AREA (24:1)
               MOVE ORDNR                  TO VORDSF-IO-AREA (25:6)
               MOVE OSDATO                 TO VORDSF-IO-AREA (31:8)
               MOVE REGTM                  TO VORDSF-IO-AREA (39:4)
               MOVE KUNDNR                 TO VORDSF-IO-AREA (43:6)
               MOVE ORDMOT                 TO VORDSF-IO-AREA (49:2)
               MOVE AVD                    TO VORDSF-IO-AREA (51:1)
               WRITE VORDSF-IO-AREA
           END-IF
           IF  (I-02 AND I-50 AND NOT-I-29)
               MOVE SPACES TO VORDSF-IO-AREA
               INITIALIZE VORDSF-IO-AREA
               MOVE FIRMA                  TO VORDSF-IO-AREA (1:3)
               MOVE REKVNR                 TO VORDSF-IO-AREA (4:15)
               MOVE 'R'                    TO VORDSF-IO-AREA (24:1)
               MOVE ORDNR                  TO VORDSF-IO-AREA (25:6)
               MOVE OSDATO                 TO VORDSF-IO-AREA (31:8)
               MOVE REGTM                  TO VORDSF-IO-AREA (39:4)
               MOVE KUNDNR                 TO VORDSF-IO-AREA (43:6)
               MOVE ORDMOT                 TO VORDSF-IO-AREA (49:2)
               MOVE AVD                    TO VORDSF-IO-AREA (51:1)
               WRITE VORDSF-IO-AREA
           END-IF
           IF  (I-03 AND I-50 AND NOT-I-28)
               MOVE SPACES TO VORDSF-IO-AREA
               INITIALIZE VORDSF-IO-AREA
               MOVE FIRMA                  TO VORDSF-IO-AREA (1:3)
               MOVE VADR1F                 TO VORDSF-IO-AREA (4:20)
               MOVE 'V'                    TO VORDSF-IO-AREA (24:1)
               MOVE ORDNR                  TO VORDSF-IO-AREA (25:6)
               MOVE OSDATO                 TO VORDSF-IO-AREA (31:8)
               MOVE REGTM                  TO VORDSF-IO-AREA (39:4)
               MOVE KUNDNR                 TO VORDSF-IO-AREA (43:6)
               MOVE ORDMOT                 TO VORDSF-IO-AREA (49:2)
               MOVE AVD                    TO VORDSF-IO-AREA (51:1)
               WRITE VORDSF-IO-AREA
           END-IF
           IF  (I-04 AND I-50 AND I-38)
               MOVE SPACES TO VORDSF-IO-AREA
               INITIALIZE VORDSF-IO-AREA
               MOVE FIRMA                  TO VORDSF-IO-AREA (1:3)
               MOVE OPKEY                  TO VORDSF-IO-AREA (4:20)
               MOVE 'A'                    TO VORDSF-IO-AREA (24:1)
               MOVE ORDNR                  TO VORDSF-IO-AREA (25:6)
               MOVE OSDATO                 TO VORDSF-IO-AREA (31:8)
               MOVE REGTM                  TO VORDSF-IO-AREA (39:4)
               MOVE KUNDNR                 TO VORDSF-IO-AREA (43:6)
               MOVE ORDMOT                 TO VORDSF-IO-AREA (49:2)
               MOVE AVD                    TO VORDSF-IO-AREA (51:1)
               WRITE VORDSF-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-51 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE '* * *  VENTEORDRE OVERSI' TO LISTE-IO-AREA (32:24)
               MOVE 'KT                    ' TO LISTE-IO-AREA (56:22)
               MOVE '* * *'                TO LISTE-IO-AREA (90:5)
               MOVE 'KJØREDATO'            TO LISTE-IO-AREA (100:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (111:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (125:4)
               IF  (I-51)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40U
               MOVE XO-40U (1:4)           TO LISTE-IO-AREA (129:4)
               IF  I-U1
                   MOVE 01                 TO LISTE-BEFORE-SKIP
                   MOVE 1                  TO LISTE-AFTER-SPACE
                   PERFORM LISTE-PRINT-LINE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               IF  I-U1
                   MOVE 1                  TO LISTE-AFTER-SPACE
                   PERFORM LISTE-PRINT-LINE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ORD.NR. '             TO LISTE-IO-AREA (1:8)
               MOVE 'AVD'                  TO LISTE-IO-AREA (9:3)
               MOVE 'DATO'                 TO LISTE-IO-AREA (14:4)
               MOVE 'TID'                  TO LISTE-IO-AREA (22:3)
               MOVE 'KUNDE '               TO LISTE-IO-AREA (27:6)
               MOVE 'KUNDENAVN'            TO LISTE-IO-AREA (34:9)
               MOVE 'OM'                   TO LISTE-IO-AREA (65:2)
               MOVE 'LA'                   TO LISTE-IO-AREA (68:2)
               MOVE 'ORDRESUM'             TO LISTE-IO-AREA (75:8)
               MOVE 'REKV.NR.'             TO LISTE-IO-AREA (84:8)
               MOVE 'VAREADRESSE/MERKNADER' TO LISTE-IO-AREA (100:21)
               IF  I-U1
                   MOVE 1                  TO LISTE-AFTER-SPACE
                   PERFORM LISTE-PRINT-LINE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               IF  I-U1
                   MOVE 1                  TO LISTE-AFTER-SPACE
                   PERFORM LISTE-PRINT-LINE
               END-IF
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE '* * *  VENTEORDRE OVERSI' TO LISTE-IO-AREA (32:24)
               MOVE 'KT                    ' TO LISTE-IO-AREA (56:22)
               MOVE '* * *'                TO LISTE-IO-AREA (90:5)
               MOVE 'KJØREDATO'            TO LISTE-IO-AREA (100:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (111:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (125:4)
               IF  (I-51)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40U
               MOVE XO-40U (1:4)           TO LISTE-IO-AREA (129:4)
               IF  I-U1
                   MOVE 01                 TO LISTE-BEFORE-SKIP
                   MOVE 1                  TO LISTE-AFTER-SPACE
                   PERFORM LISTE-PRINT-LINE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               IF  I-U1
                   MOVE 1                  TO LISTE-AFTER-SPACE
                   PERFORM LISTE-PRINT-LINE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ORD.NR. '             TO LISTE-IO-AREA (1:8)
               MOVE 'AVD'                  TO LISTE-IO-AREA (9:3)
               MOVE 'DATO'                 TO LISTE-IO-AREA (14:4)
               MOVE 'TID'                  TO LISTE-IO-AREA (22:3)
               MOVE 'KUNDE '               TO LISTE-IO-AREA (27:6)
               MOVE 'KUNDENAVN'            TO LISTE-IO-AREA (34:9)
               MOVE 'OM'                   TO LISTE-IO-AREA (65:2)
               MOVE 'LA'                   TO LISTE-IO-AREA (68:2)
               MOVE 'ORDRESUM'             TO LISTE-IO-AREA (75:8)
               MOVE 'REKV.NR.'             TO LISTE-IO-AREA (84:8)
               MOVE 'VAREADRESSE/MERKNADER' TO LISTE-IO-AREA (100:21)
               IF  I-U1
                   MOVE 1                  TO LISTE-AFTER-SPACE
                   PERFORM LISTE-PRINT-LINE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE '------------'         TO LISTE-IO-AREA (121:12)
               IF  I-U1
                   MOVE 1                  TO LISTE-AFTER-SPACE
                   PERFORM LISTE-PRINT-LINE
               END-IF
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-50 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ORDNR                  TO LISTE-IO-AREA (1:6)
               MOVE KTSIFF                 TO LISTE-IO-AREA (8:1)
               MOVE AVD                    TO LISTE-IO-AREA (10:1)
               MOVE ORDATO                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (12:8)
               MOVE REGTMN                 TO EDIT-REGTMN
               MOVE EDIT-REGTMN            TO LISTE-IO-AREA (21:5)
               MOVE KUNDNR                 TO LISTE-IO-AREA (27:6)
               MOVE KNAVN1                 TO LISTE-IO-AREA (34:30)
               MOVE ORDMOT                 TO LISTE-IO-AREA (65:2)
               MOVE LAGER                  TO LISTE-IO-AREA (68:2)
               MOVE ORDTOT                 TO XO-72YY9
               MOVE XO-72YY9               TO LISTE-IO-AREA (71:12)
               INITIALIZE ORDTOT
               MOVE REKVNR                 TO LISTE-IO-AREA (84:15)
               IF  (I-33)
                   MOVE VAADR1             TO LISTE-IO-AREA (100:30)
               END-IF
               IF  I-U1
                   MOVE 1                  TO LISTE-AFTER-SPACE
                   PERFORM LISTE-PRINT-LINE
               END-IF
           END-IF
           IF  (I-L2 AND I-53 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '***'                  TO LISTE-IO-AREA (4:3)
               MOVE 'ANTALL ORDRE '        TO LISTE-IO-AREA (22:13)
               MOVE ANTORD                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (35:6)
               INITIALIZE ANTORD
               MOVE 'NETTO FIRMATOTAL '    TO LISTE-IO-AREA (53:17)
               MOVE FIRTOT                 TO XO-82YY9
               MOVE XO-82YY9               TO LISTE-IO-AREA (70:13)
               INITIALIZE FIRTOT
               IF  I-U1
                   MOVE 1                  TO LISTE-BEFORE-SPACE
                   PERFORM LISTE-PRINT-LINE
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
           INITIALIZE PAR-DATA-FIELDS
           SET PAR-EOF-OFF                 TO TRUE
           SET PAR-PROCESS                 TO TRUE
           OPEN INPUT PAR
           SET ORDREM-LEVEL-INIT           TO TRUE
           INITIALIZE ORDREM-DATA-FIELDS
           SET ORDREM-EOF-OFF              TO TRUE
           SET ORDREM-PROCESS              TO TRUE
           OPEN INPUT ORDREM
           OPEN OUTPUT VORDFIL
           OPEN OUTPUT VORDSF
           IF I-U1
               OPEN OUTPUT LISTE
           END-IF
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PAR
           CLOSE ORDREM
           CLOSE VORDFIL
           CLOSE VORDSF
           IF I-U1
               CLOSE LISTE
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
