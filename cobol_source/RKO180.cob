       IDENTIFICATION DIVISION.
       PROGRAM-ID. RKO180R.
      **OBS ved endring i excel på Report Web *****************
      **********************************************  Z-WIN-RPG2   ****
      *    KONV. IFRA RSK180 UTVIDET RECORD.     ***TXT***ok ss***    *
      * PROGRAM   RKO180                                              *
      * R E S K O N T R O   S A L D O L I S T E                       *
      * UTLISTING AV RESKONTROFILE PR 15 OG SISTE I MND               *
      * ALDERSFORDELT SALDO PR KUNDE                                  *
      * SUB.TOTAL PR. 2 FØRSTE SIFRE I RESK.NR. PÅ FIRMA=920. <LOYDS> *        *
      * ENDR 24.07.98 TATT UT SUMMERING FOR FIRMA 920.                *        *
      *               TILPASSET TIL ÅR 2000.                          *        *
      * ENDR 18.08.98 PROGRAMMET STOPPET VED FEIL FORF.-DATO.         *        *
      *               SKRIVER NÅ FEILMELDING OG KJØRER VIDERE.        *        *
      *      18.08.98 SETTER PÅ H0 VED FEIL I PARAMDATO.              *
      *      21.12.98 SKRIVER TOTAL PR GRP FOR ALLE FIRMA.            *
      *      18.01.99 FEIL I KONTROLLUTSKRIFT. MANGLET GRP 3-9.       *
      *      15.11.00 TATT UT 19-ÅRHUNDRE I TEKST MOT 19/20.          *
      *      25.10.02 SJEKKER OM LISTE SKAL SKRIVES PR 15.            *
      *      27.02.03 SKRIVER LISTE UANSETT NÅR UPSI-8 ER PÅ.         *
      *      18.02.04 SKRIVER FIRMANR PÅ TOTALLINJE FOR GRUPPE.       *
      *      07.03.06 SKRIVER IKKE PERIODE NÅR U7 ER PÅ               *
      ***************************************************************** ********
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RKO180.rpg
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
           SELECT RESPAR
               ASSIGN TO UT-S-RESPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RESPAR-STATUS.
           SELECT INFILE
               ASSIGN TO UT-S-INFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INFILE-STATUS.
           SELECT KUNDEMA
               ASSIGN TO KUNDEMA
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMA-STATUS
               RECORD KEY IS KUNDEMA-KEY1.
           SELECT SYSPARM
               ASSIGN TO SYSPARM
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS SYSPARM-STATUS
               RECORD KEY IS SYSPARM-KEY1.
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
       FD RESPAR
               BLOCK CONTAINS 300
               RECORD CONTAINS 300.
       01  RESPAR-IO-AREA.
           05  RESPAR-IO-AREA-X            PICTURE X(300).
       FD INFILE
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  INFILE-IO-AREA.
           05  INFILE-IO-AREA-X            PICTURE X(200).
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1            PICTURE X(9).
               10  FILLER                  PICTURE X(190).
       FD SYSPARM
               RECORD CONTAINS 160.
       01  SYSPARM-IO-AREA.
           05  SYSPARM-IO-AREA-X.
               10  SYSPARM-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(150).
      *BUGFILO O   F  80  80            PRINTERSYSLST
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       WORKING-STORAGE SECTION.
       77  SLR-MAX   VALUE 10              PICTURE 9(4) USAGE BINARY.
       01  TABLES.
      * **START RBS - DATASTRUKTUR FOR SUB-PROGRAM RBS000 ********              áÓ""
           05  SLR-TABLE.
               10  SLR-ENTRY
                                           OCCURS 10 TIMES
                                           INDEXED BY SLR-I
                                                      SLR-S.
                   15  SLR                 PICTURE S9(11)V9(2).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  PARAM-STATUS                PICTURE 99 VALUE 0.
           10  RESPAR-STATUS               PICTURE 99 VALUE 0.
           10  INFILE-STATUS               PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  SYSPARM-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  DTOPAR-XX-STATUS            PICTURE 99 VALUE 0.
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
               88  RESPAR-EOF-OFF          VALUE '0'.
               88  RESPAR-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESPAR-READ-OFF         VALUE '0'.
               88  RESPAR-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESPAR-PROCESS-OFF      VALUE '0'.
               88  RESPAR-PROCESS          VALUE '1'.
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
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  SYSPARM-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  CONSOLE-IO-AREA.
               10  CONSOLE-IO-AREA-X       PICTURE X(800).
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
      *****************************************************************
      * DATASTRUKTUR FOR SUBPROGRAM DATO8SIF                          *
      *****************************************************************
      *DSDS: DATA STRUCTURE FIELDS
           05  DTOPAR-XX-DATA-FIELDS.
               10  DTOKOD                  PICTURE X(1).
               10  FILLER                  PICTURE X(256).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(1).
               10  DTODTO                  PICTURE X(6).
               10  FILLER                  PICTURE X(250).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(15).
               10  DTO8SI                  PICTURE X(8).
               10  FILLER                  PICTURE X(234).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(23).
               10  DTOMEL                  PICTURE X(57).
               10  FILLER                  PICTURE X(177).
      *****************************************************************
      * DATASTRUKTUR FOR SUBPROGRAM RBS000                            *
      *****************************************************************
           05  LDATA-XX REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  LONR                    PICTURE X(5).
               10  FILLER                  PICTURE X(252).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(5).
               10  LFIRMA                  PICTURE X(3).
               10  FILLER                  PICTURE X(249).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(8).
               10  LUNDGR                  PICTURE X(3).
               10  FILLER                  PICTURE X(246).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(11).
               10  LPROG                   PICTURE X(8).
               10  FILLER                  PICTURE X(238).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(19).
               10  LANTX-IO.
                   15  LANTX               PICTURE S9(3).
               10  FILLER                  PICTURE X(235).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(22).
               10  FINAVN                  PICTURE X(30).
               10  FILLER                  PICTURE X(205).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(52).
               10  LOPNVN                  PICTURE X(35).
               10  FILLER                  PICTURE X(170).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(87).
               10  LPRIID                  PICTURE X(4).
               10  FILLER                  PICTURE X(166).
      *     *  BESTILLINGSOPPGAVER (OVERSTYRING AV RBS-FILE) *    *
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(91).
               10  BJOBN                   PICTURE X(8).
               10  FILLER                  PICTURE X(158).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(99).
               10  BBEST                   PICTURE X(1).
               10  FILLER                  PICTURE X(157).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(100).
               10  BPERS                   PICTURE X(30).
               10  FILLER                  PICTURE X(127).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(130).
               10  BETTB                   PICTURE X(40).
               10  FILLER                  PICTURE X(87).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(170).
               10  BFORS                   PICTURE X(40).
               10  FILLER                  PICTURE X(47).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(210).
               10  BMEMO                   PICTURE X(40).
               10  FILLER                  PICTURE X(7).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(250).
               10  BANTX-IO.
                   15  BANTX               PICTURE S9(3).
               10  FILLER                  PICTURE X(4).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(253).
               10  BPCLAS                  PICTURE X(1).
               10  FILLER                  PICTURE X(3).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(254).
               10  BPRJE                   PICTURE X(3).
      * * START RBS - PARAMETERKORT OVERSTYRING AV RBS-FILE  * *
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
      * * START RBS - PARAMETERKORT OVERSTYRING AV RBS-FILE  * *
           05  PARAM-DATA-FIELDS.
               10  PJOBN                   PICTURE X(8).
               10  PRKODE                  PICTURE X(1).
               10  PPERS                   PICTURE X(30).
               10  PANTX-IO.
                   15  PANTX               PICTURE S9(3).
               10  PETTB                   PICTURE X(40).
               10  PFORS                   PICTURE X(40).
               10  PMEMO                   PICTURE X(40).
      * * END   RBS - PARAMETERKORT OVERSTYRING AV RBS-FILE  * *
           05  RESPAR-DATA-FIELDS.
               10  PAAR                    PICTURE X(2).
               10  PDATO-IO.
                   15  PDATO               PICTURE S9(6).
               10  PERIOD                  PICTURE X(15).
               10  PERIO5                  PICTURE X(5).
               10  PFORF                   PICTURE X(6).
               10  FORF30                  PICTURE X(6).
               10  FORF60                  PICTURE X(6).
               10  FORF90                  PICTURE X(6).
               10  FOF180                  PICTURE X(6).
               10  UFOF30                  PICTURE X(6).
               10  UFOF60                  PICTURE X(6).
               10  UFOF90                  PICTURE X(6).
               10  UFO180                  PICTURE X(6).
           05  INFILE-LEVEL-04.
               10  INFILE-04-L4.
                   15  INFILE-04-L4-FIRMA  PICTURE X(3).
               10  INFILE-04-L3.
                   15  INFILE-04-L3-RESKGR PICTURE X(1).
               10  INFILE-04-L2.
                   15  INFILE-04-L2-RESKT  PICTURE X(2).
               10  INFILE-04-L1.
                   15  INFILE-04-L1-RESKNR PICTURE X(6).
           05  INFILE-DATA-FIELDS.
               10  REC60                   PICTURE X(60).
               10  RESKEY                  PICTURE X(9).
               10  RESKNR                  PICTURE X(6).
               10  RESKT                   PICTURE X(2).
               10  RESKGR                  PICTURE X(1).
               10  RESGRN-IO.
                   15  RESGRN              PICTURE S9(1).
               10  FIRMA                   PICTURE X(3).
               10  FOFALL                  PICTURE X(6).
               10  BEL-IO.
                   15  BEL                 PICTURE S9(7)V9(2).
           05  KUNDEMA-DATA-FIELDS.
               10  NAVN1                   PICTURE X(22).
               10  PSTED                   PICTURE X(15).
               10  PNR                     PICTURE X(4).
               10  BETBET                  PICTURE X(2).
               10  KRG-IO.
                   15  KRG                 PICTURE S9(4).
               10  KANM                    PICTURE X(1).
           05  SYSPARM-DATA-FIELDS.
               10  SYSAFS                  PICTURE X(1).
      **************************************************************
      *    RUTINE FOR OVERSTYRING AV RBS-FILE VED BESTILLINGSJOB"S *
      **************************************************************
      *                    MOVE "PFORF   "BUGFL1  8        LEDETXT DEBUG
      *          BUGFL1    DEBUGBUGFILO   PFORF            VIS FELT/IND
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L4            PICTURE X(3).
               10  THE-PRIOR-L3            PICTURE X(1).
               10  THE-PRIOR-L2            PICTURE X(2).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  PFORF8                  PICTURE X(8).
               10  NF0308                  PICTURE X(8).
               10  NF0608                  PICTURE X(8).
               10  NF0908                  PICTURE X(8).
               10  NF1808                  PICTURE X(8).
               10  UF0308                  PICTURE X(8).
               10  UF0608                  PICTURE X(8).
               10  UF0908                  PICTURE X(8).
               10  UF1808                  PICTURE X(8).
               10  TOTSAL-IO.
                   15  TOTSAL              PICTURE S9(8)V9(2).
               10  FOFAL8                  PICTURE X(8).
               10  F30AKK-IO.
                   15  F30AKK              PICTURE S9(8)V9(2).
               10  F60AKK-IO.
                   15  F60AKK              PICTURE S9(8)V9(2).
               10  F90AKK-IO.
                   15  F90AKK              PICTURE S9(8)V9(2).
               10  F180AK-IO.
                   15  F180AK              PICTURE S9(8)V9(2).
               10  F6MDAK-IO.
                   15  F6MDAK              PICTURE S9(8)V9(2).
               10  U30AKK-IO.
                   15  U30AKK              PICTURE S9(8)V9(2).
               10  U60AKK-IO.
                   15  U60AKK              PICTURE S9(8)V9(2).
               10  U90AKK-IO.
                   15  U90AKK              PICTURE S9(8)V9(2).
               10  U180AK-IO.
                   15  U180AK              PICTURE S9(8)V9(2).
               10  U6MDAK-IO.
                   15  U6MDAK              PICTURE S9(8)V9(2).
               10  SUFORF-IO.
                   15  SUFORF              PICTURE S9(8)V9(2).
               10  SUMUFO-IO.
                   15  SUMUFO              PICTURE S9(8)V9(2).
               10  TOTSL3-IO.
                   15  TOTSL3              PICTURE S9(9)V9(2).
               10  TOTSGT-IO.
                   15  TOTSGT              PICTURE S9(9)V9(2).
               10  TOTGRT-IO.
                   15  TOTGRT              PICTURE S9(9)V9(2).
               10  F6MDL3-IO.
                   15  F6MDL3              PICTURE S9(8)V9(2).
               10  F6MDGT-IO.
                   15  F6MDGT              PICTURE S9(8)V9(2).
               10  F180L3-IO.
                   15  F180L3              PICTURE S9(8)V9(2).
               10  F180GT-IO.
                   15  F180GT              PICTURE S9(8)V9(2).
               10  F90L3-IO.
                   15  F90L3               PICTURE S9(8)V9(2).
               10  F90GT-IO.
                   15  F90GT               PICTURE S9(8)V9(2).
               10  F60L3-IO.
                   15  F60L3               PICTURE S9(8)V9(2).
               10  F60GT-IO.
                   15  F60GT               PICTURE S9(8)V9(2).
               10  F30L3-IO.
                   15  F30L3               PICTURE S9(8)V9(2).
               10  F30GT-IO.
                   15  F30GT               PICTURE S9(8)V9(2).
               10  SFOFL3-IO.
                   15  SFOFL3              PICTURE S9(8)V9(2).
               10  SFOFGT-IO.
                   15  SFOFGT              PICTURE S9(8)V9(2).
               10  U6MDL3-IO.
                   15  U6MDL3              PICTURE S9(8)V9(2).
               10  U6MDGT-IO.
                   15  U6MDGT              PICTURE S9(8)V9(2).
               10  U180L3-IO.
                   15  U180L3              PICTURE S9(8)V9(2).
               10  U180GT-IO.
                   15  U180GT              PICTURE S9(8)V9(2).
               10  U90L3-IO.
                   15  U90L3               PICTURE S9(8)V9(2).
               10  U90GT-IO.
                   15  U90GT               PICTURE S9(8)V9(2).
               10  U60L3-IO.
                   15  U60L3               PICTURE S9(8)V9(2).
               10  U60GT-IO.
                   15  U60GT               PICTURE S9(8)V9(2).
               10  U30L3-IO.
                   15  U30L3               PICTURE S9(8)V9(2).
               10  U30GT-IO.
                   15  U30GT               PICTURE S9(8)V9(2).
               10  SUMUL3-IO.
                   15  SUMUL3              PICTURE S9(8)V9(2).
               10  SUMUGT-IO.
                   15  SUMUGT              PICTURE S9(8)V9(2).
               10  TOTSPR-IO.
                   15  TOTSPR              PICTURE S9(8)V9(2).
               10  SUMFPR-IO.
                   15  SUMFPR              PICTURE S9(8).
               10  SUMUPR-IO.
                   15  SUMUPR              PICTURE S9(8).
               10  F30PR-IO.
                   15  F30PR               PICTURE S9(7).
               10  F60PR-IO.
                   15  F60PR               PICTURE S9(7).
               10  F90PR-IO.
                   15  F90PR               PICTURE S9(7).
               10  F180PR-IO.
                   15  F180PR              PICTURE S9(7).
               10  F6MDPR-IO.
                   15  F6MDPR              PICTURE S9(7).
               10  DESAL3-IO.
                   15  DESAL3              PICTURE S9(8)V9(2).
               10  DESAGT-IO.
                   15  DESAGT              PICTURE S9(8)V9(2).
               10  KRSAL3-IO.
                   15  KRSAL3              PICTURE S9(8)V9(2).
               10  KRSAGT-IO.
                   15  KRSAGT              PICTURE S9(8)V9(2).
               10  X-IO.
                   15  X                   PICTURE S9(2).
               10  SYSKEY                  PICTURE X(10).
           05  EDITTING-FIELDS.
               10  XO-40YNZ                PICTURE ZZZZ.
               10  EDIT-F30PR              PICTURE ZZZZ.ZZZ-.
               10  EDIT-F60PR              PICTURE ZZZZ.ZZZ-.
               10  EDIT-F90PR              PICTURE ZZZZ.ZZZ-.
               10  EDIT-F180PR             PICTURE ZZZZ.ZZZ-.
               10  EDIT-F6MDPR             PICTURE ZZZZ.ZZZ-.
               10  EDIT-SUMFPR             PICTURE ZZ.ZZZ.ZZZ-.
               10  EDIT-SUMUPR             PICTURE ZZ.ZZZ.ZZZ-.
               10  EDIT-TOTSPR             PICTURE ZZ.ZZZ.ZZZ,99-.
               10  XO-82YY9R               PICTURE ZZ.ZZZ.ZZZ,99-.
               10  XO-92YY9R               PICTURE ZZZ.ZZZ.ZZZ,99-.
               10  XO-112YY9R              PICTURE ZZ.ZZZ.ZZZ.ZZZ,99-.
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
           SET NOT-I-81                    TO TRUE
           SET NOT-I-82                    TO TRUE
           SET NOT-I-83                    TO TRUE
           SET NOT-I-84                    TO TRUE
           SET NOT-I-01                    TO TRUE
           SET NOT-I-04                    TO TRUE
           SET NOT-I-05                    TO TRUE
           SET NOT-I-03                    TO TRUE
 
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
                   PERFORM PARAM-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET PARAM-PROCESS       TO TRUE
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
                   PERFORM RESPAR-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET RESPAR-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  INFILE-PROCESS
               SET INFILE-PROCESS-OFF      TO TRUE
               SET INFILE-READ             TO TRUE
           END-IF
 
           IF  INFILE-READ
           AND RECORD-SELECTED-OFF
               PERFORM INFILE-GET
               SET INFILE-READ-OFF         TO TRUE
               IF  NOT INFILE-EOF
                   PERFORM INFILE-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET INFILE-PROCESS      TO TRUE
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
 
           IF  RESPAR-PROCESS
               PERFORM RESPAR-IDSET
           END-IF
 
           IF  INFILE-PROCESS
               PERFORM INFILE-IDSET
           END-IF
 
           IF  INFILE-PROCESS
               PERFORM INFILE-CHK-LEVEL
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
           PERFORM DETAIL-OVERFLOW
 
           IF  PARAM-PROCESS
               PERFORM PARAM-FLDSET
           END-IF
 
           IF  RESPAR-PROCESS
               PERFORM RESPAR-FLDSET
           END-IF
 
           IF  INFILE-PROCESS
               PERFORM INFILE-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  INFILE-PROCESS
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
               SET I-H0                    TO TRUE
           END-IF
           IF  (I-81)
               MOVE PJOBN                  TO BJOBN
               SET NOT-I-89                TO TRUE
               IF  PRKODE = 'B'
                   SET I-89                TO TRUE
               END-IF
               MOVE PRKODE                 TO BBEST
           END-IF
           IF  (I-81 AND I-89)
               MOVE PPERS                  TO BPERS
               MOVE PANTX                  TO BANTX-IO
           END-IF
           IF  (I-81)
               GO TO SLUTT-T
           END-IF
           IF  (I-82 AND I-89)
               MOVE PETTB                  TO BETTB
           END-IF
           IF  (I-82)
               GO TO SLUTT-T
           END-IF
           IF  (I-83 AND I-89)
               MOVE PFORS                  TO BFORS
           END-IF
           IF  (I-83)
               GO TO SLUTT-T
           END-IF
           IF  (I-84 AND I-89)
               MOVE PMEMO                  TO BMEMO
           END-IF
           IF  (I-84)
               GO TO SLUTT-T
           END-IF
           IF  (I-L4)
               PERFORM RBSRUT-S
           END-IF
           IF  (I-86)
               GO TO SLUTT-T
      **   TEST OM DET ER FLERE SUBTOTALER PR RESKONTROGRUPPE. ****
           END-IF
           IF  (I-L2 AND NOT-I-L3)
               SET I-55                    TO TRUE
           END-IF
           IF  (I-L3)
               SET NOT-I-55                TO TRUE
      **   GJØR OM TIL 8-SIFRET DATO
           END-IF
           IF  (I-01)
               MOVE PFORF                  TO DTODTO
               PERFORM DTORUT-S
           END-IF
           IF  (I-01 AND I-96)
               SET I-99                    TO TRUE
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               MOVE DTO8SI                 TO PFORF8
      *
           END-IF
           IF  (I-01)
               MOVE FORF30                 TO DTODTO
               PERFORM DTORUT-S
           END-IF
           IF  (I-01 AND I-96)
               SET I-99                    TO TRUE
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               MOVE DTO8SI                 TO NF0308
      *
           END-IF
           IF  (I-01)
               MOVE FORF60                 TO DTODTO
               PERFORM DTORUT-S
           END-IF
           IF  (I-01 AND I-96)
               SET I-99                    TO TRUE
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               MOVE DTO8SI                 TO NF0608
      *
           END-IF
           IF  (I-01)
               MOVE FORF90                 TO DTODTO
               PERFORM DTORUT-S
           END-IF
           IF  (I-01 AND I-96)
               SET I-99                    TO TRUE
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               MOVE DTO8SI                 TO NF0908
      *
           END-IF
           IF  (I-01)
               MOVE FOF180                 TO DTODTO
               PERFORM DTORUT-S
           END-IF
           IF  (I-01 AND I-96)
               SET I-99                    TO TRUE
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               MOVE DTO8SI                 TO NF1808
      *
           END-IF
           IF  (I-01)
               MOVE UFOF30                 TO DTODTO
               PERFORM DTORUT-S
           END-IF
           IF  (I-01 AND I-96)
               SET I-99                    TO TRUE
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               MOVE DTO8SI                 TO UF0308
      *
           END-IF
           IF  (I-01)
               MOVE UFOF60                 TO DTODTO
               PERFORM DTORUT-S
           END-IF
           IF  (I-01 AND I-96)
               SET I-99                    TO TRUE
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               MOVE DTO8SI                 TO UF0608
      *
           END-IF
           IF  (I-01)
               MOVE UFOF90                 TO DTODTO
               PERFORM DTORUT-S
           END-IF
           IF  (I-01 AND I-96)
               SET I-99                    TO TRUE
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               MOVE DTO8SI                 TO UF0908
      *
           END-IF
           IF  (I-01)
               MOVE UFO180                 TO DTODTO
               PERFORM DTORUT-S
           END-IF
           IF  (I-01 AND I-96)
               SET I-99                    TO TRUE
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               MOVE DTO8SI                 TO UF1808
      *
           END-IF
           IF  (I-01)
               SET NOT-I-31                TO TRUE
               IF  PAAR > '80'
                   SET I-31                TO TRUE
               END-IF
      *
           END-IF
           IF  (I-01)
               GO TO SLUTT-T
           END-IF
           ADD BEL                         TO TOTSAL
      **         B1        TAG
      *
           MOVE FOFALL                     TO DTODTO
           PERFORM DTORUT-S
           IF  (NOT-I-96)
               MOVE DTO8SI                 TO FOFAL8
               SET NOT-I-15                TO TRUE
               IF  FOFAL8 > PFORF8
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-96)
               SET NOT-I-15                TO TRUE
               IF  FOFALL > PFORF
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-15)
               GO TO C1-T
           END-IF
           IF  (NOT-I-96)
               SET NOT-I-17                TO TRUE
               IF  FOFAL8 NOT > NF0308
                   SET I-17                TO TRUE
               END-IF
           END-IF
           IF  (I-96)
               SET NOT-I-17                TO TRUE
               IF  FOFALL NOT > FORF30
                   SET I-17                TO TRUE
               END-IF
           END-IF
           IF  (I-17)
               GO TO B4-T
           END-IF
           ADD BEL                         TO F30AKK
           GO TO D1-T.
 
       B4-T.
           IF  (NOT-I-96)
               SET NOT-I-19                TO TRUE
               IF  FOFAL8 NOT > NF0608
                   SET I-19                TO TRUE
               END-IF
           END-IF
           IF  (I-96)
               SET NOT-I-19                TO TRUE
               IF  FOFALL NOT > FORF60
                   SET I-19                TO TRUE
               END-IF
           END-IF
           IF  (I-19)
               GO TO B5-T
           END-IF
           ADD BEL                         TO F60AKK
           GO TO D1-T.
 
       B5-T.
           IF  (NOT-I-96)
               SET NOT-I-20                TO TRUE
               IF  FOFAL8 NOT > NF0908
                   SET I-20                TO TRUE
               END-IF
           END-IF
           IF  (I-96)
               SET NOT-I-20                TO TRUE
               IF  FOFALL NOT > FORF90
                   SET I-20                TO TRUE
               END-IF
           END-IF
           IF  (I-20)
               GO TO B6-T
           END-IF
           ADD BEL                         TO F90AKK
           GO TO D1-T.
 
       B6-T.
           IF  (NOT-I-96)
               SET NOT-I-21                TO TRUE
               IF  FOFAL8 NOT > NF1808
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-96)
               SET NOT-I-21                TO TRUE
               IF  FOFALL NOT > FOF180
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-21)
               GO TO B7-T
           END-IF
           ADD BEL                         TO F180AK
           GO TO D1-T.
 
       B7-T.
           ADD BEL                         TO F6MDAK
           GO TO D1-T.
 
       C1-T.
           IF  (NOT-I-96)
               SET NOT-I-23                TO TRUE
               IF  FOFAL8 > UF0308
                   SET I-23                TO TRUE
               END-IF
           END-IF
           IF  (I-96)
               SET NOT-I-23                TO TRUE
               IF  FOFALL > UFOF30
                   SET I-23                TO TRUE
               END-IF
           END-IF
           IF  (I-23)
               GO TO C4-T
           END-IF
           ADD BEL                         TO U30AKK
           GO TO D1-T.
 
       C4-T.
           IF  (NOT-I-96)
               SET NOT-I-25                TO TRUE
               IF  FOFAL8 > UF0608
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-96)
               SET NOT-I-25                TO TRUE
               IF  FOFALL > UFOF60
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-25)
               GO TO C5-T
           END-IF
           ADD BEL                         TO U60AKK
           GO TO D1-T.
 
       C5-T.
           IF  (NOT-I-96)
               SET NOT-I-26                TO TRUE
               IF  FOFAL8 > UF0908
                   SET I-26                TO TRUE
               END-IF
           END-IF
           IF  (I-96)
               SET NOT-I-26                TO TRUE
               IF  FOFALL > UFOF90
                   SET I-26                TO TRUE
               END-IF
           END-IF
           IF  (I-26)
               GO TO C6-T
           END-IF
           ADD BEL                         TO U90AKK
           GO TO D1-T.
 
       C6-T.
           IF  (NOT-I-96)
               SET NOT-I-27                TO TRUE
               IF  FOFAL8 > UF1808
                   SET I-27                TO TRUE
               END-IF
           END-IF
           IF  (I-96)
               SET NOT-I-27                TO TRUE
               IF  FOFALL > UFO180
                   SET I-27                TO TRUE
               END-IF
           END-IF
           IF  (I-27)
               GO TO C7-T
           END-IF
           ADD BEL                         TO U180AK
           GO TO D1-T.
 
       C7-T.
           ADD BEL                         TO U6MDAK.
 
       D1-T.
           CONTINUE.
 
       SLUTT-T.
      *****************************************************
      *          G1        TAG
           CONTINUE.
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE FIRMA                      TO SYSKEY (1:3)
           MOVE 'RES*011'                  TO SYSKEY (4:7)
           MOVE SYSKEY                     TO SYSPARM-KEY1
           READ SYSPARM RECORD KEY IS SYSPARM-KEY1
           INVALID KEY
               SET I-45                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-45                TO TRUE
               PERFORM SYSPARM-FLDSET
               PERFORM SYSPARM-IDSET
           END-READ
           IF  (NOT-I-45)
               SET NOT-I-45                TO TRUE
               IF  SYSAFS NOT = 'J'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-45)
               SET NOT-I-45                TO TRUE
               IF  PERIO5 = '01-15'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-45 AND NOT-I-U8)
               SET I-86                    TO TRUE
               GO TO RBSEND-T
           END-IF
           IF  (NOT-I-89)
               MOVE ' '                    TO BBEST
           END-IF
           MOVE 'RES02'                    TO LONR
           MOVE FIRMA                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'RKO180  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF.
 
       RBSEND-T.
           CONTINUE.
      *****************************************************************
      *  SUBRUTINE FOR Å HENTE 8-SIFRET DATO.                         * *
      *****************************************************************
 
       DTORUT-S SECTION.
       DTORUT-S-P.
           SET NOT-I-96                    TO TRUE
           MOVE 'B'                        TO DTOKOD
           CALL 'DATO8SIF' USING DTOPAR-XX-DATA-FIELDS
           SET NOT-I-96                    TO TRUE
           IF  DTOKOD = 'F'
               SET I-96                    TO TRUE
           END-IF.
      ******************************************************
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L0 AND I-86)
               GO TO L1END-T
           END-IF
           IF  (I-L1)
               MOVE 0,00                   TO SUFORF
               ADD F6MDAK                  TO SUFORF
               ADD F180AK                  TO SUFORF
               ADD F90AKK                  TO SUFORF
               ADD F60AKK                  TO SUFORF
               ADD F30AKK                  TO SUFORF
               ADD U6MDAK                  TO SUMUFO
               ADD U180AK                  TO SUMUFO
               ADD U90AKK                  TO SUMUFO
               ADD U60AKK                  TO SUMUFO
               ADD U30AKK                  TO SUMUFO
               MOVE RESKEY                 TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-90                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-90            TO TRUE
                   PERFORM KUNDEMA-FLDSET
                   PERFORM KUNDEMA-IDSET
               END-READ
           END-IF
           IF  (I-L1 AND NOT-I-90)
               SET NOT-I-79                TO TRUE
               IF  KANM = 'I'
                   SET I-79                TO TRUE
               END-IF
           END-IF
           IF  (I-L1)
               ADD TOTSAL                  TO TOTSL3
               ADD TOTSAL                  TO TOTSGT
               ADD TOTSAL                  TO TOTGRT
               ADD F6MDAK                  TO F6MDL3
               ADD F6MDAK                  TO F6MDGT
               ADD F180AK                  TO F180L3
               ADD F180AK                  TO F180GT
               ADD F90AKK                  TO F90L3
               ADD F90AKK                  TO F90GT
               ADD F60AKK                  TO F60L3
               ADD F60AKK                  TO F60GT
               ADD F30AKK                  TO F30L3
               ADD F30AKK                  TO F30GT
               ADD SUFORF                  TO SFOFL3
               ADD SUFORF                  TO SFOFGT
               ADD U6MDAK                  TO U6MDL3
               ADD U6MDAK                  TO U6MDGT
               ADD U180AK                  TO U180L3
               ADD U180AK                  TO U180GT
               ADD U90AKK                  TO U90L3
               ADD U90AKK                  TO U90GT
               ADD U60AKK                  TO U60L3
               ADD U60AKK                  TO U60GT
               ADD U30AKK                  TO U30L3
               ADD U30AKK                  TO U30GT
               ADD SUMUFO                  TO SUMUL3
               ADD SUMUFO                  TO SUMUGT
               ADD TOTSAL                  TO TOTSPR ROUNDED
               ADD SUFORF                  TO SUMFPR ROUNDED
               ADD SUMUFO                  TO SUMUPR
               ADD F30AKK                  TO F30PR ROUNDED
               ADD F60AKK                  TO F60PR ROUNDED
               ADD F90AKK                  TO F90PR ROUNDED
               ADD F180AK                  TO F180PR ROUNDED
               ADD F6MDAK                  TO F6MDPR ROUNDED
               SET NOT-I-46                TO TRUE
               IF  TOTSAL NOT < 0,00
                   SET I-46                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-46)
               ADD TOTSAL                  TO DESAL3
               ADD TOTSAL                  TO DESAGT
           END-IF
           IF  (I-L1 AND NOT-I-46)
               ADD TOTSAL                  TO KRSAL3
               ADD TOTSAL                  TO KRSAGT
           END-IF
           IF  (I-L1)
               MOVE 0,00                   TO SUMUFO
               MOVE 0,00                   TO SUFORF
               MOVE 0,00                   TO F30AKK
               MOVE 0,00                   TO F60AKK
               MOVE 0,00                   TO F90AKK
               MOVE 0,00                   TO F180AK
               MOVE 0,00                   TO F6MDAK
               MOVE 0,00                   TO U30AKK
               MOVE 0,00                   TO U60AKK
               MOVE 0,00                   TO U90AKK
               MOVE 0,00                   TO U180AK
               MOVE 0,00                   TO U6MDAK
               MOVE 0,00                   TO TOTSAL
           END-IF.
 
       L1END-T.
           IF  (I-L3)
               ADD 1 TO RESGRN         GIVING X
               ADD TOTSL3                  TO SLR (X)
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           END-IF
           .
 
       PARAM-GET SECTION.
       PARAM-GET-P.
           IF  PARAM-EOF-OFF
               READ PARAM
               AT END
                   SET PARAM-EOF           TO TRUE
               END-READ
           END-IF.
 
       PARAM-FLDSET SECTION.
       PARAM-FLDSET-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '1' )
               MOVE PARAM-IO-AREA (8:8)    TO PJOBN (1:8)
               MOVE PARAM-IO-AREA (19:1)   TO PRKODE (1:1)
               MOVE PARAM-IO-AREA (32:30)  TO PPERS (1:30)
               MOVE PARAM-IO-AREA (69:3)   TO PANTX-IO
               INSPECT PANTX-IO REPLACING ALL ' ' BY '0'
           WHEN ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '2' )
               MOVE PARAM-IO-AREA (21:40)  TO PETTB (1:40)
           WHEN ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '3' )
               MOVE PARAM-IO-AREA (21:40)  TO PFORS (1:40)
           WHEN ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '4' )
               MOVE PARAM-IO-AREA (21:40)  TO PMEMO (1:40)
           END-EVALUATE.
 
       PARAM-IDCHK SECTION.
       PARAM-IDCHK-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '1' )
             OR ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '2' )
             OR ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '3' )
             OR ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '4' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       PARAM-IDSET SECTION.
       PARAM-IDSET-P.
           EVALUATE TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '1' )
               SET I-81                    TO TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '2' )
               SET I-82                    TO TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '3' )
               SET I-83                    TO TRUE
           WHEN ( PARAM-IO-AREA (1:1) = 'R'
            AND   PARAM-IO-AREA (2:1) = '4' )
               SET I-84                    TO TRUE
           END-EVALUATE.
 
       RESPAR-GET SECTION.
       RESPAR-GET-P.
           IF  RESPAR-EOF-OFF
               READ RESPAR
               AT END
                   SET RESPAR-EOF          TO TRUE
               END-READ
           END-IF.
 
       RESPAR-FLDSET SECTION.
       RESPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ( RESPAR-IO-AREA (1:1) = '9'
            AND   RESPAR-IO-AREA (2:1) = '0' )
               MOVE RESPAR-IO-AREA (5:2)   TO PAAR (1:2)
               MOVE RESPAR-IO-AREA (7:6)   TO PDATO-IO
               INSPECT PDATO-IO REPLACING ALL ' ' BY '0'
               MOVE RESPAR-IO-AREA (19:15) TO PERIOD (1:15)
               MOVE RESPAR-IO-AREA (19:5)  TO PERIO5 (1:5)
               MOVE RESPAR-IO-AREA (13:6)  TO PFORF (1:6)
               MOVE RESPAR-IO-AREA (34:6)  TO FORF30 (1:6)
               MOVE RESPAR-IO-AREA (40:6)  TO FORF60 (1:6)
               MOVE RESPAR-IO-AREA (46:6)  TO FORF90 (1:6)
               MOVE RESPAR-IO-AREA (52:6)  TO FOF180 (1:6)
               MOVE RESPAR-IO-AREA (58:6)  TO UFOF30 (1:6)
               MOVE RESPAR-IO-AREA (64:6)  TO UFOF60 (1:6)
               MOVE RESPAR-IO-AREA (70:6)  TO UFOF90 (1:6)
               MOVE RESPAR-IO-AREA (76:6)  TO UFO180 (1:6)
           END-EVALUATE.
 
       RESPAR-IDCHK SECTION.
       RESPAR-IDCHK-P.
           EVALUATE TRUE
           WHEN ( RESPAR-IO-AREA (1:1) = '9'
            AND   RESPAR-IO-AREA (2:1) = '0' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       RESPAR-IDSET SECTION.
       RESPAR-IDSET-P.
           EVALUATE TRUE
           WHEN ( RESPAR-IO-AREA (1:1) = '9'
            AND   RESPAR-IO-AREA (2:1) = '0' )
               SET I-01                    TO TRUE
           END-EVALUATE.
 
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
           WHEN ( INFILE-IO-AREA (1:1) = '0'
            AND   INFILE-IO-AREA (2:1) = '2' )
               MOVE INFILE-IO-AREA (1:60)  TO REC60 (1:60)
               MOVE INFILE-IO-AREA (3:9)   TO RESKEY (1:9)
               MOVE INFILE-IO-AREA (6:6)   TO RESKNR (1:6)
               MOVE INFILE-IO-AREA (6:2)   TO RESKT (1:2)
               MOVE INFILE-IO-AREA (6:1)   TO RESKGR (1:1)
               MOVE INFILE-IO-AREA (6:1)   TO RESGRN-IO
               INSPECT RESGRN-IO REPLACING ALL ' ' BY '0'
               MOVE INFILE-IO-AREA (3:3)   TO FIRMA (1:3)
               MOVE INFILE-IO-AREA (42:6)  TO FOFALL (1:6)
               MOVE INFILE-IO-AREA (48:9)  TO BEL-IO
               INSPECT BEL-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       INFILE-IDCHK SECTION.
       INFILE-IDCHK-P.
           EVALUATE TRUE
           WHEN ( INFILE-IO-AREA (1:1) = '0'
            AND   INFILE-IO-AREA (2:1) = '2' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       INFILE-IDSET SECTION.
       INFILE-IDSET-P.
           EVALUATE TRUE
           WHEN ( INFILE-IO-AREA (1:1) = '0'
            AND   INFILE-IO-AREA (2:1) = '2' )
               SET I-04                    TO TRUE
           END-EVALUATE.
 
       INFILE-CHK-LEVEL SECTION.
       INFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( INFILE-IO-AREA (1:1) = '0'
            AND   INFILE-IO-AREA (2:1) = '2' )
               MOVE LOW-VALUES             TO INFILE-LEVEL-04
               MOVE INFILE-IO-AREA (3:3)   TO INFILE-04-L4-FIRMA
               MOVE INFILE-IO-AREA (6:1)   TO INFILE-04-L3-RESKGR
               MOVE INFILE-IO-AREA (6:2)   TO INFILE-04-L2-RESKT
               MOVE INFILE-IO-AREA (6:6)   TO INFILE-04-L1-RESKNR
               IF  INFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INFILE-04-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   WHEN  INFILE-04-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  INFILE-04-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INFILE-04-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INFILE-04-L4          TO THE-PRIOR-L4
               MOVE  INFILE-04-L3          TO THE-PRIOR-L3
               MOVE  INFILE-04-L2          TO THE-PRIOR-L2
               MOVE  INFILE-04-L1          TO THE-PRIOR-L1
               SET INFILE-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (16:22) TO NAVN1 (1:22)
               MOVE KUNDEMA-IO-AREA (106:15) TO PSTED (1:15)
               MOVE KUNDEMA-IO-AREA (121:4) TO PNR (1:4)
               MOVE KUNDEMA-IO-AREA (127:2) TO BETBET (1:2)
               MOVE KUNDEMA-IO-AREA (129:4) TO KRG-IO
               INSPECT KRG-IO REPLACING ALL ' ' BY '0'
               MOVE KUNDEMA-IO-AREA (161:1) TO KANM (1:1)
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-05                        TO TRUE.
 
       SYSPARM-FLDSET SECTION.
       SYSPARM-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE SYSPARM-IO-AREA (069:1) TO SYSAFS (1:1)
           END-EVALUATE.
 
       SYSPARM-IDSET SECTION.
       SYSPARM-IDSET-P.
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
           IF  (I-04 AND I-L2 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE '*** ALDERSFORDELT SALDOL' TO LISTE-IO-AREA (32:24)
               MOVE 'ISTE *** FOR'         TO LISTE-IO-AREA (56:12)
               IF  (I-U7)
                   MOVE 'ISTE ***    '     TO LISTE-IO-AREA (56:12)
               END-IF
               IF  (NOT-I-U7)
                   MOVE PERIOD             TO LISTE-IO-AREA (69:15)
               END-IF
               IF  (I-31 AND NOT-I-U7)
                   MOVE '19'               TO LISTE-IO-AREA (86:2)
               END-IF
               IF  (NOT-I-31 AND NOT-I-U7)
                   MOVE '20'               TO LISTE-IO-AREA (86:2)
               END-IF
               IF  (NOT-I-U7)
                   MOVE PAAR               TO LISTE-IO-AREA (88:2)
               END-IF
               MOVE 'FRAMSTILT'            TO LISTE-IO-AREA (100:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (110:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (124:4)
               IF  (I-L4)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (129:4)
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
               MOVE 'RESKNR BM N A V N'    TO LISTE-IO-AREA (1:17)
               MOVE '*'                    TO LISTE-IO-AREA (35:1)
               MOVE 'FORFALT SALDO ALDERFORDE' TO LISTE-IO-AREA (49:24)
               MOVE 'LT'                   TO LISTE-IO-AREA (73:2)
               MOVE 'S U M   *    S U M'   TO LISTE-IO-AREA (91:18)
               MOVE 'KRED.   S A L D O'    TO LISTE-IO-AREA (114:17)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'P O S T A D R E S S E' TO LISTE-IO-AREA (11:21)
               MOVE '*   0-30 DG  31-60 DG' TO LISTE-IO-AREA (35:21)
               MOVE '61-90 DG  3-6 MND  OV.6' TO LISTE-IO-AREA (59:23)
               MOVE 'MNDR   FORFALT  * IKKE F' TO LISTE-IO-AREA (83:24)
               MOVE 'ORFALT  GR.  PR.'     TO LISTE-IO-AREA (107:16)
               MOVE PDATO                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (124:8)
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
           END-IF
           IF  (I-01 AND I-96)
               MOVE SPACES TO CONSOLE-IO-AREA
               INITIALIZE CONSOLE-IO-AREA
               MOVE '==> CANCEL RKO180  ==>  ' TO CONSOLE-IO-AREA
                                                                (1:24)
               MOVE 'FEIL I DATO (SUBRUT DATO' TO CONSOLE-IO-AREA
                                                               (25:24)
               MOVE '8SIF)       '         TO CONSOLE-IO-AREA (49:12)
               DISPLAY CONSOLE-IO-AREA
               MOVE SPACES TO CONSOLE-IO-AREA
               INITIALIZE CONSOLE-IO-AREA
               MOVE '==> '                 TO CONSOLE-IO-AREA (1:4)
               MOVE 'PFORF : '             TO CONSOLE-IO-AREA (5:8)
               MOVE PFORF                  TO CONSOLE-IO-AREA (15:6)
               MOVE 'NF0308: '             TO CONSOLE-IO-AREA (21:8)
               MOVE NF0308                 TO CONSOLE-IO-AREA (31:8)
               MOVE 'NF0608: '             TO CONSOLE-IO-AREA (41:8)
               MOVE NF0608                 TO CONSOLE-IO-AREA (49:8)
               DISPLAY CONSOLE-IO-AREA
               MOVE SPACES TO CONSOLE-IO-AREA
               INITIALIZE CONSOLE-IO-AREA
               MOVE '==> '                 TO CONSOLE-IO-AREA (1:4)
               MOVE 'NF0908: '             TO CONSOLE-IO-AREA (5:8)
               MOVE NF0908                 TO CONSOLE-IO-AREA (13:8)
               MOVE 'NF1808: '             TO CONSOLE-IO-AREA (21:8)
               MOVE NF1808                 TO CONSOLE-IO-AREA (31:8)
               MOVE 'UF0308: '             TO CONSOLE-IO-AREA (41:8)
               MOVE UF0308                 TO CONSOLE-IO-AREA (49:8)
               DISPLAY CONSOLE-IO-AREA
               MOVE SPACES TO CONSOLE-IO-AREA
               INITIALIZE CONSOLE-IO-AREA
               MOVE '==> '                 TO CONSOLE-IO-AREA (1:4)
               MOVE 'UF0608: '             TO CONSOLE-IO-AREA (5:8)
               MOVE UF0608                 TO CONSOLE-IO-AREA (13:8)
               MOVE 'UF0908: '             TO CONSOLE-IO-AREA (21:8)
               MOVE UF0908                 TO CONSOLE-IO-AREA (31:8)
               MOVE 'UF1808: '             TO CONSOLE-IO-AREA (41:8)
               MOVE UF1808                 TO CONSOLE-IO-AREA (49:8)
               DISPLAY CONSOLE-IO-AREA
               MOVE SPACES TO CONSOLE-IO-AREA
               INITIALIZE CONSOLE-IO-AREA
               MOVE '==> '                 TO CONSOLE-IO-AREA (1:4)
               MOVE DTOMEL                 TO CONSOLE-IO-AREA (4:57)
               DISPLAY CONSOLE-IO-AREA
           END-IF
           IF  (I-04 AND I-96)
               MOVE SPACES TO CONSOLE-IO-AREA
               INITIALIZE CONSOLE-IO-AREA
               MOVE '==> PROGRAM: RKO180 ' TO CONSOLE-IO-AREA (1:20)
               DISPLAY CONSOLE-IO-AREA
               MOVE SPACES TO CONSOLE-IO-AREA
               INITIALIZE CONSOLE-IO-AREA
               MOVE '==> '                 TO CONSOLE-IO-AREA (1:4)
               MOVE DTOMEL                 TO CONSOLE-IO-AREA (14:57)
               DISPLAY CONSOLE-IO-AREA
               MOVE SPACES TO CONSOLE-IO-AREA
               INITIALIZE CONSOLE-IO-AREA
               MOVE '==> REC:'             TO CONSOLE-IO-AREA (1:8)
               MOVE REC60                  TO CONSOLE-IO-AREA (11:60)
      *****************************************************************
      * DUMMY-LINJE FOR Å LAGE REF. TIL DATAFELT (FJERNE FEILMELDING) *
      *****************************************************************
               DISPLAY CONSOLE-IO-AREA
           END-IF.
 
       DETAIL-OVERFLOW SECTION.
       DETAIL-OVERFLOW-P.
           IF  (I-OF AND NOT-I-L2 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE '*** ALDERSFORDELT SALDOL' TO LISTE-IO-AREA (32:24)
               MOVE 'ISTE *** FOR'         TO LISTE-IO-AREA (56:12)
               IF  (I-U7)
                   MOVE 'ISTE ***    '     TO LISTE-IO-AREA (56:12)
               END-IF
               IF  (NOT-I-U7)
                   MOVE PERIOD             TO LISTE-IO-AREA (69:15)
               END-IF
               IF  (I-31 AND NOT-I-U7)
                   MOVE '19'               TO LISTE-IO-AREA (86:2)
               END-IF
               IF  (NOT-I-31 AND NOT-I-U7)
                   MOVE '20'               TO LISTE-IO-AREA (86:2)
               END-IF
               IF  (NOT-I-U7)
                   MOVE PAAR               TO LISTE-IO-AREA (88:2)
               END-IF
               MOVE 'FRAMSTILT'            TO LISTE-IO-AREA (100:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (110:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (124:4)
               IF  (I-L4)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (129:4)
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
               MOVE 'RESKNR BM N A V N'    TO LISTE-IO-AREA (1:17)
               MOVE '*'                    TO LISTE-IO-AREA (35:1)
               MOVE 'FORFALT SALDO ALDERFORDE' TO LISTE-IO-AREA (49:24)
               MOVE 'LT'                   TO LISTE-IO-AREA (73:2)
               MOVE 'S U M   *    S U M'   TO LISTE-IO-AREA (91:18)
               MOVE 'KRED.   S A L D O'    TO LISTE-IO-AREA (114:17)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'P O S T A D R E S S E' TO LISTE-IO-AREA (11:21)
               MOVE '*   0-30 DG  31-60 DG' TO LISTE-IO-AREA (35:21)
               MOVE '61-90 DG  3-6 MND  OV.6' TO LISTE-IO-AREA (59:23)
               MOVE 'MNDR   FORFALT  * IKKE F' TO LISTE-IO-AREA (83:24)
               MOVE 'ORFALT  GR.  PR.'     TO LISTE-IO-AREA (107:16)
               MOVE PDATO                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (124:8)
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
           IF  (I-L1 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE RESKNR                 TO LISTE-IO-AREA (1:6)
               MOVE BETBET                 TO LISTE-IO-AREA (8:2)
               IF  (NOT-I-90)
                   MOVE NAVN1              TO LISTE-IO-AREA (11:22)
               END-IF
               IF  (I-90)
                   MOVE 'IKKE I KUNDEARKIVET' TO LISTE-IO-AREA (11:19)
               END-IF
               MOVE '*'                    TO LISTE-IO-AREA (35:1)
               IF  (I-79)
                   MOVE 'I'                TO LISTE-IO-AREA (35:1)
               END-IF
               MOVE F30PR                  TO EDIT-F30PR
               MOVE EDIT-F30PR             TO LISTE-IO-AREA (38:9)
               INITIALIZE F30PR
               MOVE F60PR                  TO EDIT-F60PR
               MOVE EDIT-F60PR             TO LISTE-IO-AREA (48:9)
               INITIALIZE F60PR
               MOVE F90PR                  TO EDIT-F90PR
               MOVE EDIT-F90PR             TO LISTE-IO-AREA (58:9)
               INITIALIZE F90PR
               MOVE F180PR                 TO EDIT-F180PR
               MOVE EDIT-F180PR            TO LISTE-IO-AREA (68:9)
               INITIALIZE F180PR
               MOVE F6MDPR                 TO EDIT-F6MDPR
               MOVE EDIT-F6MDPR            TO LISTE-IO-AREA (78:9)
               INITIALIZE F6MDPR
               MOVE SUMFPR                 TO EDIT-SUMFPR
               MOVE EDIT-SUMFPR            TO LISTE-IO-AREA (89:11)
               INITIALIZE SUMFPR
               MOVE SUMUPR                 TO EDIT-SUMUPR
               MOVE EDIT-SUMUPR            TO LISTE-IO-AREA (102:11)
               INITIALIZE SUMUPR
               MOVE KRG-IO                 TO LISTE-IO-AREA (114:4)
               MOVE TOTSPR                 TO EDIT-TOTSPR
               MOVE EDIT-TOTSPR            TO LISTE-IO-AREA (119:14)
               INITIALIZE TOTSPR
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE PNR                    TO LISTE-IO-AREA (11:4)
               MOVE PSTED                  TO LISTE-IO-AREA (16:15)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L3 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALER GRUPPE'       TO LISTE-IO-AREA (1:14)
               MOVE RESKGR                 TO LISTE-IO-AREA (16:1)
               MOVE 'T O T A L T'          TO LISTE-IO-AREA (22:11)
               MOVE '0-30 DAGER      31-60 DA' TO LISTE-IO-AREA (40:24)
               MOVE 'GER      61-90 DAGER' TO LISTE-IO-AREA (64:20)
               MOVE '3-6 MNDR.      OVER 6 MN' TO LISTE-IO-AREA (91:24)
               MOVE 'DR.'                  TO LISTE-IO-AREA (115:3)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FORF. POSTER'         TO LISTE-IO-AREA (1:12)
               MOVE SFOFL3                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (19:14)
               INITIALIZE SFOFL3
               MOVE F30L3                  TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (36:14)
               INITIALIZE F30L3
               MOVE F60L3                  TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (53:14)
               INITIALIZE F60L3
               MOVE F90L3                  TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (70:14)
               INITIALIZE F90L3
               MOVE F180L3                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (87:14)
               INITIALIZE F180L3
               MOVE F6MDL3                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (104:14)
               INITIALIZE F6MDL3
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE SUMUL3                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (19:14)
               INITIALIZE SUMUL3
               MOVE U30L3                  TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (36:14)
               INITIALIZE U30L3
               MOVE U60L3                  TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (53:14)
               INITIALIZE U60L3
               MOVE U90L3                  TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (70:14)
               INITIALIZE U90L3
               MOVE U180L3                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (87:14)
               INITIALIZE U180L3
               MOVE U6MDL3                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (104:14)
               INITIALIZE U6MDL3
               MOVE 'UFORF. POSTER'        TO LISTE-IO-AREA (1:13)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'DEB  SALDO'           TO LISTE-IO-AREA (2:10)
               MOVE DESAL3                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (19:14)
               INITIALIZE DESAL3
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KRED SALDO'           TO LISTE-IO-AREA (2:10)
               MOVE KRSAL3                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (19:14)
               INITIALIZE KRSAL3
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE TOTSL3                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (18:15)
               INITIALIZE TOTSL3
               MOVE 'TOTAL SALDO'          TO LISTE-IO-AREA (1:11)
               MOVE FIRMA                  TO LISTE-IO-AREA (13:3)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L4 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALER FIRMA'        TO LISTE-IO-AREA (1:13)
               MOVE FIRMA                  TO LISTE-IO-AREA (16:3)
               MOVE '0-30 DAGER      31-60 DA' TO LISTE-IO-AREA (38:24)
               MOVE 'GER      61-90 DAGER' TO LISTE-IO-AREA (62:20)
               MOVE '3 - 6 MNDR.    OVER 6 MN' TO LISTE-IO-AREA (89:24)
               MOVE 'DR.'                  TO LISTE-IO-AREA (113:3)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FORF. POSTER'         TO LISTE-IO-AREA (1:12)
               MOVE SFOFGT                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (19:14)
               INITIALIZE SFOFGT
               MOVE F30GT                  TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (36:14)
               INITIALIZE F30GT
               MOVE F60GT                  TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (53:14)
               INITIALIZE F60GT
               MOVE F90GT                  TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (70:14)
               INITIALIZE F90GT
               MOVE F180GT                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (87:14)
               INITIALIZE F180GT
               MOVE F6MDGT                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (104:14)
               INITIALIZE F6MDGT
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'UFORF. POSTER'        TO LISTE-IO-AREA (1:13)
               MOVE SUMUGT                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (19:14)
               INITIALIZE SUMUGT
               MOVE U30GT                  TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (36:14)
               INITIALIZE U30GT
               MOVE U60GT                  TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (53:14)
               INITIALIZE U60GT
               MOVE U90GT                  TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (70:14)
               INITIALIZE U90GT
               MOVE U180GT                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (87:14)
               INITIALIZE U180GT
               MOVE U6MDGT                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (104:14)
               INITIALIZE U6MDGT
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'DEB  SALDO'           TO LISTE-IO-AREA (2:10)
               MOVE DESAGT                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (19:14)
               INITIALIZE DESAGT
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KRED SALDO'           TO LISTE-IO-AREA (2:10)
               MOVE KRSAGT                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (19:14)
               INITIALIZE KRSAGT
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTAL SALDO'          TO LISTE-IO-AREA (1:11)
               MOVE TOTSGT                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (18:15)
               INITIALIZE TOTSGT
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'A L D E R S F O R D E L' TO LISTE-IO-AREA (10:23)
               MOVE 'T  S A L D O L I S T E' TO LISTE-IO-AREA (34:22)
               MOVE 'P R.'                 TO LISTE-IO-AREA (59:4)
               MOVE PDATO                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (65:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'G R A N D T O T A L'  TO LISTE-IO-AREA (10:19)
               MOVE TOTGRT                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (32:15)
               INITIALIZE TOTGRT
               MOVE 01                     TO LISTE-AFTER-SKIP
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'AVSTEMMING PROG. RKO180 ' TO LISTE-IO-AREA (1:24)
               MOVE 'DATO'                 TO LISTE-IO-AREA (46:4)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (52:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-BEFORE-SPACE
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'AVSTEMMES MOT KONTOKURA' TO LISTE-IO-AREA (10:23)
               MOVE 'NT (ÅRLIG I JOBB RES50A' TO LISTE-IO-AREA (33:23)
               MOVE 'M, PROGRAM RSK227).    ' TO LISTE-IO-AREA (56:23)
               MOVE PDATO                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (85:8)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'GRP'                  TO LISTE-IO-AREA (7:3)
               MOVE 'BELØP'                TO LISTE-IO-AREA (27:5)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '0'                    TO LISTE-IO-AREA (8:1)
               MOVE SLR (1)                TO XO-112YY9R
               MOVE XO-112YY9R             TO LISTE-IO-AREA (15:18)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '1'                    TO LISTE-IO-AREA (8:1)
               MOVE SLR (2)                TO XO-112YY9R
               MOVE XO-112YY9R             TO LISTE-IO-AREA (15:18)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '2'                    TO LISTE-IO-AREA (8:1)
               MOVE SLR (3)                TO XO-112YY9R
               MOVE XO-112YY9R             TO LISTE-IO-AREA (15:18)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '3'                    TO LISTE-IO-AREA (8:1)
               MOVE SLR (4)                TO XO-112YY9R
               MOVE XO-112YY9R             TO LISTE-IO-AREA (15:18)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '4'                    TO LISTE-IO-AREA (8:1)
               MOVE SLR (5)                TO XO-112YY9R
               MOVE XO-112YY9R             TO LISTE-IO-AREA (15:18)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '5'                    TO LISTE-IO-AREA (8:1)
               MOVE SLR (6)                TO XO-112YY9R
               MOVE XO-112YY9R             TO LISTE-IO-AREA (15:18)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '6'                    TO LISTE-IO-AREA (8:1)
               MOVE SLR (7)                TO XO-112YY9R
               MOVE XO-112YY9R             TO LISTE-IO-AREA (15:18)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '7'                    TO LISTE-IO-AREA (8:1)
               MOVE SLR (8)                TO XO-112YY9R
               MOVE XO-112YY9R             TO LISTE-IO-AREA (15:18)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '8'                    TO LISTE-IO-AREA (8:1)
               MOVE SLR (9)                TO XO-112YY9R
               MOVE XO-112YY9R             TO LISTE-IO-AREA (15:18)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '9'                    TO LISTE-IO-AREA (8:1)
               MOVE SLR (10)               TO XO-112YY9R
               MOVE XO-112YY9R             TO LISTE-IO-AREA (15:18)
      *****************************************************************
      * FEILMELDINGER PÅ KONSOLLET:                                   *
      *****************************************************************
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-U1 AND I-U2 AND I-U3)
           AND (I-99 AND I-U8 AND I-U7)
           AND (I-05 AND I-55 AND I-97)
           AND (I-98 AND I-03)
               MOVE SPACES TO CONSOLE-IO-AREA
               INITIALIZE CONSOLE-IO-AREA
               MOVE PSDS                   TO CONSOLE-IO-AREA (1:80)
               MOVE R                      TO CONSOLE-IO-AREA (73:8)
               MOVE P-IO                   TO CONSOLE-IO-AREA (78:3)
               MOVE S-IO                   TO CONSOLE-IO-AREA (76:5)
               MOVE DTODTO                 TO CONSOLE-IO-AREA (75:6)
               MOVE DTOMEL                 TO CONSOLE-IO-AREA (24:57)
               MOVE LONR                   TO CONSOLE-IO-AREA (116:5)
               MOVE LFIRMA                 TO CONSOLE-IO-AREA (118:3)
               MOVE LUNDGR                 TO CONSOLE-IO-AREA (118:3)
               MOVE LPROG                  TO CONSOLE-IO-AREA (113:8)
               MOVE LOPNVN                 TO CONSOLE-IO-AREA (86:35)
               MOVE LANTX-IO               TO CONSOLE-IO-AREA (118:3)
               MOVE LPRIID                 TO CONSOLE-IO-AREA (117:4)
               MOVE BJOBN                  TO CONSOLE-IO-AREA (113:8)
               MOVE BBEST                  TO CONSOLE-IO-AREA (120:1)
               MOVE BPERS                  TO CONSOLE-IO-AREA (91:30)
               MOVE BETTB                  TO CONSOLE-IO-AREA (81:40)
               MOVE BFORS                  TO CONSOLE-IO-AREA (81:40)
               MOVE BMEMO                  TO CONSOLE-IO-AREA (81:40)
               MOVE BANTX-IO               TO CONSOLE-IO-AREA (118:3)
               MOVE BPCLAS                 TO CONSOLE-IO-AREA (120:1)
               MOVE BPRJE                  TO CONSOLE-IO-AREA (118:3)
               DISPLAY CONSOLE-IO-AREA
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
           MOVE 1                          TO LR-CHECK
           INITIALIZE PARAM-DATA-FIELDS
           SET PARAM-EOF-OFF               TO TRUE
           SET PARAM-PROCESS               TO TRUE
           OPEN INPUT PARAM
           INITIALIZE RESPAR-DATA-FIELDS
           SET RESPAR-EOF-OFF              TO TRUE
           SET RESPAR-PROCESS              TO TRUE
           OPEN INPUT RESPAR
           SET INFILE-LEVEL-INIT           TO TRUE
           INITIALIZE INFILE-DATA-FIELDS
           SET INFILE-EOF-OFF              TO TRUE
           SET INFILE-PROCESS              TO TRUE
           OPEN INPUT INFILE
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           INITIALIZE SYSPARM-DATA-FIELDS
           OPEN INPUT SYSPARM
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           PERFORM VARYING SLR-I FROM 1 BY 1
                     UNTIL SLR-I > SLR-MAX
               INITIALIZE SLR (SLR-I)
           END-PERFORM
           SET SLR-I                       TO 1
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PARAM
           CLOSE RESPAR
           CLOSE INFILE
           CLOSE KUNDEMA
           CLOSE SYSPARM
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
