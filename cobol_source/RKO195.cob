       IDENTIFICATION DIVISION.
       PROGRAM-ID. RKO195R.
      *    KONV. IFRA RSK195 UTVIDET RECORD.     ***TXT***ok ss***    *
      **********************************************  Z-WIN-RPG2   ****
      **OBS ved endring i excel på Report Web *****************
      * PROGRAM RKO195                                                *
      * L E V E R A N D Ø R R E S K O N T R O                         *
      * FORFALLSLISTE PR FORFALLSDAG                                  *
      * TOTALER PR VALUTA I UTENLANDSK MYNT OG NORSKE KRONER          *        *
      * ENDRET: 15.11.94 LAGT INN VALUTATABELL, SKRIVER UT VALUTABELØP*        *
      *                  OG VALUTATYPE.                               *
      *         24.07.98 TILPASSET ÅR 2000.                           *        *
      *         17.08.98 ENDRET TEKST I FEILMELDING.                  *
      *         18.08.98 SETTER PÅ H0 VED FEIL I PARAMDATO.           *
      *         23.09.98 NY VERSJON AV ADVALUTA.                      *
      *         20.12.01 SATT INN KJØREDATO PÅ LISTEN.                *
      *         03.01.02 UTVIDET VALUTATABELL.                        *
      ***************************************************************** ********
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RKO195.rpg
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
           SELECT RESPAR
               ASSIGN TO UT-S-RESPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RESPAR-STATUS.
           SELECT RESFILE
               ASSIGN TO UT-S-RESFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RESFILE-STATUS.
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
       DATA DIVISION.
       FILE SECTION.
       FD RESPAR
               BLOCK CONTAINS 300
               RECORD CONTAINS 300.
       01  RESPAR-IO-AREA.
           05  RESPAR-IO-AREA-X            PICTURE X(300).
       FD RESFILE
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  RESFILE-IO-AREA.
           05  RESFILE-IO-AREA-X           PICTURE X(200).
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1            PICTURE X(9).
               10  FILLER                  PICTURE X(190).
      *BUGFILO O   F  80  80            PRINTERSYSLST
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       WORKING-STORAGE SECTION.
       77  AAD-MAX   VALUE 22              PICTURE 9(4) USAGE BINARY.
       77  ASM-MAX   VALUE 22              PICTURE 9(4) USAGE BINARY.
       77  AST-MAX   VALUE 22              PICTURE 9(4) USAGE BINARY.
       77  ATX-MAX   VALUE 22              PICTURE 9(4) USAGE BINARY.
       77  ABL-MAX   VALUE 22              PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  AAD-TABLE.
               10  AAD-ENTRY
                                           OCCURS 22 TIMES
                                           INDEXED BY AAD-I
                                                      AAD-S.
                   15  AAD                 PICTURE X(1).
           05  ASM-TABLE.
               10  ASM-ENTRY
                                           OCCURS 22 TIMES
                                           INDEXED BY ASM-I
                                                      ASM-S.
                   15  ASM                 PICTURE S9(8)V9(2).
           05  AST-TABLE.
               10  AST-ENTRY
                                           OCCURS 22 TIMES
                                           INDEXED BY AST-I
                                                      AST-S.
                   15  AST                 PICTURE X(3).
           05  ATX-TABLE.
               10  ATX-ENTRY
                                           OCCURS 22 TIMES
                                           INDEXED BY ATX-I
                                                      ATX-S.
                   15  ATX                 PICTURE X(20).
      * **START RBS - DATASTRUKTUR FOR SUB-PROGRAM RBS000 ********
           05  ABL-TABLE.
               10  ABL-ENTRY
                                           OCCURS 22 TIMES
                                           INDEXED BY ABL-I
                                                      ABL-S.
                   15  ABL                 PICTURE S9(8)V9(2).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  RESPAR-STATUS               PICTURE 99 VALUE 0.
           10  RESFILE-STATUS              PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  VALPAR-XX-STATUS            PICTURE 99 VALUE 0.
           10  TKDATA-XX-STATUS            PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
           10  DTOPAR-XX-STATUS            PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
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
               88  RESFILE-EOF-OFF         VALUE '0'.
               88  RESFILE-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESFILE-READ-OFF        VALUE '0'.
               88  RESFILE-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESFILE-PROCESS-OFF     VALUE '0'.
               88  RESFILE-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  RESFILE-LEVEL-INIT-OFF  VALUE '0'.
               88  RESFILE-LEVEL-INIT      VALUE '1'.
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
      * * START - DATASTRUKTUR FOR SUB-PROGRAM ADVALUTA ********
      *DSDS: DATA STRUCTURE FIELDS
           05  VALPAR-XX-DATA-FIELDS.
               10  AKSEKD                  PICTURE X(1).
               10  FILLER                  PICTURE X(256).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(1).
               10  ADVK                    PICTURE X(1).
               10  FILLER                  PICTURE X(255).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(2).
               10  STDVK                   PICTURE X(3).
               10  FILLER                  PICTURE X(252).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(5).
               10  NORVN                   PICTURE X(20).
               10  FILLER                  PICTURE X(232).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(25).
               10  STDVN                   PICTURE X(20).
               10  FILLER                  PICTURE X(212).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(45).
               10  LANDKD                  PICTURE X(2).
               10  FILLER                  PICTURE X(210).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(47).
               10  LAND                    PICTURE X(20).
               10  FILLER                  PICTURE X(190).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(67).
               10  VALIX-IO.
                   15  VALIX               PICTURE S9(3).
               10  FILLER                  PICTURE X(187).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(70).
               10  DIV                     PICTURE X(10).
               10  FILLER                  PICTURE X(177).
      * * END - DATASTRUKTUR FOR SUB-PROGRAM ADVALUTA ********
           05  TKDATA-XX REDEFINES VALPAR-XX-DATA-FIELDS.
               10  TKTK                    PICTURE X(2).
               10  FILLER                  PICTURE X(255).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(2).
               10  TKTEXT                  PICTURE X(7).
               10  FILLER                  PICTURE X(248).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(9).
               10  TKBILN                  PICTURE X(6).
               10  FILLER                  PICTURE X(242).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(15).
               10  TKREFN                  PICTURE X(6).
               10  FILLER                  PICTURE X(236).
           05  LDATA-XX REDEFINES VALPAR-XX-DATA-FIELDS.
               10  LONR                    PICTURE X(5).
               10  FILLER                  PICTURE X(252).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(5).
               10  LFIRMA                  PICTURE X(3).
               10  FILLER                  PICTURE X(249).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(8).
               10  LUNDGR                  PICTURE X(3).
               10  FILLER                  PICTURE X(246).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(11).
               10  LPROG                   PICTURE X(8).
               10  FILLER                  PICTURE X(238).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(19).
               10  LANTX-IO.
                   15  LANTX               PICTURE S9(3).
               10  FILLER                  PICTURE X(235).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(22).
               10  FINAVN                  PICTURE X(30).
               10  FILLER                  PICTURE X(205).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(52).
               10  LOPNVN                  PICTURE X(35).
               10  FILLER                  PICTURE X(170).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(87).
               10  LPRIID                  PICTURE X(4).
               10  FILLER                  PICTURE X(166).
      *     *  BESTILLINGSOPPGAVER (OVERSTYRING AV RBS-FILE) *    *
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(91).
               10  BJOBN                   PICTURE X(8).
               10  FILLER                  PICTURE X(158).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(99).
               10  BBEST                   PICTURE X(1).
               10  FILLER                  PICTURE X(157).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(100).
               10  BPERS                   PICTURE X(30).
               10  FILLER                  PICTURE X(127).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(130).
               10  BETTB                   PICTURE X(40).
               10  FILLER                  PICTURE X(87).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(170).
               10  BFORS                   PICTURE X(40).
               10  FILLER                  PICTURE X(47).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(210).
               10  BMEMO                   PICTURE X(40).
               10  FILLER                  PICTURE X(7).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(250).
               10  BANTX-IO.
                   15  BANTX               PICTURE S9(3).
               10  FILLER                  PICTURE X(4).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(253).
               10  BPCLAS                  PICTURE X(1).
               10  FILLER                  PICTURE X(3).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(254).
               10  BPRJE                   PICTURE X(3).
      * * END - RBS - DATASTRUKTUR FOR SUB-PROGRAM RBSH01 ********
      *****************************************************************
      * DATASTRUKTUR FOR SUBPROGRAM DATO8SIF                          *
      *****************************************************************
           05  DTOPAR-XX REDEFINES VALPAR-XX-DATA-FIELDS.
               10  DTOKOD                  PICTURE X(1).
               10  FILLER                  PICTURE X(256).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(1).
               10  DTODTO                  PICTURE X(6).
               10  FILLER                  PICTURE X(250).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(15).
               10  AAR4SI-IO.
                   15  AAR4SI              PICTURE S9(4).
               10  FILLER                  PICTURE X(238).
           05  FILLER REDEFINES VALPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(23).
               10  DTOMEL                  PICTURE X(57).
               10  FILLER                  PICTURE X(177).
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
      *****************************************************************
      * DATASTRUKTUR FOR SUBPROGRAM DATO8SIF                          *
      *****************************************************************
           05  DTOPAR-XX REDEFINES TKDATA-XX-DATA-FIELDS.
               10  DTOKOD                  PICTURE X(1).
               10  FILLER                  PICTURE X(256).
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(1).
               10  DTODTO                  PICTURE X(6).
               10  FILLER                  PICTURE X(250).
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(15).
               10  AAR4SI-IO.
                   15  AAR4SI              PICTURE S9(4).
               10  FILLER                  PICTURE X(238).
           05  FILLER REDEFINES TKDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(23).
               10  DTOMEL                  PICTURE X(57).
               10  FILLER                  PICTURE X(177).
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
      *****************************************************************
      * DATASTRUKTUR FOR SUBPROGRAM DATO8SIF                          *
      *****************************************************************
           05  DTOPAR-XX REDEFINES LDATA-XX-DATA-FIELDS.
               10  DTOKOD                  PICTURE X(1).
               10  FILLER                  PICTURE X(256).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(1).
               10  DTODTO                  PICTURE X(6).
               10  FILLER                  PICTURE X(250).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(15).
               10  AAR4SI-IO.
                   15  AAR4SI              PICTURE S9(4).
               10  FILLER                  PICTURE X(238).
           05  FILLER REDEFINES LDATA-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(23).
               10  DTOMEL                  PICTURE X(57).
               10  FILLER                  PICTURE X(177).
      *DSDS: DATA STRUCTURE FIELDS
           05  DTOPAR-XX-DATA-FIELDS.
               10  DTOKOD                  PICTURE X(1).
               10  FILLER                  PICTURE X(79).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(1).
               10  DTODTO                  PICTURE X(6).
               10  FILLER                  PICTURE X(73).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(15).
               10  AAR4SI-IO.
                   15  AAR4SI              PICTURE S9(4).
               10  FILLER                  PICTURE X(61).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(23).
               10  DTOMEL                  PICTURE X(57).
           05  RESPAR-DATA-FIELDS.
               10  PDATO-IO.
                   15  PDATO               PICTURE S9(6).
               10  PDAG-IO.
                   15  PDAG                PICTURE S9(2).
               10  PMND-IO.
                   15  PMND                PICTURE S9(2).
      *                                      11  120PÅR
           05  RESFILE-LEVEL-02.
               10  RESFILE-02-L4.
                   15  RESFILE-02-L4-FIRMA PICTURE X(3).
               10  RESFILE-02-L3.
                   15  RESFILE-02-L3-GRP   PICTURE X(1).
               10  RESFILE-02-L2.
                   15  RESFILE-02-L2-FDATO PICTURE X(6).
               10  RESFILE-02-L1.
                   15  RESFILE-02-L1-RESKNR PICTURE X(6).
           05  RESFILE-DATA-FIELDS.
               10  REC60                   PICTURE X(60).
               10  RESKEY                  PICTURE X(9).
               10  FIRMA                   PICTURE X(3).
               10  RESKNR                  PICTURE X(6).
               10  TRKODE                  PICTURE X(2).
               10  BA-ELGR                 PICTURE X(2).
               10  BMND                    PICTURE X(2).
               10  BDAG                    PICTURE X(2).
               10  BNR                     PICTURE X(6).
               10  BILNR                   PICTURE X(6).
               10  REFNR                   PICTURE X(6).
               10  FA-ELGR-IO.
                   15  FA-ELGR             PICTURE S9(2).
               10  FMND-IO.
                   15  FMND                PICTURE S9(2).
               10  FDAG-IO.
                   15  FDAG                PICTURE S9(2).
               10  FDATO                   PICTURE X(6).
               10  BEL-IO.
                   15  BEL                 PICTURE S9(7)V9(2).
               10  GRP                     PICTURE X(1).
      *                                      61  62 BMÅTE
               10  VALUTA-IO.
                   15  VALUTA              PICTURE S9(8)V9(2).
      *                                      74  74 BART
               10  VTKODE                  PICTURE X(1).
      *                                      77  77 GIRO
           05  KUNDEMA-DATA-FIELDS.
      *                                       6  11 KUNDE
               10  NAVN                    PICTURE X(30).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L4            PICTURE X(3).
               10  THE-PRIOR-L3            PICTURE X(1).
               10  THE-PRIOR-L2            PICTURE X(6).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  FFDAG-IO.
                   15  FFDAG               PICTURE S9(6).
               10  FFMND-IO.
                   15  FFMND               PICTURE S9(3).
               10  PDAGER-IO.
                   15  PDAGER              PICTURE S9(6).
               10  DAGSUM-IO.
                   15  DAGSUM              PICTURE S9(8)V9(2).
               10  TOTSUM-IO.
                   15  TOTSUM              PICTURE S9(8)V9(2).
               10  SUMINL-IO.
                   15  SUMINL              PICTURE S9(8)V9(2).
               10  SUMTOT-IO.
                   15  SUMTOT              PICTURE S9(8)V9(2).
               10  FDAGER-IO.
                   15  FDAGER              PICTURE S9(6).
               10  SUMFF-IO.
                   15  SUMFF               PICTURE S9(8)V9(2).
               10  NDAGER-IO.
                   15  NDAGER              PICTURE S9(5).
               10  SUM90-IO.
                   15  SUM90               PICTURE S9(8)V9(2).
               10  SUM60-IO.
                   15  SUM60               PICTURE S9(8)V9(2).
               10  SUM30-IO.
                   15  SUM30               PICTURE S9(8)V9(2).
               10  SUM00-IO.
                   15  SUM00               PICTURE S9(8)V9(2).
               10  X-IO.
                   15  X                   PICTURE S9(3).
               10  SUMABL-IO.
                   15  SUMABL              PICTURE S9(8)V9(2).
               10  TEKST                   PICTURE X(7).
           05  EDITTING-FIELDS.
               10  XO-40YNZ                PICTURE ZZZZ.
               10  XO-82YY9R               PICTURE ZZ.ZZZ.ZZZ,99-.
               10  XO-72YY9R               PICTURE Z.ZZZ.ZZZ,99-.
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
 
           IF  RESFILE-PROCESS
               SET RESFILE-PROCESS-OFF     TO TRUE
               SET RESFILE-READ            TO TRUE
           END-IF
 
           IF  RESFILE-READ
           AND RECORD-SELECTED-OFF
               PERFORM RESFILE-GET
               SET RESFILE-READ-OFF        TO TRUE
               IF  NOT RESFILE-EOF
                   PERFORM RESFILE-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET RESFILE-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  RESPAR-PROCESS
               PERFORM RESPAR-IDSET
           END-IF
 
           IF  RESFILE-PROCESS
               PERFORM RESFILE-IDSET
           END-IF
 
           IF  RESFILE-PROCESS
               PERFORM RESFILE-CHK-LEVEL
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
 
           IF  RESPAR-PROCESS
               PERFORM RESPAR-FLDSET
           END-IF
 
           IF  RESFILE-PROCESS
               PERFORM RESFILE-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  RESFILE-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-87)
               SET I-H0                    TO TRUE
      *****
      ** SETT INDIKATOR FOR FØRSTE BEHANDLING, FYLL VALUTATABELL.
           END-IF
           SET NOT-I-88                    TO TRUE
           IF  (NOT-I-89)
               SET I-88                    TO TRUE
               SET I-89                    TO TRUE
           END-IF
           IF  (I-88)
               PERFORM VALRUT-S
      *****
      ** PARAMETER
      *****
      *  01                MOVE "PDATO   "BUGFL1  8        LEDETXT DEBUG
      *  01      BUGFL1    DEBUGBUGFILO   PDATO            VIS FELT/IND
           END-IF
           IF  (I-01)
               MOVE 'A'                    TO DTOKOD
               MOVE PDATO                  TO DTODTO
               PERFORM DTORUT-S
           END-IF
           IF  (I-01 AND I-99)
               SET I-87                    TO TRUE
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               MULTIPLY 360 BY AAR4SI  GIVING FFDAG
               MULTIPLY 30 BY PMND     GIVING FFMND
               ADD FFMND TO FFDAG      GIVING PDAGER
               ADD PDAG                    TO PDAGER
               GO TO UT-T
      *****
      ** RESKONTRORECORD
      ** FORFALTE BELØP
      **
      *****
           END-IF
           IF  (I-L4)
               PERFORM RBSRUT-S
           END-IF
           IF  (I-86)
               GO TO UT-T
           END-IF
           IF  (I-L4)
               SUBTRACT PAGE0              FROM PAGE0
               SUBTRACT SUMABL             FROM SUMABL
               SUBTRACT SUMINL             FROM SUMINL
               SUBTRACT SUMTOT             FROM SUMTOT
           END-IF
           IF  (I-L3)
               SET NOT-I-90                TO TRUE
               IF  GRP = '1'
                   SET I-90                TO TRUE
               END-IF
               SET NOT-I-91                TO TRUE
               IF  GRP = '2'
                   SET I-91                TO TRUE
               END-IF
           END-IF
           IF  (I-02)
               ADD BEL                     TO DAGSUM
               ADD BEL                     TO TOTSUM
           END-IF
           IF  (I-02 AND I-91)
               ADD BEL                     TO SUMINL
               ADD BEL                     TO SUMTOT
           END-IF
           MOVE 'B'                        TO DTOKOD
           MOVE FDATO                      TO DTODTO
           PERFORM DTORUT-S
           IF  (NOT-I-99)
               MULTIPLY 360 BY AAR4SI  GIVING FFDAG
           END-IF
           IF  (I-99)
               MULTIPLY 360 BY FA-ELGR GIVING FFDAG
           END-IF
           MULTIPLY 30 BY FMND         GIVING FFMND
           ADD FFMND TO FFDAG          GIVING FDAGER
           ADD FDAG                        TO FDAGER
           SET NOT-I-11                    TO TRUE
           SET NOT-I-12                    TO TRUE
           IF  PDAGER NOT > FDAGER
               SET I-12                    TO TRUE
           END-IF
           IF  PDAGER > FDAGER
               SET I-11                    TO TRUE
           END-IF
      *                    MOVE "FDAGER  "BUGFL1  8        LEDETXT DEBUG
      *          BUGFL1    DEBUGBUGFILO   FDAGER           VIS FELT/IND
           IF  (I-12)
               GO TO A1-T
           END-IF
           IF  (I-11)
               ADD BEL                     TO SUMFF
               GO TO VAL-T
           END-IF.
 
       A1-T.
           SUBTRACT PDAGER FROM FDAGER GIVING NDAGER
           SET NOT-I-13                    TO TRUE
           SET NOT-I-14                    TO TRUE
           IF  NDAGER NOT > 90
               SET I-14                    TO TRUE
           END-IF
           IF  NDAGER > 90
               SET I-13                    TO TRUE
           END-IF
           IF  (I-14)
               GO TO A2-T
           END-IF
           IF  (I-13)
               ADD BEL                     TO SUM90
               GO TO VAL-T
           END-IF.
 
       A2-T.
           SET NOT-I-15                    TO TRUE
           SET NOT-I-16                    TO TRUE
           IF  NDAGER NOT > 60
               SET I-16                    TO TRUE
           END-IF
           IF  NDAGER > 60
               SET I-15                    TO TRUE
           END-IF
           IF  (I-16)
               GO TO A3-T
           END-IF
           IF  (I-15)
               ADD BEL                     TO SUM60
               GO TO VAL-T
           END-IF.
 
       A3-T.
           SET NOT-I-17                    TO TRUE
           SET NOT-I-18                    TO TRUE
           IF  NDAGER NOT > 30
               SET I-18                    TO TRUE
           END-IF
           IF  NDAGER > 30
               SET I-17                    TO TRUE
           END-IF
           IF  (I-17)
               ADD BEL                     TO SUM30
           END-IF
           IF  (I-18)
               ADD BEL                     TO SUM00
           END-IF.
 
       VAL-T.
           IF  (I-02)
               MOVE RESKEY                 TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-75                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-75            TO TRUE
                   PERFORM KUNDEMA-FLDSET
                   PERFORM KUNDEMA-IDSET
               END-READ
           END-IF
           IF  (I-02 AND I-90)
               MOVE 1                      TO X
               SET NOT-I-19                TO TRUE
               SET AAD-S                   TO 1
               PERFORM WITH TEST AFTER
                       VARYING AAD-I FROM X BY 1
                         UNTIL AAD-I >= AAD-MAX
                            OR I-19
                   IF  VTKODE = AAD (AAD-I)
                       SET I-19            TO TRUE
                       SET AAD-S           TO AAD-I
                   END-IF
               END-PERFORM
               SET X                       TO AAD-S
           END-IF
           IF  (I-90 AND I-19)
               ADD VALUTA                  TO ASM (X)
               ADD BEL                     TO ABL (X)
               ADD BEL                     TO SUMABL
               ADD BEL                     TO SUMTOT
      ******************************************************
      ** TEKST IFØLGE TRANSKODE
           END-IF
           PERFORM TKRUT-S
      *
           .
 
       UT-T.
           CONTINUE.
 
       VALRUT-S SECTION.
       VALRUT-S-P.
           MOVE 0                          TO X.
 
       LESLOP-T.
           ADD 1                           TO X
           SET NOT-I-96                    TO TRUE
           MOVE 'I'                        TO AKSEKD
           MOVE X                          TO VALIX-IO
           CALL 'ADVALUTA' USING VALPAR-XX-DATA-FIELDS
           MOVE ADVK                       TO AAD (X)
           MOVE STDVK                      TO AST (X)
           MOVE STDVN                      TO ATX (X)
           SET NOT-I-52                    TO TRUE
           IF  X = 22
               SET I-52                    TO TRUE
           END-IF
           IF  (NOT-I-52)
               GO TO LESLOP-T
           END-IF.
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
           MOVE TKTEXT                     TO TEKST.
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE ' '                        TO BBEST
           MOVE 'RES13'                    TO LONR
           MOVE FIRMA                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'RKO195  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF.
      *****************************************************************
      *  SUBRUTINE FOR Å HENTE 8-SIFRET DATO.                         * *
      *****************************************************************
 
       DTORUT-S SECTION.
       DTORUT-S-P.
           SET NOT-I-99                    TO TRUE
           CALL 'DATO8SIF' USING DTOPAR-XX-DATA-FIELDS
           SET NOT-I-99                    TO TRUE
           IF  DTOKOD = 'F'
               SET I-99                    TO TRUE
           END-IF.
      ******************************************************
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L0 AND I-86)
               GO TO SLUTT-T
           END-IF
           IF  (I-L2)
               SET I-94                    TO TRUE
           END-IF
           IF  (I-L2 AND I-94)
               PERFORM EXCEPTION-OUTPUT
           END-IF
           IF  (I-L2)
               SET NOT-I-94                TO TRUE
      *****
           END-IF
           IF  (I-L4)
               MOVE 0                      TO X
           END-IF.
 
       LOOP-T.
           IF  (I-L4)
               ADD 1                       TO X
               SET NOT-I-50                TO TRUE
               IF  ASM (X) NOT = 0
                   SET I-50                TO TRUE
               END-IF
               SET NOT-I-51                TO TRUE
               IF  X = 22
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-L4 AND I-50 AND NOT-I-95)
               SET I-92                    TO TRUE
               SET I-93                    TO TRUE
               SET I-95                    TO TRUE
           END-IF
           IF  (I-L4 AND I-92 AND I-93 AND I-95)
               PERFORM EXCEPTION-OUTPUT
               SET NOT-I-50                TO TRUE
           END-IF
           IF  (I-L4)
               SET NOT-I-92                TO TRUE
               SET NOT-I-93                TO TRUE
           END-IF
           IF  (I-L4 AND I-50)
               PERFORM EXCEPTION-OUTPUT
           END-IF
           IF  (I-L4 AND NOT-I-51)
               GO TO LOOP-T
           END-IF
           IF  (I-L4)
               SET NOT-I-50                TO TRUE
           END-IF
           IF  (I-L4 AND I-95)
               SET I-53                    TO TRUE
           END-IF
           IF  (I-L4 AND I-53)
               PERFORM EXCEPTION-OUTPUT
           END-IF
           IF  (I-L4)
               SET NOT-I-53                TO TRUE
           END-IF
           IF  (I-L4 AND I-95)
               SET I-54                    TO TRUE
           END-IF
           IF  (I-L4 AND I-54)
               PERFORM EXCEPTION-OUTPUT
           END-IF
           IF  (I-L4)
               SET NOT-I-54                TO TRUE
               SET NOT-I-95                TO TRUE
           END-IF
           IF  (I-L4 AND I-51)
               GO TO SLUTT-T
           END-IF.
 
       SLUTT-T.
      *****************************************************************
      *  RUTINE FOR Å HENTE VALUTAKODER.                              *
      *****************************************************************
           CONTINUE.
 
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
           WHEN ( RESPAR-IO-AREA (1:1) = '9'
            AND   RESPAR-IO-AREA (2:1) = '0' )
               MOVE RESPAR-IO-AREA (7:6)   TO PDATO-IO
               INSPECT PDATO-IO REPLACING ALL ' ' BY '0'
               MOVE RESPAR-IO-AREA (7:2)   TO PDAG-IO
               INSPECT PDAG-IO REPLACING ALL ' ' BY '0'
               MOVE RESPAR-IO-AREA (9:2)   TO PMND-IO
               INSPECT PMND-IO REPLACING ALL ' ' BY '0'
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
 
       RESFILE-GET SECTION.
       RESFILE-GET-P.
           IF  RESFILE-EOF-OFF
               READ RESFILE
               AT END
                   SET RESFILE-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RESFILE-FLDSET SECTION.
       RESFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ( RESFILE-IO-AREA (1:1) = '0'
            AND   RESFILE-IO-AREA (2:1) = '2' )
               MOVE RESFILE-IO-AREA (1:60) TO REC60 (1:60)
               MOVE RESFILE-IO-AREA (3:9)  TO RESKEY (1:9)
               MOVE RESFILE-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE RESFILE-IO-AREA (6:6)  TO RESKNR (1:6)
               MOVE RESFILE-IO-AREA (22:2) TO TRKODE (1:2)
               MOVE RESFILE-IO-AREA (24:2) TO BA-ELGR (1:2)
               MOVE RESFILE-IO-AREA (26:2) TO BMND (1:2)
               MOVE RESFILE-IO-AREA (28:2) TO BDAG (1:2)
               MOVE RESFILE-IO-AREA (30:6) TO BNR (1:6)
               MOVE RESFILE-IO-AREA (30:6) TO BILNR (1:6)
               MOVE RESFILE-IO-AREA (36:6) TO REFNR (1:6)
               MOVE RESFILE-IO-AREA (42:2) TO FA-ELGR-IO
               INSPECT FA-ELGR-IO REPLACING ALL ' ' BY '0'
               MOVE RESFILE-IO-AREA (44:2) TO FMND-IO
               INSPECT FMND-IO REPLACING ALL ' ' BY '0'
               MOVE RESFILE-IO-AREA (46:2) TO FDAG-IO
               INSPECT FDAG-IO REPLACING ALL ' ' BY '0'
               MOVE RESFILE-IO-AREA (42:6) TO FDATO (1:6)
               MOVE RESFILE-IO-AREA (48:9) TO BEL-IO
               INSPECT BEL-IO REPLACING ALL ' ' BY '0'
               MOVE RESFILE-IO-AREA (89:1) TO GRP (1:1)
               MOVE RESFILE-IO-AREA (64:10) TO VALUTA-IO
               INSPECT VALUTA-IO REPLACING ALL ' ' BY '0'
               MOVE RESFILE-IO-AREA (76:1) TO VTKODE (1:1)
           END-EVALUATE.
 
       RESFILE-IDCHK SECTION.
       RESFILE-IDCHK-P.
           EVALUATE TRUE
           WHEN ( RESFILE-IO-AREA (1:1) = '0'
            AND   RESFILE-IO-AREA (2:1) = '2' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       RESFILE-IDSET SECTION.
       RESFILE-IDSET-P.
           EVALUATE TRUE
           WHEN ( RESFILE-IO-AREA (1:1) = '0'
            AND   RESFILE-IO-AREA (2:1) = '2' )
               SET I-02                    TO TRUE
           END-EVALUATE.
 
       RESFILE-CHK-LEVEL SECTION.
       RESFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( RESFILE-IO-AREA (1:1) = '0'
            AND   RESFILE-IO-AREA (2:1) = '2' )
               MOVE LOW-VALUES             TO RESFILE-LEVEL-02
               MOVE RESFILE-IO-AREA (3:3)  TO RESFILE-02-L4-FIRMA
               MOVE RESFILE-IO-AREA (89:1) TO RESFILE-02-L3-GRP
               MOVE RESFILE-IO-AREA (42:6) TO RESFILE-02-L2-FDATO
               MOVE RESFILE-IO-AREA (6:6)  TO RESFILE-02-L1-RESKNR
               IF  RESFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  RESFILE-02-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   WHEN  RESFILE-02-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  RESFILE-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  RESFILE-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  RESFILE-02-L4         TO THE-PRIOR-L4
               MOVE  RESFILE-02-L3         TO THE-PRIOR-L3
               MOVE  RESFILE-02-L2         TO THE-PRIOR-L2
               MOVE  RESFILE-02-L1         TO THE-PRIOR-L1
               SET RESFILE-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (16:30) TO NAVN (1:30)
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
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
           IF  (I-02 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FDAG-IO                TO LISTE-IO-AREA (2:2)
               MOVE '.'                    TO LISTE-IO-AREA (4:1)
               MOVE FMND-IO                TO LISTE-IO-AREA (5:2)
               MOVE '.'                    TO LISTE-IO-AREA (7:1)
               MOVE FA-ELGR-IO             TO LISTE-IO-AREA (8:2)
               MOVE RESKNR                 TO LISTE-IO-AREA (12:6)
               IF  (NOT-I-75)
                   MOVE NAVN               TO LISTE-IO-AREA (20:30)
               END-IF
               MOVE BDAG                   TO LISTE-IO-AREA (52:2)
               MOVE '.'                    TO LISTE-IO-AREA (54:1)
               MOVE BMND                   TO LISTE-IO-AREA (55:2)
               MOVE '.'                    TO LISTE-IO-AREA (57:1)
               MOVE BA-ELGR                TO LISTE-IO-AREA (58:2)
               MOVE BNR                    TO LISTE-IO-AREA (62:6)
               MOVE REFNR                  TO LISTE-IO-AREA (71:6)
               MOVE TEKST                  TO LISTE-IO-AREA (80:7)
               IF  (I-90 AND I-19)
                   MOVE AST (X)            TO LISTE-IO-AREA (90:3)
               END-IF
               IF  (I-90)
                   MOVE VALUTA             TO XO-82YY9R
                   MOVE XO-82YY9R          TO LISTE-IO-AREA (96:14)
               END-IF
               MOVE BEL                    TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (113:13)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-01 AND I-99)
               MOVE SPACES TO CONSOLE-IO-AREA
               INITIALIZE CONSOLE-IO-AREA
               MOVE '==> CANCEL RSK195  ==>  ' TO CONSOLE-IO-AREA
                                                                (1:24)
               MOVE 'FEIL I DATO (SUBRUT DATO' TO CONSOLE-IO-AREA
                                                               (25:24)
               MOVE '8SIF)       '         TO CONSOLE-IO-AREA (49:12)
               DISPLAY CONSOLE-IO-AREA
               MOVE SPACES TO CONSOLE-IO-AREA
               INITIALIZE CONSOLE-IO-AREA
               MOVE '==> '                 TO CONSOLE-IO-AREA (1:4)
               MOVE 'PDATO : '             TO CONSOLE-IO-AREA (5:8)
               MOVE PDATO-IO               TO CONSOLE-IO-AREA (15:6)
               DISPLAY CONSOLE-IO-AREA
               MOVE SPACES TO CONSOLE-IO-AREA
               INITIALIZE CONSOLE-IO-AREA
               MOVE '==> '                 TO CONSOLE-IO-AREA (1:4)
               MOVE DTOMEL                 TO CONSOLE-IO-AREA (4:57)
               DISPLAY CONSOLE-IO-AREA
           END-IF
           IF  (I-02 AND I-99)
               MOVE SPACES TO CONSOLE-IO-AREA
               INITIALIZE CONSOLE-IO-AREA
               MOVE '==> PROGRAM: RSK195 ' TO CONSOLE-IO-AREA (1:20)
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
               DISPLAY CONSOLE-IO-AREA
           END-IF.
 
       EXCEPTION-OUTPUT SECTION.
       EXCEPTION-OUTPUT-P.
           IF  (I-94 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'T O T A L T  B E L Ø P' TO LISTE-IO-AREA (69:22)
               MOVE FDAG-IO                TO LISTE-IO-AREA (97:2)
               MOVE '.'                    TO LISTE-IO-AREA (99:1)
               MOVE FMND-IO                TO LISTE-IO-AREA (100:2)
               MOVE '.'                    TO LISTE-IO-AREA (102:1)
               MOVE FA-ELGR-IO             TO LISTE-IO-AREA (103:2)
               MOVE '*'                    TO LISTE-IO-AREA (126:1)
               MOVE DAGSUM                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (112:14)
               INITIALIZE DAGSUM
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-92 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '* * * * *  K R E D I T O' TO LISTE-IO-AREA (2:24)
               MOVE 'R R E S K O N T R O  * *' TO LISTE-IO-AREA (27:24)
               MOVE '* * *  P R.'          TO LISTE-IO-AREA (52:11)
               MOVE PDATO                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (64:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-93 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'S A L D O  P R.  V A L U' TO LISTE-IO-AREA (2:24)
               MOVE 'T A'                  TO LISTE-IO-AREA (27:3)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TYPE VALUTA'          TO LISTE-IO-AREA (2:11)
               MOVE 'VALUTABELØP'          TO LISTE-IO-AREA (38:11)
               MOVE 'BELØP'                TO LISTE-IO-AREA (65:5)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-50 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE AST (X)                TO LISTE-IO-AREA (2:3)
               MOVE ATX (X)                TO LISTE-IO-AREA (7:20)
               MOVE ASM (X)                TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (36:14)
               INITIALIZE ASM (X)
               MOVE ABL (X)                TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (57:14)
               INITIALIZE ABL (X)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-53 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'SUM UTLAND'           TO LISTE-IO-AREA (2:10)
               MOVE SUMABL                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (57:14)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-54 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'SUM INNLAND'          TO LISTE-IO-AREA (2:11)
               MOVE SUMINL                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (57:14)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'SUM TOTALT '          TO LISTE-IO-AREA (2:11)
               MOVE SUMTOT                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (57:14)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L3 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE '*  F O R F A L L S L I S' TO LISTE-IO-AREA (33:24)
               MOVE 'T E'                  TO LISTE-IO-AREA (58:3)
               IF  (I-90)
                   MOVE 'U T E N L A N D S K E' TO LISTE-IO-AREA
                                                               (63:21)
               END-IF
               IF  (I-91)
                   MOVE 'I N N E N L A N D S K E' TO LISTE-IO-AREA
                                                               (63:23)
               END-IF
               MOVE 'K R E D I T O R E R'  TO LISTE-IO-AREA (88:19)
               MOVE '*'                    TO LISTE-IO-AREA (108:1)
               MOVE 'P R.'                 TO LISTE-IO-AREA (110:4)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (115:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (124:4)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (129:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE '************************' TO LISTE-IO-AREA (49:24)
               MOVE '************************' TO LISTE-IO-AREA (73:24)
               MOVE '************************' TO LISTE-IO-AREA (97:24)
               MOVE '************'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FORFALLS   RESK'      TO LISTE-IO-AREA (2:15)
               MOVE 'N A V N'              TO LISTE-IO-AREA (32:7)
               MOVE 'BILAGS   BILAGS    REF' TO LISTE-IO-AREA (53:22)
               MOVE 'TEKST   VALUTA'       TO LISTE-IO-AREA (81:14)
               MOVE 'BELØP          BELØP' TO LISTE-IO-AREA (102:20)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'DATO      NR'         TO LISTE-IO-AREA (4:12)
               MOVE 'DATO      NR      NR' TO LISTE-IO-AREA (55:20)
               MOVE 'TYPE       VALUTA'    TO LISTE-IO-AREA (90:17)
               MOVE 'KRONER'               TO LISTE-IO-AREA (116:6)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE '************************' TO LISTE-IO-AREA (49:24)
               MOVE '************************' TO LISTE-IO-AREA (73:24)
               MOVE '************************' TO LISTE-IO-AREA (97:24)
               MOVE '************'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE '*  F O R F A L L S L I S' TO LISTE-IO-AREA (33:24)
               MOVE 'T E'                  TO LISTE-IO-AREA (58:3)
               IF  (I-90)
                   MOVE 'U T E N L A N D S K E' TO LISTE-IO-AREA
                                                               (63:21)
               END-IF
               IF  (I-91)
                   MOVE 'I N N E N L A N D S K E' TO LISTE-IO-AREA
                                                               (63:23)
               END-IF
               MOVE 'K R E D I T O R E R'  TO LISTE-IO-AREA (88:19)
               MOVE '*'                    TO LISTE-IO-AREA (108:1)
               MOVE 'P R.'                 TO LISTE-IO-AREA (110:4)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (115:8)
               MOVE 'SIDE'                 TO LISTE-IO-AREA (124:4)
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (129:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE '************************' TO LISTE-IO-AREA (49:24)
               MOVE '************************' TO LISTE-IO-AREA (73:24)
               MOVE '************************' TO LISTE-IO-AREA (97:24)
               MOVE '************'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FORFALLS   RESK'      TO LISTE-IO-AREA (2:15)
               MOVE 'N A V N'              TO LISTE-IO-AREA (32:7)
               MOVE 'BILAGS   BILAGS    REF' TO LISTE-IO-AREA (53:22)
               MOVE 'TEKST   VALUTA'       TO LISTE-IO-AREA (81:14)
               MOVE 'BELØP          BELØP' TO LISTE-IO-AREA (102:20)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'DATO      NR'         TO LISTE-IO-AREA (4:12)
               MOVE 'DATO      NR      NR' TO LISTE-IO-AREA (55:20)
               MOVE 'TYPE       VALUTA'    TO LISTE-IO-AREA (90:17)
               MOVE 'KRONER'               TO LISTE-IO-AREA (116:6)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE '************************' TO LISTE-IO-AREA (49:24)
               MOVE '************************' TO LISTE-IO-AREA (73:24)
               MOVE '************************' TO LISTE-IO-AREA (97:24)
               MOVE '************'         TO LISTE-IO-AREA (121:12)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ' '                    TO LISTE-IO-AREA (1:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L4 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '* * * * *  K R E D I T O' TO LISTE-IO-AREA (2:24)
               MOVE 'R R E S K O N T R O  * *' TO LISTE-IO-AREA (27:24)
               MOVE '* * *  P R.'          TO LISTE-IO-AREA (52:11)
               MOVE PDATO                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (64:8)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'F O R F A L T E  B E L Ø' TO LISTE-IO-AREA (4:24)
               MOVE 'P'                    TO LISTE-IO-AREA (29:1)
               MOVE SUMFF                  TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (44:14)
               INITIALIZE SUMFF
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'I K K E  F O R F A L T E' TO LISTE-IO-AREA (4:24)
               MOVE 'B E L Ø P'            TO LISTE-IO-AREA (30:9)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '0 - 30  D A G E R'    TO LISTE-IO-AREA (6:17)
               MOVE SUM00                  TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (25:14)
               INITIALIZE SUM00
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '31 - 60  D A G E R'   TO LISTE-IO-AREA (5:18)
               MOVE SUM30                  TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (25:14)
               INITIALIZE SUM30
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '61 - 90  D A G E R'   TO LISTE-IO-AREA (5:18)
               MOVE SUM60                  TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (25:14)
               INITIALIZE SUM60
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'OVER  90  D A G E R'  TO LISTE-IO-AREA (4:19)
               MOVE SUM90                  TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (25:14)
               INITIALIZE SUM90
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'T O T A L  S A L D O' TO LISTE-IO-AREA (4:20)
               MOVE TOTSUM                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (25:14)
               INITIALIZE TOTSUM
      *****************************************************************
      * DUMMY-LINJE FOR Å LAGE REF. TIL DATAFELT (FJERNE FEILMELDING) *
      *****************************************************************
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-U1 AND I-U2 AND I-U3)
           AND (I-LR AND I-U8 AND I-U7)
           AND (I-03 AND I-97 AND I-98)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE PSDS                   TO LISTE-IO-AREA (1:80)
               MOVE R                      TO LISTE-IO-AREA (73:8)
               MOVE P-IO                   TO LISTE-IO-AREA (78:3)
               MOVE S-IO                   TO LISTE-IO-AREA (76:5)
               MOVE DTODTO                 TO LISTE-IO-AREA (75:6)
               MOVE DTOMEL                 TO LISTE-IO-AREA (24:57)
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
               MOVE FINAVN                 TO LISTE-IO-AREA (91:30)
               MOVE AKSEKD                 TO LISTE-IO-AREA (120:1)
               MOVE NORVN                  TO LISTE-IO-AREA (101:20)
               MOVE LANDKD                 TO LISTE-IO-AREA (119:2)
               MOVE LAND                   TO LISTE-IO-AREA (101:20)
               MOVE VALIX-IO               TO LISTE-IO-AREA (118:3)
               MOVE DIV                    TO LISTE-IO-AREA (111:10)
               MOVE TKTK                   TO LISTE-IO-AREA (119:2)
               MOVE TKREFN                 TO LISTE-IO-AREA (115:6)
               MOVE TKBILN                 TO LISTE-IO-AREA (115:6)
      *****************************************************************
      * FEILMELDINGER PÅ KONSOLLET:                                   *
      *****************************************************************
               MOVE 1                      TO LISTE-AFTER-SPACE
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
           INITIALIZE RESPAR-DATA-FIELDS
           SET RESPAR-EOF-OFF              TO TRUE
           SET RESPAR-PROCESS              TO TRUE
           OPEN INPUT RESPAR
           SET RESFILE-LEVEL-INIT          TO TRUE
           INITIALIZE RESFILE-DATA-FIELDS
           SET RESFILE-EOF-OFF             TO TRUE
           SET RESFILE-PROCESS             TO TRUE
           OPEN INPUT RESFILE
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           PERFORM VARYING AAD-I FROM 1 BY 1
                     UNTIL AAD-I > AAD-MAX
               INITIALIZE AAD (AAD-I)
           END-PERFORM
           SET AAD-I                       TO 1
           PERFORM VARYING ASM-I FROM 1 BY 1
                     UNTIL ASM-I > ASM-MAX
               INITIALIZE ASM (ASM-I)
           END-PERFORM
           SET ASM-I                       TO 1
           PERFORM VARYING AST-I FROM 1 BY 1
                     UNTIL AST-I > AST-MAX
               INITIALIZE AST (AST-I)
           END-PERFORM
           SET AST-I                       TO 1
           PERFORM VARYING ATX-I FROM 1 BY 1
                     UNTIL ATX-I > ATX-MAX
               INITIALIZE ATX (ATX-I)
           END-PERFORM
           SET ATX-I                       TO 1
           PERFORM VARYING ABL-I FROM 1 BY 1
                     UNTIL ABL-I > ABL-MAX
               INITIALIZE ABL (ABL-I)
           END-PERFORM
           SET ABL-I                       TO 1
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE RESPAR
           CLOSE RESFILE
           CLOSE KUNDEMA
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
