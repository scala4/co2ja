       IDENTIFICATION DIVISION.
       PROGRAM-ID. RKO275R.
      **********************************************  Z-WIN-RPG2   ****
      **OBS ved endring i excel på Report Web *****************
      * PROGRAM   : RKO275 - ÅPNE POSTER PR SISTE (RES40/UPSI 1 PÅ)   *
      *                      ÅPNE POSTER PR 31.12 (RES42/UPSI 1 AV)   *
      * LAGET AV  : MORTEN TUVRØNNINGEN AUG 2010                      *
      * E XX.XX.XX:                                                   *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RKO275.rpg
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
           SELECT RESKFIL
               ASSIGN TO UT-S-RESKFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RESKFIL-STATUS.
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
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD REGPAR
               BLOCK CONTAINS 100
               RECORD CONTAINS 100.
       01  REGPAR-IO-AREA.
           05  REGPAR-IO-AREA-X            PICTURE X(100).
       FD RESKFIL
               BLOCK CONTAINS 200
               RECORD CONTAINS 200.
       01  RESKFIL-IO-AREA.
           05  RESKFIL-IO-AREA-X           PICTURE X(200).
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
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
      *BUGFILO O   F  80  80            PRINTERSYSLST
      * **START RBS - DATASTRUKTUR FOR SUB-PROGRAM RBS000 ********
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  REGPAR-STATUS               PICTURE 99 VALUE 0.
           10  RESKFIL-STATUS              PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  KUNDEMX-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  TKDATA-XX-STATUS            PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
           10  DTOPAR-XX-STATUS            PICTURE 99 VALUE 0.
           10  VALPAR-XX-STATUS            PICTURE 99 VALUE 0.
 
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
               88  RESKFIL-EOF-OFF         VALUE '0'.
               88  RESKFIL-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKFIL-READ-OFF        VALUE '0'.
               88  RESKFIL-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKFIL-PROCESS-OFF     VALUE '0'.
               88  RESKFIL-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  RESKFIL-LEVEL-INIT-OFF  VALUE '0'.
               88  RESKFIL-LEVEL-INIT      VALUE '1'.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  KUNDEMX-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
      * * START RBS - PARAMETERKORT OVERSTYRING AV RBS-FILE  * *
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
      * * END   RBS - PARAMETERKORT OVERSTYRING AV RBS-FILE  * *
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
      * * START RBS - PARAMETERKORT OVERSTYRING AV RBS-FILE  * *
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
      * * END   RBS - PARAMETERKORT OVERSTYRING AV RBS-FILE  * *
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
      * * END   RBS - PARAMETERKORT OVERSTYRING AV RBS-FILE  * *
      * * START - DATASTRUKTUR FOR SUB-PROGRAM ADVALUTA ********
           05  VALPAR-XX REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  AKSEKD                  PICTURE X(1).
               10  FILLER                  PICTURE X(79).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(1).
               10  ADVK                    PICTURE X(1).
               10  FILLER                  PICTURE X(78).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(2).
               10  STDVK                   PICTURE X(3).
               10  FILLER                  PICTURE X(75).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(5).
               10  NORVN                   PICTURE X(20).
               10  FILLER                  PICTURE X(55).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(25).
               10  STDVN                   PICTURE X(20).
               10  FILLER                  PICTURE X(35).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(45).
               10  LANDKD                  PICTURE X(2).
               10  FILLER                  PICTURE X(33).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(47).
               10  LAND                    PICTURE X(20).
               10  FILLER                  PICTURE X(13).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(67).
               10  VALIX-IO.
                   15  VALIX               PICTURE S9(3).
               10  FILLER                  PICTURE X(10).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(70).
               10  DIV                     PICTURE X(10).
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
           05  REGPAR-DATA-FIELDS.
               10  PA-ELGR                 PICTURE X(2).
               10  PMND                    PICTURE X(2).
               10  PMND1                   PICTURE X(1).
               10  PMND2                   PICTURE X(1).
               10  PDAG                    PICTURE X(2).
               10  PDAG1                   PICTURE X(1).
               10  PDAG2                   PICTURE X(1).
           05  RESKFIL-LEVEL-02.
               10  RESKFIL-02-L5.
                   15  RESKFIL-02-L5-FIRMA PICTURE X(3).
               10  RESKFIL-02-L4.
                   15  RESKFIL-02-L4-RESK1 PICTURE X(1).
               10  RESKFIL-02-L3.
                   15  RESKFIL-02-L3-RESKNR PICTURE X(9).
               10  RESKFIL-02-L2.
                   15  RESKFIL-02-L2-BILART PICTURE X(1).
               10  RESKFIL-02-L1.
                   15  RESKFIL-02-L1-BILNR PICTURE X(6).
           05  RESKFIL-DATA-FIELDS.
               10  REC60                   PICTURE X(60).
               10  FIRMA                   PICTURE X(3).
               10  RESK1                   PICTURE X(1).
               10  RESK2                   PICTURE X(6).
               10  RESKNR                  PICTURE X(9).
               10  BILA-ELGR               PICTURE X(2).
               10  BILMND                  PICTURE X(2).
               10  BILDAG                  PICTURE X(2).
               10  BILNR                   PICTURE X(6).
               10  REFNR                   PICTURE X(6).
               10  FDATO-IO.
                   15  FDATO               PICTURE S9(6).
               10  FFA-ELGR-IO.
                   15  FFA-ELGR            PICTURE S9(2).
               10  FFMND-IO.
                   15  FFMND               PICTURE S9(2).
               10  FFDAG-IO.
                   15  FFDAG               PICTURE S9(2).
               10  BELO-ELGP-IO.
                   15  BELO-ELGP           PICTURE S9(7)V9(2).
               10  TRKODE                  PICTURE X(2).
               10  VALUTA-IO.
                   15  VALUTA              PICTURE S9(8)V9(2).
               10  BILART                  PICTURE X(1).
               10  VT                      PICTURE X(1).
               10  TEKSTR                  PICTURE X(24).
           05  KUNDEMA-DATA-FIELDS.
               10  NAVN1                   PICTURE X(30).
               10  NAVN2                   PICTURE X(30).
               10  ADR                     PICTURE X(30).
               10  PSTED                   PICTURE X(15).
               10  PNR                     PICTURE X(4).
               10  HDIST1                  PICTURE X(1).
               10  BM                      PICTURE X(2).
               10  KRGR-IO.
                   15  KRGR                PICTURE S9(4).
               10  KANM                    PICTURE X(1).
           05  KUNDEMX-DATA-FIELDS.
               10  KORGNR                  PICTURE X(9).
           05  FIRMAF-DATA-FIELDS.
               10  FNR                     PICTURE X(3).
               10  RPROS-IO.
                   15  RPROS               PICTURE S9(1)V9(2).
               10  ORGNR-IO.
                   15  ORGNR               PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L5            PICTURE X(3).
               10  THE-PRIOR-L4            PICTURE X(1).
               10  THE-PRIOR-L3            PICTURE X(9).
               10  THE-PRIOR-L2            PICTURE X(1).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  FIRBEL-IO.
                   15  FIRBEL              PICTURE S9(9)V9(2).
               10  ORGNR9-IO.
                   15  ORGNR9              PICTURE S9(11).
               10  UMONTH-N-IO.
                   15  UMONTH-N            PICTURE S9(2).
               10  UYEAR-N-IO.
                   15  UYEAR-N             PICTURE S9(2).
               10  INBDAG-IO.
                   15  INBDAG              PICTURE S9(2).
               10  INBMND-IO.
                   15  INBMND              PICTURE S9(2).
               10  INBAAR-IO.
                   15  INBAAR              PICTURE S9(2).
               10  HJELP1-IO.
                   15  HJELP1              PICTURE S9(4).
               10  INBDAT-IO.
                   15  INBDAT              PICTURE S9(6).
               10  INN1-IO.
                   15  INN1                PICTURE S9(6).
               10  INN2-IO.
                   15  INN2                PICTURE S9(3).
               10  INNDAG-IO.
                   15  INNDAG              PICTURE S9(6).
               10  SUMBEL-IO.
                   15  SUMBEL              PICTURE S9(8)V9(2).
               10  GRPBEL-IO.
                   15  GRPBEL              PICTURE S9(8)V9(2).
               10  ANTKL4-IO.
                   15  ANTKL4              PICTURE S9(5).
               10  RENKL4-IO.
                   15  RENKL4              PICTURE S9(7)V9(2).
               10  RENLL4-IO.
                   15  RENLL4              PICTURE S9(7)V9(2).
               10  KUXKEY                  PICTURE X(10).
               10  TOTVAL-IO.
                   15  TOTVAL              PICTURE S9(10)V9(2).
               10  VALTYP                  PICTURE X(3).
               10  ANTKUN-IO.
                   15  ANTKUN              PICTURE S9(5).
               10  TOTBEL-IO.
                   15  TOTBEL              PICTURE S9(9)V9(2).
               10  ANT-IO.
                   15  ANT                 PICTURE S9(5).
               10  DAG1-IO.
                   15  DAG1                PICTURE S9(6).
               10  DAG2-IO.
                   15  DAG2                PICTURE S9(3).
               10  FDAG-IO.
                   15  FDAG                PICTURE S9(6).
               10  RDAG-IO.
                   15  RDAG                PICTURE S9(4).
               10  SUM-X-IO.
                   15  SUM-X               PICTURE S9(8)V9(2).
               10  SUMA-IO.
                   15  SUMA                PICTURE S9(10).
               10  SUMB-IO.
                   15  SUMB                PICTURE S9(12).
               10  RENTEB-IO.
                   15  RENTEB              PICTURE S9(7)V9(2).
               10  RENTL3-IO.
                   15  RENTL3              PICTURE S9(7)V9(2).
               10  RENTL5-IO.
                   15  RENTL5              PICTURE S9(7)V9(2).
               10  RENTLE-IO.
                   15  RENTLE              PICTURE S9(7)V9(2).
               10  TEKST                   PICTURE X(24).
           05  EDITTING-FIELDS.
               10  XO-40YNZ                PICTURE ZZZZ.
               10  XO-72YY9                PICTURE Z.ZZZ.ZZZ,99.
               10  XO-82YY9R               PICTURE ZZ.ZZZ.ZZZ,99-.
               10  XO-72YY9R               PICTURE Z.ZZZ.ZZZ,99-.
               10  XO-82YY9                PICTURE ZZ.ZZZ.ZZZ,99.
               10  XO-102YY9R              PICTURE Z.ZZZ.ZZZ.ZZZ,99-.
               10  XO-50YY9                PICTURE ZZ.ZZ9.
               10  XO-92YY9R               PICTURE ZZZ.ZZZ.ZZZ,99-.
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
           SET NOT-I-05                    TO TRUE
           SET NOT-I-04                    TO TRUE
 
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
                   SET REGPAR-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  RESKFIL-PROCESS
               SET RESKFIL-PROCESS-OFF     TO TRUE
               SET RESKFIL-READ            TO TRUE
           END-IF
 
           IF  RESKFIL-READ
           AND RECORD-SELECTED-OFF
               PERFORM RESKFIL-GET
               SET RESKFIL-READ-OFF        TO TRUE
               IF  NOT RESKFIL-EOF
                   SET RESKFIL-PROCESS     TO TRUE
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
 
           IF  RESKFIL-PROCESS
               PERFORM RESKFIL-IDSET
           END-IF
 
           IF  RESKFIL-PROCESS
               PERFORM RESKFIL-CHK-LEVEL
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
 
           IF  REGPAR-PROCESS
               PERFORM REGPAR-FLDSET
           END-IF
 
           IF  RESKFIL-PROCESS
               PERFORM RESKFIL-FLDOFF
               PERFORM RESKFIL-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  RESKFIL-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-96)
               SET I-H0                    TO TRUE
           END-IF
           IF  (I-L5)
               SUBTRACT FIRBEL             FROM FIRBEL
               SUBTRACT RENTL5             FROM RENTL5
               SUBTRACT RENTLE             FROM RENTLE
               MOVE FIRMA                  TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-11                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-11            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
           END-IF
           IF  (I-L5 AND NOT-I-11)
               ADD ORGNR TO ZERO       GIVING ORGNR9
               PERFORM RBSRUT-S
      *
           END-IF
           IF  (I-01)
               MOVE UMONTH                 TO INBMND-IO
               MOVE UYEAR                  TO INBAAR-IO
               SUBTRACT 1 FROM UDAY    GIVING INBDAG
               SET NOT-I-61                TO TRUE
               IF  INBDAG = 00
                   SET I-61                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-61)
               SUBTRACT 1 FROM UMONTH  GIVING INBMND
               MOVE 30                     TO INBDAG-IO
               SET NOT-I-63                TO TRUE
               IF  INBMND = 02
                   SET I-63                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-63)
               MOVE 28                     TO INBDAG-IO
           END-IF
           IF  (I-01)
               SET NOT-I-62                TO TRUE
               IF  INBMND = 00
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-62)
               SUBTRACT 1 FROM UYEAR   GIVING INBAAR
               MOVE 12                     TO INBMND-IO
           END-IF
           IF  (I-01)
               MOVE INBAAR                 TO HJELP1-IO (3:2)
               MOVE INBMND                 TO HJELP1 (1:2)
               MOVE HJELP1                 TO INBDAT-IO (3:4)
               MOVE INBDAG                 TO INBDAT (1:2)
               MOVE 'A'                    TO DTOKOD
               MOVE INBDAT                 TO DTODTO
               PERFORM DTORUT-S
           END-IF
           IF  (I-01 AND I-99)
               SET I-96                    TO TRUE
               GO TO UT-T
           END-IF
           IF  (I-01)
               MULTIPLY 360 BY AAR4SI  GIVING INN1
               MULTIPLY 30 BY INBMND   GIVING INN2
               ADD INN1 TO ZERO        GIVING INNDAG
               ADD INN2                    TO INNDAG
               ADD UDAY                    TO INNDAG
               GO TO UT-T
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
           END-IF
           SET NOT-I-25                    TO TRUE
           IF  (I-L4)
               SET NOT-I-12                TO TRUE
      *
           END-IF
           IF  (I-L3)
               SUBTRACT SUMBEL             FROM SUMBEL
               SUBTRACT RENTL3             FROM RENTL3
           END-IF
           IF  (I-L4)
               SUBTRACT GRPBEL             FROM GRPBEL
               SUBTRACT ANTKL4             FROM ANTKL4
               SUBTRACT RENKL4             FROM RENKL4
               SUBTRACT RENLL4             FROM RENLL4
      *  L4      RESK1     COMP "2"                    5812
           END-IF
           IF  (I-L4)
               SET NOT-I-58                TO TRUE
               IF  RESK1 < '2'
                   SET I-58                TO TRUE
               END-IF
               SET NOT-I-59                TO TRUE
               IF  RESK1 = '9'
                   SET I-59                TO TRUE
               END-IF
      *
           END-IF
           IF  (I-L3)
               SET NOT-I-79                TO TRUE
           END-IF
           IF  (I-L3 AND NOT-I-12)
               MOVE RESKNR                 TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-09                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-09            TO TRUE
                   PERFORM KUNDEMA-FLDOFF
                   PERFORM KUNDEMA-FLDSET
                   PERFORM KUNDEMA-IDSET
               END-READ
           END-IF
           IF  (I-L3 AND I-59)
               MOVE RESKNR                 TO KUXKEY (1:9)
               MOVE '1'                    TO KUXKEY (10:1)
               MOVE KUXKEY                 TO KUNDEMX-KEY1
               READ KUNDEMX RECORD KEY IS KUNDEMX-KEY1
               INVALID KEY
                   SET I-10                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-10            TO TRUE
                   PERFORM KUNDEMX-FLDOFF
                   PERFORM KUNDEMX-FLDSET
                   PERFORM KUNDEMX-IDSET
               END-READ
           END-IF
           IF  (I-L3 AND NOT-I-12 AND NOT-I-09)
               SET NOT-I-79                TO TRUE
               IF  KANM = 'I'
                   SET I-79                TO TRUE
               END-IF
           END-IF
           SET NOT-I-14                    TO TRUE
           SET NOT-I-13                    TO TRUE
           IF  BILART NOT < '4'
               SET I-14                    TO TRUE
           END-IF
           IF  BILART < '4'
               SET I-13                    TO TRUE
           END-IF
           SET NOT-I-19                    TO TRUE
           IF  RESK1 = '9'
               SET I-19                    TO TRUE
           END-IF
           SET NOT-I-20                    TO TRUE
           IF  HDIST1 = '0'
               SET I-20                    TO TRUE
           END-IF
           IF  (I-19 AND I-20)
               SET I-25                    TO TRUE
      *  L3 59             MOVE "KORGNR  "BUGFL1  8         DISPLAY FIELD
      *  L3 59   BUGFL1    DEBUGBUGFILO   KORGNR            VIS INDIKATOR
      *   RUTINE FOR SUMMERING AV VALUTABELØP  *
           END-IF
           IF  (I-25)
               ADD VALUTA                  TO TOTVAL
               SET NOT-I-75                TO TRUE
               IF  TOTVAL = 0
                   SET I-75                TO TRUE
               END-IF
      *  25                MOVE VALTYP    TYPVAL  1
           END-IF
           MOVE '   '                      TO VALTYP
           IF  (I-25)
               PERFORM VALRUT-S
      ********************************************
           END-IF
           IF  (I-L3)
               ADD 1                       TO ANTKUN
               ADD 1                       TO ANTKL4
           END-IF
           ADD BELO-ELGP                   TO FIRBEL
           ADD BELO-ELGP                   TO TOTBEL
           ADD BELO-ELGP                   TO SUMBEL
           ADD BELO-ELGP                   TO GRPBEL
      *
           ADD 1                           TO ANT
      *
           IF  (I-02 AND NOT-I-36)
               SET NOT-I-36                TO TRUE
               IF  NAVN1 = TEKSTR
                   SET I-36                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-36)
               MOVE TEKSTR                 TO TEKST
           END-IF
           IF  (I-02 AND I-36)
               PERFORM TKRUT-S
      *
      *****************************************************
      *       BEREGNING  AV  RENTE-DAGER          *********
      *****************************************************
           END-IF
           IF  (NOT-I-58 AND NOT-I-59)
               GO TO UT-T
           END-IF
           MOVE 'B'                        TO DTOKOD
           MOVE FDATO                      TO DTODTO
           PERFORM DTORUT-S
           IF  (I-99)
               MULTIPLY 360 BY FFA-ELGR GIVING DAG1
           END-IF
           IF  (NOT-I-99)
               MULTIPLY 360 BY AAR4SI  GIVING DAG1
           END-IF
           MULTIPLY 30 BY FFMND        GIVING DAG2
           ADD DAG1 TO ZERO            GIVING FDAG
           ADD DAG2                        TO FDAG
           ADD FFDAG                       TO FDAG
           SUBTRACT FDAG FROM INNDAG   GIVING RDAG
           SET NOT-I-21                    TO TRUE
           IF  RDAG NOT > 0
               SET I-21                    TO TRUE
           END-IF
           IF  (I-21)
               GO TO UT-T
      *****************************************************
      *       BEREGNING  AV  RENTE-BELØP          *********
      *****************************************************
           END-IF
           MULTIPLY RPROS BY BELO-ELGP GIVING SUM-X ROUNDED
           MULTIPLY 12 BY SUM-X        GIVING SUMA ROUNDED
           MULTIPLY RDAG BY SUMA       GIVING SUMB ROUNDED
           DIVIDE SUMB BY 36000        GIVING RENTEB ROUNDED
           ADD RENTEB                      TO RENTL3
           IF  (I-58)
               ADD RENTEB                  TO RENTL5
               ADD RENTEB                  TO RENKL4
           END-IF
           IF  (I-59)
               ADD RENTEB                  TO RENTLE
               ADD RENTEB                  TO RENLL4
      *****************************************************
      *
           END-IF
           .
 
       UT-T.
           CONTINUE.
 
       TKRUT-S SECTION.
       TKRUT-S-P.
           MOVE TRKODE                     TO TKTK
           MOVE '       '                  TO TKTEXT
           MOVE BILNR                      TO TKBILN
           MOVE REFNR                      TO TKREFN
           CALL 'RESTRAN' USING TKDATA-XX-DATA-FIELDS
           MOVE TKTEXT                     TO TEKST (1:7).
      ******************************************************            000245
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE '  '                       TO BBEST
           IF  (NOT-I-U8)
               MOVE 'RES42'                TO LONR
           END-IF
           IF  (I-U8)
               MOVE 'RES40'                TO LONR
           END-IF
           MOVE FIRMA                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'RKO275  '                 TO LPROG
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
      *****************************************************************
      *  RUTINE FOR Å HENTE VALUTAKODER.                              *
      *****************************************************************
 
       VALRUT-S SECTION.
       VALRUT-S-P.
           SET NOT-I-95                    TO TRUE
           MOVE 'A'                        TO AKSEKD
           MOVE VT                         TO ADVK
           CALL 'ADVALUTA' USING VALPAR-XX-DATA-FIELDS
           MOVE STDVK                      TO VALTYP
           IF  (NOT-I-95)
               MOVE VT                     TO VALTYP (3:1)
           END-IF.
      ******************************************************
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L0 AND I-12)
               GO TO UT2-T
           END-IF
           IF  (I-L3)
               SET NOT-I-35                TO TRUE
               IF  SUMBEL < 0,00
                   SET I-35                TO TRUE
               END-IF
               SET NOT-I-53                TO TRUE
               IF  RENTL3 < 0,00
                   SET I-53                TO TRUE
               END-IF
           END-IF
           IF  (I-L4)
               SET NOT-I-51                TO TRUE
               IF  RENKL4 < 0,00
                   SET I-51                TO TRUE
               END-IF
               SET NOT-I-52                TO TRUE
               IF  RENLL4 < 0,00
                   SET I-52                TO TRUE
               END-IF
           END-IF
           IF  (I-L5)
               SET NOT-I-55                TO TRUE
               IF  RENTL5 < 0,00
                   SET I-55                TO TRUE
               END-IF
               SET NOT-I-56                TO TRUE
               IF  RENTLE < 0,00
                   SET I-56                TO TRUE
               END-IF
           END-IF.
 
       UT2-T.
      ******************************************************
      *    SUBRUTINE FOR CALL AV COBOL SUBRUTINE RESTRAN.  *
      *    DENNE RUTINE HENTER RESKONTRO TEKST.            *
      ******************************************************
           CONTINUE.
 
       REGPAR-GET SECTION.
       REGPAR-GET-P.
           IF  REGPAR-EOF-OFF
               READ REGPAR
               AT END
                   SET REGPAR-EOF          TO TRUE
               END-READ
           END-IF.
 
       REGPAR-FLDSET SECTION.
       REGPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE REGPAR-IO-AREA (3:2)   TO PA-ELGR (1:2)
               MOVE REGPAR-IO-AREA (5:2)   TO PMND (1:2)
               MOVE REGPAR-IO-AREA (5:1)   TO PMND1 (1:1)
               MOVE REGPAR-IO-AREA (6:1)   TO PMND2 (1:1)
               MOVE REGPAR-IO-AREA (7:2)   TO PDAG (1:2)
               MOVE REGPAR-IO-AREA (7:1)   TO PDAG1 (1:1)
               MOVE REGPAR-IO-AREA (8:1)   TO PDAG2 (1:1)
           END-EVALUATE.
 
       REGPAR-IDSET SECTION.
       REGPAR-IDSET-P.
           SET I-01                        TO TRUE.
 
       RESKFIL-GET SECTION.
       RESKFIL-GET-P.
           IF  RESKFIL-EOF-OFF
               READ RESKFIL
               AT END
                   SET RESKFIL-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RESKFIL-FLDOFF SECTION.
       RESKFIL-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-31                TO TRUE
               SET NOT-I-30                TO TRUE
               SET NOT-I-36                TO TRUE
           END-EVALUATE.
 
       RESKFIL-FLDSET SECTION.
       RESKFIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RESKFIL-IO-AREA (1:60) TO REC60 (1:60)
               MOVE RESKFIL-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE RESKFIL-IO-AREA (6:1)  TO RESK1 (1:1)
               MOVE RESKFIL-IO-AREA (6:6)  TO RESK2 (1:6)
               MOVE RESKFIL-IO-AREA (3:9)  TO RESKNR (1:9)
               MOVE RESKFIL-IO-AREA (24:2) TO BILA-ELGR (1:2)
               MOVE RESKFIL-IO-AREA (26:2) TO BILMND (1:2)
               MOVE RESKFIL-IO-AREA (28:2) TO BILDAG (1:2)
               MOVE RESKFIL-IO-AREA (30:6) TO BILNR (1:6)
               MOVE RESKFIL-IO-AREA (36:6) TO REFNR (1:6)
               MOVE RESKFIL-IO-AREA (42:6) TO FDATO-IO
               INSPECT FDATO-IO REPLACING ALL ' ' BY '0'
               MOVE RESKFIL-IO-AREA (42:2) TO FFA-ELGR-IO
               INSPECT FFA-ELGR-IO REPLACING ALL ' ' BY '0'
               MOVE RESKFIL-IO-AREA (44:2) TO FFMND-IO
               INSPECT FFMND-IO REPLACING ALL ' ' BY '0'
               MOVE RESKFIL-IO-AREA (46:2) TO FFDAG-IO
               INSPECT FFDAG-IO REPLACING ALL ' ' BY '0'
               MOVE RESKFIL-IO-AREA (48:9) TO BELO-ELGP-IO
               INSPECT BELO-ELGP-IO REPLACING ALL ' ' BY '0'
               IF  BELO-ELGP > ZERO
                   SET I-31                TO TRUE
               END-IF
               IF  BELO-ELGP < ZERO
                   SET I-30                TO TRUE
               END-IF
               MOVE RESKFIL-IO-AREA (22:2) TO TRKODE (1:2)
               MOVE RESKFIL-IO-AREA (64:10) TO VALUTA-IO
               INSPECT VALUTA-IO REPLACING ALL ' ' BY '0'
               MOVE RESKFIL-IO-AREA (74:1) TO BILART (1:1)
               MOVE RESKFIL-IO-AREA (76:1) TO VT (1:1)
               MOVE RESKFIL-IO-AREA (90:24) TO TEKSTR (1:24)
               IF  TEKSTR = SPACES
                   SET I-36                TO TRUE
               END-IF
           END-EVALUATE.
 
       RESKFIL-IDSET SECTION.
       RESKFIL-IDSET-P.
           SET I-02                        TO TRUE.
 
       RESKFIL-CHK-LEVEL SECTION.
       RESKFIL-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO RESKFIL-LEVEL-02
               MOVE RESKFIL-IO-AREA (3:3)  TO RESKFIL-02-L5-FIRMA
               MOVE RESKFIL-IO-AREA (6:1)  TO RESKFIL-02-L4-RESK1
               MOVE RESKFIL-IO-AREA (3:9)  TO RESKFIL-02-L3-RESKNR
               MOVE RESKFIL-IO-AREA (74:1) TO RESKFIL-02-L2-BILART
               MOVE RESKFIL-IO-AREA (30:6) TO RESKFIL-02-L1-BILNR
               IF  RESKFIL-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  RESKFIL-02-L5 NOT = THE-PRIOR-L5
                       PERFORM SETON-I-L5
                   WHEN  RESKFIL-02-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   WHEN  RESKFIL-02-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  RESKFIL-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  RESKFIL-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  RESKFIL-02-L5         TO THE-PRIOR-L5
               MOVE  RESKFIL-02-L4         TO THE-PRIOR-L4
               MOVE  RESKFIL-02-L3         TO THE-PRIOR-L3
               MOVE  RESKFIL-02-L2         TO THE-PRIOR-L2
               MOVE  RESKFIL-02-L1         TO THE-PRIOR-L1
               SET RESKFIL-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       KUNDEMA-FLDOFF SECTION.
       KUNDEMA-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-08                TO TRUE
           END-EVALUATE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (16:30) TO NAVN1 (1:30)
               MOVE KUNDEMA-IO-AREA (46:30) TO NAVN2 (1:30)
               MOVE KUNDEMA-IO-AREA (76:30) TO ADR (1:30)
               MOVE KUNDEMA-IO-AREA (106:15) TO PSTED (1:15)
               MOVE KUNDEMA-IO-AREA (121:4) TO PNR (1:4)
               MOVE KUNDEMA-IO-AREA (185:1) TO HDIST1 (1:1)
               MOVE KUNDEMA-IO-AREA (127:2) TO BM (1:2)
               MOVE KUNDEMA-IO-AREA (129:4) TO KRGR-IO
               INSPECT KRGR-IO REPLACING ALL ' ' BY '0'
               IF  KRGR = ZERO
                   SET I-08                TO TRUE
               END-IF
               MOVE KUNDEMA-IO-AREA (161:1) TO KANM (1:1)
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-03                        TO TRUE.
 
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
           SET I-05                        TO TRUE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (5:3)   TO FNR (1:3)
               MOVE FIRMAF-IO-AREA (136:3) TO RPROS-IO
               INSPECT RPROS-IO REPLACING ALL ' ' BY '0'
               MOVE FIRMAF-IO-AREA (896:5) TO ORGNR-IO
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-04                        TO TRUE.
 
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
           IF  (I-L3 AND NOT-I-12 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (1:1)
               IF  (NOT-I-09)
                   MOVE NAVN1              TO LISTE-IO-AREA (10:30)
               END-IF
               IF  (I-09)
                   MOVE 'RESKONTRONR IKKE I KUNDE' TO LISTE-IO-AREA
                                                               (10:24)
               END-IF
               IF  (I-09)
                   MOVE 'ARKIV'            TO LISTE-IO-AREA (34:5)
               END-IF
               IF  (I-59)
                   MOVE 'NO '              TO LISTE-IO-AREA (41:3)
               END-IF
               IF  (I-59 AND NOT-I-10)
                   MOVE KORGNR             TO LISTE-IO-AREA (44:9)
               END-IF
               IF  (I-59 AND I-10)
                   MOVE '*MANGLER*'        TO LISTE-IO-AREA (44:9)
               END-IF
               IF  (I-59 AND I-15)
                   MOVE '*MANGLER*'        TO LISTE-IO-AREA (44:9)
               END-IF
               IF  (I-59)
                   MOVE ' MVA'             TO LISTE-IO-AREA (53:4)
               END-IF
               IF  (I-59 AND I-10)
                   MOVE ' (ORG.NR IKKE I KAJ2)' TO LISTE-IO-AREA
                                                               (57:21)
               END-IF
               IF  (I-59 AND I-15)
                   MOVE ' (ORG.NR IKKE I KAJ2)' TO LISTE-IO-AREA
                                                               (57:21)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               IF  I-OF
                   PERFORM HEADING-OVERFLOW
                   SET NOT-I-OF            TO TRUE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '='                    TO LISTE-IO-AREA (1:1)
               IF  (NOT-I-09)
                   MOVE NAVN2              TO LISTE-IO-AREA (10:30)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               IF  I-OF
                   PERFORM HEADING-OVERFLOW
                   SET NOT-I-OF            TO TRUE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '=='                   TO LISTE-IO-AREA (1:2)
               IF  (NOT-I-09)
                   MOVE ADR                TO LISTE-IO-AREA (10:30)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               IF  I-OF
                   PERFORM HEADING-OVERFLOW
                   SET NOT-I-OF            TO TRUE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '**'                   TO LISTE-IO-AREA (1:2)
               IF  (NOT-I-09)
                   MOVE PNR                TO LISTE-IO-AREA (10:4)
               END-IF
               IF  (NOT-I-09)
                   MOVE PSTED              TO LISTE-IO-AREA (15:15)
               END-IF
               IF  (I-79)
                   MOVE '* SENDT TIL INKASSO *' TO LISTE-IO-AREA
                                                               (31:21)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               IF  I-OF
                   PERFORM HEADING-OVERFLOW
                   SET NOT-I-OF            TO TRUE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'BETM'                 TO LISTE-IO-AREA (12:4)
               IF  (NOT-I-09)
                   MOVE BM                 TO LISTE-IO-AREA (17:2)
               END-IF
               IF  (I-08)
                   MOVE '*'                TO LISTE-IO-AREA (21:1)
               END-IF
               MOVE 'RESKNR'               TO LISTE-IO-AREA (31:6)
               MOVE RESK2                  TO LISTE-IO-AREA (38:6)
               MOVE 'DATO'                 TO LISTE-IO-AREA (48:4)
               MOVE PDAG                   TO LISTE-IO-AREA (53:2)
               MOVE '.'                    TO LISTE-IO-AREA (55:1)
               MOVE PMND                   TO LISTE-IO-AREA (56:2)
               MOVE '.'                    TO LISTE-IO-AREA (58:1)
               MOVE PA-ELGR                TO LISTE-IO-AREA (59:2)
               MOVE 'SISTE INNB.DATO'      TO LISTE-IO-AREA (63:15)
               MOVE INBDAT                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (79:8)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               IF  I-OF
                   PERFORM HEADING-OVERFLOW
                   SET NOT-I-OF            TO TRUE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'BILAGS  '             TO LISTE-IO-AREA (1:8)
               MOVE 'B'                    TO LISTE-IO-AREA (10:1)
               MOVE 'BILAGS'               TO LISTE-IO-AREA (12:6)
               MOVE 'REFER.'               TO LISTE-IO-AREA (19:6)
               MOVE 'BETALINGS    '        TO LISTE-IO-AREA (51:13)
               MOVE 'FORFALLS'             TO LISTE-IO-AREA (91:8)
               IF  (I-25)
                   MOVE 'VALUTA'           TO LISTE-IO-AREA (100:6)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ' DATO   '             TO LISTE-IO-AREA (1:8)
               MOVE 'A'                    TO LISTE-IO-AREA (10:1)
               MOVE '  NR. '               TO LISTE-IO-AREA (12:6)
               MOVE '  NR. '               TO LISTE-IO-AREA (19:6)
               MOVE 'TEKST                   ' TO LISTE-IO-AREA (26:24)
               MOVE 'SPESIFIKASJON'        TO LISTE-IO-AREA (51:13)
               MOVE 'D E B E T'            TO LISTE-IO-AREA (68:9)
               MOVE 'K R E D I T'          TO LISTE-IO-AREA (79:11)
               MOVE '  DATO  '             TO LISTE-IO-AREA (91:8)
               IF  (I-25)
                   MOVE ' TYPE '           TO LISTE-IO-AREA (100:6)
               END-IF
               IF  (I-25)
                   MOVE '    VALUTABELØP'  TO LISTE-IO-AREA (107:15)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-02 AND I-13 AND NOT-I-12)
           AND (NOT-I-86)
               IF  I-OF
                   PERFORM HEADING-OVERFLOW
                   SET NOT-I-OF            TO TRUE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE BILDAG                 TO LISTE-IO-AREA (1:2)
               MOVE '.'                    TO LISTE-IO-AREA (3:1)
               MOVE BILMND                 TO LISTE-IO-AREA (4:2)
               MOVE '.'                    TO LISTE-IO-AREA (6:1)
               MOVE BILA-ELGR              TO LISTE-IO-AREA (7:2)
               MOVE BILART                 TO LISTE-IO-AREA (10:1)
               MOVE BILNR                  TO LISTE-IO-AREA (12:6)
               MOVE REFNR                  TO LISTE-IO-AREA (19:6)
               MOVE TEKST                  TO LISTE-IO-AREA (26:24)
               INITIALIZE TEKST
               IF  (I-31)
                   MOVE BELO-ELGP          TO XO-72YY9
                   MOVE XO-72YY9           TO LISTE-IO-AREA (65:12)
               END-IF
               IF  (I-30)
                   MOVE BELO-ELGP          TO XO-72YY9
                   MOVE XO-72YY9           TO LISTE-IO-AREA (78:12)
               END-IF
               MOVE FFDAG-IO               TO LISTE-IO-AREA (91:2)
               MOVE '.'                    TO LISTE-IO-AREA (93:1)
               MOVE FFMND-IO               TO LISTE-IO-AREA (94:2)
               MOVE '.'                    TO LISTE-IO-AREA (96:1)
               MOVE FFA-ELGR-IO            TO LISTE-IO-AREA (97:2)
               MOVE VALTYP                 TO LISTE-IO-AREA (101:3)
               IF  (I-25)
                   MOVE VALUTA             TO XO-82YY9R
                   MOVE XO-82YY9R          TO LISTE-IO-AREA (108:14)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-02 AND I-14 AND NOT-I-12)
           AND (NOT-I-86)
               IF  I-OF
                   PERFORM HEADING-OVERFLOW
                   SET NOT-I-OF            TO TRUE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE BILDAG                 TO LISTE-IO-AREA (1:2)
               MOVE '.'                    TO LISTE-IO-AREA (3:1)
               MOVE BILMND                 TO LISTE-IO-AREA (4:2)
               MOVE '.'                    TO LISTE-IO-AREA (6:1)
               MOVE BILA-ELGR              TO LISTE-IO-AREA (7:2)
               MOVE BILART                 TO LISTE-IO-AREA (10:1)
               MOVE BILNR                  TO LISTE-IO-AREA (12:6)
               MOVE REFNR                  TO LISTE-IO-AREA (19:6)
               MOVE TEKST                  TO LISTE-IO-AREA (26:24)
               INITIALIZE TEKST
               IF  (NOT-I-25)
                   MOVE BELO-ELGP          TO XO-72YY9R
                   MOVE XO-72YY9R          TO LISTE-IO-AREA (51:13)
               END-IF
               IF  (I-31)
                   MOVE BELO-ELGP          TO XO-72YY9
                   MOVE XO-72YY9           TO LISTE-IO-AREA (65:12)
               END-IF
               IF  (I-30)
                   MOVE BELO-ELGP          TO XO-72YY9
                   MOVE XO-72YY9           TO LISTE-IO-AREA (78:12)
               END-IF
               MOVE FFDAG-IO               TO LISTE-IO-AREA (91:2)
               MOVE '.'                    TO LISTE-IO-AREA (93:1)
               MOVE FFMND-IO               TO LISTE-IO-AREA (94:2)
               MOVE '.'                    TO LISTE-IO-AREA (96:1)
               MOVE FFA-ELGR-IO            TO LISTE-IO-AREA (97:2)
               IF  (I-25)
                   MOVE VALTYP             TO LISTE-IO-AREA (101:3)
               END-IF
               IF  (I-25)
                   MOVE VALUTA             TO XO-82YY9R
                   MOVE XO-82YY9R          TO LISTE-IO-AREA (108:14)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-01 AND I-99)
               MOVE SPACES TO CONSOLE-IO-AREA
               INITIALIZE CONSOLE-IO-AREA
               MOVE '==> CANCEL RKO275  ==>  ' TO CONSOLE-IO-AREA
                                                                (1:24)
               MOVE 'FEIL I DATO (SUBRUT DATO' TO CONSOLE-IO-AREA
                                                               (25:24)
               MOVE '8SIF)       '         TO CONSOLE-IO-AREA (49:12)
               DISPLAY CONSOLE-IO-AREA
               MOVE SPACES TO CONSOLE-IO-AREA
               INITIALIZE CONSOLE-IO-AREA
               MOVE '==> '                 TO CONSOLE-IO-AREA (1:4)
               MOVE 'INBDAT: '             TO CONSOLE-IO-AREA (5:8)
               MOVE INBDAT-IO              TO CONSOLE-IO-AREA (15:6)
               DISPLAY CONSOLE-IO-AREA
           END-IF
           IF  (I-02 AND I-99)
               MOVE SPACES TO CONSOLE-IO-AREA
               INITIALIZE CONSOLE-IO-AREA
               MOVE '==> PROGRAM: RKO275 ' TO CONSOLE-IO-AREA (1:20)
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
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L4 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               IF  (NOT-I-U8)
                   MOVE 'Å P N E    P O S T E R  ' TO LISTE-IO-AREA
                                                               (33:24)
               END-IF
               IF  (NOT-I-U8)
                   MOVE '  P R'            TO LISTE-IO-AREA (57:5)
               END-IF
               IF  (NOT-I-U8)
                   MOVE PDAG1              TO LISTE-IO-AREA (66:1)
               END-IF
               IF  (NOT-I-U8)
                   MOVE PDAG2              TO LISTE-IO-AREA (68:1)
               END-IF
               IF  (NOT-I-U8)
                   MOVE '.'                TO LISTE-IO-AREA (70:1)
               END-IF
               IF  (NOT-I-U8)
                   MOVE PMND1              TO LISTE-IO-AREA (72:1)
               END-IF
               IF  (NOT-I-U8)
                   MOVE PMND2              TO LISTE-IO-AREA (74:1)
               END-IF
               IF  (I-U8)
                   MOVE 'S A L D O S P E S I F I' TO LISTE-IO-AREA
                                                               (34:23)
               END-IF
               IF  (I-U8)
                   MOVE 'K A S J O N    P R    ' TO LISTE-IO-AREA
                                                               (58:22)
               END-IF
               IF  (I-U8)
                   MOVE PDAG1              TO LISTE-IO-AREA (80:1)
               END-IF
               IF  (I-U8)
                   MOVE PDAG2              TO LISTE-IO-AREA (82:1)
               END-IF
               IF  (I-U8)
                   MOVE '.'                TO LISTE-IO-AREA (84:1)
               END-IF
               IF  (I-U8)
                   MOVE PMND1              TO LISTE-IO-AREA (86:1)
               END-IF
               IF  (I-U8)
                   MOVE PMND2              TO LISTE-IO-AREA (88:1)
               END-IF
               MOVE 'SIDE'                 TO LISTE-IO-AREA (113:4)
               IF  (I-L5)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (117:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'NO'                   TO LISTE-IO-AREA (1:2)
               MOVE ORGNR9-IO              TO LISTE-IO-AREA (4:11)
               MOVE 'MVA'                  TO LISTE-IO-AREA (16:3)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               IF  (NOT-I-U8)
                   MOVE 'Å P N E    P O S T E R  ' TO LISTE-IO-AREA
                                                               (33:24)
               END-IF
               IF  (NOT-I-U8)
                   MOVE '  P R'            TO LISTE-IO-AREA (57:5)
               END-IF
               IF  (NOT-I-U8)
                   MOVE PDAG1              TO LISTE-IO-AREA (66:1)
               END-IF
               IF  (NOT-I-U8)
                   MOVE PDAG2              TO LISTE-IO-AREA (68:1)
               END-IF
               IF  (NOT-I-U8)
                   MOVE '.'                TO LISTE-IO-AREA (70:1)
               END-IF
               IF  (NOT-I-U8)
                   MOVE PMND1              TO LISTE-IO-AREA (72:1)
               END-IF
               IF  (NOT-I-U8)
                   MOVE PMND2              TO LISTE-IO-AREA (74:1)
               END-IF
               IF  (I-U8)
                   MOVE 'S A L D O S P E S I F I' TO LISTE-IO-AREA
                                                               (34:23)
               END-IF
               IF  (I-U8)
                   MOVE 'K A S J O N    P R    ' TO LISTE-IO-AREA
                                                               (58:22)
               END-IF
               IF  (I-U8)
                   MOVE PDAG1              TO LISTE-IO-AREA (80:1)
               END-IF
               IF  (I-U8)
                   MOVE PDAG2              TO LISTE-IO-AREA (82:1)
               END-IF
               IF  (I-U8)
                   MOVE '.'                TO LISTE-IO-AREA (84:1)
               END-IF
               IF  (I-U8)
                   MOVE PMND1              TO LISTE-IO-AREA (86:1)
               END-IF
               IF  (I-U8)
                   MOVE PMND2              TO LISTE-IO-AREA (88:1)
               END-IF
               MOVE 'SIDE'                 TO LISTE-IO-AREA (113:4)
               IF  (I-L5)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (117:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'NO'                   TO LISTE-IO-AREA (1:2)
               MOVE ORGNR9-IO              TO LISTE-IO-AREA (4:11)
               MOVE 'MVA'                  TO LISTE-IO-AREA (16:3)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L3 AND NOT-I-12 AND NOT-I-86)
               IF  I-OF
                   PERFORM HEADING-OVERFLOW
                   SET NOT-I-OF            TO TRUE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'RESKONTRO-SALDO    ***' TO LISTE-IO-AREA (3:22)
               IF  (NOT-I-35)
                   MOVE SUMBEL             TO XO-82YY9
                   MOVE XO-82YY9           TO LISTE-IO-AREA (64:13)
               END-IF
               IF  (I-35)
                   MOVE SUMBEL             TO XO-82YY9
                   MOVE XO-82YY9           TO LISTE-IO-AREA (77:13)
               END-IF
               IF  (NOT-I-75)
                   MOVE VALTYP             TO LISTE-IO-AREA (101:3)
                   INITIALIZE VALTYP
               END-IF
               IF  (NOT-I-75)
                   MOVE TOTVAL             TO XO-102YY9R
                   MOVE XO-102YY9R         TO LISTE-IO-AREA (105:17)
                   INITIALIZE TOTVAL
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L3 AND NOT-I-12 AND I-58)
           AND (NOT-I-86)
           OR  (I-L3 AND NOT-I-12 AND I-59)
           AND (NOT-I-86)
               IF  I-OF
                   PERFORM HEADING-OVERFLOW
                   SET NOT-I-OF            TO TRUE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'BEREGNET MORARENTE PR.' TO LISTE-IO-AREA (3:22)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (26:8)
               IF  (NOT-I-53)
                   MOVE RENTL3             TO XO-72YY9
                   MOVE XO-72YY9           TO LISTE-IO-AREA (65:12)
               END-IF
               IF  (I-53)
                   MOVE RENTL3             TO XO-72YY9
                   MOVE XO-72YY9           TO LISTE-IO-AREA (78:12)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L3 AND NOT-I-12 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE '************************' TO LISTE-IO-AREA (49:24)
               MOVE '************************' TO LISTE-IO-AREA (73:24)
               MOVE '************************' TO LISTE-IO-AREA (97:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L4 AND NOT-I-86)
               IF  I-OF
                   PERFORM HEADING-OVERFLOW
                   SET NOT-I-OF            TO TRUE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'SALDO GRUPPE'         TO LISTE-IO-AREA (1:12)
               MOVE RESK1                  TO LISTE-IO-AREA (15:1)
               MOVE GRPBEL                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (19:14)
               MOVE 'ANTALL KUNDER/'       TO LISTE-IO-AREA (35:14)
               MOVE 'LEVRANDØRER MED SALDO' TO LISTE-IO-AREA (49:21)
               MOVE ANTKL4                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (71:6)
               INITIALIZE ANTKL4
               MOVE 2                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               IF  I-OF
                   PERFORM HEADING-OVERFLOW
                   SET NOT-I-OF            TO TRUE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'BEREGNET MORARENTE PR.' TO LISTE-IO-AREA (3:22)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (26:8)
               MOVE 'KUNDER'               TO LISTE-IO-AREA (35:6)
               IF  (NOT-I-51)
                   MOVE RENKL4             TO XO-72YY9
                   MOVE XO-72YY9           TO LISTE-IO-AREA (65:12)
               END-IF
               IF  (I-51)
                   MOVE RENKL4             TO XO-72YY9
                   MOVE XO-72YY9           TO LISTE-IO-AREA (78:12)
               END-IF
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               IF  I-OF
                   PERFORM HEADING-OVERFLOW
                   SET NOT-I-OF            TO TRUE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'BEREGNET MORARENTE PR.' TO LISTE-IO-AREA (3:22)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (26:8)
               MOVE 'LEVRANDØR'            TO LISTE-IO-AREA (35:9)
               IF  (NOT-I-52)
                   MOVE RENLL4             TO XO-72YY9
                   MOVE XO-72YY9           TO LISTE-IO-AREA (65:12)
               END-IF
               IF  (I-52)
                   MOVE RENLL4             TO XO-72YY9
                   MOVE XO-72YY9           TO LISTE-IO-AREA (78:12)
               END-IF
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L5 AND NOT-I-86)
               IF  I-OF
                   PERFORM HEADING-OVERFLOW
                   SET NOT-I-OF            TO TRUE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTAL SALDO'          TO LISTE-IO-AREA (1:11)
               IF  (NOT-I-11)
                   MOVE FNR                TO LISTE-IO-AREA (13:3)
               END-IF
               MOVE FIRBEL                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (18:15)
               MOVE 'ANTALL KUNDER/'       TO LISTE-IO-AREA (35:14)
               MOVE 'LEVRANDØRER MED SALDO' TO LISTE-IO-AREA (49:21)
               MOVE ANTKUN                 TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (71:6)
               INITIALIZE ANTKUN
               MOVE 3                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               IF  I-OF
                   PERFORM HEADING-OVERFLOW
                   SET NOT-I-OF            TO TRUE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'BEREGNET MORARENTE PR.' TO LISTE-IO-AREA (3:22)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (26:8)
               MOVE 'KUNDER'               TO LISTE-IO-AREA (35:6)
               IF  (NOT-I-55)
                   MOVE RENTL5             TO XO-72YY9
                   MOVE XO-72YY9           TO LISTE-IO-AREA (65:12)
               END-IF
               IF  (I-55)
                   MOVE RENTL5             TO XO-72YY9
                   MOVE XO-72YY9           TO LISTE-IO-AREA (78:12)
               END-IF
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               IF  I-OF
                   PERFORM HEADING-OVERFLOW
                   SET NOT-I-OF            TO TRUE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'BEREGNET MORARENTE PR.' TO LISTE-IO-AREA (3:22)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (26:8)
               MOVE 'LEVRANDØR'            TO LISTE-IO-AREA (35:9)
               IF  (NOT-I-56)
                   MOVE RENTLE             TO XO-72YY9
                   MOVE XO-72YY9           TO LISTE-IO-AREA (65:12)
               END-IF
               IF  (I-56)
                   MOVE RENTLE             TO XO-72YY9
                   MOVE XO-72YY9           TO LISTE-IO-AREA (78:12)
               END-IF
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'GRANTOTAL  SALDO'     TO LISTE-IO-AREA (3:16)
               MOVE TOTBEL                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (21:15)
               MOVE 'ANTALL  RESK. RECORDS' TO LISTE-IO-AREA (39:21)
               MOVE ANT                    TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (62:6)
      *****************************************************************
      * DUMMY-LINJE FOR Å LAGE REF. TIL DATAFELT (FJERNE FEILMELDING) *
      *****************************************************************
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-U1 AND I-U2 AND I-U3)
           AND (I-LR AND I-U8 AND I-U7)
           AND (I-03 AND I-04 AND I-97)
           AND (I-98 AND I-05)
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
               MOVE TKTK                   TO LISTE-IO-AREA (119:2)
               MOVE TKTEXT                 TO LISTE-IO-AREA (114:7)
               MOVE TKBILN                 TO LISTE-IO-AREA (115:6)
               MOVE TKREFN                 TO LISTE-IO-AREA (115:6)
               MOVE BPCLAS                 TO LISTE-IO-AREA (120:1)
               MOVE BPRJE                  TO LISTE-IO-AREA (118:3)
               MOVE AKSEKD                 TO LISTE-IO-AREA (120:1)
               MOVE ADVK                   TO LISTE-IO-AREA (120:1)
               MOVE NORVN                  TO LISTE-IO-AREA (101:20)
               MOVE STDVN                  TO LISTE-IO-AREA (101:20)
               MOVE LANDKD                 TO LISTE-IO-AREA (119:2)
               MOVE LAND                   TO LISTE-IO-AREA (101:20)
               MOVE VALIX-IO               TO LISTE-IO-AREA (118:3)
               MOVE DIV                    TO LISTE-IO-AREA (111:10)
               MOVE AKSEKD                 TO LISTE-IO-AREA (120:1)
               MOVE ADVK                   TO LISTE-IO-AREA (120:1)
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
           MOVE 1                          TO LR-CHECK
           INITIALIZE REGPAR-DATA-FIELDS
           SET REGPAR-EOF-OFF              TO TRUE
           SET REGPAR-PROCESS              TO TRUE
           OPEN INPUT REGPAR
           SET RESKFIL-LEVEL-INIT          TO TRUE
           INITIALIZE RESKFIL-DATA-FIELDS
           SET RESKFIL-EOF-OFF             TO TRUE
           SET RESKFIL-PROCESS             TO TRUE
           OPEN INPUT RESKFIL
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           INITIALIZE KUNDEMX-DATA-FIELDS
           OPEN INPUT KUNDEMX
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE REGPAR
           CLOSE RESKFIL
           CLOSE KUNDEMA
           CLOSE KUNDEMX
           CLOSE FIRMAF
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
