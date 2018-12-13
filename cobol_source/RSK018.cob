       IDENTIFICATION DIVISION.
       PROGRAM-ID. RSK018R.
      **********************************************  Z-WIN-RPG2   ****
      *   PROGRAM RSK018                                             *
      *   E 19.10.98 TILPASSET ÅR 2000.                              *
      *   E 29.12.04 PRINTER VALUTA OG VALUTA-TYPE PÅ KVITT. LISTE.  *
      *   E 14.03.05 AVVISER POSTER MED FEIL, SKRIVER EGEN FEILLISTE *
      *   E 11.11.08 TAR MED BILAGSART I POS 77                      *
      *   E 14.12.10 GODTOK BILAGSDATO STØRRE ENN DAGENS DATO        *
      *   E 17.02.11 LEGGER INN STANDARD VALUTATYPE                  *
      *   E 03.03.11 BEREGNET DAGENS DATO FEIL                       *
      *   E 15.04.11 TATT MED UTVIDET BELØP                          *
      *              HAR IKKE ENDRET INPUTBELØPET, FLYTTER BARE      *
      *              GAMLE BELØPSFELT INN I UTVIDEDE I TILLEGG.      *
      *   E 15.04.11 TATT MED UTVIDET BELØP                          *  000013
      *   DANNE POSTER TIL RESKONTRO FOR NYE FIRMAER                 *
      *   INPUTKONTROLLER OG AVSTEMINGSSUMMER                        *
      ****************************************************************  ********
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RSK018.rpg
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
           SELECT KORT
               ASSIGN TO UT-S-KORT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KORT-STATUS.
           SELECT RESFIL
               ASSIGN TO UT-S-RESFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RESFIL-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
           SELECT FLISTE
               ASSIGN TO UT-S-FLISTE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FLISTE-STATUS.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT KUNDEMA
               ASSIGN TO KUNDEMA
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMA-STATUS
               RECORD KEY IS KUNDEMA-KEY1.
           SELECT INPFEIL
               ASSIGN TO UT-S-INPFEIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INPFEIL-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD KORT
               BLOCK CONTAINS 3440
               RECORD CONTAINS 80.
       01  KORT-IO-AREA.
           05  KORT-IO-AREA-X              PICTURE X(80).
       FD RESFIL
               BLOCK CONTAINS 200
               RECORD CONTAINS 200.
       01  RESFIL-IO-AREA.
           05  RESFIL-IO-AREA-X            PICTURE X(200).
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       FD FLISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  FLISTE-IO-PRINT.
           05  FLISTE-IO-AREA-CONTROL      PICTURE X VALUE ' '.
        02 FLISTE-IO-AREA.
           05  FLISTE-IO-AREA-X            PICTURE X(132).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1            PICTURE X(9).
               10  FILLER                  PICTURE X(190).
      *UGFILO O   F  80  80            PRINTERSYSLST
       FD INPFEIL
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  INPFEIL-IO-AREA.
           05  INPFEIL-IO-AREA-X           PICTURE X(80).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  KORT-STATUS                 PICTURE 99 VALUE 0.
           10  RESFIL-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  FLISTE-STATUS               PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  INPFEIL-STATUS              PICTURE 99 VALUE 0.
           10  DTOPAR-XX-STATUS            PICTURE 99 VALUE 0.
           10  VALPAR-XX-STATUS            PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  KORT-EOF-OFF            VALUE '0'.
               88  KORT-EOF                VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KORT-READ-OFF           VALUE '0'.
               88  KORT-READ               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KORT-PROCESS-OFF        VALUE '0'.
               88  KORT-PROCESS            VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  KORT-LEVEL-INIT-OFF     VALUE '0'.
               88  KORT-LEVEL-INIT         VALUE '1'.
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
           05  FLISTE-DATA-FIELDS.
               10  FLISTE-AFTER-SPACE      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  FLISTE-AFTER-SKIP       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  FLISTE-BEFORE-SPACE     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  FLISTE-BEFORE-SKIP      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  FLISTE-MAX-LINES        PICTURE 9(4) BINARY
                                           VALUE 0.
               10  FLISTE-LINE-COUNT       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  FLISTE-CLR-IO           PICTURE X VALUE 'Y'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
      * * START - DATASTRUKTUR FOR SUB-PROGRAM ADVALUTA ********
           05  VALPAR-XX REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  AKSEKD                  PICTURE X(1).
               10  FILLER                  PICTURE X(256).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(1).
               10  ADVK                    PICTURE X(1).
               10  FILLER                  PICTURE X(255).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(2).
               10  STDVK                   PICTURE X(3).
               10  FILLER                  PICTURE X(252).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(5).
               10  NORVN                   PICTURE X(20).
               10  FILLER                  PICTURE X(232).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(25).
               10  STDVN                   PICTURE X(20).
               10  FILLER                  PICTURE X(212).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(45).
               10  LANDKD                  PICTURE X(2).
               10  FILLER                  PICTURE X(210).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(47).
               10  LAND                    PICTURE X(20).
               10  FILLER                  PICTURE X(190).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(67).
               10  VALIX-IO.
                   15  VALIX               PICTURE S9(3).
               10  FILLER                  PICTURE X(187).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(70).
               10  DIV                     PICTURE X(10).
               10  FILLER                  PICTURE X(177).
      * * END - DATASTRUKTUR FOR SUB-PROGRAM ADVALUTA ********
      * **START RBS - DATASTRUKTUR FOR SUB-PROGRAM RBS000 ********
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
      * * END - RBS - DATASTRUKTUR FOR SUB-PROGRAM RBSH01 ********
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
      * **START RBS - DATASTRUKTUR FOR SUB-PROGRAM RBS000 ********
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
           05  KORT-LEVEL-01.
               10  KORT-01-L3.
                   15  KORT-01-L3-FIRMA    PICTURE X(3).
               10  KORT-01-L2.
                   15  KORT-01-L2-KNR1     PICTURE X(1).
               10  KORT-01-L1.
                   15  KORT-01-L1-KUNDE    PICTURE X(6).
           05  KORT-DATA-FIELDS.
               10  REC080                  PICTURE X(80).
               10  FIRMA                   PICTURE X(3).
               10  FKUNDE                  PICTURE X(9).
               10  KNR1                    PICTURE X(1).
               10  KUNDE                   PICTURE X(6).
               10  TK                      PICTURE X(2).
               10  BDATO-IO.
                   15  BDATO               PICTURE S9(6).
               10  BDAG                    PICTURE X(2).
               10  BMND                    PICTURE X(2).
               10  BAAR                    PICTURE X(2).
               10  BILNR                   PICTURE X(6).
               10  REFNR                   PICTURE X(6).
               10  FFDATO-IO.
                   15  FFDATO              PICTURE S9(6).
               10  FFDAG                   PICTURE X(2).
               10  FFMND                   PICTURE X(2).
               10  FFAAR                   PICTURE X(2).
               10  BELO-ELGP-IO.
                   15  BELO-ELGP           PICTURE S9(7)V9(2).
               10  TEGN                    PICTURE X(1).
               10  BETBET                  PICTURE X(2).
               10  VALUTA-IO.
                   15  VALUTA              PICTURE S9(8)V9(2).
               10  VALTYP                  PICTURE X(1).
               10  VT2F                    PICTURE X(2).
               10  STDVT                   PICTURE X(3).
               10  BILART                  PICTURE X(1).
           05  FIRMAF-DATA-FIELDS.
               10  FILLER                  PICTURE X.
           05  KUNDEMA-DATA-FIELDS.
      *                                      16  45 NAVN
               10  KUNBET                  PICTURE X(2).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(1).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  DDATO                   PICTURE X(6).
               10  DDATO8-IO.
                   15  DDATO8              PICTURE S9(8).
               10  TIDSP-IO.
                   15  TIDSP               PICTURE S9(6).
               10  BDATO2                  PICTURE X(6).
               10  NYBEL-IO.
                   15  NYBEL               PICTURE S9(7)V9(2).
               10  BEL132-IO.
                   15  BEL132              PICTURE S9(11)V9(2).
               10  SUM-X-IO.
                   15  SUM-X               PICTURE S9(8)V9(2).
               10  SUML2-IO.
                   15  SUML2               PICTURE S9(8)V9(2).
               10  SUMLR-IO.
                   15  SUMLR               PICTURE S9(11)V9(2).
               10  ANTALL-IO.
                   15  ANTALL              PICTURE S9(5).
               10  ANTL2-IO.
                   15  ANTL2               PICTURE S9(5).
               10  ANTLR-IO.
                   15  ANTLR               PICTURE S9(5).
               10  FSUM-IO.
                   15  FSUM                PICTURE S9(8)V9(2).
               10  FSUML2-IO.
                   15  FSUML2              PICTURE S9(8)V9(2).
               10  FANTLR-IO.
                   15  FANTLR              PICTURE S9(5).
               10  FANTL3-IO.
                   15  FANTL3              PICTURE S9(5).
               10  FANTL2-IO.
                   15  FANTL2              PICTURE S9(5).
               10  NYVAL-IO.
                   15  NYVAL               PICTURE S9(8)V9(2).
               10  VAL154-IO.
                   15  VAL154              PICTURE S9(11)V9(4).
               10  VT3UT                   PICTURE X(3).
               10  VT1UT                   PICTURE X(1).
           05  EDITTING-FIELDS.
               10  XO-72YY9R               PICTURE Z.ZZZ.ZZZ,99-.
               10  XO-82YY9R               PICTURE ZZ.ZZZ.ZZZ,99-.
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
               10  XO-50YNZ                PICTURE ZZZZZ.
               10  XO-112YY9R              PICTURE ZZ.ZZZ.ZZZ.ZZZ,99-.
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
               88  NOT-SET-I-OV            VALUE '0'.
               88  SET-I-OV                VALUE '1'.
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
           IF  NOT-SET-I-OV
               SET NOT-I-OV                TO TRUE
           END-IF
           SET NOT-SET-I-OV                TO TRUE
 
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
           IF  KORT-PROCESS
               SET KORT-PROCESS-OFF        TO TRUE
               SET KORT-READ               TO TRUE
           END-IF
 
           IF  KORT-READ
           AND RECORD-SELECTED-OFF
               PERFORM KORT-GET
               SET KORT-READ-OFF           TO TRUE
               IF  NOT KORT-EOF
                   PERFORM KORT-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET KORT-PROCESS        TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  KORT-PROCESS
               PERFORM KORT-IDSET
           END-IF
 
           IF  KORT-PROCESS
               PERFORM KORT-CHK-LEVEL
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
 
           IF  KORT-PROCESS
               PERFORM KORT-FLDOFF
               PERFORM KORT-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  KORT-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (NOT-I-45)
               PERFORM RBSR20-S
           END-IF
           IF  (NOT-I-45)
               PERFORM RBSR21-S
           END-IF
           IF  (NOT-I-45)
               SET I-45                    TO TRUE
      **  KONTROLL AV GYLDIG FIRMA
      *
           END-IF
           IF  (I-88)
               SET I-89                    TO TRUE
           END-IF
           IF  (I-40)
               SET NOT-I-41                TO TRUE
           END-IF
           IF  (NOT-I-40)
               SET I-40                    TO TRUE
               SET I-41                    TO TRUE
           END-IF
           IF  (I-41)
               MOVE UDATE                  TO DDATO
               MOVE UYEAR                  TO DDATO (1:2)
               MOVE UDAY                   TO DDATO (5:2)
               SET NOT-I-42                TO TRUE
               IF  UYEAR > 80
                   SET I-42                TO TRUE
               END-IF
               MOVE DDATO                  TO DDATO8-IO (3:6)
           END-IF
           IF  (I-41 AND I-42)
               MOVE '19'                   TO DDATO8 (1:2)
           END-IF
           IF  (I-41 AND NOT-I-42)
               MOVE '20'                   TO DDATO8 (1:2)
           END-IF
           IF  (I-41)
               ACCEPT SYSTEM-TIME-X      FROM TIME
               MOVE SYSTEM-TIME            TO TIDSP (1:6)
           END-IF
           SET NOT-I-80                    TO TRUE
           SET NOT-I-81                    TO TRUE
           SET NOT-I-83                    TO TRUE
           SET NOT-I-84                    TO TRUE
           SET NOT-I-87                    TO TRUE
           SET NOT-I-88                    TO TRUE
           IF  (I-L3)
               SET NOT-I-91                TO TRUE
               SET NOT-I-89                TO TRUE
           END-IF
           IF  (I-L2)
               SET NOT-I-92                TO TRUE
           END-IF
           IF  (I-13)
               SET I-88                    TO TRUE
           END-IF
           IF  (I-L3)
               MOVE FIRMA                  TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-20                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-20            TO TRUE
                   PERFORM FIRMAF-IDSET
               END-READ
           END-IF
           IF  (I-20)
               SET I-80                    TO TRUE
               SET I-88                    TO TRUE
      *
      **  KONTROLL AV GYLDIG KUNDENR.
      *
           END-IF
           IF  (I-L1)
               MOVE FKUNDE                 TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-21                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-21            TO TRUE
                   PERFORM KUNDEMA-FLDSET
                   PERFORM KUNDEMA-IDSET
               END-READ
           END-IF
           IF  (I-21)
               SET I-81                    TO TRUE
               SET I-88                    TO TRUE
      *
      **  KONTROLL AV TRANSKODE
      *
      *
      **  KONTROLL AV GYLDIG BILAGSDATO
      *
           END-IF
           IF  (I-01)
               MOVE BDATO                  TO DTODTO
               PERFORM DTORUT-S
           END-IF
           IF  (I-01 AND I-96)
               SET I-83                    TO TRUE
               SET I-88                    TO TRUE
           END-IF
           IF  (I-01 AND NOT-I-88)
               MOVE BDATO                  TO BDATO2
               MOVE BAAR                   TO BDATO2 (1:2)
               MOVE BDAG                   TO BDATO2 (5:2)
               SET NOT-I-83                TO TRUE
               IF  BDATO2 > DDATO
                   SET I-83                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-83)
               SET I-88                    TO TRUE
      *
      **  KONTROLL AV GYLDIG FORFALLSDATO
      *
           END-IF
           IF  (I-01)
               MOVE FFDATO                 TO DTODTO
               PERFORM DTORUT-S
           END-IF
           IF  (I-01 AND I-96)
               SET I-84                    TO TRUE
               SET I-88                    TO TRUE
      *
      **  TEST AV GYLDIG TEGN SAMT SETTE RIKTIG TEGN I BELØP
      **  SUMMERE TRANSENE
      *
           END-IF
           IF  (I-01 AND I-12)
               GO TO HOP-T
           END-IF
           IF  (I-01)
               SET NOT-I-30                TO TRUE
               IF  TEGN = '+'
                   SET I-30                TO TRUE
               END-IF
               SET NOT-I-31                TO TRUE
               IF  TEGN = '-'
                   SET I-31                TO TRUE
               END-IF
               SET NOT-I-32                TO TRUE
               IF  BETBET = '07'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-30 AND NOT-I-31)
               SET I-87                    TO TRUE
               SET I-88                    TO TRUE
           END-IF.
 
       HOP-T.
           IF  (I-01 AND I-30)
               ADD BELO-ELGP TO ZERO   GIVING NYBEL
           END-IF
           IF  (I-01 AND I-31)
               SUBTRACT BELO-ELGP FROM ZERO GIVING NYBEL
           END-IF
           IF  (I-01 AND I-12)
               ADD BELO-ELGP TO ZERO   GIVING NYBEL
           END-IF
           IF  (I-01 AND NOT-I-30 AND NOT-I-31)
               ADD BELO-ELGP TO ZERO   GIVING NYBEL
           END-IF
           IF  (I-01)
               ADD NYBEL TO ZERO       GIVING BEL132
           END-IF
           IF  (I-01 AND NOT-I-88)
               ADD NYBEL                   TO SUM-X
               ADD NYBEL                   TO SUML2
               ADD NYBEL                   TO SUMLR
               ADD 1                       TO ANTALL
               ADD 1                       TO ANTL2
               ADD 1                       TO ANTLR
               SET NOT-I-94                TO TRUE
               IF  ANTLR NOT = 0
                   SET I-94                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-88)
               ADD NYBEL                   TO FSUM
               ADD NYBEL                   TO FSUML2
               ADD 1                       TO FANTLR
               SET NOT-I-90                TO TRUE
               IF  FANTLR NOT = 0
                   SET I-90                TO TRUE
               END-IF
               ADD 1                       TO FANTL3
               SET NOT-I-91                TO TRUE
               IF  FANTL3 NOT = 0
                   SET I-91                TO TRUE
               END-IF
               ADD 1                       TO FANTL2
               SET NOT-I-92                TO TRUE
               IF  FANTL2 NOT = 0
                   SET I-92                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-30)
               ADD VALUTA TO ZERO      GIVING NYVAL
               SET NOT-I-93                TO TRUE
               IF  NYVAL NOT = 0
                   SET I-93                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-31)
               SUBTRACT VALUTA FROM ZERO GIVING NYVAL
               SET NOT-I-93                TO TRUE
               IF  NYVAL NOT = 0
                   SET I-93                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-12)
               ADD VALUTA TO ZERO      GIVING NYVAL
               SET NOT-I-93                TO TRUE
               IF  NYVAL NOT = 0
                   SET I-93                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-30 AND NOT-I-31)
               ADD VALUTA TO ZERO      GIVING NYVAL
               SET NOT-I-93                TO TRUE
               IF  NYVAL NOT = 0
                   SET I-93                TO TRUE
               END-IF
           END-IF
           IF  (I-01)
               ADD NYVAL TO ZERO       GIVING VAL154
           END-IF
           IF  (I-93)
               PERFORM VALRUT-S
           END-IF
           IF  (I-01 AND I-88)
               OR  (I-01 AND I-U1)
               SET I-44                    TO TRUE
           END-IF.
 
       DTORUT-S SECTION.
       DTORUT-S-P.
           SET NOT-I-96                    TO TRUE
           MOVE 'A'                        TO DTOKOD
           CALL 'DATO8SIF' USING DTOPAR-XX-DATA-FIELDS
           SET NOT-I-96                    TO TRUE
           IF  DTOKOD = 'F'
               SET I-96                    TO TRUE
           END-IF.
      *****************************************************************
      *  RUTINE FOR Å HENTE VALUTAKODER.                              *
      *****************************************************************
 
       VALRUT-S SECTION.
       VALRUT-S-P.
           IF  (NOT-I-15)
               GO TO STDRUT-T
           END-IF
           SET NOT-I-98                    TO TRUE
           MOVE 'A'                        TO AKSEKD
           MOVE VALTYP                     TO VT1UT
           MOVE VALTYP                     TO ADVK
           CALL 'ADVALUTA' USING VALPAR-XX-DATA-FIELDS
           MOVE STDVK                      TO VT3UT
           GO TO ENDVAL-T.
 
       STDRUT-T.
           MOVE STDVT                      TO VT3UT
           SET NOT-I-98                    TO TRUE
           MOVE 'I'                        TO AKSEKD
           MOVE STDVT                      TO STDVK
           CALL 'ADVALUTA' USING VALPAR-XX-DATA-FIELDS
           MOVE ADVK                       TO VT1UT.
 
       ENDVAL-T.
           CONTINUE.
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
 
       RBSR20-S SECTION.
       RBSR20-S-P.
           MOVE ' '                        TO BBEST
           MOVE 'RES24'                    TO LONR
      *R                   MOVE FIRMA     LFIRMA            FIRMANR.
           MOVE '399'                      TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'RSK018  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS.
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
 
       RBSR21-S SECTION.
       RBSR21-S-P.
           MOVE ' '                        TO BBEST
           MOVE 'RES98'                    TO LONR
           MOVE '399'                      TO LFIRMA
      *R                   MOVE FIRMA     LFIRMA            FIRMANR.
           MOVE '000'                      TO LUNDGR
           MOVE 'RSK018  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS021' USING LDATA-XX-DATA-FIELDS.
 
       KORT-GET SECTION.
       KORT-GET-P.
           IF  KORT-EOF-OFF
               READ KORT
               AT END
                   SET KORT-EOF            TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KORT-FLDOFF SECTION.
       KORT-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( KORT-IO-AREA (1:1) = '0'
            AND   KORT-IO-AREA (2:1) = '2' )
               SET NOT-I-13                TO TRUE
               SET NOT-I-11                TO TRUE
               SET NOT-I-12                TO TRUE
               SET NOT-I-14                TO TRUE
               SET NOT-I-15                TO TRUE
           END-EVALUATE.
 
       KORT-FLDSET SECTION.
       KORT-FLDSET-P.
           EVALUATE TRUE
           WHEN ( KORT-IO-AREA (1:1) = '0'
            AND   KORT-IO-AREA (2:1) = '2' )
               MOVE KORT-IO-AREA (1:80)    TO REC080 (1:80)
               MOVE KORT-IO-AREA (3:3)     TO FIRMA (1:3)
               MOVE KORT-IO-AREA (3:9)     TO FKUNDE (1:9)
               MOVE KORT-IO-AREA (6:1)     TO KNR1 (1:1)
               MOVE KORT-IO-AREA (6:6)     TO KUNDE (1:6)
               MOVE KORT-IO-AREA (12:2)    TO TK (1:2)
               MOVE KORT-IO-AREA (14:6)    TO BDATO-IO
               INSPECT BDATO-IO REPLACING ALL ' ' BY '0'
               MOVE KORT-IO-AREA (14:2)    TO BDAG (1:2)
               MOVE KORT-IO-AREA (16:2)    TO BMND (1:2)
               MOVE KORT-IO-AREA (18:2)    TO BAAR (1:2)
               MOVE KORT-IO-AREA (20:6)    TO BILNR (1:6)
               IF  BILNR = SPACES
                   SET I-13                TO TRUE
               END-IF
               MOVE KORT-IO-AREA (26:6)    TO REFNR (1:6)
               IF  REFNR = SPACES
                   SET I-11                TO TRUE
               END-IF
               MOVE KORT-IO-AREA (32:6)    TO FFDATO-IO
               INSPECT FFDATO-IO REPLACING ALL ' ' BY '0'
               MOVE KORT-IO-AREA (32:2)    TO FFDAG (1:2)
               MOVE KORT-IO-AREA (34:2)    TO FFMND (1:2)
               MOVE KORT-IO-AREA (36:2)    TO FFAAR (1:2)
               MOVE KORT-IO-AREA (38:9)    TO BELO-ELGP-IO
               INSPECT BELO-ELGP-IO REPLACING ALL ' ' BY '0'
               MOVE KORT-IO-AREA (47:1)    TO TEGN (1:1)
               IF  TEGN = SPACES
                   SET I-12                TO TRUE
               END-IF
               MOVE KORT-IO-AREA (61:2)    TO BETBET (1:2)
               IF  BETBET = SPACES
                   SET I-14                TO TRUE
               END-IF
               MOVE KORT-IO-AREA (64:10)   TO VALUTA-IO
               INSPECT VALUTA-IO REPLACING ALL ' ' BY '0'
               MOVE KORT-IO-AREA (76:1)    TO VALTYP (1:1)
               MOVE KORT-IO-AREA (74:2)    TO VT2F (1:2)
               IF  VT2F = SPACES
                   SET I-15                TO TRUE
               END-IF
               MOVE KORT-IO-AREA (74:3)    TO STDVT (1:3)
               MOVE KORT-IO-AREA (77:1)    TO BILART (1:1)
           END-EVALUATE.
 
       KORT-IDCHK SECTION.
       KORT-IDCHK-P.
           EVALUATE TRUE
           WHEN ( KORT-IO-AREA (1:1) = '0'
            AND   KORT-IO-AREA (2:1) = '2' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       KORT-IDSET SECTION.
       KORT-IDSET-P.
           EVALUATE TRUE
           WHEN ( KORT-IO-AREA (1:1) = '0'
            AND   KORT-IO-AREA (2:1) = '2' )
               SET I-01                    TO TRUE
           END-EVALUATE.
 
       KORT-CHK-LEVEL SECTION.
       KORT-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( KORT-IO-AREA (1:1) = '0'
            AND   KORT-IO-AREA (2:1) = '2' )
               MOVE LOW-VALUES             TO KORT-LEVEL-01
               MOVE KORT-IO-AREA (3:3)     TO KORT-01-L3-FIRMA
               MOVE KORT-IO-AREA (6:1)     TO KORT-01-L2-KNR1
               MOVE KORT-IO-AREA (6:6)     TO KORT-01-L1-KUNDE
               IF  KORT-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  KORT-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  KORT-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  KORT-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  KORT-01-L3            TO THE-PRIOR-L3
               MOVE  KORT-01-L2            TO THE-PRIOR-L2
               MOVE  KORT-01-L1            TO THE-PRIOR-L1
               SET KORT-LEVEL-INIT         TO TRUE
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
 
       FLISTE-PRINT-LINE SECTION.
       FLISTE-PRINT-LINE-P.
           IF  FLISTE-BEFORE-SKIP > 0
               PERFORM FLISTE-SKIP-BEFORE
           END-IF
           IF  FLISTE-BEFORE-SPACE > 0
               PERFORM FLISTE-SPACE-BEFORE
               IF  FLISTE-AFTER-SKIP > 0
                   PERFORM FLISTE-SKIP-AFTER
               END-IF
               IF  FLISTE-AFTER-SPACE > 0
                   PERFORM FLISTE-SPACE-AFTER
               END-IF
           ELSE
               IF  FLISTE-AFTER-SKIP > 0
                   PERFORM FLISTE-SKIP-AFTER
               END-IF
               PERFORM FLISTE-SPACE-AFTER
           END-IF
           IF  FLISTE-LINE-COUNT NOT < FLISTE-MAX-LINES
               SET I-OV                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OV            TO TRUE
               END-IF
           END-IF.
 
       FLISTE-SKIP-BEFORE SECTION.
       FLISTE-SKIP-BEFORE-P.
           WRITE FLISTE-IO-PRINT        AFTER ADVANCING PAGE
           MOVE 1                          TO FLISTE-LINE-COUNT
           MOVE 0                          TO FLISTE-BEFORE-SKIP
           INITIALIZE FLISTE-IO-AREA.
 
       FLISTE-SPACE-BEFORE SECTION.
       FLISTE-SPACE-BEFORE-P.
           WRITE FLISTE-IO-PRINT        AFTER FLISTE-BEFORE-SPACE LINES
           ADD FLISTE-BEFORE-SPACE         TO FLISTE-LINE-COUNT
           MOVE SPACES TO FLISTE-IO-AREA
           INITIALIZE FLISTE-IO-AREA
           MOVE 0                          TO FLISTE-BEFORE-SPACE.
 
       FLISTE-SKIP-AFTER SECTION.
       FLISTE-SKIP-AFTER-P.
           WRITE FLISTE-IO-PRINT       BEFORE ADVANCING PAGE
           MOVE 1                          TO FLISTE-LINE-COUNT
           MOVE 0                          TO FLISTE-AFTER-SKIP
           INITIALIZE FLISTE-IO-AREA.
 
       FLISTE-SPACE-AFTER SECTION.
       FLISTE-SPACE-AFTER-P.
           WRITE FLISTE-IO-PRINT       BEFORE FLISTE-AFTER-SPACE LINES
           ADD FLISTE-AFTER-SPACE          TO FLISTE-LINE-COUNT
           INITIALIZE FLISTE-IO-AREA
           MOVE 0                          TO FLISTE-AFTER-SPACE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-02                        TO TRUE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (127:2) TO KUNBET (1:2)
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-03                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE KUNDE                  TO LISTE-IO-AREA (3:6)
               MOVE TK                     TO LISTE-IO-AREA (11:2)
               MOVE BDATO                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (14:8)
               MOVE BILART                 TO LISTE-IO-AREA (24:1)
               MOVE BILNR                  TO LISTE-IO-AREA (26:6)
               MOVE REFNR                  TO LISTE-IO-AREA (34:6)
               MOVE FFDATO                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (41:8)
               MOVE NYBEL                  TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (50:13)
               MOVE TEGN                   TO LISTE-IO-AREA (65:1)
               MOVE NYVAL                  TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (68:14)
               MOVE VT3UT                  TO LISTE-IO-AREA (82:3)
               IF  (I-88)
                   MOVE '* AVVIST  '       TO LISTE-IO-AREA (87:10)
               END-IF
               IF  (NOT-I-88)
                   MOVE 'GODKJENT  '       TO LISTE-IO-AREA (87:10)
               END-IF
               IF  (I-80)
                   MOVE 'FIRMA IKKE I FIRMAMASTER' TO LISTE-IO-AREA
                                                              (109:24)
               END-IF
               IF  (I-81)
                   MOVE 'UKJENT KUNDE/LEVERANDØR ' TO LISTE-IO-AREA
                                                              (109:24)
               END-IF
               IF  (I-83)
                   MOVE 'UGYLDIG BILAGSDATO      ' TO LISTE-IO-AREA
                                                              (109:24)
               END-IF
               IF  (I-84)
                   MOVE 'UGYLDIG FORFALLSDATO    ' TO LISTE-IO-AREA
                                                              (109:24)
               END-IF
               IF  (I-87)
                   MOVE 'UGYLDIG FORTEGN         ' TO LISTE-IO-AREA
                                                              (109:24)
               END-IF
               IF  (I-13)
                   MOVE 'BILAGSNR BLANK          ' TO LISTE-IO-AREA
                                                              (109:24)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-01 AND NOT-I-88)
               MOVE SPACES TO RESFIL-IO-AREA
               INITIALIZE RESFIL-IO-AREA
               MOVE '31'                   TO RESFIL-IO-AREA (1:2)
               MOVE FKUNDE                 TO RESFIL-IO-AREA (3:9)
               IF  (NOT-I-11)
                   MOVE REFNR              TO RESFIL-IO-AREA (12:6)
               END-IF
               IF  (I-11)
                   MOVE BILNR              TO RESFIL-IO-AREA (12:6)
               END-IF
               MOVE TK                     TO RESFIL-IO-AREA (18:2)
               MOVE BAAR                   TO RESFIL-IO-AREA (20:2)
               MOVE BMND                   TO RESFIL-IO-AREA (22:2)
               MOVE BDAG                   TO RESFIL-IO-AREA (24:2)
               MOVE BILNR                  TO RESFIL-IO-AREA (26:6)
               MOVE BILART                 TO RESFIL-IO-AREA (32:1)
               MOVE FFAAR                  TO RESFIL-IO-AREA (33:2)
               MOVE FFMND                  TO RESFIL-IO-AREA (35:2)
               MOVE FFDAG                  TO RESFIL-IO-AREA (37:2)
               MOVE NYBEL-IO               TO RESFIL-IO-AREA (39:9)
               MOVE '1'                    TO RESFIL-IO-AREA (48:1)
               IF  (NOT-I-14)
                   MOVE BETBET             TO RESFIL-IO-AREA (49:2)
               END-IF
               IF  (I-14)
                   MOVE KUNBET             TO RESFIL-IO-AREA (49:2)
               END-IF
               MOVE NYVAL-IO               TO RESFIL-IO-AREA (51:10)
               MOVE VT1UT                  TO RESFIL-IO-AREA (70:1)
               INITIALIZE VT1UT
               MOVE BEL132                 TO XO-112P
               MOVE XO-112P-EF             TO RESFIL-IO-AREA (114:7)
               MOVE VAL154                 TO XO-114P
               MOVE XO-114P-EF             TO RESFIL-IO-AREA (121:8)
               MOVE VT3UT                  TO RESFIL-IO-AREA (129:3)
               INITIALIZE VT3UT
               MOVE 'NYER'                 TO RESFIL-IO-AREA (188:4)
               IF  (I-32)
                   MOVE 'KFAK'             TO RESFIL-IO-AREA (188:4)
               END-IF
               MOVE DDATO8                 TO XO-80P
               MOVE XO-80P-EF              TO RESFIL-IO-AREA (192:5)
               MOVE TIDSP                  TO XO-60P
               MOVE XO-60P-EF              TO RESFIL-IO-AREA (197:4)
               WRITE RESFIL-IO-AREA
           END-IF
           IF  (I-LR AND I-U7 AND I-U8)
           AND (I-02 AND I-03 AND I-97)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE PSDS                   TO LISTE-IO-AREA (41:80)
               MOVE R                      TO LISTE-IO-AREA (113:8)
               MOVE P-IO                   TO LISTE-IO-AREA (118:3)
               MOVE S-IO                   TO LISTE-IO-AREA (116:5)
               MOVE DTODTO                 TO LISTE-IO-AREA (115:6)
               MOVE DTO8SI                 TO LISTE-IO-AREA (113:8)
               MOVE DTOMEL                 TO LISTE-IO-AREA (64:57)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-01 AND I-88)
               MOVE SPACES TO FLISTE-IO-AREA
               INITIALIZE FLISTE-IO-AREA
               MOVE KUNDE                  TO FLISTE-IO-AREA (3:6)
               MOVE TK                     TO FLISTE-IO-AREA (11:2)
               MOVE BDATO                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO FLISTE-IO-AREA (14:8)
               MOVE BILART                 TO FLISTE-IO-AREA (24:1)
               MOVE BILNR                  TO FLISTE-IO-AREA (26:6)
               MOVE REFNR                  TO FLISTE-IO-AREA (34:6)
               MOVE FFDATO                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO FLISTE-IO-AREA (41:8)
               MOVE NYBEL                  TO XO-72YY9R
               MOVE XO-72YY9R              TO FLISTE-IO-AREA (50:13)
               MOVE TEGN                   TO FLISTE-IO-AREA (65:1)
               MOVE NYVAL                  TO XO-82YY9R
               MOVE XO-82YY9R              TO FLISTE-IO-AREA (68:14)
               MOVE VT3UT                  TO FLISTE-IO-AREA (82:3)
               INITIALIZE VT3UT
               IF  (I-80)
                   MOVE 'FIRMA IKKE I FIRMAMASTER' TO FLISTE-IO-AREA
                                                              (109:24)
               END-IF
               IF  (I-81)
                   MOVE 'UKJENT KUNDE/LEVERANDØR ' TO FLISTE-IO-AREA
                                                              (109:24)
               END-IF
               IF  (I-83)
                   MOVE 'UGYLDIG BILAGSDATO      ' TO FLISTE-IO-AREA
                                                              (109:24)
               END-IF
               IF  (I-84)
                   MOVE 'UGYLDIG FORFALLSDATO    ' TO FLISTE-IO-AREA
                                                              (109:24)
               END-IF
               IF  (I-87)
                   MOVE 'UGYLDIG FORTEGN         ' TO FLISTE-IO-AREA
                                                              (109:24)
               END-IF
               IF  (I-13)
                   MOVE 'BILAGSNR BLANK          ' TO FLISTE-IO-AREA
                                                              (109:24)
               END-IF
               MOVE 1                      TO FLISTE-AFTER-SPACE
               PERFORM FLISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L3)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KVITTERING PROG. RSK018 ' TO LISTE-IO-AREA (1:24)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'K O N T R O L L  A V' TO LISTE-IO-AREA (20:20)
               MOVE 'G A M L E  R E S K O N T' TO LISTE-IO-AREA (42:24)
               MOVE 'R O P O S T E R'      TO LISTE-IO-AREA (67:15)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (100:8)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (35:30)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'RESKNR'               TO LISTE-IO-AREA (3:6)
               MOVE 'TK'                   TO LISTE-IO-AREA (11:2)
               MOVE 'BIL.DTO'              TO LISTE-IO-AREA (15:7)
               MOVE 'A'                    TO LISTE-IO-AREA (24:1)
               MOVE 'BIL.NR'               TO LISTE-IO-AREA (26:6)
               MOVE 'REF.NR'               TO LISTE-IO-AREA (34:6)
               MOVE 'FF-.DTO'              TO LISTE-IO-AREA (42:7)
               MOVE '       BELØP'         TO LISTE-IO-AREA (51:12)
               MOVE 'T'                    TO LISTE-IO-AREA (65:1)
               MOVE ' VALUTABELØP'         TO LISTE-IO-AREA (70:12)
               MOVE 'T'                    TO LISTE-IO-AREA (84:1)
               MOVE 'FEILMELDINGER           ' TO LISTE-IO-AREA
                                                              (109:24)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-88 AND NOT-I-89)
      *LISTE  H 3101   L3
      *      OR        OV
               MOVE SPACES TO FLISTE-IO-AREA
               INITIALIZE FLISTE-IO-AREA
               MOVE 'AVSTEMMING PROG. RSK018 ' TO FLISTE-IO-AREA (1:24)
               MOVE 01                     TO FLISTE-BEFORE-SKIP
               MOVE 3                      TO FLISTE-BEFORE-SPACE
               MOVE 1                      TO FLISTE-AFTER-SPACE
               PERFORM FLISTE-PRINT-LINE
      *       H  1     L3
               MOVE SPACES TO FLISTE-IO-AREA
               INITIALIZE FLISTE-IO-AREA
               MOVE 'A V V I S T E   R E S K ' TO FLISTE-IO-AREA
                                                               (21:24)
               MOVE 'O N T R O P O S T E R   ' TO FLISTE-IO-AREA
                                                               (45:24)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO FLISTE-IO-AREA (100:8)
               MOVE 1                      TO FLISTE-AFTER-SPACE
               PERFORM FLISTE-PRINT-LINE
      *       H  1     L3
               MOVE SPACES TO FLISTE-IO-AREA
               INITIALIZE FLISTE-IO-AREA
               MOVE FINAVN                 TO FLISTE-IO-AREA (35:30)
               MOVE 1                      TO FLISTE-AFTER-SPACE
               PERFORM FLISTE-PRINT-LINE
      *       H 11     L3
               MOVE SPACES TO FLISTE-IO-AREA
               INITIALIZE FLISTE-IO-AREA
               MOVE 'RESKNR'               TO FLISTE-IO-AREA (3:6)
               MOVE 'TK'                   TO FLISTE-IO-AREA (11:2)
               MOVE 'BIL.DTO'              TO FLISTE-IO-AREA (15:7)
               MOVE 'A'                    TO FLISTE-IO-AREA (24:1)
               MOVE 'BIL.NR'               TO FLISTE-IO-AREA (26:6)
               MOVE 'REF.NR'               TO FLISTE-IO-AREA (34:6)
               MOVE 'FF-.DTO'              TO FLISTE-IO-AREA (42:7)
               MOVE '       BELØP'         TO FLISTE-IO-AREA (51:12)
               MOVE 'T'                    TO FLISTE-IO-AREA (65:1)
               MOVE ' VALUTABELØP'         TO FLISTE-IO-AREA (70:12)
               MOVE 'T'                    TO FLISTE-IO-AREA (84:1)
               MOVE 'FEILMELDINGER           ' TO FLISTE-IO-AREA
                                                              (109:24)
               MOVE 1                      TO FLISTE-AFTER-SPACE
               PERFORM FLISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'KVITTERING PROG. RSK018 ' TO LISTE-IO-AREA (1:24)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'K O N T R O L L  A V' TO LISTE-IO-AREA (20:20)
               MOVE 'G A M L E  R E S K O N T' TO LISTE-IO-AREA (42:24)
               MOVE 'R O P O S T E R'      TO LISTE-IO-AREA (67:15)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (100:8)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (35:30)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'RESKNR'               TO LISTE-IO-AREA (3:6)
               MOVE 'TK'                   TO LISTE-IO-AREA (11:2)
               MOVE 'BIL.DTO'              TO LISTE-IO-AREA (15:7)
               MOVE 'A'                    TO LISTE-IO-AREA (24:1)
               MOVE 'BIL.NR'               TO LISTE-IO-AREA (26:6)
               MOVE 'REF.NR'               TO LISTE-IO-AREA (34:6)
               MOVE 'FF-.DTO'              TO LISTE-IO-AREA (42:7)
               MOVE '       BELØP'         TO LISTE-IO-AREA (51:12)
               MOVE 'T'                    TO LISTE-IO-AREA (65:1)
               MOVE ' VALUTABELØP'         TO LISTE-IO-AREA (70:12)
               MOVE 'T'                    TO LISTE-IO-AREA (84:1)
               MOVE 'FEILMELDINGER           ' TO LISTE-IO-AREA
                                                              (109:24)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-OV)
      *LISTE  H 3101   L3
      *      OR        OV
               MOVE SPACES TO FLISTE-IO-AREA
               INITIALIZE FLISTE-IO-AREA
               MOVE 'AVSTEMMING PROG. RSK018 ' TO FLISTE-IO-AREA (1:24)
               MOVE 01                     TO FLISTE-BEFORE-SKIP
               MOVE 3                      TO FLISTE-BEFORE-SPACE
               MOVE 1                      TO FLISTE-AFTER-SPACE
               PERFORM FLISTE-PRINT-LINE
      *       H  1     L3
               MOVE SPACES TO FLISTE-IO-AREA
               INITIALIZE FLISTE-IO-AREA
               MOVE 'A V V I S T E   R E S K ' TO FLISTE-IO-AREA
                                                               (21:24)
               MOVE 'O N T R O P O S T E R   ' TO FLISTE-IO-AREA
                                                               (45:24)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO FLISTE-IO-AREA (100:8)
               MOVE 1                      TO FLISTE-AFTER-SPACE
               PERFORM FLISTE-PRINT-LINE
      *       H  1     L3
               MOVE SPACES TO FLISTE-IO-AREA
               INITIALIZE FLISTE-IO-AREA
               MOVE FINAVN                 TO FLISTE-IO-AREA (35:30)
               MOVE 1                      TO FLISTE-AFTER-SPACE
               PERFORM FLISTE-PRINT-LINE
      *       H 11     L3
               MOVE SPACES TO FLISTE-IO-AREA
               INITIALIZE FLISTE-IO-AREA
               MOVE 'RESKNR'               TO FLISTE-IO-AREA (3:6)
               MOVE 'TK'                   TO FLISTE-IO-AREA (11:2)
               MOVE 'BIL.DTO'              TO FLISTE-IO-AREA (15:7)
               MOVE 'A'                    TO FLISTE-IO-AREA (24:1)
               MOVE 'BIL.NR'               TO FLISTE-IO-AREA (26:6)
               MOVE 'REF.NR'               TO FLISTE-IO-AREA (34:6)
               MOVE 'FF-.DTO'              TO FLISTE-IO-AREA (42:7)
               MOVE '       BELØP'         TO FLISTE-IO-AREA (51:12)
               MOVE 'T'                    TO FLISTE-IO-AREA (65:1)
               MOVE ' VALUTABELØP'         TO FLISTE-IO-AREA (70:12)
               MOVE 'T'                    TO FLISTE-IO-AREA (84:1)
               MOVE 'FEILMELDINGER           ' TO FLISTE-IO-AREA
                                                              (109:24)
               MOVE 1                      TO FLISTE-AFTER-SPACE
               PERFORM FLISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L2 AND I-94)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTAL GRUPPE'         TO LISTE-IO-AREA (18:12)
               MOVE KNR1                   TO LISTE-IO-AREA (31:1)
               MOVE FIRMA                  TO LISTE-IO-AREA (44:3)
               MOVE SUML2                  TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (49:14)
               INITIALIZE SUML2
               MOVE ANTL2                  TO XO-50YNZ
               MOVE XO-50YNZ               TO LISTE-IO-AREA (70:5)
               INITIALIZE ANTL2
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L3 AND I-94)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'T O T A L'            TO LISTE-IO-AREA (21:9)
               MOVE 'F I R M A'            TO LISTE-IO-AREA (33:9)
               MOVE FIRMA                  TO LISTE-IO-AREA (44:3)
               MOVE SUM-X                  TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (49:14)
               INITIALIZE SUM-X
               MOVE ANTALL                 TO XO-50YNZ
               MOVE XO-50YNZ               TO LISTE-IO-AREA (70:5)
               INITIALIZE ANTALL
               MOVE 2                      TO LISTE-BEFORE-SPACE
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'T O T A L'            TO LISTE-IO-AREA (21:9)
               MOVE '         '            TO LISTE-IO-AREA (33:9)
               MOVE SUMLR                  TO XO-112YY9R
               MOVE XO-112YY9R             TO LISTE-IO-AREA (45:18)
               INITIALIZE SUMLR
               MOVE ANTLR                  TO XO-50YNZ
               MOVE XO-50YNZ               TO LISTE-IO-AREA (70:5)
               INITIALIZE ANTLR
      * DUMMY-LINJE FOR Å FJERNE KOMPILERINGSFEIL
               MOVE 2                      TO LISTE-BEFORE-SPACE
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L2 AND I-92 AND I-44)
               MOVE SPACES TO FLISTE-IO-AREA
               INITIALIZE FLISTE-IO-AREA
               MOVE 'TOTAL GRUPPE'         TO FLISTE-IO-AREA (18:12)
               MOVE KNR1                   TO FLISTE-IO-AREA (31:1)
               MOVE FIRMA                  TO FLISTE-IO-AREA (44:3)
               MOVE FSUML2                 TO XO-82YY9R
               MOVE XO-82YY9R              TO FLISTE-IO-AREA (49:14)
               INITIALIZE FSUML2
               MOVE FANTL2                 TO XO-50YNZ
               MOVE XO-50YNZ               TO FLISTE-IO-AREA (70:5)
               INITIALIZE FANTL2
               MOVE 1                      TO FLISTE-BEFORE-SPACE
               MOVE 2                      TO FLISTE-AFTER-SPACE
               PERFORM FLISTE-PRINT-LINE
           END-IF
           IF  (I-L3 AND I-91 AND I-44)
               MOVE SPACES TO FLISTE-IO-AREA
               INITIALIZE FLISTE-IO-AREA
               MOVE 'T O T A L'            TO FLISTE-IO-AREA (21:9)
               MOVE 'F I R M A'            TO FLISTE-IO-AREA (33:9)
               MOVE FIRMA                  TO FLISTE-IO-AREA (44:3)
               MOVE FSUM                   TO XO-82YY9R
               MOVE XO-82YY9R              TO FLISTE-IO-AREA (49:14)
               INITIALIZE FSUM
               MOVE FANTL3                 TO XO-50YNZ
               MOVE XO-50YNZ               TO FLISTE-IO-AREA (70:5)
               INITIALIZE FANTL3
               MOVE 2                      TO FLISTE-BEFORE-SPACE
               MOVE 2                      TO FLISTE-AFTER-SPACE
               PERFORM FLISTE-PRINT-LINE
           END-IF
           IF  (I-LR AND I-94)
               MOVE SPACES TO FLISTE-IO-AREA
               INITIALIZE FLISTE-IO-AREA
               IF  (NOT-I-90)
                   MOVE '>>>>>>>>>>>>>>>>>>>>>>>>' TO FLISTE-IO-AREA
                                                                (4:24)
               END-IF
               IF  (NOT-I-90)
                   MOVE '       INGEN FEIL       ' TO FLISTE-IO-AREA
                                                               (28:24)
               END-IF
               IF  (NOT-I-90)
                   MOVE '<<<<<<<<<<<<<<<<<<<<<<<<' TO FLISTE-IO-AREA
                                                               (52:24)
               END-IF
               IF  (I-90)
                   MOVE 'ANTALL FEIL TOTALT:' TO FLISTE-IO-AREA (2:19)
               END-IF
               IF  (I-90)
                   MOVE FANTLR             TO XO-50YNZ
                   MOVE XO-50YNZ           TO FLISTE-IO-AREA (21:5)
                   INITIALIZE FANTLR
               END-IF
               MOVE 2                      TO FLISTE-BEFORE-SPACE
               MOVE 2                      TO FLISTE-AFTER-SPACE
               PERFORM FLISTE-PRINT-LINE
           END-IF
           IF  (I-LR AND I-44)
               MOVE SPACES TO INPFEIL-IO-AREA
               INITIALIZE INPFEIL-IO-AREA
               MOVE '* ÅÅ JOB JNM=RES98M,CLAS' TO INPFEIL-IO-AREA
                                                                (1:24)
               MOVE 'S=5,DISP=K,PRI=9        ' TO INPFEIL-IO-AREA
                                                               (25:24)
               WRITE INPFEIL-IO-AREA
               MOVE SPACES TO INPFEIL-IO-AREA
               INITIALIZE INPFEIL-IO-AREA
               MOVE '* ÅÅ LST LST=SYSLST,LST=' TO INPFEIL-IO-AREA
                                                                (1:24)
               MOVE '00E,CLASS=Z,DISP=H      ' TO INPFEIL-IO-AREA
                                                               (25:24)
               WRITE INPFEIL-IO-AREA
               MOVE SPACES TO INPFEIL-IO-AREA
               INITIALIZE INPFEIL-IO-AREA
               MOVE '// JOB RES98M  SENDER MA' TO INPFEIL-IO-AREA
                                                                (1:24)
               MOVE 'IL NÅR FEIL I RSK018/NYE' TO INPFEIL-IO-AREA
                                                               (25:24)
               MOVE 'RESKONTROPOSTER         ' TO INPFEIL-IO-AREA
                                                               (49:24)
               WRITE INPFEIL-IO-AREA
               MOVE SPACES TO INPFEIL-IO-AREA
               INITIALIZE INPFEIL-IO-AREA
               MOVE '/*'                   TO INPFEIL-IO-AREA (1:2)
               WRITE INPFEIL-IO-AREA
               MOVE SPACES TO INPFEIL-IO-AREA
               INITIALIZE INPFEIL-IO-AREA
               MOVE '*  STEP 01 TELNET-CLIENT' TO INPFEIL-IO-AREA
                                                                (1:24)
               MOVE ' MAIL FRA HOST.         ' TO INPFEIL-IO-AREA
                                                               (25:24)
               WRITE INPFEIL-IO-AREA
               MOVE SPACES TO INPFEIL-IO-AREA
               INITIALIZE INPFEIL-IO-AREA
               MOVE '* ÅÅ LST CLASS=Z,DISP=H ' TO INPFEIL-IO-AREA
                                                                (1:24)
               MOVE '                        ' TO INPFEIL-IO-AREA
                                                               (25:24)
               WRITE INPFEIL-IO-AREA
               MOVE SPACES TO INPFEIL-IO-AREA
               INITIALIZE INPFEIL-IO-AREA
               MOVE '// EXEC EMAIL,SIZE=E' TO INPFEIL-IO-AREA (1:20)
               MOVE 'MAIL,PARM='           TO INPFEIL-IO-AREA (21:10)
               MOVE '''ID=02'''              TO INPFEIL-IO-AREA (31:7)
               WRITE INPFEIL-IO-AREA
               MOVE SPACES TO INPFEIL-IO-AREA
               INITIALIZE INPFEIL-IO-AREA
               MOVE 'SET FROM=REGNSKAP@AUTODA' TO INPFEIL-IO-AREA
                                                                (1:24)
               MOVE 'TA.NO                   ' TO INPFEIL-IO-AREA
                                                               (25:24)
      *       T        LR 44
      *                                  24 "SET TO=OPERA@AUTODATA.NO"
      *       T        LR 44
      *                                  24 "SET TO=SVERRE@AUTODATA.N"
      *                                  48 "O                       "
      *       T        LR 44
      *                                  24 "SET TO=KNUT@AUTODATA.NO "
      *       T        LR 44
      *                                  24 "SET TO=ELIN@AUTODATA.NO "
               WRITE INPFEIL-IO-AREA
               MOVE SPACES TO INPFEIL-IO-AREA
               INITIALIZE INPFEIL-IO-AREA
               MOVE 'SET TO=MORTEN@AUTODATA.N' TO INPFEIL-IO-AREA
                                                                (1:24)
               MOVE 'O                       ' TO INPFEIL-IO-AREA
                                                               (25:24)
               WRITE INPFEIL-IO-AREA
               MOVE SPACES TO INPFEIL-IO-AREA
               INITIALIZE INPFEIL-IO-AREA
               MOVE 'SET SUBJECT=Feil i DOP12' TO INPFEIL-IO-AREA
                                                                (1:24)
               MOVE 'UD - Nye reskontroposter' TO INPFEIL-IO-AREA
                                                               (25:24)
               MOVE '/kontantsalg-prog RSK018' TO INPFEIL-IO-AREA
                                                               (49:24)
               WRITE INPFEIL-IO-AREA
               MOVE SPACES TO INPFEIL-IO-AREA
               INITIALIZE INPFEIL-IO-AREA
               MOVE 'SET DISP=HOLD           ' TO INPFEIL-IO-AREA
                                                                (1:24)
               WRITE INPFEIL-IO-AREA
               MOVE SPACES TO INPFEIL-IO-AREA
               INITIALIZE INPFEIL-IO-AREA
               MOVE 'TEXT                    ' TO INPFEIL-IO-AREA
                                                                (1:24)
               WRITE INPFEIL-IO-AREA
               MOVE SPACES TO INPFEIL-IO-AREA
               INITIALIZE INPFEIL-IO-AREA
               MOVE 'Feil i kjøringen - Sjekk' TO INPFEIL-IO-AREA
                                                                (1:24)
               MOVE ' oppgave RES98 på report' TO INPFEIL-IO-AREA
                                                               (25:24)
               MOVE ' web.                   ' TO INPFEIL-IO-AREA
                                                               (49:24)
               WRITE INPFEIL-IO-AREA
               MOVE SPACES TO INPFEIL-IO-AREA
               INITIALIZE INPFEIL-IO-AREA
               MOVE '                        ' TO INPFEIL-IO-AREA
                                                                (1:24)
               MOVE '                        ' TO INPFEIL-IO-AREA
                                                               (25:24)
               MOVE '                        ' TO INPFEIL-IO-AREA
                                                               (49:24)
               WRITE INPFEIL-IO-AREA
               MOVE SPACES TO INPFEIL-IO-AREA
               INITIALIZE INPFEIL-IO-AREA
               MOVE 'Hvis det er feil i sendi' TO INPFEIL-IO-AREA
                                                                (1:24)
               MOVE 'ngen og Elin og Morten e' TO INPFEIL-IO-AREA
                                                               (25:24)
               MOVE 'r borte; varsle andre i ' TO INPFEIL-IO-AREA
                                                               (49:24)
               WRITE INPFEIL-IO-AREA
               MOVE SPACES TO INPFEIL-IO-AREA
               INITIALIZE INPFEIL-IO-AREA
               MOVE 'systemavdelingen.       ' TO INPFEIL-IO-AREA
                                                                (1:24)
               WRITE INPFEIL-IO-AREA
               MOVE SPACES TO INPFEIL-IO-AREA
               INITIALIZE INPFEIL-IO-AREA
               MOVE '/+                      ' TO INPFEIL-IO-AREA
                                                                (1:24)
               WRITE INPFEIL-IO-AREA
               MOVE SPACES TO INPFEIL-IO-AREA
               INITIALIZE INPFEIL-IO-AREA
               MOVE 'SEND                    ' TO INPFEIL-IO-AREA
                                                                (1:24)
               WRITE INPFEIL-IO-AREA
               MOVE SPACES TO INPFEIL-IO-AREA
               INITIALIZE INPFEIL-IO-AREA
               MOVE 'QUIT                    ' TO INPFEIL-IO-AREA
                                                                (1:24)
               WRITE INPFEIL-IO-AREA
               MOVE SPACES TO INPFEIL-IO-AREA
               INITIALIZE INPFEIL-IO-AREA
               MOVE '/*'                   TO INPFEIL-IO-AREA (1:2)
               WRITE INPFEIL-IO-AREA
               MOVE SPACES TO INPFEIL-IO-AREA
               INITIALIZE INPFEIL-IO-AREA
               MOVE '/&'                   TO INPFEIL-IO-AREA (1:2)
               WRITE INPFEIL-IO-AREA
               MOVE SPACES TO INPFEIL-IO-AREA
               INITIALIZE INPFEIL-IO-AREA
               MOVE '* ÅÅ EOJ'             TO INPFEIL-IO-AREA (1:8)
               WRITE INPFEIL-IO-AREA
           END-IF
           IF  (I-LR AND I-U1 AND I-U2)
           AND (I-U3 AND I-U4 AND I-U5)
           AND (I-46)
               MOVE SPACES TO FLISTE-IO-AREA
               INITIALIZE FLISTE-IO-AREA
               MOVE AKSEKD                 TO FLISTE-IO-AREA (132:1)
               MOVE NORVN                  TO FLISTE-IO-AREA (113:20)
               MOVE STDVN                  TO FLISTE-IO-AREA (113:20)
               MOVE LANDKD                 TO FLISTE-IO-AREA (131:2)
               MOVE LAND                   TO FLISTE-IO-AREA (113:20)
               MOVE VALIX-IO               TO FLISTE-IO-AREA (130:3)
               MOVE DIV                    TO FLISTE-IO-AREA (123:10)
               MOVE LONR                   TO FLISTE-IO-AREA (128:5)
               MOVE LFIRMA                 TO FLISTE-IO-AREA (130:3)
               MOVE LUNDGR                 TO FLISTE-IO-AREA (130:3)
               MOVE LPROG                  TO FLISTE-IO-AREA (125:8)
               MOVE LANTX-IO               TO FLISTE-IO-AREA (130:3)
               MOVE LOPNVN                 TO FLISTE-IO-AREA (98:35)
               MOVE LPRIID                 TO FLISTE-IO-AREA (129:4)
               MOVE BJOBN                  TO FLISTE-IO-AREA (125:8)
               MOVE BBEST                  TO FLISTE-IO-AREA (132:1)
               MOVE BPERS                  TO FLISTE-IO-AREA (103:30)
               MOVE BETTB                  TO FLISTE-IO-AREA (93:40)
               MOVE BFORS                  TO FLISTE-IO-AREA (93:40)
               MOVE BMEMO                  TO FLISTE-IO-AREA (93:40)
               MOVE BANTX-IO               TO FLISTE-IO-AREA (130:3)
               MOVE BPCLAS                 TO FLISTE-IO-AREA (132:1)
               MOVE BPRJE                  TO FLISTE-IO-AREA (130:3)
               MOVE BBEST                  TO FLISTE-IO-AREA (132:1)
               MOVE LONR                   TO FLISTE-IO-AREA (128:5)
               MOVE LFIRMA                 TO FLISTE-IO-AREA (130:3)
               MOVE LUNDGR                 TO FLISTE-IO-AREA (130:3)
               MOVE LPROG                  TO FLISTE-IO-AREA (125:8)
               MOVE LANTX-IO               TO FLISTE-IO-AREA (130:3)
               MOVE LPRIID                 TO FLISTE-IO-AREA (129:4)
               MOVE BBEST                  TO FLISTE-IO-AREA (132:1)
               MOVE LONR                   TO FLISTE-IO-AREA (128:5)
               MOVE LFIRMA                 TO FLISTE-IO-AREA (130:3)
               MOVE LUNDGR                 TO FLISTE-IO-AREA (130:3)
               MOVE LPROG                  TO FLISTE-IO-AREA (125:8)
               MOVE LANTX-IO               TO FLISTE-IO-AREA (130:3)
               MOVE LPRIID                 TO FLISTE-IO-AREA (129:4)
               MOVE VALIX-IO               TO FLISTE-IO-AREA (130:3)
               MOVE DIV                    TO FLISTE-IO-AREA (123:10)
               MOVE REC080                 TO FLISTE-IO-AREA (53:80)
               MOVE 2                      TO FLISTE-BEFORE-SPACE
               MOVE 2                      TO FLISTE-AFTER-SPACE
               PERFORM FLISTE-PRINT-LINE
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
           SET KORT-LEVEL-INIT             TO TRUE
           INITIALIZE KORT-DATA-FIELDS
           SET KORT-EOF-OFF                TO TRUE
           SET KORT-PROCESS                TO TRUE
           OPEN INPUT KORT
           OPEN OUTPUT RESFIL
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES
           OPEN OUTPUT FLISTE
           INITIALIZE FLISTE-IO-AREA
           INITIALIZE FLISTE-DATA-FIELDS
           MOVE 57                         TO FLISTE-MAX-LINES
           OPEN INPUT FIRMAF
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           OPEN OUTPUT INPFEIL.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE KORT
           CLOSE RESFIL
           IF LISTE-IO-AREA NOT = SPACES
             WRITE LISTE-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE-IO-AREA
           END-IF
           CLOSE LISTE
           IF FLISTE-IO-AREA NOT = SPACES
             WRITE FLISTE-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO FLISTE-IO-AREA
           END-IF
           CLOSE FLISTE
           CLOSE FIRMAF
           CLOSE KUNDEMA
           CLOSE INPFEIL.
 
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
