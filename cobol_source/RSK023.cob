       IDENTIFICATION DIVISION.
       PROGRAM-ID. RSK023R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM.......: RSK023                                       *
      *  UTSKRIFT AV DAGLIG POSTERINGSLISTE                           *
      *  RECORDS FRA FAKTURA-RUTINE (FRAFAK=F) PRINTES                *
      *   IKKE LENGERE. KUN MED I SUMMERING.                          *
      *E 02.04.96: FJERNET KOLONNER MED DATA SOM IKKE ER IBRUK:       *
      *            VEKSEL/AKSEPT-NR, MASKIN-ORDRE, DISKONTREINGSSTED. *
      *            ENDRET VALUTATYPE TIL INTERNASJONAL STANDARD.      *
      *E 03.04.96: KOMPILERT UTEN ENDRING: NY VERSJON AV ADVALUTA.    *
      *E 15.04.96: BELØP LA SEG OVER KONTONR (KOSTSTED).              *
      *E 23.01.97: LAGET EGEN LINJE FOR SUM PR BILAGSNR.              *
      *E           VISER TEKSTER TIL HVER ENKELT LINJE.               *
      *E           HENTER TEKST TIL MOTKONTO FOR BOKF-TRANSER.        *
      *E 24.01.97: TATT UT STJERNER I SUM-LINJE.                      *
      *E 12.06.97: TAKLER ALTERNATIVT KONTONR.                        *
      *E 29.07.97: LA STED FEIL FOR ALT.KTONR (VENSTRE KONTO).        *
      *E 23.09.98: NY VERSJON AV ADVALUTA.                            *
      *E 29.08.00: LEGGER 4-SIFRET AVD I KONTONR FOR KJØP OG SALG.    *
      *E 13.12.01: TAR MED FIRMA SOM IKKE PRINTES I AKK.-TALL.        *
      *E 25.03.10: VISER AVD4 SELVOM MAN IKKE KREVER KOSTSTED I RG02  *
      *E 15.04.11: UTVIDET REGREC                                     *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RSK023.rpg
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
           SELECT DAGREG
               ASSIGN TO UT-S-DAGREG
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS DAGREG-STATUS.
           SELECT BILTEXT
               ASSIGN TO BILTEXT
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS BILTEXT-STATUS
               RECORD KEY IS BILTEXT-KEY1.
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
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD DAGREG
               BLOCK CONTAINS 240
               RECORD CONTAINS 240.
       01  DAGREG-IO-AREA.
           05  DAGREG-IO-AREA-X            PICTURE X(240).
       FD BILTEXT
               RECORD CONTAINS 40.
       01  BILTEXT-IO-AREA.
           05  BILTEXT-IO-AREA-X.
               10  BILTEXT-KEY1            PICTURE X(11).
               10  FILLER                  PICTURE X(29).
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
      *BUGFILO O   F  80  80            PRINTERSYSLST
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
           10  DAGREG-STATUS               PICTURE 99 VALUE 0.
           10  BILTEXT-STATUS              PICTURE 99 VALUE 0.
           10  SYSPARM-STATUS              PICTURE 99 VALUE 0.
           10  KONTOMA-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  LDATA-XX-STATUS             PICTURE 99 VALUE 0.
           10  VALPAR-XX-STATUS            PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  DAGREG-EOF-OFF          VALUE '0'.
               88  DAGREG-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  DAGREG-READ-OFF         VALUE '0'.
               88  DAGREG-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  DAGREG-PROCESS-OFF      VALUE '0'.
               88  DAGREG-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  DAGREG-LEVEL-INIT-OFF   VALUE '0'.
               88  DAGREG-LEVEL-INIT       VALUE '1'.
           05  BILTEXT-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  DAGREG-LEVEL-01.
               10  DAGREG-01-L3.
                   15  DAGREG-01-L3-FNR    PICTURE X(3).
               10  DAGREG-01-L2.
                   15  DAGREG-01-L2-BA     PICTURE X(1).
               10  DAGREG-01-L1.
                   15  DAGREG-01-L1-BNR    PICTURE X(6).
           05  DAGREG-DATA-FIELDS.
               10  REC120                  PICTURE X(120).
               10  REC240                  PICTURE X(120).
               10  FNR                     PICTURE X(3).
               10  BA                      PICTURE X(1).
               10  BNR                     PICTURE X(6).
               10  TALL                    PICTURE X(1).
               10  BNRNUM-IO.
                   15  BNRNUM              PICTURE S9(6).
               10  BDTAA                   PICTURE X(2).
               10  BDTMM                   PICTURE X(2).
               10  BDTDD                   PICTURE X(2).
               10  NUMND-IO.
                   15  NUMND               PICTURE S9(2).
               10  KTO                     PICTURE X(8).
               10  HBKTO                   PICTURE X(4).
               10  KTO2S                   PICTURE X(2).
               10  KTO4S                   PICTURE X(4).
               10  BEL-IO.
                   15  BEL                 PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  TEGN                    PICTURE X(1).
               10  VAL-IO.
                   15  VAL                 PICTURE S9(9)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  FRAFAK                  PICTURE X(1).
               10  MKTO                    PICTURE X(8).
               10  MHBKTO                  PICTURE X(4).
               10  MKTO2S                  PICTURE X(2).
               10  MKTO4S                  PICTURE X(4).
               10  TXTSEQ                  PICTURE X(1).
               10  AK                      PICTURE X(1).
               10  SNR                     PICTURE X(3).
               10  AVD4                    PICTURE X(4).
               10  FOFDD                   PICTURE X(2).
               10  FOFMM                   PICTURE X(2).
               10  FOFAA                   PICTURE X(2).
               10  RDGR                    PICTURE X(3).
               10  HDIST                   PICTURE X(3).
               10  VT                      PICTURE X(1).
               10  BM                      PICTURE X(2).
               10  REFNR                   PICTURE X(6).
               10  AVD                     PICTURE X(1).
               10  SIGN-X                  PICTURE X(2).
               10  TEKST                   PICTURE X(24).
      * Brukes når felt er utvidet i      P 121 1272BEL
      * input program                     P 128 1354VAL
           05  BILTEXT-DATA-FIELDS.
               10  BILTXT                  PICTURE X(24).
           05  SYSPARM-DATA-FIELDS.
               10  SYSALT                  PICTURE X(1).
      *                                     142 142 SYSAVD
           05  KONTOMA-DATA-FIELDS.
               10  ALTKTO                  PICTURE X(4).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(1).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  BELNR-IO.
                   15  BELNR               PICTURE S9(9)V9(2).
               10  AREAB-IO.
                   15  AREAB               PICTURE S9(6).
               10  SYSKEY                  PICTURE X(10).
               10  SW                      PICTURE X(1).
               10  VALTYP                  PICTURE X(3).
               10  RECL1-IO.
                   15  RECL1               PICTURE S9(6).
               10  RECL2-IO.
                   15  RECL2               PICTURE S9(6).
               10  RECGT-IO.
                   15  RECGT               PICTURE S9(6).
               10  BELL1T-IO.
                   15  BELL1T              PICTURE S9(9)V9(2).
               10  BELL2T-IO.
                   15  BELL2T              PICTURE S9(9)V9(2).
               10  BELGTT-IO.
                   15  BELGTT              PICTURE S9(9)V9(2).
               10  BELL1-IO.
                   15  BELL1               PICTURE S9(9)V9(2).
               10  BELL1M-IO.
                   15  BELL1M              PICTURE S9(9)V9(2).
               10  BELL2-IO.
                   15  BELL2               PICTURE S9(9)V9(2).
               10  BELL2M-IO.
                   15  BELL2M              PICTURE S9(9)V9(2).
               10  BELGT-IO.
                   15  BELGT               PICTURE S9(9)V9(2).
               10  BELGTM-IO.
                   15  BELGTM              PICTURE S9(9)V9(2).
               10  TXTKY4                  PICTURE X(4).
               10  TXTKY7                  PICTURE X(7).
               10  TXTKEY                  PICTURE X(11).
               10  KTOKEY                  PICTURE X(7).
               10  ALTVKT                  PICTURE X(4).
               10  ALTHKT                  PICTURE X(4).
           05  EDITTING-FIELDS.
               10  XO-40YNZ                PICTURE ZZZZ.
               10  EDIT-BELL1              PICTURE ZZZ.ZZZ.ZZZ,ZZ-.
               10  EDIT-BELL1M             PICTURE ZZZ.ZZZ.ZZZ,ZZ-.
               10  EDIT-BELL1T             PICTURE ZZZ.ZZZ.ZZZ,ZZ-.
               10  EDIT-RECL1              PICTURE ZZZZZZ.
               10  EDIT-BEL                PICTURE ZZ.ZZZ.ZZZ,ZZ.
               10  EDIT-VAL                PICTURE ZZZ.ZZZ.ZZZ,ZZ.
               10  EDIT-BELNR              PICTURE ZZZ.ZZZ.ZZZ,ZZB-.
               10  EDIT-BELL2              PICTURE ZZZ.ZZZ.ZZZ,ZZ-.
               10  EDIT-BELL2M             PICTURE ZZZ.ZZZ.ZZZ,ZZ-.
               10  EDIT-BELL2T             PICTURE ZZZ.ZZZ.ZZZ,ZZ-.
               10  EDIT-RECL2              PICTURE ZZZZZZ.
               10  EDIT-BELGT              PICTURE ZZZ.ZZZ.ZZZ,ZZ-.
               10  EDIT-BELGTM             PICTURE ZZZ.ZZZ.ZZZ,ZZ-.
               10  EDIT-BELGTT             PICTURE ZZZ.ZZZ.ZZZ,ZZ-.
               10  EDIT-RECGT              PICTURE ZZZZZZ.
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
           IF  DAGREG-PROCESS
               SET DAGREG-PROCESS-OFF      TO TRUE
               SET DAGREG-READ             TO TRUE
           END-IF
 
           IF  DAGREG-READ
           AND RECORD-SELECTED-OFF
               PERFORM DAGREG-GET
               SET DAGREG-READ-OFF         TO TRUE
               IF  NOT DAGREG-EOF
                   SET DAGREG-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  DAGREG-PROCESS
               PERFORM DAGREG-IDSET
           END-IF
 
           IF  DAGREG-PROCESS
               PERFORM DAGREG-CHK-LEVEL
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
 
           IF  DAGREG-PROCESS
               PERFORM DAGREG-FLDOFF
               PERFORM DAGREG-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  DAGREG-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-16                    TO TRUE
           SET NOT-I-17                    TO TRUE
           SET NOT-I-11                    TO TRUE
           SET NOT-I-12                    TO TRUE
           SET NOT-I-13                    TO TRUE
      * ØNSKE OM Å SNU FORTEGN FRA BRUKERMØTE JUNI 1996, MEN DETTE
      * BRYTER MED BRUKEN AV FORTEGN I BILAGSJOURNALEN.
      *          BEL       MULT -1        BEL        202120
      *          VAL       MULT -1        VAL
      *  20                MOVE "-"       TEGN
      *  21                MOVE " "       TEGN
      *                    MOVE "REC120  "BUGFL1  8        DISPLAY FIELD
      *          BUGFL1    DEBUGBUGFILO   REC120           VIS INDIKATOR
           IF  (I-L1)
               SUBTRACT BELNR              FROM BELNR
               SET NOT-I-31                TO TRUE
           END-IF
           IF  (NOT-I-U1)
               SET NOT-I-10                TO TRUE
               IF  FRAFAK = 'F'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           SET NOT-I-15                    TO TRUE
           IF  NUMND = UMONTH
               SET I-15                    TO TRUE
           END-IF
           IF  (I-L2)
               GO TO A1-T
           END-IF
           SET NOT-I-16                    TO TRUE
           IF  BNRNUM NOT = AREAB
               SET I-16                    TO TRUE
           END-IF
           IF  (I-16)
               ADD 1                       TO AREAB
               SET NOT-I-17                TO TRUE
               IF  BNRNUM NOT = AREAB
                   SET I-17                TO TRUE
               END-IF
      *                    MOVE "REC240  "BUGFL1  8        DISPLAY FIELD
      *          BUGFL1    DEBUGBUGFILO   REC240           VIS INDIKATOR
           END-IF
           .
 
       A1-T.
           MOVE BNRNUM                     TO AREAB-IO
           IF  (I-L3)
               PERFORM RBSRUT-S
           END-IF
           IF  (I-86)
               GO TO SLUTT-T
           END-IF
           IF  (I-L3)
               MOVE 0                      TO PAGE0
               SET NOT-I-40                TO TRUE
               SET NOT-I-41                TO TRUE
               MOVE FNR                    TO SYSKEY (1:3)
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
      *  L3N40   SYSAVD    COMP "J"                      52
           END-IF
           IF  (I-L1)
               SET NOT-I-11                TO TRUE
               IF  BA = '0'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-11)
               GO TO A-T
           END-IF
           GO TO FORTS-T.
 
       A-T.
           SET NOT-I-12                    TO TRUE
           IF  SW = '1'
               SET I-12                    TO TRUE
           END-IF
           IF  (I-12)
               GO TO FORTS-T
           END-IF
           SET NOT-I-13                    TO TRUE
           IF  TALL > '4'
               SET I-13                    TO TRUE
           END-IF
           IF  (NOT-I-13)
               GO TO FORTS-T
           END-IF
           MOVE '1'                        TO SW
           SET I-18                        TO TRUE
           IF  (I-L1 AND I-11 AND I-18)
               PERFORM EXCEPTION-OUTPUT
           END-IF
           IF  (I-18)
               SET NOT-I-18                TO TRUE
           END-IF.
 
       FORTS-T.
           MOVE '   '                      TO VALTYP
           IF  (NOT-I-10)
               SET NOT-I-30                TO TRUE
               IF  VT > ' '
                   SET I-30                TO TRUE
               END-IF
           END-IF
           IF  (I-30)
               PERFORM VALRUT-S
           END-IF
           ADD 1                           TO RECL1
           ADD 1                           TO RECL2
           ADD 1                           TO RECGT
           IF  (I-07)
               ADD BEL                     TO BELNR
               ADD BEL                     TO BELL1T
               ADD BEL                     TO BELL2T
               ADD BEL                     TO BELGTT
           END-IF
           IF  (I-07 AND NOT-I-15)
               ADD BEL                     TO BELL1
           END-IF
           IF  (I-07 AND I-15)
               ADD BEL                     TO BELL1M
           END-IF
           IF  (I-07 AND NOT-I-15)
               ADD BEL                     TO BELL2
           END-IF
           IF  (I-07 AND I-15)
               ADD BEL                     TO BELL2M
           END-IF
           IF  (I-07 AND NOT-I-15)
               ADD BEL                     TO BELGT
           END-IF
           IF  (I-07 AND I-15)
               ADD BEL                     TO BELGTM
           END-IF
           IF  (NOT-I-07)
               SUBTRACT BEL                FROM BELNR
               SUBTRACT BEL                FROM BELL1T
               SUBTRACT BEL                FROM BELL2T
               SUBTRACT BEL                FROM BELGTT
           END-IF
           IF  (NOT-I-07 AND NOT-I-15)
               SUBTRACT BEL                FROM BELL1
           END-IF
           IF  (NOT-I-07 AND I-15)
               SUBTRACT BEL                FROM BELL1M
           END-IF
           IF  (NOT-I-07 AND NOT-I-15)
               SUBTRACT BEL                FROM BELL2
           END-IF
           IF  (NOT-I-07 AND I-15)
               SUBTRACT BEL                FROM BELL2M
           END-IF
           IF  (NOT-I-07 AND NOT-I-15)
               SUBTRACT BEL                FROM BELGT
           END-IF
           IF  (NOT-I-07 AND I-15)
               SUBTRACT BEL                FROM BELGTM
           END-IF
           SET NOT-I-28                    TO TRUE
           IF  TXTSEQ NOT < '1'
               SET I-28                    TO TRUE
           END-IF
           IF  (NOT-I-28)
               SET NOT-I-28                TO TRUE
               IF  TXTSEQ NOT > '9'
                   SET I-28                TO TRUE
               END-IF
           END-IF
           IF  (I-28 AND NOT-I-31)
               PERFORM TXTRUT-S
           END-IF
           SET NOT-I-46                    TO TRUE
           SET NOT-I-47                    TO TRUE
           IF  (I-41 AND I-43)
               OR  (I-41 AND NOT-I-42 AND NOT-I-43)
               SET I-46                    TO TRUE
           END-IF
           IF  (I-41 AND I-45)
               OR  (I-41 AND NOT-I-44 AND NOT-I-45)
               SET I-47                    TO TRUE
           END-IF
           IF  (I-41 AND I-46)
               OR  (I-41 AND I-47)
               PERFORM KTORUT-S
           END-IF.
 
       SLUTT-T.
           CONTINUE.
 
       RBSRUT-S SECTION.
       RBSRUT-S-P.
           SET NOT-I-86                    TO TRUE
           MOVE ' '                        TO BBEST
           MOVE 'REG02'                    TO LONR
           MOVE FNR                        TO LFIRMA
           MOVE '000'                      TO LUNDGR
           MOVE 'RSK023  '                 TO LPROG
           MOVE 0                          TO LANTX
           MOVE 'PRT1'                     TO LPRIID
           CALL 'RBS000' USING LDATA-XX-DATA-FIELDS
           SET NOT-I-86                    TO TRUE
           IF  LANTX = 0
               SET I-86                    TO TRUE
           END-IF.
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
      *****************************************************************
      * SUBRUTINE FOR Å HENTE TEKST TIL MOTKONTO.                     *
      *****************************************************************
 
       TXTRUT-S SECTION.
       TXTRUT-S-P.
           MOVE FNR                        TO TXTKY4 (1:3)
           MOVE BA                         TO TXTKY4 (4:1)
           MOVE BNR                        TO TXTKY7 (1:6)
           MOVE TXTSEQ                     TO TXTKY7 (7:1)
           MOVE TXTKY4                     TO TXTKEY (1:4)
           MOVE TXTKY7                     TO TXTKEY (5:7)
           MOVE TXTKEY                     TO BILTEXT-KEY1
           READ BILTEXT RECORD KEY IS BILTEXT-KEY1
           INVALID KEY
               SET I-29                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-29                TO TRUE
               PERFORM BILTEXT-FLDSET
               PERFORM BILTEXT-IDSET
           END-READ
           IF  (NOT-I-29)
               SET I-31                    TO TRUE
           END-IF.
      *****************************************************************
      * SUBRUTINE FOR Å HENTE ALTERNATIVT KONTONR FRA KONTOMA.        *
      *****************************************************************
 
       KTORUT-S SECTION.
       KTORUT-S-P.
           IF  (I-46)
               MOVE FNR                    TO KTOKEY (1:3)
               MOVE HBKTO                  TO KTOKEY (4:4)
               MOVE KTOKEY                 TO KONTOMA-KEY1
               READ KONTOMA RECORD KEY IS KONTOMA-KEY1
               INVALID KEY
                   SET I-48                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-48            TO TRUE
                   PERFORM KONTOMA-FLDSET
                   PERFORM KONTOMA-IDSET
               END-READ
           END-IF
           IF  (I-46 AND NOT-I-48)
               MOVE ALTKTO                 TO ALTVKT
           END-IF
           IF  (I-47)
               MOVE FNR                    TO KTOKEY (1:3)
               MOVE MHBKTO                 TO KTOKEY (4:4)
               MOVE KTOKEY                 TO KONTOMA-KEY1
               READ KONTOMA RECORD KEY IS KONTOMA-KEY1
               INVALID KEY
                   SET I-49                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-49            TO TRUE
                   PERFORM KONTOMA-FLDSET
                   PERFORM KONTOMA-IDSET
               END-READ
           END-IF
           IF  (I-47 AND NOT-I-49)
               MOVE ALTKTO                 TO ALTHKT
           END-IF.
      ******************************************************
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L0 AND I-86)
               ADD 1                       TO RECGT
           END-IF
           IF  (I-L0 AND I-07 AND I-86)
               ADD BEL                     TO BELGTT
           END-IF
           IF  (I-L0 AND I-07 AND NOT-I-15 AND I-86)
               ADD BEL                     TO BELGT
           END-IF
           IF  (I-L0 AND I-07 AND I-15 AND I-86)
               ADD BEL                     TO BELGTM
           END-IF
           IF  (I-L0 AND NOT-I-07 AND I-86)
               SUBTRACT BEL                FROM BELGTT
           END-IF
           IF  (I-L0 AND NOT-I-07 AND NOT-I-15 AND I-86)
               SUBTRACT BEL                FROM BELGT
           END-IF
           IF  (I-L0 AND NOT-I-07 AND I-15 AND I-86)
               SUBTRACT BEL                FROM BELGTM
      ******************************************************
      *    SUBRUTINE FOR CALL AV RBS000 COBOL SUBRUTINE.   *
      ******************************************************
           END-IF
           .
 
       DAGREG-GET SECTION.
       DAGREG-GET-P.
           IF  DAGREG-EOF-OFF
               READ DAGREG
               AT END
                   SET DAGREG-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       DAGREG-FLDOFF SECTION.
       DAGREG-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-42                TO TRUE
               SET NOT-I-43                TO TRUE
               SET NOT-I-07                TO TRUE
               SET NOT-I-44                TO TRUE
               SET NOT-I-45                TO TRUE
               SET NOT-I-51                TO TRUE
           END-EVALUATE.
 
       DAGREG-FLDSET SECTION.
       DAGREG-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE DAGREG-IO-AREA (1:120) TO REC120 (1:120)
               MOVE DAGREG-IO-AREA (121:120) TO REC240 (1:120)
               MOVE DAGREG-IO-AREA (3:3)   TO FNR (1:3)
               MOVE DAGREG-IO-AREA (6:1)   TO BA (1:1)
               MOVE DAGREG-IO-AREA (7:6)   TO BNR (1:6)
               MOVE DAGREG-IO-AREA (8:1)   TO TALL (1:1)
               MOVE DAGREG-IO-AREA (7:6)   TO BNRNUM-IO
               INSPECT BNRNUM-IO REPLACING ALL ' ' BY '0'
               MOVE DAGREG-IO-AREA (13:2)  TO BDTAA (1:2)
               MOVE DAGREG-IO-AREA (15:2)  TO BDTMM (1:2)
               MOVE DAGREG-IO-AREA (17:2)  TO BDTDD (1:2)
               MOVE DAGREG-IO-AREA (15:2)  TO NUMND-IO
               INSPECT NUMND-IO REPLACING ALL ' ' BY '0'
               MOVE DAGREG-IO-AREA (19:8)  TO KTO (1:8)
               MOVE DAGREG-IO-AREA (19:4)  TO HBKTO (1:4)
               MOVE DAGREG-IO-AREA (25:2)  TO KTO2S (1:2)
               IF  KTO2S = SPACES
                   SET I-42                TO TRUE
               END-IF
               MOVE DAGREG-IO-AREA (23:4)  TO KTO4S (1:4)
               IF  KTO4S = SPACES
                   SET I-43                TO TRUE
               END-IF
               MOVE DAGREG-IO-AREA (27:5)  TO BEL-IO
               MOVE DAGREG-IO-AREA (32:1)  TO TEGN (1:1)
               IF  TEGN = SPACES
                   SET I-07                TO TRUE
               END-IF
               MOVE DAGREG-IO-AREA (33:6)  TO VAL-IO
               MOVE DAGREG-IO-AREA (39:1)  TO FRAFAK (1:1)
               MOVE DAGREG-IO-AREA (47:8)  TO MKTO (1:8)
               MOVE DAGREG-IO-AREA (47:4)  TO MHBKTO (1:4)
               MOVE DAGREG-IO-AREA (53:2)  TO MKTO2S (1:2)
               IF  MKTO2S = SPACES
                   SET I-44                TO TRUE
               END-IF
               MOVE DAGREG-IO-AREA (51:4)  TO MKTO4S (1:4)
               IF  MKTO4S = SPACES
                   SET I-45                TO TRUE
               END-IF
               MOVE DAGREG-IO-AREA (55:1)  TO TXTSEQ (1:1)
               MOVE DAGREG-IO-AREA (56:1)  TO AK (1:1)
               MOVE DAGREG-IO-AREA (57:3)  TO SNR (1:3)
               MOVE DAGREG-IO-AREA (63:4)  TO AVD4 (1:4)
               IF  AVD4 = SPACES
                   SET I-51                TO TRUE
               END-IF
               MOVE DAGREG-IO-AREA (72:2)  TO FOFDD (1:2)
               MOVE DAGREG-IO-AREA (74:2)  TO FOFMM (1:2)
               MOVE DAGREG-IO-AREA (76:2)  TO FOFAA (1:2)
               MOVE DAGREG-IO-AREA (78:3)  TO RDGR (1:3)
               MOVE DAGREG-IO-AREA (81:3)  TO HDIST (1:3)
               MOVE DAGREG-IO-AREA (85:1)  TO VT (1:1)
               MOVE DAGREG-IO-AREA (86:2)  TO BM (1:2)
               MOVE DAGREG-IO-AREA (88:6)  TO REFNR (1:6)
               MOVE DAGREG-IO-AREA (94:1)  TO AVD (1:1)
               MOVE DAGREG-IO-AREA (95:2)  TO SIGN-X (1:2)
               MOVE DAGREG-IO-AREA (97:24) TO TEKST (1:24)
           END-EVALUATE.
 
       DAGREG-IDSET SECTION.
       DAGREG-IDSET-P.
           SET I-01                        TO TRUE.
 
       DAGREG-CHK-LEVEL SECTION.
       DAGREG-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO DAGREG-LEVEL-01
               MOVE DAGREG-IO-AREA (3:3)   TO DAGREG-01-L3-FNR
               MOVE DAGREG-IO-AREA (6:1)   TO DAGREG-01-L2-BA
               MOVE DAGREG-IO-AREA (7:6)   TO DAGREG-01-L1-BNR
               IF  DAGREG-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  DAGREG-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  DAGREG-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  DAGREG-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  DAGREG-01-L3          TO THE-PRIOR-L3
               MOVE  DAGREG-01-L2          TO THE-PRIOR-L2
               MOVE  DAGREG-01-L1          TO THE-PRIOR-L1
               SET DAGREG-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       BILTEXT-FLDSET SECTION.
       BILTEXT-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE BILTEXT-IO-AREA (12:24) TO BILTXT (1:24)
           END-EVALUATE.
 
       BILTEXT-IDSET SECTION.
       BILTEXT-IDSET-P.
           SET I-02                        TO TRUE.
 
       SYSPARM-FLDSET SECTION.
       SYSPARM-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE SYSPARM-IO-AREA (026:1) TO SYSALT (1:1)
           END-EVALUATE.
 
       SYSPARM-IDSET SECTION.
       SYSPARM-IDSET-P.
           SET I-03                        TO TRUE.
 
       KONTOMA-FLDSET SECTION.
       KONTOMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KONTOMA-IO-AREA (45:4) TO ALTKTO (1:4)
           END-EVALUATE.
 
       KONTOMA-IDSET SECTION.
       KONTOMA-IDSET-P.
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
           IF  (I-L3 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE 'D A G L I G'          TO LISTE-IO-AREA (36:11)
               MOVE 'P O S T E R I N G S'  TO LISTE-IO-AREA (50:19)
               MOVE 'L I S T E'            TO LISTE-IO-AREA (70:9)
               MOVE 'FOR  DEN'             TO LISTE-IO-AREA (84:8)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (95:8)
               MOVE 'OPPGNR=REG02  SIDE'   TO LISTE-IO-AREA (111:18)
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
               MOVE 'B'                    TO LISTE-IO-AREA (1:1)
               MOVE 'BILAGS'               TO LISTE-IO-AREA (3:6)
               MOVE 'BILAGS'               TO LISTE-IO-AREA (10:6)
               MOVE 'KTONR/'               TO LISTE-IO-AREA (17:6)
               MOVE 'D'                    TO LISTE-IO-AREA (39:1)
               MOVE 'A'                    TO LISTE-IO-AREA (64:1)
               MOVE 'FORFAL'               TO LISTE-IO-AREA (70:6)
               MOVE 'HAN'                  TO LISTE-IO-AREA (81:3)
               MOVE 'VAL'                  TO LISTE-IO-AREA (85:3)
               MOVE 'SI'                   TO LISTE-IO-AREA (104:2)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'A'                    TO LISTE-IO-AREA (1:1)
               MOVE 'NUMMER'               TO LISTE-IO-AREA (3:6)
               MOVE 'DATO'                 TO LISTE-IO-AREA (11:4)
               MOVE 'RESKNR.'              TO LISTE-IO-AREA (17:7)
               MOVE 'B E L Ø P'            TO LISTE-IO-AREA (29:9)
               MOVE 'K'                    TO LISTE-IO-AREA (39:1)
               MOVE 'VALUTA/KOST'          TO LISTE-IO-AREA (43:11)
               MOVE 'MOTKONTO'             TO LISTE-IO-AREA (55:8)
               MOVE 'K'                    TO LISTE-IO-AREA (64:1)
               MOVE 'SNR'                  TO LISTE-IO-AREA (66:3)
               MOVE 'DATO'                 TO LISTE-IO-AREA (71:4)
               MOVE 'RDG'                  TO LISTE-IO-AREA (77:3)
               MOVE 'DIS'                  TO LISTE-IO-AREA (81:3)
               MOVE 'TYP'                  TO LISTE-IO-AREA (85:3)
               MOVE 'BM'                   TO LISTE-IO-AREA (89:2)
               MOVE 'REFNR.'               TO LISTE-IO-AREA (92:6)
               MOVE 'AVD'                  TO LISTE-IO-AREA (100:3)
               MOVE 'GN'                   TO LISTE-IO-AREA (104:2)
               MOVE 'TEKST'                TO LISTE-IO-AREA (107:5)
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
           IF  (I-01 AND I-17 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '**********'           TO LISTE-IO-AREA (1:10)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-01 AND NOT-I-86 AND NOT-I-10)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE BA                     TO LISTE-IO-AREA (1:1)
               MOVE BNR                    TO LISTE-IO-AREA (3:6)
               MOVE BDTDD                  TO LISTE-IO-AREA (10:2)
               MOVE BDTMM                  TO LISTE-IO-AREA (12:2)
               MOVE BDTAA                  TO LISTE-IO-AREA (14:2)
               MOVE KTO                    TO LISTE-IO-AREA (17:8)
               IF  (I-46 AND NOT-I-48)
                   MOVE ALTVKT             TO LISTE-IO-AREA (17:4)
               END-IF
               IF  (I-46 AND I-48)
                   MOVE '????'             TO LISTE-IO-AREA (17:4)
               END-IF
               MOVE BEL                    TO EDIT-BEL
               MOVE EDIT-BEL               TO LISTE-IO-AREA (25:13)
               MOVE TEGN                   TO LISTE-IO-AREA (39:1)
               MOVE VAL                    TO EDIT-VAL
               MOVE EDIT-VAL               TO LISTE-IO-AREA (40:14)
               MOVE MKTO                   TO LISTE-IO-AREA (55:8)
               IF  (I-47 AND NOT-I-49)
                   MOVE ALTHKT             TO LISTE-IO-AREA (55:4)
               END-IF
               IF  (I-47 AND I-49)
                   MOVE '????'             TO LISTE-IO-AREA (55:4)
               END-IF
               MOVE AK                     TO LISTE-IO-AREA (64:1)
               MOVE SNR                    TO LISTE-IO-AREA (66:3)
               MOVE FOFAA                  TO LISTE-IO-AREA (70:2)
               MOVE FOFMM                  TO LISTE-IO-AREA (72:2)
               MOVE FOFDD                  TO LISTE-IO-AREA (74:2)
               MOVE RDGR                   TO LISTE-IO-AREA (77:3)
               MOVE HDIST                  TO LISTE-IO-AREA (81:3)
               MOVE VALTYP                 TO LISTE-IO-AREA (85:3)
               MOVE BM                     TO LISTE-IO-AREA (89:2)
               MOVE REFNR                  TO LISTE-IO-AREA (92:6)
               MOVE AVD                    TO LISTE-IO-AREA (101:1)
      *                52N51   AVD4     102
               IF  (NOT-I-51)
                   MOVE AVD4               TO LISTE-IO-AREA (99:4)
               END-IF
               MOVE SIGN-X                 TO LISTE-IO-AREA (104:2)
               MOVE TEKST                  TO LISTE-IO-AREA (107:24)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       DETAIL-OVERFLOW SECTION.
       DETAIL-OVERFLOW-P.
           IF  (I-OF AND NOT-I-L3 AND NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FINAVN                 TO LISTE-IO-AREA (1:30)
               MOVE 'D A G L I G'          TO LISTE-IO-AREA (36:11)
               MOVE 'P O S T E R I N G S'  TO LISTE-IO-AREA (50:19)
               MOVE 'L I S T E'            TO LISTE-IO-AREA (70:9)
               MOVE 'FOR  DEN'             TO LISTE-IO-AREA (84:8)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (95:8)
               MOVE 'OPPGNR=REG02  SIDE'   TO LISTE-IO-AREA (111:18)
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
               MOVE 'B'                    TO LISTE-IO-AREA (1:1)
               MOVE 'BILAGS'               TO LISTE-IO-AREA (3:6)
               MOVE 'BILAGS'               TO LISTE-IO-AREA (10:6)
               MOVE 'KTONR/'               TO LISTE-IO-AREA (17:6)
               MOVE 'D'                    TO LISTE-IO-AREA (39:1)
               MOVE 'A'                    TO LISTE-IO-AREA (64:1)
               MOVE 'FORFAL'               TO LISTE-IO-AREA (70:6)
               MOVE 'HAN'                  TO LISTE-IO-AREA (81:3)
               MOVE 'VAL'                  TO LISTE-IO-AREA (85:3)
               MOVE 'SI'                   TO LISTE-IO-AREA (104:2)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'A'                    TO LISTE-IO-AREA (1:1)
               MOVE 'NUMMER'               TO LISTE-IO-AREA (3:6)
               MOVE 'DATO'                 TO LISTE-IO-AREA (11:4)
               MOVE 'RESKNR.'              TO LISTE-IO-AREA (17:7)
               MOVE 'B E L Ø P'            TO LISTE-IO-AREA (29:9)
               MOVE 'K'                    TO LISTE-IO-AREA (39:1)
               MOVE 'VALUTA/KOST'          TO LISTE-IO-AREA (43:11)
               MOVE 'MOTKONTO'             TO LISTE-IO-AREA (55:8)
               MOVE 'K'                    TO LISTE-IO-AREA (64:1)
               MOVE 'SNR'                  TO LISTE-IO-AREA (66:3)
               MOVE 'DATO'                 TO LISTE-IO-AREA (71:4)
               MOVE 'RDG'                  TO LISTE-IO-AREA (77:3)
               MOVE 'DIS'                  TO LISTE-IO-AREA (81:3)
               MOVE 'TYP'                  TO LISTE-IO-AREA (85:3)
               MOVE 'BM'                   TO LISTE-IO-AREA (89:2)
               MOVE 'REFNR.'               TO LISTE-IO-AREA (92:6)
               MOVE 'AVD'                  TO LISTE-IO-AREA (100:3)
               MOVE 'GN'                   TO LISTE-IO-AREA (104:2)
               MOVE 'TEKST'                TO LISTE-IO-AREA (107:5)
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
 
       EXCEPTION-OUTPUT SECTION.
       EXCEPTION-OUTPUT-P.
           IF  (I-L1 AND I-11 AND I-18)
           AND (NOT-I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE BA                     TO LISTE-IO-AREA (1:1)
               MOVE 'SUM TIDL. MND'        TO LISTE-IO-AREA (5:13)
               MOVE BELL1                  TO EDIT-BELL1
               MOVE EDIT-BELL1             TO LISTE-IO-AREA (22:15)
               MOVE 'SUM PR. D.MND'        TO LISTE-IO-AREA (38:13)
               MOVE BELL1M                 TO EDIT-BELL1M
               MOVE EDIT-BELL1M            TO LISTE-IO-AREA (52:15)
               MOVE 'TOTALT'               TO LISTE-IO-AREA (68:6)
               MOVE BELL1T                 TO EDIT-BELL1T
               MOVE EDIT-BELL1T            TO LISTE-IO-AREA (75:15)
               MOVE 'ANT POSTERINGER'      TO LISTE-IO-AREA (91:15)
               MOVE RECL1                  TO EDIT-RECL1
               MOVE EDIT-RECL1             TO LISTE-IO-AREA (107:6)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND NOT-I-86 AND NOT-I-10)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '    SUM PR BILAGSNR ' TO LISTE-IO-AREA (1:20)
               MOVE BELNR                  TO EDIT-BELNR
               MOVE EDIT-BELNR             TO LISTE-IO-AREA (24:16)
               MOVE '                        ' TO LISTE-IO-AREA
                                                              (107:24)
               IF  (I-31)
                   MOVE BILTXT             TO LISTE-IO-AREA (107:24)
               END-IF
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L2 AND NOT-I-86)
               IF  I-OF
                   PERFORM DETAIL-OVERFLOW
                   SET NOT-I-OF            TO TRUE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE BA                     TO LISTE-IO-AREA (1:1)
               MOVE 'SUM TIDL. MND'        TO LISTE-IO-AREA (5:13)
               MOVE BELL1                  TO EDIT-BELL1
               MOVE EDIT-BELL1             TO LISTE-IO-AREA (24:15)
               INITIALIZE BELL1
               MOVE 'SUM PR. D.MND    '    TO LISTE-IO-AREA (47:17)
               MOVE BELL1M                 TO EDIT-BELL1M
               MOVE EDIT-BELL1M            TO LISTE-IO-AREA (62:15)
               INITIALIZE BELL1M
               MOVE 'TOTALT'               TO LISTE-IO-AREA (77:6)
               MOVE BELL1T                 TO EDIT-BELL1T
               MOVE EDIT-BELL1T            TO LISTE-IO-AREA (84:15)
               INITIALIZE BELL1T
               MOVE 'ANT POSTERINGER'      TO LISTE-IO-AREA (101:15)
               MOVE RECL1                  TO EDIT-RECL1
               MOVE EDIT-RECL1             TO LISTE-IO-AREA (117:6)
               INITIALIZE RECL1
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L3 AND NOT-I-86)
               IF  I-OF
                   PERFORM DETAIL-OVERFLOW
                   SET NOT-I-OF            TO TRUE
               END-IF
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FNR                    TO LISTE-IO-AREA (1:3)
               MOVE 'TOT SUM TIDL MND'     TO LISTE-IO-AREA (5:16)
               MOVE BELL2                  TO EDIT-BELL2
               MOVE EDIT-BELL2             TO LISTE-IO-AREA (24:15)
               INITIALIZE BELL2
               MOVE 'TOT SUM PR. D.MND'    TO LISTE-IO-AREA (47:17)
               MOVE BELL2M                 TO EDIT-BELL2M
               MOVE EDIT-BELL2M            TO LISTE-IO-AREA (62:15)
               INITIALIZE BELL2M
               MOVE 'TOTALT'               TO LISTE-IO-AREA (77:6)
               MOVE BELL2T                 TO EDIT-BELL2T
               MOVE EDIT-BELL2T            TO LISTE-IO-AREA (84:15)
               INITIALIZE BELL2T
               MOVE 'ANT POSTERINGER'      TO LISTE-IO-AREA (101:15)
               MOVE RECL2                  TO EDIT-RECL2
               MOVE EDIT-RECL2             TO LISTE-IO-AREA (117:6)
               INITIALIZE RECL2
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'SUM TIDL. MND'        TO LISTE-IO-AREA (5:13)
               MOVE BELGT                  TO EDIT-BELGT
               MOVE EDIT-BELGT             TO LISTE-IO-AREA (24:15)
               INITIALIZE BELGT
               MOVE 'SUM PR. D.MND    '    TO LISTE-IO-AREA (40:17)
               MOVE BELGTM                 TO EDIT-BELGTM
               MOVE EDIT-BELGTM            TO LISTE-IO-AREA (58:15)
               INITIALIZE BELGTM
               MOVE 'TOTALT'               TO LISTE-IO-AREA (74:6)
               MOVE BELGTT                 TO EDIT-BELGTT
               MOVE EDIT-BELGTT            TO LISTE-IO-AREA (81:15)
               INITIALIZE BELGTT
               MOVE 'ANT POSTERINGER'      TO LISTE-IO-AREA (98:15)
               MOVE RECGT                  TO EDIT-RECGT
               MOVE EDIT-RECGT             TO LISTE-IO-AREA (114:6)
               INITIALIZE RECGT
      *****************************************************************
      * DUMMY-LINJE FOR Å LAGE REF. TIL DATAFELT (FJERNE FEILMELDING) *
      *****************************************************************
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-U8 AND I-98 AND I-02)
           OR  (I-03 AND I-04)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE LONR                   TO LISTE-IO-AREA (126:5)
               MOVE LFIRMA                 TO LISTE-IO-AREA (128:3)
               MOVE LOPNVN                 TO LISTE-IO-AREA (96:35)
               MOVE LUNDGR                 TO LISTE-IO-AREA (128:3)
               MOVE LPROG                  TO LISTE-IO-AREA (123:8)
               MOVE LPRIID                 TO LISTE-IO-AREA (127:4)
               MOVE BBEST                  TO LISTE-IO-AREA (130:1)
               MOVE PSDS                   TO LISTE-IO-AREA (51:80)
               MOVE R                      TO LISTE-IO-AREA (123:8)
               MOVE P-IO                   TO LISTE-IO-AREA (128:3)
               MOVE S-IO                   TO LISTE-IO-AREA (126:5)
               MOVE BJOBN                  TO LISTE-IO-AREA (123:8)
               MOVE BBEST                  TO LISTE-IO-AREA (130:1)
               MOVE BPERS                  TO LISTE-IO-AREA (101:30)
               MOVE BETTB                  TO LISTE-IO-AREA (91:40)
               MOVE BFORS                  TO LISTE-IO-AREA (91:40)
               MOVE BMEMO                  TO LISTE-IO-AREA (91:40)
               MOVE BANTX-IO               TO LISTE-IO-AREA (128:3)
               MOVE AKSEKD                 TO LISTE-IO-AREA (130:1)
               MOVE ADVK                   TO LISTE-IO-AREA (130:1)
               MOVE NORVN                  TO LISTE-IO-AREA (111:20)
               MOVE STDVN                  TO LISTE-IO-AREA (111:20)
               MOVE LANDKD                 TO LISTE-IO-AREA (129:2)
               MOVE LAND                   TO LISTE-IO-AREA (111:20)
               MOVE VALIX-IO               TO LISTE-IO-AREA (128:3)
               MOVE DIV                    TO LISTE-IO-AREA (121:10)
               MOVE BPCLAS                 TO LISTE-IO-AREA (130:1)
               MOVE BPRJE                  TO LISTE-IO-AREA (128:3)
               MOVE REC120                 TO LISTE-IO-AREA (11:120)
               MOVE REC240                 TO LISTE-IO-AREA (11:120)
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
           SET DAGREG-LEVEL-INIT           TO TRUE
           INITIALIZE DAGREG-DATA-FIELDS
           SET DAGREG-EOF-OFF              TO TRUE
           SET DAGREG-PROCESS              TO TRUE
           OPEN INPUT DAGREG
           INITIALIZE BILTEXT-DATA-FIELDS
           OPEN INPUT BILTEXT
           INITIALIZE SYSPARM-DATA-FIELDS
           OPEN INPUT SYSPARM
           INITIALIZE KONTOMA-DATA-FIELDS
           OPEN INPUT KONTOMA
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE DAGREG
           CLOSE BILTEXT
           CLOSE SYSPARM
           CLOSE KONTOMA
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
