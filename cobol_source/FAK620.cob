       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK620R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAM: FAK620  DANNE EFAKTURA FILE TIL POSTEN.              *
      *          UPSI 1 PRINTER  KONTROLLISTE.                        *
      *          UPSI 2 TESTFILE TIL POSTEN.                          *
      *          UPSI 3 ADGANGSTEST TIL POSTEN (SJEKK KUNDENR/PASSORD)*
      *          UPSI 4 = PRINT ARKIVKOPI.                            *
      *          UPSI 8 = IKKE OPPDATERE SISTE GIROKID I FIRMAF (TEST)*
      *          FORMULARNUMMER:                                      *
      *               50000 = FAKTURA MED GIRO.                       *
      *               50002 = FAKTURA UTEN  GIRO.                     *
      *               50004 = BETALINGSDOKUMENT.                      *
      *               50002 = KREDITNOTA.                             *
      *  30/08-01 PROGRAMMERT AV ESPEN LARSEN                         *
      *  10/05-02 Fakturakid skal også fremkomme på kr.nota.          *
      *                      ref. Egil Tjønneland, Hafnor as.         *
      *  10/01-03 Rettet feil ved nedtelling av antall linjer i rest. *
      *           Feltet RSTLIN hadde 3 pos. rettet til 6.            *
      *  19/11-04 LAGT INN TEST PÅ FIRMA SOM IKKE SKAL HA GIROKID.    *
      *           FAKTURAKID BLIR TILDELT I PROG: FAK600.             *
      *  07/06-05 OMREGNER OG LEGGER UT BELØP I UTENLANDS VALUTA.     *
      *           EURO BLIR LAGT UT MED 4 DESIMALER.                  *
      *           DETTE ER IKKE TESTET (7.6.2005)                     *
      *  03/07-13 Fjernet test på fnr.622 - ikke kid.                 *
      *  21/01-16 lagt på tekst "pr.mnd." i "Ved for sen .."
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK620.rpg
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
           SELECT FAKTAB
               ASSIGN TO UT-S-FAKTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKTAB-STATUS.
           SELECT FMAST
               ASSIGN TO UT-S-FMAST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FMAST-STATUS.
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
           SELECT KUNDEMX
               ASSIGN TO KUNDEMX
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMX-STATUS
               RECORD KEY IS KUNDEMX-KEY1.
           SELECT STATTAB
               ASSIGN TO STATTAB
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS STATTAB-STATUS
               RECORD KEY IS STATTAB-KEY1.
           SELECT EFAKT
               ASSIGN TO UT-S-EFAKT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS EFAKT-STATUS.
           SELECT PRF
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PRF-STATUS.
           SELECT KIDRELF
               ASSIGN TO UT-S-KIDRELF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KIDRELF-STATUS.
           SELECT FBKOPI
               ASSIGN TO UT-S-FBKOPI
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FBKOPI-STATUS.
           SELECT FORDRE
               ASSIGN TO UT-S-FORDRE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FORDRE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD FAKTAB
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  FAKTAB-IO-AREA.
           05  FAKTAB-IO-AREA-X            PICTURE X(80).
       FD FMAST
               BLOCK CONTAINS 300
               RECORD CONTAINS 150.
       01  FMAST-IO-AREA.
           05  FMAST-IO-AREA-X             PICTURE X(150).
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
       FD KUNDEMX
               RECORD CONTAINS 200.
       01  KUNDEMX-IO-AREA.
           05  KUNDEMX-IO-AREA-X.
               10  KUNDEMX-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       FD STATTAB
               RECORD CONTAINS 40.
       01  STATTAB-IO-AREA.
           05  STATTAB-IO-AREA-X.
               10  STATTAB-KEY1            PICTURE X(8).
               10  FILLER                  PICTURE X(32).
       FD EFAKT
               BLOCK CONTAINS 256
               RECORD CONTAINS 128.
       01  EFAKT-IO-AREA.
           05  EFAKT-IO-AREA-X             PICTURE X(128).
       FD PRF
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  PRF-IO-PRINT.
           05  PRF-IO-AREA-CONTROL         PICTURE X VALUE ' '.
        02 PRF-IO-AREA.
           05  PRF-IO-AREA-X               PICTURE X(132).
       FD KIDRELF
               BLOCK CONTAINS 160
               RECORD CONTAINS 80.
       01  KIDRELF-IO-AREA.
           05  KIDRELF-IO-AREA-X           PICTURE X(80).
       FD FBKOPI
               BLOCK CONTAINS 300
               RECORD CONTAINS 150.
       01  FBKOPI-IO-AREA.
           05  FBKOPI-IO-AREA-X            PICTURE X(150).
       FD FORDRE
               BLOCK CONTAINS 160
               RECORD CONTAINS 80.
       01  FORDRE-IO-AREA.
           05  FORDRE-IO-AREA-X            PICTURE X(80).
       WORKING-STORAGE SECTION.
       77  TABFNR-MAX   VALUE 500          PICTURE 9(4) USAGE BINARY.
       77  TABGRF-MAX   VALUE 500          PICTURE 9(4) USAGE BINARY.
       77  AR1-MAX   VALUE 500             PICTURE 9(4) USAGE BINARY.
       77  AR2-MAX   VALUE 500             PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  TABFNR-TABLE.
               10  TABFNR-ENTRY
                                           OCCURS 500 TIMES
                                           INDEXED BY TABFNR-I
                                                      TABFNR-S
                                                      TABGRF-I
                                                      TABGRF-S.
                   15  TABFNR              PICTURE X(3).
                   15  TABGRF              PICTURE X(8).
           05  AR1-TABLE.
               10  AR1-ENTRY
                                           OCCURS 500 TIMES
                                           INDEXED BY AR1-I
                                                      AR1-S.
                   15  AR1                 PICTURE X(6).
           05  AR2-TABLE.
               10  AR2-ENTRY
                                           OCCURS 500 TIMES
                                           INDEXED BY AR2-I
                                                      AR2-S.
                   15  AR2                 PICTURE S9(8)V9(2).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  FAKTAB-STATUS               PICTURE 99 VALUE 0.
           10  FMAST-STATUS                PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  KUNDEMX-STATUS              PICTURE 99 VALUE 0.
           10  STATTAB-STATUS              PICTURE 99 VALUE 0.
           10  EFAKT-STATUS                PICTURE 99 VALUE 0.
           10  PRF-STATUS                  PICTURE 99 VALUE 0.
           10  KIDRELF-STATUS              PICTURE 99 VALUE 0.
           10  FBKOPI-STATUS               PICTURE 99 VALUE 0.
           10  FORDRE-STATUS               PICTURE 99 VALUE 0.
           10  MODFLT-XX-STATUS            PICTURE 99 VALUE 0.
           10  KIFELT-XX-STATUS            PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKTAB-EOF-OFF          VALUE '0'.
               88  FAKTAB-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FMAST-EOF-OFF           VALUE '0'.
               88  FMAST-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FMAST-READ-OFF          VALUE '0'.
               88  FMAST-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FMAST-PROCESS-OFF       VALUE '0'.
               88  FMAST-PROCESS           VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  FMAST-LEVEL-INIT-OFF    VALUE '0'.
               88  FMAST-LEVEL-INIT        VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  KUNDEMX-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  STATTAB-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  PRF-DATA-FIELDS.
               10  PRF-AFTER-SPACE         PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRF-AFTER-SKIP          PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRF-BEFORE-SPACE        PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRF-BEFORE-SKIP         PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRF-MAX-LINES           PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRF-LINE-COUNT          PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRF-CLR-IO              PICTURE X VALUE 'Y'.
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
           05  MODFLT-XX-DATA-FIELDS.
               10  MODBEL-IO.
                   15  MODBEL              PICTURE S9(9).
               10  FILLER                  PICTURE X(18).
           05  FILLER REDEFINES MODFLT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(9).
               10  MODSIF-IO.
                   15  MODSIF              PICTURE S9(1).
               10  FILLER                  PICTURE X(17).
           05  KIFELT-XX REDEFINES MODFLT-XX-DATA-FIELDS.
               10  KITALL                  PICTURE X(25).
               10  FILLER                  PICTURE X(2).
           05  FILLER REDEFINES MODFLT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(25).
               10  KITYPE                  PICTURE X(1).
               10  KIKTR                   PICTURE X(1).
      *DSDS: DATA STRUCTURE FIELDS
           05  KIFELT-XX-DATA-FIELDS.
               10  KITALL                  PICTURE X(25).
               10  FILLER                  PICTURE X(2).
           05  FILLER REDEFINES KIFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(25).
               10  KITYPE                  PICTURE X(1).
               10  KIKTR                   PICTURE X(1).
           05  FMAST-LEVEL-01.
               10  FMAST-01-L4.
                   15  FMAST-01-L4-FNR     PICTURE X(3).
               10  FMAST-01-L3.
                   15  FMAST-01-L3-KNR     PICTURE X(6).
               10  FMAST-01-L2.
                   15  FMAST-01-L2-BM      PICTURE X(2).
               10  FMAST-01-L1.
                   15  FMAST-01-L1-FAKNR   PICTURE X(6).
           05  FMAST-LEVEL-02.
               10  FMAST-02-L4.
                   15  FMAST-02-L4-FNR     PICTURE X(3).
               10  FMAST-02-L3.
                   15  FMAST-02-L3-KNR     PICTURE X(6).
               10  FMAST-02-L2.
                   15  FMAST-02-L2-BM      PICTURE X(2).
               10  FMAST-02-L1.
                   15  FMAST-02-L1-FAKNR   PICTURE X(6).
           05  FMAST-LEVEL-03.
               10  FMAST-03-L4.
                   15  FMAST-03-L4-FNR     PICTURE X(3).
               10  FMAST-03-L3.
                   15  FMAST-03-L3-KNR     PICTURE X(6).
               10  FMAST-03-L2.
                   15  FMAST-03-L2-BM      PICTURE X(2).
               10  FMAST-03-L1.
                   15  FMAST-03-L1-FAKNR   PICTURE X(6).
           05  FMAST-LEVEL-04.
               10  FMAST-04-L4.
                   15  FMAST-04-L4-FNR     PICTURE X(3).
               10  FMAST-04-L3.
                   15  FMAST-04-L3-KNR     PICTURE X(6).
               10  FMAST-04-L2.
                   15  FMAST-04-L2-BM      PICTURE X(2).
               10  FMAST-04-L1.
                   15  FMAST-04-L1-FAKNR   PICTURE X(6).
           05  FMAST-DATA-FIELDS.
               10  FNR                     PICTURE X(3).
               10  KNR                     PICTURE X(6).
               10  FAKNR                   PICTURE X(6).
               10  ANTLIN-IO.
                   15  ANTLIN              PICTURE S9(6).
               10  ANTSID-IO.
                   15  ANTSID              PICTURE S9(3).
               10  FAKDAG                  PICTURE X(2).
               10  FAKMND                  PICTURE X(2).
               10  FAKAAR                  PICTURE X(4).
               10  FFDAG                   PICTURE X(2).
               10  FFMND                   PICTURE X(2).
               10  FFAAR                   PICTURE X(4).
               10  FFAAR2                  PICTURE X(2).
               10  BETBET                  PICTURE X(24).
               10  FAKKID                  PICTURE X(7).
               10  GIRO                    PICTURE X(1).
               10  FILTYP                  PICTURE X(1).
               10  NTOBEL-IO.
                   15  NTOBEL              PICTURE S9(9)V9(2).
               10  NTOBEZ-IO.
                   15  NTOBEZ              PICTURE S9(11).
               10  NTOBE1                  PICTURE X(1).
               10  MVABEL-IO.
                   15  MVABEL              PICTURE S9(9)V9(2).
               10  MVABEZ-IO.
                   15  MVABEZ              PICTURE S9(11).
               10  MVABE1                  PICTURE X(1).
               10  AVRBEL-IO.
                   15  AVRBEL              PICTURE S9(3)V9(2).
               10  AVRBEZ-IO.
                   15  AVRBEZ              PICTURE S9(5).
               10  AVRBE1                  PICTURE X(1).
               10  FAKBEL-IO.
                   15  FAKBEL              PICTURE S9(9)V9(2).
               10  FAKBEZ-IO.
                   15  FAKBEZ              PICTURE S9(11).
               10  FAKBE1                  PICTURE X(1).
               10  FAKB90-IO.
                   15  FAKB90              PICTURE S9(9).
               10  FAKKR-IO.
                   15  FAKKR               PICTURE S9(9).
               10  FAKORE-IO.
                   15  FAKORE              PICTURE S9(2).
               10  MVATYP                  PICTURE X(1).
               10  FRPRO-IO.
                   15  FRPRO               PICTURE S9(2)V9(2).
               10  GIROES                  PICTURE X(1).
               10  KSALDO-IO.
                   15  KSALDO              PICTURE S9(9)V9(2).
               10  KSALDZ-IO.
                   15  KSALDZ              PICTURE S9(11).
               10  KSALD1                  PICTURE X(1).
               10  FAKKNR-IO.
                   15  FAKKNR              PICTURE S9(3).
               10  FPRTYP                  PICTURE X(1).
               10  BM                      PICTURE X(2).
               10  KRGIRO                  PICTURE X(1).
               10  REC01                   PICTURE X(150).
               10  KADR1                   PICTURE X(30).
               10  KADR2                   PICTURE X(30).
               10  KADR3                   PICTURE X(30).
               10  PNR                     PICTURE X(4).
               10  PSTED                   PICTURE X(15).
               10  REC02                   PICTURE X(150).
               10  VADR1                   PICTURE X(30).
               10  VADR2                   PICTURE X(30).
               10  VADR3                   PICTURE X(30).
               10  VADR4                   PICTURE X(19).
               10  REC03                   PICTURE X(150).
               10  LINTYP                  PICTURE X(2).
               10  ANTLEV-IO.
                   15  ANTLEV              PICTURE S9(5)V9(2).
               10  TEKST1                  PICTURE X(20).
               10  TEKST2                  PICTURE X(30).
               10  TEKSTL                  PICTURE X(50).
               10  ENHPRI-IO.
                   15  ENHPRI              PICTURE S9(7)V9(2).
               10  ENHPRZ-IO.
                   15  ENHPRZ              PICTURE S9(9).
               10  ENHPR1                  PICTURE X(1).
               10  RAB1-IO.
                   15  RAB1                PICTURE S9(2)V9(1).
               10  RAB2-IO.
                   15  RAB2                PICTURE S9(2)V9(1).
               10  RAB3-IO.
                   15  RAB3                PICTURE S9(2)V9(1).
               10  NTOIND                  PICTURE X(1).
               10  NTOPRI-IO.
                   15  NTOPRI              PICTURE S9(7)V9(2).
               10  NTOPRZ-IO.
                   15  NTOPRZ              PICTURE S9(9).
               10  NTOPR1                  PICTURE X(1).
               10  REC04                   PICTURE X(150).
           05  FIRMAF-DATA-FIELDS.
               10  KONFNR                  PICTURE X(3).
               10  FINAVN                  PICTURE X(30).
               10  FIADR                   PICTURE X(30).
               10  FIPNR                   PICTURE X(4).
               10  FIPST                   PICTURE X(26).
               10  GIROD1                  PICTURE X(4).
               10  GIROD2                  PICTURE X(2).
               10  GIROD3                  PICTURE X(5).
               10  FAKTPT                  PICTURE X(1).
               10  FAKTBK                  PICTURE X(1).
               10  FBDKID-IO.
                   15  FBDKID              PICTURE S9(6).
           05  KUNDEMA-DATA-FIELDS.
               10  BANK1                   PICTURE X(4).
               10  BANK2                   PICTURE X(2).
               10  BANK3                   PICTURE X(5).
           05  KUNDEMX-DATA-FIELDS.
               10  VALTYP                  PICTURE X(3).
           05  STATTAB-DATA-FIELDS.
               10  VKURS2-IO.
                   15  VKURS2              PICTURE S9(4)V9(4).
               10  AV100                   PICTURE X(1).
      *****************************************************************
      * HOVEDRUTINE SKAL DETTE FIRMA HA EBREV FAKTURA.                *
      *****************************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L4            PICTURE X(3).
               10  THE-PRIOR-L3            PICTURE X(6).
               10  THE-PRIOR-L2            PICTURE X(2).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  XFOOT-TOTAL                 PICTURE S9(9)V9(9).
           05  TEMPORARY-FIELDS.
               10  ANTFIR-IO.
                   15  ANTFIR              PICTURE S9(3).
               10  FNRKNR                  PICTURE X(9).
               10  FNRNAV                  PICTURE X(30).
               10  FIRMNR                  PICTURE X(3).
               10  SBDKID-IO.
                   15  SBDKID              PICTURE S9(6).
               10  ANTFIF-IO.
                   15  ANTFIF              PICTURE S9(3).
               10  FFDAGX                  PICTURE X(2).
               10  FFMNDX                  PICTURE X(2).
               10  FFAARX                  PICTURE X(4).
               10  FFAARY                  PICTURE X(2).
               10  RECNR-IO.
                   15  RECNR               PICTURE S9(4).
               10  SIDENR-IO.
                   15  SIDENR              PICTURE S9(3).
               10  SIDEL3-IO.
                   15  SIDEL3              PICTURE S9(3).
               10  ANTGL4-IO.
                   15  ANTGL4              PICTURE S9(6).
               10  ANTPM-IO.
                   15  ANTPM               PICTURE S9(5).
               10  ANTEHF-IO.
                   15  ANTEHF              PICTURE S9(5).
               10  ANTSK3-IO.
                   15  ANTSK3              PICTURE S9(6).
               10  ANTSKT-IO.
                   15  ANTSKT              PICTURE S9(6).
               10  ANTSA3-IO.
                   15  ANTSA3              PICTURE S9(6).
               10  ANTSAT-IO.
                   15  ANTSAT              PICTURE S9(6).
               10  RSTLIN-IO.
                   15  RSTLIN              PICTURE S9(6).
               10  FRMTYP                  PICTURE X(5).
               10  X-IO.
                   15  X                   PICTURE S9(3).
               10  MAXANT-IO.
                   15  MAXANT              PICTURE S9(3).
               10  MAXKNR                  PICTURE X(6).
               10  MAXLIN-IO.
                   15  MAXLIN              PICTURE S9(6).
               10  MAXSID-IO.
                   15  MAXSID              PICTURE S9(3).
               10  MAXKNL                  PICTURE X(6).
               10  MAXFAK                  PICTURE X(6).
               10  ANTFA3-IO.
                   15  ANTFA3              PICTURE S9(6).
               10  ANTFAT-IO.
                   15  ANTFAT              PICTURE S9(6).
               10  ANTKA3-IO.
                   15  ANTKA3              PICTURE S9(6).
               10  ANTKAT-IO.
                   15  ANTKAT              PICTURE S9(6).
               10  ANTTA3-IO.
                   15  ANTTA3              PICTURE S9(5).
               10  ANTTAT-IO.
                   15  ANTTAT              PICTURE S9(6).
               10  ANTST3-IO.
                   15  ANTST3              PICTURE S9(6).
               10  ANTSS3-IO.
                   15  ANTSS3              PICTURE S9(6).
               10  ANTSTT-IO.
                   15  ANTSTT              PICTURE S9(6).
               10  ANTSST-IO.
                   15  ANTSST              PICTURE S9(6).
               10  ANTFB3-IO.
                   15  ANTFB3              PICTURE S9(9)V9(2).
               10  ANTFBT-IO.
                   15  ANTFBT              PICTURE S9(9)V9(2).
               10  ANTKB3-IO.
                   15  ANTKB3              PICTURE S9(9)V9(2).
               10  ANTKBT-IO.
                   15  ANTKBT              PICTURE S9(9)V9(2).
               10  ANTTB3-IO.
                   15  ANTTB3              PICTURE S9(9)V9(2).
               10  ANTTBT-IO.
                   15  ANTTBT              PICTURE S9(9)V9(2).
               10  ANTVLS-IO.
                   15  ANTVLS              PICTURE S9(3).
               10  VPRISW-IO.
                   15  VPRISW              PICTURE S9(9)V9(2).
               10  ENHPRE-IO.
                   15  ENHPRE              PICTURE S9(5)V9(4).
               10  NTOPRE-IO.
                   15  NTOPRE              PICTURE S9(5)V9(4).
               10  NTOBEV-IO.
                   15  NTOBEV              PICTURE S9(9)V9(2).
               10  NTOBEE-IO.
                   15  NTOBEE              PICTURE S9(7)V9(4).
               10  MVABEV-IO.
                   15  MVABEV              PICTURE S9(9)V9(2).
               10  MVABEE-IO.
                   15  MVABEE              PICTURE S9(7)V9(4).
               10  AVRBEV-IO.
                   15  AVRBEV              PICTURE S9(3)V9(2).
               10  AVRBEE-IO.
                   15  AVRBEE              PICTURE S9(1)V9(4).
               10  FAKBEV-IO.
                   15  FAKBEV              PICTURE S9(9)V9(2).
               10  FAKBEE-IO.
                   15  FAKBEE              PICTURE S9(7)V9(4).
               10  KSALDV-IO.
                   15  KSALDV              PICTURE S9(9)V9(2).
               10  KSALDE-IO.
                   15  KSALDE              PICTURE S9(7)V9(4).
               10  ANTGLR-IO.
                   15  ANTGLR              PICTURE S9(6).
               10  BDLIN-IO.
                   15  BDLIN               PICTURE S9(3).
               10  AVSIDE-IO.
                   15  AVSIDE              PICTURE S9(3).
               10  SIDANT-IO.
                   15  SIDANT              PICTURE S9(3).
               10  SSIDE-IO.
                   15  SSIDE               PICTURE S9(3).
               10  ANTBD3-IO.
                   15  ANTBD3              PICTURE S9(6).
               10  ANTBS3-IO.
                   15  ANTBS3              PICTURE S9(6).
               10  ANTBDT-IO.
                   15  ANTBDT              PICTURE S9(6).
               10  ANTBST-IO.
                   15  ANTBST              PICTURE S9(6).
               10  X2-IO.
                   15  X2                  PICTURE S9(3).
               10  SGSUM-IO.
                   15  SGSUM               PICTURE S9(8)V9(2).
               10  RELBEL-IO.
                   15  RELBEL              PICTURE S9(7)V9(2).
               10  OVK8L4-IO.
                   15  OVK8L4              PICTURE S9(5).
               10  OVK8LR-IO.
                   15  OVK8LR              PICTURE S9(6).
               10  AS02L4-IO.
                   15  AS02L4              PICTURE S9(5).
               10  AS02LR-IO.
                   15  AS02LR              PICTURE S9(6).
               10  AS05L4-IO.
                   15  AS05L4              PICTURE S9(5).
               10  AS05LR-IO.
                   15  AS05LR              PICTURE S9(6).
               10  AS10L4-IO.
                   15  AS10L4              PICTURE S9(5).
               10  AS10LR-IO.
                   15  AS10LR              PICTURE S9(6).
               10  AS35L4-IO.
                   15  AS35L4              PICTURE S9(5).
               10  AS35LR-IO.
                   15  AS35LR              PICTURE S9(6).
               10  AS1TL4-IO.
                   15  AS1TL4              PICTURE S9(5).
               10  AS1TLR-IO.
                   15  AS1TLR              PICTURE S9(6).
               10  AS2TL4-IO.
                   15  AS2TL4              PICTURE S9(5).
               10  AS2TLR-IO.
                   15  AS2TLR              PICTURE S9(6).
               10  BELKID-IO.
                   15  BELKID              PICTURE S9(1).
               10  F6                      PICTURE X(6).
               10  F7                      PICTURE X(7).
               10  GIRKID                  PICTURE X(7).
               10  KXKEY1                  PICTURE X(9).
               10  KXKEY2                  PICTURE X(10).
               10  STKEY1                  PICTURE X(5).
               10  STKEY2                  PICTURE X(8).
               10  VPRISX-IO.
                   15  VPRISX              PICTURE S9(7)V9(4).
               10  VPRISE-IO.
                   15  VPRISE              PICTURE S9(7)V9(4).
           05  EDITTING-FIELDS.
               10  XO-30YNZ                PICTURE ZZZ.
               10  XO-52YN9                PICTURE ZZZZZ,99.
               10  XO-72YN9                PICTURE ZZZZZZZ,99.
               10  XO-54YN9                PICTURE ZZZZZ,9999.
               10  XO-21YNZ                PICTURE ZZ,Z.
               10  XO-72YN9R               PICTURE ZZZZZZZ,99-.
               10  XO-54YN9R               PICTURE ZZZZZ,9999-.
               10  XO-92YN9R               PICTURE ZZZZZZZZZ,99-.
               10  XO-32YN9R               PICTURE ZZZ,99-.
               10  XO-74YN9R               PICTURE ZZZZZZZ,9999-.
               10  XO-14YN9R               PICTURE Z,9999-.
               10  XO-22YN9                PICTURE ZZ,99.
               10  XO-90YN9                PICTURE ZZZZZZZZ9.
               10  XO-82YN9R               PICTURE ZZZZZZZZ,99-.
               10  XO-82YN9                PICTURE ZZZZZZZZ,99.
               10  EDIT-NTOBEZ             PICTURE Z9999999999.
               10  EDIT-MVABEZ             PICTURE Z9999999999.
               10  EDIT-AVRBEZ             PICTURE Z9999.
               10  EDIT-FAKBEZ             PICTURE Z9999999999.
               10  EDIT-KSALDZ             PICTURE Z9999999999.
               10  EDIT-ENHPRZ             PICTURE Z99999999.
               10  EDIT-NTOPRZ             PICTURE Z99999999.
               10  XO-60YY9                PICTURE ZZZ.ZZ9.
               10  XO-92YY9R               PICTURE ZZZ.ZZZ.ZZZ,99-.
               10  XO-50YY9                PICTURE ZZ.ZZ9.
               10  XO-30YY9                PICTURE ZZ9.
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
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-04                    TO TRUE
           SET NOT-I-05                    TO TRUE
           SET NOT-I-05                    TO TRUE
           SET NOT-I-05                    TO TRUE
           SET NOT-I-05                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  FMAST-PROCESS
               SET FMAST-PROCESS-OFF       TO TRUE
               SET FMAST-READ              TO TRUE
           END-IF
 
           IF  FMAST-READ
           AND RECORD-SELECTED-OFF
               PERFORM FMAST-GET
               SET FMAST-READ-OFF          TO TRUE
               IF  NOT FMAST-EOF
                   PERFORM FMAST-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET FMAST-PROCESS       TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  FMAST-PROCESS
               PERFORM FMAST-IDSET
           END-IF
 
           IF  FMAST-PROCESS
               PERFORM FMAST-CHK-LEVEL
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
 
           IF  FMAST-PROCESS
               PERFORM FMAST-FLDOFF
               PERFORM FMAST-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  FMAST-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-L4)
               SET NOT-I-45                TO TRUE
               SET NOT-I-46                TO TRUE
               SET NOT-I-58                TO TRUE
               SET NOT-I-61                TO TRUE
           END-IF
           IF  (I-L1)
               SET NOT-I-25                TO TRUE
               SET NOT-I-31                TO TRUE
           END-IF
           IF  (I-L4)
               SET NOT-I-29                TO TRUE
               SET TABFNR-S                TO TABFNR-I
               PERFORM WITH TEST AFTER
                       VARYING TABFNR-I FROM 1 BY 1
                         UNTIL TABFNR-I >= TABFNR-MAX
                            OR I-29
                   IF  FNR = TABFNR (TABFNR-I)
                       SET I-29            TO TRUE
                       SET TABFNR-S        TO TABFNR-I
                   END-IF
               END-PERFORM
               SET TABFNR-I                TO TABFNR-S
               IF  I-29
               AND TABFNR-I NOT > TABGRF-MAX
                   SET TABGRF-I            TO TABFNR-I
               END-IF
           END-IF
           IF  (NOT-I-29)
               GO TO SLUTT-T
           END-IF
           IF  (I-L4)
               ADD 1                       TO ANTFIR
               MOVE FNR                    TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-09                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-09            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
      *  **** frak 1.1.18 skal all post være a-post RETTES 2.1.18
      *  L4                SETON                         45 = A-POST
      *  **** frak 1.1.18 skal all post være a-post                     000170
           END-IF
           IF  (I-L4)
               SET NOT-I-45                TO TRUE
               IF  FAKTPT = 'A'
                   SET I-45                TO TRUE
               END-IF
               SET NOT-I-46                TO TRUE
               IF  FAKTBK = 'J'
                   SET I-46                TO TRUE
               END-IF
               SET NOT-I-58                TO TRUE
               IF  FNR = '581'
                   SET I-58                TO TRUE
               END-IF
           END-IF
           IF  (I-L4 AND NOT-I-58)
               SET NOT-I-58                TO TRUE
               IF  FNR = '653'
                   SET I-58                TO TRUE
               END-IF
           END-IF
           IF  (I-L4 AND NOT-I-58)
               SET NOT-I-58                TO TRUE
               IF  FNR = '557'
                   SET I-58                TO TRUE
               END-IF
           END-IF
           IF  (I-L4)
               SET NOT-I-17                TO TRUE
               IF  FNR = '950'
                   SET I-17                TO TRUE
               END-IF
               SET NOT-I-61                TO TRUE
               IF  FNR = '855'
                   SET I-61                TO TRUE
               END-IF
           END-IF
           IF  (I-L4 AND NOT-I-61)
               SET NOT-I-61                TO TRUE
               IF  FNR = '015'
                   SET I-61                TO TRUE
               END-IF
           END-IF
           IF  (I-L3)
               SET NOT-I-63                TO TRUE
               SET NOT-I-64                TO TRUE
               SET NOT-I-65                TO TRUE
           END-IF
           IF  (I-L3 AND I-61)
               PERFORM VALR01-S
           END-IF
           IF  (I-L2)
               SET NOT-I-36                TO TRUE
               IF  BM = '11'
                   SET I-36                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-36)
               SET NOT-I-36                TO TRUE
               IF  BM = '12'
                   SET I-36                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-36)
               MOVE FNR                    TO FNRKNR (1:3)
               MOVE KNR                    TO FNRKNR (4:6)
               MOVE FNRKNR                 TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-37                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-37            TO TRUE
                   PERFORM KUNDEMA-FLDSET
                   PERFORM KUNDEMA-IDSET
               END-READ
           END-IF
           IF  (I-09)
               GO TO SLUTT-T
           END-IF
           IF  (I-L4)
               SET NOT-I-80                TO TRUE
               IF  KONFNR > '000'
                   SET I-80                TO TRUE
               END-IF
               MOVE FINAVN                 TO FNRNAV
           END-IF
           IF  (I-L4 AND I-80)
               MOVE KONFNR                 TO FIRMNR
           END-IF
           IF  (I-L4 AND NOT-I-80)
               MOVE FNR                    TO FIRMNR
           END-IF
           IF  (I-L4)
               MOVE FIRMNR                 TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-09                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-09            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
           END-IF
           IF  (I-L4 AND NOT-I-09)
               ADD FBDKID TO ZERO      GIVING SBDKID
               SET NOT-I-18                TO TRUE
               IF  SBDKID = 0
                   SET I-18                TO TRUE
               END-IF
           END-IF
           IF  (I-L4 AND NOT-I-09 AND I-18)
               MOVE 010000                 TO SBDKID-IO
           END-IF
           IF  (I-L4)
               ADD 1                       TO ANTFIF
           END-IF
           IF  (I-L2)
               MOVE FFDAG                  TO FFDAGX
               MOVE FFMND                  TO FFMNDX
               MOVE FFAAR                  TO FFAARX
               MOVE FFAAR2                 TO FFAARY
      *****************************************************************
      * HOVEDRUTINE.                                                  *
      *****************************************************************
           END-IF
           SET NOT-I-21                    TO TRUE
           IF  (I-L4 AND NOT-I-20)
               SET I-21                    TO TRUE
               SET I-20                    TO TRUE
           END-IF
           SET NOT-I-22                    TO TRUE
           IF  (I-L1)
               SET NOT-I-93                TO TRUE
               SET NOT-I-40                TO TRUE
               MOVE 0                      TO ANTVLS
               MOVE 0                      TO RECNR
           END-IF
           ADD 1                           TO RECNR
           IF  (I-L1)
               SET I-23                    TO TRUE
           END-IF
           IF  (I-03)
               SET I-93                    TO TRUE
           END-IF
           IF  (I-04 AND I-23)
               SET I-22                    TO TRUE
           END-IF
           IF  (I-04)
               SET NOT-I-23                TO TRUE
           END-IF
           IF  (I-L1)
               MOVE 1                      TO SIDENR
               SET NOT-I-19                TO TRUE
           END-IF
           IF  (I-L3)
               SET I-19                    TO TRUE
               SUBTRACT SIDEL3             FROM SIDEL3
           END-IF
           IF  (I-L4)
               SUBTRACT AS02L4             FROM AS02L4
               SUBTRACT AS05L4             FROM AS05L4
               SUBTRACT AS10L4             FROM AS10L4
               SUBTRACT AS35L4             FROM AS35L4
               SUBTRACT AS1TL4             FROM AS1TL4
               SUBTRACT AS2TL4             FROM AS2TL4
               SUBTRACT ANTGL4             FROM ANTGL4
      ***************************************************************** 000225
      * RUTINE FOR Å TESTE PÅ FAKTURA PRINT MÅTE.                     *
      *****************************************************************
           END-IF
           IF  (I-L1)
               SET NOT-I-31                TO TRUE
               IF  FPRTYP = 'U'
                   SET I-31                TO TRUE
               END-IF
               SET I-25                    TO TRUE
               SET NOT-I-59                TO TRUE
               IF  FPRTYP = 'M'
                   SET I-59                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-59)
               SET NOT-I-59                TO TRUE
               IF  FPRTYP = 'A'
                   SET I-59                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-59)
               SET NOT-I-59                TO TRUE
               IF  FPRTYP = 'E'
                   SET I-59                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-59)
               ADD 1                       TO ANTPM
               MOVE 'U'                    TO FPRTYP
           END-IF
           IF  (I-L1)
               SET NOT-I-59                TO TRUE
               IF  FPRTYP = 'B'
                   SET I-59                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-59)
               ADD 1                       TO ANTEHF
               MOVE 'U'                    TO FPRTYP
           END-IF
           IF  (I-L1)
               SET NOT-I-32                TO TRUE
               IF  FPRTYP = '2'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-32)
               SET NOT-I-32                TO TRUE
               IF  FPRTYP = 'Y'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-L1)
               SET NOT-I-33                TO TRUE
               IF  FPRTYP = '3'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  FPRTYP = 'Z'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-L1)
               SET NOT-I-34                TO TRUE
               IF  FPRTYP = 'X'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  FPRTYP = 'Y'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  FPRTYP = 'Z'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  FPRTYP = 'U'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-34)
               ADD 1                       TO ANTSK3
               ADD 1                       TO ANTSKT
           END-IF
           IF  (I-L3 AND I-34 AND NOT-I-31)
               ADD 1                       TO ANTSA3
               ADD 1                       TO ANTSAT
      *****************************************************************
      * SETTE FORMULARTYPE: FAKT M/GIRO. FAKT U/GIRO.                 *
      *                     1 PR. FAKTURA.                            *
      *****************************************************************
           END-IF
           IF  (I-01)
               SET NOT-I-39                TO TRUE
               SET NOT-I-24                TO TRUE
               IF  FAKNR > '899999'
                   SET I-24                TO TRUE
               END-IF
               SET NOT-I-26                TO TRUE
               IF  GIRO = 'N'
                   SET I-26                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-26)
               SET NOT-I-26                TO TRUE
               IF  GIRO = 'S'
                   SET I-26                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-26)
               SET NOT-I-26                TO TRUE
               IF  GIROES = 'J'
                   SET I-26                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-26 AND I-36)
               SET I-26                    TO TRUE
           END-IF
           IF  (I-01 AND I-64)
               SET I-26                    TO TRUE
           END-IF
           IF  (I-01 AND NOT-I-24 AND NOT-I-26)
               SET I-39                    TO TRUE
           END-IF
           IF  (I-01 AND I-39)
               ADD ANTLIN TO ZERO      GIVING RSTLIN
               MOVE '50000'                TO FRMTYP
           END-IF
           IF  (I-01 AND NOT-I-39 AND NOT-I-24)
               MOVE '50002'                TO FRMTYP
           END-IF
           IF  (I-01 AND NOT-I-39 AND I-24)
               MOVE '50002'                TO FRMTYP
      *****************************************************************
      * RUTINE FOR OPPSAMLING TIL UTSKRIFT AV BETALINGSDOKUMENT       *
      * NÅR DET ER FLERE FAKTURA/KR.NOTA PR. KUNDE/BM OG DET SKAL     *
      * LAGES GIRO.                                                   *
      *****************************************************************
           END-IF
           IF  (I-L2)
               SET NOT-I-28                TO TRUE
               MOVE 0                      TO X
               SET NOT-I-35                TO TRUE
               IF  GIRO = 'N'
                   SET I-35                TO TRUE
               END-IF
               SET NOT-I-26                TO TRUE
               IF  FAKKNR > 1
                   SET I-26                TO TRUE
               END-IF
               SET NOT-I-27                TO TRUE
               IF  GIROES = 'J'
                   SET I-27                TO TRUE
               END-IF
           END-IF
           IF  (I-01)
               SET NOT-I-43                TO TRUE
               IF  KRGIRO = 'N'
                   SET I-43                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-26 AND NOT-I-27)
               AND (NOT-I-36)
               GO TO SGIROX-T
           END-IF
           IF  (I-01 AND I-35 AND NOT-I-36)
               GO TO SGIROX-T
           END-IF
           IF  (I-01 AND I-43)
               GO TO SGIROX-T
           END-IF
           IF  (I-01 AND I-64)
               GO TO SGIROX-T
           END-IF
           IF  (I-L2)
               PERFORM VARYING AR1-I FROM 1 BY 1
                         UNTIL AR1-I > AR1-MAX
                   MOVE '      '           TO AR1 (AR1-I)
               END-PERFORM
               SET I-28                    TO TRUE
               SET NOT-I-26                TO TRUE
               IF  FAKKNR > MAXANT
                   SET I-26                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-26)
               ADD FAKKNR TO ZERO      GIVING MAXANT
               MOVE KNR                    TO MAXKNR
      *
           END-IF
           IF  (I-01)
               ADD 1                       TO X
               MOVE FAKNR                  TO AR1 (X)
           END-IF
           IF  (I-01 AND NOT-I-24)
               ADD FAKBEL TO ZERO      GIVING AR2 (X)
           END-IF
           IF  (I-01 AND I-24)
               SUBTRACT FAKBEL FROM ZERO GIVING AR2 (X)
           END-IF.
 
       SGIROX-T.
      *****************************************************************
      * TEST OM NUMERISK FELT ER SATT TIL MINUS.                      *
      *****************************************************************
           IF  (I-01)
               SET NOT-I-51                TO TRUE
               IF  NTOBEZ < 0
                   SET I-51                TO TRUE
               END-IF
               SET NOT-I-52                TO TRUE
               IF  MVABEZ < 0
                   SET I-52                TO TRUE
               END-IF
               SET NOT-I-53                TO TRUE
               IF  AVRBEZ < 0
                   SET I-53                TO TRUE
               END-IF
               SET NOT-I-54                TO TRUE
               IF  FAKBEZ < 0
                   SET I-54                TO TRUE
               END-IF
               SET NOT-I-55                TO TRUE
               IF  KSALDZ < 0
                   SET I-55                TO TRUE
               END-IF
           END-IF
           IF  (I-04)
               SET NOT-I-56                TO TRUE
               IF  ENHPRZ < 0
                   SET I-56                TO TRUE
               END-IF
               SET NOT-I-57                TO TRUE
               IF  NTOPRZ < 0
                   SET I-57                TO TRUE
               END-IF
      *****************************************************************
      * SKAL GIRO PRINTES PÅ DENNE FAKTURA ?                          *
      *****************************************************************
           END-IF
           IF  (I-01)
               SET NOT-I-30                TO TRUE
               SET NOT-I-30                TO TRUE
               IF  FRMTYP = '50000'
                   SET I-30                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-30)
               MOVE FAKB90                 TO MODBEL-IO
               PERFORM SRMODB-S
      *****************************************************************
      * SAVE OG PRINT UT INFORMASJON OM STØRSTE FAKTURA.              *
      *****************************************************************
           END-IF
           IF  (I-01)
               SET NOT-I-26                TO TRUE
               IF  ANTLIN > MAXLIN
                   SET I-26                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-26)
               ADD ANTLIN TO ZERO      GIVING MAXLIN
               ADD ANTSID TO ZERO      GIVING MAXSID
               MOVE KNR                    TO MAXKNL
               MOVE FAKNR                  TO MAXFAK
      *****************************************************************
      * RUTINE FOR OG TELLING AV ANTALL                               *
      *****************************************************************
           END-IF
           IF  (I-01 AND NOT-I-24)
               ADD 1                       TO ANTFA3
               ADD 1                       TO ANTFAT
           END-IF
           IF  (I-01 AND I-24)
               ADD 1                       TO ANTKA3
               ADD 1                       TO ANTKAT
           END-IF
           IF  (I-01)
               ADD 1                       TO ANTTA3
               ADD 1                       TO ANTTAT
               ADD ANTSID                  TO ANTST3
           END-IF
           IF  (I-01 AND NOT-I-34)
               ADD ANTSID                  TO ANTSS3
               ADD ANTSID                  TO SIDEL3
           END-IF
           IF  (I-01)
               ADD ANTSID                  TO ANTSTT
           END-IF
           IF  (I-01 AND NOT-I-34)
               ADD ANTSID                  TO ANTSST
           END-IF
           IF  (I-01 AND I-32)
               ADD ANTSID                  TO ANTST3
               ADD ANTSID                  TO ANTSTT
           END-IF
           IF  (I-01 AND I-33)
               ADD ANTSID                  TO ANTST3
               ADD ANTSID                  TO ANTSTT
               ADD ANTSID                  TO ANTST3
               ADD ANTSID                  TO ANTSTT
           END-IF
           IF  (I-01 AND I-32 AND NOT-I-34)
               ADD ANTSID                  TO ANTSS3
               ADD ANTSID                  TO ANTSST
           END-IF
           IF  (I-01 AND I-33 AND NOT-I-34)
               ADD ANTSID                  TO ANTSS3
               ADD ANTSID                  TO ANTSST
               ADD ANTSID                  TO ANTSS3
               ADD ANTSID                  TO ANTSST
           END-IF
           IF  (I-01 AND NOT-I-24)
               ADD FAKBEL                  TO ANTFB3
               ADD FAKBEL                  TO ANTFBT
           END-IF
           IF  (I-01 AND I-24)
               SUBTRACT FAKBEL             FROM ANTKB3
               SUBTRACT FAKBEL             FROM ANTKBT
           END-IF
           IF  (I-01 AND NOT-I-24)
               ADD FAKBEL                  TO ANTTB3
               ADD FAKBEL                  TO ANTTBT
           END-IF
           IF  (I-01 AND I-24)
               SUBTRACT FAKBEL             FROM ANTTB3
               SUBTRACT FAKBEL             FROM ANTTBT
      *
           END-IF
           IF  (I-04)
               ADD 1                       TO ANTVLS
               SET NOT-I-40                TO TRUE
               IF  ANTVLS > 41
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND I-40)
               ADD 1                       TO SIDENR
               MOVE 1                      TO ANTVLS
           END-IF
           IF  (I-04 AND I-39)
               SUBTRACT 1                  FROM RSTLIN
           END-IF
           IF  (I-04)
               SET NOT-I-41                TO TRUE
               IF  LINTYP = '21'
                   SET I-41                TO TRUE
               END-IF
               SET NOT-I-42                TO TRUE
               IF  LINTYP = '22'
                   SET I-42                TO TRUE
               END-IF
      *  04      LINTYP    COMP "23"                     43 KUNDEREF.LINje
           END-IF
           IF  (I-04)
               SET NOT-I-44                TO TRUE
               IF  NTOIND = 'N'
                   SET I-44                TO TRUE
               END-IF
      ****** FINN ENHPRI I UTL.VALUTA  ********************************
           END-IF
           IF  (I-04 AND I-41 AND I-64)
               ADD ENHPRI TO ZERO      GIVING VPRISW
               PERFORM VALR02-S
           END-IF
           IF  (I-04 AND I-41 AND I-64)
               AND (NOT-I-65)
               ADD VPRISW TO ZERO      GIVING ENHPRI
               MULTIPLY 100 BY ENHPRI  GIVING ENHPRZ
           END-IF
           IF  (I-04 AND I-41 AND I-64)
               AND (I-65)
               ADD VPRISE TO ZERO      GIVING ENHPRE
               MULTIPLY 10000 BY ENHPRE GIVING ENHPRZ
           END-IF
           IF  (I-04 AND I-41 AND I-64)
               ADD NTOPRI TO ZERO      GIVING VPRISW
               PERFORM VALR02-S
           END-IF
           IF  (I-04 AND I-41 AND I-64)
               AND (NOT-I-65)
               ADD VPRISW TO ZERO      GIVING NTOPRI
               MULTIPLY 100 BY NTOPRI  GIVING NTOPRZ
           END-IF
           IF  (I-04 AND I-41 AND I-64)
               AND (I-65)
               ADD VPRISE TO ZERO      GIVING NTOPRE
           END-IF
           IF  (I-04 AND I-41 AND I-64)
               MULTIPLY 10000 BY NTOPRE GIVING NTOPRZ
           END-IF
           IF  (I-25)
               SET NOT-I-38                TO TRUE
           END-IF
           IF  (I-25 AND I-40 AND I-39)
               OR  (I-25 AND I-02 AND I-39)
               SET NOT-I-38                TO TRUE
               IF  RSTLIN > 17
                   SET I-38                TO TRUE
               END-IF
           END-IF.
 
       SLUTT-T.
      *****************************************************************
      * RUTINE FOR SKRIVE UT fakturatotal og forsinkelsesrente.       *
      *   Dette må gjøres via EXCPT p.g.a. EXCPT i L2.                *
      *****************************************************************
           CONTINUE.
 
       SRMODB-S SECTION.
       SRMODB-S-P.
           MOVE 0                          TO MODSIF-IO
           CALL 'MOD10ISO' USING MODFLT-XX-DATA-FIELDS
           MOVE MODSIF                     TO BELKID-IO.
      *****************************************************************
      * BEREGNE MODULUS 10 KONTROLLSIFFER FOR FAKTURA BETALINGSDOK.   *
      *****************************************************************
 
       SRMODG-S SECTION.
       SRMODG-S-P.
           MOVE SBDKID                     TO F6
      ** MLLzo
           MOVE '1'                        TO BW-A-2
           MULTIPLY 16                     BY BW-A
           MOVE F6 (6:1)                   TO BW-B-2
           MULTIPLY 16                     BY BW-B
           MOVE BW-A-1                     TO BW-B-1
           DIVIDE 16                       INTO BW-B
           MOVE BW-B-2                     TO F6 (6:1)
           MOVE F6                         TO F7 (1:6)
           MOVE '0'                        TO F7 (7:1)
           MOVE F7                         TO KITALL (1:7)
           MOVE 'T'                        TO KITYPE
           MOVE ' '                        TO KIKTR
           CALL 'MODULUS' USING KIFELT-XX-DATA-FIELDS
           MOVE F6                         TO GIRKID (1:6)
           MOVE KIKTR                      TO GIRKID (7:1).
      *****************************************************************
      * SUBRUTINE FOR Å SJEKKE OM KUNDEN SKAL HA BELØP I VALUTA,      *
      *           OG OM DENNE VALUTATYPE ER INNMELDT I STATTAB.       *
      *****************************************************************
 
       VALR01-S SECTION.
       VALR01-S-P.
           MOVE FNR                        TO KXKEY1 (1:3)
           MOVE KNR                        TO KXKEY1 (4:6)
           MOVE KXKEY1                     TO KXKEY2 (1:9)
           MOVE '2'                        TO KXKEY2 (10:1)
           MOVE KXKEY2                     TO KUNDEMX-KEY1
           READ KUNDEMX RECORD KEY IS KUNDEMX-KEY1
           INVALID KEY
               SET I-62                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-62                TO TRUE
               PERFORM KUNDEMX-FLDSET
               PERFORM KUNDEMX-IDSET
           END-READ
           IF  (NOT-I-62)
               SET NOT-I-63                TO TRUE
               IF  VALTYP > '   '
                   SET I-63                TO TRUE
               END-IF
           END-IF
           IF  (I-63)
               MOVE FNR                    TO STKEY1 (1:3)
               MOVE '06'                   TO STKEY1 (4:2)
               MOVE STKEY1                 TO STKEY2 (1:5)
               MOVE VALTYP                 TO STKEY2 (6:3)
               MOVE STKEY2                 TO STATTAB-KEY1
               READ STATTAB RECORD KEY IS STATTAB-KEY1
               INVALID KEY
                   SET I-62                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-62            TO TRUE
                   PERFORM STATTAB-FLDSET
                   PERFORM STATTAB-IDSET
               END-READ
           END-IF
           IF  (I-63 AND NOT-I-62)
               SET NOT-I-64                TO TRUE
               IF  VKURS2 > 0,0000
                   SET I-64                TO TRUE
               END-IF
           END-IF
           SET NOT-I-65                    TO TRUE
           IF  VALTYP = 'EUR'
               SET I-65                    TO TRUE
           END-IF.
      *****************************************************************
      * SUBRUTINE FOR Å OMREGNE PRIS I NOK TIL VALUTAPRIS.            *
      * KOLBERG ØNSKER IKKE OMREGNING PÅ EURO, DA DETTE ER TASTET INN.*
      *****************************************************************
 
       VALR02-S SECTION.
       VALR02-S-P.
      *          FAKNR     COMP "900000"             69     =KR.NOTA
      *          AV100     COMP "J"                      62 =KURS PR.100
      *  62      VPRISW    MULT 100       WBEL    92
      * N62      VPRISW    MULT 1         WBEL
      *          WBEL      DIV  VKURS2    VPRISX 114
           IF  (I-61 AND I-65)
               ADD VPRISW TO ZERO      GIVING VPRISX
      *  61 65 69VPRISW    MULT 100       WBEL    92       TILB.F. FEIL.
      *  61 65 69WBEL      DIV  9,00      VPRISX 114       TILB.F. FEIL.
           END-IF
           IF  (NOT-I-65)
               ADD VPRISX TO ZERO      GIVING VPRISW
           END-IF
           IF  (I-65)
               ADD VPRISX TO ZERO      GIVING VPRISE
           END-IF.
      *****************************************************************
      * OPPDRAGSHODE/FILEHEADER EPL1.                                 *
      *   P0S.20: A=A-POST, B=B-POST.                                 *
      *****************************************************************
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1 AND NOT-I-29)
               GO TO ENDL1A-T
           END-IF
           IF  (I-L1 AND NOT-I-25)
               GO TO ENDL1A-T
      ****** FINN NTOBEL I UTL.VALUTA  ********************************
           END-IF
           IF  (I-L1 AND I-64)
               ADD NTOBEL TO ZERO      GIVING VPRISW
               PERFORM VALR02-S
           END-IF
           IF  (I-L1 AND I-64 AND NOT-I-65)
               ADD VPRISW TO ZERO      GIVING NTOBEV
               MULTIPLY 100 BY NTOBEV  GIVING NTOBEZ
           END-IF
           IF  (I-L1 AND I-64 AND I-65)
               ADD VPRISE TO ZERO      GIVING NTOBEE
               MULTIPLY 10000 BY NTOBEE GIVING NTOBEZ
      ****** FINN MVABEL I UTL.VALUTA  ********************************
           END-IF
           IF  (I-L1 AND I-64)
               ADD MVABEL TO ZERO      GIVING VPRISW
               PERFORM VALR02-S
           END-IF
           IF  (I-L1 AND I-64 AND NOT-I-65)
               ADD VPRISW TO ZERO      GIVING MVABEV
               MULTIPLY 100 BY MVABEV  GIVING MVABEZ
           END-IF
           IF  (I-L1 AND I-64 AND I-65)
               ADD VPRISE TO ZERO      GIVING MVABEE
               MULTIPLY 10000 BY MVABEE GIVING MVABEZ
      ****** FINN AVRBEL I UTL.VALUTA  ********************************
           END-IF
           IF  (I-L1 AND I-64)
               ADD AVRBEL TO ZERO      GIVING VPRISW
               PERFORM VALR02-S
           END-IF
           IF  (I-L1 AND I-64 AND NOT-I-65)
               ADD VPRISW TO ZERO      GIVING AVRBEV
               MULTIPLY 100 BY AVRBEV  GIVING AVRBEZ
           END-IF
           IF  (I-L1 AND I-64 AND I-65)
               ADD VPRISE TO ZERO      GIVING AVRBEE
               MULTIPLY 10000 BY AVRBEE GIVING AVRBEZ
      ****** FINN FAKBEL I UTL.VALUTA  ********************************
           END-IF
           IF  (I-L1 AND I-64)
               ADD FAKBEL TO ZERO      GIVING VPRISW
               PERFORM VALR02-S
           END-IF
           IF  (I-L1 AND I-64 AND NOT-I-65)
               ADD VPRISW TO ZERO      GIVING FAKBEV
               MULTIPLY 100 BY FAKBEV  GIVING FAKBEZ
           END-IF
           IF  (I-L1 AND I-64 AND I-65)
               ADD VPRISE TO ZERO      GIVING FAKBEE
               MULTIPLY 10000 BY FAKBEE GIVING FAKBEZ
      ****** FINN KJØP I ÅR I UTL.VALUTA  **************************
           END-IF
           IF  (I-L1 AND I-64)
               ADD KSALDO TO ZERO      GIVING VPRISW
               PERFORM VALR02-S
           END-IF
           IF  (I-L1 AND I-64 AND NOT-I-65)
               ADD VPRISW TO ZERO      GIVING KSALDV
               MULTIPLY 100 BY KSALDV  GIVING KSALDZ
           END-IF
           IF  (I-L1 AND I-64 AND I-65)
               ADD VPRISE TO ZERO      GIVING KSALDE
               MULTIPLY 10000 BY KSALDE GIVING KSALDZ
           END-IF
           IF  (I-L1)
               SET I-90                    TO TRUE
               PERFORM EXCEPTION-OUTPUT
               SET NOT-I-90                TO TRUE
           END-IF
           IF  (I-L1 AND I-25 AND I-30)
               ADD 1                       TO ANTGL4
               ADD 1                       TO ANTGLR
           END-IF.
 
       ENDL1A-T.
      *****************************************************************
      * RUTINE FOR SKRIVE UT BETALINGSDOKUMENT MED UNDERLIGGENDE GIRO *
      * SAMT BEREGNE TOTALT GIROBELØP.                                *
      *****************************************************************
           IF  (I-L1 AND NOT-I-29)
               GO TO ENDL2A-T
           END-IF
           IF  (I-L1 AND NOT-I-25)
               GO TO ENDL2A-T
           END-IF
           IF  (I-L1 AND NOT-I-28)
               GO TO ENDL2A-T
      *****************************************************************
      * BETALINGSDOK. START OG HEADING.                               *
      *****************************************************************
           END-IF
           IF  (I-L2)
               ADD 1                       TO SBDKID
               SET NOT-I-18                TO TRUE
               IF  SBDKID NOT < 060000
                   SET I-18                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-18)
               MOVE 010000                 TO SBDKID-IO
           END-IF
           IF  (I-L2)
               PERFORM SRMODG-S
           END-IF
           IF  (I-L2)
               ADD X TO ZERO           GIVING BDLIN
               MOVE 1                      TO SIDENR
           END-IF
           IF  (I-L2 AND NOT-I-34)
               ADD 1                       TO SIDEL3
           END-IF
           IF  (I-L2)
               MOVE 0                      TO AVSIDE
               DIVIDE BDLIN BY 17      GIVING AVSIDE
               MULTIPLY AVSIDE BY 17   GIVING SIDANT
               SUBTRACT SIDANT FROM BDLIN GIVING SSIDE
               SET NOT-I-89                TO TRUE
               IF  SSIDE > 0
                   SET I-89                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-89)
               ADD 1                       TO AVSIDE
           END-IF
           IF  (I-L2)
               MOVE '50004'                TO FRMTYP
      *  88                MOVE "50002"   FRMTYP            U/GIRO
           END-IF
           IF  (I-L2)
               ADD AVSIDE                  TO ANTST3
           END-IF
           IF  (I-L2 AND NOT-I-34)
               ADD AVSIDE                  TO ANTSS3
           END-IF
           IF  (I-L2)
               ADD AVSIDE                  TO ANTSTT
           END-IF
           IF  (I-L2 AND NOT-I-34)
               ADD AVSIDE                  TO ANTSST
           END-IF
           IF  (I-L2)
               ADD AVSIDE                  TO ANTBD3
           END-IF
           IF  (I-L2 AND NOT-I-34)
               ADD AVSIDE                  TO ANTBS3
           END-IF
           IF  (I-L2)
               ADD AVSIDE                  TO ANTBDT
           END-IF
           IF  (I-L2 AND NOT-I-34)
               ADD AVSIDE                  TO ANTBST
           END-IF
           IF  (I-L2)
               SET I-91                    TO TRUE
               PERFORM EXCEPTION-OUTPUT
               SET NOT-I-91                TO TRUE
               MOVE 0                      TO X
               MOVE 0                      TO X2
      *
      *****************************************************************
      * BETALINGSDOK. SPESIFIKASJON + NY HEADING OM > 17 LINJER       *
      *****************************************************************
           END-IF
           IF  (I-L2)
               SET I-96                    TO TRUE
               MOVE 0                      TO XFOOT-TOTAL
               PERFORM WITH TEST AFTER
                       VARYING AR2-I FROM 1 BY 1
                         UNTIL AR2-I >= AR2-MAX
                   ADD AR2 (AR2-I)         TO XFOOT-TOTAL
               END-PERFORM
               MOVE XFOOT-TOTAL            TO SGSUM
               SET NOT-I-87                TO TRUE
               SET NOT-I-86                TO TRUE
               IF  SGSUM > 0
                   SET I-87                TO TRUE
               END-IF
               IF  SGSUM < 0
                   SET I-86                TO TRUE
               END-IF
           END-IF.
 
       LOOPA-T.
           IF  (I-L2)
               ADD 1                       TO X
               SET NOT-I-92                TO TRUE
               IF  X > BDLIN
                   SET I-92                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-92)
               GO TO SGSUMR-T
           END-IF
           IF  (I-L2)
               SET NOT-I-99                TO TRUE
               IF  AR1 (X) > '899999'
                   SET I-99                TO TRUE
               END-IF
               ADD AR2 (X) TO ZERO     GIVING RELBEL
               ADD 1                       TO X2
               SET NOT-I-88                TO TRUE
               IF  X2 > 17
                   SET I-88                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-88)
               SET I-91                    TO TRUE
               SET I-96                    TO TRUE
               ADD 1                       TO SIDENR
           END-IF
           IF  (I-L2 AND I-88 AND NOT-I-34)
               ADD 1                       TO SIDEL3
           END-IF
           IF  (I-L2 AND I-88)
               MOVE 1                      TO X2
           END-IF
           IF  (I-L2)
               SET I-94                    TO TRUE
               ADD 1                       TO RECNR
               PERFORM EXCEPTION-OUTPUT
               SET NOT-I-94                TO TRUE
               SET NOT-I-96                TO TRUE
               SET NOT-I-91                TO TRUE
               MOVE 0,00                   TO AR2 (X)
               GO TO LOOPA-T
      *
      *****************************************************************
      * BETALINGSDOK. GIRO TIL SLUTT.                                 *
      *****************************************************************
           END-IF
           .
 
       SGSUMR-T.
           IF  (I-L2)
               MOVE SGSUM (2:9)            TO MODBEL-IO
               PERFORM SRMODB-S
           END-IF
           IF  (I-L2)
               SET I-95                    TO TRUE
           END-IF
           IF  (I-L2 AND I-17 AND I-86)
               SET NOT-I-98                TO TRUE
               IF  X2 < 13
                   SET I-98                TO TRUE
               END-IF
           END-IF
           IF  (I-L2)
               PERFORM EXCEPTION-OUTPUT
               ADD 1                       TO ANTGL4
               ADD 1                       TO ANTGLR
               SET NOT-I-95                TO TRUE
               SET NOT-I-98                TO TRUE
           END-IF.
 
       ENDL2A-T.
      *****************************************************************
      * totalrutine for telling av brev pr. portosats.                *
      *****************************************************************
           IF  (I-L3 AND NOT-I-29)
               GO TO ENDL3A-T
           END-IF
           IF  (I-L3)
               SET NOT-I-82                TO TRUE
               IF  SIDEL3 > 8
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND I-82)
               ADD 1                       TO OVK8L4
               ADD 1                       TO OVK8LR
           END-IF
           IF  (I-L3 AND I-34)
               GO TO XGIROP-T
           END-IF
           IF  (I-L3)
               SET NOT-I-82                TO TRUE
               IF  SIDEL3 = 1
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  SIDEL3 = 2
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND I-82)
               ADD 1                       TO AS02L4
               ADD 1                       TO AS02LR
               GO TO XGIROP-T
           END-IF
           IF  (I-L3)
               SET NOT-I-82                TO TRUE
               IF  SIDEL3 NOT > 7
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND I-82)
               ADD 1                       TO AS05L4
               ADD 1                       TO AS05LR
               GO TO XGIROP-T
           END-IF
           IF  (I-L3)
               SET NOT-I-82                TO TRUE
               IF  SIDEL3 NOT > 17
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND I-82)
               ADD 1                       TO AS10L4
               ADD 1                       TO AS10LR
               GO TO XGIROP-T
           END-IF
           IF  (I-L3)
               SET NOT-I-82                TO TRUE
               IF  SIDEL3 NOT > 56
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND I-82)
               ADD 1                       TO AS35L4
               ADD 1                       TO AS35LR
               GO TO XGIROP-T
           END-IF
           IF  (I-L3)
               SET NOT-I-82                TO TRUE
               IF  SIDEL3 NOT > 165
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND I-82)
               ADD 1                       TO AS1TL4
               ADD 1                       TO AS1TLR
               GO TO XGIROP-T
           END-IF
           IF  (I-L3)
               ADD 1                       TO AS2TL4
               ADD 1                       TO AS2TLR
           END-IF.
 
       XGIROP-T.
           CONTINUE.
 
       ENDL3A-T.
      *****************************************************************
      * TOTALSUMMER.                                                  *
      *****************************************************************
      *****************************************************************
      * Subrutine for utregning av modulussiffer beløp.               *
      *           MODULUS 10.                                         *
      *****************************************************************
           CONTINUE.
 
       FMAST-GET SECTION.
       FMAST-GET-P.
           IF  FMAST-EOF-OFF
               READ FMAST
               AT END
                   SET FMAST-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FMAST-FLDOFF SECTION.
       FMAST-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( FMAST-IO-AREA (22:1) = '0' )
               SET NOT-I-15                TO TRUE
           WHEN ( FMAST-IO-AREA (22:1) = '1'
            AND   FMAST-IO-AREA (23:1) = '2' )
               SET NOT-I-10                TO TRUE
               SET NOT-I-11                TO TRUE
               SET NOT-I-12                TO TRUE
               SET NOT-I-13                TO TRUE
           END-EVALUATE.
 
       FMAST-FLDSET SECTION.
       FMAST-FLDSET-P.
           EVALUATE TRUE
           WHEN ( FMAST-IO-AREA (22:1) = '0' )
               MOVE FMAST-IO-AREA (1:3)    TO FNR (1:3)
               MOVE FMAST-IO-AREA (4:6)    TO KNR (1:6)
               MOVE FMAST-IO-AREA (10:6)   TO FAKNR (1:6)
               MOVE FMAST-IO-AREA (24:6)   TO ANTLIN-IO
               INSPECT ANTLIN-IO REPLACING ALL ' ' BY '0'
               MOVE FMAST-IO-AREA (30:3)   TO ANTSID-IO
               INSPECT ANTSID-IO REPLACING ALL ' ' BY '0'
               MOVE FMAST-IO-AREA (33:2)   TO FAKDAG (1:2)
               MOVE FMAST-IO-AREA (35:2)   TO FAKMND (1:2)
               MOVE FMAST-IO-AREA (37:4)   TO FAKAAR (1:4)
               MOVE FMAST-IO-AREA (41:2)   TO FFDAG (1:2)
               MOVE FMAST-IO-AREA (43:2)   TO FFMND (1:2)
               MOVE FMAST-IO-AREA (45:4)   TO FFAAR (1:4)
               MOVE FMAST-IO-AREA (47:2)   TO FFAAR2 (1:2)
               MOVE FMAST-IO-AREA (49:24)  TO BETBET (1:24)
               MOVE FMAST-IO-AREA (73:7)   TO FAKKID (1:7)
               MOVE FMAST-IO-AREA (80:1)   TO GIRO (1:1)
               MOVE FMAST-IO-AREA (81:1)   TO FILTYP (1:1)
               MOVE FMAST-IO-AREA (82:11)  TO NTOBEL-IO
               INSPECT NTOBEL-IO REPLACING ALL ' ' BY '0'
               MOVE FMAST-IO-AREA (82:11)  TO NTOBEZ-IO
               INSPECT NTOBEZ-IO REPLACING ALL ' ' BY '0'
               MOVE FMAST-IO-AREA (82:1)   TO NTOBE1 (1:1)
               MOVE FMAST-IO-AREA (93:11)  TO MVABEL-IO
               INSPECT MVABEL-IO REPLACING ALL ' ' BY '0'
               MOVE FMAST-IO-AREA (93:11)  TO MVABEZ-IO
               INSPECT MVABEZ-IO REPLACING ALL ' ' BY '0'
               MOVE FMAST-IO-AREA (93:1)   TO MVABE1 (1:1)
               MOVE FMAST-IO-AREA (104:5)  TO AVRBEL-IO
               INSPECT AVRBEL-IO REPLACING ALL ' ' BY '0'
               MOVE FMAST-IO-AREA (104:5)  TO AVRBEZ-IO
               INSPECT AVRBEZ-IO REPLACING ALL ' ' BY '0'
               MOVE FMAST-IO-AREA (104:1)  TO AVRBE1 (1:1)
               MOVE FMAST-IO-AREA (109:11) TO FAKBEL-IO
               INSPECT FAKBEL-IO REPLACING ALL ' ' BY '0'
               MOVE FMAST-IO-AREA (109:11) TO FAKBEZ-IO
               INSPECT FAKBEZ-IO REPLACING ALL ' ' BY '0'
               MOVE FMAST-IO-AREA (109:1)  TO FAKBE1 (1:1)
               MOVE FMAST-IO-AREA (111:9)  TO FAKB90-IO
               INSPECT FAKB90-IO REPLACING ALL ' ' BY '0'
               MOVE FMAST-IO-AREA (109:9)  TO FAKKR-IO
               INSPECT FAKKR-IO REPLACING ALL ' ' BY '0'
               MOVE FMAST-IO-AREA (118:2)  TO FAKORE-IO
               INSPECT FAKORE-IO REPLACING ALL ' ' BY '0'
               MOVE FMAST-IO-AREA (120:1)  TO MVATYP (1:1)
               MOVE FMAST-IO-AREA (121:4)  TO FRPRO-IO
               INSPECT FRPRO-IO REPLACING ALL ' ' BY '0'
               IF  FRPRO > ZERO
                   SET I-15                TO TRUE
               END-IF
               MOVE FMAST-IO-AREA (125:1)  TO GIROES (1:1)
               MOVE FMAST-IO-AREA (126:11) TO KSALDO-IO
               INSPECT KSALDO-IO REPLACING ALL ' ' BY '0'
               MOVE FMAST-IO-AREA (126:11) TO KSALDZ-IO
               INSPECT KSALDZ-IO REPLACING ALL ' ' BY '0'
               MOVE FMAST-IO-AREA (126:1)  TO KSALD1 (1:1)
               MOVE FMAST-IO-AREA (143:3)  TO FAKKNR-IO
               INSPECT FAKKNR-IO REPLACING ALL ' ' BY '0'
               MOVE FMAST-IO-AREA (146:1)  TO FPRTYP (1:1)
               MOVE FMAST-IO-AREA (148:2)  TO BM (1:2)
               MOVE FMAST-IO-AREA (150:1)  TO KRGIRO (1:1)
               MOVE FMAST-IO-AREA (1:150)  TO REC01 (1:150)
           WHEN ( FMAST-IO-AREA (22:1) = '1'
            AND   FMAST-IO-AREA (23:1) = '1' )
               MOVE FMAST-IO-AREA (1:3)    TO FNR (1:3)
               MOVE FMAST-IO-AREA (4:6)    TO KNR (1:6)
               MOVE FMAST-IO-AREA (10:6)   TO FAKNR (1:6)
               MOVE FMAST-IO-AREA (24:30)  TO KADR1 (1:30)
               MOVE FMAST-IO-AREA (54:30)  TO KADR2 (1:30)
               MOVE FMAST-IO-AREA (84:30)  TO KADR3 (1:30)
               MOVE FMAST-IO-AREA (114:4)  TO PNR (1:4)
               MOVE FMAST-IO-AREA (118:15) TO PSTED (1:15)
               MOVE FMAST-IO-AREA (148:2)  TO BM (1:2)
               MOVE FMAST-IO-AREA (1:150)  TO REC02 (1:150)
           WHEN ( FMAST-IO-AREA (22:1) = '1'
            AND   FMAST-IO-AREA (23:1) = '2' )
               MOVE FMAST-IO-AREA (1:3)    TO FNR (1:3)
               MOVE FMAST-IO-AREA (4:6)    TO KNR (1:6)
               MOVE FMAST-IO-AREA (10:6)   TO FAKNR (1:6)
               MOVE FMAST-IO-AREA (24:30)  TO VADR1 (1:30)
               IF  VADR1 = SPACES
                   SET I-10                TO TRUE
               END-IF
               MOVE FMAST-IO-AREA (54:30)  TO VADR2 (1:30)
               IF  VADR2 = SPACES
                   SET I-11                TO TRUE
               END-IF
               MOVE FMAST-IO-AREA (84:30)  TO VADR3 (1:30)
               IF  VADR3 = SPACES
                   SET I-12                TO TRUE
               END-IF
               MOVE FMAST-IO-AREA (114:19) TO VADR4 (1:19)
               IF  VADR4 = SPACES
                   SET I-13                TO TRUE
               END-IF
               MOVE FMAST-IO-AREA (148:2)  TO BM (1:2)
               MOVE FMAST-IO-AREA (1:150)  TO REC03 (1:150)
           WHEN ( FMAST-IO-AREA (22:1) = '2' )
               MOVE FMAST-IO-AREA (1:3)    TO FNR (1:3)
               MOVE FMAST-IO-AREA (4:6)    TO KNR (1:6)
               MOVE FMAST-IO-AREA (10:6)   TO FAKNR (1:6)
               MOVE FMAST-IO-AREA (22:2)   TO LINTYP (1:2)
               MOVE FMAST-IO-AREA (24:7)   TO ANTLEV-IO
               INSPECT ANTLEV-IO REPLACING ALL ' ' BY '0'
               MOVE FMAST-IO-AREA (31:20)  TO TEKST1 (1:20)
               MOVE FMAST-IO-AREA (51:30)  TO TEKST2 (1:30)
               MOVE FMAST-IO-AREA (31:50)  TO TEKSTL (1:50)
               MOVE FMAST-IO-AREA (81:9)   TO ENHPRI-IO
               INSPECT ENHPRI-IO REPLACING ALL ' ' BY '0'
               MOVE FMAST-IO-AREA (81:9)   TO ENHPRZ-IO
               INSPECT ENHPRZ-IO REPLACING ALL ' ' BY '0'
               MOVE FMAST-IO-AREA (81:1)   TO ENHPR1 (1:1)
               MOVE FMAST-IO-AREA (90:3)   TO RAB1-IO
               INSPECT RAB1-IO REPLACING ALL ' ' BY '0'
               MOVE FMAST-IO-AREA (93:3)   TO RAB2-IO
               INSPECT RAB2-IO REPLACING ALL ' ' BY '0'
               MOVE FMAST-IO-AREA (96:3)   TO RAB3-IO
               INSPECT RAB3-IO REPLACING ALL ' ' BY '0'
               MOVE FMAST-IO-AREA (99:1)   TO NTOIND (1:1)
               MOVE FMAST-IO-AREA (100:9)  TO NTOPRI-IO
               INSPECT NTOPRI-IO REPLACING ALL ' ' BY '0'
               MOVE FMAST-IO-AREA (100:9)  TO NTOPRZ-IO
               INSPECT NTOPRZ-IO REPLACING ALL ' ' BY '0'
               MOVE FMAST-IO-AREA (100:1)  TO NTOPR1 (1:1)
               MOVE FMAST-IO-AREA (148:2)  TO BM (1:2)
               MOVE FMAST-IO-AREA (1:150)  TO REC04 (1:150)
           END-EVALUATE.
 
       FMAST-IDCHK SECTION.
       FMAST-IDCHK-P.
           EVALUATE TRUE
           WHEN ( FMAST-IO-AREA (22:1) = '0' )
             OR ( FMAST-IO-AREA (22:1) = '1'
            AND   FMAST-IO-AREA (23:1) = '1' )
             OR ( FMAST-IO-AREA (22:1) = '1'
            AND   FMAST-IO-AREA (23:1) = '2' )
             OR ( FMAST-IO-AREA (22:1) = '2' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       FMAST-IDSET SECTION.
       FMAST-IDSET-P.
           EVALUATE TRUE
           WHEN ( FMAST-IO-AREA (22:1) = '0' )
               SET I-01                    TO TRUE
           WHEN ( FMAST-IO-AREA (22:1) = '1'
            AND   FMAST-IO-AREA (23:1) = '1' )
               SET I-02                    TO TRUE
           WHEN ( FMAST-IO-AREA (22:1) = '1'
            AND   FMAST-IO-AREA (23:1) = '2' )
               SET I-03                    TO TRUE
           WHEN ( FMAST-IO-AREA (22:1) = '2' )
               SET I-04                    TO TRUE
           END-EVALUATE.
 
       FMAST-CHK-LEVEL SECTION.
       FMAST-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( FMAST-IO-AREA (22:1) = '0' )
               MOVE LOW-VALUES             TO FMAST-LEVEL-01
               MOVE FMAST-IO-AREA (1:3)    TO FMAST-01-L4-FNR
               MOVE FMAST-IO-AREA (4:6)    TO FMAST-01-L3-KNR
               MOVE FMAST-IO-AREA (148:2)  TO FMAST-01-L2-BM
               MOVE FMAST-IO-AREA (10:6)   TO FMAST-01-L1-FAKNR
               IF  FMAST-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  FMAST-01-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   WHEN  FMAST-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  FMAST-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  FMAST-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  FMAST-01-L4           TO THE-PRIOR-L4
               MOVE  FMAST-01-L3           TO THE-PRIOR-L3
               MOVE  FMAST-01-L2           TO THE-PRIOR-L2
               MOVE  FMAST-01-L1           TO THE-PRIOR-L1
               SET FMAST-LEVEL-INIT        TO TRUE
           WHEN ( FMAST-IO-AREA (22:1) = '1'
            AND   FMAST-IO-AREA (23:1) = '1' )
               MOVE LOW-VALUES             TO FMAST-LEVEL-02
               MOVE FMAST-IO-AREA (1:3)    TO FMAST-02-L4-FNR
               MOVE FMAST-IO-AREA (4:6)    TO FMAST-02-L3-KNR
               MOVE FMAST-IO-AREA (148:2)  TO FMAST-02-L2-BM
               MOVE FMAST-IO-AREA (10:6)   TO FMAST-02-L1-FAKNR
               IF  FMAST-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  FMAST-02-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   WHEN  FMAST-02-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  FMAST-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  FMAST-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  FMAST-02-L4           TO THE-PRIOR-L4
               MOVE  FMAST-02-L3           TO THE-PRIOR-L3
               MOVE  FMAST-02-L2           TO THE-PRIOR-L2
               MOVE  FMAST-02-L1           TO THE-PRIOR-L1
               SET FMAST-LEVEL-INIT        TO TRUE
           WHEN ( FMAST-IO-AREA (22:1) = '1'
            AND   FMAST-IO-AREA (23:1) = '2' )
               MOVE LOW-VALUES             TO FMAST-LEVEL-03
               MOVE FMAST-IO-AREA (1:3)    TO FMAST-03-L4-FNR
               MOVE FMAST-IO-AREA (4:6)    TO FMAST-03-L3-KNR
               MOVE FMAST-IO-AREA (148:2)  TO FMAST-03-L2-BM
               MOVE FMAST-IO-AREA (10:6)   TO FMAST-03-L1-FAKNR
               IF  FMAST-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  FMAST-03-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   WHEN  FMAST-03-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  FMAST-03-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  FMAST-03-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  FMAST-03-L4           TO THE-PRIOR-L4
               MOVE  FMAST-03-L3           TO THE-PRIOR-L3
               MOVE  FMAST-03-L2           TO THE-PRIOR-L2
               MOVE  FMAST-03-L1           TO THE-PRIOR-L1
               SET FMAST-LEVEL-INIT        TO TRUE
           WHEN ( FMAST-IO-AREA (22:1) = '2' )
               MOVE LOW-VALUES             TO FMAST-LEVEL-04
               MOVE FMAST-IO-AREA (1:3)    TO FMAST-04-L4-FNR
               MOVE FMAST-IO-AREA (4:6)    TO FMAST-04-L3-KNR
               MOVE FMAST-IO-AREA (148:2)  TO FMAST-04-L2-BM
               MOVE FMAST-IO-AREA (10:6)   TO FMAST-04-L1-FAKNR
               IF  FMAST-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  FMAST-04-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   WHEN  FMAST-04-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  FMAST-04-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  FMAST-04-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  FMAST-04-L4           TO THE-PRIOR-L4
               MOVE  FMAST-04-L3           TO THE-PRIOR-L3
               MOVE  FMAST-04-L2           TO THE-PRIOR-L2
               MOVE  FMAST-04-L1           TO THE-PRIOR-L1
               SET FMAST-LEVEL-INIT        TO TRUE
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (1:3)   TO KONFNR (1:3)
               MOVE FIRMAF-IO-AREA (8:30)  TO FINAVN (1:30)
               MOVE FIRMAF-IO-AREA (504:30) TO FIADR (1:30)
               MOVE FIRMAF-IO-AREA (534:4) TO FIPNR (1:4)
               MOVE FIRMAF-IO-AREA (538:26) TO FIPST (1:26)
               MOVE FIRMAF-IO-AREA (564:4) TO GIROD1 (1:4)
               MOVE FIRMAF-IO-AREA (568:2) TO GIROD2 (1:2)
               MOVE FIRMAF-IO-AREA (570:5) TO GIROD3 (1:5)
               MOVE FIRMAF-IO-AREA (777:1) TO FAKTPT (1:1)
               MOVE FIRMAF-IO-AREA (778:1) TO FAKTBK (1:1)
               MOVE FIRMAF-IO-AREA (879:6) TO FBDKID-IO
               INSPECT FBDKID-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-05                        TO TRUE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (133:4) TO BANK1 (1:4)
               MOVE KUNDEMA-IO-AREA (137:2) TO BANK2 (1:2)
               MOVE KUNDEMA-IO-AREA (139:5) TO BANK3 (1:5)
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-05                        TO TRUE.
 
       KUNDEMX-FLDSET SECTION.
       KUNDEMX-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMX-IO-AREA (138:3) TO VALTYP (1:3)
           END-EVALUATE.
 
       KUNDEMX-IDSET SECTION.
       KUNDEMX-IDSET-P.
           SET I-05                        TO TRUE.
 
       STATTAB-FLDSET SECTION.
       STATTAB-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE STATTAB-IO-AREA (17:8) TO VKURS2-IO
               INSPECT VKURS2-IO REPLACING ALL ' ' BY '0'
               MOVE STATTAB-IO-AREA (39:1) TO AV100 (1:1)
           END-EVALUATE.
 
       STATTAB-IDSET SECTION.
       STATTAB-IDSET-P.
           SET I-05                        TO TRUE.
 
       PRF-PRINT-LINE SECTION.
       PRF-PRINT-LINE-P.
           IF  PRF-BEFORE-SKIP > 0
               PERFORM PRF-SKIP-BEFORE
           END-IF
           IF  PRF-BEFORE-SPACE > 0
               PERFORM PRF-SPACE-BEFORE
               IF  PRF-AFTER-SKIP > 0
                   PERFORM PRF-SKIP-AFTER
               END-IF
               IF  PRF-AFTER-SPACE > 0
                   PERFORM PRF-SPACE-AFTER
               END-IF
           ELSE
               IF  PRF-AFTER-SKIP > 0
                   PERFORM PRF-SKIP-AFTER
               END-IF
               PERFORM PRF-SPACE-AFTER
           END-IF
           IF  PRF-LINE-COUNT NOT < PRF-MAX-LINES
               MOVE 7                      TO PRF-AFTER-SKIP
           END-IF.
 
       PRF-SKIP-BEFORE SECTION.
       PRF-SKIP-BEFORE-P.
           WRITE PRF-IO-PRINT           AFTER ADVANCING PAGE
           MOVE 1                          TO PRF-LINE-COUNT
           MOVE 0                          TO PRF-BEFORE-SKIP
           INITIALIZE PRF-IO-AREA.
 
       PRF-SPACE-BEFORE SECTION.
       PRF-SPACE-BEFORE-P.
           WRITE PRF-IO-PRINT           AFTER PRF-BEFORE-SPACE LINES
           ADD PRF-BEFORE-SPACE            TO PRF-LINE-COUNT
           MOVE SPACES TO PRF-IO-AREA
           INITIALIZE PRF-IO-AREA
           MOVE 0                          TO PRF-BEFORE-SPACE.
 
       PRF-SKIP-AFTER SECTION.
       PRF-SKIP-AFTER-P.
           WRITE PRF-IO-PRINT          BEFORE ADVANCING PAGE
           MOVE 1                          TO PRF-LINE-COUNT
           MOVE 0                          TO PRF-AFTER-SKIP
           INITIALIZE PRF-IO-AREA.
 
       PRF-SPACE-AFTER SECTION.
       PRF-SPACE-AFTER-P.
           WRITE PRF-IO-PRINT          BEFORE PRF-AFTER-SPACE LINES
           ADD PRF-AFTER-SPACE             TO PRF-LINE-COUNT
           INITIALIZE PRF-IO-AREA
           MOVE 0                          TO PRF-AFTER-SPACE.
 
       FAKTAB-LOAD SECTION.
       FAKTAB-LOAD-P.
           OPEN INPUT FAKTAB
           SET TABFNR-I                    TO 1
           PERFORM UNTIL FAKTAB-EOF
               READ FAKTAB
               AT END
                   SET FAKTAB-EOF          TO TRUE
               NOT AT END
                   MOVE FAKTAB-IO-AREA (1:11) TO TABFNR-ENTRY
                                                            (TABFNR-I)
                   SET TABFNR-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE FAKTAB.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-21)
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE 'EPL1'                 TO EFAKT-IO-AREA (1:4)
               MOVE '112800'               TO EFAKT-IO-AREA (5:6)
               MOVE 'MEKK'                 TO EFAKT-IO-AREA (11:4)
               MOVE '0'                    TO EFAKT-IO-AREA (15:1)
               MOVE '0'                    TO EFAKT-IO-AREA (16:1)
               IF  (I-U2)
                   MOVE 'T'                TO EFAKT-IO-AREA (16:1)
               END-IF
               IF  (I-U3)
                   MOVE 'J'                TO EFAKT-IO-AREA (16:1)
               END-IF
               IF  (NOT-I-U4)
                   MOVE 'T'                TO EFAKT-IO-AREA (17:1)
               END-IF
               IF  (I-U4)
                   MOVE 'A'                TO EFAKT-IO-AREA (17:1)
               END-IF
               MOVE '0'                    TO EFAKT-IO-AREA (18:1)
               MOVE '1'                    TO EFAKT-IO-AREA (19:1)
               MOVE 'B'                    TO EFAKT-IO-AREA (20:1)
               MOVE 'S'                    TO EFAKT-IO-AREA (21:1)
               MOVE '0'                    TO EFAKT-IO-AREA (24:1)
      *                                  40 "EPL500XX"
               MOVE 'ESPEN LARSEN, AUTODA' TO EFAKT-IO-AREA (41:20)
               MOVE 'TA, TLF:23172030    ' TO EFAKT-IO-AREA (61:20)
               MOVE 'AAA'                  TO EFAKT-IO-AREA (113:3)
               MOVE 'AAAAAA'               TO EFAKT-IO-AREA (116:6)
               MOVE '0000'                 TO EFAKT-IO-AREA (122:4)
               MOVE '00'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'A'                    TO EFAKT-IO-AREA (128:1)
      *****************************************************************
      * BREVHODE/FAKTURAHEADER  EPLK                                  *
      *****************************************************************
               WRITE EFAKT-IO-AREA
           END-IF
           IF  (I-25 AND I-19 AND I-02)
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE 'EPLK'                 TO EFAKT-IO-AREA (1:4)
               MOVE 'NO'                   TO EFAKT-IO-AREA (5:2)
               MOVE PNR                    TO EFAKT-IO-AREA (7:4)
               MOVE '1'                    TO EFAKT-IO-AREA (15:1)
               IF  (I-32)
                   MOVE '2'                TO EFAKT-IO-AREA (15:1)
               END-IF
               IF  (I-33)
                   MOVE '3'                TO EFAKT-IO-AREA (15:1)
               END-IF
               MOVE '00'                   TO EFAKT-IO-AREA (16:2)
               MOVE FNR                    TO EFAKT-IO-AREA (26:3)
               MOVE KNR                    TO EFAKT-IO-AREA (29:6)
               MOVE FAKNR                  TO EFAKT-IO-AREA (35:6)
               MOVE FPRTYP                 TO EFAKT-IO-AREA (80:1)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '01'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'A'                    TO EFAKT-IO-AREA (128:1)
      *****************************************************************
      * HOPP TIL KANAL1 ØVERST PÅ SIDEN.                              *
      *****************************************************************
               WRITE EFAKT-IO-AREA
           END-IF
           IF  (I-25 AND I-02)
           OR  (I-25 AND I-40)
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE '10'                   TO EFAKT-IO-AREA (1:2)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '02'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'A'                    TO EFAKT-IO-AREA (128:1)
      *****************************************************************
      * SIDEDEFINISJON (OVERLAY) FAKTURATYPE MED ELLER UTEN GIRO. EPL5*
      *****************************************************************
               WRITE EFAKT-IO-AREA
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE 'EPL5'                 TO EFAKT-IO-AREA (1:4)
               IF  (NOT-I-38)
                   MOVE FRMTYP             TO EFAKT-IO-AREA (4:5)
               END-IF
               IF  (I-38)
                   MOVE '50002'            TO EFAKT-IO-AREA (4:5)
               END-IF
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '03'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'A'                    TO EFAKT-IO-AREA (128:1)
      *****************************************************************
      * FIRMAGRAFIKK SEGMENT.      EPL4                               *
      *****************************************************************
               WRITE EFAKT-IO-AREA
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE 'EPL400'               TO EFAKT-IO-AREA (1:6)
               MOVE TABGRF (TABGRF-I)      TO EFAKT-IO-AREA (7:8)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '04'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'A'                    TO EFAKT-IO-AREA (128:1)
      *****************************************************************
      * FAKTURAADRESSELINJER KANAL 2
      *****************************************************************
               WRITE EFAKT-IO-AREA
           END-IF
           IF  (I-25 AND I-22)
           OR  (I-25 AND I-40)
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE '20'                   TO EFAKT-IO-AREA (1:2)
               MOVE KADR1                  TO EFAKT-IO-AREA (3:30)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '05'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'A'                    TO EFAKT-IO-AREA (128:1)
               WRITE EFAKT-IO-AREA
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE ' 0'                   TO EFAKT-IO-AREA (1:2)
               MOVE KADR2                  TO EFAKT-IO-AREA (3:30)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '06'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'A'                    TO EFAKT-IO-AREA (128:1)
               WRITE EFAKT-IO-AREA
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE ' 0'                   TO EFAKT-IO-AREA (1:2)
               MOVE KADR3                  TO EFAKT-IO-AREA (3:30)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '07'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'A'                    TO EFAKT-IO-AREA (128:1)
               WRITE EFAKT-IO-AREA
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE ' 0'                   TO EFAKT-IO-AREA (1:2)
               MOVE PNR                    TO EFAKT-IO-AREA (3:4)
               MOVE PSTED                  TO EFAKT-IO-AREA (9:15)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '08'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'A'                    TO EFAKT-IO-AREA (128:1)
      *****************************************************************
      *            KUNDENR, FAKTURADATO, FORFALL OG BET.BET.          *
      *            Fakturakid og sidenr.             Kanal 3.         *
      *****************************************************************
               WRITE EFAKT-IO-AREA
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE '30'                   TO EFAKT-IO-AREA (1:2)
               MOVE KNR                    TO EFAKT-IO-AREA (3:6)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '09'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'A'                    TO EFAKT-IO-AREA (128:1)
               WRITE EFAKT-IO-AREA
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE ' 0'                   TO EFAKT-IO-AREA (1:2)
               MOVE FAKDAG                 TO EFAKT-IO-AREA (3:2)
               MOVE '.'                    TO EFAKT-IO-AREA (5:1)
               MOVE FAKMND                 TO EFAKT-IO-AREA (6:2)
               MOVE '.'                    TO EFAKT-IO-AREA (8:1)
               MOVE FAKAAR                 TO EFAKT-IO-AREA (9:4)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '10'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'A'                    TO EFAKT-IO-AREA (128:1)
               WRITE EFAKT-IO-AREA
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE ' 0'                   TO EFAKT-IO-AREA (1:2)
               MOVE FFDAG                  TO EFAKT-IO-AREA (3:2)
               MOVE '.'                    TO EFAKT-IO-AREA (5:1)
               MOVE FFMND                  TO EFAKT-IO-AREA (6:2)
               MOVE '.'                    TO EFAKT-IO-AREA (8:1)
               MOVE FFAAR                  TO EFAKT-IO-AREA (9:4)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '11'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'A'                    TO EFAKT-IO-AREA (128:1)
               WRITE EFAKT-IO-AREA
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE ' 0'                   TO EFAKT-IO-AREA (1:2)
               MOVE BETBET                 TO EFAKT-IO-AREA (3:24)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '12'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'A'                    TO EFAKT-IO-AREA (128:1)
               WRITE EFAKT-IO-AREA
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE ' 0'                   TO EFAKT-IO-AREA (1:2)
               IF  (NOT-I-24)
                   MOVE 'Fakturakid:'      TO EFAKT-IO-AREA (3:11)
               END-IF
               IF  (I-24)
                   MOVE 'Kr.notakid:'      TO EFAKT-IO-AREA (3:11)
               END-IF
               MOVE FAKKID                 TO EFAKT-IO-AREA (14:7)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '13'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'A'                    TO EFAKT-IO-AREA (128:1)
               WRITE EFAKT-IO-AREA
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE ' 0'                   TO EFAKT-IO-AREA (1:2)
               MOVE 'Side:'                TO EFAKT-IO-AREA (3:5)
               MOVE SIDENR                 TO XO-30YNZ
               MOVE XO-30YNZ               TO EFAKT-IO-AREA (8:3)
               MOVE '/'                    TO EFAKT-IO-AREA (11:1)
               MOVE ANTSID                 TO XO-30YNZ
               MOVE XO-30YNZ               TO EFAKT-IO-AREA (12:3)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '14'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'A'                    TO EFAKT-IO-AREA (128:1)
               WRITE EFAKT-IO-AREA
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE ' 0'                   TO EFAKT-IO-AREA (1:2)
               IF  (NOT-I-24)
                   MOVE 'FAKTURA '         TO EFAKT-IO-AREA (3:8)
               END-IF
               IF  (NOT-I-24)
                   MOVE FAKNR              TO EFAKT-IO-AREA (11:6)
               END-IF
               IF  (I-24)
                   MOVE 'KREDITNOTA '      TO EFAKT-IO-AREA (3:11)
               END-IF
               IF  (I-24)
                   MOVE FAKNR              TO EFAKT-IO-AREA (14:6)
               END-IF
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '15'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'A'                    TO EFAKT-IO-AREA (128:1)
      *****************************************************************
      * Levering-adresselinjer.        Kanal 4.
      *****************************************************************
               WRITE EFAKT-IO-AREA
           END-IF
           IF  (I-25 AND I-22 AND I-93)
           AND (NOT-I-10)
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE '40'                   TO EFAKT-IO-AREA (1:2)
               MOVE VADR1                  TO EFAKT-IO-AREA (3:30)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '16'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'A'                    TO EFAKT-IO-AREA (128:1)
               WRITE EFAKT-IO-AREA
           END-IF
           IF  (I-25 AND I-22 AND I-93)
           AND (NOT-I-11)
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE ' 0'                   TO EFAKT-IO-AREA (1:2)
               IF  (I-10)
                   MOVE '40'               TO EFAKT-IO-AREA (1:2)
               END-IF
               MOVE VADR2                  TO EFAKT-IO-AREA (3:30)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '17'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'A'                    TO EFAKT-IO-AREA (128:1)
               WRITE EFAKT-IO-AREA
           END-IF
           IF  (I-25 AND I-22 AND I-93)
           AND (NOT-I-12)
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE ' 0'                   TO EFAKT-IO-AREA (1:2)
               IF  (I-10 AND I-11)
                   MOVE '40'               TO EFAKT-IO-AREA (1:2)
               END-IF
               MOVE VADR3                  TO EFAKT-IO-AREA (3:30)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '18'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'A'                    TO EFAKT-IO-AREA (128:1)
               WRITE EFAKT-IO-AREA
           END-IF
           IF  (I-25 AND I-22 AND I-93)
           AND (NOT-I-13)
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE ' 0'                   TO EFAKT-IO-AREA (1:2)
               IF  (I-10 AND I-11 AND I-12)
                   MOVE '40'               TO EFAKT-IO-AREA (1:2)
               END-IF
               MOVE VADR4                  TO EFAKT-IO-AREA (3:19)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '19'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'A'                    TO EFAKT-IO-AREA (128:1)
      *****************************************************************
      * Fakturalinjer.         Kanal 5.                               *
      *****************************************************************
               WRITE EFAKT-IO-AREA
           END-IF
           IF  (I-04 AND I-25)
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               IF  (I-22)
                   MOVE '50'               TO EFAKT-IO-AREA (1:2)
               END-IF
               IF  (I-40)
                   MOVE '50'               TO EFAKT-IO-AREA (1:2)
               END-IF
               IF  (NOT-I-22 AND NOT-I-40)
                   MOVE ' 0'               TO EFAKT-IO-AREA (1:2)
               END-IF
               IF  (I-41)
                   MOVE ANTLEV             TO XO-52YN9
                   MOVE XO-52YN9           TO EFAKT-IO-AREA (3:8)
               END-IF
               IF  (NOT-I-42)
                   MOVE TEKST1             TO EFAKT-IO-AREA (13:20)
               END-IF
               IF  (NOT-I-42)
                   MOVE TEKST2             TO EFAKT-IO-AREA (35:30)
               END-IF
               IF  (I-42)
                   MOVE TEKSTL             TO EFAKT-IO-AREA (13:50)
               END-IF
               IF  (I-41)
                   MOVE ENHPRI             TO XO-72YN9
                   MOVE XO-72YN9           TO EFAKT-IO-AREA (67:10)
               END-IF
               IF  (I-65 AND I-41)
                   MOVE ENHPRE             TO XO-54YN9
                   MOVE XO-54YN9           TO EFAKT-IO-AREA (67:10)
               END-IF
               IF  (NOT-I-44 AND I-41)
                   MOVE RAB1               TO XO-21YNZ
                   MOVE XO-21YNZ           TO EFAKT-IO-AREA (79:4)
               END-IF
               IF  (NOT-I-44 AND I-41)
                   MOVE RAB2               TO XO-21YNZ
                   MOVE XO-21YNZ           TO EFAKT-IO-AREA (84:4)
               END-IF
               IF  (NOT-I-44 AND I-41)
                   MOVE RAB3               TO XO-21YNZ
                   MOVE XO-21YNZ           TO EFAKT-IO-AREA (89:4)
               END-IF
               IF  (I-44 AND I-41)
                   MOVE 'Netto'            TO EFAKT-IO-AREA (79:5)
               END-IF
               IF  (I-41)
                   MOVE NTOPRI             TO XO-72YN9R
                   MOVE XO-72YN9R          TO EFAKT-IO-AREA (94:11)
               END-IF
               IF  (I-65 AND I-41)
                   MOVE NTOPRE             TO XO-54YN9R
                   MOVE XO-54YN9R          TO EFAKT-IO-AREA (94:11)
               END-IF
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '20'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'A'                    TO EFAKT-IO-AREA (128:1)
      *****************************************************************
      * Fakturatotaler.            Kanal 6.                           *
      * 64 = I UTL.VALUTA M/2 DESIMALER.                              *
      * 65 = I UTL.VALUTA M/4 DESIMALER (EURO).                       *
      *****************************************************************
               WRITE EFAKT-IO-AREA
           END-IF
           IF  (I-02)
               MOVE SPACES TO FBKOPI-IO-AREA
               INITIALIZE FBKOPI-IO-AREA
               MOVE REC02                  TO FBKOPI-IO-AREA (1:150)
               MOVE 'NOK'                  TO FBKOPI-IO-AREA (141:3)
               IF  (I-61 AND I-64)
                   MOVE VALTYP             TO FBKOPI-IO-AREA (141:3)
               END-IF
               WRITE FBKOPI-IO-AREA
           END-IF
           IF  (I-03)
               MOVE SPACES TO FBKOPI-IO-AREA
               INITIALIZE FBKOPI-IO-AREA
               MOVE REC03                  TO FBKOPI-IO-AREA (1:150)
               MOVE 'NOK'                  TO FBKOPI-IO-AREA (141:3)
               IF  (I-61 AND I-64)
                   MOVE VALTYP             TO FBKOPI-IO-AREA (141:3)
               END-IF
               WRITE FBKOPI-IO-AREA
           END-IF
           IF  (I-04)
               MOVE SPACES TO FBKOPI-IO-AREA
               INITIALIZE FBKOPI-IO-AREA
               MOVE REC04                  TO FBKOPI-IO-AREA (1:150)
               MOVE ENHPRZ                 TO EDIT-ENHPRZ
               MOVE EDIT-ENHPRZ            TO FBKOPI-IO-AREA (81:9)
               IF  (NOT-I-56)
                   MOVE ENHPR1             TO FBKOPI-IO-AREA (81:1)
               END-IF
               IF  (I-56)
                   MOVE '-'                TO FBKOPI-IO-AREA (81:1)
               END-IF
               MOVE NTOPRZ                 TO EDIT-NTOPRZ
               MOVE EDIT-NTOPRZ            TO FBKOPI-IO-AREA (100:9)
               IF  (NOT-I-57)
                   MOVE NTOPR1             TO FBKOPI-IO-AREA (100:1)
               END-IF
               IF  (I-57)
                   MOVE '-'                TO FBKOPI-IO-AREA (100:1)
               END-IF
               MOVE SIDENR-IO              TO FBKOPI-IO-AREA (115:3)
               MOVE ANTVLS-IO              TO FBKOPI-IO-AREA (118:3)
               WRITE FBKOPI-IO-AREA
           END-IF.
 
       EXCEPTION-OUTPUT SECTION.
       EXCEPTION-OUTPUT-P.
           IF  (I-90 AND NOT-I-64)
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE '60'                   TO EFAKT-IO-AREA (1:2)
               MOVE NTOBEL                 TO XO-92YN9R
               MOVE XO-92YN9R              TO EFAKT-IO-AREA (9:13)
               MOVE MVABEL                 TO XO-92YN9R
               MOVE XO-92YN9R              TO EFAKT-IO-AREA (29:13)
               MOVE AVRBEL                 TO XO-32YN9R
               MOVE XO-32YN9R              TO EFAKT-IO-AREA (55:7)
               MOVE FAKBEL                 TO XO-92YN9R
               MOVE XO-92YN9R              TO EFAKT-IO-AREA (69:13)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '21'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'A'                    TO EFAKT-IO-AREA (128:1)
               WRITE EFAKT-IO-AREA
           END-IF
           IF  (I-90 AND I-64)
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE '60'                   TO EFAKT-IO-AREA (1:2)
               IF  (NOT-I-65)
                   MOVE NTOBEV             TO XO-92YN9R
                   MOVE XO-92YN9R          TO EFAKT-IO-AREA (9:13)
               END-IF
               IF  (I-65)
                   MOVE NTOBEE             TO XO-74YN9R
                   MOVE XO-74YN9R          TO EFAKT-IO-AREA (9:13)
               END-IF
               IF  (NOT-I-65)
                   MOVE MVABEV             TO XO-92YN9R
                   MOVE XO-92YN9R          TO EFAKT-IO-AREA (29:13)
               END-IF
               IF  (I-65)
                   MOVE MVABEE             TO XO-74YN9R
                   MOVE XO-74YN9R          TO EFAKT-IO-AREA (29:13)
               END-IF
               IF  (NOT-I-65)
                   MOVE AVRBEV             TO XO-32YN9R
                   MOVE XO-32YN9R          TO EFAKT-IO-AREA (55:7)
               END-IF
               IF  (I-65)
                   MOVE AVRBEE             TO XO-14YN9R
                   MOVE XO-14YN9R          TO EFAKT-IO-AREA (55:7)
               END-IF
               IF  (NOT-I-65)
                   MOVE FAKBEV             TO XO-92YN9R
                   MOVE XO-92YN9R          TO EFAKT-IO-AREA (69:13)
               END-IF
               IF  (I-65)
                   MOVE FAKBEE             TO XO-74YN9R
                   MOVE XO-74YN9R          TO EFAKT-IO-AREA (69:13)
               END-IF
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '21'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'A'                    TO EFAKT-IO-AREA (128:1)
               WRITE EFAKT-IO-AREA
           END-IF
           IF  (I-90 AND NOT-I-24 AND I-15)
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE ' 0'                   TO EFAKT-IO-AREA (1:2)
               MOVE 'Ved for sen betaling' TO EFAKT-IO-AREA (3:20)
               MOVE 'beregnes'             TO EFAKT-IO-AREA (24:8)
               MOVE FRPRO                  TO XO-22YN9
               MOVE XO-22YN9               TO EFAKT-IO-AREA (32:5)
               MOVE '% forsinkelsesrente ' TO EFAKT-IO-AREA (38:20)
               MOVE 'pr.mnd.'              TO EFAKT-IO-AREA (59:7)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '22'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'A'                    TO EFAKT-IO-AREA (128:1)
               WRITE EFAKT-IO-AREA
           END-IF
           IF  (I-90 AND I-61 AND I-64)
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE ' 0'                   TO EFAKT-IO-AREA (1:2)
               MOVE 'Alle priser er i NOK' TO EFAKT-IO-AREA (3:20)
               MOVE VALTYP                 TO EFAKT-IO-AREA (20:3)
               MOVE '.'                    TO EFAKT-IO-AREA (23:1)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '23'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'A'                    TO EFAKT-IO-AREA (128:1)
      *****************************************************************
      * Her begynner underliggende Giro. Hopp til kanal 8 først.     *
      *****************************************************************
               WRITE EFAKT-IO-AREA
           END-IF
           IF  (I-91)
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE '10'                   TO EFAKT-IO-AREA (1:2)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '27'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'B'                    TO EFAKT-IO-AREA (128:1)
      *****************************************************************
      * SIDEDEFINISJON (OVERLAY) FAKTURATYPE MED ELLER UTEN GIRO. EPL5*
      *****************************************************************
               WRITE EFAKT-IO-AREA
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE 'EPL5'                 TO EFAKT-IO-AREA (1:4)
               MOVE FRMTYP                 TO EFAKT-IO-AREA (4:5)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '28'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'B'                    TO EFAKT-IO-AREA (128:1)
      *****************************************************************
      * FIRMAGRAFIKK SEGMENT.      EPL4                               *
      *****************************************************************
               WRITE EFAKT-IO-AREA
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE 'EPL400'               TO EFAKT-IO-AREA (1:6)
               MOVE TABGRF (TABGRF-I)      TO EFAKT-IO-AREA (7:8)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '29'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'B'                    TO EFAKT-IO-AREA (128:1)
      *****************************************************************
      * FAKTURAADRESSELINJER KANAL 2
      *****************************************************************
               WRITE EFAKT-IO-AREA
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE '20'                   TO EFAKT-IO-AREA (1:2)
               MOVE KADR1                  TO EFAKT-IO-AREA (3:30)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '30'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'B'                    TO EFAKT-IO-AREA (128:1)
               WRITE EFAKT-IO-AREA
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE ' 0'                   TO EFAKT-IO-AREA (1:2)
               MOVE KADR2                  TO EFAKT-IO-AREA (3:30)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '31'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'B'                    TO EFAKT-IO-AREA (128:1)
               WRITE EFAKT-IO-AREA
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE ' 0'                   TO EFAKT-IO-AREA (1:2)
               MOVE KADR3                  TO EFAKT-IO-AREA (3:30)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '32'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'B'                    TO EFAKT-IO-AREA (128:1)
               WRITE EFAKT-IO-AREA
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE ' 0'                   TO EFAKT-IO-AREA (1:2)
               MOVE PNR                    TO EFAKT-IO-AREA (3:4)
               MOVE PSTED                  TO EFAKT-IO-AREA (9:15)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '33'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'B'                    TO EFAKT-IO-AREA (128:1)
      *****************************************************************
      *            KUNDENR, FAKTURADATO, FORFALL OG BET.BET.          *
      *            Fakturakid og sidenr.             Kanal 3.         *
      *****************************************************************
               WRITE EFAKT-IO-AREA
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE '30'                   TO EFAKT-IO-AREA (1:2)
               MOVE KNR                    TO EFAKT-IO-AREA (3:6)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '34'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'B'                    TO EFAKT-IO-AREA (128:1)
               WRITE EFAKT-IO-AREA
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE ' 0'                   TO EFAKT-IO-AREA (1:2)
               MOVE FAKDAG                 TO EFAKT-IO-AREA (3:2)
               MOVE '.'                    TO EFAKT-IO-AREA (5:1)
               MOVE FAKMND                 TO EFAKT-IO-AREA (6:2)
               MOVE '.'                    TO EFAKT-IO-AREA (8:1)
               MOVE FAKAAR                 TO EFAKT-IO-AREA (9:4)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '35'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'B'                    TO EFAKT-IO-AREA (128:1)
               WRITE EFAKT-IO-AREA
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE ' 0'                   TO EFAKT-IO-AREA (1:2)
               MOVE FFDAGX                 TO EFAKT-IO-AREA (3:2)
               MOVE '.'                    TO EFAKT-IO-AREA (5:1)
               MOVE FFMNDX                 TO EFAKT-IO-AREA (6:2)
               MOVE '.'                    TO EFAKT-IO-AREA (8:1)
               MOVE FFAARX                 TO EFAKT-IO-AREA (9:4)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '36'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'B'                    TO EFAKT-IO-AREA (128:1)
               WRITE EFAKT-IO-AREA
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE ' 0'                   TO EFAKT-IO-AREA (1:2)
               MOVE BETBET                 TO EFAKT-IO-AREA (3:24)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '37'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'B'                    TO EFAKT-IO-AREA (128:1)
      *       E        91N58
               WRITE EFAKT-IO-AREA
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE ' 0'                   TO EFAKT-IO-AREA (1:2)
               MOVE 'Girokid:'             TO EFAKT-IO-AREA (3:8)
      *                        GIRKID    17
               IF  (NOT-I-58)
                   MOVE GIRKID             TO EFAKT-IO-AREA (11:7)
               END-IF
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '38'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'B'                    TO EFAKT-IO-AREA (128:1)
               WRITE EFAKT-IO-AREA
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE ' 0'                   TO EFAKT-IO-AREA (1:2)
               MOVE 'Side:'                TO EFAKT-IO-AREA (3:5)
               MOVE SIDENR                 TO XO-30YNZ
               MOVE XO-30YNZ               TO EFAKT-IO-AREA (8:3)
               MOVE '/'                    TO EFAKT-IO-AREA (11:1)
               MOVE AVSIDE                 TO XO-30YNZ
               MOVE XO-30YNZ               TO EFAKT-IO-AREA (12:3)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '39'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'B'                    TO EFAKT-IO-AREA (128:1)
               WRITE EFAKT-IO-AREA
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE ' 0'                   TO EFAKT-IO-AREA (1:2)
               IF  (NOT-I-36)
                   MOVE 'BETALINGSDOKUMENT' TO EFAKT-IO-AREA (3:17)
               END-IF
               IF  (I-36)
                   MOVE 'AUTOGIROAVTALE   ' TO EFAKT-IO-AREA (3:17)
               END-IF
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '40'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'B'                    TO EFAKT-IO-AREA (128:1)
      *****************************************************************
      * Betalingspesifikasjon  Kanal 5.                               *
      *****************************************************************
               WRITE EFAKT-IO-AREA
           END-IF
           IF  (I-94)
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               IF  (I-96)
                   MOVE '50'               TO EFAKT-IO-AREA (1:2)
               END-IF
               IF  (NOT-I-96)
                   MOVE ' 0'               TO EFAKT-IO-AREA (1:2)
               END-IF
               IF  (NOT-I-99)
                   MOVE 'Faktura'          TO EFAKT-IO-AREA (35:7)
               END-IF
               IF  (I-99)
                   MOVE 'Kreditnota'       TO EFAKT-IO-AREA (35:10)
               END-IF
               MOVE AR1 (X)                TO EFAKT-IO-AREA (46:6)
               MOVE AR2 (X)                TO XO-82YN9R
               MOVE XO-82YN9R              TO EFAKT-IO-AREA (93:12)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '41'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'B'                    TO EFAKT-IO-AREA (128:1)
      *****************************************************************
      * Tekst til slutt på autogiro.                                  *
      *****************************************************************
               WRITE EFAKT-IO-AREA
           END-IF
           IF  (I-95 AND I-36 AND NOT-I-98)
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE ' 0'                   TO EFAKT-IO-AREA (1:2)
               MOVE 'Ved forfall belastes' TO EFAKT-IO-AREA (13:20)
               MOVE ' automatisk Deres ba' TO EFAKT-IO-AREA (33:20)
               MOVE 'nkkonto:'             TO EFAKT-IO-AREA (53:8)
               IF  (NOT-I-37)
                   MOVE BANK1              TO EFAKT-IO-AREA (61:4)
               END-IF
               IF  (NOT-I-37)
                   MOVE '.'                TO EFAKT-IO-AREA (65:1)
               END-IF
               IF  (NOT-I-37)
                   MOVE BANK2              TO EFAKT-IO-AREA (66:2)
               END-IF
               IF  (NOT-I-37)
                   MOVE '.'                TO EFAKT-IO-AREA (68:1)
               END-IF
               IF  (NOT-I-37)
                   MOVE BANK3              TO EFAKT-IO-AREA (69:5)
               END-IF
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '42'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'B'                    TO EFAKT-IO-AREA (128:1)
               WRITE EFAKT-IO-AREA
           END-IF
           IF  (I-95 AND I-98)
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE ' 0'                   TO EFAKT-IO-AREA (1:2)
               MOVE 'På grunn av kreditno' TO EFAKT-IO-AREA (13:20)
               MOVE 'ta viser dette betal' TO EFAKT-IO-AREA (33:20)
               MOVE 'ingsdokument beløp i ' TO EFAKT-IO-AREA (53:21)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '43'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'B'                    TO EFAKT-IO-AREA (128:1)
               WRITE EFAKT-IO-AREA
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE ' 0'                   TO EFAKT-IO-AREA (1:2)
               MOVE 'Deres favør. Kreditn' TO EFAKT-IO-AREA (13:20)
               MOVE 'ota skal normalt gå ' TO EFAKT-IO-AREA (33:20)
               MOVE 'til fradrag på fram- ' TO EFAKT-IO-AREA (53:21)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '44'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'B'                    TO EFAKT-IO-AREA (128:1)
               WRITE EFAKT-IO-AREA
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE ' 0'                   TO EFAKT-IO-AREA (1:2)
               MOVE 'tidig kjøp, og vi be' TO EFAKT-IO-AREA (13:20)
               MOVE 'r Dem ta hensyn til ' TO EFAKT-IO-AREA (33:20)
               MOVE 'beløpet ved neste    ' TO EFAKT-IO-AREA (53:21)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '45'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'B'                    TO EFAKT-IO-AREA (128:1)
               WRITE EFAKT-IO-AREA
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE ' 0'                   TO EFAKT-IO-AREA (1:2)
               MOVE 'betaling.           ' TO EFAKT-IO-AREA (13:20)
               MOVE '                    ' TO EFAKT-IO-AREA (33:20)
               MOVE '                     ' TO EFAKT-IO-AREA (53:21)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '46'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'B'                    TO EFAKT-IO-AREA (128:1)
      *       E        95 98
      *                                   2 " 0"
      *                                  32 "og sendes til oss på"
      *                                  52 " telefaksnr. 3803325"
      *                                  73 "3.                   "
      *                        FNR      115
      *                        FAKNR    121
      *                        RECNR X  125
      *                                 127 "47"
      *                                 128 "B"
      *****************************************************************
      * Betalingsspesifikasjon sum Kanal 6.                           *
      *****************************************************************
               WRITE EFAKT-IO-AREA
           END-IF
           IF  (I-95)
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE '60'                   TO EFAKT-IO-AREA (1:2)
               MOVE SGSUM                  TO XO-82YN9R
               MOVE XO-82YN9R              TO EFAKT-IO-AREA (70:12)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '51'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'B'                    TO EFAKT-IO-AREA (128:1)
               WRITE EFAKT-IO-AREA
           END-IF
           IF  (I-95 AND I-15)
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE ' 0'                   TO EFAKT-IO-AREA (1:2)
               MOVE 'Ved for sen betaling' TO EFAKT-IO-AREA (3:20)
               MOVE 'beregnes'             TO EFAKT-IO-AREA (24:8)
               MOVE FRPRO                  TO XO-22YN9
               MOVE XO-22YN9               TO EFAKT-IO-AREA (32:5)
               MOVE '% forsinkelsesrente ' TO EFAKT-IO-AREA (38:20)
               MOVE 'pr.mnd.'              TO EFAKT-IO-AREA (59:7)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '52'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'B'                    TO EFAKT-IO-AREA (128:1)
      *****************************************************************
      * Her begynner underliggende Giro. Hopp til kanal 8 først.     *
      *****************************************************************
               WRITE EFAKT-IO-AREA
           END-IF
           IF  (I-95)
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE '80'                   TO EFAKT-IO-AREA (1:2)
               MOVE FINAVN                 TO EFAKT-IO-AREA (3:30)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '53'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'B'                    TO EFAKT-IO-AREA (128:1)
               WRITE EFAKT-IO-AREA
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE ' 0'                   TO EFAKT-IO-AREA (1:2)
               MOVE FIADR                  TO EFAKT-IO-AREA (3:30)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '54'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'B'                    TO EFAKT-IO-AREA (128:1)
               WRITE EFAKT-IO-AREA
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE ' 0'                   TO EFAKT-IO-AREA (1:2)
               MOVE FIPNR                  TO EFAKT-IO-AREA (3:4)
               MOVE FIPST                  TO EFAKT-IO-AREA (7:26)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '55'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'B'                    TO EFAKT-IO-AREA (128:1)
      *****************************************************************
      * Her ligger OCR-linjen på Giro. Hopp til kanal 9 først.        *
      *****************************************************************
               WRITE EFAKT-IO-AREA
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE '90'                   TO EFAKT-IO-AREA (1:2)
               IF  (NOT-I-58 AND NOT-I-36 AND I-87)
                   MOVE GIRKID             TO EFAKT-IO-AREA (20:7)
               END-IF
               IF  (NOT-I-36 AND I-87)
                   MOVE SGSUM              TO XO-82YN9
                   MOVE XO-82YN9           TO EFAKT-IO-AREA (30:11)
               END-IF
               MOVE ' '                    TO EFAKT-IO-AREA (38:1)
               IF  (NOT-I-36 AND I-87)
                   MOVE BELKID-IO          TO EFAKT-IO-AREA (44:1)
               END-IF
               MOVE GIROD1                 TO EFAKT-IO-AREA (49:4)
               MOVE GIROD2                 TO EFAKT-IO-AREA (54:2)
               MOVE GIROD3                 TO EFAKT-IO-AREA (57:5)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '56'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'B'                    TO EFAKT-IO-AREA (128:1)
      *****************************************************************
               WRITE EFAKT-IO-AREA
           END-IF
           IF  (I-94 AND NOT-I-58)
               MOVE SPACES TO KIDRELF-IO-AREA
               INITIALIZE KIDRELF-IO-AREA
               MOVE 'A'                    TO KIDRELF-IO-AREA (1:1)
               MOVE FIRMNR                 TO KIDRELF-IO-AREA (2:3)
               MOVE SBDKID-IO              TO KIDRELF-IO-AREA (5:6)
               MOVE X-IO                   TO KIDRELF-IO-AREA (18:3)
               MOVE KNR                    TO KIDRELF-IO-AREA (21:6)
               MOVE AR1 (X)                TO KIDRELF-IO-AREA (27:6)
               MOVE RELBEL-IO              TO KIDRELF-IO-AREA (33:9)
               MOVE FFAARY                 TO KIDRELF-IO-AREA (42:2)
               MOVE FFMNDX                 TO KIDRELF-IO-AREA (44:2)
               MOVE FFDAGX                 TO KIDRELF-IO-AREA (46:2)
               IF  (NOT-I-36)
                   MOVE 'D'                TO KIDRELF-IO-AREA (48:1)
               END-IF
               IF  (I-36)
                   MOVE 'A'                TO KIDRELF-IO-AREA (48:1)
               END-IF
               MOVE SGSUM-IO               TO KIDRELF-IO-AREA (49:10)
               MOVE UYEAR                  TO KIDRELF-IO-AREA (59:2)
               MOVE UMONTH                 TO KIDRELF-IO-AREA (61:2)
               MOVE UDAY                   TO KIDRELF-IO-AREA (63:2)
               MOVE BDLIN-IO               TO KIDRELF-IO-AREA (65:3)
               WRITE KIDRELF-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-25 AND I-30)
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE '80'                   TO EFAKT-IO-AREA (1:2)
               MOVE FINAVN                 TO EFAKT-IO-AREA (3:30)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '23'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'G'                    TO EFAKT-IO-AREA (128:1)
               WRITE EFAKT-IO-AREA
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE ' 0'                   TO EFAKT-IO-AREA (1:2)
               MOVE FIADR                  TO EFAKT-IO-AREA (3:30)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '24'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'G'                    TO EFAKT-IO-AREA (128:1)
               WRITE EFAKT-IO-AREA
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE ' 0'                   TO EFAKT-IO-AREA (1:2)
               MOVE FIPNR                  TO EFAKT-IO-AREA (3:4)
               MOVE FIPST                  TO EFAKT-IO-AREA (7:26)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '25'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'G'                    TO EFAKT-IO-AREA (128:1)
      *****************************************************************
      * Her ligger OCR-linjen på Giro. Hopp til kanal 9 først.        *
      *****************************************************************
               WRITE EFAKT-IO-AREA
               MOVE SPACES TO EFAKT-IO-AREA
               INITIALIZE EFAKT-IO-AREA
               MOVE '90'                   TO EFAKT-IO-AREA (1:2)
               MOVE FAKKID                 TO EFAKT-IO-AREA (20:7)
               MOVE FAKKR                  TO XO-90YN9
               MOVE XO-90YN9               TO EFAKT-IO-AREA (29:9)
               MOVE FAKORE-IO              TO EFAKT-IO-AREA (39:2)
               MOVE BELKID-IO              TO EFAKT-IO-AREA (44:1)
               MOVE GIROD1                 TO EFAKT-IO-AREA (49:4)
               MOVE GIROD2                 TO EFAKT-IO-AREA (54:2)
               MOVE GIROD3                 TO EFAKT-IO-AREA (57:5)
               MOVE FNR                    TO EFAKT-IO-AREA (113:3)
               MOVE FAKNR                  TO EFAKT-IO-AREA (116:6)
               MOVE RECNR-IO               TO EFAKT-IO-AREA (122:4)
               MOVE '26'                   TO EFAKT-IO-AREA (126:2)
               MOVE 'G'                    TO EFAKT-IO-AREA (128:1)
      *****************************************************************
      * BETALINGSDOKUMENT.                                           *
      * HOPP TIL KANAL1 ØVERST PÅ SIDEN.                              *
      *****************************************************************
               WRITE EFAKT-IO-AREA
           END-IF
           IF  (I-L4 AND I-25 AND NOT-I-09)
           AND (NOT-I-U8 AND NOT-I-58)
               MOVE SBDKID-IO              TO FIRMAF-IO-AREA (879:6)
               REWRITE FIRMAF-IO-AREA
                   INVALID KEY
                       DISPLAY 'Bad REWRITE - file = FIRMAF'
               END-REWRITE
           END-IF
           IF  (I-L1)
               MOVE SPACES TO FBKOPI-IO-AREA
               INITIALIZE FBKOPI-IO-AREA
               MOVE REC01                  TO FBKOPI-IO-AREA (1:150)
               MOVE FAKAAR                 TO FBKOPI-IO-AREA (33:4)
               MOVE FAKMND                 TO FBKOPI-IO-AREA (37:2)
               MOVE FAKDAG                 TO FBKOPI-IO-AREA (39:2)
               MOVE FFAAR                  TO FBKOPI-IO-AREA (41:4)
               MOVE FFMND                  TO FBKOPI-IO-AREA (45:2)
               MOVE FFDAG                  TO FBKOPI-IO-AREA (47:2)
               MOVE NTOBEZ                 TO EDIT-NTOBEZ
               MOVE EDIT-NTOBEZ            TO FBKOPI-IO-AREA (82:11)
               IF  (NOT-I-51)
                   MOVE NTOBE1             TO FBKOPI-IO-AREA (82:1)
               END-IF
               IF  (I-51)
                   MOVE '-'                TO FBKOPI-IO-AREA (82:1)
               END-IF
               MOVE MVABEZ                 TO EDIT-MVABEZ
               MOVE EDIT-MVABEZ            TO FBKOPI-IO-AREA (93:11)
               IF  (NOT-I-52)
                   MOVE MVABE1             TO FBKOPI-IO-AREA (93:1)
               END-IF
               IF  (I-52)
                   MOVE '-'                TO FBKOPI-IO-AREA (93:1)
               END-IF
               MOVE AVRBEZ                 TO EDIT-AVRBEZ
               MOVE EDIT-AVRBEZ            TO FBKOPI-IO-AREA (104:5)
               IF  (NOT-I-53)
                   MOVE AVRBE1             TO FBKOPI-IO-AREA (104:1)
               END-IF
               IF  (I-53)
                   MOVE '-'                TO FBKOPI-IO-AREA (104:1)
               END-IF
               MOVE FAKBEZ                 TO EDIT-FAKBEZ
               MOVE EDIT-FAKBEZ            TO FBKOPI-IO-AREA (109:11)
               IF  (NOT-I-54)
                   MOVE FAKBE1             TO FBKOPI-IO-AREA (109:1)
               END-IF
               IF  (I-54)
                   MOVE '-'                TO FBKOPI-IO-AREA (109:1)
               END-IF
               MOVE KSALDZ                 TO EDIT-KSALDZ
               MOVE EDIT-KSALDZ            TO FBKOPI-IO-AREA (126:11)
               IF  (NOT-I-55)
                   MOVE KSALD1             TO FBKOPI-IO-AREA (126:1)
               END-IF
               IF  (I-55)
                   MOVE '-'                TO FBKOPI-IO-AREA (126:1)
               END-IF
               IF  (I-25)
                   MOVE 'J'                TO FBKOPI-IO-AREA (147:1)
               END-IF
               IF  (NOT-I-25)
                   MOVE 'N'                TO FBKOPI-IO-AREA (147:1)
               END-IF
               WRITE FBKOPI-IO-AREA
           END-IF
           IF  (I-L4 AND I-29)
               MOVE SPACES TO FORDRE-IO-AREA
               INITIALIZE FORDRE-IO-AREA
               MOVE '399001'               TO FORDRE-IO-AREA (1:6)
               MOVE '100'                  TO FORDRE-IO-AREA (7:3)
               MOVE FNR                    TO FORDRE-IO-AREA (10:3)
               MOVE 'FAKFAKTURA       '    TO FORDRE-IO-AREA (13:17)
               MOVE ANTTA3-IO              TO FORDRE-IO-AREA (30:5)
               MOVE '000000000'            TO FORDRE-IO-AREA (35:9)
      *****************************************************************
      * MAIL-PDF                                                      *
      *****************************************************************
               WRITE FORDRE-IO-AREA
               MOVE SPACES TO FORDRE-IO-AREA
               INITIALIZE FORDRE-IO-AREA
               MOVE '399001'               TO FORDRE-IO-AREA (1:6)
               MOVE '100'                  TO FORDRE-IO-AREA (7:3)
               MOVE FNR                    TO FORDRE-IO-AREA (10:3)
               MOVE 'FAKPDFMAIL       '    TO FORDRE-IO-AREA (13:17)
               MOVE ANTPM-IO               TO FORDRE-IO-AREA (30:5)
               MOVE '000000000'            TO FORDRE-IO-AREA (35:9)
               WRITE FORDRE-IO-AREA
               MOVE SPACES TO FORDRE-IO-AREA
               INITIALIZE FORDRE-IO-AREA
               MOVE '399001'               TO FORDRE-IO-AREA (1:6)
               MOVE '100'                  TO FORDRE-IO-AREA (7:3)
               MOVE FNR                    TO FORDRE-IO-AREA (10:3)
               MOVE 'FAKEHFMAIL       '    TO FORDRE-IO-AREA (13:17)
               MOVE ANTEHF-IO              TO FORDRE-IO-AREA (30:5)
               MOVE '000000000'            TO FORDRE-IO-AREA (35:9)
               WRITE FORDRE-IO-AREA
           END-IF
           IF  (I-L4 AND I-29 AND I-46)
               MOVE SPACES TO FORDRE-IO-AREA
               INITIALIZE FORDRE-IO-AREA
               MOVE '399001'               TO FORDRE-IO-AREA (1:6)
               MOVE '100'                  TO FORDRE-IO-AREA (7:3)
               MOVE FNR                    TO FORDRE-IO-AREA (10:3)
               MOVE 'FAKFAKTURABKOPI  '    TO FORDRE-IO-AREA (13:17)
               MOVE ANTTA3-IO              TO FORDRE-IO-AREA (30:5)
               MOVE '000000000'            TO FORDRE-IO-AREA (35:9)
      *****************************************************************
      * PORTO B-POST.                                                 *
      *****************************************************************
               WRITE FORDRE-IO-AREA
           END-IF
           IF  (I-L4 AND I-29 AND NOT-I-45)
               MOVE SPACES TO FORDRE-IO-AREA
               INITIALIZE FORDRE-IO-AREA
               MOVE '399001'               TO FORDRE-IO-AREA (1:6)
               MOVE '100'                  TO FORDRE-IO-AREA (7:3)
               MOVE FNR                    TO FORDRE-IO-AREA (10:3)
               MOVE 'FAKPORTO20GRAM   '    TO FORDRE-IO-AREA (13:17)
               MOVE AS02L4-IO              TO FORDRE-IO-AREA (30:5)
               MOVE '000000000'            TO FORDRE-IO-AREA (35:9)
               WRITE FORDRE-IO-AREA
               MOVE SPACES TO FORDRE-IO-AREA
               INITIALIZE FORDRE-IO-AREA
               MOVE '399001'               TO FORDRE-IO-AREA (1:6)
               MOVE '100'                  TO FORDRE-IO-AREA (7:3)
               MOVE FNR                    TO FORDRE-IO-AREA (10:3)
               MOVE 'FAKPORTO50GRAM   '    TO FORDRE-IO-AREA (13:17)
               MOVE AS05L4-IO              TO FORDRE-IO-AREA (30:5)
               MOVE '000000000'            TO FORDRE-IO-AREA (35:9)
               WRITE FORDRE-IO-AREA
               MOVE SPACES TO FORDRE-IO-AREA
               INITIALIZE FORDRE-IO-AREA
               MOVE '399001'               TO FORDRE-IO-AREA (1:6)
               MOVE '100'                  TO FORDRE-IO-AREA (7:3)
               MOVE FNR                    TO FORDRE-IO-AREA (10:3)
               MOVE 'FAKPORTO100GRAM  '    TO FORDRE-IO-AREA (13:17)
               MOVE AS10L4-IO              TO FORDRE-IO-AREA (30:5)
               MOVE '000000000'            TO FORDRE-IO-AREA (35:9)
               WRITE FORDRE-IO-AREA
               MOVE SPACES TO FORDRE-IO-AREA
               INITIALIZE FORDRE-IO-AREA
               MOVE '399001'               TO FORDRE-IO-AREA (1:6)
               MOVE '100'                  TO FORDRE-IO-AREA (7:3)
               MOVE FNR                    TO FORDRE-IO-AREA (10:3)
               MOVE 'FAKPORTO350GRAM  '    TO FORDRE-IO-AREA (13:17)
               MOVE AS35L4-IO              TO FORDRE-IO-AREA (30:5)
               MOVE '000000000'            TO FORDRE-IO-AREA (35:9)
               WRITE FORDRE-IO-AREA
               MOVE SPACES TO FORDRE-IO-AREA
               INITIALIZE FORDRE-IO-AREA
               MOVE '399001'               TO FORDRE-IO-AREA (1:6)
               MOVE '100'                  TO FORDRE-IO-AREA (7:3)
               MOVE FNR                    TO FORDRE-IO-AREA (10:3)
               MOVE 'FAKPORTO1000GRAM '    TO FORDRE-IO-AREA (13:17)
               MOVE AS1TL4-IO              TO FORDRE-IO-AREA (30:5)
               MOVE '000000000'            TO FORDRE-IO-AREA (35:9)
               WRITE FORDRE-IO-AREA
               MOVE SPACES TO FORDRE-IO-AREA
               INITIALIZE FORDRE-IO-AREA
               MOVE '399001'               TO FORDRE-IO-AREA (1:6)
               MOVE '100'                  TO FORDRE-IO-AREA (7:3)
               MOVE FNR                    TO FORDRE-IO-AREA (10:3)
               MOVE 'FAKPORTO2000GRAM '    TO FORDRE-IO-AREA (13:17)
               MOVE AS2TL4-IO              TO FORDRE-IO-AREA (30:5)
               MOVE '000000000'            TO FORDRE-IO-AREA (35:9)
      *****************************************************************
      * PORTO A-POST.                                                 *
      *****************************************************************
               WRITE FORDRE-IO-AREA
           END-IF
           IF  (I-L4 AND I-29 AND I-45)
               MOVE SPACES TO FORDRE-IO-AREA
               INITIALIZE FORDRE-IO-AREA
               MOVE '399001'               TO FORDRE-IO-AREA (1:6)
               MOVE '100'                  TO FORDRE-IO-AREA (7:3)
               MOVE FNR                    TO FORDRE-IO-AREA (10:3)
               MOVE 'FAKPORTO20GRAMA  '    TO FORDRE-IO-AREA (13:17)
               MOVE AS02L4-IO              TO FORDRE-IO-AREA (30:5)
               MOVE '000000000'            TO FORDRE-IO-AREA (35:9)
               WRITE FORDRE-IO-AREA
               MOVE SPACES TO FORDRE-IO-AREA
               INITIALIZE FORDRE-IO-AREA
               MOVE '399001'               TO FORDRE-IO-AREA (1:6)
               MOVE '100'                  TO FORDRE-IO-AREA (7:3)
               MOVE FNR                    TO FORDRE-IO-AREA (10:3)
               MOVE 'FAKPORTO50GRAMA  '    TO FORDRE-IO-AREA (13:17)
               MOVE AS05L4-IO              TO FORDRE-IO-AREA (30:5)
               MOVE '000000000'            TO FORDRE-IO-AREA (35:9)
               WRITE FORDRE-IO-AREA
               MOVE SPACES TO FORDRE-IO-AREA
               INITIALIZE FORDRE-IO-AREA
               MOVE '399001'               TO FORDRE-IO-AREA (1:6)
               MOVE '100'                  TO FORDRE-IO-AREA (7:3)
               MOVE FNR                    TO FORDRE-IO-AREA (10:3)
               MOVE 'FAKPORTO100GRAMA '    TO FORDRE-IO-AREA (13:17)
               MOVE AS10L4-IO              TO FORDRE-IO-AREA (30:5)
               MOVE '000000000'            TO FORDRE-IO-AREA (35:9)
               WRITE FORDRE-IO-AREA
               MOVE SPACES TO FORDRE-IO-AREA
               INITIALIZE FORDRE-IO-AREA
               MOVE '399001'               TO FORDRE-IO-AREA (1:6)
               MOVE '100'                  TO FORDRE-IO-AREA (7:3)
               MOVE FNR                    TO FORDRE-IO-AREA (10:3)
               MOVE 'FAKPORTO350GRAMA '    TO FORDRE-IO-AREA (13:17)
               MOVE AS35L4-IO              TO FORDRE-IO-AREA (30:5)
               MOVE '000000000'            TO FORDRE-IO-AREA (35:9)
               WRITE FORDRE-IO-AREA
               MOVE SPACES TO FORDRE-IO-AREA
               INITIALIZE FORDRE-IO-AREA
               MOVE '399001'               TO FORDRE-IO-AREA (1:6)
               MOVE '100'                  TO FORDRE-IO-AREA (7:3)
               MOVE FNR                    TO FORDRE-IO-AREA (10:3)
               MOVE 'FAKPORTO1000GRAMA'    TO FORDRE-IO-AREA (13:17)
               MOVE AS1TL4-IO              TO FORDRE-IO-AREA (30:5)
               MOVE '000000000'            TO FORDRE-IO-AREA (35:9)
               WRITE FORDRE-IO-AREA
               MOVE SPACES TO FORDRE-IO-AREA
               INITIALIZE FORDRE-IO-AREA
               MOVE '399001'               TO FORDRE-IO-AREA (1:6)
               MOVE '100'                  TO FORDRE-IO-AREA (7:3)
               MOVE FNR                    TO FORDRE-IO-AREA (10:3)
               MOVE 'FAKPORTO2000GRAMA'    TO FORDRE-IO-AREA (13:17)
               MOVE AS2TL4-IO              TO FORDRE-IO-AREA (30:5)
               MOVE '000000000'            TO FORDRE-IO-AREA (35:9)
               WRITE FORDRE-IO-AREA
           END-IF
           IF  (I-L4 AND I-29)
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE '************************' TO PRF-IO-AREA (1:24)
               MOVE '************************' TO PRF-IO-AREA (25:24)
               MOVE 01                     TO PRF-BEFORE-SKIP
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE '* FIRMATOTALER FAK620 FA' TO PRF-IO-AREA (1:24)
               MOVE 'KTURAUTSKRIFT          *' TO PRF-IO-AREA (25:24)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO PRF-IO-AREA (39:8)
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE '************************' TO PRF-IO-AREA (1:24)
               MOVE '************************' TO PRF-IO-AREA (25:24)
               MOVE 3                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE ' FIRMA:'              TO PRF-IO-AREA (1:7)
               MOVE FNR                    TO PRF-IO-AREA (8:3)
               MOVE FNRNAV                 TO PRF-IO-AREA (12:30)
               MOVE ' KONSERN:'            TO PRF-IO-AREA (42:9)
               MOVE FIRMNR                 TO PRF-IO-AREA (51:3)
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE ' ANTALL FAKTURA         ' TO PRF-IO-AREA (1:24)
               MOVE ANTFA3                 TO XO-60YY9
               MOVE XO-60YY9               TO PRF-IO-AREA (25:7)
               INITIALIZE ANTFA3
               MOVE ' TOTALT FAKTURA         ' TO PRF-IO-AREA (32:24)
               MOVE ANTFB3                 TO XO-92YY9R
               MOVE XO-92YY9R              TO PRF-IO-AREA (56:15)
               INITIALIZE ANTFB3
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE ' ANTALL KREDITNOTA      ' TO PRF-IO-AREA (1:24)
               MOVE ANTKA3                 TO XO-60YY9
               MOVE XO-60YY9               TO PRF-IO-AREA (25:7)
               INITIALIZE ANTKA3
               MOVE ' TOTALT KREDITNOTA      ' TO PRF-IO-AREA (32:24)
               MOVE ANTKB3                 TO XO-92YY9R
               MOVE XO-92YY9R              TO PRF-IO-AREA (56:15)
               INITIALIZE ANTKB3
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE ' ANTALL FAKTURA/KR.NOTA ' TO PRF-IO-AREA (1:24)
               MOVE ANTTA3                 TO XO-50YY9
               MOVE XO-50YY9               TO PRF-IO-AREA (26:6)
               INITIALIZE ANTTA3
               MOVE ' TOTALT FAKTURA/KR.NOTA ' TO PRF-IO-AREA (32:24)
               MOVE ANTTB3                 TO XO-92YY9R
               MOVE XO-92YY9R              TO PRF-IO-AREA (56:15)
               INITIALIZE ANTTB3
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE ' ANTALL FAKTURA PDF-MAIL' TO PRF-IO-AREA (1:24)
               MOVE ANTPM                  TO XO-50YY9
               MOVE XO-50YY9               TO PRF-IO-AREA (26:6)
               INITIALIZE ANTPM
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE ' ANTALL FAKTURA EHF-MAIL' TO PRF-IO-AREA (1:24)
               MOVE ANTEHF                 TO XO-50YY9
               MOVE XO-50YY9               TO PRF-IO-AREA (26:6)
               INITIALIZE ANTEHF
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE ' ANTALL SIDER TIL KUNDER' TO PRF-IO-AREA (1:24)
               MOVE ANTSS3                 TO XO-60YY9
               MOVE XO-60YY9               TO PRF-IO-AREA (25:7)
               INITIALIZE ANTSS3
               MOVE ' HERAV BETALINGSDOKUMENT' TO PRF-IO-AREA (32:24)
               MOVE ANTBS3                 TO XO-60YY9
               MOVE XO-60YY9               TO PRF-IO-AREA (57:7)
               INITIALIZE ANTBS3
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE ' ANTALL SIDER TIL TOTALT' TO PRF-IO-AREA (1:24)
               MOVE ANTST3                 TO XO-60YY9
               MOVE XO-60YY9               TO PRF-IO-AREA (25:7)
               INITIALIZE ANTST3
               MOVE ' HERAV BETALINGSDOKUMENT' TO PRF-IO-AREA (32:24)
               MOVE ANTBD3                 TO XO-60YY9
               MOVE XO-60YY9               TO PRF-IO-AREA (57:7)
               INITIALIZE ANTBD3
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE ' ANTALL SIDER MED GIRO  ' TO PRF-IO-AREA (32:24)
               MOVE ANTGL4                 TO XO-60YY9
               MOVE XO-60YY9               TO PRF-IO-AREA (57:7)
               INITIALIZE ANTGL4
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE ' ANT. BREV TIL KUNDER   ' TO PRF-IO-AREA (1:24)
               MOVE ANTSK3                 TO XO-60YY9
               MOVE XO-60YY9               TO PRF-IO-AREA (25:7)
               INITIALIZE ANTSK3
               MOVE ' HERAV OVER 8 SIDER     ' TO PRF-IO-AREA (32:24)
               MOVE OVK8L4                 TO XO-50YY9
               MOVE XO-50YY9               TO PRF-IO-AREA (58:6)
               INITIALIZE OVK8L4
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE ' ANT. BREV TIL FIRMAET  ' TO PRF-IO-AREA (1:24)
               MOVE ANTSA3                 TO XO-60YY9
               MOVE XO-60YY9               TO PRF-IO-AREA (25:7)
               INITIALIZE ANTSA3
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE ' ANT. BREV   20 GRAM    ' TO PRF-IO-AREA (1:24)
               MOVE AS02L4                 TO XO-50YY9
               MOVE XO-50YY9               TO PRF-IO-AREA (26:6)
               INITIALIZE AS02L4
               MOVE ' ANT. BREV   50 GRAM    ' TO PRF-IO-AREA (32:24)
               MOVE AS05L4                 TO XO-50YY9
               MOVE XO-50YY9               TO PRF-IO-AREA (58:6)
               INITIALIZE AS05L4
               MOVE FAKTPT                 TO PRF-IO-AREA (70:1)
               MOVE '-POST.'               TO PRF-IO-AREA (71:6)
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE ' ANT. BREV  100 GRAM    ' TO PRF-IO-AREA (1:24)
               MOVE AS10L4                 TO XO-50YY9
               MOVE XO-50YY9               TO PRF-IO-AREA (26:6)
               INITIALIZE AS10L4
               MOVE ' ANT. BREV  350 GRAM    ' TO PRF-IO-AREA (32:24)
               MOVE AS35L4                 TO XO-50YY9
               MOVE XO-50YY9               TO PRF-IO-AREA (58:6)
               INITIALIZE AS35L4
               MOVE FAKTPT                 TO PRF-IO-AREA (70:1)
               MOVE '-POST.'               TO PRF-IO-AREA (71:6)
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE ' ANT. BREV 1000 GRAM    ' TO PRF-IO-AREA (1:24)
               MOVE AS1TL4                 TO XO-50YY9
               MOVE XO-50YY9               TO PRF-IO-AREA (26:6)
               INITIALIZE AS1TL4
               MOVE ' ANT. BREV 2000 GRAM    ' TO PRF-IO-AREA (32:24)
               MOVE AS2TL4                 TO XO-50YY9
               MOVE XO-50YY9               TO PRF-IO-AREA (58:6)
               INITIALIZE AS2TL4
               MOVE FAKTPT                 TO PRF-IO-AREA (70:1)
               MOVE '-POST.'               TO PRF-IO-AREA (71:6)
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE MAXLIN                 TO XO-60YY9
               MOVE XO-60YY9               TO PRF-IO-AREA (6:7)
               INITIALIZE MAXLIN
               MOVE 'VARELINJER PÅ STØRSTE FA' TO PRF-IO-AREA (14:24)
               MOVE 'KTURA. ='             TO PRF-IO-AREA (38:8)
               MOVE MAXSID                 TO XO-30YY9
               MOVE XO-30YY9               TO PRF-IO-AREA (47:3)
               INITIALIZE MAXSID
               MOVE 'SIDER.'               TO PRF-IO-AREA (51:6)
               MOVE 2                      TO PRF-BEFORE-SPACE
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE 'KUNDE ='              TO PRF-IO-AREA (15:7)
               MOVE MAXKNL                 TO PRF-IO-AREA (23:6)
               INITIALIZE MAXKNL
               MOVE 'FAKTURA ='            TO PRF-IO-AREA (31:9)
               MOVE MAXFAK                 TO PRF-IO-AREA (41:6)
               INITIALIZE MAXFAK
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE MAXANT                 TO XO-30YY9
               MOVE XO-30YY9               TO PRF-IO-AREA (10:3)
               INITIALIZE MAXANT
               MOVE 'FAKTURA PÅ STØRSTE BETAL' TO PRF-IO-AREA (14:24)
               MOVE 'INGSDOKUMENT.'        TO PRF-IO-AREA (38:13)
               MOVE 1                      TO PRF-BEFORE-SPACE
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE 'KUNDE ='              TO PRF-IO-AREA (15:7)
               MOVE MAXKNR                 TO PRF-IO-AREA (23:6)
               INITIALIZE MAXKNR
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE '************************' TO PRF-IO-AREA (1:24)
               MOVE '************************' TO PRF-IO-AREA (25:24)
               MOVE 01                     TO PRF-BEFORE-SKIP
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE '* GRANDTOTALER FAK620 FA' TO PRF-IO-AREA (1:24)
               MOVE 'KTURAUTSKRIFT          *' TO PRF-IO-AREA (25:24)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO PRF-IO-AREA (39:8)
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE '************************' TO PRF-IO-AREA (1:24)
               MOVE '************************' TO PRF-IO-AREA (25:24)
               MOVE 3                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE 'TOTALER ALLE FIRMA'   TO PRF-IO-AREA (12:18)
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE ' ANTALL FAKTURA         ' TO PRF-IO-AREA (1:24)
               MOVE ANTFAT                 TO XO-60YY9
               MOVE XO-60YY9               TO PRF-IO-AREA (25:7)
               MOVE ' TOTALT FAKTURA         ' TO PRF-IO-AREA (32:24)
               MOVE ANTFBT                 TO XO-92YY9R
               MOVE XO-92YY9R              TO PRF-IO-AREA (56:15)
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE ' ANTALL KREDITNOTA      ' TO PRF-IO-AREA (1:24)
               MOVE ANTKAT                 TO XO-60YY9
               MOVE XO-60YY9               TO PRF-IO-AREA (25:7)
               MOVE ' TOTALT KREDITNOTA      ' TO PRF-IO-AREA (32:24)
               MOVE ANTKBT                 TO XO-92YY9R
               MOVE XO-92YY9R              TO PRF-IO-AREA (56:15)
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE ' ANTALL FAKTURA/KR.NOTA ' TO PRF-IO-AREA (1:24)
               MOVE ANTTAT                 TO XO-60YY9
               MOVE XO-60YY9               TO PRF-IO-AREA (25:7)
               MOVE ' TOTALT FAKTURA/KR.NOTA ' TO PRF-IO-AREA (32:24)
               MOVE ANTTBT                 TO XO-92YY9R
               MOVE XO-92YY9R              TO PRF-IO-AREA (56:15)
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE ' ANTALL SIDER TIL KUNDER' TO PRF-IO-AREA (1:24)
               MOVE ANTSST                 TO XO-60YY9
               MOVE XO-60YY9               TO PRF-IO-AREA (25:7)
               MOVE ' HERAV BETALINGSDOKUMENT' TO PRF-IO-AREA (32:24)
               MOVE ANTBST                 TO XO-60YY9
               MOVE XO-60YY9               TO PRF-IO-AREA (57:7)
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE ' ANTALL SIDER TIL TOTALT' TO PRF-IO-AREA (1:24)
               MOVE ANTSTT                 TO XO-60YY9
               MOVE XO-60YY9               TO PRF-IO-AREA (25:7)
               MOVE ' HERAV BETALINGSDOKUMENT' TO PRF-IO-AREA (32:24)
               MOVE ANTBDT                 TO XO-60YY9
               MOVE XO-60YY9               TO PRF-IO-AREA (57:7)
               IF  (I-U4)
                   MOVE '+ ARKIVKOPI'      TO PRF-IO-AREA (66:11)
               END-IF
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE ' ANTALL SIDER MED GIRO  ' TO PRF-IO-AREA (32:24)
               MOVE ANTGLR                 TO XO-60YY9
               MOVE XO-60YY9               TO PRF-IO-AREA (57:7)
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE ' ANT. BREV TIL KUNDER   ' TO PRF-IO-AREA (1:24)
               MOVE ANTSKT                 TO XO-60YY9
               MOVE XO-60YY9               TO PRF-IO-AREA (25:7)
               MOVE ' HERAV OVER 8 SIDER     ' TO PRF-IO-AREA (32:24)
               MOVE OVK8LR                 TO XO-60YY9
               MOVE XO-60YY9               TO PRF-IO-AREA (57:7)
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE ' ANT. BREV TIL FIRMAET  ' TO PRF-IO-AREA (1:24)
               MOVE ANTSAT                 TO XO-60YY9
               MOVE XO-60YY9               TO PRF-IO-AREA (25:7)
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE ' ANT. BREV   20 GRAM    ' TO PRF-IO-AREA (1:24)
               MOVE AS02LR                 TO XO-60YY9
               MOVE XO-60YY9               TO PRF-IO-AREA (25:7)
               MOVE ' ANT. BREV   50 GRAM    ' TO PRF-IO-AREA (32:24)
               MOVE AS05LR                 TO XO-60YY9
               MOVE XO-60YY9               TO PRF-IO-AREA (57:7)
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE ' ANT. BREV  100 GRAM    ' TO PRF-IO-AREA (1:24)
               MOVE AS10LR                 TO XO-60YY9
               MOVE XO-60YY9               TO PRF-IO-AREA (25:7)
               MOVE ' ANT. BREV  350 GRAM    ' TO PRF-IO-AREA (32:24)
               MOVE AS35LR                 TO XO-60YY9
               MOVE XO-60YY9               TO PRF-IO-AREA (57:7)
               MOVE 1                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE ' ANT. BREV 1000 GRAM    ' TO PRF-IO-AREA (1:24)
               MOVE AS1TLR                 TO XO-60YY9
               MOVE XO-60YY9               TO PRF-IO-AREA (25:7)
               MOVE ' ANT. BREV 2000 GRAM    ' TO PRF-IO-AREA (32:24)
               MOVE AS2TLR                 TO XO-60YY9
               MOVE XO-60YY9               TO PRF-IO-AREA (57:7)
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE ' ANT. FIRMA TOTALT      ' TO PRF-IO-AREA (1:24)
               MOVE ANTFIR                 TO XO-30YY9
               MOVE XO-30YY9               TO PRF-IO-AREA (29:3)
               MOVE ' ANT. FIRMA MED FAKTURA ' TO PRF-IO-AREA (32:24)
               MOVE ANTFIF                 TO XO-30YY9
               MOVE XO-30YY9               TO PRF-IO-AREA (61:3)
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
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
           PERFORM FAKTAB-LOAD
           SET FMAST-LEVEL-INIT            TO TRUE
           INITIALIZE FMAST-DATA-FIELDS
           SET FMAST-EOF-OFF               TO TRUE
           SET FMAST-PROCESS               TO TRUE
           OPEN INPUT FMAST
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN I-O FIRMAF
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           INITIALIZE KUNDEMX-DATA-FIELDS
           OPEN INPUT KUNDEMX
           INITIALIZE STATTAB-DATA-FIELDS
           OPEN INPUT STATTAB
           OPEN OUTPUT EFAKT
           OPEN OUTPUT PRF
           INITIALIZE PRF-IO-AREA
           INITIALIZE PRF-DATA-FIELDS
           MOVE 57                         TO PRF-MAX-LINES
           OPEN OUTPUT KIDRELF
           OPEN OUTPUT FBKOPI
           OPEN OUTPUT FORDRE.
           SET TABFNR-I                    TO 1
           PERFORM VARYING AR1-I FROM 1 BY 1
                     UNTIL AR1-I > AR1-MAX
               INITIALIZE AR1 (AR1-I)
           END-PERFORM
           SET AR1-I                       TO 1
           PERFORM VARYING AR2-I FROM 1 BY 1
                     UNTIL AR2-I > AR2-MAX
               INITIALIZE AR2 (AR2-I)
           END-PERFORM
           SET AR2-I                       TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FMAST
           CLOSE FIRMAF
           CLOSE KUNDEMA
           CLOSE KUNDEMX
           CLOSE STATTAB
           CLOSE EFAKT
           IF PRF-IO-AREA NOT = SPACES
             WRITE PRF-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO PRF-IO-AREA
           END-IF
           CLOSE PRF
           CLOSE KIDRELF
           CLOSE FBKOPI
           CLOSE FORDRE.
 
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
