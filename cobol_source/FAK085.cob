       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK085R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM: FAK085                                        *
      *  DANNER FAKTURA-PRINTFILE,REGSKAP,RESKONTRO,FAKTNRFILE  *
      *  3.02.00 KONSERN FIRMANR. BENYTTES MOT KUNDE.MASTER.    *
      * 11.04.00 RETTET INFORMASJONSTEKST PÅ OPPKRAV.           *
      * 11.04.00 RETTET FORFALLSDATO TIL FAKTURADATO PÅ OPPKRAV.*
      * 05.05.00 2 NYE AVD. SUMMER PÅ HAFNOR / HYDRO TEXACO.    *
      * 19.05.00 KREDITNOTA SAMME FORF SOM FAKTURA 973. MT.     *
      * 29/05-00 NYE PORTOSATSER FOR 1999. KUN A-POST.          *
      * 31/07-00 UTVIDET TIL 9 AVD.TOTALER PÅ SHELL             *
      * 30/08-00 FORFALL PÅ BM.14 ENDRET FRA 30 DAGER TIL FAKT.DATO
      *  5/10-00 KUNDEREF LAGT INN I FAKTURA-EDI FILE.          *
      *  2/ 1-01 SPESIALLØSNING VEDR. KONSERN FOR BILVAREHUSENE *
      *  2/ 1-01 SAMT HENTER GAMMEL MOMS PR. 31.12.2000         *
      *  3/01-01 NYE PORTOSATSER FOR 2001. A OG B -POST.        *
      *  5/01-01 FJERNET SPESIALLØSNING.                        *
      * 12/06-01 ENDRER IKKE BM. PÅ KR.NOTA OM KUNDEN HAR 07/14 *
      *  2/07-01 NYE PORTOSATSER.                               *
      * 15/10-01 ØKER REC.LENGDE I EDIFAKF FRA 100 TIL 150      *
      *          FOR Å LEGGE INN VAREBETEGNELSE.                *
      * 26/11-01 LAGT INN KONSERFIRMANR. I KIDREL.FILE          *
      *  7/12-01 FJERNET SVENSK TEKST OG MOMS.                  *
      * 12/07-02 TEST PÅ FORFALLSDATO KRDITNOTA FRA FIRMAFILE.  *
      * 12/03-02 NYE SUMMERINGSFELT FOR ESSO OG SHELL.          *
      * 13/03-02 NYE SUMMERINGSFELT FOR ESSO.                   *
      * 01/04-02 SUMTOTALER FOR ESSO SKAL VÆRE MED MVA.         *
      * 15/07-03 NY RUTINE FOR UTREGNING AV MVA U/AVR.          *
      * 03/09-03 POSTERING AV AVRUNDING PÅ EGEN KONTO.          *
      * 03/09-03 FJERNET RUTINER FOR UTREGNING AV PORTO.        *
      * 21/01-04 SUBTOTAL C FOR HAFNOR (KAFFEAVTALEN)           *
      * 01/01-05 RETTET MOMSSATS FRA 24% TIL 25%.               *
      * 01/02-05 LAGT INN TEKST: RENTENOTA OG PURREGEBYR.       *
      * 09/11-05 RENTE/GEBYR NOTA TILDELES AVGIFTSKODE 0.       *
      * 11/01-06 KAFFEAVTALEN 2006 ER LAGT INN I GRUPPESUM.     *
      *                       DET MÅ LEGGES INN EDBNR. HVERT ÅR.*
      * 10.02.2006  KAFFEAVTALEN TESTER NÅ PÅ VGR. 74000        *
      * 14.02.2006  NY GRUPPESUM. D=MILJØBENSIN HAFNOR. VG.71500*
      * 27.03.2006  GRUPPETOTALER PÅ BEST-STASJONER.FINA FJERNET*
      * 26.01.2012  Fjernet test på firma 986                 en*
      * 28.09.2016  NY AVG.KODE, BRUKER NY INDIKATOR=18       BH*
      ***********************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK085.rpg
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
           SELECT AVDTAB
               ASSIGN TO UT-S-AVDTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS AVDTAB-STATUS.
           SELECT FAKSGR
               ASSIGN TO UT-S-FAKSGR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKSGR-STATUS.
           SELECT FAKESS
               ASSIGN TO UT-S-FAKESS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKESS-STATUS.
           SELECT FAKTNR
               ASSIGN TO UT-S-FAKTNR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKTNR-STATUS.
           SELECT FAKPAR
               ASSIGN TO UT-S-FAKPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKPAR-STATUS.
           SELECT FAKTIN
               ASSIGN TO UT-S-FAKTIN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKTIN-STATUS.
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
           SELECT TEKSTF
               ASSIGN TO TEKSTF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS TEKSTF-STATUS
               RECORD KEY IS TEKSTF-KEY1.
           SELECT ORDFAK
               ASSIGN TO UT-S-ORDFAK
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ORDFAK-STATUS.
           SELECT RESFILE
               ASSIGN TO UT-S-RESFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RESFILE-STATUS.
           SELECT KIDRELF
               ASSIGN TO UT-S-KIDRELF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KIDRELF-STATUS.
           SELECT REGFILE
               ASSIGN TO UT-S-REGFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS REGFILE-STATUS.
           SELECT EDIFAKF
               ASSIGN TO UT-S-EDIFAKF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS EDIFAKF-STATUS.
           SELECT PRTFILE
               ASSIGN TO PRTFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PRTFILE-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD AVDTAB
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  AVDTAB-IO-AREA.
           05  AVDTAB-IO-AREA-X            PICTURE X(80).
       FD FAKSGR
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  FAKSGR-IO-AREA.
           05  FAKSGR-IO-AREA-X            PICTURE X(80).
       FD FAKESS
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  FAKESS-IO-AREA.
           05  FAKESS-IO-AREA-X            PICTURE X(80).
       FD FAKTNR
               BLOCK CONTAINS 1050
               RECORD CONTAINS 50.
       01  FAKTNR-IO-AREA.
           05  FAKTNR-IO-AREA-X            PICTURE X(50).
       FD FAKPAR
               BLOCK CONTAINS 200
               RECORD CONTAINS 200.
       01  FAKPAR-IO-AREA.
           05  FAKPAR-IO-AREA-X            PICTURE X(200).
       FD FAKTIN
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  FAKTIN-IO-AREA.
           05  FAKTIN-IO-AREA-X            PICTURE X(200).
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
       FD TEKSTF
               RECORD CONTAINS 800.
       01  TEKSTF-IO-AREA.
           05  TEKSTF-IO-AREA-X.
               10  TEKSTF-KEY1             PICTURE X(4).
               10  FILLER                  PICTURE X(796).
       FD ORDFAK
               BLOCK CONTAINS 40
               RECORD CONTAINS 20.
       01  ORDFAK-IO-AREA.
           05  ORDFAK-IO-AREA-X            PICTURE X(20).
       FD RESFILE
               BLOCK CONTAINS 140
               RECORD CONTAINS 70.
       01  RESFILE-IO-AREA.
           05  RESFILE-IO-AREA-X           PICTURE X(70).
       FD KIDRELF
               BLOCK CONTAINS 80
               RECORD CONTAINS 40.
       01  KIDRELF-IO-AREA.
           05  KIDRELF-IO-AREA-X           PICTURE X(40).
       FD REGFILE
               BLOCK CONTAINS 160
               RECORD CONTAINS 80.
       01  REGFILE-IO-AREA.
           05  REGFILE-IO-AREA-X           PICTURE X(80).
       FD EDIFAKF
               BLOCK CONTAINS 300
               RECORD CONTAINS 150.
       01  EDIFAKF-IO-AREA.
           05  EDIFAKF-IO-AREA-X           PICTURE X(150).
       FD PRTFILE
               RECORD CONTAINS 146.
       01  PRTFILE-IO-AREA.
           05  PRTFILE-IO-AREA-X           PICTURE X(146).
       FD LISTE
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  LISTE-IO-PRINT.
           05  LISTE-IO-AREA-CONTROL       PICTURE X VALUE ' '.
        02 LISTE-IO-AREA.
           05  LISTE-IO-AREA-X             PICTURE X(132).
       WORKING-STORAGE SECTION.
       77  TABAVD-MAX   VALUE 40           PICTURE 9(4) USAGE BINARY.
       77  TABAVN-MAX   VALUE 40           PICTURE 9(4) USAGE BINARY.
       77  TABFVG-MAX   VALUE 300          PICTURE 9(4) USAGE BINARY.
       77  TABFSG-MAX   VALUE 300          PICTURE 9(4) USAGE BINARY.
       77  TABFVE-MAX   VALUE 300          PICTURE 9(4) USAGE BINARY.
       77  TABFSE-MAX   VALUE 300          PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  TABAVD-TABLE.
               10  TABAVD-ENTRY
                                           OCCURS 40 TIMES
                                           INDEXED BY TABAVD-I
                                                      TABAVD-S
                                                      TABAVN-I
                                                      TABAVN-S.
                   15  TABAVD              PICTURE X(4).
                   15  TABAVN              PICTURE X(42).
           05  TABFVG-TABLE.
               10  TABFVG-ENTRY
                                           OCCURS 300 TIMES
                                           INDEXED BY TABFVG-I
                                                      TABFVG-S
                                                      TABFSG-I
                                                      TABFSG-S.
                   15  TABFVG              PICTURE X(8).
                   15  TABFSG              PICTURE X(1).
           05  TABFVE-TABLE.
               10  TABFVE-ENTRY
                                           OCCURS 300 TIMES
                                           INDEXED BY TABFVE-I
                                                      TABFVE-S
                                                      TABFSE-I
                                                      TABFSE-S.
                   15  TABFVE              PICTURE X(5).
                   15  TABFSE              PICTURE X(1).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  AVDTAB-STATUS               PICTURE 99 VALUE 0.
           10  FAKSGR-STATUS               PICTURE 99 VALUE 0.
           10  FAKESS-STATUS               PICTURE 99 VALUE 0.
           10  FAKTNR-STATUS               PICTURE 99 VALUE 0.
           10  FAKPAR-STATUS               PICTURE 99 VALUE 0.
           10  FAKTIN-STATUS               PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  KUNDEMX-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  TEKSTF-STATUS               PICTURE 99 VALUE 0.
           10  ORDFAK-STATUS               PICTURE 99 VALUE 0.
           10  RESFILE-STATUS              PICTURE 99 VALUE 0.
           10  KIDRELF-STATUS              PICTURE 99 VALUE 0.
           10  REGFILE-STATUS              PICTURE 99 VALUE 0.
           10  EDIFAKF-STATUS              PICTURE 99 VALUE 0.
           10  PRTFILE-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  BMFELT-XX-STATUS            PICTURE 99 VALUE 0.
           10  MVFELT-XX-STATUS            PICTURE 99 VALUE 0.
           10  KIFELT-XX-STATUS            PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  AVDTAB-EOF-OFF          VALUE '0'.
               88  AVDTAB-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKSGR-EOF-OFF          VALUE '0'.
               88  FAKSGR-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKESS-EOF-OFF          VALUE '0'.
               88  FAKESS-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKTIN-EOF-OFF          VALUE '0'.
               88  FAKTIN-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKTIN-READ-OFF         VALUE '0'.
               88  FAKTIN-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKTIN-PROCESS-OFF      VALUE '0'.
               88  FAKTIN-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  FAKTIN-LEVEL-INIT-OFF   VALUE '0'.
               88  FAKTIN-LEVEL-INIT       VALUE '1'.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  KUNDEMX-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  TEKSTF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
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
           05  BMFELT-XX-DATA-FIELDS.
               10  BETMAT                  PICTURE X(2).
               10  FILLER                  PICTURE X(31).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(2).
               10  BMTKST                  PICTURE X(24).
               10  FILLER                  PICTURE X(7).
           05  MVFELT-XX REDEFINES BMFELT-XX-DATA-FIELDS.
               10  BUMVA-IO.
                   15  BUMVA               PICTURE S9(9)V9(2).
               10  FILLER                  PICTURE X(22).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(11).
               10  BMMVA-IO.
                   15  BMMVA               PICTURE S9(9)V9(2).
               10  FILLER                  PICTURE X(11).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(22).
               10  MVA-IO.
                   15  MVA                 PICTURE S9(9)V9(2).
           05  KIFELT-XX REDEFINES BMFELT-XX-DATA-FIELDS.
               10  KITALL                  PICTURE X(25).
               10  FILLER                  PICTURE X(8).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(25).
               10  KITYPE                  PICTURE X(1).
               10  FILLER                  PICTURE X(7).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(26).
               10  KIKTR                   PICTURE X(1).
               10  FILLER                  PICTURE X(6).
      *DSDS: DATA STRUCTURE FIELDS
           05  MVFELT-XX-DATA-FIELDS.
               10  BUMVA-IO.
                   15  BUMVA               PICTURE S9(9)V9(2).
               10  FILLER                  PICTURE X(22).
           05  FILLER REDEFINES MVFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(11).
               10  BMMVA-IO.
                   15  BMMVA               PICTURE S9(9)V9(2).
               10  FILLER                  PICTURE X(11).
           05  FILLER REDEFINES MVFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(22).
               10  MVA-IO.
                   15  MVA                 PICTURE S9(9)V9(2).
           05  KIFELT-XX REDEFINES MVFELT-XX-DATA-FIELDS.
               10  KITALL                  PICTURE X(25).
               10  FILLER                  PICTURE X(8).
           05  FILLER REDEFINES MVFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(25).
               10  KITYPE                  PICTURE X(1).
               10  FILLER                  PICTURE X(7).
           05  FILLER REDEFINES MVFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(26).
               10  KIKTR                   PICTURE X(1).
               10  FILLER                  PICTURE X(6).
      *DSDS: DATA STRUCTURE FIELDS
           05  KIFELT-XX-DATA-FIELDS.
               10  KITALL                  PICTURE X(25).
               10  FILLER                  PICTURE X(2).
           05  FILLER REDEFINES KIFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(25).
               10  KITYPE                  PICTURE X(1).
               10  KIKTR                   PICTURE X(1).
           05  FAKTNR-DATA-FIELDS.
               10  FIRMN4                  PICTURE X(3).
               10  PFANR1-IO.
                   15  PFANR1              PICTURE S9(6).
               10  PKRNR1-IO.
                   15  PKRNR1              PICTURE S9(6).
           05  FAKPAR-DATA-FIELDS.
               10  PFADTO                  PICTURE X(6).
               10  PFDAG                   PICTURE X(2).
               10  PFMND                   PICTURE X(2).
               10  PFAAR                   PICTURE X(2).
               10  PFF15                   PICTURE X(6).
               10  PFF30                   PICTURE X(6).
               10  PFF45                   PICTURE X(6).
               10  PFF60                   PICTURE X(6).
               10  PFF90                   PICTURE X(6).
               10  PFF75                   PICTURE X(6).
               10  FO-ELGRSTE              PICTURE X(1).
               10  FAKTOM                  PICTURE X(2).
               10  UFADTO                  PICTURE X(6).
               10  UFDAG                   PICTURE X(2).
               10  UFMND                   PICTURE X(2).
               10  UFAAR                   PICTURE X(2).
               10  UFF15                   PICTURE X(6).
               10  UFF30                   PICTURE X(6).
               10  UFF45                   PICTURE X(6).
               10  UFF60                   PICTURE X(6).
               10  UFF90                   PICTURE X(6).
               10  UFF75                   PICTURE X(6).
               10  UFF105                  PICTURE X(6).
               10  UFF120                  PICTURE X(6).
               10  UFF150                  PICTURE X(6).
               10  UFF180                  PICTURE X(6).
               10  PFF105                  PICTURE X(6).
               10  PFF120                  PICTURE X(6).
               10  PFF150                  PICTURE X(6).
               10  PFF180                  PICTURE X(6).
           05  FAKTIN-LEVEL-02.
               10  FAKTIN-02-L5.
                   15  FAKTIN-02-L5-FIRMNR PICTURE X(3).
               10  FAKTIN-02-L4.
                   15  FAKTIN-02-L4-FAKRFL PICTURE X(2).
               10  FAKTIN-02-L3.
                   15  FAKTIN-02-L3-RESKNR PICTURE X(6).
               10  FAKTIN-02-L2.
                   15  FAKTIN-02-L2-FAKMTE PICTURE X(1).
                   15  FAKTIN-02-L2-AVGKOD PICTURE X(1).
                   15  FAKTIN-02-L2-BTMA-ELGTE PICTURE X(2).
                   15  FAKTIN-02-L2-BRKOD  PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  FAKTIN-02-L1.
                   15  FAKTIN-02-L1-ONR    PICTURE X(6).
           05  FAKTIN-DATA-FIELDS.
               10  FIRMNR                  PICTURE X(3).
               10  KTOKL                   PICTURE X(1).
               10  RESKNR                  PICTURE X(6).
               10  FAKRNR                  PICTURE X(1).
               10  FAKRFL                  PICTURE X(2).
               10  FAKMTE                  PICTURE X(1).
               10  AVGKOD                  PICTURE X(1).
               10  FLSIDE                  PICTURE X(1).
               10  BTMA-ELGTE              PICTURE X(2).
               10  BRKOD-IO.
                   15  BRKOD               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  ONR                     PICTURE X(6).
               10  RA                      PICTURE X(1).
               10  LINSEQ                  PICTURE X(4).
               10  GJFAKT                  PICTURE X(1).
               10  KUREF                   PICTURE X(15).
               10  VARAD1                  PICTURE X(30).
               10  FAKREF                  PICTURE X(6).
               10  LAGERK                  PICTURE X(2).
               10  BK                      PICTURE X(1).
               10  FRITT                   PICTURE X(1).
               10  HDIST                   PICTURE X(3).
               10  VGR                     PICTURE X(5).
               10  VGRAVD                  PICTURE X(1).
               10  VARAD2                  PICTURE X(30).
               10  ARTNR                   PICTURE X(20).
               10  VARBET                  PICTURE X(30).
               10  VARB20                  PICTURE X(20).
               10  LEVENH-IO.
                   15  LEVENH              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  EDBNR                   PICTURE X(7).
               10  EDB3F                   PICTURE X(3).
               10  EDB2F                   PICTURE X(2).
               10  RAB1-IO.
                   15  RAB1                PICTURE S9(2)V9(1).
               10  RAB2-IO.
                   15  RAB2                PICTURE S9(2)V9(1).
               10  RAB3-IO.
                   15  RAB3                PICTURE S9(2)V9(1).
               10  ENHPR-IO.
                   15  ENHPR               PICTURE S9(7)V9(2).
               10  REGAVD                  PICTURE X(1).
               10  KOSTPR-IO.
                   15  KOSTPR              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  RENGEB                  PICTURE X(1).
               10  KUNDNR                  PICTURE X(6).
               10  ALFKOD                  PICTURE X(3).
           05  KUNDEMA-DATA-FIELDS.
               10  CNAVN1                  PICTURE X(30).
               10  CNAVN2                  PICTURE X(30).
               10  CADR                    PICTURE X(30).
               10  CPOST                   PICTURE X(15).
               10  CPNR                    PICTURE X(4).
               10  CPNR3                   PICTURE X(3).
               10  RESGRP                  PICTURE X(2).
               10  CBETM                   PICTURE X(2).
               10  KJOPT-IO.
                   15  KJOPT               PICTURE S9(9)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  KUNKAT-IO.
                   15  KUNKAT              PICTURE S9(3) USAGE
                                                       PACKED-DECIMAL.
               10  RENTE                   PICTURE X(1).
               10  BELFAK                  PICTURE X(1).
               10  HDIST1                  PICTURE X(1).
           05  KUNDEMX-DATA-FIELDS.
               10  KKAT                    PICTURE X(3).
               10  EANLOC                  PICTURE X(13).
           05  FIRMAF-DATA-FIELDS.
               10  KONFNR                  PICTURE X(3).
               10  RNTPRO-IO.
                   15  RNTPRO              PICTURE S9(1)V9(2).
               10  RNTPRA-IO.
                   15  RNTPRA              PICTURE S9(1)V9(2).
               10  FFKODE                  PICTURE X(1).
               10  PRIFNR                  PICTURE X(1).
               10  KRFFD                   PICTURE X(1).
               10  KTOAVR                  PICTURE X(4).
               10  FTAKNR-IO.
                   15  FTAKNR              PICTURE S9(9) USAGE
                                                       PACKED-DECIMAL.
           05  TEKSTF-DATA-FIELDS.
               10  FTEKS1                  PICTURE X(100).
               10  FTEKS2                  PICTURE X(100).
      *
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L5            PICTURE X(3).
               10  THE-PRIOR-L4            PICTURE X(2).
               10  THE-PRIOR-L3            PICTURE X(6).
               10  THE-PRIOR-L2            PICTURE X(7).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  BRKODE-IO.
                   15  BRKODE              PICTURE S9(6).
               10  ANTREC-IO.
                   15  ANTREC              PICTURE S9(6).
               10  SEQ-IO.
                   15  SEQ                 PICTURE S9(6).
               10  NFADTO-IO.
                   15  NFADTO              PICTURE S9(6).
               10  AREA1-IO.
                   15  AREA1               PICTURE S9(6).
               10  AREA2-IO.
                   15  AREA2               PICTURE S9(6).
               10  KFIRNR                  PICTURE X(3).
               10  KEYT                    PICTURE X(4).
               10  RESKEY                  PICTURE X(9).
               10  KFEIL-IO.
                   15  KFEIL               PICTURE S9(9).
               10  FNRKNR                  PICTURE X(9).
               10  FELT16                  PICTURE X(16).
               10  FELT14                  PICTURE X(14).
               10  FELT30                  PICTURE X(30).
               10  KJBEL-IO.
                   15  KJBEL               PICTURE S9(9).
               10  RESKEX                  PICTURE X(10).
               10  FOFALL                  PICTURE X(6).
               10  BTOPRI-IO.
                   15  BTOPRI              PICTURE S9(7)V9(2).
               10  RABX1-IO.
                   15  RABX1               PICTURE S9(9)V9(2).
               10  RABAT1-IO.
                   15  RABAT1              PICTURE S9(7)V9(3).
               10  BRTO1-IO.
                   15  BRTO1               PICTURE S9(7)V9(2).
               10  RABAT2-IO.
                   15  RABAT2              PICTURE S9(7)V9(3).
               10  BRTO2-IO.
                   15  BRTO2               PICTURE S9(7)V9(2).
               10  RABAT3-IO.
                   15  RABAT3              PICTURE S9(7)V9(3).
               10  NETTO-IO.
                   15  NETTO               PICTURE S9(7)V9(2).
               10  NETBNR-IO.
                   15  NETBNR              PICTURE S9(7)V9(2).
               10  SUMSP-IO.
                   15  SUMSP               PICTURE S9(7)V9(2).
               10  KEYSG                   PICTURE X(8).
               10  SGRAVD                  PICTURE X(1).
               10  STAVD1-IO.
                   15  STAVD1              PICTURE S9(7)V9(2).
               10  STAVD2-IO.
                   15  STAVD2              PICTURE S9(7)V9(2).
               10  STAVD3-IO.
                   15  STAVD3              PICTURE S9(7)V9(2).
               10  STAVD4-IO.
                   15  STAVD4              PICTURE S9(7)V9(2).
               10  STAVD5-IO.
                   15  STAVD5              PICTURE S9(7)V9(2).
               10  STAVD6-IO.
                   15  STAVD6              PICTURE S9(7)V9(2).
               10  STAVD7-IO.
                   15  STAVD7              PICTURE S9(7)V9(2).
               10  STAVD8-IO.
                   15  STAVD8              PICTURE S9(7)V9(2).
               10  STAVD9-IO.
                   15  STAVD9              PICTURE S9(7)V9(2).
               10  STAVDA-IO.
                   15  STAVDA              PICTURE S9(7)V9(2).
               10  STAVDB-IO.
                   15  STAVDB              PICTURE S9(7)V9(2).
               10  STAVDC-IO.
                   15  STAVDC              PICTURE S9(7)V9(2).
               10  VGRESS                  PICTURE X(5).
               10  ESSAVD                  PICTURE X(1).
               10  STAVDD-IO.
                   15  STAVDD              PICTURE S9(7)V9(2).
               10  PRFLT4                  PICTURE X(3).
               10  PRFLT5                  PICTURE X(9).
               10  PRARE1-IO.
                   15  PRARE1              PICTURE S9(6).
               10  FAKKID                  PICTURE X(7).
               10  FFAAR                   PICTURE X(2).
               10  FFREST                  PICTURE X(4).
               10  FFMND                   PICTURE X(2).
               10  FFDAG                   PICTURE X(2).
               10  SIGNX1                  PICTURE X(1).
               10  BXBEL-IO.
                   15  BXBEL               PICTURE S9(7)V9(2).
               10  AREA8X-IO.
                   15  AREA8X              PICTURE S9(6).
               10  SEQ2-IO.
                   15  SEQ2                PICTURE S9(2).
               10  STAVD-IO.
                   15  STAVD               PICTURE S9(7)V9(2).
               10  A1                      PICTURE X(4).
               10  AVRORE-IO.
                   15  AVRORE              PICTURE S9(3)V9(2).
               10  MOMSAR-IO.
                   15  MOMSAR              PICTURE S9(7)V9(2).
               10  MOMS-IO.
                   15  MOMS                PICTURE S9(7)V9(2).
               10  MOMSX1-IO.
                   15  MOMSX1              PICTURE S9(7)V9(2).
               10  SIGNXM                  PICTURE X(1).
               10  TOTMOM-IO.
                   15  TOTMOM              PICTURE S9(8)V9(2).
               10  SIGNAV                  PICTURE X(1).
               10  TOTAVR-IO.
                   15  TOTAVR              PICTURE S9(8)V9(2).
               10  TOTAF5-IO.
                   15  TOTAF5              PICTURE S9(6)V9(2).
               10  TOTAL-IO.
                   15  TOTAL               PICTURE S9(8)V9(2).
               10  KRESUM-IO.
                   15  KRESUM              PICTURE S9(9)V9(2).
               10  FAKSUM-IO.
                   15  FAKSUM              PICTURE S9(9)V9(2).
               10  AREA9X-IO.
                   15  AREA9X              PICTURE S9(6).
               10  BYBEL-IO.
                   15  BYBEL               PICTURE S9(7)V9(2).
               10  TOTSUM-IO.
                   15  TOTSUM              PICTURE S9(9)V9(2).
               10  OREDF1-IO.
                   15  OREDF1              PICTURE S9(7)V9(2).
               10  ANTL2-IO.
                   15  ANTL2               PICTURE S9(5).
               10  ANTLR-IO.
                   15  ANTLR               PICTURE S9(5).
               10  PRARE2-IO.
                   15  PRARE2              PICTURE S9(6).
               10  ANTFAK-IO.
                   15  ANTFAK              PICTURE S9(6).
               10  TOTAFA-IO.
                   15  TOTAFA              PICTURE S9(6).
               10  ANTKRE-IO.
                   15  ANTKRE              PICTURE S9(6).
               10  TOTAKR-IO.
                   15  TOTAKR              PICTURE S9(6).
               10  NETFRA-IO.
                   15  NETFRA              PICTURE S9(7)V9(2).
           05  EDITTING-FIELDS.
               10  XO-90D                  PICTURE S9(9).
               10  XO-90U                  PICTURE 9(9).
               10  XO-52YNZ                PICTURE ZZZZZ,ZZ.
               10  EDIT-ENHPR              PICTURE ZZZZZZZ,ZZ.
               10  EDIT-BTOPRI             PICTURE ZZZZZZZ,ZZ.
               10  EDIT-RAB1               PICTURE ZZ,Z.
               10  EDIT-RAB2               PICTURE ZZ,Z.
               10  EDIT-RAB3               PICTURE ZZ,Z.
               10  EDIT-NETTO              PICTURE ZZZZ.ZZZ,ZZ-.
               10  XO-52D                  PICTURE S9(5)V9(2).
               10  XO-52U                  PICTURE 9(5)V9(2).
               10  EDIT-STAVD              PICTURE ZZZZZZZ,ZZ.
               10  EDIT-NETFRA             PICTURE ZZZZ.ZZ9,99-.
               10  EDIT-NETBNR             PICTURE ZZZZ.ZZ9,99-.
               10  EDIT-AVRORE             PICTURE ZZZ,99-.
               10  EDIT-MOMS               PICTURE ZZZZ.ZZ9,99-.
               10  XO-12YN9                PICTURE Z,99.
               10  EDIT-TOTAL              PICTURE ZZZZZ.ZZ9,99-.
               10  XO-90YYZR               PICTURE ZZZ.ZZZ.ZZZ-.
               10  XO-50YY9                PICTURE ZZ.ZZ9.
               10  XO-62YY9R               PICTURE ZZZ.ZZZ,99-.
               10  XO-82YY9R               PICTURE ZZ.ZZZ.ZZZ,99-.
               10  XO-92YY9R               PICTURE ZZZ.ZZZ.ZZZ,99-.
               10  XO-60YY9                PICTURE ZZZ.ZZ9.
               10  XO-90YY9                PICTURE ZZZ.ZZZ.ZZ9.
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
           05  TESTN-INDEX                 PICTURE 9(4) USAGE BINARY.
           05  TESTN-STATE                 PICTURE X(1).
               88  TESTN-STATE-INIT        VALUE 'I'.
               88  TESTN-STATE-NUMBER      VALUE 'N'.
               88  TESTN-STATE-SPACE       VALUE 'S'.
               88  TESTN-STATE-LEADSPACE   VALUE 'L'.
               88  TESTN-STATE-OTHER       VALUE 'O'.
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
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-01                    TO TRUE
           SET NOT-I-01                    TO TRUE
           SET NOT-I-01                    TO TRUE
           SET NOT-I-01                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  FAKTIN-PROCESS
               SET FAKTIN-PROCESS-OFF      TO TRUE
               SET FAKTIN-READ             TO TRUE
           END-IF
 
           IF  FAKTIN-READ
           AND RECORD-SELECTED-OFF
               PERFORM FAKTIN-GET
               SET FAKTIN-READ-OFF         TO TRUE
               IF  NOT FAKTIN-EOF
                   SET FAKTIN-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  FAKTIN-PROCESS
               PERFORM FAKTIN-IDSET
           END-IF
 
           IF  FAKTIN-PROCESS
               PERFORM FAKTIN-CHK-LEVEL
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
 
           IF  FAKTIN-PROCESS
               PERFORM FAKTIN-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  FAKTIN-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-75                    TO TRUE
           SET NOT-I-21                    TO TRUE
           SET NOT-I-56                    TO TRUE
           SET NOT-I-23                    TO TRUE
           SET NOT-I-60                    TO TRUE
           SET NOT-I-62                    TO TRUE
           SET NOT-I-69                    TO TRUE
           SET NOT-I-71                    TO TRUE
           SET NOT-I-73                    TO TRUE
           SET NOT-I-66                    TO TRUE
           SET NOT-I-68                    TO TRUE
           SET NOT-I-70                    TO TRUE
           SET NOT-I-72                    TO TRUE
           SET NOT-I-76                    TO TRUE
           SET NOT-I-64                    TO TRUE
           SET NOT-I-27                    TO TRUE
           SET NOT-I-28                    TO TRUE
           SET NOT-I-51                    TO TRUE
           SET NOT-I-29                    TO TRUE
           SET NOT-I-57                    TO TRUE
           SET NOT-I-58                    TO TRUE
           SET NOT-I-59                    TO TRUE
           SET NOT-I-30                    TO TRUE
           SET NOT-I-31                    TO TRUE
           SET NOT-I-92                    TO TRUE
           SET NOT-I-93                    TO TRUE
           SET NOT-I-96                    TO TRUE
           SET NOT-I-78                    TO TRUE
           SET NOT-I-80                    TO TRUE
           SET NOT-I-32                    TO TRUE
           SET NOT-I-84                    TO TRUE
           SET NOT-I-82                    TO TRUE
           SET NOT-I-33                    TO TRUE
           SET NOT-I-85                    TO TRUE
           SET NOT-I-36                    TO TRUE
           SET NOT-I-37                    TO TRUE
           SET NOT-I-38                    TO TRUE
           SET NOT-I-39                    TO TRUE
           SET NOT-I-41                    TO TRUE
           SET NOT-I-08                    TO TRUE
           SET NOT-I-18                    TO TRUE
           IF  (I-L2)
               SET NOT-I-42                TO TRUE
               SET NOT-I-44                TO TRUE
               SET NOT-I-67                TO TRUE
               SET NOT-I-45                TO TRUE
               SET NOT-I-47                TO TRUE
               SET NOT-I-35                TO TRUE
               SET NOT-I-87                TO TRUE
               SET NOT-I-88                TO TRUE
               SET NOT-I-40                TO TRUE
               SET NOT-I-50                TO TRUE
               SET NOT-I-20                TO TRUE
               SET NOT-I-16                TO TRUE
               SET NOT-I-19                TO TRUE
               SET NOT-I-98                TO TRUE
      *
           END-IF
           IF  (I-L2)
               ADD 1                       TO BRKODE
               MOVE 0,00                   TO NETBNR
               MOVE 0,00                   TO NETFRA
      *
           END-IF
           IF  (I-02)
               ADD 1                       TO ANTREC
           END-IF
           IF  (I-L5)
               MOVE 0                      TO SEQ
           END-IF
           IF  (I-02)
               ADD 1                       TO SEQ
      *
           END-IF
           IF  (I-L5 AND NOT-I-22)
               READ FAKPAR
               AT END
                   SET I-H0                TO TRUE
                   MOVE 'M'                TO E-R-R-O-R
               NOT AT END
                   PERFORM FAKPAR-FLDSET
                   PERFORM FAKPAR-IDSET
               END-READ
               SET NOT-I-09                TO TRUE
               IF  FO-ELGRSTE = 'J'
                   SET I-09                TO TRUE
               END-IF
               SET I-69                    TO TRUE
               MOVE UFADTO                 TO NFADTO-IO
           END-IF
           IF  (I-L5)
               SET I-22                    TO TRUE
               SET I-10                    TO TRUE
           END-IF.
 
       LESNRF-T.
           IF  (I-L5)
               READ FAKTNR
               AT END
                   SET I-26                TO TRUE
               NOT AT END
                   SET NOT-I-26            TO TRUE
                   PERFORM FAKTNR-FLDSET
                   PERFORM FAKTNR-IDSET
               END-READ
           END-IF
           IF  (I-L5 AND I-26)
               SET I-H1                    TO TRUE
               GO TO SLUTT-T
           END-IF
           IF  (I-L5)
               SET NOT-I-43                TO TRUE
               IF  FIRMNR = FIRMN4
                   SET I-43                TO TRUE
               END-IF
           END-IF
           IF  (I-L5 AND NOT-I-43)
               GO TO LESNRF-T
           END-IF
           IF  (I-L5)
               MOVE PFANR1                 TO AREA1-IO
               MOVE PKRNR1                 TO AREA2-IO
               SET NOT-I-14                TO TRUE
               MOVE FIRMNR                 TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-55                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-55            TO TRUE
                   PERFORM FIRMAF-FLDOFF
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
           END-IF
           IF  (I-L5 AND NOT-I-55)
               SET NOT-I-25                TO TRUE
               IF  KONFNR > '000'
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-L5)
               MOVE FIRMNR                 TO KFIRNR
           END-IF
           IF  (I-L5 AND NOT-I-55 AND I-25)
               MOVE KONFNR                 TO KFIRNR
           END-IF
           IF  (I-L5)
               SET NOT-I-95                TO TRUE
               IF  FFKODE = 'U'
                   SET I-95                TO TRUE
               END-IF
               SET NOT-I-97                TO TRUE
               IF  FIRMNR = '922'
                   SET I-97                TO TRUE
               END-IF
      *****************************************************************
      * TESTER PÅ OM FORETAKSNUMMER SKAL PRINTES.                     *
      *****************************************************************
           END-IF
           IF  (I-L5)
               SET NOT-I-25                TO TRUE
               IF  FTAKNR > 0
                   SET I-25                TO TRUE
               END-IF
               SET NOT-I-53                TO TRUE
               IF  PRIFNR = 'A'
                   SET I-53                TO TRUE
               END-IF
           END-IF
           IF  (I-L5 AND NOT-I-53)
               SET NOT-I-53                TO TRUE
               IF  PRIFNR = 'C'
                   SET I-53                TO TRUE
               END-IF
           END-IF
           IF  (I-L5 AND I-53)
               SET NOT-I-25                TO TRUE
      ***********************************************************
      *  IND=89 - PRINTER TEKSTLINJER PÅ FAKTURA.               *
      *  IND=98 - HENTER BETALINGSMÅTE FRA KUNDEARKIVET PÅ KRED *
      *           ELLERS VIL FORFALLSDATO VÆRE LIKT FAKTURADATO *
      *           PÅ KREDITNOTA.                                *
      *  IND=24 - SKAL HA FORFALL = FAK. DATO PÅ KR.NOTA BM=22  *
      *  IND=49 - SKAL HA UT REG.AVD UT I HEADINGREC.           *
      ******************************************************** **
           END-IF
           IF  (I-L2)
               SET NOT-I-24                TO TRUE
               IF  FIRMNR = '918'
                   SET I-24                TO TRUE
               END-IF
               SET NOT-I-98                TO TRUE
               IF  KRFFD = 'K'
                   SET I-98                TO TRUE
               END-IF
      *****************************************************************
      * RUTINE FOR Å HENTE FRAM TEKSTLINJER PÅ FAKTURA.               *
      *****************************************************************
           END-IF
           IF  (I-L5)
               MOVE FIRMNR                 TO KEYT (1:3)
               MOVE 'F'                    TO KEYT (4:1)
               MOVE KEYT                   TO TEKSTF-KEY1
               READ TEKSTF RECORD KEY IS TEKSTF-KEY1
               INVALID KEY
                   SET I-89                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-89            TO TRUE
                   PERFORM TEKSTF-FLDSET
                   PERFORM TEKSTF-IDSET
               END-READ
      ******************************************************
      *    RUTINE FOR Å DANNE TRANSKODE PÅ RESKONTROREC.   *
      *    RENTER/GEBYR = TRANSKODE 22                     *
      ******************************************************
           END-IF
           IF  (I-L2)
               SET NOT-I-20                TO TRUE
               IF  RENGEB = 'R'
                   SET I-20                TO TRUE
               END-IF
      ******************************************************
      *    TEST OM DET ER EN PRISTILLEGGRECORD.            *
      * DA SKAL ARTIKKELNUMMER ERSTATTES AV VARENAVN       *
      * PÅ FAKTURA-EDI RECORD. (PANT, OL)                  *
      ******************************************************
           END-IF
           SET NOT-I-06                    TO TRUE
           IF  LAGERK = 'PT'
               SET I-06                    TO TRUE
           END-IF
      ******************************************************
      *    SKAL KONTANT/OPPKRAV - TEKST SKRIVES PÅ FAKTURA *
      ******************************************************
           SET NOT-I-93                    TO TRUE
           IF  BTMA-ELGTE = '07'
               SET I-93                    TO TRUE
           END-IF
           SET NOT-I-46                    TO TRUE
           IF  FIRMNR = '956'
               SET I-46                    TO TRUE
           END-IF
           IF  (I-46)
               GO TO NOTEXT-T
           END-IF
           SET NOT-I-96                    TO TRUE
           IF  BTMA-ELGTE = '14'
               SET I-96                    TO TRUE
           END-IF.
 
       NOTEXT-T.
      ******************************************************
      *  FAKTURA UTSKRIFT UTEN BRUTTOPRIS OG RABATTER.     *
      ******************************************************
           SET NOT-I-71                    TO TRUE
           IF  FLSIDE = 'F'
               SET I-71                    TO TRUE
           END-IF
      *******************************************************
           IF  (I-14)
               GO TO A4-T
           END-IF
           SET NOT-I-21                    TO TRUE
           IF  FAKRNR = 'K'
               SET I-21                    TO TRUE
           END-IF
           IF  (I-21)
               GO TO A3-T
           END-IF
           GO TO A4-T.
 
       A3-T.
           SET I-14                        TO TRUE.
 
       A4-T.
           IF  (I-10)
               GO TO A7-T
           END-IF
           IF  (I-L2)
               GO TO A8-T
           END-IF
           IF  (I-11)
               GO TO A9-T
           END-IF
           GO TO A11-T.
 
       A7-T.
           SET NOT-I-10                    TO TRUE.
 
       A8-T.
           SET NOT-I-12                    TO TRUE
           SET NOT-I-99                    TO TRUE
           MOVE RESKNR                     TO RESKEY (4:6)
           MOVE KFIRNR                     TO RESKEY (1:3)
      *****************************************************************
      *  HENTER KUNDEDATA FRA KUNDE.ARKIVET                           *
      *****************************************************************
           MOVE RESKEY                     TO KUNDEMA-KEY1
           READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
           INVALID KEY
               SET I-90                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-90                TO TRUE
               PERFORM KUNDEMA-FLDSET
               PERFORM KUNDEMA-IDSET
           END-READ
           IF  (I-90)
               ADD 1                       TO KFEIL
      *****************************************************************
      * RUTINE FOR OG LEGGE KUNDEREF INN I KUNDEADRESSE PÅ FAKTURA.   *
      *****************************************************************
           END-IF
           IF  (NOT-I-90)
               MOVE '923'                  TO FNRKNR (1:3)
               MOVE '116075'               TO FNRKNR (4:6)
               SET NOT-I-99                TO TRUE
               IF  RESKEY = FNRKNR
                   SET I-99                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-90 AND I-99)
               MOVE '        '             TO FELT16 (1:8)
               MOVE '        '             TO FELT16 (9:8)
               MOVE '        '             TO FELT14 (1:8)
               MOVE '      '               TO FELT14 (9:6)
               MOVE FELT16                 TO FELT30 (1:16)
               MOVE FELT14                 TO FELT30 (17:14)
               MOVE FELT30                 TO CADR
               MOVE KUREF                  TO CADR (1:15)
      *****************************************************************
      * TEST OM KJØP HITTIL I ÅR SKAL FREMKOMME PÅ FAKTURA.           *
      *****************************************************************
           END-IF
           IF  (NOT-I-90)
               SET NOT-I-99                TO TRUE
               IF  BELFAK = 'J'
                   SET I-99                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-90 AND I-99)
               ADD KJOPT TO ZERO       GIVING KJBEL ROUNDED
      *****************************************************************
      * RUTINE FOR Å SKRIVE UT EAN-LOCASJONSNUMMER PÅ SHELL KJEDEKUNDE*
      * DETTE GJELDER KUNDER MED RESK.GRP 99 PÅ FIRMA 923             *
      *****************************************************************
           END-IF
           SET NOT-I-49                    TO TRUE
           IF  FIRMNR = '923'
               SET I-49                    TO TRUE
           END-IF
           IF  (NOT-I-49)
               GO TO ENDEAN-T
           END-IF
           SET NOT-I-49                    TO TRUE
           IF  RESGRP = '99'
               SET I-49                    TO TRUE
           END-IF
           IF  (NOT-I-49)
               GO TO ENDEAN-T
           END-IF
           MOVE RESKEY                     TO RESKEX (1:9)
           MOVE '1'                        TO RESKEX (10:1)
           MOVE RESKEX                     TO KUNDEMX-KEY1
           READ KUNDEMX RECORD KEY IS KUNDEMX-KEY1
           INVALID KEY
               SET I-54                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-54                TO TRUE
               PERFORM KUNDEMX-FLDSET
               PERFORM KUNDEMX-IDSET
           END-READ
           IF  (I-54)
               SET NOT-I-49                TO TRUE
           END-IF.
 
       ENDEAN-T.
           SET NOT-I-54                    TO TRUE
      *****************************************************************
      * RUTINE FOR Å SJEKKE OM KUNDEN SKAL HA SUBTOTALER.             *
      * DETTE GJELDER FIRMA 923 OG ESSO (P11) ELLER SHELL (P61)       *
      * KJØRES FOR ALLE SHELL-KUNDER (P61) + ENKELTE ANDRE SHELL.     *
      * KJØRES FORLØPIG KUN FOR ENKELTE ESSO-KUNDER (P11).            *
      * LAGT INN FOR ALLE KUNDER PÅ P61. 22/04-1999                   *
      * LAGT INN FOR ALLE KUNDER PÅ P11. 22/01-2003                   *
      *****************************************************************
           SET NOT-I-34                    TO TRUE
           SET NOT-I-94                    TO TRUE
           IF  FIRMNR = '923'
               SET I-94                    TO TRUE
           END-IF
           IF  (NOT-I-94)
               GO TO END923-T
           END-IF
           MOVE RESKEY                     TO RESKEX (1:9)
           MOVE '1'                        TO RESKEX (10:1)
           MOVE RESKEX                     TO KUNDEMX-KEY1
           READ KUNDEMX RECORD KEY IS KUNDEMX-KEY1
           INVALID KEY
               SET I-54                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-54                TO TRUE
               PERFORM KUNDEMX-FLDSET
               PERFORM KUNDEMX-IDSET
           END-READ
           IF  (I-54)
               SET NOT-I-94                TO TRUE
               GO TO END923-T
           END-IF
           SET NOT-I-54                    TO TRUE
           IF  KKAT = 'P61'
               SET I-54                    TO TRUE
           END-IF
           IF  (NOT-I-54)
               SET NOT-I-54                TO TRUE
               IF  RESKNR = '132272'
                   SET I-54                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-54)
               SET NOT-I-54                TO TRUE
               IF  RESKNR = '132784'
                   SET I-54                TO TRUE
               END-IF
           END-IF
           SET NOT-I-34                    TO TRUE
           IF  RESKNR = '112649'
               SET I-34                    TO TRUE
           END-IF
           IF  (NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  KKAT = 'P11'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-54 AND NOT-I-34)
               SET NOT-I-94                TO TRUE
           END-IF.
 
       END923-T.
           SET NOT-I-54                    TO TRUE
      *****************************************************************
      * RUTINE FOR SE OM KUNDEN SKAL HA TOTSUMMER PR. AVDELING.       *
      * DET ER KUN FIRMA 950 HAFNOR SOM SKAL HA DETTE PÅ FINA.        *
      * TEXACO HAR KUNDEKAT: 151,251,351,451,551,651,751,851,951      *
      * BEST   HAR KUNDEKAT: 135,235,335,435,535,635,735,835,935      *
      *****************************************************************
           SUBTRACT STAVDA                 FROM STAVDA
           SUBTRACT STAVDB                 FROM STAVDB
           SUBTRACT STAVDC                 FROM STAVDC
           SUBTRACT STAVDD                 FROM STAVDD
           SUBTRACT STAVD1                 FROM STAVD1
           SUBTRACT STAVD2                 FROM STAVD2
           SUBTRACT STAVD3                 FROM STAVD3
           SUBTRACT STAVD4                 FROM STAVD4
           SUBTRACT STAVD5                 FROM STAVD5
           SUBTRACT STAVD6                 FROM STAVD6
           SUBTRACT STAVD7                 FROM STAVD7
           SUBTRACT STAVD8                 FROM STAVD8
           SUBTRACT STAVD9                 FROM STAVD9
           SET NOT-I-86                    TO TRUE
           SET NOT-I-81                    TO TRUE
           IF  FIRMNR = '950'
               SET I-81                    TO TRUE
           END-IF
           IF  (NOT-I-81)
               GO TO ENDST1-T
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  KUNKAT = 151
               SET I-81                    TO TRUE
           END-IF
           IF  (I-81)
               SET I-86                    TO TRUE
           END-IF.
 
       ENDST1-T.
      ******************************************************
      *    SKAL RENTETEKST SKRIVES PÅ FAKTURA ?            *
      ******************************************************
           SET NOT-I-40                    TO TRUE
           IF  RENTE = '1'
               SET I-40                    TO TRUE
           END-IF
           SET NOT-I-48                    TO TRUE
           IF  RENTE = 'A'
               SET I-48                    TO TRUE
           END-IF
           IF  (NOT-I-40 AND NOT-I-14)
               SET I-19                    TO TRUE
      ******************************************************
           END-IF
           GO TO A12-T.
 
       A9-T.
           SET NOT-I-11                    TO TRUE
           SET NOT-I-23                    TO TRUE
           IF  RA = 'A'
               SET I-23                    TO TRUE
           END-IF
           IF  (I-23)
               GO TO A10-T
           END-IF
           GO TO A98-T.
 
       A10-T.
           SET I-60                        TO TRUE
           SET I-62                        TO TRUE
           GO TO SLUTT-T.
 
       A12-T.
           SET NOT-I-23                    TO TRUE
           IF  RA = 'A'
               SET I-23                    TO TRUE
           END-IF
           IF  (I-23)
               GO TO A12A-T
           END-IF
           SET I-64                        TO TRUE
           SET I-66                        TO TRUE.
 
       A98-T.
           SET I-68                        TO TRUE
           SET I-70                        TO TRUE
           GO TO A11-T.
 
       A12A-T.
           SET I-72                        TO TRUE
           SET I-76                        TO TRUE
           SET I-11                        TO TRUE
           GO TO SLUTT-T.
 
       A11-T.
           SET NOT-I-23                    TO TRUE
           IF  RA = 'A'
               SET I-23                    TO TRUE
           END-IF
           IF  (I-23)
               GO TO SLUTT-T
           END-IF
           IF  (I-12)
               GO TO D1-T
           END-IF
           IF  (I-14)
               GO TO B1-T
           END-IF
           GO TO B2-T.
 
       B1-T.
      *
      *  INDIKATOR 95 BETYR AT FIRMNR"ET HAR UKENTLIG FORFALL.
      *
           MOVE PFADTO                     TO FOFALL
           IF  (I-95)
               MOVE UFADTO                 TO FOFALL
      *****************************************************************
      *  INDIKATOR 98 BETYR AT KREDINOTAER SKAL HA ORGINAL BETM.
      *  IKKE OM ORDRE HAR KONTANT ELLER OPPKRAV (07 / 14)
      *****************************************************************
           END-IF
           IF  (I-98)
               SET NOT-I-98                TO TRUE
               IF  BTMA-ELGTE NOT = '07'
                   SET I-98                TO TRUE
               END-IF
           END-IF
           IF  (I-98)
               SET NOT-I-98                TO TRUE
               IF  BTMA-ELGTE NOT = '14'
                   SET I-98                TO TRUE
               END-IF
           END-IF
           IF  (I-98)
               SET NOT-I-98                TO TRUE
               IF  CBETM NOT = '07'
                   SET I-98                TO TRUE
               END-IF
           END-IF
           IF  (I-98)
               SET NOT-I-98                TO TRUE
               IF  CBETM NOT = '14'
                   SET I-98                TO TRUE
               END-IF
           END-IF
           IF  (I-98)
               SET NOT-I-98                TO TRUE
               IF  CBETM NOT = '19'
                   SET I-98                TO TRUE
               END-IF
           END-IF
           IF  (I-98 AND I-24)
               SET NOT-I-98                TO TRUE
               IF  CBETM NOT = '22'
                   SET I-98                TO TRUE
               END-IF
           END-IF
           IF  (I-98)
               MOVE CBETM                  TO BTMA-ELGTE
               GO TO B2-T
           END-IF
           GO TO C1-T.
 
       B2-T.
           SET NOT-I-56                    TO TRUE
           IF  BTMA-ELGTE = '09'
               SET I-56                    TO TRUE
           END-IF
           IF  (I-56)
               GO TO B09-T
           END-IF
           SET NOT-I-56                    TO TRUE
           IF  BTMA-ELGTE = '13'
               SET I-56                    TO TRUE
           END-IF
           IF  (I-56)
               GO TO B13-T
           END-IF
           SET NOT-I-56                    TO TRUE
           IF  BTMA-ELGTE = '21'
               SET I-56                    TO TRUE
           END-IF
           IF  (I-56)
               GO TO B21-T
           END-IF
           SET NOT-I-56                    TO TRUE
           IF  BTMA-ELGTE = '22'
               SET I-56                    TO TRUE
           END-IF
           IF  (I-56)
               GO TO B22-T
           END-IF
           SET NOT-I-56                    TO TRUE
           IF  BTMA-ELGTE = '23'
               SET I-56                    TO TRUE
           END-IF
           IF  (I-56)
               GO TO B23-T
           END-IF
           SET NOT-I-27                    TO TRUE
           IF  BTMA-ELGTE = '15'
               SET I-27                    TO TRUE
           END-IF
           IF  (I-27)
               GO TO B4-T
           END-IF
           SET NOT-I-27                    TO TRUE
           IF  BTMA-ELGTE = '31'
               SET I-27                    TO TRUE
           END-IF
           IF  (I-27)
               GO TO B4-T
           END-IF
           SET NOT-I-28                    TO TRUE
           IF  BTMA-ELGTE = '16'
               SET I-28                    TO TRUE
           END-IF
           IF  (NOT-I-28)
               SET NOT-I-28                TO TRUE
               IF  BTMA-ELGTE = '34'
                   SET I-28                TO TRUE
               END-IF
           END-IF
           IF  (I-28)
               GO TO B5-T
           END-IF
           SET NOT-I-29                    TO TRUE
           IF  BTMA-ELGTE = '07'
               SET I-29                    TO TRUE
           END-IF
           IF  (NOT-I-29)
               SET NOT-I-29                TO TRUE
               IF  BTMA-ELGTE = '14'
                   SET I-29                TO TRUE
               END-IF
           END-IF
           IF  (I-29)
               GO TO B6-T
           END-IF
           SET NOT-I-51                    TO TRUE
           IF  BTMA-ELGTE = '08'
               SET I-51                    TO TRUE
           END-IF
           IF  (NOT-I-51)
               SET NOT-I-51                TO TRUE
               IF  BTMA-ELGTE = '33'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-51)
               GO TO B7-T
           END-IF
           SET NOT-I-57                    TO TRUE
           IF  BTMA-ELGTE = '18'
               SET I-57                    TO TRUE
           END-IF
           IF  (I-57)
               GO TO B6B-T
           END-IF
           SET NOT-I-59                    TO TRUE
           IF  BTMA-ELGTE = '20'
               SET I-59                    TO TRUE
           END-IF
           IF  (I-59)
               GO TO B6B-T
           END-IF
           SET NOT-I-30                    TO TRUE
           IF  BTMA-ELGTE = '10'
               SET I-30                    TO TRUE
           END-IF
           IF  (NOT-I-30)
               SET NOT-I-30                TO TRUE
               IF  BTMA-ELGTE = '05'
                   SET I-30                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-30)
               SET NOT-I-30                TO TRUE
               IF  BTMA-ELGTE = '32'
                   SET I-30                TO TRUE
               END-IF
           END-IF
           IF  (I-30)
               GO TO B7A-T
           END-IF
           SET NOT-I-30                    TO TRUE
           IF  BTMA-ELGTE = '17'
               SET I-30                    TO TRUE
           END-IF
           IF  (NOT-I-30)
               SET NOT-I-30                TO TRUE
               IF  BTMA-ELGTE = '35'
                   SET I-30                TO TRUE
               END-IF
           END-IF
           IF  (I-30)
               GO TO B8A-T
           END-IF
           SET NOT-I-07                    TO TRUE
           IF  BTMA-ELGTE = '02'
               SET I-07                    TO TRUE
           END-IF
           IF  (I-07)
               GO TO B6C-T
           END-IF
           SET NOT-I-07                    TO TRUE
           IF  BTMA-ELGTE = '12'
               SET I-07                    TO TRUE
           END-IF
           IF  (I-07)
               GO TO B6C-T
           END-IF
           SET NOT-I-07                    TO TRUE
           IF  BTMA-ELGTE = '30'
               SET I-07                    TO TRUE
           END-IF
           IF  (I-07)
               GO TO B6C-T
           END-IF
           SET NOT-I-13                    TO TRUE
           IF  BTMA-ELGTE = '25'
               SET I-13                    TO TRUE
           END-IF
           IF  (NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  BTMA-ELGTE = '36'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (I-13)
               GO TO B9-T
           END-IF
           SET NOT-I-65                    TO TRUE
           IF  BTMA-ELGTE = '26'
               SET I-65                    TO TRUE
           END-IF
           IF  (NOT-I-65)
               SET NOT-I-65                TO TRUE
               IF  BTMA-ELGTE = '37'
                   SET I-65                TO TRUE
               END-IF
           END-IF
           IF  (I-65)
               GO TO B945-T
           END-IF
           SET NOT-I-74                    TO TRUE
           IF  BTMA-ELGTE = '27'
               SET I-74                    TO TRUE
           END-IF
           IF  (NOT-I-74)
               SET NOT-I-74                TO TRUE
               IF  BTMA-ELGTE = '38'
                   SET I-74                TO TRUE
               END-IF
           END-IF
           IF  (I-74)
               GO TO B975-T
           END-IF
           SET NOT-I-79                    TO TRUE
           IF  BTMA-ELGTE = '28'
               SET I-79                    TO TRUE
           END-IF
           IF  (NOT-I-79)
               SET NOT-I-79                TO TRUE
               IF  BTMA-ELGTE = '39'
                   SET I-79                TO TRUE
               END-IF
           END-IF
           IF  (I-79)
               GO TO B990-T
           END-IF
           GO TO B7B-T.
 
       B7A-T.
           SET NOT-I-31                    TO TRUE
           IF  PFDAG NOT > '15'
               SET I-31                    TO TRUE
           END-IF
           IF  (I-31)
               GO TO B7-T
           END-IF
           GO TO B7B-T.
 
       B8A-T.
           SET NOT-I-31                    TO TRUE
           IF  PFDAG NOT > '15'
               SET I-31                    TO TRUE
           END-IF
           IF  (I-31)
               GO TO B8-T
           END-IF
           GO TO B8B-T.
 
       B9-T.
           SET NOT-I-31                    TO TRUE
           IF  PFDAG NOT > '15'
               SET I-31                    TO TRUE
           END-IF
           IF  (I-31)
               GO TO B9A-T
           END-IF
           GO TO B9B-T.
 
       B945-T.
           SET NOT-I-31                    TO TRUE
           IF  PFDAG NOT > '15'
               SET I-31                    TO TRUE
           END-IF
           IF  (I-31)
               GO TO B945A-T
           END-IF
           GO TO B945B-T.
 
       B975-T.
           SET NOT-I-31                    TO TRUE
           IF  PFDAG NOT > '15'
               SET I-31                    TO TRUE
           END-IF
           IF  (I-31)
               GO TO B975A-T
           END-IF
           GO TO B975B-T.
 
       B990-T.
           SET NOT-I-31                    TO TRUE
           IF  PFDAG NOT > '15'
               SET I-31                    TO TRUE
           END-IF
           IF  (I-31)
               GO TO B990A-T
           END-IF
           GO TO B990B-T.
 
       B09-T.
           MOVE PFF90                      TO FOFALL
           IF  (I-95)
               MOVE UFF90                  TO FOFALL
           END-IF
           GO TO C1-T.
 
       B13-T.
           MOVE PFF120                     TO FOFALL
           IF  (I-95)
               MOVE UFF120                 TO FOFALL
           END-IF
           GO TO C1-T.
 
       B21-T.
           MOVE PFF120                     TO FOFALL
           IF  (I-95)
               MOVE UFF120                 TO FOFALL
           END-IF
           GO TO C1-T.
 
       B22-T.
           MOVE PFF150                     TO FOFALL
           IF  (I-95)
               MOVE UFF150                 TO FOFALL
           END-IF
           GO TO C1-T.
 
       B23-T.
           MOVE PFF180                     TO FOFALL
           IF  (I-95)
               MOVE UFF180                 TO FOFALL
           END-IF
           GO TO C1-T.
 
       B4-T.
           MOVE PFF60                      TO FOFALL
           IF  (I-95)
               MOVE UFF60                  TO FOFALL
           END-IF
           GO TO C1-T.
 
       B5-T.
           MOVE PFF90                      TO FOFALL
           IF  (I-95)
               MOVE UFF90                  TO FOFALL
           END-IF
           GO TO C1-T.
 
       B6-T.
           MOVE PFADTO                     TO FOFALL
           IF  (I-95)
               MOVE UFADTO                 TO FOFALL
           END-IF
           GO TO C1-T.
 
       B6B-T.
           MOVE PFF30                      TO FOFALL
           IF  (I-95)
               MOVE UFF30                  TO FOFALL
           END-IF
           GO TO C1-T.
 
       B6C-T.
           MOVE PFF15                      TO FOFALL
           IF  (I-95)
               MOVE UFF15                  TO FOFALL
           END-IF
           GO TO C1-T.
 
       B8-T.
           MOVE PFF75                      TO FOFALL
           IF  (I-95 AND NOT-I-30)
               MOVE UFF75                  TO FOFALL
           END-IF
           GO TO C1-T.
 
       B8B-T.
           MOVE PFF60                      TO FOFALL
           IF  (I-95 AND NOT-I-30)
               MOVE UFF60                  TO FOFALL
           END-IF
           GO TO C1-T.
 
       B7-T.
           MOVE PFF45                      TO FOFALL
           IF  (I-95 AND NOT-I-30)
               MOVE UFF45                  TO FOFALL
           END-IF
           GO TO C1-T.
 
       B7B-T.
           MOVE PFF30                      TO FOFALL
           IF  (I-95 AND NOT-I-30)
               MOVE UFF30                  TO FOFALL
           END-IF
           GO TO C1-T.
 
       B9A-T.
           MOVE PFF30                      TO FOFALL
           IF  (I-95 AND NOT-I-13)
               MOVE UFF30                  TO FOFALL
           END-IF
           GO TO C1-T.
 
       B9B-T.
           MOVE PFF15                      TO FOFALL
           IF  (I-95 AND NOT-I-13)
               MOVE UFF15                  TO FOFALL
           END-IF
           GO TO C1-T.
 
       B945A-T.
           MOVE PFF60                      TO FOFALL
           IF  (I-95 AND NOT-I-65)
               MOVE UFF60                  TO FOFALL
           END-IF
           GO TO C1-T.
 
       B945B-T.
           MOVE PFF45                      TO FOFALL
           IF  (I-95 AND NOT-I-65)
               MOVE UFF45                  TO FOFALL
           END-IF
           GO TO C1-T.
 
       B975A-T.
           MOVE PFF90                      TO FOFALL
           IF  (I-95 AND NOT-I-74)
               MOVE UFF90                  TO FOFALL
           END-IF
           GO TO C1-T.
 
       B975B-T.
           MOVE PFF75                      TO FOFALL
           IF  (I-95 AND NOT-I-74)
               MOVE UFF75                  TO FOFALL
           END-IF
           GO TO C1-T.
 
       B990A-T.
           MOVE PFF105                     TO FOFALL
           IF  (I-95 AND NOT-I-79)
               MOVE UFF105                 TO FOFALL
           END-IF
           GO TO C1-T.
 
       B990B-T.
           MOVE PFF90                      TO FOFALL
           IF  (I-95 AND NOT-I-79)
               MOVE UFF90                  TO FOFALL
           END-IF
           SET NOT-I-56                    TO TRUE
           IF  BTMA-ELGTE = '24'
               SET I-56                    TO TRUE
           END-IF.
 
       C1-T.
           SET I-78                        TO TRUE
           SET I-12                        TO TRUE
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *  COBOL SUBRUTINE FOR HENTING AV BETALINGSMÅTE-TEKST     *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
           MOVE BTMA-ELGTE                 TO BETMAT
           CALL 'BETBETN' USING BMFELT-XX-DATA-FIELDS
           SET NOT-I-91                    TO TRUE
           IF  BETMAT = '00'
               SET I-91                    TO TRUE
           END-IF
      * * * * * * * * * * * * * * * * * * * * * * * * *
           .
 
       D1-T.
           IF  (I-15)
               GO TO D2-T
           END-IF
           GO TO D3-T.
 
       D2-T.
           SET I-80                        TO TRUE
           SET NOT-I-15                    TO TRUE.
 
       D3-T.
           SET NOT-I-32                    TO TRUE
           IF  EDBNR = '       '
               SET I-32                    TO TRUE
           END-IF
           IF  (I-32)
               GO TO D4-T
           END-IF
           GO TO D5-T.
 
       D4-T.
           SET I-82                        TO TRUE
           GO TO E1-T.
 
       D5-T.
      *  DERSOM EDBNR BEGYNNER MED "995" ELLER "94" SNUS BELØPET.
           SET NOT-I-33                    TO TRUE
           IF  EDB3F = '995'
               SET I-33                    TO TRUE
           END-IF
           IF  (NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  EDB2F = '94'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-33)
               GO TO D6-T
           END-IF
           SET NOT-I-33                    TO TRUE
           IF  LEVENH = 0,00
               SET I-33                    TO TRUE
           END-IF
           IF  (I-33)
               GO TO D9-T
           END-IF
           MULTIPLY LEVENH BY ENHPR    GIVING BTOPRI ROUNDED
           GO TO D10-T.
 
       D9-T.
           MOVE ENHPR                      TO BTOPRI-IO
           GO TO D10-T.
 
       D6-T.
           SET NOT-I-33                    TO TRUE
           IF  LEVENH = 0,00
               SET I-33                    TO TRUE
           END-IF
           IF  (I-33)
               GO TO D7-T
           END-IF
           MULTIPLY LEVENH BY ENHPR    GIVING BTOPRI ROUNDED.
 
       D8-T.
           MULTIPLY -1 BY BTOPRI       GIVING BTOPRI
           GO TO D10-T.
 
       D7-T.
           MOVE ENHPR                      TO BTOPRI-IO
           GO TO D8-T.
 
       D10-T.
           SET NOT-I-36                    TO TRUE
           IF  RAB1 = 00,0
               SET I-36                    TO TRUE
           END-IF
           IF  (I-36)
               GO TO D11-T
           END-IF
           MULTIPLY RAB1 BY BTOPRI     GIVING RABX1
           DIVIDE RABX1 BY 100         GIVING RABAT1
           SUBTRACT RABAT1 FROM BTOPRI GIVING BRTO1 ROUNDED
           GO TO D12-T.
 
       D11-T.
           MOVE BTOPRI                     TO BRTO1-IO.
 
       D12-T.
           SET NOT-I-37                    TO TRUE
           IF  RAB2 = 00,0
               SET I-37                    TO TRUE
           END-IF
           IF  (I-37)
               GO TO D13-T
           END-IF
           MULTIPLY RAB2 BY BRTO1      GIVING RABX1
           DIVIDE RABX1 BY 100         GIVING RABAT2
           SUBTRACT RABAT2 FROM BRTO1  GIVING BRTO2 ROUNDED
           GO TO D14-T.
 
       D13-T.
           MOVE BRTO1                      TO BRTO2-IO.
 
       D14-T.
           SET NOT-I-38                    TO TRUE
           IF  RAB3 = 00,0
               SET I-38                    TO TRUE
           END-IF
           IF  (I-38)
               GO TO D15-T
           END-IF
           MULTIPLY RAB3 BY BRTO2      GIVING RABX1
           DIVIDE RABX1 BY 100         GIVING RABAT3
           SUBTRACT RABAT3 FROM BRTO2  GIVING NETTO ROUNDED
           GO TO D98-T.
 
       D15-T.
           MOVE BRTO2                      TO NETTO-IO
           IF  (I-36 AND I-37 AND I-38)
               SET I-73                    TO TRUE
           END-IF
           IF  (I-73)
               SET I-58                    TO TRUE
           END-IF.
 
       D98-T.
           ADD NETTO                       TO NETBNR
           MULTIPLY LEVENH BY KOSTPR   GIVING SUMSP
           SET I-39                        TO TRUE
           SET I-41                        TO TRUE
           SET NOT-I-08                    TO TRUE
           IF  FRITT = '5'
               SET I-08                    TO TRUE
           END-IF
           IF  (I-08)
               SET NOT-I-18                TO TRUE
               IF  HDIST1 = '0'
                   SET I-18                TO TRUE
               END-IF
           END-IF
           IF  (I-08 AND NOT-I-18)
               SET NOT-I-18                TO TRUE
               IF  CPNR = '8099'
                   SET I-18                TO TRUE
               END-IF
           END-IF
           IF  (I-08 AND NOT-I-18)
               SET NOT-I-18                TO TRUE
               IF  CPNR3 = '917'
                   SET I-18                TO TRUE
               END-IF
           END-IF
           SET NOT-I-61                    TO TRUE
           IF  FIRMNR = '960'
               SET I-61                    TO TRUE
           END-IF
           IF  (I-61)
               PERFORM FRARUT-S
      *****************************************************************
      * RUTINE FOR SUMMERING AV AVDELINGSTOTALER. SCANGROSS/SHELL     *
      *****************************************************************
           END-IF
           IF  (NOT-I-94)
               GO TO ENDSG1-T
           END-IF
           IF  (I-34)
               GO TO ESSOR1-T
           END-IF
           MOVE FIRMNR                     TO KEYSG (1:3)
           MOVE VGR                        TO KEYSG (4:5)
           SET NOT-I-81                    TO TRUE
           SET TABFVG-S                    TO TABFVG-I
           PERFORM WITH TEST AFTER
                   VARYING TABFVG-I FROM 1 BY 1
                     UNTIL TABFVG-I >= TABFVG-MAX
                        OR I-81
               IF  KEYSG = TABFVG (TABFVG-I)
                   SET I-81                TO TRUE
                   SET TABFVG-S            TO TABFVG-I
               END-IF
           END-PERFORM
           SET TABFVG-I                    TO TABFVG-S
           IF  I-81
           AND TABFVG-I NOT > TABFSG-MAX
               SET TABFSG-I                TO TABFVG-I
           END-IF
           IF  (I-81)
               MOVE TABFSG(TABFSG-I)       TO SGRAVD
           END-IF
           IF  (NOT-I-81)
               MOVE '1'                    TO SGRAVD
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  SGRAVD = '1'
               SET I-81                    TO TRUE
           END-IF
           IF  (I-81)
               ADD NETTO                   TO STAVD1
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  SGRAVD = '2'
               SET I-81                    TO TRUE
           END-IF
           IF  (I-81)
               ADD NETTO                   TO STAVD2
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  SGRAVD = '3'
               SET I-81                    TO TRUE
           END-IF
           IF  (I-81)
               ADD NETTO                   TO STAVD3
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  SGRAVD = '4'
               SET I-81                    TO TRUE
           END-IF
           IF  (I-81)
               ADD NETTO                   TO STAVD4
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  SGRAVD = '5'
               SET I-81                    TO TRUE
           END-IF
           IF  (I-81)
               ADD NETTO                   TO STAVD5
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  SGRAVD = '6'
               SET I-81                    TO TRUE
           END-IF
           IF  (I-81)
               ADD NETTO                   TO STAVD6
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  SGRAVD = '7'
               SET I-81                    TO TRUE
           END-IF
           IF  (I-81)
               ADD NETTO                   TO STAVD7
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  SGRAVD = '8'
               SET I-81                    TO TRUE
           END-IF
           IF  (I-81)
               ADD NETTO                   TO STAVD8
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  SGRAVD = '9'
               SET I-81                    TO TRUE
           END-IF
           IF  (I-81)
               ADD NETTO                   TO STAVD9
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  SGRAVD = 'A'
               SET I-81                    TO TRUE
           END-IF
           IF  (I-81)
               ADD NETTO                   TO STAVDA
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  SGRAVD = 'B'
               SET I-81                    TO TRUE
           END-IF
           IF  (I-81)
               ADD NETTO                   TO STAVDB
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  SGRAVD = 'C'
               SET I-81                    TO TRUE
           END-IF
           IF  (I-81)
               ADD NETTO                   TO STAVDC
           END-IF
           GO TO ENDSG1-T
      *****************************************************************
      * RUTINE FOR SUMMERING AV AVDELINGSTOTALER. SCANGROSS/ESSO      *
      * FRA 1.1.2003 ER ESSO OLJE VAREGRUPPE 60001 TIL 69999          *
      *****************************************************************
           .
 
       ESSOR1-T.
           MOVE VGR                        TO VGRESS
           SET NOT-I-81                    TO TRUE
           IF  VGRESS < '60001'
               SET I-81                    TO TRUE
           END-IF
           IF  (I-81)
               GO TO ESSOR2-T
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  VGRESS > '69999'
               SET I-81                    TO TRUE
           END-IF
           IF  (I-81)
               GO TO ESSOR2-T
           END-IF
           MOVE '60001'                    TO VGRESS.
 
       ESSOR2-T.
           SET NOT-I-81                    TO TRUE
           SET TABFVE-S                    TO TABFVE-I
           PERFORM WITH TEST AFTER
                   VARYING TABFVE-I FROM 1 BY 1
                     UNTIL TABFVE-I >= TABFVE-MAX
                        OR I-81
               IF  VGRESS = TABFVE (TABFVE-I)
                   SET I-81                TO TRUE
                   SET TABFVE-S            TO TABFVE-I
               END-IF
           END-PERFORM
           SET TABFVE-I                    TO TABFVE-S
           IF  I-81
           AND TABFVE-I NOT > TABFSE-MAX
               SET TABFSE-I                TO TABFVE-I
           END-IF
           IF  (I-81)
               MOVE TABFSE(TABFSE-I)       TO ESSAVD
           END-IF
           IF  (NOT-I-81)
               MOVE '1'                    TO ESSAVD
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  ESSAVD = '1'
               SET I-81                    TO TRUE
           END-IF
           IF  (I-81)
               ADD NETTO                   TO STAVD1
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  ESSAVD = '2'
               SET I-81                    TO TRUE
           END-IF
           IF  (I-81)
               ADD NETTO                   TO STAVD2
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  ESSAVD = '3'
               SET I-81                    TO TRUE
           END-IF
           IF  (I-81)
               ADD NETTO                   TO STAVD3
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  ESSAVD = '4'
               SET I-81                    TO TRUE
           END-IF
           IF  (I-81)
               ADD NETTO                   TO STAVD4
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  ESSAVD = '5'
               SET I-81                    TO TRUE
           END-IF
           IF  (I-81)
               ADD NETTO                   TO STAVD5
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  ESSAVD = '6'
               SET I-81                    TO TRUE
           END-IF
           IF  (I-81)
               ADD NETTO                   TO STAVD6
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  ESSAVD = '7'
               SET I-81                    TO TRUE
           END-IF
           IF  (I-81)
               ADD NETTO                   TO STAVD7
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  ESSAVD = '8'
               SET I-81                    TO TRUE
           END-IF
           IF  (I-81)
               ADD NETTO                   TO STAVD8
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  ESSAVD = '9'
               SET I-81                    TO TRUE
           END-IF
           IF  (I-81)
               ADD NETTO                   TO STAVD9
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  ESSAVD = 'A'
               SET I-81                    TO TRUE
           END-IF
           IF  (I-81)
               ADD NETTO                   TO STAVDA
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  ESSAVD = 'B'
               SET I-81                    TO TRUE
           END-IF
           IF  (I-81)
               ADD NETTO                   TO STAVDB
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  ESSAVD = 'C'
               SET I-81                    TO TRUE
           END-IF
           IF  (I-81)
               ADD NETTO                   TO STAVDC
           END-IF.
 
       ENDSG1-T.
      *****************************************************************
      * RUTINE FOR SUMMERING AV AVDELINGSTOTALER. HAFNOR/HYDRO-TEXACO *
      *                                        OG HAFNOR/BEST-STASJON *
      *****************************************************************
           IF  (NOT-I-86)
               GO TO ENDST2-T
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  VGR = '10958'
               SET I-81                    TO TRUE
           END-IF
           IF  (NOT-I-81)
               SET NOT-I-81                TO TRUE
               IF  VGR = '10960'
                   SET I-81                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-81)
               SET NOT-I-81                TO TRUE
               IF  VGR = '10961'
                   SET I-81                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-81)
               SET NOT-I-81                TO TRUE
               IF  VGR = '11600'
                   SET I-81                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-81)
               SET NOT-I-81                TO TRUE
               IF  EDBNR = '4068947'
                   SET I-81                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-81)
               SET NOT-I-81                TO TRUE
               IF  VGR = '76000'
                   SET I-81                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-81)
               SET NOT-I-81                TO TRUE
               IF  VGR = '76500'
                   SET I-81                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-81)
               SET NOT-I-81                TO TRUE
               IF  VGR = '12100'
                   SET I-81                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-81)
               SET NOT-I-81                TO TRUE
               IF  VGR = '74000'
                   SET I-81                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-81)
               SET NOT-I-81                TO TRUE
               IF  VGR = '74001'
                   SET I-81                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-81)
               SET NOT-I-81                TO TRUE
               IF  VGR = '71500'
                   SET I-81                TO TRUE
               END-IF
      * N81      EDBNR     COMP "5349656"                81 AVD. C
      * N81      EDBNR     COMP "5734363"                81 AVD. C
      * N81      EDBNR     COMP "5346088"                81 AVD. C
      * N81      EDBNR     COMP "5648017"                81 AVD. C
      * N81      EDBNR     COMP "6764924"                81 AVD. C KAFFE 2006
           END-IF
           IF  (NOT-I-81)
               SET NOT-I-81                TO TRUE
               IF  EDBNR = '7010397'
                   SET I-81                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-81)
               SET NOT-I-81                TO TRUE
               IF  EDBNR = '7300344'
                   SET I-81                TO TRUE
               END-IF
           END-IF
           IF  (I-81)
               GO TO HTVARE-T
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  VGRAVD = '1'
               SET I-81                    TO TRUE
           END-IF
           IF  (I-81)
               ADD NETTO                   TO STAVD1
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  VGRAVD = '2'
               SET I-81                    TO TRUE
           END-IF
           IF  (I-81)
               ADD NETTO                   TO STAVD2
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  VGRAVD = '3'
               SET I-81                    TO TRUE
           END-IF
           IF  (I-81)
               ADD NETTO                   TO STAVD3
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  VGRAVD = '4'
               SET I-81                    TO TRUE
           END-IF
           IF  (I-81)
               ADD NETTO                   TO STAVD4
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  VGRAVD = '5'
               SET I-81                    TO TRUE
           END-IF
           IF  (I-81)
               ADD NETTO                   TO STAVD5
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  VGRAVD = '6'
               SET I-81                    TO TRUE
           END-IF
           IF  (I-81)
               ADD NETTO                   TO STAVD6
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  VGRAVD = '7'
               SET I-81                    TO TRUE
           END-IF
           IF  (I-81)
               ADD NETTO                   TO STAVD7
           END-IF
           GO TO ENDST2-T
      *****************************************************************
      * TEST PÅ VAREGRUPPE.                                           *
      *****************************************************************
           .
 
       HTVARE-T.
           SET NOT-I-81                    TO TRUE
           IF  VGR = '76000'
               SET I-81                    TO TRUE
           END-IF
           IF  (I-81)
               ADD NETTO                   TO STAVD8
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  VGR = '76500'
               SET I-81                    TO TRUE
           END-IF
           IF  (NOT-I-81)
               SET NOT-I-81                TO TRUE
               IF  VGR = '75020'
                   SET I-81                TO TRUE
               END-IF
           END-IF
           IF  (I-81)
               ADD NETTO                   TO STAVD9
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  VGR = '10958'
               SET I-81                    TO TRUE
           END-IF
           IF  (NOT-I-81)
               SET NOT-I-81                TO TRUE
               IF  VGR = '10960'
                   SET I-81                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-81)
               SET NOT-I-81                TO TRUE
               IF  VGR = '10961'
                   SET I-81                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-81)
               SET NOT-I-81                TO TRUE
               IF  VGR = '11600'
                   SET I-81                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-81)
               SET NOT-I-81                TO TRUE
               IF  EDBNR = '4068947'
                   SET I-81                TO TRUE
               END-IF
           END-IF
           IF  (I-81)
               ADD NETTO                   TO STAVDA
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  VGR = '12100'
               SET I-81                    TO TRUE
           END-IF
           IF  (I-81)
               ADD NETTO                   TO STAVDB
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  VGR = '74000'
               SET I-81                    TO TRUE
           END-IF
           IF  (NOT-I-81)
               SET NOT-I-81                TO TRUE
               IF  VGR = '74001'
                   SET I-81                TO TRUE
               END-IF
      *          EDBNR     COMP "5349656"                81 AVD. C
      * N81      EDBNR     COMP "5734363"                81 AVD. C
      * N81      EDBNR     COMP "5346088"                81 AVD. C
      * N81      EDBNR     COMP "5648017"                81 AVD. C
      * N81      EDBNR     COMP "6764924"                81 AVD. C KAFFE 2006
           END-IF
           IF  (NOT-I-81)
               SET NOT-I-81                TO TRUE
               IF  EDBNR = '7010397'
                   SET I-81                TO TRUE
               END-IF
           END-IF
           IF  (I-81)
               ADD NETTO                   TO STAVDC
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  VGR = '71500'
               SET I-81                    TO TRUE
           END-IF
           IF  (I-81)
               ADD NETTO                   TO STAVDD
           END-IF.
 
       ENDST2-T.
      *****************************************************************
           CONTINUE.
 
       E1-T.
           CONTINUE.
 
       SLUTT-T.
           MOVE '1'                        TO PRFLT4 (3:1)
           MOVE FAKRFL                     TO PRFLT4 (1:2)
           MOVE PRFLT4                     TO PRFLT5 (7:3)
           MOVE BRKODE                     TO PRFLT5 (1:6)
           IF  (NOT-I-14)
               MOVE AREA1                  TO PRARE1-IO
           END-IF
           IF  (I-14)
               MOVE AREA2                  TO PRARE1-IO
           END-IF
      ** MLLzo
           IF PRARE1 < 0
               MULTIPLY -1 BY PRARE1
           END-IF
      *****************************************************************
      * BEREGNE MODULUS 10 KONTROLLSIFFER FOR FAKTURA-KID             *
      *****************************************************************
           MOVE PRARE1                     TO FAKKID (1:6)
           MOVE '0'                        TO FAKKID (7:1)
           MOVE FAKKID                     TO KITALL (1:7)
           MOVE 'T'                        TO KITYPE
           MOVE ' '                        TO KIKTR
           CALL 'MODULUS' USING KIFELT-XX-DATA-FIELDS
           MOVE PRARE1                     TO FAKKID (1:6)
           MOVE KIKTR                      TO FAKKID (7:1)
      *****************************************************************
           MOVE FOFALL (5:2)               TO FFAAR
           MOVE FOFALL (1:4)               TO FFREST
           MOVE FFREST (3:2)               TO FFMND
           MOVE FFREST (1:2)               TO FFDAG
           SET NOT-I-21                    TO TRUE
           IF  FAKRNR = 'K'
               SET I-21                    TO TRUE
           END-IF
           IF  (I-21)
               GO TO X1A-T
           END-IF
           SET NOT-I-84                    TO TRUE
           IF  NETTO < 0,00
               SET I-84                    TO TRUE
           END-IF
           IF  (I-84)
               GO TO X1B-T
           END-IF
           MOVE ' '                        TO SIGNX1
           MOVE NETTO                      TO BXBEL-IO
           GO TO X1C-T.
 
       X1B-T.
           MULTIPLY -1 BY NETTO        GIVING BXBEL
           MOVE '-'                        TO SIGNX1
           GO TO X1C-T.
 
       X1A-T.
           SET NOT-I-85                    TO TRUE
           IF  NETTO < 0,00
               SET I-85                    TO TRUE
           END-IF
           IF  (I-85)
               GO TO X1D-T
           END-IF
           MOVE '-'                        TO SIGNX1
           MOVE NETTO                      TO BXBEL-IO
           GO TO X1C-T.
 
       X1D-T.
           MULTIPLY -1 BY NETTO        GIVING BXBEL
           MOVE ' '                        TO SIGNX1.
 
       X1C-T.
      ** MLLzo
           IF BXBEL < 0
               MULTIPLY -1 BY BXBEL
           END-IF
      ******************************************************
           .
 
       FRARUT-S SECTION.
       FRARUT-S-P.
           SET NOT-I-63                    TO TRUE
           IF  RESKNR = '108801'
               SET I-63                    TO TRUE
           END-IF
           IF  (NOT-I-63)
               SET NOT-I-63                TO TRUE
               IF  RESKNR = '108802'
                   SET I-63                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-63)
               GO TO ENDFRA-T
           END-IF
           SET NOT-I-63                    TO TRUE
           IF  VGR = '19010'
               SET I-63                    TO TRUE
           END-IF
           IF  (NOT-I-63)
               GO TO ENDFRA-T
           END-IF
           SET NOT-I-63                    TO TRUE
           IF  EDBNR = '9000100'
               SET I-63                    TO TRUE
           END-IF
           IF  (NOT-I-63)
               GO TO ENDFRA-T
           END-IF
           ADD NETTO                       TO NETFRA.
 
       ENDFRA-T.
           CONTINUE.
      ****************************************************************
      *  SUBRUTINE FOR Å BEREGNE DELSUMMER MED MOMS UTEN AVRUNDING.  *
      *  DENNE BRUKES AV SCANGROSS SINE  ESSO-KUNDER.                *
      *  TATT I BRUK 1. GANG PR. 31.03.2003                          *
      ****************************************************************
 
       STAMVA-S SECTION.
       STAMVA-S-P.
           ADD STAVD TO ZERO           GIVING BUMVA
           MOVE 0                          TO BMMVA
           MOVE 99,99                      TO MVA
           IF  (I-97)
               GO TO SMOM3-T
           END-IF
           CALL 'RUTMOMS2' USING MVFELT-XX-DATA-FIELDS
           ADD BMMVA TO ZERO           GIVING STAVD
           GO TO SMOM3X-T.
 
       SMOM3-T.
           CALL 'MOMSRUT9' USING MVFELT-XX-DATA-FIELDS
           ADD BMMVA TO ZERO           GIVING STAVD.
 
       SMOM3X-T.
           CONTINUE.
      ****************************************************************
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1)
               SET I-15                    TO TRUE
           END-IF
           IF  (I-L1 AND NOT-I-14)
               MOVE AREA1                  TO AREA8X-IO
           END-IF
           IF  (I-L1 AND I-14)
               MOVE AREA2                  TO AREA8X-IO
           END-IF
           IF  (I-L1)
      ** MLLzo
               IF AREA8X < 0
                   MULTIPLY -1 BY AREA8X
               END-IF
           END-IF
           IF  (I-L2)
               SET NOT-I-15                TO TRUE
               SET I-42                    TO TRUE
      *****************************************************************
      * RUTINE FOR Å SKRIVE UT AVDELINGSTOTALER. HAFNOR/FINA  86      *
      * RUTINE FOR Å SKRIVE UT AVD.TOT. SCANGROSS/SHELL 94 N34        *
      * RUTINE FOR Å SKRIVE UT AVD.TOT. SCANGROSS/ESSO  94  34        *
      *****************************************************************
           END-IF
           IF  (I-L2 AND NOT-I-86 AND NOT-I-94)
               GO TO ENDST3-T
           END-IF
           IF  (I-L2)
               SET NOT-I-44                TO TRUE
               IF  FRITT = '5'
                   SET I-44                TO TRUE
               END-IF
               MOVE 10                     TO SEQ2
      *
           END-IF
           IF  (I-L2)
               SET NOT-I-83                TO TRUE
               ADD STAVD1 TO ZERO      GIVING STAVD
               SET NOT-I-81                TO TRUE
               IF  STAVD NOT = 0
                   SET I-81                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-81 AND I-34 AND NOT-I-44)
               PERFORM STAMVA-S
           END-IF
           IF  (I-L2 AND I-81 AND I-86)
               MOVE '9501'                 TO A1
           END-IF
           IF  (I-L2 AND I-81 AND I-94)
               MOVE '9231'                 TO A1
           END-IF
           IF  (I-L2 AND I-81 AND I-34)
               MOVE 'ESS1'                 TO A1
           END-IF
           IF  (I-L2 AND I-81)
               SET NOT-I-83                TO TRUE
               SET TABAVD-S                TO TABAVD-I
               PERFORM WITH TEST AFTER
                       VARYING TABAVD-I FROM 1 BY 1
                         UNTIL TABAVD-I >= TABAVD-MAX
                            OR I-83
                   IF  A1 = TABAVD (TABAVD-I)
                       SET I-83            TO TRUE
                       SET TABAVD-S        TO TABAVD-I
                   END-IF
               END-PERFORM
               SET TABAVD-I                TO TABAVD-S
               IF  I-83
               AND TABAVD-I NOT > TABAVN-MAX
                   SET TABAVN-I            TO TABAVD-I
               END-IF
           END-IF
           IF  (I-L2 AND I-83)
               ADD 1                       TO SEQ2
               PERFORM EXCEPTION-OUTPUT
      *
           END-IF
           IF  (I-L2)
               SET NOT-I-83                TO TRUE
               ADD STAVD2 TO ZERO      GIVING STAVD
               SET NOT-I-81                TO TRUE
               IF  STAVD NOT = 0
                   SET I-81                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-81 AND I-34 AND NOT-I-44)
               PERFORM STAMVA-S
           END-IF
           IF  (I-L2 AND I-81 AND I-86)
               MOVE '9502'                 TO A1
           END-IF
           IF  (I-L2 AND I-81 AND I-94)
               MOVE '9232'                 TO A1
           END-IF
           IF  (I-L2 AND I-81 AND I-34)
               MOVE 'ESS2'                 TO A1
           END-IF
           IF  (I-L2 AND I-81)
               SET NOT-I-83                TO TRUE
               SET TABAVD-S                TO TABAVD-I
               PERFORM WITH TEST AFTER
                       VARYING TABAVD-I FROM 1 BY 1
                         UNTIL TABAVD-I >= TABAVD-MAX
                            OR I-83
                   IF  A1 = TABAVD (TABAVD-I)
                       SET I-83            TO TRUE
                       SET TABAVD-S        TO TABAVD-I
                   END-IF
               END-PERFORM
               SET TABAVD-I                TO TABAVD-S
               IF  I-83
               AND TABAVD-I NOT > TABAVN-MAX
                   SET TABAVN-I            TO TABAVD-I
               END-IF
           END-IF
           IF  (I-L2 AND I-83)
               ADD 1                       TO SEQ2
               PERFORM EXCEPTION-OUTPUT
      *
           END-IF
           IF  (I-L2)
               SET NOT-I-83                TO TRUE
               ADD STAVD3 TO ZERO      GIVING STAVD
               SET NOT-I-81                TO TRUE
               IF  STAVD NOT = 0
                   SET I-81                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-81 AND I-34 AND NOT-I-44)
               PERFORM STAMVA-S
           END-IF
           IF  (I-L2 AND I-81 AND I-86)
               MOVE '9503'                 TO A1
           END-IF
           IF  (I-L2 AND I-81 AND I-94)
               MOVE '9233'                 TO A1
           END-IF
           IF  (I-L2 AND I-81 AND I-34)
               MOVE 'ESS3'                 TO A1
           END-IF
           IF  (I-L2 AND I-81)
               SET NOT-I-83                TO TRUE
               SET TABAVD-S                TO TABAVD-I
               PERFORM WITH TEST AFTER
                       VARYING TABAVD-I FROM 1 BY 1
                         UNTIL TABAVD-I >= TABAVD-MAX
                            OR I-83
                   IF  A1 = TABAVD (TABAVD-I)
                       SET I-83            TO TRUE
                       SET TABAVD-S        TO TABAVD-I
                   END-IF
               END-PERFORM
               SET TABAVD-I                TO TABAVD-S
               IF  I-83
               AND TABAVD-I NOT > TABAVN-MAX
                   SET TABAVN-I            TO TABAVD-I
               END-IF
           END-IF
           IF  (I-L2 AND I-83)
               ADD 1                       TO SEQ2
               PERFORM EXCEPTION-OUTPUT
      *
           END-IF
           IF  (I-L2)
               SET NOT-I-83                TO TRUE
               ADD STAVD4 TO ZERO      GIVING STAVD
               SET NOT-I-81                TO TRUE
               IF  STAVD NOT = 0
                   SET I-81                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-81 AND I-34 AND NOT-I-44)
               PERFORM STAMVA-S
           END-IF
           IF  (I-L2 AND I-81 AND I-86)
               MOVE '9504'                 TO A1
           END-IF
           IF  (I-L2 AND I-81 AND I-94)
               MOVE '9234'                 TO A1
           END-IF
           IF  (I-L2 AND I-81 AND I-34)
               MOVE 'ESS4'                 TO A1
           END-IF
           IF  (I-L2 AND I-81)
               SET NOT-I-83                TO TRUE
               SET TABAVD-S                TO TABAVD-I
               PERFORM WITH TEST AFTER
                       VARYING TABAVD-I FROM 1 BY 1
                         UNTIL TABAVD-I >= TABAVD-MAX
                            OR I-83
                   IF  A1 = TABAVD (TABAVD-I)
                       SET I-83            TO TRUE
                       SET TABAVD-S        TO TABAVD-I
                   END-IF
               END-PERFORM
               SET TABAVD-I                TO TABAVD-S
               IF  I-83
               AND TABAVD-I NOT > TABAVN-MAX
                   SET TABAVN-I            TO TABAVD-I
               END-IF
           END-IF
           IF  (I-L2 AND I-83)
               ADD 1                       TO SEQ2
               PERFORM EXCEPTION-OUTPUT
      *
           END-IF
           IF  (I-L2)
               SET NOT-I-83                TO TRUE
               ADD STAVD5 TO ZERO      GIVING STAVD
               SET NOT-I-81                TO TRUE
               IF  STAVD NOT = 0
                   SET I-81                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-81 AND I-34 AND NOT-I-44)
               PERFORM STAMVA-S
           END-IF
           IF  (I-L2 AND I-81 AND I-86)
               MOVE '9505'                 TO A1
           END-IF
           IF  (I-L2 AND I-81 AND I-94)
               MOVE '9235'                 TO A1
           END-IF
           IF  (I-L2 AND I-81 AND I-34)
               MOVE 'ESS5'                 TO A1
           END-IF
           IF  (I-L2 AND I-81)
               SET NOT-I-83                TO TRUE
               SET TABAVD-S                TO TABAVD-I
               PERFORM WITH TEST AFTER
                       VARYING TABAVD-I FROM 1 BY 1
                         UNTIL TABAVD-I >= TABAVD-MAX
                            OR I-83
                   IF  A1 = TABAVD (TABAVD-I)
                       SET I-83            TO TRUE
                       SET TABAVD-S        TO TABAVD-I
                   END-IF
               END-PERFORM
               SET TABAVD-I                TO TABAVD-S
               IF  I-83
               AND TABAVD-I NOT > TABAVN-MAX
                   SET TABAVN-I            TO TABAVD-I
               END-IF
           END-IF
           IF  (I-L2 AND I-83)
               ADD 1                       TO SEQ2
               PERFORM EXCEPTION-OUTPUT
      *
           END-IF
           IF  (I-L2)
               SET NOT-I-83                TO TRUE
               ADD STAVD6 TO ZERO      GIVING STAVD
               SET NOT-I-81                TO TRUE
               IF  STAVD NOT = 0
                   SET I-81                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-81 AND I-34 AND NOT-I-44)
               PERFORM STAMVA-S
           END-IF
           IF  (I-L2 AND I-81 AND I-86)
               MOVE '9506'                 TO A1
           END-IF
           IF  (I-L2 AND I-81 AND I-94)
               MOVE '9236'                 TO A1
           END-IF
           IF  (I-L2 AND I-81 AND I-34)
               MOVE 'ESS6'                 TO A1
           END-IF
           IF  (I-L2 AND I-81)
               SET NOT-I-83                TO TRUE
               SET TABAVD-S                TO TABAVD-I
               PERFORM WITH TEST AFTER
                       VARYING TABAVD-I FROM 1 BY 1
                         UNTIL TABAVD-I >= TABAVD-MAX
                            OR I-83
                   IF  A1 = TABAVD (TABAVD-I)
                       SET I-83            TO TRUE
                       SET TABAVD-S        TO TABAVD-I
                   END-IF
               END-PERFORM
               SET TABAVD-I                TO TABAVD-S
               IF  I-83
               AND TABAVD-I NOT > TABAVN-MAX
                   SET TABAVN-I            TO TABAVD-I
               END-IF
           END-IF
           IF  (I-L2 AND I-83)
               ADD 1                       TO SEQ2
               PERFORM EXCEPTION-OUTPUT
      *
           END-IF
           IF  (I-L2)
               SET NOT-I-83                TO TRUE
               ADD STAVD7 TO ZERO      GIVING STAVD
               SET NOT-I-81                TO TRUE
               IF  STAVD NOT = 0
                   SET I-81                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-81 AND I-34 AND NOT-I-44)
               PERFORM STAMVA-S
           END-IF
           IF  (I-L2 AND I-81 AND I-86)
               MOVE '9507'                 TO A1
           END-IF
           IF  (I-L2 AND I-81 AND I-94)
               MOVE '9237'                 TO A1
           END-IF
           IF  (I-L2 AND I-81 AND I-34)
               MOVE 'ESS7'                 TO A1
           END-IF
           IF  (I-L2 AND I-81)
               SET NOT-I-83                TO TRUE
               SET TABAVD-S                TO TABAVD-I
               PERFORM WITH TEST AFTER
                       VARYING TABAVD-I FROM 1 BY 1
                         UNTIL TABAVD-I >= TABAVD-MAX
                            OR I-83
                   IF  A1 = TABAVD (TABAVD-I)
                       SET I-83            TO TRUE
                       SET TABAVD-S        TO TABAVD-I
                   END-IF
               END-PERFORM
               SET TABAVD-I                TO TABAVD-S
               IF  I-83
               AND TABAVD-I NOT > TABAVN-MAX
                   SET TABAVN-I            TO TABAVD-I
               END-IF
           END-IF
           IF  (I-L2 AND I-83)
               ADD 1                       TO SEQ2
               PERFORM EXCEPTION-OUTPUT
           END-IF
           IF  (I-L2)
               SET NOT-I-83                TO TRUE
      *
           END-IF
           IF  (I-L2)
               SET NOT-I-83                TO TRUE
               ADD STAVD8 TO ZERO      GIVING STAVD
               SET NOT-I-81                TO TRUE
               IF  STAVD NOT = 0
                   SET I-81                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-81 AND I-34 AND NOT-I-44)
               PERFORM STAMVA-S
           END-IF
           IF  (I-L2 AND I-81 AND I-86)
               MOVE '9508'                 TO A1
           END-IF
           IF  (I-L2 AND I-81 AND I-94)
               MOVE '9238'                 TO A1
           END-IF
           IF  (I-L2 AND I-81 AND I-34)
               MOVE 'ESS8'                 TO A1
           END-IF
           IF  (I-L2 AND I-81)
               SET NOT-I-83                TO TRUE
               SET TABAVD-S                TO TABAVD-I
               PERFORM WITH TEST AFTER
                       VARYING TABAVD-I FROM 1 BY 1
                         UNTIL TABAVD-I >= TABAVD-MAX
                            OR I-83
                   IF  A1 = TABAVD (TABAVD-I)
                       SET I-83            TO TRUE
                       SET TABAVD-S        TO TABAVD-I
                   END-IF
               END-PERFORM
               SET TABAVD-I                TO TABAVD-S
               IF  I-83
               AND TABAVD-I NOT > TABAVN-MAX
                   SET TABAVN-I            TO TABAVD-I
               END-IF
           END-IF
           IF  (I-L2 AND I-83)
               ADD 1                       TO SEQ2
               PERFORM EXCEPTION-OUTPUT
           END-IF
           IF  (I-L2)
               SET NOT-I-83                TO TRUE
      *
           END-IF
           IF  (I-L2)
               SET NOT-I-83                TO TRUE
               ADD STAVD9 TO ZERO      GIVING STAVD
               SET NOT-I-81                TO TRUE
               IF  STAVD NOT = 0
                   SET I-81                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-81 AND I-34 AND NOT-I-44)
               PERFORM STAMVA-S
           END-IF
           IF  (I-L2 AND I-81 AND I-86)
               MOVE '9509'                 TO A1
           END-IF
           IF  (I-L2 AND I-81 AND I-94)
               MOVE '9239'                 TO A1
           END-IF
           IF  (I-L2 AND I-81 AND I-34)
               MOVE 'ESS9'                 TO A1
           END-IF
           IF  (I-L2 AND I-81)
               SET NOT-I-83                TO TRUE
               SET TABAVD-S                TO TABAVD-I
               PERFORM WITH TEST AFTER
                       VARYING TABAVD-I FROM 1 BY 1
                         UNTIL TABAVD-I >= TABAVD-MAX
                            OR I-83
                   IF  A1 = TABAVD (TABAVD-I)
                       SET I-83            TO TRUE
                       SET TABAVD-S        TO TABAVD-I
                   END-IF
               END-PERFORM
               SET TABAVD-I                TO TABAVD-S
               IF  I-83
               AND TABAVD-I NOT > TABAVN-MAX
                   SET TABAVN-I            TO TABAVD-I
               END-IF
           END-IF
           IF  (I-L2 AND I-83)
               ADD 1                       TO SEQ2
               PERFORM EXCEPTION-OUTPUT
           END-IF
           IF  (I-L2)
               SET NOT-I-83                TO TRUE
      *
           END-IF
           IF  (I-L2)
               SET NOT-I-83                TO TRUE
               ADD STAVDA TO ZERO      GIVING STAVD
               SET NOT-I-81                TO TRUE
               IF  STAVD NOT = 0
                   SET I-81                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-81 AND I-34 AND NOT-I-44)
               PERFORM STAMVA-S
           END-IF
           IF  (I-L2 AND I-81 AND I-86)
               MOVE '950A'                 TO A1
           END-IF
           IF  (I-L2 AND I-81 AND I-94)
               MOVE '923A'                 TO A1
           END-IF
           IF  (I-L2 AND I-81 AND I-34)
               MOVE 'ESSA'                 TO A1
           END-IF
           IF  (I-L2 AND I-81)
               SET NOT-I-83                TO TRUE
               SET TABAVD-S                TO TABAVD-I
               PERFORM WITH TEST AFTER
                       VARYING TABAVD-I FROM 1 BY 1
                         UNTIL TABAVD-I >= TABAVD-MAX
                            OR I-83
                   IF  A1 = TABAVD (TABAVD-I)
                       SET I-83            TO TRUE
                       SET TABAVD-S        TO TABAVD-I
                   END-IF
               END-PERFORM
               SET TABAVD-I                TO TABAVD-S
               IF  I-83
               AND TABAVD-I NOT > TABAVN-MAX
                   SET TABAVN-I            TO TABAVD-I
               END-IF
           END-IF
           IF  (I-L2 AND I-83)
               ADD 1                       TO SEQ2
               PERFORM EXCEPTION-OUTPUT
           END-IF
           IF  (I-L2)
               SET NOT-I-83                TO TRUE
      *
           END-IF
           IF  (I-L2)
               SET NOT-I-83                TO TRUE
               ADD STAVDB TO ZERO      GIVING STAVD
               SET NOT-I-81                TO TRUE
               IF  STAVD NOT = 0
                   SET I-81                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-81 AND I-34 AND NOT-I-44)
               PERFORM STAMVA-S
           END-IF
           IF  (I-L2 AND I-81 AND I-86)
               MOVE '950B'                 TO A1
           END-IF
           IF  (I-L2 AND I-81 AND I-94)
               MOVE '923B'                 TO A1
           END-IF
           IF  (I-L2 AND I-81 AND I-34)
               MOVE 'ESSB'                 TO A1
           END-IF
           IF  (I-L2 AND I-81)
               SET NOT-I-83                TO TRUE
               SET TABAVD-S                TO TABAVD-I
               PERFORM WITH TEST AFTER
                       VARYING TABAVD-I FROM 1 BY 1
                         UNTIL TABAVD-I >= TABAVD-MAX
                            OR I-83
                   IF  A1 = TABAVD (TABAVD-I)
                       SET I-83            TO TRUE
                       SET TABAVD-S        TO TABAVD-I
                   END-IF
               END-PERFORM
               SET TABAVD-I                TO TABAVD-S
               IF  I-83
               AND TABAVD-I NOT > TABAVN-MAX
                   SET TABAVN-I            TO TABAVD-I
               END-IF
           END-IF
           IF  (I-L2 AND I-83)
               ADD 1                       TO SEQ2
               PERFORM EXCEPTION-OUTPUT
           END-IF
           IF  (I-L2)
               SET NOT-I-83                TO TRUE
      *
           END-IF
           IF  (I-L2)
               SET NOT-I-83                TO TRUE
               ADD STAVDC TO ZERO      GIVING STAVD
               SET NOT-I-81                TO TRUE
               IF  STAVD NOT = 0
                   SET I-81                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-81 AND I-34 AND NOT-I-44)
               PERFORM STAMVA-S
           END-IF
           IF  (I-L2 AND I-81 AND I-86)
               MOVE '950C'                 TO A1
           END-IF
           IF  (I-L2 AND I-81 AND I-94)
               MOVE '923C'                 TO A1
           END-IF
           IF  (I-L2 AND I-81 AND I-34)
               MOVE 'ESSC'                 TO A1
           END-IF
           IF  (I-L2 AND I-81)
               SET NOT-I-83                TO TRUE
               SET TABAVD-S                TO TABAVD-I
               PERFORM WITH TEST AFTER
                       VARYING TABAVD-I FROM 1 BY 1
                         UNTIL TABAVD-I >= TABAVD-MAX
                            OR I-83
                   IF  A1 = TABAVD (TABAVD-I)
                       SET I-83            TO TRUE
                       SET TABAVD-S        TO TABAVD-I
                   END-IF
               END-PERFORM
               SET TABAVD-I                TO TABAVD-S
               IF  I-83
               AND TABAVD-I NOT > TABAVN-MAX
                   SET TABAVN-I            TO TABAVD-I
               END-IF
           END-IF
           IF  (I-L2 AND I-83)
               ADD 1                       TO SEQ2
               PERFORM EXCEPTION-OUTPUT
           END-IF
           IF  (I-L2)
               SET NOT-I-83                TO TRUE
      *
           END-IF
           IF  (I-L2)
               SET NOT-I-83                TO TRUE
               ADD STAVDD TO ZERO      GIVING STAVD
               SET NOT-I-81                TO TRUE
               IF  STAVD NOT = 0
                   SET I-81                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-81 AND I-34 AND NOT-I-44)
               PERFORM STAMVA-S
           END-IF
           IF  (I-L2 AND I-81 AND I-86)
               MOVE '950D'                 TO A1
           END-IF
           IF  (I-L2 AND I-81 AND I-94)
               MOVE '923C'                 TO A1
           END-IF
           IF  (I-L2 AND I-81 AND I-34)
               MOVE 'ESSC'                 TO A1
           END-IF
           IF  (I-L2 AND I-81)
               SET NOT-I-83                TO TRUE
               SET TABAVD-S                TO TABAVD-I
               PERFORM WITH TEST AFTER
                       VARYING TABAVD-I FROM 1 BY 1
                         UNTIL TABAVD-I >= TABAVD-MAX
                            OR I-83
                   IF  A1 = TABAVD (TABAVD-I)
                       SET I-83            TO TRUE
                       SET TABAVD-S        TO TABAVD-I
                   END-IF
               END-PERFORM
               SET TABAVD-I                TO TABAVD-S
               IF  I-83
               AND TABAVD-I NOT > TABAVN-MAX
                   SET TABAVN-I            TO TABAVD-I
               END-IF
           END-IF
           IF  (I-L2 AND I-83)
               ADD 1                       TO SEQ2
               PERFORM EXCEPTION-OUTPUT
           END-IF
           IF  (I-L2)
               SET NOT-I-83                TO TRUE
           END-IF.
 
       ENDST3-T.
      *****************************************************************
           IF  (I-L2)
               SET NOT-I-67                TO TRUE
               IF  NETFRA NOT = 0,00
                   SET I-67                TO TRUE
               END-IF
               MOVE '3'                    TO PRFLT5 (9:1)
               SET NOT-I-44                TO TRUE
               IF  FRITT = '5'
                   SET I-44                TO TRUE
               END-IF
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
           END-IF
           IF  (I-L2)
               MOVE 0,00                   TO AVRORE
           END-IF
           IF  (I-L2 AND I-44)
               GO TO F1-T
      *****************************************************************
      * SUBRUTINE FOR BEREGNING AV FAKTURATOTAL MED AVRUNDING.        *
      *****************************************************************
           END-IF
           IF  (I-L2)
               ADD NETBNR TO ZERO      GIVING BUMVA
               MOVE 0                      TO BMMVA
               MOVE 0                      TO MVA
           END-IF
           IF  (I-L2 AND I-97)
               GO TO SMOM1-T
           END-IF
           IF  (I-L2)
               CALL 'RUTMOMS2' USING MVFELT-XX-DATA-FIELDS
               ADD MVA TO ZERO         GIVING MOMSAR
               GO TO SMOM1X-T
           END-IF.
 
       SMOM1-T.
           IF  (I-L2)
               CALL 'MOMSRUT9' USING MVFELT-XX-DATA-FIELDS
               ADD MVA TO ZERO         GIVING MOMSAR
           END-IF.
 
       SMOM1X-T.
      *****************************************************************
      * SUBRUTINE FOR BEREGNING AV MOMS UTEN AVRUNDING.               *
      *****************************************************************
           IF  (I-L2)
               ADD NETBNR TO ZERO      GIVING BUMVA
               MOVE 0                      TO BMMVA
               MOVE 99,99                  TO MVA
           END-IF
           IF  (I-L2 AND I-97)
               GO TO SMOM2-T
           END-IF
           IF  (I-L2)
               CALL 'RUTMOMS2' USING MVFELT-XX-DATA-FIELDS
               ADD MVA TO ZERO         GIVING MOMS
               GO TO SMOM2X-T
           END-IF.
 
       SMOM2-T.
           IF  (I-L2)
               CALL 'MOMSRUT9' USING MVFELT-XX-DATA-FIELDS
               ADD MVA TO ZERO         GIVING MOMS
           END-IF.
 
       SMOM2X-T.
           IF  (I-L2)
               MOVE MOMS                   TO MOMSX1-IO
      ** MLLzo
               IF MOMSX1 < 0
                   MULTIPLY -1 BY MOMSX1
               END-IF
      *****************************************************************
      * BEREGNING AV ØREAVRUNDING.                                    *
      *****************************************************************
           END-IF
           IF  (I-L2)
               SUBTRACT MOMS FROM MOMSAR GIVING AVRORE
      *****************************************************************
           END-IF
           IF  (I-L2)
               SET NOT-I-21                TO TRUE
               IF  FAKRNR = 'K'
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-21)
               GO TO EX97A-T
           END-IF
           IF  (I-L2)
               SET NOT-I-17                TO TRUE
               IF  MOMS < 0,00
                   SET I-17                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-17)
               MOVE '-'                    TO SIGNXM
           END-IF
           IF  (I-L2 AND NOT-I-17)
               MOVE ' '                    TO SIGNXM
           END-IF
           IF  (I-L2)
               ADD MOMS                    TO TOTMOM
               SET NOT-I-17                TO TRUE
               IF  AVRORE < 0,00
                   SET I-17                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-17)
               MOVE '-'                    TO SIGNAV
           END-IF
           IF  (I-L2 AND NOT-I-17)
               MOVE ' '                    TO SIGNAV
           END-IF
           IF  (I-L2)
               ADD AVRORE                  TO TOTAVR
               ADD AVRORE                  TO TOTAF5
               GO TO EX97B-T
           END-IF.
 
       EX97A-T.
           IF  (I-L2)
               SET NOT-I-17                TO TRUE
               IF  MOMS < 0,00
                   SET I-17                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-17)
               MOVE ' '                    TO SIGNXM
           END-IF
           IF  (I-L2 AND NOT-I-17)
               MOVE '-'                    TO SIGNXM
           END-IF
           IF  (I-L2)
               SUBTRACT MOMS               FROM TOTMOM
               SET NOT-I-17                TO TRUE
               IF  AVRORE < 0,00
                   SET I-17                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-17)
               MOVE ' '                    TO SIGNAV
           END-IF
           IF  (I-L2 AND NOT-I-17)
               MOVE '-'                    TO SIGNAV
           END-IF
           IF  (I-L2)
               SUBTRACT AVRORE             FROM TOTAVR
           END-IF.
 
       EX97B-T.
           IF  (I-L2)
               ADD MOMS TO NETBNR      GIVING TOTAL
               ADD AVRORE                  TO TOTAL
               ADD AVRORE                  TO TOTAF5
               MOVE TOTAL (2:9)            TO BYBEL-IO
               SET I-50                    TO TRUE
               SET I-45                    TO TRUE
               SET I-47                    TO TRUE
           END-IF
           IF  (I-L2 AND I-14)
               ADD MOMS                    TO KRESUM
               ADD AVRORE                  TO KRESUM
           END-IF
           IF  (I-L2 AND NOT-I-14)
               ADD MOMS                    TO FAKSUM
               ADD AVRORE                  TO FAKSUM
           END-IF.
 
       F1-T.
           IF  (I-L2 AND I-14)
               ADD NETBNR                  TO KRESUM
           END-IF
           IF  (I-L2 AND NOT-I-14)
               ADD NETBNR                  TO FAKSUM
           END-IF
           IF  (I-L2 AND NOT-I-44)
               GO TO F2-T
           END-IF
           IF  (I-L2)
               MOVE NETBNR                 TO BYBEL-IO
               SET I-50                    TO TRUE
           END-IF.
 
       F2-T.
           IF  (I-L2 AND NOT-I-14)
               MOVE AREA1                  TO AREA9X-IO
           END-IF
           IF  (I-L2 AND I-14)
               MOVE AREA2                  TO AREA9X-IO
           END-IF
           IF  (I-L2)
      ** MLLzo
               IF AREA9X < 0
                   MULTIPLY -1 BY AREA9X
               END-IF
               SET NOT-I-16                TO TRUE
               SET TESTN-STATE-INIT        TO TRUE
               PERFORM WITH TEST AFTER
                 VARYING TESTN-INDEX FROM 1 BY 1
                   UNTIL TESTN-INDEX = 6
                   EVALUATE TRUE
                   WHEN TESTN-STATE-INIT
                       IF  FAKREF (TESTN-INDEX:1) NUMERIC
                           SET TESTN-STATE-NUMBER TO TRUE
                       ELSE
                           IF  FAKREF (TESTN-INDEX:1) = SPACES
                               SET TESTN-STATE-SPACE TO TRUE
                           ELSE
                               SET TESTN-STATE-OTHER TO TRUE
                           END-IF
                       END-IF
                   WHEN TESTN-STATE-NUMBER
                       IF  FAKREF (TESTN-INDEX:1) NOT NUMERIC
                           SET TESTN-STATE-OTHER TO TRUE
                       END-IF
                   WHEN TESTN-STATE-SPACE
                       IF  FAKREF (TESTN-INDEX:1) NUMERIC
                           SET TESTN-STATE-LEADSPACE TO TRUE
                       ELSE
                           IF  FAKREF (TESTN-INDEX:1) NOT = SPACES
                               SET TESTN-STATE-OTHER TO TRUE
                           END-IF
                       END-IF
                   WHEN TESTN-STATE-LEADSPACE
                       IF  FAKREF (TESTN-INDEX:1) NOT NUMERIC
                           SET TESTN-STATE-OTHER TO TRUE
                       END-IF
                   END-EVALUATE
               END-PERFORM
               IF  TESTN-STATE-NUMBER
                   SET I-16                TO TRUE
               END-IF
               SET NOT-I-87                TO TRUE
               IF  FAKRNR = 'K'
                   SET I-87                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-87)
               MULTIPLY -1 BY BYBEL    GIVING BYBEL
           END-IF
           IF  (I-L2 AND NOT-I-87)
               MULTIPLY 1 BY BYBEL     GIVING BYBEL
           END-IF
           IF  (I-L2)
               ADD BYBEL                   TO TOTSUM
               SET NOT-I-88                TO TRUE
               IF  BTMA-ELGTE = CBETM
                   SET I-88                TO TRUE
               END-IF
      *****************************************************************
      * RUTINE FOR SJEKKE OM AVRUNDINGSRECORD SKAL DANNES.            *
      *        OG SNU BELØPET I REGNSKAPSRECORD.                      *
      *****************************************************************
           END-IF
           IF  (I-L2)
               SET NOT-I-35                TO TRUE
               IF  AVRORE NOT = 0,00
                   SET I-35                TO TRUE
               END-IF
               MOVE AVRORE                 TO OREDF1-IO (5:5)
      ** MLLzo
               IF OREDF1 < 0
                   MULTIPLY -1 BY OREDF1
               END-IF
      *********************************************************
      * FAKT. KR.NOTA NUMMERTILDELING SAMT TELLING AV ANTALL. *
      *********************************************************
           END-IF
           IF  (I-L2)
               ADD 1                       TO ANTL2
               ADD 1                       TO ANTLR
           END-IF
           IF  (I-L2 AND NOT-I-14)
               ADD 1                       TO AREA1
           END-IF
           IF  (I-L2 AND I-14)
               ADD 1                       TO AREA2
           END-IF
           IF  (I-L2 AND NOT-I-14)
               MOVE AREA1                  TO PRARE1-IO
           END-IF
           IF  (I-L2 AND I-14)
               MOVE AREA2                  TO PRARE1-IO
           END-IF
           IF  (I-L2)
      ** MLLzo
               IF PRARE1 < 0
                   MULTIPLY -1 BY PRARE1
               END-IF
      ******************************************************
      **   RUTINE FOR DANNING AV FAKTURAREFERANSER.       **
      ******************************************************
           END-IF
           IF  (I-L2)
               SET NOT-I-52                TO TRUE
               IF  RESKNR < '500180'
                   SET I-52                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-52)
               SET NOT-I-52                TO TRUE
               IF  RESKNR > '500500'
                   SET I-52                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-52)
               MOVE PFADTO                 TO FAKREF
           END-IF
           IF  (I-L2 AND NOT-I-52 AND I-95)
               MOVE UFADTO                 TO FAKREF
           END-IF
           IF  (I-L2 AND NOT-I-52)
               SET I-16                    TO TRUE
      ********************************************************
      * * *  BEREGNING AV SISTE FAKTURA/KREDITNOTA-NR. PR FIRMNR.   * * *
           END-IF
           IF  (I-L5)
               SUBTRACT 1                  FROM AREA1
               SUBTRACT 1                  FROM AREA2
               MOVE AREA1                  TO PRARE1-IO
      ** MLLzo
               IF PRARE1 < 0
                   MULTIPLY -1 BY PRARE1
               END-IF
               MOVE AREA2                  TO PRARE2-IO
      ** MLLzo
               IF PRARE2 < 0
                   MULTIPLY -1 BY PRARE2
               END-IF
      * * *  ADDERING AV TOTALT ANT. FAKTURA OG KREDITNOTA  * * *
           END-IF
           IF  (I-L5)
               SUBTRACT PFANR1 FROM PRARE1 GIVING ANTFAK
               ADD 1                       TO ANTFAK
               SET NOT-I-75                TO TRUE
               IF  ANTFAK < 0
                   SET I-75                TO TRUE
               END-IF
           END-IF
           IF  (I-L5 AND NOT-I-75)
               ADD ANTFAK                  TO TOTAFA
           END-IF
           IF  (I-L5)
               SUBTRACT PKRNR1 FROM PRARE2 GIVING ANTKRE
               ADD 1                       TO ANTKRE
               SET NOT-I-75                TO TRUE
               IF  ANTKRE < 0
                   SET I-75                TO TRUE
               END-IF
           END-IF
           IF  (I-L5 AND NOT-I-75)
               ADD ANTKRE                  TO TOTAKR
      *****************************************************************
      *  SUBRUTINE FOR Å SUMMERE FRAKT/PORTO PR. FAKTURA OG PRINTE    *
      *  DETTE TIL SLUTT PÅ FAKTURAEN.                                *
      * 1. FIRMA 960, KUNDE 108801 OG 108801 (ESSO NORGE)             *
      *****************************************************************
           END-IF
           .
 
       FAKTNR-FLDSET SECTION.
       FAKTNR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKTNR-IO-AREA (3:3)   TO FIRMN4 (1:3)
               MOVE FAKTNR-IO-AREA (6:6)   TO PFANR1-IO
               INSPECT PFANR1-IO REPLACING ALL ' ' BY '0'
               MOVE FAKTNR-IO-AREA (12:6)  TO PKRNR1-IO
               INSPECT PKRNR1-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       FAKTNR-IDSET SECTION.
       FAKTNR-IDSET-P.
           SET I-01                        TO TRUE.
 
       FAKPAR-FLDSET SECTION.
       FAKPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKPAR-IO-AREA (18:6)  TO PFADTO (1:6)
               MOVE FAKPAR-IO-AREA (18:2)  TO PFDAG (1:2)
               MOVE FAKPAR-IO-AREA (20:2)  TO PFMND (1:2)
               MOVE FAKPAR-IO-AREA (22:2)  TO PFAAR (1:2)
               MOVE FAKPAR-IO-AREA (24:6)  TO PFF15 (1:6)
               MOVE FAKPAR-IO-AREA (30:6)  TO PFF30 (1:6)
               MOVE FAKPAR-IO-AREA (36:6)  TO PFF45 (1:6)
               MOVE FAKPAR-IO-AREA (42:6)  TO PFF60 (1:6)
               MOVE FAKPAR-IO-AREA (48:6)  TO PFF90 (1:6)
               MOVE FAKPAR-IO-AREA (54:6)  TO PFF75 (1:6)
               MOVE FAKPAR-IO-AREA (66:1)  TO FO-ELGRSTE (1:1)
               MOVE FAKPAR-IO-AREA (10:2)  TO FAKTOM (1:2)
               MOVE FAKPAR-IO-AREA (101:6) TO UFADTO (1:6)
               MOVE FAKPAR-IO-AREA (101:2) TO UFDAG (1:2)
               MOVE FAKPAR-IO-AREA (103:2) TO UFMND (1:2)
               MOVE FAKPAR-IO-AREA (105:2) TO UFAAR (1:2)
               MOVE FAKPAR-IO-AREA (107:6) TO UFF15 (1:6)
               MOVE FAKPAR-IO-AREA (113:6) TO UFF30 (1:6)
               MOVE FAKPAR-IO-AREA (119:6) TO UFF45 (1:6)
               MOVE FAKPAR-IO-AREA (125:6) TO UFF60 (1:6)
               MOVE FAKPAR-IO-AREA (131:6) TO UFF90 (1:6)
               MOVE FAKPAR-IO-AREA (137:6) TO UFF75 (1:6)
               MOVE FAKPAR-IO-AREA (143:6) TO UFF105 (1:6)
               MOVE FAKPAR-IO-AREA (149:6) TO UFF120 (1:6)
               MOVE FAKPAR-IO-AREA (155:6) TO UFF150 (1:6)
               MOVE FAKPAR-IO-AREA (161:6) TO UFF180 (1:6)
               MOVE FAKPAR-IO-AREA (167:6) TO PFF105 (1:6)
               MOVE FAKPAR-IO-AREA (173:6) TO PFF120 (1:6)
               MOVE FAKPAR-IO-AREA (179:6) TO PFF150 (1:6)
               MOVE FAKPAR-IO-AREA (185:6) TO PFF180 (1:6)
           END-EVALUATE.
 
       FAKPAR-IDSET SECTION.
       FAKPAR-IDSET-P.
           SET I-01                        TO TRUE.
 
       FAKTIN-GET SECTION.
       FAKTIN-GET-P.
           IF  FAKTIN-EOF-OFF
               READ FAKTIN
               AT END
                   SET FAKTIN-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FAKTIN-FLDSET SECTION.
       FAKTIN-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKTIN-IO-AREA (1:3)   TO FIRMNR (1:3)
               MOVE FAKTIN-IO-AREA (4:1)   TO KTOKL (1:1)
               MOVE FAKTIN-IO-AREA (4:6)   TO RESKNR (1:6)
               MOVE FAKTIN-IO-AREA (10:1)  TO FAKRNR (1:1)
               MOVE FAKTIN-IO-AREA (10:2)  TO FAKRFL (1:2)
               MOVE FAKTIN-IO-AREA (12:1)  TO FAKMTE (1:1)
               MOVE FAKTIN-IO-AREA (13:1)  TO AVGKOD (1:1)
               MOVE FAKTIN-IO-AREA (11:1)  TO FLSIDE (1:1)
               MOVE FAKTIN-IO-AREA (14:2)  TO BTMA-ELGTE (1:2)
               MOVE FAKTIN-IO-AREA (16:3)  TO BRKOD-IO
               MOVE FAKTIN-IO-AREA (19:6)  TO ONR (1:6)
               MOVE FAKTIN-IO-AREA (25:1)  TO RA (1:1)
               MOVE FAKTIN-IO-AREA (26:4)  TO LINSEQ (1:4)
               MOVE FAKTIN-IO-AREA (40:1)  TO GJFAKT (1:1)
               MOVE FAKTIN-IO-AREA (41:15) TO KUREF (1:15)
               MOVE FAKTIN-IO-AREA (41:30) TO VARAD1 (1:30)
               MOVE FAKTIN-IO-AREA (56:6)  TO FAKREF (1:6)
               MOVE FAKTIN-IO-AREA (62:2)  TO LAGERK (1:2)
               MOVE FAKTIN-IO-AREA (66:1)  TO BK (1:1)
               MOVE FAKTIN-IO-AREA (67:1)  TO FRITT (1:1)
               MOVE FAKTIN-IO-AREA (68:3)  TO HDIST (1:3)
               MOVE FAKTIN-IO-AREA (77:5)  TO VGR (1:5)
               MOVE FAKTIN-IO-AREA (77:1)  TO VGRAVD (1:1)
               MOVE FAKTIN-IO-AREA (77:30) TO VARAD2 (1:30)
               MOVE FAKTIN-IO-AREA (82:20) TO ARTNR (1:20)
               MOVE FAKTIN-IO-AREA (102:30) TO VARBET (1:30)
               MOVE FAKTIN-IO-AREA (102:20) TO VARB20 (1:20)
               MOVE FAKTIN-IO-AREA (137:4) TO LEVENH-IO
               MOVE FAKTIN-IO-AREA (141:7) TO EDBNR (1:7)
               MOVE FAKTIN-IO-AREA (141:3) TO EDB3F (1:3)
               MOVE FAKTIN-IO-AREA (141:2) TO EDB2F (1:2)
               MOVE FAKTIN-IO-AREA (148:3) TO RAB1-IO
               INSPECT RAB1-IO REPLACING ALL ' ' BY '0'
               MOVE FAKTIN-IO-AREA (151:3) TO RAB2-IO
               INSPECT RAB2-IO REPLACING ALL ' ' BY '0'
               MOVE FAKTIN-IO-AREA (154:3) TO RAB3-IO
               INSPECT RAB3-IO REPLACING ALL ' ' BY '0'
               MOVE FAKTIN-IO-AREA (157:9) TO ENHPR-IO
               INSPECT ENHPR-IO REPLACING ALL ' ' BY '0'
               MOVE FAKTIN-IO-AREA (166:1) TO REGAVD (1:1)
               MOVE FAKTIN-IO-AREA (171:5) TO KOSTPR-IO
               MOVE FAKTIN-IO-AREA (177:1) TO RENGEB (1:1)
               MOVE FAKTIN-IO-AREA (184:6) TO KUNDNR (1:6)
               MOVE FAKTIN-IO-AREA (190:3) TO ALFKOD (1:3)
           END-EVALUATE.
 
       FAKTIN-IDSET SECTION.
       FAKTIN-IDSET-P.
           SET I-02                        TO TRUE.
 
       FAKTIN-CHK-LEVEL SECTION.
       FAKTIN-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO FAKTIN-LEVEL-02
               MOVE FAKTIN-IO-AREA (1:3)   TO FAKTIN-02-L5-FIRMNR
               MOVE FAKTIN-IO-AREA (10:2)  TO FAKTIN-02-L4-FAKRFL
               MOVE FAKTIN-IO-AREA (4:6)   TO FAKTIN-02-L3-RESKNR
               MOVE FAKTIN-IO-AREA (12:1)  TO FAKTIN-02-L2-FAKMTE
               MOVE FAKTIN-IO-AREA (13:1)  TO FAKTIN-02-L2-AVGKOD
               MOVE FAKTIN-IO-AREA (14:2)  TO FAKTIN-02-L2-BTMA-ELGTE
               MOVE FAKTIN-IO-AREA (16:3)  TO FAKTIN-02-L2-BRKOD
               MOVE FAKTIN-IO-AREA (19:6)  TO FAKTIN-02-L1-ONR
               IF  FAKTIN-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  FAKTIN-02-L5 NOT = THE-PRIOR-L5
                       PERFORM SETON-I-L5
                   WHEN  FAKTIN-02-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   WHEN  FAKTIN-02-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  FAKTIN-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  FAKTIN-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  FAKTIN-02-L5          TO THE-PRIOR-L5
               MOVE  FAKTIN-02-L4          TO THE-PRIOR-L4
               MOVE  FAKTIN-02-L3          TO THE-PRIOR-L3
               MOVE  FAKTIN-02-L2          TO THE-PRIOR-L2
               MOVE  FAKTIN-02-L1          TO THE-PRIOR-L1
               SET FAKTIN-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (16:30) TO CNAVN1 (1:30)
               MOVE KUNDEMA-IO-AREA (46:30) TO CNAVN2 (1:30)
               MOVE KUNDEMA-IO-AREA (76:30) TO CADR (1:30)
               MOVE KUNDEMA-IO-AREA (106:15) TO CPOST (1:15)
               MOVE KUNDEMA-IO-AREA (121:4) TO CPNR (1:4)
               MOVE KUNDEMA-IO-AREA (121:3) TO CPNR3 (1:3)
               MOVE KUNDEMA-IO-AREA (125:2) TO RESGRP (1:2)
               MOVE KUNDEMA-IO-AREA (127:2) TO CBETM (1:2)
               MOVE KUNDEMA-IO-AREA (150:6) TO KJOPT-IO
               MOVE KUNDEMA-IO-AREA (162:2) TO KUNKAT-IO
               MOVE KUNDEMA-IO-AREA (165:1) TO RENTE (1:1)
               MOVE KUNDEMA-IO-AREA (169:1) TO BELFAK (1:1)
               MOVE KUNDEMA-IO-AREA (165:1) TO RENTE (1:1)
               MOVE KUNDEMA-IO-AREA (185:1) TO HDIST1 (1:1)
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-01                        TO TRUE.
 
       KUNDEMX-FLDSET SECTION.
       KUNDEMX-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMX-IO-AREA (132:3) TO KKAT (1:3)
               MOVE KUNDEMX-IO-AREA (167:13) TO EANLOC (1:13)
           END-EVALUATE.
 
       KUNDEMX-IDSET SECTION.
       KUNDEMX-IDSET-P.
           SET I-01                        TO TRUE.
 
       FIRMAF-FLDOFF SECTION.
       FIRMAF-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-77                TO TRUE
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (1:3)   TO KONFNR (1:3)
               MOVE FIRMAF-IO-AREA (136:3) TO RNTPRO-IO
               INSPECT RNTPRO-IO REPLACING ALL ' ' BY '0'
               MOVE FIRMAF-IO-AREA (154:3) TO RNTPRA-IO
               INSPECT RNTPRA-IO REPLACING ALL ' ' BY '0'
               MOVE FIRMAF-IO-AREA (779:1) TO FFKODE (1:1)
               MOVE FIRMAF-IO-AREA (703:1) TO PRIFNR (1:1)
               MOVE FIRMAF-IO-AREA (775:1) TO KRFFD (1:1)
               MOVE FIRMAF-IO-AREA (837:4) TO KTOAVR (1:4)
               IF  KTOAVR = SPACES
                   SET I-77                TO TRUE
               END-IF
               MOVE FIRMAF-IO-AREA (896:5) TO FTAKNR-IO
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-01                        TO TRUE.
 
       TEKSTF-FLDSET SECTION.
       TEKSTF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE TEKSTF-IO-AREA (11:100) TO FTEKS1 (1:100)
               MOVE TEKSTF-IO-AREA (111:100) TO FTEKS2 (1:100)
           END-EVALUATE.
 
       TEKSTF-IDSET SECTION.
       TEKSTF-IDSET-P.
           SET I-01                        TO TRUE.
 
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
 
       AVDTAB-LOAD SECTION.
       AVDTAB-LOAD-P.
           OPEN INPUT AVDTAB
           SET TABAVD-I                    TO 1
           PERFORM UNTIL AVDTAB-EOF
               READ AVDTAB
               AT END
                   SET AVDTAB-EOF          TO TRUE
               NOT AT END
                   MOVE AVDTAB-IO-AREA (1:46) TO TABAVD-ENTRY
                                                            (TABAVD-I)
                   SET TABAVD-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE AVDTAB.
 
       FAKSGR-LOAD SECTION.
       FAKSGR-LOAD-P.
           OPEN INPUT FAKSGR
           SET TABFVG-I                    TO 1
           PERFORM UNTIL FAKSGR-EOF
               READ FAKSGR
               AT END
                   SET FAKSGR-EOF          TO TRUE
               NOT AT END
                   MOVE FAKSGR-IO-AREA (1:9) TO TABFVG-ENTRY (TABFVG-I)
                   SET TABFVG-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE FAKSGR.
 
       FAKESS-LOAD SECTION.
       FAKESS-LOAD-P.
           OPEN INPUT FAKESS
           SET TABFVE-I                    TO 1
           PERFORM UNTIL FAKESS-EOF
               READ FAKESS
               AT END
                   SET FAKESS-EOF          TO TRUE
               NOT AT END
                   MOVE FAKESS-IO-AREA (1:6) TO TABFVE-ENTRY (TABFVE-I)
                   SET TABFVE-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE FAKESS.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-72)
           OR  (I-64)
               MOVE SPACES TO PRTFILE-IO-AREA
               INITIALIZE PRTFILE-IO-AREA
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (1:3)
               MOVE SEQ-IO                 TO PRTFILE-IO-AREA (4:6)
               MOVE '01'                   TO PRTFILE-IO-AREA (10:2)
               IF  (NOT-I-90)
                   MOVE CNAVN1             TO PRTFILE-IO-AREA (24:30)
               END-IF
               IF  (I-90)
                   MOVE '*** KUNDE IKKE I ARKIV *' TO PRTFILE-IO-AREA
                                                               (28:24)
               END-IF
               IF  (I-90)
                   MOVE '**'               TO PRTFILE-IO-AREA (52:2)
               END-IF
               IF  (I-72)
                   MOVE VARAD1             TO PRTFILE-IO-AREA (64:30)
               END-IF
               MOVE BTMA-ELGTE             TO PRTFILE-IO-AREA (126:2)
               MOVE PRFLT5                 TO PRTFILE-IO-AREA (128:9)
      *                                 137 "B"
               MOVE RESKNR                 TO PRTFILE-IO-AREA (138:6)
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (144:3)
               WRITE PRTFILE-IO-AREA
           END-IF
           IF  (I-76)
           OR  (I-66)
               MOVE SPACES TO PRTFILE-IO-AREA
               INITIALIZE PRTFILE-IO-AREA
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (1:3)
               MOVE SEQ-IO                 TO PRTFILE-IO-AREA (4:6)
               MOVE '02'                   TO PRTFILE-IO-AREA (10:2)
               IF  (NOT-I-90)
                   MOVE CNAVN2             TO PRTFILE-IO-AREA (24:30)
               END-IF
               IF  (I-76)
                   MOVE VARAD2             TO PRTFILE-IO-AREA (64:30)
               END-IF
               IF  (I-71)
                   MOVE PRARE1-IO          TO PRTFILE-IO-AREA (113:6)
               END-IF
               IF  (NOT-I-71)
                   MOVE PRARE1-IO          TO PRTFILE-IO-AREA (116:6)
               END-IF
               MOVE PRFLT5                 TO PRTFILE-IO-AREA (128:9)
               MOVE RESKNR                 TO PRTFILE-IO-AREA (138:6)
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (144:3)
               WRITE PRTFILE-IO-AREA
           END-IF
           IF  (I-60)
           OR  (I-68)
               MOVE SPACES TO PRTFILE-IO-AREA
               INITIALIZE PRTFILE-IO-AREA
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (1:3)
               MOVE SEQ-IO                 TO PRTFILE-IO-AREA (4:6)
               MOVE '03'                   TO PRTFILE-IO-AREA (10:2)
               IF  (NOT-I-90)
                   MOVE CADR               TO PRTFILE-IO-AREA (24:30)
               END-IF
               IF  (I-60)
                   MOVE VARAD1             TO PRTFILE-IO-AREA (64:30)
               END-IF
               IF  (I-25)
                   MOVE 'FORETAKSREG.'     TO PRTFILE-IO-AREA (97:12)
               END-IF
               IF  (I-25)
                   MOVE 'NO'               TO PRTFILE-IO-AREA (110:2)
               END-IF
               IF  (I-25)
                   MOVE FTAKNR             TO XO-90U
                   MOVE XO-90U (1:9)       TO PRTFILE-IO-AREA (113:9)
               END-IF
               IF  (I-25)
                   MOVE 'MVA'              TO PRTFILE-IO-AREA (123:3)
               END-IF
               MOVE PRFLT5                 TO PRTFILE-IO-AREA (128:9)
               MOVE RESKNR                 TO PRTFILE-IO-AREA (138:6)
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (144:3)
               WRITE PRTFILE-IO-AREA
           END-IF
           IF  (I-62)
           OR  (I-70)
               MOVE SPACES TO PRTFILE-IO-AREA
               INITIALIZE PRTFILE-IO-AREA
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (1:3)
               MOVE SEQ-IO                 TO PRTFILE-IO-AREA (4:6)
               MOVE '04'                   TO PRTFILE-IO-AREA (10:2)
               IF  (NOT-I-90)
                   MOVE CPNR               TO PRTFILE-IO-AREA (24:4)
               END-IF
               IF  (NOT-I-90)
                   MOVE CPOST              TO PRTFILE-IO-AREA (29:15)
               END-IF
               IF  (I-62)
                   MOVE VARAD2             TO PRTFILE-IO-AREA (64:30)
               END-IF
               IF  (I-49)
                   MOVE 'EAN-LOKASJONSNR.' TO PRTFILE-IO-AREA (97:16)
               END-IF
               IF  (I-49)
                   MOVE EANLOC             TO PRTFILE-IO-AREA (113:13)
               END-IF
               MOVE PRFLT5                 TO PRTFILE-IO-AREA (128:9)
               MOVE RESKNR                 TO PRTFILE-IO-AREA (138:6)
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (144:3)
               WRITE PRTFILE-IO-AREA
           END-IF
           IF  (I-78)
               MOVE SPACES TO PRTFILE-IO-AREA
               INITIALIZE PRTFILE-IO-AREA
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (1:3)
               MOVE SEQ-IO                 TO PRTFILE-IO-AREA (4:6)
               MOVE '05'                   TO PRTFILE-IO-AREA (10:2)
               MOVE RESKNR                 TO PRTFILE-IO-AREA (15:6)
               MOVE ONR                    TO PRTFILE-IO-AREA (23:6)
               MOVE GJFAKT                 TO PRTFILE-IO-AREA (29:1)
               MOVE KUREF                  TO PRTFILE-IO-AREA (31:15)
               MOVE FAKREF                 TO PRTFILE-IO-AREA (62:6)
               IF  (NOT-I-91)
                   MOVE BMTKST             TO PRTFILE-IO-AREA (70:24)
               END-IF
               MOVE 'KID.'                 TO PRTFILE-IO-AREA (95:4)
               MOVE FAKKID                 TO PRTFILE-IO-AREA (99:7)
               MOVE FFDAG                  TO PRTFILE-IO-AREA (109:2)
               MOVE FFDAG                  TO PRTFILE-IO-AREA (109:2)
               IF  (NOT-I-67 AND NOT-I-14 AND I-96)
                   MOVE PFDAG              TO PRTFILE-IO-AREA (109:2)
               END-IF
               MOVE '.'                    TO PRTFILE-IO-AREA (111:1)
               MOVE FFMND                  TO PRTFILE-IO-AREA (112:2)
               IF  (NOT-I-67 AND NOT-I-14 AND I-96)
                   MOVE PFMND              TO PRTFILE-IO-AREA (112:2)
               END-IF
               MOVE '.'                    TO PRTFILE-IO-AREA (114:1)
               MOVE FFAAR                  TO PRTFILE-IO-AREA (115:2)
               IF  (NOT-I-67 AND NOT-I-14 AND I-96)
                   MOVE PFAAR              TO PRTFILE-IO-AREA (115:2)
               END-IF
               MOVE PFDAG                  TO PRTFILE-IO-AREA (119:2)
               IF  (I-95)
                   MOVE UFDAG              TO PRTFILE-IO-AREA (119:2)
               END-IF
               MOVE '.'                    TO PRTFILE-IO-AREA (121:1)
               MOVE PFMND                  TO PRTFILE-IO-AREA (122:2)
               IF  (I-95)
                   MOVE UFMND              TO PRTFILE-IO-AREA (122:2)
               END-IF
               MOVE '.'                    TO PRTFILE-IO-AREA (124:1)
               MOVE PFAAR                  TO PRTFILE-IO-AREA (125:2)
               IF  (I-95)
                   MOVE UFAAR              TO PRTFILE-IO-AREA (125:2)
               END-IF
               MOVE BRKODE-IO              TO PRTFILE-IO-AREA (128:6)
               MOVE FAKRNR                 TO PRTFILE-IO-AREA (134:1)
               MOVE FLSIDE                 TO PRTFILE-IO-AREA (135:1)
               MOVE '2'                    TO PRTFILE-IO-AREA (136:1)
               MOVE RESKNR                 TO PRTFILE-IO-AREA (138:6)
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (144:3)
               WRITE PRTFILE-IO-AREA
           END-IF
           IF  (I-80)
               MOVE SPACES TO PRTFILE-IO-AREA
               INITIALIZE PRTFILE-IO-AREA
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (1:3)
               MOVE SEQ-IO                 TO PRTFILE-IO-AREA (4:6)
               MOVE '06'                   TO PRTFILE-IO-AREA (10:2)
               MOVE '* * * *'              TO PRTFILE-IO-AREA (15:7)
               MOVE '*** ORDRENR.'         TO PRTFILE-IO-AREA (24:12)
               MOVE ONR                    TO PRTFILE-IO-AREA (37:6)
               MOVE GJFAKT                 TO PRTFILE-IO-AREA (43:1)
               IF  (NOT-I-20)
                   MOVE 'DERES REF.'       TO PRTFILE-IO-AREA (45:10)
               END-IF
               IF  (NOT-I-20)
                   MOVE KUREF              TO PRTFILE-IO-AREA (56:15)
               END-IF
               IF  (I-20)
                   MOVE 'RENTENOTA OG PURREGEBYR ' TO PRTFILE-IO-AREA
                                                               (47:24)
               END-IF
               MOVE BRKODE-IO              TO PRTFILE-IO-AREA (128:6)
               MOVE FAKRNR                 TO PRTFILE-IO-AREA (134:1)
               MOVE FLSIDE                 TO PRTFILE-IO-AREA (135:1)
               MOVE '4'                    TO PRTFILE-IO-AREA (136:1)
               MOVE RESKNR                 TO PRTFILE-IO-AREA (138:6)
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (144:3)
               WRITE PRTFILE-IO-AREA
           END-IF
           IF  (I-82)
               MOVE SPACES TO PRTFILE-IO-AREA
               INITIALIZE PRTFILE-IO-AREA
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (1:3)
               MOVE SEQ-IO                 TO PRTFILE-IO-AREA (4:6)
               MOVE '07'                   TO PRTFILE-IO-AREA (10:2)
               MOVE ARTNR                  TO PRTFILE-IO-AREA (25:20)
               MOVE VARBET                 TO PRTFILE-IO-AREA (45:30)
               MOVE BRKODE-IO              TO PRTFILE-IO-AREA (128:6)
               MOVE FAKRNR                 TO PRTFILE-IO-AREA (134:1)
               MOVE FLSIDE                 TO PRTFILE-IO-AREA (135:1)
               MOVE '4'                    TO PRTFILE-IO-AREA (136:1)
               MOVE RESKNR                 TO PRTFILE-IO-AREA (138:6)
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (144:3)
               WRITE PRTFILE-IO-AREA
           END-IF
           IF  (I-39)
               MOVE SPACES TO PRTFILE-IO-AREA
               INITIALIZE PRTFILE-IO-AREA
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (1:3)
               MOVE SEQ-IO                 TO PRTFILE-IO-AREA (4:6)
               MOVE '08'                   TO PRTFILE-IO-AREA (10:2)
               MOVE LEVENH                 TO XO-52YNZ
               MOVE XO-52YNZ               TO PRTFILE-IO-AREA (14:8)
               MOVE ARTNR                  TO PRTFILE-IO-AREA (24:20)
               MOVE VARBET                 TO PRTFILE-IO-AREA (45:30)
               MOVE ENHPR                  TO EDIT-ENHPR
               MOVE EDIT-ENHPR             TO PRTFILE-IO-AREA (76:10)
               MOVE BTOPRI                 TO EDIT-BTOPRI
               MOVE EDIT-BTOPRI            TO PRTFILE-IO-AREA (88:10)
               MOVE RAB1                   TO EDIT-RAB1
               MOVE EDIT-RAB1              TO PRTFILE-IO-AREA (100:4)
               MOVE RAB2                   TO EDIT-RAB2
               MOVE EDIT-RAB2              TO PRTFILE-IO-AREA (105:4)
               MOVE RAB3                   TO EDIT-RAB3
               MOVE EDIT-RAB3              TO PRTFILE-IO-AREA (110:4)
               IF  (I-58)
                   MOVE 'NETTO         '   TO PRTFILE-IO-AREA (100:14)
               END-IF
               MOVE NETTO                  TO EDIT-NETTO
               MOVE EDIT-NETTO             TO PRTFILE-IO-AREA (116:12)
               MOVE BRKODE-IO              TO PRTFILE-IO-AREA (128:6)
               MOVE FAKRNR                 TO PRTFILE-IO-AREA (134:1)
               MOVE FLSIDE                 TO PRTFILE-IO-AREA (135:1)
               MOVE '4'                    TO PRTFILE-IO-AREA (136:1)
               MOVE RESKNR                 TO PRTFILE-IO-AREA (138:6)
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (144:3)
               WRITE PRTFILE-IO-AREA
           END-IF
           IF  (I-39)
               MOVE SPACES TO EDIFAKF-IO-AREA
               INITIALIZE EDIFAKF-IO-AREA
               MOVE FIRMNR                 TO EDIFAKF-IO-AREA (1:3)
               MOVE RESKNR                 TO EDIFAKF-IO-AREA (4:6)
               MOVE PRARE1-IO              TO EDIFAKF-IO-AREA (10:6)
               MOVE '02'                   TO EDIFAKF-IO-AREA (16:2)
               IF  (NOT-I-14)
                   MOVE '21'               TO EDIFAKF-IO-AREA (16:2)
               END-IF
               IF  (I-14)
                   MOVE '26'               TO EDIFAKF-IO-AREA (16:2)
               END-IF
               IF  (I-20)
                   MOVE '22'               TO EDIFAKF-IO-AREA (16:2)
               END-IF
               MOVE 'V'                    TO EDIFAKF-IO-AREA (21:1)
               MOVE ONR                    TO EDIFAKF-IO-AREA (22:6)
               MOVE ARTNR                  TO EDIFAKF-IO-AREA (28:20)
               IF  (I-06)
                   MOVE VARB20             TO EDIFAKF-IO-AREA (28:20)
               END-IF
               MOVE LEVENH                 TO XO-52U
               MOVE XO-52U (1:7)           TO EDIFAKF-IO-AREA (51:7)
               MOVE NETTO-IO               TO EDIFAKF-IO-AREA (59:9)
               MOVE RAB1-IO                TO EDIFAKF-IO-AREA (68:3)
               MOVE RAB2-IO                TO EDIFAKF-IO-AREA (71:3)
               MOVE RAB3-IO                TO EDIFAKF-IO-AREA (74:3)
               MOVE ALFKOD                 TO EDIFAKF-IO-AREA (77:3)
               MOVE EDBNR                  TO EDIFAKF-IO-AREA (80:7)
               MOVE VGR                    TO EDIFAKF-IO-AREA (87:5)
               MOVE VARBET                 TO EDIFAKF-IO-AREA (101:30)
               MOVE KUREF                  TO EDIFAKF-IO-AREA (131:15)
               MOVE LINSEQ                 TO EDIFAKF-IO-AREA (146:4)
               WRITE EDIFAKF-IO-AREA
           END-IF
           IF  (I-41)
               MOVE SPACES TO REGFILE-IO-AREA
               INITIALIZE REGFILE-IO-AREA
               MOVE '01'                   TO REGFILE-IO-AREA (1:2)
               MOVE PRARE1-IO              TO REGFILE-IO-AREA (3:6)
               IF  (NOT-I-14)
                   MOVE '2'                TO REGFILE-IO-AREA (9:1)
               END-IF
               IF  (I-14)
                   MOVE '3'                TO REGFILE-IO-AREA (9:1)
               END-IF
               MOVE PFDAG                  TO REGFILE-IO-AREA (14:2)
               IF  (I-95)
                   MOVE UFDAG              TO REGFILE-IO-AREA (14:2)
               END-IF
               MOVE PFMND                  TO REGFILE-IO-AREA (12:2)
               IF  (I-95)
                   MOVE UFMND              TO REGFILE-IO-AREA (12:2)
               END-IF
               MOVE PFAAR                  TO REGFILE-IO-AREA (10:2)
               IF  (I-95)
                   MOVE UFAAR              TO REGFILE-IO-AREA (10:2)
               END-IF
               MOVE KUNDNR                 TO REGFILE-IO-AREA (16:6)
               MOVE FIRMNR                 TO REGFILE-IO-AREA (22:3)
               IF  (NOT-I-08)
                   MOVE '6'                TO REGFILE-IO-AREA (30:1)
               END-IF
               IF  (I-08 AND NOT-I-20 AND NOT-I-18)
                   MOVE '4'                TO REGFILE-IO-AREA (30:1)
               END-IF
               IF  (I-08 AND NOT-I-20 AND I-18)
                   MOVE 'E'                TO REGFILE-IO-AREA (30:1)
               END-IF
               IF  (I-08 AND I-20)
                   MOVE '0'                TO REGFILE-IO-AREA (30:1)
               END-IF
               MOVE '5'                    TO REGFILE-IO-AREA (31:1)
               MOVE VGR                    TO REGFILE-IO-AREA (32:5)
               MOVE BXBEL-IO               TO REGFILE-IO-AREA (37:9)
               MOVE SIGNX1                 TO REGFILE-IO-AREA (46:1)
               MOVE SUMSP-IO               TO REGFILE-IO-AREA (47:9)
               INITIALIZE SUMSP-IO
               MOVE FFDAG                  TO REGFILE-IO-AREA (63:2)
               MOVE FFMND                  TO REGFILE-IO-AREA (65:2)
               MOVE FFAAR                  TO REGFILE-IO-AREA (67:2)
               MOVE HDIST                  TO REGFILE-IO-AREA (74:3)
               MOVE 'F'                    TO REGFILE-IO-AREA (77:1)
               MOVE KTOKL                  TO REGFILE-IO-AREA (78:1)
               MOVE BK                     TO REGFILE-IO-AREA (79:1)
               MOVE REGAVD                 TO REGFILE-IO-AREA (80:1)
               WRITE REGFILE-IO-AREA
           END-IF
           IF  (I-H1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMNRNR.'            TO LISTE-IO-AREA (10:9)
               MOVE FIRMNR                 TO LISTE-IO-AREA (20:3)
               MOVE 'ER IKKE I FAKT.NR.FILE  ' TO LISTE-IO-AREA (27:24)
               MOVE 'PROGRAMMET CANCELERES.  ' TO LISTE-IO-AREA (53:24)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       EXCEPTION-OUTPUT SECTION.
       EXCEPTION-OUTPUT-P.
           IF  (I-42 AND I-83)
               MOVE SPACES TO PRTFILE-IO-AREA
               INITIALIZE PRTFILE-IO-AREA
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (1:3)
               MOVE SEQ-IO                 TO PRTFILE-IO-AREA (4:6)
               MOVE SEQ2-IO                TO PRTFILE-IO-AREA (10:2)
               MOVE 'TOTAL '               TO PRTFILE-IO-AREA (25:6)
               MOVE TABAVN (TABAVN-I)      TO PRTFILE-IO-AREA (31:42)
               MOVE STAVD                  TO EDIT-STAVD
               MOVE EDIT-STAVD             TO PRTFILE-IO-AREA (76:10)
               MOVE BRKODE-IO              TO PRTFILE-IO-AREA (128:6)
               MOVE FAKRNR                 TO PRTFILE-IO-AREA (134:1)
               MOVE FLSIDE                 TO PRTFILE-IO-AREA (135:1)
               MOVE '4'                    TO PRTFILE-IO-AREA (136:1)
               MOVE RESKNR                 TO PRTFILE-IO-AREA (138:6)
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (144:3)
               WRITE PRTFILE-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L5 AND I-69)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FAKTURATOTALER PR.'   TO LISTE-IO-AREA (2:18)
               MOVE NFADTO                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (20:8)
               MOVE 'FREMSTILT'            TO LISTE-IO-AREA (30:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (40:8)
               MOVE 'FAKTURAOMGANG'        TO LISTE-IO-AREA (50:13)
               MOVE FAKTOM                 TO LISTE-IO-AREA (64:2)
               MOVE 'PROG=FAK085'          TO LISTE-IO-AREA (70:11)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMNRNR'             TO LISTE-IO-AREA (1:8)
               MOVE 'FAKT.NR TIL FAKT.NR'  TO LISTE-IO-AREA (12:19)
               MOVE 'KR.NOTA TIL KR.NOTA'  TO LISTE-IO-AREA (34:19)
               MOVE 'TOT ANT'              TO LISTE-IO-AREA (56:7)
               MOVE 'KTO.'                 TO LISTE-IO-AREA (67:4)
               MOVE 'FIRMASUM'             TO LISTE-IO-AREA (75:8)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'AVR.'                 TO LISTE-IO-AREA (67:4)
               MOVE 'ØREAVR.'              TO LISTE-IO-AREA (76:7)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FAKTURATOTALER PR.'   TO LISTE-IO-AREA (2:18)
               MOVE NFADTO                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (20:8)
               MOVE 'FREMSTILT'            TO LISTE-IO-AREA (30:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (40:8)
               MOVE 'FAKTURAOMGANG'        TO LISTE-IO-AREA (50:13)
               MOVE FAKTOM                 TO LISTE-IO-AREA (64:2)
               MOVE 'PROG=FAK085'          TO LISTE-IO-AREA (70:11)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMNRNR'             TO LISTE-IO-AREA (1:8)
               MOVE 'FAKT.NR TIL FAKT.NR'  TO LISTE-IO-AREA (12:19)
               MOVE 'KR.NOTA TIL KR.NOTA'  TO LISTE-IO-AREA (34:19)
               MOVE 'TOT ANT'              TO LISTE-IO-AREA (56:7)
               MOVE 'KTO.'                 TO LISTE-IO-AREA (67:4)
               MOVE 'FIRMASUM'             TO LISTE-IO-AREA (75:8)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'AVR.'                 TO LISTE-IO-AREA (67:4)
               MOVE 'ØREAVR.'              TO LISTE-IO-AREA (76:7)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1)
               MOVE SPACES TO ORDFAK-IO-AREA
               INITIALIZE ORDFAK-IO-AREA
               MOVE '4'                    TO ORDFAK-IO-AREA (1:1)
               MOVE FIRMNR                 TO ORDFAK-IO-AREA (2:3)
               MOVE ONR                    TO ORDFAK-IO-AREA (5:6)
               MOVE AREA8X-IO              TO ORDFAK-IO-AREA (11:6)
               MOVE GJFAKT                 TO ORDFAK-IO-AREA (20:1)
               WRITE ORDFAK-IO-AREA
           END-IF
           IF  (I-42)
               MOVE SPACES TO PRTFILE-IO-AREA
               INITIALIZE PRTFILE-IO-AREA
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (1:3)
               MOVE SEQ-IO                 TO PRTFILE-IO-AREA (4:6)
               MOVE '31'                   TO PRTFILE-IO-AREA (10:2)
               MOVE '------------'         TO PRTFILE-IO-AREA (116:12)
               MOVE PRFLT5                 TO PRTFILE-IO-AREA (128:9)
               MOVE RESKNR                 TO PRTFILE-IO-AREA (138:6)
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (144:3)
               WRITE PRTFILE-IO-AREA
               MOVE SPACES TO PRTFILE-IO-AREA
               INITIALIZE PRTFILE-IO-AREA
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (1:3)
               MOVE SEQ-IO                 TO PRTFILE-IO-AREA (4:6)
               MOVE '32'                   TO PRTFILE-IO-AREA (10:2)
               IF  (NOT-I-67 AND NOT-I-14 AND I-93)
                   MOVE 'TIL INFORMASJON.  ' TO PRTFILE-IO-AREA (24:18)
               END-IF
               IF  (NOT-I-67 AND NOT-I-14 AND I-93)
                   MOVE 'DENNE FAKTURA ER BETALT ' TO PRTFILE-IO-AREA
                                                               (42:24)
               END-IF
               IF  (NOT-I-67 AND NOT-I-14 AND I-93)
                   MOVE 'KONTANT.                ' TO PRTFILE-IO-AREA
                                                               (66:24)
               END-IF
               IF  (NOT-I-67 AND NOT-I-14 AND I-96)
                   MOVE 'TIL INFORMASJON.  ' TO PRTFILE-IO-AREA (24:18)
               END-IF
               IF  (NOT-I-67 AND NOT-I-14 AND I-96)
                   MOVE 'DETTE ER EN OPPKRAVSFAKT' TO PRTFILE-IO-AREA
                                                               (42:24)
               END-IF
               IF  (NOT-I-67 AND NOT-I-14 AND I-96)
                   MOVE 'URA.                    ' TO PRTFILE-IO-AREA
                                                               (66:24)
               END-IF
               IF  (I-67)
                   MOVE 'HERAV FRAKT/PORTO KR.' TO PRTFILE-IO-AREA
                                                               (23:21)
               END-IF
               IF  (I-67)
                   MOVE NETFRA             TO EDIT-NETFRA
                   MOVE EDIT-NETFRA        TO PRTFILE-IO-AREA (44:12)
               END-IF
               MOVE 'SUM EKSKL. MOMS'      TO PRTFILE-IO-AREA (99:15)
               MOVE NETBNR                 TO EDIT-NETBNR
               MOVE EDIT-NETBNR            TO PRTFILE-IO-AREA (116:12)
               MOVE PRFLT5                 TO PRTFILE-IO-AREA (128:9)
               MOVE RESKNR                 TO PRTFILE-IO-AREA (138:6)
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (144:3)
               WRITE PRTFILE-IO-AREA
               MOVE SPACES TO PRTFILE-IO-AREA
               INITIALIZE PRTFILE-IO-AREA
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (1:3)
               MOVE SEQ-IO                 TO PRTFILE-IO-AREA (4:6)
               MOVE '33'                   TO PRTFILE-IO-AREA (10:2)
               IF  (I-45)
                   MOVE 'ØREAVRUNDING'     TO PRTFILE-IO-AREA (55:12)
               END-IF
               IF  (I-45)
                   MOVE AVRORE             TO EDIT-AVRORE
                   MOVE EDIT-AVRORE        TO PRTFILE-IO-AREA (68:7)
      *                      45         112 "+ 25 PST. MOMS"
               END-IF
               IF  (I-45)
                   MOVE MOMS               TO EDIT-MOMS
                   MOVE EDIT-MOMS          TO PRTFILE-IO-AREA (116:12)
               END-IF
               MOVE PRFLT5                 TO PRTFILE-IO-AREA (128:9)
               MOVE RESKNR                 TO PRTFILE-IO-AREA (138:6)
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (144:3)
      *****************************************************************
      * POSTERING AV ØREAVRUNDING.                                    *
      *  OM KONTO FOR ØREAVRUNDING IKKE ER OPPRETTET BLIR 2201 BRUKT. *
      *****************************************************************
               WRITE PRTFILE-IO-AREA
           END-IF
           IF  (I-45 AND I-35)
               MOVE SPACES TO REGFILE-IO-AREA
               INITIALIZE REGFILE-IO-AREA
               MOVE '01'                   TO REGFILE-IO-AREA (1:2)
               MOVE AREA9X-IO              TO REGFILE-IO-AREA (3:6)
               IF  (NOT-I-14)
                   MOVE '2'                TO REGFILE-IO-AREA (9:1)
               END-IF
               IF  (I-14)
                   MOVE '3'                TO REGFILE-IO-AREA (9:1)
               END-IF
               MOVE PFDAG                  TO REGFILE-IO-AREA (14:2)
               IF  (I-95)
                   MOVE UFDAG              TO REGFILE-IO-AREA (14:2)
               END-IF
               MOVE PFMND                  TO REGFILE-IO-AREA (12:2)
               IF  (I-95)
                   MOVE UFMND              TO REGFILE-IO-AREA (12:2)
               END-IF
               MOVE PFAAR                  TO REGFILE-IO-AREA (10:2)
               IF  (I-95)
                   MOVE UFAAR              TO REGFILE-IO-AREA (10:2)
               END-IF
               MOVE RESKNR                 TO REGFILE-IO-AREA (16:6)
               MOVE FIRMNR                 TO REGFILE-IO-AREA (22:3)
               MOVE ' '                    TO REGFILE-IO-AREA (30:1)
               MOVE ' '                    TO REGFILE-IO-AREA (31:1)
               IF  (NOT-I-77)
                   MOVE KTOAVR             TO REGFILE-IO-AREA (32:4)
               END-IF
               IF  (I-77)
                   MOVE '2201'             TO REGFILE-IO-AREA (32:4)
               END-IF
               MOVE ' '                    TO REGFILE-IO-AREA (36:1)
               MOVE OREDF1-IO              TO REGFILE-IO-AREA (37:9)
               MOVE SIGNAV                 TO REGFILE-IO-AREA (46:1)
               MOVE FFDAG                  TO REGFILE-IO-AREA (63:2)
               MOVE FFMND                  TO REGFILE-IO-AREA (65:2)
               MOVE FFAAR                  TO REGFILE-IO-AREA (67:2)
               MOVE HDIST                  TO REGFILE-IO-AREA (74:3)
               MOVE 'F'                    TO REGFILE-IO-AREA (77:1)
               MOVE KTOKL                  TO REGFILE-IO-AREA (78:1)
               MOVE BK                     TO REGFILE-IO-AREA (79:1)
               MOVE REGAVD                 TO REGFILE-IO-AREA (80:1)
      *****************************************************************
      * POSTERING AV UTGÅENDE MOMS.                                   *
      *****************************************************************
               WRITE REGFILE-IO-AREA
           END-IF
           IF  (I-45)
               MOVE SPACES TO REGFILE-IO-AREA
               INITIALIZE REGFILE-IO-AREA
               MOVE '01'                   TO REGFILE-IO-AREA (1:2)
               MOVE AREA9X-IO              TO REGFILE-IO-AREA (3:6)
               IF  (NOT-I-14)
                   MOVE '2'                TO REGFILE-IO-AREA (9:1)
               END-IF
               IF  (I-14)
                   MOVE '3'                TO REGFILE-IO-AREA (9:1)
               END-IF
               MOVE PFDAG                  TO REGFILE-IO-AREA (14:2)
               IF  (I-95)
                   MOVE UFDAG              TO REGFILE-IO-AREA (14:2)
               END-IF
               MOVE PFMND                  TO REGFILE-IO-AREA (12:2)
               IF  (I-95)
                   MOVE UFMND              TO REGFILE-IO-AREA (12:2)
               END-IF
               MOVE PFAAR                  TO REGFILE-IO-AREA (10:2)
               IF  (I-95)
                   MOVE UFAAR              TO REGFILE-IO-AREA (10:2)
               END-IF
               MOVE RESKNR                 TO REGFILE-IO-AREA (16:6)
               MOVE FIRMNR                 TO REGFILE-IO-AREA (22:3)
               MOVE '  '                   TO REGFILE-IO-AREA (30:2)
               MOVE '2201 '                TO REGFILE-IO-AREA (32:5)
               MOVE MOMSX1-IO              TO REGFILE-IO-AREA (37:9)
               MOVE SIGNXM                 TO REGFILE-IO-AREA (46:1)
               MOVE FFDAG                  TO REGFILE-IO-AREA (63:2)
               MOVE FFMND                  TO REGFILE-IO-AREA (65:2)
               MOVE FFAAR                  TO REGFILE-IO-AREA (67:2)
               MOVE HDIST                  TO REGFILE-IO-AREA (74:3)
               MOVE 'F'                    TO REGFILE-IO-AREA (77:1)
               MOVE KTOKL                  TO REGFILE-IO-AREA (78:1)
               MOVE BK                     TO REGFILE-IO-AREA (79:1)
               MOVE REGAVD                 TO REGFILE-IO-AREA (80:1)
               WRITE REGFILE-IO-AREA
           END-IF
           IF  (I-42)
               MOVE SPACES TO PRTFILE-IO-AREA
               INITIALIZE PRTFILE-IO-AREA
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (1:3)
               MOVE SEQ-IO                 TO PRTFILE-IO-AREA (4:6)
               MOVE '34'                   TO PRTFILE-IO-AREA (10:2)
               IF  (I-47)
                   MOVE '------------'     TO PRTFILE-IO-AREA (116:12)
               END-IF
               IF  (NOT-I-47)
                   MOVE '************'     TO PRTFILE-IO-AREA (116:12)
               END-IF
               MOVE PRFLT5                 TO PRTFILE-IO-AREA (128:9)
               MOVE RESKNR                 TO PRTFILE-IO-AREA (138:6)
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (144:3)
               WRITE PRTFILE-IO-AREA
               MOVE SPACES TO PRTFILE-IO-AREA
               INITIALIZE PRTFILE-IO-AREA
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (1:3)
               MOVE SEQ-IO                 TO PRTFILE-IO-AREA (4:6)
               MOVE '35'                   TO PRTFILE-IO-AREA (10:2)
               IF  (I-19)
                   MOVE 'VED BETALING ETTER' TO PRTFILE-IO-AREA (24:18)
               END-IF
               IF  (I-19)
                   MOVE 'FORFALL BEREGNES' TO PRTFILE-IO-AREA (43:16)
               END-IF
               IF  (I-19 AND NOT-I-55 AND NOT-I-48)
                   MOVE RNTPRO             TO XO-12YN9
                   MOVE XO-12YN9           TO PRTFILE-IO-AREA (60:4)
               END-IF
               IF  (I-19 AND NOT-I-55 AND I-48)
                   MOVE RNTPRA             TO XO-12YN9
                   MOVE XO-12YN9           TO PRTFILE-IO-AREA (60:4)
               END-IF
               IF  (I-19)
                   MOVE 'PROS. FORSINKEL'  TO PRTFILE-IO-AREA (65:15)
               END-IF
               IF  (I-19)
                   MOVE 'SESRENTE PR MÅNED.' TO PRTFILE-IO-AREA (80:18)
               END-IF
               IF  (I-47)
                   MOVE 'T O T A L'        TO PRTFILE-IO-AREA (105:9)
               END-IF
               IF  (I-47)
                   MOVE TOTAL              TO EDIT-TOTAL
                   MOVE EDIT-TOTAL         TO PRTFILE-IO-AREA (115:13)
               END-IF
               MOVE PRFLT5                 TO PRTFILE-IO-AREA (128:9)
               MOVE RESKNR                 TO PRTFILE-IO-AREA (138:6)
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (144:3)
               WRITE PRTFILE-IO-AREA
               MOVE SPACES TO PRTFILE-IO-AREA
               INITIALIZE PRTFILE-IO-AREA
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (1:3)
               MOVE SEQ-IO                 TO PRTFILE-IO-AREA (4:6)
               MOVE '36'                   TO PRTFILE-IO-AREA (10:2)
               MOVE REGAVD                 TO PRTFILE-IO-AREA (13:1)
               IF  (I-99)
                   MOVE 'KJØP HITTIL I ÅR ' TO PRTFILE-IO-AREA (44:17)
               END-IF
               IF  (I-99)
                   MOVE 'EKSL.MVA. '       TO PRTFILE-IO-AREA (61:10)
               END-IF
               IF  (I-99)
                   MOVE KJBEL              TO XO-90YYZR
                   MOVE XO-90YYZR          TO PRTFILE-IO-AREA (72:12)
               END-IF
               IF  (I-99)
                   MOVE 'KRONER'           TO PRTFILE-IO-AREA (86:6)
               END-IF
               IF  (I-47)
                   MOVE '************'     TO PRTFILE-IO-AREA (116:12)
               END-IF
               MOVE PRFLT5                 TO PRTFILE-IO-AREA (128:9)
               MOVE RESKNR                 TO PRTFILE-IO-AREA (138:6)
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (144:3)
               WRITE PRTFILE-IO-AREA
           END-IF
           IF  (I-42 AND NOT-I-89 AND NOT-I-14)
               MOVE SPACES TO PRTFILE-IO-AREA
               INITIALIZE PRTFILE-IO-AREA
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (1:3)
               MOVE SEQ-IO                 TO PRTFILE-IO-AREA (4:6)
               MOVE '37'                   TO PRTFILE-IO-AREA (10:2)
               MOVE FTEKS1                 TO PRTFILE-IO-AREA (24:100)
               MOVE PRFLT5                 TO PRTFILE-IO-AREA (128:9)
               MOVE RESKNR                 TO PRTFILE-IO-AREA (138:6)
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (144:3)
               WRITE PRTFILE-IO-AREA
               MOVE SPACES TO PRTFILE-IO-AREA
               INITIALIZE PRTFILE-IO-AREA
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (1:3)
               MOVE SEQ-IO                 TO PRTFILE-IO-AREA (4:6)
               MOVE '38'                   TO PRTFILE-IO-AREA (10:2)
               MOVE FTEKS2                 TO PRTFILE-IO-AREA (24:100)
               MOVE PRFLT5                 TO PRTFILE-IO-AREA (128:9)
               MOVE RESKNR                 TO PRTFILE-IO-AREA (138:6)
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (144:3)
               WRITE PRTFILE-IO-AREA
           END-IF
           IF  (I-50)
               MOVE SPACES TO RESFILE-IO-AREA
               INITIALIZE RESFILE-IO-AREA
               MOVE '02'                   TO RESFILE-IO-AREA (1:2)
               IF  (NOT-I-14)
                   MOVE '21'               TO RESFILE-IO-AREA (3:2)
               END-IF
               IF  (I-14)
                   MOVE '26'               TO RESFILE-IO-AREA (3:2)
               END-IF
               IF  (I-20)
                   MOVE '22'               TO RESFILE-IO-AREA (3:2)
               END-IF
               MOVE RESKNR                 TO RESFILE-IO-AREA (5:6)
               MOVE PFDAG                  TO RESFILE-IO-AREA (15:2)
               IF  (I-95)
                   MOVE UFDAG              TO RESFILE-IO-AREA (15:2)
               END-IF
               MOVE PFMND                  TO RESFILE-IO-AREA (13:2)
               IF  (I-95)
                   MOVE UFMND              TO RESFILE-IO-AREA (13:2)
               END-IF
               MOVE PFAAR                  TO RESFILE-IO-AREA (11:2)
               IF  (I-95)
                   MOVE UFAAR              TO RESFILE-IO-AREA (11:2)
               END-IF
               MOVE AREA9X-IO              TO RESFILE-IO-AREA (17:6)
               MOVE AREA9X-IO              TO RESFILE-IO-AREA (23:6)
               IF  (I-16)
                   MOVE FAKREF             TO RESFILE-IO-AREA (23:6)
               END-IF
               MOVE FFAAR                  TO RESFILE-IO-AREA (29:2)
               MOVE FFMND                  TO RESFILE-IO-AREA (31:2)
               MOVE FFDAG                  TO RESFILE-IO-AREA (33:2)
               MOVE BYBEL-IO               TO RESFILE-IO-AREA (35:9)
               MOVE FIRMNR                 TO RESFILE-IO-AREA (44:3)
               IF  (NOT-I-14)
                   MOVE '2'                TO RESFILE-IO-AREA (47:1)
               END-IF
               IF  (I-14)
                   MOVE '3'                TO RESFILE-IO-AREA (47:1)
               END-IF
               MOVE BTMA-ELGTE             TO RESFILE-IO-AREA (48:2)
               IF  (I-88)
                   MOVE '1'                TO RESFILE-IO-AREA (50:1)
               END-IF
               IF  (NOT-I-88)
                   MOVE '0'                TO RESFILE-IO-AREA (50:1)
               END-IF
               MOVE MOMS-IO                TO RESFILE-IO-AREA (51:9)
      *                      49          69 "1"
      *                     N49          69 " "
               MOVE ' '                    TO RESFILE-IO-AREA (69:1)
               MOVE ' '                    TO RESFILE-IO-AREA (70:1)
               WRITE RESFILE-IO-AREA
           END-IF
           IF  (I-50)
               MOVE SPACES TO EDIFAKF-IO-AREA
               INITIALIZE EDIFAKF-IO-AREA
               MOVE FIRMNR                 TO EDIFAKF-IO-AREA (1:3)
               MOVE RESKNR                 TO EDIFAKF-IO-AREA (4:6)
               MOVE AREA9X-IO              TO EDIFAKF-IO-AREA (10:6)
               MOVE '02'                   TO EDIFAKF-IO-AREA (16:2)
               IF  (NOT-I-14)
                   MOVE '21'               TO EDIFAKF-IO-AREA (16:2)
               END-IF
               IF  (I-14)
                   MOVE '26'               TO EDIFAKF-IO-AREA (16:2)
               END-IF
               IF  (I-20)
                   MOVE '22'               TO EDIFAKF-IO-AREA (16:2)
               END-IF
               MOVE 'H'                    TO EDIFAKF-IO-AREA (21:1)
               MOVE '0'                    TO EDIFAKF-IO-AREA (30:1)
               MOVE AREA9X-IO              TO EDIFAKF-IO-AREA (31:6)
               MOVE PFAAR                  TO EDIFAKF-IO-AREA (37:2)
               IF  (I-95)
                   MOVE UFAAR              TO EDIFAKF-IO-AREA (37:2)
               END-IF
               MOVE PFMND                  TO EDIFAKF-IO-AREA (39:2)
               IF  (I-95)
                   MOVE UFMND              TO EDIFAKF-IO-AREA (39:2)
               END-IF
               MOVE PFDAG                  TO EDIFAKF-IO-AREA (41:2)
               IF  (I-95)
                   MOVE UFDAG              TO EDIFAKF-IO-AREA (41:2)
               END-IF
               MOVE FFAAR                  TO EDIFAKF-IO-AREA (43:2)
               MOVE FFMND                  TO EDIFAKF-IO-AREA (45:2)
               MOVE FFDAG                  TO EDIFAKF-IO-AREA (47:2)
               MOVE BYBEL-IO               TO EDIFAKF-IO-AREA (51:9)
               MOVE FAKKID                 TO EDIFAKF-IO-AREA (78:7)
               MOVE BTMA-ELGTE             TO EDIFAKF-IO-AREA (85:2)
               MOVE NETBNR-IO              TO EDIFAKF-IO-AREA (87:9)
               IF  (I-44)
                   MOVE 'F'                TO EDIFAKF-IO-AREA (100:1)
               END-IF
               IF  (NOT-I-44)
                   MOVE 'P'                TO EDIFAKF-IO-AREA (100:1)
               END-IF
               MOVE '0000'                 TO EDIFAKF-IO-AREA (146:4)
               WRITE EDIFAKF-IO-AREA
           END-IF
           IF  (I-50)
               MOVE SPACES TO KIDRELF-IO-AREA
               INITIALIZE KIDRELF-IO-AREA
               MOVE 'F'                    TO KIDRELF-IO-AREA (1:1)
               MOVE KFIRNR                 TO KIDRELF-IO-AREA (2:3)
               MOVE FAKKID                 TO KIDRELF-IO-AREA (5:7)
               MOVE RESKNR                 TO KIDRELF-IO-AREA (12:6)
               MOVE BYBEL-IO               TO KIDRELF-IO-AREA (18:9)
               MOVE FFAAR                  TO KIDRELF-IO-AREA (27:2)
               MOVE FFMND                  TO KIDRELF-IO-AREA (29:2)
               MOVE FFDAG                  TO KIDRELF-IO-AREA (31:2)
               MOVE PFAAR                  TO KIDRELF-IO-AREA (37:2)
               MOVE FAKTOM                 TO KIDRELF-IO-AREA (39:2)
               WRITE KIDRELF-IO-AREA
           END-IF
           IF  (I-L5)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FIRMNR                 TO LISTE-IO-AREA (6:3)
               MOVE PFANR1-IO              TO LISTE-IO-AREA (12:6)
               MOVE PRARE1-IO              TO LISTE-IO-AREA (24:6)
               MOVE PKRNR1-IO              TO LISTE-IO-AREA (34:6)
               MOVE PRARE2-IO              TO LISTE-IO-AREA (46:6)
               MOVE ANTL2                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (57:6)
               INITIALIZE ANTL2
               MOVE KTOAVR                 TO LISTE-IO-AREA (67:4)
               MOVE TOTAF5                 TO XO-62YY9R
               MOVE XO-62YY9R              TO LISTE-IO-AREA (72:11)
               INITIALIZE TOTAF5
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALT'               TO LISTE-IO-AREA (3:6)
               MOVE ANTLR                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (57:6)
               MOVE TOTAVR                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (69:14)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE '************************' TO LISTE-IO-AREA (49:24)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '***   AVSTEMNINGSTOTALER' TO LISTE-IO-AREA (1:24)
               MOVE 'I PROGRAM FAK085.'    TO LISTE-IO-AREA (28:17)
               MOVE 'FAKTURADATO'          TO LISTE-IO-AREA (53:11)
               MOVE UFDAG                  TO LISTE-IO-AREA (67:2)
               MOVE '.'                    TO LISTE-IO-AREA (69:1)
               MOVE UFMND                  TO LISTE-IO-AREA (70:2)
               MOVE '.'                    TO LISTE-IO-AREA (72:1)
               MOVE UFAAR                  TO LISTE-IO-AREA (73:2)
               MOVE 'KJØREDATO'            TO LISTE-IO-AREA (77:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (88:8)
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FAKTURABELØP'         TO LISTE-IO-AREA (3:12)
               MOVE FAKSUM                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (16:15)
               MOVE 'KREDITNOTABELØP'      TO LISTE-IO-AREA (33:15)
               MOVE KRESUM                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (49:15)
               MOVE 'MOMSBELØP'            TO LISTE-IO-AREA (65:9)
               MOVE TOTMOM                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (75:14)
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTAL FAKT. OG KR.NOTA.' TO LISTE-IO-AREA (25:23)
               MOVE TOTSUM                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (49:15)
               MOVE 'MOMSBELØP'            TO LISTE-IO-AREA (65:9)
               MOVE TOTMOM                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (75:14)
               MOVE 'ØREAVRUND'            TO LISTE-IO-AREA (91:9)
               MOVE TOTAVR                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (101:14)
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL FAKTURA'       TO LISTE-IO-AREA (3:14)
               MOVE TOTAFA                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (18:7)
               MOVE 'ANTALL KREDITNOTA'    TO LISTE-IO-AREA (31:17)
               MOVE TOTAKR                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (50:7)
               MOVE 'ANT. REC. LEST'       TO LISTE-IO-AREA (58:14)
               MOVE ANTREC                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (73:7)
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL KUNDER UTEN MAKKE' TO LISTE-IO-AREA (1:24)
               MOVE 'R I KUNDEARKIVET        ' TO LISTE-IO-AREA (25:24)
               MOVE KFEIL                  TO XO-90YY9
               MOVE XO-90YY9               TO LISTE-IO-AREA (45:11)
               MOVE 2                      TO LISTE-BEFORE-SPACE
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'DERSOM ANTALL KUNDER UTE' TO LISTE-IO-AREA (1:24)
               MOVE 'N MAKKER IKKE ER NULL,- ' TO LISTE-IO-AREA (25:24)
               MOVE 'KONTAKT SYSTEMAVD FØR PR' TO LISTE-IO-AREA (49:24)
               MOVE 'INT AV FAKTURAER.       ' TO LISTE-IO-AREA (73:24)
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE '************************' TO LISTE-IO-AREA (49:24)
      * DUMMY-LINJE FOR Å FJERNE KOMPILERINGSFEIL
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR AND I-01 AND I-92)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE PSDS                   TO LISTE-IO-AREA (41:80)
               MOVE R                      TO LISTE-IO-AREA (113:8)
               MOVE P-IO                   TO LISTE-IO-AREA (118:3)
               MOVE S-IO                   TO LISTE-IO-AREA (116:5)
               MOVE BUMVA-IO               TO LISTE-IO-AREA (110:11)
               MOVE '                        ' TO LISTE-IO-AREA (97:24)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L5 AND I-43)
               MOVE PRARE1-IO              TO FAKTNR-IO-AREA (18:6)
               INITIALIZE PRARE1-IO
               MOVE PRARE2-IO              TO FAKTNR-IO-AREA (24:6)
               INITIALIZE PRARE2-IO
               MOVE PFAAR                  TO FAKTNR-IO-AREA (30:2)
               MOVE FAKTOM                 TO FAKTNR-IO-AREA (32:2)
               IF  (I-09)
                   MOVE PFANR1-IO          TO FAKTNR-IO-AREA (39:6)
               END-IF
               IF  (I-09)
                   MOVE PKRNR1-IO          TO FAKTNR-IO-AREA (45:6)
               END-IF
               REWRITE FAKTNR-IO-AREA
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
           PERFORM AVDTAB-LOAD
           PERFORM FAKSGR-LOAD
           PERFORM FAKESS-LOAD
           INITIALIZE FAKTNR-DATA-FIELDS
           OPEN I-O FAKTNR
           INITIALIZE FAKPAR-DATA-FIELDS
           OPEN INPUT FAKPAR
           SET FAKTIN-LEVEL-INIT           TO TRUE
           INITIALIZE FAKTIN-DATA-FIELDS
           SET FAKTIN-EOF-OFF              TO TRUE
           SET FAKTIN-PROCESS              TO TRUE
           OPEN INPUT FAKTIN
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           INITIALIZE KUNDEMX-DATA-FIELDS
           OPEN INPUT KUNDEMX
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           INITIALIZE TEKSTF-DATA-FIELDS
           OPEN INPUT TEKSTF
           OPEN OUTPUT ORDFAK
           OPEN OUTPUT RESFILE
           OPEN OUTPUT KIDRELF
           OPEN OUTPUT REGFILE
           OPEN OUTPUT EDIFAKF
           OPEN OUTPUT PRTFILE
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           SET TABAVD-I                    TO 1
           SET TABFVG-I                    TO 1
           SET TABFVE-I                    TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FAKTNR
           CLOSE FAKPAR
           CLOSE FAKTIN
           CLOSE KUNDEMA
           CLOSE KUNDEMX
           CLOSE FIRMAF
           CLOSE TEKSTF
           CLOSE ORDFAK
           CLOSE RESFILE
           CLOSE KIDRELF
           CLOSE REGFILE
           CLOSE EDIFAKF
           CLOSE PRTFILE
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
