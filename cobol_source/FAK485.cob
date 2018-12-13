       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK485R.
      ****************************************************************
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM: FAK485                                             *
      *  SAMME SOM FAK085 MEN FOR FAKT.KONTANT RUTINEN.              *
      * 18/2-94 ESPEN LARSEN                                         *
      *  1/1-95 NY MOMS.  FRA 22 TIL 23 PROSENT                      *
      * 25.07.97 TAKLER ALTERNATIVT KONTONR. MT.                     *
      *  3.02.00 KONSERN FIRMANR. BENYTTES MOT KUNDE.MASTER.         *
      * 16.06.00 FJERNET TEST PÅ NULL I ANTALL VED UTREGNING.        *
      *  1/1-01 NY MOMS.  FRA 23 TIL 24 PROSENT                      *
      * 03.11.03 NY VERSJON AV KONTANTFAKTURA. MT                    *
      * 01.01.05 NY MOMSSATS.  ENDRET FRA 24% TIL 25%.               *
      * 28.03.12 ENDRET TEKSTEN ""MOMS"" TIL ""MVA"" MT                  *
      * 04.10.16 NY AVG.KODE, BRUKER NY INDIKATOR=18               BH*
      * 22.03.17 NYE LINJER PÅ FAK08. PÅLAGT AV SKATT Ø.           BH*
      *        - TELLER OG SUMMERER KREDITLINJER                   BH*
      *        - HENTER KASSEOPPGJØRSTID FRA KONTORD-SUMLINJE      BH*
      * 31.08.17 SUMMERING AV KREDITLINJER FEILET I NOEN TILFELLER BH*
      *          ENDRET INDIKATORVALG FOR Å KJØRE SUBRUTINE        BH*
      ****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK485.rpg
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
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT KONTFAK
               ASSIGN TO KONTFAK
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KONTFAK-STATUS
               RECORD KEY IS KONTFAK-KEY1.
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
           SELECT KONTORD
               ASSIGN TO KONTORD
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KONTORD-STATUS
               RECORD KEY IS KONTORD-KEY1.
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
           SELECT REGFILE
               ASSIGN TO UT-S-REGFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS REGFILE-STATUS.
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
       FD FAKTIN
               BLOCK CONTAINS 2000
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
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD KONTFAK
               RECORD CONTAINS 40.
       01  KONTFAK-IO-AREA.
           05  KONTFAK-IO-AREA-X.
               10  KONTFAK-KEY1            PICTURE X(9).
               10  FILLER                  PICTURE X(31).
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
       FD KONTORD
               RECORD CONTAINS 400.
       01  KONTORD-IO-AREA.
           05  KONTORD-IO-AREA-X.
               10  KONTORD-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(390).
       FD ORDFAK
               BLOCK CONTAINS 1960
               RECORD CONTAINS 20.
       01  ORDFAK-IO-AREA.
           05  ORDFAK-IO-AREA-X            PICTURE X(20).
       FD RESFILE
               BLOCK CONTAINS 1960
               RECORD CONTAINS 70.
       01  RESFILE-IO-AREA.
           05  RESFILE-IO-AREA-X           PICTURE X(70).
       FD REGFILE
               BLOCK CONTAINS 2640
               RECORD CONTAINS 80.
       01  REGFILE-IO-AREA.
           05  REGFILE-IO-AREA-X           PICTURE X(80).
      *PRTFILE O   F1460 146    2       DISK40 SYS015S
       FD PRTFILE
               RECORD CONTAINS 146.
       01  PRTFILE-IO-AREA.
           05  PRTFILE-IO-AREA-X           PICTURE X(146).
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
           10  FAKTIN-STATUS               PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  KONTFAK-STATUS              PICTURE 99 VALUE 0.
           10  SYSPARM-STATUS              PICTURE 99 VALUE 0.
           10  KONTOMA-STATUS              PICTURE 99 VALUE 0.
           10  KONTORD-STATUS              PICTURE 99 VALUE 0.
           10  ORDFAK-STATUS               PICTURE 99 VALUE 0.
           10  RESFILE-STATUS              PICTURE 99 VALUE 0.
           10  REGFILE-STATUS              PICTURE 99 VALUE 0.
           10  PRTFILE-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  MVFELT-XX-STATUS            PICTURE 99 VALUE 0.
           10  KOFELT-XX-STATUS            PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
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
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  KONTFAK-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  SYSPARM-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  KONTOMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  KONTORD-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  KOFELT-XX REDEFINES MVFELT-XX-DATA-FIELDS.
               10  KOFIRM                  PICTURE X(3).
               10  FILLER                  PICTURE X(30).
           05  FILLER REDEFINES MVFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(3).
               10  KOSUMN-IO.
                   15  KOSUMN              PICTURE S9(13) USAGE
                                                       PACKED-DECIMAL.
               10  FILLER                  PICTURE X(23).
      *DSDS: DATA STRUCTURE FIELDS
           05  KOFELT-XX-DATA-FIELDS.
               10  KOFIRM                  PICTURE X(3).
               10  FILLER                  PICTURE X(7).
           05  FILLER REDEFINES KOFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(3).
               10  KOSUMN-IO.
                   15  KOSUMN              PICTURE S9(13) USAGE
                                                       PACKED-DECIMAL.
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
                   15  FAKTIN-02-L2-BRKOD  PICTURE X(3).
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
               10  BRKOD                   PICTURE X(3).
               10  ONR                     PICTURE X(6).
               10  RA                      PICTURE X(1).
               10  KUREF                   PICTURE X(15).
      *                                      41  70 VARAD1
               10  FAKREF                  PICTURE X(6).
               10  FAKDAG                  PICTURE X(2).
               10  FAKMND                  PICTURE X(2).
               10  FAKAAR                  PICTURE X(2).
               10  BK                      PICTURE X(1).
               10  FRITT                   PICTURE X(1).
               10  VGR                     PICTURE X(5).
      *                                      77 106 VARAD2
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
               10  KRETYP                  PICTURE X(1).
               10  KUNDNR                  PICTURE X(6).
           05  KUNDEMA-DATA-FIELDS.
               10  CNAVN1                  PICTURE X(30).
               10  CNAVN2                  PICTURE X(30).
               10  CADR                    PICTURE X(30).
               10  CPOST                   PICTURE X(15).
               10  CPNR                    PICTURE X(4).
               10  CPNR3                   PICTURE X(3).
               10  CBETM                   PICTURE X(2).
               10  HDIST                   PICTURE X(3).
               10  HDIST1                  PICTURE X(1).
           05  FIRMAF-DATA-FIELDS.
               10  KONFNR                  PICTURE X(3).
      *                                     703 703 PRIFNR
               10  BILNR2-IO.
                   15  BILNR2              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  BILNR8-IO.
                   15  BILNR8              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  STEAVR                  PICTURE X(4).
               10  KTOAVR                  PICTURE X(4).
      *                                   P 896 9000FTAKNR
           05  KONTFAK-DATA-FIELDS.
               10  HBKTO                   PICTURE X(4).
               10  KASSE                   PICTURE X(2).
               10  ORDTOT-IO.
                   15  ORDTOT              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
           05  SYSPARM-DATA-FIELDS.
               10  SYSALT                  PICTURE X(1).
           05  KONTOMA-DATA-FIELDS.
               10  ALTKTO                  PICTURE X(4).
           05  KONTORD-DATA-FIELDS.
               10  KOTIM                   PICTURE X(2).
               10  KOMIN                   PICTURE X(2).
               10  KODAG                   PICTURE X(2).
               10  KOMND                   PICTURE X(2).
               10  KOAAR                   PICTURE X(2).
      *
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L5            PICTURE X(3).
               10  THE-PRIOR-L4            PICTURE X(2).
               10  THE-PRIOR-L3            PICTURE X(6).
               10  THE-PRIOR-L2            PICTURE X(7).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  KREANT-IO.
                   15  KREANT              PICTURE S9(2).
               10  KRETOT-IO.
                   15  KRETOT              PICTURE S9(7)V9(2).
               10  BRKODE-IO.
                   15  BRKODE              PICTURE S9(6).
               10  ANTREC-IO.
                   15  ANTREC              PICTURE S9(6).
               10  SEQ-IO.
                   15  SEQ                 PICTURE S9(6).
               10  AREA1-IO.
                   15  AREA1               PICTURE S9(6).
               10  BILNR2-N-IO.
                   15  BILNR2-N            PICTURE S9(7).
               10  AREA1F-IO.
                   15  AREA1F              PICTURE S9(6).
               10  SYSKEY                  PICTURE X(10).
               10  KTOKEY                  PICTURE X(7).
               10  ALTAVR                  PICTURE X(4).
               10  ORDTL2-IO.
                   15  ORDTL2              PICTURE S9(7)V9(2).
               10  FNRONR                  PICTURE X(9).
               10  ALTHB                   PICTURE X(4).
               10  RESKEY                  PICTURE X(9).
               10  KFEIL-IO.
                   15  KFEIL               PICTURE S9(9).
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
               10  PRFLT4                  PICTURE X(3).
               10  PRFLT5                  PICTURE X(9).
               10  PRARE1-IO.
                   15  PRARE1              PICTURE S9(6).
               10  SIGNX1                  PICTURE X(1).
               10  BXBEL-IO.
                   15  BXBEL               PICTURE S9(7)V9(2).
               10  AREA8X-IO.
                   15  AREA8X              PICTURE S9(6).
               10  TOTAL-IO.
                   15  TOTAL               PICTURE S9(7)V9(2).
               10  MOMS-IO.
                   15  MOMS                PICTURE S9(7)V9(2).
               10  MOMSX1-IO.
                   15  MOMSX1              PICTURE S9(7)V9(2).
               10  SIGNXM                  PICTURE X(1).
               10  TOTMOM-IO.
                   15  TOTMOM              PICTURE S9(7)V9(2).
               10  OREDIF-IO.
                   15  OREDIF              PICTURE S9(7)V9(2).
               10  SIGNXO                  PICTURE X(1).
               10  OREDF1-IO.
                   15  OREDF1              PICTURE S9(7)V9(2).
               10  AREA9X-IO.
                   15  AREA9X              PICTURE S9(6).
               10  TOTSUM-IO.
                   15  TOTSUM              PICTURE S9(8)V9(2).
               10  BILN8X-IO.
                   15  BILN8X              PICTURE S9(6).
               10  SIGNXH                  PICTURE X(1).
               10  KASBEL-IO.
                   15  KASBEL              PICTURE S9(7)V9(2).
               10  TOTREG-IO.
                   15  TOTREG              PICTURE S9(8)V9(2).
               10  TOTRES-IO.
                   15  TOTRES              PICTURE S9(8)V9(2).
               10  ANTL2-IO.
                   15  ANTL2               PICTURE S9(5).
               10  ANTLR-IO.
                   15  ANTLR               PICTURE S9(5).
               10  ANTFAK-IO.
                   15  ANTFAK              PICTURE S9(6).
               10  TOTAFA-IO.
                   15  TOTAFA              PICTURE S9(6).
               10  KOFLT1                  PICTURE X(4).
               10  KOFLT2                  PICTURE X(4).
               10  KOFLT3                  PICTURE X(5).
               10  KOFLT4                  PICTURE X(8).
               10  KOFLT5                  PICTURE X(13).
               10  KOKEY2-IO.
                   15  KOKEY2              PICTURE S9(13).
               10  KOKEY2-N-IO.
                   15  KOKEY2-N            PICTURE S9(13).
               10  KOSKEY                  PICTURE X(10).
           05  EDITTING-FIELDS.
               10  EDIT-ORDTOT             PICTURE ZZZZ.ZZZ,ZZ-.
               10  EDIT-ORDTL2             PICTURE ZZZZ.ZZ9,99-.
               10  EDIT-OREDIF             PICTURE ZZZZ.ZZ9,99-.
               10  EDIT-MOMS               PICTURE ZZZZ.ZZ9,99-.
               10  EDIT-KREANT             PICTURE ZZ.
               10  EDIT-KRETOT             PICTURE ZZZZ.ZZ9,99-.
               10  XO-70D                  PICTURE S9(7).
               10  XO-70U                  PICTURE 9(7).
               10  XO-50YY9                PICTURE ZZ.ZZ9.
               10  XO-82YY9R               PICTURE ZZ.ZZZ.ZZZ,99-.
               10  XO-72YY9R               PICTURE Z.ZZZ.ZZZ,99-.
               10  XO-60YY9                PICTURE ZZZ.ZZ9.
               10  XO-90YY9                PICTURE ZZZ.ZZZ.ZZ9.
               10  XO-130D                 PICTURE S9(13).
               10  XO-130U                 PICTURE 9(13).
               10  XO-70P-EF.
                 15  XO-70P                PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
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
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-05                    TO TRUE
           SET NOT-I-09                    TO TRUE
           SET NOT-I-06                    TO TRUE
           SET NOT-I-07                    TO TRUE
           SET NOT-I-10                    TO TRUE
 
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
           SET NOT-I-20                    TO TRUE
           SET NOT-I-23                    TO TRUE
           SET NOT-I-60                    TO TRUE
           SET NOT-I-62                    TO TRUE
           SET NOT-I-69                    TO TRUE
           SET NOT-I-66                    TO TRUE
           SET NOT-I-68                    TO TRUE
           SET NOT-I-70                    TO TRUE
           SET NOT-I-72                    TO TRUE
           SET NOT-I-76                    TO TRUE
           SET NOT-I-64                    TO TRUE
           SET NOT-I-78                    TO TRUE
           SET NOT-I-32                    TO TRUE
           SET NOT-I-84                    TO TRUE
           SET NOT-I-33                    TO TRUE
           SET NOT-I-51                    TO TRUE
           SET NOT-I-36                    TO TRUE
           SET NOT-I-37                    TO TRUE
           SET NOT-I-38                    TO TRUE
           SET NOT-I-41                    TO TRUE
           SET NOT-I-08                    TO TRUE
           SET NOT-I-18                    TO TRUE
           IF  (I-L1)
               SET NOT-I-56                TO TRUE
               SET NOT-I-57                TO TRUE
               SET NOT-I-58                TO TRUE
               SET NOT-I-59                TO TRUE
           END-IF
           IF  (I-L2)
               SET NOT-I-42                TO TRUE
               SET NOT-I-44                TO TRUE
               SET NOT-I-45                TO TRUE
               SET NOT-I-50                TO TRUE
               SET NOT-I-88                TO TRUE
           END-IF
           IF  (I-L5)
               SET NOT-I-40                TO TRUE
           END-IF
           IF  (I-L2)
               OR  (I-L5)
               SET NOT-I-24                TO TRUE
           END-IF
           IF  (I-L2)
               OR  (I-L5)
               MOVE 0                      TO KREANT
               MOVE 0,00                   TO KRETOT
           END-IF
           IF  (I-L2)
               ADD 1                       TO BRKODE
               MOVE 0,00                   TO NETBNR
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
               SET I-69                    TO TRUE
           END-IF
           IF  (I-L5)
               SET I-22                    TO TRUE
               SET I-10                    TO TRUE
               MOVE FIRMNR                 TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-55                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-55            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
           END-IF
           IF  (I-L5 AND NOT-I-55)
               SET I-40                    TO TRUE
           END-IF
           IF  (I-L5)
               MOVE BILNR2                 TO BILNR2-N
               MOVE BILNR2-N-IO (2:6)      TO AREA1-IO
               ADD 1                       TO AREA1
               ADD AREA1 TO ZERO       GIVING AREA1F
      *****************************************************************
      * TESTER PÅ OM FORETAKSNUMMER SKAL PRINTES.                     *
      *****************************************************************
      *  L5      FTAKNR    COMP 0                    25     FORETAKSNR.
      *  L5      PRIFNR    COMP "A"                      53 IKKE PRINT.
      *  L5N53   PRIFNR    COMP "C"                      53 IKKE PRINT.
      *  L5 53             SETOF                     25     IKKE PRINT.
      *****************************************************************
      * LESER SYSPARM OG SJEKKER OM ALTERNATIVT KONTONR SKAL BRUKES.  *
      *****************************************************************
           END-IF
           IF  (I-L5)
               SET NOT-I-81                TO TRUE
               SET NOT-I-82                TO TRUE
               SET NOT-I-83                TO TRUE
               MOVE FIRMNR                 TO SYSKEY (1:3)
               MOVE 'REGA011'              TO SYSKEY (4:7)
               MOVE SYSKEY                 TO SYSPARM-KEY1
               READ SYSPARM RECORD KEY IS SYSPARM-KEY1
               INVALID KEY
                   SET I-81                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-81            TO TRUE
                   PERFORM SYSPARM-FLDSET
                   PERFORM SYSPARM-IDSET
               END-READ
      *  L5                MOVE "SYSALT  "BUGFL1  8        DISPLAY FIELD
      *  L5      BUGFL1    DEBUGBUGFILO   SYSALT           VIS INDIKATOR
           END-IF
           IF  (I-L5 AND NOT-I-81)
               SET NOT-I-82                TO TRUE
               IF  SYSALT = 'J'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (I-L5 AND I-82)
               MOVE FIRMNR                 TO KTOKEY (1:3)
               MOVE KTOAVR                 TO KTOKEY (4:4)
               MOVE KTOKEY                 TO KONTOMA-KEY1
               READ KONTOMA RECORD KEY IS KONTOMA-KEY1
               INVALID KEY
                   SET I-83                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-83            TO TRUE
                   PERFORM KONTOMA-FLDSET
                   PERFORM KONTOMA-IDSET
               END-READ
           END-IF
           IF  (I-L5 AND I-82 AND NOT-I-83)
               MOVE ALTKTO                 TO ALTAVR
      *****************************************************************
      * TEST OM DET ER EN KREDIT ORDRE.                               *
      *****************************************************************
           END-IF
           SET NOT-I-21                    TO TRUE
           IF  ONR > '899999'
               SET I-21                    TO TRUE
           END-IF
      *****************************************************************
      * LESE FAKTURA.KONTANT.FILE                                     *
      *      SUMMERE ORDRESUM (INK.MVA). KREDIT.ORDRE ER NEGATIV      *
      *****************************************************************
           IF  (I-L2)
               SUBTRACT ORDTL2             FROM ORDTL2
           END-IF
           IF  (I-L5)
               MOVE FIRMNR                 TO FNRONR (1:3)
           END-IF
           IF  (I-L1)
               MOVE ONR                    TO FNRONR (4:6)
               MOVE FNRONR                 TO KONTFAK-KEY1
               READ KONTFAK RECORD KEY IS KONTFAK-KEY1
               INVALID KEY
                   SET I-16                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-16            TO TRUE
                   PERFORM KONTFAK-FLDSET
                   PERFORM KONTFAK-IDSET
               END-READ
           END-IF
           IF  (I-L1 AND NOT-I-16)
               ADD ORDTOT                  TO ORDTL2
           END-IF
           IF  (I-L2 AND NOT-I-16)
               OR  (I-L5 AND NOT-I-16)
               PERFORM OPPTID-S
           END-IF
           IF  (I-L1 AND I-82 AND NOT-I-16)
               MOVE FIRMNR                 TO KTOKEY (1:3)
               MOVE HBKTO                  TO KTOKEY (4:4)
               MOVE KTOKEY                 TO KONTOMA-KEY1
               READ KONTOMA RECORD KEY IS KONTOMA-KEY1
               INVALID KEY
                   SET I-85                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-85            TO TRUE
                   PERFORM KONTOMA-FLDSET
                   PERFORM KONTOMA-IDSET
               END-READ
           END-IF
           IF  (I-L1 AND I-82 AND NOT-I-85)
               MOVE ALTKTO                 TO ALTHB
           END-IF
           IF  (I-L1)
               SET I-19                    TO TRUE
      ******************************************************
      *  FAKTURA UTSKRIFT UTEN BRUTTOPRIS OG RABATTER.     *
      ******************************************************
      *          FLSIDE    COMP "F"                      71 FAKTURA FLER
      *****************************************************************
           END-IF
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
           MOVE RESKNR                     TO RESKEY (4:6)
           MOVE FIRMNR                     TO RESKEY (1:3)
           IF  (I-40)
               SET NOT-I-90                TO TRUE
               IF  KONFNR > '000'
                   SET I-90                TO TRUE
               END-IF
           END-IF
           IF  (I-40 AND I-90)
               MOVE KONFNR                 TO RESKEY (1:3)
           END-IF
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
      *
           END-IF
           SET I-78                        TO TRUE
           SET I-12                        TO TRUE.
 
       D1-T.
           IF  (I-15)
               GO TO D2-T
           END-IF
           GO TO D3-T.
 
       D2-T.
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
           GO TO E1-T.
 
       D5-T.
      *****************************************************************
      * RUTINE FOR Å SNU BELØP ORDRE/KRRDIT ORDRE.                    *
      *  DERSOM EDBNR BEGYNNER MED "995" ELLER "94" SNUES BELØPET.    *
      *****************************************************************
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
           IF  (NOT-I-21 AND I-33)
               GO TO D6-T
           END-IF
           IF  (I-21 AND NOT-I-33)
               GO TO D6-T
           END-IF
           MULTIPLY LEVENH BY ENHPR    GIVING BTOPRI ROUNDED
           GO TO D10-T.
 
       D6-T.
           MULTIPLY LEVENH BY ENHPR    GIVING BTOPRI ROUNDED
           MULTIPLY -1 BY BTOPRI       GIVING BTOPRI
      *  21N33 L1          EXSR KREDIT
           IF  (I-21 AND NOT-I-33 AND NOT-I-16)
               AND (I-19)
               PERFORM KREDIT-S
           END-IF.
 
       D10-T.
      *****************************************************************
      *  TREKKE IFRA RABATTER.                                        *
      *****************************************************************
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
           MOVE BRTO2                      TO NETTO-IO.
 
       D98-T.
           ADD NETTO                       TO NETBNR
           MULTIPLY LEVENH BY KOSTPR   GIVING SUMSP
           SET I-41                        TO TRUE
           IF  (I-19)
               SET I-20                    TO TRUE
           END-IF
           SET NOT-I-19                    TO TRUE
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
           END-IF.
 
       E1-T.
           CONTINUE.
 
       SLUTT-T.
           MOVE '1'                        TO PRFLT4 (3:1)
           MOVE FAKRFL                     TO PRFLT4 (1:2)
           MOVE PRFLT4                     TO PRFLT5 (7:3)
           MOVE BRKODE                     TO PRFLT5 (1:6)
           MOVE AREA1                      TO PRARE1-IO
      ** MLLzo
           IF PRARE1 < 0
               MULTIPLY -1 BY PRARE1
           END-IF
      *****************************************************************
      *  TEST BELØP FOR REGNSKAPSREC OG SETT TEGN                     *
      *****************************************************************
           SET NOT-I-84                    TO TRUE
           IF  NETTO < 0,00
               SET I-84                    TO TRUE
           END-IF
           IF  (I-84)
               GO TO X1B-T
           END-IF
           MOVE ' '                        TO SIGNX1
           MOVE NETTO                      TO BXBEL-IO
           ADD BXBEL                       TO TOTREG
           GO TO X1C-T.
 
       X1B-T.
           MULTIPLY -1 BY NETTO        GIVING BXBEL
           MOVE '-'                        TO SIGNX1
           SUBTRACT BXBEL                  FROM TOTREG.
 
       X1C-T.
      ** MLLzo
           IF BXBEL < 0
               MULTIPLY -1 BY BXBEL
           END-IF
      ******************************************************
           .
 
       KREDIT-S SECTION.
       KREDIT-S-P.
           SET I-24                        TO TRUE
           ADD 1                           TO KREANT
           ADD ORDTOT                      TO KRETOT
           SET NOT-I-56                    TO TRUE
           IF  KRETYP = '2'
               SET I-56                    TO TRUE
           END-IF
           SET NOT-I-57                    TO TRUE
           IF  KRETYP = '3'
               SET I-57                    TO TRUE
           END-IF
           SET NOT-I-58                    TO TRUE
           IF  KRETYP = '4'
               SET I-58                    TO TRUE
           END-IF
           SET NOT-I-59                    TO TRUE
           IF  KRETYP = '5'
               SET I-59                    TO TRUE
           END-IF.
      *****************************************************************
      *  RUTINE FOR Å HENTE TID FOR KASSEOPPGJØR.                     *
      *****************************************************************
 
       OPPTID-S SECTION.
       OPPTID-S-P.
           MOVE FIRMNR                     TO KOFIRM
           MOVE '20'                       TO KOFLT1 (1:2)
           MOVE FAKAAR                     TO KOFLT1 (3:2)
           MOVE FAKMND                     TO KOFLT2 (1:2)
           MOVE FAKDAG                     TO KOFLT2 (3:2)
           MOVE KASSE                      TO KOFLT3 (1:2)
           MOVE '101'                      TO KOFLT3 (3:3)
           MOVE KOFLT1                     TO KOFLT4 (1:4)
           MOVE KOFLT2                     TO KOFLT4 (5:4)
           MOVE KOFLT4                     TO KOFLT5 (1:8)
           MOVE KOFLT3                     TO KOFLT5 (9:5)
           MOVE KOFLT5                     TO KOKEY2
           MOVE KOKEY2                     TO KOKEY2-N
           MOVE KOKEY2-N-IO                TO KOSUMN-IO
           MOVE KOFELT                     TO KOSKEY (1:0)
           MOVE KOSKEY                     TO KONTORD-KEY1
           READ KONTORD RECORD KEY IS KONTORD-KEY1
           INVALID KEY
               SET I-86                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-86                TO TRUE
               PERFORM KONTORD-FLDSET
               PERFORM KONTORD-IDSET
           END-READ.
      *****************************************************************
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1)
               SET I-15                    TO TRUE
               MOVE AREA1                  TO AREA8X-IO
      ** MLLzo
               IF AREA8X < 0
                   MULTIPLY -1 BY AREA8X
               END-IF
           END-IF
           IF  (I-L2)
               SET NOT-I-15                TO TRUE
               SET I-42                    TO TRUE
               MOVE '3'                    TO PRFLT5 (9:1)
               SET NOT-I-44                TO TRUE
               IF  FRITT = '5'
                   SET I-44                TO TRUE
               END-IF
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *  COBOL SUBRUTINE FOR Å BEREGNE MOMS UTEN AVRUNDING.     *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
           END-IF
           IF  (I-L2)
               SET NOT-I-94                TO TRUE
               IF  FAKAAR = '04'
                   SET I-94                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-44)
               ADD NETBNR TO ZERO      GIVING TOTAL
               GO TO F1-T
           END-IF
           IF  (I-L2)
               ADD NETBNR TO ZERO      GIVING BUMVA
               MOVE 0                      TO BMMVA
               MOVE 99,99                  TO MVA
           END-IF
           IF  (I-L2 AND NOT-I-94)
               GO TO NYMOMS-T
           END-IF
           IF  (I-L2)
               CALL 'MOMSRUTX' USING MVFELT-XX-DATA-FIELDS
               GO TO XXMOMS-T
           END-IF.
 
       NYMOMS-T.
           IF  (I-L2)
               CALL 'RUTMOMS2' USING MVFELT-XX-DATA-FIELDS
           END-IF.
 
       XXMOMS-T.
           IF  (I-L2)
               ADD MVA TO ZERO         GIVING MOMS
               MOVE MOMS                   TO MOMSX1-IO
      ** MLLzo
               IF MOMSX1 < 0
                   MULTIPLY -1 BY MOMSX1
               END-IF
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
               ADD MOMSX1                  TO TOTREG
           END-IF
           IF  (I-L2 AND I-17)
               SUBTRACT MOMSX1             FROM TOTREG
           END-IF
           IF  (I-L2)
               ADD MOMS                    TO TOTMOM
               ADD MOMS TO NETBNR      GIVING TOTAL
               SET I-50                    TO TRUE
               SET I-45                    TO TRUE
           END-IF.
 
       F1-T.
           IF  (I-L2)
               SUBTRACT TOTAL FROM ORDTL2 GIVING OREDIF
               SET NOT-I-17                TO TRUE
               SET NOT-I-26                TO TRUE
               IF  OREDIF < 0
                   SET I-17                TO TRUE
               END-IF
               IF  OREDIF = 0
                   SET I-26                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-17)
               MOVE '-'                    TO SIGNXO
           END-IF
           IF  (I-L2 AND NOT-I-17)
               MOVE ' '                    TO SIGNXO
           END-IF
           IF  (I-L2)
               MOVE OREDIF                 TO OREDF1-IO
      ** MLLzo
               IF OREDF1 < 0
                   MULTIPLY -1 BY OREDF1
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-17)
               ADD OREDF1                  TO TOTREG
           END-IF
           IF  (I-L2 AND I-17)
               SUBTRACT OREDF1             FROM TOTREG
           END-IF
           IF  (I-L2 AND NOT-I-44)
               GO TO F2-T
           END-IF
           IF  (I-L2)
               SET I-50                    TO TRUE
           END-IF.
 
       F2-T.
           IF  (I-L2)
               MOVE AREA1                  TO AREA9X-IO
      ** MLLzo
               IF AREA9X < 0
                   MULTIPLY -1 BY AREA9X
               END-IF
               ADD ORDTL2                  TO TOTSUM
               SET NOT-I-88                TO TRUE
               IF  BTMA-ELGTE = CBETM
                   SET I-88                TO TRUE
               END-IF
      *****************************************************************
      * TEST OM VI SKAL RESKONTROFØRE ELLER POSTERE TIL KASSA.        *
      * ER DET HOVEDBOKSKONTO SKAL VI IKKE RESKONTROFØRE.             *
      * KORTSELSKAPER FJERNER HOVEDBOKSKONTO I KONTANT.FAKTURA.FILE   *
      *****************************************************************
           END-IF
           IF  (I-L2)
               SET NOT-I-51                TO TRUE
               IF  HBKTO > '0000'
                   SET I-51                TO TRUE
               END-IF
      *2 51                MOVE "SEQ     "BUGFL2  8        LEDETXT DEBUG
      *2 51      BUGFL2    DEBUGBUGFILO   SEQ              VIS FELT/IND
           END-IF
           IF  (I-L2 AND I-51)
               SET NOT-I-50                TO TRUE
               ADD 1                       TO BILNR8
               ADD BILNR8 TO ZERO      GIVING BILN8X
           END-IF
           IF  (I-L2)
               SET NOT-I-17                TO TRUE
               IF  ORDTL2 < 0,00
                   SET I-17                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND I-17)
               MOVE '-'                    TO SIGNXH
           END-IF
           IF  (I-L2 AND NOT-I-17)
               MOVE ' '                    TO SIGNXH
           END-IF
           IF  (I-L2)
               MOVE ORDTL2                 TO KASBEL-IO
      ** MLLzo
               IF KASBEL < 0
                   MULTIPLY -1 BY KASBEL
               END-IF
           END-IF
           IF  (I-L2 AND I-51 AND NOT-I-17)
               ADD KASBEL                  TO TOTREG
           END-IF
           IF  (I-L2 AND I-51 AND I-17)
               SUBTRACT KASBEL             FROM TOTREG
           END-IF
           IF  (I-L2 AND I-50)
               ADD ORDTL2                  TO TOTRES
      *********************************************************
      * FAKT. KR.NOTA NUMMERTILDELING SAMT TELLING AV ANTALL. *
      *********************************************************
           END-IF
           IF  (I-L2)
               ADD 1                       TO ANTL2
               ADD 1                       TO ANTLR
               ADD 1                       TO AREA1
               MOVE AREA1                  TO PRARE1-IO
      ** MLLzo
               IF PRARE1 < 0
                   MULTIPLY -1 BY PRARE1
               END-IF
      ********************************************************
      * * *  BEREGNING AV SISTE FAKTURA/KREDITNOTA-NR. PR FIRMNR.   * * *
           END-IF
           IF  (I-L5)
               SUBTRACT 1                  FROM AREA1
               ADD AREA1 TO ZERO       GIVING BILNR2
               MOVE AREA1                  TO PRARE1-IO
      ** MLLzo
               IF PRARE1 < 0
                   MULTIPLY -1 BY PRARE1
               END-IF
      * * *  ADDERING AV TOTALT ANT. FAKTURA OG KREDITNOTA  * * *
           END-IF
           IF  (I-L5)
               SUBTRACT AREA1F FROM PRARE1 GIVING ANTFAK
               ADD 1                       TO ANTFAK
               SET NOT-I-75                TO TRUE
               IF  ANTFAK < 0
                   SET I-75                TO TRUE
               END-IF
           END-IF
           IF  (I-L5 AND NOT-I-75)
               ADD ANTFAK                  TO TOTAFA
      *****************************************************************
      *  RUTINE FOR Å SUMMERE KREDITLINJER.                           *
      *****************************************************************
           END-IF
           .
 
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
               MOVE FAKTIN-IO-AREA (16:3)  TO BRKOD (1:3)
               MOVE FAKTIN-IO-AREA (19:6)  TO ONR (1:6)
               MOVE FAKTIN-IO-AREA (25:1)  TO RA (1:1)
               MOVE FAKTIN-IO-AREA (41:15) TO KUREF (1:15)
               MOVE FAKTIN-IO-AREA (56:6)  TO FAKREF (1:6)
               MOVE FAKTIN-IO-AREA (56:2)  TO FAKDAG (1:2)
               MOVE FAKTIN-IO-AREA (58:2)  TO FAKMND (1:2)
               MOVE FAKTIN-IO-AREA (60:2)  TO FAKAAR (1:2)
               MOVE FAKTIN-IO-AREA (66:1)  TO BK (1:1)
               MOVE FAKTIN-IO-AREA (67:1)  TO FRITT (1:1)
               MOVE FAKTIN-IO-AREA (77:5)  TO VGR (1:5)
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
               MOVE FAKTIN-IO-AREA (179:1) TO KRETYP (1:1)
               MOVE FAKTIN-IO-AREA (184:6) TO KUNDNR (1:6)
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
               MOVE KUNDEMA-IO-AREA (127:2) TO CBETM (1:2)
               MOVE KUNDEMA-IO-AREA (185:3) TO HDIST (1:3)
               MOVE KUNDEMA-IO-AREA (185:1) TO HDIST1 (1:1)
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-03                        TO TRUE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (1:3)   TO KONFNR (1:3)
               MOVE FIRMAF-IO-AREA (713:4) TO BILNR2-IO
               MOVE FIRMAF-IO-AREA (737:4) TO BILNR8-IO
               MOVE FIRMAF-IO-AREA (589:4) TO STEAVR (1:4)
               MOVE FIRMAF-IO-AREA (837:4) TO KTOAVR (1:4)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-05                        TO TRUE.
 
       KONTFAK-FLDSET SECTION.
       KONTFAK-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KONTFAK-IO-AREA (16:4) TO HBKTO (1:4)
               MOVE KONTFAK-IO-AREA (30:2) TO KASSE (1:2)
               MOVE KONTFAK-IO-AREA (34:5) TO ORDTOT-IO
           END-EVALUATE.
 
       KONTFAK-IDSET SECTION.
       KONTFAK-IDSET-P.
           SET I-09                        TO TRUE.
 
       SYSPARM-FLDSET SECTION.
       SYSPARM-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE SYSPARM-IO-AREA (026:1) TO SYSALT (1:1)
           END-EVALUATE.
 
       SYSPARM-IDSET SECTION.
       SYSPARM-IDSET-P.
           SET I-06                        TO TRUE.
 
       KONTOMA-FLDSET SECTION.
       KONTOMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KONTOMA-IO-AREA (45:4) TO ALTKTO (1:4)
           END-EVALUATE.
 
       KONTOMA-IDSET SECTION.
       KONTOMA-IDSET-P.
           SET I-07                        TO TRUE.
 
       KONTORD-FLDSET SECTION.
       KONTORD-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KONTORD-IO-AREA (28:2) TO KOTIM (1:2)
               MOVE KONTORD-IO-AREA (30:2) TO KOMIN (1:2)
               MOVE KONTORD-IO-AREA (34:2) TO KODAG (1:2)
               MOVE KONTORD-IO-AREA (36:2) TO KOMND (1:2)
               MOVE KONTORD-IO-AREA (38:2) TO KOAAR (1:2)
           END-EVALUATE.
 
       KONTORD-IDSET SECTION.
       KONTORD-IDSET-P.
           SET I-10                        TO TRUE.
 
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
           IF  (I-72)
           OR  (I-64)
               MOVE SPACES TO PRTFILE-IO-AREA
               INITIALIZE PRTFILE-IO-AREA
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (1:3)
               MOVE SEQ-IO                 TO PRTFILE-IO-AREA (4:6)
               MOVE '01'                   TO PRTFILE-IO-AREA (10:2)
               MOVE 'KUNDENR     :'        TO PRTFILE-IO-AREA (15:13)
               MOVE RESKNR                 TO PRTFILE-IO-AREA (29:6)
               IF  (NOT-I-90)
                   MOVE CNAVN1             TO PRTFILE-IO-AREA (51:30)
               END-IF
               IF  (I-90)
                   MOVE '*** KUNDE IKKE I ARKIV *' TO PRTFILE-IO-AREA
                                                               (55:24)
               END-IF
               IF  (I-90)
                   MOVE '**'               TO PRTFILE-IO-AREA (79:2)
               END-IF
               MOVE PRFLT5                 TO PRTFILE-IO-AREA (128:9)
      *                      49         137 "B"
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
               MOVE 'FAKTURADATO :'        TO PRTFILE-IO-AREA (15:13)
               MOVE FAKDAG                 TO PRTFILE-IO-AREA (29:2)
               MOVE '.'                    TO PRTFILE-IO-AREA (31:1)
               MOVE FAKMND                 TO PRTFILE-IO-AREA (32:2)
               MOVE '.'                    TO PRTFILE-IO-AREA (34:1)
               MOVE FAKAAR                 TO PRTFILE-IO-AREA (35:2)
               IF  (NOT-I-90)
                   MOVE CNAVN2             TO PRTFILE-IO-AREA (51:30)
               END-IF
               MOVE PRFLT5                 TO PRTFILE-IO-AREA (128:9)
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
               MOVE 'FORFALLSDATO:'        TO PRTFILE-IO-AREA (15:13)
               MOVE FAKDAG                 TO PRTFILE-IO-AREA (29:2)
               MOVE '.'                    TO PRTFILE-IO-AREA (31:1)
               MOVE FAKMND                 TO PRTFILE-IO-AREA (32:2)
               MOVE '.'                    TO PRTFILE-IO-AREA (34:1)
               MOVE FAKAAR                 TO PRTFILE-IO-AREA (35:2)
               IF  (NOT-I-90)
                   MOVE CADR               TO PRTFILE-IO-AREA (51:30)
               END-IF
               MOVE PRFLT5                 TO PRTFILE-IO-AREA (128:9)
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
                   MOVE CPNR               TO PRTFILE-IO-AREA (51:4)
               END-IF
               IF  (NOT-I-90)
                   MOVE CPOST              TO PRTFILE-IO-AREA (56:15)
               END-IF
               MOVE PRFLT5                 TO PRTFILE-IO-AREA (128:9)
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
               MOVE RESKNR                 TO PRTFILE-IO-AREA (29:6)
               IF  (NOT-I-90)
                   MOVE CNAVN1             TO PRTFILE-IO-AREA (27:30)
               END-IF
               IF  (I-90)
                   MOVE '*** KUNDE IKKE I ARKIV *' TO PRTFILE-IO-AREA
                                                               (31:24)
               END-IF
               IF  (I-90)
                   MOVE '**'               TO PRTFILE-IO-AREA (55:2)
               END-IF
               MOVE 'FAKTURANR: '          TO PRTFILE-IO-AREA (67:11)
               MOVE PRARE1-IO              TO PRTFILE-IO-AREA (78:6)
               MOVE BK                     TO PRTFILE-IO-AREA (101:1)
               MOVE BRKODE-IO              TO PRTFILE-IO-AREA (128:6)
               MOVE FAKRNR                 TO PRTFILE-IO-AREA (134:1)
               MOVE FLSIDE                 TO PRTFILE-IO-AREA (135:1)
               MOVE '2'                    TO PRTFILE-IO-AREA (136:1)
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (144:3)
               WRITE PRTFILE-IO-AREA
           END-IF
           IF  (I-20)
               MOVE SPACES TO PRTFILE-IO-AREA
               INITIALIZE PRTFILE-IO-AREA
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (1:3)
               MOVE SEQ-IO                 TO PRTFILE-IO-AREA (4:6)
               MOVE '08'                   TO PRTFILE-IO-AREA (10:2)
               MOVE ONR                    TO PRTFILE-IO-AREA (15:6)
               MOVE KUNDNR                 TO PRTFILE-IO-AREA (35:6)
               IF  (I-21 AND I-56)
                   MOVE '2 RETUR AV VARER    ' TO PRTFILE-IO-AREA
                                                               (46:20)
               END-IF
               IF  (I-21 AND I-57)
                   MOVE '3 FEIL RABATT/BELØP ' TO PRTFILE-IO-AREA
                                                               (46:20)
               END-IF
               IF  (I-21 AND I-58)
                   MOVE '4 BONUS             ' TO PRTFILE-IO-AREA
                                                               (46:20)
               END-IF
               IF  (I-21 AND I-59)
                   MOVE '5 ERSTATNING        ' TO PRTFILE-IO-AREA
                                                               (46:20)
               END-IF
               MOVE ORDTOT                 TO EDIT-ORDTOT
               MOVE EDIT-ORDTOT            TO PRTFILE-IO-AREA (73:12)
               MOVE BRKODE-IO              TO PRTFILE-IO-AREA (128:6)
               MOVE FAKRNR                 TO PRTFILE-IO-AREA (134:1)
               MOVE FLSIDE                 TO PRTFILE-IO-AREA (135:1)
               MOVE '4'                    TO PRTFILE-IO-AREA (136:1)
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (144:3)
               WRITE PRTFILE-IO-AREA
           END-IF
           IF  (I-41)
               MOVE SPACES TO REGFILE-IO-AREA
               INITIALIZE REGFILE-IO-AREA
               MOVE '01'                   TO REGFILE-IO-AREA (1:2)
               MOVE PRARE1-IO              TO REGFILE-IO-AREA (3:6)
               MOVE '2'                    TO REGFILE-IO-AREA (9:1)
               MOVE FAKDAG                 TO REGFILE-IO-AREA (14:2)
               MOVE FAKMND                 TO REGFILE-IO-AREA (12:2)
               MOVE FAKAAR                 TO REGFILE-IO-AREA (10:2)
               MOVE KUNDNR                 TO REGFILE-IO-AREA (16:6)
               MOVE FIRMNR                 TO REGFILE-IO-AREA (22:3)
               IF  (NOT-I-08)
                   MOVE '6'                TO REGFILE-IO-AREA (30:1)
               END-IF
               IF  (NOT-I-18 AND I-08)
                   MOVE '4'                TO REGFILE-IO-AREA (30:1)
               END-IF
               IF  (I-18 AND I-08)
                   MOVE 'E'                TO REGFILE-IO-AREA (30:1)
               END-IF
               MOVE '5'                    TO REGFILE-IO-AREA (31:1)
               MOVE VGR                    TO REGFILE-IO-AREA (32:5)
               MOVE BXBEL-IO               TO REGFILE-IO-AREA (37:9)
               MOVE SIGNX1                 TO REGFILE-IO-AREA (46:1)
               MOVE SUMSP-IO               TO REGFILE-IO-AREA (47:9)
               INITIALIZE SUMSP-IO
               MOVE FAKDAG                 TO REGFILE-IO-AREA (63:2)
               MOVE FAKMND                 TO REGFILE-IO-AREA (65:2)
               MOVE FAKAAR                 TO REGFILE-IO-AREA (67:2)
               MOVE HDIST                  TO REGFILE-IO-AREA (74:3)
               MOVE 'K'                    TO REGFILE-IO-AREA (77:1)
               MOVE KTOKL                  TO REGFILE-IO-AREA (78:1)
               MOVE BK                     TO REGFILE-IO-AREA (79:1)
               MOVE REGAVD                 TO REGFILE-IO-AREA (80:1)
               WRITE REGFILE-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L5 AND I-69)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FAKTURATOTALER KON'   TO LISTE-IO-AREA (2:18)
               MOVE 'TANTSALG'             TO LISTE-IO-AREA (20:8)
               MOVE 'FREMSTILT'            TO LISTE-IO-AREA (30:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (40:8)
               MOVE 'PROG=FAK485'          TO LISTE-IO-AREA (70:11)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMNRNR'             TO LISTE-IO-AREA (1:8)
               MOVE 'FRA FAKT.NR.'         TO LISTE-IO-AREA (14:12)
               MOVE 'TIL FAKT.NR.'         TO LISTE-IO-AREA (34:12)
               MOVE 'TOT ANT'              TO LISTE-IO-AREA (54:7)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OVERFLOW SECTION.
       HEADING-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FAKTURATOTALER KON'   TO LISTE-IO-AREA (2:18)
               MOVE 'TANTSALG'             TO LISTE-IO-AREA (20:8)
               MOVE 'FREMSTILT'            TO LISTE-IO-AREA (30:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (40:8)
               MOVE 'PROG=FAK485'          TO LISTE-IO-AREA (70:11)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'FIRMNRNR'             TO LISTE-IO-AREA (1:8)
               MOVE 'FRA FAKT.NR.'         TO LISTE-IO-AREA (14:12)
               MOVE 'TIL FAKT.NR.'         TO LISTE-IO-AREA (34:12)
               MOVE 'TOT ANT'              TO LISTE-IO-AREA (54:7)
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
               WRITE ORDFAK-IO-AREA
           END-IF
           IF  (I-42)
               MOVE SPACES TO PRTFILE-IO-AREA
               INITIALIZE PRTFILE-IO-AREA
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (1:3)
               MOVE SEQ-IO                 TO PRTFILE-IO-AREA (4:6)
               MOVE '09'                   TO PRTFILE-IO-AREA (10:2)
               MOVE '------------'         TO PRTFILE-IO-AREA (73:12)
               MOVE PRFLT5                 TO PRTFILE-IO-AREA (128:9)
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (144:3)
               WRITE PRTFILE-IO-AREA
               MOVE SPACES TO PRTFILE-IO-AREA
               INITIALIZE PRTFILE-IO-AREA
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (1:3)
               MOVE SEQ-IO                 TO PRTFILE-IO-AREA (4:6)
               MOVE '10'                   TO PRTFILE-IO-AREA (10:2)
               IF  (I-45)
                   MOVE 'SUM INKL. MVA '   TO PRTFILE-IO-AREA (56:14)
               END-IF
               IF  (NOT-I-45)
                   MOVE 'SUM EKSKL. MVA '  TO PRTFILE-IO-AREA (56:15)
               END-IF
               MOVE ORDTL2                 TO EDIT-ORDTL2
               MOVE EDIT-ORDTL2            TO PRTFILE-IO-AREA (73:12)
               MOVE PRFLT5                 TO PRTFILE-IO-AREA (128:9)
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (144:3)
               WRITE PRTFILE-IO-AREA
           END-IF
           IF  (I-42 AND NOT-I-26)
               MOVE SPACES TO PRTFILE-IO-AREA
               INITIALIZE PRTFILE-IO-AREA
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (1:3)
               MOVE SEQ-IO                 TO PRTFILE-IO-AREA (4:6)
               MOVE '11'                   TO PRTFILE-IO-AREA (10:2)
               MOVE 'AVRUNDINGSBELØP'      TO PRTFILE-IO-AREA (32:15)
               MOVE 'POSTERT PÅ KONTO '    TO PRTFILE-IO-AREA (48:17)
               MOVE KTOAVR                 TO PRTFILE-IO-AREA (66:4)
               IF  (I-82 AND NOT-I-83)
                   MOVE ALTAVR             TO PRTFILE-IO-AREA (66:4)
               END-IF
               MOVE OREDIF                 TO EDIT-OREDIF
               MOVE EDIT-OREDIF            TO PRTFILE-IO-AREA (73:12)
               MOVE PRFLT5                 TO PRTFILE-IO-AREA (128:9)
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (144:3)
               WRITE PRTFILE-IO-AREA
           END-IF
           IF  (I-42)
               MOVE SPACES TO PRTFILE-IO-AREA
               INITIALIZE PRTFILE-IO-AREA
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (1:3)
               MOVE SEQ-IO                 TO PRTFILE-IO-AREA (4:6)
               MOVE '12'                   TO PRTFILE-IO-AREA (10:2)
               IF  (I-94 AND I-45)
                   MOVE 'HERAV  24 PST. MVA ' TO PRTFILE-IO-AREA
                                                               (51:19)
               END-IF
               IF  (NOT-I-94 AND I-45)
                   MOVE 'HERAV  25 PST. MVA ' TO PRTFILE-IO-AREA
                                                               (51:19)
               END-IF
               IF  (I-45)
                   MOVE MOMS               TO EDIT-MOMS
                   MOVE EDIT-MOMS          TO PRTFILE-IO-AREA (73:12)
               END-IF
               MOVE PRFLT5                 TO PRTFILE-IO-AREA (128:9)
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (144:3)
               WRITE PRTFILE-IO-AREA
           END-IF
           IF  (I-51)
               MOVE SPACES TO PRTFILE-IO-AREA
               INITIALIZE PRTFILE-IO-AREA
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (1:3)
               MOVE SEQ-IO                 TO PRTFILE-IO-AREA (4:6)
               MOVE '13'                   TO PRTFILE-IO-AREA (10:2)
               MOVE 'OVERFØRT TIL H.BOK.KTO.' TO PRTFILE-IO-AREA
                                                               (21:23)
               MOVE HBKTO                  TO PRTFILE-IO-AREA (45:4)
               IF  (I-82 AND NOT-I-85)
                   MOVE ALTHB              TO PRTFILE-IO-AREA (45:4)
               END-IF
               MOVE 'MED BILAGSNR.'        TO PRTFILE-IO-AREA (50:13)
               MOVE BILN8X-IO              TO PRTFILE-IO-AREA (64:6)
               MOVE PRFLT5                 TO PRTFILE-IO-AREA (128:9)
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (144:3)
               WRITE PRTFILE-IO-AREA
           END-IF
           IF  (I-42)
               MOVE SPACES TO PRTFILE-IO-AREA
               INITIALIZE PRTFILE-IO-AREA
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (1:3)
               MOVE SEQ-IO                 TO PRTFILE-IO-AREA (4:6)
               MOVE '14'                   TO PRTFILE-IO-AREA (10:2)
               MOVE PRFLT5                 TO PRTFILE-IO-AREA (128:9)
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (144:3)
               WRITE PRTFILE-IO-AREA
           END-IF
           IF  (I-42 AND I-24)
               MOVE SPACES TO PRTFILE-IO-AREA
               INITIALIZE PRTFILE-IO-AREA
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (1:3)
               MOVE SEQ-IO                 TO PRTFILE-IO-AREA (4:6)
               MOVE '15'                   TO PRTFILE-IO-AREA (10:2)
               MOVE 'ANTALL KREDITNOTAER:' TO PRTFILE-IO-AREA (15:20)
               MOVE KREANT                 TO EDIT-KREANT
               MOVE EDIT-KREANT            TO PRTFILE-IO-AREA (36:2)
               MOVE 'TOTALBELØP KREDITNOTAER:' TO PRTFILE-IO-AREA
                                                               (47:24)
               MOVE KRETOT                 TO EDIT-KRETOT
               MOVE EDIT-KRETOT            TO PRTFILE-IO-AREA (73:12)
               MOVE PRFLT5                 TO PRTFILE-IO-AREA (128:9)
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (144:3)
               WRITE PRTFILE-IO-AREA
           END-IF
           IF  (I-42 AND NOT-I-86)
               MOVE SPACES TO PRTFILE-IO-AREA
               INITIALIZE PRTFILE-IO-AREA
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (1:3)
               MOVE SEQ-IO                 TO PRTFILE-IO-AREA (4:6)
               MOVE '16'                   TO PRTFILE-IO-AREA (10:2)
               MOVE 'KASSEOPPGJØR KJØRT :' TO PRTFILE-IO-AREA (15:20)
               MOVE KODAG                  TO PRTFILE-IO-AREA (36:2)
               MOVE '.'                    TO PRTFILE-IO-AREA (38:1)
               MOVE KOMND                  TO PRTFILE-IO-AREA (39:2)
               MOVE '.20'                  TO PRTFILE-IO-AREA (41:3)
               MOVE KOAAR                  TO PRTFILE-IO-AREA (44:2)
               MOVE KOTIM                  TO PRTFILE-IO-AREA (47:2)
               MOVE ':'                    TO PRTFILE-IO-AREA (49:1)
               MOVE KOMIN                  TO PRTFILE-IO-AREA (50:2)
               MOVE PRFLT5                 TO PRTFILE-IO-AREA (128:9)
               MOVE FIRMNR                 TO PRTFILE-IO-AREA (144:3)
      *****************************************************************
      * REGNSKAPSPOSTERING AVRUNDING.                                 *
      *****************************************************************
               WRITE PRTFILE-IO-AREA
           END-IF
           IF  (I-L2 AND NOT-I-26)
               MOVE SPACES TO REGFILE-IO-AREA
               INITIALIZE REGFILE-IO-AREA
               MOVE '01'                   TO REGFILE-IO-AREA (1:2)
               MOVE AREA9X-IO              TO REGFILE-IO-AREA (3:6)
               MOVE '2'                    TO REGFILE-IO-AREA (9:1)
               MOVE FAKDAG                 TO REGFILE-IO-AREA (14:2)
               MOVE FAKMND                 TO REGFILE-IO-AREA (12:2)
               MOVE FAKAAR                 TO REGFILE-IO-AREA (10:2)
               MOVE RESKNR                 TO REGFILE-IO-AREA (16:6)
               MOVE FIRMNR                 TO REGFILE-IO-AREA (22:3)
               MOVE ' '                    TO REGFILE-IO-AREA (30:1)
               MOVE ' '                    TO REGFILE-IO-AREA (31:1)
               MOVE KTOAVR                 TO REGFILE-IO-AREA (32:4)
               MOVE ' '                    TO REGFILE-IO-AREA (36:1)
               MOVE OREDF1-IO              TO REGFILE-IO-AREA (37:9)
               MOVE SIGNXO                 TO REGFILE-IO-AREA (46:1)
               MOVE FAKDAG                 TO REGFILE-IO-AREA (63:2)
               MOVE FAKMND                 TO REGFILE-IO-AREA (65:2)
               MOVE FAKAAR                 TO REGFILE-IO-AREA (67:2)
               MOVE HDIST                  TO REGFILE-IO-AREA (74:3)
               MOVE 'K'                    TO REGFILE-IO-AREA (77:1)
               MOVE KTOKL                  TO REGFILE-IO-AREA (78:1)
               MOVE BK                     TO REGFILE-IO-AREA (79:1)
               MOVE REGAVD                 TO REGFILE-IO-AREA (80:1)
               WRITE REGFILE-IO-AREA
           END-IF
           IF  (I-45)
               MOVE SPACES TO REGFILE-IO-AREA
               INITIALIZE REGFILE-IO-AREA
               MOVE '01'                   TO REGFILE-IO-AREA (1:2)
               MOVE AREA9X-IO              TO REGFILE-IO-AREA (3:6)
               MOVE '2'                    TO REGFILE-IO-AREA (9:1)
               MOVE FAKDAG                 TO REGFILE-IO-AREA (14:2)
               MOVE FAKMND                 TO REGFILE-IO-AREA (12:2)
               MOVE FAKAAR                 TO REGFILE-IO-AREA (10:2)
               MOVE RESKNR                 TO REGFILE-IO-AREA (16:6)
               MOVE FIRMNR                 TO REGFILE-IO-AREA (22:3)
               MOVE '  '                   TO REGFILE-IO-AREA (30:2)
               MOVE '2201 '                TO REGFILE-IO-AREA (32:5)
               MOVE MOMSX1-IO              TO REGFILE-IO-AREA (37:9)
               MOVE SIGNXM                 TO REGFILE-IO-AREA (46:1)
               MOVE FAKDAG                 TO REGFILE-IO-AREA (63:2)
               MOVE FAKMND                 TO REGFILE-IO-AREA (65:2)
               MOVE FAKAAR                 TO REGFILE-IO-AREA (67:2)
               MOVE HDIST                  TO REGFILE-IO-AREA (74:3)
               MOVE 'K'                    TO REGFILE-IO-AREA (77:1)
               MOVE KTOKL                  TO REGFILE-IO-AREA (78:1)
               MOVE BK                     TO REGFILE-IO-AREA (79:1)
               MOVE REGAVD                 TO REGFILE-IO-AREA (80:1)
      *****************************************************************
      * REGNSKAPSPOSTERING FOR OVERFØRING TIL KASSEKONTO.             *
      *****************************************************************
               WRITE REGFILE-IO-AREA
           END-IF
           IF  (I-51)
               MOVE SPACES TO REGFILE-IO-AREA
               INITIALIZE REGFILE-IO-AREA
               MOVE '01'                   TO REGFILE-IO-AREA (1:2)
               MOVE BILN8X-IO              TO REGFILE-IO-AREA (3:6)
               MOVE '8'                    TO REGFILE-IO-AREA (9:1)
               MOVE FAKDAG                 TO REGFILE-IO-AREA (14:2)
               MOVE FAKMND                 TO REGFILE-IO-AREA (12:2)
               MOVE FAKAAR                 TO REGFILE-IO-AREA (10:2)
               MOVE HBKTO                  TO REGFILE-IO-AREA (16:4)
               MOVE FIRMNR                 TO REGFILE-IO-AREA (22:3)
               MOVE '  '                   TO REGFILE-IO-AREA (30:2)
               MOVE '1161 '                TO REGFILE-IO-AREA (32:5)
               MOVE KASBEL-IO              TO REGFILE-IO-AREA (37:9)
               MOVE SIGNXH                 TO REGFILE-IO-AREA (46:1)
               MOVE FAKDAG                 TO REGFILE-IO-AREA (63:2)
               MOVE FAKMND                 TO REGFILE-IO-AREA (65:2)
               MOVE FAKAAR                 TO REGFILE-IO-AREA (67:2)
               MOVE HDIST                  TO REGFILE-IO-AREA (74:3)
               MOVE KTOKL                  TO REGFILE-IO-AREA (78:1)
               MOVE BK                     TO REGFILE-IO-AREA (79:1)
               MOVE REGAVD                 TO REGFILE-IO-AREA (80:1)
               WRITE REGFILE-IO-AREA
           END-IF
           IF  (I-50)
               MOVE SPACES TO RESFILE-IO-AREA
               INITIALIZE RESFILE-IO-AREA
               MOVE '02'                   TO RESFILE-IO-AREA (1:2)
               MOVE '21'                   TO RESFILE-IO-AREA (3:2)
               MOVE RESKNR                 TO RESFILE-IO-AREA (5:6)
               MOVE FAKDAG                 TO RESFILE-IO-AREA (15:2)
               MOVE FAKMND                 TO RESFILE-IO-AREA (13:2)
               MOVE FAKAAR                 TO RESFILE-IO-AREA (11:2)
               MOVE AREA9X-IO              TO RESFILE-IO-AREA (17:6)
               MOVE AREA9X-IO              TO RESFILE-IO-AREA (23:6)
               MOVE FAKAAR                 TO RESFILE-IO-AREA (29:2)
               MOVE FAKMND                 TO RESFILE-IO-AREA (31:2)
               MOVE FAKDAG                 TO RESFILE-IO-AREA (33:2)
               MOVE ORDTL2-IO              TO RESFILE-IO-AREA (35:9)
               MOVE FIRMNR                 TO RESFILE-IO-AREA (44:3)
               MOVE '2'                    TO RESFILE-IO-AREA (47:1)
               MOVE BTMA-ELGTE             TO RESFILE-IO-AREA (48:2)
               IF  (I-88)
                   MOVE '1'                TO RESFILE-IO-AREA (50:1)
               END-IF
               IF  (NOT-I-88)
                   MOVE '0'                TO RESFILE-IO-AREA (50:1)
               END-IF
               MOVE ' '                    TO RESFILE-IO-AREA (69:1)
               MOVE ' '                    TO RESFILE-IO-AREA (70:1)
               WRITE RESFILE-IO-AREA
           END-IF
           IF  (I-L5)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FIRMNR                 TO LISTE-IO-AREA (6:3)
               MOVE AREA1F-IO              TO LISTE-IO-AREA (20:6)
               MOVE BILNR2                 TO XO-70U
               MOVE XO-70U (1:7)           TO LISTE-IO-AREA (39:7)
               MOVE ANTL2                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (55:6)
               INITIALIZE ANTL2
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTALT'               TO LISTE-IO-AREA (3:6)
               MOVE ANTLR                  TO XO-50YY9
               MOVE XO-50YY9               TO LISTE-IO-AREA (55:6)
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
               MOVE 'AVSTEMMING KONTANT.FAKT.' TO LISTE-IO-AREA (3:24)
               MOVE 'I PROGRAM FAK485.'    TO LISTE-IO-AREA (28:17)
               MOVE 'FAKTURADATO'          TO LISTE-IO-AREA (53:11)
               MOVE FAKDAG                 TO LISTE-IO-AREA (67:2)
               MOVE '.'                    TO LISTE-IO-AREA (69:1)
               MOVE FAKMND                 TO LISTE-IO-AREA (70:2)
               MOVE '.'                    TO LISTE-IO-AREA (72:1)
               MOVE FAKAAR                 TO LISTE-IO-AREA (73:2)
               MOVE 'KJØREDATO'            TO LISTE-IO-AREA (77:9)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (88:8)
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTAL FAKTURABELØP     ' TO LISTE-IO-AREA (25:23)
               MOVE TOTSUM                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (50:14)
               MOVE 'MOMSBELØP'            TO LISTE-IO-AREA (65:9)
               MOVE TOTMOM                 TO XO-72YY9R
               MOVE XO-72YY9R              TO LISTE-IO-AREA (75:13)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTAL REGNSKAPSBELØP   ' TO LISTE-IO-AREA (25:23)
               MOVE TOTREG                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (50:14)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'TOTAL RESKONTROBELØP   ' TO LISTE-IO-AREA (25:23)
               MOVE TOTRES                 TO XO-82YY9R
               MOVE XO-82YY9R              TO LISTE-IO-AREA (50:14)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL FAKTURA'       TO LISTE-IO-AREA (3:14)
               MOVE TOTAFA                 TO XO-60YY9
               MOVE XO-60YY9               TO LISTE-IO-AREA (18:7)
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
           IF  (I-LR AND I-03 AND I-05)
           AND (I-06 AND I-07 AND I-09)
           AND (I-92)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE PSDS                   TO LISTE-IO-AREA (41:80)
               MOVE R                      TO LISTE-IO-AREA (113:8)
               MOVE P-IO                   TO LISTE-IO-AREA (118:3)
               MOVE S-IO                   TO LISTE-IO-AREA (116:5)
               MOVE BUMVA-IO               TO LISTE-IO-AREA (110:11)
               MOVE BMMVA-IO               TO LISTE-IO-AREA (110:11)
               MOVE KUREF                  TO LISTE-IO-AREA (106:15)
               MOVE FAKREF                 TO LISTE-IO-AREA (115:6)
               MOVE STEAVR                 TO LISTE-IO-AREA (117:4)
               MOVE KOFIRM                 TO LISTE-IO-AREA (118:3)
               MOVE KOSUMN                 TO XO-130U
               MOVE XO-130U (1:13)         TO LISTE-IO-AREA (108:13)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L5 AND I-40 AND NOT-I-U1)
               MOVE BILNR2                 TO XO-70P
               MOVE XO-70P-EF              TO FIRMAF-IO-AREA (713:4)
               MOVE BILNR8                 TO XO-70P
               MOVE XO-70P-EF              TO FIRMAF-IO-AREA (737:4)
               REWRITE FIRMAF-IO-AREA
                   INVALID KEY
                       DISPLAY 'Bad REWRITE - file = FIRMAF'
               END-REWRITE
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
           SET FAKTIN-LEVEL-INIT           TO TRUE
           INITIALIZE FAKTIN-DATA-FIELDS
           SET FAKTIN-EOF-OFF              TO TRUE
           SET FAKTIN-PROCESS              TO TRUE
           OPEN INPUT FAKTIN
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN I-O FIRMAF
           INITIALIZE KONTFAK-DATA-FIELDS
           OPEN INPUT KONTFAK
           INITIALIZE SYSPARM-DATA-FIELDS
           OPEN INPUT SYSPARM
           INITIALIZE KONTOMA-DATA-FIELDS
           OPEN INPUT KONTOMA
           INITIALIZE KONTORD-DATA-FIELDS
           OPEN INPUT KONTORD
           OPEN OUTPUT ORDFAK
           OPEN OUTPUT RESFILE
           OPEN OUTPUT REGFILE
           OPEN OUTPUT PRTFILE
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FAKTIN
           CLOSE KUNDEMA
           CLOSE FIRMAF
           CLOSE KONTFAK
           CLOSE SYSPARM
           CLOSE KONTOMA
           CLOSE KONTORD
           CLOSE ORDFAK
           CLOSE RESFILE
           CLOSE REGFILE
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
