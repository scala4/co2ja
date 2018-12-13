       IDENTIFICATION DIVISION.
       PROGRAM-ID. RKO005R.
      **********************************************  Z-WIN-RPG2   ****
      *  MERGER SAMMEN DAGENS REGISTRERTE REGNSKAPSTRANSER (4-8),     *
      *  INNBET. FRA BBS, INNBET. FRA POSTGIRO OG DAGENS EDI-UTBET.   *
      *  FRA TELEBANK OG POSTGIRO.                                    *
      *  ENDRER BANK-KTO FOR BBS-INNBETALING.                         *
      *  SPLITTER OUTPUT REGNSKAPSRECORDS I 2 DELER:                  *
      *  1. DE POSTER SOM SKAL HENTE DATA FRA RELATERINGSFILE.        *
      *  2. DE POSTER SOM IKKE SKAL HENTE FRA RELATERINGSFILE.        *
      *  PROGRAM....: RSK005                                          *
      *  PROGRAMERER: ESPEN LARSEN                                    *
      *  PROGRAMERT.:  ???????                                        *
      *  RETTET.....: 04.11.93 LAGT INN EDI-UTBET RECORDS.            *
      *               04.10.95 HELLANOR BODØ (932) SKAL BENYTTE MOT-  *
      *                        KONTO 2000 FOR OCR-INNBET (FØR 2010).  *
      *               12.02.96 AUTOUTSTYR NORGE SKAL BENYTTE MOTKONTO *
      *                        KONTO 1070 FOR OCR-INNBET (FØR 2000).  *
      *               25.04.96 FLYTTER TEKST-SEKVENSNR TIL OUTPUT FOR *
      *                        Å KNYTTE TEKST TIL MOTKONTO I BOKF.    *
      *               04.06.96 TAKLER 7-SIFRET KID FOR BBS            *
      *               10.06.96 TAKLER 7-SIFRET KID FOR POSTEN         *
      *               27.06.96 SØRENSEN HADDE FORSKJELLIGE HBOKSKTO   *
      *                        BANK, FOR FORSKJELLIGE TRANSER.        *
      *               31.07.96 RESKNR BLE IKKE LAGT UT FOR FAKTURA-   *
      *                        KID OCR-POST.                          *
      *               16.09.96 KONTO 2181 FOR BANK OG 1070 FOR POST   *
      *                        LEGGES INN FOR TEM (738).              *
      *               17.09.96 KONTO 1060 FOR BANK AUTO PARTS KOKSTAD *
      *               09.01.97 KONTO 2180 FOR BANK GED.               *
      *               12.02.97 KONTO 1060 FOR BANK NORSK BILDEKATALOG.*
      *               19.02.97 KONTO 2155 FOR BANK SCANGROSS(FØR 1055)*
      *               19.03.97 KONTO 1070 FOR BANK MJØSBIL.           *
      *               02.09.97 TATT UT VEKSEL/AKSEPT-NR OG MASKIN-    *
      *                        ORDRENR.                               *
      *               04.12.97 KONTO 1055 FOR BANK GRØNLAND AUTO.     *
      *               14.04.98 SAMME KONTO FOR 971 SOM FOR 970.       *
      *               15.04.98 KONTO 1030 FOR 971.                    *
      *               18.06.98 TATT UT SJEKK PÅ FIRMANR FOR Å FINNE   *
      *                        HOVEDBOKSKONTO FOR BANK OG POST. NÅ    *
      *                        HENTES KONTONR FRA SYSPARM (PROG RG02).*
      *                        HVIS SYSPARM MANGLER ELLER ER FEIL, VIL*
      *                        HHV KONTO 2000 OG 1020 BLI BRUKT FOR   *
      *                        BANK/POST.                             *
      *               04.02.99 LEGGER OCR BBS REF TIL KONTOUTSKRIFT I *
      *                        TEKSTFELTET.                           *
      *               04.02.99 JUSTERT BBS-REF.                       *
      *               06.01.00 SENDER OCR TRANS VIDERE SELV OM DEN ER *
      *                        FEIL, SLIK AT DEN KOMMER PÅ KONTROLL/  *
      *                        FEILLISTE.                             *
      *               26.06.00 JUSTERT BBS-REF I HHT AUTOGIRO.        *
      *               28.06.00 JUSTERT BBS-REF I HHT AUTOGIRO.        *
      *               12.07.00 TATT UT POST OCR.                      *
      *               29.08.00 TATT MED 4-SIFRET AVD.                 *
      *               28.12.01 LEGGER 0 I 1. SIFFER I KID HVIS DET ER *
      *                        BETALINGSSPES/AUTOGIRO FRA NY FAKT.RUT.*
      *               24.01.02 LEGGER REFNR FRA BBS I KUNDENR HVIS    *
      *                        KUNDENR ER BLANKT OG KUNDENR BRUKES    *
      *                        SOM REF.NR I BBS.                      *
      *               15.10.02 REC.-ART. 31 FRA BBS LESES OVER.       *
      * E 14.09.05 TAKLER NEGATIVE BELØP FRA BBS                      *
      * E 03.10.05 UTVIDET RESKTRA FRA 100 TIL 240                    *
      * E 25.03.09 TAKLER PREFIKS 962/963 FRA NY PURRERUTINE.         *
      * E 14.10.10 BRUKER HOVEDBOKSKONTO RELATERT TIL BANKKONTO HVIS  *
      * E          BANKKONTO I BBS MATCHER SYSPARM                    *
      * E 15.04.11 TAR MED NYTT UTVIDET BELØPSFELT                    *
      *MT 20.04.12 SKRIVER LR-LINJE PÅ MAIL BARE NÅR RECORDS ER LEST. *
      *            SENDER IKKE KOPI AV MAIL TIL MT.                   *
      *MT 24.04.12 HADDE STOKKET OM DATO OG KLOKKE PÅ BBS-TRANSER.    *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RKO005.rpg
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
           SELECT RESKTRA
               ASSIGN TO UT-S-RESKTRA
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RESKTRA-STATUS.
           SELECT KORTINN
               ASSIGN TO UT-S-KORTINN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KORTINN-STATUS.
           SELECT UTBREC
               ASSIGN TO UT-S-UTBREC
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS UTBREC-STATUS.
           SELECT RELMAST
               ASSIGN TO RELMAST
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS RELMAST-STATUS
               RECORD KEY IS RELMAST-KEY1.
           SELECT SYSPARM
               ASSIGN TO SYSPARM
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS SYSPARM-STATUS
               RECORD KEY IS SYSPARM-KEY1.
           SELECT TILRELF
               ASSIGN TO UT-S-TILRELF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TILRELF-STATUS.
           SELECT UTNRELF
               ASSIGN TO UT-S-UTNRELF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS UTNRELF-STATUS.
           SELECT BBSBEL
               ASSIGN TO UT-S-BBSBEL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS BBSBEL-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD RESKTRA
               BLOCK CONTAINS 2400
               RECORD CONTAINS 240.
       01  RESKTRA-IO-AREA.
           05  RESKTRA-IO-AREA-X           PICTURE X(240).
       FD KORTINN
               BLOCK CONTAINS 800
               RECORD CONTAINS 80.
       01  KORTINN-IO-AREA.
           05  KORTINN-IO-AREA-X           PICTURE X(80).
       FD UTBREC
               BLOCK CONTAINS 2000
               RECORD CONTAINS 500.
       01  UTBREC-IO-AREA.
           05  UTBREC-IO-AREA-X            PICTURE X(500).
       FD RELMAST
               RECORD CONTAINS 80.
       01  RELMAST-IO-AREA.
           05  RELMAST-IO-AREA-X.
               10  RELMAST-KEY1            PICTURE X(20).
               10  FILLER                  PICTURE X(60).
       FD SYSPARM
               RECORD CONTAINS 160.
       01  SYSPARM-IO-AREA.
           05  SYSPARM-IO-AREA-X.
               10  SYSPARM-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(150).
       FD TILRELF
               BLOCK CONTAINS 240
               RECORD CONTAINS 240.
       01  TILRELF-IO-AREA.
           05  TILRELF-IO-AREA-X           PICTURE X(240).
       FD UTNRELF
               BLOCK CONTAINS 240
               RECORD CONTAINS 240.
       01  UTNRELF-IO-AREA.
           05  UTNRELF-IO-AREA-X           PICTURE X(240).
      *BUGFILO O   F  80  80            PRINTERSYSLST
       FD BBSBEL
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  BBSBEL-IO-AREA.
           05  BBSBEL-IO-AREA-X            PICTURE X(80).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  RESKTRA-STATUS              PICTURE 99 VALUE 0.
           10  KORTINN-STATUS              PICTURE 99 VALUE 0.
           10  UTBREC-STATUS               PICTURE 99 VALUE 0.
           10  RELMAST-STATUS              PICTURE 99 VALUE 0.
           10  SYSPARM-STATUS              PICTURE 99 VALUE 0.
           10  TILRELF-STATUS              PICTURE 99 VALUE 0.
           10  UTNRELF-STATUS              PICTURE 99 VALUE 0.
           10  BBSBEL-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKTRA-EOF-OFF         VALUE '0'.
               88  RESKTRA-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKTRA-READ-OFF        VALUE '0'.
               88  RESKTRA-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKTRA-PROCESS-OFF     VALUE '0'.
               88  RESKTRA-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KORTINN-EOF-OFF         VALUE '0'.
               88  KORTINN-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KORTINN-READ-OFF        VALUE '0'.
               88  KORTINN-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KORTINN-PROCESS-OFF     VALUE '0'.
               88  KORTINN-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  KORTINN-LEVEL-INIT-OFF  VALUE '0'.
               88  KORTINN-LEVEL-INIT      VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  UTBREC-EOF-OFF          VALUE '0'.
               88  UTBREC-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  UTBREC-READ-OFF         VALUE '0'.
               88  UTBREC-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  UTBREC-PROCESS-OFF      VALUE '0'.
               88  UTBREC-PROCESS          VALUE '1'.
           05  RELMAST-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  SYSPARM-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  RESKTRA-DATA-FIELDS.
               10  FKOD                    PICTURE X(3).
               10  BLGART                  PICTURE X(1).
               10  BLGNR                   PICTURE X(6).
               10  BLGDTO                  PICTURE X(6).
               10  SIGN-X                  PICTURE X(2).
      *                                     232 236 PRDDHF
      *                                     237 240 PRDKHF
               10  KTORES                  PICTURE X(8).
               10  RSKNR1                  PICTURE X(1).
               10  BEL-IO.
                   15  BEL                 PICTURE S9(7)V9(2).
               10  DK2                     PICTURE X(1).
               10  MOTKTO                  PICTURE X(4).
               10  REFNR                   PICTURE X(6).
               10  REF3F                   PICTURE X(3).
               10  AVD4                    PICTURE X(4).
               10  FORFDT                  PICTURE X(6).
               10  AK                      PICTURE X(1).
               10  SNR                     PICTURE X(3).
               10  DV                      PICTURE X(1).
               10  VALUTA-IO.
                   15  VALUTA              PICTURE S9(8)V9(2).
               10  RDG                     PICTURE X(3).
               10  TK                      PICTURE X(2).
               10  TXTSEQ                  PICTURE X(1).
               10  TEKST                   PICTURE X(24).
               10  BELDUI-IO.
                   15  BELDUI              PICTURE S9(11)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  VALDUI-IO.
                   15  VALDUI              PICTURE S9(11)V9(4) USAGE
                                                       PACKED-DECIMAL.
               10  VTDU                    PICTURE X(3).
               10  OPPHAV                  PICTURE X(4).
               10  PRDDDF                  PICTURE X(5).
               10  PRDKDF                  PICTURE X(4).
           05  KORTINN-LEVEL-02.
               10  KORTINN-02-L2.
                   15  KORTINN-02-L2-FIRM  PICTURE X(3).
               10  KORTINN-02-L1.
                   15  KORTINN-02-L1-BBSKTO PICTURE X(11).
           05  KORTINN-DATA-FIELDS.
               10  FORTJE                  PICTURE X(4).
               10  FIRM                    PICTURE X(3).
               10  BBSKTO                  PICTURE X(11).
               10  RTF1                    PICTURE X(5).
               10  RTF2                    PICTURE X(6).
               10  FORTEG                  PICTURE X(1).
               10  BILNR                   PICTURE X(6).
               10  KNR                     PICTURE X(6).
               10  INKNR                   PICTURE X(5).
               10  INK3SI                  PICTURE X(3).
               10  RI                      PICTURE X(2).
               10  FKKIDB                  PICTURE X(1).
               10  FKREFB                  PICTURE X(6).
               10  FKREF1                  PICTURE X(1).
               10  RABREN-IO.
                   15  RABREN              PICTURE S9(4)V9(2).
               10  TEGN                    PICTURE X(1).
               10  INBBEL-IO.
                   15  INBBEL              PICTURE S9(7)V9(2).
               10  BILDAT                  PICTURE X(6).
               10  BBSRF1                  PICTURE X(10).
               10  BBSRF2                  PICTURE X(11).
               10  BBSSUM-IO.
                   15  BBSSUM              PICTURE S9(7)V9(2).
           05  UTBREC-DATA-FIELDS.
               10  UBRREC                  PICTURE X(120).
               10  UBBEL-IO.
                   15  UBBEL               PICTURE S9(11)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  UBVAL-IO.
                   15  UBVAL               PICTURE S9(11)V9(4) USAGE
                                                       PACKED-DECIMAL.
               10  UBVT                    PICTURE X(3).
               10  UBOPPH                  PICTURE X(4).
               10  UBPRDT                  PICTURE X(5).
               10  UBPRKL                  PICTURE X(4).
           05  RELMAST-DATA-FIELDS.
               10  RELKNR                  PICTURE X(6).
           05  SYSPARM-DATA-FIELDS.
               10  SYSHB1                  PICTURE X(4).
               10  SYSHB2                  PICTURE X(4).
               10  SYSBKT                  PICTURE X(11).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(11).
           05  TEMPORARY-FIELDS.
               10  ANTREC-IO.
                   15  ANTREC              PICTURE S9(9).
               10  F6-IO.
                   15  F6                  PICTURE S9(6).
               10  DD-IO.
                   15  DD                  PICTURE S9(2).
               10  AA-IO.
                   15  AA                  PICTURE S9(2).
               10  DDATO-IO.
                   15  DDATO               PICTURE S9(6).
               10  DDATO8-IO.
                   15  DDATO8              PICTURE S9(8).
               10  TIDSP-IO.
                   15  TIDSP               PICTURE S9(6).
               10  BEL154-IO.
                   15  BEL154              PICTURE S9(11)V9(4).
               10  SYSKEY                  PICTURE X(10).
               10  OCRKT1                  PICTURE X(4).
               10  OCRKT2                  PICTURE X(4).
               10  INBELW-IO.
                   15  INBELW              PICTURE S9(7)V9(2).
               10  TOTBEL-IO.
                   15  TOTBEL              PICTURE S9(9)V9(2).
               10  RENTEU-IO.
                   15  RENTEU              PICTURE S9(7)V9(2).
               10  UTNREN-IO.
                   15  UTNREN              PICTURE S9(7)V9(2).
               10  REN132-IO.
                   15  REN132              PICTURE S9(11)V9(2).
               10  UTR132-IO.
                   15  UTR132              PICTURE S9(11)V9(2).
               10  INBEL2-IO.
                   15  INBEL2              PICTURE S9(7)V9(2).
               10  INB132-IO.
                   15  INB132              PICTURE S9(11)V9(2).
               10  RELKY4                  PICTURE X(4).
               10  RELKEY                  PICTURE X(20).
               10  RELK16                  PICTURE X(16).
               10  SUMBEL-IO.
                   15  SUMBEL              PICTURE S9(9)V9(2).
               10  BELDUO-IO.
                   15  BELDUO              PICTURE S9(11)V9(2).
               10  VALDUO-IO.
                   15  VALDUO              PICTURE S9(11)V9(4).
           05  EDITTING-FIELDS.
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
               10  XO-72YY9R               PICTURE Z.ZZZ.ZZZ,99-.
               10  XO-92YY9R               PICTURE ZZZ.ZZZ.ZZZ,99-.
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
           SET NOT-I-05                    TO TRUE
           SET NOT-I-06                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-04                    TO TRUE
           SET NOT-I-08                    TO TRUE
           SET NOT-I-01                    TO TRUE
           SET NOT-I-09                    TO TRUE
           SET NOT-I-10                    TO TRUE
           SET NOT-I-07                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  RESKTRA-PROCESS
               SET RESKTRA-PROCESS-OFF     TO TRUE
               SET RESKTRA-READ            TO TRUE
           END-IF
 
           IF  RESKTRA-READ
           AND RECORD-SELECTED-OFF
               PERFORM RESKTRA-GET
               SET RESKTRA-READ-OFF        TO TRUE
               IF  NOT RESKTRA-EOF
                   PERFORM RESKTRA-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET RESKTRA-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  KORTINN-PROCESS
               SET KORTINN-PROCESS-OFF     TO TRUE
               SET KORTINN-READ            TO TRUE
           END-IF
 
           IF  KORTINN-READ
           AND RECORD-SELECTED-OFF
               PERFORM KORTINN-GET
               SET KORTINN-READ-OFF        TO TRUE
               IF  NOT KORTINN-EOF
                   SET KORTINN-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  UTBREC-PROCESS
               SET UTBREC-PROCESS-OFF      TO TRUE
               SET UTBREC-READ             TO TRUE
           END-IF
 
           IF  UTBREC-READ
           AND RECORD-SELECTED-OFF
               PERFORM UTBREC-GET
               SET UTBREC-READ-OFF         TO TRUE
               IF  NOT UTBREC-EOF
                   PERFORM UTBREC-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET UTBREC-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  RESKTRA-PROCESS
               PERFORM RESKTRA-IDSET
           END-IF
 
           IF  KORTINN-PROCESS
               PERFORM KORTINN-IDSET
           END-IF
 
           IF  UTBREC-PROCESS
               PERFORM UTBREC-IDSET
           END-IF
 
           IF  KORTINN-PROCESS
               PERFORM KORTINN-CHK-LEVEL
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
 
           IF  RESKTRA-PROCESS
               PERFORM RESKTRA-FLDSET
           END-IF
 
           IF  KORTINN-PROCESS
               PERFORM KORTINN-FLDOFF
               PERFORM KORTINN-FLDSET
           END-IF
 
           IF  UTBREC-PROCESS
               PERFORM UTBREC-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  KORTINN-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           ADD 1                           TO ANTREC
           SET NOT-I-50                    TO TRUE
           IF  ANTREC > 0
               SET I-50                    TO TRUE
           END-IF
           IF  (I-98)
               SET NOT-I-99                TO TRUE
           END-IF
           IF  (NOT-I-98)
               SET I-98                    TO TRUE
               SET I-99                    TO TRUE
           END-IF
           IF  (I-99)
               MOVE UDATE                  TO F6-IO
               MOVE F6 (1:2)               TO DD
               MOVE F6 (5:2)               TO AA-IO
      ** MLLzo
               IF AA < 0
                   MULTIPLY -1 BY AA
               END-IF
               MOVE F6                     TO DDATO-IO
               MOVE AA                     TO DDATO (1:2)
               MOVE DD                     TO DDATO-IO (5:2)
               SET NOT-I-31                TO TRUE
               IF  AA > 80
                   SET I-31                TO TRUE
               END-IF
               MOVE DDATO                  TO DDATO8-IO (3:6)
           END-IF
           IF  (I-99 AND I-31)
               MOVE '19'                   TO DDATO8 (1:2)
           END-IF
           IF  (I-99 AND NOT-I-31)
               MOVE '20'                   TO DDATO8 (1:2)
           END-IF
           IF  (I-99)
               ACCEPT SYSTEM-TIME-X      FROM TIME
               MOVE SYSTEM-TIME            TO TIDSP (1:6)
               MOVE 0                      TO BEL154
           END-IF
           SET NOT-I-11                    TO TRUE
           SET NOT-I-12                    TO TRUE
           SET NOT-I-13                    TO TRUE
           SET NOT-I-15                    TO TRUE
           SET NOT-I-16                    TO TRUE
           SET NOT-I-17                    TO TRUE
           SET NOT-I-18                    TO TRUE
           SET NOT-I-24                    TO TRUE
           SET NOT-I-74                    TO TRUE
           SET NOT-I-25                    TO TRUE
      ***
           IF  (I-01)
               GO TO SLUTT-T
           END-IF
           IF  (I-08)
               GO TO SLUTT-T
           END-IF
           IF  (I-09)
               GO TO SLUTT-T
           END-IF
           IF  (I-L2)
               SET NOT-I-70                TO TRUE
               SET NOT-I-71                TO TRUE
               SET NOT-I-72                TO TRUE
               SET NOT-I-73                TO TRUE
               SET NOT-I-77                TO TRUE
               SET NOT-I-78                TO TRUE
           END-IF
           IF  (I-L2 AND I-02)
               MOVE FIRM                   TO SYSKEY (1:3)
           END-IF
           IF  (I-L2)
               MOVE 'REGA011'              TO SYSKEY (4:7)
               MOVE '2000'                 TO OCRKT1
               MOVE SYSKEY                 TO SYSPARM-KEY1
               READ SYSPARM RECORD KEY IS SYSPARM-KEY1
               INVALID KEY
                   SET I-70                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-70            TO TRUE
                   PERFORM SYSPARM-FLDOFF
                   PERFORM SYSPARM-FLDSET
                   PERFORM SYSPARM-IDSET
               END-READ
           END-IF
           IF  (I-L2 AND NOT-I-70 AND NOT-I-71)
               MOVE SYSHB1                 TO OCRKT1
           END-IF
           IF  (I-L2)
               MOVE '2000'                 TO OCRKT2
               MOVE 'REGA012'              TO SYSKEY (4:7)
               MOVE SYSKEY                 TO SYSPARM-KEY1
               READ SYSPARM RECORD KEY IS SYSPARM-KEY1
               INVALID KEY
                   SET I-70                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-70            TO TRUE
                   PERFORM SYSPARM-FLDOFF
                   PERFORM SYSPARM-FLDSET
                   PERFORM SYSPARM-IDSET
               END-READ
           END-IF
           IF  (I-L2 AND NOT-I-70 AND NOT-I-72)
               AND (NOT-I-73)
               SET I-77                    TO TRUE
           END-IF
           IF  (I-L2 AND I-77)
               MOVE SYSHB2                 TO OCRKT2
      ***
           END-IF
           IF  (I-02)
               SET NOT-I-14                TO TRUE
               IF  FORTJE = 'NY01'
                   SET I-14                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-77)
               SET NOT-I-78                TO TRUE
               IF  BBSKTO = SYSBKT
                   SET I-78                TO TRUE
               END-IF
      *  02                MOVE "BBSKTO  "BUGFLD  8        LEDETXT DEBUG
      *  02      BUGFLD    DEBUGBUGFILO   BBSKTO           VIS FELT/IND
           END-IF
           IF  (I-03)
               SET NOT-I-11                TO TRUE
               IF  RI = '60'
                   SET I-11                TO TRUE
               END-IF
               SET NOT-I-12                TO TRUE
               IF  RI = '61'
                   SET I-12                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-12)
               SET NOT-I-12                TO TRUE
               IF  RI = '62'
                   SET I-12                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-12)
               SET NOT-I-12                TO TRUE
               IF  RI = '63'
                   SET I-12                TO TRUE
               END-IF
      * KID FRA NY FAKTURARUTINE STARTER MED 10000 OG SKAL HA 0 FORAN:
           END-IF
           IF  (I-03 AND I-14 AND NOT-I-11)
               SET I-74                    TO TRUE
           END-IF
           IF  (I-03)
               SET NOT-I-13                TO TRUE
               IF  FKKIDB = 'F'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-13)
               SET NOT-I-24                TO TRUE
               IF  FKREF1 = '0'
                   SET I-24                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-14)
               SET NOT-I-25                TO TRUE
               IF  FORTEG = '-'
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-25)
               MULTIPLY -1 BY INBBEL   GIVING INBELW
           END-IF
           IF  (I-03 AND NOT-I-25)
               MULTIPLY 1 BY INBBEL    GIVING INBELW
           END-IF
           IF  (I-03)
               ADD INBELW                  TO TOTBEL
      *  L2 02             SETOF                         55
      *  L2 02   FIRM      COMP "855"                    55
           END-IF
           IF  (I-03 AND NOT-I-13)
               SET NOT-I-17                TO TRUE
               IF  RABREN NOT = 0,00
                   SET I-17                TO TRUE
               END-IF
               SET NOT-I-18                TO TRUE
               IF  TEGN = '2'
                   SET I-18                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-18 AND NOT-I-13)
               MULTIPLY -1,00 BY RABREN GIVING RENTEU
           END-IF
           IF  (I-03 AND I-18 AND NOT-I-13)
               MULTIPLY 1,00 BY RABREN GIVING RENTEU
           END-IF
           IF  (I-03 AND NOT-I-13)
               ADD RENTEU TO INBELW    GIVING UTNREN
               MULTIPLY -1,00 BY UTNREN GIVING UTNREN
               ADD RENTEU TO ZERO      GIVING REN132
               ADD UTNREN TO ZERO      GIVING UTR132
           END-IF
           IF  (I-03)
               MULTIPLY -1,00 BY INBELW GIVING INBEL2
               ADD INBEL2 TO ZERO      GIVING INB132
           END-IF
           IF  (I-03 AND I-13)
               MOVE 'A'                    TO RELKY4 (1:1)
               MOVE FIRM                   TO RELKY4 (2:3)
               MOVE RELKY4                 TO RELKEY (1:4)
               MOVE FKREFB                 TO RELK16 (1:6)
               MOVE '000'                  TO RELK16 (14:3)
           END-IF
           IF  (I-03 AND I-24)
               MOVE '001'                  TO RELK16 (14:3)
           END-IF
           IF  (I-03 AND I-13)
               MOVE RELK16                 TO RELKEY (5:16)
           END-IF
           IF  (I-03)
               MOVE RELKEY                 TO RELMAST-KEY1
               READ RELMAST RECORD KEY IS RELMAST-KEY1
               INVALID KEY
                   SET I-22                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-22            TO TRUE
                   PERFORM RELMAST-FLDSET
                   PERFORM RELMAST-IDSET
               END-READ
      ***
      *
           END-IF
           IF  (I-04)
               ADD BBSSUM                  TO SUMBEL
           END-IF
           IF  (I-06)
               SET NOT-I-15                TO TRUE
               IF  REF3F = '960'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-06 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  REF3F = '961'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-06 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  REF3F = '962'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-06 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  REF3F = '963'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-06)
               SET NOT-I-17                TO TRUE
               IF  RSKNR1 = '9'
                   SET I-17                TO TRUE
               END-IF
           END-IF
           IF  (I-06 AND NOT-I-17)
               SET NOT-I-17                TO TRUE
               IF  RSKNR1 = '8'
                   SET I-17                TO TRUE
               END-IF
           END-IF
           IF  (I-06 AND I-17)
               SET NOT-I-15                TO TRUE
           END-IF
           IF  (I-06)
               SET NOT-I-16                TO TRUE
               IF  DK2 = 'K'
                   SET I-16                TO TRUE
               END-IF
           END-IF
           IF  (I-06 AND NOT-I-16)
               MULTIPLY 1,00 BY BEL    GIVING BEL
           END-IF
           IF  (I-06 AND I-16)
               MULTIPLY -1,00 BY BEL   GIVING BEL
           END-IF
           IF  (I-06 AND NOT-I-16)
               MULTIPLY 1,00 BY BELDUI GIVING BELDUO
           END-IF
           IF  (I-06 AND I-16)
               MULTIPLY -1,00 BY BELDUI GIVING BELDUO
           END-IF
           IF  (I-06 AND NOT-I-16)
               MULTIPLY 1,00 BY VALDUI GIVING VALDUO
           END-IF
           IF  (I-06 AND I-16)
               MULTIPLY -1,00 BY VALDUI GIVING VALDUO
           END-IF.
 
       SLUTT-T.
           CONTINUE.
 
       RESKTRA-GET SECTION.
       RESKTRA-GET-P.
           IF  RESKTRA-EOF-OFF
               READ RESKTRA
               AT END
                   SET RESKTRA-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RESKTRA-FLDSET SECTION.
       RESKTRA-FLDSET-P.
           EVALUATE TRUE
           WHEN ( RESKTRA-IO-AREA (1:1) = '1' )
               MOVE RESKTRA-IO-AREA (2:3)  TO FKOD (1:3)
               MOVE RESKTRA-IO-AREA (5:1)  TO BLGART (1:1)
               MOVE RESKTRA-IO-AREA (6:6)  TO BLGNR (1:6)
               MOVE RESKTRA-IO-AREA (12:6) TO BLGDTO (1:6)
               MOVE RESKTRA-IO-AREA (79:2) TO SIGN-X (1:2)
           WHEN ( RESKTRA-IO-AREA (1:1) = '2' )
               MOVE RESKTRA-IO-AREA (2:8)  TO KTORES (1:8)
               MOVE RESKTRA-IO-AREA (2:1)  TO RSKNR1 (1:1)
               MOVE RESKTRA-IO-AREA (10:9) TO BEL-IO
               INSPECT BEL-IO REPLACING ALL ' ' BY '0'
               MOVE RESKTRA-IO-AREA (19:1) TO DK2 (1:1)
               MOVE RESKTRA-IO-AREA (20:4) TO MOTKTO (1:4)
               MOVE RESKTRA-IO-AREA (24:6) TO REFNR (1:6)
               MOVE RESKTRA-IO-AREA (24:3) TO REF3F (1:3)
               MOVE RESKTRA-IO-AREA (30:4) TO AVD4 (1:4)
               MOVE RESKTRA-IO-AREA (42:6) TO FORFDT (1:6)
               MOVE RESKTRA-IO-AREA (48:1) TO AK (1:1)
               MOVE RESKTRA-IO-AREA (49:3) TO SNR (1:3)
               MOVE RESKTRA-IO-AREA (52:1) TO DV (1:1)
               MOVE RESKTRA-IO-AREA (53:10) TO VALUTA-IO
               INSPECT VALUTA-IO REPLACING ALL ' ' BY '0'
               MOVE RESKTRA-IO-AREA (69:3) TO RDG (1:3)
               MOVE RESKTRA-IO-AREA (72:2) TO TK (1:2)
               MOVE RESKTRA-IO-AREA (76:1) TO TXTSEQ (1:1)
               MOVE RESKTRA-IO-AREA (77:24) TO TEKST (1:24)
               MOVE RESKTRA-IO-AREA (101:7) TO BELDUI-IO
               MOVE RESKTRA-IO-AREA (108:8) TO VALDUI-IO
               MOVE RESKTRA-IO-AREA (116:3) TO VTDU (1:3)
               MOVE RESKTRA-IO-AREA (228:4) TO OPPHAV (1:4)
               MOVE RESKTRA-IO-AREA (232:5) TO PRDDDF (1:5)
               MOVE RESKTRA-IO-AREA (237:4) TO PRDKDF (1:4)
           END-EVALUATE.
 
       RESKTRA-IDCHK SECTION.
       RESKTRA-IDCHK-P.
           EVALUATE TRUE
           WHEN ( RESKTRA-IO-AREA (1:1) = '1' )
             OR ( RESKTRA-IO-AREA (1:1) = '2' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       RESKTRA-IDSET SECTION.
       RESKTRA-IDSET-P.
           EVALUATE TRUE
           WHEN ( RESKTRA-IO-AREA (1:1) = '1' )
               SET I-05                    TO TRUE
           WHEN ( RESKTRA-IO-AREA (1:1) = '2' )
               SET I-06                    TO TRUE
           END-EVALUATE.
 
       KORTINN-GET SECTION.
       KORTINN-GET-P.
           IF  KORTINN-EOF-OFF
               READ KORTINN
               AT END
                   SET KORTINN-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KORTINN-FLDOFF SECTION.
       KORTINN-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( KORTINN-IO-AREA (7:1) = '3'
            AND   KORTINN-IO-AREA (8:1) = '0' )
               SET NOT-I-76                TO TRUE
               SET NOT-I-75                TO TRUE
           END-EVALUATE.
 
       KORTINN-FLDSET SECTION.
       KORTINN-FLDSET-P.
           EVALUATE TRUE
           WHEN ( KORTINN-IO-AREA (7:1) = '2'
            AND   KORTINN-IO-AREA (8:1) = '0' )
               MOVE KORTINN-IO-AREA (01:4) TO FORTJE (1:4)
               MOVE KORTINN-IO-AREA (57:3) TO FIRM (1:3)
               MOVE KORTINN-IO-AREA (70:11) TO BBSKTO (1:11)
           WHEN ( KORTINN-IO-AREA (7:1) = '3'
            AND   KORTINN-IO-AREA (8:1) = '0' )
               MOVE KORTINN-IO-AREA (22:5) TO RTF1 (1:5)
               IF  RTF1 = SPACES
                   SET I-76                TO TRUE
               END-IF
               MOVE KORTINN-IO-AREA (27:6) TO RTF2 (1:6)
               MOVE KORTINN-IO-AREA (32:1) TO FORTEG (1:1)
               MOVE KORTINN-IO-AREA (50:6) TO BILNR (1:6)
               MOVE KORTINN-IO-AREA (56:6) TO KNR (1:6)
               IF  KNR = SPACES
                   SET I-75                TO TRUE
               END-IF
               MOVE KORTINN-IO-AREA (62:5) TO INKNR (1:5)
               MOVE KORTINN-IO-AREA (64:3) TO INK3SI (1:3)
               MOVE KORTINN-IO-AREA (62:2) TO RI (1:2)
               MOVE KORTINN-IO-AREA (67:1) TO FKKIDB (1:1)
               MOVE KORTINN-IO-AREA (68:6) TO FKREFB (1:6)
               MOVE KORTINN-IO-AREA (68:1) TO FKREF1 (1:1)
               MOVE KORTINN-IO-AREA (67:6) TO RABREN-IO
               INSPECT RABREN-IO REPLACING ALL ' ' BY '0'
               MOVE KORTINN-IO-AREA (73:1) TO TEGN (1:1)
               MOVE KORTINN-IO-AREA (41:9) TO INBBEL-IO
               INSPECT INBBEL-IO REPLACING ALL ' ' BY '0'
               MOVE KORTINN-IO-AREA (16:6) TO BILDAT (1:6)
               MOVE KORTINN-IO-AREA (22:10) TO BBSRF1 (1:10)
               MOVE KORTINN-IO-AREA (22:11) TO BBSRF2 (1:11)
           WHEN ( KORTINN-IO-AREA (7:1) = '8'
            AND   KORTINN-IO-AREA (8:1) = '8' )
               MOVE KORTINN-IO-AREA (33:9) TO BBSSUM-IO
               INSPECT BBSSUM-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       KORTINN-IDSET SECTION.
       KORTINN-IDSET-P.
           EVALUATE TRUE
           WHEN ( KORTINN-IO-AREA (7:1) = '2'
            AND   KORTINN-IO-AREA (8:1) = '0' )
               SET I-02                    TO TRUE
           WHEN ( KORTINN-IO-AREA (7:1) = '3'
            AND   KORTINN-IO-AREA (8:1) = '0' )
               SET I-03                    TO TRUE
           WHEN ( KORTINN-IO-AREA (7:1) = '8'
            AND   KORTINN-IO-AREA (8:1) = '8' )
               SET I-04                    TO TRUE
           WHEN  OTHER
               SET I-08                    TO TRUE
           END-EVALUATE.
 
       KORTINN-CHK-LEVEL SECTION.
       KORTINN-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( KORTINN-IO-AREA (7:1) = '2'
            AND   KORTINN-IO-AREA (8:1) = '0' )
               MOVE LOW-VALUES             TO KORTINN-LEVEL-02
               MOVE KORTINN-IO-AREA (57:3) TO KORTINN-02-L2-FIRM
               MOVE KORTINN-IO-AREA (70:11) TO KORTINN-02-L1-BBSKTO
               IF  KORTINN-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  KORTINN-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  KORTINN-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  KORTINN-02-L2         TO THE-PRIOR-L2
               MOVE  KORTINN-02-L1         TO THE-PRIOR-L1
               SET KORTINN-LEVEL-INIT      TO TRUE
           WHEN ( KORTINN-IO-AREA (7:1) = '3'
            AND   KORTINN-IO-AREA (8:1) = '0' )
               CONTINUE
           WHEN ( KORTINN-IO-AREA (7:1) = '8'
            AND   KORTINN-IO-AREA (8:1) = '8' )
               CONTINUE
           WHEN OTHER
               CONTINUE
           END-EVALUATE.
 
       UTBREC-GET SECTION.
       UTBREC-GET-P.
           IF  UTBREC-EOF-OFF
               READ UTBREC
               AT END
                   SET UTBREC-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       UTBREC-FLDSET SECTION.
       UTBREC-FLDSET-P.
           EVALUATE TRUE
           WHEN ( UTBREC-IO-AREA (30:1) = 'A' )
               MOVE UTBREC-IO-AREA (41:120) TO UBRREC (1:120)
               MOVE UTBREC-IO-AREA (399:7) TO UBBEL-IO
               MOVE UTBREC-IO-AREA (406:8) TO UBVAL-IO
               MOVE UTBREC-IO-AREA (414:3) TO UBVT (1:3)
               MOVE UTBREC-IO-AREA (488:4) TO UBOPPH (1:4)
               MOVE UTBREC-IO-AREA (492:5) TO UBPRDT (1:5)
               MOVE UTBREC-IO-AREA (497:4) TO UBPRKL (1:4)
           END-EVALUATE.
 
       UTBREC-IDCHK SECTION.
       UTBREC-IDCHK-P.
           EVALUATE TRUE
           WHEN ( UTBREC-IO-AREA (30:1) = 'A' )
             OR ( UTBREC-IO-AREA (30:1) NOT = 'A' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       UTBREC-IDSET SECTION.
       UTBREC-IDSET-P.
           EVALUATE TRUE
           WHEN ( UTBREC-IO-AREA (30:1) = 'A' )
               SET I-01                    TO TRUE
           WHEN ( UTBREC-IO-AREA (30:1) NOT = 'A' )
               SET I-09                    TO TRUE
           END-EVALUATE.
 
       RELMAST-FLDSET SECTION.
       RELMAST-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RELMAST-IO-AREA (21:6) TO RELKNR (1:6)
           END-EVALUATE.
 
       RELMAST-IDSET SECTION.
       RELMAST-IDSET-P.
           SET I-10                        TO TRUE.
 
       SYSPARM-FLDOFF SECTION.
       SYSPARM-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-71                TO TRUE
               SET NOT-I-72                TO TRUE
               SET NOT-I-73                TO TRUE
           END-EVALUATE.
 
       SYSPARM-FLDSET SECTION.
       SYSPARM-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE SYSPARM-IO-AREA (101:4) TO SYSHB1 (1:4)
               IF  SYSHB1 = SPACES
                   SET I-71                TO TRUE
               END-IF
               MOVE SYSPARM-IO-AREA (112:4) TO SYSHB2 (1:4)
               IF  SYSHB2 = SPACES
                   SET I-72                TO TRUE
               END-IF
               MOVE SYSPARM-IO-AREA (116:11) TO SYSBKT (1:11)
               IF  SYSBKT = SPACES
                   SET I-73                TO TRUE
               END-IF
           END-EVALUATE.
 
       SYSPARM-IDSET SECTION.
       SYSPARM-IDSET-P.
           SET I-07                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-06 AND I-15)
               MOVE SPACES TO TILRELF-IO-AREA
               INITIALIZE TILRELF-IO-AREA
               MOVE '21'                   TO TILRELF-IO-AREA (1:2)
               MOVE FKOD                   TO TILRELF-IO-AREA (3:3)
               MOVE BLGART                 TO TILRELF-IO-AREA (6:1)
               MOVE BLGNR                  TO TILRELF-IO-AREA (7:6)
               MOVE BLGDTO                 TO TILRELF-IO-AREA (13:6)
               MOVE KTORES                 TO TILRELF-IO-AREA (19:8)
               MOVE BEL-IO                 TO TILRELF-IO-AREA (27:9)
               MOVE ' '                    TO TILRELF-IO-AREA (36:1)
               MOVE MOTKTO                 TO TILRELF-IO-AREA (37:4)
               MOVE REFNR                  TO TILRELF-IO-AREA (41:6)
               MOVE FORFDT                 TO TILRELF-IO-AREA (59:6)
               MOVE AK                     TO TILRELF-IO-AREA (65:1)
               MOVE SNR                    TO TILRELF-IO-AREA (66:3)
               MOVE DV                     TO TILRELF-IO-AREA (69:1)
               MOVE VALUTA-IO              TO TILRELF-IO-AREA (70:10)
               MOVE RDG                    TO TILRELF-IO-AREA (80:3)
               MOVE TK                     TO TILRELF-IO-AREA (83:2)
               MOVE SIGN-X                 TO TILRELF-IO-AREA (85:2)
               MOVE TXTSEQ                 TO TILRELF-IO-AREA (96:1)
               MOVE TEKST                  TO TILRELF-IO-AREA (97:24)
               MOVE BELDUO                 TO XO-112P
               MOVE XO-112P-EF             TO TILRELF-IO-AREA (121:7)
               MOVE VALDUO                 TO XO-114P
               MOVE XO-114P-EF             TO TILRELF-IO-AREA (128:8)
               MOVE VTDU                   TO TILRELF-IO-AREA (136:3)
               MOVE OPPHAV                 TO TILRELF-IO-AREA (228:4)
               MOVE PRDDDF                 TO TILRELF-IO-AREA (232:5)
               MOVE PRDKDF                 TO TILRELF-IO-AREA (237:4)
               WRITE TILRELF-IO-AREA
           END-IF
           IF  (I-03 AND I-11)
               MOVE SPACES TO TILRELF-IO-AREA
               INITIALIZE TILRELF-IO-AREA
               MOVE '21'                   TO TILRELF-IO-AREA (1:2)
               MOVE FIRM                   TO TILRELF-IO-AREA (3:3)
               MOVE '4'                    TO TILRELF-IO-AREA (6:1)
               MOVE BILNR                  TO TILRELF-IO-AREA (7:6)
               MOVE BILDAT                 TO TILRELF-IO-AREA (13:6)
               MOVE KNR                    TO TILRELF-IO-AREA (19:6)
               IF  (I-74 AND I-75 AND I-76)
                   MOVE RTF2               TO TILRELF-IO-AREA (19:6)
               END-IF
               MOVE '  '                   TO TILRELF-IO-AREA (25:2)
               MOVE INBEL2-IO              TO TILRELF-IO-AREA (27:9)
               MOVE ' '                    TO TILRELF-IO-AREA (36:1)
               MOVE '2000'                 TO TILRELF-IO-AREA (37:4)
               MOVE OCRKT1                 TO TILRELF-IO-AREA (37:4)
               IF  (I-78)
                   MOVE OCRKT2             TO TILRELF-IO-AREA (37:4)
               END-IF
               MOVE '9'                    TO TILRELF-IO-AREA (41:1)
               IF  (I-74)
                   MOVE '0'                TO TILRELF-IO-AREA (41:1)
               END-IF
               MOVE INKNR                  TO TILRELF-IO-AREA (42:5)
               IF  (I-74 AND I-75 AND I-76)
                   MOVE 'UKJENT'           TO TILRELF-IO-AREA (41:6)
               END-IF
               MOVE BILDAT                 TO TILRELF-IO-AREA (59:6)
               MOVE ' '                    TO TILRELF-IO-AREA (65:1)
               MOVE '31'                   TO TILRELF-IO-AREA (83:2)
               MOVE RABREN-IO              TO TILRELF-IO-AREA (87:6)
               MOVE 'BBS REF: '            TO TILRELF-IO-AREA (97:9)
               IF  (NOT-I-14)
                   MOVE BBSRF1             TO TILRELF-IO-AREA (105:10)
               END-IF
               IF  (I-14)
                   MOVE BBSRF2             TO TILRELF-IO-AREA (105:11)
               END-IF
               MOVE INB132                 TO XO-112P
               MOVE XO-112P-EF             TO TILRELF-IO-AREA (121:7)
               MOVE BEL154                 TO XO-114P
               MOVE XO-114P-EF             TO TILRELF-IO-AREA (128:8)
               MOVE 'NOK'                  TO TILRELF-IO-AREA (136:3)
               MOVE 'BBS '                 TO TILRELF-IO-AREA (228:4)
               MOVE DDATO8                 TO XO-80P
               MOVE XO-80P-EF              TO TILRELF-IO-AREA (232:5)
               MOVE TIDSP                  TO XO-60P
               MOVE XO-60P-EF              TO TILRELF-IO-AREA (237:4)
               WRITE TILRELF-IO-AREA
           END-IF
           IF  (I-03 AND I-12)
               MOVE SPACES TO TILRELF-IO-AREA
               INITIALIZE TILRELF-IO-AREA
               MOVE '21'                   TO TILRELF-IO-AREA (1:2)
               MOVE FIRM                   TO TILRELF-IO-AREA (3:3)
               MOVE '4'                    TO TILRELF-IO-AREA (6:1)
               MOVE BILNR                  TO TILRELF-IO-AREA (7:6)
               MOVE BILDAT                 TO TILRELF-IO-AREA (13:6)
               MOVE KNR                    TO TILRELF-IO-AREA (19:6)
               IF  (I-74 AND I-75 AND I-76)
                   MOVE RTF2               TO TILRELF-IO-AREA (19:6)
               END-IF
               MOVE '  '                   TO TILRELF-IO-AREA (25:2)
               MOVE UTNREN-IO              TO TILRELF-IO-AREA (27:9)
               MOVE ' '                    TO TILRELF-IO-AREA (36:1)
               MOVE '2000'                 TO TILRELF-IO-AREA (37:4)
               MOVE OCRKT1                 TO TILRELF-IO-AREA (37:4)
               IF  (I-78)
                   MOVE OCRKT2             TO TILRELF-IO-AREA (37:4)
               END-IF
               MOVE '9'                    TO TILRELF-IO-AREA (41:1)
               IF  (I-74)
                   MOVE '0'                TO TILRELF-IO-AREA (41:1)
               END-IF
               MOVE INKNR                  TO TILRELF-IO-AREA (42:5)
               IF  (I-74 AND I-75 AND I-76)
                   MOVE 'UKJENT'           TO TILRELF-IO-AREA (41:6)
               END-IF
               MOVE BILDAT                 TO TILRELF-IO-AREA (59:6)
               MOVE ' '                    TO TILRELF-IO-AREA (65:1)
               MOVE '31'                   TO TILRELF-IO-AREA (83:2)
               MOVE '      '               TO TILRELF-IO-AREA (87:6)
               MOVE 'BBS REF: '            TO TILRELF-IO-AREA (97:9)
               IF  (NOT-I-14)
                   MOVE BBSRF1             TO TILRELF-IO-AREA (105:10)
               END-IF
               IF  (I-14)
                   MOVE BBSRF2             TO TILRELF-IO-AREA (105:11)
               END-IF
               MOVE UTR132                 TO XO-112P
               MOVE XO-112P-EF             TO TILRELF-IO-AREA (121:7)
               MOVE BEL154                 TO XO-114P
               MOVE XO-114P-EF             TO TILRELF-IO-AREA (128:8)
               MOVE 'NOK'                  TO TILRELF-IO-AREA (136:3)
               MOVE 'BBS '                 TO TILRELF-IO-AREA (228:4)
               MOVE DDATO8                 TO XO-80P
               MOVE XO-80P-EF              TO TILRELF-IO-AREA (232:5)
               MOVE TIDSP                  TO XO-60P
               MOVE XO-60P-EF              TO TILRELF-IO-AREA (237:4)
               WRITE TILRELF-IO-AREA
           END-IF
           IF  (I-03 AND I-12 AND I-17)
               MOVE SPACES TO TILRELF-IO-AREA
               INITIALIZE TILRELF-IO-AREA
               MOVE '21'                   TO TILRELF-IO-AREA (1:2)
               MOVE FIRM                   TO TILRELF-IO-AREA (3:3)
               MOVE '4'                    TO TILRELF-IO-AREA (6:1)
               MOVE BILNR                  TO TILRELF-IO-AREA (7:6)
               MOVE BILDAT                 TO TILRELF-IO-AREA (13:6)
               MOVE KNR                    TO TILRELF-IO-AREA (19:6)
               IF  (I-74 AND I-75 AND I-76)
                   MOVE RTF2               TO TILRELF-IO-AREA (19:6)
               END-IF
               MOVE '  '                   TO TILRELF-IO-AREA (25:2)
               MOVE RENTEU-IO              TO TILRELF-IO-AREA (27:9)
               MOVE ' '                    TO TILRELF-IO-AREA (36:1)
               MOVE '2000'                 TO TILRELF-IO-AREA (37:4)
               MOVE OCRKT1                 TO TILRELF-IO-AREA (37:4)
               IF  (I-78)
                   MOVE OCRKT2             TO TILRELF-IO-AREA (37:4)
               END-IF
               MOVE '9'                    TO TILRELF-IO-AREA (41:1)
               IF  (I-74)
                   MOVE '0'                TO TILRELF-IO-AREA (41:1)
               END-IF
               MOVE '970'                  TO TILRELF-IO-AREA (41:3)
               MOVE INK3SI                 TO TILRELF-IO-AREA (44:3)
               IF  (I-74 AND I-75 AND I-76)
                   MOVE 'UKJENT'           TO TILRELF-IO-AREA (41:6)
               END-IF
               MOVE BILDAT                 TO TILRELF-IO-AREA (59:6)
               MOVE ' '                    TO TILRELF-IO-AREA (65:1)
               MOVE '31'                   TO TILRELF-IO-AREA (83:2)
               MOVE '      '               TO TILRELF-IO-AREA (87:6)
               MOVE 'BBS REF: '            TO TILRELF-IO-AREA (97:9)
               IF  (NOT-I-14)
                   MOVE BBSRF1             TO TILRELF-IO-AREA (105:10)
               END-IF
               IF  (I-14)
                   MOVE BBSRF2             TO TILRELF-IO-AREA (105:11)
               END-IF
               MOVE REN132                 TO XO-112P
               MOVE XO-112P-EF             TO TILRELF-IO-AREA (121:7)
               MOVE BEL154                 TO XO-114P
               MOVE XO-114P-EF             TO TILRELF-IO-AREA (128:8)
               MOVE 'NOK'                  TO TILRELF-IO-AREA (136:3)
               MOVE 'BBS '                 TO TILRELF-IO-AREA (228:4)
               MOVE DDATO8                 TO XO-80P
               MOVE XO-80P-EF              TO TILRELF-IO-AREA (232:5)
               MOVE TIDSP                  TO XO-60P
               MOVE XO-60P-EF              TO TILRELF-IO-AREA (237:4)
               WRITE TILRELF-IO-AREA
           END-IF
           IF  (I-03 AND I-13)
               MOVE SPACES TO TILRELF-IO-AREA
               INITIALIZE TILRELF-IO-AREA
               MOVE '21'                   TO TILRELF-IO-AREA (1:2)
               MOVE FIRM                   TO TILRELF-IO-AREA (3:3)
               MOVE '4'                    TO TILRELF-IO-AREA (6:1)
               MOVE BILNR                  TO TILRELF-IO-AREA (7:6)
               MOVE BILDAT                 TO TILRELF-IO-AREA (13:6)
               IF  (I-22)
                   MOVE '      '           TO TILRELF-IO-AREA (19:6)
               END-IF
               IF  (NOT-I-22)
                   MOVE RELKNR             TO TILRELF-IO-AREA (19:6)
               END-IF
               MOVE '  '                   TO TILRELF-IO-AREA (25:2)
               MOVE INBEL2-IO              TO TILRELF-IO-AREA (27:9)
               MOVE ' '                    TO TILRELF-IO-AREA (36:1)
               MOVE '2000'                 TO TILRELF-IO-AREA (37:4)
               MOVE OCRKT1                 TO TILRELF-IO-AREA (37:4)
               IF  (I-78)
                   MOVE OCRKT2             TO TILRELF-IO-AREA (37:4)
               END-IF
               MOVE FKREFB                 TO TILRELF-IO-AREA (41:6)
               MOVE 'F'                    TO TILRELF-IO-AREA (47:1)
               IF  (I-24)
                   MOVE 'B'                TO TILRELF-IO-AREA (47:1)
               END-IF
               MOVE BILDAT                 TO TILRELF-IO-AREA (59:6)
               MOVE ' '                    TO TILRELF-IO-AREA (65:1)
               MOVE '31'                   TO TILRELF-IO-AREA (83:2)
               MOVE '      '               TO TILRELF-IO-AREA (87:6)
               MOVE 'BBS REF: '            TO TILRELF-IO-AREA (97:9)
               IF  (NOT-I-14)
                   MOVE BBSRF1             TO TILRELF-IO-AREA (105:10)
               END-IF
               IF  (I-14)
                   MOVE BBSRF2             TO TILRELF-IO-AREA (105:11)
               END-IF
               IF  (I-05 AND I-10 AND I-U8)
                   MOVE 'DUMMY'            TO TILRELF-IO-AREA (116:5)
               END-IF
               IF  (I-07 AND I-10 AND I-U8)
                   MOVE 'DUMMY'            TO TILRELF-IO-AREA (116:5)
               END-IF
               MOVE INB132                 TO XO-112P
               MOVE XO-112P-EF             TO TILRELF-IO-AREA (121:7)
               MOVE BEL154                 TO XO-114P
               MOVE XO-114P-EF             TO TILRELF-IO-AREA (128:8)
               MOVE 'NOK'                  TO TILRELF-IO-AREA (136:3)
               MOVE 'BBS '                 TO TILRELF-IO-AREA (228:4)
               MOVE DDATO8                 TO XO-80P
               MOVE XO-80P-EF              TO TILRELF-IO-AREA (232:5)
               MOVE TIDSP                  TO XO-60P
               MOVE XO-60P-EF              TO TILRELF-IO-AREA (237:4)
               WRITE TILRELF-IO-AREA
           END-IF
           IF  (I-03 AND NOT-I-11 AND NOT-I-12)
           AND (NOT-I-13)
               MOVE SPACES TO TILRELF-IO-AREA
               INITIALIZE TILRELF-IO-AREA
               MOVE '21'                   TO TILRELF-IO-AREA (1:2)
               MOVE FIRM                   TO TILRELF-IO-AREA (3:3)
               MOVE '4'                    TO TILRELF-IO-AREA (6:1)
               MOVE BILNR                  TO TILRELF-IO-AREA (7:6)
               MOVE BILDAT                 TO TILRELF-IO-AREA (13:6)
               MOVE KNR                    TO TILRELF-IO-AREA (19:6)
               IF  (I-74 AND I-75 AND I-76)
                   MOVE RTF2               TO TILRELF-IO-AREA (19:6)
               END-IF
               MOVE '  '                   TO TILRELF-IO-AREA (25:2)
               MOVE INBEL2-IO              TO TILRELF-IO-AREA (27:9)
               MOVE ' '                    TO TILRELF-IO-AREA (36:1)
               MOVE '2000'                 TO TILRELF-IO-AREA (37:4)
               MOVE OCRKT1                 TO TILRELF-IO-AREA (37:4)
               IF  (I-78)
                   MOVE OCRKT2             TO TILRELF-IO-AREA (37:4)
               END-IF
               MOVE '9'                    TO TILRELF-IO-AREA (41:1)
               IF  (I-74)
                   MOVE '0'                TO TILRELF-IO-AREA (41:1)
               END-IF
               MOVE INKNR                  TO TILRELF-IO-AREA (42:5)
               IF  (I-74 AND I-75 AND I-76)
                   MOVE 'UKJENT'           TO TILRELF-IO-AREA (41:6)
               END-IF
               MOVE BILDAT                 TO TILRELF-IO-AREA (59:6)
               MOVE ' '                    TO TILRELF-IO-AREA (65:1)
               MOVE '31'                   TO TILRELF-IO-AREA (83:2)
               MOVE RABREN-IO              TO TILRELF-IO-AREA (87:6)
               MOVE 'BBS REF: '            TO TILRELF-IO-AREA (97:9)
               IF  (NOT-I-14)
                   MOVE BBSRF1             TO TILRELF-IO-AREA (105:10)
               END-IF
               IF  (I-14)
                   MOVE BBSRF2             TO TILRELF-IO-AREA (105:11)
               END-IF
               MOVE INB132                 TO XO-112P
               MOVE XO-112P-EF             TO TILRELF-IO-AREA (121:7)
               MOVE BEL154                 TO XO-114P
               MOVE XO-114P-EF             TO TILRELF-IO-AREA (128:8)
               MOVE 'NOK'                  TO TILRELF-IO-AREA (136:3)
               MOVE 'BBS '                 TO TILRELF-IO-AREA (228:4)
               MOVE DDATO8                 TO XO-80P
               MOVE XO-80P-EF              TO TILRELF-IO-AREA (232:5)
               MOVE TIDSP                  TO XO-60P
               MOVE XO-60P-EF              TO TILRELF-IO-AREA (237:4)
               WRITE TILRELF-IO-AREA
           END-IF
           IF  (I-06 AND NOT-I-15)
               MOVE SPACES TO UTNRELF-IO-AREA
               INITIALIZE UTNRELF-IO-AREA
               MOVE '21'                   TO UTNRELF-IO-AREA (1:2)
               MOVE FKOD                   TO UTNRELF-IO-AREA (3:3)
               MOVE BLGART                 TO UTNRELF-IO-AREA (6:1)
               MOVE BLGNR                  TO UTNRELF-IO-AREA (7:6)
               MOVE BLGDTO                 TO UTNRELF-IO-AREA (13:6)
               MOVE KTORES                 TO UTNRELF-IO-AREA (19:8)
               MOVE BEL-IO                 TO UTNRELF-IO-AREA (27:9)
               MOVE ' '                    TO UTNRELF-IO-AREA (36:1)
               MOVE MOTKTO                 TO UTNRELF-IO-AREA (37:4)
               MOVE REFNR                  TO UTNRELF-IO-AREA (41:6)
               MOVE AVD4                   TO UTNRELF-IO-AREA (47:4)
               MOVE FORFDT                 TO UTNRELF-IO-AREA (59:6)
               MOVE AK                     TO UTNRELF-IO-AREA (65:1)
               MOVE SNR                    TO UTNRELF-IO-AREA (66:3)
               MOVE DV                     TO UTNRELF-IO-AREA (69:1)
               MOVE VALUTA-IO              TO UTNRELF-IO-AREA (70:10)
               MOVE RDG                    TO UTNRELF-IO-AREA (80:3)
               MOVE TK                     TO UTNRELF-IO-AREA (83:2)
               MOVE SIGN-X                 TO UTNRELF-IO-AREA (85:2)
               MOVE TXTSEQ                 TO UTNRELF-IO-AREA (96:1)
               MOVE TEKST                  TO UTNRELF-IO-AREA (97:24)
               MOVE BELDUO                 TO XO-112P
               MOVE XO-112P-EF             TO UTNRELF-IO-AREA (121:7)
               MOVE VALDUO                 TO XO-114P
               MOVE XO-114P-EF             TO UTNRELF-IO-AREA (128:8)
               MOVE VTDU                   TO UTNRELF-IO-AREA (136:3)
               MOVE OPPHAV                 TO UTNRELF-IO-AREA (228:4)
               MOVE PRDDDF                 TO UTNRELF-IO-AREA (232:5)
               MOVE PRDKDF                 TO UTNRELF-IO-AREA (237:4)
               WRITE UTNRELF-IO-AREA
           END-IF
           IF  (I-01)
               MOVE SPACES TO UTNRELF-IO-AREA
               INITIALIZE UTNRELF-IO-AREA
               MOVE UBRREC                 TO UTNRELF-IO-AREA (1:120)
               MOVE UBBEL                  TO XO-112P
               MOVE XO-112P-EF             TO UTNRELF-IO-AREA (121:7)
               MOVE UBVAL                  TO XO-114P
               MOVE XO-114P-EF             TO UTNRELF-IO-AREA (128:8)
               MOVE UBVT                   TO UTNRELF-IO-AREA (136:3)
               MOVE UBOPPH                 TO UTNRELF-IO-AREA (228:4)
               MOVE UBPRDT                 TO UTNRELF-IO-AREA (232:5)
               MOVE UBPRKL                 TO UTNRELF-IO-AREA (237:4)
               WRITE UTNRELF-IO-AREA
           END-IF
           IF  (I-99)
               MOVE SPACES TO BBSBEL-IO-AREA
               INITIALIZE BBSBEL-IO-AREA
               MOVE '* ÅÅ JOB JNM=BBS10M,CLAS' TO BBSBEL-IO-AREA (1:24)
               MOVE 'S=5,DISP=D,PRI=9        ' TO BBSBEL-IO-AREA
                                                               (25:24)
               WRITE BBSBEL-IO-AREA
               MOVE SPACES TO BBSBEL-IO-AREA
               INITIALIZE BBSBEL-IO-AREA
               MOVE '* ÅÅ LST LST=SYSLST,LST=' TO BBSBEL-IO-AREA (1:24)
               MOVE '00E,CLASS=Z,DISP=H      ' TO BBSBEL-IO-AREA
                                                               (25:24)
               WRITE BBSBEL-IO-AREA
               MOVE SPACES TO BBSBEL-IO-AREA
               INITIALIZE BBSBEL-IO-AREA
               MOVE '// JOB BBS10M  SENDER MA' TO BBSBEL-IO-AREA (1:24)
               MOVE 'IL M/BBS OCR SUMBELØP TI' TO BBSBEL-IO-AREA
                                                               (25:24)
               MOVE 'L SYSREGNOPPG           ' TO BBSBEL-IO-AREA
                                                               (49:24)
               WRITE BBSBEL-IO-AREA
               MOVE SPACES TO BBSBEL-IO-AREA
               INITIALIZE BBSBEL-IO-AREA
               MOVE '/*'                   TO BBSBEL-IO-AREA (1:2)
               WRITE BBSBEL-IO-AREA
               MOVE SPACES TO BBSBEL-IO-AREA
               INITIALIZE BBSBEL-IO-AREA
               MOVE '*  STEP 01 MAIL         ' TO BBSBEL-IO-AREA (1:24)
               MOVE '                        ' TO BBSBEL-IO-AREA
                                                               (25:24)
               WRITE BBSBEL-IO-AREA
               MOVE SPACES TO BBSBEL-IO-AREA
               INITIALIZE BBSBEL-IO-AREA
               MOVE '* ÅÅ LST CLASS=Z,DISP=H ' TO BBSBEL-IO-AREA (1:24)
               MOVE '                        ' TO BBSBEL-IO-AREA
                                                               (25:24)
               WRITE BBSBEL-IO-AREA
               MOVE SPACES TO BBSBEL-IO-AREA
               INITIALIZE BBSBEL-IO-AREA
               MOVE '// EXEC EMAIL,SIZE=E' TO BBSBEL-IO-AREA (1:20)
               MOVE 'MAIL,PARM='           TO BBSBEL-IO-AREA (21:10)
               MOVE '''ID=02'''              TO BBSBEL-IO-AREA (31:7)
               WRITE BBSBEL-IO-AREA
               MOVE SPACES TO BBSBEL-IO-AREA
               INITIALIZE BBSBEL-IO-AREA
               MOVE 'SET FROM=REGNSKAP@AUTODA' TO BBSBEL-IO-AREA (1:24)
               MOVE 'TA.NO                   ' TO BBSBEL-IO-AREA
                                                               (25:24)
               WRITE BBSBEL-IO-AREA
               MOVE SPACES TO BBSBEL-IO-AREA
               INITIALIZE BBSBEL-IO-AREA
               MOVE 'SET TO=SYSREGNOPPG@AUTOD' TO BBSBEL-IO-AREA (1:24)
               MOVE 'ATA.NO                  ' TO BBSBEL-IO-AREA
                                                               (25:24)
      *       D        99
      *                                  24 "SET COPY=MORTEN@AUTODATA"
      *                                  48 ".NO                     "
               WRITE BBSBEL-IO-AREA
               MOVE SPACES TO BBSBEL-IO-AREA
               INITIALIZE BBSBEL-IO-AREA
               MOVE 'SET SUBJECT=BBS10 - OCR ' TO BBSBEL-IO-AREA (1:24)
               MOVE 'INNBETALINGER SUMBELØP  ' TO BBSBEL-IO-AREA
                                                               (25:24)
               WRITE BBSBEL-IO-AREA
               MOVE SPACES TO BBSBEL-IO-AREA
               INITIALIZE BBSBEL-IO-AREA
               MOVE 'SET DISP=HOLD           ' TO BBSBEL-IO-AREA (1:24)
               MOVE '                        ' TO BBSBEL-IO-AREA
                                                               (25:24)
               WRITE BBSBEL-IO-AREA
               MOVE SPACES TO BBSBEL-IO-AREA
               INITIALIZE BBSBEL-IO-AREA
               MOVE 'TEXT                    ' TO BBSBEL-IO-AREA (1:24)
               WRITE BBSBEL-IO-AREA
               MOVE SPACES TO BBSBEL-IO-AREA
               INITIALIZE BBSBEL-IO-AREA
               MOVE 'FIRMA'                TO BBSBEL-IO-AREA (1:5)
               MOVE 'BBS KONTONR'          TO BBSBEL-IO-AREA (8:11)
               MOVE 'HBOK'                 TO BBSBEL-IO-AREA (21:4)
               MOVE 'FRA '                 TO BBSBEL-IO-AREA (27:4)
               MOVE 'SUM INNBETALT '       TO BBSBEL-IO-AREA (37:14)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO BBSBEL-IO-AREA (73:8)
               WRITE BBSBEL-IO-AREA
           END-IF
           IF  (I-04)
               MOVE SPACES TO BBSBEL-IO-AREA
               INITIALIZE BBSBEL-IO-AREA
               MOVE FIRM                   TO BBSBEL-IO-AREA (2:3)
               MOVE BBSKTO                 TO BBSBEL-IO-AREA (8:11)
               IF  (NOT-I-78)
                   MOVE OCRKT1             TO BBSBEL-IO-AREA (21:4)
               END-IF
               IF  (NOT-I-78)
                   MOVE 'FIRM'             TO BBSBEL-IO-AREA (27:4)
               END-IF
               IF  (I-78)
                   MOVE OCRKT2             TO BBSBEL-IO-AREA (21:4)
               END-IF
               IF  (I-78)
                   MOVE 'RG02'             TO BBSBEL-IO-AREA (27:4)
               END-IF
               IF  (I-78)
                   MOVE '<---- AVVIK'      TO BBSBEL-IO-AREA (52:11)
               END-IF
               MOVE BBSSUM                 TO XO-72YY9R
               MOVE XO-72YY9R              TO BBSBEL-IO-AREA (38:13)
               WRITE BBSBEL-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR AND I-50)
               MOVE SPACES TO BBSBEL-IO-AREA
               INITIALIZE BBSBEL-IO-AREA
               MOVE 'GRAND TOTAL INKL AVVISTE' TO BBSBEL-IO-AREA (1:24)
               MOVE ' TRANSER'             TO BBSBEL-IO-AREA (25:8)
               MOVE SUMBEL                 TO XO-92YY9R
               MOVE XO-92YY9R              TO BBSBEL-IO-AREA (36:15)
               WRITE BBSBEL-IO-AREA
               MOVE SPACES TO BBSBEL-IO-AREA
               INITIALIZE BBSBEL-IO-AREA
               MOVE '/+                      ' TO BBSBEL-IO-AREA (1:24)
               MOVE '                        ' TO BBSBEL-IO-AREA
                                                               (25:24)
               WRITE BBSBEL-IO-AREA
               MOVE SPACES TO BBSBEL-IO-AREA
               INITIALIZE BBSBEL-IO-AREA
               MOVE 'SEND                    ' TO BBSBEL-IO-AREA (1:24)
               WRITE BBSBEL-IO-AREA
               MOVE SPACES TO BBSBEL-IO-AREA
               INITIALIZE BBSBEL-IO-AREA
               MOVE 'QUIT                    ' TO BBSBEL-IO-AREA (1:24)
               MOVE '                        ' TO BBSBEL-IO-AREA
                                                               (25:24)
               WRITE BBSBEL-IO-AREA
               MOVE SPACES TO BBSBEL-IO-AREA
               INITIALIZE BBSBEL-IO-AREA
               MOVE '/*'                   TO BBSBEL-IO-AREA (1:2)
               WRITE BBSBEL-IO-AREA
               MOVE SPACES TO BBSBEL-IO-AREA
               INITIALIZE BBSBEL-IO-AREA
               MOVE '/&'                   TO BBSBEL-IO-AREA (1:2)
               WRITE BBSBEL-IO-AREA
               MOVE SPACES TO BBSBEL-IO-AREA
               INITIALIZE BBSBEL-IO-AREA
               MOVE '* ÅÅ EOJ'             TO BBSBEL-IO-AREA (1:8)
               WRITE BBSBEL-IO-AREA
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
           MOVE 3                          TO LR-CHECK
           INITIALIZE RESKTRA-DATA-FIELDS
           SET RESKTRA-EOF-OFF             TO TRUE
           SET RESKTRA-PROCESS             TO TRUE
           OPEN INPUT RESKTRA
           SET KORTINN-LEVEL-INIT          TO TRUE
           INITIALIZE KORTINN-DATA-FIELDS
           SET KORTINN-EOF-OFF             TO TRUE
           SET KORTINN-PROCESS             TO TRUE
           OPEN INPUT KORTINN
           INITIALIZE UTBREC-DATA-FIELDS
           SET UTBREC-EOF-OFF              TO TRUE
           SET UTBREC-PROCESS              TO TRUE
           OPEN INPUT UTBREC
           INITIALIZE RELMAST-DATA-FIELDS
           OPEN INPUT RELMAST
           INITIALIZE SYSPARM-DATA-FIELDS
           OPEN INPUT SYSPARM
           OPEN OUTPUT TILRELF
           OPEN OUTPUT UTNRELF
           OPEN OUTPUT BBSBEL.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE RESKTRA
           CLOSE KORTINN
           CLOSE UTBREC
           CLOSE RELMAST
           CLOSE SYSPARM
           CLOSE TILRELF
           CLOSE UTNRELF
           CLOSE BBSBEL.
 
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
