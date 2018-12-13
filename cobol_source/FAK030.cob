       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK030R.
      **********************************************  Z-WIN-RPG2   ****
      *    JCL    : XFAK10AU                                    ******
      *    PROGRAM: FAK030                                           *
      *    DANNE FAKTURARECORDS FRA FERDIGMELDTE ORDRERECORDS TIL    *
      *    FAKTURERING.                                              *
      *    SAMT TILDELING AV ORDRESUMRABATT / ORDREGEBYR.            *
      *         FAKTURAGEBYR TILDELES I PROGRAM FAK075.              *
      *    TABELLEN MÅ OPPDATERES VED ORDRESUMRAB/EKSP.GEBYR.        *
      *    UPSI 8 OPPDATERER IKKE AVSTEMMINGSFILE (TEST)             *
      *    UPSI 1 KONTANTSALGSFAKTURARUT(TAR MED LINJER MED 0 I LEV) *
      *           DETTE FOR Å OPPDATERE ORDRENR.FILE SOM FAKT. ORDRE.*
      * 3/2-95 EL. FAKTURAMÅTE 5,6 DANNER TEKSTLINJER PÅ VAREADRESSE *
      * 7/2-95 EL. STATISTIKKREC. FOR FEILEKSP. BLIR IKKE TATT MED.  *
      *15/3-96 EL. MERKET RECORD SOM KOMMER FRA NY RESTORDRERUTINE.  *
      *            DISSE SKAL IKKE HA AUTOMATISK FRAKT.              *
      * 8/1-97 EL. TILATER AVDELING A OG B.                          *
      *23/1-97 EL. MERKET VAREADRESSE SOM TEKSTRECORD MED A I POS 37 *
      *10/5-99 EL. SERVICEORDRE LEGGER UT SERVICETYPE I POS 132.(O-TYPE)
      *11/4-00 EL. UPSI 1 TAR MED VARELINJER MED 0 I ANTALL.
      *16/2-01 EL. FJERNET TILDELING AV NRK.AVGIFT DA DETTE UTGÅR.
      * 4/4-01 EL. SPESIAL ORDREGEBYR HAFNOR.
      * 8/6-01 EL. FJERNET TEST PÅ FULL LEVERING PÅ HAFNOR"S SPESIAL
      *            GEBYR.
      *29/8-01 EL. EGEN VGR. PÅ PANTER FOR FIRMA 956 OG 957.
      *4/10-01 EL. FAKTURAMÅTE 9 (FAKTURA PR. MND.) LAGT INN.
      *1010-01 EL. VARELINJER MED 0 I ANT.LEVERT TIL FAKTURERING
      *             PÅ FIRMA 923, DISSE FJERNES I FAK055.
      *07.11.01 EL.ROMNES OSLO SKAL HA FRAKTANDEL SOM FAKTURAGEBYR
      *                   OSLO = GEBYRKODE F, UTENBYS = GEBYRKODE H.
      *22.11.01 EL.KORRIGERT VAREADRESSE 4, STARTET I POS.11 I FELTET.
      *27.03.02 EL.FRAKT BENYTTER GEBYRKODE A I KUNDEMAS.
      *20.03.03 EL.OVERFØRER VARELINJEKODE1 (SELGERORDRE) TIL FAKT.REC.
      *22.06.04 EL.BELØP PÅ KR. 0,01 ENDRES TIL 0,00 PÅ BEMA.
      *28.10.04 EL.RODIN SKAL IKKE HA GEBYR PÅ SERVICEORDRE.
      *16.02.05 EL.AUTO-MATERIELL VIL HA ALFAKODE I ARTIKKELNR.
      *22.07.05 EL.OMREGNING TIL NETTOPRIS VIA SUB.PROG NETTOPRI
      *            PÅ KUNDE 115115 TOYOTA FOR 918 S.OG B.
      *26.10.05 EL.FJERNING AV FRAKT KCL ETTER EGNE REGLER.
      *29.11.05 EL.ALFAKODE I ARTIKKELNR. TESTES NÅ FRA FIRMAFILE/ORFI
      *12.12.05 EL.PANT/PRISTILEGG BLIR TATT MED I ORDRESUM FOR
      *            BEREGNING AV ORDRE/FAKTURAGEBYR.
      *07.03.06 EL.FAST FRAKT KUNDEKAT. 129 PÅ FIRMA 913 FOMA.
      *07.03.06 EL.NORMAL FRAK PÅ FIRMA 913 BED ILPOST, FLY, HENTES.
      *15.09.06 EL.RUTINE FOR Å MERKE ORDRE SOM ER MANUELT BESTILT.
      *            DETTE KAN GI BESTILLINGEBYR. HYDROTEXACO/HAFNOR.
      *04.04.08 EN.FJERNET TEST På 970 - AVDELING 1 UANSETT.
      *13.06.08 SS.LAGT INN SONEFRAKT 825
      ****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK030.rpg
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
           SELECT TEKSTAB
               ASSIGN TO UT-S-TEKSTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TEKSTAB-STATUS.
           SELECT OSKFILE
               ASSIGN TO UT-S-OSKFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OSKFILE-STATUS.
           SELECT FAFILE
               ASSIGN TO UT-S-FAFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAFILE-STATUS.
           SELECT ORDFAKT
               ASSIGN TO UT-S-ORDFAKT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ORDFAKT-STATUS.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT ORDNRM
               ASSIGN TO ORDNRM
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS ORDNRM-STATUS
               RECORD KEY IS ORDNRM-KEY1.
           SELECT VAREMAS
               ASSIGN TO VAREMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAREMAS-STATUS
               RECORD KEY IS VAREMAS-KEY1.
           SELECT AVSTEMF
               ASSIGN TO AVSTEMF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS AVSTEMF-STATUS
               RECORD KEY IS AVSTEMF-KEY1.
           SELECT OUTF
               ASSIGN TO UT-S-OUTF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTF-STATUS.
           SELECT OUTFRAK
               ASSIGN TO UT-S-OUTFRAK
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTFRAK-STATUS.
           SELECT TOTALER
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS TOTALER-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD TEKSTAB
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  TEKSTAB-IO-AREA.
           05  TEKSTAB-IO-AREA-X           PICTURE X(80).
       FD OSKFILE
               BLOCK CONTAINS 4070
               RECORD CONTAINS 110.
       01  OSKFILE-IO-AREA.
           05  OSKFILE-IO-AREA-X           PICTURE X(110).
       FD FAFILE
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  FAFILE-IO-AREA.
           05  FAFILE-IO-AREA-X            PICTURE X(200).
       FD ORDFAKT
               BLOCK CONTAINS 328
               RECORD CONTAINS 164.
       01  ORDFAKT-IO-AREA.
           05  ORDFAKT-IO-AREA-X           PICTURE X(164).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD ORDNRM
               RECORD CONTAINS 100.
       01  ORDNRM-IO-AREA.
           05  ORDNRM-IO-AREA-X.
               10  ORDNRM-KEY1             PICTURE X(9).
               10  FILLER                  PICTURE X(91).
       FD VAREMAS
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X.
               10  VAREMAS-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       FD AVSTEMF
               RECORD CONTAINS 1000.
       01  AVSTEMF-IO-AREA.
           05  AVSTEMF-IO-AREA-X.
               10  AVSTEMF-KEY1            PICTURE X(3).
               10  FILLER                  PICTURE X(997).
       FD OUTF
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  OUTF-IO-AREA.
           05  OUTF-IO-AREA-X              PICTURE X(200).
       FD OUTFRAK
               BLOCK CONTAINS 200
               RECORD CONTAINS 100.
       01  OUTFRAK-IO-AREA.
           05  OUTFRAK-IO-AREA-X           PICTURE X(100).
       FD TOTALER
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  TOTALER-IO-PRINT.
           05  TOTALER-IO-AREA-CONTROL     PICTURE X VALUE ' '.
        02 TOTALER-IO-AREA.
           05  TOTALER-IO-AREA-X           PICTURE X(132).
       WORKING-STORAGE SECTION.
       77  TABTTY-MAX   VALUE 30           PICTURE 9(4) USAGE BINARY.
       77  TABTTE-MAX   VALUE 30           PICTURE 9(4) USAGE BINARY.
       77  TABOSK-MAX   VALUE 100          PICTURE 9(4) USAGE BINARY.
       77  TABOSD-MAX   VALUE 100          PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  TABTTY-TABLE.
               10  TABTTY-ENTRY
                                           OCCURS 30 TIMES
                                           INDEXED BY TABTTY-I
                                                      TABTTY-S
                                                      TABTTE-I
                                                      TABTTE-S.
                   15  TABTTY              PICTURE X(1).
                   15  TABTTE              PICTURE X(30).
           05  TABOSK-TABLE.
               10  TABOSK-ENTRY
                                           OCCURS 100 TIMES
                                           INDEXED BY TABOSK-I
                                                      TABOSK-S
                                                      TABOSD-I
                                                      TABOSD-S.
                   15  TABOSK              PICTURE X(7).
                   15  TABOSD              PICTURE X(103).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  TEKSTAB-STATUS              PICTURE 99 VALUE 0.
           10  OSKFILE-STATUS              PICTURE 99 VALUE 0.
           10  FAFILE-STATUS               PICTURE 99 VALUE 0.
           10  ORDFAKT-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  ORDNRM-STATUS               PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  AVSTEMF-STATUS              PICTURE 99 VALUE 0.
           10  OUTF-STATUS                 PICTURE 99 VALUE 0.
           10  OUTFRAK-STATUS              PICTURE 99 VALUE 0.
           10  TOTALER-STATUS              PICTURE 99 VALUE 0.
           10  SUMKEY-XX-STATUS            PICTURE 99 VALUE 0.
           10  SUMDAT-XX-STATUS            PICTURE 99 VALUE 0.
           10  PRIDAT-XX-STATUS            PICTURE 99 VALUE 0.
           10  FRADAT-XX-STATUS            PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  TEKSTAB-EOF-OFF         VALUE '0'.
               88  TEKSTAB-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  OSKFILE-EOF-OFF         VALUE '0'.
               88  OSKFILE-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAFILE-EOF-OFF          VALUE '0'.
               88  FAFILE-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAFILE-READ-OFF         VALUE '0'.
               88  FAFILE-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAFILE-PROCESS-OFF      VALUE '0'.
               88  FAFILE-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  FAFILE-LEVEL-INIT-OFF   VALUE '0'.
               88  FAFILE-LEVEL-INIT       VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDFAKT-EOF-OFF         VALUE '0'.
               88  ORDFAKT-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDFAKT-READ-OFF        VALUE '0'.
               88  ORDFAKT-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDFAKT-PROCESS-OFF     VALUE '0'.
               88  ORDFAKT-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  ORDFAKT-LEVEL-INIT-OFF  VALUE '0'.
               88  ORDFAKT-LEVEL-INIT      VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  ORDNRM-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  VAREMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  AVSTEMF-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  TOTALER-DATA-FIELDS.
               10  TOTALER-AFTER-SPACE     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-AFTER-SKIP      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-BEFORE-SPACE    PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-BEFORE-SKIP     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-MAX-LINES       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-LINE-COUNT      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  TOTALER-CLR-IO          PICTURE X VALUE 'Y'.
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
           05  SUMKEY-XX-DATA-FIELDS.
               10  KFNR                    PICTURE X(3).
               10  FILLER                  PICTURE X(100).
           05  FILLER REDEFINES SUMKEY-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(3).
               10  KAVD                    PICTURE X(1).
               10  FILLER                  PICTURE X(99).
           05  FILLER REDEFINES SUMKEY-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(4).
               10  KKODE                   PICTURE X(1).
               10  FILLER                  PICTURE X(98).
           05  FILLER REDEFINES SUMKEY-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(5).
               10  KSEQ-IO.
                   15  KSEQ                PICTURE S9(2).
               10  FILLER                  PICTURE X(96).
           05  SUMDAT-XX REDEFINES SUMKEY-XX-DATA-FIELDS.
               10  FRABEL-IO.
                   15  FRABEL              PICTURE S9(7)V9(2).
               10  FILLER                  PICTURE X(94).
           05  FILLER REDEFINES SUMKEY-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(9).
               10  TILBEL-IO.
                   15  TILBEL              PICTURE S9(7)V9(2).
               10  FILLER                  PICTURE X(85).
           05  FILLER REDEFINES SUMKEY-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(18).
               10  RABPRO-IO.
                   15  RABPRO              PICTURE S9(2)V9(2).
               10  FILLER                  PICTURE X(81).
           05  FILLER REDEFINES SUMKEY-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(22).
               10  GEBYR-IO.
                   15  GEBYR               PICTURE S9(7)V9(2).
               10  FILLER                  PICTURE X(72).
           05  FILLER REDEFINES SUMKEY-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(31).
               10  SVGR                    PICTURE X(5).
               10  FILLER                  PICTURE X(67).
           05  FILLER REDEFINES SUMKEY-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(36).
               10  SEDBNR                  PICTURE X(7).
               10  FILLER                  PICTURE X(60).
           05  FILLER REDEFINES SUMKEY-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(43).
               10  STKS1                   PICTURE X(20).
               10  FILLER                  PICTURE X(40).
           05  FILLER REDEFINES SUMKEY-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(63).
               10  STKS2                   PICTURE X(30).
               10  FILLER                  PICTURE X(10).
           05  FILLER REDEFINES SUMKEY-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(93).
               10  SREST                   PICTURE X(10).
           05  PRIDAT-XX REDEFINES SUMKEY-XX-DATA-FIELDS.
               10  PRBEL-IO.
                   15  PRBEL               PICTURE S9(7)V9(2).
               10  FILLER                  PICTURE X(94).
           05  FILLER REDEFINES SUMKEY-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(9).
               10  PRRAB1-IO.
                   15  PRRAB1              PICTURE S9(2)V9(1).
               10  FILLER                  PICTURE X(91).
           05  FILLER REDEFINES SUMKEY-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(12).
               10  PRRAB2-IO.
                   15  PRRAB2              PICTURE S9(2)V9(1).
               10  FILLER                  PICTURE X(88).
           05  FILLER REDEFINES SUMKEY-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(15).
               10  PRRAB3-IO.
                   15  PRRAB3              PICTURE S9(2)V9(1).
               10  FILLER                  PICTURE X(85).
           05  FRADAT-XX REDEFINES SUMKEY-XX-DATA-FIELDS.
               10  PNR                     PICTURE X(4).
               10  FILLER                  PICTURE X(99).
           05  FILLER REDEFINES SUMKEY-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(4).
               10  FRAPRO-IO.
                   15  FRAPRO              PICTURE S9(1)V9(1).
               10  FILLER                  PICTURE X(97).
      *DSDS: DATA STRUCTURE FIELDS
           05  SUMDAT-XX-DATA-FIELDS.
               10  FRABEL-IO.
                   15  FRABEL              PICTURE S9(7)V9(2).
               10  FILLER                  PICTURE X(94).
           05  FILLER REDEFINES SUMDAT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(9).
               10  TILBEL-IO.
                   15  TILBEL              PICTURE S9(7)V9(2).
               10  FILLER                  PICTURE X(85).
           05  FILLER REDEFINES SUMDAT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(18).
               10  RABPRO-IO.
                   15  RABPRO              PICTURE S9(2)V9(2).
               10  FILLER                  PICTURE X(81).
           05  FILLER REDEFINES SUMDAT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(22).
               10  GEBYR-IO.
                   15  GEBYR               PICTURE S9(7)V9(2).
               10  FILLER                  PICTURE X(72).
           05  FILLER REDEFINES SUMDAT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(31).
               10  SVGR                    PICTURE X(5).
               10  FILLER                  PICTURE X(67).
           05  FILLER REDEFINES SUMDAT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(36).
               10  SEDBNR                  PICTURE X(7).
               10  FILLER                  PICTURE X(60).
           05  FILLER REDEFINES SUMDAT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(43).
               10  STKS1                   PICTURE X(20).
               10  FILLER                  PICTURE X(40).
           05  FILLER REDEFINES SUMDAT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(63).
               10  STKS2                   PICTURE X(30).
               10  FILLER                  PICTURE X(10).
           05  FILLER REDEFINES SUMDAT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(93).
               10  SREST                   PICTURE X(10).
           05  PRIDAT-XX REDEFINES SUMDAT-XX-DATA-FIELDS.
               10  PRBEL-IO.
                   15  PRBEL               PICTURE S9(7)V9(2).
               10  FILLER                  PICTURE X(94).
           05  FILLER REDEFINES SUMDAT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(9).
               10  PRRAB1-IO.
                   15  PRRAB1              PICTURE S9(2)V9(1).
               10  FILLER                  PICTURE X(91).
           05  FILLER REDEFINES SUMDAT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(12).
               10  PRRAB2-IO.
                   15  PRRAB2              PICTURE S9(2)V9(1).
               10  FILLER                  PICTURE X(88).
           05  FILLER REDEFINES SUMDAT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(15).
               10  PRRAB3-IO.
                   15  PRRAB3              PICTURE S9(2)V9(1).
               10  FILLER                  PICTURE X(85).
           05  FRADAT-XX REDEFINES SUMDAT-XX-DATA-FIELDS.
               10  PNR                     PICTURE X(4).
               10  FILLER                  PICTURE X(99).
           05  FILLER REDEFINES SUMDAT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(4).
               10  FRAPRO-IO.
                   15  FRAPRO              PICTURE S9(1)V9(1).
               10  FILLER                  PICTURE X(97).
      *DSDS: DATA STRUCTURE FIELDS
           05  PRIDAT-XX-DATA-FIELDS.
               10  PRBEL-IO.
                   15  PRBEL               PICTURE S9(7)V9(2).
               10  FILLER                  PICTURE X(9).
           05  FILLER REDEFINES PRIDAT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(9).
               10  PRRAB1-IO.
                   15  PRRAB1              PICTURE S9(2)V9(1).
               10  FILLER                  PICTURE X(6).
           05  FILLER REDEFINES PRIDAT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(12).
               10  PRRAB2-IO.
                   15  PRRAB2              PICTURE S9(2)V9(1).
               10  FILLER                  PICTURE X(3).
           05  FILLER REDEFINES PRIDAT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(15).
               10  PRRAB3-IO.
                   15  PRRAB3              PICTURE S9(2)V9(1).
           05  FRADAT-XX REDEFINES PRIDAT-XX-DATA-FIELDS.
               10  PNR                     PICTURE X(4).
               10  FILLER                  PICTURE X(14).
           05  FILLER REDEFINES PRIDAT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(4).
               10  FRAPRO-IO.
                   15  FRAPRO              PICTURE S9(1)V9(1).
               10  FILLER                  PICTURE X(12).
      *DSDS: DATA STRUCTURE FIELDS
           05  FRADAT-XX-DATA-FIELDS.
               10  PNR                     PICTURE X(4).
               10  FILLER                  PICTURE X(2).
           05  FILLER REDEFINES FRADAT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(4).
               10  FRAPRO-IO.
                   15  FRAPRO              PICTURE S9(1)V9(1).
           05  FAFILE-LEVEL-01.
               10  FAFILE-01-L2.
                   15  FAFILE-01-L2-FIRMNR PICTURE X(3).
               10  FAFILE-01-L1.
                   15  FAFILE-01-L1-FONR   PICTURE X(6).
           05  FAFILE-DATA-FIELDS.
               10  REC1                    PICTURE X(200).
               10  FIRMNR                  PICTURE X(3).
               10  FONR                    PICTURE X(6).
           05  FAFILE-MP                   PICTURE X(9).
           05  FAFILE-MC                   PICTURE X(9).
           05  FAFILE-M-01             REDEFINES FAFILE-MC.
               10  FAFILE-M-01-M2.
                   15  FAFILE-M-01-M2-FIRMNR-G.
                       20  FAFILE-M-01-M2-FIRMNR PICTURE X(3).
               10  FAFILE-M-01-M1.
                   15  FAFILE-M-01-M1-FONR-G.
                       20  FAFILE-M-01-M1-FONR PICTURE X(6).
           05  ORDFAKT-LEVEL-02.
               10  ORDFAKT-02-L2.
                   15  ORDFAKT-02-L2-FIRMNR PICTURE X(3).
               10  ORDFAKT-02-L1.
                   15  ORDFAKT-02-L1-ONR   PICTURE X(6).
           05  ORDFAKT-DATA-FIELDS.
               10  ORDTYP                  PICTURE X(1).
               10  ONR                     PICTURE X(6).
               10  KNR1                    PICTURE X(6).
               10  KNR1F                   PICTURE X(1).
               10  KNR2F                   PICTURE X(2).
               10  DIRREG                  PICTURE X(1).
               10  AVGF                    PICTURE X(1).
               10  LAGERK                  PICTURE X(2).
               10  BKKODE                  PICTURE X(1).
               10  BETBET                  PICTURE X(2).
               10  OGEBYR                  PICTURE X(1).
               10  AVD                     PICTURE X(1).
               10  KRETYP                  PICTURE X(1).
               10  SERVT                   PICTURE X(1).
               10  PRITYP                  PICTURE X(1).
               10  FAKTM                   PICTURE X(1).
               10  ODATO                   PICTURE X(6).
               10  OM                      PICTURE X(2).
               10  OMND                    PICTURE X(2).
               10  TYPE-X                  PICTURE X(1).
               10  BSFREK                  PICTURE X(1).
               10  KUNKAT                  PICTURE X(3).
               10  KAT1                    PICTURE X(1).
               10  RORRUT                  PICTURE X(6).
               10  HND                     PICTURE X(3).
               10  FAKREF                  PICTURE X(6).
               10  REKVNR                  PICTURE X(15).
               10  FORS2F                  PICTURE X(2).
               10  FORS3F                  PICTURE X(3).
               10  FORS5F                  PICTURE X(5).
               10  POSTNR                  PICTURE X(4).
               10  KADR4                   PICTURE X(4).
               10  VADR1                   PICTURE X(30).
               10  VADR2                   PICTURE X(30).
               10  VADR3                   PICTURE X(30).
               10  PAADR3                  PICTURE X(4).
               10  VADR4                   PICTURE X(20).
               10  VADPNR                  PICTURE X(4).
               10  POSNR                   PICTURE X(3).
               10  BESANT-IO.
                   15  BESANT              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  RESANT-IO.
                   15  RESANT              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  LEVANT-IO.
                   15  LEVANT              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ALFAK                   PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  ARTN6F                  PICTURE X(6).
               10  ART16F                  PICTURE X(16).
               10  ART4S                   PICTURE X(4).
               10  VAREB                   PICTURE X(30).
               10  TEKST1                  PICTURE X(8).
               10  TEKST2                  PICTURE X(8).
               10  TEKST3                  PICTURE X(8).
               10  TEKST4                  PICTURE X(8).
               10  TEKST5                  PICTURE X(8).
               10  TEKST6                  PICTURE X(8).
               10  TEKST                   PICTURE X(50).
               10  BELO-ELGP-IO.
                   15  BELO-ELGP           PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  EDBNR-IO.
                   15  EDBNR               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  RAB1-IO.
                   15  RAB1                PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  RAB2-IO.
                   15  RAB2                PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  RAB3-IO.
                   15  RAB3                PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  VGR-IO.
                   15  VGR                 PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  SVS-IO.
                   15  SVS                 PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  NPT-IO.
                   15  NPT                 PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  VLKOD1                  PICTURE X(1).
               10  PRISTT                  PICTURE X(1).
           05  ORDFAKT-MP                  PICTURE X(9).
           05  ORDFAKT-MC                  PICTURE X(9).
           05  ORDFAKT-M-02            REDEFINES ORDFAKT-MC.
               10  ORDFAKT-M-02-M2.
                   15  ORDFAKT-M-02-M2-FIRMNR-G.
                       20  ORDFAKT-M-02-M2-FIRMNR PICTURE X(3).
               10  ORDFAKT-M-02-M1.
                   15  ORDFAKT-M-02-M1-ONR-G.
                       20  ORDFAKT-M-02-M1-ONR PICTURE X(6).
           05  FIRMAF-DATA-FIELDS.
               10  FINAVN                  PICTURE X(30).
               10  FGEB                    PICTURE X(1).
               10  FGEBNR-IO.
                   15  FGEBNR              PICTURE S9(7).
               10  FAKALF                  PICTURE X(1).
               10  KPRIS                   PICTURE X(1).
           05  ORDNRM-DATA-FIELDS.
               10  ONRTYP                  PICTURE X(1).
           05  VAREMAS-DATA-FIELDS.
               10  VMSTAT                  PICTURE X(3).
           05  AVSTEMF-DATA-FIELDS.
               10  ORDFAK-IO.
                   15  ORDFAK              PICTURE S9(6).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  ORDSUM-IO.
                   15  ORDSUM              PICTURE S9(7)V9(2).
               10  BESSUM-IO.
                   15  BESSUM              PICTURE S9(7)V9(2).
               10  FROSUM-IO.
                   15  FROSUM              PICTURE S9(7)V9(2).
               10  TOTNYF-IO.
                   15  TOTNYF              PICTURE S9(7)V9(2).
               10  RABGEB-IO.
                   15  RABGEB              PICTURE S9(7)V9(2).
               10  FAKKRN                  PICTURE X(1).
               10  FAKTYP                  PICTURE X(1).
               10  AOL2G-IO.
                   15  AOL2G               PICTURE S9(5).
               10  AOLRG-IO.
                   15  AOLRG               PICTURE S9(5).
               10  ARL2G-IO.
                   15  ARL2G               PICTURE S9(6).
               10  ARLRG-IO.
                   15  ARLRG               PICTURE S9(6).
               10  ARL2-IO.
                   15  ARL2                PICTURE S9(6).
               10  ARLR-IO.
                   15  ARLR                PICTURE S9(6).
               10  AOL2N-IO.
                   15  AOL2N               PICTURE S9(5).
               10  AOLRN-IO.
                   15  AOLRN               PICTURE S9(5).
               10  ARL2N-IO.
                   15  ARL2N               PICTURE S9(6).
               10  ARLRN-IO.
                   15  ARLRN               PICTURE S9(6).
               10  KNR                     PICTURE X(6).
               10  FNR                     PICTURE X(3).
               10  NULANT-IO.
                   15  NULANT              PICTURE S9(5)V9(2).
               10  NULL-X-IO.
                   15  NULL-X              PICTURE S9(7)V9(2).
               10  AVGKOD                  PICTURE X(1).
               10  AVGTYP                  PICTURE X(1).
               10  ANTFRF-IO.
                   15  ANTFRF              PICTURE S9(5).
               10  ANTLAN-IO.
                   15  ANTLAN              PICTURE S9(5).
               10  ANTKAN-IO.
                   15  ANTKAN              PICTURE S9(5).
               10  BELO-ELGP-N-IO.
                   15  BELO-ELGP-N         PICTURE S9(7)V9(2).
               10  RAB1-N-IO.
                   15  RAB1-N              PICTURE S9(2)V9(1).
               10  RAB2-N-IO.
                   15  RAB2-N              PICTURE S9(2)V9(1).
               10  RAB3-N-IO.
                   15  RAB3-N              PICTURE S9(2)V9(1).
               10  VGRFAK                  PICTURE X(5).
               10  VGR-N-IO.
                   15  VGR-N               PICTURE S9(5).
               10  VGRAVD                  PICTURE X(1).
               10  ARL2F-IO.
                   15  ARL2F               PICTURE S9(5).
               10  ARLRF-IO.
                   15  ARLRF               PICTURE S9(5).
               10  FELT92-IO.
                   15  FELT92              PICTURE S9(7)V9(2).
               10  NYFRAK-IO.
                   15  NYFRAK              PICTURE S9(7)V9(2).
               10  ANT1-IO.
                   15  ANT1                PICTURE S9(5)V9(2).
               10  ANTFRB-IO.
                   15  ANTFRB              PICTURE S9(5).
               10  AOL2-IO.
                   15  AOL2                PICTURE S9(5).
               10  AOLR-IO.
                   15  AOLR                PICTURE S9(5).
               10  GEBANT-IO.
                   15  GEBANT              PICTURE S9(5)V9(2).
               10  FNRONR                  PICTURE X(9).
               10  EDBNRN-IO.
                   15  EDBNRN              PICTURE S9(7).
               10  EDBNRA                  PICTURE X(7).
               10  FNRENR                  PICTURE X(10).
               10  LINSUM-IO.
                   15  LINSUM              PICTURE S9(7)V9(2).
               10  SUM-X-IO.
                   15  SUM-X               PICTURE S9(10)V9(2).
               10  NPTSUM-IO.
                   15  NPTSUM              PICTURE S9(7)V9(2).
               10  OSKKEY                  PICTURE X(7).
               10  PRITIL-IO.
                   15  PRITIL              PICTURE S9(7)V9(2).
               10  SVSTIL-IO.
                   15  SVSTIL              PICTURE S9(7)V9(2).
               10  VGRTIL                  PICTURE X(5).
               10  AVSKEY                  PICTURE X(3).
               10  FRPROS-IO.
                   15  FRPROS              PICTURE S9(1)V9(1).
               10  VGRNUM-IO.
                   15  VGRNUM              PICTURE S9(5).
               10  VGRALF                  PICTURE X(5).
               10  VGR1F                   PICTURE X(1).
               10  SUMG15                  PICTURE X(1).
               10  SUMG05                  PICTURE X(1).
               10  SUMG50                  PICTURE X(1).
               10  ORDFGR-IO.
                   15  ORDFGR              PICTURE S9(7)V9(2).
           05  EDITTING-FIELDS.
               10  XO-52P-EF.
                 15  XO-52P                PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-72P-EF.
                 15  XO-72P                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-70D                  PICTURE S9(7).
               10  XO-70U                  PICTURE 9(7).
               10  XO-21D                  PICTURE S9(2)V9(1).
               10  XO-21U                  PICTURE 9(2)V9(1).
               10  XO-72D                  PICTURE S9(7)V9(2).
               10  XO-72U                  PICTURE 9(7)V9(2).
               10  XO-11YY9                PICTURE Z,9.
               10  XO-72YY9R               PICTURE Z.ZZZ.ZZZ,99-.
               10  XO-72YY9                PICTURE Z.ZZZ.ZZZ,99.
               10  XO-50YY9                PICTURE ZZ.ZZ9.
               10  XO-60YY9                PICTURE ZZZ.ZZ9.
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
               88  NOT-CALL-MATCH-RECS     VALUE '0'.
               88  CALL-MATCH-RECS         VALUE '1'.
           05  FILLER                      PICTURE X.
               88  NOT-SET-I-MR            VALUE '0'.
               88  SET-I-MR                VALUE '1'.
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
           SET NOT-I-05                    TO TRUE
           SET NOT-I-04                    TO TRUE
           SET NOT-I-06                    TO TRUE
           SET NOT-I-10                    TO TRUE
           SET NOT-I-10                    TO TRUE
           SET NOT-I-10                    TO TRUE
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  FAFILE-PROCESS
               SET FAFILE-PROCESS-OFF      TO TRUE
               SET FAFILE-READ             TO TRUE
           END-IF
 
           IF  FAFILE-READ
               PERFORM FAFILE-GET
               SET FAFILE-READ-OFF         TO TRUE
               IF  NOT FAFILE-EOF
                   PERFORM FAFILE-MATCH-SET
               END-IF
           END-IF
 
           IF  ORDFAKT-PROCESS
               SET ORDFAKT-PROCESS-OFF     TO TRUE
               SET ORDFAKT-READ            TO TRUE
           END-IF
 
           IF  ORDFAKT-READ
               PERFORM ORDFAKT-GET
               SET ORDFAKT-READ-OFF        TO TRUE
               IF  NOT ORDFAKT-EOF
                   PERFORM ORDFAKT-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM ORDFAKT-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  FAFILE-PROCESS
               PERFORM FAFILE-IDSET
           END-IF
 
           IF  ORDFAKT-PROCESS
               PERFORM ORDFAKT-IDSET
           END-IF
 
           IF  FAFILE-PROCESS
               PERFORM FAFILE-CHK-LEVEL
           END-IF
 
           IF  ORDFAKT-PROCESS
               PERFORM ORDFAKT-CHK-LEVEL
           END-IF
 
           IF I-1ST
               GO TO LR-INDICATOR-TEST
           END-IF.
 
       MAINLINE-TOTAL-CALCS.
           PERFORM TOTAL-CALCS
           IF  I-LR
               PERFORM LR-CALCS
           END-IF
           PERFORM TOTAL-OUTPUT.
 
       LR-INDICATOR-TEST.
           IF  I-LR
               GO TO MAINLINE-TERMINATION
           END-IF
           PERFORM DETAIL-OVERFLOW
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  FAFILE-PROCESS
               PERFORM FAFILE-FLDSET
           END-IF
 
           IF  ORDFAKT-PROCESS
               PERFORM ORDFAKT-FLDOFF
               PERFORM ORDFAKT-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  FAFILE-PROCESS
           OR  ORDFAKT-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-64                    TO TRUE
           SET NOT-I-67                    TO TRUE
           SET NOT-I-88                    TO TRUE
           IF  (I-L2)
               MOVE FIRMNR                 TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-16                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-16            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
               SET NOT-I-60                TO TRUE
               IF  KPRIS = 'P'
                   SET I-60                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-60)
               SET NOT-I-60                TO TRUE
               IF  KPRIS = ' '
                   SET I-60                TO TRUE
               END-IF
      * **  test på foma fjernet 5.01.12 sonefrakt
      *  L2      FIRMNR    COMP "913"                    93 FOMA
           END-IF
           IF  (I-L2)
               SET NOT-I-93                TO TRUE
               IF  FIRMNR = 'XYZ'
                   SET I-93                TO TRUE
               END-IF
               SET NOT-I-94                TO TRUE
               IF  FIRMNR = 'XYZ'
                   SET I-94                TO TRUE
               END-IF
               SET NOT-I-98                TO TRUE
               IF  FIRMNR = '918'
                   SET I-98                TO TRUE
               END-IF
               SET NOT-I-41                TO TRUE
               IF  FIRMNR = '764'
                   SET I-41                TO TRUE
               END-IF
               SET NOT-I-57                TO TRUE
               IF  FAKALF = 'J'
                   SET I-57                TO TRUE
               END-IF
           END-IF
           IF  (I-L1)
               SUBTRACT ORDSUM             FROM ORDSUM
               SUBTRACT BESSUM             FROM BESSUM
               SUBTRACT FROSUM             FROM FROSUM
           END-IF
           IF  (I-L2)
               SUBTRACT TOTNYF             FROM TOTNYF
           END-IF
           IF  (I-L1)
               SUBTRACT RABGEB             FROM RABGEB
               SET NOT-I-99                TO TRUE
               IF  ORDTYP = '9'
                   SET I-99                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-99)
               MOVE '2'                    TO FAKKRN
           END-IF
           IF  (I-L1 AND NOT-I-99)
               MOVE '1'                    TO FAKKRN
           END-IF
           IF  (I-L1 AND I-99)
               MOVE 'K'                    TO FAKTYP
           END-IF
           IF  (I-L1 AND NOT-I-99)
               MOVE 'F'                    TO FAKTYP
           END-IF
           IF  (I-L1)
               SET NOT-I-66                TO TRUE
               SET NOT-I-65                TO TRUE
               SET NOT-I-70                TO TRUE
               SET NOT-I-39                TO TRUE
               SET NOT-I-76                TO TRUE
               SET NOT-I-42                TO TRUE
               SET NOT-I-90                TO TRUE
               SET NOT-I-95                TO TRUE
           END-IF
           SET NOT-I-36                    TO TRUE
           SET NOT-I-37                    TO TRUE
           SET NOT-I-55                    TO TRUE
      *****************************************************************
      *   RUTINE FOR TELLING AV ANT. ORDRE OG ANTALL FAKTURARECORD.   *
      *   SAMT ANTALL RECORD FRA ORDRERUTINEN.                        *
      *****************************************************************
           IF  (I-L1 AND I-01)
               ADD 1                       TO AOL2G
               ADD 1                       TO AOLRG
           END-IF
           IF  (I-01)
               ADD 1                       TO ARL2G
               ADD 1                       TO ARLRG
               ADD 1                       TO ARL2
               ADD 1                       TO ARLR
           END-IF
           IF  (I-L1 AND I-02)
               ADD 1                       TO AOL2N
               ADD 1                       TO AOLRN
           END-IF
           IF  (NOT-I-01)
               ADD 1                       TO ARL2N
               ADD 1                       TO ARLRN
      *****************************************************************
           END-IF
           IF  (I-01)
               GO TO SLUTT-T
      *****************************************************************
      * HEADINGRECORD 1.                                              *
      *****************************************************************
           END-IF
           IF  (I-02)
               MOVE KNR1                   TO KNR
               MOVE FIRMNR                 TO FNR
               SET NOT-I-12                TO TRUE
               IF  TYPE-X = 'K'
                   SET I-12                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-12)
               SET NOT-I-13                TO TRUE
               IF  KRETYP = '2'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-12 AND NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KRETYP = '5'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-12 AND NOT-I-13)
               SET NOT-I-13                TO TRUE
               IF  KRETYP = '6'
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (I-02)
               SET NOT-I-51                TO TRUE
               IF  FNR = '950'
                   SET I-51                TO TRUE
               END-IF
               SET NOT-I-58                TO TRUE
               IF  FNR = '915'
                   SET I-58                TO TRUE
               END-IF
      *****************************************************************
      * TEST PÅ FIRMA SOM ØNSKER ALFAKODE SAMMEN MED ARTIKKELNR.      *
      * HUSK OGSÅ Å RETTE PROGRAM FAK710 (EDI-DATA TIL THORSHOV REKV.)*
      * DETTE LIGGER NÅ I FIRMAFILE FRA ORFI.                         *
      *****************************************************************
      *  02      FNR       COMP "970"                    57 AUTO-MATERIELL
      *  02N57   FNR       COMP "855"                    57 KOLBERG
      *  02N57   FNR       COMP "871"                    57 KOLBERG
      *  02N57   FNR       COMP "877"                    57 KOLBERG
      *  02N57   FNR       COMP "883"                    57 KOLBERG
      *  02N57   FNR       COMP "915"                    57 RODIN
      *****************************************************************
      * TEST PÅ FIRMA SOM ØNSKER FRAKTENDRINGER.                      *
      * KCL UTGÅR PR.15.01.07 METTE.
      *****************************************************************
           END-IF
           IF  (I-02)
               MOVE ' '                    TO SUMG05
               MOVE ' '                    TO SUMG15
               MOVE ' '                    TO SUMG50
               SET NOT-I-47                TO TRUE
               IF  FNR = '999'
                   SET I-47                TO TRUE
               END-IF
      *  02      FNR       COMP "855"                    47 KOLBERG
      *  02N47   FNR       COMP "871"                    47 KOLBERG
      *  02N47   FNR       COMP "877"                    47 KOLBERG
      *****************************************************************
           END-IF
           IF  (I-02)
               SET NOT-I-85                TO TRUE
               IF  FAKTM = '5'
                   SET I-85                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-85)
               SET NOT-I-85                TO TRUE
               IF  FAKTM = '6'
                   SET I-85                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-85)
               SET NOT-I-85                TO TRUE
               IF  FAKTM = '7'
                   SET I-85                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-85)
               SET NOT-I-85                TO TRUE
               IF  FAKTM = '8'
                   SET I-85                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-85)
               SET NOT-I-85                TO TRUE
               IF  FAKTM = '9'
                   SET I-85                TO TRUE
               END-IF
           END-IF
           IF  (I-02)
               PERFORM AVDRUT-S
      *****************************************************************
      * SKAL VAREADRESSERECORDS DANNES.                               *
      *****************************************************************
      *****************************************************************
           END-IF
           IF  (I-02)
               SET NOT-I-31                TO TRUE
               SET NOT-I-30                TO TRUE
               IF  KNR > '500180'
                   SET I-31                TO TRUE
               END-IF
               IF  KNR = '500180'
                   SET I-30                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-31)
               SET NOT-I-30                TO TRUE
               IF  KNR NOT > '500500'
                   SET I-30                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-85)
               SET I-30                    TO TRUE
           END-IF
           MOVE 0,00                       TO NULANT
           MOVE 0                          TO NULL-X
           IF  (I-02)
               SET NOT-I-21                TO TRUE
               IF  AVGF = '1'
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-21)
               MOVE '5'                    TO AVGKOD
           END-IF
           IF  (I-02 AND NOT-I-21)
               MOVE '6'                    TO AVGKOD
           END-IF
           IF  (I-02 AND I-21)
               MOVE 'F'                    TO AVGTYP
           END-IF
           IF  (I-02 AND NOT-I-21)
               MOVE 'P'                    TO AVGTYP
           END-IF
           IF  (I-02)
               SET NOT-I-77                TO TRUE
               IF  BKKODE = 'S'
                   SET I-77                TO TRUE
               END-IF
               PERFORM SUMKOD-S
           END-IF
           IF  (I-02)
               SET NOT-I-26                TO TRUE
               SET TESTN-STATE-INIT        TO TRUE
               PERFORM WITH TEST AFTER
                 VARYING TESTN-INDEX FROM 1 BY 1
                   UNTIL TESTN-INDEX = 1
                   EVALUATE TRUE
                   WHEN TESTN-STATE-INIT
                       IF  KRETYP (TESTN-INDEX:1) NUMERIC
                           SET TESTN-STATE-NUMBER TO TRUE
                       ELSE
                           IF  KRETYP (TESTN-INDEX:1) = SPACES
                               SET TESTN-STATE-SPACE TO TRUE
                           ELSE
                               SET TESTN-STATE-OTHER TO TRUE
                           END-IF
                       END-IF
                   WHEN TESTN-STATE-NUMBER
                       IF  KRETYP (TESTN-INDEX:1) NOT NUMERIC
                           SET TESTN-STATE-OTHER TO TRUE
                       END-IF
                   WHEN TESTN-STATE-SPACE
                       IF  KRETYP (TESTN-INDEX:1) NUMERIC
                           SET TESTN-STATE-LEADSPACE TO TRUE
                       ELSE
                           IF  KRETYP (TESTN-INDEX:1) NOT = SPACES
                               SET TESTN-STATE-OTHER TO TRUE
                           END-IF
                       END-IF
                   WHEN TESTN-STATE-LEADSPACE
                       IF  KRETYP (TESTN-INDEX:1) NOT NUMERIC
                           SET TESTN-STATE-OTHER TO TRUE
                       END-IF
                   END-EVALUATE
               END-PERFORM
               IF  TESTN-STATE-NUMBER
                   SET I-26                TO TRUE
               END-IF
               SET NOT-I-23                TO TRUE
               IF  LAGERK > '  '
                   SET I-23                TO TRUE
               END-IF
               SET NOT-I-24                TO TRUE
               IF  BKKODE > ' '
                   SET I-24                TO TRUE
               END-IF
      *****************************************************************
      * HEADINGRECORD 2  HND,FAKTURAREF, REKV.NR.                     *
      *****************************************************************
           END-IF
           IF  (I-05)
               SET NOT-I-28                TO TRUE
               IF  RORRUT > '        '
                   SET I-28                TO TRUE
               END-IF
               SET NOT-I-27                TO TRUE
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
                   SET I-27                TO TRUE
               END-IF
               SET NOT-I-22                TO TRUE
               IF  REKVNR > '        '
                   SET I-22                TO TRUE
               END-IF
               SET NOT-I-25                TO TRUE
               IF  HND > '   '
                   SET I-25                TO TRUE
               END-IF
               SET NOT-I-34                TO TRUE
               IF  POSTNR > '1299'
                   SET I-34                TO TRUE
               END-IF
               SET NOT-I-90                TO TRUE
               IF  FORS5F = 'HENTE'
                   SET I-90                TO TRUE
               END-IF
           END-IF
           IF  (I-05 AND NOT-I-90)
               SET NOT-I-90                TO TRUE
               IF  FORS5F = 'ER HE'
                   SET I-90                TO TRUE
               END-IF
           END-IF
           IF  (I-05 AND NOT-I-90)
               SET NOT-I-90                TO TRUE
               IF  FORS5F = 'ER UT'
                   SET I-90                TO TRUE
               END-IF
           END-IF
           IF  (I-05 AND NOT-I-90)
               SET NOT-I-90                TO TRUE
               IF  FORS3F = 'FLY'
                   SET I-90                TO TRUE
               END-IF
           END-IF
           IF  (I-05 AND NOT-I-90)
               SET NOT-I-90                TO TRUE
               IF  FORS2F = 'IL'
                   SET I-90                TO TRUE
               END-IF
      *****************************************************************
      * HEADINGRECORD 2 SKAL ORDREN HA BESTILLINGSGEBYR.              *
      *    KODE B SETTES INN I POS 36 I FAKREC.                       *
      *    DETTE GJELDER MANUELLE ORDRE FOR HAFNOR.                   *
      *    TEST PÅ KUNDEGRUPPE FOREGÅR I PROG. FAK056.                *
      *****************************************************************
           END-IF
           IF  (NOT-I-05)
               GO TO ENDMGB-T
           END-IF
           IF  (NOT-I-51)
               GO TO ENDMGB-T
           END-IF
           IF  (I-05 AND I-99)
               GO TO ENDMGB-T
           END-IF
           IF  (I-05 AND I-44)
               GO TO ENDMGB-T
           END-IF
           IF  (I-05)
               SET NOT-I-97                TO TRUE
               IF  OM = 'LD'
                   SET I-97                TO TRUE
               END-IF
           END-IF
           IF  (I-05 AND I-97)
               GO TO ENDMGB-T
           END-IF
           IF  (I-05)
               SET NOT-I-97                TO TRUE
               IF  DIRREG = 'J'
                   SET I-97                TO TRUE
               END-IF
           END-IF
           IF  (I-05 AND I-97)
               GO TO ENDMGB-T
           END-IF
           IF  (I-05)
               SET NOT-I-97                TO TRUE
               IF  BKKODE = 'H'
                   SET I-97                TO TRUE
               END-IF
           END-IF
           IF  (I-05 AND I-97)
               GO TO ENDMGB-T
           END-IF
           IF  (I-05)
               SET NOT-I-97                TO TRUE
               IF  BKKODE = 'R'
                   SET I-97                TO TRUE
               END-IF
           END-IF
           IF  (I-05 AND NOT-I-97)
               SET NOT-I-97                TO TRUE
               IF  RORRUT = '******'
                   SET I-97                TO TRUE
               END-IF
           END-IF
           IF  (I-05 AND NOT-I-97)
               SET NOT-I-97                TO TRUE
               IF  RORRUT > '000000'
                   SET I-97                TO TRUE
               END-IF
           END-IF
           IF  (I-05 AND I-97)
               GO TO ENDMGB-T
           END-IF
           IF  (I-05 AND I-51)
               SET I-95                    TO TRUE
           END-IF.
 
       ENDMGB-T.
           IF  (I-05)
               SET NOT-I-97                TO TRUE
      *****************************************************************
      *  HEADINGRECORD 3 VAREADRESSE                                  *
      *****************************************************************
           END-IF
           IF  (I-04)
               SET I-39                    TO TRUE
               SET NOT-I-11                TO TRUE
               IF  VADR1 > '        '
                   SET I-11                TO TRUE
               END-IF
               SET NOT-I-18                TO TRUE
               IF  VADR2 > '        '
                   SET I-18                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND I-11 AND NOT-I-30)
               OR  (I-04 AND I-18 AND NOT-I-30)
               SET I-36                    TO TRUE
           END-IF
           IF  (I-04)
               SET NOT-I-17                TO TRUE
               IF  VADR3 > '        '
                   SET I-17                TO TRUE
               END-IF
               SET NOT-I-19                TO TRUE
               IF  VADR4 > '        '
                   SET I-19                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND I-17 AND NOT-I-30)
               OR  (I-04 AND I-19 AND NOT-I-30)
               SET I-37                    TO TRUE
           END-IF
           IF  (I-04)
               SET NOT-I-81                TO TRUE
               SET NOT-I-82                TO TRUE
               SET NOT-I-83                TO TRUE
               SET NOT-I-84                TO TRUE
      *
           END-IF
           IF  (I-04 AND I-85 AND I-11)
               SET I-81                    TO TRUE
           END-IF
           IF  (I-04 AND I-85 AND I-18)
               SET I-82                    TO TRUE
           END-IF
           IF  (I-04 AND I-85 AND I-17)
               SET I-83                    TO TRUE
           END-IF
           IF  (I-04 AND I-85 AND I-19)
               SET I-84                    TO TRUE
      *****************************************************************
      * SJEKK OM FOMAKUNDEN SKAL HA FRAKTUTGEVNING. KUNDKAT 105       *
      *       DA SKAL EVENTUELL ANNEN FRAKTRECORD FJERNES.            *
      *       ORDREN SKAL SUMMERES OG BEREGNES FRAKT UTFRA %.         *
      *       FJERNET 12.06.2012                                      *
      *****************************************************************
           END-IF
           IF  (I-06 AND I-93 AND NOT-I-42)
               AND (NOT-I-12 AND NOT-I-90 AND NOT-I-94)
               PERFORM FRARUT-S
           END-IF
           IF  (I-06 AND I-93 AND NOT-I-42)
               AND (NOT-I-12 AND NOT-I-90 AND I-94)
               PERFORM FRARU2-S
           END-IF
           IF  (I-06)
               SET I-42                    TO TRUE
               SET NOT-I-86                TO TRUE
           END-IF
           IF  (I-06 AND I-93 AND I-76)
               SET NOT-I-86                TO TRUE
               IF  VGR = 99030
                   SET I-86                TO TRUE
               END-IF
           END-IF
           IF  (I-06 AND I-93 AND I-76)
               AND (I-86)
               ADD 1                       TO ANTFRF
           END-IF
           IF  (I-06)
               SET NOT-I-14                TO TRUE
           END-IF
           IF  (I-06 AND I-08 AND I-09)
               SET I-14                    TO TRUE
      *****
           END-IF
           IF  (I-06 AND I-14)
               SET NOT-I-15                TO TRUE
               IF  TEKST1 > '        '
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-06 AND I-14 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  TEKST2 > '        '
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-06 AND I-14 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  TEKST3 > '        '
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-06 AND I-14 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  TEKST4 > '        '
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-06 AND I-14 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  TEKST5 > '        '
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-06 AND I-14 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  TEKST6 > '        '
                   SET I-15                TO TRUE
               END-IF
      *****************************************************************
      * TEST OM DET ER VANLIG SALG AV VARE SOM SKAL HA ALFAKODE SOM   *
      * DEL AV ARTIKKELNR.                                            *
      *****************************************************************
           END-IF
           IF  (I-06)
               SET NOT-I-69                TO TRUE
               SET NOT-I-68                TO TRUE
           END-IF
           IF  (I-06 AND NOT-I-14)
               SET NOT-I-68                TO TRUE
               IF  EDBNR > 0
                   SET I-68                TO TRUE
               END-IF
           END-IF
           IF  (I-06 AND NOT-I-14 AND I-68)
               SET NOT-I-69                TO TRUE
               IF  EDBNR < 9000000
                   SET I-69                TO TRUE
               END-IF
           END-IF
           IF  (I-06 AND I-69 AND I-57)
               AND (NOT-I-59)
               ADD 1                       TO ANTLAN
           END-IF
           IF  (I-06 AND I-69 AND I-57)
               AND (I-59)
               ADD 1                       TO ANTKAN
           END-IF
           IF  (I-06)
               SET NOT-I-29                TO TRUE
               IF  ALFAK = '***'
                   SET I-29                TO TRUE
               END-IF
           END-IF
           IF  (I-06 AND I-29)
               SET I-14                    TO TRUE
               SET NOT-I-15                TO TRUE
      *****************************************************************
      * ER BELØPET 0,01 SKAL DETTE SETTES TIL 0,00                    *
      * DETTE ØNSKES AV BEMA.                                         *
      *****************************************************************
           END-IF
           IF  (I-06 AND I-41 AND NOT-I-09)
               SET NOT-I-09                TO TRUE
               IF  BELO-ELGP = 0,01
                   SET I-09                TO TRUE
               END-IF
           END-IF
           IF  (I-06 AND I-41 AND I-09)
               MOVE 0,00                   TO BELO-ELGP
      *****************************************************************
      * OMREGNING FRA ORDREPRIS OG RABATT TIL NETTOPRIS.              *
      * S.OG B. (918) ØNSKER DETTE PÅ 115115 TOYOTA.                  *
      *****************************************************************
           END-IF
           IF  (NOT-I-06)
               GO TO ENDNPR-T
           END-IF
           IF  (I-06 AND NOT-I-98)
               GO TO ENDNPR-T
           END-IF
           IF  (I-06)
               SET NOT-I-96                TO TRUE
               IF  KNR = '115115'
                   SET I-96                TO TRUE
               END-IF
           END-IF
           IF  (I-06 AND NOT-I-96)
               GO TO ENDNPR-T
           END-IF
           IF  (I-06)
               SET NOT-I-96                TO TRUE
               IF  PRITYP = 'O'
                   SET I-96                TO TRUE
               END-IF
           END-IF
           IF  (I-06 AND NOT-I-96)
               GO TO ENDNPR-T
           END-IF
           MOVE BELO-ELGP                  TO BELO-ELGP-N
           MOVE BELO-ELGP-N-IO             TO PRBEL-IO
           MOVE RAB1                       TO RAB1-N
           MOVE RAB1-N-IO                  TO PRRAB1-IO
           MOVE RAB2                       TO RAB2-N
           MOVE RAB2-N-IO                  TO PRRAB2-IO
           MOVE RAB3                       TO RAB3-N
           MOVE RAB3-N-IO                  TO PRRAB3-IO
           CALL 'NETTOPRI' USING PRIDAT-XX-DATA-FIELDS
           ADD PRBEL TO ZERO           GIVING BELO-ELGP
           MOVE 0,0                        TO RAB1
           MOVE 0,0                        TO RAB2
           MOVE 0,0                        TO RAB3.
 
       ENDNPR-T.
      *****************************************************************
           IF  (I-06)
               SET NOT-I-20                TO TRUE
               SET NOT-I-40                TO TRUE
               MOVE VGR                    TO VGR-N
               MOVE VGR-N-IO               TO VGRFAK
      ** MLLzo
               MOVE '1'                    TO BW-A-2
               MULTIPLY 16                 BY BW-A
               MOVE VGRFAK (5:1)           TO BW-B-2
               MULTIPLY 16                 BY BW-B
               MOVE BW-A-1                 TO BW-B-1
               DIVIDE 16                   INTO BW-B
               MOVE BW-B-2                 TO VGRFAK (5:1)
               MOVE VGRFAK (1:1)           TO VGRAVD
               SET NOT-I-56                TO TRUE
               IF  VGRFAK = '00000'
                   SET I-56                TO TRUE
               END-IF
           END-IF
           IF  (I-06 AND I-56)
               MOVE '     '                TO VGRFAK
           END-IF
           IF  (I-06 AND NOT-I-14)
               SET NOT-I-20                TO TRUE
               IF  LEVANT > 0,00
                   SET I-20                TO TRUE
               END-IF
           END-IF
           IF  (I-06 AND I-93 AND I-86)
               SET NOT-I-20                TO TRUE
           END-IF
           IF  (I-06 AND NOT-I-14 AND I-U1)
               SET NOT-I-40                TO TRUE
               IF  LEVANT = 0,00
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (I-06 AND I-60 AND NOT-I-07)
               AND (I-20 AND NOT-I-14 AND NOT-I-12)
               PERFORM PANTER-S
           END-IF
           IF  (I-06 AND I-60 AND NOT-I-07)
               AND (I-20 AND NOT-I-14 AND I-12)
               AND (I-13)
               PERFORM PANTER-S
           END-IF
           IF  (I-06 AND I-55)
               ADD 1                       TO ARL2F
               ADD 1                       TO ARLRF
      ******************************************************
           END-IF
           IF  (I-04 AND I-36)
               ADD 1                       TO ARL2
           END-IF
           IF  (I-04 AND I-37)
               ADD 1                       TO ARL2
           END-IF
           IF  (I-04 AND I-81)
               ADD 1                       TO ARL2
           END-IF
           IF  (I-04 AND I-82)
               ADD 1                       TO ARL2
           END-IF
           IF  (I-04 AND I-83)
               ADD 1                       TO ARL2
           END-IF
           IF  (I-04 AND I-84)
               ADD 1                       TO ARL2
           END-IF
           IF  (I-06 AND NOT-I-14 AND I-20)
               ADD 1                       TO ARL2
           END-IF
           IF  (I-06 AND NOT-I-14 AND I-40)
               ADD 1                       TO ARL2
           END-IF
           IF  (I-06 AND I-55)
               ADD 1                       TO ARL2
           END-IF
           IF  (I-06 AND I-14 AND I-15)
               ADD 1                       TO ARL2
      ******************************************************
           END-IF
           IF  (I-04 AND I-36)
               ADD 1                       TO ARLR
           END-IF
           IF  (I-04 AND I-37)
               ADD 1                       TO ARLR
           END-IF
           IF  (I-04 AND I-81)
               ADD 1                       TO ARLR
           END-IF
           IF  (I-04 AND I-82)
               ADD 1                       TO ARLR
           END-IF
           IF  (I-04 AND I-83)
               ADD 1                       TO ARLR
           END-IF
           IF  (I-04 AND I-84)
               ADD 1                       TO ARLR
           END-IF
           IF  (I-06 AND NOT-I-14 AND I-20)
               ADD 1                       TO ARLR
           END-IF
           IF  (I-06 AND NOT-I-14 AND I-40)
               ADD 1                       TO ARLR
           END-IF
           IF  (I-06 AND I-55)
               ADD 1                       TO ARLR
           END-IF
           IF  (I-06 AND I-14 AND I-15)
               ADD 1                       TO ARLR
      ******************************************************
           END-IF
           IF  (I-06 AND NOT-I-35 AND NOT-I-14)
               PERFORM ORDTOT-S
           END-IF
           IF  (I-06 AND I-93 AND I-76)
               AND (NOT-I-14 AND NOT-I-86)
               PERFORM FRORUT-S
           END-IF
           IF  (I-06 AND I-47 AND NOT-I-12)
               SET NOT-I-64                TO TRUE
               IF  POSNR = '999'
                   SET I-64                TO TRUE
               END-IF
           END-IF
           IF  (I-06 AND I-47)
               SET NOT-I-67                TO TRUE
               IF  ARTN6F = 'FRAKT  '
                   SET I-67                TO TRUE
               END-IF
           END-IF
           IF  (I-06 AND I-47 AND NOT-I-67)
               SET NOT-I-64                TO TRUE
           END-IF
           IF  (I-06)
               SET NOT-I-67                TO TRUE
      *****************************************************************
           END-IF
           IF  (I-06 AND I-47 AND NOT-I-14)
               AND (NOT-I-12 AND NOT-I-64)
               PERFORM KCLTOT-S
           END-IF
           IF  (I-06 AND I-47 AND NOT-I-12)
               AND (I-64)
               PERFORM KCLFRA-S
           END-IF
           IF  (I-06 AND I-47 AND NOT-I-12)
               AND (I-64)
               SUBTRACT 1                  FROM ARL2
               SUBTRACT 1                  FROM ARLR
           END-IF.
 
       SLUTT-T.
      *****************************************************************
      * RUTINE FOR Å BEREGNE FRAKTUTGEVNING FOMA.                     *
      *****************************************************************
           CONTINUE.
 
       AVDRUT-S SECTION.
       AVDRUT-S-P.
           SET NOT-I-50                    TO TRUE
           IF  FIRMNR = '918'
               SET I-50                    TO TRUE
           END-IF
           IF  (I-50)
               GO TO ENDAVD-T
      ******************************************************
      *          FIRMNR    COMP "970"                    50 AUTOMATRIELL
      *  50      AVD       COMP "1"                  5252   ULIK
      *  50 52             MOVE "1"       AVD               RETT AVD.
      *  50 52             GOTO ENDAVD
      ******************************************************
           END-IF
           SET NOT-I-52                    TO TRUE
           IF  AVD < '0'
               SET I-52                    TO TRUE
           END-IF
           SET NOT-I-53                    TO TRUE
           IF  AVD = 'A'
               SET I-53                    TO TRUE
           END-IF
           IF  (NOT-I-53)
               SET NOT-I-53                TO TRUE
               IF  AVD = 'B'
                   SET I-53                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-53)
               SET NOT-I-53                TO TRUE
               IF  AVD = 'C'
                   SET I-53                TO TRUE
               END-IF
           END-IF
           IF  (I-52 AND NOT-I-53)
               MOVE '1'                    TO AVD
           END-IF.
 
       ENDAVD-T.
           CONTINUE.
      ***********************************************************
      * SUBRUTINE FOR ORDRESUMKODE HÅNTERING.                   *
      * KONTANT OG OPPKRAV SKAL ALDRI HA GEBYR. 8/9-88 EL.      *
      * FIRMA 918 SKAL IKKE HA GEBYR PÅ BET.MÅTE 22.  25/8-94   *
      * FIRMA 950 HAR SPESIALRUTINE FOR GEBYRKODE B OG H.       *
      ***********************************************************
 
       SUMKOD-S SECTION.
       SUMKOD-S-P.
           SET NOT-I-35                    TO TRUE
           SET NOT-I-66                    TO TRUE
           SET NOT-I-70                    TO TRUE
           SET NOT-I-78                    TO TRUE
           SET NOT-I-79                    TO TRUE
           SET NOT-I-44                    TO TRUE
           IF  BETBET = '07'
               SET I-44                    TO TRUE
           END-IF
           IF  (NOT-I-44)
               SET NOT-I-44                TO TRUE
               IF  BETBET = '47'
                   SET I-44                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-44)
               SET NOT-I-44                TO TRUE
               IF  BETBET = '14'
                   SET I-44                TO TRUE
               END-IF
           END-IF
           IF  (I-44)
               MOVE '1'                    TO OGEBYR
           END-IF
           SET NOT-I-80                    TO TRUE
           IF  FIRMNR = '950'
               SET I-80                    TO TRUE
           END-IF
           SET NOT-I-48                    TO TRUE
           IF  FIRMNR = '918'
               SET I-48                    TO TRUE
           END-IF
           SET NOT-I-38                    TO TRUE
           IF  FIRMNR = '912'
               SET I-38                    TO TRUE
           END-IF
           IF  (I-48)
               SET NOT-I-44                TO TRUE
               IF  BETBET = '22'
                   SET I-44                TO TRUE
               END-IF
           END-IF
           IF  (I-48 AND I-44)
               MOVE '1'                    TO OGEBYR
           END-IF
           SET NOT-I-32                    TO TRUE
           IF  OGEBYR > ' '
               SET I-32                    TO TRUE
           END-IF
           IF  (NOT-I-32)
               MOVE FGEB                   TO OGEBYR
           END-IF
           SET NOT-I-33                    TO TRUE
           IF  OGEBYR = 'J'
               SET I-33                    TO TRUE
           END-IF
           IF  (I-33)
               MOVE 'G'                    TO OGEBYR
           END-IF
           IF  (I-12)
               MOVE 'N'                    TO OGEBYR
           END-IF
           IF  (I-58 AND I-77)
               MOVE 'N'                    TO OGEBYR
           END-IF
           SET NOT-I-35                    TO TRUE
           IF  OGEBYR = '1'
               SET I-35                    TO TRUE
           END-IF
           IF  (NOT-I-35)
               SET NOT-I-35                TO TRUE
               IF  OGEBYR = 'N'
                   SET I-35                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-35)
               SET NOT-I-35                TO TRUE
               IF  OGEBYR = 'F'
                   SET I-35                TO TRUE
               END-IF
           END-IF
           SET NOT-I-66                    TO TRUE
           IF  KNR1F = '2'
               SET I-66                    TO TRUE
           END-IF
           IF  (NOT-I-66)
               SET NOT-I-66                TO TRUE
               IF  KNR1F = '5'
                   SET I-66                TO TRUE
               END-IF
      *****************************************************************
      * SPESIALGEBYR HYDRO/TEXACO OG BEST-KJEDEN FOR HAFNOR.          *
      *    LAGET 4.4.2001                                             *
      * ORDRE MED GEBYRKODE B OG H, SKAL HA GEBYR OM DETTE ER ORDRE   *
      *       FRA ORRE OG DET ER SPESIELLE VARER (STATKODE TEST)      *
      *****************************************************************
           END-IF
           IF  (NOT-I-80)
               GO TO SKEND-T
           END-IF
           IF  (I-80)
               SET NOT-I-78                TO TRUE
               IF  OGEBYR = 'B'
                   SET I-78                TO TRUE
               END-IF
               SET NOT-I-79                TO TRUE
               IF  OGEBYR = 'H'
                   SET I-79                TO TRUE
               END-IF
           END-IF
           IF  (I-80 AND I-79)
               MOVE 'T'                    TO OGEBYR
           END-IF
           IF  (NOT-I-78 AND NOT-I-79)
               GO TO SKEND-T
           END-IF
           MOVE FIRMNR                     TO FNRONR (1:3)
           MOVE ONR                        TO FNRONR (4:6)
           MOVE FNRONR                     TO ORDNRM-KEY1
           READ ORDNRM RECORD KEY IS ORDNRM-KEY1
           INVALID KEY
               SET I-16                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-16                TO TRUE
               PERFORM ORDNRM-FLDSET
               PERFORM ORDNRM-IDSET
           END-READ
           IF  (NOT-I-16)
               SET NOT-I-46                TO TRUE
               IF  ONRTYP = 'O'
                   SET I-46                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-16 AND NOT-I-46)
               SET I-35                    TO TRUE
           END-IF
           IF  (I-16)
               SET I-35                    TO TRUE
           END-IF
           IF  (I-35)
               SET NOT-I-78                TO TRUE
               SET NOT-I-79                TO TRUE
           END-IF.
 
       SKEND-T.
           CONTINUE.
      ******************************************************
      * SUBRUTINE FOR ORDRESUMMERING FOR TILDELING AV      *
      * EKSPEDISJONSGEBYR OG ORDRESUMRABATT.               *
      ******************************************************
 
       ORDTOT-S SECTION.
       ORDTOT-S-P.
           SET NOT-I-46                    TO TRUE
           SET NOT-I-61                    TO TRUE
           IF  LEVANT < BESANT
               SET I-61                    TO TRUE
           END-IF
           IF  (I-61 AND NOT-I-78 AND NOT-I-79)
               SET I-66                    TO TRUE
           END-IF
           SET NOT-I-62                    TO TRUE
           IF  EDBNR = FGEBNR
               SET I-62                    TO TRUE
           END-IF
           IF  (I-62)
               SET I-66                    TO TRUE
               GO TO TOTEND-T
      *****************************************************************
      * SPESIALRUTINE HAFNOR. HAR VAREN STATKODE SOM TILSIER ATT DET  *
      *       SKAL VÆRE GEBYR.                                        *
      * FJERNET NOEN STATKODER I FØLGE E-MAIL FRA VIDAR HAFNOR 6/4-01 *
      *****************************************************************
           END-IF
           IF  (NOT-I-78 AND NOT-I-79)
               GO TO SUMRUT-T
           END-IF
           ADD EDBNR TO ZERO           GIVING EDBNRN
           MOVE EDBNRN                     TO EDBNRA
      ** MLLzo
           MOVE '1'                        TO BW-A-2
           MULTIPLY 16                     BY BW-A
           MOVE EDBNRA (7:1)               TO BW-B-2
           MULTIPLY 16                     BY BW-B
           MOVE BW-A-1                     TO BW-B-1
           DIVIDE 16                       INTO BW-B
           MOVE BW-B-2                     TO EDBNRA (7:1)
           MOVE FIRMNR                     TO FNRENR (1:3)
           MOVE EDBNRA                     TO FNRENR (4:7)
           MOVE FNRENR                     TO VAREMAS-KEY1
           READ VAREMAS RECORD KEY IS VAREMAS-KEY1
           INVALID KEY
               SET I-16                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-16                TO TRUE
               PERFORM VAREMAS-FLDSET
               PERFORM VAREMAS-IDSET
           END-READ
           IF  (I-16)
               GO TO TOTEND-T
           END-IF
           SET NOT-I-46                    TO TRUE
           IF  VMSTAT = '200'
               SET I-46                    TO TRUE
           END-IF
           IF  (NOT-I-46)
               SET NOT-I-46                TO TRUE
               IF  VMSTAT = '201'
                   SET I-46                TO TRUE
               END-IF
      * N46      VMSTAT    COMP "204"                    46 GIR GEBYR.  .
      * N46      VMSTAT    COMP "205"                    46 GIR GEBYR.  .
      * N46      VMSTAT    COMP "206"                    46 GIR GEBYR.  .
      * N46      VMSTAT    COMP "207"                    46 GIR GEBYR.  .
      * N46      VMSTAT    COMP "208"                    46 GIR GEBYR.  .
      * N46      VMSTAT    COMP "210"                    46 GIR GEBYR.  .
      * N46      VMSTAT    COMP "211"                    46 GIR GEBYR.  .
      * N46      VMSTAT    COMP "212"                    46 GIR GEBYR.  .
      * N46      VMSTAT    COMP "213"                    46 GIR GEBYR.  .
      * N46      VMSTAT    COMP "214"                    46 GIR GEBYR.  .
      * N46      VMSTAT    COMP "220"                    46 GIR GEBYR.  .
      * N46      VMSTAT    COMP "222"                    46 GIR GEBYR.  .
      * N46      VMSTAT    COMP "223"                    46 GIR GEBYR.  .
      * N46      VMSTAT    COMP "224"                    46 GIR GEBYR.  .
      * N46      VMSTAT    COMP "225"                    46 GIR GEBYR.  .
      * N46      VMSTAT    COMP "226"                    46 GIR GEBYR.  .
      * N46      VMSTAT    COMP "227"                    46 GIR GEBYR.  .
      * N46      VMSTAT    COMP "228"                    46 GIR GEBYR.  .
           END-IF
           IF  (NOT-I-46)
               SET NOT-I-46                TO TRUE
               IF  VMSTAT = '250'
                   SET I-46                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-46)
               SET NOT-I-46                TO TRUE
               IF  VMSTAT = '256'
                   SET I-46                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-46)
               SET NOT-I-46                TO TRUE
               IF  VMSTAT = '258'
                   SET I-46                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-46)
               SET NOT-I-46                TO TRUE
               IF  VMSTAT = '259'
                   SET I-46                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-46)
               SET NOT-I-46                TO TRUE
               IF  VMSTAT = '270'
                   SET I-46                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-46)
               GO TO TOTEND-T
      *******   UTREGNING A ORDRESUM   ************
           END-IF
           .
 
       SUMRUT-T.
           SUBTRACT LINSUM                 FROM LINSUM
           IF  (I-20)
               MULTIPLY LEVANT BY BELO-ELGP GIVING LINSUM
           END-IF
           MULTIPLY RAB1 BY LINSUM     GIVING SUM-X
           DIVIDE SUM-X BY 100         GIVING SUM-X
           SUBTRACT SUM-X                  FROM LINSUM
           MULTIPLY RAB2 BY LINSUM     GIVING SUM-X
           DIVIDE SUM-X BY 100         GIVING SUM-X
           SUBTRACT SUM-X                  FROM LINSUM
           MULTIPLY RAB3 BY LINSUM     GIVING SUM-X
           DIVIDE SUM-X BY 100         GIVING SUM-X
           SUBTRACT SUM-X                  FROM LINSUM
           ADD LINSUM                      TO ORDSUM
           IF  (NOT-I-07 AND I-20)
               MULTIPLY LEVANT BY NPT  GIVING NPTSUM
               ADD NPTSUM                  TO ORDSUM
           END-IF
           SET I-65                        TO TRUE.
 
       TOTEND-T.
           CONTINUE.
      ******************************************************
      * SUBRUTINE FOR TILDELING.                           *
      * EKSPEDISJONSGEBYR OG ORDRESUMRABATT.               *
      ******************************************************
 
       TILRUT-S SECTION.
       TILRUT-S-P.
           MOVE 0                          TO KSEQ
           MOVE FIRMNR                     TO KFNR
           MOVE AVD                        TO KAVD
           MOVE OGEBYR                     TO KKODE
           IF  (I-38 AND NOT-I-34)
               MOVE 'A'                    TO KKODE
           END-IF
           IF  (I-38 AND I-34)
               MOVE 'B'                    TO KKODE
           END-IF
           ADD 1                           TO KSEQ
      *****************************************************************
      ***      LES MOT TABELL 1. GANG.
      *****************************************************************
           .
 
       LESPNY-T.
           MOVE SUMKEY                     TO OSKKEY (8:0)
           SET NOT-I-71                    TO TRUE
           SET TABOSK-S                    TO TABOSK-I
           PERFORM WITH TEST AFTER
                   VARYING TABOSK-I FROM 1 BY 1
                     UNTIL TABOSK-I >= TABOSK-MAX
                        OR I-71
               IF  OSKKEY = TABOSK (TABOSK-I)
                   SET I-71                TO TRUE
                   SET TABOSK-S            TO TABOSK-I
               END-IF
           END-PERFORM
           SET TABOSK-I                    TO TABOSK-S
           IF  I-71
           AND TABOSK-I NOT > TABOSD-MAX
               SET TABOSD-I                TO TABOSK-I
           END-IF
           SET NOT-I-72                    TO TRUE
           IF  KAVD = ' '
               SET I-72                    TO TRUE
           END-IF
           IF  (NOT-I-71 AND NOT-I-72)
               MOVE ' '                    TO KAVD
               GO TO LESPNY-T
           END-IF
           IF  (NOT-I-71)
               GO TO ENDTIL-T
      *****************************************************************
           END-IF
           .
 
       FRATIL-T.
           MOVE TABOSD(TABOSD-I) (104:0)   TO SUMDAT
           SET NOT-I-72                    TO TRUE
           IF  ORDSUM NOT < FRABEL
               SET I-72                    TO TRUE
           END-IF
           SET NOT-I-73                    TO TRUE
           IF  ORDSUM NOT > TILBEL
               SET I-73                    TO TRUE
           END-IF
           IF  (I-72 AND I-73)
               GO TO ORDRAB-T
      ***      LES MOT TABELL 2. GANG.
           END-IF
           ADD 1                           TO KSEQ
           MOVE SUMKEY                     TO OSKKEY (8:0)
           SET NOT-I-71                    TO TRUE
           SET TABOSK-S                    TO TABOSK-I
           PERFORM WITH TEST AFTER
                   VARYING TABOSK-I FROM 1 BY 1
                     UNTIL TABOSK-I >= TABOSK-MAX
                        OR I-71
               IF  OSKKEY = TABOSK (TABOSK-I)
                   SET I-71                TO TRUE
                   SET TABOSK-S            TO TABOSK-I
               END-IF
           END-PERFORM
           SET TABOSK-I                    TO TABOSK-S
           IF  I-71
           AND TABOSK-I NOT > TABOSD-MAX
               SET TABOSD-I                TO TABOSK-I
           END-IF
           IF  (NOT-I-71)
               GO TO ENDTIL-T
           END-IF
           IF  (I-71)
               GO TO FRATIL-T
      *****************************************************************
           END-IF
           .
 
       ORDRAB-T.
           SET NOT-I-74                    TO TRUE
           IF  RABPRO = 0,00
               SET I-74                    TO TRUE
           END-IF
           IF  (I-74)
               GO TO ORDGEB-T
           END-IF
           MULTIPLY RABPRO BY ORDSUM   GIVING SUM-X
           DIVIDE SUM-X BY 100         GIVING RABGEB
           SET I-70                        TO TRUE
           GO TO ENDTIL-T.
 
       ORDGEB-T.
           IF  (I-66)
               GO TO ENDTIL-T
           END-IF
           SET NOT-I-74                    TO TRUE
           IF  GEBYR = 0,00
               SET I-74                    TO TRUE
           END-IF
           IF  (I-74)
               GO TO ENDTIL-T
           END-IF
           MOVE GEBYR                      TO RABGEB-IO
           SET I-70                        TO TRUE.
 
       ENDTIL-T.
           CONTINUE.
      *******************************************************
      *   SUBRUTINE FOR DANNING AV PRISTILLEGG RECORD.      *
      *   PANTER, NRK AVGIFT, PARFYME AVGIFT, FRAKTTILLEGG. *
      *******************************************************
 
       PANTER-S SECTION.
       PANTER-S-P.
           ADD NPT TO ZERO             GIVING PRITIL
           ADD NPT TO ZERO             GIVING SVSTIL
           MOVE VGRFAK                     TO VGRTIL
      **    AVIK HAFNOR     *********************************
           IF  (I-51)
               SET NOT-I-45                TO TRUE
               IF  ALFAK = 'SØN'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-51 AND I-45)
               MOVE 'E'                    TO PRISTT
           END-IF
           IF  (I-51)
               SET NOT-I-45                TO TRUE
               IF  ALFAK = 'PHI'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-51 AND I-45)
               MOVE 'M'                    TO PRISTT
           END-IF
           IF  (I-51)
               SET NOT-I-45                TO TRUE
               IF  ALFAK = 'JVC'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-51 AND NOT-I-45)
               SET NOT-I-45                TO TRUE
               IF  ALFAK = 'MEM'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-51 AND I-45)
               MOVE 'K'                    TO PRISTT
           END-IF
           IF  (I-51)
               SET NOT-I-45                TO TRUE
               IF  ALFAK = 'DRI'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-51 AND I-45)
               MOVE 'P'                    TO PRISTT
           END-IF
           IF  (I-51)
               SET NOT-I-45                TO TRUE
               IF  ALFAK = 'KNU'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-51 AND NOT-I-45)
               SET NOT-I-45                TO TRUE
               IF  ALFAK = 'GEL'
                   SET I-45                TO TRUE
               END-IF
      *  51 45             MOVE "N"       PRISTT            = NRK AVGIFT
           END-IF
           IF  (I-51)
               SET NOT-I-45                TO TRUE
               IF  PRISTT = 'D'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-51 AND I-45)
               MOVE '29998'                TO VGRTIL
           END-IF
           IF  (I-51)
               SET NOT-I-45                TO TRUE
               IF  PRISTT = 'L'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-51 AND I-45)
               MOVE '29999'                TO VGRTIL
      **    AVIK S.O.B. UTGÅR 16.02.2001    *****************
      *  58      VGRAVD    COMP "4"                      45 = RADIO
      *  58 45             MOVE "N"       PRISTT            = NRK AVGIFT.
      **    AVIK FIRMA 764 BEMA    **************************
           END-IF
           SET NOT-I-75                    TO TRUE
           IF  FNR = '764'
               SET I-75                    TO TRUE
           END-IF
           IF  (NOT-I-75)
               SET NOT-I-75                TO TRUE
               IF  FNR = '787'
                   SET I-75                TO TRUE
               END-IF
           END-IF
           IF  (I-75)
               SET NOT-I-45                TO TRUE
               IF  ALFAK = 'WEC'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-75 AND NOT-I-45)
               SET NOT-I-45                TO TRUE
               IF  ALFAK = 'HØY'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-75 AND I-45)
               MOVE '19998'                TO VGRTIL
           END-IF
           IF  (I-75)
               SET NOT-I-45                TO TRUE
               IF  ALFAK = 'SON'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-75 AND I-45)
               MOVE '19999'                TO VGRTIL
           END-IF
           IF  (I-75)
               SET NOT-I-45                TO TRUE
               IF  ALFAK = 'OLJ'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-75 AND NOT-I-45)
               SET NOT-I-45                TO TRUE
               IF  ALFAK = 'KJE'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-75 AND I-45)
               MOVE '39999'                TO VGRTIL
           END-IF
           IF  (I-75)
               SET NOT-I-45                TO TRUE
               IF  ALFAK = 'FUL'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-75 AND NOT-I-45)
               SET NOT-I-45                TO TRUE
               IF  ALFAK = 'BAT'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-75 AND I-45)
               MOVE '59999'                TO VGRTIL
           END-IF
           IF  (I-75)
               SET NOT-I-45                TO TRUE
               IF  ALFAK = 'DYN'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-75 AND NOT-I-45)
               SET NOT-I-45                TO TRUE
               IF  ALFAK = 'STA'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-75 AND I-45)
               MOVE '79998'                TO VGRTIL
      **    AVIK FIRMA 956 VENG HALDEN           ************
           END-IF
           SET NOT-I-75                    TO TRUE
           IF  FNR = '956'
               SET I-75                    TO TRUE
           END-IF
           IF  (I-75)
               SET NOT-I-45                TO TRUE
               IF  VGRFAK = '61084'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-75 AND NOT-I-45)
               SET NOT-I-45                TO TRUE
               IF  VGRFAK = '95100'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-75 AND NOT-I-45)
               SET NOT-I-45                TO TRUE
               IF  VGRFAK = '95101'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-75 AND NOT-I-45)
               SET NOT-I-45                TO TRUE
               IF  VGRFAK = '95102'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-75 AND NOT-I-45)
               SET NOT-I-45                TO TRUE
               IF  VGRFAK = '95111'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-75 AND I-45)
               MOVE '95999'                TO VGRTIL
      **    AVIK FIRMA 915 RODIN    *************************
           END-IF
           SET NOT-I-75                    TO TRUE
           IF  FNR = '915'
               SET I-75                    TO TRUE
           END-IF
           IF  (I-75)
               SET NOT-I-45                TO TRUE
               IF  VGRFAK = '57000'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-75 AND NOT-I-45)
               SET NOT-I-45                TO TRUE
               IF  VGRFAK = '57100'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-75)
               SET NOT-I-43                TO TRUE
               IF  ALFAK = 'VAL'
                   SET I-43                TO TRUE
               END-IF
           END-IF
           IF  (I-75 AND I-45 AND I-43)
               MOVE '57121'                TO VGRTIL
           END-IF
           IF  (I-75)
               SET NOT-I-45                TO TRUE
               IF  VGRFAK = '12000'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-75 AND NOT-I-45)
               SET NOT-I-45                TO TRUE
               IF  VGRFAK = '12050'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-75 AND NOT-I-45)
               SET NOT-I-45                TO TRUE
               IF  VGRFAK = '12060'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-75 AND NOT-I-45)
               SET NOT-I-45                TO TRUE
               IF  VGRFAK = '12070'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-75 AND NOT-I-45)
               SET NOT-I-45                TO TRUE
               IF  VGRFAK = '12100'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-75 AND NOT-I-45)
               SET NOT-I-45                TO TRUE
               IF  VGRFAK = '12150'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-75 AND NOT-I-45)
               SET NOT-I-45                TO TRUE
               IF  VGRFAK = '12160'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-75 AND NOT-I-45)
               SET NOT-I-45                TO TRUE
               IF  VGRFAK = '12170'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-75)
               SET NOT-I-43                TO TRUE
               IF  ALFAK = 'VAR'
                   SET I-43                TO TRUE
               END-IF
           END-IF
           IF  (I-75 AND I-45 AND I-43)
               MOVE '12180'                TO VGRTIL
      **    AVIK FIRMA 990 BILMATERIELL TROMSØ   ************
      *          FNR       COMP "999"                    75 = BILMATERIE
      *  75      VGRTIL    COMP "16110"                  45 =           BATTERIE
      *  75N45   VGRTIL    COMP "16120"                  45 =           BATTERIE
      *  75N45   VGRTIL    COMP "16140"                  45 =           BATTERIE
      *  75N45   VGRTIL    COMP "16080"                  45 =           BATTERIE
      *  75N45   PRISTT    COMP "P"                      45 = PANT
      *  75 45             MOVE "19990"   VGRTIL            = EGEN VAREG
      **    AVIK FIRMA 740 BILDELER ÅLESUND         *********
           END-IF
           SET NOT-I-75                    TO TRUE
           IF  FNR = '740'
               SET I-75                    TO TRUE
           END-IF
           IF  (I-75)
               SET NOT-I-45                TO TRUE
               IF  PRISTT = 'P'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-75 AND I-45)
               MOVE '19997'                TO VGRTIL
      **    AVIK FIRMA 910 PER HAGEN                *********
           END-IF
           SET NOT-I-75                    TO TRUE
           IF  FNR = '910'
               SET I-75                    TO TRUE
           END-IF
           IF  (I-75)
               SET NOT-I-45                TO TRUE
               IF  PRISTT = 'P'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-75 AND I-45)
               MOVE '10012'                TO VGRTIL
      **    AVIK FIRMA 855 KCL                      *********
           END-IF
           SET NOT-I-75                    TO TRUE
           IF  FNR = '855'
               SET I-75                    TO TRUE
           END-IF
           IF  (NOT-I-75)
               SET NOT-I-75                TO TRUE
               IF  FNR = '871'
                   SET I-75                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-75)
               SET NOT-I-75                TO TRUE
               IF  FNR = '877'
                   SET I-75                TO TRUE
               END-IF
           END-IF
           IF  (I-75)
               SET NOT-I-45                TO TRUE
               IF  PRISTT = 'P'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-75 AND I-45)
               MOVE '21390'                TO VGRTIL
           END-IF
           IF  (I-75)
               SET NOT-I-45                TO TRUE
               IF  PRISTT = 'Q'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-75 AND I-45)
               MOVE '99941'                TO VGRTIL
      **    AVIK FIRMA 911 HØISTAD                  *********
           END-IF
           SET NOT-I-75                    TO TRUE
           IF  FNR = '911'
               SET I-75                    TO TRUE
           END-IF
           IF  (NOT-I-75)
               SET NOT-I-75                TO TRUE
               IF  FNR = '903'
                   SET I-75                TO TRUE
               END-IF
           END-IF
           IF  (I-75)
               SET NOT-I-45                TO TRUE
               IF  PRISTT = 'P'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-75 AND I-45)
               MOVE '11198'                TO VGRTIL
      **    AVIK FIRMA 927 STEGLET                  *********
           END-IF
           SET NOT-I-75                    TO TRUE
           IF  FNR = '927'
               SET I-75                    TO TRUE
           END-IF
           IF  (I-75)
               SET NOT-I-45                TO TRUE
               IF  PRISTT = 'P'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-75 AND I-45)
               MOVE '98098'                TO VGRTIL
      **    AVIK FIRMA 920 LØNSETHAGEN SERVICE      *********
           END-IF
           SET NOT-I-75                    TO TRUE
           IF  FNR = '920'
               SET I-75                    TO TRUE
           END-IF
           IF  (I-75)
               SET NOT-I-45                TO TRUE
               IF  PRISTT = 'V'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-75 AND I-45)
               MOVE '80011'                TO VGRTIL
      **    AVIK FIRMA 963 ROMNES   *************************
           END-IF
           SET NOT-I-75                    TO TRUE
           IF  FNR = '963'
               SET I-75                    TO TRUE
           END-IF
           IF  (I-75)
               MOVE '40999'                TO VGRTIL
           END-IF
           SET NOT-I-75                    TO TRUE
           IF  FNR = '912'
               SET I-75                    TO TRUE
           END-IF
           IF  (I-75)
               MOVE '40280'                TO VGRTIL
      **    AVIK FIRMA 705 JAHRE MOTOR   ********************
           END-IF
           SET NOT-I-75                    TO TRUE
           IF  FNR = '705'
               SET I-75                    TO TRUE
           END-IF
           IF  (I-75)
               SET NOT-I-45                TO TRUE
               IF  PRISTT = 'P'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-75 AND I-45)
               MOVE '20999'                TO VGRTIL
           END-IF
           IF  (I-75)
               SET NOT-I-45                TO TRUE
               IF  PRISTT = 'V'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-75 AND I-45)
               MOVE '98112'                TO VGRTIL
      **    AVIK FIRMA 958 HEGGELI       ********************
           END-IF
           SET NOT-I-75                    TO TRUE
           IF  FNR = '958'
               SET I-75                    TO TRUE
           END-IF
           IF  (I-75)
               MOVE '13199'                TO VGRTIL
               SET NOT-I-45                TO TRUE
               IF  ALFAK = 'DRI'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-75 AND I-45)
               MOVE '17010'                TO VGRTIL
           END-IF
           IF  (I-75)
               SET NOT-I-45                TO TRUE
               IF  ALFAK = 'CAL'
                   SET I-45                TO TRUE
               END-IF
           END-IF
           IF  (I-75 AND I-45)
               MOVE '11399'                TO VGRTIL
      **    FINN PRISTILLEGGS TEKST  ************************
           END-IF
           SET NOT-I-45                    TO TRUE
           IF  PRISTT NOT > ' '
               SET I-45                    TO TRUE
           END-IF
           IF  (I-45)
               MOVE 'P'                    TO PRISTT
           END-IF
           SET NOT-I-49                    TO TRUE
           SET TABTTY-S                    TO TABTTY-I
           PERFORM WITH TEST AFTER
                   VARYING TABTTY-I FROM 1 BY 1
                     UNTIL TABTTY-I >= TABTTY-MAX
                        OR I-49
               IF  PRISTT = TABTTY (TABTTY-I)
                   SET I-49                TO TRUE
                   SET TABTTY-S            TO TABTTY-I
               END-IF
           END-PERFORM
           SET TABTTY-I                    TO TABTTY-S
           IF  I-49
           AND TABTTY-I NOT > TABTTE-MAX
               SET TABTTE-I                TO TABTTY-I
           END-IF
           IF  (NOT-I-49)
               MOVE 'P'                    TO PRISTT
               SET NOT-I-49                TO TRUE
               SET TABTTY-S                TO TABTTY-I
               PERFORM WITH TEST AFTER
                       VARYING TABTTY-I FROM 1 BY 1
                         UNTIL TABTTY-I >= TABTTY-MAX
                            OR I-49
                   IF  PRISTT = TABTTY (TABTTY-I)
                       SET I-49            TO TRUE
                       SET TABTTY-S        TO TABTTY-I
                   END-IF
               END-PERFORM
               SET TABTTY-I                TO TABTTY-S
               IF  I-49
               AND TABTTY-I NOT > TABTTE-MAX
                   SET TABTTE-I            TO TABTTY-I
               END-IF
           END-IF
           SET I-55                        TO TRUE.
      *****************************************************************
      *  SUBRUTINE FOR OPPDATERING AV AVSTEMMINGSFILE.                *
      *****************************************************************
 
       AVSRLR-S SECTION.
       AVSRLR-S-P.
           MOVE 'ORD'                      TO AVSKEY
           MOVE AVSKEY                     TO AVSTEMF-KEY1
           READ AVSTEMF RECORD KEY IS AVSTEMF-KEY1
           INVALID KEY
               SET I-95                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-95                TO TRUE
               PERFORM AVSTEMF-FLDSET
               PERFORM AVSTEMF-IDSET
           END-READ
           IF  (NOT-I-95)
               SET I-97                    TO TRUE
           END-IF
           SET NOT-I-95                    TO TRUE
           ADD AOLRN                       TO ORDFAK.
      *****************************************************************
      *    RUTINE FOR BEREGNING AV FRAKTUTGEVINGSPROSENT.             *
      *    VIA SUBPROGRAM FRAKTSO2.                                   *
      *    FOMA ØNSKER DETTE PÅ KUNDER MED KUNDEKAT 105.              *
      *    FOMA ØNSKER DETTE PÅ KUNDER MED KUNDEKAT 129.              *
      *    FOMA ØNSKER DETTE PÅ KUNDER MED KUNDEKAT 138.              *
      *    FOMA ØNSKER DETTE PÅ KUNDER MED KUNDEKAT 128.              *
      *    FOMA ØNSKER DETTE PÅ KUNDER MED KUNDEKAT 109.              *
      *****************************************************************
 
       FRARUT-S SECTION.
       FRARUT-S-P.
           SET NOT-I-91                    TO TRUE
           IF  KUNKAT = '129'
               SET I-91                    TO TRUE
           END-IF
           IF  (I-91)
               MOVE 3,5                    TO FRPROS
               SET I-76                    TO TRUE
               GO TO TESPRO-T
           END-IF
           SET NOT-I-91                    TO TRUE
           IF  KUNKAT = '149'
               SET I-91                    TO TRUE
           END-IF
           IF  (I-91)
               MOVE 5,0                    TO FRPROS
               SET I-76                    TO TRUE
               GO TO TESPRO-T
           END-IF
           SET NOT-I-75                    TO TRUE
           IF  KUNKAT = '105'
               SET I-75                    TO TRUE
           END-IF
           IF  (NOT-I-75)
               SET NOT-I-75                TO TRUE
               IF  KUNKAT = '128'
                   SET I-75                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-75)
               SET NOT-I-75                TO TRUE
               IF  KUNKAT = '101'
                   SET I-75                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-75)
               SET NOT-I-75                TO TRUE
               IF  KUNKAT = '138'
                   SET I-75                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-75)
               SET NOT-I-75                TO TRUE
               IF  KUNKAT = '106'
                   SET I-75                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-75)
               SET NOT-I-75                TO TRUE
               IF  KUNKAT = '116'
                   SET I-75                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-75)
               SET NOT-I-75                TO TRUE
               IF  KUNKAT = '144'
                   SET I-75                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-75)
               SET NOT-I-75                TO TRUE
               IF  KUNKAT = '147'
                   SET I-75                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-75)
               SET NOT-I-75                TO TRUE
               IF  KUNKAT = '109'
                   SET I-75                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-75)
               GO TO ENDFRA-T
           END-IF
           IF  (I-39)
               MOVE VADPNR                 TO PNR
           END-IF
           IF  (NOT-I-39)
               MOVE POSTNR                 TO PNR
           END-IF
           MOVE 0,0                        TO FRAPRO
           CALL 'FRAKTSO2' USING FRADAT-XX-DATA-FIELDS
           ADD FRAPRO TO ZERO          GIVING FRPROS.
 
       TESPRO-T.
           SET NOT-I-76                    TO TRUE
           IF  FRPROS > 0,0
               SET I-76                    TO TRUE
           END-IF.
 
       ENDFRA-T.
           CONTINUE.
      *****************************************************************
      *    RUTINE FOR BEREGNING AV FRAKTUTGEVINGSPROSENT.             *
      *    VIA SUBPROGRAM FRAK2922.                                   *
      *    FOMA ØNSKER DETTE PÅ KUNDER MED KUNDEKAT 606.              *
      *    828   NSKER DETTE PÅ KUNDER MED KUNDEKAT 106.              *
      *****************************************************************
 
       FRARU2-S SECTION.
       FRARU2-S-P.
           SET NOT-I-75                    TO TRUE
           IF  KAT1 = '3'
               SET I-75                    TO TRUE
           END-IF
           IF  (I-75)
               GO TO ENDFR2-T
           END-IF
           SET NOT-I-75                    TO TRUE
           IF  KNR2F = '15'
               SET I-75                    TO TRUE
           END-IF
           IF  (I-75)
               GO TO ENDFR2-T
           END-IF
           SET NOT-I-75                    TO TRUE
           IF  KNR1F = '2'
               SET I-75                    TO TRUE
           END-IF
           IF  (I-75)
               GO TO ENDFR2-T
           END-IF
           SET NOT-I-75                    TO TRUE
           IF  KUNKAT = '106'
               SET I-75                    TO TRUE
           END-IF
           IF  (I-75)
               MOVE 3,0                    TO FRPROS
               SET I-76                    TO TRUE
               GO TO ENDFR2-T
           END-IF
           IF  (I-39)
               MOVE PAADR3                 TO PNR
           END-IF
           IF  (NOT-I-39)
               MOVE KADR4                  TO PNR
           END-IF
           MOVE 0,0                        TO FRAPRO
           CALL 'FRAK2922' USING FRADAT-XX-DATA-FIELDS
           ADD FRAPRO TO ZERO          GIVING FRPROS
           SET NOT-I-76                    TO TRUE
           IF  FRPROS > 0,0
               SET I-76                    TO TRUE
           END-IF.
 
       ENDFR2-T.
           CONTINUE.
      *****************************************************************
      * ORDRESUMMERING BEREGNING AV FRAKTUTGJEVNING FOMA              *
      *****************************************************************
 
       FRORUT-S SECTION.
       FRORUT-S-P.
           SUBTRACT LINSUM                 FROM LINSUM
           MULTIPLY LEVANT BY BELO-ELGP GIVING LINSUM
           MULTIPLY RAB1 BY LINSUM     GIVING SUM-X
           DIVIDE SUM-X BY 100         GIVING SUM-X
           SUBTRACT SUM-X                  FROM LINSUM
           MULTIPLY RAB2 BY LINSUM     GIVING SUM-X
           DIVIDE SUM-X BY 100         GIVING SUM-X
           SUBTRACT SUM-X                  FROM LINSUM
           MULTIPLY RAB3 BY LINSUM     GIVING SUM-X
           DIVIDE SUM-X BY 100         GIVING SUM-X
           SUBTRACT SUM-X                  FROM LINSUM
           ADD LINSUM                      TO FROSUM.
      *****************************************************************
      * SUBRUTINE FOR UTREGNING AV ORDRETOTAL FOR Å SE OM FRAKT SKAL  *
      *           FJERNES.                                            *
      *****************************************************************
 
       KCLTOT-S SECTION.
       KCLTOT-S-P.
           SUBTRACT LINSUM                 FROM LINSUM
           SET NOT-I-54                    TO TRUE
           IF  BESANT > LEVANT
               SET I-54                    TO TRUE
           END-IF
           IF  (I-54 AND NOT-I-08)
               MULTIPLY BESANT BY BELO-ELGP GIVING LINSUM
           END-IF
           IF  (NOT-I-54 AND I-20)
               MULTIPLY LEVANT BY BELO-ELGP GIVING LINSUM
           END-IF
           MULTIPLY RAB1 BY LINSUM     GIVING SUM-X
           DIVIDE SUM-X BY 100         GIVING SUM-X
           SUBTRACT SUM-X                  FROM LINSUM
           MULTIPLY RAB2 BY LINSUM     GIVING SUM-X
           DIVIDE SUM-X BY 100         GIVING SUM-X
           SUBTRACT SUM-X                  FROM LINSUM
           MULTIPLY RAB3 BY LINSUM     GIVING SUM-X
           DIVIDE SUM-X BY 100         GIVING SUM-X
           SUBTRACT SUM-X                  FROM LINSUM
           ADD LINSUM                      TO BESSUM
      *****************************************************************
      * HVILKEN ORDRESUMGRENSE SKAL BENYTTES.                         *
      *****************************************************************
           ADD VGR TO ZERO             GIVING VGRNUM
           MOVE VGRNUM                     TO VGRALF
      ** MLLzo
           MOVE '1'                        TO BW-A-2
           MULTIPLY 16                     BY BW-A
           MOVE VGRALF (5:1)               TO BW-B-2
           MULTIPLY 16                     BY BW-B
           MOVE BW-A-1                     TO BW-B-1
           DIVIDE 16                       INTO BW-B
           MOVE BW-B-2                     TO VGRALF (5:1)
           MOVE VGRALF (1:1)               TO VGR1F
           SET NOT-I-87                    TO TRUE
           IF  VGR1F = '1'
               SET I-87                    TO TRUE
           END-IF
           IF  (I-87)
               MOVE 'J'                    TO SUMG15
           END-IF
           SET NOT-I-87                    TO TRUE
           IF  VGR1F = '2'
               SET I-87                    TO TRUE
           END-IF
           IF  (I-87)
               MOVE 'J'                    TO SUMG05
           END-IF
           SET NOT-I-87                    TO TRUE
           IF  VGR1F = '3'
               SET I-87                    TO TRUE
           END-IF
           IF  (I-87)
               MOVE 'J'                    TO SUMG50
           END-IF
           SET NOT-I-87                    TO TRUE
           IF  VGR1F = '4'
               SET I-87                    TO TRUE
           END-IF
           IF  (I-87)
               MOVE 'J'                    TO SUMG15
           END-IF
           SET NOT-I-87                    TO TRUE
           IF  VGR1F = '5'
               SET I-87                    TO TRUE
           END-IF
           IF  (I-87)
               MOVE 'J'                    TO SUMG50
           END-IF
           SET NOT-I-87                    TO TRUE
           IF  VGR1F = '6'
               SET I-87                    TO TRUE
           END-IF
           IF  (I-87)
               MOVE 'J'                    TO SUMG15
           END-IF
           SET NOT-I-87                    TO TRUE
           IF  VGR1F = '7'
               SET I-87                    TO TRUE
           END-IF
           IF  (I-87)
               MOVE 'J'                    TO SUMG50
           END-IF
           SET NOT-I-87                    TO TRUE
           IF  VGR1F = '8'
               SET I-87                    TO TRUE
           END-IF
           IF  (I-87)
               MOVE 'J'                    TO SUMG15
           END-IF.
      *****************************************************************
      * SUBRUTINE FOR SJEKKE OM ORDREN SKAL VÆRE FRAKTFRITT.          *
      * 1: ER KUNDEN INNMELDT MED BESØKFREK "F" OG ORDRESUM 5.000/>   *
      * 2A:OM DET KUN ER VGR 11000-19999 OG ORDRESUM OVER  15.000/>   *
      * 2B:OM DET KUN ER VGR 20000-29999 OG ORDRESUM OVER   5.000/>   *
      * 2C:OM DET KUN ER VGR 30000-39999 OG ORDRESUM OVER  50.000/>   *
      * 2D:OM DET KUN ER VGR 40000-49999 OG ORDRESUM OVER  15.000/>   *
      * 2E:OM DET KUN ER VGR 50000-59999 OG ORDRESUM OVER  50.000/>   *
      * 2F:OM DET KUN ER VGR 60000-69999 OG ORDRESUM OVER  15.000/>   *
      * 2G:OM DET KUN ER VGR 70000-79999 OG ORDRESUM OVER  50.000/>   *
      * 2H:OM DET KUN ER VGR 80000-89999 OG ORDRESUM OVER  15.000/>   *
      * OM DET FLERE VGR KOMBINASKJONER  OG ORDRESUM OVER  15.000/>   *
      *****************************************************************
 
       KCLFRA-S SECTION.
       KCLFRA-S-P.
           SET NOT-I-64                    TO TRUE
           SET NOT-I-67                    TO TRUE
           IF  BSFREK = 'F'
               SET I-67                    TO TRUE
           END-IF
           IF  (I-67)
               MOVE 'J'                    TO SUMG05
           END-IF
           SET NOT-I-87                    TO TRUE
           IF  SUMG50 = 'J'
               SET I-87                    TO TRUE
           END-IF
           SET NOT-I-88                    TO TRUE
           IF  SUMG15 = 'J'
               SET I-88                    TO TRUE
           END-IF
           SET NOT-I-89                    TO TRUE
           IF  SUMG05 = 'J'
               SET I-89                    TO TRUE
           END-IF
           IF  (I-88 AND I-89 AND NOT-I-67)
               SET NOT-I-89                TO TRUE
           END-IF
           IF  (I-87)
               MOVE 50000                  TO ORDFGR
           END-IF
           IF  (I-88)
               MOVE 15000                  TO ORDFGR
           END-IF
           IF  (I-89)
               MOVE 5000                   TO ORDFGR
           END-IF
           SET NOT-I-87                    TO TRUE
           SET NOT-I-88                    TO TRUE
           SET NOT-I-89                    TO TRUE
           SET NOT-I-64                    TO TRUE
           IF  BESSUM NOT < ORDFGR
               SET I-64                    TO TRUE
           END-IF
           SET I-88                        TO TRUE.
      *****************************************************************
      **** FAKTURA RECORD FRA RENTERUTINE / ELLER DANNET FØR.
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1 AND I-93 AND I-76)
               SET NOT-I-92                TO TRUE
               IF  FROSUM < 10,00
                   SET I-92                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-93 AND I-92)
               SET NOT-I-76                TO TRUE
           END-IF
           IF  (I-L1 AND I-93 AND I-76)
               MULTIPLY FRPROS BY FROSUM GIVING FELT92 ROUNDED
               DIVIDE FELT92 BY 100    GIVING NYFRAK ROUNDED
               ADD NYFRAK                  TO TOTNYF
               MOVE 1,00                   TO ANT1
               ADD 1                       TO ANTFRB
               ADD 1                       TO ARL2
               ADD 1                       TO ARLR
               ADD 1                       TO ARL2F
               ADD 1                       TO ARLRF
      ******************************************************
      * ORDRENR.TOTAL RUTINE.                              *
      ******************************************************
           END-IF
           IF  (I-L1)
               ADD 1                       TO AOL2
               ADD 1                       TO AOLR
               SET NOT-I-63                TO TRUE
               IF  ORDSUM = 0
                   SET I-63                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-63)
               GO TO ENDL1-T
           END-IF
           IF  (I-L1 AND I-65)
               PERFORM TILRUT-S
           END-IF
           IF  (I-L1 AND I-70)
               ADD 1                       TO ARL2
               ADD 1                       TO ARLR
               MOVE 1,00                   TO GEBANT
           END-IF.
 
       ENDL1-T.
           CONTINUE.
 
       LR-CALCS SECTION.
       LR-CALCS-P.
           PERFORM AVSRLR-S
      ******************************************************
      * SUBRUTINE FOR KORRIGERING AV AVDELING.             *
      ******************************************************
           .
 
       FAFILE-GET SECTION.
       FAFILE-GET-P.
           IF  FAFILE-EOF-OFF
               READ FAFILE
               AT END
                   SET FAFILE-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FAFILE-FLDSET SECTION.
       FAFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAFILE-IO-AREA (1:200) TO REC1 (1:200)
               MOVE FAFILE-IO-AREA (1:3)   TO FIRMNR (1:3)
               MOVE FAFILE-IO-AREA (19:6)  TO FONR (1:6)
           END-EVALUATE.
 
       FAFILE-IDSET SECTION.
       FAFILE-IDSET-P.
           SET I-01                        TO TRUE.
 
       FAFILE-CHK-LEVEL SECTION.
       FAFILE-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO FAFILE-LEVEL-01
               MOVE FAFILE-IO-AREA (1:3)   TO FAFILE-01-L2-FIRMNR
               MOVE FAFILE-IO-AREA (19:6)  TO FAFILE-01-L1-FONR
               IF  FAFILE-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  FAFILE-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  FAFILE-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  FAFILE-01-L2          TO THE-PRIOR-L2
               MOVE  FAFILE-01-L1          TO THE-PRIOR-L1
               SET FAFILE-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       FAFILE-MATCH-SET SECTION.
       FAFILE-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE FAFILE-IO-AREA (1:3)   TO FAFILE-M-01-M2-FIRMNR
               MOVE FAFILE-IO-AREA (19:6)  TO FAFILE-M-01-M1-FONR
           END-EVALUATE.
 
       ORDFAKT-GET SECTION.
       ORDFAKT-GET-P.
           IF  ORDFAKT-EOF-OFF
               READ ORDFAKT
               AT END
                   SET ORDFAKT-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       ORDFAKT-FLDOFF SECTION.
       ORDFAKT-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( ORDFAKT-IO-AREA (1:1) = 'O'
            AND   ORDFAKT-IO-AREA (19:1) NOT = ' ' )
               SET NOT-I-08                TO TRUE
               SET NOT-I-59                TO TRUE
               SET NOT-I-09                TO TRUE
               SET NOT-I-07                TO TRUE
           END-EVALUATE.
 
       ORDFAKT-FLDSET SECTION.
       ORDFAKT-FLDSET-P.
           EVALUATE TRUE
           WHEN ( ORDFAKT-IO-AREA (1:1) = 'O'
            AND   ORDFAKT-IO-AREA (19:1) = ' '
            AND   ORDFAKT-IO-AREA (20:1) = '1' )
               MOVE ORDFAKT-IO-AREA (2:3)  TO FIRMNR (1:3)
               MOVE ORDFAKT-IO-AREA (5:1)  TO ORDTYP (1:1)
               MOVE ORDFAKT-IO-AREA (5:6)  TO ONR (1:6)
               MOVE ORDFAKT-IO-AREA (21:6) TO KNR1 (1:6)
               MOVE ORDFAKT-IO-AREA (21:1) TO KNR1F (1:1)
               MOVE ORDFAKT-IO-AREA (21:2) TO KNR2F (1:2)
               MOVE ORDFAKT-IO-AREA (88:1) TO DIRREG (1:1)
               MOVE ORDFAKT-IO-AREA (89:1) TO AVGF (1:1)
               MOVE ORDFAKT-IO-AREA (90:2) TO LAGERK (1:2)
               MOVE ORDFAKT-IO-AREA (92:1) TO BKKODE (1:1)
               MOVE ORDFAKT-IO-AREA (94:2) TO BETBET (1:2)
               MOVE ORDFAKT-IO-AREA (96:1) TO OGEBYR (1:1)
               MOVE ORDFAKT-IO-AREA (98:1) TO AVD (1:1)
               MOVE ORDFAKT-IO-AREA (99:1) TO KRETYP (1:1)
               MOVE ORDFAKT-IO-AREA (100:1) TO SERVT (1:1)
               MOVE ORDFAKT-IO-AREA (103:1) TO PRITYP (1:1)
               MOVE ORDFAKT-IO-AREA (106:1) TO FAKTM (1:1)
               MOVE ORDFAKT-IO-AREA (136:6) TO ODATO (1:6)
               MOVE ORDFAKT-IO-AREA (142:2) TO OM (1:2)
               MOVE ORDFAKT-IO-AREA (138:2) TO OMND (1:2)
               MOVE ORDFAKT-IO-AREA (157:1) TO TYPE-X (1:1)
               MOVE ORDFAKT-IO-AREA (162:1) TO BSFREK (1:1)
           WHEN ( ORDFAKT-IO-AREA (1:1) = 'O'
            AND   ORDFAKT-IO-AREA (19:1) = ' '
            AND   ORDFAKT-IO-AREA (20:1) = '2' )
               MOVE ORDFAKT-IO-AREA (35:3) TO KUNKAT (1:3)
               MOVE ORDFAKT-IO-AREA (35:1) TO KAT1 (1:1)
               MOVE ORDFAKT-IO-AREA (38:6) TO RORRUT (1:6)
               MOVE ORDFAKT-IO-AREA (97:3) TO HND (1:3)
               MOVE ORDFAKT-IO-AREA (44:6) TO FAKREF (1:6)
               MOVE ORDFAKT-IO-AREA (67:15) TO REKVNR (1:15)
               MOVE ORDFAKT-IO-AREA (82:2) TO FORS2F (1:2)
               MOVE ORDFAKT-IO-AREA (82:3) TO FORS3F (1:3)
               MOVE ORDFAKT-IO-AREA (82:5) TO FORS5F (1:5)
               MOVE ORDFAKT-IO-AREA (131:4) TO POSTNR (1:4)
               MOVE ORDFAKT-IO-AREA (101:4) TO KADR4 (1:4)
           WHEN ( ORDFAKT-IO-AREA (1:1) = 'O'
            AND   ORDFAKT-IO-AREA (19:1) = ' '
            AND   ORDFAKT-IO-AREA (20:1) = '3' )
               MOVE ORDFAKT-IO-AREA (21:30) TO VADR1 (1:30)
               MOVE ORDFAKT-IO-AREA (51:30) TO VADR2 (1:30)
               MOVE ORDFAKT-IO-AREA (81:30) TO VADR3 (1:30)
               MOVE ORDFAKT-IO-AREA (81:4) TO PAADR3 (1:4)
               MOVE ORDFAKT-IO-AREA (111:20) TO VADR4 (1:20)
               MOVE ORDFAKT-IO-AREA (111:4) TO VADPNR (1:4)
           WHEN ( ORDFAKT-IO-AREA (1:1) = 'O'
            AND   ORDFAKT-IO-AREA (19:1) NOT = ' ' )
               MOVE ORDFAKT-IO-AREA (17:3) TO POSNR (1:3)
               MOVE ORDFAKT-IO-AREA (21:4) TO BESANT-IO
               IF  BESANT = ZERO
                   SET I-08                TO TRUE
               END-IF
               MOVE ORDFAKT-IO-AREA (25:4) TO RESANT-IO
               MOVE ORDFAKT-IO-AREA (29:4) TO LEVANT-IO
               MOVE ORDFAKT-IO-AREA (34:3) TO ALFAK (1:3)
               MOVE ORDFAKT-IO-AREA (37:20) TO ARTNR (1:20)
               MOVE ORDFAKT-IO-AREA (37:6) TO ARTN6F (1:6)
               MOVE ORDFAKT-IO-AREA (37:16) TO ART16F (1:16)
               MOVE ORDFAKT-IO-AREA (53:4) TO ART4S (1:4)
               IF  ART4S = SPACES
                   SET I-59                TO TRUE
               END-IF
               MOVE ORDFAKT-IO-AREA (57:30) TO VAREB (1:30)
               MOVE ORDFAKT-IO-AREA (37:8) TO TEKST1 (1:8)
               MOVE ORDFAKT-IO-AREA (45:8) TO TEKST2 (1:8)
               MOVE ORDFAKT-IO-AREA (53:8) TO TEKST3 (1:8)
               MOVE ORDFAKT-IO-AREA (61:8) TO TEKST4 (1:8)
               MOVE ORDFAKT-IO-AREA (69:8) TO TEKST5 (1:8)
               MOVE ORDFAKT-IO-AREA (77:8) TO TEKST6 (1:8)
               MOVE ORDFAKT-IO-AREA (37:50) TO TEKST (1:50)
               MOVE ORDFAKT-IO-AREA (94:5) TO BELO-ELGP-IO
               IF  BELO-ELGP = ZERO
                   SET I-09                TO TRUE
               END-IF
               MOVE ORDFAKT-IO-AREA (87:4) TO EDBNR-IO
               MOVE ORDFAKT-IO-AREA (99:2) TO RAB1-IO
               MOVE ORDFAKT-IO-AREA (101:2) TO RAB2-IO
               MOVE ORDFAKT-IO-AREA (103:2) TO RAB3-IO
               MOVE ORDFAKT-IO-AREA (91:3) TO VGR-IO
               MOVE ORDFAKT-IO-AREA (121:5) TO SVS-IO
               MOVE ORDFAKT-IO-AREA (126:4) TO NPT-IO
               IF  NPT = ZERO
                   SET I-07                TO TRUE
               END-IF
               MOVE ORDFAKT-IO-AREA (134:1) TO VLKOD1 (1:1)
               MOVE ORDFAKT-IO-AREA (164:1) TO PRISTT (1:1)
           END-EVALUATE.
 
       ORDFAKT-IDCHK SECTION.
       ORDFAKT-IDCHK-P.
           EVALUATE TRUE
           WHEN ( ORDFAKT-IO-AREA (1:1) = 'O'
            AND   ORDFAKT-IO-AREA (19:1) = ' '
            AND   ORDFAKT-IO-AREA (20:1) = '1' )
             OR ( ORDFAKT-IO-AREA (1:1) = 'O'
            AND   ORDFAKT-IO-AREA (19:1) = ' '
            AND   ORDFAKT-IO-AREA (20:1) = '2' )
             OR ( ORDFAKT-IO-AREA (1:1) = 'O'
            AND   ORDFAKT-IO-AREA (19:1) = ' '
            AND   ORDFAKT-IO-AREA (20:1) = '3' )
             OR ( ORDFAKT-IO-AREA (1:1) = 'O'
            AND   ORDFAKT-IO-AREA (19:1) NOT = ' ' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       ORDFAKT-IDSET SECTION.
       ORDFAKT-IDSET-P.
           EVALUATE TRUE
           WHEN ( ORDFAKT-IO-AREA (1:1) = 'O'
            AND   ORDFAKT-IO-AREA (19:1) = ' '
            AND   ORDFAKT-IO-AREA (20:1) = '1' )
               SET I-02                    TO TRUE
           WHEN ( ORDFAKT-IO-AREA (1:1) = 'O'
            AND   ORDFAKT-IO-AREA (19:1) = ' '
            AND   ORDFAKT-IO-AREA (20:1) = '2' )
               SET I-05                    TO TRUE
           WHEN ( ORDFAKT-IO-AREA (1:1) = 'O'
            AND   ORDFAKT-IO-AREA (19:1) = ' '
            AND   ORDFAKT-IO-AREA (20:1) = '3' )
               SET I-04                    TO TRUE
           WHEN ( ORDFAKT-IO-AREA (1:1) = 'O'
            AND   ORDFAKT-IO-AREA (19:1) NOT = ' ' )
               SET I-06                    TO TRUE
           END-EVALUATE.
 
       ORDFAKT-CHK-LEVEL SECTION.
       ORDFAKT-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( ORDFAKT-IO-AREA (1:1) = 'O'
            AND   ORDFAKT-IO-AREA (19:1) = ' '
            AND   ORDFAKT-IO-AREA (20:1) = '1' )
               MOVE LOW-VALUES             TO ORDFAKT-LEVEL-02
               MOVE ORDFAKT-IO-AREA (2:3)  TO ORDFAKT-02-L2-FIRMNR
               MOVE ORDFAKT-IO-AREA (5:6)  TO ORDFAKT-02-L1-ONR
               IF  ORDFAKT-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDFAKT-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  ORDFAKT-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDFAKT-02-L2         TO THE-PRIOR-L2
               MOVE  ORDFAKT-02-L1         TO THE-PRIOR-L1
               SET ORDFAKT-LEVEL-INIT      TO TRUE
           WHEN ( ORDFAKT-IO-AREA (1:1) = 'O'
            AND   ORDFAKT-IO-AREA (19:1) = ' '
            AND   ORDFAKT-IO-AREA (20:1) = '2' )
               CONTINUE
           WHEN ( ORDFAKT-IO-AREA (1:1) = 'O'
            AND   ORDFAKT-IO-AREA (19:1) = ' '
            AND   ORDFAKT-IO-AREA (20:1) = '3' )
               CONTINUE
           WHEN ( ORDFAKT-IO-AREA (1:1) = 'O'
            AND   ORDFAKT-IO-AREA (19:1) NOT = ' ' )
               CONTINUE
           END-EVALUATE.
 
       ORDFAKT-MATCH-SET SECTION.
       ORDFAKT-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( ORDFAKT-IO-AREA (1:1) = 'O'
            AND   ORDFAKT-IO-AREA (19:1) = ' '
            AND   ORDFAKT-IO-AREA (20:1) = '1' )
               MOVE ORDFAKT-IO-AREA (2:3)  TO ORDFAKT-M-02-M2-FIRMNR
               MOVE ORDFAKT-IO-AREA (5:6)  TO ORDFAKT-M-02-M1-ONR
           WHEN ( ORDFAKT-IO-AREA (1:1) = 'O'
            AND   ORDFAKT-IO-AREA (19:1) = ' '
            AND   ORDFAKT-IO-AREA (20:1) = '2' )
               SET NOT-CALL-MATCH-RECS     TO TRUE
           WHEN ( ORDFAKT-IO-AREA (1:1) = 'O'
            AND   ORDFAKT-IO-AREA (19:1) = ' '
            AND   ORDFAKT-IO-AREA (20:1) = '3' )
               SET NOT-CALL-MATCH-RECS     TO TRUE
           WHEN ( ORDFAKT-IO-AREA (1:1) = 'O'
            AND   ORDFAKT-IO-AREA (19:1) NOT = ' ' )
               SET NOT-CALL-MATCH-RECS     TO TRUE
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (8:30)  TO FINAVN (1:30)
               MOVE FIRMAF-IO-AREA (165:1) TO FGEB (1:1)
               MOVE FIRMAF-IO-AREA (178:7) TO FGEBNR-IO
               INSPECT FGEBNR-IO REPLACING ALL ' ' BY '0'
               MOVE FIRMAF-IO-AREA (158:1) TO FAKALF (1:1)
               MOVE FIRMAF-IO-AREA (781:1) TO KPRIS (1:1)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-10                        TO TRUE.
 
       ORDNRM-FLDSET SECTION.
       ORDNRM-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE ORDNRM-IO-AREA (76:1)  TO ONRTYP (1:1)
           END-EVALUATE.
 
       ORDNRM-IDSET SECTION.
       ORDNRM-IDSET-P.
           SET I-10                        TO TRUE.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (123:3) TO VMSTAT (1:3)
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-10                        TO TRUE.
 
       AVSTEMF-FLDSET SECTION.
       AVSTEMF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE AVSTEMF-IO-AREA (53:6) TO ORDFAK-IO
               INSPECT ORDFAK-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       AVSTEMF-IDSET SECTION.
       AVSTEMF-IDSET-P.
           SET I-03                        TO TRUE.
 
       TOTALER-PRINT-LINE SECTION.
       TOTALER-PRINT-LINE-P.
           IF  TOTALER-BEFORE-SKIP > 0
               PERFORM TOTALER-SKIP-BEFORE
           END-IF
           IF  TOTALER-BEFORE-SPACE > 0
               PERFORM TOTALER-SPACE-BEFORE
               IF  TOTALER-AFTER-SKIP > 0
                   PERFORM TOTALER-SKIP-AFTER
               END-IF
               IF  TOTALER-AFTER-SPACE > 0
                   PERFORM TOTALER-SPACE-AFTER
               END-IF
           ELSE
               IF  TOTALER-AFTER-SKIP > 0
                   PERFORM TOTALER-SKIP-AFTER
               END-IF
               PERFORM TOTALER-SPACE-AFTER
           END-IF
           IF  TOTALER-LINE-COUNT NOT < TOTALER-MAX-LINES
               SET I-OF                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OF            TO TRUE
               END-IF
           END-IF.
 
       TOTALER-SKIP-BEFORE SECTION.
       TOTALER-SKIP-BEFORE-P.
           WRITE TOTALER-IO-PRINT       AFTER ADVANCING PAGE
           MOVE 1                          TO TOTALER-LINE-COUNT
           MOVE 0                          TO TOTALER-BEFORE-SKIP
           INITIALIZE TOTALER-IO-AREA.
 
       TOTALER-SPACE-BEFORE SECTION.
       TOTALER-SPACE-BEFORE-P.
           WRITE TOTALER-IO-PRINT       AFTER TOTALER-BEFORE-SPACE
                                                                 LINES
           ADD TOTALER-BEFORE-SPACE        TO TOTALER-LINE-COUNT
           MOVE SPACES TO TOTALER-IO-AREA
           INITIALIZE TOTALER-IO-AREA
           MOVE 0                          TO TOTALER-BEFORE-SPACE.
 
       TOTALER-SKIP-AFTER SECTION.
       TOTALER-SKIP-AFTER-P.
           WRITE TOTALER-IO-PRINT      BEFORE ADVANCING PAGE
           MOVE 1                          TO TOTALER-LINE-COUNT
           MOVE 0                          TO TOTALER-AFTER-SKIP
           INITIALIZE TOTALER-IO-AREA.
 
       TOTALER-SPACE-AFTER SECTION.
       TOTALER-SPACE-AFTER-P.
           WRITE TOTALER-IO-PRINT      BEFORE TOTALER-AFTER-SPACE LINES
           ADD TOTALER-AFTER-SPACE         TO TOTALER-LINE-COUNT
           INITIALIZE TOTALER-IO-AREA
           MOVE 0                          TO TOTALER-AFTER-SPACE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  FAFILE-EOF
               MOVE HIGH-VALUES            TO FAFILE-MC
                                              FAFILE-MP
           END-IF
           IF  ORDFAKT-EOF
               MOVE HIGH-VALUES            TO ORDFAKT-MC
                                              ORDFAKT-MP
           END-IF
           IF  FAFILE-MC < FAFILE-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  ORDFAKT-MC < ORDFAKT-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  FAFILE-MC < ORDFAKT-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FAFILE-PROCESS      TO TRUE
                   MOVE FAFILE-MC          TO FAFILE-MP
                   IF  FAFILE-MC = ORDFAKT-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  ORDFAKT-MC < FAFILE-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET ORDFAKT-PROCESS     TO TRUE
                   MOVE ORDFAKT-MC         TO ORDFAKT-MP
                   IF  ORDFAKT-MC = FAFILE-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  FAFILE-MC = ORDFAKT-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FAFILE-PROCESS      TO TRUE
                   MOVE FAFILE-MC          TO FAFILE-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       TEKSTAB-LOAD SECTION.
       TEKSTAB-LOAD-P.
           OPEN INPUT TEKSTAB
           SET TABTTY-I                    TO 1
           PERFORM UNTIL TEKSTAB-EOF
               READ TEKSTAB
               AT END
                   SET TEKSTAB-EOF         TO TRUE
               NOT AT END
                   MOVE TEKSTAB-IO-AREA (1:31) TO TABTTY-ENTRY
                                                            (TABTTY-I)
                   SET TABTTY-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE TEKSTAB.
 
       OSKFILE-LOAD SECTION.
       OSKFILE-LOAD-P.
           OPEN INPUT OSKFILE
           SET TABOSK-I                    TO 1
           PERFORM UNTIL OSKFILE-EOF
               READ OSKFILE
               AT END
                   SET OSKFILE-EOF         TO TRUE
               NOT AT END
                   MOVE OSKFILE-IO-AREA (1:110) TO TABOSK-ENTRY
                                                            (TABOSK-I)
                   SET TABOSK-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE OSKFILE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               MOVE SPACES TO OUTF-IO-AREA
               INITIALIZE OUTF-IO-AREA
               MOVE REC1                   TO OUTF-IO-AREA (1:200)
      *****************************************************************
      * VAREADRESSERECORD 1.                                          *
      *****************************************************************
               WRITE OUTF-IO-AREA
           END-IF
           IF  (I-04 AND I-36)
               MOVE SPACES TO OUTF-IO-AREA
               INITIALIZE OUTF-IO-AREA
               MOVE FNR                    TO OUTF-IO-AREA (1:3)
               MOVE KNR                    TO OUTF-IO-AREA (4:6)
               MOVE FAKTYP                 TO OUTF-IO-AREA (10:1)
               MOVE AVGTYP                 TO OUTF-IO-AREA (13:1)
               MOVE BETBET                 TO OUTF-IO-AREA (14:2)
               MOVE ONR                    TO OUTF-IO-AREA (19:6)
               MOVE 'A'                    TO OUTF-IO-AREA (25:1)
               MOVE ' '                    TO OUTF-IO-AREA (37:1)
               IF  (I-11)
                   MOVE VADR1              TO OUTF-IO-AREA (41:30)
               END-IF
               MOVE ODATO                  TO OUTF-IO-AREA (71:6)
               IF  (I-18)
                   MOVE VADR2              TO OUTF-IO-AREA (77:30)
               END-IF
               IF  (I-77)
                   MOVE SERVT              TO OUTF-IO-AREA (132:1)
               END-IF
               MOVE NULANT                 TO XO-52P
               MOVE XO-52P-EF              TO OUTF-IO-AREA (133:4)
               MOVE NULANT                 TO XO-52P
               MOVE XO-52P-EF              TO OUTF-IO-AREA (137:4)
               MOVE AVD                    TO OUTF-IO-AREA (166:1)
               MOVE NULL-X                 TO XO-72P
               MOVE XO-72P-EF              TO OUTF-IO-AREA (171:5)
               MOVE ' '                    TO OUTF-IO-AREA (177:1)
               MOVE OGEBYR                 TO OUTF-IO-AREA (178:1)
               MOVE FAKKRN                 TO OUTF-IO-AREA (181:1)
               MOVE KNR                    TO OUTF-IO-AREA (184:6)
      *****************************************************************
      * VAREADRESSERECORD 2.                                          *
      *****************************************************************
               WRITE OUTF-IO-AREA
           END-IF
           IF  (I-04 AND I-37)
               MOVE SPACES TO OUTF-IO-AREA
               INITIALIZE OUTF-IO-AREA
               MOVE FNR                    TO OUTF-IO-AREA (1:3)
               MOVE KNR                    TO OUTF-IO-AREA (4:6)
               MOVE FAKTYP                 TO OUTF-IO-AREA (10:1)
               MOVE AVGTYP                 TO OUTF-IO-AREA (13:1)
               MOVE BETBET                 TO OUTF-IO-AREA (14:2)
               MOVE ONR                    TO OUTF-IO-AREA (19:6)
               MOVE 'A'                    TO OUTF-IO-AREA (25:1)
               MOVE ' '                    TO OUTF-IO-AREA (37:1)
               IF  (I-17)
                   MOVE VADR3              TO OUTF-IO-AREA (41:30)
               END-IF
               MOVE ODATO                  TO OUTF-IO-AREA (71:6)
               IF  (I-19)
                   MOVE VADR4              TO OUTF-IO-AREA (77:20)
               END-IF
               IF  (I-19)
                   MOVE '          '       TO OUTF-IO-AREA (97:10)
               END-IF
               IF  (I-77)
                   MOVE SERVT              TO OUTF-IO-AREA (132:1)
               END-IF
               MOVE NULANT                 TO XO-52P
               MOVE XO-52P-EF              TO OUTF-IO-AREA (133:4)
               MOVE NULANT                 TO XO-52P
               MOVE XO-52P-EF              TO OUTF-IO-AREA (137:4)
               MOVE AVD                    TO OUTF-IO-AREA (166:1)
               MOVE NULL-X                 TO XO-72P
               MOVE XO-72P-EF              TO OUTF-IO-AREA (171:5)
               MOVE ' '                    TO OUTF-IO-AREA (177:1)
               MOVE OGEBYR                 TO OUTF-IO-AREA (178:1)
               MOVE FAKKRN                 TO OUTF-IO-AREA (181:1)
               MOVE KNR                    TO OUTF-IO-AREA (184:6)
      *****************************************************************
      * VAREADRESSE SOM TEKSTRECORD 1   (FAKT.MÅTE 5)                *
      *****************************************************************
               WRITE OUTF-IO-AREA
           END-IF
           IF  (I-04 AND I-81)
               MOVE SPACES TO OUTF-IO-AREA
               INITIALIZE OUTF-IO-AREA
               MOVE FNR                    TO OUTF-IO-AREA (1:3)
               MOVE KNR                    TO OUTF-IO-AREA (4:6)
               MOVE FAKTYP                 TO OUTF-IO-AREA (10:1)
               MOVE AVGTYP                 TO OUTF-IO-AREA (13:1)
               MOVE BETBET                 TO OUTF-IO-AREA (14:2)
               MOVE ONR                    TO OUTF-IO-AREA (19:6)
               MOVE 'L'                    TO OUTF-IO-AREA (25:1)
               MOVE 'A'                    TO OUTF-IO-AREA (37:1)
               IF  (I-22)
                   MOVE REKVNR             TO OUTF-IO-AREA (41:15)
               END-IF
               IF  (I-27)
                   MOVE FAKREF             TO OUTF-IO-AREA (56:6)
               END-IF
               IF  (I-23)
                   MOVE LAGERK             TO OUTF-IO-AREA (62:2)
               END-IF
               IF  (I-24)
                   MOVE BKKODE             TO OUTF-IO-AREA (66:1)
               END-IF
               MOVE AVGKOD                 TO OUTF-IO-AREA (67:1)
               IF  (I-25)
                   MOVE HND                TO OUTF-IO-AREA (68:3)
               END-IF
               MOVE ODATO                  TO OUTF-IO-AREA (71:6)
               MOVE VADR1                  TO OUTF-IO-AREA (82:30)
               IF  (I-77)
                   MOVE SERVT              TO OUTF-IO-AREA (132:1)
               END-IF
               MOVE NULANT                 TO XO-52P
               MOVE XO-52P-EF              TO OUTF-IO-AREA (133:4)
               MOVE NULANT                 TO XO-52P
               MOVE XO-52P-EF              TO OUTF-IO-AREA (137:4)
               MOVE AVD                    TO OUTF-IO-AREA (166:1)
               MOVE NULL-X                 TO XO-72P
               MOVE XO-72P-EF              TO OUTF-IO-AREA (171:5)
               MOVE ' '                    TO OUTF-IO-AREA (177:1)
               MOVE OGEBYR                 TO OUTF-IO-AREA (178:1)
               IF  (I-26)
                   MOVE KRETYP             TO OUTF-IO-AREA (179:1)
               END-IF
               MOVE FAKKRN                 TO OUTF-IO-AREA (181:1)
               MOVE KNR                    TO OUTF-IO-AREA (184:6)
      *****************************************************************
      * VAREADRESSE SOM TEKSTRECORD 2   (FAKT.MÅTE 5)                *
      *****************************************************************
               WRITE OUTF-IO-AREA
           END-IF
           IF  (I-04 AND I-82)
               MOVE SPACES TO OUTF-IO-AREA
               INITIALIZE OUTF-IO-AREA
               MOVE FNR                    TO OUTF-IO-AREA (1:3)
               MOVE KNR                    TO OUTF-IO-AREA (4:6)
               MOVE FAKTYP                 TO OUTF-IO-AREA (10:1)
               MOVE AVGTYP                 TO OUTF-IO-AREA (13:1)
               MOVE BETBET                 TO OUTF-IO-AREA (14:2)
               MOVE ONR                    TO OUTF-IO-AREA (19:6)
               MOVE 'L'                    TO OUTF-IO-AREA (25:1)
               MOVE 'A'                    TO OUTF-IO-AREA (37:1)
               IF  (I-22)
                   MOVE REKVNR             TO OUTF-IO-AREA (41:15)
               END-IF
               IF  (I-27)
                   MOVE FAKREF             TO OUTF-IO-AREA (56:6)
               END-IF
               IF  (I-23)
                   MOVE LAGERK             TO OUTF-IO-AREA (62:2)
               END-IF
               IF  (I-24)
                   MOVE BKKODE             TO OUTF-IO-AREA (66:1)
               END-IF
               MOVE AVGKOD                 TO OUTF-IO-AREA (67:1)
               IF  (I-25)
                   MOVE HND                TO OUTF-IO-AREA (68:3)
               END-IF
               MOVE ODATO                  TO OUTF-IO-AREA (71:6)
               MOVE VADR2                  TO OUTF-IO-AREA (82:30)
               IF  (I-77)
                   MOVE SERVT              TO OUTF-IO-AREA (132:1)
               END-IF
               MOVE NULANT                 TO XO-52P
               MOVE XO-52P-EF              TO OUTF-IO-AREA (133:4)
               MOVE NULANT                 TO XO-52P
               MOVE XO-52P-EF              TO OUTF-IO-AREA (137:4)
               MOVE AVD                    TO OUTF-IO-AREA (166:1)
               MOVE NULL-X                 TO XO-72P
               MOVE XO-72P-EF              TO OUTF-IO-AREA (171:5)
               MOVE ' '                    TO OUTF-IO-AREA (177:1)
               MOVE OGEBYR                 TO OUTF-IO-AREA (178:1)
               IF  (I-26)
                   MOVE KRETYP             TO OUTF-IO-AREA (179:1)
               END-IF
               MOVE FAKKRN                 TO OUTF-IO-AREA (181:1)
               MOVE KNR                    TO OUTF-IO-AREA (184:6)
      *****************************************************************
      * VAREADRESSE SOM TEKSTRECORD 3   (FAKT.MÅTE 5)                *
      *****************************************************************
               WRITE OUTF-IO-AREA
           END-IF
           IF  (I-04 AND I-83)
               MOVE SPACES TO OUTF-IO-AREA
               INITIALIZE OUTF-IO-AREA
               MOVE FNR                    TO OUTF-IO-AREA (1:3)
               MOVE KNR                    TO OUTF-IO-AREA (4:6)
               MOVE FAKTYP                 TO OUTF-IO-AREA (10:1)
               MOVE AVGTYP                 TO OUTF-IO-AREA (13:1)
               MOVE BETBET                 TO OUTF-IO-AREA (14:2)
               MOVE ONR                    TO OUTF-IO-AREA (19:6)
               MOVE 'L'                    TO OUTF-IO-AREA (25:1)
               MOVE 'A'                    TO OUTF-IO-AREA (37:1)
               IF  (I-22)
                   MOVE REKVNR             TO OUTF-IO-AREA (41:15)
               END-IF
               IF  (I-27)
                   MOVE FAKREF             TO OUTF-IO-AREA (56:6)
               END-IF
               IF  (I-23)
                   MOVE LAGERK             TO OUTF-IO-AREA (62:2)
               END-IF
               IF  (I-24)
                   MOVE BKKODE             TO OUTF-IO-AREA (66:1)
               END-IF
               MOVE AVGKOD                 TO OUTF-IO-AREA (67:1)
               IF  (I-25)
                   MOVE HND                TO OUTF-IO-AREA (68:3)
               END-IF
               MOVE ODATO                  TO OUTF-IO-AREA (71:6)
               MOVE VADR3                  TO OUTF-IO-AREA (82:30)
               IF  (I-77)
                   MOVE SERVT              TO OUTF-IO-AREA (132:1)
               END-IF
               MOVE NULANT                 TO XO-52P
               MOVE XO-52P-EF              TO OUTF-IO-AREA (133:4)
               MOVE NULANT                 TO XO-52P
               MOVE XO-52P-EF              TO OUTF-IO-AREA (137:4)
               MOVE AVD                    TO OUTF-IO-AREA (166:1)
               MOVE NULL-X                 TO XO-72P
               MOVE XO-72P-EF              TO OUTF-IO-AREA (171:5)
               MOVE ' '                    TO OUTF-IO-AREA (177:1)
               MOVE OGEBYR                 TO OUTF-IO-AREA (178:1)
               IF  (I-26)
                   MOVE KRETYP             TO OUTF-IO-AREA (179:1)
               END-IF
               MOVE FAKKRN                 TO OUTF-IO-AREA (181:1)
               MOVE KNR                    TO OUTF-IO-AREA (184:6)
      *****************************************************************
      * VAREADRESSE SOM TEKSTRECORD 4   (FAKT.MÅTE 5)                *
      *****************************************************************
               WRITE OUTF-IO-AREA
           END-IF
           IF  (I-04 AND I-84)
               MOVE SPACES TO OUTF-IO-AREA
               INITIALIZE OUTF-IO-AREA
               MOVE FNR                    TO OUTF-IO-AREA (1:3)
               MOVE KNR                    TO OUTF-IO-AREA (4:6)
               MOVE FAKTYP                 TO OUTF-IO-AREA (10:1)
               MOVE AVGTYP                 TO OUTF-IO-AREA (13:1)
               MOVE BETBET                 TO OUTF-IO-AREA (14:2)
               MOVE ONR                    TO OUTF-IO-AREA (19:6)
               MOVE 'L'                    TO OUTF-IO-AREA (25:1)
               MOVE 'A'                    TO OUTF-IO-AREA (37:1)
               IF  (I-22)
                   MOVE REKVNR             TO OUTF-IO-AREA (41:15)
               END-IF
               IF  (I-27)
                   MOVE FAKREF             TO OUTF-IO-AREA (56:6)
               END-IF
               IF  (I-23)
                   MOVE LAGERK             TO OUTF-IO-AREA (62:2)
               END-IF
               IF  (I-24)
                   MOVE BKKODE             TO OUTF-IO-AREA (66:1)
               END-IF
               MOVE AVGKOD                 TO OUTF-IO-AREA (67:1)
               IF  (I-25)
                   MOVE HND                TO OUTF-IO-AREA (68:3)
               END-IF
               MOVE ODATO                  TO OUTF-IO-AREA (71:6)
               MOVE VADR4                  TO OUTF-IO-AREA (82:20)
               IF  (I-77)
                   MOVE SERVT              TO OUTF-IO-AREA (132:1)
               END-IF
               MOVE NULANT                 TO XO-52P
               MOVE XO-52P-EF              TO OUTF-IO-AREA (133:4)
               MOVE NULANT                 TO XO-52P
               MOVE XO-52P-EF              TO OUTF-IO-AREA (137:4)
               MOVE AVD                    TO OUTF-IO-AREA (166:1)
               MOVE NULL-X                 TO XO-72P
               MOVE XO-72P-EF              TO OUTF-IO-AREA (171:5)
               MOVE ' '                    TO OUTF-IO-AREA (177:1)
               MOVE OGEBYR                 TO OUTF-IO-AREA (178:1)
               IF  (I-26)
                   MOVE KRETYP             TO OUTF-IO-AREA (179:1)
               END-IF
               MOVE FAKKRN                 TO OUTF-IO-AREA (181:1)
               MOVE KNR                    TO OUTF-IO-AREA (184:6)
      *****************************************************************
      *  VANLIG FAKTURA RECORDS.                                      *
      *****************************************************************
               WRITE OUTF-IO-AREA
           END-IF
           IF  (I-06 AND NOT-I-14 AND I-20)
           AND (NOT-I-64)
           OR  (I-06 AND NOT-I-14 AND I-40)
           AND (NOT-I-64)
               MOVE SPACES TO OUTF-IO-AREA
               INITIALIZE OUTF-IO-AREA
               MOVE FNR                    TO OUTF-IO-AREA (1:3)
               MOVE KNR                    TO OUTF-IO-AREA (4:6)
               MOVE FAKTYP                 TO OUTF-IO-AREA (10:1)
               MOVE AVGTYP                 TO OUTF-IO-AREA (13:1)
               MOVE BETBET                 TO OUTF-IO-AREA (14:2)
               MOVE ONR                    TO OUTF-IO-AREA (19:6)
               MOVE 'L'                    TO OUTF-IO-AREA (25:1)
               IF  (I-51 AND I-95)
                   MOVE 'B'                TO OUTF-IO-AREA (36:1)
               END-IF
               MOVE ' '                    TO OUTF-IO-AREA (37:1)
               IF  (I-22)
                   MOVE REKVNR             TO OUTF-IO-AREA (41:15)
               END-IF
               IF  (I-27)
                   MOVE FAKREF             TO OUTF-IO-AREA (56:6)
               END-IF
               IF  (I-23)
                   MOVE LAGERK             TO OUTF-IO-AREA (62:2)
               END-IF
               IF  (I-28)
                   MOVE 'R'                TO OUTF-IO-AREA (64:1)
               END-IF
               MOVE VLKOD1                 TO OUTF-IO-AREA (65:1)
               IF  (I-24)
                   MOVE BKKODE             TO OUTF-IO-AREA (66:1)
               END-IF
               MOVE AVGKOD                 TO OUTF-IO-AREA (67:1)
               IF  (I-25)
                   MOVE HND                TO OUTF-IO-AREA (68:3)
               END-IF
               MOVE ODATO                  TO OUTF-IO-AREA (71:6)
               MOVE VGRFAK                 TO OUTF-IO-AREA (77:5)
               MOVE ARTNR                  TO OUTF-IO-AREA (82:20)
               IF  (I-46 AND I-79)
                   MOVE '*'                TO OUTF-IO-AREA (100:1)
               END-IF
               IF  (I-69 AND I-57 AND I-59)
                   MOVE ALFAK              TO OUTF-IO-AREA (82:3)
               END-IF
               IF  (I-69 AND I-57 AND I-59)
                   MOVE ' '                TO OUTF-IO-AREA (85:1)
               END-IF
               IF  (I-69 AND I-57 AND I-59)
                   MOVE ART16F             TO OUTF-IO-AREA (86:16)
               END-IF
               MOVE VAREB                  TO OUTF-IO-AREA (102:30)
               IF  (I-77)
                   MOVE SERVT              TO OUTF-IO-AREA (132:1)
               END-IF
               MOVE BESANT                 TO XO-52P
               MOVE XO-52P-EF              TO OUTF-IO-AREA (133:4)
               MOVE LEVANT                 TO XO-52P
               MOVE XO-52P-EF              TO OUTF-IO-AREA (137:4)
               MOVE EDBNR                  TO XO-70U
               MOVE XO-70U (1:7)           TO OUTF-IO-AREA (141:7)
               MOVE RAB1                   TO XO-21U
               MOVE XO-21U (1:3)           TO OUTF-IO-AREA (148:3)
               IF RAB2 < 0
                 MOVE RAB2                 TO XO-21D
                 MOVE XO-21D (1:3)         TO OUTF-IO-AREA (151:3)
               ELSE
                 MOVE RAB2                 TO XO-21U
                 MOVE XO-21U (1:3)         TO OUTF-IO-AREA (151:3)
               END-IF
               IF RAB3 < 0
                 MOVE RAB3                 TO XO-21D
                 MOVE XO-21D (1:3)         TO OUTF-IO-AREA (154:3)
               ELSE
                 MOVE RAB3                 TO XO-21U
                 MOVE XO-21U (1:3)         TO OUTF-IO-AREA (154:3)
               END-IF
               MOVE BELO-ELGP              TO XO-72U
               MOVE XO-72U (1:9)           TO OUTF-IO-AREA (157:9)
               MOVE AVD                    TO OUTF-IO-AREA (166:1)
               MOVE SVS                    TO XO-72P
               MOVE XO-72P-EF              TO OUTF-IO-AREA (171:5)
               MOVE ' '                    TO OUTF-IO-AREA (177:1)
               MOVE OGEBYR                 TO OUTF-IO-AREA (178:1)
               IF  (I-26)
                   MOVE KRETYP             TO OUTF-IO-AREA (179:1)
               END-IF
               MOVE FAKKRN                 TO OUTF-IO-AREA (181:1)
               MOVE KNR                    TO OUTF-IO-AREA (184:6)
               MOVE ALFAK                  TO OUTF-IO-AREA (190:3)
      *****************************************************************
      *  FAKTURA TILEGGSRECORD (PANT,NRK.AVGIFT,OSV)                  *
      *           TILDELES LAGERKODE PT.                              *
      *           SKAL IKKE OPPDATERE VARE.STAT.MASTER MED ANTALL.    *
      *****************************************************************
               WRITE OUTF-IO-AREA
           END-IF
           IF  (I-06 AND I-55)
               MOVE SPACES TO OUTF-IO-AREA
               INITIALIZE OUTF-IO-AREA
               MOVE FNR                    TO OUTF-IO-AREA (1:3)
               MOVE KNR                    TO OUTF-IO-AREA (4:6)
               MOVE FAKTYP                 TO OUTF-IO-AREA (10:1)
               MOVE AVGTYP                 TO OUTF-IO-AREA (13:1)
               MOVE BETBET                 TO OUTF-IO-AREA (14:2)
               MOVE ONR                    TO OUTF-IO-AREA (19:6)
               MOVE 'L'                    TO OUTF-IO-AREA (25:1)
               IF  (I-51 AND I-95)
                   MOVE 'B'                TO OUTF-IO-AREA (36:1)
               END-IF
               MOVE ' '                    TO OUTF-IO-AREA (37:1)
               IF  (I-22)
                   MOVE REKVNR             TO OUTF-IO-AREA (41:15)
               END-IF
               IF  (I-27)
                   MOVE FAKREF             TO OUTF-IO-AREA (56:6)
               END-IF
               MOVE 'PT'                   TO OUTF-IO-AREA (62:2)
               IF  (I-28)
                   MOVE 'R'                TO OUTF-IO-AREA (64:1)
               END-IF
               MOVE VLKOD1                 TO OUTF-IO-AREA (65:1)
               IF  (I-24)
                   MOVE BKKODE             TO OUTF-IO-AREA (66:1)
               END-IF
               MOVE AVGKOD                 TO OUTF-IO-AREA (67:1)
               IF  (I-25)
                   MOVE HND                TO OUTF-IO-AREA (68:3)
               END-IF
               MOVE ODATO                  TO OUTF-IO-AREA (71:6)
               MOVE VGRTIL                 TO OUTF-IO-AREA (77:5)
               MOVE ARTNR                  TO OUTF-IO-AREA (82:20)
               IF  (I-46 AND I-79)
                   MOVE '*'                TO OUTF-IO-AREA (100:1)
               END-IF
               IF  (I-69 AND I-57 AND I-59)
                   MOVE ALFAK              TO OUTF-IO-AREA (82:3)
               END-IF
               IF  (I-69 AND I-57 AND I-59)
                   MOVE ' '                TO OUTF-IO-AREA (85:1)
               END-IF
               IF  (I-69 AND I-57 AND I-59)
                   MOVE ART16F             TO OUTF-IO-AREA (86:16)
               END-IF
               IF  (I-49)
                   MOVE TABTTE (TABTTE-I)  TO OUTF-IO-AREA (102:30)
               END-IF
               IF  (NOT-I-49)
                   MOVE '  PANT              ' TO OUTF-IO-AREA (102:20)
               END-IF
               IF  (I-77)
                   MOVE SERVT              TO OUTF-IO-AREA (132:1)
               END-IF
               MOVE BESANT                 TO XO-52P
               MOVE XO-52P-EF              TO OUTF-IO-AREA (133:4)
               MOVE LEVANT                 TO XO-52P
               MOVE XO-52P-EF              TO OUTF-IO-AREA (137:4)
               IF EDBNR < 0
                 MOVE EDBNR                TO XO-70D
                 MOVE XO-70D (1:7)         TO OUTF-IO-AREA (141:7)
               ELSE
                 MOVE EDBNR                TO XO-70U
                 MOVE XO-70U (1:7)         TO OUTF-IO-AREA (141:7)
               END-IF
               MOVE '000000000'            TO OUTF-IO-AREA (148:9)
               MOVE PRITIL-IO              TO OUTF-IO-AREA (157:9)
               MOVE AVD                    TO OUTF-IO-AREA (166:1)
               MOVE SVSTIL                 TO XO-72P
               MOVE XO-72P-EF              TO OUTF-IO-AREA (171:5)
               MOVE ' '                    TO OUTF-IO-AREA (177:1)
               MOVE OGEBYR                 TO OUTF-IO-AREA (178:1)
               IF  (I-26)
                   MOVE KRETYP             TO OUTF-IO-AREA (179:1)
               END-IF
               MOVE FAKKRN                 TO OUTF-IO-AREA (181:1)
               MOVE KNR                    TO OUTF-IO-AREA (184:6)
               MOVE ALFAK                  TO OUTF-IO-AREA (190:3)
      *****************************************************************
      *  TEKSTRECORD  (IKKE VAREADRESSE)                              *
      *****************************************************************
               WRITE OUTF-IO-AREA
           END-IF
           IF  (I-06 AND I-14 AND I-15)
               MOVE SPACES TO OUTF-IO-AREA
               INITIALIZE OUTF-IO-AREA
               MOVE FNR                    TO OUTF-IO-AREA (1:3)
               MOVE KNR                    TO OUTF-IO-AREA (4:6)
               MOVE FAKTYP                 TO OUTF-IO-AREA (10:1)
               MOVE AVGTYP                 TO OUTF-IO-AREA (13:1)
               MOVE BETBET                 TO OUTF-IO-AREA (14:2)
               MOVE ONR                    TO OUTF-IO-AREA (19:6)
               MOVE 'L'                    TO OUTF-IO-AREA (25:1)
               IF  (I-51 AND I-95)
                   MOVE 'B'                TO OUTF-IO-AREA (36:1)
               END-IF
               MOVE ' '                    TO OUTF-IO-AREA (37:1)
               IF  (I-22)
                   MOVE REKVNR             TO OUTF-IO-AREA (41:15)
               END-IF
               IF  (I-27)
                   MOVE FAKREF             TO OUTF-IO-AREA (56:6)
               END-IF
               IF  (I-23)
                   MOVE LAGERK             TO OUTF-IO-AREA (62:2)
               END-IF
               IF  (I-24)
                   MOVE BKKODE             TO OUTF-IO-AREA (66:1)
               END-IF
               MOVE AVGKOD                 TO OUTF-IO-AREA (67:1)
               IF  (I-25)
                   MOVE HND                TO OUTF-IO-AREA (68:3)
               END-IF
               MOVE ODATO                  TO OUTF-IO-AREA (71:6)
               MOVE TEKST                  TO OUTF-IO-AREA (82:50)
               IF  (I-77)
                   MOVE SERVT              TO OUTF-IO-AREA (132:1)
               END-IF
               MOVE NULANT                 TO XO-52P
               MOVE XO-52P-EF              TO OUTF-IO-AREA (133:4)
               MOVE NULANT                 TO XO-52P
               MOVE XO-52P-EF              TO OUTF-IO-AREA (137:4)
               MOVE AVD                    TO OUTF-IO-AREA (166:1)
               MOVE SVS                    TO XO-72P
               MOVE XO-72P-EF              TO OUTF-IO-AREA (171:5)
               MOVE ' '                    TO OUTF-IO-AREA (177:1)
               MOVE OGEBYR                 TO OUTF-IO-AREA (178:1)
               IF  (I-26)
                   MOVE KRETYP             TO OUTF-IO-AREA (179:1)
               END-IF
               MOVE FAKKRN                 TO OUTF-IO-AREA (181:1)
               MOVE KNR                    TO OUTF-IO-AREA (184:6)
      *****************************************************************
      *  FRAKTUTGEVNINGSRECORD FOMA.                                  *
      *****************************************************************
               WRITE OUTF-IO-AREA
           END-IF
           IF  (I-06 AND I-47 AND I-88)
               MOVE SPACES TO OUTFRAK-IO-AREA
               INITIALIZE OUTFRAK-IO-AREA
               IF  (NOT-I-64)
                   MOVE 'A'                TO OUTFRAK-IO-AREA (1:1)
               END-IF
               IF  (I-64)
                   MOVE 'F'                TO OUTFRAK-IO-AREA (1:1)
               END-IF
               MOVE FNR                    TO OUTFRAK-IO-AREA (2:3)
               MOVE KNR                    TO OUTFRAK-IO-AREA (5:6)
               MOVE ONR                    TO OUTFRAK-IO-AREA (11:6)
               MOVE ODATO                  TO OUTFRAK-IO-AREA (17:6)
               MOVE ALFAK                  TO OUTFRAK-IO-AREA (23:3)
               MOVE ARTNR                  TO OUTFRAK-IO-AREA (26:20)
               IF EDBNR < 0
                 MOVE EDBNR                TO XO-70D
                 MOVE XO-70D (1:7)         TO OUTFRAK-IO-AREA (46:7)
               ELSE
                 MOVE EDBNR                TO XO-70U
                 MOVE XO-70U (1:7)         TO OUTFRAK-IO-AREA (46:7)
               END-IF
               IF BELO-ELGP < 0
                 MOVE BELO-ELGP            TO XO-72D
                 MOVE XO-72D (1:9)         TO OUTFRAK-IO-AREA (53:9)
               ELSE
                 MOVE BELO-ELGP            TO XO-72U
                 MOVE XO-72U (1:9)         TO OUTFRAK-IO-AREA (53:9)
               END-IF
               MOVE ORDSUM-IO              TO OUTFRAK-IO-AREA (62:9)
               MOVE BESSUM-IO              TO OUTFRAK-IO-AREA (71:9)
               MOVE BSFREK                 TO OUTFRAK-IO-AREA (80:1)
               MOVE ORDFGR-IO              TO OUTFRAK-IO-AREA (81:9)
               WRITE OUTFRAK-IO-AREA
           END-IF
           IF  (I-1P)
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE 'PROG FAK030  DATO'    TO TOTALER-IO-AREA (2:17)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO TOTALER-IO-AREA (20:8)
               MOVE 'DAGLIG OPPDATERING AV' TO TOTALER-IO-AREA (50:21)
               MOVE 'FAKTURAFILE.'         TO TOTALER-IO-AREA (72:12)
               MOVE 01                     TO TOTALER-BEFORE-SKIP
               MOVE 2                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE 'FIRMA'                TO TOTALER-IO-AREA (2:5)
               MOVE 'GML.FAKTURAFILE'      TO TOTALER-IO-AREA (40:15)
               MOVE 'FRA ORDREFILE'        TO TOTALER-IO-AREA (59:13)
               MOVE 'NYE TILLEGGS'         TO TOTALER-IO-AREA (82:12)
               MOVE 'NY FAKTURAFILE'       TO TOTALER-IO-AREA (98:14)
               MOVE 'ANMERK.'              TO TOTALER-IO-AREA (114:7)
               MOVE 1                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE 'ORDRE'                TO TOTALER-IO-AREA (40:5)
               MOVE 'RECORD'               TO TOTALER-IO-AREA (49:6)
               MOVE 'ORDRE'                TO TOTALER-IO-AREA (59:5)
               MOVE 'RECORD'               TO TOTALER-IO-AREA (68:6)
               MOVE 'RECORD'               TO TOTALER-IO-AREA (87:6)
               MOVE 'ORDRE'                TO TOTALER-IO-AREA (97:5)
               MOVE 'RECORD'               TO TOTALER-IO-AREA (106:6)
               MOVE 1                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE '------------------------' TO TOTALER-IO-AREA
                                                                (1:24)
               MOVE '------------------------' TO TOTALER-IO-AREA
                                                               (25:24)
               MOVE '------------------------' TO TOTALER-IO-AREA
                                                               (49:24)
               MOVE '------------------------' TO TOTALER-IO-AREA
                                                               (73:24)
               MOVE '------------------------' TO TOTALER-IO-AREA
                                                               (97:24)
               MOVE 2                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
           END-IF
           IF  (I-06 AND I-47 AND I-64)
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE FIRMNR                 TO TOTALER-IO-AREA (3:3)
               MOVE 'KNR'                  TO TOTALER-IO-AREA (7:3)
               MOVE KNR                    TO TOTALER-IO-AREA (11:6)
               MOVE 'ONR'                  TO TOTALER-IO-AREA (18:3)
               MOVE ONR                    TO TOTALER-IO-AREA (22:6)
               MOVE 'BESØKFREK'            TO TOTALER-IO-AREA (29:9)
               MOVE BSFREK                 TO TOTALER-IO-AREA (40:1)
               MOVE 'ORDRETOT'             TO TOTALER-IO-AREA (42:8)
               MOVE BESSUM                 TO XO-72YY9R
               MOVE XO-72YY9R              TO TOTALER-IO-AREA (53:13)
               MOVE 'FJ.FRAKT'             TO TOTALER-IO-AREA (66:8)
               MOVE BELO-ELGP              TO XO-72YY9R
               MOVE XO-72YY9R              TO TOTALER-IO-AREA (74:13)
               MOVE POSNR                  TO TOTALER-IO-AREA (88:3)
               MOVE ARTN6F                 TO TOTALER-IO-AREA (92:6)
               MOVE 'BEL.GR='              TO TOTALER-IO-AREA (99:7)
               MOVE ORDFGR                 TO XO-72YY9
               MOVE XO-72YY9               TO TOTALER-IO-AREA (106:12)
               MOVE 1                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
           END-IF.
 
       DETAIL-OVERFLOW SECTION.
       DETAIL-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE 'PROG FAK030  DATO'    TO TOTALER-IO-AREA (2:17)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO TOTALER-IO-AREA (20:8)
               MOVE 'DAGLIG OPPDATERING AV' TO TOTALER-IO-AREA (50:21)
               MOVE 'FAKTURAFILE.'         TO TOTALER-IO-AREA (72:12)
               MOVE 01                     TO TOTALER-BEFORE-SKIP
               MOVE 2                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE 'FIRMA'                TO TOTALER-IO-AREA (2:5)
               MOVE 'GML.FAKTURAFILE'      TO TOTALER-IO-AREA (40:15)
               MOVE 'FRA ORDREFILE'        TO TOTALER-IO-AREA (59:13)
               MOVE 'NYE TILLEGGS'         TO TOTALER-IO-AREA (82:12)
               MOVE 'NY FAKTURAFILE'       TO TOTALER-IO-AREA (98:14)
               MOVE 'ANMERK.'              TO TOTALER-IO-AREA (114:7)
               MOVE 1                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE 'ORDRE'                TO TOTALER-IO-AREA (40:5)
               MOVE 'RECORD'               TO TOTALER-IO-AREA (49:6)
               MOVE 'ORDRE'                TO TOTALER-IO-AREA (59:5)
               MOVE 'RECORD'               TO TOTALER-IO-AREA (68:6)
               MOVE 'RECORD'               TO TOTALER-IO-AREA (87:6)
               MOVE 'ORDRE'                TO TOTALER-IO-AREA (97:5)
               MOVE 'RECORD'               TO TOTALER-IO-AREA (106:6)
               MOVE 1                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE '------------------------' TO TOTALER-IO-AREA
                                                                (1:24)
               MOVE '------------------------' TO TOTALER-IO-AREA
                                                               (25:24)
               MOVE '------------------------' TO TOTALER-IO-AREA
                                                               (49:24)
               MOVE '------------------------' TO TOTALER-IO-AREA
                                                               (73:24)
               MOVE '------------------------' TO TOTALER-IO-AREA
                                                               (97:24)
               MOVE 2                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-93 AND I-76)
               MOVE SPACES TO OUTF-IO-AREA
               INITIALIZE OUTF-IO-AREA
               MOVE FNR                    TO OUTF-IO-AREA (1:3)
               MOVE KNR                    TO OUTF-IO-AREA (4:6)
               MOVE FAKTYP                 TO OUTF-IO-AREA (10:1)
               MOVE AVGTYP                 TO OUTF-IO-AREA (13:1)
               MOVE BETBET                 TO OUTF-IO-AREA (14:2)
               MOVE ONR                    TO OUTF-IO-AREA (19:6)
               MOVE 'L'                    TO OUTF-IO-AREA (25:1)
               MOVE ' '                    TO OUTF-IO-AREA (37:1)
               IF  (I-22)
                   MOVE REKVNR             TO OUTF-IO-AREA (41:15)
               END-IF
               IF  (I-27)
                   MOVE FAKREF             TO OUTF-IO-AREA (56:6)
               END-IF
               IF  (I-23)
                   MOVE LAGERK             TO OUTF-IO-AREA (62:2)
               END-IF
               IF  (I-28)
                   MOVE 'R'                TO OUTF-IO-AREA (64:1)
               END-IF
               IF  (I-24)
                   MOVE BKKODE             TO OUTF-IO-AREA (66:1)
               END-IF
               MOVE AVGKOD                 TO OUTF-IO-AREA (67:1)
               IF  (I-25)
                   MOVE HND                TO OUTF-IO-AREA (68:3)
               END-IF
               MOVE ODATO                  TO OUTF-IO-AREA (71:6)
               MOVE '99030'                TO OUTF-IO-AREA (77:5)
               MOVE FRPROS                 TO XO-11YY9
               MOVE XO-11YY9               TO OUTF-IO-AREA (82:3)
               MOVE ' PROSENT         '    TO OUTF-IO-AREA (85:17)
               IF  (NOT-I-91)
                   MOVE 'SONEFRAKTTILLEGG    ' TO OUTF-IO-AREA (102:20)
               END-IF
               IF  (I-91)
                   MOVE 'FAST FRAKTTILLEGG   ' TO OUTF-IO-AREA (102:20)
               END-IF
               MOVE '          '           TO OUTF-IO-AREA (122:10)
               IF  (I-77)
                   MOVE SERVT              TO OUTF-IO-AREA (132:1)
               END-IF
               MOVE ANT1                   TO XO-52P
               MOVE XO-52P-EF              TO OUTF-IO-AREA (133:4)
               MOVE ANT1                   TO XO-52P
               MOVE XO-52P-EF              TO OUTF-IO-AREA (137:4)
               MOVE '9700005'              TO OUTF-IO-AREA (141:7)
               MOVE '000'                  TO OUTF-IO-AREA (148:3)
               MOVE '000'                  TO OUTF-IO-AREA (151:3)
               MOVE '000'                  TO OUTF-IO-AREA (154:3)
               MOVE NYFRAK-IO              TO OUTF-IO-AREA (157:9)
               MOVE AVD                    TO OUTF-IO-AREA (166:1)
               MOVE NULL-X                 TO XO-72P
               MOVE XO-72P-EF              TO OUTF-IO-AREA (171:5)
               MOVE ' '                    TO OUTF-IO-AREA (177:1)
               MOVE OGEBYR                 TO OUTF-IO-AREA (178:1)
               IF  (I-26)
                   MOVE KRETYP             TO OUTF-IO-AREA (179:1)
               END-IF
               MOVE 'E'                    TO OUTF-IO-AREA (180:1)
               MOVE FAKKRN                 TO OUTF-IO-AREA (181:1)
               MOVE KNR                    TO OUTF-IO-AREA (184:6)
      *****************************************************************
      *  ORDREGEBYR RECORD.                                           *
      *****************************************************************
               WRITE OUTF-IO-AREA
           END-IF
           IF  (I-L1 AND I-70)
               MOVE SPACES TO OUTF-IO-AREA
               INITIALIZE OUTF-IO-AREA
               MOVE FNR                    TO OUTF-IO-AREA (1:3)
               MOVE KNR                    TO OUTF-IO-AREA (4:6)
               MOVE FAKTYP                 TO OUTF-IO-AREA (10:1)
               MOVE AVGTYP                 TO OUTF-IO-AREA (13:1)
               MOVE BETBET                 TO OUTF-IO-AREA (14:2)
               MOVE ONR                    TO OUTF-IO-AREA (19:6)
               MOVE 'L'                    TO OUTF-IO-AREA (25:1)
               IF  (I-51 AND I-95)
                   MOVE 'B'                TO OUTF-IO-AREA (36:1)
               END-IF
               MOVE ' '                    TO OUTF-IO-AREA (37:1)
               IF  (I-22)
                   MOVE REKVNR             TO OUTF-IO-AREA (41:15)
               END-IF
               IF  (I-27)
                   MOVE FAKREF             TO OUTF-IO-AREA (56:6)
               END-IF
               IF  (I-23)
                   MOVE LAGERK             TO OUTF-IO-AREA (62:2)
               END-IF
               IF  (I-28)
                   MOVE 'R'                TO OUTF-IO-AREA (64:1)
               END-IF
               IF  (I-24)
                   MOVE BKKODE             TO OUTF-IO-AREA (66:1)
               END-IF
               MOVE AVGKOD                 TO OUTF-IO-AREA (67:1)
               IF  (I-25)
                   MOVE HND                TO OUTF-IO-AREA (68:3)
               END-IF
               MOVE ODATO                  TO OUTF-IO-AREA (71:6)
               MOVE SVGR                   TO OUTF-IO-AREA (77:5)
               MOVE STKS1                  TO OUTF-IO-AREA (82:20)
               MOVE STKS2                  TO OUTF-IO-AREA (102:30)
               IF  (I-77)
                   MOVE SERVT              TO OUTF-IO-AREA (132:1)
               END-IF
               MOVE GEBANT                 TO XO-52P
               MOVE XO-52P-EF              TO OUTF-IO-AREA (133:4)
               MOVE GEBANT                 TO XO-52P
               MOVE XO-52P-EF              TO OUTF-IO-AREA (137:4)
               MOVE SEDBNR                 TO OUTF-IO-AREA (141:7)
               MOVE '000'                  TO OUTF-IO-AREA (148:3)
               MOVE '000'                  TO OUTF-IO-AREA (151:3)
               MOVE '000'                  TO OUTF-IO-AREA (154:3)
               MOVE RABGEB-IO              TO OUTF-IO-AREA (157:9)
               MOVE AVD                    TO OUTF-IO-AREA (166:1)
               MOVE NULL-X                 TO XO-72P
               MOVE XO-72P-EF              TO OUTF-IO-AREA (171:5)
               MOVE ' '                    TO OUTF-IO-AREA (177:1)
               MOVE OGEBYR                 TO OUTF-IO-AREA (178:1)
               IF  (I-26)
                   MOVE KRETYP             TO OUTF-IO-AREA (179:1)
               END-IF
               MOVE 'E'                    TO OUTF-IO-AREA (180:1)
               MOVE FAKKRN                 TO OUTF-IO-AREA (181:1)
               MOVE KNR                    TO OUTF-IO-AREA (184:6)
      *****************************************************************
      *  FRAKTRECORDS KCL TIL KONTROLLISTE. A=ANFØRER FRAKT, F=FJERNET*
      *****************************************************************
               WRITE OUTF-IO-AREA
           END-IF
           IF  (I-L1 AND I-70 AND I-80)
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE FIRMNR                 TO TOTALER-IO-AREA (3:3)
               MOVE 'KNR'                  TO TOTALER-IO-AREA (7:3)
               MOVE KNR                    TO TOTALER-IO-AREA (11:6)
               MOVE 'ONR'                  TO TOTALER-IO-AREA (18:3)
               MOVE ONR                    TO TOTALER-IO-AREA (22:6)
               MOVE 'GEBYRKODE'            TO TOTALER-IO-AREA (29:9)
               MOVE OGEBYR                 TO TOTALER-IO-AREA (39:1)
               MOVE 'GEBYR'                TO TOTALER-IO-AREA (41:5)
               MOVE RABGEB                 TO XO-72YY9R
               MOVE XO-72YY9R              TO TOTALER-IO-AREA (53:13)
               MOVE 1                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
           END-IF
           IF  (I-L1 AND I-93 AND I-76)
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE FIRMNR                 TO TOTALER-IO-AREA (3:3)
               MOVE 'KNR'                  TO TOTALER-IO-AREA (7:3)
               MOVE KNR                    TO TOTALER-IO-AREA (11:6)
               MOVE 'ONR'                  TO TOTALER-IO-AREA (18:3)
               MOVE ONR                    TO TOTALER-IO-AREA (22:6)
               MOVE 'KUNDEKAT '            TO TOTALER-IO-AREA (29:9)
               MOVE KUNKAT                 TO TOTALER-IO-AREA (38:3)
               MOVE 'ORDRETOT'             TO TOTALER-IO-AREA (42:8)
               MOVE FROSUM                 TO XO-72YY9R
               MOVE XO-72YY9R              TO TOTALER-IO-AREA (53:13)
               MOVE 'FRAKTTOT'             TO TOTALER-IO-AREA (66:8)
               MOVE NYFRAK                 TO XO-72YY9R
               MOVE XO-72YY9R              TO TOTALER-IO-AREA (74:13)
               MOVE FRPROS                 TO XO-11YY9
               MOVE XO-11YY9               TO TOTALER-IO-AREA (88:3)
               MOVE '%'                    TO TOTALER-IO-AREA (91:1)
               MOVE 1                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
           END-IF
           IF  (I-L2 AND I-93)
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE FIRMNR                 TO TOTALER-IO-AREA (3:3)
               IF  (NOT-I-16)
                   MOVE FINAVN             TO TOTALER-IO-AREA (8:30)
               END-IF
               MOVE 'ANT.GML.FRAKTR'       TO TOTALER-IO-AREA (41:14)
               MOVE ANTFRF                 TO XO-50YY9
               MOVE XO-50YY9               TO TOTALER-IO-AREA (57:6)
               MOVE 'ANT.NYE FRAKTR'       TO TOTALER-IO-AREA (67:14)
               MOVE ANTFRB                 TO XO-50YY9
               MOVE XO-50YY9               TO TOTALER-IO-AREA (83:6)
               MOVE 'TOT. NY FRAKT'        TO TOTALER-IO-AREA (90:13)
               MOVE TOTNYF                 TO XO-72YY9
               MOVE XO-72YY9               TO TOTALER-IO-AREA (104:12)
               MOVE 1                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
           END-IF
           IF  (I-L2)
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE FIRMNR                 TO TOTALER-IO-AREA (3:3)
               IF  (NOT-I-16)
                   MOVE FINAVN             TO TOTALER-IO-AREA (8:30)
               END-IF
               MOVE AOL2G                  TO XO-50YY9
               MOVE XO-50YY9               TO TOTALER-IO-AREA (39:6)
               INITIALIZE AOL2G
               MOVE ARL2G                  TO XO-60YY9
               MOVE XO-60YY9               TO TOTALER-IO-AREA (48:7)
               INITIALIZE ARL2G
               MOVE AOL2N                  TO XO-50YY9
               MOVE XO-50YY9               TO TOTALER-IO-AREA (58:6)
               INITIALIZE AOL2N
               MOVE ARL2N                  TO XO-60YY9
               MOVE XO-60YY9               TO TOTALER-IO-AREA (67:7)
               INITIALIZE ARL2N
               MOVE ARL2F                  TO XO-50YY9
               MOVE XO-50YY9               TO TOTALER-IO-AREA (87:6)
               INITIALIZE ARL2F
               MOVE AOL2                   TO XO-50YY9
               MOVE XO-50YY9               TO TOTALER-IO-AREA (96:6)
               INITIALIZE AOL2
               MOVE ARL2                   TO XO-60YY9
               MOVE XO-60YY9               TO TOTALER-IO-AREA (105:7)
               INITIALIZE ARL2
               MOVE 1                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
           END-IF
           IF  (I-L2 AND I-57)
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE FIRMNR                 TO TOTALER-IO-AREA (3:3)
               IF  (NOT-I-16)
                   MOVE FINAVN             TO TOTALER-IO-AREA (8:30)
               END-IF
               MOVE 'ANT.ARTNR < 16'       TO TOTALER-IO-AREA (41:14)
               MOVE ANTKAN                 TO XO-50YY9
               MOVE XO-50YY9               TO TOTALER-IO-AREA (57:6)
               INITIALIZE ANTKAN
               MOVE 'ANT.ARTNR > 16'       TO TOTALER-IO-AREA (67:14)
               MOVE ANTKAN                 TO XO-50YY9
               MOVE XO-50YY9               TO TOTALER-IO-AREA (83:6)
               INITIALIZE ANTKAN
               MOVE 1                      TO TOTALER-AFTER-SPACE
               PERFORM TOTALER-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO TOTALER-IO-AREA
               INITIALIZE TOTALER-IO-AREA
               MOVE 'T O T A L T'          TO TOTALER-IO-AREA (8:11)
               MOVE AOLRG                  TO XO-50YY9
               MOVE XO-50YY9               TO TOTALER-IO-AREA (39:6)
               MOVE ARLRG                  TO XO-60YY9
               MOVE XO-60YY9               TO TOTALER-IO-AREA (48:7)
               MOVE AOLRN                  TO XO-50YY9
               MOVE XO-50YY9               TO TOTALER-IO-AREA (58:6)
               MOVE ARLRN                  TO XO-60YY9
               MOVE XO-60YY9               TO TOTALER-IO-AREA (67:7)
               MOVE ARLRF                  TO XO-50YY9
               MOVE XO-50YY9               TO TOTALER-IO-AREA (87:6)
               MOVE AOLR                   TO XO-50YY9
               MOVE XO-50YY9               TO TOTALER-IO-AREA (96:6)
               MOVE ARLR                   TO XO-60YY9
               MOVE XO-60YY9               TO TOTALER-IO-AREA (105:7)
               MOVE 1                      TO TOTALER-BEFORE-SPACE
               PERFORM TOTALER-PRINT-LINE
           END-IF
           IF  (I-LR AND I-97 AND NOT-I-U8)
               MOVE ORDFAK-IO              TO AVSTEMF-IO-AREA (53:6)
               REWRITE AVSTEMF-IO-AREA
                   INVALID KEY
                       DISPLAY 'Bad REWRITE - file = AVSTEMF'
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
           MOVE 2                          TO LR-CHECK
           PERFORM TEKSTAB-LOAD
           PERFORM OSKFILE-LOAD
           SET FAFILE-LEVEL-INIT           TO TRUE
           INITIALIZE FAFILE-DATA-FIELDS
           SET FAFILE-EOF-OFF              TO TRUE
           SET FAFILE-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO FAFILE-MC
                                              FAFILE-MP
           OPEN INPUT FAFILE
           SET ORDFAKT-LEVEL-INIT          TO TRUE
           INITIALIZE ORDFAKT-DATA-FIELDS
           SET ORDFAKT-EOF-OFF             TO TRUE
           SET ORDFAKT-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO ORDFAKT-MC
                                              ORDFAKT-MP
           OPEN INPUT ORDFAKT
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           INITIALIZE ORDNRM-DATA-FIELDS
           OPEN INPUT ORDNRM
           INITIALIZE VAREMAS-DATA-FIELDS
           OPEN INPUT VAREMAS
           INITIALIZE AVSTEMF-DATA-FIELDS
           OPEN I-O AVSTEMF
           OPEN OUTPUT OUTF
           OPEN OUTPUT OUTFRAK
           OPEN OUTPUT TOTALER
           INITIALIZE TOTALER-IO-AREA
           INITIALIZE TOTALER-DATA-FIELDS
           MOVE 57                         TO TOTALER-MAX-LINES.
           SET TABTTY-I                    TO 1
           SET TABOSK-I                    TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FAFILE
           CLOSE ORDFAKT
           CLOSE FIRMAF
           CLOSE ORDNRM
           CLOSE VAREMAS
           CLOSE AVSTEMF
           CLOSE OUTF
           CLOSE OUTFRAK
           IF TOTALER-IO-AREA NOT = SPACES
             WRITE TOTALER-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO TOTALER-IO-AREA
           END-IF
           CLOSE TOTALER.
 
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
 
       SETOFF-I-M SECTION.
           SET NOT-I-M1                    TO TRUE.
           SET NOT-I-M2                    TO TRUE.
           SET NOT-I-M3                    TO TRUE.
           SET NOT-I-M4                    TO TRUE.
           SET NOT-I-M5                    TO TRUE.
           SET NOT-I-M6                    TO TRUE.
           SET NOT-I-M7                    TO TRUE.
           SET NOT-I-M8                    TO TRUE.
           SET NOT-I-M9                    TO TRUE.
 
       SETON-I-M9 SECTION.
           SET I-M9                        TO TRUE.
           PERFORM SETON-I-M8.
 
       SETON-I-M8 SECTION.
           SET I-M8                        TO TRUE.
           PERFORM SETON-I-M7.
 
       SETON-I-M7 SECTION.
           SET I-M7                        TO TRUE.
           PERFORM SETON-I-M6.
 
       SETON-I-M6 SECTION.
           SET I-M6                        TO TRUE.
           PERFORM SETON-I-M5.
 
       SETON-I-M5 SECTION.
           SET I-M5                        TO TRUE.
           PERFORM SETON-I-M4.
 
       SETON-I-M4 SECTION.
           SET I-M4                        TO TRUE.
           PERFORM SETON-I-M3.
 
       SETON-I-M3 SECTION.
           SET I-M3                        TO TRUE.
           PERFORM SETON-I-M2.
 
       SETON-I-M2 SECTION.
           SET I-M2                        TO TRUE.
           PERFORM SETON-I-M1.
 
       SETON-I-M1 SECTION.
           SET I-M1                        TO TRUE.
 
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
