       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK075R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAM: FAK075                                    *
      * PROGRAMMET DANNER:                                 *
      * STATISTIKK-RECORDS.                                *
      * FAKTURAGEBYR RECORDS. (ORDREGEBYR TILDELES I FAK030)
      * FAKTURABONUS RECORDS.                              *
      * SUMFILE FOR OPPDATERING AV KJØP HITTIL I ÅR.       *
      * 06.05.93 DANNE SUMFILE PR VIRKELIG KUNDENR,        *
      *          ISTEDENFOR FAKTURA KUNDENR.               *
      * 15.02.94 UPSI 1 = FAKTURA.KONTANTSALGS.RUTINE      *
      *  8.05.96 ENDRING I FAKTURAGEBYR SUMMERINGSRUTINE.  *
      *          (FJERNET TEST PÅ OM ANT.LEV = 0)          *
      * 12.07.96 DANNE HASTERGEBYR RECORD.                 *
      *          (O.SØRENSEN SHELL KJEDEKUNDER)            *
      *          RECORDEN MERKES I FAK035.                 *
      * 12.12.96 REG.AVDELING UTVIDET MED A OG B.          *
      * 19.05.99 SERVICEORDRE MED SERVICEKODE G,K,A, BLIR  *
      *          SATT TIL 0 I BELØP PÅ FAKTURA-RECORD.     *
      * 04.02.00 KONSERNFIRMANR LAGT UT I SUMFILE.         *
      * 05.09.00 FAKTURAPÅSLAG I % FOR RADIOAVD. I S.OG B. *
      *          BRUKER BONUSRUTINE MED VANLIG EDBNR.      *
      * 19.09.00 RETTET FEIL I FAKTURAPÅSLAG FOR S.OG B.   *
      *  5.10.00 EGEN RUTINE FOR RADIO FRAKT OG FORSIKRING *
      *          P.G.A. ATT DETTE MÅ UT PR. ORDRE.         *
      *  5.06.01 EGEN RUTINE FOR RADIO FRAKT OG FORSIKRING *
      *          UTGÅR.                                    *
      * 14.08.01 RECORDART (L) SETTES INN I ALLE TILLEGGS- *
      *          RECORD SOM DANNES.                        *
      * 20.06.03 VARELINJEKODE 1 LEGGES INN I KR.NOTA TYPE *
      *          NÅR DET IKKE ER KR.NOTA. (SELGERORDRE 923)*
      *  9.09.03 IKKE FAKTURAGEBYR OM DET ER FINNES ORDRE  *
      *          MED IKKE GEBYR PÅ EN SAMLEFAKTURA. DA     *
      *          DISSE ORDRE IKKE ER MED I FAKTURASUMMEN.  *
      * 14.01.04 RENTENOTA BLIR MERKET MED LAGERKODE "RG"  *
      *          RENTENOTA MED 0 I ANTALL ENDRES TIL 1,00. *
      *          ALT DETTE I VAREREC.                      *
      *          P.G.A. FEIL UTREGNING I STATISTIKKER.     *
      ******************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK075.rpg
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
           SELECT OSKFILE
               ASSIGN TO UT-S-OSKFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OSKFILE-STATUS.
           SELECT BONUST
               ASSIGN TO UT-S-BONUST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS BONUST-STATUS.
           SELECT FLOPP
               ASSIGN TO UT-S-FLOPP
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FLOPP-STATUS.
           SELECT FAKTIN
               ASSIGN TO UT-S-FAKTIN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKTIN-STATUS.
           SELECT FAKPAR
               ASSIGN TO UT-S-FAKPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKPAR-STATUS.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT KUNDEMX
               ASSIGN TO KUNDEMX
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMX-STATUS
               RECORD KEY IS KUNDEMX-KEY1.
           SELECT VAREREC
               ASSIGN TO UT-S-VAREREC
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VAREREC-STATUS.
           SELECT FAKTUT
               ASSIGN TO UT-S-FAKTUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKTUT-STATUS.
           SELECT SUMFILE
               ASSIGN TO UT-S-SUMFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS SUMFILE-STATUS.
           SELECT PRF
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PRF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD OSKFILE
               BLOCK CONTAINS 4070
               RECORD CONTAINS 110.
       01  OSKFILE-IO-AREA.
           05  OSKFILE-IO-AREA-X           PICTURE X(110).
       FD BONUST
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  BONUST-IO-AREA.
           05  BONUST-IO-AREA-X            PICTURE X(80).
       FD FLOPP
               BLOCK CONTAINS 400
               RECORD CONTAINS 40.
       01  FLOPP-IO-AREA.
           05  FLOPP-IO-AREA-X             PICTURE X(40).
       FD FAKTIN
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  FAKTIN-IO-AREA.
           05  FAKTIN-IO-AREA-X            PICTURE X(200).
       FD FAKPAR
               BLOCK CONTAINS 200
               RECORD CONTAINS 200.
       01  FAKPAR-IO-AREA.
           05  FAKPAR-IO-AREA-X            PICTURE X(200).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD KUNDEMX
               RECORD CONTAINS 200.
       01  KUNDEMX-IO-AREA.
           05  KUNDEMX-IO-AREA-X.
               10  KUNDEMX-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       FD VAREREC
               BLOCK CONTAINS 4100
               RECORD CONTAINS 82.
       01  VAREREC-IO-AREA.
           05  VAREREC-IO-AREA-X           PICTURE X(82).
       FD FAKTUT
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  FAKTUT-IO-AREA.
           05  FAKTUT-IO-AREA-X            PICTURE X(200).
       FD SUMFILE
               BLOCK CONTAINS 9440
               RECORD CONTAINS 20.
       01  SUMFILE-IO-AREA.
           05  SUMFILE-IO-AREA-X           PICTURE X(20).
       FD PRF
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  PRF-IO-PRINT.
           05  PRF-IO-AREA-CONTROL         PICTURE X VALUE ' '.
        02 PRF-IO-AREA.
           05  PRF-IO-AREA-X               PICTURE X(132).
       WORKING-STORAGE SECTION.
       77  TABOSK-MAX   VALUE 100          PICTURE 9(4) USAGE BINARY.
       77  TABOSD-MAX   VALUE 100          PICTURE 9(4) USAGE BINARY.
       77  TABBON-MAX   VALUE 20           PICTURE 9(4) USAGE BINARY.
       77  TABBDA-MAX   VALUE 20           PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  TABOSK-TABLE.
               10  TABOSK-ENTRY
                                           OCCURS 100 TIMES
                                           INDEXED BY TABOSK-I
                                                      TABOSK-S
                                                      TABOSD-I
                                                      TABOSD-S.
                   15  TABOSK              PICTURE X(7).
                   15  TABOSD              PICTURE X(103).
           05  TABBON-TABLE.
               10  TABBON-ENTRY
                                           OCCURS 20 TIMES
                                           INDEXED BY TABBON-I
                                                      TABBON-S
                                                      TABBDA-I
                                                      TABBDA-S.
                   15  TABBON              PICTURE X(4).
                   15  TABBDA              PICTURE X(60).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  OSKFILE-STATUS              PICTURE 99 VALUE 0.
           10  BONUST-STATUS               PICTURE 99 VALUE 0.
           10  FLOPP-STATUS                PICTURE 99 VALUE 0.
           10  FAKTIN-STATUS               PICTURE 99 VALUE 0.
           10  FAKPAR-STATUS               PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  KUNDEMX-STATUS              PICTURE 99 VALUE 0.
           10  VAREREC-STATUS              PICTURE 99 VALUE 0.
           10  FAKTUT-STATUS               PICTURE 99 VALUE 0.
           10  SUMFILE-STATUS              PICTURE 99 VALUE 0.
           10  PRF-STATUS                  PICTURE 99 VALUE 0.
           10  SUMKEY-XX-STATUS            PICTURE 99 VALUE 0.
           10  SUMDAT-XX-STATUS            PICTURE 99 VALUE 0.
           10  BONDAT-XX-STATUS            PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  OSKFILE-EOF-OFF         VALUE '0'.
               88  OSKFILE-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  BONUST-EOF-OFF          VALUE '0'.
               88  BONUST-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FLOPP-EOF-OFF           VALUE '0'.
               88  FLOPP-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FLOPP-READ-OFF          VALUE '0'.
               88  FLOPP-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FLOPP-PROCESS-OFF       VALUE '0'.
               88  FLOPP-PROCESS           VALUE '1'.
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
           05  FILLER                      PIC X VALUE '0'.
               88  FAKPAR-EOF-OFF          VALUE '0'.
               88  FAKPAR-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKPAR-READ-OFF         VALUE '0'.
               88  FAKPAR-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FAKPAR-PROCESS-OFF      VALUE '0'.
               88  FAKPAR-PROCESS          VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  KUNDEMX-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  BONDAT-XX REDEFINES SUMKEY-XX-DATA-FIELDS.
               10  BOC1                    PICTURE X(1).
               10  FILLER                  PICTURE X(102).
           05  FILLER REDEFINES SUMKEY-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(1).
               10  BOPROS-IO.
                   15  BOPROS              PICTURE S9(2)V9(1).
               10  FILLER                  PICTURE X(99).
           05  FILLER REDEFINES SUMKEY-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(4).
               10  BOC2                    PICTURE X(1).
               10  FILLER                  PICTURE X(98).
           05  FILLER REDEFINES SUMKEY-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(5).
               10  BOTEK1                  PICTURE X(20).
               10  FILLER                  PICTURE X(78).
           05  FILLER REDEFINES SUMKEY-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(25).
               10  BOC3                    PICTURE X(1).
               10  FILLER                  PICTURE X(77).
           05  FILLER REDEFINES SUMKEY-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(26).
               10  BOTEK2                  PICTURE X(20).
               10  FILLER                  PICTURE X(57).
           05  FILLER REDEFINES SUMKEY-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(46).
               10  BOC4                    PICTURE X(1).
               10  FILLER                  PICTURE X(56).
           05  FILLER REDEFINES SUMKEY-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(47).
               10  BOVGR                   PICTURE X(5).
               10  FILLER                  PICTURE X(51).
           05  FILLER REDEFINES SUMKEY-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(52).
               10  BOC5                    PICTURE X(1).
               10  FILLER                  PICTURE X(50).
           05  FILLER REDEFINES SUMKEY-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(53).
               10  BOEDBN                  PICTURE X(7).
               10  FILLER                  PICTURE X(43).
           05  FILLER REDEFINES SUMKEY-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(53).
               10  BOEDB3                  PICTURE X(3).
               10  FILLER                  PICTURE X(47).
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
           05  BONDAT-XX REDEFINES SUMDAT-XX-DATA-FIELDS.
               10  BOC1                    PICTURE X(1).
               10  FILLER                  PICTURE X(102).
           05  FILLER REDEFINES SUMDAT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(1).
               10  BOPROS-IO.
                   15  BOPROS              PICTURE S9(2)V9(1).
               10  FILLER                  PICTURE X(99).
           05  FILLER REDEFINES SUMDAT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(4).
               10  BOC2                    PICTURE X(1).
               10  FILLER                  PICTURE X(98).
           05  FILLER REDEFINES SUMDAT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(5).
               10  BOTEK1                  PICTURE X(20).
               10  FILLER                  PICTURE X(78).
           05  FILLER REDEFINES SUMDAT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(25).
               10  BOC3                    PICTURE X(1).
               10  FILLER                  PICTURE X(77).
           05  FILLER REDEFINES SUMDAT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(26).
               10  BOTEK2                  PICTURE X(20).
               10  FILLER                  PICTURE X(57).
           05  FILLER REDEFINES SUMDAT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(46).
               10  BOC4                    PICTURE X(1).
               10  FILLER                  PICTURE X(56).
           05  FILLER REDEFINES SUMDAT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(47).
               10  BOVGR                   PICTURE X(5).
               10  FILLER                  PICTURE X(51).
           05  FILLER REDEFINES SUMDAT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(52).
               10  BOC5                    PICTURE X(1).
               10  FILLER                  PICTURE X(50).
           05  FILLER REDEFINES SUMDAT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(53).
               10  BOEDBN                  PICTURE X(7).
               10  FILLER                  PICTURE X(43).
           05  FILLER REDEFINES SUMDAT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(53).
               10  BOEDB3                  PICTURE X(3).
               10  FILLER                  PICTURE X(47).
      *DSDS: DATA STRUCTURE FIELDS
           05  BONDAT-XX-DATA-FIELDS.
               10  BOC1                    PICTURE X(1).
               10  FILLER                  PICTURE X(59).
           05  FILLER REDEFINES BONDAT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(1).
               10  BOPROS-IO.
                   15  BOPROS              PICTURE S9(2)V9(1).
               10  FILLER                  PICTURE X(56).
           05  FILLER REDEFINES BONDAT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(4).
               10  BOC2                    PICTURE X(1).
               10  FILLER                  PICTURE X(55).
           05  FILLER REDEFINES BONDAT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(5).
               10  BOTEK1                  PICTURE X(20).
               10  FILLER                  PICTURE X(35).
           05  FILLER REDEFINES BONDAT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(25).
               10  BOC3                    PICTURE X(1).
               10  FILLER                  PICTURE X(34).
           05  FILLER REDEFINES BONDAT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(26).
               10  BOTEK2                  PICTURE X(20).
               10  FILLER                  PICTURE X(14).
           05  FILLER REDEFINES BONDAT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(46).
               10  BOC4                    PICTURE X(1).
               10  FILLER                  PICTURE X(13).
           05  FILLER REDEFINES BONDAT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(47).
               10  BOVGR                   PICTURE X(5).
               10  FILLER                  PICTURE X(8).
           05  FILLER REDEFINES BONDAT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(52).
               10  BOC5                    PICTURE X(1).
               10  FILLER                  PICTURE X(7).
           05  FILLER REDEFINES BONDAT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(53).
               10  BOEDBN                  PICTURE X(7).
           05  FILLER REDEFINES BONDAT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(53).
               10  BOEDB3                  PICTURE X(3).
               10  FILLER                  PICTURE X(4).
           05  FLOPP-DATA-FIELDS.
               10  FAKAY1                  PICTURE X(15).
               10  BRKOD1-IO.
                   15  BRKOD1              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
           05  FLOPP-MP                    PICTURE X(18).
           05  FLOPP-MC                    PICTURE X(18).
           05  FLOPP-M-02              REDEFINES FLOPP-MC.
               10  FLOPP-M-02-M2.
                   15  FLOPP-M-02-M2-FAKAY1-G.
                       20  FLOPP-M-02-M2-FAKAY1 PICTURE X(15).
               10  FLOPP-M-02-M1.
                   15  FLOPP-M-02-M1-BRKOD1-G.
                       20  FLOPP-M-02-M1-BRKOD1 PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
           05  FAKTIN-LEVEL-03.
               10  FAKTIN-03-L4.
                   15  FAKTIN-03-L4-FIRMNR PICTURE X(3).
               10  FAKTIN-03-L3.
                   15  FAKTIN-03-L3-FAKEY2 PICTURE X(15).
                   15  FAKTIN-03-L3-BRKOD2 PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  FAKTIN-03-L2.
                   15  FAKTIN-03-L2-RESKNR PICTURE X(6).
               10  FAKTIN-03-L1.
                   15  FAKTIN-03-L1-ORDNR  PICTURE X(6).
           05  FAKTIN-DATA-FIELDS.
               10  FIRMNR                  PICTURE X(3).
               10  FAKEY2                  PICTURE X(15).
               10  BRKOD2-IO.
                   15  BRKOD2              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  FAKREC                  PICTURE X(200).
               10  FAKTYP                  PICTURE X(1).
               10  FAKART                  PICTURE X(1).
               10  BETB                    PICTURE X(2).
               10  ORDNR                   PICTURE X(6).
               10  RECART                  PICTURE X(1).
               10  SEQNR-IO.
                   15  SEQNR               PICTURE S9(4).
               10  HASGEB                  PICTURE X(1).
               10  LAGERK                  PICTURE X(2).
               10  VLKOD1                  PICTURE X(1).
               10  BK                      PICTURE X(1).
               10  HDIST                   PICTURE X(3).
               10  DATO-IO.
                   15  DATO                PICTURE S9(6).
               10  VGR                     PICTURE X(5).
               10  OKODE                   PICTURE X(1).
               10  BESENH-IO.
                   15  BESENH              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  LEVENH-IO.
                   15  LEVENH              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  EDBNR                   PICTURE X(7).
               10  EDB2F                   PICTURE X(2).
               10  EDB3F                   PICTURE X(3).
               10  RAB1-IO.
                   15  RAB1                PICTURE S9(2)V9(1).
               10  RAB2-IO.
                   15  RAB2                PICTURE S9(2)V9(1).
               10  RAB3-IO.
                   15  RAB3                PICTURE S9(2)V9(1).
               10  ENHPR-IO.
                   15  ENHPR               PICTURE S9(7)V9(2).
               10  REGAVD                  PICTURE X(1).
               10  KPAR                    PICTURE X(2).
               10  KPMND                   PICTURE X(2).
               10  KOSTPR-IO.
                   15  KOSTPR              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  BONUSK                  PICTURE X(1).
               10  RECTYP                  PICTURE X(1).
               10  OGEBYR                  PICTURE X(1).
               10  KRETYP                  PICTURE X(1).
               10  RESKNR                  PICTURE X(6).
               10  RESKN1                  PICTURE X(1).
           05  FAKTIN-MP                   PICTURE X(18).
           05  FAKTIN-MC                   PICTURE X(18).
           05  FAKTIN-M-03             REDEFINES FAKTIN-MC.
               10  FAKTIN-M-03-M2.
                   15  FAKTIN-M-03-M2-FAKEY2-G.
                       20  FAKTIN-M-03-M2-FAKEY2 PICTURE X(15).
               10  FAKTIN-M-03-M1.
                   15  FAKTIN-M-03-M1-BRKOD2-G.
                       20  FAKTIN-M-03-M1-BRKOD2 PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
           05  FAKPAR-DATA-FIELDS.
               10  FNRMND                  PICTURE X(1).
               10  PAR                     PICTURE X(2).
               10  PMND                    PICTURE X(2).
           05  FIRMAF-DATA-FIELDS.
               10  KONFNR                  PICTURE X(3).
               10  FGEB                    PICTURE X(1).
               10  ELEK                    PICTURE X(1).
           05  KUNDEMX-DATA-FIELDS.
               10  FPRTYP                  PICTURE X(1).
      *****************************************************************
      *  HOVEDRUTINE.                                                 *
      *****************************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L4            PICTURE X(3).
               10  THE-PRIOR-L3            PICTURE X(18).
               10  THE-PRIOR-L2            PICTURE X(6).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  RFFKOD                  PICTURE X(1).
               10  KXKEY1                  PICTURE X(9).
               10  KXKEY2                  PICTURE X(10).
               10  PRTYPF                  PICTURE X(1).
               10  NULL9-IO.
                   15  NULL9               PICTURE S9(7)V9(2).
               10  ANTSOL-IO.
                   15  ANTSOL              PICTURE S9(6).
               10  KORANT-IO.
                   15  KORANT              PICTURE S9(5)V9(2).
               10  FAKRNR                  PICTURE X(1).
               10  ANTINN-IO.
                   15  ANTINN              PICTURE S9(7).
               10  ANTF-IO.
                   15  ANTF                PICTURE S9(7).
               10  ANTV-IO.
                   15  ANTV                PICTURE S9(7).
               10  NYPRIS-IO.
                   15  NYPRIS              PICTURE S9(7)V9(2).
               10  FORDNR                  PICTURE X(6).
               10  NORDNR-IO.
                   15  NORDNR              PICTURE S9(6).
               10  ORDSE4-IO.
                   15  ORDSE4              PICTURE S9(4).
               10  ORDSEQ-IO.
                   15  ORDSEQ              PICTURE S9(3).
               10  BONKOD                  PICTURE X(1).
               10  ANTGEB-IO.
                   15  ANTGEB              PICTURE S9(7).
               10  ANTHGB-IO.
                   15  ANTHGB              PICTURE S9(7).
               10  GEBANT-IO.
                   15  GEBANT              PICTURE S9(5)V9(2).
               10  NULL-X-IO.
                   15  NULL-X              PICTURE S9(7)V9(2).
               10  ANTRFF-IO.
                   15  ANTRFF              PICTURE S9(7).
               10  RFFANT-IO.
                   15  RFFANT              PICTURE S9(5)V9(2).
               10  ANTBON-IO.
                   15  ANTBON              PICTURE S9(7).
               10  BONANT-IO.
                   15  BONANT              PICTURE S9(5)V9(2).
               10  LINSUM-IO.
                   15  LINSUM              PICTURE S9(7)V9(2).
               10  SUM-X-IO.
                   15  SUM-X               PICTURE S9(10)V9(2).
               10  BONGRL-IO.
                   15  BONGRL              PICTURE S9(7)V9(2).
               10  RFFGRL-IO.
                   15  RFFGRL              PICTURE S9(7)V9(2).
               10  SUMLIN-IO.
                   15  SUMLIN              PICTURE S9(7)V9(2).
               10  FAKTOT-IO.
                   15  FAKTOT              PICTURE S9(7)V9(2).
               10  ORDTOT-IO.
                   15  ORDTOT              PICTURE S9(7)V9(2).
               10  OSKKEY                  PICTURE X(7).
               10  FGEBYR-IO.
                   15  FGEBYR              PICTURE S9(7)V9(2).
               10  TOTGEB-IO.
                   15  TOTGEB              PICTURE S9(7)V9(2).
               10  SEQNR2-IO.
                   15  SEQNR2              PICTURE S9(4).
               10  BONKEY                  PICTURE X(4).
               10  FBONUS-IO.
                   15  FBONUS              PICTURE S9(7)V9(2).
               10  TOTBON-IO.
                   15  TOTBON              PICTURE S9(7)V9(2).
               10  SEQNR3-IO.
                   15  SEQNR3              PICTURE S9(4).
               10  RFFKEY                  PICTURE X(4).
               10  RFFSUM-IO.
                   15  RFFSUM              PICTURE S9(7)V9(2).
               10  TOTRFF-IO.
                   15  TOTRFF              PICTURE S9(7)V9(2).
           05  EDITTING-FIELDS.
               10  XO-72P-EF.
                 15  XO-72P                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-52P-EF.
                 15  XO-52P                PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-60P-EF.
                 15  XO-60P                PICTURE S9(6) USAGE
                                                       PACKED-DECIMAL.
               10  XO-30P-EF.
                 15  XO-30P                PICTURE S9(3) USAGE
                                                       PACKED-DECIMAL.
               10  XO-70YY9                PICTURE Z.ZZZ.ZZ9.
               10  XO-72YY9R               PICTURE Z.ZZZ.ZZZ,99-.
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
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-01                    TO TRUE
           SET NOT-I-09                    TO TRUE
           SET NOT-I-05                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  FLOPP-PROCESS
               SET FLOPP-PROCESS-OFF       TO TRUE
               SET FLOPP-READ              TO TRUE
           END-IF
 
           IF  FLOPP-READ
               PERFORM FLOPP-GET
               SET FLOPP-READ-OFF          TO TRUE
               IF  NOT FLOPP-EOF
                   PERFORM FLOPP-MATCH-SET
               END-IF
           END-IF
 
           IF  FAKTIN-PROCESS
               SET FAKTIN-PROCESS-OFF      TO TRUE
               SET FAKTIN-READ             TO TRUE
           END-IF
 
           IF  FAKTIN-READ
               PERFORM FAKTIN-GET
               SET FAKTIN-READ-OFF         TO TRUE
               IF  NOT FAKTIN-EOF
                   PERFORM FAKTIN-MATCH-SET
               END-IF
           END-IF
 
           IF  FAKPAR-PROCESS
               SET FAKPAR-PROCESS-OFF      TO TRUE
               SET FAKPAR-READ             TO TRUE
           END-IF
 
           IF  FAKPAR-READ
           AND RECORD-SELECTED-OFF
               PERFORM FAKPAR-GET
               SET FAKPAR-READ-OFF         TO TRUE
               IF  NOT FAKPAR-EOF
                   SET FAKPAR-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
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
 
           IF  FLOPP-PROCESS
               PERFORM FLOPP-IDSET
           END-IF
 
           IF  FAKTIN-PROCESS
               PERFORM FAKTIN-IDSET
           END-IF
 
           IF  FAKPAR-PROCESS
               PERFORM FAKPAR-IDSET
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
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  FLOPP-PROCESS
               PERFORM FLOPP-FLDSET
           END-IF
 
           IF  FAKTIN-PROCESS
               PERFORM FAKTIN-FLDSET
           END-IF
 
           IF  FAKPAR-PROCESS
               PERFORM FAKPAR-FLDSET
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
           SET NOT-I-10                    TO TRUE
           SET NOT-I-12                    TO TRUE
           SET NOT-I-24                    TO TRUE
           SET NOT-I-25                    TO TRUE
           SET NOT-I-49                    TO TRUE
           SET NOT-I-14                    TO TRUE
           SET NOT-I-20                    TO TRUE
           SET NOT-I-33                    TO TRUE
           SET NOT-I-31                    TO TRUE
           SET NOT-I-32                    TO TRUE
           IF  (I-L1)
               SET NOT-I-47                TO TRUE
               SUBTRACT RFFGRL             FROM RFFGRL
               MOVE ' '                    TO RFFKOD
           END-IF
           IF  (I-L3)
               SET NOT-I-65                TO TRUE
               SET NOT-I-70                TO TRUE
               SET NOT-I-40                TO TRUE
               SET NOT-I-41                TO TRUE
               SET NOT-I-45                TO TRUE
               SET NOT-I-21                TO TRUE
               SUBTRACT FAKTOT             FROM FAKTOT
           END-IF
           IF  (I-L2)
               SUBTRACT SUMLIN             FROM SUMLIN
           END-IF
           IF  (I-L3)
               SUBTRACT BONGRL             FROM BONGRL
               MOVE ' '                    TO BONKOD
           END-IF
           IF  (I-L4)
               SET NOT-I-17                TO TRUE
               SET NOT-I-19                TO TRUE
               MOVE FIRMNR                 TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-16                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-16            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
           END-IF
           IF  (I-L4 AND NOT-I-16)
               SET NOT-I-17                TO TRUE
               IF  FGEB = 'F'
                   SET I-17                TO TRUE
               END-IF
               SET NOT-I-78                TO TRUE
               IF  ELEK = 'N'
                   SET I-78                TO TRUE
               END-IF
               SET NOT-I-19                TO TRUE
               IF  KONFNR > '000'
                   SET I-19                TO TRUE
               END-IF
      *****************************************************************
      * SPESIALRUTINE FOR FAKT. 31.12.2000. SKAL FJERNES ETTER DETTE. *
      *****************************************************************
      *  L4                SETOF                     99     UNTAK 311200
      *  L4NU1   FIRMNR    COMP "694"                    99 UNTAK 311200
      *  L4NU1N99FIRMNR    COMP "695"                    99 UNTAK 311200
      *  L4NU1N99FIRMNR    COMP "696"                    99 UNTAK 311200
      *  L4NU1N99FIRMNR    COMP "697"                    99 UNTAK 311200
      *  L4NU1N99FIRMNR    COMP "698"                    99 UNTAK 311200
      *  L4NU1N99FIRMNR    COMP "699"                    99 UNTAK 311200
      *  L4NU1 99          SETOF                     19     UNTAK 311200
      *****************************************************************
      *****************************************************************
      *
           END-IF
           IF  (I-L3)
               SET NOT-I-76                TO TRUE
           END-IF
           IF  (I-L3 AND NOT-I-78)
               OR  (I-L3 AND NOT-I-17)
               GO TO ENDMX-T
           END-IF
           IF  (I-L3)
               MOVE ' '                    TO PRTYPF
               MOVE FIRMNR                 TO KXKEY1 (1:3)
           END-IF
           IF  (I-L3 AND I-19)
               MOVE KONFNR                 TO KXKEY1 (1:3)
           END-IF
           IF  (I-L3)
               MOVE RESKNR                 TO KXKEY1 (4:6)
               MOVE KXKEY1                 TO KXKEY2 (1:9)
               MOVE '1'                    TO KXKEY2 (10:1)
               MOVE KXKEY2                 TO KUNDEMX-KEY1
               READ KUNDEMX RECORD KEY IS KUNDEMX-KEY1
               INVALID KEY
                   SET I-77                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-77            TO TRUE
                   PERFORM KUNDEMX-FLDSET
                   PERFORM KUNDEMX-IDSET
               END-READ
           END-IF
           IF  (I-L3 AND NOT-I-77)
               MOVE FPRTYP                 TO PRTYPF
           END-IF
           IF  (I-L3 AND I-77)
               MOVE ' '                    TO PRTYPF
           END-IF
           IF  (I-L3)
               SET NOT-I-76                TO TRUE
               IF  PRTYPF = 'M'
                   SET I-76                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-76)
               SET NOT-I-76                TO TRUE
               IF  PRTYPF = 'X'
                   SET I-76                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-76)
               SET NOT-I-76                TO TRUE
               IF  PRTYPF = 'U'
                   SET I-76                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-76)
               SET NOT-I-76                TO TRUE
               IF  PRTYPF = 'B'
                   SET I-76                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-76)
               SET NOT-I-76                TO TRUE
               IF  PRTYPF = 'E'
                   SET I-76                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-76)
               SET NOT-I-76                TO TRUE
               IF  PRTYPF = 'A'
                   SET I-76                TO TRUE
               END-IF
           END-IF.
 
       ENDMX-T.
      *****************************************************************
           IF  (I-03)
               SET NOT-I-12                TO TRUE
               IF  EDBNR NOT = '       '
                   SET I-12                TO TRUE
               END-IF
      *****************************************************************
      * RUTINE FOR Å NULLSTILLE BELØP PÅ SERVICE-ORDRE.               *
      * DETTE GJELDER SERVICE-TYPE G(GARANTI), K(KULANSE) OG          *
      *       A(SERVICEAVTALE)                                        *
      *****************************************************************
           END-IF
           IF  (I-03 AND I-12)
               SET NOT-I-24                TO TRUE
               IF  BK = 'S'
                   SET I-24                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-24)
               SET NOT-I-25                TO TRUE
               IF  OKODE = 'G'
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-24 AND NOT-I-25)
               SET NOT-I-25                TO TRUE
               IF  OKODE = 'K'
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-24 AND NOT-I-25)
               SET NOT-I-25                TO TRUE
               IF  OKODE = 'A'
                   SET I-25                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-24 AND I-25)
               MOVE 0,00                   TO NULL9
               ADD 1                       TO ANTSOL
      *****************************************************************
      * RUTINE FOR Å SETTE INN ANTALL PÅ RENTE/GEBYR NOTA            *
      *****************************************************************
           END-IF
           IF  (I-03 AND I-12)
               SET NOT-I-31                TO TRUE
               IF  RECTYP = 'R'
                   SET I-31                TO TRUE
               END-IF
               SET NOT-I-32                TO TRUE
               IF  BESENH = 0,00
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-12 AND I-31)
               AND (I-32)
               MOVE 1,00                   TO KORANT
           END-IF
           IF  (I-L1)
               SUBTRACT ORDTOT             FROM ORDTOT
               SET NOT-I-62                TO TRUE
               SET NOT-I-66                TO TRUE
               SET NOT-I-75                TO TRUE
           END-IF
           IF  (I-03)
               SET NOT-I-61                TO TRUE
               IF  HASGEB = 'H'
                   SET I-61                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-61)
               SET I-62                    TO TRUE
      *****************************************************************
      * RETTE AVDELING OM DEN ER LAVERE ENN 1.                        *
      *****************************************************************
           END-IF
           IF  (I-03)
               SET NOT-I-81                TO TRUE
               IF  REGAVD < '1'
                   SET I-81                TO TRUE
               END-IF
               SET NOT-I-82                TO TRUE
               IF  REGAVD = 'A'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  REGAVD = 'B'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  REGAVD = 'C'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-81 AND NOT-I-82)
               MOVE '1'                    TO REGAVD
      *****************************************************************
      * RUTINE FOR Å BESTEMME OM DET SKAL VÆRE FAKTURA FLERSIDE       *
      *        ELLER OM DET SKAL VÆRE FAKTURA DATAPOST.               *
      *****************************************************************
           END-IF
           IF  (I-03 AND I-MR)
               MOVE 'F'                    TO FAKART
           END-IF
           IF  (I-03)
               SET NOT-I-22                TO TRUE
               IF  RECART = 'A'
                   SET I-22                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-MR)
               GO TO AA-T
           END-IF
           IF  (I-03)
               SET NOT-I-20                TO TRUE
               IF  FAKART = 'F'
                   SET I-20                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-20)
               GO TO AA-T
      *  03                MOVE "D"       FAKART            DATAPOST IKKE I BRUK
           END-IF
           IF  (I-03)
               MOVE 'F'                    TO FAKART
      *****************************************************************
      * RUTINE FOR MERKING AV KREDITNOTA                              *
      * ALL KONTANTSALG ER MERKET SOM FAKTURA.                        *
      *****************************************************************
           END-IF
           .
 
       AA-T.
           IF  (I-03)
               SET NOT-I-10                TO TRUE
               IF  FAKTYP = 'K'
                   SET I-10                TO TRUE
               END-IF
               SET NOT-I-11                TO TRUE
               IF  ORDNR > '899999'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-U1 AND I-11)
               SET I-10                    TO TRUE
           END-IF
           IF  (I-03 AND I-10)
               MOVE '2'                    TO FAKRNR
           END-IF
           IF  (I-03 AND NOT-I-10)
               MOVE '1'                    TO FAKRNR
      *****************************************************************
      * TELLE RUTINE.                                                 *
      *****************************************************************
           END-IF
           IF  (I-03)
               ADD 1                       TO ANTINN
               ADD 1                       TO ANTF
           END-IF
           IF  (I-12)
               ADD 1                       TO ANTV
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *  HVIS EDBNR STARTER MED "94" ELLER "995" SNUS BELØP I VAREREC *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
           END-IF
           IF  (I-12)
               MOVE ENHPR                  TO NYPRIS-IO
               SET NOT-I-33                TO TRUE
               IF  EDB3F = '995'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-12 AND NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  EDB2F = '94'
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (I-12 AND I-33)
               MULTIPLY -1 BY ENHPR    GIVING NYPRIS
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *  FAKTURASEQENSE PR ORDRENR LEGGES UT I VAREREC                    *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
           END-IF
           IF  (I-12)
               SET NOT-I-14                TO TRUE
               IF  ORDNR NOT = FORDNR
                   SET I-14                TO TRUE
               END-IF
           END-IF
           IF  (I-12 AND I-14)
               MOVE ORDNR                  TO FORDNR
               MOVE ORDNR                  TO NORDNR-IO
               MOVE 1                      TO ORDSE4
           END-IF
           IF  (I-12 AND NOT-I-14)
               ADD 1                       TO ORDSE4
           END-IF
           IF  (I-12)
               SET NOT-I-13                TO TRUE
               IF  ORDSE4 > 999
                   SET I-13                TO TRUE
               END-IF
           END-IF
           IF  (I-12 AND I-13)
               SUBTRACT 1000               FROM ORDSE4
           END-IF
           IF  (I-12)
               ADD ORDSE4 TO ZERO      GIVING ORDSEQ
      *****************************************************************
      * RUTINE FOR FAKTURABONUS/FAKTURAGEBYR OG RADIO FRAKT/FORSIKR.  *
      * RADIO FRAKT/FORSIKRING UTÅR FRA 1.6-2001                      *
      *****************************************************************
           END-IF
           IF  (NOT-I-03)
               GO TO SLUTT-T
           END-IF
           IF  (NOT-I-12)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-15                    TO TRUE
           IF  RESKN1 = '5'
               SET I-15                    TO TRUE
           END-IF
           IF  (NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  RESKN1 = '2'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  BETB = '07'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  BETB = '47'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  BETB = '14'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           SET NOT-I-28                    TO TRUE
           IF  FIRMNR = '918'
               SET I-28                    TO TRUE
           END-IF
           SET NOT-I-95                    TO TRUE
           IF  FIRMNR = '942'
               SET I-95                    TO TRUE
           END-IF
      *          VGR       COMP "40000"                29    IKKE RADIO
      * N29      VGR       COMP "49900"              29  29  IKKE RADIO
           SET NOT-I-40                    TO TRUE
           IF  BONUSK > ' '
               SET I-40                    TO TRUE
           END-IF
           IF  (I-40)
               MOVE BONUSK                 TO BONKOD
      *  28N29N15          MOVE "R"       RFFKOD  1         SOGB RADIO
      *  28N29N15          SETON                     49     RADIO-GEBYR
           END-IF
           PERFORM SUMRUT-S
           IF  (NOT-I-17 AND NOT-I-62)
               GO TO SLUTT-T
           END-IF
           IF  (I-10 AND NOT-I-95)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-15                    TO TRUE
           IF  RESKN1 = '5'
               SET I-15                    TO TRUE
           END-IF
           IF  (NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  RESKN1 = '2'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-15)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-18                    TO TRUE
           IF  OGEBYR = 'N'
               SET I-18                    TO TRUE
           END-IF
           IF  (NOT-I-18)
               SET NOT-I-18                TO TRUE
               IF  OGEBYR = '1'
                   SET I-18                TO TRUE
               END-IF
           END-IF
           IF  (I-18 AND NOT-I-62)
               SET I-21                    TO TRUE
               GO TO SLUTT-T
           END-IF
           IF  (I-76 AND NOT-I-62)
               GO TO SLUTT-T
           END-IF
           SET NOT-I-76                    TO TRUE
           PERFORM FAKSUM-S.
 
       SLUTT-T.
      *****************************************************************
      *  TOTALRUTINE FOR HASTER GEBYR.                                *
      *****************************************************************
           CONTINUE.
 
       SUMRUT-S SECTION.
       SUMRUT-S-P.
           SET NOT-I-59                    TO TRUE
           IF  FIRMNR = '950'
               SET I-59                    TO TRUE
           END-IF
           IF  (I-59)
               SET NOT-I-58                TO TRUE
               IF  VGR = '19995'
                   SET I-58                TO TRUE
               END-IF
           END-IF
           IF  (I-59 AND I-58)
               GO TO ENDSUM-T
           END-IF
           SET NOT-I-59                    TO TRUE
           IF  FIRMNR = '923'
               SET I-59                    TO TRUE
           END-IF
           IF  (I-59)
               SET NOT-I-58                TO TRUE
               IF  VGR = '42251'
                   SET I-58                TO TRUE
               END-IF
           END-IF
           IF  (I-59 AND I-58)
               GO TO ENDSUM-T
           END-IF
           IF  (I-24 AND I-25)
               GO TO ENDSUM-T
           END-IF
           MULTIPLY LEVENH BY NYPRIS   GIVING LINSUM ROUNDED
           MULTIPLY RAB1 BY LINSUM     GIVING SUM-X
           DIVIDE SUM-X BY 100         GIVING SUM-X ROUNDED
           SUBTRACT SUM-X                  FROM LINSUM
           MULTIPLY RAB2 BY LINSUM     GIVING SUM-X
           DIVIDE SUM-X BY 100         GIVING SUM-X ROUNDED
           SUBTRACT SUM-X                  FROM LINSUM
           MULTIPLY RAB3 BY LINSUM     GIVING SUM-X
           DIVIDE SUM-X BY 100         GIVING SUM-X ROUNDED
           SUBTRACT SUM-X                  FROM LINSUM
           IF  (I-40)
               ADD LINSUM                  TO BONGRL
           END-IF
           IF  (I-49)
               ADD LINSUM                  TO RFFGRL
           END-IF
           IF  (NOT-I-10)
               ADD LINSUM                  TO SUMLIN
           END-IF
           IF  (I-10)
               SUBTRACT LINSUM             FROM SUMLIN
           END-IF.
 
       ENDSUM-T.
           CONTINUE.
      ******************************************************
      * SUBRUTINE FOR UTREGNING AV FAKTURATOTAL FOR        *
      * BEREGNING AV FAKTURAGEBYR OG HASTEORDREGEBYR.      *
      ******************************************************
 
       FAKSUM-S SECTION.
       FAKSUM-S-P.
           MULTIPLY LEVENH BY NYPRIS   GIVING LINSUM
           MULTIPLY RAB1 BY LINSUM     GIVING SUM-X
           DIVIDE SUM-X BY 100         GIVING SUM-X
           SUBTRACT SUM-X                  FROM LINSUM
           MULTIPLY RAB2 BY LINSUM     GIVING SUM-X
           DIVIDE SUM-X BY 100         GIVING SUM-X
           SUBTRACT SUM-X                  FROM LINSUM
           MULTIPLY RAB3 BY LINSUM     GIVING SUM-X
           DIVIDE SUM-X BY 100         GIVING SUM-X
           SUBTRACT SUM-X                  FROM LINSUM
           IF  (NOT-I-18 AND NOT-I-62)
               ADD LINSUM                  TO FAKTOT
           END-IF
           IF  (I-62)
               ADD LINSUM                  TO ORDTOT
           END-IF
           IF  (NOT-I-18 AND NOT-I-62)
               SET I-65                    TO TRUE
           END-IF
           IF  (I-62)
               SET I-66                    TO TRUE
           END-IF.
      ******************************************************
      * SUBRUTINE FOR TILDELING AV FAKTURAGEBYR.           *
      *                         OG HASTERGEBYR.            *
      ******************************************************
 
       TILRUT-S SECTION.
       TILRUT-S-P.
           MOVE 0                          TO KSEQ
           MOVE FIRMNR                     TO KFNR
           MOVE ' '                        TO KAVD
           IF  (I-65)
               MOVE 'F'                    TO KKODE
           END-IF
           IF  (I-66)
               MOVE 'H'                    TO KKODE
           END-IF.
 
       LESTAB-T.
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
           MOVE TABOSD(TABOSD-I) (104:0)   TO SUMDAT
           IF  (I-65)
               SET NOT-I-72                TO TRUE
               IF  FAKTOT NOT < FRABEL
                   SET I-72                TO TRUE
               END-IF
               SET NOT-I-73                TO TRUE
               IF  FAKTOT NOT > TILBEL
                   SET I-73                TO TRUE
               END-IF
           END-IF
           IF  (I-66)
               SET NOT-I-72                TO TRUE
               IF  ORDTOT NOT < FRABEL
                   SET I-72                TO TRUE
               END-IF
               SET NOT-I-73                TO TRUE
               IF  ORDTOT NOT > TILBEL
                   SET I-73                TO TRUE
               END-IF
           END-IF
           IF  (I-72 AND I-73)
               GO TO FAKGEB-T
           END-IF
           GO TO LESTAB-T.
 
       FAKGEB-T.
           SET NOT-I-74                    TO TRUE
           IF  GEBYR = 0,00
               SET I-74                    TO TRUE
           END-IF
           IF  (I-74)
               GO TO ENDTIL-T
           END-IF
           MOVE GEBYR                      TO FGEBYR-IO
           ADD FGEBYR                      TO SUMLIN
           ADD FGEBYR                      TO TOTGEB
           IF  (I-65)
               SET I-70                    TO TRUE
           END-IF
           IF  (I-66)
               SET I-75                    TO TRUE
           END-IF
           IF  (I-75)
               ADD 1 TO SEQNR          GIVING SEQNR2
           END-IF
           IF  (I-70 AND NOT-I-75)
               ADD 1 TO SEQNR          GIVING SEQNR2
           END-IF.
 
       ENDTIL-T.
           CONTINUE.
      *************************************************************
      * SUBRUTINE FOR TILDELING AV FAKTURA BONUS/FAKTURAPÅSLAG I %*
      *************************************************************
 
       BONRUT-S SECTION.
       BONRUT-S-P.
           MOVE FIRMNR                     TO BONKEY (1:3)
           MOVE BONKOD                     TO BONKEY (4:1)
           SET NOT-I-42                    TO TRUE
           SET TABBON-S                    TO TABBON-I
           PERFORM WITH TEST AFTER
                   VARYING TABBON-I FROM 1 BY 1
                     UNTIL TABBON-I >= TABBON-MAX
                        OR I-42
               IF  BONKEY = TABBON (TABBON-I)
                   SET I-42                TO TRUE
                   SET TABBON-S            TO TABBON-I
               END-IF
           END-PERFORM
           SET TABBON-I                    TO TABBON-S
           IF  I-42
           AND TABBON-I NOT > TABBDA-MAX
               SET TABBDA-I                TO TABBON-I
           END-IF
           IF  (NOT-I-42)
               GO TO ENDBON-T
           END-IF
           MOVE TABBDA(TABBDA-I) (61:0)    TO BONDAT
           SET NOT-I-44                    TO TRUE
           IF  BOEDB3 = '995'
               SET I-44                    TO TRUE
           END-IF
           MULTIPLY BOPROS BY BONGRL   GIVING SUM-X
           DIVIDE SUM-X BY 100         GIVING FBONUS
           IF  (I-10 AND I-44)
               ADD FBONUS                  TO SUMLIN
           END-IF
           IF  (I-10 AND NOT-I-44)
               SUBTRACT FBONUS             FROM SUMLIN
           END-IF
           IF  (NOT-I-10 AND I-44)
               SUBTRACT FBONUS             FROM SUMLIN
           END-IF
           IF  (NOT-I-10 AND NOT-I-44)
               ADD FBONUS                  TO SUMLIN
           END-IF
           IF  (I-10 AND I-44)
               ADD FBONUS                  TO TOTBON
           END-IF
           IF  (I-10 AND NOT-I-44)
               SUBTRACT FBONUS             FROM TOTBON
           END-IF
           IF  (NOT-I-10 AND I-44)
               SUBTRACT FBONUS             FROM TOTBON
           END-IF
           IF  (NOT-I-10 AND NOT-I-44)
               ADD FBONUS                  TO TOTBON
           END-IF
           SET I-45                        TO TRUE
           IF  (I-45 AND NOT-I-70 AND NOT-I-75)
               ADD 1 TO SEQNR          GIVING SEQNR3
           END-IF
           IF  (I-45 AND I-70 AND NOT-I-75)
               ADD 1 TO SEQNR2         GIVING SEQNR3
           END-IF
           IF  (I-45 AND NOT-I-70 AND I-75)
               ADD 1 TO SEQNR2         GIVING SEQNR3
           END-IF.
 
       ENDBON-T.
           CONTINUE.
      *************************************************************
      * SUBRUTINE FOR TILDELING AV RADIO FRAKT OG FORSIKRING I %  *
      *************************************************************
 
       RFFRUT-S SECTION.
       RFFRUT-S-P.
           MOVE FIRMNR                     TO RFFKEY (1:3)
           MOVE RFFKOD                     TO RFFKEY (4:1)
           SET NOT-I-42                    TO TRUE
           SET TABBON-S                    TO TABBON-I
           PERFORM WITH TEST AFTER
                   VARYING TABBON-I FROM 1 BY 1
                     UNTIL TABBON-I >= TABBON-MAX
                        OR I-42
               IF  RFFKEY = TABBON (TABBON-I)
                   SET I-42                TO TRUE
                   SET TABBON-S            TO TABBON-I
               END-IF
           END-PERFORM
           SET TABBON-I                    TO TABBON-S
           IF  I-42
           AND TABBON-I NOT > TABBDA-MAX
               SET TABBDA-I                TO TABBON-I
           END-IF
           IF  (NOT-I-42)
               GO TO ENDRFF-T
           END-IF
           MOVE TABBDA(TABBDA-I) (61:0)    TO BONDAT
           SET NOT-I-44                    TO TRUE
           IF  BOEDB3 = '995'
               SET I-44                    TO TRUE
           END-IF
           MULTIPLY BOPROS BY RFFGRL   GIVING SUM-X
           DIVIDE SUM-X BY 100         GIVING RFFSUM
           IF  (I-10 AND I-44)
               ADD RFFSUM                  TO SUMLIN
           END-IF
           IF  (I-10 AND NOT-I-44)
               SUBTRACT RFFSUM             FROM SUMLIN
           END-IF
           IF  (NOT-I-10 AND I-44)
               SUBTRACT RFFSUM             FROM SUMLIN
           END-IF
           IF  (NOT-I-10 AND NOT-I-44)
               ADD RFFSUM                  TO SUMLIN
           END-IF
           IF  (I-10 AND I-44)
               ADD RFFSUM                  TO TOTRFF
           END-IF
           IF  (I-10 AND NOT-I-44)
               SUBTRACT RFFSUM             FROM TOTRFF
           END-IF
           IF  (NOT-I-10 AND I-44)
               SUBTRACT RFFSUM             FROM TOTRFF
           END-IF
           IF  (NOT-I-10 AND NOT-I-44)
               ADD RFFSUM                  TO TOTRFF
           END-IF
           SET I-47                        TO TRUE
           IF  (I-45 AND NOT-I-70 AND NOT-I-75)
               ADD 1 TO SEQNR          GIVING SEQNR3
           END-IF
           IF  (I-45 AND I-70 AND NOT-I-75)
               ADD 1 TO SEQNR2         GIVING SEQNR3
           END-IF
           IF  (I-45 AND NOT-I-70 AND I-75)
               ADD 1 TO SEQNR2         GIVING SEQNR3
           END-IF.
 
       ENDRFF-T.
           CONTINUE.
      *******************************************************
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1 AND I-66)
               PERFORM TILRUT-S
           END-IF
           IF  (I-L1 AND I-75)
               ADD 1                       TO ANTGEB
               ADD 1                       TO ANTHGB
               MOVE 1,00                   TO GEBANT
               MOVE 0                      TO NULL-X
               ADD 1                       TO ANTF
               ADD 1                       TO ANTV
      *****************************************************************
      *  TOTALRUTINE FOR RADIO  FRAKT OG FORSIKRING.                  *
      *****************************************************************
           END-IF
           IF  (I-L1)
               SET NOT-I-48                TO TRUE
               IF  RFFGRL NOT = 0
                   SET I-48                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-48)
               PERFORM RFFRUT-S
           END-IF
           IF  (I-L1 AND I-47)
               ADD 1                       TO ANTRFF
               MOVE 1,00                   TO RFFANT
               MOVE 0                      TO NULL-X
               ADD 1                       TO ANTF
               ADD 1                       TO ANTV
      *****************************************************************
      *  TOTALRUTINE FOR FAKTURA GEBYR (IKKE HASTERGEBYR SAMTIDIG)    *
      *****************************************************************
           END-IF
           IF  (I-L3 AND I-65 AND NOT-I-66 AND NOT-I-21)
               PERFORM TILRUT-S
           END-IF
           IF  (I-L3 AND I-70)
               ADD 1                       TO ANTGEB
               MOVE 1,00                   TO GEBANT
               MOVE 0                      TO NULL-X
               ADD 1                       TO ANTF
               ADD 1                       TO ANTV
      *****************************************************************
      *  TOTALRUTINE FOR FAKTURA BONUS                                *
      *****************************************************************
           END-IF
           IF  (I-L3)
               SET NOT-I-41                TO TRUE
               IF  BONGRL NOT = 0
                   SET I-41                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND I-41)
               PERFORM BONRUT-S
           END-IF
           IF  (I-L3 AND I-45)
               ADD 1                       TO ANTBON
               MOVE 1,00                   TO BONANT
               MOVE 0                      TO NULL-X
               ADD 1                       TO ANTF
               ADD 1                       TO ANTV
      ******************************************************
      * SUBRUTINE FOR UTREGNING AV FAKTURATOTAL FOR        *
      * TOTALKJØP HITTIL I ÅR PR. KUNDE.                   *
      * SERVICE-ORDRER MED 0 I PRIS SUMMERES IKKE.         *
      ******************************************************
           END-IF
           .
 
       FLOPP-GET SECTION.
       FLOPP-GET-P.
           IF  FLOPP-EOF-OFF
               READ FLOPP
               AT END
                   SET FLOPP-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FLOPP-FLDSET SECTION.
       FLOPP-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FLOPP-IO-AREA (1:15)   TO FAKAY1 (1:15)
               MOVE FLOPP-IO-AREA (16:3)   TO BRKOD1-IO
           END-EVALUATE.
 
       FLOPP-IDSET SECTION.
       FLOPP-IDSET-P.
           SET I-02                        TO TRUE.
 
       FLOPP-MATCH-SET SECTION.
       FLOPP-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE FLOPP-IO-AREA (1:15)   TO FLOPP-M-02-M2-FAKAY1
               MOVE FLOPP-IO-AREA (16:3)   TO FLOPP-M-02-M1-BRKOD1-G
           END-EVALUATE.
 
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
               MOVE FAKTIN-IO-AREA (1:15)  TO FAKEY2 (1:15)
               MOVE FAKTIN-IO-AREA (16:3)  TO BRKOD2-IO
               MOVE FAKTIN-IO-AREA (1:200) TO FAKREC (1:200)
               MOVE FAKTIN-IO-AREA (10:1)  TO FAKTYP (1:1)
               MOVE FAKTIN-IO-AREA (11:1)  TO FAKART (1:1)
               MOVE FAKTIN-IO-AREA (14:2)  TO BETB (1:2)
               MOVE FAKTIN-IO-AREA (19:6)  TO ORDNR (1:6)
               MOVE FAKTIN-IO-AREA (25:1)  TO RECART (1:1)
               MOVE FAKTIN-IO-AREA (26:4)  TO SEQNR-IO
               INSPECT SEQNR-IO REPLACING ALL ' ' BY '0'
               MOVE FAKTIN-IO-AREA (36:1)  TO HASGEB (1:1)
               MOVE FAKTIN-IO-AREA (62:2)  TO LAGERK (1:2)
               MOVE FAKTIN-IO-AREA (65:1)  TO VLKOD1 (1:1)
               MOVE FAKTIN-IO-AREA (66:1)  TO BK (1:1)
               MOVE FAKTIN-IO-AREA (68:3)  TO HDIST (1:3)
               MOVE FAKTIN-IO-AREA (71:6)  TO DATO-IO
               INSPECT DATO-IO REPLACING ALL ' ' BY '0'
               MOVE FAKTIN-IO-AREA (77:5)  TO VGR (1:5)
               MOVE FAKTIN-IO-AREA (132:1) TO OKODE (1:1)
               MOVE FAKTIN-IO-AREA (133:4) TO BESENH-IO
               MOVE FAKTIN-IO-AREA (137:4) TO LEVENH-IO
               MOVE FAKTIN-IO-AREA (141:7) TO EDBNR (1:7)
               MOVE FAKTIN-IO-AREA (141:2) TO EDB2F (1:2)
               MOVE FAKTIN-IO-AREA (141:3) TO EDB3F (1:3)
               MOVE FAKTIN-IO-AREA (148:3) TO RAB1-IO
               INSPECT RAB1-IO REPLACING ALL ' ' BY '0'
               MOVE FAKTIN-IO-AREA (151:3) TO RAB2-IO
               INSPECT RAB2-IO REPLACING ALL ' ' BY '0'
               MOVE FAKTIN-IO-AREA (154:3) TO RAB3-IO
               INSPECT RAB3-IO REPLACING ALL ' ' BY '0'
               MOVE FAKTIN-IO-AREA (157:9) TO ENHPR-IO
               INSPECT ENHPR-IO REPLACING ALL ' ' BY '0'
               MOVE FAKTIN-IO-AREA (166:1) TO REGAVD (1:1)
               MOVE FAKTIN-IO-AREA (167:2) TO KPAR (1:2)
               MOVE FAKTIN-IO-AREA (169:2) TO KPMND (1:2)
               MOVE FAKTIN-IO-AREA (171:5) TO KOSTPR-IO
               MOVE FAKTIN-IO-AREA (176:1) TO BONUSK (1:1)
               MOVE FAKTIN-IO-AREA (177:1) TO RECTYP (1:1)
               MOVE FAKTIN-IO-AREA (178:1) TO OGEBYR (1:1)
               MOVE FAKTIN-IO-AREA (179:1) TO KRETYP (1:1)
               MOVE FAKTIN-IO-AREA (184:6) TO RESKNR (1:6)
               MOVE FAKTIN-IO-AREA (184:1) TO RESKN1 (1:1)
           END-EVALUATE.
 
       FAKTIN-IDSET SECTION.
       FAKTIN-IDSET-P.
           SET I-03                        TO TRUE.
 
       FAKTIN-CHK-LEVEL SECTION.
       FAKTIN-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO FAKTIN-LEVEL-03
               MOVE FAKTIN-IO-AREA (1:3)   TO FAKTIN-03-L4-FIRMNR
               MOVE FAKTIN-IO-AREA (1:15)  TO FAKTIN-03-L3-FAKEY2
               MOVE FAKTIN-IO-AREA (16:3)  TO FAKTIN-03-L3-BRKOD2
               MOVE FAKTIN-IO-AREA (184:6) TO FAKTIN-03-L2-RESKNR
               MOVE FAKTIN-IO-AREA (19:6)  TO FAKTIN-03-L1-ORDNR
               IF  FAKTIN-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  FAKTIN-03-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   WHEN  FAKTIN-03-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  FAKTIN-03-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  FAKTIN-03-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  FAKTIN-03-L4          TO THE-PRIOR-L4
               MOVE  FAKTIN-03-L3          TO THE-PRIOR-L3
               MOVE  FAKTIN-03-L2          TO THE-PRIOR-L2
               MOVE  FAKTIN-03-L1          TO THE-PRIOR-L1
               SET FAKTIN-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       FAKTIN-MATCH-SET SECTION.
       FAKTIN-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKTIN-IO-AREA (1:15)  TO FAKTIN-M-03-M2-FAKEY2
               MOVE FAKTIN-IO-AREA (16:3)  TO FAKTIN-M-03-M1-BRKOD2-G
           END-EVALUATE.
 
       FAKPAR-GET SECTION.
       FAKPAR-GET-P.
           IF  FAKPAR-EOF-OFF
               READ FAKPAR
               AT END
                   SET FAKPAR-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FAKPAR-FLDSET SECTION.
       FAKPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKPAR-IO-AREA (3:1)   TO FNRMND (1:1)
               MOVE FAKPAR-IO-AREA (6:2)   TO PAR (1:2)
               MOVE FAKPAR-IO-AREA (8:2)   TO PMND (1:2)
           END-EVALUATE.
 
       FAKPAR-IDSET SECTION.
       FAKPAR-IDSET-P.
           SET I-01                        TO TRUE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (1:3)   TO KONFNR (1:3)
               MOVE FIRMAF-IO-AREA (165:1) TO FGEB (1:1)
               MOVE FIRMAF-IO-AREA (169:1) TO ELEK (1:1)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-09                        TO TRUE.
 
       KUNDEMX-FLDSET SECTION.
       KUNDEMX-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMX-IO-AREA (43:1) TO FPRTYP (1:1)
           END-EVALUATE.
 
       KUNDEMX-IDSET SECTION.
       KUNDEMX-IDSET-P.
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
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  FLOPP-EOF
               MOVE HIGH-VALUES            TO FLOPP-MC
                                              FLOPP-MP
           END-IF
           IF  FAKTIN-EOF
               MOVE HIGH-VALUES            TO FAKTIN-MC
                                              FAKTIN-MP
           END-IF
           IF  FLOPP-MC < FLOPP-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  FAKTIN-MC < FAKTIN-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  FLOPP-MC < FAKTIN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FLOPP-PROCESS       TO TRUE
                   MOVE FLOPP-MC           TO FLOPP-MP
                   IF  FLOPP-MC = FAKTIN-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  FAKTIN-MC < FLOPP-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FAKTIN-PROCESS      TO TRUE
                   MOVE FAKTIN-MC          TO FAKTIN-MP
                   IF  FAKTIN-MC = FLOPP-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  FLOPP-MC = FAKTIN-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FLOPP-PROCESS       TO TRUE
                   MOVE FLOPP-MC           TO FLOPP-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
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
 
       BONUST-LOAD SECTION.
       BONUST-LOAD-P.
           OPEN INPUT BONUST
           SET TABBON-I                    TO 1
           PERFORM UNTIL BONUST-EOF
               READ BONUST
               AT END
                   SET BONUST-EOF          TO TRUE
               NOT AT END
                   MOVE BONUST-IO-AREA (1:64) TO TABBON-ENTRY
                                                            (TABBON-I)
                   SET TABBON-I            UP BY 1
               END-READ
           END-PERFORM
           CLOSE BONUST.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-12)
               MOVE SPACES TO VAREREC-IO-AREA
               INITIALIZE VAREREC-IO-AREA
               MOVE '8'                    TO VAREREC-IO-AREA (1:1)
               MOVE REGAVD                 TO VAREREC-IO-AREA (2:1)
               MOVE KOSTPR                 TO XO-72P
               MOVE XO-72P-EF              TO VAREREC-IO-AREA (3:5)
               MOVE BESENH                 TO XO-52P
               MOVE XO-52P-EF              TO VAREREC-IO-AREA (8:4)
               IF  (I-31 AND I-32)
                   MOVE KORANT             TO XO-52P
                   MOVE XO-52P-EF          TO VAREREC-IO-AREA (8:4)
               END-IF
               MOVE LEVENH                 TO XO-52P
               MOVE XO-52P-EF              TO VAREREC-IO-AREA (12:4)
               IF  (I-31 AND I-32)
                   MOVE KORANT             TO XO-52P
                   MOVE XO-52P-EF          TO VAREREC-IO-AREA (12:4)
               END-IF
               MOVE EDBNR                  TO VAREREC-IO-AREA (16:7)
               MOVE RAB1-IO                TO VAREREC-IO-AREA (23:3)
               MOVE RAB2-IO                TO VAREREC-IO-AREA (26:3)
               MOVE RAB3-IO                TO VAREREC-IO-AREA (29:3)
               MOVE NYPRIS-IO              TO VAREREC-IO-AREA (32:9)
               MOVE FAKRNR                 TO VAREREC-IO-AREA (41:1)
               IF  (NOT-I-U1)
                   MOVE PMND               TO VAREREC-IO-AREA (42:2)
               END-IF
               IF  (I-U1)
                   MOVE KPMND              TO VAREREC-IO-AREA (42:2)
               END-IF
               MOVE BK                     TO VAREREC-IO-AREA (44:1)
               MOVE RESKNR                 TO VAREREC-IO-AREA (45:6)
               MOVE FIRMNR                 TO VAREREC-IO-AREA (51:3)
               MOVE HDIST                  TO VAREREC-IO-AREA (54:3)
               MOVE OKODE                  TO VAREREC-IO-AREA (57:1)
               IF  (NOT-I-U1)
                   MOVE PAR                TO VAREREC-IO-AREA (58:2)
               END-IF
               IF  (I-U1)
                   MOVE KPAR               TO VAREREC-IO-AREA (58:2)
               END-IF
               MOVE VGR                    TO VAREREC-IO-AREA (60:5)
               IF  (I-11)
                   MOVE KRETYP             TO VAREREC-IO-AREA (66:1)
               END-IF
               IF  (NOT-I-11)
                   MOVE VLKOD1             TO VAREREC-IO-AREA (66:1)
               END-IF
               IF  (NOT-I-U1)
                   MOVE FNRMND             TO VAREREC-IO-AREA (68:1)
               END-IF
               IF  (I-U1)
                   MOVE 'K'                TO VAREREC-IO-AREA (68:1)
               END-IF
               MOVE LAGERK                 TO VAREREC-IO-AREA (69:2)
               IF  (I-31)
                   MOVE 'RG'               TO VAREREC-IO-AREA (69:2)
               END-IF
               MOVE NORDNR                 TO XO-60P
               MOVE XO-60P-EF              TO VAREREC-IO-AREA (71:4)
               MOVE ORDSEQ                 TO XO-30P
               MOVE XO-30P-EF              TO VAREREC-IO-AREA (75:2)
               MOVE DATO                   TO XO-60P
               MOVE XO-60P-EF              TO VAREREC-IO-AREA (77:4)
               MOVE BETB                   TO VAREREC-IO-AREA (81:2)
      * FAKTURA VAREREC HASTERGEBYR.
               WRITE VAREREC-IO-AREA
           END-IF
           IF  (I-03)
               MOVE SPACES TO FAKTUT-IO-AREA
               INITIALIZE FAKTUT-IO-AREA
               MOVE FAKREC                 TO FAKTUT-IO-AREA (1:200)
               IF  (I-24 AND I-25)
                   MOVE NULL9-IO           TO FAKTUT-IO-AREA (157:9)
               END-IF
               MOVE FAKART                 TO FAKTUT-IO-AREA (11:1)
      * HASTER-GEBYR RECORDS.
               WRITE FAKTUT-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-75)
               MOVE SPACES TO VAREREC-IO-AREA
               INITIALIZE VAREREC-IO-AREA
               MOVE '8'                    TO VAREREC-IO-AREA (1:1)
               MOVE REGAVD                 TO VAREREC-IO-AREA (2:1)
               MOVE NULL-X                 TO XO-72P
               MOVE XO-72P-EF              TO VAREREC-IO-AREA (3:5)
               MOVE GEBANT                 TO XO-52P
               MOVE XO-52P-EF              TO VAREREC-IO-AREA (8:4)
               MOVE GEBANT                 TO XO-52P
               MOVE XO-52P-EF              TO VAREREC-IO-AREA (12:4)
               MOVE SEDBNR                 TO VAREREC-IO-AREA (16:7)
               MOVE '000'                  TO VAREREC-IO-AREA (23:3)
               MOVE '000'                  TO VAREREC-IO-AREA (26:3)
               MOVE '000'                  TO VAREREC-IO-AREA (29:3)
               MOVE FGEBYR-IO              TO VAREREC-IO-AREA (32:9)
               MOVE FAKRNR                 TO VAREREC-IO-AREA (41:1)
               IF  (NOT-I-U1)
                   MOVE PMND               TO VAREREC-IO-AREA (42:2)
               END-IF
               IF  (I-U1)
                   MOVE KPMND              TO VAREREC-IO-AREA (42:2)
               END-IF
               MOVE BK                     TO VAREREC-IO-AREA (44:1)
               MOVE RESKNR                 TO VAREREC-IO-AREA (45:6)
               MOVE FIRMNR                 TO VAREREC-IO-AREA (51:3)
               MOVE HDIST                  TO VAREREC-IO-AREA (54:3)
               MOVE OKODE                  TO VAREREC-IO-AREA (57:1)
               IF  (NOT-I-U1)
                   MOVE PAR                TO VAREREC-IO-AREA (58:2)
               END-IF
               IF  (I-U1)
                   MOVE KPAR               TO VAREREC-IO-AREA (58:2)
               END-IF
               MOVE SVGR                   TO VAREREC-IO-AREA (60:5)
               IF  (I-11)
                   MOVE KRETYP             TO VAREREC-IO-AREA (66:1)
               END-IF
               IF  (NOT-I-11)
                   MOVE VLKOD1             TO VAREREC-IO-AREA (66:1)
               END-IF
               IF  (NOT-I-U1)
                   MOVE FNRMND             TO VAREREC-IO-AREA (68:1)
               END-IF
               IF  (I-U1)
                   MOVE 'K'                TO VAREREC-IO-AREA (68:1)
               END-IF
               MOVE LAGERK                 TO VAREREC-IO-AREA (69:2)
               MOVE NORDNR                 TO XO-60P
               MOVE XO-60P-EF              TO VAREREC-IO-AREA (71:4)
               MOVE ORDSEQ                 TO XO-30P
               MOVE XO-30P-EF              TO VAREREC-IO-AREA (75:2)
               MOVE DATO                   TO XO-60P
               MOVE XO-60P-EF              TO VAREREC-IO-AREA (77:4)
               MOVE BETB                   TO VAREREC-IO-AREA (81:2)
      * FAKTURA RADIO FRAKT OG FORSIKRINGS RECORD
               WRITE VAREREC-IO-AREA
           END-IF
           IF  (I-L1 AND I-47)
               MOVE SPACES TO VAREREC-IO-AREA
               INITIALIZE VAREREC-IO-AREA
               MOVE '8'                    TO VAREREC-IO-AREA (1:1)
               MOVE REGAVD                 TO VAREREC-IO-AREA (2:1)
               IF  (I-28)
                   MOVE '4'                TO VAREREC-IO-AREA (2:1)
               END-IF
               MOVE NULL-X                 TO XO-72P
               MOVE XO-72P-EF              TO VAREREC-IO-AREA (3:5)
               MOVE RFFANT                 TO XO-52P
               MOVE XO-52P-EF              TO VAREREC-IO-AREA (8:4)
               MOVE RFFANT                 TO XO-52P
               MOVE XO-52P-EF              TO VAREREC-IO-AREA (12:4)
               MOVE BOEDBN                 TO VAREREC-IO-AREA (16:7)
               MOVE '000'                  TO VAREREC-IO-AREA (23:3)
               MOVE '000'                  TO VAREREC-IO-AREA (26:3)
               MOVE '000'                  TO VAREREC-IO-AREA (29:3)
               MOVE RFFSUM-IO              TO VAREREC-IO-AREA (32:9)
               MOVE FAKRNR                 TO VAREREC-IO-AREA (41:1)
               IF  (NOT-I-U1)
                   MOVE PMND               TO VAREREC-IO-AREA (42:2)
               END-IF
               IF  (I-U1)
                   MOVE KPMND              TO VAREREC-IO-AREA (42:2)
               END-IF
               MOVE BK                     TO VAREREC-IO-AREA (44:1)
               MOVE RESKNR                 TO VAREREC-IO-AREA (45:6)
               MOVE FIRMNR                 TO VAREREC-IO-AREA (51:3)
               MOVE HDIST                  TO VAREREC-IO-AREA (54:3)
               MOVE OKODE                  TO VAREREC-IO-AREA (57:1)
               IF  (NOT-I-U1)
                   MOVE PAR                TO VAREREC-IO-AREA (58:2)
               END-IF
               IF  (I-U1)
                   MOVE KPAR               TO VAREREC-IO-AREA (58:2)
               END-IF
               MOVE BOVGR                  TO VAREREC-IO-AREA (60:5)
               IF  (I-11)
                   MOVE KRETYP             TO VAREREC-IO-AREA (66:1)
               END-IF
               IF  (NOT-I-11)
                   MOVE VLKOD1             TO VAREREC-IO-AREA (66:1)
               END-IF
               IF  (NOT-I-U1)
                   MOVE FNRMND             TO VAREREC-IO-AREA (68:1)
               END-IF
               IF  (I-U1)
                   MOVE 'K'                TO VAREREC-IO-AREA (68:1)
               END-IF
               MOVE LAGERK                 TO VAREREC-IO-AREA (69:2)
               MOVE NORDNR                 TO XO-60P
               MOVE XO-60P-EF              TO VAREREC-IO-AREA (71:4)
               MOVE ORDSEQ                 TO XO-30P
               MOVE XO-30P-EF              TO VAREREC-IO-AREA (75:2)
               MOVE DATO                   TO XO-60P
               MOVE XO-60P-EF              TO VAREREC-IO-AREA (77:4)
               MOVE BETB                   TO VAREREC-IO-AREA (81:2)
      * FAKTURA GEBYR VARERECORDS.
               WRITE VAREREC-IO-AREA
           END-IF
           IF  (I-L3 AND I-70)
               MOVE SPACES TO VAREREC-IO-AREA
               INITIALIZE VAREREC-IO-AREA
               MOVE '8'                    TO VAREREC-IO-AREA (1:1)
               MOVE REGAVD                 TO VAREREC-IO-AREA (2:1)
               MOVE NULL-X                 TO XO-72P
               MOVE XO-72P-EF              TO VAREREC-IO-AREA (3:5)
               MOVE GEBANT                 TO XO-52P
               MOVE XO-52P-EF              TO VAREREC-IO-AREA (8:4)
               MOVE GEBANT                 TO XO-52P
               MOVE XO-52P-EF              TO VAREREC-IO-AREA (12:4)
               MOVE SEDBNR                 TO VAREREC-IO-AREA (16:7)
               MOVE '000'                  TO VAREREC-IO-AREA (23:3)
               MOVE '000'                  TO VAREREC-IO-AREA (26:3)
               MOVE '000'                  TO VAREREC-IO-AREA (29:3)
               MOVE FGEBYR-IO              TO VAREREC-IO-AREA (32:9)
               MOVE FAKRNR                 TO VAREREC-IO-AREA (41:1)
               IF  (NOT-I-U1)
                   MOVE PMND               TO VAREREC-IO-AREA (42:2)
               END-IF
               IF  (I-U1)
                   MOVE KPMND              TO VAREREC-IO-AREA (42:2)
               END-IF
               MOVE BK                     TO VAREREC-IO-AREA (44:1)
               MOVE RESKNR                 TO VAREREC-IO-AREA (45:6)
               MOVE FIRMNR                 TO VAREREC-IO-AREA (51:3)
               MOVE HDIST                  TO VAREREC-IO-AREA (54:3)
               MOVE OKODE                  TO VAREREC-IO-AREA (57:1)
               IF  (NOT-I-U1)
                   MOVE PAR                TO VAREREC-IO-AREA (58:2)
               END-IF
               IF  (I-U1)
                   MOVE KPAR               TO VAREREC-IO-AREA (58:2)
               END-IF
               MOVE SVGR                   TO VAREREC-IO-AREA (60:5)
               IF  (I-11)
                   MOVE KRETYP             TO VAREREC-IO-AREA (66:1)
               END-IF
               IF  (NOT-I-11)
                   MOVE VLKOD1             TO VAREREC-IO-AREA (66:1)
               END-IF
               IF  (NOT-I-U1)
                   MOVE FNRMND             TO VAREREC-IO-AREA (68:1)
               END-IF
               IF  (I-U1)
                   MOVE 'K'                TO VAREREC-IO-AREA (68:1)
               END-IF
               MOVE LAGERK                 TO VAREREC-IO-AREA (69:2)
               MOVE NORDNR                 TO XO-60P
               MOVE XO-60P-EF              TO VAREREC-IO-AREA (71:4)
               MOVE ORDSEQ                 TO XO-30P
               MOVE XO-30P-EF              TO VAREREC-IO-AREA (75:2)
               MOVE DATO                   TO XO-60P
               MOVE XO-60P-EF              TO VAREREC-IO-AREA (77:4)
               MOVE BETB                   TO VAREREC-IO-AREA (81:2)
      * FAKTURA BONUS VARERECORDS.
               WRITE VAREREC-IO-AREA
           END-IF
           IF  (I-L3 AND I-45)
               MOVE SPACES TO VAREREC-IO-AREA
               INITIALIZE VAREREC-IO-AREA
               MOVE '8'                    TO VAREREC-IO-AREA (1:1)
               MOVE REGAVD                 TO VAREREC-IO-AREA (2:1)
               IF  (I-28)
                   MOVE '4'                TO VAREREC-IO-AREA (2:1)
               END-IF
               MOVE NULL-X                 TO XO-72P
               MOVE XO-72P-EF              TO VAREREC-IO-AREA (3:5)
               MOVE BONANT                 TO XO-52P
               MOVE XO-52P-EF              TO VAREREC-IO-AREA (8:4)
               MOVE BONANT                 TO XO-52P
               MOVE XO-52P-EF              TO VAREREC-IO-AREA (12:4)
               MOVE BOEDBN                 TO VAREREC-IO-AREA (16:7)
               MOVE '000'                  TO VAREREC-IO-AREA (23:3)
               MOVE '000'                  TO VAREREC-IO-AREA (26:3)
               MOVE '000'                  TO VAREREC-IO-AREA (29:3)
               MOVE FBONUS-IO              TO VAREREC-IO-AREA (32:9)
               MOVE FAKRNR                 TO VAREREC-IO-AREA (41:1)
               IF  (NOT-I-U1)
                   MOVE PMND               TO VAREREC-IO-AREA (42:2)
               END-IF
               IF  (I-U1)
                   MOVE KPMND              TO VAREREC-IO-AREA (42:2)
               END-IF
               MOVE BK                     TO VAREREC-IO-AREA (44:1)
               MOVE RESKNR                 TO VAREREC-IO-AREA (45:6)
               MOVE FIRMNR                 TO VAREREC-IO-AREA (51:3)
               MOVE HDIST                  TO VAREREC-IO-AREA (54:3)
               MOVE OKODE                  TO VAREREC-IO-AREA (57:1)
               IF  (NOT-I-U1)
                   MOVE PAR                TO VAREREC-IO-AREA (58:2)
               END-IF
               IF  (I-U1)
                   MOVE KPAR               TO VAREREC-IO-AREA (58:2)
               END-IF
               MOVE BOVGR                  TO VAREREC-IO-AREA (60:5)
               IF  (I-11)
                   MOVE KRETYP             TO VAREREC-IO-AREA (66:1)
               END-IF
               IF  (NOT-I-11)
                   MOVE VLKOD1             TO VAREREC-IO-AREA (66:1)
               END-IF
               IF  (NOT-I-U1)
                   MOVE FNRMND             TO VAREREC-IO-AREA (68:1)
               END-IF
               IF  (I-U1)
                   MOVE 'K'                TO VAREREC-IO-AREA (68:1)
               END-IF
               MOVE LAGERK                 TO VAREREC-IO-AREA (69:2)
               MOVE NORDNR                 TO XO-60P
               MOVE XO-60P-EF              TO VAREREC-IO-AREA (71:4)
               MOVE ORDSEQ                 TO XO-30P
               MOVE XO-30P-EF              TO VAREREC-IO-AREA (75:2)
               MOVE DATO                   TO XO-60P
               MOVE XO-60P-EF              TO VAREREC-IO-AREA (77:4)
               MOVE BETB                   TO VAREREC-IO-AREA (81:2)
               WRITE VAREREC-IO-AREA
           END-IF
           IF  (I-L1 AND I-75)
               MOVE SPACES TO FAKTUT-IO-AREA
               INITIALIZE FAKTUT-IO-AREA
               MOVE FAKREC                 TO FAKTUT-IO-AREA (1:200)
               MOVE FAKART                 TO FAKTUT-IO-AREA (11:1)
               MOVE 'L'                    TO FAKTUT-IO-AREA (25:1)
               MOVE SEQNR2-IO              TO FAKTUT-IO-AREA (26:4)
               MOVE SVGR                   TO FAKTUT-IO-AREA (77:5)
               MOVE STKS1                  TO FAKTUT-IO-AREA (82:20)
               MOVE STKS2                  TO FAKTUT-IO-AREA (102:30)
               MOVE GEBANT                 TO XO-52P
               MOVE XO-52P-EF              TO FAKTUT-IO-AREA (133:4)
               MOVE GEBANT                 TO XO-52P
               MOVE XO-52P-EF              TO FAKTUT-IO-AREA (137:4)
               MOVE SEDBNR                 TO FAKTUT-IO-AREA (141:7)
               MOVE '000'                  TO FAKTUT-IO-AREA (148:3)
               MOVE '000'                  TO FAKTUT-IO-AREA (151:3)
               MOVE '000'                  TO FAKTUT-IO-AREA (154:3)
               MOVE FGEBYR-IO              TO FAKTUT-IO-AREA (157:9)
               MOVE NULL-X                 TO XO-72P
               MOVE XO-72P-EF              TO FAKTUT-IO-AREA (171:5)
               MOVE 'F'                    TO FAKTUT-IO-AREA (177:1)
      * FAKTURA FRAKT/FORSIKRINGS RECORD.
               WRITE FAKTUT-IO-AREA
           END-IF
           IF  (I-L1 AND I-47)
               MOVE SPACES TO FAKTUT-IO-AREA
               INITIALIZE FAKTUT-IO-AREA
               MOVE FAKREC                 TO FAKTUT-IO-AREA (1:200)
               MOVE FAKART                 TO FAKTUT-IO-AREA (11:1)
               MOVE 'L'                    TO FAKTUT-IO-AREA (25:1)
               MOVE SEQNR3-IO              TO FAKTUT-IO-AREA (26:4)
               MOVE BOVGR                  TO FAKTUT-IO-AREA (77:5)
               MOVE BOTEK1                 TO FAKTUT-IO-AREA (82:20)
               MOVE BOTEK2                 TO FAKTUT-IO-AREA (102:20)
               MOVE '          '           TO FAKTUT-IO-AREA (122:10)
               MOVE RFFANT                 TO XO-52P
               MOVE XO-52P-EF              TO FAKTUT-IO-AREA (133:4)
               MOVE RFFANT                 TO XO-52P
               MOVE XO-52P-EF              TO FAKTUT-IO-AREA (137:4)
               MOVE BOEDBN                 TO FAKTUT-IO-AREA (141:7)
               MOVE '000'                  TO FAKTUT-IO-AREA (148:3)
               MOVE '000'                  TO FAKTUT-IO-AREA (151:3)
               MOVE '000'                  TO FAKTUT-IO-AREA (154:3)
               MOVE RFFSUM-IO              TO FAKTUT-IO-AREA (157:9)
               IF  (I-28)
                   MOVE '4'                TO FAKTUT-IO-AREA (166:1)
               END-IF
               MOVE NULL-X                 TO XO-72P
               MOVE XO-72P-EF              TO FAKTUT-IO-AREA (171:5)
               MOVE 'B'                    TO FAKTUT-IO-AREA (177:1)
      * FAKTURAGEBYR RECORDS.
               WRITE FAKTUT-IO-AREA
           END-IF
           IF  (I-L3 AND I-70)
               MOVE SPACES TO FAKTUT-IO-AREA
               INITIALIZE FAKTUT-IO-AREA
               MOVE FAKREC                 TO FAKTUT-IO-AREA (1:200)
               MOVE FAKART                 TO FAKTUT-IO-AREA (11:1)
               MOVE 'L'                    TO FAKTUT-IO-AREA (25:1)
               MOVE SEQNR2-IO              TO FAKTUT-IO-AREA (26:4)
               MOVE SVGR                   TO FAKTUT-IO-AREA (77:5)
               MOVE STKS1                  TO FAKTUT-IO-AREA (82:20)
               MOVE STKS2                  TO FAKTUT-IO-AREA (102:30)
               MOVE GEBANT                 TO XO-52P
               MOVE XO-52P-EF              TO FAKTUT-IO-AREA (133:4)
               MOVE GEBANT                 TO XO-52P
               MOVE XO-52P-EF              TO FAKTUT-IO-AREA (137:4)
               MOVE SEDBNR                 TO FAKTUT-IO-AREA (141:7)
               MOVE '000'                  TO FAKTUT-IO-AREA (148:3)
               MOVE '000'                  TO FAKTUT-IO-AREA (151:3)
               MOVE '000'                  TO FAKTUT-IO-AREA (154:3)
               MOVE FGEBYR-IO              TO FAKTUT-IO-AREA (157:9)
               MOVE NULL-X                 TO XO-72P
               MOVE XO-72P-EF              TO FAKTUT-IO-AREA (171:5)
               MOVE 'F'                    TO FAKTUT-IO-AREA (177:1)
      * FAKTURABONUS RECORDS.
               WRITE FAKTUT-IO-AREA
           END-IF
           IF  (I-L3 AND I-45)
               MOVE SPACES TO FAKTUT-IO-AREA
               INITIALIZE FAKTUT-IO-AREA
               MOVE FAKREC                 TO FAKTUT-IO-AREA (1:200)
               MOVE FAKART                 TO FAKTUT-IO-AREA (11:1)
               MOVE 'L'                    TO FAKTUT-IO-AREA (25:1)
               MOVE SEQNR3-IO              TO FAKTUT-IO-AREA (26:4)
               MOVE BOVGR                  TO FAKTUT-IO-AREA (77:5)
               MOVE BOTEK1                 TO FAKTUT-IO-AREA (82:20)
               MOVE BOTEK2                 TO FAKTUT-IO-AREA (102:20)
               MOVE '          '           TO FAKTUT-IO-AREA (122:10)
               MOVE BONANT                 TO XO-52P
               MOVE XO-52P-EF              TO FAKTUT-IO-AREA (133:4)
               MOVE BONANT                 TO XO-52P
               MOVE XO-52P-EF              TO FAKTUT-IO-AREA (137:4)
               MOVE BOEDBN                 TO FAKTUT-IO-AREA (141:7)
               MOVE '000'                  TO FAKTUT-IO-AREA (148:3)
               MOVE '000'                  TO FAKTUT-IO-AREA (151:3)
               MOVE '000'                  TO FAKTUT-IO-AREA (154:3)
               MOVE FBONUS-IO              TO FAKTUT-IO-AREA (157:9)
               IF  (I-28)
                   MOVE '4'                TO FAKTUT-IO-AREA (166:1)
               END-IF
               MOVE NULL-X                 TO XO-72P
               MOVE XO-72P-EF              TO FAKTUT-IO-AREA (171:5)
               MOVE 'B'                    TO FAKTUT-IO-AREA (177:1)
               WRITE FAKTUT-IO-AREA
           END-IF
           IF  (I-L2)
               MOVE SPACES TO SUMFILE-IO-AREA
               INITIALIZE SUMFILE-IO-AREA
               MOVE 'S'                    TO SUMFILE-IO-AREA (1:1)
               MOVE FIRMNR                 TO SUMFILE-IO-AREA (2:3)
               IF  (I-19)
                   MOVE KONFNR             TO SUMFILE-IO-AREA (2:3)
               END-IF
               MOVE RESKNR                 TO SUMFILE-IO-AREA (5:6)
               MOVE SUMLIN                 TO XO-72P
               MOVE XO-72P-EF              TO SUMFILE-IO-AREA (11:5)
               MOVE FAKTYP                 TO SUMFILE-IO-AREA (20:1)
      *RF     T  1     L3 78N76
      *RF     T  1     L3 70
      *                        FIRMNR     4
      *                      19KONFNR     4
      *                        RESKNR    10
      *                        FGEB      14
      *                        PRTYPF    15
      *                78                17 "78"
      *                76                20 "76"
      *                        ELEK      22
      *                        OGEBYR    25
      *                        FGEBYRX   35
      *                        SEDBNR    45
               WRITE SUMFILE-IO-AREA
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
               MOVE '***   AVSTEMNINGSTOTALER' TO PRF-IO-AREA (1:24)
               MOVE '    --- FAK075 ---   ***' TO PRF-IO-AREA (25:24)
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
               MOVE ANTINN                 TO XO-70YY9
               MOVE XO-70YY9               TO PRF-IO-AREA (4:9)
               MOVE 'FAKT. REC. LEST INN.    ' TO PRF-IO-AREA (14:24)
               MOVE 1                      TO PRF-BEFORE-SPACE
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE ANTGEB                 TO XO-70YY9
               MOVE XO-70YY9               TO PRF-IO-AREA (4:9)
               MOVE 'FAKT.GEBYR RECORDS'   TO PRF-IO-AREA (14:18)
               MOVE TOTGEB                 TO XO-72YY9R
               MOVE XO-72YY9R              TO PRF-IO-AREA (38:13)
               MOVE 'FAKT.GEBYR BELØP. '   TO PRF-IO-AREA (53:18)
               MOVE 'HERAV HASTERGEBYR '   TO PRF-IO-AREA (73:18)
               MOVE ANTHGB                 TO XO-70YY9
               MOVE XO-70YY9               TO PRF-IO-AREA (92:9)
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE ANTRFF                 TO XO-70YY9
               MOVE XO-70YY9               TO PRF-IO-AREA (4:9)
               MOVE 'FRAKT/FORS RECORDS'   TO PRF-IO-AREA (14:18)
               MOVE TOTRFF                 TO XO-72YY9R
               MOVE XO-72YY9R              TO PRF-IO-AREA (38:13)
               MOVE 'FRAKT/FORS BELØP. '   TO PRF-IO-AREA (53:18)
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE ANTBON                 TO XO-70YY9
               MOVE XO-70YY9               TO PRF-IO-AREA (4:9)
               MOVE 'FAKT.BONUS RECORDS'   TO PRF-IO-AREA (14:18)
               MOVE TOTBON                 TO XO-72YY9R
               MOVE XO-72YY9R              TO PRF-IO-AREA (38:13)
               MOVE 'FAKT.BONUS BELØP. '   TO PRF-IO-AREA (53:18)
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE ANTSOL                 TO XO-60YY9
               MOVE XO-60YY9               TO PRF-IO-AREA (6:7)
               MOVE 'FAKT.SERV. RECORDS'   TO PRF-IO-AREA (14:18)
               MOVE 'SATT TIL 0 I PRIS.'   TO PRF-IO-AREA (33:18)
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE ANTF                   TO XO-70YY9
               MOVE XO-70YY9               TO PRF-IO-AREA (4:9)
               MOVE 'FAKT. REC. TIL FAKT.    ' TO PRF-IO-AREA (14:24)
               MOVE ANTV                   TO XO-70YY9
               MOVE XO-70YY9               TO PRF-IO-AREA (42:9)
               MOVE 'VARERECORDS TIL STAT.' TO PRF-IO-AREA (53:21)
               MOVE 2                      TO PRF-AFTER-SPACE
               PERFORM PRF-PRINT-LINE
               MOVE SPACES TO PRF-IO-AREA
               INITIALIZE PRF-IO-AREA
               MOVE '       KJØRT'         TO PRF-IO-AREA (7:12)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO PRF-IO-AREA (20:8)
               MOVE 1                      TO PRF-AFTER-SPACE
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
           MOVE 3                          TO LR-CHECK
           PERFORM OSKFILE-LOAD
           PERFORM BONUST-LOAD
           INITIALIZE FLOPP-DATA-FIELDS
           SET FLOPP-EOF-OFF               TO TRUE
           SET FLOPP-PROCESS               TO TRUE
           MOVE LOW-VALUES                 TO FLOPP-MC
                                              FLOPP-MP
           OPEN INPUT FLOPP
           SET FAKTIN-LEVEL-INIT           TO TRUE
           INITIALIZE FAKTIN-DATA-FIELDS
           SET FAKTIN-EOF-OFF              TO TRUE
           SET FAKTIN-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO FAKTIN-MC
                                              FAKTIN-MP
           OPEN INPUT FAKTIN
           INITIALIZE FAKPAR-DATA-FIELDS
           SET FAKPAR-EOF-OFF              TO TRUE
           SET FAKPAR-PROCESS              TO TRUE
           OPEN INPUT FAKPAR
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           INITIALIZE KUNDEMX-DATA-FIELDS
           OPEN INPUT KUNDEMX
           OPEN OUTPUT VAREREC
           OPEN OUTPUT FAKTUT
           OPEN OUTPUT SUMFILE
           OPEN OUTPUT PRF
           INITIALIZE PRF-IO-AREA
           INITIALIZE PRF-DATA-FIELDS
           MOVE 57                         TO PRF-MAX-LINES.
           SET TABOSK-I                    TO 1
           SET TABBON-I                    TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FLOPP
           CLOSE FAKTIN
           CLOSE FAKPAR
           CLOSE FIRMAF
           CLOSE KUNDEMX
           CLOSE VAREREC
           CLOSE FAKTUT
           CLOSE SUMFILE
           IF PRF-IO-AREA NOT = SPACES
             WRITE PRF-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO PRF-IO-AREA
           END-IF
           CLOSE PRF.
 
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
