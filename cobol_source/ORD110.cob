       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORD110R.
      **********************************************  Z-WIN-RPG2   ****
      ***** Program har excel i RWEB *********** XX2000XXOKXXEL *******
      *       O R D R E R U T I N E P R O G R A M   O R D 1 1 0       *
      *       -------------------------------------------------       *
      *  1. DANNE NY ON-LINE ORDREFILE. (REORGANISERING)              *
      *  2. DANNE DAGENS FERDIG REG.ORDREFILE TIL OPPDAT. SERVICEPROS.*
      *     PR. VAREGRUPPEMASTER.                                     *
      *  3. DANNE RESTORDREFILE TIL OPPDATERING I RESTORDRERUTINEN.   *
      *  4. DANNE ORDRE OG AVIKSFILE TIL ORDRE OG AVIKSLISTE.         *
      *  5. SELEKTERE ORDRE TIL FAKTURARUTINEN.                       *
      *  6. DANNE SALGSFILE TIL OPPDATERING AV MND"S. VARESALG.       *
      *  7. OPPDATERE FIRMAFILE. ANT.ORDRE, ANT. VARELINJER PR. MND.  *
      *  8. PRINTE SELGERKOPIER.  DISKFILE                            *
      *  9. PRINTE FERDIGMELDINGSRAPPORT. DISKFILE                    *
      * 10. PRINTE ORDRERAPPORT PR. FIRMA OG TOTALRAPPORT FOR AVSTEM. *
      * 11. DANNE VERKTØYKASSEFILE/ SAMT VERKSTEDSORDREFILE.          *
      * 12. OPPDATERE ORDRENR-DASTER.    NULLFAKTURA-ORDRE.           *
      * 13. OPPDATERE UTAKS.ORDRE.FILE.  KOMM.ORDRE FOR HAFNOR.       *
      * 14. AVSTEMMER OG OPPDATERER AVSTEMFILE. U1 = OMKJØRING.       *
      * 15. ORDRE SOM SKAL SAMLEFAKTURERES HALV/HEL MND. BLIR LIGGENDE*
      *     PÅ ORDREFILE TIL ØNSKET FAKTURA PERIODE. DETTE FOR AT DET *
      *     SKAL VÆRE MULIG Å ENDRE ORDREN HELT FRAM TIL FAKTURERING. *
      *     OBS: NOEN KUNDER FOR OVERFØRT FAKTURAMÅTE FRA FAKT.KUNDENR*
      *          I FAKTURARUTINEN (FAK035).                           *
      * ENDRINGER :                                                   *
      * 16.01.18 - LAGT INN TEST PÅ STATUS="I" (BETYR IKKE NOE)
      * 27.05.14 - LAGT INN TEST PÅ NYFSAL(NY FORHÅNDSSALGSRUTINE)
      * 02.08.06 - LAGT INN AVD I UTAKORD-FOR EXIDE SØNNAK            *
      * 11.01.08 - COMP UTEN ENDRING PGA BETBET ENDRING.              *
      * 26.02.08 - FJERNET FORDRUTINE FOR SØRENSEN OG BALCHEN.        *
      * 15.02.12 - LAGT INN BK=M OG G SALG TIL SELVKOST.              *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ORD110.rpg
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
           SELECT ORDREFS
               ASSIGN TO UT-S-ORDREFS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ORDREFS-STATUS.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT FAKPAR
               ASSIGN TO UT-S-FAKPAR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FAKPAR-STATUS.
           SELECT AVSTEMF
               ASSIGN TO AVSTEMF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS AVSTEMF-STATUS
               RECORD KEY IS AVSTEMF-KEY1.
           SELECT ORDREM
               ASSIGN TO ORDREM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS ORDREM-STATUS
               RECORD KEY IS ORDREM-KEY1.
           SELECT DAGORDR
               ASSIGN TO UT-S-DAGORDR
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS DAGORDR-STATUS.
           SELECT RESTORD
               ASSIGN TO UT-S-RESTORD
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS RESTORD-STATUS.
           SELECT AVIKSUM
               ASSIGN TO UT-S-AVIKSUM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS AVIKSUM-STATUS.
           SELECT ORDFAKT
               ASSIGN TO UT-S-ORDFAKT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ORDFAKT-STATUS.
           SELECT SALGANT
               ASSIGN TO UT-S-SALGANT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS SALGANT-STATUS.
           SELECT VKASSEF
               ASSIGN TO UT-S-VKASSEF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS VKASSEF-STATUS.
           SELECT UTAKORD
               ASSIGN TO UTAKORD
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS UTAKORD-STATUS.
           SELECT ORDNRM
               ASSIGN TO ORDNRM
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS ORDNRM-STATUS
               RECORD KEY IS ORDNRM-KEY1.
           SELECT ORDRAPP
               ASSIGN TO UT-S-ORDRAPP
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ORDRAPP-STATUS.
           SELECT FMRAPP
               ASSIGN TO UT-S-FMRAPP
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FMRAPP-STATUS.
           SELECT PAKKS
               ASSIGN TO UT-S-PAKKS
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PAKKS-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD ORDREFS
               BLOCK CONTAINS 328
               RECORD CONTAINS 164.
       01  ORDREFS-IO-AREA.
           05  ORDREFS-IO-AREA-X           PICTURE X(164).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD FAKPAR
               BLOCK CONTAINS 200
               RECORD CONTAINS 200.
       01  FAKPAR-IO-AREA.
           05  FAKPAR-IO-AREA-X            PICTURE X(200).
       FD AVSTEMF
               RECORD CONTAINS 1000.
       01  AVSTEMF-IO-AREA.
           05  AVSTEMF-IO-AREA-X.
               10  AVSTEMF-KEY1            PICTURE X(3).
               10  FILLER                  PICTURE X(997).
       FD ORDREM
               RECORD CONTAINS 164.
       01  ORDREM-IO-AREA.
           05  ORDREM-IO-AREA-X.
               10  ORDREM-KEY1             PICTURE X(20).
               10  FILLER                  PICTURE X(144).
       FD DAGORDR
               BLOCK CONTAINS 120
               RECORD CONTAINS 60.
       01  DAGORDR-IO-AREA.
           05  DAGORDR-IO-AREA-X           PICTURE X(60).
       FD RESTORD
               BLOCK CONTAINS 260
               RECORD CONTAINS 130.
       01  RESTORD-IO-AREA.
           05  RESTORD-IO-AREA-X           PICTURE X(130).
       FD AVIKSUM
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  AVIKSUM-IO-AREA.
           05  AVIKSUM-IO-AREA-X           PICTURE X(200).
       FD ORDFAKT
               BLOCK CONTAINS 328
               RECORD CONTAINS 164.
       01  ORDFAKT-IO-AREA.
           05  ORDFAKT-IO-AREA-X           PICTURE X(164).
       FD SALGANT
               BLOCK CONTAINS 40
               RECORD CONTAINS 20.
       01  SALGANT-IO-AREA.
           05  SALGANT-IO-AREA-X           PICTURE X(20).
       FD VKASSEF
               BLOCK CONTAINS 140
               RECORD CONTAINS 70.
       01  VKASSEF-IO-AREA.
           05  VKASSEF-IO-AREA-X           PICTURE X(70).
       FD UTAKORD
               RECORD CONTAINS 120.
       01  UTAKORD-IO-AREA.
           05  UTAKORD-IO-AREA-X           PICTURE X(120).
       FD ORDNRM
               RECORD CONTAINS 100.
       01  ORDNRM-IO-AREA.
           05  ORDNRM-IO-AREA-X.
               10  ORDNRM-KEY1             PICTURE X(9).
               10  FILLER                  PICTURE X(91).
       FD ORDRAPP
               BLOCK CONTAINS 300
               RECORD CONTAINS 150.
       01  ORDRAPP-IO-AREA.
           05  ORDRAPP-IO-AREA-X           PICTURE X(150).
       FD FMRAPP
               BLOCK CONTAINS 300
               RECORD CONTAINS 150.
       01  FMRAPP-IO-AREA.
           05  FMRAPP-IO-AREA-X            PICTURE X(150).
       FD PAKKS
               BLOCK CONTAINS 300
               RECORD CONTAINS 150.
       01  PAKKS-IO-AREA.
           05  PAKKS-IO-AREA-X             PICTURE X(150).
       WORKING-STORAGE SECTION.
       77  ARK-MAX   VALUE 13              PICTURE 9(4) USAGE BINARY.
       77  ARO-MAX   VALUE 13              PICTURE 9(4) USAGE BINARY.
       77  AOD-MAX   VALUE 13              PICTURE 9(4) USAGE BINARY.
       77  AOK-MAX   VALUE 13              PICTURE 9(4) USAGE BINARY.
       77  ALD-MAX   VALUE 13              PICTURE 9(4) USAGE BINARY.
       77  ALK-MAX   VALUE 13              PICTURE 9(4) USAGE BINARY.
       77  ALR-MAX   VALUE 13              PICTURE 9(4) USAGE BINARY.
       77  TOB-MAX   VALUE 13              PICTURE 9(4) USAGE BINARY.
       77  TKB-MAX   VALUE 13              PICTURE 9(4) USAGE BINARY.
       77  TRB-MAX   VALUE 13              PICTURE 9(4) USAGE BINARY.
       77  TRO-MAX   VALUE 13              PICTURE 9(4) USAGE BINARY.
       77  TRK-MAX   VALUE 13              PICTURE 9(4) USAGE BINARY.
       77  ARF-MAX   VALUE 9               PICTURE 9(4) USAGE BINARY.
       77  ORA-MAX   VALUE 8               PICTURE 9(4) USAGE BINARY.
       77  KRA-MAX   VALUE 6               PICTURE 9(4) USAGE BINARY.
       77  ORB-MAX   VALUE 7               PICTURE 9(4) USAGE BINARY.
       77  KRB-MAX   VALUE 6               PICTURE 9(4) USAGE BINARY.
       77  OAT-MAX   VALUE 8               PICTURE 9(4) USAGE BINARY.
       77  KAT-MAX   VALUE 6               PICTURE 9(4) USAGE BINARY.
       77  OBT-MAX   VALUE 7               PICTURE 9(4) USAGE BINARY.
       77  KBT-MAX   VALUE 6               PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  ARK-TABLE.
               10  ARK-ENTRY
                                           OCCURS 13 TIMES
                                           INDEXED BY ARK-I
                                                      ARK-S.
                   15  ARK                 PICTURE S9(7)V9(2).
           05  ARO-TABLE.
               10  ARO-ENTRY
                                           OCCURS 13 TIMES
                                           INDEXED BY ARO-I
                                                      ARO-S.
                   15  ARO                 PICTURE S9(7)V9(2).
           05  AOD-TABLE.
               10  AOD-ENTRY
                                           OCCURS 13 TIMES
                                           INDEXED BY AOD-I
                                                      AOD-S.
                   15  AOD                 PICTURE S9(5).
           05  AOK-TABLE.
               10  AOK-ENTRY
                                           OCCURS 13 TIMES
                                           INDEXED BY AOK-I
                                                      AOK-S.
                   15  AOK                 PICTURE S9(5).
           05  ALD-TABLE.
               10  ALD-ENTRY
                                           OCCURS 13 TIMES
                                           INDEXED BY ALD-I
                                                      ALD-S.
                   15  ALD                 PICTURE S9(5).
           05  ALK-TABLE.
               10  ALK-ENTRY
                                           OCCURS 13 TIMES
                                           INDEXED BY ALK-I
                                                      ALK-S.
                   15  ALK                 PICTURE S9(5).
           05  ALR-TABLE.
               10  ALR-ENTRY
                                           OCCURS 13 TIMES
                                           INDEXED BY ALR-I
                                                      ALR-S.
                   15  ALR                 PICTURE S9(5).
           05  TOB-TABLE.
               10  TOB-ENTRY
                                           OCCURS 13 TIMES
                                           INDEXED BY TOB-I
                                                      TOB-S.
                   15  TOB                 PICTURE S9(7)V9(2).
           05  TKB-TABLE.
               10  TKB-ENTRY
                                           OCCURS 13 TIMES
                                           INDEXED BY TKB-I
                                                      TKB-S.
                   15  TKB                 PICTURE S9(7)V9(2).
           05  TRB-TABLE.
               10  TRB-ENTRY
                                           OCCURS 13 TIMES
                                           INDEXED BY TRB-I
                                                      TRB-S.
                   15  TRB                 PICTURE S9(7)V9(2).
           05  TRO-TABLE.
               10  TRO-ENTRY
                                           OCCURS 13 TIMES
                                           INDEXED BY TRO-I
                                                      TRO-S.
                   15  TRO                 PICTURE S9(7)V9(2).
           05  TRK-TABLE.
               10  TRK-ENTRY
                                           OCCURS 13 TIMES
                                           INDEXED BY TRK-I
                                                      TRK-S.
                   15  TRK                 PICTURE S9(7)V9(2).
           05  ARF-TABLE.
               10  ARF-ENTRY
                                           OCCURS 9 TIMES
                                           INDEXED BY ARF-I
                                                      ARF-S.
                   15  ARF                 PICTURE S9(7)V9(2).
           05  ORA-TABLE.
               10  ORA-ENTRY
                                           OCCURS 8 TIMES
                                           INDEXED BY ORA-I
                                                      ORA-S.
                   15  ORA                 PICTURE S9(6).
           05  KRA-TABLE.
               10  KRA-ENTRY
                                           OCCURS 6 TIMES
                                           INDEXED BY KRA-I
                                                      KRA-S.
                   15  KRA                 PICTURE S9(6).
           05  ORB-TABLE.
               10  ORB-ENTRY
                                           OCCURS 7 TIMES
                                           INDEXED BY ORB-I
                                                      ORB-S.
                   15  ORB                 PICTURE S9(8)V9(2).
           05  KRB-TABLE.
               10  KRB-ENTRY
                                           OCCURS 6 TIMES
                                           INDEXED BY KRB-I
                                                      KRB-S.
                   15  KRB                 PICTURE S9(8)V9(2).
           05  OAT-TABLE.
               10  OAT-ENTRY
                                           OCCURS 8 TIMES
                                           INDEXED BY OAT-I
                                                      OAT-S.
                   15  OAT                 PICTURE S9(6).
           05  KAT-TABLE.
               10  KAT-ENTRY
                                           OCCURS 6 TIMES
                                           INDEXED BY KAT-I
                                                      KAT-S.
                   15  KAT                 PICTURE S9(6).
           05  OBT-TABLE.
               10  OBT-ENTRY
                                           OCCURS 7 TIMES
                                           INDEXED BY OBT-I
                                                      OBT-S.
                   15  OBT                 PICTURE S9(8)V9(2).
           05  KBT-TABLE.
               10  KBT-ENTRY
                                           OCCURS 6 TIMES
                                           INDEXED BY KBT-I
                                                      KBT-S.
                   15  KBT                 PICTURE S9(8)V9(2).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  ORDREFS-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  FAKPAR-STATUS               PICTURE 99 VALUE 0.
           10  AVSTEMF-STATUS              PICTURE 99 VALUE 0.
           10  ORDREM-STATUS               PICTURE 99 VALUE 0.
           10  DAGORDR-STATUS              PICTURE 99 VALUE 0.
           10  RESTORD-STATUS              PICTURE 99 VALUE 0.
           10  AVIKSUM-STATUS              PICTURE 99 VALUE 0.
           10  ORDFAKT-STATUS              PICTURE 99 VALUE 0.
           10  SALGANT-STATUS              PICTURE 99 VALUE 0.
           10  VKASSEF-STATUS              PICTURE 99 VALUE 0.
           10  UTAKORD-STATUS              PICTURE 99 VALUE 0.
           10  ORDNRM-STATUS               PICTURE 99 VALUE 0.
           10  ORDRAPP-STATUS              PICTURE 99 VALUE 0.
           10  FMRAPP-STATUS               PICTURE 99 VALUE 0.
           10  PAKKS-STATUS                PICTURE 99 VALUE 0.
           10  BMFELT-XX-STATUS            PICTURE 99 VALUE 0.
           10  MVFELT-XX-STATUS            PICTURE 99 VALUE 0.
           10  DATOER-XX-STATUS            PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDREFS-EOF-OFF         VALUE '0'.
               88  ORDREFS-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDREFS-READ-OFF        VALUE '0'.
               88  ORDREFS-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ORDREFS-PROCESS-OFF     VALUE '0'.
               88  ORDREFS-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  ORDREFS-LEVEL-INIT-OFF  VALUE '0'.
               88  ORDREFS-LEVEL-INIT      VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  AVSTEMF-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  ORDREM-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  ORDNRM-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
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
               10  FILLER                  PICTURE X(78).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(2).
               10  BMTKST                  PICTURE X(24).
               10  FILLER                  PICTURE X(54).
           05  MVFELT-XX REDEFINES BMFELT-XX-DATA-FIELDS.
               10  BUMVA-IO.
                   15  BUMVA               PICTURE S9(9)V9(2).
               10  FILLER                  PICTURE X(69).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(11).
               10  BMMVA-IO.
                   15  BMMVA               PICTURE S9(9)V9(2).
               10  FILLER                  PICTURE X(58).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(22).
               10  MVA-IO.
                   15  MVA                 PICTURE S9(9)V9(2).
               10  FILLER                  PICTURE X(47).
           05  DATOER-XX REDEFINES BMFELT-XX-DATA-FIELDS.
               10  DATOK                   PICTURE X(1).
               10  FILLER                  PICTURE X(79).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(1).
               10  DATO6                   PICTURE X(6).
               10  FILLER                  PICTURE X(73).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(7).
               10  DMA8                    PICTURE X(8).
               10  FILLER                  PICTURE X(65).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(15).
               10  AMD8                    PICTURE X(8).
               10  FILLER                  PICTURE X(57).
           05  FILLER REDEFINES BMFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(23).
               10  DATOM                   PICTURE X(57).
      *DSDS: DATA STRUCTURE FIELDS
           05  MVFELT-XX-DATA-FIELDS.
               10  BUMVA-IO.
                   15  BUMVA               PICTURE S9(9)V9(2).
               10  FILLER                  PICTURE X(69).
           05  FILLER REDEFINES MVFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(11).
               10  BMMVA-IO.
                   15  BMMVA               PICTURE S9(9)V9(2).
               10  FILLER                  PICTURE X(58).
           05  FILLER REDEFINES MVFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(22).
               10  MVA-IO.
                   15  MVA                 PICTURE S9(9)V9(2).
               10  FILLER                  PICTURE X(47).
           05  DATOER-XX REDEFINES MVFELT-XX-DATA-FIELDS.
               10  DATOK                   PICTURE X(1).
               10  FILLER                  PICTURE X(79).
           05  FILLER REDEFINES MVFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(1).
               10  DATO6                   PICTURE X(6).
               10  FILLER                  PICTURE X(73).
           05  FILLER REDEFINES MVFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(7).
               10  DMA8                    PICTURE X(8).
               10  FILLER                  PICTURE X(65).
           05  FILLER REDEFINES MVFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(15).
               10  AMD8                    PICTURE X(8).
               10  FILLER                  PICTURE X(57).
           05  FILLER REDEFINES MVFELT-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(23).
               10  DATOM                   PICTURE X(57).
      *DSDS: DATA STRUCTURE FIELDS
           05  DATOER-XX-DATA-FIELDS.
               10  DATOK                   PICTURE X(1).
               10  FILLER                  PICTURE X(79).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(1).
               10  DATO6                   PICTURE X(6).
               10  FILLER                  PICTURE X(73).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(7).
               10  DMA8                    PICTURE X(8).
               10  FILLER                  PICTURE X(65).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(15).
               10  AMD8                    PICTURE X(8).
               10  FILLER                  PICTURE X(57).
           05  FILLER REDEFINES DATOER-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(23).
               10  DATOM                   PICTURE X(57).
           05  ORDREFS-LEVEL-01.
               10  ORDREFS-01-L2.
                   15  ORDREFS-01-L2-FIRMA PICTURE X(3).
               10  ORDREFS-01-L1.
                   15  ORDREFS-01-L1-ORDNR PICTURE X(6).
           05  ORDREFS-LEVEL-02.
               10  ORDREFS-02-L2.
                   15  ORDREFS-02-L2-FIRMA PICTURE X(3).
               10  ORDREFS-02-L1.
                   15  ORDREFS-02-L1-ORDNR PICTURE X(6).
           05  ORDREFS-LEVEL-03.
               10  ORDREFS-03-L2.
                   15  ORDREFS-03-L2-FIRMA PICTURE X(3).
               10  ORDREFS-03-L1.
                   15  ORDREFS-03-L1-ORDNR PICTURE X(6).
           05  ORDREFS-LEVEL-04.
               10  ORDREFS-04-L2.
                   15  ORDREFS-04-L2-FIRMA PICTURE X(3).
               10  ORDREFS-04-L1.
                   15  ORDREFS-04-L1-ORDNR PICTURE X(6).
           05  ORDREFS-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  ORDNR                   PICTURE X(6).
               10  KUNDNR                  PICTURE X(6).
               10  KNAVN1                  PICTURE X(30).
               10  KNAVN2                  PICTURE X(30).
               10  KTSIFF                  PICTURE X(1).
               10  DIRREG                  PICTURE X(1).
               10  FRITT                   PICTURE X(1).
               10  LAGER                   PICTURE X(2).
               10  BK                      PICTURE X(1).
               10  SKAF                    PICTURE X(1).
               10  BETM                    PICTURE X(2).
               10  GEBYR                   PICTURE X(1).
               10  FRAKT                   PICTURE X(1).
               10  AVD                     PICTURE X(1).
               10  KRTYPE                  PICTURE X(1).
               10  REST                    PICTURE X(1).
               10  KOMUTA                  PICTURE X(1).
               10  SAMFAK                  PICTURE X(1).
               10  PLUKAV                  PICTURE X(2).
               10  ORDATO-IO.
                   15  ORDATO              PICTURE S9(6).
               10  ORDDAG                  PICTURE X(2).
               10  ORDMND                  PICTURE X(2).
               10  ORDAAR                  PICTURE X(2).
               10  ORDMOT                  PICTURE X(2).
               10  TERMID                  PICTURE X(4).
               10  SELGKP                  PICTURE X(1).
               10  FERDIM                  PICTURE X(1).
               10  FAKTNR                  PICTURE X(2).
               10  KONFAK                  PICTURE X(1).
               10  REGKL-IO.
                   15  REGKL               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  RUTID                   PICTURE X(1).
               10  ANTPRT-IO.
                   15  ANTPRT              PICTURE S9(2).
               10  STATUS-X                PICTURE X(1).
               10  OHREC1                  PICTURE X(164).
               10  FAKREF                  PICTURE X(6).
               10  AVNAVN                  PICTURE X(11).
               10  REKVNR                  PICTURE X(15).
               10  FORSM                   PICTURE X(15).
               10  FORS5F                  PICTURE X(5).
               10  HND                     PICTURE X(3).
               10  KADR                    PICTURE X(30).
               10  POSTNR                  PICTURE X(4).
               10  PSTED                   PICTURE X(15).
               10  OHREC2                  PICTURE X(164).
               10  VAADR1                  PICTURE X(30).
               10  VAADR2                  PICTURE X(30).
               10  VAADR3                  PICTURE X(30).
               10  VAADR4                  PICTURE X(20).
               10  OHREC3                  PICTURE X(164).
               10  LAGLOC                  PICTURE X(6).
               10  POSNR-IO.
                   15  POSNR               PICTURE S9(3).
               10  ANTBES-IO.
                   15  ANTBES              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ANTRES-IO.
                   15  ANTRES              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ANTLEV-IO.
                   15  ANTLEV              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  NOREST                  PICTURE X(1).
               10  ALF                     PICTURE X(3).
               10  TEKST                   PICTURE X(50).
               10  ARTNR                   PICTURE X(20).
               10  VARBET                  PICTURE X(30).
               10  VARB20                  PICTURE X(20).
               10  EDBNR-IO.
                   15  EDBNR               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  VGR-IO.
                   15  VGR                 PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  ORPRIS-IO.
                   15  ORPRIS              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ORRAB1-IO.
                   15  ORRAB1              PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  ORRAB2-IO.
                   15  ORRAB2              PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  ORRAB3-IO.
                   15  ORRAB3              PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  RGPRIS-IO.
                   15  RGPRIS              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  RGRAB1-IO.
                   15  RGRAB1              PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  RGRAB2-IO.
                   15  RGRAB2              PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  RGRAB3-IO.
                   15  RGRAB3              PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  KOSPRI-IO.
                   15  KOSPRI              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  PRITIL-IO.
                   15  PRITIL              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  PRITYP                  PICTURE X(1).
               10  VLKOD2                  PICTURE X(1).
               10  DIFLEV-IO.
                   15  DIFLEV              PICTURE S9(5)V9(2).
               10  VEKT                    PICTURE X(5).
               10  NETFAK-IO.
                   15  NETFAK              PICTURE S9(1)V9(4) USAGE
                                                       PACKED-DECIMAL.
               10  OVREC                   PICTURE X(164).
           05  FIRMAF-DATA-FIELDS.
               10  FINAVN                  PICTURE X(30).
               10  FSLETT                  PICTURE X(1).
               10  FFAKGR                  PICTURE X(1).
               10  FIADR                   PICTURE X(30).
               10  FIPNR                   PICTURE X(4).
               10  FIPOST                  PICTURE X(26).
               10  FMRUT                   PICTURE X(1).
               10  FAVLDM-IO.
                   15  FAVLDM              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  FAORDM-IO.
                   15  FAORDM              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  FIREST                  PICTURE X(1).
               10  NYFSAL                  PICTURE X(1).
               10  FADODM-IO.
                   15  FADODM              PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  BRTYPE                  PICTURE X(1).
           05  FAKPAR-DATA-FIELDS.
               10  PNFAKO                  PICTURE X(2).
               10  PSRDAT                  PICTURE X(6).
               10  PSFAKT                  PICTURE X(1).
               10  SFIMND                  PICTURE X(1).
               10  PFFAKO                  PICTURE X(2).
               10  PFAKGR                  PICTURE X(1).
               10  IKKEF1                  PICTURE X(3).
               10  IKKEF2                  PICTURE X(3).
               10  IKKEF3                  PICTURE X(3).
           05  AVSTEMF-DATA-FIELDS.
               10  AVSG1                   PICTURE X(110).
               10  AVSG2                   PICTURE X(110).
      *****************************************************************
      *                 H O V E D R U T I N E .                       *
      *****************************************************************
      * FIRMA SOM BENYTTER VERKTØYKASSER.
           05  ORDNRM-DATA-FIELDS.
               10  FILLER                  PICTURE X.
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  FMVLI-IO.
                   15  FMVLI               PICTURE S9(6).
               10  FMVLIK-IO.
                   15  FMVLIK              PICTURE S9(6).
               10  FMVNUL-IO.
                   15  FMVNUL              PICTURE S9(6).
               10  FMVLIO-IO.
                   15  FMVLIO              PICTURE S9(6).
               10  FMOSUM-IO.
                   15  FMOSUM              PICTURE S9(8)V9(2).
               10  TYPTEK                  PICTURE X(16).
               10  NYAVD                   PICTURE X(1).
               10  X-IO.
                   15  X                   PICTURE S9(2).
               10  A-ELGMD                 PICTURE X(6).
               10  ORDAT3-IO.
                   15  ORDAT3              PICTURE S9(6).
               10  ORDAT2                  PICTURE X(8).
               10  ANTPRX-IO.
                   15  ANTPRX              PICTURE S9(2).
               10  ANTUTG-IO.
                   15  ANTUTG              PICTURE S9(5)V9(2).
               10  EDBNR3-IO.
                   15  EDBNR3              PICTURE S9(3).
               10  EDBNR2-IO.
                   15  EDBNR2              PICTURE S9(2).
               10  NETTO-IO.
                   15  NETTO               PICTURE S9(7)V9(2).
               10  SUM1-IO.
                   15  SUM1                PICTURE S9(9)V9(2).
               10  RABBEL-IO.
                   15  RABBEL              PICTURE S9(7)V9(2).
               10  NETSTK-IO.
                   15  NETSTK              PICTURE S9(7)V9(2).
               10  NETPRI-IO.
                   15  NETPRI              PICTURE S9(7)V9(2).
               10  BSTPRI-IO.
                   15  BSTPRI              PICTURE S9(7)V9(2).
               10  NETSUM-IO.
                   15  NETSUM              PICTURE S9(7)V9(2).
               10  NETANT-IO.
                   15  NETANT              PICTURE S9(7)V9(2).
               10  RESANT-IO.
                   15  RESANT              PICTURE S9(5)V9(2).
               10  RESBEL-IO.
                   15  RESBEL              PICTURE S9(7)V9(2).
               10  PRITUT-IO.
                   15  PRITUT              PICTURE S9(7)V9(2).
               10  REGPRI-IO.
                   15  REGPRI              PICTURE S9(7)V9(2).
               10  ANTB-IO.
                   15  ANTB                PICTURE S9(5).
               10  ANTR-IO.
                   15  ANTR                PICTURE S9(5).
               10  ANTB50-IO.
                   15  ANTB50              PICTURE S9(5).
               10  ANTR50-IO.
                   15  ANTR50              PICTURE S9(5).
               10  VGRAVD-IO.
                   15  VGRAVD              PICTURE S9(1).
               10  VGR-N-IO.
                   15  VGR-N               PICTURE S9(5).
               10  ANTBEH-IO.
                   15  ANTBEH              PICTURE S9(7)V9(2).
               10  ANTVKA-IO.
                   15  ANTVKA              PICTURE S9(5)V9(2).
               10  ANTKBE-IO.
                   15  ANTKBE              PICTURE S9(7)V9(2).
               10  ANTKUT-IO.
                   15  ANTKUT              PICTURE S9(7)V9(2).
               10  VEKTN-IO.
                   15  VEKTN               PICTURE S9(3)V9(2).
               10  VEKTVL-IO.
                   15  VEKTVL              PICTURE S9(7)V9(2).
               10  VEKTL2-IO.
                   15  VEKTL2              PICTURE S9(9)V9(2).
               10  ANTLF-IO.
                   15  ANTLF               PICTURE S9(7)V9(2).
               10  VDIFF2-IO.
                   15  VDIFF2              PICTURE S9(7)V9(2).
               10  ODIFF2-IO.
                   15  ODIFF2              PICTURE S9(7)V9(2).
               10  FDIFF2-IO.
                   15  FDIFF2              PICTURE S9(7)V9(2).
               10  Y-IO.
                   15  Y                   PICTURE S9(1).
               10  ANTEL-IO.
                   15  ANTEL               PICTURE S9(4).
               10  TIDSP-IO.
                   15  TIDSP               PICTURE S9(6).
               10  ANTL-IO.
                   15  ANTL                PICTURE S9(2).
               10  ANTB1-IO.
                   15  ANTB1               PICTURE S9(5).
               10  ANTB2-IO.
                   15  ANTB2               PICTURE S9(3)V9(2).
               10  ANTB3-IO.
                   15  ANTB3               PICTURE S9(5)V9(2).
               10  ANTR1-IO.
                   15  ANTR1               PICTURE S9(5).
               10  ANTR2-IO.
                   15  ANTR2               PICTURE S9(3)V9(2).
               10  ANTR3-IO.
                   15  ANTR3               PICTURE S9(5)V9(2).
               10  MOMS-IO.
                   15  MOMS                PICTURE S9(7)V9(2).
               10  TOTBEL-IO.
                   15  TOTBEL              PICTURE S9(7)V9(2).
               10  ORDKEY                  PICTURE X(9).
               10  FMOUT-IO.
                   15  FMOUT               PICTURE S9(6).
               10  FMORD-IO.
                   15  FMORD               PICTURE S9(6).
               10  FMOKOR-IO.
                   15  FMOKOR              PICTURE S9(6).
               10  FMOKO2-IO.
                   15  FMOKO2              PICTURE S9(8).
               10  FMOPRO-IO.
                   15  FMOPRO              PICTURE S9(2)V9(1).
               10  FMVPRO-IO.
                   15  FMVPRO              PICTURE S9(2)V9(1).
               10  FMVNPR-IO.
                   15  FMVNPR              PICTURE S9(2)V9(1).
               10  PSRDTO                  PICTURE X(8).
               10  AVSKEY                  PICTURE X(3).
               10  AVSGB                   PICTURE X(110).
               10  AVSGC                   PICTURE X(110).
           05  EDITTING-FIELDS.
               10  XO-70D                  PICTURE S9(7).
               10  XO-70U                  PICTURE 9(7).
               10  XO-72P-EF.
                 15  XO-72P                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-50D                  PICTURE S9(5).
               10  XO-50U                  PICTURE 9(5).
               10  XO-52P-EF.
                 15  XO-52P                PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-21P-EF.
                 15  XO-21P                PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  XO-40YNZ                PICTURE ZZZZ.
               10  XO-20YY9                PICTURE Z9.
               10  XO-50YNZ                PICTURE ZZZZZ.
               10  EDIT-ANTB2              PICTURE ZZZ,99.
               10  EDIT-ANTB3              PICTURE ZZZZZ,99.
               10  EDIT-ANTR2              PICTURE ZZZ,99.
               10  EDIT-ANTR3              PICTURE ZZZZZ,99.
               10  XO-72YN9                PICTURE ZZZZZZZ,99.
               10  XO-52YN9                PICTURE ZZZZZ,99.
               10  XO-40YN9                PICTURE ZZZ9.
               10  XO-70P-EF.
                 15  XO-70P                PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  XO-50P-EF.
                 15  XO-50P                PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  XO-60P-EF.
                 15  XO-60P                PICTURE S9(6) USAGE
                                                       PACKED-DECIMAL.
               10  XO-52D                  PICTURE S9(5)V9(2).
               10  XO-52U                  PICTURE 9(5)V9(2).
               10  XO-72D                  PICTURE S9(7)V9(2).
               10  XO-72U                  PICTURE 9(7)V9(2).
               10  XO-21D                  PICTURE S9(2)V9(1).
               10  XO-21U                  PICTURE 9(2)V9(1).
               10  XO-14P-EF.
                 15  XO-14P                PICTURE S9(1)V9(4) USAGE
                                                       PACKED-DECIMAL.
               10  XO-72YY9                PICTURE Z.ZZZ.ZZZ,99.
               10  XO-52YY9                PICTURE ZZ.ZZZ,99.
               10  XO-72YYZR               PICTURE Z.ZZZ.ZZZ,ZZ-.
               10  XO-60YY9                PICTURE ZZZ.ZZ9.
               10  XO-21YY9                PICTURE ZZ,9.
               10  XO-82YY9                PICTURE ZZ.ZZZ.ZZZ,99.
               10  XO-72YY9R               PICTURE Z.ZZZ.ZZZ,99-.
               10  XO-82YY9R               PICTURE ZZ.ZZZ.ZZZ,99-.
               10  XO-92YY9                PICTURE ZZZ.ZZZ.ZZZ,99.
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
           SET NOT-I-08                    TO TRUE
           SET NOT-I-08                    TO TRUE
           SET NOT-I-08                    TO TRUE
           SET NOT-I-08                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  ORDREFS-PROCESS
               SET ORDREFS-PROCESS-OFF     TO TRUE
               SET ORDREFS-READ            TO TRUE
           END-IF
 
           IF  ORDREFS-READ
           AND RECORD-SELECTED-OFF
               PERFORM ORDREFS-GET
               SET ORDREFS-READ-OFF        TO TRUE
               IF  NOT ORDREFS-EOF
                   PERFORM ORDREFS-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET ORDREFS-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  ORDREFS-PROCESS
               PERFORM ORDREFS-IDSET
           END-IF
 
           IF  ORDREFS-PROCESS
               PERFORM ORDREFS-CHK-LEVEL
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
 
           IF  ORDREFS-PROCESS
               PERFORM ORDREFS-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  ORDREFS-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-04)
               SET NOT-I-41                TO TRUE
               IF  FIRMA = '963'
                   SET I-41                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND NOT-I-41)
               SET NOT-I-41                TO TRUE
               IF  FIRMA = '970'
                   SET I-41                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND NOT-I-41)
               SET NOT-I-41                TO TRUE
               IF  FIRMA = '978'
                   SET I-41                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND NOT-I-41)
               SET NOT-I-41                TO TRUE
               IF  FIRMA = '913'
                   SET I-41                TO TRUE
               END-IF
      *
           END-IF
           IF  (I-L2 AND NOT-I-10)
               PERFORM FAPRUT-S
           END-IF
           IF  (I-L2 AND NOT-I-10)
               SET I-10                    TO TRUE
           END-IF
           IF  (I-L2)
               PERFORM FIRRUT-S
           END-IF
           IF  (I-L2)
               SET NOT-I-93                TO TRUE
               SUBTRACT FMVLI              FROM FMVLI
               SUBTRACT FMVLIK             FROM FMVLIK
               SUBTRACT FMVNUL             FROM FMVNUL
               SUBTRACT FMVLIO             FROM FMVLIO
               SUBTRACT FMOSUM             FROM FMOSUM
           END-IF
           IF  (I-90)
               SET NOT-I-91                TO TRUE
           END-IF
           IF  (I-L2)
               SET I-91                    TO TRUE
           END-IF
           IF  (I-84)
               SET NOT-I-85                TO TRUE
           END-IF
           SET NOT-I-33                    TO TRUE
           SET NOT-I-39                    TO TRUE
           SET NOT-I-42                    TO TRUE
           SET NOT-I-50                    TO TRUE
           SET NOT-I-52                    TO TRUE
           SET NOT-I-83                    TO TRUE
           SET NOT-I-84                    TO TRUE
           SET NOT-I-86                    TO TRUE
           SET NOT-I-87                    TO TRUE
           IF  (I-L1)
               SET NOT-I-53                TO TRUE
               SET NOT-I-36                TO TRUE
               SET NOT-I-44                TO TRUE
               SET NOT-I-90                TO TRUE
               SET NOT-I-92                TO TRUE
               SET NOT-I-55                TO TRUE
               SET NOT-I-57                TO TRUE
               SET NOT-I-49                TO TRUE
               SET I-51                    TO TRUE
               SET I-85                    TO TRUE
               SUBTRACT NETSUM             FROM NETSUM
               SUBTRACT NETANT             FROM NETANT
           END-IF
           IF  (I-03)
               SET I-53                    TO TRUE
           END-IF
           IF  (I-04 AND I-51)
               SET I-52                    TO TRUE
           END-IF
           IF  (I-04)
               SET NOT-I-51                TO TRUE
           END-IF
           IF  (I-01)
               PERFORM INRUT1-S
           END-IF
           IF  (I-04)
               PERFORM INRUT4-S
           END-IF
           IF  (I-L1)
               PERFORM REORG-S
           END-IF
           IF  (I-01 AND I-21)
               PERFORM SKOPI-S
           END-IF
           IF  (I-01 AND I-15)
               PERFORM FM1RUT-S
           END-IF
           IF  (I-04)
               PERFORM OSURUT-S
           END-IF
           IF  (I-04 AND I-21)
               PERFORM SKOPI-S
           END-IF
           IF  (I-04 AND I-31)
               PERFORM SERVI1-S
           END-IF
           IF  (I-04 AND I-15 AND NOT-I-12)
               PERFORM RORRUT-S
           END-IF
           IF  (I-04 AND NOT-I-15 AND NOT-I-12)
               AND (I-18)
               PERFORM RORRUT-S
           END-IF
           IF  (I-04 AND I-31)
               PERFORM AVIRUT-S
           END-IF
           IF  (I-04 AND I-31)
               PERFORM AVSRUT-S
           END-IF
           IF  (I-01 AND I-11 AND I-35)
               PERFORM FAKRUT-S
           END-IF
           IF  (I-04 AND I-31)
               OR  (I-04 AND I-15)
               PERFORM BEHRUT-S
           END-IF
           IF  (I-04 AND I-15 AND I-17)
               OR  (I-04 AND I-15 AND I-41)
               PERFORM VKARUT-S
           END-IF
           IF  (I-04 AND I-90)
               PERFORM FM4RUT-S
           END-IF
           IF  (I-04 AND I-95 AND I-15)
               PERFORM VEKRUT-S
           END-IF.
 
       FIRRUT-S SECTION.
       FIRRUT-S-P.
           MOVE FIRMA                      TO FIRMAF-KEY1
           READ FIRMAF RECORD KEY IS FIRMAF-KEY1
           INVALID KEY
               SET I-80                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-80                TO TRUE
               PERFORM FIRMAF-FLDSET
               PERFORM FIRMAF-IDSET
           END-READ
      * FIRMA SOM HENTER ORDRE TIL EGET DATASYSTEM                    *
           SET NOT-I-75                    TO TRUE
           IF  BRTYPE = 'E'
               SET I-75                    TO TRUE
           END-IF
           SET NOT-I-95                    TO TRUE
           IF  FIRMA = '950'
               SET I-95                    TO TRUE
           END-IF
           SET NOT-I-88                    TO TRUE
           IF  FMRUT = '1'
               SET I-88                    TO TRUE
           END-IF
           SET NOT-I-89                    TO TRUE
           IF  FMRUT = '2'
               SET I-89                    TO TRUE
           END-IF
           PERFORM VARYING ARK-I FROM 1 BY 1
                     UNTIL ARK-I > ARK-MAX
               MOVE 0                      TO ARK (ARK-I)
           END-PERFORM
           SET ARK-I                       TO 1
           PERFORM VARYING ARO-I FROM 1 BY 1
                     UNTIL ARO-I > ARO-MAX
               MOVE 0                      TO ARO (ARO-I)
           END-PERFORM
           SET ARO-I                       TO 1
           PERFORM VARYING AOD-I FROM 1 BY 1
                     UNTIL AOD-I > AOD-MAX
               MOVE 0                      TO AOD (AOD-I)
           END-PERFORM
           SET AOD-I                       TO 1
           PERFORM VARYING AOK-I FROM 1 BY 1
                     UNTIL AOK-I > AOK-MAX
               MOVE 0                      TO AOK (AOK-I)
           END-PERFORM
           SET AOK-I                       TO 1
           PERFORM VARYING ALD-I FROM 1 BY 1
                     UNTIL ALD-I > ALD-MAX
               MOVE 0                      TO ALD (ALD-I)
           END-PERFORM
           SET ALD-I                       TO 1
           PERFORM VARYING ALK-I FROM 1 BY 1
                     UNTIL ALK-I > ALK-MAX
               MOVE 0                      TO ALK (ALK-I)
           END-PERFORM
           SET ALK-I                       TO 1
           PERFORM VARYING ALR-I FROM 1 BY 1
                     UNTIL ALR-I > ALR-MAX
               MOVE 0                      TO ALR (ALR-I)
           END-PERFORM
           SET ALR-I                       TO 1
           PERFORM VARYING TOB-I FROM 1 BY 1
                     UNTIL TOB-I > TOB-MAX
               MOVE 0                      TO TOB (TOB-I)
           END-PERFORM
           SET TOB-I                       TO 1
           PERFORM VARYING TKB-I FROM 1 BY 1
                     UNTIL TKB-I > TKB-MAX
               MOVE 0                      TO TKB (TKB-I)
           END-PERFORM
           SET TKB-I                       TO 1
           PERFORM VARYING TRB-I FROM 1 BY 1
                     UNTIL TRB-I > TRB-MAX
               MOVE 0                      TO TRB (TRB-I)
           END-PERFORM
           SET TRB-I                       TO 1
           PERFORM VARYING TRO-I FROM 1 BY 1
                     UNTIL TRO-I > TRO-MAX
               MOVE 0                      TO TRO (TRO-I)
           END-PERFORM
           SET TRO-I                       TO 1
           PERFORM VARYING TRK-I FROM 1 BY 1
                     UNTIL TRK-I > TRK-MAX
               MOVE 0                      TO TRK (TRK-I)
           END-PERFORM
           SET TRK-I                       TO 1.
      *****************************************************************
      *  SUBRUTINE FOR SETTING AV FELLES INDIKATORER PR. ORDRE.       *
      *****************************************************************
 
       INRUT1-S SECTION.
       INRUT1-S-P.
           SET NOT-I-12                    TO TRUE
           SET NOT-I-15                    TO TRUE
           SET NOT-I-31                    TO TRUE
      *                    SETOF                     37
           SET NOT-I-47                    TO TRUE
           SET NOT-I-12                    TO TRUE
           IF  STATUS-X = 'U'
               SET I-12                    TO TRUE
           END-IF
           IF  (NOT-I-12)
               SET NOT-I-12                TO TRUE
               IF  FSLETT = 'S'
                   SET I-12                TO TRUE
               END-IF
           END-IF
           SET NOT-I-13                    TO TRUE
           IF  STATUS-X = 'C'
               SET I-13                    TO TRUE
           END-IF
           SET NOT-I-14                    TO TRUE
           IF  KONFAK = 'F'
               SET I-14                    TO TRUE
           END-IF
           IF  (NOT-I-14)
               SET NOT-I-14                TO TRUE
               IF  FAKTNR = PFFAKO
                   SET I-14                TO TRUE
               END-IF
           END-IF
           SET NOT-I-15                    TO TRUE
           IF  FERDIM = '*'
               SET I-15                    TO TRUE
           END-IF
           SET NOT-I-17                    TO TRUE
           IF  STATUS-X = 'V'
               SET I-17                    TO TRUE
           END-IF
           IF  (NOT-I-17)
               SET NOT-I-17                TO TRUE
               IF  BK = 'V'
                   SET I-17                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-17)
               SET NOT-I-17                TO TRUE
               IF  BK = 'W'
                   SET I-17                TO TRUE
               END-IF
           END-IF
           SET NOT-I-18                    TO TRUE
           IF  STATUS-X = 'X'
               SET I-18                    TO TRUE
           END-IF
           IF  (NOT-I-18)
               SET NOT-I-18                TO TRUE
               IF  BK = 'R'
                   SET I-18                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-18)
               SET NOT-I-18                TO TRUE
               IF  BK = 'F'
                   SET I-18                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-18)
               SET NOT-I-18                TO TRUE
               IF  BK = 'T'
                   SET I-18                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-18)
               SET NOT-I-18                TO TRUE
               IF  BK = 'O'
                   SET I-18                TO TRUE
               END-IF
           END-IF
           SET NOT-I-07                    TO TRUE
           IF  STATUS-X = 'F'
               SET I-07                    TO TRUE
           END-IF
      *RN07      NYFSAL    COMP "J"                      07=NY FORHÅNDSORDRE.
      *RN07      NYFSAL    COMP "X"                      07=NY FORHÅNDSORDRE.
           IF  (I-18 AND I-07)
               SET NOT-I-18                TO TRUE
           END-IF
           SET NOT-I-46                    TO TRUE
           IF  BK = 'P'
               SET I-46                    TO TRUE
           END-IF
           SET NOT-I-77                    TO TRUE
           IF  BK = 'I'
               SET I-77                    TO TRUE
           END-IF
           IF  (NOT-I-77)
               SET NOT-I-77                TO TRUE
               IF  BK = 'J'
                   SET I-77                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-77)
               SET NOT-I-77                TO TRUE
               IF  BK = 'M'
                   SET I-77                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-77)
               SET NOT-I-77                TO TRUE
               IF  BK = 'G'
                   SET I-77                TO TRUE
               END-IF
           END-IF
           SET NOT-I-76                    TO TRUE
           IF  BK = 'B'
               SET I-76                    TO TRUE
           END-IF
           IF  (NOT-I-76)
               SET NOT-I-76                TO TRUE
               IF  STATUS-X = 'B'
                   SET I-76                TO TRUE
               END-IF
           END-IF
           MOVE '        '                 TO TYPTEK (1:8)
           MOVE '        '                 TO TYPTEK (9:8)
           SET NOT-I-43                    TO TRUE
           IF  SKAF = 'P'
               SET I-43                    TO TRUE
           END-IF
           IF  (I-43)
               MOVE 'PLUKKORD'             TO TYPTEK (1:8)
               MOVE 'RE      '             TO TYPTEK (9:8)
           END-IF
           SET NOT-I-43                    TO TRUE
           IF  SKAF = 'V'
               SET I-43                    TO TRUE
           END-IF
           IF  (I-43)
               MOVE 'VENTEORD'             TO TYPTEK (1:8)
               MOVE 'RE      '             TO TYPTEK (9:8)
           END-IF
           SET NOT-I-43                    TO TRUE
           IF  SKAF = 'P'
               SET I-43                    TO TRUE
           END-IF
           IF  (NOT-I-43)
               SET NOT-I-43                TO TRUE
               IF  SKAF = 'V'
                   SET I-43                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-43)
               SET NOT-I-47                TO TRUE
               IF  STATUS-X = 'J'
                   SET I-47                TO TRUE
               END-IF
           END-IF
           IF  (I-47)
               MOVE 'IKKE FUL'             TO TYPTEK (1:8)
               MOVE 'LFØRT   '             TO TYPTEK (9:8)
           END-IF
           SET NOT-I-47                    TO TRUE
           IF  (NOT-I-43)
               SET NOT-I-47                TO TRUE
               IF  STATUS-X = 'H'
                   SET I-47                TO TRUE
               END-IF
           END-IF
           IF  (I-47)
               MOVE 'SAMLEORD'             TO TYPTEK (1:8)
               MOVE 'RE IF.  '             TO TYPTEK (9:8)
           END-IF
           SET NOT-I-43                    TO TRUE
           IF  (NOT-I-43)
               SET NOT-I-47                TO TRUE
               IF  STATUS-X = 'I'
                   SET I-47                TO TRUE
               END-IF
           END-IF
           IF  (I-47)
               MOVE 'VENTER G'             TO TYPTEK (1:8)
               MOVE 'ODKJENN.'             TO TYPTEK (9:8)
           END-IF
           SET NOT-I-43                    TO TRUE
           SET NOT-I-47                    TO TRUE
           IF  STATUS-X = 'J'
               SET I-47                    TO TRUE
           END-IF
           SET NOT-I-48                    TO TRUE
           IF  STATUS-X = 'R'
               SET I-48                    TO TRUE
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  STATUS-X = 'S'
               SET I-81                    TO TRUE
           END-IF
           SET NOT-I-35                    TO TRUE
           IF  STATUS-X = 'M'
               SET I-35                    TO TRUE
           END-IF
           IF  (NOT-I-35)
               SET NOT-I-35                TO TRUE
               IF  STATUS-X = 'A'
                   SET I-35                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-35)
               SET NOT-I-35                TO TRUE
               IF  STATUS-X = 'C'
                   SET I-35                TO TRUE
               END-IF
      *          FAKTNR    COMP "FA"                     37=FORD SENDES-EDI
      * N37      FAKTNR    COMP "FB"                     37=FORD SENDT-EDI
           END-IF
           SET NOT-I-23                    TO TRUE
           IF  RUTID = 'K'
               SET I-23                    TO TRUE
           END-IF
           SET NOT-I-43                    TO TRUE
           IF  RUTID = 'S'
               SET I-43                    TO TRUE
           END-IF
           SET NOT-I-16                    TO TRUE
           IF  RUTID = 'L'
               SET I-16                    TO TRUE
           END-IF
           IF  (NOT-I-16)
               SET NOT-I-16                TO TRUE
               IF  STATUS-X = 'L'
                   SET I-16                TO TRUE
               END-IF
           END-IF
           IF  (I-16 AND I-35)
               SET I-15                    TO TRUE
           END-IF
           IF  (I-16 AND I-43)
               SET I-15                    TO TRUE
           END-IF
           SET NOT-I-21                    TO TRUE
           IF  SELGKP = '*'
               SET I-21                    TO TRUE
           END-IF
           IF  (I-21)
               SET I-31                    TO TRUE
           END-IF
           IF  (I-75 AND NOT-I-31)
               SET I-12                    TO TRUE
           END-IF
           IF  (I-23)
               SET NOT-I-21                TO TRUE
           END-IF
           IF  (I-43)
               SET NOT-I-21                TO TRUE
           END-IF
           IF  (I-95 AND NOT-I-76)
               SET NOT-I-21                TO TRUE
           END-IF
           SET NOT-I-22                    TO TRUE
           IF  DIRREG = 'J'
               SET I-22                    TO TRUE
           END-IF
           SET NOT-I-58                    TO TRUE
           IF  KOMUTA = 'J'
               SET I-58                    TO TRUE
           END-IF
           SET NOT-I-79                    TO TRUE
           IF  BETM = '14'
               SET I-79                    TO TRUE
           END-IF
           SET NOT-I-78                    TO TRUE
           IF  BETM = '07'
               SET I-78                    TO TRUE
           END-IF
           IF  (NOT-I-78)
               SET NOT-I-78                TO TRUE
               IF  BETM = '47'
                   SET I-78                TO TRUE
               END-IF
           END-IF
           SET NOT-I-97                    TO TRUE
           IF  FRITT = '1'
               SET I-97                    TO TRUE
           END-IF
           SET NOT-I-30                    TO TRUE
           IF  ANTPRT > 1
               SET I-30                    TO TRUE
           END-IF
           MOVE AVD                        TO NYAVD
           SET NOT-I-34                    TO TRUE
           IF  NYAVD = '0'
               SET I-34                    TO TRUE
           END-IF
           IF  (NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  NYAVD = '1'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  NYAVD = '2'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  NYAVD = '3'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  NYAVD = '4'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  NYAVD = '5'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  NYAVD = '6'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  NYAVD = '7'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  NYAVD = '8'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  NYAVD = '9'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  NYAVD = 'A'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  NYAVD = 'B'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  NYAVD = 'C'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-34)
               MOVE '1'                    TO NYAVD
           END-IF
           MOVE 0                          TO X
           SET NOT-I-34                    TO TRUE
           IF  NYAVD = 'A'
               SET I-34                    TO TRUE
           END-IF
           IF  (NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  NYAVD = 'B'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  NYAVD = 'C'
                   SET I-34                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-34)
               MOVE NYAVD                  TO X-IO (2:1)
           END-IF
           SET NOT-I-34                    TO TRUE
           IF  NYAVD = 'A'
               SET I-34                    TO TRUE
           END-IF
           IF  (I-34)
               MOVE 10                     TO X-IO
           END-IF
           SET NOT-I-34                    TO TRUE
           IF  NYAVD = 'B'
               SET I-34                    TO TRUE
           END-IF
           IF  (I-34)
               MOVE 11                     TO X-IO
           END-IF
           SET NOT-I-34                    TO TRUE
           IF  NYAVD = 'C'
               SET I-34                    TO TRUE
           END-IF
           IF  (I-34)
               MOVE 12                     TO X-IO
           END-IF
           ADD 1                           TO X
           SET NOT-I-34                    TO TRUE
      ***** RUTINE FOR  ORDRE OG AVIKSLISTE PR ORDREMOTAGER. *****
           SET NOT-I-40                    TO TRUE
      ***** RUTINE FOR Å SNU ORDREDATO TIL ÅR MND DAG OG 4 SIFFERET ÅR
           MOVE ORDATO                     TO A-ELGMD
           MOVE ORDAAR                     TO A-ELGMD (1:2)
           MOVE ORDDAG                     TO A-ELGMD (5:2)
           MOVE A-ELGMD                    TO ORDAT3-IO
      *
           MOVE 'A'                        TO DATOK
           MOVE ORDATO                     TO DATO6
           CALL 'DATO8SIF' USING DATOER-XX-DATA-FIELDS
           MOVE AMD8                       TO ORDAT2.
      *****************************************************************
      *  SUBRUTINE FOR SETTING AV FELLES INDIKATORER PR. VARELINJE.   *
      *****************************************************************
 
       INRUT4-S SECTION.
       INRUT4-S-P.
           SET NOT-I-24                    TO TRUE
           IF  EDBNR = 0000000
               SET I-24                    TO TRUE
           END-IF
           SET NOT-I-25                    TO TRUE
           IF  PRITYP = 'N'
               SET I-25                    TO TRUE
           END-IF
           IF  (NOT-I-25)
               SET NOT-I-25                TO TRUE
               IF  PRITYP = 'T'
                   SET I-25                TO TRUE
               END-IF
           END-IF
           SET NOT-I-26                    TO TRUE
           IF  ORPRIS NOT = RGPRIS
               SET I-26                    TO TRUE
           END-IF
           SET NOT-I-28                    TO TRUE
           IF  ANTLEV > 0,00
               SET I-28                    TO TRUE
           END-IF
           SET NOT-I-19                    TO TRUE
           IF  EDBNR > 9000000
               SET I-19                    TO TRUE
           END-IF
           SET NOT-I-59                    TO TRUE
           IF  PRITIL > 0,00
               SET I-59                    TO TRUE
           END-IF
           SET NOT-I-82                    TO TRUE
           IF  ANTBES > 0,00
               SET I-82                    TO TRUE
           END-IF.
      *****************************************************************
      *  SUBRUTINE FOR REORGANISERING AV ORDREMASTER.                 *
      *  FØLGENDE ORDRE SKAL IKKE LEGGES UT PÅ NY ORDREMASTER.        *
      *  1. UTGÅRMELDTE ORDRE. (STATUS = U)                           *
      *  2. FERDIG FAKTURERTE KONTANT.FAKTURA. STATUS=C, KONFAK=F     *
      *  2. FERDIG FAKTURERTE ORDRE. (STATUS = C + FAKTURAOMG.NR =    *
      *                              SISTE FULLFØRTE FAKTURAOMG.NR.   *
      *  3. FERDIGMELDTE LAGEROVERFØRINGER (STATUS = L + * I FERDIGM. *
      *  4. FERDIGMELDTE VERKSTEDSORDRE (STATUS = V + * I FERDIGMELDT.*
      *  4A BK = "V" ELLER "W" BEHANDLES SOM VERKSTEDORDRE HER        *
      *  5. FERDIGMELDTE REST-REGISTR.  (STATUS = X + * I FERDIGMELDT.*
      *  6. FERDIGMELDTE RETUR LEVR.         BK = B + * I FERDIGMELDT.*
      *****************************************************************
 
       REORG-S SECTION.
       REORG-S-P.
           SET NOT-I-11                    TO TRUE
           IF  (I-12)
               GO TO ENDREO-T
           END-IF
           IF  (I-13 AND I-14)
               GO TO ENDREO-T
           END-IF
           IF  (I-15 AND I-16)
               GO TO ENDREO-T
           END-IF
           IF  (I-15 AND I-17)
               GO TO ENDREO-T
           END-IF
           IF  (I-18)
               GO TO ENDREO-T
           END-IF
           IF  (I-15 AND I-76)
               GO TO ENDREO-T
           END-IF
           SET I-11                        TO TRUE
           IF  (I-30)
               MOVE 1                      TO ANTPRX
           END-IF.
 
       ENDREO-T.
           CONTINUE.
      *****************************************************************
      *                SUBRUTINE FOR ORDRESUMMERING.                  *
      *****************************************************************
 
       OSURUT-S SECTION.
       OSURUT-S-P.
      ***********  SUMMERING AV ANTALL ORDRE/VARELINJER PR.FIRMA DENNE MND.
           IF  (I-31 AND I-52)
               ADD 1                       TO FAORDM
           END-IF
           IF  (I-31 AND I-52 AND I-22)
               ADD 1                       TO FADODM
           END-IF
           IF  (I-31 AND NOT-I-24)
               ADD 1                       TO FAVLDM
           END-IF
           IF  (I-24)
               GO TO ENDOSU-T
      ****** SUMMERING AV ORDRELINJE TIL ORDRE PRIS/RABATT.
           END-IF
           IF  (I-12)
               SUBTRACT ANTRES FROM ANTBES GIVING ANTUTG
      *  SNU BELØP DERSOM KREDIT-GEBYR ELLER ANNEN RETUR.
           END-IF
           DIVIDE EDBNR BY 10000       GIVING EDBNR3
           DIVIDE EDBNR BY 100000      GIVING EDBNR2
           SET NOT-I-98                    TO TRUE
           IF  EDBNR2 = 94
               SET I-98                    TO TRUE
           END-IF
           IF  (NOT-I-98)
               SET NOT-I-98                TO TRUE
               IF  EDBNR3 = 995
                   SET I-98                TO TRUE
               END-IF
           END-IF
           IF  (I-82)
               MULTIPLY ANTBES BY ORPRIS GIVING NETTO ROUNDED
           END-IF
           IF  (NOT-I-82)
               ADD ORPRIS TO ZERO      GIVING NETTO
           END-IF
           MULTIPLY ORRAB1 BY NETTO    GIVING SUM1
           DIVIDE SUM1 BY 100          GIVING RABBEL ROUNDED
           SUBTRACT RABBEL                 FROM NETTO
           MULTIPLY ORRAB2 BY NETTO    GIVING SUM1
           DIVIDE SUM1 BY 100          GIVING RABBEL ROUNDED
           SUBTRACT RABBEL                 FROM NETTO
           MULTIPLY ORRAB3 BY NETTO    GIVING SUM1
           DIVIDE SUM1 BY 100          GIVING RABBEL ROUNDED
           SUBTRACT RABBEL                 FROM NETTO
           IF  (I-98)
               MULTIPLY -1 BY NETTO    GIVING NETTO
           END-IF
           IF  (I-82)
               DIVIDE NETTO BY ANTBES  GIVING NETSTK ROUNDED
           END-IF
           IF  (NOT-I-82)
               ADD NETTO TO ZERO       GIVING NETSTK
           END-IF
           IF  (NOT-I-12)
               MULTIPLY ANTLEV BY NETSTK GIVING NETPRI ROUNDED
           END-IF
           IF  (I-12)
               MULTIPLY ANTUTG BY NETSTK GIVING NETPRI ROUNDED
           END-IF
           MULTIPLY ANTBES BY NETSTK   GIVING BSTPRI ROUNDED
           IF  (NOT-I-23)
               ADD NETPRI                  TO NETSUM
           END-IF
           IF  (I-23)
               SUBTRACT NETPRI             FROM NETSUM
           END-IF
           ADD ANTLEV                      TO NETANT
           SUBTRACT ANTLEV FROM ANTBES GIVING RESANT
           SET NOT-I-32                    TO TRUE
           IF  RESANT > 0
               SET I-32                    TO TRUE
           END-IF
           MULTIPLY RESANT BY NETSTK   GIVING RESBEL ROUNDED
      ****** LEGG TIL NETTO PRISTILLEGG (FRAKTTILLEGG/PANT)  *****
           IF  (I-59)
               MULTIPLY ANTBES BY PRITIL GIVING NETTO ROUNDED
               ADD NETTO                   TO BSTPRI
           END-IF
           IF  (I-59 AND NOT-I-12)
               MULTIPLY ANTLEV BY PRITIL GIVING NETTO ROUNDED
           END-IF
           IF  (I-59 AND I-12)
               MULTIPLY ANTUTG BY PRITIL GIVING NETTO ROUNDED
           END-IF
           IF  (I-59 AND NOT-I-23)
               ADD NETTO TO ZERO       GIVING PRITUT
           END-IF
           IF  (I-59 AND I-23)
               SUBTRACT NETTO FROM ZERO GIVING PRITUT
           END-IF
           IF  (I-59)
               ADD PRITUT                  TO NETSUM
               MULTIPLY RESANT BY PRITIL GIVING NETTO ROUNDED
           END-IF
           IF  (I-59 AND NOT-I-23)
               ADD NETTO TO ZERO       GIVING NETTO
           END-IF
           IF  (I-59 AND I-23)
               SUBTRACT NETTO FROM ZERO GIVING NETTO
           END-IF
           IF  (I-59)
               ADD NETTO                   TO RESBEL
      *****************************************************************
      * SUMMERING AV ORDRELINJER TIL REGISTER PRIS/RABATT             *
      * REGISTERPRISEN I ORDREFILE ER IBEREGNET NETTOFAKTOR/PÅSLAG.   *
      *****************************************************************
           END-IF
           IF  (NOT-I-31)
               GO TO ENDOSU-T
           END-IF
           IF  (I-82)
               MULTIPLY ANTBES BY RGPRIS GIVING NETTO ROUNDED
           END-IF
           IF  (NOT-I-82)
               ADD RGPRIS TO ZERO      GIVING NETTO
           END-IF
           MULTIPLY RGRAB1 BY NETTO    GIVING SUM1 ROUNDED
           DIVIDE SUM1 BY 100          GIVING RABBEL ROUNDED
           SUBTRACT RABBEL                 FROM NETTO
           MULTIPLY RGRAB2 BY NETTO    GIVING SUM1 ROUNDED
           DIVIDE SUM1 BY 100          GIVING RABBEL ROUNDED
           SUBTRACT RABBEL                 FROM NETTO
           MULTIPLY RGRAB3 BY NETTO    GIVING SUM1 ROUNDED
           DIVIDE SUM1 BY 100          GIVING RABBEL ROUNDED
           SUBTRACT RABBEL                 FROM NETTO
           IF  (I-98)
               MULTIPLY -1 BY NETTO    GIVING NETTO
           END-IF
           MOVE NETTO                      TO REGPRI-IO
           IF  (I-59)
               MULTIPLY ANTBES BY PRITIL GIVING NETTO ROUNDED
               ADD NETTO                   TO REGPRI
           END-IF.
 
       ENDOSU-T.
           CONTINUE.
      *****************************************************************
      *     SUBRUTINE FOR Å DANNE SERVICEPROSENT PR. VGR. RECORDS.    *
      *  FØLGENDE RECORDS SELEKTERES:                                 *
      *     DAGENS FULLFØRTE VARERECORDS FRA ORDRE OG SALG.           *
      *     IKKE KREDIT, IKKE TEKSTLINJER.                            *
      *     IKKE UTGÅRMELDTE ORDRE.                                   * 000523
      *     IKKE FORHÅNDSORDRE.                                       *
      *     IKKE FORHÅNDSALG/REGISTRERING TIL RESTORDREREGISTER.      *
      *     VERKSTEDSUTAK.                                            *
      *     KUN FIRMA 915,918,950,963,912,604,615,634,717,738         *
      *****************************************************************
 
       SERVI1-S SECTION.
       SERVI1-S-P.
           SET NOT-I-40                    TO TRUE
           IF  FIRMA = '915'
               SET I-40                    TO TRUE
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  FIRMA = '918'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  FIRMA = '963'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  FIRMA = '604'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  FIRMA = '615'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  FIRMA = '634'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  FIRMA = '717'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  FIRMA = '738'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  FIRMA = '912'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               GO TO ENDSV1-T
           END-IF
           IF  (I-12)
               GO TO ENDSV1-T
           END-IF
           IF  (I-16)
               GO TO ENDSV1-T
           END-IF
           IF  (I-17)
               GO TO ENDSV1-T
           END-IF
           IF  (I-18)
               GO TO ENDSV1-T
           END-IF
           IF  (I-07)
               GO TO ENDSV1-T
           END-IF
           IF  (I-23)
               GO TO ENDSV1-T
           END-IF
           IF  (I-76)
               GO TO ENDSV1-T
           END-IF
           IF  (I-24)
               GO TO ENDSV1-T
           END-IF
           SET I-83                        TO TRUE
           IF  (I-82)
               MOVE 1                      TO ANTB
           END-IF
           IF  (I-32)
               MOVE 1                      TO ANTR
           END-IF
           ADD ANTBES TO ZERO          GIVING ANTB50
           ADD RESANT TO ZERO          GIVING ANTR50
           MOVE VGR                        TO VGR-N
           MOVE VGR-N-IO (1:1)             TO VGRAVD.
 
       ENDSV1-T.
           CONTINUE.
      *****************************************************************
      *     SUBRUTINE FOR Å DANNE RESTORDRERECORDS.                   *
      *  FØLGENDE RECORDS SELEKTERES:                                 *
      *     DAGENS FERDIGMELDTE ORDRE HVOR ANTALL BESTILT ER ULIK     *
      *     ANTALL LEVERT. ELLER REN RESTORDRE-REGISTERT ORDRE.       *
      *     IKKE KREDITORDRE.                                         * 000560
      *     IKKE ORDRE MED KODE "1" I REST. (IKKE RESTORDRE)          *
      *     IKKE ORDRE MED BK = B   = RETUR TIL LEVERANDØR.           *
      *     IKKE ORDRE MED BK = S   = SERVICE ORDRE.                  *
      *     IKKE ORDRE MED KODE "P" I BK (PRODUKSJONSORDRE) SOM IKKE  *
      *                             HAR "2" I REST. (RESTORDRENOTERES)*
      *     IKKE ORDRE MED KODE "0" I KODE NULLREST.(FERDIGMELDT M/0) *
      *     IKKE ORDRE FOR FIRMA "999" SYSTEMTEST.                    *
      *     IKKE ORDRE FOR FIRMA MED "N" (NEI) I FIRMAFILE.           *
      *****************************************************************
 
       RORRUT-S SECTION.
       RORRUT-S-P.
           SET NOT-I-84                    TO TRUE
           SET NOT-I-40                    TO TRUE
           IF  REST = '2'
               SET I-40                    TO TRUE
           END-IF
           IF  (I-23)
               GO TO ENDROR-T
           END-IF
           IF  (I-46 AND NOT-I-40)
               GO TO ENDROR-T
           END-IF
           SET NOT-I-40                    TO TRUE
           IF  FIRMA = '000'
               SET I-40                    TO TRUE
           END-IF
           IF  (NOT-I-40 AND NOT-I-18)
               SET NOT-I-40                TO TRUE
               IF  FIREST = 'N'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40 AND NOT-I-18)
               SET NOT-I-40                TO TRUE
               IF  REST = '1'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  NOREST = '0'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-40)
               SET NOT-I-40                TO TRUE
               IF  BK = 'S'
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (I-40)
               GO TO ENDROR-T
           END-IF
           IF  (I-76)
               GO TO ENDROR-T
           END-IF
           IF  (NOT-I-82)
               GO TO ENDROR-T
           END-IF
           IF  (NOT-I-18 AND NOT-I-32)
               GO TO ENDROR-T
           END-IF
           IF  (I-24 AND NOT-I-18)
               GO TO ENDROR-T
           END-IF
           SET I-84                        TO TRUE
      ***** RUTINE FOR VAREADRESSE OG REKV.NR RECORDS  ******
           IF  (NOT-I-85)
               GO TO ENDROR-T
           END-IF
           IF  (I-53)
               SET NOT-I-86                TO TRUE
               IF  VAADR1 > '        '
                   SET I-86                TO TRUE
               END-IF
           END-IF
           IF  (I-53 AND NOT-I-86)
               SET NOT-I-86                TO TRUE
               IF  VAADR4 > '        '
                   SET I-86                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-86)
               SET NOT-I-86                TO TRUE
               IF  REKVNR > '        '
                   SET I-86                TO TRUE
               END-IF
           END-IF
           IF  (I-53)
               SET NOT-I-87                TO TRUE
               IF  VAADR2 > '        '
                   SET I-87                TO TRUE
               END-IF
           END-IF
           IF  (I-53 AND NOT-I-87)
               SET NOT-I-87                TO TRUE
               IF  VAADR3 > '        '
                   SET I-87                TO TRUE
               END-IF
           END-IF.
 
       ENDROR-T.
           CONTINUE.
      *****************************************************************
      *     SUBRUTINE FOR Å AVDEKKE PRIS OG RABATT AVIK.              *
      *  FØLGENDE RECORDS SELEKTERES:                                 *
      *     DAGENS FERDIGREGISTRETE ORDRE.                            *
      *     ORDRE PRIS/RABATT AVIKER FRA REGISTER PRIS/RABATT.        *
      *     ORDRE PRIS ER LAVERE ENN SELVKOSTPRIS. (IKKE KR.NOTA) 8/91*
      *     IKKE KR.NOTA TYPE 3. (FEIL PRIS/RABATT)                   *
      *     IKKE TEKSTLINJER.                                         *
      *     IKKE LAGEROVERFØRINGSORDRE.                               *
      *     RETUR LEVERANDØR. (SKAL VÆRE MED FRA 16.03.04) REF:SOGB.  *
      *     IKKE RETUR REST,FORHÅND OG TILBUDS REGISTRERING           *
      *     ALLTID VARELINJER MED 0 I PRIS.  (8/5-95)                 *
      *       SKAFFEVARER MED MERKNADSKODE "A" ORDREPRIS BLIR SATT INN*
      *          I REGISTERPRIS (CICSORRE) FÅR Å FORHINDRE PRISAVIK.  *
      *     ALLTID RESTORDRE-REGISTERING"S ORDRE.                     *
      *****************************************************************
 
       AVIRUT-S SECTION.
       AVIRUT-S-P.
           IF  (NOT-I-23)
               GO TO AVIORD-T
           END-IF
           SET NOT-I-33                    TO TRUE
           IF  KRTYPE = '3'
               SET I-33                    TO TRUE
           END-IF
           IF  (NOT-I-33)
               GO TO AVIORD-T
           END-IF
           SET NOT-I-33                    TO TRUE
           GO TO ENDAVI-T.
 
       AVIORD-T.
           IF  (I-16)
               GO TO ENDAVI-T
      *  76                GOTO ENDAVI                     =RETUR LEV.
           END-IF
           IF  (I-24)
               GO TO ENDAVI-T
           END-IF
           IF  (I-12)
               GO TO ENDAVI-T
           END-IF
           IF  (I-18)
               GO TO ENDAVI-T
           END-IF
           IF  (I-07)
               GO TO ENDAVI-T
      *****************************************************************
      * VARER MED REGISTRERPRIS 0,01 SKAL IKKE KOMME UT SOM PRISAVIK  *
      *       OM ORDREPRISEN = 0,00                                   *
      *****************************************************************
      *          RGPRIS    COMP 0,01                     39=GRATISVARER.
           END-IF
           SET NOT-I-33                    TO TRUE
           IF  ORPRIS = 0,00
               SET I-33                    TO TRUE
           END-IF
           IF  (NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  ORPRIS NOT = RGPRIS
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-33 AND NOT-I-23)
               SET NOT-I-33                TO TRUE
               IF  ORPRIS < KOSPRI
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  ORRAB1 NOT = RGRAB1
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  ORRAB2 NOT = RGRAB2
                   SET I-33                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-33)
               SET NOT-I-33                TO TRUE
               IF  ORRAB3 NOT = RGRAB3
                   SET I-33                TO TRUE
               END-IF
           END-IF.
 
       ENDAVI-T.
           CONTINUE.
      *****************************************************************
      *     SUBRUTINE FOR Å ORDRESUMMERING PR. AVDELING PR. FIRMA.    *
      *  FØLGENDE RECORDS SUMMERES.                                   *
      *     DAGENS FERDIGREGISTRETE ORDRE.                            *
      *     IKKE TEKSTLINJER.                                         *
      *     IKKE LAGEROVERFØRINGSORDRE.                               *
      *     IKKE RETUR LEVERANDØR.                                    *
      *     IKKE UTGÅRMELDTE ORDRE.                                   *
      *     IKKE FORHÅNDSALG/REGISTRERING TIL RESTORDREREGISTER.      *
      *****************************************************************
 
       AVSRUT-S SECTION.
       AVSRUT-S-P.
           IF  (I-16)
               GO TO ENDAVS-T
           END-IF
           IF  (I-76)
               GO TO ENDAVS-T
           END-IF
           IF  (I-12)
               GO TO ENDAVS-T
           END-IF
           IF  (I-18)
               GO TO ENDAVS-T
           END-IF
           IF  (I-17)
               GO TO ENDAVS-T
           END-IF
           IF  (I-52 AND NOT-I-23)
               ADD 1                       TO AOD (X)
           END-IF
           IF  (I-52 AND I-23)
               ADD 1                       TO AOK (X)
           END-IF
           IF  (I-24)
               GO TO ENDAVS-T
           END-IF
           IF  (I-78 AND NOT-I-23)
               ADD NETPRI                  TO ARK (X)
           END-IF
           IF  (I-78 AND I-23)
               SUBTRACT NETPRI             FROM ARK (X)
           END-IF
           IF  (I-78 AND I-59)
               ADD PRITUT                  TO ARK (X)
           END-IF
           IF  (I-79 AND NOT-I-23)
               ADD NETPRI                  TO ARO (X)
           END-IF
           IF  (I-79 AND I-23)
               SUBTRACT NETPRI             FROM ARO (X)
           END-IF
           IF  (I-79 AND I-59)
               ADD PRITUT                  TO ARK (X)
           END-IF
           IF  (NOT-I-23)
               ADD 1                       TO ALD (X)
           END-IF
           IF  (I-23)
               ADD 1                       TO ALK (X)
           END-IF
           IF  (NOT-I-23 AND I-32)
               ADD 1                       TO ALR (X)
           END-IF
           IF  (NOT-I-23)
               ADD BSTPRI                  TO TOB (X)
           END-IF
           IF  (I-23)
               ADD BSTPRI                  TO TKB (X)
           END-IF
           IF  (NOT-I-23 AND I-32)
               ADD RESBEL                  TO TRB (X)
           END-IF
           IF  (I-19)
               ADD BSTPRI TO ZERO      GIVING REGPRI
           END-IF
           IF  (NOT-I-33)
               ADD BSTPRI TO ZERO      GIVING REGPRI
           END-IF
           IF  (NOT-I-23)
               ADD REGPRI                  TO TRO (X)
           END-IF
           IF  (I-23)
               ADD REGPRI                  TO TRK (X)
           END-IF.
 
       ENDAVS-T.
           CONTINUE.
      *****************************************************************
      *     SUBRUTINE FOR ORDRE SOM LEGGES UT TIL FAKTURERING.        *
      *  FØLGENDE ORDRE GÅR TIL FAKTURERING.                          *
      *     FERDIGMELDTE ORDRE SOM SKAL LEGGES UT PÅ NY ORDREFILE.    *
      *     IKKE ORDRE SOM ER MERKET FAKTURERT (C) OG FAKTURERING HAR *
      *          FUNNET STED. (FAKTURAOMGANG = FAKTURAOMGANG FULLFØRT)*
      *     ORDREDATO =/< FAKTURADATO I FAKTURAPARAMETER.             *
      *     FAKTURAGRUPPE I FAKTURAPARAMETER = 0.                     *
      *     FAKTURAGRUPPE I FAKTURAPARAMETER = FAKTURAGRUPPE I FIRMAF.*
      *     SALG MERKET MED SAMLEFAKTURE SKAL KUN MED HVIS  DENNE     *
      *           FAKTURAOMGANG SKAL HA MED SAMLEFAKTURA.             *
      *           KR.NOTA SKAL ALLTID MED, DOG IKKE PÅ KONTANTKUNDENR.*
      *  4/2-2002 (KREDITNOTA SKAL NÅ FØLGE VANLIGE REGLER.)          *
      *     I FAKTURAPARAMETER LIGGER OGSÅ EN TEST PÅ FIRMA SOM IKKE  *
      *           SKAL HA FAKTURA VED DENNE FAKTURAOMGANG.            *
      *****************************************************************
 
       FAKRUT-S SECTION.
       FAKRUT-S-P.
      *                    GOTO ENDFAK                     =IKKE NY FAKT.OMG.
           IF  (I-76)
               GO TO ENDFAK-T
           END-IF
           IF  (I-18)
               GO TO ENDFAK-T
      *  37                GOTO ENDFAK                     =FORD FAKT. VENTER.
           END-IF
           SET NOT-I-20                    TO TRUE
           IF  FIRMA = IKKEF1
               SET I-20                    TO TRUE
           END-IF
           IF  (NOT-I-20)
               SET NOT-I-20                TO TRUE
               IF  FIRMA = IKKEF2
                   SET I-20                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-20)
               SET NOT-I-20                TO TRUE
               IF  FIRMA = IKKEF3
                   SET I-20                TO TRUE
               END-IF
           END-IF
           IF  (I-20)
               GO TO ENDFAK-T
           END-IF
           SET NOT-I-36                    TO TRUE
           IF  ORDAT2 NOT > PSRDTO
               SET I-36                    TO TRUE
           END-IF
           IF  (NOT-I-36)
               GO TO ENDFAK-T
           END-IF
           SET NOT-I-36                    TO TRUE
           IF  FFAKGR = PFAKGR
               SET I-36                    TO TRUE
           END-IF
           IF  (NOT-I-36)
               GO TO ENDFAK-T
           END-IF
           SET NOT-I-98                    TO TRUE
           IF  KUNDNR = '500190'
               SET I-98                    TO TRUE
           END-IF
           IF  (NOT-I-98)
               SET NOT-I-98                TO TRUE
               IF  KUNDNR = '500191'
                   SET I-98                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-98)
               SET NOT-I-98                TO TRUE
               IF  KUNDNR = '500192'
                   SET I-98                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-98)
               SET NOT-I-98                TO TRUE
               IF  KUNDNR = '500193'
                   SET I-98                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-98)
               SET NOT-I-98                TO TRUE
               IF  KUNDNR = '500194'
                   SET I-98                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-98)
               SET NOT-I-98                TO TRUE
               IF  KUNDNR = '500195'
                   SET I-98                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-98)
               SET NOT-I-98                TO TRUE
               IF  KUNDNR = '500196'
                   SET I-98                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-98)
               SET NOT-I-98                TO TRUE
               IF  KUNDNR = '500197'
                   SET I-98                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-98)
               SET NOT-I-98                TO TRUE
               IF  KUNDNR = '500198'
                   SET I-98                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-98)
               SET NOT-I-98                TO TRUE
               IF  KUNDNR = '500199'
                   SET I-98                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-98)
               SET NOT-I-98                TO TRUE
               IF  KUNDNR = '500200'
                   SET I-98                TO TRUE
               END-IF
      *  23N98             GOTO ENDFAK                      KR.NOTA (FJERNET)
           END-IF
           SET NOT-I-98                    TO TRUE
           IF  SAMFAK = '1'
               SET I-98                    TO TRUE
           END-IF
           IF  (NOT-I-98)
               SET NOT-I-98                TO TRUE
               IF  SAMFAK = '6'
                   SET I-98                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-98)
               SET NOT-I-98                TO TRUE
               IF  SAMFAK = '7'
                   SET I-98                TO TRUE
               END-IF
           END-IF
           IF  (I-98 AND NOT-I-96)
               SET NOT-I-36                TO TRUE
      *          ORDAT2    COMP "20020131"             9898=SKAL MED P.G.A FEIL
      *                    GOTO ENDFAK                      PARAM PR. 31.01.02
           END-IF
           SET NOT-I-98                    TO TRUE
           IF  SAMFAK = '8'
               SET I-98                    TO TRUE
           END-IF
           IF  (NOT-I-98)
               SET NOT-I-98                TO TRUE
               IF  SAMFAK = '9'
                   SET I-98                TO TRUE
               END-IF
           END-IF
           IF  (I-98 AND NOT-I-94)
               SET NOT-I-36                TO TRUE
           END-IF.
 
       ENDFAK-T.
           CONTINUE.
      *****************************************************************
      *     SUBRUTINE FOR DAGENS BEHOLDNINGSKORRIGERINGER             *
      *     FOR OPPDATERING AV MÅNEDENS VARESALG.                     *
      *     ----------------------------------------------            *
      *  1. ALLE DAGENS FERDIG-REGISTRERTE ORDRE.                     *
      *  2. ALLE DAGENS FERDIGMELDTE ORDRE HVOR ANT.LEV ER KORRIGERT. *
      *          DAGENS UTGÅRMELDTE ER ANT.LEV. SATT TIL 0.           *
      *  2. IKKE LAGEROVERFØRINGER.                                   *
      *  3. IKKE KREDITNOTA.                                          *
      *  4. IKKE REST-REGISTERING.                                    *
      *  5. IKKE TEKST LINJER.                                        *
      *  6. IKKE SKAFFEVARER.                                         *
      *  7. IKKE DAGENS ORDRE SOM ER UTGÅRMELDT.                      *
      *  8. IKKE RETUR LEVERANDØR.                                    *
      *****************************************************************
 
       BEHRUT-S SECTION.
       BEHRUT-S-P.
           IF  (I-16)
               GO TO ENDBEH-T
           END-IF
           IF  (I-18)
               GO TO ENDBEH-T
           END-IF
           IF  (I-07)
               GO TO ENDBEH-T
           END-IF
           IF  (I-23)
               GO TO ENDBEH-T
           END-IF
           IF  (I-76)
               GO TO ENDBEH-T
           END-IF
           IF  (I-24)
               GO TO ENDBEH-T
           END-IF
           IF  (I-19)
               GO TO ENDBEH-T
           END-IF
           IF  (I-31 AND I-12)
               GO TO ENDBEH-T
           END-IF
           IF  (I-31)
               ADD ANTLEV TO ZERO      GIVING ANTBEH
           END-IF
           IF  (I-15 AND NOT-I-31 AND NOT-I-12)
               SUBTRACT ANTRES FROM ANTBES GIVING ANTBEH
               SUBTRACT ANTBEH FROM ANTLEV GIVING ANTBEH
           END-IF
           IF  (I-15 AND NOT-I-31 AND I-12)
               SUBTRACT ANTLEV FROM ZERO GIVING ANTBEH
           END-IF
           SET NOT-I-39                    TO TRUE
           IF  ANTBEH NOT = 0,00
               SET I-39                    TO TRUE
           END-IF.
 
       ENDBEH-T.
           CONTINUE.
      *****************************************************************
      *     SUBRUTINE FOR Å DANNE FILE FOR OPPDATERING AV             *
      *     SALGSTAFILE. VERKSTEDSORDRE OG VERKTØYKASSER.             *
      *     ---------------------------------------------             *
      *  1. ALLE DAGENS FERDIGMELDTE VERKSTEDSORDRE.                  *
      *  2. ALLE DAGENS FERDIGMELDTE VERKTØYKASSE-ORDRE. 963,970,978,913
      *  3. IKKE LAGEROVERFØRINGER.                                   *
      *  3. IKKE UTGÅRMELDTE.                                         *
      *  4. IKKE REST-REGISTERING.                                    *
      *  5. IKKE TEKST LINJER.                                        *
      *  6. IKKE SKAFFEVARER.                                         *
      *  6. IKKE RETUR LEVERANDØR.                                    *
      *****************************************************************
 
       VKARUT-S SECTION.
       VKARUT-S-P.
           IF  (I-12)
               GO TO ENDVKA-T
           END-IF
           IF  (I-16)
               GO TO ENDVKA-T
           END-IF
           IF  (I-76)
               GO TO ENDVKA-T
           END-IF
           IF  (I-18)
               GO TO ENDVKA-T
           END-IF
           IF  (I-07)
               GO TO ENDVKA-T
           END-IF
           IF  (I-24)
               GO TO ENDVKA-T
           END-IF
           IF  (I-19)
               GO TO ENDVKA-T
           END-IF
           IF  (I-17)
               SET I-42                    TO TRUE
           END-IF
           IF  (I-41 AND I-16)
               SET I-42                    TO TRUE
      *
      *  IND. 41 MÅ SETTES AV OG PÅ PGA. FOR LITE INDIKATORER.
      *
           END-IF
           IF  (NOT-I-42)
               SET NOT-I-41                TO TRUE
               IF  FIRMA = '963'
                   SET I-41                TO TRUE
               END-IF
           END-IF
           IF  (I-41 AND NOT-I-42)
               SET NOT-I-42                TO TRUE
               IF  VGR = 20705
                   SET I-42                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-41 AND NOT-I-42)
               SET NOT-I-41                TO TRUE
               IF  FIRMA = '970'
                   SET I-41                TO TRUE
               END-IF
           END-IF
           IF  (I-41 AND NOT-I-42)
               SET NOT-I-42                TO TRUE
               IF  VGR = 12010
                   SET I-42                TO TRUE
               END-IF
           END-IF
           IF  (I-41 AND NOT-I-42)
               SET NOT-I-42                TO TRUE
               IF  VGR = 12012
                   SET I-42                TO TRUE
               END-IF
           END-IF
           IF  (I-41 AND NOT-I-42)
               SET NOT-I-42                TO TRUE
               IF  VGR = 33001
                   SET I-42                TO TRUE
               END-IF
           END-IF
           IF  (I-41 AND NOT-I-42)
               SET NOT-I-42                TO TRUE
               IF  VGR = 14130
                   SET I-42                TO TRUE
               END-IF
           END-IF
           IF  (I-41 AND NOT-I-42)
               SET NOT-I-42                TO TRUE
               IF  VGR = 34002
                   SET I-42                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-42)
               SET NOT-I-41                TO TRUE
               IF  FIRMA = '978'
                   SET I-41                TO TRUE
               END-IF
           END-IF
           IF  (I-41 AND NOT-I-42)
               SET NOT-I-42                TO TRUE
               IF  VGR = 32000
                   SET I-42                TO TRUE
               END-IF
           END-IF
           IF  (I-41 AND NOT-I-42)
               SET NOT-I-42                TO TRUE
               IF  VGR = 32050
                   SET I-42                TO TRUE
               END-IF
           END-IF
           IF  (I-41 AND NOT-I-42)
               SET NOT-I-42                TO TRUE
               IF  VGR = 72000
                   SET I-42                TO TRUE
               END-IF
           END-IF
           IF  (I-41 AND NOT-I-42)
               SET NOT-I-42                TO TRUE
               IF  VGR = 51000
                   SET I-42                TO TRUE
               END-IF
           END-IF
           IF  (I-41 AND NOT-I-42)
               SET NOT-I-42                TO TRUE
               IF  VGR = 37000
                   SET I-42                TO TRUE
               END-IF
           END-IF
           IF  (I-41 AND NOT-I-42)
               SET NOT-I-42                TO TRUE
               IF  VGR = 30020
                   SET I-42                TO TRUE
               END-IF
           END-IF
           IF  (I-41 AND NOT-I-42)
               SET NOT-I-42                TO TRUE
               IF  VGR = 30050
                   SET I-42                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-42)
               SET NOT-I-41                TO TRUE
               IF  FIRMA = '913'
                   SET I-41                TO TRUE
               END-IF
           END-IF
           IF  (I-41 AND NOT-I-42)
               SET NOT-I-42                TO TRUE
               IF  VGR = 11300
                   SET I-42                TO TRUE
               END-IF
           END-IF
           IF  (I-41 AND NOT-I-42)
               SET NOT-I-42                TO TRUE
               IF  VGR = 11190
                   SET I-42                TO TRUE
               END-IF
           END-IF
           IF  (I-41 AND NOT-I-42)
               SET NOT-I-42                TO TRUE
               IF  VGR = 15500
                   SET I-42                TO TRUE
               END-IF
           END-IF
           IF  (I-41 AND NOT-I-42)
               SET NOT-I-42                TO TRUE
               IF  VGR = 15510
                   SET I-42                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-42)
               GO TO ENDVKA-T
           END-IF
           IF  (NOT-I-23)
               ADD ANTBES TO ZERO      GIVING ANTVKA
           END-IF
           IF  (I-23)
               SUBTRACT ANTBES FROM ZERO GIVING ANTVKA
           END-IF
           ADD ANTVKA TO ZERO          GIVING ANTKBE
           IF  (NOT-I-23)
               ADD ANTLEV TO ZERO      GIVING ANTVKA
           END-IF
           IF  (I-23)
               SUBTRACT ANTLEV FROM ZERO GIVING ANTVKA
           END-IF
           ADD ANTVKA TO ZERO          GIVING ANTKUT.
 
       ENDVKA-T.
           CONTINUE.
      *****************************************************************
      *     SUBRUTINE FOR Å REGNE UT TOTAL VEKT PR. DAG PR. FIRMA     *
      *     ---------------------------------------------             *
      *  1. KUN FIRMA 950 HAFNOR. (INDIKATOR 95)                      *
      *  2. DAGENS FERDIGMELDTE ORDRE.                                *
      *  3. IKKE UTGÅRMELDTE.                                         *
      *  3. IKKE KREDIT NOTA.                                         *
      *  4. IKKE REST-REGISTERING.                                    *
      *  5. IKKE TEKST LINJER.                                        *
      *  6. IKKE SKAFFEVARER.                                         *
      *  7. IKKE RETUR LEVERANDØR.                                    *
      *****************************************************************
 
       VEKRUT-S SECTION.
       VEKRUT-S-P.
           IF  (I-12)
               GO TO ENDVEK-T
           END-IF
           IF  (I-23)
               GO TO ENDVEK-T
           END-IF
           IF  (I-16)
               GO TO ENDVEK-T
           END-IF
           IF  (I-76)
               GO TO ENDVEK-T
           END-IF
           IF  (I-18)
               GO TO ENDVEK-T
           END-IF
           IF  (I-24)
               GO TO ENDVEK-T
           END-IF
           IF  (I-19)
               GO TO ENDVEK-T
           END-IF
           MOVE VEKT                       TO VEKTN-IO
           MULTIPLY VEKTN BY ANTLEV    GIVING VEKTVL
           ADD VEKTVL                      TO VEKTL2.
 
       ENDVEK-T.
           CONTINUE.
      *****************************************************************
      *     SUBRUTINE FOR Å PRINTE FERDIGMELDINGSRAPPORT.             *
      *     ---------------------------------------------             *
      *  1. ALLE DAGENS FERDIGMELDTE ORDRE.                           *
      *  2. IKKE ORDRE FRA "SALG" OG "KRED".                          *
      *  3. IKKE REST/FORHÅND/TILBUDS - ORDRE REGISTRERING.           *
      *  4. IKKE RETUR LEVERANDØR.                                    *
      *****************************************************************
 
       FM1RUT-S SECTION.
       FM1RUT-S-P.
           SUBTRACT ODIFF2                 FROM ODIFF2
           IF  (I-18)
               GO TO ENDFM1-T
           END-IF
           IF  (I-76)
               GO TO ENDFM1-T
           END-IF
           IF  (I-23)
               GO TO ENDFM1-T
           END-IF
           IF  (I-43)
               GO TO ENDFM1-T
           END-IF
           IF  (I-88)
               SET I-90                    TO TRUE
               SET I-92                    TO TRUE
               SET I-93                    TO TRUE
           END-IF
           IF  (I-89)
               SET I-90                    TO TRUE
               SET I-92                    TO TRUE
               SET I-93                    TO TRUE
           END-IF.
 
       ENDFM1-T.
           CONTINUE.
      *****************************************************************
      *     SUBRUTINE FOR Å PRINTE FERDIGMELDINGSRAPPORT.             *
      * ANTVLIO = ANTALL VARELINJER FERDIGMELDT (IKKE KR.NOTA, IKKE   *
      *           UTGÅRMELDTE OG IKKE VARELINJER M/0 I ANT. LEV.      *
      *****************************************************************
 
       FM4RUT-S SECTION.
       FM4RUT-S-P.
           IF  (I-55)
               SET NOT-I-92                TO TRUE
               SET NOT-I-55                TO TRUE
           END-IF
           SET NOT-I-99                    TO TRUE
           IF  POSNR = 999
               SET I-99                    TO TRUE
           END-IF
           IF  (NOT-I-99)
               SET NOT-I-99                TO TRUE
               IF  POSNR = 998
                   SET I-99                TO TRUE
               END-IF
           END-IF
           IF  (I-24)
               GO TO ENDFM4-T
           END-IF
           IF  (NOT-I-12)
               SUBTRACT DIFLEV FROM ANTLEV GIVING ANTLF
               SET NOT-I-54                TO TRUE
               IF  DIFLEV NOT = 0
                   SET I-54                TO TRUE
               END-IF
           END-IF
           IF  (I-12)
               SUBTRACT ANTLEV FROM ZERO GIVING DIFLEV
               SET NOT-I-54                TO TRUE
               IF  DIFLEV NOT = 0
                   SET I-54                TO TRUE
               END-IF
           END-IF
           IF  (I-54)
               SET I-55                    TO TRUE
           END-IF
           IF  (I-54 AND NOT-I-99)
               SET I-57                    TO TRUE
           END-IF
           ADD 1                           TO FMVLI
           IF  (I-54 AND NOT-I-99)
               ADD 1                       TO FMVLIK
           END-IF
           IF  (NOT-I-28)
               ADD 1                       TO FMVNUL
           END-IF
           IF  (NOT-I-23 AND NOT-I-12 AND I-28)
               ADD 1                       TO FMVLIO
               ADD NETPRI                  TO FMOSUM
           END-IF
           SET NOT-I-56                    TO TRUE
           IF  NOREST = '0'
               SET I-56                    TO TRUE
           END-IF
           IF  (I-56)
               SET I-55                    TO TRUE
           END-IF
           IF  (I-54)
               MULTIPLY DIFLEV BY NETSTK GIVING VDIFF2
               ADD VDIFF2                  TO ODIFF2
               ADD VDIFF2                  TO FDIFF2
           END-IF
           IF  (I-88)
               SET I-55                    TO TRUE
           END-IF
           IF  (I-54)
               MOVE VGR                    TO VGR-N
               MOVE VGR-N-IO (1:1)         TO Y
               SET NOT-I-40                TO TRUE
               IF  Y = 0
                   SET I-40                TO TRUE
               END-IF
           END-IF
           IF  (I-54 AND I-40)
               MOVE 1                      TO Y
           END-IF
           IF  (I-54)
               ADD VDIFF2                  TO ARF (Y)
           END-IF.
 
       ENDFM4-T.
           CONTINUE.
      *****************************************************************
      *  SUBRUTINE FOR UTSKRIFT AV SELGERKOPI.                        *
      *  FØLGENDE ORDRE SKAL DET PRINTES SELGERKOPI AV.               *
      *  -  ALLE ORDRE MERKET MED * I SELGERKOPI OG "1" ELLER "2"     *
      *                I ORDRERUTINE I FIRMAFILE.                     *
      *****************************************************************
 
       SKOPI-S SECTION.
       SKOPI-S-P.
           IF  (I-04)
               GO TO SKOPI4-T
           END-IF
           IF  (NOT-I-L1)
               GO TO ENDSKO-T
           END-IF
           SUBTRACT ANTL                   FROM ANTL
           SUBTRACT ANTEL                  FROM ANTEL
           ADD REGKL TO ZERO           GIVING TIDSP
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *  COBOL SUBPROGRAM FOR HENTING AV BETALINGSMÅTE-TEKST      *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
           MOVE BETM                       TO BETMAT
           CALL 'BETBETN' USING BMFELT-XX-DATA-FIELDS
           SET NOT-I-74                    TO TRUE
           IF  BETMAT = '00'
               SET I-74                    TO TRUE
           END-IF
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
           SET NOT-I-29                    TO TRUE
           IF  (I-79)
               SET I-29                    TO TRUE
           END-IF
           IF  (I-78)
               SET I-29                    TO TRUE
           END-IF
           IF  (I-17)
               SET I-29                    TO TRUE
           END-IF
           GO TO ENDSKO-T.
 
       SKOPI4-T.
           SET NOT-I-27                    TO TRUE
           IF  (I-25)
               SET I-27                    TO TRUE
           END-IF
           IF  (I-26)
               SET I-27                    TO TRUE
           END-IF
           IF  (NOT-I-24 AND I-28)
               ADD 1                       TO ANTEL
           END-IF
           ADD 2                           TO ANTL
           SET NOT-I-50                    TO TRUE
           IF  ANTL NOT < 32
               SET I-50                    TO TRUE
           END-IF
           IF  (I-50)
               MOVE 1                      TO ANTL
      * * * * * EDITERING AV ANTALL BESTILT OG ANTALL REST.  * * *
           END-IF
           SET NOT-I-71                    TO TRUE
           SET NOT-I-73                    TO TRUE
           ADD ANTBES TO ZERO          GIVING ANTB1
           SET NOT-I-70                    TO TRUE
           IF  ANTBES > ANTB1
               SET I-70                    TO TRUE
           END-IF
           IF  (I-70)
               SET NOT-I-71                TO TRUE
               IF  ANTBES > 99,99
                   SET I-71                TO TRUE
               END-IF
           END-IF
           IF  (I-70 AND NOT-I-71)
               ADD ANTBES TO ZERO      GIVING ANTB2
           END-IF
           IF  (I-70 AND I-71)
               ADD ANTBES TO ZERO      GIVING ANTB3
           END-IF
           ADD ANTRES TO ZERO          GIVING ANTR1
           SET NOT-I-72                    TO TRUE
           IF  ANTRES > ANTR1
               SET I-72                    TO TRUE
           END-IF
           IF  (I-72)
               SET NOT-I-73                TO TRUE
               IF  ANTRES > 99,99
                   SET I-73                TO TRUE
               END-IF
           END-IF
           IF  (I-72 AND NOT-I-73)
               ADD ANTRES TO ZERO      GIVING ANTR2
           END-IF
           IF  (I-72 AND I-73)
               ADD ANTRES TO ZERO      GIVING ANTR3
           END-IF.
 
       ENDSKO-T.
           CONTINUE.
      **************************************************************
      *            SUBRUTINE FOR UTREGNING AV MOMS.                *
      *            OG AVRUNDING AV ORDRETOTAL.                     *
      *  BEREGNES KUN FOR KONTANSALGSORDRE SOM SKAL HA SELGERKOPI. *
      *  ER KODE FRITT = 1 ER DET UTEN MOMS.                       *
      **************************************************************
 
       MOMRUT-S SECTION.
       MOMRUT-S-P.
           ADD NETSUM TO ZERO          GIVING BUMVA
           MOVE 0                          TO BMMVA
           MOVE 0                          TO MVA
           CALL 'RUTMOMS2' USING MVFELT-XX-DATA-FIELDS
           ADD MVA TO ZERO             GIVING MOMS
           ADD BMMVA TO ZERO           GIVING TOTBEL
           IF  (I-97)
               ADD BUMVA TO ZERO       GIVING TOTBEL
           END-IF.
      **************************************************************
      *            SUBRUTINE TILSLUTT PR. ORDRE.                   *
      *  1. FERDIGMELDTE ORDRE MED NULL I ORDRESUM OPPDATERER      *
      *            ORDRENR-FILE OG MERKER DENNE SOM NULL-ORDRE.    *
      *  2. OPPTELLING TIL ORDRESTATUSRAPPORT PR. FIRMA OG TOTALT. *
      **************************************************************
 
       OSLRUT-S SECTION.
       OSLRUT-S-P.
      *  37 12             GOTO SLFORD                     = FORD,UTGÅR MELDT
           IF  (NOT-I-35)
               GO TO ORDSUM-T
           END-IF
           IF  (I-12)
               GO TO ORDSUM-T
           END-IF
           IF  (I-76)
               GO TO ORDSUM-T
           END-IF
           IF  (I-16)
               GO TO ORDSUM-T
           END-IF
           IF  (I-17)
               GO TO ORDSUM-T
           END-IF
           IF  (I-18)
               GO TO ORDSUM-T
           END-IF
           IF  (NOT-I-36)
               GO TO ORDSUM-T
      *          SLFORD    TAG
           END-IF
           IF  (I-U7)
               GO TO ORDSUM-T
           END-IF
           SET NOT-I-44                    TO TRUE
           IF  NETSUM = 0,00
               SET I-44                    TO TRUE
           END-IF
           IF  (I-44)
               SET NOT-I-44                TO TRUE
               IF  NETANT = 0,00
                   SET I-44                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-44)
               GO TO ORDSUM-T
           END-IF
           MOVE FIRMA                      TO ORDKEY (1:3)
           MOVE ORDNR                      TO ORDKEY (4:6)
           MOVE ORDKEY                     TO ORDNRM-KEY1
           READ ORDNRM RECORD KEY IS ORDNRM-KEY1
           INVALID KEY
               SET I-45                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-45                TO TRUE
               PERFORM ORDNRM-IDSET
           END-READ
      **  SUMMERING VED ORDRESLUTT TIL FERDIGMELDINGSRAPPORT. ******
           .
 
       ORDSUM-T.
           IF  (I-90 AND I-12)
               ADD 1                       TO FMOUT
           END-IF
           IF  (I-90 AND NOT-I-12)
               ADD 1                       TO FMORD
           END-IF
           IF  (I-90 AND NOT-I-12 AND I-57)
               ADD 1                       TO FMOKOR
      ****SKAL DENNE ORDRE UT PÅ DAGLIG ORDRERAPPORT ? *************
           END-IF
           IF  (I-48)
               SET I-49                    TO TRUE
           END-IF
           IF  (I-47)
               SET I-49                    TO TRUE
           END-IF
           IF  (I-12)
               SET I-49                    TO TRUE
           END-IF
           IF  (I-30)
               SET I-49                    TO TRUE
           END-IF
           IF  (I-81)
               SET I-49                    TO TRUE
           END-IF
           IF  (I-95 AND I-17 AND I-31)
               SET I-49                    TO TRUE
           END-IF
           IF  (I-31 AND I-76)
               SET I-49                    TO TRUE
           END-IF
           IF  (I-31 AND I-77)
               SET I-49                    TO TRUE
      **  SUMMERING AV ANTALL VED ORDRESLUTT TIL ORDRERAPPORT.  ******
           END-IF
           IF  (I-76 AND I-15 AND NOT-I-31)
               ADD 1                       TO ORA (2)
           END-IF
           IF  (I-76 AND I-15)
               GO TO ENDOSL-T
           END-IF
           IF  (I-12 AND I-15)
               ADD 1                       TO ORA (2)
           END-IF
           IF  (I-12 AND I-23 AND I-15)
               ADD 1                       TO KRA (2)
               ADD NETSUM                  TO KRB (2)
           END-IF
           IF  (I-12)
               GO TO OSLSUM-T
           END-IF
           IF  (I-31)
               ADD 1                       TO ORA (1)
           END-IF
           IF  (I-31 AND I-22)
               ADD 1                       TO ORA (7)
           END-IF
           IF  (I-31 AND I-23)
               ADD 1                       TO KRA (1)
           END-IF
           IF  (I-15 AND I-16)
               ADD 1                       TO ORA (3)
           END-IF
           IF  (I-15 AND I-16 AND I-23)
               ADD 1                       TO KRA (3)
           END-IF
           IF  (I-15 AND I-17)
               ADD 1                       TO ORA (3)
           END-IF
           IF  (I-15 AND I-17 AND I-23)
               ADD 1                       TO KRA (3)
           END-IF
           IF  (I-18)
               ADD 1                       TO ORA (3)
           END-IF
           IF  (I-18 AND I-23)
               ADD 1                       TO KRA (3)
           END-IF
           IF  (I-13 AND I-14)
               ADD 1                       TO ORA (4)
           END-IF
           IF  (I-13 AND I-14 AND I-23)
               ADD 1                       TO KRA (4)
           END-IF
           IF  (I-11)
               ADD 1                       TO ORA (5)
           END-IF
           IF  (I-11 AND I-23)
               ADD 1                       TO KRA (5)
           END-IF
           IF  (I-11 AND NOT-I-35)
               ADD 1                       TO ORA (6)
           END-IF
           IF  (I-11 AND I-23 AND NOT-I-35)
               ADD 1                       TO KRA (6)
      **  SUMMERING AV BELØP VED ORDRESLUTT TIL ORDRERAPPORT.  ******
           END-IF
           .
 
       OSLSUM-T.
           IF  (I-16)
               GO TO ENDOSL-T
           END-IF
           IF  (I-17)
               GO TO ENDOSL-T
           END-IF
           IF  (I-18)
               GO TO ENDOSL-T
           END-IF
           IF  (I-76)
               GO TO ENDOSL-T
           END-IF
           IF  (I-12 AND I-15)
               ADD NETSUM                  TO ORB (2)
           END-IF
           IF  (I-12)
               GO TO ENDOSL-T
           END-IF
           IF  (I-31)
               ADD NETSUM                  TO ORB (1)
           END-IF
           IF  (I-31 AND I-22)
               ADD NETSUM                  TO ORB (7)
           END-IF
           IF  (I-31 AND I-23)
               ADD NETSUM                  TO KRB (1)
           END-IF
           IF  (I-15 AND I-17)
               ADD NETSUM                  TO ORB (3)
           END-IF
           IF  (I-15 AND I-17 AND I-23)
               ADD NETSUM                  TO KRB (3)
           END-IF
           IF  (I-13 AND I-14)
               ADD NETSUM                  TO ORB (4)
           END-IF
           IF  (I-13 AND I-14 AND I-23)
               ADD NETSUM                  TO KRB (4)
           END-IF
           IF  (I-11)
               ADD NETSUM                  TO ORB (5)
           END-IF
           IF  (I-11 AND I-23)
               ADD NETSUM                  TO KRB (5)
           END-IF
           IF  (I-11 AND NOT-I-35)
               ADD NETSUM                  TO ORB (6)
           END-IF
           IF  (I-11 AND I-23 AND NOT-I-35)
               ADD NETSUM                  TO KRB (6)
           END-IF.
 
       ENDOSL-T.
           CONTINUE.
      **************************************************************
      *      SUBRUTINE FOR Å LEGGE UT TOTALRECORDERS PR. AVD.      *
      *  TOTALRECORD TIL ORDRE OG AVIKSLISTE LEGGES PR. AVDELING   *
      *  HVIS DET ER SALG/KREDIT PÅ DEN AVDELING.                  *
      **************************************************************
 
       AVTRUT-S SECTION.
       AVTRUT-S-P.
           SET NOT-I-60                    TO TRUE
           SET NOT-I-61                    TO TRUE
           SET NOT-I-62                    TO TRUE
           SET NOT-I-63                    TO TRUE
           SET NOT-I-64                    TO TRUE
           SET NOT-I-65                    TO TRUE
           SET NOT-I-66                    TO TRUE
           SET NOT-I-67                    TO TRUE
           SET NOT-I-68                    TO TRUE
           SET NOT-I-69                    TO TRUE
           SET NOT-I-05                    TO TRUE
           SET NOT-I-34                    TO TRUE
           SET NOT-I-09                    TO TRUE
           SET NOT-I-60                    TO TRUE
           IF  TOB (1) NOT = 0,00
               SET I-60                    TO TRUE
           END-IF
           IF  (NOT-I-60)
               SET NOT-I-60                TO TRUE
               IF  TKB (1) NOT = 0,00
                   SET I-60                TO TRUE
               END-IF
           END-IF
           SET NOT-I-61                    TO TRUE
           IF  TOB (2) NOT = 0,00
               SET I-61                    TO TRUE
           END-IF
           IF  (NOT-I-61)
               SET NOT-I-61                TO TRUE
               IF  TKB (2) NOT = 0,00
                   SET I-61                TO TRUE
               END-IF
           END-IF
           SET NOT-I-62                    TO TRUE
           IF  TOB (3) NOT = 0,00
               SET I-62                    TO TRUE
           END-IF
           IF  (NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  TKB (3) NOT = 0,00
                   SET I-62                TO TRUE
               END-IF
           END-IF
           SET NOT-I-63                    TO TRUE
           IF  TOB (4) NOT = 0,00
               SET I-63                    TO TRUE
           END-IF
           IF  (NOT-I-63)
               SET NOT-I-63                TO TRUE
               IF  TKB (4) NOT = 0,00
                   SET I-63                TO TRUE
               END-IF
           END-IF
           SET NOT-I-64                    TO TRUE
           IF  TOB (5) NOT = 0,00
               SET I-64                    TO TRUE
           END-IF
           IF  (NOT-I-64)
               SET NOT-I-64                TO TRUE
               IF  TKB (5) NOT = 0,00
                   SET I-64                TO TRUE
               END-IF
           END-IF
           SET NOT-I-65                    TO TRUE
           IF  TOB (6) NOT = 0,00
               SET I-65                    TO TRUE
           END-IF
           IF  (NOT-I-65)
               SET NOT-I-65                TO TRUE
               IF  TKB (6) NOT = 0,00
                   SET I-65                TO TRUE
               END-IF
           END-IF
           SET NOT-I-66                    TO TRUE
           IF  TOB (7) NOT = 0,00
               SET I-66                    TO TRUE
           END-IF
           IF  (NOT-I-66)
               SET NOT-I-66                TO TRUE
               IF  TKB (7) NOT = 0,00
                   SET I-66                TO TRUE
               END-IF
           END-IF
           SET NOT-I-67                    TO TRUE
           IF  TOB (8) NOT = 0,00
               SET I-67                    TO TRUE
           END-IF
           IF  (NOT-I-67)
               SET NOT-I-67                TO TRUE
               IF  TKB (8) NOT = 0,00
                   SET I-67                TO TRUE
               END-IF
           END-IF
           SET NOT-I-68                    TO TRUE
           IF  TOB (9) NOT = 0,00
               SET I-68                    TO TRUE
           END-IF
           IF  (NOT-I-68)
               SET NOT-I-68                TO TRUE
               IF  TKB (9) NOT = 0,00
                   SET I-68                TO TRUE
               END-IF
           END-IF
           SET NOT-I-69                    TO TRUE
           IF  TOB (10) NOT = 0,00
               SET I-69                    TO TRUE
           END-IF
           IF  (NOT-I-69)
               SET NOT-I-69                TO TRUE
               IF  TKB (10) NOT = 0,00
                   SET I-69                TO TRUE
               END-IF
           END-IF
           SET NOT-I-05                    TO TRUE
           IF  TOB (11) NOT = 0,00
               SET I-05                    TO TRUE
           END-IF
           IF  (NOT-I-05)
               SET NOT-I-05                TO TRUE
               IF  TKB (11) NOT = 0,00
                   SET I-05                TO TRUE
               END-IF
           END-IF
           SET NOT-I-34                    TO TRUE
           IF  TOB (12) NOT = 0,00
               SET I-34                    TO TRUE
           END-IF
           IF  (NOT-I-34)
               SET NOT-I-34                TO TRUE
               IF  TKB (12) NOT = 0,00
                   SET I-34                TO TRUE
               END-IF
           END-IF
           SET NOT-I-09                    TO TRUE
           IF  TOB (13) NOT = 0,00
               SET I-09                    TO TRUE
           END-IF
           IF  (NOT-I-34)
               SET NOT-I-09                TO TRUE
               IF  TKB (13) NOT = 0,00
                   SET I-09                TO TRUE
               END-IF
           END-IF
           IF  (I-93)
               SET NOT-I-57                TO TRUE
               IF  FMORD = 0
                   SET I-57                TO TRUE
               END-IF
           END-IF
           IF  (I-93 AND NOT-I-57)
               MULTIPLY 100 BY FMOKOR  GIVING FMOKO2
               DIVIDE FMOKO2 BY FMORD  GIVING FMOPRO ROUNDED
           END-IF
           IF  (I-93)
               SET NOT-I-57                TO TRUE
               IF  FMVLI = 0
                   SET I-57                TO TRUE
               END-IF
           END-IF
           IF  (I-93 AND NOT-I-57)
               MULTIPLY 100 BY FMVLIK  GIVING FMOKO2
               DIVIDE FMOKO2 BY FMVLI  GIVING FMVPRO ROUNDED
               MULTIPLY 100 BY FMVNUL  GIVING FMOKO2
               DIVIDE FMOKO2 BY FMVLI  GIVING FMVNPR ROUNDED
           END-IF.
      **************************************************************
      *      SUBRUTINE VED FIRMABRUDD.                             *
      *  1. OVERFØRE FIRMATOTALER TIL GRANDTOTALER, FOR AVSTEMMING.*
      *                                                            *
      **************************************************************
 
       FL2RUT-S SECTION.
       FL2RUT-S-P.
           SET ORA-I                       TO 1
           PERFORM VARYING OAT-I FROM 1 BY 1
                     UNTIL OAT-I > OAT-MAX
                        OR ORA-I > ORA-MAX
               ADD ORA (ORA-I)             TO OAT (OAT-I)
               SET ORA-I                UP BY 1
           END-PERFORM
           SUBTRACT ORA (8)                FROM ORA (8)
           SET KRA-I                       TO 1
           PERFORM VARYING KAT-I FROM 1 BY 1
                     UNTIL KAT-I > KAT-MAX
                        OR KRA-I > KRA-MAX
               ADD KRA (KRA-I)             TO KAT (KAT-I)
               SET KRA-I                UP BY 1
           END-PERFORM
           SET ORB-I                       TO 1
           PERFORM VARYING OBT-I FROM 1 BY 1
                     UNTIL OBT-I > OBT-MAX
                        OR ORB-I > ORB-MAX
               ADD ORB (ORB-I)             TO OBT (OBT-I)
               SET ORB-I                UP BY 1
           END-PERFORM
           SET KRB-I                       TO 1
           PERFORM VARYING KBT-I FROM 1 BY 1
                     UNTIL KBT-I > KBT-MAX
                        OR KRB-I > KRB-MAX
               ADD KRB (KRB-I)             TO KBT (KBT-I)
               SET KRB-I                UP BY 1
           END-PERFORM.
      *****************************************************************
      *  SUBRUTINE VED FOR FAKTURAPARAMETER.                          *
      *     ER FAKTURAOMGANG LIK SISTE FULLFØRTE FAKTURAOMGANG BLIR   *
      *     DET IKKE LAGT UT ORDRER TIL FAKTURERING.                  *
      *     P.G.A. ÅR 2000 BLIR DATO OMGJORT TIL 8 SIFFER ÅÅÅÅMMDD    *
      *****************************************************************
 
       FAPRUT-S SECTION.
       FAPRUT-S-P.
           READ FAKPAR
           AT END
               SET I-H0                    TO TRUE
               MOVE 'M'                    TO E-R-R-O-R
           NOT AT END
               PERFORM FAKPAR-FLDSET
               PERFORM FAKPAR-IDSET
           END-READ
           MOVE 'A'                        TO DATOK
           MOVE PSRDAT                     TO DATO6
           CALL 'DATO8SIF' USING DATOER-XX-DATA-FIELDS
           MOVE AMD8                       TO PSRDTO
           SET NOT-I-96                    TO TRUE
           IF  PSFAKT = 'J'
               SET I-96                    TO TRUE
           END-IF
           SET NOT-I-94                    TO TRUE
           IF  SFIMND = 'J'
               SET I-94                    TO TRUE
           END-IF
           SET NOT-I-38                    TO TRUE
           IF  PNFAKO = PFFAKO
               SET I-38                    TO TRUE
           END-IF.
      *****************************************************************
      *  SUBRUTINE FOR AVSTEMMING TIL SLUTT.                          *
      *                                                               *
      *                                                               *
      *****************************************************************
 
       AVSRLR-S SECTION.
       AVSRLR-S-P.
           MOVE 'ORD'                      TO AVSKEY
           SET NOT-I-97                    TO TRUE
           MOVE AVSKEY                     TO AVSTEMF-KEY1
           READ AVSTEMF RECORD KEY IS AVSTEMF-KEY1
           INVALID KEY
               SET I-94                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-94                TO TRUE
               PERFORM AVSTEMF-FLDSET
               PERFORM AVSTEMF-IDSET
           END-READ
           IF  (NOT-I-94)
               SET I-97                    TO TRUE
           END-IF
           MOVE AVSG1                      TO AVSGB
      *                    MOVE AVSG2     AVSGB
           MOVE AVSG2                      TO AVSGC
      *                    MOVE AVSG3     AVSGC
           .
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1 AND I-21 AND I-78)
               PERFORM MOMRUT-S
           END-IF
           IF  (I-L1)
               PERFORM OSLRUT-S
           END-IF
           IF  (I-L2)
               PERFORM AVTRUT-S
           END-IF
           IF  (I-L2)
               PERFORM FL2RUT-S
           END-IF.
 
       LR-CALCS SECTION.
       LR-CALCS-P.
           PERFORM AVSRLR-S
      *****************************************************************
      *  SUBRUTINE VED NYTT FIRMA.                                    *
      *****************************************************************
           .
 
       ORDREFS-GET SECTION.
       ORDREFS-GET-P.
           IF  ORDREFS-EOF-OFF
               READ ORDREFS
               AT END
                   SET ORDREFS-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       ORDREFS-FLDSET SECTION.
       ORDREFS-FLDSET-P.
           EVALUATE TRUE
           WHEN ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) = ' '
            AND   ORDREFS-IO-AREA (20:1) = '1' )
               MOVE ORDREFS-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE ORDREFS-IO-AREA (5:6)  TO ORDNR (1:6)
               MOVE ORDREFS-IO-AREA (21:6) TO KUNDNR (1:6)
               MOVE ORDREFS-IO-AREA (27:30) TO KNAVN1 (1:30)
               MOVE ORDREFS-IO-AREA (57:30) TO KNAVN2 (1:30)
               MOVE ORDREFS-IO-AREA (87:1) TO KTSIFF (1:1)
               MOVE ORDREFS-IO-AREA (88:1) TO DIRREG (1:1)
               MOVE ORDREFS-IO-AREA (89:1) TO FRITT (1:1)
               MOVE ORDREFS-IO-AREA (90:2) TO LAGER (1:2)
               MOVE ORDREFS-IO-AREA (92:1) TO BK (1:1)
               MOVE ORDREFS-IO-AREA (93:1) TO SKAF (1:1)
               MOVE ORDREFS-IO-AREA (94:2) TO BETM (1:2)
               MOVE ORDREFS-IO-AREA (96:1) TO GEBYR (1:1)
               MOVE ORDREFS-IO-AREA (97:1) TO FRAKT (1:1)
               MOVE ORDREFS-IO-AREA (98:1) TO AVD (1:1)
               MOVE ORDREFS-IO-AREA (99:1) TO KRTYPE (1:1)
               MOVE ORDREFS-IO-AREA (100:1) TO REST (1:1)
               MOVE ORDREFS-IO-AREA (105:1) TO KOMUTA (1:1)
               MOVE ORDREFS-IO-AREA (106:1) TO SAMFAK (1:1)
               MOVE ORDREFS-IO-AREA (108:2) TO PLUKAV (1:2)
               MOVE ORDREFS-IO-AREA (136:6) TO ORDATO-IO
               INSPECT ORDATO-IO REPLACING ALL ' ' BY '0'
               MOVE ORDREFS-IO-AREA (136:2) TO ORDDAG (1:2)
               MOVE ORDREFS-IO-AREA (138:2) TO ORDMND (1:2)
               MOVE ORDREFS-IO-AREA (140:2) TO ORDAAR (1:2)
               MOVE ORDREFS-IO-AREA (142:2) TO ORDMOT (1:2)
               MOVE ORDREFS-IO-AREA (144:4) TO TERMID (1:4)
               MOVE ORDREFS-IO-AREA (148:1) TO SELGKP (1:1)
               MOVE ORDREFS-IO-AREA (149:1) TO FERDIM (1:1)
               MOVE ORDREFS-IO-AREA (150:2) TO FAKTNR (1:2)
               MOVE ORDREFS-IO-AREA (152:1) TO KONFAK (1:1)
               MOVE ORDREFS-IO-AREA (153:4) TO REGKL-IO
               MOVE ORDREFS-IO-AREA (157:1) TO RUTID (1:1)
               MOVE ORDREFS-IO-AREA (162:2) TO ANTPRT-IO
               INSPECT ANTPRT-IO REPLACING ALL ' ' BY '0'
               MOVE ORDREFS-IO-AREA (164:1) TO STATUS-X (1:1)
               MOVE ORDREFS-IO-AREA (1:164) TO OHREC1 (1:164)
           WHEN ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) = ' '
            AND   ORDREFS-IO-AREA (20:1) = '2' )
               MOVE ORDREFS-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE ORDREFS-IO-AREA (5:6)  TO ORDNR (1:6)
               MOVE ORDREFS-IO-AREA (44:6) TO FAKREF (1:6)
               MOVE ORDREFS-IO-AREA (50:11) TO AVNAVN (1:11)
               MOVE ORDREFS-IO-AREA (67:15) TO REKVNR (1:15)
               MOVE ORDREFS-IO-AREA (82:15) TO FORSM (1:15)
               MOVE ORDREFS-IO-AREA (82:5) TO FORS5F (1:5)
               MOVE ORDREFS-IO-AREA (97:3) TO HND (1:3)
               MOVE ORDREFS-IO-AREA (101:30) TO KADR (1:30)
               MOVE ORDREFS-IO-AREA (131:4) TO POSTNR (1:4)
               MOVE ORDREFS-IO-AREA (135:15) TO PSTED (1:15)
               MOVE ORDREFS-IO-AREA (1:164) TO OHREC2 (1:164)
           WHEN ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) = ' '
            AND   ORDREFS-IO-AREA (20:1) = '3' )
               MOVE ORDREFS-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE ORDREFS-IO-AREA (5:6)  TO ORDNR (1:6)
               MOVE ORDREFS-IO-AREA (21:30) TO VAADR1 (1:30)
               MOVE ORDREFS-IO-AREA (51:30) TO VAADR2 (1:30)
               MOVE ORDREFS-IO-AREA (81:30) TO VAADR3 (1:30)
               MOVE ORDREFS-IO-AREA (111:20) TO VAADR4 (1:20)
               MOVE ORDREFS-IO-AREA (1:164) TO OHREC3 (1:164)
           WHEN ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) NOT = ' ' )
               MOVE ORDREFS-IO-AREA (2:3)  TO FIRMA (1:3)
               MOVE ORDREFS-IO-AREA (5:6)  TO ORDNR (1:6)
               MOVE ORDREFS-IO-AREA (11:6) TO LAGLOC (1:6)
               MOVE ORDREFS-IO-AREA (17:3) TO POSNR-IO
               INSPECT POSNR-IO REPLACING ALL ' ' BY '0'
               MOVE ORDREFS-IO-AREA (21:4) TO ANTBES-IO
               MOVE ORDREFS-IO-AREA (25:4) TO ANTRES-IO
               MOVE ORDREFS-IO-AREA (29:4) TO ANTLEV-IO
               MOVE ORDREFS-IO-AREA (33:1) TO NOREST (1:1)
               MOVE ORDREFS-IO-AREA (34:3) TO ALF (1:3)
               MOVE ORDREFS-IO-AREA (37:50) TO TEKST (1:50)
               MOVE ORDREFS-IO-AREA (37:20) TO ARTNR (1:20)
               MOVE ORDREFS-IO-AREA (57:30) TO VARBET (1:30)
               MOVE ORDREFS-IO-AREA (57:20) TO VARB20 (1:20)
               MOVE ORDREFS-IO-AREA (87:4) TO EDBNR-IO
               MOVE ORDREFS-IO-AREA (91:3) TO VGR-IO
               MOVE ORDREFS-IO-AREA (94:5) TO ORPRIS-IO
               MOVE ORDREFS-IO-AREA (99:2) TO ORRAB1-IO
               MOVE ORDREFS-IO-AREA (101:2) TO ORRAB2-IO
               MOVE ORDREFS-IO-AREA (103:2) TO ORRAB3-IO
               MOVE ORDREFS-IO-AREA (105:5) TO RGPRIS-IO
               MOVE ORDREFS-IO-AREA (110:2) TO RGRAB1-IO
               MOVE ORDREFS-IO-AREA (112:2) TO RGRAB2-IO
               MOVE ORDREFS-IO-AREA (114:2) TO RGRAB3-IO
               MOVE ORDREFS-IO-AREA (121:5) TO KOSPRI-IO
               MOVE ORDREFS-IO-AREA (126:4) TO PRITIL-IO
               MOVE ORDREFS-IO-AREA (137:1) TO PRITYP (1:1)
               MOVE ORDREFS-IO-AREA (135:1) TO VLKOD2 (1:1)
               MOVE ORDREFS-IO-AREA (144:7) TO DIFLEV-IO
               INSPECT DIFLEV-IO REPLACING ALL ' ' BY '0'
               MOVE ORDREFS-IO-AREA (154:5) TO VEKT (1:5)
               MOVE ORDREFS-IO-AREA (159:3) TO NETFAK-IO
               MOVE ORDREFS-IO-AREA (1:164) TO OVREC (1:164)
           END-EVALUATE.
 
       ORDREFS-IDCHK SECTION.
       ORDREFS-IDCHK-P.
           EVALUATE TRUE
           WHEN ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) = ' '
            AND   ORDREFS-IO-AREA (20:1) = '1' )
             OR ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) = ' '
            AND   ORDREFS-IO-AREA (20:1) = '2' )
             OR ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) = ' '
            AND   ORDREFS-IO-AREA (20:1) = '3' )
             OR ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) NOT = ' ' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       ORDREFS-IDSET SECTION.
       ORDREFS-IDSET-P.
           EVALUATE TRUE
           WHEN ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) = ' '
            AND   ORDREFS-IO-AREA (20:1) = '1' )
               SET I-01                    TO TRUE
           WHEN ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) = ' '
            AND   ORDREFS-IO-AREA (20:1) = '2' )
               SET I-02                    TO TRUE
           WHEN ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) = ' '
            AND   ORDREFS-IO-AREA (20:1) = '3' )
               SET I-03                    TO TRUE
           WHEN ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) NOT = ' ' )
               SET I-04                    TO TRUE
           END-EVALUATE.
 
       ORDREFS-CHK-LEVEL SECTION.
       ORDREFS-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) = ' '
            AND   ORDREFS-IO-AREA (20:1) = '1' )
               MOVE LOW-VALUES             TO ORDREFS-LEVEL-01
               MOVE ORDREFS-IO-AREA (2:3)  TO ORDREFS-01-L2-FIRMA
               MOVE ORDREFS-IO-AREA (5:6)  TO ORDREFS-01-L1-ORDNR
               IF  ORDREFS-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDREFS-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  ORDREFS-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDREFS-01-L2         TO THE-PRIOR-L2
               MOVE  ORDREFS-01-L1         TO THE-PRIOR-L1
               SET ORDREFS-LEVEL-INIT      TO TRUE
           WHEN ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) = ' '
            AND   ORDREFS-IO-AREA (20:1) = '2' )
               MOVE LOW-VALUES             TO ORDREFS-LEVEL-02
               MOVE ORDREFS-IO-AREA (2:3)  TO ORDREFS-02-L2-FIRMA
               MOVE ORDREFS-IO-AREA (5:6)  TO ORDREFS-02-L1-ORDNR
               IF  ORDREFS-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDREFS-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  ORDREFS-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDREFS-02-L2         TO THE-PRIOR-L2
               MOVE  ORDREFS-02-L1         TO THE-PRIOR-L1
               SET ORDREFS-LEVEL-INIT      TO TRUE
           WHEN ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) = ' '
            AND   ORDREFS-IO-AREA (20:1) = '3' )
               MOVE LOW-VALUES             TO ORDREFS-LEVEL-03
               MOVE ORDREFS-IO-AREA (2:3)  TO ORDREFS-03-L2-FIRMA
               MOVE ORDREFS-IO-AREA (5:6)  TO ORDREFS-03-L1-ORDNR
               IF  ORDREFS-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDREFS-03-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  ORDREFS-03-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDREFS-03-L2         TO THE-PRIOR-L2
               MOVE  ORDREFS-03-L1         TO THE-PRIOR-L1
               SET ORDREFS-LEVEL-INIT      TO TRUE
           WHEN ( ORDREFS-IO-AREA (1:1) = 'O'
            AND   ORDREFS-IO-AREA (19:1) NOT = ' ' )
               MOVE LOW-VALUES             TO ORDREFS-LEVEL-04
               MOVE ORDREFS-IO-AREA (2:3)  TO ORDREFS-04-L2-FIRMA
               MOVE ORDREFS-IO-AREA (5:6)  TO ORDREFS-04-L1-ORDNR
               IF  ORDREFS-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  ORDREFS-04-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  ORDREFS-04-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  ORDREFS-04-L2         TO THE-PRIOR-L2
               MOVE  ORDREFS-04-L1         TO THE-PRIOR-L1
               SET ORDREFS-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (8:30)  TO FINAVN (1:30)
               MOVE FIRMAF-IO-AREA (123:1) TO FSLETT (1:1)
               MOVE FIRMAF-IO-AREA (503:1) TO FFAKGR (1:1)
               MOVE FIRMAF-IO-AREA (504:30) TO FIADR (1:30)
               MOVE FIRMAF-IO-AREA (534:4) TO FIPNR (1:4)
               MOVE FIRMAF-IO-AREA (538:26) TO FIPOST (1:26)
               MOVE FIRMAF-IO-AREA (704:1) TO FMRUT (1:1)
               MOVE FIRMAF-IO-AREA (787:4) TO FAVLDM-IO
               MOVE FIRMAF-IO-AREA (791:3) TO FAORDM-IO
               MOVE FIRMAF-IO-AREA (876:1) TO FIREST (1:1)
               MOVE FIRMAF-IO-AREA (878:1) TO NYFSAL (1:1)
               MOVE FIRMAF-IO-AREA (901:3) TO FADODM-IO
               MOVE FIRMAF-IO-AREA (956:1) TO BRTYPE (1:1)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-08                        TO TRUE.
 
       FAKPAR-FLDSET SECTION.
       FAKPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FAKPAR-IO-AREA (10:2)  TO PNFAKO (1:2)
               MOVE FAKPAR-IO-AREA (12:6)  TO PSRDAT (1:6)
               MOVE FAKPAR-IO-AREA (67:1)  TO PSFAKT (1:1)
               MOVE FAKPAR-IO-AREA (68:1)  TO SFIMND (1:1)
               MOVE FAKPAR-IO-AREA (89:2)  TO PFFAKO (1:2)
               MOVE FAKPAR-IO-AREA (91:1)  TO PFAKGR (1:1)
               MOVE FAKPAR-IO-AREA (191:3) TO IKKEF1 (1:3)
               MOVE FAKPAR-IO-AREA (194:3) TO IKKEF2 (1:3)
               MOVE FAKPAR-IO-AREA (197:3) TO IKKEF3 (1:3)
           END-EVALUATE.
 
       FAKPAR-IDSET SECTION.
       FAKPAR-IDSET-P.
           SET I-08                        TO TRUE.
 
       AVSTEMF-FLDSET SECTION.
       AVSTEMF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE AVSTEMF-IO-AREA (11:110) TO AVSG1 (1:110)
               MOVE AVSTEMF-IO-AREA (131:110) TO AVSG2 (1:110)
           END-EVALUATE.
 
       AVSTEMF-IDSET SECTION.
       AVSTEMF-IDSET-P.
           SET I-08                        TO TRUE.
 
       ORDNRM-IDSET SECTION.
       ORDNRM-IDSET-P.
           SET I-08                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-11)
               MOVE SPACES TO ORDREM-IO-AREA
               INITIALIZE ORDREM-IO-AREA
               MOVE OHREC1                 TO ORDREM-IO-AREA (1:164)
               MOVE ' '                    TO ORDREM-IO-AREA (97:1)
               MOVE NYAVD                  TO ORDREM-IO-AREA (98:1)
               MOVE '  '                   TO ORDREM-IO-AREA (148:2)
               IF  (I-36)
                   MOVE PNFAKO             TO ORDREM-IO-AREA (150:2)
               END-IF
               IF  (I-30)
                   MOVE ANTPRX-IO          TO ORDREM-IO-AREA (162:2)
               END-IF
               IF  (I-36)
                   MOVE 'C'                TO ORDREM-IO-AREA (164:1)
               END-IF
               WRITE ORDREM-IO-AREA
           END-IF
           IF  (I-02 AND I-11)
               MOVE SPACES TO ORDREM-IO-AREA
               INITIALIZE ORDREM-IO-AREA
               MOVE OHREC2                 TO ORDREM-IO-AREA (1:164)
               WRITE ORDREM-IO-AREA
           END-IF
           IF  (I-03 AND I-11)
               MOVE SPACES TO ORDREM-IO-AREA
               INITIALIZE ORDREM-IO-AREA
               MOVE OHREC3                 TO ORDREM-IO-AREA (1:164)
               WRITE ORDREM-IO-AREA
           END-IF
           IF  (I-04 AND I-11)
               MOVE SPACES TO ORDREM-IO-AREA
               INITIALIZE ORDREM-IO-AREA
               MOVE OVREC                  TO ORDREM-IO-AREA (1:164)
               MOVE '      '               TO ORDREM-IO-AREA (138:6)
               WRITE ORDREM-IO-AREA
           END-IF
           IF  (I-01 AND I-36)
               MOVE SPACES TO ORDFAKT-IO-AREA
               INITIALIZE ORDFAKT-IO-AREA
               MOVE OHREC1                 TO ORDFAKT-IO-AREA (1:164)
               MOVE NYAVD                  TO ORDFAKT-IO-AREA (98:1)
               WRITE ORDFAKT-IO-AREA
           END-IF
           IF  (I-02 AND I-36)
               MOVE SPACES TO ORDFAKT-IO-AREA
               INITIALIZE ORDFAKT-IO-AREA
               MOVE OHREC2                 TO ORDFAKT-IO-AREA (1:164)
               WRITE ORDFAKT-IO-AREA
           END-IF
           IF  (I-03 AND I-36)
               MOVE SPACES TO ORDFAKT-IO-AREA
               INITIALIZE ORDFAKT-IO-AREA
               MOVE OHREC3                 TO ORDFAKT-IO-AREA (1:164)
               WRITE ORDFAKT-IO-AREA
           END-IF
           IF  (I-04 AND I-36)
               MOVE SPACES TO ORDFAKT-IO-AREA
               INITIALIZE ORDFAKT-IO-AREA
               MOVE OVREC                  TO ORDFAKT-IO-AREA (1:164)
               WRITE ORDFAKT-IO-AREA
           END-IF
           IF  (I-04 AND I-39)
               MOVE SPACES TO SALGANT-IO-AREA
               INITIALIZE SALGANT-IO-AREA
               MOVE 'D'                    TO SALGANT-IO-AREA (1:1)
               MOVE EDBNR                  TO XO-70U
               MOVE XO-70U (1:7)           TO SALGANT-IO-AREA (2:7)
               MOVE ANTBEH                 TO XO-72P
               MOVE XO-72P-EF              TO SALGANT-IO-AREA (9:5)
               INITIALIZE ANTBEH
               MOVE ORDAAR                 TO SALGANT-IO-AREA (14:2)
               MOVE ORDMND                 TO SALGANT-IO-AREA (16:2)
               MOVE FIRMA                  TO SALGANT-IO-AREA (18:3)
               WRITE SALGANT-IO-AREA
           END-IF
           IF  (I-04 AND I-42)
               MOVE SPACES TO VKASSEF-IO-AREA
               INITIALIZE VKASSEF-IO-AREA
               IF  (I-41)
                   MOVE '8'                TO VKASSEF-IO-AREA (1:1)
               END-IF
               IF  (I-16)
                   MOVE 'V'                TO VKASSEF-IO-AREA (1:1)
               END-IF
               IF  (I-17)
                   MOVE 'V'                TO VKASSEF-IO-AREA (1:1)
               END-IF
               MOVE VGR                    TO XO-50U
               MOVE XO-50U (1:5)           TO VKASSEF-IO-AREA (2:5)
               MOVE ANTVKA                 TO XO-52P
               MOVE XO-52P-EF              TO VKASSEF-IO-AREA (12:4)
               INITIALIZE ANTVKA
               IF EDBNR < 0
                 MOVE EDBNR                TO XO-70D
                 MOVE XO-70D (1:7)         TO VKASSEF-IO-AREA (16:7)
               ELSE
                 MOVE EDBNR                TO XO-70U
                 MOVE XO-70U (1:7)         TO VKASSEF-IO-AREA (16:7)
               END-IF
               MOVE FIRMA                  TO VKASSEF-IO-AREA (51:3)
               MOVE ORDNR                  TO VKASSEF-IO-AREA (54:6)
               MOVE POSNR-IO               TO VKASSEF-IO-AREA (60:3)
               MOVE VLKOD2                 TO VKASSEF-IO-AREA (63:1)
               WRITE VKASSEF-IO-AREA
           END-IF
           IF  (I-04 AND I-42 AND I-58)
               MOVE SPACES TO UTAKORD-IO-AREA
               INITIALIZE UTAKORD-IO-AREA
               MOVE 'V'                    TO UTAKORD-IO-AREA (1:1)
               MOVE FIRMA                  TO UTAKORD-IO-AREA (2:3)
               MOVE ORDNR                  TO UTAKORD-IO-AREA (5:6)
               IF EDBNR < 0
                 MOVE EDBNR                TO XO-70D
                 MOVE XO-70D (1:7)         TO UTAKORD-IO-AREA (11:7)
               ELSE
                 MOVE EDBNR                TO XO-70U
                 MOVE XO-70U (1:7)         TO UTAKORD-IO-AREA (11:7)
               END-IF
               MOVE BK                     TO UTAKORD-IO-AREA (18:1)
               MOVE ANTKUT                 TO XO-72P
               MOVE XO-72P-EF              TO UTAKORD-IO-AREA (19:5)
               INITIALIZE ANTKUT
               MOVE ALF                    TO UTAKORD-IO-AREA (24:3)
               MOVE ARTNR                  TO UTAKORD-IO-AREA (37:20)
               MOVE ORPRIS                 TO XO-72P
               MOVE XO-72P-EF              TO UTAKORD-IO-AREA (57:5)
               MOVE ORRAB1                 TO XO-21P
               MOVE XO-21P-EF              TO UTAKORD-IO-AREA (62:2)
               MOVE ORRAB2                 TO XO-21P
               MOVE XO-21P-EF              TO UTAKORD-IO-AREA (64:2)
               MOVE ORRAB3                 TO XO-21P
               MOVE XO-21P-EF              TO UTAKORD-IO-AREA (66:2)
               MOVE ANTKBE                 TO XO-72P
               MOVE XO-72P-EF              TO UTAKORD-IO-AREA (68:5)
               IF VGR < 0
                 MOVE VGR                  TO XO-50D
                 MOVE XO-50D (1:5)         TO UTAKORD-IO-AREA (73:5)
               ELSE
                 MOVE VGR                  TO XO-50U
                 MOVE XO-50U (1:5)         TO UTAKORD-IO-AREA (73:5)
               END-IF
               MOVE KUNDNR                 TO UTAKORD-IO-AREA (78:6)
               MOVE ORDATO-IO              TO UTAKORD-IO-AREA (84:6)
               MOVE ORDMOT                 TO UTAKORD-IO-AREA (90:2)
               MOVE PRITIL                 TO XO-52P
               MOVE XO-52P-EF              TO UTAKORD-IO-AREA (92:4)
               MOVE RUTID                  TO UTAKORD-IO-AREA (96:1)
               MOVE NYAVD                  TO UTAKORD-IO-AREA (97:1)
               MOVE REKVNR                 TO UTAKORD-IO-AREA (101:15)
               MOVE FORS5F                 TO UTAKORD-IO-AREA (116:5)
               WRITE UTAKORD-IO-AREA
           END-IF
           IF  (I-52 AND I-21)
           OR  (I-50 AND I-21)
               MOVE SPACES TO PAKKS-IO-AREA
               INITIALIZE PAKKS-IO-AREA
               MOVE FINAVN                 TO PAKKS-IO-AREA (2:30)
               MOVE KNAVN1                 TO PAKKS-IO-AREA (37:30)
               IF  (I-53)
                   MOVE VAADR1             TO PAKKS-IO-AREA (79:30)
               END-IF
               MOVE FIRMA                  TO PAKKS-IO-AREA (133:3)
               MOVE 'F'                    TO PAKKS-IO-AREA (136:1)
               MOVE '6'                    TO PAKKS-IO-AREA (137:1)
               MOVE 'ORD04'                TO PAKKS-IO-AREA (138:5)
               MOVE 'ORD110'               TO PAKKS-IO-AREA (143:6)
               MOVE 'A'                    TO PAKKS-IO-AREA (150:1)
               WRITE PAKKS-IO-AREA
               MOVE SPACES TO PAKKS-IO-AREA
               INITIALIZE PAKKS-IO-AREA
               MOVE FIADR                  TO PAKKS-IO-AREA (2:30)
               MOVE KNAVN2                 TO PAKKS-IO-AREA (37:30)
               IF  (I-53)
                   MOVE VAADR2             TO PAKKS-IO-AREA (79:30)
               END-IF
               MOVE FIRMA                  TO PAKKS-IO-AREA (133:3)
               MOVE ' '                    TO PAKKS-IO-AREA (136:1)
               MOVE '6'                    TO PAKKS-IO-AREA (137:1)
               MOVE 'ORD04'                TO PAKKS-IO-AREA (138:5)
               MOVE 'ORD110'               TO PAKKS-IO-AREA (143:6)
               MOVE 'X'                    TO PAKKS-IO-AREA (150:1)
               WRITE PAKKS-IO-AREA
               MOVE SPACES TO PAKKS-IO-AREA
               INITIALIZE PAKKS-IO-AREA
               MOVE FIPNR                  TO PAKKS-IO-AREA (2:4)
               MOVE FIPOST                 TO PAKKS-IO-AREA (7:26)
               MOVE KADR                   TO PAKKS-IO-AREA (37:30)
               IF  (I-53)
                   MOVE VAADR3             TO PAKKS-IO-AREA (79:30)
               END-IF
               MOVE FIRMA                  TO PAKKS-IO-AREA (133:3)
               MOVE ' '                    TO PAKKS-IO-AREA (136:1)
               MOVE '6'                    TO PAKKS-IO-AREA (137:1)
               MOVE 'ORD04'                TO PAKKS-IO-AREA (138:5)
               MOVE 'ORD110'               TO PAKKS-IO-AREA (143:6)
               MOVE 'X'                    TO PAKKS-IO-AREA (150:1)
               WRITE PAKKS-IO-AREA
               MOVE SPACES TO PAKKS-IO-AREA
               INITIALIZE PAKKS-IO-AREA
               MOVE POSTNR                 TO PAKKS-IO-AREA (37:4)
               MOVE PSTED                  TO PAKKS-IO-AREA (42:15)
               IF  (I-53)
                   MOVE VAADR4             TO PAKKS-IO-AREA (79:20)
               END-IF
               MOVE HND                    TO PAKKS-IO-AREA (111:3)
               MOVE FIRMA                  TO PAKKS-IO-AREA (133:3)
               MOVE ' '                    TO PAKKS-IO-AREA (136:1)
               MOVE '7'                    TO PAKKS-IO-AREA (137:1)
               MOVE 'ORD04'                TO PAKKS-IO-AREA (138:5)
               MOVE 'ORD110'               TO PAKKS-IO-AREA (143:6)
               MOVE 'X'                    TO PAKKS-IO-AREA (150:1)
               WRITE PAKKS-IO-AREA
               MOVE SPACES TO PAKKS-IO-AREA
               INITIALIZE PAKKS-IO-AREA
               MOVE ORDNR                  TO PAKKS-IO-AREA (15:6)
               MOVE KTSIFF                 TO PAKKS-IO-AREA (22:1)
               MOVE 'SIDE'                 TO PAKKS-IO-AREA (25:4)
               IF  (I-52)
                   MOVE ZERO TO PAGE0
               END-IF
               ADD 1                       TO PAGE0
               MOVE PAGE0                  TO XO-40YNZ
               MOVE XO-40YNZ               TO PAKKS-IO-AREA (29:4)
               IF  (I-22)
                   MOVE '*** DIREKTE ORDRE ***' TO PAKKS-IO-AREA
                                                               (36:21)
               END-IF
               IF  (I-18)
                   MOVE '* REST-REGISTRERING ' TO PAKKS-IO-AREA (77:20)
               END-IF
               IF  (I-76)
                   MOVE '* RETUR LEVERANDØR. ' TO PAKKS-IO-AREA (77:20)
               END-IF
               IF  (I-30)
                   MOVE '* UTLISTET    GANGER' TO PAKKS-IO-AREA (97:20)
               END-IF
               IF  (I-30)
                   MOVE ANTPRT             TO XO-20YY9
                   MOVE XO-20YY9           TO PAKKS-IO-AREA (108:2)
               END-IF
               MOVE FIRMA                  TO PAKKS-IO-AREA (133:3)
               MOVE ' '                    TO PAKKS-IO-AREA (136:1)
               MOVE '6'                    TO PAKKS-IO-AREA (137:1)
               MOVE 'ORD04'                TO PAKKS-IO-AREA (138:5)
               MOVE 'ORD110'               TO PAKKS-IO-AREA (143:6)
               MOVE 'X'                    TO PAKKS-IO-AREA (150:1)
               WRITE PAKKS-IO-AREA
               MOVE SPACES TO PAKKS-IO-AREA
               INITIALIZE PAKKS-IO-AREA
               MOVE KUNDNR                 TO PAKKS-IO-AREA (4:6)
               MOVE ORDATO                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO PAKKS-IO-AREA (12:8)
               MOVE FRITT                  TO PAKKS-IO-AREA (22:1)
               MOVE LAGER                  TO PAKKS-IO-AREA (25:2)
               MOVE BK                     TO PAKKS-IO-AREA (29:1)
               MOVE SKAF                   TO PAKKS-IO-AREA (32:1)
               IF  (NOT-I-74)
                   MOVE BMTKST             TO PAKKS-IO-AREA (35:24)
               END-IF
               MOVE BETM                   TO PAKKS-IO-AREA (61:2)
               MOVE REKVNR                 TO PAKKS-IO-AREA (65:15)
               MOVE ORDMOT                 TO PAKKS-IO-AREA (83:2)
               MOVE AVNAVN                 TO PAKKS-IO-AREA (87:11)
               MOVE 'REF.'                 TO PAKKS-IO-AREA (100:4)
               MOVE FAKREF                 TO PAKKS-IO-AREA (104:6)
               MOVE REST                   TO PAKKS-IO-AREA (112:1)
               MOVE GEBYR                  TO PAKKS-IO-AREA (115:1)
               MOVE FIRMA                  TO PAKKS-IO-AREA (133:3)
               MOVE 'I'                    TO PAKKS-IO-AREA (136:1)
               MOVE '8'                    TO PAKKS-IO-AREA (137:1)
               MOVE 'ORD04'                TO PAKKS-IO-AREA (138:5)
               MOVE 'ORD110'               TO PAKKS-IO-AREA (143:6)
               MOVE 'X'                    TO PAKKS-IO-AREA (150:1)
               WRITE PAKKS-IO-AREA
           END-IF
           IF  (I-04 AND I-21)
               MOVE SPACES TO PAKKS-IO-AREA
               INITIALIZE PAKKS-IO-AREA
               MOVE POSNR-IO               TO PAKKS-IO-AREA (3:3)
               IF  (NOT-I-24 AND NOT-I-70)
                   MOVE ANTB1              TO XO-50YNZ
                   MOVE XO-50YNZ           TO PAKKS-IO-AREA (7:5)
               END-IF
               IF  (NOT-I-24 AND I-70 AND NOT-I-71)
                   MOVE ANTB2              TO EDIT-ANTB2
                   MOVE EDIT-ANTB2         TO PAKKS-IO-AREA (6:6)
               END-IF
               IF  (NOT-I-24 AND I-70 AND I-71)
                   MOVE ANTB3              TO EDIT-ANTB3
                   MOVE EDIT-ANTB3         TO PAKKS-IO-AREA (6:8)
               END-IF
               IF  (NOT-I-24 AND NOT-I-72)
                   MOVE ANTR1              TO XO-50YNZ
                   MOVE XO-50YNZ           TO PAKKS-IO-AREA (15:5)
               END-IF
               IF  (NOT-I-24 AND I-72 AND NOT-I-73)
                   MOVE ANTR2              TO EDIT-ANTR2
                   MOVE EDIT-ANTR2         TO PAKKS-IO-AREA (14:6)
               END-IF
               IF  (NOT-I-24 AND I-72 AND I-73)
                   MOVE ANTR3              TO EDIT-ANTR3
                   MOVE EDIT-ANTR3         TO PAKKS-IO-AREA (14:8)
               END-IF
               IF  (NOT-I-24 AND NOT-I-28)
                   MOVE '0'                TO PAKKS-IO-AREA (24:1)
               END-IF
               IF  (NOT-I-24)
                   MOVE LAGLOC             TO PAKKS-IO-AREA (27:6)
               END-IF
               IF  (NOT-I-24)
                   MOVE ALF                TO PAKKS-IO-AREA (34:3)
               END-IF
               IF  (NOT-I-24)
                   MOVE ARTNR              TO PAKKS-IO-AREA (38:20)
               END-IF
               IF  (NOT-I-24)
                   MOVE VARBET             TO PAKKS-IO-AREA (59:30)
               END-IF
               IF  (I-24)
                   MOVE TEKST              TO PAKKS-IO-AREA (38:50)
               END-IF
               IF  (NOT-I-24 AND NOT-I-25)
                   MOVE RGPRIS             TO XO-72YN9
                   MOVE XO-72YN9           TO PAKKS-IO-AREA (90:10)
               END-IF
               IF  (NOT-I-24 AND I-27)
                   MOVE ORPRIS             TO XO-72YN9
                   MOVE XO-72YN9           TO PAKKS-IO-AREA (101:10)
               END-IF
               IF  (NOT-I-24 AND I-29)
                   MOVE NETPRI             TO XO-72YN9
                   MOVE XO-72YN9           TO PAKKS-IO-AREA (101:10)
               END-IF
               MOVE FIRMA                  TO PAKKS-IO-AREA (133:3)
               MOVE ' '                    TO PAKKS-IO-AREA (136:1)
               IF  (I-59)
                   MOVE '6'                TO PAKKS-IO-AREA (137:1)
               END-IF
               IF  (NOT-I-59)
                   MOVE '7'                TO PAKKS-IO-AREA (137:1)
               END-IF
               MOVE 'ORD04'                TO PAKKS-IO-AREA (138:5)
               MOVE 'ORD110'               TO PAKKS-IO-AREA (143:6)
               MOVE 'X'                    TO PAKKS-IO-AREA (150:1)
               WRITE PAKKS-IO-AREA
           END-IF
           IF  (I-04 AND I-21 AND I-59)
               MOVE SPACES TO PAKKS-IO-AREA
               INITIALIZE PAKKS-IO-AREA
               IF  (NOT-I-24)
                   MOVE POSNR-IO           TO PAKKS-IO-AREA (3:3)
               END-IF
               IF  (NOT-I-24)
                   MOVE ALF                TO PAKKS-IO-AREA (34:3)
               END-IF
               IF  (NOT-I-24)
                   MOVE ARTNR              TO PAKKS-IO-AREA (38:20)
               END-IF
               IF  (NOT-I-95 AND NOT-I-24)
                   MOVE 'PANT/NRK.AVG./PARF.AVG. ' TO PAKKS-IO-AREA
                                                               (61:24)
               END-IF
               IF  (NOT-I-95 AND NOT-I-24)
                   MOVE 'OL. '             TO PAKKS-IO-AREA (85:4)
               END-IF
               IF  (I-95 AND NOT-I-24)
                   MOVE 'PANT/FRAKT-TILL./NRK.AVG' TO PAKKS-IO-AREA
                                                               (61:24)
               END-IF
               IF  (I-95 AND NOT-I-24)
                   MOVE '.   '             TO PAKKS-IO-AREA (85:4)
               END-IF
               IF  (NOT-I-24)
                   MOVE PRITIL             TO XO-52YN9
                   MOVE XO-52YN9           TO PAKKS-IO-AREA (92:8)
               END-IF
               IF  (NOT-I-24 AND I-29)
                   MOVE PRITUT             TO XO-72YN9
                   MOVE XO-72YN9           TO PAKKS-IO-AREA (101:10)
                   INITIALIZE PRITUT
               END-IF
               MOVE FIRMA                  TO PAKKS-IO-AREA (133:3)
               MOVE ' '                    TO PAKKS-IO-AREA (136:1)
               MOVE '6'                    TO PAKKS-IO-AREA (137:1)
               MOVE 'ORD04'                TO PAKKS-IO-AREA (138:5)
               MOVE 'ORD110'               TO PAKKS-IO-AREA (143:6)
               MOVE 'X'                    TO PAKKS-IO-AREA (150:1)
               WRITE PAKKS-IO-AREA
           END-IF
           IF  (I-04 AND I-83)
               MOVE SPACES TO DAGORDR-IO-AREA
               INITIALIZE DAGORDR-IO-AREA
               MOVE '72'                   TO DAGORDR-IO-AREA (1:2)
               MOVE FIRMA                  TO DAGORDR-IO-AREA (3:3)
               MOVE ANTB50                 TO XO-50P
               MOVE XO-50P-EF              TO DAGORDR-IO-AREA (6:3)
               INITIALIZE ANTB50
               MOVE UDATE                  TO DAGORDR-IO-AREA (9:6)
               MOVE ANTB                   TO XO-50P
               MOVE XO-50P-EF              TO DAGORDR-IO-AREA (15:3)
               INITIALIZE ANTB
               MOVE ANTR                   TO XO-50P
               MOVE XO-50P-EF              TO DAGORDR-IO-AREA (18:3)
               INITIALIZE ANTR
               MOVE BSTPRI                 TO XO-72P
               MOVE XO-72P-EF              TO DAGORDR-IO-AREA (21:5)
               MOVE RESBEL                 TO XO-72P
               MOVE XO-72P-EF              TO DAGORDR-IO-AREA (26:5)
               IF VGR < 0
                 MOVE VGR                  TO XO-50D
                 MOVE XO-50D (1:5)         TO DAGORDR-IO-AREA (31:5)
               ELSE
                 MOVE VGR                  TO XO-50U
                 MOVE XO-50U (1:5)         TO DAGORDR-IO-AREA (31:5)
               END-IF
               MOVE VGRAVD-IO              TO DAGORDR-IO-AREA (36:1)
               IF  (I-95)
                   MOVE NYAVD              TO DAGORDR-IO-AREA (36:1)
               END-IF
               MOVE ANTR50                 TO XO-50P
               MOVE XO-50P-EF              TO DAGORDR-IO-AREA (37:3)
               INITIALIZE ANTR50
               MOVE KUNDNR                 TO DAGORDR-IO-AREA (41:6)
               MOVE ORDNR                  TO DAGORDR-IO-AREA (47:6)
               IF EDBNR < 0
                 MOVE EDBNR                TO XO-70D
                 MOVE XO-70D (1:7)         TO DAGORDR-IO-AREA (53:7)
               ELSE
                 MOVE EDBNR                TO XO-70U
                 MOVE XO-70U (1:7)         TO DAGORDR-IO-AREA (53:7)
               END-IF
               MOVE DIRREG                 TO DAGORDR-IO-AREA (60:1)
               WRITE DAGORDR-IO-AREA
           END-IF
           IF  (I-86)
               MOVE SPACES TO RESTORD-IO-AREA
               INITIALIZE RESTORD-IO-AREA
               MOVE '1'                    TO RESTORD-IO-AREA (1:1)
               MOVE FIRMA                  TO RESTORD-IO-AREA (2:3)
               MOVE KUNDNR                 TO RESTORD-IO-AREA (5:6)
               MOVE ORDNR                  TO RESTORD-IO-AREA (14:6)
               MOVE NYAVD                  TO RESTORD-IO-AREA (20:1)
               MOVE REKVNR                 TO RESTORD-IO-AREA (21:15)
               IF  (I-53)
                   MOVE VAADR1             TO RESTORD-IO-AREA (36:30)
               END-IF
               IF  (I-53)
                   MOVE VAADR4             TO RESTORD-IO-AREA (66:20)
               END-IF
               MOVE ORDAT3                 TO XO-60P
               MOVE XO-60P-EF              TO RESTORD-IO-AREA (96:4)
               WRITE RESTORD-IO-AREA
           END-IF
           IF  (I-87)
               MOVE SPACES TO RESTORD-IO-AREA
               INITIALIZE RESTORD-IO-AREA
               MOVE '2'                    TO RESTORD-IO-AREA (1:1)
               MOVE FIRMA                  TO RESTORD-IO-AREA (2:3)
               MOVE KUNDNR                 TO RESTORD-IO-AREA (5:6)
               MOVE ORDNR                  TO RESTORD-IO-AREA (14:6)
               MOVE NYAVD                  TO RESTORD-IO-AREA (20:1)
               IF  (I-53)
                   MOVE VAADR2             TO RESTORD-IO-AREA (21:30)
               END-IF
               IF  (I-53)
                   MOVE VAADR3             TO RESTORD-IO-AREA (51:30)
               END-IF
               MOVE ORDAT3                 TO XO-60P
               MOVE XO-60P-EF              TO RESTORD-IO-AREA (96:4)
               WRITE RESTORD-IO-AREA
           END-IF
           IF  (I-84)
               MOVE SPACES TO RESTORD-IO-AREA
               INITIALIZE RESTORD-IO-AREA
               MOVE '3'                    TO RESTORD-IO-AREA (1:1)
               MOVE FIRMA                  TO RESTORD-IO-AREA (2:3)
               MOVE KUNDNR                 TO RESTORD-IO-AREA (5:6)
               MOVE POSNR-IO               TO RESTORD-IO-AREA (11:3)
               MOVE ORDNR                  TO RESTORD-IO-AREA (14:6)
               MOVE NYAVD                  TO RESTORD-IO-AREA (20:1)
               IF EDBNR < 0
                 MOVE EDBNR                TO XO-70D
                 MOVE XO-70D (1:7)         TO RESTORD-IO-AREA (21:7)
               ELSE
                 MOVE EDBNR                TO XO-70U
                 MOVE XO-70U (1:7)         TO RESTORD-IO-AREA (21:7)
               END-IF
               MOVE ALF                    TO RESTORD-IO-AREA (28:3)
               MOVE ARTNR                  TO RESTORD-IO-AREA (31:20)
               IF  (NOT-I-18)
                   MOVE RESANT             TO XO-52P
                   MOVE XO-52P-EF          TO RESTORD-IO-AREA (51:4)
               END-IF
               IF  (I-18)
                   MOVE ANTBES             TO XO-52P
                   MOVE XO-52P-EF          TO RESTORD-IO-AREA (51:4)
               END-IF
               MOVE ORPRIS                 TO XO-72P
               MOVE XO-72P-EF              TO RESTORD-IO-AREA (55:5)
               MOVE ORRAB1                 TO XO-21P
               MOVE XO-21P-EF              TO RESTORD-IO-AREA (60:2)
               MOVE ORRAB2                 TO XO-21P
               MOVE XO-21P-EF              TO RESTORD-IO-AREA (62:2)
               MOVE ORRAB3                 TO XO-21P
               MOVE XO-21P-EF              TO RESTORD-IO-AREA (64:2)
               MOVE LAGER                  TO RESTORD-IO-AREA (66:2)
               MOVE BK                     TO RESTORD-IO-AREA (68:1)
               MOVE HND                    TO RESTORD-IO-AREA (69:3)
               MOVE NOREST                 TO RESTORD-IO-AREA (72:1)
               MOVE ORDMOT                 TO RESTORD-IO-AREA (73:2)
               MOVE FRITT                  TO RESTORD-IO-AREA (75:1)
               MOVE BETM                   TO RESTORD-IO-AREA (76:2)
               MOVE ORDAT3                 TO XO-60P
               MOVE XO-60P-EF              TO RESTORD-IO-AREA (96:4)
               MOVE VARBET                 TO RESTORD-IO-AREA (101:30)
               WRITE RESTORD-IO-AREA
           END-IF
           IF  (I-04 AND I-33)
               MOVE SPACES TO AVIKSUM-IO-AREA
               INITIALIZE AVIKSUM-IO-AREA
               MOVE FIRMA                  TO AVIKSUM-IO-AREA (1:3)
               MOVE KUNDNR                 TO AVIKSUM-IO-AREA (4:6)
               MOVE ORDMOT                 TO AVIKSUM-IO-AREA (10:2)
               IF VGR < 0
                 MOVE VGR                  TO XO-50D
                 MOVE XO-50D (1:5)         TO AVIKSUM-IO-AREA (12:5)
               ELSE
                 MOVE VGR                  TO XO-50U
                 MOVE XO-50U (1:5)         TO AVIKSUM-IO-AREA (12:5)
               END-IF
               MOVE ORDNR                  TO AVIKSUM-IO-AREA (17:6)
               MOVE ALF                    TO AVIKSUM-IO-AREA (23:3)
               MOVE ARTNR                  TO AVIKSUM-IO-AREA (26:20)
               MOVE VARB20                 TO AVIKSUM-IO-AREA (46:20)
               MOVE ANTBES                 TO XO-52U
               MOVE XO-52U (1:7)           TO AVIKSUM-IO-AREA (66:7)
               MOVE ORPRIS                 TO XO-72U
               MOVE XO-72U (1:9)           TO AVIKSUM-IO-AREA (73:9)
               IF RGPRIS < 0
                 MOVE RGPRIS               TO XO-72D
                 MOVE XO-72D (1:9)         TO AVIKSUM-IO-AREA (82:9)
               ELSE
                 MOVE RGPRIS               TO XO-72U
                 MOVE XO-72U (1:9)         TO AVIKSUM-IO-AREA (82:9)
               END-IF
               MOVE ORRAB1                 TO XO-21U
               MOVE XO-21U (1:3)           TO AVIKSUM-IO-AREA (91:3)
               IF ORRAB2 < 0
                 MOVE ORRAB2               TO XO-21D
                 MOVE XO-21D (1:3)         TO AVIKSUM-IO-AREA (94:3)
               ELSE
                 MOVE ORRAB2               TO XO-21U
                 MOVE XO-21U (1:3)         TO AVIKSUM-IO-AREA (94:3)
               END-IF
               IF ORRAB3 < 0
                 MOVE ORRAB3               TO XO-21D
                 MOVE XO-21D (1:3)         TO AVIKSUM-IO-AREA (97:3)
               ELSE
                 MOVE ORRAB3               TO XO-21U
                 MOVE XO-21U (1:3)         TO AVIKSUM-IO-AREA (97:3)
               END-IF
               IF RGRAB1 < 0
                 MOVE RGRAB1               TO XO-21D
                 MOVE XO-21D (1:3)         TO AVIKSUM-IO-AREA (100:3)
               ELSE
                 MOVE RGRAB1               TO XO-21U
                 MOVE XO-21U (1:3)         TO AVIKSUM-IO-AREA (100:3)
               END-IF
               IF RGRAB2 < 0
                 MOVE RGRAB2               TO XO-21D
                 MOVE XO-21D (1:3)         TO AVIKSUM-IO-AREA (103:3)
               ELSE
                 MOVE RGRAB2               TO XO-21U
                 MOVE XO-21U (1:3)         TO AVIKSUM-IO-AREA (103:3)
               END-IF
               IF RGRAB3 < 0
                 MOVE RGRAB3               TO XO-21D
                 MOVE XO-21D (1:3)         TO AVIKSUM-IO-AREA (106:3)
               ELSE
                 MOVE RGRAB3               TO XO-21U
                 MOVE XO-21U (1:3)         TO AVIKSUM-IO-AREA (106:3)
               END-IF
               IF  (I-19)
                   MOVE 'S'                TO AVIKSUM-IO-AREA (109:1)
               END-IF
               IF  (I-25)
                   MOVE 'N'                TO AVIKSUM-IO-AREA (109:1)
               END-IF
               IF  (I-18)
                   MOVE 'R'                TO AVIKSUM-IO-AREA (109:1)
               END-IF
               MOVE NYAVD                  TO AVIKSUM-IO-AREA (110:1)
               MOVE 'D'                    TO AVIKSUM-IO-AREA (111:1)
               MOVE NETFAK                 TO XO-14P
               MOVE XO-14P-EF              TO AVIKSUM-IO-AREA (112:3)
               IF KOSPRI < 0
                 MOVE KOSPRI               TO XO-72D
                 MOVE XO-72D (1:9)         TO AVIKSUM-IO-AREA (115:9)
               ELSE
                 MOVE KOSPRI               TO XO-72U
                 MOVE XO-72U (1:9)         TO AVIKSUM-IO-AREA (115:9)
               END-IF
               MOVE KNAVN1                 TO AVIKSUM-IO-AREA (124:30)
               MOVE KNAVN2                 TO AVIKSUM-IO-AREA (154:30)
      *                        ORDMO2   185
               IF  (I-76)
                   MOVE 'R'                TO AVIKSUM-IO-AREA (186:1)
               END-IF
               WRITE AVIKSUM-IO-AREA
           END-IF
           IF  (I-91 AND I-90)
               MOVE SPACES TO FMRAPP-IO-AREA
               INITIALIZE FMRAPP-IO-AREA
               MOVE FINAVN                 TO FMRAPP-IO-AREA (1:30)
               MOVE 'KONTROLL-LISTE  FERDIGME' TO FMRAPP-IO-AREA
                                                               (37:24)
               MOVE 'LDINGER / KORREKSJONER' TO FMRAPP-IO-AREA (61:22)
               MOVE 'FERDIGMELDINGSDATO'   TO FMRAPP-IO-AREA (93:18)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO FMRAPP-IO-AREA (113:8)
               MOVE FIRMA                  TO FMRAPP-IO-AREA (133:3)
               MOVE 'F'                    TO FMRAPP-IO-AREA (136:1)
               MOVE '7'                    TO FMRAPP-IO-AREA (137:1)
               MOVE 'ORD06'                TO FMRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO FMRAPP-IO-AREA (143:6)
               MOVE 'A'                    TO FMRAPP-IO-AREA (150:1)
               WRITE FMRAPP-IO-AREA
               MOVE SPACES TO FMRAPP-IO-AREA
               INITIALIZE FMRAPP-IO-AREA
               MOVE 'ORDRENR  ORDREDATO  RESK' TO FMRAPP-IO-AREA (1:24)
               MOVE 'NR PL POS LAGLOC  ALFA  ' TO FMRAPP-IO-AREA
                                                               (25:24)
               MOVE 'ARTIKKELNR'           TO FMRAPP-IO-AREA (49:10)
               MOVE 'LEV FØR KORR  ANT TIL' TO FMRAPP-IO-AREA (70:21)
               MOVE 'FAKT    KORR VERDI'   TO FMRAPP-IO-AREA (92:18)
               MOVE '**   MERKNADER   **'  TO FMRAPP-IO-AREA (114:19)
               MOVE FIRMA                  TO FMRAPP-IO-AREA (133:3)
               MOVE ' '                    TO FMRAPP-IO-AREA (136:1)
               MOVE '6'                    TO FMRAPP-IO-AREA (137:1)
               MOVE 'ORD06'                TO FMRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO FMRAPP-IO-AREA (143:6)
               MOVE 'B'                    TO FMRAPP-IO-AREA (150:1)
               WRITE FMRAPP-IO-AREA
               MOVE SPACES TO FMRAPP-IO-AREA
               INITIALIZE FMRAPP-IO-AREA
               MOVE '------------------------' TO FMRAPP-IO-AREA (1:24)
               MOVE '------------------------' TO FMRAPP-IO-AREA
                                                               (25:24)
               MOVE '------------------------' TO FMRAPP-IO-AREA
                                                               (49:24)
               MOVE '------------------------' TO FMRAPP-IO-AREA
                                                               (73:24)
               MOVE '------------------------' TO FMRAPP-IO-AREA
                                                               (97:24)
               MOVE '-------------------'  TO FMRAPP-IO-AREA (114:19)
               MOVE FIRMA                  TO FMRAPP-IO-AREA (133:3)
               MOVE ' '                    TO FMRAPP-IO-AREA (136:1)
               MOVE '6'                    TO FMRAPP-IO-AREA (137:1)
               MOVE 'ORD06'                TO FMRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO FMRAPP-IO-AREA (143:6)
               MOVE 'C'                    TO FMRAPP-IO-AREA (150:1)
               WRITE FMRAPP-IO-AREA
           END-IF
           IF  (I-90 AND I-55 AND NOT-I-12)
               MOVE SPACES TO FMRAPP-IO-AREA
               INITIALIZE FMRAPP-IO-AREA
               IF  (I-92)
                   MOVE ORDNR              TO FMRAPP-IO-AREA (2:6)
               END-IF
               IF  (I-92)
                   MOVE ORDATO             TO EDIT-DATE
                   MOVE EDIT-DATE (7:8)    TO FMRAPP-IO-AREA (11:8)
               END-IF
               IF  (I-92)
                   MOVE KUNDNR             TO FMRAPP-IO-AREA (21:6)
               END-IF
               IF  (I-92)
                   MOVE PLUKAV             TO FMRAPP-IO-AREA (28:2)
               END-IF
               MOVE POSNR-IO               TO FMRAPP-IO-AREA (31:3)
               MOVE LAGLOC                 TO FMRAPP-IO-AREA (35:6)
               MOVE ALF                    TO FMRAPP-IO-AREA (44:3)
               MOVE ARTNR                  TO FMRAPP-IO-AREA (49:20)
               IF  (I-54)
                   MOVE ANTLF              TO XO-72YY9
                   MOVE XO-72YY9           TO FMRAPP-IO-AREA (69:12)
                   INITIALIZE ANTLF
               END-IF
               IF  (I-54)
                   MOVE ANTLEV             TO XO-52YY9
                   MOVE XO-52YY9           TO FMRAPP-IO-AREA (86:9)
               END-IF
               IF  (I-54)
                   MOVE VDIFF2             TO XO-72YYZR
                   MOVE XO-72YYZR          TO FMRAPP-IO-AREA (98:13)
                   INITIALIZE VDIFF2
               END-IF
               IF  (I-54)
                   MOVE EDBNR              TO XO-70P
                   MOVE XO-70P-EF          TO FMRAPP-IO-AREA (111:4)
               END-IF
               IF  (I-54)
                   MOVE '*  KORRIGERT     *' TO FMRAPP-IO-AREA (115:18)
               END-IF
               IF  (I-99)
                   MOVE ORPRIS             TO XO-72YY9
                   MOVE XO-72YY9           TO FMRAPP-IO-AREA (120:12)
               END-IF
               IF  (I-99)
                   MOVE '* FRAKT '         TO FMRAPP-IO-AREA (115:8)
               END-IF
               IF  (I-99)
                   MOVE '*'                TO FMRAPP-IO-AREA (132:1)
               END-IF
               IF  (I-56)
                   MOVE '*  KORR 0 REST   *' TO FMRAPP-IO-AREA (115:18)
               END-IF
               IF  (NOT-I-28 AND NOT-I-54)
                   MOVE '*  NULL ER LEV.  *' TO FMRAPP-IO-AREA (115:18)
               END-IF
               IF  (NOT-I-28 AND I-54)
                   MOVE '*  KORR TIL NULL *' TO FMRAPP-IO-AREA (115:18)
               END-IF
               MOVE FIRMA                  TO FMRAPP-IO-AREA (133:3)
               MOVE ' '                    TO FMRAPP-IO-AREA (136:1)
               MOVE '1'                    TO FMRAPP-IO-AREA (137:1)
               MOVE 'ORD06'                TO FMRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO FMRAPP-IO-AREA (143:6)
               MOVE 'V'                    TO FMRAPP-IO-AREA (149:1)
               MOVE 'X'                    TO FMRAPP-IO-AREA (150:1)
               WRITE FMRAPP-IO-AREA
           END-IF
           IF  (I-L2)
               MOVE SPACES TO ORDRAPP-IO-AREA
               INITIALIZE ORDRAPP-IO-AREA
               MOVE FINAVN                 TO ORDRAPP-IO-AREA (1:30)
               MOVE 'DAGLIG ORDRERAPPORT.' TO ORDRAPP-IO-AREA (49:20)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO ORDRAPP-IO-AREA (113:8)
               MOVE FIRMA                  TO ORDRAPP-IO-AREA (133:3)
               MOVE 'F'                    TO ORDRAPP-IO-AREA (136:1)
               MOVE '7'                    TO ORDRAPP-IO-AREA (137:1)
               MOVE 'ORD12'                TO ORDRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO ORDRAPP-IO-AREA (143:6)
               MOVE 'A'                    TO ORDRAPP-IO-AREA (150:1)
               WRITE ORDRAPP-IO-AREA
               MOVE SPACES TO ORDRAPP-IO-AREA
               INITIALIZE ORDRAPP-IO-AREA
               MOVE 'ORDRENR  ORDREDATO  RESK' TO ORDRAPP-IO-AREA
                                                                (1:24)
               MOVE 'NR  KUNDENAVN'        TO ORDRAPP-IO-AREA (25:13)
               MOVE 'TERM  SIGN  ORDRETYPE' TO ORDRAPP-IO-AREA (61:21)
               MOVE 'ORDRESUM'             TO ORDRAPP-IO-AREA (93:8)
               MOVE 'MERKNADER'            TO ORDRAPP-IO-AREA (113:9)
               MOVE FIRMA                  TO ORDRAPP-IO-AREA (133:3)
               MOVE ' '                    TO ORDRAPP-IO-AREA (136:1)
               MOVE '6'                    TO ORDRAPP-IO-AREA (137:1)
               MOVE 'ORD12'                TO ORDRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO ORDRAPP-IO-AREA (143:6)
               MOVE 'B'                    TO ORDRAPP-IO-AREA (150:1)
               WRITE ORDRAPP-IO-AREA
               MOVE SPACES TO ORDRAPP-IO-AREA
               INITIALIZE ORDRAPP-IO-AREA
               MOVE '------------------------' TO ORDRAPP-IO-AREA
                                                                (1:24)
               MOVE '------------------------' TO ORDRAPP-IO-AREA
                                                               (25:24)
               MOVE '------------------------' TO ORDRAPP-IO-AREA
                                                               (49:24)
               MOVE '------------------------' TO ORDRAPP-IO-AREA
                                                               (73:24)
               MOVE '------------------------' TO ORDRAPP-IO-AREA
                                                               (97:24)
               MOVE '-------------------'  TO ORDRAPP-IO-AREA (114:19)
               MOVE FIRMA                  TO ORDRAPP-IO-AREA (133:3)
               MOVE ' '                    TO ORDRAPP-IO-AREA (136:1)
               MOVE '6'                    TO ORDRAPP-IO-AREA (137:1)
               MOVE 'ORD12'                TO ORDRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO ORDRAPP-IO-AREA (143:6)
               MOVE 'C'                    TO ORDRAPP-IO-AREA (150:1)
               WRITE ORDRAPP-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND I-44 AND NOT-I-45)
           AND (I-36 AND NOT-I-U7)
               MOVE 'N'                    TO ORDNRM-IO-AREA (11:1)
               MOVE UDAY                   TO ORDNRM-IO-AREA (12:2)
               MOVE UMONTH                 TO ORDNRM-IO-AREA (14:2)
               MOVE UYEAR                  TO ORDNRM-IO-AREA (16:2)
      * DNRM  T        L1 44N45
      *      AND       37 12NU7
      *                                  11 "N"
      *                        UDAY  X   13
      *                        UMONTHX   15
      *                        UYEAR X   17
               REWRITE ORDNRM-IO-AREA
                   INVALID KEY
                       DISPLAY 'Bad REWRITE - file = ORDNRM'
               END-REWRITE
           END-IF
           IF  (I-L1 AND I-21 AND I-29)
               MOVE SPACES TO PAKKS-IO-AREA
               INITIALIZE PAKKS-IO-AREA
               MOVE '----------'           TO PAKKS-IO-AREA (101:10)
               MOVE FIRMA                  TO PAKKS-IO-AREA (133:3)
               MOVE ' '                    TO PAKKS-IO-AREA (136:1)
               MOVE '6'                    TO PAKKS-IO-AREA (137:1)
               MOVE 'ORD04'                TO PAKKS-IO-AREA (138:5)
               MOVE 'ORD110'               TO PAKKS-IO-AREA (143:6)
               MOVE 'X'                    TO PAKKS-IO-AREA (150:1)
               WRITE PAKKS-IO-AREA
               MOVE SPACES TO PAKKS-IO-AREA
               INITIALIZE PAKKS-IO-AREA
               IF  (NOT-I-97)
                   MOVE 'SUM EKSKL. MOMS'  TO PAKKS-IO-AREA (70:15)
               END-IF
               IF  (I-97)
                   MOVE 'T O T A L'        TO PAKKS-IO-AREA (70:9)
               END-IF
               MOVE NETSUM                 TO XO-72YN9
               MOVE XO-72YN9               TO PAKKS-IO-AREA (101:10)
               IF  (I-78 AND I-97)
                   MOVE TOTBEL             TO XO-72YN9
                   MOVE XO-72YN9           TO PAKKS-IO-AREA (101:10)
                   INITIALIZE TOTBEL
               END-IF
               MOVE FIRMA                  TO PAKKS-IO-AREA (133:3)
               MOVE ' '                    TO PAKKS-IO-AREA (136:1)
               MOVE '6'                    TO PAKKS-IO-AREA (137:1)
               MOVE 'ORD04'                TO PAKKS-IO-AREA (138:5)
               MOVE 'ORD110'               TO PAKKS-IO-AREA (143:6)
               MOVE 'X'                    TO PAKKS-IO-AREA (150:1)
               WRITE PAKKS-IO-AREA
           END-IF
           IF  (I-L1 AND I-21 AND I-78)
           AND (NOT-I-97)
               MOVE SPACES TO PAKKS-IO-AREA
               INITIALIZE PAKKS-IO-AREA
               MOVE '+ 25 PST. MOMS'       TO PAKKS-IO-AREA (70:14)
               MOVE MOMS                   TO XO-72YN9
               MOVE XO-72YN9               TO PAKKS-IO-AREA (101:10)
               INITIALIZE MOMS
               MOVE ' '                    TO PAKKS-IO-AREA (136:1)
               MOVE FIRMA                  TO PAKKS-IO-AREA (133:3)
               MOVE '6'                    TO PAKKS-IO-AREA (137:1)
               MOVE 'ORD04'                TO PAKKS-IO-AREA (138:5)
               MOVE 'ORD110'               TO PAKKS-IO-AREA (143:6)
               MOVE 'X'                    TO PAKKS-IO-AREA (150:1)
               WRITE PAKKS-IO-AREA
               MOVE SPACES TO PAKKS-IO-AREA
               INITIALIZE PAKKS-IO-AREA
               MOVE '----------'           TO PAKKS-IO-AREA (101:10)
               MOVE FIRMA                  TO PAKKS-IO-AREA (133:3)
               MOVE ' '                    TO PAKKS-IO-AREA (136:1)
               MOVE '6'                    TO PAKKS-IO-AREA (137:1)
               MOVE 'ORD04'                TO PAKKS-IO-AREA (138:5)
               MOVE 'ORD110'               TO PAKKS-IO-AREA (143:6)
               MOVE 'X'                    TO PAKKS-IO-AREA (150:1)
               WRITE PAKKS-IO-AREA
               MOVE SPACES TO PAKKS-IO-AREA
               INITIALIZE PAKKS-IO-AREA
               MOVE 'T O T A L'            TO PAKKS-IO-AREA (70:9)
               MOVE TOTBEL                 TO XO-72YN9
               MOVE XO-72YN9               TO PAKKS-IO-AREA (101:10)
               INITIALIZE TOTBEL
               MOVE FIRMA                  TO PAKKS-IO-AREA (133:3)
               MOVE ' '                    TO PAKKS-IO-AREA (136:1)
               MOVE '6'                    TO PAKKS-IO-AREA (137:1)
               MOVE 'ORD04'                TO PAKKS-IO-AREA (138:5)
               MOVE 'ORD110'               TO PAKKS-IO-AREA (143:6)
               MOVE 'X'                    TO PAKKS-IO-AREA (150:1)
               WRITE PAKKS-IO-AREA
           END-IF
           IF  (I-L1 AND I-21)
               MOVE SPACES TO PAKKS-IO-AREA
               INITIALIZE PAKKS-IO-AREA
               MOVE FORSM                  TO PAKKS-IO-AREA (8:15)
               MOVE TIDSP                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO PAKKS-IO-AREA (26:8)
               MOVE ANTEL                  TO XO-40YN9
               MOVE XO-40YN9               TO PAKKS-IO-AREA (84:4)
               MOVE FIRMA                  TO PAKKS-IO-AREA (133:3)
               MOVE 'K'                    TO PAKKS-IO-AREA (136:1)
               MOVE '1'                    TO PAKKS-IO-AREA (137:1)
               MOVE 'ORD04'                TO PAKKS-IO-AREA (138:5)
               MOVE 'ORD110'               TO PAKKS-IO-AREA (143:6)
               MOVE 'X'                    TO PAKKS-IO-AREA (150:1)
               WRITE PAKKS-IO-AREA
           END-IF
           IF  (I-L2 AND NOT-I-80 AND NOT-I-U8)
               MOVE FAVLDM                 TO XO-70P
               MOVE XO-70P-EF              TO FIRMAF-IO-AREA (787:4)
               MOVE FAORDM                 TO XO-50P
               MOVE XO-50P-EF              TO FIRMAF-IO-AREA (791:3)
               MOVE FADODM                 TO XO-50P
               MOVE XO-50P-EF              TO FIRMAF-IO-AREA (901:3)
               REWRITE FIRMAF-IO-AREA
                   INVALID KEY
                       DISPLAY 'Bad REWRITE - file = FIRMAF'
               END-REWRITE
           END-IF
           IF  (I-L2 AND I-60)
               MOVE SPACES TO AVIKSUM-IO-AREA
               INITIALIZE AVIKSUM-IO-AREA
               MOVE FIRMA                  TO AVIKSUM-IO-AREA (1:3)
               MOVE 'AVDELING 0'           TO AVIKSUM-IO-AREA (14:10)
               MOVE AOD-ENTRY (1)          TO AVIKSUM-IO-AREA (25:5)
               MOVE ALD-ENTRY (1)          TO AVIKSUM-IO-AREA (30:5)
               MOVE ALR-ENTRY (1)          TO AVIKSUM-IO-AREA (35:5)
               MOVE TOB-ENTRY (1)          TO AVIKSUM-IO-AREA (40:9)
               MOVE TRB-ENTRY (1)          TO AVIKSUM-IO-AREA (49:9)
               MOVE UDATE                  TO AVIKSUM-IO-AREA (58:6)
               MOVE AOK-ENTRY (1)          TO AVIKSUM-IO-AREA (64:5)
               MOVE ALK-ENTRY (1)          TO AVIKSUM-IO-AREA (69:5)
               MOVE TKB-ENTRY (1)          TO AVIKSUM-IO-AREA (74:9)
               MOVE TRO-ENTRY (1)          TO AVIKSUM-IO-AREA (83:9)
               MOVE TRK-ENTRY (1)          TO AVIKSUM-IO-AREA (92:9)
               MOVE '0'                    TO AVIKSUM-IO-AREA (110:1)
               MOVE 'T'                    TO AVIKSUM-IO-AREA (111:1)
               MOVE ARO (1)                TO XO-72P
               MOVE XO-72P-EF              TO AVIKSUM-IO-AREA (121:5)
               MOVE ARK (1)                TO XO-72P
               MOVE XO-72P-EF              TO AVIKSUM-IO-AREA (126:5)
               WRITE AVIKSUM-IO-AREA
           END-IF
           IF  (I-L2 AND I-61)
               MOVE SPACES TO AVIKSUM-IO-AREA
               INITIALIZE AVIKSUM-IO-AREA
               MOVE FIRMA                  TO AVIKSUM-IO-AREA (1:3)
               MOVE 'AVDELING 1'           TO AVIKSUM-IO-AREA (14:10)
               MOVE AOD-ENTRY (2)          TO AVIKSUM-IO-AREA (25:5)
               MOVE ALD-ENTRY (2)          TO AVIKSUM-IO-AREA (30:5)
               MOVE ALR-ENTRY (2)          TO AVIKSUM-IO-AREA (35:5)
               MOVE TOB-ENTRY (2)          TO AVIKSUM-IO-AREA (40:9)
               MOVE TRB-ENTRY (2)          TO AVIKSUM-IO-AREA (49:9)
               MOVE UDATE                  TO AVIKSUM-IO-AREA (58:6)
               MOVE AOK-ENTRY (2)          TO AVIKSUM-IO-AREA (64:5)
               MOVE ALK-ENTRY (2)          TO AVIKSUM-IO-AREA (69:5)
               MOVE TKB-ENTRY (2)          TO AVIKSUM-IO-AREA (74:9)
               MOVE TRO-ENTRY (2)          TO AVIKSUM-IO-AREA (83:9)
               MOVE TRK-ENTRY (2)          TO AVIKSUM-IO-AREA (92:9)
               MOVE '1'                    TO AVIKSUM-IO-AREA (110:1)
               MOVE 'T'                    TO AVIKSUM-IO-AREA (111:1)
               MOVE ARO (2)                TO XO-72P
               MOVE XO-72P-EF              TO AVIKSUM-IO-AREA (121:5)
               MOVE ARK (2)                TO XO-72P
               MOVE XO-72P-EF              TO AVIKSUM-IO-AREA (126:5)
               WRITE AVIKSUM-IO-AREA
           END-IF
           IF  (I-L2 AND I-62)
               MOVE SPACES TO AVIKSUM-IO-AREA
               INITIALIZE AVIKSUM-IO-AREA
               MOVE FIRMA                  TO AVIKSUM-IO-AREA (1:3)
               MOVE 'AVDELING 2'           TO AVIKSUM-IO-AREA (14:10)
               MOVE AOD-ENTRY (3)          TO AVIKSUM-IO-AREA (25:5)
               MOVE ALD-ENTRY (3)          TO AVIKSUM-IO-AREA (30:5)
               MOVE ALR-ENTRY (3)          TO AVIKSUM-IO-AREA (35:5)
               MOVE TOB-ENTRY (3)          TO AVIKSUM-IO-AREA (40:9)
               MOVE TRB-ENTRY (3)          TO AVIKSUM-IO-AREA (49:9)
               MOVE UDATE                  TO AVIKSUM-IO-AREA (58:6)
               MOVE AOK-ENTRY (3)          TO AVIKSUM-IO-AREA (64:5)
               MOVE ALK-ENTRY (3)          TO AVIKSUM-IO-AREA (69:5)
               MOVE TKB-ENTRY (3)          TO AVIKSUM-IO-AREA (74:9)
               MOVE TRO-ENTRY (3)          TO AVIKSUM-IO-AREA (83:9)
               MOVE TRK-ENTRY (3)          TO AVIKSUM-IO-AREA (92:9)
               MOVE '2'                    TO AVIKSUM-IO-AREA (110:1)
               MOVE 'T'                    TO AVIKSUM-IO-AREA (111:1)
               MOVE ARO (3)                TO XO-72P
               MOVE XO-72P-EF              TO AVIKSUM-IO-AREA (121:5)
               MOVE ARK (3)                TO XO-72P
               MOVE XO-72P-EF              TO AVIKSUM-IO-AREA (126:5)
               WRITE AVIKSUM-IO-AREA
           END-IF
           IF  (I-L2 AND I-63)
               MOVE SPACES TO AVIKSUM-IO-AREA
               INITIALIZE AVIKSUM-IO-AREA
               MOVE FIRMA                  TO AVIKSUM-IO-AREA (1:3)
               MOVE 'AVDELING 3'           TO AVIKSUM-IO-AREA (14:10)
               MOVE AOD-ENTRY (4)          TO AVIKSUM-IO-AREA (25:5)
               MOVE ALD-ENTRY (4)          TO AVIKSUM-IO-AREA (30:5)
               MOVE ALR-ENTRY (4)          TO AVIKSUM-IO-AREA (35:5)
               MOVE TOB-ENTRY (4)          TO AVIKSUM-IO-AREA (40:9)
               MOVE TRB-ENTRY (4)          TO AVIKSUM-IO-AREA (49:9)
               MOVE UDATE                  TO AVIKSUM-IO-AREA (58:6)
               MOVE AOK-ENTRY (4)          TO AVIKSUM-IO-AREA (64:5)
               MOVE ALK-ENTRY (4)          TO AVIKSUM-IO-AREA (69:5)
               MOVE TKB-ENTRY (4)          TO AVIKSUM-IO-AREA (74:9)
               MOVE TRO-ENTRY (4)          TO AVIKSUM-IO-AREA (83:9)
               MOVE TRK-ENTRY (4)          TO AVIKSUM-IO-AREA (92:9)
               MOVE '3'                    TO AVIKSUM-IO-AREA (110:1)
               MOVE 'T'                    TO AVIKSUM-IO-AREA (111:1)
               MOVE ARO (4)                TO XO-72P
               MOVE XO-72P-EF              TO AVIKSUM-IO-AREA (121:5)
               MOVE ARK (4)                TO XO-72P
               MOVE XO-72P-EF              TO AVIKSUM-IO-AREA (126:5)
               WRITE AVIKSUM-IO-AREA
           END-IF
           IF  (I-L2 AND I-64)
               MOVE SPACES TO AVIKSUM-IO-AREA
               INITIALIZE AVIKSUM-IO-AREA
               MOVE FIRMA                  TO AVIKSUM-IO-AREA (1:3)
               MOVE 'AVDELING 4'           TO AVIKSUM-IO-AREA (14:10)
               MOVE AOD-ENTRY (5)          TO AVIKSUM-IO-AREA (25:5)
               MOVE ALD-ENTRY (5)          TO AVIKSUM-IO-AREA (30:5)
               MOVE ALR-ENTRY (5)          TO AVIKSUM-IO-AREA (35:5)
               MOVE TOB-ENTRY (5)          TO AVIKSUM-IO-AREA (40:9)
               MOVE TRB-ENTRY (5)          TO AVIKSUM-IO-AREA (49:9)
               MOVE UDATE                  TO AVIKSUM-IO-AREA (58:6)
               MOVE AOK-ENTRY (5)          TO AVIKSUM-IO-AREA (64:5)
               MOVE ALK-ENTRY (5)          TO AVIKSUM-IO-AREA (69:5)
               MOVE TKB-ENTRY (5)          TO AVIKSUM-IO-AREA (74:9)
               MOVE TRO-ENTRY (5)          TO AVIKSUM-IO-AREA (83:9)
               MOVE TRK-ENTRY (5)          TO AVIKSUM-IO-AREA (92:9)
               MOVE '4'                    TO AVIKSUM-IO-AREA (110:1)
               MOVE 'T'                    TO AVIKSUM-IO-AREA (111:1)
               MOVE ARO (5)                TO XO-72P
               MOVE XO-72P-EF              TO AVIKSUM-IO-AREA (121:5)
               MOVE ARK (5)                TO XO-72P
               MOVE XO-72P-EF              TO AVIKSUM-IO-AREA (126:5)
               WRITE AVIKSUM-IO-AREA
           END-IF
           IF  (I-L2 AND I-65)
               MOVE SPACES TO AVIKSUM-IO-AREA
               INITIALIZE AVIKSUM-IO-AREA
               MOVE FIRMA                  TO AVIKSUM-IO-AREA (1:3)
               MOVE 'AVDELING 5'           TO AVIKSUM-IO-AREA (14:10)
               MOVE AOD-ENTRY (6)          TO AVIKSUM-IO-AREA (25:5)
               MOVE ALD-ENTRY (6)          TO AVIKSUM-IO-AREA (30:5)
               MOVE ALR-ENTRY (6)          TO AVIKSUM-IO-AREA (35:5)
               MOVE TOB-ENTRY (6)          TO AVIKSUM-IO-AREA (40:9)
               MOVE TRB-ENTRY (6)          TO AVIKSUM-IO-AREA (49:9)
               MOVE UDATE                  TO AVIKSUM-IO-AREA (58:6)
               MOVE AOK-ENTRY (6)          TO AVIKSUM-IO-AREA (64:5)
               MOVE ALK-ENTRY (6)          TO AVIKSUM-IO-AREA (69:5)
               MOVE TKB-ENTRY (6)          TO AVIKSUM-IO-AREA (74:9)
               MOVE TRO-ENTRY (6)          TO AVIKSUM-IO-AREA (83:9)
               MOVE TRK-ENTRY (6)          TO AVIKSUM-IO-AREA (92:9)
               MOVE '5'                    TO AVIKSUM-IO-AREA (110:1)
               MOVE 'T'                    TO AVIKSUM-IO-AREA (111:1)
               MOVE ARO (6)                TO XO-72P
               MOVE XO-72P-EF              TO AVIKSUM-IO-AREA (121:5)
               MOVE ARK (6)                TO XO-72P
               MOVE XO-72P-EF              TO AVIKSUM-IO-AREA (126:5)
               WRITE AVIKSUM-IO-AREA
           END-IF
           IF  (I-L2 AND I-66)
               MOVE SPACES TO AVIKSUM-IO-AREA
               INITIALIZE AVIKSUM-IO-AREA
               MOVE FIRMA                  TO AVIKSUM-IO-AREA (1:3)
               MOVE 'AVDELING 6'           TO AVIKSUM-IO-AREA (14:10)
               MOVE AOD-ENTRY (7)          TO AVIKSUM-IO-AREA (25:5)
               MOVE ALD-ENTRY (7)          TO AVIKSUM-IO-AREA (30:5)
               MOVE ALR-ENTRY (7)          TO AVIKSUM-IO-AREA (35:5)
               MOVE TOB-ENTRY (7)          TO AVIKSUM-IO-AREA (40:9)
               MOVE TRB-ENTRY (7)          TO AVIKSUM-IO-AREA (49:9)
               MOVE UDATE                  TO AVIKSUM-IO-AREA (58:6)
               MOVE AOK-ENTRY (7)          TO AVIKSUM-IO-AREA (64:5)
               MOVE ALK-ENTRY (7)          TO AVIKSUM-IO-AREA (69:5)
               MOVE TKB-ENTRY (7)          TO AVIKSUM-IO-AREA (74:9)
               MOVE TRO-ENTRY (7)          TO AVIKSUM-IO-AREA (83:9)
               MOVE TRK-ENTRY (7)          TO AVIKSUM-IO-AREA (92:9)
               MOVE '6'                    TO AVIKSUM-IO-AREA (110:1)
               MOVE 'T'                    TO AVIKSUM-IO-AREA (111:1)
               MOVE ARO (7)                TO XO-72P
               MOVE XO-72P-EF              TO AVIKSUM-IO-AREA (121:5)
               MOVE ARK (7)                TO XO-72P
               MOVE XO-72P-EF              TO AVIKSUM-IO-AREA (126:5)
               WRITE AVIKSUM-IO-AREA
           END-IF
           IF  (I-L2 AND I-67)
               MOVE SPACES TO AVIKSUM-IO-AREA
               INITIALIZE AVIKSUM-IO-AREA
               MOVE FIRMA                  TO AVIKSUM-IO-AREA (1:3)
               MOVE 'AVDELING 7'           TO AVIKSUM-IO-AREA (14:10)
               MOVE AOD-ENTRY (8)          TO AVIKSUM-IO-AREA (25:5)
               MOVE ALD-ENTRY (8)          TO AVIKSUM-IO-AREA (30:5)
               MOVE ALR-ENTRY (8)          TO AVIKSUM-IO-AREA (35:5)
               MOVE TOB-ENTRY (8)          TO AVIKSUM-IO-AREA (40:9)
               MOVE TRB-ENTRY (8)          TO AVIKSUM-IO-AREA (49:9)
               MOVE UDATE                  TO AVIKSUM-IO-AREA (58:6)
               MOVE AOK-ENTRY (8)          TO AVIKSUM-IO-AREA (64:5)
               MOVE ALK-ENTRY (8)          TO AVIKSUM-IO-AREA (69:5)
               MOVE TKB-ENTRY (8)          TO AVIKSUM-IO-AREA (74:9)
               MOVE TRO-ENTRY (8)          TO AVIKSUM-IO-AREA (83:9)
               MOVE TRK-ENTRY (8)          TO AVIKSUM-IO-AREA (92:9)
               MOVE '7'                    TO AVIKSUM-IO-AREA (110:1)
               MOVE 'T'                    TO AVIKSUM-IO-AREA (111:1)
               MOVE ARO (8)                TO XO-72P
               MOVE XO-72P-EF              TO AVIKSUM-IO-AREA (121:5)
               MOVE ARK (8)                TO XO-72P
               MOVE XO-72P-EF              TO AVIKSUM-IO-AREA (126:5)
               WRITE AVIKSUM-IO-AREA
           END-IF
           IF  (I-L2 AND I-68)
               MOVE SPACES TO AVIKSUM-IO-AREA
               INITIALIZE AVIKSUM-IO-AREA
               MOVE FIRMA                  TO AVIKSUM-IO-AREA (1:3)
               MOVE 'AVDELING 8'           TO AVIKSUM-IO-AREA (14:10)
               MOVE AOD-ENTRY (9)          TO AVIKSUM-IO-AREA (25:5)
               MOVE ALD-ENTRY (9)          TO AVIKSUM-IO-AREA (30:5)
               MOVE ALR-ENTRY (9)          TO AVIKSUM-IO-AREA (35:5)
               MOVE TOB-ENTRY (9)          TO AVIKSUM-IO-AREA (40:9)
               MOVE TRB-ENTRY (9)          TO AVIKSUM-IO-AREA (49:9)
               MOVE UDATE                  TO AVIKSUM-IO-AREA (58:6)
               MOVE AOK-ENTRY (9)          TO AVIKSUM-IO-AREA (64:5)
               MOVE ALK-ENTRY (9)          TO AVIKSUM-IO-AREA (69:5)
               MOVE TKB-ENTRY (9)          TO AVIKSUM-IO-AREA (74:9)
               MOVE TRO-ENTRY (9)          TO AVIKSUM-IO-AREA (83:9)
               MOVE TRK-ENTRY (9)          TO AVIKSUM-IO-AREA (92:9)
               MOVE '8'                    TO AVIKSUM-IO-AREA (110:1)
               MOVE 'T'                    TO AVIKSUM-IO-AREA (111:1)
               MOVE ARO (9)                TO XO-72P
               MOVE XO-72P-EF              TO AVIKSUM-IO-AREA (121:5)
               MOVE ARK (9)                TO XO-72P
               MOVE XO-72P-EF              TO AVIKSUM-IO-AREA (126:5)
               WRITE AVIKSUM-IO-AREA
           END-IF
           IF  (I-L2 AND I-69)
               MOVE SPACES TO AVIKSUM-IO-AREA
               INITIALIZE AVIKSUM-IO-AREA
               MOVE FIRMA                  TO AVIKSUM-IO-AREA (1:3)
               MOVE 'AVDELING 9'           TO AVIKSUM-IO-AREA (14:10)
               MOVE AOD-ENTRY (10)         TO AVIKSUM-IO-AREA (25:5)
               MOVE ALD-ENTRY (10)         TO AVIKSUM-IO-AREA (30:5)
               MOVE ALR-ENTRY (10)         TO AVIKSUM-IO-AREA (35:5)
               MOVE TOB-ENTRY (10)         TO AVIKSUM-IO-AREA (40:9)
               MOVE TRB-ENTRY (10)         TO AVIKSUM-IO-AREA (49:9)
               MOVE UDATE                  TO AVIKSUM-IO-AREA (58:6)
               MOVE AOK-ENTRY (10)         TO AVIKSUM-IO-AREA (64:5)
               MOVE ALK-ENTRY (10)         TO AVIKSUM-IO-AREA (69:5)
               MOVE TKB-ENTRY (10)         TO AVIKSUM-IO-AREA (74:9)
               MOVE TRO-ENTRY (10)         TO AVIKSUM-IO-AREA (83:9)
               MOVE TRK-ENTRY (10)         TO AVIKSUM-IO-AREA (92:9)
               MOVE '9'                    TO AVIKSUM-IO-AREA (110:1)
               MOVE 'T'                    TO AVIKSUM-IO-AREA (111:1)
               MOVE ARO (10)               TO XO-72P
               MOVE XO-72P-EF              TO AVIKSUM-IO-AREA (121:5)
               MOVE ARK (10)               TO XO-72P
               MOVE XO-72P-EF              TO AVIKSUM-IO-AREA (126:5)
               WRITE AVIKSUM-IO-AREA
           END-IF
           IF  (I-L2 AND I-05)
               MOVE SPACES TO AVIKSUM-IO-AREA
               INITIALIZE AVIKSUM-IO-AREA
               MOVE FIRMA                  TO AVIKSUM-IO-AREA (1:3)
               MOVE 'AVDELING A'           TO AVIKSUM-IO-AREA (14:10)
               MOVE AOD-ENTRY (11)         TO AVIKSUM-IO-AREA (25:5)
               MOVE ALD-ENTRY (11)         TO AVIKSUM-IO-AREA (30:5)
               MOVE ALR-ENTRY (11)         TO AVIKSUM-IO-AREA (35:5)
               MOVE TOB-ENTRY (11)         TO AVIKSUM-IO-AREA (40:9)
               MOVE TRB-ENTRY (11)         TO AVIKSUM-IO-AREA (49:9)
               MOVE UDATE                  TO AVIKSUM-IO-AREA (58:6)
               MOVE AOK-ENTRY (11)         TO AVIKSUM-IO-AREA (64:5)
               MOVE ALK-ENTRY (11)         TO AVIKSUM-IO-AREA (69:5)
               MOVE TKB-ENTRY (11)         TO AVIKSUM-IO-AREA (74:9)
               MOVE TRO-ENTRY (11)         TO AVIKSUM-IO-AREA (83:9)
               MOVE TRK-ENTRY (11)         TO AVIKSUM-IO-AREA (92:9)
               MOVE 'A'                    TO AVIKSUM-IO-AREA (110:1)
               MOVE 'T'                    TO AVIKSUM-IO-AREA (111:1)
               MOVE ARO (11)               TO XO-72P
               MOVE XO-72P-EF              TO AVIKSUM-IO-AREA (121:5)
               MOVE ARK (11)               TO XO-72P
               MOVE XO-72P-EF              TO AVIKSUM-IO-AREA (126:5)
               WRITE AVIKSUM-IO-AREA
           END-IF
           IF  (I-L2 AND I-34)
               MOVE SPACES TO AVIKSUM-IO-AREA
               INITIALIZE AVIKSUM-IO-AREA
               MOVE FIRMA                  TO AVIKSUM-IO-AREA (1:3)
               MOVE 'AVDELING B'           TO AVIKSUM-IO-AREA (14:10)
               MOVE AOD-ENTRY (12)         TO AVIKSUM-IO-AREA (25:5)
               MOVE ALD-ENTRY (12)         TO AVIKSUM-IO-AREA (30:5)
               MOVE ALR-ENTRY (12)         TO AVIKSUM-IO-AREA (35:5)
               MOVE TOB-ENTRY (12)         TO AVIKSUM-IO-AREA (40:9)
               MOVE TRB-ENTRY (12)         TO AVIKSUM-IO-AREA (49:9)
               MOVE UDATE                  TO AVIKSUM-IO-AREA (58:6)
               MOVE AOK-ENTRY (12)         TO AVIKSUM-IO-AREA (64:5)
               MOVE ALK-ENTRY (12)         TO AVIKSUM-IO-AREA (69:5)
               MOVE TKB-ENTRY (12)         TO AVIKSUM-IO-AREA (74:9)
               MOVE TRO-ENTRY (12)         TO AVIKSUM-IO-AREA (83:9)
               MOVE TRK-ENTRY (12)         TO AVIKSUM-IO-AREA (92:9)
               MOVE 'B'                    TO AVIKSUM-IO-AREA (110:1)
               MOVE 'T'                    TO AVIKSUM-IO-AREA (111:1)
               MOVE ARO (12)               TO XO-72P
               MOVE XO-72P-EF              TO AVIKSUM-IO-AREA (121:5)
               MOVE ARK (12)               TO XO-72P
               MOVE XO-72P-EF              TO AVIKSUM-IO-AREA (126:5)
               WRITE AVIKSUM-IO-AREA
           END-IF
           IF  (I-L2 AND I-09)
               MOVE SPACES TO AVIKSUM-IO-AREA
               INITIALIZE AVIKSUM-IO-AREA
               MOVE FIRMA                  TO AVIKSUM-IO-AREA (1:3)
               MOVE 'AVDELING C'           TO AVIKSUM-IO-AREA (14:10)
               MOVE AOD-ENTRY (13)         TO AVIKSUM-IO-AREA (25:5)
               MOVE ALD-ENTRY (13)         TO AVIKSUM-IO-AREA (30:5)
               MOVE ALR-ENTRY (13)         TO AVIKSUM-IO-AREA (35:5)
               MOVE TOB-ENTRY (13)         TO AVIKSUM-IO-AREA (40:9)
               MOVE TRB-ENTRY (13)         TO AVIKSUM-IO-AREA (49:9)
               MOVE UDATE                  TO AVIKSUM-IO-AREA (58:6)
               MOVE AOK-ENTRY (13)         TO AVIKSUM-IO-AREA (64:5)
               MOVE ALK-ENTRY (13)         TO AVIKSUM-IO-AREA (69:5)
               MOVE TKB-ENTRY (13)         TO AVIKSUM-IO-AREA (74:9)
               MOVE TRO-ENTRY (13)         TO AVIKSUM-IO-AREA (83:9)
               MOVE TRK-ENTRY (13)         TO AVIKSUM-IO-AREA (92:9)
               MOVE 'C'                    TO AVIKSUM-IO-AREA (110:1)
               MOVE 'T'                    TO AVIKSUM-IO-AREA (111:1)
               MOVE ARO (13)               TO XO-72P
               MOVE XO-72P-EF              TO AVIKSUM-IO-AREA (121:5)
               MOVE ARK (13)               TO XO-72P
               MOVE XO-72P-EF              TO AVIKSUM-IO-AREA (126:5)
               WRITE AVIKSUM-IO-AREA
           END-IF
           IF  (I-L1 AND I-90 AND I-12)
               MOVE SPACES TO FMRAPP-IO-AREA
               INITIALIZE FMRAPP-IO-AREA
               MOVE ORDNR                  TO FMRAPP-IO-AREA (2:6)
               MOVE ORDATO                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO FMRAPP-IO-AREA (11:8)
               MOVE KUNDNR                 TO FMRAPP-IO-AREA (21:6)
               MOVE ODIFF2                 TO XO-72YYZR
               MOVE XO-72YYZR              TO FMRAPP-IO-AREA (98:13)
               MOVE '*  UTGÅR-MELDT   *'   TO FMRAPP-IO-AREA (115:18)
               MOVE FIRMA                  TO FMRAPP-IO-AREA (133:3)
               MOVE ' '                    TO FMRAPP-IO-AREA (136:1)
               MOVE '1'                    TO FMRAPP-IO-AREA (137:1)
               MOVE 'ORD06'                TO FMRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO FMRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO FMRAPP-IO-AREA (150:1)
               WRITE FMRAPP-IO-AREA
           END-IF
           IF  (I-L2 AND I-93)
               MOVE SPACES TO FMRAPP-IO-AREA
               INITIALIZE FMRAPP-IO-AREA
               MOVE '**   F I R M A T O T A L' TO FMRAPP-IO-AREA (6:24)
               MOVE FINAVN                 TO FMRAPP-IO-AREA (33:30)
               MOVE '***   DATO '          TO FMRAPP-IO-AREA (63:11)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO FMRAPP-IO-AREA (74:8)
               MOVE FIRMA                  TO FMRAPP-IO-AREA (133:3)
               MOVE 'F'                    TO FMRAPP-IO-AREA (136:1)
               MOVE '6'                    TO FMRAPP-IO-AREA (137:1)
               MOVE 'ORD06'                TO FMRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO FMRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO FMRAPP-IO-AREA (150:1)
               WRITE FMRAPP-IO-AREA
               MOVE SPACES TO FMRAPP-IO-AREA
               INITIALIZE FMRAPP-IO-AREA
               MOVE '------------------------' TO FMRAPP-IO-AREA (1:24)
               MOVE '------------------------' TO FMRAPP-IO-AREA
                                                               (25:24)
               MOVE '------------------------' TO FMRAPP-IO-AREA
                                                               (49:24)
               MOVE '---------'            TO FMRAPP-IO-AREA (73:9)
               MOVE FIRMA                  TO FMRAPP-IO-AREA (133:3)
               MOVE ' '                    TO FMRAPP-IO-AREA (136:1)
               MOVE '7'                    TO FMRAPP-IO-AREA (137:1)
               MOVE 'ORD06'                TO FMRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO FMRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO FMRAPP-IO-AREA (150:1)
               WRITE FMRAPP-IO-AREA
               MOVE SPACES TO FMRAPP-IO-AREA
               INITIALIZE FMRAPP-IO-AREA
               MOVE 'ANTALL ORDRE UTGÅRMELDT' TO FMRAPP-IO-AREA (6:23)
               MOVE FMOUT                  TO XO-60YY9
               MOVE XO-60YY9               TO FMRAPP-IO-AREA (37:7)
               INITIALIZE FMOUT
               MOVE FIRMA                  TO FMRAPP-IO-AREA (133:3)
               MOVE ' '                    TO FMRAPP-IO-AREA (136:1)
               MOVE '7'                    TO FMRAPP-IO-AREA (137:1)
               MOVE 'ORD06'                TO FMRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO FMRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO FMRAPP-IO-AREA (150:1)
               WRITE FMRAPP-IO-AREA
               MOVE SPACES TO FMRAPP-IO-AREA
               INITIALIZE FMRAPP-IO-AREA
               MOVE 'ANTALL ORDRE FERDIGMELDT' TO FMRAPP-IO-AREA (6:24)
               MOVE FMORD                  TO XO-60YY9
               MOVE XO-60YY9               TO FMRAPP-IO-AREA (37:7)
               INITIALIZE FMORD
               MOVE 'ANTALL ORDRE MED KORREK' TO FMRAPP-IO-AREA (48:23)
               MOVE 'SJONER'               TO FMRAPP-IO-AREA (71:6)
               MOVE FMOKOR                 TO XO-60YY9
               MOVE XO-60YY9               TO FMRAPP-IO-AREA (84:7)
               INITIALIZE FMOKOR
               MOVE 'KORREKSJONER I PROSENT' TO FMRAPP-IO-AREA (95:22)
               MOVE FMOPRO                 TO XO-21YY9
               MOVE XO-21YY9               TO FMRAPP-IO-AREA (119:4)
               INITIALIZE FMOPRO
               MOVE FIRMA                  TO FMRAPP-IO-AREA (133:3)
               MOVE ' '                    TO FMRAPP-IO-AREA (136:1)
               MOVE '7'                    TO FMRAPP-IO-AREA (137:1)
               MOVE 'ORD06'                TO FMRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO FMRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO FMRAPP-IO-AREA (150:1)
               WRITE FMRAPP-IO-AREA
               MOVE SPACES TO FMRAPP-IO-AREA
               INITIALIZE FMRAPP-IO-AREA
               MOVE 'ANTALL VARELINJER FERDIG' TO FMRAPP-IO-AREA (6:24)
               MOVE 'MELDT'                TO FMRAPP-IO-AREA (30:5)
               MOVE FMVLI                  TO XO-60YY9
               MOVE XO-60YY9               TO FMRAPP-IO-AREA (37:7)
               INITIALIZE FMVLI
               MOVE 'ANTALL VARELINJER MED' TO FMRAPP-IO-AREA (48:21)
               MOVE 'KORREKSJONER'         TO FMRAPP-IO-AREA (70:12)
               MOVE FMVLIK                 TO XO-60YY9
               MOVE XO-60YY9               TO FMRAPP-IO-AREA (84:7)
               INITIALIZE FMVLIK
               MOVE 'KORREKSJONER I PROSENT' TO FMRAPP-IO-AREA (95:22)
               MOVE FMVPRO                 TO XO-21YY9
               MOVE XO-21YY9               TO FMRAPP-IO-AREA (119:4)
               INITIALIZE FMVPRO
               MOVE FIRMA                  TO FMRAPP-IO-AREA (133:3)
               MOVE ' '                    TO FMRAPP-IO-AREA (136:1)
               MOVE '7'                    TO FMRAPP-IO-AREA (137:1)
               MOVE 'ORD06'                TO FMRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO FMRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO FMRAPP-IO-AREA (150:1)
               WRITE FMRAPP-IO-AREA
               MOVE SPACES TO FMRAPP-IO-AREA
               INITIALIZE FMRAPP-IO-AREA
               MOVE 'ANTALL VARELINJER MED' TO FMRAPP-IO-AREA (48:21)
               MOVE 'NULL LEVERT'          TO FMRAPP-IO-AREA (70:11)
               MOVE FMVNUL                 TO XO-60YY9
               MOVE XO-60YY9               TO FMRAPP-IO-AREA (84:7)
               INITIALIZE FMVNUL
               MOVE 'ANTALL I PROSENT'     TO FMRAPP-IO-AREA (95:16)
               MOVE FMVNPR                 TO XO-21YY9
               MOVE XO-21YY9               TO FMRAPP-IO-AREA (119:4)
               INITIALIZE FMVNPR
               MOVE FIRMA                  TO FMRAPP-IO-AREA (133:3)
               MOVE ' '                    TO FMRAPP-IO-AREA (136:1)
               MOVE '7'                    TO FMRAPP-IO-AREA (137:1)
               MOVE 'ORD06'                TO FMRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO FMRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO FMRAPP-IO-AREA (150:1)
               WRITE FMRAPP-IO-AREA
               MOVE SPACES TO FMRAPP-IO-AREA
               INITIALIZE FMRAPP-IO-AREA
               MOVE 'ANT. ORDRELINJER  FERDIG' TO FMRAPP-IO-AREA (6:24)
               MOVE 'MELDT'                TO FMRAPP-IO-AREA (30:5)
               MOVE FMVLIO                 TO XO-60YY9
               MOVE XO-60YY9               TO FMRAPP-IO-AREA (37:7)
               INITIALIZE FMVLIO
               MOVE 'SUM ORDRELINJER FERDIG' TO FMRAPP-IO-AREA (46:22)
               MOVE 'MELDT'                TO FMRAPP-IO-AREA (68:5)
               MOVE FMOSUM                 TO XO-82YY9
               MOVE XO-82YY9               TO FMRAPP-IO-AREA (78:13)
               INITIALIZE FMOSUM
               MOVE 'ORDRE VARELINJER'     TO FMRAPP-IO-AREA (95:16)
               MOVE ' MED LEVERT ANTALL'   TO FMRAPP-IO-AREA (111:18)
               MOVE FIRMA                  TO FMRAPP-IO-AREA (133:3)
               MOVE ' '                    TO FMRAPP-IO-AREA (136:1)
               MOVE '7'                    TO FMRAPP-IO-AREA (137:1)
               MOVE 'ORD06'                TO FMRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO FMRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO FMRAPP-IO-AREA (150:1)
               WRITE FMRAPP-IO-AREA
               MOVE SPACES TO FMRAPP-IO-AREA
               INITIALIZE FMRAPP-IO-AREA
               MOVE '************************' TO FMRAPP-IO-AREA (3:24)
               MOVE '************************' TO FMRAPP-IO-AREA
                                                               (27:24)
               MOVE '********************' TO FMRAPP-IO-AREA (51:20)
               MOVE FIRMA                  TO FMRAPP-IO-AREA (133:3)
               MOVE 'F'                    TO FMRAPP-IO-AREA (136:1)
               MOVE '6'                    TO FMRAPP-IO-AREA (137:1)
               MOVE 'ORD06'                TO FMRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO FMRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO FMRAPP-IO-AREA (150:1)
               WRITE FMRAPP-IO-AREA
               MOVE SPACES TO FMRAPP-IO-AREA
               INITIALIZE FMRAPP-IO-AREA
               MOVE '** TOTALER FOR KORRIGERT' TO FMRAPP-IO-AREA (3:24)
               MOVE ' ORDRESUM PR HOVEDVAREGR' TO FMRAPP-IO-AREA
                                                               (27:24)
               MOVE 'UPPE'                 TO FMRAPP-IO-AREA (51:4)
               MOVE FINAVN                 TO FMRAPP-IO-AREA (56:30)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO FMRAPP-IO-AREA (86:8)
               MOVE FIRMA                  TO FMRAPP-IO-AREA (133:3)
               MOVE ' '                    TO FMRAPP-IO-AREA (136:1)
               MOVE '6'                    TO FMRAPP-IO-AREA (137:1)
               MOVE 'ORD06'                TO FMRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO FMRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO FMRAPP-IO-AREA (150:1)
               WRITE FMRAPP-IO-AREA
               MOVE SPACES TO FMRAPP-IO-AREA
               INITIALIZE FMRAPP-IO-AREA
               MOVE '************************' TO FMRAPP-IO-AREA (3:24)
               MOVE '************************' TO FMRAPP-IO-AREA
                                                               (27:24)
               MOVE '********************' TO FMRAPP-IO-AREA (51:20)
               MOVE FIRMA                  TO FMRAPP-IO-AREA (133:3)
               MOVE ' '                    TO FMRAPP-IO-AREA (136:1)
               MOVE '8'                    TO FMRAPP-IO-AREA (137:1)
               MOVE 'ORD06'                TO FMRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO FMRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO FMRAPP-IO-AREA (150:1)
               WRITE FMRAPP-IO-AREA
               MOVE SPACES TO FMRAPP-IO-AREA
               INITIALIZE FMRAPP-IO-AREA
               MOVE 'HOVEDGRUPPE 1.'       TO FMRAPP-IO-AREA (3:14)
               MOVE 'NETTO'                TO FMRAPP-IO-AREA (36:5)
               MOVE ARF (1)                TO XO-72YY9R
               MOVE XO-72YY9R              TO FMRAPP-IO-AREA (43:13)
               INITIALIZE ARF (1)
               MOVE FIRMA                  TO FMRAPP-IO-AREA (133:3)
               MOVE ' '                    TO FMRAPP-IO-AREA (136:1)
               MOVE '6'                    TO FMRAPP-IO-AREA (137:1)
               MOVE 'ORD06'                TO FMRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO FMRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO FMRAPP-IO-AREA (150:1)
               WRITE FMRAPP-IO-AREA
               MOVE SPACES TO FMRAPP-IO-AREA
               INITIALIZE FMRAPP-IO-AREA
               MOVE 'HOVEDGRUPPE 2.'       TO FMRAPP-IO-AREA (3:14)
               MOVE 'NETTO'                TO FMRAPP-IO-AREA (36:5)
               MOVE ARF (2)                TO XO-72YY9R
               MOVE XO-72YY9R              TO FMRAPP-IO-AREA (43:13)
               INITIALIZE ARF (2)
               MOVE FIRMA                  TO FMRAPP-IO-AREA (133:3)
               MOVE ' '                    TO FMRAPP-IO-AREA (136:1)
               MOVE '6'                    TO FMRAPP-IO-AREA (137:1)
               MOVE 'ORD06'                TO FMRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO FMRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO FMRAPP-IO-AREA (150:1)
               WRITE FMRAPP-IO-AREA
               MOVE SPACES TO FMRAPP-IO-AREA
               INITIALIZE FMRAPP-IO-AREA
               MOVE 'HOVEDGRUPPE 3.'       TO FMRAPP-IO-AREA (3:14)
               MOVE 'NETTO'                TO FMRAPP-IO-AREA (36:5)
               MOVE ARF (3)                TO XO-72YY9R
               MOVE XO-72YY9R              TO FMRAPP-IO-AREA (43:13)
               INITIALIZE ARF (3)
               MOVE FIRMA                  TO FMRAPP-IO-AREA (133:3)
               MOVE ' '                    TO FMRAPP-IO-AREA (136:1)
               MOVE '6'                    TO FMRAPP-IO-AREA (137:1)
               MOVE 'ORD06'                TO FMRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO FMRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO FMRAPP-IO-AREA (150:1)
               WRITE FMRAPP-IO-AREA
               MOVE SPACES TO FMRAPP-IO-AREA
               INITIALIZE FMRAPP-IO-AREA
               MOVE 'HOVEDGRUPPE 4.'       TO FMRAPP-IO-AREA (3:14)
               MOVE 'NETTO'                TO FMRAPP-IO-AREA (36:5)
               MOVE ARF (4)                TO XO-72YY9R
               MOVE XO-72YY9R              TO FMRAPP-IO-AREA (43:13)
               INITIALIZE ARF (4)
               MOVE FIRMA                  TO FMRAPP-IO-AREA (133:3)
               MOVE ' '                    TO FMRAPP-IO-AREA (136:1)
               MOVE '6'                    TO FMRAPP-IO-AREA (137:1)
               MOVE 'ORD06'                TO FMRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO FMRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO FMRAPP-IO-AREA (150:1)
               WRITE FMRAPP-IO-AREA
               MOVE SPACES TO FMRAPP-IO-AREA
               INITIALIZE FMRAPP-IO-AREA
               MOVE 'HOVEDGRUPPE 5.'       TO FMRAPP-IO-AREA (3:14)
               MOVE 'NETTO'                TO FMRAPP-IO-AREA (36:5)
               MOVE ARF (5)                TO XO-72YY9R
               MOVE XO-72YY9R              TO FMRAPP-IO-AREA (43:13)
               INITIALIZE ARF (5)
               MOVE ' '                    TO FMRAPP-IO-AREA (136:1)
               MOVE FIRMA                  TO FMRAPP-IO-AREA (133:3)
               MOVE '6'                    TO FMRAPP-IO-AREA (137:1)
               MOVE 'ORD06'                TO FMRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO FMRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO FMRAPP-IO-AREA (150:1)
               WRITE FMRAPP-IO-AREA
               MOVE SPACES TO FMRAPP-IO-AREA
               INITIALIZE FMRAPP-IO-AREA
               MOVE 'HOVEDGRUPPE 6.'       TO FMRAPP-IO-AREA (3:14)
               MOVE 'NETTO'                TO FMRAPP-IO-AREA (36:5)
               MOVE ARF (6)                TO XO-72YY9R
               MOVE XO-72YY9R              TO FMRAPP-IO-AREA (43:13)
               INITIALIZE ARF (6)
               MOVE FIRMA                  TO FMRAPP-IO-AREA (133:3)
               MOVE ' '                    TO FMRAPP-IO-AREA (136:1)
               MOVE '6'                    TO FMRAPP-IO-AREA (137:1)
               MOVE 'ORD06'                TO FMRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO FMRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO FMRAPP-IO-AREA (150:1)
               WRITE FMRAPP-IO-AREA
               MOVE SPACES TO FMRAPP-IO-AREA
               INITIALIZE FMRAPP-IO-AREA
               MOVE 'HOVEDGRUPPE 7.'       TO FMRAPP-IO-AREA (3:14)
               MOVE 'NETTO'                TO FMRAPP-IO-AREA (36:5)
               MOVE ARF (7)                TO XO-72YY9R
               MOVE XO-72YY9R              TO FMRAPP-IO-AREA (43:13)
               INITIALIZE ARF (7)
               MOVE FIRMA                  TO FMRAPP-IO-AREA (133:3)
               MOVE ' '                    TO FMRAPP-IO-AREA (136:1)
               MOVE '6'                    TO FMRAPP-IO-AREA (137:1)
               MOVE 'ORD06'                TO FMRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO FMRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO FMRAPP-IO-AREA (150:1)
               WRITE FMRAPP-IO-AREA
               MOVE SPACES TO FMRAPP-IO-AREA
               INITIALIZE FMRAPP-IO-AREA
               MOVE 'HOVEDGRUPPE 8.'       TO FMRAPP-IO-AREA (3:14)
               MOVE 'NETTO'                TO FMRAPP-IO-AREA (36:5)
               MOVE ARF (8)                TO XO-72YY9R
               MOVE XO-72YY9R              TO FMRAPP-IO-AREA (43:13)
               INITIALIZE ARF (8)
               MOVE FIRMA                  TO FMRAPP-IO-AREA (133:3)
               MOVE ' '                    TO FMRAPP-IO-AREA (136:1)
               MOVE '6'                    TO FMRAPP-IO-AREA (137:1)
               MOVE 'ORD06'                TO FMRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO FMRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO FMRAPP-IO-AREA (150:1)
               WRITE FMRAPP-IO-AREA
               MOVE SPACES TO FMRAPP-IO-AREA
               INITIALIZE FMRAPP-IO-AREA
               MOVE 'HOVEDGRUPPE 9.'       TO FMRAPP-IO-AREA (3:14)
               MOVE 'NETTO'                TO FMRAPP-IO-AREA (36:5)
               MOVE ARF (9)                TO XO-72YY9R
               MOVE XO-72YY9R              TO FMRAPP-IO-AREA (43:13)
               INITIALIZE ARF (9)
               MOVE FIRMA                  TO FMRAPP-IO-AREA (133:3)
               MOVE ' '                    TO FMRAPP-IO-AREA (136:1)
               MOVE '6'                    TO FMRAPP-IO-AREA (137:1)
               MOVE 'ORD06'                TO FMRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO FMRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO FMRAPP-IO-AREA (150:1)
               WRITE FMRAPP-IO-AREA
           END-IF
           IF  (I-L1 AND I-49)
               MOVE SPACES TO ORDRAPP-IO-AREA
               INITIALIZE ORDRAPP-IO-AREA
               MOVE ORDNR                  TO ORDRAPP-IO-AREA (1:6)
               MOVE KTSIFF                 TO ORDRAPP-IO-AREA (8:1)
               MOVE ORDATO                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO ORDRAPP-IO-AREA (11:8)
               MOVE KUNDNR                 TO ORDRAPP-IO-AREA (21:6)
               MOVE KNAVN1                 TO ORDRAPP-IO-AREA (29:30)
               MOVE TERMID                 TO ORDRAPP-IO-AREA (61:4)
               MOVE ORDMOT                 TO ORDRAPP-IO-AREA (68:2)
               MOVE 'ORDRE          '      TO ORDRAPP-IO-AREA (73:15)
               IF  (I-43)
                   MOVE 'SALGSBILAG     '  TO ORDRAPP-IO-AREA (73:15)
               END-IF
               IF  (I-23)
                   MOVE 'KREDITBILAG    '  TO ORDRAPP-IO-AREA (73:15)
               END-IF
               IF  (I-16)
                   MOVE 'LAGEROVERFØRING'  TO ORDRAPP-IO-AREA (73:15)
               END-IF
               IF  (I-76)
                   MOVE 'RETUR TIL LEV. '  TO ORDRAPP-IO-AREA (73:15)
               END-IF
               IF  (I-46)
                   MOVE 'PROD. ORDRE.   '  TO ORDRAPP-IO-AREA (73:15)
      *                      77          87 "SALG TIL SELVK."
               END-IF
               IF  (I-77)
                   MOVE 'SALG SELVK.BK='   TO ORDRAPP-IO-AREA (73:14)
               END-IF
               IF  (I-77)
                   MOVE BK                 TO ORDRAPP-IO-AREA (87:1)
               END-IF
               IF  (NOT-I-95 AND I-17)
                   MOVE 'VERKSTEDS-ORDRE'  TO ORDRAPP-IO-AREA (73:15)
               END-IF
               IF  (I-95 AND I-17)
                   MOVE 'KOMMISJONSORDRE'  TO ORDRAPP-IO-AREA (73:15)
               END-IF
               IF  (I-18)
                   MOVE 'REST-ORDRE.    '  TO ORDRAPP-IO-AREA (73:15)
               END-IF
               IF  (I-22)
                   MOVE 'DIREKTE ORDRE  '  TO ORDRAPP-IO-AREA (73:15)
               END-IF
               MOVE NETSUM                 TO XO-72YY9R
               MOVE XO-72YY9R              TO ORDRAPP-IO-AREA (89:13)
               IF  (I-77)
                   MOVE 'BEHANDLINGSKODE =' TO ORDRAPP-IO-AREA (113:17)
               END-IF
               IF  (I-77)
                   MOVE BK                 TO ORDRAPP-IO-AREA (131:1)
               END-IF
               IF  (I-95 AND I-17)
                   MOVE 'SØNNAK UTLEV. ORDRE.' TO ORDRAPP-IO-AREA
                                                              (113:20)
               END-IF
               IF  (I-30)
                   MOVE 'PRINTET    GANGER.  ' TO ORDRAPP-IO-AREA
                                                              (113:20)
               END-IF
               IF  (I-30)
                   MOVE ANTPRT             TO XO-20YY9
                   MOVE XO-20YY9           TO ORDRAPP-IO-AREA (121:2)
               END-IF
               IF  (I-47)
                   MOVE TYPTEK             TO ORDRAPP-IO-AREA (113:16)
               END-IF
               IF  (I-47)
                   MOVE '    '             TO ORDRAPP-IO-AREA (129:4)
               END-IF
               IF  (I-48)
                   MOVE 'IKKE PRINTET.       ' TO ORDRAPP-IO-AREA
                                                              (113:20)
               END-IF
               IF  (I-81)
                   MOVE 'KREDIT-STOPP.       ' TO ORDRAPP-IO-AREA
                                                              (113:20)
               END-IF
               IF  (I-47 AND I-81)
                   MOVE 'IKKE FULLF./KR.STOPP' TO ORDRAPP-IO-AREA
                                                              (113:20)
               END-IF
               IF  (I-12)
                   MOVE 'UTGÅRMELDT.         ' TO ORDRAPP-IO-AREA
                                                              (113:20)
               END-IF
               MOVE FIRMA                  TO ORDRAPP-IO-AREA (133:3)
               MOVE ' '                    TO ORDRAPP-IO-AREA (136:1)
               MOVE '1'                    TO ORDRAPP-IO-AREA (137:1)
               MOVE 'ORD12'                TO ORDRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO ORDRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO ORDRAPP-IO-AREA (150:1)
               WRITE ORDRAPP-IO-AREA
           END-IF
           IF  (I-L2)
               MOVE SPACES TO ORDRAPP-IO-AREA
               INITIALIZE ORDRAPP-IO-AREA
               MOVE FINAVN                 TO ORDRAPP-IO-AREA (1:30)
               MOVE 'DAGLIG ORDRETOTALER.' TO ORDRAPP-IO-AREA (49:20)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO ORDRAPP-IO-AREA (113:8)
               MOVE FIRMA                  TO ORDRAPP-IO-AREA (133:3)
               MOVE 'F'                    TO ORDRAPP-IO-AREA (136:1)
               MOVE '7'                    TO ORDRAPP-IO-AREA (137:1)
               MOVE 'ORD12'                TO ORDRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO ORDRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO ORDRAPP-IO-AREA (150:1)
               WRITE ORDRAPP-IO-AREA
               MOVE SPACES TO ORDRAPP-IO-AREA
               INITIALIZE ORDRAPP-IO-AREA
               MOVE 'ANTALL'               TO ORDRAPP-IO-AREA (41:6)
               MOVE 'NTO.ORDRESUM'         TO ORDRAPP-IO-AREA (51:12)
               MOVE 'ANTALL'               TO ORDRAPP-IO-AREA (86:6)
               MOVE 'NTO.ORDRESUM'         TO ORDRAPP-IO-AREA (96:12)
               MOVE FIRMA                  TO ORDRAPP-IO-AREA (133:3)
               MOVE ' '                    TO ORDRAPP-IO-AREA (136:1)
               MOVE '6'                    TO ORDRAPP-IO-AREA (137:1)
               MOVE 'ORD12'                TO ORDRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO ORDRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO ORDRAPP-IO-AREA (150:1)
               WRITE ORDRAPP-IO-AREA
               MOVE SPACES TO ORDRAPP-IO-AREA
               INITIALIZE ORDRAPP-IO-AREA
               MOVE '------------------------' TO ORDRAPP-IO-AREA
                                                                (1:24)
               MOVE '------------------------' TO ORDRAPP-IO-AREA
                                                               (25:24)
               MOVE '------------------------' TO ORDRAPP-IO-AREA
                                                               (49:24)
               MOVE '------------------------' TO ORDRAPP-IO-AREA
                                                               (73:24)
               MOVE '------------------------' TO ORDRAPP-IO-AREA
                                                               (97:24)
               MOVE '-------------------'  TO ORDRAPP-IO-AREA (114:19)
               MOVE FIRMA                  TO ORDRAPP-IO-AREA (133:3)
               MOVE ' '                    TO ORDRAPP-IO-AREA (136:1)
               MOVE '8'                    TO ORDRAPP-IO-AREA (137:1)
               MOVE 'ORD12'                TO ORDRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO ORDRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO ORDRAPP-IO-AREA (150:1)
               WRITE ORDRAPP-IO-AREA
               MOVE SPACES TO ORDRAPP-IO-AREA
               INITIALIZE ORDRAPP-IO-AREA
               MOVE 'TOTALT REGISTRERT IDAG..' TO ORDRAPP-IO-AREA
                                                                (1:24)
               MOVE '...............'      TO ORDRAPP-IO-AREA (25:15)
               MOVE ORA (1)                TO XO-60YY9
               MOVE XO-60YY9               TO ORDRAPP-IO-AREA (40:7)
               INITIALIZE ORA (1)
               MOVE ORB (1)                TO XO-82YY9R
               MOVE XO-82YY9R              TO ORDRAPP-IO-AREA (50:14)
               INITIALIZE ORB (1)
               MOVE 'HERAV KREDITNOTA'     TO ORDRAPP-IO-AREA (67:16)
               MOVE KRA (1)                TO XO-60YY9
               MOVE XO-60YY9               TO ORDRAPP-IO-AREA (85:7)
               INITIALIZE KRA (1)
               MOVE KRB (1)                TO XO-82YY9R
               MOVE XO-82YY9R              TO ORDRAPP-IO-AREA (95:14)
               INITIALIZE KRB (1)
               MOVE FIRMA                  TO ORDRAPP-IO-AREA (133:3)
               MOVE ' '                    TO ORDRAPP-IO-AREA (136:1)
               MOVE '7'                    TO ORDRAPP-IO-AREA (137:1)
               MOVE 'ORD12'                TO ORDRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO ORDRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO ORDRAPP-IO-AREA (150:1)
               WRITE ORDRAPP-IO-AREA
               MOVE SPACES TO ORDRAPP-IO-AREA
               INITIALIZE ORDRAPP-IO-AREA
               MOVE 'HERAV DIREKTE REGISTRERT' TO ORDRAPP-IO-AREA
                                                                (1:24)
               MOVE 'E ORDRE IDAG...'      TO ORDRAPP-IO-AREA (25:15)
               MOVE ORA (7)                TO XO-60YY9
               MOVE XO-60YY9               TO ORDRAPP-IO-AREA (40:7)
               INITIALIZE ORA (7)
               MOVE ORB (7)                TO XO-82YY9R
               MOVE XO-82YY9R              TO ORDRAPP-IO-AREA (50:14)
               INITIALIZE ORB (7)
               MOVE FIRMA                  TO ORDRAPP-IO-AREA (133:3)
               MOVE ' '                    TO ORDRAPP-IO-AREA (136:1)
               MOVE '7'                    TO ORDRAPP-IO-AREA (137:1)
               MOVE 'ORD12'                TO ORDRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO ORDRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO ORDRAPP-IO-AREA (150:1)
               WRITE ORDRAPP-IO-AREA
               MOVE SPACES TO ORDRAPP-IO-AREA
               INITIALIZE ORDRAPP-IO-AREA
               MOVE 'ORDRE UTGÅRMELDT IDAG...' TO ORDRAPP-IO-AREA
                                                                (1:24)
               MOVE '...............'      TO ORDRAPP-IO-AREA (25:15)
               MOVE ORA (2)                TO XO-60YY9
               MOVE XO-60YY9               TO ORDRAPP-IO-AREA (40:7)
               INITIALIZE ORA (2)
               MOVE ORB (2)                TO XO-82YY9R
               MOVE XO-82YY9R              TO ORDRAPP-IO-AREA (50:14)
               INITIALIZE ORB (2)
               MOVE 'HERAV KREDITNOTA'     TO ORDRAPP-IO-AREA (67:16)
               MOVE KRA (2)                TO XO-60YY9
               MOVE XO-60YY9               TO ORDRAPP-IO-AREA (85:7)
               INITIALIZE KRA (2)
               MOVE KRB (2)                TO XO-82YY9R
               MOVE XO-82YY9R              TO ORDRAPP-IO-AREA (95:14)
               INITIALIZE KRB (2)
               MOVE FIRMA                  TO ORDRAPP-IO-AREA (133:3)
               MOVE ' '                    TO ORDRAPP-IO-AREA (136:1)
               MOVE '7'                    TO ORDRAPP-IO-AREA (137:1)
               MOVE 'ORD12'                TO ORDRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO ORDRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO ORDRAPP-IO-AREA (150:1)
               WRITE ORDRAPP-IO-AREA
               MOVE SPACES TO ORDRAPP-IO-AREA
               INITIALIZE ORDRAPP-IO-AREA
               MOVE 'FERDIGMELDTE ORDRE SOM I' TO ORDRAPP-IO-AREA
                                                                (1:24)
               MOVE 'KKE FAKTURERES.'      TO ORDRAPP-IO-AREA (25:15)
               MOVE ORA (3)                TO XO-60YY9
               MOVE XO-60YY9               TO ORDRAPP-IO-AREA (40:7)
               INITIALIZE ORA (3)
               MOVE ORB (3)                TO XO-82YY9R
               MOVE XO-82YY9R              TO ORDRAPP-IO-AREA (50:14)
               INITIALIZE ORB (3)
               MOVE 'HERAV KREDITNOTA'     TO ORDRAPP-IO-AREA (67:16)
               MOVE KRA (3)                TO XO-60YY9
               MOVE XO-60YY9               TO ORDRAPP-IO-AREA (85:7)
               INITIALIZE KRA (3)
               MOVE KRB (3)                TO XO-82YY9R
               MOVE XO-82YY9R              TO ORDRAPP-IO-AREA (95:14)
               INITIALIZE KRB (3)
               MOVE FIRMA                  TO ORDRAPP-IO-AREA (133:3)
               MOVE ' '                    TO ORDRAPP-IO-AREA (136:1)
               MOVE '7'                    TO ORDRAPP-IO-AREA (137:1)
               MOVE 'ORD12'                TO ORDRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO ORDRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO ORDRAPP-IO-AREA (150:1)
               WRITE ORDRAPP-IO-AREA
               MOVE SPACES TO ORDRAPP-IO-AREA
               INITIALIZE ORDRAPP-IO-AREA
               MOVE 'ORDRE FAKTURERT I DAG...' TO ORDRAPP-IO-AREA
                                                                (1:24)
               MOVE '...............'      TO ORDRAPP-IO-AREA (25:15)
               MOVE ORA (4)                TO XO-60YY9
               MOVE XO-60YY9               TO ORDRAPP-IO-AREA (40:7)
               INITIALIZE ORA (4)
               MOVE ORB (4)                TO XO-82YY9R
               MOVE XO-82YY9R              TO ORDRAPP-IO-AREA (50:14)
               INITIALIZE ORB (4)
               MOVE 'HERAV KREDITNOTA'     TO ORDRAPP-IO-AREA (67:16)
               MOVE KRA (4)                TO XO-60YY9
               MOVE XO-60YY9               TO ORDRAPP-IO-AREA (85:7)
               INITIALIZE KRA (4)
               MOVE KRB (4)                TO XO-82YY9R
               MOVE XO-82YY9R              TO ORDRAPP-IO-AREA (95:14)
               INITIALIZE KRB (4)
               MOVE FIRMA                  TO ORDRAPP-IO-AREA (133:3)
               MOVE ' '                    TO ORDRAPP-IO-AREA (136:1)
               MOVE '8'                    TO ORDRAPP-IO-AREA (137:1)
               MOVE 'ORD12'                TO ORDRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO ORDRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO ORDRAPP-IO-AREA (150:1)
               WRITE ORDRAPP-IO-AREA
               MOVE SPACES TO ORDRAPP-IO-AREA
               INITIALIZE ORDRAPP-IO-AREA
               MOVE 'TOTAL ORDREFILE PR. I DA' TO ORDRAPP-IO-AREA
                                                                (1:24)
               MOVE 'G..............'      TO ORDRAPP-IO-AREA (25:15)
               MOVE ORA (5)                TO XO-60YY9
               MOVE XO-60YY9               TO ORDRAPP-IO-AREA (40:7)
               INITIALIZE ORA (5)
               MOVE ORB (5)                TO XO-82YY9R
               MOVE XO-82YY9R              TO ORDRAPP-IO-AREA (50:14)
               INITIALIZE ORB (5)
               MOVE 'HERAV KREDITNOTA'     TO ORDRAPP-IO-AREA (67:16)
               MOVE KRA (5)                TO XO-60YY9
               MOVE XO-60YY9               TO ORDRAPP-IO-AREA (85:7)
               INITIALIZE KRA (5)
               MOVE KRB (5)                TO XO-82YY9R
               MOVE XO-82YY9R              TO ORDRAPP-IO-AREA (95:14)
               INITIALIZE KRB (5)
               MOVE FIRMA                  TO ORDRAPP-IO-AREA (133:3)
               MOVE ' '                    TO ORDRAPP-IO-AREA (136:1)
               MOVE '7'                    TO ORDRAPP-IO-AREA (137:1)
               MOVE 'ORD12'                TO ORDRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO ORDRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO ORDRAPP-IO-AREA (150:1)
               WRITE ORDRAPP-IO-AREA
               MOVE SPACES TO ORDRAPP-IO-AREA
               INITIALIZE ORDRAPP-IO-AREA
               MOVE 'HERAV IKKE FERDIGMELDTE ' TO ORDRAPP-IO-AREA
                                                                (1:24)
               MOVE 'ORDRE..........'      TO ORDRAPP-IO-AREA (25:15)
               MOVE ORA (6)                TO XO-60YY9
               MOVE XO-60YY9               TO ORDRAPP-IO-AREA (40:7)
               INITIALIZE ORA (6)
               MOVE ORB (6)                TO XO-82YY9R
               MOVE XO-82YY9R              TO ORDRAPP-IO-AREA (50:14)
               INITIALIZE ORB (6)
               MOVE 'HERAV KREDITNOTA'     TO ORDRAPP-IO-AREA (67:16)
               MOVE KRA (6)                TO XO-60YY9
               MOVE XO-60YY9               TO ORDRAPP-IO-AREA (85:7)
               INITIALIZE KRA (6)
               MOVE KRB (6)                TO XO-82YY9R
               MOVE XO-82YY9R              TO ORDRAPP-IO-AREA (95:14)
               INITIALIZE KRB (6)
               MOVE FIRMA                  TO ORDRAPP-IO-AREA (133:3)
               MOVE ' '                    TO ORDRAPP-IO-AREA (136:1)
               MOVE '7'                    TO ORDRAPP-IO-AREA (137:1)
               MOVE 'ORD12'                TO ORDRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO ORDRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO ORDRAPP-IO-AREA (150:1)
      *****************************************************************
      * NESTE LINJE PRINTES KUN FOR HAFNOR (SUMMERING AV VEKT)        *
      *****************************************************************
               WRITE ORDRAPP-IO-AREA
           END-IF
           IF  (I-L2 AND I-95)
               MOVE SPACES TO ORDRAPP-IO-AREA
               INITIALIZE ORDRAPP-IO-AREA
               MOVE 'TOTAL VEKT DAGENS FERDIG' TO ORDRAPP-IO-AREA
                                                                (1:24)
               MOVE 'MELDTE ORDRE...'      TO ORDRAPP-IO-AREA (25:15)
               MOVE VEKTL2                 TO XO-92YY9
               MOVE XO-92YY9               TO ORDRAPP-IO-AREA (49:14)
               INITIALIZE VEKTL2
               MOVE 'KG.'                  TO ORDRAPP-IO-AREA (64:3)
               MOVE FIRMA                  TO ORDRAPP-IO-AREA (133:3)
               MOVE ' '                    TO ORDRAPP-IO-AREA (136:1)
               MOVE '7'                    TO ORDRAPP-IO-AREA (137:1)
               MOVE 'ORD12'                TO ORDRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO ORDRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO ORDRAPP-IO-AREA (150:1)
               WRITE ORDRAPP-IO-AREA
           END-IF
           IF  (I-LR)
               MOVE SPACES TO ORDRAPP-IO-AREA
               INITIALIZE ORDRAPP-IO-AREA
               MOVE 'DAGLIG ORDRE GRANDTO' TO ORDRAPP-IO-AREA (49:20)
               MOVE 'TALER.'               TO ORDRAPP-IO-AREA (69:6)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO ORDRAPP-IO-AREA (113:8)
               MOVE FIRMA                  TO ORDRAPP-IO-AREA (133:3)
               MOVE 'F'                    TO ORDRAPP-IO-AREA (136:1)
               MOVE '8'                    TO ORDRAPP-IO-AREA (137:1)
               MOVE 'ORD12'                TO ORDRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO ORDRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO ORDRAPP-IO-AREA (150:1)
               WRITE ORDRAPP-IO-AREA
               MOVE SPACES TO ORDRAPP-IO-AREA
               INITIALIZE ORDRAPP-IO-AREA
               MOVE 'SJEFSLISTE'           TO ORDRAPP-IO-AREA (1:10)
               MOVE 'ANTALL'               TO ORDRAPP-IO-AREA (41:6)
               MOVE 'NTO.ORDRESUM'         TO ORDRAPP-IO-AREA (51:12)
               MOVE 'ANTALL'               TO ORDRAPP-IO-AREA (86:6)
               MOVE 'NTO.ORDRESUM'         TO ORDRAPP-IO-AREA (96:12)
               MOVE FIRMA                  TO ORDRAPP-IO-AREA (133:3)
               MOVE ' '                    TO ORDRAPP-IO-AREA (136:1)
               MOVE '6'                    TO ORDRAPP-IO-AREA (137:1)
               MOVE 'ORD12'                TO ORDRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO ORDRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO ORDRAPP-IO-AREA (150:1)
               WRITE ORDRAPP-IO-AREA
               MOVE SPACES TO ORDRAPP-IO-AREA
               INITIALIZE ORDRAPP-IO-AREA
               MOVE '------------------------' TO ORDRAPP-IO-AREA
                                                                (1:24)
               MOVE '------------------------' TO ORDRAPP-IO-AREA
                                                               (25:24)
               MOVE '------------------------' TO ORDRAPP-IO-AREA
                                                               (49:24)
               MOVE '------------------------' TO ORDRAPP-IO-AREA
                                                               (73:24)
               MOVE '------------------------' TO ORDRAPP-IO-AREA
                                                               (97:24)
               MOVE '-------------------'  TO ORDRAPP-IO-AREA (114:19)
               MOVE FIRMA                  TO ORDRAPP-IO-AREA (133:3)
               MOVE ' '                    TO ORDRAPP-IO-AREA (136:1)
               MOVE '8'                    TO ORDRAPP-IO-AREA (137:1)
               MOVE 'ORD12'                TO ORDRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO ORDRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO ORDRAPP-IO-AREA (150:1)
               WRITE ORDRAPP-IO-AREA
               MOVE SPACES TO ORDRAPP-IO-AREA
               INITIALIZE ORDRAPP-IO-AREA
               MOVE 'TOTALT REGISTRERT IDAG..' TO ORDRAPP-IO-AREA
                                                                (1:24)
               MOVE '...............'      TO ORDRAPP-IO-AREA (25:15)
               MOVE OAT (1)                TO XO-60YY9
               MOVE XO-60YY9               TO ORDRAPP-IO-AREA (40:7)
               MOVE OBT (1)                TO XO-82YY9R
               MOVE XO-82YY9R              TO ORDRAPP-IO-AREA (50:14)
               MOVE 'HERAV KREDITNOTA'     TO ORDRAPP-IO-AREA (67:16)
               MOVE KAT (1)                TO XO-60YY9
               MOVE XO-60YY9               TO ORDRAPP-IO-AREA (85:7)
               MOVE KBT (1)                TO XO-82YY9R
               MOVE XO-82YY9R              TO ORDRAPP-IO-AREA (95:14)
               MOVE FIRMA                  TO ORDRAPP-IO-AREA (133:3)
               MOVE ' '                    TO ORDRAPP-IO-AREA (136:1)
               MOVE '7'                    TO ORDRAPP-IO-AREA (137:1)
               MOVE 'ORD12'                TO ORDRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO ORDRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO ORDRAPP-IO-AREA (150:1)
               WRITE ORDRAPP-IO-AREA
               MOVE SPACES TO ORDRAPP-IO-AREA
               INITIALIZE ORDRAPP-IO-AREA
               MOVE 'HERAV DIREKTE REGISTRERT' TO ORDRAPP-IO-AREA
                                                                (1:24)
               MOVE 'E ORDRE IDAG...'      TO ORDRAPP-IO-AREA (25:15)
               MOVE OAT (7)                TO XO-60YY9
               MOVE XO-60YY9               TO ORDRAPP-IO-AREA (40:7)
               MOVE OBT (7)                TO XO-82YY9R
               MOVE XO-82YY9R              TO ORDRAPP-IO-AREA (50:14)
               MOVE FIRMA                  TO ORDRAPP-IO-AREA (133:3)
               MOVE ' '                    TO ORDRAPP-IO-AREA (136:1)
               MOVE '7'                    TO ORDRAPP-IO-AREA (137:1)
               MOVE 'ORD12'                TO ORDRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO ORDRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO ORDRAPP-IO-AREA (150:1)
               WRITE ORDRAPP-IO-AREA
               MOVE SPACES TO ORDRAPP-IO-AREA
               INITIALIZE ORDRAPP-IO-AREA
               MOVE 'ORDRE UTGÅRMELDT IDAG...' TO ORDRAPP-IO-AREA
                                                                (1:24)
               MOVE '...............'      TO ORDRAPP-IO-AREA (25:15)
               MOVE OAT (2)                TO XO-60YY9
               MOVE XO-60YY9               TO ORDRAPP-IO-AREA (40:7)
               MOVE OBT (2)                TO XO-82YY9R
               MOVE XO-82YY9R              TO ORDRAPP-IO-AREA (50:14)
               MOVE 'HERAV KREDITNOTA'     TO ORDRAPP-IO-AREA (67:16)
               MOVE KAT (2)                TO XO-60YY9
               MOVE XO-60YY9               TO ORDRAPP-IO-AREA (85:7)
               MOVE KBT (2)                TO XO-82YY9R
               MOVE XO-82YY9R              TO ORDRAPP-IO-AREA (95:14)
               MOVE FIRMA                  TO ORDRAPP-IO-AREA (133:3)
               MOVE ' '                    TO ORDRAPP-IO-AREA (136:1)
               MOVE '7'                    TO ORDRAPP-IO-AREA (137:1)
               MOVE 'ORD12'                TO ORDRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO ORDRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO ORDRAPP-IO-AREA (150:1)
               WRITE ORDRAPP-IO-AREA
               MOVE SPACES TO ORDRAPP-IO-AREA
               INITIALIZE ORDRAPP-IO-AREA
               MOVE 'FERDIGMELDTE ORDRE SOM I' TO ORDRAPP-IO-AREA
                                                                (1:24)
               MOVE 'KKE FAKTURERES.'      TO ORDRAPP-IO-AREA (25:15)
               MOVE OAT (3)                TO XO-60YY9
               MOVE XO-60YY9               TO ORDRAPP-IO-AREA (40:7)
               MOVE OBT (3)                TO XO-82YY9R
               MOVE XO-82YY9R              TO ORDRAPP-IO-AREA (50:14)
               MOVE 'HERAV KREDITNOTA'     TO ORDRAPP-IO-AREA (67:16)
               MOVE KAT (3)                TO XO-60YY9
               MOVE XO-60YY9               TO ORDRAPP-IO-AREA (85:7)
               MOVE KBT (3)                TO XO-82YY9R
               MOVE XO-82YY9R              TO ORDRAPP-IO-AREA (95:14)
               MOVE FIRMA                  TO ORDRAPP-IO-AREA (133:3)
               MOVE ' '                    TO ORDRAPP-IO-AREA (136:1)
               MOVE '7'                    TO ORDRAPP-IO-AREA (137:1)
               MOVE 'ORD12'                TO ORDRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO ORDRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO ORDRAPP-IO-AREA (150:1)
               WRITE ORDRAPP-IO-AREA
               MOVE SPACES TO ORDRAPP-IO-AREA
               INITIALIZE ORDRAPP-IO-AREA
               MOVE 'ORDRE FAKTURERT I DAG...' TO ORDRAPP-IO-AREA
                                                                (1:24)
               MOVE '...............'      TO ORDRAPP-IO-AREA (25:15)
               MOVE PSRDTO                 TO ORDRAPP-IO-AREA (26:8)
               MOVE OAT (4)                TO XO-60YY9
               MOVE XO-60YY9               TO ORDRAPP-IO-AREA (40:7)
               MOVE OBT (4)                TO XO-82YY9R
               MOVE XO-82YY9R              TO ORDRAPP-IO-AREA (50:14)
               MOVE 'HERAV KREDITNOTA'     TO ORDRAPP-IO-AREA (67:16)
               MOVE KAT (4)                TO XO-60YY9
               MOVE XO-60YY9               TO ORDRAPP-IO-AREA (85:7)
               MOVE KBT (4)                TO XO-82YY9R
               MOVE XO-82YY9R              TO ORDRAPP-IO-AREA (95:14)
               MOVE FIRMA                  TO ORDRAPP-IO-AREA (133:3)
               MOVE ' '                    TO ORDRAPP-IO-AREA (136:1)
               MOVE '8'                    TO ORDRAPP-IO-AREA (137:1)
               MOVE 'ORD12'                TO ORDRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO ORDRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO ORDRAPP-IO-AREA (150:1)
               WRITE ORDRAPP-IO-AREA
               MOVE SPACES TO ORDRAPP-IO-AREA
               INITIALIZE ORDRAPP-IO-AREA
               MOVE 'TOTAL ORDREFILE PR. I DA' TO ORDRAPP-IO-AREA
                                                                (1:24)
               MOVE 'G..............'      TO ORDRAPP-IO-AREA (25:15)
               MOVE OAT (5)                TO XO-60YY9
               MOVE XO-60YY9               TO ORDRAPP-IO-AREA (40:7)
               MOVE OBT (5)                TO XO-82YY9R
               MOVE XO-82YY9R              TO ORDRAPP-IO-AREA (50:14)
               MOVE 'HERAV KREDITNOTA'     TO ORDRAPP-IO-AREA (67:16)
               MOVE KAT (5)                TO XO-60YY9
               MOVE XO-60YY9               TO ORDRAPP-IO-AREA (85:7)
               MOVE KBT (5)                TO XO-82YY9R
               MOVE XO-82YY9R              TO ORDRAPP-IO-AREA (95:14)
               MOVE FIRMA                  TO ORDRAPP-IO-AREA (133:3)
               MOVE ' '                    TO ORDRAPP-IO-AREA (136:1)
               MOVE '2'                    TO ORDRAPP-IO-AREA (137:1)
               MOVE 'ORD12'                TO ORDRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO ORDRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO ORDRAPP-IO-AREA (150:1)
               WRITE ORDRAPP-IO-AREA
               MOVE SPACES TO ORDRAPP-IO-AREA
               INITIALIZE ORDRAPP-IO-AREA
               MOVE 'HERAV IKKE FERDIGMELDTE ' TO ORDRAPP-IO-AREA
                                                                (1:24)
               MOVE 'ORDRE..........'      TO ORDRAPP-IO-AREA (25:15)
               MOVE OAT (6)                TO XO-60YY9
               MOVE XO-60YY9               TO ORDRAPP-IO-AREA (40:7)
               MOVE OBT (6)                TO XO-82YY9R
               MOVE XO-82YY9R              TO ORDRAPP-IO-AREA (50:14)
               MOVE 'HERAV KREDITNOTA'     TO ORDRAPP-IO-AREA (67:16)
               MOVE KAT (6)                TO XO-60YY9
               MOVE XO-60YY9               TO ORDRAPP-IO-AREA (85:7)
               MOVE KBT (6)                TO XO-82YY9R
               MOVE XO-82YY9R              TO ORDRAPP-IO-AREA (95:14)
               MOVE FIRMA                  TO ORDRAPP-IO-AREA (133:3)
               MOVE ' '                    TO ORDRAPP-IO-AREA (136:1)
               MOVE '2'                    TO ORDRAPP-IO-AREA (137:1)
               MOVE 'ORD12'                TO ORDRAPP-IO-AREA (138:5)
               MOVE 'ORD110'               TO ORDRAPP-IO-AREA (143:6)
               MOVE 'X'                    TO ORDRAPP-IO-AREA (150:1)
               WRITE ORDRAPP-IO-AREA
           END-IF
           IF  (I-LR AND I-97 AND NOT-I-U8)
               MOVE UDATE                  TO AVSTEMF-IO-AREA (11:6)
               MOVE OAT-ENTRY (1)          TO AVSTEMF-IO-AREA (17:6)
               MOVE OAT-ENTRY (2)          TO AVSTEMF-IO-AREA (23:6)
               MOVE OAT-ENTRY (3)          TO AVSTEMF-IO-AREA (29:6)
               MOVE OAT-ENTRY (4)          TO AVSTEMF-IO-AREA (35:6)
               MOVE OAT-ENTRY (5)          TO AVSTEMF-IO-AREA (41:6)
               MOVE OAT-ENTRY (6)          TO AVSTEMF-IO-AREA (47:6)
               MOVE '000000'               TO AVSTEMF-IO-AREA (53:6)
               MOVE OAT-ENTRY (8)          TO AVSTEMF-IO-AREA (59:6)
               MOVE '            '         TO AVSTEMF-IO-AREA (65:12)
               MOVE '                        ' TO AVSTEMF-IO-AREA
                                                               (77:24)
               MOVE '                    ' TO AVSTEMF-IO-AREA (101:20)
               MOVE AVSGB                  TO AVSTEMF-IO-AREA (131:110)
               MOVE '2'                    TO AVSTEMF-IO-AREA (240:1)
               MOVE AVSGC                  TO AVSTEMF-IO-AREA (251:110)
               MOVE '3'                    TO AVSTEMF-IO-AREA (360:1)
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
           MOVE 1                          TO LR-CHECK
           SET ORDREFS-LEVEL-INIT          TO TRUE
           INITIALIZE ORDREFS-DATA-FIELDS
           SET ORDREFS-EOF-OFF             TO TRUE
           SET ORDREFS-PROCESS             TO TRUE
           OPEN INPUT ORDREFS
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN I-O FIRMAF
           INITIALIZE FAKPAR-DATA-FIELDS
           OPEN INPUT FAKPAR
           INITIALIZE AVSTEMF-DATA-FIELDS
           OPEN I-O AVSTEMF
           OPEN OUTPUT ORDREM
           OPEN OUTPUT DAGORDR
           OPEN OUTPUT RESTORD
           OPEN OUTPUT AVIKSUM
           OPEN OUTPUT ORDFAKT
           OPEN OUTPUT SALGANT
           OPEN OUTPUT VKASSEF
           OPEN OUTPUT UTAKORD
           OPEN I-O ORDNRM
           OPEN OUTPUT ORDRAPP
           OPEN OUTPUT FMRAPP
           OPEN OUTPUT PAKKS.
           PERFORM VARYING ARK-I FROM 1 BY 1
                     UNTIL ARK-I > ARK-MAX
               INITIALIZE ARK (ARK-I)
           END-PERFORM
           SET ARK-I                       TO 1
           PERFORM VARYING ARO-I FROM 1 BY 1
                     UNTIL ARO-I > ARO-MAX
               INITIALIZE ARO (ARO-I)
           END-PERFORM
           SET ARO-I                       TO 1
           PERFORM VARYING AOD-I FROM 1 BY 1
                     UNTIL AOD-I > AOD-MAX
               INITIALIZE AOD (AOD-I)
           END-PERFORM
           SET AOD-I                       TO 1
           PERFORM VARYING AOK-I FROM 1 BY 1
                     UNTIL AOK-I > AOK-MAX
               INITIALIZE AOK (AOK-I)
           END-PERFORM
           SET AOK-I                       TO 1
           PERFORM VARYING ALD-I FROM 1 BY 1
                     UNTIL ALD-I > ALD-MAX
               INITIALIZE ALD (ALD-I)
           END-PERFORM
           SET ALD-I                       TO 1
           PERFORM VARYING ALK-I FROM 1 BY 1
                     UNTIL ALK-I > ALK-MAX
               INITIALIZE ALK (ALK-I)
           END-PERFORM
           SET ALK-I                       TO 1
           PERFORM VARYING ALR-I FROM 1 BY 1
                     UNTIL ALR-I > ALR-MAX
               INITIALIZE ALR (ALR-I)
           END-PERFORM
           SET ALR-I                       TO 1
           PERFORM VARYING TOB-I FROM 1 BY 1
                     UNTIL TOB-I > TOB-MAX
               INITIALIZE TOB (TOB-I)
           END-PERFORM
           SET TOB-I                       TO 1
           PERFORM VARYING TKB-I FROM 1 BY 1
                     UNTIL TKB-I > TKB-MAX
               INITIALIZE TKB (TKB-I)
           END-PERFORM
           SET TKB-I                       TO 1
           PERFORM VARYING TRB-I FROM 1 BY 1
                     UNTIL TRB-I > TRB-MAX
               INITIALIZE TRB (TRB-I)
           END-PERFORM
           SET TRB-I                       TO 1
           PERFORM VARYING TRO-I FROM 1 BY 1
                     UNTIL TRO-I > TRO-MAX
               INITIALIZE TRO (TRO-I)
           END-PERFORM
           SET TRO-I                       TO 1
           PERFORM VARYING TRK-I FROM 1 BY 1
                     UNTIL TRK-I > TRK-MAX
               INITIALIZE TRK (TRK-I)
           END-PERFORM
           SET TRK-I                       TO 1
           PERFORM VARYING ARF-I FROM 1 BY 1
                     UNTIL ARF-I > ARF-MAX
               INITIALIZE ARF (ARF-I)
           END-PERFORM
           SET ARF-I                       TO 1
           PERFORM VARYING ORA-I FROM 1 BY 1
                     UNTIL ORA-I > ORA-MAX
               INITIALIZE ORA (ORA-I)
           END-PERFORM
           SET ORA-I                       TO 1
           PERFORM VARYING KRA-I FROM 1 BY 1
                     UNTIL KRA-I > KRA-MAX
               INITIALIZE KRA (KRA-I)
           END-PERFORM
           SET KRA-I                       TO 1
           PERFORM VARYING ORB-I FROM 1 BY 1
                     UNTIL ORB-I > ORB-MAX
               INITIALIZE ORB (ORB-I)
           END-PERFORM
           SET ORB-I                       TO 1
           PERFORM VARYING KRB-I FROM 1 BY 1
                     UNTIL KRB-I > KRB-MAX
               INITIALIZE KRB (KRB-I)
           END-PERFORM
           SET KRB-I                       TO 1
           PERFORM VARYING OAT-I FROM 1 BY 1
                     UNTIL OAT-I > OAT-MAX
               INITIALIZE OAT (OAT-I)
           END-PERFORM
           SET OAT-I                       TO 1
           PERFORM VARYING KAT-I FROM 1 BY 1
                     UNTIL KAT-I > KAT-MAX
               INITIALIZE KAT (KAT-I)
           END-PERFORM
           SET KAT-I                       TO 1
           PERFORM VARYING OBT-I FROM 1 BY 1
                     UNTIL OBT-I > OBT-MAX
               INITIALIZE OBT (OBT-I)
           END-PERFORM
           SET OBT-I                       TO 1
           PERFORM VARYING KBT-I FROM 1 BY 1
                     UNTIL KBT-I > KBT-MAX
               INITIALIZE KBT (KBT-I)
           END-PERFORM
           SET KBT-I                       TO 1
           INITIALIZE TEMPORARY-FIELDS
           INITIALIZE PREDEFINED-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE ORDREFS
           CLOSE FIRMAF
           CLOSE FAKPAR
           CLOSE AVSTEMF
           CLOSE ORDREM
           CLOSE DAGORDR
           CLOSE RESTORD
           CLOSE AVIKSUM
           CLOSE ORDFAKT
           CLOSE SALGANT
           CLOSE VKASSEF
           CLOSE UTAKORD
           CLOSE ORDNRM
           CLOSE ORDRAPP
           CLOSE FMRAPP
           CLOSE PAKKS.
 
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
