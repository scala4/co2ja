       IDENTIFICATION DIVISION.
       PROGRAM-ID. RKO121R.
      **********************************************  Z-WIN-RPG2P     *
      * NY VERSJON AV RKO.RKO021                 ***TXT***OK MT      *
      * PROGRAM.......: RKO021 - FØR SEPT.05-RSK021                  *
      * SAMME PROGRAM SOM RSK020, MEN HENSYNTAR FLERE FIRMAER I SHELL*         *
      * RUTINEN DE SOM ER MED:FNR. 950-965-923-983-990-942.          *         *
      * DANNE DAGENS RESKONTRORECORDS OG REGNSKAPSRECORDS            *         *
      * GRUNNLAG FOR DANNING AV RECORDENE ER TRANSFILENE FRA BILART  *         *
      * 0 - 3 OG 4 - 8  (FAKR - BOKF), SAMT RESKONTRO OG REGN.POST   *  ER     *
      * FRA FAKTURERING                                              *         *
      * TILLEGG TIL DETTE KOMMER GAMLE RESK.POSTER FOR NYE FIRMAER   *         *
      *  7/09/93 OVERFØRT KTOKL. FRA FAKTURA REGNSKAPSREC.           *         *
      * 21/02/94 TILPASSET FOR KONT.FAKT. (FLERE HOVEDBOKSKONTI)     *         *
      *  5/08/94 UPSI 1 (FAK.STAT) SETTE KODE "*" I RECORDS.         *         *
      * 25/04/96 BENYTTER DS2 (DISKONTERINGSSTED9 TIL TEKST-SEKV.NR  *         *
      * 23/01/97 LEGGER INN TEKST FRA FAKR-TRANSER.                  *         *
      *          TATT UT BEHANDLING AV DISKONTERING AV VEKSEL (1131) *         *
      * 02.09.97 TATT UT VEKSEL/AKSEPT-NR OG MASKINORDRENR           *
      * 15.04.11 TATT MED UTVIDET BELØPSFELT                         *
      *E  21.10.98: FLYTTER TKODE FRA 88-89 TIL 72-73 I DAGRESK.     *
      *E  29.08.00: TATT MED 4-SIFRET AVD.                           *
      *E  31.08.00: ENDRET RECORDLENGDE FRA 80 TIL 100 PÅ FREGF.     *
      *E  11.05.04: FJERNET OMDØPING AV KUNDENR PÅ BEMA (764)        *
      *E  03.10.05: UTVIDET ART03 FRA 70 TIL 240 BYTE                *
      *E            UTVIDET DAGRESK  FRA 89 TIL 200 BYTE             *
      *E            LAGT INN TEKST I RESK.FIL                        *
      *E  23.04.08: ENDRET EDITERING I AVSTEMMINGSSUM                *
      *E  15.04.11: ENDRET NAVN TIL RLRT 4-8 FRA 120 TIL 240         *
      *E            UTVIDET FILE FOR RKO021 PÅ LISTEN                *
      *E            TATT MED NYE UTVIDEDE BELØPSFELT                 *
      *E            TATT MED NYTT FELT MED VATUTATYPE INT. STANDARD  *
      *E            TATT MED NYE FELT MED PROD.-DATO OG KLOKKE       *
      *E OBS!       BEL3 ENDRES NÅR NYTT BELØPSFELT I FAKTURERING.   *
      *E OBS!       VALUTABELØP LAGES IKKE I FAKTURERINGEN.          *
      *E            Fjernet test på fnr 983                          *
      *E 02.10.12   SKRIVER FIL MED AVSTEMMINGSTALL                  *
      *E 09.10.12   REDIGERT/FJERNET SONE I DATO PÅ AVSTEMM          *
      ****************************************************************  ********
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RKO121.rpg
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
           SELECT ART03
               ASSIGN TO UT-S-ART03
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ART03-STATUS.
           SELECT ART48
               ASSIGN TO UT-S-ART48
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS ART48-STATUS.
           SELECT FRESKF
               ASSIGN TO UT-S-FRESKF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FRESKF-STATUS.
           SELECT FREGF
               ASSIGN TO UT-S-FREGF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FREGF-STATUS.
           SELECT UTERESK
               ASSIGN TO UT-S-UTERESK
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS UTERESK-STATUS.
           SELECT DAGRESK
               ASSIGN TO UT-S-DAGRESK
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS DAGRESK-STATUS.
           SELECT DAGREGN
               ASSIGN TO UT-S-DAGREGN
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS DAGREGN-STATUS.
           SELECT AVSTEMM
               ASSIGN TO UT-S-AVSTEMM
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS AVSTEMM-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD ART03
               BLOCK CONTAINS 240
               RECORD CONTAINS 240.
       01  ART03-IO-AREA.
           05  ART03-IO-AREA-X             PICTURE X(240).
       FD ART48
               BLOCK CONTAINS 240
               RECORD CONTAINS 240.
       01  ART48-IO-AREA.
           05  ART48-IO-AREA-X             PICTURE X(240).
       FD FRESKF
               BLOCK CONTAINS 140
               RECORD CONTAINS 70.
       01  FRESKF-IO-AREA.
           05  FRESKF-IO-AREA-X            PICTURE X(70).
       FD FREGF
               BLOCK CONTAINS 200
               RECORD CONTAINS 100.
       01  FREGF-IO-AREA.
           05  FREGF-IO-AREA-X             PICTURE X(100).
       FD UTERESK
               BLOCK CONTAINS 200
               RECORD CONTAINS 200.
       01  UTERESK-IO-AREA.
           05  UTERESK-IO-AREA-X           PICTURE X(200).
       FD DAGRESK
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  DAGRESK-IO-AREA.
           05  DAGRESK-IO-AREA-X           PICTURE X(200).
       FD DAGREGN
               BLOCK CONTAINS 240
               RECORD CONTAINS 240.
       01  DAGREGN-IO-AREA.
           05  DAGREGN-IO-AREA-X           PICTURE X(240).
       FD AVSTEMM
               BLOCK CONTAINS 120
               RECORD CONTAINS 120.
       01  AVSTEMM-IO-AREA.
           05  AVSTEMM-IO-AREA-X           PICTURE X(120).
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
           10  ART03-STATUS                PICTURE 99 VALUE 0.
           10  ART48-STATUS                PICTURE 99 VALUE 0.
           10  FRESKF-STATUS               PICTURE 99 VALUE 0.
           10  FREGF-STATUS                PICTURE 99 VALUE 0.
           10  UTERESK-STATUS              PICTURE 99 VALUE 0.
           10  DAGRESK-STATUS              PICTURE 99 VALUE 0.
           10  DAGREGN-STATUS              PICTURE 99 VALUE 0.
           10  AVSTEMM-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  ART03-EOF-OFF           VALUE '0'.
               88  ART03-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ART03-READ-OFF          VALUE '0'.
               88  ART03-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ART03-PROCESS-OFF       VALUE '0'.
               88  ART03-PROCESS           VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ART48-EOF-OFF           VALUE '0'.
               88  ART48-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ART48-READ-OFF          VALUE '0'.
               88  ART48-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  ART48-PROCESS-OFF       VALUE '0'.
               88  ART48-PROCESS           VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FRESKF-EOF-OFF          VALUE '0'.
               88  FRESKF-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FRESKF-READ-OFF         VALUE '0'.
               88  FRESKF-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FRESKF-PROCESS-OFF      VALUE '0'.
               88  FRESKF-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FREGF-EOF-OFF           VALUE '0'.
               88  FREGF-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FREGF-READ-OFF          VALUE '0'.
               88  FREGF-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FREGF-PROCESS-OFF       VALUE '0'.
               88  FREGF-PROCESS           VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  UTERESK-EOF-OFF         VALUE '0'.
               88  UTERESK-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  UTERESK-READ-OFF        VALUE '0'.
               88  UTERESK-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  UTERESK-PROCESS-OFF     VALUE '0'.
               88  UTERESK-PROCESS         VALUE '1'.
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
           05  ART03-DATA-FIELDS.
               10  FK1                     PICTURE X(3).
               10  BA1                     PICTURE X(1).
               10  BN1                     PICTURE X(6).
               10  BDD1                    PICTURE X(2).
               10  BDM1                    PICTURE X(2).
               10  BDA-ELG1                PICTURE X(2).
               10  RNR1                    PICTURE X(6).
               10  RNRF1                   PICTURE X(1).
               10  VT1                     PICTURE X(1).
               10  VAL1-IO.
                   15  VAL1                PICTURE S9(8)V9(2).
               10  BM1                     PICTURE X(2).
               10  FDD1                    PICTURE X(2).
               10  FDM1                    PICTURE X(2).
               10  FDA-ELG1                PICTURE X(2).
               10  RF1                     PICTURE X(6).
               10  AV1                     PICTURE X(1).
               10  BT1-IO.
                   15  BT1                 PICTURE S9(7)V9(2).
               10  TK1                     PICTURE X(2).
               10  SI1                     PICTURE X(2).
               10  TEKSFH                  PICTURE X(24).
               10  BELHUF-IO.
                   15  BELHUF              PICTURE S9(11)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  VALHUF-IO.
                   15  VALHUF              PICTURE S9(11)V9(4) USAGE
                                                       PACKED-DECIMAL.
               10  VTHUF                   PICTURE X(3).
               10  HOPPHA                  PICTURE X(4).
               10  PRDDFH                  PICTURE X(5).
               10  PRDKFH                  PICTURE X(4).
               10  KT1                     PICTURE X(8).
               10  BEL1-IO.
                   15  BEL1                PICTURE S9(7)V9(2).
               10  DK1A                    PICTURE X(1).
               10  SNR1                    PICTURE X(3).
               10  RDG1                    PICTURE X(3).
               10  AK1                     PICTURE X(1).
               10  TEKSFD                  PICTURE X(24).
               10  AVD4                    PICTURE X(4).
               10  BELDUF-IO.
                   15  BELDUF              PICTURE S9(11)V9(2) USAGE
                                                       PACKED-DECIMAL.
      * DETALJLINJER M/VAL?                  88  95 VALDUF
      * IKKE I BRUK I FAKR ENNÅ              96  98 VTDUF
               10  DOPPHA                  PICTURE X(4).
               10  PRDDFD                  PICTURE X(5).
               10  PRDKFD                  PICTURE X(4).
           05  ART48-DATA-FIELDS.
               10  FK2                     PICTURE X(3).
               10  BA2                     PICTURE X(1).
               10  BN2                     PICTURE X(6).
               10  BDD2                    PICTURE X(2).
               10  BDM2                    PICTURE X(2).
               10  BDA-ELG2                PICTURE X(2).
               10  RSK2                    PICTURE X(6).
               10  RSK2F                   PICTURE X(2).
               10  RSK1F                   PICTURE X(1).
               10  RES2                    PICTURE X(2).
               10  KTO4SI                  PICTURE X(4).
               10  BEL2-IO.
                   15  BEL2                PICTURE S9(7)V9(2).
               10  MKT2                    PICTURE X(4).
               10  REF2                    PICTURE X(6).
               10  AVD42                   PICTURE X(4).
               10  FDD2                    PICTURE X(2).
               10  FDM2                    PICTURE X(2).
               10  FDA-ELG2                PICTURE X(2).
               10  AK2                     PICTURE X(1).
               10  SN2                     PICTURE X(3).
               10  DV2                     PICTURE X(1).
               10  VAL2-IO.
                   15  VAL2                PICTURE S9(8)V9(2).
               10  RDG2                    PICTURE X(3).
               10  TK2                     PICTURE X(2).
               10  SI2                     PICTURE X(2).
               10  TKODE                   PICTURE X(2).
               10  TXTSEQ                  PICTURE X(1).
               10  TEKSTB                  PICTURE X(24).
               10  BELDUB-IO.
                   15  BELDUB              PICTURE S9(11)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  VALDUB-IO.
                   15  VALDUB              PICTURE S9(11)V9(4) USAGE
                                                       PACKED-DECIMAL.
               10  VTDUB                   PICTURE X(3).
               10  OPPHAV                  PICTURE X(4).
               10  PRDDTB                  PICTURE X(5).
               10  PRDKLB                  PICTURE X(4).
           05  FRESKF-DATA-FIELDS.
               10  TK3                     PICTURE X(2).
               10  RSK3                    PICTURE X(6).
               10  BD3                     PICTURE X(6).
               10  BN3                     PICTURE X(6).
               10  RN3                     PICTURE X(6).
               10  FD3                     PICTURE X(6).
               10  BEL3-IO.
                   15  BEL3                PICTURE S9(7)V9(2).
               10  FNR3                    PICTURE X(3).
               10  BA3                     PICTURE X(1).
               10  BB3                     PICTURE X(2).
               10  BGIRO                   PICTURE X(1).
           05  FREGF-DATA-FIELDS.
               10  BN4                     PICTURE X(6).
               10  BA4                     PICTURE X(1).
               10  BD4                     PICTURE X(6).
               10  RSK4                    PICTURE X(6).
               10  FNR4                    PICTURE X(3).
               10  AK4                     PICTURE X(1).
               10  KL4                     PICTURE X(1).
               10  VGR4                    PICTURE X(5).
               10  KTOFAK                  PICTURE X(4).
               10  BEL4-IO.
                   15  BEL4                PICTURE S9(7)V9(2).
               10  TEGN4                   PICTURE X(1).
               10  KOSTPR-IO.
                   15  KOSTPR              PICTURE S9(7)V9(2).
               10  FDD4                    PICTURE X(2).
               10  FDM4                    PICTURE X(2).
               10  FDA-ELG4                PICTURE X(2).
               10  HDIST4                  PICTURE X(3).
               10  FAKTYP                  PICTURE X(1).
               10  KTOKL                   PICTURE X(1).
               10  BK4                     PICTURE X(1).
               10  REGAVD                  PICTURE X(1).
               10  KNSTED                  PICTURE X(4).
           05  UTERESK-DATA-FIELDS.
               10  REC6                    PICTURE X(200).
               10  BEL6-IO.
                   15  BEL6                PICTURE S9(7)V9(2).
               10  BM6                     PICTURE X(2).
               10  BL6132-IO.
                   15  BL6132              PICTURE S9(11)V9(2) USAGE
                                                       PACKED-DECIMAL.
           05  TEMPORARY-FIELDS.
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
               10  NUL154-IO.
                   15  NUL154              PICTURE S9(11)V9(4).
               10  NUL102-IO.
                   15  NUL102              PICTURE S9(8)V9(2).
               10  VAL0-IO.
                   15  VAL0                PICTURE S9(9)V9(2).
               10  VAL0U-IO.
                   15  VAL0U               PICTURE S9(11)V9(4).
               10  AREA1                   PICTURE X(2).
               10  AREA2                   PICTURE X(2).
               10  AREA3                   PICTURE X(2).
               10  AREA4                   PICTURE X(4).
               10  AREA5                   PICTURE X(6).
               10  RECRES-IO.
                   15  RECRES              PICTURE S9(6).
               10  RESANT-IO.
                   15  RESANT              PICTURE S9(13).
               10  BELRES-IO.
                   15  BELRES              PICTURE S9(9)V9(2).
               10  BELRSU-IO.
                   15  BELRSU              PICTURE S9(11)V9(2).
               10  BL3132-IO.
                   15  BL3132              PICTURE S9(11)V9(2).
               10  NYERES-IO.
                   15  NYERES              PICTURE S9(6).
               10  NYEBEL-IO.
                   15  NYEBEL              PICTURE S9(9)V9(2).
               10  NYEBEU-IO.
                   15  NYEBEU              PICTURE S9(11)V9(2).
               10  RECREG-IO.
                   15  RECREG              PICTURE S9(6).
               10  BELRGU-IO.
                   15  BELRGU              PICTURE S9(11)V9(2).
               10  BL4132-IO.
                   15  BL4132              PICTURE S9(11)V9(2).
               10  KOST4P-IO.
                   15  KOST4P              PICTURE S9(9)V9(2).
               10  BL4154-IO.
                   15  BL4154              PICTURE S9(11)V9(4).
               10  TEGN1                   PICTURE X(1).
               10  BELREG-IO.
                   15  BELREG              PICTURE S9(9)V9(2).
               10  BELUT2-IO.
                   15  BELUT2              PICTURE S9(7)V9(2).
               10  BL2132-IO.
                   15  BL2132              PICTURE S9(11)V9(2).
               10  VT2                     PICTURE X(1).
               10  DS2                     PICTURE X(1).
               10  LRRGAN-IO.
                   15  LRRGAN              PICTURE S9(13).
               10  LRRGBE-IO.
                   15  LRRGBE              PICTURE S9(13)V9(2).
               10  LRRGBU-IO.
                   15  LRRGBU              PICTURE S9(13)V9(2).
               10  LRRSAN-IO.
                   15  LRRSAN              PICTURE S9(13).
               10  LRRSBE-IO.
                   15  LRRSBE              PICTURE S9(13)V9(2).
               10  LERSBU-IO.
                   15  LERSBU              PICTURE S9(13)V9(2).
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
               10  XO-72P-EF.
                 15  XO-72P                PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-82P-EF.
                 15  XO-82P                PICTURE S9(8)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  XO-92P-EF.
                 15  XO-92P                PICTURE S9(9)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  EDIT-RECRES             PICTURE ZZZZZZ.
               10  EDIT-BELRES             PICTURE ZZZZZZZZZ,ZZ-.
               10  EDIT-BELRSU             PICTURE ZZZZZZZZZZZ,ZZ-.
               10  EDIT-RECREG             PICTURE ZZZZZZ.
               10  EDIT-BELREG             PICTURE ZZZZZZZZZ,ZZ-.
               10  EDIT-BELRGU             PICTURE ZZZZZZZZZZZ,ZZ-.
               10  EDIT-NYERES             PICTURE ZZZZZZ.
               10  XO-92YY9R               PICTURE ZZZ.ZZZ.ZZZ,99-.
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
           SET NOT-I-06                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  ART03-PROCESS
               SET ART03-PROCESS-OFF       TO TRUE
               SET ART03-READ              TO TRUE
           END-IF
 
           IF  ART03-READ
           AND RECORD-SELECTED-OFF
               PERFORM ART03-GET
               SET ART03-READ-OFF          TO TRUE
               IF  NOT ART03-EOF
                   PERFORM ART03-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET ART03-PROCESS       TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  ART48-PROCESS
               SET ART48-PROCESS-OFF       TO TRUE
               SET ART48-READ              TO TRUE
           END-IF
 
           IF  ART48-READ
           AND RECORD-SELECTED-OFF
               PERFORM ART48-GET
               SET ART48-READ-OFF          TO TRUE
               IF  NOT ART48-EOF
                   PERFORM ART48-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET ART48-PROCESS       TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  FRESKF-PROCESS
               SET FRESKF-PROCESS-OFF      TO TRUE
               SET FRESKF-READ             TO TRUE
           END-IF
 
           IF  FRESKF-READ
           AND RECORD-SELECTED-OFF
               PERFORM FRESKF-GET
               SET FRESKF-READ-OFF         TO TRUE
               IF  NOT FRESKF-EOF
                   PERFORM FRESKF-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET FRESKF-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  FREGF-PROCESS
               SET FREGF-PROCESS-OFF       TO TRUE
               SET FREGF-READ              TO TRUE
           END-IF
 
           IF  FREGF-READ
           AND RECORD-SELECTED-OFF
               PERFORM FREGF-GET
               SET FREGF-READ-OFF          TO TRUE
               IF  NOT FREGF-EOF
                   PERFORM FREGF-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET FREGF-PROCESS       TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  UTERESK-PROCESS
               SET UTERESK-PROCESS-OFF     TO TRUE
               SET UTERESK-READ            TO TRUE
           END-IF
 
           IF  UTERESK-READ
           AND RECORD-SELECTED-OFF
               PERFORM UTERESK-GET
               SET UTERESK-READ-OFF        TO TRUE
               IF  NOT UTERESK-EOF
                   PERFORM UTERESK-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET UTERESK-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  ART03-PROCESS
               PERFORM ART03-IDSET
           END-IF
 
           IF  ART48-PROCESS
               PERFORM ART48-IDSET
           END-IF
 
           IF  FRESKF-PROCESS
               PERFORM FRESKF-IDSET
           END-IF
 
           IF  FREGF-PROCESS
               PERFORM FREGF-IDSET
           END-IF
 
           IF  UTERESK-PROCESS
               PERFORM UTERESK-IDSET
           END-IF
 
           IF I-1ST
               GO TO LR-INDICATOR-TEST
           END-IF.
 
       MAINLINE-TOTAL-CALCS.
           IF  I-LR
               PERFORM LR-CALCS
           END-IF
           PERFORM TOTAL-OUTPUT.
 
       LR-INDICATOR-TEST.
           IF  I-LR
               GO TO MAINLINE-TERMINATION
           END-IF
 
           IF  ART03-PROCESS
               PERFORM ART03-FLDSET
           END-IF
 
           IF  ART48-PROCESS
               PERFORM ART48-FLDSET
           END-IF
 
           IF  FRESKF-PROCESS
               PERFORM FRESKF-FLDSET
           END-IF
 
           IF  FREGF-PROCESS
               PERFORM FREGF-FLDOFF
               PERFORM FREGF-FLDSET
           END-IF
 
           IF  UTERESK-PROCESS
               PERFORM UTERESK-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           SET NOT-I-1ST                   TO TRUE
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
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
               MOVE 0                      TO NUL154
               MOVE 0                      TO NUL102
           END-IF
           SET NOT-I-11                    TO TRUE
           SET NOT-I-12                    TO TRUE
           SET NOT-I-21                    TO TRUE
           SET NOT-I-22                    TO TRUE
           SET NOT-I-23                    TO TRUE
           SET NOT-I-41                    TO TRUE
           SET NOT-I-42                    TO TRUE
           SET NOT-I-54                    TO TRUE
           SET NOT-I-19                    TO TRUE
           SET NOT-I-61                    TO TRUE
           SET NOT-I-62                    TO TRUE
           SET NOT-I-63                    TO TRUE
           SET NOT-I-64                    TO TRUE
           SET NOT-I-65                    TO TRUE
           SET NOT-I-66                    TO TRUE
           SET NOT-I-69                    TO TRUE
           SET NOT-I-60                    TO TRUE
           SET NOT-I-88                    TO TRUE
           SET NOT-I-90                    TO TRUE
           SET NOT-I-91                    TO TRUE
           SET NOT-I-92                    TO TRUE
      *                    SETOF                     93
      *
      *  BM = 19   ENDRES TIL  06. SAMT ATT RESKONTRORECORDS FRA FAKTURERING
      *  MED BM = 19 - BLIR OVERFØRT TIL KUNDENR = ?????? (SHELL-HOVEDKONTOR)
      *
      *  02                MOVE "VALDUF  "BUGFL1  8        DISPLAY FIELD
      *  02      BUGFL1    DEBUGBUGFILO   VALDUF           VIS INDIKATOR
           IF  (I-01)
               SET NOT-I-19                TO TRUE
               IF  BM1 = '19'
                   SET I-19                TO TRUE
               END-IF
           END-IF
           IF  (I-04)
               SET NOT-I-90                TO TRUE
               IF  FNR3 = '923'
                   SET I-90                TO TRUE
               END-IF
               SET NOT-I-91                TO TRUE
               IF  FNR3 = '950'
                   SET I-91                TO TRUE
               END-IF
               SET NOT-I-92                TO TRUE
               IF  FNR3 = '965'
                   SET I-92                TO TRUE
               END-IF
      *  04      FNR3      COMP "990"                    93
      *  04      FNR3      COMP "983"                    94
      *  04      FNR3      COMP "942"                    97
           END-IF
           IF  (I-04)
               SET NOT-I-19                TO TRUE
               IF  BB3 = '19'
                   SET I-19                TO TRUE
               END-IF
           END-IF
           IF  (I-06)
               SET NOT-I-19                TO TRUE
               IF  BM6 = '19'
                   SET I-19                TO TRUE
               END-IF
      *
      *  BM = 29   ENDRES TIL  06. SAMT ATT RESKONTRORECORDS FRA FAKTURERING
      *  MED BM = 29 - BLIR OVERFØRT TIL KUNDENR = ?????? (STATOIL)     NTOR)
      *
           END-IF
           IF  (I-01)
               SET NOT-I-96                TO TRUE
               IF  BM1 = '29'
                   SET I-96                TO TRUE
               END-IF
           END-IF
           IF  (I-04)
               SET NOT-I-96                TO TRUE
               IF  BB3 = '29'
                   SET I-96                TO TRUE
               END-IF
           END-IF
           IF  (I-06)
               SET NOT-I-96                TO TRUE
               IF  BM6 = '29'
                   SET I-96                TO TRUE
               END-IF
      *****************************************************************
      * TILDEL REGNSKAPS TEKST.                                       *
      *****************************************************************
           END-IF
           IF  (I-05)
               SET NOT-I-88                TO TRUE
               IF  BA4 = '8'
                   SET I-88                TO TRUE
               END-IF
      *
      *  TEST FOR Å LEGGE UT VALUTA KUN PÅ FØRSTE DETALJREC FRA "FAKR"
      *
           END-IF
           IF  (I-01)
               SET I-52                    TO TRUE
           END-IF
           IF  (I-02 AND I-52)
               SET I-54                    TO TRUE
           END-IF
           IF  (I-02 AND NOT-I-52)
               MOVE 0,00                   TO VAL0
               MOVE 0,00                   TO VAL0U
           END-IF
           IF  (I-02)
               SET NOT-I-52                TO TRUE
      *
           END-IF
           IF  (I-04)
               GO TO A1-T
           END-IF
           IF  (NOT-I-01)
               GO TO SLUTT-T
           END-IF
           IF  (I-01)
               SET NOT-I-11                TO TRUE
               IF  TK1 = '21'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-11)
               GO TO A1-T
           END-IF
           IF  (I-01)
               SET NOT-I-12                TO TRUE
               IF  TK1 = '26'
                   SET I-12                TO TRUE
               END-IF
           END-IF
           IF  (I-12)
               GO TO A1-T
           END-IF
           GO TO B1-T.
 
       A1-T.
           IF  (I-04)
               SET NOT-I-95                TO TRUE
               IF  BGIRO = '1'
                   SET I-95                TO TRUE
               END-IF
               MOVE BB3                    TO BM1
               SET NOT-I-77                TO TRUE
               IF  BB3 = '07'
                   SET I-77                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND NOT-I-77)
               SET NOT-I-77                TO TRUE
               IF  BB3 = '14'
                   SET I-77                TO TRUE
               END-IF
           END-IF
           IF  (I-04)
               MOVE TK3                    TO TK1
               MOVE RN3                    TO RF1
               MOVE RSK1F                  TO RNRF1
           END-IF.
 
       B1-T.
           SET NOT-I-21                    TO TRUE
           IF  TK1 = '26'
               SET I-21                    TO TRUE
           END-IF
           IF  (I-21)
               GO TO B2-T
           END-IF
           GO TO SLUTT-T.
 
       B2-T.
           SET NOT-I-22                    TO TRUE
           IF  RF1 = '      '
               SET I-22                    TO TRUE
           END-IF
           IF  (I-22)
               GO TO B3-T
           END-IF
           GO TO SLUTT-T.
 
       B3-T.
           SET NOT-I-23                    TO TRUE
           IF  RNRF1 NOT = '5'
               SET I-23                    TO TRUE
           END-IF
           IF  (I-23)
               GO TO B4-T
           END-IF
           GO TO SLUTT-T.
 
       B4-T.
           MOVE '99'                       TO AREA1
           MOVE BDM1                       TO AREA2
           MOVE BDA-ELG1                   TO AREA3
           MOVE AREA1                      TO AREA4 (1:2)
           MOVE AREA2                      TO AREA4 (3:2)
           MOVE AREA4                      TO AREA5 (1:4)
           MOVE AREA3                      TO AREA5 (5:2)
           IF  (I-04)
               MOVE AREA5                  TO RN3
           END-IF
           IF  (NOT-I-04)
               MOVE AREA5                  TO RF1
           END-IF.
 
       SLUTT-T.
           IF  (I-03)
               SET NOT-I-61                TO TRUE
               IF  RES2 = '  '
                   SET I-61                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-61)
               GO TO RESKO1-T
           END-IF
           IF  (I-03)
               GO TO ENDE-T
           END-IF.
 
       RESKO1-T.
           IF  (I-03)
               SET NOT-I-60                TO TRUE
               IF  KTO4SI = '    '
                   SET I-60                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-60)
               GO TO ENDE-T
           END-IF
           IF  (I-03)
               SET NOT-I-62                TO TRUE
               IF  RSK2F = '30'
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-62)
               GO TO RESK99-T
           END-IF
           IF  (I-03)
               SET NOT-I-63                TO TRUE
               IF  RSK2F = '50'
                   SET I-63                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-63)
               GO TO RESK99-T
           END-IF
           IF  (I-03)
               SET NOT-I-64                TO TRUE
               IF  RSK1F NOT > '2'
                   SET I-64                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-64)
               GO TO RESK99-T
           END-IF
           IF  (I-03)
               SET NOT-I-65                TO TRUE
               IF  RSK1F = '4'
                   SET I-65                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-65)
               GO TO RESK99-T
           END-IF
           IF  (I-03)
               SET NOT-I-66                TO TRUE
               IF  RSK1F NOT < '6'
                   SET I-66                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-66)
               GO TO RESK99-T
           END-IF
           IF  (I-03)
               GO TO ENDE-T
           END-IF.
 
       RESK99-T.
           IF  (I-03)
               SET I-69                    TO TRUE
           END-IF.
 
       ENDE-T.
           IF  (I-01)
               ADD 1                       TO RECRES
               ADD 1                       TO RESANT
           END-IF
           IF  (I-03 AND I-69)
               ADD 1                       TO RECRES
               ADD 1                       TO RESANT
           END-IF
           IF  (I-04)
               ADD 1                       TO RECRES
               ADD 1                       TO RESANT
           END-IF
           IF  (I-06)
               ADD 1                       TO RECRES
               ADD 1                       TO RESANT
           END-IF
           IF  (I-01)
               ADD BT1                     TO BELRES
               ADD BELHUF                  TO BELRSU
           END-IF
           IF  (I-03 AND I-69)
               ADD BEL2                    TO BELRES
               ADD BELDUB                  TO BELRSU
           END-IF
           IF  (I-04)
               ADD BEL3                    TO BELRES
               ADD BEL3                    TO BELRSU
               ADD BEL3 TO ZERO        GIVING BL3132
           END-IF
           IF  (I-06)
               ADD BEL6                    TO BELRES
               ADD BL6132                  TO BELRSU
               ADD 1                       TO NYERES
               ADD BEL6                    TO NYEBEL
               ADD BL6132                  TO NYEBEU
           END-IF
           IF  (I-02)
               ADD 1                       TO RECREG
           END-IF
           IF  (I-03)
               ADD 1                       TO RECREG
           END-IF
           IF  (I-05)
               ADD 1                       TO RECREG
           END-IF
           IF  (I-03)
               ADD BEL2                    TO BELREG
               ADD BELDUB                  TO BELRGU
           END-IF
           IF  (I-05)
               SET NOT-I-50                TO TRUE
               IF  TEGN4 = '-'
                   SET I-50                TO TRUE
               END-IF
               ADD BEL4 TO ZERO        GIVING BL4132
           END-IF
           IF  (I-05 AND I-50)
               SUBTRACT BEL4               FROM BELREG
           END-IF
           IF  (I-05 AND NOT-I-50)
               ADD BEL4                    TO BELREG
           END-IF
           IF  (I-05 AND I-50)
               SUBTRACT BL4132             FROM BELRGU
           END-IF
           IF  (I-05 AND NOT-I-50)
               ADD BL4132                  TO BELRGU
           END-IF
           IF  (I-05)
               ADD KOSTPR TO ZERO      GIVING KOST4P
               ADD KOSTPR TO ZERO      GIVING BL4154
           END-IF
           IF  (I-02)
               SET NOT-I-41                TO TRUE
               IF  DK1A = 'D'
                   SET I-41                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-41)
               MOVE '-'                    TO TEGN1
           END-IF
           IF  (I-02 AND NOT-I-41)
               MOVE ' '                    TO TEGN1
               ADD BEL1                    TO BELREG
           END-IF
           IF  (I-02 AND I-41)
               SUBTRACT BEL1               FROM BELREG
           END-IF
           IF  (I-02 AND NOT-I-41)
               ADD BELDUF                  TO BELRGU
           END-IF
           IF  (I-02 AND I-41)
               SUBTRACT BELDUF             FROM BELRGU
           END-IF
           IF  (I-03)
               SET NOT-I-42                TO TRUE
               IF  BEL2 < 0,00
                   SET I-42                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-42)
               MULTIPLY -1,00 BY BEL2  GIVING BELUT2
           END-IF
           IF  (I-03 AND NOT-I-42)
               MULTIPLY 1,00 BY BEL2   GIVING BELUT2
           END-IF
           IF  (I-03 AND I-42)
               MULTIPLY -1,00 BY BELDUB GIVING BL2132
           END-IF
           IF  (I-03 AND NOT-I-42)
               MULTIPLY 1,00 BY BELDUB GIVING BL2132
           END-IF
           IF  (I-03)
               MOVE ' '                    TO VT2
               MOVE DV2                    TO VT2
               MOVE TXTSEQ                 TO DS2
           END-IF
           IF  (I-01)
               SET NOT-I-26                TO TRUE
               IF  BT1 < 0,00
                   SET I-26                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-26)
               MULTIPLY -1,00 BY VAL1  GIVING VAL1
               MULTIPLY -1,00 BY VALHUF GIVING VALHUF
      * SJEKKE OM NYTT VALUTAFELT,VALDUB, HAR RETT TEGN
           END-IF
           IF  (I-03)
               SET NOT-I-27                TO TRUE
               IF  BEL2 < 0,00
                   SET I-27                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-27)
               MULTIPLY -1,00 BY VAL2  GIVING VAL2
           END-IF
           IF  (I-01)
               SET NOT-I-71                TO TRUE
               IF  RF1 = '      '
                   SET I-71                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-71)
               MOVE BN1                    TO RF1
           END-IF
           IF  (I-03)
               SET NOT-I-72                TO TRUE
               IF  REF2 = '      '
                   SET I-72                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-72)
               MOVE BN2                    TO REF2
           END-IF
           IF  (I-04)
               SET NOT-I-73                TO TRUE
               IF  RN3 = '      '
                   SET I-73                TO TRUE
               END-IF
           END-IF
           IF  (I-04 AND I-73)
               MOVE BN3                    TO RN3
           END-IF.
 
       LR-CALCS SECTION.
       LR-CALCS-P.
           ADD RECREG TO ZERO          GIVING LRRGAN
           ADD BELREG TO ZERO          GIVING LRRGBE
           ADD BELRGU TO ZERO          GIVING LRRGBU
           ADD RECRES TO ZERO          GIVING LRRSAN
           ADD BELRES TO ZERO          GIVING LRRSBE
           ADD BELRSU TO ZERO          GIVING LERSBU.
 
       ART03-GET SECTION.
       ART03-GET-P.
           IF  ART03-EOF-OFF
               READ ART03
               AT END
                   SET ART03-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       ART03-FLDSET SECTION.
       ART03-FLDSET-P.
           EVALUATE TRUE
           WHEN ( ART03-IO-AREA (1:1) = '1' )
               MOVE ART03-IO-AREA (2:3)    TO FK1 (1:3)
               MOVE ART03-IO-AREA (5:1)    TO BA1 (1:1)
               MOVE ART03-IO-AREA (6:6)    TO BN1 (1:6)
               MOVE ART03-IO-AREA (12:2)   TO BDD1 (1:2)
               MOVE ART03-IO-AREA (14:2)   TO BDM1 (1:2)
               MOVE ART03-IO-AREA (16:2)   TO BDA-ELG1 (1:2)
               MOVE ART03-IO-AREA (18:6)   TO RNR1 (1:6)
               MOVE ART03-IO-AREA (18:1)   TO RNRF1 (1:1)
               MOVE ART03-IO-AREA (25:1)   TO VT1 (1:1)
               MOVE ART03-IO-AREA (26:10)  TO VAL1-IO
               INSPECT VAL1-IO REPLACING ALL ' ' BY '0'
               MOVE ART03-IO-AREA (37:2)   TO BM1 (1:2)
               MOVE ART03-IO-AREA (39:2)   TO FDD1 (1:2)
               MOVE ART03-IO-AREA (41:2)   TO FDM1 (1:2)
               MOVE ART03-IO-AREA (43:2)   TO FDA-ELG1 (1:2)
               MOVE ART03-IO-AREA (45:6)   TO RF1 (1:6)
               MOVE ART03-IO-AREA (51:1)   TO AV1 (1:1)
               MOVE ART03-IO-AREA (53:9)   TO BT1-IO
               INSPECT BT1-IO REPLACING ALL ' ' BY '0'
               MOVE ART03-IO-AREA (62:2)   TO TK1 (1:2)
               MOVE ART03-IO-AREA (69:2)   TO SI1 (1:2)
               MOVE ART03-IO-AREA (71:24)  TO TEKSFH (1:24)
               MOVE ART03-IO-AREA (95:7)   TO BELHUF-IO
               MOVE ART03-IO-AREA (102:8)  TO VALHUF-IO
               MOVE ART03-IO-AREA (110:3)  TO VTHUF (1:3)
               MOVE ART03-IO-AREA (228:4)  TO HOPPHA (1:4)
               MOVE ART03-IO-AREA (232:5)  TO PRDDFH (1:5)
               MOVE ART03-IO-AREA (237:4)  TO PRDKFH (1:4)
           WHEN ( ART03-IO-AREA (1:1) = '2' )
               MOVE ART03-IO-AREA (2:8)    TO KT1 (1:8)
               MOVE ART03-IO-AREA (10:9)   TO BEL1-IO
               INSPECT BEL1-IO REPLACING ALL ' ' BY '0'
               MOVE ART03-IO-AREA (19:1)   TO DK1A (1:1)
               MOVE ART03-IO-AREA (26:3)   TO SNR1 (1:3)
               MOVE ART03-IO-AREA (30:3)   TO RDG1 (1:3)
               MOVE ART03-IO-AREA (39:1)   TO AK1 (1:1)
               MOVE ART03-IO-AREA (40:24)  TO TEKSFD (1:24)
               MOVE ART03-IO-AREA (64:4)   TO AVD4 (1:4)
               MOVE ART03-IO-AREA (81:7)   TO BELDUF-IO
               MOVE ART03-IO-AREA (228:4)  TO DOPPHA (1:4)
               MOVE ART03-IO-AREA (232:5)  TO PRDDFD (1:5)
               MOVE ART03-IO-AREA (237:4)  TO PRDKFD (1:4)
           END-EVALUATE.
 
       ART03-IDCHK SECTION.
       ART03-IDCHK-P.
           EVALUATE TRUE
           WHEN ( ART03-IO-AREA (1:1) = '1' )
             OR ( ART03-IO-AREA (1:1) = '2' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       ART03-IDSET SECTION.
       ART03-IDSET-P.
           EVALUATE TRUE
           WHEN ( ART03-IO-AREA (1:1) = '1' )
               SET I-01                    TO TRUE
           WHEN ( ART03-IO-AREA (1:1) = '2' )
               SET I-02                    TO TRUE
           END-EVALUATE.
 
       ART48-GET SECTION.
       ART48-GET-P.
           IF  ART48-EOF-OFF
               READ ART48
               AT END
                   SET ART48-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       ART48-FLDSET SECTION.
       ART48-FLDSET-P.
           EVALUATE TRUE
           WHEN ( ART48-IO-AREA (1:1) = '2'
            AND   ART48-IO-AREA (2:1) = '1' )
               MOVE ART48-IO-AREA (3:3)    TO FK2 (1:3)
               MOVE ART48-IO-AREA (6:1)    TO BA2 (1:1)
               MOVE ART48-IO-AREA (7:6)    TO BN2 (1:6)
               MOVE ART48-IO-AREA (13:2)   TO BDD2 (1:2)
               MOVE ART48-IO-AREA (15:2)   TO BDM2 (1:2)
               MOVE ART48-IO-AREA (17:2)   TO BDA-ELG2 (1:2)
               MOVE ART48-IO-AREA (19:6)   TO RSK2 (1:6)
               MOVE ART48-IO-AREA (19:2)   TO RSK2F (1:2)
               MOVE ART48-IO-AREA (19:1)   TO RSK1F (1:1)
               MOVE ART48-IO-AREA (25:2)   TO RES2 (1:2)
               MOVE ART48-IO-AREA (23:4)   TO KTO4SI (1:4)
               MOVE ART48-IO-AREA (27:9)   TO BEL2-IO
               INSPECT BEL2-IO REPLACING ALL ' ' BY '0'
               MOVE ART48-IO-AREA (37:4)   TO MKT2 (1:4)
               MOVE ART48-IO-AREA (41:6)   TO REF2 (1:6)
               MOVE ART48-IO-AREA (47:4)   TO AVD42 (1:4)
               MOVE ART48-IO-AREA (59:2)   TO FDD2 (1:2)
               MOVE ART48-IO-AREA (61:2)   TO FDM2 (1:2)
               MOVE ART48-IO-AREA (63:2)   TO FDA-ELG2 (1:2)
               MOVE ART48-IO-AREA (65:1)   TO AK2 (1:1)
               MOVE ART48-IO-AREA (66:3)   TO SN2 (1:3)
               MOVE ART48-IO-AREA (69:1)   TO DV2 (1:1)
               MOVE ART48-IO-AREA (70:10)  TO VAL2-IO
               INSPECT VAL2-IO REPLACING ALL ' ' BY '0'
               MOVE ART48-IO-AREA (80:3)   TO RDG2 (1:3)
               MOVE ART48-IO-AREA (83:2)   TO TK2 (1:2)
               MOVE ART48-IO-AREA (85:2)   TO SI2 (1:2)
               MOVE ART48-IO-AREA (88:2)   TO TKODE (1:2)
               MOVE ART48-IO-AREA (96:1)   TO TXTSEQ (1:1)
               MOVE ART48-IO-AREA (97:24)  TO TEKSTB (1:24)
               MOVE ART48-IO-AREA (121:7)  TO BELDUB-IO
               MOVE ART48-IO-AREA (128:8)  TO VALDUB-IO
               MOVE ART48-IO-AREA (136:3)  TO VTDUB (1:3)
               MOVE ART48-IO-AREA (228:4)  TO OPPHAV (1:4)
               MOVE ART48-IO-AREA (232:5)  TO PRDDTB (1:5)
               MOVE ART48-IO-AREA (237:4)  TO PRDKLB (1:4)
           END-EVALUATE.
 
       ART48-IDCHK SECTION.
       ART48-IDCHK-P.
           EVALUATE TRUE
           WHEN ( ART48-IO-AREA (1:1) = '2'
            AND   ART48-IO-AREA (2:1) = '1' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       ART48-IDSET SECTION.
       ART48-IDSET-P.
           EVALUATE TRUE
           WHEN ( ART48-IO-AREA (1:1) = '2'
            AND   ART48-IO-AREA (2:1) = '1' )
               SET I-03                    TO TRUE
           END-EVALUATE.
 
       FRESKF-GET SECTION.
       FRESKF-GET-P.
           IF  FRESKF-EOF-OFF
               READ FRESKF
               AT END
                   SET FRESKF-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FRESKF-FLDSET SECTION.
       FRESKF-FLDSET-P.
           EVALUATE TRUE
           WHEN ( FRESKF-IO-AREA (1:1) = '0'
            AND   FRESKF-IO-AREA (2:1) = '2' )
               MOVE FRESKF-IO-AREA (3:2)   TO TK3 (1:2)
               MOVE FRESKF-IO-AREA (5:6)   TO RSK3 (1:6)
               MOVE FRESKF-IO-AREA (5:1)   TO RSK1F (1:1)
               MOVE FRESKF-IO-AREA (11:6)  TO BD3 (1:6)
               MOVE FRESKF-IO-AREA (17:6)  TO BN3 (1:6)
               MOVE FRESKF-IO-AREA (23:6)  TO RN3 (1:6)
               MOVE FRESKF-IO-AREA (29:6)  TO FD3 (1:6)
               MOVE FRESKF-IO-AREA (35:9)  TO BEL3-IO
               INSPECT BEL3-IO REPLACING ALL ' ' BY '0'
               MOVE FRESKF-IO-AREA (44:3)  TO FNR3 (1:3)
               MOVE FRESKF-IO-AREA (47:1)  TO BA3 (1:1)
               MOVE FRESKF-IO-AREA (48:2)  TO BB3 (1:2)
               MOVE FRESKF-IO-AREA (69:1)  TO BGIRO (1:1)
           END-EVALUATE.
 
       FRESKF-IDCHK SECTION.
       FRESKF-IDCHK-P.
           EVALUATE TRUE
           WHEN ( FRESKF-IO-AREA (1:1) = '0'
            AND   FRESKF-IO-AREA (2:1) = '2' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       FRESKF-IDSET SECTION.
       FRESKF-IDSET-P.
           EVALUATE TRUE
           WHEN ( FRESKF-IO-AREA (1:1) = '0'
            AND   FRESKF-IO-AREA (2:1) = '2' )
               SET I-04                    TO TRUE
           END-EVALUATE.
 
       FREGF-GET SECTION.
       FREGF-GET-P.
           IF  FREGF-EOF-OFF
               READ FREGF
               AT END
                   SET FREGF-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FREGF-FLDOFF SECTION.
       FREGF-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( FREGF-IO-AREA (1:1) = '0'
            AND   FREGF-IO-AREA (2:1) = '1' )
               SET NOT-I-08                TO TRUE
           END-EVALUATE.
 
       FREGF-FLDSET SECTION.
       FREGF-FLDSET-P.
           EVALUATE TRUE
           WHEN ( FREGF-IO-AREA (1:1) = '0'
            AND   FREGF-IO-AREA (2:1) = '1' )
               MOVE FREGF-IO-AREA (3:6)    TO BN4 (1:6)
               MOVE FREGF-IO-AREA (9:1)    TO BA4 (1:1)
               MOVE FREGF-IO-AREA (10:6)   TO BD4 (1:6)
               MOVE FREGF-IO-AREA (16:6)   TO RSK4 (1:6)
               MOVE FREGF-IO-AREA (22:3)   TO FNR4 (1:3)
               MOVE FREGF-IO-AREA (30:1)   TO AK4 (1:1)
               MOVE FREGF-IO-AREA (31:1)   TO KL4 (1:1)
               IF  KL4 = SPACES
                   SET I-08                TO TRUE
               END-IF
               MOVE FREGF-IO-AREA (32:5)   TO VGR4 (1:5)
               MOVE FREGF-IO-AREA (32:4)   TO KTOFAK (1:4)
               MOVE FREGF-IO-AREA (37:9)   TO BEL4-IO
               INSPECT BEL4-IO REPLACING ALL ' ' BY '0'
               MOVE FREGF-IO-AREA (46:1)   TO TEGN4 (1:1)
               MOVE FREGF-IO-AREA (47:9)   TO KOSTPR-IO
               INSPECT KOSTPR-IO REPLACING ALL ' ' BY '0'
               MOVE FREGF-IO-AREA (63:2)   TO FDD4 (1:2)
               MOVE FREGF-IO-AREA (65:2)   TO FDM4 (1:2)
               MOVE FREGF-IO-AREA (67:2)   TO FDA-ELG4 (1:2)
               MOVE FREGF-IO-AREA (74:3)   TO HDIST4 (1:3)
               MOVE FREGF-IO-AREA (77:1)   TO FAKTYP (1:1)
               MOVE FREGF-IO-AREA (78:1)   TO KTOKL (1:1)
               MOVE FREGF-IO-AREA (79:1)   TO BK4 (1:1)
               MOVE FREGF-IO-AREA (80:1)   TO REGAVD (1:1)
               MOVE FREGF-IO-AREA (81:4)   TO KNSTED (1:4)
           END-EVALUATE.
 
       FREGF-IDCHK SECTION.
       FREGF-IDCHK-P.
           EVALUATE TRUE
           WHEN ( FREGF-IO-AREA (1:1) = '0'
            AND   FREGF-IO-AREA (2:1) = '1' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       FREGF-IDSET SECTION.
       FREGF-IDSET-P.
           EVALUATE TRUE
           WHEN ( FREGF-IO-AREA (1:1) = '0'
            AND   FREGF-IO-AREA (2:1) = '1' )
               SET I-05                    TO TRUE
           END-EVALUATE.
 
       UTERESK-GET SECTION.
       UTERESK-GET-P.
           IF  UTERESK-EOF-OFF
               READ UTERESK
               AT END
                   SET UTERESK-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       UTERESK-FLDSET SECTION.
       UTERESK-FLDSET-P.
           EVALUATE TRUE
           WHEN ( UTERESK-IO-AREA (1:1) = '3'
            AND   UTERESK-IO-AREA (2:1) = '1' )
               MOVE UTERESK-IO-AREA (1:200) TO REC6 (1:200)
               MOVE UTERESK-IO-AREA (39:9) TO BEL6-IO
               INSPECT BEL6-IO REPLACING ALL ' ' BY '0'
               MOVE UTERESK-IO-AREA (49:2) TO BM6 (1:2)
               MOVE UTERESK-IO-AREA (114:7) TO BL6132-IO
           END-EVALUATE.
 
       UTERESK-IDCHK SECTION.
       UTERESK-IDCHK-P.
           EVALUATE TRUE
           WHEN ( UTERESK-IO-AREA (1:1) = '3'
            AND   UTERESK-IO-AREA (2:1) = '1' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       UTERESK-IDSET SECTION.
       UTERESK-IDSET-P.
           EVALUATE TRUE
           WHEN ( UTERESK-IO-AREA (1:1) = '3'
            AND   UTERESK-IO-AREA (2:1) = '1' )
               SET I-06                    TO TRUE
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
               MOVE 7                      TO LISTE-AFTER-SKIP
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
           IF  (I-01)
               MOVE SPACES TO DAGRESK-IO-AREA
               INITIALIZE DAGRESK-IO-AREA
               MOVE '31'                   TO DAGRESK-IO-AREA (1:2)
               MOVE FK1                    TO DAGRESK-IO-AREA (3:3)
               MOVE RNR1                   TO DAGRESK-IO-AREA (6:6)
               MOVE RF1                    TO DAGRESK-IO-AREA (12:6)
               MOVE TK1                    TO DAGRESK-IO-AREA (18:2)
               MOVE BDA-ELG1               TO DAGRESK-IO-AREA (20:2)
               MOVE BDM1                   TO DAGRESK-IO-AREA (22:2)
               MOVE BDD1                   TO DAGRESK-IO-AREA (24:2)
               MOVE BN1                    TO DAGRESK-IO-AREA (26:6)
               MOVE BA1                    TO DAGRESK-IO-AREA (32:1)
               MOVE FDA-ELG1               TO DAGRESK-IO-AREA (33:2)
               MOVE FDM1                   TO DAGRESK-IO-AREA (35:2)
               MOVE FDD1                   TO DAGRESK-IO-AREA (37:2)
               MOVE BT1-IO                 TO DAGRESK-IO-AREA (39:9)
               MOVE ' '                    TO DAGRESK-IO-AREA (48:1)
               MOVE BM1                    TO DAGRESK-IO-AREA (49:2)
               IF  (I-19)
                   MOVE '06'               TO DAGRESK-IO-AREA (49:2)
               END-IF
               IF  (I-96)
                   MOVE '06'               TO DAGRESK-IO-AREA (49:2)
               END-IF
               MOVE VAL1-IO                TO DAGRESK-IO-AREA (51:10)
               MOVE VT1                    TO DAGRESK-IO-AREA (70:1)
               MOVE ' '                    TO DAGRESK-IO-AREA (71:1)
               MOVE TEKSFH                 TO DAGRESK-IO-AREA (90:24)
               MOVE BELHUF                 TO XO-112P
               MOVE XO-112P-EF             TO DAGRESK-IO-AREA (114:7)
               MOVE VALHUF                 TO XO-114P
               MOVE XO-114P-EF             TO DAGRESK-IO-AREA (121:8)
               MOVE VTHUF                  TO DAGRESK-IO-AREA (129:3)
               MOVE HOPPHA                 TO DAGRESK-IO-AREA (188:4)
               MOVE PRDDFH                 TO DAGRESK-IO-AREA (192:5)
               MOVE PRDKFH                 TO DAGRESK-IO-AREA (197:4)
               WRITE DAGRESK-IO-AREA
           END-IF
           IF  (I-03 AND I-69)
               MOVE SPACES TO DAGRESK-IO-AREA
               INITIALIZE DAGRESK-IO-AREA
               MOVE '31'                   TO DAGRESK-IO-AREA (1:2)
               MOVE FK2                    TO DAGRESK-IO-AREA (3:3)
               MOVE RSK2                   TO DAGRESK-IO-AREA (6:6)
               MOVE REF2                   TO DAGRESK-IO-AREA (12:6)
               MOVE TK2                    TO DAGRESK-IO-AREA (18:2)
               MOVE BDA-ELG2               TO DAGRESK-IO-AREA (20:2)
               MOVE BDM2                   TO DAGRESK-IO-AREA (22:2)
               MOVE BDD2                   TO DAGRESK-IO-AREA (24:2)
               MOVE BN2                    TO DAGRESK-IO-AREA (26:6)
               MOVE BA2                    TO DAGRESK-IO-AREA (32:1)
               MOVE FDA-ELG2               TO DAGRESK-IO-AREA (33:2)
               MOVE FDM2                   TO DAGRESK-IO-AREA (35:2)
               MOVE FDD2                   TO DAGRESK-IO-AREA (37:2)
               MOVE BEL2-IO                TO DAGRESK-IO-AREA (39:9)
               MOVE '1'                    TO DAGRESK-IO-AREA (48:1)
               MOVE '  '                   TO DAGRESK-IO-AREA (49:2)
               MOVE VAL2-IO                TO DAGRESK-IO-AREA (51:10)
               MOVE DV2                    TO DAGRESK-IO-AREA (70:1)
               MOVE ' '                    TO DAGRESK-IO-AREA (71:1)
               MOVE TKODE                  TO DAGRESK-IO-AREA (72:2)
               MOVE TEKSTB                 TO DAGRESK-IO-AREA (90:24)
               MOVE BELDUB                 TO XO-112P
               MOVE XO-112P-EF             TO DAGRESK-IO-AREA (114:7)
               MOVE VALDUB                 TO XO-114P
               MOVE XO-114P-EF             TO DAGRESK-IO-AREA (121:8)
               MOVE VTDUB                  TO DAGRESK-IO-AREA (129:3)
               MOVE OPPHAV                 TO DAGRESK-IO-AREA (188:4)
               MOVE PRDDTB                 TO DAGRESK-IO-AREA (192:5)
               MOVE PRDKLB                 TO DAGRESK-IO-AREA (197:4)
               WRITE DAGRESK-IO-AREA
           END-IF
           IF  (I-04)
               MOVE SPACES TO DAGRESK-IO-AREA
               INITIALIZE DAGRESK-IO-AREA
               MOVE '31'                   TO DAGRESK-IO-AREA (1:2)
               MOVE FNR3                   TO DAGRESK-IO-AREA (3:3)
               MOVE RSK3                   TO DAGRESK-IO-AREA (6:6)
               IF  (I-90 AND I-19)
                   MOVE '125999'           TO DAGRESK-IO-AREA (6:6)
               END-IF
               IF  (I-91 AND I-19)
                   MOVE '149600'           TO DAGRESK-IO-AREA (6:6)
               END-IF
               IF  (I-92 AND I-19)
                   MOVE '114122'           TO DAGRESK-IO-AREA (6:6)
      *                   93 19          11 "102350"
      *                   94 19          11 "120403"
      *                   97 19          11 "134170"
      *                   67 68          11 "110953"
               END-IF
               MOVE RN3                    TO DAGRESK-IO-AREA (12:6)
               MOVE TK3                    TO DAGRESK-IO-AREA (18:2)
               MOVE BD3                    TO DAGRESK-IO-AREA (20:6)
               MOVE BN3                    TO DAGRESK-IO-AREA (26:6)
               MOVE BA3                    TO DAGRESK-IO-AREA (32:1)
               MOVE FD3                    TO DAGRESK-IO-AREA (33:6)
               MOVE BEL3-IO                TO DAGRESK-IO-AREA (39:9)
               MOVE ' '                    TO DAGRESK-IO-AREA (48:1)
               IF  (I-77)
                   MOVE '1'                TO DAGRESK-IO-AREA (48:1)
               END-IF
               MOVE BB3                    TO DAGRESK-IO-AREA (49:2)
               IF  (I-19)
                   MOVE '06'               TO DAGRESK-IO-AREA (49:2)
               END-IF
               IF  (I-96)
                   MOVE '06'               TO DAGRESK-IO-AREA (49:2)
               END-IF
               MOVE NUL102-IO              TO DAGRESK-IO-AREA (51:10)
               IF  (I-95)
                   MOVE '1'                TO DAGRESK-IO-AREA (71:1)
               END-IF
               IF  (NOT-I-95)
                   MOVE ' '                TO DAGRESK-IO-AREA (71:1)
               END-IF
               MOVE BL3132                 TO XO-112P
               MOVE XO-112P-EF             TO DAGRESK-IO-AREA (114:7)
               MOVE NUL154                 TO XO-114P
               MOVE XO-114P-EF             TO DAGRESK-IO-AREA (121:8)
               MOVE 'NOK'                  TO DAGRESK-IO-AREA (129:3)
               MOVE 'FAKT'                 TO DAGRESK-IO-AREA (188:4)
               MOVE DDATO8                 TO XO-80P
               MOVE XO-80P-EF              TO DAGRESK-IO-AREA (192:5)
               MOVE TIDSP                  TO XO-60P
               MOVE XO-60P-EF              TO DAGRESK-IO-AREA (197:4)
               WRITE DAGRESK-IO-AREA
           END-IF
           IF  (I-06)
               MOVE SPACES TO DAGRESK-IO-AREA
               INITIALIZE DAGRESK-IO-AREA
               MOVE REC6                   TO DAGRESK-IO-AREA (1:200)
               IF  (I-19)
                   MOVE '06'               TO DAGRESK-IO-AREA (49:2)
               END-IF
               IF  (I-96)
                   MOVE '06'               TO DAGRESK-IO-AREA (49:2)
               END-IF
               MOVE ' '                    TO DAGRESK-IO-AREA (71:1)
               WRITE DAGRESK-IO-AREA
           END-IF
           IF  (I-02)
               MOVE SPACES TO DAGREGN-IO-AREA
               INITIALIZE DAGREGN-IO-AREA
               MOVE '32'                   TO DAGREGN-IO-AREA (1:2)
               MOVE FK1                    TO DAGREGN-IO-AREA (3:3)
               MOVE BA1                    TO DAGREGN-IO-AREA (6:1)
               MOVE BN1                    TO DAGREGN-IO-AREA (7:6)
               MOVE BDA-ELG1               TO DAGREGN-IO-AREA (13:2)
               MOVE BDM1                   TO DAGREGN-IO-AREA (15:2)
               MOVE BDD1                   TO DAGREGN-IO-AREA (17:2)
               MOVE RNR1                   TO DAGREGN-IO-AREA (19:6)
               MOVE '  '                   TO DAGREGN-IO-AREA (25:2)
               MOVE BEL1                   TO XO-72P
               MOVE XO-72P-EF              TO DAGREGN-IO-AREA (27:5)
               MOVE TEGN1                  TO DAGREGN-IO-AREA (32:1)
               IF  (I-54)
                   MOVE VAL1               TO XO-82P
                   MOVE XO-82P-EF          TO DAGREGN-IO-AREA (33:6)
               END-IF
               IF  (NOT-I-54)
                   MOVE VAL0               TO XO-92P
                   MOVE XO-92P-EF          TO DAGREGN-IO-AREA (33:6)
               END-IF
               MOVE KT1                    TO DAGREGN-IO-AREA (47:8)
               MOVE ' '                    TO DAGREGN-IO-AREA (55:1)
               MOVE AK1                    TO DAGREGN-IO-AREA (56:1)
               MOVE SNR1                   TO DAGREGN-IO-AREA (57:3)
               MOVE AVD4                   TO DAGREGN-IO-AREA (63:4)
               MOVE FDA-ELG1               TO DAGREGN-IO-AREA (72:2)
               MOVE FDM1                   TO DAGREGN-IO-AREA (74:2)
               MOVE FDD1                   TO DAGREGN-IO-AREA (76:2)
               MOVE RDG1                   TO DAGREGN-IO-AREA (78:3)
               MOVE '   '                  TO DAGREGN-IO-AREA (81:3)
               MOVE ' '                    TO DAGREGN-IO-AREA (84:1)
               MOVE VT1                    TO DAGREGN-IO-AREA (85:1)
               MOVE BM1                    TO DAGREGN-IO-AREA (86:2)
               MOVE RF1                    TO DAGREGN-IO-AREA (88:6)
               MOVE AV1                    TO DAGREGN-IO-AREA (94:1)
               MOVE SI1                    TO DAGREGN-IO-AREA (95:2)
               MOVE TEKSFD                 TO DAGREGN-IO-AREA (97:24)
               MOVE BELDUF                 TO XO-112P
               MOVE XO-112P-EF             TO DAGREGN-IO-AREA (121:7)
      *                        BELDUF   127
               IF  (I-54)
                   MOVE VALHUF             TO XO-114P
                   MOVE XO-114P-EF         TO DAGREGN-IO-AREA (128:8)
               END-IF
               IF  (NOT-I-54)
                   MOVE VAL0U              TO XO-114P
                   MOVE XO-114P-EF         TO DAGREGN-IO-AREA (128:8)
               END-IF
               MOVE VTHUF                  TO DAGREGN-IO-AREA (136:3)
      * DETALJLINJER M/VAL?    VALDUF   135
      *                        VTDUF    138
               MOVE DOPPHA                 TO DAGREGN-IO-AREA (228:4)
               MOVE PRDDFD                 TO DAGREGN-IO-AREA (232:5)
               MOVE PRDKFD                 TO DAGREGN-IO-AREA (237:4)
               WRITE DAGREGN-IO-AREA
           END-IF
           IF  (I-03)
               MOVE SPACES TO DAGREGN-IO-AREA
               INITIALIZE DAGREGN-IO-AREA
               MOVE '32'                   TO DAGREGN-IO-AREA (1:2)
               MOVE FK2                    TO DAGREGN-IO-AREA (3:3)
               MOVE BA2                    TO DAGREGN-IO-AREA (6:1)
               MOVE BN2                    TO DAGREGN-IO-AREA (7:6)
               MOVE BDA-ELG2               TO DAGREGN-IO-AREA (13:2)
               MOVE BDM2                   TO DAGREGN-IO-AREA (15:2)
               MOVE BDD2                   TO DAGREGN-IO-AREA (17:2)
               MOVE RSK2                   TO DAGREGN-IO-AREA (19:6)
               MOVE RES2                   TO DAGREGN-IO-AREA (25:2)
               MOVE BELUT2                 TO XO-72P
               MOVE XO-72P-EF              TO DAGREGN-IO-AREA (27:5)
               IF  (I-42)
                   MOVE '-'                TO DAGREGN-IO-AREA (32:1)
               END-IF
               IF  (NOT-I-42)
                   MOVE ' '                TO DAGREGN-IO-AREA (32:1)
               END-IF
               MOVE VAL2                   TO XO-82P
               MOVE XO-82P-EF              TO DAGREGN-IO-AREA (33:6)
               MOVE MKT2                   TO DAGREGN-IO-AREA (47:4)
               MOVE '    '                 TO DAGREGN-IO-AREA (51:4)
               MOVE DS2                    TO DAGREGN-IO-AREA (55:1)
               MOVE AK2                    TO DAGREGN-IO-AREA (56:1)
               MOVE SN2                    TO DAGREGN-IO-AREA (57:3)
               MOVE AVD42                  TO DAGREGN-IO-AREA (63:4)
               MOVE FDA-ELG2               TO DAGREGN-IO-AREA (72:2)
               MOVE FDM2                   TO DAGREGN-IO-AREA (74:2)
               MOVE FDD2                   TO DAGREGN-IO-AREA (76:2)
               MOVE RDG2                   TO DAGREGN-IO-AREA (78:3)
               MOVE '   '                  TO DAGREGN-IO-AREA (81:3)
               MOVE ' '                    TO DAGREGN-IO-AREA (84:1)
               MOVE VT2                    TO DAGREGN-IO-AREA (85:1)
               MOVE '  '                   TO DAGREGN-IO-AREA (86:2)
               MOVE REF2                   TO DAGREGN-IO-AREA (88:6)
               MOVE ' '                    TO DAGREGN-IO-AREA (94:1)
               MOVE SI2                    TO DAGREGN-IO-AREA (95:2)
               MOVE TEKSTB                 TO DAGREGN-IO-AREA (97:24)
               MOVE BL2132                 TO XO-112P
               MOVE XO-112P-EF             TO DAGREGN-IO-AREA (121:7)
               MOVE VALDUB                 TO XO-114P
               MOVE XO-114P-EF             TO DAGREGN-IO-AREA (128:8)
               MOVE VTDUB                  TO DAGREGN-IO-AREA (136:3)
               MOVE OPPHAV                 TO DAGREGN-IO-AREA (228:4)
               MOVE PRDDTB                 TO DAGREGN-IO-AREA (232:5)
               MOVE PRDKLB                 TO DAGREGN-IO-AREA (237:4)
               WRITE DAGREGN-IO-AREA
           END-IF
           IF  (I-05)
               MOVE SPACES TO DAGREGN-IO-AREA
               INITIALIZE DAGREGN-IO-AREA
               MOVE '32'                   TO DAGREGN-IO-AREA (1:2)
               MOVE FNR4                   TO DAGREGN-IO-AREA (3:3)
               MOVE BA4                    TO DAGREGN-IO-AREA (6:1)
               MOVE BN4                    TO DAGREGN-IO-AREA (7:6)
               MOVE BD4                    TO DAGREGN-IO-AREA (13:6)
               MOVE RSK4                   TO DAGREGN-IO-AREA (19:6)
               MOVE '  '                   TO DAGREGN-IO-AREA (25:2)
               MOVE BEL4                   TO XO-72P
               MOVE XO-72P-EF              TO DAGREGN-IO-AREA (27:5)
               MOVE TEGN4                  TO DAGREGN-IO-AREA (32:1)
               MOVE KOST4P                 TO XO-92P
               MOVE XO-92P-EF              TO DAGREGN-IO-AREA (33:6)
               IF  (NOT-I-88)
                   MOVE FAKTYP             TO DAGREGN-IO-AREA (39:1)
               END-IF
               MOVE KTOKL                  TO DAGREGN-IO-AREA (40:1)
               IF  (NOT-I-08)
                   MOVE KL4                TO DAGREGN-IO-AREA (47:1)
               END-IF
               IF  (NOT-I-08)
                   MOVE VGR4               TO DAGREGN-IO-AREA (48:5)
               END-IF
               IF  (NOT-I-08)
                   MOVE '  '               TO DAGREGN-IO-AREA (53:2)
               END-IF
               IF  (I-08)
                   MOVE KTOFAK             TO DAGREGN-IO-AREA (47:4)
               END-IF
               IF  (I-08)
                   MOVE '    '             TO DAGREGN-IO-AREA (51:4)
               END-IF
               MOVE AK4                    TO DAGREGN-IO-AREA (56:1)
               MOVE KNSTED                 TO DAGREGN-IO-AREA (63:4)
               MOVE FDA-ELG4               TO DAGREGN-IO-AREA (72:2)
               MOVE FDM4                   TO DAGREGN-IO-AREA (74:2)
               MOVE FDD4                   TO DAGREGN-IO-AREA (76:2)
               MOVE HDIST4                 TO DAGREGN-IO-AREA (81:3)
               MOVE BK4                    TO DAGREGN-IO-AREA (84:1)
               MOVE '  '                   TO DAGREGN-IO-AREA (86:2)
               MOVE BN4                    TO DAGREGN-IO-AREA (88:6)
               MOVE REGAVD                 TO DAGREGN-IO-AREA (94:1)
               MOVE '  '                   TO DAGREGN-IO-AREA (95:2)
               IF  (NOT-I-88)
                   MOVE '                        ' TO DAGREGN-IO-AREA
                                                               (97:24)
               END-IF
               IF  (I-88)
                   MOVE 'KASSEOPPGJØR KONTANTSALG' TO DAGREGN-IO-AREA
                                                               (97:24)
               END-IF
               IF  (I-U1)
                   MOVE '*'                TO DAGREGN-IO-AREA (97:1)
               END-IF
               MOVE BL4132                 TO XO-112P
               MOVE XO-112P-EF             TO DAGREGN-IO-AREA (121:7)
               MOVE BL4154                 TO XO-114P
               MOVE XO-114P-EF             TO DAGREGN-IO-AREA (128:8)
               MOVE 'NOK'                  TO DAGREGN-IO-AREA (136:3)
               MOVE 'FAKT'                 TO DAGREGN-IO-AREA (228:4)
               MOVE DDATO8                 TO XO-80P
               MOVE XO-80P-EF              TO DAGREGN-IO-AREA (232:5)
               MOVE TIDSP                  TO XO-60P
               MOVE XO-60P-EF              TO DAGREGN-IO-AREA (237:4)
               WRITE DAGREGN-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '***   AVSTEMNINGSTOTALER' TO LISTE-IO-AREA (1:24)
               MOVE ' RKO121'              TO LISTE-IO-AREA (25:7)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-BEFORE-SPACE
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT REC RESKTRANSER'  TO LISTE-IO-AREA (2:19)
               MOVE RECRES                 TO EDIT-RECRES
               MOVE EDIT-RECRES            TO LISTE-IO-AREA (23:6)
               MOVE 'BELØP   RESKTRANSER'  TO LISTE-IO-AREA (30:19)
               MOVE BELRES                 TO EDIT-BELRES
               MOVE EDIT-BELRES            TO LISTE-IO-AREA (53:13)
               MOVE 'RKO021'               TO LISTE-IO-AREA (75:6)
               MOVE 'DATO'                 TO LISTE-IO-AREA (83:4)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (89:8)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE BELRSU                 TO EDIT-BELRSU
               MOVE EDIT-BELRSU            TO LISTE-IO-AREA (51:15)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT REC REGNTRANSER'  TO LISTE-IO-AREA (2:19)
               MOVE RECREG                 TO EDIT-RECREG
               MOVE EDIT-RECREG            TO LISTE-IO-AREA (23:6)
               MOVE 'BELØP   REGNTRANSER'  TO LISTE-IO-AREA (30:19)
               MOVE BELREG                 TO EDIT-BELREG
               MOVE EDIT-BELREG            TO LISTE-IO-AREA (53:13)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE BELRGU                 TO EDIT-BELRGU
               MOVE EDIT-BELRGU            TO LISTE-IO-AREA (51:15)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'INNLEGGING AV GAMLE RES' TO LISTE-IO-AREA (3:23)
               MOVE 'KONTROPOSTER PÅ NYE FIRM' TO LISTE-IO-AREA (26:24)
               MOVE 'AER'                  TO LISTE-IO-AREA (50:3)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL RECORDS'       TO LISTE-IO-AREA (2:14)
               MOVE NYERES                 TO EDIT-NYERES
               MOVE EDIT-NYERES            TO LISTE-IO-AREA (23:6)
               MOVE 'TOTAL BELØP'          TO LISTE-IO-AREA (30:11)
               MOVE NYEBEL                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (51:15)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE NYEBEU                 TO XO-112YY9R
               MOVE XO-112YY9R             TO LISTE-IO-AREA (48:18)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO AVSTEMM-IO-AREA
               INITIALIZE AVSTEMM-IO-AREA
               MOVE DDATO8-IO              TO AVSTEMM-IO-AREA (1:8)
               MOVE '010'                  TO AVSTEMM-IO-AREA (9:3)
               MOVE 'REG'                  TO AVSTEMM-IO-AREA (12:3)
               MOVE 'RKO021'               TO AVSTEMM-IO-AREA (15:6)
               MOVE '*RKO021*'             TO AVSTEMM-IO-AREA (21:8)
               MOVE TIDSP-IO               TO AVSTEMM-IO-AREA (30:6)
               MOVE LRRGAN-IO              TO AVSTEMM-IO-AREA (36:13)
               MOVE LRRGBE-IO              TO AVSTEMM-IO-AREA (49:15)
               MOVE LRRGBU-IO              TO AVSTEMM-IO-AREA (64:15)
               MOVE DDATO8-IO              TO AVSTEMM-IO-AREA (79:8)
               WRITE AVSTEMM-IO-AREA
               MOVE SPACES TO AVSTEMM-IO-AREA
               INITIALIZE AVSTEMM-IO-AREA
               MOVE '********'             TO AVSTEMM-IO-AREA (1:8)
               MOVE '010'                  TO AVSTEMM-IO-AREA (9:3)
               MOVE 'REG'                  TO AVSTEMM-IO-AREA (12:3)
               MOVE '******'               TO AVSTEMM-IO-AREA (15:6)
               MOVE '*RKO021*'             TO AVSTEMM-IO-AREA (21:8)
               MOVE TIDSP-IO               TO AVSTEMM-IO-AREA (30:6)
               MOVE LRRGAN-IO              TO AVSTEMM-IO-AREA (36:13)
               MOVE LRRGBE-IO              TO AVSTEMM-IO-AREA (49:15)
               MOVE LRRGBU-IO              TO AVSTEMM-IO-AREA (64:15)
               MOVE DDATO8-IO              TO AVSTEMM-IO-AREA (79:8)
               WRITE AVSTEMM-IO-AREA
               MOVE SPACES TO AVSTEMM-IO-AREA
               INITIALIZE AVSTEMM-IO-AREA
               MOVE DDATO8-IO              TO AVSTEMM-IO-AREA (1:8)
               MOVE '010'                  TO AVSTEMM-IO-AREA (9:3)
               MOVE 'RES'                  TO AVSTEMM-IO-AREA (12:3)
               MOVE 'RKO021'               TO AVSTEMM-IO-AREA (15:6)
               MOVE '*RKO021*'             TO AVSTEMM-IO-AREA (21:8)
               MOVE TIDSP-IO               TO AVSTEMM-IO-AREA (30:6)
               MOVE LRRSAN-IO              TO AVSTEMM-IO-AREA (36:13)
               MOVE LRRSBE-IO              TO AVSTEMM-IO-AREA (49:15)
               MOVE LERSBU-IO              TO AVSTEMM-IO-AREA (64:15)
               MOVE DDATO8-IO              TO AVSTEMM-IO-AREA (79:8)
               WRITE AVSTEMM-IO-AREA
               MOVE SPACES TO AVSTEMM-IO-AREA
               INITIALIZE AVSTEMM-IO-AREA
               MOVE '********'             TO AVSTEMM-IO-AREA (1:8)
               MOVE '010'                  TO AVSTEMM-IO-AREA (9:3)
               MOVE 'RES'                  TO AVSTEMM-IO-AREA (12:3)
               MOVE '******'               TO AVSTEMM-IO-AREA (15:6)
               MOVE '*RKO021*'             TO AVSTEMM-IO-AREA (21:8)
               MOVE TIDSP-IO               TO AVSTEMM-IO-AREA (30:6)
               MOVE LRRSAN-IO              TO AVSTEMM-IO-AREA (36:13)
               MOVE LRRSBE-IO              TO AVSTEMM-IO-AREA (49:15)
               MOVE LERSBU-IO              TO AVSTEMM-IO-AREA (64:15)
               MOVE DDATO8-IO              TO AVSTEMM-IO-AREA (79:8)
               WRITE AVSTEMM-IO-AREA
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
           MOVE 5                          TO LR-CHECK
           INITIALIZE ART03-DATA-FIELDS
           SET ART03-EOF-OFF               TO TRUE
           SET ART03-PROCESS               TO TRUE
           OPEN INPUT ART03
           INITIALIZE ART48-DATA-FIELDS
           SET ART48-EOF-OFF               TO TRUE
           SET ART48-PROCESS               TO TRUE
           OPEN INPUT ART48
           INITIALIZE FRESKF-DATA-FIELDS
           SET FRESKF-EOF-OFF              TO TRUE
           SET FRESKF-PROCESS              TO TRUE
           OPEN INPUT FRESKF
           INITIALIZE FREGF-DATA-FIELDS
           SET FREGF-EOF-OFF               TO TRUE
           SET FREGF-PROCESS               TO TRUE
           OPEN INPUT FREGF
           INITIALIZE UTERESK-DATA-FIELDS
           SET UTERESK-EOF-OFF             TO TRUE
           SET UTERESK-PROCESS             TO TRUE
           OPEN INPUT UTERESK
           OPEN OUTPUT DAGRESK
           OPEN OUTPUT DAGREGN
           OPEN OUTPUT AVSTEMM
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE ART03
           CLOSE ART48
           CLOSE FRESKF
           CLOSE FREGF
           CLOSE UTERESK
           CLOSE DAGRESK
           CLOSE DAGREGN
           CLOSE AVSTEMM
           IF LISTE-IO-AREA NOT = SPACES
             WRITE LISTE-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE-IO-AREA
           END-IF
           CLOSE LISTE.
 
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
