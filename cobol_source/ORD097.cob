       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORD097R.
      **********************************************  Z-WIN-RPG2      *
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ORD097.rpg
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
           SELECT MEKOTAB
               ASSIGN TO UT-S-MEKOTAB
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS MEKOTAB-STATUS.
           SELECT INNPUT
               ASSIGN TO UT-S-INNPUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INNPUT-STATUS.
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
           SELECT VAREMAS
               ASSIGN TO VAREMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAREMAS-STATUS
               RECORD KEY IS VAREMAS-KEY1.
           SELECT VARETIL
               ASSIGN TO VARETIL
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VARETIL-STATUS
               RECORD KEY IS VARETIL-KEY1.
           SELECT VAGRMAS
               ASSIGN TO VAGRMAS
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS VAGRMAS-STATUS
               RECORD KEY IS VAGRMAS-KEY1.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT OUTPUT-X
               ASSIGN TO OUTPUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTPUT-X-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD MEKOTAB
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  MEKOTAB-IO-AREA.
           05  MEKOTAB-IO-AREA-X           PICTURE X(80).
       FD INNPUT
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  INNPUT-IO-AREA.
           05  INNPUT-IO-AREA-X            PICTURE X(200).
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
       FD VAREMAS
               RECORD CONTAINS 200.
       01  VAREMAS-IO-AREA.
           05  VAREMAS-IO-AREA-X.
               10  VAREMAS-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       FD VARETIL
               RECORD CONTAINS 200.
       01  VARETIL-IO-AREA.
           05  VARETIL-IO-AREA-X.
               10  VARETIL-KEY1            PICTURE X(12).
               10  FILLER                  PICTURE X(188).
       FD VAGRMAS
               RECORD CONTAINS 80.
       01  VAGRMAS-IO-AREA.
           05  VAGRMAS-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  VAGRMAS-KEY1            PICTURE X(8).
               10  FILLER                  PICTURE X(71).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD OUTPUT-X
               RECORD CONTAINS 600.
       01  OUTPUT-X-IO-AREA.
           05  OUTPUT-X-IO-AREA-X          PICTURE X(600).
       WORKING-STORAGE SECTION.
       77  TABA-MAX   VALUE 100            PICTURE 9(4) USAGE BINARY.
       77  TABID-MAX   VALUE 100           PICTURE 9(4) USAGE BINARY.
       01  TABLES.
           05  TABA-TABLE.
               10  TABA-ENTRY
                                           OCCURS 100 TIMES
                                           INDEXED BY TABA-I
                                                      TABA-S
                                                      TABID-I
                                                      TABID-S.
                   15  TABA                PICTURE X(3).
                   15  TABID               PICTURE X(4).
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  MEKOTAB-STATUS              PICTURE 99 VALUE 0.
           10  INNPUT-STATUS               PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  KUNDEMX-STATUS              PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  VARETIL-STATUS              PICTURE 99 VALUE 0.
           10  VAGRMAS-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  OUTPUT-X-STATUS             PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  MEKOTAB-EOF-OFF         VALUE '0'.
               88  MEKOTAB-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNPUT-EOF-OFF          VALUE '0'.
               88  INNPUT-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNPUT-READ-OFF         VALUE '0'.
               88  INNPUT-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNPUT-PROCESS-OFF      VALUE '0'.
               88  INNPUT-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  INNPUT-LEVEL-INIT-OFF   VALUE '0'.
               88  INNPUT-LEVEL-INIT       VALUE '1'.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  KUNDEMX-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  VAREMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  VARETIL-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  VAGRMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  INNPUT-LEVEL-01.
               10  INNPUT-01-L1.
                   15  INNPUT-01-L1-FIRMA  PICTURE X(3).
           05  INNPUT-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  VNAVN                   PICTURE X(30).
               10  ANTLEV-IO.
                   15  ANTLEV              PICTURE S9(5)V9(2).
               10  ORPRIS-IO.
                   15  ORPRIS              PICTURE S9(7)V9(2).
               10  ORDNR                   PICTURE X(6).
               10  ORDNR1                  PICTURE X(1).
               10  VGR                     PICTURE X(5).
               10  KOSPRI-IO.
                   15  KOSPRI              PICTURE S9(7)V9(2).
               10  OPRIMM-IO.
                   15  OPRIMM              PICTURE S9(7)V9(2).
               10  AAMMDD                  PICTURE X(6).
               10  AA                      PICTURE X(2).
               10  MM                      PICTURE X(2).
               10  DD                      PICTURE X(2).
               10  KUNDNR                  PICTURE X(6).
               10  ORDMOT                  PICTURE X(2).
               10  EDBNR                   PICTURE X(7).
               10  BETM                    PICTURE X(2).
               10  AVD                     PICTURE X(1).
               10  REKVNR                  PICTURE X(15).
               10  RABB1-IO.
                   15  RABB1               PICTURE S9(2)V9(1).
               10  RABB2-IO.
                   15  RABB2               PICTURE S9(2)V9(1).
               10  RABB3-IO.
                   15  RABB3               PICTURE S9(2)V9(1).
               10  REGKL-IO.
                   15  REGKL               PICTURE S9(6).
               10  PRITIL-IO.
                   15  PRITIL              PICTURE S9(5)V9(2).
               10  PTTYPE                  PICTURE X(1).
               10  ANTBES-IO.
                   15  ANTBES              PICTURE S9(5)V9(2).
               10  LK                      PICTURE X(2).
               10  STATUS-X                PICTURE X(1).
               10  VEILPR-IO.
                   15  VEILPR              PICTURE S9(7)V9(2).
               10  FMDATO-IO.
                   15  FMDATO              PICTURE S9(8).
               10  FMAA                    PICTURE X(2).
               10  FMMM                    PICTURE X(2).
               10  FMDD                    PICTURE X(2).
               10  FMTID-IO.
                   15  FMTID               PICTURE S9(8).
           05  KUNDEMA-DATA-FIELDS.
               10  KNAVN1                  PICTURE X(30).
               10  KAT-IO.
                   15  KAT                 PICTURE S9(3) USAGE
                                                       PACKED-DECIMAL.
               10  HND-IO.
                   15  HND                 PICTURE S9(3).
           05  KUNDEMX-DATA-FIELDS.
               10  SNR1                    PICTURE X(3).
               10  KORGNR                  PICTURE X(9).
           05  VAREMAS-DATA-FIELDS.
               10  UTPRIS-IO.
                   15  UTPRIS              PICTURE S9(7)V9(2).
               10  BEHINN-IO.
                   15  BEHINN              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  BEHUT-IO.
                   15  BEHUT               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  STATK                   PICTURE X(3).
               10  PRODG1                  PICTURE X(7).
               10  LEVRAB-IO.
                   15  LEVRAB              PICTURE S9(2)V9(1) USAGE
                                                       PACKED-DECIMAL.
               10  LEVPRI-IO.
                   15  LEVPRI              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
           05  VARETIL-DATA-FIELDS.
               10  LEVNR1-IO.
                   15  LEVNR1              PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  SINNPR-IO.
                   15  SINNPR              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  PRODG2                  PICTURE X(4).
               10  PRODG3                  PICTURE X(3).
               10  PRODG4                  PICTURE X(4).
               10  VVEILP-IO.
                   15  VVEILP              PICTURE S9(7)V9(2).
           05  VAGRMAS-DATA-FIELDS.
               10  TKONTO                  PICTURE X(4).
           05  FIRMAF-DATA-FIELDS.
               10  KONFNR                  PICTURE X(3).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L1            PICTURE X(3).
           05  TEMPORARY-FIELDS.
               10  TELLER-IO.
                   15  TELLER              PICTURE S9(6).
               10  MEKOID                  PICTURE X(4).
               10  KEY9                    PICTURE X(9).
               10  KNAVN                   PICTURE X(30).
               10  KKAT-IO.
                   15  KKAT                PICTURE S9(3).
               10  KAT-N-IO.
                   15  KAT-N               PICTURE S9(3).
               10  KHND-IO.
                   15  KHND                PICTURE S9(3).
               10  KEY10                   PICTURE X(10).
               10  KEYVAR                  PICTURE X(10).
               10  KEY8                    PICTURE X(8).
               10  KEY12                   PICTURE X(12).
               10  LEVNRN-IO.
                   15  LEVNRN              PICTURE S9(6).
               10  LEVNR                   PICTURE X(6).
               10  LEVNVN                  PICTURE X(30).
               10  BEHOLD-IO.
                   15  BEHOLD              PICTURE S9(7)V9(2).
               10  NULL9-IO.
                   15  NULL9               PICTURE S9(9).
               10  SUMBEL-IO.
                   15  SUMBEL              PICTURE S9(7)V9(2).
               10  SUM1-IO.
                   15  SUM1                PICTURE S9(9)V9(2).
               10  RABBEL-IO.
                   15  RABBEL              PICTURE S9(7)V9(2).
               10  SUMMVA-IO.
                   15  SUMMVA              PICTURE S9(7)V9(2).
               10  SUMORD-IO.
                   15  SUMORD              PICTURE S9(7)V9(2).
               10  SUMKST-IO.
                   15  SUMKST              PICTURE S9(7)V9(2).
               10  BRF-IO.
                   15  BRF                 PICTURE S9(7)V9(2).
               10  TOTBRF-IO.
                   15  TOTBRF              PICTURE S9(7)V9(2).
               10  TOTUTP-IO.
                   15  TOTUTP              PICTURE S9(7)V9(2).
               10  TOTLEV-IO.
                   15  TOTLEV              PICTURE S9(7)V9(2).
               10  TOTIPR-IO.
                   15  TOTIPR              PICTURE S9(7)V9(2).
               10  TOTVEI-IO.
                   15  TOTVEI              PICTURE S9(7)V9(2).
           05  EDITTING-FIELDS.
               10  XO-52YN9                PICTURE ZZZZZ,99.
               10  XO-72YN9                PICTURE ZZZZZZZ,99.
               10  XO-21YN9                PICTURE ZZ,9.
               10  XO-60YN9                PICTURE ZZZZZ9.
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
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
           SET NOT-I-04                    TO TRUE
           SET NOT-I-06                    TO TRUE
           SET NOT-I-05                    TO TRUE
           SET NOT-I-07                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  INNPUT-PROCESS
               SET INNPUT-PROCESS-OFF      TO TRUE
               SET INNPUT-READ             TO TRUE
           END-IF
 
           IF  INNPUT-READ
           AND RECORD-SELECTED-OFF
               PERFORM INNPUT-GET
               SET INNPUT-READ-OFF         TO TRUE
               IF  NOT INNPUT-EOF
                   SET INNPUT-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  INNPUT-PROCESS
               PERFORM INNPUT-IDSET
           END-IF
 
           IF  INNPUT-PROCESS
               PERFORM INNPUT-CHK-LEVEL
           END-IF
 
           IF I-1ST
               GO TO LR-INDICATOR-TEST
           END-IF.
 
       MAINLINE-TOTAL-CALCS.
           CONTINUE.
 
       LR-INDICATOR-TEST.
           IF  I-LR
               GO TO MAINLINE-TERMINATION
           END-IF
 
           IF  INNPUT-PROCESS
               PERFORM INNPUT-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  INNPUT-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-01)
               SET NOT-I-21                TO TRUE
               SET NOT-I-22                TO TRUE
      *  NULLSTILLER TELLER PR FIRMA
           END-IF
           IF  (I-L1)
               MOVE 0                      TO TELLER
      *  TELLER PR LINJE
           END-IF
           IF  (I-01)
               ADD 1                       TO TELLER
      *
      *  SJEKK PÅ FNR FOR MEKOID
      *
           END-IF
           SET NOT-I-30                    TO TRUE
           SET TABA-S                      TO TABA-I
           PERFORM WITH TEST AFTER
                   VARYING TABA-I FROM 1 BY 1
                     UNTIL TABA-I >= TABA-MAX
                        OR I-30
               IF  FIRMA = TABA (TABA-I)
                   SET I-30                TO TRUE
                   SET TABA-S              TO TABA-I
               END-IF
           END-PERFORM
           SET TABA-I                      TO TABA-S
           IF  I-30
           AND TABA-I NOT > TABID-MAX
               SET TABID-I                 TO TABA-I
           END-IF
           IF  (I-30)
               MOVE TABID(TABID-I)         TO MEKOID
           END-IF
           IF  (I-L1)
               MOVE FIRMA                  TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-17                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-17            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
           END-IF
           IF  (I-L1 AND NOT-I-17)
               SET NOT-I-19                TO TRUE
               IF  KONFNR > '000'
                   SET I-19                TO TRUE
               END-IF
      *  FINNER KUNDEOPPLYSN FRA KUNDEMA
           END-IF
           MOVE FIRMA                      TO KEY9 (1:3)
           IF  (I-19)
               MOVE KONFNR                 TO KEY9 (1:3)
           END-IF
           MOVE KUNDNR                     TO KEY9 (4:6)
           MOVE KEY9                       TO KUNDEMA-KEY1
           READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
           INVALID KEY
               SET I-11                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-11                TO TRUE
               PERFORM KUNDEMA-FLDSET
               PERFORM KUNDEMA-IDSET
           END-READ
           IF  (NOT-I-11)
               MOVE KNAVN1                 TO KNAVN
               MOVE KAT                    TO KAT-N
               MOVE KAT-N-IO               TO KKAT-IO
               MOVE HND                    TO KHND-IO
      *  FINNER KUNDEOPPLYSN FRA KUNDEMX
           END-IF
           MOVE KEY9                       TO KEY10 (1:9)
           MOVE '1'                        TO KEY10 (10:1)
           MOVE KEY10                      TO KUNDEMX-KEY1
           READ KUNDEMX RECORD KEY IS KUNDEMX-KEY1
           INVALID KEY
               SET I-12                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-12                TO TRUE
               PERFORM KUNDEMX-FLDSET
               PERFORM KUNDEMX-IDSET
           END-READ
      *  FINNER OPPLYSN FRA VAREMAS
           MOVE FIRMA                      TO KEYVAR (1:3)
           MOVE EDBNR                      TO KEYVAR (4:7)
           MOVE KEYVAR                     TO VAREMAS-KEY1
           READ VAREMAS RECORD KEY IS VAREMAS-KEY1
           INVALID KEY
               SET I-13                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-13                TO TRUE
               PERFORM VAREMAS-FLDSET
               PERFORM VAREMAS-IDSET
           END-READ
      *  FINNER OPPLYSN FRA VAGRMAS
           MOVE FIRMA                      TO KEY8 (1:3)
           MOVE VGR                        TO KEY8 (4:5)
           MOVE KEY8                       TO VAGRMAS-KEY1
           READ VAGRMAS RECORD KEY IS VAGRMAS-KEY1
           INVALID KEY
               SET I-14                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-14                TO TRUE
               PERFORM VAGRMAS-FLDSET
               PERFORM VAGRMAS-IDSET
           END-READ
           IF  (NOT-I-14)
               SET NOT-I-16                TO TRUE
               IF  TKONTO = '0000'
                   SET I-16                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-14 AND NOT-I-16)
               SET NOT-I-16                TO TRUE
               IF  TKONTO = '    '
                   SET I-16                TO TRUE
               END-IF
      *
      *  FINNER OPPLYSNINGER FRA VARETIL
      *                    MOVEL"81"      KEY12  12
           END-IF
           MOVE '80'                       TO KEY12 (1:2)
           MOVE KEYVAR                     TO KEY12 (3:10)
           MOVE KEY12                      TO VARETIL-KEY1
           READ VARETIL RECORD KEY IS VARETIL-KEY1
           INVALID KEY
               SET I-31                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-31                TO TRUE
               PERFORM VARETIL-FLDSET
               PERFORM VARETIL-IDSET
           END-READ
           IF  (NOT-I-31)
               ADD LEVNR1 TO ZERO      GIVING LEVNRN
               MOVE LEVNRN                 TO LEVNR
      *  FINNER LEVNAVN FRA KUNDEMA
           END-IF
           IF  (NOT-I-31)
               MOVE FIRMA                  TO KEY9 (1:3)
               MOVE LEVNR                  TO KEY9 (4:6)
               MOVE KEY9                   TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-32                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-32            TO TRUE
                   PERFORM KUNDEMA-FLDSET
                   PERFORM KUNDEMA-IDSET
               END-READ
           END-IF
           IF  (NOT-I-31 AND NOT-I-32)
               MOVE KNAVN1                 TO LEVNVN
      *  SJEKKER OM KONTANTSALG
           END-IF
           SET NOT-I-21                    TO TRUE
           IF  BETM = '07'
               SET I-21                    TO TRUE
           END-IF
           IF  (NOT-I-21)
               SET NOT-I-21                TO TRUE
               IF  BETM = '14'
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-21)
               SET NOT-I-21                TO TRUE
               IF  BETM = '47'
                   SET I-21                TO TRUE
               END-IF
      *  FINNER BEHOLDNING
           END-IF
           SUBTRACT BEHUT FROM BEHINN  GIVING BEHOLD
           MOVE 0                          TO NULL9
      *  SJEKKER OM ORDRENR BEGYNNER MED 9
           SET NOT-I-22                    TO TRUE
           IF  ORDNR1 = '9'
               SET I-22                    TO TRUE
           END-IF
      *  TREKKER FRA RABBAT
           ADD ORPRIS TO ZERO          GIVING SUMBEL
           MULTIPLY RABB1 BY SUMBEL    GIVING SUM1
           DIVIDE SUM1 BY 100          GIVING RABBEL ROUNDED
           SUBTRACT RABBEL                 FROM SUMBEL
           MULTIPLY RABB2 BY SUMBEL    GIVING SUM1
           DIVIDE SUM1 BY 100          GIVING RABBEL ROUNDED
           SUBTRACT RABBEL                 FROM SUMBEL
           MULTIPLY RABB3 BY SUMBEL    GIVING SUM1
           DIVIDE SUM1 BY 100          GIVING RABBEL ROUNDED
           SUBTRACT RABBEL                 FROM SUMBEL
      * LEGGER TIL PRISTILLEGGET
      *          SUMBEL    ADD  PRITIL    SUMBEL
      * LEGGER TIL MVA
           MULTIPLY 1,25 BY SUMBEL     GIVING SUMMVA
      *
      * DIVERSE UTREGNINGER
           MULTIPLY ANTLEV BY SUMBEL   GIVING SUMORD
           MULTIPLY ANTLEV BY KOSPRI   GIVING SUMKST
           SUBTRACT KOSPRI FROM SUMBEL GIVING BRF
           MULTIPLY ANTLEV BY BRF      GIVING TOTBRF
           MULTIPLY ANTLEV BY UTPRIS   GIVING TOTUTP
           MULTIPLY ANTLEV BY LEVPRI   GIVING TOTLEV
           MULTIPLY ANTLEV BY SINNPR   GIVING TOTIPR
           MULTIPLY ANTLEV BY VVEILP   GIVING TOTVEI
      *
      * SJEKKER OM NEGATIVE TALL
           SET NOT-I-40                    TO TRUE
           IF  BRF < 0,00
               SET I-40                    TO TRUE
           END-IF
           SET NOT-I-41                    TO TRUE
           IF  TOTBRF < 0,00
               SET I-41                    TO TRUE
           END-IF.
 
       INNPUT-GET SECTION.
       INNPUT-GET-P.
           IF  INNPUT-EOF-OFF
               READ INNPUT
               AT END
                   SET INNPUT-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INNPUT-FLDSET SECTION.
       INNPUT-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INNPUT-IO-AREA (1:3)   TO FIRMA (1:3)
               MOVE INNPUT-IO-AREA (4:3)   TO ALFA (1:3)
               MOVE INNPUT-IO-AREA (7:20)  TO ARTNR (1:20)
               MOVE INNPUT-IO-AREA (27:30) TO VNAVN (1:30)
               MOVE INNPUT-IO-AREA (57:7)  TO ANTLEV-IO
               INSPECT ANTLEV-IO REPLACING ALL ' ' BY '0'
               MOVE INNPUT-IO-AREA (64:9)  TO ORPRIS-IO
               INSPECT ORPRIS-IO REPLACING ALL ' ' BY '0'
               MOVE INNPUT-IO-AREA (73:6)  TO ORDNR (1:6)
               MOVE INNPUT-IO-AREA (73:1)  TO ORDNR1 (1:1)
               MOVE INNPUT-IO-AREA (79:5)  TO VGR (1:5)
               MOVE INNPUT-IO-AREA (84:9)  TO KOSPRI-IO
               INSPECT KOSPRI-IO REPLACING ALL ' ' BY '0'
               MOVE INNPUT-IO-AREA (93:9)  TO OPRIMM-IO
               INSPECT OPRIMM-IO REPLACING ALL ' ' BY '0'
               MOVE INNPUT-IO-AREA (102:6) TO AAMMDD (1:6)
               MOVE INNPUT-IO-AREA (102:2) TO AA (1:2)
               MOVE INNPUT-IO-AREA (104:2) TO MM (1:2)
               MOVE INNPUT-IO-AREA (106:2) TO DD (1:2)
               MOVE INNPUT-IO-AREA (108:6) TO KUNDNR (1:6)
               MOVE INNPUT-IO-AREA (114:2) TO ORDMOT (1:2)
               MOVE INNPUT-IO-AREA (116:7) TO EDBNR (1:7)
               MOVE INNPUT-IO-AREA (123:2) TO BETM (1:2)
               MOVE INNPUT-IO-AREA (125:1) TO AVD (1:1)
               MOVE INNPUT-IO-AREA (126:15) TO REKVNR (1:15)
               MOVE INNPUT-IO-AREA (141:3) TO RABB1-IO
               INSPECT RABB1-IO REPLACING ALL ' ' BY '0'
               MOVE INNPUT-IO-AREA (144:3) TO RABB2-IO
               INSPECT RABB2-IO REPLACING ALL ' ' BY '0'
               MOVE INNPUT-IO-AREA (147:3) TO RABB3-IO
               INSPECT RABB3-IO REPLACING ALL ' ' BY '0'
               MOVE INNPUT-IO-AREA (150:6) TO REGKL-IO
               INSPECT REGKL-IO REPLACING ALL ' ' BY '0'
               MOVE INNPUT-IO-AREA (156:7) TO PRITIL-IO
               INSPECT PRITIL-IO REPLACING ALL ' ' BY '0'
               MOVE INNPUT-IO-AREA (163:1) TO PTTYPE (1:1)
               MOVE INNPUT-IO-AREA (164:7) TO ANTBES-IO
               INSPECT ANTBES-IO REPLACING ALL ' ' BY '0'
               MOVE INNPUT-IO-AREA (171:2) TO LK (1:2)
               MOVE INNPUT-IO-AREA (173:1) TO STATUS-X (1:1)
               MOVE INNPUT-IO-AREA (174:9) TO VEILPR-IO
               INSPECT VEILPR-IO REPLACING ALL ' ' BY '0'
               MOVE INNPUT-IO-AREA (183:8) TO FMDATO-IO
               INSPECT FMDATO-IO REPLACING ALL ' ' BY '0'
               MOVE INNPUT-IO-AREA (189:2) TO FMAA (1:2)
               MOVE INNPUT-IO-AREA (187:2) TO FMMM (1:2)
               MOVE INNPUT-IO-AREA (185:2) TO FMDD (1:2)
               MOVE INNPUT-IO-AREA (191:8) TO FMTID-IO
               INSPECT FMTID-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       INNPUT-IDSET SECTION.
       INNPUT-IDSET-P.
           SET I-01                        TO TRUE.
 
       INNPUT-CHK-LEVEL SECTION.
       INNPUT-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO INNPUT-LEVEL-01
               MOVE INNPUT-IO-AREA (1:3)   TO INNPUT-01-L1-FIRMA
               IF  INNPUT-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INNPUT-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INNPUT-01-L1          TO THE-PRIOR-L1
               SET INNPUT-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (16:30) TO KNAVN1 (1:30)
               MOVE KUNDEMA-IO-AREA (162:2) TO KAT-IO
               MOVE KUNDEMA-IO-AREA (185:3) TO HND-IO
               INSPECT HND-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-02                        TO TRUE.
 
       KUNDEMX-FLDSET SECTION.
       KUNDEMX-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMX-IO-AREA (105:3) TO SNR1 (1:3)
               MOVE KUNDEMX-IO-AREA (180:9) TO KORGNR (1:9)
           END-EVALUATE.
 
       KUNDEMX-IDSET SECTION.
       KUNDEMX-IDSET-P.
           SET I-03                        TO TRUE.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (75:9) TO UTPRIS-IO
               INSPECT UTPRIS-IO REPLACING ALL ' ' BY '0'
               MOVE VAREMAS-IO-AREA (97:5) TO BEHINN-IO
               MOVE VAREMAS-IO-AREA (102:5) TO BEHUT-IO
               MOVE VAREMAS-IO-AREA (123:3) TO STATK (1:3)
               MOVE VAREMAS-IO-AREA (146:7) TO PRODG1 (1:7)
               MOVE VAREMAS-IO-AREA (153:2) TO LEVRAB-IO
               MOVE VAREMAS-IO-AREA (165:5) TO LEVPRI-IO
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-04                        TO TRUE.
 
       VARETIL-FLDSET SECTION.
       VARETIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VARETIL-IO-AREA (68:4) TO LEVNR1-IO
               MOVE VARETIL-IO-AREA (139:5) TO SINNPR-IO
               MOVE VARETIL-IO-AREA (169:4) TO PRODG2 (1:4)
               MOVE VARETIL-IO-AREA (173:3) TO PRODG3 (1:3)
               MOVE VARETIL-IO-AREA (176:4) TO PRODG4 (1:4)
               MOVE VARETIL-IO-AREA (180:9) TO VVEILP-IO
               INSPECT VVEILP-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       VARETIL-IDSET SECTION.
       VARETIL-IDSET-P.
           SET I-06                        TO TRUE.
 
       VAGRMAS-FLDSET SECTION.
       VAGRMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAGRMAS-IO-AREA (69:4) TO TKONTO (1:4)
           END-EVALUATE.
 
       VAGRMAS-IDSET SECTION.
       VAGRMAS-IDSET-P.
           SET I-05                        TO TRUE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (1:3)   TO KONFNR (1:3)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-07                        TO TRUE.
 
       MEKOTAB-LOAD SECTION.
       MEKOTAB-LOAD-P.
           OPEN INPUT MEKOTAB
           SET TABA-I                      TO 1
           PERFORM UNTIL MEKOTAB-EOF
               READ MEKOTAB
               AT END
                   SET MEKOTAB-EOF         TO TRUE
               NOT AT END
                   MOVE MEKOTAB-IO-AREA (1:7) TO TABA-ENTRY (TABA-I)
                   SET TABA-I              UP BY 1
               END-READ
           END-PERFORM
           CLOSE MEKOTAB.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               MOVE SPACES TO OUTPUT-X-IO-AREA
               INITIALIZE OUTPUT-X-IO-AREA
               MOVE FIRMA                  TO OUTPUT-X-IO-AREA (1:3)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (4:1)
               MOVE ORDNR                  TO OUTPUT-X-IO-AREA (5:6)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (11:1)
               MOVE 'O'                    TO OUTPUT-X-IO-AREA (13:1)
               IF  (I-22)
                   MOVE 'K'                TO OUTPUT-X-IO-AREA (13:1)
               END-IF
               MOVE ';'                    TO OUTPUT-X-IO-AREA (15:1)
               MOVE DD                     TO OUTPUT-X-IO-AREA (16:2)
               MOVE '.'                    TO OUTPUT-X-IO-AREA (18:1)
               MOVE MM                     TO OUTPUT-X-IO-AREA (19:2)
               MOVE '.'                    TO OUTPUT-X-IO-AREA (21:1)
               MOVE '20'                   TO OUTPUT-X-IO-AREA (22:2)
               MOVE AA                     TO OUTPUT-X-IO-AREA (24:2)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (26:1)
               MOVE KUNDNR                 TO OUTPUT-X-IO-AREA (27:6)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (33:1)
               IF  (NOT-I-11)
                   MOVE KNAVN              TO OUTPUT-X-IO-AREA (34:30)
               END-IF
               MOVE ';'                    TO OUTPUT-X-IO-AREA (64:1)
               MOVE ALFA                   TO OUTPUT-X-IO-AREA (65:3)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (68:1)
               MOVE ARTNR                  TO OUTPUT-X-IO-AREA (69:20)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (89:1)
               MOVE VNAVN                  TO OUTPUT-X-IO-AREA (90:30)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (120:1)
               MOVE ANTBES                 TO XO-52YN9
               MOVE XO-52YN9               TO OUTPUT-X-IO-AREA (123:8)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (131:1)
               MOVE ANTLEV                 TO XO-52YN9
               MOVE XO-52YN9               TO OUTPUT-X-IO-AREA (134:8)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (142:1)
               IF  (NOT-I-13)
                   MOVE UTPRIS             TO XO-72YN9
                   MOVE XO-72YN9           TO OUTPUT-X-IO-AREA (146:10)
               END-IF
               MOVE ';'                    TO OUTPUT-X-IO-AREA (156:1)
               MOVE ORPRIS                 TO XO-72YN9
               MOVE XO-72YN9               TO OUTPUT-X-IO-AREA (161:10)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (171:1)
               MOVE RABB1                  TO XO-21YN9
               MOVE XO-21YN9               TO OUTPUT-X-IO-AREA (176:4)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (180:1)
               MOVE RABB2                  TO XO-21YN9
               MOVE XO-21YN9               TO OUTPUT-X-IO-AREA (185:4)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (189:1)
               MOVE RABB3                  TO XO-21YN9
               MOVE XO-21YN9               TO OUTPUT-X-IO-AREA (194:4)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (198:1)
               MOVE SUMORD                 TO XO-72YN9
               MOVE XO-72YN9               TO OUTPUT-X-IO-AREA (202:10)
               IF  (I-22)
                   MOVE '-'                TO OUTPUT-X-IO-AREA (199:1)
               END-IF
               MOVE ';'                    TO OUTPUT-X-IO-AREA (212:1)
               MOVE VGR                    TO OUTPUT-X-IO-AREA (213:5)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (218:1)
               MOVE LK                     TO OUTPUT-X-IO-AREA (219:2)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (221:1)
               IF  (NOT-I-11)
                   MOVE KKAT-IO            TO OUTPUT-X-IO-AREA (222:3)
               END-IF
               MOVE ';'                    TO OUTPUT-X-IO-AREA (225:1)
               IF  (NOT-I-11)
                   MOVE KHND-IO            TO OUTPUT-X-IO-AREA (226:3)
               END-IF
               MOVE ';'                    TO OUTPUT-X-IO-AREA (229:1)
               IF  (NOT-I-12)
                   MOVE SNR1               TO OUTPUT-X-IO-AREA (230:3)
               END-IF
               MOVE ';'                    TO OUTPUT-X-IO-AREA (233:1)
               IF  (NOT-I-14)
                   MOVE TKONTO             TO OUTPUT-X-IO-AREA (235:4)
               END-IF
               IF  (I-14)
                   MOVE '3010'             TO OUTPUT-X-IO-AREA (235:4)
               END-IF
               IF  (NOT-I-14 AND I-16)
                   MOVE '3010'             TO OUTPUT-X-IO-AREA (235:4)
               END-IF
               MOVE ';'                    TO OUTPUT-X-IO-AREA (239:1)
               MOVE KOSPRI                 TO XO-72YN9
               MOVE XO-72YN9               TO OUTPUT-X-IO-AREA (244:10)
               IF  (I-22)
                   MOVE '-'                TO OUTPUT-X-IO-AREA (240:1)
               END-IF
               MOVE ';'                    TO OUTPUT-X-IO-AREA (254:1)
               MOVE SUMKST                 TO XO-72YN9
               MOVE XO-72YN9               TO OUTPUT-X-IO-AREA (258:10)
               IF  (I-22)
                   MOVE '-'                TO OUTPUT-X-IO-AREA (255:1)
               END-IF
               MOVE ';'                    TO OUTPUT-X-IO-AREA (268:1)
               MOVE BRF                    TO XO-72YN9
               MOVE XO-72YN9               TO OUTPUT-X-IO-AREA (272:10)
               IF  (I-40)
                   MOVE '-'                TO OUTPUT-X-IO-AREA (269:1)
               END-IF
               MOVE ';'                    TO OUTPUT-X-IO-AREA (282:1)
               MOVE TOTBRF                 TO XO-72YN9
               MOVE XO-72YN9               TO OUTPUT-X-IO-AREA (286:10)
               IF  (I-41)
                   MOVE '-'                TO OUTPUT-X-IO-AREA (283:1)
               END-IF
               MOVE ';'                    TO OUTPUT-X-IO-AREA (296:1)
               MOVE TOTUTP                 TO XO-72YN9
               MOVE XO-72YN9               TO OUTPUT-X-IO-AREA (300:10)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (310:1)
               MOVE VEILPR                 TO XO-72YN9
               MOVE XO-72YN9               TO OUTPUT-X-IO-AREA (314:10)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (324:1)
               MOVE REGKL-IO               TO OUTPUT-X-IO-AREA (325:6)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (331:1)
               MOVE PRITIL                 TO XO-52YN9
               MOVE XO-52YN9               TO OUTPUT-X-IO-AREA (334:8)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (342:1)
               MOVE PTTYPE                 TO OUTPUT-X-IO-AREA (344:1)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (345:1)
               MOVE PRODG1                 TO OUTPUT-X-IO-AREA (346:7)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (353:1)
               MOVE PRODG2                 TO OUTPUT-X-IO-AREA (354:4)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (358:1)
               MOVE PRODG3                 TO OUTPUT-X-IO-AREA (359:3)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (362:1)
               MOVE PRODG4                 TO OUTPUT-X-IO-AREA (363:4)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (367:1)
               MOVE LEVNRN-IO              TO OUTPUT-X-IO-AREA (368:6)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (374:1)
               IF  (NOT-I-32)
                   MOVE LEVNVN             TO OUTPUT-X-IO-AREA (375:30)
               END-IF
               MOVE ';'                    TO OUTPUT-X-IO-AREA (405:1)
               IF  (NOT-I-12)
                   MOVE KORGNR             TO OUTPUT-X-IO-AREA (406:9)
               END-IF
               MOVE ';'                    TO OUTPUT-X-IO-AREA (415:1)
               MOVE STATUS-X               TO OUTPUT-X-IO-AREA (418:1)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (421:1)
               MOVE MEKOID                 TO OUTPUT-X-IO-AREA (422:4)
               MOVE '-000'                 TO OUTPUT-X-IO-AREA (426:4)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (430:1)
               MOVE 'KREDIT    '           TO OUTPUT-X-IO-AREA (431:10)
               IF  (I-21)
                   MOVE 'KONTANT   '       TO OUTPUT-X-IO-AREA (431:10)
               END-IF
               MOVE ';'                    TO OUTPUT-X-IO-AREA (441:1)
               MOVE '          '           TO OUTPUT-X-IO-AREA (442:10)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (452:1)
               IF  (NOT-I-13)
                   MOVE LEVPRI             TO XO-72YN9
                   MOVE XO-72YN9           TO OUTPUT-X-IO-AREA (456:10)
               END-IF
               MOVE ';'                    TO OUTPUT-X-IO-AREA (466:1)
               IF  (NOT-I-31)
                   MOVE SINNPR             TO XO-72YN9
                   MOVE XO-72YN9           TO OUTPUT-X-IO-AREA (471:10)
               END-IF
               MOVE ';'                    TO OUTPUT-X-IO-AREA (481:1)
               MOVE TOTIPR                 TO XO-72YN9
               MOVE XO-72YN9               TO OUTPUT-X-IO-AREA (486:10)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (496:1)
               MOVE TOTLEV                 TO XO-72YN9
               MOVE XO-72YN9               TO OUTPUT-X-IO-AREA (501:10)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (511:1)
               MOVE TOTVEI                 TO XO-72YN9
               MOVE XO-72YN9               TO OUTPUT-X-IO-AREA (516:10)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (526:1)
               MOVE LEVRAB                 TO XO-21YN9
               MOVE XO-21YN9               TO OUTPUT-X-IO-AREA (531:4)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (535:1)
               MOVE TELLER                 TO XO-60YN9
               MOVE XO-60YN9               TO OUTPUT-X-IO-AREA (536:6)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (542:1)
               MOVE FMDD                   TO OUTPUT-X-IO-AREA (543:2)
               MOVE '.'                    TO OUTPUT-X-IO-AREA (545:1)
               MOVE FMMM                   TO OUTPUT-X-IO-AREA (546:2)
               MOVE '.'                    TO OUTPUT-X-IO-AREA (548:1)
               MOVE '20'                   TO OUTPUT-X-IO-AREA (549:2)
               MOVE FMAA                   TO OUTPUT-X-IO-AREA (551:2)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (553:1)
      *                        FMDATOX  552
      *                                 553 ";"
               WRITE OUTPUT-X-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P)
               MOVE SPACES TO OUTPUT-X-IO-AREA
               INITIALIZE OUTPUT-X-IO-AREA
               MOVE 'FNR'                  TO OUTPUT-X-IO-AREA (1:3)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (4:1)
               MOVE 'ORDNR '               TO OUTPUT-X-IO-AREA (5:6)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (11:1)
               MOVE 'F/K'                  TO OUTPUT-X-IO-AREA (12:3)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (15:1)
               MOVE 'ORDR DATO'            TO OUTPUT-X-IO-AREA (17:9)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (26:1)
               MOVE 'KUNDE '               TO OUTPUT-X-IO-AREA (27:6)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (33:1)
               MOVE 'KUNDE NAVN'           TO OUTPUT-X-IO-AREA (34:10)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (64:1)
               MOVE 'ALF'                  TO OUTPUT-X-IO-AREA (65:3)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (68:1)
               MOVE 'ARTIKKEL NR'          TO OUTPUT-X-IO-AREA (69:11)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (89:1)
               MOVE 'VAREBESKRIVELSE'      TO OUTPUT-X-IO-AREA (90:15)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (120:1)
               MOVE 'ANT BEST  '           TO OUTPUT-X-IO-AREA (121:10)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (131:1)
               MOVE 'ANT LEV   '           TO OUTPUT-X-IO-AREA (132:10)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (142:1)
               MOVE 'UTSALGSPRIS'          TO OUTPUT-X-IO-AREA (144:11)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (156:1)
               MOVE 'ORDREPRIS  '          TO OUTPUT-X-IO-AREA (158:11)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (171:1)
               MOVE 'RAB 1'                TO OUTPUT-X-IO-AREA (172:5)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (180:1)
               MOVE 'RAB 2'                TO OUTPUT-X-IO-AREA (181:5)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (189:1)
               MOVE 'RAB 3'                TO OUTPUT-X-IO-AREA (190:5)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (198:1)
               MOVE 'NETTO PRIS'           TO OUTPUT-X-IO-AREA (200:10)
      *                      22         199 "-"
               MOVE ';'                    TO OUTPUT-X-IO-AREA (212:1)
               MOVE 'VGR  '                TO OUTPUT-X-IO-AREA (213:5)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (218:1)
               MOVE 'LK'                   TO OUTPUT-X-IO-AREA (219:2)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (221:1)
               MOVE 'KAT'                  TO OUTPUT-X-IO-AREA (222:3)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (225:1)
               MOVE 'HND'                  TO OUTPUT-X-IO-AREA (226:3)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (229:1)
               MOVE 'SNR'                  TO OUTPUT-X-IO-AREA (230:3)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (233:1)
               MOVE 'KONTO'                TO OUTPUT-X-IO-AREA (234:5)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (239:1)
               MOVE 'KOSTPRIS  '           TO OUTPUT-X-IO-AREA (240:10)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (254:1)
               MOVE 'SUM KOSTPRIS '        TO OUTPUT-X-IO-AREA (255:13)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (268:1)
               MOVE 'BRF BEL      '        TO OUTPUT-X-IO-AREA (269:13)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (282:1)
               MOVE 'TOT. BRF     '        TO OUTPUT-X-IO-AREA (283:13)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (296:1)
               MOVE 'SUM UTSALGSPR'        TO OUTPUT-X-IO-AREA (297:13)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (310:1)
               MOVE 'VEIL PRIS    '        TO OUTPUT-X-IO-AREA (311:13)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (324:1)
               MOVE 'KLOKKE'               TO OUTPUT-X-IO-AREA (325:6)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (331:1)
               MOVE 'PRISTILL  '           TO OUTPUT-X-IO-AREA (332:10)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (342:1)
               MOVE 'PT'                   TO OUTPUT-X-IO-AREA (343:2)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (345:1)
               MOVE 'PRODG 1'              TO OUTPUT-X-IO-AREA (346:7)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (353:1)
               MOVE 'PG 2'                 TO OUTPUT-X-IO-AREA (354:4)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (358:1)
               MOVE 'PG3'                  TO OUTPUT-X-IO-AREA (359:3)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (362:1)
               MOVE 'PG 4'                 TO OUTPUT-X-IO-AREA (363:4)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (367:1)
               MOVE 'LEVNR '               TO OUTPUT-X-IO-AREA (368:6)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (374:1)
               MOVE 'LEV. NAVN '           TO OUTPUT-X-IO-AREA (375:10)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (405:1)
               MOVE 'ORGNR'                TO OUTPUT-X-IO-AREA (406:5)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (415:1)
               MOVE 'STA'                  TO OUTPUT-X-IO-AREA (416:3)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (421:1)
               MOVE 'MEKO ID'              TO OUTPUT-X-IO-AREA (423:7)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (430:1)
               MOVE 'KONT/KRED '           TO OUTPUT-X-IO-AREA (431:10)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (441:1)
               MOVE 'FRITT     '           TO OUTPUT-X-IO-AREA (442:10)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (452:1)
               MOVE 'LEV PRIS  '           TO OUTPUT-X-IO-AREA (453:10)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (466:1)
               MOVE 'SISTE INNPRIS'        TO OUTPUT-X-IO-AREA (468:13)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (481:1)
               MOVE 'SUM INNPRIS  '        TO OUTPUT-X-IO-AREA (483:13)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (496:1)
               MOVE 'SUM LEVPRIS  '        TO OUTPUT-X-IO-AREA (498:13)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (511:1)
               MOVE 'SUM VEILPRIS '        TO OUTPUT-X-IO-AREA (513:13)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (526:1)
               MOVE 'LEV RAB'              TO OUTPUT-X-IO-AREA (528:7)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (535:1)
               MOVE 'RADNR '               TO OUTPUT-X-IO-AREA (536:6)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (542:1)
               MOVE 'FM DATO '             TO OUTPUT-X-IO-AREA (545:8)
               MOVE ';'                    TO OUTPUT-X-IO-AREA (553:1)
               WRITE OUTPUT-X-IO-AREA
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
           PERFORM MEKOTAB-LOAD
           SET INNPUT-LEVEL-INIT           TO TRUE
           INITIALIZE INNPUT-DATA-FIELDS
           SET INNPUT-EOF-OFF              TO TRUE
           SET INNPUT-PROCESS              TO TRUE
           OPEN INPUT INNPUT
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           INITIALIZE KUNDEMX-DATA-FIELDS
           OPEN INPUT KUNDEMX
           INITIALIZE VAREMAS-DATA-FIELDS
           OPEN INPUT VAREMAS
           INITIALIZE VARETIL-DATA-FIELDS
           OPEN INPUT VARETIL
           INITIALIZE VAGRMAS-DATA-FIELDS
           OPEN INPUT VAGRMAS
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           OPEN OUTPUT OUTPUT-X.
           SET TABA-I                      TO 1
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INNPUT
           CLOSE KUNDEMA
           CLOSE KUNDEMX
           CLOSE VAREMAS
           CLOSE VARETIL
           CLOSE VAGRMAS
           CLOSE FIRMAF
           CLOSE OUTPUT-X.
 
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
