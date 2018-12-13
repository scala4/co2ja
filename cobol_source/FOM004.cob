       IDENTIFICATION DIVISION.
       PROGRAM-ID. FOM004R.
      **********************************************  Z-WIN-RPG2   ****
      * KOPI AV FAK051
      *
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FOM004.rpg
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
           SELECT INNFIL
               ASSIGN TO UT-S-INNFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INNFIL-STATUS.
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
           SELECT OUTFIL
               ASSIGN TO UT-S-OUTFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTFIL-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD INNFIL
               BLOCK CONTAINS 320
               RECORD CONTAINS 160.
       01  INNFIL-IO-AREA.
           05  INNFIL-IO-AREA-X            PICTURE X(160).
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
               RECORD CONTAINS 200.
       01  VAGRMAS-IO-AREA.
           05  VAGRMAS-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  VAGRMAS-KEY1            PICTURE X(8).
               10  FILLER                  PICTURE X(191).
       FD OUTFIL
               BLOCK CONTAINS 700
               RECORD CONTAINS 350.
       01  OUTFIL-IO-AREA.
           05  OUTFIL-IO-AREA-X            PICTURE X(350).
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
           10  INNFIL-STATUS               PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  KUNDEMX-STATUS              PICTURE 99 VALUE 0.
           10  VAREMAS-STATUS              PICTURE 99 VALUE 0.
           10  VARETIL-STATUS              PICTURE 99 VALUE 0.
           10  VAGRMAS-STATUS              PICTURE 99 VALUE 0.
           10  OUTFIL-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  INNFIL-EOF-OFF          VALUE '0'.
               88  INNFIL-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNFIL-READ-OFF         VALUE '0'.
               88  INNFIL-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNFIL-PROCESS-OFF      VALUE '0'.
               88  INNFIL-PROCESS          VALUE '1'.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  KUNDEMX-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  VAREMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  VARETIL-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  VAGRMAS-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  INNFIL-DATA-FIELDS.
               10  KONS                    PICTURE X(3).
               10  FAKMND                  PICTURE X(6).
               10  KUNDNR                  PICTURE X(6).
               10  SEQNR                   PICTURE X(5).
               10  ORDDTO                  PICTURE X(8).
               10  AA                      PICTURE X(2).
               10  MM                      PICTURE X(2).
               10  DD                      PICTURE X(2).
               10  TTMM                    PICTURE X(4).
               10  ORDNR                   PICTURE X(6).
               10  FIRMA                   PICTURE X(3).
               10  BM                      PICTURE X(2).
               10  LK                      PICTURE X(2).
               10  AVD                     PICTURE X(1).
               10  FK                      PICTURE X(1).
               10  KRTYPE                  PICTURE X(1).
               10  VGR                     PICTURE X(5).
               10  EDBNR                   PICTURE X(7).
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  ANTLEV-IO.
                   15  ANTLEV              PICTURE S9(5)V9(2).
               10  OPRIS-IO.
                   15  OPRIS               PICTURE S9(7)V9(2).
               10  RAB1-IO.
                   15  RAB1                PICTURE S9(2)V9(1).
               10  RAB2-IO.
                   15  RAB2                PICTURE S9(2)V9(1).
               10  RAB3-IO.
                   15  RAB3                PICTURE S9(2)V9(1).
               10  NTOSUM-IO.
                   15  NTOSUM              PICTURE S9(7)V9(2).
               10  SELVK-IO.
                   15  SELVK               PICTURE S9(7)V9(2).
               10  FAKTNR                  PICTURE X(6).
               10  ORDTYP                  PICTURE X(1).
               10  ORDMOT                  PICTURE X(2).
           05  KUNDEMA-DATA-FIELDS.
               10  KNAVN1                  PICTURE X(30).
               10  KKAT-IO.
                   15  KKAT                PICTURE S9(3) USAGE
                                                       PACKED-DECIMAL.
               10  HND                     PICTURE X(3).
           05  KUNDEMX-DATA-FIELDS.
               10  SNR                     PICTURE X(3).
               10  SERV                    PICTURE X(3).
               10  ORGNR                   PICTURE X(9).
           05  VAREMAS-DATA-FIELDS.
               10  VNAVN                   PICTURE X(30).
           05  VARETIL-DATA-FIELDS.
               10  LEVNR-IO.
                   15  LEVNR               PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  LARTNR                  PICTURE X(20).
               10  LEVBET                  PICTURE X(30).
           05  VAGRMAS-DATA-FIELDS.
               10  KONTO-IO.
                   15  KONTO               PICTURE S9(4).
      *****************************************************************
      * CHAIN MOT VAGRMAS FOR Å FINNE HB-KONTO                        *
      *****************************************************************
           05  TEMPORARY-FIELDS.
               10  KEY-X                   PICTURE X(8).
               10  ANT-IO.
                   15  ANT                 PICTURE S9(8).
               10  KEYVAR                  PICTURE X(10).
               10  KEYVAT                  PICTURE X(12).
               10  LEVER                   PICTURE X(6).
               10  LEVNR-N-IO.
                   15  LEVNR-N             PICTURE S9(7).
               10  KEY9                    PICTURE X(9).
               10  LNAVN                   PICTURE X(30).
               10  KEY10                   PICTURE X(10).
               10  OPRIMM-IO.
                   15  OPRIMM              PICTURE S9(7)V9(2).
               10  ANTF-IO.
                   15  ANTF                PICTURE S9(8).
           05  EDITTING-FIELDS.
               10  XO-80YY9                PICTURE ZZ.ZZZ.ZZ9.
               10  XO-52YN9                PICTURE ZZZZZ,99.
               10  XO-72YN9                PICTURE ZZZZZZZ,99.
               10  XO-21YN9                PICTURE ZZ,9.
               10  XO-30D                  PICTURE S9(3).
               10  XO-30U                  PICTURE 9(3).
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
           SET NOT-I-07                    TO TRUE
           SET NOT-I-06                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  INNFIL-PROCESS
               SET INNFIL-PROCESS-OFF      TO TRUE
               SET INNFIL-READ             TO TRUE
           END-IF
 
           IF  INNFIL-READ
           AND RECORD-SELECTED-OFF
               PERFORM INNFIL-GET
               SET INNFIL-READ-OFF         TO TRUE
               IF  NOT INNFIL-EOF
                   SET INNFIL-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  INNFIL-PROCESS
               PERFORM INNFIL-IDSET
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
 
           IF  INNFIL-PROCESS
               PERFORM INNFIL-FLDSET
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
           MOVE FIRMA                      TO KEY-X (1:3)
           MOVE VGR                        TO KEY-X (4:5)
           MOVE KEY-X                      TO VAGRMAS-KEY1
           READ VAGRMAS RECORD KEY IS VAGRMAS-KEY1
           INVALID KEY
               SET I-10                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-10                TO TRUE
               PERFORM VAGRMAS-FLDSET
               PERFORM VAGRMAS-IDSET
           END-READ
           IF  (NOT-I-10)
               SET NOT-I-17                TO TRUE
               IF  KONTO = 0000
                   SET I-17                TO TRUE
               END-IF
           END-IF
           IF  (I-01)
               SET NOT-I-50                TO TRUE
           END-IF
           ADD 1                           TO ANT
           SET NOT-I-82                    TO TRUE
           IF  KUNDNR = '115967'
               SET I-82                    TO TRUE
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '115968'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '119193'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '119194'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '119195'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '119197'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '119274'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '119276'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '119278'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '119582'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '119674'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '123739'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '131898'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '132510'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '136034'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '500555'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '500560'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '500563'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '500570'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '500580'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '500900'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '500950'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '500951'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '500952'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '500953'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '500954'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '500955'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '500959'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '500961'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '500962'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '500970'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '500956'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '500551'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '500963'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '123992'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '123994'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '123996'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '123993'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '123989'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '123985'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '137575'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '123991'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '128657'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '123986'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '136700'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '128658'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '139458'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '123997'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '123998'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '123990'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '127623'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '119584'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '119673'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '109793'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '109795'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '127622'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '109794'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '123740'
                   SET I-82                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-82)
               SET NOT-I-82                TO TRUE
               IF  KUNDNR = '500960'
                   SET I-82                TO TRUE
               END-IF
      *  FINNER OPPLYSN FRA VAREMAS
           END-IF
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
      *  FINNER OPPLYSN FRA VARETIL
           MOVE '80'                       TO KEYVAT (1:2)
           MOVE KEYVAR                     TO KEYVAT (3:10)
           MOVE KEYVAT                     TO VARETIL-KEY1
           READ VARETIL RECORD KEY IS VARETIL-KEY1
           INVALID KEY
               SET I-14                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-14                TO TRUE
               PERFORM VARETIL-FLDSET
               PERFORM VARETIL-IDSET
           END-READ
           IF  (NOT-I-14)
               MOVE LEVNR                  TO LEVNR-N
               MOVE LEVNR-N-IO (2:6)       TO LEVER
      *  FINNER KUNDEOPPLYSN FRA LEVNAVN
           END-IF
           MOVE FIRMA                      TO KEY9 (1:3)
           MOVE LEVER                      TO KEY9 (4:6)
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
               MOVE KNAVN1                 TO LNAVN
      *  FINNER KUNDEOPPLYSN FRA KUNDEMA
           END-IF
           MOVE FIRMA                      TO KEY9 (1:3)
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
      *****************************************************************
      * DIV SJEKKER                                                   *
      *****************************************************************
           SET NOT-I-31                    TO TRUE
           IF  RAB1 = 0
               SET I-31                    TO TRUE
           END-IF
           SET NOT-I-32                    TO TRUE
           IF  RAB2 = 0
               SET I-32                    TO TRUE
           END-IF
           SET NOT-I-33                    TO TRUE
           IF  RAB3 = 0
               SET I-33                    TO TRUE
           END-IF
      *  SJEKKER OM PRISTILLEGG
           SET NOT-I-19                    TO TRUE
           IF  LK = 'PT'
               SET I-19                    TO TRUE
           END-IF
      *****************************************************************
           SET NOT-I-81                    TO TRUE
           IF  FK = 'K'
               SET I-81                    TO TRUE
           END-IF
           SET NOT-I-21                    TO TRUE
           IF  ANTLEV = 0
               SET I-21                    TO TRUE
           END-IF
      *  81                Z-ADD0         SELVK
      *  21                Z-ADDSELVK     SELVK1  92
      * N21      SELVK     DIV  ANTLEV    SELVK1  92
           MULTIPLY 1,25 BY OPRIS      GIVING OPRIMM
           SET NOT-I-63                    TO TRUE
           IF  NTOSUM < 0,00
               SET I-63                    TO TRUE
           END-IF
           IF  (I-01)
               SET I-50                    TO TRUE
           END-IF
           IF  (I-50)
               ADD 1                       TO ANTF
           END-IF.
 
       SLUTT-T.
           CONTINUE.
 
       INNFIL-GET SECTION.
       INNFIL-GET-P.
           IF  INNFIL-EOF-OFF
               READ INNFIL
               AT END
                   SET INNFIL-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INNFIL-FLDSET SECTION.
       INNFIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INNFIL-IO-AREA (1:3)   TO KONS (1:3)
               MOVE INNFIL-IO-AREA (4:6)   TO FAKMND (1:6)
               MOVE INNFIL-IO-AREA (10:6)  TO KUNDNR (1:6)
               MOVE INNFIL-IO-AREA (16:5)  TO SEQNR (1:5)
               MOVE INNFIL-IO-AREA (21:8)  TO ORDDTO (1:8)
               MOVE INNFIL-IO-AREA (23:2)  TO AA (1:2)
               MOVE INNFIL-IO-AREA (25:2)  TO MM (1:2)
               MOVE INNFIL-IO-AREA (27:2)  TO DD (1:2)
               MOVE INNFIL-IO-AREA (29:4)  TO TTMM (1:4)
               MOVE INNFIL-IO-AREA (33:6)  TO ORDNR (1:6)
               MOVE INNFIL-IO-AREA (39:3)  TO FIRMA (1:3)
               MOVE INNFIL-IO-AREA (42:2)  TO BM (1:2)
               MOVE INNFIL-IO-AREA (44:2)  TO LK (1:2)
               MOVE INNFIL-IO-AREA (46:1)  TO AVD (1:1)
               MOVE INNFIL-IO-AREA (47:1)  TO FK (1:1)
               MOVE INNFIL-IO-AREA (48:1)  TO KRTYPE (1:1)
               MOVE INNFIL-IO-AREA (51:5)  TO VGR (1:5)
               MOVE INNFIL-IO-AREA (60:7)  TO EDBNR (1:7)
               MOVE INNFIL-IO-AREA (67:3)  TO ALFA (1:3)
               MOVE INNFIL-IO-AREA (70:20) TO ARTNR (1:20)
               MOVE INNFIL-IO-AREA (97:7)  TO ANTLEV-IO
               INSPECT ANTLEV-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (104:9) TO OPRIS-IO
               INSPECT OPRIS-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (113:3) TO RAB1-IO
               INSPECT RAB1-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (116:3) TO RAB2-IO
               INSPECT RAB2-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (119:3) TO RAB3-IO
               INSPECT RAB3-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (122:9) TO NTOSUM-IO
               INSPECT NTOSUM-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (131:9) TO SELVK-IO
               INSPECT SELVK-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (140:6) TO FAKTNR (1:6)
               MOVE INNFIL-IO-AREA (146:1) TO ORDTYP (1:1)
               MOVE INNFIL-IO-AREA (148:2) TO ORDMOT (1:2)
           END-EVALUATE.
 
       INNFIL-IDSET SECTION.
       INNFIL-IDSET-P.
           SET I-01                        TO TRUE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (16:30) TO KNAVN1 (1:30)
               MOVE KUNDEMA-IO-AREA (162:2) TO KKAT-IO
               MOVE KUNDEMA-IO-AREA (185:3) TO HND (1:3)
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-02                        TO TRUE.
 
       KUNDEMX-FLDSET SECTION.
       KUNDEMX-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMX-IO-AREA (105:3) TO SNR (1:3)
               MOVE KUNDEMX-IO-AREA (117:3) TO SERV (1:3)
               MOVE KUNDEMX-IO-AREA (180:9) TO ORGNR (1:9)
           END-EVALUATE.
 
       KUNDEMX-IDSET SECTION.
       KUNDEMX-IDSET-P.
           SET I-03                        TO TRUE.
 
       VAREMAS-FLDSET SECTION.
       VAREMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAREMAS-IO-AREA (36:30) TO VNAVN (1:30)
           END-EVALUATE.
 
       VAREMAS-IDSET SECTION.
       VAREMAS-IDSET-P.
           SET I-04                        TO TRUE.
 
       VARETIL-FLDSET SECTION.
       VARETIL-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VARETIL-IO-AREA (68:4) TO LEVNR-IO
               MOVE VARETIL-IO-AREA (13:20) TO LARTNR (1:20)
               MOVE VARETIL-IO-AREA (33:30) TO LEVBET (1:30)
               MOVE VARETIL-IO-AREA (68:4) TO LEVNR-IO
           END-EVALUATE.
 
       VARETIL-IDSET SECTION.
       VARETIL-IDSET-P.
           SET I-07                        TO TRUE.
 
       VAGRMAS-FLDSET SECTION.
       VAGRMAS-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE VAGRMAS-IO-AREA (69:4) TO KONTO-IO
               INSPECT KONTO-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       VAGRMAS-IDSET SECTION.
       VAGRMAS-IDSET-P.
           SET I-06                        TO TRUE.
 
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
           IF  (I-01 AND I-50)
               MOVE SPACES TO OUTFIL-IO-AREA
               INITIALIZE OUTFIL-IO-AREA
               MOVE ORDNR                  TO OUTFIL-IO-AREA (1:6)
               MOVE ';'                    TO OUTFIL-IO-AREA (7:1)
               MOVE SEQNR                  TO OUTFIL-IO-AREA (8:5)
               MOVE ';'                    TO OUTFIL-IO-AREA (13:1)
               MOVE FK                     TO OUTFIL-IO-AREA (15:1)
               MOVE ';'                    TO OUTFIL-IO-AREA (16:1)
               MOVE ORDDTO                 TO OUTFIL-IO-AREA (17:8)
               MOVE ';'                    TO OUTFIL-IO-AREA (25:1)
               MOVE KUNDNR                 TO OUTFIL-IO-AREA (26:6)
               MOVE ';'                    TO OUTFIL-IO-AREA (32:1)
               IF  (NOT-I-11)
                   MOVE KNAVN1             TO OUTFIL-IO-AREA (33:30)
               END-IF
               MOVE ';'                    TO OUTFIL-IO-AREA (63:1)
               IF  (I-11)
                   MOVE '* UKJENT KUNDE '  TO OUTFIL-IO-AREA (33:15)
               END-IF
               MOVE ';'                    TO OUTFIL-IO-AREA (63:1)
               MOVE ALFA                   TO OUTFIL-IO-AREA (64:3)
               MOVE ';'                    TO OUTFIL-IO-AREA (67:1)
               MOVE ARTNR                  TO OUTFIL-IO-AREA (68:20)
               MOVE ';'                    TO OUTFIL-IO-AREA (88:1)
               IF  (NOT-I-13)
                   MOVE VNAVN              TO OUTFIL-IO-AREA (89:30)
               END-IF
               MOVE ';'                    TO OUTFIL-IO-AREA (119:1)
               MOVE ANTLEV                 TO XO-52YN9
               MOVE XO-52YN9               TO OUTFIL-IO-AREA (126:8)
               MOVE ';'                    TO OUTFIL-IO-AREA (134:1)
               MOVE OPRIS                  TO XO-72YN9
               MOVE XO-72YN9               TO OUTFIL-IO-AREA (139:10)
               MOVE ';'                    TO OUTFIL-IO-AREA (149:1)
               MOVE RAB1                   TO XO-21YN9
               MOVE XO-21YN9               TO OUTFIL-IO-AREA (152:4)
               IF  (I-31)
                   MOVE '0,0'              TO OUTFIL-IO-AREA (153:3)
               END-IF
               MOVE ';'                    TO OUTFIL-IO-AREA (156:1)
               MOVE RAB2                   TO XO-21YN9
               MOVE XO-21YN9               TO OUTFIL-IO-AREA (159:4)
               IF  (I-32)
                   MOVE '0,0'              TO OUTFIL-IO-AREA (160:3)
               END-IF
               MOVE ';'                    TO OUTFIL-IO-AREA (163:1)
               MOVE RAB3                   TO XO-21YN9
               MOVE XO-21YN9               TO OUTFIL-IO-AREA (166:4)
               IF  (I-33)
                   MOVE '0,0'              TO OUTFIL-IO-AREA (167:3)
               END-IF
               MOVE ';'                    TO OUTFIL-IO-AREA (170:1)
               MOVE NTOSUM                 TO XO-72YN9
               MOVE XO-72YN9               TO OUTFIL-IO-AREA (175:10)
               IF  (I-63)
                   MOVE '-'                TO OUTFIL-IO-AREA (171:1)
               END-IF
               MOVE ';'                    TO OUTFIL-IO-AREA (185:1)
               MOVE VGR                    TO OUTFIL-IO-AREA (186:5)
               MOVE ';'                    TO OUTFIL-IO-AREA (191:1)
               MOVE LK                     TO OUTFIL-IO-AREA (192:2)
               MOVE ';'                    TO OUTFIL-IO-AREA (194:1)
               IF  (I-81)
                   MOVE KRTYPE             TO OUTFIL-IO-AREA (195:1)
               END-IF
               MOVE ';'                    TO OUTFIL-IO-AREA (196:1)
               IF  (I-82)
                   MOVE 'INTERNAL'         TO OUTFIL-IO-AREA (197:8)
               END-IF
               MOVE ';'                    TO OUTFIL-IO-AREA (205:1)
               MOVE KKAT                   TO XO-30U
               MOVE XO-30U (1:3)           TO OUTFIL-IO-AREA (206:3)
               MOVE ';'                    TO OUTFIL-IO-AREA (209:1)
               MOVE HND                    TO OUTFIL-IO-AREA (210:3)
               MOVE ';'                    TO OUTFIL-IO-AREA (213:1)
               MOVE SNR                    TO OUTFIL-IO-AREA (214:3)
               MOVE ';'                    TO OUTFIL-IO-AREA (217:1)
               MOVE SERV                   TO OUTFIL-IO-AREA (218:3)
               MOVE ';'                    TO OUTFIL-IO-AREA (221:1)
               MOVE FAKMND                 TO OUTFIL-IO-AREA (222:6)
               MOVE ';'                    TO OUTFIL-IO-AREA (228:1)
               MOVE '5000'                 TO OUTFIL-IO-AREA (229:4)
               IF  (NOT-I-17)
                   MOVE KONTO-IO           TO OUTFIL-IO-AREA (229:4)
               END-IF
               MOVE ';'                    TO OUTFIL-IO-AREA (233:1)
               MOVE SELVK                  TO XO-72YN9
               MOVE XO-72YN9               TO OUTFIL-IO-AREA (235:10)
               MOVE ';'                    TO OUTFIL-IO-AREA (245:1)
               MOVE LEVER                  TO OUTFIL-IO-AREA (246:6)
               MOVE ';'                    TO OUTFIL-IO-AREA (252:1)
               MOVE LNAVN                  TO OUTFIL-IO-AREA (253:30)
               INITIALIZE LNAVN
               MOVE ';'                    TO OUTFIL-IO-AREA (283:1)
               MOVE LARTNR                 TO OUTFIL-IO-AREA (284:20)
               MOVE ';'                    TO OUTFIL-IO-AREA (304:1)
               MOVE LEVBET                 TO OUTFIL-IO-AREA (305:30)
               MOVE ';'                    TO OUTFIL-IO-AREA (335:1)
               IF  (NOT-I-12)
                   MOVE ORGNR              TO OUTFIL-IO-AREA (336:9)
               END-IF
               MOVE ';'                    TO OUTFIL-IO-AREA (345:1)
               WRITE OUTFIL-IO-AREA
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-1P)
               MOVE SPACES TO OUTFIL-IO-AREA
               INITIALIZE OUTFIL-IO-AREA
               MOVE 'DOCUMENT NR '         TO OUTFIL-IO-AREA (1:12)
               MOVE ';'                    TO OUTFIL-IO-AREA (13:1)
               MOVE 'ROW '                 TO OUTFIL-IO-AREA (14:4)
               MOVE ';'                    TO OUTFIL-IO-AREA (18:1)
               MOVE 'F=INVOICE K=REFUND  ' TO OUTFIL-IO-AREA (19:20)
               MOVE ';'                    TO OUTFIL-IO-AREA (39:1)
               MOVE 'ORDER DATE '          TO OUTFIL-IO-AREA (40:11)
               MOVE ';'                    TO OUTFIL-IO-AREA (51:1)
               MOVE 'CUSTOMER '            TO OUTFIL-IO-AREA (52:9)
               MOVE ';'                    TO OUTFIL-IO-AREA (61:1)
               MOVE 'CUSTOMER NAME      '  TO OUTFIL-IO-AREA (62:19)
               MOVE ';'                    TO OUTFIL-IO-AREA (81:1)
               MOVE 'ALPHA '               TO OUTFIL-IO-AREA (82:6)
               MOVE ';'                    TO OUTFIL-IO-AREA (88:1)
               MOVE 'PART NUMBER '         TO OUTFIL-IO-AREA (89:12)
               MOVE ';'                    TO OUTFIL-IO-AREA (101:1)
               MOVE 'ITEM DESCRIPTION   '  TO OUTFIL-IO-AREA (102:19)
               MOVE ';'                    TO OUTFIL-IO-AREA (121:1)
               MOVE 'QUANTITY DELIVERED '  TO OUTFIL-IO-AREA (122:19)
               MOVE ';'                    TO OUTFIL-IO-AREA (141:1)
               MOVE 'LIST PRICE '          TO OUTFIL-IO-AREA (142:11)
               MOVE ';'                    TO OUTFIL-IO-AREA (153:1)
               MOVE 'DISCOUNT1 '           TO OUTFIL-IO-AREA (154:10)
               MOVE ';'                    TO OUTFIL-IO-AREA (164:1)
               MOVE 'DISCOUNT2 '           TO OUTFIL-IO-AREA (165:10)
               MOVE ';'                    TO OUTFIL-IO-AREA (175:1)
               MOVE 'DISCOUNT3 '           TO OUTFIL-IO-AREA (176:10)
               MOVE ';'                    TO OUTFIL-IO-AREA (186:1)
               MOVE 'NET PRICE (NOK) '     TO OUTFIL-IO-AREA (187:16)
               MOVE ';'                    TO OUTFIL-IO-AREA (203:1)
               MOVE 'GROUP'                TO OUTFIL-IO-AREA (204:5)
               MOVE ';'                    TO OUTFIL-IO-AREA (209:1)
               MOVE 'INVENT LOC'           TO OUTFIL-IO-AREA (210:10)
               MOVE ';'                    TO OUTFIL-IO-AREA (220:1)
               MOVE 'CR.TYPE'              TO OUTFIL-IO-AREA (221:7)
               MOVE ';'                    TO OUTFIL-IO-AREA (228:1)
               MOVE 'INT.INVOICE'          TO OUTFIL-IO-AREA (229:11)
               MOVE ';CAT;DIST;SALE;'      TO OUTFIL-IO-AREA (240:15)
               MOVE 'SERV;'                TO OUTFIL-IO-AREA (255:5)
               MOVE 'INV.DAT;'             TO OUTFIL-IO-AREA (260:8)
               MOVE 'ACCOUNT;'             TO OUTFIL-IO-AREA (268:8)
               MOVE 'COST PRICE;'          TO OUTFIL-IO-AREA (276:11)
               MOVE 'SUPPLIER;'            TO OUTFIL-IO-AREA (288:9)
               MOVE 'SUPPLYER NAME;'       TO OUTFIL-IO-AREA (297:14)
               MOVE 'SUPPLYER ARTNR;'      TO OUTFIL-IO-AREA (311:15)
               MOVE 'SUPPLYER TERMS;'      TO OUTFIL-IO-AREA (325:15)
               MOVE 'ORG.NUMBER;'          TO OUTFIL-IO-AREA (340:11)
               WRITE OUTFIL-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANT                    TO XO-80YY9
               MOVE XO-80YY9               TO LISTE-IO-AREA (11:10)
               MOVE 'RECORDS LEST.      '  TO LISTE-IO-AREA (24:19)
               MOVE 2                      TO LISTE-BEFORE-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ANTF                   TO XO-80YY9
               MOVE XO-80YY9               TO LISTE-IO-AREA (11:10)
               MOVE 'RECORDS SELEKTERT  '  TO LISTE-IO-AREA (24:19)
               MOVE 2                      TO LISTE-BEFORE-SPACE
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
           INITIALIZE INNFIL-DATA-FIELDS
           SET INNFIL-EOF-OFF              TO TRUE
           SET INNFIL-PROCESS              TO TRUE
           OPEN INPUT INNFIL
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
           OPEN OUTPUT OUTFIL
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INNFIL
           CLOSE KUNDEMA
           CLOSE KUNDEMX
           CLOSE VAREMAS
           CLOSE VARETIL
           CLOSE VAGRMAS
           CLOSE OUTFIL
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
