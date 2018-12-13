       IDENTIFICATION DIVISION.
       PROGRAM-ID. STA187R.
      **********************************************  Z-WIN-RPG2   ****
      *          AVDELING (1. SIFFER I VAREGRUPPE) LEGGES  UT I       *
      *          STEDEN FOR KUNDEKATEGORI PÅ ENKELTE FIRMA.           *
      *          UPSI 1 NÅR SELGERSTAT (STA33)                        *
      * 02.02.01 LAGT INN KONSERN-TEST FOR HENTING AV KUNDEDATA.      *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: STA187.rpg
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
           SELECT PARFILE
               ASSIGN TO UT-S-PARFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PARFILE-STATUS.
           SELECT MASTER
               ASSIGN TO UT-S-MASTER
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS MASTER-STATUS.
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
           SELECT PRTFILE
               ASSIGN TO UT-S-PRTFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PRTFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD PARFILE
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  PARFILE-IO-AREA.
           05  PARFILE-IO-AREA-X           PICTURE X(80).
       FD MASTER
               BLOCK CONTAINS 4350
               RECORD CONTAINS 145.
       01  MASTER-IO-AREA.
           05  MASTER-IO-AREA-X            PICTURE X(145).
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
      *BUGFILO O   F  80  80            PRINTERSYSLST
       FD PRTFILE
               BLOCK CONTAINS 4200
               RECORD CONTAINS 210.
       01  PRTFILE-IO-AREA.
           05  PRTFILE-IO-AREA-X           PICTURE X(210).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  PARFILE-STATUS              PICTURE 99 VALUE 0.
           10  MASTER-STATUS               PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  PRTFILE-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  PARFILE-EOF-OFF         VALUE '0'.
               88  PARFILE-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARFILE-READ-OFF        VALUE '0'.
               88  PARFILE-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PARFILE-PROCESS-OFF     VALUE '0'.
               88  PARFILE-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  MASTER-EOF-OFF          VALUE '0'.
               88  MASTER-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  MASTER-READ-OFF         VALUE '0'.
               88  MASTER-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  MASTER-PROCESS-OFF      VALUE '0'.
               88  MASTER-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  MASTER-LEVEL-INIT-OFF   VALUE '0'.
               88  MASTER-LEVEL-INIT       VALUE '1'.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  PARFILE-DATA-FIELDS.
               10  MND                     PICTURE X(2).
               10  SPES                    PICTURE X(1).
           05  MASTER-LEVEL-02.
               10  MASTER-02-L3.
                   15  MASTER-02-L3-FIRMA  PICTURE X(3).
               10  MASTER-02-L2.
                   15  MASTER-02-L2-KUNDE  PICTURE X(6).
               10  MASTER-02-L1.
                   15  MASTER-02-L1-VGR    PICTURE X(5).
           05  MASTER-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  KUNDE                   PICTURE X(6).
               10  VGR                     PICTURE X(5).
               10  AVDEL                   PICTURE X(1).
               10  AFJOR-IO.
                   15  AFJOR               PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  A1-IO.
                   15  A1                  PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  A2-IO.
                   15  A2                  PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  A3-IO.
                   15  A3                  PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  A4-IO.
                   15  A4                  PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  A5-IO.
                   15  A5                  PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  A6-IO.
                   15  A6                  PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  A7-IO.
                   15  A7                  PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  A8-IO.
                   15  A8                  PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  A9-IO.
                   15  A9                  PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  A10-IO.
                   15  A10                 PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  A11-IO.
                   15  A11                 PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  A12-IO.
                   15  A12                 PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  BFJOR-IO.
                   15  BFJOR               PICTURE S9(9)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  B1-IO.
                   15  B1                  PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  B2-IO.
                   15  B2                  PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  B3-IO.
                   15  B3                  PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  B4-IO.
                   15  B4                  PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  B5-IO.
                   15  B5                  PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  B6-IO.
                   15  B6                  PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  B7-IO.
                   15  B7                  PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  B8-IO.
                   15  B8                  PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  B9-IO.
                   15  B9                  PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  B10-IO.
                   15  B10                 PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  B11-IO.
                   15  B11                 PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  B12-IO.
                   15  B12                 PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  ANTDMF-IO.
                   15  ANTDMF              PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  BELDMF-IO.
                   15  BELDMF              PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
           05  FIRMAF-DATA-FIELDS.
               10  KONFNR                  PICTURE X(3).
      *  01                MOVE "MND     "BUGFL2  8        LEDETXT DEBUG
      *  01      BUGFL2    DEBUGBUGFILO   MND              VIS FELT/IND
      *****************************************************
      *  RUTINE FOR HAND.DIST UNDERTRYKKING.              *
      *         INDKATOR 08 = ØNSKER IKKE HND.DIST.       *
      *****************************************************
           05  KUNDEMA-DATA-FIELDS.
               10  ALFA                    PICTURE X(4).
               10  NAVN1                   PICTURE X(30).
               10  ADR                     PICTURE X(30).
               10  POSTST                  PICTURE X(15).
               10  POSTNR                  PICTURE X(4).
               10  KAT-IO.
                   15  KAT                 PICTURE S9(3) USAGE
                                                       PACKED-DECIMAL.
               10  HDIST                   PICTURE X(3).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(6).
               10  THE-PRIOR-L1            PICTURE X(5).
           05  TEMPORARY-FIELDS.
               10  REGFNR                  PICTURE X(3).
               10  KUNKEY                  PICTURE X(9).
               10  ANTAKK-IO.
                   15  ANTAKK              PICTURE S9(7)V9(2).
               10  BELAKK-IO.
                   15  BELAKK              PICTURE S9(8)V9(2).
               10  ADPPR-IO.
                   15  ADPPR               PICTURE S9(7).
               10  ADMFPR-IO.
                   15  ADMFPR              PICTURE S9(7).
               10  AAKPR-IO.
                   15  AAKPR               PICTURE S9(7).
               10  AFJPR-IO.
                   15  AFJPR               PICTURE S9(7).
               10  BDPPR-IO.
                   15  BDPPR               PICTURE S9(8).
               10  BDMFPR-IO.
                   15  BDMFPR              PICTURE S9(8).
               10  BAKPR-IO.
                   15  BAKPR               PICTURE S9(8).
               10  BFJPR-IO.
                   15  BFJPR               PICTURE S9(8).
               10  AVVPR-IO.
                   15  AVVPR               PICTURE S9(8).
           05  EDITTING-FIELDS.
               10  XO-30D                  PICTURE S9(3).
               10  XO-30U                  PICTURE 9(3).
               10  XO-30P-EF.
                 15  XO-30P                PICTURE S9(3) USAGE
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
           SET NOT-I-05                    TO TRUE
           SET NOT-I-04                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  PARFILE-PROCESS
               SET PARFILE-PROCESS-OFF     TO TRUE
               SET PARFILE-READ            TO TRUE
           END-IF
 
           IF  PARFILE-READ
           AND RECORD-SELECTED-OFF
               PERFORM PARFILE-GET
               SET PARFILE-READ-OFF        TO TRUE
               IF  NOT PARFILE-EOF
                   PERFORM PARFILE-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET PARFILE-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  MASTER-PROCESS
               SET MASTER-PROCESS-OFF      TO TRUE
               SET MASTER-READ             TO TRUE
           END-IF
 
           IF  MASTER-READ
           AND RECORD-SELECTED-OFF
               PERFORM MASTER-GET
               SET MASTER-READ-OFF         TO TRUE
               IF  NOT MASTER-EOF
                   PERFORM MASTER-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET MASTER-PROCESS      TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  PARFILE-PROCESS
               PERFORM PARFILE-IDSET
           END-IF
 
           IF  MASTER-PROCESS
               PERFORM MASTER-IDSET
           END-IF
 
           IF  MASTER-PROCESS
               PERFORM MASTER-CHK-LEVEL
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
 
           IF  PARFILE-PROCESS
               PERFORM PARFILE-FLDOFF
               PERFORM PARFILE-FLDSET
           END-IF
 
           IF  MASTER-PROCESS
               PERFORM MASTER-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  MASTER-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-L3)
               SET NOT-I-08                TO TRUE
               IF  FIRMA = '932'
                   SET I-08                TO TRUE
               END-IF
      *****************************************************
      *  RUTINE FOR KATEGORI ELLER AVDELING ELLER 000     *
      *         INDKATOR 10 = AVDELING.                   *
      *         INDKATOR 09 = ØNSKER IKKE KATEGORI.       *
      *     FJERNET IFLG. STEIN 13.10.00-INDIKATOR 09    **
      *  L3      FIRMA     COMP "764"                    09 = BEMA
           END-IF
           IF  (I-L3)
               SET NOT-I-10                TO TRUE
               IF  FIRMA = '918'
                   SET I-10                TO TRUE
               END-IF
      *****************************************************
      *  RUTINE FOR ATT KUNDENR IKKE BLIR SATT TIL 000000.*
      *  DETTE SKAL KUN BENYTTES PÅ SELGERSTAT (STA33)    *
      *  STA33 SKAL DA HA UPSI 1 PÅ.                      *
      *         INDKATOR 07 = ALLE KUNDER                 *
      *****************************************************
           END-IF
           IF  (I-L3 AND I-U1)
               SET NOT-I-07                TO TRUE
               IF  FIRMA = '764'
                   SET I-07                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND I-U1 AND NOT-I-07)
               SET NOT-I-07                TO TRUE
               IF  FIRMA = '608'
                   SET I-07                TO TRUE
               END-IF
      *****************************************************************
      * RUTINE FOR Å HENTE KONSERNFIRMANR. VED OPPSLAG I KUNDE.MASTER *
      *****************************************************************
           END-IF
           IF  (I-L3)
               MOVE FIRMA                  TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-55                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-55            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
               SET NOT-I-90                TO TRUE
               IF  KONFNR > '000'
                   SET I-90                TO TRUE
               END-IF
           END-IF
           MOVE FIRMA                      TO REGFNR
           IF  (I-90)
               MOVE KONFNR                 TO REGFNR
      *****************************************************
      *  RUTINE FOR OPPSLAG PÅ KUNDEMASTER.               *
      *****************************************************
           END-IF
           IF  (I-L2)
               SET NOT-I-52                TO TRUE
           END-IF
           IF  (I-02 AND NOT-I-52)
               MOVE REGFNR                 TO KUNKEY (1:3)
               MOVE KUNDE                  TO KUNKEY (4:6)
               MOVE KUNKEY                 TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-85                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-85            TO TRUE
                   PERFORM KUNDEMA-IDCHK
                   PERFORM KUNDEMA-FLDOFF
                   PERFORM KUNDEMA-FLDSET
                   PERFORM KUNDEMA-IDSET
               END-READ
           END-IF
           IF  (I-02)
               SET I-52                    TO TRUE
           END-IF
           IF  (I-02 AND I-10)
               SET NOT-I-22                TO TRUE
      *****************************************************************
           END-IF
           IF  (I-02)
               ADD A12                     TO ANTAKK
               ADD B12                     TO BELAKK
               SET NOT-I-11                TO TRUE
               IF  MND = '01'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-11)
               GO TO SLUTT1-T
           END-IF
           IF  (I-02)
               ADD A11                     TO ANTAKK
               ADD B11                     TO BELAKK
               SET NOT-I-11                TO TRUE
               IF  MND = '02'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-11)
               GO TO SLUTT1-T
           END-IF
           IF  (I-02)
               ADD A10                     TO ANTAKK
               ADD B10                     TO BELAKK
               SET NOT-I-11                TO TRUE
               IF  MND = '03'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-11)
               GO TO SLUTT1-T
           END-IF
           IF  (I-02)
               ADD A9                      TO ANTAKK
               ADD B9                      TO BELAKK
               SET NOT-I-11                TO TRUE
               IF  MND = '04'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-11)
               GO TO SLUTT1-T
           END-IF
           IF  (I-02)
               ADD A8                      TO ANTAKK
               ADD B8                      TO BELAKK
               SET NOT-I-11                TO TRUE
               IF  MND = '05'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-11)
               GO TO SLUTT1-T
           END-IF
           IF  (I-02)
               ADD A7                      TO ANTAKK
               ADD B7                      TO BELAKK
               SET NOT-I-11                TO TRUE
               IF  MND = '06'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-11)
               GO TO SLUTT1-T
           END-IF
           IF  (I-02)
               ADD A6                      TO ANTAKK
               ADD B6                      TO BELAKK
               SET NOT-I-11                TO TRUE
               IF  MND = '07'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-11)
               GO TO SLUTT1-T
           END-IF
           IF  (I-02)
               ADD A5                      TO ANTAKK
               ADD B5                      TO BELAKK
               SET NOT-I-11                TO TRUE
               IF  MND = '08'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-11)
               GO TO SLUTT1-T
           END-IF
           IF  (I-02)
               ADD A4                      TO ANTAKK
               ADD B4                      TO BELAKK
               SET NOT-I-11                TO TRUE
               IF  MND = '09'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-11)
               GO TO SLUTT1-T
           END-IF
           IF  (I-02)
               ADD A3                      TO ANTAKK
               ADD B3                      TO BELAKK
               SET NOT-I-11                TO TRUE
               IF  MND = '10'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-11)
               GO TO SLUTT1-T
           END-IF
           IF  (I-02)
               ADD A2                      TO ANTAKK
               ADD B2                      TO BELAKK
               SET NOT-I-11                TO TRUE
               IF  MND = '11'
                   SET I-11                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-11)
               GO TO SLUTT1-T
           END-IF
           IF  (I-02)
               ADD A1                      TO ANTAKK
               ADD B1                      TO BELAKK
           END-IF.
 
       SLUTT1-T.
           IF  (I-02)
               ADD A12 TO ZERO         GIVING ADPPR ROUNDED
               ADD ANTDMF TO ZERO      GIVING ADMFPR ROUNDED
               ADD ANTAKK TO ZERO      GIVING AAKPR ROUNDED
               ADD AFJOR TO ZERO       GIVING AFJPR ROUNDED
               ADD B12 TO ZERO         GIVING BDPPR ROUNDED
               ADD BELDMF TO ZERO      GIVING BDMFPR ROUNDED
               ADD BELAKK TO ZERO      GIVING BAKPR ROUNDED
               ADD BFJOR TO ZERO       GIVING BFJPR ROUNDED
               SUBTRACT BFJPR FROM BAKPR GIVING AVVPR ROUNDED
               MOVE 0,00                   TO ANTAKK
               MOVE 0,00                   TO BELAKK
           END-IF.
 
       PARFILE-GET SECTION.
       PARFILE-GET-P.
           IF  PARFILE-EOF-OFF
               READ PARFILE
               AT END
                   SET PARFILE-EOF         TO TRUE
               END-READ
           END-IF.
 
       PARFILE-FLDOFF SECTION.
       PARFILE-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( PARFILE-IO-AREA (1:1) = 'P' )
               SET NOT-I-21                TO TRUE
           END-EVALUATE.
 
       PARFILE-FLDSET SECTION.
       PARFILE-FLDSET-P.
           EVALUATE TRUE
           WHEN ( PARFILE-IO-AREA (1:1) = 'P' )
               MOVE PARFILE-IO-AREA (2:2)  TO MND (1:2)
               MOVE PARFILE-IO-AREA (10:1) TO SPES (1:1)
               IF  SPES = SPACES
                   SET I-21                TO TRUE
               END-IF
           END-EVALUATE.
 
       PARFILE-IDCHK SECTION.
       PARFILE-IDCHK-P.
           EVALUATE TRUE
           WHEN ( PARFILE-IO-AREA (1:1) = 'P' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       PARFILE-IDSET SECTION.
       PARFILE-IDSET-P.
           EVALUATE TRUE
           WHEN ( PARFILE-IO-AREA (1:1) = 'P' )
               SET I-01                    TO TRUE
           END-EVALUATE.
 
       MASTER-GET SECTION.
       MASTER-GET-P.
           IF  MASTER-EOF-OFF
               READ MASTER
               AT END
                   SET MASTER-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       MASTER-FLDSET SECTION.
       MASTER-FLDSET-P.
           EVALUATE TRUE
           WHEN ( MASTER-IO-AREA (1:1) = '8'
            AND   MASTER-IO-AREA (2:1) = '2' )
               MOVE MASTER-IO-AREA (3:3)   TO FIRMA (1:3)
               MOVE MASTER-IO-AREA (6:6)   TO KUNDE (1:6)
               MOVE MASTER-IO-AREA (12:5)  TO VGR (1:5)
               MOVE MASTER-IO-AREA (12:1)  TO AVDEL (1:1)
               MOVE MASTER-IO-AREA (17:5)  TO AFJOR-IO
               MOVE MASTER-IO-AREA (22:4)  TO A1-IO
               MOVE MASTER-IO-AREA (26:4)  TO A2-IO
               MOVE MASTER-IO-AREA (30:4)  TO A3-IO
               MOVE MASTER-IO-AREA (34:4)  TO A4-IO
               MOVE MASTER-IO-AREA (38:4)  TO A5-IO
               MOVE MASTER-IO-AREA (42:4)  TO A6-IO
               MOVE MASTER-IO-AREA (46:4)  TO A7-IO
               MOVE MASTER-IO-AREA (50:4)  TO A8-IO
               MOVE MASTER-IO-AREA (54:4)  TO A9-IO
               MOVE MASTER-IO-AREA (58:4)  TO A10-IO
               MOVE MASTER-IO-AREA (62:4)  TO A11-IO
               MOVE MASTER-IO-AREA (66:4)  TO A12-IO
               MOVE MASTER-IO-AREA (70:6)  TO BFJOR-IO
               MOVE MASTER-IO-AREA (76:5)  TO B1-IO
               MOVE MASTER-IO-AREA (81:5)  TO B2-IO
               MOVE MASTER-IO-AREA (86:5)  TO B3-IO
               MOVE MASTER-IO-AREA (91:5)  TO B4-IO
               MOVE MASTER-IO-AREA (96:5)  TO B5-IO
               MOVE MASTER-IO-AREA (101:5) TO B6-IO
               MOVE MASTER-IO-AREA (106:5) TO B7-IO
               MOVE MASTER-IO-AREA (111:5) TO B8-IO
               MOVE MASTER-IO-AREA (116:5) TO B9-IO
               MOVE MASTER-IO-AREA (121:5) TO B10-IO
               MOVE MASTER-IO-AREA (126:5) TO B11-IO
               MOVE MASTER-IO-AREA (131:5) TO B12-IO
               MOVE MASTER-IO-AREA (137:4) TO ANTDMF-IO
               MOVE MASTER-IO-AREA (141:5) TO BELDMF-IO
           END-EVALUATE.
 
       MASTER-IDCHK SECTION.
       MASTER-IDCHK-P.
           EVALUATE TRUE
           WHEN ( MASTER-IO-AREA (1:1) = '8'
            AND   MASTER-IO-AREA (2:1) = '2' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       MASTER-IDSET SECTION.
       MASTER-IDSET-P.
           EVALUATE TRUE
           WHEN ( MASTER-IO-AREA (1:1) = '8'
            AND   MASTER-IO-AREA (2:1) = '2' )
               SET I-02                    TO TRUE
           END-EVALUATE.
 
       MASTER-CHK-LEVEL SECTION.
       MASTER-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( MASTER-IO-AREA (1:1) = '8'
            AND   MASTER-IO-AREA (2:1) = '2' )
               MOVE LOW-VALUES             TO MASTER-LEVEL-02
               MOVE MASTER-IO-AREA (3:3)   TO MASTER-02-L3-FIRMA
               MOVE MASTER-IO-AREA (6:6)   TO MASTER-02-L2-KUNDE
               MOVE MASTER-IO-AREA (12:5)  TO MASTER-02-L1-VGR
               IF  MASTER-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  MASTER-02-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  MASTER-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  MASTER-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  MASTER-02-L3          TO THE-PRIOR-L3
               MOVE  MASTER-02-L2          TO THE-PRIOR-L2
               MOVE  MASTER-02-L1          TO THE-PRIOR-L1
               SET MASTER-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (1:3)   TO KONFNR (1:3)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-05                        TO TRUE.
 
       KUNDEMA-FLDOFF SECTION.
       KUNDEMA-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( KUNDEMA-IO-AREA (1:1) = '8'
            AND   KUNDEMA-IO-AREA (2:1) = '0' )
               SET NOT-I-22                TO TRUE
           END-EVALUATE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ( KUNDEMA-IO-AREA (1:1) = '8'
            AND   KUNDEMA-IO-AREA (2:1) = '0' )
               MOVE KUNDEMA-IO-AREA (12:4) TO ALFA (1:4)
               MOVE KUNDEMA-IO-AREA (16:30) TO NAVN1 (1:30)
               MOVE KUNDEMA-IO-AREA (76:30) TO ADR (1:30)
               MOVE KUNDEMA-IO-AREA (106:15) TO POSTST (1:15)
               MOVE KUNDEMA-IO-AREA (121:4) TO POSTNR (1:4)
               MOVE KUNDEMA-IO-AREA (162:2) TO KAT-IO
               IF  KAT = ZERO
                   SET I-22                TO TRUE
               END-IF
               MOVE KUNDEMA-IO-AREA (185:3) TO HDIST (1:3)
           END-EVALUATE.
 
       KUNDEMA-IDCHK SECTION.
       KUNDEMA-IDCHK-P.
           EVALUATE TRUE
           WHEN ( KUNDEMA-IO-AREA (1:1) = '8'
            AND   KUNDEMA-IO-AREA (2:1) = '0' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           EVALUATE TRUE
           WHEN ( KUNDEMA-IO-AREA (1:1) = '8'
            AND   KUNDEMA-IO-AREA (2:1) = '0' )
               SET I-04                    TO TRUE
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02)
               MOVE SPACES TO PRTFILE-IO-AREA
               INITIALIZE PRTFILE-IO-AREA
               MOVE '83'                   TO PRTFILE-IO-AREA (1:2)
               MOVE FIRMA                  TO PRTFILE-IO-AREA (3:3)
               IF  (NOT-I-10 AND NOT-I-85)
                   MOVE KAT                TO XO-30U
                   MOVE XO-30U (1:3)       TO PRTFILE-IO-AREA (6:3)
               END-IF
               IF  (NOT-I-10 AND I-85)
                   MOVE '000'              TO PRTFILE-IO-AREA (6:3)
      *                                   8 "000"
      *                      09           8 "000"
               END-IF
               IF  (I-10)
                   MOVE AVDEL              TO PRTFILE-IO-AREA (7:1)
               END-IF
               IF  (NOT-I-85)
                   MOVE HDIST              TO PRTFILE-IO-AREA (9:3)
               END-IF
               IF  (I-85)
                   MOVE '000'              TO PRTFILE-IO-AREA (9:3)
               END-IF
               IF  (I-08)
                   MOVE '000'              TO PRTFILE-IO-AREA (9:3)
               END-IF
               IF  (NOT-I-85)
                   MOVE ALFA               TO PRTFILE-IO-AREA (12:4)
               END-IF
               IF  (I-85)
                   MOVE 'AAAA'             TO PRTFILE-IO-AREA (12:4)
               END-IF
               MOVE KUNDE                  TO PRTFILE-IO-AREA (16:6)
               IF  (I-21 AND I-22 AND NOT-I-07)
                   MOVE '000000'           TO PRTFILE-IO-AREA (16:6)
               END-IF
               MOVE VGR                    TO PRTFILE-IO-AREA (22:5)
               IF  (NOT-I-85)
                   MOVE NAVN1              TO PRTFILE-IO-AREA (27:30)
               END-IF
               IF  (I-85)
                   MOVE 'KUNDENAVN UKJENT' TO PRTFILE-IO-AREA (27:16)
               END-IF
               IF  (I-85)
                   MOVE '              '   TO PRTFILE-IO-AREA (43:14)
               END-IF
               IF  (I-21 AND I-22 AND NOT-I-07)
                   MOVE 'KUNDER UTEN KAT'  TO PRTFILE-IO-AREA (27:15)
               END-IF
               IF  (I-21 AND I-22 AND NOT-I-07)
                   MOVE '               '  TO PRTFILE-IO-AREA (42:15)
               END-IF
               IF  (NOT-I-85)
                   MOVE ADR                TO PRTFILE-IO-AREA (57:30)
               END-IF
               IF  (I-85)
                   MOVE 'ADRESSE UKJENT'   TO PRTFILE-IO-AREA (57:14)
               END-IF
               IF  (I-85)
                   MOVE '                ' TO PRTFILE-IO-AREA (71:16)
               END-IF
               IF  (I-21 AND I-22 AND NOT-I-07)
                   MOVE 'ADRESSE UKJENT'   TO PRTFILE-IO-AREA (57:14)
               END-IF
               IF  (I-21 AND I-22 AND NOT-I-07)
                   MOVE '                ' TO PRTFILE-IO-AREA (71:16)
               END-IF
               MOVE '                 '    TO PRTFILE-IO-AREA (87:17)
               MOVE '                 '    TO PRTFILE-IO-AREA (104:17)
               MOVE ADMFPR-IO              TO PRTFILE-IO-AREA (121:7)
               INITIALIZE ADMFPR-IO
               MOVE ADPPR-IO               TO PRTFILE-IO-AREA (128:7)
               INITIALIZE ADPPR-IO
               MOVE AAKPR-IO               TO PRTFILE-IO-AREA (135:7)
               INITIALIZE AAKPR-IO
               MOVE AFJPR-IO               TO PRTFILE-IO-AREA (142:7)
               INITIALIZE AFJPR-IO
               MOVE BDPPR-IO               TO PRTFILE-IO-AREA (149:8)
               INITIALIZE BDPPR-IO
               MOVE BAKPR-IO               TO PRTFILE-IO-AREA (157:8)
               INITIALIZE BAKPR-IO
               MOVE BFJPR-IO               TO PRTFILE-IO-AREA (165:8)
               INITIALIZE BFJPR-IO
               MOVE AVVPR-IO               TO PRTFILE-IO-AREA (173:8)
               INITIALIZE AVVPR-IO
               IF  (NOT-I-85)
                   MOVE POSTNR             TO PRTFILE-IO-AREA (181:4)
               END-IF
               IF  (I-85)
                   MOVE '    '             TO PRTFILE-IO-AREA (181:4)
               END-IF
               IF  (I-21 AND I-22 AND NOT-I-07)
                   MOVE '    '             TO PRTFILE-IO-AREA (181:4)
               END-IF
               IF  (NOT-I-85)
                   MOVE POSTST             TO PRTFILE-IO-AREA (185:15)
               END-IF
               IF  (I-85)
                   MOVE '               '  TO PRTFILE-IO-AREA (185:15)
               END-IF
               IF  (I-21 AND I-22 AND NOT-I-07)
                   MOVE '               '  TO PRTFILE-IO-AREA (185:15)
               END-IF
               MOVE BDMFPR-IO              TO PRTFILE-IO-AREA (201:8)
               INITIALIZE BDMFPR-IO
               IF  (NOT-I-10 AND NOT-I-85)
                   MOVE KAT                TO XO-30P
                   MOVE XO-30P-EF          TO PRTFILE-IO-AREA (209:2)
               END-IF
               WRITE PRTFILE-IO-AREA
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
           INITIALIZE PARFILE-DATA-FIELDS
           SET PARFILE-EOF-OFF             TO TRUE
           SET PARFILE-PROCESS             TO TRUE
           OPEN INPUT PARFILE
           SET MASTER-LEVEL-INIT           TO TRUE
           INITIALIZE MASTER-DATA-FIELDS
           SET MASTER-EOF-OFF              TO TRUE
           SET MASTER-PROCESS              TO TRUE
           OPEN INPUT MASTER
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           OPEN OUTPUT PRTFILE.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PARFILE
           CLOSE MASTER
           CLOSE FIRMAF
           CLOSE KUNDEMA
           CLOSE PRTFILE.
 
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
