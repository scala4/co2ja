       IDENTIFICATION DIVISION.
       PROGRAM-ID. RKO185R.
      **********************************************  Z-WIN-RPG2   ****
      * NY VERSJON AV RSK.RSK185                 ***TXT***OK EN
      *                                                                        *
      *  PROGRAM   RKO185  - JCL:RES.XRES31UD                                  *
      *  L E V E R A N D Ø R R E S K O N T R O                                 *
      *  DANNE UTSKRIFTSFILE FOR FORFALLSLISTE.                                *
      *  DANNE FILE FOR REMITTERINGER BANKGIRO                                 *
      *  DANNE FILE FOR UTBETALINGSSPESIFIKASJON                               *
      *  HJELPEFILE FOR REMITTERINGER OG UTBETALINGSSPESFIKASJON               *
      *  30/12-93  E.L.  LAGT INN TEST SLIK ATT POSTER LAGT UT TIL NY          *
      *            UTBET.RUTINE IKKE BLIR LAGT UT.                             *
      *            (FJERNET GENERELL TEST PÅ OM FIRMA BRUKER NY RUTINE         *
      *             DA DETTE MEDFØRER ATT UTENLANDSKE IKKE KOMMER UT.)         *
      *  15.11.94  MERKER FORFALLSFILE MED UTENLANDSKREDITORER (BLE FØR        *
      *            KUN GJORT FOR FIMANR 000.                                   *
      *  24.01.96  HAFNOR SKAL PLUKKE UTLANDSLEVERANDØR VIA HND=0XX.           *
      *  14.03.96  FEILRETTING: ROMNES BLE KUTTET UT AV HAFNOR (N31).          *
      *  19.11.96  JAHRE SKAL PLUKKE UTLANDSLEVERANDØR VIA HND=0XX.            *
      *  30.10.97  LAGT INN KODE FOR IKKE GIRO (UTBRUT=I) FOR Å STOPPE         *
      *            UTSKRIFT AV GIRO FOR HELE FIRMAET.                          *
      *  25.06.98  TAKLER ÅR 2000.                                             *
      *            LEGGER IKKE LENGER UT OPPDATERINGSFILE.                     *
      *  17.08.98  ENDRET TEKST I FEILMELDING.
      *  11.03.03  FJERNET GIROFILER/GIROBEHANDLING.
      *  SEPT. 05  RECORDLENGDE FRA 89 -> 200
      *  10.11.05  SANITAS SKAL PLUKKE UTLANDSLEVERANDØR VIA HND=0XX.          *
      *  29.01.14  LAGT INN S OG B I VALUTABEHANDLING                          *
      **************************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RKO185.rpg
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
           SELECT RESKOMA
               ASSIGN TO RESKOMA
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS INDEXED
               STATUS IS RESKOMA-STATUS
               RECORD KEY IS RESKOMA-KEY1.
           SELECT KUNDEMA
               ASSIGN TO KUNDEMA
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMA-STATUS
               RECORD KEY IS KUNDEMA-KEY1.
           SELECT FFILE
               ASSIGN TO UT-S-FFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD RESKOMA
               RECORD CONTAINS 200.
       01  RESKOMA-IO-AREA.
           05  RESKOMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  RESKOMA-KEY1.
                   15  RESKOMA-KEY1N       PICTURE S9(14).
               10  FILLER                  PICTURE X(185).
       FD KUNDEMA
               RECORD CONTAINS 200.
       01  KUNDEMA-IO-AREA.
           05  KUNDEMA-IO-AREA-X.
               10  FILLER                  PICTURE X(1).
               10  KUNDEMA-KEY1            PICTURE X(9).
               10  FILLER                  PICTURE X(190).
       FD FFILE
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  FFILE-IO-AREA.
           05  FFILE-IO-AREA-X             PICTURE X(200).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  RESKOMA-STATUS              PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  FFILE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  RESKOMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKOMA-EOF-OFF         VALUE '0'.
               88  RESKOMA-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKOMA-READ-OFF        VALUE '0'.
               88  RESKOMA-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  RESKOMA-PROCESS-OFF     VALUE '0'.
               88  RESKOMA-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  RESKOMA-LEVEL-INIT-OFF  VALUE '0'.
               88  RESKOMA-LEVEL-INIT      VALUE '1'.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  RESKOMA-LEVEL-02.
               10  RESKOMA-02-L2.
                   15  RESKOMA-02-L2-RFIRM PICTURE X(3).
               10  RESKOMA-02-L1.
                   15  RESKOMA-02-L1-KNRKEY PICTURE X(9).
                   15  RESKOMA-02-L1-RESKNR PICTURE X(6).
           05  RESKOMA-DATA-FIELDS.
               10  RECORD-X                PICTURE X(200).
               10  KNRKEY                  PICTURE X(9).
               10  RESKNR                  PICTURE X(6).
               10  BMA-ELGTE               PICTURE X(2).
               10  RESKOD                  PICTURE X(1).
               10  RFIRM                   PICTURE X(3).
           05  KUNDEMA-DATA-FIELDS.
               10  KBM                     PICTURE X(2).
               10  HND                     PICTURE X(1).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L2            PICTURE X(3).
               10  THE-PRIOR-L1            PICTURE X(15).
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
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  RESKOMA-PROCESS
               SET RESKOMA-PROCESS-OFF     TO TRUE
               SET RESKOMA-READ            TO TRUE
           END-IF
 
           IF  RESKOMA-READ
           AND RECORD-SELECTED-OFF
               PERFORM RESKOMA-GET
               SET RESKOMA-READ-OFF        TO TRUE
               IF  NOT RESKOMA-EOF
                   SET RESKOMA-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  RESKOMA-PROCESS
               PERFORM RESKOMA-IDSET
           END-IF
 
           IF  RESKOMA-PROCESS
               PERFORM RESKOMA-CHK-LEVEL
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
 
           IF  RESKOMA-PROCESS
               PERFORM RESKOMA-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  RESKOMA-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-20                    TO TRUE
           SET NOT-I-15                    TO TRUE
           SET NOT-I-27                    TO TRUE
           IF  (I-L2)
               SET NOT-I-31                TO TRUE
               IF  RFIRM = '963'
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-31)
               SET NOT-I-31                TO TRUE
               IF  RFIRM = '705'
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-31)
               SET NOT-I-31                TO TRUE
               IF  RFIRM = '855'
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-31)
               SET NOT-I-31                TO TRUE
               IF  RFIRM = '918'
                   SET I-31                TO TRUE
               END-IF
           END-IF
           IF  (I-02)
               SET NOT-I-20                TO TRUE
               IF  RESKOD = '9'
                   SET I-20                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-20)
               SET NOT-I-20                TO TRUE
               IF  RESKOD = '8'
                   SET I-20                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-31 AND NOT-I-20)
               SET NOT-I-20                TO TRUE
               IF  RESKOD = '7'
                   SET I-20                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-20)
               GO TO SLUTT-T
           END-IF
           IF  (I-L1 AND I-31)
               MOVE KNRKEY                 TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-30                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-30            TO TRUE
                   PERFORM KUNDEMA-FLDSET
                   PERFORM KUNDEMA-IDSET
               END-READ
           END-IF
           IF  (I-02)
               SET NOT-I-27                TO TRUE
               IF  RESKOD = '7'
                   SET I-27                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-31 AND NOT-I-30)
               SET NOT-I-15                TO TRUE
               IF  HND = '0'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-31 AND NOT-I-30)
               AND (I-27)
               MOVE KBM                    TO BMA-ELGTE
           END-IF.
 
       SLUTT-T.
      *
           CONTINUE.
 
       RESKOMA-GET SECTION.
       RESKOMA-GET-P.
           IF  RESKOMA-EOF-OFF
               READ RESKOMA
               AT END
                   SET RESKOMA-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       RESKOMA-FLDSET SECTION.
       RESKOMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RESKOMA-IO-AREA (1:200) TO RECORD-X (1:200)
               MOVE RESKOMA-IO-AREA (3:9)  TO KNRKEY (1:9)
               MOVE RESKOMA-IO-AREA (6:6)  TO RESKNR (1:6)
               MOVE RESKOMA-IO-AREA (61:2) TO BMA-ELGTE (1:2)
               MOVE RESKOMA-IO-AREA (6:1)  TO RESKOD (1:1)
               MOVE RESKOMA-IO-AREA (3:3)  TO RFIRM (1:3)
           END-EVALUATE.
 
       RESKOMA-IDSET SECTION.
       RESKOMA-IDSET-P.
           SET I-02                        TO TRUE.
 
       RESKOMA-CHK-LEVEL SECTION.
       RESKOMA-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO RESKOMA-LEVEL-02
               MOVE RESKOMA-IO-AREA (3:3)  TO RESKOMA-02-L2-RFIRM
               MOVE RESKOMA-IO-AREA (3:9)  TO RESKOMA-02-L1-KNRKEY
               MOVE RESKOMA-IO-AREA (6:6)  TO RESKOMA-02-L1-RESKNR
               IF  RESKOMA-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  RESKOMA-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  RESKOMA-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  RESKOMA-02-L2         TO THE-PRIOR-L2
               MOVE  RESKOMA-02-L1         TO THE-PRIOR-L1
               SET RESKOMA-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (127:2) TO KBM (1:2)
               MOVE KUNDEMA-IO-AREA (185:1) TO HND (1:1)
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-03                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02 AND I-20)
               MOVE SPACES TO FFILE-IO-AREA
               INITIALIZE FFILE-IO-AREA
               MOVE RECORD-X               TO FFILE-IO-AREA (1:200)
               MOVE BMA-ELGTE              TO FFILE-IO-AREA (61:2)
               IF  (I-15)
                   MOVE '1'                TO FFILE-IO-AREA (89:1)
               END-IF
               IF  (NOT-I-15)
                   MOVE '2'                TO FFILE-IO-AREA (89:1)
               END-IF
               WRITE FFILE-IO-AREA
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
           SET RESKOMA-LEVEL-INIT          TO TRUE
           INITIALIZE RESKOMA-DATA-FIELDS
           SET RESKOMA-EOF-OFF             TO TRUE
           SET RESKOMA-PROCESS             TO TRUE
           OPEN INPUT RESKOMA
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           OPEN OUTPUT FFILE.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE RESKOMA
           CLOSE KUNDEMA
           CLOSE FFILE.
 
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
