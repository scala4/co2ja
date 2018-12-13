       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORD973R.
      **********************************************  Z-WIN-RPG2   ****
      *       O R D R E R U T I N E P R O G R A M   O R D 9 7 3       *
      *       -------------------------------------------------       *
      *  1. DANNER RECORDTYPE A(KUNDEKAT.TOT.RECORD) OG RECORDTYPE B  *
      *            OG RECORDTYPE B(KUNDEKAT/VGR TOT.RECORD.           *
      *                                                               *
      * 08/5-2003 AV ESPEN LARSEN.                                    *
      * 15/5-2003 RUTINE FOR KORR. AV KUNDEKAT.                       *
      * 28/5-2003 HENTER KUNDEKAT FRA KUNDEMX (FIRMA STYRT).          *
      *  5/9-2004 HAFNOR VIL SETTE INN 0 I FØRSTE SIFFER PÅ KUNDEKAT  *
      *16/11-2007 LAGT INN FLERE KUNDEKAT PÅ HAFNOR.                  *
      *27/05-2008 LAGT INN K-KAT  50 OG 61 FOR HAFNOR                 *
      *05/06-2008 LAGT INN FIRMA 717                                  *
      *19/08-2008 LAGT INN SELVKOST                                   *
      *29/04-2010 LAGT INN 199                                        *
      *02/09-2010 LAGT INN 288                                        *
      *23/08-2011 fjernet  288,344,216                                *
      *20/10-2011 fjernet  872                                        *
      *30/12-2011 LAGT INN 199                                        *
      *06/01-2012 LAGT INN 900                                        *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: ORD973.rpg
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
           SELECT INNF
               ASSIGN TO UT-S-INNF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INNF-STATUS.
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
           SELECT UTF
               ASSIGN TO UT-S-UTF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS UTF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD INNF
               BLOCK CONTAINS 88
               RECORD CONTAINS 44.
       01  INNF-IO-AREA.
           05  INNF-IO-AREA-X              PICTURE X(44).
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
       FD UTF
               BLOCK CONTAINS 88
               RECORD CONTAINS 44.
       01  UTF-IO-AREA.
           05  UTF-IO-AREA-X               PICTURE X(44).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  INNF-STATUS                 PICTURE 99 VALUE 0.
           10  KUNDEMA-STATUS              PICTURE 99 VALUE 0.
           10  KUNDEMX-STATUS              PICTURE 99 VALUE 0.
           10  UTF-STATUS                  PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  INNF-EOF-OFF            VALUE '0'.
               88  INNF-EOF                VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNF-READ-OFF           VALUE '0'.
               88  INNF-READ               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNF-PROCESS-OFF        VALUE '0'.
               88  INNF-PROCESS            VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  INNF-LEVEL-INIT-OFF     VALUE '0'.
               88  INNF-LEVEL-INIT         VALUE '1'.
           05  KUNDEMA-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  KUNDEMX-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  INNF-LEVEL-01.
               10  INNF-01-L3.
                   15  INNF-01-L3-FIRMA    PICTURE X(3).
               10  INNF-01-L2.
                   15  INNF-01-L2-TOTTYP   PICTURE X(1).
               10  INNF-01-L1.
                   15  INNF-01-L1-KNR      PICTURE X(6).
           05  INNF-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  TOTTYP                  PICTURE X(1).
               10  KNR                     PICTURE X(6).
               10  INNREC                  PICTURE X(41).
               10  KFIRMA                  PICTURE X(3).
           05  KUNDEMA-DATA-FIELDS.
               10  KKAT-IO.
                   15  KKAT                PICTURE S9(3) USAGE
                                                       PACKED-DECIMAL.
      *                                     187 187 KHND
               10  KHND                    PICTURE X(3).
           05  KUNDEMX-DATA-FIELDS.
               10  KKAT1                   PICTURE X(3).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(1).
               10  THE-PRIOR-L1            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  FNRKNR                  PICTURE X(9).
               10  KATNUM-IO.
                   15  KATNUM              PICTURE S9(3).
               10  KKAT-N-IO.
                   15  KKAT-N              PICTURE S9(3).
               10  KATN2-IO.
                   15  KATN2               PICTURE S9(2).
               10  KUNKAT                  PICTURE X(3).
               10  FNRKN1                  PICTURE X(10).
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
           SET NOT-I-03                    TO TRUE
           SET NOT-I-04                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  INNF-PROCESS
               SET INNF-PROCESS-OFF        TO TRUE
               SET INNF-READ               TO TRUE
           END-IF
 
           IF  INNF-READ
           AND RECORD-SELECTED-OFF
               PERFORM INNF-GET
               SET INNF-READ-OFF           TO TRUE
               IF  NOT INNF-EOF
                   SET INNF-PROCESS        TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  INNF-PROCESS
               PERFORM INNF-IDSET
           END-IF
 
           IF  INNF-PROCESS
               PERFORM INNF-CHK-LEVEL
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
 
           IF  INNF-PROCESS
               PERFORM INNF-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  INNF-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-L1)
               SET NOT-I-51                TO TRUE
      *****************************************************************
      * TEST PÅ FIRMA SOM SKAL BRUKE KUNDEKAT. I KUNDE.MASTER. (KAJO) *
      *****************************************************************
           END-IF
           IF  (I-L3)
               SET NOT-I-10                TO TRUE
               IF  FIRMA = '399'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  FIRMA = '199'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  FIRMA = '900'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  FIRMA = '208'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  FIRMA = '992'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  FIRMA = '395'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  FIRMA = '424'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  FIRMA = '474'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  FIRMA = '514'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  FIRMA = '658'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  FIRMA = '638'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  FIRMA = '652'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  FIRMA = '659'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  FIRMA = '673'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  FIRMA = '674'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  FIRMA = '717'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  FIRMA = '764'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  FIRMA = '787'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  FIRMA = '828'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  FIRMA = '910'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  FIRMA = '912'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  FIRMA = '913'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  FIRMA = '922'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  FIRMA = '956'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  FIRMA = '900'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  FIRMA = '529'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  FIRMA = '963'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  FIRMA = '970'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  FIRMA = '977'
                   SET I-10                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-10)
               SET NOT-I-10                TO TRUE
               IF  FIRMA = '978'
                   SET I-10                TO TRUE
               END-IF
      *****************************************************************
      * TEST PÅ FIRMA SOM SKAL BRUKE KUNDEKAT. I KUNDE.XTRA. (KAJ2)   *
      *****************************************************************
           END-IF
           IF  (I-L3)
               SET NOT-I-20                TO TRUE
               IF  FIRMA = '915'
                   SET I-20                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-20)
               SET NOT-I-20                TO TRUE
               IF  FIRMA = '916'
                   SET I-20                TO TRUE
               END-IF
      *****************************************************************
           END-IF
           IF  (NOT-I-10 AND NOT-I-20)
               GO TO SLUTT-T
      *****************************************************************
      * TEST PÅ RECORDART SOM SKAL BENYTTES.                          *
      *****************************************************************
           END-IF
           IF  (I-L2)
               SET NOT-I-11                TO TRUE
               IF  TOTTYP = 'K'
                   SET I-11                TO TRUE
               END-IF
               SET NOT-I-12                TO TRUE
               IF  TOTTYP = 'X'
                   SET I-12                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-11 AND NOT-I-12)
               GO TO SLUTT-T
           END-IF
           IF  (I-10)
               GO TO KAJO-T
           END-IF
           IF  (I-20)
               GO TO KAJ2-T
      *****************************************************************
      * OPPSLAG MOT KUNDE.MASTER FOR Å HENTE KUNDEKAT.                *
      *****************************************************************
           END-IF
           .
 
       KAJO-T.
           IF  (I-L1)
               MOVE KFIRMA                 TO FNRKNR (1:3)
               MOVE KNR                    TO FNRKNR (4:6)
               MOVE FNRKNR                 TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-19                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-19            TO TRUE
                   PERFORM KUNDEMA-FLDSET
                   PERFORM KUNDEMA-IDSET
               END-READ
           END-IF
           IF  (I-L1 AND NOT-I-19)
               MOVE KKAT                   TO KKAT-N
               MOVE KKAT-N-IO              TO KATNUM-IO
               MOVE KKAT                   TO KKAT-N
               MOVE KKAT-N-IO (2:2)        TO KATN2-IO
               MOVE KATNUM                 TO KUNKAT
      ** MLLzo
               MOVE '1'                    TO BW-A-2
               MULTIPLY 16                 BY BW-A
               MOVE KUNKAT (3:1)           TO BW-B-2
               MULTIPLY 16                 BY BW-B
               MOVE BW-A-1                 TO BW-B-1
               DIVIDE 16                   INTO BW-B
               MOVE BW-B-2                 TO KUNKAT (3:1)
           END-IF
           IF  (I-L1 AND I-19)
               MOVE '000'                  TO KUNKAT
           END-IF
           IF  (I-L1)
               SET I-51                    TO TRUE
           END-IF
           GO TO KORUT-T
      *****************************************************************
      * OPPSLAG MOT KUNDE.XTRA   FOR Å HENTE KUNDEKAT.                *
      *****************************************************************
           .
 
       KAJ2-T.
           IF  (I-L1)
               MOVE KFIRMA                 TO FNRKNR (1:3)
               MOVE KNR                    TO FNRKNR (4:6)
               MOVE FNRKNR                 TO FNRKN1 (1:9)
               MOVE '1'                    TO FNRKN1 (10:1)
               MOVE FNRKN1                 TO KUNDEMX-KEY1
               READ KUNDEMX RECORD KEY IS KUNDEMX-KEY1
               INVALID KEY
                   SET I-21                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-21            TO TRUE
                   PERFORM KUNDEMX-FLDSET
                   PERFORM KUNDEMX-IDSET
               END-READ
           END-IF
           IF  (I-L1 AND NOT-I-21)
               MOVE KKAT1                  TO KUNKAT
           END-IF
           IF  (I-L1 AND I-21)
               MOVE '000'                  TO KUNKAT
      *  L1      KUNKAT    COMP "000"                  22  LAVERE
           END-IF
           IF  (I-L1)
               SET NOT-I-22                TO TRUE
               IF  KUNKAT < 'ÅÅÅ'
                   SET I-22                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-22)
               MOVE '000'                  TO KUNKAT
           END-IF
           IF  (I-L1)
               MOVE KFIRMA                 TO FNRKNR (1:3)
               MOVE KNR                    TO FNRKNR (4:6)
               MOVE FNRKNR                 TO KUNDEMA-KEY1
               READ KUNDEMA RECORD KEY IS KUNDEMA-KEY1
               INVALID KEY
                   SET I-19                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-19            TO TRUE
                   PERFORM KUNDEMA-FLDSET
                   PERFORM KUNDEMA-IDSET
               END-READ
               SET I-51                    TO TRUE
           END-IF
           GO TO KORUT-T
      *****************************************************************
      * ENDRING AV KUNDEKATEGORI FIRMA 399 AUTODATA.                  *
      *****************************************************************
           .
 
       KORUT-T.
           IF  (I-L1)
               SET NOT-I-61                TO TRUE
               IF  FIRMA = '399'
                   SET I-61                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61)
               SET NOT-I-62                TO TRUE
               IF  KUNKAT < '999'
                   SET I-62                TO TRUE
               END-IF
               SET NOT-I-63                TO TRUE
               IF  KUNKAT > '300'
                   SET I-63                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61 AND I-62)
               AND (I-63)
               MOVE '120'                  TO KUNKAT
           END-IF
           IF  (I-L1)
               SET NOT-I-61                TO TRUE
               IF  FIRMA = '950'
                   SET I-61                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61)
               SET NOT-I-62                TO TRUE
               IF  KATN2 = 35
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KATN2 = 02
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KATN2 = 03
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KATN2 = 04
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KATN2 = 05
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KATN2 = 06
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KATN2 = 07
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KATN2 = 08
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KATN2 = 09
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KATN2 = 10
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KATN2 = 11
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KATN2 = 12
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KATN2 = 13
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KATN2 = 14
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KATN2 = 15
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KATN2 = 21
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KATN2 = 30
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KATN2 = 31
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KATN2 = 32
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KATN2 = 35
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KATN2 = 41
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KATN2 = 50
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KATN2 = 51
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KATN2 = 52
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KATN2 = 53
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KATN2 = 60
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KATN2 = 61
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KATN2 = 65
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KATN2 = 68
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KATN2 = 69
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KATN2 = 71
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KATN2 = 73
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KATN2 = 74
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KATN2 = 80
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KATN2 = 81
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KATN2 = 82
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KATN2 = 83
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KATN2 = 84
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KATN2 = 85
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61 AND NOT-I-62)
               SET NOT-I-62                TO TRUE
               IF  KATN2 = 88
                   SET I-62                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND I-61 AND I-62)
               ADD KATN2 TO ZERO       GIVING KATNUM
               MOVE KATNUM                 TO KUNKAT
      ** MLLzo
               MOVE '1'                    TO BW-A-2
               MULTIPLY 16                 BY BW-A
               MOVE KUNKAT (3:1)           TO BW-B-2
               MULTIPLY 16                 BY BW-B
               MOVE BW-A-1                 TO BW-B-1
               DIVIDE 16                   INTO BW-B
               MOVE BW-B-2                 TO KUNKAT (3:1)
      *****************************************************************
           END-IF
           .
 
       SLUTT-T.
           CONTINUE.
 
       INNF-GET SECTION.
       INNF-GET-P.
           IF  INNF-EOF-OFF
               READ INNF
               AT END
                   SET INNF-EOF            TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INNF-FLDSET SECTION.
       INNF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INNF-IO-AREA (1:3)     TO FIRMA (1:3)
               MOVE INNF-IO-AREA (4:1)     TO TOTTYP (1:1)
               MOVE INNF-IO-AREA (5:6)     TO KNR (1:6)
               MOVE INNF-IO-AREA (1:41)    TO INNREC (1:41)
               MOVE INNF-IO-AREA (42:3)    TO KFIRMA (1:3)
           END-EVALUATE.
 
       INNF-IDSET SECTION.
       INNF-IDSET-P.
           SET I-01                        TO TRUE.
 
       INNF-CHK-LEVEL SECTION.
       INNF-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO INNF-LEVEL-01
               MOVE INNF-IO-AREA (1:3)     TO INNF-01-L3-FIRMA
               MOVE INNF-IO-AREA (4:1)     TO INNF-01-L2-TOTTYP
               MOVE INNF-IO-AREA (5:6)     TO INNF-01-L1-KNR
               IF  INNF-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INNF-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  INNF-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INNF-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INNF-01-L3            TO THE-PRIOR-L3
               MOVE  INNF-01-L2            TO THE-PRIOR-L2
               MOVE  INNF-01-L1            TO THE-PRIOR-L1
               SET INNF-LEVEL-INIT         TO TRUE
           END-EVALUATE.
 
       KUNDEMA-FLDSET SECTION.
       KUNDEMA-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMA-IO-AREA (162:2) TO KKAT-IO
               MOVE KUNDEMA-IO-AREA (185:3) TO KHND (1:3)
           END-EVALUATE.
 
       KUNDEMA-IDSET SECTION.
       KUNDEMA-IDSET-P.
           SET I-03                        TO TRUE.
 
       KUNDEMX-FLDSET SECTION.
       KUNDEMX-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMX-IO-AREA (132:3) TO KKAT1 (1:3)
           END-EVALUATE.
 
       KUNDEMX-IDSET SECTION.
       KUNDEMX-IDSET-P.
           SET I-04                        TO TRUE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-51)
               MOVE SPACES TO UTF-IO-AREA
               INITIALIZE UTF-IO-AREA
               MOVE INNREC                 TO UTF-IO-AREA (1:41)
               IF  (I-11)
                   MOVE 'A'                TO UTF-IO-AREA (4:1)
               END-IF
               IF  (I-12)
                   MOVE 'B'                TO UTF-IO-AREA (4:1)
               END-IF
               MOVE 'KAT'                  TO UTF-IO-AREA (5:3)
               MOVE KUNKAT                 TO UTF-IO-AREA (8:3)
               MOVE KFIRMA                 TO UTF-IO-AREA (42:3)
      * HANDELS DISTRIKT TOTALER                                      *
               WRITE UTF-IO-AREA
               MOVE SPACES TO UTF-IO-AREA
               INITIALIZE UTF-IO-AREA
               MOVE INNREC                 TO UTF-IO-AREA (1:41)
               IF  (I-11)
                   MOVE 'I'                TO UTF-IO-AREA (4:1)
               END-IF
               IF  (I-12)
                   MOVE 'J'                TO UTF-IO-AREA (4:1)
               END-IF
               MOVE 'HND'                  TO UTF-IO-AREA (5:3)
               MOVE KHND                   TO UTF-IO-AREA (8:3)
               MOVE KFIRMA                 TO UTF-IO-AREA (42:3)
               WRITE UTF-IO-AREA
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
           SET INNF-LEVEL-INIT             TO TRUE
           INITIALIZE INNF-DATA-FIELDS
           SET INNF-EOF-OFF                TO TRUE
           SET INNF-PROCESS                TO TRUE
           OPEN INPUT INNF
           INITIALIZE KUNDEMA-DATA-FIELDS
           OPEN INPUT KUNDEMA
           INITIALIZE KUNDEMX-DATA-FIELDS
           OPEN INPUT KUNDEMX
           OPEN OUTPUT UTF.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INNF
           CLOSE KUNDEMA
           CLOSE KUNDEMX
           CLOSE UTF.
 
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
