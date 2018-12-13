       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK605R.
      **********************************************  Z-WIN-RPG2   ****
      * PROGRAM: FAK605  MERGE OG OPPDAT. TOTAL SALDO I FAKT.KONTROL. *
      *          REC. SAMT BEREGNE ANT. SIDER PR. FAKT/BM OG SJEKKE   *
      *          OM GIRO SKAL LAGES  PÅ EN EGEN SIDE TIL SLUTT.       *
      *  25/07-01 PROGRAMMERT AV ESPEN LARSEN                         *
      *  25/02-02 NY TEST PÅ NEGATIVE SALDO.                          *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK605.rpg
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
           SELECT FKREC2
               ASSIGN TO UT-S-FKREC2
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FKREC2-STATUS.
           SELECT FKREC1
               ASSIGN TO UT-S-FKREC1
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FKREC1-STATUS.
           SELECT KUNDEMX
               ASSIGN TO KUNDEMX
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS KUNDEMX-STATUS
               RECORD KEY IS KUNDEMX-KEY1.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
       DATA DIVISION.
       FILE SECTION.
       FD FKREC2
               BLOCK CONTAINS 80
               RECORD CONTAINS 40.
       01  FKREC2-IO-AREA.
           05  FKREC2-IO-AREA-X            PICTURE X(40).
       FD FKREC1
               BLOCK CONTAINS 400
               RECORD CONTAINS 200.
       01  FKREC1-IO-AREA.
           05  FKREC1-IO-AREA-X            PICTURE X(200).
       FD KUNDEMX
               RECORD CONTAINS 200.
       01  KUNDEMX-IO-AREA.
           05  KUNDEMX-IO-AREA-X.
               10  KUNDEMX-KEY1            PICTURE X(10).
               10  FILLER                  PICTURE X(190).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  FKREC2-STATUS               PICTURE 99 VALUE 0.
           10  FKREC1-STATUS               PICTURE 99 VALUE 0.
           10  KUNDEMX-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  FKREC2-EOF-OFF          VALUE '0'.
               88  FKREC2-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FKREC2-READ-OFF         VALUE '0'.
               88  FKREC2-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FKREC2-PROCESS-OFF      VALUE '0'.
               88  FKREC2-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  FKREC2-LEVEL-INIT-OFF   VALUE '0'.
               88  FKREC2-LEVEL-INIT       VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FKREC1-EOF-OFF          VALUE '0'.
               88  FKREC1-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FKREC1-READ-OFF         VALUE '0'.
               88  FKREC1-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  FKREC1-PROCESS-OFF      VALUE '0'.
               88  FKREC1-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  FKREC1-LEVEL-INIT-OFF   VALUE '0'.
               88  FKREC1-LEVEL-INIT       VALUE '1'.
           05  KUNDEMX-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  FKREC2-LEVEL-02.
               10  FKREC2-02-L3.
                   15  FKREC2-02-L3-FNR    PICTURE X(3).
               10  FKREC2-02-L2.
                   15  FKREC2-02-L2-KNR    PICTURE X(6).
               10  FKREC2-02-L1.
                   15  FKREC2-02-L1-BM     PICTURE X(2).
           05  FKREC2-DATA-FIELDS.
               10  FNR                     PICTURE X(3).
               10  KNR                     PICTURE X(6).
               10  BM                      PICTURE X(2).
               10  ANTFAK-IO.
                   15  ANTFAK              PICTURE S9(3).
               10  KSALDO-IO.
                   15  KSALDO              PICTURE S9(9)V9(2).
           05  FKREC2-MP                   PICTURE X(11).
           05  FKREC2-MC                   PICTURE X(11).
           05  FKREC2-M-02             REDEFINES FKREC2-MC.
               10  FKREC2-M-02-M3.
                   15  FKREC2-M-02-M3-FNR-G.
                       20  FKREC2-M-02-M3-FNR PICTURE X(3).
               10  FKREC2-M-02-M2.
                   15  FKREC2-M-02-M2-KNR-G.
                       20  FKREC2-M-02-M2-KNR PICTURE X(6).
               10  FKREC2-M-02-M1.
                   15  FKREC2-M-02-M1-BM-G.
                       20  FKREC2-M-02-M1-BM PICTURE X(2).
           05  FKREC1-LEVEL-01.
               10  FKREC1-01-L3.
                   15  FKREC1-01-L3-FNR    PICTURE X(3).
               10  FKREC1-01-L2.
                   15  FKREC1-01-L2-KNR    PICTURE X(6).
               10  FKREC1-01-L1.
                   15  FKREC1-01-L1-BM     PICTURE X(2).
           05  FKREC1-DATA-FIELDS.
               10  ANTVL-IO.
                   15  ANTVL               PICTURE S9(6).
           05  FKREC1-MP                   PICTURE X(11).
           05  FKREC1-MC                   PICTURE X(11).
           05  FKREC1-M-01             REDEFINES FKREC1-MC.
               10  FKREC1-M-01-M3.
                   15  FKREC1-M-01-M3-FNR-G.
                       20  FKREC1-M-01-M3-FNR PICTURE X(3).
               10  FKREC1-M-01-M2.
                   15  FKREC1-M-01-M2-KNR-G.
                       20  FKREC1-M-01-M2-KNR PICTURE X(6).
               10  FKREC1-M-01-M1.
                   15  FKREC1-M-01-M1-BM-G.
                       20  FKREC1-M-01-M1-BM PICTURE X(2).
           05  KUNDEMX-DATA-FIELDS.
               10  FPRTYP                  PICTURE X(1).
               10  MXGIRO                  PICTURE X(1).
           05  FIRMAF-DATA-FIELDS.
               10  KONFNR                  PICTURE X(3).
               10  RPROS-IO.
                   15  RPROS               PICTURE S9(1)V9(2).
               10  FAKTS                   PICTURE X(1).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(6).
               10  THE-PRIOR-L1            PICTURE X(2).
           05  TEMPORARY-FIELDS.
               10  FRENTE-IO.
                   15  FRENTE              PICTURE S9(2)V9(2).
               10  FIRMNR                  PICTURE X(3).
               10  FNRKNR                  PICTURE X(9).
               10  MXKEY                   PICTURE X(10).
               10  ANTSID-IO.
                   15  ANTSID              PICTURE S9(3).
               10  SIDANT-IO.
                   15  SIDANT              PICTURE S9(6).
               10  SSIDE-IO.
                   15  SSIDE               PICTURE S9(6).
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
           SET NOT-I-01                    TO TRUE
           SET NOT-I-08                    TO TRUE
           SET NOT-I-09                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  FKREC2-PROCESS
               SET FKREC2-PROCESS-OFF      TO TRUE
               SET FKREC2-READ             TO TRUE
           END-IF
 
           IF  FKREC2-READ
               PERFORM FKREC2-GET
               SET FKREC2-READ-OFF         TO TRUE
               IF  NOT FKREC2-EOF
                   PERFORM FKREC2-MATCH-SET
               END-IF
           END-IF
 
           IF  FKREC1-PROCESS
               SET FKREC1-PROCESS-OFF      TO TRUE
               SET FKREC1-READ             TO TRUE
           END-IF
 
           IF  FKREC1-READ
               PERFORM FKREC1-GET
               SET FKREC1-READ-OFF         TO TRUE
               IF  NOT FKREC1-EOF
                   PERFORM FKREC1-MATCH-SET
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
 
           IF  FKREC2-PROCESS
               PERFORM FKREC2-IDSET
           END-IF
 
           IF  FKREC1-PROCESS
               PERFORM FKREC1-IDSET
           END-IF
 
           IF  FKREC2-PROCESS
               PERFORM FKREC2-CHK-LEVEL
           END-IF
 
           IF  FKREC1-PROCESS
               PERFORM FKREC1-CHK-LEVEL
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
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  FKREC2-PROCESS
               PERFORM FKREC2-FLDSET
           END-IF
 
           IF  FKREC1-PROCESS
               PERFORM FKREC1-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  FKREC2-PROCESS
           OR  FKREC1-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-21                    TO TRUE
           SET NOT-I-22                    TO TRUE
           IF  (I-L3)
               MOVE 0,00                   TO FRENTE
               MOVE FNR                    TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-25                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-25            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
               SET NOT-I-58                TO TRUE
               IF  FAKTS = 'X'
                   SET I-58                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-58)
               SET NOT-I-58                TO TRUE
               IF  FAKTS = 'Y'
                   SET I-58                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND NOT-I-58)
               SET NOT-I-58                TO TRUE
               IF  FAKTS = 'Z'
                   SET I-58                TO TRUE
               END-IF
           END-IF
           IF  (I-L3)
               SET NOT-I-80                TO TRUE
               IF  KONFNR > '000'
                   SET I-80                TO TRUE
               END-IF
           END-IF
           IF  (I-L3 AND I-80)
               MOVE KONFNR                 TO FIRMNR
           END-IF
           IF  (I-L3 AND NOT-I-80)
               MOVE FNR                    TO FIRMNR
           END-IF
           IF  (I-L3 AND NOT-I-25)
               ADD RPROS TO ZERO       GIVING FRENTE
      *****************************************************************
      * FIRMA SOM SKAL HA BETALINGSDOKUMENT SELV OM GIRO = NEGATIV    *
      *       DETTE BENYTTES FOR Å INFORMASJON PÅ BETALINGSDOKUMENTET.*
      *****************************************************************
           END-IF
           IF  (I-L3)
               SET NOT-I-95                TO TRUE
               IF  FNR = '722'
                   SET I-95                TO TRUE
               END-IF
               SET NOT-I-93                TO TRUE
               IF  FNR = '923'
                   SET I-93                TO TRUE
               END-IF
      *****************************************************************
      * KUNDENR SOM IKKE SKAL PRINTE FAKTURA.                         *
      *****************************************************************
           END-IF
           IF  (I-L2)
               SET NOT-I-59                TO TRUE
               IF  KNR = '500165'
                   SET I-59                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-59)
               SET NOT-I-59                TO TRUE
               IF  KNR = '500190'
                   SET I-59                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-59)
               SET NOT-I-59                TO TRUE
               IF  KNR = '500191'
                   SET I-59                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-59)
               SET NOT-I-59                TO TRUE
               IF  KNR = '500192'
                   SET I-59                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-59)
               SET NOT-I-59                TO TRUE
               IF  KNR = '500193'
                   SET I-59                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-59)
               SET NOT-I-59                TO TRUE
               IF  KNR = '500194'
                   SET I-59                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-59 AND NOT-I-93)
               SET NOT-I-59                TO TRUE
               IF  KNR = '500195'
                   SET I-59                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-59)
               SET NOT-I-59                TO TRUE
               IF  KNR = '500196'
                   SET I-59                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-59)
               SET NOT-I-59                TO TRUE
               IF  KNR = '500197'
                   SET I-59                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-59)
               SET NOT-I-59                TO TRUE
               IF  KNR = '500198'
                   SET I-59                TO TRUE
               END-IF
           END-IF
           IF  (I-L2 AND NOT-I-59)
               SET NOT-I-59                TO TRUE
               IF  KNR = '500199'
                   SET I-59                TO TRUE
               END-IF
      *****************************************************************
      * SJEKK OM  BETALINGSKODE TILSIER ATT DET SKAL VÆRE GIRO.       *
      *****************************************************************
           END-IF
           IF  (I-L1)
               SET NOT-I-15                TO TRUE
               IF  BM = '02'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  BM = '03'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  BM = '04'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  BM = '05'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  BM = '08'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  BM = '11'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  BM = '12'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  BM = '15'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  BM = '16'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  BM = '17'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  BM = '25'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  BM = '26'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  BM = '27'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  BM = '28'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  BM = '30'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  BM = '31'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  BM = '32'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  BM = '33'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  BM = '34'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  BM = '35'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  BM = '36'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  BM = '37'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  BM = '38'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-L1 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  BM = '39'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-02)
               SET NOT-I-10                TO TRUE
               IF  ANTFAK > 1
                   SET I-10                TO TRUE
               END-IF
               SET NOT-I-11                TO TRUE
               SET NOT-I-12                TO TRUE
               IF  KSALDO < 0,00
                   SET I-11                TO TRUE
               END-IF
               IF  KSALDO = 0,00
                   SET I-12                TO TRUE
               END-IF
      *****************************************************************
      * RUTINE FOR Å HENTE GIROKODE FRA KUNDEMX.                      *
      *****************************************************************
           END-IF
           IF  (I-02)
               SET NOT-I-31                TO TRUE
               SET NOT-I-32                TO TRUE
               SET NOT-I-29                TO TRUE
               MOVE FIRMNR                 TO FNRKNR (1:3)
               MOVE KNR                    TO FNRKNR (4:6)
               MOVE FNRKNR                 TO MXKEY (1:9)
               MOVE '1'                    TO MXKEY (10:1)
               MOVE MXKEY                  TO KUNDEMX-KEY1
               READ KUNDEMX RECORD KEY IS KUNDEMX-KEY1
               INVALID KEY
                   SET I-24                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-24            TO TRUE
                   PERFORM KUNDEMX-FLDSET
                   PERFORM KUNDEMX-IDSET
               END-READ
           END-IF
           IF  (I-02 AND NOT-I-24)
               SET NOT-I-29                TO TRUE
               IF  FPRTYP > ' '
                   SET I-29                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND I-11 AND NOT-I-10)
               SET I-31                    TO TRUE
           END-IF
           IF  (I-02 AND I-11 AND I-10)
               AND (NOT-I-95)
               SET I-31                    TO TRUE
           END-IF
           IF  (I-02 AND I-12)
               SET I-31                    TO TRUE
           END-IF
           IF  (I-02 AND NOT-I-15)
               SET I-31                    TO TRUE
           END-IF
           IF  (I-02 AND NOT-I-24 AND NOT-I-31)
               SET NOT-I-31                TO TRUE
               IF  MXGIRO = 'N'
                   SET I-31                TO TRUE
               END-IF
      *****************************************************************
      * OM KUNDEN HAR FLERE FAKTURA SKAL DET IKKE PRINTES GIRO PÅ     *
      *    FAKTURAEN, MEN PÅ BETALINGSDOKUMENT TIL SLUTT. (SAMLEGIRO) *
      *****************************************************************
           END-IF
           IF  (I-02 AND NOT-I-24 AND I-10)
               SET NOT-I-32                TO TRUE
               IF  MXGIRO = 'S'
                   SET I-32                TO TRUE
               END-IF
           END-IF
           IF  (I-02 AND NOT-I-31 AND I-10)
               SET I-32                    TO TRUE
           END-IF
           IF  (I-02)
               GO TO SLUTT-T
      *****************************************************************
      * RUTINE FOR OG BEREGNE ANTALL SIDER PÅ FAKTURA.                *
      *    41 LINJER U/GIRO           17 LINJER M/GIRO.               *
      *****************************************************************
           END-IF
           IF  (I-01)
               MOVE 0                      TO ANTSID
               DIVIDE ANTVL BY 41      GIVING ANTSID
               MULTIPLY ANTSID BY 41   GIVING SIDANT
               SUBTRACT SIDANT FROM ANTVL GIVING SSIDE
               SET NOT-I-21                TO TRUE
               IF  SSIDE > 0
                   SET I-21                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-21)
               ADD 1                       TO ANTSID
      *****************************************************************
      * RUTINE FOR OG SJEKKE OM DET SKAL PRINTE GIRO PÅ EGEN SIDE.    *
      *  ER DET MERE EN 17 LINJER PÅ SISTE SIDE, ELLER OM SISTE SIDE  *
      *  ER FULL (41 LINJER), SKAL DET LAGES EN GIRO PÅ EGEN SIDE.    *
      *****************************************************************
           END-IF
           IF  (I-01)
               SET NOT-I-22                TO TRUE
               IF  SSIDE > 17
                   SET I-22                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND NOT-I-22)
               SET NOT-I-22                TO TRUE
               IF  SSIDE = 0
                   SET I-22                TO TRUE
               END-IF
           END-IF
           IF  (I-01 AND I-10)
               SET I-22                    TO TRUE
           END-IF
           IF  (I-01 AND I-11)
               SET NOT-I-22                TO TRUE
           END-IF
           IF  (I-01 AND I-31)
               SET NOT-I-22                TO TRUE
      ***************************************************************
           END-IF
           .
 
       SLUTT-T.
           CONTINUE.
 
       FKREC2-GET SECTION.
       FKREC2-GET-P.
           IF  FKREC2-EOF-OFF
               READ FKREC2
               AT END
                   SET FKREC2-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FKREC2-FLDSET SECTION.
       FKREC2-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FKREC2-IO-AREA (1:3)   TO FNR (1:3)
               MOVE FKREC2-IO-AREA (4:6)   TO KNR (1:6)
               MOVE FKREC2-IO-AREA (10:2)  TO BM (1:2)
               MOVE FKREC2-IO-AREA (12:3)  TO ANTFAK-IO
               INSPECT ANTFAK-IO REPLACING ALL ' ' BY '0'
               MOVE FKREC2-IO-AREA (15:11) TO KSALDO-IO
               INSPECT KSALDO-IO REPLACING ALL ' ' BY '0'
           END-EVALUATE.
 
       FKREC2-IDSET SECTION.
       FKREC2-IDSET-P.
           SET I-02                        TO TRUE.
 
       FKREC2-CHK-LEVEL SECTION.
       FKREC2-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO FKREC2-LEVEL-02
               MOVE FKREC2-IO-AREA (1:3)   TO FKREC2-02-L3-FNR
               MOVE FKREC2-IO-AREA (4:6)   TO FKREC2-02-L2-KNR
               MOVE FKREC2-IO-AREA (10:2)  TO FKREC2-02-L1-BM
               IF  FKREC2-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  FKREC2-02-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  FKREC2-02-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  FKREC2-02-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  FKREC2-02-L3          TO THE-PRIOR-L3
               MOVE  FKREC2-02-L2          TO THE-PRIOR-L2
               MOVE  FKREC2-02-L1          TO THE-PRIOR-L1
               SET FKREC2-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       FKREC2-MATCH-SET SECTION.
       FKREC2-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE FKREC2-IO-AREA (1:3)   TO FKREC2-M-02-M3-FNR
               MOVE FKREC2-IO-AREA (4:6)   TO FKREC2-M-02-M2-KNR
               MOVE FKREC2-IO-AREA (10:2)  TO FKREC2-M-02-M1-BM
           END-EVALUATE.
 
       FKREC1-GET SECTION.
       FKREC1-GET-P.
           IF  FKREC1-EOF-OFF
               READ FKREC1
               AT END
                   SET FKREC1-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       FKREC1-FLDSET SECTION.
       FKREC1-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FKREC1-IO-AREA (1:3)   TO FNR (1:3)
               MOVE FKREC1-IO-AREA (4:6)   TO KNR (1:6)
               MOVE FKREC1-IO-AREA (24:6)  TO ANTVL-IO
               INSPECT ANTVL-IO REPLACING ALL ' ' BY '0'
               MOVE FKREC1-IO-AREA (148:2) TO BM (1:2)
           END-EVALUATE.
 
       FKREC1-IDSET SECTION.
       FKREC1-IDSET-P.
           SET I-01                        TO TRUE.
 
       FKREC1-CHK-LEVEL SECTION.
       FKREC1-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO FKREC1-LEVEL-01
               MOVE FKREC1-IO-AREA (1:3)   TO FKREC1-01-L3-FNR
               MOVE FKREC1-IO-AREA (4:6)   TO FKREC1-01-L2-KNR
               MOVE FKREC1-IO-AREA (148:2) TO FKREC1-01-L1-BM
               IF  FKREC1-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  FKREC1-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  FKREC1-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  FKREC1-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  FKREC1-01-L3          TO THE-PRIOR-L3
               MOVE  FKREC1-01-L2          TO THE-PRIOR-L2
               MOVE  FKREC1-01-L1          TO THE-PRIOR-L1
               SET FKREC1-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       FKREC1-MATCH-SET SECTION.
       FKREC1-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE FKREC1-IO-AREA (1:3)   TO FKREC1-M-01-M3-FNR
               MOVE FKREC1-IO-AREA (4:6)   TO FKREC1-M-01-M2-KNR
               MOVE FKREC1-IO-AREA (148:2) TO FKREC1-M-01-M1-BM
           END-EVALUATE.
 
       KUNDEMX-FLDSET SECTION.
       KUNDEMX-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE KUNDEMX-IO-AREA (43:1) TO FPRTYP (1:1)
               MOVE KUNDEMX-IO-AREA (159:1) TO MXGIRO (1:1)
           END-EVALUATE.
 
       KUNDEMX-IDSET SECTION.
       KUNDEMX-IDSET-P.
           SET I-08                        TO TRUE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (1:3)   TO KONFNR (1:3)
               MOVE FIRMAF-IO-AREA (136:3) TO RPROS-IO
               INSPECT RPROS-IO REPLACING ALL ' ' BY '0'
               MOVE FIRMAF-IO-AREA (780:1) TO FAKTS (1:1)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-09                        TO TRUE.
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  FKREC2-EOF
               MOVE HIGH-VALUES            TO FKREC2-MC
                                              FKREC2-MP
           END-IF
           IF  FKREC1-EOF
               MOVE HIGH-VALUES            TO FKREC1-MC
                                              FKREC1-MP
           END-IF
           IF  FKREC2-MC < FKREC2-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  FKREC1-MC < FKREC1-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  FKREC2-MC < FKREC1-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FKREC2-PROCESS      TO TRUE
                   MOVE FKREC2-MC          TO FKREC2-MP
                   IF  FKREC2-MC = FKREC1-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  FKREC1-MC < FKREC2-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FKREC1-PROCESS      TO TRUE
                   MOVE FKREC1-MC          TO FKREC1-MP
                   IF  FKREC1-MC = FKREC2-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  FKREC2-MC = FKREC1-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET FKREC2-PROCESS      TO TRUE
                   MOVE FKREC2-MC          TO FKREC2-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01 AND I-MR)
               MOVE ANTSID-IO              TO FKREC1-IO-AREA (30:3)
               IF  (I-31)
                   MOVE 'N'                TO FKREC1-IO-AREA (80:1)
               END-IF
               IF  (NOT-I-31 AND NOT-I-32)
                   MOVE 'J'                TO FKREC1-IO-AREA (80:1)
               END-IF
               IF  (NOT-I-31 AND I-32)
                   MOVE 'S'                TO FKREC1-IO-AREA (80:1)
               END-IF
               MOVE FRENTE-IO              TO FKREC1-IO-AREA (121:4)
               IF  (NOT-I-22)
                   MOVE 'N'                TO FKREC1-IO-AREA (125:1)
               END-IF
               IF  (I-22)
                   MOVE 'J'                TO FKREC1-IO-AREA (125:1)
               END-IF
               MOVE KSALDO-IO              TO FKREC1-IO-AREA (126:11)
               MOVE ANTFAK-IO              TO FKREC1-IO-AREA (143:3)
               IF  (I-24)
                   MOVE 'N'                TO FKREC1-IO-AREA (146:1)
               END-IF
               IF  (I-58)
                   MOVE FAKTS              TO FKREC1-IO-AREA (146:1)
               END-IF
               IF  (I-59)
                   MOVE 'U'                TO FKREC1-IO-AREA (146:1)
               END-IF
               IF  (I-29)
                   MOVE FPRTYP             TO FKREC1-IO-AREA (146:1)
               END-IF
               REWRITE FKREC1-IO-AREA
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
           SET FKREC2-LEVEL-INIT           TO TRUE
           INITIALIZE FKREC2-DATA-FIELDS
           SET FKREC2-EOF-OFF              TO TRUE
           SET FKREC2-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO FKREC2-MC
                                              FKREC2-MP
           OPEN INPUT FKREC2
           SET FKREC1-LEVEL-INIT           TO TRUE
           INITIALIZE FKREC1-DATA-FIELDS
           SET FKREC1-EOF-OFF              TO TRUE
           SET FKREC1-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO FKREC1-MC
                                              FKREC1-MP
           OPEN I-O FKREC1
           INITIALIZE KUNDEMX-DATA-FIELDS
           OPEN INPUT KUNDEMX
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FKREC2
           CLOSE FKREC1
           CLOSE KUNDEMX
           CLOSE FIRMAF.
 
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
