       IDENTIFICATION DIVISION.
       PROGRAM-ID. RKO030R.
      **********************************************  Z-WIN-RPG2   ****
      * NY VERSJON AV RSK.RSK030                 ***TXT***OK MT
      *  PROGRAM....: RKO030 - FØR SEPT.05-RSK030                     *
      *  PROGRAM....: RKO030, OPPDAT MÅNEDENS KONTOKURANT.            *
      *  ENDRET.....: 19.09.97 LEGGER INN 6-SIFRET PERIODE (ERSTATTET *
      *                        RESPAR MED AUTOPAR).                   *
      *                        SJEKKER DATO MOT SUBRUTINE ""DATO8SIF"". *
      *                        LEGGER INN BILAGSÅRHUNDRE.             *
      *                        MER DETALJERT KVITTERING M/FEILMELD.   *
      *               21.10.98 LEGGER INN 6-SIFRET PERIODE I DAGENS   *
      *                        RESKONTROTRANSER (DAGRESK/DAGRESU).    *
      *               18.01.00 PÅMINNELSE OM Å SJEKKE SANERINGER.     *
      *               14.09.05 UTVIDET DAGENS TRANSER TIL 120 BYTE    *
      *               18.09.09 UTVIDET DAGENS TRANSER TIL 120 BYTE    *
      *               15.04.11 NYE BELØPSFELT                         *
      *               04.10.12 SKRIVER AVSTEMMINGSFIL                 *
      *  FÅR........: PARAMETERFILE MED REGNSKAPSPERIODE (AUTOPAR),   *
      *               SAMTLIGE DAGENS RESKONTROTRANSER (DAGRESK),     *
      *               MÅNEDENS TIDLIGERE RESKONTROTRANSER (GMLKTO).   *
      *  GJØR.......: MERGER DAGENS RESKONTROTRANSER MED MÅNEDENS     *
      *               TIDLIGERE RESKONTROTRANSER.                     *
      *               HENTER REGSKAPSPARAMETER MED INNEVÆRENDE PERIODE*
      *               OG BEREGNER NESTE PERIODE. SETTER INNEVÆRENDE   *
      *               PERIODE PÅ TRANSER MED BILAGSDATO LIK ELLER     *
      *               MINDRE ENN INNEVÆRENDE PERIODE, OG NESTE PERIODE*
      *               PÅ ALLE TRANSER MED BILAGSMÅNED FRAM I TID.     *
      *               LEGGER PÅ BILAGSÅRHUNDRE PÅ ALLE TRANSER: HVIS  *
      *               BILAGSÅR > 80 SETTES 19 ELLERS SETTES 20 (HENTES*
      *               VIA SUBPROGRAM "DATO8SIF".                      *
      *               UPSI-1 ER PÅ: BRUKER FORRIGE PERIODE FRA AUTOPAR*
      *  GIR........: MÅNEDENS RESKONTROTRANSER.                      *
      *****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RKO030.rpg
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
           SELECT GMLKTO
               ASSIGN TO UT-S-GMLKTO
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS GMLKTO-STATUS.
           SELECT DAGRESK
               ASSIGN TO UT-S-DAGRESK
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS DAGRESK-STATUS.
           SELECT AUTOPAR
               ASSIGN TO AUTOPAR
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS AUTOPAR-STATUS
               RECORD KEY IS AUTOPAR-KEY1.
           SELECT NYKTO
               ASSIGN TO UT-S-NYKTO
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS NYKTO-STATUS.
           SELECT DAGRESU
               ASSIGN TO UT-S-DAGRESU
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS DAGRESU-STATUS.
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
       FD GMLKTO
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  GMLKTO-IO-AREA.
           05  GMLKTO-IO-AREA-X            PICTURE X(200).
       FD DAGRESK
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  DAGRESK-IO-AREA.
           05  DAGRESK-IO-AREA-X           PICTURE X(200).
       FD AUTOPAR
               RECORD CONTAINS 1000.
       01  AUTOPAR-IO-AREA.
           05  AUTOPAR-IO-AREA-X.
               10  AUTOPAR-KEY1            PICTURE X(3).
               10  FILLER                  PICTURE X(997).
       FD NYKTO
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  NYKTO-IO-AREA.
           05  NYKTO-IO-AREA-X             PICTURE X(200).
       FD DAGRESU
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  DAGRESU-IO-AREA.
           05  DAGRESU-IO-AREA-X           PICTURE X(200).
       FD AVSTEMM
               BLOCK CONTAINS 120
               RECORD CONTAINS 120.
       01  AVSTEMM-IO-AREA.
           05  AVSTEMM-IO-AREA-X           PICTURE X(120).
      *****************************************************************
      * DATASTRUKTUR FOR SUBPROGRAM DATO8SIF                          *
      *****************************************************************
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
           10  GMLKTO-STATUS               PICTURE 99 VALUE 0.
           10  DAGRESK-STATUS              PICTURE 99 VALUE 0.
           10  AUTOPAR-STATUS              PICTURE 99 VALUE 0.
           10  NYKTO-STATUS                PICTURE 99 VALUE 0.
           10  DAGRESU-STATUS              PICTURE 99 VALUE 0.
           10  AVSTEMM-STATUS              PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
           10  DTOPAR-XX-STATUS            PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLKTO-EOF-OFF          VALUE '0'.
               88  GMLKTO-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLKTO-READ-OFF         VALUE '0'.
               88  GMLKTO-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  GMLKTO-PROCESS-OFF      VALUE '0'.
               88  GMLKTO-PROCESS          VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  DAGRESK-EOF-OFF         VALUE '0'.
               88  DAGRESK-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  DAGRESK-READ-OFF        VALUE '0'.
               88  DAGRESK-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  DAGRESK-PROCESS-OFF     VALUE '0'.
               88  DAGRESK-PROCESS         VALUE '1'.
           05  AUTOPAR-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
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
           05  DTOPAR-XX-DATA-FIELDS.
               10  DTOKOD                  PICTURE X(1).
               10  FILLER                  PICTURE X(22).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(1).
               10  DTODTO                  PICTURE X(6).
               10  FILLER                  PICTURE X(16).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(15).
               10  DTOARH                  PICTURE X(2).
               10  FILLER                  PICTURE X(6).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(15).
               10  DTO8SI                  PICTURE X(8).
           05  FILLER REDEFINES DTOPAR-XX-DATA-FIELDS.
               10  FILLER                  PICTURE X(15).
               10  DTOPER                  PICTURE X(6).
               10  FILLER                  PICTURE X(2).
           05  GMLKTO-DATA-FIELDS.
               10  REC1                    PICTURE X(200).
               10  RA1                     PICTURE X(1).
               10  SALD-IO.
                   15  SALD                PICTURE S9(8)V9(2).
               10  SALV-IO.
                   15  SALV                PICTURE S9(9)V9(2).
               10  SALDU-IO.
                   15  SALDU               PICTURE S9(11)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  VSALDU-IO.
                   15  VSALDU              PICTURE S9(11)V9(4) USAGE
                                                       PACKED-DECIMAL.
               10  REC2                    PICTURE X(200).
               10  RA2                     PICTURE X(1).
               10  BEL2-IO.
                   15  BEL2                PICTURE S9(7)V9(2).
               10  VAL2-IO.
                   15  VAL2                PICTURE S9(8)V9(2).
               10  BEL2U-IO.
                   15  BEL2U               PICTURE S9(11)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  VAL2U-IO.
                   15  VAL2U               PICTURE S9(11)V9(4) USAGE
                                                       PACKED-DECIMAL.
           05  GMLKTO-MP                   PICTURE X(1).
           05  GMLKTO-MC                   PICTURE X(1).
           05  GMLKTO-M-01             REDEFINES GMLKTO-MC.
               10  GMLKTO-M-01-M1.
                   15  GMLKTO-M-01-M1-RA1-G.
                       20  GMLKTO-M-01-M1-RA1 PICTURE X(1).
           05  GMLKTO-M-02             REDEFINES GMLKTO-MC.
               10  GMLKTO-M-02-M1.
                   15  GMLKTO-M-02-M1-RA2-G.
                       20  GMLKTO-M-02-M1-RA2 PICTURE X(1).
           05  DAGRESK-DATA-FIELDS.
               10  REC3                    PICTURE X(200).
               10  RA3                     PICTURE X(1).
               10  BILDAT-IO.
                   15  BILDAT              PICTURE S9(6).
               10  BEL3-IO.
                   15  BEL3                PICTURE S9(7)V9(2).
               10  VAL3-IO.
                   15  VAL3                PICTURE S9(8)V9(2).
               10  BEL3U-IO.
                   15  BEL3U               PICTURE S9(11)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  VAL3U-IO.
                   15  VAL3U               PICTURE S9(11)V9(4) USAGE
                                                       PACKED-DECIMAL.
           05  DAGRESK-MP                  PICTURE X(1).
           05  DAGRESK-MC                  PICTURE X(1).
           05  DAGRESK-M-03            REDEFINES DAGRESK-MC.
               10  DAGRESK-M-03-M1.
                   15  DAGRESK-M-03-M1-RA3-G.
                       20  DAGRESK-M-03-M1-RA3 PICTURE X(1).
           05  AUTOPAR-DATA-FIELDS.
               10  PARPER                  PICTURE X(6).
               10  FORPER                  PICTURE X(6).
               10  PARMND                  PICTURE X(2).
           05  TEMPORARY-FIELDS.
               10  F6-IO.
                   15  F6                  PICTURE S9(6).
               10  DD-IO.
                   15  DD                  PICTURE S9(2).
               10  AA-IO.
                   15  AA                  PICTURE S9(2).
               10  DDATO                   PICTURE X(6).
               10  DDATO8                  PICTURE X(8).
               10  TIDSP-IO.
                   15  TIDSP               PICTURE S9(6).
               10  TOTREC-IO.
                   15  TOTREC              PICTURE S9(6).
               10  TOTBEL-IO.
                   15  TOTBEL              PICTURE S9(9)V9(2).
               10  TOTVAL-IO.
                   15  TOTVAL              PICTURE S9(10)V9(2).
               10  TOTBEU-IO.
                   15  TOTBEU              PICTURE S9(11)V9(2).
               10  TOTVAU-IO.
                   15  TOTVAU              PICTURE S9(11)V9(4).
               10  ANTTOT-IO.
                   15  ANTTOT              PICTURE S9(6).
               10  BELTOT-IO.
                   15  BELTOT              PICTURE S9(9)V9(2).
               10  BELTOU-IO.
                   15  BELTOU              PICTURE S9(11)V9(2).
               10  ANTPER-IO.
                   15  ANTPER              PICTURE S9(6).
               10  BELPER-IO.
                   15  BELPER              PICTURE S9(9)V9(2).
               10  BELPEU-IO.
                   15  BELPEU              PICTURE S9(11)V9(2).
               10  ANTNES-IO.
                   15  ANTNES              PICTURE S9(6).
               10  BELNES-IO.
                   15  BELNES              PICTURE S9(9)V9(2).
               10  BELNEU-IO.
                   15  BELNEU              PICTURE S9(11)V9(2).
               10  ANTFEI-IO.
                   15  ANTFEI              PICTURE S9(6).
               10  BELFEI-IO.
                   15  BELFEI              PICTURE S9(9)V9(2).
               10  BELFEU-IO.
                   15  BELFEU              PICTURE S9(11)V9(2).
               10  LRRSAN-IO.
                   15  LRRSAN              PICTURE S9(13).
               10  LRRSBE-IO.
                   15  LRRSBE              PICTURE S9(13)V9(2).
               10  LRRSBU-IO.
                   15  LRRSBU              PICTURE S9(13)V9(2).
               10  AUTKEY                  PICTURE X(3).
               10  NUMPER-IO.
                   15  NUMPER              PICTURE S9(6).
               10  MND-IO.
                   15  MND                 PICTURE S9(2).
               10  NESPER                  PICTURE X(6).
           05  EDITTING-FIELDS.
               10  XO-60YNZ                PICTURE ZZZZZZ.
               10  XO-92YY9R               PICTURE ZZZ.ZZZ.ZZZ,99-.
               10  XO-112YY9R              PICTURE ZZ.ZZZ.ZZZ.ZZZ,99-.
               10  XO-102YY9R              PICTURE Z.ZZZ.ZZZ.ZZZ,99-.
               10  XO-114YY9R              PICTURE
                                                 ZZ.ZZZ.ZZZ.ZZZ,9999-.
               10  XO-60YN9R               PICTURE ZZZZZ9-.
               10  EDIT-BELTOT             PICTURE ZZZ.ZZZ.ZZZ,ZZ-.
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
           SET NOT-I-03                    TO TRUE
           SET NOT-I-04                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  GMLKTO-PROCESS
               SET GMLKTO-PROCESS-OFF      TO TRUE
               SET GMLKTO-READ             TO TRUE
           END-IF
 
           IF  GMLKTO-READ
               PERFORM GMLKTO-GET
               SET GMLKTO-READ-OFF         TO TRUE
               IF  NOT GMLKTO-EOF
                   PERFORM GMLKTO-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM GMLKTO-MATCH-SET
               END-IF
           END-IF
 
           IF  DAGRESK-PROCESS
               SET DAGRESK-PROCESS-OFF     TO TRUE
               SET DAGRESK-READ            TO TRUE
           END-IF
 
           IF  DAGRESK-READ
               PERFORM DAGRESK-GET
               SET DAGRESK-READ-OFF        TO TRUE
               IF  NOT DAGRESK-EOF
                   PERFORM DAGRESK-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   PERFORM DAGRESK-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  GMLKTO-PROCESS
               PERFORM GMLKTO-IDSET
           END-IF
 
           IF  DAGRESK-PROCESS
               PERFORM DAGRESK-IDSET
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
           PERFORM DETAIL-OVERFLOW
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  GMLKTO-PROCESS
               PERFORM GMLKTO-FLDSET
           END-IF
 
           IF  DAGRESK-PROCESS
               PERFORM DAGRESK-FLDSET
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
           IF  (I-88)
               SET NOT-I-89                TO TRUE
           END-IF
           IF  (NOT-I-88)
               SET I-88                    TO TRUE
               SET I-89                    TO TRUE
           END-IF
           IF  (I-89)
               MOVE UDATE                  TO F6-IO
               MOVE F6 (1:2)               TO DD
               MOVE F6 (5:2)               TO AA-IO
      ** MLLzo
               IF AA < 0
                   MULTIPLY -1 BY AA
               END-IF
               MOVE F6                     TO DDATO
               MOVE AA                     TO DDATO (1:2)
               MOVE DD                     TO DDATO (5:2)
               SET NOT-I-31                TO TRUE
               IF  AA > 80
                   SET I-31                TO TRUE
               END-IF
               MOVE DDATO                  TO DDATO8 (3:6)
           END-IF
           IF  (I-89 AND I-31)
               MOVE '19'                   TO DDATO8 (1:2)
           END-IF
           IF  (I-89 AND NOT-I-31)
               MOVE '20'                   TO DDATO8 (1:2)
           END-IF
           IF  (I-89)
               ACCEPT SYSTEM-TIME-X      FROM TIME
               MOVE SYSTEM-TIME            TO TIDSP (1:6)
      *  89                MOVE "DDATO8  "BUGFL1  8        DISPLAY FIELD
      *  89      BUGFL1    DEBUGBUGFILO   DDATO8           VIS INDIKATOR
           END-IF
           IF  (NOT-I-99)
               PERFORM PARRUT-S
           END-IF
           IF  (I-01)
               ADD 1                       TO TOTREC
               ADD SALD                    TO TOTBEL
               ADD SALV                    TO TOTVAL
               ADD SALDU                   TO TOTBEU
               ADD VSALDU                  TO TOTVAU
           END-IF
           IF  (I-02)
               ADD 1                       TO TOTREC
               ADD BEL2                    TO TOTBEL
               ADD VAL2                    TO TOTVAL
               ADD BEL2U                   TO TOTBEU
               ADD VAL2U                   TO TOTVAU
           END-IF
           IF  (I-03)
               ADD 1                       TO TOTREC
               ADD BEL3                    TO TOTBEL
               ADD VAL3                    TO TOTVAL
               ADD BEL3U                   TO TOTBEU
               ADD VAL3U                   TO TOTVAU
               PERFORM DTORUT-S
           END-IF
           IF  (I-03 AND NOT-I-98 AND I-20)
               SET I-98                    TO TRUE
           END-IF
           IF  (I-03)
               ADD 1                       TO ANTTOT
               ADD BEL3                    TO BELTOT
               ADD BEL3U                   TO BELTOU
           END-IF
           IF  (I-03 AND NOT-I-98 AND I-31)
               ADD 1                       TO ANTPER
               ADD BEL3                    TO BELPER
               ADD BEL3U                   TO BELPEU
           END-IF
           IF  (I-03 AND NOT-I-98 AND I-30)
               ADD 1                       TO ANTNES
               ADD BEL3                    TO BELNES
               ADD BEL3U                   TO BELNEU
           END-IF
           IF  (I-03 AND I-98)
               ADD 1                       TO ANTFEI
               SET NOT-I-13                TO TRUE
               IF  ANTFEI > 0
                   SET I-13                TO TRUE
               END-IF
               ADD BEL3                    TO BELFEI
               ADD BEL3U                   TO BELFEU
           END-IF
           IF  (NOT-I-03)
               GO TO STOP-X-T
           END-IF.
 
       STOP-X-T.
           CONTINUE.
 
       PARRUT-S SECTION.
       PARRUT-S-P.
           MOVE 'A01'                      TO AUTKEY
           MOVE AUTKEY                     TO AUTOPAR-KEY1
           READ AUTOPAR RECORD KEY IS AUTOPAR-KEY1
           INVALID KEY
               SET I-20                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-20                TO TRUE
               PERFORM AUTOPAR-FLDOFF
               PERFORM AUTOPAR-FLDSET
               PERFORM AUTOPAR-IDSET
           END-READ
      *R    U8             MOVE "      "  PARPER           TEST FEIL
      *R    U8             SETON                     15    TEST FEIL
      *R    U7             MOVE "199912"  PARPER           TEST ÅRSKIFTE
      *R    U7             MOVE "12"      PARMND           ------""------
           IF  (NOT-I-20 AND I-15)
               SET I-20                    TO TRUE
           END-IF
           IF  (I-20)
               GO TO ENDPAR-T
           END-IF
           IF  (I-U1)
               MOVE FORPER                 TO PARPER
           END-IF
           SET NOT-I-21                    TO TRUE
           IF  PARMND NOT < '12'
               SET I-21                    TO TRUE
           END-IF
           MOVE PARPER                     TO NUMPER-IO
           IF  (I-21)
               ADD 100                     TO NUMPER
               MOVE PARMND                 TO MND-IO
               SUBTRACT MND                FROM NUMPER
           END-IF
           ADD 1                           TO NUMPER
           MOVE NUMPER                     TO NESPER
           SET I-99                        TO TRUE.
 
       ENDPAR-T.
           CONTINUE.
      *****************************************************************
      *  SUBRUTINE FOR Å HENTE 8-SIFRET DATO.                         * *
      *****************************************************************
 
       DTORUT-S SECTION.
       DTORUT-S-P.
           SET NOT-I-30                    TO TRUE
           SET NOT-I-31                    TO TRUE
           SET NOT-I-98                    TO TRUE
           MOVE 'B'                        TO DTOKOD
           MOVE BILDAT                     TO DTODTO
           CALL 'DATO8SIF' USING DTOPAR-XX-DATA-FIELDS
           SET NOT-I-98                    TO TRUE
           IF  DTOKOD = 'F'
               SET I-98                    TO TRUE
           END-IF
      *R    U6             MOVE "199802"  DTOPER            TEST SKIFTE
           IF  (NOT-I-98)
               SET NOT-I-30                TO TRUE
               SET NOT-I-31                TO TRUE
               IF  DTOPER NOT > PARPER
                   SET I-31                TO TRUE
               END-IF
               IF  DTOPER > PARPER
                   SET I-30                TO TRUE
               END-IF
           END-IF.
 
       LR-CALCS SECTION.
       LR-CALCS-P.
           ADD TOTREC TO ZERO          GIVING LRRSAN
           ADD TOTBEL TO ZERO          GIVING LRRSBE
           ADD TOTBEU TO ZERO          GIVING LRRSBU
      *****************************************************************
      *  SUBRUTINE FOR Å HENTE PARAMETER PERIODER.                    * *
      *****************************************************************
           .
 
       GMLKTO-GET SECTION.
       GMLKTO-GET-P.
           IF  GMLKTO-EOF-OFF
               READ GMLKTO
               AT END
                   SET GMLKTO-EOF          TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       GMLKTO-FLDSET SECTION.
       GMLKTO-FLDSET-P.
           EVALUATE TRUE
           WHEN ( GMLKTO-IO-AREA (1:1) = '3'
            AND   GMLKTO-IO-AREA (2:1) = '0' )
               MOVE GMLKTO-IO-AREA (1:200) TO REC1 (1:200)
               MOVE GMLKTO-IO-AREA (1:1)   TO RA1 (1:1)
               MOVE GMLKTO-IO-AREA (38:10) TO SALD-IO
               INSPECT SALD-IO REPLACING ALL ' ' BY '0'
               MOVE GMLKTO-IO-AREA (50:11) TO SALV-IO
               INSPECT SALV-IO REPLACING ALL ' ' BY '0'
               MOVE GMLKTO-IO-AREA (114:7) TO SALDU-IO
               MOVE GMLKTO-IO-AREA (121:8) TO VSALDU-IO
           WHEN ( GMLKTO-IO-AREA (1:1) = '3'
            AND   GMLKTO-IO-AREA (2:1) = '1' )
               MOVE GMLKTO-IO-AREA (1:200) TO REC2 (1:200)
               MOVE GMLKTO-IO-AREA (1:1)   TO RA2 (1:1)
               MOVE GMLKTO-IO-AREA (39:9)  TO BEL2-IO
               INSPECT BEL2-IO REPLACING ALL ' ' BY '0'
               MOVE GMLKTO-IO-AREA (51:10) TO VAL2-IO
               INSPECT VAL2-IO REPLACING ALL ' ' BY '0'
               MOVE GMLKTO-IO-AREA (114:7) TO BEL2U-IO
               MOVE GMLKTO-IO-AREA (121:8) TO VAL2U-IO
           END-EVALUATE.
 
       GMLKTO-IDCHK SECTION.
       GMLKTO-IDCHK-P.
           EVALUATE TRUE
           WHEN ( GMLKTO-IO-AREA (1:1) = '3'
            AND   GMLKTO-IO-AREA (2:1) = '0' )
             OR ( GMLKTO-IO-AREA (1:1) = '3'
            AND   GMLKTO-IO-AREA (2:1) = '1' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       GMLKTO-IDSET SECTION.
       GMLKTO-IDSET-P.
           EVALUATE TRUE
           WHEN ( GMLKTO-IO-AREA (1:1) = '3'
            AND   GMLKTO-IO-AREA (2:1) = '0' )
               SET I-01                    TO TRUE
           WHEN ( GMLKTO-IO-AREA (1:1) = '3'
            AND   GMLKTO-IO-AREA (2:1) = '1' )
               SET I-02                    TO TRUE
           END-EVALUATE.
 
       GMLKTO-MATCH-SET SECTION.
       GMLKTO-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( GMLKTO-IO-AREA (1:1) = '3'
            AND   GMLKTO-IO-AREA (2:1) = '0' )
               MOVE GMLKTO-IO-AREA (1:1)   TO GMLKTO-M-01-M1-RA1
           WHEN ( GMLKTO-IO-AREA (1:1) = '3'
            AND   GMLKTO-IO-AREA (2:1) = '1' )
               MOVE GMLKTO-IO-AREA (1:1)   TO GMLKTO-M-02-M1-RA2
           END-EVALUATE.
 
       DAGRESK-GET SECTION.
       DAGRESK-GET-P.
           IF  DAGRESK-EOF-OFF
               READ DAGRESK
               AT END
                   SET DAGRESK-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       DAGRESK-FLDSET SECTION.
       DAGRESK-FLDSET-P.
           EVALUATE TRUE
           WHEN ( DAGRESK-IO-AREA (1:1) = '3'
            AND   DAGRESK-IO-AREA (2:1) = '1' )
               MOVE DAGRESK-IO-AREA (1:200) TO REC3 (1:200)
               MOVE DAGRESK-IO-AREA (1:1)  TO RA3 (1:1)
               MOVE DAGRESK-IO-AREA (20:6) TO BILDAT-IO
               INSPECT BILDAT-IO REPLACING ALL ' ' BY '0'
               MOVE DAGRESK-IO-AREA (39:9) TO BEL3-IO
               INSPECT BEL3-IO REPLACING ALL ' ' BY '0'
               MOVE DAGRESK-IO-AREA (51:10) TO VAL3-IO
               INSPECT VAL3-IO REPLACING ALL ' ' BY '0'
               MOVE DAGRESK-IO-AREA (114:7) TO BEL3U-IO
               MOVE DAGRESK-IO-AREA (121:8) TO VAL3U-IO
           END-EVALUATE.
 
       DAGRESK-IDCHK SECTION.
       DAGRESK-IDCHK-P.
           EVALUATE TRUE
           WHEN ( DAGRESK-IO-AREA (1:1) = '3'
            AND   DAGRESK-IO-AREA (2:1) = '1' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       DAGRESK-IDSET SECTION.
       DAGRESK-IDSET-P.
           EVALUATE TRUE
           WHEN ( DAGRESK-IO-AREA (1:1) = '3'
            AND   DAGRESK-IO-AREA (2:1) = '1' )
               SET I-03                    TO TRUE
           END-EVALUATE.
 
       DAGRESK-MATCH-SET SECTION.
       DAGRESK-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ( DAGRESK-IO-AREA (1:1) = '3'
            AND   DAGRESK-IO-AREA (2:1) = '1' )
               MOVE DAGRESK-IO-AREA (1:1)  TO DAGRESK-M-03-M1-RA3
           END-EVALUATE.
 
       AUTOPAR-FLDOFF SECTION.
       AUTOPAR-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-15                TO TRUE
           END-EVALUATE.
 
       AUTOPAR-FLDSET SECTION.
       AUTOPAR-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE AUTOPAR-IO-AREA (12:6) TO PARPER (1:6)
               IF  PARPER = SPACES
                   SET I-15                TO TRUE
               END-IF
               MOVE AUTOPAR-IO-AREA (18:6) TO FORPER (1:6)
               MOVE AUTOPAR-IO-AREA (16:2) TO PARMND (1:2)
           END-EVALUATE.
 
       AUTOPAR-IDSET SECTION.
       AUTOPAR-IDSET-P.
           SET I-04                        TO TRUE.
 
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
               SET I-OF                    TO TRUE
               IF  IN-DETAIL-OUTPUT
                   SET SET-I-OF            TO TRUE
               END-IF
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
 
       MATCHING-RECORDS SECTION.
       MATCHING-RECORDS-P.
           IF  GMLKTO-EOF
               MOVE HIGH-VALUES            TO GMLKTO-MC
                                              GMLKTO-MP
           END-IF
           IF  DAGRESK-EOF
               MOVE HIGH-VALUES            TO DAGRESK-MC
                                              DAGRESK-MP
           END-IF
           IF  GMLKTO-MC < GMLKTO-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  DAGRESK-MC < DAGRESK-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  GMLKTO-MC < DAGRESK-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET GMLKTO-PROCESS      TO TRUE
                   MOVE GMLKTO-MC          TO GMLKTO-MP
                   IF  GMLKTO-MC = DAGRESK-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  DAGRESK-MC < GMLKTO-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET DAGRESK-PROCESS     TO TRUE
                   MOVE DAGRESK-MC         TO DAGRESK-MP
                   IF  DAGRESK-MC = GMLKTO-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  GMLKTO-MC = DAGRESK-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET GMLKTO-PROCESS      TO TRUE
                   MOVE GMLKTO-MC          TO GMLKTO-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-01)
               MOVE SPACES TO NYKTO-IO-AREA
               INITIALIZE NYKTO-IO-AREA
               MOVE REC1                   TO NYKTO-IO-AREA (1:200)
               WRITE NYKTO-IO-AREA
           END-IF
           IF  (I-02)
               MOVE SPACES TO NYKTO-IO-AREA
               INITIALIZE NYKTO-IO-AREA
               MOVE REC2                   TO NYKTO-IO-AREA (1:200)
               WRITE NYKTO-IO-AREA
           END-IF
           IF  (I-03)
               MOVE SPACES TO NYKTO-IO-AREA
               INITIALIZE NYKTO-IO-AREA
               MOVE REC3                   TO NYKTO-IO-AREA (1:200)
               IF  (NOT-I-98)
                   MOVE DTOARH             TO NYKTO-IO-AREA (82:2)
               END-IF
               IF  (NOT-I-98 AND I-31)
                   MOVE PARPER             TO NYKTO-IO-AREA (84:6)
               END-IF
               IF  (NOT-I-98 AND I-30)
                   MOVE NESPER             TO NYKTO-IO-AREA (84:6)
               END-IF
               IF  (NOT-I-98 AND I-30)
                   MOVE DTOPER             TO NYKTO-IO-AREA (84:6)
               END-IF
               WRITE NYKTO-IO-AREA
           END-IF
           IF  (I-03)
               MOVE SPACES TO DAGRESU-IO-AREA
               INITIALIZE DAGRESU-IO-AREA
               MOVE REC3                   TO DAGRESU-IO-AREA (1:200)
               IF  (NOT-I-98)
                   MOVE DTOARH             TO DAGRESU-IO-AREA (82:2)
               END-IF
               IF  (NOT-I-98 AND I-31)
                   MOVE PARPER             TO DAGRESU-IO-AREA (84:6)
               END-IF
               IF  (NOT-I-98 AND I-30)
                   MOVE DTOPER             TO DAGRESU-IO-AREA (84:6)
               END-IF
               WRITE DAGRESU-IO-AREA
           END-IF.
 
       DETAIL-OVERFLOW SECTION.
       DETAIL-OVERFLOW-P.
           IF  (I-U8 AND I-LR AND I-OF)
           AND (I-97 AND I-04)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE DTODTO                 TO LISTE-IO-AREA (115:6)
               MOVE DTO8SI                 TO LISTE-IO-AREA (113:8)
               MOVE PSDS                   TO LISTE-IO-AREA (41:80)
               MOVE R                      TO LISTE-IO-AREA (113:8)
               MOVE P-IO                   TO LISTE-IO-AREA (118:3)
               MOVE S-IO                   TO LISTE-IO-AREA (116:5)
               MOVE 1                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO AVSTEMM-IO-AREA
               INITIALIZE AVSTEMM-IO-AREA
               MOVE DDATO8                 TO AVSTEMM-IO-AREA (1:8)
               MOVE '040'                  TO AVSTEMM-IO-AREA (9:3)
               MOVE 'RES'                  TO AVSTEMM-IO-AREA (12:3)
               MOVE 'RKO030'               TO AVSTEMM-IO-AREA (15:6)
               MOVE '*RKO030*'             TO AVSTEMM-IO-AREA (21:8)
               MOVE TIDSP-IO               TO AVSTEMM-IO-AREA (30:6)
               MOVE LRRSAN-IO              TO AVSTEMM-IO-AREA (36:13)
               MOVE LRRSBE-IO              TO AVSTEMM-IO-AREA (49:15)
               MOVE LRRSBU-IO              TO AVSTEMM-IO-AREA (64:15)
               MOVE DDATO8                 TO AVSTEMM-IO-AREA (79:8)
               WRITE AVSTEMM-IO-AREA
               MOVE SPACES TO AVSTEMM-IO-AREA
               INITIALIZE AVSTEMM-IO-AREA
               MOVE '********'             TO AVSTEMM-IO-AREA (1:8)
               MOVE '040'                  TO AVSTEMM-IO-AREA (9:3)
               MOVE 'RES'                  TO AVSTEMM-IO-AREA (12:3)
               MOVE '******'               TO AVSTEMM-IO-AREA (15:6)
               MOVE '*RKO030*'             TO AVSTEMM-IO-AREA (21:8)
               MOVE TIDSP-IO               TO AVSTEMM-IO-AREA (30:6)
               MOVE LRRSAN-IO              TO AVSTEMM-IO-AREA (36:13)
               MOVE LRRSBE-IO              TO AVSTEMM-IO-AREA (49:15)
               MOVE LRRSBU-IO              TO AVSTEMM-IO-AREA (64:15)
               MOVE DDATO8                 TO AVSTEMM-IO-AREA (79:8)
               WRITE AVSTEMM-IO-AREA
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'AVSTEMMING PROG. RKO030 ' TO LISTE-IO-AREA (1:24)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-BEFORE-SPACE
               MOVE 3                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANT REC NY KONTOKUR'  TO LISTE-IO-AREA (3:19)
               MOVE TOTREC                 TO XO-60YNZ
               MOVE XO-60YNZ               TO LISTE-IO-AREA (31:6)
               INITIALIZE TOTREC
               MOVE 'DATO'                 TO LISTE-IO-AREA (46:4)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (52:8)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'BELØP  NY KONTOKUR:'  TO LISTE-IO-AREA (3:19)
               MOVE TOTBEL                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (26:15)
               INITIALIZE TOTBEL
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
      *                                  21 "UTVIDET BELØPSFELT:"
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE TOTBEU                 TO XO-112YY9R
               MOVE XO-112YY9R             TO LISTE-IO-AREA (23:18)
               INITIALIZE TOTBEU
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'VALUTA NY KONTOKUR:'  TO LISTE-IO-AREA (3:19)
               MOVE TOTVAL                 TO XO-102YY9R
               MOVE XO-102YY9R             TO LISTE-IO-AREA (24:17)
               INITIALIZE TOTVAL
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
      *                                  21 "UTVIDET BELØPSFELT:"
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE TOTVAU                 TO XO-114YY9R
               MOVE XO-114YY9R             TO LISTE-IO-AREA (23:20)
               INITIALIZE TOTVAU
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'DAGENS RESKONTROTRANSER:' TO LISTE-IO-AREA (1:24)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'REGNSKAPSPERIODE FRA AUT' TO LISTE-IO-AREA (1:24)
               MOVE 'OPAR:'                TO LISTE-IO-AREA (25:5)
               MOVE PARPER                 TO LISTE-IO-AREA (31:6)
               MOVE 'NESTE PERIODE:'       TO LISTE-IO-AREA (38:14)
               MOVE NESPER                 TO LISTE-IO-AREA (53:6)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR AND I-20)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '>>>>>>>>> '           TO LISTE-IO-AREA (1:10)
               MOVE 'FEIL/BLANK REG.PER. I' TO LISTE-IO-AREA (11:21)
               MOVE ' PARAMETER <<<<<<<<<' TO LISTE-IO-AREA (32:20)
               MOVE '<=======================' TO LISTE-IO-AREA (72:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '>>>>>>>>> '           TO LISTE-IO-AREA (1:10)
               MOVE 'REGFILE OG AKKHBOK MÅ' TO LISTE-IO-AREA (11:21)
               MOVE ' RETTES    <<<<<<<<<' TO LISTE-IO-AREA (32:20)
               MOVE '<=======================' TO LISTE-IO-AREA (72:24)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-LR)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL TRANSER MED PERIO' TO LISTE-IO-AREA (1:24)
               MOVE 'DE '                  TO LISTE-IO-AREA (25:3)
               MOVE PARPER                 TO LISTE-IO-AREA (28:6)
               MOVE ':'                    TO LISTE-IO-AREA (35:1)
               MOVE ANTPER                 TO XO-60YN9R
               MOVE XO-60YN9R              TO LISTE-IO-AREA (36:7)
               MOVE 'BELØP:'               TO LISTE-IO-AREA (44:6)
               MOVE BELPER                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (51:15)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE BELPEU                 TO XO-112YY9R
               MOVE XO-112YY9R             TO LISTE-IO-AREA (48:18)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL TRANSER FREMDATER' TO LISTE-IO-AREA (1:24)
               MOVE 'T'                    TO LISTE-IO-AREA (25:1)
               MOVE ':'                    TO LISTE-IO-AREA (35:1)
               MOVE ANTNES                 TO XO-60YN9R
               MOVE XO-60YN9R              TO LISTE-IO-AREA (36:7)
               MOVE 'BELØP:'               TO LISTE-IO-AREA (44:6)
               MOVE BELNES                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (51:15)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE BELNES                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (51:15)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL TRANSER MED FEIL ' TO LISTE-IO-AREA (1:24)
               MOVE 'PERIODE   :'          TO LISTE-IO-AREA (25:11)
               MOVE ANTFEI                 TO XO-60YN9R
               MOVE XO-60YN9R              TO LISTE-IO-AREA (36:7)
               MOVE 'BELØP:'               TO LISTE-IO-AREA (44:6)
               MOVE BELFEI                 TO XO-92YY9R
               MOVE XO-92YY9R              TO LISTE-IO-AREA (51:15)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE BELFEU                 TO XO-112YY9R
               MOVE XO-112YY9R             TO LISTE-IO-AREA (48:18)
               IF  (I-13)
                   MOVE '<=======================' TO LISTE-IO-AREA
                                                               (72:24)
               END-IF
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ANTALL TRANSER TOTALT   ' TO LISTE-IO-AREA (1:24)
               MOVE ':'                    TO LISTE-IO-AREA (35:1)
               MOVE ANTTOT                 TO XO-60YN9R
               MOVE XO-60YN9R              TO LISTE-IO-AREA (36:7)
               MOVE 'BELØP:'               TO LISTE-IO-AREA (44:6)
               MOVE BELTOT                 TO EDIT-BELTOT
               MOVE EDIT-BELTOT            TO LISTE-IO-AREA (51:15)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE BELTOU                 TO XO-112YY9R
               MOVE XO-112YY9R             TO LISTE-IO-AREA (48:18)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE '************************' TO LISTE-IO-AREA (49:24)
               MOVE 2                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '***** OBS! SANERING MÅ H' TO LISTE-IO-AREA (1:24)
               MOVE 'ENSYNTAS I DAGLIG AVSTEM' TO LISTE-IO-AREA (25:24)
               MOVE 'MING (JFR RSK003)  *****' TO LISTE-IO-AREA (49:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (1:24)
               MOVE '************************' TO LISTE-IO-AREA (25:24)
               MOVE '************************' TO LISTE-IO-AREA (49:24)
      * DUMMY-LINJE FOR Å FJERNE KOMPILERINGSFEIL
               MOVE 1                      TO LISTE-AFTER-SPACE
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
           MOVE 2                          TO LR-CHECK
           INITIALIZE GMLKTO-DATA-FIELDS
           SET GMLKTO-EOF-OFF              TO TRUE
           SET GMLKTO-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO GMLKTO-MC
                                              GMLKTO-MP
           OPEN INPUT GMLKTO
           INITIALIZE DAGRESK-DATA-FIELDS
           SET DAGRESK-EOF-OFF             TO TRUE
           SET DAGRESK-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO DAGRESK-MC
                                              DAGRESK-MP
           OPEN INPUT DAGRESK
           INITIALIZE AUTOPAR-DATA-FIELDS
           OPEN INPUT AUTOPAR
           OPEN OUTPUT NYKTO
           OPEN OUTPUT DAGRESU
           OPEN OUTPUT AVSTEMM
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE GMLKTO
           CLOSE DAGRESK
           CLOSE AUTOPAR
           CLOSE NYKTO
           CLOSE DAGRESU
           CLOSE AVSTEMM
           IF LISTE-IO-AREA NOT = SPACES
             WRITE LISTE-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE-IO-AREA
           END-IF
           CLOSE LISTE.
 
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
