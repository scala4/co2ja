       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAK083R.
      **********************************************  Z-WIN-RPG2   ****
      * KOPI AV FAK.FAK078
      * BRUKES I STA.XSTA963F
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: FAK083.rpg
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
           SELECT OUTFIL
               ASSIGN TO UT-S-OUTFIL
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS OUTFIL-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD INNFIL
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  INNFIL-IO-AREA.
           05  INNFIL-IO-AREA-X            PICTURE X(200).
      *UTFIL  O   F     300            ESDS
       FD OUTFIL
               BLOCK CONTAINS 1000
               RECORD CONTAINS 500.
       01  OUTFIL-IO-AREA.
           05  OUTFIL-IO-AREA-X            PICTURE X(500).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  INNFIL-STATUS               PICTURE 99 VALUE 0.
           10  OUTFIL-STATUS               PICTURE 99 VALUE 0.
 
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
           05  FILLER                      PIC X VALUE '1'.
               88  INNFIL-LEVEL-INIT-OFF   VALUE '0'.
               88  INNFIL-LEVEL-INIT       VALUE '1'.
           05  INNFIL-LEVEL-01.
               10  INNFIL-01-L3.
                   15  INNFIL-01-L3-FIRMA  PICTURE X(3).
               10  INNFIL-01-L2.
                   15  INNFIL-01-L2-KUNDNR PICTURE X(6).
               10  INNFIL-01-L1.
                   15  INNFIL-01-L1-VGR    PICTURE X(5).
           05  INNFIL-DATA-FIELDS.
               10  REC                     PICTURE X(150).
               10  FIRMA                   PICTURE X(3).
               10  KUNDNR                  PICTURE X(6).
               10  VGR                     PICTURE X(5).
               10  BM                      PICTURE X(2).
               10  FK                      PICTURE X(1).
               10  KRTYPE                  PICTURE X(1).
               10  NTOSUM-IO.
                   15  NTOSUM              PICTURE S9(7)V9(2).
               10  FAKMND                  PICTURE X(6).
               10  FAAR-IO.
                   15  FAAR                PICTURE S9(2).
               10  FMND-IO.
                   15  FMND                PICTURE S9(2).
               10  OAAR-IO.
                   15  OAAR                PICTURE S9(2).
               10  OMND-IO.
                   15  OMND                PICTURE S9(2).
               10  ODAG-IO.
                   15  ODAG                PICTURE S9(2).
               10  FAKOMG                  PICTURE X(1).
               10  ANTLEV-IO.
                   15  ANTLEV              PICTURE S9(5)V9(2).
               10  SELVK-IO.
                   15  SELVK               PICTURE S9(7)V9(2).
               10  NAVN                    PICTURE X(30).
               10  HND-IO.
                   15  HND                 PICTURE S9(3).
               10  KAT-IO.
                   15  KAT                 PICTURE S9(3).
               10  VGRNVN                  PICTURE X(30).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(6).
               10  THE-PRIOR-L1            PICTURE X(5).
           05  TEMPORARY-FIELDS.
               10  BELDMF-IO.
                   15  BELDMF              PICTURE S9(8).
               10  ANTAKF-IO.
                   15  ANTAKF              PICTURE S9(8).
               10  ANTAK-IO.
                   15  ANTAK               PICTURE S9(8).
               10  BELAK-IO.
                   15  BELAK               PICTURE S9(8).
               10  BELAKF-IO.
                   15  BELAKF              PICTURE S9(8).
               10  BELFJO-IO.
                   15  BELFJO              PICTURE S9(8).
               10  BJANFJ-IO.
                   15  BJANFJ              PICTURE S9(8).
               10  BFEBFJ-IO.
                   15  BFEBFJ              PICTURE S9(8).
               10  BMARFJ-IO.
                   15  BMARFJ              PICTURE S9(8).
               10  BAPRFJ-IO.
                   15  BAPRFJ              PICTURE S9(8).
               10  BMAIFJ-IO.
                   15  BMAIFJ              PICTURE S9(8).
               10  BJUNFJ-IO.
                   15  BJUNFJ              PICTURE S9(8).
               10  BJULFJ-IO.
                   15  BJULFJ              PICTURE S9(8).
               10  BAUGFJ-IO.
                   15  BAUGFJ              PICTURE S9(8).
               10  BSEPFJ-IO.
                   15  BSEPFJ              PICTURE S9(8).
               10  BOKTFJ-IO.
                   15  BOKTFJ              PICTURE S9(8).
               10  BNOVFJ-IO.
                   15  BNOVFJ              PICTURE S9(8).
               10  BDESFJ-IO.
                   15  BDESFJ              PICTURE S9(8).
               10  BJANAR-IO.
                   15  BJANAR              PICTURE S9(8).
               10  BFEBAR-IO.
                   15  BFEBAR              PICTURE S9(8).
               10  BMARAR-IO.
                   15  BMARAR              PICTURE S9(8).
               10  BAPRAR-IO.
                   15  BAPRAR              PICTURE S9(8).
               10  BMAIAR-IO.
                   15  BMAIAR              PICTURE S9(8).
               10  BJUNAR-IO.
                   15  BJUNAR              PICTURE S9(8).
               10  BJULAR-IO.
                   15  BJULAR              PICTURE S9(8).
               10  BAUGAR-IO.
                   15  BAUGAR              PICTURE S9(8).
               10  BSEPAR-IO.
                   15  BSEPAR              PICTURE S9(8).
               10  BOKTAR-IO.
                   15  BOKTAR              PICTURE S9(8).
               10  BNOVAR-IO.
                   15  BNOVAR              PICTURE S9(8).
               10  BDESAR-IO.
                   15  BDESAR              PICTURE S9(8).
               10  MND                     PICTURE X(2).
               10  AAR                     PICTURE X(2).
               10  DAG                     PICTURE X(2).
               10  AARN-IO.
                   15  AARN                PICTURE S9(2).
               10  MNDN-IO.
                   15  MNDN                PICTURE S9(2).
               10  DAGN-IO.
                   15  DAGN                PICTURE S9(2).
               10  FJORN-IO.
                   15  FJORN               PICTURE S9(2).
           05  EDITTING-FIELDS.
               10  EDIT-ANTAK              PICTURE ZZZZZ.ZZ9.
               10  EDIT-ANTAKF             PICTURE ZZZZZ.ZZ9.
               10  EDIT-BELDMF             PICTURE ZZZZZ.ZZ9.
               10  EDIT-BELAK              PICTURE ZZZZZ.ZZ9.
               10  EDIT-BELAKF             PICTURE ZZZZZ.ZZ9.
               10  EDIT-BELFJO             PICTURE ZZZZZ.ZZ9.
               10  EDIT-BJANFJ             PICTURE ZZZZZ.ZZ9.
               10  EDIT-BFEBFJ             PICTURE ZZZZZ.ZZ9.
               10  EDIT-BMARFJ             PICTURE ZZZZZ.ZZ9.
               10  EDIT-BAPRFJ             PICTURE ZZZZZ.ZZ9.
               10  EDIT-BMAIFJ             PICTURE ZZZZZ.ZZ9.
               10  EDIT-BJUNFJ             PICTURE ZZZZZ.ZZ9.
               10  EDIT-BJULFJ             PICTURE ZZZZZ.ZZ9.
               10  EDIT-BAUGFJ             PICTURE ZZZZZ.ZZ9.
               10  EDIT-BSEPFJ             PICTURE ZZZZZ.ZZ9.
               10  EDIT-BOKTFJ             PICTURE ZZZZZ.ZZ9.
               10  EDIT-BNOVFJ             PICTURE ZZZZZ.ZZ9.
               10  EDIT-BDESFJ             PICTURE ZZZZZ.ZZ9.
               10  EDIT-BJANAR             PICTURE ZZZZZ.ZZ9.
               10  EDIT-BFEBAR             PICTURE ZZZZZ.ZZ9.
               10  EDIT-BMARAR             PICTURE ZZZZZ.ZZ9.
               10  EDIT-BAPRAR             PICTURE ZZZZZ.ZZ9.
               10  EDIT-BMAIAR             PICTURE ZZZZZ.ZZ9.
               10  EDIT-BJUNAR             PICTURE ZZZZZ.ZZ9.
               10  EDIT-BJULAR             PICTURE ZZZZZ.ZZ9.
               10  EDIT-BAUGAR             PICTURE ZZZZZ.ZZ9.
               10  EDIT-BSEPAR             PICTURE ZZZZZ.ZZ9.
               10  EDIT-BOKTAR             PICTURE ZZZZZ.ZZ9.
               10  EDIT-BNOVAR             PICTURE ZZZZZ.ZZ9.
               10  EDIT-BDESAR             PICTURE ZZZZZ.ZZ9.
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
 
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-01                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
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
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  INNFIL-PROCESS
               PERFORM INNFIL-IDSET
           END-IF
 
           IF  INNFIL-PROCESS
               PERFORM INNFIL-CHK-LEVEL
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
 
           IF  INNFIL-PROCESS
               PERFORM INNFIL-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  INNFIL-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           SET NOT-I-50                    TO TRUE
           SET NOT-I-11                    TO TRUE
           SET NOT-I-21                    TO TRUE
           SET NOT-I-36                    TO TRUE
           SET NOT-I-37                    TO TRUE
           SET NOT-I-39                    TO TRUE
           SET NOT-I-40                    TO TRUE
           SET NOT-I-41                    TO TRUE
           SET NOT-I-27                    TO TRUE
           SET NOT-I-28                    TO TRUE
           IF  (I-L1)
               MOVE 0                      TO BELDMF
               MOVE 0                      TO ANTAKF
               MOVE 0                      TO ANTAK
               MOVE 0                      TO BELAK
               MOVE 0                      TO BELAKF
               MOVE 0                      TO BELFJO
               MOVE 0                      TO BJANFJ
               MOVE 0                      TO BFEBFJ
               MOVE 0                      TO BMARFJ
               MOVE 0                      TO BAPRFJ
               MOVE 0                      TO BMAIFJ
               MOVE 0                      TO BJUNFJ
               MOVE 0                      TO BJULFJ
               MOVE 0                      TO BAUGFJ
               MOVE 0                      TO BSEPFJ
               MOVE 0                      TO BOKTFJ
               MOVE 0                      TO BNOVFJ
               MOVE 0                      TO BDESFJ
               MOVE 0                      TO BJANAR
               MOVE 0                      TO BFEBAR
               MOVE 0                      TO BMARAR
               MOVE 0                      TO BAPRAR
               MOVE 0                      TO BMAIAR
               MOVE 0                      TO BJUNAR
               MOVE 0                      TO BJULAR
               MOVE 0                      TO BAUGAR
               MOVE 0                      TO BSEPAR
               MOVE 0                      TO BOKTAR
               MOVE 0                      TO BNOVAR
               MOVE 0                      TO BDESAR
      *****************************************************************
      * DIVERSE SJEKKER OG UTREGNINGER                                *
      *****************************************************************
           END-IF
           MOVE UMONTH                     TO MND
           MOVE UYEAR                      TO AAR
           MOVE UDAY                       TO DAG
           MOVE AAR                        TO AARN-IO
           MOVE MND                        TO MNDN-IO
           MOVE DAG                        TO DAGN-IO
           SUBTRACT 1 FROM AARN        GIVING FJORN
           SET NOT-I-21                    TO TRUE
           IF  FAAR = FJORN
               SET I-21                    TO TRUE
           END-IF
           SET NOT-I-23                    TO TRUE
           IF  FAAR = AARN
               SET I-23                    TO TRUE
           END-IF
      * N21                GOTO SLUTT
           SET NOT-I-22                    TO TRUE
           IF  FMND = MNDN
               SET I-22                    TO TRUE
           END-IF
           SET NOT-I-25                    TO TRUE
           IF  FMND < MNDN
               SET I-25                    TO TRUE
           END-IF
           SET NOT-I-26                    TO TRUE
           IF  FMND < MNDN
               SET I-26                    TO TRUE
           END-IF
      *
           SET NOT-I-61                    TO TRUE
           IF  FMND = 01
               SET I-61                    TO TRUE
           END-IF
           SET NOT-I-62                    TO TRUE
           IF  FMND = 02
               SET I-62                    TO TRUE
           END-IF
           SET NOT-I-63                    TO TRUE
           IF  FMND = 03
               SET I-63                    TO TRUE
           END-IF
           SET NOT-I-64                    TO TRUE
           IF  FMND = 04
               SET I-64                    TO TRUE
           END-IF
           SET NOT-I-65                    TO TRUE
           IF  FMND = 05
               SET I-65                    TO TRUE
           END-IF
           SET NOT-I-66                    TO TRUE
           IF  FMND = 06
               SET I-66                    TO TRUE
           END-IF
           SET NOT-I-67                    TO TRUE
           IF  FMND = 07
               SET I-67                    TO TRUE
           END-IF
           SET NOT-I-68                    TO TRUE
           IF  FMND = 08
               SET I-68                    TO TRUE
           END-IF
           SET NOT-I-69                    TO TRUE
           IF  FMND = 09
               SET I-69                    TO TRUE
           END-IF
           SET NOT-I-70                    TO TRUE
           IF  FMND = 10
               SET I-70                    TO TRUE
           END-IF
           SET NOT-I-71                    TO TRUE
           IF  FMND = 11
               SET I-71                    TO TRUE
           END-IF
           SET NOT-I-72                    TO TRUE
           IF  FMND = 12
               SET I-72                    TO TRUE
           END-IF
      *
           IF  (I-21)
               SET NOT-I-27                TO TRUE
               IF  OMND = MNDN
                   SET I-27                TO TRUE
               END-IF
           END-IF
           IF  (I-21 AND I-27)
               SET NOT-I-28                TO TRUE
               IF  ODAG NOT > DAGN
                   SET I-28                TO TRUE
               END-IF
      * N22                GOTO SLUTT
      *
           END-IF
           SET NOT-I-31                    TO TRUE
           IF  FK = 'F'
               SET I-31                    TO TRUE
           END-IF
           SET NOT-I-32                    TO TRUE
           IF  FK = 'K'
               SET I-32                    TO TRUE
           END-IF
           IF  (I-32)
               SET NOT-I-33                TO TRUE
               IF  KRTYPE = '2'
                   SET I-33                TO TRUE
               END-IF
      *
      * REGNER UT DIV BELØP
      *
      *          BELDMF    ADD  NTOSUM    BELDMF  80
           END-IF
           IF  (I-21 AND I-26)
               OR  (I-21 AND I-28)
               ADD NTOSUM                  TO BELAKF
           END-IF
           IF  (I-23 AND I-25)
               ADD NTOSUM                  TO BELAK
           END-IF
           IF  (I-21 AND I-28)
               ADD NTOSUM                  TO BELDMF
      *  32      BELDMF    SUB  NTOSUM    BELDMF  80
           END-IF
           IF  (I-21)
               ADD NTOSUM                  TO BELFJO
      *
           END-IF
           IF  (I-21 AND I-61)
               ADD NTOSUM                  TO BJANFJ
           END-IF
           IF  (I-21 AND I-62)
               ADD NTOSUM                  TO BFEBFJ
           END-IF
           IF  (I-21 AND I-63)
               ADD NTOSUM                  TO BMARFJ
           END-IF
           IF  (I-21 AND I-64)
               ADD NTOSUM                  TO BAPRFJ
           END-IF
           IF  (I-21 AND I-65)
               ADD NTOSUM                  TO BMAIFJ
           END-IF
           IF  (I-21 AND I-66)
               ADD NTOSUM                  TO BJUNFJ
           END-IF
           IF  (I-21 AND I-67)
               ADD NTOSUM                  TO BJULFJ
           END-IF
           IF  (I-21 AND I-68)
               ADD NTOSUM                  TO BAUGFJ
           END-IF
           IF  (I-21 AND I-69)
               ADD NTOSUM                  TO BSEPFJ
           END-IF
           IF  (I-21 AND I-70)
               ADD NTOSUM                  TO BOKTFJ
           END-IF
           IF  (I-21 AND I-71)
               ADD NTOSUM                  TO BNOVFJ
           END-IF
           IF  (I-21 AND I-72)
               ADD NTOSUM                  TO BDESFJ
      *
           END-IF
           IF  (I-23 AND I-61)
               ADD NTOSUM                  TO BJANAR
           END-IF
           IF  (I-23 AND I-62)
               ADD NTOSUM                  TO BFEBAR
           END-IF
           IF  (I-23 AND I-63)
               ADD NTOSUM                  TO BMARAR
           END-IF
           IF  (I-23 AND I-64)
               ADD NTOSUM                  TO BAPRAR
           END-IF
           IF  (I-23 AND I-65)
               ADD NTOSUM                  TO BMAIAR
           END-IF
           IF  (I-23 AND I-66)
               ADD NTOSUM                  TO BJUNAR
           END-IF
           IF  (I-23 AND I-67)
               ADD NTOSUM                  TO BJULAR
           END-IF
           IF  (I-23 AND I-68)
               ADD NTOSUM                  TO BAUGAR
           END-IF
           IF  (I-23 AND I-69)
               ADD NTOSUM                  TO BSEPAR
           END-IF
           IF  (I-23 AND I-70)
               ADD NTOSUM                  TO BOKTAR
           END-IF
           IF  (I-23 AND I-71)
               ADD NTOSUM                  TO BNOVAR
           END-IF
           IF  (I-23 AND I-72)
               ADD NTOSUM                  TO BDESAR
      *
      * REGNER UT DIV ANTALL
      *
           END-IF
           IF  (I-31 AND I-21 AND I-26)
               OR  (I-31 AND I-21 AND I-28)
               ADD ANTLEV                  TO ANTAKF
           END-IF
           IF  (I-33 AND I-21 AND I-26)
               AND (I-32)
               SUBTRACT ANTLEV             FROM ANTAKF
           END-IF
           IF  (I-33 AND I-21 AND I-28)
               AND (I-32)
               SUBTRACT ANTLEV             FROM ANTAKF
           END-IF
           IF  (I-31 AND I-23 AND I-25)
               ADD ANTLEV                  TO ANTAK
           END-IF
           IF  (I-33 AND I-23 AND I-25)
               AND (I-32)
               SUBTRACT ANTLEV             FROM ANTAK
           END-IF
           SET I-50                        TO TRUE.
 
       SLUTT-T.
      *****************************************************************
      * DIVERSE SJEKKER                                               *
      *****************************************************************
           CONTINUE.
 
       TOTAL-CALCS SECTION.
       TOTAL-CALCS-P.
           IF  (I-L1)
               SET NOT-I-39                TO TRUE
               IF  BELDMF < 0
                   SET I-39                TO TRUE
               END-IF
               SET NOT-I-40                TO TRUE
               IF  BELAK < 0
                   SET I-40                TO TRUE
               END-IF
               SET NOT-I-41                TO TRUE
               IF  BELAKF < 0
                   SET I-41                TO TRUE
               END-IF
               SET NOT-I-37                TO TRUE
               IF  ANTAKF < 0
                   SET I-37                TO TRUE
               END-IF
               SET NOT-I-36                TO TRUE
               IF  ANTAK < 0
                   SET I-36                TO TRUE
               END-IF
               SET NOT-I-42                TO TRUE
               IF  BELFJO < 0
                   SET I-42                TO TRUE
               END-IF
               SET NOT-I-73                TO TRUE
               IF  BJANFJ < 0
                   SET I-73                TO TRUE
               END-IF
               SET NOT-I-74                TO TRUE
               IF  BFEBFJ < 0
                   SET I-74                TO TRUE
               END-IF
               SET NOT-I-75                TO TRUE
               IF  BMARFJ < 0
                   SET I-75                TO TRUE
               END-IF
               SET NOT-I-76                TO TRUE
               IF  BAPRFJ < 0
                   SET I-76                TO TRUE
               END-IF
               SET NOT-I-77                TO TRUE
               IF  BMAIFJ < 0
                   SET I-77                TO TRUE
               END-IF
               SET NOT-I-78                TO TRUE
               IF  BJUNFJ < 0
                   SET I-78                TO TRUE
               END-IF
               SET NOT-I-79                TO TRUE
               IF  BJULFJ < 0
                   SET I-79                TO TRUE
               END-IF
               SET NOT-I-80                TO TRUE
               IF  BAUGFJ < 0
                   SET I-80                TO TRUE
               END-IF
               SET NOT-I-81                TO TRUE
               IF  BSEPFJ < 0
                   SET I-81                TO TRUE
               END-IF
               SET NOT-I-82                TO TRUE
               IF  BOKTFJ < 0
                   SET I-82                TO TRUE
               END-IF
               SET NOT-I-83                TO TRUE
               IF  BNOVFJ < 0
                   SET I-83                TO TRUE
               END-IF
               SET NOT-I-84                TO TRUE
               IF  BDESFJ < 0
                   SET I-84                TO TRUE
               END-IF
               SET NOT-I-85                TO TRUE
               IF  BJANAR < 0
                   SET I-85                TO TRUE
               END-IF
               SET NOT-I-86                TO TRUE
               IF  BFEBAR < 0
                   SET I-86                TO TRUE
               END-IF
               SET NOT-I-87                TO TRUE
               IF  BMARAR < 0
                   SET I-87                TO TRUE
               END-IF
               SET NOT-I-88                TO TRUE
               IF  BAPRAR < 0
                   SET I-88                TO TRUE
               END-IF
               SET NOT-I-89                TO TRUE
               IF  BMAIAR < 0
                   SET I-89                TO TRUE
               END-IF
               SET NOT-I-90                TO TRUE
               IF  BJUNAR < 0
                   SET I-90                TO TRUE
               END-IF
               SET NOT-I-91                TO TRUE
               IF  BJULAR < 0
                   SET I-91                TO TRUE
               END-IF
               SET NOT-I-92                TO TRUE
               IF  BAUGAR < 0
                   SET I-92                TO TRUE
               END-IF
               SET NOT-I-93                TO TRUE
               IF  BSEPAR < 0
                   SET I-93                TO TRUE
               END-IF
               SET NOT-I-94                TO TRUE
               IF  BOKTAR < 0
                   SET I-94                TO TRUE
               END-IF
               SET NOT-I-95                TO TRUE
               IF  BNOVAR < 0
                   SET I-95                TO TRUE
               END-IF
               SET NOT-I-96                TO TRUE
               IF  BDESAR < 0
                   SET I-96                TO TRUE
               END-IF
           END-IF.
 
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
               MOVE INNFIL-IO-AREA (1:150) TO REC (1:150)
               MOVE INNFIL-IO-AREA (1:3)   TO FIRMA (1:3)
               MOVE INNFIL-IO-AREA (4:6)   TO KUNDNR (1:6)
               MOVE INNFIL-IO-AREA (22:5)  TO VGR (1:5)
               MOVE INNFIL-IO-AREA (199:2) TO BM (1:2)
               MOVE INNFIL-IO-AREA (59:1)  TO FK (1:1)
               MOVE INNFIL-IO-AREA (60:1)  TO KRTYPE (1:1)
               MOVE INNFIL-IO-AREA (61:9)  TO NTOSUM-IO
               INSPECT NTOSUM-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (70:6)  TO FAKMND (1:6)
               MOVE INNFIL-IO-AREA (72:2)  TO FAAR-IO
               INSPECT FAAR-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (74:2)  TO FMND-IO
               INSPECT FMND-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (78:2)  TO OAAR-IO
               INSPECT OAAR-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (80:2)  TO OMND-IO
               INSPECT OMND-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (82:2)  TO ODAG-IO
               INSPECT ODAG-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (100:1) TO FAKOMG (1:1)
               MOVE INNFIL-IO-AREA (84:7)  TO ANTLEV-IO
               INSPECT ANTLEV-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (91:9)  TO SELVK-IO
               INSPECT SELVK-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (101:30) TO NAVN (1:30)
               MOVE INNFIL-IO-AREA (131:3) TO HND-IO
               INSPECT HND-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (140:3) TO KAT-IO
               INSPECT KAT-IO REPLACING ALL ' ' BY '0'
               MOVE INNFIL-IO-AREA (146:30) TO VGRNVN (1:30)
           END-EVALUATE.
 
       INNFIL-IDSET SECTION.
       INNFIL-IDSET-P.
           SET I-01                        TO TRUE.
 
       INNFIL-CHK-LEVEL SECTION.
       INNFIL-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO INNFIL-LEVEL-01
               MOVE INNFIL-IO-AREA (1:3)   TO INNFIL-01-L3-FIRMA
               MOVE INNFIL-IO-AREA (4:6)   TO INNFIL-01-L2-KUNDNR
               MOVE INNFIL-IO-AREA (22:5)  TO INNFIL-01-L1-VGR
               IF  INNFIL-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INNFIL-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  INNFIL-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INNFIL-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INNFIL-01-L3          TO THE-PRIOR-L3
               MOVE  INNFIL-01-L2          TO THE-PRIOR-L2
               MOVE  INNFIL-01-L1          TO THE-PRIOR-L1
               SET INNFIL-LEVEL-INIT       TO TRUE
           END-EVALUATE.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1)
               MOVE SPACES TO OUTFIL-IO-AREA
               INITIALIZE OUTFIL-IO-AREA
               MOVE KAT-IO                 TO OUTFIL-IO-AREA (1:3)
               MOVE ';'                    TO OUTFIL-IO-AREA (4:1)
               MOVE HND-IO                 TO OUTFIL-IO-AREA (5:3)
               MOVE ';'                    TO OUTFIL-IO-AREA (8:1)
               MOVE KUNDNR                 TO OUTFIL-IO-AREA (9:6)
               MOVE ';'                    TO OUTFIL-IO-AREA (15:1)
               MOVE VGR                    TO OUTFIL-IO-AREA (16:5)
               MOVE ';'                    TO OUTFIL-IO-AREA (21:1)
               MOVE VGRNVN                 TO OUTFIL-IO-AREA (22:30)
               MOVE ';'                    TO OUTFIL-IO-AREA (52:1)
      *                      35          53 "-"
      *                        ANTDM  B  63 "     . 0 "
               MOVE ';'                    TO OUTFIL-IO-AREA (64:1)
               IF  (I-36)
                   MOVE '-'                TO OUTFIL-IO-AREA (65:1)
               END-IF
               MOVE ANTAK                  TO EDIT-ANTAK
               MOVE EDIT-ANTAK             TO OUTFIL-IO-AREA (68:9)
               INITIALIZE ANTAK
               MOVE ';'                    TO OUTFIL-IO-AREA (77:1)
               IF  (I-37)
                   MOVE '-'                TO OUTFIL-IO-AREA (78:1)
               END-IF
               MOVE ANTAKF                 TO EDIT-ANTAKF
               MOVE EDIT-ANTAKF            TO OUTFIL-IO-AREA (81:9)
               MOVE ';'                    TO OUTFIL-IO-AREA (90:1)
      *                      38          91 "-"
      *                        BELDM  B 104 "     . 0 "
               MOVE ';'                    TO OUTFIL-IO-AREA (105:1)
               IF  (I-39)
                   MOVE '-'                TO OUTFIL-IO-AREA (106:1)
               END-IF
               MOVE BELDMF                 TO EDIT-BELDMF
               MOVE EDIT-BELDMF            TO OUTFIL-IO-AREA (111:9)
               MOVE ';'                    TO OUTFIL-IO-AREA (120:1)
               IF  (I-40)
                   MOVE '-'                TO OUTFIL-IO-AREA (121:1)
               END-IF
               MOVE BELAK                  TO EDIT-BELAK
               MOVE EDIT-BELAK             TO OUTFIL-IO-AREA (127:9)
               INITIALIZE BELAK
               MOVE ';'                    TO OUTFIL-IO-AREA (136:1)
               IF  (I-41)
                   MOVE '-'                TO OUTFIL-IO-AREA (137:1)
               END-IF
               MOVE BELAKF                 TO EDIT-BELAKF
               MOVE EDIT-BELAKF            TO OUTFIL-IO-AREA (142:9)
               INITIALIZE BELAKF
               MOVE ';'                    TO OUTFIL-IO-AREA (151:1)
      *                      42         152 "-"
      *                        BELAV  B 165 "     . 0 "
               MOVE ';'                    TO OUTFIL-IO-AREA (166:1)
               MOVE NAVN                   TO OUTFIL-IO-AREA (167:30)
               MOVE ';'                    TO OUTFIL-IO-AREA (197:1)
               MOVE FIRMA                  TO OUTFIL-IO-AREA (198:3)
               MOVE '_'                    TO OUTFIL-IO-AREA (201:1)
               MOVE KUNDNR                 TO OUTFIL-IO-AREA (202:6)
               MOVE ';'                    TO OUTFIL-IO-AREA (208:1)
               IF  (I-42)
                   MOVE '-'                TO OUTFIL-IO-AREA (209:1)
               END-IF
               MOVE BELFJO                 TO EDIT-BELFJO
               MOVE EDIT-BELFJO            TO OUTFIL-IO-AREA (210:9)
               MOVE ';'                    TO OUTFIL-IO-AREA (219:1)
               IF  (I-73)
                   MOVE '-'                TO OUTFIL-IO-AREA (220:1)
               END-IF
               MOVE BJANFJ                 TO EDIT-BJANFJ
               MOVE EDIT-BJANFJ            TO OUTFIL-IO-AREA (221:9)
               MOVE ';'                    TO OUTFIL-IO-AREA (230:1)
               IF  (I-74)
                   MOVE '-'                TO OUTFIL-IO-AREA (231:1)
               END-IF
               MOVE BFEBFJ                 TO EDIT-BFEBFJ
               MOVE EDIT-BFEBFJ            TO OUTFIL-IO-AREA (232:9)
               MOVE ';'                    TO OUTFIL-IO-AREA (241:1)
               IF  (I-75)
                   MOVE '-'                TO OUTFIL-IO-AREA (242:1)
               END-IF
               MOVE BMARFJ                 TO EDIT-BMARFJ
               MOVE EDIT-BMARFJ            TO OUTFIL-IO-AREA (243:9)
               MOVE ';'                    TO OUTFIL-IO-AREA (252:1)
               IF  (I-76)
                   MOVE '-'                TO OUTFIL-IO-AREA (253:1)
               END-IF
               MOVE BAPRFJ                 TO EDIT-BAPRFJ
               MOVE EDIT-BAPRFJ            TO OUTFIL-IO-AREA (254:9)
               MOVE ';'                    TO OUTFIL-IO-AREA (263:1)
               IF  (I-77)
                   MOVE '-'                TO OUTFIL-IO-AREA (264:1)
               END-IF
               MOVE BMAIFJ                 TO EDIT-BMAIFJ
               MOVE EDIT-BMAIFJ            TO OUTFIL-IO-AREA (265:9)
               MOVE ';'                    TO OUTFIL-IO-AREA (274:1)
               IF  (I-78)
                   MOVE '-'                TO OUTFIL-IO-AREA (275:1)
               END-IF
               MOVE BJUNFJ                 TO EDIT-BJUNFJ
               MOVE EDIT-BJUNFJ            TO OUTFIL-IO-AREA (276:9)
               MOVE ';'                    TO OUTFIL-IO-AREA (285:1)
               IF  (I-79)
                   MOVE '-'                TO OUTFIL-IO-AREA (286:1)
               END-IF
               MOVE BJULFJ                 TO EDIT-BJULFJ
               MOVE EDIT-BJULFJ            TO OUTFIL-IO-AREA (287:9)
               MOVE ';'                    TO OUTFIL-IO-AREA (296:1)
               IF  (I-80)
                   MOVE '-'                TO OUTFIL-IO-AREA (297:1)
               END-IF
               MOVE BAUGFJ                 TO EDIT-BAUGFJ
               MOVE EDIT-BAUGFJ            TO OUTFIL-IO-AREA (298:9)
               MOVE ';'                    TO OUTFIL-IO-AREA (307:1)
               IF  (I-81)
                   MOVE '-'                TO OUTFIL-IO-AREA (308:1)
               END-IF
               MOVE BSEPFJ                 TO EDIT-BSEPFJ
               MOVE EDIT-BSEPFJ            TO OUTFIL-IO-AREA (309:9)
               MOVE ';'                    TO OUTFIL-IO-AREA (318:1)
               IF  (I-82)
                   MOVE '-'                TO OUTFIL-IO-AREA (319:1)
               END-IF
               MOVE BOKTFJ                 TO EDIT-BOKTFJ
               MOVE EDIT-BOKTFJ            TO OUTFIL-IO-AREA (320:9)
               MOVE ';'                    TO OUTFIL-IO-AREA (329:1)
               IF  (I-83)
                   MOVE '-'                TO OUTFIL-IO-AREA (330:1)
               END-IF
               MOVE BNOVFJ                 TO EDIT-BNOVFJ
               MOVE EDIT-BNOVFJ            TO OUTFIL-IO-AREA (331:9)
               MOVE ';'                    TO OUTFIL-IO-AREA (340:1)
               IF  (I-84)
                   MOVE '-'                TO OUTFIL-IO-AREA (341:1)
               END-IF
               MOVE BDESFJ                 TO EDIT-BDESFJ
               MOVE EDIT-BDESFJ            TO OUTFIL-IO-AREA (342:9)
               MOVE ';'                    TO OUTFIL-IO-AREA (351:1)
               IF  (I-85)
                   MOVE '-'                TO OUTFIL-IO-AREA (352:1)
               END-IF
               MOVE BJANAR                 TO EDIT-BJANAR
               MOVE EDIT-BJANAR            TO OUTFIL-IO-AREA (353:9)
               MOVE ';'                    TO OUTFIL-IO-AREA (362:1)
               IF  (I-86)
                   MOVE '-'                TO OUTFIL-IO-AREA (363:1)
               END-IF
               MOVE BFEBAR                 TO EDIT-BFEBAR
               MOVE EDIT-BFEBAR            TO OUTFIL-IO-AREA (364:9)
               MOVE ';'                    TO OUTFIL-IO-AREA (373:1)
               IF  (I-87)
                   MOVE '-'                TO OUTFIL-IO-AREA (374:1)
               END-IF
               MOVE BMARAR                 TO EDIT-BMARAR
               MOVE EDIT-BMARAR            TO OUTFIL-IO-AREA (375:9)
               MOVE ';'                    TO OUTFIL-IO-AREA (384:1)
               IF  (I-88)
                   MOVE '-'                TO OUTFIL-IO-AREA (385:1)
               END-IF
               MOVE BAPRAR                 TO EDIT-BAPRAR
               MOVE EDIT-BAPRAR            TO OUTFIL-IO-AREA (386:9)
               MOVE ';'                    TO OUTFIL-IO-AREA (395:1)
               IF  (I-89)
                   MOVE '-'                TO OUTFIL-IO-AREA (396:1)
               END-IF
               MOVE BMAIAR                 TO EDIT-BMAIAR
               MOVE EDIT-BMAIAR            TO OUTFIL-IO-AREA (397:9)
               MOVE ';'                    TO OUTFIL-IO-AREA (406:1)
               IF  (I-90)
                   MOVE '-'                TO OUTFIL-IO-AREA (407:1)
               END-IF
               MOVE BJUNAR                 TO EDIT-BJUNAR
               MOVE EDIT-BJUNAR            TO OUTFIL-IO-AREA (408:9)
               MOVE ';'                    TO OUTFIL-IO-AREA (417:1)
               IF  (I-91)
                   MOVE '-'                TO OUTFIL-IO-AREA (418:1)
               END-IF
               MOVE BJULAR                 TO EDIT-BJULAR
               MOVE EDIT-BJULAR            TO OUTFIL-IO-AREA (419:9)
               MOVE ';'                    TO OUTFIL-IO-AREA (428:1)
               IF  (I-92)
                   MOVE '-'                TO OUTFIL-IO-AREA (429:1)
               END-IF
               MOVE BAUGAR                 TO EDIT-BAUGAR
               MOVE EDIT-BAUGAR            TO OUTFIL-IO-AREA (430:9)
               MOVE ';'                    TO OUTFIL-IO-AREA (439:1)
               IF  (I-93)
                   MOVE '-'                TO OUTFIL-IO-AREA (440:1)
               END-IF
               MOVE BSEPAR                 TO EDIT-BSEPAR
               MOVE EDIT-BSEPAR            TO OUTFIL-IO-AREA (441:9)
               MOVE ';'                    TO OUTFIL-IO-AREA (450:1)
               IF  (I-94)
                   MOVE '-'                TO OUTFIL-IO-AREA (451:1)
               END-IF
               MOVE BOKTAR                 TO EDIT-BOKTAR
               MOVE EDIT-BOKTAR            TO OUTFIL-IO-AREA (452:9)
               MOVE ';'                    TO OUTFIL-IO-AREA (461:1)
               IF  (I-95)
                   MOVE '-'                TO OUTFIL-IO-AREA (462:1)
               END-IF
               MOVE BNOVAR                 TO EDIT-BNOVAR
               MOVE EDIT-BNOVAR            TO OUTFIL-IO-AREA (463:9)
               MOVE ';'                    TO OUTFIL-IO-AREA (472:1)
               IF  (I-96)
                   MOVE '-'                TO OUTFIL-IO-AREA (473:1)
               END-IF
               MOVE BDESAR                 TO EDIT-BDESAR
               MOVE EDIT-BDESAR            TO OUTFIL-IO-AREA (474:9)
               MOVE ';'                    TO OUTFIL-IO-AREA (483:1)
               WRITE OUTFIL-IO-AREA
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
           SET INNFIL-LEVEL-INIT           TO TRUE
           INITIALIZE INNFIL-DATA-FIELDS
           SET INNFIL-EOF-OFF              TO TRUE
           SET INNFIL-PROCESS              TO TRUE
           OPEN INPUT INNFIL
           OPEN OUTPUT OUTFIL.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INNFIL
           CLOSE OUTFIL.
 
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
