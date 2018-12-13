       IDENTIFICATION DIVISION.
       PROGRAM-ID. RKO219R.
      **********************************************  Z-WIN-RPG2   ****
      *  PROGRAM...: RKO219, SLETTEKODEMERKING JFR FIRMAF            *
      *  E 16.01.13: VISER ENTEN SLETTET NÅ ELLER TIDLIGERE          *
      *  E 10.09.13: RETTET BEREGNING AV ANTALL                      *
      *  FÅR       : FIRMAFILE (FIRMAF)                              *
      *              KONTOKURANT (KTOKURI)                           *
      *  GJØR      : MERKER RECORDS MED SLETTEKODE HVIS FIRMA ER     *
      *              SLETTET.                                        *
      *              INITIERER UTVIDET BELØPSFELT HVIS U8 ER PÅ.     *
      *  GIR       : KONTOKURANT (KTOKURO)                           *
      *  UPSI      : U1=DANNER NULLRECORD PÅ NYTT ÅR                 *
      *  UPSI      : U6=DANNER FØRSTEGANGS AVSTEMMINGSRECORD TIL     *
      *                 RK52.                                        *
      *              U7=DEBUGGER ER PÅ.                              *
      *              U8=INITIERER UTVIDEDE BELØPSFELT FRA GAMLE.     *
      ****************************************************************
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: RKO219.rpg
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
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT KTOKURI
               ASSIGN TO UT-S-KTOKURI
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KTOKURI-STATUS.
           SELECT KTOKURO
               ASSIGN TO UT-S-KTOKURO
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS KTOKURO-STATUS.
           SELECT AVSTEMO
               ASSIGN TO UT-S-AVSTEMO
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS AVSTEMO-STATUS.
           SELECT PRINT-1
               ASSIGN TO UT-S-PRINT1
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PRINT-1-STATUS.
           SELECT BUGFILO
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS BUGFILO-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
       FD KTOKURI
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  KTOKURI-IO-AREA.
           05  KTOKURI-IO-AREA-X           PICTURE X(200).
       FD KTOKURO
               BLOCK CONTAINS 2000
               RECORD CONTAINS 200.
       01  KTOKURO-IO-AREA.
           05  KTOKURO-IO-AREA-X           PICTURE X(200).
       FD AVSTEMO
               BLOCK CONTAINS 120
               RECORD CONTAINS 120.
       01  AVSTEMO-IO-AREA.
           05  AVSTEMO-IO-AREA-X           PICTURE X(120).
       FD PRINT-1
               BLOCK CONTAINS 133
               RECORD CONTAINS 133.
       01  PRINT-1-IO-PRINT.
           05  PRINT-1-IO-AREA-CONTROL     PICTURE X VALUE ' '.
        02 PRINT-1-IO-AREA.
           05  PRINT-1-IO-AREA-X           PICTURE X(132).
       FD BUGFILO
               BLOCK CONTAINS 81
               RECORD CONTAINS 81.
       01  BUGFILO-IO-PRINT.
           05  BUGFILO-IO-AREA-CONTROL     PICTURE X VALUE ' '.
        02 BUGFILO-IO-AREA.
           05  BUGFILO-IO-AREA-X           PICTURE X(80).
       WORKING-STORAGE SECTION.
       01  ACCEPT-COMMAND-LINE             PICTURE X(80).
       01  FILE-STATUS-TABLE.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  KTOKURI-STATUS              PICTURE 99 VALUE 0.
           10  KTOKURO-STATUS              PICTURE 99 VALUE 0.
           10  AVSTEMO-STATUS              PICTURE 99 VALUE 0.
           10  PRINT-1-STATUS              PICTURE 99 VALUE 0.
           10  BUGFILO-STATUS              PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
           05  FILLER                      PIC X VALUE '0'.
               88  KTOKURI-EOF-OFF         VALUE '0'.
               88  KTOKURI-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KTOKURI-READ-OFF        VALUE '0'.
               88  KTOKURI-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  KTOKURI-PROCESS-OFF     VALUE '0'.
               88  KTOKURI-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  KTOKURI-LEVEL-INIT-OFF  VALUE '0'.
               88  KTOKURI-LEVEL-INIT      VALUE '1'.
           05  PRINT-1-DATA-FIELDS.
               10  PRINT-1-AFTER-SPACE     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINT-1-AFTER-SKIP      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINT-1-BEFORE-SPACE    PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINT-1-BEFORE-SKIP     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINT-1-MAX-LINES       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINT-1-LINE-COUNT      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  PRINT-1-CLR-IO          PICTURE X VALUE 'Y'.
           05  BUGFILO-DATA-FIELDS.
               10  BUGFILO-AFTER-SPACE     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  BUGFILO-AFTER-SKIP      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  BUGFILO-BEFORE-SPACE    PICTURE 9(4) BINARY
                                           VALUE 0.
               10  BUGFILO-BEFORE-SKIP     PICTURE 9(4) BINARY
                                           VALUE 0.
               10  BUGFILO-MAX-LINES       PICTURE 9(4) BINARY
                                           VALUE 0.
               10  BUGFILO-LINE-COUNT      PICTURE 9(4) BINARY
                                           VALUE 0.
               10  BUGFILO-CLR-IO          PICTURE X VALUE 'Y'.
           05  FIRMAF-DATA-FIELDS.
               10  FSLETT                  PICTURE X(1).
           05  KTOKURI-LEVEL-02.
               10  KTOKURI-02-L5.
                   15  KTOKURI-02-L5-FIRMA PICTURE X(3).
               10  KTOKURI-02-L4.
                   15  KTOKURI-02-L4-RESK  PICTURE X(6).
           05  KTOKURI-LEVEL-03.
               10  KTOKURI-03-L5.
                   15  KTOKURI-03-L5-FIRMA PICTURE X(3).
               10  KTOKURI-03-L4.
                   15  KTOKURI-03-L4-RESK  PICTURE X(6).
           05  KTOKURI-DATA-FIELDS.
               10  REC02                   PICTURE X(200).
               10  FIRMA                   PICTURE X(3).
               10  RESK                    PICTURE X(6).
               10  SBEL-IO.
                   15  SBEL                PICTURE S9(8)V9(2).
               10  SVBEL-IO.
                   15  SVBEL               PICTURE S9(9)V9(2).
               10  SBELU-IO.
                   15  SBELU               PICTURE S9(11)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  SVBELU-IO.
                   15  SVBELU              PICTURE S9(11)V9(4) USAGE
                                                       PACKED-DECIMAL.
               10  SSLETT                  PICTURE X(1).
               10  REC03                   PICTURE X(200).
               10  BELO-ELGP-IO.
                   15  BELO-ELGP           PICTURE S9(7)V9(2).
               10  VBELO-ELGP-IO.
                   15  VBELO-ELGP          PICTURE S9(8)V9(2).
               10  BELO-ELGPU-IO.
                   15  BELO-ELGPU          PICTURE S9(11)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  VBELO-ELGU-IO.
                   15  VBELO-ELGU          PICTURE S9(11)V9(4) USAGE
                                                       PACKED-DECIMAL.
               10  TSLETT                  PICTURE X(1).
      *****************************************************************
      * HENTER DAGENS DATO                                            *
      *****************************************************************
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L5            PICTURE X(3).
               10  THE-PRIOR-L4            PICTURE X(6).
           05  TEMPORARY-FIELDS.
               10  KLOKKE-IO.
                   15  KLOKKE              PICTURE S9(6).
               10  DDMMAA                  PICTURE X(6).
               10  DD                      PICTURE X(2).
               10  MMAA                    PICTURE X(4).
               10  MM                      PICTURE X(2).
               10  AA                      PICTURE X(2).
               10  PDTO4-IO.
                   15  PDTO4               PICTURE S9(4).
               10  PRDDTO-IO.
                   15  PRDDTO              PICTURE S9(8).
               10  PRDKLK-IO.
                   15  PRDKLK              PICTURE S9(6).
               10  BUGFL1                  PICTURE X(8).
               10  ANT02-IO.
                   15  ANT02               PICTURE S9(9).
               10  ANT03-IO.
                   15  ANT03               PICTURE S9(9).
               10  TOTBEL-IO.
                   15  TOTBEL              PICTURE S9(11)V9(2).
               10  TOTBEU-IO.
                   15  TOTBEU              PICTURE S9(11)V9(2).
               10  ANFS02-IO.
                   15  ANFS02              PICTURE S9(9).
               10  SLFBEL-IO.
                   15  SLFBEL              PICTURE S9(11)V9(2).
               10  SLFBEU-IO.
                   15  SLFBEU              PICTURE S9(11)V9(2).
               10  ANFS03-IO.
                   15  ANFS03              PICTURE S9(9).
               10  ANDS02-IO.
                   15  ANDS02              PICTURE S9(9).
               10  SLDBEL-IO.
                   15  SLDBEL              PICTURE S9(11)V9(2).
               10  SLDBEU-IO.
                   15  SLDBEU              PICTURE S9(11)V9(2).
               10  ANDS03-IO.
                   15  ANDS03              PICTURE S9(9).
               10  ANTS02-IO.
                   15  ANTS02              PICTURE S9(9).
               10  SLEBEL-IO.
                   15  SLEBEL              PICTURE S9(11)V9(2).
               10  SLEBEU-IO.
                   15  SLEBEU              PICTURE S9(11)V9(2).
               10  ANTS03-IO.
                   15  ANTS03              PICTURE S9(9).
               10  ANTA02-IO.
                   15  ANTA02              PICTURE S9(9).
               10  AKTBEL-IO.
                   15  AKTBEL              PICTURE S9(11)V9(2).
               10  AKTBEU-IO.
                   15  AKTBEU              PICTURE S9(11)V9(2).
               10  ANTA03-IO.
                   15  ANTA03              PICTURE S9(9).
               10  ANTALL-IO.
                   15  ANTALL              PICTURE S9(9).
               10  ANTSLE-IO.
                   15  ANTSLE              PICTURE S9(9).
               10  ANTAKT-IO.
                   15  ANTAKT              PICTURE S9(9).
               10  ANFSLE-IO.
                   15  ANFSLE              PICTURE S9(9).
               10  ANDSLE-IO.
                   15  ANDSLE              PICTURE S9(9).
               10  LRRSAN-IO.
                   15  LRRSAN              PICTURE S9(13).
               10  LRRSBE-IO.
                   15  LRRSBE              PICTURE S9(13)V9(2).
               10  LRRSBU-IO.
                   15  LRRSBU              PICTURE S9(13)V9(2).
               10  LRRAKT-IO.
                   15  LRRAKT              PICTURE S9(13).
               10  LRRABE-IO.
                   15  LRRABE              PICTURE S9(13)V9(2).
               10  LRRABU-IO.
                   15  LRRABU              PICTURE S9(13)V9(2).
               10  LRANUL-IO.
                   15  LRANUL              PICTURE S9(13).
               10  LRBNUL-IO.
                   15  LRBNUL              PICTURE S9(13)V9(2).
               10  LRBNUU-IO.
                   15  LRBNUU              PICTURE S9(13)V9(2).
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
               10  XO-112YYZR              PICTURE ZZ.ZZZ.ZZZ.ZZZ,ZZ-.
               10  XO-90YY9                PICTURE ZZZ.ZZZ.ZZ9.
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
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  KTOKURI-PROCESS
               SET KTOKURI-PROCESS-OFF     TO TRUE
               SET KTOKURI-READ            TO TRUE
           END-IF
 
           IF  KTOKURI-READ
           AND RECORD-SELECTED-OFF
               PERFORM KTOKURI-GET
               SET KTOKURI-READ-OFF        TO TRUE
               IF  NOT KTOKURI-EOF
                   PERFORM KTOKURI-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET KTOKURI-PROCESS     TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  KTOKURI-PROCESS
               PERFORM KTOKURI-IDSET
           END-IF
 
           IF  KTOKURI-PROCESS
               PERFORM KTOKURI-CHK-LEVEL
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
 
           IF  KTOKURI-PROCESS
               PERFORM KTOKURI-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  KTOKURI-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (NOT-I-91)
               ACCEPT SYSTEM-TIME-X      FROM TIME
               MOVE SYSTEM-TIME            TO KLOKKE (1:6)
               MOVE UDATE                  TO DDMMAA
               MOVE DDMMAA (1:2)           TO DD
               MOVE DDMMAA (3:4)           TO MMAA
               MOVE MMAA (1:2)             TO MM
               MOVE MMAA (3:2)             TO AA
               MOVE 20                     TO PDTO4 (1:2)
               MOVE AA                     TO PDTO4-IO (3:2)
               MOVE PDTO4                  TO PRDDTO (1:4)
               MOVE MM                     TO PDTO4 (1:2)
               MOVE DD                     TO PDTO4-IO (3:2)
               MOVE PDTO4                  TO PRDDTO-IO (5:4)
               ACCEPT SYSTEM-TIME-X      FROM TIME
               MOVE SYSTEM-TIME            TO PRDKLK (1:6)
           END-IF
           IF  (NOT-I-91 AND I-U7)
               MOVE 'PRDDTO  '             TO BUGFL1
           END-IF
           IF  (NOT-I-91)
               SET I-91                    TO TRUE
      ****************************************************************
      *  SJEKKER OM FIRMA ELLER TRANS ER SLETTET                     *
      ****************************************************************
           END-IF
           SET NOT-I-28                    TO TRUE
           SET NOT-I-29                    TO TRUE
           IF  (I-L5)
               SET NOT-I-27                TO TRUE
               MOVE FIRMA                  TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-20                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-20            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
           END-IF
           IF  (I-L5 AND I-20)
               SET I-27                    TO TRUE
           END-IF
           IF  (I-L5 AND NOT-I-20)
               SET NOT-I-27                TO TRUE
               IF  FSLETT = 'S'
                   SET I-27                TO TRUE
               END-IF
           END-IF
           IF  (I-L5 AND I-U7 AND I-27)
               MOVE 'FIRMA   '             TO BUGFL1
           END-IF
           IF  (I-02)
               SET NOT-I-28                TO TRUE
               IF  SSLETT = 'S'
                   SET I-28                TO TRUE
               END-IF
           END-IF
           IF  (I-03)
               SET NOT-I-28                TO TRUE
               IF  TSLETT = 'S'
                   SET I-28                TO TRUE
               END-IF
           END-IF
           IF  (I-27)
               OR  (I-28)
               SET I-29                    TO TRUE
           END-IF
           IF  (I-02 AND I-U8)
               ADD SBEL TO ZERO        GIVING SBELU
               ADD SVBEL TO ZERO       GIVING SVBELU
           END-IF
           IF  (I-03 AND I-U8)
               ADD BELO-ELGP TO ZERO   GIVING BELO-ELGPU
               ADD VBELO-ELGP TO ZERO  GIVING VBELO-ELGU
           END-IF
           IF  (I-02)
               ADD 1                       TO ANT02
           END-IF
           IF  (I-03)
               ADD 1                       TO ANT03
           END-IF
           IF  (I-02)
               ADD SBEL                    TO TOTBEL ROUNDED
               ADD SBELU                   TO TOTBEU ROUNDED
           END-IF
           IF  (I-03)
               ADD BELO-ELGP               TO TOTBEL
               ADD BELO-ELGPU              TO TOTBEU
      ****************************************************************
      *  BEREGNING AV SLETTEDE FIRMA/TRANSER                         *
      ****************************************************************
           END-IF
           IF  (I-U7 AND I-28)
               MOVE 'REC02   '             TO BUGFL1
           END-IF
           IF  (I-02 AND I-27 AND NOT-I-28)
               ADD 1                       TO ANFS02
               ADD SBEL                    TO SLFBEL ROUNDED
               ADD SBELU                   TO SLFBEU ROUNDED
           END-IF
           IF  (I-03 AND I-27 AND NOT-I-28)
               ADD 1                       TO ANFS03
               ADD BELO-ELGP               TO SLFBEL
               ADD BELO-ELGPU              TO SLFBEU
           END-IF
           IF  (I-02 AND I-28)
               ADD 1                       TO ANDS02
               ADD SBEL                    TO SLDBEL ROUNDED
               ADD SBELU                   TO SLDBEU ROUNDED
           END-IF
           IF  (I-03 AND I-28)
               ADD 1                       TO ANDS03
               ADD BELO-ELGP               TO SLDBEL
               ADD BELO-ELGPU              TO SLDBEU
           END-IF
           IF  (I-02 AND I-29)
               ADD 1                       TO ANTS02
               ADD SBEL                    TO SLEBEL ROUNDED
               ADD SBELU                   TO SLEBEU ROUNDED
           END-IF
           IF  (I-03 AND I-29)
               ADD 1                       TO ANTS03
               ADD BELO-ELGP               TO SLEBEL
               ADD BELO-ELGPU              TO SLEBEU
      ****************************************************************
      *  BEREGNING AV AKTIVE FIRMA/TRANSER                           *
      ****************************************************************
           END-IF
           IF  (I-02 AND NOT-I-29)
               ADD 1                       TO ANTA02
               ADD SBEL                    TO AKTBEL ROUNDED
               ADD SBELU                   TO AKTBEU ROUNDED
           END-IF
           IF  (I-03 AND NOT-I-29)
               ADD 1                       TO ANTA03
               ADD BELO-ELGP               TO AKTBEL
               ADD BELO-ELGPU              TO AKTBEU
      ****************************************************************
      *  BEREGNING AV TOTALER                                        *
      ****************************************************************
           END-IF
           .
 
       LR-CALCS SECTION.
       LR-CALCS-P.
           ADD ANT03 TO ANT02          GIVING ANTALL
           ADD ANTS03 TO ANTS02        GIVING ANTSLE
           ADD ANTA03 TO ANTA02        GIVING ANTAKT
           ADD ANFS03 TO ANFS02        GIVING ANFSLE
           ADD ANDS03 TO ANDS02        GIVING ANDSLE
           ADD ANTSLE TO ZERO          GIVING LRRSAN
           ADD SLEBEL TO ZERO          GIVING LRRSBE
           ADD SLEBEU TO ZERO          GIVING LRRSBU
           ADD ANTAKT TO ZERO          GIVING LRRAKT
           ADD AKTBEL TO ZERO          GIVING LRRABE
           ADD AKTBEU TO ZERO          GIVING LRRABU
           MOVE 0                          TO LRANUL
           MOVE 0                          TO LRBNUL
           MOVE 0                          TO LRBNUU
      *R         ANTALL    SUB  ANTSLE    ANTAKT  90
      *R         TOTBEL    SUB  SLEBEL    AKTBEL 132H
      *R         TOTBEU    SUB  SLEBEU    AKTBEU 132H
           .
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (123:1) TO FSLETT (1:1)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-01                        TO TRUE.
 
       KTOKURI-GET SECTION.
       KTOKURI-GET-P.
           IF  KTOKURI-EOF-OFF
               READ KTOKURI
               AT END
                   SET KTOKURI-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       KTOKURI-FLDSET SECTION.
       KTOKURI-FLDSET-P.
           EVALUATE TRUE
           WHEN ( KTOKURI-IO-AREA (1:1) = '3'
            AND   KTOKURI-IO-AREA (2:1) = '0' )
               MOVE KTOKURI-IO-AREA (1:200) TO REC02 (1:200)
               MOVE KTOKURI-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE KTOKURI-IO-AREA (6:6)  TO RESK (1:6)
               MOVE KTOKURI-IO-AREA (38:10) TO SBEL-IO
               INSPECT SBEL-IO REPLACING ALL ' ' BY '0'
               MOVE KTOKURI-IO-AREA (50:11) TO SVBEL-IO
               INSPECT SVBEL-IO REPLACING ALL ' ' BY '0'
               MOVE KTOKURI-IO-AREA (114:7) TO SBELU-IO
               MOVE KTOKURI-IO-AREA (121:8) TO SVBELU-IO
               MOVE KTOKURI-IO-AREA (187:1) TO SSLETT (1:1)
           WHEN ( KTOKURI-IO-AREA (1:1) = '3'
            AND   KTOKURI-IO-AREA (2:1) = '1' )
               MOVE KTOKURI-IO-AREA (1:200) TO REC03 (1:200)
               MOVE KTOKURI-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE KTOKURI-IO-AREA (6:6)  TO RESK (1:6)
               MOVE KTOKURI-IO-AREA (39:9) TO BELO-ELGP-IO
               INSPECT BELO-ELGP-IO REPLACING ALL ' ' BY '0'
               MOVE KTOKURI-IO-AREA (51:10) TO VBELO-ELGP-IO
               INSPECT VBELO-ELGP-IO REPLACING ALL ' ' BY '0'
               MOVE KTOKURI-IO-AREA (114:7) TO BELO-ELGPU-IO
               MOVE KTOKURI-IO-AREA (121:8) TO VBELO-ELGU-IO
               MOVE KTOKURI-IO-AREA (187:1) TO TSLETT (1:1)
           END-EVALUATE.
 
       KTOKURI-IDCHK SECTION.
       KTOKURI-IDCHK-P.
           EVALUATE TRUE
           WHEN ( KTOKURI-IO-AREA (1:1) = '3'
            AND   KTOKURI-IO-AREA (2:1) = '0' )
             OR ( KTOKURI-IO-AREA (1:1) = '3'
            AND   KTOKURI-IO-AREA (2:1) = '1' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       KTOKURI-IDSET SECTION.
       KTOKURI-IDSET-P.
           EVALUATE TRUE
           WHEN ( KTOKURI-IO-AREA (1:1) = '3'
            AND   KTOKURI-IO-AREA (2:1) = '0' )
               SET I-02                    TO TRUE
           WHEN ( KTOKURI-IO-AREA (1:1) = '3'
            AND   KTOKURI-IO-AREA (2:1) = '1' )
               SET I-03                    TO TRUE
           END-EVALUATE.
 
       KTOKURI-CHK-LEVEL SECTION.
       KTOKURI-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ( KTOKURI-IO-AREA (1:1) = '3'
            AND   KTOKURI-IO-AREA (2:1) = '0' )
               MOVE LOW-VALUES             TO KTOKURI-LEVEL-02
               MOVE KTOKURI-IO-AREA (3:3)  TO KTOKURI-02-L5-FIRMA
               MOVE KTOKURI-IO-AREA (6:6)  TO KTOKURI-02-L4-RESK
               IF  KTOKURI-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  KTOKURI-02-L5 NOT = THE-PRIOR-L5
                       PERFORM SETON-I-L5
                   WHEN  KTOKURI-02-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   END-EVALUATE
               END-IF
               MOVE  KTOKURI-02-L5         TO THE-PRIOR-L5
               MOVE  KTOKURI-02-L4         TO THE-PRIOR-L4
               SET KTOKURI-LEVEL-INIT      TO TRUE
           WHEN ( KTOKURI-IO-AREA (1:1) = '3'
            AND   KTOKURI-IO-AREA (2:1) = '1' )
               MOVE LOW-VALUES             TO KTOKURI-LEVEL-03
               MOVE KTOKURI-IO-AREA (3:3)  TO KTOKURI-03-L5-FIRMA
               MOVE KTOKURI-IO-AREA (6:6)  TO KTOKURI-03-L4-RESK
               IF  KTOKURI-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  KTOKURI-03-L5 NOT = THE-PRIOR-L5
                       PERFORM SETON-I-L5
                   WHEN  KTOKURI-03-L4 NOT = THE-PRIOR-L4
                       PERFORM SETON-I-L4
                   END-EVALUATE
               END-IF
               MOVE  KTOKURI-03-L5         TO THE-PRIOR-L5
               MOVE  KTOKURI-03-L4         TO THE-PRIOR-L4
               SET KTOKURI-LEVEL-INIT      TO TRUE
           END-EVALUATE.
 
       PRINT-1-PRINT-LINE SECTION.
       PRINT-1-PRINT-LINE-P.
           IF  PRINT-1-BEFORE-SKIP > 0
               PERFORM PRINT-1-SKIP-BEFORE
           END-IF
           IF  PRINT-1-BEFORE-SPACE > 0
               PERFORM PRINT-1-SPACE-BEFORE
               IF  PRINT-1-AFTER-SKIP > 0
                   PERFORM PRINT-1-SKIP-AFTER
               END-IF
               IF  PRINT-1-AFTER-SPACE > 0
                   PERFORM PRINT-1-SPACE-AFTER
               END-IF
           ELSE
               IF  PRINT-1-AFTER-SKIP > 0
                   PERFORM PRINT-1-SKIP-AFTER
               END-IF
               PERFORM PRINT-1-SPACE-AFTER
           END-IF
           IF  PRINT-1-LINE-COUNT NOT < PRINT-1-MAX-LINES
               MOVE 7                      TO PRINT-1-AFTER-SKIP
           END-IF.
 
       PRINT-1-SKIP-BEFORE SECTION.
       PRINT-1-SKIP-BEFORE-P.
           WRITE PRINT-1-IO-PRINT       AFTER ADVANCING PAGE
           MOVE 1                          TO PRINT-1-LINE-COUNT
           MOVE 0                          TO PRINT-1-BEFORE-SKIP
           INITIALIZE PRINT-1-IO-AREA.
 
       PRINT-1-SPACE-BEFORE SECTION.
       PRINT-1-SPACE-BEFORE-P.
           WRITE PRINT-1-IO-PRINT       AFTER PRINT-1-BEFORE-SPACE
                                                                 LINES
           ADD PRINT-1-BEFORE-SPACE        TO PRINT-1-LINE-COUNT
           MOVE SPACES TO PRINT-1-IO-AREA
           INITIALIZE PRINT-1-IO-AREA
           MOVE 0                          TO PRINT-1-BEFORE-SPACE.
 
       PRINT-1-SKIP-AFTER SECTION.
       PRINT-1-SKIP-AFTER-P.
           WRITE PRINT-1-IO-PRINT      BEFORE ADVANCING PAGE
           MOVE 1                          TO PRINT-1-LINE-COUNT
           MOVE 0                          TO PRINT-1-AFTER-SKIP
           INITIALIZE PRINT-1-IO-AREA.
 
       PRINT-1-SPACE-AFTER SECTION.
       PRINT-1-SPACE-AFTER-P.
           WRITE PRINT-1-IO-PRINT      BEFORE PRINT-1-AFTER-SPACE LINES
           ADD PRINT-1-AFTER-SPACE         TO PRINT-1-LINE-COUNT
           INITIALIZE PRINT-1-IO-AREA
           MOVE 0                          TO PRINT-1-AFTER-SPACE.
 
       BUGFILO-PRINT-LINE SECTION.
       BUGFILO-PRINT-LINE-P.
           IF  BUGFILO-BEFORE-SKIP > 0
               PERFORM BUGFILO-SKIP-BEFORE
           END-IF
           IF  BUGFILO-BEFORE-SPACE > 0
               PERFORM BUGFILO-SPACE-BEFORE
               IF  BUGFILO-AFTER-SKIP > 0
                   PERFORM BUGFILO-SKIP-AFTER
               END-IF
               IF  BUGFILO-AFTER-SPACE > 0
                   PERFORM BUGFILO-SPACE-AFTER
               END-IF
           ELSE
               IF  BUGFILO-AFTER-SKIP > 0
                   PERFORM BUGFILO-SKIP-AFTER
               END-IF
               PERFORM BUGFILO-SPACE-AFTER
           END-IF
           IF  BUGFILO-LINE-COUNT NOT < BUGFILO-MAX-LINES
               MOVE 7                      TO BUGFILO-AFTER-SKIP
           END-IF.
 
       BUGFILO-SKIP-BEFORE SECTION.
       BUGFILO-SKIP-BEFORE-P.
           WRITE BUGFILO-IO-PRINT       AFTER ADVANCING PAGE
           MOVE 1                          TO BUGFILO-LINE-COUNT
           MOVE 0                          TO BUGFILO-BEFORE-SKIP
           INITIALIZE BUGFILO-IO-AREA.
 
       BUGFILO-SPACE-BEFORE SECTION.
       BUGFILO-SPACE-BEFORE-P.
           WRITE BUGFILO-IO-PRINT       AFTER BUGFILO-BEFORE-SPACE
                                                                 LINES
           ADD BUGFILO-BEFORE-SPACE        TO BUGFILO-LINE-COUNT
           MOVE SPACES TO BUGFILO-IO-AREA
           INITIALIZE BUGFILO-IO-AREA
           MOVE 0                          TO BUGFILO-BEFORE-SPACE.
 
       BUGFILO-SKIP-AFTER SECTION.
       BUGFILO-SKIP-AFTER-P.
           WRITE BUGFILO-IO-PRINT      BEFORE ADVANCING PAGE
           MOVE 1                          TO BUGFILO-LINE-COUNT
           MOVE 0                          TO BUGFILO-AFTER-SKIP
           INITIALIZE BUGFILO-IO-AREA.
 
       BUGFILO-SPACE-AFTER SECTION.
       BUGFILO-SPACE-AFTER-P.
           WRITE BUGFILO-IO-PRINT      BEFORE BUGFILO-AFTER-SPACE LINES
           ADD BUGFILO-AFTER-SPACE         TO BUGFILO-LINE-COUNT
           INITIALIZE BUGFILO-IO-AREA
           MOVE 0                          TO BUGFILO-AFTER-SPACE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-02)
               MOVE SPACES TO KTOKURO-IO-AREA
               INITIALIZE KTOKURO-IO-AREA
               MOVE REC02                  TO KTOKURO-IO-AREA (1:200)
               MOVE SBELU                  TO XO-112P
               MOVE XO-112P-EF             TO KTOKURO-IO-AREA (114:7)
               MOVE SVBELU                 TO XO-114P
               MOVE XO-114P-EF             TO KTOKURO-IO-AREA (121:8)
               IF  (I-27)
                   MOVE PRDDTO             TO XO-80P
                   MOVE XO-80P-EF          TO KTOKURO-IO-AREA (178:5)
               END-IF
               IF  (I-27)
                   MOVE PRDKLK             TO XO-60P
                   MOVE XO-60P-EF          TO KTOKURO-IO-AREA (183:4)
               END-IF
               IF  (I-27)
                   MOVE 'S'                TO KTOKURO-IO-AREA (187:1)
               END-IF
               WRITE KTOKURO-IO-AREA
           END-IF
           IF  (I-03)
               MOVE SPACES TO KTOKURO-IO-AREA
               INITIALIZE KTOKURO-IO-AREA
               MOVE REC03                  TO KTOKURO-IO-AREA (1:200)
               MOVE BELO-ELGPU             TO XO-112P
               MOVE XO-112P-EF             TO KTOKURO-IO-AREA (114:7)
               MOVE VBELO-ELGU             TO XO-114P
               MOVE XO-114P-EF             TO KTOKURO-IO-AREA (121:8)
               IF  (I-27)
                   MOVE PRDDTO             TO XO-80P
                   MOVE XO-80P-EF          TO KTOKURO-IO-AREA (178:5)
               END-IF
               IF  (I-27)
                   MOVE PRDKLK             TO XO-60P
                   MOVE XO-60P-EF          TO KTOKURO-IO-AREA (183:4)
               END-IF
               IF  (I-27)
                   MOVE 'S'                TO KTOKURO-IO-AREA (187:1)
               END-IF
               WRITE KTOKURO-IO-AREA
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-LR)
               MOVE SPACES TO AVSTEMO-IO-AREA
               INITIALIZE AVSTEMO-IO-AREA
               MOVE PRDDTO-IO              TO AVSTEMO-IO-AREA (1:8)
               MOVE '015'                  TO AVSTEMO-IO-AREA (9:3)
               MOVE 'MRK'                  TO AVSTEMO-IO-AREA (12:3)
               MOVE 'RKO219'               TO AVSTEMO-IO-AREA (15:6)
               MOVE '*RKO219*'             TO AVSTEMO-IO-AREA (21:8)
               MOVE PRDKLK-IO              TO AVSTEMO-IO-AREA (30:6)
               MOVE ANFSLE-IO              TO AVSTEMO-IO-AREA (40:9)
               MOVE SLFBEL-IO              TO AVSTEMO-IO-AREA (51:13)
               MOVE LRRSBU-IO              TO AVSTEMO-IO-AREA (64:15)
               MOVE PRDDTO-IO              TO AVSTEMO-IO-AREA (79:8)
               WRITE AVSTEMO-IO-AREA
               MOVE SPACES TO AVSTEMO-IO-AREA
               INITIALIZE AVSTEMO-IO-AREA
               MOVE '********'             TO AVSTEMO-IO-AREA (1:8)
               MOVE '015'                  TO AVSTEMO-IO-AREA (9:3)
               MOVE 'MRK'                  TO AVSTEMO-IO-AREA (12:3)
               MOVE '******'               TO AVSTEMO-IO-AREA (15:6)
               MOVE '*RKO219*'             TO AVSTEMO-IO-AREA (21:8)
               MOVE PRDKLK-IO              TO AVSTEMO-IO-AREA (30:6)
               MOVE ANFSLE-IO              TO AVSTEMO-IO-AREA (40:9)
               MOVE SLFBEL-IO              TO AVSTEMO-IO-AREA (51:13)
               MOVE LRRSBU-IO              TO AVSTEMO-IO-AREA (64:15)
               MOVE PRDDTO-IO              TO AVSTEMO-IO-AREA (79:8)
               WRITE AVSTEMO-IO-AREA
           END-IF
           IF  (I-LR AND NOT-I-U6)
               MOVE SPACES TO AVSTEMO-IO-AREA
               INITIALIZE AVSTEMO-IO-AREA
               MOVE PRDDTO-IO              TO AVSTEMO-IO-AREA (1:8)
               MOVE '020'                  TO AVSTEMO-IO-AREA (9:3)
               MOVE 'MRK'                  TO AVSTEMO-IO-AREA (12:3)
               MOVE 'RKO219'               TO AVSTEMO-IO-AREA (15:6)
               MOVE '*RKO219*'             TO AVSTEMO-IO-AREA (21:8)
               MOVE PRDKLK-IO              TO AVSTEMO-IO-AREA (30:6)
               MOVE LRRSAN-IO              TO AVSTEMO-IO-AREA (36:13)
               MOVE LRRSBE-IO              TO AVSTEMO-IO-AREA (49:15)
               MOVE LRRSBU-IO              TO AVSTEMO-IO-AREA (64:15)
               MOVE PRDDTO-IO              TO AVSTEMO-IO-AREA (79:8)
               WRITE AVSTEMO-IO-AREA
               MOVE SPACES TO AVSTEMO-IO-AREA
               INITIALIZE AVSTEMO-IO-AREA
               MOVE '********'             TO AVSTEMO-IO-AREA (1:8)
               MOVE '020'                  TO AVSTEMO-IO-AREA (9:3)
               MOVE 'MRK'                  TO AVSTEMO-IO-AREA (12:3)
               MOVE '******'               TO AVSTEMO-IO-AREA (15:6)
               MOVE '*RKO219*'             TO AVSTEMO-IO-AREA (21:8)
               MOVE PRDKLK-IO              TO AVSTEMO-IO-AREA (30:6)
               MOVE LRRSAN-IO              TO AVSTEMO-IO-AREA (36:13)
               MOVE LRRSBE-IO              TO AVSTEMO-IO-AREA (49:15)
               MOVE LRRSBU-IO              TO AVSTEMO-IO-AREA (64:15)
               MOVE PRDDTO-IO              TO AVSTEMO-IO-AREA (79:8)
               WRITE AVSTEMO-IO-AREA
           END-IF
           IF  (I-LR AND I-U6)
               MOVE SPACES TO AVSTEMO-IO-AREA
               INITIALIZE AVSTEMO-IO-AREA
               MOVE PRDDTO-IO              TO AVSTEMO-IO-AREA (1:8)
               MOVE '030'                  TO AVSTEMO-IO-AREA (9:3)
               MOVE 'MRK'                  TO AVSTEMO-IO-AREA (12:3)
               MOVE 'RKO219'               TO AVSTEMO-IO-AREA (15:6)
               MOVE '*RKO219*'             TO AVSTEMO-IO-AREA (21:8)
               MOVE PRDKLK-IO              TO AVSTEMO-IO-AREA (30:6)
               MOVE LRRAKT-IO              TO AVSTEMO-IO-AREA (36:13)
               MOVE LRRABE-IO              TO AVSTEMO-IO-AREA (49:15)
               MOVE LRRABU-IO              TO AVSTEMO-IO-AREA (64:15)
               MOVE PRDDTO-IO              TO AVSTEMO-IO-AREA (79:8)
               WRITE AVSTEMO-IO-AREA
               MOVE SPACES TO AVSTEMO-IO-AREA
               INITIALIZE AVSTEMO-IO-AREA
               MOVE '********'             TO AVSTEMO-IO-AREA (1:8)
               MOVE '030'                  TO AVSTEMO-IO-AREA (9:3)
               MOVE 'MRK'                  TO AVSTEMO-IO-AREA (12:3)
               MOVE '******'               TO AVSTEMO-IO-AREA (15:6)
               MOVE '*RKO219*'             TO AVSTEMO-IO-AREA (21:8)
               MOVE PRDKLK-IO              TO AVSTEMO-IO-AREA (30:6)
               MOVE LRRAKT-IO              TO AVSTEMO-IO-AREA (36:13)
               MOVE LRRABE-IO              TO AVSTEMO-IO-AREA (49:15)
               MOVE LRRABU-IO              TO AVSTEMO-IO-AREA (64:15)
               MOVE PRDDTO-IO              TO AVSTEMO-IO-AREA (79:8)
               WRITE AVSTEMO-IO-AREA
           END-IF
           IF  (I-LR AND I-U1)
               MOVE SPACES TO AVSTEMO-IO-AREA
               INITIALIZE AVSTEMO-IO-AREA
               MOVE PRDDTO-IO              TO AVSTEMO-IO-AREA (1:8)
               MOVE '030'                  TO AVSTEMO-IO-AREA (9:3)
               MOVE 'MRK'                  TO AVSTEMO-IO-AREA (12:3)
               MOVE 'RKO219'               TO AVSTEMO-IO-AREA (15:6)
               MOVE '*RKO219*'             TO AVSTEMO-IO-AREA (21:8)
               MOVE PRDKLK-IO              TO AVSTEMO-IO-AREA (30:6)
               MOVE LRANUL-IO              TO AVSTEMO-IO-AREA (36:13)
               MOVE LRBNUL-IO              TO AVSTEMO-IO-AREA (49:15)
               MOVE LRBNUU-IO              TO AVSTEMO-IO-AREA (64:15)
               MOVE PRDDTO-IO              TO AVSTEMO-IO-AREA (79:8)
               WRITE AVSTEMO-IO-AREA
               MOVE SPACES TO AVSTEMO-IO-AREA
               INITIALIZE AVSTEMO-IO-AREA
               MOVE '********'             TO AVSTEMO-IO-AREA (1:8)
               MOVE '030'                  TO AVSTEMO-IO-AREA (9:3)
               MOVE 'MRK'                  TO AVSTEMO-IO-AREA (12:3)
               MOVE '******'               TO AVSTEMO-IO-AREA (15:6)
               MOVE '*RKO219*'             TO AVSTEMO-IO-AREA (21:8)
               MOVE PRDKLK-IO              TO AVSTEMO-IO-AREA (30:6)
               MOVE LRANUL-IO              TO AVSTEMO-IO-AREA (36:13)
               MOVE LRBNUL-IO              TO AVSTEMO-IO-AREA (49:15)
               MOVE LRBNUU-IO              TO AVSTEMO-IO-AREA (64:15)
               MOVE PRDDTO-IO              TO AVSTEMO-IO-AREA (79:8)
               WRITE AVSTEMO-IO-AREA
           END-IF
           IF  (I-LR)
               MOVE SPACES TO PRINT-1-IO-AREA
               INITIALIZE PRINT-1-IO-AREA
               MOVE 'AVSTEMMINGSTOTALER FOR K' TO PRINT-1-IO-AREA
                                                                (1:24)
               MOVE 'ONTOKURANTER  DATO:'  TO PRINT-1-IO-AREA (25:19)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO PRINT-1-IO-AREA (44:8)
               MOVE 'KLOKKE:'              TO PRINT-1-IO-AREA (54:7)
               MOVE KLOKKE                 TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO PRINT-1-IO-AREA (61:8)
               MOVE 'RKO219'               TO PRINT-1-IO-AREA (75:6)
               MOVE 01                     TO PRINT-1-BEFORE-SKIP
               MOVE 3                      TO PRINT-1-BEFORE-SPACE
               MOVE 2                      TO PRINT-1-AFTER-SPACE
               PERFORM PRINT-1-PRINT-LINE
               MOVE SPACES TO PRINT-1-IO-AREA
               INITIALIZE PRINT-1-IO-AREA
               MOVE '************************' TO PRINT-1-IO-AREA
                                                                (1:24)
               MOVE '************************' TO PRINT-1-IO-AREA
                                                               (25:24)
               MOVE '************************' TO PRINT-1-IO-AREA
                                                               (49:24)
               MOVE '************************' TO PRINT-1-IO-AREA
                                                               (57:24)
               MOVE 1                      TO PRINT-1-AFTER-SPACE
               PERFORM PRINT-1-PRINT-LINE
               MOVE SPACES TO PRINT-1-IO-AREA
               INITIALIZE PRINT-1-IO-AREA
               MOVE '*'                    TO PRINT-1-IO-AREA (1:1)
               MOVE '*'                    TO PRINT-1-IO-AREA (80:1)
               MOVE 1                      TO PRINT-1-AFTER-SPACE
               PERFORM PRINT-1-PRINT-LINE
               MOVE SPACES TO PRINT-1-IO-AREA
               INITIALIZE PRINT-1-IO-AREA
               MOVE '*'                    TO PRINT-1-IO-AREA (1:1)
               MOVE 'BELØP '               TO PRINT-1-IO-AREA (25:6)
               MOVE 'ANTALL'               TO PRINT-1-IO-AREA (45:6)
               MOVE 'UTV BELØP '           TO PRINT-1-IO-AREA (61:10)
               MOVE '*'                    TO PRINT-1-IO-AREA (80:1)
               MOVE 1                      TO PRINT-1-AFTER-SPACE
               PERFORM PRINT-1-PRINT-LINE
               MOVE SPACES TO PRINT-1-IO-AREA
               INITIALIZE PRINT-1-IO-AREA
               MOVE '*'                    TO PRINT-1-IO-AREA (1:1)
               MOVE '* INN    :'           TO PRINT-1-IO-AREA (1:10)
               MOVE TOTBEL                 TO XO-112YYZR
               MOVE XO-112YYZR             TO PRINT-1-IO-AREA (13:18)
               MOVE ANTALL                 TO XO-90YY9
               MOVE XO-90YY9               TO PRINT-1-IO-AREA (40:11)
               MOVE TOTBEU                 TO XO-112YYZR
               MOVE XO-112YYZR             TO PRINT-1-IO-AREA (53:18)
               MOVE '*'                    TO PRINT-1-IO-AREA (80:1)
               MOVE 1                      TO PRINT-1-AFTER-SPACE
               PERFORM PRINT-1-PRINT-LINE
               MOVE SPACES TO PRINT-1-IO-AREA
               INITIALIZE PRINT-1-IO-AREA
               MOVE '*'                    TO PRINT-1-IO-AREA (1:1)
               MOVE '* SLETTET:'           TO PRINT-1-IO-AREA (1:10)
               MOVE SLEBEL                 TO XO-112YYZR
               MOVE XO-112YYZR             TO PRINT-1-IO-AREA (13:18)
               MOVE ANTSLE                 TO XO-90YY9
               MOVE XO-90YY9               TO PRINT-1-IO-AREA (40:11)
               MOVE SLEBEU                 TO XO-112YYZR
               MOVE XO-112YYZR             TO PRINT-1-IO-AREA (53:18)
               MOVE '*'                    TO PRINT-1-IO-AREA (80:1)
               MOVE 1                      TO PRINT-1-AFTER-SPACE
               PERFORM PRINT-1-PRINT-LINE
               MOVE SPACES TO PRINT-1-IO-AREA
               INITIALIZE PRINT-1-IO-AREA
               MOVE '*'                    TO PRINT-1-IO-AREA (1:1)
               MOVE '* (NÅ)   :'           TO PRINT-1-IO-AREA (1:10)
               MOVE SLFBEL                 TO XO-112YYZR
               MOVE XO-112YYZR             TO PRINT-1-IO-AREA (13:18)
               MOVE ANFSLE                 TO XO-90YY9
               MOVE XO-90YY9               TO PRINT-1-IO-AREA (40:11)
               MOVE SLFBEU                 TO XO-112YYZR
               MOVE XO-112YYZR             TO PRINT-1-IO-AREA (53:18)
               MOVE '*'                    TO PRINT-1-IO-AREA (80:1)
               MOVE 1                      TO PRINT-1-AFTER-SPACE
               PERFORM PRINT-1-PRINT-LINE
               MOVE SPACES TO PRINT-1-IO-AREA
               INITIALIZE PRINT-1-IO-AREA
               MOVE '*'                    TO PRINT-1-IO-AREA (1:1)
               MOVE '* (TIDL.):'           TO PRINT-1-IO-AREA (1:10)
               MOVE SLDBEL                 TO XO-112YYZR
               MOVE XO-112YYZR             TO PRINT-1-IO-AREA (13:18)
               MOVE ANDSLE                 TO XO-90YY9
               MOVE XO-90YY9               TO PRINT-1-IO-AREA (40:11)
               MOVE SLDBEU                 TO XO-112YYZR
               MOVE XO-112YYZR             TO PRINT-1-IO-AREA (53:18)
               MOVE '*'                    TO PRINT-1-IO-AREA (80:1)
               MOVE 1                      TO PRINT-1-AFTER-SPACE
               PERFORM PRINT-1-PRINT-LINE
               MOVE SPACES TO PRINT-1-IO-AREA
               INITIALIZE PRINT-1-IO-AREA
               MOVE '*'                    TO PRINT-1-IO-AREA (1:1)
               MOVE '* AKTIVE :'           TO PRINT-1-IO-AREA (1:10)
               MOVE AKTBEL                 TO XO-112YYZR
               MOVE XO-112YYZR             TO PRINT-1-IO-AREA (13:18)
               MOVE ANTAKT                 TO XO-90YY9
               MOVE XO-90YY9               TO PRINT-1-IO-AREA (40:11)
               MOVE AKTBEU                 TO XO-112YYZR
               MOVE XO-112YYZR             TO PRINT-1-IO-AREA (53:18)
               MOVE '*'                    TO PRINT-1-IO-AREA (80:1)
               MOVE 1                      TO PRINT-1-AFTER-SPACE
               PERFORM PRINT-1-PRINT-LINE
               MOVE SPACES TO PRINT-1-IO-AREA
               INITIALIZE PRINT-1-IO-AREA
               MOVE '*'                    TO PRINT-1-IO-AREA (1:1)
               MOVE '*'                    TO PRINT-1-IO-AREA (80:1)
               MOVE 1                      TO PRINT-1-AFTER-SPACE
               PERFORM PRINT-1-PRINT-LINE
               MOVE SPACES TO PRINT-1-IO-AREA
               INITIALIZE PRINT-1-IO-AREA
               MOVE '* Beløp og antall på lin' TO PRINT-1-IO-AREA
                                                                (1:24)
               MOVE 'jen for AKTIVE transaksj' TO PRINT-1-IO-AREA
                                                               (25:24)
               MOVE 'oner skal stemme med bel' TO PRINT-1-IO-AREA
                                                               (49:24)
               MOVE 'øp og  *'             TO PRINT-1-IO-AREA (73:8)
               MOVE 1                      TO PRINT-1-AFTER-SPACE
               PERFORM PRINT-1-PRINT-LINE
               MOVE SPACES TO PRINT-1-IO-AREA
               INITIALIZE PRINT-1-IO-AREA
               MOVE '* antall fra RKO220 i RE' TO PRINT-1-IO-AREA
                                                                (1:24)
               MOVE 'S50AM (denne jobben), og' TO PRINT-1-IO-AREA
                                                               (25:24)
               MOVE ' med beløp i RKO040 i DO' TO PRINT-1-IO-AREA
                                                               (49:24)
               MOVE 'P12UD  *'             TO PRINT-1-IO-AREA (73:8)
               MOVE 1                      TO PRINT-1-AFTER-SPACE
               PERFORM PRINT-1-PRINT-LINE
               MOVE SPACES TO PRINT-1-IO-AREA
               INITIALIZE PRINT-1-IO-AREA
               MOVE '* fra siste kveldskjørin' TO PRINT-1-IO-AREA
                                                                (1:24)
               MOVE 'g.                      ' TO PRINT-1-IO-AREA
                                                               (25:24)
               MOVE '                        ' TO PRINT-1-IO-AREA
                                                               (49:24)
               MOVE '        *'            TO PRINT-1-IO-AREA (72:9)
               MOVE 1                      TO PRINT-1-AFTER-SPACE
               PERFORM PRINT-1-PRINT-LINE
               MOVE SPACES TO PRINT-1-IO-AREA
               INITIALIZE PRINT-1-IO-AREA
               MOVE '*'                    TO PRINT-1-IO-AREA (1:1)
               MOVE '*'                    TO PRINT-1-IO-AREA (80:1)
               MOVE 1                      TO PRINT-1-AFTER-SPACE
               PERFORM PRINT-1-PRINT-LINE
               MOVE SPACES TO PRINT-1-IO-AREA
               INITIALIZE PRINT-1-IO-AREA
               MOVE '************************' TO PRINT-1-IO-AREA
                                                                (1:24)
               MOVE '************************' TO PRINT-1-IO-AREA
                                                               (25:24)
               MOVE '************************' TO PRINT-1-IO-AREA
                                                               (49:24)
               MOVE '************************' TO PRINT-1-IO-AREA
                                                               (57:24)
      ****************************************************************
      * DUMMY-LINJE FOR Å FJERNE FEILMELDING.                        *
      ****************************************************************
               MOVE 1                      TO PRINT-1-AFTER-SPACE
               PERFORM PRINT-1-PRINT-LINE
           END-IF
           IF  (I-LR AND I-U2)
           AND (I-U3 AND I-U4 AND I-01)
               MOVE 1                      TO PRINT-1-AFTER-SPACE
               PERFORM PRINT-1-PRINT-LINE
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
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           SET KTOKURI-LEVEL-INIT          TO TRUE
           INITIALIZE KTOKURI-DATA-FIELDS
           SET KTOKURI-EOF-OFF             TO TRUE
           SET KTOKURI-PROCESS             TO TRUE
           OPEN INPUT KTOKURI
           OPEN OUTPUT KTOKURO
           OPEN OUTPUT AVSTEMO
           OPEN OUTPUT PRINT-1
           INITIALIZE PRINT-1-IO-AREA
           INITIALIZE PRINT-1-DATA-FIELDS
           MOVE 57                         TO PRINT-1-MAX-LINES
           IF I-U7
               OPEN OUTPUT BUGFILO
           END-IF
           INITIALIZE BUGFILO-IO-AREA
           INITIALIZE BUGFILO-DATA-FIELDS
           MOVE 57                         TO BUGFILO-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE FIRMAF
           CLOSE KTOKURI
           CLOSE KTOKURO
           CLOSE AVSTEMO
           IF PRINT-1-IO-AREA NOT = SPACES
             WRITE PRINT-1-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO PRINT-1-IO-AREA
           END-IF
           CLOSE PRINT-1
           IF I-U7
               CLOSE BUGFILO
           END-IF.
 
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
