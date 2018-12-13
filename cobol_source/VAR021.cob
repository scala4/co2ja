       IDENTIFICATION DIVISION.
       PROGRAM-ID. VAR021R.
      **********************************************  Z-WIN-RPG2   ****
      * 05.09.2011 - Fjernet test på fnr 452
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: VAR021.rpg
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
           SELECT INNPUT2
               ASSIGN TO UT-S-INNPUT2
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INNPUT2-STATUS.
           SELECT INNPUT
               ASSIGN TO UT-S-INNPUT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INNPUT-STATUS.
           SELECT UTFILE
               ASSIGN TO UT-S-UTFILE
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS UTFILE-STATUS.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD INNPUT2
               BLOCK CONTAINS 9600
               RECORD CONTAINS 200.
       01  INNPUT2-IO-AREA.
           05  INNPUT2-IO-AREA-X           PICTURE X(200).
       FD INNPUT
               BLOCK CONTAINS 9600
               RECORD CONTAINS 200.
       01  INNPUT-IO-AREA.
           05  INNPUT-IO-AREA-X            PICTURE X(200).
       FD UTFILE
               BLOCK CONTAINS 9000
               RECORD CONTAINS 100.
       01  UTFILE-IO-AREA.
           05  UTFILE-IO-AREA-X            PICTURE X(100).
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
           10  INNPUT2-STATUS              PICTURE 99 VALUE 0.
           10  INNPUT-STATUS               PICTURE 99 VALUE 0.
           10  UTFILE-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  INNPUT2-EOF-OFF         VALUE '0'.
               88  INNPUT2-EOF             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNPUT2-READ-OFF        VALUE '0'.
               88  INNPUT2-READ            VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNPUT2-PROCESS-OFF     VALUE '0'.
               88  INNPUT2-PROCESS         VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNPUT-EOF-OFF          VALUE '0'.
               88  INNPUT-EOF              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNPUT-READ-OFF         VALUE '0'.
               88  INNPUT-READ             VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INNPUT-PROCESS-OFF      VALUE '0'.
               88  INNPUT-PROCESS          VALUE '1'.
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
           05  INNPUT2-DATA-FIELDS.
               10  FIRMA                   PICTURE X(3).
               10  EDBNR                   PICTURE X(7).
               10  LEV1-IO.
                   15  LEV1                PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
               10  LEV2-IO.
                   15  LEV2                PICTURE S9(7) USAGE
                                                       PACKED-DECIMAL.
           05  INNPUT2-MP                  PICTURE X(10).
           05  INNPUT2-MC                  PICTURE X(10).
           05  INNPUT2-M-02            REDEFINES INNPUT2-MC.
               10  INNPUT2-M-02-M2.
                   15  INNPUT2-M-02-M2-FIRMA-G.
                       20  INNPUT2-M-02-M2-FIRMA PICTURE X(3).
               10  INNPUT2-M-02-M1.
                   15  INNPUT2-M-02-M1-EDBNR-G.
                       20  INNPUT2-M-02-M1-EDBNR PICTURE X(7).
           05  INNPUT-DATA-FIELDS.
               10  ALFA                    PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  INN-IO.
                   15  INN                 PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  UT-IO.
                   15  UT                  PICTURE S9(7)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  KINNH-IO.
                   15  KINNH               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  REST-IO.
                   15  REST                PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  MERK                    PICTURE X(1).
               10  AIBES-IO.
                   15  AIBES               PICTURE S9(7).
               10  LOC                     PICTURE X(6).
               10  LAG13-IO.
                   15  LAG13               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  LAG93-IO.
                   15  LAG93               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  LAG15-IO.
                   15  LAG15               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  LAG17-IO.
                   15  LAG17               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  LAG92-IO.
                   15  LAG92               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  LAG18-IO.
                   15  LAG18               PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  MINB-IO.
                   15  MINB                PICTURE S9(5) USAGE
                                                       PACKED-DECIMAL.
               10  REBEST                  PICTURE X(1).
      *
      * MERKNAD PÅ BESTILLING ELLER UTGÅR SKAL IKKE MED. HELLER IKKE X
      * I REBESTILLINGSFELTET. + 0 I MIN-BEH.
      *
           05  INNPUT-MP                   PICTURE X(10).
           05  INNPUT-MC                   PICTURE X(10).
           05  INNPUT-M-03             REDEFINES INNPUT-MC.
               10  INNPUT-M-03-M2.
                   15  INNPUT-M-03-M2-FIRMA-G.
                       20  INNPUT-M-03-M2-FIRMA PICTURE X(3).
               10  INNPUT-M-03-M1.
                   15  INNPUT-M-03-M1-EDBNR-G.
                       20  INNPUT-M-03-M1-EDBNR PICTURE X(7).
           05  TEMPORARY-FIELDS.
               10  LEVNR-IO.
                   15  LEVNR               PICTURE S9(6).
               10  BEH-IO.
                   15  BEH                 PICTURE S9(7).
               10  SUM1-IO.
                   15  SUM1                PICTURE S9(9).
               10  FORSL-IO.
                   15  FORSL               PICTURE S9(7).
               10  HFORS-IO.
                   15  HFORS               PICTURE S9(7)V9(2).
               10  HKINNH-IO.
                   15  HKINNH              PICTURE S9(5)V9(1).
               10  ANTFOR-IO.
                   15  ANTFOR              PICTURE S9(7)V9(2).
           05  EDITTING-FIELDS.
               10  XO-70YNZR               PICTURE ZZZZZZZ-.
               10  XO-60YNZ                PICTURE ZZZZZZ.
               10  XO-70YY9R               PICTURE Z.ZZZ.ZZ9-.
               10  XO-70YNZ                PICTURE ZZZZZZZ.
               10  XO-50YNZ                PICTURE ZZZZZ.
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
           SET NOT-I-02                    TO TRUE
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-H
 
           IF  I-LR
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  INNPUT2-PROCESS
               SET INNPUT2-PROCESS-OFF     TO TRUE
               SET INNPUT2-READ            TO TRUE
           END-IF
 
           IF  INNPUT2-READ
               PERFORM INNPUT2-GET
               SET INNPUT2-READ-OFF        TO TRUE
               IF  NOT INNPUT2-EOF
                   PERFORM INNPUT2-MATCH-SET
               END-IF
           END-IF
 
           IF  INNPUT-PROCESS
               SET INNPUT-PROCESS-OFF      TO TRUE
               SET INNPUT-READ             TO TRUE
           END-IF
 
           IF  INNPUT-READ
               PERFORM INNPUT-GET
               SET INNPUT-READ-OFF         TO TRUE
               IF  NOT INNPUT-EOF
                   PERFORM INNPUT-MATCH-SET
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  CALL-MATCH-RECS
               PERFORM MATCHING-RECORDS
           END-IF
 
           IF  INNPUT2-PROCESS
               PERFORM INNPUT2-IDSET
           END-IF
 
           IF  INNPUT-PROCESS
               PERFORM INNPUT-IDSET
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
           PERFORM DETAIL-OVERFLOW
           SET NOT-I-MR                    TO TRUE
           IF  SET-I-MR
               SET I-MR                    TO TRUE
               SET NOT-SET-I-MR            TO TRUE
           END-IF
 
           IF  INNPUT2-PROCESS
               PERFORM INNPUT2-FLDOFF
               PERFORM INNPUT2-FLDSET
           END-IF
 
           IF  INNPUT-PROCESS
               PERFORM INNPUT-FLDOFF
               PERFORM INNPUT-FLDSET
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
           IF  (I-03 AND I-20)
               SET I-10                    TO TRUE
               GO TO SLUTT-T
      * ** ROGALAND KUN LAGER 10.
           END-IF
           IF  (I-03)
               SET NOT-I-15                TO TRUE
               IF  FIRMA = '965'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  FIRMA = '968'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  FIRMA = '764'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  FIRMA = '738'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  FIRMA = '958'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  FIRMA = '957'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  FIRMA = '904'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  FIRMA = '903'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  FIRMA = '911'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  FIRMA = '912'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  FIRMA = '940'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  FIRMA = '658'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  FIRMA = '950'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  FIRMA = '963'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  FIRMA = '930'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  FIRMA = '628'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  FIRMA = '956'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  FIRMA = '900'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  FIRMA = '432'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  FIRMA = '414'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  FIRMA = '910'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  FIRMA = '722'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  FIRMA = '634'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-15)
               SET NOT-I-15                TO TRUE
               IF  FIRMA = '787'
                   SET I-15                TO TRUE
               END-IF
           END-IF
           IF  (I-03)
               SET NOT-I-16                TO TRUE
               IF  FIRMA = '614'
                   SET I-16                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-16)
               SET NOT-I-16                TO TRUE
               IF  FIRMA = '963'
                   SET I-16                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-16)
               SET NOT-I-16                TO TRUE
               IF  FIRMA = '930'
                   SET I-16                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-16)
               SET NOT-I-16                TO TRUE
               IF  FIRMA = '414'
                   SET I-16                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-16)
               SET NOT-I-16                TO TRUE
               IF  FIRMA = '911'
                   SET I-16                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-16)
               SET NOT-I-16                TO TRUE
               IF  FIRMA = '910'
                   SET I-16                TO TRUE
               END-IF
           END-IF
           IF  (I-03)
               SET NOT-I-51                TO TRUE
               IF  FIRMA = '370'
                   SET I-51                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-51)
               SET NOT-I-51                TO TRUE
               IF  FIRMA = '371'
                   SET I-51                TO TRUE
               END-IF
      *  03      FIRMA     COMP "689"                    17
           END-IF
           IF  (I-03)
               SET NOT-I-78                TO TRUE
               IF  FIRMA = '608'
                   SET I-78                TO TRUE
               END-IF
               SET NOT-I-77                TO TRUE
               IF  FIRMA = '958'
                   SET I-77                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-77)
               SET NOT-I-77                TO TRUE
               IF  FIRMA = '231'
                   SET I-77                TO TRUE
               END-IF
      *  03N77   FIRMA     COMP "910"                    77
           END-IF
           IF  (I-03)
               SET NOT-I-12                TO TRUE
               IF  MERK = '1'
                   SET I-12                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-51 AND NOT-I-12)
               GO TO IKPBES-T
           END-IF
           IF  (I-03 AND NOT-I-12)
               SET NOT-I-12                TO TRUE
               IF  MERK = '2'
                   SET I-12                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-12)
               SET NOT-I-12                TO TRUE
               IF  REBEST = '2'
                   SET I-12                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-12)
               SET NOT-I-12                TO TRUE
               IF  REBEST = 'X'
                   SET I-12                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND NOT-I-12 AND NOT-I-78)
               SET NOT-I-12                TO TRUE
               IF  REBEST = ' '
                   SET I-12                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-12)
               SET I-10                    TO TRUE
           END-IF
           IF  (I-03 AND NOT-I-78)
               SET NOT-I-78                TO TRUE
               IF  FIRMA = '995'
                   SET I-78                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-12)
               GO TO SLUTT-T
           END-IF.
 
       IKPBES-T.
      *
           IF  (I-02)
               ADD LEV1 TO ZERO        GIVING LEVNR
           END-IF
           IF  (I-03)
               SUBTRACT UT FROM INN    GIVING BEH
           END-IF
           IF  (I-03 AND I-15)
               SUBTRACT LAG13              FROM BEH
               SUBTRACT LAG93              FROM BEH
               SUBTRACT LAG15              FROM BEH
               SUBTRACT LAG17              FROM BEH
               SUBTRACT LAG92              FROM BEH
               SUBTRACT LAG18              FROM BEH
           END-IF
           IF  (I-03)
               SET NOT-I-96                TO TRUE
               IF  BEH < 0
                   SET I-96                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-96)
               MOVE 0                      TO BEH
      * **
           END-IF
           IF  (I-03)
               SET NOT-I-13                TO TRUE
               IF  KINNH = 0
                   SET I-13                TO TRUE
               END-IF
               ADD AIBES TO BEH        GIVING SUM1
               SUBTRACT SUM1 FROM MINB GIVING FORSL
           END-IF
           IF  (I-03 AND NOT-I-78)
               ADD REST                    TO FORSL
           END-IF
           IF  (I-03)
               SET NOT-I-36                TO TRUE
               SET NOT-I-35                TO TRUE
               IF  FORSL > 0
                   SET I-36                TO TRUE
               END-IF
               IF  FORSL < 0
                   SET I-35                TO TRUE
               END-IF
           END-IF
           IF  (I-03 AND I-35)
               MOVE 0                      TO FORSL
           END-IF
           IF  (I-03 AND NOT-I-13 AND I-16)
               AND (I-36)
               ADD KINNH TO ZERO       GIVING FORSL
           END-IF
           IF  (I-03 AND NOT-I-13 AND I-36)
               AND (I-77)
               SUBTRACT SUM1 FROM KINNH GIVING FORSL
           END-IF
           IF  (I-03 AND NOT-I-13 AND NOT-I-16)
               AND (NOT-I-77)
               PERFORM INNRUT-S
           END-IF.
 
       SLUTT-T.
           IF  (I-03)
               SET NOT-I-10                TO TRUE
               IF  FORSL NOT > 0
                   SET I-10                TO TRUE
               END-IF
      *  03 17   FORSL     COMP 3                      10
      * ** SUBRUTINE FOR BEREGNING AV KART.INNH
           END-IF
           .
 
       INNRUT-S SECTION.
       INNRUT-S-P.
           SET NOT-I-80                    TO TRUE
           IF  FORSL = 0
               SET I-80                    TO TRUE
           END-IF
           SET NOT-I-81                    TO TRUE
           IF  BEH = 0
               SET I-81                    TO TRUE
           END-IF
           IF  (I-80)
               GO TO SLUTTK-T
           END-IF
           ADD FORSL TO ZERO           GIVING HFORS
           MOVE 0                          TO HKINNH
           MOVE 0                          TO ANTFOR
           DIVIDE KINNH BY 2           GIVING HKINNH.
 
       LOOP-T.
           SET NOT-I-85                    TO TRUE
           IF  HFORS NOT < KINNH
               SET I-85                    TO TRUE
           END-IF
           IF  (I-85)
               SUBTRACT KINNH              FROM HFORS
               ADD 1                       TO ANTFOR
               GO TO LOOP-T
           END-IF
           SET NOT-I-86                    TO TRUE
           IF  HFORS NOT < HKINNH
               SET I-86                    TO TRUE
           END-IF
           IF  (I-86)
               ADD 1                       TO ANTFOR
           END-IF
           SET NOT-I-82                    TO TRUE
           IF  ANTFOR = 0
               SET I-82                    TO TRUE
           END-IF
           IF  (I-82)
               MOVE 1                      TO ANTFOR
           END-IF
           MULTIPLY ANTFOR BY KINNH    GIVING FORSL ROUNDED.
 
       SLUTTK-T.
           CONTINUE.
 
       INNPUT2-GET SECTION.
       INNPUT2-GET-P.
           IF  INNPUT2-EOF-OFF
               READ INNPUT2
               AT END
                   SET INNPUT2-EOF         TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INNPUT2-FLDOFF SECTION.
       INNPUT2-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-50                TO TRUE
           END-EVALUATE.
 
       INNPUT2-FLDSET SECTION.
       INNPUT2-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INNPUT2-IO-AREA (3:3)  TO FIRMA (1:3)
               MOVE INNPUT2-IO-AREA (6:7)  TO EDBNR (1:7)
               MOVE INNPUT2-IO-AREA (68:4) TO LEV1-IO
               IF  LEV1 = ZERO
                   SET I-50                TO TRUE
               END-IF
               MOVE INNPUT2-IO-AREA (72:4) TO LEV2-IO
           END-EVALUATE.
 
       INNPUT2-IDSET SECTION.
       INNPUT2-IDSET-P.
           SET I-02                        TO TRUE.
 
       INNPUT2-MATCH-SET SECTION.
       INNPUT2-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE INNPUT2-IO-AREA (3:3)  TO INNPUT2-M-02-M2-FIRMA
               MOVE INNPUT2-IO-AREA (6:7)  TO INNPUT2-M-02-M1-EDBNR
           END-EVALUATE.
 
       INNPUT-GET SECTION.
       INNPUT-GET-P.
           IF  INNPUT-EOF-OFF
               READ INNPUT
               AT END
                   SET INNPUT-EOF          TO TRUE
               END-READ
           END-IF.
 
       INNPUT-FLDOFF SECTION.
       INNPUT-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-20                TO TRUE
           END-EVALUATE.
 
       INNPUT-FLDSET SECTION.
       INNPUT-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INNPUT-IO-AREA (3:3)   TO FIRMA (1:3)
               MOVE INNPUT-IO-AREA (6:7)   TO EDBNR (1:7)
               MOVE INNPUT-IO-AREA (13:3)  TO ALFA (1:3)
               MOVE INNPUT-IO-AREA (16:20) TO ARTNR (1:20)
               MOVE INNPUT-IO-AREA (97:5)  TO INN-IO
               MOVE INNPUT-IO-AREA (102:5) TO UT-IO
               MOVE INNPUT-IO-AREA (108:3) TO KINNH-IO
               MOVE INNPUT-IO-AREA (114:3) TO REST-IO
               MOVE INNPUT-IO-AREA (127:1) TO MERK (1:1)
               MOVE INNPUT-IO-AREA (129:7) TO AIBES-IO
               INSPECT AIBES-IO REPLACING ALL ' ' BY '0'
               MOVE INNPUT-IO-AREA (140:6) TO LOC (1:6)
               MOVE INNPUT-IO-AREA (179:3) TO LAG13-IO
               MOVE INNPUT-IO-AREA (182:3) TO LAG93-IO
               MOVE INNPUT-IO-AREA (185:3) TO LAG15-IO
               MOVE INNPUT-IO-AREA (188:3) TO LAG17-IO
               MOVE INNPUT-IO-AREA (191:3) TO LAG92-IO
               MOVE INNPUT-IO-AREA (194:3) TO LAG18-IO
               MOVE INNPUT-IO-AREA (197:3) TO MINB-IO
               IF  MINB = ZERO
                   SET I-20                TO TRUE
               END-IF
               MOVE INNPUT-IO-AREA (200:1) TO REBEST (1:1)
           END-EVALUATE.
 
       INNPUT-IDSET SECTION.
       INNPUT-IDSET-P.
           SET I-03                        TO TRUE.
 
       INNPUT-MATCH-SET SECTION.
       INNPUT-MATCH-SET-P.
           SET CALL-MATCH-RECS             TO TRUE
           EVALUATE TRUE
           WHEN ANY
               MOVE INNPUT-IO-AREA (3:3)   TO INNPUT-M-03-M2-FIRMA
               MOVE INNPUT-IO-AREA (6:7)   TO INNPUT-M-03-M1-EDBNR
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
           IF  INNPUT2-EOF
               MOVE HIGH-VALUES            TO INNPUT2-MC
                                              INNPUT2-MP
           END-IF
           IF  INNPUT-EOF
               MOVE HIGH-VALUES            TO INNPUT-MC
                                              INNPUT-MP
           END-IF
           IF  INNPUT2-MC < INNPUT2-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           IF  INNPUT-MC < INNPUT-MP
               SET I-H0                    TO TRUE
               MOVE 'B'                    TO E-R-R-O-R
           END-IF
           EVALUATE TRUE
           WHEN  INNPUT2-MC < INNPUT-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET INNPUT2-PROCESS     TO TRUE
                   MOVE INNPUT2-MC         TO INNPUT2-MP
                   IF  INNPUT2-MC = INNPUT-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  INNPUT-MC < INNPUT2-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET INNPUT-PROCESS      TO TRUE
                   MOVE INNPUT-MC          TO INNPUT-MP
                   IF  INNPUT-MC = INNPUT2-MP
                       SET SET-I-MR        TO TRUE
                   ELSE
                       SET NOT-SET-I-MR    TO TRUE
                   END-IF
               END-IF
           WHEN  INNPUT2-MC = INNPUT-MC
               IF  RECORD-SELECTED-OFF
                   SET RECORD-SELECTED     TO TRUE
                   SET INNPUT2-PROCESS     TO TRUE
                   MOVE INNPUT2-MC         TO INNPUT2-MP
                   SET SET-I-MR            TO TRUE
               END-IF
           END-EVALUATE.
 
       DETAIL-OUTPUT SECTION.
       DETAIL-OUTPUT-P.
           IF  (I-03 AND I-MR AND NOT-I-10)
               MOVE SPACES TO UTFILE-IO-AREA
               INITIALIZE UTFILE-IO-AREA
               MOVE FIRMA                  TO UTFILE-IO-AREA (1:3)
               MOVE EDBNR                  TO UTFILE-IO-AREA (4:7)
               MOVE ALFA                   TO UTFILE-IO-AREA (11:3)
               MOVE ARTNR                  TO UTFILE-IO-AREA (14:20)
               MOVE FORSL                  TO XO-70YNZR
               MOVE XO-70YNZR              TO UTFILE-IO-AREA (33:8)
               MOVE LEVNR                  TO XO-60YNZ
               MOVE XO-60YNZ               TO UTFILE-IO-AREA (41:6)
               MOVE LOC                    TO UTFILE-IO-AREA (95:6)
               WRITE UTFILE-IO-AREA
           END-IF
           IF  (I-1P)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'LISTE FORSLAGSFILE     ' TO LISTE-IO-AREA (26:23)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '-----------------------' TO LISTE-IO-AREA (26:23)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-03 AND I-MR AND NOT-I-10)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE FIRMA                  TO LISTE-IO-AREA (1:3)
               MOVE EDBNR                  TO LISTE-IO-AREA (4:7)
               MOVE ALFA                   TO LISTE-IO-AREA (11:3)
               MOVE ARTNR                  TO LISTE-IO-AREA (14:20)
               MOVE FORSL                  TO XO-70YY9R
               MOVE XO-70YY9R              TO LISTE-IO-AREA (31:10)
               INITIALIZE FORSL
               MOVE LEVNR                  TO XO-60YNZ
               MOVE XO-60YNZ               TO LISTE-IO-AREA (41:6)
               MOVE BEH                    TO XO-70YNZ
               MOVE XO-70YNZ               TO LISTE-IO-AREA (54:7)
               MOVE MINB                   TO XO-50YNZ
               MOVE XO-50YNZ               TO LISTE-IO-AREA (66:5)
               MOVE AIBES                  TO XO-70YNZ
               MOVE XO-70YNZ               TO LISTE-IO-AREA (74:7)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       DETAIL-OVERFLOW SECTION.
       DETAIL-OVERFLOW-P.
           IF  (I-OF)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'LISTE FORSLAGSFILE     ' TO LISTE-IO-AREA (26:23)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '-----------------------' TO LISTE-IO-AREA (26:23)
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
           MOVE 1                          TO LR-CHECK
           INITIALIZE INNPUT2-DATA-FIELDS
           SET INNPUT2-EOF-OFF             TO TRUE
           SET INNPUT2-PROCESS             TO TRUE
           MOVE LOW-VALUES                 TO INNPUT2-MC
                                              INNPUT2-MP
           OPEN INPUT INNPUT2
           INITIALIZE INNPUT-DATA-FIELDS
           SET INNPUT-EOF-OFF              TO TRUE
           SET INNPUT-PROCESS              TO TRUE
           MOVE LOW-VALUES                 TO INNPUT-MC
                                              INNPUT-MP
           OPEN INPUT INNPUT
           OPEN OUTPUT UTFILE
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE INNPUT2
           CLOSE INNPUT
           CLOSE UTFILE
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
