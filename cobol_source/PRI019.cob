       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRI019R.
      **********************************************  Z-WIN-RPG2   ****
      * PRISLISTER UTLISTINGSPROGRAM. MED OMREGNING AV PRISER.      *
      * NU1  =  48 LINJERS PAPIR.                                   *
      *  U1  =  72 LINJERS PAPIR.                                   *
      *  U2  =  INGEN FORSIDE.                                      *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *
      **  armrpg: RPG to COBOL/VSE Version - 2018/09/26 2.5 R0 0362
      **        : Inglenet Business Solutions :
      ** options: -mv
      **  Source: PRI019.rpg
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
           SELECT PKORT
               ASSIGN TO UT-S-PKORT
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS PKORT-STATUS.
           SELECT INF
               ASSIGN TO UT-S-INF
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS INF-STATUS.
           SELECT RABMAST
               ASSIGN TO RABMAST
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS RABMAST-STATUS
               RECORD KEY IS RABMAST-KEY1.
           SELECT FIRMAF
               ASSIGN TO FIRMAF
               ACCESS MODE IS DYNAMIC
               ORGANIZATION IS INDEXED
               STATUS IS FIRMAF-STATUS
               RECORD KEY IS FIRMAF-KEY1.
           SELECT LISTE
               ASSIGN TO SYS020-UR-3203-SYSLST
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS SEQUENTIAL
               STATUS IS LISTE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD PKORT
               BLOCK CONTAINS 80
               RECORD CONTAINS 80.
       01  PKORT-IO-AREA.
           05  PKORT-IO-AREA-X             PICTURE X(80).
       FD INF
               BLOCK CONTAINS 9440
               RECORD CONTAINS 80.
       01  INF-IO-AREA.
           05  INF-IO-AREA-X               PICTURE X(80).
       FD RABMAST
               RECORD CONTAINS 40.
       01  RABMAST-IO-AREA.
           05  RABMAST-IO-AREA-X.
               10  RABMAST-KEY1            PICTURE X(20).
               10  FILLER                  PICTURE X(20).
       FD FIRMAF
               RECORD CONTAINS 1000.
       01  FIRMAF-IO-AREA.
           05  FIRMAF-IO-AREA-X.
               10  FILLER                  PICTURE X(3).
               10  FIRMAF-KEY1             PICTURE X(3).
               10  FILLER                  PICTURE X(994).
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
           10  PKORT-STATUS                PICTURE 99 VALUE 0.
           10  INF-STATUS                  PICTURE 99 VALUE 0.
           10  RABMAST-STATUS              PICTURE 99 VALUE 0.
           10  FIRMAF-STATUS               PICTURE 99 VALUE 0.
           10  LISTE-STATUS                PICTURE 99 VALUE 0.
 
       01  WORK-AREA-BATCH.
           05  FILLER                      PIC X VALUE '0'.
               88  PKORT-EOF-OFF           VALUE '0'.
               88  PKORT-EOF               VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PKORT-READ-OFF          VALUE '0'.
               88  PKORT-READ              VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  PKORT-PROCESS-OFF       VALUE '0'.
               88  PKORT-PROCESS           VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INF-EOF-OFF             VALUE '0'.
               88  INF-EOF                 VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INF-READ-OFF            VALUE '0'.
               88  INF-READ                VALUE '1'.
           05  FILLER                      PIC X VALUE '0'.
               88  INF-PROCESS-OFF         VALUE '0'.
               88  INF-PROCESS             VALUE '1'.
           05  FILLER                      PIC X VALUE '1'.
               88  INF-LEVEL-INIT-OFF      VALUE '0'.
               88  INF-LEVEL-INIT          VALUE '1'.
           05  RABMAST-KEY-NUM             PICTURE 9 VALUE 1 BINARY.
           05  FIRMAF-KEY-NUM              PICTURE 9 VALUE 1 BINARY.
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
           05  PKORT-DATA-FIELDS.
               10  TEKST                   PICTURE X(49).
               10  TEKST1                  PICTURE X(38).
               10  RABGRP                  PICTURE X(6).
               10  PPROS-IO.
                   15  PPROS               PICTURE S9(3)V9(2).
           05  INF-LEVEL-01.
               10  INF-01-L3.
                   15  INF-01-L3-FIRMA     PICTURE X(3).
               10  INF-01-L2.
                   15  INF-01-L2-SNR       PICTURE S9(4).
               10  INF-01-L1.
                   15  INF-01-L1-LNR       PICTURE X(2).
           05  INF-DATA-FIELDS.
               10  SNR-IO.
                   15  SNR                 PICTURE S9(4).
               10  LNR                     PICTURE X(2).
               10  POS-X                   PICTURE X(1).
               10  ALF                     PICTURE X(3).
               10  ARTNR                   PICTURE X(20).
               10  VBET                    PICTURE X(24).
               10  PRIS-IO.
                   15  PRIS                PICTURE S9(5)V9(2) USAGE
                                                       PACKED-DECIMAL.
               10  PT                      PICTURE X(1).
               10  FIRMA                   PICTURE X(3).
               10  VGR                     PICTURE X(5).
           05  RABMAST-DATA-FIELDS.
               10  RABRA                   PICTURE X(1).
               10  RAB1-IO.
                   15  RAB1                PICTURE S9(2)V9(1).
               10  RAB2-IO.
                   15  RAB2                PICTURE S9(2)V9(1).
               10  RAB3-IO.
                   15  RAB3                PICTURE S9(2)V9(1).
               10  NETFAK-IO.
                   15  NETFAK              PICTURE S9(1)V9(4).
               10  SLETT                   PICTURE X(1).
           05  FIRMAF-DATA-FIELDS.
               10  FINAVN                  PICTURE X(30).
           05  THE-PRIOR-LEVEL.
               10  THE-PRIOR-L3            PICTURE X(3).
               10  THE-PRIOR-L2            PICTURE X(4).
               10  THE-PRIOR-L1            PICTURE X(2).
           05  TEMPORARY-FIELDS.
               10  BEL-IO.
                   15  BEL                 PICTURE S9(5)V9(2).
               10  B92-IO.
                   15  B92                 PICTURE S9(7)V9(2).
               10  ALF1                    PICTURE X(3).
               10  ANR1                    PICTURE X(20).
               10  BET1                    PICTURE X(24).
               10  BEL1-IO.
                   15  BEL1                PICTURE S9(5)V9(2).
               10  PT1                     PICTURE X(1).
               10  ALF2                    PICTURE X(3).
               10  ANR2                    PICTURE X(20).
               10  BET2                    PICTURE X(24).
               10  BEL2-IO.
                   15  BEL2                PICTURE S9(5)V9(2).
               10  PT2                     PICTURE X(1).
               10  F11                     PICTURE X(11).
               10  F6                      PICTURE X(6).
               10  F17                     PICTURE X(17).
               10  RABKEY                  PICTURE X(20).
               10  PRI11-IO.
                   15  PRI11               PICTURE S9(9)V9(2).
               10  NPRIS-IO.
                   15  NPRIS               PICTURE S9(7)V9(2).
               10  T1-IO.
                   15  T1                  PICTURE S9(1).
               10  T2-IO.
                   15  T2                  PICTURE SV9(2).
               10  T3-IO.
                   15  T3                  PICTURE S9(2).
               10  OPPGNR                  PICTURE X(6).
               10  FIRMNR                  PICTURE X(3).
               10  FIRMNA                  PICTURE X(30).
           05  EDITTING-FIELDS.
               10  XO-40YNZ                PICTURE ZZZZ.
               10  XO-52YNZ                PICTURE ZZZZZ,ZZ.
               10  XO-52YYZ                PICTURE ZZ.ZZZ,ZZ.
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
           SET NOT-I-1P                    TO TRUE.
 
       MAINLINE-LOOP.
           PERFORM HEADING-OUTPUT
 
           PERFORM HALT-INDICATOR-CHECK
           SET NOT-I-02                    TO TRUE
           SET NOT-I-04                    TO TRUE
           SET NOT-I-01                    TO TRUE
           SET NOT-I-06                    TO TRUE
           SET NOT-I-03                    TO TRUE
 
           PERFORM SETOFF-I-L
           PERFORM SETOFF-I-H
 
           IF  I-LR
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           SET RECORD-SELECTED-OFF         TO TRUE
           IF  PKORT-PROCESS
               SET PKORT-PROCESS-OFF       TO TRUE
               SET PKORT-READ              TO TRUE
           END-IF
 
           IF  PKORT-READ
           AND RECORD-SELECTED-OFF
               PERFORM PKORT-GET
               SET PKORT-READ-OFF          TO TRUE
               IF  NOT PKORT-EOF
                   PERFORM PKORT-IDCHK
                   PERFORM HALT-INDICATOR-CHECK
                   SET PKORT-PROCESS       TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  INF-PROCESS
               SET INF-PROCESS-OFF         TO TRUE
               SET INF-READ                TO TRUE
           END-IF
 
           IF  INF-READ
           AND RECORD-SELECTED-OFF
               PERFORM INF-GET
               SET INF-READ-OFF            TO TRUE
               IF  NOT INF-EOF
                   SET INF-PROCESS         TO TRUE
                   SET RECORD-SELECTED     TO TRUE
               END-IF
           END-IF
 
           IF  LR-CHECK < 1
               SET I-LR                    TO TRUE
               PERFORM SETON-I-L9
               GO TO MAINLINE-TOTAL-CALCS
           END-IF
 
           IF  PKORT-PROCESS
               PERFORM PKORT-IDSET
           END-IF
 
           IF  INF-PROCESS
               PERFORM INF-IDSET
           END-IF
 
           IF  INF-PROCESS
               PERFORM INF-CHK-LEVEL
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
 
           IF  PKORT-PROCESS
               PERFORM PKORT-FLDOFF
               PERFORM PKORT-FLDSET
           END-IF
 
           IF  INF-PROCESS
               PERFORM INF-FLDSET
           END-IF
 
           PERFORM DETAIL-CALCS
           IF  INF-PROCESS
               SET NOT-I-1ST               TO TRUE
           END-IF
           GO TO MAINLINE-LOOP.
 
       MAINLINE-TERMINATION.
           PERFORM TERMINATION
           MOVE ZERO                       TO RETURN-CODE
           STOP RUN.
 
       DETAIL-CALCS SECTION.
       DETAIL-CALCS-P.
           IF  (I-02)
               GO TO SLUTT-T
           END-IF
           IF  (I-04)
               GO TO SLUTT-T
           END-IF
           IF  (I-L3)
               MOVE FIRMA                  TO FIRMAF-KEY1
               READ FIRMAF RECORD KEY IS FIRMAF-KEY1
               INVALID KEY
                   SET I-98                TO TRUE
               NOT INVALID KEY
                   SET NOT-I-98            TO TRUE
                   PERFORM FIRMAF-FLDSET
                   PERFORM FIRMAF-IDSET
               END-READ
           END-IF
           IF  (I-L3 AND NOT-I-98 AND NOT-I-U2)
               PERFORM SIDE1-S
           END-IF
           IF  (I-01)
               ADD PRIS TO ZERO        GIVING BEL
               MOVE 'N'                    TO PT
           END-IF
           IF  (I-01 AND NOT-I-08)
               PERFORM RABRUT-S
           END-IF
           IF  (I-01 AND I-30 AND NOT-I-07)
               MULTIPLY PPROS BY BEL   GIVING B92 ROUNDED
               DIVIDE B92 BY 100       GIVING B92 ROUNDED
               ADD B92                     TO BEL
           END-IF
           IF  (I-01)
               PERFORM AVRRUT-S
      *******************************************************
      * PRIS KOLONNE RUTINE.                                *
      *******************************************************
           END-IF
           SET NOT-I-22                    TO TRUE
           IF  POS-X = '2'
               SET I-22                    TO TRUE
           END-IF
           IF  (I-22)
               GO TO P22-T
           END-IF
           MOVE ALF                        TO ALF1
           MOVE ARTNR                      TO ANR1
           MOVE VBET                       TO BET1
           MOVE BEL                        TO BEL1-IO
           MOVE PT                         TO PT1
           GO TO SLUTT-T.
 
       P22-T.
           MOVE ALF                        TO ALF2
           MOVE ARTNR                      TO ANR2
           MOVE VBET                       TO BET2
           MOVE BEL                        TO BEL2-IO
           MOVE PT                         TO PT2.
 
       SLUTT-T.
      ******************************************************
      *    BEREGNING AV NETTOPRIS PÅ NETTOPRIS-VARER.      *
      ******************************************************
           CONTINUE.
 
       RABRUT-S SECTION.
       RABRUT-S-P.
           SET NOT-I-77                    TO TRUE
           IF  PT = 'N'
               SET I-77                    TO TRUE
           END-IF
           MOVE RABGRP                     TO F11 (1:6)
           MOVE VGR                        TO F11 (7:5)
           MOVE ALF                        TO F6 (1:3)
           IF  (I-77)
               MOVE 'NTO'                  TO F6 (4:3)
           END-IF
           IF  (NOT-I-77)
               MOVE '   '                  TO F6 (4:3)
           END-IF
           MOVE F11                        TO F17 (1:11)
           MOVE F6                         TO F17 (12:6)
           MOVE F17                        TO RABKEY (4:17)
           MOVE FIRMA                      TO RABKEY (1:3)
           MOVE RABKEY                     TO RABMAST-KEY1
           READ RABMAST RECORD KEY IS RABMAST-KEY1
           INVALID KEY
               SET I-30                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-30                TO TRUE
               PERFORM RABMAST-FLDOFF
               PERFORM RABMAST-FLDSET
               PERFORM RABMAST-IDSET
           END-READ
           IF  (NOT-I-30 AND NOT-I-28)
               GO TO RABRNY-T
           END-IF
           IF  (NOT-I-30)
               SET NOT-I-38                TO TRUE
               IF  RABRA = '8'
                   SET I-38                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-30 AND I-38)
               GO TO RABR2-T
           END-IF
           IF  (NOT-I-30 AND NOT-I-38)
               GO TO RABN2-T
           END-IF.
 
       RABRNY-T.
           IF  (NOT-I-77)
               MOVE 'NTO'                  TO F6 (4:3)
           END-IF
           IF  (I-77)
               MOVE '   '                  TO F6 (4:3)
           END-IF
           MOVE RABKEY                     TO RABMAST-KEY1
           READ RABMAST RECORD KEY IS RABMAST-KEY1
           INVALID KEY
               SET I-30                    TO TRUE
           NOT INVALID KEY
               SET NOT-I-30                TO TRUE
               PERFORM RABMAST-FLDOFF
               PERFORM RABMAST-FLDSET
               PERFORM RABMAST-IDSET
           END-READ
           IF  (NOT-I-30 AND NOT-I-28)
               SET I-30                    TO TRUE
           END-IF
           IF  (NOT-I-30)
               SET NOT-I-37                TO TRUE
               IF  RABRA = '9'
                   SET I-37                TO TRUE
               END-IF
           END-IF
           IF  (NOT-I-30 AND I-37)
               GO TO RABN2-T
           END-IF
           IF  (I-30)
               GO TO RABEND-T
           END-IF.
 
       RABR2-T.
           MULTIPLY RAB1 BY PRIS       GIVING PRI11
           DIVIDE PRI11 BY 100         GIVING PRI11 ROUNDED
           SUBTRACT PRI11 FROM PRIS    GIVING NPRIS
           MULTIPLY RAB2 BY NPRIS      GIVING PRI11
           DIVIDE PRI11 BY 100         GIVING PRI11 ROUNDED
           SUBTRACT PRI11                  FROM NPRIS
           MULTIPLY RAB3 BY NPRIS      GIVING PRI11
           DIVIDE PRI11 BY 100         GIVING PRI11 ROUNDED
           SUBTRACT PRI11 FROM NPRIS   GIVING BEL
           GO TO RABEND-T.
 
       RABN2-T.
           MULTIPLY NETFAK BY PRIS     GIVING BEL ROUNDED
           GO TO RABEND-T.
 
       RABEND-T.
           CONTINUE.
      ******************************************************
      *    ØREAVRUNDING PÅ UTSALGSPRIS.                    *
      *    UNDER KR. 50,00 OPP TIL NÆRMESTE 10 ØRE.        *
      *    OVER  KR. 50,00 OPP TIL NÆRMESTE KRONE.         *
      ******************************************************
 
       AVRRUT-S SECTION.
       AVRRUT-S-P.
           SET NOT-I-51                    TO TRUE
           SET NOT-I-49                    TO TRUE
           SET NOT-I-50                    TO TRUE
           IF  BEL > 50,00
               SET I-51                    TO TRUE
           END-IF
           IF  BEL < 50,00
               SET I-49                    TO TRUE
           END-IF
           IF  BEL = 50,00
               SET I-50                    TO TRUE
           END-IF
           IF  (I-50)
               GO TO AVREND-T
      **** UNDER 50 KRONER.          ***********************
           END-IF
           IF  (I-49)
               MOVE BEL (7:1)              TO T1-IO
               SET NOT-I-50                TO TRUE
               IF  T1 = 0
                   SET I-50                TO TRUE
               END-IF
           END-IF
           IF  (I-49 AND NOT-I-50)
               DIVIDE T1 BY 100        GIVING T2
               ADD 0,10                    TO BEL
               SUBTRACT T2                 FROM BEL
           END-IF
           IF  (I-49)
               GO TO AVREND-T
      **** OVER  50 KRONER.          ***********************
           END-IF
           IF  (I-51)
               MOVE BEL (6:2)              TO T3-IO
               SET NOT-I-50                TO TRUE
               IF  T3 = 0
                   SET I-50                TO TRUE
               END-IF
           END-IF
           IF  (I-51 AND NOT-I-50)
               DIVIDE T3 BY 100        GIVING T2
               ADD 1,00                    TO BEL
               SUBTRACT T2                 FROM BEL
           END-IF
           IF  (I-51)
               GO TO AVREND-T
           END-IF.
 
       AVREND-T.
           CONTINUE.
      ******************************************************
      *    SUBRUTINE FOR PRINTING AV SIDE 1 PR. FIRMA      *
      ******************************************************
 
       SIDE1-S SECTION.
       SIDE1-S-P.
           SET I-86                        TO TRUE
           MOVE 'PRI523'                   TO OPPGNR
           MOVE FIRMA                      TO FIRMNR
           MOVE FINAVN                     TO FIRMNA
           PERFORM EXCEPTION-OUTPUT
           SET NOT-I-86                    TO TRUE.
      ******************************************************
 
       PKORT-GET SECTION.
       PKORT-GET-P.
           IF  PKORT-EOF-OFF
               READ PKORT
               AT END
                   SET PKORT-EOF           TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       PKORT-FLDOFF SECTION.
       PKORT-FLDOFF-P.
           EVALUATE TRUE
           WHEN ( PKORT-IO-AREA (1:1) = '9'
            AND   PKORT-IO-AREA (2:1) = '0' )
               SET NOT-I-99                TO TRUE
               SET NOT-I-97                TO TRUE
           WHEN ( PKORT-IO-AREA (1:1) = '9'
            AND   PKORT-IO-AREA (2:1) = '1' )
               SET NOT-I-08                TO TRUE
               SET NOT-I-07                TO TRUE
           END-EVALUATE.
 
       PKORT-FLDSET SECTION.
       PKORT-FLDSET-P.
           EVALUATE TRUE
           WHEN ( PKORT-IO-AREA (1:1) = '9'
            AND   PKORT-IO-AREA (2:1) = '0' )
               MOVE PKORT-IO-AREA (12:49)  TO TEKST (1:49)
               IF  TEKST = SPACES
                   SET I-99                TO TRUE
               END-IF
               MOVE PKORT-IO-AREA (12:38)  TO TEKST1 (1:38)
               IF  TEKST1 = SPACES
                   SET I-97                TO TRUE
               END-IF
           WHEN ( PKORT-IO-AREA (1:1) = '9'
            AND   PKORT-IO-AREA (2:1) = '1' )
               MOVE PKORT-IO-AREA (17:6)   TO RABGRP (1:6)
               IF  RABGRP = SPACES
                   SET I-08                TO TRUE
               END-IF
               MOVE PKORT-IO-AREA (50:5)   TO PPROS-IO
               INSPECT PPROS-IO REPLACING ALL ' ' BY '0'
               IF  PPROS = ZERO
                   SET I-07                TO TRUE
               END-IF
           END-EVALUATE.
 
       PKORT-IDCHK SECTION.
       PKORT-IDCHK-P.
           EVALUATE TRUE
           WHEN ( PKORT-IO-AREA (1:1) = '9'
            AND   PKORT-IO-AREA (2:1) = '0' )
             OR ( PKORT-IO-AREA (1:1) = '9'
            AND   PKORT-IO-AREA (2:1) = '1' )
               CONTINUE
           WHEN  OTHER
               SET I-H0                    TO TRUE
               MOVE 'A'                    TO E-R-R-O-R
           END-EVALUATE.
 
       PKORT-IDSET SECTION.
       PKORT-IDSET-P.
           EVALUATE TRUE
           WHEN ( PKORT-IO-AREA (1:1) = '9'
            AND   PKORT-IO-AREA (2:1) = '0' )
               SET I-02                    TO TRUE
           WHEN ( PKORT-IO-AREA (1:1) = '9'
            AND   PKORT-IO-AREA (2:1) = '1' )
               SET I-04                    TO TRUE
           END-EVALUATE.
 
       INF-GET SECTION.
       INF-GET-P.
           IF  INF-EOF-OFF
               READ INF
               AT END
                   SET INF-EOF             TO TRUE
                   SUBTRACT 1            FROM LR-CHECK
               END-READ
           END-IF.
 
       INF-FLDSET SECTION.
       INF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE INF-IO-AREA (1:4)      TO SNR-IO
               INSPECT SNR-IO REPLACING ALL ' ' BY '0'
               MOVE INF-IO-AREA (5:2)      TO LNR (1:2)
               MOVE INF-IO-AREA (7:1)      TO POS-X (1:1)
               MOVE INF-IO-AREA (8:3)      TO ALF (1:3)
               MOVE INF-IO-AREA (11:20)    TO ARTNR (1:20)
               MOVE INF-IO-AREA (31:24)    TO VBET (1:24)
               MOVE INF-IO-AREA (55:4)     TO PRIS-IO
               MOVE INF-IO-AREA (59:1)     TO PT (1:1)
               MOVE INF-IO-AREA (61:3)     TO FIRMA (1:3)
               MOVE INF-IO-AREA (68:5)     TO VGR (1:5)
           END-EVALUATE.
 
       INF-IDSET SECTION.
       INF-IDSET-P.
           SET I-01                        TO TRUE.
 
       INF-CHK-LEVEL SECTION.
       INF-CHK-LEVEL-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE LOW-VALUES             TO INF-LEVEL-01
               MOVE INF-IO-AREA (61:3)     TO INF-01-L3-FIRMA
               MOVE INF-IO-AREA (1:4)      TO INF-01-L2-SNR
               MOVE INF-IO-AREA (5:2)      TO INF-01-L1-LNR
               IF  INF-LEVEL-INIT
                   EVALUATE TRUE
                   WHEN  INF-01-L3 NOT = THE-PRIOR-L3
                       PERFORM SETON-I-L3
                   WHEN  INF-01-L2 NOT = THE-PRIOR-L2
                       PERFORM SETON-I-L2
                   WHEN  INF-01-L1 NOT = THE-PRIOR-L1
                       PERFORM SETON-I-L1
                   END-EVALUATE
               END-IF
               MOVE  INF-01-L3             TO THE-PRIOR-L3
               MOVE  INF-01-L2             TO THE-PRIOR-L2
               MOVE  INF-01-L1             TO THE-PRIOR-L1
               SET INF-LEVEL-INIT          TO TRUE
           END-EVALUATE.
 
       RABMAST-FLDOFF SECTION.
       RABMAST-FLDOFF-P.
           EVALUATE TRUE
           WHEN ANY
               SET NOT-I-28                TO TRUE
           END-EVALUATE.
 
       RABMAST-FLDSET SECTION.
       RABMAST-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE RABMAST-IO-AREA (1:1)  TO RABRA (1:1)
               MOVE RABMAST-IO-AREA (22:3) TO RAB1-IO
               INSPECT RAB1-IO REPLACING ALL ' ' BY '0'
               MOVE RABMAST-IO-AREA (25:3) TO RAB2-IO
               INSPECT RAB2-IO REPLACING ALL ' ' BY '0'
               MOVE RABMAST-IO-AREA (28:3) TO RAB3-IO
               INSPECT RAB3-IO REPLACING ALL ' ' BY '0'
               MOVE RABMAST-IO-AREA (31:5) TO NETFAK-IO
               INSPECT NETFAK-IO REPLACING ALL ' ' BY '0'
               MOVE RABMAST-IO-AREA (40:1) TO SLETT (1:1)
               IF  SLETT = SPACES
                   SET I-28                TO TRUE
               END-IF
           END-EVALUATE.
 
       RABMAST-IDSET SECTION.
       RABMAST-IDSET-P.
           SET I-06                        TO TRUE.
 
       FIRMAF-FLDSET SECTION.
       FIRMAF-FLDSET-P.
           EVALUATE TRUE
           WHEN ANY
               MOVE FIRMAF-IO-AREA (8:30)  TO FINAVN (1:30)
           END-EVALUATE.
 
       FIRMAF-IDSET SECTION.
       FIRMAF-IDSET-P.
           SET I-03                        TO TRUE.
 
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
 
       EXCEPTION-OUTPUT SECTION.
       EXCEPTION-OUTPUT-P.
           IF  (I-86)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (17:24)
               MOVE '******************'   TO LISTE-IO-AREA (41:18)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 3                      TO LISTE-BEFORE-SPACE
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (17:1)
               MOVE '*'                    TO LISTE-IO-AREA (58:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (17:1)
               MOVE 'OPPGAVENR. 752     PROG.' TO LISTE-IO-AREA (23:24)
               MOVE OPPGNR                 TO LISTE-IO-AREA (47:6)
               MOVE '*'                    TO LISTE-IO-AREA (58:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (17:1)
               MOVE 'PRISLISTER              ' TO LISTE-IO-AREA (23:24)
               MOVE '*'                    TO LISTE-IO-AREA (58:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (17:1)
               MOVE '                        ' TO LISTE-IO-AREA (23:24)
               MOVE '*'                    TO LISTE-IO-AREA (58:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (17:1)
               MOVE '*'                    TO LISTE-IO-AREA (58:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*     DENNE OPPGAVE'  TO LISTE-IO-AREA (17:19)
               MOVE '*'                    TO LISTE-IO-AREA (58:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*     FREMSTILT'      TO LISTE-IO-AREA (17:15)
               MOVE UDATE                  TO EDIT-DATE
               MOVE EDIT-DATE (7:8)        TO LISTE-IO-AREA (33:8)
               MOVE '*'                    TO LISTE-IO-AREA (58:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*     TILHØRER FIRMA' TO LISTE-IO-AREA (17:20)
               MOVE FIRMNR                 TO LISTE-IO-AREA (38:3)
               MOVE '*'                    TO LISTE-IO-AREA (58:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (17:1)
               MOVE '*'                    TO LISTE-IO-AREA (58:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (17:1)
               MOVE FIRMNA                 TO LISTE-IO-AREA (23:30)
               MOVE '*'                    TO LISTE-IO-AREA (58:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (17:1)
               MOVE '*'                    TO LISTE-IO-AREA (58:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '*'                    TO LISTE-IO-AREA (17:1)
               MOVE '*'                    TO LISTE-IO-AREA (58:1)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '************************' TO LISTE-IO-AREA (17:24)
               MOVE '******************'   TO LISTE-IO-AREA (41:18)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       HEADING-OUTPUT SECTION.
       HEADING-OUTPUT-P.
           IF  (I-L2 AND NOT-I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               IF  (NOT-I-98)
                   MOVE FINAVN             TO LISTE-IO-AREA (1:30)
               END-IF
               IF  (I-99)
                   MOVE '***   P R I S L I S T E ' TO LISTE-IO-AREA
                                                               (47:24)
               END-IF
               IF  (I-99)
                   MOVE 'R   ***     DATO ' TO LISTE-IO-AREA (72:17)
               END-IF
               IF  (I-99)
                   MOVE UDATE              TO EDIT-DATE
                   MOVE EDIT-DATE (7:8)    TO LISTE-IO-AREA (90:8)
               END-IF
               IF  (NOT-I-99)
                   MOVE TEKST              TO LISTE-IO-AREA (52:49)
               END-IF
               MOVE 'SIDE'                 TO LISTE-IO-AREA (112:4)
               MOVE SNR                    TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (117:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ALFA ARTIKKELNUMMER'  TO LISTE-IO-AREA (1:19)
               MOVE 'VAREBENEVNELSE'       TO LISTE-IO-AREA (26:14)
               MOVE 'PRIS'                 TO LISTE-IO-AREA (55:4)
               MOVE 'ALFA ARTIKKELNUMMER'  TO LISTE-IO-AREA (62:19)
               MOVE 'VAREBENEVNELSE'       TO LISTE-IO-AREA (87:14)
               MOVE 'PRIS'                 TO LISTE-IO-AREA (116:4)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '------------------------' TO LISTE-IO-AREA (73:24)
               MOVE '------------------------' TO LISTE-IO-AREA (97:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L2 AND I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               IF  (NOT-I-98)
                   MOVE FINAVN             TO LISTE-IO-AREA (1:30)
               END-IF
               IF  (I-97)
                   MOVE '***   P R I S L I S T E ' TO LISTE-IO-AREA
                                                               (37:24)
               END-IF
               IF  (I-97)
                   MOVE 'R   ***'          TO LISTE-IO-AREA (61:7)
               END-IF
               IF  (NOT-I-97)
                   MOVE TEKST1             TO LISTE-IO-AREA (32:38)
               END-IF
               MOVE '   SIDE'              TO LISTE-IO-AREA (70:7)
               MOVE SNR                    TO XO-40YNZ
               MOVE XO-40YNZ               TO LISTE-IO-AREA (77:4)
               MOVE 01                     TO LISTE-BEFORE-SKIP
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ALLE PRISER ER VEILEDEND' TO LISTE-IO-AREA (1:24)
               MOVE 'E EKSKL. MOMS, OG KAN EN' TO LISTE-IO-AREA (25:24)
               MOVE 'DRES UTEN FORHÅNDSVARSEL' TO LISTE-IO-AREA (49:24)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'LEVERINGSDAGENS PRIS GJE' TO LISTE-IO-AREA (22:24)
               MOVE 'LDER.'                TO LISTE-IO-AREA (46:5)
               MOVE 2                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE 'ALFA ARTIKKELNUMMER'  TO LISTE-IO-AREA (6:19)
               MOVE 'PRIS'                 TO LISTE-IO-AREA (36:4)
               MOVE 'ALFA ARTIKKELNUMMER'  TO LISTE-IO-AREA (45:19)
               MOVE 'PRIS'                 TO LISTE-IO-AREA (75:4)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE '------------------------' TO LISTE-IO-AREA (1:24)
               MOVE '------------------------' TO LISTE-IO-AREA (25:24)
               MOVE '------------------------' TO LISTE-IO-AREA (49:24)
               MOVE '--------'             TO LISTE-IO-AREA (73:8)
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF.
 
       TOTAL-OUTPUT SECTION.
       TOTAL-OUTPUT-P.
           IF  (I-L1 AND NOT-I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ALF1                   TO LISTE-IO-AREA (1:3)
               INITIALIZE ALF1
               MOVE ANR1                   TO LISTE-IO-AREA (5:20)
               INITIALIZE ANR1
               MOVE BET1                   TO LISTE-IO-AREA (26:24)
               INITIALIZE BET1
               MOVE BEL1                   TO XO-52YNZ
               MOVE XO-52YNZ               TO LISTE-IO-AREA (51:8)
               INITIALIZE BEL1
      *                        PT1    B  59
               MOVE '*'                    TO LISTE-IO-AREA (60:1)
               MOVE ALF2                   TO LISTE-IO-AREA (62:3)
               INITIALIZE ALF2
               MOVE ANR2                   TO LISTE-IO-AREA (66:20)
               INITIALIZE ANR2
               MOVE BET2                   TO LISTE-IO-AREA (87:24)
               INITIALIZE BET2
               MOVE BEL2                   TO XO-52YNZ
               MOVE XO-52YNZ               TO LISTE-IO-AREA (112:8)
               INITIALIZE BEL2
      *                        PT2    B 120
               MOVE 1                      TO LISTE-AFTER-SPACE
               PERFORM LISTE-PRINT-LINE
           END-IF
           IF  (I-L1 AND I-U1)
               MOVE SPACES TO LISTE-IO-AREA
               INITIALIZE LISTE-IO-AREA
               MOVE ALF1                   TO LISTE-IO-AREA (6:3)
               INITIALIZE ALF1
               MOVE ANR1                   TO LISTE-IO-AREA (10:20)
               INITIALIZE ANR1
               MOVE BEL1                   TO XO-52YYZ
               MOVE XO-52YYZ               TO LISTE-IO-AREA (30:9)
               INITIALIZE BEL1
               MOVE PT1                    TO LISTE-IO-AREA (39:1)
               INITIALIZE PT1
               MOVE '*'                    TO LISTE-IO-AREA (42:1)
               MOVE ALF2                   TO LISTE-IO-AREA (45:3)
               INITIALIZE ALF2
               MOVE ANR2                   TO LISTE-IO-AREA (49:20)
               INITIALIZE ANR2
               MOVE BEL2                   TO XO-52YYZ
               MOVE XO-52YYZ               TO LISTE-IO-AREA (70:9)
               INITIALIZE BEL2
               MOVE PT2                    TO LISTE-IO-AREA (79:1)
               INITIALIZE PT2
      *******************************************
      *  PRINTRUTINE FOR FØRSTESIDE PR. FIRMA.  *
      *******************************************
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
           INITIALIZE PKORT-DATA-FIELDS
           SET PKORT-EOF-OFF               TO TRUE
           SET PKORT-PROCESS               TO TRUE
           OPEN INPUT PKORT
           SET INF-LEVEL-INIT              TO TRUE
           INITIALIZE INF-DATA-FIELDS
           SET INF-EOF-OFF                 TO TRUE
           SET INF-PROCESS                 TO TRUE
           OPEN INPUT INF
           INITIALIZE RABMAST-DATA-FIELDS
           OPEN INPUT RABMAST
           INITIALIZE FIRMAF-DATA-FIELDS
           OPEN INPUT FIRMAF
           OPEN OUTPUT LISTE
           INITIALIZE LISTE-IO-AREA
           INITIALIZE LISTE-DATA-FIELDS
           MOVE 57                         TO LISTE-MAX-LINES.
           INITIALIZE TEMPORARY-FIELDS.
 
       TERMINATION SECTION.
       TERMINATION-P.
           CLOSE PKORT
           CLOSE INF
           CLOSE RABMAST
           CLOSE FIRMAF
           IF LISTE-IO-AREA NOT = SPACES
             WRITE LISTE-IO-PRINT BEFORE 1 LINE
             MOVE SPACES TO LISTE-IO-AREA
           END-IF
           CLOSE LISTE.
 
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
